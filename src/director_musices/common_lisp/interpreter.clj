(ns director-musices.common-lisp.interpreter
  (:use [clojure.java.io :only [copy file delete-file resource]])
  (:require (director-musices
              [global :as global]
              [util :as util])
            [clojure.string :as clj-str]
            [taoensso.timbre :as log]
            [seesaw.core :as ssw]))

(defn abcl-error [e]
  (log/error e "CL failed with:")
  (global/show-info-panel
    :error "Operation failed. See Help->Log for info."))

(defn get-new-interpreter []
  (when-let [i (org.armedbear.lisp.Interpreter/getInstance)]
    (.dispose i))
  (org.armedbear.lisp.Interpreter/createInstance)
  (org.armedbear.lisp.Interpreter/getInstance))

(def interpreter (atom (get-new-interpreter)))

(let [thread-pool (atom (java.util.concurrent.Executors/newFixedThreadPool 1))
      res (atom nil)
      error (atom false)
      get-thread-pool (fn [] @thread-pool)]
  (defn eval-abcl [s]
    (reset! res nil)
    (reset! error nil)
    (.invokeAll
      (get-thread-pool)
      [(fn []
         (try
           (reset! res (.eval @interpreter 
                              (str "(let ((*debugger-hook*
                                            #'sys::%debugger-hook-function)) "
                                            s ")")))
           (catch Throwable e
             (reset! error e)
             (abcl-error e))))])
    @res)
  
  (defn reload-abcl []
    (.shutdown (get-thread-pool))
    (reset! thread-pool (java.util.concurrent.Executors/newFixedThreadPool 1))
    (reset! interpreter (get-new-interpreter)))
  
  (defn repl []
    (.start (Thread. (fn [] (.invokeAll (get-thread-pool)
                                        [#(.run @interpreter)]))))))

(declare abcl-path)
(defn load-abcl [path & [{:keys [base-dir no-reload] :as opts}]]
  (try
    (let [in (if base-dir
               (apply file base-dir (drop 1 (clj-str/split path #":")))
               (resource (.replaceAll path ":" "/")))
          out-name (last (.split path ":"))
          out (file (util/tmp-dir) out-name)]
      (if in
        (do
          (spit out (slurp in))
          (eval-abcl (str "(load \"" (abcl-path (.getCanonicalPath out)) "\")"))
          (delete-file out)
          (log/info "loaded" path)
          (if (and base-dir (global/get-arg :watch)
                   (not no-reload))
            (util/watch-file
              in (fn [] 
                   (log/info "reloading" (.getName in))
                   (load-abcl path (assoc opts :no-reload true)))))
          )
        (log/error path "does not exist!")))
    (catch Exception e
      (log/error "failed loading" path ", error:" e)
      (.printStackTrace e))))

(defn load-multiple-abcl [prefix fs]
  (doseq [f fs]
    (load-abcl (str prefix ":" f ".lsp"))))

(defn load-multiple-abcl-with-progress [opts & defs-in]
  (let [{:keys [percent-done current-file]}
        (merge {:percent-done #() :current-file #()}
               opts)
        load-opts (dissoc opts :percent-done :current-file)
        defs (partition 2 defs-in)
        total (reduce + (map #(count (second %)) defs))
        number-done-a (atom 0)
        ]
    (add-watch number-done-a nil
               (fn [_ _ _ v]
                 (percent-done (/ v total))))
    (doseq [[prefix files] defs]
      (doseq [f files]
        (let [path (str prefix ":" f ".lsp")]
          (current-file path)
          (load-abcl path load-opts))
        (swap! number-done-a inc)
        ))
    ))

(defn str->abcl [s] (eval-abcl (str "\"" s "\"")))

(let [windows? (.startsWith (System/getProperty "os.name") "Windows")]
  (defn abcl-path [path]
    (if windows?
      (.replace path "\\" "/")
      path)))

(defn abcl-path-str [path] (str->abcl (abcl-path path)))
