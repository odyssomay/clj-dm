(ns director-musices.common-lisp.interpreter
  (:use [clojure.java.io :only [copy file delete-file resource]])
  (:require [director-musices.util :as util]
            [taoensso.timbre :as log]))

(def interpreter
  (future
    (do (when-let [i (org.armedbear.lisp.Interpreter/getInstance)]
          (.dispose i))
        (org.armedbear.lisp.Interpreter/createInstance)
        (org.armedbear.lisp.Interpreter/getInstance))))

(defn repl [] (.start (Thread. (fn [] (.run @interpreter)))))

(let [thread-pool (java.util.concurrent.Executors/newFixedThreadPool 1)
      res (atom nil)]
  (defn eval-abcl [s]
    (reset! res nil)
    (.invokeAll thread-pool
                [(fn [] 
                   (reset! res (.eval @interpreter (str "(progn " s ")")))
                   )])
    @res
    ))

(defn eval-abcl-dm [s] (eval-abcl (str "(in-package :dm) " s)))

(declare abcl-path)
(defn load-abcl [path]
  (try
    (let [in (resource (.replaceAll path ":" "/"))
          out-name (last (.split path ":"))
          out (file (util/tmp-dir) out-name)]
      (if in
        (do
          (spit out (slurp in))
          (eval-abcl (str "(load \"" (abcl-path (.getCanonicalPath out)) "\")"))
          (delete-file out)
          (log/info "loaded" path)
          )
        (log/error path "does not exist!")))
    (catch Exception e
      (log/error "failed loading" path ", error:" e))))

;; UTIL

(defn load-dir-abcl [excluding & dirs]
  (let [fl (apply file dirs)]
    (dorun
      (map #(apply load-abcl (concat dirs [%]))
           (remove #(some (partial = %)
                          excluding)
                   (.list fl))))))

(defn load-dir-in-abcl [including & dirs]
  (let [fl (apply file dirs)]
    (dorun
      (map #(apply load-abcl (concat dirs [%]))
           including))))

(defn load-multiple-abcl [prefix fs]
  (doseq [f fs]
    (load-abcl (str prefix ":" f ".lsp"))))

(defn load-multiple-abcl-with-progress [progress-fns & defs-in]
  (let [{:keys [percent-done current-file]}
        (merge {:percent-done #() :current-file #()}
               progress-fns)
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
          (load-abcl path))
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