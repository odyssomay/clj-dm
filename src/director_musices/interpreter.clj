(ns director-musices.interpreter
  (:use (director-musices [utils :only [log]])
        [clojure.java.io :only [copy file delete-file resource]]))

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

(def buffer-id (atom 0))

(declare abcl-path)
(defn load-abcl [path]
  (try 
    (println path)
    (let [f (file (last (.split path ":")))
          r (resource (.replaceAll path ":" "/"))]
      (if r
        (do 
          (spit f (slurp (resource (.replaceAll path ":" "/"))))
          (eval-abcl (str "(load \"" (abcl-path (.getCanonicalPath f)) "\")"))
          (swap! buffer-id inc)
          (delete-file f)
          (log :trace (str "loaded " path))
          )
        (println "... does not exist!")))
    (catch Exception e
      (log :error e (str "failed loading " path)))))

;; UTIL

(defn load-dir-abcl [excluding & dirs]
  (let [fl (apply file dirs)]
    (dorun 
      (map #(apply load-abcl (concat dirs [%]))
           (remove #(some (partial = %)
                          excluding)
                   (.list fl))))))

(defn load-dir-in-abcl 
  [including & dirs]
  (let [fl (apply file dirs)]
    (dorun 
      (map #(apply load-abcl (concat dirs [%]))
           including))))

(defn load-multiple-abcl [prefix fs]
  (doseq [f fs]
    (load-abcl (str prefix f))))

(defn load-multiple-abcl-with-progress [& defs-in]
  (let [defs (partition 2 defs-in)
        total (reduce + (map #(count (second %)) defs))
        number-done (atom 0)
        percent-done (atom 0)
        ]
    (println total defs)
    (add-watch number-done ::update-percent 
               (fn [_ _ _ new-number-done] 
                 (reset! percent-done (/ new-number-done total))))
    (.start (Thread. 
              (fn []
                (doseq [[prefix files] defs]
                  (doseq [f files]
                    (load-abcl (str prefix f))
                    (swap! number-done inc)))
                )))
    {:total total
     :number-done number-done
     :percent-done percent-done}
    ))
    

(defn str->abcl [s] (eval-abcl (str "\"" s "\"")))

(let [windows? (.startsWith (System/getProperty "os.name") "Windows")]
  (defn abcl-path [path] 
    (if windows?
      (.replace path "\\" "/")
      path)))

(defn abcl-path-str [path] (str->abcl (abcl-path path)))