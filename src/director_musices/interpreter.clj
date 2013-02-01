(ns director-musices.interpreter
  (:use (director-musices [utils :only [log]])
        [clojure.java.io :only [copy file delete-file resource]]))

(def *interpreter*
  (future 
    (do (when-let [i (org.armedbear.lisp.Interpreter/getInstance)]
          (.dispose i))
        (org.armedbear.lisp.Interpreter/createInstance)
        (org.armedbear.lisp.Interpreter/getInstance))))

(defn repl [] (.start (Thread. (fn [] (.run @*interpreter*)))))

(defn eval-abcl [s]
  (.eval @*interpreter* 
    (str "(let ((forms '(" s ")))
            (loop for form in (butlast forms)
                  do (eval form))
            (eval (first (last forms))))")))

;(defn eval-abcl [s]
;  (try 
;    (.eval *interpreter* s)
;    (catch Exception e
;      (println "abcl form eval failed:" s "\n" e)
;      (.printStackTrace e))))

(defn eval-abcl-dm [s]
  (eval-abcl (str "(in-package :dm) " s)))

(def buffer-id (atom 0))

(defn load-abcl [path]
  (try 
    (println path)
    (let [f (file (last (.split path ":")))
          r (resource (.replaceAll path ":" "/"))]
      (if r
        (do 
          (spit f (slurp (resource (.replaceAll path ":" "/"))))
          (eval-abcl (str "(load \"" (.getName f) "\")"))
          (swap! buffer-id inc)
          (delete-file f)
          (log :trace (str "loaded " path))
          )
        (println "... does not exist!")))
    (catch Exception e
      (log :error e (str "failed loading " path)))))

(defn import-abcl [package]
  (eval-abcl (str "(import " package ")")))

(defn abcl-f [package function]
  (let [p (org.armedbear.lisp.Packages/findPackage (.toUpperCase package))
        f (.getSymbolFunction (.findAccessibleSymbol p (.toUpperCase function)))]
    f))

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
