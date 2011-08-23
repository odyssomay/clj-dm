(ns director-musices.interpreter
  (:use (director-musices [utils :only [log]])
        [clojure.java.io :only [copy file resource]]))

(def *interpreter*
  (future 
    (do (org.armedbear.lisp.Interpreter/createInstance)
        (org.armedbear.lisp.Interpreter/getInstance))))

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

(defn load-abcl [f]
  (try 
    (println f)
;    (-> (with-open [s1 (.openStream (resource (.replaceAll f ":" "/")))
;                    s2 (java.io.StringWriter.)]
;          (copy s1 s2)
;          (.toString s2))
;        eval-abcl)
    (eval-abcl (str "(load \"" 
                    (.getCanonicalPath (apply file (.split f ":")))
                    ;(.getCanonicalPath (resource (.replaceAll f ":" "/")))
                    "\")"))
    (log :trace (str "loaded " f))
    (catch Exception e
      (log :error e (str "failed loading " f)))))

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
