(ns director-musices.interpreter
  (:use (Hafni.swing utils)))

;; BASIC

(def *interpreter*
  (do (org.armedbear.lisp.Interpreter/createInstance)
      (org.armedbear.lisp.Interpreter/getInstance)))

(defn eval-abcl [s]
  (.eval *interpreter* s))

(defn load-abcl [& dirs]
  (if-not (= (first (last dirs)) \.) ; do not load hidden files
    (do (eval-abcl (str "(load \"" (.getPath (apply file dirs)) "\")"))
        (println "loaded file:" (last dirs)))
    (println "omitted hidden file:" (last dirs))))

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

;; TEST

(defn call-test-glue []
  (.execute (abcl-f "DM" "TEST")))
