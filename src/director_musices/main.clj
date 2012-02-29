(ns director-musices.main
  (:gen-class))

(defn -main [& args]
  (eval '(do (use 'director-musices.core) (director-musices))))
