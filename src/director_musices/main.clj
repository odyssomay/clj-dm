(ns director-musices.main
  (:gen-class))

(javax.swing.UIManager/put "FileChooser.readOnly" true)

(defn -main [& args]
  (eval (list 'do '(use 'director-musices.core) (apply list 'director-musices args))))
