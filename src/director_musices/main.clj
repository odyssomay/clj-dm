(ns director-musices.main
  (:gen-class))

(javax.swing.UIManager/put "FileChooser.readOnly" true)

(defn -main [& args]
  (eval '(do (use 'director-musices.core) (director-musices))))
