(ns director-musices.main
  (:require [director-musices.core :as core]
            [seesaw.core :as ssw])
  (:gen-class))

(defn -main [& args]
  (ssw/invoke-later
    (javax.swing.UIManager/put "FileChooser.readOnly" true)
    (ssw/native!)
    (apply core/director-musices args)))
