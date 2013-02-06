(ns director-musices.score.menu
  (:use director-musices.score.score)
  (:require [seesaw.core :as ssw]))

(def file-menu
  (ssw/menu
    :text "File"
    :items 
    [(ssw/action :name "Open Score"
                 :handler choose-and-open-score)
     (ssw/action :name "Save Score"
                 :handler choose-and-save-score)
     (ssw/action :name "Save Performance"
                 :handler choose-and-save-performance)
     (ssw/separator)
     (ssw/action :name "Import Midifile"
                 :handler choose-and-open-midi)
     (ssw/action :name "Save Midifile"
                 :handler choose-and-save-midi)
     (ssw/separator)
     (ssw/action :name "Quit"
                 :handler (fn [&_ ] (System/exit 0)))
     ]))