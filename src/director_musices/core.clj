(ns director-musices.core
  (:gen-class)
  (:use (director-musices rulepalette player score)
;        (director-musices.gui score)
        )
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
     (ssw/action :name "Import midifile"
                 :handler choose-and-open-midi)
     (ssw/separator)
     (ssw/action :name "Open Rulepalette"
                 :handler choose-and-open-rulepalette)
     (ssw/action :name "Save Rulepalette")
     (ssw/separator)
     (ssw/action :name "Quit")
     ]))

(def apply-menu
  (ssw/menu
    :text "Apply"
    :items
    [(ssw/action :name "Current Rulepalette"
                 :handler apply-current-rulepalette)
     (ssw/action :name "All Rulepalettes"
                 :handler apply-all-rulepalettes)
     (let [cb (ssw/checkbox :text "Reset on Apply")]
       (ssw/listen cb :selection (fn [& _] (set-reset-on-apply (.isSelected cb))))
       cb)
     ]))

(def help-menu
  (ssw/menu
    :text "Help"
    :items 
    [(ssw/action :name "About")]))

(defn init-menu-bar []
  (ssw/menubar :items [file-menu
                       apply-menu
                       help-menu
                       ]))

(defn director-musices [& args]
  (let [fr (ssw/frame 
             :title "Director Musices"
             :content (ssw/border-panel :north player-panel :center (ssw/top-bottom-split score-panel
                                                                                          rulepalette-container))
             :menubar (init-menu-bar)
             :size [800 :by 600]
             :on-close :exit
             )]
    (ssw/show! fr)
    nil))

