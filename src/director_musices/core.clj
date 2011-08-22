(ns director-musices.core
  (:gen-class)
  (:use (director-musices rulepalette player)
        (director-musices.gui score))
  (:require [seesaw.core :as ssw]))

(defn init-main-area []
  (ssw/border-panel :north player-panel :south *score-panel* :center *rulepalette-panel*))

(defn init-rulepalette-menu []
  (ssw/menu 
    :text "rulepalette"
    :items
    [(ssw/action :name "Load rulepalette"
                 :handler choose-and-open-rulepalette)
     (ssw/action :name "Save rulepalette"
                 :handler choose-and-save-rulepalette)
     :separator
     (ssw/action :name "Apply rulepalette"
                 :handler apply-current-rulepalette)
     ]))

(defn init-score-menu []
  (ssw/menu 
    :text "score"
    :items
    [(ssw/action :name "Load score"
                 :handler choose-and-open-score)
     (ssw/action :name "Save score"
                 :handler choose-and-save-score)]))

(defn init-menu-bar []
  (ssw/menubar :items [(init-rulepalette-menu)
                       (init-score-menu)]))

(defn -main [& args]
  (let [fr (ssw/frame 
             :title "Director Musices"
             :content (init-main-area)
             :menubar (init-menu-bar)
             :size [400 :by 300])]
    (ssw/show! fr)
    nil))
