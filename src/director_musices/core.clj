(ns director-musices.core
  (:gen-class)
  (:use (director-musices rulepalette player score)
;        (director-musices.gui score)
        )
  (:require [seesaw.core :as ssw]))

(def rulepalettes (atom []))
(def rulepalette-container (ssw/tabbed-panel))

(defn add-rulepalette [c]
  (swap! rulepalettes conj c)
  (ssw/config! rulepalette-container :tabs @rulepalettes))

(defn init-rulepalette-menu []
  (ssw/menu 
    :text "rulepalette"
    :items
    [(ssw/action :name "Load rulepalette"
                 :handler (comp add-rulepalette choose-and-open-rulepalette))]))

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

(defn director-musices [& args]
  (let [fr (ssw/frame 
             :title "Director Musices"
             :content (ssw/border-panel :north player-panel :center (ssw/top-bottom-split (ssw/scrollable score-panel)
                                                                                          rulepalette-container))
             :menubar (init-menu-bar)
             :size [400 :by 300]
             :on-close :exit
             )]
    (ssw/show! fr)
    nil))
