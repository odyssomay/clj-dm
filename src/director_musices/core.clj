(ns director-musices.core
  (:gen-class)
  (:use (director-musices rulepalette player score)
;        (director-musices.gui score)
        )
  (:require [seesaw.core :as ssw]))

(def main-area 
  (javax.swing.JDesktopPane.))

(defn add-to-main-area [ifr]
  (when ifr
    (.pack ifr)
    (.setVisible ifr true)
    (.add main-area ifr)
    (.moveToFront main-area ifr)))

(defn init-rulepalette-menu []
  (ssw/menu 
    :text "rulepalette"
    :items
    [(ssw/action :name "Load rulepalette"
                 :handler (comp add-to-main-area choose-and-open-rulepalette))]))

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
                                                                                          main-area))
             :menubar (init-menu-bar)
             :size [400 :by 300])]
    (ssw/show! fr)
    nil))
