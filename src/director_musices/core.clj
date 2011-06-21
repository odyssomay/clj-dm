(ns director-musices.core
  (:use (director-musices.gui rulepalette score)
        (Hafni arrow)
        (Hafni.swing action component layout menu view)))

(defn init-main-area []
  (border-layout :north *score-panel* :center *rulepalette-panel*))

(defn init-rulepalette-menu []
  (menu "rulepalette"
        :content 
        [(comp-and-events (menu-item :text "Load rulepalette")
                          :act choose-and-open-rulepalette)
         (comp-and-events (menu-item :text "Save rulepalette")
                          :act choose-and-save-rulepalette)
         []
         (comp-and-events (menu-item :text "Apply rulepalette")
                          :act apply-current-rulepalette)
        ]))

(defn init-score-menu []
  (menu "score"
        :content
        [(comp-and-events (menu-item :text "Load score")
                          :act choose-and-open-score)
         (comp-and-events (menu-item :text "Save score")
                          :act choose-and-save-score)]))

(defn init-menu-bar []
  (let [mb (menu-bar :content [(init-rulepalette-menu)
                               (init-score-menu)
                               ])]
    mb))

(defn -main [& args]
  (let [fr (frame :title "Director Musices"
                  :content (init-main-area)
                  :menu_bar (init-menu-bar)
                  :size 300 200)]
    nil))
