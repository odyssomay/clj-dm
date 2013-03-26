(ns director-musices.score.menu
  (:require (director-musices.score
              [global :as global]
              [ui :as ui])
            (director-musices.rulepalette
              [ui :as rule-ui])
            (seesaw
              [core :as ssw])))

(defn a-if-score [& opts]
  (let [a (apply ssw/action opts)
        update #(ssw/config! a :enabled? %)]
    (global/on-score-loaded
      (fn [loaded?] (update loaded?)))
    (update (global/get-score-loaded?))
    a))

(defn file-menu []
  (ssw/menu
    :text "File"
    :items 
    [(ssw/action :name "Open Score..."
                 :handler ui/choose-and-open-score)
     (a-if-score :name "Save Score As..."
                 :handler ui/choose-and-save-score)
     (a-if-score :name "Save Performance As..."
                 :handler ui/choose-and-save-performance)
     (ssw/separator)
     (ssw/action :name "Import Score from Midifile..."
                 :handler ui/choose-and-open-midi)
     (a-if-score :name "Export Performance to Midifile..."
                 :handler ui/choose-and-save-midi)
     (ssw/separator)
     (ssw/action :name "Open Rulepalette..."
                 :handler rule-ui/choose-and-open-rulepalette)
     (ssw/action :name "Open Default Rulepalette"
                 :handler rule-ui/open-default-rulepalette)
     (ssw/action :name "Quit"
                 :handler (fn [&_ ] (System/exit 0)))
     ]))