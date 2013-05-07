(ns director-musices.score.menu
  (:require (director-musices.score
              [global :as g]
              [ui :as ui])
            (director-musices.rulepalette
              [ui :as rule-ui])
            (seesaw
              [core :as ssw])))

(defn file-menu []
  (ssw/menu
    :text "File"
    :items 
    [(ssw/action :name "Open Score..."
                 :handler ui/choose-and-open-score)
     (ssw/action :name "Open Test Score"
                 :handler ui/open-test-score)
     (ssw/action :name "Open Performance..."
                 :handler ui/choose-and-open-performance)
     (g/a-if-score :name "Save Score As..."
                   :handler ui/choose-and-save-score)
     (g/a-if-score :name "Save Performance As..."
                   :handler ui/choose-and-save-performance)
     :separator
     (g/a-if-score :name "Save pdm As..."
                   :handler ui/choose-and-save-pdm)
     :separator
     (ssw/action :name "Import Score from Midifile..."
                 :handler ui/choose-and-open-midi)
     (g/a-if-score :name "Export Performance to Midifile..."
                   :handler ui/choose-and-save-midi)
     :separator
     (ssw/action :name "Open Rulepalette..."
                 :handler rule-ui/choose-and-open-rulepalette)
     (ssw/action :name "Open Default Rulepalette"
                 :handler rule-ui/open-default-rulepalette)
     :separator
     (ssw/action :name "Quit"
                 :handler (fn [&_ ] (System/exit 0)))
     ]))