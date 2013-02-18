(ns director-musices.rulepalette.global
  (:require [director-musices.util :as util]
            [seesaw.core :as ssw]))

(def rulepalette-filenames (atom ["Default"]))

(def rulepalettes (atom []))
(def rulepalette-container (ssw/tabbed-panel))

(let [l (ssw/label "No rulepalette loaded yet, click here to load one!")
      p (util/centered-component l)]
  ;(ssw/listen p :mouse-clicked (fn [e] (choose-and-open-rulepalette)))
  
  (def rulepalette-panel (ssw/horizontal-panel :items [p]))
  )
