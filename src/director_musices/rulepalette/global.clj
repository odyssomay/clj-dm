(ns director-musices.rulepalette.global
  (:require [director-musices.util :as util]
            [seesaw.core :as ssw]))

(def rulepalette-filenames (atom ["Default"]))

(def rulepalettes (atom []))
(def rulepalette-container (ssw/tabbed-panel))

(def rulepalette-panel (ssw/horizontal-panel))
