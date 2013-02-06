(ns director-musices.rulepalette.menu
  (:use [director-musices.rulepalette.rulepalette])
  (:require [seesaw.core :as ssw]))

(def rules-menu
  (ssw/menu :text "Rules"
            :items
            [(ssw/action :name "Open Rulepalette"
                         :handler choose-and-open-rulepalette)
             (ssw/action :name "Open Default Rulepalette" :handler open-default-rulepalette)
             (ssw/action :name "Save Rulepalette")
             (ssw/separator)
             (ssw/action :name "Apply current Rulepalette"
                         :handler apply-current-rulepalette)
             (ssw/action :name "Apply all Rulepalettes"
                         :handler apply-all-rulepalettes)
             (let [cb (ssw/checkbox :text "Reset on Apply")]
               (ssw/listen cb :selection (fn [& _] (set-reset-on-apply (.isSelected cb))))
               cb)
             ]))
