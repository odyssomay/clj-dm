(ns director-musices.rulepalette.global
  (:require [director-musices.util :as util]
            [seesaw.core :as ssw]))

(def ^{:private true} env (atom {}))

(defn get-rulepalettes          [] (:rulepalettes @env))
(defn get-rulepalette-container [] (:rulepalette-container @env))
(defn get-rulepalette-panel     [] (:rulepalette-panel @env))

(defn init []
  (reset! env
          {:rulepalettes []
           :rulepalette-container (ssw/tabbed-panel)
           :rulepalette-panel (ssw/horizontal-panel)}))