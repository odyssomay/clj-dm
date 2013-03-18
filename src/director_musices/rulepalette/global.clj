(ns director-musices.rulepalette.global
  (:require [director-musices.util :as util]
            [seesaw.core :as ssw]))

(def ^{:private true} env (atom {}))

(defn get-rulepalettes          [] (:rulepalettes @env))
(defn get-rulepalette-container [] (:rulepalette-container @env))
(defn get-rulepalette-panel     [] (:rulepalette-panel @env))

(def ^{:private true} r-cont-loaded? (atom false))

(defn load-rulepalette-container []
  (when (not @r-cont-loaded?)
    (ssw/config! (get-rulepalette-panel)
                 :items [(get-rulepalette-container)])
    (reset! r-cont-loaded? true)))

(defn init []
  (reset! r-cont-loaded? false)
  (reset! env
          {:rulepalettes []
           :rulepalette-container (ssw/tabbed-panel)
           :rulepalette-panel (ssw/horizontal-panel)}))
