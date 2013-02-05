(ns director-musices.global
  (:require [seesaw.core :as ssw]))

(ssw/native!)

(let [f (ssw/frame)
      cp (ssw/card-panel)]
  (defn get-frame [] f)
  
  (defn get-center-panel [] cp)
  
  (defn show-progress-bar [])
  
  (defn update-progress-bar [& {:as options}])
  
  (defn hide-progress-bar [])
  )
