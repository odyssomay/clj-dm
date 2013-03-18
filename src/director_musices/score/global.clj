(ns director-musices.score.global
  (:require [director-musices.util :as util]
            [seesaw.core :as ssw]))

(let [score-loaded? (atom false)
      loaded-watchers (atom [])
      score-path (atom "")
      score-panel-atom (atom nil)
      scale-atom (atom 1.0)
      scale-watchers (atom [])]
  (add-watch score-path nil
             (fn [& _]
               (reset! score-loaded? true)
               (doseq [f @loaded-watchers] (f true))))
  
  (defn get-score-loaded? [] @score-loaded?)
  (defn on-score-loaded [f]
    (swap! loaded-watchers conj f))
  
  (defn get-score-path [] @score-path)
  (defn set-score-path [path] (reset! score-path path))
  
  (defn get-score-panel [] @score-panel-atom)
  
  (defn scale! [scale]
    (reset! scale-atom scale)
    (doseq [f @scale-watchers] (f scale)))
  
  (defn on-scale-change [f]
    (f @scale-atom)
    (swap! scale-watchers conj f))
  
  (defn init []
    (reset! score-panel-atom (ssw/horizontal-panel))
    (reset! loaded-watchers [])
    (reset! scale-watchers []))
  )