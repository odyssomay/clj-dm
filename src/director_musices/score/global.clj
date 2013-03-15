(ns director-musices.score.global
  (:require [director-musices.util :as util]
            [seesaw.core :as ssw]))

(let [score-loaded? (atom false)
      score-path (atom "")
      score-panel-atom (atom nil)]
  (add-watch score-path nil (fn [& _] (reset! score-loaded? true)))
  
  (defn get-score-loaded? [] @score-loaded?)
  (defn on-score-loaded [f] (add-watch score-loaded? (gensym)
                                       (fn [_ _ _ v] (f v))))
  (defn get-score-path [] @score-path)
  (defn set-score-path [path] (reset! score-path path))

  (defn init []
    (reset! score-panel-atom (ssw/horizontal-panel)))
  
  (defn get-score-panel [] @score-panel-atom))