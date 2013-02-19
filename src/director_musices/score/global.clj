(ns director-musices.score.global
  (:require [director-musices.util :as util]
            [seesaw.core :as ssw]))

(def score-loaded? (atom false))
(def score-path (atom ""))

;; This is the only place where score-loaded? is modified!
(add-watch score-path nil (fn [& _] (reset! score-loaded? true)))

(def score-panel (ssw/horizontal-panel))