(ns director-musices.score.global
  (:require [director-musices.util :as util]
            [seesaw.core :as ssw]))

(def score-loaded? (atom false))
(def score-path (atom ""))

;; This is the only place where score-loaded? is modified!
(add-watch score-path nil (fn [& _] (reset! score-loaded? true)))

(let [l (ssw/label "No score loaded yet, click here to load one!")
      p (util/centered-component l)]
  ;(ssw/listen p :mouse-clicked (fn [e] (choose-and-open-score)))
  
  (def score-panel (ssw/horizontal-panel :items [p])))

; (def score-panel (ssw/horizontal-panel))

; (defn init []
;   (let [l (ssw/label "No score loaded yet, click here to load one!")
;         p (util/centered-component l)]
;     (ssw/listen p :mouse-clicked (fn [e] (choose-and-open-score)))
;     (ssw/config! score-panel :items [p])))