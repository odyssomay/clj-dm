(ns director-musices.score
  (:use (director-musices [glue :only [load-active-score-from-file]]
                          [load-mus :only [load-mus-from-path]]
                          [draw-score :only [score-component score-graph-component]]))
  (:require [seesaw 
             [core :as ssw]
             [chooser :as ssw-chooser]]))

(def score-panel (ssw/vertical-panel))

(defn update-score-panel [path score]
  (let [s (ssw/slider :min -20 :max 20)
        sc (score-component 
             (for [{n :n :as note} (:notes (first score))]
               (if n
                 (assoc note :pitch (first n)
                        :length (* 4 (second n)))
                 note))
             :clef \G :scale-x 3 ;:scale 2
             )]
    (ssw/listen s :change (fn [& _] (.setScale sc (.getValue s))))
    (ssw/config! score-panel :items
      [(ssw/horizontal-panel :items [s])
       sc])))
  
(defn choose-and-open-score [& _]
  (ssw-chooser/choose-file
    :success-fn (fn [_ f]
                  (let [path (.getCanonicalPath f)]
                    (load-active-score-from-file path)
                    ;(set-score (load-mus-from-path path))
                    (update-score-panel path (load-mus-from-path path))))))

(defn choose-and-save-score [& _] )

