(ns director-musices.score
  (:use (director-musices [glue :only [load-active-score-from-file]]
                          [load-mus :only [load-mus-from-path]]
                          [draw-score :only [score-component score-graph-component]]))
  (:require [seesaw 
             [core :as ssw]
             [chooser :as ssw-chooser]]))

(def current-score (atom nil))

(def score-panel (ssw/vertical-panel))

;(def current-score-component (score-component 

(defn set-score [score]
  (swap! current-score (constantly score)))

(defn update-score-panel [path]
  (ssw/config! score-panel
    :items [;(ssw/label :text (str "Currently loaded score: " (last (.split path "/"))))
            (score-component 
              (for [{n :n :as note} (:notes (first @current-score))]
                               (if n
                                 (assoc note :pitch (first n)
                                             :length (* 4 (second n)))
                                 note))
              :clef \G :scale-x 1.5 ;:scale 2
              )
            ]))
  
(defn choose-and-open-score [& _]
  (ssw-chooser/choose-file
    :success-fn (fn [_ f]
                  (load-active-score-from-file (.getCanonicalPath f))
                  (set-score (load-mus-from-path (.getCanonicalPath f)))
                  (update-score-panel (.getCanonicalPath f)))))

(defn choose-and-save-score [& _] )

