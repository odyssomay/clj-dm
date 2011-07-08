(ns director-musices.gui.score
  (:use (director-musices glue load-mus)
        (director-musices.gui track)
        clj-arrow.arrow
        (Hafni.swing component dialog view)))

(def *current-score* (atom nil))

(def *score-panel* (panel))

(defn set-score [score]
  (swap! *current-score* (constantly score)))

(defn init-score-pvariables [score]
  (vec (map init-track-pvariables score)))

;; SISTA TONEN KOMMER INTE MED

(defn score->string [score]
  (apply str
         (map track->string score)))

(defn path->score [path]
  (let [mus (load-mus (load-string (str "'[" 
                                        (slurp path) 
                                        "]")))]
    mus))

(defn save-score [score path]
  (spit path (score->string score)))

(defn track->tune-abc4j
  [track]
  (let [tune (abc.notation.Tune.)
        music (.getMusic tune)]
    (add-track-to-score track music)
    tune))

(defn tune->score-abc4j
  [tune]
  (let [score (abc.ui.swing.JScoreComponent.)]
    (.setJustification score true)
    (.setTune score tune)
    score))

(defn track->abc4j
  [track]
  (tune->score-abc4j
    (track->tune-abc4j track)))

;; Arrows

(def update-score-panel
  (>>> (arr (fn [path]
              [(label :text (str "Currently loaded score: " 
                                 (last (.split path "/"))))]))
       (input-arr *score-panel* :content)))
  
(def choose-and-open-score 
  (flow (arr open-file) >>>
        (arr :path) >>>
        (arr (fn [path]
               (set-score 
                 (path->score path))
               (load-active-score-from-file path)
               path)) >>>
        update-score-panel))

(def choose-and-save-score
  (flow (arr save-file) >>>
        (arr :path) >>>
        (arr (fn [path]
               (save-score @*current-score* path))))) 

;; test

;(def *test-tracks* (load-score "test2.mus"))

;(def *test-track*-abc4j
;  (track->abc4j (first *test-tracks*)))

;(use 'Hafni.swing.view)
;(defn show-test-track []
;  (frame :size 400 200
;         :content (scroll-pane *test-track*-abc4j)
;         :dont_exit_on_close))

