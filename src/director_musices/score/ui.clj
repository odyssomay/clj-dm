(ns director-musices.score.ui
  (:use [clojure.java.io :only [file resource]])
  (:require (director-musices.score
              [global :as global]
              [glue :as glue])
            (director-musices.score.draw
              [graph :as draw-graph]
              [track :as draw-track])
            (director-musices
              [global :as dm-global]
              [player :as player]
              [util :as util])
            (seesaw
              [chooser :as ssw-chooser]
              [core :as ssw]
              [mig :as ssw-mig]))
  (:import javax.swing.SwingUtilities))

(def score-panel-reloader (atom nil))

(defn track-options-dialog [track-id]
  (let [p (ssw-mig/mig-panel)
        items
        (for [property ["trackname" "midi-channel"
                        "midi-initial-volume" "midi-initial-program"
                        "midi-bank-msb" "midi-bank-lsb"
                        "midi-pan" "midi-reverb" 
                        ;"synth" 
                        "instrument-type"
                        "track-delay"]]
          [[(ssw/label :text property)] [(ssw/text :text (str (glue/get-track-property track-id property))
                                                   :columns 15) 
                                         "wrap"]])]
    (ssw/config! p :items (reduce concat items))
    (-> (ssw/dialog :content p :option-type :ok-cancel
                    :success-fn (fn [_] 
                                  (doseq [item items]
                                    (glue/set-track-property track-id (.getText (ffirst item))
                                                             (.getText (first (second item)))))))
      ssw/pack!
      ssw/show!)
    ))

(defn track-options-view [id]
  (let [property-display
        (for [property ["trackname" "midi-channel"
                        "midi-initial-volume" "midi-initial-program"
                        "midi-bank-msb" "midi-bank-lsb"
                        "midi-pan" "midi-reverb" 
                        ;"synth" 
                        "instrument-type"
                        "track-delay"]]
          [[(ssw/label :text property)]
           [(ssw/label :text (str (glue/get-track-property id property))) "gapleft 20, wrap"]])]
    (ssw-mig/mig-panel :items (reduce concat
                                      [[(str "Track " id) "span"]]
                                      property-display)
                       :background "#DDD")))

(defn edit-note [tc id mouse-evt]
  (let [note-id (draw-track/get-note-for-x tc (.getX mouse-evt))
        ta (ssw/text :text (-> (clojure.string/replace (str (glue/get-segment id note-id))
                                                       ", " "\n")
                               (clojure.string/replace #"\{|\}" ""))
                     :multi-line? true)]
    (ssw/show! (ssw/dialog :content (ssw/scrollable ta) :option-type :ok-cancel :size [300 :by 300]
                           :success-fn
                           (fn [& _]
                             (glue/set-segment id note-id
                                               (read-string (str "{" (.getText ta) "}"))))))))

(defn show-graph [view tc]
  (if-let [choice (ssw/input "what type?" :choices [:sl :dr] 
                             :to-string #(subs (str %) 1))]
    (let [c (draw-graph/graph-component tc choice)]
      (.add view (draw-graph/get-view c) "span")
      )))

(defn score-view [id]
  (let [opts-view (track-options-view id)
        tc (draw-track/track-component (glue/get-track id) :clef \G :scale-x 0.2)
        gc (draw-graph/graph-component tc :sl)
        view (ssw-mig/mig-panel :items [[opts-view "dock west"]
                                        [(draw-track/get-view tc) "span"]
                                        [(draw-graph/get-view gc) "span"]
                                        ]
                                :constraints ["insets 0, gap 0" "" ""]
                                :background "white")
        ]
    (ssw/listen (draw-track/get-view tc)
                :mouse-clicked
                (fn [evt]
                  (let [popup (ssw/popup :items [(ssw/action :name "Edit note..."
                                                             :handler (fn [_] (edit-note tc id evt)))
                                                 (ssw/action :name "Show Graph..."
                                                             :handler (fn [_] (show-graph view tc)))])]
                    (cond
                      (SwingUtilities/isRightMouseButton evt)
                      (.show popup (.getSource evt) (.getX evt) (.getY evt))))))
    ; (ssw/listen graph-label :mouse-clicked
    ;             (fn [_]
    ;               (if-let [choice (ssw/input "what type?" :choices [:dr/ndr :sl :dr] 
    ;                                          :to-string #(subs (str %) 1))]
    ; note: not possible to use (name) here, since (name :dr/ndr) => "ndr"
    ;                 (let [c (draw-score/score-graph-component choice sc :height 150)
    ;                       remove-label (ssw/label :icon (resource "icons/stats_delete_small.png"))]
    ;                   (ssw/listen remove-label :mouse-clicked
    ;                               (fn [_] 
    ;                                 (.remove view remove-label)
    ;                                 (.remove view c)
    ;                                 (.revalidate view)))
    ;                   (.add view remove-label)
    ;                   (.add view c "span")
    ;                   (.revalidate view)))))
    (add-watch score-panel-reloader (gensym)
               (fn [& _] (draw-track/set-track tc (glue/get-track id))))
    {:score-component tc
     :view view
     }))

(defn update-score-panel []
  (let [mouse-position-x-start (atom 0)
        initial-scale-x (atom 1)
        new-scale-x (atom 1)
        p (ssw-mig/mig-panel :constraints ["insets 0, gap 0"])
        s-p (ssw/scrollable p :border nil)
        score-views (for [i (range (glue/get-track-count))]
                      (let [sv (score-view i)
                            sc (:score-component sv)
                            ]
                        (ssw/listen (draw-track/get-view sc) 
                                    :mouse-pressed (fn [e] 
                                                     (reset! mouse-position-x-start (.getX e))
                                                     (reset! initial-scale-x (draw-track/get-scale-x sc)))
                                    :mouse-dragged (fn [e] 
                                                     (reset! new-scale-x (* @initial-scale-x
                                                                            (/ (.getX e)
                                                                               @mouse-position-x-start)))))
                        (add-watch new-scale-x i (fn [_ _ _ scale-x] (draw-track/set-scale-x sc scale-x)))
                        [(:view sv) "span"]
                        ;(:view sv)
                        ))]
    (ssw/config! p
                 :items
                 (interleave score-views
                             (take (count score-views)
                                   (repeatedly #(vec [(ssw/separator :orientation :horizontal) "growx, span"]))))
                 )
    (.setUnitIncrement (.getVerticalScrollBar s-p) 10)
    (.setUnitIncrement (.getHorizontalScrollBar s-p) 20)
    (ssw/config! (global/get-score-panel) :items [s-p])
    p))

(defn reload-score-panel []
  (swap! score-panel-reloader not))

;; =====
;; Loading
;; =====

(defn- load-new-score-with [f & [info-text]]
  (.removeAll (global/get-score-panel))
  (dm-global/update-progress-bar
    :indeterminate? true
    :large-text "Loading score"
    :small-text info-text)
  (dm-global/show-progress-bar)
  (util/thread
    (f)
    (ssw/invoke-now
      (update-score-panel)
      (player/update-player)
      (dm-global/hide-progress-bar))))

(defn load-score-from-path [path]
  (load-new-score-with
    #(glue/load-active-score-from-file path)
    path)
  (global/set-score-path path))

(defn load-score-from-midi [path]
  (load-new-score-with
    #(glue/load-active-score-from-midi-file path)
    path)
  (global/set-score-path path))

;; =====
;; Menu functions
;; =====

(defn choose-and-open-score [& _]
  (ssw-chooser/choose-file
    :success-fn 
    (fn [_ f]
      (let [path (.getCanonicalPath f)]
        (load-score-from-path path)
        ))))

(defn choose-and-save-performance [& _]
  (if-let [f (util/new-file-dialog)]
    (spit f (glue/get-active-score))))

(defn choose-and-save-score [& _]
  (if-let [f (util/new-file-dialog)]
    (let [path (.getCanonicalPath f)]
      (glue/save-score-to-path path)
      )))

(defn choose-and-open-midi [& _]
  (ssw-chooser/choose-file
    :success-fn (fn [_ f]
                  (let [path (.getCanonicalPath f)]
                    (load-score-from-midi path)))))

(defn choose-and-save-midi [& _]
  (if-let [f (util/new-file-dialog)]
    (glue/save-midi-to-path (.getCanonicalPath f))))

;; =====
;; Init
;; =====

(defn init []
  (global/init)
  (ssw/config! (global/get-score-panel) :items 
               [(util/start-panel
                  "No score loaded"
                  [(ssw/action :name "Open test score"
                               :handler (fn [_]
                                          (load-new-score-with
                                            #(let [f (file (util/tmp-dir) "test-score.mus")]
                                               (spit f (slurp (resource "Mozart-Amaj-newformat.mus")))
                                               (glue/load-active-score-from-file (.getCanonicalPath f)))
                                            "Mozart-Amaj-newformat.mus")))
                   (ssw/action :name "Open from disk..."
                               :handler choose-and-open-score)])]))

; (defn reload-ui []
;   (load-score-from-path @global/score-path))