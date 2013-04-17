(ns director-musices.score.ui
  (:require (director-musices.score
              [edit-note :as edit-note]
              [global :as global]
              [glue :as glue])
            (director-musices.score.draw
              [graph :as draw-graph]
              [parameter :as draw-parameter]
              [phrase :as draw-phrase]
              [position :as draw-position]
              [track :as draw-track])
            (director-musices
              [global :as dm-global]
              [player :as player]
              [util :as util])
            (seesaw
              [border :as ssw-border]
              [chooser :as ssw-chooser]
              [core :as ssw]
              [dnd :as ssw-dnd]
              [mig :as ssw-mig])
            [clojure.java.io :as jio])
  (:import javax.swing.SwingUtilities))

;; =====
;; Updating
;; =====
(def score-panel-reloader (atom nil))

(defn- reload-score-panel_hidden []
  (swap! score-panel-reloader not))

(defn update-player []
  (dm-global/show-info-panel :loading "Updating player")
  (util/thread
    (player/pause!)
    (let [f (java.io.File. (util/tmp-dir) "buffer.midi")
          p (player/position)]
      (glue/save-midi-to-path (.getCanonicalPath f))
      (player/open-midi-file f)
      (player/position! p))
    (dm-global/hide-info-panel)))

(def ^{:private true} score-changed? (atom false))

(defn reload-later! []
  (reset! score-changed? true))

(defn reload-score []
  (reload-score-panel_hidden)
  (reload-later!))

(defn reload-score-and-player []
  (reload-score)
  (update-player))

(defn reload-if-changed! []
  (when @score-changed?
    (reload-score)
    (update-player)
    (reset! score-changed? false)))

;; =====
;; Track property editor
;; ====

; All properties:
; "trackname" "midi-channel"
; "midi-initial-volume" "midi-initial-program"
; "midi-bank-msb" "midi-bank-lsb"
; "midi-pan" "midi-reverb" 
; "synth" 
; "instrument-type"
; "track-delay"

(def track-properties-bg "#DDD")

(def track-properties
  [{:display-name "Active"
    :property "active-p"
    :type :bool}
   {:display-name "Track"
    :property "trackname"
    :type :string}
   {:display-name "Midi ch"
    :property "midi-channel"
    :min 1 :max 16}
   {:display-name "Synth"
    :property "synth"
    :type :synth}
   {:display-name "Program"
    :property "midi-initial-program"
    :type :midi-program-list}
   {:display-name "Delay"
    :property "track-delay"
    :min 0 :max 100}
   {:display-name "Pan"
    :property "midi-pan"
    :value 64
    :type :slider :min 0 :max 127
    :spacing 32 :minor-spacing 8
    :snap? true}
   {:display-name "Volume"
    :property "midi-initial-volume"
    :value 0
    :type :slider :min -39 :max 0
    :spacing 13 :minor-spacing 13
    :snap? false}
   ])

(defn track-property-editor [id property-map]
  (let [{:keys [type property display-name]} property-map
        value (or (glue/get-track-property id property)
                  (:value property-map))
        c (case type
            :bool (ssw/checkbox :selected? value
                                :halign :right)
            :string (ssw/text :text (str value) :columns 5)
            :synth (ssw/combobox :id :synth :model (glue/get-defined-synths))
            :midi-program-list
            (ssw/combobox :id :program-list
                          :model (glue/get-track-synth-program-list id))
            :slider
            (ssw/slider :value value
                        :min   (:min property-map)
                        :max   (:max property-map)
                        :major-tick-spacing (:spacing property-map)
                        :minor-tick-spacing (:minor-spacing property-map)
                        :snap-to-ticks? (:snap? property-map))
            (ssw/spinner :model (ssw/spinner-model (long value)
                                                   :from (long (:min property-map))
                                                   :to (long (:max property-map))
                                                   )))
        get-value (case type
                    :string ssw/text
                    :bool #(.isSelected %)
                    ssw/selection)
        listen-property (case type
                          :string :document
                          :synth :selection
                          :bool :selection
                          :midi-program-list :selection
                          :change)
        update-property
        (fn [value]
          (glue/set-track-property id property value)
          (case type
            :synth (ssw/config! (ssw/select (.getParent c) [:#program-list])
                                :model (glue/get-track-synth-program-list id))
            nil))]
    (when (= type :slider)
      (let [update-c (fn [show?] (ssw/config! c :paint-labels? show? :paint-ticks? show?))]
        (update-c false)
        (ssw/listen c
                    :mouse-entered (fn [e] (update-c true))
                    :mouse-exited (fn [e] (update-c false)))))
    (ssw/listen c listen-property
                (fn [& _] (update-property (get-value c))
                  (reload-later!)))
    c))

(defn property-display [track-properties id]
  (for [property-map track-properties]
    (let [{:keys [display-name]} property-map
          c (track-property-editor id property-map)]
      [[(ssw/label :text display-name) "gapright 5, w 50!"]
       [c "wrap, w 100!"]])))

(defn track-properties-view [id]
  (let [top-view (ssw-mig/mig-panel
                   :items (reduce concat
                                  (property-display
                                    (take 2 track-properties) id))
                   :constraints ["gap 1, insets 0"])
        extra-view (ssw-mig/mig-panel
                     :items (reduce concat
                                    (property-display
                                      (drop 2 track-properties) id))
                     :constraints ["gap 1, insets 0"])
        view (ssw-mig/mig-panel
               :border (ssw-border/line-border
                         :thickness 0 :right 1
                         :color (java.awt.Color. 150 150 150)))
        expand (ssw/action :name "More")
        retract (ssw/action :name "Less")
        expand-view (fn [& _]
                      (ssw/config! view :items [[top-view "span"]
                                                [extra-view "span"]
                                                [retract "span, align center"]]))
        retract-view (fn [& _]
                       (ssw/config! view :items [[top-view "span"]
                                                 [expand "span, align center"]]))]
    (ssw/config! expand :handler expand-view)
    (ssw/config! retract :handler retract-view)
    (retract-view)
    view))

(defn ask-and-show-property [view tc]
  (when-let [choice (ssw/input "What parameter?")]
    (let [parameter (keyword choice)
          pc (draw-parameter/parameter-component tc parameter)
          c (draw-parameter/get-view pc)
          remove-parameter #(do (.remove view c)
                              (.revalidate view)
                              (.repaint view))
          popup (ssw/popup
                  :items
                  [(ssw/action :name "Remove parameter"
                               :handler (fn [_] (remove-parameter)))])]
      (.add view c "span")
      (.revalidate view))))

(defn show-graph [view tc type]
  (let [gc (draw-graph/graph-component tc type)
        c (draw-graph/get-view gc)
        remove-graph #(do (.remove view c)
                        (.revalidate view)
                        (.repaint view))
        custom-scale? (draw-graph/has-custom-scaling? type)
        autoscale? (ssw/checkbox-menu-item
                     :text "Automatic scaling"
                     :selected? (not custom-scale?)
                     :enabled? custom-scale?)
        popup (ssw/popup :items [autoscale?
                                 (ssw/action :name "Remove graph"
                                             :handler (fn [_] (remove-graph)))])]
    (ssw/listen autoscale? :selection
                (fn [_] (draw-graph/set-automatic-scaling
                          gc (ssw/selection autoscale?))
                  (draw-graph/refresh! gc)))
    (ssw/listen c :mouse-clicked
                (fn [evt]
                  (when (SwingUtilities/isRightMouseButton evt)
                    (.show popup (.getSource evt) (.getX evt) (.getY evt)))))
    (.add view c "span")
    (.revalidate view)))

(defn ask-and-show-graph [view tc]
  (if-let [choice (ssw/input "what type?" :choices
                             (draw-graph/get-available-properties)
                             :to-string draw-graph/get-property-display-name
                             :title "Select graph type")]
    (show-graph view tc choice)))

(defn remove-phrase-marks [tc id note-id]
  (glue/remove-segment-parameter id note-id "phrase-start")
  (glue/remove-segment-parameter id note-id "phrase-end")
  (reload-score))

(defn note-popup-menu [tc id evt view parameter-view]
  (let [note (draw-track/get-note-for-x
               tc (.getX evt))
        note-id (:index note)
        phrase-action
        (fn [type l]
          (ssw/action
            :name (str "Set phrase-" (name type) " " l)
            :handler (fn [_]
                       (glue/set-segment-parameter
                         id note-id
                         (str "phrase-" (name type))
                         l)
                       (reload-score))))
        popup (ssw/popup
                :items
                [(phrase-action :start '(4 5 6))
                 (phrase-action :start '(5 6))
                 (phrase-action :start '(6))
                 :separator
                 (phrase-action :end '(4 5 6))
                 (phrase-action :end '(5 6))
                 (phrase-action :end '(6))
                 :separator
                 (ssw/action :name "Remove all phrase marks"
                             :handler (fn [_] (remove-phrase-marks
                                                tc id note-id)))
                 :separator
                 (ssw/action :name "Edit note..."
                             :handler (fn [_] (edit-note/edit-note
                                                tc id evt reload-score)))
                 :separator
                 (ssw/action :name "Show Property..."
                             :handler (fn [_] (ask-and-show-property
                                                parameter-view tc)))
                 (ssw/action :name "Show Graph..."
                             :handler (fn [_] (ask-and-show-graph view tc)))])]
    (.show popup (.getSource evt) (.getX evt) (.getY evt))))

(defn score-view [id]
  (let [opts-view (track-properties-view id)
        tc (draw-track/track-component (glue/get-track id) :clef \G :scale-x 0.2)
        pc (draw-phrase/phrase-component tc)
        parameter-view (ssw-mig/mig-panel :background "white")
        view (ssw-mig/mig-panel :items [[opts-view "dock west"]
                                        [(draw-phrase/get-view pc) "span"]
                                        [(draw-track/get-view tc) "span, id track"]
                                        [parameter-view "span"]]
                                :constraints ["insets 0, gap 0" "" ""]
                                :background "white")]
    (ssw/listen
      (draw-track/get-view tc)
      :mouse-clicked
      (fn [evt]
        (cond
          (SwingUtilities/isRightMouseButton evt)
           (note-popup-menu tc id evt view parameter-view)
          (and (SwingUtilities/isLeftMouseButton evt)
               (== (.getClickCount evt) 2))
           (edit-note/edit-note tc id evt reload-score))))
    (add-watch score-panel-reloader (gensym)
               (fn [& _] (draw-track/set-track tc (glue/get-track id))))
    (global/on-scale-change #(draw-track/set-scale tc %))
    {:track-component tc
     :score-component tc
     :view view}))

(defn update-score-panel []
  (let [mouse-position-x-start (atom 0)
        initial-scale-x (atom 1)
        new-scale-x (atom 1)
        temporary-scale-x (atom 1)
        position-components (atom [])
        p (ssw-mig/mig-panel :constraints ["insets 0, gap 0"])
        s-p (ssw/scrollable p :border nil)
        score-views
        (for [i (range (glue/get-track-count))]
          (let [sv (score-view i)
                sc (:score-component sv)
                offs draw-track/first-note-offset
                was-dragged? (atom false)]
            (ssw/listen (draw-track/get-view sc) 
                        :mouse-pressed
                        (fn [e]
                          (reset! was-dragged? false)
                          (reset! mouse-position-x-start (.getX e))
                          (reset! initial-scale-x (draw-track/get-scale-x sc)))
                        :mouse-dragged
                        (fn [e]
                          (reset! was-dragged? true)
                          (reset! temporary-scale-x
                                  (* @initial-scale-x
                                     (/ (.getX e)
                                        @mouse-position-x-start))))
                        :mouse-released
                        (fn [e]
                          (if @was-dragged?
                            (reset! new-scale-x
                                    (* @initial-scale-x
                                       (/ (- (.getX e) offs)
                                          (- @mouse-position-x-start offs)
                                          ))))))
            (add-watch temporary-scale-x
                       (gensym) (fn [_ _ _ scale-x]
                                  (draw-track/set-temporary-scale-x sc scale-x)))
            (add-watch new-scale-x
                       (gensym) (fn [_ _ _ scale-x]
                                  (draw-track/set-scale-x sc scale-x)))
            sv))
        position-component (draw-position/position-component
                             (:track-component (first score-views)))
        position-setter-component
        (draw-position/position-setter-component
          (:track-component (first score-views))
          (:view (first score-views))
          player/position!)]
    (draw-track/on-state-change
      (:track-component (first score-views))
      #(do 
         (doto p .revalidate .repaint)
         (doseq [c (.getComponents p)]
           (doto c .revalidate .repaint))))
    (ssw/config!
      p :items
      (concat [[(draw-position/get-view position-component)
                "pos 0 0 100% 100%"]
               [position-setter-component "span"]]
        (interleave (map #(vector (:view %) "span") score-views)
                    (take (count score-views)
                          (repeatedly #(vec [(ssw/separator
                                               :orientation :horizontal)
                                             "growx, span"]))))))
    (player/listen-to-position
      (fn [position]
        (ssw/invoke-later
          (draw-position/set-position-indicator
            position-component position))))
    (.setUnitIncrement (.getVerticalScrollBar s-p) 10)
    (.setUnitIncrement (.getHorizontalScrollBar s-p) 20)
    (ssw/config! (global/get-score-panel) :items [s-p])
    p))

;; =====
;; Loading
;; =====

(defn- load-new-score-with [f & [info-text]]
  (.removeAll (global/get-score-panel))
  (dm-global/show-info-panel :loading "Loading score")
  (util/thread
    (f)
    (ssw/invoke-now
      (update-score-panel)
      (update-player)
      (dm-global/hide-info-panel))))

(defn- load-score-from-file-with [file f]
  (let [path (.getCanonicalPath file)
        name (.getName file)]
    (load-new-score-with #(f path) name)
    (global/set-score-path path)))

(defn load-score-from-file [file]
  (load-score-from-file-with
    file glue/load-active-score-from-file))

(defn load-score-from-midi-file [file]
  (load-score-from-file-with
    file glue/load-active-score-from-midi-file))

;; =====
;; Menu functions
;; =====

(defn choose-and-open-score [& _]
  (ssw-chooser/choose-file
    :success-fn 
    (fn [_ f] (load-score-from-file f))))

(defn choose-and-save-performance [& _]
  (if-let [f (util/new-file-dialog)]
    (spit f (glue/get-active-score))))

(defn choose-and-save-score [& _]
  (if-let [f (util/new-file-dialog)]
    (let [path (.getCanonicalPath f)]
      (glue/save-score-to-path path))))

(defn choose-and-save-pdm [& _]
  (if-let [f (util/new-file-dialog)]
    (let [path (.getCanonicalPath f)]
      (util/thread
        (dm-global/show-info-panel
          :loading (str "Saving pDM " (.getName f)))
        (glue/save-pdm-to-path path)
        (dm-global/hide-info-panel)))))

(defn choose-and-open-midi [& _]
  (ssw-chooser/choose-file
    :success-fn (fn [_ f] (load-score-from-midi-file f))))

(defn choose-and-save-midi [& _]
  (if-let [f (util/new-file-dialog)]
    (glue/save-midi-to-path (.getCanonicalPath f))))

(defn open-test-score [& _]
  (let [f (jio/file (util/tmp-dir) "Mozart-Amaj-newformat.mus")]
    (spit f (slurp (jio/resource "Mozart-Amaj-newformat.mus")))
    (load-score-from-file f)))

;; =====
;; Init
;; =====

(defn init []
  (global/init)
  (ssw/config! (global/get-score-panel)
               :items
               [(util/start-panel
                  "No score loaded"
                  [(ssw/action :name "Open test score"
                               :handler open-test-score)
                   (ssw/action :name "Open from disk..."
                               :handler choose-and-open-score)])]))
