(ns director-musices.score.ui
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
              [border :as ssw-border]
              [chooser :as ssw-chooser]
              [color :as ssw-color]
              [core :as ssw]
              [mig :as ssw-mig])
            [clojure.java.io :as jio])
  (:import javax.swing.SwingUtilities))

(declare reload-score)

(def score-panel-reloader (atom nil))

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
    :type :slider :min 0 :max 127
    :spacing 32 :minor-spacing 8
    :snap? true}
   {:display-name "Volume"
    :property "midi-initial-volume"
    :type :slider :min -39 :max 0
    :spacing 13 :minor-spacing 13
    :snap? false}
   ])

(defn track-property-editor [id property-map]
  (let [{:keys [type property display-name]} property-map
        value (glue/get-track-property id property)
        c (case type
            :bool (ssw/checkbox :selected? value
                                :background track-properties-bg
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
                        :snap-to-ticks? (:snap? property-map)
                        :background track-properties-bg)
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
        update-property (fn [value]
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
                  (player/update-later!)))
    c))

(defn track-properties-view [id]
  (let [property-display
        (for [property-map track-properties]
          (let [{:keys [display-name]} property-map
                c (track-property-editor id property-map)]
            [[(ssw/label :text display-name) "gapright 5"]
             [c "w 100!, wrap"]
             ]))
        view (ssw-mig/mig-panel :items (reduce concat property-display)
                                :background track-properties-bg
                                :constraints ["gap 1"])]
    view))

(defn- set-edit-note-location [dialog tc evt note]
  (let [[nx ny] (draw-track/get-note-component-position tc note)
        scale (draw-track/get-scale tc)
        x-diff (- nx (.getX evt))
        y-diff (- ny (.getY evt))
        x (+ (.getXOnScreen evt)
             x-diff
             (* scale 5))
        y (+ (.getYOnScreen evt)
             y-diff
             (* scale 5))]
    (.setLocation dialog x y)))

(defn- arrow-component [border-color]
  (proxy [javax.swing.JComponent] []
    (paint [g]
      (let [g (.create g)]
        (.setColor g border-color)
        (.fillPolygon g
          (int-array [0 25 0])
          (int-array [0 25 25])
          3)))
    (getPreferredSize []
      (java.awt.Dimension. 25 25))))

(defn- note-value-view [view-items segment-atom k value border-color]
  (let [k-label (ssw/label :text (name k))
        t (ssw/text :text (pr-str value)
                    :columns 10)
        delete (ssw/label :icon "icons/delete.png")]
    (ssw/listen t :document
                (fn [& _]
                  (try
                    (swap! segment-atom assoc k
                           (read-string (ssw/text t)))
                    (ssw/config! t :background :white)
                    (catch Exception e
                      (ssw/config! t :background :red)))))
    (ssw/listen delete :mouse-clicked
                (fn [& _]
                  (swap! view-items
                         (fn [items]
                           (remove #(let [f (first %)]
                                      (or (= f k-label)
                                          (= f t)
                                          (= f delete)))
                                   items)))
                  (swap! segment-atom dissoc k)))
    [[k-label] [t "gapleft 5"]
     [delete "gapleft 4, wrap"]]))

(defn- edit-note-add [view-items segment-atom border-color]
  (let [add-label (ssw/label :icon "icons/add.png"
                             :halign :center)]
    (ssw/listen
      add-label
      :mouse-clicked
      (fn [_]
        (let [new-name (ssw/text)
              add-new-view
              (fn [items k]
                (concat
                  items
                  (note-value-view view-items segment-atom
                                   k "" border-color)))
              remove-new-name
              (fn [items]
                (remove #(= (first %) new-name) items))]
          (swap! view-items concat [[new-name "growx, wrap"]])
          (ssw/listen
            new-name
            :action
            (fn [_]
              (let [k (keyword (ssw/text new-name))]
                (swap! view-items
                       (fn [items]
                         (remove-new-name
                           (if (= k "")
                             items
                             (add-new-view items k))))))))
          (ssw/request-focus! new-name))))
    [add-label
     "dock south, growx, gapbottom 7"]))

(defn edit-note [tc id mouse-evt]
  (let [note (draw-track/get-note-for-x tc (.getX mouse-evt))
        note-id (:index note)
        segment (glue/get-segment id note-id)
        segment-atom (atom segment)
        view-items (atom [])
        sorted (sort-by (comp name first) segment)
        border-color (ssw-color/to-color "#FF5050")
        view (ssw-mig/mig-panel
               :constraints ["gap 1, insets 5"]
               :border (ssw-border/line-border
                         :color border-color
                         :thickness 1))
        dialog (ssw/frame
                 :content 
                 (ssw-mig/mig-panel
                   :items [[(arrow-component border-color)
                            "span"]
                           [view]]
                   :constraints ["insets 0, gap 0"])
                 :on-close :dispose
                 :resizable? false
                 :undecorated? true)]
    (add-watch view-items nil
               (fn [_ _ _ items]
                 (ssw/config! view :items items)
                 (ssw/pack! dialog)))
    (reset! view-items
            (cons
              (edit-note-add view-items segment-atom border-color)
              (reduce
                concat
                (for [[k value] sorted]
                  (note-value-view
                    view-items segment-atom
                    k value border-color)))))
    (ssw/listen dialog
                :window-deactivated
                (fn [& _] (.dispose dialog)
                  (glue/set-segment id note-id @segment-atom)
                  (reload-score)))
    (doto dialog
      (set-edit-note-location tc mouse-evt note)
      (.setBackground (java.awt.Color. 0 0 0 0))
      ssw/pack!
      ssw/show!)))

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
    (.revalidate view)
    ))

(defn ask-and-show-graph [view tc]
  (if-let [choice (ssw/input "what type?" :choices
                             (draw-graph/get-available-properties)
                             :to-string draw-graph/get-property-display-name
                             :title "Select graph type")
           ]
    (show-graph view tc choice)))

(defn phrase-action [type l]
  (ssw/action :name (str "Set phrase-" (name type) " " l)))

(defn remove-phrase-marks [tc id evt]
  (let [note-id (:index (draw-track/get-note-for-x
                          tc (.getX evt)))]
    (glue/remove-segment-parameter id note-id "phrase-start")
    (glue/remove-segment-parameter id note-id "phrase-end")))

(defn score-view [id]
  (let [opts-view (track-properties-view id)
        tc (draw-track/track-component (glue/get-track id) :clef \G :scale-x 0.2)
        view (ssw-mig/mig-panel :items [[opts-view "dock west"]
                                        [(draw-track/get-view tc) "span"]]
                                :constraints ["insets 0, gap 0" "" ""]
                                :background "white")]
    (ssw/listen
      (draw-track/get-view tc)
      :mouse-clicked
      (fn [evt]
        (cond
          (SwingUtilities/isRightMouseButton evt)
          (let [popup (ssw/popup
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
                                                        tc id evt)))
                         :separator
                         (ssw/action :name "Edit note..."
                                     :handler (fn [_] (edit-note tc id evt)))
                         (ssw/action :name "Show Graph..."
                                     :handler (fn [_] (ask-and-show-graph view tc)))])]
            (.show popup (.getSource evt) (.getX evt) (.getY evt)))
          (and (SwingUtilities/isLeftMouseButton evt)
               (== (.getClickCount evt) 2))
          (edit-note tc id evt)
          )))
    (add-watch score-panel-reloader (gensym)
               (fn [& _] (draw-track/set-track tc (glue/get-track id))))
    (global/on-scale-change #(draw-track/set-scale tc %))
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

(defn- reload-score-panel_hidden []
  (swap! score-panel-reloader not))

(defn reload-score []
  (reload-score-panel_hidden)
  (player/update-player))

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
      (glue/save-score-to-path path)
      )))

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
  (ssw/config! (global/get-score-panel) :items 
               [(util/start-panel
                  "No score loaded"
                  [(ssw/action :name "Open test score"
                               :handler open-test-score)
                   (ssw/action :name "Open from disk..."
                               :handler choose-and-open-score)])]))
