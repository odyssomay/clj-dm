(ns director-musices.score.ui
  (:require (director-musices.score
              [edit-note :as edit-note]
              [global :as global]
              [glue :as glue]
              [mixer :as mixer])
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
(def ^{:private true} score-changed? (atom false))

(defn- reload-score-panel_hidden []
  (swap! score-panel-reloader not))

(defn reload-player []
  (reset! score-changed? false)
  (dm-global/show-info-panel :loading "Updating player")
  (player/pause!)
  (let [f (java.io.File. (util/tmp-dir) "buffer.midi")
        p (player/position)]
    (glue/save-midi-to-path (.getCanonicalPath f))
    (player/open-midi-file f)
    (player/position! p))
  (dm-global/hide-info-panel))

(defn reload-later! []
  (reset! score-changed? true))

(defn reload-score []
  (ssw/invoke-now
    (reload-score-panel_hidden)
    (reload-later!)))

(defn reload-score-and-player []
  (reload-score)
  (reload-player))

(defn reload-player-if-changed! []
  (when @score-changed?
    (reload-player)
    (reset! score-changed? false)))

(defn ask-and-show-parameter [view tc]
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
      (ssw/listen
        c :mouse-clicked (fn [e]
                           (if (SwingUtilities/isRightMouseButton e)
                             (.show popup (.getSource e) (.getX e) (.getY e)))))
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
                 (ssw/action :name "Remove all phrase marks on this note"
                             :handler (fn [_] (remove-phrase-marks
                                                tc id note-id)))
                 :separator
                 (ssw/action :name "Edit note..."
                             :handler (fn [_] (edit-note/edit-note
                                                tc id evt reload-score)))
                 :separator
                 (ssw/action :name "Show Parameter..."
                             :handler (fn [_] (ask-and-show-parameter
                                                parameter-view tc)))
                 (ssw/action :name "Show Graph..."
                             :handler (fn [_] (ask-and-show-graph view tc)))])]
    (.show popup (.getSource evt) (.getX evt) (.getY evt))))

(defn redraw-component! [c]
  (doto c .revalidate .repaint))

(defn score-view [id]
  (let [tc (draw-track/track-component (glue/get-track id) :clef \G :scale-x 0.2)
        pc (draw-phrase/phrase-component tc)
        parameter-view (ssw-mig/mig-panel :background "white"
                                          :constraints ["insets 0, gap 0"])
        view (ssw-mig/mig-panel :items [[(draw-phrase/get-view pc) "span"]
                                        [(draw-track/get-view tc) "span, id track"]
                                        [parameter-view "span"]]
                                :constraints ["insets 0, gap 0"]
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
     :repaint! (fn []
                 (redraw-component! (draw-track/get-view tc))
                 (redraw-component! parameter-view))
     :parameter-view parameter-view
     :view view}))

(defn add-scale-x-listeners [score-views]
  (let [mouse-position-x-start (atom 0)
        initial-scale-x (atom 1)
        new-scale-x (atom 1)
        temporary-scale-x (atom 1)]
    (doseq [score-view score-views]
      (let [sv score-view
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
                                      (- @mouse-position-x-start offs)))))))
        (add-watch temporary-scale-x
                   (gensym) (fn [_ _ _ scale-x]
                              (draw-track/set-temporary-scale-x sc scale-x)))
        (add-watch new-scale-x
                   (gensym) (fn [_ _ _ scale-x]
                              (draw-track/set-scale-x sc scale-x)))))))

(defn update-score-panel []
  (let [p (ssw-mig/mig-panel :constraints ["insets 0, gap 0"])
        s-p (ssw/scrollable p :border nil)
        score-views (map score-view (range (glue/get-track-count)))
        mxr (mixer/mixer reload-later!)
        position-component (draw-position/position-component
                             (:track-component (first score-views)))
        position-setter-component
        (draw-position/position-setter-component
          (:track-component (first score-views))
          (:view (first score-views))
          player/position!)
        view (ssw-mig/mig-panel :items [[s-p "grow"]]
                                :constraints ["insets 0, gap 0, fill"])
        clear-view (fn [] (.removeAll view))
        
        to-score (util/button-label
                   (fn [l] (ssw/config!
                             view :items [[s-p "grow"]]))
                   :icon "icons/score.png"
                   :tip "Show score")
        mxr-view (ssw-mig/mig-panel :items [[to-score "aligny top"]
                                            [mxr "align left, aligny top"]]
                                    :constraints ["insets 0, fill"])
        to-mixer (util/button-label
                   (fn [l] (ssw/config!
                             view :items [[(ssw/scrollable
                                             mxr-view :border nil) "grow"]]))
                   :icon "icons/mixer.png"
                   :tip "Show mixer")]
    (add-scale-x-listeners score-views)
    (draw-track/on-state-change
      (:track-component (first score-views))
      #(do
         (doseq [{:keys [repaint!]} score-views]
           (repaint!))
         (redraw-component! p)
         (doseq [c (.getComponents p)]
           (redraw-component! c))))
    (ssw/config!
      p :items
      (concat [[to-mixer "pos 0 0"]
               [(draw-position/get-view position-component)
                "pos 0 0 100% 100%"]
               [position-setter-component "span"]]
        (interleave (map #(vector (:view %) "span") score-views)
                    (take (count score-views)
                          (repeatedly #(vec [(ssw/separator
                                               :orientation :horizontal)
                                             "growx, span"]))))
        [[:fill-v "growy"]]))
    (player/listen-to-position
      (fn [position]
        (ssw/invoke-later
          (draw-position/set-position-indicator
            position-component position))))
    (.setUnitIncrement (.getVerticalScrollBar s-p) 10)
    (.setUnitIncrement (.getHorizontalScrollBar s-p) 20)
    (ssw/config! (global/get-score-panel) :items [view])
    view))

;; =====
;; Loading
;; =====

(defn- load-new-score-with [f & [info-text]]
  (.removeAll (global/get-score-panel))
  (dm-global/show-info-panel :loading "Loading score")
  (util/thread
    (f)
    (ssw/invoke-now (update-score-panel))
    (reload-player)
    (dm-global/hide-info-panel)))

(defn- load-score-from-file-with [file f]
  (let [path (.getCanonicalPath file)
        name (.getName file)]
    (load-new-score-with #(f path) name)
    (global/set-score-path path)))

(defn load-score-from-file [file]
  (load-score-from-file-with
    file glue/load-active-score-from-file))

(defn load-performance-from-file [file]
  (load-score-from-file-with
    file glue/load-active-performance-from-file))

(defn load-score-from-midi-file [file]
  (load-score-from-file-with
    file glue/load-active-score-from-midi-file))

;; =====
;; Menu functions
;; =====
(def score-filter ["Score files (.mus)" ["mus"]])
(def perf-filter ["Performance files (.perf)" ["perf"]])
(def midi-filter ["midi files (.mid .midi)" ["mid" "midi"]])
(def pdm-filter ["pdm files (.pdm)" ["pdm"]])

(defn choose-and-open-score [& _]
  (if-let [f (util/choose-file
               :title "Open Score"
               :type :open
               :filters [score-filter])]
    (load-score-from-file f)))

(defn choose-and-save-score [& _]
  (if-let [f (util/choose-file
               :title "Save Score"
               :type :save
               :file-ending "mus"
               :filters [score-filter])]
    (let [path (.getCanonicalPath f)]
      (glue/save-score-to-path path))))

(defn choose-and-open-performance [& _]
  (if-let [f (util/choose-file
               :title "Open Performance"
               :type :open
               :filters [perf-filter])]
    (load-performance-from-file f)))

(defn choose-and-save-performance [& _]
  (if-let [f (util/choose-file
               :title "Save Performance"
               :type :save
               :file-ending "perf"
               :filters [perf-filter])]
    (spit f (glue/get-active-score))))

(defn choose-and-save-pdm [& _]
  (if-let [f (util/choose-file
               :title "Save pdm"
               :type :save
               :file-ending "pdm"
               :filters [pdm-filter])]
    (let [path (.getCanonicalPath f)]
      (util/thread
        (dm-global/show-info-panel
          :loading (str "Saving pdm " (.getName f)))
        (glue/save-pdm-to-path path)
        (dm-global/hide-info-panel)))))

(defn choose-and-open-midi [& _]
  (if-let [f (util/choose-file
               :title "Open midi"
               :type :open
               :filters [midi-filter])]
    (load-score-from-midi-file f)))

(defn choose-and-save-midi [& _]
  (if-let [f (util/choose-file
               :title "Save midi"
               :type :save
               :file-ending "midi"
               :filters [midi-filter])]
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
