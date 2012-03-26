(ns director-musices.score
  (:use (director-musices [glue :only [load-active-score-from-file get-active-score]]
                          [interpreter :only [eval-abcl]]
                          [load-mus :only [load-mus-from-path]]
                          [draw-score :only [score-component score-graph-component get-note-for-x]]
                          [utils :only [new-file-dialog]]))
  (:require [seesaw 
             [core :as ssw]
             [chooser :as ssw-chooser]
             [mig :as ssw-mig]]))

;; score object

(defn value->clj [v]
  (condp = v 
    'T true
    'NIL nil
    v))

(defn clj->value [v]
  (condp = v
    true 'T
    nil 'NIL
    v))

(defn segment->map [segment]
  (let [raw (.writeToString segment)]
    (->> (.replaceAll raw " \\. " " ")
         read-string
         (map (fn [[k & vs]]
                [(keyword (.toLowerCase (str k)))
                 (if (== (count vs) 1)
                   (value->clj (first vs))
                   (map value->clj vs))]))
         (into {}))))

(defn map->segment [m]
  (let [raw (with-open [s (java.io.StringWriter.)]
              (binding [*out* s]
                (prn (map (fn [[k v]]
                            (concat (list (symbol (.toUpperCase (name k))))
                              (if (coll? v)
                                (map clj->value v)
                                (list (clj->value v))))) m))
                (str s)))]
    (.replaceAll raw "," "")))

(defn get-track [track-index]
  (let [raw (eval-abcl (str "(map 'list #'var-list (segment-list (nth " track-index " (track-list *active-score*))))"))]
    (->> raw
         .copyToArray
         (map segment->map))))

(defn get-segment [track-index segment-index]
  (nth (get-track track-index) segment-index nil))

(defn set-segment [track-index segment-index segment]
  (let [segment (if (map? segment)
                    (map->segment segment)
                    segment)]
    (eval-abcl (str "(setf (var-list (nth " segment-index 
                                     " (segment-list (nth " track-index 
                                                     " (track-list *active-score*))))) '"
                       segment ")"))))

(defn get-segment-parameter [track-index segment-index k]
  (get (get-segment track-index segment-index) (keyword (name k))))

(defn set-segment-parameter [track-index segment-index k v]
  (set-segment track-index segment-index
    (map->segment (assoc (get-segment track-index segment-index) 
                         (keyword (.toUpperCase (name k)))
                         v))))

;; score gui

;(def score-panel (ssw-mig/mig-panel))
(def score-panel (ssw/horizontal-panel))

(defn convert-track [track]
  (for [{:keys [dr ndr n] :as note} track]
    (assoc
      (if n
        (assoc note :pitch (first n)
          :length (* 4 (second n)))
        note)
      :dr/ndr (/ dr ndr))))

(def score-panel-reloader (atom nil))

(defn score-view [id]
  (let [view (ssw-mig/mig-panel)
        sc (score-component 
             (convert-track (get-track id)) :clef \G )
        mouse-position-x-start (atom 0)
        initial-scale-x (atom 1)
        show-graph (ssw/action :name "graph"
                     :handler (fn [_]
                                (if-let [choice (ssw/input "what type?" :choices [:dr/ndr :sl :dr] :to-string #(subs (str %) 1))]
                                  (let [c (score-graph-component choice sc)]
                                    (ssw/config! c :popup (fn [_] [(ssw/action :name "remove graph" 
                                                                               :handler 
                                                                               (fn [_] (.remove score-panel c)
                                                                                       (.revalidate score-panel)))]))
                                    (.add view c "span"))))
                               )]
    (ssw/listen sc 
                :mouse-clicked (fn [evt] (let [note-id (get-note-for-x (.getX evt) sc)
                                               ta (ssw/text :text (clojure.string/replace (str (get-segment id note-id))
                                                                                          "," ",\n")
                                                            :multi-line? true)]
                                           (ssw/show! (ssw/dialog :content (ssw/scrollable ta) :option-type :ok-cancel :size [300 :by 300]
                                                                  :success-fn (fn [& _] (set-segment id note-id (read-string (.getText ta)))))))))
    sc))

(comment
(defn update-score-panel 
  ([parent-split score i]
   (let [view (score-view i)]
     (if (= (inc i) (count score))
       (do (.addPropertyChangeListener parent-split 
                                       (reify java.beans.PropertyChangeListener
                                         (propertyChange [this e]
                                           (if (= (.getPropertyName e) "dividerLocation")
                                             (.setScaleFromHeight view (- (.getHeight parent-split) (.getNewValue e) 10))
                                             ))))
         view)
       (let [split (ssw/top-bottom-split view nil :border 0)
             bottom (update-score-panel split score (inc i))]
         (.setBottomComponent split bottom)
         (.addPropertyChangeListener split
                                     (reify java.beans.PropertyChangeListener
                                       (propertyChange [this e]
                                         (if (= (.getPropertyName e) "dividerLocation")
                                           (.setScaleFromHeight view (.getNewValue e))))))
         (.setDividerSize split 3)
         (.setResizeWeight split (/ (inc i) (count score)))
         split))))
  ([score]
   (ssw/config! score-panel :items [(ssw/scrollable (update-score-panel nil score 0))])))
)

(defn update-score-panel [score]
  (let [mouse-position-x-start (atom 0)
        initial-scale-x (atom 1)
        new-scale-x (atom 1)
        p (ssw-mig/mig-panel)
        score-views (for [i (range (count score))]
                      (let [view (score-view i)]
                        (ssw/listen view                 
                                    :mouse-pressed (fn [e] 
                                                     (reset! mouse-position-x-start (.getX e))
                                                     (reset! initial-scale-x (:scale-x @(.getOptionsAtom view))))
                                    :mouse-dragged (fn [e] 
                                                     (reset! new-scale-x (* @initial-scale-x (/ (.getX e) @mouse-position-x-start)))))
                        (add-watch new-scale-x i (fn [_ _ _ scale-x] (.setScaleX view scale-x)))
                        [[(str "track " i)] [view "span"]]))]
    (ssw/config! p :items (reduce concat score-views))
    (ssw/config! score-panel :items [(ssw/scrollable p)])
    p))

(defn reload-score-panel [] )

(defn choose-and-open-score [& _]
  (ssw-chooser/choose-file
    :success-fn (fn [_ f]
                  (.removeAll score-panel)
                  (let [path (.getCanonicalPath f)]
                    (load-active-score-from-file path)
                    ;(set-score (load-mus-from-path path))
                    (update-score-panel (load-mus-from-path path))
                    ))))

(defn choose-and-save-score [& _]
  (if-let [f (new-file-dialog)]
    (spit f (get-active-score))))

