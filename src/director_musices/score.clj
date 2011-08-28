(ns director-musices.score
  (:use (director-musices [glue :only [load-active-score-from-file]]
                          [interpreter :only [eval-abcl]]
                          [load-mus :only [load-mus-from-path]]
                          [draw-score :only [score-component score-graph-component]]))
  (:require [seesaw 
             [core :as ssw]
             [chooser :as ssw-chooser]
             [mig :as ssw-mig]]))

;; score object

(defn value->clj [v]
  (case v 
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

(def score-panel (ssw-mig/mig-panel))

(defn convert-track [track]
  (for [{:keys [dr ndr n] :as note} track]
    (assoc
      (if n
        (assoc note :pitch (first n)
          :length (* 4 (second n)))
        note)
      :dr/ndr (/ dr ndr))))

(def score-panel-reloader (atom nil))

(defn update-score-panel [path score]
  (let [tracks (ssw/combobox :model (range (count score)))
        scale (ssw/slider :min 0 :max 500)
        scale-x (ssw/slider :min 0 :max 500)
        sc (score-component 
             (convert-track (get-track 0)) :clef \G)
        show-graph (ssw/action :name "graph"
                     :handler (fn [_]
                                (if-let [choice (ssw/input "what type?" :choices [:dr/ndr] :to-string #(subs (str %) 1))]
                                  (let [c (score-graph-component choice sc)]
                                    (ssw/config! c :popup (fn [_] [(ssw/action :name "remove graph" 
                                                                               :handler 
                                                                               (fn [_] (.remove score-panel c)
                                                                                       (.revalidate score-panel)))]))
                                    (.add score-panel c "span")))))]
    (ssw/listen tracks :selection 
      (fn [_] (.setNotes sc (convert-track (get-track (.getSelectedItem tracks))))))
    (ssw/listen scale :change (fn [& _] (.setScale sc (/ (.getValue scale) 100))))
    (ssw/listen scale-x :change (fn [& _] (.setScaleX sc (/ (.getValue scale-x) 100))))
    (.setValue scale 100)
    (.setValue scale-x 300)
    (reset! score-panel-reloader #(.setSelectedItem tracks (.getSelectedItem tracks)))
    (ssw/config! score-panel :items
      [["track"] [tracks]
       ["scale"] [scale] ["scale length"] [scale-x] [show-graph "wrap"]
       [sc "span"]])))

(defn reload-score-panel []
  (if @score-panel-reloader
    (@score-panel-reloader)))
  
(defn choose-and-open-score [& _]
  (ssw-chooser/choose-file
    :success-fn (fn [_ f]
                  (let [path (.getCanonicalPath f)]
                    (load-active-score-from-file path)
                    ;(set-score (load-mus-from-path path))
                    (update-score-panel path (load-mus-from-path path))))))

(defn choose-and-save-score [& _] )

