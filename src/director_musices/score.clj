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

;(def score-panel (ssw-mig/mig-panel))
(def score-panel (ssw/tabbed-panel :placement :left))

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
        scale (ssw/slider :min 1 :max 500)
        scale-x (ssw/slider :min 1 :max 500)
        sc (score-component 
             (convert-track (get-track id)) :clef \G )
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
    (ssw/listen sc :mouse-clicked (fn [evt] (let [note-id (get-note-for-x (.getX evt) sc)
                                                  ta (ssw/text :text (clojure.string/replace (str (get-segment id note-id))
                                                                              "," ",\n")
                                                               :multi-line? true)]
                                              (ssw/show! (ssw/dialog :content (ssw/scrollable ta) :option-type :ok-cancel :size [300 :by 300]
                                                                     :success-fn (fn [& _] (set-segment id note-id (read-string (.getText ta)))))))))
    (ssw/listen scale :change (fn [& _] (.setScale sc (/ (.getValue scale) 100))))
    (ssw/listen scale-x :change (fn [& _] (.setScaleX sc (/ (.getValue scale-x) 100))))
    (.setValue scale 100)
    (.setValue scale-x 300)
    (ssw/config! view :items
      [["scale"] [scale] ["scale length"] [scale-x] [show-graph "wrap"]
       [sc "span"]])
    view))

(defn update-score-panel [score]
  (let [views (for [i (range (count score))]
                {:title i :content (ssw/scrollable (score-view i))})]
    (ssw/config! score-panel :tabs views)))

(defn reload-score-panel [] )

(defn choose-and-open-score [& _]
  (ssw-chooser/choose-file
    :success-fn (fn [_ f]
                  (.removeAll score-panel)
                  (let [path (.getCanonicalPath f)]
                    (load-active-score-from-file path)
                    ;(set-score (load-mus-from-path path))
                    (update-score-panel (load-mus-from-path path))))))

(defn choose-and-save-score [& _]
  (if-let [f (new-file-dialog)]
    (spit f (get-active-score))))

