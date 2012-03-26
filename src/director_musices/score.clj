(ns director-musices.score
  (:use (director-musices [glue :only [load-active-score-from-file get-active-score]]
                          [interpreter :only [eval-abcl]]
                          [draw-score :only [score-component score-graph-component get-note-for-x]]
                          [utils :only [new-file-dialog]]
                          [player :only [update-player]])
        [clojure.java.io :only [resource]])
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

(defn get-track-count []
  (.javaInstance (eval-abcl "(length (track-list *active-score*))")))

(defn get-track-property [id property]
  (.javaInstance (eval-abcl (str "(" property " (nth " id " (track-list *active-score*)))"))))

(defn set-track-property [id property value]
  (let [type (case property
               "trackname" :string
               "instrument-type" :string
               :native)
        value (case type
                :string (str "\"" value "\"")
                :native (str value))]
    (eval-abcl (str "(setf (" property " (nth " id " (track-list *active-score*))) " value ")"))))

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
                                                                                          "," "\n")
                                                            :multi-line? true)]
                                           (ssw/show! (ssw/dialog :content (ssw/scrollable ta) :option-type :ok-cancel :size [300 :by 300]
                                                                  :success-fn (fn [& _] (set-segment id note-id (read-string (.getText ta)))))))))
    sc))

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
          [[(ssw/label :text property)] [(ssw/text :text (str (get-track-property track-id property))
                                                   :columns 15) 
                                         "wrap"]])]
    (ssw/config! p :items (reduce concat items))
    (-> (ssw/dialog :content p :option-type :ok-cancel
                    :success-fn (fn [_] 
                                  (doseq [item items]
                                    (set-track-property track-id (.getText (ffirst item)) (.getText (first (second item)))))))
      ssw/pack!
      ssw/show!)
    ))

(defn update-score-panel []
  (let [mouse-position-x-start (atom 0)
        initial-scale-x (atom 1)
        new-scale-x (atom 1)
        p (ssw-mig/mig-panel)
        score-views (for [i (range (get-track-count))]
                      (let [view (score-view i)
                            options-label (ssw/label :icon (resource "icons/gear.png"))
                            graph-label   (ssw/label :icon (resource "icons/stats.png"))]
                        (ssw/listen view                 
                                    :mouse-pressed (fn [e] 
                                                     (reset! mouse-position-x-start (.getX e))
                                                     (reset! initial-scale-x (:scale-x @(.getOptionsAtom view))))
                                    :mouse-dragged (fn [e] 
                                                     (reset! new-scale-x (* @initial-scale-x (/ (.getX e) @mouse-position-x-start)))))
                        (add-watch new-scale-x i (fn [_ _ _ scale-x] (.setScaleX view scale-x)))
                        (ssw/listen options-label :mouse-clicked (fn [_] (track-options-dialog i))) 
                        [[options-label]
                         [graph-label]
                         [view "span"]]))]
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
                    (update-score-panel)
                    (update-player)
                    ))))

(defn choose-and-save-performance [& _]
  (if-let [f (new-file-dialog)]
    (spit f (get-active-score))))

(defn choose-and-save-score [& _]
  (choose-and-save-performance))

