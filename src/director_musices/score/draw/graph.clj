(ns director-musices.score.draw.graph
  (:require [director-musices.score.draw.track :as draw-track]
            (seesaw [graphics :as ssw-graphics])))

;; =====
;; Calculate
;; =====
(def ^{:private true} properties
  {:dr {:display "Duration"
        :fn :dr}
   :sl {:display "Soundlevel"
        :fn :sl
        :range 11}
   :dr-div-ndr {:display "Duration difference (in %)"
                :fn #(* 100
                        (- (/ (:dr %) (:ndr %))
                           1))
                :range 31
                :interval {:magnitude 3
                           :lines 10
                           }
                }})

(defn get-available-properties [] (keys properties))
(defn get-property-display-name [property]
  (get-in properties [property :display]))

(defn calculate-property [note property]
  (let [f (get-in properties [property :fn])
        v (f note)]
    (or v 0)))

(defn graph-data [state property]
  (let [{:keys [property-values
                automatic-scaling]} state
        property-vals (remove nil? (conj property-values 0))
        property-max (reduce max property-vals)
        property-min (reduce min property-vals)
        property-diff (- property-max property-min)]
    {:max property-max
     :min property-min
     :diff property-diff
     :furthest
     (let [prange (get-in properties [property :range])]
       (if (and (not automatic-scaling)
                prange)
         prange
         (max property-max
              (- property-min))))
     }))

(defn update-graph-data [state]
  (let [{:keys [track-component property]} state]
    (assoc state :graph-data (graph-data state property))))

(defn update-scale-y [state]
  (let [{:keys [line-interval line-height]} state
        {:keys [magnitude]} line-interval]
    (assoc state
      :scale-y (/ line-height magnitude))))

(defn update-height [state]
  (let [{:keys [graph-data scale-y]} state
        {:keys [furthest]} graph-data]
    (assoc state :height (max (* 2 furthest scale-y) 50))))

(defn guess-interval [furthest expected-lines]
  (let [magnitudes (map #(Math/pow 10 %) (range -10 10))
        nr-lines (map #(let [lines (quot furthest %)]
                         {:magnitude %
                          :lines lines
                          :diff (Math/abs (- lines expected-lines))})
                      magnitudes)
        interval (first (sort-by :diff nr-lines))]
    (:magnitude interval)))

(defn update-line-interval [state]
  (let [{:keys [graph-data property
                automatic-scaling]} state
        {:keys [furthest]} graph-data
        custom-interval (get-in properties [property :interval])]
    (assoc state
      :line-interval
      (if (and (not automatic-scaling)
               custom-interval)
        custom-interval
        (guess-interval furthest 10)))))

(defn update-state [state]
  (-> state
      update-graph-data
      update-line-interval
      update-scale-y
      update-height))

(defn calculate-property-values [state]
  (let [{:keys [track-component property]} state
        notes (draw-track/get-notes track-component)
        property-vals (map #(calculate-property % property) notes)]
    property-vals))

(defn update-property-values [state]
  (let [{:keys [property-values]} state]
    (assoc state
      :prev-property-values property-values
      :property-values (calculate-property-values state))))

;; =====
;; Drawing
;; =====
(defn draw-note-property-graph [g state property-values]
  (let [gc (.create g)
        {:keys [track-component scale-y]} state
        notes (draw-track/get-notes track-component)
        scale-x (draw-track/get-scale-x track-component)]
    (doseq [i (range (count property-values))]
      (let [note (nth notes i)
            next-note (nth notes (inc i) nil)
            value (nth property-values i)
            next-value (nth property-values (inc i) nil)
            y (* scale-y (- value))
            ]
        (.translate gc (double (* (:x-offset note) scale-x)) (double 0))
        (.fillOval gc -2 (- y 2) 4 4)
        (when next-value
            (.drawLine gc
                       0 y
                       (* (:x-offset next-note) scale-x)
                       (* scale-y (- next-value))))
        ))))

(defn draw-height-line [state g y sy long?]
  (let [{:keys [track-component]} state
        scale (draw-track/get-scale track-component)
        w (if long? 5 2)]
    (.setColor g java.awt.Color/black)
    (.drawLine g 0 sy (- w) sy)
    (when long?
      (.drawString g (str (float (- y)))
                   (+ -30 (if (< y 0) 4 0))
                   (+ (int sy)
                      4))
      (.setColor g (java.awt.Color. 200 200 200))
      (.drawLine g 1 sy
                 (.getWidth (:track-view state))
                 sy))))

(defn draw-height-lines [g state]
  (let [{:keys [scale-y height graph-data
                line-interval]} state
        {:keys [diff furthest]} graph-data
        scaled-interval (* scale-y line-interval)
        gc (.create g)]
    (.translate gc 40 0)
    (.setColor gc java.awt.Color/black)
    (let [h (/ height 2)]
      (.drawLine gc 0 0 0 h)
      (.drawLine gc 0 0 0 (- h)))
    (doseq [i (rest (range))
            :let [y (* line-interval i)]
            :while (<= y furthest)]
      (let [sy (* scaled-interval i)
            long? (zero? (rem i 2))]
          (draw-height-line state gc y sy long?)
          (draw-height-line state gc (- y) (- sy) long?)))))

(defn paint [g state]
  (let [{:keys [height track-view
                track-component property]} state
        gc (.create g)
        width (.getWidth track-view)
        scale (draw-track/get-scale track-component)]
    (ssw-graphics/anti-alias gc)
    (.scale gc 1.0 scale)
    (.translate gc 0 5)
    
    (let [middle (int (/ height 2))]
      (.translate gc 0 middle))
    
    (.setColor gc java.awt.Color/black)
    (.drawLine gc 0 0 width 0)
    (draw-height-lines gc state)
    
    (.scale gc scale 1.0)
    (.translate gc
                (+ (if (:clef (draw-track/get-track track-component))
                     35 0)
                   10)
                0)
    
    (.drawString gc (get-property-display-name
                      property)
                 0 -2)
    
    ;; IMPORTANT
    ;; quickfix to make graph dots appear 
    ;; in the middle of notes
    (.translate gc 4 0)
    ;; end quickfix
    
    (when (:prev-property-values state)
      (.setColor gc (java.awt.Color. 120 120 120))
      (draw-note-property-graph gc state (:prev-property-values state)))
    (.setColor gc java.awt.Color/red)
    (draw-note-property-graph gc state (:property-values state))
    ))

(defn graph-component [track-component property & {:as graph-opts}]
  (let [state (atom (-> (merge
                          {:track-component track-component
                           :graph-data (graph-data track-component property)
                           :track-view (draw-track/get-view track-component)
                           :property property
                           :line-height 8
                           :automatic-scaling false}
                          graph-opts)
                        update-property-values
                        update-state))
        c (proxy [javax.swing.JComponent] []
            (paintComponent [g] (paint g @state))
            (getPreferredSize []
              (java.awt.Dimension.
                (.width (.getPreferredSize (draw-track/get-view track-component)))
                (* (draw-track/get-scale track-component)
                   (+ (:height @state) 10))
                )))
        refresh! (fn [] (swap! state update-state))
        update-property-values! (fn [] (swap! state
                                              #(-> %
                                                   update-property-values
                                                   update-state)))]
    (draw-track/on-state-change track-component refresh!)
    (draw-track/on-track-change track-component update-property-values!)
    (add-watch state (gensym) (fn [& _] (.revalidate c) (.repaint c)))
    {:view c
     :state state}))

;; =====
;; API
;; =====

(defn get-view [tgc] (:view tgc))
(defn set-automatic-scaling [tgc automatic-scaling]
  (swap! (:state tgc) assoc
         :automatic-scaling automatic-scaling))
