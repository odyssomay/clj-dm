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
        :fn :sl}
   :dr-div-ndr {:display "Duration difference (in %)"
                :fn #(* 100
                        (- (/ (:dr %) (:ndr %))
                           1))}})

(defn get-available-properties [] (keys properties))
(defn get-property-display-name [property]
  (get-in properties [property :display]))

(defn calculate-property [note property]
  (let [f (get-in properties [property :fn])
        v (f note)]
    (or v 0)))

(defn graph-data [track-component property]
  (let [notes (draw-track/get-notes track-component)
        property-vals (remove nil? (concat [0] (map #(calculate-property
                                                       % property)
                                                    notes)))
        property-max (reduce max property-vals)
        property-min (reduce min property-vals)
        property-diff (- property-max property-min)]
    {:max property-max
     :min property-min
     :diff property-diff
     :furthest (max property-max
                    (- property-min))}))

(defn update-graph-data [state]
  (let [{:keys [track-component property]} state]
    (assoc state :graph-data (graph-data track-component property))))

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
    interval))

(defn update-line-interval [state]
  (let [{:keys [graph-data]} state
        {:keys [furthest]} graph-data]
    (assoc state
      :line-interval
      (guess-interval furthest 10))))

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
  (let [w (if long? 5 2)]
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
        {:keys [magnitude lines]} line-interval
        scaled-interval (* scale-y magnitude)
        indices (range 1 (inc lines))
        gc (.create g)]
    (.translate gc 40 0)
    (.setColor gc java.awt.Color/black)
    (let [h (/ height 2)]
      (.drawLine gc 0 0 0 h)
      (.drawLine gc 0 0 0 (- h)))
    (doseq [i indices]
      (let [y (* magnitude i)
            sy (* scaled-interval i)
            long? (zero? (rem i 2))]
          (draw-height-line state gc y sy long?)
          (draw-height-line state gc (- y) (- sy) long?)))))

(defn paint [g state]
  (let [{:keys [height track-view track-component]} state
        gc (.create g)
        width (.getWidth track-view)]
    (ssw-graphics/anti-alias gc)
    
    (let [scale (draw-track/get-scale track-component)]
      (.scale gc scale scale))
    
    (.translate gc 0 5)
    
    (let [middle (int (/ height 2))]
      (.translate gc 0 middle))
    
    (.setColor gc java.awt.Color/black)
    (.drawLine gc 0 0 width 0)
    (draw-height-lines gc state)
    (.translate gc (+ (if (:clef (draw-track/get-track track-component)) 35 0) 10) 0)
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
                           :line-height 8}
                          graph-opts)
                        update-state
                        update-property-values))
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
    {:view c}))

;; =====
;; API
;; =====

(defn get-view [tgc] (:view tgc))
