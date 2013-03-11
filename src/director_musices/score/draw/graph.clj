(ns director-musices.score.draw.graph
  (:require [director-musices.score.draw.track :as draw-track]
            (seesaw [graphics :as ssw-graphics])))

(defn draw-note-property-graph [g state]
  (let [gc (.create g)
        {:keys [track-component scale-y property]} state
        scale-x (draw-track/get-scale-x track-component)
        notes (draw-track/get-notes track-component)]
    (doseq [i (range (count notes))]
      (let [note (nth notes i)
            note+1 (nth notes (inc i) nil)
            y (* scale-y (- (get note property 0)))
            ]
        (.translate gc (double (* (:x-offset note) scale-x)) (double 0))
        (.fillOval gc -2 (- y 2) 4 4)
        (when note+1
            (.drawLine gc
                       0 y
                       (* (:x-offset note+1) scale-x)
                       (* scale-y (- (get note+1 property 0)))))
        ))))

(defn draw-height-line [state g y sy long? w]
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
               sy)
    )
  )

(defn guess-interval [furthest expected-lines]
  (let [magnitudes (map #(Math/pow 10 %) (range -10 10))
        nr-lines (map #(let [lines (quot furthest %)]
                         {:magnitude %
                          :lines lines
                          :diff (Math/abs (- lines expected-lines))})
                      magnitudes)
        interval (first (sort-by :diff nr-lines))
        ]
    (assoc interval :interval (:magnitude interval))
    ))

(defn draw-height-lines [g state]
  (let [{:keys [scale-y height graph-data]} state
        {:keys [diff furthest]} graph-data
        expected-lines 10
        {:keys [interval lines]}
        (guess-interval furthest expected-lines)
        ;interval (/ furthest expected-lines)
        scaled-interval (* scale-y interval)
        indices (range 1 (inc lines))
        gc (.create g)]
    ;(guess-interval furthest expected-lines)
    (.translate gc 40 0)
    (.setColor gc java.awt.Color/black)
    (let [h (/ height 2)];(* scale-y interval lines)]
      (.drawLine gc 0 0 0 h)
      (.drawLine gc 0 0 0 (- h)))
    (doseq [i indices]
      (let [y (* interval i)
            sy (* scaled-interval i)
            long? (zero? (rem i 2))
            w (if long? 5 2)]
        ;(when (<= sy (/ height 2))
          (draw-height-line state gc y sy long? w)
          (draw-height-line state gc (- y) (- sy) long? w)
         ; )
        ))
    ))

(defn paint [g state]
  (.setColor g java.awt.Color/red)
  ;(.fillRect g 0 0 100 100)
  (let [{:keys [height track-view track-component]} state
        gc (.create g)
        width (.getWidth track-view)]
    (ssw-graphics/anti-alias gc)
    
    (.translate gc 0 5)
    
    (let [middle (int (/ height 2))]
      (.translate gc 0 middle))
    
    (.setColor gc java.awt.Color/black)
    (.drawLine gc 0 0 width 0)
    (draw-height-lines gc state)
    (.setColor gc java.awt.Color/red)
    (.translate gc (+ (if (:clef (draw-track/get-track track-component)) 35 0) 10) 0)
    ; (.drawLine gc 10 0 10 100)
    (draw-note-property-graph gc state)
    )
  )

(defn graph-data [track-component property]
  (let [notes (draw-track/get-notes track-component)
        property-vals (remove nil? (concat [0] (map #(get % property nil) notes)))
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
  (let [{:keys [graph-data height]} state
        {:keys [diff furthest]} graph-data]
    (assoc state :scale-y
      (if (== diff 0)
        1
        (/ (/ height 2)
           furthest)))))

(defn update-state [state]
  (-> state
      update-graph-data
      update-scale-y))

(defn graph-component [track-component property & {:as graph-opts}]
  (let [state (atom (merge (update-state
                             {:track-component track-component
                              :graph-data (graph-data track-component property)
                              :track-view (draw-track/get-view track-component)
                              :property property
                              :height 100})
                           graph-opts))
        c (proxy [javax.swing.JComponent] []
            (paintComponent [g]
              ;(proxy-super paintComponent g)
              ;(.clearRect g 0 0 (.getWidth this) (.getHeight this))
              (paint g @state)
              )
            (getPreferredSize []
              (java.awt.Dimension.
                (.width (.getPreferredSize (draw-track/get-view track-component)))
                (+ (:height @state) 10)
                )))
        refresh (fn [] (swap! state update-state))
        ]
    (draw-track/on-state-change track-component refresh)
    (add-watch state (gensym) (fn [& _] (.revalidate c) (.repaint c)))
    {:view c}))

(defn get-view [tgc] (:view tgc))
