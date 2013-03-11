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
    
    (.setColor gc java.awt.Color/red)
    (.translate gc (+ (if (:clef (draw-track/get-track track-component)) 35 0) 10) 0)
    ; (.drawLine gc 10 0 10 100)
    (draw-note-property-graph gc state)
    )
  )

(defn calculate-scale-y [track-component property height]
  (let [notes (draw-track/get-notes track-component)
        property-vals (remove nil? (concat [0] (map #(get % property nil) notes)))
        property-max (reduce max property-vals)
        property-min (reduce min property-vals)
        property-diff (- property-max property-min)]
    (if (== property-diff 0)
      1
      (/ (/ height 2)
         property-diff))))

(defn graph-component [track-component property & {:keys [height] :or {height 100} :as graph-opts}]
  (let [state (atom (merge {:track-component track-component
                            :track-view (draw-track/get-view track-component)
                            :property property
                            :scale-y (calculate-scale-y track-component property height)
                            :height height}
                           graph-opts))
        c (proxy [javax.swing.JComponent] []
            (paintComponent [g]
              ;(proxy-super paintComponent g)
              ;(.clearRect g 0 0 (.getWidth this) (.getHeight this))
              (paint g @state))
            (getPreferredSize []
              (java.awt.Dimension.
                (.width (.getPreferredSize (draw-track/get-view track-component)))
                (+ (:height @state) 10)
                )))
        refresh (fn []
                  (swap! state (fn [state]
                                 (assoc state :scale-y
                                   (calculate-scale-y track-component
                                                      (:property state)
                                                      (:height state))))))
        ]
    (draw-track/on-state-change track-component refresh)
    {:view c}))

(defn get-view [tgc] (:view tgc))
