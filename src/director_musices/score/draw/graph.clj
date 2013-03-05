(ns director-musices.score.draw.graph
  (:require [director-musices.score.draw.track :as draw-track]
            (seesaw [graphics :as ssw-graphics])))

(defn draw-note-property-graph_OLD [g notes property scale-y {:keys [scale-x] :as options}]
  (let [gcx (.create g)]
    (doseq [i (range (count notes))]
      (let [note (nth notes i)
            note+1 (nth notes (inc i) nil)
            y (* scale-y (- (get note property 0)))
            ]
        (.translate gcx (double (* (:x-offset note) scale-x)) (double 0))
        (.fillOval gcx -2 (- y 2) 4 4)
        (when note+1
          (let [y (* scale-y (- (get note property 0)))]
            (.drawLine gcx 
                       0 y
                       (* (:x-offset (inc i) notes) scale-x) (* scale-y (- (get note+1 property 0))))))
        ))))

; (defn draw-graph-height-lines [g width scale-y value-max value-min]
;   (let [gc (.create g)
;         gc-text (.create gc)
;         step (/ (max value-max (Math/abs value-min)) 10)
;         draw-height-line (fn [h height pre]
;                            (if (and (< h 9) (odd? h))
;                              (let [text (str pre (apply str (take 5 (str (* h step)))))]
;                                (.drawString gc-text text (float 40) (float (- (* (/ 1 0.8) height) 1)))))
;                            (.drawLine gc 0 height width height))
;         ]
;     (.setColor gc (java.awt.Color. 200 200 200))
;     (.setColor gc-text (java.awt.Color. 50 50 50))
;     (.scale gc-text 0.8 0.8)
;     (doseq [h (range 0 11)]
;       (let [height (* scale-y h step)]
;         (draw-height-line h (- height) "")
;         (draw-height-line h height "-")))))

; (defn paint [g track-state graph-state]
;   (let [gc (.create g)
;         {:keys [track scale]} track-state
;         {:keys [notes clef]} track
;         graph-options @graph-options-atom
;         scale-y (get-scale-y)
;         scaled-width (* scale (get-width))
;         ]
;     (ssw-graphics/anti-alias gc)
;     (.scale gc scale scale)
;     (.translate gc 0.0 (double (+ (* scale-y (get-property-max)) 5)))
;     (draw-graph-height-lines gc scaled-width scale-y (get-property-max) (get-property-min))
;     (.drawLine gc 0 0 scaled-width 0)
;     (let [gcc (.create g)]
;       (.scale gcc 1.2 1.2)
;       (.setColor gcc (java.awt.Color. 0 0 0))
;       (let [text (:title graph-options)
;             metrics (.getFontMetrics gcc)
;             width (.stringWidth metrics text)
;             height (.getHeight metrics)]
;         ;(.fillRect gcc 5 (- -15 height) (+ 10 width) (+ 10 height))
;         (.fillRect gcc 5 5 (+ 10 width) (+ 10 height))
;         (.setColor gcc (java.awt.Color. 255 255 255))
;         (.drawString gcc (:title graph-options) 10 (+ 10 height))
;         ))
;     (.translate gc (double (if clef 45 10)) 0.0)
;     ;(.drawLine gc -5 0 -5 (- (- (get-property-max)) 5))
;     ;(.drawLine gc -5 0 -5 (+ (- (get-property-min)) 5))
;     (draw-note-property-graph gc notes property scale-y options)
;     ))

; (defn track-graph-component [property track-component & {:as graph-opts}]
;   (let [options-atom (:state track-component)
;         graph-options-atom (atom (merge {:title (str property) :height 50} graph-opts))
;         get-notes (fn [] (:notes @options-atom))
;         get-width (fn [] (get-score-component-width (get-notes) @options-atom))
;         get-property-max (fn [] (apply max (remove nil? (concat [0] (map #(get % property) (get-notes))))))
;         get-property-min (fn [] (apply min (remove nil? (concat [0] (map #(get % property) (get-notes))))))
;         get-height (fn [] (- (get-property-max) (get-property-min)))
;         get-scale-y (fn []
;                       (let [height (get-height)]
;                         (if (== height 0)
;                           1
;                           (/ (:height @graph-options-atom)
;                              height))))
;         c (proxy [javax.swing.JComponent javax.swing.Scrollable] []
;             (paintComponent [g] (paint g (draw-track/get-state track-component) ))
              
;             (getPreferredSize []
;               (java.awt.Dimension. 
;                 (.width (.getPreferredSize score-component))
;                 (* (:scale @options-atom) (+ 10 (* (get-scale-y) (get-height)))))); (+ 12 (get-height)))))
;             ;; graph properties
;             ; (setHeight [height] (swap! graph-options-atom assoc :height height))
;             ; (setTitle [title] (swap! graph-options-atom assoc :title title))
;             )]
;     (add-watch options-atom (gensym) (fn [& _] (.revalidate c) (.repaint c)))
;     (add-watch graph-options-atom (gensym) (fn [& _] (.revalidate c) (.repaint c)))
;     c))

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
        property-max (reduce max (remove nil? (concat [0] (map #(get % property nil) notes))))
        property-min (reduce min (remove nil? (concat [0] (map #(get % property nil) notes))))
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
                )))]
    {:view c}))

(defn get-view [tgc] (:view tgc))