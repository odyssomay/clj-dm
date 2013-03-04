(ns director-musices.score.draw.graph)


; (defprotocol scoreGraphProperties
;   (setHeight [this height] )
;   (setTitle [this title]))

; (defn draw-note-property-graph [g notes property scale-y {:keys [scale-x] :or {scale-x 1} :as options}]
;   (let [gcx (.create g)]
;     (doseq [i (range (count notes))]
;       (.translate gcx (double (* (:x-offset i notes) scale-x)) (double 0))
;       (let [note (nth notes i)
;             note+1 (nth notes (inc i) nil)
;             y (* scale-y (- (get note property 0)))
;             ]
;           (.fillOval gcx -2 (- y 2) 4 4)
;         (when note+1
;           (let [y (* scale-y (- (get note property 0)))]
;             (.drawLine gcx 
;                        0 y
;                        (* (:x-offset (inc i) notes) scale-x) (* scale-y (- (get note+1 property 0))))))
;         ;(.drawString gcx (str (get note property)) (double 3) (double (- (get note property))))
;         ))))

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

; (defn score-graph-component [property score-component & {:as graph-opts}]
;   (let [options-atom (.getOptionsAtom score-component)
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
;             (paintComponent [g]
;               (let [gc (.create g)
;                     options @options-atom
;                     notes (:notes options)
;                     scale (:scale options)
;                     clef (:clef options)
;                     graph-options @graph-options-atom
;                     scale-y (get-scale-y)
;                     scaled-width (* scale (get-width))
;                     ]
;                 (ssw-graphics/anti-alias gc)
;                 (.scale gc scale scale)
;                 (.translate gc 0.0 (double (+ (* scale-y (get-property-max)) 5)))
;                 (draw-graph-height-lines gc scaled-width scale-y (get-property-max) (get-property-min))
;                 (.drawLine gc 0 0 scaled-width 0)
;                 (let [gcc (.create g)]
;                   (.scale gcc 1.2 1.2)
;                   (.setColor gcc (java.awt.Color. 0 0 0))
;                   (let [text (:title graph-options)
;                         metrics (.getFontMetrics gcc)
;                         width (.stringWidth metrics text)
;                         height (.getHeight metrics)]
;                     ;(.fillRect gcc 5 (- -15 height) (+ 10 width) (+ 10 height))
;                     (.fillRect gcc 5 5 (+ 10 width) (+ 10 height))
;                     (.setColor gcc (java.awt.Color. 255 255 255))
;                     (.drawString gcc (:title graph-options) 10 (+ 10 height))
;                     ))
;                 (.translate gc (double (if clef 45 10)) 0.0)
;                 ;(.drawLine gc -5 0 -5 (- (- (get-property-max)) 5))
;                 ;(.drawLine gc -5 0 -5 (+ (- (get-property-min)) 5))
;                 (draw-note-property-graph gc notes property scale-y options)
;                 ))
;             (getPreferredSize []
;               (java.awt.Dimension. 
;                 (.width (.getPreferredSize score-component))
;                 (* (:scale @options-atom) (+ 10 (* (get-scale-y) (get-height)))))); (+ 12 (get-height)))))
;             ; scrollable
;             (getPreferredScrollableViewportSize [] (.getPreferredSize this))
;             (getScrollableBlockIncrement [_ _ _] 500)
;             (getScrollableTracksViewportHeight [] false)
;             (getScrollableTracksViewportWidth [] false)
;             (getScrollableUnitIncrement [_ _ _] 500)
;             ;; graph properties
;             (setHeight [height] (swap! graph-options-atom assoc :height height))
;             (setTitle [title] (swap! graph-options-atom assoc :title title)))]
;     (add-watch options-atom (gensym) (fn [& _] (.revalidate c) (.repaint c)))
;     (add-watch graph-options-atom (gensym) (fn [& _] (.revalidate c) (.repaint c)))
;     c))