(ns director-musices.score.draw-score
  (:use
    (director-musices [utils :only [log]])
    [clojure.java.io :only [resource]])
  (:require [seesaw [core :as ssw]
                    [graphics :as ssw-graphics]])
  (:import (java.awt Font)))

(def line-separation 7)

(def svg-universe (com.kitfox.svg.SVGUniverse.))

(defn init-svg []
  (doseq [id ["BlackNotehead" "whole" "quarter"]]
    (.loadSVG svg-universe (resource (str "score/" id ".svg")))))

(defn get-diagram [id]
  (.getDiagram svg-universe (.toURI (resource id))))

(defn draw-svg [g id]
  (try
    (.render (get-diagram id) g)
    (catch Exception e
      (log :error e (str "unable to render file svg: " id)))))

(defn get-relative-x-offset [i notes]
  (:distance (nth notes (dec i) {:distance 0})))

(defn get-total-note-x-offset [i notes]
  (if-let [offset (reduce + (map :distance (take i notes)))]
    offset 0))

(defn abs [x] (if (< x 0) (- x ) x))

(defn get-note-for-x [x sc] 
  (let [options @(.getOptionsAtom sc)
        x (* (/ 1 (* (:scale options) (:scale-x options)))
             (- x (+ (if (:clef options) 35 0) 10)))
        note-distances (reductions + (concat [0] (map :distance (:notes options))))]
    (ffirst (sort-by second (map-indexed #(vec [%1 (abs (- x %2))]) note-distances)))))

(defn get-height [note]
  (let [p (:pitch note)
        p (if (list? p) (first p) p)] ; chord drawing quickfix
    (- 9 
       (case (first p)
         \C 0, \D 1, \E 2, \F 3
         \G 4, \A 5, \B 6)
       (* 7 (- (read-string (str (last p))) 4)))))

(defn get-y-offset [note]
  (* (get-height note) (/ line-separation 2)))

(defn draw-lines [g]
  (let [bounds (.getClipBounds g)
        gc (.create g)]
    (.setColor gc java.awt.Color/gray)
    (doseq [line (range 5)]
      (let [y (* line-separation line)]
        (.drawLine gc (.x bounds) y (+ (.x bounds) (.width bounds)) y)))))

(defn transform-for-note [g note {:keys [scale-x]}] 
  (if (:rest note)
    (condp <= (:nlength note)
      1    (do (.translate g (double (* (/ (:distance note) 2) scale-x)) (+ line-separation 4.5))
               (.scale g 0.3 0.25))
      1/2  (do (.translate g (double (* (/ (:distance note) 2) scale-x)) (double line-separation))
               (.scale g 0.3 0.25)) 
      1/4  (do (.translate g 0 4))
      1/8  (do (.translate g 0 8)
               (.scale g 1.15 1.15))
      1/16 (do (.translate g 0 2)
               (.scale g 1.15 1.15))
      (do (.translate g 0 2)
        (.scale g 1.15 1.15))
      )
    (cond
      (= (:length note) 4) (do (.translate g 0.0 0.2) (.scale g 1.05 1.05)) 
      :else (do (.scale g 0.28 0.28) (.translate g 0 -74))
      )))

(defn draw-bar [g]
  (.drawLine g -5 0 -5 (* 4 line-separation)))

(defn draw-accidental [g {[_ accidental] :pitch}]
  (if (or (= accidental \#)
          (= accidental \b))
    (let [gc (.create g)]
      (case accidental
        \# (do
             (.scale gc 0.7 0.7)
             (.translate gc -7 -4)
             (draw-svg gc "score/sharp.svg"))
        \b (do 
             (.translate gc -7 -6)
             (draw-svg gc "score/flat.svg"))))))

(defn draw-dots [g {:keys [dot]}]
  (when dot
    (.drawOval g 10 3 1.5 1.5)
    (if (== dot 2)
      (.drawOval g 14 3 1.5 1.5))))
;  (when (or (= length 3)
;            (and (= (class length) clojure.lang.Ratio)
;                 (= 3 (numerator length))))
;    (.drawOval g 10 3 1.5 1.5)))

(defn draw-note-help-lines [g height]
  (let [upper? (< height 0)
        lower? (> height 8)]
    (if (or upper? lower?)
      (doseq [h (range (if upper? 2 10)  
                       (+ (java.lang.Math/abs height) (if lower? 2 0)) 
                       2)]
        (let [y (* h (/ line-separation 2) (if upper? -1 1))]
          (.drawLine g -1.5 y 9 y))))))

(defn hollow-non-exact-notes [g note]
  (if (condp == (:nlength note)
        1 false 1/2 false 1/4 false
        1/8 false 1/16 false 1/32 false
        true)
    (.setComposite g (java.awt.AlphaComposite/getInstance java.awt.AlphaComposite/SRC_OVER 0.5))))

(defn draw-note [g note {:keys [scale] :as options}]
  (let [img
        (condp <= (:nlength note)
          1 "whole"
          1/2 "half"
          1/4 "quarter"
          1/8 "eighth"
          1/16 "sixteenth"
          1/32 "thirtysecond"
          "thirtysecond")
        gc (.create g)]
    (if (:rest note)
      (do (transform-for-note gc note options)
          (draw-svg gc (str "score/" img "_rest.svg")))
      (let [height (get-height note)]
        (let [gc (.create g)
              y-offset (get-y-offset note)]
          (draw-note-help-lines gc height)
          (.translate gc (double 0) (double y-offset))
          (draw-accidental gc note)
          (draw-dots gc note)
          (transform-for-note gc note options)
          (hollow-non-exact-notes gc note)
          (draw-svg gc (str "score/" img ".svg")))))))

(defn draw-notes [g notes {:keys [scale-x] :or {scale-x 1} :as options}]
  (let [gc (.create g)]
    (doseq [i (range (count notes))]
      (.translate gc (double (* (get-relative-x-offset i notes) scale-x)) (double 0))
      (let [note (nth notes i)]
        (if (:bar note)
          (draw-bar gc))
        (draw-note gc note options)))))

(defn draw-clef [g clef]
  (let [gc (.create g)]
    (case clef
      \F (do (.scale gc 1.15 1.15)
             (draw-svg gc "score/fclef.svg"))
      \C (do (.scale gc 1.12 1.12)
             (.translate gc 0.0 0.3)
             (draw-svg gc "score/cclef.svg"))
      \G (do (.scale gc 1.15 1.15)
             (.translate gc 0 -7)
             (draw-svg gc "score/gclef.svg")))))

(defn get-score-component-width [notes {:keys [scale scale-x clef]}]
  (* scale-x scale
     (+ (if clef 35 0) 10
        (get-total-note-x-offset (count notes) notes))))

(defn get-notes-distance [notes default-distance]
  (map (fn [note] (if (contains? note :distance)
                    note (assoc note :distance (* default-distance (:length note))))) notes))

(defprotocol scoreProperties
  (setScale [this scale] )
  (setScaleX [this scale-x])
  (setScaleFromHeight [this height])
  (setNotes [this notes])
  (getOptionsAtom [this]))

(defn note-component [note options-atom]
  (let [get-notes #(:notes @options-atom)
        get-heights (fn [] (remove nil? (map #(if (:pitch %)
                                                  (get-y-offset %)) (get-notes))))
        get-lowest (fn [] (- (apply min (cons 0 (get-heights))) 30))
        get-highest (fn [] (+ (apply max (cons (* 5 line-separation) (get-heights))) 10))
        get-height (fn [] (- (get-highest) (get-lowest)))
        c (proxy [javax.swing.JComponent] []
            (paintComponent [g]
              (let [gc (.create g)
                    options @options-atom
                    scale (:scale options)
                    scale-x (:scale-x options)
                    notes (:notes @options-atom)]
                (ssw-graphics/anti-alias gc)
                (.scale gc scale scale)
                (.translate gc 0 (- (get-lowest)))
                (draw-lines gc)
                (draw-note gc note @options-atom)
                ))
            (getPreferredSize [] 
              (let [options @options-atom]
                (java.awt.Dimension. 
                  (* (:distance note) (:scale options) (:scale-x options))
                  ;(get-score-component-width (get-notes) @options-atom)
                  (* (:scale @options-atom) (get-height)))))
            )]
    c))

(defn score-component2 [notes & {:keys [default-distance] :or {default-distance 20} :as opts}] 
  (let [options-atom (atom (merge {:scale 1 :scale-x 1 :default-distance 20
                                   :notes (get-notes-distance notes default-distance)} opts))
        note-components (map #(note-component % options-atom) (:notes @options-atom))
        ;c (ssw/horizontal-panel :items note-components)
        c (proxy [javax.swing.JPanel director_musices.score.draw_score.scoreProperties] []
            (setScale [scale] (swap! options-atom assoc :scale scale))
            (setScaleX [scale-x] (swap! options-atom assoc :scale-x scale-x))
            (setNotes [notes] (println "setNotes(notes) not implemented :("))
            (getOptionsAtom [] options-atom))
        ]
    (.setLayout c (javax.swing.BoxLayout. c javax.swing.BoxLayout/X_AXIS))
    (doseq [n note-components]
      (.add c n))
    (add-watch options-atom (gensym) (fn [& _] (.revalidate c) (.repaint c)))
    c
    ))

(defn score-component [notes & {:keys [default-distance] :or {default-distance 1/8} :as opts}] 
  (let [options-atom (atom (merge {:scale 1 :scale-x 1 :default-distance 1/8
                                   :notes (get-notes-distance notes default-distance)} opts))
        get-notes #(:notes @options-atom)
        get-heights (fn [] (remove nil? (map #(if (:pitch %)
                                                  (get-y-offset %)) (get-notes))))
        get-lowest (fn [] (- (apply min (cons 0 (get-heights))) 30))
        get-highest (fn [] (+ (apply max (cons (* 5 line-separation) (get-heights))) 10))
        get-height (fn [] (- (get-highest) (get-lowest)))
        c (proxy [javax.swing.JComponent javax.swing.Scrollable 
                  director_musices.score.draw_score.scoreProperties] []
            (paintComponent [g]
              (let [gc (.create g)
                    options @options-atom
                    scale (:scale options)
                    scale-x (:scale-x options)
                    notes (get-notes)]
                (ssw-graphics/anti-alias gc)
                ;(.translate gc 0 20)
                (.scale gc scale scale)
                (.translate gc (double 0.0) (double (- (get-lowest))))
                (draw-lines gc)
                (when-let [clef (:clef options)]
                  (.translate gc 5 0)
                  (draw-clef gc clef)
                  (.translate gc 30 0)
                  ;(draw-bar gc)
                  )
                (.translate gc 10 0)
                (draw-notes gc notes options)))
            (getPreferredSize [] 
              (java.awt.Dimension. 
                (get-score-component-width (get-notes) @options-atom)
                (* (:scale @options-atom) (get-height))))
            ; scrollable
            (getPreferredScrollableViewportSize [] (.getPreferredSize this))
            (getScrollableBlockIncrement [_ _ _] 500)
            (getScrollableTracksViewportHeight [] false)
            (getScrollableTracksViewportWidth [] false)
            (getScrollableUnitIncrement [_ _ _] 500)
            ; scoreProperties
            (setScale [scale] (swap! options-atom assoc :scale scale))
            (setScaleX [scale-x] (swap! options-atom assoc :scale-x scale-x))
            (setScaleFromHeight [height]
              (swap! options-atom assoc :scale (/ height (get-height))))
            (setNotes [notes] 
              (swap! options-atom assoc :notes 
                (get-notes-distance notes (:default-distance @options-atom))))
            (getOptionsAtom [] options-atom))]
    (add-watch options-atom (gensym) (fn [& _] (.revalidate c) (.repaint c)))
    c))

;; GRAPHS

(defprotocol scoreGraphProperties
  (setHeight [this height] )
  (setTitle [this title]))

(defn draw-note-property-graph [g notes property scale-y {:keys [scale-x] :or {scale-x 1} :as options}]
  (let [gcx (.create g)]
    (doseq [i (range (count notes))]
      (.translate gcx (double (* (get-relative-x-offset i notes) scale-x)) (double 0))
      (let [note (nth notes i)
            note+1 (nth notes (inc i) nil)
            y (* scale-y (- (get note property 0)))
            ]
          (.fillOval gcx -2 (- y 2) 4 4)
        (when note+1
          (let [y (* scale-y (- (get note property 0)))]
            (.drawLine gcx 
                       0 y
                       (* (get-relative-x-offset (inc i) notes) scale-x) (* scale-y (- (get note+1 property 0))))))
        ;(.drawString gcx (str (get note property)) (double 3) (double (- (get note property))))
        ))))

(defn draw-graph-height-lines [g width scale-y value-max value-min]
  (let [gc (.create g)
        gc-text (.create gc)
        step (/ (max value-max (Math/abs value-min)) 10)
        draw-height-line (fn [h height pre]
                           (if (and (< h 9) (odd? h))
                             (let [text (str pre (apply str (take 5 (str (* h step)))))]
                               (.drawString gc-text text (float 40) (float (- (* (/ 1 0.8) height) 1)))))
                           (.drawLine gc 0 height width height))
        ]
    (.setColor gc (java.awt.Color. 200 200 200))
    (.setColor gc-text (java.awt.Color. 50 50 50))
    (.scale gc-text 0.8 0.8)
    (doseq [h (range 0 11)]
      (let [height (* scale-y h step)]
        (draw-height-line h (- height) "")
        (draw-height-line h height "-")))))

(defn score-graph-component [property score-component & {:as graph-opts}]
  (let [options-atom (.getOptionsAtom score-component)
        graph-options-atom (atom (merge {:title (str property) :height 50} graph-opts))
        get-notes (fn [] (:notes @options-atom))
        get-width (fn [] (get-score-component-width (get-notes) @options-atom))
        get-property-max (fn [] (apply max (remove nil? (concat [0] (map #(get % property) (get-notes))))))
        get-property-min (fn [] (apply min (remove nil? (concat [0] (map #(get % property) (get-notes))))))
        get-height (fn [] (- (get-property-max) (get-property-min)))
        get-scale-y (fn [] 
                      (let [height (get-height)]
                        (if (== height 0)
                          1
                          (/ (:height @graph-options-atom)
                             height))))
        c (proxy [javax.swing.JComponent javax.swing.Scrollable] []
            (paintComponent [g]
              (let [gc (.create g)
                    options @options-atom
                    notes (:notes options)
                    scale (:scale options)
                    clef (:clef options)
                    graph-options @graph-options-atom
                    scale-y (get-scale-y)
                    scaled-width (* scale (get-width))
                    ]
                (ssw-graphics/anti-alias gc)
                (.scale gc scale scale)
                (.translate gc 0.0 (double (+ (* scale-y (get-property-max)) 5)))
                (draw-graph-height-lines gc scaled-width scale-y (get-property-max) (get-property-min))
                (.drawLine gc 0 0 scaled-width 0)
                (let [gcc (.create g)]
                  (.scale gcc 1.2 1.2)
                  (.setColor gcc (java.awt.Color. 0 0 0))
                  (let [text (:title graph-options)
                        metrics (.getFontMetrics gcc)
                        width (.stringWidth metrics text)
                        height (.getHeight metrics)]
                    ;(.fillRect gcc 5 (- -15 height) (+ 10 width) (+ 10 height))
                    (.fillRect gcc 5 5 (+ 10 width) (+ 10 height))
                    (.setColor gcc (java.awt.Color. 255 255 255))
                    (.drawString gcc (:title graph-options) 10 (+ 10 height))
                    ))
                (.translate gc (double (if clef 45 10)) 0.0)
                ;(.drawLine gc -5 0 -5 (- (- (get-property-max)) 5))
                ;(.drawLine gc -5 0 -5 (+ (- (get-property-min)) 5))
                (draw-note-property-graph gc notes property scale-y options)
                ))
            (getPreferredSize []
              (java.awt.Dimension. 
                (.width (.getPreferredSize score-component))
                (* (:scale @options-atom) (+ 10 (* (get-scale-y) (get-height)))))); (+ 12 (get-height)))))
            ; scrollable
            (getPreferredScrollableViewportSize [] (.getPreferredSize this))
            (getScrollableBlockIncrement [_ _ _] 500)
            (getScrollableTracksViewportHeight [] false)
            (getScrollableTracksViewportWidth [] false)
            (getScrollableUnitIncrement [_ _ _] 500)
            ;; graph properties
            (setHeight [height] (swap! graph-options-atom assoc :height height))
            (setTitle [title] (swap! graph-options-atom assoc :title title)))]
    (add-watch options-atom (gensym) (fn [& _] (.revalidate c) (.repaint c)))
    (add-watch graph-options-atom (gensym) (fn [& _] (.revalidate c) (.repaint c)))
    c))

(defn show-test-component []
;  (init-svg)
  (let [test-score [{:length 1/4  :rest true   :bar true :t -10}
                    {:length 1/2 :rest true   :pitch "A#4" :bar true :t 0} 
                    {:length 1   :pitch "A4"  :rest true :t 10}
                    {:length 2   :pitch "Db4" :t 20} 
                    {:length 1   :pitch "A4"  :bar true :t 10}]
        test-score2
        '(
         {:length 3, :pitch nil, :bar 1, :n (nil 3/4), :rest t, :meter (3 4), :mm 90} 
;         {:length 3, :pitch nil, :bar 2, :n (nil 3/4), :phrase-start (4 5 6 7), :rest t} 
;         {:length 3, :pitch nil, :bar 3, :n (nil 3/4), :rest t} 
;         {:length 3, :pitch nil, :bar 4, :n (nil 3/4), :rest t} 
;         {:length 3, :pitch nil, :bar 5, :n (nil 3/4), :rest t} 
;         {:length 3, :pitch nil, :bar 6, :n (nil 3/4), :rest t} 
;         {:length 3, :pitch nil, :bar 7, :n (nil 3/4), :rest t}
;         {:length 3, :pitch nil, :bar 8, :n (nil 3/4), :rest t}
;         {:length 3, :pitch nil, :bar 9, :n (nil 3/4), :rest t} 
;         {:length 3, :pitch nil, :bar 10, :n (nil 3/4), :rest t} 
;         {:length 3, :pitch nil, :bar 11, :n (nil 3/4), :rest t} 
;         {:length 3, :pitch nil, :bar 12, :n (nil 3/4), :rest t} 
;         {:length 3, :pitch nil, :bar 13, :n (nil 3/4), :rest t} 
;         {:length 3, :pitch nil, :bar 14, :n (nil 3/4), :rest t} 
;         {:length 3, :pitch nil, :bar 15, :n (nil 3/4), :rest t}
;         {:length 2, :pitch nil, :bar 16, :n (nil 1/2), :rest t}
         {:length 1, :pitch "F#4", :n (F#4 1/4)}
         {:length 1, :pitch "F#4", :bar 17, :n (F#4 1/4), :phrase-end (4 5 6 7)}
;         {:length 1, :pitch nil, :n (nil 1/4), :rest t} {:length 1, :pitch nil, :n (nil 1/4), :rest t}
         )
        sc (score-component [] :clef \F)]
    (.setNotes sc test-score)
    (.setScale sc 2)
    (.setScaleX sc 3)
    (ssw/show! (ssw/frame 
                 :size [500 :by 200]
                 :content 
                 (ssw/vertical-panel
                   :items [
                           (ssw/scrollable sc)
                           (ssw/scrollable
                             (score-graph-component :t sc))
                           ])))))
