(ns director-musices.score.draw.track
  (:use [clojure.java.io :only [resource]])
  (:require (director-musices.score.draw
              [calculate :as calc]
              interfaces)
            [taoensso.timbre :as log]
            [seesaw [core :as ssw]
                    [graphics :as ssw-graphics]]))

(defn abs [x] (if (< x 0) (- x ) x))

(defn get-note-for-x [x sc] 
  (let [options @(.getOptionsAtom sc)
        x (* (/ 1 (* (:scale options) (:scale-x options)))
             (- x (+ (if (:clef options) 35 0) 10)))
        note-distances (reductions + (concat [0] (map :x-offset (:notes options))))]
    (ffirst (sort-by second (map-indexed #(vec [%1 (abs (- x %2))]) note-distances)))))

(def line-separation calc/line-separation)

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
      (log/error "unable to render file svg:" id ", error:" e))))

(defn transform-for-note [g note {:keys [scale-x]}] 
  (if (:rest note)
    (condp <= (:nlength note)
      1    (do (.translate g (double (* (/ (:x-offset note) 2) scale-x)) (+ line-separation 4.5))
               (.scale g 0.3 0.25))
      1/2  (do (.translate g (double (* (/ (:x-offset note) 2) scale-x)) (double line-separation))
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

(defn draw-lines [g]
  (let [bounds (.getClipBounds g)
        gc (.create g)]
    (.setColor gc java.awt.Color/gray)
    (doseq [line (range 5)]
      (let [y (* line-separation line)]
        (.drawLine gc (.x bounds) y (+ (.x bounds) (.width bounds)) y)))))

(defn draw-note-help-lines [g height]
  (let [upper? (< height 0)
        lower? (> height 8)]
    (if (or upper? lower?)
      (doseq [h (range (if upper? 2 10)  
                       (+ (java.lang.Math/abs height) (if lower? 2 0)) 
                       2)]
        (let [y (* h (/ line-separation 2) (if upper? -1 1))]
          (.drawLine g -1.5 y 9 y))))))

(defn set-hollow [g note]
  (when (:hollow? note)
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
      (let [height (:height note)]
        (let [gc (.create g)
              y-offset (:y-offset note)]
          (draw-note-help-lines gc height)
          (.translate gc (double 0) (double y-offset))
          (draw-accidental gc note)
          (draw-dots gc note)
          (transform-for-note gc note options)
          (set-hollow gc note)
          (draw-svg gc (str "score/" img ".svg")))))))

(defn draw-notes [g notes {:keys [scale-x] :or {scale-x 1} :as options}]
  (let [gc (.create g)]
    (doseq [i (range (count notes))]
      (let [note (nth notes i)]
        (.translate gc (double (* (:x-offset note) scale-x)) (double 0))
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
  (* scale
     (+ (if clef 35 0) 10
        (* scale-x
           (let [ln (last notes)]
             (+ (:absolute-x-offset ln)
                (:length ln)))))))

(defn score-component [notes & {:keys [default-distance] :or {default-distance 1/8} :as opts}] 
  (let [options-atom (atom (merge {:scale 1 :scale-x 1 :default-distance 1/8
                                   :notes (calc/calculate-notes notes)} opts))
        get-notes #(:notes @options-atom)
        get-heights (fn [] (remove nil? (map #(if (:pitch %)
                                                  (:y-offset %)) (get-notes))))
        get-lowest (fn [] (- (apply min (cons 0 (get-heights))) 30))
        get-highest (fn [] (+ (apply max (cons (* 5 line-separation) (get-heights))) 10))
        get-height (fn [] (- (get-highest) (get-lowest)))
        c (proxy [javax.swing.JComponent javax.swing.Scrollable 
                  director_musices.score.draw.interfaces.scoreProperties] []
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
              (swap! options-atom assoc :notes notes))
            (getOptionsAtom [] options-atom))]
    (add-watch options-atom (gensym) (fn [& _] (.revalidate c) (.repaint c)))
    c))