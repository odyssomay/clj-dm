(ns director-musices.draw-score
  (:use
    (director-musices [utils :only [log]])
    [clojure.java.io :only [resource]])
  (:require [seesaw [core :as ssw]
                    [graphics :as ssw-graphics]])
  (:import (java.awt Font)))

(def line-separation 7)

(def svg-universe
  (com.kitfox.svg.SVGUniverse.))

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
  (reduce + (map :distance (take i notes))))

(defn get-height [note]
  (- 9 
     (case (first (:pitch note))
       \C 0, \D 1, \E 2, \F 3
       \G 4, \A 5, \B 6)
     (* 7 (- (read-string (str (last (:pitch note)))) 4))))

(defn get-y-offset [note]
  (* (get-height note) (/ line-separation 2)))

(defn- draw-lines [g]
  (let [bounds (.getClipBounds g)
        gc (.create g)]
    (.setColor gc java.awt.Color/gray)
    (doseq [line (range 5)]
      (let [y (* line-separation line)]
        (.drawLine gc (.x bounds) y (+ (.x bounds) (.width bounds)) y)))))

(defn transform-for-note [g note] 
  (if (:rest note)
    (condp >= (:length note)
      1/2 (do (.translate g 0 10))
      1 (do (.translate g 0 4))
      2 (do (.translate g (double (/ (:distance note) 2)) (double line-separation))
            (.scale g 0.3 0.25)) 
      4 (do (.translate g (double (/ (:distance note) 1)) (+ line-separation 4.5))
            (.scale g 0.3 0.25)))
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

(defn draw-dots [g {:keys [length]}]
  (when (or (= length 3)
            (and (= (class length) clojure.lang.Ratio)
                 (= 3 (numerator length))))
    (.drawOval g 10 3 1 1)))

(defn draw-note [g note {:keys [scale]}]
  (let [img
        (some (fn [[length id]]
                (if (>= (:length note) length)
                  id))
              (partition 2 [4   "whole"
                            2   "half"
                            1   "quarter"
                            1/2 "eighth"
                            1/4 "sixteenth"
                            1/8 "thirtysecond"]))
        gc (.create g)]
    (if (:rest note)
      (do (transform-for-note gc note)
          (draw-svg gc (str "score/" img "_rest.svg")))
      (let [height (get-height note)]
        (if (and (or (< height 0)
                     (> height 7))
                 (odd? height))
          nil)
        ;(.drawLine g 0 0 0 20)
        (let [gc (.create g)]
          (draw-accidental gc note)
          (draw-dots gc note)
          (transform-for-note gc note)
          (.render (.getDiagram svg-universe (.toURI (resource (str "score/" img ".svg")))) gc))))))

(defn draw-notes [g notes {:keys [scale-x] :or {scale-x 1} :as options}]
  (let [gcx (.create g)]
    (doseq [i (range (count notes))]
      (.translate gcx (double (* (get-relative-x-offset i notes) scale-x)) (double 0))
      (let [note (nth notes i)
            gcy (.create gcx)]
        (if (:bar note)
          (draw-bar gcy))
        (if-not (:rest note)
          (.translate gcy (double 0) (double (get-y-offset note))))
        (draw-note gcy note options)))))

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

(defprotocol scoreProperties
  (setScale [this scale] )
  (setScaleX [this scale-x]))

(defn score-component [notes & {:as opts}] 
  (let [{:keys [scale scale-x clef default-distance title] :as options} 
        (merge {:scale 1 :scale-x 1 :default-distance 20} opts)
        scale-atom (atom scale)
        scale-x-atom (atom scale-x)
        notes (map (fn [note] (if (contains? note :distance)
                                note (assoc note :distance (* default-distance (:length note))))) notes)
        heights (remove nil? (map #(if (:pitch %)
                                       (get-y-offset %)) notes))
        lowest (- (min (apply min heights) 0) 30)
        highest (max (apply max heights) (* 5 line-separation))
        height (- highest lowest ;(Math/abs highest) (Math/abs lowest) ;(* 5 line-separation)
                  )]
    (proxy [javax.swing.JComponent javax.swing.Scrollable 
            director_musices.draw_score.scoreProperties] []
      (paintComponent [g]
        (let [gc (.create g)
              scale @scale-atom
              scale-x @scale-x-atom
              options (merge options {:scale scale :scale-x scale-x})]
          (ssw-graphics/anti-alias gc)
          ;(.translate gc 0 20)
          (.scale gc scale scale)
          (.translate gc 0 (- lowest))
          (draw-lines gc)
          (when clef
            (.translate gc 5 0)
            (draw-clef gc clef)
            (.translate gc 30 0)
            ;(draw-bar gc)
            )
          (.translate gc 10 0)
          (draw-notes gc notes options)))
      (getPreferredSize [] 
        (java.awt.Dimension. (get-score-component-width notes options) (* scale height)))
      ; scrollable
      (getPreferredScrollableViewportSize [] (.getPreferredSize this))
      (getScrollableBlockIncrement [_ _ _] 500)
      (getScrollableTracksViewportHeight [] false)
      (getScrollableTracksViewportWidth [] false)
      (getScrollableUnitIncrement [_ _ _] 500)
      ; scoreProperties
      (setScale [this scale] )
                ;(reset! scale-atom scale))
      (setScaleX [this scale-x] 
;                 (reset! scale-x-atom scale-x)
                 ))))

;; GRAPHS

(defn draw-note-property-graph [g notes property {:keys [scale-x] :or {scale-x 1} :as options}]
  (let [gcx (.create g)]
    (doseq [i (range (count notes))]
      (.translate gcx (* (get-relative-x-offset i notes) scale-x) 0)
      (let [note (nth notes i)]
        (.drawOval gcx 0 (- (get note property)) 2 2)
        (.drawString gcx (str (get note property)) 3 (- (get note property)))
        ))))

;(defn draw-coordinate-system [g property-max property-min]

(defn score-graph-component [notes property & {:as opts}]
  (let [{:keys [scale scale-x clef default-distance title] :as options} 
        (merge {:scale 1 :scale-x 1 :default-distance 20} opts)

        notes (map (fn [note] (if (contains? note :distance)
                                note (assoc note :distance (* default-distance (:length note))))) notes)
        width (get-score-component-width notes options)
        property-max (apply max (map #(get % property) notes))
        property-min (apply min (map #(get % property) notes))
        height (+ 12 (- property-max property-min))]
    (proxy [javax.swing.JComponent javax.swing.Scrollable] []
      (paintComponent [g]
        (let [gc (.create g)]
          (ssw-graphics/anti-alias gc)
          (if title
            (.drawString gc title 10 10)
            (.drawString gc (str property) 10 10))
          (.scale gc scale scale)
          (let [b (.getClipBounds g)]
            (.drawLine gc 0 height (.width b) height)) 
          (.translate gc (if clef 45 10) height)
          (draw-note-property-graph gc notes property options)
          ))
      (getPreferredSize [] (java.awt.Dimension. (get-score-component-width notes options) height))
      ; scrollable
      (getPreferredScrollableViewportSize [] (.getPreferredSize this))
      (getScrollableBlockIncrement [_ _ _] 500)
      (getScrollableTracksViewportHeight [] false)
      (getScrollableTracksViewportWidth [] false)
      (getScrollableUnitIncrement [_ _ _] 500)
       )))

(defn show-test-component []
;  (init-svg)
  (let [test-score2 [{:length 1/2 :rest true   :bar true}
                    {:length 3/4   :pitch "A#0" :bar true} 
                    {:length 1   :pitch "A0"  :rest true}
                    {:length 2   :pitch "Db0"} 
                    {:length 1   :pitch "A0"  :bar true}]
        test-score
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
         )]
    (ssw/show! (ssw/frame 
                 :size [500 :by 200]
                 :content 
                 (ssw/vertical-panel
                   :items [
                           (ssw/scrollable
                             (score-component test-score :scale 2 :scale-x 2 :clef \C))
                           ;(ssw/scrollable
                             (score-graph-component test-score :distance :scale 2 :scale-x 2 :clef \C :title "blablablablablablablabla");)
                           ])))))
