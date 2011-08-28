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
  (if-let [offset (reduce + (map :distance (take i notes)))]
    offset 0))

(defn get-height [note]
  (- 9 
     (case (first (:pitch note))
       \C 0, \D 1, \E 2, \F 3
       \G 4, \A 5, \B 6)
     (* 7 (- (read-string (str (last (:pitch note)))) 4))))

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
    (condp >= (:length note)
      1/4 (do (.translate g 0 2)
              (.scale g 1.15 1.15))
      1/2 (do (.translate g 0 8)
              (.scale g 1.15 1.15))
      1 (do (.translate g 0 4))
      2 (do (.translate g (double (* (/ (:distance note) 2) scale-x)) (double line-separation))
            (.scale g 0.3 0.25)) 
      4 (do (.translate g (double (* (/ (:distance note) 2) scale-x)) (+ line-separation 4.5))
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
    (.drawOval g 10 3 1.5 1.5)))

(defn draw-note [g note {:keys [scale] :as options}]
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
      (do (transform-for-note gc note options)
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
          (transform-for-note gc note options)
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

(defn get-notes-distance [notes default-distance]
  (map (fn [note] (if (contains? note :distance)
                    note (assoc note :distance (* default-distance (:length note))))) notes))

(defprotocol scoreProperties
  (setScale [this scale] )
  (setScaleX [this scale-x])
  (setNotes [this notes])
  (getOptionsAtom [this]))

(defn score-component [notes & {:keys [default-distance] :or {default-distance 20} :as opts}] 
  (let [options-atom (atom (merge {:scale 1 :scale-x 1 :default-distance 20
                                   :notes (get-notes-distance notes default-distance)} opts))
        get-notes #(:notes @options-atom)
        get-heights (fn [] (remove nil? (map #(if (:pitch %)
                                                  (get-y-offset %)) (get-notes))))
        get-lowest (fn [] (- (apply min (cons 0 (get-heights))) 30))
        get-highest (fn [] (+ (apply max (cons (* 5 line-separation) (get-heights))) 10))
        get-height (fn [] (- (get-highest) (get-lowest)))
        c (proxy [javax.swing.JComponent javax.swing.Scrollable 
                  director_musices.draw_score.scoreProperties] []
            (paintComponent [g]
              (let [gc (.create g)
                    options @options-atom
                    scale (:scale options)
                    scale-x (:scale-x options)
                    notes (get-notes)]
                (ssw-graphics/anti-alias gc)
                ;(.translate gc 0 20)
                (.scale gc scale scale)
                (.translate gc 0 (- (get-lowest)))
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
            note+1 (nth notes (inc i) nil)]
        (if note+1
          (.drawLine gcx 
            0 (* scale-y (- (get note property)))
            (* (get-relative-x-offset (inc i) notes) scale-x) (* scale-y (- (get note+1 property)))))
        ;(.drawString gcx (str (get note property)) (double 3) (double (- (get note property))))
        ))))

;(defn draw-coordinate-system [g property-max property-min]

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
                    scale-y (get-scale-y)]
                (ssw-graphics/anti-alias gc)
                (.drawString gc (:title graph-options) 10 15)
                (.scale gc scale scale)
                ;(.translate gc 0.0 (double (* scale-y height)))
                (.translate gc 0.0 (double (* scale-y (get-property-max))))
                (let [b (.getClipBounds g)]
                  (.drawLine gc (.x b) 0 (+ (.x b) (.width b)) 0))
                (.translate gc (double (if clef 45 10)) 0.0)
                (.drawLine gc -5 0 -5 (- (- (get-property-max)) 5))
                (.drawLine gc -5 0 -5 (+ (- (get-property-min)) 5))
                (draw-note-property-graph gc notes property scale-y options)
                ))
            (getPreferredSize []
              (java.awt.Dimension. 
                (.width (.getPreferredSize score-component))
                (* (:scale @options-atom) (get-scale-y) (+ 12 (get-height)))))
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
