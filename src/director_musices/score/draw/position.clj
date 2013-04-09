(ns director-musices.score.draw.position
  (:require (director-musices [player :as player])
            (director-musices.score.draw [track :as draw-track])
            [seesaw.core :as ssw]))

;; =====
;; Display position
;; =====
(defn paint [this position track-component g]
  (let [g (.create g)
        s (.getSize this)
        track-view (draw-track/get-view track-component)
        w (.getWidth track-view)
        scale (draw-track/get-scale track-component)
        scale-x (draw-track/get-scale-x track-component)
        first-note-offset (* scale draw-track/first-note-offset)]
    (.translate g (.getX track-view) 0)
    (.translate g (double first-note-offset) 0.0)
    (.setColor g java.awt.Color/red)
    (.translate
      g (double (* position (- w first-note-offset)))
      (double 0))
    (.drawLine g 0 0 0 (.getHeight this))))

(defn position-component [track-component]
  (let [position (atom 0)
        c (proxy [javax.swing.JComponent] []
            (paintComponent [g]
              (paint this @position track-component g)
              ))
        repaint #(do (.revalidate c) (.repaint c))]
    (add-watch position nil (fn [& _] (repaint)))
    {:view c
     :position position}))

(defn get-view [pc] (:view pc))

(defn set-position-indicator [pc position]
  (reset! (:position pc) position))

;; =====
;; Set position
;; =====
(defn draw-position-ticks [g x-info track-component]
  (when-let [s (player/get-sequencer)]
    (let [g (.create g)
          {:keys [x width]} x-info
          seconds (/ (.getMicrosecondLength s)
                     1e6)]
      (.setColor g java.awt.Color/black)
      (.translate g x 0)
      (doseq [second-pos (range (inc seconds))]
        (let [pos (* (/ second-pos seconds)
                     width)]
          (.drawLine g pos 0 pos 15)
          ; (cond
          ;   (zero? (rem pos 5))
          ;    (.drawLine g pos 0 pos 15)
          ;   )
          )))))

(defn position-setter-component [track-component score-view set-position]
  (let [x-info (fn [c]
                 (let [w (.getWidth c)
                       tw (.getWidth (draw-track/get-view
                                      track-component))
                       tw (- tw draw-track/first-note-offset)
                       x (- w tw)
                       w (- w x)]
                   {:width w
                    :x x}))
        c (proxy [javax.swing.JComponent] []
            (paintComponent [g]
              (.setColor g (java.awt.Color. 200 200 200))
              (.fillRect g 0 0
                         (.getWidth this)
                         (.getHeight this))
              ;(draw-position-ticks g (x-info this) track-component)
              )
            (getPreferredSize []
              (java.awt.Dimension.
                (.width (.getPreferredSize score-view))
                15)))
        set-position-from-x
        (fn [in-x]
          (let [{:keys [x width]} (x-info c)]
            (set-position (/ (- in-x x)
                             width))))]
    (ssw/listen c :mouse-clicked
                (fn [e] (set-position-from-x (.getX e))))
    c))
