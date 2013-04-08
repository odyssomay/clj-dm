(ns director-musices.score.draw.position
  (:require (director-musices.score.draw [track :as draw-track])
            [seesaw.core :as ssw]))

;; =====
;; Display position
;; =====
(defn paint [this position track-component g]
  (let [g (.create g)
        s (.getSize this)
        w (.width s)
        h (.height s)
        scale-x (draw-track/get-scale-x track-component)]
    (.translate g draw-track/first-note-offset 0)
    (.setColor g java.awt.Color/red)
    (.translate
      g (double (* position
                   (- w draw-track/first-note-offset)))
      (double 0))
    (.drawLine g 0 0 0 (.height s))))

(defn position-component [track-component]
  (let [position (atom 0)
        c (proxy [javax.swing.JComponent] []
            (paintComponent [g]
              (paint this @position track-component g)
              ))
        repaint #(do (.revalidate c) (.repaint c))]
    (draw-track/on-state-change track-component repaint)
    (add-watch position nil (fn [& _] (repaint)))
    {:view c
     :position position}))

(defn get-view [pc] (:view pc))

(defn set-position-indicator [pc position]
  (reset! (:position pc) position))

;; =====
;; Set position
;; =====
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
              (let [{:keys [x width]} (x-info this)
                    h (.getHeight this)]
                (.setColor g java.awt.Color/red)
                (.fillRect g x 0 width h)))
            (getPreferredSize []
              (java.awt.Dimension.
                (.width (.getPreferredSize score-view))
                15)))
        set-position-from-x
        (fn [in-x]
          (let [{:keys [x width]} (x-info c)]
            (set-position
              (float (/ (- in-x x)
                        width)))))]
    (ssw/listen c :mouse-clicked
                (fn [e] (set-position-from-x (.getX e))))
    c))
