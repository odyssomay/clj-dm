(ns director-musices.score.draw.parameter
  (:require (director-musices.score.draw
              [track :as draw-track])))

(defn draw-properties [g state]
  (let [g (.create g)
        {:keys [track-component parameter]} state
        notes (draw-track/get-notes track-component)
        scale-x (draw-track/get-scale-x track-component)]
    (.translate g draw-track/first-note-offset 0)
    (doseq [i (range (count notes))]
      (let [note (nth notes i)
            value (get note parameter nil)
            value (if (number? value) (int value) value)]
        (.translate g (double (* (:x-offset note) scale-x))
                    (double 0))
        (when value
          (.drawString g (pr-str value) 0 0))))))

(defn paint [g state]
  (let [{:keys [track-component parameter]} state]
    (draw-properties g state)))

(defn parameter-component [track-component parameter & {:as opts}]
  (let [state (atom {:track-component track-component
                     :parameter parameter})
        c (proxy [javax.swing.JComponent] []
            (paintComponent [g]
              (let [g (.create g)
                    s (.getSize this)
                    fm (.getFontMetrics g)]
                (.setColor g java.awt.Color/red)
                ;(.fillRect g 0 0 (.width s) (.height s))
                (.translate g 0 (int (+ (/ (.height s) 2)
                                        (.getDescent fm))))
                (.drawString g (name parameter) 5 0)
                (.setColor g java.awt.Color/black)
                (paint g @state)))
            (getPreferredSize []
              (java.awt.Dimension.
                (.width (.getPreferredSize (draw-track/get-view track-component)))
                (* (draw-track/get-scale track-component) 20))))]
    {:view c
     :state state}))

(defn get-view [pc] (:view pc))
