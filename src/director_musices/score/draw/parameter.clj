(ns director-musices.score.draw.parameter
  (:require (director-musices.score.draw
              [track :as draw-track])))

(defn draw-properties [g state]
  (let [g (.create g)
        {:keys [track-component parameter]} state
        notes (draw-track/get-notes track-component)
        scale (draw-track/get-scale track-component)
        scale-x (draw-track/get-scale-x track-component)]
    (.translate g (int (* draw-track/first-note-offset scale)) 0)
    (doseq [i (range (count notes))]
      (let [note (nth notes i)
            value (get note parameter nil)
            value (if (number? value) (int value) value)]
        (.translate g (double (* (:x-offset note) scale scale-x))
                    (double 0))
        (when value
          (.drawString g (pr-str value) 0 0))))))

(defn paint [c g state]
  (let [{:keys [track-component parameter]} state
        scale (draw-track/get-scale track-component)
        s (.getSize c)
        fm (.getFontMetrics g)]
    (.setColor g java.awt.Color/red)
    (.translate g 0 (int (+ (/ (.height s) 2)
                            (.getDescent fm))))
    (.drawString g (name parameter) 5 0)
    (.setColor g java.awt.Color/black)
    (draw-properties g state)))

(defn parameter-component [track-component parameter & {:as opts}]
  (let [state (atom {:track-component track-component
                     :parameter parameter})
        c (proxy [javax.swing.JComponent] []
            (paintComponent [g]
              (let [g (.create g)]
                (paint this g @state)))
            (getPreferredSize []
              (java.awt.Dimension.
                (.width (.getPreferredSize (draw-track/get-view
                                             track-component)))
                20)))]
    (draw-track/on-state-change track-component (fn []
                                                  (.revalidate c)
                                                  (.repaint c)))
    {:view c
     :state state}))

(defn get-view [pc] (:view pc))
