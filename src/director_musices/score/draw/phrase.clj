(ns director-musices.score.draw.phrase
  (:require (director-musices.score.draw
              [track :as draw-track])
            (seesaw [graphics :as ssw-graphics])))

(def phrase-height 5)

(defn draw-phrases [g state]
  (let [g (.create g)
        {:keys [track-component level-map levels]} state
        scale-x (draw-track/get-scale-x track-component)
        minlevel (reduce min levels)
        maxlevel (reduce max levels)]
    (.translate g draw-track/first-note-offset 3)
    (doseq [level levels]
      (doseq [{:keys [start end]} (get level-map level)]
        (let [start-offset (* (or (:absolute-x-offset start) 0) scale-x)
              end-offset (* (or (:absolute-x-offset end) 0) scale-x)]
          (.setColor g java.awt.Color/black)
          (if (and start end)
            (.drawLine g (+ phrase-height start-offset) 0 end-offset 0)
            (.setColor g java.awt.Color/red))
          (if start
            (.drawLine g
                       start-offset phrase-height
                       (+ start-offset phrase-height) 0))
          (if end
            (.drawLine g
                       end-offset 0
                       (+ end-offset phrase-height) phrase-height))
          ))
      (.translate g 0 phrase-height))))

(defn paint [c g state]
  (let [{:keys [track-component parameter]} state
        scale (draw-track/get-scale track-component)]
    (.setColor g java.awt.Color/black)
    (when-not (zero? (count (:levels state)))
      (draw-phrases g state))))

(defn create-level-map [k notes]
  (reduce (fn [level-map note]
            (let [levels (k note)
                  levels (if (sequential? levels)
                           levels
                           (list levels))
                  m (into {} (map #(vector % [(assoc note
                                                :phrase-mark k)])
                                  levels))]
              (merge-with concat level-map m)))
          {}
          notes))

(defn partition-notes [notes]
  (let [{phrase-starts :phrase-start
         phrase-ends :phrase-end}
        (group-by :phrase-mark notes)
        cps (count phrase-starts)
        cpe (count phrase-ends)
        ;; make sure the collections
        ;; are of equal size
        phrase-starts (concat phrase-starts
                              (repeat (max 0 (- cpe cps))
                                      nil))
        phrase-ends (concat phrase-ends
                            (repeat (max 0 (- cps cpe))
                                    nil))]
    ;; and group them together!
    (map (fn [start end] {:start start :end end})
         phrase-starts phrase-ends)))

(defn sort-phrases [notes]
  (let [filtered (filter #(or (:phrase-start %)
                              (:phrase-end %)) notes)
        start-notes (filter :phrase-start notes)
        end-notes (filter :phrase-end notes)
        level-map (merge-with concat
                              (create-level-map :phrase-start start-notes)
                              (create-level-map :phrase-end end-notes))
        sorted-level-map
        (reduce (fn [level-map [k v]]
                  (assoc level-map k (partition-notes (sort-by :absolute-x-offset v))))
                {}
                level-map)]
    {:level-map sorted-level-map
     :levels (sort (keys level-map))}))

(defn calculate-phrases [state]
  (let [{:keys [track-component]} state
        notes (draw-track/get-notes track-component)]
    (merge state (sort-phrases notes))))

(defn phrase-component [track-component]
  (let [state (atom (-> {:track-component track-component}
                        calculate-phrases))
        c (proxy [javax.swing.JComponent] []
            (paintComponent [g]
              (let [g (.create g)
                    scale (draw-track/get-scale track-component)]
                (.scale g scale scale)
                (paint this g @state)))
            (getPreferredSize []
              (java.awt.Dimension.
                (.width (.getPreferredSize (draw-track/get-view
                                             track-component)))
                (+ (* (draw-track/get-scale track-component)
                      phrase-height
                      (count (:levels @state)))
                   6))))]
    (draw-track/on-state-change
      track-component (fn [] (swap! state calculate-phrases)))
    {:view c
     :state state}))

(defn get-view [pc] (:view pc))
