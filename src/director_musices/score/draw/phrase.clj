(ns director-musices.score.draw.phrase
  (:require (director-musices.score.draw
              [track :as draw-track])
            (seesaw [graphics :as ssw-graphics])))

(def phrase-height 5)

(def colors
  (cycle (map #(java.awt.Color/decode %)
              ["#AC2400"
               "#310075"
               "#AC9B00"
               "#007B35"])))

(defn draw-phrases [g state]
  (let [g (.create g)
        {:keys [track-component level-map
                levels max-heights]} state
        scale-x (draw-track/get-scale-x track-component)
        minlevel (reduce min levels)
        maxlevel (reduce max levels)]
    (.translate g draw-track/first-note-offset 3)
    (doseq [level levels]
      (let [current-level (get level-map level)]
        (doseq [i (range (count current-level))]
          (let [{:keys [start end height]} (nth current-level i nil)
                start-offset (* (or (:absolute-x-offset start) 0) scale-x)
                end-offset (* (or (:absolute-x-offset end) 0) scale-x)
                g (.create g)]
            (.translate g 0 (* 2 height))
            (.setColor g (let [c (nth colors level height)]
                           (if (zero? (mod height 2))
                             c (.darker c))))
            (if (and start end)
              (.drawLine g (+ phrase-height start-offset) 0 end-offset 0)
              (.setColor g java.awt.Color/red))
            (if start (.drawLine g start-offset phrase-height
                                 (+ start-offset phrase-height)
                                 0))
            (if end (.drawLine g end-offset 0
                               (+ end-offset phrase-height)
                               phrase-height)))))
      (.translate g 0
                  (+ phrase-height
                     (* 2 (get max-heights level)))))))

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
  (let [{:keys [phrase-marks
                phrase-starts]}
        (reduce (fn [m note]
                  (case (:phrase-mark note)
                    :phrase-start (update-in m [:phrase-starts] conj note)
                    :phrase-end (if (empty? (:phrase-starts m))
                                  (update-in m [:phrase-marks] conj {:end note})
                                  (-> m
                                      (update-in [:phrase-marks] conj
                                                 {:start (peek (:phrase-starts m))
                                                  :end note})
                                      (update-in [:phrase-starts] pop)))))
                {:phrase-marks []
                 :phrase-starts (clojure.lang.PersistentQueue/EMPTY)}
                notes)]
    (concat phrase-marks
            (map #(hash-map :start %) phrase-starts))))

(defn phrases-overlap? [phrase1 phrase2]
  (let [{start1 :start end1 :end} phrase1
        {start2 :start end2 :end} phrase2]
    (if (and start1 end1 start2 end2)
      (let [xstart1 (:absolute-x-offset start1)
            xend1   (:absolute-x-offset end1)
            xstart2 (:absolute-x-offset start2)
            xend2   (:absolute-x-offset end2)]
        (or (and (>= xstart2 xstart1)
                 (<= xstart2 xend1))
            (and (>= xend2 xstart1)
                 (<= xend2 xend1)))))))

(defn phrase-fits? [phrase phrases]
  (let [phrases (filter #(= (:height %)
                            (:height phrase))
                        phrases)]
    (every? #(not (phrases-overlap? % phrase))
            phrases)))

(defn find-phrase-height [phrase phrases]
  (or (first (filter #(phrase-fits? (assoc phrase :height %)
                                    phrases)
                     (range 10)))
      0))

(defn add-phrase-heights [phrases]
  (reduce (fn [phrases phrase]
            (conj phrases
                  (assoc phrase :height
                    (find-phrase-height phrase phrases))))
          [] phrases))

(defn calculate-max-heights [level-map]
  (reduce (fn [max-heights [k v]]
            (assoc max-heights k
              (reduce max (map :height v))))
          {} level-map))

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
                  (assoc level-map k
                    (->> v
                         (sort-by :absolute-x-offset)
                         partition-notes
                         add-phrase-heights)))
                {}
                level-map)]
    {:level-map sorted-level-map
     :levels (sort (keys level-map))
     :max-heights (calculate-max-heights sorted-level-map)}))

(defn calculate-phrases [state]
  (let [{:keys [track-component]} state
        notes (draw-track/get-notes track-component)]
    (merge state (sort-phrases notes))))

(defn calculate-total-height [state]
  (assoc state :total-height
    (reduce (fn [total-height max-height]
              (+ total-height phrase-height (* max-height 2)))
            0 (vals (:max-heights state)))))

(defn calculate-state [state]
  (-> state
      calculate-phrases
      calculate-total-height))

(defn phrase-component [track-component]
  (let [state (atom (-> {:track-component track-component}
                        calculate-state))
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
                      (:total-height @state))
                   6))))]
    (draw-track/on-track-change
      track-component (fn [] (swap! state calculate-state)))
    {:view c
     :state state}))

(defn get-view [pc] (:view pc))
