(ns director-musices.load-mus
  (:use clojure.tools.logging))

(defn load-mus-notes [raw_notes]
  (map
    #(into {} (map (fn [[option v]]
                       [(keyword option) v])
                       (partition 2 %)))
    raw_notes))

(defn load-mus [coll]
  (let [raw_tracks (->> (partition-by (partial = 'mono-track) coll)
                        (remove #(= (first %) 'mono-track)))
        opt_tracks (map (fn [track]
                          (let [splitted (split-with #(keyword? (first %)) (partition 2 track))]
                           {:options (into {} (map vec (first splitted)))
                            :notes (vec (load-mus-notes (apply concat (second splitted))))}))
                        raw_tracks)]
    (vec opt_tracks)))

