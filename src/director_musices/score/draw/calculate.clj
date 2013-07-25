(ns director-musices.score.draw.calculate)

(def line-separation 7)

(defn rest? [note]
  (boolean (:rest note)))

(defn add-rests [track]
  (assoc track :notes
    (map #(assoc % :rest (rest? %))
         (:notes track))))

(defn pitch->height [track pitch]
  (let [h (- 9
             (case (first pitch)
               \C 0, \D 1, \E 2, \F 3
               \G 4, \A 5, \B 6)
             (* 7 (- (read-string (str (last pitch))) 4)))]
    (case (:clef track)
      \G h
      \F (- h 12))))

(defn note-height [track note]
  (let [p (:pitch note)
        p (if (list? p) (first p) p)]
    (pitch->height track p)))

(defn height->y-offset [height]
  (* height (/ line-separation 2)))

(defn note-y-offset [note]
  (height->y-offset (:height note)))

(defn note-hollow? [note]
  (not (contains? #{1 1/2 1/4 1/8 1/16 1/32}
                  (:nlength note))))

(defn note-chord [track note]
  (let [pitches (:pitch note)]
    (if (list? pitches)
      (seq
        (for [pitch (rest pitches)]
          (let [height (pitch->height track pitch)]
            {:pitch pitch
             :height height
             :y-offset (height->y-offset height)
             :nlength (:nlength note)}))))))

(defn note-data [track note]
  (if (:rest note)
    note
    (let [note (assoc note :height (note-height track note))]
      (assoc note
        :y-offset (note-y-offset note)
        :chord (note-chord track note)
        :hollow? (note-hollow? note)))))

(defn add-absolute-lengths [notes]
  (:new-notes
    (reduce (fn [{:keys [new-notes total-length previous-length]}
                 {:keys [length] :as note}]
              (let [new-length (+ total-length length)]
                {:new-notes (conj new-notes (assoc note
                                              :absolute-x-offset total-length
                                              :x-offset previous-length))
                 :previous-length length
                 :total-length new-length}
                ))
            {:new-notes []
             :previous-length 0
             :total-length 0}
            notes)))

(defn calculate-notes [track]
  (assoc track :notes
    (-> (for [{:keys [dr ndr n rest] :as note} (:notes track)]
          (->> (merge note
                      (when n
                        {:pitch (first n)
                         :length dr
                         :nlength (second n)}))
               (note-data track)))
        add-absolute-lengths)))

(defn add-track-heights [track]
  (let [{:keys [notes]} track
        heights (map :y-offset (remove :rest notes))
        lowest (- (reduce min (cons 0 heights))
                  30)
        highest (+ (reduce max (cons (* 5 line-separation) heights))
                   10)
        last-note (last notes)
        width (+ (:absolute-x-offset last-note)
                 (:length last-note))]
    (assoc track
      :size {:lowest lowest
             :highest highest
             :height (- highest lowest)
             :width width
             })))

(defn add-track-width [track]
  (let [notes (:notes track)
        ln (last notes)]
    (assoc track :width (+ (:absolute-x-offset ln) (:length ln)))))

(defn add-note-indices [track]
  (let [notes (->> (:notes track)
                   (map-indexed (fn [index note] (assoc note :index index))))]
    (assoc track :notes notes)))

(defn calculate-clef [track]
  (let [notes (remove :rest (:notes track))
        octaves (remove nil? (map #(if-let [n (:n %)]
                                     (if-let [pitch (first n)]
                                       (let [pitch (if (list? pitch)
                                                     (first pitch)
                                                     pitch)]
                                         (read-string (str (last pitch))))))
                                  notes))
        average (/ (reduce + octaves) (count octaves))]
    (if (< average 4) \F \G)))

(defn add-clef [track]
  (assoc track :clef
    (or (:clef track) (calculate-clef track))))

(defn calculate-track [track]
  (-> track
      add-rests
      add-clef
      calculate-notes
      add-track-width
      add-track-heights
      add-note-indices))

;; =====
;; Helper functions
;; =====
(defn get-notes [track] (:notes track))
(defn get-height  [track] (get-in track [:size :height]))
(defn get-lowest  [track] (get-in track [:size :lowest]))
(defn get-highest [track] (get-in track [:size :highest]))
(defn get-width   [track] (get-in track [:size :width]))

;; =====
;; Testing
;; =====

(def test-notes
  '[{:dr 923.0769, :ndr 923.0769, :sl -4, :bar 1, :n ("E4" 1/4), :mm 130, :key "A", :meter (6 8)}
    {:dr 461.53845, :ndr 461.53845, :sl -4, :n ("E4" 1/8)}
    {:dr 923.0769, :ndr 923.0769, :sl -4, :n ("E4" 1/4)}
    {:dr 461.53845, :ndr 461.53845, :sl -4, :n ("E4" 1/8)}
    {:dr 923.0769, :ndr 923.0769, :sl -4, :bar 2, :n ("E4" 1/4)}
    {:dr 461.53845, :ndr 461.53845, :sl -4, :n ("E4" 1/8)}
    {:dr 923.0769, :ndr 923.0769, :sl -4, :n ("E4" 1/4)}
    {:dr 461.53845, :ndr 461.53845, :sl -4, :n ("E4" 1/8)}
    {:dr 923.0769, :ndr 923.0769, :sl -4, :bar 3, :n ("E4" 1/4)}
    {:dr 461.53845, :ndr 461.53845, :sl -4, :n ("E4" 1/8)}
    {:dr 923.0769, :ndr 923.0769, :sl -4, :n ("E4" 1/4)}
    {:dr 461.53845, :ndr 461.53845, :sl -4, :n ("E4" 1/8)}
    {:dr 923.0769, :ndr 923.0769, :sl -4, :bar 4, :n ("E4" 1/4)}
    {:dr 461.53845, :ndr 461.53845, :sl -4, :n ("B4" 1/8)}
    {:dr 923.0769, :ndr 923.0769, :sl -4, :n ("A4" 1/4)}
    {:dr 461.53845, :ndr 461.53845, :sl -4, :n ("Ab4" 1/8)}
    {:dr 923.0769, :ndr 923.0769, :sl -4, :bar 5, :n ("E4" 1/4)}
    {:dr 461.53845, :ndr 461.53845, :sl -4, :n ("E4" 1/8)}
    {:dr 923.0769, :ndr 923.0769, :sl -4, :n ("E4" 1/4)}
    {:dr 461.53845, :ndr 461.53845, :sl -4, :n ("E4" 1/8)}
    {:dr 923.0769, :ndr 923.0769, :sl -4, :bar 6, :n ("E4" 1/4)}
    {:dr 461.53845, :ndr 461.53845, :sl -4, :n ("E4" 1/8)}
    {:dr 923.0769, :ndr 923.0769, :sl -4, :n ("E4" 1/4)}
    {:dr 461.53845, :ndr 461.53845, :sl -4, :n ("E4" 1/8)}
    {:dr 923.0769, :ndr 923.0769, :sl -4, :bar 7, :n ("E4" 1/4)}
    {:dr 461.53845, :ndr 461.53845, :sl -4, :n ("E4" 1/8)}
    {:dr 923.0769, :ndr 923.0769, :sl -4, :n ("E4" 1/4)}
    {:dr 461.53845, :ndr 461.53845, :sl -4, :n (("B4" "F#4") 1/8)}
    {:dr 923.0769, :ndr 923.0769, :sl -4, :bar 8, :n (("A4" "E4") 1/4)}
    {:dr 461.53845, :ndr 461.53845, :sl -4, :n (("Ab4" "D4") 1/8)}
    {:dr 923.0769, :ndr 923.0769, :sl -4, :n (("E4" "C#4") 1/4)}
    ])

(def test-track {:clef \G :notes test-notes})

(use 'clojure.pprint)
(defn test-run []
  (let [notes (calculate-notes test-notes)
        track (calculate-track test-track)]
    ;(pprint notes)
    (pprint (update-in track [:notes] #(take 5 %)))
    ))
