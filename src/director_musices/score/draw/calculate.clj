(ns director-musices.score.draw.calculate)

(def line-separation 7)

(defn note-height [track note]
  (let [p (:pitch note)
        ;; HACK - fix for chords
        p (if (list? p) (first p) p)
        h (- 9
             (case (first p)
               \C 0, \D 1, \E 2, \F 3
               \G 4, \A 5, \B 6)
             (* 7 (- (read-string (str (last p))) 4)))]
    (case (:clef track)
      \G h
      \F (- h 12))))

(defn note-y-offset [note]
  (* (:height note) (/ line-separation 2)))

(defn note-hollow? [note]
  (not (contains? #{1 1/2 1/4 1/8 1/16 1/32}
                  (:nlength note))))

(defn note-data [track note]
  (let [note (assoc note :height (note-height track note))]
    (assoc note
      :y-offset (note-y-offset note)
      :hollow? (note-hollow? note))))

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
    (-> (for [{:keys [dr ndr n] :as note} (:notes track)]
          (->> (merge note
                      (when n
                        {:pitch (first n)
                         :length dr
                         :nlength (second n)}))
               (note-data track)))
        add-absolute-lengths)))

(defn add-track-heights [track]
  (let [{:keys [notes]} track
        heights (remove nil? (map #(if (:pitch %)
                                       (:y-offset %))
                                  notes))
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

(defn calculate-clef [track] \G)

(defn add-clef [track]
  (assoc track :clef
    (or (:clef track) (calculate-clef track))))

(defn calculate-track [track]
  (-> track
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
