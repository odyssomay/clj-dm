(ns director-musices.gui.note
  (:use clojure.tools.logging))

(defn add-midi-height [{[height _] :n :as note}]  
  (if height
    (assoc note
           :f0 (-> (case (first height)
                     \C 60, \D 62, \E 64, \F 65,
                     \G 67, \A 69, \B 71)
                 (+ (case (second height)
                      \# 1, \b -1, 0))
                 (+ (* 12 (- (java.lang.Character/getNumericValue (last height))
                             4)))))
    note))

(defn add-dr [{[_ length] :n :as note} bpm]
  (let [dr (* length (* 1000 (/ bpm 60)))]
    (assoc note :dr dr
                :ndr dr)))

(defn init-note-pvariables [note]
  (-> note
      add-midi-height
      (add-dr 100)
      (assoc :sl 0) ))

(defn note->string [note]
  (str "(" (apply str
  (map (fn [[k v]]
         (str (name k) " " v " ")) note))
       ")\n"))

;; abc4j

(defn tone-to-note [tone]
    (load-string (str "abc.notation.Note/" (str tone))))

(defn get-note-duration [{[height length] :n :keys [ndr? ndr dr]}]
  (* length abc.notation.Note/WHOLE))

(defn get-strict-note-duration [{[height length] :n :keys [ndr? ndr dr]}]
  (abc.notation.Note/convertToNoteLengthStrict 1 (denominator length)))

(defn n-to-abc4j [{[height length] :n :keys [ndr? ndr dr] :as note}]
  (let [tone (if height
               (tone-to-note (first height))
               abc.notation.Note/REST)
        jnote (proxy [abc.notation.Note] [tone]
                (getDuration [] (get-note-duration note))
                (getStrictDuration [] (get-strict-note-duration note)))]
    (when height
      (let [octave (java.lang.Character/getNumericValue (last height))
            accidental (case (second height)
                         \# abc.notation.AccidentalType/SHARP
                         \b abc.notation.AccidentalType/FLAT
                         abc.notation.AccidentalType/NONE)]
        (.setOctaveTransposition jnote (- octave 4))
        (.setAccidental jnote accidental)
        (if (= (numerator length) 3) (.setDotted jnote 1))))
      jnote))

(defn note-to-abc4j [{:keys [bar n staccato tie meter] :as note}]
  (assoc note :abc4j
         {:bar (when bar (abc.notation.BarLine.))
          :note (when n
                  (let [abc4j_note (n-to-abc4j note)]
                    (when staccato (.setStaccato abc4j_note true))
                    abc4j_note))
          :separator (when-not tie (abc.notation.NotesSeparator.))
          :signature (when meter (abc.notation.TimeSignature. (first meter) (second meter)))}))

(defn add-note-to-score [note score]
  (let [{:keys [bar note separator signature]} (:abc4j note)]
    (if signature (.addElement score signature))
    (if bar (.addElement score bar))
    (if note (.addElement score note))
    (if separator (.addElement score separator))))

