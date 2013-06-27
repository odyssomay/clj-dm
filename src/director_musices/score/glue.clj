(ns director-musices.score.glue
  (:require [director-musices.util :as util]
            (director-musices.common-lisp
              [interpreter :as inr]
              [glue :as glue])))

; The CL function doesn't work
; (defn load-active-score-from-string [string]
;   (glue/eval-dm
;     (str "(read-active-score-from-string \"" string "\")
;           (init-music-score)")))


(defn load-active-score-from-file [path]
  (glue/eval-dm
    (str "(read-active-score-from-file \"" (inr/abcl-path path) "\")"
         "(init-music-score)")))

(defn load-active-performance-from-file [path]
  (glue/eval-dm
    (str "(read-active-score-from-file \"" (inr/abcl-path path) "\")")))

(defn save-score-to-path [path]
  (glue/eval-dm (str "(save-score-fpath \"" (inr/abcl-path path) "\")")))

(defn save-pdm-to-path [path]
  (glue/eval-dm (str "(pdm-save-rule-data-fpath \"" (inr/abcl-path path) "\")")))

(defn load-active-score-from-midi-file [path]
  (glue/eval-dm (str "(load-midifile-fpath \"" (inr/abcl-path path) "\")"
                     "(init-music-score)")))

(defn save-midi-to-path [path]
  (glue/eval-dm (str "(save-performance-midifile1-fpath \"" (inr/abcl-path path) "\")")))

(defn get-active-score []
  (glue/eval-dm "(get-active-score)"))


;; =====
;; Easy access
;; =====
(defn track-list [] "(track-list *active-score*)")

(defn segment-list [track-id]
  (str "(segment-list (nth " track-id " " (track-list) "))"))

(defn segment [track-id id]
  (str "(nth " id " " (segment-list track-id) ")"))


(defn value->clj [v]
  (condp = v
    'T true
    'NIL nil
    v))

(defn clj->value [v]
  (if (sequential? v)
    (map clj->value v)
    (condp = v
      true 'T
      false 'NIL
      nil 'NIL
      v)))

(defn segment->map [segment]
  (let [raw (.printObject segment)]
    (->> raw
         read-string
         (map (fn [[k vs]]
                [(keyword (.toLowerCase (str k)))
                 (value->clj vs)]))
         (into {}))))

(defn map->segment [m]
  (let [raw (prn-str (map (fn [[k v]]
                            (concat (list (symbol (.toUpperCase (name k))))
                                    (if (coll? v)
                                      (map clj->value v)
                                      (list (clj->value v))))) m))]
    (.replaceAll raw "," "")))

(defn get-track [track-index]
  (let [raw (glue/eval-dm
              (str "(map 'list (lambda (segment)
                                 (map 'list
                                      (lambda (x)
                                        (list (car x)
                                              (cdr x)))
                                      (var-list segment)))"
                   (segment-list track-index)
                   ")"))
        notes (->> raw
                   .copyToArray
                   (map segment->map))]
    {:notes notes}))

(defn set-segment-parameter [track-index segment-index k v]
  (glue/eval-dm
    (str "(set-var " (segment track-index segment-index)
         " '" (name k) " '" (pr-str v) ")")))

(defn remove-segment-parameter [track-index segment-index k]
  (glue/eval-dm
    (str "(rem-var " (segment track-index segment-index)
         " '" (name k) ")")))

(defn get-segment [track-index segment-index]
  (nth (:notes (get-track track-index)) segment-index nil))

(defn set-segment [track-index segment-index segment]
  (doseq [[k v] segment]
      (set-segment-parameter track-index segment-index k v)))

(defn get-track-count []
  (.javaInstance (glue/eval-dm "(length (track-list *active-score*))")))

(defn get-defined-synths []
  (map str (.copyToArray (glue/eval-dm "*defined-synth-names*"))))

(defn- property-acc [id property]
  (str "(" property " (nth " id " (track-list *active-score*)))"))

(defn get-track-synth-program-list [id]
  (map str (-> (str "(program-list " (property-acc id "synth") ")")
               glue/eval-dm
               .copyToArray)))

(defn get-current-synth [id]
  (or
    (util/find-i
      (.javaInstance
        (glue/eval-dm
          (str "(synth-symbol-to-name '"
               (.getName
                 (.typeOf (glue/eval-dm (property-acc id "synth"))))
               ")")))
      (get-defined-synths))
    (first (get-defined-synths))))

(defn get-track-property [id property]
  (let [value (case property
                "synth" (get-current-synth id)
                "active-p" (-> (property-acc id property)
                               glue/eval-dm
                               .printObject
                               read-string
                               value->clj
                               boolean)
                (-> (property-acc id property)
                    glue/eval-dm
                    .javaInstance))]
    (if (instance? org.armedbear.lisp.Nil value)
      nil value)))

(defn set-track-property [id property value]
  (let [type (case property
               "active-p" :bool
               "trackname" :string
               "instrument-type" :string
               "midi-channel" :int
               "midi-initial-program" :mip
               "synth" :synth
               :native)
        value (case type
                :bool (clj->value value)
                :string (str "\"" value "\"")
                :synth (str "(make-synth \"" value "\")")
                :mip (read-string (re-find #"[0-9]+" value))
                :int (str (int value))
                :native (str value))]
    (glue/eval-dm (str "(setf " (property-acc id property) " " value ")"))))
