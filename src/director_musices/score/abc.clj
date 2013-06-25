(ns director-musices.score.abc
  (:require [director-musices.util :as util]
            [director-musices.score.ui :as ui]
            [clojure.string :as cstr]
            [clojure.java.io :as jio]
            [instaparse.core :as insta]))

;;;;
;;;; Prepare string

(defn remove-comments [string]
  (cstr/replace string #"%.*?(\n|$)" "\n"))

(defn concat-backslash-lines [string]
  (cstr/replace string #"\\[ \t]*\n" ""))

(defn prepare-input [string]
  (-> string
      remove-comments
      concat-backslash-lines))

;;;;
;;;; String parser

(def parser-string
  "
  S = descriptors track
  descriptors = (descriptor | whitespace)+
  descriptor = #'[A-Z]:.*'
  
  track = (note | bar | phrase-mark | whitespace)+
  
  bar = ('|' | thick-bar | repeat-bar) #'[0-9]'?
  thick-bar = '||' | '[|' | '|]'
  repeat-bar = '::' | '|:' | ':|'
  
  notes = (note | whitespace)+
  note = accidental? note-height octave? note-length?
  octave = #'[\\',]+'
  accidental = '^' | '^^' | '_' | '__' | '='
  note-height = #'[A-Za-z]'
  note-length = #'/{0,2}[0-9]'
  
  phrase-mark = phrase-start | phrase-end
  phrase-start = '('
  phrase-end = ')'
  
  whitespace = #'[\\s!]+'
  ")

(def parser (insta/parser parser-string))

;;;;
;;;; Parse descriptors

(defn remove-whitespace [elements]
  (remove (fn [[type]] (= type :whitespace)) elements))

(defn parse-descriptors [descriptors]
  (let [descriptors (->> descriptors
                         rest
                         remove-whitespace
                         (map second))
        m (into {}
                (for [descriptor descriptors]
                  [(keyword (str (first descriptor)))
                   (subs descriptor 2)]))
        env {:title (:T m)
             :default-note-length (read-string (:L m))}]
    env))

;;;;
;;;; Parse track

(defn parse-note-length-mod [raw]
  (if raw
    (case raw
      "/" 1/2
      "//" 1/4
      (let [raw (if (.startsWith raw "/")
                  (str "1" raw)
                  raw)]
        (read-string raw)))
    1))

(defn parse-note-length [default note-length]
  (* default (parse-note-length-mod note-length)))

(defn parse-note-height [note-height]
  (cond
    (re-matches #"[A-G]" note-height)
    (str note-height 4)
    (re-matches #"[a-g]" note-height)
    (str (.toUpperCase note-height) 5)
    :else nil))

(defn add-bar [note bar]
  (if bar (concat note ['bar bar]) note))

(defn add-rest [note note-height]
  (if note-height note (concat note '[rest t])))

(defn parse-note [env bar note]
  (let [{:keys [note-height] :as m} (into {} (rest note))
        note-height (parse-note-height note-height)
        note-length (parse-note-length (:default-note-length env)
                                       (:note-length m))
        note (list 'n (list note-height note-length))]
    (-> (list 'n (list note-height note-length))
        (add-bar bar)
        (add-rest note-height))))

(defn parse-track [env track]
  (->> (rest track)
       (reduce (fn [{:keys [prev bars out] :as m} [type :as v]]
                 (case type
                   :note
                   (let [bar (if (= (first prev) :bar)
                               (inc bars) nil)
                         note (parse-note env bar v)]
                     {:prev v
                      :bars (if bar bar bars)
                      :out (conj out note)})
                   :bar (assoc m :prev v)
                   m))
               {:prev nil
                :bars 0
                :out []})
       :out))

(defn parse-abc [string]
  (let [string (prepare-input string)
        parsed (rest (parser string))
        env (parse-descriptors (first parsed))
        track (parse-track env (second parsed))]
    {:env env
     :track track}))

;;;;
;;;; Create string output

(defn track->dm [notes]
  (reduce (fn [prev note]
            (str prev (pr-str note) "\n"))
          ""
          notes))

(def track-default
"mono-track
 :trackname \"V1\"
 :midi-channel 1
 :midi-initial-volume 0
 :track-delay 0
 :midi-initial-program 1
 :synth \"SBlive\"
")

(defn abc->dm [string]
  (let [parsed (parse-abc string)]
    (str track-default (track->dm (:track parsed)))))

;;;;
;;;; Menu

(defn open-abc-from-file [f]
  (let [buffer (jio/file (util/tmp-dir) "abc-buffer.mus")]
    (spit buffer (abc->dm (slurp f)))
    (ui/load-score-from-file buffer)))

(defn choose-and-open-abc [& _]
  (if-let [f (util/choose-file
               :title "Import score from abc file"
               :type :open
               :filters [["abc files (.abc)" ["abc"]]])]
    (open-abc-from-file f)))

;;;;
;;;; Testing

(def test-input
  "X:1
  T:The Legacy Jig
  M:6/8
  L:1/8
  R:jig
  K:G
  GFG BAB | gfg gab | GFG BAB | d2A AFD |
  GFG BAB | gfg gab | age edB |1 dBA AFD :|2 dBA ABd |:
  efe edB | dBA ABd | efe edB | gdB ABd |
  efe edB | d2d def | gfe edB |1 dBA ABd :|2 dBA AFD |]")

(defn run-test []
  ;(parser test-input)
  ;(parse-abc test-input)
  ;(parse-element (parser test-input))
  (abc->dm test-input)
  )
