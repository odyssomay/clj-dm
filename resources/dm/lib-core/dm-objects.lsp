;;;-*-Mode: LISP; Package: DM -*-
;;
;; **************************************
;;   definitions for non-music objects
;; **************************************
;; 2/10/2000/af added "midifile-input-articulation-threshold" to the score object
;; 04/10/2002 Stephane Letz (Grame) Add a midishare-seq field in the midishare-settings class
;; 2006-02-15/af added voice class

(in-package :dm)


;; =========================
;;   SYSTEM-SETTINGS CLASS
;; =========================
;
; - flags for debugging level  Killed: load-verbose, verbose-eval-selection
;
(defclass system-settings ()
    ((log-to-file-enabled-default :initarg :log-to-file-enabled-default :accessor log-to-file-enabled-default
       :initform nil)
     (log-to-score-enabled-default :initarg :log-to-score-enabled-default :accessor log-to-score-enabled-default
       :initform nil)
     (log-to-file-enabled :initarg :log-to-file-enabled :accessor log-to-file-enabled
       :initform nil)
     (log-to-score-enabled :initarg :log-to-score-enabled :accessor log-to-score-enabled
       :initform nil)
     (log-requested :initarg :log-requested :accessor log-requested :initform nil)
     (log-filename-default :initarg :log-filename-default :accessor log-filename-default :initform 
      nil)
     (log-filehandle :initarg :log-filehandle :accessor log-filehandle :initform nil)
     (show-applying-rule-window :initarg :show-applying-rule-window :accessor show-applying-rule-window :initform nil)   ;put t
     ;(demo-version :initarg :demo-version :accessor demo-version
     ;  :initform nil)     ; for making dm-demo 
     (rule-debug-info :initarg :rule-debug-info :accessor rule-debug-info
       :initform nil)     ; set t for debugging
     ;(verbose :initarg :verbose :accessor verbose
     ;  :initform t)     ; set t for debugging
     (verbose-i/o :initarg :verbose-i/o :accessor verbose-i/o
       :initform t)     ; set t checking i/o messages
     ))



;; =================
;;   LOGFILE CLASS
;; =================
;
(defclass logfile ()
    ((fpath :initarg :fpath :accessor fpath :initform nil)
     (filehandle :initarg :filehandle :accessor filehandle :initform nil)
     (status :initarg :status :accessor status :initform nil)
     ))


;; =======================
;;   DISPLAYING-SETTINGS
;; =======================
;     
(defclass displaying-settings ()     
     ((print-right-margin :initarg :print-right-margin :accessor print-right-margin
       :initform nil)
     (toclip? :initarg :toclip? :accessor toclip? :initform nil)
     (drawnote-start-bar :initarg :drawnote-start-bar :accessor drawnote-start-bar :initform 1)
     (drawnote-w :initarg :drawnote-w :accessor drawnote-w :initform nil)
     (print-drm-p :initarg :print-drm-p :accessor print-drm-p :initform nil)
     (to-printer? :initarg :to-printer? :accessor to-printer? :initform nil)
      #+:mswindows
      (music-font-face :initarg :music-font-face :accessor music-font-face :initform "Anastasia")
     ))


;; =========================
;;   RULE-SETTINGS CLASS
;; =========================
;
(defclass rule-settings ()
    ((all-rules-default :initarg :all-rules-default :accessor all-rules-default
       :initform '(
                   (high-loud 1.0)
                   (melodic-charge 1.0 :amp 1 :dur 1 :vibamp 1)
                   (harmonic-charge 1.0 :amp 1 :dur 1 :vibfreq 1)
                   (duration-contrast 1.0 :amp 1 :dur 1)
                   (duration-contrast-art 1.0)
                   (double-duration 1.0)
                   (punctuation 1.1 :dur 1 :duroff 1 :markphlevel7 nil)
                   (phrase-arch 1.5 :phlevel 5 :turn 0.3 :next 1.3 :amp 2)
                   (phrase-arch 1.5 :phlevel 6 :turn 0.3 :amp 2 :last 0.2)
                   (normalize-sl t)
                   (normalize-dr t)
                   (final-ritard 1.0)
                   ;(high-sharp 2.0)
                   ;(melodic-intonation 0)
                   ;(mixed-intonation 0)
                   ;(inegalles 0)
                   ;(final-ritard 1)
                   ))
     (all-rules :initarg :all-rules :accessor all-rules
       :initform '(
                   (high-loud 1.0)
                   (melodic-charge 1.0)
                   (harmonic-charge 1.0)
                   (duration-contrast 1.0)
                   (double-duration 1.0)
                   (punctuation 1.0)
                   (phrase-arch 0.7 :phlevel 5 :turn 0.3 :next 1.3 :amp 4)
                   (phrase-arch 0.5 :phlevel 6 :turn 2 :amp 4 :last 0.2)
                   (high-sharp 2.0)
                   (melodic-intonation 0)
                   (mixed-intonation 0)
                   (inegalles 0)
                   (normalize-sl t)
                   (normalize-dr t)
                   ))
     ;; ***** it should be init to the same value of the previous slot    ;from Rule Dialog
     (init-music-include-dynamics :initarg :init-music-include-dynamics :accessor init-music-include-dynamics
      :initform t)
     (print-prop-list :initarg :print-prop-list :accessor print-prop-list :initform
       '(rules dc env n dot phrase subph q rest ack bar master dr dro punct synt pr channel))
     ; this is an old list '(min mi mc n bar)
     (print-not-prop-list :initarg :print-not-prop-list :accessor print-not-prop-list
       :initform '(a0 f0 ndr dro dr))
     (prop-list :initarg :prop-list :accessor prop-list
       :initform nil)
     (sync-rule-list-default :initarg :sync-rule-list-default :accessor sync-rule-list-default
       :initform '((no-sync nil)(melodic-sync t)(simple-mel-sync)) );(bar-sync nil)
     (sync-rule-list :initarg :sync-rule-list :accessor sync-rule-list
       :initform '((no-sync nil)(melodic-sync t)(simple-mel-sync)) )  ;(bar-sync nil)
     ;; ***** it should be init to the same value of the previous slot  
     (vlist-all :initarg :vlist-all :accessor vlist-all
       :initform nil)
     ))

;; =========================
;;   VOCAL-SETTINGS CLASS
;; =========================
;
(defclass vocal-settings ()
  ((phoneme-features 
    :initarg :phoneme-features :accessor phoneme-features
    :initform '(
                ("A:" voc tense low back cont voice seg)
                ("A"  voc low back cont voice seg)
                ("E:"  voc tense mjuk low cont voice seg)
                ))
   (phoneme-init-parameters 
    :initarg :phoneme-init-parameters :accessor phoneme-init-parameters
    :initform '(
                ("A:" F1 550 F2 1000 F3 2550 F4 2900 F5 3300)
                ("A" F1 600 F2 1000 F3 2450 F4 2900 F5 3100)
                ("E:" F1 400 F2 1620 F3 2400 F4 3000 F5 3380)
                ("E" F1 550 F2 1500 F3 2400 F4 3000 F5 3300) 
                ("I:" F1 300 F2 1800 F3 2500 F4 3000 F5 3300)
                ("I" F1 350 F2 1750 F3 2500 F4 3000 F5 3300)
                ("O:" F1 350 F2 680 F3 2350 F4 2750 F5 3000)
                ("O" F1 350 F2 680 F3 2350 F4 2750 F5 3000)
                ("U:" F1 350 F2 1400 F3 2200 F4 2900 F5 3150)
                ("U" F1 440 F2 1120 F3 2030 F4 2650 F5 3000)
                ("Y:" F1 330 F2 330 F3 2170 F4 2670 F5 3400)
                ("Y" F1 330 F2 1650 F3 2170 F4 2670 F5 3400)
                ("Å:" F1 400 F2 700 F3 2150 F4 2800 F5 3500)
                ("Å" F1 450 F2 850 F3 2500 F4 2950 F5 3250)
                ("Ä3" F1 550 F2 1220 F3 2050 F4 2800 F5 3500)
                ("Ö4" F1 550 F2 1150 F3 2450 F4 2800 F5 3100)
                ("Å:" F1 400 F2 700 F3 2150 F4 2800 F5 3500)
                ("Ä:" F1 550 F2 1400 F3 2200 F4 2750 F5 3400)
                ("Ä" F1 450 F2 1300 F3 2250 F4 2700 F5 3000)
                ("Ö:" F1 450 F2 1450 F3 2300 F4 2900 F5 3100)
                ("Ö" F1 450 F2 1250 F3 2400 F4 2900 F5 3100)
                ))
   ))


;; ============================
;;   MIDISHARE-SETTINGS CLASS
;; ============================
;
(defclass midishare-settings ()
     ((midishare-refnum :initarg :midishare-refnum :accessor midishare-refnum :initform nil)
      ;A MidiShare sequence to be filled and played by the MidiShare Player
      (midishare-seq  :initarg :midishare-seq :accessor midishare-seq  :initform 0)
      ;midiport can be 0 or 1, external midi or internal synth or the opposite!!!!!!!!!!
      (midishare-port :initarg :midishare-port :accessor midishare-port :initform 2)
      ))

;; ============================
;;   SCORE-SETTINGS CLASS /AF
;; ============================
;
(defclass score-settings ()
     ((default-tempo :initarg :default-tempo :accessor default-tempo :initform 120)
      (notation-var-list :initarg :notation-var-list :accessor notation-var-list
                         :initform  '(acc-end acc-start bar dot dyn key legato-end legato-start meter mm modus 
                                                   n pedal ph phrase-end phrase-start pr q rest rit-end rit-start 
                                                   slur staccato staccato-end staccato-start text tie tuple) )
      (performance-var-list :initarg :performance-var-list :accessor performance-var-list
                            :initform '(dc dr dro f0 ndr pedal-perf sl va vf) )
      (midifile-input-articulation-threshold :initarg :midifile-input-articulation-threshold
                                             :accessor midifile-input-articulation-threshold
        :initform 0.4)
      (midifile-input-notevalues 
       :initarg :midifile-input-notevalues :accessor midifile-input-notevalues
       :initform (sort (list   ; all notevalues and triplets between 1 and 1/16
                   1/64 1/32 1/16 1/8 1/4 1/2 1
                   (* 1/3 1) (* 2/3 1)     ;half note triplets
                   (* 1/3 1/2)      ;quarter note triplets
                   (* 1/3 1/4)      ;eighth note triplets
                   (* 1/3 1/8)      ;sixteens note triplets
                   (* 1/3 1/16)     ;thirtysecond note triplets
                   (* 3/2 1/2)(* 3/2 1/4)(* 3/2 1/8)(* 3/2 1/16)(* 3/2 1/32) ;dotted notes
                        )  #'>) )
;;;        (sort (list   ;old division 
;;;                         1/64 1/32 1/16 3/32 1/8 5/32 3/16 7/32 ;64nd division
;;;                         1/24 1/12 1/6 5/24            ;triplets on a eighth note
;;;                         )  #'>)
      (midifile-input-quantize-dr 
       :initarg :midifile-input-quantize-dr :accessor midifile-input-quantize-dr
       :initform nil)
      (midifile-input-quantize-notevalues 
       :initarg :midifile-input-quantize-notevalues :accessor midifile-input-quantize-notevalues
       :initform 1/16)
      (midifile-input-quantize-triplet-notevalues 
       :initarg :midifile-input-quantize-triplet-notevalues :accessor midifile-input-quantize-triplet-notevalues
       :initform 1)
      (midifile-input-quantize-split-at-barlines 
       :initarg :midifile-input-quantize-split-at-barlines :accessor midifile-input-quantize-split-at-barlines
       :initform t)
      (midifile-note-subdiv-levels 
       :initarg :midifile-note-subdiv-levels :accessor midifile-note-subdiv-levels
       :initform 2)
      ))

;; ==========================
;;   PLAYING-SETTINGS CLASS
;; ==========================
;
(defclass playing-settings ()
    (
     (output-midi-tick-res :initarg :output-midi-tick-res :accessor output-midi-tick-res
                           :initform 240)   ;ticks per quarter note for midifile output
     (anticipation-time :initarg :anticipation-time :accessor anticipation-time
       :initform 3000)   ;in msec
     (channel :initarg :channel :accessor channel :initform 1)     ;current midi channel
     (channel1? :initarg :channel1? :accessor channel1?
       :initform nil)    ;send all voices on channel one
     (delay :initarg :delay :accessor delay
       :initform 1000)
     (DMtempo :initarg :DMtempo :accessor DMtempo :initform 1.0) ;the value to divide durations in msec to ticks ???
     ;; ***** there is a conflict between names to be solved  
     (last-list :initarg :last-list :accessor last-list :initform nil)
     (last-time :initarg :last-time :accessor last-time
       :initform 0)       ;used by play and midi-open
     (play-active :initarg :play-active :accessor play-active
       :initform nil)   ;still in play ? 
     (play1-active :initarg :play1-active :accessor play1-active
       :initform nil)   ;still in play1 ?
     (play-stop-p :initarg :play-stop-p :accessor play-stop-p :initform nil)
     (repeat :initarg :repeat :accessor repeat
       :initform nil)    ;true if repeat
     (send-before-note-on? :initarg :send-before-note-on? :accessor send-before-note-on?
       :initform t)     ;send control and other midi param. ?
     (midifile-out-save-chord-phrase? :initarg :midifile-out-save-chord-phrase?
                                      :accessor midifile-out-save-chord-phrase?
                                      :initform t)     ;store chords and phrases as lyrics
     (send-note-off? :initarg :send-note-off? :accessor send-note-off?
       :initform t)     ;send noton 0 after each tone
     (start-bar :initarg :start-bar :accessor start-bar
       :initform 1)
     (start-time :initarg :start-time :accessor start-time :initform nil)
     (play-synth-name-default :initarg :play-synth-name-default
         :accessor play-synth-name-default :initform "SBlive")   ;current syntname
     (to-midi-file? :initarg :to-midi-file? :accessor to-midi-file? :initform nil)
     (transpose-shift :initarg :transpose-shift :accessor transpose-shift :initform 0)
     (vel-mean :initarg :vel-mean :accessor vel-mean :initform 64)  ;the mean value of velocity
     (vel-scale :initarg :vel-scale :accessor vel-scale :initform 1)  ; scaling factor for velocity
     (volume-control-number :initarg :volume-control-number :accessor volume-control-number
                            :initform 7)
     (play-time-shape-sampling-rate :initarg :play-time-shape-sampling-rate :accessor play-time-shape-sampling-rate
       :initform 10) ;sampling interval in ms for time shapes
     )
   )



;; ===================
;;   PATHNAMES CLASS
;; ===================
;

(defclass pathnames ()
     ((lib-directory :initarg :lib-directory :accessor lib-directory
       :initform (translate-logical-pathname "dm:lib;"))       
     (rules-directory :initarg :rules-directory :accessor rules-directory
       :initform (translate-logical-pathname "dm:rules;")) 
     (rule-sets-directory :initarg :rule-sets-directory :accessor rule-sets-directory
       :initform (translate-logical-pathname "dm:rulepalettes;")) 
     (music-directory :initarg :music-directory :accessor music-directory
       :initform (translate-logical-pathname "dm:scores;"))
     (log-directory :initarg :log-directory :accessor log-directory
       :initform (translate-logical-pathname "dm:logs;"))
     (current-music-directory :initarg :current-music-directory :accessor current-music-directory
         :initform (translate-logical-pathname "dm:scores;"))
       (current-rule-sets-directory :initarg :current-rule-sets-directory :accessor current-rule-sets-directory
        :initform (translate-logical-pathname "dm:rulepalettes;"))
      ))


;; ===============================
;;   ENVIRONMENT-SETTINGS OBJECT
;; ===============================
;;
;; it encorporates and replaces the corresponding previous global variables.
;; Only one global variables is left: *env-settings*
;; contaning the current environment. But it is not called explicitely anymore: it could be also 
;; avoided with same careful programming. To get a value from this object, we use get-dm-var now. 
;; Note that many environment can be defined in the system at the same time: get-dm-var work on
;; the current one
;; 
;;      Killed: *prefs-fpath* (pathname "dm:DM-preferences")
;;              *default-pathname-defaults* (pathname "dm:")
;; - default playing settings
;; 
;;;
;      
(defclass environment-settings 
      (displaying-settings
       rule-settings
       vocal-settings
       midishare-settings
       score-settings 
       playing-settings
       pathnames
       system-settings )()
   )
                  


;; ----------------------
;;   GET and SET-DM-VAR
;; ----------------------
;;
;
(defun get-dm-var (var-name)
   (slot-value *env-settings* var-name))


(defun set-dm-var (var-name value)
   (setf (slot-value *env-settings* var-name) value))

  

(defconstant *tones* '("C" "C#" "Db" "D" "D#" "Eb" "E" "F" "F#"
                "Gb" "G" "G#" "Ab" "A" "A#" "Bb" "B"))
(defconstant *12tones* '("C" "C#" "D" "Eb" "E" "F" "F#"
                "G" "Ab" "A" "Bb" "B"))
(defconstant *modus* '("maj" "min"))








