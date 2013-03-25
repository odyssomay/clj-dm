;;;-*-Mode: LISP; Package: DM -*-
;;
;; *******************************
;;   Rule dialog and rule setups
;; *******************************
;;
;; 9101 /Anders Friberg
;; 9706 /Vittorio Colombo
;; 1203/AF implemented rule interaction

(in-package :dm)



;  rule-apply-list
;  rule-apply-list-barsync
;  rule-apply-list-melsync
;  rule-apply-group-1-bar
;  rule-apply-group-1
;  rule-apply-group-1-mel
;  rule-apply-group-1-mel*2
;  rule-apply-atonal-piano-mel
;  apply-all
;  apply-all-melsync))



;(defun foo () (rule-apply-list-melsync (get-dm-var 'all-rules1*2)))



;; -------------------
;;   RULE-APPLY-LIST
;; -------------------

#|
#+:mswindows
(defun rule-apply-list (rule-list)
  (let ((start 0)(result "")(my-window (make-applying)))
    (setf start (get-internal-real-time))
    
    (mapc #'(lambda (rule)
              (apply (car rule) (cdr rule))
              (setf result (format nil "~A~A - Elapsed time ~,3F   s~%"
                             result (car rule) (/ (- (get-internal-real-time) start)
                                                  internal-time-units-per-second)))
              (set-dialog-field my-window :result-text result)
              (setf start (get-internal-real-time))
              ) rule-list )
    (close my-window)
    ))
|#

#+(and :mswindows :allegro)
(defun rule-apply-list (rule-list)
  (if (get-dm-var 'verbose-i/o)
      (let ((start 0)(result "")(my-window (make-applying)))
        (setf start (get-internal-real-time))
        
        (mapc #'(lambda (rule)
                  (apply (car rule) (cdr rule))
                  (setf result (format nil "~A~A - Elapsed time ~,3F   s~%"
                                 result (car rule) (/ (- (get-internal-real-time) start)
                                                      internal-time-units-per-second)))
                  (set-dialog-field my-window :result-text result)
                  (setf start (get-internal-real-time))
                  ) rule-list )
        (close my-window) )
    (dolist (rule rule-list)
      (apply (car rule) (cdr rule)) )))

(defun rule-apply-list (rule-list)
  (dolist (rule rule-list)
    (if (get-dm-var 'verbose-i/o) (print rule))
    (apply (car rule) (cdr rule))
    ))

;; ------------------------
;;   RULE-APPLY-LIST-SYNC
;; ------------------------
;;
;; it's called by Apply-Rules in RuleDialog

#|
(defun rule-apply-list-sync (rule-list sync-type)
  (rule-apply-list rule-list)
  (cond
   ((eq sync-type 'melodic-sync)
    (if (> (length (active-track-list *active-score*)) 1)
        (then
         (if (get-dm-var 'verbose-i/o) (print "  Sync on mel ** V16 **"))
         (melodic-sync-rule-list rule-list) ))
    )
   ((eq sync-type 'bar-sync)
    (if (> (active-track-list *active-score*) 1)
        (then
         (if (get-dm-var 'verbose-i/o) (print "  Sync on bar"))
         (bar-sync) ))
    )
   ((eq sync-type 'no-sync)
    (if (get-dm-var 'verbose-i/o) (print " no Sync")))
   (t (advice "wrong sync option !"))
   )
  (rem-dro)
  (dr-limits)
  (dro-limits)
  )
|#
;;removed logfile arg to rule-apply-list ??????????????
(defun rule-apply-list-sync (rule-list sync-type)
  (cond
   ((eq sync-type 'melodic-sync)
    (if (> (length (active-track-list *active-score*)) 1)
        (then
         (if (get-dm-var 'verbose-i/o) (print "  Sync on melody"))
         (apply-rule-list-melodic-sync rule-list) )
      (rule-apply-list rule-list)
      )
    )
   ((eq sync-type 'simple-mel-sync)
    (if (> (length (active-track-list *active-score*)) 1)
        (then
         (if (get-dm-var 'verbose-i/o) (print "  Simple sync on melody"))
         (apply-rule-list-simple-mel-sync rule-list) )
      (rule-apply-list rule-list)
      )
    )((eq sync-type 'bar-sync)
      (rule-apply-list rule-list)
      (if (> (length (active-track-list *active-score*)) 1)
          (then
           (if (get-dm-var 'verbose-i/o) (print "  Sync on bar"))
           (bar-sync) ))
      )
   ((eq sync-type 'no-sync)
    (rule-apply-list rule-list)
    (if (get-dm-var 'verbose-i/o) (print " no Sync")) )
   (t (advice "wrong sync option !"))
   )
  (rem-dro)
  (dr-limits)
  (dro-limits)
  )

;; ------------------------------
;;  2 - RULE-APPLY-LIST-BARSYNC
;; ------------------------------
;;

(defun rule-apply-list-barsync (rule-list)
  (rule-apply-list rule-list)
  )


(defun rule-apply-group-1-bar ()
  ;(clock-cursor)
  (rule-apply-list-barsync (get-dm-var 'all-rules1))
  (redraw-display-windows) ;def in drawProp
  )

(defun rule-apply-group-1 ()
  (rule-apply-list (get-dm-var 'all-rules1))
  (redraw-display-windows) ;def in drawProp
  )

(defun rule-apply-group-1-mel ()
  ;(clock-cursor)
  (rule-apply-list-melsync (get-dm-var 'all-rules1))
  (redraw-display-windows) ;def in drawProp
  )

(defun rule-apply-group-1-mel*2 ()
  ;(clock-cursor)
  (rule-apply-list-melsync (get-dm-var 'all-rules1*2))
  (redraw-display-windows) ;def in drawProp
  )

(defun rule-apply-atonal-piano-mel ()
  ;(clock-cursor)
  (rule-apply-list-melsync  (get-dm-var 'atonal-piano-rules))
  (redraw-display-windows) ;def in drawProp
  )


;; ------------------------------------------------------------------------------
;;  new way of applying rules and handle rule interaction
;;  inspired by pdm format
;;  1203/AF
;; ------------------------------------------------------------------------------

;main function using file path input
;handles mono/poly automatic
;c interaction parameter, c=1 no interaction, c=2 quadratic, etc
(defun rule-interaction-apply-rules-sync (rulelist c)
  (let ((polyp (if (> (length (track-list *active-score*)) 1) t nil))) ;true if polyphonic
    (if polyp (make-simple-syncmel))
    (rule-interaction-apply-rules rulelist polyp)
    (rule-interaction-scale-and-reapply c)
    (if polyp (sync-note1))
    (if polyp (remove-last-track)) ; get rid of the sync track
    (rem-all :artvec)
    (rem-all :slvec)
    (rem-all :tvec)
    ))


(defun rule-interaction-apply-rules (rulelist polyp)
  (let ((vec)
        (vec-size (length rulelist))
        ;(monop (if (= 1 (length (track-list *active-score*))) t nil)) ;true if monophonic
        (monop (not polyp))
        )
    ;(make-simple-syncmel)
    ;-------initialize deviation vectors for each note-------------
    (each-track
     (let ((syncp (string-equal (trackname *this-track*) "sync-track"))) 
       (if (or monop syncp)
           (each-note
            (set-this :tvec (make-array vec-size :element-type 'single-float :initial-element 0.0))
            ))
       (if (or monop (not syncp))
           (each-note-if
            (not (this 'rest))
            (then
             (set-this :slvec (make-array vec-size :element-type 'single-float :initial-element 0.0))
             (set-this :artvec (make-array vec-size :element-type 'fixnum :initial-element 0))
             )))
       ))
    ;(print-ll "monop " monop " syncp " syncp)
    ;----apply each rule from the list and collect deviations-------
    (reset-music)
    ;(print "reset ok")
    (dotimes  (i (length rulelist))
      (print (nth i rulelist))
      (eval (nth i rulelist)) ;apply the rule
      (normalize-sl)
      (normalize-dr)
      ;(print "rule ok")
      (each-track
       (let ((syncp (string-equal (trackname *this-track*) "sync-track")))
         (if (or monop syncp)
             (each-note
              ;(print (nth *i* *v*))
              (setq vec (this :tvec))
              (setf (aref vec i) 
                (if (> (this 'dr) 1.0)
                    (round2three (- (/ (this 'dr) (this 'ndr)) 1.0)) ;percent increase in tempo
                  0.0 ;fix for very short dr - shouldn't affect anything important
                  ))       
              (set-this :tvec vec)
              ))
         (if (or monop (not syncp))
             (each-note-if
              (this :slvec)
              (then
               (setq vec (this :slvec))
               (setf (aref vec i) (round2three (float (- (this-or-zero 'sl) (this-or-zero 'nsl)))))
               (set-this :slvec vec)
               )))
         (if (or monop (not syncp))
             (each-note-if
              (this :artvec)
              (then
               (setq vec (this :artvec))
               (setf (aref vec i) (round (this-or-zero 'dro)))
               (set-this :artvec vec)
               )))
         )
       ;(print "vector ok")
       (reset-music)
       ))))

;collect the deviation vectors, compute the interaction scaling and apply to performance vars
(defun rule-interaction-scale-and-reapply (c)
  (each-note
   (then
    (if (this :slvec)
          (add-this 'sl (rule-interaction-compute-scaling-from-vector (this :slvec) c) ))
    (if (this :artvec)
          (set-this 'dro (+ (or (this 'dro) 0)
                            (rule-interaction-compute-scaling-from-vector (this :artvec) c) )))
    (if (this :tvec)
        (set-this 'dr (* (this 'dr) 
                         (1+ (rule-interaction-compute-scaling-from-vector (this :tvec) c)))) )
    )))

    
;;apply gradual scaling of positive and negative numbers separately
;; (n1^c + n2^c + n3^c...)^(1/c)
(defun rule-interaction-compute-scaling-from-vector (vec c)
  (- (expt (loop for i from 0 to (1- (length vec))
               sum (if (plusp (aref vec i))  ;sum over pos numbers
                       (expt (aref vec i) c)
                     0 ))
           (/ 1.0 c) )
     (expt (loop for i from 0 to (1- (length vec))
               sum (if (minusp (aref vec i))  ;sum over neg number
                       (expt (- (aref vec i)) c)
                     0 ))
           (/ 1.0 c) )))


#| testing
(setq rulelist
   '(
     (HIGH-LOUD 1.0)
     (MELODIC-CHARGE 1.0 :AMP 1 :DUR 1 :VIBAMP 1)
     (HARMONIC-CHARGE 1.0 :AMP 1 :DUR 1 :VIBFREQ 1)
     (DURATION-CONTRAST 1.0 :AMP 1 :DUR 1)
     (DOUBLE-DURATION 1.0)
     (PUNCTUATION 1.1 :DUR 1 :DUROFF 1 :MARKPHLEVEL7 NIL)
     (PHRASE-ARCH 1.5 :PHLEVEL 5 :TURN 0.3 :NEXT 1.3 :AMP 2)
     (PHRASE-ARCH 1.5 :PHLEVEL 6 :TURN 2 :AMP 2 :LAST 0.2)
     (NORMALIZE-SL)
     (NORMALIZE-DR)
     (FINAL-RITARD 1.0)
     ))

(setq rulelist
   '(
     (metrical-accent 1.0)
     (harmonic-accent 1.0)
     (melodic-contour-accent 1.0)
     ;(phrase-arch 1 :phlevel 5 :turn 0.6 :next 1.3 :amp 2)
     (phrase-arch 1 :phlevel 6 :turn 0.5 :amp 2 :last 0.2 :next 1.5 :2next 3 :acc 0.5)
     ))

(defun foo (c)
  (reset-music)
  (rule-interaction-apply-rules-sync rulelist c)
  ;(normalize-sl)
  ;(normalize-dr)
  (redraw-display-windows) )
  
|#

;--------- batch application and playing -used for testing of rules-----------------
;; not loaded

#|

(defun foo ()
  (rule-apply-list
   '(
     (HIGH-LOUD 1.0)
     (MELODIC-CHARGE 1.0 :AMP 1 :DUR 1 :VIBAMP 1)
     (HARMONIC-CHARGE 1.0 :AMP 1 :DUR 1 :VIBFREQ 1)
     (DURATION-CONTRAST 1.0 :AMP 1 :DUR 1)
     ; (DURATION-CONTRAST-ART-DR 1.0)
     (DOUBLE-DURATION 1.0)
     (PUNCTUATION 1.1 :DUR 1 :DUROFF 1 :MARKPHLEVEL7 NIL)
     (PHRASE-ARCH 1.5 :PHLEVEL 5 :TURN 0.3 :NEXT 1.3 :AMP 2)
     (PHRASE-ARCH 1.5 :PHLEVEL 6 :TURN 2 :AMP 2 :LAST 0.2)
     (NORMALIZE-SL)
     (NORMALIZE-DR)
     (FINAL-RITARD 1.0)
     
     )))


(defun apply-all (quant)
  (let
      ((rulelist
        `(
          (high-loud ,quant)                   ;a0
          (melodic-charge ,quant)                  ;a0 dr va
          (harmonic-charge ,quant)                 ;a0 dr
          (faster-uphill ,quant)                  ;dr
          (duration-contrast ,quant)               ;a0 dr
          (Double-duration ,quant) ;dr
          (social-duration-care ,quant)       ;dr
          (phrase ,quant)                      ;dro dr
          (leap-articulation-dro ,quant)                  ;dro
          (repetition-articulation-dro ,quant)                ;dro
          (normalize-sl)
          (normalize-dr)
          )))
    (rule-apply-list-melsync rulelist) ))

(defun apply-all-melsync (quant)
  (let
      ((rulelist
        `(
          (faster-uphill ,quant)                  ;dr
          (duration-contrast ,quant)              ;a0 dr
          (Double-duration ,quant)          ;dr
          (social-duration-care ,quant)       ;dr
          (melodic-charge ,quant)                  ;a0 dr va
          (normalize-dr-bar)
          (high-loud ,quant)                   ;a0
          (harmonic-charge ,quant)                 ;a0 dr
          (phrase ,quant)                      ;dro dr
          (leap-articulation-dro ,quant)                  ;dro
          (repetition-articulation-dro ,quant)                ;dro
          (normalize-sl)
          (normalize-dr)
          )))
    (rule-apply-list-melsync rulelist) ))

(defvar *batch1*
    '(
      "hkyrie"
      "Bourrek"
      "HaydnKvart"
      "Sarabandek"
      "MozartAdur"
      "alaturka"
      "Ekor"
      "Berwald"
      "HaydnKorsord"
      "BachGmFuga"
      "Mendelson"
      "MozEssdur"
      "Ichtraumte"
      "Brahms"
      "H�ndel"
      "Mazurka" 
      "Mozdmkvart"
      "Ofullborda"
      "Roman"
      "Waltz"
      "Vide"
      "Militarer"
      ) )

(defvar *batch2*
    '(
      "Pisonatf"
      "chaconne"
      "finnskog"
      "fugacdur"
      "haydnfdur"
      "herdinna"
      "heroisk"
      "Midsommar"
      "Mozgdkvart"
      "Petruska"
      "sej"
      "Sorgeliga"
      "Svinstad"
      "Trauer"
      "Vila"
      "lucia"
      "Ramel"
      ))

(defvar *batch3*
    '(
      "DebussyGm4"
      "gesualdo5"
      "Backoral4"
      "fancy3"
      ;  "Nun fanget an4"
      ;  "Ricercare4K"
      ;  "Berwald4" "SchubertGdurKvart"
      ; "Invention8k" "Bachsonat3"
      ) )

(defun compute-batch1 ()
  (compute-batch *batch1*) )

(defun compute-batch2 ()
  (compute-batch *batch2*) )

(defun compute-batch3 ()
  (compute-batch *batch3*) )

(defun compute-batch (batch)
  (mapc '(lambda (file)
           (load-music-f file)
           (rule-apply-group-1-mel)
           (set-all 'va 0)
           (rem-all 'vf)
           (set-all 'dc 0)
           (rem-dro)
           (save-music-all-f (concatenate 'string file ".ar"))
           (mixed-intonation 1)
           (rem-dro)
           (save-music-all-f (concatenate 'string file ".ari"))
           )
    batch
    ))

(defun play-batch1 ()
  (play-batch *batch1*) )

(defun play-batch1-compare ()
  (play-batch-compare *batch1*) )

(defun play-batch2 ()
  (play-batch *batch2*) )

(defun play-batch2-compare ()
  (play-batch-compare *batch2*) )

(defun play-batch3 ()
  (play-batch *batch3*) )

(defun play-batch3-compare ()
  (play-batch-compare *batch3*) )

(defun play-batch3-compare1 ()
  (play-batch-compare1 *batch3*) )

(defun play-batch (batch)
  (mapc '(lambda (file)
           (print "file : " file)
           (read-pv (concatenate 'string file ".ar"))
           (play) )
    batch
    ))

(defun play-batch-compare (batch)
  (mapc '(lambda (file)
           (load-music-f (concatenate 'string file ".ar"))   ;no int
           (playlist)
           (load-music-f (concatenate 'string file ".ari"))   ;int
           (playlist) )
    batch
    ))

(defun play-batch-compare1 (batch)
  (mapc '(lambda (file)
           (print "file : " file)
           (read-pv file)                  ;no rules
           (init-music)
           (play)
           (read-pv (concatenate 'string file ".ar")) ;all rules
           (play)
           (play1) )                       ;each voice
    batch
    ))


;---punctuation batch----

(defvar *batch1*
    '(
      "HaydnKorsordK"
      ) )
(defvar *batch1*
    '(
      "Bourrek"
      "BachGmFuga"
      "Ichtraumte"
      "Mazurka" 
      "Roman"
      "chaconne"
      "finnskog"
      "Mozgdkvart"
      "Berwald"
      "HaydnKorsordK"
      "Vila"
      ) )

(defun compute-batch1-punct ()
  (compute-batch-punct *batch1*) )

(defun compute-batch-punct (batch)
  (mapc '(lambda (file)
           (load-music-f (concatenate 'string "monophonic:" file))
           (repetition-articulation-dro 1)
           (punctuation 1.0)
           (normalize-sl)
           (normalize-dr)
           (save-music-all-f (concatenate 'string "temp:" file ".punct"))
           (load-music-f (concatenate 'string "monophonic:" file))
           (high-loud 1)
           (melodic-charge 1)
           (harmonic-charge 1)
           (faster-uphill 1)
           (duration-contrast 1)
           (Double-duration 1)
           (leap-tone-duration 1)
           (social-duration-care 1)
           (leap-articulation-dro 1)
           (repetition-articulation-dro 1)
           (mixed-intonation 1)	            ;dc-list
           (punctuation 1.0)
           (normalize-sl)
           (normalize-dr)
           (final-ritard 0.5)
           (save-music-all-f (concatenate 'string "temp:" file ".ar"))
           )
    batch
    ))

(defun play-batch1-punct ()
  (mapc '(lambda (file)
           (load-music-f (concatenate 'string "monophonic:" file))
           (play)
           (draw-droff)
           (load-music-all-f (concatenate 'string "temp:" file ".punct"))
           (play)
           (draw-droff)
           (load-music-all-f (concatenate 'string "temp:" file ".ar"))
           (playlist)
           (draw-droff)
           )
    *batch1*
    ))

;(defun foo () (play-punct-batch))

;----------pitch-acc---------------

(defun play-batch-pitch-acc ()
  (mapc '(lambda (file)
           (load-music-f (concatenate 'string "monophonic:" file))
           (pitch-acc 1)
           (draw-f0-mean)
           )
    *batch1*
    ))

(defun play-batch-pitch-acc ()
  (mapc '(lambda (file)
           (load-music-f (concatenate 'string "monophonic:" file))
           (pitch-acc 4)
           (play)
           (draw-ddr%)
           )
    *batch1*
    ))

(defun play-batch-phrase ()
  (mapc '(lambda (file)
           (load-music-f (concatenate 'string "monophonic:" file))
           (mark-apply-phrase)
           (play)
           (draw-droff)
           )
    *batch1*
    ))

;---- intonation batch--------------  

(defun play-intonation-batch (batch)
  (mapc '(lambda (file)
           (print "file : " file)
           (load-music-f file)
           (rule-apply-group-1-mel)
           (set-all 'va 0)
           (rem-all 'vf)
           (rule-apply-list *intonation-rules1*)
           (playlist)
           )
    batch
    ))

(defun apply-int ()
  (rule-apply-group-1-mel)
  (set-all 'va 0)
  (rem-all 'vf)
  (rule-apply-list *intonation-rules1*)
  ;(playlist)
  )

(defun apply-no-int ()
  (rule-apply-group-1-mel)
  (set-all 'va 0)
  (rem-all 'vf)
  )

;;(defun foo () (play-intonation-batch *batch1*))
;;(defun foo () (play-intonation-batch *batch2*))
;;(defun foo () (play-intonation-batch *batch3*))

(defvar *intonation-rules1* '(
                              (mixed-intonation 1)
                              ;  (normalize-dr)
                              ;  (dr-limits)
                              (rem-dro)
                              ))



(defun play-intonation (batch)
  (mapc '(lambda (file)
           (print "file : " file)
           (read-pv file)
           (init-music)
           (set-all 'va 0)
           (high-sharp 1)
           (melodic-intonation 1)
           (gc)
           (play) )
    batch
    ))
|#

