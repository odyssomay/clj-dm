;;;-*-Mode: LISP; Package: DM -*-
;;
;; ****************************************
;;   init and conversion functions
;;   /Anders Friberg
;; ****************************************
;;2004-04-26/af fixed a bug in the application of set-dynamics
;;2004-09-21/af added  nsl support in reset-music function - other functions should be fixed as well
;;2006-02-15/af added  voice track init



(in-package :dm)

;; --------initialize -----------------------------------
;;
;; --------------
;;   INIT-MUSIC
;; --------------
;;
;; initializes all tracks in *active-score*
;;
(defun init-music ()
   (each-track-all-tracks  ;for init after save if some track are inactivated
    (if (typep *this-track* 'voice-track)
       (init-voice-track) )
    (init-music-score)
    ))

 
;; --------------
;;   INIT-MUSIC-SCORE
;; --------------
;;
;; initializes the note variables from the score
;;
(defun init-music-score ()
  (each-note
    ;(if (this 'dr) 
     ; (error "Performance parameters in file. Use Load music performance ... instead" ))
    (when (not (this 'rest))    ;not rest - set sl and f0
      (set-this 'sl 0)
      (set-this 'f0 (note-to-f0 (this 'n))) )
    (when (this 'bind)  ; converting old tie
      (rem-this 'bind)
      (set-this 'tie t)) )
  (set-dur-from-tempo)
  (mark-bar)
  ;(convert-chord)
  (if (get-dm-var 'init-music-include-dynamics)
     (set-dynamics 1))
  ;(set-first 'va 0)
  ;(set-first 'dc 0)
  ;(set-first 'vol 0)
  t)

;; --------------
;;   INIT-VOICE
;; --------------
;;
;; initializes voice tracks 
;;


;uses the dm vars to set up features and initial values for voice sysnthesis
(defun init-voice-track ()
  (each-segment
    ;(if (this 'dr) 
    ;  (error "Performance parameters in file. Use Load music performance ... instead" ))
   (cond 
     ((this 'ph)
      (set-this 'sl 0) ;speech segments in rests will not be initialized by init-music-score
      (let ((ph (this 'ph))
            (feature-list (get-dm-var 'phoneme-features))
            (var-list (get-dm-var 'phoneme-init-parameters)) )
        ;init features
        (dolist (feature feature-list)
          (when (string= ph (car feature))
            (dolist (one (cdr feature))
              (set-this one t) )
            (return) ))
        ;init parameters
        (dolist (var var-list)
          (when (string= ph (car var))
            (setf (var-list *this-segment*) (append (var-list *this-segment*) (list-to-alist (cdr var))))
            (return) ))
        )))))

#|
;;custom voice init for jaans experiment
(defun init-voice-track-jaan ()
  (each-segment
    ;(if (this 'dr) 
    ;  (error "Performance parameters in file. Use Load music performance ... instead" ))
   (cond 
     ((this 'ph)
      (set-this 'sl 0) ;speech segments in rests will not be initialized by init-music-score
      (let ((ph (this 'ph)))
         (cond
          ((string= ph "S") (set-this 'f0 38)(set-this 'dr 139)(set-this 'ndr 139)(set-this 'cons t)(set-this 'locked-dr t))
          ((string= ph "TS") (set-this 'f0 37)(set-this 'dr 163)(set-this 'ndr 163)(set-this 'cons t)(set-this 'locked-dr t))
          ((string= ph "D") (set-this 'f0 41)(set-this 'dr 50)(set-this 'ndr 50)(set-this 'cons t)(set-this 'locked-dr t))
          ((string= ph "T") (set-this 'f0 36)(set-this 'dr 139)(set-this 'ndr 139)(set-this 'cons t)(set-this 'locked-dr t))
          ((string= ph "K") (set-this 'f0 40)(set-this 'dr 102)(set-this 'ndr 102)(set-this 'cons t)(set-this 'locked-dr t))
          
          ((string= ph "E") (set-this 'dr 240)(set-this 'ndr 240)(set-this 'vowel t)(set-this 'f1 470)(set-this 'f2 1630)(set-this 'f3 2560)(set-this 'f4 3220))
          ((string= ph "O") (set-this 'dr 380)(set-this 'ndr 380)(set-this 'vowel t)(set-this 'f1 470)(set-this 'f2 900)(set-this 'f3 2560)(set-this 'f4 3050))
          ((string= ph "A") (set-this 'dr 250)(set-this 'ndr 250)(set-this 'vowel t)(set-this 'f1 540)(set-this 'f2 1050)(set-this 'f3 2640)(set-this 'f4 3120))
          ((string= ph "I") (set-this 'dr 160)(set-this 'ndr 160)(set-this 'vowel t)(set-this 'f1 410)(set-this 'f2 2080)(set-this 'f3 2540)(set-this 'f4 3200))
          ((string= ph "EI") (set-this 'dr 340)(set-this 'ndr 340)(set-this 'vowel t)
           (set-this 'f1 (this-segment-make-time-shape))(insert-break-point (this 'f1) 0 580)(insert-break-point (this 'f1) (this 'dr) 430)
           (set-this 'f2 (this-segment-make-time-shape))(insert-break-point (this 'f2) 0 1680)(insert-break-point (this 'f2) (this 'dr) 2150)
           (set-this 'f3 (this-segment-make-time-shape))(insert-break-point (this 'f3) 0 2620)(insert-break-point (this 'f3) (this 'dr) 2600)
           (set-this 'f4 (this-segment-make-time-shape))(insert-break-point (this 'f4) 0 3160)(insert-break-point (this 'f4) (this 'dr) 3350)
           )
           ;(set-this 'f1 580)(set-this 'f2 1680)(set-this 'f3 2620)(set-this 'f4 3160))
          (t (warn "phoneme not recognized: " ph))
          )))
      (t (set-this 'dr 50) (set-this 'ndr 50)) ;everything else including rests will get a dr of 50 ms
    )))
|#


;; ---------------
;;   RESET-MUSIC
;; ---------------
;;
;; reinitialize before rule application
;;
#|
(defun reset-music ()
  (each-note
    (set-this 'dr (this 'ndr))
    (rem-this 'dro)
    (if (not (this 'rest))
      (set-this 'sl 0) )
    (rem-this 'dc)
    (rem-this 'va)
    (rem-this 'vf)
    (rem-this 'vol)
    (rem-this 'pedal-perf) )
  (if (get-dm-var 'init-music-include-dynamics) (set-dynamics 1))
  ;(set-first 'va 0)
  ;(set-first 'dc 0)
  ;(set-first 'vol 0)
  )
|#

;;added nsl support
(defun reset-music ()
  (each-note
    (set-this 'dr (this 'ndr))
    (rem-this 'dro)
   (if (not (this 'rest))
       (if (this 'nsl) (set-this 'sl (this 'nsl))
         (set-this 'sl 0) ))
    (rem-this 'dc)
    (rem-this 'va)
    (rem-this 'vf)
    (rem-this 'vol)
    (rem-this 'pedal-perf) )
  (if (get-dm-var 'init-music-include-dynamics) (set-dynamics 1))
  ;(set-first 'va 0)
  (set-first 'dc 0)
  ;(set-first 'vol 0)
  )

;a fix for not resetting read pitch bend
(defun reset-music-no-dc ()
  (each-note
    (set-this 'dr (this 'ndr))
    (rem-this 'dro)
   (if (not (this 'rest))
       (if (this 'nsl) (set-this 'sl (this 'nsl))
         (set-this 'sl 0) ))
    ;(rem-this 'dc)
    (rem-this 'va)
    (rem-this 'vf)
    (rem-this 'vol)
    (rem-this 'pedal-perf) )
  (if (get-dm-var 'init-music-include-dynamics) (set-dynamics 1))
  ;(set-first 'va 0)
  ;(set-first 'dc 0)
  ;(set-first 'vol 0)
  )


;;
;; -----------------
;;   INIT-MUSIC-MF
;; -----------------
;;
;; as above for midi-file
;;
(defun init-music-mf ()
  (each-note
    (cond ((this 'rest))     ;rest
          (t (set-this 'sl 0) ;else
             (set-this 'n (cons (f0-to-toneoctave (this 'f0))
                                (cdr (this 'n))))
              )))
  (set-dur-from-tempo)
  (mark-bar)
  ;(convert-chord)
  (if (get-dm-var 'init-music-include-dynamics) (set-dynamics 1))
  ;(set-first 'va 0)
  ;(set-first 'dc 0)
  ;(set-first 'vol 0)
  t)

;;
;; ----------------
;;   SET-DYNAMICS
;; ----------------
;;
#|
(defun set-dynamics (quant)
  (let ((distance (* 4 quant))(a0add 0)) ;dB
     (each-note-if
       (this 'dyn)
       (then
        (setq a0add
          (* distance
             (case (this 'dyn)
               (ppp -3)(pp -2)(p -1)(mp 0)(mf 1)(f 2)(ff 3)(fff 4)
               (t  (if (get-dm-var 'verbose-i/o) (print-this "this dynamic not implemented " (this 'dyn) nil)))
               )))
        (if (not (this 'rest)) (add-this 'sl a0add))
        ))))
|#

;;distance defines the distance between the dynamics marking in dB
;;2004-04-26/af bugfix
(defun set-dynamics (quant)
  (let ((dsl 0) (distance (* 4 quant))) ;dB
     (each-note
      (if (first?) (setq dsl 0)) ;reset for each track "each track" could not be used
      (if (this 'dyn) 
          (setq dsl (* distance (case (this 'dyn)
                                  (ppp -3)(pp -2)(p -1)(mp 0)(mf 1)(f 2)(ff 3)(fff 4)
                                  (t  (if (get-dm-var 'verbose-i/o) (print-this "this dynamic mark is not implemented: " 'dyn nil)))
                                  ))))
      (if (and (numberp dsl) (not (zerop dsl)) (not (this 'rest)) (this 'sl))
          (add-this 'sl dsl))
      )))

;;
;; ----------------------
;;   SET-DUR-FROM-TEMPO
;; ----------------------
;;
;; sets dr and ndr from mm, note ,dot and t
;;
;;; (defun set-dur-from-tempo ()
;;;  (let ((mm 187))
;;;   (each-note 
;;;      (if (this 'mm)
;;;          (setq mm (this 'mm)))
;;;      (set-this 'dr
;;;        (* (nom-dr *i*) (/ 187.5 mm) ))
;;;      (set-this 'ndr (this 'dr)) )))

;;including the new note format with fractions
;;and default-tempo from the env objects /af
;;; (defun set-dur-from-tempo ()
;;;   (let ((mm (get-dm-var 'default-tempo))
;;;         factor)
;;;      (each-note 
;;;        (when (this 'mm)
;;;           (setq mm (this 'mm))
;;;           (setq factor (/ (* 4.0 60000.0) mm)) )
;;;        (if (listp (cdr (this 'n)))
;;;           (progn
;;;             (set-this 'ndr (* factor (apply #'+ (cdr (this 'n)))))
;;;             (set-this 'dr (this 'ndr)) )
;;;           (progn
;;;             (set-this 'dr
;;;               (* (nom-dr *i*) (/ 187.5 mm) ))
;;;              (set-this 'ndr (this 'dr)) )))))

;new using get-note-value-fraction
;including that the beat can be for example half notes
(defun set-dur-from-tempo ()
   (let* ((mm (get-dm-var 'default-tempo))
         (beat-value 4)  ;default quarter note beat
          (factor (/ (* beat-value 60000.0) mm))
          )
     (if (not (get-first 'mm)) (warn "no mm mark in the beginning of score"))
     (if (not (get-first 'meter)) (warn "no meter mark in the beginning score"))
      (each-note
       (when (this 'meter)
           (setq beat-value (cadr (this 'meter))) )
       (when (this 'mm)
           (setq mm (this 'mm))
           (setq factor (/ (* beat-value 60000.0) mm)) )
       (set-this 'ndr (* factor (get-note-value-fraction *i*)))
       (set-this 'dr (this 'ndr)) )))

      
;convert the note information to one fraction (e.g. 3/8, 1/4)
;both the new and old format are allowed
(defun get-note-value-fraction (i)
   (if (listp (cdr (iget i 'n)))
      (apply #'+ (cdr (iget i 'n)))
      (let ((notevalue (/ 1 (cdr (iget i 'n))))) ;fraction list notation
            (if (iget i 'dot)
               (case (iget i 'dot)
                 (1 (setq notevalue (* notevalue 3/2)))
                 (2 (setq notevalue (* notevalue 7/4)))
                 (t (error 'get-note-value-fraction "wrong dot value on note" *i*)) ))
            (if (iget i 't)
               (let ((tuple (iget i 't)))
                  (case tuple
                    (3 (setq notevalue (* notevalue 2/3)))
                    (5 (setq notevalue (* notevalue 4/5)))
                    (t (if (listp tuple)
                          (setq notevalue (/ (* notevalue (car tuple)) (cdr tuple)))
                          (error 'get-note-value-fraction "this tuplet not implemented: " tuple) ))
                    )))
            (if (iget i 'tuple)
               (let ((tuple (iget i 'tuple)))
                  (case tuple
                    (3 (setq notevalue (* notevalue 2/3)))
                    (5 (setq notevalue (* notevalue 4/5)))
                    (t (if (listp tuple)
                          (setq notevalue (/ (* notevalue (car tuple)) (cdr tuple)))
                          (error 'get-note-value-fraction "this tuplet not implemented: " tuple) ))
                    )))
            notevalue )))
;;
;; ----------
;;   NOM-DR
;; ----------
;;
;; get nominal duration in ms float
;; for tuplets 4/4 time is assumed otherwise the dr factor must be supplied
;; as a list: t (6 . 5)
;;
(defun nom-dr (i)
 (let ((nom-dr (float (note-to-dr (iget i 'n)))))
   (if (iget i 'dot)
       (case (iget i 'dot)
          (1 (setq nom-dr (* nom-dr 1.5)))
          (2 (setq nom-dr (* nom-dr 1.75)))
          (t (error 'nom-dr "wrong dot value on note" *i*)) ))
   (if (iget i 't)
     (let ((tuple (iget i 't)))
       (case tuple
         (3 (setq nom-dr (/ (* nom-dr 2) 3. )))
         (5 (setq nom-dr (/ (* nom-dr 4) 5. )))
         (t (if (listp tuple)
                (setq nom-dr (/ (* nom-dr (car tuple)) (cdr tuple)))
                (error 'nom-dr "this tuplet not implemented: " tuple) ))
        )))
   (if (iget i 'tuple)
     (let ((tuple (iget i 'tuple)))
       (case tuple
         (3 (setq nom-dr (/ (* nom-dr 2) 3. )))
         (5 (setq nom-dr (/ (* nom-dr 4) 5. )))
         (t (if (listp tuple)
                (setq nom-dr (/ (* nom-dr (car tuple)) (cdr tuple)))
                (error 'nom-dr "this tuplet not implemented: " tuple) ))
        )))
   nom-dr ))



#|  ej klart -svOrt med trioler osv
(defun set-note-from-ndr ()
 (let ((mm)(ndr))
  (each-note
    (if (this 'mm) (setq mm (this 'mm)))
    (let ((note (this 'n))
          (dot (this 'dot))
          (triol (this 't)) )
        (if dot 
            (case dot
              (1 (setq val (+ val (
        (set-this 'n 
          (to-note (note-to-tone note)
                   (note-to-octave note)
                   (dr-to-notevalue 
)))))))))))))
|#


;;
;; ------------
;;   MARK-BAR
;; ------------
;;
;;; (defun mark-bar ()
;;;   (block mark-bar
;;;     (each-track
;;;       (let ((bar-dr 0)(dr-ack 0)(barnr 0)(meter))
;;;         (each-note
;;;           (if (this 'bar) (return-from mark-bar))  ;if already done exit
;;;           (if (first?) (set-this 'bar barnr))
;;;           (when (this 'meter) 
;;;             (setq meter (this 'meter))
;;;             (setq bar-dr
;;;                   (round
;;;                    (* (car meter)
;;;                       (note-to-dr (cons nil (cadr meter))) )))
;;;             (setq dr-ack 0)
;;;             (set-this 'bar (incf barnr)))
;;;           (when (= (round dr-ack) bar-dr)
;;;             (setq dr-ack 0) 
;;;             (set-this 'bar (incf barnr)) )
;;;           ;(format t "~&bar-dr : ~A" bar-dr)
;;;           (setq dr-ack (+ dr-ack (nom-dr *i*)))
;;;           )))))
;;; 
;;; ;as above for midifile
;;; (defun mark-bar-mf ()
;;;   (block mark-bar
;;;     (each-track
;;;       (let ((bar-dr 0)(dr-ack 0)(barnr 0)(meter))
;;;         (each-note
;;;           (if (this 'bar) (return-from mark-bar))  ;if already done exit
;;;           (if (first?) (set-this 'bar barnr))
;;;           (when (this 'meter) 
;;;             (setq meter (this 'meter))
;;;             (setq bar-dr (* (car meter) (/ 1 (cadr meter))))
;;;             (setq dr-ack 0)
;;;             (set-this 'bar (incf barnr)))
;;;           (when (= dr-ack bar-dr)
;;;             (setq dr-ack 0) 
;;;             (set-this 'bar (incf barnr)) )
;;;           (while (> dr-ack bar-dr)
;;;             (setq dr-ack (- dr-ack bar-dr)) 
;;;             (incf barnr) )
;;;           ;(format t "~&bar-dr : ~A" bar-dr)
;;;           (setq dr-ack (+ dr-ack (apply '+ (cdr (this 'n)))))
;;;           )))))


;supports both note formats
;;;(defun mark-bar ()
;;;  (block mark-bar
;;;    (each-track
;;;      (let ((bar-dr 0)(dr-ack 0)(barnr 0)(meter))
;;;        (each-note
;;;          (if (this 'bar) (return-from mark-bar))  ;if already done exit
;;;          (if (first?) (set-this 'bar barnr))
;;;          (when (this 'meter) 
;;;            (setq meter (this 'meter))
;;;            (setq bar-dr (/ (car meter) (cadr meter)))
;;;            (setq dr-ack 0)
;;;            (set-this 'bar (incf barnr)))
;;;          (when (= dr-ack bar-dr)
;;;            (setq dr-ack 0) 
;;;            (set-this 'bar (incf barnr)) )
;;;          (while (> dr-ack bar-dr)
;;;            (setq dr-ack (- dr-ack bar-dr)) 
;;;            (incf barnr) )
;;;          (format t "~&note : ~A dr-ack : ~A barnr : ~A" (this 'n) dr-ack barnr)
;;;          (incf dr-ack (get-note-value-fraction *i*))
;;;          )))))

;;0009/af mark also bar right after jump
;;0010/af included default meter and warn
(defun mark-bar ()
  (block mark-bar
    (each-track
      (let ((bar-dr 1)(dr-ack 0)(barnr 1)(meter '(4 4)) (meter-found-p nil)) ;default 4/4
        (each-note
          (if (this 'bar) (return-from mark-bar))  ;if already done exit
          (if (first?) (set-this 'bar barnr))
          (when (this 'meter) 
            (setq meter (this 'meter))
            (setq bar-dr (/ (car meter) (cadr meter)))
            (setq dr-ack 0)
            (if (not (first?)) (set-this 'bar (incf barnr)))
            (setq meter-found-p t) )
          (while (> dr-ack bar-dr)
            (setq dr-ack (- dr-ack bar-dr)) 
            (incf barnr) )
          (when (= dr-ack bar-dr)
            (setq dr-ack 0) 
            (set-this 'bar (incf barnr)) )
          ;(format t "~&note : ~A dr-ack : ~A barnr : ~A" (this 'n) dr-ack barnr)
          (incf dr-ack (get-note-value-fraction *i*))
         )
        (if (not meter-found-p) (warn "mark-bar: meter not found in track - using 4/4"))
        ))))



;;
;; -----------------
;;   ASK-FOR-METER
;; -----------------
;;
(defun ask-for-meter ()
  (let ((meter t))
    (while (and meter (not (listp meter)))
      (print "enter meter (4 4) etc ")
      (setq meter (read)))
    meter ))
  
 
;; ------------------------------------------------------------------
;; -----   C O N V E R S I O N S     --------------------------------
;; ------------------------------------------------------------------
;;
;; -----------------
;;   TONE-TO-TONNR
;; -----------------
;;0009/af case insensitive
(defun tone-to-tonnr (tone)
  (cond ((string-equal tone "C") 0)
        ((string-equal tone "D") 2)
        ((string-equal tone "E") 4)
        ((string-equal tone "F") 5)
        ((string-equal tone "G") 7)
        ((string-equal tone "A") 9)
        ((string-equal tone "B") 11)
        ((string-equal tone "C#") 1)
        ((string-equal tone "Db") 1)
        ((string-equal tone "D#") 3)
        ((string-equal tone "Eb") 3)
        ((string-equal tone "F#") 6)
        ((string-equal tone "Gb") 6)
        ((string-equal tone "G#") 8)
        ((string-equal tone "Ab") 8)
        ((string-equal tone "A#") 10)
        ((string-equal tone "Bb") 10)
        ((string-equal tone "Cb") 11)
        ((string-equal tone "Fb") 4)
        ((string-equal tone "B#") 0)
        ((string-equal tone "E#") 5)
        (t (error "tone not defined: ~A" tone))
        ))

;;
;; -----------------
;;   TONNR-TO-TONE
;; -----------------
;;
;; convert a number to a string
;;
(defun tonnr-to-tone (tonnr)
       (nth (mod tonnr 12) *12tones*) )

;;
;; ----------------------
;;   SHARP-TONE-TO-FLAT
;; ----------------------
;;
(defun sharp-tone-to-flat (tone)
 (cond ((string= tone "C#") "B")
        ((string= tone "D#") "Eb")
        ((string= tone "E#") "F")
        ((string= tone "F#") "Gb")
        ((string= tone "G#") "Ab")
        ((string= tone "A#") "Bb")
        ((string= tone "B#") "C")
        ))

;;
;; ----------------------
;;   FLAT-TONE-TO-SHARP
;; ----------------------
;;
(defun flat-tone-to-sharp (tone)
 (cond ((string= tone "Cb") "B")
        ((string= tone "Db") "C#")
        ((string= tone "Eb") "D#")
        ((string= tone "Fb") "E")
        ((string= tone "Gb") "F#")
        ((string= tone "Ab") "G#")
        ((string= tone "Bb") "A#")
        ))

;;
;; ---------------
;;   SHARP-TONE?
;; ---------------
;;
(defun sharp-tone? (tone)
  (and (> (length tone) 1)(char= #\# (char tone 1))) )

;;
;; --------------
;;   FLAT-TONE?
;; --------------
;;
(defun flat-tone? (tone)
  (and (> (length tone) 1)(char= #\b (char tone 1))) )

;;
;; ------------------------
;;   F0-TO-TONEOCTAVE-ONE
;; ------------------------ 
;;
(defun f0-to-toneoctave-one (f0)
 (let ((octave -1))
  (while (>= f0 12)
      (incf octave)
      (decf f0 12) )
  (concatenate 'string 
               (tonnr-to-tone f0)
               (prin1-to-string octave)) ))

;;
;; ------------------------
;;   F0-TO-TONEOCTAVE
;; ------------------------ 
;;
;; for chords or single values
;;
(defun f0-to-toneoctave (f0)
 (cond ((integerp f0)
        (f0-to-toneoctave-one f0))
       ((listp f0)
        (mapcar #'f0-to-toneoctave-one f0) )
       (t (error "Not a notenumber or list ~A" f0))
       ))

;;
;; --------------------
;;   TONEOCTAVE-TO-F0
;; -------------------- 
;;
(defun toneoctave-to-f0 (tone)
  (+ (* 12 (toneoctave-to-octave tone))
     12 
     (tone-to-tonnr (toneoctave-to-tone tone)) ))
 
;;
;; ------------------------
;;   TONEOCTAVE-TO-OCTAVE
;; ------------------------ 
;;
(defun toneoctave-to-octave (tone)
  (if tone
    (read-from-string
     (subseq tone (1- (length tone))) )
    nil ))

;;
;; ----------------------
;;   TONEOCTAVE-TO-TONE
;; ---------------------- 
;;
(defun toneoctave-to-tone (tone)
 (if tone
     (subseq tone 0 (1-  (length tone)))
     nil ))

;;
;; -----------------
;;   TO-TONEOCTAVE
;; ----------------- 
;;
(defun to-toneoctave (tone octave)
  (concatenate 'string tone (prin1-to-string octave)) )

;;
;; --------------------------------
;;   SHARP-TONEOCTAVE-TO-FLAT-ONE
;; -------------------------------- 
;;
(defun sharp-toneoctave-to-flat-one (toneoctave)
  (let ((tone (toneoctave-to-tone toneoctave)))
    (cond
     ((sharp-tone? tone)
      (to-toneoctave (sharp-tone-to-flat tone) (toneoctave-to-octave toneoctave)) )
     (t toneoctave) )))

;;
;; ----------------------------
;;   SHARP-TONEOCTAVE-TO-FLAT
;; ---------------------------- 
;;    
;; for chords or single values
;;
(defun sharp-toneoctave-to-flat (toneoctave)
(cond ((stringp toneoctave)
        (sharp-toneoctave-to-flat-one toneoctave))
       ((listp toneoctave)
        (mapcar #'sharp-toneoctave-to-flat-one toneoctave) )
       (t (error "Not a toneoctave or list ~A" toneoctave))
       ))

;;
;; --------------------------------
;;   FLAT-TONEOCTAVE-TO-SHARP-ONE
;; -------------------------------- 
;;
(defun flat-toneoctave-to-sharp-one (toneoctave)
  (let ((tone (toneoctave-to-tone toneoctave)))
    (cond
     ((flat-tone? tone)
      (to-toneoctave (flat-tone-to-sharp tone) (toneoctave-to-octave toneoctave)) )
     (t toneoctave) )))

;;
;; ----------------------------
;;   FLAT-TONEOCTAVE-TO-SHARP
;; ---------------------------- 
;;    
;; for chords or single values      
;;
(defun flat-toneoctave-to-sharp (toneoctave)
(cond ((stringp toneoctave)
        (flat-toneoctave-to-sharp-one toneoctave))
       ((listp toneoctave)
        (mapcar #'flat-toneoctave-to-sharp-one toneoctave) )
       (t (error "Not a toneoctave or list ~A" toneoctave))
       ))

;; --------------------------------------------
;; ----  notes ex: ("Gb3" . 4) ----------------
;; --------------------------------------------
;;
;; ------------------
;;   NOTE-TO-OCTAVE
;; ------------------ 
;;
(defun note-to-octave (note)
  (if (car note)
    (read-from-string 
      (subseq (car note) (1- (length (car note)))) )
    nil ))

;(defun note-to-name (note)
; (if (car note)
;     (subseq (car note) 0 (1-  (length (car note))))
;     nil ))

;;
;; ----------------
;;   NOTE-TO-TONE
;; ---------------- 
;;
(defun note-to-tone (note)
  (let ((ton (car note)))
    (if (listp ton)(setq ton (car ton)))
    (if ton
        (subseq ton 0 (1-  (length ton)))
         nil )))

;;
;; ---------------------
;;   NOTE-TO-NOTEVALUE
;; --------------------- 
;;
(defun note-to-notevalue (note)
   (cdr note) )

;
;; ---------------------
;;   midiNOTEvalue-TO-NOTEVALUE
;; --------------------- 
;; a simple utility for converting notevalues from midifile to the old format
;; do not consider composite notevalues i.e. a list of notevalues
;; or double dotted
(defun midinotevalue-to-notevalue ()
  (each-note-if
   (listp (cdr (this 'n)))
   (then
    (let ((midinotevalue (cdr (this 'n))))
      (cond ((= 1 (length midinotevalue))
             (cond ((and (= 1 (numerator (car midinotevalue)))           ;normal
                         (member (denominator (car midinotevalue)) '(1 2 4 8 16 32 64)) )
                    (set-this 'n (cons (car (this 'n)) (denominator (car midinotevalue)))) )
                                       
                   ((and (= 3 (numerator (car midinotevalue)))           ;dotted
                         (member (denominator (car midinotevalue)) '(1 2 4 8 16 32 64)) )
                    (set-this 'n (cons (car (this 'n)) (/ (denominator (car midinotevalue)) 2)))
                    (set-this 'dot 1) )
                   
                   ((and (= 1 (numerator (car midinotevalue)))           ;triplets
                          (member (denominator (car midinotevalue)) '(3 6 12 24 48 96 192)) )
                    (set-this 'n (cons (car (this 'n)) (/ (* (denominator (car midinotevalue)) 2) 3)))
                    (set-this 'tuple 3) )
                   (t (error "not a valid notevalue for conversion : ~A" midinotevalue))
                   ))
            (t (error "not a single notevalue : ~A" midinotevalue))
            )))))
                   
;;
;; -----------------
;;   NOTE-TO-TONNR
;; ----------------- 
;;
(defun note-to-tonnr (note)
   (tone-to-tonnr (note-to-tone note)))

;(defun note-to-f0 (note)
;		(+ (* 12 (note-to-octave note))
;     12 
;     (tone-to-tonnr (note-to-tone note)) ))

;;
;; --------------
;;   NOTE-TO-F0
;; -------------- 
;;
;; chords included with sorted lists: top note first
;;
(defun note-to-f0 (note)
 (if (not (listp (car note)))
		(+ (* 12 (note-to-octave note))
     12 
     (tone-to-tonnr (note-to-tone note)) )
  (sort (mapcar 'toneoctave-to-f0 (car note)) '>)
  ))

;;
;; --------------
;;   NOTE-TO-DR
;; -------------- 
;;
(defun note-to-dr (note)
  (case (note-to-notevalue note)
    (1 1280)(2 640)(4 320)(8 160)(16 80)(32 40)(64 20)(128 10) ))

;;
;; -------------------
;;   DR-TO-NOTEVALUE
;; -------------------
;;
(defun dr-to-notevalue (dr)
  (case dr
    (1280 1)(640 2)(320 4)(160 8)(80 16)(40 32)(20 64)(10 128) ))
;;
;; -----------
;;   TO-NOTE
;; -----------
;;
(defun to-note (name octave val)
  (cons (concatenate 'string name (prin1-to-string octave)) val) )
  
;(setq note '("A#3" . 16))

;; ----------------------
;;   SHARP-NOTE-TO-FLAT
;; ----------------------
;;
(defun sharp-note-to-flat (note)
  (cons (sharp-toneoctave-to-flat (car note)) (cdr note)) )

;; ----------------------
;;   FLAT-NOTE-TO-SHARP
;; ----------------------
;;
(defun flat-note-to-sharp (note)
  (cons (flat-toneoctave-to-sharp (car note)) (cdr note)) )


;;keep only top note in chords
(defun remove-chord-notes ()
  (each-note-if
   (not (this 'rest))
   (listp (car (this 'n)))
   (then 
    (let ((note (car (this 'n)))
          (value (cdr (this 'n)))
          )
      ;(setq note (car note))
      ;(if (= (length note) 1) (setq note (car note)))
      (set-this 'n (cons (car note) value))
      ))))

(defun keep-only-chord-notes ()
  (each-note-if
   (not (this 'rest))
   (then
    (let ((note (car (this 'n)))
          (value (cdr (this 'n)))
          )
      (cond ((listp note)  ;if chord remove top note
             (setq note (cdr note))
             (if (= (length note) 1) (setq note (car note)))
             (set-this 'n (cons note value)) )
            (t              ;if not chord insert rest
             (set-this 'n (cons nil value))
             (set-this 'rest t) )
      )))))

;; -------------------------------------------------------------
;; ----------   chord conversion   -----------------------------
;; -------------------------------------------------------------

;; --------------------------------
;;   qlist-to-tonnrlist
;; --------------------------------

(defun qlist-to-tonnrlist (qlist)
  (setq qlist (mapcar #'tone-to-tonnr qlist))
  (let ((root (car qlist)))
    (setq qlist
          (mapcar #'-
                  qlist
                  (make-list (length qlist) :initial-element root) ))
    (setq qlist
          (mapcar #'(lambda (nr)
                     (loop while (< nr 0) do (incf nr 12))
                     (loop while (> nr 12) do (decf nr 12))
                     nr )
                  qlist) )
    qlist ))


;; --------------------------------
;;   qname-to-qlist
;; --------------------------------

;;0009/af new syntax and the old one as well
;; case insensitive
(defun qname-to-qlist (str)
    (cond
      ((string-equal str "Cmaj") '("C" "E" "G"))
      ((string-equal str "Cmin") '("C" "Eb" "G"))
      ((string-equal str "Cdim") '("C" "Eb" "Gb"))
      ((string-equal str "Dmaj") '("D" "F#" "A"))
      ((string-equal str "Dmin") '("D" "F" "A"))
      ((string-equal str "Ddim") '("D" "F" "Ab"))
      ((string-equal str "Emaj") '("E" "G#" "B"))
      ((string-equal str "Emin") '("E" "G" "B"))
      ((string-equal str "Edim") '("E" "G" "Bb"))
      ((string-equal str "Fmaj") '("F" "A" "C"))
      ((string-equal str "Fmin") '("F" "Ab" "C"))
      ((string-equal str "Fdim") '("F" "Ab" "B"))
      ((string-equal str "Gmaj") '("G" "B" "D"))
      ((string-equal str "Gmin") '("G" "Bb" "D"))
      ((string-equal str "Gdim") '("G" "Bb" "Db"))
      ((string-equal str "Amaj") '("A" "C#" "E"))
      ((string-equal str "Amin") '("A" "C" "E"))
      ((string-equal str "Adim") '("A" "C" "Eb"))
      ((string-equal str "Bmaj") '("B" "D#" "F#"))
      ((string-equal str "Bmin") '("B" "D" "F#"))
      ((string-equal str "Bdim") '("B" "D" "F"))
      ((string-equal str "C#maj") '("C#" "F" "G#"))
      ((string-equal str "C#min") '("C#" "E" "G#"))
      ((string-equal str "C#dim") '("C#" "E" "G"))
      ((string-equal str "Dbmaj") '("Db" "F" "Ab"))
      ((string-equal str "Dbmin") '("Db" "E" "Ab"))
      ((string-equal str "Dbdim") '("Db" "E" "G"))
      ((string-equal str "D#maj") '("D#" "G" "A#"))
      ((string-equal str "D#min") '("D#" "F#" "A#"))
      ((string-equal str "D#dim") '("D#" "F#" "A"))
      ((string-equal str "Ebmaj") '("Eb" "G" "Bb"))
      ((string-equal str "Ebmin") '("Eb" "Gb" "Bb"))
      ((string-equal str "Ebdim") '("Eb" "Gb" "A"))
      ((string-equal str "F#maj") '("F#" "A#" "C#"))
      ((string-equal str "F#min") '("F#" "A" "C#"))
      ((string-equal str "F#dim") '("F#" "A" "C"))
      ((string-equal str "Gbmaj") '("Gb" "Bb" "Db"))
      ((string-equal str "Gbmin") '("Gb" "A" "Db"))
      ((string-equal str "Gbdim") '("Gb" "A" "C"))
      ((string-equal str "G#maj") '("G#" "C" "D#"))
      ((string-equal str "G#min") '("G#" "B" "D#"))
      ((string-equal str "G#dim") '("G#" "B" "D"))
      ((string-equal str "Abmaj") '("Ab" "C" "Eb"))
      ((string-equal str "Abmin") '("Ab" "B" "Eb"))
      ((string-equal str "Abdim") '("Ab" "B" "D"))
      ((string-equal str "A#maj") '("A#" "D" "F"))
      ((string-equal str "A#min") '("A#" "C#" "F"))
      ((string-equal str "A#dim") '("A#" "C#" "E"))
      ((string-equal str "Bbmaj") '("Bb" "D" "F"))
      ((string-equal str "Bbmin") '("Bb" "Db" "F"))
     ((string-equal str "Bbdim") '("Bb" "Db" "E"))
           ((string-equal str "C") '("C" "E" "G"))
      ((string-equal str "Cm") '("C" "Eb" "G"))
      ((string-equal str "D") '("D" "F#" "A"))
      ((string-equal str "Dm") '("D" "F" "A"))
      ((string-equal str "E") '("E" "G#" "B"))
      ((string-equal str "Em") '("E" "G" "B"))
      ((string-equal str "F") '("F" "A" "C"))
      ((string-equal str "Fm") '("F" "Ab" "C"))
      ((string-equal str "G") '("G" "B" "D"))
      ((string-equal str "Gm") '("G" "Bb" "D"))
      ((string-equal str "A") '("A" "C#" "E"))
      ((string-equal str "Am") '("A" "C" "E"))
      ((string-equal str "B") '("B" "D#" "F#"))
      ((string-equal str "Bm") '("B" "D" "F#"))
      ((string-equal str "C#") '("C#" "F" "G#"))
      ((string-equal str "C#m") '("C#" "E" "G#"))
      ((string-equal str "Db") '("Db" "F" "Ab"))
      ((string-equal str "Dbm") '("Db" "E" "Ab"))
      ((string-equal str "D#") '("D#" "G" "A#"))
      ((string-equal str "D#m") '("D#" "F#" "A#"))
      ((string-equal str "Eb") '("Eb" "G" "Bb"))
      ((string-equal str "Ebm") '("Eb" "Gb" "Bb"))
      ((string-equal str "F#") '("F#" "A#" "C#"))
      ((string-equal str "F#m") '("F#" "A" "C#"))
      ((string-equal str "Gb") '("Gb" "Bb" "Db"))
      ((string-equal str "Gbm") '("Gb" "A" "Db"))
      ((string-equal str "G#") '("G#" "C" "D#"))
      ((string-equal str "G#m") '("G#" "B" "D#"))
      ((string-equal str "Ab") '("Ab" "C" "Eb"))
      ((string-equal str "Abm") '("Ab" "B" "Eb"))
      ((string-equal str "A#") '("A#" "D" "F"))
      ((string-equal str "A#m") '("A#" "C#" "F"))
      ((string-equal str "Bb") '("Bb" "D" "F"))
     ((string-equal str "Bbm") '("Bb" "Db" "F"))
     (t (error "qname-to-qlist: chord symbol not recognized:  ~A" str))
     ))


;; --------------------------------
;;   CHORD-NOTE-LIST-TO-CHORDNAME
;; --------------------------------
;;
;;;(defun chord-note-list-to-chordname (chord-note-list)
;;;  (let ((chordname (car chord-note-list))
;;;        (tonnrlist (qlist-to-tonnrlist chord-note-list))
;;;        (not-implemented? nil) )
;;;    (cond
;;;     ((equal tonnrlist '(0 4 7)))
;;;     ((equal tonnrlist '(0 3 7))
;;;      (setq chordname (concatenate 'string chordname "m")))
;;;     ((equal tonnrlist '(0 3 6))
;;;      (setq chordname (concatenate 'string chordname "dim")))
;;;     (t (setq not-implemented? t))
;;;     )
;;;    (if  not-implemented?
;;;         (progn
;;;           (warn "Chordlist ~A not translated to chord" chord-note-list)
;;;           nil)
;;;         chordname
;;;    )))
;;0009/af new syntax
(defun chord-note-list-to-chordname (chord-note-list)
  (let ((chordname (car chord-note-list))
        (tonnrlist (qlist-to-tonnrlist chord-note-list))
        (not-implemented? nil) )
    (cond
     ((equal tonnrlist '(0 4 7))
      (setq chordname (concatenate 'string chordname "maj")))
     ((equal tonnrlist '(0 3 7))
      (setq chordname (concatenate 'string chordname "min")))
     ((equal tonnrlist '(0 3 6))
      (setq chordname (concatenate 'string chordname "dim")))
     (t (setq not-implemented? t))
     )
    (if  not-implemented?
        (progn
          (warn "Chordlist ~A not translated to chord" chord-note-list)
          nil)
      chordname
      )))

;0009/af converts all chord lists to names in the active tracks
(defun convert-chord-list-to-chord-name ()
  (each-note-if
   (this 'q)
   (listp (this 'q))
   (then 
    (set-this 'q (chord-note-list-to-chordname (this 'q)))
    ))
   (redraw-music-windows))

;;
;; -----------------
;;   CONVERT-CHORD
;; -----------------
;;
;; convert from number notation to chord-note-list'
;;
(defun convert-chord ()
 (let ((last-key-nr)(minor?))
 (block convert-chord
 (each-note
  (if (first?)
   (if (and (listp (this 'q)) (tone-to-tonnr (car (this 'q))))
       (return-from convert-chord)
       (if (get-dm-var 'verbose-i/o) (format t "~&Chord conversion")) ))
  (if (this 'key) (setq last-key-nr (tone-to-tonnr (this 'key)))
                  (if (first?) (setq last-key-nr (ask-for-keynr))) )
  (if (numberp (this 'q))
    (let ((this-q (this 'q)))
      (if (minusp this-q)
          (setq minor? t)
          (setq minor? nil) )
      (setq this-q (abs this-q))
      (if minor?
          (case (mod (+ this-q last-key-nr) 12)
            (0 (set-this 'q '("C" "Eb" "G")))
            (1 (set-this 'q '("Db" "E" "Ab")))
            (2 (set-this 'q '("D" "F" "A")))
            (3 (set-this 'q '("Eb" "Gb" "Ab")))
            (4 (set-this 'q '("E" "G" "A")))
            (5 (set-this 'q '("F" "Ab" "C")))
            (6 (set-this 'q '("F#" "A" "C#")))
            (7 (set-this 'q '("G" "B" "D")))
            (8 (set-this 'q '("Ab" "H" "Eb")))
            (9 (set-this 'q '("A" "C" "E")))
            (10 (set-this 'q '("B" "Db" "F")))
            (11 (set-this 'q '("H" "D" "F#"))) )
          (case (mod (+ this-q last-key-nr) 12)
            (0 (set-this 'q '("C" "E" "G")))
            (1 (set-this 'q '("Db" "F" "Ab")))
            (2 (set-this 'q '("D" "F#" "A")))
            (3 (set-this 'q '("Eb" "G" "Ab")))
            (4 (set-this 'q '("E" "G#" "A")))
            (5 (set-this 'q '("F" "A" "C")))
            (6 (set-this 'q '("F#" "A#" "C#")))
            (7 (set-this 'q '("G" "H" "D")))
            (8 (set-this 'q '("Ab" "C" "Eb")))
            (9 (set-this 'q '("A" "C#" "E")))
            (10 (set-this 'q '("B" "D" "F")))
            (11 (set-this 'q '("H" "D#" "F#"))) )
)))))))      

;;
;; -----------------
;;   ASK-FOR-KEYNR
;; -----------------
;;
(defun ask-for-keynr ()
  (if (get-dm-var 'verbose-i/o) 
    (let ((keynr))
      (while (not keynr)
        (print "enter key (in quotes) ")
        (setq keynr (tone-to-tonnr (read))) )
      keynr )
    0))

;;
;; ---------------
;;   ASK-FOR-KEY
;; ---------------
;;
(defun ask-for-key ()
 (let ((key t))
  (while (and key (not (stringp key)))
   (print "enter key (in quotes) ")
        (setq key (read)))
  key ))



