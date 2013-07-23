;; Bisesi/Parncutt accent rules - Analysis of accent positions

;;130108/af started on metrical accent


(in-package :dm)



#|
(defun mark-metrical-accent ()
  (rem-all :beat)
  (rem-all 'accent-m-auto)
  
  (mark-beat)
  
  (each-note-if
   (this :beat)
   (this 'bar)
   (not (this 'rest))
   (then
    (set-this 'accent-m-auto 4) ))
  
  (each-note-if
   (this :beat)
   (not (this 'bar))
   (not (this 'rest))
   (then
    ;(set-this 'accent-m-auto 0)
    ))
  )
|#


;-------------- metrical accent ----------------------------------------

#|
;a short version setting accent-m directly
;didnt work with sync using new name
(defun mark-metrical-accent-test ()
  (rem-all :beat0)(rem-all :beat1)(rem-all :beat2)(rem-all :beat3)
  (rem-all 'accent-m)
  
  (mark-beat-levels)
  
  (each-note-if
   (not (this 'rest))
   (then
    (when (this :beat0) (set-this 'accent-m nil))
    (when (this :beat1) (set-this 'accent-m 3))
    (when (this :beat2) (set-this 'accent-m 6))
    (when (this :beat3) (set-this 'accent-m 9))
    )))
|#

;main function
; 4 different beat levels are defined
(defun mark-metrical-accent ()
  (rem-all :beat0)(rem-all :beat1)(rem-all :beat2)(rem-all :beat3)
  (rem-all :beat0sal)(rem-all :beat1sal)(rem-all :beat2sal)(rem-all :beat3sal)
  (rem-all :beat0dr)(rem-all :beat1dr)(rem-all :beat2dr)(rem-all :beat3dr)
  (rem-all 'accent-m)
  ;mark metrical levels
  (mark-beat-levels)
  ;default saliences
  (each-note-if
   (not (this 'rest))
   (then
    (when (this :beat0) (set-this :beat0sal 3))
    (when (this :beat1) (set-this :beat1sal 3))
    (when (this :beat2) (set-this :beat2sal 3))
    (when (this :beat3) (set-this :beat3sal 3)) ))
  ;compute pulse intervals
  (mark-beat-fraction-dr)
  ;gaussian scaling
  (scale-beat-salience)
  ;summarize into final accent salience
  (let ((beat0sal 0) (beat1sal 0) (beat2sal 0) (beat3sal 0))
    (each-note-if
     (not (this 'rest))
     (not (first?))  ;quickfix since it doesn't work to apply accent on the first note
     (then
      (setq beat0sal (or (this :beat0sal) 0))
      (setq beat1sal (or (this :beat1sal) 0))
      (setq beat2sal (or (this :beat2sal) 0))
      (setq beat3sal (or (this :beat3sal) 0))
      (set-this 'accent-m (+ beat0sal beat1sal beat2sal beat3sal))
      )))
  )

;mark 4 different metrical levels from notated meter
;beat0 sub-beat 
;beat1 the normal beat
;beat2 half bar or bar
;beat3 bar or 2 bars
(defun mark-beat-levels ()
  (let (beat0 beat1 beat2 beat3
              (ack-value 0.0) fractions)
    (each-track
     (setq ack-value 0.0)
     (each-note             ;mark beat
      (when (this 'meter)
        (setq fractions (get-beat-fractions (this 'meter)))
        (setq beat0 (first fractions))
        (setq beat1 (second fractions))
        (setq beat2 (third fractions))
        (setq beat3 (fourth fractions)) )
      ;(when (this 'bar) (setq ack-value 0))
      (when (zerop (mod ack-value beat0)) (set-this :beat0 t))
      (when (zerop (mod ack-value beat1)) (set-this :beat1 t))
      (when (zerop (mod ack-value beat2)) (set-this :beat2 t))
      (when (zerop (mod ack-value beat3)) (set-this :beat3 t))
      (incf ack-value (get-note-value-fraction *i*))
      ))))

;list of translations from meter to beat level fractions
(defun get-beat-fractions (meter)
  (let ((beat0 1/8) (beat1 1/4)(beat2 2/4)(beat3 4/4)) ;default 4/4
        (cond
         ((equal meter '(4 4)) (setq beat0 1/8 beat1 1/4 beat2 2/4 beat3 4/4))
         ((equal meter '(2 2)) (setq beat0 1/4 beat1 1/2 beat2 2/2 beat3 4/2))
         ((equal meter '(4 2)) (setq beat0 1/4 beat1 1/2 beat2 2/2 beat3 4/2))
         ((equal meter '(2 4)) (setq beat0 1/8 beat1 1/4 beat2 2/4 beat3 4/4))
         ((equal meter '(3 4)) (setq beat0 1/8 beat1 1/4 beat2 3/4 beat3 6/4))
         ((equal meter '(3 8)) (setq beat0 1/8 beat1 3/8 beat2 6/8 beat3 12/4))
         ((equal meter '(6 8)) (setq beat0 1/8 beat1 3/8 beat2 6/8 beat3 12/8))
         ((equal meter '(9 8)) (setq beat0 1/8 beat1 3/8 beat2 9/8 beat3 18/8))
         ((equal meter '(12 8))(setq beat0 1/8 beat1 3/8 beat2 6/8 beat3 12/8))
         (t (warn " get-beat-level-fractions, meter not defined: ~A ,using (4 4) instead" meter))
         )
        (list beat0 beat1 beat2 beat3)
        ))

;mark the duration in ms of each beat level
;only at meter mark
(defun mark-beat-fraction-dr ()
  (let (beat-value factor fractions)
    (each-note
       (when (this 'meter)
           (setq beat-value (cadr (this 'meter))) )
       (when (this 'mm)
           (setq factor (/ (* beat-value 60000.0) (this 'mm))) )
       (when (this 'meter)
           (setq fractions (get-beat-fractions (this 'meter)))
           ;(print-ll " fractions " fractions)
           (set-this :beat0dr (* factor (first fractions)))
           (set-this :beat1dr (* factor (second fractions)))
           (set-this :beat2dr (* factor (third fractions)))
           (set-this :beat3dr (* factor (fourth fractions)))
           )
     )))

(defun scale-beat-salience ()
  (let ((m 1.0) (s 1.65) ; mean and SD for gauss distribution
        beat0dr beat1dr beat2dr beat3dr )
    (each-note
     (when (this :beat0dr) (setq beat0dr (/ (this :beat0dr) 1000.0)))
     (when (this :beat1dr) (setq beat1dr (/ (this :beat1dr) 1000.0)))
     (when (this :beat2dr) (setq beat2dr (/ (this :beat2dr) 1000.0)))
     (when (this :beat3dr) (setq beat3dr (/ (this :beat3dr) 1000.0)))
     
     (when (this :beat0sal) 
       (set-this :beat0sal (* (this :beat0sal) (log-gauss-trans beat0dr m s)))
       ;(print-ll "log-gauss-trans " (log-gauss-trans beat0dr m s))
       )
     (when (this :beat1sal) (set-this :beat1sal (* (this :beat1sal) (log-gauss-trans beat1dr m s))))
     (when (this :beat2sal) (set-this :beat2sal (* (this :beat2sal) (log-gauss-trans beat2dr m s))))
     (when (this :beat3sal) (set-this :beat3sal (* (this :beat3sal) (log-gauss-trans beat3dr m s))))
     )))

(defun log-gauss-trans (x m s)
  (let ((xlog (log x))
        (mlog (log m))
        (slog (log s)) )
    (exp (* -0.5 (expt (/ (- xlog mlog) slog) 2)))
    ))


;------------- melodic accent -----------------------------

;main function
(defun mark-melodic-accent ()
  (rem-all 'accent-c)
  ;mark metrical levels
  (mark-mel-peak-and-valley)
  (mark-mel-salience-dev-from-mean)
  (mark-mel-salience-prev-interval)
  (mark-mel-final-combination)
  )

(defun mark-mel-peak-and-valley ()
  (rem-all :peak)(rem-all :valley)
  (each-note-if
   (not (first?))
   (not (last?))   
   (not (prev 'rest))
   (not (this 'rest))
   (not (next 'rest))
   (> (this-f0) (prev-f0))
   (>= (this-f0) (next-f0))
   (then
    (set-this :peak t) ))
  (each-note-if
   (not (first?))
   (not (last?))   
   (not (prev 'rest))
   (not (this 'rest))
   (not (next 'rest))
   (< (this-f0) (prev-f0))
   (<= (this-f0) (next-f0))
   (then
    (set-this :valley t) )) )

;; mark on all notes except repeats
(defun mark-mel-peak-and-valley ()
  (rem-all :peak)(rem-all :valley)
  (each-note-if
   (not (first?))
   (not (last?))   
   (not (prev 'rest))
   (not (this 'rest))
   ;(not (next 'rest))
   (> (this-f0) (prev-f0))
   ;(>= (this-f0) (next-f0))
   (then
    (set-this :peak t) ))
  (each-note-if
   (not (first?))
   (not (last?))   
   (not (prev 'rest))
   (not (this 'rest))
   ;(not (next 'rest))
   (< (this-f0) (prev-f0))
   ;(<= (this-f0) (next-f0))
   (then
    (set-this :valley t) )) )

(defun mark-mel-salience-dev-from-mean ()
  (rem-all :melsal1)
  (each-track
   ;compute mean f0 for the track
   (let ((mean-f0 0) (n-f0 0))
     (each-note-if
      (this 'f0)
      (incf mean-f0 (this-f0))
      (incf n-f0) )
     (setq mean-f0 (float (/ mean-f0 n-f0)))
     (print-ll "mean f0 of this track = " mean-f0) 
     (each-note-if
      (this :peak)
      (then
       (set-this :melsal1 (float (/ (max 0 (- (this-f0) mean-f0)) 4))) ))
     (each-note-if
      (this :valley)
      (then
       (set-this :melsal1 (float (/ (max 0 (- mean-f0 (this-f0))) 12))) ))
     )))

(defun mark-mel-salience-prev-interval ()
  (rem-all :melsal2)
  (each-track
   ;compute mean f0 for the track
   (each-note-if
    (this :peak)
    ;(> (abs (- (this-f0) (prev-f0))) 2)
    (then
     (set-this :melsal2 (float (/ (abs (- (this-f0) (prev-f0))) 4))) ))
   (each-note-if
    (this :valley)
   ; (> (abs (- (this-f0) (prev-f0))) 2)
    (then
     (set-this :melsal2 (float (/ (abs (- (this-f0) (prev-f0))) 12))) ))
   ))

#|
(defun mark-mel-final-combination ()
  (each-note-if
   (or (this :peak) (this :valley))
   (then
    (set-this 'accent-c (min 5 (round (* 1.5 (+ (this :melsal1) (this :melsal2))))))
    )))
|#

(defun mark-mel-final-combination ()
  (each-note-if
   (or (this :peak) (this :valley))
   (then
    (let ((sal (min 5 (round (* 1.5 (* (this :melsal1) (this :melsal2)))))))
    (if (> sal 0) (set-this 'accent-c sal))
      ))))

;;--- new melodic accent from Richard mail 130513------

;;;1.       No need to mark peaks and valleys. Start by treating all notes equal. We call that musical democracy;-)

;;;2.       :melsal1 is merely the distance of each tone from the mean pitch in semitones. Always positive of course, 
;;;         to do this you will take the "absolute value" of the difference. (Maybe we will have to divide this value by 2 
;;;          for pitches below the mean but we can experiment with that later, first I would leave it out and see what happens.)

;;;3.       :melsal2 is the size of the preceding leap in semitones. Again, the absolute value. 
;;;         (again, we might later decide to divide by two for falling intervals. But not yet)

;;;4.       Just multiply :melsal1 and :melsal2 together. The result is the melodic accent. Every tone in the whole melody gets
;;;         a value.

;;;5.       Maybe we should subtract a threshold, and if the result is negative just make it zero. 
;;;         But I would like to see how steps 1-4 work without doing that. Might be an unnecessary complexity.

#|
(defun mark-melodic-accent-2 ()
  (rem-all 'accent-c)
  (rem-all :melsal1)
  (rem-all :melsal2)
  (rem-all :melsal3)
  (each-track
   ;compute mean f0 for the track
   (let ((mean-f0 0) (n-f0 0))
     (each-note-if
      (this 'f0)
      (incf mean-f0 (this-f0))
      (incf n-f0) )
     (setq mean-f0 (float (/ mean-f0 n-f0)))
     (print-ll "mean f0 of this track = " mean-f0) 
     (each-note-if
      (not (first?))
      (not (this 'rest))
      (not (prev 'rest))
      (then
       (set-this :melsal1 (abs (- (this-f0) mean-f0)))
       (set-this :melsal2 (abs (- (this-f0) (prev-f0))))
       (set-this :melsal3 (* (this :melsal1) (this :melsal2)))
       (let ((salience (round (/ (this :melsal3) 15.0))))
         (if (> salience 0) (set-this 'accent-c salience)) )
       )))))
|#

(defun mark-melodic-accent-2 ()
  (rem-all 'accent-c)
  (rem-all :melsal1)
  (rem-all :melsal2)
  (rem-all :melsal3)
  (each-track
   ;compute mean f0 for the track
   (let ((mean-f0 0) (n-f0 0))
     (each-note-if
      (this 'f0)
      (incf mean-f0 (this-f0))
      (incf n-f0) )
     (setq mean-f0 (float (/ mean-f0 n-f0)))
     (print-ll "mean f0 of this track = " mean-f0) 
     (each-note-if
      (not (first?))
      (not (this 'rest))
      (not (prev 'rest))
      (then
       (let ((f0-dist-mean (- (this-f0) mean-f0))
             (f0-int (- (this-f0) (prev-f0))) )
         (if (> f0-dist-mean 0) (set-this :melsal1 f0-dist-mean))      ;above mean
         (if (<= f0-dist-mean 0) (set-this :melsal1 (abs (* f0-dist-mean 0.7)))) ;below mean
         (if (> f0-int 0) (set-this :melsal2 f0-int))                  ;rising interval
         (if (<= f0-int 0) (set-this :melsal2 (abs (* f0-int 0.7)))) ) ;falling interval
         (set-this :melsal3 (* (this :melsal1) (this :melsal2)))
       (let ((salience (round (/ (this :melsal3) 15.0))))
         (if (> salience 0) (set-this 'accent-c salience)) )
       )))))


;------------- harmonic accent -----------------------------

(defun mark-harmonic-accent ()
  (if (or (not (get-first 'q)) (not (get-first 'key))) 
    (Print-ll "mark-harm-accent : no chord or key on first note - skipping rule")
    (progn 
      (rem-all 'accent-h)
      (accent-harm-charge)
      )))

(defun accent-harm-charge ()
  (let ((key)(sal)(quant 1))
    (each-note 
     (if (this 'key) (setq key (this 'key)))
     (when (and (this 'q) (not (this 'rest)))   ; not on rests
       (setq sal (* quant 1.5 (sqrt (harmonic-charge-fn (this 'q) key nil))))
       (if (> sal 0) (set-this 'accent-h sal))
       ))))
  
;-------------- batch processing for testing --------------

(setq *batch-rules*
      '((mark-metrical-accent)
        (METRICAL-ACCENT 1.5 :CURVE :QUADRATIC :amp 0.25)
        ))

(setq *batch-rules*
      '((mark-melodic-accent)
        (melodic-contour-ACCENT 1 :CURVE :QUADRATIC :width 2)
        ))
(setq *batch-rules*
      '((mark-harmonic-accent)
        (harmonic-accent 1 :CURVE :QUADRATIC :width 2)
        ))

(defun run-accent-batch ()
  (let ((dir (ask-user-for-directory)) (comp 1))
    (when dir
      (let ((dir-list (directory dir)))
        ;(pprint dir-list)
        (dolist (fpath dir-list)
          ;(print-ll "fpath = " fpath)
          (when (string-equal (pathname-type fpath) "mus")
            (print-ll "   fpath = " fpath)
            (load-score-fpath fpath)
            (each-track (setf (synth *this-track*) (make-synth "Roland-1010")) ) ; set synth
            (rule-interaction-apply-rules-sync *batch-rules* comp)
            (save-performance-fpath (merge-pathnames (make-pathname :type "per") fpath))
            (save-performance-midifile1-fpath (merge-pathnames (make-pathname :type "mid") fpath))
            ))))))