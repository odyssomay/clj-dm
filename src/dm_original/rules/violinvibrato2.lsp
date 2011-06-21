
(in-package :dm)
;(> (this 'dr) 333) ;longer than two vib cycles at 6Hz

;;------------ Set global variables -------------------

(defvar *epsilon*)
(setq *epsilon* 1)


;;----------- Main functions---------------

(defun violin-vibrato-init (quant &key (max-delay 600) (max-delay-ioi 1500) (ioi-threshold 350) (attack 350) (release 300))
  ;;Clean up score
  (violin-vibrato-clean-up-score-vars)
  
  ;;Global variables (user defined)
  (defvar *vib-ioi-threshold*) (setq *vib-ioi-threshold* ioi-threshold)
  (defvar *vib-attack-time*) (setq *vib-attack-time* attack)
  (defvar *vib-release-time*) (setq *vib-release-time* release)

  (mark-melodic-charge)
  (mark-metric-level)

  (violin-vibrato-category)
  (violin-vibrato-set-delay :max-delay max-delay :max-ioi max-delay-ioi)
  )

(defun violin-vibrato-extent (quant &key (ext 20) (ext2sl 1) (ext2vol 1) (ext2mc 1) (ext2ml 0) (ext-offset 0.33))
  (violin-vibrato-set-note-extent
   :base-extent ext :sl-coefficient ext2sl :mel-charge-coefficient ext2mc
   :metric-coefficient ext2ml)
  (violin-vibrato-set-extent-edges ext2vol)
  (violin-vibrato-make-extent-env quant :extent-offset-factor ext-offset)
  )

(defun violin-vibrato-rate (quant &key (rate 6) (tail% 15) (tail-cycles 3) (rate2ext 0.1) (rate2mc 0) (rate2ml 0))
  ;;Calculate mean extent. Needed for centering rate as function of extent.
  (setq cumsum 0)
  (setq counter 0)
  (each-note-if
   (this 'vib-note-extent)
   (setq cumsum (+ cumsum (this 'vib-note-extent)))
   (setq counter (+ counter 1)))
  (setq mean-extent (/ cumsum counter))
  
  (violin-vibrato-set-note-rate
   :base-rate rate :mean-extent mean-extent :extent-coefficient rate2ext
   :mel-charge-coefficient rate2mc :metric-coefficient rate2ml)
  (violin-vibrato-set-rate-edges :tail tail%)
  (violin-vibrato-make-rate-env quant :tail-cycles tail-cycles)
  
  ;(violin-vibrato-clean-up-score-vars)
  )

(defun violin-vibrato-clean-up-score-vars () ;;(&bool (flag-sl-slope))
  ;;Clean up vibrato score variables
  (rem-all 'vib-on)
  (rem-all 'vib-short)
  (rem-all 'vib-delay)
  (rem-all 'vib-ext-1)
  (rem-all 'vib-ext-2)
  (rem-all 'vib-note-extent)
  (rem-all 'vib-rate-1)
  (rem-all 'vib-rate-2)
  (rem-all 'vib-note-rate)
  (rem-all 'vib-tail-time)
  ;(if (flag-sl-slope) rem-all 'sl-slope)
  )


;;---------vibrato on/off & delay----------------------

(defun violin-vibrato-category ()
  ;;vibrato note length categories
    (each-note-if
     (> (this 'dr) *vib-ioi-threshold*)
     (not (this 'rest))
     (then
      (set-this 'vib-on t)
      ;;vibrato short decision here
      )))

;;set delay as function of IOI  
(defun violin-vibrato-set-delay (&key (max-delay 600) (max-ioi 1500))
  (each-note-if
   (this 'vib-on)
   (then
    (cond 
     ((> (this 'dr) max-ioi) (set-this 'vib-delay max-delay))
     (t (set-this 'vib-delay (linear-interpolation (this 'dr) *vib-ioi-threshold* 0 max-ioi max-delay)))
     (when (< (this 'dr) (+ (this 'vib-delay) *vib-attack-time* *vib-release-time*))
       (set-this 'vib-delay 0)
       (when (< (this 'dr) (+ *vib-attack-time* *vib-release-time*)) ;;Mark short notes
         (set-this 'vib-short t)
         ))))))


;;-------------Vibrato extent-------------------------

;;set mean vibrato extent for each note as function of sound level and musical structure
(defun violin-vibrato-set-note-extent 
    (&key (base-extent 20) (sl-coefficient 1) (mel-charge-coefficient 0) (metric-coefficient 0))
  (each-note-if
   (this 'vib-on)
   (then
    (let ((vib-note-extent (+ base-extent 
                              (* sl-coefficient (this 'sl))  ;sound level
                              (* mel-charge-coefficient (if (this :mc) (- (abs (this :mc)) 3.25) 0)) 
                              (* metric-coefficient (if (this :metric-level) (- (this :metric-level) 1))) ;;dependence on metrical stress
                              )))
      (set-this 'vib-note-extent (round vib-note-extent))
    ))))


;;calculate vib extent at start and end of note --> extent slope
(defun violin-vibrato-set-extent-edges (quant)
  (each-note-if
   (this 'vib-on)
   (then                         ;;sustain-interval=(- (this 'dr) (this 'vib-delay) *vib-attack-time* *vib-release-time*)
    (cond
     ((this 'vib-short)  ;;old--> ((or (this 'vib-short) (this 'phrase-end)) 
      (set-this 'vib-ext-1 (this 'vib-note-extent))
      (set-this 'vib-ext-2 (this 'vib-note-extent)))
     ;;Set slope for sustain part in extent
     (t (set-this 'vib-ext-1 (- (this 'vib-note-extent) (* 0.5 quant (this 'sl-slope) (- (this 'dr) (this 'vib-delay) *vib-attack-time* *vib-release-time*))))
        (set-this 'vib-ext-2 (+ (this 'vib-note-extent) (* 0.5 quant (this 'sl-slope) (- (this 'dr) (this 'vib-delay) *vib-attack-time* *vib-release-time*)))))
     ))))

;;generate envelopes for vibrato extent from three values in the score
(defun violin-vibrato-make-extent-env (quant &key (extent-offset-factor 0.33))
        (each-note-if
         (this 'vib-on)
         (then
          (cond
           ((this 'vib-short)
            (setq attack-time (/ (this 'dr) 2.0))
            (setq release-time (/ (this 'dr) 2.0)))
           (t (setq attack-time *vib-attack-time*)
              (setq release-time *vib-release-time*)))
          (set-this 'va (this-note-make-time-shape :interpolation :linear))
          (set-y-max (this 'va) 100)
          (set-y-min (this 'va) 0)
          ;;Insert breakpoints
          (cond
           ((> (this 'vib-delay) 0)
            (insert-break-point (this 'va) 0 0)
            (insert-break-point (this 'va) (this 'vib-delay) 0)
            (insert-break-point (this 'va) (+ (this 'vib-delay) *epsilon*) (* quant (this 'vib-ext-1) extent-offset-factor)))
           (t (insert-break-point (this 'va) 0 (* quant (this 'vib-ext-1) extent-offset-factor)))  ;;else statement
           )
          (insert-break-point (this 'va) (+ (this 'vib-delay) attack-time) (* quant (this 'vib-ext-1)))
          (if (not (this 'vib-short))
              (insert-break-point (this 'va) (- (this 'dr) release-time) (* quant (this 'vib-ext-2)))
            )
          (insert-break-point (this 'va) (this 'dr) (* quant (this 'vib-ext-2) extent-offset-factor))
          (if (not (last?)) (set-next 'va 0))
          )))


;;--------------Vibrato rate-------------------------------

;;set mean vibrato extent for each note as function of sound level and musical structure
(defun violin-vibrato-set-note-rate 
    (&key (base-rate 6) (mean-extent 15) (extent-coefficient 0.1) (mel-charge-coefficient 0) (metric-coefficient 0))
  (each-note-if
   (this 'vib-on)
   (then
    (let ((vib-note-rate (+ base-rate 
                          (* extent-coefficient (- (this 'vib-note-extent) mean-extent))  ;dependence on extent
                          (* mel-charge-coefficient (if (this :mc) (- (abs (this :mc)) 3.25) 0)) ;;dependence on mel charge
                          (* metric-coefficient (if (this :metric-level) (- (this :metric-level) 1) 0)) ;;dependence on metrical stress
                       )))
      (set-this 'vib-note-rate (/ (round (* 10 vib-note-rate)) 10)) ;;afronden op decimalen
      ))))

(defun violin-vibrato-set-rate-edges (&key (tail 15))
  (each-note-if
   (this 'vib-on)
   (then
    (set-this 'vib-rate-1 (this 'vib-note-rate))
    (set-this 'vib-rate-2 (* (+ 1 (/ tail 100)) (this 'vib-note-rate))) ;;Tail is increase of rate in % in the last vibrato cycles
    )))

;;generate envelopes for vibrato rate from two values in the score
(defun violin-vibrato-make-rate-env (quant &key (tail-cycles 3))
  (let ()
    (each-note-if
     (this 'vib-on)
     (then
      (set-this 'vf (this-note-make-time-shape :interpolation :linear))
      (set-y-max (this 'vf) 12)
      (set-y-min (this 'vf) 4)
      (set-this 'vib-tail-time (* (/ 1000 (this 'vib-rate-1)) tail-cycles)) ;;Calculates vibrato tail in milliseconds
      ;;Insert breakpoints
      (insert-break-point (this 'vf) 0 (* quant (this 'vib-rate-1)))
      (insert-break-point (this 'vf) (this 'dr) (* quant (this 'vib-rate-2)))
      (when (> (this 'dr) (this 'vib-tail-time))
        (insert-break-point (this 'vf) (- (this 'dr) (this 'vib-tail-time)) (* quant (this 'vib-rate-1)))
        )
      ))))


;;------------volume shapes---------------------------

;; Measurements: (short-slope 0.0286) (long-slope 0.00533)
(defun violin-volume-shape (quant &key (sl2vol 1) (short-slope 0.01) (long-slope 0.006))
  (rem-all 'sl-slope)
  
  (sl-to-vol-smoothing3 sl2vol)
  (within-note-volume-shape quant :short-slope short-slope :long-slope long-slope)
  )
                            

;; Uses a 3-point method to calculate the volume slope (edges are calculated using a 2-point extrapolation method)
#|
(defun sl-to-vol-smoothing3 (quant)
   (let ((max-sl 0))
      (each-note-if         ;get max sl
        (this 'sl)
        (then
         (if (> (this 'sl) max-sl) (setq max-sl (this 'sl)))
         ))
      (each-note-if         ;transfer sl to volume
        (this 'sl)
        (then
         (set-this :volume (- (this 'sl) (+ max-sl 5) )) ;;Normalization (max level: -5 dB)
         ))
      (each-note-if         ;smoothing
        (this :volume)
        (then
         (let ((env (this-note-make-time-shape :interpolation :linear)))
            (set-y-max env 25)
            (set-y-min env -64)
           (cond
            ((or (first?) (this 'phrase-start))
             (set-this 'sl-slope (/ (- (next :volume) (this :volume)) (* 0.5 (+ (this 'dr) (next 'dr))))))
            ((or (last?) (this 'phrase-end))
             (set-this 'sl-slope (/ (- (this :volume) (prev :volume)) (* 0.5 (+ (this 'dr) (prev 'dr))))))
            (t (set-this 'sl-slope (/ (- (next :volume) (prev :volume)) (+ (this 'dr) (* 0.5 (+ (prev 'dr) (next 'dr)))))))
            ) ;; end cond
           (insert-break-point env 0 (- (this :volume) (* 0.5 quant (this 'sl-slope) (this 'dr))))
           (insert-break-point env (this 'dr)
                               (+ (this :volume) (* 0.5 quant (this 'sl-slope) (this 'dr))))           
           (set-this 'vol env)
           )))
     (rem-all :volume)
     ))
|#
;with checking of notes next to rests
;090421/af fixed a bug at end of phrases
(defun sl-to-vol-smoothing3 (quant)
   (let ((max-sl 0))
      (each-note-if         ;get max sl
        (this 'sl)
        (then
         (if (> (this 'sl) max-sl) (setq max-sl (this 'sl)))
         ))
      (each-note-if         ;transfer sl to volume
        (this 'sl)
        (then
         (set-this :volume (- (this 'sl) (+ max-sl 5) )) ;;Normalization (max level: -5 dB)
         ))
      (each-note-if         ;smoothing
        (this :volume)
        (then
         (let ((env (this-note-make-time-shape :interpolation :linear)))
            (set-y-max env 25)
            (set-y-min env -64)
           (cond
            ((and (not (first?)) (not (last?)) (prev 'rest) (next 'rest)) ;isolated tone
             (set-this 'sl-slope 0))
            
            ((and (or (first?) (this 'phrase-start) (prev 'rest))
                  (next :volume) )
             ;(print-ll "next volume  , *track* " *this-track*  " *i* " *i* " note " (this 'n))
             ;(if (not (next :volume)) (print-ll "no next volume  , *track* " *this-track*  " *i* " *i* " note " (this 'n)))
             (set-this 'sl-slope (/ (- (next :volume) (this :volume)) (* 0.5 (+ (this 'dr) (next 'dr))))))
            
            ((and (or (last?) (this 'phrase-end) (next 'rest))
                  (prev :volume) )
             ;(if (not (prev :volume)) (print-ll "no prev volume  , *track* " *this-track*  " *i* " *i* " note " (this 'n)))
             (set-this 'sl-slope (/ (- (this :volume) (prev :volume)) (* 0.5 (+ (this 'dr) (prev 'dr))))))
            
            (t (set-this 'sl-slope (/ (- (next :volume) (prev :volume)) (+ (this 'dr) (* 0.5 (+ (prev 'dr) (next 'dr)))))))
            ) ;; end cond
           (insert-break-point env 0 (- (this :volume) (* 0.5 quant (this 'sl-slope) (this 'dr))))
           (insert-break-point env (this 'dr)
                               (+ (this :volume) (* 0.5 quant (this 'sl-slope) (this 'dr))))           
           (set-this 'vol env)
           )))
     (rem-all :volume)
     ))


;;generates within-note volume shapes. Rise in volume untill vibrato delay time, then a decrease
(defun within-note-volume-shape (quant &key (short-slope 0.0286) (long-slope 0.00533))
  (each-note-if
   (not (this 'rest))
   (then
    (add-bp-shape
     (this 'vol)
     (this-note-make-time-shape
      :interpolation :linear
      :y-max 25
      :y-min 0
      :break-point-list
      (cond 
       ((< (this 'dr) *vib-ioi-threshold*)
        (list
         0 0
         (* (this 'dr) 0.8) (* quant short-slope (* (this 'dr) 0.8))
         (this 'dr) 0))
       ((> (this 'vib-delay) 0)
        (list
         0 0
         (this 'vib-delay) (* quant long-slope (this 'vib-delay))
         (this 'dr) 0)))
      )))
   ))
  
     

;;------------utilities---------------------------

;;Meter 4/4 for now
(defun mark-metric-level ()
  (if (not (get-first 'meter))
      (print "MARK-METRIC-LEVEL: no metric level marked - missing meter in the first note")
    (let ((beat-value 1/4)
        (measure-value 1)
        (ack-value 0)
        )
    (each-track
     (setq ack-value 0)
     (each-note
      ;(print ack-value)
      (when (this 'meter) 
        (setq beat-value (/ 1 (cadr (this 'meter))))
        (setq measure-value (/ (car (this 'meter)) (cadr (this 'meter)))))
      (when (not (this 'rest))
        (cond
         ;measure 
         ((zerop (mod ack-value measure-value))
          (set-this :metric-level 4) )
         ((zerop (mod ack-value (/ measure-value 2)))
          (set-this :metric-level 2) )
         ;beat
         ((zerop (mod ack-value beat-value))
          (set-this :metric-level 1) )
         ((zerop (mod ack-value (/ beat-value 2)))
          (set-this :metric-level 0.5) )
         ((zerop (mod ack-value (/ beat-value 3)))
          (set-this :metric-level 1/3) )
         ((zerop (mod ack-value (/ beat-value 4)))
          (set-this :metric-level 0.25) )
         ((zerop (mod ack-value (/ beat-value 6)))
          (set-this :metric-level 1/6) )
         ((zerop (mod ack-value (/ beat-value 8)))
          (set-this :metric-level 1/8) )
         ((zerop (mod ack-value (/ beat-value 12)))
          (set-this :metric-level 1/12) )
         ((zerop (mod ack-value (/ beat-value 16)))
          (set-this :metric-level 1/16) )
         )
        (set-this :note-value-fraction (get-note-value-fraction *i*))
        )
      
      (incf ack-value (get-note-value-fraction *i*))
      )))))

    
