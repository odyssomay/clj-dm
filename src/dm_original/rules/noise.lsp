;;Random variations in IOI and sound level
;;Models variations found in the litterature from the internal 'clock' and from motor control.
;;Developed by Friberg, Bresin and Juslin starting in 1999


(in-package :dm)


;;----------------
;;toplevel function
;;----------------

(defun noise (quant &key (amp 1)(dur 1))
  (when (not (zerop (* dur quant)))
    (internal-clock-noise (* dur quant))
    (motor-noise (* dur quant)) )
  (when (not (zerop (* amp quant)))
    (motor-noise-amp (* amp quant)) )
  (when (not (get-dm-var 'rule-debug-info)) (rem-all 'ddr)(rem-all 'dddr))
  )


;;----------------
;;internal clock
;;----------------

(defun internal-clock-noise (quant)
   (let (ms-range)
      (each-note
       (cond
        ((> (this 'dr) 1300) ;y = 0.0975x - 75.976
         (setq ms-range
           (round (* 4 quant (- (* 0.0975 (this 'dr)) 75.976 )))))
        (t  ;(> (this 'dr) 250), y = 0.032x + 6.7398
         (setq ms-range
               (round (* 4 quant (+ (* 0.032 (this 'dr)) 6.7398 ))))
         ;(if (first?) (print ms-range))
         )
        )
       (set-this 'ddr (- (random ms-range) (/ ms-range 2.0)))
       )
      (one-over-f-filter 'ddr 'dddr)
      (each-note
        (add-this 'dr (* 0.1 (noise-total-lin-scaling (this 'dr) (this 'dddr)))) )
     )
  (when (not (get-dm-var 'rule-debug-info)) (rem-all 'ddr)(rem-all 'dddr))
  )

(defun one-over-f-filter (invar outvar)
  (let ((b0 0)(b1 0)(b2 0)(b3 0)(b4 0)(b5 0)(b6 0))
    (each-note-if 
      (this invar)
      (let ((white (this invar)))
        (setq b0 (infix (0.99886 * b0 + white * 0.0555179))) 
        (setq b1 (infix (0.99332 * b1 + white * 0.0750759))) 
        (setq b2 (infix (0.96900 * b2 + white * 0.1538520))) 
        (setq b3 (infix (0.86650 * b3 + white * 0.3104856))) 
        (setq b4 (infix (0.55000 * b4 + white * 0.5329522))) 
        (setq b5 (infix (-0.7616 * b5 - white * 0.0168980)))
        (set-this outvar (infix (b0 + b1 + b2 + b3 + b4 + b5 + b6 + white * 0.5362)))
        (setq b6 (infix (white * 0.115926)))
        ))))

(defun noise-total-lin-scaling (dr ddr)
  (* (+ (* 0.00143 dr) 0.57) ddr) )

#| ;;test of algorithm -working!!
(defun one-over-f-filter ()
  (let ((b0 0)(b1 0)(b2 0)(b3 0)(b4 0)(b5 0)(b6 0)
        (fpath (show-dialog-for-saving-files-PD "save pink sequence")) )
    (if fpath
      (with-open-file (ofile fpath :direction :output
                               :if-does-not-exist :create
                               :if-exists :supersede)
        (dotimes  (i 1000000)
          (let ((white (- (random 5000) 2500)))
            (setq b0 (infix (0.99886 * b0 + white * 0.0555179))) 
            (setq b1 (infix (0.99332 * b1 + white * 0.0750759))) 
            (setq b2 (infix (0.96900 * b2 + white * 0.1538520))) 
            (setq b3 (infix (0.86650 * b3 + white * 0.3104856))) 
            (setq b4 (infix (0.55000 * b4 + white * 0.5329522))) 
            (setq b5 (infix (-0.7616 * b5 - white * 0.0168980)))
            (print (round (infix (b0 + b1 + b2 + b3 + b4 + b5 + b6 + white * 0.5362))) 
|#       
    

;;----------------
;; motor noise
;;----------------
           
;;changed to onset position deviations
;;generates sort of high pass spectrum
;;range modeled so that rms of clock noise divided by rms motor noise = 0.57
;;found in Gilden et al for IOI=300
;;010620/af lin-scaling removed - probably a mistake -could not hear any difference on the bellman
;; the value at 300 ms IOI is the same, which corresponded to the shortest notes in bellman
(defun motor-noise (quant)
   (let ((range-ms (* 6.78 quant))
         (ddr 0.0))
   (each-note-if
    (not (first?))
    (then
     (setq ddr
           (* range-ms (/ (* 0.33 (+ (- (random 201) 100)
                                     (- (random 201) 100)
                                     (- (random 201) 100) ))
                          33.0)))
     ;(setq ddr (noise-total-lin-scaling (this 'dr) ddr))
     (add-prev 'dr (- ddr))
     (add-this 'dr ddr)
     )))
  (when (not (get-dm-var 'rule-debug-info)) (rem-all 'ddr))
  )



;;----------------
;; motor noise sound level
;;----------------

(defun motor-noise-amp (quant)
   (let ((range-sl (* 1 quant)))
      (each-note-if
       (not (this 'rest))
       (then
        (add-this 'sl
          (* range-sl (/ (* 0.33 (+ (- (random 201) 100)
                                    (- (random 201) 100)
                                    (- (random 201) 100) ))
                         33.0))
          )))))


;;------------------------
;; old stuff
;;------------------------

#|
;;constant white noise in ms
;;range from guy (2.8% at 300 ms)
;;and portion of white and 1/f noise from science article
(defun make-motor-noise (quant)
   (let ((range-ms (* 5.35 quant)))
   (each-note
    (add-this 'dr
      (* range-ms (/ (* 0.33 (+ (- (random 201) 100)
                                (- (random 201) 100)
                                (- (random 201) 100) ))
                     33.0))
      ))))

(defun make-motor-noise (quant)
   (let ((range-ms (* 5.35 quant)))
   (each-note
    (set-this 'ddr
      (* range-ms (/ (* 0.33 (+ (- (random 201) 100)
                                (- (random 201) 100)
                                (- (random 201) 100) ))
                     33.0))
      )
    (add-this 'dr (this 'ddr))
)))
|#

#|
;parabolic curve value on each note
;with 1/f filter
(defun make-internal-clock-noise (quant)
   (let ((ms-range)(sl-range))
      (each-note
       (set-this 'ddr 0) )
      (each-note
       (setq ms-range (round (* 4 quant (+ (* 3E-5 (expt (this 'dr) 2))
                                           (- (* 0.0015 (this 'dr)))
                                           14.858 ))))
       (if (first?) (set-this 'dddr 0))
       (if (not (last?))
          (then
           (let ((rand-ms (- (random ms-range) (/ ms-range 2.0))))
           ;(let ((rand-ms (random ms-range)))
              ;(print rand-ms)
              (add-this 'ddr rand-ms)
              ;(add-next 'ddr (- rand-ms))
              )))
       )
      (each-note-if
       (not (first?))
       (this 'ddr)
       (prev 'ddr)
       (then
        ;(set-this 'dddr (this 'ddr))
        (set-this 'dddr (+ (* 0.01 (- (this 'ddr) (prev 'dddr))) (prev 'dddr))) 
        ;empirically derived const = 0.58 (1.74) so that sd=3.05 at 300 ms IOI
        ;and filter param above = 0.2 (0.05)
        (add-this 'dr (* 4 (this 'dddr)))         ))
      ))  

;linear curve segments on each note
;with 1/f filter
(defun make-internal-clock-noise (quant)
   (let ((ms-range)(sl-range))
      (each-note
       (set-this 'ddr 0) )
      (each-note
       (cond
        ((> (this 'dr) 1300) ;y = 0.0975x - 75.976
         (setq ms-range
           (round (* 4 quant (- (* 0.0975 (this 'dr)) 75.976 )))))
        (t  ;(> (this 'dr) 250)
         (setq ms-range ;;;y = 0.032x + 6.7398
           (round (* 4 quant (+ (* 0.032 (this 'dr)) 6.7398 )))))
                 ;(t
                 ; (setq ms-range (round (* 4 quant 14.74))) )
        )
       (if (first?) (set-this 'dddr 0))
       (if (not (last?))
          (then
           (let ((rand-ms (- (random ms-range) (/ ms-range 2.0))))
              ;(let ((rand-ms (random ms-range)))
              ;(print rand-ms)
              (add-this 'ddr rand-ms)
              ;(add-next 'ddr (- rand-ms))
              )))
       )
      (each-note-if
       (not (first?))
       (this 'ddr)
       (prev 'ddr)
       (then
        ;(set-this 'dddr (this 'ddr))
        ; filter param below = 0.2 (0.05), 0.01
        (set-this 'dddr (+ (* 0.1 (- (this 'ddr) (prev 'dddr))) (prev 'dddr))) 
        ;empirically derived const = 0.58 (1.74) so that sd=3.05 at 300 ms IOI
        (add-this 'dr (* 1.25 (this 'dddr)))         ))
      ))  
|#
#|

;;modeling random variations in IOI and sound level
;;quant=1 corresponds to +-2.5% IOI duration change with respect to the beat
;; and to +-1 dB in sound level
(defun make-jitter (quant)
   (let ((beat-dur)(ms-range)(sl-range))
      (each-note
       (set-this 'ddr 0) )
      (each-note
       (if (this 'mm)
          (then 
            (setq beat-dur (/ 60000.0 (this 'mm)))
            ;(setq ms-range (round (* 0.05 quant beat-dur)))
            (setq ms-range (round (* quant 710.67 (exp  (this 'mm) -0.6906))))
            (setq sl-range (round (* 10 quant)))
            ))
       ;(print-ll "beat-dur:" beat-dur " ms-range:" ms-range)
       (if (and                ;duration
            (not (this 'rest))
            (not (last?)) )
          (then
           (let ((rand-ms (- (random ms-range) (/ ms-range 2.0))))
              ;(print rand-ms)
              (add-this 'dr rand-ms)
              (add-next 'dr (- rand-ms))
              )))
;;;        (if (not (this 'rest))  ;sound level 
;;;           (then
;;;            (let ((rand-sl (/ (- (random sl-range) (/ sl-range 2.0)) 10.0)))
;;;               ;(print rand-ms)
;;;               (add-this 'sl rand-sl)
;;;               )))
       )))

(defun make-jitter (quant)
   (let ((beat-dur)(ms-range)(sl-range))
      (each-note
       (set-this 'ddr 0) )
      (each-note
       (if (this 'mm)
          (then 
            (setq beat-dur (/ 60000.0 (this 'mm)))
            ;(setq ms-range (round (* 0.05 quant beat-dur)))
            (setq ms-range (round (* 4 quant 710.67 (expt  (this 'mm) -0.6906))))
            (setq sl-range (round (* 10 quant)))
            ))
       (print-ll "beat-dur:" beat-dur " ms-range:" ms-range)
       (if (and                ;duration
            ;(not (this 'rest))
            (not (last?)) )
          (then
           (let ((rand-ms (- (random ms-range) (/ ms-range 2.0))))
              ;(print rand-ms)
              (add-this 'ddr rand-ms)
              (add-next 'ddr (- rand-ms))
              )))
       )
      (each-note-if
       (not (first?))
       (not (last?))
       (this 'ddr)
       (next 'ddr)
       (prev 'ddr)
       (then
        (add-this 'dr (/ (+ (this 'ddr) (prev 'ddr) (next 'ddr)) 3.0))
        ))
       ))

(defun make-jitter (quant)
   (let ((beat-dur)(ms-range)(sl-range))
      (each-note
       (set-this 'ddr 0) )
      (each-note
       (if (this 'mm)
          (then 
            (setq beat-dur (/ 60000.0 (this 'mm)))
            ;(setq ms-range (round (* 0.05 quant beat-dur)))
            (setq ms-range (round (* 4 quant 710.67 (expt  (this 'mm) -0.6906))))
            (setq sl-range (round (* 10 quant)))
            ))
       ;(print-ll "beat-dur:" beat-dur " ms-range:" ms-range)
       (if (and                ;duration
                ;(not (this 'rest))
                (not (last?)) )
          (then
           (let ((rand-ms (- (random ms-range) (/ ms-range 2.0))))
              ;(print rand-ms)
              (add-this 'ddr rand-ms)
              (add-next 'ddr (- rand-ms))
              )))
       )
      (let ((notes 0.0)(dddr-tot 0.0))
         (each-note-if
          (not (first?))
          (not (last?))
          (this 'ddr)
          (next 'ddr)
          (prev 'ddr)
          (then
           ;(set-this 'dddr (this 'ddr))
           (set-this 'dddr (/ (+ (this 'ddr) (prev 'ddr) (next 'ddr)) 3.0))
           (add-this 'dr (this 'dddr))
           (incf notes)
           (incf dddr-tot (abs (this 'dddr)))
           ))
         (print-ll "mean dddr: " (/ dddr-tot notes))
         )
      ))

;power curve value on each note
(defun make-jitter (quant)
   (let ((note-tempo)(ms-range)(sl-range))
      (each-note
       (set-this 'ddr 0) )
      (each-note
            (setq note-tempo (/ 60000.0 (this 'dr)))
            ;(setq ms-range (round (* 0.05 quant beat-dur)))
            (setq ms-range (round (* 4 quant 710.67 (expt  note-tempo -0.6906))))
            ;(setq sl-range (round (* 10 quant)))
       ;(print-ll "note-tempo " note-tempo " ms-range:" ms-range)
       (if (not (last?))
          (then
           (let ((rand-ms (- (random ms-range) (/ ms-range 2.0))))
              ;(print rand-ms)
              (add-this 'ddr rand-ms)
              (add-next 'ddr (- rand-ms))
              )))
       )
      (let ((notes 0.0)(dddr-tot 0.0))
         (each-note-if
          (not (first?))
          (not (last?))
          (this 'ddr)
          (next 'ddr)
          (prev 'ddr)
          (then
           ;(set-this 'dddr (this 'ddr))
           (set-this 'dddr (/ (+ (this 'ddr) (prev 'ddr) (next 'ddr)) 3.0))
           (add-this 'dr (this 'dddr))
           (incf notes)
           (incf dddr-tot (abs (this 'dddr)))
           ))
         (print-ll "mean dddr: " (/ dddr-tot notes))
         )
      ))       

;relative deviations
(defun make-jitter-relative (quant)
   (let ((note-tempo)(percent-range)(sl-range))
      (each-note
       (set-this 'ddr 0) )
      (each-note
            ;(setq note-tempo (/ 60000.0 (this 'dr)))
            ;(setq ms-range (round (* 0.05 quant beat-dur)))
            (setq percent-range (round (* 4 quant 5))) ;percent range
            ;(setq sl-range (round (* 10 quant)))
       ;(print-ll "note-tempo " note-tempo " percent-range " percent-range)
       (if (not (last?))
          (then
           (let ((rand-percent (- (random (+ percent-range 1)) (/ percent-range 2.0))))
              ;(print rand-ms)
              (add-this 'ddr rand-percent)
              ;(add-next 'ddr (- rand-ms))
              )))
       )
      (let ((notes 0.0)(dddr-tot 0.0))
         (each-note-if
          (not (first?))
          (not (last?))
          (this 'ddr)
          (next 'ddr)
          (prev 'ddr)
          (then
           ;(set-this 'dddr (this 'ddr))
           (set-this 'dddr (/ (+ (this 'ddr) (prev 'ddr) (next 'ddr)) 3.0))
           (set-this 'dr (* (this 'dr) (+ 1 (/ (this 'dddr) 100))))
           (incf notes)
           (incf dddr-tot (abs (this 'dddr)))
           ))
         (print-ll "mean dddr: " (/ dddr-tot notes))
         )
      ))

;parabolic curve value on each note
;with smoothing
(defun make-jitter (quant)
   (let ((ms-range)(sl-range))
      (each-note
       (set-this 'ddr 0) )
      (each-note
            (setq ms-range (round (* 4 quant (+ (* 3E-5 (power (this 'dr) 2))
                                                (- (* 0.0015 (this 'dr)))
                                                14.858 ))))
                                                
            ;(setq sl-range (round (* 10 quant)))
       ;(print-ll "note-tempo " note-tempo " ms-range:" ms-range)
       (if (not (last?))
          (then
           (let ((rand-ms (- (random ms-range) (/ ms-range 2.0))))
              ;(print rand-ms)
              (add-this 'ddr rand-ms)
              (add-next 'ddr (- rand-ms))
              )))
       )
      (let ((notes 0.0)(dddr-tot 0.0))
         (each-note-if
          (not (first?))
          (not (last?))
          (this 'ddr)
          (next 'ddr)
          (prev 'ddr)
          (then
           ;(set-this 'dddr (this 'ddr))
           (set-this 'dddr (/ (+ (this 'ddr) (prev 'ddr) (next 'ddr)) 3.0))
           (add-this 'dr (this 'dddr))
           (incf notes)
           (incf dddr-tot (abs (this 'dddr)))
           ))
         (print-ll "mean dddr: " (/ dddr-tot notes))
         )
      ))  

|#       
    
                

