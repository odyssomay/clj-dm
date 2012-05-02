;;Random variations in IOI and sound level
;;Models variations found in the litterature from the internal 'clock' and from motor control.
;;Developed by Friberg, Bresin and Juslin starting in 1999
;;000303/af extracted out the latest stuff from Germ project and added the 'noise' function

(in-package :dm)


;;toplevel function
(defun noise (quant &key (amp 1)(dur 1))
  (when (not (zerop (* dur quant)))
    (make-internal-clock-noise (* dur quant))
    (make-motor-noise (* dur quant)) )
  (when (not (zerop (* amp quant)))
    (make-motor-noise-amp (* amp quant)) )
  )


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
        (set-this 'dddr (+ (* 0.1 (- (this 'ddr) (prev 'dddr))) (prev 'dddr))) 
        ;empirically derived const = 0.58 (1.74) so that sd=3.05 at 300 ms IOI
        ;and filter param above = 0.2 (0.05), 0.01 
        (add-this 'dr (* 1.25 (this 'dddr)))         ))
      ))  


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

(defun make-motor-noise-amp (quant)
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