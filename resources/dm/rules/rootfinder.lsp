
;;root finding algorithm by Craig Sapp 27 april 2001

;;if the sharps and flats are unknown
;;try all flats and all sharps and see which fits the best

(in-package :dm)


(defun find-roots (meter-level)
  (mark-metric-level)
  (mark-base-40-pitch)
  (let ((alpha 0.85)
        (delta -4.0)
        (lambda -3.0)
        (vx '(0 7 7 1000 4 4 4 4 4 1000 8 1 1 8 8 5 5 5 5 5 1000 9 2 2 2 9 1000 6 6 6 6 6 1000 3 3 3 3 10 7 7))
        (vy '(0 -2 -4 1000 4 2 0 -2 -4 1000 3 1 1 -3 -5 5 3 1 -1 -3 1000 4 2 0 -2 -4 1000 4 2 0
              -2 -4 1000 3 1 -1 -3 -5 4 2))
        )
  (p-each-note
   (when (and (cdar (p-this ':metric-level))
              (>= (cdar (p-this ':metric-level)) meter-level) )
     (princ "note: ")
     (print (mapcar 'cdr (p-this 'n)))
     (let ((pitch-list (mapcar 'cdr (p-this :base-40-pitch)))
           (duration-list (mapcar 'cdr (p-this :note-value-fraction)))
           (level-list (mapcar 'cdr (p-this :metric-level)))
           dsum lsum i imag
           (score '()) 
           )
       (print pitch-list)
       (print duration-list)
       (print level-list)
       
       (loop for r from 0 to (1- (length pitch-list)) do
             (setq dsum 0.0)
             (setq lsum 0.0)      
             (loop for n from 0 to (1- (length pitch-list)) do
                   (setq i (mod (+ (- (nth n pitch-list) (nth r pitch-list)) 40) 40))
                   (setq imag (sqrt (+ (* (expt alpha 2) (expt (nth i vx) 2)) (expt (nth i vy) 2))))
                   (incf dsum (* imag (+ delta (log (* 4 (nth n duration-list)) 2))))
                   (incf lsum (* imag (+ lambda (log (nth n level-list) 2))))
                   )
             (push (cons (nth r pitch-list) (expt 2 (/ (+ dsum lsum) (length pitch-list)))) score)
             )
       (print score)
       )))))

;;4/4 for now
(defun mark-metric-level ()
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
      ))))

(defun mark-base-40-pitch ()
  (let ((base-40-pitch 0))
  (each-note-if
   (not (this 'rest))
   (then
    (let ((tone (subseq (note-to-tone (this 'n)) 0 1)))
      (cond 
       ((string= "C" tone)
        (setq base-40-pitch 0) )
       ((string= "D" tone)
        (setq base-40-pitch 6) )
       ((string= "E" tone)
        (setq base-40-pitch 12) )
       ((string= "F" tone)
        (setq base-40-pitch 17) )
       ((string= "G" tone)
        (setq base-40-pitch 23) )
       ((string= "A" tone)
        (setq base-40-pitch 29) )
       ((string= "B" tone)
        (setq base-40-pitch 35) )
       ))
    (if (flat-tone? (note-to-tone (this 'n))) (decf base-40-pitch 1))
    (if (sharp-tone? (note-to-tone (this 'n))) (incf base-40-pitch 1))
    (set-this :base-40-pitch base-40-pitch)
    ))))
      
