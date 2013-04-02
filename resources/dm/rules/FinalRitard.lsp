
;; final ritard according to the model in Friberg & Sundberg (1999) JASA

;;971117/af converted to DM2
;;100505/af fixed so that also very short tracks are processed

(in-package :dm)

#|
(defun final-ritard (quant &key (q 3))
  (if *rule-debug-info* (print-ll "q = " q))
  (let ((length (* 1300. (abs quant)))
        (vend (/ 1.0 (* 4.0 quant))) )
    (each-note-if
      (last?)
      (then
        (let ((istart *i*) (ndrtot 0))
          (while (< ndrtot length)
            (decf istart)
            (incf ndrtot (iget istart 'ndr)) )
          (let* (
                 ;(xon 0) 
                 (xoff 0) ;normalized
                 (exponent (/ (- q 1.0) q))
                 (k (- (expt vend q) 1))
                 ;(namnare (- (expt (+ 1 k) exponent) 1))
                 (namnare (* (- q 1) k))
                 (const (- (/ q (* (- q 1) k))))
                 (ton 0)
                 (toff 0) )  ;normalized
                  ;(print-ll "exponent " exponent " namnare " namnare)
            (loop for i from istart to (- *i* 1) do
                  (setq xoff (+ xoff (/ (iget i 'ndr) ndrtot))) ;normalized
                  (setq ton toff)
                  (setq toff (+ (/ (* q (expt (+ 1 (* k xoff)) exponent)) namnare) const))  ;normalized
                  ;(print-ll "xoff " xoff " ton " ton " toff " toff)
                  (iset i 'dr 
                            (* (iget i 'dr)
                               (/ (* ndrtot (- toff ton))
                                  (iget i 'ndr) )))
                  ))))))
  (final-ritard-last-note) )
|#

;;NEW 0009/AF
;;added key parameter :length specifying rit length in msec
;;changed quant scaling so that quant=1 => 1/4 and quant=0 => 1
#|
(defun final-ritard (quant &key (q 3) length)
  (if *rule-debug-info* (print-ll "q = " q))
  (let ((len (if length length (* 1300. (abs quant))))
        (vend (/ 1.0 (+ 1.0 (* 3.0 quant)))) )
    (each-note-if
      (last?)
      (then
        (let ((istart *i*) (ndrtot 0))
          (while (< ndrtot len)
            (decf istart)
            (incf ndrtot (iget istart 'ndr)) )
          (let* (
                 ;(xon 0) 
                 (xoff 0) ;normalized
                 (exponent (/ (- q 1.0) q))
                 (k (- (expt vend q) 1))
                 ;(namnare (- (expt (+ 1 k) exponent) 1))
                 (namnare (* (- q 1) k))
                 (const (- (/ q (* (- q 1) k))))
                 (ton 0)
                 (toff 0) )  ;normalized
                  ;(print-ll "exponent " exponent " namnare " namnare)
            (loop for i from istart to (- *i* 1) do
                  (setq xoff (+ xoff (/ (iget i 'ndr) ndrtot))) ;normalized
                  (setq ton toff)
                  (setq toff (+ (/ (* q (expt (+ 1 (* k xoff)) exponent)) namnare) const))  ;normalized
                  ;(print-ll "xoff " xoff " ton " ton " toff " toff)
                  (iset i 'dr 
                            (* (iget i 'dr)
                               (/ (* ndrtot (- toff ton))
                                  (iget i 'ndr) )))
                  ))))))
  (final-ritard-last-note) )
|#
;;allows also very short tracks
(defun final-ritard (quant &key (q 3) length)
  (if *rule-debug-info* (print-ll "q = " q))
  (let ((len (if length length (* 1300. (abs quant))))
        (vend (/ 1.0 (+ 1.0 (* 3.0 quant)))) )
    (each-note-if
      (last?)
      (then
        (let ((istart *i*) (ndrtot 0))
          (while (and (< ndrtot len) (> istart 0))
            (decf istart)
            (incf ndrtot (iget istart 'ndr)) )
          (let* (
                 ;(xon 0) 
                 (xoff 0) ;normalized
                 (exponent (/ (- q 1.0) q))
                 (k (- (expt vend q) 1))
                 ;(namnare (- (expt (+ 1 k) exponent) 1))
                 (namnare (* (- q 1) k))
                 (const (- (/ q (* (- q 1) k))))
                 (ton 0)
                 (toff 0) )  ;normalized
                  ;(print-ll "exponent " exponent " namnare " namnare)
            (loop for i from istart to (- *i* 1) do
                  (setq xoff (+ xoff (/ (iget i 'ndr) ndrtot))) ;normalized
                  (setq ton toff)
                  (setq toff (+ (/ (* q (expt (+ 1 (* k xoff)) exponent)) namnare) const))  ;normalized
                  ;(print-ll "xoff " xoff " ton " ton " toff " toff)
                  (iset i 'dr 
                            (* (iget i 'dr)
                               (/ (* ndrtot (- toff ton))
                                  (iget i 'ndr) )))
                  ))))))
  (final-ritard-last-note) )


;;new-last-ntempo-factor is the decrease in tempo of the second final note and
;; should be a value between 0 and 1
(defun equalize-tempo (new-last-ntempo-factor)
  (let ((last-ntempo))
    (each-note
      (setq *i* (1- (i?last)))
      (setq last-ntempo (this-ntempo))
      (return) )
    ;(print last-ntempo)
    (scale-tempo (/  (1- new-last-ntempo-factor) (1- last-ntempo)))
    ))

(defun final-ritard-last-note ()
  (let ((factor 1.25))  ;  the increase in dr for the last note rel. sec. last
    (each-note
      (setq *i* (i?last))
      (when (< (* factor (this 'dr)) (prev 'dr))
        (set-this 'dr (* factor (prev 'dr))) )
      (return) )))

                               