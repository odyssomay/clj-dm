;;;-*-Mode: LISP; Package: DM -*-

;;New accent rules inpired by CMJ91 paper
;;/Anders Friberg


(in-package "DM")


;;-------- short between long --------------------  

;;including long rests
(defun accent-short-between-long (quant &key (amp 1) (dur 1) (art 1))
  (each-note-if
   (not (first?))
   (not (last?))
   (< (this 'dr) (prev 'dr))
   (< (this 'dr) (next 'dr))
   (not (this 'rest))
   (< (this 'dr) 300)
   (then
    (add-this 'sl (* 3.0 quant amp ))
    (let ((dradd  (* 20 quant dur )))                        ;(- (* (this 'dr) 1.3 quant dur ) (this 'dr))))
      (add-this 'dr dradd)
      (add-prev 'dr (- dradd)) )
    (if (prev 'dro)
        (add-prev 'dro (* 30 quant art ))
      (set-prev 'dro (* 30 quant art ))
      ))))

