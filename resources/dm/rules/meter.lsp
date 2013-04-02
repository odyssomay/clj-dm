;;-*-Mode: LISP; Package: DM -*-
;;
;; *****************************************************
;;   Swing and ritard rules using timemaps proposed by
;;   David Jaffe (ICMC84)   
;; *****************************************************
;;
;; /Anders Friberg  Spring 1986.
;; 9203 cl
;; 9801 
 

(in-package "DM")


;;----------- swing ---------------------------------------



;accent to note before rest
(defun acc-rest (quant)
  (each-note-if
    (next 'rest)
    (then 
      (set-this 'a0 (+ (this 'a0) (* 10 quant))) )))


;inegalles only on consequtive eighth notes
;not in syncopation or in combination with rests
(defun baroque-inegalles-8 (quant)
  (mark-beat)
  (each-note-if
    (this :beat)
    (not (last?))
    (= 8 (note-to-notevalue (this 'n)))
    (= 8 (note-to-notevalue (next 'n)))
    (not (this 'rest))
    (not (next 'rest))
    (/= (this-f0) (next-f0))
    (then
      (let ((addval (* (this 'dr) 0.22 quant)))
        (add-this 'dr addval)
        (add-next 'dr (- addval))
        )))
  ;(rem-all :beat)
  )

(defun baroque-inegalles-16 (quant)
  (mark-beat-8)
  (each-note-if
    (this :beat-8)
    (not (last?))
    (= 16 (note-to-notevalue (this 'n)))
    (= 16 (note-to-notevalue (next 'n)))
    (not (this 'rest))
    (not (next 'rest))
    (/= (this-f0) (next-f0))
    (then
      (let ((addval (* (this 'dr) 0.22 quant)))
        (add-this 'dr addval)
        (add-next 'dr (- addval))
        )))
  ;(rem-all :beat-8)
  )


(defun mark-beat ()
 (if (not (get-first 'meter))
   (error "no meter in the first note"))
  (let (beat-dr dr-ack) 
    (each-note             ;mark beat
      (when (this 'meter) 
        (setq beat-dr (round (note-to-dr (cons nil (cadr (this 'meter)))))))
      (when (this 'bar) 
        (setq dr-ack 0))
      (when (or (this 'bar)(zerop (mod (round dr-ack) beat-dr)))
        (set-this :beat t)
        ;(print-ll *i* "  " (this 'n))
        )
      (setq dr-ack (+ dr-ack (nom-dr *i*)))
      )))

(defun mark-beat-8 ()
 (if (not (get-first 'meter))
   (error "no meter in the first note"))
  (let (beat-dr dr-ack)
    (each-note             ;mark beat
      (when (this 'meter) 
        (setq beat-dr (round (/ (note-to-dr (cons nil (cadr (this 'meter)))) 2.0))))
      (when (this 'bar) 
        (setq dr-ack 0))
      (when (or (this 'bar)(zerop (mod (round dr-ack) beat-dr)))
        (set-this :beat-8 t)
        ;(print-ll *i* "  " (this 'n))
        )
      (setq dr-ack (+ dr-ack (nom-dr *i*)))
      )))


;swing type inegalles on every weak onset
(defun inegalles (quant)
  (let ((mm (get-first 'mm)))
    (cond ((> quant 4.5)
           (setq quant 4.5)
           (warn "Quantity > 4.5 is not applicable. 4.5 will be used instead"))
          ((< quant -4.5)
           (setq quant -4.5)
           (warn "Quantity < -4.5 is not applicable. -4.5 will be used instead")) )
    (swing8 (round (infix (quant * 0.22 * 30000. / mm))) 0)
    ))

;; **************
(defun inegalles (quant)
   (print "inegalles has been temporarily disabled"))
   

(defun inegalles-dot (quant &key (dotted-note-value 16))
  (print-ll "dotted-note-value = " dotted-note-value) 
  (ifn (or (= dotted-note-value 8)
           (= dotted-note-value 16))
       (error (format nil "inegalles-dot, wrong :dotted-note-value : ~A" dotted-note-value)))
  (let ((mulvalue (* quant 0.33)))
    (each-note-if
      (not (last?))
      (this 'dot)
      (or (and (= dotted-note-value 8)
               (= 8 (note-to-notevalue (this 'n)))
               (= 16 (note-to-notevalue (next 'n))))
          (and (= dotted-note-value 16)
               (= 16 (note-to-notevalue (this 'n)))
               (= 32 (note-to-notevalue (next 'n)))) )
      (then
        (let ((ddr (* (next 'dr) mulvalue)))
          ;(print ddr)
          (add-this 'dr (- ddr))
          (add-next 'dr ddr)
          )))))

;;; 
;;; 
;;; ;8'th notes
;;; ;normal: d8=35 a0-quant=0
;;; (defun swing8 (d8 a0-quant)
;;;   (declare (special d8))
;;; ; (mark-bar)
;;;  (let ((dmap))
;;;   (let ((fn8+ #'(lambda (x) (declare (special d8))(/ (* x d8) 160)))
;;;         (fn8- #'(lambda (x) (declare (special d8))(- (/ (* x d8) 160)))) )
;;;     (for (end-time 0 320 960)
;;;      (setq dmap (append1 dmap (list (+ end-time 160) fn8-)))
;;;      (setq dmap (append1 dmap (list (+ end-time 320) fn8+))) ))
;;;  (bar-dmap dmap a0-quant)))


;;; (defun swing16 (d16 a0-quant)
;;;   (declare (special d16))
;;; ; (mark-bar)
;;;  (let ((dmap))
;;;   (let ((fn16+ #'(lambda (x) (declare (special d16))(/ (* x d16) 160)))
;;;         (fn16- #'(lambda (x) (declare (special d16))(- (/ (* x d16) 160)))) )
;;;     (for (end-time 0 160 1120)
;;;      (setq dmap (append dmap (list (list (+ end-time 80) fn16-))))
;;;      (setq dmap (append dmap (list (list (+ end-time 160) fn16+)))) ))
;;;  (bar-dmap dmap a0-quant)))


;; in progress
(defun waltz-beat-kreisler (quant)
  (setq quant (- quant))
  (let ((mm (get-first 'mm)))
    (waltz-beat-kreisler-1 (round (infix (quant * 0.22 * 30000. / mm))) 0)
    ))


(defun waltz-beat-kreisler-1 (d8 a0-quant)
  (declare (special d8))
; (mark-bar)
 (let ((dmap))
  (let ((fn8+ #'(lambda (x) (declare (special d8))(/ (* x d8) 320)))
        (fn8- #'(lambda (x) (declare (special d8))(- (/ (* x d8) 320))))
        (fn8 #'(lambda (x) (declare (special d8)) 0 ))
        )
     (setq dmap (append1 dmap (list 320 fn8-)))
     (setq dmap (append1 dmap (list 640 fn8+)))
     (setq dmap (append1 dmap (list 960 fn8)))
     )
 (bar-dmap dmap a0-quant)))

;;;  
;;; ;perturb the timing of the bar according to the dmap
;;; ;specified in ms resp 0.1ms for the perturbation
;;; (defun bar-dmap (dmap level-quant)
;;;   (each-note-if
;;;     (this 'bar)
;;;     (then
;;;       (let ((dr-bar 0)(dr-ack 0)(ddr 0)
;;;             (ibar (if (i?next *i* 'bar)
;;;                       (1- (i?next *i* 'bar))
;;;                       (i?last) )))
;;;         (for (*i* *i* 1 ibar)
;;;           (setq dr-bar (+ dr-bar (this 'dr))) )
;;;         (set-this 'ddr (apply-dmap 0 dmap))
;;;         (for (*i* (1+ *i*) 1 ibar)
;;;           (setq dr-ack (+ dr-ack (prev 'dr)))
;;;           (setq ddr (apply-dmap (/ (* dr-ack 1280.) dr-bar) 
;;;                                 dmap ))
;;;           (set-this 'ddr ddr) ))))
;;;  (each-note-if
;;;    (this 'ddr)
;;;    (not (first?))
;;;    (then
;;;         (set-this 'dr (+ (this 'dr) (this 'ddr)))
;;;         (set-prev 'dr (- (prev 'dr) (this 'ddr)))
;;;         (if (not (this 'rest))
;;;             (add-this-l0
;;;               (- (* level-quant (/ (this 'ddr) 10.))) ))
;;;         (rem-this 'ddr) )))
;;; 
;;; 
;;; 
;;; (defun bar-dmap (dmap level-quant)
;;;   (each-note-if
;;;     (this 'bar)
;;;     (then
;;;       (let ((dr-bar 0)(dr-ack 0)(ddr 0)
;;;             (ibar (if (i?next *i* 'bar)
;;;                       (1- (i?next *i* 'bar))
;;;                       (i?last) )))
;;;         (for (*i* *i* 1 ibar)
;;;           (setq dr-bar (+ dr-bar (this 'dr))) )
;;;         (set-this 'ddr (apply-dmap 0 dmap))
;;;         (for (*i* (1+ *i*) 1 ibar)
;;;           (setq dr-ack (+ dr-ack (prev 'dr)))
;;;           (setq ddr (apply-dmap (/ (* dr-ack 1280.) dr-bar) 
;;;                                 dmap ))
;;;           (set-this 'ddr ddr) ))))
;;;  (each-note-if
;;;    (this 'ddr)
;;;    (not (first?))
;;;    (then
;;;         (add-this 'dr  (this 'ddr))
;;;         (add-prev 'dr (- (this 'ddr)))
;;;         (if (not (this 'rest))
;;;             (add-this-l0
;;;               (- (* level-quant (/ (this 'ddr) 10.))) ))
;;;         (rem-this 'ddr) )))

;returns the current delta duration from the dmap for the
;pre-time value related to the beginning of the dmap
(defun apply-dmap (pre-time dmap)
  (let ((prev-dtime 0)(prev-end-time 0))
    (untilexit end
               (let ((end-time (caar dmap))
                     (dmap-fn  (cadar dmap)))
                 (pop dmap)
                 (if (or (null dmap)
                         (< pre-time end-time))
                   (return-from end
                     (+ prev-dtime (funcall dmap-fn
                                            (- pre-time prev-end-time))) )
                   (progn
                     (setq prev-dtime
                           (+ prev-dtime
                              (funcall dmap-fn (- end-time prev-end-time))))
                     (setq prev-end-time end-time))
 )))))

#|      
(setq dmap
  `((160 fn-)
    (320 fn+)
    (480 fn-)
    (640 fn+)
    (800 fn-)
    (960 fn+)
    (1120 fn-)
    (1280 fn+)
 ))
|#

(defun fn+ (x)
  (/ (* x 16) 160))

(defun fn- (x)
  (- (/ (* x 16) 160)))

;--- Ritard ------------------------------------

(defun final-ritard (quant &key (shape :sqrt))
  (if *rule-debug-info* (print-ll "shape = " shape))
  (cond ((eq shape :sqrt)
         (final-ritard-aux 'final-ritard-fn-sqrt quant))
        ((eq shape :lin)
         (final-ritard-aux 'final-ritard-fn-lin quant))
        (t (error (format nil "final-ritard, not allowable shape : ~A" shape)))
        )
  (if (not *rule-debug-info*) (rem-all 'newdr))
  )

;1/sqrt(1 - time/ttot)
(defun final-ritard-fn-sqrt (time ttot)
  (/ 1 (sqrt (- 1 (/ time ttot)))))

(defun final-ritard-fn-lin (time ttot)
  (/ 1 (- 1 (* .77 (/ time ttot)))) )

;;;  
;;; 
;;; ;apply a final ritardando 
;;; ; length specifies the ritardando length in ms to the onset
;;; ; of the last note
;;; ; the last note will be 3.3 times longer for quant = 1
;;; (defun final-ritard-aux (fn quant)
;;;  (let ((length (* 2000. (abs quant))))
;;;   (each-note-if
;;;     (>= length (drsum *i* (i?last)) ) ;terrible
;;;     (then
;;;         (let ((ttot (* 1.1 length))
;;;               (dr-ack (- length (drsum *i* (i?last)))) )
;;;           (for (*i* *i* 1 (i?last))
;;;             (set-this 'newdr (* (this 'dr)
;;;                              (funcall fn dr-ack ttot) ))
;;;             ;(print-ll "dr-ack" dr-ack "ritrot " (funcall fn dr-ack ttot))
;;;             (setq  dr-ack (+ dr-ack (this 'dr))) )
;;;           (setq *i* (i?last)) )))
;;;   (each-note-if
;;;         (this 'newdr)
;;;         (then
;;;          (add-this 'dr (* quant 0.3 (- (this 'newdr) (this 'dr ))))
;;;          ;(rem-this 'newdr)
;;;          ))))



#|
;--- New Ritard ------------------------------------

(defun final-ritard-new (quant &key (shape :sqrt))
  (if *rule-debug-info* (print-ll "shape = " shape))
  (let ((length (* 2000. (abs quant)))
        (maxval (* 3.0 quant)))
    (each-note-if
      (>= length (drsum *i* (i?last)) ) ;terrible
      (then
        (let ((istart *i*)
              (iend (i?last)))
          (cond 
           ((eq shape :sqrt)
            (iramp-sqrt3-new-decimal istart iend 0.0 maxval 'ddr)
            (iset iend 'ddr maxval) )
           ((eq shape :lin)
            (iramp-new-decimal istart iend 0.0 maxval 'ddr)
            (iset iend 'ddr maxval))
           (t (error (format nil "final-ritard, not allowable shape : ~A" shape)))
           ))
        (setq *i* (i?last))))
    ;apply
    (each-note-if
      (this 'ddr)
      (set-this 'dr (* (this 'dr) (+ 1. (this 'ddr))))
      (rem-this 'ddr) )))
|#
