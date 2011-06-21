
;;
;; *****************************************
;;   Definitions for the shape object
;;   
;; *****************************************

(in-package :dm)

;;; // File: shapes.idl
;;; // Version: 0.1
;;; // Date of creation [yy.mm.dd]: 98.08.25
;;; // Author: Peter Lunden <ludde@kacor.kth.se>

;;; adapted to DM by Anders Friberg
;;; 2005-05-11/af added the function get-last-x-value so that an envelope can stop before end of note
;;; 2005-05-12/af added staircase-bp-shape suited for reading eg pitch bend from midifile
;; -----------------
;;   dependant
;; -----------------

(defclass dependant ()
  ((dependee :reader get-dependee :initarg :dependee :initform nil)
    ))
(defmethod set-dependee ((self dependant) dependee)
   (setf (slot-value self 'dependee) dependee) )

;; -----------------
;;   dependant
;; -----------------

(defclass shape (dependant)())

;;------------------------------------
;;  bp-shape
;;------------------------------------

(defclass bp-shape (shape)
  ((break-points :reader get-break-points :initarg :break-points :initform nil)
   (x-min :reader xget-x-min :initarg :x-min :initform 0)
   (x-max :reader xget-x-max :initarg :x-max :initform 1)
   (y-min :reader xget-y-min :initarg :y-min :initform 0)
   (y-max :reader xget-y-max :initarg :y-max :initform 1)) )

;;fixa get-x-min med indirekt anrop alltså objekt möjligt som värde

;internal utility for setting the whole list
(defmethod set-break-points ((self bp-shape) value)
   (setf (slot-value self 'break-points) value) )

(defmethod get-x-min ((self bp-shape))
   (if (numberp (xget-x-min self))
     (xget-x-min self)
     (funcall (xget-x-min self) self) ))

(defmethod get-x-max ((self bp-shape))
   (if (numberp (xget-x-max self))
     (xget-x-max self)
     (funcall (xget-x-max self) self) ))

(defmethod get-y-min ((self bp-shape))
   (if (numberp (xget-y-min self))
     (xget-y-min self)
     (funcall (xget-y-min self) self) ))

(defmethod get-y-max ((self bp-shape))
   (if (numberp (xget-y-max self))
     (xget-y-max self)
     (funcall (xget-y-max self) self) ))

(defmethod set-x-min ((self bp-shape) value)
   (setf (slot-value self 'x-min) value) )
(defmethod set-x-max ((self bp-shape) value)
   (setf (slot-value self 'x-max) value) )
(defmethod set-y-min ((self bp-shape) value)
   (setf (slot-value self 'y-min) value) )
(defmethod set-y-max ((self bp-shape) value)
   (setf (slot-value self 'y-max) value) )

(defmethod get-n-of-bp ((self bp-shape))
   (length (get-break-points self)) )

(defmethod get-break-point ((self bp-shape) index)
   (let ((bp (nth index (get-break-points self))))
   (values (first bp) (second bp)) ))

(defmethod get-norm-x-value ((self bp-shape) index)
   (first (nth index (get-break-points self))) )

(defmethod get-norm-y-value ((self bp-shape) index)
   (second (nth index (get-break-points self))) )

(defmethod get-x-value ((self bp-shape) index)
   (norm-x-to-x self (get-norm-x-value self index)) )

(defmethod get-y-value ((self bp-shape) index)
  (norm-y-to-y self (get-norm-y-value self index)) )

;;utility for finding the last position
(defmethod get-last-x-value ((self bp-shape))
  (get-x-value self (1- (get-n-of-bp self))) )

;;set a y-value to an existing x-position index
(defmethod set-norm-y-value ((self bp-shape) index value)
    (setf (second (nth index (get-break-points self)))
      (+ value)))

(defmethod set-y-value ((self bp-shape) index value)
    (set-norm-y-value self index (Y-TO-NORM-Y self value)))

;convert from normalized x to x
(defmethod norm-x-to-x ((self bp-shape) x)
    (+ (* (- (get-x-max self) (get-x-min self)) x) (get-x-min self)))

(defmethod norm-y-to-y ((self bp-shape) y)
    (+ (* (- (get-y-max self) (get-y-min self)) y) (get-y-min self)))

(defmethod x-to-norm-x ((self bp-shape) x)
     (/ (- (float x) (get-x-min self)) (- (get-x-max self) (get-x-min self))) )

(defmethod y-to-norm-y ((self bp-shape) y)
     (/ (- (float y) (get-y-min self)) (- (get-y-max self) (get-y-min self))) )

;adds a bp to the end
(defmethod add-norm-break-point ((self bp-shape) x y)
   (set-break-points self (nconc (get-break-points self) (list (list x y)))) )

(defmethod remove-break-point ((self bp-shape) index)
   (set-break-points self
      (delete-if #'(lambda (a) a) (get-break-points self) :start index :end (1+ index)) )
   )

;;ej fixad
;;; (defmethod get-nearest-bps-index ((self bp-shape) x-value)
;;;   (let ((n 0) m x1 x2
;;;          (xmin (get-x-min self))
;;;          (xmax (get-x-max self)) ) ;***** fixa funktionsanrop  
;;;      ;;check boundaries
;;;      (if (or (< x-value (x-to-abs-x (get-x-value self 0) xmin xmax))
;;;              (> x-value (x-to-abs-x (get-x-value self (1- (get-n-of-bp self))) xmin xmax)) )
;;;         (error "x-value ~A outside range of break-points" x-value) )
;;;      ;;select the two nearest times
;;;      (untilexit loopa
;;;        (setq x2 (x-to-abs-x (get-x-value self n) (get-type self n) xmin xmax))
;;;        ;(print-ll x1 " " x2)
;;;        (cond ((< x-value x2)
;;;               (setq m (1- n))
;;;               (return-from loopa) )
;;;              ((= x-value x2)
;;;               (setq m n)
;;;               (return-from loopa) ))
;;;        (incf n) )
;;;      ;;interpolate if not equal values
;;;      (values m n) ))
;;; 

#|
(defmethod insert-norm-break-point ((self bp-shape) x y)
   (let ((n 0) xi) 
      (cond 
            ((or (< x 0)(> x 1)(< y 0)(> y 1))
             (warn "x=~A, y= ~A outside range of normalized break-points (0..1)" x y) )
            ((not (get-break-points self))
             (set-break-points self (list (list x y))) )
            ((< x (get-norm-x-value self 0))
             (set-break-points self (nconc (list (list x y)) (get-break-points self))) )
            ((> x (get-norm-x-value self (1- (get-n-of-bp self))))
             (set-break-points self (nconc (get-break-points self) (list (list x y)))) )
            (t
              (untilexit loopa
                (setq xi (get-norm-x-value self n))
                (print xi)
                (cond ((< x xi)
                       (set-break-points self 
                         (ninsert-element-in-list (get-break-points self) n (list x y) ))
                       (return-from loopa) )
                      ((= x xi)
                       (error "attempt to set a break-point on a existing position: x= ~A y= ~A" x 
                         y) ))
                         (incf n) ) ))))

;;allowing bp's outside region - a bit dangerous****
(defmethod insert-norm-break-point ((self bp-shape) x y)
  (let ((n 0) xi)
    (if (or (< x 0)(> x 1)(< y 0)(> y 1))
            (warn "x=~A, y= ~A outside range of normalized break-points (0..1)" x y) )
      (cond       
            ((not (get-break-points self))
             (set-break-points self (list (list x y))) )
            ((< x (get-norm-x-value self 0))
             (set-break-points self (nconc (list (list x y)) (get-break-points self))) )
            ((> x (get-norm-x-value self (1- (get-n-of-bp self))))
             (set-break-points self (nconc (get-break-points self) (list (list x y)))) )
            (t
              (untilexit loopa
                (setq xi (get-norm-x-value self n))
                (print xi)
                (cond ((< x xi)
                       (set-break-points self 
                         (ninsert-element-in-list (get-break-points self) n (list x y) ))
                       (return-from loopa) )
                      ((= x xi)
                       (error "attempt to set a break-point on a existing position: x= ~A y= ~A" x 
                         y) ))
                (incf n) ) ))))
|#

;;allowing bp's outside region - a bit dangerous****
;also allowing same position - also dangerous!
(defmethod insert-norm-break-point ((self bp-shape) x y)
  (let ((n 0) xi)
    (if (or (< x 0)(> x 1)(< y 0)(> y 1))
            (warn "x=~A, y= ~A outside range of normalized break-points (0..1)" x y) )
      (cond       
            ((not (get-break-points self))
             (set-break-points self (list (list x y))) )
            ((< x (get-norm-x-value self 0))
             (set-break-points self (nconc (list (list x y)) (get-break-points self))) )
            ((>= x (get-norm-x-value self (1- (get-n-of-bp self))))
             (set-break-points self (nconc (get-break-points self) (list (list x y)))) )
            (t
              (untilexit loopa
                (setq xi (get-norm-x-value self n))
                ;(print xi)
                (cond ((< x xi)
                       (set-break-points self 
                         (ninsert-element-in-list (get-break-points self) n (list x y) ))
                       (return-from loopa) )
;;;                      ((= x xi)
;;;                       (error "attempt to set a break-point on a existing position: x= ~A y= ~A" x 
;;;                         y) )
                      )
                (incf n) ) ))))

(defmethod insert-break-point ((self bp-shape) x y)
   (insert-norm-break-point self (x-to-norm-x self x) (y-to-norm-y self y)) )

;inserts a list of break-points
;format: (<time in ms> <value> ..........)
(defmethod insert-break-point-list ((self bp-shape) list)
  (until (not list)
   (insert-break-point self (pop list) (pop list)) ))


;utility function
;inserts an element in a list destructively
(defun ninsert-element-in-list (lista n element)
   (cond
    ((= n 0)
     (push element lista) )
    (t
     (rplacd (nthcdr (1- n) lista) (nconc (list element) (nthcdr n lista))) ))
     lista)


;;------------------------------------
;;  interpolated-bp-shape
;;------------------------------------

(defclass interpolated-bp-shape (bp-shape)
  ((interpolation-fn :reader get-interpolation-fn :initarg :interpolation-fn :initform nil)
   ))

(defmethod set-interpolation-fn ((self interpolated-bp-shape) fn)
   (setf (slot-value self 'interpolation-fn) fn) )

;returns an interpolated value for a given x value
;x and y in normalized values
(defmethod get-norm-value-at ((self interpolated-bp-shape) x-value)
  (if (> X-VALUE 1.0) (setq X-VALUE 1.0)) ;fix for round off errors
  (if (< X-VALUE 0) (setq X-VALUE 0))
   (let ((n 0) x1 x2 y1 y2) 
      ;;select the two nearest times
      (untilexit loopa
        (setq x2 (get-norm-x-value self n))
        ;(print-ll "X-value " x-value " x1 " x1 " x2 " x2)
        (cond ((< x-value x2)
               (setq x1 (get-norm-x-value self (1- n)))
               (setq y1 (get-norm-y-value self (1- n)))
               (setq y2 (get-norm-y-value self n))
               (return-from loopa) )
              ((= x-value x2)
               (setq y2 (get-norm-y-value self n))
               (setq y1 y2)
               (return-from loopa) ))
        (incf n) )
      ;;interpolate if not equal values
      (if (= y1 y2)
         y1
         (funcall (get-interpolation-fn self) x-value x1 (float y1) x2 y2)
           )))

(defmethod get-value-at ((self interpolated-bp-shape) x)
   (norm-y-to-y self (get-norm-value-at self (x-to-norm-x self x))) )

;;adds bp-shape envelope to the INTERPOLATED-BP-SHAPE
;; if both has linear interpolation the result will be "exact"
;; for other interpolation functions the result will be approximate
;; but using the interpolated values for new points
(defmethod add-bp-shape ((self interpolated-bp-shape) bp-shape)
  ;(print-ll (get-break-points self) (get-break-points bp-shape))
  ;PART 1: add new y-values to existing positions
  (loop for i from 0 to (1- (GET-N-OF-BP self)) do
        ;(print (get-y-value self i))
        (set-y-value self i (+ (get-y-value self i)
                             (get-value-at BP-SHAPE (norm-x-to-x self (get-break-point self i)))))
        )
  ;;PART 2: any new points are inserted in the old envelope
  (loop for i from 0 to (1- (GET-N-OF-BP BP-SHAPE)) do
        (when (not (member (get-break-point BP-SHAPE i) (get-break-points self) :key #'car))
          (print-ll "new bp : " (norm-x-to-x BP-SHAPE (get-break-point BP-SHAPE i)))
          (insert-break-point self
                              (norm-x-to-x BP-SHAPE (get-break-point BP-SHAPE i))
                              (+ (get-y-value BP-SHAPE i)
                                 (get-value-at self (norm-x-to-x BP-SHAPE (get-break-point BP-SHAPE i)))))
          ))
        )


;;------------------------------------
;;  linear-bp-shape,  COSINUS-BP-SHAPE
;;------------------------------------

(defclass linear-bp-shape (interpolated-bp-shape)())

(defmethod initialize-instance :after ((self linear-bp-shape) &rest initargs)
   (set-interpolation-fn self 'linear-interpolation)
   self )

(defclass cosinus-bp-shape (interpolated-bp-shape)())

(defmethod initialize-instance :after ((self cosinus-bp-shape) &rest initargs)
   (set-interpolation-fn self 'cosinus-interpolation)
   self )

(defclass staircase-bp-shape (interpolated-bp-shape)())

(defmethod initialize-instance :after ((self staircase-bp-shape) &rest initargs)
   (set-interpolation-fn self 'staircase-interpolation)
   self )

(defun linear-interpolation (x x1 y1 x2 y2)
   (infix ((y1 - y2) * x / (x1 - x2)
           + y2
           - (y1 - y2) * x2 / (x1 - x2) )))

;(defun cosinus-interpolation (x x1 y1 x2 y2)
;   (+ y1 (* (- y2 y1) (cos (/ (* (- x x1) pi) (- x2 x1))))) )

(defun cosinus-interpolation (x x1 y1 x2 y2)
         (+ (/ (+ y1 y2) 2.)
                  (* (/ (- y1 y2) 2.)
                     (cos (/ (* (- x x1) pi) (- x2 x1))) )) )

(defun staircase-interpolation (x x1 y1 x2 y2)
         x1 )
;;-------------------------------------
;; DM stuff
;;-------------------------------------

;; not mac and pc compatible?????
;;to be used only within rulemacros in DM
(defun this-note-make-time-shape (&key (interpolation :cosinus) (y-max 1000) (y-min -1000) break-point-list)
  (let ((tshape
   (make-instance 
     (case interpolation
       (:cosinus 'cosinus-bp-shape)
       (:linear 'linear-bp-shape)
       (:staircase 'staircase-bp-shape))
     :dependee *this-note*
     :x-max #'(lambda (self) (get-var (get-dependee self) 'dr)) 
     :y-max y-max ;default maximum y value, fix for skipping y scaling
     :y-min y-min ;default minimum y value, fix for skipping y scaling
     )))
    (when break-point-list (insert-break-point-list tshape break-point-list))
    tshape ))

(defun this-segment-make-time-shape (&key (interpolation :cosinus) (y-max 1000) (y-min -1000) break-point-list)
  (let ((tshape
   (make-instance 
     (case interpolation
       (:cosinus 'cosinus-bp-shape)
       (:linear 'linear-bp-shape)
       (:staircase 'staircase-bp-shape))
     :dependee *this-segment*
     :x-max #'(lambda (self) (get-var (get-dependee self) 'dr)) 
     :y-max y-max ;default maximum y value, fix for skipping y scaling
     :y-min y-min ;default minimum y value, fix for skipping y scaling
     )))
    (when break-point-list (insert-break-point-list tshape break-point-list))
    tshape ))

;;-------------------------------------
;; testing
;;-------------------------------------

#| testing testing
(defun foo ()
    (each-note
      (set-this 'va (this-note-make-time-shape))
      (set-y-max (this 'va) 100)
      (insert-break-point (this 'va) 0 0)
      (insert-break-point (this 'va) 20 50)
      (insert-break-point (this 'va) 40 40)
      (insert-break-point (this 'va) (* (this 'dr) 0.8) 30)
      (insert-break-point (this 'va) (this 'dr) 0)
     ))

(defun foo ()
    (each-note
     (set-this 'va (this-note-make-time-shape 
                    :interpolation :cosinus
                    :y-max 100
                    :y-min 0
                    :break-point-list
                    (list 0 0 (this 'dr) 50) ))
     ))

(defun bar ()
    (each-note
     (ADD-BP-SHAPE
                (this 'va)
      (this-note-make-time-shape 
       :interpolation :linear
       :y-max 100
       :y-min 0
       :break-point-list
       (list 0 0 (* (this 'dr) 0.2) 50
                          (this 'dr) 0)
        ))
     ))


(defun foo ()
    (each-note
      (set-this 'va (this-note-make-time-shape))
      (set-y-max (this 'va) 100)
      (insert-break-point (this 'va) 0 0)
      (insert-break-point (this 'va) (/ (this 'dr) 2.0) 50)
      (insert-break-point (this 'va) (this 'dr) 0)
      (set-this 'vf 5.5)
      ))
(defun foo ()
    (each-note
      (set-this 'va (this-note-make-time-shape))
      (set-y-max (this 'va) 100)
      (insert-break-point (this 'va) 0 0)
      (insert-break-point (this 'va) (this 'dr) 50)
     ))
(defun foo ()
  (each-note-if
   (not (this 'rest))
   (> (this 'dr) 500)
   (then
      (set-this 'va (this-note-make-time-shape))
      (set-y-max (this 'va) 100)
      (insert-break-point (this 'va) 0 0)
      (insert-break-point (this 'va) 50 0)
      (insert-break-point (this 'va) 300 30)
      (insert-break-point (this 'va) (- (this 'dr) 150) 30)
      (insert-break-point (this 'va) (this 'dr) 0)
      (set-this 'vf 6)
    )))

(defun foo ()
  (each-note-if
   (not (this 'rest))
   (then
    (set-this 'vf 6)
    (cond 
     ((> (this 'dr) 1000)
      (set-this 'va (this-note-make-time-shape))
      (set-y-max (this 'va) 100)
      (insert-break-point (this 'va) 0 0)
      (insert-break-point (this 'va) 200 0)
      (insert-break-point (this 'va) 700 30)
      (insert-break-point (this 'va) (- (this 'dr) 150) 30)
      (insert-break-point (this 'va) (this 'dr) 0)
      )
     ((> (this 'dr) 500)
      (set-this 'va (this-note-make-time-shape))
      (set-y-max (this 'va) 100)
      (insert-break-point (this 'va) 0 0)
      (insert-break-point (this 'va) 20 0)
      (insert-break-point (this 'va) 400 30)
      (insert-break-point (this 'va) (- (this 'dr) 150) 30)
      (insert-break-point (this 'va) (this 'dr) 0)
      )
     
     ))))
   
   
   |#

#|
(defun sl-to-vol-smoothing (quant)
   (let ((max-sl 0))
      (each-note-if         ;get max sl
        (this 'sl)
        (then
         (if (> (this 'sl) max-sl) (setq max-sl (this 'sl))) ))
      (each-note-if         ;transfer sl to volume
        (this 'sl)
        (then
         (set-this 'vol (- (this 'sl) max-sl))
         ;(set-this 'sl 0)
         ))
      (each-note-if         ;smoothing
        (this 'vol)
        (not (last?))
        (next 'vol)
        (not (this 'phrase-end))
        (then
         (let ((env (this-note-make-time-shape :interpolation :linear)))
            (set-y-max env 10)
            (set-y-min env -64)
            (insert-break-point env 0 (this 'vol))
            (insert-break-point env (this 'dr)
              (+ (this 'vol) (* quant 0.7 (- (next 'vol) (this 'vol)))) )
            (set-this 'vol env)
            )))
     ))
|#

(defun sl-to-vol-smoothing (quant)
   (let ((max-sl 0))
      (each-note-if         ;get max sl
        (this 'sl)
        (then
         (if (> (this 'sl) max-sl) (setq max-sl (this 'sl))) ))
      (each-note-if         ;transfer sl to volume
        (this 'sl)
        (then
         (set-this :volume (- (this 'sl) max-sl))
         ;(set-this 'sl 0)
         ))
      (each-note-if         ;smoothing
        (this :volume)
        ;(not (last?))
        ;(next 'vol)
        ;(not (this 'phrase-end))
        (then
         (let ((env (this-note-make-time-shape :interpolation :linear)))
            (set-y-max env 25)
            (set-y-min env -64)
           (if (not (last?))
             (insert-break-point env 0 (this :volume)))
           (cond
            ((or (last-1?) (next 'phrase-end))
             (insert-break-point env (this 'dr)
                                 (+ (next :volume) (* 0.5 quant (- (next :volume) (this :volume))))))
            ((or (last?) (this 'phrase-end))
             (insert-break-point env 0 (+ (this :volume) (* 0.5 (- (this :volume) (prev :volume)))))
             (insert-break-point env (this 'dr)
                                 (+ (this :volume) (* 0.5 (- 1 quant) (this :volume) (prev :volume)))))
            (t (insert-break-point env (this 'dr)
               (+ (this :volume) (* quant (- (next :volume) (this :volume)))))))
           (set-this 'vol env)
           )))
     (rem-all :volume)
     ))


;; Including extrapolation at the edges
(defun sl-to-vol-smoothing-old (quant)
   (let ((max-sl 0))
      (each-note-if         ;get max sl
        (this 'sl)
        (then
         (if (> (this 'sl) max-sl) (setq max-sl (this 'sl))) ))
      (each-note-if         ;transfer sl to volume
        (this 'sl)
        (then
         (set-this :volume (- (this 'sl) max-sl))
         ;(set-this 'sl 0)
         ))
      (each-note-if         ;smoothing
        (this :volume)
        ;(not (last?))
        ;(next 'vol)
        ;(not (this 'phrase-end))
        (then
         (let ((env (this-note-make-time-shape :interpolation :linear)))
            (set-y-max env 25)
            (set-y-min env -64)
           (insert-break-point env 0 (this :volume))
           (if (or (last?) (this 'phrase-end))
             (insert-break-point env (this 'dr)
               (+ (this :volume) (* quant (- (* 2 (this :volume)) (prev :volume)))))
             (insert-break-point env (this 'dr)
               (+ (this :volume) (* quant (- (next :volume) (this :volume))))))
           (set-this 'vol env)
           )))
     (rem-all :volume)
     ))

;eof
