;;;-*-Mode: LISP; Package: DM -*-
;;
;; *********************************
;;   some macros; see if necessary
;; *********************************

(in-package :dm)

#-:allegro
(defmacro while (test &rest body)
 `(loop while ,test do ,@body) )

#-:allegro
(defmacro until (test &rest body)
 `(loop until ,test do ,@body) )

;;
;; -------------
;;   UNTILEXIT
;; -------------
;;
(defmacro untilexit (tag &rest body)
 `(block ,tag
    (loop
    ,@body )))

;;; -------
;;;   IFN
;;; -------
;;;
     ;already defined in ACL? --no
 (defmacro ifn (test then &optional else)
 `(if (not ,test) ,then ,else) )
 
;; --------
;;   NEWR
;; --------
;;
(defmacro newr (l element)
  `(if (not ,l)
     (setq ,l (list ,element))
     (rplacd (last ,l) (list ,element))))

;; -----------
;;   APPEND1
;; -----------
;;
(defmacro append1 (list element)
  `(append ,list (list ,element)) )

;; ------------
;;   PRINT-LL
;; ------------
;;
;; print a list in New Line separated format
;;
(defun print-ll (&rest body)
  (progn
    (terpri)
    (dolist (one body) (princ one))))

;; ------------
;;   PRIN1-LL
;; ------------
;;
;; some as before, but without NewLine after each atom
;;
(defun prin1-ll (&rest body)
  (progn
    (dolist (one body) (princ one))))


