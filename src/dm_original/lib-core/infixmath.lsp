;;;-*-Mode: LISP; Package: DM -*-
;;
;; ****************************************
;; infix from Winston & Horn p185
;; unary fkns must have parenthesises
;; as usual see ae-atom? for list of fkns
;;
;; /Anders Friberg  October 1987. 
;; ****************************************



(defpackage :infix
   #+mcl (:use :common-lisp :ccl)
   #+(and :mswindows :allegro) (:use :cg :cl :excl)
   #+:lispworks (:add-use-defaults t) 
   )

;(in-package :infix)
(in-package :dm)

;; -------------
;;   DM::INFIX
;; -------------
;;

;(defmacro dm::infix (list)
(defmacro infix (list)
  `(,@(inf-to-pre list)) )

;(defun foo ()(let ((dr 2)(i 3)) (infix (-19 / (sqrt dr) - 0.5))))


;; ---------------
;;    WEIGHT
;; ---------------
;;
(defun weight (op)
  (cond ((equal op '=) 0)
        ((equal op '+) 1)
        ((equal op '-) 1)
        ((equal op '*) 2)
        ((equal op '/) 2)
        (t (print `(,op not an operator)) 4)))

;; ---------------
;;    OPCODE
;; ---------------
;;
(defun opcode (op)
  (cond ((equal op '=) 'setq)
        ((equal op '+) '+)
        ((equal op '-) '-)
        ((equal op '*) '*)
        ((equal op '/) '/)
        (t (print `(,op not an operator)) 4)))

;(defun inf-to-pre (ae)
;  (cond ((atom ae) ae)
;        (t (inf-aux ae nil nil))))


;; ----------------
;;    INF-TO-PRE
;; ----------------
;;
(defun inf-to-pre (ae)
  (cond ((ae-atom? ae) ae)
        (t (inf-aux ae nil nil))))


;; --------------
;;    AE-ATOM
;; --------------
;;
(defun ae-atom? (ae)
   (if (or (atom ae)
           (equal (car ae) 'sqrt)
           (equal (car ae) 'this)
           (equal (car ae) 'next)
           (equal (car ae) 'prev) )
        ae
        nil
        ))

;  INF-AUX


;; ---------------
;;    INF-AUX
;; ---------------
;;
(defun inf-aux (ae operators operands)
   (inf-iter (cdr ae) operators (cons (inf-to-pre (car ae)) operands)))


;; ---------------
;;    INF-ITER
;; ---------------
;;
(defun inf-iter (ae operators operands)
  (cond ((and (null ae) (null operators))
         (car operands))
        ((and (not (null ae))
              (or (null operators)
                  (> (weight (car ae))
                     (weight (car operators)))))
         (inf-aux (cdr ae)
                   (cons (car ae) operators)
                   operands))
        (t (inf-iter ae
                      (cdr operators)
                      (cons (list (opcode (car operators))
                                  (cadr operands)
                                  (car operands))
                            (cddr operands))))))

