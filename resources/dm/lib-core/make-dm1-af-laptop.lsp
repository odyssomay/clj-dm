;;
;; ****************************************
;; This file is the loader for Director Musices
;; Anders Friberg
;; ****************************************
;;

#+:mswindows
(proclaim '(optimize (speed 2) (safety 1) (space 1) (debug 3)))
#+:mswindows
(eval-when (compile eval load) (require :streamc))

;;;(defpackage :dm
;;;  #+mcl (:use :common-lisp :ccl)
;;;  #+:mswindows (:use :common-lisp-user :common-graphics :allegro :common-lisp))

;;;(defpackage :dm
;;;  #+mcl (:use :common-lisp :ccl)
;;;  #+:mswindows (:use :ide :cg :cl :excl) )

(defpackage :dm
  #+mcl (:use :common-lisp :ccl)
  #+:mswindows (:use :cg :cl :excl) )

(in-package :dm)

(defun translate-logical-pathnames-PD ()
   #+mcl
   (setf (logical-pathname-translations "dm") `(("**;*.*.*" #P"musperf:dm:**:*.*")))
   #+:mswindows
   (setf (logical-pathname-translations "dm") `(("**;*.*.*" "c:\\af\\dm\\**\\*.*"))))


(translate-logical-pathnames-PD)

(load "dm:lib-core;make-dm2.lsp")