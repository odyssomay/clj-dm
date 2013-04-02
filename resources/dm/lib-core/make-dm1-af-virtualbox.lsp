;;
;; ****************************************
;; This file is the loader for Director Musices
;; Anders Friberg
;; ****************************************
;;


#+:mswindows
(proclaim '(optimize (speed 2) (safety 1) (space 1) (debug 3)))
#+(and :mswindows :allegro)
(eval-when (compile eval load) (require :streamc))

(defpackage :dm
  #+mcl (:use :common-lisp :ccl)
  #+(and :mswindows :allegro) (:use :cg :cl :excl)
  #+:lispworks (:add-use-defaults t) 
  #+:lispworks (:use "CAPI")
 )

(in-package :dm)


(defun translate-logical-pathnames-PD ()
   #+mcl
   (setf (logical-pathname-translations "dm") `(("**;*.*.*" #P"musperf:dm:**:*.*")))
   #+:mswindows
   (setf (logical-pathname-translations "dm") `(("**;*.*.*" "c:\\af\\dm-sync\\dm-source\\**\\*.*"))))


(translate-logical-pathnames-PD)

(load "dm:lib-core;make-dm2.lsp")

;;uncomment for creating a stand-alone application in LispWorks
;;should be called from command window
;;lispworks-5-0-0-x86-win32.exe -init make-dm1.lsp

;(deliver 'display-main-window "Dirmustest" 0 :interface :capi :keep-eval t :keep-lisp-reader t)
;(quit)