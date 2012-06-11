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
  (setf (logical-pathname-translations "dm") `(("**;*.*.*" "Y:\\dept\\tmh\\music\\proj\\musperf\\dm\\dm-source\\**\\*.*")))  ; new win7
  ;;;(setf (logical-pathname-translations "dm") `(("**;*.*.*" "J:\\nada.kth.se\\dept\\tmh\\music\\proj\\musperf\\dm\\**\\*.*")))
   #+:lispworks ;mac
   (setf (logical-pathname-translations "dm") `(("**;*.*.*" "/afs/nada.kth.se/dept/tmh/music/proj/musperf/dm/**/*.*"))))


(translate-logical-pathnames-PD)

(load "dm:lib-core;make-dm2.lsp")

;;uncomment for creating a stand-alone application in LispWorks
;;should be called from command window
;;lispworks-5-0-0-x86-win32.exe -init make-dm1.lsp

;(deliver 'display-main-window "Dirmustest" 0 :interface :capi :keep-eval t :keep-lisp-reader t)
;(quit)