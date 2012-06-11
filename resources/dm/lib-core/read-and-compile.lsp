;;;-*-Mode: LISP; Package: DM -*-
;;
;; ***********************************************************
;; ***********************************************************
;; 

;;; this file should merge with utilities, environment or something like that
;;;
;;;  concatenate-pathnames
;;;  load-last-version
;;;

(in-package :dm)

;; -------------------------
;;   CONCATENATE PATHNAMES
;; -------------------------
;;
(defun concatenate-pathnames (path1 path2)
  (pathname
   (concatenate 'string
                (namestring path1)(namestring path2))))

;; ---------------------
;;   LOAD LAST VERSION
;; ---------------------
;;

#+:mswindows
(defun load-last-version (pathname)
  (if
    (or
     (not (probe-file (concatenate 'string pathname *.fasl-pathname*)))
     (<  (file-write-date (concatenate 'string pathname *.fasl-pathname*))
         (file-write-date (concatenate 'string pathname *.lisp-pathname*)) ))
    (load (concatenate 'string pathname *.lisp-pathname*))
    (load (concatenate 'string pathname *.fasl-pathname*))
     ))

#+:MCL
(defun load-last-version (pathname)
  (if
    (or
     (not (probe-file (namestring (merge-pathnames *.fasl-pathname* pathname))))
     (<  (file-write-date (namestring (merge-pathnames *.fasl-pathname* pathname)))
         (file-write-date (namestring (merge-pathnames *.lisp-pathname* pathname))) ))
    (load (namestring (merge-pathnames *.lisp-pathname* pathname)))
    (load (namestring (merge-pathnames *.fasl-pathname* pathname)))
     ))


;;---------------------------------------------------
;; compiling
;;---------------------------------------------------

;compiles all files that has been changed
(defun compile-dm ()
 (dolist (file *dm-def-files*)  ;load the system
  (compile-file-if-old file)) )

;only compile if the file changed after last compilation
(defun compile-file-if-old (pathname)
   (if
       (or
           (not (probe-file (namestring (merge-pathnames *.fasl-pathname* pathname))))
           (<  (file-write-date (namestring (merge-pathnames *.fasl-pathname* pathname)))
              (file-write-date (namestring (merge-pathnames *.lisp-pathname* pathname))) ))
      (compile-file (namestring (merge-pathnames *.lisp-pathname* pathname)) :verbose t) ))

;compiles all files
(defun recompile-dm ()
  (dolist (pathname *dm-def-files*)  ;load the system
     (compile-file (namestring (merge-pathnames *.lisp-pathname* pathname)) :verbose t)) )

;;---------------------------------------------------
;; file loading
;;---------------------------------------------------

(defun load-files-if-changed-dm ()
   (dolist (file *dm-def-files*)  ;load the system
      (load-file-if-changed file)) )


;load file if changed and not compiled
;; the filename must be exactly as in the list (get-dm-var 'dm-def-files)
;; including case
(defun load-file-if-changed (pathname)
   (if
       (or
           (not (probe-file (namestring (merge-pathnames *.fasl-pathname* pathname))))
           (<  (file-write-date (namestring (merge-pathnames *.fasl-pathname* pathname)))
              (file-write-date (namestring (merge-pathnames *.lisp-pathname* pathname))) ))
      (load (namestring (merge-pathnames *.lisp-pathname* pathname)) :verbose t) ))

;;---------------------------------------------------
;; dm standalone saving
;;---------------------------------------------------

#+:MCL
(defun save-dm ()
 (compile-dm)
 (if (not (member 'midi-open *restore-lisp-functions*))
   (setq *restore-lisp-functions* (append *restore-lisp-functions* (list 'midi-open))))
 (if (not (member 'set-dm-directories *restore-lisp-functions*))
   (setq *restore-lisp-functions* (append *restore-lisp-functions* (list 'set-dm-directories))))
 (if (not (member 'load-files-if-changed-dm *restore-lisp-functions*))
   (setq *restore-lisp-functions* (append *restore-lisp-functions* (list 'load-files-if-changed-dm))))
 (if (not (member 'load-preferences *restore-lisp-functions*))
   (setq *restore-lisp-functions* (append *restore-lisp-functions* (list 'load-preferences))))
  ;(if (not (member 'move-listener-window *restore-lisp-functions*))
  ;  (setq *restore-lisp-functions* (append *restore-lisp-functions* (list 'move-listener-window))))
 (save-application
  (concatenate-pathnames "home:" "Director Musices")
  ))

#+:MCL
(defun save-dm ()
 (compile-dm)
 (if (not (member 'midi-open *restore-lisp-functions*))
   (setq *restore-lisp-functions* (append *restore-lisp-functions* (list 'midi-open))))
 (if (not (member 'load-files-if-changed-dm *restore-lisp-functions*))
   (setq *restore-lisp-functions* (append *restore-lisp-functions* (list 'load-files-if-changed-dm))))
 ;(if (not (member 'load-preferences *restore-lisp-functions*))
  ; (setq *restore-lisp-functions* (append *restore-lisp-functions* (list 'load-preferences))))
  ;(if (not (member 'move-listener-window *restore-lisp-functions*))
  ;  (setq *restore-lisp-functions* (append *restore-lisp-functions* (list 'move-listener-window))))
 (save-application
  (concatenate-pathnames "home:" "Director Musices 2a")
  ))
 

#+:MCL
(defun save-dm-demo ()
  (set-dm-var 'lib-directory (concatenate-pathnames "home:" "Lib;"))
  (set-dm-var 'rules-directory (concatenate-pathnames "home:" "Rules;"))
  (set-dm-var 'rule-sets-directory (concatenate-pathnames "home:" "Rulepalettes;"))
  (set-dm-var 'music-directory (concatenate-pathnames "home:" "scores;"))
  (if (find-menu "tools") (menu-deinstall (find-menu "tools")))
  (if (find-menu "Lisp") (menu-deinstall (find-menu "Lisp")))
  ;(if (not (member 'move-listener-window *restore-lisp-functions*))
  ;  (setq *restore-lisp-functions* (append *restore-lisp-functions* (list 'move-listener-window))))
  (if (not (member 'midi-open *restore-lisp-functions*))
    (setq *restore-lisp-functions* (append *restore-lisp-functions* (list 'midi-open))))
  (if (not (member 'set-dm-directories *restore-lisp-functions*))
    (setq *restore-lisp-functions* (append *restore-lisp-functions* (list 'set-dm-directories))))
  (if (not (member 'load-preferences *restore-lisp-functions*))
    (setq *restore-lisp-functions* (append *restore-lisp-functions* (list 'load-preferences))))
 ; (if (not (member 'dm-demo-open-windows *restore-lisp-functions*))
 ;   (setq *restore-lisp-functions* (append *restore-lisp-functions* (list 'dm-demo-open-windows))))
  (save-application "home:Director Musices Demo 2.1a" :excise-compiler t)
   )
#+:MCL(defun save-dm-demo ()
  (if (find-menu "tools") (menu-deinstall (find-menu "tools")))
  (if (find-menu "Lisp") (menu-deinstall (find-menu "Lisp")))
  (if (not (member 'install-midishare-interface *restore-lisp-functions*))
    (setq *restore-lisp-functions* (append *restore-lisp-functions* (list 'install-midishare-interface))))
  (if (not (member 'midi-open *restore-lisp-functions*))
    (setq *restore-lisp-functions* (append *restore-lisp-functions* (list 'midi-open))))
 ; (if (not (member 'dm-demo-open-windows *restore-lisp-functions*))
 ;   (setq *restore-lisp-functions* (append *restore-lisp-functions* (list 'dm-demo-open-windows))))
  (save-application "home:Director Musices Demo 2.4a" :excise-compiler t)
   )
#+:MCL
(defun dm-demo-open-windows ()
 ;(load-music-fpath "monophonic;ekor.mus")
 ;(rdraw:draw-ddr%)
 (open-default-rule-set)
 (princ "Welcome to Director Musices Demo 2.1a")
 )


#+:mswindows
(defun save-dm-image ()
  (if (not (member 'midi-open *session-init-fns*))
      (setq *session-init-fns* (append *session-init-fns* (list 'midi-open))) )
  (if (not (member 'install-music-menus *session-init-fns*))
      (setq *session-init-fns* (append *session-init-fns* (list 'install-music-menus))) )
   (save-image) )

;;eof