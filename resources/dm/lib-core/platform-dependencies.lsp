;;;-*-Mode: LISP; Package: DM -*-
;;
;; ********************************************************************
;;   This file is supposed to solve some of the platform dependencies
;;   between Mac and PC
;; ********************************************************************
;; 
;;  show-dialog-for-opening-files-PD (message &key directory extensions) 
;;  show-dialog-for-saving-files-PD (message &key directory extensions)
;;  set-platform-dependant-variables-PD ()
;;  load-utilities-PD
;;  advice (msg &key (mode 'win)(title 'Message))

(in-package :dm)

;; ---------------------------------
;;   SHOW-DIALOG-FOR-OPENING-FILES
;; ---------------------------------


(defun show-dialog-for-opening-files-PD (message &key directory extensions mac-file-type)
  #+:MCL
  (declare (ignore extensions))
  ;(print directory)
  #+:MCL
   (choose-file-dialog  
    :mac-file-type  mac-file-type
    :button-string message
    :directory nil)
  #+(and :mswindows :allegro)
  (ask-user-for-existing-pathname 
   message
   :host (if (pathnamep directory) (namestring directory) directory)
   :allowed-types extensions
   :change-current-directory-p t
   :share-aware-p t)
  #+:lispworks
  (declare (ignore extensions))
  #+:lispworks
  (capi:prompt-for-file 
   message
   :pathname directory
   ;:filters extensions ;doesnt work, the list format different
  )
  )


;; --------------------------------
;;   SHOW-DIALOG-FOR-SAVING-FILES
;; --------------------------------
;;
;; please note that the message string is required
;;
; FOR MAC - TO BE REMOVED
;   (setq *save-definitions* (if (get-dm-var 'demo-version) nil t)
;      *save-local-symbols* (if (get-dm-var 'demo-version) nil t)
;      *fasl-save-local-symbols* (if (get-dm-var 'demo-version) nil t)
;      *print-case* :downcase
;      *paste-with-styles* nil)
;

(defun show-dialog-for-saving-files-PD (message &key directory extensions)
   #+:MCL
   (declare (ignore extensions))
   ;(print directory)
   #+:MCL
   (choose-new-file-dialog :directory nil :prompt message)
   #+(and :mswindows :allegro)
   (ask-user-for-new-pathname message
    :host  (if directory (directory-namestring directory) nil)
    :initial-name (if directory (file-namestring directory) nil)
    :allowed-types extensions )
  #+:lispworks
  (declare (ignore extensions))
  #+:lispworks
  (capi:prompt-for-file 
   message
   :operation :save
   :pathname directory
   )
  )



;; ----------
;;   ADVICE
;; ----------
;;
;; general purpose function for message output and printing
;; both at debugging and run time
;;
(defun advice (msg &key (mode 'win)(title "Message"))
   #+:MCL
   (declare (ignore title))
   
   (when (get-dm-var 'verbose)
      (cond ((eql mode 'win)
             #+(and :allegro :mswindows)
             (pop-up-message-dialog *dm-main-window*
               title msg warning-icon "OK")
             #+:MCL
             (print msg) 
             #+:lispworks
             (display-message msg) 
             )
            ((eql mode 'txt)
             (print msg))
            (t
              (print "Wrong mode in advice function")))))
           
          

;; ----------
;;   with-waiting-cursor
;; ----------
;;
;; general purpose macro for displaying a waiting cursor
;;
#+(and :mswindows :allegro)
(defmacro with-waiting-cursor (&rest body)
 `(with-hourglass
    ,@body ))
#+:MCL
(defmacro with-waiting-cursor (&rest body)
 `(with-cursor *watch-cursor*
    ,@body ))
#+:lispworks
(defmacro with-waiting-cursor (&rest body)
 `(progn
    ,@body ))

