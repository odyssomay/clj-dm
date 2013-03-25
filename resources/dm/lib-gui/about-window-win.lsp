;;; -*- lisp-version: "5.0 [Windows/x86] (11/2/98 10:51)" common-graphics: "1.323.2.84" form: about-window-*-
;;;
;;; Define :about-window
 
(in-package :dm)


(defun display-about-window ()
  (pop-up-modal-dialog (make-about-window)) )
 
(defun make-about-window ()
         (make-window :about-window
           :parent *dm-main-window*
           :device 'dialog
           :exterior (make-box-relative 
                  (round (- (/ (width (screen *system*)) 2.0) 156))
                  (round (- (/ (height (screen *system*)) 2.0) 209))
                  311 418)
           :border :dialog-box
           :close-button t
           :cursor-name :arrow-cursor
           :maximize-button nil
           :minimize-button nil
           :name :about-window
           :pop-up t
           :resizable nil
           :scrollbars nil
           :state :shrunk
           :status-bar nil
           :system-menu t
           :title "About Director Musices"
           :title-bar t
           :toolbar nil
           :help-string nil
           :widgets
           (list (make-instance 'default-button
                   :left 120
                   :name :ok-button
                   :title "OK"
                   :top 328
                   :width 57
                   :on-change 
                   #'(lambda (widget old new) 
                       (return-t-from-pop-up-dialog widget old new) t))
                 (make-instance 'static-picture
                   :height 392
                   :name :static-picture-1
                   :pixmap-name nil ;:dmstartup
                   :pixmap-source
                   (if *demo-version*
                      (concatenate-pathnames *dm-home-directory* "dmstartup.bmp")
                      (translate-logical-pathname "dm:pcexe;dmstartup.bmp") )
                   :pixmap-use-handle nil
                   :stretching nil
                   :unavailable-color-mapper
                   (list (cons black gray) (cons dark-gray gray)
                         (cons dark-blue gray) (cons dark-green gray)
                         (cons dark-red gray) (cons dark-cyan gray)
                         (cons dark-yellow gray)
                         (cons dark-magenta gray) (cons red dark-red)
                         (cons green dark-green) (cons blue dark-blue)
                         (cons yellow dark-yellow)
                         (cons cyan dark-cyan)
                         (cons magenta dark-magenta))
                   :width 304
                   :font '#.(make-font-ex nil :|MS SANS SERIF| 11 nil)))
           ))
