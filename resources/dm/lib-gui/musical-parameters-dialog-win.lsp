;;;-*-Mode: LISP; Package: DM -*-
;;
;; **********************************
;;   loading and setting the system 
;; **********************************

(in-package :dm)

;; ===================================
;;   CLASS musical-parameters-dialog
;; ===================================
;;
(defclass musical-parameters-dialog (dialog)
    ())


(defun make-musical-parameters-dialog (&key (parent *dm-main-window*) 
                                 (window-interior 
                                   (make-box 73 52 545 254))
                                 (name :musical-parameters-dialog) 
                                 (title "Musical parameters dialog"))
   (open-dialog 
       ()
     'musical-parameters-dialog parent 
     :name name 
     :title title 
     :font (make-font :swiss :system 16 '(:bold)) 
     :window-state :normal 
     :window-border :frame 
     :left-attachment nil 
     :top-attachment nil 
     :right-attachment nil 
     :bottom-attachment nil 
     :user-movable t 
     :user-resizable nil 
     :user-closable t 
     :user-shrinkable t 
     :user-scrollable nil 
     :overlapped nil 
     :background-color light-gray 
     :pop-up-p nil 
     :window-interior window-interior)
 )
   
