;;;-*-Mode: LISP; Package: DM -*-
;;
;; **********************************
;;   
;; **********************************
;;
;; 9801 /Vittorio Colombo

(in-package :dm)


;;=======================
;;  CLASS dm-var-dialog
;;=======================
;;
(defclass dm-var-dialog (dialog)
    ()
   )


;;----------------------
;;  MAKE-DM-VAR-DIALOG
;;----------------------
;;
;; Create an instance of the window.
;;
(defun make-dm-var-dialog ()
   (make-window 'dm-var-dialog 
     :parent *dm-main-window*
     :device 'dialog
     :pop-up nil
     :close-button t
     :maximize-button t
     :minimize-button t
     :pop-up nil
     :overlapped *overlapped-windows*
     :resizable t
     :scrollbars nil
     :title "DM variables" 
     :font (make-font :swiss :system 16 '(:bold)) 
     :window-state :normal
     :window-border :frame 
     :background-color light-gray 
     :window-interior (make-box 309 26 555 425)
     :widgets (list      
                (make-instance 'static-text 
                  :name :static-text 
                  :title nil 
                  :value "Select a variable"
                  :box (make-box 4 5 242 24) 
                  :font (make-font-ex :swiss :arial 13 nil))
                (make-instance 'single-item-list 
                  :name :lst-var 
                  :title nil
                  :box (make-box 4 28 242 220) 
                  :right-attachment :right 
                  :key 'capitalize-object 
                  :range (sort (mapcar #'clos:slot-definition-name
                                 (clos:class-slots (find-class 'environment-settings))) #'string-lessp)
                  :on-change #'(lambda (widget new old)
                                    ;(set-dialog-fields widget
                                    (set-dialog-fields (parent widget) 
                                      :edit (prin1-to-string (get-dm-var new))                                      :static new)
                                    (values t nil)  ; ***** remove ?
                                    )
                  :tabstop t
                  :groupstart t 
                  :font (make-font-ex :swiss :ms\ sans\ serif 12 nil))
                (make-instance 'static-text
                  :name :txt1
                  :value "Variable"
                  :box (make-box 4 230 78 256)
                  :font (make-font-ex :modern :courier\ new 12 nil))
                (make-instance 'editable-text 
                  :name :static
                  :read-only t
                  :scrollable t
                  :right-attachment :right
                  :left-attachement :left
                  :value nil
                  :box (make-box 82 230 242 256) 
                  :font (make-font-ex :swiss :arial 12 nil))
                (make-instance 'multi-line-editable-text
                  :name :edit
                  :value ""
                  :scrollable t
                  :box (make-box 4 262 242 357)
                  :right-attachment :right 
                  :bottom-attachment :bottom 
                  :font (make-font-ex :modern :courier\ new 12 nil))
                (make-instance 'button 
                  :name :button-modify 
                  :title "Set"
                  :box (make-box 4 366 66 388)
                  :right-attachment :left
                  :left-attachement :left
                  :top-attachment :bottom 
                  :bottom-attachment :bottom 
                  :font (make-font-ex :swiss :arial 13 nil)
                  :set-value-fn #'(lambda (widget new old)
                                    (let ((var (dialog-field (parent widget) :lst-var)))
                                       (set-DM-var var 
                                         (read-from-string (dialog-field (parent widget) :edit)))))
                  :tab-control nil)
;;;                 (make-instance 'button 
;;;                   :name :button-close 
;;;                   :title "Close"
;;;                   :box (make-box 180 366 242 388)
;;;                   :right-attachment :right
;;;                   :left-attachment :right 
;;;                   :top-attachment :bottom
;;;                   :bottom-attachment :bottom 
;;;                   :set-value-fn #'(lambda (widget new old)
;;;                                     (close (parent widget)))
;;;                   :font (make-font-ex :swiss :arial 13 nil))
                )
     
     ;))


   ;(set-dialog-field name :lst-var 'All-rules)   ; I would like to select the first item...
   
   ;(list-widget-set-index (widget :lst-var name) 1 nil)
   
   
   ))

