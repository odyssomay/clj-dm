;;; -*- lisp-version: "5.0 [Windows/x86] (8/29/98 11:02)" common-graphics: "1.323.2.84" form: form2-*-
;;;
;;; Define :form2
 
(in-package :dm)

;;;----------------------------------
;;; draw-ask-for-vars
;;;----------------------------------

(defun draw-ask-for-vars ()
   (pop-up-modal-dialog (make-draw-select-vars-window)) )

(defun make-draw-select-vars-window
    (&key (parent *dm-main-window*)
     (exterior (make-box 335 121 653 518)) (name :draw-select-vars-window)
     (title "Select variables") form-p)
  (let ((parent
         (make-window name
           :parent parent
           :device 'dialog
           :widgets
           (list (make-instance 'default-button
                   :height 24
                   :left 226
                   :name :ok-button
                   :on-change 
                   #'(lambda (widget old new)
                       (let
                        ((my-scrolle (my-scrollee (find-window 'music-window *dm-main-window* :owned-p t)))
                         (notation-val (value (find-sibling :notation-var-list widget)))
                         (perf-val (value (find-sibling :performance-var-list widget)))
                         (own-val-1 (value (find-sibling :own-var-1 widget)))
                         (own-val-2 (value (find-sibling :own-var-2 widget)))
                         (own-val-3 (value (find-sibling :own-var-3 widget))) )
                        (setf (selected-notation-vars my-scrolle) notation-val)
                        (setf (selected-performance-vars my-scrolle) perf-val)
                        (setf (selected-own-var-1 my-scrolle) own-val-1)
                        (setf (selected-own-var-2 my-scrolle) own-val-2)
                        (setf (selected-own-var-3 my-scrolle) own-val-3)
                        (setf (selected-vars my-scrolle)
                              (append
                               notation-val
                               perf-val
                               (if (string-equal "" own-val-1) nil (list (read-from-string own-val-1)))
                               (if (string-equal "" own-val-2) nil (list (read-from-string own-val-2)))
                               (if (string-equal "" own-val-3) nil (list (read-from-string own-val-3)))
                               ))
                        (redisplay-window my-scrolle) )
                       (return-t-from-pop-up-dialog widget old new)
                                       t)
                   :title "OK"
                   :top 336
                   :width 69)
                 (make-instance 'cancel-button
                   :height 27
                   :left 225
                   :name :cancel-button-1
                   :top 296
                   :width 70)
                 (make-instance 'editable-text
                   :height 25
                   :left 224
                   :name :own-var-3
                   :on-change nil
                   :template-string nil
                   :top 88
                   :value (selected-own-var-3 
                            (my-scrollee (find-window 'music-window *dm-main-window* :owned-p t)))
                   :width 72
                   :up-down-control nil)
                 (make-instance 'editable-text
                   :height 25
                   :left 224
                   :name :own-var-2
                   :template-string nil
                   :top 56
                   :value (selected-own-var-2 
                            (my-scrollee (find-window 'music-window *dm-main-window* :owned-p t)))
                   :width 72
                   :up-down-control nil)
                 (make-instance 'editable-text
                   :height 25
                   :left 224
                   :name :own-var-1
                   :template-string nil
                   :top 24
                   :value (selected-own-var-1 
                            (my-scrollee (find-window 'music-window *dm-main-window* :owned-p t)))
                   :width 72)
                 (make-instance 'static-text
                   :foreground-color dark-blue
                   :height 18
                   :left 224
                   :name :static-text-8
                   :top 6
                   :value "Other"
                   :width 88
                   :scrollbars nil)
                 (make-instance 'multi-item-list
                   :height 336
                   :left 16
                   :name :notation-var-list
                   :range (get-dm-var 'notation-var-list)
                   :tabs nil
                   :top 24
                   :value (selected-notation-vars 
                            (my-scrollee (find-window 'music-window *dm-main-window* :owned-p t)))
                   :width 90)
                 (make-instance 'multi-item-list
                   :height 336
                   :left 119
                   :name :performance-var-list
                   :range (get-dm-var 'performance-var-list)
                   :tabs nil
                   :top 24
                   :value (selected-performance-vars 
                           (my-scrollee (find-window 'music-window *dm-main-window* :owned-p t)))
                   :width 90)
                 (make-instance 'static-text
                   :foreground-color dark-blue
                   :height 18
                   :left 119
                   :name :static-text-7
                   :top 6
                   :value "Performance"
                   :width 88
                   :scrollbars nil)
                 (make-instance 'static-text
                   :foreground-color dark-blue
                   :height 18
                   :left 16
                   :name :static-text-2
                   :top 6
                   :value "Score"
                   :width 88
                   :scrollbars nil))
           :exterior exterior
           :border :frame
           :close-button t
           :cursor-name :arrow-cursor
           :maximize-button nil
           :minimize-button nil
           :name :form2
           :package-name :common-graphics-user
           :pop-up t
           :resizable nil
           :scrollbars nil
           :state :shrunk
           :status-bar nil
           :system-menu nil
           :title title
           :title-bar t
           :toolbar nil
           :form-p form-p
           )))
    parent))


;;;----------------------------------
;;; draw-edit-segment-vars
;;;----------------------------------

(defun draw-edit-segment-vars (segment)
   (pop-up-modal-dialog (make-draw-edit-segment-vars-window segment)) )

(defun make-draw-edit-segment-vars-window 
    (segment &key (parent *dm-main-window*)
             (exterior (make-box 100 100 470 283)) ;335 121 653 518
             (name :draw-edit-segment-vars-window)
     (title "Edit segment variables") form-p)
  (let ((parent
         (make-window name
           :parent parent
           :device 'dialog
           :widgets
           (list (make-instance 'default-button
                   :height 27
                   :left 290
                   :name :ok-button
                   :on-change 
                   #'(lambda (widget old new)
                       (setf (var-list segment)
                             (list-to-alist
                              (read-from-string
                               (value (find-sibling :multi-line-editable-text-1 widget)))))
                       ;(print (value (find-sibling :multi-line-editable-text-1 widget)))
                        (redisplay-window (my-scrollee (find-window 'music-window *dm-main-window* :owned-p t))) 
                       (return-t-from-pop-up-dialog widget old new)
                                       t)
                   :title "OK"
                   :top 115
                   :width 70)
                 (make-instance 'cancel-button
                   :height 27
                   :left 215
                   :name :cancel-button-1
                   :top 115
                   :width 70)
                 (make-instance 'button
                   :height 27
                   :left 130
                   :name :delete-button-1
                   :top 115
                   :width 70
                   :title "Delete"
                   :on-click
                   #'(lambda (widget old new)
                       (setf (var-list segment)
                             (list-to-alist
                              (read-from-string
                               (value (find-sibling :multi-line-editable-text-1 widget)))))
                       ;(print (value (find-sibling :multi-line-editable-text-1 widget)))
                        (redisplay-window (my-scrollee (find-window 'music-window *dm-main-window* :owned-p t))) 
                       (return-t-from-pop-up-dialog widget old new)
                       t)
                   )       
                 (make-instance 'multi-line-editable-text
                   :height 90
                   :left 5
                   :name :multi-line-editable-text-1
                   :tabs nil
                   :template-string nil
                   :top 24
                   :font #.(make-font-ex :modern :|COURIER NEW| 12 nil)
                   :value (prin1-to-string (alist-to-list (var-list segment)))
                   :width 350)
                          
                 (make-instance 'static-text
                   :foreground-color dark-blue
                   :height 18
                   :left 16
                   :name :static-text-2
                   :top 6
                   :value "All segment vars"
                   :width 200
                   :scrollbars nil))
           :exterior exterior
           :border :frame
           :close-button t
           :cursor-name :arrow-cursor
           :maximize-button nil
           :minimize-button nil
           :name name
           :pop-up t
           :resizable nil
           :scrollbars nil
           :state :shrunk
           :status-bar nil
           :system-menu nil
           :title title
           :title-bar t
           :toolbar nil
           :form-p form-p
           )))
    parent))
