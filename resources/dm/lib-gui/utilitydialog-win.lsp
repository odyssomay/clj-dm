;; some score editing utilities
;; 981102/af first version acl5
;; 041109/af added remove-one-parameter *****should be included also in MAC version **************


(in-package :dm)

(defvar *utility-dialog-font*)
(setq *utility-dialog-font* (make-font-ex nil "MS Sans Serif" 15 nil))

;;;----------------------------------
;;; remove one paramter
;;;----------------------------------

(defun remove-one-parameter ()
  (multiple-value-bind (par second button) 
      (pop-up-string-dialog (screen *system*) "remove parameter" "select a note parameter to be removed from the whole score:"
                            'question-icon "nil" "OK" "Cancel")
    (print par)
    (if (= button 1) (rem-all (read-from-string par))) ))


;;;----------------------------------
;;; set-tempo-dialog
;;;----------------------------------

(defun set-tempo-dialog ()
   (pop-up-modal-dialog 
     (make-window :set-tempo-window
           :parent *dm-main-window*
           :device 'dialog
           :widgets
           (list (make-instance 'cancel-button
                   :left 128
                   :name :cancel-button
                   :font *utility-dialog-font*
                   :top 80
                   :width 59)
                 (make-instance 'default-button
                   :height 25
                   :left 128
                   :name :default-button
                   :font *utility-dialog-font*
                   :title "OK"
                   :top 120
                   :width 59
                   :on-change 
                   #'(lambda (widget old new)
                       (let ((new-mm (read-from-string (value (find-sibling :txtbeats widget)))))
                         (cond ((or (not (numberp new-mm))
                                    (<= new-mm 0.0) )
                                (advice "Enter a positive number") )
                               (t (set-tempo new-mm :init-dur? t)
                                  (redraw-music-windows) 
                                  (return-t-from-pop-up-dialog widget old new) )))
                       t)
                   )
                 (make-instance 'static-text
                   :height 14
                   :left 83
                   :name :static-text
                   :font *utility-dialog-font*
                   :top 22
                   :value "Beats per minute"
                   :width 120
                   :scrollbars nil)
                 (make-instance 'editable-text
                   :left 8
                   :name :txtbeats
                   :font *utility-dialog-font*
                   :template-string nil
                   :top 18
                   :value (princ-to-string (get-first 'mm))
                   :width 64))
           
           :exterior (make-box 383 190 589 377)
           :border :frame
           :close-button t
           :cursor-name :arrow-cursor
           :maximize-button nil
           :minimize-button nil
           :name :set-tempo-window
           :pop-up t
           :resizable nil
           :scrollbars nil
           :state :shrunk
           :status-bar nil
           :system-menu nil
           :title "Edit tempo"
           :title-bar t
           :toolbar nil
       )))
 
;;;----------------------------------
;;; change-octave-dialog
;;;----------------------------------

(defun change-octave-dialog ()
   (pop-up-modal-dialog 
     (make-window :change-octave-window
           :parent *dm-main-window*
           :device 'dialog
           :widgets
           (list (make-instance 'cancel-button
                   :left 128
                   :name :cancel-button
                   :font *utility-dialog-font*
                   :top 80
                   :width 59)
                 (make-instance 'default-button
                   :height 25
                   :left 128
                   :name :default-button
                   :font *utility-dialog-font*
                   :title "OK"
                   :top 120
                   :width 59
                   :on-change 
                   #'(lambda (widget old new)
                       (let ((octave (read-from-string (value (find-sibling :txtbeats widget)))))
                         (cond ((not (numberp octave))
                                (advice "Not a number") )
                               (t (trans-octave octave)
                                  (redraw-music-windows) 
                                  (return-t-from-pop-up-dialog widget old new) ))
                          t)) )
                 (make-instance 'static-text
                   :height 20
                   :left 83
                   :name :static-text
                   :font *utility-dialog-font*
                   :top 22
                   :value "Octaves up (down neg)"
                   :width 150
                   :scrollbars nil)
                 (make-instance 'editable-text
                   :left 8
                   :name :txtbeats
                   :font *utility-dialog-font*
                   :template-string nil
                   :top 18
                   :value "1"
                   :width 64))
           
           :exterior (make-box 383 190 630 377)
           :border :frame
           :close-button t
           :cursor-name :arrow-cursor
           :maximize-button nil
           :minimize-button nil
           :name :change-octave-window
           :pop-up t
           :resizable nil
           :scrollbars nil
           :state :shrunk
           :status-bar nil
           :system-menu nil
           :title "Change octave"
           :title-bar t
           :toolbar nil
       )))
 
;;;----------------------------------
;;; set-key-dialog
;;;----------------------------------

(defun set-key-dialog ()
   (pop-up-modal-dialog 
     (make-window :change-octave-window
           :parent *dm-main-window*
           :device 'dialog
           :widgets
           (list (make-instance 'cancel-button
                   :left 128
                   :name :cancel-button
                   :font *utility-dialog-font*
                   :top 80
                   :width 59)
                 (make-instance 'default-button
                   :height 25
                   :left 128
                   :name :default-button
                   :font *utility-dialog-font*
                   :title "OK"
                   :top 120
                   :width 59
                   :on-change 
                   #'(lambda (widget old new)
                       (set-first 'key (value (find-sibling :key widget)))
                       (set-first 'modus (value (find-sibling :modus widget)))
                          (redraw-music-windows) 
                          (return-t-from-pop-up-dialog widget old new)
                          t) )
                 (make-instance 'combo-box
                   :height 123
                   :left 16
                   :name :key
                   :font *utility-dialog-font*
                   :range *tones*
                   :top 16
                   :value (get-first 'key)
                   :on-change-test 'string=
                   :width 65)
                 (make-instance 'combo-box
                   :height 123
                   :left 16
                   :name :modus
                   :font *utility-dialog-font*
                   :range *modus*
                   :top 48
                   :value (get-first 'modus)
                   :on-change-test 'string=                   
                   :width 65)
                 )
           
           :exterior (make-box 383 190 589 377)
           :border :frame
           :close-button t
           :cursor-name :arrow-cursor
           :maximize-button nil
           :minimize-button nil
           :name :change-octave-window
           :pop-up t
           :resizable nil
           :scrollbars nil
           :state :shrunk
           :status-bar nil
           :system-menu nil
           :title "Set key"
           :title-bar t
           :toolbar nil
       )))
 

;;;----------------------------------
;;; set-meter-dialog
;;;----------------------------------

(defun set-meter-dialog ()
   (pop-up-modal-dialog 
     (make-window :set-meter-window
           :parent *dm-main-window*
           :device 'dialog
           :widgets
           (list (make-instance 'cancel-button
                   :left 128
                   :name :cancel-button
                   :font *utility-dialog-font*
                   :top 80
                   :width 59)
                 (make-instance 'default-button
                   :height 25
                   :left 128
                   :name :default-button
                   :font *utility-dialog-font*
                   :title "OK"
                   :top 120
                   :width 59
                   :on-change 
                   #'(lambda (widget old new)
                       (set-first 'meter 
                         (list (value (find-sibling :upper widget))
                               (value (find-sibling :lower widget)) ))
                       (rebar)
                          (redraw-music-windows) 
                          (return-t-from-pop-up-dialog widget old new)
                          t) )
                 (make-instance 'combo-box
                   :height 200
                   :left 16
                   :name :upper
                   :font *utility-dialog-font*
                   :range '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
                            16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32)
                   :top 16
                   :value (if (get-first 'meter) (first (get-first 'meter)) nil)
                   :width 65)
                 (make-instance 'combo-box
                   :height 123
                   :left 16
                   :name :lower
                   :font *utility-dialog-font*
                   :range '(1 2 4 8 16 32)
                   :top 48
                   :value (if (get-first 'meter) (second (get-first 'meter)) nil)
                   :width 65)
                 )
           
           :exterior (make-box 383 190 589 377)
           :border :frame
           :close-button t
           :cursor-name :arrow-cursor
           :maximize-button nil
           :minimize-button nil
           :name :set-meter-window
           :pop-up t
           :resizable nil
           :scrollbars nil
           :state :shrunk
           :status-bar nil
           :system-menu nil
           :title "Set meter"
           :title-bar t
           :toolbar nil
       )))

