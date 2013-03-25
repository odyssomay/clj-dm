;;;-*-Mode: LISP; Package: DM -*-
;;
;; ****************************************************************
;;   displays a dialog before loading midifiles
;; ****************************************************************
;; 4/10/2000 Anders Friberg

(in-package :dm)

;; -------------------
;;  SET-TEMPO-DIALOG
;; -------------------
;;
(defun midifile-input-dialog ()
  (let ((w
         (make-instance 'dialog
           :window-type :document 
           :window-title "MIDIfile input processing"
           :view-position :centered
           :close-box-p nil
           :view-size #@(200 120)
           :view-font '("Chicago" 12 :srcor :plain)
           :view-subviews
           (list 
            (make-dialog-item 'editable-text-dialog-item
                              #@(13 13)
                              #@(30 15)
                              (princ-to-string (get-dm-var 'midifile-input-articulation-threshold))
                              nil
                              :draw-outline t
                              ;:view-font '("Geneva" 9 :srccopy :plain)
                              :allow-returns nil
                              :view-nick-name 'articulation-threshold)
            (make-dialog-item 'static-text-dialog-item
                              #@(50 13)
                              #@(200 16)
                              "Articulation threshold"
                              nil)
            (make-dialog-item 'check-box-dialog-item
                              #@(12 45)
                              #@(155 22)
                              "Quantize durations"
                              nil
                              :check-box-checked-p (get-dm-var 'midifile-input-quantize-dr)
                              :view-nick-name 'quantize-durations)
            (make-dialog-item 'button-dialog-item
                              #@(30 95)
                              #@(60 18)
                              "Cancel"
                              #'(lambda
                                  (item)
                                  item
                                  (return-from-modal-dialog nil))
                              :default-button nil)
            (make-dialog-item 'button-dialog-item
                              #@(100 95)
                              #@(60 18)
                              "OK"
                              #'(lambda (item)
                                  item
                                  (let ((new-threshold 
                                         (read-from-string 
                                          (dialog-item-text 
                                           (find-named-sibling item 'articulation-threshold)))))
                                    (cond ((or (not (numberp new-threshold))
                                               (> new-threshold 1)
                                               (< new-threshold 0) )                                
                                           (message-dialog "Enter a number between 0.0 and 1.0"))
                                          (t (set-dm-var 'midifile-input-articulation-threshold new-threshold)
                                             (set-dm-var 'midifile-input-quantize-dr 
                                                         (check-box-checked-p 
                                                          (find-named-sibling item 'quantize-durations)))
                                             (return-from-modal-dialog t)
                                             ))))
                              :default-button t)
            ))))
    (modal-dialog w) ))
