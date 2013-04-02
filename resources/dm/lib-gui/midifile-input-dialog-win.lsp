;;;-*-Mode: LISP; Package: DM -*-
;;
;; **********************************
;;   
;; **********************************
;;
;; 3/10/2000 Anders Friberg

(in-package :dm)


;;=======================
;;  CLASS midifile-input-dialog
;;=======================
;;
(defclass midifile-input-dialog (dialog) ())


;;----------------------
;;  midifile-input-dialog
;;----------------------
;;
;; Create an instance of the window.
;;
(defun midifile-input-dialog ()
  (pop-up-modal-dialog 
      (make-window 
          :midifile-input-dialog
        :parent *dm-main-window*
        :device 'dialog
        :widgets
        (list 
         (make-instance 'check-box
           :font (make-font-ex nil "Tahoma / ANSI" 14 nil)
           :left 10
           :name :quantize-durations
           :title "Quantize durations"
           :tooltip "Quantize durations to the divisions given below"
           :top 8
           :value (get-dm-var 'midifile-input-quantize-dr)
           :width 136)
         (make-instance 'combo-box :font
           (make-font-ex nil "Tahoma / ANSI" 14 nil)
           :height 144 :left 32 :name :notevalues
           :on-change-test 'equal :range
           '("1/2" "1/4" "1/8" "1/16" "1/32" "1/64") :top 33
           :value (princ-to-string (get-dm-var 'midifile-input-quantize-notevalues))
           :tooltip "Smallest quantize note value"
           :width 64)
         (make-instance 'combo-box :font
           (make-font-ex nil "Tahoma / ANSI" 14 nil)
           :height 144 :left 120 :name :triplet-notevalues
           :on-change-test 'equal :range
           '("1" "1/3" "1/6" "1/12" "1/24" "1/48") :top 33
           :value (princ-to-string (get-dm-var 'midifile-input-quantize-triplet-notevalues))
           :width 64
           :tooltip "Smallest triple quantize note value")
         (make-instance 'check-box :font
           (make-font-ex nil "Tahoma / ANSI" 14 nil)
           :height 25 :left 10 :name :split-at-barlines 
           :title "Split notes at barlines" :top 63 
           :value (get-dm-var 'midifile-input-quantize-split-at-barlines)
           :tooltip "Split notes and rests at barlines"
           :width 158)
         (make-instance 'editable-text
           :font (make-font-ex nil "Tahoma / ANSI" 14 nil)
           :height 25 :left 8 :top 95 :width 48
           :name :articulation-threshold
           :template-string nil
           :up-down-control nil
           :value (princ-to-string (get-dm-var 'midifile-input-articulation-threshold))
           :tooltip "percent of interonset duration"
           )
         (make-instance 'static-text
           :font (make-font-ex nil "Tahoma / ANSI" 14 nil)
           :height 19
           :left 59
           :name :static-text-13
           :top 100
           :value "Articulation threshold"
           :tooltip "rest between notes shorter than this value is deleted and the prec. note is lengthened"
           :width 133)
         (make-instance 'cancel-button
           :font (make-font-ex nil "Tahoma / ANSI" 14 nil)
           :height 31
           :left 8
           :name :cancel-button
           :top 140
           :width 64)
         (make-instance 'default-button
           :font (make-font-ex nil "Tahoma / ANSI" 14 nil)
           :height 31
           :left 128
           :name :default-button
           :title "OK"
           :top 140
           :width 64
           :on-change 
           #'(lambda (widget old new)
               (let ((new-threshold 
                      (read-from-string (value (find-sibling :articulation-threshold widget)))))
                 (cond ((or (not (numberp new-threshold))
                            (> new-threshold 1)
                            (< new-threshold 0) )                                
                        (advice "Enter a number between 0.0 and 1.0"))
                       (t (set-dm-var 'midifile-input-articulation-threshold new-threshold)
                          (set-dm-var 'midifile-input-quantize-dr 
                                      (value (find-sibling :quantize-durations widget)))
                          (set-dm-var 'midifile-input-quantize-notevalues 
                                      (read-from-string
                                       (value (find-sibling :notevalues widget))))
                          (set-dm-var 'midifile-input-quantize-triplet-notevalues 
                                      (read-from-string
                                       (value (find-sibling :triplet-notevalues widget))))
                          (set-dm-var 'midifile-input-quantize-split-at-barlines 
                                      (value (find-sibling :split-at-barlines widget)))
                          (return-t-from-pop-up-dialog widget old new) ))
                 t))
           )       
         )          
        :exterior (make-box 256 149 470 360)
        :border :frame
        :close-button t
        :cursor-name :arrow-cursor
        :maximize-button nil
        :minimize-button nil
        :name :midifile-input
        :pop-up t
        :resizable nil
        :scrollbars nil
        :state :shrunk
        :status-bar nil
        :system-menu nil
        :title "MIDIfile input processing"
        :title-bar t
        :toolbar nil
        )))




