;; Bisesi/Parncutt accent ruless
;; A GUI interface for setting accent parameters manually
;;
;; 101207/af first version
;; 110628/af adapted to right-click menu in score window

(in-package :dm)

;(defvar *utility-dialog-font*)
;(setq *utility-dialog-font* (make-font-ex nil "MS Sans Serif" 15 nil))


;;;----------------------------------
;;; edit-accents-dialog
;;;----------------------------------

;;main function for starting the GUI
(defun edit-accents-dialog (&key (track-number 1) (note-number 1))
  (mark-note-number)
  (bring-window-to-front
         (make-window :set-accents-window 
           :parent *dm-main-window*
           :device 'dialog
           :class 'dialog
           :overlapped *overlapped-windows*
           ;:exterior exterior
           ;:interior (make-box 8 25 402 357)
           :interior (make-box 8 65 402 397)

           :close-button t
           :font (make-font-ex :swiss "Tahoma / ANSI" 11)
           :form-state :normal
           :maximize-button t
           :minimize-button t
           :name :form1
           :pop-up nil
           :resizable nil
           :scrollbars nil
           :state :normal
           :system-menu t
           :title "Set accent marks in score"
           :title-bar t
           :dialog-items (make-set-accents-widgets track-number note-number)
           ;:form-p form-p
           :form-package-name nil)))

;;utility for marking the note number with :nr
;; starts from 1 in each track
(defun mark-note-number ()
  (each-track
   (let ((n 1))
     (each-note
      (set-this :nr n)
      (incf n)
      ))))

;;defines the graphical widgets used in the window
(defun make-set-accents-widgets (track-number note-number)
  (list 
   ;trackbars   
   (make-instance 'trackbar :direction-of-increase :up :height 177
     :left 182 :name :trackbar-height :orientation :vertical 
     :range (list 0 50) :top 24 :value 2 :width 21
     :on-change #'(lambda (widget old new)
                    (setf (value (find-sibling :editable-text-height widget)) (prin1-to-string (/ (value widget) 10.0))) ))
   
   (make-instance 'trackbar :height 21 :left -4 :name :trackbar-left
     :range (list 0 15) :top 223 :value 2 :width 177
     :on-change #'(lambda (widget old new)
                    (setf (value (find-sibling :editable-text-notes-left widget)) (prin1-to-string (value widget))) ))
   
   (make-instance 'trackbar :height 21 :left 221 :name :trackbar-right
     :range (list 0 15) :top 223 :value 2 :width 177
     :on-change #'(lambda (widget old new)
                    (setf (value (find-sibling :editable-text-notes-right widget)) (prin1-to-string (value widget))) ))
   
   ;combo boxes
   (make-instance 'combo-box :font
     (make-font-ex nil "Tahoma / ANSI" 11) :height 24
     :left 25 :name :combo-box-left :range
     (list :linear :quadratic :cubic :exponential :cosine :gaussian :hand-gesture)
     :top 121 :value :quadratic :width 121)
   (make-instance 'combo-box :font
     (make-font-ex nil "Tahoma / ANSI" 11) :height 24
     :left 250 :name :combo-box-right :range
     (list :linear :quadratic :cubic :exponential :cosine :gaussian :hand-gesture)
     :top 121 :value :quadratic :width 121)
   (make-instance 'combo-box :font
     (make-font-ex nil "Tahoma / ANSI" 11) :height 24
     :left 25 :name :combo-box-param :range
     (list :sound_level :IOI_duration)
     :top 49 :value :sound_level :width 121)
   
   ;editable text
   (make-instance 'editable-text :font
     (make-font-ex nil "Tahoma / ANSI" 11) :height 17
     :left 25 :name :editable-text-track-number :top 9 :value (prin1-to-string (1+ track-number))
      :width 44)
   (make-instance 'editable-text :font
     (make-font-ex nil "Tahoma / ANSI" 11) :height 17
     :left 25 :name :editable-text-note-number :top 29 :value (prin1-to-string (1+ note-number))
     :width 44)
   (make-instance 'editable-text :font
     (make-font-ex nil "Tahoma / ANSI" 11) :height 17
     :left 45 :name :editable-text-notes-left :top 256 :value
     "2" :width 44)
   (make-instance 'editable-text :font
     (make-font-ex nil "Tahoma / ANSI" 11) :height 17
     :left 290 :name :editable-text-notes-right :top 256 :value
     "2" :width 44)
   (make-instance 'editable-text :font
     (make-font-ex nil "Tahoma / ANSI" 11) :height 17
     :left 173 :name :editable-text-height :top 216 :value
     "2" :width 44)
   
   ;buttons
   (make-instance 'button :font
     (make-font-ex nil "Tahoma / ANSI" 11) :height 21
     :left 20 :name :button5 :title "Add" :top 300
     :width 71
     :on-change 
     #'(lambda (widget old new)
         (let ((track-number (read-from-string (value (find-sibling :editable-text-track-number widget))))
               (note-number (read-from-string (value (find-sibling :editable-text-note-number widget))))
               (ext-left (read-from-string (value (find-sibling :editable-text-notes-left widget))))
               (ext-right (read-from-string (value (find-sibling :editable-text-notes-right widget))))
               (peak (read-from-string (value (find-sibling :editable-text-height widget))))
               (fun-left (value (find-sibling :combo-box-left widget)))
               (fun-right (value (find-sibling :combo-box-right widget)))
               (perf-param (value (find-sibling :combo-box-param widget)))
               )
           (accent-apply-button-fn track-number note-number ext-left ext-right peak fun-left fun-right perf-param)
           )))
      
   (make-instance 'button :font
     (make-font-ex nil "Tahoma / ANSI" 11) :height 21
     :left 161 :name :button-remove :title "Remove" :top 300
     :width 71
     :on-change 
     #'(lambda (widget old new)
         (let ((track-number (read-from-string (value (find-sibling :editable-text-track-number widget))))
               (note-number (read-from-string (value (find-sibling :editable-text-note-number widget))))
               (perf-param (value (find-sibling :combo-box-param widget)))
               )
           (accent-remove-button-fn track-number note-number perf-param)
           )))
   
   ;static text
   (make-instance 'static-text :font
     (make-font-ex nil "Tahoma / ANSI" 11) 
     :height 17 :left 37  :top 102  :width 84
     :name :static-text-14 :value "Left curvature")
   (make-instance 'static-text :font
     (make-font-ex nil "Tahoma / ANSI" 11) :height 17
     :left 78 :name :static-text-track :top 11 :value
     "Track number " :width 154)
   (make-instance 'static-text :font
     (make-font-ex nil "Tahoma / ANSI" 11) :height 17
     :left 78 :name :static-text-26 :top 31 :value
     "Accented note" :width 154)
   (make-instance 'static-text :font
     (make-font-ex nil "Tahoma / ANSI" 11) :height 17
     :left 35 :name :static-text-16 :top 199 :value
     "Notes before" :width 85)
   (make-instance 'static-text :font
     (make-font-ex nil "Tahoma / ANSI" 11) :height 17
     :left 177 :name :static-text-27 :top 199 :value
     "Height" :width 36)
   (make-instance 'static-text :font
     (make-font-ex nil "Tahoma / ANSI" 11) :height 17
     :left 270 :name :static-text-18 :top 199 :value
     "Notes after" :width 85)
   (make-instance 'static-text :font
     (make-font-ex nil "Tahoma / ANSI" 11) :height 17
     :left 263 :name :static-text-15 :top 102 :value
     "Right curvature" :width 91)
   ))

;; called when apply button clicked
(defun accent-apply-button-fn (track-number note-number ext-left ext-right peak fun-left fun-right perf-param)
  (each-track-if
   (= (1- track-number) *i*)
   (each-note-if
    (= (1- note-number) *i*) 
    (then
     (if (equal :sound_level perf-param)
         (set-this 'accent-sl (list ext-left ext-right peak fun-left fun-right))
       (set-this 'accent-dr (list ext-left ext-right peak fun-left fun-right))
       ))))           
  ;(reset-music)
  ;(accent-main-sl 1)        
  ;(accent-main-dr 1)        
  (redraw-music-windows)
  ;(redraw-display-windows)
  )

;; called when remove button clicked
(defun accent-remove-button-fn (track-number note-number perf-param) 
  (each-track-if
   (= (1- track-number) *i*)
   (each-note-if
    (= (1- note-number) *i*) 
    (then
     (if (equal :sound_level perf-param)
         (rem-this 'accent-sl)
       (rem-this 'accent-dr)
       ))))
  ;(reset-music)
  ;(accent-main-sl 1)        
  ;(accent-main-dr 1)        
  (redraw-music-windows)
  ;(redraw-display-windows)
  )




