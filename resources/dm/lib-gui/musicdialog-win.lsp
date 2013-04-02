;;;-*-Mode: LISP; Package: DM -*-
;;
;; *******************************
;;   Music (track) dialog
;; *******************************
;;
;; 9810 /Anders Friberg
;; 990512af  General midi program names
;; 000925/af New synths and cosmetic changes
;; 061003/af added bank select
;; 061018/af added reverb and pan
;; 110416/af moved all synth def to syntobjects, some cleaning

(in-package :dm)

(defvar *track-window-font*)
(setq *track-window-font* (make-font :swiss :arial 15))

;;============================
;;  CLASS EDIT-MUSIC-WINDOW
;;============================

(defclass edit-music-window (dialog)
    ((score-object :initarg :score-object :accessor score-object :initform nil)
     (track-dialog-item-list :initarg :track-dialog-item-list :accessor track-dialog-item-list 
       :initform nil)
    ))


(defun make-or-update-edit-music-window ()
  ;(make-edit-music-window)
  (dolist (window (windows *dm-main-window* :owned-p t :states nil)) 
    (when (typep window 'edit-music-window)
      (close window)
      ))
  (make-edit-music-window)
  )


;;---------------------------
;;  MAKE-EDIT-MUSIC-WINDOW
;;---------------------------
;;
(defun make-edit-music-window-1 (&key (parent *dm-main-window*) 
                                 (exterior (make-box-relative 0 0 1000 352)) ;set again below
                                 ;(name :edit-music-window)
                                  (title "Score"))
   (make-window :edit-music-window
    :device 'edit-music-window
    :parent parent
    :widgets
     (list 
       (make-instance 'static-text
         :left 7
         :name :static-text-1
         :foreground-color dark-blue
         :font (make-font :swiss :arial 14)
         :top 6
         :value
 "Type            Active   Name                                    Instrument type   Channel   Synth                                   BankMSB BankLSB  Program                                 Volume     Pan         Reverb   Delay"
         :width 940
         :scrollbars nil)
       )
     :title title 
     :font (make-font :swiss :system 16 '(:bold))
     ;:font (make-font-ex :swiss nil 11)
     :exterior exterior
     :border :frame
     :close-button t
     :cursor-name :arrow-cursor
     :maximize-button nil
     :minimize-button t
     :pop-up nil
     :overlapped *overlapped-windows*
     :resizable t
     :scrollbars nil
     :state :shrunk
     :status-bar nil
     :system-menu t
     :title title
     :title-bar t
     :toolbar nil
     :help-string nil
     :background-color #S(rgb :red 152 :green 201 :blue 224) ; persika #S(rgb :red 253 :green 198 :blue 136) ;blå #S(rgb :red 147 :green 216 :blue 249) ;light-gray 
     ;:background-color light-gray 
   ))
   

;; -----------------
;;   make-edit-music-window
;; -----------------
;; main function used by Open score..
;; opens the score window with the tracks in *active-score*


(defun make-edit-music-window ()
   (let* ((my-window (make-edit-music-window-1))
          (ypos 15) (yincr 18) ymax)
      
      (setf (title my-window) (nickname *active-score*))
      (setf (score-object my-window) *active-score*)
      (dolist (track (track-list *active-score*))
         (setf (track-dialog-item-list my-window)
               (append 
                 (track-dialog-item-list my-window)
                 (list
                  (make-instance 'track-dialog-item
                    :view-container my-window
                    :x 0
                    :y ypos
                    :track track ))))
         (incf ypos yincr)
         )
      (setq ymax ypos)
      (setf ypos 28)
     (setf (height my-window) (+ 55 ymax)) ;increased for new xp appearence
     (setf (width my-window) 968)  ;bug? exterior does not work 
     (setf (left my-window) 0)
     (setf (top my-window) 60)
     (show-window my-window :normal)
      ))

;; =========================================
;;   CLASS TRACK-DIALOG-ITEM
;; =========================================
;;
;; dialog with x subviews: 
   
(defclass track-dialog-item ()
    ((view-container :initarg :view-container :accessor view-container)
     (x :initarg :x :accessor x)
     (y :initarg :y :accessor y)
     (track :initarg :track :accessor track)
     (midi-program-combo-box :initarg :midi-program-combo-box :accessor midi-program-combo-box)
     ))


(defmethod initialize-instance :after ((item track-dialog-item) &rest initargs
                                       &key track )
  (setf (track item) track)
  ;;;    (add-component
  ;;;     (make-instance 'trackbar  ;;negative values not possible
  ;;;       :direction-of-increase :right
  ;;;       :height 18
  ;;;       :left 600
  ;;;       :name :midi-volume
  ;;;       :on-change #'(lambda (widget new-value old-value)
  ;;;                      (declare (ignore-if-unused widget new-value old-value))
  ;;;                      ;(print new-value)
  ;;;                      (setf (midi-initial-volume (track item)) (- new-value 64)) t)
  ;;;       :range '(0 64)
  ;;;       :tick-side :none
  ;;;       :delayed t
  ;;;       :top (+ 6 (y item))
  ;;;       :value (+ (midi-initial-volume (track item)) 64)
  ;;;       :visible-range '(0 0)
  ;;;       :width 134
  ;;;       :old-window nil
  ;;;       :scrollbars nil
  ;;;       :tooltip "Initial track volume. Range: 0 to -64 dB"
  ;;;       :font '#.(make-font-ex nil :|MS SANS SERIF| 11 nil))
  ;;;     (view-container item) )
  
  (add-component
   (make-instance 'combo-box
     :height 282
     :left 904
     :name :track-delay
     :on-change #'(lambda (widget new-value old-value)
                    (declare (ignore-if-unused widget new-value old-value))
                    (setf (track-delay (track item)) new-value) t)
     :range '(0 1 2 3 4 5 6 7 8 9 10 12 14 16 18 20 25 30 35 40 45 50 55 60 65 70 80 90 100)
     :top (+ 6 (y item))
     :value (track-delay (track item))
     :tooltip "Initial track delay. Range: 0 to 100 ms"
     :width 51
     :font *track-window-font*)
   (view-container item) )
  (add-component
   (make-instance 'combo-box
     :height 282
     :left 858
     :name :midi-reverb
     :on-change #'(lambda (widget new-value old-value)
                    (declare (ignore-if-unused widget new-value old-value))
                    (setf (midi-reverb (track item)) new-value) t)
     :range *list-0-127*
     :top (+ 6 (y item))
     :value (midi-reverb (track item))
     :tooltip "Initial midi reverb Range: 0-127"
     :width 46
     :font *track-window-font*)
   (view-container item) )
  (add-component
   (make-instance 'combo-box
     :height 282
     :left 812
     :name :midi-pan
     :on-change #'(lambda (widget new-value old-value)
                    (declare (ignore-if-unused widget new-value old-value))
                    (setf (midi-pan (track item)) new-value) t)
     :range *list-0-127*
     :top (+ 6 (y item))
     :value (midi-pan (track item))
     :tooltip "Initial midi pan. Range: 0 far left, 64 middle, 127 far right"
     :width 46
     :font *track-window-font*)
   (view-container item) )
  (add-component
   (make-instance 'combo-box
     :height 282
     :left 761
     :name :midi-volume
     :on-change #'(lambda (widget new-value old-value)
                    (declare (ignore-if-unused widget new-value old-value))
                    (setf (midi-initial-volume (track item)) new-value) t)
     :range '(0 -1 -2 -3 -4 -5 -6 -7 -8 -9 -10 -11 -12 -13 -14 -15 -16 -17 -18 -19 -20 -21 -22 -23 -24 -25 -26 -27 -28 -29 -30 
              -31 -32 -33 -34 -35 -36 -37 -38 -39 -40 -41 -42 -43 -44 -45 -46 -47 -48 -49 -50 -51 -52 -53 -54 -55 -56 -57 -58 -59 -60 -61 -62 -63 -64)
     :top (+ 6 (y item))
     :value (round (midi-initial-volume (track item)))
     :tooltip "Initial track volume. Range: 0 to -64 dB"
     :width 51
     :font *track-window-font*)
   (view-container item) )
  
  (add-component
   (setf (midi-program-combo-box item)
     (make-instance 'combo-box
       :height 282
       :left 621
       :name :midi-program
       :on-change #'(lambda (widget new-value old-value)
                      (declare (ignore-if-unused widget new-value old-value))
                      (setf (midi-initial-program (track item))
                        (program-name-to-number new-value) ) t)
       :on-change-test 'equal
       :range (program-list (synth (track item)))
       :top (+ 6 (y item))
       :value (nth (1- (midi-initial-program (track item))) (program-list (synth (track item))))
       :tooltip "Initial midi program number. Range: 1 to 128"
       :width 140
       :font *track-window-font*))
   (view-container item) )
  (add-component
   (make-instance 'combo-box
     :height 282
     :left 575
     :name :midi-bank-lsb
     :on-change #'(lambda (widget new-value old-value)
                    (declare (ignore-if-unused widget new-value old-value))
                    (setf (midi-bank-lsb (track item)) new-value) t)
     :range *list-0-127*
     :top (+ 6 (y item))
     :value (midi-bank-lsb (track item))
     :tooltip "Initial midi bank (LSB)"
     :width 46
     :font *track-window-font*)
   (view-container item) )
  (add-component
   (make-instance 'combo-box
     :height 282
     :left 529
     :name :midi-bank-msb
     :on-change #'(lambda (widget new-value old-value)
                    (declare (ignore-if-unused widget new-value old-value))
                    (setf (midi-bank-msb (track item)) new-value) t)
     :range *list-0-127*
     :top (+ 6 (y item))
     :value (midi-bank-msb (track item))
     :tooltip "Initial midi bank (MSB)"
     :width 46
     :font *track-window-font*)
   (view-container item) )
  (add-component
   (make-instance 'combo-box
     :background-color white
     :foreground-color t
     :height 211
     :left 369
     :name :synth
     :on-change #'(lambda (widget new-value old-value)
                    (declare (ignore-if-unused widget new-value old-value))
                    (setf (synth (track item)) (make-synth new-value))
                    (setf (range (midi-program-combo-box item))  ;update midi program listing
                      (program-list (synth (track item))) )
                    (setf (value (midi-program-combo-box item))
                      (nth (1- (midi-initial-program (track item)))
                           (program-list (synth (track item)))) )
                    t)
     :on-change-test 'string-equal
     ;; :range '("Pinnacle" "SBlive" "Roland-A90" "Roland-PMA5" "Roland-1010" "Roland-1010-OrchII" "Roland-1010-OrchIIB" "Kontakt2-Piano" "Kontakt2-wind" "Technics-SX-P30" "Proteus" "SampleCell" "S3000" "FZ1" "fb01" "dx21" "Musse" "Generator")
     :range *defined-synth-names*
     :top (+ 6 (y item))
     :value (synth-symbol-to-name (type-of (synth track)))
     :tooltip "The selected synth. object defines the conversion of perf. variables to midi"
     :width 160
     :font *track-window-font*)
   (view-container item) )
  (add-component
   (make-instance 'combo-box
     :height 282
     :left 323
     :name :midi-channel
     :on-change #'(lambda (widget new-value old-value)
                    (declare (ignore-if-unused widget new-value old-value))
                    (setf (midi-channel (track item)) new-value) t)
     :range '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
     :top (+ 6 (y item))
     :value (midi-channel (track item))
     :tooltip "Initial midi channel. Range: 1 to 16"
     :width 46
     :font *track-window-font*)
   (view-container item) )
  (add-component
   (make-instance 'combo-box
     :foreground-color t
     :height 134
     :left 242
     :name :instrument-type
     :on-change #'(lambda (widget new-value old-value)
                    (declare (ignore-if-unused widget new-value old-value))
                    (setf (instrument-type (track item)) new-value) t)
     :on-change-test 'string-equal
     :range '("Keyboard" "String" "Voice" "Wind" "Percussion")
     :top (+ 6 (y item))
     :value (instrument-type (track item))
     :width 81
     :tooltip "Not implemented yet"
     :available nil
     :font *track-window-font*)
   (view-container item) )
  (add-component
   (make-instance 'editable-text
     :height 24
     :left 102
     :name :track-name
     :on-change #'(lambda (widget new-value old-value)
                    (declare (ignore-if-unused widget new-value old-value))
                    (setf (trackname (track item)) new-value)
                    (redraw-music-windows) t)
     :template-string nil
     :top (+ 5 (y item))
     :value (trackname (track item))
     :tooltip "Track name"
     :width 140
     :font *track-window-font*)
   (view-container item) )
  (add-component
   (make-instance 'check-box
     :left 75
     :name :check-box-1
     :on-change #'(lambda (widget new-value old-value)
                    (declare (ignore-if-unused widget new-value old-value))
                    (setf (active-p (track item)) new-value)
                    (redraw-music-windows) (redraw-display-windows) t)
     :title nil
     :value (active-p (track item))
     :top (+ 6 (y item))
     :tooltip "Playing and rule application only on Active tracks"
     :width 11
     :font *track-window-font*) 
   (view-container item) )
  (add-component
   (make-instance 'static-text
     :left 7
     :name :type
     :top (+ 8 (y item))
     :value (type-of track)
     :width 60
     :tooltip "Type of track. Can only be changed in the score file"
     :scrollbars nil
     :font (make-font :swiss :arial 14))
   (view-container item) )
  )

;;; (defun program-name-to-number (name)
;;;    (let ((n 0))
;;;       (until (equal (subseq name n (1+ n)) "=")
;;;         (incf n) )
;;;       (read-from-string (subseq name 0  n))
;;;       ))

(defun program-name-to-number (name)
   (let ((n 0))
      (until (or (equal (subseq name n (1+ n)) "=")
                 (equal (subseq name n (1+ n)) " ")
                 (= n (length name)) )
        (incf n) )
      (read-from-string (subseq name 0  n))
      ))

(defvar *list-0-127*)
(setq *list-0-127*
      '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44
        45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85
        86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119
        120 121 122 123 124 125 126 127)
      )

