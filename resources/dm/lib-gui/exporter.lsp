;;;-*-Mode: LISP; Package: DM -*-
;;
;; ****************************************************
;;   EXPORTER for exporting data to other application
;; ****************************************************
;;
;; 9707 /Vittorio Colombo, new tool for Win 95 version

(in-package :dm)

;; ==================
;;   CLASS EXPORTER
;; ==================
;;
(defclass exporter ()
    ((win-stream :initarg :win-stream :accessor win-stream)
     (curr-score :initarg :curr-score :accessor curr-score)
     (curr-track :initarg :curr-track :accessor curr-track)
     (curr-property :initarg :curr-property :accessor curr-property)
     (curr-text :initarg :curr-text :accessor curr-text)
     (heading :initarg :heading :accessor heading)
     ))

(defmethod initialize-instance :after ((exporter exporter) &rest initargs
                                       &key curr-score (curr-text ""))
   (setf (win-stream exporter)
         (open-stream           ;creates a bitmap-window
           'dialog      ;suitable for graphics.
           
           *dm-main-window*            ;background stream
           :io                 ;input and output
           :title "Exporter"
           :name :exporter
           :window-exterior
           (make-box 392 26 870 564)))
   
   (setf (heading exporter)
         (make-instance 'heading :parent exporter :curr-score curr-score))
   (setf (curr-text exporter) curr-text)      ; ***** I think we can avoid this
   (add-other-fields exporter 
     :x (x (heading exporter)) 
     :y (+ (y (heading exporter)) 100))
   )

(defmethod initialize-instance :after ((exporter exporter) &rest initargs
                                       &key curr-score (curr-text ""))
   (setf (win-stream exporter)
         (make-window :exporter  ;creates a window
           :device 'dialog      ;suitable for graphics.
           
           :parent *dm-main-window*      
           :title "Exporter"
           :close-button t
           :maximize-button nil
           :minimize-button t
           :pop-up nil
           :overlapped *overlapped-windows*
           :resizable nil
           :scrollbars nil

           :window-exterior
           (make-box 392 26 870 564)))
   
   (setf (heading exporter)
         (make-instance 'heading :parent exporter :curr-score curr-score))
   (setf (curr-text exporter) curr-text)      ; ***** I think we can avoid this
   (add-other-fields exporter 
     :x (x (heading exporter)) 
     :y (+ (y (heading exporter)) 100))
   )

;; =================
;;   CLASS HEADING
;; =================
;;
(defclass heading ()
    ((parent :initarg :parent :accessor parent)
     (x :initarg :x :accessor x)
     (y :initarg :y :accessor y)
     ))


(defmethod initialize-instance :after ((item heading) &rest initargs
                                       &key (x 6) (y 10) (curr-score nil) )
   (dolist (widg (list
                   (make-instance 'lisp-group-box 
                     :name :lisp-group-box 
                     :title "File and track"
                     :box (make-box-relative x y 461 92) 
                     :tabstop nil 
                     :groupstart nil 
                     :foreground-color blue 
                     :font (make-font-ex :swiss :arial 13 '(:bold)))
                   (make-instance 'static-text 
                     :name :static-text-12 
                     :title "Static Text" 
                     :value "Filename" 
                     :box (make-box-relative (+ x 17)(+ y 23) 67 18) 
                     :border :none 
                     :tabstop nil 
                     :groupstart t 
                     :foreground-color blue 
                     :background-color light-gray 
                     :font (make-font-ex :swiss :arial 13 nil))
                   (make-instance 'static-text 
                     :name :static-text-23 
                     :title"Static Text" 
                     :value "Nickname"
                     :box (make-box-relative (+ x 17)(+ y 51) 60 20)
                     :border :none
                     :tabstop nil 
                     :groupstart t 
                     :foreground-color blue 
                     :background-color light-gray 
                     :font (make-font-ex :swiss :arial 13 nil))
                   (make-instance 'static-text 
                     :name :static-text-24 
                     :title nil 
                     :value "Track list"
                     :box (make-box-relative (+ x 219)(+ y 49) 57 20) 
                     :tabstop nil 
                     :groupstart t 
                     :foreground-color blue 
                     :background-color light-gray 
                     :font (make-font-ex :swiss :arial 13 nil) 
                     :tab-control nil)
                   (make-instance 'static-text 
                     :name :txtfilename 
                     :title nil 
                     :value (namestring (score-filename curr-score))
                     :box (make-box-relative (+ x 81)(+ y 21) 195 20)
                     :border :static 
                     :tabstop nil 
                     :groupstart t 
                     :background-color white 
                     :font (make-font-ex :swiss :arial 13 nil))
                   (make-instance 'static-text 
                     :name :txtnickname 
                     :title nil 
                     :value (nickname curr-score)
                     :box (make-box-relative (+ x 81)(+ y 49) 126 20)
                     :border :static 
                     :tabstop nil 
                     :groupstart t 
                     :background-color white 
                     :font (make-font-ex :swiss :arial 13 nil) 
                     :tab-control nil)
                   (make-instance 'single-item-list 
                     :name :lsttrack 
                     :title "Single Item List" 
                     :box (make-box-relative (+ x 282) (+ y 23) 173 48)
                     :tabstop t 
                     :groupstart t 
                     :background-color (make-rgb :red 192 :green 253 :blue 254) 
                     :set-value-fn #'(lambda (widget new old)
                                       (setf (curr-track (parent item)) new)
                                       ) 
                     :key 'give-me-track-name 
                     :range (track-list curr-score)
                     :font (make-font-ex nil :arial 13 nil) 
                     :tab-control nil)
                   ))
      (add-component widg (win-stream (parent item)))
      (setf (x item) x)
      (setf (y item) y)
      ))


;; ---------------------------
;;   Method ADD-OTHER-FIELDS
;; ---------------------------
;;
(defmethod add-other-fields ((exporter exporter) &key x y)
   (dolist (widg (list
                   (make-instance 'lisp-group-box 
                     :name :lisp-group-box-property
                     :title "Filter"
                     :box (make-box-relative x y 461 65) 
                     :tabstop nil 
                     :groupstart nil 
                     :foreground-color blue 
                     :font (make-font-ex :swiss :arial 13 '(:bold)))
                   (make-instance 'static-text 
                     :name :text-prop
                     :title "" 
                     :value "Choose vars (sep by space)" 
                     :box (make-box-relative (+ x 17)(+ y 23) 180 25) 
                     :border :none 
                     :tabstop nil 
                     :groupstart t 
                     :foreground-color blue 
                     :background-color light-gray 
                     :font (make-font-ex :swiss :arial 13 nil))
                   (make-instance 'editable-text 
                     :name :edit-prop
                     :title 'edit-prop 
                     :value 'nil
                     :box (make-box-relative (+ x 200) (+ y 23 ) 150 25)
                     :border :plain
                     :tabstop t 
                     :groupstart t
                     :set-value-fn #'(lambda (widget new old)
                                       (setf (curr-property exporter) new)
                                       )
                     :font (make-font-ex :modern :courier\ new 13 nil))
                   (make-instance 'multi-line-editable-text 
                     :name :editable-text-prop 
                     :title 'nil
                     :value (curr-text exporter)
                     :box (make-box-relative x (+ y 80) 461 280) 
                     :tabstop t 
                     :groupstart t
                     :scrollable :vertical
                     :font (make-font-ex :modern :courier\ new 13 nil))
                   (make-instance 'button 
                     :name :ok-button 
                     :title "Extract" 
                     :box (make-box-relative (+ x 360) (+ y 23 ) 90 25)
                     :tabstop nil 
                     :groupstart nil 
                     :set-value-fn #'(lambda (widget new old)
                                      (button-ok exporter)
                                       )
                     :font (make-font-ex :swiss :arial 13 '(:bold)))
                   (make-instance 'button 
                     :name :export-to-file-button 
                     :title "export to file.."
                     :available-p nil 
                     :box (make-box-relative x (+ y 370) 130 25)
                     :tabstop nil 
                     :groupstart nil
                     :set-value-fn #'(lambda (widget new old)
                                       (export-to-file exporter)
                                       )
                     :font (make-font-ex :swiss :arial 13 '(:bold)))
                   (make-instance 'button
                     :Available-P t
                     :name :export-to-clipboard-button 
                     :title "export to clipboard" 
                     :box (make-box-relative (+ x 140)(+ y 370) 130 25)
                     :tabstop nil 
                     :groupstart nil
                     :set-value-fn #'(lambda (widget new old)
                                        (push-lisp-clipboard *dm-main-window*
                                          (curr-text exporter))
                                       )
                     :font (make-font-ex :swiss :arial 13 '(:bold)))
;;;                    (make-instance 'button 
;;;                      :name :close-button 
;;;                      :title "close" 
;;;                      :box (make-box-relative (+ x 360) (+ y 370) 100 25)
;;;                      :tabstop nil 
;;;                      :groupstart nil
;;;                      :set-value-fn #'(lambda (widget new old)
;;;                            (close-button (parent widget)))
;;;                      :font (make-font-ex :swiss :arial 13 '(:bold)))
                   ))
      (add-component widg (win-stream exporter))
      ))

(defmethod button-ok ((exporter exporter))
   (cond ((not (slot-boundp exporter 'curr-track))
          (advice "Please select a track !" :title "Exporter" :mode 'win))
         (t
           (cond ((not (slot-boundp exporter 'curr-property))
                  (advice "Please indicate a property !" :title "Exporter" :mode 'win))
                 (t
                   (setf (curr-text exporter)
                         (format nil "Score filename ~A~ASelected property ~A~A--------------------"
                           (score-filename (curr-score exporter)) #\Newline
                           (curr-property exporter) #\Newline))
                   (each-segment 
                     (setf (curr-text exporter)
                           (format nil "~A~A~A~A~A" (curr-text exporter) #\Newline
                             *i*  #\Tab
                             (this (read-from-string (curr-property exporter))))))
                   (set-dialog-field :exporter :editable-text-prop 
                     (curr-text exporter)))))))

(defmethod button-ok ((exporter exporter))
   (cond ((not (slot-boundp exporter 'curr-track))
          (advice "Please select a track !" :title "Exporter" :mode 'win))
         (t
           (cond ((not (slot-boundp exporter 'curr-property))
                  (advice "Please indicate a property !" :title "Exporter" :mode 'win))
                 (t
                   (setf (curr-text exporter)
                         (format nil "Score filename ~A~ASelected property ~A~A--------------------"
                           (score-filename (curr-score exporter)) #\Newline
                           (curr-property exporter) #\Newline))
                   (each-segment 
                     (setf (curr-text exporter)
                           (format nil "~A~A~A" (curr-text exporter) #\Newline *i*) )
                     (let ((prop t) (index 0))
                        (loop while prop do
                         (multiple-value-setq (prop index) 
                           (read-from-string (curr-property exporter) nil nil :start index) )
                         (if (eq nil prop) (return))
                         (setf (curr-text exporter)
                               (format nil "~A~A~A" (curr-text exporter) #\Tab (this prop)))
                         )))
                   (set-dialog-field :exporter :editable-text-prop 
                     (curr-text exporter)))))))


   
