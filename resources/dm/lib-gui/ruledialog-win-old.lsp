;;;-*-Mode: LISP; Package: DM -*-
;;
;; *******************************
;;   Rule dialog and rule setups
;; *******************************
;;
;; 9203 /Anders Friberg
;; 9706 /Vittorio Colombo ACL for Win95 specific version
;; 9711 /vc former "rule-dialog.lsp" split in two files: one containing common routines and one for each Mac and Win
;;       Common functions were moved to file ruledialog-common.lsp
;; 9801 /vc new layout with checkboxes for logs. Fixed bug with radio buttons.
;; 0005/af new more compact layout
 
;;   (defun apply-rules-window (dialog)
;;   (defmethod save-rule-set-as ((item apply-rules-window))
;;   (defun edit-rule-set (&key pathname)
;;   (defmethod rule-list ((item apply-rules-window))
;;   (defmethod rule-list-all ((item apply-rules-window))
;;   (defmethod apply-rules ((item apply-rules-window))


(in-package :dm)

;;============================
;;  CLASS APPLY-RULES-WINDOW
;;============================
;;
;;; (defclass apply-rules-window (dialog)
;;;     ((logfile :initarg :logfile :accessor logfile :initform (make-instance 'logfile))
;;;      (log-to-file-enabled :initarg :log-to-file-enabled :accessor log-to-file-enabled
;;;        :initform (get-dm-var 'log-to-file-enabled-default))
;;;      (log-to-score-enabled :initarg :log-to-score-enabled :accessor log-to-score-enabled
;;;        :initform (get-dm-var 'log-to-score-enabled-default))
;;;      (PAL-pathname :initarg :PAL-pathname :accessor PAL-pathname :initform nil)
;;;      (rule-quant-fields-list :initarg :rule-quant-fields-list :accessor rule-quant-fields-list 
;;;        :initform ())
;;;      (rule-check-fields-list :initarg :rule-check-fields-list :accessor rule-check-fields-list
;;;        :initform ())
;;;      (sync-type :initarg :sync-type :accessor sync-type :initform nil)
;;;      ))

(defclass apply-rules-window (dialog)
    ((logfile :initarg :logfile :accessor logfile :initform (make-instance 'logfile))
     (log-to-file-enabled :initarg :log-to-file-enabled :accessor log-to-file-enabled
       :initform (get-dm-var 'log-to-file-enabled-default))
     (log-to-score-enabled :initarg :log-to-score-enabled :accessor log-to-score-enabled
       :initform (get-dm-var 'log-to-score-enabled-default))
     (PAL-pathname :initarg :PAL-pathname :accessor PAL-pathname :initform nil)
     (rule-dialog-item-list :initarg :rule-dialog-item-list :accessor rule-dialog-item-list 
       :initform ())
     (sync-type :initarg :sync-type :accessor sync-type :initform nil)
     (button-stop :initarg :button-stop :accessor button-stop :initform nil)
     (button-play-perf :initarg :button-play-perf :accessor button-play-perf :initform nil)
     (button-play-nom :initarg :button-play-nom :accessor button-play-nom :initform nil)
    ))


;;---------------------------
;;  MAKE-APPLY-RULES-WINDOW
;;---------------------------
;;
(defun make-apply-rules-window (&key (parent *dm-main-window*) 
                                 (window-interior (make-box-relative 4 170 716 352)) 
                                 (name :apply-rules-window) (title "Rule palette"))
   (make-window name
     :device 'apply-rules-window
     :parent parent 
     :title title 
     :font (make-font :swiss :system 16 '(:bold)) 
     :state :shrunk
     :window-border :frame 
     :left-attachment nil 
     :top-attachment nil 
     :right-attachment nil 
     :bottom-attachment nil 
     :user-movable t 
     :user-resizable t 
     :user-closable t 
     :user-shrinkable t 
     :user-scrollable nil 
     :overlapped *overlapped-windows* 
     :background-color #S(rgb :red 146 :green 201 :blue 183) ;light-gray 
     ;:background-color light-gray 
     :pop-up-p nil 
     :window-interior window-interior
     :widgets
     (list 
       (make-instance 'button 
         :name :button-save 
         :title "Save as.." 
         :box (make-box 2 122 100 144) 
         :tabstop nil 
         :set-value-fn #'(lambda (widget old new)
                           (save-rule-set-as (parent widget)))
         :tooltip "Save the current rule palette as a text file"
         :font (make-font-ex :swiss :arial 12 ) 
         :tab-control nil)
       (make-instance 'button 
         :name :button-clear 
         :title "Init&Apply" 
         :box (make-box 2 50 100 72) 
         :tabstop nil 
         :set-value-fn #'(lambda (widget old new)
                           (with-waiting-cursor
                           (reset-music)
                           (apply-rules (parent widget))))
         :tooltip "Reset performance and apply the rules"
         :font (make-font-ex :swiss :arial 12) 
         :tab-control nil)
       (make-instance 'button 
         :name :button-apply 
         :title "Apply" 
         :box (make-box 2 74 100 96) 
         :tabstop nil 
         :set-value-fn #'(lambda (widget old new)
                           (with-waiting-cursor
                           (apply-rules (parent widget))))
         :font (make-font-ex :swiss :arial 12) 
         :tooltip "Apply the rules on top of the previous performance"
         :tab-control nil)
       (make-instance 'check-box 
         :name :check-box-log-to-file 
         :title "log to file"
         :value (get-dm-var 'log-to-file-enabled-default)
         :box (make-box 2 146 100 168) 
         :tabstop nil
         :set-value-fn #'(lambda (widget new old)
                           (setf (log-to-file-enabled (parent widget)) new)
                           (values t ;; Accept the new value
                             nil))
         :tooltip "Saves a log of rule applications to a file"
         :font (make-font-ex :swiss :arial 12)
         :background-color t
          )
       (make-instance 'check-box 
         :name :check-box-log-to-score 
         :title "log to score"
         :value (get-dm-var 'log-to-score-enabled-default)
         :value t 
         :box (make-box 2 170 100 192) 
         :tabstop nil
         :available-p nil
         :set-value-fn #'(lambda (widget new old)
                           (setf (log-to-score-enabled (parent widget)) new)
                           (values t ;; Accept the new value
                             nil))
         :font (make-font-ex :swiss :arial 12)
         :background-color t
          )
       (make-instance 'editable-text 
         :name :scale-factor-setting 
         :value "1.5" 
         :box (make-box 50 98 100 120) 
         :tabstop t 
         :tooltip "Scale factor value"
         :font (make-font-ex :modern :courier\ new 12))
       (make-instance 'button 
         :name :button-scale 
         :title "Scale:" 
         :box (make-box 2 98 50 120) 
         :tabstop nil 
         :set-value-fn #'(lambda (widget old new)
                           (scale-all-rules (parent widget)
                             (dialog-field (parent widget) :scale-factor-setting)))
         :tooltip "Multiply all rule quantities with the given value"
         :font (make-font-ex :swiss :arial 13)) 
;;;        (make-instance 'lisp-group-box 
;;;          :name :grouper 
;;;          :title "Synchronization"
;;;          :box (make-box 7 4 148 100) 
;;;          :tabstop nil
;;;          :foreground-color blue 
;;;          :tooltip "Synchronization type for polyphonic music"
;;;          :font 
;;;          (make-font-ex :swiss :arial 13 '(:bold :italic)))
;;;        (make-instance 'lisp-group-box 
;;;          :name :grouper-2 
;;;          :title "Create log"
;;;          :box (make-box 7 103 148 178) 
;;;          :tabstop nil
;;;          :foreground-color blue 
;;;          :font 
;;;          (make-font-ex :swiss :arial 13 '(:bold :italic)))
      )
   ))
   





;; ---------------------------
;;   Method SAVE-RULE-SET-AS
;; ---------------------------
;;
;; from the menu. Save a rule set in a file
;; 
(defmethod save-rule-set-as ((item apply-rules-window))
   (let ((PAL-pathname (show-dialog-for-saving-files-PD "Save a rule palette"
                 :directory (get-dm-var 'current-rule-sets-directory)
                 :extensions '(("PAL palette files" . "*.pal")("All files" . "*.*")) )))
      (when PAL-pathname 
           (with-open-file (ofile PAL-pathname :direction :output 
                             :if-does-not-exist :create
                             :if-exists :supersede)
                (princ "(in-package \"DM\")" ofile)(terpri ofile)
                (princ "(set-dm-var 'all-rules '(" ofile)(terpri ofile)
                (dolist (rule (rule-list-all item))
                   (prin1 rule ofile)(terpri ofile) )
                (princ "))" ofile)(terpri ofile)
                
                (princ "(set-dm-var 'sync-rule-list '" ofile)
;;;                 (do-subviews (subview item)
;;;                   (when (eq (view-nick-name subview) 'rule-sync)
;;;                      (princ "(" ofile)
;;;                      (princ (dialog-item-text subview) ofile)
;;;                      (princ " " ofile)
;;;                      (princ (radio-button-pushed-p subview) ofile)
;;;                      (princ ")" ofile) ))
             (princ (get-dm-var 'sync-rule-list) ofile)
                (princ ")" ofile)(terpri ofile)
                )
           (setf (title item) (file-namestring PAL-pathname))
           (setf (PAL-pathname item) PAL-pathname) 
           )))

(defmethod save-rule-set-as ((item apply-rules-window))
  (print (PAL-pathname item))
   (let ((PAL-pathname (show-dialog-for-saving-files-PD "Save rule palette"
                 :directory (PAL-pathname item)
                 :extensions '(("PAL palette files" . "*.pal")("All files" . "*.*")) )))
      (when PAL-pathname 
           (with-open-file (ofile PAL-pathname :direction :output 
                             :if-does-not-exist :create
                             :if-exists :supersede)
                (princ "(in-package \"DM\")" ofile)(terpri ofile)
                (princ "(set-dm-var 'all-rules '(" ofile)(terpri ofile)
                (dolist (rule (rule-list-all item))
                   (prin1 rule ofile)(terpri ofile) )
                (princ "))" ofile)(terpri ofile)
                
                (princ "(set-dm-var 'sync-rule-list '" ofile)
;;;                 (do-subviews (subview item)
;;;                   (when (eq (view-nick-name subview) 'rule-sync)
;;;                      (princ "(" ofile)
;;;                      (princ (dialog-item-text subview) ofile)
;;;                      (princ " " ofile)
;;;                      (princ (radio-button-pushed-p subview) ofile)
;;;                      (princ ")" ofile) ))
             (princ (get-dm-var 'sync-rule-list) ofile)
                (princ ")" ofile)(terpri ofile)
                )
           (setf (title item) (file-namestring PAL-pathname))
           (setf (PAL-pathname item) PAL-pathname) 
           )))



;; -----------------
;;   EDIT-RULE-SET
;; -----------------
;;
;; opens the rule setup window with the rules as defined in
;; all-rules and sync-rule-list
;; It's called by one of the two previous functions for opening a rule-set
;;

(defun edit-rule-set (&key pathname)
   (let* ((my-window (make-apply-rules-window))
          (ypos 2)(xcolumn 110)(yincr 20)(ymax))
      
      (setf (title my-window) (if pathname (file-namestring pathname) "Rule palette"))
      (setf (PAL-pathname my-window) (if pathname pathname "Rule palette.pal"))
      (dolist (rulepair (get-dm-var 'all-rules))
         (cond
               ((numberp (cadr rulepair)) ;rule with quantity
                (setf (rule-dialog-item-list my-window)
                      (append 
                        (rule-dialog-item-list my-window)
                        (list
                         (make-instance 'rule-quant-dialog-item
                           :view-container my-window
                           :x xcolumn
                           :y ypos
                           :rule-setting (cadr rulepair)
                           :rule-text (string-capitalize (rule-call-list-to-string rulepair))))
                        )))
               (t           ;rule without quantity
                 (setf (rule-dialog-item-list my-window)
                       (append
                        (rule-dialog-item-list my-window)
                        (list
                         (make-instance 'rule-check-box-dialog-item
                           :view-container my-window
                           :x xcolumn
                           :y ypos
                           :rule-setting (cadr rulepair)
                           :rule-text (string-capitalize (symbol-name (car rulepair))))) 
                         )))
               )
         (incf ypos yincr)
         )
      (setq ymax ypos)
      (setf ypos 194)
      (dolist (rulepair (get-dm-var 'sync-rule-list))
         (add-component (make-instance 'radio-button 
                       :name (car rulepair)
                       :title (string-capitalize (symbol-name (car rulepair)))
                       :box (make-box 2 ypos 100 (+ ypos 20)) 
                       :font (make-font-ex :swiss :arial 12) 
                       :background-color t 
                       :cluster :sync-radio-button-cluster
                       :click-off :no
                       :set-value-fn #'(lambda (widget new old)
                                         (setf (sync-type my-window) 
                                               (slot-value  widget 'cg::name))
                                         (values t ;; Accept the new value
                                           nil))
                       :value (cadr rulepair))
           my-window)
         (when (cadr rulepair)
            (setf (sync-type my-window) (car rulepair)))
         (incf ypos yincr))
      (add-component 
        (setf (button-play-nom my-window) 
        (make-instance 'button 
          :name :button-play-nom 
          :title "Play nominal"
          :box (make-box 2 26 100 48) 
          :tabstop nil
          :set-value-fn #'(lambda (widget old new)
                            (with-waiting-cursor
                            ;(setf (dialog-item-available-p  (button-stop my-window)) t)
                            ;(setf (dialog-item-available-p  (button-play-nom my-window)) nil)
                            ;(setf (dialog-item-available-p  (button-play-perf my-window)) nil)
                            (reset-music)
                            (playlist-mplayer-nominal)))   ; (eval-enqueue '(playlist)))
          :tooltip "Reset performance and play"          
          :font (make-font-ex :swiss :arial 12 '(:bold)))) 
        my-window)
      (add-component 
        (setf (button-play-perf my-window) 
        (make-instance 'button 
          :name :button-play-perf 
          :title "Play performed" 
          :box (make-box 2 2 100 24) 
          :tabstop nil
          :set-value-fn #'(lambda (widget old new)
                            (with-waiting-cursor
                            ;(setf (dialog-item-available-p  (button-stop my-window)) t)
                            ;(setf (dialog-item-available-p  (button-play-nom my-window)) nil)
                            ;(setf (dialog-item-available-p  (button-play-perf my-window)) nil)
                            (reset-music)
                            (apply-rules (parent widget))
                            (playlist-mplayer-performed))) ;******* (eval-enqueue '(playlist)))
          :tooltip "Reset performance, apply the rules and play"
          :foreground-color #S(rgb :red 255 :green 255 :blue 128) 
          :font (make-font-ex :swiss :arial 12 '(:bold))))
        my-window)
     (resize-window  my-window (make-position (interior-width my-window) (+ 2 (max ymax ypos))))
     ;(setf (interior-wiht my-window) (+ 2 (max ymax ypos)))
      (show-window my-window :normal)
      ))

;; -------------------------------------------------
;;   SCALE-ALL-RULES method for apply-rules-window 
;; -------------------------------------------------
;;
;; scale all rules with scrollbar with the amount given
;; in the scale-factor-setting dialog item
;; something wrong with the updating of the scrollbars graphically.
;
(defmethod scale-all-rules ((item apply-rules-window) new-value)
   (let ((scale-factor (read-from-string new-value)))      
      (dolist (rule (rule-dialog-item-list item))
         (if (typep rule 'rule-quant-dialog-item)
            (set-rule-setting rule (* scale-factor (rule-setting rule))) )) 
      ))
            

;; -------------------------------------------------------
;; ------------- rule dialog item definition -------------
;; -------------------------------------------------------

;; =========================================
;;   CLASS RULE-QUANT-DIALOG-ITEM
;;   rule with scroll-bar and number input
;; =========================================
;;
;; dialog with 3 subviews: scroll-bar, editable number and editable rule-name

(defclass rule-quant-dialog-item ()
    ((view-container :initarg :view-container :accessor view-container)
     (x :initarg :x :accessor x)
     (y :initarg :y :accessor y)
     (rule-setting :initarg :rule-setting :accessor rule-setting)
     (rule-text :initarg :rule-text :accessor rule-text)
     (k-number :initarg :k-number :accessor k-number)
     (k-scroll-bar :initarg :k-scroll-bar :accessor k-scroll-bar)
     (k-text :initarg :k-text :accessor k-text)
     ))

(defmethod initialize-instance :after ((item rule-quant-dialog-item) &rest initargs
                                       &key (rule-setting 1.0) (rule-text 'no-text!) )
   (add-component (setf (k-number item) 
                     (make-instance 'editable-text 
                       :name :number
                       :title nil
                       :value (format nil "~A" rule-setting)
                       :box (make-box (x item) (y item) (+ (x item) 55) (+ (y item) 20)) 
                       :tabstop t 
                       :groupstart t 
                       :font (make-font-ex :modern :courier\ new 12 nil)
                       :tooltip "Rule quantity"          
                       :set-value-fn #'(lambda (widget new old)
                                         (if (and (string/= new "")
                                                  (numberp (read-from-string new)))
                                            (progn
                                              (setf (value (k-scroll-bar item)) 
                                                (+ 50 (round (* (read-from-string new) 10))))
                                              (setf (rule-setting item) (read-from-string new) ))
                                            ))))
     (view-container item))
   (add-component (make-instance 'button 
                 :name :zero-button 
                 :title '0 
                 :box (make-box (+ (x item) 55) (+ (y item) 2) (+ (x item) 75) (+ (y item) 18))
                 :tabstop nil 
                 :groupstart nil
                 :font (make-font-ex :swiss :arial 12)
                 :tooltip "Set the rule quantity to zero"          
                 :on-change #'(lambda (widget new old)
                                   (set-rule-setting item 0)
                                   )
                 )
     (view-container item))
   (add-component (setf (k-scroll-bar item)
                     (make-instance 'horizontal-scroll-bar 
                       :name :scroll-bar
                       :title nil
                       :value (+ (round (* rule-setting 10)) 50)
                       :box (make-box (+ (x item) 75) (1+ (y item)) (+ (x item) 200) (+ (y item) 18))
                       :tabstop t 
                       :groupstart nil
                       :delayed nil
                       :direction :down
                       :page-increment 5
                       :background-color #S(rgb :red 221 :green 221 :blue 221) ;light-gray
                       
                       ;:tick-frequency 10 
                       ;:tick-side :bottom 
                       ;:orientation :horizontal 
 
                       :range (list 0 100)
                       :tooltip "Rule quantity"          
                       :on-change #'(lambda (widget new old)
                                         (progn
                                           (setf (value (k-number item))
                                             (format nil "~a" (/ (- new 50) 10.0)))
                                           (setf (rule-setting item) (/ (- new 50) 10.0)))
                                         )))
     (view-container item))
   (add-component (setf (k-text item)       
                     (make-instance 'editable-text 
                       :name :parameter-txt 
                       :title nil
                       :value rule-text
                       :box (make-box (+ (x item) 200) (y item) (+ (x item) 600) (+ (y item) 20))
                       :tabstop t  
                       :groupstart t 
                       :font (make-font-ex :modern :courier\ new 12 nil)
                       :tooltip "Rule name and :key parameters"          
                       :set-value-fn #'(lambda (widget new old)
                                         (setf (rule-text item) new ))
                       
                       ))
     (view-container item))
   )  



;; --------------------
;;   SET-RULE-SETTING
;; --------------------
;;
;; sets a new-value for one rule-quant-dialog-item

(defmethod set-rule-setting ((item rule-quant-dialog-item) setting)
   (setf (rule-setting item) setting)
   (setf (value (k-number item)) (format nil "~a" setting))
   (setf (value (k-scroll-bar item)) (+ (round (* setting 10)) 50))
)


;; ====================================
;;   CLASS RULE-CHECK-BOX-DIALOG-ITEM
;;   rule with check-box
;; ====================================
;;
;; dialog with 3 subviews: scroll-bar, editable number and editable rule-name

(defclass rule-check-box-dialog-item ()
    ((view-container :initarg :view-container :accessor view-container)
     (x :initarg :x :accessor x)
     (y :initarg :y :accessor y)
     (rule-setting :initarg :rule-setting :accessor rule-setting)
     (rule-text :initarg :rule-text :accessor rule-text)
     (rule-check-box :initarg :rule-check-box :accessor rule-check-box)
     ))

   
(defmethod initialize-instance :after ((item rule-check-box-dialog-item) &rest initargs
                                       &key (rule-setting t) (rule-text 'no-text!) )
   (add-component (setf (rule-check-box item) 
                     (make-instance 'check-box 
                       :name rule-text
                       :title rule-text
                       :font (make-font-ex :modern :courier\ new 12 nil)
                       :background-color t 
                       :value rule-setting
                       :box (make-box (+ (x item) 190) (y item) (+ (x item) 600) (+ (y item) 20))  
                       :tabstop t
                       :tooltip "Selection of rule without quantity"          
                       :set-value-fn #'(lambda (widget new old)   
                                         (setf (rule-setting item) new)
                                         (values t ;; Accept the new value
                                            nil)
                                         )
                       ))
     (view-container item))
)



;; ---------------------------------------------
;;   APPLY-RULES method for apply-rules-window 
;; ---------------------------------------------
;;
;; the type of synchronization is now stored in the slot of the main window
;; the code is much simpler

(defmethod apply-rules ((item apply-rules-window))
   
   ; this variable is for convenience: when applying a rule to a note, one single test is enough
   ; to see if I must call the log routine or not. I save time in case no log is required
   (set-dm-var 'log-requested (or (log-to-file-enabled item)(log-to-score-enabled item)))
   (set-dm-var 'log-to-file-enabled (log-to-file-enabled item))
   (set-dm-var 'log-to-score-enabled (log-to-score-enabled item))
   
   (when (get-dm-var 'log-requested)
      (cond ((open-logfile (logfile item))(set-dm-var 'log-filehandle (filehandle (logfile item))))
            (t (set-dm-var 'log-requested nil))))
   
   ;(rule-list-all item)
   (rule-apply-list-sync (rule-list item)(sync-type item))
   (redraw-display-windows)
   
   (when (get-dm-var 'log-requested)
      (set-dm-var 'log-requested nil)
      (close-logfile (logfile item))
      (set-dm-var 'log-filehandle nil))
   )

;; -------------------------------------------
;;   Method RULE-LIST for apply-rules-window 
;; -------------------------------------------
;;          
;; extracts the rule list from the window
;; in the format for the 'apply-rule-list functions
;;
(defmethod rule-list ((item apply-rules-window))
   (let ((l))
      (dolist (rule (rule-dialog-item-list item))
         (cond ((typep rule 'rule-quant-dialog-item)
                (when (not (zerop (rule-setting rule)))
                   (newr l (string-to-rule-call-list 
                            (rule-text rule)
                            (rule-setting rule) ))))
               ((typep rule 'rule-check-box-dialog-item)
                (when (rule-setting rule) 
                   (newr l (list (read-from-string (rule-text rule))))
                   ))
               ))
         l))


;; -----------------------------------------------
;;   RULE-LIST-ALL method for apply-rules-window 
;; -----------------------------------------------
;;
;; extracts the rule list from the window
;; in the load and save rule setups format.
;; Called by apply-rules
;;

(defmethod rule-list-all ((item apply-rules-window))
   (let ((l))
     (dolist (rule (rule-dialog-item-list item))
       (when (not (equal "" (rule-text rule)))
         (cond ((typep rule 'rule-quant-dialog-item)
                (newr l (string-to-rule-call-list 
                         (rule-text rule)
                         (rule-setting rule) )))
               ((typep rule 'rule-check-box-dialog-item)
                (newr l (list (read-from-string (rule-text rule))
                          (rule-setting rule) )) )
               )))
      l))



