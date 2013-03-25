;;;-*-Mode: LISP; Package: DM -*-
;;
;; ****************************************************************
;;   displays modal utility dialogs called from Menu, Mac version
;; ****************************************************************
;;
;; the original filename is UtilityDialog
;;
;; 9505/AF
;; 9702/VC adapted to new system
;;
;; the following functions are defined:
;;   set-tempo-dialog ()
;;   change-octave-dialog ()
;;   set-key-dialog ()
;;   set-meter-dialog ()

(in-package :dm)

;; -------------------
;;  SET-TEMPO-DIALOG
;; -------------------
;;
(defun set-tempo-dialog ()
  (let ((all-tracks? t) (mm (get-first-tempo))(init-dur? t))
    (let ((w
           (make-instance 'dialog
             :window-type :double-edge-box :window-title "Set tempo"
             :view-position :centered
             :view-size #@(160 120)
             :view-font '("Chicago" 12 :srcor :plain)
             :view-subviews
             (list 
              (make-dialog-item 'editable-text-dialog-item
                                #@(3 3)
                                #@(30 15)
                                (prin1-to-string mm)
                                #'(lambda (item)
                                    (if (string/= (dialog-item-text item) "")
                                      (setq mm (read-from-string (dialog-item-text item)))
                                      (setq mm nil)
                                      ))
                                :draw-outline t
                                ;:view-font '("Geneva" 9 :srccopy :plain)
                                :allow-returns nil
                                :view-nick-name 'number)
              (make-dialog-item 'static-text-dialog-item
                                #@(40 3)
                                #@(200 16)
                                "beats per minute"
                                nil)
              (make-dialog-item 'radio-button-dialog-item
                                #@(2 25)
                                #@(100 15)
                                "All tracks"
                                #'(lambda
                                    (item)
                                    item
                                    (setq all-tracks? t))
                                :radio-button-pushed-p t
                                :dialog-item-enabled-p nil)
              (make-dialog-item 'radio-button-dialog-item
                                #@(2 45)
                                #@(110 16)
                                "Active tracks"
                                #'(lambda
                                    (item)
                                    item
                                    (setq all-tracks? nil))
                                :radio-button-pushed-p nil
                                :dialog-item-enabled-p nil)
              (make-dialog-item 'check-box-dialog-item
                                #@(2 65)
                                #@(155 22)
                                "Initialize durations"
                                #'(lambda
                                    (item)
                                    (cond
                                     ((check-box-checked-p item)
                                      (setq init-dur? t))
                                     ((not (check-box-checked-p item))
                                      (setq init-dur? nil))))
                                :check-box-checked-p init-dur?)
              (make-dialog-item 'button-dialog-item
                                #@(10 95)
                                #@(60 18)
                                "Cancel"
                                #'(lambda
                                    (item)
                                    item
                                    (return-from-modal-dialog t))
                                :default-button nil)
              (make-dialog-item 
               'button-dialog-item
               #@(80 95)
               #@(60 18)
               "OK"
               #'(lambda (item) 
                   item
                   (if (not (numberp mm)) 
                     (message-dialog (format nil "Not a number: ~A" mm))
                     (set-tempo mm :init-dur? init-dur?) )
                   (return-from-modal-dialog t))
               :default-button t)
              )) ))
      (modal-dialog w) )))


;; -----------------------
;;  CHANGE-OCTAVE-DIALOG
;; -----------------------
;;
(defun change-octave-dialog ()
  (let ((all-tracks? nil) (octave 0))
    (let ((w
           (make-instance 'dialog
             :window-type :double-edge-box :window-title "Set tempo"
             :view-position :centered
             :view-size #@(230 110)
             :view-font '("Chicago" 12 :srcor :plain)
             :view-subviews
             (list 
              (make-dialog-item 'editable-text-dialog-item
                                #@(3 3)
                                #@(30 15)
                                "0"
                                #'(lambda (item)
                                    (if (string/= (dialog-item-text item) "")
                                      (setq octave (read-from-string (dialog-item-text item)))
                                      (setq octave nil)
                                      ))
                                :draw-outline t
                                :allow-returns nil
                                :view-nick-name 'number)
              (make-dialog-item 'static-text-dialog-item
                                #@(40 3)
                                #@(300 16)
                                "octaves up (down negative)"
                                nil)
              (make-dialog-item 'radio-button-dialog-item
                                #@(2 25)
                                #@(100 15)
                                "All tracks"
                                #'(lambda
                                    (item)
                                    item
                                    (setq all-tracks? t))
                                :radio-button-pushed-p all-tracks?                                 :dialog-item-enabled-p nil)
              (make-dialog-item 'radio-button-dialog-item
                                #@(2 45)
                                #@(110 16)
                                "Active tracks"
                                #'(lambda
                                    (item)
                                    item
                                    (setq all-tracks? nil))
                                :radio-button-pushed-p (not all-tracks?)                                 :dialog-item-enabled-p nil)
               (make-dialog-item 'button-dialog-item
                                #@(10 75)
                                #@(60 18)
                                "Cancel"
                                #'(lambda
                                    (item)
                                    item
                                    (return-from-modal-dialog t))
                                :default-button nil)
              (make-dialog-item 'button-dialog-item
                                #@(80 75)
                                #@(60 18)
                                "OK"
                                #'(lambda
                                    (item)
                                    item
                                    (if (not (integerp octave))
                                        (message-dialog (format nil "Not an integer number: ~A" octave))
                                        (trans-octave octave) )
                                    (return-from-modal-dialog t) )
                                :default-button t)
              )) ))
      (modal-dialog w) )))

;; -----------------
;;  SET-KEY-DIALOG
;; -----------------
;;
(defun set-key-dialog ()
  (let ((all-tracks? t)
        (key (get-first 'key))
        (modus (get-first 'modus)))
    (let
      ((w
        (make-instance 'dialog
          :window-type :double-edge-box :window-title "Set key"
          :view-position :centered
          :view-size #@(160 110)
          :view-font '("Chicago" 12 :srcor :plain)
          :view-subviews
          (list 
           (make-dialog-item
            'editable-text-dialog-item
            #@(10 3)
            #@(30 15)
            key
            #'(lambda (item)
                (setq key (dialog-item-text item)) )
            :draw-outline t
            :allow-returns nil)
           (make-dialog-item
            'editable-text-dialog-item
            #@(53 3)
            #@(30 15)
            modus
            #'(lambda (item)
                (setq modus (dialog-item-text item)) )
            :draw-outline t
            :allow-returns nil)
           (make-dialog-item
            'radio-button-dialog-item
            #@(2 25)
            #@(100 15)
            "All tracks"
            #'(lambda
                (item)
                item
                (setq all-tracks? t))
            :radio-button-pushed-p t
            :dialog-item-enabled-p nil)
           (make-dialog-item
            'radio-button-dialog-item
            #@(2 45)
            #@(110 16)
            "Active tracks"
            #'(lambda
                (item)
                item
                (setq all-tracks? nil))
            :radio-button-pushed-p nil
            :dialog-item-enabled-p nil)
           (make-dialog-item
            'button-dialog-item
            #@(10 75)
            #@(60 18)
            "Cancel"
            #'(lambda
                (item)
                item
                (return-from-modal-dialog t))
            :default-button nil)
           (make-dialog-item 
            'button-dialog-item
            #@(80 75)
            #@(60 18)
            "OK"
            #'(lambda
                (item)
                item
                (if (or (not (member key *tones* :test #'equal))
                        (not (member modus *modus* :test #'equal)) )
                  (message-dialog 
                   (format nil "Wrong format: ~A ~A The following symbols are allowed:  ~A ~A"
                           key modus *tones* *modus*) )
                  (set-key key modus) )
                    (return-from-modal-dialog t) )
            :default-button t)
           )) ))
      (modal-dialog w) )))

;; -------------------
;;  SET-METER-DIALOG
;; -------------------
;;
(defun set-meter-dialog ()
  (let ((all-tracks? t)
        (m1 "4")
        (m2 "4")
        (rebar? t))
    (if (get-first 'meter)
      (progn
        (setq m1 (prin1-to-string (car (get-first 'meter))))
        (setq m2 (prin1-to-string (cadr (get-first 'meter)))) ))
    (let
      ((w
        (make-instance 'dialog
          :window-type :double-edge-box :window-title "Set key"
          :view-position :centered
          :view-size #@(160 120)
          :view-font '("Chicago" 12 :srcor :plain)
          :view-subviews
          (list 
           (make-dialog-item
            'editable-text-dialog-item
            #@(10 3)
            #@(30 15)
            m1
            #'(lambda (item)
                (setq m1 (dialog-item-text item)) )
            :draw-outline t
            :allow-returns nil)
           (make-dialog-item
            'editable-text-dialog-item
            #@(53 3)
            #@(30 15)
            m2
            #'(lambda (item)
                (setq m2 (dialog-item-text item)) )
            :draw-outline t
            :allow-returns nil)
           (make-dialog-item
            'radio-button-dialog-item
            #@(2 25)
            #@(100 15)
            "All tracks"
            #'(lambda
                (item)
                item
                (setq all-tracks? t))
            :radio-button-pushed-p t
            :dialog-item-enabled-p nil)
           (make-dialog-item
            'radio-button-dialog-item
            #@(2 45)
            #@(110 16)
            "Active tracks"
            #'(lambda
                (item)
                item
                (setq all-tracks? nil))
            :radio-button-pushed-p nil
            :dialog-item-enabled-p nil)
           (make-dialog-item
            'check-box-dialog-item
            #@(2 65)
            #@(134 22)
            "Recompute bars"
            #'(lambda
                (item)
                (cond
                 ((check-box-checked-p item)
                  (setq rebar? t))
                 ((not (check-box-checked-p item))
                  (setq rebar? nil))))
            :check-box-checked-p rebar?)

           (make-dialog-item
            'button-dialog-item
            #@(10 90)
            #@(60 18)
            "Cancel"
            #'(lambda
                (item)
                item
                (return-from-modal-dialog t))
            :default-button nil)
           (make-dialog-item 
            'button-dialog-item
            #@(80 90)
            #@(60 18)
            "OK"
            #'(lambda (item)
                item
                (if (or (string= m1 "")
                        (string= m2 "")
                        (not (integerp (read-from-string m1)))
                        (not (integerp (read-from-string m2)))
                        (not (> (read-from-string m1) 0))
                        (not (> (read-from-string m2) 0)) )
                  (message-dialog 
                   (format nil "Not positive integers: ~A/~A" m1 m2) )
                  (set-meter (read-from-string m1) (read-from-string m2)
                             :rebar? rebar?) )
                    (return-from-modal-dialog t) )
            :default-button t)
           )) ))
      (modal-dialog w) )))
