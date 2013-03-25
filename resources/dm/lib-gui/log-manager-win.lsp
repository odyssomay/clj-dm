;;;-*-Mode: LISP; Package: DM -*-
;;
;; ************************************************************************
;;   LOG-MANAGER for reading the logs created during the rule application
;; ************************************************************************
;;
;; 9709 /Vittorio Colombo, new tool for Win 95 version
;; 9801 /VC last update

(in-package :dm)


;; =====================
;;   CLASS LOG-MANAGER
;; =====================
;;
(defclass log-manager (dialog)
    ())


;; --------------------
;;   MAKE-LOG-MANAGER
;; --------------------
;;
(defun make-log-manager (&key (parent *dm-main-window*) 
                          (window-interior (make-box 15 56 541 554)) 
                          (name :log-manager) (title "Log manager"))
   (open-dialog 
     (list

       (make-instance 'static-text 
         :name :txt-static-1 
         :title nil
         :value "Filename" 
         :box (make-box 75 22 136 44) 
         ;  :foreground-color blue 
         ;  :background-color light-gray 
         :font (make-font-ex :swiss :arial 13 nil))
       (make-instance 'editable-text 
         :name :txt-logfilename 
         :tabstop nil
         :title nil
         :read-only t
         :value (delete #\Newline "") 
         :box (make-box 140 20 433 42) 
         :groupstart t 
         :font (make-font-ex :swiss :arial 13 '(:bold)))
       (make-instance 'button 
         :name :button-logfile 
         :title "Browse.."
         :value t 
         :box (make-box 441 20 515 42) 
         :tabstop t 
         :set-value-fn #'(lambda (widget new old)
                           (let ((a nil)(b nil)(txt nil)(filestream)
                                 (fpath (show-dialog-for-opening-files-PD "Opening the log file"
                                         :directory (get-dm-var 'log-directory)
                                         :extensions '(("LOG files" . "*.log")("All files" . "*.*"))) ))
                              (when fpath (set-dialog-field :log-manager :txt-logfilename (namestring fpath))
                                 (setf filestream (open fpath :direction :input))
                                 (while (not b)
                                   (multiple-value-setq (a b) (read-line filestream))
                                   (setf txt (format nil "~A~A~%" txt a))
                                   )
                                 (set-dialog-field name :txt-multi-line txt)
                                 (print "^^^^^^^^^^^^^^^")
                                 (print txt)
                                 )
                              )
                           )
         :font (make-font-ex :swiss :arial 13 '(:bold)))
       (make-instance 'multi-line-editable-text 
         :name :txt-multi-line 
         :title nil
         :value (delete #\Newline "") 
         :box (make-box 6 56 522 468) 
         :tabstop t 
         :groupstart t 
         :scrollable t
         :bottom-attachment :bottom
         :right-attachment :right
         :font (make-font-ex :modern :courier\ new 13 nil))
       (make-instance 'button 
         :name :button-close 
         :title "Close"
         :box (make-box 480 473 522 495)
         :right-attachment :right
         :left-attachment :right 
         :top-attachment :bottom
         :bottom-attachment :bottom 
         :set-value-fn #'(lambda (widget new old)
                           (close (parent widget)))
         :font (make-font-ex :swiss :arial 13 nil))
      (make-instance 'lisp-group-box 
         :name :lisp-group-box-1
         :title "Logfile"
         :box (make-box 6 4 522 52) 
         :foreground-color blue 
         :font (make-font-ex :swiss :arial 15 '(:italic)))
       )
     'log-manager parent 
     :name name 
     :title title 
     :font (make-font :swiss :system 16 '(:bold)) 
     :window-state :normal
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
     :overlapped nil 
     :background-color light-gray 
     :pop-up-p nil 
     :window-interior window-interior))






