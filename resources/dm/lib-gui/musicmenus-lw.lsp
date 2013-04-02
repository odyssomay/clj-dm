;;-*-Mode: LISP; Package: DM -*-
;;

(in-package :dm)

;; *********************************************************
;;   creates the menus in the Lisp environment main window
;;   ACL Win 95 version
;; *********************************************************
;;
;; filename: MusicMenus-Win.lsp    is a Win translation of the Mac version "MusicMenus"
;;
;; 9201 cl/Anders Friberg
;; 9702 Win95 version cl/Vittorio Colombo
;; 20001006 Roberto Bresin added: Menu Item "Offtime duration (% of IOI)"


;;------------global variables-----------------

(defvar *dm-main-window*) ;holds the music menus window below which is the parent of all dm windows
;(setq *dm-main-window*) 
(defvar *overlapped-windows* t)
(setq *overlapped-windows* t)

(defvar *dm-main-window-title*)
#+(and :mswindows :allegro)
(setq *dm-main-window-title*
 (multiple-value-bind (sec min hour date month year) 
                       (get-decoded-time)
                       (format nil "Director Musices 2.6   compiled: ~A/~A/~A ~A:~2,'0D"
                         date month year hour min)
                       ))

(defvar *dm-home-directory*)

;;--------startup function -----------------------

;startup function for dm windows version
;;; (defun install-music-menus ()
;;;      (in-package :dm)
;;;      (loop for i from 1 to 3 do (format nil "loop test ~A" i)) ;dummy for the exe generation
;;;      (setq *print-case* :downcase)
;;;      (setq *dm-home-directory* (excl::current-directory))
;;;      (display-main-window)
;;;      ;(display-about-window)
;;;      (open-default-rule-set)
;;;      (when *demo-version* (setq *debugger-hook* #'dm-debugger))
;;;      ;(display-message-window)
;;;    )
;;; 
;;; ;displays a simple error message and return with abort
;;; ;works only in the ide environment since the abort restart is missing in the standalone
;;; (defun dm-debugger (condition a)
;;;    ;(format t "~&Error: ~A" condition)
;;;    (pop-up-message-dialog (screen *system*) "Error" (format nil "~A" condition) error-icon "OK")
;;;    (abort) )

#+(and :mswindows :allegro)
(defun dm-exit ()
   (if (y-or-n-dialog "Are you sure you want to exit Director Musices?")
      (excl:exit 0 :no-unwind t)))

;;-------- init function including toploop-----------------------------------

#+(and :mswindows :allegro)
(defun dm-init-function ()
   
   (in-package :dm)
   (loop for i from 1 to 3 do (format nil "loop test ~A" i)) ;dummy for the exe generation
   (setq *print-case* :downcase)
   (setq *dm-home-directory* (excl::current-directory))
   
   ;; Create the main application window.
   ;; ("Maker-function" is newly exported for 5.0.1.
   ;; In the meantime, just hardcode the known function name.)
   (let* ((main-window (display-main-window)))
      
     ;; Do some miscellaneous setup and initialization here. 
     (if 
         (not *coda-demo-version*)
         (open-default-rule-set)
       (open-rule-set-fpath 
        (merge-pathnames *dm-home-directory* "Coda3Basic.pal")) )
      ;(when *demo-version* (setq *debugger-hook* #'dm-debugger))
      
      
      ;; Once things are all set up, show the main window.
      (select-window main-window)
      
      ;; If the user aborts, this outer loop will set the
      ;; abort handler back up again and then re-enter the
      ;; general event-handling loop below.
      (loop
       (with-simple-restart
        (abort "Unwind to the top-level event handling loop.")
        
        ;; Catch all otherwise unhandled errors.
        (handler-case
         
         ;; This inner loop processes events one at a time,
         ;; exiting the application when the main window
         ;; has been closed.  (Other tests could be used
         ;; to determine when to exit the app.)
         (loop
          (when (cg.base:closed-stream-p main-window) ;;; (closed-stream-p main-window)   ;;original:  (aclwin:closed-stream-p main-window)
             (print "That's all, folks!")
             
             ;; Return anything other than a window
             ;; to cause the application to exit.
             (return-from dm-init-function))
          
          ;; Handle each message that CG receives.
          (process-single-event))
         
         ;; If a simple warning occurs, just print it
         ;; in the console window, which the user could
         ;; expose if they want to see the warnings.
         (warning (condition)
           (format t "Warning:  ~a" condition))
         
         ;; If any other error occurs (which is not handled
         ;; elsewhere), ask the user what to do.
         (error (condition)
           (case (ask-user-for-choice
                  (format nil "Error:  ~a"
                    condition)
                  ':~continue :~debug
                  nil nil "Unhandled Error")
             
             ;; Offer choices to continue, exit, or debug.
             
             ;; Signaling the abort restart will jump to
             ;; the with-simple-restart above, which will
             ;; re-enter the general event-handling loop.
             (:~continue (abort))
             
             ;; Returning a non-window from this initialization
             ;; function will cause the application to exit.
             ;(:~exit (return-from my-init-function))
             
             ;; The IDE debugger isn't available in a standalone,
             ;; but this will allow use of the TTY debugger
             ;; in the console window.
             (:~debug
              (win:ShowWindow (cg::console-hwnd (app *system*))
                win:SW_NORMAL)
              (break)))))))))

;;-------- main window -----------------------

; the main window class
#+:allegro
(defclass dm-main-window (frame-window) ())
#+:lispworks
(define-interface dm-main-window () ())

#+:allegro
(defmethod user-close ((self dm-main-window))
   (if *demo-version* (dm-exit) (close *dm-main-window*)) )

;displays the main window for dm
#+:lispworks
(defun display-main-window ()
 ; (in-package :dm)
  (display (setq *dm-main-window* 
          (make-instance 'dm-main-window
                         :title "Director Musices"
                         :visible-min-width 400
            :menu-bar-items
              (list 
                (make-instance 'menu
                  :name :file-menu
                  :title "~File"
                  :items
                  (remove nil
                   (list 
                     (make-instance 'menu-item
                       :title "~Open Score ..."
                       :callback #'(lambda (da in) (declare (ignore da in)) (load-score))
                      ; :event-synonym '(control-key #\O)
                       )
                    (make-instance 'menu-item
                      :title "Open ~Performance ..."
                      :callback #'(lambda (da in) (declare (ignore da in)) (load-performance)) )
                    (make-instance 'menu-item
                      :title '"Open ~MIDI file ..."
                      :callback #'(lambda (da in) (declare (ignore da in)) (load-midifile)) )
                   ; menu-separator  
                    (make-instance 'menu-item
                      :title "~Save Score As..."
                      :callback #'(lambda (da in) (declare (ignore da in)) (save-score))
                     ; :event-synonym '(control-key #\S)
                      )
                    (make-instance 'menu-item
                      :title "S~ave Performance As..."
                      :callback #'(lambda (da in) (declare (ignore da in)) (save-performance)) )
                    ;menu-separator  
                    (make-instance 'menu-item
                      :title "Save Performance M~IDI file 1 As..."
                      :callback #'(lambda (da in) (declare (ignore da in)) (save-performance-midifile1)) )
                    (make-instance 'menu-item
                      :title "Save Performance MI~DI file 0 As..."
                      :callback #'(lambda (da in) (declare (ignore da in)) (save-performance-midifile0)) )
                   ; menu-separator  
                    (make-instance 'menu-item
                      :title "Save pDM Score As..."
                      :callback #'(lambda (da in) (declare (ignore da in)) (pdm-save-rule-data)) )

                    (when *notesenses-version*  menu-separator)
                    (when *notesenses-version*
                      (make-instance 'menu-item
                      :title "Save Performance as OTA ..."
                      :callback #'(lambda (da in) (declare (ignore da in)) (sm-save-as-ota)) ))
                    (when *notesenses-version*
                      (make-instance 'menu-item
                      :title "Save Performance as OTA HEX ..."
                        :callback #'(lambda (da in) (declare (ignore da in)) (sm-save-as-ota-hex)) ))
                    (when *notesenses-version*
                      (make-instance 'menu-item
                      :title "~Save Monophonic Ringtones ..."
                      :callback #'(lambda (da in) (declare (ignore da in)) (save-monophonic-ringtones)) ))

                      ;   menu-separator
                         (make-instance 'menu-item
                           :name
                           'exit
                           :title "E~xit"
                           :callback #'(lambda (da in) (declare (ignore da in)) (dm-exit))
                          ; :event-synonym '(alt-key vk-f4)
                          ; :help-string "Exit Director Musices"
                           )
                         )))



                (make-instance 'menu
                  :name :edit-menu
                  :title "Edit"
                  :items
                   (list 
                    (make-instance 'menu-item
                           :name 'cut-command
                           :title "~Cut"
                           :callback 'cut-command
                          ; :event-synonym '(control-key #\X)
                          ; :help-string "Copy contents to clipboard and delete"
                           )
                     (make-instance 'menu-item
                       :name 'copy-command
                       :title "C~opy"
                       :callback 'copy-command
                     ;  :event-synonym '(control-key #\C)
                     ;  :help-string "Copy contents to clipboard"
                       )
                     (make-instance 'menu-item
                       :name 'paste-command
                       :title "~Paste"
                       :callback 'paste-command
                    ;   :event-synonym '(control-key #\V)
                    ;   :help-string "Paste contents from clipboard"
                       )
                   ;  menu-separator
                     (make-instance 'menu-item
                      :title "Tempo ..."
                      :callback #'(lambda (da in) (declare (ignore da in)) (set-tempo-dialog)))
                     (make-instance 'menu-item
                      :title "Octave ..."
                      :callback #'(lambda (da in) (declare (ignore da in)) (change-octave-dialog)))
                     (make-instance 'menu-item
                      :title "Meter ..."
                      :callback #'(lambda (da in) (declare (ignore da in)) (set-meter-dialog)))
                     (make-instance 'menu-item
                      :title "Key ..."
                      :callback #'(lambda (da in) (declare (ignore da in)) (set-key-dialog)))
                     ; menu-separator
                     (make-instance 'menu-item
                      :title "Remove one parameter ..."
                      :callback #'(lambda (da in) (declare (ignore da in)) (remove-one-parameter)))
                     ))              
                
                (make-instance 'menu
                  :name :edit-menu
                  :title "~Rules"
                  :items
                   (list
                    (make-instance 'menu-item
                      :title "Load rules ..."
                      ;:event-synonym '(control-key #\R)
                      :callback #'(lambda (da in) (declare (ignore da in)) (load-rules)))
                    (make-instance 'menu-item
                      :title "Open rule palette ..."
                     ; :event-synonym '(control-key #\R)
                      :callback #'(lambda (da in) (declare (ignore da in)) (open-rule-set)))
                    (make-instance 'menu-item
                      :title "Open default rule palette"
                      :callback #'(lambda (da in) (declare (ignore da in)) (open-default-rule-set)) )
                   ))
                
                (make-instance 'menu
                  :name :edit-menu
                  :title "~Display"
                  :items
                   (list
                    (make-instance 'menu-item
                      :title "Score da indow"
                      :callback #'(lambda (da in) (declare (ignore da in)) (draw-music)) )
                  ;  menu-separator  
                    (make-instance 'menu-item
                      :title "delta Sound Level"
                      :callback #'(lambda (da in) (declare (ignore da in)) (draw-level)) )
                    (make-instance 'menu-item
                      :title "delta Volume"
                      :callback #'(lambda (da in) (declare (ignore da in)) (draw-volume)) )
                  ;  menu-separator  
                    (make-instance 'menu-item
                      :title "delta Tempo"
                      :callback #'(lambda (da in) (declare (ignore da in)) (draw-tempo)) )
                    (make-instance 'menu-item
                      :title "delta Duration (ms)"
                      :callback #'(lambda (da in) (declare (ignore da in)) (draw-ddr)) )
                    (make-instance 'menu-item
                      :title "delta Duration (%)"
                      :callback #'(lambda (da in) (declare (ignore da in)) (draw-ddr%)) )
                  ;  menu-separator  
                    (make-instance 'menu-item
                      :title "Offtime duration (ms)"
                      :callback #'(lambda (da in) (declare (ignore da in)) (draw-droff)) )
                    (make-instance 'menu-item
                      :title "Offtime duration (% of IOI)"
                      :callback #'(lambda (da in) (declare (ignore da in)) (draw-dro%IOI)) )
                  ;  menu-separator  
                    (make-instance 'menu-item
                      :title "Duration"
                      :callback #'(lambda (da in) (declare (ignore da in)) (draw-dr)) )
                    (make-instance 'menu-item
                      :title "Bar duration"
                      :callback #'(lambda (da in) (declare (ignore da in)) (draw-bar-dr)) )
                    (make-instance 'menu-item
                      :title "Beat duration (ms)"
                      :callback #'(lambda (da in) (declare (ignore da in)) (draw-beat-dr)) )
                    (make-instance 'menu-item
                      :title "Beat duration (%)"
                      :callback #'(lambda (da in) (declare (ignore da in)) (draw-beat-dr%)) )
                  ;  menu-separator  
                    (make-instance 'menu-item
                      :title "delta Cent"
                      :callback #'(lambda (da in) (declare (ignore da in)) (draw-dcent)) )
                    (make-instance 'menu-item
                      :title "Vibrato amplitude"
                      :callback #'(lambda (da in) (declare (ignore da in)) (draw-va)) )
                    (make-instance 'menu-item
                      :title "Vibrato frequency"
                      :callback #'(lambda (da in) (declare (ignore da in)) (draw-vf)) )
                    ;;;        (make-instance 'menu-item
                    ;;;          :title "Level envelope"
                    ;;;          :callback #'(lambda (da in) (declare (ignore da in)) (draw-envelope)) )
                    
                    ))
                
                
                (make-instance 'menu
                  :name :edit-menu
                  :title "~Play"
                  :items
                   (list
;;;                      :title "Play preferences ..."
;;;             ;        :callback #'(lambda (da in) (declare (ignore da in)) (play-prefs-dialog)) )
                    ; another item, "Play parameters", has been commented out
                    ; another item, "Check for play" has been commented out
                   ; menu-separator  
                    (make-instance 'menu-item
                      :title "Play"
                      :callback #'(lambda (da in) (declare (ignore da in)) (playlist)) )
                    
;;;                       :title "Play last"
;;;                       :callback #'(lambda (da in) (declare (ignore da in)) (playlast)) )          ; (eval-enqueue '(playlast))) ) *                    ; another item, "Play seq" has been commented out
                   ; menu-separator  
                    (make-instance 'menu-item
                      :title "Print playlist"
                      :callback #'(lambda (da in) (declare (ignore da in)) (print-playlist)) )
;;;                       :title "All notes off"
;;;                       :callback #'(lambda (da in) (declare (ignore da in)) (all-notes-off *active-score* 0)) )
                    ))

                (make-instance 'menu
                  :name :edit-menu
                  :title "~Tools"
                  :items
                   (remove nil
                   (list
;;;                     (make-instance 'menu-item
;;;                       :title "~Analyser"
;;;                       :callback #'(lambda (da in) (declare (ignore da in)) (analyse *active-score*)) )
                    (when (not *demo-version*)
                        (make-instance 'menu-item
                          :title "~inspect Active score"
                          :callback #'(lambda (da in) (declare (ignore da in)) (inspect *active-score*)) ))
                    (when (not *demo-version*)
                        (make-instance 'menu-item
                          :title "mac-to-pc-text"
                          :callback #'(lambda (da in) (declare (ignore da in)) (mac-to-pc-text)) ))
                    (when (not *demo-version*)
                        (make-instance 'menu-item
                          :title "remove-extra-crlf"
                          :callback #'(lambda (da in) (declare (ignore da in)) (remove-extra-crlf)) ))
                    (when (not *demo-version*)
                        (make-instance 'menu-item
                          :title "unix-to-pc-text"
                          :callback #'(lambda (da in) (declare (ignore da in)) (unix-to-pc-text)) ))
                    (make-instance 'menu-item
                      :title "~Exporter..."
                      :callback #'(lambda (da in) (declare (ignore da in)) (make-instance 'exporter :curr-score *active-score*)) )
                    (make-instance 'menu-item
                      :title "Export individual rule data..."
                      :callback #'(lambda (da in) (declare (ignore da in)) (export-individual-rule-data)) )
                   ; menu-separator  
                    (make-instance 'menu-item
                      :title "Reset sound level"
                      :callback #'(lambda (da in) (declare (ignore da in)) (with-waiting-cursor (reset-sound-level))) )
                    ;;0009/af added this item
                    (make-instance 'menu-item
                      :title "Convert chord list to chord ~name"
                      :callback #'(lambda (da in) (declare (ignore da in)) (with-waiting-cursor (convert-chord-list-to-chord-name))) )
                    ;;0009/af added this item
                    (make-instance 'menu-item
                      :title "Distribute ~chord analysis"
                      :callback #'(lambda (da in) (declare (ignore da in)) (with-waiting-cursor (distribute-chord-analysis))) )
                    ;;0009/af added this item
                    (make-instance 'menu-item
                      :title "Distribute ~phrase analysis"
                      :callback #'(lambda (da in) (declare (ignore da in)) (with-waiting-cursor (distribute-phrase-analysis))) )
                    ;;0009/af moved this item
                     (make-instance 'menu-item
                      :title "Rebar"
                       :callback #'(lambda (da in) (declare (ignore da in)) (with-waiting-cursor (rebar))) )
                     (make-instance 'menu-item
                      :title "Make Radio-baton sync track"
                       :callback #'(lambda (da in) (declare (ignore da in)) (with-waiting-cursor (make-radio-baton))) )
                    
                  ;  menu-separator
                    (make-instance 'menu-item
                      :title "Print all score vars"
                      :callback #'(lambda (da in) (declare (ignore da in)) (print-music)) )
                    (make-instance 'menu-item
                      :title "Print all score vars round"
                      :callback #'(lambda (da in) (declare (ignore da in)) (print-music-round)) )
                    
                    (when *notesenses-version*
                      menu-separator)
                    (when *notesenses-version*
                      (make-instance 'menu-item
                      :title "~Midinotevalue to notevalue"
                      :callback #'(lambda (da in) (declare (ignore da in)) (midinotevalue-to-notevalue)) ))
;;;                    (when *notesenses-version*
;;;                      (make-instance 'menu-item
;;;                      :title "Save performance as OTA ..."
;;;                      :callback #'(lambda (da in) (declare (ignore da in)) (sm-save-as-ota)) ))
;;;                    (when *notesenses-version*
;;;                      (make-instance 'menu-item
;;;                      :title "Save performance as OTA HEX ..."
;;;                        :callback #'(lambda (da in) (declare (ignore da in)) (sm-save-as-ota-hex)) ))
;;;                    (when *notesenses-version*
;;;                      (make-instance 'menu-item
;;;                      :title "~Save monophonic ringtones ..."
;;;                      :callback #'(lambda (da in) (declare (ignore da in)) (save-monophonic-ringtones)) ))

                  ;  menu-separator
                    (make-instance 'menu-item
                      :title "~DM var settings ..."
                      :callback #'(lambda (da in) (declare (ignore da in)) (make-dm-var-dialog)) )
                    )))

                (make-instance 'menu
                  :name :help-menu
                  :title "~Help"
                  :items
                   (list
                    (make-instance 'menu-item
                      :title "~Music Performance home page ..."
                      :callback #'(lambda (da in) (declare (ignore da in)) (invoke-html-browser "http://www.speech.kth.se/music/performance/")) )
                    (make-instance 'menu-item
                      :title "~About Director Musices ..."
                      :callback #'(lambda (da in) (declare (ignore da in)) (display-about-window)) )
                    ))
                )))))






#+:allegro
(defun display-main-window ()
   (setq *dm-main-window* 
          (make-window :dm-main-window
            :parent (screen *system*)
            :device 'dm-main-window
            :exterior (make-box 0 0 (- (width (screen *system*)) 0)
                        ;(- (height (screen *system*)) 10)
                        60)
            :border :frame
            :close-button t
            :cursor-name :arrow-cursor
            :maximize-button t
            :background-color gray ;(make-rgb :red 64 :green 128 :blue 128)
            :minimize-button t
            :pop-up nil
            :resizable t
            :scrollbars nil
            :state :maximized    ;;; :normal ;; :visible in 5.0.1
            :status-bar nil
            :system-menu t
            :title *dm-main-window-title*
            :title-bar t
            :toolbar nil
            ;:form-p form-p
            ;:path nil
            :help-string nil
            :menu
            (make-window :default-menu
              :device 'menu-bar
              :parent (screen *system*)
              :menu-items
              (list 
                (make-instance 'menu-item
                  :name :file-menu
                  :title "~File"
                  :value
                  (open-menu
                   (remove nil
                   (list 
                     (make-instance 'menu-item
                       :title "~Open Score ..."
                       :value #'(lambda (win) (declare (ignore win)) (load-score))
                       :available-p t
                       :event-synonym '(control-key #\O)
                       :selected-p nil
                       :font nil)
                    (make-instance 'menu-item
                      :title "Open ~Performance ..."
                      :value #'(lambda (win) (declare (ignore win)) (load-performance))
                      :available-p t
                      :selected-p nil)
                    (make-instance 'menu-item
                      :title '"Open ~MIDI file ..."
                      :value #'(lambda (win) (declare (ignore win)) (load-midifile))
                      :available-p t
                      :selected-p nil)
                    menu-separator  
                    (make-instance 'menu-item
                      :title "~Save Score As..."
                      :value #'(lambda (win) (declare (ignore win)) (save-score))
                      :available-p t
                      :event-synonym '(control-key #\S)
                      :selected-p nil)
                    (make-instance 'menu-item
                      :title "S~ave Performance As..."
                      :value #'(lambda (win) (declare (ignore win)) (save-performance))
                      :available-p t
                      :selected-p nil)
                    menu-separator  
                    (make-instance 'menu-item
                      :title "Save Performance M~IDI file 1 As..."
                      :value #'(lambda (win) (declare (ignore win)) (save-performance-midifile1))
                      :available-p t
                      :selected-p nil)
                    (make-instance 'menu-item
                      :title "Save Performance MI~DI file 0 As..."
                      :value #'(lambda (win) (declare (ignore win)) (save-performance-midifile0))
                      :available-p t
                      :selected-p nil)
                    menu-separator  
                    (make-instance 'menu-item
                      :title "Save pDM Score As..."
                      :value #'(lambda (win) (declare (ignore win)) (pdm-save-rule-data))
                      :available-p t
                      :selected-p nil)

                    (when *notesenses-version*
                      menu-separator)
                    (when *notesenses-version*
                      (make-instance 'menu-item
                      :title "Save Performance as OTA ..."
                      :value #'(lambda nil (sm-save-as-ota)) ))
                    (when *notesenses-version*
                      (make-instance 'menu-item
                      :title "Save Performance as OTA HEX ..."
                        :value #'(lambda nil (sm-save-as-ota-hex)) ))
                    (when *notesenses-version*
                      (make-instance 'menu-item
                      :title "~Save Monophonic Ringtones ..."
                      :value #'(lambda nil (save-monophonic-ringtones)) ))

;;;                     menu-separator
;;;                          (make-instance 'menu-item
;;;                                :name 'new-text-editor
;;;                                :title "~New Text"
;;;                                :value 'new-text-editor
;;;                                :selected nil
;;;                                :available t
;;;                                :event-synonym nil
;;;                            :help-string "New editor")
;;;                          (make-instance 'menu-item
;;;                            :name 'open-text-file
;;;                            :title "Open ~Text File..."
;;;                            :value 'open-text-file
;;;                            :selected nil
;;;                            :available t
;;;                            :event-synonym nil
;;;                            :help-string "Open a file")
;;;                          (make-instance 'menu-item
;;;                            :name :save
;;;                            :title "Save T~ext File"
;;;                            :value 'save-text-file
;;;                            :selected nil
;;;                            :available t
;;;                            :event-synonym nil
;;;                            :help-string "Save to file")
;;;                          (make-instance 'menu-item
;;;                            :name 'save-as-text-file
;;;                            :title "Save Text File ~As..."
;;;                            :value 'save-as-text-file
;;;                            :selected nil
;;;                            :available t
;;;                            :event-synonym nil
;;;                            :help-string "Save to new file")
                         menu-separator
                         (make-instance 'menu-item
                           :name
                           'exit
                           :title "E~xit"
                           :value #'(lambda (win) (declare (ignore win)) (dm-exit))
                           ;;;'excl:exit
                           :selected nil
                           :available t
                           :event-synonym '(alt-key vk-f4)
                           :help-string "Exit Director Musices")))
                       'pull-down-menu (screen *system*)
                           :name :file-menu
                       :on-click 'funcall-menu-item-with-window)
                      :selected nil
                      :available t
                      :event-synonym nil
                      :help-string nil )
                (make-instance 'menu-item
                  :name :edit-menu
                  :title "~Edit"
                  :value
                  (open-menu
                   (list (make-instance 'menu-item
                           :name 'cut-command
                           :title "~Cut"
                           :value 'cut-command
                           :selected nil
                           :available t
                           :event-synonym '(control-key #\X)
                           :help-string "Copy contents to clipboard and delete")
                     (make-instance 'menu-item
                       :name 'copy-command
                       :title "C~opy"
                       :value 'copy-command
                       :selected nil
                       :available t
                       :event-synonym '(control-key #\C)
                       :help-string "Copy contents to clipboard")
                     (make-instance 'menu-item
                       :name 'paste-command
                       :title "~Paste"
                       :value 'paste-command
                       :selected nil
                       :available t
                       :event-synonym '(control-key #\V)
                       :help-string "Paste contents from clipboard")
                     menu-separator
                     (make-instance 'menu-item
                      :title "Tempo ..."
                      :value #'(lambda (win) (declare (ignore win)) (set-tempo-dialog)))
                     (make-instance 'menu-item
                      :title "Octave ..."
                      :value #'(lambda (win) (declare (ignore win)) (change-octave-dialog)))
                     (make-instance 'menu-item
                      :title "Meter ..."
                      :value #'(lambda (win) (declare (ignore win)) (set-meter-dialog)))
                     (make-instance 'menu-item
                      :title "Key ..."
                      :value #'(lambda (win) (declare (ignore win)) (set-key-dialog)))
                      menu-separator
                     (make-instance 'menu-item
                      :title "Remove one parameter ..."
                      :value #'(lambda (win) (declare (ignore win)) (remove-one-parameter)))

                     )
                   'pull-down-menu (screen *system*)
                   :name :edit-menu
                   :on-click 'funcall-menu-item-with-window)
                  :selected nil
                  :available t
                  :event-synonym nil
                  :help-string nil)
                
                
                (make-instance 'menu-item
                  :name :edit-menu
                  :title "~Rules"
                  :value
                  (open-menu
                   (list
                    (make-instance 'menu-item
                      :title "Load rules ..."
                      ;:event-synonym '(control-key #\R)
                      :value #'(lambda nil (load-rules)))
                    (make-instance 'menu-item
                      :title "Open rule palette ..."
                      :event-synonym '(control-key #\R)
                      :value #'(lambda nil (open-rule-set)))
                    (make-instance 'menu-item
                      :title "Open default rule palette"
                      :value #'(lambda nil (open-default-rule-set))))
                   'pull-down-menu (screen *system*)
                   :name :edit-menu
                   :on-click 'funcall-menu-item)
                  :selected nil
                  :available t
                  :event-synonym nil
                  :help-string nil)
                
                
                (make-instance 'menu-item
                  :name :edit-menu
                  :title "~Display"
                  :value
                  (open-menu
                   (list
                    (make-instance 'menu-item
                      :title "Score window"
                      :value #'(lambda nil (draw-music)) )
                    menu-separator  
                    (make-instance 'menu-item
                      :title "delta Sound Level"
                      :value #'(lambda nil (draw-level)) )
                    (make-instance 'menu-item
                      :title "delta Volume"
                      :value #'(lambda nil (draw-volume)) )
                    menu-separator  
                    (make-instance 'menu-item
                      :title "delta Tempo"
                      :value #'(lambda nil (draw-tempo)) )
                    (make-instance 'menu-item
                      :title "delta Duration (ms)"
                      :value #'(lambda nil (draw-ddr)) )
                    (make-instance 'menu-item
                      :title "delta Duration (%)"
                      :value #'(lambda nil (draw-ddr%)) )
                    menu-separator  
                    (make-instance 'menu-item
                      :title "Offtime duration (ms)"
                      :value #'(lambda nil (draw-droff)) )
                    (make-instance 'menu-item
                      :title "Offtime duration (% of IOI)"
                      :value #'(lambda nil (draw-dro%IOI)) )
                    menu-separator  
                    (make-instance 'menu-item
                      :title "Duration"
                      :value #'(lambda nil (draw-dr)) )
                    (make-instance 'menu-item
                      :title "Bar duration"
                      :value #'(lambda nil (draw-bar-dr)) )
                    (make-instance 'menu-item
                      :title "Beat duration (ms)"
                      :value #'(lambda nil (draw-beat-dr)) )
                    (make-instance 'menu-item
                      :title "Beat duration (%)"
                      :value #'(lambda nil (draw-beat-dr%)) )
                    menu-separator  
                    (make-instance 'menu-item
                      :title "delta Cent"
                      :value #'(lambda nil (draw-dcent)) )
                    (make-instance 'menu-item
                      :title "Vibrato amplitude"
                      :value #'(lambda nil (draw-va)) )
                    (make-instance 'menu-item
                      :title "Vibrato frequency"
                      :value #'(lambda nil (draw-vf)) )
                    ;;;        (make-instance 'menu-item
                    ;;;          :title "Level envelope"
                    ;;;          :value #'(lambda nil (draw-envelope)) )
                    
                    )
                   'pull-down-menu (screen *system*)
                   :name :edit-menu
                   :on-click 'funcall-menu-item)
                  :selected nil
                  :available t
                  :event-synonym nil
                  :help-string nil)
                
                
                (make-instance 'menu-item
                  :name :edit-menu
                  :title "~Play"
                  :value
                  (open-menu
                   (list
                    (make-instance 'menu-item
                      :available-p nil
                      :title "Play preferences ..."
                      :value #'(lambda nil (play-prefs-dialog)) )
                    ; another item, "Play parameters", has been commented out
                    ; another item, "Check for play" has been commented out
                    menu-separator  
                    (make-instance 'menu-item
                      :title "Play"
                      :value #'(lambda nil (playlist)) )
                    
                    ;; :command-key #\U
                    ;; :style :bold)
;;;                     (make-instance 'menu-item
;;;                       :title "Play last"
;;;                       :value #'(lambda nil (playlast)) )          ; (eval-enqueue '(playlast))) ) ********
                    ; another item, "Play seq" has been commented out
                    menu-separator  
                    (make-instance 'menu-item
                      :available-p t
                      :title "Print playlist"
                      :value #'(lambda nil (print-playlist)) )
;;;                     menu-separator
;;;                     (make-instance 'menu-item
;;;                       :available-p t
;;;                       :title "All notes off"
;;;                       :value #'(lambda nil (all-notes-off *active-score* 0)) )
                    ;:command-key #\-
                    )
                   'pull-down-menu (screen *system*)
                   :name :edit-menu
                   :on-click 'funcall-menu-item)
                  :selected nil
                  :available t
                  :event-synonym nil
                  :help-string nil)
                (make-instance 'menu-item
                  :name :edit-menu
                  :title "~Tools"
                  :value
                  (open-menu
                   (remove nil
                   (list
;;;                     (make-instance 'menu-item
;;;                       :title "~Analyser"
;;;                       :value #'(lambda nil (analyse *active-score*)) )
                    (when (not *demo-version*)
                        (make-instance 'menu-item
                          :title "~inspect Active score"
                          :value #'(lambda nil (inspect *active-score*)) ))
                    (when (not *demo-version*)
                        (make-instance 'menu-item
                          :title "mac-to-pc-text"
                          :value #'(lambda nil (mac-to-pc-text)) ))
                    (when (not *demo-version*)
                        (make-instance 'menu-item
                          :title "remove-extra-crlf"
                          :value #'(lambda nil (remove-extra-crlf)) ))
                    (when (not *demo-version*)
                        (make-instance 'menu-item
                          :title "unix-to-pc-text"
                          :value #'(lambda nil (unix-to-pc-text)) ))
                    (make-instance 'menu-item
                      :title "~Exporter..."
                      :value #'(lambda nil (make-instance 'exporter :curr-score *active-score*)) )
                    (make-instance 'menu-item
                      :title "Export individual rule data..."
                      :value #'(lambda nil (export-individual-rule-data)) )
                    menu-separator  
                    (make-instance 'menu-item
                      :title "Reset sound level"
                      :value #'(lambda nil (with-waiting-cursor (reset-sound-level))) )
                    ;;0009/af added this item
                    (make-instance 'menu-item
                      :title "Convert chord list to chord ~name"
                      :value #'(lambda nil (with-waiting-cursor (convert-chord-list-to-chord-name))) )
                    ;;0009/af added this item
                    (make-instance 'menu-item
                      :title "Distribute ~chord analysis"
                      :value #'(lambda nil (with-waiting-cursor (distribute-chord-analysis))) )
                    ;;0009/af added this item
                    (make-instance 'menu-item
                      :title "Distribute ~phrase analysis"
                      :value #'(lambda nil (with-waiting-cursor (distribute-phrase-analysis))) )
                    ;;0009/af moved this item
                     (make-instance 'menu-item
                      :title "Rebar"
                       :value #'(lambda nil (with-waiting-cursor (rebar))) )
                     (make-instance 'menu-item
                      :title "Make Radio-baton sync track"
                       :value #'(lambda nil (with-waiting-cursor (make-radio-baton))) )
                    
                    menu-separator
                    (make-instance 'menu-item
                      :title "Print all score vars"
                      :value #'(lambda nil (print-music)) )
                    (make-instance 'menu-item
                      :title "Print all score vars round"
                      :value #'(lambda nil (print-music-round)) )
                    
                    (when *notesenses-version*
                      menu-separator)
                    (when *notesenses-version*
                      (make-instance 'menu-item
                      :title "~Midinotevalue to notevalue"
                      :value #'(lambda nil (midinotevalue-to-notevalue)) ))
;;;                    (when *notesenses-version*
;;;                      (make-instance 'menu-item
;;;                      :title "Save performance as OTA ..."
;;;                      :value #'(lambda nil (sm-save-as-ota)) ))
;;;                    (when *notesenses-version*
;;;                      (make-instance 'menu-item
;;;                      :title "Save performance as OTA HEX ..."
;;;                        :value #'(lambda nil (sm-save-as-ota-hex)) ))
;;;                    (when *notesenses-version*
;;;                      (make-instance 'menu-item
;;;                      :title "~Save monophonic ringtones ..."
;;;                      :value #'(lambda nil (save-monophonic-ringtones)) ))

                    menu-separator
                    (make-instance 'menu-item
                      :title "~DM var settings ..."
                      :value #'(lambda nil (make-dm-var-dialog)) )
;;;                     (make-instance 'menu-item
;;;                       :title "Select music font ..."
;;;                       :value #'(lambda nil (let ((font (ask-user-for-font)))
;;;                                               (if font (set-dm-var 'music-font-face
;;;                                                          (font-face font))) )))
;;;                     (make-instance 'menu-item
;;;                       :title "~Musical parameters..."
;;;                       :value #'(lambda nil (make-musical-parameters-dialog)) )
;;;                     menu-separator
;;;                     (make-instance 'menu-item
;;;                       :title "~Pattern matching"
;;;                       :value #'(lambda nil (make-pattern-matching-dialog)) )
                    ))
                   'pull-down-menu (screen *system*)
                   :name :edit-menu
                   :on-click 'funcall-menu-item)
                  :selected nil
                  :available t
                  :event-synonym nil
                  :help-string nil)
;;;                 (make-instance 'menu-item
;;;                   :name :edit-menu
;;;                   :title "Midi~share"
;;;                   :value
;;;                   (open-menu
;;;                    (list
;;;                     (make-instance 'menu-item
;;;                       :title "open Midishare (DM client)"
;;;                       :value #'(lambda nil (midi-open)) )
;;;                     (make-instance 'menu-item
;;;                       :title "close Midishare (DM client)"
;;;                       :value #'(lambda nil (midi-close)) )
;;;                     (make-instance 'menu-item
;;;                       :title "Midishare toolbox..."
;;;                       :value #'(lambda nil (make-midishare-toolbox)) )
;;;                     )
;;;                    'pull-down-menu (screen *system*)
;;;                    :name :edit-menu
;;;                    :on-click 'funcall-menu-item)
;;;                   :selected nil
;;;                   :available nil
;;;                   :event-synonym nil
;;;                   :help-string nil)
                (make-instance 'menu-item
                  :name :help-menu
                  :title "~Help"
                  :value
                  (open-menu
                   (list
                    (make-instance 'menu-item
                      :title "~Music Performance home page ..."
                      :value #'(lambda nil (invoke-html-browser "http://www.speech.kth.se/music/performance/")) )
                    (make-instance 'menu-item
                      :title "~About Director Musices ..."
                      :value #'(lambda nil (display-about-window)) )
                    )
                   'pull-down-menu (screen *system*)
                   :name :help-menu
                   :on-click 'funcall-menu-item)
                  :selected nil
                  :available t
                  :event-synonym nil
                  :help-string nil)
                ))
            )))

;--------------------------------------------------------------
;; Message window
;; not working right now
;;-------------------------------------------------------------

(defvar *message-window*)

#+(and :mswindows :allegro)
(defun display-message-window ()
  (setq  *message-window*
    (make-window :form3
           :parent *dm-main-window*
           :device 'text-edit-window
           :exterior (make-box 254 219 883 664)
           :border :frame
           :close-button t
           :cursor-name :arrow-cursor
           :device 'text-edit-window
           :form-state :normal
           :maximize-button t
           :minimize-button t
           :name :form3
           :package-name :dm
           :pop-up nil
           :resizable t
           :scrollbars nil
           :state :normal
           :status-bar nil
           :system-menu t
           :title "Form3"
           :title-bar t
           :toolbar nil
           :help-string nil
      ))
   (setq *standard-output* *message-window*)
   (setq *standard-input* *message-window*)
   )
