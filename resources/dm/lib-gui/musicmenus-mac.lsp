;;;-*-Mode: LISP; Package: DM -*-
;;
;; *********************************************************
;;   creates the menus in the Lisp environment main window
;;   Mac version
;; *********************************************************
;;
;; filename: MusicMenusMac.lsp     this is just the old MusicMenus for Mac, renamed
;;
;; all menus in Rulle
;; 9201 cl/Anders Friberg
;; 2002.010.04 Stephane Letz (Grame) Stop command in Play menu


(in-package :dm)

#|
  (menu-deinstall (find-menu "Play"))
  (menu-deinstall (find-menu "Music"))
  (menu-deinstall (find-menu "Rules"))
  (menu-deinstall (find-menu "Display"))
  (menu-deinstall (find-menu "Utilities"))
  )
|#

(defun install-music-menus ()
  (menu-install
   (make-instance 'menu
     :menu-title "Music"
     :menu-items
     (list 
      (make-instance 'menu-item
        :menu-item-title "Open score ..."
        :menu-item-action #'(lambda nil (load-score))
        :command-key #\M
        :style :bold)
      (make-instance 'menu-item
        :menu-item-title "Open performance ..."
        :menu-item-action #'(lambda nil (load-performance)) )
      (make-instance 'menu-item
        :menu-item-title "Open MIDI file ..."
        :menu-item-action #'(lambda nil (load-midifile)) )
      (make-instance 'menu-item
        :menu-item-title "-"
        :disabled t)
      (make-instance 'menu-item
        :menu-item-title "Save score as ..."
        :menu-item-action #'(lambda nil (save-score)))
      (make-instance 'menu-item
        :menu-item-title "Save performance as ..."
        :menu-item-action #'(lambda nil (save-performance)))
      (make-instance 'menu-item
        :menu-item-title "Save performance MIDI file1 as..."
        :menu-item-action 
        #'(lambda nil (save-performance-midifile1))
        :disabled nil)
      (make-instance 'menu-item
        :menu-item-title "-"
        :disabled t)
      (make-instance 'menu-item
        :menu-item-title "Save pDM Score As..."
        :menu-item-action #'(lambda nil (pdm-save-rule-data)))
      )))
  
  (menu-install
   (make-instance 
     'menu
     :menu-title "Rules"
     :menu-items
     (list 
      (make-instance 'menu-item
        :menu-item-title "Load rules ..."
        :menu-item-action #'(lambda nil (load-rules)) )
      (make-instance 'menu-item
        :menu-item-title "Open rule palette ..."
        :menu-item-action #'(lambda nil (open-rule-set)) )
      (make-instance 'menu-item
        :menu-item-title "Open default rule palette"
        :menu-item-action #'(lambda nil (open-default-rule-set)) )
      
      )))
  
  (menu-install
   (make-instance 
     'menu
     :menu-title "Display"
     :menu-items
     (list 
      (make-instance 'menu-item
        :menu-item-title "Score window"
        :menu-item-action #'(lambda nil (draw-music)) )
      (make-instance 'menu-item :menu-item-title "-" :disabled t)
      
      (make-instance 'menu-item
        :menu-item-title "delta Sound Level"
        :menu-item-action #'(lambda nil (draw-level)) )
      (make-instance 'menu-item
        :menu-item-title "delta Volume"
        :menu-item-action #'(lambda nil (draw-volume)) )
      (make-instance 'menu-item :menu-item-title "-" :disabled t)
      
      (make-instance 'menu-item
        :menu-item-title "delta Tempo"
        :menu-item-action #'(lambda nil (draw-tempo)) )
      (make-instance 'menu-item
        :menu-item-title "delta Duration (ms)"
        :menu-item-action #'(lambda nil (draw-ddr)) )
      (make-instance 'menu-item
        :menu-item-title "delta Duration (%)"
        :menu-item-action #'(lambda nil (draw-ddr%)) )
      (make-instance 'menu-item :menu-item-title "-" :disabled t)
      
      (make-instance 'menu-item
        :menu-item-title "Offtime duration (ms)"
        :menu-item-action #'(lambda nil (draw-droff)) )
      (make-instance 'menu-item
        :menu-item-title "Offtime duration (% of IOI)"
        :menu-item-action #'(lambda nil (draw-dro%IOI)) )
      (make-instance 'menu-item :menu-item-title "-" :disabled t)
      
      (make-instance 'menu-item
        :menu-item-title "Duration"
        :menu-item-action #'(lambda nil (draw-dr)) )
      (make-instance 'menu-item
        :menu-item-title "Bar duration"
        :menu-item-action #'(lambda nil (draw-bar-dr)) )
      (make-instance 'menu-item
        :menu-item-title "Beat duration (ms)"
        :menu-item-action #'(lambda nil (draw-beat-dr)) )
      (make-instance 'menu-item
        :menu-item-title "Beat duration (%)"
        :menu-item-action #'(lambda nil (draw-beat-dr%)) )
      (make-instance 'menu-item :menu-item-title "-" :disabled t)
      
      (make-instance 'menu-item
        :menu-item-title "delta Cent"
        :menu-item-action #'(lambda nil (draw-dcent)) )
      (make-instance 'menu-item
        :menu-item-title "Vibrato amplitude"
        :menu-item-action #'(lambda nil (draw-va)) )
      (make-instance 'menu-item
        :menu-item-title "Vibrato frequency"
        :menu-item-action #'(lambda nil (draw-vf)) )
      ;                      (make-instance 'menu-item
      ;                                     :menu-item-title "Level envelope"
      ;                                     :menu-item-action #'(lambda nil (draw-envelope)) )
      (make-instance 'menu-item :menu-item-title "-" :disabled t)
      
      (make-instance 'menu-item
        :menu-item-title "Draw parameters ..."
        :menu-item-action #'(lambda nil (edit-draw-parameters))
        :disabled t)
      (make-instance 'menu-item
        :menu-item-title "Draw preferences ..."
        :menu-item-action #'(lambda nil (draw-prefs-dialog))
        :disabled t)
      )))
  
  
  (menu-install
   (make-instance 'menu
     :menu-title "Play"
     :menu-items
     (list 
      (make-instance 'menu-item
        :menu-item-title "Play preferences ..."
        :menu-item-action #'(lambda nil (play-prefs-dialog))
        :disabled t)
      ; (make-instance 'menu-item
      ;                :menu-item-title "Play parameters ..."
      ;                :menu-item-action #'(lambda nil ())
      ;                :disabled t)
      ; (make-instance 'menu-item
      ;                :menu-item-title "Check for play"
      ;                :menu-item-action #'(lambda nil ())
      ;                :disabled t)
      (make-instance 'menu-item :menu-item-title "-" :disabled t)
      
      (make-instance 'menu-item
        :menu-item-title "Play"
        :menu-item-action 
        #'(lambda nil (eval-enqueue '(playlist)))
        :command-key #\1
        :style :bold)

       (make-instance 'menu-item
        :menu-item-title "Stop"
        :command-key #\2
        :menu-item-action #'(lambda nil (stoplist)) )

      (make-instance 'menu-item
        :menu-item-title "Play last"
        :command-key #\3
        :menu-item-action
        #'(lambda nil (eval-enqueue '(playlast))) )
      ;(make-instance 'menu-item
      ;               :menu-item-title "Play seq"
      ;               :menu-item-action
      ;               #'(lambda nil (eval-enqueue '(playlistseq))) )
      (make-instance 'menu-item
        :menu-item-title "Print playlist"
        :menu-item-action #'(lambda nil (print-playlist)) )

  
    
   
      (make-instance 'menu-item :menu-item-title "-" :disabled t)
    
      (make-instance 'menu-item
        :menu-item-title "All notes off"
        :menu-item-action #'(lambda nil (all-notes-off *active-score* 0))
        :command-key #\-
        :disabled nil)
      )))
  
  
  (menu-install
   (make-instance 'menu
     :menu-title "Utilities"
     :menu-items
     (list 
      ;(make-instance 'menu-item :menu-item-title "SCORE EDIT" :disabled t)
      (make-instance 'menu-item
        :menu-item-title "Tempo ..."
        :menu-item-action #'(lambda nil (set-tempo-dialog))
        :disabled nil)
      (make-instance 'menu-item
        :menu-item-title "Octave ..."
        :menu-item-action #'(lambda nil (change-octave-dialog))
        :disabled nil)
      (make-instance 'menu-item
        :menu-item-title "Meter ..."
        :menu-item-action #'(lambda nil (set-meter-dialog))
        :disabled nil)
      (make-instance 'menu-item
        :menu-item-title "Key ..."
        :menu-item-action #'(lambda nil (set-key-dialog))
        :disabled nil)
      (make-instance 'menu-item :menu-item-title "-" :disabled t)
      
      (make-instance 'menu-item
        :menu-item-title "Reset sound level"
        :menu-item-action #'(lambda nil (with-waiting-cursor (reset-sound-level))) 
        :disabled nil )
      (make-instance 'menu-item
        :menu-item-title "Convert chord list to chord name"
        :menu-item-action #'(lambda nil (with-waiting-cursor (convert-chord-list-to-chord-name))) 
        :disabled nil )
      (make-instance 'menu-item
        :menu-item-title "Distribute chord analysis"
        :menu-item-action #'(lambda nil (with-waiting-cursor (distribute-chord-analysis))) 
        :disabled nil )
      (make-instance 'menu-item
        :menu-item-title "Distribute phrase analysis"
        :menu-item-action #'(lambda nil (with-waiting-cursor (distribute-phrase-analysis)))
        :disabled nil )
      (make-instance 'menu-item
        :menu-item-title "Rebar"
        :menu-item-action #'(lambda nil (rebar))
        :disabled nil)
      (make-instance 'menu-item :menu-item-title "-" :disabled t)
      
      (make-instance 'menu-item
        :menu-item-title "Print all score vars"
        :menu-item-action #'(lambda nil (print-music)))
      (make-instance 'menu-item
        :menu-item-title "Print all score vars round"
        :menu-item-action #'(lambda nil (print-music-round)) )
      
      (make-instance 'menu-item :menu-item-title "-" :disabled t)
      
      (make-instance 'menu-item
        :menu-item-title "Export individual rule data..."
        :menu-item-action #'(lambda nil (export-individual-rule-data)))

      
      )))
  )
