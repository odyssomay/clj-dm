;;;-*-Mode: LISP; Package: DM -*-

;;;A tool for exporting the note effects of each individual
;;;rule - to be used in the real-time expressive sequencer in EyesWeb
;;;one loop through the music for each rule
;;;the rules and quantities are taken from the top rule dialog window
;;;2002-10-14/Anders Friberg

;;;-----------FORMAT----------------
;;;text file tabbed
;;;2 lines header:
;;;
;;;1st header line specify number of columns
;;;
;;;2nd header line
;;;first is subdivision ticks per quarter (480)
;;;following numbers specify binary which parameter is controlled 
;;;by that column: 
;;;1 = tempo (beats per min), 
;;;2 = legato (1=legato, 0.5=staccato)
;;;4 = key velocity (probably better with sound level)
;;;
;;;rest of lines
;;;column (mandatory part)
;;;1 starting points in ticks
;;;2 nominal tempo (average tempo)
;;;3 tempo dev from average
;;;4 average legato
;;;5 legato dev
;;;6 average key vel (sound level better)
;;;7 velocity dev
;;;
;;;rest of column left to the rules but it has to be splitted
;;;into deviations of tempo, legato and sound level
;;;
;;;Example
;;;
;;;13												
;;;480	1	0	2	0	4	0	1	            4      	1     	2	1	4
;;;0	      93.5	0	1	0	64	0	-0.893545255	3.01233312	-2.43514108	-0.097185176	-2.116248625	-7.65561672
;;;1440	93.5	0	1	0	64	0	5.73794995	     -4.69953	1.547429872	0	3.032760977	6.4260144
;;;1920	93.5	0	1	0	64	0	1.728234686	     -4.95772152	1.547429872	0	4.337687823	9.75934848
;;;2880	93.5	0	1	0	64	0	1.728234686	     -4.95772152	1.547429872	0	6.20819215	14.3851992
;;;3840	93.5	0	1	0	64	0	-2.092799329	10.4914764	1.547429902	0	6.999339629	16.28996088
;;;-----------------------------------------------------------------------------------

(in-package :dm)



;;main function from the menu
;;opens a file save dialog
(defun export-individual-rule-data ()
   (let ((fpath (show-dialog-for-saving-files-PD "Save individual rule data (EyesWeb format)"
                 :directory (merge-pathnames (make-pathname :type "dmird") (score-filename *active-score*))
                 :extensions '(("MUS score files" . "*.dmird")("All files" . "*.*")) )))
     (if fpath (with-waiting-cursor 
                 (save-individual-rule-data-fpath fpath) )
       (print "I could not save the file") )) 
  )

(defun save-individual-rule-data-fpath (fpath)
  (with-open-file (ofile fpath :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)
      (print-individual-rule-data-stream ofile) )
  (if (get-dm-var 'verbose-i/o)
      (print-ll "Active score saved as individual rule data in " fpath))
  )

;prints a tab delimited list of perf data for each rule
(defun print-individual-rule-data-stream (stream)
  (let ((mm 120) 
        (bigl '())
        (headerl (list (get-dm-var 'output-midi-tick-res) 1 0 2 0 4 0))
        (n (number-of-segments *active-score*))
        (ticks (get-dm-var 'output-midi-tick-res))
        (tickcount 0.0) )
    (let ((tickl (make-array n :element-type 'integer))
          (tempol (make-array n :element-type 'single-float))
          (tempodevl (make-array n :element-type 'integer :initial-element 0))
          (artl (make-array n :element-type 'integer :initial-element 1))
          (artdevl (make-array n :element-type 'integer :initial-element 0))
          (sll (make-array n :element-type 'integer :initial-element 64))
          (sldevl (make-array n :element-type 'integer :initial-element 0))
          )
          
    ;;---first columns---
    (each-note
     (if (this 'mm) (setq mm (this 'mm))) ;update nominal tempo
     (setf (aref tickl *i*) (round tickcount))
     (incf tickcount (/ (* ticks mm (this 'ndr)) 60000.0)) ;update tick counter
     (setf (aref tempol *i*) (float mm)) )
    (newr bigl tickl)
    (newr bigl tempol)
    (newr bigl tempodevl)
    (newr bigl artl)
    (newr bigl artdevl)
    (newr bigl sll)
    (newr bigl sldevl)
   )
    ;;---rule columns---
    ;;one loop through the music for each rule
    ;;the rules and quantities are taken from te top rule window
    (dolist (rule (rule-list (get-top-rule-window)))
      ;a list container for each parameter first = bin indication of
      ;which parameter as describe above
      (let ((dt (make-array n :element-type 'single-float))
            (dsl (make-array n :element-type 'single-float))
            (art (make-array n :element-type 'single-float))
            )
        (print rule)
        (reset-music)
        (apply (car rule) (cdr rule))
        (NORMALIZE-SL)
        (NORMALIZE-DR)
        (setq headerl (append headerl '(1 2 4)))
        (each-note
         (if (this 'mm) (setq mm (this 'mm))) ;update nominal tempo
         (setf (aref dt *i*) (- (* mm (/ (this 'ndr)(this 'dr))) mm)) ;delta tempo variations 
         (setf (aref art *i*) (/ (- (this-dr) (or (this 'dro) 0.0))   ;delta articulation
                      (this-dr) ))
         (setf (aref dsl *i*) (or (this 'sl) 0.0)) )                  ;delta sound level 
        (newr bigl dt)
        (newr bigl art)
        (newr bigl dsl)
        ))
    
    ;---printing-------------
    (format stream "~A~A" (length headerl) #\newline)
    (print-tab-list headerl stream)
    (print-bigl bigl n stream) 
     ))

(defun print-bigl (l n stream)
  (let ((m (length l)))
    (dotimes (i  n)
      (dotimes (j m)   
        (format stream "~A~A" (aref (nth j l) i) #\tab) )
      (format stream "~A"  #\newline)
      )))

(defun print-tab-list (l stream)
  (let ((m (length l)))
      (dotimes (j m)   
        (format stream "~A~A" (nth j l) #\tab) )
      (format stream "~A"  #\newline)
      ))
  
       
