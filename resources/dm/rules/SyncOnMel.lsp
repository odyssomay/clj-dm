
;_________________  synchronization rules ________________


;;; Make a new melody out of all voices. The notes in this melody are selected
;;; as following:
;;; (1) More than one onset at the same time : the voice with the shortest
;;;     note (including following rests and bind in the note) is selected.
;;; (2) if there are several notes with the same duration from above : select
;;;     the note with the highest melodic charge.
;;; On this new melody all rules are applied and then the rest of the voices
;;; are syncronized according to this one


;; 9201 cl /af
;; 960205/af Sync the last note offset
;; 000914/af new function: simple-sync
;; 101213/af adding accent-dr marks for Erica and Richard
;; 111116/af adding accent-h,m,c marks for Erica and Richard


(in-package :dm)

(defvar *sync-dro?* nil)
(defvar *sync-voice* "sync-track")

(defun sync-voice-p ()
  (if (eq *sync-voice* *vlist*) t nil))


;;---- top level ------------------------

;; evaluate for the simple-mel-sync
(defun apply-rule-list-simple-mel-sync (rule-list)
   (add-one-track *active-score* (simple-sync-make-mel))
   (rule-apply-list rule-list)
   (sync-note1)
   (remove-one-track *active-score* (1- (length (track-list *active-score*))))
  )

(defun apply-rule-list-melodic-sync (rule-list)
   (add-one-track *active-score* (sync-make-mel))
   (rule-apply-list rule-list)
   (sync-note1)
   (remove-one-track *active-score* (1- (length (track-list *active-score*))))
   )

(defun make-syncmel ()
 (add-one-track *active-score* (sync-make-mel)) )
  
(defun make-simple-syncmel ()
    (add-one-track *active-score* (simple-sync-make-mel)) )

(defun remove-last-track ()
  (remove-one-track *active-score* (1- (length (track-list *active-score*)))) )
  
;;---------------------------------------------



;;make the extracted melody in "sync-track"
;;
;;p-each-note
;;  minl = min of this oondr's
;;  cond
;;      no minl and first note       (first is pause)
;;         put in the shortest rest
;;         update last-from-voice
;;      no minl and all rests
;;         put in the shortest rest
;;         CHECK if prev note still is on add dur to that one
;;      no minl                      ("not minl and prev bind ? ")
;;         put in the shortest note
;;      minl                         (the normal case)
;;         if mc set from-voice to max mc in minl
;;            else if last-voice is in minl continue with same voice
;;                    else take the first in minl
;;         put in the selected note with the dur to next event
;;

;;101213/af adding accent-dr marks for Erica and Richard
;;111116/af adding accent-h,m,c marks for Erica and Richard
(defun sync-make-mel ()
   (sync-mark-oondr)
   (sync-mark-mc)
   (let ((ton)
         (last-from-voice)
         (*sync-track* (make-instance 'mono-track :trackname "sync-track")) )
      (p-each-note
        (let ((minl (p-min-all (p-this :oondr))))
           (cond 
                 
                 ((and (not minl) (p-first?)) ;if first rest
                  ;(print-ll "not minl and first " *cur-notes*)
                  (let ((from-voice (car (p-min (p-this 'ndr))))
                        (ndr (cdr (p-min (p-this 'ndr)))))
                     (add-one-segment *sync-track* (sync-make-rest from-voice ndr))
                     (setq last-from-voice from-voice)
                     ))
                 
                 ((and (not minl) (sync-cur-only-rest?))         ;if rest
                  ;(print-ll "not minl and rests " *cur-notes*)
                  (ifn (sync-all-only-rest?)
                    (then 
                      ;(print "     not only rests")
                      (let* ((old-ton (car (last (segment-list *sync-track*))))
                             (new-dr (+ (get-dr-to-next-event)
                                        (get-var old-ton 'ndr) )))
                         (set-var old-ton 'dr new-dr)
                         (set-var old-ton 'ndr new-dr) ))
                    (let ((from-voice (car (p-min (p-this 'ndr))))
                          (ndr (cdr (p-min (p-this 'ndr)))))
                       (add-one-segment *sync-track* (sync-make-rest from-voice ndr))
                       )))
                 
                 ((not minl)          ;if else 
                  ;(print-ll "not minl and prev bind ? " *cur-notes*)
                  (let ((from-voice (car (p-min (p-this 'ndr))))
                        (ndr (cdr (p-min (p-this 'ndr)))))
                     (add-one-segment *sync-track* (sync-make-rest from-voice ndr))
                     ))
                 
                 (minl  ;not only rests
                   (let ((l))
                      (dolist (v-oondr minl)           ;construct new index list
                         (let ((v (car v-oondr)))
                            (newr l (cons v (v-to-ni v))) ))
                      (let* ((*cur-notes* l) ;set the master from max melodic-charge
                             (from-voice
                               (or (car (p-max (p-this :mc)))
                                   (if (assoc last-from-voice *cur-notes* :test #'eq) ;cont with same
                                      last-from-voice
                                      (caar *cur-notes*) ))))
                         (setq ton (make-instance 'segment))
                         (sync-copy-prop ton from-voice) )
                      (if (p-this 'phrase-start) ;copy from any *cur-notes*
                         (set-var ton 'phrase-start (cdar (p-this 'phrase-start))))
                      (if (p-this 'phrase-end)
                          (set-var ton 'phrase-end (cdar (p-this 'phrase-end))))
                     (if (p-this 'accent-dr)
                         (set-var ton 'accent-dr (cdar (p-this 'accent-dr))))
                     (if (p-this 'accent-h)
                         (set-var ton 'accent-h (cdar (p-this 'accent-h))))
                     (if (p-this 'accent-m)
                         (set-var ton 'accent-m (cdar (p-this 'accent-m))))
                     (if (p-this 'accent-c)
                         (set-var ton 'accent-c (cdar (p-this 'accent-c))))

                      (add-one-segment *sync-track* ton) ))) ;cond
           ))
      (set-var (car (segment-list *sync-track*)) :mr t)
   (rem-all :oondr)
   (rem-all :mc)
      *sync-track*
      )
  )

;;------------simple sync mel ---------------------------------


;; take all notes including rests
;; selects the highest pitch
;; about 50% faster
#|
(defun simple-sync-make-mel ()
   (let ((ton)
         (last-from-voice)
         (*sync-track* (make-instance 'mono-track :trackname "sync-track")) )
      (p-each-note
       (let ((minl (p-min-all (p-this 'ndr)))
             (l))
         
         ;construct new index list of the shortest notes
         (dolist (v-oondr minl)           
           (let ((v (car v-oondr)))
             (newr l (cons v (v-to-ni v))) ))
         
         ;a new master note from top f0
         (let* ((*cur-notes* l) ;set the master from max f0
                (from-voice (or (car (p-max (p-this-f0)))
                                (caar *cur-notes*) )))
           (setq ton (make-instance 'segment))
           (simple-sync-copy-prop ton from-voice) )
         
         ;copy phrasing from any of the current notes
         (if (p-this 'phrase-start)
             (set-var ton 'phrase-start (cdar (p-this 'phrase-start))))
         (if (p-this 'phrase-end)
             (set-var ton 'phrase-end (cdar (p-this 'phrase-end))))
         
         (add-one-segment *sync-track* ton) ))
     (set-var (car (segment-list *sync-track*)) :mr t)
      *sync-track*
     ))
|#
;just taking the top note in *cur-notes*
(defun simple-sync-make-mel ()
   (let ((ton)
         (last-from-voice)
         (*sync-track* (make-instance 'mono-track :trackname "sync-track")) )
      (p-each-note
       (let ((minl (p-min-all (p-this 'ndr)))
             (l))
                  
         ;a new master note from top f0
         (let* ((from-voice (or (car (p-max (p-this-f0)))
                                (caar *cur-notes*) ))) ;if all rests?
           (setq ton (make-instance 'segment))
           (if (not from-voice) (print-ll "*ndr-to-next* " *ndr-to-next* " *all-notes* " *all-notes* " *cur-notes* " *cur-notes*))
           (simple-sync-copy-prop ton from-voice) )
         
         ;copy phrasing from any of the current notes
         (if (p-this 'phrase-start)
             (set-var ton 'phrase-start (cdar (p-this 'phrase-start))))
         (if (p-this 'phrase-end)
             (set-var ton 'phrase-end (cdar (p-this 'phrase-end))))
         
         (add-one-segment *sync-track* ton) ))
     (set-var (car (segment-list *sync-track*)) :mr t)
      *sync-track*
     ))

;;101213/af adding accent-dr marks for Erica and Richard
;;111116/af adding accent-h,m,c marks for Erica and Richard
(defun simple-sync-make-mel ()
  (let ((ton)
        (last-from-voice)
        (*sync-track* (make-instance 'mono-track :trackname "sync-track")) )
    (p-each-note
     (let ((minl (p-min-all (p-this 'ndr)))
           (l))
       
       ;a new master note from top f0
       (let* ((from-voice (or (car (p-max (p-this-f0)))
                              (caar *cur-notes*) ))) ;if all rests?
         (setq ton (make-instance 'segment))
         (if (not from-voice) (print-ll "*ndr-to-next* " *ndr-to-next* " *all-notes* " *all-notes* " *cur-notes* " *cur-notes*))
         (simple-sync-copy-prop ton from-voice) )
       
       ;copy phrasing from any of the current notes
       (if (p-this 'phrase-start)
           (set-var ton 'phrase-start (cdar (p-this 'phrase-start))))
       (if (p-this 'phrase-end)
           (set-var ton 'phrase-end (cdar (p-this 'phrase-end))))
       (if (p-this 'accent-dr)
           (set-var ton 'accent-dr (cdar (p-this 'accent-dr))))
       (if (p-this 'accent-h)
           (set-var ton 'accent-h (cdar (p-this 'accent-h))))
       (if (p-this 'accent-m)
           (set-var ton 'accent-m (cdar (p-this 'accent-m))))
       (if (p-this 'accent-c)
           (set-var ton 'accent-c (cdar (p-this 'accent-c))))
       
       (add-one-segment *sync-track* ton) ))
    (set-var (car (segment-list *sync-track*)) :mr t)
    *sync-track*
    ))

;;120914/af added stuff for statistical-analysis
(defun simple-sync-make-mel ()
  (let ((ton)
        (last-from-voice)
        (*sync-track* (make-instance 'mono-track :trackname "sync-track")) )
    (p-each-note
     (let ((minl (p-min-all (p-this 'ndr)))
           (l))
       
       ;a new master note from top f0
       (let* ((from-voice (or (car (p-max (p-this-f0)))
                              (caar *cur-notes*) ))) ;if all rests?
         (setq ton (make-instance 'segment))
         (if (not from-voice) (print-ll "*ndr-to-next* " *ndr-to-next* " *all-notes* " *all-notes* " *cur-notes* " *cur-notes*))
         (simple-sync-copy-prop ton from-voice) )
       
       ;copy phrasing from any of the current notes
       (if (p-this 'phrase-start)
           (set-var ton 'phrase-start (cdar (p-this 'phrase-start))))
       (if (p-this 'phrase-end)
           (set-var ton 'phrase-end (cdar (p-this 'phrase-end))))
       (if (p-this 'accent-dr)
           (set-var ton 'accent-dr (cdar (p-this 'accent-dr))))
       (if (p-this 'accent-h)
           (set-var ton 'accent-h (cdar (p-this 'accent-h))))
       (if (p-this 'accent-m)
           (set-var ton 'accent-m (cdar (p-this 'accent-m))))
       (if (p-this 'accent-c)
           (set-var ton 'accent-c (cdar (p-this 'accent-c))))
       (if (p-this :real-sl-norm)  ; use max real-sl-norm
           (set-var ton :real-sl-norm (cdr (p-max (p-this :real-sl-norm)))))
       (if (p-this :new-tone)  ; transfer :new-tone from any note
           (set-var ton :new-tone t))
       
       (add-one-segment *sync-track* ton) ))
    (set-var (car (segment-list *sync-track*)) :mr t)
    *sync-track*
    ))

;------------------radio baton sync track ---------------------------------

;; add a sync track for the radio baton
;; on the menu
(defun make-radio-baton ()
  (each-note (set-this :mr t))
  (make-radio-baton-track)
  (let ((*active-score* 
         (make-instance 'score
           :track-list (last (track-list *active-score*)))))
    (init-music-score) )
  (sync-note1)
  (rem-all :mr)
  (make-or-update-edit-music-window)
  )
  
#|
(defun make-radio-baton-track ()
  (if (not (get-first 'mm)) (error "make-radio-baton-track: no mm in first note"))
  (if (not (get-first 'meter)) (error "make-radio-baton-track: no meter in first note"))
  (let ((bars (ceiling (/ (* (/ (get-first 'mm) 60000.0)
                    (total-length-ndr))
                          (car (get-first 'meter)))))
        (radio-track (make-instance 'mono-track :trackname "radio-baton-track"))
        (beats (car (get-first 'meter))) )
    (print-ll "number of bars: " bars)
    (loop for bar from 1 to bars do
          (loop for beat from 1 to (1- beats) do
                (let ((note (make-instance 'segment)))
                  (set-var note 'n '("C4" 1/8))
                  (when (and (= bar 1) (= beat 1))  ;add mm and meter on the first note
                      (set-var note 'mm (get-first 'mm))
                      (set-var note 'meter (get-first 'meter)) )
                  (add-one-segment radio-track note) )
                (let ((note (make-instance 'segment)))
                  (set-var note 'n '(nil 1/8))
                  (set-var note 'rest t)
                  (add-one-segment radio-track note) ))              
          (let ((note (make-instance 'segment)))
            (set-var note 'n '("C4" 1/8))
            (add-one-segment radio-track note) )
          (let ((note (make-instance 'segment)))
            (set-var note 'n '(nil 1/16))
            (set-var note 'rest t)
            (add-one-segment radio-track note) )
          (let ((note (make-instance 'segment)))
            (set-var note 'n '("D4" 1/16))
            (add-one-segment radio-track note) )
          )
    (add-one-track *active-score* radio-track)
    ))
|#
;;new version allowing meter changes in the score
;;needs proper bar markings in the score
;;follows the first track, ie it has to be the full length of the piece
;; adds staccato marks instead of pauses - use the score-staccato rule
;; 6/8 gives two beats per measure
(defun make-radio-baton-track ()
  (if (not (get-first 'mm)) (error "make-radio-baton-track: no mm in first note"))
  (if (not (get-first 'meter)) (error "make-radio-baton-track: no meter in first note"))
  (let ((radio-track (make-instance 'mono-track :trackname "radio-baton-track"))
        (beats (car (get-first 'meter)))
        (beat-division (cadr (get-first 'meter)))
        )
    (each-track-if
     (first?)
    (each-note-if
     (this 'bar)
     (then
      (when (this 'meter) 
        (setq beats (car (this 'meter)))
        (setq beat-division (cadr (this 'meter)))
        (when (and (= beats 6) (= beat-division 8)) ;;6/8 special case
          (setq beats 2)
          (setq beat-division 8/3) ))
      (loop for beat from 1 to (1- beats) do
            (let ((note (make-instance 'segment)))
              (set-var note 'n (list "C4" (/ 1 beat-division)))
              (when (and (this 'meter) (= beat 1))  ;copy mm and meter
                (set-var note 'mm (this 'mm))
                (set-var note 'meter (this 'meter)) )
              (add-one-segment radio-track note) )
            )     
      (let ((note (make-instance 'segment)))
        (set-var note 'n (list "C4" (/ 1 (* beat-division 2))))
        (add-one-segment radio-track note) )    
      (let ((note (make-instance 'segment)))
        (set-var note 'n (list "D4" (/ 1 (* beat-division 2))))
        (add-one-segment radio-track note) )
      )))
    (set-var (first (segment-list radio-track)) 'staccato-start t) ;; staccato
    (set-var (car (last (segment-list radio-track))) 'staccato-end t) ;; staccato
    (add-one-track *active-score* radio-track)
    ))

              

(defun total-length-ndr ()
  (let ((max-ndr 0))
    (each-track
     (let ((track-ndr 0))
       (each-note
        (incf track-ndr (this 'ndr)) )
       ;(print track-ndr)
       (if (> track-ndr max-ndr) (setq max-ndr track-ndr)) ))
    max-ndr))
     

;;--------------- sync utilities ----------------------------------------

;mark melodic charge values in music
;;0009/af fixed string chord syntax
(defun sync-mark-mc ()
 (block markmc
 (let ((qnr))
  (each-note 
     (if (and (first?) (not (this 'q)))(return-from markmc))
     (if (this 'q)
         (setq qnr (tone-to-tonnr (chord-to-root (this 'q)))) )
   (ifn (this 'rest)
    (set-this :mc
        (melodic-charge-fn
             (mod (- (tone-to-tonnr (note-to-tone (this 'n))) qnr) 12)
             )))))))

;mark onset-to-onset nominal duration
(defun sync-mark-oondr ()
  (each-note-if
    (not (this 'rest))
    (then 
      (cond 
       ((not (last?))
        (let ((oondr (this 'ndr)) (i *i*))
          (while (or (this 'tie)(this 'bind))
            (incf *i*)
            (setq oondr (+ oondr (this 'ndr))) )
          (while (and (not (>= (+ *i* 2) *v-length*)) (next 'rest) )
            (incf *i*)
            (setq oondr (+ oondr (this 'ndr))) )
          (iset i :oondr oondr) ))
       (t
        (set-this :oondr (this 'ndr)) )))))

(defun sync-cur-only-rest? ()
  (= (length *cur-notes*) (length (p-this 'rest))) )

(defun sync-all-only-rest? ()
  (= (length *all-notes*) (length (p-this-all 'rest))) )

;(defun foo () (p-each-note (print (p-this :mc))))

(defun sync-make-rest (from-voice dr)
    (let ((ton (make-instance 'segment)))
       (sync-copy-prop ton from-voice)
       (set-var ton 'dr dr)
       (set-var ton 'rest t)
       ton ))

;with phrase markers from any voice -funkar ej *cur-notes* ar tillfalligt omdef
(defun sync-copy-prop (ton from-voice)
  (set-var ton 'f0 (v-this from-voice 'f0))
  (set-var ton 'n (v-this from-voice 'n))
  (if (v-this from-voice 'sl) (set-var ton 'sl (v-this from-voice 'sl)))
  (if (v-this from-voice 'slur) (set-var ton 'slur (v-this from-voice 'slur)))
  (if (v-this from-voice 'tie) (set-var ton 'tie (v-this from-voice 'tie)))
  (if (v-this from-voice 'bar) (set-var ton 'bar (v-this from-voice 'bar)))
  (if (v-this from-voice 'q) (set-var ton 'q (v-this from-voice 'q)))
  (if (v-this from-voice 'key) (set-var ton 'key (v-this from-voice 'key)))
  (if (v-this from-voice 'mm) (set-var ton 'mm (v-this from-voice 'mm)))
  (if (v-this from-voice 'phrase) (set-var ton 'phrase (v-this from-voice 'phrase)))
  (if (v-this from-voice 'phrase-start) (set-var ton 'phrase-start (v-this from-voice 'phrase-start)))
  (if (v-this from-voice 'phrase-end) (set-var ton 'phrase-end (v-this from-voice 'phrase-end)))
  (if (v-this from-voice 'subph) (set-var ton 'subph (v-this from-voice 'subph)))
  (if (v-this from-voice 'rit-start) (set-var ton 'rit-start (v-this from-voice 'rit-start)))
  (if (v-this from-voice 'rit-end) (set-var ton 'rit-end (v-this from-voice 'rit-end)))
  ;(print-ll from-voice " " (v-this from-voice 'n)
  ;          " bar " (v-this from-voice 'bar) " rit " (p-this 'rit-start))
  (let ((drmin (get-dr-to-next-event)))
    (set-var ton 'ndr drmin)
    (set-var ton 'dr drmin) )
  ton
  )

;; consider both rests and notes
(defun simple-sync-copy-prop (ton from-voice)
  (set-var ton 'f0 (v-this from-voice 'f0))
  (set-var ton 'n (v-this from-voice 'n))
  (if (v-this from-voice 'rest) (set-var ton 'rest (v-this from-voice 'rest)))
  (if (v-this from-voice 'sl) (set-var ton 'sl (v-this from-voice 'sl)))
  (if (v-this from-voice 'slur) (set-var ton 'slur (v-this from-voice 'slur)))
  ;(if (v-this from-voice 'tie) (set-var ton 'tie (v-this from-voice 'tie)))
  (if (v-this from-voice 'bar) (set-var ton 'bar (v-this from-voice 'bar)))
  (if (v-this from-voice 'q) (set-var ton 'q (v-this from-voice 'q)))
  (if (v-this from-voice 'key) (set-var ton 'key (v-this from-voice 'key)))
  (if (v-this from-voice 'mm) (set-var ton 'mm (v-this from-voice 'mm)))
  ;(if (v-this from-voice 'phrase) (set-var ton 'phrase (v-this from-voice 'phrase)))
  ;(if (v-this from-voice 'phrase-start) (set-var ton 'phrase-start (v-this from-voice 'phrase-start)))
  ;(if (v-this from-voice 'phrase-end) (set-var ton 'phrase-end (v-this from-voice 'phrase-end)))
  ;(if (v-this from-voice 'subph) (set-var ton 'subph (v-this from-voice 'subph)))
  (if (v-this from-voice 'rit-start) (set-var ton 'rit-start (v-this from-voice 'rit-start)))
  (if (v-this from-voice 'rit-end) (set-var ton 'rit-end (v-this from-voice 'rit-end)))
  (let ((drmin (get-dr-to-next-event)))
    (set-var ton 'ndr drmin)
    (set-var ton 'dr drmin) )
  ton
  )

#|
(defun foo ()
  (p-each-note
    (print-ll *cur-notes* "  " (p-this 'phrase-start))
    ))
|#

;(defun foo ()(p-each-note (print-ll *cur-notes* "  " (cdr (p-min (p-this 'ndr))))))

(defun get-dr-to-next-event ()
    *ndr-to-next*)



;;-------------------sync notes --------------------------


;;syncronize all voices to the current master voice
#|
(defun sync-note1 ()
  (let ((dr-ack (make-array (length (active-track-list *active-score*)) :initial-element 0))
        (mrvi)(mrv) (dr-ack-mr))
    (p-each-note
        ;(print *cur-notes*)
      (if (p-this :mr)
        (then (setq mrv (caar (p-this :mr)))
              (setq mrvi (vname-to-i mrv)) ))
      ;(print-ll " mrv " mrv " mrvi " mrvi)
      (if (> (length *cur-notes*) 1)
        (then
          (setq dr-ack-mr (aref dr-ack mrvi))
          ;; mark dr
          (ifn (assoc mrv *cur-notes* :test #'eq)
               (print-ll "sync-note1 " "no master " *cur-notes*) )
          ; (prin1 "       " mrv)
          (dolist (v-i *cur-notes*)
            (let ((v (car v-i))
                  (i (cdr v-i)))
              (ifn (equal v mrv)
                   (let* ((vi (vname-to-i v))
                          (diff (- dr-ack-mr (aref dr-ack vi))) ) ;diff in duration
                     (if (> i 0)   ;not on first note
                       (v-iset (cons v (1- i))                     ;update dr
                               'dr
                               (+ (cdr (v-iget (cons v (1- i)) 'dr)) diff )))
                     (if (and *sync-dro?* (v-iget (cons mrv i) 'dro))
                       (v-iset (cons v i)                     ;update dro
                               'dro
                               (cdr (v-iget (cons mrv i) 'dro)) ))
                     (setf (aref dr-ack vi) (+ (aref dr-ack vi) diff))   ;update dr-ack
                     ;   (prin1 diff " ")
                     ))))
            ;(print dr-ack)
          ))
      ;; incr dr-ack
      (dolist (v-dr (p-this 'dr))
        (let* ((v (car v-dr))
               (dr (cdr v-dr))
               (vi (vname-to-i v)))
          (setf (aref dr-ack vi) (+ (aref dr-ack vi) dr)) ))
      ;; last note offset
      (when (p-last?)
        ;(print "last note")
        (let ((dr-ack-mr (aref dr-ack mrvi)))
        (dolist (vi *all-notes*)
          (let ((voice-i (vname-to-i (car vi))))
            (v-iset vi 'dr
                    (+ (-  dr-ack-mr (aref dr-ack voice-i)) (cdr (v-iget vi 'dr))) )
            ))))

      ))
   )
|#
(defun sync-note1 ()
  (let ((dr-ack (make-array (length (active-track-list *active-score*)) :initial-element 0))
        (mrvi)(mrv) (dr-ack-mr))
    (p-each-note
        ;(print *cur-notes*)
      (if (p-this :mr)
        (then (setq mrv (caar (p-this :mr)))
              (setq mrvi (vname-to-i mrv)) ))
      ;(print-ll " mrv " mrv " mrvi " mrvi)
      (if (> (length *cur-notes*) 1)
        (then
          (setq dr-ack-mr (aref dr-ack mrvi))
          ;; mark dr
         (cond
          ((not (assoc mrv *cur-notes* :test #'eq))
           (print-ll "sync-note1 " "no master " *cur-notes*) )
          ; (prin1 "       " mrv)
          (t
           (dolist (v-i *cur-notes*)
            (let ((v (car v-i))
                  (i (cdr v-i)))
              (ifn (equal v mrv)
                   (let* ((vi (vname-to-i v))
                          (diff (- dr-ack-mr (aref dr-ack vi))) ) ;diff in duration
                     (if (> i 0)   ;not on first note
                       (v-iset (cons v (1- i))                     ;update dr
                               'dr
                               (+ (cdr (v-iget (cons v (1- i)) 'dr)) diff )))
                     (if (and *sync-dro?* (v-iget (cons mrv i) 'dro))
                       (v-iset (cons v i)                     ;update dro
                               'dro
                               (cdr (v-iget (cons mrv i) 'dro)) ))
                     (setf (aref dr-ack vi) (+ (aref dr-ack vi) diff))   ;update dr-ack
                     ;   (prin1 diff " ")
                     ))))))
            ;(print dr-ack)
          ))
      ;; incr dr-ack
      (dolist (v-dr (p-this 'dr))
        (let* ((v (car v-dr))
               (dr (cdr v-dr))
               (vi (vname-to-i v)))
          (setf (aref dr-ack vi) (+ (aref dr-ack vi) dr)) ))
      ;; last note offset
      (when (p-last?)
        ;(print "last note")
        (let ((dr-ack-mr (aref dr-ack mrvi)))
        (dolist (vi *all-notes*)
          (let ((voice-i (vname-to-i (car vi))))
            (v-iset vi 'dr
                    (+ (-  dr-ack-mr (aref dr-ack voice-i)) (cdr (v-iget vi 'dr))) )
            ))))

      ))
   )

;;; convert from voice name to vector index
;;; redefined as track object to vector index
(defun vname-to-i (vn)
  (block loop
   (loop for i from 0 to (1- (length (active-track-list *active-score*))) do
     (if (equal vn (nth i (active-track-list *active-score*)))
        (return-from loop i) ))))

#|
(defun prin-vec (vec)
  (prin1 "#[")
  (for (i 0 1 (1- (length vec)))
    (prin1-ll (round (aref vec i)) " ") )
  (prin1 "]") )
|#

;_______________________ not used stuff ___________________________

#| 
;mark onset-to-onset real duration
(defun mark-oodr ()
  (each-note-if
    (not (this 'rest))
    (let ((oodr (this 'dr)) (i *i*))
       (while (this 'bind)
              (incf *i*)
              (setq oodr (+ oodr (this 'dr))) )
       (while (next 'rest)
              (incf *i*)
              (setq oodr (+ oodr (this 'dr))) )
       (iset i 'oodr oodr) )))
|# 

#|
(defun ndr-tot ((v . i))
    (let ((ndr (v-iget (cons v i) 'ndr)))
     (ifn (p-last?)
      (while (v-iget (cons v i) 'bind)
             (incf i)
             (setq ndr (+ ndr (v-iget (cons v i) 'ndr))) )
      (while (v-iget (cons v (1+ i)) 'rest)
             (incf i)
             (setq ndr (+ ndr (v-iget (cons v i) 'ndr))) )
      )))

;a variant including the absolute time of the onset of the notes
(defmacro p-each-note-abs body
  `(let ((ndrtot (makevector (length *vlist*) 0)))  ;absolute time for onsets
 						(p-each-note
         (ifn (p-first?)
          (mapc #'(lambda ((v . i))           ;add old dr to ndrtot
                   (let ((vi (vname-to-i v)))
                     (setf (aref ndrtot vi) (+ (aref ndrtot vi) (cdr (v-iget (cons v (1- i)) 'ndr))))))
                 *cur-notes* ))
          ,@body
        )))        
;(defun foo () (p-each-note-abs (print-ll ndrtot *cur-notes*)))
;(defun foo () (p-each-note (print *cur-notes*)))
|#        

#|
(defun select-mr1 ()
  (p-each-note
    (if (p-first?) (v-set-this (caar *cur-notes*) :mr t)) ;initial master
    (ifn (p-last?)
     (let ((minl (p-min-all (p-this :oondr)))
           (l) )
(print-ll minl "     " *cur-notes*)
        (if minl  ;not only rests
         (then 
         (mapc #'(lambda ((v . oondr))           ;construct new index list
                        (newr l (cons v (v-to-ni v))) )
                 minl )
          (let ((*cur-notes* l)) ;set the master from max melodic-charge
               (v-set-next (car (p-max (p-this :mc))) :mr t) )
          ))))))
|#


