(in-package :dm)

  ;; fixed intonation
  ;; 20010316/af
  ;; 20020222/jn

  (defun fixed-intonation-quarter-comma ()
    (let ((keynr 0))
      (each-note-if
       (not (this 'rest))
       (then
        (set-this 'dc
                  (quarter-comma-intervals
                   (mod (- (tone-to-tonnr (note-to-tone (this 'n))) keynr)
  12)
                   ))))))

  (defun quarter-comma-intervals (semit)
    (case semit
      (0 0)(1 -24)(2 -7)(3 10)(4 -14)(5 3)
      (6 -21)(7 -3)(8 -27)(9 -10)(10 7)(11 -17)
    ))

  (defun fixed-intonation-silbermann-sorge ()
    (let ((keynr 0))
      (each-note-if
       (not (this 'rest))
       (then
        (set-this 'dc
                  (silbermann-sorge-intervals
                   (mod (- (tone-to-tonnr (note-to-tone (this 'n))) keynr)
  12)
                   ))))))

    (defun silbermann-sorge-intervals (semit)
    (case semit
      (0 0)(1 -14)(2 -4)(3 6)(4 -8)(5 2)
      (6 -12)(7 -2)(8 -16)(9 -6)(10 4)(11 -10)
    ))

  (defun fixed-intonation-werckmeister-3 ()
    (let ((keynr 0))
      (each-note-if
       (not (this 'rest))
       (then
        (set-this 'dc
                  (werckmeister-3-intervals
                   (mod (- (tone-to-tonnr (note-to-tone (this 'n))) keynr)
  12)
                   ))))))

    (defun werckmeister-3-intervals (semit)
    (case semit
      (0 0)(1 -10)(2 -8)(3 -6)(4 -10)(5 -2)
      (6 -12)(7 -4)(8 -8)(9 -12)(10 -4)(11 -8)
    ))

  (defun fixed-intonation-neidhardt-dorf-1732 ()
    (let ((keynr 0))
      (each-note-if
       (not (this 'rest))
       (then
        (set-this 'dc
                  (neidhardt-dorf-1732-intervals
                   (mod (- (tone-to-tonnr (note-to-tone (this 'n))) keynr)
  12)
                   ))))))

    (defun neidhardt-dorf-1732-intervals (semit)
    (case semit
      (0 0)(1 -6)(2 -2)(3 -4)(4 -10)(5 -2)
      (6 -8)(7 0)(8 -6)(9 -6)(10 -2)(11 -8)
    ))

  (defun fixed-intonation-neidhardt-kl-stadt-1732 ()
    (let ((keynr 0))
      (each-note-if
       (not (this 'rest))
       (then
        (set-this 'dc
                  (neidhardt-kl-stadt-1732-intervals
                   (mod (- (tone-to-tonnr (note-to-tone (this 'n))) keynr)
  12)
                   ))))))

    (defun neidhardt-kl-stadt-1732-intervals (semit)
    (case semit
      (0 0)(1 -6)(2 -4)(3 -4)(4 -8)(5 -2)
      (6 -8)(7 -2)(8 -4)(9 -6)(10 -4)(11 -8)
    ))

  (defun fixed-intonation-neidhardt-gr-stadt-1732 ()
    (let ((keynr 0))
      (each-note-if
       (not (this 'rest))
       (then
        (set-this 'dc
                  (neidhardt-gr-stadt-1732-intervals
                   (mod (- (tone-to-tonnr (note-to-tone (this 'n))) keynr)
  12)
                   ))))))

    (defun neidhardt-gr-stadt-1732-intervals (semit)
    (case semit
      (0 0)(1 -4)(2 -4)(3 -2)(4 -6)(5 0)
      (6 -4)(7 -2)(8 -4)(9 -6)(10 0)(11 -4)
    ))

  (defun fixed-intonation-vogel-scheidemann ()
    (let ((keynr 0))
      (each-note-if
       (not (this 'rest))
       (then
        (set-this 'dc
                  (vogel-scheidemann-intervals
                   (mod (- (tone-to-tonnr (note-to-tone (this 'n))) keynr)
  12)
                   ))))))

    (defun vogel-scheidemann-intervals (semit)
    (case semit
      (0 0)(1 -13)(2 -7)(3 -6)(4 -8)(5 3)
      (6 -15)(7 -3)(8 -17)(9 -5)(10 1)(11 -12)
    ))