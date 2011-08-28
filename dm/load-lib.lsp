;; File used for testing the lisp environment

(load "package-dm.lsp")
(load "lib-core/infixmath.lsp")

(load "lib-core/basicmacros.lsp")

(load "lib-core/scoreobjects.lsp")
(load "lib-core/syntobjects.lsp")
(load "lib-core/musicio.lsp")
(load "lib-core/dm-objects.lsp")
(load "lib-core/rulemacros.lsp")
(load "lib-core/rule-groups.lsp")
(load "lib-core/shapeobjects.lsp")
(load "lib-core/initconvert.lsp")

(load "init.lsp")

;; rules

(load "rules/frules1.lsp")
(load "rules/frules2.lsp")
(load "rules/Intonation.lsp")
(load "rules/FinalRitard.lsp")
(load "rules/utilityrules.lsp")
(load "rules/Punctuation.lsp")
(load "rules/phrasearch.lsp")


(in-package "DM")

;; TESTING

(read-active-score-from-file "../test2.mus")

(defvar test-rulelist
  '(
    (HIGH-LOUD 1.0)
    (MELODIC-CHARGE 1.0 :AMP 1 :DUR 1 :VIBAMP 1)
    (HARMONIC-CHARGE 1.0 :AMP 1 :DUR 1 :VIBFREQ 1)
    (DURATION-CONTRAST 1.0 :AMP 1 :DUR 1)
;    (DURATION-CONTRAST-ART-DR 1.0)
    (DOUBLE-DURATION 1.0)
    (PUNCTUATION 1.1 :DUR 1 :DUROFF 1 :MARKPHLEVEL7 NIL)
    (PHRASE-ARCH 1.5 :PHLEVEL 5 :TURN 0.3 :NEXT 1.3 :AMP 2)
    (PHRASE-ARCH 1.5 :PHLEVEL 6 :TURN 2 :AMP 2 :LAST 0.2)
    (NORMALIZE-SL)
    (NORMALIZE-DR)
    (FINAL-RITARD 1.0)
   ))

(init-music-score)

(rule-apply-list-sync test-rulelist 'no-sync)

;; Changed files:

;; infixmath.lsp - changed package to :dm
;; dm-objects.lsp - removed pathnames from class environment-settings
