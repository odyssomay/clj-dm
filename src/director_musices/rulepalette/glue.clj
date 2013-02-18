(ns director-musices.rulepalette.glue
  (:require [director-musices.common-lisp.glue :as glue]))

(defn apply-rules [rulelist-string sync-rule & [rule-interaction-c]]
  (prn rulelist-string sync-rule rule-interaction-c)
  (if rule-interaction-c
    (glue/eval-dm (str "(reset-music)
                        (rule-interaction-apply-rules-sync '(" 
                        rulelist-string ") '" rule-interaction-c ")"))
    (glue/eval-dm (str "(reset-music) (rule-apply-list-sync '("
                       rulelist-string ") '" sync-rule ")"))))