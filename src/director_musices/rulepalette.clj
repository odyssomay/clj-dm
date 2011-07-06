(ns director-musices.rulepalette
  (:use (director-musices glue)))

(defn string->rulepalette [string]
  (->> (load-string (str "(let [set-dm-var (fn [s content]
                                             [(keyword s) content])
                                in-package (fn [_] )]
                               [" string "])"))
       (remove nil?) 
       (map (fn [[k coll]]
            [k (vec 
                 (map (fn [[name & args]]
                        (if (> (count args) 0)
                          [name (apply array-map (cons :k args))]
                          [name (array-map)]))
                 coll))]))
       (into {})))

(defn rule->string [[rule_name args]]
  (let [str_args (->> (map (fn [[k v]]
                             (if (= k :k)
                               (str " " v)
                               (str " " k " " v))) args)
                      (apply str))]
    (str "\n(" rule_name str_args ")")))

(defn rulepalette->string [palette]
  (->> palette
       (map (fn [[sym rules]]
              (str "(set-dm-var '" (name sym) " '("
                   (apply str 
                          (map rule->string rules))
                   "))\n\n")))
       (apply str)))

(defn path->rulepalette [path]
  (string->rulepalette (slurp path)))

(defn save-rulepalette [path rulepalette]
  (spit path (rulepalette->string rulepalette)))

(defn apply-rulepalette [rulepalette sync-rule]
  (apply-rules (apply str (map rule->string (:all-rules rulepalette)))
               sync-rule))

;; Tests

;(def *test-rulepalette*
;  (path->rulepalette "src/rulepalettes/default2.pal"))

;(defn test-rulepalette-string []
;  (= *test-rulepalette* (string->rulepalette (rulepalette->string *test-rulepalette*))))
