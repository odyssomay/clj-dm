(ns director-musices.rulepalette.menu
  (:require (director-musices
              [player :as player]
              [util :as util])
            (director-musices.rulepalette
              [global :as global]
              [glue :as glue]
              [ui :as ui])
            [director-musices.score.menu :as score-menu]
            (seesaw
              [chooser :as ssw-chooser]
              [core :as ssw])))

;; Default

(def default-rulepalette
"(in-package \"DM\")
(set-dm-var 'all-rules '(
(HIGH-LOUD 1.0)
(MELODIC-CHARGE 1.0 :AMP 1 :DUR 1 :VIBAMP 1)
(HARMONIC-CHARGE 1.0 :AMP 1 :DUR 1 :VIBFREQ 1)
(DURATION-CONTRAST 1.0 :AMP 1 :DUR 1)
(DOUBLE-DURATION 1.0)
(PUNCTUATION 1.1 :DUR 1 :DUROFF 1 :MARKPHLEVEL7 NIL)
(PHRASE-ARCH 1.5 :PHLEVEL 5 :TURN 0.3 :NEXT 1.3 :AMP 2)
(PHRASE-ARCH 1.5 :PHLEVEL 6 :TURN 2 :AMP 2 :LAST 0.2)
(NORMALIZE-SL T)
(NORMALIZE-DR T)
(FINAL-RITARD 1.0)
))
(set-dm-var 'sync-rule-list '((NO-SYNC NIL) (MELODIC-SYNC T)))")

;; loading

(defn string->rulepalette [string]
  (->> (load-string (str "(let [set-dm-var (fn [s content]
                         [(keyword s) content])
                         in-package (fn [_] )]
                         [" string "])"))
    (remove nil?)
    (into {})))

(defn path->rulepalette [path]
  (string->rulepalette (slurp path)))

;; Display


(def reset-on-apply (atom false))
(defn set-reset-on-apply [new-value]
  (swap! reset-on-apply (constantly new-value)))

(defn save-rp-obj [{:keys [rule-panel]}]
  (if-let [f (util/new-file-dialog rule-panel)]
    (spit f
          (str "(in-package \"DM\")\n(set-dm-var 'all-rules '(\n"
               (ui/panel->rules rule-panel)
               "))\n(set-dm-var 'sync-rule-list '((NO-SYNC NIL) (MELODIC-SYNC T)))"))))

(defn apply-rp-obj [{:keys [rule-interaction? rule-interaction-c syncrule rule-panel]}]
  (util/with-indeterminate-progress "applying rules"
    (glue/apply-rules (ui/panel->rules rule-panel) @syncrule (if @rule-interaction? @rule-interaction-c))
    (score-menu/reload-score-panel)))

(defn apply-current-rulepalette [& _]
  (let [rp-obj (nth @global/rulepalettes (.getSelectedIndex global/rulepalette-container) {})]
    (apply-rp-obj rp-obj))
  (player/update-player))

(defn apply-all-rulepalettes [& _]
  (doseq [rp-obj @global/rulepalettes]
    (apply-rp-obj rp-obj))
  (player/update-player))

(defn choose-and-open-rulepalette [& _]
  (ssw-chooser/choose-file :success-fn 
    (fn [_ f] 
      (ui/add-rulepalette 
        (assoc (ui/rulepalette-view (path->rulepalette (.getCanonicalPath f)))
               :title (.getName f))))))

(defn open-default-rulepalette [& _]
  (ui/add-rulepalette (assoc (ui/rulepalette-view (string->rulepalette default-rulepalette)) :title "Default")))

(def rules-menu
  (ssw/menu :text "Rules"
            :items
            [(ssw/action :name "Open Rulepalette"
                         :handler choose-and-open-rulepalette)
             (ssw/action :name "Open Default Rulepalette" :handler open-default-rulepalette)
             (ssw/action :name "Save Rulepalette")
             (ssw/separator)
             (ssw/action :name "Apply current Rulepalette"
                         :handler apply-current-rulepalette)
             (ssw/action :name "Apply all rulepalettes"
                         :handler apply-all-rulepalettes)
             (let [cb (ssw/checkbox :text "Reset on Apply")]
               (ssw/listen cb :selection (fn [& _] (set-reset-on-apply (.isSelected cb))))
               cb)
             ]))
