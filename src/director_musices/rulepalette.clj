(ns director-musices.rulepalette
  (:use [director-musices
         [glue :only [apply-rules]]
         [utils :only [find-i]]]
        [clojure.java.io :only [resource]])
  (:require [seesaw 
             [core :as ssw]
             [chooser :as ssw-chooser]
             [mig :as ssw-mig]]))

(def *syncrule-field*
  (ssw/text :columns 10 :text "melodic-sync"))

(def *rule-panel* 
  (ssw-mig/mig-panel))

(def *rules*
  (atom []))
(add-watch *rules* nil (fn [_ _ _ items]
                         (ssw/config! *rule-panel* :items items)))

(def *rulepalette-panel* 
  (ssw/border-panel :center (ssw/scrollable *rule-panel*)
                    :south (ssw/flow-panel :items ["Sync rule" *syncrule-field*])))

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

(def components-per-line 8)
(def slider-precision 1000)

(defn panel->rules [panel]
  (apply str
         (map (fn [[ch l k args]]
                (if (.isSelected ch)
                  (str \( (.getText l) " " 
                       (condp = (class k)
                         javax.swing.JSlider (double (/ (.getValue k) slider-precision))
                         (.getText k)) " " 
                       (.getText args) ")\n")))
              (partition 4 (.getComponents panel)))))

(defn get-line-index-starting-with [up]
  (/ 
    (find-i up (.getComponents *rule-panel*))
    components-per-line))

(defn get-rule-at [line]
  (take components-per-line (drop (* line components-per-line) @*rules*)))

(defn add-rule-at [line rule]
  (swap! *rules* (fn [coll]
                   (let [i (* components-per-line line)]
                     (concat (take i coll) rule (drop i coll))))))

(defn remove-rule-at [line]
  (swap! *rules* (fn [coll]
                   (let [rules (partition 7 coll)]
                     (concat (take (* components-per-line line) coll)
                             (drop (* components-per-line (inc line)) coll))))))

(defn move-rule [line offset]
  (let [rule (get-rule-at line)]
    (remove-rule-at line)
    (add-rule-at (+ line offset) rule)))

(defn rule-name-dialog [& [previous-name previous-no-parameters?]]
  (let [tf (ssw/text :text previous-name :columns 20)
        cb (ssw/checkbox :text "Rule does not have any parameters"
                         :selected? previous-no-parameters?)]
    (if (ssw/show! (ssw/pack! (ssw/dialog :content (ssw/border-panel :center tf :south cb))))
      {:name (.getText tf)
       :no-parameters? (.isSelected cb)})))

(declare rule-display)
(defn rule-display* [[rule-name & [k & args]]]
  (let [up-action (ssw/action :icon (resource "icons/up_alt.png"))
        up (ssw/button :action up-action)
        no-parameters? (not (number? k))]
    (ssw/config! up-action :handler 
      (fn [_]
        (move-rule (get-line-index-starting-with up) -1)))
    (concat 
      [up
       (ssw/action :icon (resource "icons/down_alt.png")
                   :handler (fn [_] (move-rule (get-line-index-starting-with up) 1)))
       (ssw/checkbox :selected? true)
       (let [l (ssw/label :text (str rule-name))]
         (ssw/listen l :mouse-clicked 
                     (fn [e]
                       (if (== (.getClickCount e) 2)
                         (if-let [{nm :name no-p? :no-parameters?} (rule-name-dialog (.getText l) no-parameters?)]
                           (if no-p? 
                             (let [line (get-line-index-starting-with up)]
                               (remove-rule-at line)
                               (add-rule-at line (rule-display [nm 'T])))
                             (if no-parameters?
                               (let [line (get-line-index-starting-with up)]
                                 (remove-rule-at line)
                                 (add-rule-at line (rule-display [nm 0.0])))
                               (.setText l nm)))))))
         l)]
       (if no-parameters?
         ["" "" ""]
         (let [t (ssw/text :text k :columns 5)
               s (ssw/slider :min (* -5 slider-precision) :max (* 5 slider-precision) :value (* k slider-precision)
                      :snap-to-ticks? false 
                      ;:minor-tick-spacing 10 :major-tick-spacing 50 :paint-labels? true
                      )]
           (.addActionListener t
             (reify java.awt.event.ActionListener
               (actionPerformed [_ _] (.setValue s (* (read-string (.getText t)) slider-precision)))))
           (.addChangeListener s
             (reify javax.swing.event.ChangeListener
               (stateChanged [_ _] (.setText t (str (double (/ (.getValue s) slider-precision)))))))
           [t s
            (ssw/text :text (apply str (interpose " " args)) :columns 30)]))
      [(ssw/action :icon (resource "icons/delete.png")
                   :handler (fn [_] (remove-rule-at (get-line-index-starting-with up))))])))

(defn rule-display [args]
  (let [rules (remove nil? (rule-display* args))]
    (concat (map vector (butlast rules)) [[(last rules) "wrap"]])))

(defn rulepalette-display [rulepalette]
  (apply concat (map rule-display (:all-rules rulepalette))))
; (reduce (fn [prev next]
;           (let [rules (rule-display next)]
;             (concat prev (map vector (butlast rules)) [[(last rules) "wrap"]])))
;         []
;         (:all-rules rulepalette)))

;; actions

(defn apply-current-rulepalette [& _] 
  (apply-rules (panel->rules *rule-panel*) (.getText *syncrule-field*)))

(defn choose-and-open-rulepalette [& _]
  (ssw-chooser/choose-file :success-fn (fn [_ f] 
                                         (reset! *rules* (rulepalette-display (path->rulepalette (.getCanonicalPath f)))))))

(defn choose-and-save-rulepalette [& _]
  )

