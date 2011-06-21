(ns director-musices.gui.rulepalette
  (:use (director-musices rulepalette)
        (director-musices.gui score)
        (Hafni arrow utils)
        (Hafni.swing component dialog layout text view)))

(def *current-rulepalette* (atom nil))

(def *current-syncrule* (atom "no-sync"))

(def *rulepalette-panel* 
  (let [l (label :text "No rulepalette loaded")]
    (.setHorizontalAlignment (component l) javax.swing.JLabel/CENTER)
    (border-layout :center l 
                   :south (flow-layout :content [(label :text "Sync rule")
                                                 (comp-and-events (text-field :text @*current-syncrule* :width 10)
                                                                  :act (arr #(swap! *current-syncrule* (constantly %))))]))))

;; Datastructure

(defn update-rulepalette [ruleset rule_index k v]
  (swap! *current-rulepalette*
         #(assoc-in % [ruleset rule_index 1 k] v)))

(defn set-rulepalette [rulepalette]
  (swap! *current-rulepalette* (constantly rulepalette)))

;; Display

(defn- create-rule-display [ruleset index [rule_name args]]
  (let [l (label :text (str rule_name))
        fields (map (fn [[k v]]
                      [(label :text (if (= k :k) nil (str k)))
                       (comp-and-events (text-field :text (str v) :width 5)
                                        :act (arr #(update-rulepalette ruleset index k (read-string %)))
                                        )]) args)]
    (cons l (apply concat fields))))

(defn create-rulepalette-display []
  (let [content (map-indexed (partial create-rule-display :all-rules)
                             (:all-rules @*current-rulepalette*))
        p (javax.swing.JPanel. (net.miginfocom.swing.MigLayout.))
        s (scroll-pane p)]
    (dorun (map (fn [coll]
                  (dorun (map #(.add p (component %)) (butlast coll)))
                  (.add p (component (last coll)) "wrap")) content))
    s))

;; arrows

(def apply-current-rulepalette
  (arr (fn [_]
         (if @*current-rulepalette*
           (if @*current-score*
             (apply-rulepalette @*current-rulepalette* @*current-syncrule*)
             (error-message :message "No score loaded"))
           (error-message :message "No rulepalette loaded")))))

(def update-rulepalette-panel
  (>>> (arr (ignore create-rulepalette-display))
       (input-arr *rulepalette-panel* :center)))

(def choose-and-open-rulepalette
  (>>> (arr (constantly [nil nil]))
       (||| (arr open-file)
            (>>> (fst (arr (fn [{path :path}]
                             (set-rulepalette (path->rulepalette path)))))
                 update-rulepalette-panel))))

(def choose-and-save-rulepalette
  (arr (fn [_]
         (if @*current-rulepalette*
           (save-rulepalette (:path (save-file))
                             @*current-rulepalette*)))))
