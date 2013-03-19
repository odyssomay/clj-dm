(ns director-musices.rulepalette.ui
  (:use [clojure.java.io :only [resource file]])
  (:require (director-musices.rulepalette
              [global :as global]
              [glue :as glue])
            (director-musices
              [player :as player]
              [util :as util])
            [director-musices.score.ui :as score-ui]
            (seesaw
              [chooser :as ssw-chooser]
              [core :as ssw]
              [mig :as ssw-mig]))
  )

(defprotocol Rulepalette
  (get-rules       [this])
  (update-rule!    [this id f])
  (get-rule        [this id])
  (on-rule-change  [this id f])
  (on-order-change [this f])
  (get-name        [this]))

(def slider-precision 1000)

(defn rules->string [rules]
  (apply str
         (for [{:keys [name parameterless? enabled? v options]} rules]
           (if enabled?
             (str "(" name " "
                  (if (not parameterless?)
                    (str v " " options)
                    )
                  ") ")
             ))))

(defn apply-rulepalette [rulepalette syncrule rule-interaction]
  (glue/apply-rules (rules->string (map deref (:rules rulepalette)))
                    syncrule rule-interaction)
  (score-ui/reload-score))

(defn- parameter-view [rule-atom]
  (let [{:keys [v options]} @rule-atom
        value-text (ssw/text :text v :columns 5)
        slider (ssw/slider :min (* -5 slider-precision) :max (* 5 slider-precision)
                           :value (* v slider-precision) :snap-to-ticks? false)
        options-text (ssw/text :text options :columns 30)
        update-value #(swap! rule-atom assoc :v (read-string (.getText value-text)))
        update-options #(swap! rule-atom assoc :options (.getText options-text))]
    (ssw/listen value-text :action 
                (fn [_] (update-value)
                  (.setValue slider (* (read-string (.getText value-text)) 
                                       slider-precision))))
    (ssw/listen slider :change 
                (fn [_] (update-value)
                  (.setText value-text (str (double (/ (.getValue slider) 
                                                       slider-precision))))))
    (.addDocumentListener (.getDocument options-text)
      (reify javax.swing.event.DocumentListener
        (changedUpdate [_ _])
        (insertUpdate [_ _] (update-options))
        (removeUpdate [_ _] (update-options))))
    [[value-text "gapleft 3"] [slider] [options-text "wrap"]]
    ))

(defn- rule-view [rule-atom]
  (let [{:keys [name parameterless? enabled? v options]} @rule-atom
        enabled? (ssw/checkbox :selected? enabled?)]
    (ssw/listen enabled? :selection
                (fn [_] (swap! rule-atom assoc
                               :enabled? (.isSelected enabled?))))
    ;(add-watch rule-atom nil (fn [_ _ _ v] (println "new value:" (pr-str v))))
    (concat [[enabled?] [name]]
            (if parameterless?
              [["" "wrap"]]
              (parameter-view rule-atom)
    ))))

(defn- options-view [rulepalette]
  (let [rule-interact-num (ssw/spinner :model 2)
        rule-interact? (ssw/checkbox :text "Rule interact:"
                                     :selected? false)
        update-rule-interact
        (fn [& _]
          (ssw/config! rule-interact-num
                       :enabled? (.isSelected rule-interact?)))
        sync-rule (ssw/combobox :model ["melodic-sync"
                                        "no-sync"
                                        "simple-mel-sync"])
        apply-this (fn [& _] (apply-rulepalette
                               rulepalette
                               (ssw/selection sync-rule)
                               (if (.isSelected rule-interact?)
                                 (ssw/selection rule-interact-num))))]
    (ssw/listen rule-interact? :selection update-rule-interact)
    (update-rule-interact)
    (ssw-mig/mig-panel
      :constraints ["gap 1"]
      :items [[(ssw/action :name "Apply"
                           :handler apply-this)
               "span, growx"]
              [(ssw/action :name "Apply & Play"
                           :handler (fn [_]
                                      (apply-this)
                                      (player/start!)))
               "span, growx"]
              [rule-interact? "span"]
              [rule-interact-num "w 50!, align right, span"]
              ["Sync:" "span"]
              [sync-rule "growx, span"]
              ])))

(defn rulepalette-view [rulepalette]
  (ssw/scrollable
    (ssw-mig/mig-panel
      :items [[(options-view rulepalette) "dock west"]
              [(ssw-mig/mig-panel
                 :items (reduce concat (map rule-view (:rules rulepalette)))
                 :constraints ["gap 0"]
                 )
               "span"]])
    :border nil))

;; =====
;; Loading
;; =====

(defn- string->rulepalette-raw [string]
  (->> (load-string (str "(let [set-dm-var (fn [s content]
                                             [(keyword s) content])
                                in-package (fn [_] )]
                            [" string "])"))
       (remove nil?)
       (into {})))

(defn rule-raw->rule [l]
  (let [rule-name (str (first l))]
    (merge {:name rule-name
            :enabled? true
            :id (gensym rule-name)}
           (if (or (= (second l) 'T)
                   (= (second l) 'F))
             {:parameterless? true}
             {:v (second l)
              :options (apply str (interpose " " (drop 2 l)))}
             ))))

(defn string->rulepalette [string & {:as opts}]
  (let [raw (string->rulepalette-raw string)
        rules-no-map (->> (:all-rules raw)
                          (map rule-raw->rule)
                          (map #(vec [(:id %) (atom %)]))
                          )
        order (atom (map first rules-no-map))
        rules (into {} rules-no-map)]
    (reify Rulepalette
      (get-rules [this]
        (reduce (fn [rule-list id]
                  (conj rule-list (get-rule this id)))
                []
                @order))
      (update-rule! [this id f]
        (swap! (get rules id) f))
      (get-rule [this id] @(get rules id))
      (on-rule-change [this id f]
        (add-watch (get rules id) nil
                   (fn [_ _ _ new-rule] (f new-rule))))
      (on-order-change [this f])
      (get-name [this] (str (:name opts)))
      )))

(defn path->rulepalette [path]
  (string->rulepalette
    (slurp path)
    :path path :name (.getName (file path))))

(defn add-rulepalette [rulepalette]
  (.add (global/get-rulepalette-container)
        (get-name rulepalette)
        (rulepalette-view rulepalette))
  (global/load-rulepalette-container))

;; =====
;; Default
;; =====

(def default-rulepalette
  (-> (string->rulepalette
        "(in-package \"DM\")
        (set-dm-var 'all-rules '((HIGH-LOUD 1.0)
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
      (assoc :name "Default")))

;; =====
;; Menu functions
;; =====

(defn open-default-rulepalette [& _]
  (add-rulepalette default-rulepalette))

(defn choose-and-open-rulepalette [& _]
  (ssw-chooser/choose-file :success-fn 
    (fn [_ f] 
      (add-rulepalette
        (path->rulepalette (.getCanonicalPath f))))))

;; =====
;; Init
;; =====
(defn init []
  (global/init)
  (.removeAll (global/get-rulepalette-container))
  (ssw/config! (global/get-rulepalette-panel)
               :items [(util/start-panel
                         "No rulepalette loaded"
                         [(ssw/action :name "Open default rulepalette"
                                      :handler open-default-rulepalette) 
                          (ssw/action :name "Open from disk..."
                                      :handler choose-and-open-rulepalette)])]))

;; =====
;; Testing
;; =====
(def rulepalette-test
  {:path nil
   :name "Default"
   :rules [(atom {:name "high-loud"
                  :v 1.0 :options ""
                  :enabled? true
                  })
           (atom {:name "melodic-charge"
                  :v 1.0 :options ":amp 1 :dur 1 :vibamp 1"
                  :enabled? true
                  })
           (atom {:name "normalize-sl"
                  :parameterless? true
                  :enabled? true
                  })
           ]})

(defn run-rulepalette-test []
  (init)
  (let [f (ssw/frame :title "rulepalette test"
                     :content ;(rulepalette-view rulepalette-test)
                     ;(rulepalette-view default-rulepalette)
                     (global/get-rulepalette-panel)
                     :size [700 :by 400])]
    (ssw/show! f)
    ))
