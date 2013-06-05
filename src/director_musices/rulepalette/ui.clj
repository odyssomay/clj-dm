(ns director-musices.rulepalette.ui
  (:use [clojure.java.io :only [resource file]])
  (:require (director-musices.rulepalette
              [global :as global]
              [glue :as glue])
            (director-musices
              [global :as dm-global]
              [player :as player]
              [util :as util])
            (director-musices.score
              [global :as score-global]
              [ui :as score-ui])
            (seesaw
              [chooser :as ssw-chooser]
              [core :as ssw]
              [mig :as ssw-mig])
            [taoensso.timbre :as log]
            [clojure.string :as cstr]))

(defprotocol Rulepalette
  (get-rules       [this])
  (update-rule!    [this id f])
  (add-rule!       [this rule])
  (remove-rule!    [this id])
  (move-rule-up!   [this id])
  (move-rule-down! [this id])
  (get-rule        [this id])
  (on-rule-change  [this id f])
  (on-order-change [this f])
  (get-name        [this])
  
  (set-editable    [this editable?])
  (on-set-editable [this f]))

(def slider-precision 1000)

(defn rules->string [rules & [add-newline?]]
  (apply str
         (for [{:keys [name parameterless? enabled? v options]} rules]
           (if enabled?
             (str "(" name " "
                  (if (not parameterless?)
                    (str v " " options))
                  ")"
                  (if add-newline? "\n" " "))))))

(defn choose-and-save-rulepalette [rulepalette]
  (if-let [f (util/choose-file
               :title "Save Rulepalette"
               :type :save
               :file-ending "pal"
               :filters [["Rulepalette files (.pal)" ["pal"]]])]
    (let [out (str "(in-package \"DM\")\n\n"
                   "(set-dm-var 'all-rules '(\n"
                   (rules->string (get-rules rulepalette) true)
                   "))\n"
                   "(set-dm-var 'sync-rule-list '((NO-SYNC NIL) (MELODIC-SYNC T)))")]
      (spit f out))))

(defn apply-rulepalette [rulepalette syncrule rule-interaction & [play?]]
  (dm-global/show-info-panel :loading "Applying rulepalette")
  (util/thread
    (glue/apply-rules (rules->string (get-rules rulepalette))
                      syncrule rule-interaction)
    (score-ui/reload-score-and-player)
    (dm-global/hide-info-panel)
    (if play? (player/start!))))

(defn- parameter-view [rp rule]
  (let [{:keys [v options id]} rule
        updating-value? (atom false)
        value-text (ssw/text :text v :columns 5)
        text-background (.getBackground value-text)
        slider (ssw/slider :min (* -5 slider-precision) :max (* 5 slider-precision)
                           :value (* v slider-precision) :snap-to-ticks? false)
        options-text (ssw/text :text options :columns 30)
        update-value (fn []
                       (update-rule!
                         rp id
                         #(assoc % :v (read-string (.getText value-text)))))
        update-options (fn [] (update-rule!
                                rp id
                                #(assoc % :options (.getText options-text))))]
    (ssw/listen value-text :document
                (fn [_]
                  (when-not
                    (try (let [new-v (read-string (.getText value-text))]
                           (when (number? new-v)
                             (reset! updating-value? true)
                             (update-value)
                             (ssw/invoke-now
                               (.setValue slider (* new-v slider-precision)))
                             (reset! updating-value? false)
                             (ssw/config! value-text :background
                                          text-background)))
                      (catch Exception e nil))
                    (ssw/config! value-text :background :red))))
    (ssw/listen slider :change
                (fn [_]
                  (when-not @updating-value?
                    (.setText value-text (str (double (/ (.getValue slider)
                                                         slider-precision))))
                    (update-value))))
    (.addDocumentListener (.getDocument options-text)
      (reify javax.swing.event.DocumentListener
        (changedUpdate [_ _])
        (insertUpdate [_ _] (update-options))
        (removeUpdate [_ _] (update-options))))
    [[value-text "gapleft 3"] [slider] [options-text]]))

(defn- rule-view [rp rule]
  (let [{:keys [name parameterless? enabled? v options id]} rule
        name (cstr/capitalize (cstr/replace name "-" " "))
        enabled? (ssw/checkbox :selected? enabled?)
        move-up (util/button-label (fn [_] (move-rule-up! rp id))
                                   :icon "icons/up.png")
        move-down (util/button-label (fn [_] (move-rule-down! rp id))
                                     :icon "icons/down.png")
        delete (util/button-label (fn [_] (remove-rule! rp id))
                                  :icon "icons/delete.png")]
    (ssw/listen enabled? :selection
                (fn [_] (update-rule!
                          rp id #(assoc % :enabled?
                                   (.isSelected enabled?)))))
    (on-set-editable rp #(doseq [c [move-up move-down delete]]
                           (.setVisible c %)))
    (concat [[move-up "gapleft 10"]
             [move-down]
             [enabled?] [name]]
            (if parameterless?
              [[""] [""] [""]]
              (parameter-view rp rule))
            [[delete "wrap, gapleft 2"]])))

(defn rules-view [rulepalette]
  (let [p (ssw-mig/mig-panel :constraints ["gap 0"])
        update-view (fn []
                      (ssw/config!
                        p
                        :items (reduce concat (map #(rule-view rulepalette %)
                                                   (get-rules rulepalette)))))]
    (update-view)
    (on-order-change rulepalette update-view)
    p))

(defn rule-name-dialog [& [previous-name previous-no-parameters?]]
  (let [rules ["Melodic charge"
               "Duration contrast"]
        rule-name-box (ssw/combobox :model rules :editable? true)
        cb (ssw/checkbox :text "Rule does not have any parameters"
                         :selected? previous-no-parameters?)]
    (ssw/selection! rule-name-box (or previous-name (first rules)))
    (when (-> (ssw/dialog
                :content (ssw/border-panel
                           :center rule-name-box
                           :south cb)
                :resizable? false)
              ssw/pack!
              ssw/show!)
      (let [rule-name (cstr/replace (cstr/trim (ssw/text rule-name-box))
                                    #"\s+" "-")]
        (assoc (if (.isSelected cb)
                 {:parameterless? true}
                 {:options ""
                  :v 1.0
                  :parameterless? false})
          :name rule-name
          :id (gensym rule-name)
          :enabled? true)))))

(defn options-view [rulepalette]
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
        apply-this (fn [play?] (apply-rulepalette
                                 rulepalette
                                 (ssw/selection sync-rule)
                                 (if (.isSelected rule-interact?)
                                   (ssw/selection rule-interact-num))
                                 play?))
        editable? (ssw/checkbox :text "editable"
                                :selected? false)
        add-rule (ssw/action
                   :name "Add rule"
                   :handler (fn [_]
                              (if-let [rule (rule-name-dialog)]
                                (add-rule! rulepalette rule))))]
    (ssw/listen rule-interact? :selection update-rule-interact)
    (ssw/listen editable? :selection
                (fn [& _]
                  (set-editable rulepalette (.isSelected editable?))))
    (update-rule-interact)
    (ssw-mig/mig-panel
      :constraints ["gap 3"]
      :items [["Apply" "span"]
              [(score-global/a-if-score
                 :name "Apply"
                 :handler (fn [_] (apply-this false)))
               "span, growx, gapleft 7"]
              [(score-global/a-if-score
                 :name "Apply & Play"
                 :handler (fn [_] (apply-this true)))
               "span, growx, gapleft 7"]
              ["Apply options" "gaptop 7, span"]
              [rule-interact? "span, gapleft 7"]
              [rule-interact-num "w 50!, align right, span"]
              ["Sync:" "span, gapleft 7"]
              [sync-rule "growx, span, gapleft 7"]
              ["Rulepalette" "gaptop 7, span"]
              [editable? "span, gapleft 7"]
              [add-rule "growx, span, gapleft 7"]
              [(ssw/action
                 :name "Save As..."
                 :handler (fn [_]
                            (choose-and-save-rulepalette rulepalette)))
               "growx, span, gapleft 7, gaptop 5"]])))

(defn rulepalette-view [rulepalette]
  (ssw/scrollable
    (ssw-mig/mig-panel
      :items [[(options-view rulepalette) "dock west"]
              [(rules-view rulepalette) "span"]])
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
           (if (contains? #{'T 'F 'NIL nil} (second l))
             {:parameterless? true}
             {:v (second l)
              :options (.toLowerCase (apply str (interpose " " (drop 2 l))))}))))

(defn string->rulepalette [string & {:as opts}]
  (let [raw (string->rulepalette-raw string)
        rules-no-map (->> (:all-rules raw)
                          (map rule-raw->rule)
                          (map #(vec [(:id %) (atom %)])))
        order (atom (map first rules-no-map))
        rules (atom (into {} rules-no-map))
        editable? (atom false)
        remove-rule
        (fn [id]
          (swap! rules dissoc id)
          (swap! order
                 (fn [order]
                   (remove #(= id %) order))))
        move-rule
        (fn [id direction]
          (swap! order
                 (fn [order]
                   (->> order
                        (reduce (fn [order i]
                                  (or (if (> (count order) 0)
                                        (case direction
                                          :up (if (= i id)
                                                (conj (pop order)
                                                      i (peek order)))
                                          :down (if (= (peek order) id)
                                                  (conj (pop order)
                                                        i id ::remove))))
                                      (conj order i)))
                                [])
                        (remove (partial = ::remove))))))]
    (reify Rulepalette
      (get-rules [this]
        (reduce (fn [rule-list id]
                  (conj rule-list (get-rule this id)))
                []
                @order))
      (update-rule! [this id f]
        (swap! (get @rules id) f))
      (add-rule! [this rule]
        (let [id (:id rule)]
          (swap! rules assoc (:id rule) (atom rule))
          (swap! order conj (:id rule))))
      (remove-rule! [this id] (remove-rule id))
      (move-rule-up! [this id]
        (move-rule id :up))
      (move-rule-down! [this id]
        (move-rule id :down))
      (get-rule [this id] @(get @rules id))
      (on-rule-change [this id f]
        (add-watch (get @rules id) nil
                   (fn [_ _ _ new-rule] (f new-rule))))
      (on-order-change [this f]
        (add-watch order nil (fn [& _] (f))))
      (get-name [this] (str (:name opts)))
      
      (set-editable [this new-editable?]
        (reset! editable? new-editable?))
      (on-set-editable [this f]
        (add-watch editable? (gensym)
                   (fn [_ _ _ e] (f e)))
        (f @editable?)))))

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

(defn default-rulepalette []
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
        (set-dm-var 'sync-rule-list '((NO-SYNC NIL) (MELODIC-SYNC T)))"
        :name "Default")))

;; =====
;; Menu functions
;; =====
(defn load-rulepalette-from-file [f]
  (add-rulepalette
    (path->rulepalette (.getCanonicalPath f))))

(defn open-default-rulepalette [& _]
  (add-rulepalette (default-rulepalette)))

(defn choose-and-open-rulepalette [& _]
  (if-let [f (util/choose-file
               :title "Open Rulepalette"
               :type :open
               :filters [["Rulepalette files (.pal)" ["pal"]]])]
    (load-rulepalette-from-file f)))

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
                          (ssw/action :name "Open from file..."
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
                  })]})

(defn run-rulepalette-test []
  (init)
  (let [f (ssw/frame :title "rulepalette test"
                     :content ;(rulepalette-view rulepalette-test)
                     ;(rulepalette-view default-rulepalette)
                     (global/get-rulepalette-panel)
                     :size [700 :by 400])]
    (ssw/show! f)))
