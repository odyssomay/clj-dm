(ns director-musices.global
  (:require [taoensso.timbre :as log]
            (seesaw
              [core :as ssw]
              [mig :as ssw-mig]))
  (:import javax.swing.UIManager))

(let [linux? (.startsWith (System/getProperty "os.name") "Linux")]
  (defn native! []
    (if linux?
      nil
      (ssw/native!))))

(native!)

(def env (atom {}))
(def pb-env (atom {}))
(def max-value 100)

;; =====
;; Progress bar
;; =====
(defn update-progress-bar [& {:keys [indeterminate? percent-done
                                     large-text small-text]
                              :as options}]
  (let [{:keys [progress-bar large-label small-label]} @pb-env]
    (if percent-done   (ssw/config! progress-bar :value (* max-value percent-done)))
    (if indeterminate? (ssw/config! progress-bar :indeterminate? indeterminate?))
    (if large-text     (ssw/config! large-label :text large-text))
    (if small-text     (ssw/config! small-label :text small-text))
    ))

(defn- init-progress-bar []
  (let [pb (ssw/progress-bar :min 0 :max max-value)
        large-label (ssw/label :text "Large")
        small-label (ssw/label :text "Small")]
    (reset! pb-env
            {:progress-bar pb
             :large-label large-label
             :small-label small-label})
    
    (ssw/config! (:progress-bar-panel @env)
                 :center (seesaw.mig/mig-panel
                           :constraints ["" "[grow][][grow]" 
                                         "[grow][][][][grow]"]
                           :items [[:fill-v "span"]
                                   [:fill-h] [large-label] [:fill-h "wrap"]
                                   [:fill-h] [small-label] [:fill-h "wrap"]
                                   [:fill-h] [pb "gaptop 10, width 300!"] [:fill-h "wrap"]
                                   [:fill-v "gaptop 100, span"]]
                           :size [400 :by 300]))
    (.setFont large-label (.deriveFont (.getFont large-label) (float 16)))
    ))

;; =====
;; Error
;; =====

(let [error-env (atom {})]
  (defn configure-error [& args]
    (let [{:keys [text]} args
          {:keys [error-text button-panel]} @error-env]
      (if text (ssw/config! error-text :text text))
      (ssw/config! button-panel
                   :items [(ssw/action :name "Ignore")
                           (ssw/action :name "Restart")])))
  
  (defn init-error []
    (let [error-title (ssw/label "An error occured :(")
          error-text (ssw/label "An error occured.")
          button-panel (ssw/horizontal-panel)
          content (ssw-mig/mig-panel :items [[error-title "wrap, gaptop 50, gapbottom 5"]
                                             [error-text "wrap, gapbottom 20"]
                                             [button-panel "wrap"]])]
      (.setFont error-title (.deriveFont (.getFont error-title) (float 15)))
      (ssw/config! (:error-panel @env)
                   :center (ssw/horizontal-panel
                             :items [:fill-h content :fill-h]))
      (reset! error-env
              {:error-text error-text
               :button-panel button-panel}))))

;; =====
;; Init
;; =====
(defn init []
  (let [main-panel (ssw/border-panel)
        progress-bar-panel (ssw/border-panel)
        error-panel (ssw/border-panel)
        cp (ssw/card-panel :items [[main-panel :main]
                                   [progress-bar-panel :progress-bar]
                                   [error-panel :error]])
        f (ssw/frame :content cp)]
    (ssw/show-card! cp :main)
    
    (reset! env
            {:main-panel main-panel
             :progress-bar-panel progress-bar-panel
             :error-panel error-panel
             :card-panel cp
             :frame f})
    (init-progress-bar)
    (init-error)
    ))

(defn get-frame      [] (:frame @env))
(defn get-main-panel [] (:main-panel @env))

(defn show-progress-bar [] (ssw/show-card! (:card-panel @env) :progress-bar))
(defn hide-progress-bar [] (ssw/show-card! (:card-panel @env) :main))

(defn show-error [] (ssw/show-card! (:card-panel @env) :error))

(let [arg-map (atom nil)]
  (defn set-arg-map [as] (reset! arg-map as))
  
  (defn get-arg [a]
    (if (not (contains? @arg-map a))
      (log/warn "tried to access arg:" a "but it is not defined!"))
    (get @arg-map a nil)))
