(ns director-musices.global
  (:require [seesaw.core :as ssw]))

(ssw/native!)

(let [main-panel (ssw/border-panel)
      progress-bar-panel (ssw/border-panel)
      cp (ssw/card-panel :items [[main-panel :main]
                                 [progress-bar-panel :progress-bar]])
      f (ssw/frame :content cp)]
  
  (ssw/show-card! cp :main)
  
  (defn get-frame [] f)
  
  ;(defn get-center-panel [] cp)
  
  (defn get-main-panel [] main-panel)
  (let [max-value 100
        pb (ssw/progress-bar)
        large-label (ssw/label :text "Large")
        small-label (ssw/label :text "Small")]
    (ssw/config! progress-bar-panel
                 :center (ssw/vertical-panel 
                           :items [large-label small-label pb]))
    
    (defn update-progress-bar [& {:keys [indeterminate? percent-done
                                         large-text small-text]
                                  :as options}]
      (if percent-done (ssw/config! pb :value (* max-value percent-done)))
      (if indeterminate? (ssw/config! pb :indeterminate? indeterminate?))
      (if large-text (ssw/config! large-label :text large-text))
      (if small-text (ssw/config! small-label :text small-text))
      )
    
    (defn show-progress-bar [] (ssw/show-card! cp :progress-bar))
    (defn hide-progress-bar [] (ssw/show-card! cp :main)))
  )
