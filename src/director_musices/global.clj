(ns director-musices.global
  (:require [taoensso.timbre :as log]
            [seesaw.core :as ssw]
            seesaw.mig))

(ssw/native!)

(let [main-panel (ssw/border-panel)
      progress-bar-panel (ssw/border-panel)
      cp (ssw/card-panel :items [[main-panel :main]
                                 [progress-bar-panel :progress-bar]])
      f (ssw/frame :content cp)]
  
  (ssw/show-card! cp :main)
  
  (defn get-frame [] f)
  
  (defn get-main-panel [] main-panel)
  (let [max-value 100
        pb (ssw/progress-bar)
        large-label (ssw/label :text "Large")
        small-label (ssw/label :text "Small")]
    (ssw/config! progress-bar-panel
                 :center (seesaw.mig/mig-panel
                           :constraints ["" "[grow][][grow]" 
                                         "[grow][][][][grow]"]
                           :items [[:fill-v "span"]
                                   [:fill-h] [large-label] [:fill-h "wrap"]
                                   [:fill-h] [small-label] [:fill-h "wrap"]
                                   [:fill-h] [pb "gaptop 10, width 300!"] [:fill-h "wrap"]
                                   [:fill-v "gaptop 100, span"]]
                           ;:maximum-size [400 :by 300]
                           ;:align :center
                           :size [400 :by 300]
                           ))
    (.setFont large-label (.deriveFont (.getFont large-label) (float 16)))
    
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

(let [arg-map (atom nil)]
  (defn set-arg-map [as] (reset! arg-map as))
  
  (defn get-arg [a]
    (if (not (contains? @arg-map a))
      (log/warn "tried to access arg:" a "but it is not defined!"))
    (get @arg-map a nil)))
