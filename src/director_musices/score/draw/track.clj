(ns director-musices.score.draw.track
  (:use [clojure.java.io :only [resource]])
  (:require [director-musices.score.draw.calculate :as calc]
            [taoensso.timbre :as log]
            [seesaw [core :as ssw]
                    [graphics :as ssw-graphics]]))

(def line-separation calc/line-separation)
(def first-note-offset 45)

(def svg-universe (com.kitfox.svg.SVGUniverse.))

(defn get-diagram [id]
  (.getDiagram svg-universe (.toURI (resource id))))

(defn draw-svg [g id]
  (try
    (.render (get-diagram id) g)
    (catch Exception e
      (log/error "unable to render file svg:" id ", error:" e))))

(defn transform-for-note [g note {:keys [scale-x]}] 
  (if (:rest note)
    (condp <= (:nlength note)
      1    (do (.translate g (double (* (/ (:x-offset note) 2) scale-x)) (+ line-separation 4.5))
               (.scale g 0.3 0.25))
      1/2  (do (.translate g (double (* (/ (:x-offset note) 2) scale-x)) (double line-separation))
               (.scale g 0.3 0.25)) 
      1/4  (do (.translate g 0 4))
      1/8  (do (.translate g 0 8)
               (.scale g 1.15 1.15))
      1/16 (do (.translate g 0 2)
               (.scale g 1.15 1.15))
      (do (.translate g 0 2)
        (.scale g 1.15 1.15))
      )
    (cond
      (= (:length note) 4) (do (.translate g 0.0 0.2) (.scale g 1.05 1.05)) 
      :else (do (.scale g 0.28 0.28) (.translate g 0 -74))
      )))

(defn draw-bar [g]
  (.drawLine g -5 0 -5 (* 4 line-separation)))

(defn draw-accidental [g {[_ accidental] :pitch}]
  (if (or (= accidental \#)
          (= accidental \b))
    (let [gc (.create g)]
      (case accidental
        \# (do
             (.scale gc 0.7 0.7)
             (.translate gc -7 -4)
             (draw-svg gc "score/sharp.svg"))
        \b (do 
             (.translate gc -7 -6)
             (draw-svg gc "score/flat.svg"))))))

(defn draw-dots [g {:keys [dot]}]
  (when dot
    (.drawOval g 10 3 1.5 1.5)
    (if (== dot 2)
      (.drawOval g 14 3 1.5 1.5))))

(defn draw-note-help-lines [g height]
  (let [gc (.create g)
        upper? (< height 0)
        lower? (> height 8)]
    (.setColor gc java.awt.Color/black)
    (if (or upper? lower?)
      (doseq [h (range (if upper? 2 10)  
                       (+ (java.lang.Math/abs height) (if lower? 2 0)) 
                       2)]
        (let [y (* h (/ line-separation 2) (if upper? -1 1))]
          (.drawLine gc -1.5 y 9 y))))))

(defn set-hollow [g note]
  (when (:hollow? note)
    (.setComposite g (java.awt.AlphaComposite/getInstance
                       java.awt.AlphaComposite/SRC_OVER 0.5))))

(defn draw-note [g note {:keys [scale] :as options}]
  (let [img
        (condp <= (:nlength note)
          1 "whole"
          1/2 "half"
          1/4 "quarter"
          1/8 "eighth"
          1/16 "sixteenth"
          1/32 "thirtysecond"
          "thirtysecond")
        gc (.create g)]
    (if (:rest note)
      (do (transform-for-note gc note options)
          (draw-svg gc (str "score/" img "_rest.svg")))
      (let [height (:height note)]
        (let [gc (.create g)
              y-offset (:y-offset note)]
          (set-hollow gc note)
          (draw-note-help-lines gc height)
          (.translate gc (double 0) (double y-offset))
          (draw-accidental gc note)
          (draw-dots gc note)
          (transform-for-note gc note options)
          (draw-svg gc (str "score/" img ".svg")))))))

(defn draw-notes [g notes {:keys [scale-x] :as options}]
  (let [gc (.create g)]
    (doseq [i (range (count notes))]
      (let [note (nth notes i)]
        (.translate gc (double (* (:x-offset note) scale-x)) (double 0))
        (if (:bar note)
          (draw-bar gc))
        (draw-note gc note options)))))

(defn draw-clef [g clef]
  (let [gc (.create g)]
    (case clef
      \F (do (.scale gc 1.15 1.15)
             (draw-svg gc "score/fclef.svg"))
      \C (do (.scale gc 1.12 1.12)
             (.translate gc 0.0 0.3)
             (draw-svg gc "score/cclef.svg"))
      \G (do (.scale gc 1.15 1.15)
             (.translate gc 0 -7)
             (draw-svg gc "score/gclef.svg")))))

(defn get-track-component-width [{:keys [track scale scale-x clef]}]
  (* scale
     (+ (if clef 35 0) 10
        (* scale-x
           (let [ln (last (calc/get-notes track))]
             (+ (:absolute-x-offset ln)
                (:length ln)))))))

(defn get-track-component-height [{:keys [track scale] :as state}]
  (* scale (calc/get-height track)))

(defn draw-lines [track g]
  (let [width (calc/get-width track)
        gc (.create g)]
    (.setColor gc java.awt.Color/gray)
    (doseq [line (range 5)]
      (let [y (* line-separation line)]
        (.drawLine gc 0 y width y)))))

(defn highlight-note [g note {:keys [track scale-x scale] :as state}]
  (let [gc (.create g)]
    (ssw-graphics/anti-alias gc)
    (.scale gc scale scale)
    (.translate gc (double 0.0) 
                (double (- (calc/get-lowest track))))
    (when (:clef state)
      (.translate gc 5 0)
      (.translate gc 30 0))
    (.translate gc 10 0)
    (.translate gc
                (double (* scale-x (:absolute-x-offset note)))
                (double (:y-offset note)))
    (.setColor gc (java.awt.Color. 255 0 0 100))
    (.fillRect gc -1 -1 11 10)
    (.setColor gc java.awt.Color/red)
    (.drawRect gc -1 -1 11 10)
    ))

(defn draw-line-indicator [g state]
  (if-let [position (:position-indicator state)]
    (let [gc (.create g)]
      (.translate gc 45.0 0.0)
      (.setColor gc java.awt.Color/red)
      (.translate gc 
                  (double (* (- (get-track-component-width state) 45)
                     position))
                  (double 0))
      (.drawLine gc 0 0 0 (get-track-component-height state))
      )))

(defn paint [g state]
  (let [gc (.create g)
        scale (:scale state)
        scale-x (:scale-x state)
        track (:track state)
        notes (calc/get-notes track)]
    (ssw-graphics/anti-alias gc)
    (.setColor gc java.awt.Color/black)
    (.scale gc scale scale)
    (.translate gc
                (double 0.0) 
                (double (- (calc/get-lowest track))))
    (draw-lines (:track state) gc)
    (when-let [clef (:clef state)]
      (.translate gc 5 0)
      (draw-clef gc clef)
      (.translate gc 30 0)
      ;(draw-bar gc)
      )
    (.translate gc 10 0)
    (draw-notes gc notes state)))

(defn create-image [state]
  (let [track (:track state)
        img (java.awt.image.BufferedImage.
              (get-track-component-width state)
              (get-track-component-height state)
              java.awt.image.BufferedImage/TYPE_INT_ARGB)
        g (.getGraphics img)]
    (paint g state)
    img))

(defn track-component [track & {:as opts}] 
  (let [state (atom (merge {:scale 1 :scale-x 1
                            :position-indicator nil
                            :track (calc/calculate-track track)}
                           opts))
        image-atom (atom (create-image @state))
        update-image (fn [] (reset! image-atom (create-image @state)))
        get-track #(:track @state)
        c (proxy [javax.swing.JComponent] []
            (paintComponent [g]
              (.drawImage g @image-atom 0 0 nil)
              (when-let [hnote (:highlighted-note @state)]
                (highlight-note g hnote @state)))
            (getPreferredSize []
              (let [t (get-track)]
                (java.awt.Dimension.
                  (get-track-component-width @state)
                  (* (:scale @state) (calc/get-height t))))))]
    (add-watch state (gensym)
               (fn [_ _ _ state]
                 (update-image)
                 (.revalidate c)
                 (.repaint c)))
    {:view c
     :state state}))

;; =====
;; API
;; =====
(defn get-state [component-m] @(:state component-m))
(defn get-state-atom [component-m] (:state component-m))

(defn on-state-change [component-m f]
  (add-watch (:state component-m) (gensym :state-change-listener)
             (fn [& _] (f))))

(defn on-track-change [component-m f]
  (add-watch (:state component-m) (gensym :track-change-listener)
             (fn [_ _ prev-state new-state]
               (if (not= (:track prev-state)
                         (:track new-state))
                 (f)))))

(defn set-scale-x [component-m scale-x]
  (swap! (:state component-m) assoc :scale-x scale-x))
(defn get-scale-x [component-m] (:scale-x @(:state component-m)))

(defn set-scale [component-m scale]
  (swap! (:state component-m) assoc :scale scale))
(defn get-scale [component-m] (:scale @(:state component-m)))

(defn get-view [component-m] (:view component-m))

(defn set-position-indicator [component-m position]
  (swap! (:state component-m) assoc :position-indicator position))

(defn abs [x] (if (< x 0) (- x ) x))

(defn get-note-component-position [tc note]
  (let [scale (get-scale tc)
        scale-x (get-scale-x tc)
        {:keys [absolute-x-offset y-offset]} note]
    [(* scale (+ first-note-offset
                 (* scale-x absolute-x-offset)))
     (* scale y-offset)]))

(defn get-note-for-x [component-m x]
  (let [{:keys [track scale scale-x]} @(:state component-m)
        notes (->> (:notes track)
                   (map #(assoc % :cpos (get-note-component-position
                                          component-m %))))]
    (first (sort-by #(abs (- x (first (:cpos %)))) notes))))

(defn get-track [component-m] (:track @(:state component-m)))
(defn set-track [component-m track]
  (swap! (:state component-m) assoc :track (calc/calculate-track track)))

(defn get-notes [component-m] (:notes (get-track component-m)))

(defn highlight-note! [component-m note]
  (swap! (:state component-m) assoc :highlighted-note note))

(defn stop-note-highlight! [component-m]
  (swap! (:state component-m) dissoc :highlighted-note))
