(ns director-musices.score.draw.track
  (:use [clojure.java.io :only [resource]])
  (:require [director-musices.score.draw.calculate :as calc]
            [taoensso.timbre :as log]
            [seesaw [core :as ssw]
                    [graphics :as ssw-graphics]]))

(def line-separation calc/line-separation)
(def first-note-offset 45)

(def svg-universe (com.kitfox.svg.SVGUniverse.))

(def get-diagram
  (memoize
    (fn [id]
      (.getDiagram svg-universe (.toURI (resource id))))))

(defn draw-svg [g id]
  (try
    (.render (get-diagram id) g)
    (catch Exception e
      (log/error "unable to render file svg:" id ", error:" e))))

(defn transform-for-note [g note {:keys [scale-x]}] 
  (if (:rest note)
    (condp <= (:nlength note)
      1    (do (.translate g 0.0 (+ line-separation 4.5))
               (.scale g 0.3 0.25))
      1/2  (do (.translate g 0.0 (double line-separation))
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
      (= (:nlength note) 1) (do (.translate g 0.0 0.2) (.scale g 1.05 1.05))
      :else (do (.scale g 0.28 0.28) (.translate g 0 -74)))))

(defn draw-bar [g {:keys [scale-x]}]
  (let [x (max -5 (min -1 (* -50 scale-x)))]
    (.drawLine g x 0 x (* 4 line-separation))))

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

(defn draw-note [g note top-note? {:keys [scale] :as options}]
  (let [img (condp <= (:nlength note)
              1 "whole"
              1/2 "half"
              1/4 "quarter"
              1/8 "eighth"
              1/16 "sixteenth"
              1/32 "thirtysecond"
              "thirtysecond")
        img (if top-note?
              img
              (condp <= (:nlength note)
                1/8 "quarter"
                1/16 "quarter"
                1/32 "quarter"
                img))
        ; img (if (#{1/8 1/16 1/32} (:nlength note)))
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
          (draw-bar gc options))
        (let [sorted-notes (sort-by :y-offset (conj (:chord note) note))]
          (draw-note gc (first sorted-notes) true options)
          (doseq [n (rest sorted-notes)]
            (draw-note gc n false options)))))))

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

(defn paint [img g state]
  (let [gc (.create g)
        scale (:scale state)
        scale-x (:scale-x state)
        track (:track state)
        notes (calc/get-notes track)]
    (let [w (.getWidth img)
          h (.getHeight img)]
      (.setColor g java.awt.Color/white)
      (.fillRect g 0 0 w h))
    (ssw-graphics/anti-alias gc)
    (.setColor gc java.awt.Color/black)
    (.scale gc scale scale)
    (.translate gc
                (double 0.0) 
                (double (- (calc/get-lowest track))))
    (draw-lines (:track state) gc)
    (when-let [clef (:clef track)]
      (.translate gc 5 0)
      (draw-clef gc clef)
      (.translate gc 30 0)
      ;(draw-bar gc)
      )
    (.translate gc 10 0)
    (draw-notes gc notes state)))

(defn create-image [state]
  (let [track (:track state)
        ba (byte-array (map byte [0 63 127]))
        icm (java.awt.image.IndexColorModel.
              2 3 ba ba ba)
        img (java.awt.image.BufferedImage.
              (int (get-track-component-width state))
              (int (get-track-component-height state))
              java.awt.image.BufferedImage/TYPE_BYTE_INDEXED)
        g (.getGraphics img)]
    (paint img g state)
    img))

(defn track-component [track & {:as opts}] 
  (let [state (atom (merge {:scale 1 :scale-x 1
                            :track (calc/calculate-track track)}
                           opts))
        temporary-scale-x (atom nil)
        temporary-scale (atom nil)
        state-listeners (atom [])
        track-listeners (atom [])
        image-atom (atom (create-image @state))
        update-image (fn [] (reset! image-atom (create-image @state)))
        get-track #(:track @state)
        c (proxy [javax.swing.JComponent] []
            (paintComponent [g]
              (let [g (.create g)]
                (when-let [scale-x @temporary-scale-x]
                  (.scale g (double (/ scale-x (:scale-x @state))) 1.0))
                (when-let [scale @temporary-scale]
                  (.scale g scale scale))
                (.drawImage g @image-atom 0 0 nil)
                (when-let [hnote (:highlighted-note @state)]
                  (highlight-note g hnote @state))))
            (getPreferredSize []
              (let [t (get-track)]
                (java.awt.Dimension.
                  (get-track-component-width @state)
                  (* (:scale @state) (calc/get-height t))))))
        fire-state-listeners
        (fn []
          (doseq [f @state-listeners]
            (f)))
        fire-track-listeners
        (fn []
          (doseq [f @track-listeners]
            (f)))]
    (add-watch state (gensym)
               (fn [_ _ old-state state]
                 (ssw/invoke-now
                   (update-image)
                   (if (not= (:track old-state)
                             (:track state))
                     (fire-track-listeners))
                   (fire-state-listeners))))
    (add-watch temporary-scale nil
               (fn [& _] (ssw/invoke-now
                           (fire-state-listeners))))
    (add-watch temporary-scale-x nil
               (fn [& _] (ssw/invoke-now
                           (fire-state-listeners))))
    {:view c
     :state state
     :state-listeners state-listeners
     :track-listeners track-listeners
     :temporary-scale temporary-scale
     :temporary-scale-x temporary-scale-x
     :image-atom image-atom}))

;; =====
;; API
;; =====
(defn get-state [component-m] @(:state component-m))
(defn get-state-atom [component-m] (:state component-m))

(defn on-state-change [component-m f]
  (swap! (:state-listeners component-m) conj f))

(defn on-track-change [component-m f]
  (swap! (:track-listeners component-m) conj f))

(defn set-temporary-scale-x [component-m scale-x]
  (reset! (:temporary-scale-x component-m) (max 0.01 scale-x)))
(defn set-scale-x [component-m scale-x]
  (swap! (:state component-m) assoc :scale-x (max 0.01 scale-x))
  (reset! (:temporary-scale-x component-m) nil))
(defn get-scale-x [component-m] (:scale-x @(:state component-m)))

(defn set-temporary-scale [component-m scale]
  (reset! (:temporary-scale component-m) scale))
(defn set-scale [component-m scale]
  (swap! (:state component-m) assoc :scale scale)
  (reset! (:temporary-scale component-m) nil))
(defn get-scale [component-m] (:scale @(:state component-m)))

(defn get-view [component-m] (:view component-m))

(defn get-track [component-m] (:track @(:state component-m)))
(defn set-track [component-m track]
  (swap! (:state component-m) assoc :track (calc/calculate-track track)))

(defn get-notes [component-m] (:notes (get-track component-m)))

(defn abs [x] (if (< x 0) (- x ) x))

(defn get-note-component-position [tc note]
  (let [scale (get-scale tc)
        scale-x (get-scale-x tc)
        {:keys [absolute-x-offset y-offset]} note
        y-offset (if (:rest note)
                   (* 3 line-separation)
                   y-offset)]
    [(* scale (+ first-note-offset
                 (* scale-x absolute-x-offset)))
     (* scale (- y-offset (calc/get-lowest (get-track tc))))]))

(defn get-note-for-x [component-m x]
  (let [{:keys [track scale scale-x]} @(:state component-m)
        notes (->> (:notes track)
                   (map #(assoc % :cpos (get-note-component-position
                                          component-m %))))]
    (first (sort-by #(abs (- x (first (:cpos %)))) notes))))

(defn highlight-note! [component-m note]
  (swap! (:state component-m) assoc :highlighted-note note))

(defn stop-note-highlight! [component-m]
  (swap! (:state component-m) dissoc :highlighted-note))

(defn get-image [component-m]
  @(:image-atom component-m))