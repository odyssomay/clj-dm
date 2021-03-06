(ns director-musices.score.edit-note
  (:require [director-musices.util :as util]
            (director-musices.score [glue :as glue])
            (director-musices.score.draw [track :as draw-track])
            (seesaw
              [border :as ssw-border]
              [color :as ssw-color]
              [core :as ssw]
              [mig :as ssw-mig])))

(defn update-segment! [track-index segment-index old-segment new-segment]
  (doseq [k (keys new-segment)]
    (let [old-value (get old-segment k nil)
          new-value (get new-segment k)]
      (when (not= old-value new-value)
        (glue/set-segment-parameter
          track-index segment-index k new-value))))
  (doseq [k (keys old-segment)]
    (when-not (contains? new-segment k)
      (glue/remove-segment-parameter track-index segment-index k))))

(defn- set-edit-note-location [dialog tc evt note]
  (let [[nx ny] (draw-track/get-note-component-position tc note)
        scale (draw-track/get-scale tc)
        x-diff (- nx (.getX evt))
        y-diff (- ny (.getY evt))
        x (+ (.getXOnScreen evt)
             x-diff
             (* scale 5))
        y (+ (.getYOnScreen evt)
             y-diff
             (* scale 5))]
    (.setLocation dialog x y)))

(defn- arrow-component [border-color]
  (proxy [javax.swing.JComponent] []
    (paint [g]
      (let [g (.create g)]
        (.setColor g border-color)
        (.fillPolygon g
          (int-array [0 25 0])
          (int-array [0 25 25])
          3)))
    (getPreferredSize []
      (java.awt.Dimension. 25 25))))

(defn- note-value-view [view-items segment-atom k value border-color]
  (let [k-label (ssw/label :text (name k))
        t (ssw/text :text (if (= value ::empty)
                            "" (pr-str value))
                    :columns 10
                    :background (if (= value ::empty)
                                  :red
                                  :white))
        delete
        (util/button-label
          (fn [delete]
            (swap! view-items
                   (fn [items]
                     (remove #(let [f (first %)]
                                (or (= f k-label)
                                    (= f t)
                                    (= f delete)))
                             items)))
            (swap! segment-atom dissoc k))
          :icon "icons/delete.png")]
    (ssw/listen t :document
                (fn [& _]
                  (try
                    (swap! segment-atom assoc k
                           (read-string (ssw/text t)))
                    (ssw/config! t :background :white)
                    (catch Exception e
                      (ssw/config! t :background :red)))))
    [[k-label] [t "gapleft 5"]
     [delete "gapleft 4, wrap"]]))

(defn- edit-note-add [view-items segment-atom border-color]
  (let [add-label
        (util/button-label
          (fn [_]
            (let [new-name (ssw/text)
                  add-new-view
                  (fn [items k]
                    (concat
                      items
                      (note-value-view view-items segment-atom
                                       k ::empty border-color)))
                  remove-new-name
                  (fn [items]
                    (remove #(= (first %) new-name) items))]
              (swap! view-items concat [[new-name "growx, wrap"]])
              (ssw/listen
                new-name
                :action
                (fn [_]
                  (let [k (keyword (ssw/text new-name))]
                    (swap! view-items
                           (fn [items]
                             (remove-new-name
                               (if (= k "")
                                 items
                                 (add-new-view items k)))))
                    ;; HACK
                    (let [t (first (last (butlast @view-items)))]
                      (ssw/request-focus! t)))))
              (ssw/request-focus! new-name)))
          :icon "icons/add.png"
          :halign :center)]
    [add-label "dock south, growx, gapbottom 7"]))

(defn edit-note [tc id mouse-evt reload-score]
  (let [note (draw-track/get-note-for-x tc (.getX mouse-evt))
        note-id (:index note)
        segment (glue/get-segment id note-id)
        segment-atom (atom segment)
        view-items (atom [])
        sorted (sort-by (comp name first) segment)
        border-color (ssw-color/to-color "#FF5050")
        view (ssw-mig/mig-panel
               :constraints ["gap 1, insets 5"]
               :border (ssw-border/line-border
                         :color border-color
                         :thickness 1)
               :background :white)
        dialog (ssw/frame
                 :content
                 (ssw-mig/mig-panel
                   :items [[(arrow-component border-color)
                            "span"]
                           [view]]
                   :constraints ["insets 0, gap 0"]
                   :background (java.awt.Color. 0 0 0 0))
                 :on-close :dispose
                 :resizable? false
                 :undecorated? true)]
    (.setOpaque view true)
    (add-watch view-items nil
               (fn [_ _ _ items]
                 (ssw/config! view :items items)
                 (ssw/pack! dialog)))
    (reset! view-items
            (cons
              (edit-note-add view-items segment-atom border-color)
              (reduce
                concat
                (for [[k value] sorted]
                  (note-value-view
                    view-items segment-atom
                    k value border-color)))))
    (ssw/listen dialog
                :window-deactivated
                (fn [& _] (.dispose dialog)
                  (update-segment! id note-id segment @segment-atom)
                  (reload-score)))
    (doto dialog
      (set-edit-note-location tc mouse-evt note)
      (.setBackground (java.awt.Color. 0 0 0 0))
      ssw/pack!
      ssw/show!)))
