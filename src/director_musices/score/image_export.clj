(ns director-musices.score.image-export
  (:require [director-musices.util :as util]
            (director-musices.score.draw
              [graph :as draw-graph]
              [track :as draw-track])))

(defn export-image [image f]
  (javax.imageio.ImageIO/write image "png" f))

(defn choose-and-export-image [image]
  (if-let [f (util/choose-file
               :title "Save Image"
               :type :save
               :file-ending "png"
               :filters [["Image files (.png)" ["png"]]])]
    (export-image image f)))

(defn choose-and-export-graph-to-image [gc]
  (choose-and-export-image (draw-graph/get-image gc)))

(defn choose-and-export-track-to-image [tc]
  (choose-and-export-image (draw-track/get-image tc)))

