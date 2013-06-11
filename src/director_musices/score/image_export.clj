(ns director_musices.score.image_export
  (:require (director-musices.score.draw
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
  (draw-graph/get-image gc))

(defn choose-and-export-track-to-image [tc]
  (draw-track/get-image tc))

