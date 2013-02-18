(ns director-musices.score.menu
  (:require (director-musices
              [player :as player]
              [util :as util])
            (director-musices.score
              [global :as global]
              [glue :as glue]
              [ui :as ui])
            (seesaw
              [chooser :as ssw-chooser]
              [core :as ssw])))

(defn choose-and-open-score [& _]
  (ssw-chooser/choose-file
    :success-fn 
    (fn [_ f]
      (let [path (.getCanonicalPath f)]
        (ui/load-score-from-path path)
        ))))

(defn choose-and-save-performance [& _]
  (if-let [f (util/new-file-dialog)]
    (spit f (glue/get-active-score))))

(defn choose-and-save-score [& _]
  (if-let [f (util/new-file-dialog)]
    (let [path (.getCanonicalPath f)]
      ;(.execute (abcl-f "DM" "save-score-fpath") (str->abcl path))
      )))

(defn choose-and-open-midi [& _]
  (ssw-chooser/choose-file
    :success-fn (fn [_ f]
                  (let [path (.getCanonicalPath f)]
                    (ui/load-score-from-midi path)))))
                  
(defn choose-and-save-midi [& _]
  )

(def file-menu
  (ssw/menu
    :text "File"
    :items 
    [(ssw/action :name "Open Score"
                 :handler choose-and-open-score)
     (ssw/action :name "Save Score"
                 :handler choose-and-save-score)
     (ssw/action :name "Save Performance"
                 :handler choose-and-save-performance)
     (ssw/separator)
     (ssw/action :name "Import Midifile"
                 :handler choose-and-open-midi)
     (ssw/action :name "Save Midifile"
                 :handler choose-and-save-midi)
     (ssw/separator)
     (ssw/action :name "Quit"
                 :handler (fn [&_ ] (System/exit 0)))
     ]))