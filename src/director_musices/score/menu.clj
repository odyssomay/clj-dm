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
      (glue/save-score-to-path path)
      )))

(defn choose-and-open-midi [& _]
  (ssw-chooser/choose-file
    :success-fn (fn [_ f]
                  (let [path (.getCanonicalPath f)]
                    (ui/load-score-from-midi path)))))
                  
(defn choose-and-save-midi [& _]
  (if-let [f (util/new-file-dialog)]
    (glue/save-midi-to-path (.getCanonicalPath f))))

(defn a-if-score [& opts]
  (let [a (apply ssw/action opts)
        update #(ssw/config! a :enabled? %)]
    (add-watch global/score-loaded? (gensym)
               (fn [_ _ _ loaded?] (update loaded?)))
    (update @global/score-loaded?)
    a))

(def file-menu
  (ssw/menu
    :text "File"
    :items 
    [(ssw/action :name "Open Score..."
                 :handler choose-and-open-score)
     (a-if-score :name "Save Score As..."
                 :handler choose-and-save-score)
     (a-if-score :name "Save Performance As..."
                 :handler choose-and-save-performance)
     (ssw/separator)
     (ssw/action :name "Import Score From Midifile..."
                 :handler choose-and-open-midi)
     (a-if-score :name "Save Midifile As..."
                 :handler choose-and-save-midi)
     (ssw/separator)
     (ssw/action :name "Quit"
                 :handler (fn [&_ ] (System/exit 0)))
     ]))