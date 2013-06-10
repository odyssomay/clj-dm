(ns director-musices.common-lisp.command-line
  (:require [director-musices.global :as global]
            [director-musices.common-lisp.glue :as glue]
            [seesaw
             [core :as ssw]
             [mig :as ssw-mig]])
  (:import java.awt.event.KeyEvent))

(def cl-panel (ssw-mig/mig-panel))
(def frame (ssw/dialog :content cl-panel
                       :size [500 :by 500]))

(defn init []
  (let [history (atom '(""))
        history-position (atom -1)
        
        output (ssw/text :editable? false
                         :multi-line? true)
        input (ssw/text)
        
        set-history-text
        (fn []
          (let [i @history-position
                string (if (== i -1)
                         ""
                         (nth @history i))]
          (ssw/text! input string)))]
    (.setFont output (java.awt.Font. "Monospaced", java.awt.Font/PLAIN, 12))
    (ssw/listen input
      :action
      (fn [_]
        (.append output (str "dm=> " (ssw/text input) "\n"))
        (.append output
                 (.printObject
                   (glue/eval-dm (ssw/text input))))
        (.append output "\n")
        (swap! history conj (ssw/text input))
        (reset! history-position -1)
        (ssw/text! input ""))
      :key-pressed
      (fn [e]
        (condp = (.getKeyCode e)
          KeyEvent/VK_UP (do (swap! history-position
                                    (fn [i]
                                      (min (inc i) (dec (count @history)))))
                           (set-history-text))
          KeyEvent/VK_DOWN (do (swap! history-position
                                      (fn [i]
                                        (max (dec i) -1)))
                             (set-history-text))
          nil))
      )
    (ssw/config! cl-panel
                 :constraints ["fill, insets 0" "" "[grow 0][grow 100][grow 0]"]
                 :items [["Output" "wrap, grow 0"]
                         [(ssw/scrollable output) "wrap, growx, growy 100"]
                         [input "growx"]])))

(defn show! [& _]
  (-> frame
      (.setLocationRelativeTo (global/get-frame))
      ssw/show!))

(defn run-test []
  (init)
  (show!))
