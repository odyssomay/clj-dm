(ns director-musices.glue
  (:use clojure.java.io
        (director-musices interpreter)
        (Hafni.swing component layout text view)))

(def *dm-init?* (atom nil))

(defn load-package-dm []
  (load-abcl "src" "dm_original" "package-dm.lsp"))

(defn load-core []
  (load-dir-abcl
    [] "src" "dm_original" "lib-core")
  (load-abcl "src" "dm_original" "init.lsp"))

(defn load-rules []
  (load-dir-in-abcl
    ["frules1.lsp" "frules2.lsp" "Intonation.lsp"
     "FinalRitard.lsp" "utilityrules.lsp" "Punctuation.lsp"
     "phrasearch.lsp"]
    "src" "dm_original" "rules"))
(comment 
(defn init-dm []
  (when-not @*dm-init?*
    (let [text_area (text-area)]
      (.setUpdatePolicy (.getCaret (component text_area)) javax.swing.text.DefaultCaret/ALWAYS_UPDATE)
      (frame :content (border-layout :north (label :text "Loading lisp environment")
                                     :center (scroll-pane text_area)) 
             :size 300 200 :dont_exit_on_close)
      (with-open [out-writer 
                  (proxy [java.io.Writer] []
                    (write ([input offset length] 
                            (if (string? input)
                              (.write this (subs input offset (+ offset length)))))
                           ([input]
                            (if (number? input)
                              ((input-arr text_area :append) (str (char input)))
                              ((input-arr text_area :append) (str input)))
                            (.setCaretPosition (component text_area) (.getLength (.getDocument (component text_area)))))
                           )
                    (flush [] )
                    (close [] ))]
        (binding [*out* out-writer]
          (load-package-dm)
          (load-core)
          (load-rules)
          (swap! *dm-init?* (constantly true)))))))
)

(defn init-dm []
  (when-not @*dm-init?*
    (load-package-dm)
    (load-core)
    (load-rules)
    (swap! *dm-init?* (constantly true))))

(defn str->abcl [s]
  (eval-abcl (str "\"" s "\"")))

(defn load-active-score [string]
  (init-dm)
  (eval-abcl "(in-package :dm)")
  (.execute (abcl-f "DM" "read-active-score-from-string") (str->abcl string))
  (eval-abcl "(init-music-score)"))

(defn load-active-score-from-file [path]
  (init-dm)
  (eval-abcl "(in-package :dm)")
  (.execute (abcl-f "DM" "read-active-score-from-file") (str->abcl path))
  (eval-abcl "(init-music-score)"))

(defn get-active-score []
  (.execute (abcl-f "DM" "get-active-score")))

(defn apply-rules [rulelist-string sync-rule]
  (init-dm)
  (eval-abcl "(in-package :dm)")
  (eval-abcl (str "(rule-apply-list-sync '(" 
                  rulelist-string ") '" sync-rule ")")) )

