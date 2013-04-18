(defproject 
  director-musices "3.0.1"
  :description "FIXME: write description"
  :manifest ["SplashScreen-Image" "splash.gif"]
  :dependencies [[org.clojure/clojure "1.5.0"]
                 [seesaw "1.4.3"]
                 [com.miglayout/miglayout "3.7.4"]
                 [abcl "1.1.1"]
                 [kitfox/svgsalamander "1.0"]
                 [org.clojure/tools.cli "0.2.2"]
                 [com.taoensso/timbre "1.5.2"]]
  :jvm-opts ["-Xmx500M"]
  :profiles {:dev {:dependencies [[org.clojure/tools.namespace "0.2.2"]]}
             :jar {:main director-musices.main}})
