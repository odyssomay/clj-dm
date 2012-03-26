(defproject 
  director-musices "1"
  :description "FIXME: write description"
  :disable-deps-clean true
  :resources-path "resources"
  :main director-musices.main
  :manifest ["SplashScreen-Image" "splash.gif"]
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/tools.logging "0.1.2"]
                 [seesaw "1.1.0"]
                 [com.miglayout/miglayout "3.7.4"]
                 [org.armedbear.lisp/abcl "0.25.0"]
                 [log4j "1.2.15" :exclusions [javax.mail/mail
                                              javax.jms/jms
                                              com.sun.jdmk/jmxtools
                                              com.sun.jmx/jmxri]]]
;  :dev-dependencies [[com.stuartsierra/lazytest "1.1.2"]]
  )
