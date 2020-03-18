;;; -*- Mode: Lisp -*-

(asdf:defsystem "kichwa"
  :description "kichwa: a simple IRC bot"
  :version "0.0.1"
  :author "Drew Kuehn <mookeen @ the mail that is G.com>"
  :licence "Public Domain"
  ;;:serial t
  :depends-on ("trivial-http"
               "drakma"
               "trivial-timeout"
               "cl-json"
               "birch"
               "local-time"
               "bordeaux-threads"
               "split-sequence"
               "flexi-streams"
               "cl+ssl"
               "cl-dbi"
               ;;"sqlite"
               )
  :components ((:file "packages")
               (:file "kichwa" :depends-on ("packages"))
               (:file "pretty-print" :depends-on ("packages"))
               (:file "web-query" :depends-on ("packages"))
               (:file "init" :depends-on ("packages" "kichwa"))
               (:file "weather"
                :depends-on ("packages" "kichwa" "web-query")
                :perform (load-op :after (op component)
                                  (funcall
                                   (intern "REREAD-TIMEZONE-REPOSITORY"
                                                 '#:local-time))))
               (:file "exchange-rate"
                :depends-on ("packages" "kichwa" "web-query" "pretty-print"))
               (:file "exclaim" :depends-on ("packages" "kichwa"))
               (:file "bitcoin" :depends-on ("packages" "kichwa" "pretty-print"))))
