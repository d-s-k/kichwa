(defpackage :kichwa
  (:use :cl
        :birch
        :split-sequence
        :trivial-timeout)
  (:import-from :bordeaux-threads
                #:make-thread)
  (:shadow #:connect
           #:connect-socket
           #:read-message
           #:process-message
           #:process-message-loop)
  (:export #:kichwa-connection
           #:connect
           #:connect-socket
           #:command-event
           #:command
           #:arguments
           #:handle-command
           #:/privmsg-symmetrically
           #:defcommand
           #:list-commands
           #:command-documentation

           #:format-integer-with-commas
           #:format-float-with-commas

           #:urlify
           #:execute
           #:json-query
           #:base-url
           #:parameter-slots
           ))

;;; Kludgity kludge-kludge: inherit shadowed symbols from KICHWA
(eval-when (:compile-toplevel :load-toplevel :execute)
  (macrolet ((define-kichwa-package (package &rest options)
               `(defpackage ,package
                  (:shadowing-import-from
                   :kichwa ,@(mapcar #'symbol-name
                                     (package-shadowing-symbols :kichwa)))
                  ,@options)))

    (define-kichwa-package :kichwa/exclaim
      (:use :cl :kichwa :birch))

    (define-kichwa-package :kichwa/weather
      (:use :cl
            :json
            :local-time
            :trivial-timeout
            :birch
            :kichwa))

    (define-kichwa-package :kichwa/bitcoin
        (:use :cl
              :json
              :trivial-http
              :trivial-timeout
              :birch
              :kichwa))

    (define-kichwa-package :kichwa/exchange-rate
        (:use :cl
              :json
              :trivial-timeout
              :birch
              :kichwa))
    ))

;;; Local Variables:
;;; Eval: (put 'define-kichwa-package 'common-lisp-indent-function '(4 2))
;;; End:
