(in-package #:kichwa/exchange-rate)

(defparameter *openexchangerates.org-key*
  "a015730963b9477f8306542e29a62a5d")

;;; XXX bitcoin.lisp has its own version of this. Factor them into a
;;; common file.
(define-condition invalid-currency (error)
  ((currency :initarg :currency
             :reader invalid-currency-currency))
  (:report invalid-currency-report))

(defun invalid-currency-report (condition stream)
  (format stream
          "~A is not a recognized currency."
          (invalid-currency-currency condition)))

(defclass exchange-rates-query (json-query)
  ((base-url :initform "https://openexchangerates.org/api/latest.json"
             :allocation :class)
   (parameter-slots :initform '(app_id base show_alternative) :allocation :class)
   (app_id :initarg :app_id :initform *openexchangerates.org-key*)
   (base :initarg :base :initform "USD")
   (show_alternative :initarg :show_alternative :initform "true")))

(defparameter *timeout-seconds* 10)

(defmethod execute ((query exchange-rates-query))
  (with-open-stream (stream (drakma:http-request
                             (urlify query)
                             :want-stream t
                             :connection-timeout *timeout-seconds*))
    (cl-json:decode-json stream)))

(defun value-in-base-currency (currency rates-alist)
  (let ((currency-name-with-plus-signs
          (intern (format nil "+~A+" (symbol-name currency))
                  '#:keyword)))
    (or (cdr (assoc currency-name-with-plus-signs rates-alist))
        (error 'invalid-currency :currency currency))))

(defun convert-amount (amount source destination rates-alist)
  (let ((source-value (value-in-base-currency source rates-alist))
        (destination-value (value-in-base-currency destination rates-alist)))
    (* (/ amount source-value) destination-value)))

(defun rates-from-response (response)
  (cdr (assoc :rates response)))

(defun unix-to-timestring (unix-time)
  (local-time:format-rfc1123-timestring
   nil
   (local-time:unix-to-timestamp unix-time)))

(defun format-response (amount source destination timestamp rates-alist)
  (format nil
          "As of ~A, ~A ~A = ~A ~A"
          (unix-to-timestring timestamp)
          amount
          source
          (format-float-with-commas
           (convert-amount amount source destination rates-alist))
          destination))

(defcommand :convert (connection event)
  "Usage: CONVERT <amount> <source currency> <destination currency>. Display
current value of <amount> of <source currency> in <destination currency>"
  (destructuring-bind (&optional amount source destination &rest junk)
      (mapcar #'string-upcase
              (split-sequence:split-sequence #\Space
                                             (arguments event)
                                             :remove-empty-subseqs t))
    (declare (ignore junk))
    (/privmsg-symmetrically
     connection
     event
     (if (not (and destination source amount))
         "Usage: CONVERT <amount> <source currency> <destination currency>"
         #+:debug
         (let* ((response (execute (make-instance 'exchange-rates-query)))
                (rates (cdr (assoc :rates response)))
                (timestamp (cdr (assoc :timestamp response))))
           (with-standard-io-syntax
             (let ((*read-default-float-format* 'double-float)
                   (*read-eval* nil))
               (format-response (read-from-string amount)
                                (intern (string-upcase source)
                                        '#:keyword)
                                (intern (string-upcase destination)
                                        '#:keyword)
                                timestamp
                                rates))))
         #-:debug
         (handler-case
             (let* ((response (execute (make-instance 'exchange-rates-query)))
                    (rates (cdr (assoc :rates response)))
                    (timestamp (cdr (assoc :timestamp response))))
               (with-standard-io-syntax
                 (let ((*read-default-float-format* 'double-float)
                       (*read-eval* nil))
                   (format-response (read-from-string amount)
                                    (intern (string-upcase source)
                                            '#:keyword)
                                    (intern (string-upcase destination)
                                            '#:keyword)
                                    timestamp
                                    rates))))
           (invalid-currency (c)
             (invalid-currency-report c nil))
           (error (c)
             (declare (ignore c))
             "Unable to complete action."))))))
