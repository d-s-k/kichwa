;;; Powered by CoinDesk: http://www.coindesk.com/price/

(in-package #:kichwa/bitcoin)

(defparameter *endpoint-format-string*
  "http://api.coindesk.com/v1/bpi/currentprice/~A.json")

(defparameter *currencies*
  '#("AED" "AFN" "ALL" "AMD" "ANG" "AOA" "ARS" "AUD" "AWG" "AZN" "BAM" "BBD"
     "BDT" "BGN" "BHD" "BIF" "BMD" "BND" "BOB" "BRL" "BSD" "BTC" "BTN" "BWP"
     "BYR" "BZD" "CAD" "CDF" "CHF" "CLF" "CLP" "CNY" "COP" "CRC" "CUP" "CVE"
     "CZK" "DJF" "DKK" "DOP" "DZD" "EEK" "EGP" "ERN" "ETB" "EUR" "FJD" "FKP"
     "GBP" "GEL" "GHS" "GIP" "GMD" "GNF" "GTQ" "GYD" "HKD" "HNL" "HRK" "HTG"
     "HUF" "IDR" "ILS" "INR" "IQD" "IRR" "ISK" "JEP" "JMD" "JOD" "JPY" "KES"
     "KGS" "KHR" "KMF" "KPW" "KRW" "KWD" "KYD" "KZT" "LAK" "LBP" "LKR" "LRD"
     "LSL" "LTL" "LVL" "LYD" "MAD" "MDL" "MGA" "MKD" "MMK" "MNT" "MOP" "MRO"
     "MTL" "MUR" "MVR" "MWK" "MXN" "MYR" "MZN" "NAD" "NGN" "NIO" "NOK" "NPR"
     "NZD" "OMR" "PAB" "PEN" "PGK" "PHP" "PKR" "PLN" "PYG" "QAR" "RON" "RSD"
     "RUB" "RWF" "SAR" "SBD" "SCR" "SDG" "SEK" "SGD" "SHP" "SLL" "SOS" "SRD"
     "STD" "SVC" "SYP" "SZL" "THB" "TJS" "TMT" "TND" "TOP" "TRY" "TTD" "TWD"
     "TZS" "UAH" "UGX" "USD" "UYU" "UZS" "VEF" "VND" "VUV" "WST" "XAF" "XAG"
     "XAU" "XBT" "XCD" "XDR" "XOF" "XPF" "YER" "ZAR" "ZMK" "ZMW" "ZWL")
  "Valid currencies for use with CoinDesk")

(defparameter *cost-of-a-tulip-in-usd* 2.0d0)

(defvar *timeout-seconds* 10)

(define-condition invalid-currency (error)
  ((currency :initarg :currency
             :reader invalid-currency-currency))
  (:report invalid-currency-report))

(defun invalid-currency-report (condition stream)
  (format stream
          "~S is not a currency recognized by CoinDesk."
          (invalid-currency-currency condition)))

;; (defun format-integer-with-commas (integer)
;;   (declare (integer integer))
;;   (assert (integerp integer))
;;   (let* ((commas (floor (log integer 1000)))
;;          (source (write-to-string integer))
;;          (dest (make-string (+ (length source) commas))))
;;     (loop
;;       :for count :from 1 :to (length source)
;;       :for source-index :downfrom (1- (length source))
;;       :for comma? := (and (zerop (mod count 3)) (> source-index 0))
;;       :for dest-index := (1- (length dest)) :then (- dest-index (if comma? 2 1))
;;       :do (if comma?
;;               (progn (setf (char dest dest-index) #\,)
;;                      (setf (char dest (1+ dest-index))
;;                            (char source source-index)))
;;               (setf (char dest dest-index)
;;                     (char source source-index)))
;;       :finally (return dest))))

;; (defun format-float-with-commas (float)
;;   (let ((source (format nil "~4$" float)))
;;     (destructuring-bind (integral-part fractional-part)
;;         (split-sequence:split-sequence #\. source)
;;       (concatenate 'string
;;                    (format-integer-with-commas
;;                     (read-from-string integral-part))
;;                    "."
;;                    fractional-part))))

;;; Note: does not return the response verbatim
(defun query (&optional (currency "USD"))
  (let ((tulips? (equalp currency "tulips")))
    (when tulips? (setf currency "USD"))
    (unless (find currency *currencies* :test #'equal)
      (error 'invalid-currency :currency currency))
    (destructuring-bind (code headers stream)
        (with-timeout (*timeout-seconds*)
          (http-get (format nil *endpoint-format-string*
                            currency)))
      (declare (ignore code headers))
      (unwind-protect
           (let* ((response-alist (decode-json stream))
                  (time-alist (cdr (assoc :time response-alist)))
                  (currency-key (intern (format nil "+~A+" currency) :keyword))
                  (bpi-alist
                   (cdr (assoc currency-key (cdr (assoc :bpi response-alist)))))
                  (time-format
                   '((:day 2) " " :short-month " "
                     (:hour 2) ":" (:min 2) " (" :timezone ")"))
                  (time
                   (local-time:format-timestring
                    nil
                    (local-time:parse-timestring
                     (cdr
                      (assoc :updated-+iso+ time-alist)))
                    :format time-format
                    :timezone local-time:+utc-zone+))
                  (rate (cdr (assoc :rate bpi-alist))))
             (if tulips?
                 (list (cons :time time)
                       (cons :currency "tulips")
                       (cons :rate
                             (format-float-with-commas
                              (/ (with-standard-io-syntax
                                   (let ((*read-default-float-format* 'double-float)
                                         (*read-eval* nil))
                                     (read-from-string (remove #\, rate))))
                                 *cost-of-a-tulip-in-usd*))))
                 (list (cons :time time)
                       (cons :currency currency)
                       (cons :rate rate))))
        (close stream)))))

(defun format-bitcoin-query-response (stream alist)
  (flet ((lookup (key) (cdr (assoc key alist))))
    (declare (inline lookup))
    (format stream
            "Bitcoin rate as of ~A: ~A ~A."
            (lookup :time)
            (lookup :rate)
            (lookup :currency))))

(defcommand :bc (connection event)
  "Usage: BC [CURRENCY]. Display current Bitcoin value in CURRENCY
  from CoinDesk. Currency defaults to \"USD\"."
  (let ((arg (string-upcase (arguments event))))
    (handler-case (/privmsg-symmetrically
                   connection
                   event
                   (format-bitcoin-query-response
                    nil
                    (if (zerop (length arg)) (query) (query arg))))
      (invalid-currency (c)
        (/privmsg-symmetrically
         connection
         event
         (invalid-currency-report c nil)))
      (timeout-error (c)
        (declare (ignore c))
        (/privmsg-symmetrically
         connection
         event
         "Your request timed out. Maybe try again in a few seconds?")))))
