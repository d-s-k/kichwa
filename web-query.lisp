(in-package #:kichwa)

(defgeneric urlify (thing &optional encoder)
  (:documentation "Convert THING to a URL string"))

(defgeneric execute (query)
  (:documentation "Execute QUERY and return the response"))

(defclass web-query ()
  ((base-url :reader base-url :allocation :class)
   ;; could use MOP, but oh well
   (parameter-slots :reader parameter-slots
                    :initform (list)
                    :allocation :class)))

(defclass json-query (web-query) ())

(defmethod initialize-instance :after ((query web-query)
                                       &key &allow-other-keys)
  (mapc (lambda (slot-name)
          (setf (slot-value query slot-name)
                (trivial-http:escape-url-query
                 (slot-value query slot-name))))
        (parameter-slots query)))

(defun url-encode (text)
  (drakma:url-encode text drakma:*drakma-default-external-format*))

;;; Turns out this is completely unneccessary after switching to
;;; Drakma, as HTTP-REQUEST does URL encoding and can be passed an
;;; alist of parameters. Neverthleless, I'm leaving it for now for
;;; flexibility and for the the added benefit of easily being able to
;;; ask objects for their url.
(defmethod urlify ((query web-query) &optional (encoder #'url-encode))
  (let ((separators (list "?" "&")))
    (rplacd (last separators) (cdr separators))
    (apply #'concatenate
           'string
           (list*
            (base-url query)
            (mapcan #'list
                    separators
                    (mapcar (lambda (slot-name)
                              (concatenate 'string
                                           (funcall encoder
                                                    (string-downcase
                                                     (symbol-name slot-name)))
                                           "="
                                           (slot-value query slot-name)))
                            (parameter-slots query)))))))

(defparameter *timeout-seconds* 9
  "Duration to wait for an HTTP response before signalling a
TRIVIAL-TIMEOUT:TIMEOUT-ERROR")

(defvar *cache-response-p* t
  "If true, EXECUTE will use a cached copy of the query response if
  one is available and if the response contains ETag and Last-Modified
  headers")

(defvar *response-cache* (make-hash-table :test 'equal))

(defstruct entity identifier last-modified body)

(defun cached-entity-headers (uri)
  (let ((entity (gethash uri *response-cache*))
        (headers (list))
        identifier
        last-modified)
    (when entity
      (when (setf last-modified (entity-last-modified entity))
        (push (cons "If-Modified-Since" last-modified) headers))
      (when (setf identifier (entity-identifier entity))
        (push (cons "If-None-Match" identifier) headers))
      headers)))

(defun maybe-entity (headers-alist &optional body)
  (let ((id (cdr (assoc :etag headers-alist)))
        (last-modified (cdr (assoc :last-modified headers-alist))))
    (when (or id last-modified)
      (make-entity :identifier id
                   :last-modified last-modified
                   :body body))))

(defun cache-entity-for-uri (uri entity)
  (setf (gethash uri *response-cache*) entity))

(defun cached-entity (uri)
  (gethash uri *response-cache*))

(defmethod execute ((query json-query))
  (let ((uri (urlify query)) stream status headers)
    (with-open-stream (stream
                       (multiple-value-setq (stream status headers)
                         (drakma:http-request
                          (urlify query)
                          :want-stream t
                          :preserve-uri t
                          :connection-timeout *timeout-seconds*
                          :additional-headers (and
                                               *cache-response-p*
                                               (cached-entity-headers uri)))))
      (case status
        (304 (progn
               #+:debug (format t "EXECUTE: Got 304: Using cached copy of~%~%")
               (entity-body (cached-entity uri))))
        (t (let ((response (cl-json:decode-json stream)))
             (when *cache-response-p*
               (let ((entity (maybe-entity headers response)))
                 (when entity
                   (cache-entity-for-uri uri entity))))
             response))))))
