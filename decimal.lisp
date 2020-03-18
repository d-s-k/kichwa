;;;; Decimal numbers (reading/printing)

(in-package #:kichwa)

(defun decimal-from-string (string)
  (destructuring-bind (whole fractional) (split-sequence #\. string)
    (/ (read-from-string (concatenate 'string whole fractional))
       (expt 10 (length fractional)))))

(defun print-decimal (rational &optional (stream t))
  (assert (rationalp rational))
  (let* ((x (ceiling (log (denominator rational) 10)))
         (source-string (prin1-to-string (* rational (expt 10 x))))
         (decimal-position (- (length source-string) x)))
    (cond ((zerop decimal-position)
           (format stream "0.~A" source-string))
          ((plusp decimal-position)
           (format stream "~A.~A"
                   (subseq source-string 0 decimal-position)
                   (subseq source-string decimal-position)))
          ((minusp decimal-position)
           (format stream "0.~A~A"
                   (make-string (- decimal-position)
                                :initial-element #\0)
                   source-string))))
  rational)

(defparameter *sharp-r-readtable*
  (let ((*readtable* (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\r 'sharp-r-reader)
    *readtable*))

(setf (symbol-function 'original-sharp-r-reader)
      (get-dispatch-macro-character #\# #\r *readtable*))

(defun sharp-r-reader (stream subchar radix)
  (if (= radix 10)
      (let* ((decimal? nil)
             (string (coerce
                      (loop for char = (peek-char nil stream nil nil)
                            while (or (eql char #\.)
                                      (digit-char-p char))
                            when (eql char #\.) do (setf decimal? t)
                              collect (read-char stream char))
                      'string)))
        (if decimal?
            (decimal-from-string string)
            (with-input-from-string (stream-head string)
              (original-sharp-r-reader (make-concatenated-stream stream-head
                                                                 stream)
                                       subchar
                                       radix))))
      (original-sharp-r-reader stream subchar radix)))
