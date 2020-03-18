(in-package #:kichwa)

(defun format-integer-with-commas (integer)
  (declare (integer integer))
  (check-type integer integer)
  (let* ((source (format nil "~D" integer))
         (length (length source))
         (commas (- (floor length 3) (if (minusp integer) 1 0)))
         (dest (make-string (+ length commas)))
         (offset commas))
    (do ((index (1- length) (1- index))
         (indicator 1 (mod (1+ indicator) 3)))
        (nil)
      (setf (char dest (+ index offset)) (char source index))
      (cond ((zerop index)
             (return dest))
            ((zerop indicator)
             (setf (char dest (+ index (decf offset))) #\,))))))

(defun format-float-with-commas (float)
  (let ((source (format nil "~4$" float)))
    (destructuring-bind (integral-part fractional-part)
        (split-sequence:split-sequence #\. source)
      (concatenate 'string
                   (format-integer-with-commas
                    (read-from-string integral-part))
                   "."
                   fractional-part))))
