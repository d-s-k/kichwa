(in-package #:kichwa)

;;; Not currently used for anything

(defun memoized (symbol) (get symbol 'memoized))
(defsetf memoized (symbol) (v)
  `(setf (get ,symbol 'memoized) ,v))

(defun unmemoize (symbol)
  (let ((memoized-function (get symbol 'memoized)))
    (when memoized-function
      (setf (symbol-function symbol) memoized-function))))

(defun memoize-persistently (symbol file &aux (eof (make-symbol "EOF")))
  (flet ((wrapper (&rest memo-args)
           (format t "MEMOIZATION: got arguments ~A~%" memo-args)
           (with-open-file (stream file :direction :io
                                        :if-exists :append
                                        :if-does-not-exist :create)
             (loop for key = (read stream nil eof)
                   for value = (read stream nil nil)
                   do (format t "MEMOIZATION: cur key = ~A.~%" key)
                      (cond ((equalp memo-args key)
                             (format t "MEMOIZATION: using cached value.~%")
                             (return-from wrapper value))
                            (t
                             (format t "MEMOIZATION: this should write the file...")
                             (write-char #\Space stream)
                             (prin1 memo-args stream)
                             (write-char #\Space stream)
                             (let ((cached (apply (memoized symbol) memo-args)))
                               (prin1 cached stream)
                               (finish-output stream)
                               (return-from wrapper cached))))))))
    (setf (memoized symbol) (symbol-function symbol)
          (symbol-function symbol) #'wrapper)))
