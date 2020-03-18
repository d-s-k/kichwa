(in-package #:kichwa)

;;; These are copied from birch. Wrapping READ-LINE in WITH-TIMEOUT is
;;; the only modification. --dsk

(defun read-message (connection)
  (parse-message
   (string-trim '(#\Return)
                (handler-bind
                    ((flex:external-format-encoding-error
                       (lambda (error)
                         (declare (ignore error))
                         (invoke-restart 'use-value #\REPLACEMENT_CHARACTER))))
                  ;;(read-line (socket-stream connection))
                  #-nil(with-timeout ((read-timeout connection))
                         (read-line (socket-stream connection)))))))

(defun process-message (connection)
  "Reads a message from CONNECTION and calls HANDLE-MESSAGE on it. Should
probably be called in a loop. See PROCESS-MESSAGE-LOOP."
  (multiple-value-call #'handle-message
    connection
    (read-message connection)))

(defun process-message-loop (connection)
  "Continuously calls READ-MESSAGE until the connection is closed."
  ;; We keep executing PROCESS-MESSAGE until END-OF-FILE is reached. In that
  ;; case, we check if we wanted to quit, and if not we try to reconnect until
  ;; that succeeds.
  (loop do (handler-case
               (loop (process-message connection))
             (end-of-file ()
               (if (activep connection)
                   (handler-case
                       (progn (sleep 5)
                              (connect connection)
                              t)
                     (serious-condition nil))
                   (loop-finish))))))
