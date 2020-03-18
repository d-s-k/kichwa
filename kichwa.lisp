(in-package :kichwa)

(defclass kichwa-connection (connection)
  ((autojoin-channels :initarg :autojoin-channels
                      :initform (list)
                      :accessor autojoin-channels)
   (command-char :initarg :command-char
                 :initform #\,
                 :accessor command-char)
   (connect-timeout :initarg :connect-timeout
                    :initform 5000.0
                    :accessor connect-timeout)
   (read-timeout :initarg :read-timeout
                 :initform 500.0
                 :accessor read-timeout)
   (max-reconnects :initarg :max-reconnects
                   :initform 10
                   :accessor max-reconnects)
   (%reconnect-counter :initform 0
                       :accessor %reconnect-counter))
  (:documentation "Kichwa's connection class"))

(defclass kichwa-ssl-connection (kichwa-connection) ()
  (:documentation "Kichwa's SSL/TLS connection class"))

(define-condition kichwa-connection-error (error)
  ((connection :initarg :connection
               :reader kichwa-connection-error-connection))
  (:documentation
   "This is the base class for all kichwa errors that
   implicate a connection."))

(define-condition max-reconnects-reached (kichwa-connection-error)
  ((reconnects :initarg :reconnects
               :initform 0
               :reader max-reconnects-reached-reconnects))
  (:report (lambda (condition stream)
             (format stream
                     "~A has reached its maximum number of reconnects at ~A."
                     (kichwa-connection-error-connection condition)
                     (max-reconnects-reached-reconnects condition))))
  (:documentation
   "signalled when a KICHWA-CONNECTION has reached its maximum number
   of reconnect attempts"))

(defparameter *connection*
  (make-instance 'kichwa-connection
                 :server-host "irc.freenode.net"
                 :server-port 6667
                 :nick "kichwa"
                 :real-name "Kichwa Tembo"
                 :autojoin-channels '("#bestchannelever"
                                      ("#secondbestchannel" . "snort"))))

(defparameter *ssl-connection*
  (make-instance 'kichwa-ssl-connection
                 :server-host "irc.freenode.net"
                 :server-port 6697
                 :nick "kichwa-ssl"
                 :real-name "Kichwa Tembo (TLS)"
                 :autojoin-channels '("#ypn"
                                      "#bestchannelever"
                                      ("#secondbestchannel" . "snort"))))

;;; Shadowed functions from birch, reimplimited as GFs so we can
;;; specialize and set sockopts, timeouts, etc.
(defgeneric connect-socket (connection)
  (:method ((connection connection)) (birch:connect-socket connection)))

(defun %connect-socket (connection)
  (let ((socket (usocket:socket-connect (server-host connection)
                                        (server-port connection)
                                        :timeout (connect-timeout connection)
                                        :element-type '(unsigned-byte 8))))
    (setf (birch/connection::%socket connection) socket
          (activep connection) t)
    (when (read-timeout connection)
      (setf (usocket:socket-option socket :receive-timeout)
            (read-timeout connection)))))

(defmethod connect-socket ((connection kichwa-connection))
  (%connect-socket connection)
  (setf (socket-stream connection)
        (flexi-streams:make-flexi-stream
         (usocket:socket-stream (birch/connection::%socket connection))
         :external-format '(:UTF-8 :eol-style :crlf))))

(defmethod connect-socket ((connection kichwa-ssl-connection))
  (%connect-socket connection)
  ;;(format t "... CONNECT-SOCKET connecting for SSL")
  (setf (socket-stream connection)
        (cl+ssl:make-ssl-client-stream
         (usocket:socket-stream (birch/connection::%socket connection))
         :external-format '(:UTF-8 :eol-style :crlf))))

(defgeneric connect (connection)
  ;; Copied verbatim from birch's init.lisp.
  (:method ((connection connection))
    ;; Initialization
    (connect-socket connection)
    (unless (user connection)
      (setf (user connection) (nick connection)))
    ;; Registration
    (if (pass connection) (/pass connection (pass connection)))
    (/nick connection (nick connection))
    (/user connection (user connection) 0 (real-name connection)))
  (:documentation
   "Open a connection and perform initial registration."))

(defun ensure-plist (thing &key (key-test #'symbolp))
  (or (null thing)
      (and (consp thing)
           (loop for item in thing
                 for even = t then (not even)
                 when even
                   do (or (funcall key-test item)
                          (return-from ensure-plist nil))
                 finally (return-from ensure-plist t)))))

;;; Hm, this is reallly just a plist editor, isn't it?
(defun make-interactive-reinitializer (&rest initargs
                                       &aux (initargs (copy-list initargs)))
  (assert (evenp (length initargs)))
  (flet ((read* ()
           ;; Returns two values: the value of (READ STREAM) or NIL,
           ;; and a Boolean indicating whether return was typed.
           (let* ((first-char (peek-char nil))
                  (return-typed? (or (char= first-char #\Return)
                                     (char= first-char #\Newline))))
             (values (unless return-typed? (read))
                     return-typed?))))
    (lambda (instance)
      (block :no-changes
        (apply #'reinitialize-instance
               instance
               (block :save-changes
                 (let ((n-slots (/ (length initargs) 2)))
                   (loop do
                     (format t "~&Current configurable slots for ~A:~%"
                             instance)
                     (loop for (key value) on initargs by #'cddr
                           for choice upfrom 0
                           do (format t "~T~A. ~A: ~T~A~%" choice key value))
                     (format t "~&~&Enter a slot number, S to save ~
                              current settings, or any other key ~
                              to quit.~%Choice: ")
                     (multiple-value-bind (value user-hit-return?) (read*)
                       (cond (user-hit-return?
                              (return-from :no-changes initargs))
                             ((eq value 'S)
                              (return-from :save-changes initargs))
                             ((and (integerp value) (<= 0 value (1- n-slots)))
                              (let ((selection (elt initargs (* value 2))))
                                (format t "~&Enter a new value for slot ~A, ~
                                          or just RETURN return to the ~
                                          previous menu: "
                                        selection)
                                (multiple-value-bind (value abscond?) (read*)
                                  (unless abscond?
                                    (setf (getf initargs selection) value)))))
                             (t
                              (return-from :no-changes initargs))))))))))))

(defun connection-reinitialization-interactor (connection)
  (funcall (make-interactive-reinitializer
            :server-host (server-host connection)
            :server-port (server-port connection)
            :real-name (real-name connection)
            :pass (pass connection))
           connection))

;;; XXX Ewww.
(defun connect-and-begin-processing (connection)
  "Connect CONNECTION, then create a thread running a read loop on it"
  (symbol-macrolet ((conditions-to-handle
                     (list 'stream-error
                           'usocket:socket-condition
                           'usocket:ns-condition
                           'trivial-timeout:timeout-error)))
    (macrolet ((with-lossage-handling (form (condition) &body body)
                 `(HANDLER-CASE ,form
                    ((OR ,@conditions-to-handle) (,condition) ,@body))))
      (make-thread
       (lambda ()
         (connect connection)
         (block :done
           (loop do
             (block :again
               (restart-case
                   (with-lossage-handling
                       (usocket:with-mapped-conditions ()
                         (process-message-loop connection))
                       (condition)
                     (cond
                       ((>= (%reconnect-counter connection)
                            (max-reconnects connection))
                        (error 'max-reconnects-reached
                               :connection connection
                               :reconnects (max-reconnects
                                            connection)))
                       (t
                        (format *error-output*
                                "~&!!! ~A: Attempting reconnect in 5s...~%"
                                condition)
                        (finish-output *error-output*)
                        (sleep 5)
                        (invoke-restart 'reconnect))))
                 (reconnect ()
                   :report (lambda (s)
                             (format s "Try to reconnect to ~A"
                                     (host connection)))
                   (close (socket-stream connection))
                   (incf (%reconnect-counter connection))
                   (with-lossage-handling (connect connection) (condition)
                     (format *error-output*
                             "~&!!! Reconnect failed: ~A~%" condition)))
                 (reconfigure (new-slot-values-plist)
                   :report "Reconfigure the connection before retrying."
                   :interactive (lambda ()
                                  (error
                                   "FIXME! Add this restart for real")))
                 (give-up ()
                   :report "Deactivate connection and leave thread."
                   (return-from :done))))))
         (ignore-errors (close (socket-stream connection)))
         (setf (activep connection) nil)
         (format *error-output*
                 "~&!!! Leaving thread ~A~%"
                 (bt:thread-name (bt:current-thread)))
         (force-output))
       :name "Kichwa"
       :initial-bindings `((*standard-output* . ,*standard-output*)
                           (*error-output* . ,*error-output*)
                           (*trace-output* . ,*trace-output*))))))

(defun join-channels (connection)
  "Join CONNECTION's AUTOJOIN-CHANNELS. When called after a reconnect,
additionally join the channels we were on before losing the
connection."
  (labels ((channel-name (x) (if (consp x) (car x) x))
           (channel= (c1 c2) (equalp (channel-name c1) (channel-name c2))))
    (mapc (lambda (channel)
            (typecase channel
              (string (/join connection channel))
              (cons (destructuring-bind (channel . key) channel
                      (/join connection channel key)))
              (otherwise (warn "~S is not a valid channel. Ignoring."
                               channel))))
          (union (autojoin-channels connection)
                 ;; FIXME: fails to rejoin any *keyed* channel that is
                 ;; not a member of AUTOJOIN-CHANNELS because we don't
                 ;; keep track of keys
                 (mapcar #'name (channels connection)) :test #'channel=))))

;;; Don't let any SERIOUS-CONDITION that might occur during event
;;; handling kill the connection.
(defmethod handle-event :around ((connection kichwa-connection) (event event))
  (handler-case (when (next-method-p) (call-next-method))
    (serious-condition (condition)
      (format *error-output* "*** ~&Caught error: ~A~%" condition))))

(defmethod handle-message ((connection kichwa-connection)
                           prefix
                           (command (eql :RPL_WELCOME))
                           params)
  (format t "~&*** Received RPL_WELCOME.~%")
  (finish-output)
  ;; We've successfully connected, so clear the reconnect counter.
  (setf (%reconnect-counter connection) 0)
  (join-channels connection))

;;;; Bot commands

(defclass command-event (privmsg-event)
  ((command :initarg :command
            :initform nil
            :reader command)
   (arguments :initarg :arguments
              :initform nil
              :accessor arguments))
  (:documentation
   "Event dispatched when a PRIVMSG message is received from the
server that is prefixed by the COMMAND-CHAR associated with that
event's user's CONNECTION object. COMMAND is a keyword representing
the command, without the prefix character. ARGUMENTS is the remainder
of the message string following any trailing whitespace after the
command."))

(defmethod handle-event ((connection kichwa-connection) (event privmsg-event))
  (let ((private? (not (channel event)))
        (message (message event)))
    (format t "~&*** PRIVMSG ~@[~A: ~]<~A> ~A~%"
            (and (not private?) (name (channel event)))
            (nick (user event))
            message)
    (when (eql (char message 0) (command-char connection))
      (multiple-value-bind (command-name remaining-subseq)
          (split-sequence #\Space message
                          :start 1
                          :count 1
                          :remove-empty-subseqs t)
        (let ((command (intern (string-upcase (car command-name))
                               :keyword))
              (arguments-string (subseq message remaining-subseq)))
          (handle-event connection
                        (make-instance 'command-event
                                       :command command
                                       :arguments arguments-string
                                       :channel (channel event)
                                       :message (message event)
                                       :user (user event))))))))

(defmethod handle-event :before ((connection kichwa-connection)
                                 (event command-event))
  (format t "~&*** Got command from ~A ~S with args ~S~%"
          (or (and (channel event) (name (channel event)))
              (nick (user event)))
          (command event)
          (arguments event)))

(defmethod handle-event ((connection kichwa-connection) (event command-event))
  (handle-command connection event (command event)))

(defun /privmsg-symmetrically (connection event message)
  "Reply with a PRIVMSG to a PRIVMSG event in the manner it was received,
i.e., either on a channel or directly to the user."
  (let ((private-message? (not (channel event))))
    (/privmsg connection
              (if private-message? (user event) (channel event))
              message)))

(defgeneric handle-command (connection event command)
  (:documentation "Called by HANDLE-EVENT when the event is a
COMMAND-EVENT, so we can unpack the command and dispatch on
it. COMMAND is a keyword representing the IRC user command."))

(defmethod handle-command ((connection kichwa-connection)
                           (event command-event)
                           command)
  (/privmsg-symmetrically connection
                          event
                          (format nil
                                  "Unrecognized command \"~A\"."
                                  command)))

(defmacro defcommand (name (connection event) &body body)
  "Define a bot command named by the keyword NAME. CONNECTION and
EVENT are variables to be bound within BODY. If the first element of
BODY is a string, it is a documentation string to be retrieved by
COMMAND-DOCUMENTATION."
  ;; Keep docstring on one line for IRC
  (let* ((docstring (and (stringp (car body))
                         (substitute #\Space #\Newline (car body))))
         (body (if docstring (cdr body) body)))
    `(PROGN
       (DEFMETHOD HANDLE-COMMAND ((,connection KICHWA-CONNECTION)
                                  (,event COMMAND-EVENT)
                                  (,(gensym "COMMAND") (EQL ,name)))
         ,@body)
       (SETF (GET 'DEFCOMMAND ,name) ,docstring))))

(defun find-command (command-specifier)
  "If the user commmand named by COMMAND-SPECIFIER exists, return its
associated method, otherwise return NIL."
  (find-method #'handle-command
               '()
               `(,(find-class 'kichwa-connection)
                  ,(find-class 'command-event)
                  (EQL ,command-specifier))
               nil))

(defun remove-command (command-specifier)
  "Remove from the system the user command indicated by COMMAND-SPECIFIER"
  (remove-method #'handle-command (find-command command-specifier))
  (remprop 'defcommand command-specifier))

(defun list-commands ()
  "List all bot commands."
  (loop :for key :in (symbol-plist 'defcommand) :by #'cddr
        :collect key))

(defun command-documentation (command &optional default)
  "If the keyword COMMAND names a bot command, retrieve its
documentation string. Otherwise, return DEFAULT."
  (get 'defcommand command default))

(defcommand :help (connection event)
  "Usage: HELP [COMMAND]. If COMMAND is supplied, provide its
documentation, otherwise list defined commands."
  (let* ((prefix (command-char connection))
         (argument (car (split-sequence prefix
                                        (string-upcase (arguments event))
                                        :count 1
                                        :remove-empty-subseqs t)))
         (commands (sort (list-commands) #'string< :key #'symbol-name)))
    (/privmsg-symmetrically
     connection
     event
     (if argument
         (let* ((command (intern (string-upcase argument) :keyword))
                (doc (command-documentation command :fail)))
           (if (eq doc :fail)
               (format nil "Unrecognized command ~S." command)
               (or doc
                   (format nil "No documentation available for ~S." command))))
         (format nil "Commands:~{ ~A~}." commands)))))

;;; Misc
(defmethod /quit ((connection kichwa-connection) &optional message)
  (call-next-method connection
                    (or message "Connection reset by Peer Gynt")))


(defun start (&optional (connection *connection*))
  (connect-and-begin-processing connection))

(defun start-ssl (&optional (connection *ssl-connection*))
  (connect-and-begin-processing connection))

(defun die (&optional (connection *connection*))
  (/quit connection))

;;; 8ball (move this to its own file)

(defcommand :8ball (connection event)
  "Usage: 8BALL. Help me decide things."
  (let* ((question
          (string-trim '(#\Space #\Tab #\Linefeed #\Page #\Return)
                       (arguments event)))
         (question-length (length question))
         (messages #("Undoubtedly."
                     "Ask again tomorrow."
                     "It is certain."
                     "Without a doubt."
                     "Outlook good."
                     "My sources say no."
                     "Meh."
                     "Concentrate and ask again."
                     "Nope, you did it wrong. Concentrate harder."
                     "How should I know?"
                     "I wouldn't know."
                     "Sigh, do I look like I know?"
                     "Yes. Now leave me alone."
                     "No. Yes. No... Yyyyy-no."
                     "Maybe, but you'd better double-check with your tea leaves."
                     "In your dreams, and in mine."
                     "What do I look like, a magic eight-ball?"
                     "The subtle tingle in my Erogenous Zone indicates it is so."
                     "I'm in the mood for NO."
                     "Think positive!"
                     "Make it so."
                     "Yes, please."
                     "NEVER."
                     "You bet your sweet bippie!"
                     "Yes. Pack your travelling bag, we're going for a RIDE!"
                     "Sadly, no."
                     "Sadly, yes."
                     "Sadly, you'll never know.."
                     "Experts agree on that matter."
                     "100000 satisfied customers can't be wrong."
                     "Think of it this way: would you be any happier if you knew?"
                     "Yes, and isn't it marvelous?"
                     "Sadly, I've forgotten the question."
                     "Sadly, you will never find out."
                     "Sadly, it is a minute possibility."
                     "The spirits are silent on the matter."
                     "I'm more interested in your thoughts."
                     "Ignorance is bliss. Kichwa is here to help you achieve bliss."
                     "No blame."
                     "You may rely on it."
                     "Absolutely!!!!     *wink wink*"
                     "Signs point to yes."
                     "Uh-huh."
                     "Maybe. Who wants to know?"
                     "More or less, yeah, I suppose."
                     "It's a matter of fact."
                     "I feel a YES coming on!"
                     "Yes! Do you believe it?"
                     "You can achieve miracles, if you believe."
                     "Focus on your psychic energies on your muladhara chakra, align your wellness crystals, plug your left nostril, and chant the sacred HRROOOOOOOONG!! -- only then will you know the answer to the question."
                     "If it pleases Nuku."
                     "Good question."
                     "GREAT question."
                     "Excellent question."
                     "Stupendous question."
                     "You have the finest questions."
                     "Marvelous question."
                     "Gosh, such a good question."
                     "Have I told you lately how much I love your questions?"
                     "My latest bowel movement indicates it is not meant to be."
                     "You're not going to believe this, but yes."
                     "No, but I'm known to be a liar."
                     "You wouldn't believe me if I told you."
                     "It's a secret. Unfortunately, everybody knows but you."
                     "Lovin' these questions!"
                     "Whatever floats your boat."
                     "Only you can truly KNOW."
                     "Wouldn't surprise me!"
                     "Tremendous question."
                     "Negative."
                     "In some ways, yes; in other ways, no."
                     "Truly fine question."
                     "How much wood would a woodchuck chuck if a woodchuck would chuck wood?"
                     "Oooh, you SHAKE that 8-ball like you mean it ~"
                     "Focus your orgone on your Erogenous Zone and regress with me as we ponder this question together."
                     "No. Truly fine question."
                     ))
         (n-messages (length messages)))
    (/privmsg-symmetrically connection
                            event
                            (cond ((zerop question-length)
                                   "What, so you're just going to shake me without asking anything?")
                                  ((notany #'alpha-char-p question)
                                   "Is that moon language or what?")
                                  ((or (eql #\. (char question (1- question-length)))
                                       (eql #\! (char question (1- question-length))))
                                   "That doesn't look like a question to me.")
                                  (t (aref messages (random n-messages)))))))


;;; Local Variables:
;;; Eval: (put 'with-lossage-handling 'common-lisp-indent-function 2)
;;; End:
