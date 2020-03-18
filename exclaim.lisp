(in-package #:kichwa/exclaim)

(defparameter *exclamations*
  (list "Great googly moogly!"
        "Hot dog!"
        "Holy cow!"
        "Holy mackerel!"
        "Great Scott!"
        "Great Caesar's ghost!"
        "Judas priest!"
        "Jesus H. Christ on a bike!"
        "Jumpin' Jehova!"
        "Jumpin' Jehoshaphat!"
        "Holy Dinah!"
        "Hokey doodle!"
        "Crikey!"
        "Inconceivable!"
        "Gee willikers!"
        "Great galloping guppies!"
        "Goodness gracious, great balls of fire!"
        "Holy cow!"
        "Holy smoke!"
        "Jesus, Mary, and Joseph!"
        "Shiver me timbers!"
        "Well, I'll be a monkey's uncle!"
        "I declare!"
        "Gadzooks!"
        "Hot diggity dog!"
        "Yowzer!"
        "Zowie!"
        "Good night!"
        "Gee whiz!"
        "I'll be jiggered!"
        "I'll be damned!"
        "Jeepers!"
        "Hot damn!"
        "Hell's bells!"
        "Egads!"
        "Ay-ay-ay!"))

(defparameter *zippy-quotes-path* #P"/home/dsk/lisp-projects/kichwa/pinhead.txt")

(defcommand :exclaim (connection event)
  "Usage: EXCLAIM. Interject an expression of surprise."
  (/privmsg-symmetrically connection
                          event
                          (nth (random (length *exclamations*))
                               *exclamations*)))

(defun random-zippy-quote ()
  (handler-case (with-open-file (s *zippy-quotes-path*)
                  ;; There are 764 quotes, SAYETH THE LORD
                  (let ((quote-num (random 764)))
                    (do ((q (read-line s) (read-line s nil :eof))
                         (i 0 (1+ i)))
                        ((or (eq q :eof) (= i quote-num)) q))))
    (file-error (c)
      (declare (ignore c))
      "Zippy quotes not found :(")))

(defcommand :yow (connection event)
  "Usage: YOW. Provide a random Zippy the Pinhead quote."
  (/privmsg-symmetrically connection
                          event
                          (random-zippy-quote)))
