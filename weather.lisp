;;; OpenwWeatherMap client

(in-package #:kichwa/weather)

(defparameter *owm-key*
  "84d1919fd1e06fbec5ec164c13adaa5d")

;;; Queries

(defclass owm-query (json-query)
  ((parameter-slots :initform '(appid) :allocation :class)
   (appid :initarg :appid :initform *owm-key*)))

(defclass weather-query (owm-query)
  ((base-url :initform "http://api.openweathermap.org/data/2.5/weather")))

(defclass forecast-query (owm-query)
  ((base-url :initform "http://api.openweathermap.org/data/2.5/forecast")))

(defclass by-city-mixin ()
  ((q :initarg :q :initform (error "Q slot is required"))))

(defclass by-zip-mixin ()
  ((zip :initarg :zip :initform (error "ZIP slot is required"))))

(defclass weather-query-by-city (weather-query by-city-mixin)
  ((parameter-slots :initform '(q appid) :allocation :class)))

(defclass weather-query-by-zip (weather-query by-zip-mixin)
  ((parameter-slots :initform '(zip appid) :allocation :class)))

;; (defclass weather-query-by-coordinates (weather-query)
;;   ((parameter-slots :initform '(lat lon) :allocation :class)
;;    (lat :initarg :lat :initform 0)
;;    (lon :initarg :lon :initform 0)))

(defclass forecast-query-by-city (forecast-query by-city-mixin)
  ((parameter-slots :initform '(q appid) :allocation :class)))

(defclass forecast-query-by-zip (forecast-query by-zip-mixin)
  ((parameter-slots :initform '(zip appid) :allocation :class)))

(defparameter *city-timezone-db-pathname*
  (merge-pathnames "geonames.sqlite"
                   (asdf:system-source-directory :kichwa)))

;;; FIXME: should fail gracefully
;; -> the two columns we care about from the local db
(defun nearest-timezone+admin1 (longitude latitude)
  (dbi:with-connection (connection :sqlite3
                                   :database-name *city-timezone-db-pathname*)
    (let* ((query (dbi:prepare
                   connection
                   "SELECT name AS NAME, MIN(ABS(?-longitude)+ABS(?-latitude)) AS DIST, admin1code AS ADMIN1CODE, timezone AS TIMEZONE FROM geonames"))
           (result (dbi:fetch (dbi:execute query longitude latitude))))
      (values (getf result :timezone)
              (getf result :admin1code)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp '+cardinal-directions+)
    (defconstant +cardinal-directions+
      (vector "N" "NNE" "NE" "ENE" "E" "ESE" "SE" "SSE"
              "S" "SSW" "SW" "WSW" "W" "WNW" "NW" "NNW"))))

(defun azimuth->cardinal-direction (azimuth)
  (svref +cardinal-directions+
         (floor (/ (mod (+ azimuth 11.25) 360) 22.5))))

(defun kelvin->celsius (k) (- k 273.14))
(defun kelvin->fahrenheit (k) (+ 32.0 (* 1.8 (kelvin->celsius k))))

(defun format-weather-query-response (stream alist)
  "Write human-readable data from an ALIST containing an
OpenWeatherMap response on STREAM."
  (flet ((lookup (key &optional (alist alist) (default "N/A"))
           (or (cdr (assoc key alist :test #'eq))
               default)))
    (let* ((coordinates-alist (lookup :coord alist nil))
           (longitude (lookup :lon coordinates-alist))
           (latitude (lookup :lat coordinates-alist))
           ;(timezone (nearest-timezone longitude latitude))
           (sky (lookup :description (car (lookup :weather alist nil))))
           (main-alist (lookup :main alist nil))
           (temperature (lookup :temp main-alist nil))
           (pressure (lookup :pressure main-alist))
           (humidity (lookup :humidity main-alist))
           (visibility (lookup :visibility))
           (wind-alist (lookup :wind alist nil))
           (wind-speed (lookup :speed wind-alist))
           (wind-direction (lookup :deg wind-alist nil))
           (clouds (lookup :all (lookup :clouds alist nil)))
           (time (lookup :dt alist nil))
           (country (lookup :country (lookup :sys alist nil)))
           (city (lookup :name)))
      (multiple-value-bind (timezone admin1)
          (nearest-timezone+admin1 longitude latitude)
        (if (null coordinates-alist)
            (format stream "City not found.")
            (format stream
                    #.(concatenate 'string
                                   "Weather in ~A,~A ~A at ~A: "
                                   "[ Temp: ~2$°C (~2$°F) ] "
                                   "[ Conditions: ~A, ~A% cloudy, "
                                   "~A% humidity, wind ~A at ~A m/s ] "
                                   "[ Visibility: ~A m ] "
                                   "[ Pressure: ~A hPa ]")
                    city
                    (if (and (stringp admin1)
                             (every #'alpha-char-p admin1))
                        (concatenate 'string " " admin1 ",")
                        "")
                    country
                    (if time
                        (format-timestring
                         nil
                         (unix-to-timestamp time)
                         :format '((:hour 2) ":" (:min 2) " (" :timezone ")")
                         :timezone (find-timezone-by-location-name timezone))

                        "N/A")
                    (if temperature (kelvin->celsius temperature) "N/A")
                    (if temperature (kelvin->fahrenheit temperature) "N/A")
                    sky
                    clouds
                    humidity
                    (if wind-direction
                        (azimuth->cardinal-direction wind-direction)
                        "N/A")
                    wind-speed
                    visibility
                    pressure))))))

;;; Commands
(defcommand :w (connection event)
  "Usage: W <city>[,code]. Print weather report for city, optionally
with ISO 3166 (two-letter) country code. Data courtesy of
OpenWeatherMap."
  (/privmsg-symmetrically
   connection
   event
   (format-weather-query-response
    nil
    (handler-case (execute (make-instance 'weather-query-by-city
                                          :q (arguments event)))
        #+nil(query 'weather-query-by-city (arguments event))
      (usocket:timeout-error (c)
        (declare (ignore c))
        (format nil
                #.(concatenate
                   'string
                   "~A: Unfortunately, your request has timed out. "
                   "Please try again in a minute or two.")
                (nick (user event))))))))

(defcommand :wz (connection event)
  "Usage: WZ <ZIP code>[,<country code>]. Print weather report for ZIP
code, optionally with ISO 3166 (two-letter) country code. If country
code is not supplied, US is default. Data courtesy of OpenWeatherMap."
  (/privmsg-symmetrically connection
                          event
                          (format-weather-query-response
                           nil
                           #+nil(query 'weather-query-by-zip (arguments event))
                           (execute (make-instance 'weather-query-by-zip
                                                   :zip (arguments event)))
                           )))


;; (defcommand :wl (connection event)
;;   "Usage: WL <latitude> <longitude>. Print weather report for given coordinates."
;;   (let ((arglist (split-sequence:split-sequence #\Space
;;                                                  (arguments event)
;;                                                  :count 2
;;                                                  :remove-empty-subseqs t)))
;;     (/privmsg-symmetrically
;;      connection
;;      event
;;      (if (< (length arglist) 2)
;;          "Wrong number of arguments. Usage: WL <latitude> <longitude>."
;;          (format-weather-query-response
;;           nil
;;           (execute (make-instance 'weather-query-by-coordinates
;;                                   :lat (first arglist)
;;                                   :lon (second arglist))))))))


;;; Someday, I'll figure out a way to succinctly summarize the forecast data ...
#|
(defun summarize-forecast-query (alist)
  (let* ((days (let ((record (assoc :list alist)))
                 (if record
                     (cdr record)
                     (return-from summarize-forecast-query nil))))
         (city (cdr (assoc :city alist)))
         (city-name (cdr (assoc :name city)))
         (coordinates (cdr (assoc :coord city)))
         (latitude (cdr (assoc :lat coordinates)))
         (longitude (cdr (assoc :lon coordinates)))
         (timezone
          (find-timezone-by-location-name
           (nearest-timezone longitude latitude)))
         (predictions
          (mapcar (lambda (day)
                    (let ((time (unix-to-timestamp (cdr (assoc :dt day))))
                          (main (cdr (assoc :main day)))
                          (weather (cdr (assoc :weather day))))
                      (let ((temp (kelvin->celsius (cdr (assoc :temp main))))
                            (weather-description
                             (cdr (assoc :description (car weather)))))
                        (list
                         (read-from-string
                          (format-timestring nil
                                             time
                                             :format '(:short-weekday)
                                             :timezone timezone))
                         (parse-integer
                          (format-timestring nil
                                             time
                                             :format '(:hour)
                                             :timezone timezone))
                         (format-timestring nil
                                            time
                                            :format '(:timezone)
                                            :timezone timezone)
                         temp
                         weather-description))))
                  days)))
    (values predictions city-name)))

(defun format-forecast-query-reponse (stream alist)
  (let* ((days (let ((record (assoc :list alist)))
                (if record
                    (cdr record)
                    (return-from format-forecast-query-reponse nil))))
         (city (cdr (assoc :city alist)))
         (city-name (cdr (assoc :name city)))
         (coordinates (cdr (assoc :coord city)))
         (latitude (cdr (assoc :lat coordinates)))
         (longitude (cdr (assoc :lon coordinates)))
         (timezone (nearest-timezone longitude latitude)))
    (format stream "Five-day forecast for ~A:~%" city-name)
    (loop :for day :in days
       :do (destructuring-bind (time main weather)
               (mapcar (lambda (key) (cdr (assoc key day)))
                       '(:dt :main :weather))
             (let ((temp-min (cdr (assoc :temp--min main)))
                   (temp-max (cdr (assoc :temp--max main)))
                   (weather-description
                    (cdr (assoc :description (car weather)))))
               (format stream
                       "[ ~A : High: ~1$°C (~1$°F) Low: ~1$°C (~1$°F) - ~A ]~%"
                       (format-timestring
                        nil
                        (unix-to-timestamp time)
                        :format '(:short-weekday " " :hour ":00")
                        :timezone (find-timezone-by-location-name timezone))
                       (kelvin->celsius temp-max)
                       (kelvin->fahrenheit temp-max)
                       (kelvin->celsius temp-min)
                       (kelvin->fahrenheit temp-min)
                       weather-description))))
    (fresh-line stream)))
|#
