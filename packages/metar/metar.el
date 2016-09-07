;;; metar.el --- Retrieve and decode METAR weather information

;; Copyright (C) 2007, 2014-2016  Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@delysid.org>
;; Version: 0.2
;; Package-Requires: ((cl-lib "0.5"))
;; Keywords: comm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Run `M-x metar RET' to get a simple weather report from weather.noaa.gov.
;; The value of `calendar-latitude' and `calendar-longitude' will be used to
;; automatically determine a nearby station.  If these variables are not set,
;; you will be prompted to enter the location manually.
;;
;; With `C-u M-x metar RET', country and station name need to be entered.
;; `C-u C-u M-x metar RET' will prompt for the METAR station code (4 letters).
;;
;; Customize `metar-units' to change length, speed, temperature or pressure
;; units to your liking.
;;
;; For programmatic access to decoded weather reports, use:
;;
;;   (metar-decode (metar-get-record "CODE"))

;;; Code:

(require 'calc-units)
(require 'cl-lib)
(require 'format-spec)
(require 'rx)
(require 'solar)
(require 'url)

(defgroup metar ()
  "METAR weather reports."
  :group 'net-utils)

(defcustom metar-units '((length . m)
			 (pressure . hPa)
			 (speed . kph)
			 (temperature . degC))
  "Default measurement units to use when reporting weather information."
  :group 'metar
  :type '(list (cons :format "%v"
		     (const :tag "Length: " length)
		     (choice (const :tag "Meter" m)
			     (const :tag "Inch" in)
			     (const :tag "Foot" ft)
			     (const :tag "Yard" yd)
			     (const :tag "Mile" mi)))
	       (cons :format "%v"
		     (const :tag "Pressure:" pressure)
		     (choice (const :tag "Pascal" Pa)
			     (const :tag "Hecto pascal" hPa)
			     (const :tag "Bar" bar)
			     (const :tag "Inch of mercury" inHg)
			     (const :tag "Standard atmosphere" atm)
			     (const :tag "Meter of mercury" mHg)
			     (const :tag "Punds per square inch" psi)))
	       (cons :format "%v"
		     (const :tag "Speed:" speed)
		     (choice (const :tag "Kilometers per hour" kph)
			     (const :tag "Miles per hour" mph)
			     (const :tag "Knot" knot)))
	       (cons :format "%v"
		     (const :tag "Temperature:" temperature)
		     (choice (const :tag "Degree Celsius" degC)
			     (const :tag "Degree Kelvin" degK)
			     (const :tag "Degree Fahrenheit" degF)))))

(defcustom metar-stations-info-url "http://weather.noaa.gov/data/nsd_bbsss.txt"
  "URL to use for retrieving station meta information."
  :group 'metar
  :type 'string)

(defvar metar-stations nil
  "Variable containing (cached) METAR station information.
Use the function `metar-stations' to get the actual station list.")

(defun metar-station-convert-dms-to-deg (string)
  "Convert degrees, minutes and optional seconds, to degrees."
  (when (string-match (rx string-start
			  (group (1+ digit)) ?- (group (1+ digit))
			  (optional ?- (group (1+ digit)))
			  (group (char ?N ?E ?S ?W))
			  string-end) string)
    (funcall (if (memq (aref (match-string 4 string) 0) '(?N ?E)) #'+ #'-)
	     (+ (string-to-number (match-string 1 string))
		(/ (string-to-number (match-string 2 string)) 60.0)
		(if (match-string 3 string)
		    (/ (string-to-number (match-string 3 string)) 3600.0)
		  0)))))

(defun metar-stations ()
  "Retrieve a list of METAR stations.
Results are cached in variable `metar-stations'.
If this variable is nil, the information is retrieved from the Internet."
  (or metar-stations
      (let ((data (with-temp-buffer
		    (url-insert-file-contents metar-stations-info-url)
		    (mapcar (lambda (entry)
			      (split-string entry ";"))
			    (split-string (buffer-string) "\n")))))
	(setq metar-stations nil)
	(while data
	  (when (and (nth 7 (car data)) (nth 8 (car data))
		     (not (string= (nth 2 (car data)) "----")))
	    (setq metar-stations
		  (append
		   (let ((item (car data)))
		     (list
		      (list (cons 'code (nth 2 item))
			    (cons 'name (nth 3 item))
			    (cons 'country (nth 5 item))
			    (cons 'latitude
				  (metar-station-convert-dms-to-deg (nth 7 item)))
			    (cons 'longitude
				  (metar-station-convert-dms-to-deg (nth 8 item)))
			    (cons 'altitude (string-to-number (nth 12 item))))))
		   metar-stations)))
	  (setq data (cdr data)))
	;; (unless metar-timer
	;;   (setq metar-timer
	;; 	(run-with-timer 600 nil (lambda () (setq metar-stations nil)))))
	metar-stations)))

(defun metar-stations-get (station-code key)
  "Get meta information for station with STATION-CODE and KEY.
KEY can be one of the symbols `code', `name', `country', `latitude',
`longitude' or `altitude'."
  (let ((stations (metar-stations)) result)
    (while stations
      (when (string= (cdr (assoc 'code (car stations))) station-code)
	(setq result (cdr (assoc key (car stations)))
	      stations nil))
      (setq stations (cdr stations)))
    result))

(defun metar-latitude-longitude-bearing (latitude1 longitude1
					 latitude2 longitude2)
  "Calculate bearing from start point LATITUDE1/LONGITUDE1 to end point
LATITUDE2/LONGITUDE2."
  (% (+ 360
	(truncate
	 (radians-to-degrees
	  (atan (* (sin (degrees-to-radians (- longitude2 longitude1)))
		   (cos (degrees-to-radians latitude2)))
		(- (* (cos (degrees-to-radians latitude1))
		      (sin (degrees-to-radians latitude2)))
		   (* (sin (degrees-to-radians latitude1))
		      (cos (degrees-to-radians latitude2))
		      (cos (degrees-to-radians (- longitude2 longitude1)))))))))
     360))

(defun metar-latitude-longitude-distance-haversine (latitude1 longitude1
						    latitude2 longitude2)
  "Caluclate the distance (in kilometers) between two points on the
surface of the earth given as LATITUDE1, LONGITUDE1, LATITUDE2 and LONGITUDE2."
  (cl-macrolet ((distance (d1 d2)
			  `(expt (sin (/ (degrees-to-radians (- ,d2 ,d1)) 2)) 2)))
    (let ((a (+ (distance latitude1 latitude2)
		(* (cos (degrees-to-radians latitude1))
		   (cos (degrees-to-radians latitude2))
		   (distance longitude1 longitude2)))))
      (* 6371 (* 2 (atan (sqrt a) (sqrt (- 1 a))))))))

(defun metar-find-station-by-latitude/longitude (latitude longitude &optional
							  radius)
  "Find a station near the coordinates given by LATITUDE and LONGITUDE.
Returns a cons where car is the station code and cdr is the distance in
kilometers.
If RADIUS is non-nil, only stations within this range (in kilometers) are
considered.
If no match if found, nil is returned."
  (interactive
   (list
    (solar-get-number "Enter latitude (decimal fraction; + north, - south): ")
    (solar-get-number "Enter longitude (decimal fraction; + east, - west): ")))
  (let ((stations (metar-stations))
	(best-distance (or radius 10000))
	(station-code nil))
    (while stations
      (let ((station-latitude (cdr (assoc 'latitude (car stations))))
	    (station-longitude (cdr (assoc 'longitude (car stations)))))
	(when (and station-latitude station-longitude)
	  (let ((distance (metar-latitude-longitude-distance-haversine
			   latitude longitude
			   station-latitude station-longitude)))
	    (when (< distance best-distance)
	      (setq best-distance distance
		    station-code (cdr (assoc 'code (car stations))))))))
      (setq stations (cdr stations)))
    (if (called-interactively-p 'interactive)
	(if station-code
	    (message "%s, %s (%s) at %s is %d km away from %s."
		     (metar-stations-get station-code 'name)
		     (metar-stations-get station-code 'country)
		     station-code
		     (let ((float-output-format "%.1f"))
		       (format "%s%s, %s%s"
			       (abs (metar-stations-get station-code 'latitude))
			       (if (> (metar-stations-get station-code 'latitude) 0) "N" "S")
			       (abs (metar-stations-get station-code 'longitude))
			       (if (> (metar-stations-get station-code 'longitude) 0) "E" "W")))
		     best-distance
		     (let ((float-output-format "%.1f"))
		       (format "%s%s, %s%s"
			       (if (numberp latitude)
				   (abs latitude)
				 (+ (aref latitude 0)
				    (/ (aref latitude 1) 60.0)))
			       (if (numberp latitude)
				   (if (> latitude 0) "N" "S")
				 (if (equal (aref latitude 2) 'north) "N" "S"))
			       (if (numberp longitude)
				   (abs longitude)
				 (+ (aref longitude 0)
				    (/ (aref longitude 1) 60.0)))
			       (if (numberp longitude)
				   (if (> longitude 0) "E" "W")
				 (if (equal (aref longitude 2) 'east)
				     "E" "W")))))
	  (message "No appropriate station found."))
      (when station-code
	(cons station-code (round best-distance))))))

(defun metar-convert-unit (value new-unit &optional convert-units-function)
  "Convert VALUE to NEW-UNIT.
VALUE is a string with the value followed by the unit, like \"5 knot\"
and NEW-UNIT should be a unit name like \"kph\" or similar.
CONVERT-UNITS-FUNCTION designates the function actually doing the conversion.
It must have the signature of `math-convert-units', which is the default."
  (cl-check-type value string)
  (unless (symbolp new-unit)
    (setq new-unit (intern new-unit)))
  (let ((expr (math-simplify (math-read-expr value))))
    ;; Sneakily work around bug#19582.
    (when (eq (car-safe expr) 'neg)
      (setq expr `(* -1 ,(cadr expr))))
    (cl-assert (or (math-zerop expr)
		   (not (memq (math-single-units-in-expr-p expr) '(nil wrong))))
	       nil
	       "Metar: Not exactly one unit in expression: %S" expr)
    (let ((res (math-simplify-units
		(funcall (or convert-units-function 'math-convert-units)
			 expr
			 (math-build-var-name new-unit)
			 t))))
      (cl-assert (math-realp res) nil
		 "Metar: Not a Calc real number: %S" res)
      (cons (string-to-number (math-format-value (if (integerp res)
						     res
						   (math-float res))))
	    new-unit))))

(defun metar-convert-temperature (string &optional unit)
  (metar-convert-unit (concat (if (= (aref string 0) ?M)
				  (concat "-" (substring string 1))
				string)
			      "degC")
		      (or unit (cdr (assq 'temperature metar-units)))
		      (lambda (expr new-unit-var pure)
			(math-convert-temperature expr
						  (math-build-var-name 'degC)
						  new-unit-var
						  pure))))

(defcustom metar-url
  "http://weather.noaa.gov/pub/data/observations/metar/stations/%s.TXT"
  "URL used to fetch station specific information.
%s is replaced with the 4 letter station code."
  :group 'metar
  :type 'string)

(defun metar-url (station)
  (format metar-url
	  (upcase (cl-etypecase station
		    (string station)
		    (symbol (symbol-name station))))))

(defconst metar-record-regexp
  (rx (group (1+ digit)) ?/ (group (1+ digit)) ?/ (group (1+ digit))
      space
      (group (1+ digit)) ?: (group (1+ digit))
      ?\n
      (group "%s" (* not-newline)))
  "Regular expression used to extract METAR information from `metar-url'.
%s is replaced with the station code which always has to be present in a METAR
record.")

(defun metar-get-record (station)
  "Retrieve a METAR/SPECI record for STATION from the Internet.
Return a cons where `car' is the time of the measurement (as an emacs-lsip
time value) and `cdr' is a string containing the actual METAR code.
If no record was found for STATION, nil is returned."
  (with-temp-buffer
    (url-insert-file-contents (metar-url station))
    (when (re-search-forward (format metar-record-regexp station) nil t)
      (cons (encode-time
	     0
	     (string-to-number (match-string 5))
	     (string-to-number (match-string 4))
	     (string-to-number (match-string 3))
	     (string-to-number (match-string 2))
	     (string-to-number (match-string 1))
	     0)
	    (match-string 6)))))

(defconst metar-could-regexp
  (rx symbol-start
      (group (or "FEW" "SCT" "BKN" "OVC"))
      (group (= 3 digit))
      (optional (group (or "TCU" "CB")))
      symbol-end)
  "Regular expression to match cloud information in METAR records.")

(defun metar-clouds (info)
  (let ((clouds ())
	(from 0))
    (while (string-match metar-could-regexp info from)
      (setq from (match-end 0)
	    clouds (push (append (list (match-string 1 info)
				       (metar-convert-unit
					(concat (match-string 2 info) " ft")
					(cdr (assq 'length metar-units))))
				 (when (match-string 3 info)
				   (list (match-string 3 info))))
			 clouds)))
    clouds))

(defconst metar-phenomena '(("BC" . "patches")
			    ("BL" . "blowing")
			    ("BR" . "mist")
			    ("DR" . "drifting")
			    ("DS" . "dust storm")
			    ("DU" . "widespread dust")
			    ("DZ" . "drizzle")
			    ("FC" . "funnel cloud")
			    ("FG" . "fog")
			    ("FU" . "smoke")
			    ("FZ" . "freezing")
			    ("GR" . "hail")
			    ("GS" . "small hail/snow pellets")
			    ("HZ" . "haze")
			    ("IC" . "ice crystals")
			    ("MI" . "shallow")
			    ("PL" . "ice pellets")
			    ("PO" . "well developed dust/sand swirls")
			    ("PR" . "partials")
			    ("PY" . "spray")
			    ("RA" . "rain")
			    ("SA" . "sand")
			    ("SG" . "snow grains")
			    ("SH" . "showers")
			    ("SN" . "snow")
			    ("SQ" . "squall")
			    ("SS" . "sand storm")
			    ("TS" . "thunderstorm")
			    ("VA" . "volcanic ash")
			    ("VC" . "vicinity"))
  "Alist of codes and descriptions for METAR weather phenomenoa.")

(defconst metar-phenomena-regexp
  (eval `(rx symbol-start
	     (group (optional (char ?+ ?-)))
	     (group (1+ (or ,@(mapcar #'car metar-phenomena))))
	     symbol-end))
  "Regular expression to match weather phenomena in METAR records.")

(defun metar-phenomena (info)
  (when (string-match metar-phenomena-regexp info)
    (let ((words ()))
      (when (string= (match-string 1 info) "-")
	(push "light" words))
      (let ((obs (match-string 2 info)))
	(while (> (length obs) 0)
	  (setq words (nconc words
			     (list (cdr (assoc-string (substring obs 0 2)
						      metar-phenomena))))
		obs (substring obs 2))))
      (mapconcat #'identity words " "))))

(defconst metar-wind-regexp
  (rx symbol-start
      (group (or "VRB" (= 3 digit)))
      (group (repeat 2 3 digit)) (optional (char ?G) (group (1+ digit)))
      "KT"
      symbol-end
      (optional (one-or-more not-newline)
		symbol-start
		(group (= 3 digit)) (char ?V) (group (= 3 digit))
		symbol-end))
  "Regular expression to match wind information in METAR records.")

(defsubst metar-degrees (value)
  (cons value 'degrees))

(defun metar-wind (info)
  (when (string-match metar-wind-regexp info)
    (append
     (if (string= (match-string 1 info) "VRB")
	 (when (and (match-string 4 info) (match-string 5 info))
	   (list :from (string-to-number (match-string 4 info))
		 :to (string-to-number (match-string 5 info))))
       (append
	(list :direction (metar-degrees
			  (string-to-number (match-string 1 info))))
	(when (and (match-string 4 info) (match-string 5 info))
	  (list :from (metar-degrees (string-to-number (match-string 4 info)))
		:to (metar-degrees (string-to-number (match-string 5 info)))))))
     (list :speed (metar-convert-unit (concat (match-string 2 info) "knot")
				      (cdr (assq 'speed metar-units))))
     (when (match-string 3 info)
       (list :gust (metar-convert-unit (concat (match-string 3 info) "knot")
				       (cdr (assq 'speed metar-units))))))))

(defconst metar-visibility-regexp
  (rx symbol-start (group (1+ digit)) (optional (group "SM")) symbol-end)
  "Regular expression to match information about visibility in METAR records.")

(defconst metar-temperature-and-dewpoint-regexp
  (rx symbol-start
      (group (group (optional (char ?M))) (1+ digit))
      (char ?/)
      (group (group (optional (char ?M))) (1+ digit))
      symbol-end)
  "Regular expression to match temperature and dewpoint information in METAR
records.")

(defun metar-temperature (info)
  (when (string-match metar-temperature-and-dewpoint-regexp info)
    (metar-convert-temperature (match-string 1 info))))

(defun metar-dewpoint (info)
  (when (string-match metar-temperature-and-dewpoint-regexp info)
    (metar-convert-temperature (match-string 3 info))))

(defun metar-humidity (info)
  (when (string-match metar-temperature-and-dewpoint-regexp info)
    (cons (round
	   (metar-magnus-formula-humidity-from-dewpoint
	    (save-match-data (car (metar-convert-temperature
				   (match-string 1 info) 'degC)))
	    (car (metar-convert-temperature (match-string 3 info) 'degC))))
	  'percent)))

(defconst metar-pressure-regexp
  (rx symbol-start (group (char ?Q ?A)) (group (1+ digit)) symbol-end)
  "Regular expression to match air pressure information in METAR records.")

(defun metar-pressure (info)
  (when (string-match metar-pressure-regexp info)
    (metar-convert-unit
     (concat (match-string 2 info)
	     (cond
	      ((string= (match-string 1 info) "Q") "hPa")
	      ((string= (match-string 1 info) "A") "cinHg")))
     (cdr (assq 'pressure metar-units)))))

(defun metar-decode (record)
  "Return a lisp structure describing the weather information in RECORD."
  (when record
    (let* ((codes (cdr record))
	   (temperature (metar-temperature codes))
	   (dewpoint (metar-dewpoint codes))
	   (humidity (metar-humidity codes))
	   (pressure (metar-pressure codes))
	   (wind (metar-wind codes)))
      (append
       (list (cons 'station (car (split-string codes " ")))
	     (cons 'timestamp (car record))
	     (cons 'wind wind)
	     (cons 'temperature temperature)
	     (cons 'dewpoint dewpoint)
	     (cons 'humidity humidity)
	     (cons 'pressure pressure))
       (when (metar-phenomena codes)
	 (list (cons 'phenomena (metar-phenomena codes))))))))

(defun metar-magnus-formula-humidity-from-dewpoint (temperature dewpoint)
  "Calculate relative humidity (in %) from TEMPERATURE and DEWPOINT (in
degrees celsius)."
  (* 10000
     (expt 10
	   (- (/ (- (* 0.4343
		       (+ 243.12 temperature)
		       (/ (* dewpoint 17.62)
			  (+ 243.12 dewpoint)))
		    (* 0.4343 17.62 temperature))
		 (+ 243.12 temperature))
	      2))))

;;;###autoload
(defun metar (&optional arg)
  "Display recent weather information.
If a prefix argument is given, prompt for country and station name.
If two prefix arguments are given, prompt for exact station code.
Otherwise, determine the best station via latitude/longitude."
  (interactive "p")
  (unless arg (setq arg 1))
  (let (station)
    (cond
     ((= arg 1)
      (unless calendar-longitude
	(setq calendar-longitude
	      (solar-get-number
	       "Enter longitude (decimal fraction; + east, - west): ")))
      (unless calendar-latitude
	(setq calendar-latitude
	      (solar-get-number
	     "Enter latitude (decimal fraction; + north, - south): ")))
      (when (and calendar-latitude calendar-longitude
		 (setq station (metar-find-station-by-latitude/longitude
				(calendar-latitude) (calendar-longitude))))
	(message "Found %s %d kilometers away." (car station) (cdr station))
	(setq station (car station))))
     ((= arg 4)
      (let* ((country (completing-read "Country: " (metar-station-countries) nil t))
	     (name (completing-read "Station name: " (mapcar (lambda (s) (cdr (assq 'name s)))
							     (metar-stations-in-country country))
				    nil t)))
	(setq station (cdr (assq 'code (cl-find-if (lambda (s)
						     (and (string= name (cdr (assq 'name s)))
							  (string= country (cdr (assq 'country s)))))
						   (metar-stations)))))))
     ((= arg 16)
      (setq station (completing-read "Enter METAR station code: "
				     (mapcar (lambda (station-info)
					       (cdr (assq 'code station-info)))
					     (metar-stations))
				     nil t))))
    (let ((info (metar-decode (metar-get-record station))))
      (if info
	  (message "%d minutes ago at %s: %d°%c, %s%d%% humidity, %.1f %S."
		   (/ (truncate (float-time (time-since
					     (cdr (assoc 'timestamp info)))))
		      60)
		   (or (metar-stations-get (cdr (assoc 'station info)) 'name)
		       (cdr (assoc 'station info)))
		   (cadr (assoc 'temperature info))
		   (cond
		    ((eq (cdr (assq 'temperature metar-units)) 'degC) ?C)
		    ((eq (cdr (assq 'temperature metar-units)) 'degF) ?F))
		   (if (assoc 'phenomena info)
		       (concat (cdr (assoc 'phenomena info)) ", ")
		     "")
		   (cadr (assoc 'humidity info))
		   (cadr (assoc 'pressure info)) (cddr (assoc 'pressure info)))
	(message "No weather information found, sorry.")))))
  
(defun metar-station-countries ()
  (let (countries)
    (dolist (station (metar-stations))
      (let ((country (cdr (assq 'country station))))
	(cl-pushnew country countries :test #'equal)))
    countries))

(defun metar-stations-in-country (country)
  (cl-loop for station-info in (metar-stations)
	   when (string= country (cdr (assq 'country station-info)))
	   collect station-info))

(defun metar-average-temperature (country)
  "Display average temperature from all stations in COUNTRY."
  (interactive
   (list (completing-read "Country: " (metar-station-countries) nil t)))
  (let ((count 0) (temp-sum 0)
	(stations (metar-stations))
	(url-show-status nil)
	(progress (make-progress-reporter
		   "Downloading METAR records..."
		   0
		   (cl-count-if (lambda (station)
				  (string= (cdr (assoc 'country station))
					   country))
				(metar-stations)))))
    (while stations
      (when (string= (cdr (assoc 'country (car stations))) country)
	(let ((temp (cdr (assoc 'temperature
				(metar-decode
				 (metar-get-record
				  (cdr (assoc 'code (car stations)))))))))
	  (when temp
	    (setq temp-sum (+ temp-sum temp)
		  count (+ count 1))
	    (progress-reporter-update progress count))))
      (setq stations (cdr stations)))
    (progress-reporter-done progress)
    (if (called-interactively-p 'interactive)
	(message "Average temperature in %s is %s"
		 country
		 (if (> count 0)
		     (format "%.1f°C (%d stations)"
			     (/ (float temp-sum) count)
			     count)
		   "unknown"))
      (when (> count 0)
	(/ (float temp-sum) count)))))

(defun metar-format (format report)
  (format-spec
   format
   (list (cons ?d
	       (let ((dewpoint (cdr (assq 'dewpoint report))))
		 (format "%.1f°%c"
			 (car dewpoint)
			 (cond ((eq (cdr dewpoint) 'degC) ?C)
			       ((eq (cdr dewpoint) 'degF) ?F)
			       ((eq (cdr dewpoint) 'degK) ?K)))))
	 (cons ?h
	       (let ((humidity (cdr (assq 'humidity report))))
		 (format "%d%%" (car humidity))))
	 (cons ?p
	       (let ((pressure (cdr (assq 'pressure report))))
		 (format "%.1f %S" (car pressure) (cdr pressure))))
	 (cons ?s (cdr (assq 'station report)))
	 (cons ?t
	       (let ((temperature (cdr (assq 'temperature report))))
		 (format "%.1f°%c"
			 (car temperature)
			 (cond ((eq (cdr temperature) 'degC) ?C)
			       ((eq (cdr temperature) 'degF) ?F))))))))

(provide 'metar)
;;; metar.el ends here
