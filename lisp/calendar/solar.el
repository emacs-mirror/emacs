;;; solar.el --- calendar functions for solar events.

;; Copyright (C) 1992 Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Keywords: calendar
;; Human-Keywords: sunrise, sunset, equinox, solstice, calendar, diary,
;;	holidays

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;; Commentary:

;; This collection of functions implements the features of calendar.el and
;; diary.el that deal with times of day, sunrise/sunset, and
;; eqinoxes/solstices.

;; Based on the ``Almanac for Computers 1984,'' prepared by the Nautical
;; Almanac Office, United States Naval Observatory, Washington, 1984 and
;; on ``Astronomical Formulae for Calculators,'' 3rd ed., by Jean Meeus,
;; Willmann-Bell, Inc., 1985.
;;
;; WARNINGS:
;;    1. SUNRISE/SUNSET calculations will be accurate only to +/- 2 minutes.
;;       Locations should be between +/- 65 degrees of latitude.
;;       Dates should be in the latter half of the 20th century.
;;
;;    2. Equinox/solstice times will be accurate only to +/- 15 minutes.

;; The author would be delighted to have an astronomically more sophisticated
;; person rewrite the code for the solar calculations in this file!

;; Comments, corrections, and improvements should be sent to
;;  Edward M. Reingold               Department of Computer Science
;;  (217) 333-6733                   University of Illinois at Urbana-Champaign
;;  reingold@cs.uiuc.edu             1304 West Springfield Avenue
;;                                   Urbana, Illinois 61801

;;; Code:

(if (fboundp 'atan)
    (require 'lisp-float-type)
  (error "Solar calculations impossible since floating point is unavailable."))

(require 'cal-dst)

;;;###autoload
(defvar calendar-time-display-form
  '(12-hours ":" minutes am-pm
    (if time-zone " (") time-zone (if time-zone ")"))
  "*The pseudo-pattern that governs the way a time of day is formatted.

A pseudo-pattern is a list of expressions that can involve the keywords
`12-hours', `24-hours', and `minutes',  all numbers in string form,
and `am-pm' and `time-zone',  both alphabetic strings.

For example, the form

  '(24-hours \":\" minutes
    (if time-zone \" (\") time-zone (if time-zone \")\"))

would give military-style times like `21:07 (UTC)'.")

;;;###autoload
(defvar calendar-latitude nil
  "*Latitude of `calendar-location-name' in degrees, + north, - south.
For example, 40.7 for New York City.
It may not be a good idea to set this in advance for your site;
if there may be users running Emacs at your site
who are physically located elsewhere, they would get the wrong
value and might not know how to override it.")

;;;###autoload
(defvar calendar-longitude nil
  "*Longitude of `calendar-location-name' in degrees, + east, - west.
For example, -74.0 for New York City.
It may not be a good idea to set this in advance for your site;
if there may be users running Emacs at your site
who are physically located elsewhere, they would get the wrong
value and might not know how to override it.")

;;;###autoload
(defvar calendar-location-name
  '(let ((float-output-format "%.1f"))
     (format "%s%s, %s%s"
	     (abs calendar-latitude)
	     (if (> calendar-latitude 0) "N" "S")
	     (abs calendar-longitude)
	     (if (> calendar-longitude 0) "E" "W")))
  "*Expression evaluating to name of `calendar-longitude', calendar-latitude'.
Default value is just the latitude, longitude pair.")

(defvar solar-n-hemi-seasons
  '("Vernal Equinox" "Summer Solstice" "Autumnal Equinox" "Winter Solstice")
  "List of season changes for the northern hemisphere.")

(defvar solar-s-hemi-seasons
  '("Autumnal Equinox" "Winter Solstice" "Vernal Equinox" "Summer Solstice")
  "List of season changes for the southern hemisphere.")

(defun solar-setup ()
  "Prompt user for latitude, longitude, and time zone."
  (beep)
  (if (not calendar-longitude)
      (setq calendar-longitude
            (solar-get-number
             "Enter longitude (decimal fraction; + east, - west): ")))
  (if (not calendar-latitude)
      (setq calendar-latitude
            (solar-get-number
             "Enter latitude (decimal fraction; + north, - south): ")))
  (if (not calendar-time-zone)
      (setq calendar-time-zone
            (solar-get-number
             "Enter difference from Coordinated Universal Time (in minutes): "))))

(defun solar-get-number (prompt)
  "Return a number from the minibuffer, prompting with PROMPT.
Returns nil if nothing was entered."
  (let ((x (read-string prompt "")))
    (if (not (string-equal x ""))
        (string-to-int x))))

(defun solar-sin-degrees (x)
  (sin (degrees-to-radians x)))

(defun solar-cosine-degrees (x)
  (cos (degrees-to-radians x)))

(defun solar-tangent-degrees (x)
  (tan (degrees-to-radians x)))

(defun solar-xy-to-quadrant (x y)
  "Determines the quadrant of the point X, Y."
  (if (> x 0)
      (if (> y 0) 1 4)
      (if (> y 0) 2 3)))

(defun solar-degrees-to-quadrant (angle)
  "Determines the quadrant of ANGLE."
  (1+ (floor (mod angle 360) 90)))

(defun solar-arctan (x quad)
  "Arctangent of X in quadrant QUAD."
  (let ((deg (radians-to-degrees (atan x))))
    (cond ((equal quad 2)   (+ deg 180))
	  ((equal quad 3)   (+ deg 180))
	  ((equal quad 4)   (+ deg 360))
	  (t                deg))))

(defun solar-arccos (x)
  (let ((y (sqrt (- 1 (* x x)))))
    (solar-arctan (/ y x) (solar-xy-to-quadrant x y))))

(defun solar-arcsin (y)
  (let ((x (sqrt (- 1 (* y y)))))
    (solar-arctan (/ y x) (solar-xy-to-quadrant x y))))

(defconst solar-earth-inclination 23.441884 
  "Inclination of earth's equator to its solar orbit in degrees.")

(defconst solar-cos-inclination (solar-cosine-degrees solar-earth-inclination) 
  "Cosine of earth's inclination.")

(defconst solar-sin-inclination (solar-sin-degrees solar-earth-inclination)
  "Sine of earth's inclination.")

(defconst solar-earth-orbit-eccentricity 0.016718
  "Eccentricity of orbit of the earth around the sun.")

(defmacro solar-degrees-to-hours (deg)
  (list '/ deg 15))

(defmacro solar-hours-to-days (hour)
  (list '/ hour 24))

(defun solar-longitude-of-sun (day)
  "Longitude of the sun at DAY in the year."
  (let ((mean-anomaly (- (* 0.9856 day) 3.289)))
    (mod (+ mean-anomaly 
	    (* 1.916 (solar-sin-degrees mean-anomaly))
	    (* 0.020 (solar-sin-degrees (* 2 mean-anomaly)))
	    282.634)
	 360)))

(defun solar-right-ascension (longitude)
  "Right ascension of the sun, given its LONGITUDE."
  (solar-degrees-to-hours
   (solar-arctan
    (* solar-cos-inclination (solar-tangent-degrees longitude))
    (solar-degrees-to-quadrant longitude))))

(defun solar-declination (longitude)
  "Declination of the sun, given its LONGITUDE."
  (solar-arcsin
   (* solar-sin-inclination
      (solar-sin-degrees longitude))))

(defun solar-sunrise (date)
  "Calculates the *standard* time of sunrise for Gregorian DATE for location
given by `calendar-latitude' and `calendar-longitude'.  Returns a decimal fraction
of hours.  Returns nil if the sun does not rise at that location on that day."
  (let* ((day-of-year (calendar-day-number date))
	 (approx-sunrise
          (+ day-of-year
             (solar-hours-to-days
              (-  6 (solar-degrees-to-hours calendar-longitude)))))
	 (solar-longitude-of-sun-at-sunrise
          (solar-longitude-of-sun approx-sunrise))
	 (solar-right-ascension-at-sunrise
          (solar-right-ascension solar-longitude-of-sun-at-sunrise))
	 (solar-declination-at-sunrise
          (solar-declination solar-longitude-of-sun-at-sunrise))
	 (cos-local-sunrise
          (/ (- (solar-cosine-degrees (+ 90 (/ 50.0 60.0)))
                (* (solar-sin-degrees solar-declination-at-sunrise)
                   (solar-sin-degrees calendar-latitude)))
             (* (solar-cosine-degrees solar-declination-at-sunrise)
                (solar-cosine-degrees calendar-latitude)))))
    (if (<= (abs cos-local-sunrise) 1);; otherwise, no sunrise that day
      (let* ((local-sunrise (solar-degrees-to-hours
                             (- 360 (solar-arccos cos-local-sunrise))))
             (local-mean-sunrise
	      (mod (- (+ local-sunrise solar-right-ascension-at-sunrise)
		      (+ (* 0.065710 approx-sunrise)
			 6.622))
		   24)))
	(+ (- local-mean-sunrise (solar-degrees-to-hours calendar-longitude))
	   (/ calendar-time-zone 60.0))))))

(defun solar-sunset (date)
  "Calculates the *standard* time of sunset for Gregorian DATE for location
given by `calendar-latitude' and `calendar-longitude'.  Returns a decimal fractions
of hours.  Returns nil if the sun does not set at that location on that day."
  (let* ((day-of-year (calendar-day-number date))
	 (approx-sunset
          (+ day-of-year
             (solar-hours-to-days
              (- 18 (solar-degrees-to-hours calendar-longitude)))))
	 (solar-longitude-of-sun-at-sunset
          (solar-longitude-of-sun approx-sunset))
	 (solar-right-ascension-at-sunset
          (solar-right-ascension solar-longitude-of-sun-at-sunset))
	 (solar-declination-at-sunset
          (solar-declination solar-longitude-of-sun-at-sunset))
	 (cos-local-sunset
          (/ (- (solar-cosine-degrees (+ 90 (/ 50.0 60.0)))
                (* (solar-sin-degrees solar-declination-at-sunset)
                   (solar-sin-degrees calendar-latitude)))
             (* (solar-cosine-degrees solar-declination-at-sunset)
                (solar-cosine-degrees calendar-latitude)))))
    (if (<= (abs cos-local-sunset) 1);; otherwise, no sunset that day
      (let* ((local-sunset (solar-degrees-to-hours
                            (solar-arccos cos-local-sunset)))
             (local-mean-sunset
	      (mod (- (+ local-sunset solar-right-ascension-at-sunset)
		      (+ (* 0.065710 approx-sunset) 6.622))
		   24)))
	(+ (- local-mean-sunset (solar-degrees-to-hours calendar-longitude))
	   (/ calendar-time-zone 60.0))))))

(defun solar-time-string (time date &optional style)
  "Printable form for decimal fraction *standard* TIME on DATE.
Optional parameter STYLE forces the time to be standard time when its value
is 'standard and daylight savings time (if available) when its value is
'daylight.

Format used is given by `calendar-time-display-form'.  Converted to daylight
savings time according to `calendar-daylight-savings-starts',
`calendar-daylight-savings-ends', `calendar-daylight-savings-starts-time',
`calendar-daylight-savings-ends-time', and `calendar-daylight-savings-offset'."
  (let* ((year (extract-calendar-year date))
	 (time (round (* 60 time)))
	 (rounded-abs-date (+ (calendar-absolute-from-gregorian date)
			      (/ time 60.0 24.0)))
         (dst-starts (and calendar-daylight-savings-starts
                          (+ (calendar-absolute-from-gregorian
                              (eval calendar-daylight-savings-starts))
			     (/ calendar-daylight-savings-starts-time
				60.0 24.0))))
         (dst-ends (and calendar-daylight-savings-ends
                        (+ (calendar-absolute-from-gregorian
                            (eval calendar-daylight-savings-ends))
			   (/ (- calendar-daylight-savings-ends-time
				 calendar-daylight-time-offset)
			      60.0 24.0))))
	 (dst (and (not (eq style 'standard))
                   (or (eq style 'daylight)
                       (and dst-starts dst-ends
                            (or (and (< dst-starts dst-ends);; northern hemi.
                                     (<= dst-starts rounded-abs-date)
                                     (< rounded-abs-date dst-ends))
                                (and (< dst-ends dst-starts);; southern hemi.
                                     (or (< rounded-abs-date dst-ends)
                                         (<= dst-starts rounded-abs-date)))))
                       (and dst-starts (not dst-ends)
                            (<= dst-starts rounded-abs-date))
                       (and dst-ends (not dst-starts)
                            (< rounded-abs-date dst-ends)))))
	 (time-zone (if dst
			calendar-daylight-time-zone-name
			calendar-standard-time-zone-name))
	 (time (+ time (if dst calendar-daylight-time-offset 0)))
	 (24-hours (/ time 60))
	 (minutes (format "%02d" (% time 60)))
	 (12-hours (format "%d" (1+ (% (+ 24-hours 11) 12))))
	 (am-pm (if (>= 24-hours 12) "pm" "am"))
	 (24-hours (format "%02d" 24-hours)))
    (mapconcat 'eval calendar-time-display-form "")))

(defun solar-sunrise-sunset (date)
  "String giving local times of sunrise and sunset on Gregorian DATE."
  (let ((rise (solar-sunrise date))
	(set (solar-sunset date)))
    (format "%s, %s at %s"
	    (if rise
		(concat "Sunrise " (solar-time-string rise date))
	      "No sunrise")
	    (if set
		(concat "sunset " (solar-time-string set date))
	      "no sunset")
	    (eval calendar-location-name))))

(defun solar-apparent-longitude-of-sun (date)
  "Apparent longitude of the sun on Gregorian DATE."
  (let* ((time (/ (- (calendar-absolute-from-gregorian date) 
		     (calendar-absolute-from-gregorian '(1 0.5 1900)))
		  36525))
	 (l (+ 279.69668
	       (* 36000.76892 time)
	       (* 0.0003025 time time)))
	 (m (+ 358.47583
	       (* 35999.04975 time)
	       (* -0.000150 time time)
	       (* -0.0000033 time time time)))
	 (c (+ (* (+ 1.919460
		     (* -0.004789 time)
		     (* -0.000014 time time))
		  (solar-sin-degrees m))
	       (* (+ 0.020094
		     (* -0.000100 time))
		  (solar-sin-degrees (* 2 m)))
	       (* 0.000293
		  (solar-sin-degrees (* 3 m)))))
	 (L (+ l c))
	 (omega (+ 259.18
		   (* -1934.142 time)))
	 (app (+ L
		 -0.00569
		 (* -0.00479
		    (solar-sin-degrees omega)))))
    app))

(defun solar-ephemeris-correction (year)
  "Difference in minutes between Ephemeris time and UTC in YEAR.
Value is only an approximation."
  (let ((T (/ (- year 1900) 100.0)))
    (+ 0.41 (* 1.2053 T) (* 0.4992 T T))))

(defun solar-equinoxes/solstices (k year)
  "Date of equinox/solstice K for YEAR.  K=0, spring equinox; K=1, summer
solstice; K=2, fall equinox; K=3, winter solstice.  Accurate to within
several minutes."
  (let ((date (list (+ 3 (* k 3)) 21 year))
        app
	(correction 1000))
    (while (> correction 0.00001)
      (setq app (mod (solar-apparent-longitude-of-sun date) 360))
      (setq correction (* 58 (solar-sin-degrees (- (* k 90) app))))
      (setq date (list (extract-calendar-month date)
		       (+ (extract-calendar-day date) correction)
		       year)))
    (list (extract-calendar-month date)
          (+ (extract-calendar-day date) (/ calendar-time-zone 60.0 24.0)
             (- (/ (solar-ephemeris-correction year) 60.0 24.0)))
          year)))

;;;###autoload
(defun sunrise-sunset (&optional arg)
  "Local time of sunrise and sunset for today.  Accurate to +/- 2 minutes.
If called with an optional prefix argument, prompts for date.

If called with an optional double prefix argument, prompts for longitude,
latitude, time zone, and date.

This function is suitable for execution in a .emacs file."
 (interactive "p")
 (if (and (< arg 16)
          (not (and calendar-latitude calendar-longitude calendar-time-zone)))
     (solar-setup))
 (let* ((calendar-longitude
         (if (< arg 16) calendar-longitude
           (solar-get-number
            "Enter longitude (decimal fraction; + east, - west): ")))
        (calendar-latitude
         (if (< arg 16) calendar-latitude
           (solar-get-number
            "Enter latitude (decimal fraction; + north, - south): ")))
        (calendar-time-zone
         (if (< arg 16) calendar-time-zone
           (solar-get-number
            "Enter difference from Coordinated Universal Time (in minutes): ")))
        (calendar-location-name
         (if (< arg 16) calendar-location-name
           (let ((float-output-format "%.1f"))
             (format "%s%s, %s%s"
                     (abs calendar-latitude)
                     (if (> calendar-latitude 0) "N" "S")
                     (abs calendar-longitude)
                     (if (> calendar-longitude 0) "E" "W")))))
        (calendar-standard-time-zone-name
         (if (< arg 16) calendar-standard-time-zone-name
           (cond ((= calendar-time-zone 0) "UTC")
                 ((< calendar-time-zone 0)
                     (format "UTC%dmin" calendar-time-zone))
                 (t  (format "UTC+%dmin" calendar-time-zone)))))
        (calendar-daylight-savings-starts
         (if (< arg 16) calendar-daylight-savings-starts))
        (calendar-daylight-savings-ends
         (if (< arg 16) calendar-daylight-savings-ends))
        (date (if (< arg 4) (calendar-current-date) (calendar-read-date)))
        (date-string (calendar-date-string date t))
        (time-string (solar-sunrise-sunset date))
        (msg (format "%s: %s" date-string time-string))
        (one-window (one-window-p t)))
   (if (<= (length msg) (frame-width))
       (message msg)
     (with-output-to-temp-buffer "*temp*"
       (princ (concat date-string "\n" time-string)))
     (message (substitute-command-keys
               (if one-window
                   (if pop-up-windows
                       "Type \\[delete-other-windows] to remove temp window."
                     "Type \\[switch-to-buffer] RET to remove temp window.")
                 "Type \\[switch-to-buffer-other-window] RET to restore old contents of temp window."))))))
 
(defun calendar-sunrise-sunset ()
  "Local time of sunrise and sunset for date under cursor.
Accurate to +/- 2 minutes."
  (interactive)
  (if (not (and calendar-latitude calendar-longitude calendar-time-zone))
      (solar-setup))
  (message
   (solar-sunrise-sunset
    (or (calendar-cursor-to-date)
	(error "Cursor is not on a date!")))))

(defun diary-sunrise-sunset ()
  "Local time of sunrise and sunset as a diary entry.
Accurate to +/- 2 minutes."
  (if (not (and calendar-latitude calendar-longitude calendar-time-zone))
      (solar-setup))
  (solar-sunrise-sunset date))

(defun diary-sabbath-candles ()
  "Local time of candle lighting diary entry--applies if date is a Friday.
No diary entry if there is no sunset on that date."
  (if (not (and calendar-latitude calendar-longitude calendar-time-zone))
      (solar-setup))
  (if (= (% (calendar-absolute-from-gregorian date) 7) 5);;  Friday
      (let* ((sunset (solar-sunset date))
	     (light (if sunset (- sunset (/ 18.0 60.0)))))
        (if light (format "%s Sabbath candle lighting"
                          (solar-time-string light date))))))

;;;###autoload
(defun solar-equinoxes-solstices ()
  "Date and time of equinoxes and solstices, if visible in the calendar window.
Requires floating point."
  (let ((m displayed-month)
        (y displayed-year))
    (increment-calendar-month m y (cond ((= 1 (% m 3)) -1)
					((= 2 (% m 3))  1)
					(t              0)))
    (let* ((calendar-standard-time-zone-name
            (if calendar-time-zone calendar-standard-time-zone-name "UTC"))
           (calendar-daylight-savings-starts
            (if calendar-time-zone calendar-daylight-savings-starts))
           (calendar-daylight-savings-ends
            (if calendar-time-zone calendar-daylight-savings-ends))
           (calendar-time-zone (if calendar-time-zone calendar-time-zone 0))
           (k (1- (/ m 3)))
	   (date (solar-equinoxes/solstices k y))
	   (day (extract-calendar-day date))
	   (time (* 24 (- day (truncate day))))
	   (s-hemi (and calendar-latitude (< calendar-latitude 0)))
           ;; Time zone/DST can't move the date out of range,
           ;; so let solar-time-string do the conversion.
	   (date (list (extract-calendar-month date)
		       (truncate day)
		       (extract-calendar-year date))))
      (list (list date
		  (format "%s %s"
			  (nth k (if s-hemi solar-s-hemi-seasons
                                   solar-n-hemi-seasons))
			  (solar-time-string time date)))))))

(provide 'solar)

;;; solar.el ends here
