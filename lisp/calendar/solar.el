;;; solar.el --- calendar functions for solar events.

;; Copyright (C) 1992, 1993, 1995 Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Keywords: calendar
;; Human-Keywords: sunrise, sunset, equinox, solstice, calendar, diary,
;;	holidays

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This collection of functions implements the features of calendar.el,
;; diary.el, and holiday.el that deal with times of day, sunrise/sunset, and
;; eqinoxes/solstices.

;; Based on the ``Almanac for Computers 1984,'' prepared by the Nautical
;; Almanac Office, United States Naval Observatory, Washington, 1984, on
;; ``Astronomical Formulae for Calculators,'' 3rd ed., by Jean Meeus,
;; Willmann-Bell, Inc., 1985, and on ``Astronomical Algorithms'' by Jean
;; Meeus, Willmann-Bell, Inc., 1991.

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
  (error "Solar/lunar calculations impossible since floating point is unavailable."))

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
  "*Latitude of `calendar-location-name' in degrees.

The value can be either a decimal fraction (one place of accuracy is
sufficient), + north, - south, such as 40.7 for New York City, or the value
can be a vector [degrees minutes north/south] such as [40 50 north] for New
York City.

This variable should be set in site-local.el.")

;;;###autoload
(defvar calendar-longitude nil
  "*Longitude of `calendar-location-name' in degrees.

The value can be either a decimal fraction (one place of accuracy is
sufficient), + east, - west, such as -73.9 for New York City, or the value
can be a vector [degrees minutes east/west] such as [73 55 west] for New
York City.

This variable should be set in site-local.el.")

(defsubst calendar-latitude ()
  "Convert calendar-latitude to a signed decimal fraction, if needed."
  (if (numberp calendar-latitude)
      calendar-latitude
    (let ((lat (+ (aref calendar-latitude 0)
                  (/ (aref calendar-latitude 1) 60.0))))
      (if (equal (aref calendar-latitude 2) 'north)
          lat
        (- lat)))))

(defsubst calendar-longitude ()
  "Convert calendar-longitude to a signed decimal fraction, if needed."
  (if (numberp calendar-longitude)
      calendar-longitude
    (let ((long (+ (aref calendar-longitude 0)
                  (/ (aref calendar-longitude 1) 60.0))))
      (if (equal (aref calendar-longitude 2) 'east)
          long
        (- long)))))

;;;###autoload
(defvar calendar-location-name
  '(let ((float-output-format "%.1f"))
     (format "%s%s, %s%s"
             (if (numberp calendar-latitude)
                 (abs calendar-latitude)
               (+ (aref calendar-latitude 0)
                  (/ (aref calendar-latitude 1) 60.0)))
             (if (numberp calendar-latitude)
                 (if (> calendar-latitude 0) "N" "S")
               (if (equal (aref calendar-latitude 2) 'north) "N" "S"))
             (if (numberp calendar-longitude)
                 (abs calendar-longitude)
               (+ (aref calendar-longitude 0)
                  (/ (aref calendar-longitude 1) 60.0)))
             (if (numberp calendar-longitude)
                 (if (> calendar-longitude 0) "E" "W")
               (if (equal (aref calendar-longitude 2) 'east) "E" "W"))))
  "*Expression evaluating to name of `calendar-longitude', calendar-latitude'.
For example, \"New York City\".  Default value is just the latitude, longitude
pair.

This variable should be set in site-local.el.")

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

(defsubst solar-sin-degrees (x)
  (sin (degrees-to-radians (mod x 360.0))))

(defsubst solar-cosine-degrees (x)
  (cos (degrees-to-radians (mod x 360.0))))

(defsubst solar-tangent-degrees (x)
  (tan (degrees-to-radians (mod x 360.0))))

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

(defsubst solar-degrees-to-hours (deg)
  (/ deg 15.0))

(defsubst solar-hours-to-days (hour)
  (/ hour 24.0))

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
  "Calculates the *standard* time of sunrise for Gregorian DATE.
Calculation is for location given by `calendar-latitude' and
`calendar-longitude'.

Returns a decimal fraction of hours.  Returns nil if the sun does not rise at
that location on that day."
  (let* ((day-of-year (calendar-day-number date))
	 (approx-sunrise
          (+ day-of-year
             (solar-hours-to-days
              (-  6 (solar-degrees-to-hours (calendar-longitude))))))
	 (solar-longitude-of-sun-at-sunrise
          (solar-longitude-of-sun approx-sunrise))
	 (solar-right-ascension-at-sunrise
          (solar-right-ascension solar-longitude-of-sun-at-sunrise))
	 (solar-declination-at-sunrise
          (solar-declination solar-longitude-of-sun-at-sunrise))
	 (cos-local-sunrise
          (/ (- (solar-cosine-degrees (+ 90 (/ 50.0 60.0)))
                (* (solar-sin-degrees solar-declination-at-sunrise)
                   (solar-sin-degrees (calendar-latitude))))
             (* (solar-cosine-degrees solar-declination-at-sunrise)
                (solar-cosine-degrees (calendar-latitude))))))
    (if (<= (abs cos-local-sunrise) 1);; otherwise, no sunrise that day
      (let* ((local-sunrise (solar-degrees-to-hours
                             (- 360 (solar-arccos cos-local-sunrise))))
             (local-mean-sunrise
	      (mod (- (+ local-sunrise solar-right-ascension-at-sunrise)
		      (+ (* 0.065710 approx-sunrise)
			 6.622))
		   24)))
	(+ (- local-mean-sunrise (solar-degrees-to-hours (calendar-longitude)))
	   (/ calendar-time-zone 60.0))))))

(defun solar-sunset (date)
  "Calculates the *standard* time of sunset for Gregorian DATE.
Calculation is for location given by `calendar-latitude' and
`calendar-longitude'.

Returns a decimal fractions of hours.  Returns nil if the sun does not set at
that location on that day."
  (let* ((day-of-year (calendar-day-number date))
	 (approx-sunset
          (+ day-of-year
             (solar-hours-to-days
              (- 18 (solar-degrees-to-hours (calendar-longitude))))))
	 (solar-longitude-of-sun-at-sunset
          (solar-longitude-of-sun approx-sunset))
	 (solar-right-ascension-at-sunset
          (solar-right-ascension solar-longitude-of-sun-at-sunset))
	 (solar-declination-at-sunset
          (solar-declination solar-longitude-of-sun-at-sunset))
	 (cos-local-sunset
          (/ (- (solar-cosine-degrees (+ 90 (/ 50.0 60.0)))
                (* (solar-sin-degrees solar-declination-at-sunset)
                   (solar-sin-degrees (calendar-latitude))))
             (* (solar-cosine-degrees solar-declination-at-sunset)
                (solar-cosine-degrees (calendar-latitude))))))
    (if (<= (abs cos-local-sunset) 1);; otherwise, no sunset that day
      (let* ((local-sunset (solar-degrees-to-hours
                            (solar-arccos cos-local-sunset)))
             (local-mean-sunset
	      (mod (- (+ local-sunset solar-right-ascension-at-sunset)
		      (+ (* 0.065710 approx-sunset) 6.622))
		   24)))
	(+ (- local-mean-sunset (solar-degrees-to-hours (calendar-longitude)))
	   (/ calendar-time-zone 60.0))))))

(defun solar-time-string (time time-zone)
  "Printable form for decimal fraction TIME in TIME-ZONE.
Format used is given by `calendar-time-display-form'."
  (let* ((time (round (* 60 time)))
	 (24-hours (/ time 60))
	 (minutes (format "%02d" (% time 60)))
	 (12-hours (format "%d" (1+ (% (+ 24-hours 11) 12))))
	 (am-pm (if (>= 24-hours 12) "pm" "am"))
	 (24-hours (format "%02d" 24-hours)))
    (mapconcat 'eval calendar-time-display-form "")))

(defun solar-sunrise-sunset (date)
  "String giving local times of sunrise and sunset on Gregorian DATE."
  (let* ((rise (solar-sunrise date))
         (adj-rise (if rise (dst-adjust-time date rise)))
         (set (solar-sunset date))
         (adj-set (if set (dst-adjust-time date set))))
    (format "%s, %s at %s"
	    (if (and rise (calendar-date-equal date (car adj-rise)))
		(concat "Sunrise " (apply 'solar-time-string (cdr adj-rise)))
	      "No sunrise")
	    (if (and set (calendar-date-equal date (car adj-set)))
		(concat "sunset " (apply 'solar-time-string (cdr adj-set)))
	      "no sunset")
	    (eval calendar-location-name))))

(defun solar-date-next-longitude (d l)
  "First moment on or after Julian day number D when sun's longitude is a
multiple of L degrees at calendar-location-name with that location's
local time (including any daylight savings rules).

L must be an integer divisor of 360.

Result is in local time expressed astronomical (Julian) day numbers.

The values of calendar-daylight-savings-starts,
calendar-daylight-savings-starts-time, calendar-daylight-savings-ends,
calendar-daylight-savings-ends-time, calendar-daylight-time-offset, and
calendar-time-zone are used to interpret local time."
  (let* ((long)
         (start d)
         (start-long (solar-longitude d))
         (next (mod (* l (1+ (floor (/ start-long l)))) 360))
         (end (+ d (* (/ l 360.0) 400)))
         (end-long (solar-longitude end)))
    (while                 ;; bisection search for nearest minute
        (< 0.00001 (- end start))
      ;; start   <= d    < end
      ;; start-long <= next < end-long when next != 0
      ;; when next = 0, we look for the discontinuity (start-long is near 360
      ;;                and end-long is small (less than l).
      (setq d (/ (+ start end) 2.0))
      (setq long (solar-longitude d))
      (if (or (and (/= next 0) (< long next))
              (and (= next 0) (< l long)))
          (progn
            (setq start d)
            (setq start-long long))
        (setq end d)
        (setq end-long long)))
    (/ (+ start end) 2.0)))

(defun solar-longitude (d)
  "Longitude of sun on astronomical (Julian) day number D.
Accuracy is about 0.01 degree (about 365.25*24*60*0.01/360 = 15 minutes).

The values of calendar-daylight-savings-starts,
calendar-daylight-savings-starts-time, calendar-daylight-savings-ends,
calendar-daylight-savings-ends-time, calendar-daylight-time-offset, and
calendar-time-zone are used to interpret local time."
  (let* ((a-d (calendar-absolute-from-astro d))
         (date (calendar-gregorian-from-absolute (floor a-d)))
         (time (* 24 (- a-d (truncate a-d))))
	 (rounded-abs-date (+ (calendar-absolute-from-gregorian date)
			      (/ (round (* 60 time)) 60.0 24.0)))
         ;; get local standard time
	 (a-d (- rounded-abs-date
               (if (dst-in-effect rounded-abs-date)
                   (/ calendar-daylight-time-offset 24.0 60.0) 0)))
         ;; get Universal Time
         (a-d (- a-d (/ calendar-time-zone 60.0 24.0)))
         (date (calendar-astro-from-absolute a-d))
         ;; get Ephemeris Time
         (date (+ date (solar-ephemeris-correction
                        (extract-calendar-year
                         (calendar-gregorian-from-absolute
                          (floor
                           (calendar-absolute-from-astro
                            date)))))))
         (T (/ (- date 2451545.0) 36525.0))
	 (Lo (mod (+ 280.46645 (* 36000.76983 T) (* 0.0003032 T T)) 360.0))
	 (M (mod (+ 357.52910
                    (* 35999.05030 T)
                    (* -0.0001559 T T)
                    (* -0.00000048 T T T))
                 360.0))
	 (e (+ 0.016708617 (* -0.000042037 T) (* -0.0000001236 T T)))
	 (C (+ (* (+ 1.914600 (* -0.004817 T) (* -0.000014 T T))
		  (solar-sin-degrees M))
	       (* (+ 0.019993 (* -0.000101 T)) (solar-sin-degrees (* 2 M)))
	       (* 0.000290 (solar-sin-degrees (* 3 M)))))
	 (true-longitude (+ Lo C))
	 (omega (+ 125.04 (* -1934.136 T)))
	 (apparent-longitude (mod
			      (+ true-longitude
				 -0.00569
				 (* -0.00478 (solar-sin-degrees omega)))
                              360.0)))
    apparent-longitude))

(defun solar-ephemeris-correction (year)
  "Ephemeris time minus Universal Time at astronomical (Julian) day D.
Result is in days For the years 1800-1987, the maximum error is 1.9 seconds.
For the other years, the maximum error is about 30 seconds."
  (cond ((and (<= 1988 year) (< year 2020))
         (/ (+ year -2000 67.0) 60.0 60.0 24.0))
        ((and (<= 1900 year) (< year 1988))
         (let* ((theta (/ (- (calendar-astro-from-absolute
                              (calendar-absolute-from-gregorian
                               (list 7 1 year)))
                             (calendar-astro-from-absolute
                              (calendar-absolute-from-gregorian
                               '(1 1 1900))))
                          36525.0))
                (theta2 (* theta theta))
                (theta3 (* theta2 theta))
                (theta4 (* theta2 theta2))
                (theta5 (* theta3 theta2)))
           (+ -0.00002
              (* 0.000297 theta)
              (* 0.025184 theta2)
              (* -0.181133 theta3)
              (* 0.553040 theta4)
              (* -0.861938 theta5)
              (* 0.677066 theta3 theta3)
              (* -0.212591 theta4 theta3))))
        ((and (<= 1800 year) (< year 1900))
         (let* ((theta (/ (- (calendar-astro-from-absolute
                              (calendar-absolute-from-gregorian
                               (list 7 1 year)))
                             (calendar-astro-from-absolute
                              (calendar-absolute-from-gregorian
                               '(1 1 1900))))
                          36525.0))
                (theta2 (* theta theta))
                (theta3 (* theta2 theta))
                (theta4 (* theta2 theta2))
                (theta5 (* theta3 theta2)))
           (+ -0.000009
              (* 0.003844 theta)
              (* 0.083563 theta2)
              (* 0.865736 theta3)
              (* 4.867575 theta4)
              (* 15.845535 theta5)
              (* 31.332267 theta3 theta3)
              (* 38.291999 theta4 theta3)
              (* 28.316289 theta4 theta4)
              (* 11.636204 theta4 theta5)
              (* 2.043794 theta5 theta5))))
        ((and (<= 1620 year) (< year 1800))
         (let ((x (/ (- year 1600) 10.0)))
           (/ (+ (* 2.19167 x x) (* -40.675 x) 196.58333) 60.0 60.0 24.0)))
        (t (let* ((tmp (- (calendar-astro-from-absolute
                           (calendar-absolute-from-gregorian
                            (list 1 1 year)))
                          2382148))
                  (second (- (/ (* tmp tmp) 41048480.0) 15)))
             (/ second 60.0 60.0 24.0)))))

;;;###autoload
(defun sunrise-sunset (&optional arg)
  "Local time of sunrise and sunset for today.  Accurate to +/- 2 minutes.
If called with an optional prefix argument, prompt for date.

If called with an optional double prefix argument, prompt for longitude,
latitude, time zone, and date, and always use standard time.

This function is suitable for execution in a .emacs file."
 (interactive "p")
 (or arg (setq arg 1))
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
                     (if (numberp calendar-latitude)
                         (abs calendar-latitude)
                       (+ (aref calendar-latitude 0)
                          (/ (aref calendar-latitude 1) 60.0)))
                     (if (numberp calendar-latitude)
                         (if (> calendar-latitude 0) "N" "S")
                       (if (equal (aref calendar-latitude 2) 'north) "N" "S"))
                     (if (numberp calendar-longitude)
                         (abs calendar-longitude)
                       (+ (aref calendar-longitude 0)
                          (/ (aref calendar-longitude 1) 60.0)))
                     (if (numberp calendar-longitude)
                         (if (> calendar-longitude 0) "E" "W")
                       (if (equal (aref calendar-longitude 2) 'east)
                           "E" "W"))))))
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
  (let ((date (calendar-cursor-to-date t)))
    (message "%s: %s"
             (calendar-date-string date t t)
             (solar-sunrise-sunset date))))

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
	     (light (if sunset
                        (dst-adjust-time
                         date
                         (- sunset (/ 18.0 60.0))))))
        (if (and light (calendar-date-equal date (car light)))
            (format "%s Sabbath candle lighting"
                    (apply 'solar-time-string (cdr light)))))))

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
	   (d (solar-date-next-longitude
               (calendar-astro-from-absolute
                (calendar-absolute-from-gregorian
                 (list (+ 3 (* k 3)) 15 y)))
               90))
           (abs-day (calendar-absolute-from-astro d)))
      (list
       (list (calendar-gregorian-from-absolute (floor abs-day))
             (format "%s %s"
                     (nth k (if (and calendar-latitude
                                     (< (calendar-latitude) 0))
                                solar-s-hemi-seasons
                              solar-n-hemi-seasons))
                     (solar-time-string
                      (* 24 (- abs-day (floor abs-day)))
                      (if (dst-in-effect abs-day)
                          calendar-daylight-time-zone-name
                        calendar-standard-time-zone-name))))))))


(provide 'solar)

;;; solar.el ends here
