;;; solar.el --- calendar functions for solar events

;; Copyright (C) 1992, 1993, 1995, 1997 Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;;	Denis B. Roegel <Denis.Roegel@loria.fr>
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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This collection of functions implements the features of calendar.el,
;; diary.el, and holiday.el that deal with times of day, sunrise/sunset, and
;; equinoxes/solstices.

;; Based on the ``Almanac for Computers 1984,'' prepared by the Nautical
;; Almanac Office, United States Naval Observatory, Washington, 1984, on
;; ``Astronomical Formulae for Calculators,'' 3rd ed., by Jean Meeus,
;; Willmann-Bell, Inc., 1985, on ``Astronomical Algorithms'' by Jean Meeus,
;; Willmann-Bell, Inc., 1991, and on ``Planetary Programs and Tables from
;; -4000 to +2800'' by Pierre Bretagnon and Jean-Louis Simon, Willmann-Bell,
;; Inc., 1986.

;;
;; Accuracy:
;;    1. Sunrise/sunset times will be accurate to the minute for years
;;       1951--2050.  For other years the times will be within +/- 2 minutes.
;;
;;    2. Equinox/solstice times will be accurate to the minute for years
;;       1951--2050.  For other years the times will be within +/- 1 minute.

;; Technical details of all the calendrical calculations can be found in
;; ``Calendrical Calculations'' by Nachum Dershowitz and Edward M. Reingold,
;; Cambridge University Press (1997).

;; Comments, corrections, and improvements should be sent to
;;  Edward M. Reingold               Department of Computer Science
;;  (217) 333-6733                   University of Illinois at Urbana-Champaign
;;  reingold@cs.uiuc.edu             1304 West Springfield Avenue
;;                                   Urbana, Illinois 61801

;;; Code:

(if (fboundp 'atan)
    (require 'lisp-float-type)
  (error "Solar/lunar calculations impossible since floating point is unavailable"))

(require 'cal-dst)
(require 'cal-julian)

;;;###autoload
(defcustom calendar-time-display-form
  '(12-hours ":" minutes am-pm
    (if time-zone " (") time-zone (if time-zone ")"))
  "*The pseudo-pattern that governs the way a time of day is formatted.

A pseudo-pattern is a list of expressions that can involve the keywords
`12-hours', `24-hours', and `minutes',  all numbers in string form,
and `am-pm' and `time-zone',  both alphabetic strings.

For example, the form

  '(24-hours \":\" minutes
    (if time-zone \" (\") time-zone (if time-zone \")\"))

would give military-style times like `21:07 (UTC)'."
  :type 'sexp
  :group 'calendar)

;;;###autoload
(defcustom calendar-latitude nil
  "*Latitude of `calendar-location-name' in degrees.

The value can be either a decimal fraction (one place of accuracy is
sufficient), + north, - south, such as 40.7 for New York City, or the value
can be a vector [degrees minutes north/south] such as [40 50 north] for New
York City.

This variable should be set in `site-start'.el."
  :type '(choice (const nil)
		 (number :tag "Exact")
		 (vector :value [0 0 north]
			 (integer :tag "Degrees")
			 (integer :tag "Minutes")
			 (choice :tag "Position"
				 (const north)
				 (const south))))
  :group 'calendar)

;;;###autoload
(defcustom calendar-longitude nil
  "*Longitude of `calendar-location-name' in degrees.

The value can be either a decimal fraction (one place of accuracy is
sufficient), + east, - west, such as -73.9 for New York City, or the value
can be a vector [degrees minutes east/west] such as [73 55 west] for New
York City.

This variable should be set in `site-start'.el."
  :type '(choice (const nil)
		 (number :tag "Exact")
		 (vector :value [0 0 west]
			 (integer :tag "Degrees")
			 (integer :tag "Minutes")
			 (choice :tag "Position"
				 (const east)
				 (const west))))
  :group 'calendar)

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
(defcustom calendar-location-name
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
  "*Expression evaluating to name of `calendar-longitude', `calendar-latitude'.
For example, \"New York City\".  Default value is just the latitude, longitude
pair.

This variable should be set in `site-start'.el."
  :type 'sexp
  :group 'calendar)

(defcustom solar-error 0.5
"*Tolerance (in minutes) for sunrise/sunset calculations.

A larger value makes the calculations for sunrise/sunset faster, but less
accurate.  The default is half a minute (30 seconds), so that sunrise/sunset
times will be correct to the minute.

It is useless to set the value smaller than 4*delta, where delta is the
accuracy in the longitude of the sun (given by the function
`solar-ecliptic-coordinates') in degrees since (delta/360) x (86400/60) = 4 x
delta.  At present, delta = 0.01 degrees, so the value of the variable
`solar-error' should be at least 0.04 minutes (about 2.5 seconds)."
  :type 'number
  :group 'calendar)

(defvar solar-n-hemi-seasons
  '("Vernal Equinox" "Summer Solstice" "Autumnal Equinox" "Winter Solstice")
  "List of season changes for the northern hemisphere.")

(defvar solar-s-hemi-seasons
  '("Autumnal Equinox" "Winter Solstice" "Vernal Equinox" "Summer Solstice")
  "List of season changes for the southern hemisphere.")

(defvar solar-sidereal-time-greenwich-midnight 
   nil 
   "Sidereal time at Greenwich at midnight (universal time).")

(defvar solar-spring-or-summer-season nil  
  "T if spring or summer and nil otherwise.
Needed for polar areas, in order to know whether the day lasts 0 or 24 hours.")

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

;; The condition-case stuff is needed to catch bogus arithmetic
;; exceptions that occur on some machines (like Sparcs)
(defun solar-sin-degrees (x)
  (condition-case nil
      (sin (degrees-to-radians (mod x 360.0)))
    (solar-sin-degrees x)))
(defun solar-cosine-degrees (x)
   (condition-case nil
       (cos (degrees-to-radians (mod x 360.0)))
     (solar-cosine-degrees x)))
(defun solar-tangent-degrees (x)
  (condition-case nil
      (tan (degrees-to-radians (mod x 360.0)))
    (solar-tangent-degrees x)))
      
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

(defun solar-atn2 (x y)
   "Arctan of point X, Y."
   (if (= x 0)
       (if (> y 0) 90 270)
     (solar-arctan (/ y x) x)))

(defun solar-arccos (x)
     "Arcos of X."
     (let ((y (sqrt (- 1 (* x x)))))
       (solar-atn2 x y)))

(defun solar-arcsin (y)
     "Arcsin of Y."
     (let ((x (sqrt (- 1 (* y y)))))
       (solar-atn2 x y)
       ))

(defsubst solar-degrees-to-hours (degrees)
  "Convert DEGREES to hours."
  (/ degrees 15.0))

(defsubst solar-hours-to-days (hour)
  "Convert HOUR to decimal fraction of a day."
  (/ hour 24.0))

(defun solar-right-ascension (longitude obliquity)
  "Right ascension of the sun, in hours, given LONGITUDE and OBLIQUITY.
Both arguments are in degrees."
  (solar-degrees-to-hours
   (solar-arctan
    (* (solar-cosine-degrees obliquity) (solar-tangent-degrees longitude))
    (solar-degrees-to-quadrant longitude))))

(defun solar-declination (longitude obliquity)
  "Declination of the sun, in degrees, given LONGITUDE and OBLIQUITY.
Both arguments are in degrees."
  (solar-arcsin
   (* (solar-sin-degrees obliquity)
      (solar-sin-degrees longitude))))

(defun solar-sunrise-and-sunset (time latitude longitude)
  "Sunrise, sunset and length of day. 
Parameters are the midday TIME and the LATITUDE, LONGITUDE of the location.

TIME is a pair with the first component being the number of Julian centuries
elapsed at 0 Universal Time, and the second component being the universal
time.  For instance, the pair corresponding to November 28, 1995 at 16 UT is
\(-0.040945 16), -0.040945 being the number of julian centuries elapsed between
Jan 1, 2000 at 12 UT and November 28, 1995 at 0 UT.

Coordinates are included because this function is called with latitude=10
degrees to find out if polar regions have 24 hours of sun or only night."
  (let* ((rise-time (solar-moment -1 latitude longitude time))
         (set-time (solar-moment 1 latitude longitude time))
         (day-length))
    (if (not (and rise-time set-time))
        (if (or (and (> latitude 0) solar-spring-or-summer-season)
                (and (< latitude 0) (not solar-spring-or-summer-season)))
          (setq day-length 24)
          (setq day-length 0))
        (setq day-length (- set-time rise-time)))
    (list (if rise-time (+ rise-time (/ calendar-time-zone 60.0)) nil)
          (if set-time (+ set-time (/ calendar-time-zone 60.0)) nil)
          day-length)))

(defun solar-moment (direction latitude longitude time)
  "Sunrise/sunset at location.
Sunrise if DIRECTION =-1 or sunset if =1 at LATITUDE, LONGITUDE, with midday
being TIME.

TIME is a pair with the first component being the number of Julian centuries
elapsed at 0 Universal Time, and the second component being the universal
time.  For instance, the pair corresponding to November 28, 1995 at 16 UT is
\(-0.040945 16), -0.040945 being the number of julian centuries elapsed between
Jan 1, 2000 at 12 UT and November 28, 1995 at 0 UT.

Uses binary search."
  (let* ((ut (car (cdr time)))
         (possible 1) ; we assume that rise or set are possible
         (utmin (+ ut (* direction 12.0))) 
         (utmax ut)    ; the time searched is between utmin and utmax
            ; utmin and utmax are in hours
         (utmoment-old 0.0)    ; rise or set approximation
         (utmoment 1.0) ; rise or set approximation
         (hut 0)         ; sun height at utmoment
         (t0 (car time))
         (hmin (car (cdr 
               (solar-horizontal-coordinates (list t0 utmin) 
                                                latitude longitude t))))
         (hmax (car (cdr 
               (solar-horizontal-coordinates (list t0 utmax) 
                                                latitude longitude t)))))
       ; -0.61 degrees is the height of the middle of the sun, when it rises
       ;   or sets.
     (if (< hmin -0.61) 
              (if (> hmax -0.61)
                  (while ;(< i 20) ; we perform a simple dichotomy
                         ; (> (abs (+ hut 0.61)) epsilon)
                         (>= (abs (- utmoment utmoment-old))
                             (/ solar-error 60))
                    (setq utmoment-old utmoment)
                    (setq utmoment (/ (+ utmin utmax) 2))
                    (setq hut (car (cdr 
                                    (solar-horizontal-coordinates 
                                   (list t0 utmoment) latitude longitude t))))
                    (if (< hut -0.61) (setq utmin utmoment))
                    (if (> hut -0.61) (setq utmax utmoment))
                   )
                (setq possible 0)) ; the sun never rises
                (setq possible 0)) ; the sun never sets
     (if (equal possible 0) nil utmoment)))

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


(defun solar-daylight (time)
  "Printable form for time expressed in hours."
  (format "%d:%02d"
          (floor time)
          (floor (* 60 (- time (floor time))))))

(defun solar-exact-local-noon (date)
  "Date and Universal Time of local noon at *local date* date. 

The date may be different from the one asked for, but it will be the right
local date.  The second component of date should be an integer."
  (let* ((nd date)
         (ut (- 12.0 (/ (calendar-longitude) 15)))
         (te (solar-time-equation date ut)))
    (setq ut (- ut te))
    (if (>= ut 24)
        (progn 
          (setq nd (list (car date) (+ 1 (car (cdr date)))
                         (car (cdr (cdr date)))))
          (setq ut (- ut 24))))
    (if (< ut 0)
        (progn 
          (setq nd (list (car date) (- (car (cdr date)) 1)
                         (car (cdr (cdr date)))))
          (setq ut (+ ut 24))))
    (setq nd (calendar-gregorian-from-absolute
                       (calendar-absolute-from-gregorian nd)))
        ; date standardization
    (list nd ut)))

(defun solar-sunrise-sunset (date)
  "List of *local* times of sunrise, sunset, and daylight on Gregorian DATE.

Corresponding value is nil if there is no sunrise/sunset."
  (let* (; first, get the exact moment of local noon.
         (exact-local-noon (solar-exact-local-noon date))
         ; get the time from the 2000 epoch.
         (t0 (solar-julian-ut-centuries (car exact-local-noon)))
         ; store the sidereal time at Greenwich at midnight of UT time.
         ; find if summer or winter slightly above the equator
         (equator-rise-set
          (progn (setq solar-sidereal-time-greenwich-midnight 
                       (solar-sidereal-time t0))
                 (solar-sunrise-and-sunset 
                  (list t0 (car (cdr exact-local-noon)))
                  10.0
                  (calendar-longitude))))
         ; store the spring/summer information,
         ; compute sunrise and sunset (two first components of rise-set).
         ; length of day is the third component (it is only the difference
         ; between sunset and sunrise when there is a sunset and a sunrise)
         (rise-set
          (progn
            (setq solar-spring-or-summer-season 
                  (if (> (car (cdr (cdr equator-rise-set))) 12) 1 0))
            (solar-sunrise-and-sunset 
             (list t0 (car (cdr exact-local-noon)))
             (calendar-latitude)
             (calendar-longitude))))
         (rise (car rise-set))
         (adj-rise (if rise (dst-adjust-time date rise) nil))
         (set (car (cdr rise-set)))
         (adj-set (if set (dst-adjust-time date set) nil))
         (length  (car (cdr (cdr rise-set)))) )
    (list
     (and rise (calendar-date-equal date (car adj-rise)) (cdr adj-rise))
     (and set (calendar-date-equal date (car adj-set)) (cdr adj-set))
     (solar-daylight length))))

(defun solar-sunrise-sunset-string (date)
  "String of *local* times of sunrise, sunset, and daylight on Gregorian DATE."
  (let ((l (solar-sunrise-sunset date)))
    (format
     "%s, %s at %s (%s hours daylight)"
     (if (car l)
         (concat "Sunrise " (apply 'solar-time-string (car l)))
       "No sunrise")
     (if (car (cdr l))
         (concat "sunset " (apply 'solar-time-string (car (cdr l))))
       "no sunset")
     (eval calendar-location-name)
     (car (cdr (cdr l))))))

(defun solar-julian-ut-centuries (date)
  "Number of Julian centuries elapsed since 1 Jan, 2000 at noon  U.T. for Gregorian DATE."
  (/ (- (calendar-absolute-from-gregorian date) 
        (calendar-absolute-from-gregorian '(1 1.5 2000)))
     36525.0))
  
(defun solar-ephemeris-time(time)
  "Ephemeris Time at moment TIME.

TIME is a pair with the first component being the number of Julian centuries
elapsed at 0 Universal Time, and the second component being the universal
time.  For instance, the pair corresponding to November 28, 1995 at 16 UT is
\(-0.040945 16), -0.040945 being the number of julian centuries elapsed between
Jan 1, 2000 at 12 UT and November 28, 1995 at 0 UT.

Result is in julian centuries of ephemeris time."
  (let* ((t0 (car time))
         (ut (car (cdr time)))
         (t1 (+ t0 (/ (/ ut 24.0) 36525)))
         (y (+ 2000 (* 100 t1)))
         (dt (* 86400 (solar-ephemeris-correction (floor y)))))
      (+ t1 (/ (/ dt 86400) 36525))))

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

(defun solar-horizontal-coordinates 
          (time latitude longitude for-sunrise-sunset)
  "Azimuth and height of the sun at TIME, LATITUDE, and LONGITUDE.

TIME is a pair with the first component being the number of Julian centuries
elapsed at 0 Universal Time, and the second component being the universal
time.  For instance, the pair corresponding to November 28, 1995 at 16 UT is
\(-0.040945 16), -0.040945 being the number of julian centuries elapsed between
Jan 1, 2000 at 12 UT and November 28, 1995 at 0 UT.

The azimuth is given in degrees as well as the height (between -180 and 180)."
  (let* ((ut (car (cdr time)))
         (ec (solar-equatorial-coordinates time for-sunrise-sunset))
         (st (+ solar-sidereal-time-greenwich-midnight
                (* ut 1.00273790935)))
         (ah (- (* st 15) (* 15 (car ec)) (* -1 (calendar-longitude))))
                       ; hour angle (in degrees)
         (de (car (cdr ec)))
         (azimuth (solar-atn2 (- (* (solar-cosine-degrees ah)
                                   (solar-sin-degrees latitude))
                                (* (solar-tangent-degrees de)
                                   (solar-cosine-degrees latitude)))
                              (solar-sin-degrees ah)))
         (height (solar-arcsin 
                  (+ (* (solar-sin-degrees latitude) (solar-sin-degrees de))
                     (* (solar-cosine-degrees latitude)
                        (solar-cosine-degrees de)
                        (solar-cosine-degrees ah))))))
    (if (> height 180) (setq height (- height 360)))
    (list azimuth height)))

(defun solar-equatorial-coordinates (time for-sunrise-sunset)
  "Right ascension (in hours) and declination (in degrees) of the sun at TIME.

TIME is a pair with the first component being the number of Julian centuries
elapsed at 0 Universal Time, and the second component being the universal
time.  For instance, the pair corresponding to November 28, 1995 at 16 UT is
\(-0.040945 16), -0.040945 being the number of julian centuries elapsed between
Jan 1, 2000 at 12 UT and November 28, 1995 at 0 UT."
   (let* ((tm (solar-ephemeris-time time)) 
          (ec (solar-ecliptic-coordinates tm for-sunrise-sunset)))
     (list (solar-right-ascension (car ec) (car (cdr ec)))
           (solar-declination (car ec) (car (cdr ec))))))

(defun solar-ecliptic-coordinates (time for-sunrise-sunset)
  "Apparent longitude of the sun, ecliptic inclination, (both in degrees)
equation of time (in hours) and nutation in longitude (in seconds)
at moment `time', expressed in julian centuries of Ephemeris Time
since January 1st, 2000, at 12 ET."
  (let* ((l (+ 280.46645
               (* 36000.76983 time)
               (* 0.0003032 time time))) ; sun mean longitude 
         (ml (+ 218.3165
                (* 481267.8813 time))) ; moon mean longitude 
         (m (+ 357.52910
               (* 35999.05030 time)
               (* -0.0001559 time time)
               (* -0.00000048 time time time))) ; sun mean anomaly 
         (i (+ 23.43929111 (* -0.013004167 time)
               (* -0.00000016389 time time)
               (* 0.0000005036 time time time))); mean inclination 
         (c (+ (* (+ 1.914600
                     (* -0.004817 time)
                     (* -0.000014 time time))
                  (solar-sin-degrees m))
               (* (+ 0.019993 (* -0.000101 time))
                  (solar-sin-degrees (* 2 m)))
               (* 0.000290
                  (solar-sin-degrees (* 3 m))))) ; center equation 
         (L (+ l c)) ; total longitude 
         (omega (+ 125.04
                   (* -1934.136 time))) ; longitude of moon's ascending node
                                        ; on the ecliptic
         (nut (if (not for-sunrise-sunset)
                 (+ (* -17.20 (solar-sin-degrees omega))
                 (* -1.32 (solar-sin-degrees (* 2 l)))
                 (* -0.23 (solar-sin-degrees (* 2 ml)))
                 (* 0.21 (solar-sin-degrees (* 2 omega))))
               nil))
                  ; nut = nutation in longitude, measured in seconds of angle.
         (ecc (if (not for-sunrise-sunset)
                 (+ 0.016708617
                 (* -0.000042037 time)
                 (* -0.0000001236 time time)) ; eccentricity of earth's orbit
               nil))
         (app (+ L
                 -0.00569
                 (* -0.00478
                    (solar-sin-degrees omega)))) ; apparent longitude of sun
         (y (if (not for-sunrise-sunset)
                 (* (solar-tangent-degrees (/ i 2)) 
                  (solar-tangent-degrees (/ i 2)))
                nil))
         (time-eq (if (not for-sunrise-sunset)
                    (/ (* 12 (+ (* y (solar-sin-degrees (* 2 l)))
                     (* -2 ecc (solar-sin-degrees m))
                     (* 4 ecc y (solar-sin-degrees m) 
                                (solar-cosine-degrees (* 2 l)))
                     (* -0.5 y y  (solar-sin-degrees (* 4 l)))
                     (* -1.25 ecc ecc (solar-sin-degrees (* 2 m)))))
                      3.1415926535)
                    nil)))
                  ; equation of time, in hours
    (list app i time-eq nut)))

(defun solar-longitude (d)
  "Longitude of sun on astronomical (Julian) day number D.
Accurary is about 0.0006 degree (about 365.25*24*60*0.0006/360 = 1 minutes).

The values of calendar-daylight-savings-starts,
calendar-daylight-savings-starts-time, calendar-daylight-savings-ends,
calendar-daylight-savings-ends-time, calendar-daylight-time-offset, and
calendar-time-zone are used to interpret local time."
  (let* ((a-d (calendar-absolute-from-astro d))
         ;; get Universal Time
         (date (calendar-astro-from-absolute
                (- a-d
                   (if (dst-in-effect a-d)
                       (/ calendar-daylight-time-offset 24.0 60.0) 0)
                   (/ calendar-time-zone 60.0 24.0))))
         ;; get Ephemeris Time
         (date (+ date (solar-ephemeris-correction
                        (extract-calendar-year
                         (calendar-gregorian-from-absolute
                          (floor
                           (calendar-absolute-from-astro
                            date)))))))
         (U (/ (- date 2451545) 3652500))
         (longitude
          (+ 4.9353929
             (* 62833.1961680 U)
             (* 0.0000001
                (apply '+
                       (mapcar '(lambda (x)
                                  (* (car x)
                                     (sin (mod
                                           (+ (car (cdr x))
                                              (* (car (cdr (cdr x))) U))
                                           (* 2 pi)))))
                               solar-data-list)))))
         (aberration
          (* 0.0000001 (- (* 17 (cos (+ 3.10 (* 62830.14 U)))) 973)))
         (A1 (mod (+ 2.18 (* U (+ -3375.70 (* 0.36 U)))) (* 2 pi)))
         (A2 (mod (+ 3.51 (* U (+ 125666.39 (* 0.10 U)))) (* 2 pi)))
         (nutation (* -0.0000001 (+ (* 834 (sin A1)) (* 64 (sin A2))))))
    (mod (radians-to-degrees (+ longitude aberration nutation)) 360.0)))

(defconst solar-data-list
  '((403406 4.721964 1.621043)
    (195207 5.937458 62830.348067)
    (119433 1.115589 62830.821524)
    (112392 5.781616 62829.634302)
    (3891 5.5474 125660.5691)
    (2819 1.5120 125660.984)
    (1721 4.1897 62832.4766)
    (0 1.163 0.813)
    (660 5.415 125659.31)
    (350 4.315 57533.85)
    (334 4.553 -33.931)
    (314 5.198 777137.715)
    (268 5.989 78604.191)
    (242 2.911 5.412)
    (234 1.423 39302.098)
    (158 0.061 -34.861)
    (132 2.317 115067.698)
    (129 3.193 15774.337)
    (114 2.828 5296.670)
    (99 0.52 58849.27)
    (93 4.65 5296.11)
    (86 4.35 -3980.70)
    (78 2.75 52237.69)
    (72 4.50 55076.47)
    (68 3.23 261.08)
    (64 1.22 15773.85)
    (46 0.14 188491.03)
    (38 3.44 -7756.55)
    (37 4.37 264.89)
    (32 1.14 117906.27)
    (29 2.84 55075.75)
    (28 5.96 -7961.39)
    (27 5.09 188489.81)
    (27 1.72 2132.19)
    (25 2.56 109771.03)
    (24 1.92 54868.56)
    (21 0.09 25443.93)
    (21 5.98 -55731.43)
    (20 4.03 60697.74)
    (18 4.47 2132.79)
    (17 0.79 109771.63)
    (14 4.24 -7752.82)
    (13 2.01 188491.91)
    (13 2.65 207.81)
    (13 4.98 29424.63)
    (12 0.93 -7.99)
    (10 2.21 46941.14)
    (10 3.59 -68.29)
    (10 1.50 21463.25)
    (10 2.55 157208.40)))

(defun solar-ephemeris-correction (year)
  "Ephemeris time minus Universal Time during Gregorian year.
Result is in days.

For the years 1800-1987, the maximum error is 1.9 seconds.
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

(defun solar-sidereal-time (t0)
  "Sidereal time (in hours) in Greenwich.

At T0=Julian centuries of universal time.
T0 must correspond to 0 hours UT."
   (let* ((mean-sid-time (+ 6.6973746
                           (* 2400.051337 t0)
                           (* 0.0000258622 t0 t0)
                           (* -0.0000000017222 t0 t0 t0)))
          (et (solar-ephemeris-time (list t0 0.0)))
          (nut-i (solar-ecliptic-coordinates et nil))
          (nut (car (cdr (cdr (cdr nut-i))))) ; nutation
          (i (car (cdr nut-i)))) ; inclination
       (mod (+ (mod (+ mean-sid-time 
                    (/ (/ (* nut (solar-cosine-degrees i)) 15) 3600)) 24.0)
               24.0)
            24.0)))

(defun solar-time-equation (date ut)
  "Equation of time expressed in hours at Gregorian DATE at Universal time UT."
  (let* ((et (solar-date-to-et date ut))
         (ec (solar-ecliptic-coordinates et nil)))
     (car (cdr (cdr ec)))))

(defun solar-date-to-et (date ut)
  "Ephemeris Time at Gregorian DATE at Universal Time UT (in hours).
Expressed in julian centuries of Ephemeris Time."
    (let ((t0 (solar-julian-ut-centuries date)))
      (solar-ephemeris-time (list t0 ut))))

;;;###autoload
(defun sunrise-sunset (&optional arg)
  "Local time of sunrise and sunset for today.  Accurate to a few seconds.
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
        (time-string (solar-sunrise-sunset-string date))
        (msg (format "%s: %s" date-string time-string))
        (one-window (one-window-p t)))
   (if (<= (length msg) (frame-width))
       (message "%s" msg)
     (with-output-to-temp-buffer "*temp*"
       (princ (concat date-string "\n" time-string)))
     (message "%s"
	      (substitute-command-keys
               (if one-window
                   (if pop-up-windows
                       "Type \\[delete-other-windows] to remove temp window."
                     "Type \\[switch-to-buffer] RET to remove temp window.")
                 "Type \\[switch-to-buffer-other-window] RET to restore old contents of temp window."))))))
 
(defun calendar-sunrise-sunset ()
  "Local time of sunrise and sunset for date under cursor.
Accurate to a few seconds."
  (interactive)
  (if (not (and calendar-latitude calendar-longitude calendar-time-zone))
      (solar-setup))
  (let ((date (calendar-cursor-to-date t)))
    (message "%s: %s"
             (calendar-date-string date t t)
             (solar-sunrise-sunset-string date))))

(defun diary-sunrise-sunset ()
  "Local time of sunrise and sunset as a diary entry.
Accurate to a few seconds."
  (if (not (and calendar-latitude calendar-longitude calendar-time-zone))
      (solar-setup))
  (solar-sunrise-sunset-string date))

(defun diary-sabbath-candles ()
  "Local time of candle lighting diary entry--applies if date is a Friday.
No diary entry if there is no sunset on that date."
  (if (not (and calendar-latitude calendar-longitude calendar-time-zone))
      (solar-setup))
  (if (= (% (calendar-absolute-from-gregorian date) 7) 5);;  Friday
      (let* ((sunset (car (cdr (solar-sunrise-sunset date))))
                  (light (if sunset
                        (cons (- (car sunset) (/ 18.0 60.0)) (cdr sunset)))))
        (if sunset
            (format "%s Sabbath candle lighting"
                    (apply 'solar-time-string light))))))

(defun solar-equinoxes/solstices (k year)
  "Date of equinox/solstice K for YEAR.
K=0, spring equinox; K=1, summer solstice; K=2, fall equinox;
K=3, winter solstice. 
RESULT is a gregorian local date.

Accurate to less than a minute between 1951 and 2050."
  (let* ((JDE0 (solar-mean-equinoxes/solstices k year))
         (T (/ (- JDE0 2451545.0) 36525))
         (W (- (* 35999.373 T) 2.47))
         (Delta-lambda (+ 1 (* 0.0334 (solar-cosine-degrees W))
                            (* 0.0007 (solar-cosine-degrees (* 2 W)))))
         (S (apply '+ (mapcar '(lambda(x) 
                                 (* (car x) (solar-cosine-degrees 
                                             (+ (* (car (cdr (cdr x))) T)
                                                  (car (cdr x)))))) 
                              solar-seasons-data)))
         (JDE (+ JDE0 (/ (* 0.00001 S) Delta-lambda)))
         (correction (+ 102.3 (* 123.5 T) (* 32.5 T T))) 
             ; ephemeris time correction
         (JD (- JDE (/ correction 86400)))
         (date (calendar-gregorian-from-absolute (floor (- JD 1721424.5))))
         (time (- (- JD 0.5) (floor (- JD 0.5))))
         )
      (list (car date) (+ (car (cdr date)) time
                          (/ (/ calendar-time-zone 60.0) 24.0))
            (car (cdr (cdr date))))))

; from Meeus, 1991, page 166
(defun solar-mean-equinoxes/solstices (k year)
  "Julian day of mean equinox/solstice K for YEAR.  
K=0, spring equinox; K=1, summer solstice; K=2, fall equinox; K=3, winter
solstice.  These formulas are only to be used between 1000 BC and 3000 AD."
  (let ((y (/ year 1000.0))
        (z (/ (- year 2000) 1000.0)))
    (if (< year 1000) ; actually between -1000 and 1000
             (cond ((equal k 0) (+ 1721139.29189
                                   (*  365242.13740 y)
                                   (* 0.06134 y y)
                                   (* 0.00111 y y y)
                                   (* -0.00071 y y y y)))
                   ((equal k 1) (+ 1721233.25401
                                   (* 365241.72562 y)
                                   (* -0.05323 y y)
                                   (* 0.00907 y y y)
                                   (* 0.00025 y y y y)))
                   ((equal k 2) (+ 1721325.70455
                                   (* 365242.49558 y)
                                   (* -0.11677 y y)
                                   (* -0.00297 y y y)
                                   (* 0.00074 y y y y)))
                   ((equal k 3) (+ 1721414.39987
                                   (* 365242.88257 y)
                                   (* -0.00769 y y)
                                   (* -0.00933 y y y)
                                   (* -0.00006 y y y y))))
                      ; actually between 1000 and 3000
             (cond ((equal k 0) (+ 2451623.80984
                                   (* 365242.37404  z)
                                   (* 0.05169 z z)
                                   (* -0.00411 z z z)
                                   (* -0.00057 z z z z)))
                   ((equal k 1) (+ 2451716.56767
                                   (* 365241.62603 z)
                                   (* 0.00325 z z)
                                   (* 0.00888 z z z)
                                   (* -0.00030 z z z z)))
                   ((equal k 2) (+ 2451810.21715
                                   (* 365242.01767 z)
                                   (* -0.11575 z z)
                                   (* 0.00337 z z z)
                                   (* 0.00078 z z z z)))
                   ((equal k 3) (+ 2451900.05952
                                   (* 365242.74049 z)
                                   (* -0.06223 z z)
                                   (* -0.00823 z z z)
                                   (* 0.00032 z z z z)))))))

; from Meeus, 1991, page 167
(defconst solar-seasons-data
  '((485 324.96 1934.136)
    (203 337.23 32964.467)
    (199 342.08 20.186)
    (182 27.85 445267.112)
    (156 73.14 45036.886)
    (136 171.52 22518.443)
    (77 222.54 65928.934)
    (74 296.72 3034.906)
    (70 243.58 9037.513)
    (58 119.81 33718.147)
    (52 297.17 150.678)
    (50 21.02 2281.226)
    (45 247.54 29929.562)
    (44 325.15 31555.956)
    (29 60.93 4443.417)
    (18 155.12 67555.328)
    (17 288.79 4562.452)
    (16 198.04 62894.029)
    (14 199.76 31436.921)
    (12 95.39 14577.848)
    (12 287.11 31931.756)
    (12 320.81 34777.259)
    (9 227.73 1222.114)
    (8 15.45 16859.074)))

;;;###autoload
(defun solar-equinoxes-solstices ()
  "*local* date and time of equinoxes and solstices, if visible in the calendar window.
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
           (d0 (solar-equinoxes/solstices k y)) 
           (d1 (list (car d0) (floor (car (cdr d0))) (car (cdr (cdr d0)))))
           (h0 (* 24 (- (car (cdr d0)) (floor (car (cdr d0))))))
           (adj (dst-adjust-time d1 h0))
           (d (list (car d1) (+ (car (cdr d1))  
                  (/ (car (cdr adj)) 24.0))
                    (car (cdr (cdr d1)))))
           ; The following is nearly as accurate, but not quite:
	   ;(d0 (solar-date-next-longitude
           ;    (calendar-astro-from-absolute
           ;     (calendar-absolute-from-gregorian
           ;      (list (+ 3 (* k 3)) 15 y)))
           ;    90))
           ;(abs-day (calendar-absolute-from-astro d)))
           (abs-day (calendar-absolute-from-gregorian d)))
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
