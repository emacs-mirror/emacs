;;; Timezone package for GNU Emacs

;; Copyright (C) 1990, 1991, 1992, 1993 Free Software Foundation, Inc.

;;; Author: Masanobu Umeda
;;; Maintainer: umerin@mse.kyutech.ac.jp

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

;;; Code:

(provide 'timezone)

(defvar timezone-world-timezones
  '(("PST" .  -800)
    ("PDT" .  -700)
    ("MST" .  -700)
    ("MDT" .  -600)
    ("CST" .  -600)
    ("CDT" .  -500)
    ("EST" .  -500)
    ("EDT" .  -400)
    ("AST" .  -400)			;by <clamen@CS.CMU.EDU>
    ("NST" .  -330)			;by <clamen@CS.CMU.EDU>
    ("GMT" .  +000)
    ("BST" .  +100)
    ("MET" .  +100)
    ("EET" .  +200)
    ("JST" .  +900)
    ("GMT+1"  .  +100) ("GMT+2"  .  +200) ("GMT+3"  .  +300)
    ("GMT+4"  .  +400) ("GMT+5"  .  +500) ("GMT+6"  .  +600)
    ("GMT+7"  .  +700) ("GMT+8"  .  +800) ("GMT+9"  .  +900)
    ("GMT+10" . +1000) ("GMT+11" . +1100) ("GMT+12" . +1200) ("GMT+13" . +1300)
    ("GMT-1"  .  -100) ("GMT-2"  .  -200) ("GMT-3"  .  -300)
    ("GMT-4"  .  -400) ("GMT-5"  .  -500) ("GMT-6"  .  -600)
    ("GMT-7"  .  -700) ("GMT-8"  .  -800) ("GMT-9"  .  -900)
    ("GMT-10" . -1000) ("GMT-11" . -1100) ("GMT-12" . -1200))
  "*Time differentials of timezone from GMT in hour.")

(defvar timezone-months-assoc
  '(("JAN" .  1)("FEB" .  2)("MAR" .  3)
    ("APR" .  4)("MAY" .  5)("JUN" .  6)
    ("JUL" .  7)("AUG" .  8)("SEP" .  9)
    ("OCT" . 10)("NOV" . 11)("DEC" . 12))
  "Alist of first three letters of a month and its numerical representation.")

(defun timezone-make-date-arpa-standard (date &optional local timezone)
  "Convert DATE to an arpanet standard date.
Optional 1st argumetn LOCAL specifies the default local timezone of the DATE.
Optional 2nd argument TIMEZONE specifies a timezone to be represented in."
  (let* ((date   (timezone-parse-date date))
	 (year   (string-to-int (aref date 0)))
	 (month  (string-to-int (aref date 1)))
	 (day    (string-to-int (aref date 2)))
	 (time   (timezone-parse-time (aref date 3)))
	 (hour   (string-to-int (aref time 0)))
	 (minute (string-to-int (aref time 1)))
	 (second (string-to-int (aref time 2)))
	 (local  (or (aref date 4) local)) ;Use original if defined
	 (timezone (or timezone local))
	 (diff   (- (timezone-zone-to-minute timezone)
		    (timezone-zone-to-minute local)))
	 (new    (timezone-fix-time year month day
				    hour (+ minute diff) second)))
    (timezone-make-arpa-date (aref new 0) (aref new 1) (aref new 2)
			     (timezone-make-time-string
			      (aref new 3) (aref new 4) (aref new 5))
			     timezone)
    ))

(defun timezone-make-date-sortable (date &optional local timezone)
  "Convert DATE to a sortable date string.
Optional 1st argumetn LOCAL specifies the default local timezone of the DATE.
Optional 2nd argument TIMEZONE specifies a timezone to be represented in."
  (let* ((date   (timezone-parse-date date))
	 (year   (string-to-int (aref date 0)))
	 (month  (string-to-int (aref date 1)))
	 (day    (string-to-int (aref date 2)))
	 (time   (timezone-parse-time (aref date 3)))
	 (hour   (string-to-int (aref time 0)))
	 (minute (string-to-int (aref time 1)))
	 (second (string-to-int (aref time 2)))
	 (local  (or (aref date 4) local)) ;Use original if defined
	 (timezone (or timezone local))
	 (diff   (- (timezone-zone-to-minute timezone)
		    (timezone-zone-to-minute local)))
	 (new    (timezone-fix-time year month day
				    hour (+ minute diff) second)))
    (timezone-make-sortable-date (aref new 0) (aref new 1) (aref new 2)
				 (timezone-make-time-string
				  (aref new 3) (aref new 4) (aref new 5)))
    ))


;;
;; Parsers and Constructors of Date and Time
;;

(defun timezone-make-arpa-date (year month day time &optional timezone)
  "Make arpanet standard date string from YEAR, MONTH, DAY, and TIME.
Optional argument TIMEZONE specifies a time zone."
  (format "%02d %s %4d %s%s"
	  day
	  (capitalize (car (rassq month timezone-months-assoc)))
	  ;;(- year (* (/ year 100) 100))	;1990 -> 90
	  (if (< year 100) (+ year 1900) year) ;90->1990
	  time
	  (if timezone (concat " " timezone) "")
	  ))

(defun timezone-make-sortable-date (year month day time)
  "Make sortable date string from YEAR, MONTH, DAY, and TIME."
  (format "%4d%02d%02d%s"
	  ;;(- year (* (/ year 100) 100))	;1990 -> 90
	  (if (< year 100) (+ year 1900) year) ;90->1990
	  month day time))

(defun timezone-make-time-string (hour minute second)
  "Make time string from HOUR, MINUTE, and SECOND."
  (format "%02d:%02d:%02d" hour minute second))

(defun timezone-parse-date (date)
  "Parse DATE and return a vector [year month day time timezone].
19 is prepended to year if necessary. Timezone may be NIL if nothing.
Understand the following styles:
 (1) 14 Apr 89 03:20[:12] [GMT]
 (2) Fri, 17 Mar 89 4:01[:33] [GMT]
 (3) Mon Jan 16 16:12[:37] [GMT] 1989
 (4) 6 May 1992 1641-JST (Wednesday)"
  (let ((date (or date ""))
	(year nil)
	(month nil)
	(day nil)
	(time nil)
	(zone nil))			;This may be nil.
    (cond ((string-match
	    "\\([0-9]+\\)[ \t]+\\([^ \t,]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+:[0-9:]+\\)[ \t]*\\'" date)
	   ;; Styles: (1) and (2) without timezone
	   (setq year 3 month 2 day 1 time 4 zone nil))
	  ((string-match
	    "\\([0-9]+\\)[ \t]+\\([^ \t,]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+:[0-9:]+\\)[ \t]*\\([-+a-zA-Z0-9]+\\)" date)
	   ;; Styles: (1) and (2) with timezone and buggy timezone
	   (setq year 3 month 2 day 1 time 4 zone 5))
	  ((string-match
	    "\\([^ \t,]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+:[0-9:]+\\)[ \t]+\\([0-9]+\\)" date)
	   ;; Styles: (3) without timezone
	   (setq year 4 month 1 day 2 time 3 zone nil))
	  ((string-match
	    "\\([^ \t,]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+:[0-9:]+\\)[ \t]+\\([-+a-zA-Z0-9]+\\)[ \t]+\\([0-9]+\\)" date)
	   ;; Styles: (3) with timezoen
	   (setq year 5 month 1 day 2 time 3 zone 4))
	  ((string-match
	    "\\([0-9]+\\)[ \t]+\\([^ \t,]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+\\)[ \t]*\\([-+a-zA-Z0-9]+\\)" date)
	   ;; Styles: (4) with timezone
	   (setq year 3 month 2 day 1 time 4 zone 5))
	  )
    (if year
	(progn
	  (setq year
		(substring date (match-beginning year) (match-end year)))
	  ;; It is now Dec 1992.  8 years before the end of the World.
	  (if (< (length year) 4)
	      (setq year (concat "19" (substring year -2 nil))))
	  (setq month
		(int-to-string
		 (cdr
		  (assoc
		   (upcase
		    ;; Don't use `match-end' in order to take 3
		    ;; letters from the beginning.
		    (substring date
			       (match-beginning month)
			       (+ (match-beginning month) 3)))
		   timezone-months-assoc))))
	  (setq day
		(substring date (match-beginning day) (match-end day)))
	  (setq time
		(substring date (match-beginning time) (match-end time)))))
    (if zone
	(setq zone
	      (substring date (match-beginning zone) (match-end zone))))
    ;; Return a vector.
    (if year
	(vector year month day time zone)
      (vector "0" "0" "0" "0" nil))
    ))

(defun timezone-parse-time (time)
  "Parse TIME (HH:MM:SS) and return a vector [hour minute second].
Recognize HH:MM:SS, HH:MM, HHMMSS, HHMM."
  (let ((time (or time ""))
	(hour nil)
	(minute nil)
	(second nil))
    (cond ((string-match "\\`\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)\\'" time)
	   ;; HH:MM:SS
	   (setq hour 1 minute 2 second 3))
	  ((string-match "\\`\\([0-9]+\\):\\([0-9]+\\)\\'" time)
	   ;; HH:MM
	   (setq hour 1 minute 2 second nil))
	  ((string-match "\\`\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\'" time)
	   ;; HHMMSS
	   (setq hour 1 minute 2 second 3))
	  ((string-match "\\`\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\'" time)
	   ;; HHMM
	   (setq hour 1 minute 2 second nil))
	  )
    ;; Return [hour minute second]
    (vector
     (if hour
	 (substring time (match-beginning hour) (match-end hour)) "0")
     (if minute
	 (substring time (match-beginning minute) (match-end minute)) "0")
     (if second
	 (substring time (match-beginning second) (match-end second)) "0"))
    ))


;; Miscellaneous

(defun timezone-zone-to-minute (timezone)
  "Translate TIMEZONE (in zone name or integer) to integer minute."
  (if timezone
      (progn
	(setq timezone
	      (or (cdr (assoc (upcase timezone) timezone-world-timezones))
		  ;; +900
		  timezone))
	(if (stringp timezone)
	    (setq timezone (string-to-int timezone)))
	;; Taking account of minute in timezone.
	;; HHMM -> MM
	;;(+ (* 60 (/ timezone 100)) (% timezone 100))
	;; ANSI C compliance about truncation of integer division
	;; by eggert@twinsun.com (Paul Eggert)
	(let* ((abszone (max timezone (- timezone)))
 	       (minutes (+ (* 60 (/ abszone 100)) (% abszone 100))))
 	  (if (< timezone 0) (- minutes) minutes)))
    0))

(defun timezone-fix-time (year month day hour minute second)
  "Fix date and time."
  ;; MINUTE may be larger than 60 or smaller than -60.
  (let ((hour-fix
	 (if (< minute 0)
	     ;;(/ (- minute 59) 60) (/ minute 60)
	     ;; ANSI C compliance about truncation of integer division
	     ;; by eggert@twinsun.com (Paul Eggert)
	     (- (/ (- 59 minute) 60)) (/ minute 60))))
    (setq hour (+ hour hour-fix))
    (setq minute (- minute (* 60 hour-fix))))
  ;; HOUR may be larger than 24 or smaller than 0.
  (cond ((<= 24 hour)			;24 -> 00
	 (setq hour (- hour 24))
	 (setq day  (1+ day))
	 (if (< (timezone-last-day-of-month month year) day)
	     (progn
	       (setq month (1+ month))
	       (setq day 1)
	       (if (< 12 month)
		   (progn
		     (setq month 1)
		     (setq year (1+ year))
		     ))
	       )))
	((> 0 hour)
	 (setq hour (+ hour 24))
	 (setq day  (1- day))
	 (if (> 1 day)
	     (progn
	       (setq month (1- month))
	       (if (> 1 month)
		   (progn
		     (setq month 12)
		     (setq year (1- year))
		     ))
	       (setq day (timezone-last-day-of-month month year))
	       )))
	)
  (vector year month day hour minute second))

;; Partly copied from Calendar program by Edward M. Reingold.
;; Thanks a lot.

(defun timezone-last-day-of-month (month year)
  "The last day in MONTH during YEAR."
  (if (and (= month 2) (timezone-leap-year-p year))
      29
    (aref [31 28 31 30 31 30 31 31 30 31 30 31] (1- month))))

(defun timezone-leap-year-p (year)
  "Returns t if YEAR is a Gregorian leap year."
  (or (and (zerop  (% year 4))
	   (not (zerop (% year 100))))
      (zerop (% year 400))))

;;; timezone.el ends here
