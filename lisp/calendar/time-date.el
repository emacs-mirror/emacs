;;; time-date.el --- date and time handling functions
;; Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;;	Masanobu Umeda <umerin@mse.kyutech.ac.jp>
;; Keywords: mail news util

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'parse-time)

(autoload 'timezone-make-date-arpa-standard "timezone")

;;;###autoload
(defun date-to-time (date)
  "Parse a string that represents a date-time and return a time value."
  (condition-case ()
      (apply 'encode-time
	     (parse-time-string
	      ;; `parse-time-string' isn't sufficiently general or
	      ;; robust.  It fails to grok some of the formats that
	      ;; timzeone does (e.g. dodgy post-2000 stuff from some
	      ;; Elms) and either fails or returns bogus values.  Lars
	      ;; reverted this change, but that loses non-trivially
	      ;; often for me.  -- fx
	      (timezone-make-date-arpa-standard date)))
    (error (error "Invalid date: %s" date))))

(defun time-to-seconds (time)
  "Convert time value TIME to a floating point number.
You can use `float-time' instead."
  (+ (* (car time) 65536.0)
     (cadr time)
     (/ (or (nth 2 time) 0) 1000000.0)))

;;;###autoload
(defun seconds-to-time (seconds)
  "Convert SECONDS (a floating point number) to a time value."
  (list (floor seconds 65536)
	(floor (mod seconds 65536))
	(floor (* (- seconds (ffloor seconds)) 1000000))))

;;;###autoload
(defun time-less-p (t1 t2)
  "Say whether time value T1 is less than time value T2."
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
	   (< (nth 1 t1) (nth 1 t2)))))

;;;###autoload
(defun days-to-time (days)
  "Convert DAYS into a time value."
  (let* ((seconds (* 1.0 days 60 60 24))
	 (rest (expt 2 16))
	 (ms (condition-case nil (floor (/ seconds rest))
	       (range-error (expt 2 16)))))
    (list ms (condition-case nil (round (- seconds (* ms rest)))
	       (range-error (expt 2 16))))))

;;;###autoload
(defun time-since (time)
  "Return the time elapsed since TIME.
TIME should be either a time value or a date-time string."
  (when (stringp time)
    ;; Convert date strings to internal time.
    (setq time (date-to-time time)))
  (let* ((current (current-time))
	 (rest (when (< (nth 1 current) (nth 1 time))
		 (expt 2 16))))
    (list (- (+ (car current) (if rest -1 0)) (car time))
	  (- (+ (or rest 0) (nth 1 current)) (nth 1 time)))))

;;;###autoload
(defalias 'subtract-time 'time-subtract)

;;;###autoload
(defun time-subtract (t1 t2)
  "Subtract two time values.
Return the difference in the format of a time value."
  (let ((borrow (< (cadr t1) (cadr t2))))
    (list (- (car t1) (car t2) (if borrow 1 0))
	  (- (+ (if borrow 65536 0) (cadr t1)) (cadr t2)))))

;;;###autoload
(defun time-add (t1 t2)
  "Add two time values.  One should represent a time difference."
  (let ((high (car t1))
	(low (if (consp (cdr t1)) (nth 1 t1) (cdr t1)))
	(micro (if (numberp (car-safe (cdr-safe (cdr t1))))
		   (nth 2 t1)
		 0))
	(high2 (car t2))
	(low2 (if (consp (cdr t2)) (nth 1 t2) (cdr t2)))
	(micro2 (if (numberp (car-safe (cdr-safe (cdr t2))))
		    (nth 2 t2)
		  0)))
    ;; Add
    (setq micro (+ micro micro2))
    (setq low (+ low low2))
    (setq high (+ high high2))

    ;; Normalize
    ;; `/' rounds towards zero while `mod' returns a positive number,
    ;; so we can't rely on (= a (+ (* 100 (/ a 100)) (mod a 100))).
    (setq low (+ low (/ micro 1000000) (if (< micro 0) -1 0)))
    (setq micro (mod micro 1000000))
    (setq high (+ high (/ low 65536) (if (< low 0) -1 0)))
    (setq low (logand low 65535))

    (list high low micro)))

;;;###autoload
(defun date-to-day (date)
  "Return the number of days between year 1 and DATE.
DATE should be a date-time string."
  (time-to-days (date-to-time date)))

;;;###autoload
(defun days-between (date1 date2)
  "Return the number of days between DATE1 and DATE2.
DATE1 and DATE2 should be date-time strings."
  (- (date-to-day date1) (date-to-day date2)))

;;;###autoload
(defun date-leap-year-p (year)
  "Return t if YEAR is a leap year."
  (or (and (zerop (% year 4))
	   (not (zerop (% year 100))))
      (zerop (% year 400))))

;;;###autoload
(defun time-to-day-in-year (time)
  "Return the day number within the year corresponding to TIME."
  (let* ((tim (decode-time time))
	 (month (nth 4 tim))
	 (day (nth 3 tim))
	 (year (nth 5 tim))
	 (day-of-year (+ day (* 31 (1- month)))))
    (when (> month 2)
      (setq day-of-year (- day-of-year (/ (+ 23 (* 4 month)) 10)))
      (when (date-leap-year-p year)
	(setq day-of-year (1+ day-of-year))))
    day-of-year))

;;;###autoload
(defun time-to-days (time)
  "The number of days between the Gregorian date 0001-12-31bce and TIME.
TIME should be a time value.
The Gregorian date Sunday, December 31, 1bce is imaginary."
  (let* ((tim (decode-time time))
	 (month (nth 4 tim))
	 (day (nth 3 tim))
	 (year (nth 5 tim)))
    (+ (time-to-day-in-year time)	; 	Days this year
       (* 365 (1- year))		;	+ Days in prior years
       (/ (1- year) 4)			;	+ Julian leap years
       (- (/ (1- year) 100))		;	- century years
       (/ (1- year) 400))))		;	+ Gregorian leap years

;;;###autoload
(defun safe-date-to-time (date)
  "Parse a string that represents a date-time and return a time value.
If DATE is malformed, return a time value of zeros."
  (condition-case ()
      (date-to-time date)
    (error '(0 0))))

(provide 'time-date)

;;; arch-tag: addcf07b-b20a-465b-af72-550b8ac5190f
;;; time-date.el ends here
