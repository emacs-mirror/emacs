;;; time-date.el --- Date and time handling functions
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
  "Convert DATE into time."
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
  "Convert TIME to a floating point number."
  (+ (* (car time) 65536.0)
     (cadr time)
     (/ (or (nth 2 time) 0) 1000000.0)))

(defun seconds-to-time (seconds)
  "Convert SECONDS (a floating point number) to an Emacs time structure."
  (list (floor seconds 65536)
	(floor (mod seconds 65536))
	(floor (* (- seconds (ffloor seconds)) 1000000))))

(defun time-less-p (t1 t2)
  "Say whether time T1 is less than time T2."
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
	   (< (nth 1 t1) (nth 1 t2)))))

(defun days-to-time (days)
  "Convert DAYS into time."
  (let* ((seconds (* 1.0 days 60 60 24))
	 (rest (expt 2 16))
	 (ms (condition-case nil (floor (/ seconds rest))
	       (range-error (expt 2 16)))))
    (list ms (condition-case nil (round (- seconds (* ms rest)))
	       (range-error (expt 2 16))))))

(defun time-since (time)
  "Return the time since TIME, which is either an internal time or a date."
  (when (stringp time)
    ;; Convert date strings to internal time.
    (setq time (date-to-time time)))
  (let* ((current (current-time))
	 (rest (when (< (nth 1 current) (nth 1 time))
		 (expt 2 16))))
    (list (- (+ (car current) (if rest -1 0)) (car time))
	  (- (+ (or rest 0) (nth 1 current)) (nth 1 time)))))

(defun subtract-time (t1 t2)
  "Subtract two internal times."
  (let ((borrow (< (cadr t1) (cadr t2))))
    (list (- (car t1) (car t2) (if borrow 1 0))
	  (- (+ (if borrow 65536 0) (cadr t1)) (cadr t2)))))

(defun date-to-day (date)
  "Return the number of days between year 1 and DATE."
  (time-to-days (date-to-time date)))

(defun days-between (date1 date2)
  "Return the number of days between DATE1 and DATE2."
  (- (date-to-day date1) (date-to-day date2)))

(defun date-leap-year-p (year)
  "Return t if YEAR is a leap year."
  (or (and (zerop (% year 4))
	   (not (zerop (% year 100))))
      (zerop (% year 400))))

(defun time-to-day-in-year (time)
  "Return the day number within the year of the date month/day/year."
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

(defun time-to-days (time)
  "The number of days between the Gregorian date 0001-12-31bce and TIME.
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
  "Parse DATE and return a time structure.
If DATE is malformed, a zero time will be returned."
  (condition-case ()
      (date-to-time date)
    (error '(0 0))))

(provide 'time-date)

;;; time-date.el ends here
