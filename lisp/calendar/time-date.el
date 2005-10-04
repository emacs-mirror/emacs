;;; time-date.el --- Date and time handling functions
;; Copyright (C) 1998, 1999, 2000, 2001, 2002, 2004, 2005
;;        Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Time values come in three formats.  The oldest format is a cons
;; cell of the form (HIGH . LOW).  This format is obsolete, but still
;; supported.  The two other formats are the lists (HIGH LOW) and
;; (HIGH LOW MICRO).  The first two formats specify HIGH * 2^16 + LOW
;; seconds; the third format specifies HIGH * 2^16 + LOW + MICRO /
;; 1000000 seconds.  We should have 0 <= MICRO < 1000000 and 0 <= LOW
;; < 2^16.  If the time value represents a point in time, then HIGH is
;; nonnegative.  If the time value is a time difference, then HIGH can
;; be negative as well.  The macro `with-decoded-time-value' and the
;; function `encode-time-value' make it easier to deal with these
;; three formats.  See `time-subtract' for an example of how to use
;; them.

;;; Code:

(defmacro with-decoded-time-value (varlist &rest body)
  "Decode a time value and bind it according to VARLIST, then eval BODY.

The value of the last form in BODY is returned.

Each element of the list VARLIST is a list of the form
\(HIGH-SYMBOL LOW-SYMBOL MICRO-SYMBOL [TYPE-SYMBOL] TIME-VALUE).
The time value TIME-VALUE is decoded and the result it bound to
the symbols HIGH-SYMBOL, LOW-SYMBOL and MICRO-SYMBOL.

The optional TYPE-SYMBOL is bound to the type of the time value.
Type 0 is the cons cell (HIGH . LOW), type 1 is the list (HIGH
LOW), and type 3 is the list (HIGH LOW MICRO)."
  (declare (indent 1)
	   (debug ((&rest (symbolp symbolp symbolp &or [symbolp form] form))
		   body)))
  (if varlist
      (let* ((elt (pop varlist))
	     (high (pop elt))
	     (low (pop elt))
	     (micro (pop elt))
	     (type (unless (eq (length elt) 1)
		     (pop elt)))
	     (time-value (car elt))
	     (gensym (make-symbol "time")))
	`(let* ,(append `((,gensym ,time-value)
			  (,high (pop ,gensym))
			  ,low ,micro)
			(when type `(,type)))
	   (if (consp ,gensym)
	       (progn
		 (setq ,low (pop ,gensym))
		 (if ,gensym
		     ,(append `(setq ,micro (car ,gensym))
			      (when type `(,type 2)))
		   ,(append `(setq ,micro 0)
			    (when type `(,type 1)))))
	     ,(append `(setq ,low ,gensym ,micro 0)
		      (when type `(,type 0))))
	   (with-decoded-time-value ,varlist ,@body)))
    `(progn ,@body)))

(defun encode-time-value (high low micro type)
  "Encode HIGH, LOW, and MICRO into a time value of type TYPE.
Type 0 is the cons cell (HIGH . LOW), type 1 is the list (HIGH LOW),
and type 3 is the list (HIGH LOW MICRO)."
  (cond
   ((eq type 0) (cons high low))
   ((eq type 1) (list high low))
   ((eq type 2) (list high low micro))))

(autoload 'parse-time-string "parse-time")
(autoload 'timezone-make-date-arpa-standard "timezone")

;;;###autoload
(defun date-to-time (date)
  "Parse a string that represents a date-time and return a time value."
  (condition-case ()
      (apply 'encode-time
	     (parse-time-string
	      ;; `parse-time-string' isn't sufficiently general or
	      ;; robust.  It fails to grok some of the formats that
	      ;; timezone does (e.g. dodgy post-2000 stuff from some
	      ;; Elms) and either fails or returns bogus values.  Lars
	      ;; reverted this change, but that loses non-trivially
	      ;; often for me.  -- fx
	      (timezone-make-date-arpa-standard date)))
    (error (error "Invalid date: %s" date))))

;;;###autoload
(defun time-to-seconds (time)
  "Convert time value TIME to a floating point number.
You can use `float-time' instead."
  (with-decoded-time-value ((high low micro time))
    (+ (* 1.0 high 65536)
       low
       (/ micro 1000000.0))))

;;;###autoload
(defun seconds-to-time (seconds)
  "Convert SECONDS (a floating point number) to a time value."
  (list (floor seconds 65536)
	(floor (mod seconds 65536))
	(floor (* (- seconds (ffloor seconds)) 1000000))))

;;;###autoload
(defun time-less-p (t1 t2)
  "Say whether time value T1 is less than time value T2."
  (with-decoded-time-value ((high1 low1 micro1 t1)
			    (high2 low2 micro2 t2))
    (or (< high1 high2)
	(and (= high1 high2)
	     (or (< low1 low2)
		 (and (= low1 low2)
		      (< micro1 micro2)))))))

;;;###autoload
(defun days-to-time (days)
  "Convert DAYS into a time value."
  (let* ((seconds (* 1.0 days 60 60 24))
	 (high (condition-case nil (floor (/ seconds 65536))
		 (range-error most-positive-fixnum))))
    (list high (condition-case nil (floor (- seconds (* 1.0 high 65536)))
		 (range-error 65535)))))

;;;###autoload
(defun time-since (time)
  "Return the time elapsed since TIME.
TIME should be either a time value or a date-time string."
  (when (stringp time)
    ;; Convert date strings to internal time.
    (setq time (date-to-time time)))
  (time-subtract (current-time) time))

;;;###autoload
(defalias 'subtract-time 'time-subtract)

;;;###autoload
(defun time-subtract (t1 t2)
  "Subtract two time values.
Return the difference in the format of a time value."
  (with-decoded-time-value ((high low micro type t1)
			    (high2 low2 micro2 type2 t2))
    (setq high (- high high2)
	  low (- low low2)
	  micro (- micro micro2)
	  type (max type type2))
    (when (< micro 0)
      (setq low (1- low)
	    micro (+ micro 1000000)))
    (when (< low 0)
      (setq high (1- high)
	    low (+ low 65536)))
    (encode-time-value high low micro type)))

;;;###autoload
(defun time-add (t1 t2)
  "Add two time values.  One should represent a time difference."
  (with-decoded-time-value ((high low micro type t1)
			    (high2 low2 micro2 type2 t2))
    (setq high (+ high high2)
	  low (+ low low2)
	  micro (+ micro micro2)
	  type (max type type2))
    (when (>= micro 1000000)
      (setq low (1+ low)
	    micro (- micro 1000000)))
    (when (>= low 65536)
      (setq high (1+ high)
	    low (- low 65536)))
    (encode-time-value high low micro type)))

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

(defun time-to-number-of-days (time)
  "Return the number of days represented by TIME.
The number of days will be returned as a floating point number."
  (/ (time-to-seconds time) (* 60 60 24)))

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
