;;; time-date.el --- Date and time handling functions

;; Copyright (C) 1998-2018 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;;	Masanobu Umeda <umerin@mse.kyutech.ac.jp>
;; Keywords: mail news util

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Time values come in several formats.  The oldest format is a cons
;; cell of the form (HIGH . LOW).  This format is obsolete, but still
;; supported.  The other formats are the lists (HIGH LOW), (HIGH LOW
;; USEC), and (HIGH LOW USEC PSEC).  These formats specify the time
;; value equal to HIGH * 2^16 + LOW + USEC * 10^-6 + PSEC * 10^-12
;; seconds, where missing components are treated as zero.  HIGH can be
;; negative, either because the value is a time difference, or because
;; it represents a time stamp before the epoch.  Typically, there are
;; more time values than the underlying system time type supports,
;; but the reverse can also be true.

;;; Code:

(defmacro with-decoded-time-value (varlist &rest body)
  "Decode a time value and bind it according to VARLIST, then eval BODY.

The value of the last form in BODY is returned.

Each element of the list VARLIST is a list of the form
\(HIGH-SYMBOL LOW-SYMBOL MICRO-SYMBOL [PICO-SYMBOL [TYPE-SYMBOL]] TIME-VALUE).
The time value TIME-VALUE is decoded and the result is bound to
the symbols HIGH-SYMBOL, LOW-SYMBOL and MICRO-SYMBOL.
The optional PICO-SYMBOL is bound to the picoseconds part.

The optional TYPE-SYMBOL is bound to the type of the time value.
Type 0 is the cons cell (HIGH . LOW), type 1 is the list (HIGH
LOW), type 2 is the list (HIGH LOW MICRO), and type 3 is the
list (HIGH LOW MICRO PICO)."
  (declare (indent 1)
	   (debug ((&rest (symbolp symbolp symbolp
                           &or [symbolp symbolp form] [symbolp form] form))
		   body)))
  (if varlist
      (let* ((elt (pop varlist))
	     (high (pop elt))
	     (low (pop elt))
	     (micro (pop elt))
	     (pico (unless (<= (length elt) 2)
		     (pop elt)))
	     (type (unless (eq (length elt) 1)
		     (pop elt)))
	     (time-value (car elt))
	     (gensym (make-symbol "time")))
	`(let* ,(append `((,gensym (or ,time-value (current-time)))
			  (,gensym
			   (cond
			    ((integerp ,gensym)
			     (list (ash ,gensym -16)
				   (logand ,gensym 65535)))
			    ((floatp ,gensym)
			     (let* ((usec (* 1000000 (mod ,gensym 1)))
				    (ps (round (* 1000000 (mod usec 1))))
				    (us (floor usec))
				    (lo (floor (mod ,gensym 65536)))
				    (hi (floor ,gensym 65536)))
			       (if (eq ps 1000000)
				   (progn
				     (setq ps 0)
				     (setq us (1+ us))
				     (if (eq us 1000000)
					 (progn
					   (setq us 0)
					   (setq lo (1+ lo))
					   (if (eq lo 65536)
					       (progn
						 (setq lo 0)
						 (setq hi (1+ hi))))))))
			       (list hi lo us ps)))
			    (t ,gensym)))
			  (,high (pop ,gensym))
			  ,low ,micro)
			(when pico `(,pico))
			(when type `(,type)))
	   (if (consp ,gensym)
	       (progn
		 (setq ,low (pop ,gensym))
		 (if ,gensym
		     (progn
		       (setq ,micro (car ,gensym))
		       ,(cond (pico
			       `(if (cdr ,gensym)
				    ,(append `(setq ,pico (cadr ,gensym))
					     (when type `(,type 3)))
				  ,(append `(setq ,pico 0)
					   (when type `(,type 2)))))
			      (type
			       `(setq type 2))))
		   ,(append `(setq ,micro 0)
			    (when pico `(,pico 0))
			    (when type `(,type 1)))))
	     ,(append `(setq ,low ,gensym ,micro 0)
		      (when pico `(,pico 0))
		      (when type `(,type 0))))
	   (with-decoded-time-value ,varlist ,@body)))
    `(progn ,@body)))

(defun encode-time-value (high low micro pico &optional type)
  "Encode HIGH, LOW, MICRO, and PICO into a time value of type TYPE.
Type 0 is the cons cell (HIGH . LOW), type 1 is the list (HIGH LOW),
type 2 is (HIGH LOW MICRO), and type 3 is (HIGH LOW MICRO PICO).

For backward compatibility, if only four arguments are given,
it is assumed that PICO was omitted and should be treated as zero."
  (when (null type)
    (setq type pico)
    (setq pico 0))
  (cond
   ((eq type 0) (cons high low))
   ((eq type 1) (list high low))
   ((eq type 2) (list high low micro))
   ((eq type 3) (list high low micro pico))))

(make-obsolete 'encode-time-value nil "25.1")
(make-obsolete 'with-decoded-time-value nil "25.1")

(autoload 'parse-time-string "parse-time")
(autoload 'timezone-make-date-arpa-standard "timezone")

;;;###autoload
;; `parse-time-string' isn't sufficiently general or robust.  It fails
;; to grok some of the formats that timezone does (e.g. dodgy
;; post-2000 stuff from some Elms) and either fails or returns bogus
;; values.  timezone-make-date-arpa-standard should help.
(defun date-to-time (date)
  "Parse a string DATE that represents a date-time and return a time value.
If DATE lacks timezone information, GMT is assumed."
  (condition-case err
      (apply 'encode-time (parse-time-string date))
    (error
     (let ((overflow-error '(error "Specified time is not representable")))
       (if (equal err overflow-error)
	   (apply 'signal err)
	 (condition-case err
	     (apply 'encode-time
		    (parse-time-string
		     (timezone-make-date-arpa-standard date)))
	   (error
	    (if (equal err overflow-error)
		(apply 'signal err)
	      (error "Invalid date: %s" date)))))))))

;;;###autoload
(defalias 'time-to-seconds 'float-time)

;;;###autoload
(defun seconds-to-time (seconds)
  "Convert SECONDS to a time value."
  (time-add 0 seconds))

;;;###autoload
(defun days-to-time (days)
  "Convert DAYS into a time value."
  (let ((time (condition-case nil (seconds-to-time (* 86400.0 days))
		(range-error (list most-positive-fixnum 65535)))))
    (if (integerp days)
	(setcdr (cdr time) nil))
    time))

;;;###autoload
(defun time-since (time)
  "Return the time elapsed since TIME.
TIME should be either a time value or a date-time string."
  (when (stringp time)
    ;; Convert date strings to internal time.
    (setq time (date-to-time time)))
  (time-subtract nil time))

;;;###autoload
(define-obsolete-function-alias 'subtract-time 'time-subtract "26.1")

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

(defun time-date--day-in-year (tim)
  "Return the day number within the year corresponding to the decoded time TIM."
  (let* ((month (nth 4 tim))
	 (day (nth 3 tim))
	 (year (nth 5 tim))
	 (day-of-year (+ day (* 31 (1- month)))))
    (when (> month 2)
      (setq day-of-year (- day-of-year (/ (+ 23 (* 4 month)) 10)))
      (when (date-leap-year-p year)
	(setq day-of-year (1+ day-of-year))))
    day-of-year))

;;;###autoload
(defun time-to-day-in-year (time)
  "Return the day number within the year corresponding to TIME."
  (time-date--day-in-year (decode-time time)))

;;;###autoload
(defun time-to-days (time)
  "The number of days between the Gregorian date 0001-12-31bce and TIME.
TIME should be a time value.
The Gregorian date Sunday, December 31, 1bce is imaginary."
  (let* ((tim (decode-time time))
	 (year (nth 5 tim)))
    (+ (time-date--day-in-year tim)	;	Days this year
       (* 365 (1- year))		;	+ Days in prior years
       (/ (1- year) 4)			;	+ Julian leap years
       (- (/ (1- year) 100))		;	- century years
       (/ (1- year) 400))))		;	+ Gregorian leap years

(defun time-to-number-of-days (time)
  "Return the number of days represented by TIME.
Returns a floating point number."
  (/ (float-time time) (* 60 60 24)))

;;;###autoload
(defun safe-date-to-time (date)
  "Parse a string DATE that represents a date-time and return a time value.
If DATE is malformed, return a time value of zeros."
  (condition-case ()
      (date-to-time date)
    (error '(0 0))))


;;;###autoload
(defun format-seconds (string seconds)
  "Use format control STRING to format the number SECONDS.
The valid format specifiers are:
%y is the number of (365-day) years.
%d is the number of days.
%h is the number of hours.
%m is the number of minutes.
%s is the number of seconds.
%z is a non-printing control flag (see below).
%% is a literal \"%\".

Upper-case specifiers are followed by the unit-name (e.g. \"years\").
Lower-case specifiers return only the unit.

\"%\" may be followed by a number specifying a width, with an
optional leading \".\" for zero-padding.  For example, \"%.3Y\" will
return something of the form \"001 year\".

The \"%z\" specifier does not print anything.  When it is used, specifiers
must be given in order of decreasing size.  To the left of \"%z\", nothing
is output until the first non-zero unit is encountered.

This function does not work for SECONDS greater than `most-positive-fixnum'."
  (let ((start 0)
        (units '(("y" "year"   31536000)
                 ("d" "day"       86400)
                 ("h" "hour"       3600)
                 ("m" "minute"       60)
                 ("s" "second"        1)
                 ("z")))
        (case-fold-search t)
        spec match usedunits zeroflag larger prev name unit num zeropos)
    (while (string-match "%\\.?[0-9]*\\(.\\)" string start)
      (setq start (match-end 0)
            spec (match-string 1 string))
      (unless (string-equal spec "%")
        (or (setq match (assoc (downcase spec) units))
            (error "Bad format specifier: `%s'" spec))
        (if (assoc (downcase spec) usedunits)
            (error "Multiple instances of specifier: `%s'" spec))
        (if (string-equal (car match) "z")
            (setq zeroflag t)
          (unless larger
            (setq unit (nth 2 match)
                  larger (and prev (> unit prev))
                  prev unit)))
        (push match usedunits)))
    (and zeroflag larger
         (error "Units are not in decreasing order of size"))
    (dolist (u units)
      (setq spec (car u)
            name (cadr u)
            unit (nth 2 u))
      (when (string-match (format "%%\\(\\.?[0-9]+\\)?\\(%s\\)" spec) string)
        (if (string-equal spec "z")     ; must be last in units
            (setq string
                  (replace-regexp-in-string
                   "%z" ""
                   (substring string (min (or zeropos (match-end 0))
                                          (match-beginning 0)))))
          ;; Cf article-make-date-line in gnus-art.
          (setq num (floor seconds unit)
                seconds (- seconds (* num unit)))
          ;; Start position of the first non-zero unit.
          (or zeropos
              (setq zeropos (unless (zerop num) (match-beginning 0))))
          (setq string
                (replace-match
                 (format (concat "%" (match-string 1 string) "d%s") num
                         (if (string-equal (match-string 2 string) spec)
                             ""       ; lower-case, no unit-name
                           (format " %s%s" name
                                   (if (= num 1) "" "s"))))
                 t t string))))))
  (replace-regexp-in-string "%%" "%" string))

(defvar seconds-to-string
  (list (list 1 "ms" 0.001)
        (list 100 "s" 1)
        (list (* 60 100) "m" 60.0)
        (list (* 3600 30) "h" 3600.0)
        (list (* 3600 24 400) "d" (* 3600.0 24.0))
        (list nil "y" (* 365.25 24 3600)))
  "Formatting used by the function `seconds-to-string'.")
;;;###autoload
(defun seconds-to-string (delay)
  "Convert the time interval in seconds to a short string."
  (cond ((> 0 delay) (concat "-" (seconds-to-string (- delay))))
        ((= 0 delay) "0s")
        (t (let ((sts seconds-to-string) here)
             (while (and (car (setq here (pop sts)))
                         (<= (car here) delay)))
             (concat (format "%.2f" (/ delay (car (cddr here)))) (cadr here))))))

(provide 'time-date)

;;; time-date.el ends here
