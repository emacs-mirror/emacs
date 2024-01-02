;;; time-date.el --- Date and time handling functions  -*- lexical-binding: t -*-

;; Copyright (C) 1998-2024 Free Software Foundation, Inc.

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

(require 'cl-lib)
(require 'subr-x)

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
	`(let* ,(append `((,gensym (or ,time-value (time-convert nil 'list)))
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
DATE should be in one of the forms recognized by `parse-time-string'.
If DATE lacks timezone information, GMT is assumed."
  (condition-case err
      (let ((parsed (parse-time-string date)))
	(when (decoded-time-year parsed)
	  (decoded-time-set-defaults parsed))
	(encode-time parsed))
    (error
     (let ((overflow-error '(error "Specified time is not representable")))
       (if (equal err overflow-error)
	   (signal (car err) (cdr err))
	 (condition-case err
	     (encode-time (parse-time-string
			   (timezone-make-date-arpa-standard date)))
	   (error
	    (if (equal err overflow-error)
		(signal (car err) (cdr err))
	      (error "Invalid date: %s" date)))))))))

;;;###autoload
(defalias 'time-to-seconds #'float-time)

;;;###autoload
(defun seconds-to-time (seconds)
  "Convert SECONDS to a proper time, like `current-time' would."
  ;; FIXME: Should we (declare (obsolete time-convert "27.1")) ?
  (time-convert seconds 'list))

;;;###autoload
(defun days-to-time (days)
  "Convert Emacs-epoch DAYS into a time value.
Note that this does not use the same epoch as `time-to-days'; you
must subtract (time-to-days 0) first to convert, and may get nil
if the result is before the start."
  ;; FIXME: We should likely just pass `t' to `time-convert'.
  ;; All uses I could find in Emacs, GNU ELPA, and NonGNU ELPA can handle
  ;; any valid time representation as return value.
  (let ((time (time-convert (* 86400 days) 'list)))
    ;; Traditionally, this returned a two-element list if DAYS was an integer.
    ;; Keep that tradition if time-convert outputs timestamps in list form.
    (if (and (integerp days) (consp (cdr time)))
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
(define-obsolete-function-alias 'subtract-time #'time-subtract "26.1")

;;;###autoload
(defun date-to-day (date)
  "Return the absolute date of DATE, a date-time string.
The absolute date is the number of days elapsed since the imaginary
Gregorian date Sunday, December 31, 1 BC."
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
  (let* ((month (decoded-time-month tim))
         (day (decoded-time-day tim))
         (year (decoded-time-year tim))
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
  "The absolute pseudo-Gregorian date for TIME, a time value.
The absolute date is the number of days elapsed since the imaginary
Gregorian date Sunday, December 31, 1 BC."
  (let* ((tim (decode-time time))
	 (year (decoded-time-year tim)))
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
If DATE is malformed, return a time value of zero."
  (condition-case ()
      (date-to-time date)
    (error 0)))


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

The \"%s\" spec takes an additional optional parameter,
introduced by the \",\" character, to say how many decimals to
use.  \"%,1s\" means \"use one decimal\".

The \"%z\" specifier does not print anything.  When it is used, specifiers
must be given in order of decreasing size.  To the left of \"%z\", nothing
is output until the first non-zero unit is encountered.

The \"%x\" specifier does not print anything.  When it is used,
specifiers must be given in order of decreasing size.  To the
right of \"%x\", trailing zero units are not output."
  (let ((start 0)
        (units '(("y" "year"   31536000)
                 ("d" "day"       86400)
                 ("h" "hour"       3600)
                 ("m" "minute"       60)
                 ("s" "second"        1)
                 ("z")
                 ("x")))
        (case-fold-search t)
        spec match usedunits zeroflag larger prev name unit num
        leading-zeropos trailing-zeropos fraction
        chop-leading chop-trailing)
    (while (string-match "%\\.?[0-9]*\\(,[0-9]\\)?\\(.\\)" string start)
      (setq start (match-end 0)
            spec (match-string 2 string))
      (unless (string-equal spec "%")
        (or (setq match (assoc (downcase spec) units))
            (error "Bad format specifier: `%s'" spec))
        (if (assoc (downcase spec) usedunits)
            (error "Multiple instances of specifier: `%s'" spec))
        (if (or (string-equal (car match) "z")
                (string-equal (car match) "x"))
            (setq zeroflag t)
          (unless larger
            (setq unit (nth 2 match)
                  larger (and prev (> unit prev))
                  prev unit)))
        (push match usedunits)))
    (when (and zeroflag larger)
      (error "Units are not in decreasing order of size"))
    (unless (numberp seconds)
      (setq seconds (float-time seconds)))
    (setq fraction (mod seconds 1)
          seconds (round seconds))
    (dolist (u units)
      (setq spec (car u)
            name (cadr u)
            unit (nth 2 u))
      (when (string-match
             (format "%%\\(\\.?[0-9]+\\)?\\(,[0-9]+\\)?\\(%s\\)" spec)
             string)
        (cond
         ((string-equal spec "z")
          (setq chop-leading (and leading-zeropos
                                  (min leading-zeropos (match-beginning 0)))))
         ((string-equal spec "x")
          (setq chop-trailing t))
         (t
          ;; Cf article-make-date-line in gnus-art.
          (setq num (floor seconds unit)
                seconds (- seconds (* num unit)))
          (let ((is-zero (zerop (if (= unit 1)
                                    (+ num fraction)
                                  num))))
            ;; Start position of the first non-zero unit.
            (when (and (not leading-zeropos)
                       (not is-zero))
              (setq leading-zeropos (match-beginning 0)))
            (unless is-zero
              (setq trailing-zeropos nil))
            (when (and (not trailing-zeropos)
                       is-zero)
              (setq trailing-zeropos (match-beginning 0))))
          (setq string
                (replace-match
                 (format (if (match-string 2 string)
                             (concat
                              "%"
                              (and (match-string 1 string)
                                   (if (= (elt (match-string 1 string) 0) ?.)
                                       (concat "0" (substring
                                                    (match-string 1 string) 1))
                                     (match-string 1 string)))
                              (concat "." (substring
                                           (match-string 2 string) 1))
                              "f%s")
                           (concat "%" (match-string 1 string) "d%s"))
                         (if (= unit 1)
                             (+ num fraction)
                           num)
                         (if (string-equal (match-string 3 string) spec)
                             ""         ; lower-case, no unit-name
                           (format " %s%s" name
                                   (if (= num 1) "" "s"))))
                 t t string))))))
    (let ((pre string))
      (when (and chop-trailing trailing-zeropos)
        (setq string (substring string 0 trailing-zeropos)))
      (when chop-leading
        (setq string (substring string chop-leading)))
      ;; If we ended up removing everything, return the formatted
      ;; string in full.
      (when (equal string "")
        (setq string pre)))
    (setq string (replace-regexp-in-string "%[zx]" "" string)))
  (string-trim (string-replace "%%" "%" string)))

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

(defun date-days-in-month (year month)
  "The number of days in MONTH in YEAR."
  (unless (and (numberp month) (<= 1 month 12))
    (error "Month %s is invalid" month))
  (if (= month 2)
      (if (date-leap-year-p year)
          29
        28)
    (if (memq month '(1 3 5 7 8 10 12))
        31
      30)))

(defun date-ordinal-to-time (year ordinal)
  "Convert a YEAR/ORDINAL to the equivalent `decoded-time' structure.
ORDINAL is the number of days since the start of the year, with
January 1st being 1."
  (let ((month 1))
    (while (> ordinal (date-days-in-month year month))
      (setq ordinal (- ordinal (date-days-in-month year month))
            month (1+ month)))
    (list nil nil nil ordinal month year nil nil nil)))

(defun decoded-time-add (time delta)
  "Add DELTA to TIME, both of which are `decoded-time' structures.
TIME should represent a time, while DELTA should have non-nil
entries only for the values that should be altered.

For instance, if you want to \"add two months\" to TIME, then
leave all other fields but the month field in DELTA nil, and make
the month field 2.  For instance:

  (decoded-time-add (decode-time) (make-decoded-time :month 2))

The values in DELTA can be negative.

If applying a month/year delta leaves the time spec invalid, it
is decreased to be valid (\"add one month\" to January 31st 2019
will yield a result of February 28th 2019 and \"add one year\" to
February 29th 2020 will result in February 28th 2021).

Fields are added in a most to least significant order, so if the
adjustment described above happens, it happens before adding
days, hours, minutes or seconds.

When changing the time bits in TIME (i.e., second/minute/hour),
changes in daylight saving time are not taken into account."
  (let ((time (copy-sequence time))
        seconds)
    ;; Years are simple.
    (when (decoded-time-year delta)
      (cl-incf (decoded-time-year time) (decoded-time-year delta)))

    ;; Months are pretty simple, but start at 1 (for January).
    (when (decoded-time-month delta)
      (let ((new (+ (1- (decoded-time-month time)) (decoded-time-month delta))))
        (setf (decoded-time-month time) (1+ (mod new 12)))
        (cl-incf (decoded-time-year time) (/ new 12))))

    ;; Adjust for month length (as described in the doc string).
    (setf (decoded-time-day time)
          (min (date-days-in-month (decoded-time-year time)
                                   (decoded-time-month time))
               (decoded-time-day time)))

    ;; Days are iterative.
    (when-let* ((days (decoded-time-day delta)))
      (let ((increase (> days 0))
            (days (abs days)))
        (while (> days 0)
          (decoded-time--alter-day time increase)
          (cl-decf days))))

    ;; Do the time part, which is pretty simple (except for leap
    ;; seconds, I guess).
    ;; Time zone adjustments are basically the same as time adjustments.
    (setq seconds (time-convert (or (decoded-time-second delta) 0) t))
    (setq seconds
	  (time-add seconds
		    (time-convert (+ (* (or (decoded-time-hour delta) 0) 3600)
				     (* (or (decoded-time-minute delta) 0) 60)
				     (or (decoded-time-zone delta) 0))
				  (cdr seconds))))

    (decoded-time--alter-second time seconds)
    time))

(defun decoded-time--alter-month (time increase)
  "Increase or decrease the month in TIME by 1."
  (if increase
      (progn
        (cl-incf (decoded-time-month time))
        (when (> (decoded-time-month time) 12)
          (setf (decoded-time-month time) 1)
          (cl-incf (decoded-time-year time))))
    (cl-decf (decoded-time-month time))
    (when (zerop (decoded-time-month time))
      (setf (decoded-time-month time) 12)
      (cl-decf (decoded-time-year time)))))

(defun decoded-time--alter-day (time increase)
  "Increase or decrease the day in TIME by 1."
  (if increase
      (progn
        (cl-incf (decoded-time-day time))
        (when (> (decoded-time-day time)
                 (date-days-in-month (decoded-time-year time)
                                     (decoded-time-month time)))
          (setf (decoded-time-day time) 1)
          (decoded-time--alter-month time t)))
    (cl-decf (decoded-time-day time))
    (when (zerop (decoded-time-day time))
      (decoded-time--alter-month time nil)
      (setf (decoded-time-day time)
            (date-days-in-month (decoded-time-year time)
                                (decoded-time-month time))))))

(defun decoded-time--alter-second (time seconds)
  "Increase the time in TIME by SECONDS."
  (let* ((time-sec (time-convert (or (decoded-time-second time) 0) t))
	 (time-hz (cdr time-sec))
	 (old (time-add time-sec
			(time-convert
			 (+ (* 3600 (or (decoded-time-hour time) 0))
			    (* 60 (or (decoded-time-minute time) 0)))
			 time-hz)))
	 (new (time-convert (time-add old seconds) t))
	 (new-hz (cdr new))
	 (secsperday (time-convert 86400 new-hz)))
    ;; Hm...  DST...
    (while (time-less-p new 0)
      (decoded-time--alter-day time nil)
      (setq new (time-add new secsperday)))
    (while (not (time-less-p new secsperday))
      (decoded-time--alter-day time t)
      (setq new (time-subtract new secsperday)))
    (let ((sec (time-convert new 'integer)))
      (setf (decoded-time-second time) (time-add
					(time-convert (% sec 60) new-hz)
					(time-subtract
					 new (time-convert sec new-hz)))
	    (decoded-time-minute time) (% (/ sec 60) 60)
	    (decoded-time-hour time) (/ sec 3600)))))

(cl-defun make-decoded-time (&key second minute hour
                                  day month year
                                  (dst -1) zone)
  "Return a `decoded-time' structure with only the keywords given filled out."
  (list second minute hour day month year nil dst zone))

(defun decoded-time-set-defaults (time &optional default-zone)
  "Set most nil values in `decoded-time' TIME to default values.
This can set TIME's year, month, day, hour, minute and second.
The default value is based on January 1st, 1970 at midnight.
This year is used to guarantee portability; see Info
node `(elisp) Time of Day'.

Optional argument DEFAULT-ZONE specifies what time zone to
default to when TIME's time zone is nil (meaning local time).

TIME is modified and returned."
  (unless (decoded-time-second time)
    (setf (decoded-time-second time) 0))
  (unless (decoded-time-minute time)
    (setf (decoded-time-minute time) 0))
  (unless (decoded-time-hour time)
    (setf (decoded-time-hour time) 0))

  (unless (decoded-time-day time)
    (setf (decoded-time-day time) 1))
  (unless (decoded-time-month time)
    (setf (decoded-time-month time) 1))
  (unless (decoded-time-year time)
    (setf (decoded-time-year time) 1970))

  (unless (decoded-time-zone time)
    (setf (decoded-time-zone time) default-zone))

  ;; Do not set decoded-time-weekday or decoded-time-dst,
  ;; as encode-time can infer them well enough when unknown.

  time)

(defun decoded-time-period (time)
  "Interpret TIME as a period and return its length in seconds.
For computational purposes, years are 365 days long and months
are 30 days long."
  (+ (if (consp (decoded-time-second time))
         ;; Fractional second.
         (/ (float (car (decoded-time-second time)))
            (cdr (decoded-time-second time)))
       (or (decoded-time-second time) 0))
     (* (or (decoded-time-minute time) 0) 60)
     (* (or (decoded-time-hour time) 0) 60 60)
     (* (or (decoded-time-day time) 0) 60 60 24)
     (* (or (decoded-time-month time) 0) 60 60 24 30)
     (* (or (decoded-time-year time) 0) 60 60 24 365)))

(provide 'time-date)

;;; time-date.el ends here
