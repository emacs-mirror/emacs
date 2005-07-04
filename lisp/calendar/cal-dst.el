;;; cal-dst.el --- calendar functions for daylight savings rules

;; Copyright (C) 1993, 1994, 1995, 1996 Free Software Foundation, Inc.

;; Author: Paul Eggert <eggert@twinsun.com>
;;	Edward M. Reingold <reingold@cs.uiuc.edu>
;; Keywords: calendar
;; Human-Keywords: daylight savings time, calendar, diary, holidays

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This collection of functions implements the features of calendar.el and
;; holiday.el that deal with daylight savings time.

;; Comments, corrections, and improvements should be sent to
;;  Edward M. Reingold               Department of Computer Science
;;  (217) 333-6733                   University of Illinois at Urbana-Champaign
;;  reingold@cs.uiuc.edu             1304 West Springfield Avenue
;;                                   Urbana, Illinois 61801

;;; Code:

(require 'calendar)
(require 'cal-persia)

(defvar calendar-current-time-zone-cache nil
  "Cache for result of calendar-current-time-zone.")

(defvar calendar-system-time-basis
  (calendar-absolute-from-gregorian '(1 1 1970))
  "Absolute date of starting date of system clock.")

(defun calendar-absolute-from-time (x utc-diff)
  "Absolute local date of time X; local time is UTC-DIFF seconds from UTC.

X is (HIGH . LOW) or (HIGH LOW . IGNORED) where HIGH and LOW are the
high and low 16 bits, respectively, of the number of seconds since
1970-01-01 00:00:00 UTC, ignoring leap seconds.

Returns the pair (ABS-DATE . SECONDS) where SECONDS after local midnight on
absolute date ABS-DATE is the equivalent moment to X."
  (let* ((h (car x))
	 (xtail (cdr x))
         (l (+ utc-diff (if (numberp xtail) xtail (car xtail))))
         (u (+ (* 512 (mod h 675)) (floor l 128))))
    ;; Overflow is a terrible thing!
    (cons (+ calendar-system-time-basis
	     ;; floor((2^16 h +l) / (60*60*24))
	     (* 512 (floor h 675)) (floor u 675))
	  ;; (2^16 h +l) mod (60*60*24)
	  (+ (* (mod u 675) 128) (mod l 128)))))

(defun calendar-time-from-absolute (abs-date s)
  "Time of absolute date ABS-DATE, S seconds after midnight.

Returns the list (HIGH LOW) where HIGH and LOW are the high and low
16 bits, respectively, of the number of seconds 1970-01-01 00:00:00 UTC,
ignoring leap seconds, that is the equivalent moment to S seconds after
midnight UTC on absolute date ABS-DATE."
  (let* ((a (- abs-date calendar-system-time-basis))
         (u (+ (* 163 (mod a 512)) (floor s 128))))
    ;; Overflow is a terrible thing!
    (list
     ;; floor((60*60*24*a + s) / 2^16)
     (+ a (* 163 (floor a 512)) (floor u 512))
     ;; (60*60*24*a + s) mod 2^16
     (+ (* 128 (mod u 512)) (mod s 128)))))

(defun calendar-next-time-zone-transition (time)
  "Return the time of the next time zone transition after TIME.
Both TIME and the result are acceptable arguments to current-time-zone.
Return nil if no such transition can be found."
  (let* ((base 65536);; 2^16 = base of current-time output
	 (quarter-multiple 120);; approx = (seconds per quarter year) / base
	 (time-zone (current-time-zone time))
	 (time-utc-diff (car time-zone))
         hi
	 hi-zone
         (hi-utc-diff time-utc-diff)
         (quarters '(2 1 3)))
    ;; Heuristic: probe the time zone offset in the next three calendar
    ;; quarters, looking for a time zone offset different from TIME.
    (while (and quarters (eq time-utc-diff hi-utc-diff))
      (setq hi (cons (+ (car time) (* (car quarters) quarter-multiple)) 0))
      (setq hi-zone (current-time-zone hi))
      (setq hi-utc-diff (car hi-zone))
      (setq quarters (cdr quarters)))
    (and
     time-utc-diff
     hi-utc-diff
     (not (eq time-utc-diff hi-utc-diff))
     ;; Now HI is after the next time zone transition.
     ;; Set LO to TIME, and then binary search to increase LO and decrease HI
     ;; until LO is just before and HI is just after the time zone transition.
     (let* ((tail (cdr time))
	    (lo (cons (car time) (if (numberp tail) tail (car tail))))
	    probe)
       (while
	   ;; Set PROBE to halfway between LO and HI, rounding down.
	   ;; If PROBE equals LO, we are done.
	   (let* ((lsum (+ (cdr lo) (cdr hi)))
		  (hsum (+ (car lo) (car hi) (/ lsum base)))
		  (hsumodd (logand 1 hsum)))
	     (setq probe (cons (/ (- hsum hsumodd) 2)
			       (/ (+ (* hsumodd base) (% lsum base)) 2)))
	     (not (equal lo probe)))
	 ;; Set either LO or HI to PROBE, depending on probe results.
	 (if (eq (car (current-time-zone probe)) hi-utc-diff)
	     (setq hi probe)
	   (setq lo probe)))
       hi))))

(defun calendar-time-zone-daylight-rules (abs-date utc-diff)
  "Return daylight transition rule for ABS-DATE, UTC-DIFF sec offset from UTC.
ABS-DATE must specify a day that contains a daylight savings transition.
The result has the proper form for calendar-daylight-savings-starts'."
  (let* ((date (calendar-gregorian-from-absolute abs-date))
	 (weekday (% abs-date 7))
	 (m (extract-calendar-month date))
	 (d (extract-calendar-day date))
	 (y (extract-calendar-year date))
         (last (calendar-last-day-of-month m y))
	 (candidate-rules
	  (append
	   ;; Day D of month M.
	   (list (list 'list m d 'year))
	   ;; The first WEEKDAY of month M.
           (if (< d 8)
               (list (list 'calendar-nth-named-day 1 weekday m 'year)))
	   ;; The last WEEKDAY of month M.
           (if (> d (- last 7))
               (list (list 'calendar-nth-named-day -1 weekday m 'year)))
	   ;; The first WEEKDAY after day J of month M, for D-6 < J <= D.
           (let (l)
             (calendar-for-loop j from (max 2 (- d 6)) to (min d (- last 8)) do
		(setq l
		      (cons
		       (list 'calendar-nth-named-day 1 weekday m 'year j)
		       l)))
	     l)
	   ;; 01-01 and 07-01 for this year's Persian calendar.
	   (if (and (= m 3) (<= 20 d) (<= d 21))
	       '((calendar-gregorian-from-absolute
		  (calendar-absolute-from-persian
		   (list 1 1 (- year 621))))))
	   (if (and (= m 9) (<= 22 d) (<= d 23))
	       '((calendar-gregorian-from-absolute
		  (calendar-absolute-from-persian
		   (list 7 1 (- year 621))))))))
	 (prevday-sec (- -1 utc-diff)) ;; last sec of previous local day
	 (year (1+ y)))
    ;; Scan through the next few years until only one rule remains.
    (while
	(let ((rules candidate-rules)
	      new-rules)
	  (while
	      (let*
		  ((rule (car rules))
		   (date
		    ;; The following is much faster than
		    ;; (calendar-absolute-from-gregorian (eval rule)).
		    (cond ((eq (car rule) 'calendar-nth-named-day)
			   (eval (cons 'calendar-nth-named-absday (cdr rule))))
			  ((eq (car rule) 'calendar-gregorian-from-absolute)
			   (eval (car (cdr rule))))
			  (t (let ((g (eval rule)))
			       (calendar-absolute-from-gregorian g))))))
		(or (equal
		     (current-time-zone
		      (calendar-time-from-absolute date prevday-sec))
		     (current-time-zone
		      (calendar-time-from-absolute (1+ date) prevday-sec)))
		    (setq new-rules (cons rule new-rules)))
		(setq rules (cdr rules))))
	  ;; If no rules remain, just use the first candidate rule;
	  ;; it's wrong in general, but it's right for at least one year.
	  (setq candidate-rules (if new-rules (nreverse new-rules)
				  (list (car candidate-rules))))
	  (setq year (1+ year))
	  (cdr candidate-rules)))
    (car candidate-rules)))

(defun calendar-current-time-zone ()
  "Return UTC difference, dst offset, names and rules for current time zone.

Returns (UTC-DIFF DST-OFFSET STD-ZONE DST-ZONE DST-STARTS DST-ENDS
DST-STARTS-TIME DST-ENDS-TIME), based on a heuristic probing of what the
system knows:

UTC-DIFF is an integer specifying the number of minutes difference between
    standard time in the current time zone and Coordinated Universal Time
    (Greenwich Mean Time).  A negative value means west of Greenwich.
DST-OFFSET is an integer giving the daylight savings time offset in minutes.
STD-ZONE is a string giving the name of the time zone when no seasonal time
    adjustment is in effect.
DST-ZONE is a string giving the name of the time zone when there is a seasonal
    time adjustment in effect.
DST-STARTS and DST-ENDS are sexps in the variable `year' giving the daylight
    savings time start and end rules, in the form expected by
    `calendar-daylight-savings-starts'.
DST-STARTS-TIME and DST-ENDS-TIME are integers giving the number of minutes
    after midnight that daylight savings time starts and ends.

If the local area does not use a seasonal time adjustment, STD-ZONE and
DST-ZONE are equal, and all the DST-* integer variables are 0.

Some operating systems cannot provide all this information to Emacs; in this
case, `calendar-current-time-zone' returns a list containing nil for the data
it can't find."
  (or
   calendar-current-time-zone-cache
   (setq
    calendar-current-time-zone-cache
    (let* ((t0 (current-time))
	   (t0-zone (current-time-zone t0))
	   (t0-utc-diff (car t0-zone))
	   (t0-name (car (cdr t0-zone))))
      (if (not t0-utc-diff)
	  ;; Little or no time zone information is available.
	  (list nil nil t0-name t0-name nil nil nil nil)
	(let* ((t1 (calendar-next-time-zone-transition t0))
	       (t2 (and t1 (calendar-next-time-zone-transition t1))))
	  (if (not t2)
	      ;; This locale does not have daylight savings time.
	      (list (/ t0-utc-diff 60) 0 t0-name t0-name nil nil 0 0)
	    ;; Use heuristics to find daylight savings parameters.
	    (let* ((t1-zone (current-time-zone t1))
		   (t1-utc-diff (car t1-zone))
		   (t1-name (car (cdr t1-zone)))
		   (t1-date-sec (calendar-absolute-from-time t1 t0-utc-diff))
		   (t2-date-sec (calendar-absolute-from-time t2 t1-utc-diff))
		   (t1-rules (calendar-time-zone-daylight-rules
			      (car t1-date-sec) t0-utc-diff))
		   (t2-rules (calendar-time-zone-daylight-rules
			      (car t2-date-sec) t1-utc-diff))
		   (t1-time (/ (cdr t1-date-sec) 60))
		   (t2-time (/ (cdr t2-date-sec) 60)))
	      (cons
	       (/ (min t0-utc-diff t1-utc-diff) 60)
	       (cons
		(/ (abs (- t0-utc-diff t1-utc-diff)) 60)
		(if (< t0-utc-diff t1-utc-diff)
		    (list t0-name t1-name t1-rules t2-rules t1-time t2-time)
		    (list t1-name t0-name t2-rules t1-rules t2-time t1-time)
		    )))))))))))

;;; The following eight defvars relating to daylight savings time should NOT be
;;; marked to go into loaddefs.el where they would be evaluated when Emacs is
;;; dumped.  These variables' appropriate values depend on the conditions under
;;; which the code is INVOKED; so it's inappropriate to initialize them when
;;; Emacs is dumped---they should be initialized when calendar.el is loaded.
;;; They default to US Eastern time if time zone info is not available.

(calendar-current-time-zone)

(defvar calendar-time-zone (or (car calendar-current-time-zone-cache) -300)
  "*Number of minutes difference between local standard time at
`calendar-location-name' and Coordinated Universal (Greenwich) Time.  For
example, -300 for New York City, -480 for Los Angeles.")

(defvar calendar-daylight-time-offset
  (or (car (cdr calendar-current-time-zone-cache)) 60)
  "*Number of minutes difference between daylight savings and standard time.

If the locale never uses daylight savings time, set this to 0.")

(defvar calendar-standard-time-zone-name
  (or (car (nthcdr 2 calendar-current-time-zone-cache)) "EST")
  "*Abbreviated name of standard time zone at `calendar-location-name'.
For example, \"EST\" in New York City, \"PST\" for Los Angeles.")

(defvar calendar-daylight-time-zone-name
  (or (car (nthcdr 3 calendar-current-time-zone-cache)) "EDT")
  "*Abbreviated name of daylight-savings time zone at `calendar-location-name'.
For example, \"EDT\" in New York City, \"PDT\" for Los Angeles.")

;;;###autoload
(put 'calendar-daylight-savings-starts 'risky-local-variable t)
(defvar calendar-daylight-savings-starts
  (or (car (nthcdr 4 calendar-current-time-zone-cache))
      (and (not (zerop calendar-daylight-time-offset))
	   '(calendar-nth-named-day 1 0 4 year)))
  "*Sexp giving the date on which daylight savings time starts.
This is an expression in the variable `year' whose value gives the Gregorian
date in the form (month day year) on which daylight savings time starts.  It is
used to determine the starting date of daylight savings time for the holiday
list and for correcting times of day in the solar and lunar calculations.

For example, if daylight savings time is mandated to start on October 1,
you would set `calendar-daylight-savings-starts' to

      '(10 1 year)

If it starts on the first Sunday in April, you would set it to

      '(calendar-nth-named-day 1 0 4 year)

If the locale never uses daylight savings time, set this to nil.")

;;;###autoload
(put 'calendar-daylight-savings-ends 'risky-local-variable t)
(defvar calendar-daylight-savings-ends
  (or (car (nthcdr 5 calendar-current-time-zone-cache))
      (and (not (zerop calendar-daylight-time-offset))
	   '(calendar-nth-named-day -1 0 10 year)))
  "*Sexp giving the date on which daylight savings time ends.
This is an expression in the variable `year' whose value gives the Gregorian
date in the form (month day year) on which daylight savings time ends.  It is
used to determine the starting date of daylight savings time for the holiday
list and for correcting times of day in the solar and lunar calculations.

For example, if daylight savings time ends on the last Sunday in October:

      '(calendar-nth-named-day -1 0 10 year)

If the locale never uses daylight savings time, set this to nil.")

(defvar calendar-daylight-savings-starts-time
  (or (car (nthcdr 6 calendar-current-time-zone-cache)) 120)
  "*Number of minutes after midnight that daylight savings time starts.")

(defvar calendar-daylight-savings-ends-time
  (or (car (nthcdr 7 calendar-current-time-zone-cache))
      calendar-daylight-savings-starts-time)
  "*Number of minutes after midnight that daylight savings time ends.")

(defun dst-in-effect (date)
  "True if on absolute DATE daylight savings time is in effect.
Fractional part of DATE is local standard time of day."
  (let* ((year (extract-calendar-year
                (calendar-gregorian-from-absolute (floor date))))
         (dst-starts-gregorian (eval calendar-daylight-savings-starts))
         (dst-ends-gregorian (eval calendar-daylight-savings-ends))
         (dst-starts (and dst-starts-gregorian
                          (+ (calendar-absolute-from-gregorian
                              dst-starts-gregorian)
                             (/ calendar-daylight-savings-starts-time
                                60.0 24.0))))
         (dst-ends (and dst-ends-gregorian
                        (+ (calendar-absolute-from-gregorian
                            dst-ends-gregorian)
                           (/ (- calendar-daylight-savings-ends-time
                                 calendar-daylight-time-offset)
                              60.0 24.0)))))
    (and dst-starts dst-ends
         (if (< dst-starts dst-ends)
             (and (<= dst-starts date) (< date dst-ends))
           (or (<= dst-starts date) (< date dst-ends))))))

(defun dst-adjust-time (date time &optional style)
  "Adjust, to account for dst on DATE, decimal fraction standard TIME.
Returns a list (date adj-time zone) where `date' and `adj-time' are the values
adjusted for `zone'; here `date' is a list (month day year), `adj-time' is a
decimal fraction time, and `zone' is a string.

Optional parameter STYLE forces the result time to be standard time when its
value is 'standard and daylight savings time (if available) when its value is
'daylight.

Conversion to daylight savings time is done according to
`calendar-daylight-savings-starts', `calendar-daylight-savings-ends',
`calendar-daylight-savings-starts-time',
`calendar-daylight-savings-ends-time', and
`calendar-daylight-savings-offset'."

  (let* ((rounded-abs-date (+ (calendar-absolute-from-gregorian date)
			      (/ (round (* 60 time)) 60.0 24.0)))
         (dst (dst-in-effect rounded-abs-date))
	 (time-zone (if dst
			calendar-daylight-time-zone-name
			calendar-standard-time-zone-name))
	 (time (+ rounded-abs-date
                  (if dst (/ calendar-daylight-time-offset 24.0 60.0) 0))))
    (list (calendar-gregorian-from-absolute (truncate time))
          (* 24.0 (- time (truncate time)))
          time-zone)))

(provide 'cal-dst)

;;; arch-tag: a141d204-213c-4ca5-bdc6-f9df3aa92aad
;;; cal-dst.el ends here
