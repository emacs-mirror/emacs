;;; cal-dst.el --- calendar functions for daylight savings rules.

;; Copyright (C) 1993 Free Software Foundation, Inc.

;; Author: Paul Eggert <eggert@twinsun.com>
;;	Edward M. Reingold <reingold@cs.uiuc.edu>
;; Keywords: calendar
;; Human-Keywords: daylight savings time, calendar, diary, holidays

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
;; holiday.el that deal with daylight savings time.

;; Comments, corrections, and improvements should be sent to
;;  Edward M. Reingold               Department of Computer Science
;;  (217) 333-6733                   University of Illinois at Urbana-Champaign
;;  reingold@cs.uiuc.edu             1304 West Springfield Avenue
;;                                   Urbana, Illinois 61801

;;; Code:

(require 'calendar)

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
	     (* 512 (mod h 675)) (floor u 675))
	  ;; (2^16 h +l) % (60*60*24)
	  (+ (* (mod u 675) 128) (floor l 128)))))

(defun calendar-time-from-absolute (abs-date s)
  "Time of absolute date ABS-DATE, S seconds after midnight.

Returns the pair (HIGH . LOW) where HIGH and LOW are the high and low
16 bits, respectively, of the number of seconds 1970-01-01 00:00:00 UTC,
ignoring leap seconds, that is the equivalent moment to S seconds after
midnight UTC on absolute date ABS-DATE."
  (let* ((a (- abs-date calendar-system-time-basis))
         (u (+ (* 163 (mod a 512)) (floor s 128))))
    ;; Overflow is a terrible thing!
    (cons
     ;; (60*60*24*a + s) / 2^16
     (+ a (* 163 (floor a 512)) (floor u 512))
     ;; (60*60*24*a + s) % 2^16
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
ABS-DIFF must specify a day that contains a daylight savings transition.
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
	   ;; Israel is special.
	   (if (zerop weekday)
	       (if (< m 7)
		   (list
		     '(calendar-gregorian-from-absolute
		       (calendar-dayname-on-or-before
			0
			(calendar-absolute-from-hebrew
			 (list 1 28 (+ year 3760))))))
		 (list '(calendar-gregorian-from-absolute
			 (calendar-dayname-on-or-before
			  0
			  (- (calendar-absolute-from-hebrew
			      (list 7 1 (+ year 3761))) 3))))))))
	 (prevday-sec (- -1 utc-diff)) ;; last sec of previous local day
	 last-surviving-rule
	 (i 1))
    ;; Scan through the next few years; take the rule that explains them best.
    (while (and candidate-rules (cdr candidate-rules) (<= i 28))
      (let ((year (+ y i))
	    new-rules)
	(while candidate-rules
	  (let* ((rule (car candidate-rules))
		 (date (calendar-absolute-from-gregorian (eval rule))))
	    (or (equal (current-time-zone
			(calendar-time-from-absolute date prevday-sec))
		       (current-time-zone
			(calendar-time-from-absolute (1+ date) prevday-sec)))
		(progn
		  (setq new-rules (cons rule new-rules))
		  (setq last-surviving-rule rule))))
	  (setq candidate-rules (cdr candidate-rules)))
	(setq candidate-rules (nreverse new-rules)))
      (setq i (1+ i)))
    last-surviving-rule))

(defun calendar-current-time-zone ()
  "Return UTC difference, dst offset, names and rules for current time zone.

Returns (UTC-DIFF DST-OFFSET STD-ZONE DST-ZONE DST-STARTS DST-ENDS DST-SWITCH),
based on a heuristic probing of what the system knows:

UTC-DIFF is an integer specifying the number of minutes difference between
    standard time in the current time zone and Coordinated Universal Time
    (Greenwich Mean Time).  A negative value means west of Greenwich.
DST-OFFSET is an integer giving the daylight savings time offset in minutes.
STD-ZONE is a string giving the name of the time zone when no seasonal time
    adjustment is in effect.
DST-ZONE is a string giving the name of the time zone when there is a seasonal
    time adjustment in effect.
DST-STARTS and DST-ENDS are sexps in the variable `year' giving the daylight
    savings time start rules, in the form expected by
    `calendar-daylight-savings-starts'.
DST-SWITCH is an integer giving the number of minutes after midnight that
    daylight savings time starts or ends.

If the local area does not use a seasonal time adjustment, DST-OFFSET and
DST-SWITCH are 0, STD-ZONE and DST-ZONE are equal, and DST-STARTS and DST-ENDS
are nil.

Some operating systems cannot provide all this information to Emacs; in this
case, `calendar-current-time-zone' returns a list containing nil for the data
it can't find."
  (or
   calendar-current-time-zone-cache
   (progn
     (message "Checking time zone data...")
     (setq
      calendar-current-time-zone-cache
      (let* ((now (current-time))
             (now-zone (current-time-zone now))
             (now-utc-diff (car now-zone))
             (now-name (car (cdr now-zone)))
             (next (calendar-next-time-zone-transition now)))
        (if (null next)
            (list (and now-utc-diff (/ now-utc-diff 60))
                  0 now-name now-name nil nil 0)
          (let* ((next-zone (current-time-zone next))
                 (next-utc-diff (car next-zone))
                 (next-name (car (cdr next-zone)))
                 (next-absdate-seconds
                  (calendar-absolute-from-time next now-utc-diff))
                 (next-transitions
                  (calendar-time-zone-daylight-rules
                   (car next-absdate-seconds) now-utc-diff))
                 (nextnext (calendar-next-time-zone-transition next))
                 (now-transitions
                  (calendar-time-zone-daylight-rules
                   (car (calendar-absolute-from-time nextnext next-utc-diff))
                   next-utc-diff))
                 (now-is-std (< now-utc-diff next-utc-diff)))
            (list (/ (min now-utc-diff next-utc-diff) 60)
                  (/ (abs (- now-utc-diff next-utc-diff)) 60)
                  (if now-is-std now-name next-name)
                  (if now-is-std next-name now-name)
                  (if now-is-std next-transitions now-transitions)
                  (if now-is-std now-transitions next-transitions)
                  (/ (cdr next-absdate-seconds) 60))))))
     (message "Checking time zone data...done")))
  calendar-current-time-zone-cache)

;;; The following six defvars relating to daylight savings time should NOT be
;;; marked to go into loaddefs.el where they would be evaluated when Emacs is
;;; dumped.  These variables' appropriate values depend on the conditions under
;;; which the code is INVOKED; so it's inappropriate to initialize them when
;;; Emacs is dumped---they should be initialized when calendar.el is loaded.

(calendar-current-time-zone)

(defvar calendar-time-zone (car calendar-current-time-zone-cache)
  "*Number of minutes difference between local standard time at
`calendar-location-name' and Coordinated Universal (Greenwich) Time.  For
example, -300 for New York City, -480 for Los Angeles.")

(defvar calendar-daylight-time-offset
  (car (cdr calendar-current-time-zone-cache))
  "*Number of minutes difference between daylight savings and standard time.
  
If the locale never uses daylight savings time, set this to 0.")

(defvar calendar-standard-time-zone-name
  (car (nthcdr 2 calendar-current-time-zone-cache))
  "*Abbreviated name of standard time zone at `calendar-location-name'.
For example, \"EST\" in New York City, \"PST\" for Los Angeles.")

(defvar calendar-daylight-time-zone-name
  (car (nthcdr 3 calendar-current-time-zone-cache))
  "*Abbreviated name of daylight-savings time zone at `calendar-location-name'.
For example, \"EDT\" in New York City, \"PDT\" for Los Angeles.")
  
(defvar calendar-daylight-savings-starts
  (car (nthcdr 4 calendar-current-time-zone-cache))
  "*Sexp giving the date on which daylight savings time starts.
This is an expression in the variable `year' whose value gives the Gregorian
date in the form (month day year) on which daylight savings time starts.  It is
used to determine the starting date of daylight savings time for the holiday
list and for correcting times of day in the solar and lunar calculations.

For example, if daylight savings time is mandated to start on October 1,
you would set `calendar-daylight-savings-starts' to

      '(10 1 year)

For a more complex example, daylight savings time begins in Israel on the
first Sunday after Passover ends on Nisan 21:

      '(calendar-gregorian-from-absolute
        (calendar-dayname-on-or-before
         0
         (calendar-absolute-from-hebrew (list 1 28 (+ year 3760)))))

because Nisan is the first month in the Hebrew calendar.

If the locale never uses daylight savings time, set this to nil.")

(defvar calendar-daylight-savings-ends
  (car (nthcdr 5 calendar-current-time-zone-cache))
  "*Sexp giving the date on which daylight savings time ends.
This is an expression in the variable `year' whose value gives the Gregorian
date in the form (month day year) on which daylight savings time ends.  It is
used to determine the starting date of daylight savings time for the holiday
list and for correcting times of day in the solar and lunar calculations.

For example, daylight savings time ends in Israel on the Sunday Selichot
begins:

      '(calendar-gregorian-from-absolute
        (calendar-dayname-on-or-before
         0
         (- (calendar-absolute-from-hebrew (list 7 1 (+ year 3761))) 3)))

If the locale never uses daylight savings time, set this to nil.")
  
(defvar calendar-daylight-savings-switchover-time
  (car (nthcdr 6 calendar-current-time-zone-cache))
  "*Number of minutes after midnight that daylight savings time begins/ends.
If the locale never uses daylight savings time, set this to 0.")

(provide 'cal-dst)

;;; cal-dst.el ends here
