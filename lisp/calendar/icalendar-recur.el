;;; icalendar-recur.el --- Support for iCalendar recurrences and time zones -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Richard Lawrence

;; Author: Richard Lawrence <rwl@recursewithless.net>
;; Created: December 2024
;; Keywords: calendar

;; This file is part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a sub-library for working with recurrence rules and time
;; zones, as defined by RFC5545 (see especially Secs. 3.3.10 and
;; 3.8.5.3, which are required reading before you make any changes to
;; the code below) and related standards (especially RFC8984 Sec. 4.3,
;; also strongly recommended reading).  Recurrence rules and time zones
;; are mutually dependent: to calculate the date and time of future
;; instances of a recurring event, you must be able to apply time zone
;; rules; and to apply time zone rules, you must be able to calculate
;; the date and time of recurring events, namely the shifts between
;; observances of standard and daylight savings time.  For example, an
;; event that occurs "on the last Friday of every month at 11AM" in a
;; given time zone should recur at 11AM daylight savings time in July,
;; but 11AM standard time in January, for a typical time zone that
;; shifts from standard to DST and back once each year.  These shifts
;; occur at, say, "the last Sunday in March at 2AM" and "the first
;; Sunday in November at 2AM".  So to calculate an absolute time for a
;; given instance of the original event, you first have to calculate the
;; nearest instance of the shift between standard and daylight savings
;; time, which itself involves applying a recurrence rule of the same
;; form.
;;
;; This mutual dependence between recurrence rules and time zones is not
;; a *vicious* circle, because the shifts between time zone observances
;; have fixed offsets from UTC time which are made explicit in iCalendar
;; data.  But it does make things complicated.  RFC5545 focuses on making
;; recurrence rules expressive enough to cover existing practices,
;; including time zone observance shifts, rather than on being easy to
;; implement.
;;
;; So be forewarned: here be dragons.  The code here was difficult to get
;; working, in part because this mutual dependence means it is difficult
;; to implement anything less than the whole system, in part because
;; recurrence rules are very flexible in order to cover as many
;; practical uses as possible, in part because time zone practices are
;; themselves complicated, and in part because there are a *lot* of edge
;; cases to worry about.  Much of it is tedious and repetitive but
;; doesn't lend itself to further simplification or abstraction.  If you
;; need to make changes, make them slowly, and use the tests in
;; test/lisp/calendar/icalendar-recur-tests.el to make sure they don't
;; break anything.
;;
;; Notation: `date/time' with a slash in symbol names means "`date' or
;; `date-time'", i.e., is a way of indicating that a function can
;; accept either type of value, and `dt' is typically used for an
;; argument of either type.  `date-time' should always refer to *just*
;; date-time values, not plain (calendar-style) dates.

;;; Code:
(require 'icalendar-ast)
(require 'icalendar-parser)
(require 'icalendar-utils)
(require 'cl-lib)
(require 'calendar)
(require 'cal-dst)
(require 'simple)
(require 'seq)
(eval-when-compile '(require 'icalendar-macs))


;; Recurrence Intervals
;;
;; Two important ideas in the following:
;;
;; 1) Because recurrence sets are potentially infinite, we always
;; calculate recurrences within certain upper and lower bounds.  These
;; bounds might be determined by a user interface (e.g. the week or
;; month displayed in a calendar) or might be derived from the logic of
;; the recurrence rule itself.  In the former case, where the bounds can
;; be arbitrary, it's called a 'window' here (as in "window of
;; time").  In the latter case, it's called an 'interval' here (after the
;; "INTERVAL=..." clause in recurrence rules).
;;
;; Unlike a window, an interval must be synced up with the recurrence
;; rule: its bounds must fall at successive integer multiples of the
;; product of the recurrence rule's FREQ and INTERVAL values, relative
;; to a starting date/time.  For example, a recurrence rule with a
;; MONTHLY frequency and INTERVAL=3 will have an interval that is three
;; months long.  If its start date is, e.g., in November, then the first
;; interval runs from November to February, the next from February to
;; May, and so on.  Because intervals depend only on the starting
;; date/time, the frequency, and the interval length, it is relatively
;; straightforward to compute the bounds of the interval surrounding an
;; arbitrary point in time (without enumerating them successively from
;; the start time); see `icalendar-recur-find-interval', which calls
;; this arbitrary point in time the 'target'.
;;
;; 2) An interval is the smallest unit of time for which we compute
;; values of the recurrence set.  This is because the "BYSETPOS=..."
;; clause in a recurrence rule operates on the sequence of recurrences
;; in a single interval.  Since it selects recurrences by their index in
;; this sequence, the sequence must have a determinate length and known
;; bounds.  The function `icalendar-recur-recurrences-in-interval' is the
;; main function to compute recurrences in a given interval.
;;
;; The way to compute the recurrences in an arbitrary *window* is thus
;; to find the interval bounds which are closest to the window's lower
;; and upper bound, and then compute the recurrences for all the
;; intervals in between, i.e., that "cover" the window.  This is what the
;; function `icalendar-recur-recurrences-in-window' does.
;;
;; Note that the recurrence set for a recurrence rule with a COUNT
;; clause cannot be computed for an arbitrary interval (or window);
;; instead, the set must be enumerated from the beginning, so that the
;; enumeration can stop after a fixed number of recurrences.  This is
;; what the function `icalendar-recur-recurrences-to-count' does.  But
;; also in this case, recurrences are generated for one interval at a
;; time, because a BYSETPOS clause might apply.
;;
;; An interval is represented as a list (LOW HIGH NEXT-LOW) of decoded
;; times.  The length of time between LOW and HIGH corresponds to the
;; FREQ rule part: they are one year apart for a 'YEARLY rule, a month
;; apart for a 'MONTHLY rule, etc.  NEXT-LOW is the upper bound of the
;; interval: it is equal to LOW in the subsequent interval.  When the
;; INTERVAL rule part is equal to 1 (the default), HIGH and NEXT-LOW are
;; the same, but if it is > 1, NEXT-LOW is equal to LOW + INTERVAL *
;; FREQ.  For example, in a 'MONTHLY rule where INTERVAL=3, which means
;; "every three months", LOW and HIGH bound the first month, while HIGH
;; and NEXT-LOW bound the following two months.
;;
;; The times between LOW and HIGH are candidates for recurrences.  LOW
;; is an inclusive lower bound, and HIGH is an exclusive upper bound:
;; LOW <= R < HIGH for each recurrence R in the interval.  The times
;; between HIGH and NEXT-LOW are not candidates for recurrences.
;;
;; The following functions deal with constructing intervals, given a
;; target, a start date/time, and intervalsize, and optionally a time
;; zone.  The main entry point is `icalendar-recur-find-interval'.

;; Look, dragons already:
(defun icr:find-absolute-interval (target dtstart intervalsize freqs
                                   &optional vtimezone)
  "Find a recurrence interval based on a fixed number of seconds.

INTERVALSIZE should be the total size of the interval in seconds.  FREQS
should be the number of seconds between the lower bound of the interval
and the upper bound for candidate recurrences; it is the number of
seconds in the unit of time in a recurrence rule's FREQ part.  The
returned interval looks like (LOW LOW+FREQS LOW+INTERVALSIZE).  See
`icalendar-recur-find-interval' for other arguments' meanings."
  ;; We assume here that the interval needs to be calculated using
  ;; absolute times for SECONDLY, MINUTELY, and HOURLY rules.
  ;; There are two reasons for this:
  ;;
  ;; 1) Time zone shifts.  If we don't use absolute times, and instead
  ;;    find interval boundaries using local clock times with e.g.
  ;;    `ical:date/time-add' (as we do with time units of a day or
  ;;    greater below), we have to adjust for clock time changes.  Using
  ;;    absolute times is simpler.
  ;; 2) More problematically, using local clock times, at least in its
  ;;    most straightforward implementation, has pathological results
  ;;    when `intervalsize' is relatively prime with 60 (for a SECONDLY
  ;;    rule, similarly for the others): intervals generated by
  ;;    successive enumeration from one target value will not in general
  ;;    align with intervals generated from a different, but nearby,
  ;;    target value.  (So going this route seems to mean giving up on
  ;;    the idea that intervals can be calculated just from `target',
  ;;    `dtstart' and `intervalsize', and instead always enumerating
  ;;    them from the beginning.)
  ;;
  ;; In effect, we are deciding that a rule like "every 3 hours" always
  ;; means every 3 * 60 * 60 = 10800 seconds after `dtstart', and not
  ;; "every 10800 seconds, except when there's a time zone observance
  ;; change".  People who want the latter have another option: use a
  ;; DAILY rule and specify the (local) times for the hours they want in
  ;; the BYHOUR clause, etc.  (People who want it for a number of hours,
  ;; e.g. 7, which does not divide 24, unfortunately do *not* have this
  ;; option, but anyone who wants that but does not want to understand
  ;; "7 hours" as a fixed number of seconds has a pathology that I
  ;; cannot cure here.)
  ;;
  ;; RFC5545 does not seem to pronounce one way or the other on whether
  ;; this decision is correct: there are no examples of SECONDLY rules
  ;; to go on, and the few examples for MINUTELY and HOURLY rules only
  ;; use "nice" values in the INTERVAL clause (real-life examples
  ;; probably(?)  will too).  Our assumption has some possibly
  ;; unintuitive consequences for `intervalsize' values that are not
  ;; "nice" (basically, whenever intervalsize and either 60 or 24 are
  ;; relatively prime), and for how interval boundaries behave at the
  ;; shifts between time zone observances (since local clock times in
  ;; the interval bounds will shift from what they would have been
  ;; before the observance change -- arguably correct but possibly
  ;; surprising, depending on the case).  But the alternative seems
  ;; worse, so until countervailing evidence emerges, this approach
  ;; seems reasonable.
  (let* ((given-start-zone (decoded-time-zone dtstart))
         (start-w/zone (cond (given-start-zone dtstart)
                             ((ical:vtimezone-component-p vtimezone)
                              (ical:date-time-variant dtstart :tz vtimezone))
                             (t
                              ;; "Floating" time should be interpreted in user's
                              ;; current time zone; see RFC5545 Sec 3.3.5
                              (ical:date-time-variant
                               dtstart :zone (car (current-time-zone))))))
         (start-abs (ignore-errors
                      (time-convert (encode-time start-w/zone) 'integer)))
         (given-target-zone (decoded-time-zone target))
         (target-w/zone (cond (given-target-zone target)
                              (vtimezone
                               (ical:date-time-variant target :tz vtimezone))
                              (t
                               (ical:date-time-variant
                                target :zone (car (current-time-zone))))))
         (target-abs (ignore-errors
                         (time-convert (encode-time target-w/zone) 'integer)))
         low-abs low high next-low)

    (unless (zerop (mod intervalsize freqs))
      ;; Bad things will happen if intervalsize is not an integer
      ;; multiple of freqs
      (error "FREQS=%d does not divide INTERVALSIZE=%d" freqs intervalsize))
    (unless (and start-abs target-abs)
      (when (not start-abs)
        (error "Could not determine an offset for DTSTART=%s" dtstart))
      (when (not target-abs)
        (error "Could not determine an offset for TARGET=%s" target)))

    ;; Find the lower bound below target that is the closest integer
    ;; multiple of intervalsize seconds from dtstart
    (setq low-abs (- target-abs
                     (mod (- target-abs start-abs) intervalsize)))

    (if vtimezone
        (setq low (icr:tz-decode-time low-abs vtimezone)
              high (icr:tz-decode-time (+ low-abs freqs) vtimezone)
              next-low (icr:tz-decode-time (+ low-abs intervalsize) vtimezone))
      ;; best we can do is decode into target's zone:
      (let ((offset (decoded-time-zone target-w/zone)))
        (setq low (icr:tz-decode-time low-abs offset)
              high (icr:tz-decode-time (+ low-abs freqs) offset)
              next-low (icr:tz-decode-time (+ low-abs intervalsize) offset))))

    (unless (and given-start-zone given-target-zone)
      ;; but if we started with floating times, we should return floating times:
      (setf (decoded-time-zone low) nil)
      (setf (decoded-time-dst low) -1)
      (setf (decoded-time-zone high) nil)
      (setf (decoded-time-dst high) -1)
      (setf (decoded-time-zone next-low) nil)
      (setf (decoded-time-dst next-low) -1))

    (list low high next-low)))

(defun icr:find-secondly-interval (target dtstart intervalsize &optional vtimezone)
  "Find a SECONDLY recurrence interval.
See `icalendar-recur-find-interval' for arguments' meanings."
  (icr:find-absolute-interval
   target
   dtstart
   intervalsize
   1
   vtimezone))

(defun icr:find-minutely-interval (target dtstart intervalsize &optional vtimezone)
  "Find a MINUTELY recurrence interval.
See `icalendar-recur-find-interval' for arguments' meanings."
  (icr:find-absolute-interval
   target
   ;; A MINUTELY interval always runs from the beginning of a minute to
   ;; the beginning of the next minute:
   (ical:date-time-variant dtstart :second 0 :tz 'preserve)
   (* 60 intervalsize)
   60
   vtimezone))

(defun icr:find-hourly-interval (target dtstart intervalsize &optional vtimezone)
  "Find an HOURLY recurrence interval.
See `icalendar-recur-find-interval' for arguments' meanings."
  (icr:find-absolute-interval
   target
   ;; An HOURLY interval always runs from the beginning of an hour to
   ;; the beginning of the next hour:
   (ical:date-time-variant dtstart :minute 0 :second 0 :tz 'preserve)
   (* 60 60 intervalsize)
   (* 60 60)
   vtimezone))

(defun icr:find-daily-interval (target dtstart intervalsize &optional vtimezone)
  "Find a DAILY recurrence interval.
See `icalendar-recur-find-interval' for arguments' meanings."
  (let* ((start-absdate (calendar-absolute-from-gregorian
                         (ical:date/time-to-date dtstart)))
         (target-absdate (calendar-absolute-from-gregorian
                          (ical:date/time-to-date target)))
         ;; low-absdate is the closest absolute date below target that
         ;; is an integer multiple of intervalsize days from dtstart
         (low-absdate (- target-absdate
                         (mod (- target-absdate start-absdate) intervalsize)))
         (high-absdate (1+ low-absdate))
         (next-low-absdate (+ low-absdate intervalsize)))

    (let* ((low-dt (ical:date-to-date-time
                     (calendar-gregorian-from-absolute low-absdate)))
           (high-dt (ical:date-to-date-time
                      (calendar-gregorian-from-absolute high-absdate)))
           (next-low-dt (ical:date-to-date-time
                          (calendar-gregorian-from-absolute next-low-absdate))))

      (when vtimezone
        (icr:tz-set-zone low-dt vtimezone)
        (icr:tz-set-zone high-dt vtimezone)
        (icr:tz-set-zone next-low-dt vtimezone))

      ;; Return the bounds:
      (list low-dt high-dt next-low-dt))))

(defun icr:find-weekly-interval (target dtstart intervalsize
                                 &optional weekstart vtimezone)
  "Find a WEEKLY recurrence interval.
See `icalendar-recur-find-interval' for arguments' meanings."
  (let* ((target-date (ical:date/time-to-date target))
         (start-date (ical:date/time-to-date dtstart))
         ;; the absolute dates of the week start before target and
         ;; dtstart; these are always a whole number of weeks apart:
         (target-week-abs (calendar-nth-named-absday
                           -1
                           (or weekstart 1)
                           (calendar-extract-month target-date)
                           (calendar-extract-year target-date)
                           (calendar-extract-day target-date)))
         (start-abs (calendar-nth-named-absday
                     -1
                     (or weekstart 1)
                     (calendar-extract-month start-date)
                     (calendar-extract-year start-date)
                     (calendar-extract-day start-date)))
         (intsize-days (* 7 intervalsize))
         ;; the absolute date of the week start before target which is
         ;; an integer multiple of intervalsize weeks from dtstart:
         (low-abs (- target-week-abs
                  (mod (- target-week-abs start-abs) intsize-days)))
         ;; then use this to find the interval bounds:
         (low (ical:date-to-date-time
               (calendar-gregorian-from-absolute low-abs)))
         (high (ical:date-to-date-time
               (calendar-gregorian-from-absolute (+ 7 low-abs))))
         (next-low (ical:date-to-date-time
                    (calendar-gregorian-from-absolute (+ intsize-days low-abs)))))

    (when vtimezone
      (icr:tz-set-zone low vtimezone)
      (icr:tz-set-zone high vtimezone)
      (icr:tz-set-zone next-low vtimezone))

    ;; Return the bounds:
    (list low high next-low)))

(defun icr:find-monthly-interval (target dtstart intervalsize &optional vtimezone)
  "Find a MONTHLY recurrence interval.
See `icalendar-recur-find-interval' for arguments' meanings."
  (let* ((start-month (ical:date/time-month dtstart))
         (start-year (ical:date/time-year dtstart))
         ;; we calculate in "absolute months", i.e., number of months
         ;; since the beginning of the Gregorian calendar, to make
         ;; finding the lower bound easier:
         (start-abs-months (+ (* 12 (1- start-year)) (1- start-month)))
         (target-month (ical:date/time-month target))
         (target-year (ical:date/time-year target))
         (target-abs-months (+ (* 12 (1- target-year)) (1- target-month)))
         ;; number of "absolute months" between start of dtstart's month
         ;; and start of target's month:
         (nmonths (- target-abs-months start-abs-months))
         ;; the number of months after dtstart that is the closest integer
         ;; multiple of intervalsize months before target:
         (lmonths (- nmonths (mod nmonths intervalsize)))
         ;; convert these "absolute months" back to Gregorian month and year:
         (mod-month (mod (+ start-month lmonths) 12))
         (low-month (if (zerop mod-month) 12 mod-month))
         (low-year (+ (/ lmonths 12) start-year
                      ;; iff we cross a year boundary moving forward in
                      ;; time from start-month to target-month, we need
                      ;; to add one to the year:
                      (if (<= start-month target-month) 0 1)))
         ;; and now we can use these to calculate the interval bounds:
         (low (ical:make-date-time :year low-year :month low-month :day 1
                                   :hour 0 :minute 0 :second 0 :tz vtimezone))
         (high (ical:date/time-add low :month 1 vtimezone))
         (next-low (ical:date/time-add low :month intervalsize vtimezone)))

    ;; Return the bounds:
    (list low high next-low)))

(defun icr:find-yearly-interval (target dtstart intervalsize &optional vtimezone)
  "Find a YEARLY recurrence interval.
See `icalendar-recur-find-interval' for arguments' meanings."
  (let* ((start-year (ical:date/time-year dtstart))
         (target-year (ical:date/time-year target))
         ;; The year before target that is the closest integer multiple
         ;; of intervalsize years after dtstart:
         (low-year (- target-year
                      (mod (- target-year start-year) intervalsize)))
         (low (ical:make-date-time :year low-year :month 1 :day 1
                                   :hour 0 :minute 0 :second 0 :tz vtimezone))
         (high (ical:make-date-time :year (1+ low-year) :month 1 :day 1
                                    :hour 0 :minute 0 :second 0 :tz vtimezone))
         (next-low (ical:make-date-time :year (+ low-year intervalsize)
                                        :month 1 :day 1 :hour 0 :minute 0 :second 0
                                        :tz vtimezone)))

    ;; Return the bounds:
    (list low high next-low)))

(defun icr:find-interval (target dtstart recur-value &optional vtimezone)
  "Return the recurrence interval around TARGET.

TARGET and DTSTART should be `icalendar-date' or `icalendar-date-time'
values.  RECUR-VALUE should be an `icalendar-recur'.

The returned value is a list (LOW HIGH NEXT-LOW) which
represents the lower and upper bounds of a recurrence interval around
TARGET.  For some N, LOW is equal to START + N*INTERVALSIZE units, HIGH
is equal to START + (N+1)*INTERVALSIZE units, and LOW <= TARGET < HIGH.
START here is a time derived from DTSTART depending on RECUR-VALUE's
FREQ part: the first day of the year for a \\='YEARLY rule, first day
of the month for a \\='MONTHLY rule, etc.

RECUR-VALUE's interval determines INTERVALSIZE, and its frequency
determines the units: a month for \\='MONTHLY, etc.

If VTIMEZONE is provided, it is used to set time zone information in the
returned interval bounds.  Otherwise, the bounds contain no time zone
information and represent floating local times."
  (let ((freq (ical:recur-freq recur-value))
        (intsize (ical:recur-interval-size recur-value))
        (weekstart (ical:recur-weekstart recur-value)))
    (cl-case freq
      (SECONDLY (icr:find-secondly-interval target dtstart intsize vtimezone))
      (MINUTELY (icr:find-minutely-interval target dtstart intsize vtimezone))
      (HOURLY (icr:find-hourly-interval target dtstart intsize vtimezone))
      (DAILY (icr:find-daily-interval target dtstart intsize vtimezone))
      (WEEKLY (icr:find-weekly-interval target dtstart intsize
                                        weekstart vtimezone))
      (MONTHLY (icr:find-monthly-interval target dtstart intsize vtimezone))
      (YEARLY (icr:find-yearly-interval target dtstart intsize vtimezone)))))

(defun icr:nth-interval (n dtstart recur-value &optional vtimezone)
  "Return the Nth recurrence interval after DTSTART.

The returned value is a list (LOW HIGH NEXT-LOW) which represent the Nth
recurrence interval after DTSTART.  LOW is equal to START +
N*INTERVALSIZE units, HIGH is equal to START + (N+1)*INTERVALSIZE units,
and LOW <= TARGET < HIGH.  START here is a time derived from DTSTART
depending on RECUR-VALUE's FREQ part: the first day of the year for a
\\='YEARLY rule, first day of the month for a \\='MONTHLY rule, etc.

RECUR-VALUE's interval determines INTERVALSIZE, and its frequency
determines the units: a month for \\='MONTHLY, etc.

N should be a non-negative integer.  Interval 0 is the interval
containing DTSTART.  DTSTART should be an `icalendar-date' or
`icalendar-date-time' value.  RECUR-VALUE should be an
`icalendar-recur'.

If VTIMEZONE is provided, it is used to set time zone information in the
returned interval bounds.  Otherwise, the bounds contain no time zone
information and represent floating local times."
  (when (< n 0) (error "Recurrence interval undefined for negative N"))
  (let* ((start-dt (if (cl-typep dtstart 'ical:date)
                       (ical:date-to-date-time dtstart :tz vtimezone)
                     dtstart))
         (freq (ical:recur-freq recur-value))
         (intervalsize (ical:recur-interval-size recur-value))
         (unit (cl-case freq
                 (YEARLY :year)
                 (MONTHLY :month)
                 (WEEKLY :week)
                 (DAILY :day)
                 (HOURLY :hour)
                 (MINUTELY :minute)
                 (SECONDLY :second)))
         (target (ical:date/time-add start-dt unit (* n intervalsize) vtimezone)))
    (icr:find-interval target dtstart recur-value vtimezone)))

(defun icr:next-interval (interval recur-value &optional vtimezone)
  "Return the next recurrence interval after INTERVAL.

Given a recurrence interval (LOW HIGH NEXT), returns the next interval
\(NEXT HIGHER HIGHER-NEXT), where HIGHER and HIGHER-NEXT are determined
by the frequency and interval sizes of RECUR-VALUE."
  (let* ((new-low (caddr interval))
         (freq (ical:recur-freq recur-value))
         (unit (cl-case freq
                 (YEARLY :year)
                 (MONTHLY :month)
                 (WEEKLY :week)
                 (DAILY :day)
                 (HOURLY :hour)
                 (MINUTELY :minute)
                 (SECONDLY :second)))
         (intervalsize (ical:recur-interval-size recur-value))
         (new-high (ical:date/time-add new-low unit 1 vtimezone))
         (new-next (ical:date/time-add new-low unit intervalsize vtimezone)))

    (when vtimezone
      (icr:tz-set-zone new-low vtimezone)
      ;; (icr:tz-set-zone new-high vtimezone)
      ;; (icr:tz-set-zone new-next vtimezone)
      )

    (list new-low new-high new-next)))

(defun icr:previous-interval (interval recur-value dtstart &optional vtimezone)
  "Given a recurrence INTERVAL, return the previous interval.

For an interval (LOW HIGH NEXT-LOW), the previous interval is
\(PREV-LOW PREV-HIGH LOW), where PREV-LOW and PREV-HIGH are determined by
the frequency and interval sizes of RECUR-VALUE (see
`icalendar-recur-find-interval').  If the resulting period of time
between PREV-LOW and PREV-HIGH occurs entirely before DTSTART, then the
interval does not exist; in this case nil is returned."
  (let* ((upper (car interval))
         (freq (ical:recur-freq recur-value))
         (unit (cl-case freq
                 (YEARLY :year)
                 (MONTHLY :month)
                 (WEEKLY :week)
                 (DAILY :day)
                 (HOURLY :hour)
                 (MINUTELY :minute)
                 (SECONDLY :second)))
         (intervalsize (ical:recur-interval-size recur-value))
         (new-low (ical:date/time-add upper unit (* -1 intervalsize) vtimezone))
         (new-high (ical:date/time-add new-low unit 1 vtimezone)))

    (when vtimezone
      ;; (icr:tz-set-zone new-low vtimezone)
      ;; (icr:tz-set-zone new-high vtimezone)
      (icr:tz-set-zone upper vtimezone))

    (unless (ical:date-time< new-high dtstart)
      (list new-low new-high upper))))



;; Refining intervals into subintervals
;;
;; For a given interval, the various BY*=... clauses in a recurrence
;; rule specify the recurrences in that interval.
;;
;; RFC5545 unfortunately has an overly-complicated conceptual model for
;; how recurrences are to be calculated which is based on "expanding" or
;; "limiting" the recurrence set for each successive clause.  This model
;; is difficult to think about and implement, and the text of the
;; standard is ambiguous.  I did not succeed in producing a working
;; implementation based on the description in the standard, and the
;; existing implementations don't seem to agree on how it's to be
;; implemented anyway.
;;
;; Fortunately, RFC8984 (JSCalendar) is a forthcoming standard which
;; attempts to resolve the ambiguities while being semantically
;; backward-compatible with RFC5545.  It provides a much cleaner
;; conceptual model: the recurrence set is generated by starting with a
;; list of candidates, which consist of every second in (what is here
;; called) an interval, and then filtering out any candidates which do
;; not match the rule's clauses.  The most straightforward implementation
;; of this model, however, is unusably slow in typical cases.  Consider
;; for example the case of calculating the onset of daylight savings
;; time in a given year: the interval is a year long, so it consists of
;; over 31 million seconds.  Although it's easy to generate Lisp
;; timestamps for each of those seconds, filtering them through the
;; various BY* clauses means decoding each of those timestamps, which
;; means doing a fairly expensive computation over 31 million times, and
;; then throwing away the result in all but one case.  When I implemented
;; this model, I was not patient enough to sit through the calculations
;; for even MONTHLY rules (which on my laptop took minutes).
;;
;; So instead of implementing RFC8984's model directly, the strategy
;; here is to do something equivalent but much more efficient: rather
;; than thinking of an interval as consisting of a set of successive
;; seconds, we think of it as described by its bounds; and for each BY*
;; clause, we *refine* the interval into subintervals by computing the
;; bounds of each subinterval corresponding to the value(s) in that
;; clause.  For example, in a YEARLY rule, the initial interval is one
;; year long, say all of 2025.  If it has a "BYMONTH=4,10" clause, then
;; we refine this interval into two subintervals, each one month long:
;; one for April 2025 and one for October 2025.  This is much more
;; efficient in the typical case, because the number of bounds which
;; describe the final set of subintervals is usually *much* smaller than
;; the number of seconds in the original interval.
;;
;; The following functions are responsible for computing these
;; refinements.  The main entry point here is
;; `icalendar-recur-refine-from-clauses', which takes care of
;; successively refining the interval both by the explicit values in the
;; rule's clauses and by the implicit values in DTSTART.  (There, too,
;; RFC8984 is helpful: it gives a much more explicit description of how
;; the information in DTSTART interacts with the BY* clauses to further
;; refine the subintervals.)

(defun icr:refine-byyearday (interval yeardays &optional vtimezone)
  "Resolve INTERVAL into a list of subintervals matching YEARDAYS.

YEARDAYS should be a list of values from a recurrence rule's
BYYEARDAY=... clause; see `icalendar-recur' for the possible values."
  (let* ((sorted-ydays (sort yeardays
                             :lessp (lambda (a b)
                                      (let ((pos-a (if (< 0 a) a (+ 366 a)))
                                            (pos-b (if (< 0 b) b (+ 366 b))))
                                        (< pos-a pos-b)))))
         (interval-start (car interval))
         (start-year (decoded-time-year interval-start))
         (interval-end (cadr interval))
         (end-year (decoded-time-year interval-end))
         (subintervals nil))
    (while (<= start-year end-year)
      ;; For each year in the interval...
      (dolist (n sorted-ydays)
        ;; ...the subinterval is one day long on the nth yearday
        (let* ((nthday (calendar-date-from-day-of-year start-year n))
               (low (ical:make-date-time :year start-year
                                         :month (calendar-extract-month nthday)
                                         :day (calendar-extract-day nthday)
                                         :hour 0 :minute 0 :second 0
                                         :tz vtimezone))
               (high (ical:date/time-add low :day 1 vtimezone)))
          ;; "Clip" the subinterval bounds if they fall outside the
          ;; interval.  Careful!  This clipping can lead to high <= low,
          ;; so need to check it is still the case that low < high
          ;; before pushing the subinterval
          (when (ical:date/time< low interval-start)
            (setq low interval-start))
          (when (ical:date/time< interval-end high)
            (setq high interval-end))
          (when (and (ical:date-time<= interval-start low)
                     (ical:date-time< low high)
                     (ical:date-time<= high interval-end))
            (push (list low high) subintervals))))

      (setq start-year (1+ start-year)))
    (nreverse subintervals)))

(defun icr:refine-byweekno (interval weeknos &optional weekstart vtimezone)
  "Resolve INTERVAL into a list of subintervals matching WEEKNOS.

WEEKNOS should be a list of values from a recurrence rule's
BYWEEKNO=... clause, and WEEKSTART should be the value of its
WKST=... clause (if any).  See `icalendar-recur' for the possible values."
  (let* ((sorted-weeknos (sort weeknos
                               :lessp (lambda (a b)
                                        (let ((pos-a (if (< 0 a) a (+ 53 a)))
                                              (pos-b (if (< 0 b) b (+ 53 b))))
                                          (< pos-a pos-b)))))
         (interval-start (car interval))
         (start-year (decoded-time-year interval-start))
         (interval-end (cadr interval))
         (end-year (decoded-time-year interval-end))
         (subintervals nil))
    (while (<= start-year end-year)
      ;; For each year in the interval...
      (dolist (wn sorted-weeknos)
        ;; ...the subinterval is one week long in the wn-th week
        (let* ((nth-wstart (ical:start-of-weekno wn start-year weekstart))
               (low (ical:make-date-time :year (calendar-extract-year nth-wstart)
                                         :month (calendar-extract-month nth-wstart)
                                         :day (calendar-extract-day nth-wstart)
                                         :hour 0 :minute 0 :second 0
                                         :tz vtimezone))
               (high (ical:date/time-add low :day 7 vtimezone)))
          ;; "Clip" the subinterval bounds if they fall outside the
          ;; interval, as above.  This can happen often here because week
          ;; boundaries generally do not align with year boundaries.
          (when (ical:date/time< low interval-start)
            (setq low interval-start))
          (when (ical:date/time< interval-end high)
            (setq high interval-end))
          (when (and (ical:date-time<= interval-start low)
                     (ical:date-time< low high)
                     (ical:date-time<= high interval-end))
              (push (list low high) subintervals))))
      (setq start-year (1+ start-year)))
    (nreverse subintervals)))

(defun icr:refine-bymonth (interval months &optional vtimezone)
  "Resolve INTERVAL into a list of subintervals matching MONTHS.

MONTHS should be a list of values from a recurrence rule's
BYMONTH=... clause; see `icalendar-recur' for the possible values."
  (let* ((sorted-months (sort months))
         (interval-start (car interval))
         (start-year (decoded-time-year interval-start))
         (interval-end (cadr interval))
         (end-year (decoded-time-year interval-end))
         (subintervals nil))
    (while (<= start-year end-year)
      ;; For each year in the interval...
      (dolist (m sorted-months)
        ;; ...the subinterval is from the first day of the given month
        ;; to the first day of the next
        (let* ((low (ical:make-date-time :year start-year :month m :day 1
                                         :hour 0 :minute 0 :second 0
                                         :tz vtimezone))
               (high (ical:date/time-add low :month 1 vtimezone)))

          ;; Clip the subinterval bounds, as above
          (when (ical:date/time< low interval-start)
            (setq low interval-start))
          (when (ical:date/time< interval-end high)
            (setq high interval-end))
          (when (and (ical:date/time<= interval-start low)
                     (ical:date/time< low high)
                     (ical:date/time<= high interval-end))
            (push (list low high) subintervals))))
      (setq start-year (1+ start-year)))

    (nreverse subintervals)))

(defun icr:refine-bymonthday (interval monthdays &optional vtimezone)
  "Resolve INTERVAL into a list of subintervals matching MONTHDAYS.

MONTHDAYS should be a list of values from a recurrence rule's
BYMONTHDAY=... clause; see `icalendar-recur' for the possible values."
  (let* ((sorted-mdays (sort monthdays
                             :lessp (lambda (a b)
                                      (let ((pos-a (if (< 0 a) a (+ 31 a)))
                                            (pos-b (if (< 0 b) b (+ 31 b))))
                                        (< pos-a pos-b)))))
         (interval-start (car interval))
         (interval-end (cadr interval))
         (subintervals nil))
    (while (ical:date-time<= interval-start interval-end)
      ;; For each month in the interval...
      (dolist (m sorted-mdays)
        ;; ...the subinterval is one day long on the given monthday
        (let* ((month (ical:date/time-month interval-start))
               (year (ical:date/time-year interval-start))
               (monthday (if (< 0 m) m
                           (+ m 1 (calendar-last-day-of-month month year))))
               (low (ical:date-time-variant interval-start :day monthday
                                            :hour 0 :minute 0 :second 0
                                            :tz vtimezone))
               (high (ical:date/time-add low :day 1 vtimezone)))

          (ignore-errors ; ignore invalid dates, e.g. 2025-02-29
            ;; Clip subinterval, as above
            (when (ical:date/time< low interval-start)
              (setq low interval-start))
            (when (ical:date/time< interval-end high)
              (setq high interval-end))
            (when (and (ical:date/time<= interval-start low)
                       (ical:date/time< low high)
                       (ical:date/time<= high interval-end))
              (push (list low high) subintervals)))))
      (setq interval-start
            (ical:date/time-add interval-start :month 1 vtimezone)))
    (nreverse subintervals)))

(defun icr:refine-byday (interval weekdays &optional in-month vtimezone)
  "Refine INTERVAL to days matching the given WEEKDAYS.

WEEKDAYS should be a list of values from a recurrence rule's
BYDAY=... clause; see `icalendar-recur' for the possible values.

If WEEKDAYS contains pairs (DOW . OFFSET), then IN-MONTH indicates
whether OFFSET is relative to the month of the start of the interval.  If
it is nil, OFFSET will be relative to the year, rather than the month."
  (let* ((sorted-weekdays (sort (seq-filter #'natnump weekdays)))
         (with-offsets (sort (seq-filter #'consp weekdays)
                             :lessp (lambda (w1 w2) (and (< (car w1) (car w2))))))
         (interval-start (car interval))
         (start-abs (calendar-absolute-from-gregorian
                     (ical:date-time-to-date interval-start)))
         (interval-end (cadr interval))
         (end-abs (calendar-absolute-from-gregorian
                   (ical:date-time-to-date interval-end)))
         (subintervals nil))

    ;; For days where an offset was given, the subinterval is a single
    ;; weekday relative to the month or year of interval-start:
    (dolist (wo with-offsets)
      (let* ((dow (car wo))
             (offset (cdr wo))
             (low-date
              (ical:nth-weekday-in offset dow
                                   (ical:date/time-year interval-start)
                                   (when in-month
                                     (ical:date/time-month interval-start))))
             (low (ical:date-to-date-time low-date :tz vtimezone))
             (high (ical:date/time-add low :day 1 vtimezone)))
        (when (ical:date/time< low interval-start)
          (setq low interval-start))
        (when (ical:date/time< interval-end high)
          (setq high interval-end))
        (when vtimezone
          (icr:tz-set-zone low vtimezone)
          (icr:tz-set-zone high vtimezone))
        (when (and (ical:date/time<= interval-start low)
                   (ical:date/time<= high interval-end)
                   (ical:date/time< low high))
          (push (list low high) subintervals))))

    ;; When no offset was given, for each day in the interval...
    (while (and (<= start-abs end-abs)
                sorted-weekdays)
      ;; ...the subinterval is one day long on matching weekdays.
      (let* ((gdate (calendar-gregorian-from-absolute start-abs)))
        (when (memq (calendar-day-of-week gdate) sorted-weekdays)
          (let* ((low (ical:date-to-date-time gdate))
                 (high (ical:date/time-add low :day 1 vtimezone)))
            (when (ical:date/time< low interval-start)
              (setq low interval-start))
            (when (ical:date/time< interval-end high)
              (setq high interval-end))
            (when vtimezone
              (icr:tz-set-zone low vtimezone)
              (icr:tz-set-zone high vtimezone))
            (when (and (ical:date/time<= interval-start low)
                       (ical:date/time<= high interval-end)
                       (ical:date/time< low high))
              (push (list low high) subintervals)))))
      (setq start-abs (1+ start-abs)))

    ;; Finally, sort and return all subintervals:
    (sort subintervals
          :lessp (lambda (int1 int2)
                   (ical:date-time< (car int1) (car int2)))
          :in-place t)))

(defun icr:refine-byhour (interval hours &optional vtimezone)
  "Resolve INTERVAL into a list of subintervals matching HOURS.

HOURS should be a list of values from a recurrence rule's
BYHOUR=... clause; see `icalendar-recur' for the possible values."
  (let* ((sorted-hours (sort hours))
         (interval-start (car interval))
         (interval-end (cadr interval))
         (subintervals nil))
    (while (ical:date-time<= interval-start interval-end)
      ;; For each day in the interval...
      (dolist (h sorted-hours)
        ;; ...the subinterval is one hour long in the given hour
        (let* ((low (ical:date-time-variant interval-start
                                            :hour h :minute 0 :second 0
                                            :tz vtimezone))
               (high (ical:date/time-add low :hour 1 vtimezone)))
          (ignore-errors ; do not generate subintervals for nonexisting times
            (when (ical:date/time< low interval-start)
              (setq low interval-start))
            (when (ical:date/time< interval-end high)
              (setq high interval-end))
            (when (and (ical:date/time<= interval-start low)
                       (ical:date/time< low high)
                       (ical:date/time<= high interval-end))
              (push (list low high) subintervals)))))
      (setq interval-start (ical:date/time-add interval-start :day 1 vtimezone)))
    (nreverse subintervals)))

(defun icr:refine-byminute (interval minutes &optional vtimezone)
  "Resolve INTERVAL into a list of subintervals matching MINUTES.

MINUTES should be a list of values from a recurrence rule's
BYMINUTE=... clause; see `icalendar-recur' for the possible values."
  (let* ((sorted-minutes (sort minutes))
         (interval-start (car interval))
         (interval-end (cadr interval))
         ;; we use absolute times (in seconds) for the loop variables in
         ;; case the interval crosses the boundary between two observances:
         (low-ts (time-convert (encode-time interval-start) 'integer))
         (end-ts (time-convert (encode-time interval-end) 'integer))
         (subintervals nil))
    (while (<= low-ts end-ts)
      ;; For each hour in the interval...
      (dolist (m sorted-minutes)
        ;; ...the subinterval is one minute long in the given minute
        (let* ((low (ical:date-time-variant interval-start :minute m :second 0
                                            :tz vtimezone))
               (high (ical:date/time-add low :minute 1 vtimezone)))
          (ignore-errors ; do not generate subintervals for nonexisting times
            ;; Clip the subinterval, as above
            (when (ical:date/time< low interval-start)
              (setq low interval-start))
            (when (ical:date/time< interval-end high)
              (setq high interval-end))
            (when (and (ical:date/time<= interval-start low)
                       (ical:date/time< low high)
                       (ical:date/time<= high interval-end))
              (push (list low high) subintervals)))))
      (setq low-ts (+ low-ts (* 60 60))
            interval-start (if vtimezone (icr:tz-decode-time low-ts vtimezone)
                             (ical:date/time-add interval-start :hour 1))))
    (nreverse subintervals)))

(defun icr:refine-bysecond (interval seconds &optional vtimezone)
  "Resolve INTERVAL into a list of subintervals matching SECONDS.

SECONDS should be a list of values from a recurrence rule's
BYSECOND=... clause; see `icalendar-recur' for the possible values."
  (let* ((sorted-seconds (sort seconds))
         (interval-start (car interval))
         (interval-end (cadr interval))
         ;; we use absolute times (in seconds) for the loop variables in
         ;; case the interval crosses the boundary between two observances:
         (low-ts (time-convert (encode-time interval-start) 'integer))
         (end-ts (time-convert (encode-time interval-end) 'integer))
         (subintervals nil))
    (while (<= low-ts end-ts)
      ;; For each minute in the interval...
      (dolist (s sorted-seconds)
        ;; ...the subinterval is one second long: the given second
        (let* ((low (ical:date-time-variant interval-start :second s
                                            :tz vtimezone))
               (high (ical:date/time-add low :second 1 vtimezone)))
          (when (ical:date/time< low interval-start)
            (setq low interval-start))
          (when (ical:date/time< interval-end high)
            (setq high interval-end))
          (when (and (ical:date/time<= interval-start low)
                     (ical:date/time< low high)
                     (ical:date/time<= high interval-end))
            (push (list low high) subintervals))))
      (setq low-ts (+ low-ts 60)
            interval-start (if vtimezone
                               (icr:tz-decode-time low-ts vtimezone)
                             (ical:date/time-add interval-start :minute 1))))
    (nreverse subintervals)))

;; TODO: should this just become a generic function, with the above
;; refine-by* functions becoming its methods?
(defun icr:refine-by (unit interval values
                      &optional byday-inmonth weekstart vtimezone)
  "Resolve INTERVAL into a list of subintervals matching VALUES for UNIT."
  (cl-case unit
    (BYYEARDAY (icr:refine-byyearday interval values vtimezone))
    (BYWEEKNO (icr:refine-byweekno interval values weekstart vtimezone))
    (BYMONTH (icr:refine-bymonth interval values vtimezone))
    (BYMONTHDAY (icr:refine-bymonthday interval values vtimezone))
    (BYDAY (icr:refine-byday interval values byday-inmonth vtimezone))
    (BYHOUR (icr:refine-byhour interval values vtimezone))
    (BYMINUTE (icr:refine-byminute interval values vtimezone))
    (BYSECOND (icr:refine-bysecond interval values vtimezone))))

(defun icr:make-bysetpos-filter (setpos)
  "Return a filter on values for the indices in SETPOS.

SETPOS should be a list of positive or negative integers between -366
and 366, indicating a fixed index in a set of recurrences for *one
interval* of a recurrence set, as found in the BYSETPOS=...  clause of
an `icalendar-recur'.  For example, in a YEARLY recurrence rule with an
INTERVAL of 1, the SETPOS represent indices in the recurrence instances
generated for a single year.

The returned value is a closure which can be called on the list of
recurrences for one interval to filter it by index."
  (lambda (dts)
    (let* ((len (length dts))
           (keep-indices (mapcar
                          (lambda (pos)
                            ;; sequence indices are 0-based, POS's are 1-based:
                            (if (< pos 0)
                                (+ pos len)
                              (1- pos)))
                          setpos)))
      (delq nil
        (seq-map-indexed
         (lambda (dt index)
           (when (memq index keep-indices)
                 dt))
         dts)))))

(defun icr:refine-from-clauses (interval recur-value dtstart
                                &optional vtimezone)
  "Resolve INTERVAL into subintervals based on the clauses in RECUR-VALUE.

The resulting list of subintervals represents all times in INTERVAL
which match the BY* clauses of RECUR-VALUE except BYSETPOS, as well as
the constraints implicit in DTSTART.  (For example, if there is no
BYMINUTE clause, subintervals will have the same minute value as
DTSTART.)

If specified, VTIMEZONES should be a list of `icalendar-vtimezone'
components and TZID should be the `icalendar-tzid' property value of one
of those timezones.  In this case, TZID states the time zone of DTSTART,
and the offsets effective in that time zone on the dates and times of
recurrences will be local to that time zone."
  (let ((freq (ical:recur-freq recur-value))
        (weekstart (ical:recur-weekstart recur-value))
        (subintervals (list interval)))

    (dolist (byunit (list 'BYMONTH 'BYWEEKNO
                          'BYYEARDAY 'BYMONTHDAY 'BYDAY
                          'BYHOUR 'BYMINUTE 'BYSECOND))
      (let ((values (ical:recur-by* byunit recur-value))
            (in-month nil))
        ;; When there is no explicit BY* clause, use the value implicit
        ;; in DTSTART.  (These conditions are adapted from RFC8984:
        ;;   https://www.rfc-editor.org/rfc/rfc8984.html#section-4.3.3.1-4.3.1
        ;; Basically, the conditions are somewhat complicated because
        ;; the meanings of various BY* clauses are not independent and
        ;; so we have to be careful about the information we take to be
        ;; implicit in DTSTART, especially with MONTHLY and YEARLY
        ;; rules.  For example, we *do* want to take the weekday of
        ;; DTSTART as an implicit constraint if a BYWEEKNO clause is
        ;; present, but not if an explicit BYDAY or BYMONTHDAY clause is
        ;; also present, since they might contain conflicting
        ;; constraints.)
        (when (and (eq byunit 'BYSECOND)
                   (not (eq freq 'SECONDLY))
                   (not values))
          (setq values (list (ical:date/time-second dtstart))))
        (when (and (eq byunit 'BYMINUTE)
                   (not (memq freq '(SECONDLY MINUTELY)))
                   (not values))
          (setq values (list (ical:date/time-minute dtstart))))
        (when (and (eq byunit 'BYHOUR)
                   (not (memq freq '(SECONDLY MINUTELY HOURLY)))
                   (not values))
          (setq values (list (ical:date/time-hour dtstart))))
        (when (and (eq byunit 'BYDAY)
                   (eq freq 'WEEKLY)
                   (not values))
          (setq values (list (ical:date/time-weekday dtstart))))
        (when (and (eq byunit 'BYMONTHDAY)
                   (eq freq 'MONTHLY)
                   (not (ical:recur-by* 'BYDAY recur-value))
                   (not values))
          (setq values (list (ical:date/time-monthday dtstart))))
        (when (and (eq freq 'YEARLY)
                   (not (ical:recur-by* 'BYYEARDAY recur-value)))
          (when (and (eq byunit 'BYMONTH)
                     (not values)
                     (not (ical:recur-by* 'BYWEEKNO recur-value))
                     (or (ical:recur-by* 'BYMONTHDAY recur-value)
                         (not (ical:recur-by* 'BYDAY recur-value))))
            (setq values (list (ical:date/time-month dtstart))))
          (when (and (eq byunit 'BYMONTHDAY)
                     (not values)
                     (not (ical:recur-by* 'BYWEEKNO recur-value))
                     (not (ical:recur-by* 'BYDAY recur-value)))
            (setq values (list (ical:date/time-monthday dtstart))))
          (when (and (eq byunit 'BYDAY)
                     (not values)
                     (ical:recur-by* 'BYWEEKNO recur-value)
                     (not (ical:recur-by* 'BYMONTHDAY recur-value)))
            (setq values (list (ical:date/time-weekday dtstart)))))

        ;; Handle offsets in a BYDAY clause:
        ;; "If present, this [offset] indicates the nth occurrence of a
        ;; specific day within the MONTHLY or YEARLY "RRULE".  For
        ;; example, within a MONTHLY rule, +1MO (or simply 1MO)
        ;; represents the first Monday within the month, whereas -1MO
        ;; represents the last Monday of the month.  The numeric value
        ;; in a BYDAY rule part with the FREQ rule part set to YEARLY
        ;; corresponds to an offset within the month when the BYMONTH
        ;; rule part is present"
        (when (and (eq byunit 'BYDAY)
                   (or (eq freq 'MONTHLY)
                       (and (eq freq 'YEARLY)
                            (ical:recur-by* 'BYMONTH recur-value))))
          (setq in-month t))

        ;; On each iteration of the loop, we refine the subintervals
        ;; with these explicit or implicit values:
        (when values
          (setq subintervals
                (delq nil
                      (mapcan (lambda (in)
                                (icr:refine-by byunit in values in-month
                                               weekstart vtimezone))
                              subintervals))))))

    ;; Finally return the refined subintervals after we've looked at all
    ;; clauses:
    subintervals))

;; Once we have refined an interval into a final set of subintervals, we
;; need to convert those subintervals into a set of recurrences.  For a
;; recurrence set where DTSTART and the recurrences are date-times, the
;; recurrence set (in this interval) consists of every date-time
;; corresponding to each second of any subinterval.  When DTSTART and the
;; recurrences are plain dates, the recurrence set consists of each
;; distinct date in any subinterval.
(defun icr:subintervals-to-date-times (subintervals &optional vtimezone)
  "Transform SUBINTERVALS into a list of `icalendar-date-time' recurrences.

The returned list of recurrences contains one date-time value for each
second of each subinterval."
  (let (recurrences)
    (dolist (int subintervals)
      (let* ((start (car int))
             (dt start)
             ;; Use absolute times for the loop in case the subinterval
             ;; crosses the boundary between two observances.
             ;; N.B. floating times will be correctly treated as local
             ;; times by encode-time.
             (end (time-convert (encode-time (cadr int)) 'integer))
             (tick (time-convert (encode-time start) 'integer)))
        (while (time-less-p tick end)
          (push dt recurrences)
          (setq tick (1+ tick)
                dt (if vtimezone (icr:tz-decode-time tick vtimezone)
                     (ical:date/time-add dt :second 1))))))
    (nreverse recurrences)))

(defun icr:subintervals-to-dates (subintervals)
  "Transform SUBINTERVALS into a list of `icalendar-date' recurrences.

The returned list of recurrences contains one date value for each
day of each subinterval."
  (let (recurrences)
    (dolist (int subintervals)
      (let* ((start (car int))
             (start-abs (calendar-absolute-from-gregorian
                         (ical:date-time-to-date start)))
             (end (cadr int))
             (end-abs (calendar-absolute-from-gregorian
                       (ical:date-time-to-date end)))
             ;; end is an exclusive upper bound, but number-sequence
             ;; needs an *inclusive* upper bound, so if end is at
             ;; midnight, the bound is the previous day:
             (bound (if (zerop (+ (decoded-time-hour end)
                                  (decoded-time-minute end)
                                  (decoded-time-second end)))
                        (1- end-abs)
                      end-abs)))
        (setq recurrences
              (append recurrences
                      (mapcar #'calendar-gregorian-from-absolute
                              (number-sequence start-abs bound))))))
    recurrences))

(defun icr:subintervals-to-recurrences (subintervals dtstart &optional vtimezone)
  "Transform SUBINTERVALS into a list of recurrences.

The returned list of recurrences contains all distinct values in each
subinterval of the same type as DTSTART."
  (if (cl-typep dtstart 'ical:date)
      (icr:subintervals-to-dates subintervals)
    (icr:subintervals-to-date-times subintervals vtimezone)))


;; Calculating recurrences in a given interval or window
;;
;; We can now put all of the above together to compute the set of
;; recurrences in a given interval (`icr:recurrences-in-interval'), and
;; thereby in a given window (`icr:recurences-in-window'); or, if the
;; rule describing the set has a COUNT clause, we can enumerate the
;; recurrences in each interval starting from the beginning of the set
;; (`icr:recurrences-to-count').
(defun icr:recurrences-in-interval (interval component &optional vtimezone nmax)
  "Return a list of the recurrences of COMPONENT in INTERVAL.

INTERVAL should be a list (LOW HIGH NEXT) of date-times which bound a
single recurrence interval, as returned e.g. by
`icalendar-recur-find-interval'.  (To find the recurrences in an
arbitrary window of time, rather than between interval boundaries, see
`icalendar-recur-recurrences-in-window'.)

COMPONENT should be an iCalendar component node representing a recurring
event: it should contain at least an `icalendar-dtstart' and either an
`icalendar-rrule' or `icalendar-rdate' property.

If specified, VTIMEZONE should be an `icalendar-vtimezone' component.
In this case, the dates and times of recurrences will be computed with
UTC offsets local to that time zone.

If specified, NMAX should be a positive integer containing a maximum
number of recurrences to return from this interval.  In this case, if the
interval contains more than NMAX recurrences, only the first NMAX
recurrences will be returned; otherwise all recurrences in the interval
are returned.  (The NMAX argument mainly exists to support recurrence
rules with a COUNT clause; see `icalendar-recur-recurrences-to-count'.)

The returned list is a list of `icalendar-date' or `icalendar-date-time'
values representing the start times of recurrences.  Note that any
values of type `icalendar-period' in COMPONENT's `icalendar-rdate'
property (or properties) will NOT be included in the list; it is the
callee's responsibility to handle any such values separately.

The computed recurrences for INTERVAL are cached in COMPONENT and
retrieved on subsequent calls with the same arguments."
  (ical:with-component component
      ((ical:dtstart :value dtstart)
       (ical:tzoffsetfrom :value offset-from)
       (ical:rrule :value recur-value)
       (ical:rdate :all rdate-nodes) ;; TODO: these can also be ical:period values
       (ical:exdate :all exdate-nodes))
    (if (not (or recur-value rdate-nodes))
        ;; No recurrences to calculate, so just return early:
        nil
      ;; Otherwise, calculate recurrences in the interval:
      (when (memq (ical:ast-node-type component) '(ical:standard ical:daylight))
        ;; In time zone observances, set the zone field in dtstart
        ;; from the TZOFFSETFROM property:
        (setq dtstart
              (ical:date-time-variant dtstart
                                      :zone offset-from
                                      :dst (not (ical:daylight-component-p
                                                 component)))))
      (cl-labels ((get-interval
                    (apply-partially #'icr:-set-get-interval component))
                  (put-interval
                    (apply-partially #'icr:-set-put-interval component)))
        (let ((cached (get-interval interval)))
          (cond ((eq cached :none) nil)
                (cached cached)
                (t
                 (let* (;; Start by generating all the recurrences matching the
                        ;; BY* clauses except for BYSETPOS:
                        (subs (icr:refine-from-clauses interval recur-value dtstart
                                                       vtimezone))
                        (sub-recs (icr:subintervals-to-recurrences subs dtstart
                                                                   vtimezone))
                        ;; Apply any BYSETPOS clause to this set:
                        (keep-indices (ical:recur-by* 'BYSETPOS recur-value))
                        (pos-recs
                         (if keep-indices
                             (funcall (icr:make-bysetpos-filter keep-indices)
                                      sub-recs)
                           sub-recs))
                        ;; Remove any recurrences before DTSTART or after UNTIL
                        ;; (both of which are inclusive bounds):
                        (until (ical:recur-until recur-value))
                        (until-recs
                         (seq-filter
                          (lambda (rec) (and (ical:date/time<= dtstart rec)
                                             (or (not until)
                                                 (ical:date/time<= rec until))))
                          pos-recs))
                        ;; Include any values in the interval from the
                        ;; RDATE property:
                        (low (car interval))
                        (high (cadr interval))
                        (rdates
                         (mapcar #'ical:ast-node-value
                                 (apply #'append
                                        (mapcar #'ical:ast-node-value
                                                rdate-nodes))))
                        (interval-rdates
                         (seq-filter
                          (lambda (rec)
                            ;; only include ical:date and ical:date-time
                            ;; values from RDATE; callee is responsible
                            ;; for handling ical:period values
                            (unless (cl-typep rec 'ical:period)
                              (and (ical:date/time<= low rec)
                                   (ical:date/time< high rec))))
                          rdates))
                        (included-recs (append until-recs interval-rdates))
                        ;; Exclude any values from the EXDATE property;
                        ;; this gives us the complete set of recurrences
                        ;; in this interval:
                        (exdates
                         (mapcar #'ical:ast-node-value
                                 (append
                                  (mapcar #'ical:ast-node-value exdate-nodes))))
                        (all-recs
                         (if exdates
                             (seq-filter
                              (lambda (rec) (not (member rec exdates)))
                              included-recs)
                           included-recs))
                        ;; Limit to the first NMAX recurrences if requested.
                        ;; `icr:recurrences-to-count' provides NMAX so as not to
                        ;; store more recurrences in the final interval than the
                        ;; COUNT clause allows:
                        (nmax-recs
                         (if nmax (seq-take all-recs nmax)
                           all-recs)))
                   ;; Store and return the computed recurrences:
                   (put-interval interval (or nmax-recs :none))
                   nmax-recs))))))))

(defun icr:recurrences-in-window (lower upper component &optional vtimezone)
  "Return the recurrences of COMPONENT in the window between LOWER and UPPER.

LOWER and UPPER may be arbitrary `icalendar-date' or
`icalendar-date-time' values.  COMPONENT should be an iCalendar component
node representing a recurring event: it should contain at least an
`icalendar-dtstart' and either an `icalendar-rrule' or `icalendar-rdate'
property.

If specified, VTIMEZONE should be an `icalendar-vtimezone' component.
In this case, the dates and times of recurrences will be computed with
UTC offsets local to that time zone."
  (ical:with-component component
      ((ical:dtstart :value dtstart)
       (ical:tzoffsetfrom :value offset-from)
       (ical:rrule :value recur-value)
       (ical:rdate :all rdate-nodes))
    (if (not (or recur-value rdate-nodes))
        ;; No recurrences to calculate, so just return early:
        nil
      ;; Otherwise, calculate the recurrences in the window:
      (when (memq (ical:ast-node-type component) '(ical:standard ical:daylight))
        ;; in time zone observances, set the zone field in dtstart
        ;; from the TZOFFSETFROM property:
        (setq dtstart
              (ical:date-time-variant dtstart
                                      :zone offset-from
                                      :dst (not (ical:daylight-component-p
                                                 component)))))

      (let* (;; don't look for nonexistent intervals:
             (low-start (if (ical:date/time< lower dtstart) dtstart lower))
             (until (ical:recur-until recur-value))
             (high-end (if (and until (ical:date/time< until upper)) until upper))
             (curr-interval (icr:find-interval low-start dtstart recur-value
                                               vtimezone))
             (high-interval (icr:find-interval high-end dtstart recur-value
                                               vtimezone))
             (high-intbound (cadr high-interval))
             (recurrences nil))

        (while (ical:date-time< (car curr-interval) high-intbound)
          (setq recurrences
                (append
                 (icr:recurrences-in-interval curr-interval component vtimezone)
                 recurrences))
          (setq curr-interval (icr:next-interval curr-interval recur-value
                                                 vtimezone)))

        ;; exclude any recurrences inside the first and last intervals but
        ;; outside the window before returning:
        (seq-filter
         (lambda (dt)
           (and (ical:date/time<= lower dt)
                (ical:date/time< dt upper)))
         recurrences)))))

(defun icr:recurrences-in-window-w/end-times
    (lower upper component &optional vtimezone)
  "Like `icalendar-recurrences-in-window', but returns end times.

The return value is a list of (START END) pairs representing the start
and end time of each recurrence of COMPONENT in the window defined by
LOWER and UPPER.

In the returned pairs, START and END are both `icalendar-date' or
`icalendar-date-time' values of the same type as COMPONENT's
`icalendar-dtstart'.  Each END time is computed by adding COMPONENT's
`icalendar-duration' value to START for each recurrence START between
LOWER and UPPER.  Or, if the recurrence is given by an `icalendar-period'
value in an `icalendar-rdate' property, START and END are determined by
the period."
  (ical:with-component component
    ((ical:duration :value duration)
     (ical:rdate :all rdate-nodes))
    ;; TODO: for higher-level applications showing a schedule, it might
    ;; be useful to include recurrences which start outside the window,
    ;; but end inside it.  This would mean we can't simply use
    ;; `recurrences-in-window' like this.
    (let ((starts (icr:recurrences-in-window lower upper component vtimezone))
          (periods (seq-filter
                    (lambda (vnode)
                      (when (eq 'ical:period (ical:ast-node-type vnode))
                        (ical:ast-node-value vnode)))
                    (append
                     (mapcar #'ical:ast-node-value rdate-nodes)))))
      (when (or starts periods)
        (seq-uniq
         (append (mapcar
                  (lambda (dt) (list dt (ical:date/time-add-duration
                                         dt duration vtimezone)))
                  starts)
                 (mapcar
                  (lambda (p)
                    (let ((start (ical:period-start p)))
                      (list start
                            (or (ical:period-end p)
                                (ical:date/time-add-duration
                                 start (ical:period-dur-value p) vtimezone)))))
                  periods)))))))

(defun icr:recurrences-to-count (component &optional vtimezone)
  "Return all the recurrences in COMPONENT up to COUNT in its recurrence rule.

COMPONENT should be an iCalendar component node representing a recurring
event: it should contain at least an `icalendar-dtstart' and an
`icalendar-rrule', which must contain a COUNT=... clause.

Warning: this function finds *all* the recurrences in COMPONENT's
recurrence set.  If the value of COUNT is large, this can be slow.

If specified, VTIMEZONE should be an `icalendar-vtimezone' component.
In this case, the dates and times of recurrences will be computed with
UTC offsets local to that time zone."
  (ical:with-component component
      ((ical:dtstart :value dtstart)
       (ical:tzoffsetfrom :value offset-from)
       (ical:rrule :value recur-value)
       (ical:rdate :all rdate-nodes))
    (when (memq (ical:ast-node-type component) '(ical:standard ical:daylight))
      ;; in time zone observances, set the zone field in dtstart
      ;; from the TZOFFSETFROM property:
      (setq dtstart
            (ical:date-time-variant dtstart
                                    :zone offset-from
                                    :dst (not (ical:daylight-component-p
                                               component)))))
    (unless (or recur-value rdate-nodes)
      (error "No recurrence data in component: %s" component))
    (unless (ical:recur-count recur-value)
      (error "Recurrence rule has no COUNT clause"))
    (let ((count (ical:recur-count recur-value))
          (int (icr:nth-interval 0 dtstart recur-value vtimezone))
          recs)
      (while (length< recs count)
        (setq recs
              (append recs (icr:recurrences-in-interval int component vtimezone
                                                        (- count (length recs)))))
        (setq int (icr:next-interval int recur-value vtimezone)))
      recs)))



;; Recurrence set representation
;;
;; We represent a recurrence set as a map from intervals to the
;; recurrences in that interval.  The primary purpose of this
;; representation is to memoize the computation of recurrences, since
;; the computation is relatively expensive and the results are needed
;; repeatedly, particularly for time zone observances.  The map is stored
;; in the `:recurrence-set' property of the iCalendar component which
;; represents the recurring event.
;;
;; Internally, we use a hash table for the map, since the set can grow
;; quite large.  We use the start date-times of intervals as the keys,
;; since these uniquely identify intervals within a given component; we
;; ignore the weekday, zone and dst fields in the keys, mostly to avoid
;; cache misses during time zone observance lookups, which must generate
;; intervals with different zone values.
;;
;; In order to avoid repeating the computation of recurrences, we store
;; the keyword `:none' as the value when there are no recurrences in a
;; given interval.  This distinguishes the value from nil, so that,
;; whereas (gethash some-key the-map) => nil means "We haven't computed
;; recurrences yet for this interval", (gethash some-key the-map) =>
;; :none means "We've computed that there are no recurrences in this
;; interval", and can skip the computation of recurrences.  See
;; `icalendar-recur-recurrences-in-interval', which performs the check.

(defun icr:-make-set ()
  (make-hash-table :test #'equal))

(defsubst icr:-key-from-interval (interval)
  (take 6 (car interval))) ; (secs mins hours day month year)

(defun icr:-set-get-interval (component interval)
  (let ((set (ical:ast-node-meta-get :recurrence-set component))
        (key (icr:-key-from-interval interval)))
    (when (hash-table-p set)
      (gethash key set))))

(defun icr:-set-put-interval (component interval recurrences)
  (let ((set (or (ical:ast-node-meta-get :recurrence-set component)
                 (icr:-make-set)))
        (key (icr:-key-from-interval interval)))
    (setf (gethash key set) recurrences)
    (ical:ast-node-meta-set component :recurrence-set set)))


;; Timezones:

(define-error 'ical:tz-nonexistent-time "Date-time does not exist" 'ical:error)

(define-error 'ical:tz-no-observance "No observance found for date-time"
              'ical:error)

(define-error 'ical:tz-data-insufficient
              "Insufficient time zone data to create VTIMEZONE"
              'ical:error)

(define-error 'ical:tz-unsupported
              "Time zone rules not expressible as iCalendar RRULE"
              'ical:error)

;; In RFC5545 Section 3.3.10, we read: "If the computed local start time
;; of a recurrence instance does not exist ... the time of the
;; recurrence instance is interpreted in the same manner as an explicit
;; DATE-TIME value describing that date and time, as specified in
;; Section 3.3.5." which in turn says:
;; "If, based on the definition of the referenced time zone, the local
;; time described occurs more than once (when changing from daylight to
;; standard time), the DATE-TIME value refers to the first occurrence of
;; the referenced time.  Thus, TZID=America/New_York:20071104T013000
;; indicates November 4, 2007 at 1:30 A.M.  EDT (UTC-04:00).  If the
;; local time described does not occur (when changing from standard to
;; daylight time), the DATE-TIME value is interpreted using the UTC
;; offset before the gap in local times.  Thus,
;; TZID=America/New_York:20070311T023000 indicates March 11, 2007 at
;; 3:30 A.M.  EDT (UTC-04:00), one hour after 1:30 A.M.  EST (UTC-05:00)."

;; TODO: verify that these functions are correct for time zones other
;; than US Eastern.
(defun icr:nonexistent-date-time-p (dt obs-onset observance)
  "Return non-nil if DT does not exist in a given OBSERVANCE.

Some local date-times do not exist in a given time zone.  When switching
from standard to daylight savings time, the local clock time jumps over
a certain range of times.  This function tests whether DT is one of those
non-existent local times.

DT and OBS-ONSET should be `icalendar-date-time' values; OBS-ONSET
should be the (local) time immediately at the onset of the
OBSERVANCE.  OBSERVANCE should be an `icalendar-standard' or
`icalendar-daylight' component.

If this function returns t, then per RFC5545 Section 3.3.5, DT must be
interpreted using the UTC offset in effect prior to the onset of
OBSERVANCE.  For example, at the switch from Standard to Daylight
Savings time in US Eastern, the nonexistent time 2:30AM (Standard) must
be re-interpreted as 3:30AM DST."
  (when (ical:daylight-component-p observance)
    (ical:with-component observance
        ((ical:tzoffsetfrom :value offset-from)
         (ical:tzoffsetto :value offset-to))
      (and (= (decoded-time-year dt) (decoded-time-year obs-onset))
           (= (decoded-time-month dt) (decoded-time-month obs-onset))
           (= (decoded-time-day dt) (decoded-time-day obs-onset))
           (let* ((onset-secs (+ (decoded-time-second obs-onset)
                                 (* 60 (decoded-time-minute obs-onset))
                                 (* 60 60 (decoded-time-hour obs-onset))))
                  (dt-secs (+ (decoded-time-second dt)
                              (* 60 (decoded-time-minute dt))
                              (* 60 60 (decoded-time-hour dt))))
                  (jumped (abs (- offset-from offset-to)))
                  (after-jumped (+ onset-secs jumped)))
             (and
              (<= onset-secs dt-secs)
              (< dt-secs after-jumped)))))))

(defun icr:date-time-occurs-twice-p (dt obs-onset observance)
  "Return non-nil if DT occurs twice in the given OBSERVANCE.

Some local date-times occur twice in a given time zone.  When switching
from daylight savings to standard time time, the local clock time is
typically set back, so that a certain range of clock times occurs twice,
once in daylight savings time and once in standard time.  This function
tests whether DT is one of those local times which occur twice.

DT and OBS-ONSET should be `icalendar-date-time' values; OBS-ONSET
should be the (local) time immediately at the relevant onset of the
OBSERVANCE.  OBSERVANCE should be an `icalendar-standard' or
`icalendar-daylight' component.

If this function returns t, then per RFC5545 Section 3.3.5, DT must be
interpreted as the first occurrence of this clock time, i.e., in
daylight savings time, prior to OBS-ONSET."
  (when (ical:standard-component-p observance)
    (ical:with-component observance
        ((ical:tzoffsetfrom :value offset-from)
         (ical:tzoffsetto :value offset-to))
      (and (= (decoded-time-year dt) (decoded-time-year obs-onset))
           (= (decoded-time-month dt) (decoded-time-month obs-onset))
           (= (decoded-time-day dt) (decoded-time-day obs-onset))
           (let* ((onset-secs (+ (decoded-time-second obs-onset)
                                 (* 60 (decoded-time-minute obs-onset))
                                 (* 60 60 (decoded-time-hour obs-onset))))
                  (dt-secs (+ (decoded-time-second dt)
                              (* 60 (decoded-time-minute dt))
                              (* 60 60 (decoded-time-hour dt))))
                  (repeated (abs (- offset-from offset-to)))
                  (start-repeateds (- onset-secs repeated)))
             (and
              (<= start-repeateds dt-secs)
              (< dt-secs onset-secs)))))))

(defun icr:tz--get-updated-in (dt obs-onset observance)
  "Determine how to update DT's zone and dst slots from OBSERVANCE.

DT should be an `icalendar-date-time', OBSERVANCE an
`icalendar-standard' or `icalendar-daylight', and OBS-ONSET the nearest
onset of OBSERVANCE before DT.  Returns an `icalendar-date-time' that can
be used to update DT.

In most cases, the return value will contain a zone offset equal to
OBSERVANCE's `icalendar-tzoffsetto' value.

However, when DT falls within a range of nonexistent times after
OBS-ONSET, or a range of local times that occur twice (see
`icalendar-recur-nonexistent-date-time-p' and
`icalendar-recur-date-time-occurs-twice-p'), it needs to be interpreted
with the UTC offset in effect prior to the OBS-ONSET of OBSERVANCE (see
RFC5545 Section 3.3.5).  So e.g. at the switch from Standard to Daylight
in US Eastern, 2:30AM EST (a nonexistent time) becomes 3:30AM EDT, and
at the switch from Daylight to Standard, 1:30AM (which occurs twice)
becomes 1:30AM EDT, the first occurence."
  (ical:with-component observance
      ((ical:tzoffsetfrom :value offset-from)
       (ical:tzoffsetto :value offset-to))
    (let* ((is-daylight (ical:daylight-component-p observance))
           (to-dt (ical:date-time-variant dt :dst is-daylight :zone offset-to))
           (from-dt (ical:date-time-variant dt :dst (not is-daylight)
                                            :zone offset-from))
          updated)
      (cond ((icr:nonexistent-date-time-p to-dt obs-onset observance)
             ;; In this case, RFC5545 requires that we take the same
             ;; point in absolute time as from-dt, but re-decode it into
             ;; to-dt's zone:
             (setq updated (decode-time (encode-time from-dt) offset-to))
             (setf (decoded-time-dst updated) is-daylight))
            ((icr:date-time-occurs-twice-p to-dt obs-onset observance)
             ;; In this case, RFC5545 requires that we interpret dt as
             ;; from-dt, since that is the first occurrence of the clock
             ;; time in the zone:
             (setq updated from-dt))
            (t
             ;; Otherwise we interpret dt as to-dt, i.e., with the
             ;; offset effective within the observance:
             (setq updated to-dt)))
      updated)))

(defun icr:tz-for (tzid vtimezones)
  "Return the `icalendar-vtimezone' for the TZID.

VTIMEZONES should be a list of `icalendar-vtimezone' components.  TZID
should be a time zone identifier, as found e.g. in an
`icalendar-tzidparam' parameter.  The first time zone in VTIMEZONES whose
`icalendar-tzid' value matches this parameter's value is returned."
  (catch 'found
    (dolist (tz vtimezones)
      (ical:with-component tz
          ((ical:tzid :value tzidval))
        (when (equal tzidval tzid)
          (throw 'found tz))))))

;; DRAGONS DRAGONS DRAGONS
(defun icr:tz-observance-on (dt vtimezone &optional update nonexisting)
  "Return the time zone observance in effect on DT in VTIMEZONE.

If there is such an observance, the returned value is a list (OBSERVANCE
ONSET).  OBSERVANCE is an `icalendar-standard' or `icalendar-daylight'
component node.  ONSET is the recurrence of OBSERVANCE (an
`icalendar-date-time') which occurs closest in time, but before, DT.

If there is no such observance in VTIMEZONE, the returned value is nil.

VTIMEZONE should be an `icalendar-vtimezone' component node.

DT may be an an `icalendar-date-time' or a Lisp timestamp.  If it is a
date-time, it represents a local time assumed to be in VTIMEZONE.  Any
existing offset in DT is ignored, and DT is compared with the local
clock time at the start of each observance in VTIMEZONE to determine the
correct observance and onset.  (This is so that the correct observance
can be found for clock times generated during recurrence rule
calculations.)

If UPDATE is non-nil, the observance found will be used to update the
offset value in DT (as a side effect) before returning the observance
and onset.

If UPDATE is non-nil, NONEXISTING specifies how to handle clock times
that do not exist in the observance (see
`icalendar-recur-tz-nonexistent-date-time-p').  The keyword `:error'
means to signal an \\='icalendar-tz-nonexistent-time error, without
modifying any of the fields in DT.  Otherwise, the default is to
interpret DT using the offset from UTC before the onset of the found
observance, and then reset the clock time in DT to the corresponding
existing time after the onset of the observance.  For example, the
nonexisting time 2:30AM in Standard time on the day of the switch to
Daylight time in the US Eastern time zone will be reset to 3:30AM
Eastern Daylight time.

If DT is a Lisp timestamp, it represents an absolute time and
comparisons with the onsets in VTIMEZONE are performed with absolute
times.  UPDATE and NONEXISTING have no meaning in this case and are
ignored."
  (ical:with-component vtimezone
    ((ical:standard :all stds)
     (ical:daylight :all dls))
    (let (given-abs-time     ;; = `dt', if given a Lisp timestamp
          given-clock-time   ;; = `dt', if given a decoded time
          nearest-observance ;; the observance we're looking for
          nearest-onset      ;; latest onset of this observance before `dt'
          updated)           ;; stores how `dt's fields should be updated
                             ;; in line with this observance, if requested

      (if (cl-typep dt 'ical:date-time)
          ;; We were passed a date-time with local clock time, not an
          ;; absolute time; in this case, we must make local clock time
          ;; comparisons with the observance onset start and recurrences
          ;; (in order to determine the correct offset for it within the
          ;; zone)
          (setq given-clock-time dt
                given-abs-time nil)
        ;; We were passed an absolute time, not a date-time; in this
        ;; case, we can make comparisons in absolute time with
        ;; observance onset start and recurrences (in order to determine
        ;; the correct offset for decoding it)
        (setq given-abs-time dt
              given-clock-time nil))

      (dolist (obs (append stds dls))
        (ical:with-component obs
          ((ical:dtstart :value start)
           (ical:rrule :value recur-value)
           (ical:rdate :all rdate-nodes)
           (ical:tzoffsetfrom :value offset-from))
          ;; DTSTART of the observance must be given as local time, and is
          ;; combined with TZOFFSETFROM to define the effective onset
          ;; for the observance in absolute time.
          (let* ((is-daylight (ical:daylight-component-p obs))
                 (effective-start
                  (ical:date-time-variant start :zone offset-from
                                          :dst (not is-daylight)))
                 (observance-might-apply
                  (if given-clock-time
                      (ical:date-time-locally<= effective-start given-clock-time)
                    (ical:time<= (encode-time effective-start) given-abs-time))))

            (when observance-might-apply
              ;; Initialize our return values on the first iteration
              ;; where an observance potentially applies:
              (unless nearest-onset
                (setq nearest-onset effective-start
                      nearest-observance obs)
                (when (and update given-clock-time)
                  (setq updated
                        (icr:tz--get-updated-in given-clock-time
                                                effective-start obs))))

              ;; We first check whether any RDATEs in the observance are
              ;; the relevant onset:
              (let ((rdates
                     (mapcar #'ical:ast-node-value
                             (apply #'append
                                    (mapcar #'ical:ast-node-value rdate-nodes)))))
                (dolist (rd rdates)
                  (let* ((effective-rd
                          ;; N.B.: we don't have to worry about rd being
                          ;; an ical:period or ical:date here because in
                          ;; time zone observances, RDATE values are
                          ;; *only* allowed to be local date-times; see
                          ;; https://www.rfc-editor.org/rfc/rfc5545#section-3.6.5
                          ;; and `ical:rrule-validator'
                          (ical:date-time-variant rd :zone offset-from
                                                  :dst (not is-daylight)))
                         (onset-applies
                          (if given-clock-time
                              (ical:date-time-locally<= effective-rd
                                                        given-clock-time)
                            (ical:time<= (encode-time effective-rd)
                                         given-abs-time))))

                    (when (and onset-applies nearest-onset
                               (ical:date-time< nearest-onset effective-rd))
                      (setq nearest-onset effective-rd
                            nearest-observance obs)

                      (when (and update given-clock-time)
                        (setq updated
                              (icr:tz--get-updated-in given-clock-time
                                                      effective-rd obs)))))))

              ;; If the observance has a recurrence value, it's the
              ;; relevant observance if it:
              ;; (1) has a recurrence which starts before dt
              ;; (2) that recurrence is the nearest in the zone
              ;;     which starts before dt
              ;; Note that we intentionally do *not* pass `vtimezone'
              ;; through here to find-interval, recurrences-in-interval,
              ;; etc. so as not to cause infinite recursion.  Instead we
              ;; directly pass `offset-from' (the offset from UTC at the
              ;; start of each observance onset), which
              ;; `icr:tz-set-zone' knows to handle specially without
              ;; calling this function.
              (when recur-value
                (let* ((target (or given-clock-time
                                   (decode-time given-abs-time offset-from)))
                       (int (icr:find-interval
                             target effective-start recur-value offset-from))
                       (int-recs (icr:recurrences-in-interval
                                  int obs offset-from))
                       ;; The closest observance onset before `dt' might
                       ;; actually be in the previous interval, e.g.
                       ;; if `dt' is in January after an annual change to
                       ;; Standard Time in November.  So check that as well.
                       (prev-int (icr:previous-interval int recur-value
                                                        effective-start
                                                        offset-from))
                       (prev-recs (when prev-int
                                    (icr:recurrences-in-interval
                                     prev-int obs offset-from)))
                       (recs (append prev-recs int-recs))
                       (keep-recs<=given
                        (if given-clock-time
                            (lambda (rec)
                              (ical:date-time-locally<= rec given-clock-time))
                          (lambda (rec)
                            (ical:time<= (encode-time rec) given-abs-time))))
                       (srecs (sort (seq-filter ; (1)
                                     keep-recs<=given
                                     recs)
                                    :lessp #'ical:date-time<
                                    :in-place t :reverse t))
                       (latest-rec (car srecs)))

                  (when (and latest-rec
                             (ical:date-time< nearest-onset latest-rec)) ; (2)
                    (setf (decoded-time-dst latest-rec)
                          ;; if obs is a DAYLIGHT observance, latest-rec
                          ;; represents the last moment of standard time, and
                          ;; vice versa
                          (not is-daylight))
                    (setq nearest-onset latest-rec
                          nearest-observance obs)
                    (when (and update given-clock-time)
                      (setq updated
                            (icr:tz--get-updated-in given-clock-time
                                                    latest-rec obs))))))))))

      ;; We've now found the nearest observance, if there was one.
      ;; Update `dt' as a side effect if requested.  This saves
      ;; repeating a lot of the above in a separate function.
      (when (and update given-clock-time nearest-observance updated)
        ;; signal an error when `dt' does not exist if requested, so the
        ;; nonexistence can be handled further up the stack:
        (when (and (eq :error nonexisting)
                   (not (ical:date-time-locally-simultaneous-p dt updated)))
          (signal 'ical:tz-nonexistent-time
                  (list
                   :message
                   (format "%d-%02d-%02d %02d:%02d:%02d does not exist in %s"
                           (decoded-time-year dt)
                           (decoded-time-month dt)
                           (decoded-time-day dt)
                           (decoded-time-hour dt)
                           (decoded-time-minute dt)
                           (decoded-time-second dt)
                           (or
                            (ical:with-property-of nearest-observance
                                                   'ical:tzname nil value)
                            "time zone observance"))
                   :date-time dt
                   :observance nearest-observance)))
        ;; otherwise we copy `updated' over to `dt', which resets the
        ;; clock time in `dt' if it did not exist:
        (setf (decoded-time-zone dt) (decoded-time-zone updated))
        (setf (decoded-time-dst dt) (decoded-time-dst updated))
        (setf (decoded-time-second dt) (decoded-time-second updated))
        (setf (decoded-time-minute dt) (decoded-time-minute updated))
        (setf (decoded-time-hour dt) (decoded-time-hour updated))
        (setf (decoded-time-day dt) (decoded-time-day updated))
        (setf (decoded-time-month dt) (decoded-time-month updated))
        (setf (decoded-time-year dt) (decoded-time-year updated))
        (setf (decoded-time-weekday dt)
              (calendar-day-of-week (ical:date-time-to-date updated))))

      ;; Return the observance and onset if found, nil if not:
      (when nearest-observance
        (list nearest-observance nearest-onset)))))

(defun icr:tz-offset-in (observance)
  "Return the offset (in seconds) from UTC in effect during OBSERVANCE.

OBSERVANCE should be an `icalendar-standard' or `icalendar-daylight'
subcomponent of a particular `icalendar-vtimezone'.  The returned value
is the value of its `icalendar-tzoffsetto' property."
  (ical:with-property-of observance 'ical:tzoffsetto nil value))

(defun icr:tz-decode-time (ts vtimezone)
  "Decode Lisp timestamp TS with the appropriate offset in VTIMEZONE.

VTIMEZONE should be an `icalendar-vtimezone' component node.  The correct
observance for TS will be looked up in VTIMEZONE, TS will be decoded
with the UTC offset of that observance, and its dst slot will be set
based on whether the observance is an `icalendar-standard' or
`icalendar-daylight' component.  If VTIMEZONE does not have an
observance that applies to TS, it is decoded into UTC time.

VTIMEZONE may also be an `icalendar-utc-offset'.  In this case TS is
decoded directly into this UTC offset, and its dst slot is set to -1."
  (let* ((observance (when (ical:vtimezone-component-p vtimezone)
                       (car (icr:tz-observance-on ts vtimezone))))
         (offset (cond (observance (icr:tz-offset-in observance))
                       ((cl-typep vtimezone 'ical:utc-offset)
                        vtimezone)
                       (t 0))))

    (ical:date-time-variant ; ensures weekday gets set, too
     (decode-time ts offset)
     :zone offset
     :dst (if observance (ical:daylight-component-p observance)
            -1))))

(defun icr:tz-set-zone (dt vtimezone &optional nonexisting)
  "Set the time zone offset and dst flag in DT based on VTIMEZONE.

DT should be an `icalendar-date-time' and VTIMEZONE should be an
`icalendar-vtimezone'.  VTIMEZONE can also be an `icalendar-utc-offset',
in which case this value is directly set in DT's zone field (without
changing its dst flag).  The updated DT is returned.

This function generally sets only the zone and dst slots of DT, without
changing the other slots; its main purpose is to adjust date-times
generated from other date-times during recurrence rule calculations,
where a different time zone observance may be in effect in the original
date-time.  It cannot be used to re-decode a fixed point in time into a
different time zone; for that, see `icalendar-recur-tz-decode-time'.

If given, NONEXISTING is a keyword that specifies what to do if DT
represents a clock time that does not exist according to the relevant
observance in VTIMEZONE.  The value :error means to signal an
\\='icalendar-tz-nonexistent-time error, and nil means to reset the
clock time in DT to an existing one; see
`icalendar-recur-tz-observance-on'."
  (if (cl-typep vtimezone 'ical:utc-offset)
      ;; This is where the recurrence rule/time zone mutual dependence
      ;; bottoms out; don't remove this conditional!
      (setf (decoded-time-zone dt) vtimezone)

    ;; Otherwise, if there's already zone information in dt, trust it
    ;; without looking up the observance.  This is partly a performance
    ;; optimization (because the lookup is expensive) and partly about
    ;; avoiding problems: looking up the observance uses the clock time
    ;; in dt without considering the zone information, and doing this
    ;; when dt has already been adjusted to contain valid zone
    ;; information can invalidate that information.
    ;;
    ;; It's reliable to skip the lookup when dt already contains zone
    ;; information only because `icalendar-make-date-time',
    ;; `icalendar-date/time-add', and in particular
    ;; `icalendar-date-time-variant' are careful to remove the UTC
    ;; offset and DST information in the date-times they construct,
    ;; unless provided with enough information to fill those slots.
    (unless (and (cl-typep dt 'ical:date-time)
                 (decoded-time-zone dt)
                 (booleanp (decoded-time-dst dt)))
      ;; This updates the relevant slots in dt as a side effect:
      ;; TODO: if no observance is found, is it ever sensible to signal an error,
      ;; instead of just leaving the zone slot unset?
      (icr:tz-observance-on dt vtimezone t nonexisting)))
    dt)

(defun icr:tz-set-zones-in (vtimezones node)
  "Recursively set time zone offset and dst flags in times in NODE.

VTIMEZONES should be a list of the `icalendar-vtimezone' components in
the calendar containing NODE.  NODE can be any iCalendar syntax node.  If
NODE is a property node with an `icalendar-tzidparam' parameter and an
`icalendar-date-time' or `icalendar-period' value, the appropriate time
zone observance for its value is looked up in VTIMEZONES, and used to
set the zone and dst slots in its value.  Otherwise, the function is
called recursively on NODE's children."
  (cond
   ((ical:property-node-p node)
    (ical:with-property node
      ((ical:tzidparam :value tzid))
      (when (and tzid (eq value-type 'ical:date-time))
        (let* ((tz (icr:tz-for tzid vtimezones))
               updated)
          (cond ((eq value-type 'ical:date-time)
                 (setq updated (icr:tz-set-zone value tz)))
                ((eq value-type 'ical:period)
                 (setq updated
                       (ical:make-period
                        (icr:tz-set-zone (ical:period-start value) tz)
                        :end
                        (if (ical:period--defined-end value)
                            (icr:tz-set-zone (ical:period--defined-end value) tz)
                          (ical:period-end value tz))
                        :duration (ical:period-dur-value value)))))
          (ical:ast-node-set-value value-node updated)))))
   ((ical:component-node-p node) ; includes VCALENDAR nodes
    (mapc (apply-partially #'icr:tz-set-zones-in vtimezones)
          (ical:ast-node-children node)))
   (t nil)))

(defun icr:tzname-on (dt vtimezone)
  "Return the name of the time zone observance in effect on DT in VTIMEZONE.

DT should be an `icalendar-date' or `icalendar-date-time'.  VTIMEZONE
should be the `icalendar-vtimezone' component in which to interpret DT.

The observance in effect on DT within VTIMEZONE is computed.  The
returned value is the value of the `icalendar-tzname' property of this
observance."
  (when-let* ((obs/onset (icr:tz-observance-on dt vtimezone))
              (observance (car obs/onset)))
    (ical:with-property-of observance 'ical:tzname)))

(defconst icr:-tz-warning
  "This time zone information was inferred from incomplete system information; it should be correct for the date-times within this calendar file referencing this zone, but you should not rely on it more widely.")

(defconst icr:-emacs-local-tzid
  "Emacs_Local_")

(defun icr:-tz-info-sexp-p (_ sexp)
  "Validate that SEXP gives time zone info like from `calendar-current-time-zone'."
  (and (listp sexp)
       (length= sexp 8)
       (let ((utc-diff (nth 0 sexp))
             (dst-offset (nth 1 sexp))
             (std-zone (nth 2 sexp))
             (dst-zone (nth 3 sexp))
             (dst-starts (nth 4 sexp))
             (dst-ends (nth 5 sexp))
             (dst-starts-time (nth 6 sexp))
             (dst-ends-time (nth 7 sexp)))
         (and
          (integerp utc-diff) (< (abs utc-diff) (* 60 24))
          (integerp dst-offset) (< (abs utc-diff) (* 60 24))
          (stringp std-zone)
          (stringp dst-zone)
          (or (and (listp dst-starts) (memq 'year (flatten-list dst-starts)))
              (and (null dst-starts) (equal std-zone dst-zone)))
          (or (and (listp dst-ends) (memq 'year (flatten-list dst-ends)))
              (and (null dst-ends) (equal std-zone dst-zone)))
          (or (and (integerp dst-starts-time) (< (abs dst-starts-time) (* 60 24)))
              (null dst-starts-time))
          (or (and (integerp dst-ends-time) (< (abs dst-ends-time) (* 60 24)))
              (null dst-ends-time))))))

(defun icr:current-tz-to-vtimezone (&optional tz tzid start-year)
  "Convert TZ to an `icalendar-vtimezone'.

TZ defaults to the output of `calendar-current-time-zone'; if specified,
it should be a list of the same form as that function returns.
Depending on TZ, this function might signal the following errors:

`icalendar-tz-data-insufficient' if the data in TZ is not complete
  enough to determine time zone rules.
`icalendar-tz-unsupported' if the data in TZ cannot be expressed as an
  RFC5545 `icalendar-rrule' property.

TZID, if specified, should be a string to identify this time zone; it
defaults to `icalendar-recur--emacs-local-tzid' plus the name of the
standard observance according to `calendar-current-time-zone'.

START-YEAR, if specified, should be an integer giving the year in which
to start the observances in the time zone.  It defaults to 1970."
  (when (and tz (not (icr:-tz-info-sexp-p nil tz)))
    (signal 'ical:tz-data-insufficient
            (list :tz tz
                  :level 2
                  :message
                  "Badly formed TZ data; see `calendar-current-time-zone'")))
  (let* ((tzdata (or tz (calendar-current-time-zone)))
         (std-offset (* 60 (nth 0 tzdata)))
         (dst-offset (+ std-offset
                        (* 60 (nth 1 tzdata))))
         (std-name (nth 2 tzdata))
         (dst-name (nth 3 tzdata))
         (dst-starts (nth 4 tzdata))
         (dst-ends (nth 5 tzdata))
         (dst-start-minutes (nth 6 tzdata))
         (dst-end-minutes (nth 7 tzdata)))

    (unless (and std-offset
                 (or (equal std-name dst-name)
                     (and dst-starts dst-ends dst-start-minutes dst-end-minutes)))
      (signal 'ical:tz-data-insufficient
              (list :tz tz :level 2
                    :message "Unable to create VTIMEZONE from TZ")))

    (if (equal std-name dst-name)
        ;; Local time zone doesn't use DST:
        (ical:make-vtimezone
         (ical:tzid (or tzid (concat icr:-emacs-local-tzid std-name)))
         (ical:make-standard
          (ical:tzname std-name)
          (ical:dtstart (ical:make-date-time :year (or start-year 1970)
                                             :month 1 :day 1
                                             :hour 0 :minute 0 :second 0))
          (ical:tzoffsetfrom std-offset)
          (ical:tzoffsetto std-offset)
          (ical:comment icr:-tz-warning)))

      ;; Otherwise we can provide both STANDARD and DAYLIGHT subcomponents:
      (let* ((std->dst-rule
              (if (eq (car dst-starts) 'calendar-nth-named-day)
                  `((FREQ YEARLY)
                    (BYMONTH (,(nth 3 dst-starts)))
                    (BYDAY (,(cons (nth 2 dst-starts)
                                   (nth 1 dst-starts)))))
                ;; The only other rules that `calendar-current-time-zone'
                ;; can return are based on the Persian calendar, which we
                ;; cannot express in an `icalendar-recur' value, at least
                ;; pending an implementation of RFC 7529
                (signal 'ical:tz-unsupported
                        (list :tz tz
                              :level 2
                              :message
                              (format "Unable to export DST rule for time zone: %s"
                                      dst-starts)))))
             (dst-start-date (calendar-dlet ((year (or start-year 1970)))
                               (eval dst-starts t)))
             (dst-start
              (ical:date-to-date-time dst-start-date
                                      :hour (/ dst-start-minutes 60)
                                      :minute (mod dst-start-minutes 60)
                                      :second 0))
             (dst->std-rule
              (if (eq (car dst-ends) 'calendar-nth-named-day)
                  `((FREQ YEARLY)
                    (BYMONTH (,(nth 3 dst-ends)))
                    (BYDAY (,(cons (nth 2 dst-ends)
                                   (nth 1 dst-ends)))))
                (signal 'ical:tz-unsupported
                        (list :tz tz
                              :level 2
                              :message
                              (format "Unable to export DST rule for time zone: %s"
                                      dst-ends)))))
             (std-start-date (calendar-dlet ((year (1- (or start-year 1970))))
                               (eval dst-ends t)))
             (std-start
              (ical:date-to-date-time std-start-date
                                      :hour (/ dst-end-minutes 60)
                                      :minute (mod dst-end-minutes 60)
                                      :second 0)))

        (ical:make-vtimezone
         (ical:tzid (or tzid (concat icr:-emacs-local-tzid std-name)))
         (ical:make-standard
          (ical:tzname std-name)
          (ical:dtstart std-start)
          (ical:rrule dst->std-rule)
          (ical:tzoffsetfrom dst-offset)
          (ical:tzoffsetto std-offset)
          (ical:comment icr:-tz-warning))
         (ical:make-daylight
          (ical:tzname dst-name)
          (ical:dtstart dst-start)
          (ical:rrule std->dst-rule)
          (ical:tzoffsetfrom std-offset)
          (ical:tzoffsetto dst-offset)
          (ical:comment icr:-tz-warning)))))))

(provide 'icalendar-recur)

;; Local Variables:
;; read-symbol-shorthands: (("ical:" . "icalendar-") ("icr:" . "icalendar-recur-"))
;; End:
;;; icalendar-recur.el ends here
