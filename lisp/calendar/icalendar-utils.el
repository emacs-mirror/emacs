;;; icalendar-utils.el --- iCalendar utility functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Richard Lawrence

;; Author: Richard Lawrence <rwl@recursewithless.net>
;; Created: January 2025
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

;; This file contains a variety of utility functions to work with
;; iCalendar data which are used throughout the rest of the iCalendar
;; library.  Most of the functions here deal with calendar and clock
;; arithmetic, and help smooth over the type distinction between plain
;; dates and date-times.

;;; Code:
(require 'cl-lib)
(require 'calendar)
(eval-when-compile (require 'icalendar-macs))
(require 'icalendar-parser)

;; Accessors for commonly used properties

(defun ical:component-dtstart (component)
  "Return the value of the `icalendar-dtstart' property of COMPONENT.
COMPONENT can be any component node."
  (ical:with-property-of component 'ical:dtstart nil value))

(defun ical:component-dtend (component)
  "Return the value of the `icalendar-dtend' property of COMPONENT.
COMPONENT can be any component node."
  (ical:with-property-of component 'ical:dtend nil value))

(defun ical:component-rdate (component)
  "Return the value of the `icalendar-rdate' property of COMPONENT.
COMPONENT can be any component node."
  (ical:with-property-of component 'ical:rdate nil value))

(defun ical:component-summary (component)
  "Return the value of the `icalendar-summary' property of COMPONENT.
COMPONENT can be any component node."
  (ical:with-property-of component 'ical:summary nil value))

(defun ical:component-description (component)
  "Return the value of the `icalendar-description' property of COMPONENT.
COMPONENT can be any component node."
  (ical:with-property-of component 'ical:description nil value))

(defun ical:component-tzname (component)
  "Return the value of the `icalendar-tzname' property of COMPONENT.
COMPONENT can be any component node."
  (ical:with-property-of component 'ical:tzname nil value))

(defun ical:component-uid (component)
  "Return the value of the `icalendar-uid' property of COMPONENT.
COMPONENT can be any component node."
  (ical:with-property-of component 'ical:uid nil value))

(defun ical:component-url (component)
  "Return the value of the `icalendar-url' property of COMPONENT.
COMPONENT can be any component node."
  (ical:with-property-of component 'ical:url nil value))

(defun ical:property-tzid (property)
  "Return the value of the `icalendar-tzid' parameter of PROPERTY."
  (ical:with-param-of property 'ical:tzidparam nil value))

;; String manipulation
(defun ical:trimp (s &optional trim-left trim-right)
  "Like `string-trim', but return nil if the trimmed string is empty."
  (when (and s (stringp s))
    (let ((trimmed (string-trim s trim-left trim-right)))
      (unless (equal "" trimmed) trimmed))))

(defun ical:strip-mailto (s)
  "Remove \"mailto:\" case-insensitively from the start of S."
  (let ((case-fold-search t))
    (replace-regexp-in-string "^mailto:" "" s)))


;; Date/time

;; N.B. Notation: "date/time" is used in function names when a function
;; can accept either `icalendar-date' or `icalendar-date-time' values;
;; in contrast, "date-time" means it accepts *only*
;; `icalendar-date-time' values, not plain dates.
;; TODO: turn all the 'date/time' functions into methods dispatched by
;; type?

(defun ical:date-time-to-date (dt)
  "Convert an `icalendar-date-time' value DT to an `icalendar-date'."
  (list (decoded-time-month dt)
        (decoded-time-day dt)
        (decoded-time-year dt)))

(cl-defun ical:date-to-date-time (dt &key (hour 0) (minute 0) (second 0) (tz nil))
  "Convert an `icalendar-date' value DT to an `icalendar-date-time'.

The following keyword arguments are accepted:
  :hour, :minute, :second - integers representing a local clock time on date DT
  :tz - an `icalendar-vtimezone' in which to interpret this clock time

If these arguments are all unspecified, the hour, minute, and second
slots of the returned date-time will be zero, and it will contain no
time zone information.  See `icalendar-make-date-time' for more on these
arguments."
  (ical:make-date-time
   :year (calendar-extract-year dt)
   :month (calendar-extract-month dt)
   :day (calendar-extract-day dt)
   :hour hour
   :minute minute
   :second second
   :tz tz))

(defun ical:date/time-to-date (dt)
  "Extract a Gregorian date from DT.
An `icalendar-date' value is returned unchanged.
An `icalendar-date-time' value is converted to an `icalendar-date'."
  (if (cl-typep dt 'ical:date)
      dt
    (ical:date-time-to-date dt)))

;; Type-aware accessors for date/time slots that work for both ical:date
;; and ical:date-time:
;; NOTE: cl-typecase ONLY works here if dt is valid according to
;; `ical:-decoded-date-time-p'!  May need to adjust this if it's
;; necessary to work with incomplete decoded-times
(defun ical:date/time-year (dt)
  "Return DT's year slot.
DT may be either an `icalendar-date' or an `icalendar-date-time'."
  (cl-typecase dt
    (ical:date (calendar-extract-year dt))
    (ical:date-time (decoded-time-year dt))))

(defun ical:date/time-month (dt)
  "Return DT's month slot.
DT may be either an `icalendar-date' or an `icalendar-date-time'."
  (cl-typecase dt
    (ical:date (calendar-extract-month dt))
    (ical:date-time (decoded-time-month dt))))

(defun ical:date/time-monthday (dt)
  "Return DT's day of the month slot.
DT may be either an `icalendar-date' or an `icalendar-date-time'."
  (cl-typecase dt
    (ical:date (calendar-extract-day dt))
    (ical:date-time (decoded-time-day dt))))

(defun ical:date/time-weekno (dt &optional weekstart)
  "Return DT's ISO week number.
DT may be either an `icalendar-date' or an `icalendar-date-time'.
WEEKSTART defaults to 1; it represents the day which starts the week,
and should be an integer between 0 (= Sunday) and 6 (= Saturday)."
  ;; TODO: Add support for weekstart.
  ;; calendar-iso-from-absolute doesn't support this yet.
  (when (and weekstart (not (= weekstart 1)))
    (error "Support for WEEKSTART other than 1 (=Monday) not implemented yet"))
  (let* ((gdate (ical:date/time-to-date dt))
         (isodate (calendar-iso-from-absolute
                   (calendar-absolute-from-gregorian gdate)))
         (weekno (car isodate)))
    weekno))

(defun ical:date/time-weekday (dt)
  "Return DT's day of the week.
DT may be either an `icalendar-date' or an `icalendar-date-time'."
  (cl-typecase dt
    (ical:date (calendar-day-of-week dt))
    (ical:date-time
     (or (decoded-time-weekday dt)
         ;; compensate for possibly-nil weekday slot if the date-time
         ;; has been constructed by `make-decoded-time'; cf. comment
         ;; in `icalendar--decoded-date-time-p':
         (calendar-day-of-week (ical:date-time-to-date dt))))))

(defun ical:date/time-hour (dt)
  "Return DT's hour slot, or nil.
DT may be either an `icalendar-date' or an `icalendar-date-time'."
  (when (cl-typep dt 'ical:date-time)
    (decoded-time-hour dt)))

(defun ical:date/time-minute (dt)
  "Return DT's minute slot, or nil.
DT may be either an `icalendar-date' or an `icalendar-date-time'."
  (when (cl-typep dt 'ical:date-time)
    (decoded-time-minute dt)))

(defun ical:date/time-second (dt)
  "Return DT's second slot, or nil.
DT may be either an `icalendar-date' or an `icalendar-date-time'."
  (when (cl-typep dt 'ical:date-time)
    (decoded-time-second dt)))

(defun ical:date/time-zone (dt)
  "Return DT's time zone slot, or nil.
DT may be either an `icalendar-date' or an `icalendar-date-time'."
  (when (cl-typep dt 'ical:date-time)
    (decoded-time-zone dt)))

;;; Date/time comparisons and arithmetic:
(defun ical:date< (dt1 dt2)
  "Return non-nil if date DT1 is strictly earlier than date DT2.
DT1 and DT2 must both be `icalendar-date' values of the form (MONTH DAY YEAR)."
  (< (calendar-absolute-from-gregorian dt1)
     (calendar-absolute-from-gregorian dt2)))

(defun ical:date<= (dt1 dt2)
  "Return non-nil if date DT1 is earlier than or the same date as DT2.
DT1 and DT2 must both be `icalendar-date' values of the form (MONTH DAY YEAR)."
  (or (calendar-date-equal dt1 dt2) (ical:date< dt1 dt2)))

(defun ical:date-time-locally-earlier (dt1 dt2 &optional or-equal)
  "Return non-nil if date-time DT1 is locally earlier than DT2.

Unlike `icalendar-date-time<', this function assumes both times are
local to some time zone and does not consider their zone information.

If OR-EQUAL is non-nil, this function acts like `<=' rather than `<':
it will return non-nil if DT1 and DT2 are locally the same time."
  (let ((year1 (decoded-time-year dt1))
        (year2 (decoded-time-year dt2))
        (month1 (decoded-time-month dt1))
        (month2 (decoded-time-month dt2))
        (day1 (decoded-time-day dt1))
        (day2 (decoded-time-day dt2))
        (hour1 (decoded-time-hour dt1))
        (hour2 (decoded-time-hour dt2))
        (minute1 (decoded-time-minute dt1))
        (minute2 (decoded-time-minute dt2))
        (second1 (decoded-time-second dt1))
        (second2 (decoded-time-second dt2)))
    (or (< year1 year2)
        (and (= year1 year2)
             (or (< month1 month2)
                 (and (= month1 month2)
                      (or (< day1 day2)
                          (and (= day1 day2)
                               (or (< hour1 hour2)
                                   (and (= hour1 hour2)
                                        (or (< minute1 minute2)
                                            (and (= minute1 minute2)
                                                 (if or-equal
                                                     (<= second1 second2)
                                                   (< second1 second2))))))))))))))

(defun ical:date-time-locally< (dt1 dt2)
  "Return non-nil if date-time DT1 is locally strictly earlier than DT2.

Unlike `icalendar-date-time<', this function assumes both times are
local to some time zone and does not consider their zone information."
  (ical:date-time-locally-earlier dt1 dt2 nil))

(defun ical:date-time-locally<= (dt1 dt2)
  "Return non-nil if date-time DT1 is locally earlier than, or equal to, DT2.

Unlike `icalendar-date-time<=', this function assumes both times are
local to some time zone and does not consider their zone information."
  (ical:date-time-locally-earlier dt1 dt2 t))

(defun ical:date-time< (dt1 dt2)
  "Return non-nil if date-time DT1 is strictly earlier than DT2.

DT1 and DT2 must both be decoded times, and either both or neither
should have time zone information.

If one has a time zone offset and the other does not, the offset
returned from `current-time-zone' is used as the missing offset; if
`current-time-zone' cannot provide this information, an error is
signaled."
  (let ((zone1 (decoded-time-zone dt1))
        (zone2 (decoded-time-zone dt2)))
    (cond ((and (integerp zone1) (integerp zone2))
           (time-less-p (encode-time dt1) (encode-time dt2)))
          ((and (null zone1) (null zone2))
           (ical:date-time-locally< dt1 dt2))
          (t
           ;; Cf. RFC5545 Sec. 3.3.5:
           ;; "The recipient of an iCalendar object with a property value
           ;; consisting of a local time, without any relative time zone
           ;; information, SHOULD interpret the value as being fixed to whatever
           ;; time zone the "ATTENDEE" is in at any given moment.  This means
           ;; that two "Attendees", in different time zones, receiving the same
           ;; event definition as a floating time, may be participating in the
           ;; event at different actual times.  Floating time SHOULD only be
           ;; used where that is the reasonable behavior."
           ;; I'm interpreting this to mean that if we get here, where
           ;; one date-time has zone information and the other doesn't,
           ;; we should use the offset from (current-time-zone).
           (let* ((user-tz (current-time-zone))
                  (user-offset (car user-tz))
                  (dt1z (ical:date-time-variant dt1 :zone (or zone1 user-offset)))
                  (dt2z (ical:date-time-variant dt2 :zone (or zone2 user-offset))))
             (if user-offset
                 (time-less-p (encode-time dt1z) (encode-time dt2z))
               (error "Too little zone information for comparison: %s %s"
                      dt1 dt2)))))))

;; Two different notions of equality are relevant to decoded times:
;; strict equality (`icalendar-date-time=') of all slots, or
;; simultaneity (`icalendar-date-time-simultaneous-p').
;; Most tests probably want the strict notion, because it distinguishes
;; between simultaneous events decoded into different time zones,
;; whereas most user-facing functions (e.g. sorting events by date and time)
;; probably want simultaneity.
(defun ical:date-time= (dt1 dt2)
  "Return non-nil if DT1 and DT2 are decoded-times with identical slot values.

Note that this function returns nil if DT1 and DT2 represent times in
different time zones, even if they are simultaneous.  For the latter, see
`icalendar-date-time-simultaneous-p'."
  (equal dt1 dt2))

(defun ical:date-time-locally-simultaneous-p (dt1 dt2)
  "Return non-nil if DT1 and DT2 are locally simultaneous date-times.
Note that this function ignores zone information in dt1 and dt2.  It
returns non-nil if DT1 and DT2 represent the same clock time in
different time zones, even if they encode to different absolute times."
  (and (eq (decoded-time-year dt1)   (decoded-time-year dt2))
       (eq (decoded-time-month dt1)  (decoded-time-month dt2))
       (eq (decoded-time-day dt1)    (decoded-time-day dt2))
       (eq (decoded-time-hour dt1)   (decoded-time-hour dt2))
       (eq (decoded-time-minute dt1) (decoded-time-minute dt2))
       (eq (decoded-time-second dt1) (decoded-time-second dt2))))

(defun ical:date-time-simultaneous-p (dt1 dt2)
  "Return non-nil if DT1 and DT2 are simultaneous date-times.

This function returns non-nil if DT1 and DT2 encode to the same Lisp
timestamp.  Thus they can count as simultaneous even if they represent
times in different timezones.  If both date-times lack an offset from
UTC, they are treated as simultaneous if they encode to the same
timestamp in UTC.

If only one date-time has an offset, they are treated as
non-simultaneous if they represent different clock times according to
`icalendar-date-time-locally-simultaneous-p'.  Otherwise an error is
signaled."
  (let ((zone1 (decoded-time-zone dt1))
        (zone2 (decoded-time-zone dt2)))
    (cond ((and (integerp zone1) (integerp zone2))
           (time-equal-p (encode-time dt1) (encode-time dt2)))
          ((and (null zone1) (null zone2))
           (time-equal-p (encode-time (ical:date-time-variant dt1 :zone 0))
                         (encode-time (ical:date-time-variant dt2 :zone 0))))
          (t
           ;; Best effort:
           ;; TODO: I'm not convinced this is the right thing to do yet.
           ;; Might want to be stricter here and fix the problem of comparing
           ;; times with and without zone information elsewhere.
           (if (ical:date-time-locally-simultaneous-p dt1 dt2)
               (error "Missing zone information: %s %s" dt1 dt2)
             nil)))))

(defun ical:date-time<= (dt1 dt2)
  "Return non-nil if DT1 is earlier than, or simultaneous with, DT2.
DT1 and DT2 must both be decoded times, and either both or neither must have
time zone information."
  (or (ical:date-time< dt1 dt2)
      (ical:date-time-simultaneous-p dt1 dt2)))

(defun ical:date/time< (dt1 dt2)
  "Return non-nil if DT1 is strictly earlier than DT2.
DT1 and DT2 must be either `icalendar-date' or `icalendar-date-time'
values.  If they are not of the same type, only the date in the
`icalendar-date-time' value will be considered."
  (cl-typecase dt1
    (ical:date
     (if (cl-typep dt2 'ical:date)
         (ical:date< dt1 dt2)
       (ical:date< dt1 (ical:date-time-to-date dt2))))

    (ical:date-time
     (if (cl-typep dt2 'ical:date-time)
         (ical:date-time< dt1 dt2)
       (ical:date< (ical:date-time-to-date dt1) dt2)))))

(defun ical:date/time<= (dt1 dt2)
  "Return non-nil if DT1 is earlier than or simultaneous to DT2.
DT1 and DT2 must be either `icalendar-date' or `icalendar-date-time'
values.  If they are not of the same type, only the date in the
`icalendar-date-time' value will be considered."
  (cl-typecase dt1
    (ical:date
     (if (cl-typep dt2 'ical:date)
         (ical:date<= dt1 dt2)
       (ical:date<= dt1 (ical:date-time-to-date dt2))))

    (ical:date-time
     (if (cl-typep dt2 'ical:date-time)
         (ical:date-time<= dt1 dt2)
       (ical:date<= (ical:date-time-to-date dt1) dt2)))))

(defun ical:date/time-min (&rest dts)
  "Return the earliest date or date-time among DTS.

The DTS may be any `icalendar-date' or `icalendar-date-time' values, and
will be ordered by `icalendar-date/time<='."
  (car (sort dts :lessp #'ical:date/time<=)))

(defun ical:date/time-max (&rest dts)
  "Return the latest date or date-time among DTS.

The DTS may be any `icalendar-date' or `icalendar-date-time' values, and
will be ordered by `icalendar-date/time<='."
  (car (sort dts :reverse t :lessp #'ical:date/time<=)))

(defun ical:date-add (date unit n)
  "Add N UNITs to DATE.

UNIT should be `:year', `:month', `:week', or `:day'; time units will be
ignored.  N may be a positive or negative integer."
  (if (memq unit '(:hour :minute :second))
      date
    (let* ((dt (ical:make-date-time :year (calendar-extract-year date)
                                    :month (calendar-extract-month date)
                                    :day (calendar-extract-day date)))
           (delta (if (eq unit :week)
                      (make-decoded-time :day (* 7 n))
                    (make-decoded-time unit n)))
           (new-dt (decoded-time-add dt delta)))
      (ical:date-time-to-date new-dt))))

(defun ical:date-time-add (dt delta &optional vtimezone)
  "Like `decoded-time-add', but also updates weekday and time zone slots.

DT and DELTA should be `icalendar-date-time' values (decoded times), as
in `decoded-time-add'.  VTIMEZONE, if given, should be an
`icalendar-vtimezone'.  The resulting date-time will be given the offset
determined by VTIMEZONE at the local time determined by adding DELTA to
DT.

This function assumes that time units in DELTA larger than an hour
should not affect the local clock time in the result, even when crossing
an observance boundary in VTIMEZONE.  This means that e.g. if DT is at
9AM daylight savings time on the day before the transition to standard
time, then the result of adding a DELTA of two days will be at 9AM
standard time, even though this is not exactly 48 hours later.  Adding a
DELTA of 48 hours, on the other hand, will result in a time exactly 48
hours later, but at a different local time."
  (require 'icalendar-recur) ; for icr:tz-decode-time; avoids circular requires
  (declare-function icalendar-recur-tz-decode-time "icalendar-recur")

  (if (not vtimezone)
      ;; the simple case: we have no time zone info, so just use
      ;; `decoded-time-add':
      (let ((sum (decoded-time-add dt delta)))
        (ical:date-time-variant sum))
    ;; `decoded-time-add' does not take time zone shifts into account,
    ;; so we need to do the adjustment ourselves.  We first add the units
    ;; larger than an hour using `decoded-time-add', holding the clock
    ;; time fixed, as described in the docstring.  Then we add the time
    ;; units as a fixed number of seconds and re-decode the resulting
    ;; absolute time into the time zone.
    (let* ((cal-delta (make-decoded-time :year (or (decoded-time-year delta) 0)
                                         :month (or (decoded-time-month delta) 0)
                                         :day (or (decoded-time-day delta) 0)))
           (cal-sum (decoded-time-add dt cal-delta))
           (dt-w/zone (ical:date-time-variant cal-sum
                                              :tz vtimezone))
           (secs-delta (+ (or (decoded-time-second delta) 0)
                          (* 60 (or (decoded-time-minute delta) 0))
                          (* 60 60 (or (decoded-time-hour delta) 0))))
           (sum-ts (time-add (encode-time dt-w/zone) secs-delta)))
      (icalendar-recur-tz-decode-time sum-ts vtimezone))))

;; TODO: rework so that it's possible to add dur-values to plain dates.
;; Perhaps rename this to "date/time-inc" or so, or use kwargs to allow
;; multiple units, or...
(defun ical:date/time-add (dt unit n &optional vtimezone)
  "Add N UNITs to DT.

DT should be an `icalendar-date' or `icalendar-date-time'.  UNIT should
be `:year', `:month', `:week', `:day', `:hour', `:minute', or `:second';
time units will be ignored if DT is an `icalendar-date'.  N may be a
positive or negative integer."
  (cl-typecase dt
    (ical:date-time
     (let ((delta (if (eq unit :week) (make-decoded-time :day (* 7 n))
                    (make-decoded-time unit n))))
       (ical:date-time-add dt delta vtimezone)))
    (ical:date (ical:date-add dt unit n))))

(defun ical:date/time-add-duration (start duration &optional vtimezone)
  "Return the end date(-time) which is a length of DURATION after START.

START should be an `icalendar-date' or `icalendar-date-time'; the
returned value will be of the same type as START.  DURATION should be an
`icalendar-dur-value'.  VTIMEZONE, if specified, should be the
`icalendar-vtimezone' representing the time zone of START."
  (if (integerp duration)
      ;; number of weeks:
      (setq duration (make-decoded-time :day (* 7 duration))))
  (cl-typecase start
    (ical:date
     (ical:date-time-to-date
      (ical:date-time-add (ical:date-to-date-time start) duration)))
    (ical:date-time
     (ical:date-time-add start duration vtimezone))))

(defun ical:duration-between (start end)
  "Return the duration between START and END.

START should be an `icalendar-date' or `icalendar-date-time'; END must
be of the same type as START.  The returned value is an
`icalendar-dur-value', i.e., a time delta in the sense of
`decoded-time-add'."
  (cl-typecase start
    (ical:date
     (make-decoded-time :day (- (calendar-absolute-from-gregorian end)
                                (calendar-absolute-from-gregorian start))))
    (ical:date-time
     (let* ((start-abs (time-convert (encode-time start) 'integer))
            (end-abs (time-convert (encode-time end) 'integer))
            (dur-secs (- end-abs start-abs))
            (days (/ dur-secs (* 60 60 24)))
            (dur-nodays (mod dur-secs (* 60 60 24)))
            (hours (/ dur-nodays (* 60 60)))
            (dur-nohours (mod dur-nodays (* 60 60)))
            (minutes (/ dur-nohours 60))
            (seconds (mod dur-nohours 60)))
       (make-decoded-time :day days
                          :hour hours :minute minutes :second seconds)))))

(defun ical:date/time-to-local (dt)
  "Reinterpret DT in Emacs local time if necessary.
If DT is an `icalendar-date-time', encode and re-decode it into Emacs
local time.  If DT is an `icalendar-date', return it unchanged."
  (cl-typecase dt
    (ical:date dt)
    (ical:date-time
     (ical:date-time-variant ; ensure weekday is present too
      (decode-time (encode-time dt))))))

(defun ical:dates-until (start end &optional locally)
  "Return a list of `icalendar-date' values between START and END.

START and END may be either `icalendar-date' or `icalendar-date-time'
values.  START is an inclusive lower bound, and END is an exclusive
upper bound.  (Note, however, that if END is a date-time and its time is
after midnight, then its date will be included in the returned list.)

If LOCALLY is non-nil and START and END are date-times, these will be
interpreted into Emacs local time, so that the dates returned are valid
for the local time zone."
  (require 'icalendar-recur) ; avoid circular requires
  (declare-function icalendar-recur-subintervals-to-dates "icalendar-recur")

  (when locally
    (when (cl-typep start 'ical:date-time)
      (setq start (ical:date/time-to-local start)))
    (when (cl-typep end 'ical:date-time)
      (setq end (ical:date/time-to-local end))))
  (cl-typecase start
    (ical:date
     (cl-typecase end
       (ical:date
        (icalendar-recur-subintervals-to-dates
         (list (list (ical:date-to-date-time start)
                     (ical:date-to-date-time end)))))
       (ical:date-time
        (icalendar-recur-subintervals-to-dates
         (list (list (ical:date-to-date-time start) end))))))
    (ical:date-time
     (cl-typecase end
       (ical:date
        (icalendar-recur-subintervals-to-dates
         (list (list start (ical:date-to-date-time end)))))
       (ical:date-time
        (icalendar-recur-subintervals-to-dates (list (list start end))))))))


(cl-defun ical:make-date-time (&key second minute hour day month year
                                    (dst -1 given-dst) zone tz)
  "Make an `icalendar-date-time' from the given keyword arguments.

This function is like `make-decoded-time', except that it automatically
sets the weekday slot set based on the date arguments, and it accepts an
additional keyword argument: `:tz'.  If provided, its value should be an
`icalendar-vtimezone', and the `:zone' and `:dst' arguments should not
be provided.  In this case, the zone and dst slots in the returned
date-time will be adjusted to the correct values in the given time zone
for the local time represented by the remaining arguments."
  (when (and tz (or zone given-dst))
    (error "Possibly conflicting time zone data in args"))
  (apply #'ical:date-time-variant (make-decoded-time)
         `(:second ,second :minute ,minute :hour ,hour
           :day ,day :month ,month :year ,year
           ;; Don't pass these keywords unless they were given explicitly.
           ;; TODO: is there a cleaner way to write this?
           ,@(when tz (list :tz tz))
           ,@(when given-dst (list :dst dst))
           ,@(when zone (list :zone zone)))))

(cl-defun ical:date-time-variant (dt &key second minute hour
                                          day month year
                                          (dst -1 given-dst)
                                          (zone nil given-zone)
                                          tz)
  "Return a variant of DT with slots modified as in the given arguments.

DT should be an `icalendar-date-time'; the keyword arguments have the
same meanings as in `make-decoded-time'.  The returned variant will have
slot values as specified by the arguments or copied from DT, except that
the weekday slot will be updated if necessary, and the zone and dst
fields will not be set unless given explicitly (because varying the date
and clock time generally invalidates the time zone information in DT).

One additional keyword argument is accepted: `:tz'.  If provided, its
value should be an `icalendar-vtimezone', an `icalendar-utc-offset', or
the symbol \\='preserve.  If it is a time zone component, the zone and
dst slots in the returned variant will be adjusted to the correct
values in the given time zone for the local time represented by the
variant.  If it is a UTC offset, the variant's zone slot will contain
this value, but its dst slot will not be adjusted.  If it is the symbol
\\='preserve, then both the zone and dst fields are copied from DT into
the variant."
  (require 'icalendar-recur) ; for icr:tz-set-zone; avoids circular requires
  (declare-function icalendar-recur-tz-set-zone "icalendar-recur")

  (let ((variant
         (make-decoded-time :second (or second (decoded-time-second dt))
                            :minute (or minute (decoded-time-minute dt))
                            :hour (or hour (decoded-time-hour dt))
                            :day (or day (decoded-time-day dt))
                            :month (or month (decoded-time-month dt))
                            :year (or year (decoded-time-year dt))
                            ;; For zone and dst slots, trust the value
                            ;; if explicitly specified or explicitly
                            ;; requested to preserve, but not otherwise
                            :dst (cond (given-dst dst)
                                       ((eq 'preserve tz) (decoded-time-dst dt))
                                       (t -1))
                            :zone (cond (given-zone zone)
                                        ((eq 'preserve tz) (decoded-time-zone dt))
                                        (t nil)))))
    ;; update weekday slot when possible, since it depends on the date
    ;; slots, which might have changed.  (It's not always possible,
    ;; because pure time values are also represented as decoded-times,
    ;; with empty date slots.)
    (unless (or (null (decoded-time-year variant))
                (null (decoded-time-month variant))
                (null (decoded-time-day variant)))
      (setf (decoded-time-weekday variant)
            (calendar-day-of-week (ical:date-time-to-date variant))))
    ;; if given a time zone or UTC offset, update zone and dst slots,
    ;; which also might have changed:
    (when (and tz (not (eq 'preserve tz)))
      (icalendar-recur-tz-set-zone variant tz))
    variant))

(defun ical:date/time-in-period-p (dt period &optional vtimezone)
  "Return non-nil if DT occurs within PERIOD.

DT can be an `icalendar-date' or `icalendar-date-time' value.  PERIOD
should be an `icalendar-period' value.  VTIMEZONE, if given, is passed
to `icalendar-period-end' to compute the end time of the period if it
was not specified explicitly."
  (and (ical:date/time<= (ical:period-start period) dt)
       (ical:date/time< dt (ical:period-end period vtimezone))))

;; TODO: surely this exists already?
(defun ical:time<= (a b)
  "Compare two Lisp timestamps A and B: is A <= B?"
  (or (time-equal-p a b)
      (time-less-p a b)))

(defun ical:number-of-weeks (year &optional weekstart)
  "Return the number of weeks in (Gregorian) YEAR.

RFC5545 defines week 1 as the first week to include at least four days
in the year.  Weeks are assumed to start on Monday (= 1) unless WEEKSTART
is specified, in which case it should be an integer between 0 (= Sunday)
and 6 (= Saturday)."
  ;; There are 53 weeks in a year if Jan 1 is the fourth day after
  ;; WEEKSTART, e.g. if the week starts on Monday and Jan 1 is a
  ;; Thursday, or in a leap year if Jan 1 is the third day after WEEKSTART
  (let* ((jan1wd (calendar-day-of-week (list 1 1 year)))
         (delta (mod (- jan1wd (or weekstart 1)) 7)))
    (if (or (= 4 delta)
            (and (= 3 delta) (calendar-leap-year-p year)))
        53
      52)))

(defun ical:start-of-weekno (weekno year &optional weekstart)
  "Return the start of the WEEKNOth week in the (Gregorian) YEAR.

RFC5545 defines week 1 as the first week to include at least four days
in the year.  Weeks are assumed to start on Monday (= 1) unless WEEKSTART
is specified, in which case it should be an integer between 0 (= Sunday)
and 6 (= Saturday).  The returned value is an `icalendar-date'.

If WEEKNO is negative, it refers to the WEEKNOth week before the end of
the year: -1 is the last week of the year, -2 second to last, etc."
  (calendar-gregorian-from-absolute
   (+
    (* 7 (if (< 0 weekno)
             (1- weekno)
           (+ 1 weekno (ical:number-of-weeks year weekstart))))
    (calendar-dayname-on-or-before
     (or weekstart 1)
     ;; Three days after Jan 1. gives us the nearest occurrence;
     ;; see `calendar-dayname-on-or-before'
     (+ 3 (calendar-absolute-from-gregorian (list 1 1 year)))))))

(defun ical:nth-weekday-in (n weekday year &optional month)
  "Return the Nth WEEKDAY in YEAR or MONTH.

If MONTH is specified, it refers to MONTH in YEAR, and N acts as an
index for WEEKDAYs within the month.  Otherwise, N acts as an index for
WEEKDAYs within the entire YEAR.

N should be an integer.  If N<0, it counts from the end of the month or
year: if N=-1, it refers to the last WEEKDAY in the month or year, if
N=-2 the second to last, and so on."
  (if month
      (calendar-nth-named-day n weekday month year)
    (let* ((jan1 (calendar-absolute-from-gregorian (list 1 1 year)))
           (dec31 (calendar-absolute-from-gregorian (list 12 31 year))))
      ;; Adapted from `calendar-nth-named-absday'.
      ;; TODO: we could generalize that function to make month an optional
      ;; argument, but that would mean changing its interface.
      (calendar-gregorian-from-absolute
       (if (> n 0)
           (+ (* 7 (1- n))
              (calendar-dayname-on-or-before
               weekday
               (+ 6 jan1)))
         (+ (* 7 (1+ n))
            (calendar-dayname-on-or-before
             weekday
             dec31)))))))

(provide 'icalendar-utils)
;; Local Variables:
;; read-symbol-shorthands: (("ical:" . "icalendar-"))
;; End:
;;; icalendar-utils.el ends here
