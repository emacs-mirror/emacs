;;; icalendar-recur-tests.el --- Tests for icalendar-recur  -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Free Software Foundation, Inc.

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

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'ert)
(eval-when-compile (require 'icalendar-macs))
(require 'icalendar-recur)
(require 'icalendar-utils)
(require 'icalendar-parser)
(require 'icalendar-ast)

;; Some constants for tests that use time zones:
(defconst ict:tz-eastern
  (ical:parse-from-string 'ical:vtimezone
"BEGIN:VTIMEZONE
TZID:America/New_York
LAST-MODIFIED:20050809T050000Z
BEGIN:DAYLIGHT
DTSTART:19670430T020000
RRULE:FREQ=YEARLY;BYMONTH=4;BYDAY=-1SU;UNTIL=19730429T070000Z
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
BEGIN:STANDARD
DTSTART:19671029T020000
RRULE:FREQ=YEARLY;BYMONTH=10;BYDAY=-1SU;UNTIL=20061029T060000Z
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
TZNAME:EST
END:STANDARD
BEGIN:DAYLIGHT
DTSTART:19740106T020000
RDATE:19750223T020000
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
BEGIN:DAYLIGHT
DTSTART:19760425T020000
RRULE:FREQ=YEARLY;BYMONTH=4;BYDAY=-1SU;UNTIL=19860427T070000Z
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
BEGIN:DAYLIGHT
DTSTART:19870405T020000
RRULE:FREQ=YEARLY;BYMONTH=4;BYDAY=1SU;UNTIL=20060402T070000Z
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
BEGIN:DAYLIGHT
DTSTART:20070311T020000
RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=2SU
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
BEGIN:STANDARD
DTSTART:20071104T020000
RRULE:FREQ=YEARLY;BYMONTH=11;BYDAY=1SU
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
TZNAME:EST
END:STANDARD
END:VTIMEZONE
")
"`icalendar-vtimezone' representing America/New_York (Eastern) time.")

(defconst ict:est-latest
  (ical:with-component ict:tz-eastern
      ((ical:standard :all stds))
    (seq-find (lambda (obs)
                (ical:date-time=
                 (ical:make-date-time :year 2007 :month 11 :day 4
                                      :hour 2 :minute 0 :second 0)
                 (ical:with-property-of obs 'ical:dtstart nil value)))
              stds))
  "The observance of Eastern Standard Time which began 2007-11-04")

(defconst ict:edt-latest
  (ical:with-component ict:tz-eastern
      ((ical:daylight :all dls))
    (seq-find (lambda (obs)
                (ical:date-time=
                 (ical:make-date-time :year 2007 :month 3 :day 11
                                      :hour 2 :minute 0 :second 0)
                 (ical:with-property-of obs 'ical:dtstart nil value)))
              dls))
  "The observance of Eastern Daylight Time which began 2007-03-11")

(defconst ict:est -18000  ;; = -0500
  "UTC offset for Eastern Standard Time")

(defconst ict:edt -14400 ;; = -0400
  "UTC offset for Eastern Daylight Time")


;; Tests for basic functions:

(ert-deftest ict:recur-bysetpos-filter ()
  "Test that `icr:make-bysetpos-filter' filters correctly by position"
  (let* ((t1 (list 1 1 2024))
         (t2 (list 2 1 2024))
         (t3 (list 12 30 2024))
         (dts (list t1 t2 t3))
         (filter (icr:make-bysetpos-filter (list 1 -1)))
         (filtered (funcall filter dts)))
    (should (member t1 filtered))
    (should (member t3 filtered))
    (should-not (member t2 filtered))))

(ert-deftest ict:recur-yearday-number ()
  "Test that `calendar-date-from-day-of-year' finds correct dates"
  (let* ((year 2025)
         (daynos (list '(1 . (1 1 2025))
                       '(8 . (1 8 2025))
                       '(-1 . (12 31 2025))
                       '(363 . (12 29 2025)))))
    (dolist (d daynos)
      (let ((dayno (car d))
            (date (cdr d)))
        (should
         (equal date (calendar-date-from-day-of-year year dayno)))))))

(ert-deftest ict:date-time-add ()
  "Does `ical:date-time-add' correctly handle time zone transitions?"
  ;; A sum that does not use a time zone at all:
  (let* ((dt (ical:make-date-time :year 2007 :month 1 :day 1
                                  :hour 12 :minute 0 :second 0))
         (delta (make-decoded-time :day 2))
         (expected (ical:date-time-variant dt :day 3)))
    (should (equal expected (ical:date-time-add dt delta))))

  ;; A sum that does not cross an observance boundary:
  (let* ((dt (ical:make-date-time :year 2007 :month 2 :day 1
                                  :hour 12 :minute 0 :second 0
                                  :zone ict:est :dst nil))
         (delta (make-decoded-time :day 2))
         (expected (ical:date-time-variant dt :day 3 :tz 'preserve)))
    (should (equal expected (ical:date-time-add dt delta ict:tz-eastern))))

  ;; A sum that crosses the Std->DST boundary and should preserve clock time:
  (let* ((dt (ical:make-date-time :year 2007 :month 3 :day 10
                                  :hour 12 :minute 0 :second 0
                                  :zone ict:est :dst nil))
         (delta (make-decoded-time :day 2))
         (expected (ical:date-time-variant dt :day 12 :zone ict:edt :dst t)))
    (should (equal expected (ical:date-time-add dt delta ict:tz-eastern))))

  ;; A sum that crosses the Std->DST boundary and should be exactly 48 hours later:
  (let* ((dt (ical:make-date-time :year 2007 :month 3 :day 10
                                  :hour 12 :minute 0 :second 0
                                  :zone ict:est :dst nil))
         (delta (make-decoded-time :hour 48))
         (expected (ical:date-time-variant dt :day 12 :hour 13
                                           :zone ict:edt :dst t)))
    (should (equal expected (ical:date-time-add dt delta ict:tz-eastern))))

  ;; A sum that crosses the DST->Std boundary and should preserve clock time:
  (let* ((dt (ical:make-date-time :year 2007 :month 11 :day 3
                                  :hour 12 :minute 0 :second 0
                                  :zone ict:edt :dst t))
         (delta (make-decoded-time :day 2))
         (expected (ical:date-time-variant dt :day 5 :zone ict:est :dst nil)))
    (should (equal expected (ical:date-time-add dt delta ict:tz-eastern))))

  ;; A sum that crosses the DST->Std boundary and should be exactly 48 hours later:
  (let* ((dt (ical:make-date-time :year 2007 :month 11 :day 3
                                  :hour 12 :minute 0 :second 0
                                  :zone ict:edt :dst t))
         (delta (make-decoded-time :hour 48))
         (expected (ical:date-time-variant dt :day 5 :hour 11
                                           :zone ict:est :dst nil)))
    (should (equal expected (ical:date-time-add dt delta ict:tz-eastern))))

  ;; A sum that lands exactly on the Std->DST boundary and should result
  ;; in a clock time one hour later:
  (let* ((dt (ical:make-date-time :year 2007 :month 3 :day 10
                                  :hour 2 :minute 0 :second 0
                                  :zone ict:est :dst nil))
         (delta (make-decoded-time :hour 24))
         (expected (ical:date-time-variant dt :day 11 :hour 3
                                           :zone ict:edt :dst t)))
    (should (equal expected (ical:date-time-add dt delta ict:tz-eastern))))

  ;; A sum that lands exactly on the DST->Std boundary and should result
  ;; in a clock time one hour earlier:
  (let* ((dt (ical:make-date-time :year 2007 :month 11 :day 3
                                  :hour 2 :minute 0 :second 0
                                  :zone ict:edt :dst t))
         (delta (make-decoded-time :hour 24))
         (expected (ical:date-time-variant dt :day 4 :hour 1
                                           :zone ict:est :dst nil)))
    (should (equal expected (ical:date-time-add dt delta ict:tz-eastern)))))

(ert-deftest ict:recur-nonexistent-date-time-p ()
  "Does `icr:nonexistent-date-time-p' correctly identify nonexistent times?"
  (let*  ((dst-onset (ical:make-date-time :year 2025 :month 3 :day 9
                                          :hour 2 :minute 0 :second 0
                                          :zone ict:est :dst nil))
          ;; 2:30 AM falls into the gap when shifting from 2AM EST to 3AM EDT:
          (nonexistent1 (ical:make-date-time :year 2025 :month 3 :day 9
                                             :hour 2 :minute 30 :second 0
                                             :zone ict:est :dst nil))
          (nonexistent2 (ical:date-time-variant nonexistent1
                                                :zone ict:edt :dst t))
          (std-onset (ical:make-date-time :year 2025 :month 11 :day 2
                                          :hour 2 :minute 0 :second 0
                                          :zone ict:edt :dst t))
          ;; 1:30AM around the shift back to EST exists twice (once in
          ;; EDT, once in EST) and should not be nonexistent:
          (existent1 (ical:make-date-time :year 2025 :month 11 :day 2
                                          :hour 1 :minute 30 :second 0
                                          :zone ict:edt :dst t))
          (existent2 (ical:make-date-time :year 2025 :month 11 :day 2
                                          :hour 1 :minute 30 :second 0
                                          :zone ict:est :dst nil)))
    (should (icr:nonexistent-date-time-p nonexistent1 dst-onset ict:edt-latest))
    (should (icr:nonexistent-date-time-p nonexistent2 dst-onset ict:edt-latest))
    (should-not
     (icr:nonexistent-date-time-p existent1 std-onset ict:est-latest))
    (should-not
     (icr:nonexistent-date-time-p existent2 std-onset ict:est-latest))))

(ert-deftest ict:recur-date-time-occurs-twice-p ()
  "Does `icr:date-time-occurs-twice-p' correctly identify times that occur twice?"
  (let*  ((std-onset (ical:make-date-time :year 2025 :month 11 :day 2
                                          :hour 2 :minute 0 :second 0
                                          :zone ict:edt :dst t))
          ;; 1:00, 1:30 AM occur twice when shifting from 2AM EDT to 1AM EST:
          (twice1 (ical:make-date-time :year 2025 :month 11 :day 2
                                       :hour 1 :minute 0 :second 0))
          (twice2 (ical:make-date-time :year 2025 :month 11 :day 2
                                       :hour 1 :minute 30 :second 0))
          ;; 12:59 AM, 2AM should not occur twice:
          (once1 (ical:make-date-time :year 2025 :month 11 :day 2
                                      :hour 0 :minute 59 :second 0
                                      :zone ict:edt :dst t))
          (once2 (ical:make-date-time :year 2025 :month 11 :day 2
                                      :hour 2 :minute 0 :second 0
                                      :zone ict:est :dst nil)))
    (should (icr:date-time-occurs-twice-p twice1 std-onset ict:est-latest))
    (should (icr:date-time-occurs-twice-p twice2 std-onset ict:est-latest))
    (should-not
     (icr:date-time-occurs-twice-p once1 std-onset ict:est-latest))
    (should-not
     (icr:date-time-occurs-twice-p once2 std-onset ict:est-latest))))

(ert-deftest ict:recur-find-secondly-interval ()
  "Does `icr:find-secondly-interval' find correct intervals?"
  (let* ((dtstart (ical:make-date-time :year 2025 :month 1 :day 1
                                       :hour 0 :minute 0 :second 0
                                       ;; Use UTC for the tests with no
                                       ;; time zone, so that the results
                                       ;; don't depend on system's local time
                                       :zone 0))
         (dtstart/tz (ical:date-time-variant dtstart :zone ict:est :dst nil)))

    ;; Year numbers are monotonically increasing in the following test cases,
    ;; to make it easy to tell which of them fails.

    ;; No timezone, just clock time, around a target that doesn't fall on
    ;; an interval boundary:
    (let* ((target (ical:date-time-variant dtstart :year 2026 :second 5 :zone 0))
           (expected-int
            (list
             (ical:date-time-variant target :second 0 :tz 'preserve)
             (ical:date-time-variant target :second 1 :tz 'preserve)
             (ical:date-time-variant target :second 10 :tz 'preserve))))
      (should
       (equal expected-int
              (icr:find-secondly-interval target dtstart 10))))

    ;; No timezone, just clock time, around a target that does fall on
    ;; an interval boundary:
    (let* ((target (ical:date-time-variant dtstart :year 2027 :second 10 :zone 0))
           (expected-int
            (list
             (ical:date-time-variant target :second 10 :tz 'preserve)
             (ical:date-time-variant target :second 11 :tz 'preserve)
             (ical:date-time-variant target :second 20 :tz 'preserve))))
      (should
       (equal expected-int
              (icr:find-secondly-interval target dtstart 10))))

    ;; With timezone, around a target that falls on an interval
    ;; boundary, in the same observance:
    (let* ((target (ical:date-time-variant dtstart/tz
                                           :year 2028 :month 2 :second 20
                                           :zone ict:est :dst nil))
           (expected-int
            (list
             (ical:date-time-variant target :second 20 :tz 'preserve)
             (ical:date-time-variant target :second 21 :tz 'preserve)
             (ical:date-time-variant target :second 30 :tz 'preserve))))
      (should
       (equal expected-int
              (icr:find-secondly-interval target dtstart/tz 10
                                          ict:tz-eastern))))

    ;; With timezone, around a target that does not fall on an interval
    ;; boundary, and after the time zone observance shift:
    (let* ((target (ical:date-time-variant dtstart/tz
                                           :year 2029 :month 5 :second 30
                                           :zone ict:edt :dst t))
           (expected-int
            (list
             (ical:date-time-variant target :second 30 :tz 'preserve)
             (ical:date-time-variant target :second 31 :tz 'preserve)
             (ical:date-time-variant target :second 40 :tz 'preserve))))
      (should
       (equal expected-int
              (icr:find-secondly-interval target dtstart/tz 10 ict:tz-eastern))))

    ;; With timezone, around a target that falls into the gap in local
    ;; times and thus does not exist as a local time.  In this case, what
    ;; is supposed to happen is that the clock time value in the [observance]
    ;; recurrences "is interpreted using the UTC offset before the gap
    ;; in local times."  So we should get the same absolute times back,
    ;; but re-decoded into the new observance, i.e., one hour later.
    (let* ((target (ical:date-time-variant dtstart/tz
                                           :year 2030 :month 3 :day 10
                                           :hour 2 :minute 30 :second 0
                                           :zone ict:est :dst nil))
           (expected-int
            (list
             (ical:date-time-variant target :hour 3 :second 0
                                     :zone ict:edt :dst t)
             (ical:date-time-variant target :hour 3 :second 1
                                     :zone ict:edt :dst t)
             (ical:date-time-variant target
                                     :hour 3 :second 10
                                     :zone ict:edt :dst t))))
      (should
       (equal expected-int
              (icr:find-secondly-interval target dtstart/tz 10 ict:tz-eastern))))

    ;; With timezone, with a "pathological" interval size of 59 seconds.
    ;; There should be no problem with this case, because the interval
    ;; bounds calculation is done in absolute time, but it's annoying to
    ;; calculate the expected interval by hand:
    (let* ((target (ical:date-time-variant dtstart/tz
                                           :year 2031 :month 4 :day 15
                                           :hour 12 :minute 0 :second 0
                                           :zone ict:edt :dst t))
           (intsize 59)
           (expected-int
            (list
             (ical:date-time-variant target :hour 11 :minute 59 :second 16
                                     :tz 'preserve)
             (ical:date-time-variant target :hour 11 :minute 59 :second 17
                                     :tz 'preserve)
             (ical:date-time-variant target :hour 12 :minute 0 :second 15
                                     :tz 'preserve))))
      (should
       (equal expected-int
              (icr:find-secondly-interval target dtstart/tz intsize
                                          ict:tz-eastern))))))

(ert-deftest ict:recur-find-minutely-interval ()
  "Does `icr:find-minutely-interval' find correct intervals?"
  (let* ((dtstart (ical:make-date-time :year 2025 :month 1 :day 1
                                       :hour 0 :minute 0
                                       ;; make sure intervals are
                                       ;; bounded on whole minutes:
                                       :second 23))
         (dtstart/tz (ical:date-time-variant dtstart :zone ict:est :dst nil)))

    ;; Year numbers are monotonically increasing in the following test cases,
    ;; to make it easy to tell which of them fails.

    ;; No timezone, just a fixed offset, around a target that doesn't fall on
    ;; an interval boundary:
    (let* ((target (ical:date-time-variant dtstart :year 2026 :minute 5))
           (intsize 10)
           (expected-int
            (list
             (ical:date-time-variant target :minute 0 :second 0)
             (ical:date-time-variant target :minute 1 :second 0)
             (ical:date-time-variant target :minute 10 :second 0))))
      (should
       (equal expected-int
              (icr:find-minutely-interval target dtstart intsize))))

    ;; No timezone, just clock time, around a target that does fall on
    ;; an interval boundary:
    (let* ((target (ical:date-time-variant dtstart :year 2027 :minute 10))
           (intsize 10)
           (expected-int
            (list
             (ical:date-time-variant target :minute 10 :second 0)
             (ical:date-time-variant target :minute 11 :second 0)
             (ical:date-time-variant target :minute 20 :second 0))))
      (should
       (equal expected-int
              (icr:find-minutely-interval target dtstart intsize))))

    ;; With timezone, around a target that falls on an interval
    ;; boundary, in the same observance:
    (let* ((target (ical:date-time-variant dtstart/tz
                                           :year 2028 :month 2 :minute 20
                                           :zone ict:est :dst nil))
           (intsize 10)
           (expected-int
            (list
             (ical:date-time-variant target :minute 20 :second 0
                                     :zone ict:est :dst nil)
             (ical:date-time-variant target :minute 21 :second 0
                                     :zone ict:est :dst nil)
             (ical:date-time-variant target :minute 30 :second 0
                                     :zone ict:est :dst nil))))
      (should
       (equal expected-int
              (icr:find-minutely-interval target dtstart/tz intsize
                                          ict:tz-eastern))))

    ;; With timezone, around a target that does not fall on an interval
    ;; boundary, and after the time zone observance shift:
    (let* ((target (ical:date-time-variant dtstart/tz
                                           :year 2029 :month 5 :minute 30
                                           :zone ict:edt :dst t))
           (intsize 10)
           (expected-int
            (list
             (ical:date-time-variant target :minute 30 :second 0
                                     :zone ict:edt :dst t)
             (ical:date-time-variant target :minute 31 :second 0
                                     :zone ict:edt :dst t)
             (ical:date-time-variant target :minute 40 :second 0
                                     :zone ict:edt :dst t))))
      (should
       (equal expected-int
              (icr:find-minutely-interval target dtstart/tz intsize
                                          ict:tz-eastern))))

    ;; With timezone, around a target that falls into the gap in local
    ;; times and thus does not exist as a local time.  In this case, what
    ;; is supposed to happen is that the clock time value in the [observance]
    ;; recurrences "is interpreted using the UTC offset before the gap
    ;; in local times."  So we should get the same absolute times back,
    ;; but re-decoded into the new observance, i.e., one hour later.
    (let* ((target (ical:date-time-variant dtstart/tz
                                           :year 2030 :month 3 :day 10
                                           :hour 2 :minute 30 :second 0
                                           :zone ict:est :dst nil))
           (intsize 10)
           (expected-int
            (list
             (ical:date-time-variant target :hour 3 :minute 30 :second 0
                                     :zone ict:edt :dst t)
             (ical:date-time-variant target :hour 3 :minute 31 :second 0
                                     :zone ict:edt :dst t)
             (ical:date-time-variant target
                                     :hour 3 :minute 40 :second 0
                                     :zone ict:edt :dst t))))
      (should
       (equal expected-int
              (icr:find-minutely-interval target dtstart/tz intsize
                                          ict:tz-eastern))))))

(ert-deftest ict:recur-find-hourly-interval ()
  "Does `icr:find-hourly-interval' find correct intervals?"
  (let* ((dtstart (ical:make-date-time :year 2025 :month 1 :day 1
                                       :hour 0
                                       ;; make sure intervals are bounded on
                                       ;; whole hours:
                                       :minute 11 :second 23))
         (dtstart/tz (ical:date-time-variant dtstart :zone ict:est :dst nil)))

    ;; Year numbers are monotonically increasing in the following test cases,
    ;; to make it easy to tell which of them fails.
    ;; No timezone, just clock time, around a target that doesn't fall on
    ;; an interval boundary:
    (let* ((target (ical:date-time-variant dtstart :year 2026 :hour 5))
           (intsize 10)
           (expected-int
            (list
             (ical:date-time-variant target :hour 0 :minute 0 :second 0)
             (ical:date-time-variant target :hour 1 :minute 0 :second 0)
             (ical:date-time-variant target :hour 10 :minute 0 :second 0))))
      (should
       (equal expected-int
              (icr:find-hourly-interval target dtstart intsize))))

    ;; No timezone, just clock time, around a target that does fall on
    ;; an interval boundary:
    (let* ((target (ical:date-time-variant dtstart :year 2027 :hour 10))
           (intsize 10)
           (expected-int
            (list
             (ical:date-time-variant target :hour 10 :minute 0 :second 0)
             (ical:date-time-variant target :hour 11 :minute 0 :second 0)
             (ical:date-time-variant target :hour 20 :minute 0 :second 0))))
      (should
       (equal expected-int
              (icr:find-hourly-interval target dtstart intsize))))

    ;; With timezone, around a target that falls on an interval
    ;; boundary, in the same observance:
    (let* ((target (ical:date-time-variant dtstart/tz
                                           :year 2028 :month 2 :hour 10
                                           :zone ict:est :dst nil))
           (intsize 2)
           (expected-int
            (list
             (ical:date-time-variant target :hour 10 :minute 0 :second 0
                                     :zone ict:est :dst nil)
             (ical:date-time-variant target :hour 11 :minute 0 :second 0
                                     :zone ict:est :dst nil)
             (ical:date-time-variant target :hour 12 :minute 0 :second 0
                                     :zone ict:est :dst nil))))
      (should
       (equal expected-int
              (icr:find-hourly-interval target dtstart/tz intsize
                                        ict:tz-eastern))))

    ;; With time zone, around a target that does not fall on an interval
    ;; boundary, and after the time zone observance shift.  Note that
    ;; because of our decision to calculate with absolute times in
    ;; SECONDLY/MINUTELY/HOURLY rules (see `icr:find-secondly-recurrence-rule')
    ;; the interval clock times shift an hour here:
    (let* ((target (ical:date-time-variant dtstart/tz
                                           :year 2029 :month 5 :hour 12
                                           :zone ict:edt :dst t))
           (intsize 2)
           (expected-int
            (list
             (ical:date-time-variant target :hour 11 :minute 0 :second 0
                                     :zone ict:edt :dst t)
             (ical:date-time-variant target :hour 12 :minute 0 :second 0
                                     :zone ict:edt :dst t)
             (ical:date-time-variant target :hour 13 :minute 0 :second 0
                                     :zone ict:edt :dst t))))
      (should
       (equal expected-int
              (icr:find-hourly-interval target dtstart/tz intsize
                                        ict:tz-eastern))))

    ;; With timezone, around a target that falls into the gap in local
    ;; times and thus does not exist as a local time.  In this case, what
    ;; is supposed to happen is that the clock time value in the [observance]
    ;; recurrences "is interpreted using the UTC offset before the gap
    ;; in local times."  So we should get the same absolute times back,
    ;; but re-decoded into the new observance, i.e., one hour later.
    (let* ((target (ical:make-date-time :year 2030 :month 3 :day 10
                                        :hour 2 :minute 30 :second 30
                                        :zone ict:est :dst nil))
           (intsize 2)
           (expected-int
            (list
             (ical:date-time-variant target :hour 3 :minute 0 :second 0
                                     :zone ict:edt :dst t)
             (ical:date-time-variant target :hour 4 :minute 0 :second 0
                                     :zone ict:edt :dst t)
             (ical:date-time-variant target :hour 5 :minute 0 :second 0
                                     :zone ict:edt :dst t))))
      (should
       (equal expected-int
              (icr:find-hourly-interval target dtstart/tz intsize
                                        ict:tz-eastern))))))

(ert-deftest ict:recur-find-daily-interval-w/date ()
  "Does `icr:find-daily-interval' find correct date intervals?"
  (let* ((dtstart (list 1 8 2025)))
    ;; Since all the results should be the same after the initial
    ;; calculation of the absolute dates DTSTART and TARGET, we just
    ;; test one simple case here and test with date-times more
    ;; thoroughly below.

    ;; A target that doesn't fall on an interval boundary:
    (let* ((target (list 1 9 2026))
           (intsize 7)
           (expected-int
            (list
             (ical:make-date-time :year 2026 :month 1 :day 7
                                  :hour 0 :minute 0 :second 0)
             (ical:make-date-time :year 2026 :month 1 :day 8
                                  :hour 0 :minute 0 :second 0)
             (ical:make-date-time :year 2026 :month 1 :day 14
                                  :hour 0 :minute 0 :second 0))))
      (should (equal expected-int
                     (icr:find-daily-interval target dtstart intsize))))))

(ert-deftest ict:recur-find-daily-interval-w/date-time ()
  "Does `icr:find-daily-interval' find correct date-time intervals?"
  (let* ((dtstart (ical:make-date-time :year 2025 :month 1 :day 8 ; a Wednesday
                                       ;; make sure intervals are bounded on
                                       ;; whole days:
                                       :hour 7 :minute 11 :second 23))
         (dtstart/tz (ical:date-time-variant dtstart :zone ict:est :dst nil)))

    ;; Year numbers are monotonically increasing in the following test cases,
    ;; to make it easy to tell which of them fails.

    ;; No timezone, just clock time, around a target that doesn't fall on
    ;; an interval boundary:
    (let* ((target (ical:date-time-variant dtstart
                                              :year 2026 :month 1 :day 9))
           (intsize 7)
           (expected-int
            (list
             (ical:date-time-variant target :day 7 :hour 0 :minute 0 :second 0)
             (ical:date-time-variant target :day 8 :hour 0 :minute 0 :second 0)
             (ical:date-time-variant target :day 14
                                     :hour 0 :minute 0 :second 0))))
      (should
       (equal expected-int
              (icr:find-daily-interval target dtstart intsize))))

    ;; No timezone, just clock time, around a target that does fall on
    ;; an interval boundary:
    (let* ((target (ical:date-time-variant dtstart :year 2027 :month 1 :day 6))
           (intsize 7)
           (expected-int
            (list
             (ical:date-time-variant target :day 6 :hour 0 :minute 0 :second 0)
             (ical:date-time-variant target :day 7 :hour 0 :minute 0 :second 0)
             (ical:date-time-variant target :day 13 :hour 0 :minute 0 :second 0))))
      (should
       (equal expected-int
              (icr:find-daily-interval target dtstart intsize))))

    ;; With timezone, around a target that falls on an interval
    ;; boundary, in the same observance:
    (let* ((target (ical:date-time-variant dtstart/tz :year 2028 :month 2 :day 2
                                           :zone ict:est :dst nil))
           (intsize 7)
           (expected-int
            (list
             (ical:date-time-variant target :day 2 :hour 0 :minute 0 :second 0
                                     :tz 'preserve)
             (ical:date-time-variant target :day 3 :hour 0 :minute 0 :second 0
                                     :tz 'preserve)
             (ical:date-time-variant target :day 9 :hour 0 :minute 0 :second 0
                                     :tz 'preserve))))
      (should
       (equal expected-int
              (icr:find-daily-interval target dtstart/tz intsize ict:tz-eastern))))

    ;; With time zone, around a target that does not fall on an interval
    ;; boundary, and after the time zone observance shift.
    (let* ((target (ical:date-time-variant dtstart/tz
                                           :year 2029 :month 5 :day 28
                                           :zone ict:edt :dst t))
           (intsize 7)
           (expected-int
            (list
             (ical:date-time-variant target :day 23 :hour 0 :minute 0 :second 0
                                     :tz 'preserve)
             (ical:date-time-variant target :day 24 :hour 0 :minute 0 :second 0
                                     :tz 'preserve)
             (ical:date-time-variant target :day 30 :hour 0 :minute 0 :second 0
                                     :tz 'preserve))))
      (should
       (equal expected-int
              (icr:find-daily-interval target dtstart/tz intsize
                                       ict:tz-eastern))))))

(ert-deftest ict:recur-find-weekly-interval-w/date ()
  "Does `icr:find-weekly-interval' find correct date intervals?"
  (let* ((dtstart '(1 8 2025)))
    ;; Since all the results should be the same after the initial
    ;; calculation of the absolute dates DTSTART and TARGET, we just
    ;; test one simple case here and test with date-times more
    ;; thoroughly below.

    ;; A target that doesn't fall on an interval boundary:
    (let* ((target '(1 9 2026))
           (intsize 2)
           (expected-int-mon
            (list
             (ical:make-date-time :year 2026 :month 1 :day 5
                                  :hour 0 :minute 0 :second 0)
             (ical:make-date-time :year 2026 :month 1 :day 12
                                  :hour 0 :minute 0 :second 0)
             (ical:make-date-time :year 2026 :month 1 :day 19
                                  :hour 0 :minute 0 :second 0))))
      (should (equal expected-int-mon
                     (icr:find-weekly-interval target dtstart intsize))))))

(ert-deftest ict:recur-find-weekly-interval-w/date-time ()
  "Does `icr:find-weekly-interval' find correct date-time intervals?"
  (let* ((dtstart (ical:make-date-time :year 2025 :month 1 :day 8 ; a Wednesday
                                       ;; make sure intervals are bounded on
                                       ;; whole days:
                                       :hour 7 :minute 11 :second 23)))

    ;; Year numbers are monotonically increasing in the following test cases,
    ;; to make it easy to tell which of them fails.

    ;; No timezone, just clock time, around a target that doesn't fall on
    ;; an interval boundary:
    (let* ((target (ical:date-time-variant dtstart :year 2026 :month 1 :day 9))
           (intsize 2)
           (weds 3)
           ;; expected interval for Monday (default) week start:
           (expected-int-mon
            (list
             (ical:date-time-variant target :day 5 :hour 0 :minute 0 :second 0)
             (ical:date-time-variant target :day 12 :hour 0 :minute 0 :second 0)
             (ical:date-time-variant target :day 19 :hour 0 :minute 0 :second 0)))
           ;; expected interval for Wednesday week start:
           (expected-int-wed
            (list
             (ical:date-time-variant target :day 7 :hour 0 :minute 0 :second 0)
             (ical:date-time-variant target :day 14 :hour 0 :minute 0 :second 0)
             (ical:date-time-variant target :day 21 :hour 0 :minute 0 :second 0))))
      (should
       (equal expected-int-mon
              (icr:find-weekly-interval target dtstart intsize)))
      (should
       (equal expected-int-wed
              (icr:find-weekly-interval target dtstart intsize weds))))

    ;; Around a target that does fall on an interval boundary, Monday week start:
    (let* ((target (ical:date-time-variant dtstart :year 2027 :month 1 :day 4))
           (intsize 3)
           ;; expected interval for Monday (default) week start:
           (expected-int-mon
            (list
             (ical:date-time-variant target :year 2026 :month 12 :day 21
                                     :hour 0 :minute 0 :second 0)
             (ical:date-time-variant target :year 2026 :month 12 :day 28
                                     :hour 0 :minute 0 :second 0)
             (ical:date-time-variant target :day 11
                                     :hour 0 :minute 0 :second 0))))
      (should
       (equal expected-int-mon
              (icr:find-weekly-interval target dtstart intsize))))

    ;; Around a target that does fall on an interval boundary, Sunday week start:
    (let* ((target (ical:date-time-variant dtstart :year 2028 :month 1 :day 2))
           (intsize 3)
           (sun 0)
           ;; expected interval for Sunday week start:
           (expected-int-sun
            (list
             (ical:date-time-variant target :day 2 :hour 0 :minute 0 :second 0)
             (ical:date-time-variant target :day 9 :hour 0 :minute 0 :second 0)
             (ical:date-time-variant target :day 23 :hour 0 :minute 0 :second 0))))
      (should
       (equal expected-int-sun
              (icr:find-weekly-interval target dtstart intsize sun))))))

(ert-deftest ict:recur-find-monthly-interval ()
  "Does `icr:find-monthly-interval' find correct intervals?"
  ;; Year numbers are monotonically increasing in the following test cases,
  ;; to make it easy to tell which of them fails.

  ;; One test with dates, to make sure that works:
  (let* ((dtstart '(1 8 2025))
         (target '(10 9 2025))
           (intsize 5)
           (expected-int
            (list
             (ical:make-date-time :year 2025 :month 6 :day 1
                                  :hour 0 :minute 0 :second 0)
             (ical:make-date-time :year 2025 :month 7 :day 1
                                  :hour 0 :minute 0 :second 0)
             (ical:make-date-time :year 2025 :month 11 :day 1
                                  :hour 0 :minute 0 :second 0))))
      (should (equal expected-int
                     (icr:find-monthly-interval target dtstart intsize))))

  ;; Around a target that doesn't fall on an interval boundary:
  (let* ((dtstart (ical:make-date-time :year 2025 :month 1 :day 1
                                       ;; make sure intervals are bounded on
                                       ;; whole days:
                                       :hour 7 :minute 11 :second 23))
         (target (ical:date-time-variant dtstart :year 2026 :month 3 :day 9))
         (intsize 2)
         (expected-int
          (list
           (ical:date-time-variant target :day 1 :hour 0 :minute 0 :second 0)
           (ical:date-time-variant target :month 4 :day 1
                                   :hour 0 :minute 0 :second 0)
           (ical:date-time-variant target :month 5 :day 1
                                   :hour 0 :minute 0 :second 0))))
    (should
     (equal expected-int
            (icr:find-monthly-interval target dtstart intsize))))

  ;; Around a target that does fall on an interval boundary:
  (let* ((dtstart (ical:make-date-time :year 2025 :month 1 :day 1
                                       ;; make sure intervals are bounded on
                                       ;; whole days:
                                       :hour 7 :minute 11 :second 23))
         (target (ical:date-time-variant dtstart :year 2027 :month 5 :day 1))
         (intsize 7)
         (expected-int
          (list
           (ical:date-time-variant target :year 2027 :month 5 :day 1
                                   :hour 0 :minute 0 :second 0)
           (ical:date-time-variant target :year 2027 :month 6 :day 1
                                   :hour 0 :minute 0 :second 0)
           (ical:date-time-variant target :year 2027 :month 12 :day 1
                                   :hour 0 :minute 0 :second 0))))
    (should
     (equal expected-int
            (icr:find-monthly-interval target dtstart intsize))))

  ;; Around a target that does not fall on an interval boundary, where
  ;; start month > target month
  (let* ((dtstart (ical:make-date-time :year 2028 :month 11 :day 11
                                       :hour 11 :minute 11 :second 11))
         (target (ical:date-time-variant dtstart
                                         :year 2029 :month 4 :day 15))
         (intsize 2)
         (expected-int
          (list
           (ical:date-time-variant target :year 2029 :month 3 :day 1
                                   :hour 0 :minute 0 :second 0)
           (ical:date-time-variant target :year 2029 :month 4 :day 1
                                   :hour 0 :minute 0 :second 0)
           (ical:date-time-variant target :year 2029 :month 5 :day 1
                                   :hour 0 :minute 0 :second 0))))
      (should
       (equal expected-int
              (icr:find-monthly-interval target dtstart intsize))))

  ;; Around a target that falls on an interval boundary, where
  ;; start month > target month
  (let* ((dtstart (ical:make-date-time :year 2029 :month 11 :day 11
                                       :hour 11 :minute 11 :second 11 ))
         (target (ical:date-time-variant dtstart
                                         :year 2030 :month 5 :day 1))
         (intsize 2)
         (expected-int
          (list
           (ical:date-time-variant target :year 2030 :month 5 :day 1
                                   :hour 0 :minute 0 :second 0)
           (ical:date-time-variant target :year 2030 :month 6 :day 1
                                   :hour 0 :minute 0 :second 0)
           (ical:date-time-variant target :year 2030 :month 7 :day 1
                                   :hour 0 :minute 0 :second 0))))
      (should
       (equal expected-int
              (icr:find-monthly-interval target dtstart intsize))))

  ;; Around a target that falls on an interval boundary, where
  ;; start month = target month
  (let* ((dtstart (ical:make-date-time :year 2031 :month 11 :day 11
                                       :hour 11 :minute 11 :second 11 ))
         (target (ical:date-time-variant dtstart :year 2032 :month 11 :day 11))
         (intsize 2)
         (expected-int
          (list
           (ical:date-time-variant target :year 2032 :month 11 :day 1
                                   :hour 0 :minute 0 :second 0)
           (ical:date-time-variant target :year 2032 :month 12 :day 1
                                   :hour 0 :minute 0 :second 0)
           (ical:date-time-variant target :year 2033 :month 1 :day 1
                                   :hour 0 :minute 0 :second 0))))
      (should
       (equal expected-int
              (icr:find-monthly-interval target dtstart intsize)))))

(ert-deftest ict:recur-find-yearly-interval ()
  "Does `icr:find-yearly-interval' find correct date intervals?"
  ;; Year numbers are monotonically increasing in the following test cases,
  ;; to make it easy to tell which of them fails.

  ;; One test with dates, to make sure that works:
  (let* ((dtstart '(1 8 2025))
         (target '(10 9 2025))
         (intsize 2)
         (expected-int
          (list
           (ical:make-date-time :year 2025 :month 1 :day 1
                                :hour 0 :minute 0 :second 0)
           (ical:make-date-time :year 2026 :month 1 :day 1
                                :hour 0 :minute 0 :second 0)
           (ical:make-date-time :year 2027 :month 1 :day 1
                                :hour 0 :minute 0 :second 0))))
    (should (equal expected-int
                   (icr:find-yearly-interval target dtstart intsize))))

  ;; A target not on an interval boundary:
  (let* ((dtstart (ical:make-date-time :year 2026 :month 3 :day 1
                                       :hour 1 :minute 2 :second 3))
         (target (ical:make-date-time :year 2026 :month 7 :day 28
                                      :hour 11 :minute 58 :second 0))
         (intsize 3)
         (expected-int
          (list
           (ical:make-date-time :year 2026 :month 1 :day 1
                                :hour 0 :minute 0 :second 0)
           (ical:make-date-time :year 2027 :month 1 :day 1
                                :hour 0 :minute 0 :second 0)
           (ical:make-date-time :year 2029 :month 1 :day 1
                                :hour 0 :minute 0 :second 0))))
    (should (equal expected-int
                   (icr:find-yearly-interval target dtstart intsize))))

  ;; A target on an interval boundary:
  (let* ((dtstart (ical:make-date-time :year 2027 :month 3 :day 1
                                       :hour 1 :minute 2 :second 3))
         (target (ical:make-date-time :year 2028 :month 1 :day 1
                                      :hour 0 :minute 0 :second 0))
         (intsize 4)
         (expected-int
          (list
           (ical:make-date-time :year 2027 :month 1 :day 1
                                :hour 0 :minute 0 :second 0)
           (ical:make-date-time :year 2028 :month 1 :day 1
                                :hour 0 :minute 0 :second 0)
           (ical:make-date-time :year 2031 :month 1 :day 1
                                :hour 0 :minute 0 :second 0))))
    (should (equal expected-int
                   (icr:find-yearly-interval target dtstart intsize))))

  ;; A target earlier than dtstart but in the same year;
  ;; it's important that this works when looking up recurrences of
  ;; time zone observance onsets
  (let* ((dtstart (ical:make-date-time :year 2029 :month 5 :day 28
                                       :hour 1 :minute 2 :second 3))
         (target (ical:make-date-time :year 2029 :month 2 :day 14
                                      :hour 11 :minute 58 :second 0))
         (intsize 1)
         (expected-int
          (list
           (ical:make-date-time :year 2029 :month 1 :day 1
                                :hour 0 :minute 0 :second 0)
           (ical:make-date-time :year 2030 :month 1 :day 1
                                :hour 0 :minute 0 :second 0)
           (ical:make-date-time :year 2030 :month 1 :day 1
                                :hour 0 :minute 0 :second 0))))
    (should (equal expected-int
                   (icr:find-yearly-interval target dtstart intsize)))))

;; Subintervals:

(ert-deftest ict:recur-refine-byyearday ()
  "Does `icr:refine-byyearday' correctly refine by yeardays?"
  (let* ((low (ical:make-date-time :year 2025 :month 1 :day 1
                                   :hour 0 :minute 0 :second 0))
         (high (ical:date/time-add low :year 1))
         (interval (list low high high))
         (yeardays (list 2 -7))
         (sub1 (list (ical:date-time-variant low :day 2)
                     (ical:date-time-variant low :day 3)))
         (sub2 (list (ical:date-time-variant low :month 12 :day 25)
                     (ical:date-time-variant low :month 12 :day 26)))
         (expected-subintervals (list sub1 sub2)))
    (should (equal expected-subintervals
                   (icr:refine-byyearday interval yeardays)))))

(ert-deftest ict:recur-refine-bymonth ()
  "Does `icr:refine-bymonth' correctly refine by months?"
  (let* ((low (ical:make-date-time :year 2025 :month 1 :day 1
                                   :hour 0 :minute 0 :second 0))
         (high (ical:date/time-add low :year 1))
         (interval (list low high high))
         (months (list 9 2))
         (sub1 (list (ical:date-time-variant low :month 2 :day 1)
                     (ical:date-time-variant low :month 3 :day 1)))
         (sub2 (list (ical:date-time-variant low :month 9 :day 1)
                     (ical:date-time-variant low :month 10 :day 1)))
         (expected-subintervals (list sub1 sub2)))
    (should (equal expected-subintervals
                   (icr:refine-bymonth interval months)))))

(ert-deftest ict:recur-refine-bymonthday ()
  "Does `icr:refine-bymonthday' correctly refine by days of the month?"
  (let* ((low (ical:make-date-time :year 2025 :month 2 :day 1
                                   :hour 0 :minute 0 :second 0))
         (high (ical:date/time-add low :month 1))
         (interval (list low high high))
         (monthdays (list -1 2 29))
         ;; N.B. we should get no subinterval for Feb. 29, 2025
         (sub1 (list (ical:date-time-variant low :day 2)
                     (ical:date-time-variant low :day 3)))
         (sub2 (list (ical:date-time-variant low :day 28)
                     (ical:date-time-variant low :month 3 :day 1)))
         (expected-subintervals (list sub1 sub2)))
    (should (equal expected-subintervals
                   (icr:refine-bymonthday interval monthdays)))))

(ert-deftest ict:recur-refine-byday ()
  "Does `icr:refine-byday' correctly refine by days of the week?"
  ;; The simple case: just day names
  (let* ((low (ical:make-date-time :year 2025 :month 3 :day 3 ; a Monday
                                   :hour 0 :minute 0 :second 0))
         (high (ical:date/time-add low :day 7))
         (interval (list low high high))
         (days (list 0 6)) ; just the weekend, please!
         (sub1 (list (ical:date-time-variant low :day 8)
                     (ical:date-time-variant low :day 9)))
         (sub2 (list (ical:date-time-variant low :day 9)
                     (ical:date-time-variant low :day 10)))
         (expected-subintervals (list sub1 sub2)))
    (should (equal expected-subintervals
                   (icr:refine-byday interval days))))

  ;; Day names with offsets within the month
  (let* ((low (ical:make-date-time :year 2025 :month 3 :day 1 ; a Saturday
                                   :hour 0 :minute 0 :second 0))
         (high (ical:date/time-add low :month 1))
         (interval (list low high high))
         (days (list '(1 . 2) '(1 . -1)))  ; second and last Monday
         (sub1 (list (ical:date-time-variant low :day 10)
                     (ical:date-time-variant low :day 11)))
         (sub2 (list (ical:date-time-variant low :day 31)
                     (ical:date-time-variant low :month 4 :day 1)))
         (expected-subintervals (list sub1 sub2)))
    (should (equal expected-subintervals
                   (icr:refine-byday interval days t))))

  ;; Day names with offsets within the year
  (let* ((low (ical:make-date-time :year 2025 :month 1 :day 1
                                   :hour 0 :minute 0 :second 0))
         (high (ical:date/time-add low :year 1))
         (interval (list low high high))
         (days (list '(5 . 1) '(5 . -1)))  ; first and last Friday
         (sub1 (list (ical:date-time-variant low :day 3)
                     (ical:date-time-variant low :day 4)))
         (sub2 (list (ical:date-time-variant low :month 12 :day 26)
                     (ical:date-time-variant low :month 12 :day 27)))
         (expected-subintervals (list sub1 sub2)))
    (should (equal expected-subintervals
                   (icr:refine-byday interval days nil)))))

(ert-deftest ict:recur-refine-byhour ()
  "Does `icr:refine-byhour' correctly refine by hours?"
  ;; No time zone, just clock times:
  (let* ((low (ical:make-date-time :year 2025 :month 1 :day 1
                                   :hour 0 :minute 0 :second 0))
         (high (ical:date/time-add low :day 1))
         (interval (list low high high))
         (hours (list 2 19))
         (sub1 (list (ical:date-time-variant low :hour 2)
                     (ical:date-time-variant low :hour 3)))
         (sub2 (list (ical:date-time-variant low :hour 19)
                     (ical:date-time-variant low :hour 20)))
         (expected-subintervals (list sub1 sub2)))
    (should (equal expected-subintervals
                   (icr:refine-byhour interval hours))))

  ;; With time zone, but without crossing an observance boundary:
  (let* ((low (ical:make-date-time :year 2025 :month 2 :day 1
                                   :hour 0 :minute 0 :second 0
                                   :zone ict:est :dst nil))
         (high (ical:date/time-add low :day 1 ict:tz-eastern))
         (interval (list low high high))
         (hours (list 2 19))
         (sub1 (list (ical:date-time-variant low :hour 2 :tz 'preserve)
                     (ical:date-time-variant low :hour 3 :tz 'preserve)))
         (sub2 (list (ical:date-time-variant low :hour 19 :tz 'preserve)
                     (ical:date-time-variant low :hour 20 :tz 'preserve)))
         (expected-subintervals (list sub1 sub2)))
    (should (equal expected-subintervals
                   (icr:refine-byhour interval hours ict:tz-eastern)))))

(ert-deftest ict:recur-refine-byminute ()
  "Does `icr:refine-byminute' correctly refine by minutes?"
  ;; No time zone, just clock times:
  (let* ((low (ical:make-date-time :year 2025 :month 5 :day 1
                                   :hour 13 :minute 0 :second 0))
         (high (ical:date/time-add low :hour 1))
         (interval (list low high high))
         (minutes (list 7 59))
         (sub1 (list (ical:date-time-variant low :minute 7)
                     (ical:date-time-variant low :minute 8)))
         (sub2 (list (ical:date-time-variant low :minute 59)
                     (ical:date-time-variant low :hour 14 :minute 0)))
         (expected-subintervals (list sub1 sub2)))
    (should (equal expected-subintervals
                   (icr:refine-byminute interval minutes))))

  ;; With time zone, but without crossing an observance boundary:
  (let* ((low (ical:make-date-time :year 2025 :month 2 :day 1
                                   :hour 13 :minute 0 :second 0
                                   :zone ict:est :dst nil))
         (high (ical:date/time-add low :hour 1 ict:tz-eastern))
         (interval (list low high high))
         (minutes (list 7 59))
         (sub1 (list (ical:date-time-variant low :minute 7 :tz 'preserve)
                     (ical:date-time-variant low :minute 8 :tz 'preserve)))
         (sub2 (list (ical:date-time-variant low :minute 59 :tz 'preserve)
                     (ical:date-time-variant low :hour 14 :minute 0
                                             :tz 'preserve)))
         (expected-subintervals (list sub1 sub2)))
    (should (equal expected-subintervals
                   (icr:refine-byminute interval minutes ict:tz-eastern)))))

(ert-deftest ict:recur-refine-bysecond ()
  "Does `icr:refine-bysecond' correctly refine by seconds?"
  ;; No time zone, just clock times:
  (let* ((low (ical:make-date-time :year 2025 :month 5 :day 1
                                   :hour 13 :minute 59 :second 0))
         (high (ical:date/time-add low :minute 1))
         (interval (list low high high))
         (seconds (list 24 59))
         (sub1 (list (ical:date-time-variant low :second 24)
                     (ical:date-time-variant low :second 25)))
         (sub2 (list (ical:date-time-variant low :second 59)
                     (ical:date-time-variant low :hour 14 :minute 0 :second 0)))
         (expected-subintervals (list sub1 sub2)))
    (should (equal expected-subintervals
                   (icr:refine-bysecond interval seconds))))

  ;; With time zone, but without crossing an observance boundary:
  (let* ((low (ical:make-date-time :year 2025 :month 2 :day 1
                                   :hour 13 :minute 19 :second 0
                                   :zone ict:est :dst nil))
         (high (ical:date/time-add low :minute 1 ict:tz-eastern))
         (interval (list low high high))
         (seconds (list 24 59))
         (sub1 (list (ical:date-time-variant low :second 24 :tz 'preserve)
                     (ical:date-time-variant low :second 25 :tz 'preserve)))
         (sub2 (list (ical:date-time-variant low :second 59 :tz 'preserve)
                     (ical:date-time-variant low :minute 20 :second 0
                                             :tz 'preserve)))
         (expected-subintervals (list sub1 sub2)))
    (should (equal expected-subintervals
                   (icr:refine-bysecond interval seconds ict:tz-eastern)))))

(ert-deftest ict:recur-subintervals-to-dates ()
  "Does `icr:subintervals-to-dates' correctly generate recurrences?"
  ;; Two subintervals, the first three days long, the second less than a single day
  (let* ((low1 (ical:make-date-time :year 2025 :month 5 :day 1
                                    :hour 13 :minute 59 :second 0))
         (high1 (ical:date/time-add low1 :day 3))
         (sub1 (list low1 high1))
         (low2 (ical:make-date-time :year 2025 :month 5 :day 31
                                    :hour 14 :minute 0 :second 0))
         (high2 (ical:date/time-add low2 :hour 3)) ; later but on the same day
         (sub2 (list low2 high2))
         (low-date1 (ical:date-time-to-date low1))
         (low-date2 (ical:date-time-to-date low2))
         (expected-recs (list low-date1
                              (ical:date/time-add low-date1 :day 1)
                              (ical:date/time-add low-date1 :day 2)
                              (ical:date/time-add low-date1 :day 3)
                              low-date2)))
    (should (equal expected-recs
                   (icr:subintervals-to-dates (list sub1 sub2))))))

(ert-deftest ict:recur-subintervals-to-date-times ()
  "Does `icr:subintervals-to-date-times' correctly generate recurrences?"
  ;; Two subintervals, each one second long, no time zone
  (let* ((low1 (ical:make-date-time :year 2025 :month 5 :day 1
                                    :hour 13 :minute 59 :second 0))
         (high1 (ical:date/time-add low1 :second 1))
         (sub1 (list low1 high1))
         (low2 (ical:make-date-time :year 2025 :month 5 :day 2
                                    :hour 14 :minute 0 :second 0))
         (high2 (ical:date/time-add low2 :second 1))
         (sub2 (list low2 high2))
         (expected-recs (list low1 low2)))
    (should (equal expected-recs
                   (icr:subintervals-to-date-times (list sub1 sub2)))))

  ;; A subinterval five seconds long, with time zone
  (let* ((low1 (ical:make-date-time :year 2025 :month 6 :day 1
                                    :hour 13 :minute 59 :second 0
                                    :zone ict:edt :dst t))
         (high1 (ical:date/time-add low1 :second 5 ict:tz-eastern))
         (sub1 (list low1 high1))
         (expected-recs
          (list low1
                (ical:date/time-add low1 :second 1 ict:tz-eastern)
                (ical:date/time-add low1 :second 2 ict:tz-eastern)
                (ical:date/time-add low1 :second 3 ict:tz-eastern)
                (ical:date/time-add low1 :second 4 ict:tz-eastern))))
    (should (equal expected-recs
                   (icr:subintervals-to-date-times (list sub1) ict:tz-eastern))))

  ;; A subinterval five seconds long, with time zone, which crosses an
  ;; observance boundary where the final three seconds occur after
  ;; clocks are set forward an hour; these seconds should therefore be in EDT:
  (let* ((low1 (ical:make-date-time :year 2025 :month 3 :day 9
                                    :hour 1 :minute 59 :second 58
                                    :zone ict:est :dst nil))
         (high1 (ical:make-date-time :year 2025 :month 3 :day 9
                                     :hour 3 :minute 0 :second 3
                                     :zone ict:edt :dst t))
         (sub1 (list low1 high1))
         (expected-recs
          (list low1
                (ical:date-time-variant low1 :second 59 :tz 'preserve)
                (ical:date-time-variant high1 :second 0 :tz 'preserve)
                (ical:date-time-variant high1 :second 1 :tz 'preserve)
                (ical:date-time-variant high1 :second 2 :tz 'preserve))))
    (should (equal expected-recs
                   (icr:subintervals-to-date-times (list sub1) ict:tz-eastern))))

  ;; A subinterval five seconds long, with time zone, which crosses an
  ;; observance boundary where the final three seconds occur after
  ;; clocks are set back an hour; these seconds should therefore be in
  ;; EST:
  (let* ((low1 (ical:make-date-time :year 2024 :month 11 :day 3
                                    :hour 1 :minute 59 :second 58
                                    :zone ict:edt :dst t))
         (high1 (ical:make-date-time :year 2024 :month 11 :day 3
                                     :hour 1 :minute 0 :second 2
                                     :zone ict:est :dst nil))
         (sub1 (list low1 high1))
         (expected-recs
          (list low1
                (ical:date-time-variant low1 :second 59 :tz 'preserve)
                (ical:date-time-variant high1 :second 0 :tz 'preserve)
                (ical:date-time-variant high1 :second 1 :tz 'preserve))))
    (should (equal expected-recs
                   (icr:subintervals-to-date-times (list sub1) ict:tz-eastern)))))

;; Tests for time zone functions:

(ert-deftest ict:recur-tz-observance-on/nonexistent ()
  "Does `icr:tz-observance-on' correctly interpret nonexistent times?"
  (let* ((onset-start (ical:make-date-time :year 2030 :month 3 :day 10
                                           :hour 2 :minute 0 :second 0
                                           :zone ict:est :dst nil))
         (start-shifted (ical:date-time-variant onset-start :hour 3
                                                :zone ict:edt :dst t))
         ;; 2:30AM falls into the gap when the clock jumps from 2AM to 3AM:
         (nonexistent (ical:date-time-variant onset-start :minute 30
                                              :zone ict:est :dst nil))
         (nonexistent-shifted (ical:date-time-variant nonexistent :hour 3
                                                      :zone ict:edt :dst t)))
    (icr:tz-observance-on onset-start ict:tz-eastern t) ;; updates the time to EDT
    (icr:tz-observance-on nonexistent ict:tz-eastern t) ;; updates the time to EDT
    (should (equal onset-start start-shifted))
    (should (equal nonexistent nonexistent-shifted))))

(ert-deftest ict:recur-tz-observance-on/occurs-twice ()
  "Does `icr:tz-observance-on' correctly interpret times that occur twice?"
  (let* ((onset-start (ical:make-date-time :year 2025 :month 11 :day 2
                                           :hour 2 :minute 0 :second 0
                                           :zone ict:edt :dst t))
         ;; 1:30AM occurs twice when the clock is set back from 2AM to 1AM:
         (no-zone (ical:date-time-variant onset-start :hour 1 :minute 30))
         (first (ical:date-time-variant onset-start :hour 1 :minute 30
                                        :zone ict:edt :dst t))
         (second (ical:date-time-variant first :zone ict:est :dst nil))
         (first+1h (ical:date/time-add first :hour 1 ict:tz-eastern)))
    (icr:tz-observance-on no-zone ict:tz-eastern t) ;; sets zone
    (should (equal first no-zone))
    (should (equal second first+1h))))

(ert-deftest ict:recur-tz-observance-on ()
  "Does `icr:tz-observance-on' correctly find observances?"

  ;; A date before the start of all observances in the timezone.
  ;; In this case, there is no matching observance, so we should get nil.
  (let* ((dt (ical:make-date-time :year 1900 :month 1 :day 1
                                  :hour 12 :minute 0 :second 0
                                  :zone ict:est :dst nil))
         (ts (encode-time dt)))
    (should (null (icr:tz-observance-on dt ict:tz-eastern)))
    (should (null (icr:tz-observance-on ts ict:tz-eastern))))

  ;; A date matching the start of one of the STANDARD observances:
  (let* ((dt (ical:make-date-time :year 1967 :month 10 :day 29
                                  :hour 2 :minute 0 :second 0
                                  :zone ict:edt :dst t))
         (obs/onset (icr:tz-observance-on dt ict:tz-eastern))
         (obs (car obs/onset))
         (onset (cadr obs/onset))
         ;; make sure we get the same result with an absolute time:
         (ts (encode-time dt))
         (ts-obs/onset (icr:tz-observance-on ts ict:tz-eastern)))
    (should (eq 'ical:standard (ical:ast-node-type obs)))
    (should (equal dt onset))
    (should (equal obs/onset ts-obs/onset)))

  ;; A date matching the start of a DAYLIGHT observance:
  (let* ((dt (ical:make-date-time :year 1967 :month 4 :day 30
                                  :hour 2 :minute 0 :second 0
                                  :zone ict:est :dst nil))
         (obs/onset (icr:tz-observance-on dt ict:tz-eastern))
         (obs (car obs/onset))
         (onset (cadr obs/onset))
         ;; make sure we get the same result with an absolute time:
         (ts (encode-time dt))
         (ts-obs/onset (icr:tz-observance-on ts ict:tz-eastern)))
    (should (eq 'ical:daylight (ical:ast-node-type obs)))
    (should (equal dt onset))
    (should (equal obs/onset ts-obs/onset)))

  ;; A date matching an RDATE of a DAYLIGHT observance:
  (let* ((dt (ical:make-date-time :year 1975 :month 2 :day 23
                                  :hour 2 :minute 0 :second 0
                                  :zone ict:est :dst nil))
         (obs/onset (icr:tz-observance-on dt ict:tz-eastern))
         (obs (car obs/onset))
         (onset (cadr obs/onset))
         ;; make sure we get the same result with an absolute time:
         (ts (encode-time dt))
         (ts-obs/onset (icr:tz-observance-on ts ict:tz-eastern)))
    (should (eq 'ical:daylight (ical:ast-node-type obs)))
    (should (equal dt onset))
    (should (equal obs/onset ts-obs/onset)))

  ;; A date matching the end of a STANDARD observance:
  (let* ((ut (ical:make-date-time :year 2006 :month 10 :day 29
                                  :hour 6 :minute 0 :second 0
                                  :zone 0 :dst nil)) ; UNTIL is in UTC
         (dt (ical:make-date-time :year 2006 :month 10 :day 29
                                  :hour 2 :minute 0 :second 0
                                  :zone ict:edt :dst t))
         (obs/onset (icr:tz-observance-on dt ict:tz-eastern))
         (obs (car obs/onset))
         (onset (cadr obs/onset))
         ;; make sure we get the same result with an absolute time:
         (ts (encode-time dt))
         (ts-obs/onset (icr:tz-observance-on ts ict:tz-eastern)))
    (should (ical:date-time-simultaneous-p ut dt))
    (should (eq 'ical:standard (ical:ast-node-type obs)))
    (should (equal dt onset))
    (should (equal obs/onset ts-obs/onset)))

  ;; A date matching the end of a DAYLIGHT observance:
  (let* ((ut (ical:make-date-time :year 2006 :month 4 :day 2
                                  :hour 7 :minute 0 :second 0
                                  :zone 0 :dst nil)) ; UNTIL is in UTC
         (dt (ical:make-date-time :year 2006 :month 4 :day 2
                                  :hour 2 :minute 0 :second 0
                                  :zone ict:est :dst nil))
         (obs/onset (icr:tz-observance-on dt ict:tz-eastern))
         (obs (car obs/onset))
         (onset (cadr obs/onset))
         ;; make sure we get the same result with an absolute time:
         (ts (encode-time dt))
         (ts-obs/onset (icr:tz-observance-on ts ict:tz-eastern)))
    (should (ical:date-time-simultaneous-p ut dt))
    (should (eq 'ical:daylight (ical:ast-node-type obs)))
    (should (equal dt onset))
    (should (equal obs/onset ts-obs/onset)))

  ;; A date matching an onset in the middle of a DAYLIGHT observance
  ;; which has ended:
  (let* ((dt (ical:make-date-time :year 1980 :month 4 :day 27
                                  :hour 2 :minute 0 :second 0
                                  :zone ict:est :dst nil))
         (end (ical:make-date-time :year 1986 :month 4 :day 27
                                   :hour 7 :minute 0 :second 0
                                   :zone 0)) ; UNTIL is in UTC
         (obs/onset (icr:tz-observance-on dt ict:tz-eastern))
         (obs (car obs/onset))
         (onset (cadr obs/onset))
         ;; make sure we get the same result with an absolute time:
         (ts (encode-time dt))
         (ts-obs/onset (icr:tz-observance-on ts ict:tz-eastern)))
    (should (eq 'ical:daylight (ical:ast-node-type obs)))
    (should (equal dt onset))
    (should (equal end (ical:recur-until
                        (ical:with-property-of obs 'ical:rrule nil value))))
    (should (equal obs/onset ts-obs/onset)))

  ;; A date matching an onset of the DAYLIGHT observance which is
  ;; ongoing:
  (let* ((dt (ical:make-date-time :year 2025 :month 3 :day 9
                                  :hour 2 :minute 0 :second 0
                                  :zone ict:est :dst nil))
         (obs/onset (icr:tz-observance-on dt ict:tz-eastern))
         (obs (car obs/onset))
         (onset (cadr obs/onset))
         ;; make sure we get the same result with an absolute time:
         (ts (encode-time dt))
         (ts-obs/onset (icr:tz-observance-on ts ict:tz-eastern)))
    (should (eq 'ical:daylight (ical:ast-node-type obs)))
    (should (equal dt onset))
    (should (equal obs/onset ts-obs/onset)))

  ;; A date in the middle of the DAYLIGHT observance which is ongoing:
  (let* ((start (ical:make-date-time :year 2025 :month 3 :day 9
                                     :hour 2 :minute 0 :second 0
                                     :zone ict:est :dst nil))
         (dt (ical:make-date-time :year 2025 :month 5 :day 28
                                  :hour 2 :minute 0 :second 0
                                  :zone ict:edt :dst t))
         (obs/onset (icr:tz-observance-on dt ict:tz-eastern))
         (obs (car obs/onset))
         (onset (cadr obs/onset))
         ;; make sure we get the same result with an absolute time:
         (ts (encode-time dt))
         (ts-obs/onset (icr:tz-observance-on ts ict:tz-eastern)))
    (should (eq 'ical:daylight (ical:ast-node-type obs)))
    (should (equal start onset))
    (should (equal obs/onset ts-obs/onset)))

  ;; A date in the middle of the STANDARD observance which is ongoing:
  (let* ((start (ical:make-date-time :year 2025 :month 11 :day 2
                                     :hour 2 :minute 0 :second 0
                                     :zone ict:edt :dst t))
         (dt (ical:make-date-time :year 2026 :month 1 :day 28
                                  :hour 12 :minute 30 :second 0
                                  :zone ict:est :dst nil))
         (obs/onset (icr:tz-observance-on dt ict:tz-eastern))
         (obs (car obs/onset))
         (onset (cadr obs/onset))
         ;; make sure we get the same result with an absolute time:
         (ts (encode-time dt))
         (ts-obs/onset (icr:tz-observance-on ts ict:tz-eastern)))
    (should (eq 'ical:standard (ical:ast-node-type obs)))
    (should (equal start onset))
    (should (equal obs/onset ts-obs/onset)))

  ;; The following two tests were useful in detecting a broken optimization:
  (let* ((start (ical:make-date-time :year 2006 :month 10 :day 29
                                     :hour 2 :minute 0 :second 0
                                     :zone ict:edt :dst t))
         (dt (ical:make-date-time :year 2006 :month 11 :day 1
                                  :hour 12 :minute 30 :second 0
                                  :zone ict:est :dst nil))
         (obs/onset (icr:tz-observance-on dt ict:tz-eastern))
         (obs (car obs/onset))
         (onset (cadr obs/onset))
         ;; make sure we get the same result with an absolute time:
         (ts (encode-time dt))
         (ts-obs/onset (icr:tz-observance-on ts ict:tz-eastern)))
    (should (eq 'ical:standard (ical:ast-node-type obs)))
    (should (equal start onset))
    (should (equal obs/onset ts-obs/onset)))

  (let* ((start (ical:make-date-time :year 2007 :month 11 :day 4
                                     :hour 2 :minute 0 :second 0
                                     :zone ict:edt :dst t))
         (dt (ical:make-date-time :year 2008 :month 2 :day 1
                                  :hour 12 :minute 30 :second 0
                                  :zone ict:est :dst nil))
         (obs/onset (icr:tz-observance-on dt ict:tz-eastern))
         (obs (car obs/onset))
         (onset (cadr obs/onset))
         ;; make sure we get the same result with an absolute time:
         (ts (encode-time dt))
         (ts-obs/onset (icr:tz-observance-on ts ict:tz-eastern)))
    (should (eq 'ical:standard (ical:ast-node-type obs)))
    (should (equal start onset))
    (should (equal obs/onset ts-obs/onset)))


  ;; A date in the middle of the STANDARD observance which is ongoing;
  ;; test that the update flag correctly sets the zone information:
  (let* ((start (ical:make-date-time :year 2025 :month 11 :day 2
                                     :hour 2 :minute 0 :second 0
                                     :zone ict:edt :dst t))
         (dt (ical:make-date-time :year 2026 :month 1 :day 28
                                  :hour 12 :minute 30 :second 0
                                  ;; no zone information
                                  ))
         (obs/onset (icr:tz-observance-on dt ict:tz-eastern t))
         (obs (car obs/onset))
         (onset (cadr obs/onset)))
    (should (eq 'ical:standard (ical:ast-node-type obs)))
    (should (equal start onset))))


;; Tests for recurrence rule interpretation:
(cl-defmacro ict:rrule-test (recur-string doc
                             &key dtstart
                             (low dtstart)
                             high
                             tz
                             rdates
                             exdates
                             members
                             nonmembers
                             size
                             (tags nil)
                             source)

  "Create a test which parses RECUR-STRING to an `icalendar-recur',
creates an event with a recurrence set from this value, and checks
various properties of the recurrence set.

DTSTART should be an `icalendar-date' or `icalendar-date-time'
  value appropriate to the RECUR-STRING.  The value will be
  bound to the symbol `dtstart'; this symbol can thus be used inside
  the expressions for MEMBERS and NONMEMBERS.
LOW and HIGH should be the bounds of the window in which to compute
  recurrences.  LOW defaults to DTSTART.
TZ, if present, should be an `icalendar-vtimezone'.
  Date-times in the recurrence set will be calculated relative to this
  time zone.
RDATES, if present, should be a list of additional
  `icalendar-date' or `icalendar-date-time' values to be added to
  the recurrence set *in addition to* those generated by the
  recurrence rule (see `icalendar-rdate').
EXDATES, if present, should be a list of `icalendar-date' or
  `icalendar-date-time' values to be excluded from the recurrence
  set, *even if* they are in RDATES or generated by the
  recurrence rule (see `icalendar-exdate').
MEMBERS, if present, should be a list of values that are expected
  to be present in the recurrence set.
NONMEMBERS, if present, should be a list of values that are expected
  to be excluded from the recurrence set.
SIZE, if present, should be a positive integer representing the
  expected size of the recurrence set.  Defaults to the value of the
  COUNT clause in the recurrence rule, if any.
TAGS is passed on to `ert-deftest'.
SOURCE should be a symbol; it is used to name the test."
  `(ert-deftest ,(intern (concat "ict:rrule-test-" (symbol-name source))) ()
     ,(format "Parse and evaluate recur-value example from `%s':\n%s"
              source doc)
     :tags ,tags
     (let* ((parsed (ical:parse-from-string 'ical:recur ,recur-string))
            (recvalue (ical:ast-node-value parsed))
            (until (ical:recur-until recvalue))
            (count (ical:recur-count recvalue))
            (dtstart ,dtstart)
            (tzid
             (when (cl-typep dtstart 'ical:date-time)
               "America/New_York"))
            (recset-size (or ,size count))
            (vevent
             (ical:make-vevent
              (ical:uid (concat "uid-test-" ,(symbol-name source)))
              (ical:dtstart dtstart (ical:tzidparam tzid))
              (ical:rrule parsed)
              (ical:rdate ,rdates)
              (ical:exdate ,exdates)))
            ;; default for HIGH: UNTIL or DTSTART+3*INTERVAL
            (win-high
             (or ,high
                 until
                 (cadr
                  (icr:nth-interval 2 ,dtstart recvalue))))
            (recs
             (if count
                 (icr:recurrences-to-count vevent ,tz)
               (icr:recurrences-in-window ,low win-high vevent ,tz))))
       (should (ical:ast-node-valid-p parsed))
       (when ,members
         (dolist (dt ,members)
           (should (member dt recs))))
       (when ,nonmembers
         (dolist (dt ,nonmembers)
           (should-not (member dt recs))))
       (when recset-size
         (should (length= recs recset-size))))))

(ict:rrule-test
 "FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-1"
 "Last non-weekend day of the month"
 :dtstart '(3 31 2025)
 :high '(6 1 2025)
 :members '((3 31 2025) (4 30 2025) (5 30 2025))
 :nonmembers '((5 31 2025)) ;; 5/31/2025 is a Saturday
 :source rfc5545-sec3.3.10/1)

(ict:rrule-test
 "FREQ=YEARLY;INTERVAL=2;BYMONTH=1;BYDAY=SU;BYHOUR=8,9;BYMINUTE=30"
 "Every Sunday in January at 8:30AM and 9:30AM, every other year"
 :dtstart (ical:read-date-time "20250105T083000")
 :high (ical:read-date-time "20271231T000000")
 :members
 (let ((jan3-27 (ical:make-date-time :year 2027 :month 1 :day 3
                                     :hour 8 :minute 30 :second 0)))
   (list dtstart
         ;; 2025: Jan 5, 12, 19, 26
         (ical:date-time-variant dtstart :hour 9)
         (ical:date-time-variant dtstart :day 12)
         (ical:date-time-variant dtstart :day 12 :hour 9)
         (ical:date-time-variant dtstart :day 19)
         (ical:date-time-variant dtstart :day 19 :hour 9)
         (ical:date-time-variant dtstart :day 19)
         (ical:date-time-variant dtstart :day 19 :hour 9)
         (ical:date-time-variant dtstart :day 26)
         (ical:date-time-variant dtstart :day 26 :hour 9)
         ;; 2027: Jan 3, 10, 17, 24, 31
         (ical:date-time-variant jan3-27 :hour 9)
         (ical:date-time-variant jan3-27 :day 10)
         (ical:date-time-variant jan3-27 :day 10 :hour 9)
         (ical:date-time-variant jan3-27 :day 17)
         (ical:date-time-variant jan3-27 :day 17 :hour 9)
         (ical:date-time-variant jan3-27 :day 24)
         (ical:date-time-variant jan3-27 :day 24 :hour 9)
         (ical:date-time-variant jan3-27 :day 31)
         (ical:date-time-variant jan3-27 :day 31 :hour 9)))
 :nonmembers
 (list
  (ical:make-date-time :year 2026 :month 1 :day 4
                       :hour 8 :minute 30 :second 0)
  (ical:make-date-time :year 2026 :month 1 :day 4
                       :hour 9 :minute 30 :second 0))
 :source rfc5545-sec3.3.10/2)

(ict:rrule-test
 "FREQ=YEARLY;BYMONTH=2;BYMONTHDAY=-1"
 "Every year on the last day in February"
 :dtstart '(2 29 2024)
 :high '(3 1 2028)
 :members '((2 28 2025) (2 28 2026) (2 28 2027) (2 29 2028))
 :nonmembers '((2 28 2028))
 :source leap-day/1)

(ict:rrule-test
 "FREQ=YEARLY;INTERVAL=4;BYMONTH=2;BYMONTHDAY=29"
 "Every four years on February 29"
 :dtstart '(2 29 2024)
 :high '(3 1 2028)
 :members '((2 29 2028))
 :nonmembers '((2 28 2028))
 :source leap-day/2)

(ict:rrule-test
"FREQ=DAILY;COUNT=10"
"Daily for 10 occurrences"
:dtstart (ical:make-date-time :year 1997 :month 9 :day 2
                              :hour 9 :minute 0 :second 0)
:members
;; (1997 9:00 AM EDT) September 2-11
(mapcar
 (lambda (day) (ical:date-time-variant dtstart :day day))
 (number-sequence 2 11))
:source rfc5545-sec3.3.10/3)

(ict:rrule-test
 "RRULE:FREQ=YEARLY"
 "Every year on a specific date, e.g. an anniversary"
 :dtstart '(11 11 2024)
 :high '(10 1 2030)
 :members '((11 11 2024)
            (11 11 2025)
            (11 11 2026)
            (11 11 2027)
            (11 11 2028)
            (11 11 2029))
 :nonmembers '((11 11 2030))
 :source rfc5545-sec3.6.1/3)

;; Time zone tests

(ict:rrule-test
 "RRULE:FREQ=YEARLY;BYMONTH=4;BYDAY=-1SU;UNTIL=19730429T070000Z"
 "Every year on the last Sunday of April (through 1973-04-29) at 2AM.
(Onset of US Eastern Daylight Time.)"
 :tz ict:tz-eastern
 ;; DTSTART and all the times below are at *3*AM EDT, because 2AM EST
 ;; (the onset of the observance) does not exist as a local time:
 :dtstart (ical:make-date-time :year 1967 :month 4 :day 30
                               :hour 3 :minute 0 :second 0
                               :zone ict:edt :dst t)
 :high (ical:date-time-variant dtstart :year 1973 :month 4 :day 30
                               :zone ict:edt :dst t)
 :members
 (list
  (ical:date-time-variant dtstart :year 1968 :day 28 :tz 'preserve)
  (ical:date-time-variant dtstart :year 1969 :day 27 :tz 'preserve)
  (ical:date-time-variant dtstart :year 1970 :day 26 :tz 'preserve)
  (ical:date-time-variant dtstart :year 1971 :day 25 :tz 'preserve)
  (ical:date-time-variant dtstart :year 1972 :day 30 :tz 'preserve)
  (ical:date-time-variant dtstart :year 1973 :day 29 :tz 'preserve))
 :source rfc5545-sec3.6.5/1)

(ict:rrule-test
 "RRULE:FREQ=YEARLY;BYMONTH=11;BYDAY=1SU"
 "Every year on the first Sunday of November at 2AM.
(Onset of Eastern Standard Time)."
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 2007 :month 11 :day 4
                               :hour 2 :minute 0 :second 0
                               :zone ict:edt :dst t)
 :high (ical:date-time-variant dtstart :year 2010 :month 11 :day 8
                               :zone ict:est :dst nil)
 :members
 ;; all the times below are at *1*AM EST, because 2AM EDT (the onset of
 ;; the observance) is when clocks get set back:
 (list (ical:date-time-variant dtstart
                               :year 2008 :month 11 :day 2
                               :zone ict:est :dst nil)
       (ical:date-time-variant dtstart
                               :year 2009 :month 11 :day 1
                               :zone ict:est :dst nil)
       (ical:date-time-variant dtstart
                               :year 2010 :month 11 :day 7
                               :zone ict:est :dst nil))
 :source rfc5545-sec3.6.5/3.1)

(ict:rrule-test
 "RRULE:FREQ=MONTHLY;INTERVAL=3;BYDAY=1SU"
 "Every three months on the first Sunday of the month."
 :dtstart '(1 5 2025)
 :high '(1 1 2026)
 :members (list '(4 6 2025)
                '(7 6 2025)
                '(10 5 2025))
 :nonmembers (list '(1 12 2025) ;; second Sun.
                   '(2 2 2025) ;; first Sun. in Feb.
                   '(4 5 2025)) ;; Sat.
 :source monthly/interval)

(ict:rrule-test
 "RRULE:FREQ=DAILY;COUNT=10\n"
 "Daily for 10 occurrences"
 :dtstart (ical:read-date-time "19970902T090000")
 :members
 (mapcar
  (lambda (day) (ical:date-time-variant dtstart :day day))
  (number-sequence 2 11))
 :nonmembers (list (ical:date-time-variant dtstart :day 12))
 :high (ical:read-date-time "19970912T090000")
 :source rfc5545-sec3.8.5.3/1)

(ict:rrule-test
 "RRULE:FREQ=DAILY;UNTIL=19971224T000000Z\n"
 "Daily at 9AM until December 24, 1997"
 :tags '(:expensive-test)
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 9 :day 2
                               :hour 9 :minute 0 :second 0
                               :zone ict:edt :dst t)
 :members
 (append
   ;; EDT:
  (mapcar
   (lambda (day) (ical:date-time-variant dtstart :day day :tz 'preserve))
   (number-sequence 2 30)) ;; Sept. 2--30
  (mapcar
   (lambda (day) (ical:date-time-variant dtstart :month 10 :day day
                                         :tz 'preserve))
   (number-sequence 1 25)) ;; Oct. 1--25
  ;; EST:
  (mapcar
   (lambda (day)
     (ical:date-time-variant dtstart :month 10 :day day :zone ict:est :dst nil))
   (number-sequence 26 31))) ;; Oct. 26--31
 :source rfc5545-sec3.8.5.3/2)

(ict:rrule-test
 "RRULE:FREQ=DAILY;INTERVAL=2\n"
 "Every other day - forever"
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 9 :day 2
                               :hour 9 :minute 0 :second 0
                               :zone ict:edt :dst t)
 :high (ical:make-date-time :year 1997 :month 12 :day 4
                            :hour 0 :minute 0 :second 0
                            :zone ict:est :dst nil)
 :members
 (append
  ;; (1997 9:00 AM EDT) September 2,4,6,8...24,26,28,30;
  ;;                    October 2,4,6...20,22,24
  (mapcar
   (lambda (n)
     (ical:date-time-variant dtstart :day (* 2 n) :tz 'preserve))
   (number-sequence 1 15))
  (mapcar
   (lambda (n)
     (ical:date-time-variant dtstart :month 10 :day (* 2 n) :tz 'preserve))
   (number-sequence 1 12))
  ;; (1997 9:00 AM EST) October 26,28,30;
  ;;                    November 1,3,5,7...25,27,29;
  ;;                    December 1,3,...
  (mapcar
   (lambda (n)
     (ical:date-time-variant dtstart :month 10 :day (* 2 n)
                             :zone ict:est :dst nil))
   (number-sequence 13 15))
  (mapcar
   (lambda (n)
     (ical:date-time-variant dtstart :month 11 :day (1- (* 2 n))
                             :zone ict:est :dst nil))
   (number-sequence 1 15))
  (mapcar
   (lambda (n)
     (ical:date-time-variant dtstart :month 12 :day (1- (* 2 n))
                             :zone ict:est :dst nil))
   (number-sequence 1 2)))

 :nonmembers
 (list
  ;; e.g.
  (ical:make-date-time :year 1997 :month 10 :day 27
                       :hour 9 :minute 0 :second 0
                       :zone ict:est :dst nil))
 :source rfc5545-sec3.8.5.3/3)

(ict:rrule-test
 "RRULE:FREQ=DAILY;INTERVAL=10;COUNT=5\n"
 "Every ten days for five recurrences"
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 9 :day 2
                               :hour 9 :minute 0 :second 0
                               :zone ict:edt :dst t)

 :members ;; (1997 9:00 AM EDT) September 2,12,22; October 2,12
 (list
  dtstart
  (ical:make-date-time :year 1997 :month 9 :day 12
                       :hour 9 :minute 0 :second 0
                       :zone ict:edt :dst t)
  (ical:make-date-time :year 1997 :month 9 :day 22
                       :hour 9 :minute 0 :second 0
                       :zone ict:edt :dst t)
  (ical:make-date-time :year 1997 :month 10 :day 2
                       :hour 9 :minute 0 :second 0
                       :zone ict:edt :dst t)
  (ical:make-date-time :year 1997 :month 10 :day 12
                       :hour 9 :minute 0 :second 0
                       :zone ict:edt :dst t))
 :source rfc5545-sec3.8.5.3/4)

(ict:rrule-test
 "RRULE:FREQ=YEARLY;UNTIL=20000131T140000Z;BYMONTH=1;BYDAY=SU,MO,TU,WE,TH,FR,SA\n"
 "Every day in January, for three years (weekdays explicit)"
 :tags '(:expensive-test)
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1998 :month 1 :day 1
                               :hour 9 :minute 0 :second 0
                               :zone ict:est :dst nil)
 :high (ical:make-date-time :year 2000 :month 2 :day 1
                            :hour 9 :minute 0 :second 0
                            :zone ict:est :dst nil)
 :members
 ;; (1998 9:00 AM EST)January 1-31
 ;; (1999 9:00 AM EST)January 1-31
 ;; (2000 9:00 AM EST)January 1-31
 (append
  (mapcar
   (lambda (day) (ical:date-time-variant dtstart :day day :tz 'preserve))
   (number-sequence 1 31))
  (mapcar
   (lambda (day)
     (ical:date-time-variant dtstart :year 1999 :day day :tz 'preserve))
   (number-sequence 1 31))
  (mapcar
   (lambda (day)
     (ical:date-time-variant dtstart :year 2000 :day day :tz 'preserve))
   (number-sequence 1 31)))
 :source rfc5545-sec3.8.5.3/5)

(ict:rrule-test
 "RRULE:FREQ=DAILY;UNTIL=20000131T140000Z;BYMONTH=1\n"
 "Every day in January, for three years (weekdays implicit)"
 :tags '(:expensive-test)
 ;; TODO: as things are currently implemented, this way of expressing
 ;; the rule is quite expensive, since we end up computing intervals and
 ;; recurrences for every day of the year, even though the only relevant
 ;; days are in January and there are no recurrences on the other days.
 ;; We could try to optimize e.g. icr:refine-from-clauses to deal with such
 ;; cases.
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1998 :month 1 :day 1
                               :hour 9 :minute 0 :second 0
                               :zone ict:est :dst nil)
 :high (ical:make-date-time :year 2000 :month 2 :day 1
                            :hour 9 :minute 0 :second 0
                            :zone ict:est :dst nil)
 :members
 ;; (1998 9:00 AM EST)January 1-31
 ;; (1999 9:00 AM EST)January 1-31
 ;; (2000 9:00 AM EST)January 1-31
 (append
  (mapcar
   (lambda (day) (ical:date-time-variant dtstart :day day :tz 'preserve))
   (number-sequence 1 31))
  (mapcar
   (lambda (day)
     (ical:date-time-variant dtstart :year 1999 :day day :tz 'preserve))
   (number-sequence 1 31))
  (mapcar
   (lambda (day)
     (ical:date-time-variant dtstart :year 2000 :day day :tz 'preserve))
   (number-sequence 1 31)))
 :source rfc5545-sec3.8.5.3/6)

(ict:rrule-test
 "RRULE:FREQ=WEEKLY;COUNT=10\n"
 "Weekly for ten occurrences"
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 9 :day 2
                               :hour 9 :minute 0 :second 0
                               :zone ict:edt :dst t)
 :members
 (append
  ;; (1997 9:00 AM EDT) September 2,9,16,23,30;October 7,14,21
  (mapcar
   (lambda (day)
     (ical:date-time-variant dtstart :day day :tz 'preserve))
   (list 2 9 16 23 30))
  (mapcar
   (lambda (day)
     (ical:date-time-variant dtstart :month 10 :day day :tz 'preserve))
   (list 7 14 21))
  ;; (1997 9:00 AM EST) October 28;November 4
  (list
   (ical:make-date-time :year 1997 :month 10 :day 28
                        :hour 9 :minute 0 :second 0
                        :zone ict:est :dst nil)
   (ical:make-date-time :year 1997 :month 11 :day 4
                        :hour 9 :minute 0 :second 0
                        :zone ict:est :dst nil)))
 :source rfc5545-sec3.8.5.3/7)

(ict:rrule-test
 "RRULE:FREQ=WEEKLY;UNTIL=19971224T000000Z\n"
 "Every week until December 24, 1997"
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 9 :day 2
                               :hour 9 :minute 0 :second 0
                               :zone ict:edt :dst t)
 :members
 (let ((oct97 (ical:date-time-variant dtstart :month 10
                                      :zone ict:edt :dst t))
       (nov97 (ical:date-time-variant dtstart :month 11
                                      :zone ict:est :dst nil))
       (dec97 (ical:date-time-variant dtstart :month 12
                                      :zone ict:est :dst nil)))
   (append
    ;; (1997 9:00 AM EDT) September 2,9,16,23,30;
    ;;                    October 7,14,21
    (mapcar
     (lambda (day)
       (ical:date-time-variant dtstart :day day :tz 'preserve))
     (list 2 9 16 23 30))
    (mapcar
     (lambda (day)
       (ical:date-time-variant oct97 :day day :tz 'preserve))
     (list 7 14 21))
    ;; (1997 9:00 AM EST) October 28;
    ;;                    November 4,11,18,25;
    ;;                    December 2,9,16,23
    (list (ical:date-time-variant oct97 :day 28 :zone ict:est :dst nil))
    (mapcar
     (lambda (day)
       (ical:date-time-variant nov97 :day day :tz 'preserve))
     (list 4 11 18 25))
    (mapcar
     (lambda (day)
       (ical:date-time-variant dec97 :day day :tz 'preserve))
     (list 2 9 16 23))))
 :source rfc5545-sec3.8.5.3/8)

(ict:rrule-test
 "RRULE:FREQ=WEEKLY;INTERVAL=2;WKST=SU\n"
 "Every other week - forever; Weekstart on Sunday"
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 9 :day 2
                               :hour 9 :minute 0 :second 0
                               :zone ict:edt :dst t)
 :high (ical:make-date-time :year 1998 :month 3 :day 1
                            :hour 9 :minute 0 :second 0
                            :zone ict:est :dst nil)
 :members
 (list
  ;; ==> (1997 9:00 AM EDT) September 2,16,30;
  ;;                        October 14
  dtstart
  (ical:date-time-variant dtstart :day 16 :tz 'preserve)
  (ical:date-time-variant dtstart :day 30 :tz 'preserve)
  (ical:date-time-variant dtstart :month 10 :day 14 :tz 'preserve)
  ;;    (1997 9:00 AM EST) October 28;
  ;;                       November 11,25;
  ;;                       December 9,23
  (ical:date-time-variant dtstart :month 10 :day 28 :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 11 :day 11 :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 11 :day 25 :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 12 :day 23 :zone ict:est :dst nil)
  ;;    (1998 9:00 AM EST) January 6,20;
  ;;                       February 3, 17
  (ical:date-time-variant dtstart :year 1998 :month 1 :day 6
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :year 1998 :month 1 :day 20
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :year 1998 :month 2 :day 3
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :year 1998 :month 2 :day 17
                          :zone ict:est :dst nil))
 :source rfc5545-sec3.8.5.3/9)

(ict:rrule-test
"RRULE:FREQ=WEEKLY;UNTIL=19971007T000000Z;WKST=SU;BYDAY=TU,TH\n"
"Weekly on Tuesday and Thursday for five weeks, using UNTIL"
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 9 :day 2
                               :hour 9 :minute 0 :second 0
                               :zone ict:edt :dst t)
 :high (ical:make-date-time :year 1997 :month 10 :day 8
                            :hour 0 :minute 0 :second 0 :zone 0)
 :members
 (list
  ;; ==> (1997 9:00 AM EDT) September 2,4,9,11,16,18,23,25,30;
  ;;                        October 2
  dtstart
  (ical:date-time-variant dtstart :day 4 :tz 'preserve)
  (ical:date-time-variant dtstart :day 9 :tz 'preserve)
  (ical:date-time-variant dtstart :day 11 :tz 'preserve)
  (ical:date-time-variant dtstart :day 16 :tz 'preserve)
  (ical:date-time-variant dtstart :day 18 :tz 'preserve)
  (ical:date-time-variant dtstart :day 23 :tz 'preserve)
  (ical:date-time-variant dtstart :day 25 :tz 'preserve)
  (ical:date-time-variant dtstart :day 30 :tz 'preserve)
  (ical:date-time-variant dtstart :month 10 :day 2 :tz 'preserve))
 :source rfc5545-sec3.8.5.3/10)

(ict:rrule-test
"RRULE:FREQ=WEEKLY;COUNT=10;WKST=SU;BYDAY=TU,TH\n"
"Weekly on Tuesday and Thursday for five weeks, using COUNT"
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 9 :day 2
                               :hour 9 :minute 0 :second 0
                               :zone ict:edt :dst t)
 :high (ical:make-date-time :year 1997 :month 10 :day 8
                            :hour 0 :minute 0 :second 0 :zone 0)
 :members
 (list
  ;; ==> (1997 9:00 AM EDT) September 2,4,9,11,16,18,23,25,30;
  ;;                        October 2
  dtstart
  (ical:date-time-variant dtstart :day 4 :tz 'preserve)
  (ical:date-time-variant dtstart :day 9 :tz 'preserve)
  (ical:date-time-variant dtstart :day 11 :tz 'preserve)
  (ical:date-time-variant dtstart :day 16 :tz 'preserve)
  (ical:date-time-variant dtstart :day 18 :tz 'preserve)
  (ical:date-time-variant dtstart :day 23 :tz 'preserve)
  (ical:date-time-variant dtstart :day 25 :tz 'preserve)
  (ical:date-time-variant dtstart :day 30 :tz 'preserve)
  (ical:date-time-variant dtstart :month 10 :day 2 :tz 'preserve))
 :source rfc5545-sec3.8.5.3/11)

(ict:rrule-test
 "RRULE:FREQ=WEEKLY;INTERVAL=2;UNTIL=19971224T000000Z;WKST=SU;BYDAY=MO,WE,FR\n"
 "Every other week on Monday, Wednesday, and Friday until December 24,
1997, starting on Monday, September 1, 1997"
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 9 :day 1
                               :hour 9 :minute 0 :second 0
                               :zone ict:edt :dst t)
 :members
 (list
  dtstart
  ;; ==> (1997 9:00 AM EDT) September 1,3,5,15,17,19,29;
  ;;                        October 1,3,13,15,17
  (ical:date-time-variant dtstart :day 3 :tz 'preserve)
  (ical:date-time-variant dtstart :day 5 :tz 'preserve)
  (ical:date-time-variant dtstart :day 15 :tz 'preserve)
  (ical:date-time-variant dtstart :day 17 :tz 'preserve)
  (ical:date-time-variant dtstart :day 19 :tz 'preserve)
  (ical:date-time-variant dtstart :day 29 :tz 'preserve)
  (ical:date-time-variant dtstart :month 10 :day 1 :tz 'preserve)
  (ical:date-time-variant dtstart :month 10 :day 3 :tz 'preserve)
  (ical:date-time-variant dtstart :month 10 :day 13 :tz 'preserve)
  (ical:date-time-variant dtstart :month 10 :day 15 :tz 'preserve)
  (ical:date-time-variant dtstart :month 10 :day 17 :tz 'preserve)
  ;;     (1997 9:00 AM EST) October 27,29,31;
  ;;                        November 10,12,14,24,26,28;
  ;;                        December 8,10,12,22
  (ical:date-time-variant dtstart :month 10 :day 27 :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 10 :day 29 :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 10 :day 31 :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 11 :day 10 :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 11 :day 12 :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 11 :day 14 :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 11 :day 24 :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 11 :day 26 :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 11 :day 28 :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 12 :day 8  :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 12 :day 10 :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 12 :day 12 :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 12 :day 22 :zone ict:est :dst nil))
 :nonmembers
 (list
  ;; These match the rule, but are just past the UNTIL date:
  (ical:date-time-variant dtstart :month 12 :day 24 :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 12 :day 26 :zone ict:est :dst nil))
 :source rfc5545-sec3.8.5.3/12)

(ict:rrule-test
 "RRULE:FREQ=WEEKLY;INTERVAL=2;COUNT=8;WKST=SU;BYDAY=TU,TH\n"
 "Every other week on Tuesday and Thursday, for 8 occurrences"
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 9 :day 2
                               :hour 9 :minute 0 :second 0
                               :zone ict:edt :dst t)
 :members
 ;; ==> (1997 9:00 AM EDT) September 2,4,16,18,30;
 ;;                        October 2,14,16
 (list
   dtstart
   (ical:date-time-variant dtstart :day 4 :tz 'preserve)
   (ical:date-time-variant dtstart :day 16 :tz 'preserve)
   (ical:date-time-variant dtstart :day 18 :tz 'preserve)
   (ical:date-time-variant dtstart :day 30 :tz 'preserve)
   (ical:date-time-variant dtstart :month 10 :day 2 :tz 'preserve)
   (ical:date-time-variant dtstart :month 10 :day 14 :tz 'preserve)
   (ical:date-time-variant dtstart :month 10 :day 16 :tz 'preserve))
 :source rfc5545-sec3.8.5.3/13)

(ict:rrule-test
 "RRULE:FREQ=MONTHLY;COUNT=10;BYDAY=1FR\n"
 "Monthly on the first Friday for 10 occurrences"
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 9 :day 5
                               :hour 9 :minute 0 :second 0
                               :zone ict:edt :dst t)
 :members
 (list
  ;; ==> (1997 9:00 AM EDT) September 5;October 3
  dtstart
  (ical:date-time-variant dtstart :month 10 :day 3 :tz 'preserve)
  ;;     (1997 9:00 AM EST) November 7;December 5
  (ical:date-time-variant dtstart :month 11 :day 7 :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 12 :day 5 :zone ict:est :dst nil)
  ;;     (1998 9:00 AM EST) January 2;February 6;March 6;April 3
  (ical:date-time-variant dtstart :year 1998 :month 1 :day 2
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :year 1998 :month 2 :day 6
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :year 1998 :month 3 :day 6
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :year 1998 :month 4 :day 3
                          :zone ict:est :dst nil)
  ;;     (1998 9:00 AM EDT) May 1;June 5
  (ical:date-time-variant dtstart :year 1998 :month 5 :day 1 :tz 'preserve)
  (ical:date-time-variant dtstart :year 1998 :month 6 :day 5 :tz 'preserve))
  :source rfc5545-sec3.8.5.3/14)

(ict:rrule-test
 "RRULE:FREQ=MONTHLY;UNTIL=19971224T000000Z;BYDAY=1FR\n"
 "Monthly on the first Friday until December 24, 1997"
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 9 :day 5
                               :hour 9 :minute 0 :second 0
                               :zone ict:edt :dst t)
 :members
 (list
  ;; ==> (1997 9:00 AM EDT) September 5; October 3
  dtstart
  (ical:date-time-variant dtstart :month 10 :day 3 :tz 'preserve)
  ;;     (1997 9:00 AM EST) November 7;December 5
  (ical:date-time-variant dtstart :month 11 :day 7 :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 12 :day 5 :zone ict:est :dst nil))
 :source rfc5545-sec3.8.5.3/15)

(ict:rrule-test
 "RRULE:FREQ=MONTHLY;INTERVAL=2;COUNT=10;BYDAY=1SU,-1SU\n"
 "Every other month on the first and last Sunday of the month for 10 occurrences"
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 9 :day 7
                               :hour 9 :minute 0 :second 0
                               :zone ict:edt :dst t)
 :members
 (list
  ;; ==> (1997 9:00 AM EDT) September 7,28
  dtstart
  (ical:date-time-variant dtstart :day 28 :tz 'preserve)
  ;;     (1997 9:00 AM EST) November 2,30
  (ical:date-time-variant dtstart :month 11 :day 2 :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 11 :day 30 :zone ict:est :dst nil)
  ;;     (1998 9:00 AM EST) January 4,25;March 1,29
  (ical:date-time-variant dtstart :year 1998 :month 1 :day 4
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :year 1998 :month 1 :day 25
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :year 1998 :month 3 :day 1
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :year 1998 :month 3 :day 29
                          :zone ict:est :dst nil)
  ;;     (1998 9:00 AM EDT) May 3,31
  (ical:date-time-variant dtstart :year 1998 :month 5 :day 3 :tz 'preserve)
  (ical:date-time-variant dtstart :year 1998 :month 5 :day 31 :tz 'preserve))
 :source rfc5545-sec3.8.5.3/16)

(ict:rrule-test
 "RRULE:FREQ=MONTHLY;COUNT=6;BYDAY=-2MO\n"
 "Monthly on the second-to-last Monday of the month for 6 months"
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 9 :day 22
                               :hour 9 :minute 0 :second 0
                               :zone ict:edt :dst t)
 :members
 (list
  ;; ==> (1997 9:00 AM EDT) September 22;October 20
  dtstart
  (ical:date-time-variant dtstart :month 10 :day 20 :tz 'preserve)
  ;;     (1997 9:00 AM EST) November 17;December 22
  (ical:date-time-variant dtstart :month 11 :day 17
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 12 :day 22
                          :zone ict:est :dst nil)
  ;;     (1998 9:00 AM EST) January 19;February 16
  (ical:date-time-variant dtstart :year 1998 :month 1 :day 19
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :year 1998 :month 2 :day 16
                          :zone ict:est :dst nil))
 :source rfc5545-sec3.8.5.3/17)

(ict:rrule-test
 "RRULE:FREQ=MONTHLY;BYMONTHDAY=-3\n"
 "Monthly on the third-to-the-last day of the month, forever"
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 9 :day 28
                               :hour 9 :minute 0 :second 0
                               :zone ict:edt :dst t)
 :high (ical:make-date-time :year 1998 :month 3 :day 1
                            :hour 0 :minute 0 :second 0
                            :zone ict:est :dst nil)
 :members
 (list
  ;; ==> (1997 9:00 AM EDT) September 28
  dtstart
  ;;     (1997 9:00 AM EST) October 29;November 28;December 29
  (ical:date-time-variant dtstart :month 10 :day 29
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 11 :day 28
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 12 :day 29
                          :zone ict:est :dst nil)
  ;;     (1998 9:00 AM EST) January 29;February 26
  (ical:date-time-variant dtstart :year 1998 :month 1 :day 29
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :year 1998 :month 2 :day 26
                          :zone ict:est :dst nil))
 :source rfc5545-sec3.8.5.3/18)

(ict:rrule-test
 "RRULE:FREQ=MONTHLY;COUNT=10;BYMONTHDAY=2,15\n"
 "Monthly on the 2nd and 15th of the month for 10 occurrences"
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 9 :day 2
                               :hour 9 :minute 0 :second 0
                               :zone ict:edt :dst t)
 :members
 (list
  ;; ==> (1997 9:00 AM EDT) September 2,15;October 2,15
  dtstart
  (ical:date-time-variant dtstart :day 15 :tz 'preserve)
  (ical:date-time-variant dtstart :month 10 :day 2 :tz 'preserve)
  (ical:date-time-variant dtstart :month 10 :day 15 :tz 'preserve)
  ;;     (1997 9:00 AM EST) November 2,15;December 2,15
  (ical:date-time-variant dtstart :month 11 :day 2
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 11 :day 15
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 12 :day 2
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 12 :day 15
                          :zone ict:est :dst nil)
  ;;     (1998 9:00 AM EST) January 2,15
  (ical:date-time-variant dtstart :year 1998 :month 1 :day 2
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :year 1998 :month 1 :day 15
                          :zone ict:est :dst nil))
 :source rfc5545-sec3.8.5.3/19)

(ict:rrule-test
 "RRULE:FREQ=MONTHLY;COUNT=10;BYMONTHDAY=1,-1\n"
 "Monthly on the first and last day of the month for 10 occurrences"
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 9 :day 30
                               :hour 9 :minute 0 :second 0
                               :zone ict:edt :dst t)
 :members
 (list
  ;; ==> (1997 9:00 AM EDT) September 30;October 1
  dtstart
  (ical:date-time-variant dtstart :month 10 :day 1 :tz 'preserve)
  ;;     (1997 9:00 AM EST) October 31;November 1,30;December 1,31
  (ical:date-time-variant dtstart :month 10 :day 31
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 11 :day 1
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 11 :day 30
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 12 :day 1
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 12 :day 31
                          :zone ict:est :dst nil)
  ;;     (1998 9:00 AM EST) January 1,31;February 1
  (ical:date-time-variant dtstart :year 1998 :month 1 :day 1
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :year 1998 :month 1 :day 31
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :year 1998 :month 2 :day 1
                          :zone ict:est :dst nil))
 :source rfc5545-sec3.8.5.3/20)

(ict:rrule-test
 "RRULE:FREQ=MONTHLY;INTERVAL=18;COUNT=10;BYMONTHDAY=10,11,12,13,14,15\n"
 "Every 18 months on the 10th thru 15th of the month for 10 occurrences"
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 9 :day 10
                               :hour 9 :minute 0 :second 0
                               :zone ict:edt :dst t)
 :members
 (append
  (list
   ;; ==> (1997 9:00 AM EDT) September 10,11,12,13,14,15
   dtstart
   (ical:date-time-variant dtstart :day 11 :tz 'preserve)
   (ical:date-time-variant dtstart :day 12 :tz 'preserve)
   (ical:date-time-variant dtstart :day 13 :tz 'preserve)
   (ical:date-time-variant dtstart :day 14 :tz 'preserve)
   (ical:date-time-variant dtstart :day 15 :tz 'preserve))

  ;;     (1999 9:00 AM EST) March 10,11,12,13
  (let ((mar99 (ical:make-date-time :year 1999 :month 3 :day 10
                                    :hour 9 :minute 0 :second 0
                                    :zone ict:est :dst nil)))
    (list
     mar99
     (ical:date-time-variant mar99 :day 11 :tz 'preserve)
     (ical:date-time-variant mar99 :day 12 :tz 'preserve)
     (ical:date-time-variant mar99 :day 13 :tz 'preserve))))
 :nonmembers
 (list
  ;; These match the rule but are excluded by the COUNT clause:
  (ical:make-date-time :year 1999 :month 3 :day 14
                       :hour 9 :minute 0 :second 0
                       :zone ict:est :dst nil)
  (ical:make-date-time :year 1999 :month 3 :day 15
                       :hour 9 :minute 0 :second 0
                       :zone ict:est :dst nil))
 :source rfc5545-sec3.8.5.3/21)

(ict:rrule-test
 "RRULE:FREQ=MONTHLY;INTERVAL=2;BYDAY=TU\n"
 "Every Tuesday, every other month"
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 9 :day 2
                               :hour 9 :minute 0 :second 0
                               :zone ict:edt :dst t)
 :high (ical:make-date-time :year 1998 :month 4 :day 1
                            :hour 0 :minute 0 :second 0
                            :zone ict:est :dst nil)
 :members
 (list
  ;; ==> (1997 9:00 AM EDT) September 2,9,16,23,30
  dtstart
  (ical:date-time-variant dtstart :day 9 :tz 'preserve)
  (ical:date-time-variant dtstart :day 16 :tz 'preserve)
  (ical:date-time-variant dtstart :day 23 :tz 'preserve)
  (ical:date-time-variant dtstart :day 30 :tz 'preserve)
  ;;     (1997 9:00 AM EST) November 4,11,18,25
  (ical:date-time-variant dtstart :month 11 :day 4
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 11 :day 11
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 11 :day 11
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 11 :day 25
                          :zone ict:est :dst nil)
  ;;     (1998 9:00 AM EST) January 6,13,20,27;March 3,10,17,24,31
  (ical:date-time-variant dtstart :year 1998 :month 1 :day 6
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :year 1998 :month 1 :day 13
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :year 1998 :month 1 :day 20
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :year 1998 :month 1 :day 27
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :year 1998 :month 3 :day 3
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :year 1998 :month 3 :day 10
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :year 1998 :month 3 :day 17
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :year 1998 :month 3 :day 24
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :year 1998 :month 3 :day 31
                          :zone ict:est :dst nil))
 :nonmembers
 ;; e.g. Tuesdays in December 1997:
 (list
  (ical:date-time-variant dtstart :month 12 :day 2 :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 12 :day 9 :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 12 :day 16 :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 12 :day 23 :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 12 :day 30 :zone ict:est :dst nil))
 :source rfc5545-sec3.8.5.3/22)

(ict:rrule-test
 "RRULE:FREQ=YEARLY;COUNT=10;BYMONTH=6,7\n"
 "Yearly in June and July for 10 occurrences"
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 6 :day 10
                               :hour 9 :minute 0 :second 0
                               :zone ict:edt :dst t)
 ;; Note: Since none of the BYDAY, BYMONTHDAY, or BYYEARDAY
 ;; clauses are specified, the month day is gotten from "DTSTART"
 :members
 ;; ==> (1997 9:00 AM EDT) June 10;July 10
 ;;     (1998 9:00 AM EDT) June 10;July 10
 ;;     (1999 9:00 AM EDT) June 10;July 10
 ;;     (2000 9:00 AM EDT) June 10;July 10
 ;;     (2001 9:00 AM EDT) June 10;July 10
 (mapcan
  (lambda (y)
    (list
     (ical:date-time-variant dtstart :year y :month 6 :tz 'preserve)
     (ical:date-time-variant dtstart :year y :month 7 :tz 'preserve)))
  (number-sequence 1997 2001))
 :source rfc5545-sec3.8.5.3/23)

(ict:rrule-test
 "RRULE:FREQ=YEARLY;INTERVAL=2;COUNT=10;BYMONTH=1,2,3\n"
 "Every other year on January, February, and March for 10 occurrences"
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 3 :day 10
                               :hour 9 :minute 0 :second 0
                               :zone ict:est :dst nil)
 :members
 ;; ==> (1997 9:00 AM EST) March 10
 ;;     (1999 9:00 AM EST) January 10;February 10;March 10
 ;;     (2001 9:00 AM EST) January 10;February 10;March 10
 ;;     (2003 9:00 AM EST) January 10;February 10;March 10
 (cons
  dtstart
  ;; FIXME: this mapcan appears to produce a spurious warning:
  (with-suppressed-warnings ((ignored-return-value mapcan))
    (mapcan
     (lambda (y)
       (list
        (ical:date-time-variant dtstart :year y :month 1 :tz 'preserve)
        (ical:date-time-variant dtstart :year y :month 2 :tz 'preserve)
        (ical:date-time-variant dtstart :year y :month 3 :tz 'preserve)))
   (list 1999 2001 2003))))
 :source rfc5545-sec3.8.5.3/24)

(ict:rrule-test
"RRULE:FREQ=YEARLY;INTERVAL=3;COUNT=10;BYYEARDAY=1,100,200\n"
"Every third year on the 1st, 100th, and 200th day for 10 occurrences"
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 1 :day 1
                               :hour 9 :minute 0 :second 0
                               :zone ict:est :dst nil)

 :members
 (list
  ;; ==> (1997 9:00 AM EST) January 1
  dtstart
  ;;     (1997 9:00 AM EDT) April 10;July 19
  (ical:date-time-variant dtstart :month 4 :day 10 :zone ict:edt :dst t)
  (ical:date-time-variant dtstart :month 7 :day 19 :zone ict:edt :dst t)
  ;;     (2000 9:00 AM EST) January 1
  (ical:date-time-variant dtstart :year 2000 :tz 'preserve)
  ;;     (2000 9:00 AM EDT) April 9;July 18
  (ical:date-time-variant dtstart :year 2000 :month 4 :day 9 :zone ict:edt :dst t)
  (ical:date-time-variant dtstart :year 2000 :month 7 :day 18 :zone ict:edt :dst t)
  ;;     (2003 9:00 AM EST) January 1
  (ical:date-time-variant dtstart :year 2003 :tz 'preserve)
  ;;     (2003 9:00 AM EDT) April 10;July 19
  (ical:date-time-variant dtstart :year 2003 :month 4 :day 10 :zone ict:edt :dst t)
  (ical:date-time-variant dtstart :year 2003 :month 7 :day 19 :zone ict:edt :dst t)
  ;;     (2006 9:00 AM EST) January 1
  (ical:date-time-variant dtstart :year 2006 :tz 'preserve))
 :source rfc5545-sec3.8.5.3/25)

(ict:rrule-test
 "RRULE:FREQ=YEARLY;BYDAY=20MO\n"
 "Every 20th Monday of the year, forever"
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 5 :day 19
                               :hour 9 :minute 0 :second 0
                               :zone ict:edt :dst t)
 :members
 (list
  ;; ==> (1997 9:00 AM EDT) May 19
  ;;     (1998 9:00 AM EDT) May 18
  ;;     (1999 9:00 AM EDT) May 17
  ;;     ...
  dtstart
  (ical:date-time-variant dtstart :year 1998 :day 18 :tz 'preserve)
  (ical:date-time-variant dtstart :year 1999 :day 17 :tz 'preserve))
 :source rfc5545-sec3.8.5.3/26)

(ict:rrule-test
 "RRULE:FREQ=YEARLY;BYWEEKNO=20;BYDAY=MO\n"
 "Every year on Monday in Week 20 (where the week starts Monday), forever"
 :tz ict:tz-eastern
 :dtstart
 (ical:make-date-time :year 1997 :month 5 :day 12
                      :hour 9 :minute 0 :second 0
                      :zone ict:edt :dst t)
 :members
 (list
  (ical:date-time-variant dtstart :year 1998 :day 11 :tz 'preserve)
  (ical:date-time-variant dtstart :year 1999 :day 17 :tz 'preserve))
 :nonmembers
 (list
  (ical:date-time-variant dtstart :year 1998 :day 12 :tz 'preserve) ; a Tuesday
  (ical:date-time-variant dtstart :year 1998 :day 18 :tz 'preserve)) ; wrong weekno
 :source rfc5545-sec3.8.5.3/27)

(ict:rrule-test
 "RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=TH\n"
 "Every Thursday in March, forever"
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 3 :day 13
                               :hour 9 :minute 0 :second 0
                               :zone ict:est :dst nil)
 :members
 (append
  ;; ==> (1997 9:00 AM EST) March 13,20,27
  (mapcar
   (lambda (d)
     (ical:date-time-variant dtstart :day d :tz 'preserve))
   (list 13 20 27))
  ;;     (1998 9:00 AM EST) March 5,12,19,26
  (mapcar
   (lambda (d)
     (ical:date-time-variant dtstart :year 1998 :day d :tz 'preserve))
   (list 5 12 19 26))
  ;;     (1999 9:00 AM EST) March 4,11,18,25
  (mapcar
   (lambda (d)
     (ical:date-time-variant dtstart :year 1999 :day d :tz 'preserve))
   (list 4 11 18 25)))
 :source rfc5545-sec3.8.5.3/28)

(ict:rrule-test
"RRULE:FREQ=YEARLY;BYDAY=TH;BYMONTH=6,7,8\n"
"Every Thursday, but only during June, July, and August, forever"
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 6 :day 5
                               :hour 9 :minute 0 :second 0
                               :zone ict:edt :dst t)
 :members
 (append
  ;; ==> (1997 9:00 AM EDT) June 5,12,19,26;July 3,10,17,24,31;
  ;;                        August 7,14,21,28
  (mapcar
   (lambda (d)
     (ical:date-time-variant dtstart :day d :tz 'preserve))
   (list 5 12 19 26))
  (mapcar
   (lambda (d)
     (ical:date-time-variant dtstart :month 7 :day d :tz 'preserve))
   (list 3 10 17 24 31))
  (mapcar
   (lambda (d)
     (ical:date-time-variant dtstart :month 8 :day d :tz 'preserve))
   (list 7 14 21 28))
  ;;     (1998 9:00 AM EDT) June 4,11,18,25;July 2,9,16,23,30;
  ;;                        August 6,13,20,27
  (mapcar
   (lambda (d)
     (ical:date-time-variant dtstart :year 1998 :day d :tz 'preserve))
   (list 4 11 18 25))
  (mapcar
   (lambda (d)
     (ical:date-time-variant dtstart :year 1998 :month 7 :day d :tz 'preserve))
   (list 2 9 16 23 30))
  (mapcar
   (lambda (d)
     (ical:date-time-variant dtstart :year 1998 :month 8 :day d :tz 'preserve))
   (list 6 13 20 27))
  ;;     (1999 9:00 AM EDT) June 3,10,17,24;July 1,8,15,22,29;
  ;;                        August 5,12,19,26
  (mapcar
   (lambda (d)
     (ical:date-time-variant dtstart :year 1999 :day d :tz 'preserve))
   (list 3 10 17 24))
  (mapcar
   (lambda (d)
     (ical:date-time-variant dtstart :year 1999 :month 7 :day d :tz 'preserve))
   (list 1 8 15 22 29))
  (mapcar
   (lambda (d)
     (ical:date-time-variant dtstart :year 1999 :month 8 :day d :tz 'preserve))
   (list 5 12 19 26)))
 :source rfc5545-sec3.8.5.3/29)

(ict:rrule-test
 "RRULE:FREQ=MONTHLY;BYDAY=FR;BYMONTHDAY=13\n"
 "Every Friday the 13th, forever, *excluding* DTSTART "
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 9 :day 2
                               :hour 9 :minute 0 :second 0
                               :zone ict:edt :dst t)
 :high (ical:make-date-time :year 2000 :month 10 :day 14
                            :hour 0 :minute 0 :second 0
                            :zone ict:edt :dst t)
 :exdates (list dtstart)
 :members
 (list
  ;; ==> (1998 9:00 AM EST) February 13;March 13;November 13
  ;;     (1999 9:00 AM EDT) August 13
  ;;     (2000 9:00 AM EDT) October 13
  ;;     ...
  (ical:date-time-variant dtstart :year 1998 :month 2 :day 13
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :year 1998 :month 3 :day 13
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :year 1998 :month 11 :day 13
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :year 1999 :month 8 :day 13 :tz 'preserve)
  (ical:date-time-variant dtstart :year 2000 :month 10 :day 13 :tz 'preserve))
 :source rfc5545-sec3.8.5.3/30)

(ict:rrule-test
 "RRULE:FREQ=MONTHLY;BYDAY=SA;BYMONTHDAY=7,8,9,10,11,12,13\n"
 "The first Saturday that follows the first Sunday of the month, forever"
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 9 :day 13
                               :hour 9 :minute 0 :second 0
                               :zone ict:edt :dst t)
 :high (ical:make-date-time :year 1998 :month 6 :day 14
                            :hour 0 :minute 0 :second 0
                            :zone ict:edt :dst t)
 :members
 (list
  ;; ==> (1997 9:00 AM EDT) September 13;October 11
  dtstart
  (ical:date-time-variant dtstart :month 10 :day 11 :tz 'preserve)
  ;;     (1997 9:00 AM EST) November 8;December 13
  (ical:date-time-variant dtstart :month 11 :day 8 :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 12 :day 13 :zone ict:est :dst nil)
  ;;     (1998 9:00 AM EST) January 10;February 7;March 7
  (ical:date-time-variant dtstart :year 1998 :month 1 :day 10
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :year 1998 :month 2 :day 7
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :year 1998 :month 3 :day 7
                          :zone ict:est :dst nil)
  ;;     (1998 9:00 AM EDT) April 11;May 9;June 13...
  (ical:date-time-variant dtstart :year 1998 :month 4 :day 11 :tz 'preserve)
  (ical:date-time-variant dtstart :year 1998 :month 5 :day 9 :tz 'preserve)
  (ical:date-time-variant dtstart :year 1998 :month 6 :day 13 :tz 'preserve))
 :source rfc5545-sec3.8.5.3/31)

(ict:rrule-test
 "RRULE:FREQ=YEARLY;INTERVAL=4;BYMONTH=11;BYDAY=TU;BYMONTHDAY=2,3,4,5,6,7,8\n"
 "Every 4 years, the first Tuesday after a Monday in November, forever
(U.S. Presidential Election day)"
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1996 :month 11 :day 5
                               :hour 9 :minute 0 :second 0
                               :zone ict:est :dst nil)

 :members
 (list
  ;; ==> (1996 9:00 AM EST) November 5
  dtstart
  ;;     (2000 9:00 AM EST) November 7
  (ical:date-time-variant dtstart :year 2000 :day 7 :tz 'preserve)
  ;;     (2004 9:00 AM EST) November 2
  (ical:date-time-variant dtstart :year 2004 :day 2 :tz 'preserve))
  :source rfc5545-sec3.8.5.3/32)

(ict:rrule-test
 "RRULE:FREQ=MONTHLY;COUNT=3;BYDAY=TU,WE,TH;BYSETPOS=3\n"
 "The third instance into the month of one of Tuesday, Wednesday, or
Thursday, for the next 3 months"
 ;; TODO: Yikes, why is this so slow??
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 9 :day 4
                               :hour 9 :minute 0 :second 0
                               :zone ict:edt :dst t)

 :members
 (list
  ;; ==> (1997 9:00 AM EDT) September 4;October 7
  ;;     (1997 9:00 AM EST) November 6
  dtstart
  (ical:date-time-variant dtstart :month 10 :day 7 :tz 'preserve)
  (ical:date-time-variant dtstart :month 11 :day 6 :zone ict:est :dst nil))
:source rfc5545-sec3.8.5.3/33)

(ict:rrule-test
 "RRULE:FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-2\n"
 "The second-to-last weekday of the month"
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 9 :day 29
                               :hour 9 :minute 0 :second 0
                               :zone ict:edt :dst t)
 :high (ical:make-date-time :year 1998 :month 4 :day 1
                            :hour 0 :minute 0 :second 0
                            :zone ict:est :dst nil)
 :members
 (list
  ;; ==> (1997 9:00 AM EDT) September 29
  dtstart
  ;;     (1997 9:00 AM EST) October 30;November 27;December 30
  (ical:date-time-variant dtstart :month 10 :day 30 :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 11 :day 27 :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :month 12 :day 30 :zone ict:est :dst nil)
  ;;     (1998 9:00 AM EST) January 29;February 26;March 30
  (ical:date-time-variant dtstart :year 1998 :month 1 :day 29
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :year 1998 :month 2 :day 26
                          :zone ict:est :dst nil)
  (ical:date-time-variant dtstart :year 1998 :month 3 :day 30
                          :zone ict:est :dst nil))
 :source rfc5545-sec3.8.5.3/34)

(ict:rrule-test
 ;; corrected, see Errata ID 3883: https://www.rfc-editor.org/errata/eid3883
 "RRULE:FREQ=HOURLY;INTERVAL=3;UNTIL=19970902T210000Z\n"
 "Every 3 hours from 9:00 AM to 5:00 PM on a specific day"
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 9 :day 2
                               :hour 9 :minute 0 :second 0
                               :zone ict:edt :dst t)

 :members
 (list
  ;; ==> (September 2, 1997 EDT) 09:00,12:00,15:00
  dtstart
  (ical:date-time-variant dtstart :hour 12 :tz 'preserve)
  (ical:date-time-variant dtstart :hour 15 :tz 'preserve))
 :source rfc5545-sec3.8.5.3/35)

(ict:rrule-test
 "RRULE:FREQ=MINUTELY;INTERVAL=15;COUNT=6\n"
 "Every 15 minutes for 6 occurrences"
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 9 :day 2
                               :hour 9 :minute 0 :second 0
                               :zone ict:edt :dst t)

 :members
 (list
  ;; ==> (September 2, 1997 EDT) 09:00,09:15,09:30,09:45,10:00,10:15
  dtstart
  (ical:date-time-variant dtstart :minute 15 :tz 'preserve)
  (ical:date-time-variant dtstart :minute 30 :tz 'preserve)
  (ical:date-time-variant dtstart :minute 45 :tz 'preserve)
  (ical:date-time-variant dtstart :hour 10 :minute 0 :tz 'preserve)
  (ical:date-time-variant dtstart :hour 10 :minute 15 :tz 'preserve))
 :source rfc5545-sec3.8.5.3/36)

(ict:rrule-test
 "RRULE:FREQ=MINUTELY;INTERVAL=90;COUNT=4\n"
 "Every hour and a half for 4 occurrences"
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 9 :day 2
                               :hour 9 :minute 0 :second 0
                               :zone ict:edt :dst t)
 :members
 (list
  ;; ==> (September 2, 1997 EDT) 09:00,10:30;12:00;13:30
  dtstart
  (ical:date-time-variant dtstart :hour 10 :minute 30 :tz 'preserve)
  (ical:date-time-variant dtstart :hour 12 :minute 0 :tz 'preserve)
  (ical:date-time-variant dtstart :hour 13 :minute 30 :tz 'preserve))
 :source rfc5545-sec3.8.5.3/37)

(ict:rrule-test
 "RRULE:FREQ=DAILY;BYHOUR=9,10,11,12,13,14,15,16;BYMINUTE=0,20,40\n"
 "Every 20 minutes from 9:00 AM to 4:40 PM every day"
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 9 :day 2
                               :hour 9 :minute 0 :second 0
                               :zone ict:edt :dst t)
 :high (ical:make-date-time :year 1997 :month 9 :day 4
                            :hour 0 :minute 0 :second 0
                            :zone ict:edt :dst t)
 :members
 (append
  ;; ==> (September 2, 1997 EDT) 9:00,9:20,9:40,10:00,10:20,
  ;;                             ... 16:00,16:20,16:40
  (mapcan
   (lambda (h)
     (list
      (ical:date-time-variant dtstart :hour h :minute 0 :tz 'preserve)
      (ical:date-time-variant dtstart :hour h :minute 20 :tz 'preserve)
      (ical:date-time-variant dtstart :hour h :minute 40 :tz 'preserve)))
   (number-sequence 9 16))
  ;;     (September 3, 1997 EDT) 9:00,9:20,9:40,10:00,10:20,
  ;;                             ...16:00,16:20,16:40
  (mapcan
   (lambda (h)
     (list
      (ical:date-time-variant dtstart :hour h :day 3 :minute 0 :tz 'preserve)
      (ical:date-time-variant dtstart :hour h :day 3 :minute 20 :tz 'preserve)
      (ical:date-time-variant dtstart :hour h :day 3 :minute 40 :tz 'preserve)))
   (number-sequence 9 16)))
 :source rfc5545-sec3.8.5.3/38)

(ict:rrule-test
 "RRULE:FREQ=MINUTELY;INTERVAL=20;BYHOUR=9,10,11,12,13,14,15,16\n"
 "Every 20 minutes from 9:00 AM to 4:40 PM every day
(Alternative rule for the previous example)"
 :tags '(:expensive-test)
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 9 :day 2
                               :hour 9 :minute 0 :second 0
                               :zone ict:edt :dst t)
 :high (ical:make-date-time :year 1997 :month 9 :day 4
                            :hour 0 :minute 0 :second 0
                            :zone ict:edt :dst t)
 :members
 (append
  ;; ==> (September 2, 1997 EDT) 9:00,9:20,9:40,10:00,10:20,
  ;;                             ... 16:00,16:20,16:40
  (mapcan
   (lambda (h)
     (list
      (ical:date-time-variant dtstart :hour h :minute 0 :tz 'preserve)
      (ical:date-time-variant dtstart :hour h :minute 20 :tz 'preserve)
      (ical:date-time-variant dtstart :hour h :minute 40 :tz 'preserve)))
   (number-sequence 9 16))
  ;;     (September 3, 1997 EDT) 9:00,9:20,9:40,10:00,10:20,
  ;;                             ...16:00,16:20,16:40
  (mapcan
   (lambda (h)
     (list
      (ical:date-time-variant dtstart :hour h :day 3 :minute 0 :tz 'preserve)
      (ical:date-time-variant dtstart :hour h :day 3 :minute 20 :tz 'preserve)
      (ical:date-time-variant dtstart :hour h :day 3 :minute 40 :tz 'preserve)))
   (number-sequence 9 16)))
:source rfc5545-sec3.8.5.3/39)

(ict:rrule-test
 "RRULE:FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=MO\n"
 "An example where the days generated makes a difference because of WKST:
every other week on Tuesday and Sunday, week start Monday, for four recurrences"
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 8 :day 5
                               :hour 9 :minute 0 :second 0
                               :zone ict:edt :dst t)
 :members
 (list
  ;; ==> (1997 EDT) August 5,10,19,24
  dtstart
  (ical:date-time-variant dtstart :day 10 :tz 'preserve)
  (ical:date-time-variant dtstart :day 19 :tz 'preserve)
  (ical:date-time-variant dtstart :day 24 :tz 'preserve))
 :source rfc5545-sec3.8.5.3/40)

(ict:rrule-test
 "RRULE:FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=SU\n"
 "An example where the days generated makes a difference because of WKST:
every other week on Tuesday and Sunday, week start Sunday, for four recurrences"
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 1997 :month 8 :day 5
                               :hour 9 :minute 0 :second 0
                               :zone ict:edt :dst t)
 :members
 (list
  ;; ==> (1997 EDT) August 5,17,19,31
  dtstart
  (ical:date-time-variant dtstart :day 17 :tz 'preserve)
  (ical:date-time-variant dtstart :day 19 :tz 'preserve)
  (ical:date-time-variant dtstart :day 31 :tz 'preserve))
 :source rfc5545-sec3.8.5.3/41)

(ict:rrule-test
 "RRULE:FREQ=MONTHLY;BYMONTHDAY=15,30;COUNT=5\n"
 "An example where an invalid date (i.e., February 30) is ignored."
 :tz ict:tz-eastern
 :dtstart (ical:make-date-time :year 2007 :month 1 :day 15
                               :hour 9 :minute 0 :second 0
                               :zone ict:est :dst nil)
 :high (ical:make-date-time :year 2007 :month 4 :day 1
                               :hour 0 :minute 0 :second 0
                               :zone ict:edt :dst t)
 :members
 (list
  ;; ==> (2007 EST) January 15,30
  ;;     (2007 EST) February 15
  ;;     (2007 EDT) March 15,30
  dtstart
  (ical:date-time-variant dtstart :day 30 :tz 'preserve)
  (ical:date-time-variant dtstart :month 2 :day 15 :tz 'preserve)
  (ical:date-time-variant dtstart :month 3 :day 15 :zone ict:edt :dst t)
  (ical:date-time-variant dtstart :month 3 :day 30 :zone ict:edt :dst t))
 :nonmembers
 (list
  (ical:date-time-variant dtstart :month 2 :day 28 :tz 'preserve)
  (ical:date-time-variant dtstart :month 2 :day 30 :tz 'preserve))
 :source rfc5545-sec3.8.5.3/42)

;; Local Variables:
;; read-symbol-shorthands: (("ict:" . "icalendar-test-") ("icr:" . "icalendar-recur-") ("ical:" . "icalendar-"))
;; End:
;;; icalendar-recur-tests.el ends here
