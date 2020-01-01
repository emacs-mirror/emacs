;;; iso8601-tests.el --- tests for calendar/iso8601.el    -*- lexical-binding:t -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

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

(require 'ert)
(require 'iso8601)

(ert-deftest test-iso8601-date-years ()
  (should (equal (iso8601-parse-date "1985")
                 '(nil nil nil nil nil 1985 nil nil nil)))
  (should (equal (iso8601-parse-date "-0003")
                 '(nil nil nil nil nil -3 nil nil nil)))
  (should (equal (iso8601-parse-date "+1985")
                 '(nil nil nil nil nil 1985 nil nil nil))))

(ert-deftest test-iso8601-date-dates ()
  (should (equal (iso8601-parse-date "1985-03-14")
                 '(nil nil nil 14 3 1985 nil nil nil)))
  (should (equal (iso8601-parse-date "19850314")
                 '(nil nil nil 14 3 1985 nil nil nil)))
  (should (equal (iso8601-parse-date "1985-02")
                 '(nil nil nil nil 2 1985 nil nil nil))))

(ert-deftest test-iso8601-date-obsolete ()
  (should (equal (iso8601-parse-date "--02-01")
                 '(nil nil nil 1 2 nil nil nil nil)))
  (should (equal (iso8601-parse-date "--0201")
                 '(nil nil nil 1 2 nil nil nil nil))))

(ert-deftest test-iso8601-date-weeks ()
  (should (equal (iso8601-parse-date "2008W39-6")
                 '(nil nil nil 27 9 2008 nil nil nil)))
  (should (equal (iso8601-parse-date "2009W01-1")
                 '(nil nil nil 29 12 2008 nil nil nil)))
  (should (equal (iso8601-parse-date "2009W53-7")
                 '(nil nil nil 3 1 2010 nil nil nil))))

(ert-deftest test-iso8601-date-ordinals ()
  (should (equal (iso8601-parse-date "1981-095")
                 '(nil nil nil 5 4 1981 nil nil nil))))

(ert-deftest test-iso8601-time ()
  (should (equal (iso8601-parse-time "13:47:30")
                 '(30 47 13 nil nil nil nil nil nil)))
  (should (equal (iso8601-parse-time "134730")
                 '(30 47 13 nil nil nil nil nil nil)))
  (should (equal (iso8601-parse-time "1347")
                 '(0 47 13 nil nil nil nil nil nil))))

(ert-deftest test-iso8601-combined ()
  (should (equal (iso8601-parse "2008-03-02T13:47:30")
                 '(30 47 13 2 3 2008 nil nil nil)))
  (should (equal (iso8601-parse "2008-03-02T13:47:30Z")
                 '(30 47 13 2 3 2008 nil nil 0)))
  (should (equal (iso8601-parse "2008-03-02T13:47:30+01:00")
                 '(30 47 13 2 3 2008 nil nil 3600)))
  (should (equal (iso8601-parse "2008-03-02T13:47:30-01")
                 '(30 47 13 2 3 2008 nil nil -3600))))

(ert-deftest test-iso8601-duration ()
  (should (equal (iso8601-parse-duration "P3Y6M4DT12H30M5S")
                 '(5 30 12 4 6 3 nil nil nil)))
  (should (equal (iso8601-parse-duration "P1M")
                 '(0 0 0 0 1 0 nil nil nil)))
  (should (equal (iso8601-parse-duration "PT1M")
                 '(0 1 0 0 0 0 nil nil nil)))
  (should (equal (iso8601-parse-duration "P0003-06-04T12:30:05")
                 '(5 30 12 4 6 3 nil nil nil))))

(ert-deftest test-iso8601-invalid ()
  (should-not (iso8601-valid-p " 2008-03-02T13:47:30-01"))
  (should-not (iso8601-valid-p "2008-03-02T13:47:30-01:200"))
  (should-not (iso8601-valid-p "2008-03-02T13:47:30-01 "))
  (should-not (iso8601-valid-p "2008-03-02 T 13:47:30-01 "))
  (should-not (iso8601-valid-p "20008-03-02T13:47:30-01")))

(ert-deftest test-iso8601-intervals ()
  (should (equal
           (iso8601-parse-interval "2007-03-01T13:00:00Z/2008-05-11T15:30:00Z")
           '((0 0 13 1 3 2007 nil nil 0)
             (0 30 15 11 5 2008 nil nil 0)
             ;; Hm...  can't really use decode-time for time differences...
             (0 30 2 14 3 1971 0 nil 0))))
  (should (equal (iso8601-parse-interval "2007-03-01T13:00:00Z/P1Y2M10DT2H30M")
                 '((0 0 13 1 3 2007 nil nil 0)
                   (0 30 15 11 5 2008 nil nil 0)
                   (0 30 2 10 2 1 nil nil nil))))
  (should (equal (iso8601-parse-interval "P1Y2M10DT2H30M/2008-05-11T15:30:00Z")
                 '((0 0 13 1 3 2007 nil nil 0)
                   (0 30 15 11 5 2008 nil nil 0)
                   (0 30 2 10 2 1 nil nil nil)))))

(ert-deftest standard-test-dates ()
  (should (equal (iso8601-parse-date "19850412")
                 '(nil nil nil 12 4 1985 nil nil nil)))
  (should (equal (iso8601-parse-date "1985-04-12")
                 '(nil nil nil 12 4 1985 nil nil nil)))

  (should (equal (iso8601-parse-date "1985102")
                 '(nil nil nil 12 4 1985 nil nil nil)))
  (should (equal (iso8601-parse-date "1985-102")
                 '(nil nil nil 12 4 1985 nil nil nil)))

  (should (equal (iso8601-parse-date "1985W155")
                 '(nil nil nil 12 4 1985 nil nil nil)))
  (should (equal (iso8601-parse-date "1985-W15-5")
                 '(nil nil nil 12 4 1985 nil nil nil)))

  (should (equal (iso8601-parse-date "1985W15")
                 '(nil nil nil 7 4 1985 nil nil nil)))
  (should (equal (iso8601-parse-date "1985-W15")
                 '(nil nil nil 7 4 1985 nil nil nil)))

  (should (equal (iso8601-parse-date "1985-04")
                 '(nil nil nil nil 4 1985 nil nil nil)))

  (should (equal (iso8601-parse-date "1985")
                 '(nil nil nil nil nil 1985 nil nil nil)))

  (should (equal (iso8601-parse-date "+1985-04-12")
                 '(nil nil nil 12 4 1985 nil nil nil)))
  (should (equal (iso8601-parse-date "+19850412")
                 '(nil nil nil 12 4 1985 nil nil nil))))

(ert-deftest standard-test-time-of-day-local-time ()
  (should (equal (iso8601-parse-time "152746")
                 '(46 27 15 nil nil nil nil nil nil)))
  (should (equal (iso8601-parse-time "15:27:46")
                 '(46 27 15 nil nil nil nil nil nil)))

  (should (equal (iso8601-parse-time "1528")
                 '(0 28 15 nil nil nil nil nil nil)))
  (should (equal (iso8601-parse-time "15:28")
                 '(0 28 15 nil nil nil nil nil nil)))

  (should (equal (iso8601-parse-time "15")
                 '(0 0 15 nil nil nil nil nil nil))))

(ert-deftest standard-test-time-of-day-fractions ()
  (should (equal (iso8601-parse-time "152735,5" t)
                 '((355 . 10) 27 15 nil nil nil nil nil nil)))
  (should (equal (iso8601-parse-time "15:27:35,5" t)
                 '((355 . 10) 27 15 nil nil nil nil nil nil)))

  (should (equal (iso8601-parse-time "2320,5" t)
                 '(30 20 23 nil nil nil nil nil nil)))
  (should (equal (iso8601-parse-time "23:20,8" t)
                 '(48 20 23 nil nil nil nil nil nil)))

  (should (equal (iso8601-parse-time "23,3" t)
                 '(0 18 23 nil nil nil nil nil nil))))

(ert-deftest nonstandard-test-time-of-day-decimals ()
  (should (equal (iso8601-parse-time "15:27:35.123" t)
                 '((35123 . 1000) 27 15 nil nil nil nil nil nil)))
  (should (equal (iso8601-parse-time "15:27:35.123456789" t)
                 '((35123456789 . 1000000000) 27 15 nil nil nil nil nil nil))))

(ert-deftest standard-test-time-of-day-beginning-of-day ()
  (should (equal (iso8601-parse-time "000000")
                 '(0 0 0 nil nil nil nil nil nil)))
  (should (equal (iso8601-parse-time "00:00:00")
                 '(0 0 0 nil nil nil nil nil nil)))

  (should (equal (iso8601-parse-time "0000")
                 '(0 0 0 nil nil nil nil nil nil)))
  (should (equal (iso8601-parse-time "00:00")
                 '(0 0 0 nil nil nil nil nil nil))))

(ert-deftest standard-test-time-of-day-utc ()
  (should (equal (iso8601-parse-time "232030Z")
                 '(30 20 23 nil nil nil nil nil 0)))
  (should (equal (iso8601-parse-time "23:20:30Z")
                 '(30 20 23 nil nil nil nil nil 0)))

  (should (equal (iso8601-parse-time "2320Z")
                 '(0 20 23 nil nil nil nil nil 0)))
  (should (equal (iso8601-parse-time "23:20Z")
                 '(0 20 23 nil nil nil nil nil 0)))

  (should (equal (iso8601-parse-time "23Z")
                 '(0 0 23 nil nil nil nil nil 0))))


(ert-deftest standard-test-time-of-day-zone ()
  (should (equal (iso8601-parse-time "152746+0100")
                 '(46 27 15 nil nil nil nil nil 3600)))
  (should (equal (iso8601-parse-time "15:27:46+0100")
                 '(46 27 15 nil nil nil nil nil 3600)))

  (should (equal (iso8601-parse-time "152746+01")
                 '(46 27 15 nil nil nil nil nil 3600)))
  (should (equal (iso8601-parse-time "15:27:46+01")
                 '(46 27 15 nil nil nil nil nil 3600)))

  (should (equal (iso8601-parse-time "152746-0500")
                 '(46 27 15 nil nil nil nil nil -18000)))
  (should (equal (iso8601-parse-time "15:27:46-0500")
                 '(46 27 15 nil nil nil nil nil -18000)))

  (should (equal (iso8601-parse-time "152746-05")
                 '(46 27 15 nil nil nil nil nil -18000)))
  (should (equal (iso8601-parse-time "15:27:46-05")
                 '(46 27 15 nil nil nil nil nil -18000))))

(ert-deftest standard-test-date-and-time-of-day ()
  (should (equal (iso8601-parse "19850412T101530")
                 '(30 15 10 12 4 1985 nil nil nil)))
  (should (equal (iso8601-parse "1985-04-12T10:15:30")
                 '(30 15 10 12 4 1985 nil nil nil)))

  (should (equal (iso8601-parse "1985102T235030Z")
                 '(30 50 23 12 4 1985 nil nil 0)))
  (should (equal (iso8601-parse "1985-102T23:50:30Z")
                 '(30 50 23 12 4 1985 nil nil 0)))

  (should (equal (iso8601-parse "1985W155T235030")
                 '(30 50 23 12 4 1985 nil nil nil)))
  (should (equal (iso8601-parse "1985-W155T23:50:30")
                 '(30 50 23 12 4 1985 nil nil nil))))

(ert-deftest standard-test-interval ()
  ;; A time interval starting at 20 minutes and 50 seconds past 23
  ;; hours on 12 April 1985 and ending at 30 minutes past 10 hours on
  ;; 25 June 1985.
  (should (equal (iso8601-parse-interval "19850412T232050Z/19850625T103000Z")
                 '((50 20 23 12 4 1985 nil nil 0)
                   (0 30 10 25 6 1985 nil nil 0)
                   (10 9 11 15 3 1970 0 nil 0))))
  (should (equal (iso8601-parse-interval
                  "1985-04-12T23:20:50Z/1985-06-25T10:30:00Z")
                 '((50 20 23 12 4 1985 nil nil 0)
                   (0 30 10 25 6 1985 nil nil 0)
                   (10 9 11 15 3 1970 0 nil 0))))

  ;; A time interval starting at 12 April 1985 and ending on 25 June
  ;; 1985.

  ;; This example doesn't seem valid according to the standard.
  ;; "0625" is unambiguous, and means "the year 625".  Weird.
  ;; (should (equal (iso8601-parse-interval "19850412/0625")
  ;;                '((nil nil nil 12 4 1985 nil nil nil)
  ;;                  (nil nil nil nil nil 625 nil nil nil)
  ;;                  (0 17 0 22 9 609 5 nil 0))))

  ;; A time interval of 2 years, 10 months, 15 days, 10 hours, 20
  ;; minutes and 30 seconds.
  (should (equal (iso8601-parse-duration "P2Y10M15DT10H20M30S")
                 '(30 20 10 15 10 2 nil nil nil)))

  (should (equal (iso8601-parse-duration "P00021015T102030")
                 '(30 20 10 15 10 2 nil nil nil)))
  (should (equal (iso8601-parse-duration "P0002-10-15T10:20:30")
                 '(30 20 10 15 10 2 nil nil nil)))

  ;; A time interval of 1 year and 6 months.
  (should (equal (iso8601-parse-duration "P1Y6M")
                 '(0 0 0 0 6 1 nil nil nil)))
  (should (equal (iso8601-parse-duration "P0001-06")
                 '(nil nil nil nil 6 1 nil nil nil)))

  ;; A time interval of seventy-two hours.
  (should (equal (iso8601-parse-duration "PT72H")
                 '(0 0 72 0 0 0 nil nil nil)))

  ;; Defined by start and duration
  ;; A time interval of 1 year, 2 months, 15 days and 12 hours,
  ;; beginning on 12 April 1985 at 20 minutes past 23 hours.
  (should (equal (iso8601-parse-interval "19850412T232000/P1Y2M15DT12H")
                 '((0 20 23 12 4 1985 nil nil nil)
                   (0 20 11 28 6 1986 nil nil nil)
                   (0 0 12 15 2 1 nil nil nil))))
  (should (equal (iso8601-parse-interval "1985-04-12T23:20:00/P1Y2M15DT12H")
                 '((0 20 23 12 4 1985 nil nil nil)
                   (0 20 11 28 6 1986 nil nil nil)
                   (0 0 12 15 2 1 nil nil nil))))

  ;; Defined by duration and end
  ;; A time interval of 1 year, 2 months, 15 days and 12 hours, ending
  ;; on 12 April 1985 at 20 minutes past 23 hour.
  (should (equal (iso8601-parse-interval "P1Y2M15DT12H/19850412T232000")
                 '((0 20 11 28 1 1984 nil nil nil)
                   (0 20 23 12 4 1985 nil nil nil)
                   (0 0 12 15 2 1 nil nil nil)))))

;;; iso8601-tests.el ends here
