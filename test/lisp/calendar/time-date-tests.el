;;; time-date-tests.el --- tests for calendar/time-date.el    -*- lexical-binding:t -*-

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
(require 'time-date)

(ert-deftest test-leap-year ()
  (should-not (date-leap-year-p 1999))
  (should-not (date-leap-year-p 1900))
  (should (date-leap-year-p 2000))
  (should (date-leap-year-p 2004)))

(ert-deftest test-days-in-month ()
  (should (= (date-days-in-month 2004 2) 29))
  (should (= (date-days-in-month 2004 3) 31))
  (should-not (= (date-days-in-month 1900 3) 28)))

(ert-deftest test-ordinal ()
  (should (equal (date-ordinal-to-time 2008 271)
                 '(nil nil nil 27 9 2008 nil nil nil)))
  (should (equal (date-ordinal-to-time 2008 1)
                 '(nil nil nil 1 1 2008 nil nil nil)))
  (should (equal (date-ordinal-to-time 2008 32)
                 '(nil nil nil 1 2 2008 nil nil nil)))
  (should (equal (date-ordinal-to-time 1981 095)
                 '(nil nil nil 5 4 1981 nil nil nil))))

(cl-defmethod mdec (&key second minute hour
                         day month year
                         dst zone)
  (list second minute hour day month year nil dst zone))

(ert-deftest test-decoded-add ()
  (let ((time '(12 15 16 8 7 2019 1 t 7200)))
    (should (equal (decoded-time-add time (mdec :year 1))
                   '(12 15 16 8 7 2020 1 t 7200)))

    (should (equal (decoded-time-add time (mdec :year -2))
                   '(12 15 16 8 7 2017 1 t 7200)))

    (should (equal (decoded-time-add time (mdec :month 1))
                   '(12 15 16 8 8 2019 1 t 7200)))

    (should (equal (decoded-time-add time (mdec :month 10))
                   '(12 15 16 8 5 2020 1 t 7200)))

    (should (equal (decoded-time-add time (mdec :day 1))
                   '(12 15 16 9 7 2019 1 t 7200)))

    (should (equal (decoded-time-add time (mdec :day -1))
                   '(12 15 16 7 7 2019 1 t 7200)))

    (should (equal (decoded-time-add time (mdec :day 30))
                   '(12 15 16 7 8 2019 1 t 7200)))

    (should (equal (decoded-time-add time (mdec :day -365))
                   '(12 15 16 8 7 2018 1 t 7200)))

    (should (equal (decoded-time-add time (mdec :day 365))
                   '(12 15 16 7 7 2020 1 t 7200)))

    ;; 2020 is a leap year.
    (should (equal (decoded-time-add time (mdec :day 366))
                   '(12 15 16 8 7 2020 1 t 7200)))

    (should (equal (decoded-time-add time (mdec :second 1))
                   '(13 15 16 8 7 2019 1 t 7200)))

    (should (equal (decoded-time-add time (mdec :second -1))
                   '(11 15 16 8 7 2019 1 t 7200)))

    (should (equal (decoded-time-add time (mdec :second 61))
                   '(13 16 16 8 7 2019 1 t 7200)))

    (should (equal (decoded-time-add time (mdec :hour 1 :minute 2 :second 3))
                   '(15 17 17 8 7 2019 1 t 7200)))

    (should (equal (decoded-time-add time (mdec :hour 24))
                   '(12 15 16 9 7 2019 1 t 7200)))
    ))

(ert-deftest test-decoded-add-zone ()
  (let ((time '(12 15 16 8 7 2019 1 t 7200)))
    (should (equal (decoded-time-add time (mdec :zone -3600))
                   '(12 15 15 8 7 2019 1 t 7200)))
    (should (equal (decoded-time-add time (mdec :zone -7200))
                   '(12 15 14 8 7 2019 1 t 7200)))))

(ert-deftest test-time-since ()
  (should (time-equal-p 0 (time-since nil))))

;;; time-date-tests.el ends here
