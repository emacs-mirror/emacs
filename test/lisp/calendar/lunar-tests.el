;;; lunar-tests.el --- tests for calendar/lunar.el  -*- lexical-binding:t -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Author: Stefan Kangas <stefankangas@gmail.com>

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
(require 'lunar)

(defmacro with-lunar-test (&rest body)
  `(let ((calendar-latitude 40.1)
         (calendar-longitude -88.2)
         (calendar-location-name "Urbana, IL")
         (calendar-time-zone -360)
         (calendar-standard-time-zone-name "CST")
         (calendar-time-display-form '(12-hours ":" minutes am-pm)))
     ,@body))

(ert-deftest lunar-test-phase ()
  (with-lunar-test
   (should (equal (lunar-phase 1)
                  '((1 7 1900) "11:40pm" 1 "")))))

(ert-deftest lunar-test-eclipse-check ()
  (with-lunar-test
   (should (equal (eclipse-check 1 1) "**  Eclipse **"))))

;; This fails in certain time zones.
;; Eg TZ=America/Phoenix make lisp/calendar/lunar-tests
;; Similarly with TZ=UTC.
;; Daylight saving related?
(ert-deftest lunar-test-phase-list ()
  :tags '(:unstable)
  (with-lunar-test
   (should (equal  (lunar-phase-list 3 1871)
                   '(((3 20 1871) "11:03pm" 0 "")
                     ((3 29 1871) "1:46am" 1 "**  Eclipse **")
                     ((4 5 1871) "9:20am" 2 "")
                     ((4 12 1871) "12:57am" 3 "**  Eclipse possible **")
                     ((4 19 1871) "2:06pm" 0 "")
                     ((4 27 1871) "6:49pm" 1 "")
                     ((5 4 1871) "5:57pm" 2 "")
                     ((5 11 1871) "9:29am" 3 "")
                     ((5 19 1871) "5:46am" 0 "")
                     ((5 27 1871) "8:02am" 1 ""))))))

(ert-deftest lunar-test-new-moon-time ()
  (with-lunar-test
   (should (= (round (lunar-new-moon-time 1))
              2451580))))

(ert-deftest lunar-test-new-moon-on-or-after ()
  (with-lunar-test
   (should (= (round (lunar-new-moon-on-or-after (calendar-absolute-from-gregorian '(5 5 1818))))
              664525))))

(provide 'lunar-tests)
;;; lunar-tests.el ends here
