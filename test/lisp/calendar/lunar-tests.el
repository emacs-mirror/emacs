;;; lunar-tests.el --- tests for calendar/lunar.el  -*- lexical-binding:t -*-

;; Copyright (C) 2020-2022 Free Software Foundation, Inc.

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
         (calendar-location-name "Paris")
         (calendar-time-zone 0)
         (calendar-standard-time-zone-name "UTC")
         ;; Make sure daylight saving is disabled to avoid interference
         ;; from the system settings (see bug#45818).
         (calendar-daylight-savings-starts nil)
         (calendar-time-display-form '(24-hours ":" minutes)))
     ,@body))

(ert-deftest lunar-test-phase ()
  (with-lunar-test
   (should (equal (lunar-phase 1)
                  '((1 8 1900) "05:40" 1 "")))))

(ert-deftest lunar-test-eclipse-check ()
  (with-lunar-test
   (should (equal (eclipse-check 1 1) "**  Eclipse **"))))

(ert-deftest lunar-test-phase-list ()
  (with-lunar-test
   (should (equal  (lunar-phase-list 3 1871)
                   '(((3 21 1871) "04:03" 0 "")
                     ((3 29 1871) "06:46" 1 "**  Eclipse **")
                     ((4 5 1871) "14:20" 2 "")
                     ((4 12 1871) "05:57" 3 "**  Eclipse possible **")
                     ((4 19 1871) "19:06" 0 "")
                     ((4 27 1871) "23:49" 1 "")
                     ((5 4 1871) "22:57" 2 "")
                     ((5 11 1871) "14:29" 3 "")
                     ((5 19 1871) "10:46" 0 "")
                     ((5 27 1871) "13:02" 1 ""))))))

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
