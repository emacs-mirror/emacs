;;; lunar-tests.el --- tests for calendar/lunar.el  -*- lexical-binding:t -*-

;; Copyright (C) 2020-2024 Free Software Foundation, Inc.

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
   (should (equal (eclipse-check 10.0 1) ""))
   (should (equal (eclipse-check 10.0 2) "** Lunar Eclipse **"))))

(ert-deftest lunar-test-phase-list ()
  (with-lunar-test
   (should (equal (lunar-phase-list 9 2023)
                  '(((9 6 2023) "22:27" 3 "")
                    ((9 15 2023) "01:40" 0 "")
                    ((9 22 2023) "19:33" 1 "")
                    ((9 29 2023) "09:54" 2 "** Lunar Eclipse possible **")
                    ((10 6 2023) "13:53" 3 "")
                    ((10 14 2023) "17:55" 0 "** Solar Eclipse **")
                    ((10 22 2023) "03:30" 1 "")
                    ((10 28 2023) "20:20" 2 "** Lunar Eclipse **")
                    ((11 5 2023) "08:42" 3 "")
                    ((11 13 2023) "09:27" 0 "")
                    ((11 20 2023) "10:51" 1 "")
                    ((11 27 2023) "09:13" 2 ""))))))

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
