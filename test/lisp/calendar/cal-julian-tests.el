;;; cal-julian-tests.el --- tests for calendar/cal-julian.el  -*- lexical-binding:t -*-

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
(require 'cal-julian)

(ert-deftest cal-julian-test-to-absolute ()
  (should (equal (calendar-gregorian-from-absolute
                  (calendar-julian-to-absolute
                   '(10 25 1917)))
                 '(11 7 1917))))

(ert-deftest cal-julian-test-from-absolute ()
  (should (equal (calendar-julian-from-absolute
                  (calendar-absolute-from-gregorian
                   '(11 7 1917)))
                 '(10 25 1917))))

(ert-deftest cal-julian-test-date-string ()
  (should (equal (let ((calendar-date-display-form calendar-iso-date-display-form))
                   (calendar-julian-date-string '(11 7 1917)))
                 "1917-10-25")))

(defmacro with-cal-julian-test (&rest body)
  `(save-window-excursion
     (unwind-protect
         (progn
           (calendar)
           ,@body)
       (kill-buffer calendar-buffer))))

(ert-deftest cal-julian-test-goto-date ()
  (with-cal-julian-test
   (calendar-julian-goto-date '(10 25 1917))
   (should (looking-at "7"))))

(ert-deftest cal-julian-test-astro-to-and-from-absolute ()
  (should (= (+ (calendar-astro-to-absolute 0.0)
                (calendar-astro-from-absolute 0.0))
             0.0)))

(ert-deftest cal-julian-calendar-astro-date-string ()
  (should (equal (calendar-astro-date-string '(10 25 1917)) "2421527")))

(ert-deftest calendar-astro-goto-day-number ()
  (with-cal-julian-test
   (calendar-astro-goto-day-number 2421527)
   (backward-char)
   (should (looking-at "25"))))

(provide 'cal-julian-tests)
;;; cal-julian-tests.el ends here
