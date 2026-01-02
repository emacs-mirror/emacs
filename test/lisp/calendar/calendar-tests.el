;;; calendar-tests.el --- tests for calendar/calendar.el  -*- lexical-binding:t -*-

;; Copyright (C) 2024-2026 Free Software Foundation, Inc.

;; Author: Richard Lawrence <rwl@recursewithless.net>

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
(require 'calendar)

(ert-deftest calendar-test-validity-predicate ()
  (should (eq (calendar-date-is-valid-p nil) nil))
  (should (eq (calendar-date-is-valid-p "invalid") nil))
  (should (eq (calendar-date-is-valid-p (list 1 2)) nil))
  (should (eq (calendar-date-is-valid-p (list 5 1 2025)) t)))

(ert-deftest calendar-test-date-in-calendar-mode-line ()
  "Test whether the calendar mode line displays `date' correctly."
  (save-window-excursion
    (unwind-protect
        (let* ((calendar-mode-line-format (list '(calendar-date-string date)))
               (calendar-move-hook '(calendar-update-mode-line))
               (today (calendar-current-date))
               (month (calendar-extract-month today))
               (year (calendar-extract-year today))
               (cursor-date (calendar-gregorian-from-absolute
                             (1+ (calendar-absolute-from-gregorian today)))))
          (calendar)
          (should (equal (string-trim mode-line-format)
                         (calendar-date-string today)))
          (calendar-forward-day 1)
          (should (equal (string-trim mode-line-format)
                         (calendar-date-string cursor-date)))
          (calendar-goto-today)
          (should (equal (string-trim mode-line-format)
                         (calendar-date-string today)))
          (calendar-cursor-to-visible-date cursor-date)
          (calendar-redraw)
          (should (equal (string-trim mode-line-format)
                         (calendar-date-string cursor-date)))
          (calendar-cursor-to-visible-date cursor-date)
          (calendar-scroll-left)
          (calendar-other-month month year)
          (should (equal (string-trim mode-line-format)
                         (calendar-date-string cursor-date))))
      (kill-buffer calendar-buffer))))

(provide 'calendar-tests)
;;; calendar-tests.el ends here
