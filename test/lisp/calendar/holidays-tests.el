;;; holidays-tests.el --- tests for calendar/holidays.el  -*- lexical-binding:t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

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
(require 'holidays)

(defvar displayed-month)
(defvar displayed-year)

(ert-deftest holidays-test-holiday-easter-etc ()
  "Test `holiday-easter-etc'."
  (let ((displayed-year 2026)
        (displayed-month 12))
    (should (equal (holiday-easter-etc -63 "Septuagesima Sunday")
                   '(((1 24 2027) "Septuagesima Sunday"))))
    (should (equal (holiday-easter-etc -56 "Sexagesima Sunday")
                   '(((1 31 2027) "Sexagesima Sunday"))))))

(defun holidays-test--get-holidays (mon yr years &optional months)
  "Return holidays starting from YR, MON in a span of YEARS."
  (or months (setq months 3))
  (let ((displayed-year yr)
        (displayed-month mon)
        (calendar-total-months months)
        (inhibit-message t)
        res)
    (save-window-excursion
      (dotimes (_ (/ (* years 12) calendar-total-months))
        (calendar-list-holidays)
        (with-current-buffer holiday-buffer
          (setq res (append res (string-split (buffer-string) "\n"))))
        (calendar-increment-month displayed-month displayed-year months)))
    (kill-buffer holiday-buffer)
    (string-join res "\n")))

(ert-deftest holidays-test-more-months ()
  "Test if holidays are same with more displayed months."
  (let* ((today (calendar-current-date))
         (yr (calendar-extract-year today))
         (mon (calendar-extract-month today))
         (years 1)
         (holiday-list (holidays-test--get-holidays mon yr years 3)))
    (should (equal holiday-list
                   (holidays-test--get-holidays mon yr years 12)))))

(provide 'holidays-tests)
;;; holidays-tests.el ends here
