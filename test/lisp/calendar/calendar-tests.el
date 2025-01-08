;;; calendar-tests.el --- tests for calendar/calendar.el  -*- lexical-binding:t -*-

;; Copyright (C) 2024-2025 Free Software Foundation, Inc.

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

(provide 'calendar-tests)
;;; calendar-tests.el ends here
