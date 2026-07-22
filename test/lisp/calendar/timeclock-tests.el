;;; timeclock-tests.el --- Test suite for timeclock.el  -*- lexical-binding:t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Petteri Hintsanen <petterih@iki.fi>

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

;;; Commentary:

;;; Code:

(require 'ert)
(require 'timeclock)

(ert-deftest test-timeclock-time-format ()
  (setopt timeclock-use-24hr-format nil)
  (should (equal (timeclock-time-format) "%-I:%M %p"))
  (should (equal (timeclock-time-format t) "%-I:%M:%S %p"))
  (setopt timeclock-use-24hr-format t)
  (should (equal (timeclock-time-format) "%-H:%M"))
  (should (equal (timeclock-time-format t) "%-H:%M:%S")))

(provide 'timeclock-tests)
