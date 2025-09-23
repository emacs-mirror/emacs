;;; igc-tests.el --- tests for src/igc.c  -*- lexical-binding: t -*-

;; Copyright (C) 2024-2025 Free Software Foundation, Inc.

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

(declare-function igc--set-commit-limit "igc.c")
(declare-function igc--set-pause-time "igc.c")
(declare-function igc-info "igc.c")

(ert-deftest set-commit-limit-test ()
  :tags '(:igc)
  (should (equal (igc--set-commit-limit (ash 1 30)) nil))
  (should (equal (assoc-string "commit-limit" (igc-info))
                 '("commit-limit" 1 1073741824 0)))
  (should-error (igc--set-commit-limit -1)
                :type 'args-out-of-range)
  (should-error (igc--set-commit-limit (- (ash 1 64) 1))
                :type 'args-out-of-range)
  (should (equal (igc--set-commit-limit nil) nil))
  (should (equal (assoc-string "commit-limit" (igc-info))
                 '("commit-limit" 1 -1 0))))

(ert-deftest set-pause-time-test ()
  :tags '(:igc)
  (should (equal (igc--set-pause-time 0.5) nil))
  (should (equal (assoc-string "pause-time" (igc-info))
                 '("pause-time" nil 0.5 nil)))
  (should-error (igc--set-pause-time -1) :type 'range-error)
  (should (equal (igc--set-pause-time 1.0e+INF) nil))
  (should (equal (assoc-string "pause-time" (igc-info))
                 '("pause-time" nil 1.0e+INF nil)))
  (should (equal (igc--set-pause-time 0.01) nil)))

;;; igc-tests.el ends here.
