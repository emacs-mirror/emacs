;;; profiler-tests.el --- tests for src/profiler.c  -*- lexical-binding:t -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

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

(ert-deftest profiler-tests-memory-profiler ()
  (let ((was-running (profiler-memory-running-p)))
    ;; do this first in case the profiler was already running
    (should (eq was-running (profiler-memory-stop)))
    (profiler-memory-start)
    (should-error (profiler-memory-start))
    (should (profiler-memory-running-p))
    (should (hash-table-p (profiler-memory-log)))
    ;; `profiler-memory-log' shouldn't terminate profiling.
    (should (profiler-memory-running-p))
    (profiler-memory-stop)
    (profiler-memory-log)               ;flush the log
    (should-not (profiler-memory-log))
    (when was-running (profiler-memory-start))))

(defconst profiler-tests-cpu-sampling-interval 1000000)

(ert-deftest profiler-tests-cpu-profiler ()
  (skip-unless (fboundp 'profiler-cpu-start))
  (let ((was-running (profiler-cpu-running-p)))
    (should (eq was-running (profiler-cpu-stop)))
    (profiler-cpu-start profiler-tests-cpu-sampling-interval)
    (should-error (profiler-cpu-start profiler-tests-cpu-sampling-interval))
    (should (hash-table-p (profiler-cpu-log)))
    (should (profiler-cpu-running-p))
    (profiler-cpu-stop)
    (profiler-cpu-log)
    (should-not (profiler-cpu-log))
    (when was-running
      (profiler-cpu-start (or (bound-and-true-p profiler-sampling-interval)
                              profiler-tests-cpu-sampling-interval)))))

;;; profiler-tests.el ends here
