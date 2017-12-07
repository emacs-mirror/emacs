;;; benchmark-tests.el --- Test suite for benchmark.  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

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

(require 'benchmark)
(require 'ert)

(ert-deftest benchmark-tests ()
  (let (str t-long t-short)
    (should (consp (benchmark-run nil (1+ 0))))
    (should (consp (benchmark-run 1 (1+ 0))))
    (should (stringp (benchmark nil (1+ 0))))
    (should (stringp (benchmark 1 (1+ 0))))
    (should (consp (benchmark-run-compiled nil (1+ 0))))
    (should (consp (benchmark-run-compiled 1 (1+ 0))))
    ;; First test is heavier, must need longer time.
    (should (> (car (benchmark-run nil
                      (let ((n 100000)) (while (> n 1) (setq n (1- n))))))
               (car (benchmark-run nil (1+ 0)))))
    (should (> (car (benchmark-run nil
                      (let ((n 100000)) (while (> n 1) (setq n (1- n))))))
               (car (benchmark-run nil (1+ 0)))))
    (should (> (car (benchmark-run-compiled nil
                      (let ((n 100000)) (while (> n 1) (setq n (1- n))))))
               (car (benchmark-run-compiled nil (1+ 0)))))
    (setq str (benchmark nil '(let ((n 100000)) (while (> n 1) (setq n (1- n))))))
    (string-match "Elapsed time: \\([0-9.]+\\)" str)
    (setq t-long (string-to-number (match-string 1 str)))
    (setq str (benchmark nil '(1+ 0)))
    (string-match "Elapsed time: \\([0-9.]+\\)" str)
    (setq t-short (string-to-number (match-string 1 str)))
    (should (> t-long t-short))))

;;; benchmark-tests.el ends here.
