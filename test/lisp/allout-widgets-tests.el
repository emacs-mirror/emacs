;;; allout-widgets-tests.el --- Tests for allout-widgets.el  -*- lexical-binding: t -*-

;; Copyright (C) 2020-2024 Free Software Foundation, Inc.

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
(require 'allout-widgets)

(require 'cl-lib)

(ert-deftest allout-test-range-overlaps ()
  "`allout-range-overlaps' unit tests."
  (let* (ranges
         got
         (try (lambda (from to)
                (setq got (allout-range-overlaps from to ranges))
                (setq ranges (cadr got))
                got)))
;;     ;; biggie:
;;     (setq ranges nil)
;;     ;; ~ .02 to .1 seconds for just repeated listing args instead of funcall
;;     ;; ~ 13 seconds for doing repeated funcall
;;     (message "time-trial: %s, resulting size %s"
;;              (time-trial
;;               '(let ((size 10000)
;;                      doing)
;;                  (dotimes (count size)
;;                    (setq doing (random size))
;;                    (funcall try doing (+ doing (random 5)))
;;                    ;;(list doing (+ doing (random 5)))
;;                    )))
;;              (length ranges))
;;     (sit-for 2)

    ;; fresh:
    (setq ranges nil)
    (should (equal (funcall try 3 5) '(nil ((3 5)))))
    ;; add range at end:
    (should (equal (funcall try 10 12) '(nil ((3 5) (10 12)))))
    ;; add range at beginning:
    (should (equal (funcall try 1 2) '(nil ((1 2) (3 5) (10 12)))))
    ;; insert range somewhere in the middle:
    (should (equal (funcall try 7 9) '(nil ((1 2) (3 5) (7 9) (10 12)))))
    ;; consolidate some:
    (should (equal (funcall try 5 8) '(t ((1 2) (3 9) (10 12)))))
    ;; add more:
    (should (equal (funcall try 15 17) '(nil ((1 2) (3 9) (10 12) (15 17)))))
    ;; add more:
    (should (equal (funcall try 20 22)
                   '(nil ((1 2) (3 9) (10 12) (15 17) (20 22)))))
    ;; encompass more:
    (should (equal (funcall try 4 11) '(t ((1 2) (3 12) (15 17) (20 22)))))
    ;; encompass all:
    (should (equal (funcall try 2 25) '(t ((1 25)))))

    ;; fresh slate:
    (setq ranges nil)
    (should (equal (funcall try 20 25) '(nil ((20 25)))))
    (should (equal (funcall try 30 35) '(nil ((20 25) (30 35)))))
    (should (equal (funcall try 26 28) '(nil ((20 25) (26 28) (30 35)))))
    (should (equal (funcall try 15 20) '(t ((15 25) (26 28) (30 35)))))
    (should (equal (funcall try 10 30) '(t ((10 35)))))
    (should (equal (funcall try 5 6) '(nil ((5 6) (10 35)))))
    (should (equal (funcall try 2 100) '(t ((2 100)))))

    (setq ranges nil)))

(provide 'allout-widgets-tests)
;;; allout-widgets-tests.el ends here
