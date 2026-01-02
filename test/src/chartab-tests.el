;;; chartab-tests.el --- Tests for char-tab.c -*- lexical-binding: t -*-

;; Copyright (C) 2016-2026 Free Software Foundation, Inc.

;; Author: Eli Zaretskii <eliz@gnu.org>

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

(defun chartab-set-and-test (rng)
  (let ((tbl (make-char-table nil nil))
        (from (car rng))
        (to (cdr rng)))
    (set-char-table-range tbl rng t)
    (should (eq (aref tbl from) t))
    (should (eq (aref tbl to) t))
    (should (eq (aref tbl (/ (+ from to) 2)) t))
    (when (< to (max-char))
      (should-not (eq (aref tbl (1+ to)) t)))
    (when (> from 0)
      (should-not (eq (aref tbl (1- from)) t)))))

(ert-deftest chartab-test-range-setting ()
  (mapc (lambda (elt)
          (chartab-set-and-test elt))
        '((0 . 127)
          (128 . 256)
          (#x1000 . #x1fff)
          (#x1001 . #x2000)
          (#x10000 . #x20000)
          (#x10001 . #x1ffff)
          (#x20000 . #x30000)
          (#xe0e00 . #xe0ef6)
          )))

(ert-deftest chartab-test-char-table-p ()
  (should (char-table-p (make-char-table 'foo)))
  (should (not (char-table-p (make-hash-table)))))

(ert-deftest chartab-test-char-table-subtype ()
  (should (eq (char-table-subtype (make-char-table 'foo)) 'foo)))

(ert-deftest chartab-test-char-table-parent ()
  (should (eq (char-table-parent (make-char-table 'foo)) nil))
  (let ((parent (make-char-table 'foo))
        (child (make-char-table 'bar)))
    (set-char-table-parent child parent)
    (should (eq (char-table-parent child) parent))))

(ert-deftest chartab-test-char-table-extra-slot ()
  ;; Use any type with extra slots, e.g. 'case-table.
  (let ((tbl (make-char-table 'case-table)))
    (set-char-table-extra-slot tbl 1 'bar)
    (should (eq (char-table-extra-slot tbl 1) 'bar))))

(provide 'chartab-tests)
;;; chartab-tests.el ends here
