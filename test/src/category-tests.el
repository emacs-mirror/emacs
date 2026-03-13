;;; category-tests.el --- Tests for category.c -*- lexical-binding: t -*-

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

(ert-deftest category-tests-category-table ()
  (let ((table (make-category-table)))
    (should (category-table-p table))
    (should (category-table-p (standard-category-table)))
    (should-not (category-table-p (make-char-table 'foo)))))

(ert-deftest category-tests-define-category-docstring ()
  (let ((table (make-category-table)))
    (define-category ?a "Alpha category." table)
    (should (equal (category-docstring ?a table) "Alpha category."))
    (should-error (define-category ?a "Duplicate." table))))

(ert-deftest category-tests-set-category-table ()
  (let ((table (make-category-table)))
    (with-temp-buffer
      (should (eq (set-category-table table) table))
      (should (eq (category-table) table)))))

(ert-deftest category-tests-category-set-mnemonics ()
  (let ((set (make-category-set "aZ")))
    (should (equal (category-set-mnemonics set) "Za")))
  (let ((set (make-category-set "")))
    (should (equal (category-set-mnemonics set) ""))))

(ert-deftest category-tests-char-category-set ()
  (let ((table (make-category-table)))
    (define-category ?a "Alpha category." table)
    (modify-category-entry ?x ?a table)
    (with-temp-buffer
      (set-category-table table)
      (let ((mnemonics (category-set-mnemonics (char-category-set ?x))))
        (should (string-match-p "a" mnemonics))))))

(ert-deftest category-tests-copy-category-table ()
  (let ((table (make-category-table)))
    (define-category ?a "Alpha category." table)
    (modify-category-entry ?x ?a table)
    (let ((copy (copy-category-table table)))
      (modify-category-entry ?x ?a table t)
      (with-temp-buffer
        (set-category-table copy)
        (should (equal (category-set-mnemonics (char-category-set ?x)) "a")))
      (with-temp-buffer
        (set-category-table table)
        (should (equal (category-set-mnemonics (char-category-set ?x)) ""))))))

(provide 'category-tests)
;;; category-tests.el ends here
