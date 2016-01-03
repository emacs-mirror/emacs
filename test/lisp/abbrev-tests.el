;;; abbrev-tests.el --- Test suite for abbrevs  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Eli Zaretskii <eliz@gnu.org>
;; Keywords: abbrevs

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `kill-all-abbrevs-test' will remove all user *and* system abbrevs
;; if called noninteractively with the init file loaded.

;;; Code:

(require 'ert)
(require 'abbrev)
(require 'seq)

;; set up test abbrev table and abbrev entry
(defun setup-test-abbrev-table ()
  (defvar ert-test-abbrevs nil)
  (define-abbrev-table 'ert-test-abbrevs '(("a-e-t" "abbrev-ert-test")))
  (abbrev-table-put ert-test-abbrevs :ert-test "ert-test-value")
  ert-test-abbrevs)

(ert-deftest abbrev-table-p-test ()
  (should-not (abbrev-table-p 42))
  (should-not (abbrev-table-p "aoeu"))
  (should-not (abbrev-table-p '()))
  (should-not (abbrev-table-p []))
  ;; Missing :abbrev-table-modiff counter:
  (should-not (abbrev-table-p (obarray-make)))
  (let* ((table (obarray-make)))
    (abbrev-table-put table :abbrev-table-modiff 42)
    (should (abbrev-table-p table))))

(ert-deftest abbrev-make-abbrev-table-test ()
  ;; Table without properties:
  (let ((table (make-abbrev-table)))
    (should (abbrev-table-p table))
    (should (= (length table) obarray-default-size)))
  ;; Table with one property 'foo with value 'bar:
  (let ((table (make-abbrev-table '(foo bar))))
    (should (abbrev-table-p table))
    (should (= (length table) obarray-default-size))
    (should (eq (abbrev-table-get table 'foo) 'bar))))

(ert-deftest abbrev-table-get-put-test ()
  (let ((table (make-abbrev-table)))
    (should-not (abbrev-table-get table 'foo))
    (should (= (abbrev-table-put table 'foo 42) 42))
    (should (= (abbrev-table-get table 'foo) 42))
    (should (eq (abbrev-table-put table 'foo 'bar) 'bar))
    (should (eq (abbrev-table-get table 'foo) 'bar))))

(ert-deftest copy-abbrev-table-test ()
  (defvar foo-abbrev-table nil)         ; Avoid compiler warning
  (define-abbrev-table 'foo-abbrev-table
    '())
  (should (abbrev-table-p foo-abbrev-table))
  ;; Bug 21828
  (let ((new-foo-abbrev-table
         (condition-case nil
             (copy-abbrev-table foo-abbrev-table)
           (error nil))))
    (should (abbrev-table-p new-foo-abbrev-table)))
  (should-not (string-equal (buffer-name) "*Backtrace*")))

(ert-deftest kill-all-abbrevs-test ()
  "Test undefining all defined abbrevs"
  (unless noninteractive
    (ert-skip "Cannot test kill-all-abbrevs in interactive mode"))

  (let ((num-tables 0))
    ;; ensure at least one abbrev exists
    (should (abbrev-table-p (setup-test-abbrev-table)))
    (setf num-tables (length abbrev-table-name-list))
    (kill-all-abbrevs)

    ;; no tables should have been removed/added
    (should (= num-tables (length abbrev-table-name-list)))
    ;; number of empty tables should be the same as number of tables
    (should (= num-tables (length (seq-filter
                                   (lambda (table)
                                       (abbrev-table-empty-p (symbol-value table)))
                                   abbrev-table-name-list))))))

(ert-deftest abbrev-table-name-test ()
  "Test returning name of abbrev-table"
  (let ((ert-test-abbrevs (setup-test-abbrev-table))
        (no-such-table nil))
    (should (equal 'ert-test-abbrevs (abbrev-table-name ert-test-abbrevs)))
    (should (equal nil (abbrev-table-name no-such-table)))))

(ert-deftest clear-abbrev-table-test ()
  "Test clearing single abbrev table"
  (let ((ert-test-abbrevs (setup-test-abbrev-table)))
    (should (equal "a-e-t" (symbol-name
                            (abbrev-symbol "a-e-t" ert-test-abbrevs))))
    (should (equal "abbrev-ert-test" (symbol-value
                                      (abbrev-symbol "a-e-t" ert-test-abbrevs))))

    (clear-abbrev-table ert-test-abbrevs)

    (should (equal "nil" (symbol-name
                          (abbrev-symbol "a-e-t" ert-test-abbrevs))))
    (should (equal nil (symbol-value
                        (abbrev-symbol "a-e-t" ert-test-abbrevs))))
    (should (equal t (abbrev-table-empty-p ert-test-abbrevs)))))

(provide 'abbrev-tests)
;;; abbrev-tests.el ends here
