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

;;; Code:

(require 'ert)
(require 'abbrev)

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

(provide 'abbrev-tests)
;;; abbrev-tests.el ends here
