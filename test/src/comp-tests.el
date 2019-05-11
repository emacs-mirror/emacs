;;; comp-tests.el --- unit tests for src/comp.c      -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Free Software Foundation, Inc.

;; Author: Andrea Corallo <akrl@sdf.org>

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

;; Unit tests for src/comp.c.

;;; Code:

(require 'ert)

(setq garbage-collection-messages t)

(defvar comp-tests-var1 3)

(ert-deftest  comp-tests-varref ()
  "Testing cons car cdr."
  (defun comp-tests-varref-f ()
    comp-tests-var1)

  (byte-compile #'comp-tests-varref-f)
  (native-compile #'comp-tests-varref-f)

  (should (= (comp-tests-varref-f) 3)))

(ert-deftest  comp-tests-list ()
  "Testing cons car cdr."
  (defun comp-tests-list-f ()
    (list 1 2 3))

  (byte-compile #'comp-tests-list-f)
  (native-compile #'comp-tests-list-f)

  (should (equal (comp-tests-list-f) '(1 2 3))))

(ert-deftest  comp-tests-cons-car-cdr ()
  "Testing cons car cdr."
  (defun comp-tests-cons-car-f ()
    (car (cons 1 2)))
  (byte-compile #'comp-tests-cons-car-f)
  (native-compile #'comp-tests-cons-car-f)

  (defun comp-tests-cons-cdr-f (x)
    (cdr (cons 'foo x)))
  (byte-compile #'comp-tests-cons-cdr-f)
  (native-compile #'comp-tests-cons-cdr-f)

  (should (= (comp-tests-cons-car-f) 1))
  (should (= (comp-tests-cons-cdr-f 3) 3)))

(ert-deftest  comp-tests-varset ()
  "Testing varset."
  (defun comp-tests-varset-f ()
      (setq comp-tests-var1 55))
  (byte-compile #'comp-tests-varset-f)
  (native-compile #'comp-tests-varset-f)
  (comp-tests-varset-f)

  (should (= comp-tests-var1 55)))

(ert-deftest comp-tests-gc ()
  "Try to do some longer computation to let the gc kick in."
  (dotimes (_ 100000)
    (comp-tests-cons-cdr-f 3))

  (should (= (comp-tests-cons-cdr-f 3) 3)))

;;; comp-tests.el ends here
