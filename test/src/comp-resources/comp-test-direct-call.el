;;; comp-test-direct-call.el --- compilation unit tested by comp-tests.el -*- lexical-binding: t; -*-

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

;;; Commentary:

;; Test that anonymous lambdas in a compilation unit don't prevent
;; direct calls between named functions at speed 3.
;; See `comp--func-unique-in-cu-p'.

;;; Code:

(defun comp-tests-direct-call-callee-f (x)
  (1+ x))

(defun comp-tests-direct-call-caller-f (x)
  (comp-tests-direct-call-callee-f x))

;; Two anonymous lambdas -- these must not prevent direct calls
;; between the named functions above.
(defvar comp-tests-direct-call-list1
  (mapcar (lambda (x) (1+ x)) '(1 2 3)))

(defvar comp-tests-direct-call-list2
  (mapcar (lambda (x) (1- x)) '(1 2 3)))

;;; comp-test-direct-call.el ends here
