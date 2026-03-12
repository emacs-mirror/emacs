;;; comp-test-direct-call-dup.el --- test direct calls with duplicate names -*- lexical-binding: t; -*-

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

;; Test that a duplicated function name only suppresses direct calls
;; to that name, not to other unique functions in the same CU.
;; See `comp--func-unique-in-cu-p'.

;;; Code:

;; Duplicate name -- calls to this should NOT be direct.
(defun comp-tests-dup-f (x) (1+ x))
(defun comp-tests-dup-f (x) (+ x 2))

;; Unique callee.
(defun comp-tests-unique-f (x) (* x 2))

;; Call to unique callee -- should be a direct call.
(defun comp-tests-calls-unique-f (x) (comp-tests-unique-f x))

;; Call to ambiguous callee -- should NOT be a direct call.
(defun comp-tests-calls-dup-f (x) (comp-tests-dup-f x))

;;; comp-test-direct-call-dup.el ends here
