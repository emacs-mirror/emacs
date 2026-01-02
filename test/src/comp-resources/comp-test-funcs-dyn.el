;;; comp-test-funcs-dyn.el --- compilation unit tested by comp-tests.el -*- lexical-binding: nil; -*-

;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

;; Author: Andrea Corallo <acorallo@gnu.org>

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

(require 'cl-lib)

(defun comp-tests-ffuncall-callee-dyn-f (a b)
  (list a b))

(defun comp-tests-ffuncall-callee-opt-dyn-f (a b &optional c d)
  (list a b c d))

(defun comp-tests-ffuncall-callee-rest-dyn-f (a b &rest c)
  (list a b c))

(defun comp-tests-ffuncall-callee-opt-rest-dyn-f (a b &optional c &rest d)
  (list a b c d))

(defun comp-tests-cl-macro-exp-f ()
  (cl-loop for xxx in '(a b)
	   for yyy = xxx
	   collect xxx))

(cl-defun comp-tests-cl-uninterned-arg-parse-f (a &optional b &aux)
  (list a b))

(provide 'comp-test-dyn-funcs)

;;; comp-test-funcs-dyn.el ends here
