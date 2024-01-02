;;; comp-test-pure.el --- compilation unit tested by comp-tests.el -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

(defun comp-tests-pure-callee-f (x)
  (1+ x))

(defun comp-tests-pure-caller-f ()
  (comp-tests-pure-callee-f 3))

(defun comp-tests-pure-fibn-f (a b count)
  (if (= count 0)
      b
    (comp-tests-pure-fibn-f (+ a b) a (- count 1))))

(defun comp-tests-pure-fibn-entry-f ()
  (comp-tests-pure-fibn-f 1 0 20))

;;; comp-test-pure.el ends here
