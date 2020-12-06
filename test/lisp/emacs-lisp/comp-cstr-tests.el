;;; comp-cstr-tests.el --- unit tests for src/comp.c -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

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

;; Unit tests for lisp/emacs-lisp/comp-cstr.el

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'comp-cstr)

(defun comp-cstr-test-ts (type-spec)
  "Create a constraint from TYPE-SPEC and convert it back to type specifier."
  (let ((comp-ctxt (make-comp-cstr-ctxt)))
    (comp-cstr-to-type-spec (comp-type-spec-to-cstr type-spec))))

(defun comp-cstr-typespec-test (number type-spec expected-type-spec)
  `(ert-deftest ,(intern (concat "comp-cstr-test-" (int-to-string number))) ()
     (should (equal (comp-cstr-test-ts ',type-spec)
                    ',expected-type-spec))))

(defconst comp-cstr-typespec-tests-alist
  `((symbol . symbol)
    ((or string array) . array)
    ((or symbol number) . (or symbol number))
    ((or cons atom) . (or cons atom)) ;; SBCL return T
    ((or integer number) . number)
    ((or (or integer symbol) number) . (or symbol number))
    ((or (or integer symbol) (or number list)) . (or list symbol number))
    ((or (or integer number) nil) . number)
    ((member foo) . (member foo))
    ((member foo bar) . (member foo bar))
    ((or (member foo) (member bar)) . (member foo bar))
    ((or (member foo) symbol) . symbol) ;; SBCL return (OR SYMBOL (MEMBER FOO))
    ((or (member foo) number) .  (or (member foo) number))
    ((or (integer 1 3) number) . number)
    (integer . integer)
    ((integer 1 2) . (integer 1 2))
    ((or (integer -1  0) (integer 3  4)) . (or (integer -1  0) (integer 3  4)))
    ((or (integer -1  2) (integer 3  4)) . (integer -1 4))
    ((or (integer -1  3) (integer 3  4)) . (integer -1 4))
    ((or (integer -1  4) (integer 3  4)) . (integer -1 4))
    ((or (integer -1  5) (integer 3  4)) . (integer -1 5))
    ((or (integer -1  *) (integer 3  4)) . (integer -1 *))
    ((or (integer -1  2) (integer *  4)) . (integer * 4))
    ((and string array) . string)
    ((and cons atom) . nil)
    ((and (member foo) (member foo bar baz)) . (member foo))
    ((and (member foo) (member bar)) . nil)
    ((and (member foo) symbol) . (member foo))
    ((and (member foo) string) . nil)
    ((and (member foo) (integer 1 2)) . nil)
    ((and (member 1 2) (member 3 2)) . (member 2))
    ((and number (integer 1 2)) . number)
    ((and integer (integer 1 2)) . integer)
    ((and (integer -1 0) (integer 3 5)) . nil)
    ((and (integer -1 2) (integer 3 5)) . nil)
    ((and (integer -1 3) (integer 3 5)) . (integer 3 3))
    ((and (integer -1 4) (integer 3 5)) . (integer 3 4))
    ((and (integer -1 5) nil) . nil)
    ((not symbol) . (not symbol))
    ((or (member foo) (not (member foo bar))) . (not (member bar)))
    ((or (member foo bar) (not (member foo))) . t)
    ;; Intentionally conservative, see `comp-cstr-union-1-no-mem'.
    ((or symbol (not sequence)) . t)
    ((or symbol (not symbol)) . t)
    ;; Conservative.
    ((or symbol (not sequence)) . t)
    ((or vector (not sequence)) . (not sequence))
    ((or (integer 1 10) (not (integer * 5))) . (integer 1 *))
    ((or symbol (integer 1 10) (not (integer * 5))) . (or symbol (integer 1 *)))
    ((or (not symbol) (integer 1 10) (not (integer * 5))) . (not (or symbol (integer * 0))))
    ((or symbol (not (member foo))) . (not (member foo)))
    ((or (not symbol) (not (member foo))) . (not symbol))
    ;; Conservative.
    ((or (not (member foo)) string) . (not (member foo)))
    ;; Conservative.
    ((or (member foo) (not string)) . (not string))
    ((or (not (integer 1 2)) integer) . integer)
    ((or (not (integer 1 2)) (not integer)) . (not integer))
    ((or (integer 1 2) (not integer)) . (not (or integer (integer * 0) (integer 3 *))))
    ((or number (not (integer 1 2))) . t)
    ((or atom (not (integer 1 2))) . t)
    ((or atom (not (member foo))) . t))
  "Alist type specifier -> expected type specifier.")

(defmacro comp-cstr-synthesize-tests ()
  "Generate all tests from `comp-cstr-typespec-tests-alist'."
  `(progn
     ,@(cl-loop
        for i from 0
        for (ts . exp-ts) in comp-cstr-typespec-tests-alist
        append (list (comp-cstr-typespec-test i ts exp-ts)))))

(comp-cstr-synthesize-tests)

;;; comp-cstr-tests.el ends here
