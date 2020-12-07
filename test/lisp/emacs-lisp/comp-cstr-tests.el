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
  `(;; 1
    (symbol . symbol)
    ;; 2
    ((or string array) . array)
    ;; 3
    ((or symbol number) . (or symbol number))
    ;; 4
    ((or cons atom) . (or cons atom)) ;; SBCL return T
    ;; 5
    ((or integer number) . number)
    ;; 6
    ((or (or integer symbol) number) . (or symbol number))
    ;; 7
    ((or (or integer symbol) (or number list)) . (or list symbol number))
    ;; 8
    ((or (or integer number) nil) . number)
    ;; 9
    ((member foo) . (member foo))
    ;; 10
    ((member foo bar) . (member foo bar))
    ;; 11
    ((or (member foo) (member bar)) . (member foo bar))
    ;; 12
    ((or (member foo) symbol) . symbol) ;; SBCL return (OR SYMBOL (MEMBER FOO))
    ;; 13
    ((or (member foo) number) .  (or (member foo) number))
    ;; 14
    ((or (integer 1 3) number) . number)
    ;; 15
    (integer . integer)
    ;; 16
    ((integer 1 2) . (integer 1 2))
    ;; 17
    ((or (integer -1  0) (integer 3  4)) . (or (integer -1  0) (integer 3  4)))
    ;; 18
    ((or (integer -1  2) (integer 3  4)) . (integer -1 4))
    ;; 19
    ((or (integer -1  3) (integer 3  4)) . (integer -1 4))
    ;; 20
    ((or (integer -1  4) (integer 3  4)) . (integer -1 4))
    ;; 21
    ((or (integer -1  5) (integer 3  4)) . (integer -1 5))
    ;; 22
    ((or (integer -1  *) (integer 3  4)) . (integer -1 *))
    ;; 23
    ((or (integer -1  2) (integer *  4)) . (integer * 4))
    ;; 24
    ((and string array) . string)
    ;; 25
    ((and cons atom) . nil)
    ;; 26
    ((and (member foo) (member foo bar baz)) . (member foo))
    ;; 27
    ((and (member foo) (member bar)) . nil)
    ;; 28
    ((and (member foo) symbol) . (member foo))
    ;; 29
    ((and (member foo) string) . nil)
    ;; 30
    ((and (member foo) (integer 1 2)) . nil)
    ;; 31
    ((and (member 1 2) (member 3 2)) . (member 2))
    ;; 32
    ((and number (integer 1 2)) . number)
    ;; 33
    ((and integer (integer 1 2)) . integer)
    ;; 34
    ((and (integer -1 0) (integer 3 5)) . nil)
    ;; 35
    ((and (integer -1 2) (integer 3 5)) . nil)
    ;; 36
    ((and (integer -1 3) (integer 3 5)) . (integer 3 3))
    ;; 37
    ((and (integer -1 4) (integer 3 5)) . (integer 3 4))
    ;; 38
    ((and (integer -1 5) nil) . nil)
    ;; 39
    ((not symbol) . (not symbol))
    ;; 40
    ((or (member foo) (not (member foo bar))) . (not (member bar)))
    ;; 41
    ((or (member foo bar) (not (member foo))) . t)
    ;; 42 Intentionally conservative, see `comp-cstr-union-1-no-mem'.
    ((or symbol (not sequence)) . t)
    ;; 43
    ((or symbol (not symbol)) . t)
    ;; 44 Conservative.
    ((or symbol (not sequence)) . t)
    ;; 45
    ((or vector (not sequence)) . (not sequence))
    ;; 46
    ((or (integer 1 10) (not (integer * 5))) . (integer 1 *))
    ;; 47
    ((or symbol (integer 1 10) (not (integer * 5))) . (or symbol (integer 1 *)))
    ;; 48
    ((or (not symbol) (integer 1 10) (not (integer * 5))) . (not (or symbol (integer * 0))))
    ;; 49
    ((or symbol (not (member foo))) . (not (member foo)))
    ;; 50
    ((or (not symbol) (not (member foo))) . (not symbol))
    ;; 51 Conservative.
    ((or (not (member foo)) string) . (not (member foo)))
    ;; 52 Conservative.
    ((or (member foo) (not string)) . (not string))
    ;; 53
    ((or (not (integer 1 2)) integer) . integer)
    ;; 54
    ((or (not (integer 1 2)) (not integer)) . (not integer))
    ;; 55
    ((or (integer 1 2) (not integer)) . (not (or integer (integer * 0) (integer 3 *))))
    ;; 56
    ((or number (not (integer 1 2))) . t)
    ;; 57
    ((or atom (not (integer 1 2))) . t)
    ;; 58
    ((or atom (not (member foo))) . t))
  "Alist type specifier -> expected type specifier.")

(defmacro comp-cstr-synthesize-tests ()
  "Generate all tests from `comp-cstr-typespec-tests-alist'."
  `(progn
     ,@(cl-loop
        for i from 1
        for (ts . exp-ts) in comp-cstr-typespec-tests-alist
        append (list (comp-cstr-typespec-test i ts exp-ts)))))

(comp-cstr-synthesize-tests)

;;; comp-cstr-tests.el ends here
