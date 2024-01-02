;;; comp-cstr-tests.el --- unit tests for src/comp.c -*- lexical-binding: t; -*-

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

;; Unit tests for lisp/emacs-lisp/comp-cstr.el

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'comp-cstr)

(cl-eval-when (compile eval load)

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
      ((or symbol number) . (or number symbol))
      ;; 4
      ((or cons atom) . (or atom cons)) ;; SBCL return T
      ;; 5
      ((or integer number) . number)
      ;; 6
      ((or (or integer symbol) number) . (or number symbol))
      ;; 7
      ((or (or integer symbol) (or number list)) . (or list number symbol))
      ;; 8
      ((or (or integer number) nil) . number)
      ;; 9
      ((member foo) . (member foo))
      ;; 10
      ((member foo bar) . (member bar foo))
      ;; 11
      ((or (member foo) (member bar)) . (member bar foo))
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
      ((and (member 1 2) (member 3 2)) . (integer 2 2))
      ;; 32
      ((and number (integer 1 2)) . (integer 1 2))
      ;; 33
      ((and integer (integer 1 2)) . (integer 1 2))
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
      ;; 42
      ((or symbol (not sequence)) . (not sequence))
      ;; 43
      ((or symbol (not symbol)) . t)
      ;; 44
      ((or symbol (not sequence)) . (not sequence))
      ;; 45 Conservative.
      ((or vector (not sequence)) . t)
      ;; 46
      ((or (integer 1 10) (not (integer * 5))) . (not (integer * 0)))
      ;; 47
      ((or symbol (integer 1 10) (not (integer * 5))) . (not (integer * 0)))
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
      ((or (not (integer 1 2)) integer) . t)
      ;; 54
      ((or (not (integer 1 2)) (not integer)) . (not integer))
      ;; 55
      ((or (integer 1 2) (not integer)) . (not (or (integer * 0) (integer 3 *))))
      ;; 56
      ((or number (not (integer 1 2))) . t)
      ;; 57
      ((or atom (not (integer 1 2))) . t)
      ;; 58
      ((or atom (not (member foo))) . t)
      ;; 59
      ((and symbol (not cons)) . symbol)
      ;; 60
      ((and symbol (not symbol)) . nil)
      ;; 61
      ((and atom (not symbol)) . atom)
      ;; 62
      ((and atom (not string)) . (or array sequence atom))
      ;; 63 Conservative
      ((and symbol (not (member foo))) . symbol)
      ;; 64 Conservative
      ((and symbol (not (member 3))) . symbol)
      ;; 65
      ((and (not (member foo)) (integer 1 10)) . (integer 1 10))
      ;; 66
      ((and (member foo) (not (integer 1 10))) . (member foo))
      ;; 67
      ((and t (not (member foo))) . (not (member foo)))
      ;; 68
      ((and integer (not (integer 3 4))) . (or (integer * 2) (integer 5 *)))
      ;; 69
      ((and (integer 0 20) (not (integer 5 10))) . (or (integer 0 4) (integer 11 20)))
      ;; 70
      ((and (not (member a)) (not (member b))) . (not (member a b)))
      ;; 71
      ((and (not boolean) (not (member b))) . (not (or (member b) boolean)))
      ;; 72
      ((and t (integer 1 1)) . (integer 1 1))
      ;; 73
      ((not (integer -1 5)) . (not (integer -1 5)))
      ;; 74
      ((and boolean (or number marker)) . nil)
      ;; 75
      ((and atom (or number marker)) . (or marker number))
      ;; 76
      ((and symbol (or number marker)) . nil)
      ;; 77
      ((and (or symbol string) (or number marker)) . nil)
      ;; 78
      ((and t t) . t)
      ;; 79
      ((and (or marker number) (integer 0 0)) . (integer 0 0))
      ;; 80
      ((and t (not t)) . nil)
      ;; 81
      ((or (integer 1 1) (not (integer 1 1))) . t)
      ;; 82
      ((not t) . nil)
      ;; 83
      ((not nil) . t)
      ;; 84
      ((or (not string) t) . t)
      ;; 85
      ((or (not vector) sequence) . sequence)
      ;; 86
      ((or (not symbol) null) . t)
      ;; 87
      ((and (or null integer) (not (or null integer))) . nil)
      ;; 88
      ((and (or (member a b c)) (not (or (member a b)))) . (member c)))
    "Alist type specifier -> expected type specifier."))

(defmacro comp-cstr-synthesize-tests ()
  "Generate all tests from `comp-cstr-typespec-tests-alist'."
  `(progn
     ,@(cl-loop
        for i from 1
        for (ts . exp-ts) in comp-cstr-typespec-tests-alist
        append (list (comp-cstr-typespec-test i ts exp-ts)))))

(comp-cstr-synthesize-tests)

;;; comp-cstr-tests.el ends here
