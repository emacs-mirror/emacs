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
    ;; ((and string array) . string)
    ((or symbol number) . (or symbol number))
    ((or cons atom) . (or cons atom)) ;; SBCL return T
    ;; ((and cons atom) . (or cons atom))
    ((member foo) . (member foo))
    ((member foo bar) . (member foo bar))
    ((or (member foo) (member bar)) . (member foo bar))
    ;; ((and (member foo) (member bar)) . symbol)
    ((or (member foo) symbol) . symbol) ;; SBCL return (OR SYMBOL (MEMBER FOO))
    ;; ((and (member foo) symbol) . (member foo))
    ((or (member foo) number) .  (or (member foo) number)))
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
