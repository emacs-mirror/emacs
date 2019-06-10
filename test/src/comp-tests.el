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

(ert-deftest  comp-tests-length ()
  "Testing length."
  (defun comp-tests-length-f ()
      (length '(1 2 3)))
  (byte-compile #'comp-tests-length-f)
  (native-compile #'comp-tests-length-f)

  (should (= (comp-tests-length-f) 3)))

(ert-deftest  comp-tests-aref-aset ()
  "Testing aref and aset."
  (defun comp-tests-aref-aset-f ()
    (let ((vec [1 2 3]))
      (aset vec 2 100)
      (aref vec 2)))
  (byte-compile #'comp-tests-aref-aset-f)
  (native-compile #'comp-tests-aref-aset-f)

  (should (= (comp-tests-aref-aset-f) 100)))

(ert-deftest  comp-tests-symbol-value ()
  "Testing aref and aset."
  (defvar comp-tests-var2 3)
  (defun comp-tests-symbol-value-f ()
    (symbol-value 'comp-tests-var2))
  (byte-compile #'comp-tests-symbol-value-f)
  (native-compile #'comp-tests-symbol-value-f)

  (should (= (comp-tests-symbol-value-f) 3)))

(ert-deftest  comp-tests-concat ()
  "Testing concatX opcodes."
  (defun comp-tests-concat-f (x)
    (concat "a" "b" "c" "d"
            (concat "a" "b" "c" (concat "a" "b" (concat "foo" x)))))
  (byte-compile #'comp-tests-concat-f)
  (native-compile #'comp-tests-concat-f)

  (should (string= (comp-tests-concat-f "bar") "abcdabcabfoobar")))

(ert-deftest  comp-tests-ffuncall ()
  "Testing varset."
  (defun comp-tests-ffuncall-callee-f (x y z)
    (list x y z))
  (defun comp-tests-ffuncall-caller-f ()
    (comp-tests-ffuncall-callee-f 1 2 3))
  (byte-compile #'comp-tests-ffuncall-caller-f)
  (native-compile #'comp-tests-ffuncall-caller-f)

  (should (equal (comp-tests-ffuncall-caller-f) '(1 2 3))))

(ert-deftest  comp-tests-conditionals ()
  "Testing conditionals."
  (defun comp-tests-conditionals-1-f (x)
    ;; Generate goto-if-nil
    (if x 1 2))
  (defun comp-tests-conditionals-2-f (x)
    ;; Generate goto-if-nil-else-pop
    (when x
        1340))
  (byte-compile #'comp-tests-conditionals-1-f)
  (byte-compile #'comp-tests-conditionals-2-f)
  (native-compile #'comp-tests-conditionals-1-f)
  (native-compile #'comp-tests-conditionals-2-f)

  (should (= (comp-tests-conditionals-1-f t) 1))
  (should (= (comp-tests-conditionals-1-f nil) 2))
  (should (= (comp-tests-conditionals-2-f t) 1340))
  (should (eq (comp-tests-conditionals-2-f nil) nil)))

(ert-deftest  comp-tests-fixnum ()
  "Testing some fixnum inline operation."
  (defun comp-tests-fixnum-1--f (x)
    (1- x))
  (defun comp-tests-fixnum-1+-f (x)
    (1+ x))

  (byte-compile #'comp-tests-fixnum-1--f)
  (byte-compile #'comp-tests-fixnum-1+-f)
  ;; (native-compile #'comp-tests-fixnum-1--f)
  (native-compile #'comp-tests-fixnum-1+-f)

  (should (= (comp-tests-fixnum-1--f 10) 9))
  (should (= (comp-tests-fixnum-1--f most-negative-fixnum)
             (1- most-negative-fixnum)))
  (should (equal (condition-case err
                     (comp-tests-fixnum-1--f 'a)
                   (error err))
                 '(wrong-type-argument number-or-marker-p a)))
  (should (= (comp-tests-fixnum-1+-f 10) 11))
  (should (= (comp-tests-fixnum-1+-f most-positive-fixnum)
             (1+ most-positive-fixnum)))
  (should (equal (condition-case err
                     (comp-tests-fixnum-1+-f 'a)
                   (error err))
                 '(wrong-type-argument number-or-marker-p a))))

(ert-deftest comp-tests-gc ()
  "Try to do some longer computation to let the gc kick in."
  (dotimes (_ 100000)
    (comp-tests-cons-cdr-f 3))

  (should (= (comp-tests-cons-cdr-f 3) 3)))

;;; comp-tests.el ends here
