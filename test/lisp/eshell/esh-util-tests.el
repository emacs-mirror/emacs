;;; esh-util-tests.el --- esh-util test suite  -*- lexical-binding:t -*-

;; Copyright (C) 2022-2023 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)
(require 'esh-util)

;;; Tests:

(ert-deftest esh-util-test/eshell-stringify/string ()
  "Test that `eshell-stringify' preserves the value of strings."
  (should (equal (eshell-stringify "hello") "hello")))

(ert-deftest esh-util-test/eshell-stringify/number ()
  "Test that `eshell-stringify' converts numbers to strings."
  (should (equal (eshell-stringify 42) "42"))
  (should (equal (eshell-stringify 4.2) "4.2")))

(ert-deftest esh-util-test/eshell-stringify/t ()
  "Test that `eshell-stringify' treats `t' according to `eshell-stringify-t'."
  (let ((eshell-stringify-t t))
    (should (equal (eshell-stringify t) "t")))
  (let ((eshell-stringify-t nil))
    (should (equal (eshell-stringify t) nil))))

(ert-deftest esh-util-test/eshell-stringify/nil ()
  "Test that `eshell-stringify' converts nil to a string."
  (should (equal (eshell-stringify nil) "nil")))

(ert-deftest esh-util-test/eshell-stringify/list ()
  "Test that `eshell-stringify' correctly stringifies lists."
  (should (equal (eshell-stringify '(1 2 3)) "(1 2 3)"))
  (should (equal (eshell-stringify '((1 2) (3 . 4)))
                 "((1 2)\n (3 . 4))")))

(ert-deftest esh-util-test/eshell-stringify/complex ()
  "Test that `eshell-stringify' correctly stringifies complex objects."
  (should (equal (eshell-stringify (list 'quote 'hello)) "'hello")))

(ert-deftest esh-util-test/eshell-convert-to-number/integer ()
  "Test that `eshell-convert-to-number' correctly converts integers."
  (should (equal (eshell-convert-to-number "123") 123))
  (should (equal (eshell-convert-to-number "-123") -123))
  ;; These are technially integers, since Emacs Lisp requires at least
  ;; one digit after the "." to be a float:
  (should (equal (eshell-convert-to-number "123.") 123))
  (should (equal (eshell-convert-to-number "-123.") -123)))

(ert-deftest esh-util-test/eshell-convert-to-number/floating-point ()
  "Test that `eshell-convert-to-number' correctly converts floats."
  (should (equal (eshell-convert-to-number "1.23") 1.23))
  (should (equal (eshell-convert-to-number "-1.23") -1.23))
  (should (equal (eshell-convert-to-number ".1") 0.1))
  (should (equal (eshell-convert-to-number "-.1") -0.1)))

(ert-deftest esh-util-test/eshell-convert-to-number/floating-point-exponent ()
  "Test that `eshell-convert-to-number' correctly converts exponent notation."
  ;; Positive exponent:
  (dolist (exp '("e2" "e+2" "E2" "E+2"))
    (should (equal (eshell-convert-to-number (concat "123" exp)) 12300.0))
    (should (equal (eshell-convert-to-number (concat "-123" exp)) -12300.0))
    (should (equal (eshell-convert-to-number (concat "1.23" exp)) 123.0))
    (should (equal (eshell-convert-to-number (concat "-1.23" exp)) -123.0))
    (should (equal (eshell-convert-to-number (concat "1." exp)) 100.0))
    (should (equal (eshell-convert-to-number (concat "-1." exp)) -100.0))
    (should (equal (eshell-convert-to-number (concat ".1" exp)) 10.0))
    (should (equal (eshell-convert-to-number (concat "-.1" exp)) -10.0)))
  ;; Negative exponent:
  (dolist (exp '("e-2" "E-2"))
    (should (equal (eshell-convert-to-number (concat "123" exp)) 1.23))
    (should (equal (eshell-convert-to-number (concat "-123" exp)) -1.23))
    (should (equal (eshell-convert-to-number (concat "1.23" exp)) 0.0123))
    (should (equal (eshell-convert-to-number (concat "-1.23" exp)) -0.0123))
    (should (equal (eshell-convert-to-number (concat "1." exp)) 0.01))
    (should (equal (eshell-convert-to-number (concat "-1." exp)) -0.01))
    (should (equal (eshell-convert-to-number (concat ".1" exp)) 0.001))
    (should (equal (eshell-convert-to-number (concat "-.1" exp)) -0.001))))

(ert-deftest esh-util-test/eshell-convert-to-number/floating-point/infinite ()
  "Test that `eshell-convert-to-number' correctly converts infinite floats."
  (should (equal (eshell-convert-to-number "1.0e+INF") 1.0e+INF))
  (should (equal (eshell-convert-to-number "2.e+INF") 1.0e+INF))
  (should (equal (eshell-convert-to-number "-1.0e+INF") -1.0e+INF))
  (should (equal (eshell-convert-to-number "-2.e+INF") -1.0e+INF)))

(ert-deftest esh-util-test/eshell-convert-to-number/floating-point/nan ()
  "Test that `eshell-convert-to-number' correctly converts NaNs."
  (should (equal (eshell-convert-to-number "1.0e+NaN") 1.0e+NaN))
  (should (equal (eshell-convert-to-number "2.e+NaN") 2.0e+NaN))
  (should (equal (eshell-convert-to-number "-1.0e+NaN") -1.0e+NaN))
  (should (equal (eshell-convert-to-number "-2.e+NaN") -2.0e+NaN)))

(ert-deftest esh-util-test/eshell-convert-to-number/non-numeric ()
  "Test that `eshell-convert-to-number' does nothing to non-numeric values."
  (should (equal (eshell-convert-to-number "foo") "foo"))
  (should (equal (eshell-convert-to-number "") ""))
  (should (equal (eshell-convert-to-number "123foo") "123foo")))

(ert-deftest esh-util-test/eshell-convert-to-number/no-convert ()
  "Test that `eshell-convert-to-number' does nothing when disabled."
  (let ((eshell-convert-numeric-arguments nil))
    (should (equal (eshell-convert-to-number "123") "123"))
    (should (equal (eshell-convert-to-number "1.23") "1.23"))))

;;; esh-util-tests.el ends here
