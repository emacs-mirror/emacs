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

;;; esh-util-tests.el ends here
