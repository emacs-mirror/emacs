;;; esh-util-tests.el --- esh-util test suite  -*- lexical-binding:t -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

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

(require 'tramp)
(require 'ert)
(require 'esh-util)
(require 'ert-x)

(require 'eshell-tests-helpers
         (ert-resource-file "eshell-tests-helpers"))

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
  ;; These tests depend on the particulars of how Emacs pretty-prints
  ;; lists; changes to the pretty-printer could result in different
  ;; whitespace.  We don't care about that, except to ensure there's
  ;; no leading/trailing whitespace.
  (should (equal (eshell-stringify '(1 2 3)) "(1 2 3)"))
  (should (equal (replace-regexp-in-string
                  (rx (+ (any space "\n"))) " "
                  (eshell-stringify '((1 2) (3 . 4))))
                 "((1 2) (3 . 4))")))

(ert-deftest esh-util-test/eshell-stringify/complex ()
  "Test that `eshell-stringify' correctly stringifies complex objects."
  (should (equal (eshell-stringify (list 'quote 'hello)) "'hello")))

(ert-deftest esh-util-test/eshell-convertible-to-number-p/integer ()
  "Test that `eshell-convertible-to-number-p' matches integers."
  (should (eshell-convertible-to-number-p "123"))
  (should (eshell-convertible-to-number-p "-123"))
  ;; These are technially integers, since Emacs Lisp requires at least
  ;; one digit after the "." to be a float:
  (should (eshell-convertible-to-number-p "123."))
  (should (eshell-convertible-to-number-p "-123.")))

(ert-deftest esh-util-test/eshell-convertible-to-number-p/float ()
  "Test that `eshell-convertible-to-number-p' matches floats."
  (should (eshell-convertible-to-number-p "1.23"))
  (should (eshell-convertible-to-number-p "-1.23"))
  (should (eshell-convertible-to-number-p ".1"))
  (should (eshell-convertible-to-number-p "-.1")))

(ert-deftest esh-util-test/eshell-convertible-to-number-p/float-exponent ()
  "Test that `eshell-convertible-to-number-p' matches exponent notation."
  ;; Positive exponent:
  (dolist (exp '("e2" "e+2" "E2" "E+2"))
    (should (eshell-convertible-to-number-p (concat "123" exp)))
    (should (eshell-convertible-to-number-p (concat "-123" exp)))
    (should (eshell-convertible-to-number-p (concat "1.23" exp)))
    (should (eshell-convertible-to-number-p (concat "-1.23" exp)))
    (should (eshell-convertible-to-number-p (concat "1." exp)))
    (should (eshell-convertible-to-number-p (concat "-1." exp)))
    (should (eshell-convertible-to-number-p (concat ".1" exp)))
    (should (eshell-convertible-to-number-p (concat "-.1" exp))))
  ;; Negative exponent:
  (dolist (exp '("e-2" "E-2"))
    (should (eshell-convertible-to-number-p (concat "123" exp)))
    (should (eshell-convertible-to-number-p (concat "-123" exp)))
    (should (eshell-convertible-to-number-p (concat "1.23" exp)))
    (should (eshell-convertible-to-number-p (concat "-1.23" exp)))
    (should (eshell-convertible-to-number-p (concat "1." exp)))
    (should (eshell-convertible-to-number-p (concat "-1." exp)))
    (should (eshell-convertible-to-number-p (concat ".1" exp)))
    (should (eshell-convertible-to-number-p (concat "-.1" exp)))))

(ert-deftest esh-util-test/eshell-convertible-to-number-p/float/infinite ()
  "Test that `eshell-convertible-to-number-p' matches infinite floats."
  (should (eshell-convertible-to-number-p "1.0e+INF"))
  (should (eshell-convertible-to-number-p "2.e+INF"))
  (should (eshell-convertible-to-number-p "-1.0e+INF"))
  (should (eshell-convertible-to-number-p "-2.e+INF")))

(ert-deftest esh-util-test/eshell-convertible-to-number-p/float/nan ()
  "Test that `eshell-convertible-to-number-p' matches NaNs."
  (should (eshell-convertible-to-number-p "1.0e+NaN"))
  (should (eshell-convertible-to-number-p "2.e+NaN"))
  (should (eshell-convertible-to-number-p "-1.0e+NaN"))
  (should (eshell-convertible-to-number-p "-2.e+NaN")))

(ert-deftest esh-util-test/eshell-convertible-to-number-p/non-numeric ()
  "Test that `eshell-convertible-to-number-p' returns nil for non-numerics."
  (should-not (eshell-convertible-to-number-p "foo"))
  (should-not (eshell-convertible-to-number-p ""))
  (should-not (eshell-convertible-to-number-p "123foo")))

(ert-deftest esh-util-test/eshell-convertible-to-number-p/no-convert ()
  "Test that `eshell-convertible-to-number-p' returns nil when disabled."
  (let ((eshell-convert-numeric-arguments nil))
    (should-not (eshell-convertible-to-number-p "123"))
    (should-not (eshell-convertible-to-number-p "1.23"))))

(ert-deftest esh-util-test/eshell-printable-size ()
  (should (equal (eshell-printable-size (expt 2 16)) "65536"))
  (should (equal (eshell-printable-size (expt 2 32)) "4294967296")))

(ert-deftest esh-util-test/eshell-printable-size/zero ()
  (should (equal (eshell-printable-size 0 1000 nil t) "0")))

(ert-deftest esh-util-test/eshell-printable-size/terabyte ()
  (should (equal (eshell-printable-size (1- (expt 2 40)) 1024 nil t) "1024G"))
  (should (equal (eshell-printable-size (expt 2 40) 1024 nil t) "1T"))
  (should (equal (eshell-printable-size (1- (expt 10 12)) 1000 nil t) "1000G"))
  (should (equal (eshell-printable-size (expt 10 12) 1000 nil t) "1T")))

(ert-deftest esh-util-test/eshell-printable-size/use-colors ()
  (should (equal-including-properties
           (eshell-printable-size (1- (expt 2 20)) 1024 nil t)
           "1024k"))
  (should (equal-including-properties
           (eshell-printable-size (1- (expt 2 30)) 1024 nil t)
           (propertize "1024M" 'face 'bold)))
  (should (equal-including-properties
           (eshell-printable-size (1- (expt 2 40)) 1024 nil t)
           (propertize "1024G" 'face 'bold-italic))))

(ert-deftest esh-util-test/eshell-printable-size/block-size ()
  (should (equal (eshell-printable-size (1- (expt 2 20)) nil 4096) "256"))
  (should (equal (eshell-printable-size (1- (expt 2 30)) nil 4096) "262144")))

(ert-deftest esh-util-test/eshell-printable-size/human-readable-arg ()
  (should-error (eshell-printable-size 0 999 nil t)))

(ert-deftest esh-util-test/path/get ()
  "Test that getting the Eshell path returns the expected results."
  (let ((expected-path (butlast (exec-path))))
    (should (equal (eshell-get-path) expected-path))
    (should (equal (eshell-get-path 'literal)
                   expected-path))))

(ert-deftest esh-util-test/path/get-remote ()
  "Test that getting the remote Eshell path returns the expected results."
  (let* ((default-directory ert-remote-temporary-file-directory)
         (expected-path (butlast (exec-path))))
    ;; Make sure we don't have a doubled directory separator.
    (should (seq-every-p (lambda (i) (not (string-match-p "//" i)))
                         (eshell-get-path)))
    (should (equal (eshell-get-path)
                   (mapcar (lambda (i)
                             (concat (file-remote-p default-directory) i))
                           expected-path)))
    (should (equal (eshell-get-path 'literal)
                   expected-path))))

(ert-deftest esh-util-test/split-filename/absolute ()
  "Test splitting an absolute filename."
  (should (equal (eshell-split-filename "/foo/bar/file.txt")
                 '("/" "foo/" "bar/" "file.txt"))))

(ert-deftest esh-util-test/split-filename/relative ()
  "Test splitting a relative filename."
  (should (equal (eshell-split-filename "foo/bar/file.txt")
                 '("foo/" "bar/" "file.txt"))))

(ert-deftest esh-util-test/split-filename/user ()
  "Test splitting a user filename."
  (should (equal (eshell-split-filename "~/file.txt")
                 '("~/" "file.txt")))
  (should (equal (eshell-split-filename "~user/file.txt")
                 '("~user/" "file.txt"))))

(ert-deftest esh-util-test/split-filename/remote-absolute ()
  "Test splitting a remote absolute filename."
  (skip-unless (eshell-tests-remote-accessible-p))
  (let ((remote (file-remote-p ert-remote-temporary-file-directory)))
    (should (equal (eshell-split-filename (format "%s/foo/bar/file.txt" remote))
                   `(,remote "/" "foo/" "bar/" "file.txt")))))

(ert-deftest esh-util-test/split-filename/remote-relative ()
  "Test splitting a remote relative filename."
  (skip-unless (eshell-tests-remote-accessible-p))
  (let ((remote (file-remote-p ert-remote-temporary-file-directory)))
    (should (equal (eshell-split-filename (format "%sfoo/bar/file.txt" remote))
                   `(,remote "foo/" "bar/" "file.txt")))))

(ert-deftest esh-util-test/split-filename/remote-user ()
  "Test splitting a remote user filename."
  (skip-unless (eshell-tests-remote-accessible-p))
  (let ((remote (file-remote-p ert-remote-temporary-file-directory)))
    (should (equal (eshell-split-filename (format "%s~/file.txt" remote))
                   `(,remote "~/" "file.txt")))
    (should (equal (eshell-split-filename (format "%s~user/file.txt" remote))
                   `(,remote "~user/" "file.txt")))))

;;; esh-util-tests.el ends here
