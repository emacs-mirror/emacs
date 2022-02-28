;;; esh-var-tests.el --- esh-var test suite  -*- lexical-binding:t -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

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

;; Tests for Eshell's variable handling.

;;; Code:

(require 'ert)
(require 'esh-mode)
(require 'eshell)

(require 'eshell-tests-helpers
         (expand-file-name "eshell-tests-helpers"
                           (file-name-directory (or load-file-name
                                                    default-directory))))

;;; Tests:


;; Variable interpolation

(ert-deftest esh-var-test/interp-lisp ()
  "Interpolate Lisp form evaluation"
  (should (equal (eshell-test-command-result "+ $(+ 1 2) 3") 6)))

(ert-deftest esh-var-test/interp-cmd ()
  "Interpolate command result"
  (should (equal (eshell-test-command-result "+ ${+ 1 2} 3") 6)))

(ert-deftest esh-var-test/interp-cmd-external ()
  "Interpolate command result from external command"
  (skip-unless (executable-find "echo"))
  (with-temp-eshell
   (eshell-command-result-p "echo ${*echo hi}"
                            "hi\n")))

(ert-deftest esh-var-test/interp-temp-cmd ()
  "Interpolate command result redirected to temp file"
  (should (equal (eshell-test-command-result "cat $<echo hi>") "hi")))

(ert-deftest esh-var-test/interp-concat-lisp ()
  "Interpolate and concat Lisp form"
  (should (equal (eshell-test-command-result "+ $(+ 1 2)3 3") 36)))

(ert-deftest esh-var-test/interp-concat-lisp2 ()
  "Interpolate and concat two Lisp forms"
  (should (equal (eshell-test-command-result "+ $(+ 1 2)$(+ 1 2) 3") 36)))

(ert-deftest esh-var-test/interp-concat-cmd ()
  "Interpolate and concat command"
  (should (equal (eshell-test-command-result "+ ${+ 1 2}3 3") 36)))

(ert-deftest esh-var-test/interp-concat-cmd2 ()
  "Interpolate and concat two commands"
  (should (equal (eshell-test-command-result "+ ${+ 1 2}${+ 1 2} 3") 36)))

(ert-deftest esh-var-test/interp-concat-cmd-external ()
  "Interpolate command result from external command with concatenation"
  (skip-unless (executable-find "echo"))
  (with-temp-eshell
   (eshell-command-result-p "echo ${echo hi}-${*echo there}"
                            "hi-there\n")))


;; Built-in variables

(ert-deftest esh-var-test/window-height ()
  "$LINES should equal (window-height)"
  (should (eshell-test-command-result "= $LINES (window-height)")))

(ert-deftest esh-var-test/window-width ()
  "$COLUMNS should equal (window-width)"
  (should (eshell-test-command-result "= $COLUMNS (window-width)")))

(ert-deftest esh-var-test/last-result-var ()
  "Test using the \"last result\" ($$) variable"
  (with-temp-eshell
   (eshell-command-result-p "+ 1 2; + $$ 2"
                            "3\n5\n")))

(ert-deftest esh-var-test/last-result-var2 ()
  "Test using the \"last result\" ($$) variable twice"
  (with-temp-eshell
   (eshell-command-result-p "+ 1 2; + $$ $$"
                             "3\n6\n")))

(ert-deftest esh-var-test/last-arg-var ()
  "Test using the \"last arg\" ($_) variable"
  (with-temp-eshell
   (eshell-command-result-p "+ 1 2; + $_ 4"
                             "3\n6\n")))

;; esh-var-tests.el ends here
