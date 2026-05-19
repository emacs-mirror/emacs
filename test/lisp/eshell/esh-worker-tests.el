;;; esh-io-tests.el --- esh-io test suite  -*- lexical-binding:t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

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
(require 'ert-x)
(require 'eshell)

(require 'eshell-tests-helpers
         (ert-resource-file "eshell-tests-helpers"))

;;; Tests:

(ert-deftest esh-worker-test/pipe/eshell-to-function ()
  "Test that piping an Eshell command to a function works."
  (with-temp-eshell
    (eshell-match-command-output "echo hi | #'upcase" "\\`HI\n\\'")))

(ert-deftest esh-worker-test/pipe/eshell-to-function/numeric ()
  "Test that piping an Eshell command to a function works."
  (with-temp-eshell
    (eshell-match-command-output "echo 42 | #'1+" "\\`43\n\\'")))

(ert-deftest esh-worker-test/pipe/external-to-function ()
  "Test that piping an external command to a function works."
  (with-temp-eshell
    (eshell-match-command-output "*echo hi | #'upcase" "\\`HI\n\\'")))

(ert-deftest esh-worker-test/pipe/multiple-to-function ()
  "Test that piping multiple output batches to a function works."
  (with-temp-eshell
    (eshell-match-command-output "{*echo hi; *echo bye} | #'upcase"
                                 "\\`HI\nBYE\n\\'")))

(ert-deftest esh-worker-test/pipe/multiple-to-function/numeric ()
  "Test that piping multiple numeric output batches to a function works."
  (with-temp-eshell
    (eshell-match-command-output "{echo 1; echo 2} | #'1+"
                                 "\\`13\n\\'")))

(ert-deftest esh-worker-test/pipe/eshell-to-lambda ()
  "Test that piping an Eshell command to a lambda works."
  (with-temp-eshell
    (eshell-match-command-output "echo hi | (lambda (i) (concat \"> \" i))"
                                 "\\`> hi\n\\'")))

(ert-deftest esh-worker-test/pipe/external-to-lambda ()
  "Test that piping an external command to a lambda works."
  (with-temp-eshell
    (eshell-match-command-output "*echo hi | (lambda (i) (concat \"> \" i))"
                                 "\\`> hi\n\\'")))

(ert-deftest esh-worker-test/pipe/multiple-to-lambda ()
  "Test that piping multiple output batches to a lambda works."
  (with-temp-eshell
    (eshell-match-command-output
     "{*echo hi; *echo bye} | (lambda (i) (concat \"> \" i))"
     "\\`> hi\nbye\n\\'")))

(ert-deftest esh-worker-test/pipe/multiple-pipes ()
  "Test that piping an Eshell command to a function works."
  (with-temp-eshell
    (eshell-match-command-output
     "echo hi | #'upcase | (lambda (i) (concat \"> \" i))"
     "\\`> HI\n\\'")))

;;; esh-io-tests.el ends here
