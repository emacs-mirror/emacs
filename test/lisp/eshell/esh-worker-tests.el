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

(defvar eshell-test-line-number nil)

(defun eshell-test-number (line)
  "Return LINE with a line number prepended."
  (format "%2d %s" (incf eshell-test-line-number) line))

;;; Tests:


;; Basic worker pipelines

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

(ert-deftest esh-worker-test/pipe/error-handling ()
  "Test that Eshell workers catch errors."
  (with-temp-eshell
    (eshell-match-command-output
     "echo hi | #'1+"
     "\\`1\\+: Wrong type argument: number-or-marker-p, \"hi\"\n\\'")
    (should (= eshell-last-command-status 1))))


;; `map-lines' pipelines

(ert-deftest esh-worker-test/map-lines/eshell-one-line ()
  "Test that piping a single line to `map-lines' works."
  (let ((eshell-test-line-number 0))
    (with-temp-eshell
      (eshell-match-command-output
       "echo hi | map-lines #'eshell-test-number"
       "\\` 1 hi\n\\'"))))

(ert-deftest esh-worker-test/map-lines/eshell-multiple-lines ()
  "Test that piping multiple lines to `map-lines' works.
It should call the mapped function once per line."
  (let ((eshell-test-line-number 0))
    (with-temp-eshell
      (eshell-match-command-output
       "echo 'hi\nbye' | map-lines #'eshell-test-number"
       "\\` 1 hi\n 2 bye\n\\'"))))

(ert-deftest esh-worker-test/map-lines/eshell-multiple-batches ()
  "Test that piping multiple batches of lines to `map-lines' works.
It should call the mapped function once per line, reassembling lines as
needed."
  (let ((eshell-test-line-number 0))
    (with-temp-eshell
      (eshell-match-command-output
       "{echo 'hi\nhel'; echo 'lo\nhey'} | map-lines #'eshell-test-number"
       "\\` 1 hi\n 2 hello\n 3 hey\n\\'"))))

(ert-deftest esh-worker-test/map-lines/external-one-line ()
  "Test that piping a single external line to `map-lines' works."
  (let ((eshell-test-line-number 0))
    (with-temp-eshell
      (eshell-match-command-output
       "*echo hi | map-lines #'eshell-test-number"
       "\\` 1 hi\n\\'"))))

(ert-deftest esh-worker-test/map-lines/external-multiple-lines ()
  "Test that piping multiple external lines to `map-lines' works.
It should call the mapped function once per line."
  (let ((eshell-test-line-number 0))
    (with-temp-eshell
      (eshell-match-command-output
       "*echo 'hi\nbye' | map-lines #'eshell-test-number"
       "\\` 1 hi\n 2 bye\n\\'"))))

(ert-deftest esh-worker-test/map-lines/numbers ()
  "Test that piping numbers to `map-lines' passes them to the mapped function."
  (with-temp-eshell
    (eshell-match-command-output
     "{echo 10; echo 20} | map-lines #'1+"
     "\\`11\n21\n\\'")))

(ert-deftest esh-worker-test/map-lines/numeric-conversion ()
  "Test that `map-lines' converts numeric strings when possible."
  (with-temp-eshell
    (eshell-match-command-output
     "{echo '10\n1'; echo '5\n20'} | map-lines #'1+"
     "\\`11\n16\n21\n\\'")))

(ert-deftest esh-worker-test/map-lines/error-handling ()
  "Test that `map-lines' catches errors."
  (with-temp-eshell
    (eshell-match-command-output
     "echo hi | map-lines #'1+"
     "\\`map-lines 1\\+: Wrong type argument: number-or-marker-p, \"hi\"\n\\'")
    (should (= eshell-last-command-status 1))))


;; `apply-lines' pipelines

(ert-deftest esh-worker-test/apply-lines/eshell-one-line ()
  "Test that piping a single line to `apply-lines' works."
  (with-temp-eshell
    (eshell-match-command-output "echo hi | apply-lines #'upcase"
                                 "\\`HI\n\\'")))

(ert-deftest esh-worker-test/apply-lines/eshell-multiple-lines ()
  "Test that piping multiple lines to `apply-lines' works.
It should pass each line as an argument to the applied function."
  (with-temp-eshell
    (eshell-match-command-output
     "echo 'o\ni\nfoobar' | apply-lines #'string-replace"
     "\\`fiibar\n\\'")))

(ert-deftest esh-worker-test/apply-lines/external-one-line ()
  "Test that piping a single external line to `apply-lines' works."
  (with-temp-eshell
    (eshell-match-command-output "*echo hi | apply-lines #'upcase"
                                 "\\`HI\n\\'")))

(ert-deftest esh-worker-test/apply-lines/external-multiple-lines ()
  "Test that piping multiple external lines to `apply-lines' works.
It should pass each line as an argument to the applied function."
  (with-temp-eshell
    (eshell-match-command-output
     "*echo 'o\ni\nfoobar' | apply-lines #'string-replace"
     "\\`fiibar\n\\'")))

(ert-deftest esh-worker-test/apply-lines/numbers ()
  "Test that piping numbers to `apply-lines' passes them to the function."
  (with-temp-eshell
    (eshell-match-command-output
     "{echo 5; echo 8; echo 13} | apply-lines #'+"
     "\\`26\n\\'")))

(ert-deftest esh-worker-test/apply-lines/numeric-conversion ()
  "Test that `apply-lines' converts numeric strings when possible."
  (with-temp-eshell
    (eshell-match-command-output
     "{echo '10\n1'; echo '5\n20'} | apply-lines #'+"
     "\\`45\n\\'")))

(ert-deftest esh-worker-test/apply-lines/error-handling ()
  "Test that `apply-lines' catches errors."
  (with-temp-eshell
    (eshell-match-command-output
     "echo hi | apply-lines #'1+"
     (concat "\\`apply-lines 1\\+: Wrong type argument: number-or-marker-p, "
             "\"hi\"\n\\'"))
    (should (= eshell-last-command-status 1))))

;;; esh-io-tests.el ends here
