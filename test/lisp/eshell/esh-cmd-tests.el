;;; esh-cmd-tests.el --- esh-cmd test suite  -*- lexical-binding:t -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

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

;; Tests for Eshell's command invocation.

;;; Code:

(require 'ert)
(require 'esh-mode)
(require 'eshell)

(require 'eshell-tests-helpers
         (expand-file-name "eshell-tests-helpers"
                           (file-name-directory (or load-file-name
                                                    default-directory))))

(defvar eshell-test-value nil)

;;; Tests:


;; Command invocation

(ert-deftest esh-cmd-test/simple-command-result ()
  "Test invocation with a simple command."
  (eshell-command-result-equal "+ 1 2" 3))

(ert-deftest esh-cmd-test/lisp-command ()
  "Test invocation with an elisp command."
  (eshell-command-result-equal "(+ 1 2)" 3))

(ert-deftest esh-cmd-test/lisp-command-with-quote ()
  "Test invocation with an elisp command containing a quote."
  (eshell-command-result-equal "(eq 'foo nil)" nil))

(ert-deftest esh-cmd-test/lisp-command-args ()
  "Test invocation with elisp and trailing args.
Test that trailing arguments outside the S-expression are
ignored.  e.g. \"(+ 1 2) 3\" => 3"
  (eshell-command-result-equal "(+ 1 2) 3" 3))

(ert-deftest esh-cmd-test/subcommand ()
  "Test invocation with a simple subcommand."
  (eshell-command-result-equal "{+ 1 2}" 3))

(ert-deftest esh-cmd-test/subcommand-args ()
  "Test invocation with a subcommand and trailing args.
Test that trailing arguments outside the subcommand are ignored.
e.g. \"{+ 1 2} 3\" => 3"
  (eshell-command-result-equal "{+ 1 2} 3" 3))

(ert-deftest esh-cmd-test/subcommand-lisp ()
  "Test invocation with an elisp subcommand and trailing args.
Test that trailing arguments outside the subcommand are ignored.
e.g. \"{(+ 1 2)} 3\" => 3"
  (eshell-command-result-equal "{(+ 1 2)} 3" 3))


;; Lisp forms

(ert-deftest esh-cmd-test/quoted-lisp-form ()
  "Test parsing of a quoted Lisp form."
  (eshell-command-result-equal "echo #'(1 2)" '(1 2)))

(ert-deftest esh-cmd-test/backquoted-lisp-form ()
  "Test parsing of a backquoted Lisp form."
  (let ((eshell-test-value 42))
    (eshell-command-result-equal "echo `(answer ,eshell-test-value)"
                                 '(answer 42))))

(ert-deftest esh-cmd-test/backquoted-lisp-form/splice ()
  "Test parsing of a backquoted Lisp form using splicing."
  (let ((eshell-test-value '(2 3)))
    (eshell-command-result-equal "echo `(1 ,@eshell-test-value)"
                                 '(1 2 3))))


;; Logical operators

(ert-deftest esh-cmd-test/and-operator ()
  "Test logical && operator."
  (skip-unless (executable-find "["))
  (with-temp-eshell
   (eshell-match-command-output "[ foo = foo ] && echo hi"
                                "hi\n")
   (eshell-match-command-output "[ foo = bar ] && echo hi"
                                "\\`\\'")))

(ert-deftest esh-cmd-test/or-operator ()
  "Test logical || operator."
  (skip-unless (executable-find "["))
  (with-temp-eshell
   (eshell-match-command-output "[ foo = foo ] || echo hi"
                                "\\`\\'")
   (eshell-match-command-output "[ foo = bar ] || echo hi"
                                "hi\n")))


;; Control flow statements

(ert-deftest esh-cmd-test/for-loop ()
  "Test invocation of a for loop."
  (with-temp-eshell
   (eshell-match-command-output "for i in 5 { echo $i }"
                                "5\n")))

(ert-deftest esh-cmd-test/for-loop-list ()
  "Test invocation of a for loop iterating over a list."
  (with-temp-eshell
   (eshell-match-command-output "for i in (list 1 2 (list 3 4)) { echo $i }"
                                "1\n2\n(3 4)\n")))

(ert-deftest esh-cmd-test/for-loop-multiple-args ()
  "Test invocation of a for loop iterating over multiple arguments."
  (with-temp-eshell
   (eshell-match-command-output "for i in 1 2 (list 3 4) { echo $i }"
                                "1\n2\n3\n4\n")))

(ert-deftest esh-cmd-test/for-name-loop () ; bug#15231
  "Test invocation of a for loop using `name'."
  (let ((process-environment (cons "name" process-environment)))
    (eshell-command-result-equal "for name in 3 { echo $name }"
                                 3)))

(ert-deftest esh-cmd-test/for-name-shadow-loop () ; bug#15372
  "Test invocation of a for loop using an env-var."
  (let ((process-environment (cons "name=env-value" process-environment)))
    (with-temp-eshell
     (eshell-match-command-output
      "echo $name; for name in 3 { echo $name }; echo $name"
      "env-value\n3\nenv-value\n"))))

(ert-deftest esh-cmd-test/while-loop ()
  "Test invocation of a while loop."
  (with-temp-eshell
   (let ((eshell-test-value '(0 1 2)))
     (eshell-match-command-output
      (concat "while $eshell-test-value "
              "{ setq eshell-test-value (cdr eshell-test-value) }")
      "(1 2)\n(2)\n"))))

(ert-deftest esh-cmd-test/while-loop-lisp-form ()
  "Test invocation of a while loop using a Lisp form."
  (with-temp-eshell
   (let ((eshell-test-value 0))
     (eshell-match-command-output
      (concat "while (/= eshell-test-value 3) "
              "{ setq eshell-test-value (1+ eshell-test-value) }")
      "1\n2\n3\n"))))

(ert-deftest esh-cmd-test/while-loop-ext-cmd ()
  "Test invocation of a while loop using an external command."
  (skip-unless (executable-find "["))
  (with-temp-eshell
   (let ((eshell-test-value 0))
     (eshell-match-command-output
      (concat "while {[ $eshell-test-value -ne 3 ]} "
              "{ setq eshell-test-value (1+ eshell-test-value) }")
      "1\n2\n3\n"))))

(ert-deftest esh-cmd-test/until-loop ()
  "Test invocation of an until loop."
  (with-temp-eshell
   (let ((eshell-test-value nil))
     (eshell-match-command-output
      (concat "until $eshell-test-value "
              "{ setq eshell-test-value t }")
      "t\n"))))

(ert-deftest esh-cmd-test/until-loop-lisp-form ()
  "Test invocation of an until loop using a Lisp form."
  (skip-unless (executable-find "["))
  (with-temp-eshell
   (let ((eshell-test-value 0))
     (eshell-match-command-output
      (concat "until (= eshell-test-value 3) "
              "{ setq eshell-test-value (1+ eshell-test-value) }")
      "1\n2\n3\n"))))

(ert-deftest esh-cmd-test/until-loop-ext-cmd ()
  "Test invocation of an until loop using an external command."
  (skip-unless (executable-find "["))
  (with-temp-eshell
   (let ((eshell-test-value 0))
     (eshell-match-command-output
      (concat "until {[ $eshell-test-value -eq 3 ]} "
              "{ setq eshell-test-value (1+ eshell-test-value) }")
      "1\n2\n3\n"))))

(ert-deftest esh-cmd-test/if-statement ()
  "Test invocation of an if statement."
  (let ((eshell-test-value t))
    (eshell-command-result-equal "if $eshell-test-value {echo yes}"
                                 "yes"))
  (let ((eshell-test-value nil))
    (eshell-command-result-equal "if $eshell-test-value {echo yes}"
                                 nil)))

(ert-deftest esh-cmd-test/if-else-statement ()
  "Test invocation of an if/else statement."
  (let ((eshell-test-value t))
    (eshell-command-result-equal "if $eshell-test-value {echo yes} {echo no}"
                                 "yes"))
  (let ((eshell-test-value nil))
    (eshell-command-result-equal "if $eshell-test-value {echo yes} {echo no}"
                                 "no")))

(ert-deftest esh-cmd-test/if-else-statement-lisp-form ()
  "Test invocation of an if/else statement using a Lisp form."
  (eshell-command-result-equal "if (zerop 0) {echo yes} {echo no}"
                               "yes")
  (eshell-command-result-equal "if (zerop 1) {echo yes} {echo no}"
                               "no")
  (let ((debug-on-error nil))
    (eshell-command-result-equal "if (zerop \"foo\") {echo yes} {echo no}"
                                 "no")))

(ert-deftest esh-cmd-test/if-else-statement-lisp-form-2 ()
  "Test invocation of an if/else statement using a Lisp form.
This tests when `eshell-lisp-form-nil-is-failure' is nil."
  (let ((eshell-lisp-form-nil-is-failure nil))
    (eshell-command-result-equal "if (zerop 0) {echo yes} {echo no}"
                                 "yes")
    (eshell-command-result-equal "if (zerop 1) {echo yes} {echo no}"
                                 "yes")
    (let ((debug-on-error nil))
      (eshell-command-result-equal "if (zerop \"foo\") {echo yes} {echo no}"
                                   "no"))))

(ert-deftest esh-cmd-test/if-else-statement-ext-cmd ()
  "Test invocation of an if/else statement using an external command."
  (skip-unless (executable-find "["))
  (eshell-command-result-equal "if {[ foo = foo ]} {echo yes} {echo no}"
                               "yes")
  (eshell-command-result-equal "if {[ foo = bar ]} {echo yes} {echo no}"
                               "no"))

(ert-deftest esh-cmd-test/unless-statement ()
  "Test invocation of an unless statement."
  (let ((eshell-test-value t))
    (eshell-command-result-equal "unless $eshell-test-value {echo no}"
                                 nil))
  (let ((eshell-test-value nil))
    (eshell-command-result-equal "unless $eshell-test-value {echo no}"
                                 "no")))

(ert-deftest esh-cmd-test/unless-else-statement ()
  "Test invocation of an unless/else statement."
  (let ((eshell-test-value t))
    (eshell-command-result-equal
     "unless $eshell-test-value {echo no} {echo yes}"
     "yes"))
  (let ((eshell-test-value nil))
    (eshell-command-result-equal
     "unless $eshell-test-value {echo no} {echo yes}"
     "no")))

(ert-deftest esh-cmd-test/unless-else-statement-lisp-form ()
  "Test invocation of an unless/else statement using a Lisp form."
  (eshell-command-result-equal "unless (zerop 0) {echo no} {echo yes}"
                               "yes")
  (eshell-command-result-equal "unless (zerop 1) {echo no} {echo yes}"
                               "no")
  (let ((debug-on-error nil))
    (eshell-command-result-equal "unless (zerop \"foo\") {echo no} {echo yes}"
                                 "no")))

(ert-deftest esh-cmd-test/unless-else-statement-ext-cmd ()
  "Test invocation of an unless/else statement using an external command."
  (skip-unless (executable-find "["))
  (eshell-command-result-equal "unless {[ foo = foo ]} {echo no} {echo yes}"
                               "yes")
  (eshell-command-result-equal "unless {[ foo = bar ]} {echo no} {echo yes}"
                               "no"))

;; esh-cmd-tests.el ends here
