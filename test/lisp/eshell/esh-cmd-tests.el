;;; esh-cmd-tests.el --- esh-cmd test suite  -*- lexical-binding:t -*-

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

;;; Commentary:

;; Tests for Eshell's command invocation.

;;; Code:

(require 'ert)
(require 'esh-mode)
(require 'eshell)
(require 'ert-x)

(require 'eshell-tests-helpers
         (ert-resource-file "eshell-tests-helpers"))

(defvar eshell-test-value nil)

(defun eshell-test-replace-command (command &rest args)
  "Run COMMAND with ARGS by throwing `eshell-replace-command'."
  (throw 'eshell-replace-command `(eshell-named-command ,command ',args)))

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

(ert-deftest esh-cmd-test/subcommand-shadow-value ()
  "Test that the variable `value' isn't shadowed inside subcommands."
  (with-temp-eshell
   (with-no-warnings (setq-local value "hello"))
   (eshell-match-command-output "echo ${echo $value}"
                                "hello\n")))

(ert-deftest esh-cmd-test/skip-leading-nils ()
  "Test that Eshell skips leading nil arguments for named commands."
  (eshell-command-result-equal "$eshell-test-value echo hello" "hello")
  (eshell-command-result-equal
   "$eshell-test-value $eshell-test-value echo hello" "hello"))

(ert-deftest esh-cmd-test/let-rebinds-after-defer ()
  "Test that let-bound values are properly updated after `eshell-defer'.
When inside a `let' block in an Eshell command form, we need to
ensure that deferred commands update any let-bound variables so
they have the correct values when resuming evaluation.  See
bug#59469."
  (skip-unless (executable-find "echo"))
  (with-temp-eshell
   (eshell-match-command-output
    (concat "{"
            "  export LOCAL=value; "
            "  echo \"$LOCAL\"; "
            "  *echo external; "        ; This will throw `eshell-defer'.
            "  echo \"$LOCAL\"; "
            "}")
    "value\nexternal\nvalue\n")))


;; Background command invocation

(ert-deftest esh-cmd-test/background/simple-command ()
  "Test invocation with a simple background command."
  (skip-unless (executable-find "echo"))
  (eshell-with-temp-buffer bufname ""
    (with-temp-eshell
     (eshell-match-command-output
      (format "*echo hi > #<%s> &" bufname)
      (rx bos "[echo" (? ".exe") "] " (+ digit) "\n"))
     (eshell-wait-for-subprocess t))
    (should (equal (buffer-string) "hi\n"))))

(ert-deftest esh-cmd-test/background/subcommand ()
  "Test invocation with a background command containing subcommands."
  (skip-unless (and (executable-find "echo")
                    (executable-find "rev")))
  (eshell-with-temp-buffer bufname ""
    (with-temp-eshell
     (eshell-match-command-output
      (format "*echo ${*echo hello | rev} > #<%s> &" bufname)
      (rx "[echo" (? ".exe") "] " (+ digit) "\n"))
     (eshell-wait-for-subprocess t))
    (should (equal (buffer-string) "olleh\n"))))

(ert-deftest esh-cmd-test/background/kill ()
  "Make sure that a background command that gets killed doesn't emit a prompt."
  (skip-unless (executable-find "sleep"))
  (let ((background-message (rx bos "[sleep" (? ".exe") "] " (+ digit) "\n")))
    (with-temp-eshell
      (eshell-match-command-output "*sleep 10 &" background-message)
      (kill-process (caar eshell-process-list))
      (eshell-wait-for-subprocess t)
      ;; Ensure we didn't emit another prompt after killing the
      ;; background process.
      (should (eshell-match-output background-message)))))


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

(ert-deftest esh-cmd-test/and-operator/output ()
  "Test output with logical && operator."
  (skip-unless (executable-find "sh"))
  (with-temp-eshell
   ;; Direct commands
   (eshell-match-command-output "sh -c 'echo one; exit 1' && echo two"
                                "\\`one\n\\'")
   (eshell-match-command-output "echo one && echo two"
                                "\\`one\ntwo\n\\'")
   ;; Subcommands
   (eshell-match-command-output "{ sh -c 'echo one; exit 1' } && echo two"
                                "\\`one\n\\'")
   (eshell-match-command-output "{ echo one } && echo two"
                                "\\`one\ntwo\n\\'")))

(ert-deftest esh-cmd-test/or-operator ()
  "Test logical || operator."
  (skip-unless (executable-find "["))
  (with-temp-eshell
   (eshell-match-command-output "[ foo = foo ] || echo hi"
                                "\\`\\'")
   (eshell-match-command-output "[ foo = bar ] || echo hi"
                                "hi\n")))

(ert-deftest esh-cmd-test/or-operator/output ()
  "Test output with logical || operator."
  (skip-unless (executable-find "sh"))
  (with-temp-eshell
   ;; Direct commands
   (eshell-match-command-output "sh -c 'echo one; exit 1' || echo two"
                                "\\`one\ntwo\n\\'")
   (eshell-match-command-output "echo one || echo two"
                                "\\`one\n\\'")
   ;; Subcommands
   (eshell-match-command-output "{ sh -c 'echo one; exit 1' } || echo two"
                                "\\`one\ntwo\n\\'")
   (eshell-match-command-output "{ echo one } || echo two"
                                "\\`one\n\\'")))


;; Pipelines

(ert-deftest esh-cmd-test/pipeline-wait/head-proc ()
  "Check that piping a non-process to a process command waits for the process."
  (skip-unless (executable-find "cat"))
  (with-temp-eshell
   (eshell-match-command-output "echo hi | *cat"
                                "hi")))

(ert-deftest esh-cmd-test/pipeline-wait/tail-proc ()
  "Check that piping a process to a non-process command waits for the process."
  (skip-unless (executable-find "echo"))
  (with-temp-eshell
   (eshell-match-command-output "*echo hi | echo bye"
                                "bye\nhi\n")))

(ert-deftest esh-cmd-test/pipeline-wait/multi-proc ()
  "Check that a pipeline waits for all its processes before returning."
  (skip-unless (and (executable-find "echo")
                    (executable-find "sh")
                    (executable-find "rev")))
  (with-temp-eshell
   (eshell-match-command-output
    "*echo hello | sh -c 'sleep 1; rev' 1>&2 | *echo goodbye"
    "goodbye\nolleh\n")))

(ert-deftest esh-cmd-test/pipeline-wait/subcommand ()
  "Check that piping with an asynchronous subcommand waits for the subcommand."
  (skip-unless (and (executable-find "echo")
                    (executable-find "cat")))
  (with-temp-eshell
   (eshell-match-command-output "echo ${*echo hi} | *cat"
                                "hi")))

(ert-deftest esh-cmd-test/pipeline-wait/subcommand-with-pipe ()
  "Check that piping with an asynchronous subcommand with its own pipe works.
This should also wait for the subcommand."
  (skip-unless (and (executable-find "echo")
                    (executable-find "cat")))
  (with-temp-eshell
   (eshell-match-command-output "echo ${*echo hi | *cat} | *cat"
                                "hi")))

(ert-deftest esh-cmd-test/pipeline-wait/nested-pipes ()
  "Check that piping a subcommand with its own pipe works.
This should also wait for the subcommand."
  (skip-unless (and (executable-find "echo")
                    (executable-find "cat")
                    (executable-find "sh")
                    (executable-find "sleep")))
  (with-temp-eshell
    (eshell-match-command-output
     "{ sh -c 'sleep 1; echo goodbye 1>&2' | *echo hello } | *cat"
     "hello\ngoodbye\n")))

(ert-deftest esh-cmd-test/reset-in-pipeline/subcommand ()
  "Check that subcommands reset `eshell-in-pipeline-p'."
  (skip-unless (executable-find "cat"))
  (dolist (template '("echo {%s} | *cat"
                      "echo ${%s} | *cat"
                      "*cat $<%s> | *cat"))
    (eshell-command-result-equal
     (format template "echo $eshell-in-pipeline-p")
     nil)
    (eshell-command-result-equal
     (format template "echo | echo $eshell-in-pipeline-p")
     "last")
    (eshell-command-result-equal
     (format template "echo $eshell-in-pipeline-p | echo")
     "first")
    (eshell-command-result-equal
     (format template "echo | echo $eshell-in-pipeline-p | echo")
     "t")))

(ert-deftest esh-cmd-test/reset-in-pipeline/lisp ()
  "Check that interpolated Lisp forms reset `eshell-in-pipeline-p'."
  (skip-unless (executable-find "cat"))
  (dolist (template '("echo (%s) | *cat"
                      "echo $(%s) | *cat"))
    (eshell-command-result-equal
     (format template "format \"%s\" eshell-in-pipeline-p")
     "nil")))

(ert-deftest esh-cmd-test/pipeline/replace-command ()
  "Ensure that `eshell-replace-command' doesn't affect Eshell deferral.
Pipelines want to defer (yield) execution after starting all the
processes in the pipeline, not before.  This lets us track all the
processes correctly."
  (skip-unless (and (executable-find "sleep")
                    (executable-find "cat")))
  (with-temp-eshell
    (eshell-insert-command "eshell-test-replace-command *sleep 1 | cat")
    ;; Make sure both processes are in `eshell-foreground-command'; this
    ;; makes sure that the first command (which was replaced via
    ;; `eshell-replace-command' isn't deferred by `eshell-do-eval'.
    (should (= (length (cadr eshell-foreground-command)) 2))))


;; Control flow statements

(ert-deftest esh-cmd-test/for-loop ()
  "Test invocation of a for loop."
  (with-temp-eshell
    (eshell-match-command-output "for i in 1 2 { echo $i }"
                                 "1\n2\n")))

(ert-deftest esh-cmd-test/for-loop-string ()
  "Test invocation of a for loop with complex string arguments."
  (let ((eshell-test-value "X"))
    (with-temp-eshell
      (eshell-match-command-output "for i in a b$eshell-test-value { echo $i }"
                                   "a\nbX\n"))))

(ert-deftest esh-cmd-test/for-loop-list ()
  "Test invocation of a for loop iterating over a list."
  (with-temp-eshell
   (eshell-match-command-output "for i in (list 1 2 (list 3 4)) { echo $i }"
                                "1\n2\n(3 4)\n")))

(ert-deftest esh-cmd-test/for-loop-vector ()
  "Test invocation of a for loop iterating over a vector."
  (with-temp-eshell
    (eshell-match-command-output "for i in `[1 2 3] { echo $i }"
                                 "1\n2\n3\n")))

(ert-deftest esh-cmd-test/for-loop-range ()
  "Test invocation of a for loop iterating over a range."
  (with-temp-eshell
    (eshell-match-command-output "for i in 1..5 { echo $i }"
                                 "1\n2\n3\n4\n")
    (let ((eshell-test-value 2))
      (eshell-match-command-output "for i in $eshell-test-value..5 { echo $i }"
                                   "2\n3\n4\n"))
    ;; Make sure range syntax only work when it's part of the literal
    ;; syntax; a variable expanding to something that looks like a range
    ;; doesn't count.
    (let ((eshell-test-value "1..5"))
      (eshell-match-command-output "for i in $eshell-test-value { echo $i }"
                                   "1..5\n"))))

(ert-deftest esh-cmd-test/for-loop-mixed-args ()
  "Test invocation of a for loop iterating over multiple arguments."
  (with-temp-eshell
   (eshell-match-command-output "for i in 1 2 (list 3 4) { echo $i }"
                                "1\n2\n3\n4\n")))

(ert-deftest esh-cmd-test/for-loop-name () ; bug#15231
  "Test invocation of a for loop using `name'."
  (let ((process-environment (cons "name" process-environment)))
    (eshell-command-result-equal "for name in 3 { echo $name }"
                                 3)))

(ert-deftest esh-cmd-test/for-loop-name-shadow () ; bug#15372
  "Test invocation of a for loop using an env-var."
  (let ((process-environment (cons "name=env-value" process-environment)))
    (with-temp-eshell
     (eshell-match-command-output
      "echo $name; for name in 3 { echo $name }; echo $name"
      "env-value\n3\nenv-value\n"))))

(ert-deftest esh-cmd-test/for-loop-lisp-body ()
  "Test invocation of a for loop with a Lisp body form."
  (with-temp-eshell
   (eshell-match-command-output "for i in 1 2 3 (format \"%s\" i)"
                                "1\n2\n3\n")))

(ert-deftest esh-cmd-test/for-loop-pipe ()
  "Test invocation of a for loop piped to another command."
  (skip-unless (executable-find "rev"))
  (with-temp-eshell
   (eshell-match-command-output "for i in foo bar baz { echo $i } | rev"
                                "zabraboof")))

(ert-deftest esh-cmd-test/while-loop ()
  "Test invocation of a while loop."
  (with-temp-eshell
   (let ((eshell-test-value '(0 1 2)))
     (eshell-match-command-output
      (concat "while $eshell-test-value "
              "{ (pop eshell-test-value) }")
      "0\n1\n2\n"))))

(ert-deftest esh-cmd-test/while-loop-lisp-form ()
  "Test invocation of a while loop using a Lisp form."
  (with-temp-eshell
   (let ((eshell-test-value 0))
     (eshell-match-command-output
      (concat "while (/= eshell-test-value 3) "
              "{ setq eshell-test-value (1+ eshell-test-value) }")
      "1\n2\n3\n"))))

(ert-deftest esh-cmd-test/while-loop-lisp-body ()
  "Test invocation of a while loop using a Lisp form for the body."
  (with-temp-eshell
   (let ((eshell-test-value 0))
     (eshell-match-command-output
      (concat "while (/= eshell-test-value 3) "
              "(setq eshell-test-value (1+ eshell-test-value))")
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

(ert-deftest esh-cmd-test/while-loop-pipe ()
  "Test invocation of a while loop piped to another command."
  (skip-unless (executable-find "rev"))
  (with-temp-eshell
   (let ((eshell-test-value '("foo" "bar" "baz")))
     (eshell-match-command-output
      (concat "while $eshell-test-value "
              "{ (pop eshell-test-value) }"
              " | rev")
      "zabraboof"))))

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
    (eshell-command-result-equal
     "if $eshell-test-value {echo yes} {echo no}" "yes")
    (eshell-command-result-equal
     "if $eshell-test-value {echo yes} else {echo no}" "yes"))
  (let ((eshell-test-value nil))
    (eshell-command-result-equal
     "if $eshell-test-value {echo yes} {echo no}" "no")
    (eshell-command-result-equal
     "if $eshell-test-value {echo yes} else {echo no}" "no")))

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

(ert-deftest esh-cmd-test/if-else-statement-lisp-body ()
  "Test invocation of an if/else statement using Lisp forms for the bodies."
  (eshell-command-result-equal "if (zerop 0) (format \"yes\") (format \"no\")"
                               "yes")
  (eshell-command-result-equal "if (zerop 1) (format \"yes\") (format \"no\")"
                               "no")
  (let ((debug-on-error nil))
    (eshell-command-result-equal
     "if (zerop \"foo\")  (format \"yes\") (format \"no\")"
     "no")))

(ert-deftest esh-cmd-test/if-else-statement-ext-cmd ()
  "Test invocation of an if/else statement using an external command."
  (skip-unless (executable-find "["))
  (eshell-command-result-equal "if {[ foo = foo ]} {echo yes} {echo no}"
                               "yes")
  (eshell-command-result-equal "if {[ foo = bar ]} {echo yes} {echo no}"
                               "no"))

(ert-deftest esh-cmd-test/if-else-statement-chain ()
  "Test invocation of a chained if/else statement."
  (dolist (case '((1 . "one") (2 . "two") (3 . "other")))
    (let ((eshell-test-value (car case)))
      (eshell-command-result-equal
       (concat "if (= eshell-test-value 1) {echo one} "
               "else if (= eshell-test-value 2) {echo two} "
               "else {echo other}")
       (cdr case)))))

(ert-deftest esh-cmd-test/if-statement-pipe ()
  "Test invocation of an if statement piped to another command."
  (skip-unless (executable-find "rev"))
  (with-temp-eshell
   (let ((eshell-test-value t))
     (eshell-match-command-output "if $eshell-test-value {echo yes} | rev"
                                  "\\`sey\n?"))
   (let ((eshell-test-value nil))
     (eshell-match-command-output "if $eshell-test-value {echo yes} | rev"
                                  "\\`\n?"))))

(ert-deftest esh-cmd-test/if-else-statement-pipe ()
  "Test invocation of an if/else statement piped to another command."
  (skip-unless (executable-find "rev"))
  (with-temp-eshell
   (let ((eshell-test-value t))
     (eshell-match-command-output
      "if $eshell-test-value {echo yes} {echo no} | rev"
      "\\`sey\n?"))
   (let ((eshell-test-value nil))
     (eshell-match-command-output
      "if $eshell-test-value {echo yes} {echo no} | rev"
      "\\`on\n?"))))

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


;; Direct invocation

(defmacro esh-cmd-test--deftest-invoke-directly (name command expected)
  "Test `eshell-invoke-directly-p' returns EXPECTED for COMMAND.
NAME is the name of the test case."
  (declare (indent 2))
  `(ert-deftest ,(intern (concat "esh-cmd-test/invoke-directly/"
                                 (symbol-name name)))
       ()
     (with-temp-eshell
      (should (equal (eshell-invoke-directly-p
                      (eshell-parse-command ,command nil t))
                     ,expected)))))

(esh-cmd-test--deftest-invoke-directly no-args "echo" t)
(esh-cmd-test--deftest-invoke-directly with-args "echo hi" t)
(esh-cmd-test--deftest-invoke-directly multiple-cmds "echo hi; echo bye" nil)
(esh-cmd-test--deftest-invoke-directly subcmd "echo ${echo hi}" t)
(esh-cmd-test--deftest-invoke-directly complex "ls ." nil)
(esh-cmd-test--deftest-invoke-directly complex-subcmd "echo {ls .}" nil)


;; Error handling

(ert-deftest esh-cmd-test/empty-background-command ()
  "Test that Eshell reports an error when trying to background a nil command."
  (let ((text-quoting-style 'grave))
    (with-temp-eshell
     (eshell-match-command-output "echo hi & &"
                                  "\\`Empty command before `&'\n")
     ;; Make sure the next Eshell prompt has the original input so the
     ;; user can fix it.
     (should (equal (buffer-substring eshell-last-output-end (point))
                    "echo hi & &")))))

(ert-deftest esh-cmd-test/throw ()
  "Test that calling `throw' as an Eshell command unwinds everything properly."
  (with-temp-eshell
   (should (= (catch 'tag
                (eshell-insert-command
                 "echo hi; (throw 'tag 42); echo bye"))
              42))
   (should (eshell-match-output "\\`hi\n\\'"))
   (should-not eshell-foreground-command)
   ;; Make sure we can call another command after throwing.
   (eshell-match-command-output "echo again" "\\`again\n")))

(ert-deftest esh-cmd-test/command-not-found/pipeline ()
  "Ensure that processes are stopped if a command in a pipeline is not found."
  (skip-when (or (not (executable-find "cat"))
                 (executable-find "nonexist")))
  (with-temp-eshell
    (let ((starting-process-list (process-list)))
      (eshell-match-command-output "nonexist | *cat"
                                   "\\`nonexist: command not found\n")
      (eshell-wait-for-subprocess t)
      (should (equal (process-list) starting-process-list)))))


;; `which' command

(ert-deftest esh-cmd-test/which/plain/eshell-builtin ()
  "Check that `which' finds Eshell built-in functions."
  (eshell-command-result-match "which cat" "\\`eshell/cat"))

(ert-deftest esh-cmd-test/which/plain/external-program ()
  "Check that `which' finds external programs."
  (skip-unless (executable-find "sh"))
  (ert-info (#'eshell-get-debug-logs :prefix "Command logs: ")
    (let ((actual (eshell-test-command-result "which sh"))
          (expected (concat (executable-find "sh") "\n")))
      ;; Eshell handles the casing of the PATH differently from
      ;; `executable-find'.  This means that the results may not match
      ;; exactly on case-insensitive file systems (e.g. when using
      ;; MS-Windows), so compare case-insensitively there.
      (should (if (file-name-case-insensitive-p actual)
                  (string-equal-ignore-case actual expected)
                (string-equal actual expected))))))

(ert-deftest esh-cmd-test/which/plain/not-found ()
  "Check that `which' reports an error for not-found commands."
  (skip-when (executable-find "nonexist"))
  (eshell-command-result-match "which nonexist" "\\`which: no nonexist in"))

(ert-deftest esh-cmd-test/which/alias ()
  "Check that `which' finds aliases."
  (with-temp-eshell
    (eshell-insert-command "alias cat '*cat $@*'")
    (eshell-match-command-output "which cat" "\\`cat is an alias")))

(ert-deftest esh-cmd-test/which/explicit ()
  "Check that `which' finds explicitly-external programs."
  (skip-unless (executable-find "cat"))
  (eshell-command-result-match "which *cat"
                               (concat (executable-find "cat") "\n")))

(ert-deftest esh-cmd-test/which/explicit/not-found ()
  "Check that `which' reports an error for not-found explicit commands."
  (skip-when (executable-find "nonexist"))
  (eshell-command-result-match "which *nonexist" "\\`which: no nonexist in"))

(ert-deftest esh-cmd-test/which/quoted-file ()
  "Check that `which' finds programs with quoted file names."
  (skip-unless (executable-find "cat"))
  (eshell-command-result-match "which /:cat"
                               (concat (executable-find "cat") "\n")))

(ert-deftest esh-cmd-test/which/quoted-file/not-found ()
  "Check that `which' reports an error for not-found quoted commands."
  (skip-when (executable-find "nonexist"))
  (eshell-command-result-match "which /:nonexist" "\\`which: no nonexist in"))

;; esh-cmd-tests.el ends here
