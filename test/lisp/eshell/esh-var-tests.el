;;; esh-var-tests.el --- esh-var test suite  -*- lexical-binding:t -*-

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

;; Tests for Eshell's variable handling.

;;; Code:

(require 'tramp)
(require 'ert)
(require 'esh-mode)
(require 'esh-var)
(require 'eshell)

(require 'eshell-tests-helpers
         (expand-file-name "eshell-tests-helpers"
                           (file-name-directory (or load-file-name
                                                    default-directory))))

(defvar eshell-test-value nil)

;;; Tests:


;; Variable interpolation

(ert-deftest esh-var-test/interp-var ()
  "Interpolate variable"
  (eshell-command-result-equal "echo $user-login-name"
                               user-login-name))

(ert-deftest esh-var-test/interp-quoted-var ()
  "Interpolate quoted variable"
  (eshell-command-result-equal "echo $'user-login-name'"
                               user-login-name)
  (eshell-command-result-equal "echo $\"user-login-name\""
                               user-login-name))

(ert-deftest esh-var-test/interp-quoted-var-concat ()
  "Interpolate and concat quoted variable"
  (eshell-command-result-equal "echo $'user-login-name'-foo"
                               (concat user-login-name "-foo"))
  (eshell-command-result-equal "echo $\"user-login-name\"-foo"
                               (concat user-login-name "-foo")))

(ert-deftest esh-var-test/interp-var-indices ()
  "Interpolate list variable with indices"
  (let ((eshell-test-value '("zero" "one" "two" "three" "four")))
    (eshell-command-result-equal "echo $eshell-test-value[0]"
                                 "zero")
    (eshell-command-result-equal "echo $eshell-test-value[0 2]"
                                 '("zero" "two"))
    (eshell-command-result-equal "echo $eshell-test-value[0 2 4]"
                                 '("zero" "two" "four"))))

(ert-deftest esh-var-test/interp-var-split-indices ()
  "Interpolate string variable with indices"
  (let ((eshell-test-value "zero one two three four"))
    (eshell-command-result-equal "echo $eshell-test-value[0]"
                                 "zero")
    (eshell-command-result-equal "echo $eshell-test-value[0 2]"
                                 '("zero" "two"))
    (eshell-command-result-equal "echo $eshell-test-value[0 2 4]"
                                 '("zero" "two" "four"))))

(ert-deftest esh-var-test/interp-var-string-split-indices ()
  "Interpolate string variable with string splitter and indices"
  (let ((eshell-test-value "zero:one:two:three:four"))
    (eshell-command-result-equal "echo $eshell-test-value[: 0]"
                                 "zero")
    (eshell-command-result-equal "echo $eshell-test-value[: 0 2]"
                                 '("zero" "two")))
  (let ((eshell-test-value "zeroXoneXtwoXthreeXfour"))
    (eshell-command-result-equal "echo $eshell-test-value[X 0]"
                                 "zero")
    (eshell-command-result-equal "echo $eshell-test-value[X 0 2]"
                                 '("zero" "two"))))

(ert-deftest esh-var-test/interp-var-regexp-split-indices ()
  "Interpolate string variable with regexp splitter and indices"
  (let ((eshell-test-value "zero:one!two:three!four"))
    (eshell-command-result-equal "echo $eshell-test-value['[:!]' 0]"
                                 "zero")
    (eshell-command-result-equal "echo $eshell-test-value['[:!]' 0 2]"
                                 '("zero" "two"))
    (eshell-command-result-equal "echo $eshell-test-value[\"[:!]\" 0]"
                                 "zero")
    (eshell-command-result-equal "echo $eshell-test-value[\"[:!]\" 0 2]"
                                 '("zero" "two"))))

(ert-deftest esh-var-test/interp-var-assoc ()
  "Interpolate alist variable with index"
  (let ((eshell-test-value '(("foo" . 1) (bar . 2))))
    (eshell-command-result-equal "echo $eshell-test-value[foo]"
                                 1)
    (eshell-command-result-equal "echo $eshell-test-value[#'bar]"
                                 2)))

(ert-deftest esh-var-test/interp-var-length-list ()
  "Interpolate length of list variable"
  (let ((eshell-test-value '((1 2) (3) (5 (6 7 8 9)))))
    (eshell-command-result-equal "echo $#eshell-test-value" 3)
    (eshell-command-result-equal "echo $#eshell-test-value[1]" 1)
    (eshell-command-result-equal "echo $#eshell-test-value[2][1]" 4)))

(ert-deftest esh-var-test/interp-var-length-string ()
  "Interpolate length of string variable"
  (let ((eshell-test-value "foobar"))
    (eshell-command-result-equal "echo $#eshell-test-value" 6)))

(ert-deftest esh-var-test/interp-var-length-alist ()
  "Interpolate length of alist variable"
  (let ((eshell-test-value '(("foo" . (1 2 3)))))
    (eshell-command-result-equal "echo $#eshell-test-value" 1)
    (eshell-command-result-equal "echo $#eshell-test-value[foo]" 3)))

(ert-deftest esh-var-test/interp-lisp ()
  "Interpolate Lisp form evaluation"
  (eshell-command-result-equal "+ $(+ 1 2) 3" 6))

(ert-deftest esh-var-test/interp-lisp-indices ()
  "Interpolate Lisp form evaluation with index"
  (eshell-command-result-equal "+ $(list 1 2)[1] 3" 5))

(ert-deftest esh-var-test/interp-cmd ()
  "Interpolate command result"
  (eshell-command-result-equal "+ ${+ 1 2} 3" 6))

(ert-deftest esh-var-test/interp-cmd-indices ()
  "Interpolate command result with index"
  (eshell-command-result-equal "+ ${listify 1 2}[1] 3" 5))

(ert-deftest esh-var-test/interp-cmd-external ()
  "Interpolate command result from external command"
  (skip-unless (executable-find "echo"))
  (with-temp-eshell
   (eshell-match-command-output "echo ${*echo hi}"
                                "hi\n")))

(ert-deftest esh-var-test/interp-cmd-external-indices ()
  "Interpolate command result from external command with index"
  (skip-unless (executable-find "echo"))
  (with-temp-eshell
   (eshell-match-command-output "echo ${*echo \"hi\nbye\"}[1]"
                                "bye\n")))

(ert-deftest esh-var-test/interp-temp-cmd ()
  "Interpolate command result redirected to temp file"
  (eshell-command-result-equal "cat $<echo hi>" "hi"))

(ert-deftest esh-var-test/interp-concat-lisp ()
  "Interpolate and concat Lisp form"
  (eshell-command-result-equal "+ $(+ 1 2)3 3" 36))

(ert-deftest esh-var-test/interp-concat-lisp2 ()
  "Interpolate and concat two Lisp forms"
  (eshell-command-result-equal "+ $(+ 1 2)$(+ 1 2) 3" 36))

(ert-deftest esh-var-test/interp-concat-cmd ()
  "Interpolate and concat command with literal"
  (eshell-command-result-equal "+ ${+ 1 2}3 3" 36)
  (eshell-command-result-equal "echo ${*echo \"foo\nbar\"}-baz"
                               '("foo" "bar-baz"))
  ;; Concatenating to a number in a list should produce a number...
  (eshell-command-result-equal "echo ${*echo \"1\n2\"}3"
                               '(1 23))
  ;; ... but concatenating to a string that looks like a number in a list
  ;; should produce a string.
  (eshell-command-result-equal "echo ${*echo \"hi\n2\"}3"
                               '("hi" "23")))

(ert-deftest esh-var-test/interp-concat-cmd2 ()
  "Interpolate and concat two commands"
  (eshell-command-result-equal "+ ${+ 1 2}${+ 1 2} 3" 36))

(ert-deftest esh-var-test/interp-concat-cmd-external ()
  "Interpolate command result from external command with concatenation"
  (skip-unless (executable-find "echo"))
  (with-temp-eshell
   (eshell-match-command-output "echo ${echo hi}-${*echo there}"
                                "hi-there\n")))

(ert-deftest esh-var-test/quoted-interp-var ()
  "Interpolate variable inside double-quotes"
  (eshell-command-result-equal "echo \"$user-login-name\""
                               user-login-name))

(ert-deftest esh-var-test/quoted-interp-quoted-var ()
  "Interpolate quoted variable inside double-quotes"
  (eshell-command-result-equal "echo \"hi, $'user-login-name'\""
                               (concat "hi, " user-login-name))
  (eshell-command-result-equal "echo \"hi, $\\\"user-login-name\\\"\""
                               (concat "hi, " user-login-name)))

(ert-deftest esh-var-test/quoted-interp-var-indices ()
  "Interpolate string variable with indices inside double-quotes"
  (let ((eshell-test-value '("zero" "one" "two" "three" "four")))
    (eshell-command-result-equal "echo \"$eshell-test-value[0]\""
                                 "zero")
    ;; FIXME: These tests would use the 0th index like the other tests
    ;; here, but evaluating the command just above adds an `escaped'
    ;; property to the string "zero".  This results in the output
    ;; printing the string properties, which is probably the wrong
    ;; behavior.  See bug#54486.
    (eshell-command-result-equal "echo \"$eshell-test-value[1 2]\""
                                 "(\"one\" \"two\")")
    (eshell-command-result-equal "echo \"$eshell-test-value[1 2 4]\""
                                 "(\"one\" \"two\" \"four\")")))

(ert-deftest esh-var-test/quoted-interp-var-split-indices ()
  "Interpolate string variable with indices inside double-quotes"
  (let ((eshell-test-value "zero one two three four"))
    (eshell-command-result-equal "echo \"$eshell-test-value[0]\""
                                 "zero")
    (eshell-command-result-equal "echo \"$eshell-test-value[0 2]\""
                                 "(\"zero\" \"two\")")))

(ert-deftest esh-var-test/quoted-interp-var-string-split-indices ()
  "Interpolate string variable with string splitter and indices
inside double-quotes"
  (let ((eshell-test-value "zero:one:two:three:four"))
    (eshell-command-result-equal "echo \"$eshell-test-value[: 0]\""
                                 "zero")
    (eshell-command-result-equal "echo \"$eshell-test-value[: 0 2]\""
                                 "(\"zero\" \"two\")"))
  (let ((eshell-test-value "zeroXoneXtwoXthreeXfour"))
    (eshell-command-result-equal "echo \"$eshell-test-value[X 0]\""
                                 "zero")
    (eshell-command-result-equal "echo \"$eshell-test-value[X 0 2]\""
                                 "(\"zero\" \"two\")")))

(ert-deftest esh-var-test/quoted-interp-var-regexp-split-indices ()
  "Interpolate string variable with regexp splitter and indices"
  (let ((eshell-test-value "zero:one!two:three!four"))
    (eshell-command-result-equal "echo \"$eshell-test-value['[:!]' 0]\""
                                 "zero")
    (eshell-command-result-equal "echo \"$eshell-test-value['[:!]' 0 2]\""
                                 "(\"zero\" \"two\")")
    (eshell-command-result-equal "echo \"$eshell-test-value[\\\"[:!]\\\" 0]\""
                                 "zero")
    (eshell-command-result-equal
     "echo \"$eshell-test-value[\\\"[:!]\\\" 0 2]\""
     "(\"zero\" \"two\")")))

(ert-deftest esh-var-test/quoted-interp-var-assoc ()
  "Interpolate alist variable with index inside double-quotes"
  (let ((eshell-test-value '(("foo" . 1) (bar . 2))))
    (eshell-command-result-equal "echo \"$eshell-test-value[foo]\""
                                 "1")
    (eshell-command-result-equal "echo \"$eshell-test-value[#'bar]\""
                                 "2")))

(ert-deftest esh-var-test/quoted-interp-var-length-list ()
  "Interpolate length of list variable inside double-quotes"
  (let ((eshell-test-value '((1 2) (3) (5 (6 7 8 9)))))
    (eshell-command-result-equal "echo \"$#eshell-test-value\""
                                 "3")
    (eshell-command-result-equal "echo \"$#eshell-test-value[1]\""
                                 "1")
    (eshell-command-result-equal "echo \"$#eshell-test-value[2][1]\""
                                 "4")))

(ert-deftest esh-var-test/quoted-interp-var-length-string ()
  "Interpolate length of string variable inside double-quotes"
  (let ((eshell-test-value "foobar"))
    (eshell-command-result-equal "echo \"$#eshell-test-value\""
                                 "6")))

(ert-deftest esh-var-test/quoted-interp-var-length-alist ()
  "Interpolate length of alist variable inside double-quotes"
  (let ((eshell-test-value '(("foo" . (1 2 3)))))
    (eshell-command-result-equal "echo \"$#eshell-test-value\""
                                 "1")
    (eshell-command-result-equal "echo \"$#eshell-test-value[foo]\""
                                 "3")))

(ert-deftest esh-var-test/quoted-interp-lisp ()
  "Interpolate Lisp form evaluation inside double-quotes"
  (eshell-command-result-equal "echo \"hi $(concat \\\"the\\\" \\\"re\\\")\""
                               "hi there"))

(ert-deftest esh-var-test/quoted-interp-lisp-indices ()
  "Interpolate Lisp form evaluation with index"
  (eshell-command-result-equal "concat \"$(list 1 2)[1]\" cool"
                               "2cool"))

(ert-deftest esh-var-test/quoted-interp-cmd ()
  "Interpolate command result inside double-quotes"
  (eshell-command-result-equal "echo \"hi ${echo \\\"there\\\"}\""
                               "hi there"))

(ert-deftest esh-var-test/quoted-interp-cmd-indices ()
  "Interpolate command result with index inside double-quotes"
  (eshell-command-result-equal "concat \"${listify 1 2}[1]\" cool"
                               "2cool"))

(ert-deftest esh-var-test/quoted-interp-temp-cmd ()
  "Interpolate command result redirected to temp file inside double-quotes"
  (let ((temporary-file-directory
         (file-name-as-directory (make-temp-file "esh-vars-tests" t))))
    (unwind-protect
        (eshell-command-result-equal "cat \"$<echo hi>\"" "hi")
      (delete-directory temporary-file-directory t))))

(ert-deftest esh-var-test/quoted-interp-concat-cmd ()
  "Interpolate and concat command with literal"
  (eshell-command-result-equal "echo \"${echo \\\"foo\nbar\\\"} baz\""
                               "foo\nbar baz"))


;; Interpolated variable conversion

(ert-deftest esh-var-test/interp-convert-var-number ()
  "Interpolate numeric variable"
  (let ((eshell-test-value 123))
    (eshell-command-result-equal "type-of $eshell-test-value"
                                 'integer)))

(ert-deftest esh-var-test/interp-convert-var-split-indices ()
  "Interpolate and convert string variable with indices"
  ;; Check that numeric forms are converted to numbers.
  (let ((eshell-test-value "000 010 020 030 040"))
    (eshell-command-result-equal "echo $eshell-test-value[0]"
                                 0)
    (eshell-command-result-equal "echo $eshell-test-value[0 2]"
                                 '(0 20)))
  ;; Check that multiline forms are preserved as-is.
  (let ((eshell-test-value "foo\nbar:baz\n"))
    (eshell-command-result-equal "echo $eshell-test-value[: 0]"
                                 "foo\nbar")
    (eshell-command-result-equal "echo $eshell-test-value[: 1]"
                                 "baz\n")))

(ert-deftest esh-var-test/interp-convert-quoted-var-number ()
  "Interpolate numeric quoted numeric variable"
  (let ((eshell-test-value 123))
    (eshell-command-result-equal "type-of $'eshell-test-value'"
                                 'integer)
    (eshell-command-result-equal "type-of $\"eshell-test-value\""
                                 'integer)))

(ert-deftest esh-var-test/interp-convert-quoted-var-split-indices ()
  "Interpolate and convert quoted string variable with indices"
  (let ((eshell-test-value "000 010 020 030 040"))
    (eshell-command-result-equal "echo $'eshell-test-value'[0]"
                                 0)
    (eshell-command-result-equal "echo $'eshell-test-value'[0 2]"
                                 '(0 20))))

(ert-deftest esh-var-test/interp-convert-cmd-string-newline ()
  "Interpolate trailing-newline command result"
  (eshell-command-result-equal "echo ${echo \"foo\n\"}" "foo"))

(ert-deftest esh-var-test/interp-convert-cmd-multiline ()
  "Interpolate multi-line command result"
  (eshell-command-result-equal "echo ${echo \"foo\nbar\"}"
                               '("foo" "bar"))
  ;; Numeric output should be converted to numbers...
  (eshell-command-result-equal "echo ${echo \"01\n02\n03\"}"
                               '(1 2 3))
  ;; ... but only if every line is numeric.
  (eshell-command-result-equal "echo ${echo \"01\n02\nhi\"}"
                               '("01" "02" "hi")))

(ert-deftest esh-var-test/interp-convert-cmd-number ()
  "Interpolate numeric command result"
  (eshell-command-result-equal "echo ${echo \"1\"}" 1))

(ert-deftest esh-var-test/interp-convert-cmd-split-indices ()
  "Interpolate command result with indices"
  (eshell-command-result-equal "echo ${echo \"000 010 020\"}[0]"
                               0)
  (eshell-command-result-equal "echo ${echo \"000 010 020\"}[0 2]"
                               '(0 20)))

(ert-deftest esh-var-test/quoted-interp-convert-var-number ()
  "Interpolate numeric variable inside double-quotes"
  (let ((eshell-test-value 123))
    (eshell-command-result-equal "type-of \"$eshell-test-value\""
                                 'string)))

(ert-deftest esh-var-test/quoted-interp-convert-var-split-indices ()
  "Interpolate string variable with indices inside double-quotes"
  (let ((eshell-test-value "000 010 020 030 040"))
    (eshell-command-result-equal "echo \"$eshell-test-value[0]\""
                                 "000")
    (eshell-command-result-equal "echo \"$eshell-test-value[0 2]\""
                                 "(\"000\" \"020\")")))

(ert-deftest esh-var-test/quoted-interp-convert-quoted-var-number ()
  "Interpolate numeric quoted variable inside double-quotes"
  (let ((eshell-test-value 123))
    (eshell-command-result-equal "type-of \"$'eshell-test-value'\""
                                 'string)
    (eshell-command-result-equal "type-of \"$\\\"eshell-test-value\\\"\""
                                 'string)))

(ert-deftest esh-var-test/quoted-interp-convert-quoted-var-split-indices ()
  "Interpolate quoted string variable with indices inside double-quotes"
  (let ((eshell-test-value "000 010 020 030 040"))
    (eshell-command-result-equal "echo \"$eshell-test-value[0]\""
                                 "000")
    (eshell-command-result-equal "echo \"$eshell-test-value[0 2]\""
                                 "(\"000\" \"020\")")))

(ert-deftest esh-var-test/quoted-interp-convert-cmd-string-newline ()
  "Interpolate trailing-newline command result inside double-quotes"
  (eshell-command-result-equal "echo \"${echo \\\"foo\n\\\"}\""
                               "foo")
  (eshell-command-result-equal "echo \"${echo \\\"foo\n\n\\\"}\""
                               "foo"))

(ert-deftest esh-var-test/quoted-interp-convert-cmd-multiline ()
  "Interpolate multi-line command result inside double-quotes"
  (eshell-command-result-equal "echo \"${echo \\\"foo\nbar\\\"}\""
                               "foo\nbar"))

(ert-deftest esh-var-test/quoted-interp-convert-cmd-number ()
  "Interpolate numeric command result inside double-quotes"
  (eshell-command-result-equal "echo \"${echo \\\"1\\\"}\"" "1"))

(ert-deftest esh-var-test/quoted-interp-convert-cmd-split-indices ()
  "Interpolate command result with indices inside double-quotes"
  (eshell-command-result-equal "echo \"${echo \\\"000 010 020\\\"}[0]\""
                               "000"))


;; Variable-related commands

(ert-deftest esh-var-test/set/env-var ()
  "Test that `set' with a string variable name sets an environment variable."
  (with-temp-eshell
   (eshell-match-command-output "set VAR hello" "hello\n")
   (should (equal (getenv "VAR") "hello")))
  (should-not (equal (getenv "VAR") "hello")))

(ert-deftest esh-var-test/set/symbol ()
  "Test that `set' with a symbol variable name sets a Lisp variable."
  (let (eshell-test-value)
    (eshell-command-result-equal "set #'eshell-test-value hello"
                                 "hello")
    (should (equal eshell-test-value "hello"))))

(ert-deftest esh-var-test/unset/env-var ()
  "Test that `unset' with a string variable name unsets an env var."
  (let ((process-environment (cons "VAR=value" process-environment)))
    (with-temp-eshell
     (eshell-match-command-output "unset VAR" "\\`\\'")
     (should (equal (getenv "VAR") nil)))
    (should (equal (getenv "VAR") "value"))))

(ert-deftest esh-var-test/unset/symbol ()
  "Test that `unset' with a symbol variable name unsets a Lisp variable."
  (let ((eshell-test-value "value"))
    (eshell-command-result-equal "unset #'eshell-test-value" nil)
    (should (equal eshell-test-value nil))))

(ert-deftest esh-var-test/setq ()
  "Test that `setq' sets Lisp variables."
  (let (eshell-test-value)
    (eshell-command-result-equal "setq eshell-test-value hello"
                                 "hello")
    (should (equal eshell-test-value "hello"))))

(ert-deftest esh-var-test/export ()
  "Test that `export' sets environment variables."
  (with-temp-eshell
   (eshell-match-command-output "export VAR=hello" "\\`\\'")
   (should (equal (getenv "VAR") "hello"))))

(ert-deftest esh-var-test/local-variables ()
  "Test that \"VAR=value command\" temporarily sets variables."
  (with-temp-eshell
   (push "VAR=value" process-environment)
   (eshell-match-command-output "VAR=hello env" "VAR=hello\n")
   (should (equal (getenv "VAR") "value"))))


;; Variable aliases

(ert-deftest esh-var-test/alias/function ()
  "Test using a variable alias defined as a function."
  (let ((text-quoting-style 'grave))
    (with-temp-eshell
     (push `("ALIAS" ,(lambda () "value") nil t) eshell-variable-aliases-list)
     (eshell-match-command-output "echo $ALIAS" "value\n")
     (eshell-match-command-output "set ALIAS hello"
                                  "Variable `ALIAS' is not settable\n"
                                  nil t))))

(ert-deftest esh-var-test/alias/function-pair ()
  "Test using a variable alias defined as a pair of getter/setter functions."
  (with-temp-eshell
   (let ((eshell-test-value "value"))
     (push `("ALIAS" (,(lambda () eshell-test-value)
                      . (lambda (_ value)
                          (setq eshell-test-value (upcase value))))
             nil t)
           eshell-variable-aliases-list)
     (eshell-match-command-output "echo $ALIAS" "value\n")
     (eshell-match-command-output "set ALIAS hello" "HELLO\n")
     (should (equal eshell-test-value "HELLO")))))

(ert-deftest esh-var-test/alias/string ()
  "Test using a variable alias defined as a string.
This should get/set the aliased environment variable."
  (with-temp-eshell
   (let ((eshell-test-value "lisp-value"))
     (push "eshell-test-value=env-value" process-environment)
     (push `("ALIAS" "eshell-test-value") eshell-variable-aliases-list)
     (eshell-match-command-output "echo $ALIAS" "env-value\n")
     (eshell-match-command-output "set ALIAS hello" "hello\n")
     (should (equal (getenv "eshell-test-value") "hello"))
     (should (equal eshell-test-value "lisp-value")))))

(ert-deftest esh-var-test/alias/string/prefer-lisp ()
  "Test using a variable alias defined as a string.
This sets `eshell-prefer-lisp-variables' to t and should get/set
the aliased Lisp variable."
  (with-temp-eshell
   (let ((eshell-test-value "lisp-value")
         (eshell-prefer-lisp-variables t))
     (push "eshell-test-value=env-value" process-environment)
     (push `("ALIAS" "eshell-test-value") eshell-variable-aliases-list)
     (eshell-match-command-output "echo $ALIAS" "lisp-value\n")
     (eshell-match-command-output "set ALIAS hello" "hello\n")
     (should (equal (car process-environment) "eshell-test-value=env-value"))
     (should (equal eshell-test-value "hello")))))

(ert-deftest esh-var-test/alias/symbol ()
  "Test using a variable alias defined as a symbol.
This should get/set the value bound to the symbol."
  (with-temp-eshell
   (let ((eshell-test-value "value"))
     (push '("ALIAS" eshell-test-value) eshell-variable-aliases-list)
     (eshell-match-command-output "echo $ALIAS" "value\n")
     (eshell-match-command-output "set ALIAS hello" "hello\n")
     (should (equal eshell-test-value "hello")))))

(ert-deftest esh-var-test/alias/symbol-pair ()
  "Test using a variable alias defined as a pair of symbols.
This should get the value bound to the symbol, but fail to set
it, since the setter is nil."
  (with-temp-eshell
   (let ((eshell-test-value "value")
         (text-quoting-style 'grave))
     (push '("ALIAS" (eshell-test-value . nil)) eshell-variable-aliases-list)
     (eshell-match-command-output "echo $ALIAS" "value\n")
     (eshell-match-command-output "set ALIAS hello"
                                  "Variable `ALIAS' is not settable\n"
                                  nil t))))

(ert-deftest esh-var-test/alias/export ()
  "Test that `export' properly sets variable aliases."
  (with-temp-eshell
   (let ((eshell-test-value "value"))
     (push `("ALIAS" (,(lambda () eshell-test-value)
                      . (lambda (_ value) (setq eshell-test-value value)))
             nil t)
           eshell-variable-aliases-list)
     (eshell-match-command-output "export ALIAS=hello" "\\`\\'")
     (should (equal eshell-test-value "hello")))))

(ert-deftest esh-var-test/alias/local-variables ()
  "Test that \"VAR=value cmd\" temporarily sets read-only variable aliases."
  (with-temp-eshell
   (let ((eshell-test-value "value"))
     (push `("ALIAS" ,(lambda () eshell-test-value) t t)
           eshell-variable-aliases-list)
     (eshell-match-command-output "ALIAS=hello env" "ALIAS=hello\n")
     (should (equal eshell-test-value "value")))))


;; Built-in variables

(ert-deftest esh-var-test/lines-var ()
  "$LINES should equal (window-body-height nil 'remap)"
  (eshell-command-result-equal "echo $LINES"
                               (window-body-height nil 'remap)))

(ert-deftest esh-var-test/columns-var ()
  "$COLUMNS should equal (window-body-width nil 'remap)"
  (eshell-command-result-equal "echo $COLUMNS"
                               (window-body-width nil 'remap)))

(ert-deftest esh-var-test/inside-emacs-var ()
  "Test presence of \"INSIDE_EMACS\" in subprocesses"
  (with-temp-eshell
   (eshell-match-command-output "env"
                                (format "INSIDE_EMACS=%s,eshell"
                                        emacs-version))))

(ert-deftest esh-var-test/inside-emacs-var-split-indices ()
  "Test using \"INSIDE_EMACS\" with split indices"
  (with-temp-eshell
   (eshell-match-command-output "echo $INSIDE_EMACS[, 1]"
                                "eshell")))

(ert-deftest esh-var-test/path-var/local-directory ()
  "Test using $PATH in a local directory."
  (let ((expected-path (string-join (eshell-get-path t) (path-separator))))
    (with-temp-eshell
     (eshell-match-command-output "echo $PATH" (regexp-quote expected-path)))))

(ert-deftest esh-var-test/path-var/remote-directory ()
  "Test using $PATH in a remote directory."
  (skip-unless (eshell-tests-remote-accessible-p))
  (let* ((default-directory ert-remote-temporary-file-directory)
         (expected-path (string-join (eshell-get-path t) (path-separator))))
    (with-temp-eshell
     (eshell-match-command-output "echo $PATH" (regexp-quote expected-path)))))

(ert-deftest esh-var-test/path-var/set ()
  "Test setting $PATH."
  (let* ((path-to-set-list '("/some/path" "/other/path"))
         (path-to-set (string-join path-to-set-list (path-separator))))
    (with-temp-eshell
     (eshell-match-command-output (concat "set PATH " path-to-set)
                                  (concat path-to-set "\n"))
     (eshell-match-command-output "echo $PATH" (concat path-to-set "\n"))
     (should (equal (eshell-get-path t) path-to-set-list)))))

(ert-deftest esh-var-test/path-var/set-locally ()
  "Test setting $PATH temporarily for a single command."
  (let* ((path-to-set-list '("/some/path" "/other/path"))
         (path-to-set (string-join path-to-set-list (path-separator))))
    (with-temp-eshell
     (eshell-match-command-output (concat "set PATH " path-to-set)
                                  (concat path-to-set "\n"))
     (eshell-match-command-output "PATH=/local/path env"
                                  "PATH=/local/path\n")
     ;; After the last command, the previous $PATH value should be restored.
     (eshell-match-command-output "echo $PATH" (concat path-to-set "\n"))
     (should (equal (eshell-get-path t) path-to-set-list)))))

(ert-deftest esh-var-test/path-var/preserve-across-hosts ()
  "Test that $PATH can be set independently on multiple hosts."
  (let ((local-directory default-directory)
        local-path remote-path)
    (with-temp-eshell
     ;; Set the $PATH on localhost.
     (eshell-insert-command "set PATH /local/path")
     (setq local-path (eshell-last-output))
     ;; `cd' to a remote host and set the $PATH there too.
     (eshell-insert-command
      (format "cd %s" ert-remote-temporary-file-directory))
     (eshell-insert-command "set PATH /remote/path")
     (setq remote-path (eshell-last-output))
     ;; Return to localhost and check that $PATH is the value we set
     ;; originally.
     (eshell-insert-command (format "cd %s" local-directory))
     (eshell-match-command-output "echo $PATH" (regexp-quote local-path))
     ;; ... and do the same for the remote host.
     (eshell-insert-command
      (format "cd %s" ert-remote-temporary-file-directory))
     (eshell-match-command-output "echo $PATH" (regexp-quote remote-path)))))

(ert-deftest esh-var-test/last-status-var-lisp-command ()
  "Test using the \"last exit status\" ($?) variable with a Lisp command"
  (with-temp-eshell
   (eshell-match-command-output "zerop 0; echo $?"
                                "t\n0\n")
   (eshell-match-command-output "zerop 1; echo $?"
                                "0\n")
   (eshell-match-command-output "zerop foo; echo $?"
                                "1\n" nil t)))

(ert-deftest esh-var-test/last-status-var-lisp-form ()
  "Test using the \"last exit status\" ($?) variable with a Lisp form"
  (let ((eshell-lisp-form-nil-is-failure t))
    (with-temp-eshell
     (eshell-match-command-output "(zerop 0); echo $?"
                                  "t\n0\n")
     (eshell-match-command-output "(zerop 1); echo $?"
                                  "2\n")
     (eshell-match-command-output "(zerop \"foo\"); echo $?"
                                  "1\n" nil t))))

(ert-deftest esh-var-test/last-status-var-lisp-form-2 ()
  "Test using the \"last exit status\" ($?) variable with a Lisp form.
This tests when `eshell-lisp-form-nil-is-failure' is nil."
  (let ((eshell-lisp-form-nil-is-failure nil))
    (with-temp-eshell
     (eshell-match-command-output "(zerop 0); echo $?"
                                  "0\n")
     (eshell-match-command-output "(zerop 0); echo $?"
                                  "0\n")
     (eshell-match-command-output "(zerop \"foo\"); echo $?"
                                  "1\n" nil t))))

(ert-deftest esh-var-test/last-status-var-ext-cmd ()
  "Test using the \"last exit status\" ($?) variable with an external command"
  (skip-unless (executable-find "["))
  (with-temp-eshell
   (eshell-match-command-output "[ foo = foo ]; echo $?"
                                "0\n")
   (eshell-match-command-output "[ foo = bar ]; echo $?"
                                "1\n")))

(ert-deftest esh-var-test/last-result-var ()
  "Test using the \"last result\" ($$) variable"
  (with-temp-eshell
   (eshell-match-command-output "+ 1 2; + $$ 2"
                                "3\n5\n")))

(ert-deftest esh-var-test/last-result-var-twice ()
  "Test using the \"last result\" ($$) variable twice"
  (with-temp-eshell
   (eshell-match-command-output "+ 1 2; + $$ $$"
                                "3\n6\n")))

(ert-deftest esh-var-test/last-result-var-ext-cmd ()
  "Test using the \"last result\" ($$) variable with an external command"
  (skip-unless (executable-find "["))
  (with-temp-eshell
   ;; MS-DOS/MS-Windows have an external command 'format', which we
   ;; don't want here.
   (let ((eshell-prefer-lisp-functions t))
     (eshell-match-command-output "[ foo = foo ]; format \"%s\" $$"
                                  "t\n")
     (eshell-match-command-output "[ foo = bar ]; format \"%s\" $$"
                                  "nil\n"))))

(ert-deftest esh-var-test/last-result-var-split-indices ()
  "Test using the \"last result\" ($$) variable with split indices"
  (with-temp-eshell
   (eshell-match-command-output
    "string-join (list \"01\" \"02\") :; + $$[: 1] 3"
    "01:02\n5\n")
   (eshell-match-command-output
    "string-join (list \"01\" \"02\") :; echo \"$$[: 1]\""
    "01:02\n02\n")))

(ert-deftest esh-var-test/last-arg-var ()
  "Test using the \"last arg\" ($_) variable"
  (with-temp-eshell
   (eshell-match-command-output "+ 1 2; + $_ 4"
                                "3\n6\n")))

(ert-deftest esh-var-test/last-arg-var-indices ()
  "Test using the \"last arg\" ($_) variable with indices"
  (with-temp-eshell
   (eshell-match-command-output "+ 1 2; + $_[0] 4"
                                "3\n5\n")
   (eshell-match-command-output "+ 1 2; + $_[1] 4"
                                "3\n6\n")))

(ert-deftest esh-var-test/last-arg-var-split-indices ()
  "Test using the \"last arg\" ($_) variable with split indices"
  (with-temp-eshell
   (eshell-match-command-output "concat 01:02 03:04; + $_[0][: 1] 5"
                                "01:0203:04\n7\n")
   (eshell-match-command-output "concat 01:02 03:04; echo \"$_[0][: 1]\""
                                "01:0203:04\n02\n")))

;; esh-var-tests.el ends here
