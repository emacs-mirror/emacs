;;; em-alias-tests.el --- em-alias test suite  -*- lexical-binding:t -*-

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

;; Tests for Eshell's alias module.

;;; Code:

(require 'ert)
(require 'esh-mode)
(require 'eshell)
(require 'em-alias)

(require 'eshell-tests-helpers
         (expand-file-name "eshell-tests-helpers"
                           (file-name-directory (or load-file-name
                                                    default-directory))))
;;; Tests:

(ert-deftest em-alias-test/simple-alias ()
  "Test a simple alias with no arguments"
  (with-temp-eshell
   (eshell-insert-command "alias say-hi 'echo hi'")
   (eshell-match-command-output "say-hi" "hi\n")
   (eshell-match-command-output "say-hi bye" "hi\n")))

(ert-deftest em-alias-test/alias-arg-vars ()
  "Test alias with $0, $1, ... variables"
  (with-temp-eshell
   (eshell-insert-command "alias show-args 'printnl $0 \"$1 $2\"'")
   (eshell-match-command-output "show-args one two" "show-args\none two\n")))

(ert-deftest em-alias-test/alias-arg-vars-indices ()
  "Test alias with $1, $2, ... variables using indices"
  (with-temp-eshell
   (eshell-insert-command "alias funny-sum '+ $1[0] $2[1]'")
   (eshell-match-command-output "funny-sum (list 1 2) (list 3 4)"
                                "5\n")))

(ert-deftest em-alias-test/alias-arg-vars-split-indices ()
  "Test alias with $0, $1, ... variables using split indices"
  (with-temp-eshell
   (eshell-insert-command "alias my-prefix 'echo $0[- 0]'")
   (eshell-match-command-output "my-prefix"
                                "my\n")
   (eshell-insert-command "alias funny-sum '+ $1[: 0] $2[: 1]'")
   (eshell-match-command-output "funny-sum 1:2 3:4"
                                "5\n")))

(ert-deftest em-alias-test/alias-all-args-var ()
  "Test alias with the $* variable"
  (with-temp-eshell
   (eshell-insert-command "alias show-all-args 'printnl $*'")
   (eshell-match-command-output "show-all-args" "\\`\\'")
   (eshell-match-command-output "show-all-args a" "a\n")
   (eshell-match-command-output "show-all-args a b c" "a\nb\nc\n")))

(ert-deftest em-alias-test/alias-all-args-var-indices ()
  "Test alias with the $* variable using indices"
  (with-temp-eshell
   (eshell-insert-command "alias add-pair '+ $*[0] $*[1]'")
   (eshell-match-command-output "add-pair 1 2" "3\n")))

(ert-deftest em-alias-test/alias-all-args-var-split-indices ()
  "Test alias with the $* variable using split indices"
  (with-temp-eshell
   (eshell-insert-command "alias add-funny-pair '+ $*[0][: 0] $*[1][: 1]'")
   (eshell-match-command-output "add-funny-pair 1:2 3:4" "5\n")))

;; em-alias-tests.el ends here
