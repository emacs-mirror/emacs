;;; esh-arg-tests.el --- esh-arg test suite  -*- lexical-binding:t -*-

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

;; Tests for Eshell's argument handling.

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

(ert-deftest esh-arg-test/escape/nonspecial ()
  "Test that \"\\c\" and \"c\" are equivalent when \"c\" is not a
special character."
  (with-temp-eshell
   (eshell-match-command-output "echo he\\llo"
                                "hello\n")))

(ert-deftest esh-arg-test/escape/nonspecial-unicode ()
  "Test that \"\\c\" and \"c\" are equivalent when \"c\" is a
unicode character (unicode characters are nonspecial by
definition)."
  (with-temp-eshell
   (eshell-match-command-output "echo Vid\\éos"
                                "Vidéos\n")))

(ert-deftest esh-arg-test/escape/special ()
  "Test that the backslash is not preserved for escaped special
chars."
  (with-temp-eshell
   (eshell-match-command-output "echo he\\\\llo"
                                ;; Backslashes are doubled for regexp.
                                "he\\\\llo\n")))

(ert-deftest esh-arg-test/escape/newline ()
  "Test that an escaped newline is equivalent to the empty string.
When newlines are *nonspecial*, an escaped newline should be
treated as just a newline."
  (with-temp-eshell
   (eshell-match-command-output "echo hi\\\nthere"
                                "hithere\n")))

(ert-deftest esh-arg-test/escape/newline-conditional ()
  "Test invocation of an if/else statement using line continuations."
  (let ((eshell-test-value t))
    (eshell-command-result-equal
     "if $eshell-test-value \\\n{echo yes} \\\n{echo no}"
     "yes"))
  (let ((eshell-test-value nil))
    (eshell-command-result-equal
     "if $eshell-test-value \\\n{echo yes} \\\n{echo no}"
     "no")))

(ert-deftest esh-arg-test/escape-quoted/nonspecial ()
  "Test that the backslash is preserved for escaped nonspecial
chars."
  (with-temp-eshell
   (eshell-match-command-output "echo \"h\\i\""
                                ;; Backslashes are doubled for regexp.
                                "h\\\\i\n")))

(ert-deftest esh-arg-test/escape-quoted/special ()
  "Test that the backslash is not preserved for escaped special
chars."
  (with-temp-eshell
   (eshell-match-command-output "echo \"\\\"hi\\\\\""
                                ;; Backslashes are doubled for regexp.
                                "\\\"hi\\\\\n")))

(ert-deftest esh-arg-test/escape-quoted/newline ()
  "Test that an escaped newline is equivalent to the empty string.
When newlines are *nonspecial*, an escaped newline should be
treated literally, as a backslash and a newline."
  (with-temp-eshell
   (eshell-match-command-output "echo \"hi\\\nthere\""
                                "hithere\n")))

;; esh-arg-tests.el ends here
