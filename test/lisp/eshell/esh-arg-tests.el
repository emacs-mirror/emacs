;;; esh-arg-tests.el --- esh-arg test suite  -*- lexical-binding:t -*-

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

;; Tests for Eshell's argument handling.

;;; Code:

(require 'ert)
(require 'esh-mode)
(require 'eshell)
(require 'ert-x)

(require 'eshell-tests-helpers
         (ert-resource-file "eshell-tests-helpers"))

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
  "Test that an escaped newline is equivalent to the empty string."
  (with-temp-eshell
   (eshell-match-command-output "echo hi\\\nthere"
                                "hithere\n")))

(ert-deftest esh-arg-test/escape/trailing-newline ()
  "Test that an escaped newline is equivalent to the empty string."
  (with-temp-eshell
   (eshell-match-command-output "echo hi\\\n"
                                "hi\n")))

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
  "Test that an escaped newline is equivalent to the empty string."
  (with-temp-eshell
   (eshell-match-command-output "echo \"hi\\\nthere\""
                                "hithere\n")))

(ert-deftest esh-arg-test/special-reference/default ()
  "Test that \"#<buf>\" refers to the buffer \"buf\"."
  (with-temp-buffer
    (rename-buffer "my-buffer" t)
    (eshell-command-result-equal
     (format "echo #<%s>" (buffer-name))
     (current-buffer))))

(ert-deftest esh-arg-test/special-reference/buffer ()
  "Test that \"#<buffer buf>\" refers to the buffer \"buf\"."
  (with-temp-buffer
    (rename-buffer "my-buffer" t)
    (eshell-command-result-equal
     (format "echo #<buffer %s>" (buffer-name))
     (current-buffer))))

(ert-deftest esh-arg-test/special-reference/marker ()
  "Test that \"#<marker N buf>\" refers to a marker in the buffer \"buf\"."
  (with-temp-buffer
    (rename-buffer "my-buffer" t)
    (insert "hello")
    (let ((marker (make-marker)))
      (set-marker marker 1 (current-buffer))
      (eshell-command-result-equal
       (format "echo #<marker 1 %s>" (buffer-name))
       marker))))

(ert-deftest esh-arg-test/special-reference/quoted ()
  "Test that '#<buffer \"foo bar\">' refers to the buffer \"foo bar\"."
  (with-temp-buffer
    (rename-buffer "foo bar" t)
    (eshell-command-result-equal
     (format "echo #<buffer \"%s\">" (buffer-name))
     (current-buffer))
    (eshell-command-result-equal
     (format "echo #<buffer '%s'>" (buffer-name))
     (current-buffer))))

(ert-deftest esh-arg-test/special-reference/nested ()
  "Test that nested special references work correctly."
  (with-temp-buffer
    (rename-buffer "my-buffer" t)
    (insert "hello")
    (let ((marker (make-marker)))
      (set-marker marker 1 (current-buffer))
      (eshell-command-result-equal
       (format "echo #<marker 1 #<%s>>" (buffer-name))
       marker)
      (eshell-command-result-equal
       (format "echo #<marker 1 #<buffer %s>>" (buffer-name))
       marker))))

(ert-deftest esh-arg-test/special-reference/var-expansion ()
  "Test that variable expansion inside special references works."
  (with-temp-buffer
    (rename-buffer "my-buffer" t)
    (let ((eshell-test-value (buffer-name)))
      (eshell-command-result-equal
       "echo #<buffer $eshell-test-value>"
       (current-buffer))
      (eshell-command-result-equal
       "echo #<buffer \"$eshell-test-value\">"
       (current-buffer)))))

(ert-deftest esh-arg-test/special-reference/lisp-form ()
  "Test that Lisp forms inside special references work."
  (with-temp-eshell
   (let ((marker (make-marker))
         eshell-test-value)
     (set-marker marker 1 (current-buffer))
     (eshell-insert-command
      "setq eshell-test-value #<marker 1 (current-buffer)>")
     (should (equal eshell-test-value marker))
     (eshell-insert-command
      "setq eshell-test-value #<marker 1 #<buffer (buffer-name)>>")
     (should (equal eshell-test-value marker)))))

(ert-deftest esh-arg-test/special-reference/command-form ()
  "Test that command forms inside special references work."
  (with-temp-eshell
   (let ((marker (make-marker))
         eshell-test-value)
     (set-marker marker 1 (current-buffer))
     (eshell-insert-command
      "setq eshell-test-value #<marker 1 {current-buffer}>")
     (should (equal eshell-test-value marker))
     (eshell-insert-command
      "setq eshell-test-value #<marker 1 #<buffer {buffer-name}>>")
     (should (equal eshell-test-value marker)))))

(ert-deftest esh-arg-test/special-reference/special-characters ()
  "Test that \"#<...>\" works correctly when escaping special characters."
  (with-temp-buffer
    (rename-buffer "<my buffer>" t)
    (let ((escaped-bufname (replace-regexp-in-string
                            (rx (group (or "\\" "<" ">" space))) "\\\\\\1"
                            (buffer-name))))
      (eshell-command-result-equal
       (format "echo #<%s>" escaped-bufname)
       (current-buffer))
      (eshell-command-result-equal
       (format "echo #<buffer %s>" escaped-bufname)
       (current-buffer)))))

;; esh-arg-tests.el ends here
