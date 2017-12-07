;;; tests/eshell-tests.el --- Eshell test suite

;; Copyright (C) 1999-2017 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>

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

;; Eshell test suite.

;;; Code:

(require 'ert)
(require 'eshell)

(defmacro with-temp-eshell (&rest body)
  "Evaluate BODY in a temporary Eshell buffer."
  `(let* ((eshell-directory-name (make-temp-file "eshell" t))
          (eshell-history-file-name nil)
          (eshell-buffer (eshell t)))
     (unwind-protect
         (with-current-buffer eshell-buffer
           ,@body)
       (let (kill-buffer-query-functions)
         (kill-buffer eshell-buffer)
         (delete-directory eshell-directory-name t)))))

(defun eshell-insert-command (text &optional func)
  "Insert a command at the end of the buffer."
  (goto-char eshell-last-output-end)
  (insert-and-inherit text)
  (funcall (or func 'eshell-send-input)))

(defun eshell-match-result (regexp)
  "Check that text after `eshell-last-input-end' matches REGEXP."
  (goto-char eshell-last-input-end)
  (should (string-match-p regexp (buffer-substring-no-properties
                                  (point) (point-max)))))

(defun eshell-command-result-p (text regexp &optional func)
  "Insert a command at the end of the buffer."
  (eshell-insert-command text func)
  (eshell-match-result regexp))

(defun eshell-test-command-result (command)
  "Like `eshell-command-result', but not using HOME."
  (let ((eshell-directory-name (make-temp-file "eshell" t))
        (eshell-history-file-name nil))
    (unwind-protect
        (eshell-command-result command)
      (delete-directory eshell-directory-name t))))

;;; Tests:

(ert-deftest eshell-test/simple-command-result ()
  "Test `eshell-command-result' with a simple command."
  (should (equal (eshell-test-command-result "+ 1 2") 3)))

(ert-deftest eshell-test/lisp-command ()
  "Test `eshell-command-result' with an elisp command."
  (should (equal (eshell-test-command-result "(+ 1 2)") 3)))

(ert-deftest eshell-test/for-loop ()
  "Test `eshell-command-result' with a for loop.."
  (let ((process-environment (cons "foo" process-environment)))
    (should (equal (eshell-test-command-result
                    "for foo in 5 { echo $foo }") 5))))

(ert-deftest eshell-test/for-name-loop () ;Bug#15231
  "Test `eshell-command-result' with a for loop using `name'."
  (let ((process-environment (cons "name" process-environment)))
    (should (equal (eshell-test-command-result
                    "for name in 3 { echo $name }") 3))))

(ert-deftest eshell-test/for-name-shadow-loop () ; bug#15372
  "Test `eshell-command-result' with a for loop using an env-var."
  (let ((process-environment (cons "name=env-value" process-environment)))
    (with-temp-eshell
     (eshell-command-result-p "echo $name; for name in 3 { echo $name }; echo $name"
                              "env-value\n3\nenv-value\n"))))

(ert-deftest eshell-test/lisp-command-args ()
  "Test `eshell-command-result' with elisp and trailing args.
Test that trailing arguments outside the S-expression are
ignored.  e.g. \"(+ 1 2) 3\" => 3"
  (should (equal (eshell-test-command-result "(+ 1 2) 3") 3)))

(ert-deftest eshell-test/subcommand ()
  "Test `eshell-command-result' with a simple subcommand."
  (should (equal (eshell-test-command-result "{+ 1 2}") 3)))

(ert-deftest eshell-test/subcommand-args ()
  "Test `eshell-command-result' with a subcommand and trailing args.
Test that trailing arguments outside the subcommand are ignored.
e.g. \"{+ 1 2} 3\" => 3"
  (should (equal (eshell-test-command-result "{+ 1 2} 3") 3)))

(ert-deftest eshell-test/subcommand-lisp ()
  "Test `eshell-command-result' with an elisp subcommand and trailing args.
Test that trailing arguments outside the subcommand are ignored.
e.g. \"{(+ 1 2)} 3\" => 3"
  (should (equal (eshell-test-command-result "{(+ 1 2)} 3") 3)))

(ert-deftest eshell-test/interp-cmd ()
  "Interpolate command result"
  (should (equal (eshell-test-command-result "+ ${+ 1 2} 3") 6)))

(ert-deftest eshell-test/interp-lisp ()
  "Interpolate Lisp form evaluation"
  (should (equal (eshell-test-command-result "+ $(+ 1 2) 3") 6)))

(ert-deftest eshell-test/interp-concat ()
  "Interpolate and concat command"
  (should (equal (eshell-test-command-result "+ ${+ 1 2}3 3") 36)))

(ert-deftest eshell-test/interp-concat-lisp ()
  "Interpolate and concat Lisp form"
  (should (equal (eshell-test-command-result "+ $(+ 1 2)3 3") 36)))

(ert-deftest eshell-test/interp-concat2 ()
  "Interpolate and concat two commands"
  (should (equal (eshell-test-command-result "+ ${+ 1 2}${+ 1 2} 3") 36)))

(ert-deftest eshell-test/interp-concat-lisp2 ()
  "Interpolate and concat two Lisp forms"
  (should (equal (eshell-test-command-result "+ $(+ 1 2)$(+ 1 2) 3") 36)))

(ert-deftest eshell-test/window-height ()
  "$LINES should equal (window-height)"
  (should (eshell-test-command-result "= $LINES (window-height)")))

(ert-deftest eshell-test/window-width ()
  "$COLUMNS should equal (window-width)"
  (should (eshell-test-command-result "= $COLUMNS (window-width)")))

(ert-deftest eshell-test/last-result-var ()
  "Test using the \"last result\" ($$) variable"
  (with-temp-eshell
   (eshell-command-result-p "+ 1 2; + $$ 2"
                            "3\n5\n")))

(ert-deftest eshell-test/last-result-var2 ()
  "Test using the \"last result\" ($$) variable twice"
  (with-temp-eshell
   (eshell-command-result-p "+ 1 2; + $$ $$"
                             "3\n6\n")))

(ert-deftest eshell-test/last-arg-var ()
  "Test using the \"last arg\" ($_) variable"
  (with-temp-eshell
   (eshell-command-result-p "+ 1 2; + $_ 4"
                             "3\n6\n")))

(ert-deftest eshell-test/escape-nonspecial ()
  "Test that \"\\c\" and \"c\" are equivalent when \"c\" is not a
special character."
  (with-temp-eshell
   (eshell-command-result-p "echo he\\llo"
                            "hello\n")))

(ert-deftest eshell-test/escape-nonspecial-unicode ()
  "Test that \"\\c\" and \"c\" are equivalent when \"c\" is a
unicode character (unicode characters are nonspecial by
definition)."
  (with-temp-eshell
   (eshell-command-result-p "echo Vid\\éos"
                            "Vidéos\n")))

(ert-deftest eshell-test/escape-nonspecial-quoted ()
  "Test that the backslash is preserved for escaped nonspecial
chars"
  (with-temp-eshell
   (eshell-command-result-p "echo \"h\\i\""
                            ;; Backslashes are doubled for regexp.
                            "h\\\\i\n")))

(ert-deftest eshell-test/escape-special-quoted ()
  "Test that the backslash is not preserved for escaped special
chars"
  (with-temp-eshell
   (eshell-command-result-p "echo \"h\\\\i\""
                            ;; Backslashes are doubled for regexp.
                            "h\\\\i\n")))

(ert-deftest eshell-test/command-running-p ()
  "Modeline should show no command running"
  (with-temp-eshell
   (let ((eshell-status-in-mode-line t))
     (should (memq 'eshell-command-running-string mode-line-format))
     (should (equal eshell-command-running-string "--")))))

(ert-deftest eshell-test/forward-arg ()
  "Test moving across command arguments"
  (with-temp-eshell
   (eshell-insert-command "echo $(+ 1 (- 4 3)) \"alpha beta\" file" 'ignore)
   (let ((here (point)) begin valid)
     (eshell-bol)
     (setq begin (point))
     (eshell-forward-argument 4)
     (setq valid (= here (point)))
     (eshell-backward-argument 4)
     (prog1
         (and valid (= begin (point)))
       (eshell-bol)
       (delete-region (point) (point-max))))))

(ert-deftest eshell-test/queue-input ()
  "Test queuing command input"
  (with-temp-eshell
   (eshell-insert-command "sleep 2")
   (eshell-insert-command "echo alpha" 'eshell-queue-input)
   (let ((count 10))
     (while (and eshell-current-command
                 (> count 0))
       (sit-for 1)
       (setq count (1- count))))
   (eshell-match-result "alpha\n")))

(ert-deftest eshell-test/flush-output ()
  "Test flushing of previous output"
  (with-temp-eshell
   (eshell-insert-command "echo alpha")
   (eshell-kill-output)
   (eshell-match-result (regexp-quote "*** output flushed ***\n"))
   (should (forward-line))
   (should (= (point) eshell-last-output-start))))

(ert-deftest eshell-test/run-old-command ()
  "Re-run an old command"
  (with-temp-eshell
   (eshell-insert-command "echo alpha")
   (goto-char eshell-last-input-start)
   (string= (eshell-get-old-input) "echo alpha")))

(provide 'eshell-tests)

;;; tests/eshell-tests.el ends here
