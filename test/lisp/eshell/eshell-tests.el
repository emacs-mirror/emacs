;;; eshell-tests.el --- Eshell test suite  -*- lexical-binding:t -*-

;; Copyright (C) 1999-2023 Free Software Foundation, Inc.

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
(require 'ert-x)
(require 'esh-mode)
(require 'eshell)
(require 'eshell-tests-helpers
         (expand-file-name "eshell-tests-helpers"
                           (file-name-directory (or load-file-name
                                                    default-directory))))

(defvar eshell-test-value nil)

;;; Tests:

(ert-deftest eshell-test/pipe-headproc ()
  "Check that piping a non-process to a process command waits for the process"
  (skip-unless (executable-find "cat"))
  (with-temp-eshell
   (eshell-match-command-output "echo hi | *cat"
                                "hi")))

(ert-deftest eshell-test/pipe-tailproc ()
  "Check that piping a process to a non-process command waits for the process"
  (skip-unless (executable-find "echo"))
  (with-temp-eshell
   (eshell-match-command-output "*echo hi | echo bye"
                                "bye\nhi\n")))

(ert-deftest eshell-test/pipe-headproc-stdin ()
  "Check that standard input is sent to the head process in a pipeline"
  (skip-unless (and (executable-find "tr")
                    (executable-find "rev")))
  (with-temp-eshell
   (eshell-insert-command "tr a-z A-Z | rev")
   (eshell-insert-command "hello")
   (eshell-send-eof-to-process)
   (eshell-wait-for-subprocess)
   (should (eshell-match-output "OLLEH\n"))))

(ert-deftest eshell-test/pipe-subcommand ()
  "Check that piping with an asynchronous subcommand works"
  (skip-unless (and (executable-find "echo")
                    (executable-find "cat")))
  (with-temp-eshell
   (eshell-match-command-output "echo ${*echo hi} | *cat"
                                "hi")))

(ert-deftest eshell-test/pipe-subcommand-with-pipe ()
  "Check that piping with an asynchronous subcommand with its own pipe works"
  (skip-unless (and (executable-find "echo")
                    (executable-find "cat")))
  (with-temp-eshell
   (eshell-match-command-output "echo ${*echo hi | *cat} | *cat"
                                "hi")))

(ert-deftest eshell-test/subcommand-reset-in-pipeline ()
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

(ert-deftest eshell-test/lisp-reset-in-pipeline ()
  "Check that interpolated Lisp forms reset `eshell-in-pipeline-p'."
  (skip-unless (executable-find "cat"))
  (dolist (template '("echo (%s) | *cat"
                      "echo $(%s) | *cat"))
    (eshell-command-result-equal
     (format template "format \"%s\" eshell-in-pipeline-p")
     "nil")))

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
     (beginning-of-line)
     (setq begin (point))
     (eshell-forward-argument 4)
     (setq valid (= here (point)))
     (eshell-backward-argument 4)
     (prog1
         (and valid (= begin (point)))
       (beginning-of-line)
       (delete-region (point) (point-max))))))

(ert-deftest eshell-test/queue-input ()
  "Test queuing command input.
This should let the current command finish, then automatically
insert the queued one at the next prompt, and finally run it."
  (with-temp-eshell
   (eshell-insert-command "sleep 1; echo slept")
   (eshell-insert-command "echo alpha" #'eshell-queue-input)
   (let ((start (marker-position (eshell-beginning-of-output))))
     (eshell-wait-for (lambda () (not eshell-current-command)))
     (should (string-match "^slept\n.*echo alpha\nalpha\n$"
                           (buffer-substring-no-properties
                            start (eshell-end-of-output)))))))

(ert-deftest eshell-test/flush-output ()
  "Test flushing of previous output"
  (with-temp-eshell
   (eshell-insert-command "echo alpha")
   (eshell-kill-output)
   (should (eshell-match-output
            (concat "^" (regexp-quote "*** output flushed ***\n") "$")))))

(ert-deftest eshell-test/get-old-input ()
  "Test that we can get the input of a previous command."
  (with-temp-eshell
   (eshell-insert-command "echo alpha")
   (goto-char eshell-last-input-start)
   (should (string= (eshell-get-old-input) "echo alpha"))
   ;; Make sure that `eshell-get-old-input' works even if the point is
   ;; inside the prompt.
   (let ((inhibit-field-text-motion t))
     (beginning-of-line))
   (should (string= (eshell-get-old-input) "echo alpha"))))

(ert-deftest eshell-test/get-old-input/rerun-command ()
  "Test that we can rerun an old command when point is on it."
  (with-temp-eshell
   (let ((eshell-test-value "first"))
     (eshell-match-command-output "echo $eshell-test-value" "first"))
   ;; Go to the previous prompt.
   (forward-line -2)
   (let ((inhibit-field-text-motion t))
     (end-of-line))
   ;; Rerun the command, but with a different variable value.
   (let ((eshell-test-value "second"))
     (eshell-send-input))
   (eshell-match-output "second")))

(ert-deftest eshell-test/get-old-input/run-output ()
  "Test that we can run a line of output as a command when point is on it."
  (with-temp-eshell
   (eshell-match-command-output "echo \"echo there\"" "echo there")
   ;; Go to the output, and insert "hello" after "echo".
   (forward-line -1)
   (forward-word)
   (insert " hello")
   ;; Run the line as a command.
   (eshell-send-input)
   (eshell-match-output "(\"hello\" \"there\")")))

(provide 'eshell-tests)

;;; eshell-tests.el ends here
