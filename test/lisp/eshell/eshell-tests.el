;;; eshell-tests.el --- Eshell test suite  -*- lexical-binding:t -*-

;; Copyright (C) 1999-2024 Free Software Foundation, Inc.

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

(ert-deftest eshell-test/eshell-command/simple ()
  "Test that the `eshell-command' function writes to the current buffer."
  (skip-unless (executable-find "echo"))
  (ert-with-temp-directory eshell-directory-name
    (let ((eshell-history-file-name nil))
      (with-temp-buffer
        (eshell-command "*echo hi" t)
        (should (equal (buffer-string) "hi\n"))))))

(ert-deftest eshell-test/eshell-command/pipeline ()
  "Test that the `eshell-command' function writes to the current buffer.
This test uses a pipeline for the command."
  (skip-unless (and (executable-find "echo")
                    (executable-find "cat")))
  (ert-with-temp-directory eshell-directory-name
    (let ((eshell-history-file-name nil))
      (with-temp-buffer
        (eshell-command "*echo hi | *cat" t)
        (should (equal (buffer-string) "hi\n"))))))

(ert-deftest eshell-test/eshell-command/background ()
  "Test that `eshell-command' works for background commands."
  (skip-unless (executable-find "echo"))
  (ert-with-temp-directory eshell-directory-name
    (let ((eshell-history-file-name nil))
      ;; XXX: We can't write to the current buffer here, since
      ;; `eshell-command' will produce an invalid command in that
      ;; case.  Just make sure the command runs and produces an output
      ;; buffer.
      (eshell-command "*echo hi &")
      (with-current-buffer "*Eshell Async Command Output*"
        (while (get-buffer-process (current-buffer))
          (accept-process-output))
        (goto-char (point-min))
        (should (looking-at "\\[echo\\(\\.exe\\)?\\(<[0-9]+>\\)?\\]"))))))

(ert-deftest eshell-test/eshell-command/background-pipeline ()
  "Test that `eshell-command' works for background commands.
This test uses a pipeline for the command."
  (skip-unless (and (executable-find "echo")
                    (executable-find "cat")))
  (ert-with-temp-directory eshell-directory-name
    (let ((eshell-history-file-name nil))
      ;; XXX: As above, we can't write to the current buffer here.
      (eshell-command "*echo hi | *cat &")
      (with-current-buffer "*Eshell Async Command Output*"
        (while (get-buffer-process (current-buffer))
          (accept-process-output))
        (goto-char (point-min))
        (should (looking-at "\\[cat\\(\\.exe\\)?\\(<[0-9]+>\\)?\\]"))))))

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
   (should (eshell-match-output "alpha\n"))))

(ert-deftest eshell-test/flush-output ()
  "Test flushing of previous output"
  (with-temp-eshell
   (eshell-insert-command "echo alpha")
   (eshell-kill-output)
   (should (eshell-match-output
            (concat "^" (regexp-quote "*** output flushed ***\n") "$")))))

(ert-deftest eshell-test/run-old-command ()
  "Re-run an old command"
  (with-temp-eshell
   (eshell-insert-command "echo alpha")
   (goto-char eshell-last-input-start)
   (string= (eshell-get-old-input) "echo alpha")))

(provide 'eshell-tests)

;;; eshell-tests.el ends here
