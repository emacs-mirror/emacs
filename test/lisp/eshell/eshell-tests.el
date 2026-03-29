;;; eshell-tests.el --- Eshell test suite  -*- lexical-binding:t -*-

;; Copyright (C) 1999-2026 Free Software Foundation, Inc.

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
(require 'ert-x)
(require 'eshell)
(require 'eshell-tests-helpers
         (ert-resource-file "eshell-tests-helpers"))

(defvar eshell-test-value nil)
(defvar eshell-command-output)

;;; Tests:

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

(ert-deftest eshell-test/eshell-command/pipeline-wait ()
  "Check that `eshell-command' waits for all its processes before returning."
  (skip-unless (and (executable-find "echo")
                    (executable-find "sh")
                    (executable-find "rev")))
  (ert-with-temp-directory eshell-directory-name
    (let ((eshell-history-file-name nil))
      (with-temp-buffer
        (eshell-command
         "*echo hello | sh -c 'sleep 1; rev' 1>&2 | *echo goodbye" t)
        (should (equal (buffer-string) "goodbye\nolleh\n"))))))

(ert-deftest eshell-test/eshell-command/background ()
  "Test that `eshell-command' works for background commands."
  (skip-unless (executable-find "echo"))
  (ert-with-temp-directory eshell-directory-name
    (let ((orig-processes (process-list))
          (eshell-history-file-name nil))
      (with-temp-buffer
        (eshell-command "*echo hi &" t)
        (eshell-wait-for (lambda () (equal (process-list) orig-processes)))
        (should (equal (buffer-string) "hi\n"))))))

(ert-deftest eshell-test/eshell-command/background-pipeline ()
  "Test that `eshell-command' works for background commands.
This test uses a pipeline for the command."
  (skip-unless (and (executable-find "echo")
                    (executable-find "cat")))
  (ert-with-temp-directory eshell-directory-name
    (let ((orig-processes (process-list))
          (eshell-history-file-name nil))
      (with-temp-buffer
        (eshell-command "*echo hi | *cat &" t)
        (eshell-wait-for (lambda () (equal (process-list) orig-processes)))
        (should (equal (buffer-string) "hi\n"))))))

(ert-deftest eshell-test/eshell-command/output-buffer/sync ()
  "Test that the `eshell-command' function writes to its output buffer."
  (skip-unless (executable-find "echo"))
  (ert-with-temp-directory eshell-directory-name
    (let ((eshell-history-file-name nil))
      (eshell-command "*echo 'hi\nbye'")
      (with-current-buffer "*Eshell Command Output*"
        (should (equal (buffer-string) "hi\nbye")))
      (kill-buffer "*Eshell Command Output*"))))

(ert-deftest eshell-test/eshell-command/output-buffer/async ()
  "Test that the `eshell-command' function writes to its async output buffer."
  (skip-unless (executable-find "echo"))
  (ert-with-temp-directory eshell-directory-name
    (let ((orig-processes (process-list))
          (eshell-history-file-name nil))
      (eshell-command "*echo hi &")
      (eshell-wait-for (lambda () (equal (process-list) orig-processes)))
      (with-current-buffer "*Eshell Async Command Output*"
        (goto-char (point-min))
        (forward-line)
        (should (looking-at "hi\n"))))))

(ert-deftest eshell-test/eshell-command/output-buffer/async-kill ()
  "Test that the `eshell-command' function kills the old process when told to."
  (skip-unless (executable-find "echo"))
  (ert-with-temp-directory eshell-directory-name
    (let ((orig-processes (process-list))
          (eshell-history-file-name nil)
          (eshell-command-async-buffer 'confirm-kill-process))
      (eshell-command "sleep 5 | *echo hi &")
      (cl-letf* ((result t)
                 ;; Say "yes" only once: for the `confirm-kill-process'
                 ;; prompt.  If there are any other prompts (e.g. from
                 ;; `kill-buffer'), say "no" to make the test fail.
                 ((symbol-function 'yes-or-no-p)
                  (lambda (_prompt) (prog1 result (setq result nil)))))
        (eshell-command "*echo bye &"))
      (eshell-wait-for (lambda () (equal (process-list) orig-processes)))
      (with-current-buffer "*Eshell Async Command Output*"
        (goto-char (point-min))
        (forward-line)
        (should (looking-at "bye\n"))))))

(ert-deftest eshell-test/eshell-command/output-file ()
  "Test that `eshell-command' can write to a file."
  (ert-with-temp-file temp-file :text "initial"
    (eshell-command "echo more" temp-file)
    (should (equal (eshell-test-file-string temp-file) "moreinitial"))))

(ert-deftest eshell-test/eshell-command/output-symbol ()
  "Test that `eshell-command' can write to a symbol."
  (eshell-command "echo hi" 'eshell-command-output)
  (should (equal eshell-command-output "hi")))

(ert-deftest eshell-test/eshell-command/output-dev-null ()
  "Test that the `eshell-command' function handles /dev/null properly."
  (ert-with-temp-directory eshell-directory-name
    (let ((eshell-history-file-name nil))
      (with-temp-buffer
        (eshell-command "echo hi" "/dev/null")
        (should (equal (buffer-string) ""))))))

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
   (let ((end (point)) begin)
     (beginning-of-line)
     (setq begin (point))
     (eshell-forward-argument 4)
     (should (= end (point)))
     (eshell-backward-argument 4)
     (should (= begin (point))))))

(ert-deftest eshell-test/queue-input ()
  "Test queuing command input.
This should let the current command finish, then automatically
insert the queued one at the next prompt, and finally run it."
  (with-temp-eshell
   (eshell-insert-command "sleep 1; echo slept")
   (eshell-insert-command "echo alpha" #'eshell-queue-input)
   (let ((start (marker-position (eshell-beginning-of-output))))
     (eshell-wait-for (lambda () (not eshell-foreground-command)))
     (should (string-match "^slept\n.*echo alpha\nalpha\n$"
                           (buffer-substring-no-properties
                            start (eshell-end-of-output)))))))

(ert-deftest eshell-test/flush-output ()
  "Test flushing of previous output"
  (with-temp-eshell
   (eshell-insert-command "echo alpha")
   (eshell-delete-output)
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

(ert-deftest eshell-test/yank-output ()
  "Test that yanking a line of output into the next prompt works (bug#66469)."
  (with-temp-eshell
   (eshell-insert-command "echo hello")
   ;; Go to the output and kill the line of text.
   (forward-line -1)
   (kill-line)
   ;; Go to the last prompt and yank the previous output.
   (goto-char (point-max))
   (yank)
   ;; Go to the beginning of the prompt and add some text.
   (move-beginning-of-line 1)
   (insert-and-inherit "echo ")
   ;; Make sure when we go to the beginning of the line, we go to the
   ;; right spot (before the "echo").
   (move-end-of-line 1)
   (move-beginning-of-line 1)
   (should (looking-at "echo hello"))))

(provide 'eshell-tests)

;;; eshell-tests.el ends here
