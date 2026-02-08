;;; esh-proc-tests.el --- esh-proc test suite  -*- lexical-binding:t -*-

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

;;; Code:

(require 'tramp)
(require 'ert)
(require 'esh-mode)
(require 'eshell)
(require 'em-prompt)                    ; For `eshell-previous-prompt'
(require 'ert-x)

(require 'eshell-tests-helpers
         (ert-resource-file "eshell-tests-helpers"))

(defvar esh-proc-test--output-cmd
  (concat "sh -c '"
          "echo stdout; "
          "echo stderr >&2"
          "'")
  "A shell command that prints to both stdout and stderr.")

(defvar esh-proc-test--detect-pty-cmd
  (concat "sh -c '"
          "if [ -t 0 ]; then echo stdin; fi; "
          "if [ -t 1 ]; then echo stdout; fi; "
          "if [ -t 2 ]; then echo stderr; fi"
          "'")
  "A shell command that prints the standard streams connected as TTYs.")

(defvar eshell-test-value nil)

;;; Tests:


;; Output and redirection

(ert-deftest esh-proc-test/output/to-screen ()
  "Check that outputting stdout and stderr to the screen works."
  (skip-unless (executable-find "sh"))
  (with-temp-eshell
   (eshell-match-command-output esh-proc-test--output-cmd
                                "stdout\nstderr\n")))

(ert-deftest esh-proc-test/output/stdout-to-buffer ()
  "Check that redirecting only stdout works."
  (skip-unless (executable-find "sh"))
  (eshell-with-temp-buffer bufname "old"
    (with-temp-eshell
     (eshell-match-command-output
      (format "%s > #<%s>" esh-proc-test--output-cmd bufname)
      "stderr\n"))
    (should (equal (buffer-string) "stdout\n"))))

(ert-deftest esh-proc-test/output/stderr-to-buffer ()
  "Check that redirecting only stderr works."
  (skip-unless (executable-find "sh"))
  (eshell-with-temp-buffer bufname "old"
    (with-temp-eshell
     (eshell-match-command-output
      (format "%s 2> #<%s>" esh-proc-test--output-cmd bufname)
      "stdout\n"))
    (should (equal (buffer-string) "stderr\n"))))

(ert-deftest esh-proc-test/output/stdout-and-stderr-to-buffer ()
  "Check that redirecting stdout and stderr works."
  (skip-unless (executable-find "sh"))
  (eshell-with-temp-buffer bufname "old"
    (with-temp-eshell
     (eshell-match-command-output
      (format "%s &> #<%s>" esh-proc-test--output-cmd bufname)
      "\\`\\'"))
    (should (equal (buffer-string) "stdout\nstderr\n"))))

(ert-deftest esh-proc-test/output/remote-redirect ()
  "Check that redirecting stdout for a remote process works."
  (skip-unless (and (eshell-tests-remote-accessible-p)
                    (executable-find "echo")))
  (let ((default-directory ert-remote-temporary-file-directory))
    (eshell-with-temp-buffer bufname "old"
      (with-temp-eshell
       (eshell-match-command-output
        (format "*echo hello > #<%s>" bufname)
        "\\`\\'"))
      (should (equal (buffer-string) "hello\n")))))


;; Exit status

(ert-deftest esh-proc-test/exit-status/success ()
  "Check that successful execution is properly recorded."
  (skip-unless (executable-find "sh"))
  (with-temp-eshell
   (eshell-insert-command "sh -c 'exit 0'")
   (eshell-wait-for-subprocess)
   (should (= eshell-last-command-status 0))
   (should (eq eshell-last-command-result t))))

(ert-deftest esh-proc-test/exit-status/failure ()
  "Check that failed execution is properly recorded."
  (skip-unless (executable-find "sh"))
  (with-temp-eshell
   (eshell-insert-command "sh -c 'exit 1'")
   (eshell-wait-for-subprocess)
   (should (= eshell-last-command-status 1))
   (should (eq eshell-last-command-result nil))))

(ert-deftest esh-proc-test/exit-status/with-stderr-pipe ()
  "Check that failed execution is properly recorded even with a pipe process."
  (skip-unless (executable-find "sh"))
  (eshell-with-temp-buffer bufname "old"
    (with-temp-eshell
     (eshell-insert-command (format "sh -c 'exit 1' > #<%s>" bufname))
     (eshell-wait-for-subprocess)
     (should (= eshell-last-command-status 1))
     (should (eq eshell-last-command-result nil)))))

(ert-deftest esh-proc-test/sentinel/change-buffer ()
  "Check that changing the current buffer while running a command works.
See bug#71778."
  (let ((starting-process-list (process-list)))
    (eshell-with-temp-buffer bufname ""
      (with-temp-eshell
        (let (eshell-test-value)
          (eshell-insert-command
           (concat (format "for i in 1 2 {sleep 1; echo hello} > #<%s>; "
                           bufname)
                   "setq eshell-test-value t"))
          (with-current-buffer bufname
            (eshell-wait-for (lambda () eshell-test-value))
            (should (equal (buffer-string) "hellohello")))
          (should (equal (process-list) starting-process-list))
          (eshell-match-command-output "echo goodbye" "\\`goodbye\n"))))))


;; Pipelines

(ert-deftest esh-proc-test/sigpipe-exits-process ()
  "Test that a SIGPIPE is properly sent to a process if a pipe closes"
  (skip-unless (and (executable-find "sh")
                    (executable-find "echo")
                    (executable-find "sleep")
                    (not (eq system-type 'windows-nt))))
  (let ((starting-process-list (process-list)))
    (with-temp-eshell
     (eshell-match-command-output
      ;; The first command is like `yes' but slower.  This is to prevent
      ;; it from taxing Emacs's process filter too much and causing a
      ;; hang.  Note that we use "|&" to connect the processes so that
      ;; Emacs doesn't create an extra pipe process for the first "sh"
      ;; invocation.
      (concat "sh -c 'while true; do echo y; sleep 1; done' |& "
              "sh -c 'read NAME; echo ${NAME}'")
      "y\n")
     (eshell-wait-for-subprocess t)
     (should (equal (process-list) starting-process-list))
     ;; Make sure the exit status is from the last command in the
     ;; pipeline.
     (should (= eshell-last-command-status 0)))))

(ert-deftest esh-proc-test/pipeline-connection-type/no-pipeline ()
  "Test that all streams are PTYs when a command is not in a pipeline."
  (skip-unless (executable-find "sh"))
  (eshell-command-result-equal
   esh-proc-test--detect-pty-cmd
   ;; PTYs aren't supported on MS-Windows.
   (unless (eq system-type 'windows-nt)
     "stdin\nstdout\nstderr\n")))

(ert-deftest esh-proc-test/pipeline-connection-type/first ()
  "Test that only stdin is a PTY when a command starts a pipeline."
  (skip-unless (and (executable-find "sh")
                    (executable-find "cat")))
  (eshell-command-result-equal
   (concat esh-proc-test--detect-pty-cmd " | cat")
   (unless (eq system-type 'windows-nt)
     "stdin\n")))

(ert-deftest esh-proc-test/pipeline-connection-type/middle ()
  "Test that all streams are pipes when a command is in the middle of a
pipeline."
  (skip-unless (and (executable-find "sh")
                    (executable-find "cat")))
  (eshell-command-result-equal
   (concat "(ignore) | " esh-proc-test--detect-pty-cmd " | cat")
   nil))

(ert-deftest esh-proc-test/pipeline-connection-type/last ()
  "Test that only output streams are PTYs when a command ends a pipeline."
  (skip-unless (executable-find "sh"))
  (eshell-command-result-equal
   (concat "(ignore) | " esh-proc-test--detect-pty-cmd)
   (unless (eq system-type 'windows-nt)
     "stdout\nstderr\n")))


;; Synchronous processes

;; These tests check that synchronous subprocesses (only used on
;; MS-DOS by default) work correctly.  To help them run on MS-DOS as
;; well, we use the Emacs executable as our subprocess to test
;; against; that way, users don't need to have GNU coreutils (or
;; similar) installed.

(defsubst esh-proc-test/emacs-command (command)
  "Evaluate COMMAND in a new Emacs batch instance."
  ;; Call `eshell-quote-argument' from within an Eshell buffer since its
  ;; behavior depends on activating various Eshell modules.
  (mapconcat (lambda (arg) (with-temp-eshell (eshell-quote-argument arg)))
             `(,(expand-file-name invocation-name invocation-directory)
               "-Q" "--batch" "--eval" ,(prin1-to-string command))
             " "))

(defvar esh-proc-test/emacs-echo
  (esh-proc-test/emacs-command '(princ "hello\n"))
  "A command that prints \"hello\" to stdout using Emacs.")

(defvar esh-proc-test/emacs-upcase
  (esh-proc-test/emacs-command
   '(princ (upcase (concat (read-string "") "\n"))))
  "A command that upcases the text from stdin using Emacs.")

(ert-deftest esh-proc-test/synchronous-proc/simple/interactive ()
  "Test that synchronous processes work in an interactive shell."
  (let ((eshell-supports-asynchronous-processes nil))
    (with-temp-eshell
     (eshell-match-command-output esh-proc-test/emacs-echo
                                  "\\`hello\n"))))

(ert-deftest esh-proc-test/synchronous-proc/simple/command-result ()
  "Test that synchronous processes work via `eshell-command-result'."
  (let ((eshell-supports-asynchronous-processes nil))
    (eshell-command-result-equal esh-proc-test/emacs-echo
                                 "hello\n")))

(ert-deftest esh-proc-test/synchronous-proc/pipeline/interactive ()
  "Test that synchronous pipelines work in an interactive shell."
  (let ((eshell-supports-asynchronous-processes nil))
    (with-temp-eshell
     (eshell-match-command-output (concat esh-proc-test/emacs-echo " | "
                                          esh-proc-test/emacs-upcase)
                                  "\\`HELLO\n"))))

(ert-deftest esh-proc-test/synchronous-proc/pipeline/command-result ()
  "Test that synchronous pipelines work via `eshell-command-result'."
  (let ((eshell-supports-asynchronous-processes nil))
    (eshell-command-result-equal (concat esh-proc-test/emacs-echo " | "
                                          esh-proc-test/emacs-upcase)
                                 "HELLO\n")))


;; Killing processes

(ert-deftest esh-proc-test/kill-process/foreground-only ()
  "Test that `eshell-kill-process' only kills foreground processes."
  (with-temp-eshell
   (eshell-insert-command "sleep 100 &")
   (eshell-insert-command "sleep 100")
   (should (equal (length eshell-process-list) 2))
   ;; This should kill only the foreground process.
   (eshell-kill-process)
   (eshell-wait-for-subprocess)
   (should (equal (length eshell-process-list) 1))
   ;; Now kill everything, including the background process.
   (eshell-process-interact 'kill-process t)
   (eshell-wait-for-subprocess t)
   (should (equal (length eshell-process-list) 0))))

(ert-deftest esh-proc-test/kill-process/background-prompt ()
  "Test that killing a background process doesn't emit a new
prompt.  See bug#54136."
  (skip-unless (and (executable-find "sh")
                    (executable-find "sleep")))
  (with-temp-eshell
   (eshell-insert-command "sh -c 'while true; do sleep 1; done' &")
   (kill-process (caar eshell-process-list))
   (eshell-wait-for-subprocess)
   (should (eshell-match-output "\\[sh\\(\\.exe\\)?\\] [[:digit:]]+\n"))))

(ert-deftest esh-proc-test/kill-process/redirect-message ()
  "Test that killing a process with a redirected stderr omits the exit status."
  (skip-unless (executable-find "sleep"))
  (eshell-with-temp-buffer bufname ""
    (with-temp-eshell
     (eshell-insert-command (format "sleep 100 2> #<buffer %s>" bufname))
     (kill-process (eshell-head-process)))
    (should (equal (buffer-string) ""))))

(ert-deftest esh-proc-test/kill-pipeline ()
  "Test that killing a pipeline of processes only emits a single
prompt.  See bug#54136."
  (skip-unless (and (executable-find "sh")
                    (executable-find "echo")
                    (executable-find "sleep")))
  ;; This test doesn't work on EMBA with AOT nativecomp, but works
  ;; fine elsewhere.
  (skip-when (getenv "EMACS_EMBA_CI"))
  (with-temp-eshell
    (ert-info (#'eshell-get-debug-logs :prefix "Command logs: ")
      (eshell-insert-command
       (concat "sh -c 'while true; do echo y; sleep 1; done' | "
               "sh -c 'while true; do read NAME; done'"))
      (let ((output-start (eshell-beginning-of-output)))
        (eshell-kill-process)
        (eshell-wait-for-subprocess t)
        ;; We expect at most one exit message here (from the tail
        ;; process).  If the tail process has time to exit normally
        ;; after we kill the head, then we'll see no exit message.
        (should (string-match-p
                 (rx bos (? (or "interrupt" (seq "killed" (* nonl))) "\n") eos)
                 (buffer-substring-no-properties
                  output-start (eshell-end-of-output))))
        ;; Make sure Eshell only emitted one prompt by going back one
        ;; prompt and checking the command input.
        (eshell-previous-prompt)
        (should (string-prefix-p "sh -c" (field-string)))))))

(ert-deftest esh-proc-test/kill-pipeline-head ()
  "Test that killing the first process in a pipeline doesn't
write the exit status to the pipe.  See bug#54136."
  (skip-unless (and (executable-find "sh")
                    (executable-find "echo")
                    (executable-find "sleep")))
  (with-temp-eshell
    (ert-info (#'eshell-get-debug-logs :prefix "Command logs: ")
      (eshell-insert-command
       (concat "sh -c 'while true; do sleep 1; done' | "
               "sh -c 'while read NAME; do echo =${NAME}=; done'"))
      (let ((output-start (eshell-beginning-of-output)))
        (kill-process (eshell-head-process))
        (eshell-wait-for-subprocess t)
        (should (equal (buffer-substring-no-properties
                        output-start (eshell-end-of-output))
                       ""))))))

(ert-deftest esh-proc-test/kill/process-id ()
  "Test killing processes with the \"kill\" built-in using PIDs."
  (skip-unless (executable-find "sleep"))
  (with-temp-eshell
    (eshell-insert-command "sleep 100 &")
    (string-match (rx (group (+ digit)) eol) (eshell-last-output))
    (let ((pid (match-string 1 (eshell-last-output))))
      (should (= (length eshell-process-list) 1))
      (eshell-insert-command (format "kill %s" pid))
      (should (= eshell-last-command-status 0))
      (eshell-wait-for-subprocess t)
      (should (= (length eshell-process-list) 0)))))

(ert-deftest esh-proc-test/kill/process-object ()
  "Test killing processes with the \"kill\" built-in using process objects."
  (skip-unless (executable-find "sleep"))
  (with-temp-eshell
    (eshell-insert-command "sleep 100 &")
    (should (= (length eshell-process-list) 1))
    (eshell-insert-command "kill (caar eshell-process-list)")
    (should (= eshell-last-command-status 0))
    (eshell-wait-for-subprocess t)
    (should (= (length eshell-process-list) 0))))


;; Remote processes

(ert-deftest esh-proc-test/remote/remote-path ()
  "Ensure that setting the remote PATH in Eshell doesn't interfere with Tramp.
See bug#65551."
  (skip-unless (and (eshell-tests-remote-accessible-p)
                    (executable-find "echo")))
  (let ((default-directory ert-remote-temporary-file-directory))
    (with-temp-eshell
     (eshell-insert-command "set PATH ''")
     (eshell-match-command-output
      (format "%s hello" (executable-find "echo" t))
      "\\`hello\n"))))

;;; esh-proc-tests.el ends here
