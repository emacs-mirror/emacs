;;; esh-proc-tests.el --- esh-proc test suite  -*- lexical-binding:t -*-

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

;;; Code:

(require 'tramp)
(require 'ert)
(require 'esh-mode)
(require 'eshell)

(require 'eshell-tests-helpers
         (expand-file-name "eshell-tests-helpers"
                           (file-name-directory (or load-file-name
                                                    default-directory))))

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

(ert-deftest esh-var-test/output/remote-redirect ()
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


;; Pipelines

(ert-deftest esh-proc-test/sigpipe-exits-process ()
  "Test that a SIGPIPE is properly sent to a process if a pipe closes"
  (skip-unless (and (executable-find "sh")
                    (executable-find "echo")
                    (executable-find "sleep")))
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
   (should (eq (process-list) nil))))

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
  ;; An `eshell-pipe-broken' signal might occur internally; let Eshell
  ;; handle it!
  (let ((debug-on-error nil))
    (eshell-command-result-equal
     (concat "echo hi | " esh-proc-test--detect-pty-cmd " | cat")
     nil)))

(ert-deftest esh-proc-test/pipeline-connection-type/last ()
  "Test that only output streams are PTYs when a command ends a pipeline."
  (skip-unless (executable-find "sh"))
  ;; An `eshell-pipe-broken' signal might occur internally; let Eshell
  ;; handle it!
  (let ((debug-on-error nil))
    (eshell-command-result-equal
     (concat "echo hi | " esh-proc-test--detect-pty-cmd)
     (unless (eq system-type 'windows-nt)
       "stdout\nstderr\n"))))


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

(ert-deftest esh-proc-test/kill-pipeline ()
  "Test that killing a pipeline of processes only emits a single
prompt.  See bug#54136."
  (skip-unless (and (executable-find "sh")
                    (executable-find "echo")
                    (executable-find "sleep")))
  ;; This test doesn't work on EMBA with AOT nativecomp, but works
  ;; fine elsewhere.
  (skip-unless (not (getenv "EMACS_EMBA_CI")))
  (with-temp-eshell
   (eshell-insert-command
    (concat "sh -c 'while true; do echo y; sleep 1; done' | "
            "sh -c 'while true; do read NAME; done'"))
   (let ((output-start (eshell-beginning-of-output)))
     (eshell-kill-process)
     (eshell-wait-for-subprocess t)
     (should (string-match-p
              ;; "interrupt\n" is for MS-Windows.
              (rx (or "interrupt\n" "killed\n" "killed: 9\n"))
              (buffer-substring-no-properties
               output-start (eshell-end-of-output)))))))

(ert-deftest esh-proc-test/kill-pipeline-head ()
  "Test that killing the first process in a pipeline doesn't
write the exit status to the pipe.  See bug#54136."
  (skip-unless (and (executable-find "sh")
                    (executable-find "echo")
                    (executable-find "sleep")))
  (with-temp-eshell
   (eshell-insert-command
    (concat "sh -c 'while true; do sleep 1; done' | "
            "sh -c 'while read NAME; do echo =${NAME}=; done'"))
   (let ((output-start (eshell-beginning-of-output)))
     (kill-process (eshell-head-process))
     (eshell-wait-for-subprocess t)
     (should (equal (buffer-substring-no-properties
                     output-start (eshell-end-of-output))
                    "")))))


;; Remote processes

(ert-deftest esh-var-test/remote/remote-path ()
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
