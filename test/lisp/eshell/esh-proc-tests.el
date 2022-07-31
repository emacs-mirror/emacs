;;; esh-proc-tests.el --- esh-proc test suite  -*- lexical-binding:t -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

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

(require 'ert)
(require 'esh-mode)
(require 'eshell)

(require 'eshell-tests-helpers
         (expand-file-name "eshell-tests-helpers"
                           (file-name-directory (or load-file-name
                                                    default-directory))))

(ert-deftest esh-proc-test/sigpipe-exits-process ()
  "Test that a SIGPIPE is properly sent to a process if a pipe closes"
  (skip-unless (and (executable-find "sh")
                    (executable-find "echo")
                    (executable-find "sleep")))
  (with-temp-eshell
   (eshell-command-result-p
    ;; The first command is like `yes' but slower.  This is to prevent
    ;; it from taxing Emacs's process filter too much and causing a
    ;; hang.
    (concat "sh -c 'while true; do echo y; sleep 1; done' | "
            "sh -c 'read NAME; echo ${NAME}'")
    "y\n")
   (eshell-wait-for-subprocess t)
   (should (eq (process-list) nil))))

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

(ert-deftest esh-proc-test/kill-background-process ()
  "Test that killing a background process doesn't emit a new
prompt.  See bug#54136."
  (skip-unless (and (executable-find "sh")
                    (executable-find "sleep")))
  (with-temp-eshell
   (eshell-insert-command "sh -c 'while true; do sleep 1; done' &")
   (kill-process (caar eshell-process-list))
   ;; Give `eshell-sentinel' a chance to run.
   (sit-for 0.1)
   (eshell-match-result "\\[sh\\(\\.exe\\)?\\] [[:digit:]]+\n")))
