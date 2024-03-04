;;; emacs-tests.el --- unit tests for emacs.c -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2024 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Unit tests for src/emacs.c.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'ert-x) ; ert-with-temp-file
(require 'rx)
(require 'subr-x)

(defconst emacs-tests--lib-src
  (substitute-in-file-name "$EMACS_TEST_DIRECTORY/../lib-src/")
  "Location of the lib-src directory.")

(ert-deftest emacs-tests/seccomp/absent-file ()
  (skip-unless (string-match-p (rx bow "SECCOMP" eow)
                               system-configuration-features))
  (let ((emacs
         (expand-file-name invocation-name invocation-directory))
        (process-environment nil))
    (skip-unless (file-executable-p emacs))
    (should-not (file-exists-p "/does-not-exist.bpf"))
    (should-not
     (eql (call-process emacs nil nil nil
                        "--quick" "--batch"
                        "--seccomp=/does-not-exist.bpf")
          0))))

(ert-deftest emacs-tests/seccomp/empty-file ()
  (skip-unless (string-match-p (rx bow "SECCOMP" eow)
                               system-configuration-features))
  (let ((emacs
         (expand-file-name invocation-name invocation-directory))
        (process-environment nil))
    (skip-unless (file-executable-p emacs))
    (ert-with-temp-file filter
      :prefix "seccomp-invalid-" :suffix ".bpf"
      ;; The --seccomp option is processed early, without filename
      ;; handlers.  Therefore remote or quoted filenames wouldn't
      ;; work.
      (should-not (file-remote-p filter))
      (cl-callf file-name-unquote filter)
      ;; According to the Seccomp man page, a filter must have at
      ;; least one element, so Emacs should reject an empty file.
      (should-not
       (eql (call-process emacs nil nil nil
                          "--quick" "--batch"
                          (concat "--seccomp=" filter))
            0)))))

(ert-deftest emacs-tests/seccomp/file-too-large ()
  (skip-unless (string-match-p (rx bow "SECCOMP" eow)
                               system-configuration-features))
  (let ((emacs
         (expand-file-name invocation-name invocation-directory))
        (process-environment nil)
        ;; This value should be correct on all supported systems.
        (ushort-max #xFFFF)
        ;; Either 8 or 16, but 16 should be large enough in all cases.
        (filter-size 16))
    (skip-unless (file-executable-p emacs))
    (ert-with-temp-file filter
      :prefix "seccomp-too-large-" :suffix ".bpf"
      :text (make-string (* (1+ ushort-max) filter-size) ?a)
      ;; The --seccomp option is processed early, without filename
      ;; handlers.  Therefore remote or quoted filenames wouldn't
      ;; work.
      (should-not (file-remote-p filter))
      (cl-callf file-name-unquote filter)
      ;; The filter count must fit into an `unsigned short'.  A bigger
      ;; file should be rejected.
      (should-not
       (eql (call-process emacs nil nil nil
                          "--quick" "--batch"
                          (concat "--seccomp=" filter))
            0)))))

(ert-deftest emacs-tests/seccomp/invalid-file-size ()
  (skip-unless (string-match-p (rx bow "SECCOMP" eow)
                               system-configuration-features))
  (let ((emacs
         (expand-file-name invocation-name invocation-directory))
        (process-environment nil))
    (skip-unless (file-executable-p emacs))
    (ert-with-temp-file filter
      :prefix "seccomp-invalid-" :suffix ".bpf" :text "123456"
      ;; The --seccomp option is processed early, without filename
      ;; handlers.  Therefore remote or quoted filenames wouldn't
      ;; work.
      (should-not (file-remote-p filter))
      (cl-callf file-name-unquote filter)
      ;; The Seccomp filter file must have a file size that's a
      ;; multiple of the size of struct sock_filter, which is 8 or 16,
      ;; but never 6.
      (should-not
       (eql (call-process emacs nil nil nil
                          "--quick" "--batch"
                          (concat "--seccomp=" filter))
            0)))))

(ert-deftest emacs-tests/seccomp/allows-stdout ()
  (skip-unless (string-match-p (rx bow "SECCOMP" eow)
                               system-configuration-features))
  (let ((emacs
         (expand-file-name invocation-name invocation-directory))
        (filter (expand-file-name "seccomp-filter.bpf"
                                  emacs-tests--lib-src))
        (process-environment nil))
    (skip-unless (file-executable-p emacs))
    (skip-unless (file-readable-p filter))
    ;; The --seccomp option is processed early, without filename
    ;; handlers.  Therefore remote or quoted filenames wouldn't work.
    (should-not (file-remote-p filter))
    (cl-callf file-name-unquote filter)
    (with-temp-buffer
      (let ((start-time (current-time))
            (status (call-process
                     emacs nil t nil
                     "--quick" "--batch"
                     (concat "--seccomp=" filter)
                     (format "--eval=%S" '(message "Hi"))))
            (end-time (current-time)))
        (ert-info ((emacs-tests--seccomp-debug start-time end-time))
          (should (eql status 0)))
        (should (equal (string-trim (buffer-string)) "Hi"))))))

(ert-deftest emacs-tests/seccomp/forbids-subprocess ()
  (skip-unless (string-match-p (rx bow "SECCOMP" eow)
                               system-configuration-features))
  (let ((emacs
         (expand-file-name invocation-name invocation-directory))
        (filter (expand-file-name "seccomp-filter.bpf"
                                  emacs-tests--lib-src))
        (process-environment nil))
    (skip-unless (file-executable-p emacs))
    (skip-unless (file-readable-p filter))
    ;; The --seccomp option is processed early, without filename
    ;; handlers.  Therefore remote or quoted filenames wouldn't work.
    (should-not (file-remote-p filter))
    (cl-callf file-name-unquote filter)
    (with-temp-buffer
      (let ((start-time (current-time))
            (status
             (call-process
              emacs nil t nil
              "--quick" "--batch"
              (concat "--seccomp=" filter)
              (format "--eval=%S" `(call-process ,emacs nil nil nil
                                                 "--version"))))
            (end-time (current-time)))
        (ert-info ((emacs-tests--seccomp-debug start-time end-time))
          (should-not (eql status 0)))))))

(ert-deftest emacs-tests/bwrap/allows-stdout ()
  (let ((bash (executable-find "bash"))
        (bwrap (executable-find "bwrap"))
        (emacs
         (expand-file-name invocation-name invocation-directory))
        (filter (expand-file-name "seccomp-filter-exec.bpf"
                                  emacs-tests--lib-src))
        (process-environment nil))
    (skip-unless bash)
    (skip-unless bwrap)
    (skip-unless (file-executable-p emacs))
    (skip-unless (file-readable-p filter))
    (should-not (file-remote-p bwrap))
    (should-not (file-remote-p emacs))
    (should-not (file-remote-p filter))
    (with-temp-buffer
      (let* ((command
              (concat
               (mapconcat #'shell-quote-argument
                          `(,(file-name-unquote bwrap)
                            "--ro-bind" "/" "/"
                            "--seccomp" "20"
                            "--"
                            ,(file-name-unquote emacs)
                            "--quick" "--batch"
                            ,(format "--eval=%S" '(message "Hi")))
                          " ")
               " 20< "
               (shell-quote-argument (file-name-unquote filter))))
             (start-time (current-time))
             (status (call-process bash nil t nil "-c" command))
             (end-time (current-time)))
        (ert-info ((emacs-tests--seccomp-debug start-time end-time))
          (should (eql status 0)))
        (should (equal (string-trim (buffer-string)) "Hi"))))))

(defun emacs-tests--seccomp-debug (start-time end-time)
  "Return potentially useful debugging information for Seccomp.
Assume that the current buffer contains subprocess output for the
failing process.  START-TIME and END-TIME are time values between
which the process was running."
  ;; Add a bit of slack for the timestamps.
  (cl-callf time-subtract start-time 5)
  (cl-callf time-add end-time 5)
  (with-output-to-string
    (princ "Process output:")
    (terpri)
    (princ (buffer-substring-no-properties (point-min) (point-max)))
    ;; Search audit logs for Seccomp messages.
    (when-let ((ausearch (executable-find "ausearch")))
      (terpri)
      (princ "Potentially relevant Seccomp audit events:")
      (terpri)
      (let ((process-environment '("LC_TIME=C")))
        (call-process ausearch nil standard-output nil
                      "--message" "SECCOMP"
                      "--start"
                      (format-time-string "%D" start-time)
                      (format-time-string "%T" start-time)
                      "--end"
                      (format-time-string "%D" end-time)
                      (format-time-string "%T" end-time)
                      "--interpret")))
    ;; Print coredump information if available.
    (when-let ((coredumpctl (executable-find "coredumpctl")))
      (terpri)
      (princ "Potentially useful coredump information:")
      (terpri)
      (call-process coredumpctl nil standard-output nil
                    "info"
                    "--since" (format-time-string "%F %T" start-time)
                    "--until" (format-time-string "%F %T" end-time)
                    "--no-pager"))))

;;; emacs-tests.el ends here
