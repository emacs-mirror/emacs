;;; emacs-tests.el --- unit tests for emacs.c -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Free Software Foundation, Inc.

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
(require 'ert-x)
(require 'rx)
(require 'subr-x)

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

(cl-defmacro emacs-tests--with-temp-file
    (var (prefix &optional suffix text) &rest body)
  "Evaluate BODY while a new temporary file exists.
Bind VAR to the name of the file.  Pass PREFIX, SUFFIX, and TEXT
to `make-temp-file', which see."
  (declare (indent 2) (debug (symbolp (form form form) body)))
  (cl-check-type var symbol)
  ;; Use an uninterned symbol so that the code still works if BODY
  ;; changes VAR.
  (let ((filename (make-symbol "filename")))
    `(let ((,filename (make-temp-file ,prefix nil ,suffix ,text)))
       (unwind-protect
           (let ((,var ,filename))
             ,@body)
         (delete-file ,filename)))))

(ert-deftest emacs-tests/seccomp/empty-file ()
  (skip-unless (string-match-p (rx bow "SECCOMP" eow)
                               system-configuration-features))
  (let ((emacs
         (expand-file-name invocation-name invocation-directory))
        (process-environment nil))
    (skip-unless (file-executable-p emacs))
    (emacs-tests--with-temp-file filter ("seccomp-invalid-" ".bpf")
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
    (emacs-tests--with-temp-file
        filter ("seccomp-too-large-" ".bpf"
                (make-string (* (1+ ushort-max) filter-size) ?a))
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
    (emacs-tests--with-temp-file filter ("seccomp-invalid-" ".bpf"
                                         "123456")
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
        (filter (ert-resource-file "seccomp-filter.bpf"))
        (process-environment nil))
    (skip-unless (file-executable-p emacs))
    (skip-unless (file-readable-p filter))
    ;; The --seccomp option is processed early, without filename
    ;; handlers.  Therefore remote or quoted filenames wouldn't work.
    (should-not (file-remote-p filter))
    (cl-callf file-name-unquote filter)
    (with-temp-buffer
      (let ((status (call-process
                     emacs nil t nil
                     "--quick" "--batch"
                     (concat "--seccomp=" filter)
                     (format "--eval=%S" '(message "Hi")))))
        (ert-info ((format "Process output: %s" (buffer-string)))
          (should (eql status 0)))
        (should (equal (string-trim (buffer-string)) "Hi"))))))

(ert-deftest emacs-tests/seccomp/forbids-subprocess ()
  (skip-unless (string-match-p (rx bow "SECCOMP" eow)
                               system-configuration-features))
  (let ((emacs
         (expand-file-name invocation-name invocation-directory))
        (filter (ert-resource-file "seccomp-filter.bpf"))
        (process-environment nil))
    (skip-unless (file-executable-p emacs))
    (skip-unless (file-readable-p filter))
    ;; The --seccomp option is processed early, without filename
    ;; handlers.  Therefore remote or quoted filenames wouldn't work.
    (should-not (file-remote-p filter))
    (cl-callf file-name-unquote filter)
    (with-temp-buffer
      (let ((status
             (call-process
              emacs nil t nil
              "--quick" "--batch"
              (concat "--seccomp=" filter)
              (format "--eval=%S" `(call-process ,emacs nil nil nil
                                                 "--version")))))
        (ert-info ((format "Process output: %s" (buffer-string)))
          (should-not (eql status 0)))))))

;;; emacs-tests.el ends here
