;;; callproc-tests.el --- callproc.c tests -*- lexical-binding: t -*-

;; Copyright (C) 2016-2020 Free Software Foundation, Inc.

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
(eval-when-compile (require 'cl-lib))

(ert-deftest initial-environment-preserved ()
  "Check that `initial-environment' is not modified by Emacs (Bug #10980)."
  (skip-unless (eq system-type 'windows-nt))
  (cl-destructuring-bind (initial-shell shell)
      (with-temp-buffer
        (let ((process-environment (cons "SHELL" process-environment)))
          (call-process (expand-file-name invocation-name invocation-directory)
                        nil t nil
                        "--batch" "-Q" "--eval"
                        (prin1-to-string
                         '(progn (prin1 (getenv-internal "SHELL" initial-environment))
                                 (prin1 (getenv-internal "SHELL"))))))
        (split-string-and-unquote (buffer-string)))
    (should (equal initial-shell "nil"))
    (should-not (equal initial-shell shell))))

(ert-deftest call-process-w32-debug-spawn-error ()
  "Check that debugger runs on `call-process' failure (Bug#33016)."
  (skip-unless (eq system-type 'windows-nt))
  (let* ((debug-on-error t)
         (have-called-debugger nil)
         (debugger (lambda (&rest _)
                     (setq have-called-debugger t)
                     ;; Allow entering the debugger later in the same
                     ;; test run, before going back to the command
                     ;; loop.
                     (setq internal-when-entered-debugger -1))))
    (should (eq :got-error ;; NOTE: `should-error' would inhibit debugger.
                (condition-case-unless-debug ()
                    ;; On MS-Windows, "nul.FOO" resolves to the null
                    ;; device, and thus acts like an always-empty
                    ;; file, for any FOO, in any directory.  So
                    ;; c:/null.exe passes Emacs' test for the file's
                    ;; existence, and ensures we hit an error in the
                    ;; w32 process spawn code.
                    (call-process "c:/nul.exe")
                  (error :got-error))))
    (should have-called-debugger)))
