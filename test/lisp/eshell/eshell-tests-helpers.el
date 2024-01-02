;;; eshell-tests-helpers.el --- Eshell test suite helpers  -*- lexical-binding:t -*-

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

;; Eshell test suite helpers.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'esh-mode)
(require 'eshell)

(defvar eshell-history-file-name nil)
(defvar eshell-last-dir-ring-file-name nil)

(defvar eshell-test--max-subprocess-time 5
  "The maximum amount of time to wait for a subprocess to finish, in seconds.
See `eshell-wait-for-subprocess'.")

(defun eshell-tests-remote-accessible-p ()
  "Return if a test involving remote files can proceed.
If using this function, be sure to load `tramp' near the
beginning of the test file."
  (ignore-errors
    (and
     (file-remote-p ert-remote-temporary-file-directory)
     (file-directory-p ert-remote-temporary-file-directory)
     (file-writable-p ert-remote-temporary-file-directory))))

(defmacro with-temp-eshell (&rest body)
  "Evaluate BODY in a temporary Eshell buffer."
  `(save-current-buffer
     (ert-with-temp-directory eshell-directory-name
       (let* (;; We want no history file, so prevent Eshell from falling
              ;; back on $HISTFILE.
              (process-environment (cons "HISTFILE" process-environment))
              (eshell-history-file-name nil)
              (eshell-last-dir-ring-file-name nil)
              (eshell-buffer (eshell t)))
         (unwind-protect
             (with-current-buffer eshell-buffer
               ,@body)
           (let (kill-buffer-query-functions)
             (kill-buffer eshell-buffer)))))))

(defmacro eshell-with-temp-buffer (bufname text &rest body)
  "Create a temporary buffer containing TEXT and evaluate BODY there.
BUFNAME will be set to the name of the temporary buffer."
  (declare (indent 2))
  `(with-temp-buffer
     (insert ,text)
     (rename-buffer "eshell-temp-buffer" t)
     (let ((,bufname (buffer-name)))
       ,@body)))

(defun eshell-wait-for-subprocess (&optional all)
  "Wait until there is no interactive subprocess running in Eshell.
If ALL is non-nil, wait until there are no Eshell subprocesses at
all running.

If this takes longer than `eshell-test--max-subprocess-time',
raise an error."
  (let ((start (current-time)))
    (while (if all eshell-process-list (eshell-interactive-process-p))
      (when (> (float-time (time-since start))
               eshell-test--max-subprocess-time)
        (error "timed out waiting for subprocess(es)"))
      (sit-for 0.1))))

(defun eshell-insert-command (command &optional func)
  "Insert a COMMAND at the end of the buffer.
After inserting, call FUNC.  If FUNC is nil, instead call
`eshell-send-input'."
  (goto-char eshell-last-output-end)
  (insert-and-inherit command)
  (funcall (or func 'eshell-send-input)))

(defun eshell-last-input ()
  "Return the input of the last Eshell command."
  (buffer-substring-no-properties
   eshell-last-input-start eshell-last-input-end))

(defun eshell-last-output ()
  "Return the output of the last Eshell command."
  (buffer-substring-no-properties
   (eshell-beginning-of-output) (eshell-end-of-output)))

(defun eshell-match-output (regexp)
  "Test whether the output of the last command matches REGEXP."
  (string-match-p regexp (eshell-last-output)))

(defun eshell-match-output--explainer (regexp)
  "Explain the result of `eshell-match-output'."
  `(mismatched-output
    (command ,(eshell-last-input))
    (output ,(eshell-last-output))
    (regexp ,regexp)))

(put 'eshell-match-output 'ert-explainer #'eshell-match-output--explainer)

(defun eshell-match-command-output (command regexp &optional func
                                            ignore-errors)
  "Insert a COMMAND at the end of the buffer and match the output with REGEXP.
FUNC is the function to call after inserting the text (see
`eshell-insert-command').

If IGNORE-ERRORS is non-nil, ignore any errors signaled when
inserting the command."
  (let ((debug-on-error (and (not ignore-errors) debug-on-error)))
    (eshell-insert-command command func))
  (eshell-wait-for-subprocess)
  (should (eshell-match-output regexp)))

(defvar eshell-history-file-name)

(defun eshell-test-command-result (command)
  "Like `eshell-command-result', but not using HOME."
  (ert-with-temp-directory eshell-directory-name
    (let ((eshell-history-file-name nil))
      (eshell-command-result command))))

(defun eshell-command-result--equal (_command actual expected)
  "Compare the ACTUAL result of a COMMAND with its EXPECTED value."
  (equal actual expected))

(defun eshell-command-result--equal-explainer (command actual expected)
  "Explain the result of `eshell-command-result--equal'."
  `(nonequal-result
    (command ,command)
    (result ,actual)
    (expected ,expected)))

(put 'eshell-command-result--equal 'ert-explainer
     #'eshell-command-result--equal-explainer)

(defun eshell-command-result-equal (command result)
  "Execute COMMAND non-interactively and compare it to RESULT."
  (should (eshell-command-result--equal
           command
           (eshell-test-command-result command)
           result)))

(provide 'eshell-tests-helpers)

;;; eshell-tests-helpers.el ends here
