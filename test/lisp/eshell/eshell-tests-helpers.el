;;; eshell-tests-helpers.el --- Eshell test suite helpers  -*- lexical-binding:t -*-

;; Copyright (C) 1999-2022 Free Software Foundation, Inc.

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

(defvar eshell-test--max-subprocess-time 5
  "The maximum amount of time to wait for a subprocess to finish, in seconds.
See `eshell-wait-for-subprocess'.")

(defmacro with-temp-eshell (&rest body)
  "Evaluate BODY in a temporary Eshell buffer."
  `(save-current-buffer
     (ert-with-temp-directory eshell-directory-name
       (let* (;; We want no history file, so prevent Eshell from falling
              ;; back on $HISTFILE.
              (process-environment (cons "HISTFILE" process-environment))
              (eshell-history-file-name nil)
              (eshell-buffer (eshell t)))
         (unwind-protect
             (with-current-buffer eshell-buffer
               ,@body)
           (let (kill-buffer-query-functions)
             (kill-buffer eshell-buffer)))))))

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

(defun eshell-insert-command (text &optional func)
  "Insert a command at the end of the buffer."
  (goto-char eshell-last-output-end)
  (insert-and-inherit text)
  (funcall (or func 'eshell-send-input)))

(defun eshell-match-result (regexp)
  "Check that output of last command matches REGEXP."
  (should
   (string-match-p
    regexp (buffer-substring-no-properties
            (eshell-beginning-of-output) (eshell-end-of-output)))))

(defun eshell-command-result-p (text regexp &optional func)
  "Insert a command at the end of the buffer."
  (eshell-insert-command text func)
  (eshell-wait-for-subprocess)
  (eshell-match-result regexp))

(defvar eshell-history-file-name)

(defun eshell-test-command-result (command)
  "Like `eshell-command-result', but not using HOME."
  (ert-with-temp-directory eshell-directory-name
    (let ((eshell-history-file-name nil))
      (eshell-command-result command))))

(provide 'eshell-tests-helpers)

;;; eshell-tests-helpers.el ends here
