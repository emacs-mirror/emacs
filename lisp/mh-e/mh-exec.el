;;; mh-exec.el --- MH-E process support

;; Copyright (C) 1993, 1995, 1997,
;;  2000, 2001, 2002, 2003, 2004, 2005, 2006 Free Software Foundation, Inc.

;; Author: Bill Wohler <wohler@newt.com>
;; Maintainer: Bill Wohler <wohler@newt.com>
;; Keywords: mail
;; See: mh-e.el

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Issue shell and MH commands

;;; Change Log:

;;; Code:

(eval-when-compile (require 'mh-acros))
(mh-require-cl)

(require 'mh-buffers)
(require 'mh-utils)

(defvar mh-progs nil
  "Directory containing MH commands, such as inc, repl, and rmm.")

;;;###autoload
(put 'mh-progs 'risky-local-variable t)

(defvar mh-lib nil
  "Directory containing the MH library.
This directory contains, among other things, the components file.")

;;;###autoload
(put 'mh-lib 'risky-local-variable t)

(defvar mh-lib-progs nil
  "Directory containing MH helper programs.
This directory contains, among other things, the mhl program.")

;;;###autoload
(put 'mh-lib-progs 'risky-local-variable t)

(defvar mh-index-max-cmdline-args 500
  "Maximum number of command line args.")

(defun mh-xargs (cmd &rest args)
  "Partial imitation of xargs.
The current buffer contains a list of strings, one on each line.
The function will execute CMD with ARGS and pass the first
`mh-index-max-cmdline-args' strings to it. This is repeated till
all the strings have been used."
  (goto-char (point-min))
  (let ((current-buffer (current-buffer)))
    (with-temp-buffer
      (let ((out (current-buffer)))
        (set-buffer current-buffer)
        (while (not (eobp))
          (let ((arg-list (reverse args))
                (count 0))
            (while (and (not (eobp)) (< count mh-index-max-cmdline-args))
              (push (buffer-substring-no-properties (point) (line-end-position))
                    arg-list)
              (incf count)
              (forward-line))
            (apply #'call-process cmd nil (list out nil) nil
                   (nreverse arg-list))))
        (erase-buffer)
        (insert-buffer-substring out)))))

;; XXX This should be applied anywhere MH-E calls out to /bin/sh.
(defun mh-quote-for-shell (string)
  "Quote STRING for /bin/sh.
Adds double-quotes around entire string and quotes the characters
\\, `, and $ with a backslash."
  (concat "\""
          (loop for x across string
                concat (format (if (memq x '(?\\ ?` ?$)) "\\%c" "%c") x))
          "\""))

(defun mh-exec-cmd (command &rest args)
  "Execute mh-command COMMAND with ARGS.
The side effects are what is desired. Any output is assumed to be
an error and is shown to the user. The output is not read or
parsed by MH-E."
  (save-excursion
    (set-buffer (get-buffer-create mh-log-buffer))
    (let* ((initial-size (mh-truncate-log-buffer))
           (start (point))
           (args (mh-list-to-string args)))
      (apply 'call-process (expand-file-name command mh-progs) nil t nil args)
      (when (> (buffer-size) initial-size)
        (save-excursion
          (goto-char start)
          (insert "Errors when executing: " command)
          (loop for arg in args do (insert " " arg))
          (insert "\n"))
        (save-window-excursion
          (switch-to-buffer-other-window mh-log-buffer)
          (sit-for 5))))))

(defun mh-exec-cmd-error (env command &rest args)
  "In environment ENV, execute mh-command COMMAND with ARGS.
ENV is nil or a string of space-separated \"var=value\" elements.
Signals an error if process does not complete successfully."
  (save-excursion
    (set-buffer (get-buffer-create mh-temp-buffer))
    (erase-buffer)
    (let ((process-environment process-environment))
      ;; XXX: We should purge the list that split-string returns of empty
      ;;  strings. This can happen in XEmacs if leading or trailing spaces
      ;;  are present.
      (dolist (elem (if (stringp env) (split-string env " ") ()))
        (push elem process-environment))
      (mh-handle-process-error
       command (apply #'call-process (expand-file-name command mh-progs)
                      nil t nil (mh-list-to-string args))))))

(defun mh-exec-cmd-daemon (command filter &rest args)
  "Execute MH command COMMAND in the background.

If FILTER is non-nil then it is used to process the output
otherwise the default filter `mh-process-daemon' is used. See
`set-process-filter' for more details of FILTER.

ARGS are passed to COMMAND as command line arguments."
  (save-excursion
    (set-buffer (get-buffer-create mh-log-buffer))
    (mh-truncate-log-buffer))
  (let* ((process-connection-type nil)
         (process (apply 'start-process
                         command nil
                         (expand-file-name command mh-progs)
                         (mh-list-to-string args))))
    (set-process-filter process (or filter 'mh-process-daemon))
    process))

(defun mh-exec-cmd-env-daemon (env command filter &rest args)
  "In ennvironment ENV, execute mh-command COMMAND in the background.

ENV is nil or a string of space-separated \"var=value\" elements.
Signals an error if process does not complete successfully.

If FILTER is non-nil then it is used to process the output
otherwise the default filter `mh-process-daemon' is used. See
`set-process-filter' for more details of FILTER.

ARGS are passed to COMMAND as command line arguments."
  (let ((process-environment process-environment))
    (dolist (elem (if (stringp env) (split-string env " ") ()))
      (push elem process-environment))
    (apply #'mh-exec-cmd-daemon command filter args)))

(defun mh-process-daemon (process output)
  "PROCESS daemon that puts OUTPUT into a temporary buffer.
Any output from the process is displayed in an asynchronous
pop-up window."
  (with-current-buffer (get-buffer-create mh-log-buffer)
    (insert-before-markers output)
    (display-buffer mh-log-buffer)))

(defun mh-exec-cmd-quiet (raise-error command &rest args)
  "Signal RAISE-ERROR if COMMAND with ARGS fails.
Execute MH command COMMAND with ARGS. ARGS is a list of strings.
Return at start of mh-temp buffer, where output can be parsed and
used.
Returns value of `call-process', which is 0 for success, unless
RAISE-ERROR is non-nil, in which case an error is signaled if
`call-process' returns non-0."
  (set-buffer (get-buffer-create mh-temp-buffer))
  (erase-buffer)
  (let ((value
         (apply 'call-process
                (expand-file-name command mh-progs) nil t nil
                args)))
    (goto-char (point-min))
    (if raise-error
        (mh-handle-process-error command value)
      value)))

;; Shush compiler.
(eval-when-compile (defvar mark-active))

(defun mh-exec-cmd-output (command display &rest args)
  "Execute MH command COMMAND with DISPLAY flag and ARGS.
Put the output into buffer after point.
Set mark after inserted text.
Output is expected to be shown to user, not parsed by MH-E."
  (push-mark (point) t)
  (apply 'call-process
         (expand-file-name command mh-progs) nil t display
         (mh-list-to-string args))

  ;; The following is used instead of 'exchange-point-and-mark because the
  ;; latter activates the current region (between point and mark), which
  ;; turns on highlighting.  So prior to this bug fix, doing "inc" would
  ;; highlight a region containing the new messages, which is undesirable.
  ;; The bug wasn't seen in emacs21 but still occurred in XEmacs21.4.
  (mh-exchange-point-and-mark-preserving-active-mark))

(defun mh-exchange-point-and-mark-preserving-active-mark ()
  "Put the mark where point is now, and point where the mark is now.
This command works even when the mark is not active, and
preserves whether the mark is active or not."
  (interactive nil)
  (let ((is-active (and (boundp 'mark-active) mark-active)))
    (let ((omark (mark t)))
      (if (null omark)
          (error "No mark set in this buffer"))
      (set-mark (point))
      (goto-char omark)
      (if (boundp 'mark-active)
          (setq mark-active is-active))
      nil)))

(defun mh-exec-lib-cmd-output (command &rest args)
  "Execute MH library command COMMAND with ARGS.
Put the output into buffer after point.
Set mark after inserted text."
  (apply 'mh-exec-cmd-output (expand-file-name command mh-lib-progs) nil args))

(defun mh-handle-process-error (command status)
  "Raise error if COMMAND returned non-zero STATUS, otherwise return STATUS."
  (if (equal status 0)
      status
    (goto-char (point-min))
    (insert (if (integerp status)
                (format "%s: exit code %d\n" command status)
              (format "%s: %s\n" command status)))
    (save-excursion
      (let ((error-message (buffer-substring (point-min) (point-max))))
        (set-buffer (get-buffer-create mh-log-buffer))
        (mh-truncate-log-buffer)
        (insert error-message)))
    (error "%s failed, check buffer %s for error message"
           command mh-log-buffer)))

(provide 'mh-exec)

;; Local Variables:
;; indent-tabs-mode: nil
;; sentence-end-double-space: nil
;; End:

;; arch-tag: 2857996c-e624-46b2-a58d-979cd279d288
;;; mh-utils.el ends here
