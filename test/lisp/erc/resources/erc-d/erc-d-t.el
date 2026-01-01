;;; erc-d-t.el --- ERT helpers for ERC test server -*- lexical-binding: t -*-

;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

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

(eval-and-compile
  (let* ((d (file-name-directory (or (macroexp-file-name) buffer-file-name)))
         (load-path (cons (directory-file-name d) load-path)))
    (require 'erc-d-u)))

(require 'ert)

(defun erc-d-t-kill-related-buffers ()
  "Kill all erc- or erc-d- related buffers."
  (let (buflist)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (or erc-d-u--process-buffer
                  (derived-mode-p 'erc-mode 'erc-dcc-chat-mode))
          (push buf buflist))))
    (dolist (buf buflist)
      (when (and (boundp 'erc-server-flood-timer)
                 (timerp erc-server-flood-timer))
        (cancel-timer erc-server-flood-timer))
      (when-let* ((proc (get-buffer-process buf)))
        (delete-process proc))
      (when (buffer-live-p buf)
        (kill-buffer buf))))
  (while (when-let* ((buf (pop erc-d-u--canned-buffers)))
           (kill-buffer buf))))

(defun erc-d-t-silence-around (orig &rest args)
  "Run ORIG function with ARGS silently.
Use this on `erc-handle-login' and `erc-server-connect'."
  (let ((inhibit-message t))
    (apply orig args)))

(defvar erc-d-t-cleanup-sleep-secs 0.1)

(defmacro erc-d-t-with-cleanup (bindings cleanup &rest body)
  "Execute BODY and run CLEANUP form regardless of outcome.
`let*'-bind BINDINGS and make them available in BODY and CLEANUP.
After CLEANUP, destroy any values in BINDINGS that remain bound to
buffers or processes.  Sleep `erc-d-t-cleanup-sleep-secs' before
returning."
  (declare (indent 2))
  `(let* ,bindings
     (unwind-protect
         (progn ,@body)
       ,cleanup
       (when noninteractive
         (let (bufs procs)
           (dolist (o (list ,@(mapcar (lambda (b) (or (car-safe b) b))
                                      bindings)))
             (when (bufferp o)
               (push o bufs))
             (when (processp o)
               (push o procs)))
           (dolist (proc procs)
             (delete-process proc)
             (when-let* ((buf (process-buffer proc)))
               (push buf bufs)))
           (dolist (buf bufs)
             (when-let* ((proc (get-buffer-process buf)))
               (delete-process proc))
             (when (bufferp buf)
               (ignore-errors (kill-buffer buf)))))
         (sleep-for erc-d-t-cleanup-sleep-secs)))))

(defvar erc-d-t--wait-message-prefix "Awaiting: ")

(defmacro erc-d-t-wait-for (max-secs msg &rest body)
  "Wait for BODY to become non-nil.
Or signal error with MSG after MAX-SECS.  When MAX-SECS is negative,
signal if BODY is ever non-nil before MAX-SECS elapses.  On success,
return BODY's value.

Note: this assumes BODY is waiting on a peer's output.  It tends to
artificially accelerate consumption of all process output, which may not
be desirable."
  (declare (indent 2))
  (unless (or (stringp msg) (memq (car-safe msg) '(format concat)))
    (push msg body)
    (setq msg (prin1-to-string body)))
  (let ((inverted (make-symbol "inverted"))
        (time-out (make-symbol "time-out"))
        (result (make-symbol "result")))
    `(ert-info ((concat erc-d-t--wait-message-prefix ,msg))
       (let ((,time-out (abs ,max-secs))
             (,inverted (< ,max-secs 0))
             (,result ',result))
         (with-timeout (,time-out (if ,inverted
                                      (setq ,inverted nil)
                                    (error "Failed awaiting: %s" ,msg)))
           (while (not (setq ,result (progn ,@body)))
             (when (and (accept-process-output nil 0.1) (not noninteractive))
               (redisplay))))
         (when ,inverted
           (error "Failed awaiting: %s" ,msg))
         ,result))))

(defmacro erc-d-t-ensure-for (max-secs msg &rest body)
  "Ensure BODY remains non-nil for MAX-SECS.
On failure, emit MSG."
  (declare (indent 2))
  (unless (or (stringp msg) (memq (car-safe msg) '(format concat)))
    (push msg body)
    (setq msg (prin1-to-string body)))
  `(let ((erc-d-t--wait-message-prefix "Sustaining: "))
     (erc-d-t-wait-for (- (abs ,max-secs)) ,msg (not (progn ,@body)))))

(defun erc-d-t-search-for (timeout text &optional from on-success)
  "Wait for TEXT to appear in current buffer before TIMEOUT secs.
With marker or number FROM, only consider the portion of the buffer from
that point forward.  If TEXT is a cons, interpret it as an RX regular
expression.  If ON-SUCCESS is a function, call it when TEXT is found."
  (save-restriction
    (widen)
    (let* ((rxp (consp text))
           (fun (if rxp #'search-forward-regexp #'search-forward))
           (pat (if rxp (rx-to-string text) text))
           res)
      (erc-d-t-wait-for timeout (format "string: %s" text)
        (goto-char (or from (point-min)))
        (setq res (funcall fun pat nil t))
        (if (and on-success res)
            (funcall on-success)
          res)))))

(defun erc-d-t-absent-for (timeout text &optional from on-success)
  "Assert TEXT doesn't appear in current buffer for TIMEOUT secs."
  (erc-d-t-search-for (- (abs timeout)) text from on-success))

(defun erc-d-t-make-expecter ()
  "Return function to search for new output in buffer.
Assume new text is only inserted at or after `erc-insert-marker'.

The returned function works like `erc-d-t-search-for', but it never
revisits previously covered territory, and the optional fourth argument,
ON-SUCCESS, is nonexistent.  To reset, specify a FROM argument."
  (let (positions)
    (lambda (timeout text &optional reset-from)
      (let* ((pos (cdr (assq (current-buffer) positions)))
             (erc-d-t--wait-message-prefix (and (< timeout 0) "Sustaining: "))
             (cb (lambda ()
                   (unless pos
                     (push (cons (current-buffer) (setq pos (make-marker)))
                           positions))
                   (marker-position
                    (set-marker pos (min (point) (1- (point-max))))))))
        (when reset-from
          (set-marker pos reset-from))
        (erc-d-t-search-for timeout text pos cb)))))

(provide 'erc-d-t)
;;; erc-d-t.el ends here
