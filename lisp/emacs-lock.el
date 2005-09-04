;;; emacs-lock.el --- prevents you from exiting emacs if a buffer is locked

;; Copyright (C) 1994, 1997, 2002, 2003, 2004,
;;   2005 Free Software Foundation, Inc

;; Author: Tom Wurgler <twurgler@goodyear.com>
;; Created: 12/8/94
;; Keywords: extensions, processes

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

;; This code sets a buffer-local variable to t if toggle-emacs-lock is run,
;; then if the user attempts to exit emacs, the locked buffer name will be
;; displayed and the exit aborted.  This is just a way of protecting
;; yourself from yourself.  For example, if you have a shell running a big
;; program and exiting emacs would abort that program, you may want to lock
;; that buffer, then if you forget about it after a while, you won't
;; accidentally exit emacs.  To unlock the buffer, just goto the buffer and
;; run toggle-emacs-lock again.

;;; Code:

(defvar emacs-lock-from-exiting nil
  "Whether emacs is locked to prevent exiting.  See `check-emacs-lock'.")
(make-variable-buffer-local 'emacs-lock-from-exiting)

(defvar emacs-lock-buffer-locked nil
  "Whether a shell or telnet buffer was locked when its process was killed.")
(make-variable-buffer-local 'emacs-lock-buffer-locked)
(put 'emacs-lock-buffer-locked 'permanent-local t)

(defun check-emacs-lock ()
  "Check if variable `emacs-lock-from-exiting' is t for any buffer.
If any t is found, signal error and display the locked buffer name."
  (let ((buffers (buffer-list)))
    (save-excursion
      (while buffers
	(set-buffer (car buffers))
	(if emacs-lock-from-exiting
	    (error "Emacs is locked from exit due to buffer: %s" (buffer-name))
	  (setq buffers (cdr buffers)))))))

(defun toggle-emacs-lock ()
  "Toggle `emacs-lock-from-exiting' between t and nil for the current buffer.
See `check-emacs-lock'."
  (interactive)
  (if emacs-lock-from-exiting
      (setq emacs-lock-from-exiting nil)
    (setq emacs-lock-from-exiting t))
  (if emacs-lock-from-exiting
      (message "Buffer is now locked")
    (message "Buffer is now unlocked")))

(defun emacs-lock-check-buffer-lock ()
  "Check if variable `emacs-lock-from-exiting' is t for a buffer.
If t is found, signal error and display the locked buffer name."
  (if emacs-lock-from-exiting
      (error "Buffer `%s' is locked, can't delete it" (buffer-name))))

; These next defuns make it so if you exit a shell that is locked,  the lock
; is shut off for that shell so you can exit emacs.  Same for telnet.
; Also, if a shell or a telnet buffer was locked and the process killed,
; turn the lock back on again if the process is restarted.

(defun emacs-lock-shell-sentinel ()
  (set-process-sentinel
   (get-buffer-process (buffer-name)) (function emacs-lock-clear-sentinel)))

(defun emacs-lock-clear-sentinel (proc str)
  (if emacs-lock-from-exiting
      (progn
	(setq emacs-lock-from-exiting nil)
	(setq emacs-lock-buffer-locked t)
	(message "Buffer is now unlocked"))
    (setq emacs-lock-buffer-locked nil)))

(defun emacs-lock-was-buffer-locked ()
  (if emacs-lock-buffer-locked
      (setq emacs-lock-from-exiting t)))

(add-hook 'kill-emacs-hook 'check-emacs-lock)
(add-hook 'kill-buffer-hook 'emacs-lock-check-buffer-lock)
(add-hook 'shell-mode-hook 'emacs-lock-was-buffer-locked)
(add-hook 'shell-mode-hook 'emacs-lock-shell-sentinel)
(add-hook 'telnet-mode-hook 'emacs-lock-was-buffer-locked)
(add-hook 'telnet-mode-hook 'emacs-lock-shell-sentinel)

(provide 'emacs-lock)

;;; arch-tag: 58e6cb43-7cf0-401a-bcb6-4902a0b8bdc1
;;; emacs-lock.el ends here
