;;; emacs-lock.el --- prevents you from exiting emacs if a buffer is locked

;; Copyright (C) 1994 Free Software Foundation, Inc

;; Author: Tom Wurgler <twurgler@goodyear.com>
;; Created: 12/8/94
;; Version: 1.3
;; Keywords: 

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

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

(defvar lock-emacs-from-exiting nil
  "Whether emacs is locked to prevent exiting.  See `check-emacs-lock'.")
(make-variable-buffer-local 'lock-emacs-from-exiting)

(defun check-emacs-lock ()
  "Check if variable `lock-emacs-from-exiting' is t for any buffer.
If any t is found, signal error and display the locked buffer name."
  (let ((buffers (buffer-list)))
    (save-excursion
      (while buffers 
	(set-buffer (car buffers))
	(if lock-emacs-from-exiting
	    (error "Emacs is locked from exit due to buffer: %s" (buffer-name))
	  (setq buffers (cdr buffers)))))))

(defun toggle-emacs-lock ()
  "Toggle `lock-emacs-from-exiting' between t and nil for the current buffer.
See `check-emacs-lock'."
  (interactive)
  (if lock-emacs-from-exiting
      (setq lock-emacs-from-exiting nil)
    (setq lock-emacs-from-exiting t))
  (if lock-emacs-from-exiting
      (message "Emacs is now locked from exiting.")
    (message "Emacs is now unlocked.")))

(add-hook 'kill-emacs-hook 'check-emacs-lock)

;; emacs-lock.el ends here
