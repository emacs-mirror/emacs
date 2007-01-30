;;; vms-pmail.el --- use Emacs as the editor within VMS mail

;; Copyright (C) 1992, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007 Free Software Foundation, Inc.

;; Author: Roland B Roberts <roberts@panix.com>
;; Maintainer: FSF
;; Keywords: vms

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

;;; Code:

;;;
;;; Quick hack to use emacs as mail editor.  There are a *bunch* of
;;; changes scattered throughout emacs to make this work, namely:
;;; (1) mod to sysdep.c to allow emacs to attach to a process other
;;;     than the one that originally spawned it.
;;; (2) mod to kepteditor.com to define the logical emacs_parent_pid
;;;     which is what sysdep.c looks for, and define the logical
;;;     emacs_command_args which contains the command line
;;; (3) mod to re-parse command line arguments from emacs_command_args
;;;     then execute them as though emacs were just starting up.
;;;
(defun vms-pmail-save-and-exit ()
  "Save current buffer and exit Emacs.
If this Emacs cannot be suspended, you will be prompted about modified
buffers other than the mail buffer.  BEWARE --- suspending Emacs without
saving your mail buffer causes mail to abort the send (potentially useful
since the mail buffer is still here)."
  (interactive)
  (basic-save-buffer)
  (if (vms-system-info "LOGICAL" "DONT_SUSPEND_EMACS")
      (progn
        (save-some-buffers)
        (kill-emacs 1))
    (kill-buffer (current-buffer))
    (suspend-emacs)))

(defun vms-pmail-abort ()
  "Mark buffer as unmodified and exit Emacs.
When the editor is exited without saving its buffer, VMS mail does not
send a message.  If you have other modified buffers you will be
prompted for what to do with them."
  (interactive)
  (if (not (yes-or-no-p "Really abort mail? "))
      (ding)
    (not-modified)
    (if (vms-system-info "LOGICAL" "DONT_SUSPEND_EMACS")
        (progn
          (save-some-buffers)
          (kill-emacs 1))
      (kill-buffer (current-buffer))
      (suspend-emacs))))

(defun vms-pmail-setup ()
  "Set up file assuming use by VMS MAIL utility.
The buffer is put into text-mode, auto-save is turned off and the
following bindings are established.

\\[vms-pmail-save-and-exit]	vms-pmail-save-and-exit
\\[vms-pmail-abort]	vms-pmail-abort

All other Emacs commands are still available."
  (interactive)
  (auto-save-mode -1)
  (text-mode)
  (let ((default (vms-system-info "LOGICAL" "SYS$SCRATCH"))
        (directory (file-name-directory (buffer-file-name)))
        (filename (file-name-nondirectory (buffer-file-name))))
    (if (string= directory "SYS$SCRATCH:")
        (progn
          (cd default)
          (setq buffer-file-name (concat default filename))))
    (use-local-map (copy-keymap (current-local-map)))
    (local-set-key "\C-c\C-c" 'vms-pmail-save-and-exit)
    (local-set-key "\C-c\C-g" 'vms-pmail-abort)))

(defun indicate-mail-reply-text ()
  "Prepares received mail for re-sending by placing >'s on each line."
  (interactive)
  (goto-char (point-min))
  (while (not (eobp))
    (insert ">")
    (beginning-of-line)
    (forward-line 1))
  (set-buffer-modified-p nil)
  (goto-char (point-min)))

(defun insert-signature ()
  "Moves to the end of the buffer and inserts a \"signature\" file.
First try the file indicated by environment variable MAIL$TRAILER.
If that fails, try the file \"~/.signature\".
If neither file exists, fails quietly."
  (interactive)
  (end-of-buffer)
  (newline)
  (if (vms-system-info "LOGICAL" "MAIL$TRAILER")
      (if (file-attributes (vms-system-info "LOGICAL" "MAIL$TRAILER"))
	  (insert-file-contents (vms-system-info "LOGICAL" "MAIL$TRAILER"))
	(if (file-attributes "~/.signature")
	    (insert-file-contents "~/.signature")))))

(provide 'vms-pmail)

;;; arch-tag: 336850fc-7812-4663-8e4d-b9c13f47dce1
;;; vms-pmail.el ends here
