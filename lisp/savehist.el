;;; savehist.el --- Save minibuffer history.

;; Copyright (C) 1997,2005 Free Software Foundation

;; Author: Hrvoje Niksic <hniksic@xemacs.org>
;; Keywords: minibuffer
;; Version: 9

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

;; Many editors (e.g. Vim) have the feature of saving minibuffer
;; history to an external file after exit.  This package provides the
;; same feature in Emacs.  When set up, it saves recorded minibuffer
;; histories to a file (`~/.emacs-history' by default).  Additional
;; variables may be specified by customizing
;; `savehist-additional-variables'.

;; To use savehist, put the following to `~/.emacs':
;;
;; (require 'savehist)
;; (savehist-load)

;; If you are using a version of Emacs that does not ship with this
;; package, be sure to have `savehist.el' in a directory that is in
;; your load-path, and to byte-compile it.

;;; Code:

(require 'custom)
(eval-when-compile
  (require 'cl))

;; User variables

(defgroup savehist nil
  "Save minibuffer history."
  :group 'minibuffer)

(defcustom savehist-save-minibuffer-history t
  "If non-nil, save all recorded minibuffer histories."
  :type 'boolean
  :group 'savehist)

(defcustom savehist-additional-variables ()
  "List of additional variables to save.
Each element is a symbol whose value will be persisted across Emacs
sessions that use savehist.  The contents of variables should be
printable with the Lisp printer.  If the variable's value is a list,
it will be trimmed to `savehist-length' elements.

You don't need to add minibuffer history variables to this list.  All
minibuffer histories will be saved automatically."
  :type '(repeat (symbol :tag "Variable"))
  :group 'savehist)

(defcustom savehist-file "~/.emacs-history"
  "File name to save minibuffer history to.
The minibuffer history is a series of Lisp expressions, which should be
loaded using `savehist-load' from your .emacs.  See `savehist-load' for
more details."
  :type 'file
  :group 'savehist)

(defcustom savehist-length 100
  "Maximum length of a minibuffer list.
Minibuffer histories with more entries are trimmed when saved, the older
entries being removed first.  If set to nil, the length is unlimited."
  :type '(choice integer
		 (const :tag "Unlimited" nil))
  :group 'savehist)

(defcustom savehist-modes #o600
  "Default permissions of the history file.
This is decimal, not octal.  The default is 384 (0600 in octal).
Set to nil to use the default permissions that Emacs uses, typically
mandated by umask.  The default is a bit more restrictive to protect
the user's privacy."
  :type 'integer
  :group 'savehist)

(defcustom savehist-autosave-interval (* 5 60)
  "The interval during which savehist should autosave the history buffer."
  :type 'integer
  :group 'savehist)

(defvar savehist-coding-system
  ;; UTF-8 is usually preferable to ISO-2022-8 when available, but under
  ;; XEmacs, UTF-8 is provided by external packages, and may not always be
  ;; available, so even if it currently is available, we prefer not to
  ;; use is.
  (if (featurep 'xemacs) 'iso-2022-8 'utf-8)
  "The coding system savehist uses for saving the minibuffer history.
Changing this value while Emacs is running is supported, but considered
unwise, unless you know what you are doing.")

;; Internal variables.

(defvar savehist-timer nil)

(defvar savehist-last-checksum nil)

(defvar savehist-minibuffer-history-variables nil)

(defconst savehist-no-conversion (if (featurep 'xemacs) 'binary 'no-conversion)
  "Coding system without conversion, only used for calculating checksums.
It should be as discriminating as `savehist-coding-system' but faster.")

;; Functions.

(defun savehist-install ()
  "Hook savehist into Emacs.
This will install `savehist-autosave' in `kill-emacs-hook' and on a timer.
To undo this, call `savehist-uninstall'."
  (add-hook 'minibuffer-setup-hook 'savehist-minibuffer-hook)
  (add-hook 'kill-emacs-hook 'savehist-autosave)
  ;; Install an invocation of savehist-autosave on a timer.  This
  ;; should not cause noticeable delays for users -- savehist-autosave
  ;; executes in under 5 ms on my system.
  (unless savehist-timer
    (setq savehist-timer
	  (if (featurep 'xemacs)
	      (start-itimer
	       "savehist" 'savehist-autosave savehist-autosave-interval
	       savehist-autosave-interval)
	    (run-with-timer savehist-autosave-interval savehist-autosave-interval
			    'savehist-autosave)))))

(defun savehist-uninstall ()
  "Undo installing savehist."
  (remove-hook 'minibuffer-setup-hook 'savehist-minibuffer-hook)
  (remove-hook 'kill-emacs-hook 'savehist-autosave)
  (when savehist-timer
    (if (featurep 'xemacs)
	(delete-itimer savehist-timer)
      (cancel-timer savehist-timer))
    (setq savehist-timer nil)))

;;;###autoload
(defun savehist-load (&optional no-install)
  "Load the minibuffer histories from `savehist-file'.
Unless NO-INSTALL is present and non-nil, the function will also install
`savehist-autosave' in `kill-emacs-hook' and on a timer, ensuring that
history is saved before leaving Emacs.

This function should be normally used from your Emacs init file.  Since
it removes your current minibuffer histories, it is unwise to call it at
any other time."
  (interactive "P")
  ;; Don't set coding-system-for-read here.  We rely on autodetection
  ;; and the coding cookie to convey that information.  That way, if
  ;; the user changes the value of savehist-coding-system, we can
  ;; still correctly load the old file.
  (load savehist-file t (not (interactive-p)))
  (unless no-install
    (savehist-install)))

;;;###autoload
(defun savehist-save (&optional auto-save)
  "Save the values of minibuffer history variables.
Unbound symbols referenced in `savehist-additional-variables' are ignored.
If AUTO-SAVE is non-nil, compare the saved contents to the one last saved,
 and don't save the buffer if they are the same."
  (interactive)
  (with-temp-buffer
    (insert
     (format ";; -*- mode: emacs-lisp; coding: %s -*-\n" savehist-coding-system)
     ";; Minibuffer history file, automatically generated by `savehist'.\n\n")
    (let ((print-length nil)
	  (print-string-length nil)
	  (print-level nil)
	  (print-readably t)
	  (print-quoted t)
	  (symbol-list (append
			(and savehist-save-minibuffer-history
			     (cons 'savehist-minibuffer-history-variables
				   savehist-minibuffer-history-variables))
			savehist-additional-variables)))
      (dolist (sym symbol-list)
	(when (boundp sym)
	  (let ((value (savehist-process-for-saving (symbol-value sym))))
	    (prin1 `(setq ,sym ',value) (current-buffer))
	    (insert ?\n)))))
    ;; If autosaving, avoid writing if nothing has changed since the
    ;; last write.
    (let ((checksum (md5 (current-buffer) nil nil savehist-no-conversion)))
      (unless (and auto-save (equal checksum savehist-last-checksum))
	;; Set file-precious-flag when saving the buffer because we
	;; don't want a half-finished write ruining the entire
	;; history.  Remember that this is run from a timer and from
	;; kill-emacs-hook, and also that multiple Emacs instances
	;; could write to this file at once.
	(let ((file-precious-flag t)
	      (coding-system-for-write savehist-coding-system))
	  (write-region (point-min) (point-max) savehist-file nil
			(unless (interactive-p) 'quiet)))
	(when savehist-modes
	  (set-file-modes savehist-file savehist-modes))
	(setq savehist-last-checksum checksum)))))

(defun savehist-autosave ()
  "Save the minibuffer history if it has been modified since the last save."
  (savehist-save t))

(defun savehist-process-for-saving (value)
  ;; Process VALUE for saving to file.  If it is a list, retain only
  ;; the first `savehist-length' values and prune non-printable ones.
  ;; If VALUE is not a list, return it as-is if it is printable and
  ;; nil otherwise.
  (cond
   ((listp value)
    (when (and savehist-length (> (length value) savehist-length))
      ;; This should be: (setq value (subseq value 0 savehist-length))
      (setq value (copy-sequence value))
      (setcdr (nthcdr savehist-length value) nil))
    ;; And this should be (remove-if-not #'savehist-printable value)
    (delq nil (mapcar (lambda (x) (if (savehist-printable x) x)) value)))
   ((savehist-printable value) value)
   (t nil)))

(defun savehist-printable (value)
  "Return non-nil if VALUE is printable."
  ;; Quick response for oft-encountered types known to be printable.
  (cond
   ((stringp value))
   ((numberp value))
   ((symbolp value))
   (t
    ;; For others, check explicitly.
    (condition-case nil
	(let ((print-readably t)
	      (print-level nil)
	      (chars ()))
	  ;; Print the value into a string...
	  (prin1 value (lambda (char) (push char chars)))
	  ;; ...and attempt to read it.
	  (read (apply #'string (nreverse chars)))
	  ;; The attempt worked: the object is printable.
	  t)
      ;; The attempt failed: the object is not printable.
      (error nil)))))

(defun savehist-minibuffer-hook ()
  (add-to-list 'savehist-minibuffer-history-variables
               minibuffer-history-variable))

(provide 'savehist)

;; arch-tag: b3ce47f4-c5ad-4ebc-ad02-73aba705cf9f
;;; savehist.el ends here
