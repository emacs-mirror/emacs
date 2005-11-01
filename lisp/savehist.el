;;; savehist.el --- Save minibuffer history.

;; Copyright (C) 1997,2005 Free Software Foundation

;; Author: Hrvoje Niksic <hniksic@xemacs.org>
;; Keywords: minibuffer
;; Version: 19

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

;; To use savehist, turn on savehist-mode by putting the following in
;; `~/.emacs':
;;
;;     (savehist-mode 1)
;;
;; or with customize: `M-x customize-option RET savehist-mode RET'.
;;
;; You can also explicitly save history with `M-x savehist-save' and
;; load it by loading the `savehist-file' with `M-x load'.

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

;;;###autoload
(defcustom savehist-mode nil
  "Mode for automatic saving of minibuffer history.
Set this by calling the `savehist-mode' function or using the customize
interface."
  :type 'boolean
  :set (lambda (symbol value) (savehist-mode (or value 0)))
  :initialize 'custom-initialize-default
  :require 'savehist
  :group 'savehist)

(defcustom savehist-save-minibuffer-history t
  "*If non-nil, save all recorded minibuffer histories.
If you want to save only specific histories, use `savehist-save-hook' to
modify the value of `savehist-minibuffer-history-variables'."
  :type 'boolean
  :group 'savehist)

(defcustom savehist-additional-variables ()
  "*List of additional variables to save.
Each element is a symbol whose value will be persisted across Emacs
sessions that use savehist.  The contents of variables should be
printable with the Lisp printer.  You don't need to add minibuffer
history variables to this list, all minibuffer histories will be
saved automatically as long as `savehist-save-minibuffer-history' is
non-nil.

User options should be saved with the customize interface.  This
list is useful for saving automatically updated variables that are not
minibuffer histories, such as `compile-command' or `kill-ring'."
  :type '(repeat variable)
  :group 'savehist)

(defcustom savehist-file
  (cond
   ;; Backward compatibility with previous versions of savehist.
   ((file-exists-p "~/.emacs-history") "~/.emacs-history")
   ((and (not (featurep 'xemacs)) (file-directory-p "~/.emacs.d/"))
    "~/.emacs.d/history")
   ((and (featurep 'xemacs) (file-directory-p "~/.xemacs/"))
    "~/.xemacs/history")
   ;; For users without `~/.emacs.d/' or `~/.xemacs/'.
   (t "~/.emacs-history"))
  "*File name where minibuffer history is saved to and loaded from.
The minibuffer history is a series of Lisp expressions loaded
automatically when `savehist-mode' is turned on.  See `savehist-mode'
for more details.

If you want your minibuffer history shared between Emacs and XEmacs,
customize this value and make sure that `savehist-coding-system' is
set to a coding system that exists in both emacsen."
  :type 'file
  :group 'savehist)

(defcustom savehist-file-modes #o600
  "*Default permissions of the history file.
This is decimal, not octal.  The default is 384 (0600 in octal).
Set to nil to use the default permissions that Emacs uses, typically
mandated by umask.  The default is a bit more restrictive to protect
the user's privacy."
  :type 'integer
  :group 'savehist)

(defcustom savehist-autosave-interval (* 5 60)
  "*The interval between autosaves of minibuffer history.
If set to nil, disables timer-based autosaving."
  :type 'integer
  :group 'savehist)

(defcustom savehist-save-hook nil
  "Hook called by savehist-save before saving the variables.
You can use this hook to influence choice and content of variables to
save."
  :type 'hook)

(defvar savehist-coding-system (if (featurep 'xemacs) 'iso-2022-8 'utf-8)
  "The coding system savehist uses for saving the minibuffer history.
Changing this value while Emacs is running is supported, but considered
unwise, unless you know what you are doing.")

;; Internal variables.

(defvar savehist-timer nil)

(defvar savehist-last-checksum nil)

(defvar savehist-minibuffer-history-variables nil
  "List of minibuffer histories.
The contents of this variable is built while Emacs is running, and saved
along with minibuffer history.  You can change its value off
`savehist-save-hook' to influence which variables are saved.")

;; Coding system without any conversion, used for calculating an
;; internal checksum.  Should be as fast as possible, ideally simply
;; exposing the internal representation of buffer text.
(defconst savehist-no-conversion (if (featurep 'xemacs) 'binary 'no-conversion))

;; Whether the history has already been loaded.  This prevents
;; toggling savehist-mode from destroying existing minibuffer history.
(defvar savehist-loaded nil)

(eval-when-compile
  (when (featurep 'xemacs)
    ;; Must declare this under XEmacs, which doesn't have built-in
    ;; minibuffer history truncation.
    (defvar history-length 100)))

;; Functions.

;;;###autoload
(defun savehist-mode (arg)
  "Toggle savehist-mode.
Positive ARG turns on `savehist-mode'.  When on, savehist-mode causes
minibuffer history to be saved periodically and when exiting Emacs.
When turned on for the first time in an Emacs session, it causes the
previous minibuffer history to be loaded from `savehist-file'.

This mode should normally be turned on from your Emacs init file.
Calling it at any other time replaces your current minibuffer histories,
which is probably undesirable."
  (interactive "P")
  (setq savehist-mode
	(if (null arg)
	    (not savehist-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if (not savehist-mode)
      (savehist-uninstall)
    (when (and (not savehist-loaded)
	       (file-exists-p savehist-file))
      (condition-case errvar
	  (progn
	    ;; Don't set coding-system-for-read -- we rely on the
	    ;; coding cookie to convey that information.  That way, if
	    ;; the user changes the value of savehist-coding-system,
	    ;; we can still correctly load the old file.
	    (load savehist-file nil (not (interactive-p)))
	    (setq savehist-loaded t))
	(error
	 ;; Don't install the mode if reading failed.  Doing so would
	 ;; effectively destroy the user's data at the next save.
	 (setq savehist-mode nil)
	 (savehist-uninstall)
	 (signal (car errvar) (cdr errvar)))))
    (savehist-install)))
(add-minor-mode 'savehist-mode "")

(defun savehist-load ()
  "Obsolete function provided for transition from old versions of savehist.
Don't call this from new code, use (savehist-mode 1) instead.

This function loads the variables stored in `savehist-file' and turns on
savehist-mode.  If savehist-file is in the old format that doesn't record
the value of `savehist-minibuffer-history-variables', that value is
deducted from the contents of the file."
  (savehist-mode 1)
  ;; Old versions of savehist distributed with XEmacs didn't save
  ;; savehist-minibuffer-history-variables.  If that variable is nil
  ;; after loading the file, try to intuit the intended value.
  (when (null savehist-minibuffer-history-variables)
    (setq savehist-minibuffer-history-variables
          (with-temp-buffer
	    (ignore-errors
	      (insert-file-contents savehist-file))
            (let ((vars ()) form)
              (while (setq form (condition-case nil
				    (read (current-buffer)) (error nil)))
		;; Each form read is of the form (setq VAR VALUE).
		;; Collect VAR, i.e. (nth form 1).
                (push (nth 1 form) vars))
              vars)))))
(make-obsolete 'savehist-load 'savehist-mode)

(defun savehist-install ()
  "Hook savehist into Emacs.
Normally invoked by calling `savehist-mode' to set the minor mode.
Installs `savehist-autosave' in `kill-emacs-hook' and on a timer.   To
undo this, call `savehist-uninstall'."
  (add-hook 'minibuffer-setup-hook 'savehist-minibuffer-hook)
  (add-hook 'kill-emacs-hook 'savehist-autosave)
  ;; Install an invocation of savehist-autosave on a timer.  This
  ;; should not cause noticeable delays for users -- savehist-autosave
  ;; executes in under 5 ms on my system.
  (when (and savehist-autosave-interval
	     (null savehist-timer))
    (setq savehist-timer
	  (if (featurep 'xemacs)
	      (start-itimer
	       "savehist" 'savehist-autosave savehist-autosave-interval
	       savehist-autosave-interval)
	    (run-with-timer savehist-autosave-interval
			    savehist-autosave-interval 'savehist-autosave)))))

(defun savehist-uninstall ()
  "Undo installing savehist.
Normally invoked by calling `savehist-mode' to unset the minor mode."
  (remove-hook 'minibuffer-setup-hook 'savehist-minibuffer-hook)
  (remove-hook 'kill-emacs-hook 'savehist-autosave)
  (when savehist-timer
    (if (featurep 'xemacs)
	(delete-itimer savehist-timer)
      (cancel-timer savehist-timer))
    (setq savehist-timer nil)))

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
    (run-hooks 'savehist-save-hook)
    (let ((print-length nil)
	  (print-string-length nil)
	  (print-level nil)
	  (print-readably t)
	  (print-quoted t))
      ;; Save the minibuffer histories, along with the value of
      ;; savehist-minibuffer-history-variables itself.
      (when savehist-save-minibuffer-history
	(prin1 `(setq savehist-minibuffer-history-variables
		      ',savehist-minibuffer-history-variables)
	       (current-buffer))
	(insert ?\n)
	(dolist (symbol savehist-minibuffer-history-variables)
	  (when (boundp symbol)
	    (let ((value (savehist-trim-history (symbol-value symbol))))
	      (when value		; don't save empty histories
		(prin1 `(setq ,symbol ',value) (current-buffer))
		(insert ?\n))))))
      ;; Save the additional variables.
      (dolist (symbol savehist-additional-variables)
	(when (boundp symbol)
	  (let ((value (symbol-value symbol)))
	    (when (savehist-printable value)
	      (prin1 `(setq ,symbol ',value) (current-buffer))
	      (insert ?\n))))))
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
	(when savehist-file-modes
	  (set-file-modes savehist-file savehist-file-modes))
	(setq savehist-last-checksum checksum)))))

(defun savehist-autosave ()
  "Save the minibuffer history if it has been modified since the last save.
Does nothing if savehist-mode is off."
  (when savehist-mode
    (savehist-save t)))

(defun savehist-trim-history (value)
  ;; Retain only the first history-length items in VALUE.  Only used
  ;; under XEmacs, which doesn't (yet) implement automatic trimming of
  ;; history lists to history-length items.
  (if (and (featurep 'xemacs)
	   (natnump history-length)
	   (> (length value) history-length))
      ;; Equivalent to `(subseq value 0 history-length)', but doesn't
      ;; need cl-extra at run-time.
      (loop repeat history-length collect (pop value))
    value))

(defun savehist-printable (value)
  "Return non-nil if VALUE is printable."
  (cond
   ;; Quick response for oft-encountered types known to be printable.
   ((stringp value))
   ((numberp value))
   ((symbolp value))
   (t
    ;; For others, check explicitly.
    (with-temp-buffer
      (condition-case nil
	  (let ((print-readably t) (print-level nil))
	  ;; Print the value into a buffer...
	  (prin1 value (current-buffer))
	  ;; ...and attempt to read it.
	  (read (point-min-marker))
	  ;; The attempt worked: the object is printable.
	  t)
	;; The attempt failed: the object is not printable.
	(error nil))))))

(defun savehist-minibuffer-hook ()
  (add-to-list 'savehist-minibuffer-history-variables
	       minibuffer-history-variable))

(provide 'savehist)

;; arch-tag: b3ce47f4-c5ad-4ebc-ad02-73aba705cf9f

;;; savehist.el ends here
