;; autorevert --- Revert buffers when file on disk change.

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Author: Anders Lindgren <andersl@csd.uu.se>
;; Created: 1 Jun 1997
;; Date: 3 Jul 1997

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

;; Introduction:
;;
;; Whenever a file that Emacs is editing has been changed by another
;; program the user normally have to execute the command `revert-buffer'
;; to load the new content of the file into Emacs.
;;
;; This package contains two minor modes: Global Auto-Revert Mode and
;; Auto-Revert Mode.  Both modes automatically reverts buffers
;; whenever the corresponding files have been changed on disk.
;;
;; Auto-Revert Mode can be activated for individual buffers.
;; Global Auto-Revert Mode applies to all file buffers.
;;
;; Both modes operates by checking the time stamp of all files at
;; given intervals, the default is every five seconds.  The check is
;; aborted whenever the user actually use Emacs.  Hopefully you will
;; never even notice that this package is active (except that your
;; buffers will be reverted, of course).

;; Installation:
;;
;; To install this package, place it in somewhere on Emacs' load-path,
;; byte-compile it (not necessary), and place the following lines in
;; the appropriate init file:
;;
;; (autoload 'auto-revert-mode "autorevert" nil t)
;; (autoload 'turn-on-auto-revert-mode "autorevert" nil nil)
;; (autoload 'global-auto-revert-mode "autorevert" nil t)

;; Usage:
;;
;; Go to the appropriate buffer and press:
;;   M-x auto-revert-mode RET
;;
;; To activate Global Auto-Revert Mode, press:
;;   M-x global-auto-revert-mode RET
;;
;; To activate Global Auto-Revert Mode every time Emacs is started the
;; following line could be added to your ~/.emacs:
;;   (global-auto-revert-mode 1)
;;
;; The function `turn-on-auto-revert-mode' could be added to any major
;; mode hook to activate Auto-Revert Mode for all buffers in that
;; mode.  For example, the following line will activate Auto-Revert
;; Mode in all C mode buffers:
;;
;; (add-hook 'c-mode-hook 'turn-on-auto-revert-mode)

;;; Code:

;; Dependencies:

(require 'timer)
(eval-when-compile (require 'cl))


;; Custom Group:
;;
;; The two modes will be placed next to Auto Save Mode under the
;; Files group under Emacs.

(defgroup auto-revert nil
  "Revert individual buffer when file on disk change.

Auto-Revert Mode can be activated for individual buffer.
Global Auto-Revert Mode applies to all buffers."
  :group 'files)


;; Variables:

(defvar auto-revert-mode nil
  "*Non-nil when Auto-Revert Mode is active.

Do never set this variable directly, use the command
`auto-revert-mode' instead.")

(defcustom global-auto-revert-mode nil
  "When on, buffers are automatically reverted when files on disk change.

Set this variable when using \\[customize] only.  Otherwise, use the
command `global-auto-revert-mode' instead."
  :group 'auto-revert
  :initialize 'custom-initialize-default
  :set '(lambda (symbol value)
	  (global-auto-revert-mode (or value 0)))
  :type 'boolean
  :require 'autorevert)

(defcustom auto-revert-interval 5
  "Time, in seconds, between Auto-Revert Mode file checks."
  :group 'auto-revert
  :type 'integer)

(defcustom auto-revert-stop-on-user-input t
  "When non-nil Auto-Revert Mode stops checking files on user input."
  :group 'auto-revert
  :type 'boolean)

(defcustom auto-revert-verbose t
  "When nil, Auto-Revert Mode will not generate any messages.

Currently, messages are generated when the mode is activated or
deactivated, and whenever a file is reverted."
  :group 'auto-revert
  :type 'boolean)

(defcustom auto-revert-mode-text " ARev"
  "String to display in the mode line when Auto-Revert Mode is active.

\(When the string is not empty, make sure that it has a leading space.)"
  :tag "Auto Revert Mode Text"		; To separate it from `global-...'
  :group 'auto-revert
  :type 'string)

(defcustom auto-revert-mode-hook nil
  "Functions to run when Auto-Revert Mode is activated."
  :tag "Auto Revert Mode Hook"		; To separate it from `global-...'
  :group 'auto-revert
  :type 'hook)

(defcustom global-auto-revert-mode-text ""
  "String to display when Global Auto-Revert Mode is active.

The default is nothing since when this mode is active this text doesn't
vary neither over time, nor between buffers.  Hence a mode line text
would only waste precious space."
  :group 'auto-revert
  :type 'string)

(defcustom global-auto-revert-mode-hook nil
  "Hook called when Global Auto-Revert Mode is activated."
  :group 'auto-revert
  :type 'hook)

(defcustom global-auto-revert-non-file-buffers nil
  "*When nil only file buffers are reverted by Global Auto-Revert Mode.

When non-nil, both file buffers and buffers with a custom
`revert-buffer-function' are reverted by Global Auto-Revert Mode."
  :group 'auto-revert
  :type 'boolean)

(defcustom global-auto-revert-non-file-buffers nil
  "When nil only file buffers are reverted by Global Auto-Revert Mode.

When non-nil, both file buffers and buffers with a custom
`revert-buffer-function' are reverted by Global Auto-Revert Mode.

Use this option with care since it could lead to excessive reverts."
  :group 'auto-revert
  :type 'boolean)

(defcustom global-auto-revert-ignore-modes '()
  "List of major modes Global Auto-Revert Mode should not check."
  :group 'auto-revert
  :type '(repeat sexp))

(defcustom auto-revert-load-hook nil
  "Functions to run when Auto-Revert Mode is first loaded."
  :tag "Load Hook"
  :group 'auto-revert
  :type 'hook)

(defvar global-auto-revert-ignore-buffer nil
  "*When non-nil, Gobal Auto-Revert Mode will not revert this buffer.

This variable becomes buffer local when set in any faishon.")
(make-variable-buffer-local 'global-auto-revert-ignore-buffer)


;; Internal variables:

(defvar auto-revert-buffer-list '()
  "List of buffers in Auto-Revert Mode.

Note that only Auto-Revert Mode, never Global Auto-Revert Mode, adds
buffers to this list.

The timer function `auto-revert-buffers' is responsible for purging
the list of old buffers.")

(defvar auto-revert-timer nil
  "Timer used by Auto-Revert Mode.")

(defvar auto-revert-remaining-buffers '()
  "Buffers not checked when user input stopped execution.")


;; Functions:

;;;###autoload
(defun auto-revert-mode (&optional arg)
  "Revert buffer when file on disk change.

This is a minor mode that affect only the current buffer.
Use `global-auto-revert-mode' to automatically revert all buffers."
  (interactive "P")
  (make-local-variable 'auto-revert-mode)
  (setq auto-revert-mode
	(if (null arg)
	    (not auto-revert-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if (and auto-revert-verbose
	   (interactive-p))
      (message "Auto-Revert Mode is now %s."
	       (if auto-revert-mode "on" "off")))
  (if auto-revert-mode
      (if (not (memq (current-buffer) auto-revert-buffer-list))
	  (push (current-buffer) auto-revert-buffer-list))
    (setq auto-revert-buffer-list
	  (delq (current-buffer) auto-revert-buffer-list)))
  (auto-revert-set-timer)
  (when auto-revert-mode
    (auto-revert-buffers)
    (run-hooks 'auto-revert-mode-hook)))


;;;###autoload
(defun turn-on-auto-revert-mode ()
  "Turn on Auto-Revert Mode.

This function is designed to be added to hooks, for example:
  (add-hook 'c-mode-hook 'turn-on-auto-revert-mode)"
  (auto-revert-mode 1))


;;;###autoload
(defun global-auto-revert-mode (&optional arg)
  "Revert any buffer when file on disk change.

This is a minor mode that affect all buffers.
Use `auto-revert-mode' to revert a particular buffer."
  (interactive "P")
  (setq global-auto-revert-mode
	(if (null arg)
	    (not global-auto-revert-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if (and auto-revert-verbose
	   (interactive-p))
      (message "Gobal Auto-Revert Mode is now %s."
	       (if global-auto-revert-mode "on" "off")))
  (auto-revert-set-timer)
  (when global-auto-revert-mode
    (auto-revert-buffers)      
    (run-hooks 'global-auto-revert-mode-hook)))


(defun auto-revert-set-timer ()
  "Restart or cancel the timer."
  (if (timerp auto-revert-timer)
      (cancel-timer auto-revert-timer))
  (if (or global-auto-revert-mode auto-revert-buffer-list)
      (setq auto-revert-timer (run-with-timer auto-revert-interval
					      auto-revert-interval
					      'auto-revert-buffers))
    (setq auto-revert-timer nil)))


(defun auto-revert-buffers ()
  "Revert buffers as specified by Auto-Revert and Global Auto-Revert Mode.

Should `global-auto-revert-mode' be active all file buffers are checked.

Should `auto-revert-mode' be active in some buffers, those buffers
are checked.

Non-file buffers that have a custom `revert-buffer-function' are
reverted either when Auto-Revert Mode is active in that buffer, or
when the variable `global-auto-revert-non-file-buffers' is non-nil
and Global Auto-Revert Mode is active.

This function stops whenever the user use Emacs.  The buffers not
checked are stored in the variable `auto-revert-remaining-buffers'.

To avoid starvation, the buffers in `auto-revert-remaining-buffers'
are checked first the next time this function is called.

This function is also responslible for removing buffers no longer in
Auto-Revert mode from `auto-revert-buffer-list', and for canceling
the timer when no buffers need to be checked."
  (let ((bufs (if global-auto-revert-mode
		  (buffer-list)
		auto-revert-buffer-list))
	(remaining '())
	(new '()))
    ;; Partition `bufs' into two halves depending on whether or not
    ;; the buffers are in `auto-revert-remaining-buffers'.  The two
    ;; halves are then re-joined with the "remaining" buffers at the
    ;; head of the list.
    (dolist (buf auto-revert-remaining-buffers)
      (if (memq buf bufs)
	  (push buf remaining)))
    (dolist (buf bufs)
      (if (not (memq buf remaining))
	  (push buf new)))
    (setq bufs (nreverse (nconc new remaining)))
    (while (and bufs
		(not (and auto-revert-stop-on-user-input
			  (input-pending-p))))
      (let ((buf (car bufs)))
	(if (buffer-name buf)		; Buffer still alive?
	    (save-excursion
	      (set-buffer buf)
	      ;; Test if someone has turned off Auto-Revert Mode in a
	      ;; non-standard way, for example by changing major mode.
	      (if (and (not auto-revert-mode)
		       (memq buf auto-revert-buffer-list))
		  (setq auto-revert-buffer-list
			(delq buf auto-revert-buffer-list)))
	      (when (and
		     (or auto-revert-mode
			 (and
			  global-auto-revert-mode
			  (not global-auto-revert-ignore-buffer)
			  (not (memq major-mode
				     global-auto-revert-ignore-modes))))
		     (not (buffer-modified-p))
		     (if (buffer-file-name)
			 (and (file-readable-p (buffer-file-name))
			      (not (verify-visited-file-modtime buf)))
		       (and revert-buffer-function
			    (or (and global-auto-revert-mode
				     global-auto-revert-non-file-buffers)
				auto-revert-mode))))
		(if auto-revert-verbose
		    (message "Reverting buffer `%s'." buf))
		(revert-buffer t t)))
	  ;; Remove dead buffer from `auto-revert-buffer-list'.
	  (setq auto-revert-buffer-list
		(delq buf auto-revert-buffer-list))))
      (setq bufs (cdr bufs)))
    (setq auto-revert-remaining-buffers bufs)
    ;; Check if we should cancel the timer.
    (when (and (not global-auto-revert-mode)
	       (null auto-revert-buffer-list))
      (cancel-timer auto-revert-timer)
      (setq auto-revert-timer nil))))


;; The end:

(unless (assq 'auto-revert-mode minor-mode-alist)
  (push '(auto-revert-mode auto-revert-mode-text)
	minor-mode-alist))
(unless (assq 'global-auto-revert-mode minor-mode-alist)
  (push '(global-auto-revert-mode global-auto-revert-mode-text)
	minor-mode-alist))

(provide 'autorevert)

(run-hooks 'auto-revert-load-hook)

;; This makes it possible to set Global Auto-Revert Mode from
;; Customize.
(if global-auto-revert-mode
    (global-auto-revert-mode 1))

;; autorevert.el ends here.
