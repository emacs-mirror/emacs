;;; delsel.el --- delete selection if you insert

;;; Copyright (C) 1992 Free Software Foundation, Inc.

;; Author: Matthieu Devin <devin@lucid.com>
;; Created: 14 Jul 92
;; Last change  18-Feb-93, devin.

;;; This file is part of GNU Emacs.

;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; This file makes the active region be pending delete, meaning that
;;; text inserted while the region is active will replace the region contents.
;;; This is a popular behavior of personal computers text editors.

;;; Code:

(defvar delete-selection-mode t
  "*Non-nil means Delete Selection mode is enabled.
In Delete Selection mode, when a region is highlighted,
insertion commands first delete the region and then insert.")

(defun delete-active-region (&optional killp)
  (if killp
      (kill-region (point) (mark))
    (delete-region (point) (mark)))
  (setq mark-active nil)
  (run-hooks 'deactivate-mark-hook)
  t)

(defun delete-selection-pre-hook ()
  (if (and delete-selection-mode
	   (not buffer-read-only)
	   transient-mark-mode mark-active)
      (let ((type (and (symbolp this-command)
		       (get this-command 'delete-selection))))
	(cond ((eq type 'kill)
	       (delete-active-region t))
	      ((eq type 'supersede)
	       (if (delete-active-region ())
		   (setq this-command '(lambda () (interactive)))))
	      (type
	       (delete-active-region ()))))))

(add-hook 'pre-command-hook 'delete-selection-pre-hook)

(put 'self-insert-command 'delete-selection t)

(put 'yank 'delete-selection t)
(put 'x-yank-clipboard-selection 'delete-selection t)

(put 'delete-backward-char 'delete-selection 'supersede)
(put 'backward-delete-char-untabify 'delete-selection 'supersede)
(put 'delete-char 'delete-selection 'supersede)

(put 'newline-and-indent 'delete-selection 't)
(put 'newline 'delete-selection t)
(put 'open-line 'delete-selection t)

(defalias 'pending-delete-mode 'delete-selection-mode)
(defun delete-selection-mode (arg)
  "Toggle Delete Selection mode.
When ON, typed text replaces the selection if the selection is active.
When OFF, typed text is just inserted at point."
  (interactive "P")
  (setq delete-selection-mode
	(if (null arg) (not delete-selection-mode)
	  (> (prefix-numeric-value arg) 0)))
  (set-buffer-modified-p (buffer-modified-p))) ;No-op, but updates mode line.

;; This new definition of control-G makes the first control-G disown the 
;; selection and the second one signal a QUIT.
;; This is very useful for cancelling a selection in the minibuffer without 
;; aborting the minibuffer.
;; It has actually nothing to do with delete-selection but its more necessary
;; with pending delete because pending delete users use the selection more.
(defun keyboard-quit ()
  "Signal a `quit' condition.
During execution of Lisp code, this character causes a quit directly.
At top-level, as an editor command, this simply beeps.
In Transient Mark mode, if the mark is active, just deactivate it."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (progn
	;; Don't beep if just deactivating the region.
	(setq mark-active nil)
	(run-hooks 'deactivate-mark-hook))
    (signal 'quit nil)))

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Transient Mark mode, if the mark is active, just deactivate it."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (progn
	;; Don't beep if just deactivating the region.
	(setq mark-active nil)
	(run-hooks 'deactivate-mark-hook))
    (abort-recursive-edit)))

(define-key minibuffer-local-map "\C-g" 'minibuffer-keyboard-quit) 
(define-key minibuffer-local-ns-map "\C-g" 'minibuffer-keyboard-quit) 
(define-key minibuffer-local-completion-map "\C-g" 'minibuffer-keyboard-quit) 
(define-key minibuffer-local-must-match-map "\C-g" 'minibuffer-keyboard-quit) 
(define-key minibuffer-local-isearch-map "\C-g" 'minibuffer-keyboard-quit) 

(provide 'pending-del)

;;; pending-del.el ends here
