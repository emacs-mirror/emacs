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
	       (if (delete-active-region nil)
		   (setq this-command '(lambda () (interactive)))))
	      (type
	       (delete-active-region nil))))))

(add-hook 'pre-command-hook 'delete-selection-pre-hook)

(put 'self-insert-command 'delete-selection t)
(put 'self-insert-iso 'delete-selection t)

(put 'yank 'delete-selection t)
(put 'insert-register 'delete-selection t)

(put 'delete-backward-char 'delete-selection 'supersede)
(put 'backward-delete-char-untabify 'delete-selection 'supersede)
(put 'delete-char 'delete-selection 'supersede)

(put 'newline-and-indent 'delete-selection 't)
(put 'newline 'delete-selection t)
(put 'open-line 'delete-selection t)

;;;###autoload
(defalias 'pending-delete-mode 'delete-selection-mode)
;;;###autoload
(defun delete-selection-mode (arg)
  "Toggle Delete Selection mode.
When ON, typed text replaces the selection if the selection is active.
When OFF, typed text is just inserted at point."
  (interactive "P")
  (setq delete-selection-mode
	(if (null arg) (not delete-selection-mode)
	  (> (prefix-numeric-value arg) 0)))
  (force-mode-line-update))

;; This is very useful for cancelling a selection in the minibuffer without 
;; aborting the minibuffer.
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode mode, if the mark is active, just deactivate it;
then it takes a second C-g to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (abort-recursive-edit)))

(define-key minibuffer-local-map "\C-g" 'minibuffer-keyboard-quit) 
(define-key minibuffer-local-ns-map "\C-g" 'minibuffer-keyboard-quit) 
(define-key minibuffer-local-completion-map "\C-g" 'minibuffer-keyboard-quit) 
(define-key minibuffer-local-must-match-map "\C-g" 'minibuffer-keyboard-quit) 
(define-key minibuffer-local-isearch-map "\C-g" 'minibuffer-keyboard-quit) 

(provide 'delsel)

;;; delsel.el ends here
