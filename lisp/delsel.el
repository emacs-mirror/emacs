;;; Pending delete selection
;;; Copyright (C) 1992 Free Software Foundation, Inc.
;;; Created: 14 Jul 92, Matthieu Devin <devin@lucid.com>
;;; Last change  18-Feb-93, devin.

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


;;; This files makes the active region be pending delete, meaning that
;;; text inserted while the region is active will replace the region contents.
;;; This is a popular behavior of personal computers text editors.

(defun delete-active-region (&optional killp)
  (if (and (not buffer-read-only)
	   (extentp primary-selection-extent)
	   (eq (current-buffer) (extent-buffer primary-selection-extent))
	   (< 0 (extent-start-position primary-selection-extent))
	   (< 0 (extent-end-position primary-selection-extent)))
      (progn
	(if killp
	    (kill-region (extent-start-position primary-selection-extent)
			 (extent-end-position primary-selection-extent))
	  (delete-region (extent-start-position primary-selection-extent)
			 (extent-end-position primary-selection-extent)))
	(zmacs-deactivate-region)
	t)))

(defun pending-delete-pre-hook ()
  (let ((type (and (symbolp this-command)
		   (get this-command 'pending-delete))))
    (cond ((eq type 'kill)
	   (delete-active-region t))
	  ((eq type 'supersede)
	   (if (delete-active-region ())
	       (setq this-command '(lambda () (interactive)))))
	  (type
	   (delete-active-region ())))))

(put 'self-insert-command 'pending-delete t)

(put 'yank 'pending-delete t)
(put 'x-yank-clipboard-selection 'pending-delete t)

(put 'delete-backward-char 'pending-delete 'supersede)
(put 'backward-delete-char-untabify 'pending-delete 'supersede)
(put 'delete-char 'pending-delete 'supersede)

(put 'newline-and-indent 'pending-delete 't)
(put 'newline 'pending-delete t)
(put 'open-line 'pending-delete t)

(defun pending-delete-mode ()
  "Toggle the state of pending-delete mode.
When ON, typed text replaces the selection if the selection is active.
When OFF, typed text is just inserted at point."
  (interactive)
  (if (memq 'pending-delete-pre-hook pre-command-hook)
      (progn
	(remove-hook 'pre-command-hook 'pending-delete-pre-hook)
	(message "pending delete is OFF"))
    (progn
      (add-hook 'pre-command-hook 'pending-delete-pre-hook)
      (message
       "Pending delete is ON, use M-x pending-delete to turn it OFF"))))

(pending-delete-mode)

;; This new definition of control-G makes the first control-G disown the 
;; selection and the second one signal a QUIT.
;; This is very useful for cancelling a selection in the minibuffer without 
;; aborting the minibuffer.
;; It has actually nothing to do with pending-delete but its more necessary
;; with pending delete because pending delete users use the selection more.
(defun keyboard-quit ()
  "Signal a `quit' condition.
If this character is typed while lisp code is executing, it will be treated
 as an interrupt.
If this character is typed at top-level, this simply beeps.

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

(provide 'pending-del)

;; End of pending-del.el.
