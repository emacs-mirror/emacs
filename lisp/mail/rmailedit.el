;;; rmailedit.el --- "RMAIL edit mode"  Edit the current message

;; Copyright (C) 1985, 1994, 2001, 2002, 2003, 2004, 2005, 2006,
;;   2007, 2008, 2009 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: mail

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'rmail)
  (require 'rmailsum))

(defcustom rmail-edit-mode-hook nil
  "List of functions to call when editing an RMAIL message."
  :type 'hook
  :version "21.1"
  :group 'rmail-edit)

(defvar rmail-old-text)

(defvar rmail-edit-map nil)
(if rmail-edit-map
    nil
  ;; Make a keymap that inherits text-mode-map.
  (setq rmail-edit-map (make-sparse-keymap))
  (set-keymap-parent rmail-edit-map text-mode-map)
  (define-key rmail-edit-map "\C-c\C-c" 'rmail-cease-edit)
  (define-key rmail-edit-map "\C-c\C-]" 'rmail-abort-edit))

;; Rmail Edit mode is suitable only for specially formatted data.
(put 'rmail-edit-mode 'mode-class 'special)

(declare-function rmail-summary-disable "rmailsum" ())
(declare-function rmail-summary-enable "rmailsum" ())

(defun rmail-edit-mode ()
  "Major mode for editing the contents of an RMAIL message.
The editing commands are the same as in Text mode, together with two commands
to return to regular RMAIL:
  *  \\[rmail-abort-edit] cancels the changes
     you have made and returns to RMAIL
  *  \\[rmail-cease-edit] makes them permanent.
This functions runs the normal hook `rmail-edit-mode-hook'.
\\{rmail-edit-map}"
  (if (rmail-summary-exists)
      (save-excursion
	(set-buffer rmail-summary-buffer)
	(rmail-summary-disable)))
  (let (rmail-buffer-swapped)
    ;; Prevent change-major-mode-hook from unswapping the buffers.
    (delay-mode-hooks (text-mode))
    (use-local-map rmail-edit-map)
    (setq major-mode 'rmail-edit-mode)
    (setq mode-name "RMAIL Edit")
    (if (boundp 'mode-line-modified)
	(setq mode-line-modified (default-value 'mode-line-modified))
      (setq mode-line-format (default-value 'mode-line-format)))
    (run-mode-hooks 'rmail-edit-mode-hook)))

(defvar rmail-old-pruned nil)
(put 'rmail-old-pruned 'permanent-local t)

;;;###autoload
(defun rmail-edit-current-message ()
  "Edit the contents of this message."
  (interactive)
  (if (= rmail-total-messages 0)
      (error "No messages in this buffer"))
  (make-local-variable 'rmail-old-pruned)
  (setq rmail-old-pruned (eq rmail-header-style 'normal))
  (rmail-edit-mode)
  (make-local-variable 'rmail-old-text)
  (save-restriction
    (widen)
    (setq rmail-old-text (buffer-substring (point-min) (point-max))))
  (setq buffer-read-only nil)
  (setq buffer-undo-list nil)
  (force-mode-line-update)
  (if (and (eq (key-binding "\C-c\C-c") 'rmail-cease-edit)
	   (eq (key-binding "\C-c\C-]") 'rmail-abort-edit))
      (message "Editing: Type C-c C-c to return to Rmail, C-c C-] to abort")
    (message "%s" (substitute-command-keys
		   "Editing: Type \\[rmail-cease-edit] to return to Rmail, \\[rmail-abort-edit] to abort"))))

(defun rmail-cease-edit ()
  "Finish editing message; switch back to Rmail proper."
  (interactive)
  (if (rmail-summary-exists)
      (save-excursion
	(set-buffer rmail-summary-buffer)
	(rmail-summary-enable)))
  (widen)
  ;; Disguise any "From " lines so they don't start a new message.
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^>*From " nil t)
      (beginning-of-line)
      (insert ">")
      (forward-line)))
  ;; Make sure buffer ends with a blank line
  ;; so as not to run this message together with the following one.
  (save-excursion
    (goto-char (point-max))
    (if (/= (preceding-char) ?\n)
	(insert "\n"))
    (unless (looking-back "\n\n")
      (insert "\n")))
  (let ((old rmail-old-text)
	character-coding is-text-message coding-system
	headers-end)
    ;; Go back to Rmail mode, but carefully.
    (force-mode-line-update)
    (let (rmail-buffer-swapped)
      (kill-all-local-variables)
      (rmail-mode-1)
      (if (boundp 'tool-bar-map)
	  (set (make-local-variable 'tool-bar-map) rmail-tool-bar-map))
      (setq buffer-undo-list t)
      (rmail-variables))
    ;; If text has really changed, mark message as edited.
    (unless (and (= (length old) (- (point-max) (point-min)))
		 (string= old (buffer-substring (point-min) (point-max))))
      (setq old nil)
      (goto-char (point-min))
      (search-forward "\n\n")
      (setq headers-end (point))

      (rmail-swap-buffers-maybe)

      (setq character-coding (mail-fetch-field "content-transfer-encoding")
	    is-text-message (rmail-is-text-p)
	    coding-system (rmail-get-coding-system))
      (if character-coding
	  (setq character-coding (downcase character-coding)))

      (narrow-to-region (rmail-msgbeg rmail-current-message)
			(rmail-msgend rmail-current-message))
      (goto-char (point-min))
      (search-forward "\n\n")
      (let ((inhibit-read-only t)
	    (headers-end-1 (point)))
	(insert-buffer-substring rmail-view-buffer headers-end)
	(delete-region (point) (point-max))

	;; Re-encode the message body in whatever
	;; way it was decoded.
	(cond
	 ((string= character-coding "quoted-printable")
	  (mail-quote-printable-region headers-end-1 (point-max)))
	 ((and (string= character-coding "base64") is-text-message)
	  (base64-encode-region headers-end-1 (point-max)))
	 ((eq character-coding 'uuencode)
	  (error "Not supported yet."))
	 (t
	  (if (or (not coding-system) (not (coding-system-p coding-system)))
	      (setq coding-system 'undecided))
	  (encode-coding-region headers-end-1 (point-max) coding-system)))
	))

    (rmail-set-attribute rmail-edited-attr-index t)
	
    ;;??? BROKEN perhaps.
    ;; I think that the Summary-Line header may not be kept there any more.
;;;       (if (boundp 'rmail-summary-vector)
;;; 	  (progn
;;; 	    (aset rmail-summary-vector (1- rmail-current-message) nil)
;;; 	    (save-excursion
;;; 	      (rmail-widen-to-current-msgbeg
;;; 		(function (lambda ()
;;; 			    (forward-line 2)
;;; 			    (if (looking-at "Summary-line: ")
;;; 				(let ((buffer-read-only nil))
;;; 				  (delete-region (point)
;;; 						 (progn (forward-line 1)
;;; 							(point)))))))))))
    )

  (save-excursion
    (rmail-show-message)
    (rmail-toggle-header (if rmail-old-pruned 1 0)))
  (run-hooks 'rmail-mode-hook))

(defun rmail-abort-edit ()
  "Abort edit of current message; restore original contents."
  (interactive)
  (widen)
  (delete-region (point-min) (point-max))
  (insert rmail-old-text)
  (rmail-cease-edit)
  (rmail-highlight-headers))

(provide 'rmailedit)

;; arch-tag: 9524f335-12cc-4e95-9e9b-3208dc30550b
;;; rmailedit.el ends here
