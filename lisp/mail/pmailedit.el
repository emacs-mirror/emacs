;;; pmailedit.el --- "PMAIL edit mode"  Edit the current message

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
  (require 'pmail)
  (require 'pmailsum))

(defcustom pmail-edit-mode-hook nil
  "List of functions to call when editing an PMAIL message."
  :type 'hook
  :version "21.1"
  :group 'pmail-edit)

(defvar pmail-old-text)

(defvar pmail-edit-map nil)
(if pmail-edit-map
    nil
  ;; Make a keymap that inherits text-mode-map.
  (setq pmail-edit-map (make-sparse-keymap))
  (set-keymap-parent pmail-edit-map text-mode-map)
  (define-key pmail-edit-map "\C-c\C-c" 'pmail-cease-edit)
  (define-key pmail-edit-map "\C-c\C-]" 'pmail-abort-edit))

;; Pmail Edit mode is suitable only for specially formatted data.
(put 'pmail-edit-mode 'mode-class 'special)

(declare-function pmail-summary-disable "" ())
(declare-function pmail-summary-enable "pmailsum" ())

(defun pmail-edit-mode ()
  "Major mode for editing the contents of an PMAIL message.
The editing commands are the same as in Text mode, together with two commands
to return to regular PMAIL:
  *  \\[pmail-abort-edit] cancels the changes
     you have made and returns to PMAIL
  *  \\[pmail-cease-edit] makes them permanent.
This functions runs the normal hook `pmail-edit-mode-hook'.
\\{pmail-edit-map}"
  (if (pmail-summary-exists)
      (save-excursion
	(set-buffer pmail-summary-buffer)
	(pmail-summary-disable)))
  (let (pmail-buffer-swapped)
    ;; Prevent change-major-mode-hook from unswapping the buffers.
    (delay-mode-hooks (text-mode))
    (use-local-map pmail-edit-map)
    (setq major-mode 'pmail-edit-mode)
    (setq mode-name "PMAIL Edit")
    (if (boundp 'mode-line-modified)
	(setq mode-line-modified (default-value 'mode-line-modified))
      (setq mode-line-format (default-value 'mode-line-format)))
    (run-mode-hooks 'pmail-edit-mode-hook)))

(defvar pmail-old-pruned nil)
(put 'pmail-old-pruned 'permanent-local t)

;;;###autoload
(defun pmail-edit-current-message ()
  "Edit the contents of this message."
  (interactive)
  (if (= pmail-total-messages 0)
      (error "No messages in this buffer"))
  (make-local-variable 'pmail-old-pruned)
  (setq pmail-old-pruned (eq pmail-header-style 'normal))
  (pmail-edit-mode)
  (make-local-variable 'pmail-old-text)
  (save-restriction
    (widen)
    (setq pmail-old-text (buffer-substring (point-min) (point-max))))
  (setq buffer-read-only nil)
  (setq buffer-undo-list nil)
  (force-mode-line-update)
  (if (and (eq (key-binding "\C-c\C-c") 'pmail-cease-edit)
	   (eq (key-binding "\C-c\C-]") 'pmail-abort-edit))
      (message "Editing: Type C-c C-c to return to Pmail, C-c C-] to abort")
    (message "%s" (substitute-command-keys
		   "Editing: Type \\[pmail-cease-edit] to return to Pmail, \\[pmail-abort-edit] to abort"))))

(defun pmail-cease-edit ()
  "Finish editing message; switch back to Pmail proper."
  (interactive)
  (if (pmail-summary-exists)
      (save-excursion
	(set-buffer pmail-summary-buffer)
	(pmail-summary-enable)))
  (widen)
  ;; Disguise any "From " lines so they don't start a new message.
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\nFrom " nil t)
      (beginning-of-line)
      (insert ">")))
  ;; Make sure buffer ends with a blank line
  ;; so as not to run this message together with the following one.
  (save-excursion
    (goto-char (point-max))
    (if (/= (preceding-char) ?\n)
	(insert "\n"))
    (unless (looking-back "\n\n")
      (insert "\n")))
  (let ((old pmail-old-text)
	character-coding is-text-message coding-system
	headers-end)
    ;; Go back to Pmail mode, but carefully.
    (force-mode-line-update)
    (let (pmail-buffer-swapped)
      (kill-all-local-variables)
      (pmail-mode-1)
      (if (boundp 'tool-bar-map)
	  (set (make-local-variable 'tool-bar-map) pmail-tool-bar-map))
      (setq buffer-undo-list t)
      (pmail-variables))
    ;; If text has really changed, mark message as edited.
    (unless (and (= (length old) (- (point-max) (point-min)))
		 (string= old (buffer-substring (point-min) (point-max))))
      (setq old nil)
      (goto-char (point-min))
      (search-forward "\n\n")
      (setq headers-end (point))

      (pmail-swap-buffers-maybe)

      (setq character-coding (mail-fetch-field "content-transfer-encoding")
	    is-text-message (pmail-is-text-p)
	    coding-system (pmail-get-coding-system))
      (if character-coding
	  (setq character-coding (downcase character-coding)))

      (narrow-to-region (pmail-msgbeg pmail-current-message)
			(pmail-msgend pmail-current-message))
      (goto-char (point-min))
      (search-forward "\n\n")
      (let ((inhibit-read-only t)
	    (headers-end-1 (point)))
	(insert-buffer-substring pmail-view-buffer headers-end)
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

    (pmail-set-attribute pmail-edited-attr-index t)
	
    ;;??? BROKEN perhaps.
    ;; I think that the Summary-Line header may not be kept there any more.
;;;       (if (boundp 'pmail-summary-vector)
;;; 	  (progn
;;; 	    (aset pmail-summary-vector (1- pmail-current-message) nil)
;;; 	    (save-excursion
;;; 	      (pmail-widen-to-current-msgbeg
;;; 		(function (lambda ()
;;; 			    (forward-line 2)
;;; 			    (if (looking-at "Summary-line: ")
;;; 				(let ((buffer-read-only nil))
;;; 				  (delete-region (point)
;;; 						 (progn (forward-line 1)
;;; 							(point)))))))))))
    )

  (save-excursion
    (pmail-show-message)
    (pmail-toggle-header (if pmail-old-pruned 1 0)))
  (run-hooks 'pmail-mode-hook))

(defun pmail-abort-edit ()
  "Abort edit of current message; restore original contents."
  (interactive)
  (widen)
  (delete-region (point-min) (point-max))
  (insert pmail-old-text)
  (pmail-cease-edit)
  (pmail-highlight-headers))

(provide 'pmailedit)

;; Local Variables:
;; change-log-default-name: "ChangeLog.pmail"
;; End:

;; arch-tag: 9524f335-12cc-4e95-9e9b-3208dc30550b
;;; pmailedit.el ends here
