;;; rmailedit.el --- "RMAIL edit mode"  Edit the current message

;; Copyright (C) 1985, 1994, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
;;   2008, 2009  Free Software Foundation, Inc.

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

(require 'rmail)

(defcustom rmail-edit-mode-hook nil
  "List of functions to call when editing an RMAIL message."
  :type 'hook
  :version "21.1"
  :group 'rmail-edit)


(defvar rmail-edit-map
  (let ((map (make-sparse-keymap)))
    ;; Make a keymap that inherits text-mode-map.
    (set-keymap-parent map text-mode-map)
    (define-key map "\C-c\C-c" 'rmail-cease-edit)
    (define-key map "\C-c\C-]" 'rmail-abort-edit)
    map))

(declare-function rmail-summary-disable "rmailsum" ())

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
      (with-current-buffer rmail-summary-buffer
	(rmail-summary-disable)))
  (let ((rmail-buffer-swapped nil)) ; Prevent change-major-mode-hook
                                    ; from unswapping the buffers.
    (delay-mode-hooks (text-mode))
    (use-local-map rmail-edit-map)
    (setq major-mode 'rmail-edit-mode)
    (setq mode-name "RMAIL Edit")
    (if (boundp 'mode-line-modified)
	(setq mode-line-modified (default-value 'mode-line-modified))
      (setq mode-line-format (default-value 'mode-line-format)))
    (run-mode-hooks 'rmail-edit-mode-hook)))

;; Rmail Edit mode is suitable only for specially formatted data.
(put 'rmail-edit-mode 'mode-class 'special)


(defvar rmail-old-text)
(defvar rmail-old-pruned nil
  "Non-nil means the message being edited originally had pruned headers.")
(put 'rmail-old-pruned 'permanent-local t)

;;;###autoload
(defun rmail-edit-current-message ()
  "Edit the contents of this message."
  (interactive)
  (if (zerop rmail-total-messages)
      (error "No messages in this buffer"))
  (set (make-local-variable 'rmail-old-pruned) (rmail-msg-is-pruned))
  (rmail-edit-mode)
  (set (make-local-variable 'rmail-old-text)
       (save-restriction
	 (widen)
	 (buffer-substring (point-min) (point-max))))
  (setq buffer-read-only nil)
  (setq buffer-undo-list nil)
  ;; FIXME whether the buffer is initially marked as modified or not
  ;; depends on whether or not the underlying rmail buffer was so marked.
  ;; Seems poor.
  (force-mode-line-update)
  (if (and (eq (key-binding "\C-c\C-c") 'rmail-cease-edit)
	   (eq (key-binding "\C-c\C-]") 'rmail-abort-edit))
      (message "Editing: Type C-c C-c to return to Rmail, C-c C-] to abort")
    (message "%s" (substitute-command-keys
		   "Editing: Type \\[rmail-cease-edit] to return to Rmail, \\[rmail-abort-edit] to abort"))))


(declare-function rmail-summary-enable "rmailsum" ())

(defun rmail-cease-edit ()
  "Finish editing message; switch back to Rmail proper."
  (interactive)
  (if (rmail-summary-exists)
      (with-current-buffer rmail-summary-buffer
	(rmail-summary-enable)))
  (widen)
  ;; Disguise any "From " lines so they don't start a new message.
  (save-excursion
    (goto-char (point-min))
    (or rmail-old-pruned (forward-line 1))
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
	(pruned rmail-old-pruned)
	character-coding is-text-message coding-system
	headers-end limit)
    ;; Go back to Rmail mode, but carefully.
    (force-mode-line-update)
    (let ((rmail-buffer-swapped nil)) ; Prevent change-major-mode-hook
                                      ; from unswapping the buffers.
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
      (narrow-to-region (rmail-msgbeg rmail-current-message)
			(rmail-msgend rmail-current-message))
      (save-restriction
	(setq limit
	      (save-excursion
		(goto-char (point-min))
		(search-forward "\n\n" nil t)))
	;; All 3 of the functions we call below assume the buffer was
	;; narrowed to just the headers of the message.
	(narrow-to-region (rmail-msgbeg rmail-current-message) limit)
	(setq character-coding
	      (mail-fetch-field "content-transfer-encoding")
	      is-text-message (rmail-is-text-p)
	      coding-system (rmail-get-coding-system)))
      (if character-coding
	  (setq character-coding (downcase character-coding)))

      (goto-char limit)
      (let ((inhibit-read-only t))
	(let ((data-buffer (current-buffer))
	      (end (copy-marker (point) t)))
	  (with-current-buffer rmail-view-buffer
	    (encode-coding-region headers-end (point-max) coding-system
				  data-buffer))
	  (delete-region end (point-max)))

	;; Re-apply content-transfer-encoding, if any, on the message body.
	(cond
	 ((string= character-coding "quoted-printable")
	  (mail-quote-printable-region (point) (point-max)))
	 ((and (string= character-coding "base64") is-text-message)
	  (base64-encode-region (point) (point-max)))
	 ((and (eq character-coding 'uuencode) is-text-message)
	  (error "uuencoded messages are not supported"))))
      (rmail-set-attribute rmail-edited-attr-index t))
    ;;??? BROKEN perhaps.
;;;    (if (boundp 'rmail-summary-vector)
;;;	(aset rmail-summary-vector (1- rmail-current-message) nil))
    (save-excursion
      (rmail-show-message)
      (rmail-toggle-header (if pruned 1 0))))
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
