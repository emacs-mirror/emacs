;;; undigest.el --- digest-cracking support for the RMAIL mail reader

;; Copyright (C) 1985, 1986, 1994, 1996 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: mail

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

;; See Internet RFC 934

;;; Code:

(require 'rmail)

(defcustom rmail-digest-end-regexp (concat "End of.*Digest.*\n"
					   (regexp-quote "*********") "*"
					   "\\(\n------*\\)*")
  "*Regexp matching the end of a digest message."
  :group 'rmail
  :type 'regexp)

;;;###autoload
(defun undigestify-rmail-message ()
  "Break up a digest message into its constituent messages.
Leaves original message, deleted, before the undigestified messages."
  (interactive)
  (with-current-buffer rmail-buffer
    (widen)
    (let ((buffer-read-only nil)
	  (msg-string (buffer-substring (rmail-msgbeg rmail-current-message)
					(rmail-msgend rmail-current-message))))
      (goto-char (rmail-msgend rmail-current-message))
      (narrow-to-region (point) (point))
      (insert msg-string)
      (narrow-to-region (point-min) (1- (point-max))))
    (let ((error t)
	  (buffer-read-only nil))
      (unwind-protect
	  (progn
	    (save-restriction
	      (goto-char (point-min))
	      (delete-region (point-min)
			     (progn (search-forward "\n*** EOOH ***\n")
				    (point)))
	      (insert "\^_\^L\n0, unseen,,\n*** EOOH ***\n")
	      (narrow-to-region (point)
				(point-max))
	      (let* ((fill-prefix "")
		     (case-fold-search t)
		     start
		     (digest-name
		      (mail-strip-quoted-names
		       (or (save-restriction
			     (search-forward "\n\n")
			     (setq start (point))
			     (narrow-to-region (point-min) (point))
			     (goto-char (point-max))
			     (or (mail-fetch-field "Reply-To")
				 (mail-fetch-field "To")
				 (mail-fetch-field "Apparently-To")
				 (mail-fetch-field "From")))
			   (error "Message is not a digest--bad header")))))
		(save-excursion
		  (goto-char (point-max))
		  (skip-chars-backward " \t\n")
		  (let (found)
		    ;; compensate for broken un*x digestifiers.  Sigh Sigh.
		    (while (and (> (point) start) (not found))
		      (forward-line -1)
		      (if (looking-at rmail-digest-end-regexp)
			  (setq found t)))
		    (if (not found)
			(error "Message is not a digest--no end line"))))
		(re-search-forward (concat "^" (make-string 55 ?-) "-*\n*"))
		(replace-match "\^_\^L\n0, unseen,,\n*** EOOH ***\n")
		(save-restriction
		  (narrow-to-region (point)
				    (progn (search-forward "\n\n")
					   (point)))
		  (if (mail-fetch-field "To") nil
		    (goto-char (point-min))
		    (insert "To: " digest-name "\n")))
		(while (re-search-forward
			(concat "\n\n" (make-string 27 ?-) "-*\n*")
			nil t)
		  (replace-match "\n\n\^_\^L\n0, unseen,,\n*** EOOH ***\n")
		  (save-restriction
		    (if (looking-at "End ")
			(insert "To: " digest-name "\n\n")
		      (narrow-to-region (point)
					(progn (search-forward "\n\n"
							       nil 'move)
					       (point))))
		    (if (mail-fetch-field "To")
			nil
		      (goto-char (point-min))
		      (insert "To: " digest-name "\n")))
		  ;; Digestifiers may insert `- ' on lines that start with `-'.
		  ;; Undo that.
		  (save-excursion
		    (goto-char (point-min))
		    (if (re-search-forward
			 "\n\n----------------------------*\n*"
			 nil t)
			(let ((end (point-marker)))
			  (goto-char (point-min))
			  (while (re-search-forward "^- " end t)
			    (delete-char -2)))))
		  )))
	    (setq error nil)
	    (message "Message successfully undigestified")
	    (let ((n rmail-current-message))
	      (rmail-forget-messages)
	      (rmail-show-message n)
	      (rmail-delete-forward)
	      (if (rmail-summary-exists)
		  (rmail-select-summary
		   (rmail-update-summary)))))
	(cond (error
	       (narrow-to-region (point-min) (1+ (point-max)))
	       (delete-region (point-min) (point-max))
	       (rmail-show-message rmail-current-message)))))))

;;;###autoload
(defun unforward-rmail-message ()
  "Extract a forwarded message from the containing message.
This puts the forwarded message into a separate rmail message
following the containing message."
  (interactive)
  ;; If we are in a summary buffer, switch to the Rmail buffer.
  (with-current-buffer rmail-buffer
    (narrow-to-region (rmail-msgbeg rmail-current-message)
		      (rmail-msgend rmail-current-message))
    (goto-char (point-min))
    (let (beg end (buffer-read-only nil) msg-string who-forwarded-it)
      (setq who-forwarded-it (mail-fetch-field "From"))
      (if (re-search-forward "^----" nil t)
	  nil
	(error "No forwarded message"))
      (forward-line 1)
      (setq beg (point))
      (if (re-search-forward "^----" nil t)
	  (setq end (match-beginning 0))
	(error "No terminator for forwarded message"))
      (widen)
      (setq msg-string (buffer-substring beg end))
      (goto-char (rmail-msgend rmail-current-message))
      (narrow-to-region (point) (point))
      (insert "\^_\^L\n0, unseen,,\n*** EOOH ***\n")
      (narrow-to-region (point) (point))
      (insert "Forwarded-by: " who-forwarded-it "\n")
      (insert msg-string)
      (goto-char (point-min))
      (while (not (eobp))
	(if (looking-at "- ")
	    (delete-region (point) (+ 2 (point))))
	(forward-line 1))
      (let ((n rmail-current-message))
	(rmail-forget-messages)
	(rmail-show-message n)
	(if (rmail-summary-exists)
	    (rmail-select-summary
	     (rmail-update-summary)))))))

(provide 'undigest)

;;; undigest.el ends here
