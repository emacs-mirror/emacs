;;; undigest.el --- digest-cracking support for the RMAIL mail reader

;; Copyright (C) 1985, 1986, 1994 Free Software Foundation, Inc.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; See Internet RFC 934

;;; Code:

(require 'rmail)

(defun undigestify-rmail-message ()
  "Break up a digest message into its constituent messages.
Leaves original message, deleted, before the undigestified messages."
  (interactive)
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
		   (digest-name
		    (mail-strip-quoted-names
		     (or (save-restriction
			   (search-forward "\n\n")
			   (narrow-to-region (point-min) (point))
			   (goto-char (point-max))
			   (or (mail-fetch-field "Reply-To")
			       (mail-fetch-field "To")
			       (mail-fetch-field "Apparently-To")
			       (mail-fetch-field "From")))
			 (error "Message is not a digest")))))
	      (save-excursion
		(goto-char (point-max))
		(skip-chars-backward " \t\n")
		(let ((count 10) found)
		  ;; compensate for broken un*x digestifiers.  Sigh Sigh.
		  (while (and (> count 0) (not found))
		    (forward-line -1)
		    (setq count (1- count))
		    (if (looking-at (concat "End of.*Digest.*\n"
					    (regexp-quote "*********") "*"
					    "\\(\n------*\\)*"))
			(setq found t)))
		  (if (not found) (error "Message is not a digest"))))
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
	     (rmail-show-message rmail-current-message))))))

;;; undigest.el ends here
