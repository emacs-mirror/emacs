;;; undigest.el --- digest-cracking support for the RMAIL mail reader

;; Copyright (C) 1985, 1986, 1994, 1996, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007 Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; See Internet RFC 934 and RFC 1153
;; Also limited support for MIME digest encapsulation

;;; Code:

(require 'rmail)

(defconst rmail-mail-separator
  "\^_\^L\n0, unseen,,\n*** EOOH ***\n"
  "String for separating messages in an rmail file.")

(defcustom rmail-forward-separator-regex
  "^----.*\\([Ff]orwarded\\|[Oo]riginal\\).*[Mm]essage"
  "*Regexp to match the string that introduces forwarded messages.
This is not a header, but a string contained in the body of the message.
You may need to customize it for local needs."
  :type 'regexp
  :group 'rmail-headers)


(defconst rmail-digest-methods
  '(rmail-digest-parse-mime
    rmail-digest-parse-rfc1153strict
    rmail-digest-parse-rfc1153sloppy
    rmail-digest-parse-rfc934)
  "List of digest parsing functions, first tried first.

These functions operate on the current narrowing, and take no argument.
A function returns nil if it cannot parse the digest.  If it can, it
returns a list of cons pairs containing the start and end positions of
each undigestified message as markers.")

(defun rmail-digest-parse-mime ()
  (goto-char (point-min))
  (when (let ((head-end (progn (search-forward "\n\n" nil t) (point))))
	  (goto-char (point-min))
	  (and head-end
	       (re-search-forward
		(concat
		 "^Content-type: multipart/digest;"
		 "\\s-* boundary=\"?\\([^\";\n]+\\)[\";\n]") head-end t)
	       (search-forward (match-string 1) nil t)))
    ;; Ok, prolog separator found
    (let ((start (make-marker))
	  (end (make-marker))
	  (separator (concat "\n--" (match-string 0) "\n\n"))
	  result)
      (while (search-forward separator nil t)
	(move-marker start (match-beginning 0))
	(move-marker end (match-end 0))
	(add-to-list 'result (cons (copy-marker start) (copy-marker end t))))
      ;; Return the list of marker pairs
      (nreverse result))))

(defun rmail-digest-parse-rfc1153strict ()
  "Parse following strictly the method defined in RFC 1153.
See rmail-digest-methods."
 (rmail-digest-rfc1153
  "^-\\{70\\}\n\n"
  "^\n-\\{30\\}\n\n"
  "^\n-\\{30\\}\n\nEnd of .* Digest.*\n\\*\\{15,\\}\n+\'"))

(defun rmail-digest-parse-rfc1153sloppy ()
  "Parse using the method defined in RFC 1153, allowing for some sloppiness.
See rmail-digest-methods."
 (rmail-digest-rfc1153
  "^-\\{55,\\}\n\n"
  "^\n-\\{27,\\}\n\n"
  "^\n-\\{27,\\}\n\nEnd of"))

(defun rmail-digest-rfc1153 (prolog-sep message-sep trailer-sep)
  (goto-char (point-min))
  (when (re-search-forward prolog-sep nil t)
    ;; Ok, prolog separator found
    (let ((start (make-marker))
	  (end (make-marker))
	  separator result)
      (move-marker start (match-beginning 0))
      (move-marker end (match-end 0))
      (setq result (cons (copy-marker start) (copy-marker end t)))
      (when (re-search-forward message-sep nil t)
	;; Ok, at least one message separator found
	(setq separator (match-string 0))
	(when (re-search-forward trailer-sep nil t)
	  ;; Wonderful, we found a trailer, too.  Now, go on splitting
	  ;; the digest into separate rmail messages
	  (goto-char (cdar result))
	  (while (search-forward separator nil t)
	    (move-marker start (match-beginning 0))
	    (move-marker end (match-end 0))
	    (add-to-list 'result
			 (cons (copy-marker start) (copy-marker end t))))
	  ;; Undo masking of separators inside digestified messages
	  (goto-char (point-min))
	  (while (search-forward
		  (replace-regexp-in-string "\n-" "\n " separator) nil t)
	    (replace-match separator))
	  ;; Return the list of marker pairs
	  (nreverse result))))))

(defun rmail-digest-parse-rfc934 ()
  (goto-char (point-min))
  (when (re-search-forward "^\n?-[^ ].*\n\n?" nil t)
    ;; Message separator found
    (let ((start (make-marker))
	  (end (make-marker))
	  (separator (match-string 0))
	  result)
      (goto-char (point-min))
      (while (search-forward separator nil t)
	(move-marker start (match-beginning 0))
	(move-marker end (match-end 0))
	(add-to-list 'result (cons (copy-marker start) (copy-marker end t))))
      ;; Undo masking of separators inside digestified messages
      (goto-char (point-min))
      (while (search-forward "\n- -" nil t)
	(replace-match "\n-"))
      ;; Return the list of marker pairs
      (nreverse result))))

;;;###autoload
(defun undigestify-rmail-message ()
  "Break up a digest message into its constituent messages.
Leaves original message, deleted, before the undigestified messages."
  (interactive)
  (with-current-buffer rmail-buffer
    (widen)
    (let ((error t)
	  (buffer-read-only nil))
      (goto-char (rmail-msgend rmail-current-message))
      (let ((msg-copy (buffer-substring (rmail-msgbeg rmail-current-message)
					(rmail-msgend rmail-current-message))))
	(narrow-to-region (point) (point))
	(insert msg-copy))
      (narrow-to-region (point-min) (1- (point-max)))
      (unwind-protect
	  (progn
	    (save-restriction
	      (goto-char (point-min))
	      (delete-region (point-min)
			     (progn (search-forward "\n*** EOOH ***\n" nil t)
				    (point)))
	      (insert "\n" rmail-mail-separator)
	      (narrow-to-region (point)
				(point-max))
	      (let ((fill-prefix "")
		    (case-fold-search t)
		    digest-name type start end separator fun-list sep-list)
		(setq digest-name (mail-strip-quoted-names
				   (save-restriction
				     (search-forward "\n\n" nil 'move)
				     (setq start (point))
				     (narrow-to-region (point-min) start)
				     (or (mail-fetch-field "Reply-To")
					 (mail-fetch-field "To")
					 (mail-fetch-field "Apparently-To")
					 (mail-fetch-field "From")))))
		(unless digest-name
		  (error "Message is not a digest--bad header"))

		(setq fun-list rmail-digest-methods)
		(while (and fun-list
			    (null (setq sep-list (funcall (car fun-list)))))
		  (setq fun-list (cdr fun-list)))
		(unless sep-list
		  (error "Message is not a digest--no messages found"))

		;;; Split the digest into separate rmail messages
		(while sep-list
		  (let ((start (caar sep-list))
			(end (cdar sep-list)))
		    (delete-region start end)
		    (goto-char start)
		    (insert rmail-mail-separator)
		    (search-forward "\n\n" (caar (cdr sep-list)) 'move)
		    (save-restriction
		      (narrow-to-region end (point))
		      (unless (mail-fetch-field "To")
			(goto-char start)
			(insert "To: " digest-name "\n")))
		    (set-marker start nil)
		    (set-marker end nil))
		  (setq sep-list (cdr sep-list)))))

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
  (unwind-protect
      (with-current-buffer rmail-buffer
	(goto-char (point-min))
	(narrow-to-region (point)
			  (save-excursion (search-forward "\n\n") (point)))
	(let ((buffer-read-only nil)
	      (old-fwd-from (mail-fetch-field "Forwarded-From" nil nil t))
	      (old-fwd-date (mail-fetch-field "Forwarded-Date" nil nil t))
	      (fwd-from (mail-fetch-field "From"))
	      (fwd-date (mail-fetch-field "Date"))
	      beg end prefix forward-msg)
	  (narrow-to-region (rmail-msgbeg rmail-current-message)
			    (rmail-msgend rmail-current-message))
	  (goto-char (point-min))
	  (cond ((re-search-forward rmail-forward-separator-regex nil t)
		 (forward-line 1)
		 (skip-chars-forward "\n")
		 (setq beg (point))
		 (setq end (if (re-search-forward "^----.*[^- \t\n]" nil t)
			       (match-beginning 0) (point-max)))
		 (setq forward-msg
		       (replace-regexp-in-string
			"^- -" "-" (buffer-substring beg end))))
		((and (re-search-forward "^\\(> ?\\)[a-zA-Z-]+: .*\n" nil t)
		      (setq beg (match-beginning 0))
		      (setq prefix (match-string-no-properties 1))
		      (goto-char beg)
		      (looking-at (concat "\\(" prefix ".+\n\\)*"
					  prefix "Date: ."))
		      (looking-at (concat "\\(" prefix ".+\n\\)*"
					  prefix "From: .+\n"
					  "\\(" prefix ".+\n\\)*"
					  "\\(> ?\\)?\n" prefix)))
		 (re-search-forward "^[^>\n]" nil 'move)
		 (backward-char)
		 (skip-chars-backward " \t\n")
		 (forward-line 1)
		 (setq end (point))
		 (setq forward-msg
		       (replace-regexp-in-string
			(if (string= prefix ">") "^>" "> ?")
			"" (buffer-substring beg end))))
		(t
		 (error "No forwarded message found")))
	  (widen)
	  (goto-char (rmail-msgend rmail-current-message))
	  (narrow-to-region (point) (point))
	  (insert rmail-mail-separator)
	  (narrow-to-region (point) (point))
	  (while old-fwd-from
	    (insert "Forwarded-From: " (car old-fwd-from) "\n")
	    (insert "Forwarded-Date: " (car old-fwd-date) "\n")
	    (setq old-fwd-from (cdr old-fwd-from))
	    (setq old-fwd-date (cdr old-fwd-date)))
	  (insert "Forwarded-From: " fwd-from "\n")
	  (insert "Forwarded-Date: " fwd-date "\n")
	  (insert forward-msg)
	  (save-restriction
	    (goto-char (point-min))
	    (re-search-forward "\n$" nil 'move)
	    (narrow-to-region (point-min) (point))
	    (goto-char (point-min))
	    (while (not (eobp))
	      (unless (looking-at "^[a-zA-Z-]+: ")
		(insert "\t"))
	      (forward-line)))
	  (goto-char (point-min))))
    (let ((n rmail-current-message))
      (rmail-forget-messages)
      (rmail-show-message n))
    (if (rmail-summary-exists)
	(rmail-select-summary
	 (rmail-update-summary)))))


(provide 'undigest)

;;; arch-tag: 3a28b9fb-c1f5-43ef-9278-285f3e4b874d
;;; undigest.el ends here
