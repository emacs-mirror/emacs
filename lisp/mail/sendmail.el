;;; sendmail.el --- mail sending commands for Emacs.

;; Copyright (C) 1985, 1986, 1992 Free Software Foundation, Inc.

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

;; This mode provides mail-sending facilities from within Emacs.  It is
;; documented in the Emacs user's manual.

;;; Code:

;;;###autoload
(defvar mail-self-blind nil "\
Non-nil means insert BCC to self in messages to be sent.
This is done when the message is initialized,
so you can remove or alter the BCC field to override the default.")

;;;###autoload
(defvar mail-interactive nil "\
Non-nil means when sending a message wait for and display errors.
nil means let mailer mail back a message to report errors.")

;;;###autoload
(defvar mail-yank-ignored-headers "^via:\\|^mail-from:\\|^origin:\\|^status:\\|^remailed\\|^received:\\|^message-id:\\|^summary-line:\\|^to:\\|^subject:\\|^in-reply-to:\\|^return-path:" "\
Delete these headers from old message when it's inserted in a reply.")

;; Useful to set in site-init.el
;;;###autoload
(defconst send-mail-function 'sendmail-send-it "\
Function to call to send the current buffer as mail.
The headers are be delimited by a line which is `mail-header-separator'.")

;;;###autoload
(defvar mail-header-separator "--text follows this line--" "\
*Line used to separate headers from text in messages being composed.")

;;;###autoload
(defvar mail-archive-file-name nil "\
*Name of file to write all outgoing messages in, or nil for none.
Do not use an rmail file here!  Instead, use its inbox file.")

(defvar mail-default-reply-to nil
  "*Address to insert as default Reply-to field of outgoing messages.")

(defvar mail-alias-file nil
  "*If non-nil, the name of a file to use instead of `/usr/lib/aliases'.
This file defines aliases to be expanded by the mailer; this is a different
feature from that of defining aliases in `.mailrc' to be expanded in Emacs.
This variable has no effect unless your system uses sendmail as its mailer.")

(defvar mail-aliases t
  "Alist of mail address aliases,
or t meaning should be initialized from `~/.mailrc'.
The alias definitions in `~/.mailrc' have this form:
    alias ALIAS MEANING")

(defvar mail-yank-prefix nil
  "*Prefix insert on lines of yanked message being replied to.
nil means use indentation.")

(defvar mail-abbrevs-loaded nil)
(defvar mail-mode-map nil)

(autoload 'build-mail-aliases "mailalias"
  "Read mail aliases from `~/.mailrc' and set `mail-aliases'."
  nil)

(autoload 'expand-mail-aliases "mailalias"
  "Expand all mail aliases in suitable header fields found between BEG and END.
Suitable header fields are `To', `Cc' and `Bcc' and their `Resent-' variants.
Optional second arg EXCLUDE may be a regular expression defining text to be
removed from alias expansions."
  nil)

;;;###autoload
(defvar mail-signature nil
  "*Text inserted at end of mail buffer when a message is initialized.
If t, it means to insert the contents of the file `~/.signature'.")

(defvar mail-reply-buffer nil)
(defvar mail-send-actions nil
  "A list of actions to be performed upon successful sending of a message.")

(defvar mail-default-headers nil
  "*A string containing header lines, to be inserted in outgoing messages.
It is inserted before you edit the message,
so you can edit or delete these lines.")

(defvar mail-mode-syntax-table nil
  "Syntax table used while in mail mode.")

(if (null mail-mode-syntax-table)
    (progn
     (setq mail-mode-syntax-table (copy-syntax-table text-mode-syntax-table))
     (modify-syntax-entry ?% ". " mail-mode-syntax-table)))

(defun mail-setup (to subject in-reply-to cc replybuffer actions)
  (if (eq mail-aliases t)
      (progn
	(setq mail-aliases nil)
	(if (file-exists-p "~/.mailrc")
	    (build-mail-aliases))))
  (setq mail-send-actions actions)
  (setq mail-reply-buffer replybuffer)
  (goto-char (point-min))
  (insert "To: ")
  (save-excursion
    (if to
	;; Here removed code to extract names from within <...>
	;; on the assumption that mail-strip-quoted-names
	;; has been called and has done so.
	(let ((fill-prefix "\t")
	      (address-start (point)))
	  (insert to "\n")
	  (fill-region-as-paragraph address-start (point-max)))
      (newline))
    (if cc
	(let ((fill-prefix "\t")
	      (address-start (progn (insert "CC: ") (point))))
	  (insert cc "\n")
	  (fill-region-as-paragraph address-start (point-max))))
    (if in-reply-to
	(insert "In-reply-to: " in-reply-to "\n"))
    (insert "Subject: " (or subject "") "\n")
    (if mail-default-headers
	(insert mail-default-headers))
    (if mail-default-reply-to
	(insert "Reply-to: " mail-default-reply-to "\n"))
    (if mail-self-blind
	(insert "BCC: " (user-login-name) "\n"))
    (if mail-archive-file-name
	(insert "FCC: " mail-archive-file-name "\n"))
    (insert mail-header-separator "\n")
    ;; Insert the signature.
    (cond ((eq mail-signature t)
	   (if (file-exists-p "~/.signature")
	       (progn
		 (insert "--\n")
		 (insert-file-contents "~/.signature"))))
	  (mail-signature
	   (insert mail-signature)))
    (goto-char (point-max))
    (or (bolp) (newline)))
  (if to (goto-char (point-max)))
  (or to subject in-reply-to
      (set-buffer-modified-p nil))
  (run-hooks 'mail-setup-hook))

;;;###autoload
(defun mail-mode ()
  "Major mode for editing mail to be sent.
Like Text Mode but with these additional commands:
C-c C-s  mail-send (send the message)    C-c C-c  mail-send-and-exit
C-c C-f  move to a header field (and create it if there isn't):
	 C-c C-f C-t  move to To:	C-c C-f C-s  move to Subj:
	 C-c C-f C-b  move to BCC:	C-c C-f C-c  move to CC:
C-c C-t  move to message text.
C-c C-y  mail-yank-original (insert current message, in Rmail).
C-c C-q  mail-fill-yanked-message (fill what was yanked).
C-c C-v  mail-sent-via (add a sent-via field for each To or CC)."
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'mail-reply-buffer)
  (setq mail-reply-buffer nil)
  (make-local-variable 'mail-send-actions)
  (set-syntax-table mail-mode-syntax-table)
  (use-local-map mail-mode-map)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq major-mode 'mail-mode)
  (setq mode-name "Mail")
  (setq buffer-offer-save t)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^" mail-header-separator
				"$\\|^[ \t]*[-_][-_][-_]+$\\|"
				paragraph-start))
  (setq paragraph-separate (concat "^" mail-header-separator
				   "$\\|^[ \t]*[-_][-_][-_]+$\\|"
				   paragraph-separate))
  (run-hooks 'text-mode-hook 'mail-mode-hook))

(if mail-mode-map
    nil
  (setq mail-mode-map (nconc (make-sparse-keymap) text-mode-map))
  (define-key mail-mode-map "\C-c?" 'describe-mode)
  (define-key mail-mode-map "\C-c\C-f\C-t" 'mail-to)
  (define-key mail-mode-map "\C-c\C-f\C-b" 'mail-bcc)
  (define-key mail-mode-map "\C-c\C-f\C-f" 'mail-fcc)
  (define-key mail-mode-map "\C-c\C-f\C-c" 'mail-cc)
  (define-key mail-mode-map "\C-c\C-f\C-s" 'mail-subject)
  (define-key mail-mode-map "\C-c\C-t" 'mail-text)
  (define-key mail-mode-map "\C-c\C-y" 'mail-yank-original)
  (define-key mail-mode-map "\C-c\C-q" 'mail-fill-yanked-message)
  (define-key mail-mode-map "\C-c\C-w" 'mail-signature)
  (define-key mail-mode-map "\C-c\C-v" 'mail-sent-via)
  (define-key mail-mode-map "\C-c\C-c" 'mail-send-and-exit)
  (define-key mail-mode-map "\C-c\C-s" 'mail-send))

(defun mail-send-and-exit (arg)
  "Send message like `mail-send', then, if no errors, exit from mail buffer.
Prefix arg means don't delete this window."
  (interactive "P")
  (mail-send)
  (let ((newbuf (other-buffer (current-buffer))))
    (bury-buffer (current-buffer))
    (if (and (not arg)
	     (not (one-window-p))
	     (save-excursion
	       (set-buffer (window-buffer (next-window (selected-window) 'not)))
	       (eq major-mode 'rmail-mode)))
	(delete-window)
      (switch-to-buffer newbuf))))

(defun mail-send ()
  "Send the message in the current buffer.
If `mail-interactive' is non-nil, wait for success indication
or error messages, and inform user.
Otherwise any failure is reported in a message back to
the user from the mailer."
  (interactive)
  (if (or (buffer-modified-p)
          (y-or-n-p "Message already sent; resend? "))
      (progn
	(message "Sending...")
	(run-hooks 'mail-send-hook)
	(funcall send-mail-function)
	;; Now perform actions on successful sending.
	(while mail-send-actions
	  (condition-case nil
	      (apply (car (car mail-send-actions))
		     (cdr (car mail-send-actions)))
	    (error))
	  (setq mail-send-actions (cdr mail-send-actions)))
	(message "Sending...done")
	;; If buffer has no file, mark it as unmodified and delete autosave.
	(if (not buffer-file-name)
	    (progn
	      (set-buffer-modified-p nil)
	      (delete-auto-save-file-if-necessary t))))))

(defun sendmail-send-it ()
  (let ((errbuf (if mail-interactive
		    (generate-new-buffer " sendmail errors")
		  0))
	(tembuf (generate-new-buffer " sendmail temp"))
	(case-fold-search nil)
	delimline
	(mailbuf (current-buffer)))
    (unwind-protect
	(save-excursion
	  (set-buffer tembuf)
	  (erase-buffer)
	  (insert-buffer-substring mailbuf)
	  (goto-char (point-max))
	  ;; require one newline at the end.
	  (or (= (preceding-char) ?\n)
	      (insert ?\n))
	  ;; Change header-delimiter to be what sendmail expects.
	  (goto-char (point-min))
	  (re-search-forward
	    (concat "^" (regexp-quote mail-header-separator) "\n"))
	  (replace-match "\n")
	  (backward-char 1)
	  (setq delimline (point-marker))
	  (if mail-aliases
	      (expand-mail-aliases (point-min) delimline))
	  (goto-char (point-min))
	  ;; ignore any blank lines in the header
	  (while (and (re-search-forward "\n\n\n*" delimline t)
		      (< (point) delimline))
	    (replace-match "\n"))
	  (let ((case-fold-search t))
	    (goto-char (point-min))
	    (if (re-search-forward "^Sender:" delimline t)
		(error "Sender may not be specified."))
	    ;; Find and handle any FCC fields.
	    (goto-char (point-min))
	    (if (re-search-forward "^FCC:" delimline t)
		(mail-do-fcc delimline))
	    ;; If the From is different than current user, insert Sender.
	    (goto-char (point-min))
	    (and (re-search-forward "^From:"  delimline t)
		 (progn
		   (require 'mail-utils)
		   (not (string-equal
			 (mail-strip-quoted-names
			  (save-restriction
			    (narrow-to-region (point-min) delimline)
			    (mail-fetch-field "From")))
			 (user-login-name))))
		 (progn
		   (forward-line 1)
		   (insert "Sender: " (user-login-name) "\n")))
	    ;; "S:" is an abbreviation for "Subject:".
	    (goto-char (point-min))
	    (if (re-search-forward "^S:" delimline t)
		(replace-match "Subject:"))
	    ;; Don't send out a blank subject line
	    (goto-char (point-min))
	    (if (re-search-forward "^Subject:[ \t]*\n" delimline t)
		(replace-match ""))
	    (if mail-interactive
		(save-excursion
		  (set-buffer errbuf)
		  (erase-buffer))))
	  (apply 'call-process-region
		 (append (list (point-min) (point-max)
			       (if (boundp 'sendmail-program)
				   sendmail-program
				 "/usr/lib/sendmail")
			       nil errbuf nil
			       "-oi" "-t")
			 ;; Always specify who from,
			 ;; since some systems have broken sendmails.
			 (list "-f" (user-login-name))
;;;			 ;; Don't say "from root" if running under su.
;;;			 (and (equal (user-real-login-name) "root")
;;;			      (list "-f" (user-login-name)))
			 (and mail-alias-file
			      (list (concat "-oA" mail-alias-file)))
			 ;; These mean "report errors by mail"
			 ;; and "deliver in background".
			 (if (null mail-interactive) '("-oem" "-odb"))))
	  (if mail-interactive
	      (save-excursion
		(set-buffer errbuf)
		(goto-char (point-min))
		(while (re-search-forward "\n\n* *" nil t)
		  (replace-match "; "))
		(if (not (zerop (buffer-size)))
		    (error "Sending...failed to %s"
			   (buffer-substring (point-min) (point-max)))))))
      (kill-buffer tembuf)
      (if (bufferp errbuf)
	  (kill-buffer errbuf)))))

(defun mail-do-fcc (header-end)
  (let (fcc-list
	(rmailbuf (current-buffer))
	timezone
	(tembuf (generate-new-buffer " rmail output"))
	(case-fold-search t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^FCC:[ \t]*" header-end t)
	(setq fcc-list (cons (buffer-substring (point)
					       (progn
						 (end-of-line)
						 (skip-chars-backward " \t")
						 (point)))
			     fcc-list))
	(delete-region (match-beginning 0)
		       (progn (forward-line 1) (point))))
      (let* ((foo (current-time-zone))
	     (offset (+ (car foo) (if (nth 1 foo) 60 0)))
	     (abs (abs offset)))
	(setq timezone (format "%s%02d%02d"
			       (if (< offset 0) "-" "+")
			       (/ abs 60)
			       (% abs 60))))
      (set-buffer tembuf)
      (erase-buffer)
      (insert "\nFrom " (user-login-name) " "
	      (current-time-string) "\n")
      ;; Insert the time zone before the year.
      (forward-char -1)
      (forward-word -1)
      (insert timezone " ")
      (goto-char (point-max))
      (insert-buffer-substring rmailbuf)
      ;; Make sure messages are separated.
      (goto-char (point-max))
      (insert ?\n)
      (goto-char 2)
      ;; ``Quote'' "^From " as ">From "
      ;;  (note that this isn't really quoting, as there is no requirement
      ;;   that "^[>]+From " be quoted in the same transparent way.)
      (let ((case-fold-search nil))
	(while (search-forward "\nFrom " nil t)
	  (forward-char -5)
	  (insert ?>)))
      (while fcc-list
	(let ((buffer (get-file-buffer (car fcc-list))))
	  (if buffer
	      ;; File is present in a buffer => append to that buffer.
	      (let ((curbuf (current-buffer))
		    (beg (point-min)) (end (point-max)))
		(save-excursion
		  (set-buffer buffer)
		  ;; Keep the end of the accessible portion at the same place
		  ;; unless it is the end of the buffer.
		  (let ((max (if (/= (1+ (buffer-size)) (point-max))
				 (point-max))))
		    (unwind-protect
			(progn
			  (narrow-to-region (point-min) (1+ (buffer-size)))
			  (goto-char (point-max))
			  (if (eq major-mode 'rmail-mode)
			      ;; Append as a message to an RMAIL file
			      (let ((buffer-read-only nil))
				;; This forces RMAIL's message counters to be
				;; recomputed when the next RMAIL operation is
				;; done on the buffer.
				;; See rmail-maybe-set-message-counters.
				(setq rmail-total-messages nil)
				(insert "\C-l\n0, unseen,,\n*** EOOH ***\n"
					"From: " (user-login-name) "\n"
					"Date: " (current-time-string) "\n")
				(insert-buffer-substring curbuf beg end)
				(insert "\n\C-_")
				(rmail-set-message-counters))
			    (insert-buffer-substring curbuf beg end)))
		      (if max (narrow-to-region (point-min) max))))))
	    ;; Else append to the file directly.
	    (write-region
	     ;; Include a blank line before if file already exists.
	     (if (file-exists-p (car fcc-list)) (point-min) (1+ (point-min)))
	     (point-max) (car fcc-list) t)))
	(setq fcc-list (cdr fcc-list))))
    (kill-buffer tembuf)))

(defun mail-sent-via ()
  "Make a Sent-via header line from each To or CC header line."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; find the header-separator
    (search-forward (concat "\n" mail-header-separator "\n"))
    (forward-line -1)
    ;; put a marker at the end of the header
    (let ((end (point-marker))
	  (case-fold-search t)
	  to-line)
      (goto-char (point-min))
      ;; search for the To: lines and make Sent-via: lines from them
      ;; search for the next To: line
      (while (re-search-forward "^\\(to\\|cc\\):" end t)
	;; Grab this line plus all its continuations, sans the `to:'.
	(let ((to-line
	       (buffer-substring (point)
				 (progn
				   (if (re-search-forward "^[^ \t\n]" end t)
				       (backward-char 1)
				     (goto-char end))
				   (point)))))
	  ;; Insert a copy, with altered header field name.
	  (insert-before-markers "Sent-via:" to-line))))))

(defun mail-to ()
  "Move point to end of To-field."
  (interactive)
  (expand-abbrev)
  (mail-position-on-field "To"))

(defun mail-subject ()
  "Move point to end of Subject-field."
  (interactive)
  (expand-abbrev)
  (mail-position-on-field "Subject"))

(defun mail-cc ()
  "Move point to end of CC-field.  Create a CC field if none."
  (interactive)
  (expand-abbrev)
  (or (mail-position-on-field "cc" t)
      (progn (mail-position-on-field "to")
	     (insert "\nCC: "))))

(defun mail-bcc ()
  "Move point to end of BCC-field.  Create a BCC field if none."
  (interactive)
  (expand-abbrev)
  (or (mail-position-on-field "bcc" t)
      (progn (mail-position-on-field "to")
	     (insert "\nBCC: "))))

(defun mail-fcc ()
  "Add a new FCC field, with file name completion."
  (interactive)
  (expand-abbrev)
  (or (mail-position-on-field "fcc" t)	;Put new field after exiting FCC.
      (mail-position-on-field "to"))
  (insert "\nFCC: " (read-file-name "Folder carbon copy: ")))

(defun mail-position-on-field (field &optional soft)
  (let (end
	(case-fold-search t))
    (goto-char (point-min))
    (re-search-forward (concat "^" (regexp-quote mail-header-separator) "\n"))
    (setq end (match-beginning 0))
    (goto-char (point-min))
    (if (re-search-forward (concat "^" (regexp-quote field) ":") end t)
	(progn
	  (re-search-forward "^[^ \t]" nil 'move)
	  (beginning-of-line)
	  (skip-chars-backward "\n")
	  t)
      (or soft
	  (progn (goto-char end)
		 (insert field ": \n")
		 (skip-chars-backward "\n")))
      nil)))

(defun mail-text ()
  "Move point to beginning of text field."
  (interactive)
  (goto-char (point-min))
  (search-forward (concat "\n" mail-header-separator "\n")))

(defun mail-signature (atpoint)
  "Sign letter with contents of `mail-signature-file'."
  (interactive "P")
  (save-excursion
    (or atpoint
	(goto-char (point-max)))
    (skip-chars-backward " \t\n")
    (end-of-line)
    (or atpoint
	(delete-region (point) (point-max)))
    (insert "\n\n--\n")
    (insert-file-contents (expand-file-name "~/.signature"))))

(defun mail-fill-yanked-message (&optional justifyp)
  "Fill the paragraphs of a message yanked into this one.
Numeric argument means justify as well."
  (interactive "P")
  (save-excursion
    (goto-char (point-min))
    (search-forward (concat "\n" mail-header-separator "\n") nil t)
    (fill-individual-paragraphs (point)
				(point-max)
				justifyp
				t)))

(defun mail-yank-original (arg)
  "Insert the message being replied to, if any (in rmail).
Puts point before the text and mark after.
Normally, indents each nonblank line ARG spaces (default 3).
However, if `mail-yank-prefix' is non-nil, insert that prefix on each line.

Just \\[universal-argument] as argument means don't indent, insert no prefix,
and don't delete any header fields."
  (interactive "P")
  (if mail-reply-buffer
      (let ((start (point)))
	(delete-windows-on mail-reply-buffer)
	(insert-buffer mail-reply-buffer)
	(if (consp arg)
	    nil
	  (mail-yank-clear-headers start (mark))
	  (if (null mail-yank-prefix)
	      (indent-rigidly start (mark)
			      (if arg (prefix-numeric-value arg) 3))
	    (save-excursion
	      (goto-char start)
	      (while (< (point) (mark))
		(insert mail-yank-prefix)
		(forward-line 1)))))
	(exchange-point-and-mark)
	(if (not (eolp)) (insert ?\n)))))

(defun mail-yank-clear-headers (start end)
  (save-excursion
    (goto-char start)
    (if (search-forward "\n\n" end t)
	(save-restriction
	  (narrow-to-region start (point))
	  (goto-char start)
	  (while (let ((case-fold-search t))
		   (re-search-forward mail-yank-ignored-headers nil t))
	    (beginning-of-line)
	    (delete-region (point)
			   (progn (re-search-forward "\n[^ \t]")
				  (forward-char -1)
				  (point))))))))

;; Put these last, to reduce chance of lossage from quitting in middle of loading the file.

;;;###autoload
(defun mail (&optional noerase to subject in-reply-to cc replybuffer actions)
  "Edit a message to be sent.  Prefix arg means resume editing (don't erase).
When this function returns, the buffer `*mail*' is selected.
The value is t if the message was newly initialized; otherwise, nil.

By default, the signature file `~/.signature' is inserted at the end;
see the variable `mail-signature'.

\\<mail-mode-map>
While editing message, type \\[mail-send-and-exit] to send the message and exit.

Various special commands starting with C-c are available in sendmail mode
to move to message header fields:
\\{mail-mode-map}

If `mail-self-blind' is non-nil, a BCC to yourself is inserted
when the message is initialized.

If `mail-default-reply-to' is non-nil, it should be an address (a string);
a Reply-to: field with that address is inserted.

If `mail-archive-file-name' is non-nil, an FCC field with that file name
is inserted.

If `mail-setup-hook' is bound, its value is called with no arguments
after the message is initialized.  It can add more default fields.

When calling from a program, the second through fifth arguments
 TO, SUBJECT, IN-REPLY-TO and CC specify if non-nil
 the initial contents of those header fields.
 These arguments should not have final newlines.
The sixth argument REPLYBUFFER is a buffer whose contents
 should be yanked if the user types C-c C-y.
The seventh argument ACTIONS is a list of actions to take
 if/when the message is sent.  Each action looks like (FUNCTION . ARGS);
 when the message is sent, we apply FUNCTION to ARGS.
 This is how Rmail arranges to mark messages `answered'."
  (interactive "P")
;;; This is commented out because I found it was confusing in practice.
;;; It is easy enough to rename *mail* by hand with rename-buffer
;;; if you want to have multiple mail buffers.
;;; And then you can control which messages to save. --rms.
;;;  (let ((index 1)
;;;	buffer)
;;;    ;; If requested, look for a mail buffer that is modified and go to it.
;;;    (if noerase
;;;	(progn
;;;	  (while (and (setq buffer
;;;			    (get-buffer (if (= 1 index) "*mail*"
;;;					  (format "*mail*<%d>" index))))
;;;		      (not (buffer-modified-p buffer)))
;;;	    (setq index (1+ index)))
;;;	  (if buffer (switch-to-buffer buffer)
;;;	    ;; If none exists, start a new message.
;;;	    ;; This will never re-use an existing unmodified mail buffer
;;;	    ;; (since index is not 1 anymore).  Perhaps it should.
;;;	    (setq noerase nil))))
;;;    ;; Unless we found a modified message and are happy, start a new message.
;;;    (if (not noerase)
;;;	(progn
;;;	  ;; Look for existing unmodified mail buffer.
;;;	  (while (and (setq buffer
;;;			    (get-buffer (if (= 1 index) "*mail*"
;;;					  (format "*mail*<%d>" index))))
;;;		      (buffer-modified-p buffer))
;;;	    (setq index (1+ index)))
;;;	  ;; If none, make a new one.
;;;	  (or buffer
;;;	      (setq buffer (generate-new-buffer "*mail*")))
;;;	  ;; Go there and initialize it.
;;;	  (switch-to-buffer buffer)
;;;	  (erase-buffer)
;;;          (setq default-directory (expand-file-name "~/"))
;;;          (auto-save-mode auto-save-default)
;;;          (mail-mode)
;;;          (mail-setup to subject in-reply-to cc replybuffer actions)
;;;	  (if (and buffer-auto-save-file-name
;;;		   (file-exists-p buffer-auto-save-file-name))
;;;	      (message "Auto save file for draft message exists; consider M-x mail-recover"))
;;;          t))
  (switch-to-buffer "*mail*")
  (setq default-directory (expand-file-name "~/"))
  (auto-save-mode auto-save-default)
  (mail-mode)
  (let (initialized)
    (and (not noerase)
	 (or (not (buffer-modified-p))
	     (y-or-n-p "Unsent message being composed; erase it? "))
	 (progn (erase-buffer)
		(mail-setup to subject in-reply-to cc replybuffer actions)
		(setq initialized t)))
    (if (and buffer-auto-save-file-name
	     (file-exists-p buffer-auto-save-file-name))
	(message "Auto save file for draft message exists; consider M-x mail-recover"))
    initialized))

(defun mail-recover ()
  "Reread contents of current buffer from its last auto-save file."
  (interactive)
  (let ((file-name (make-auto-save-file-name)))
    (cond ((save-window-excursion
	     (if (not (eq system-type 'vax-vms))
		 (with-output-to-temp-buffer "*Directory*"
		   (buffer-disable-undo standard-output)
		   (call-process "ls" nil standard-output nil "-l" file-name)))
	     (yes-or-no-p (format "Recover auto save file %s? " file-name)))
	   (let ((buffer-read-only nil))
	     (erase-buffer)
	     (insert-file-contents file-name nil)))
	  (t (error "mail-recover cancelled.")))))

;;;###autoload
(defun mail-other-window (&optional noerase to subject in-reply-to cc replybuffer sendactions)
  "Like `mail' command, but display mail buffer in another window."
  (interactive "P")
  (let ((pop-up-windows t))
    (pop-to-buffer "*mail*"))
  (mail noerase to subject in-reply-to cc replybuffer sendactions))

;;;###autoload
(defun mail-other-frame (&optional noerase to subject in-reply-to cc replybuffer sendactions)
  "Like `mail' command, but display mail buffer in another frame."
  (interactive "P")
  (let ((pop-up-frames t))
    (pop-to-buffer "*mail*"))
  (mail noerase to subject in-reply-to cc replybuffer sendactions))


;;;###autoload
(define-key ctl-x-map "m" 'mail)

;;;###autoload
(define-key ctl-x-4-map "m" 'mail-other-window)

;;;###autoload
(define-key ctl-x-5-map "m" 'mail-other-frame)


;;; Do not add anything but external entries on this page.

(provide 'sendmail)

;;; sendmail.el ends here
