;;; sendmail.el --- mail sending commands for Emacs.

;; Copyright (C) 1985, 1986, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.

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
(defvar mail-from-style 'angles "\
*Specifies how \"From:\" fields look.

If `nil', they contain just the return address like:
	king@grassland.com
If `parens', they look like:
	king@grassland.com (Elvis Parsley)
If `angles', they look like:
	Elvis Parsley <king@grassland.com>")

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
(defvar send-mail-function 'sendmail-send-it "\
Function to call to send the current buffer as mail.
The headers are be delimited by a line which is `mail-header-separator'.")

;;;###autoload
(defvar mail-header-separator "--text follows this line--" "\
*Line used to separate headers from text in messages being composed.")

;;;###autoload
(defvar mail-archive-file-name nil "\
*Name of file to write all outgoing messages in, or nil for none.
This can be an inbox file or an Rmail file.")

;;;###autoload
(defvar mail-default-reply-to t
  "*Address to insert as default Reply-to field of outgoing messages.")

;;;###autoload
(defvar mail-alias-file nil
  "*If non-nil, the name of a file to use instead of `/usr/lib/aliases'.
This file defines aliases to be expanded by the mailer; this is a different
feature from that of defining aliases in `.mailrc' to be expanded in Emacs.
This variable has no effect unless your system uses sendmail as its mailer.")

;;;###autoload
(defvar mail-personal-alias-file "~/.mailrc"
  "*If non-nil, the name of the user's personal mail alias file.
This file typically should be in same format as the `.mailrc' file used by
the `Mail' or `mailx' program.
This file need not actually exist.")

(defvar mail-aliases t
  "Alist of mail address aliases,
or t meaning should be initialized from your mail aliases file.
\(The file's name is normally `~/.mailrc', but your MAILRC environment
variable can override that name.)
The alias definitions in the file have this form:
    alias ALIAS MEANING")

(defvar mail-alias-modtime nil
  "The modification time of your mail alias file when it was last examined.")

(defvar mail-yank-prefix nil
  "*Prefix insert on lines of yanked message being replied to.
nil means use indentation.")
(defvar mail-indentation-spaces 3
  "*Number of spaces to insert at the beginning of each cited line.
Used by `mail-yank-original' via `mail-yank-cite'.")
(defvar mail-yank-hooks nil
  "Obsolete hook for modifying a citation just inserted in the mail buffer.
Each hook function can find the citation between (point) and (mark t).
And each hook function should leave point and mark around the citation
text as modified.

This is a normal hook, misnamed for historical reasons.
It is semi-obsolete and mail agents should no longer use it.")

(defvar mail-citation-hook nil
  "*Hook for modifying a citation just inserted in the mail buffer.
Each hook function can find the citation between (point) and (mark t).
And each hook function should leave point and mark around the citation
text as modified.

If this hook is entirely empty (nil), a default action is taken
instead of no action.")

(defvar mail-abbrevs-loaded nil)
(defvar mail-mode-map nil)

(autoload 'build-mail-aliases "mailalias"
  "Read mail aliases from user's personal aliases file and set `mail-aliases'."
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

(defvar mail-bury-selects-summary t
  "*If non-nil, try to show RMAIL summary buffer after returning from mail.
The functions \\[mail-send-on-exit] or \\[mail-dont-send] select
the RMAIL summary buffer before returning, if it exists and this variable
is non-nil.")

;; Note: could use /usr/ucb/mail instead of sendmail;
;; options -t, and -v if not interactive.
(defvar mail-mailer-swallows-blank-line
  (if (and (string-match "sparc-sun-sunos\\(\\'\\|[^5]\\)" system-configuration)
	   (file-readable-p "/etc/sendmail.cf")
	   (let ((buffer (get-buffer-create " *temp*")))
	     (unwind-protect
		 (save-excursion
		   (set-buffer buffer)
		   (insert-file-contents "/etc/sendmail.cf")
		   (goto-char (point-min))
		   (let ((case-fold-search nil))
		     (re-search-forward "^OR\\>" nil t)))
	       (kill-buffer buffer))))
      ;; According to RFC822, "The field-name must be composed of printable
      ;; ASCII characters (i.e. characters that have decimal values between
      ;; 33 and 126, except colon)", i.e. any chars except ctl chars,
      ;; space, or colon.
      '(looking-at "[ \t]\\|[][!\"#$%&'()*+,-./0-9;<=>?@A-Z\\\\^_`a-z{|}~]+:"))
  "Set this non-nil if the system's mailer runs the header and body together.
\(This problem exists on Sunos 4 when sendmail is run in remote mode.)
The value should be an expression to test whether the problem will
actually occur.")

(defvar mail-mode-syntax-table nil
  "Syntax table used while in mail mode.")

(if (not mail-mode-syntax-table)
    (progn
     (setq mail-mode-syntax-table (copy-syntax-table text-mode-syntax-table))
     (modify-syntax-entry ?% ". " mail-mode-syntax-table)))

(defvar mail-font-lock-keywords
  (list '("^To:" . font-lock-function-name-face)
	'("^B?CC:\\|^Reply-To:" . font-lock-keyword-face)
	'("^Subject:" . font-lock-comment-face)
	'("^Subject:\\s *\\(.+\\)$" 1 font-lock-type-face)
	(list (concat "^\\(" mail-header-separator "\\)$") 1
	      'font-lock-comment-face)
	'("^[ \t]*\\sw*[>|}].*$" . font-lock-reference-face)	; Citation.
	'("^\\(X-[A-Za-z0-9-]+\\|In-reply-to\\):.*$" . font-lock-string-face))
  "Additional expressions to highlight in Mail mode.")

(defvar mail-send-hook nil
  "Normal hook run before sending mail, in Mail mode.")

(defun sendmail-synch-aliases ()
  (let ((modtime (nth 5 (file-attributes mail-personal-alias-file))))
    (or (equal mail-alias-modtime modtime)
	(setq mail-alias-modtime modtime
	      mail-aliases t))))

(defun mail-setup (to subject in-reply-to cc replybuffer actions)
  (if (eq mail-default-reply-to t)
      (setq mail-default-reply-to (getenv "REPLYTO")))
  (sendmail-synch-aliases)
  (if (eq mail-aliases t)
      (progn
	(setq mail-aliases nil)
	(if (file-exists-p mail-personal-alias-file)
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
        (let ((fill-prefix "\t")
	      (fill-column 78)
	      (address-start (point)))
	  (insert "In-reply-to: " in-reply-to "\n")
	  (fill-region-as-paragraph address-start (point-max))))
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
    ;; Insert the signature.  But remember the beginning of the message.
    (if to (setq to (point)))
    (cond ((eq mail-signature t)
	   (if (file-exists-p "~/.signature")
	       (progn
		 (insert "\n\n-- \n")
		 (insert-file-contents "~/.signature"))))
	  (mail-signature
	   (insert mail-signature)))
    (goto-char (point-max))
    (or (bolp) (newline)))
  (if to (goto-char to))
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
	 C-c C-f C-f  move to FCC:
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
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(mail-font-lock-keywords t))
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat mail-header-separator
				"$\\|[ \t]*[-_][-_][-_]+$\\|"
				paragraph-start))
  (setq paragraph-separate (concat mail-header-separator
				   "$\\|[ \t]*[-_][-_][-_]+$\\|"
				   paragraph-separate))
  (run-hooks 'text-mode-hook 'mail-mode-hook))

;;; Set up keymap.

(if mail-mode-map
    nil
  (setq mail-mode-map (nconc (make-sparse-keymap) text-mode-map))
  (define-key mail-mode-map "\C-c?" 'describe-mode)
  (define-key mail-mode-map "\C-c\C-f\C-t" 'mail-to)
  (define-key mail-mode-map "\C-c\C-f\C-b" 'mail-bcc)
  (define-key mail-mode-map "\C-c\C-f\C-f" 'mail-fcc)
  (define-key mail-mode-map "\C-c\C-f\C-c" 'mail-cc)
  (define-key mail-mode-map "\C-c\C-f\C-s" 'mail-subject)
  (define-key mail-mode-map "\C-c\C-f\C-r" 'mail-reply-to)
  (define-key mail-mode-map "\C-c\C-t" 'mail-text)
  (define-key mail-mode-map "\C-c\C-y" 'mail-yank-original)
  (define-key mail-mode-map "\C-c\C-q" 'mail-fill-yanked-message)
  (define-key mail-mode-map "\C-c\C-w" 'mail-signature)
  (define-key mail-mode-map "\C-c\C-v" 'mail-sent-via)
  (define-key mail-mode-map "\C-c\C-c" 'mail-send-and-exit)
  (define-key mail-mode-map "\C-c\C-s" 'mail-send))

(define-key mail-mode-map [menu-bar mail]
  (cons "Mail" (make-sparse-keymap "Mail")))

(define-key mail-mode-map [menu-bar mail fill]
  '("Fill Citation" . mail-fill-yanked-message))

(define-key mail-mode-map [menu-bar mail yank]
  '("Cite Original" . mail-yank-original))

(define-key mail-mode-map [menu-bar mail signature]
  '("Insert Signature" . mail-signature))

(define-key mail-mode-map [menu-bar mail cancel]
  '("Cancel" . mail-dont-send))

(define-key mail-mode-map [menu-bar mail send-stay]
  '("Send, Keep Editing" . mail-send))

(define-key mail-mode-map [menu-bar mail send]
  '("Send Message" . mail-send-and-exit))

(define-key mail-mode-map [menu-bar headers]
  (cons "Headers" (make-sparse-keymap "Move to Header")))

(define-key mail-mode-map [menu-bar headers reply-to]
  '("Reply-To" . mail-reply-to))

(define-key mail-mode-map [menu-bar headers sent-via]
  '("Sent Via" . mail-sent-via))

(define-key mail-mode-map [menu-bar headers text]
  '("Text" . mail-text))

(define-key mail-mode-map [menu-bar headers bcc]
  '("Bcc" . mail-bcc))

(define-key mail-mode-map [menu-bar headers fcc]
  '("Fcc" . mail-fcc))

(define-key mail-mode-map [menu-bar headers cc]
  '("Cc" . mail-cc))

(define-key mail-mode-map [menu-bar headers subject]
  '("Subject" . mail-subject))

(define-key mail-mode-map [menu-bar headers to]
  '("To" . mail-to))

(defun mail-send-and-exit (arg)
  "Send message like `mail-send', then, if no errors, exit from mail buffer.
Prefix arg means don't delete this window."
  (interactive "P")
  (mail-send)
  (mail-bury arg))

(defun mail-dont-send (arg)
  "Don't send the message you have been editing.
Prefix arg means don't delete this window."
  (interactive "P")
  (mail-bury arg))

(defun mail-bury (arg)
  "Bury this mail buffer."
  (let ((newbuf (other-buffer (current-buffer))))
    (bury-buffer (current-buffer))
    (if (and (fboundp 'frame-parameters)
	     (cdr (assq 'dedicated (frame-parameters)))
	     (not (null (delq (selected-frame) (visible-frame-list)))))
	(delete-frame (selected-frame))
      (let (rmail-flag summary-buffer)
	(and (not arg)
	     (not (one-window-p))
	     (save-excursion
	       (set-buffer (window-buffer (next-window (selected-window) 'not)))
	       (setq rmail-flag (eq major-mode 'rmail-mode))
	       (setq summary-buffer
		     (and mail-bury-selects-summary
			  (boundp 'rmail-summary-buffer)
			  rmail-summary-buffer
			  (buffer-name rmail-summary-buffer)
			  (not (get-buffer-window rmail-summary-buffer))
			  rmail-summary-buffer))))
	(if rmail-flag
	    ;; If the Rmail buffer has a summary, show that.
	    (if summary-buffer (switch-to-buffer summary-buffer)
	      (delete-window))
	  (switch-to-buffer newbuf))))))

(defun mail-send ()
  "Send the message in the current buffer.
If `mail-interactive' is non-nil, wait for success indication
or error messages, and inform user.
Otherwise any failure is reported in a message back to
the user from the mailer."
  (interactive)
  (if (if buffer-file-name
	  (y-or-n-p "Send buffer contents as mail message? ")
	(or (buffer-modified-p)
	    (y-or-n-p "Message already sent; resend? ")))
      (progn
	(run-hooks 'mail-send-hook)
	(message "Sending...")
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
	resend-to-addresses
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
	  (sendmail-synch-aliases)
	  (if mail-aliases
	      (expand-mail-aliases (point-min) delimline))
	  (goto-char (point-min))
	  ;; ignore any blank lines in the header
	  (while (and (re-search-forward "\n\n\n*" delimline t)
		      (< (point) delimline))
	    (replace-match "\n"))
	  (let ((case-fold-search t))
	    (goto-char (point-min))
	    ;; Find and handle any FCC fields.
	    (goto-char (point-min))
	    (if (re-search-forward "^FCC:" delimline t)
		(mail-do-fcc delimline))
	    (goto-char (point-min))
	    (require 'mail-utils)
	    (while (re-search-forward "^Resent-to:" delimline t)
	      (setq resend-to-addresses
		    (save-restriction
		      (narrow-to-region (point)
					(save-excursion
					  (end-of-line)
					  (point)))
		      (append (mail-parse-comma-list)
			      resend-to-addresses))))
;;; Apparently this causes a duplicate Sender.
;;;	    ;; If the From is different than current user, insert Sender.
;;;	    (goto-char (point-min))
;;;	    (and (re-search-forward "^From:"  delimline t)
;;;		 (progn
;;;		   (require 'mail-utils)
;;;		   (not (string-equal
;;;			 (mail-strip-quoted-names
;;;			  (save-restriction
;;;			    (narrow-to-region (point-min) delimline)
;;;			    (mail-fetch-field "From")))
;;;			 (user-login-name))))
;;;		 (progn
;;;		   (forward-line 1)
;;;		   (insert "Sender: " (user-login-name) "\n")))
	    ;; Don't send out a blank subject line
	    (goto-char (point-min))
	    (if (re-search-forward "^Subject:[ \t]*\n" delimline t)
		(replace-match ""))
	    ;; Put the "From:" field in unless for some odd reason
	    ;; they put one in themselves.
	    (goto-char (point-min))
	    (if (not (re-search-forward "^From:" delimline t))
		(let* ((login (user-login-name))
		       (fullname (user-full-name)))
		  (cond ((eq mail-from-style 'angles)
			 (insert "From: " fullname " <" login ">\n"))
			((eq mail-from-style 'parens)
			 (insert "From: " login " (" fullname ")\n"))
			((null mail-from-style)
			 (insert "From: " login "\n")))))
	    ;; Insert an extra newline if we need it to work around
	    ;; Sun's bug that swallows newlines.
	    (goto-char (1+ delimline))
	    (if (eval mail-mailer-swallows-blank-line)
		(newline))
	    (if mail-interactive
		(save-excursion
		  (set-buffer errbuf)
		  (erase-buffer))))
	  (apply 'call-process-region
		 (append (list (point-min) (point-max)
			       (if (boundp 'sendmail-program)
				   sendmail-program
				 "/usr/lib/sendmail")
			       nil errbuf nil "-oi")
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
			 (if (null mail-interactive) '("-oem" "-odb"))
			 ;; Get the addresses from the message
			 ;; unless this is a resend.
			 ;; We must not do that for a resend
			 ;; because we would find the original addresses.
			 ;; For a resend, include the specific addresses.
			 (or resend-to-addresses
			     '("-t"))))
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

;; Return non-nil if file FILE is an Rmail file.
(defun mail-file-babyl-p (file)
  (unwind-protect
      (save-excursion
	(set-buffer (get-buffer-create " mail-temp"))
	(erase-buffer)
	(insert-file-contents file nil 0 20)
	(looking-at "BABYL OPTIONS:"))
  (kill-buffer " mail-temp")))

(defun mail-do-fcc (header-end)
  (let (fcc-list
	(rmailbuf (current-buffer))
	(time (current-time))
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
      (set-buffer tembuf)
      (erase-buffer)
      ;; This initial newline is written out if the fcc file already exists.
      (insert "\nFrom " (user-login-name) " "
	      (current-time-string time) "\n")
      ;; Insert the time zone before the year.
      (forward-char -1)
      (forward-word -1)
      (require 'mail-utils)
      (insert (mail-rfc822-time-zone time) " ")
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
	(let* ((truename (file-truename (car fcc-list)))
	       (buffer
		(or (get-file-buffer (car fcc-list))
		    (get-file-buffer truename)
		    ;; Look for a buffer whose truename
		    ;; matches that of the file we want.
		    (let ((buflist (buffer-list)))
		      (save-excursion
			(while buflist
			  (set-buffer (car buflist))
			  (if (equal buffer-file-truename truename)
			      (setq buflist nil))
			  (setq buflist (cdr buflist)))
			(current-buffer)))))
	       (curbuf (current-buffer))
	       (beg (point-min)) (end (point-max))
	       (beg2 (save-excursion (goto-char (point-min))
				     (forward-line 2) (point))))
	  (if buffer
	      ;; File is present in a buffer => append to that buffer.
	      (save-excursion
		(set-buffer buffer)
		;; Keep the end of the accessible portion at the same place
		;; unless it is the end of the buffer.
		(let ((max (if (/= (1+ (buffer-size)) (point-max))
			       (point-max))))
		  (unwind-protect
		      ;; Code below lifted from rmailout.el
		      ;; function rmail-output-to-rmail-file:
		      (let ((buffer-read-only nil)
			    (msg (and (boundp 'rmail-current-message)
				      rmail-current-message)))
			;; If MSG is non-nil, buffer is in RMAIL mode.
			(if msg
			    (progn
			      (rmail-maybe-set-message-counters)
			      (widen)
			      (narrow-to-region (point-max) (point-max))
			      (insert "\C-l\n0, unseen,,\n*** EOOH ***\n"
				      "From: " (user-login-name) "\n"
				      "Date: " (mail-rfc822-date) "\n")
			      (insert-buffer-substring curbuf beg2 end)
			      (insert "\n\C-_")
			      (goto-char (point-min))
			      (widen)
			      (search-backward "\n\^_")
			      (narrow-to-region (point) (point-max))
			      (rmail-count-new-messages t)
			      (rmail-show-message msg)
			      (setq max nil))
			  ;; Output file not in rmail mode
			  ;; => just insert at the end.
			  (narrow-to-region (point-min) (1+ (buffer-size)))
			  (goto-char (point-max))
			  (insert-buffer-substring curbuf beg end)))
		    (if max (narrow-to-region (point-min) max)))))
	    ;; Else append to the file directly.
	    (if (and (file-exists-p (car fcc-list))
		     (mail-file-babyl-p (car fcc-list)))
		;; If the file is a Babyl file,
		;; convert the message to Babyl format.
		(save-excursion
		  (set-buffer (get-buffer-create " mail-temp"))
		  (insert "\C-l\n0, unseen,,\n*** EOOH ***\n"
			  "From: " (user-login-name) "\n"
			  "Date: " (mail-rfc822-date) "\n")
		  (insert-buffer-substring curbuf beg2 end)
		  (insert "\n\C-_")
		  (write-region (point-min) (point-max) (car fcc-list) t))
	      (write-region
	       (1+ (point-min)) (point-max) (car fcc-list) t))))
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

(defun mail-fcc (folder)
  "Add a new FCC field, with file name completion."
  (interactive "FFolder carbon copy: ")
  (expand-abbrev)
  (or (mail-position-on-field "fcc" t)	;Put new field after exiting FCC.
      (mail-position-on-field "to"))
  (insert "\nFCC: " folder))

(defun mail-reply-to ()      
  "Move point to end of Reply-To-field."
  (interactive)
  (expand-abbrev)
  (mail-position-on-field "Reply-To"))

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
  "Sign letter with contents of the file `~/.signature'.
Prefix arg means put contents at point."
  (interactive "P")
  (save-excursion
    (or atpoint
	(goto-char (point-max)))
    (skip-chars-backward " \t\n")
    (end-of-line)
    (or atpoint
	(delete-region (point) (point-max)))
    (insert "\n\n-- \n")
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

(defun mail-indent-citation ()
  "Modify text just inserted from a message to be cited.
The inserted text should be the region.
When this function returns, the region is again around the modified text.

Normally, indent each nonblank line `mail-indentation-spaces' spaces.
However, if `mail-yank-prefix' is non-nil, insert that prefix on each line."
  (let ((start (point)))
    (mail-yank-clear-headers start (mark t))
    (if (null mail-yank-prefix)
	(indent-rigidly start (mark t) mail-indentation-spaces)
      (save-excursion
	(goto-char start)
	(while (< (point) (mark t))
	  (insert mail-yank-prefix)
	  (forward-line 1))))))

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
	;; If the original message is in another window in the same frame,
	;; delete that window to save screen space.
	;; t means don't alter other frames.
	(delete-windows-on mail-reply-buffer t)
	(insert-buffer mail-reply-buffer)
	(if (consp arg)
	    nil
	  (goto-char start)
	  (let ((mail-indentation-spaces (if arg (prefix-numeric-value arg)
					   mail-indentation-spaces)))
	    (if mail-citation-hook
		(run-hooks 'mail-citation-hook)
	      (if mail-yank-hooks
		  (run-hooks 'mail-yank-hooks)
		(mail-indent-citation)))))
	;; This is like exchange-point-and-mark, but doesn't activate the mark.
	;; It is cleaner to avoid activation, even though the command
	;; loop would deactivate the mark because we inserted text.
	(goto-char (prog1 (mark t)
		     (set-marker (mark-marker) (point) (current-buffer))))
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

When calling from a program, the first argument if non-nil says
not to erase the existing contents of the `*mail*' buffer.

The second through fifth arguments,
 TO, SUBJECT, IN-REPLY-TO and CC, specify if non-nil
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
  (pop-to-buffer "*mail*")
  (if (file-exists-p (expand-file-name "~/"))
      (setq default-directory (expand-file-name "~/")))
  (auto-save-mode auto-save-default)
  (mail-mode)
  ;; Disconnect the buffer from its visited file
  ;; (in case the user has actually visited a file *mail*).
;  (set-visited-file-name nil)
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
	  (t (error "mail-recover cancelled")))))

;;;###autoload
(defun mail-other-window (&optional noerase to subject in-reply-to cc replybuffer sendactions)
  "Like `mail' command, but display mail buffer in another window."
  (interactive "P")
  (let ((pop-up-windows t)
	(special-display-buffer-names nil)
	(special-display-regexps nil)
	(same-window-buffer-names nil)
	(same-window-regexps nil))
    (pop-to-buffer "*mail*"))
  (mail noerase to subject in-reply-to cc replybuffer sendactions))

;;;###autoload
(defun mail-other-frame (&optional noerase to subject in-reply-to cc replybuffer sendactions)
  "Like `mail' command, but display mail buffer in another frame."
  (interactive "P")
  (let ((pop-up-frames t)
	(special-display-buffer-names nil)
	(special-display-regexps nil)
	(same-window-buffer-names nil)
	(same-window-regexps nil))
    (pop-to-buffer "*mail*"))
  (mail noerase to subject in-reply-to cc replybuffer sendactions))

;;; Do not execute these when sendmail.el is loaded,
;;; only in loaddefs.el.
;;;###autoload (define-key ctl-x-map "m" 'mail)
;;;###autoload (define-key ctl-x-4-map "m" 'mail-other-window)
;;;###autoload (define-key ctl-x-5-map "m" 'mail-other-frame)

;;;###autoload (add-hook 'same-window-buffer-names "*mail*")

;;; Do not add anything but external entries on this page.

(provide 'sendmail)

;;; sendmail.el ends here
