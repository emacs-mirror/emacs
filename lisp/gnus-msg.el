;;; gnus-msg.el --- mail and post interface for Gnus
;; Copyright (C) 1995 Free Software Foundation, Inc.

;; Author: Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;;	Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; Keywords: news

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

;;; Code:

(require 'gnus)
(require 'sendmail)
(require 'gnus-ems)

(defvar gnus-organization-file "/usr/lib/news/organization"
  "*Local news organization file.")

(defvar gnus-prepare-article-hook (list 'gnus-inews-insert-signature)
  "*A hook called after preparing body, but before preparing header headers.
The default hook (`gnus-inews-insert-signature') inserts a signature
file specified by the variable `gnus-signature-file'.")

(defvar gnus-post-prepare-function nil
  "*Function that is run after a post buffer has been prepared.
It is called with the name of the newsgroup that is posted to. It
might be used, for instance, for inserting signatures based on the
newsgroup name. (In that case, `gnus-signature-file' and
`mail-signature' should both be set to nil).")

(defvar gnus-post-prepare-hook nil
  "*Hook that is run after a post buffer has been prepared.
If you want to insert the signature, you might put
`gnus-inews-insert-signature' in this hook.")

(defvar gnus-use-followup-to t
  "*Specifies what to do with Followup-To header.
If nil, ignore the header. If it is t, use its value, but ignore 
`poster'.  If it is the symbol `ask', query the user before posting.
If it is the symbol `use', always use the value.") 

(defvar gnus-followup-to-function nil
  "*A variable that contains a function that returns a followup address.
The function will be called in the buffer of the article that is being
followed up. The buffer will be narrowed to the headers of the
article. To pick header headers, one might use `mail-fetch-field'.  The
function will be called with the name of the current newsgroup as the
argument.

Here's an example `gnus-followup-to-function':

(setq gnus-followup-to-function
      (lambda (group)
	(cond ((string= group \"mail.list\")
	       (or (mail-fetch-field \"sender\") 
		   (mail-fetch-field \"from\")))
	      (t
	       (or (mail-fetch-field \"reply-to\") 
		   (mail-fetch-field \"from\"))))))")

(defvar gnus-reply-to-function nil
  "*A variable that contains a function that returns a reply address.
See the `gnus-followup-to-function' variable for an explanation of how
this variable is used.

This function should return a string that will be used to fill in the
header.  This function may also return a list.  In that case, every
list element should be a cons where the first car should be a string
with the header name, and the cdr should be a string with the header
value.")

(defvar gnus-author-copy (getenv "AUTHORCOPY")
  "*Save outgoing articles in this file.
Initialized from the AUTHORCOPY environment variable.

If this variable begins with the character \"|\", outgoing articles
will be piped to the named program. It is possible to save an article
in an MH folder as follows:

\(setq gnus-author-copy \"|/usr/local/lib/mh/rcvstore +Article\")

If the first character is not a pipe, articles are saved using the
function specified by the `gnus-author-copy-saver' variable.")

(defvar gnus-mail-self-blind nil
  "*Non-nil means insert a BCC header in all outgoing articles.
This will result in having a copy of the article mailed to yourself.
The BCC header is inserted when the post buffer is initialized, so you
can remove or alter the BCC header to override the default.")

(defvar gnus-author-copy-saver (function rmail-output)
  "*A function called to save outgoing articles.
This function will be called with the same of the file to store the
article in. The default function is `rmail-output' which saves in Unix
mailbox format.")

(defvar gnus-user-login-name nil
  "*The login name of the user.
Got from the function `user-login-name' if undefined.")

(defvar gnus-user-full-name nil
  "*The full name of the user.
Got from the NAME environment variable if undefined.")

(defvar gnus-user-from-line nil
  "*Your full, complete e-mail address.  
Overrides the other Gnus variables if it is non-nil.

Here are two example values of this variable:

 \"Lars Magne Ingebrigtsen <larsi@ifi.uio.no>\"

and

 \"larsi@ifi.uio.no (Lars Magne Ingebrigtsen)\"

The first version is recommended, but the name has to be quoted if it
contains non-alphanumerical characters.")

(defvar gnus-signature-file "~/.signature"
  "*Your signature file.
If the variable is a string that doesn't correspond to a file, the
string itself is inserted.")

(defvar gnus-signature-function nil
  "*A function that should return a signature file name.
The function will be called with the name of the newsgroup being
posted to.
If the function returns a string that doesn't correspond to a file, the
string itself is inserted.
If the function returns nil, the `gnus-signature-file' variable will
be used instead.")

(defvar gnus-required-headers
  '(From Date Newsgroups Subject Message-ID Organization Lines X-Newsreader)
  "*Headers to be generated or prompted for when posting an article.
RFC977 and RFC1036 require From, Date, Newsgroups, Subject,
Message-ID.  Organization, Lines and X-Newsreader are optional.  If
you want Gnus not to insert some header, remove it from this list.")

(defvar gnus-deletable-headers '(Message-ID Date)
  "*Headers to be deleted if they already exists and were generated by Gnus previously.")

(defvar gnus-removable-headers '(NNTP-Posting-Host Bcc Xref)
  "*Headers to be removed unconditionally before posting.")

(defvar gnus-check-before-posting 
  '(subject-cmsg multiple-headers sendsys message-id from
		 long-lines control-chars size new-text
		 signature)
  "In non-nil, Gnus will attempt to run some checks on outgoing posts.
If this variable is t, Gnus will check everything it can.  If it is a
list, then those elements in that list will be checked.")

(defvar gnus-delete-supersedes-headers
  "^Path:\\|^Date\\|^NNTP-Posting-Host:\\|^Supersedes:"
  "*Header lines matching this regexp will be deleted before posting.
It's best to delete old Path and Date headers before posting to avoid
any confusion.")

(defvar gnus-auto-mail-to-author nil
  "*If non-nil, mail the authors of articles a copy of your follow-ups.
If this variable is `ask', the user will be prompted for whether to
mail a copy.  The string given by `gnus-mail-courtesy-message' will be
inserted at the beginning of the mail copy.

Mail is sent using the function specified by the
`gnus-mail-send-method' variable.")

;; Added by Ethan Bradford <ethanb@ptolemy.astro.washington.edu>.
(defvar gnus-mail-courtesy-message
  "The following message is a courtesy copy of an article\nthat has been posted as well.\n\n"
  "*This is inserted at the start of a mailed copy of a posted message.
If this variable is nil, no such courtesy message will be added.")

(defvar gnus-mail-reply-method (function gnus-mail-reply-using-mail)
  "*Function to compose a reply.
Three pre-made functions are `gnus-mail-reply-using-mail' (sendmail);
`gnus-mail-reply-using-mhe' (MH-E); and `gnus-mail-reply-using-vm'.")

(defvar gnus-mail-forward-method (function gnus-mail-forward-using-mail)
  "*Function to forward the current message to another user.
Three pre-made functions are `gnus-mail-forward-using-mail' (sendmail);
`gnus-mail-forward-using-mhe' (MH-E); and `gnus-mail-forward-using-vm'.") 

(defvar gnus-mail-other-window-method 'gnus-mail-other-window-using-mail
  "*Function to compose mail in the other window.
Three pre-made functions are `gnus-mail-other-window-using-mail'
(sendmail); `gnus-mail-other-window-using-mhe' (MH-E); and
`gnus-mail-other-window-using-vm'.")

(defvar gnus-mail-send-method send-mail-function
  "*Function to mail a message which is also being posted as an article.
The message must have To or Cc header.  The default is copied from
the variable `send-mail-function'.")

(defvar gnus-inews-article-function 'gnus-inews-article
  "*Function to post an article.")

(defvar gnus-inews-article-hook (list 'gnus-inews-do-fcc)
  "*A hook called before finally posting an article.
The default hook (`gnus-inews-do-fcc') does FCC processing (ie. saves
the article to a file).")

(defvar gnus-inews-article-header-hook nil
  "*A hook called after inserting the headers in an article to be posted.
The hook is called from the *post-news* buffer, narrowed to the
headers.")

(defvar gnus-mail-hook nil
  "*A hook called as the last thing after setting up a mail buffer.")

;;; Internal variables.

(defvar gnus-post-news-buffer "*post-news*")
(defvar gnus-mail-buffer "*mail*")
(defvar gnus-summary-send-map nil)
(defvar gnus-article-copy nil)
(defvar gnus-reply-subject nil)

(eval-and-compile
  (autoload 'gnus-uu-post-news "gnus-uu" nil t)
  (autoload 'rmail-output "rmailout"))


;;;
;;; Gnus Posting Functions
;;;

(define-prefix-command 'gnus-summary-send-map)
(define-key gnus-summary-mode-map "S" 'gnus-summary-send-map)
(define-key gnus-summary-send-map "p" 'gnus-summary-post-news)
(define-key gnus-summary-send-map "f" 'gnus-summary-followup)
(define-key gnus-summary-send-map "F" 'gnus-summary-followup-with-original)
(define-key gnus-summary-send-map "b" 'gnus-summary-followup-and-reply)
(define-key gnus-summary-send-map "B" 'gnus-summary-followup-and-reply-with-original)
(define-key gnus-summary-send-map "c" 'gnus-summary-cancel-article)
(define-key gnus-summary-send-map "s" 'gnus-summary-supersede-article)
(define-key gnus-summary-send-map "r" 'gnus-summary-reply)
(define-key gnus-summary-send-map "R" 'gnus-summary-reply-with-original)
(define-key gnus-summary-send-map "m" 'gnus-summary-mail-other-window)
(define-key gnus-summary-send-map "u" 'gnus-uu-post-news)
(define-key gnus-summary-send-map "om" 'gnus-summary-mail-forward)
(define-key gnus-summary-send-map "op" 'gnus-summary-post-forward)
(define-key gnus-summary-send-map "Om" 'gnus-uu-digest-mail-forward)
(define-key gnus-summary-send-map "Op" 'gnus-uu-digest-post-forward)

;;; Internal functions.

(defun gnus-number-base36 (num len)
  (if (if (< len 0) (<= num 0) (= len 0))
      ""
    (concat (gnus-number-base36 (/ num 36) (1- len))
	    (char-to-string (aref "zyxwvutsrqponmlkjihgfedcba9876543210"
				  (% num 36))))))

;;; Post news commands of Gnus group mode and summary mode

(defun gnus-group-mail ()
  "Start composing a mail."
  (interactive)
  (funcall gnus-mail-other-window-method))

(defun gnus-group-post-news ()
  "Post an article."
  (interactive)
  (let ((gnus-newsgroup-name nil))
    (gnus-post-news 'post nil nil gnus-article-buffer)))

(defun gnus-summary-post-news ()
  "Post an article."
  (interactive)
  (gnus-set-global-variables)
  (gnus-post-news 'post gnus-newsgroup-name))

(defun gnus-summary-followup (yank &optional yank-articles)
  "Compose a followup to an article.
If prefix argument YANK is non-nil, original article is yanked automatically."
  (interactive "P")
  (gnus-set-global-variables)
  (if yank-articles (gnus-summary-goto-subject (car yank-articles)))
  (save-window-excursion
    (gnus-summary-select-article))
  (let ((headers (gnus-get-header-by-number (gnus-summary-article-number)))
	(gnus-newsgroup-name gnus-newsgroup-name))
    ;; Check Followup-To: poster.
    (set-buffer gnus-article-buffer)
    (if (and gnus-use-followup-to
	     (string-equal "poster" (gnus-fetch-field "followup-to"))
	     (or (not (memq gnus-use-followup-to '(t ask)))
		 (not (gnus-y-or-n-p 
		       "Do you want to ignore `Followup-To: poster'? "))))
	;; Mail to the poster. 
	(gnus-summary-reply yank)
      (gnus-post-news nil gnus-newsgroup-name
		      headers gnus-article-buffer 
		      (or yank-articles (not (not yank)))))))

(defun gnus-summary-followup-with-original (n)
  "Compose a followup to an article and include the original article."
  (interactive "P")
  (gnus-summary-followup t (gnus-summary-work-articles n)))

;; Suggested by Daniel Quinlan <quinlan@best.com>.
(defun gnus-summary-followup-and-reply (yank &optional yank-articles)
  "Compose a followup and do an auto mail to author."
  (interactive "P")
  (gnus-set-global-variables)
  (let ((gnus-auto-mail-to-author t))
    (gnus-summary-followup yank yank-articles)))

(defun gnus-summary-followup-and-reply-with-original (n)
  "Compose a followup, include the original, and do an auto mail to author."
  (interactive "P")
  (gnus-summary-followup-and-reply t (gnus-summary-work-articles n)))

(defun gnus-summary-cancel-article (n)
  "Cancel an article you posted."
  (interactive "P")
  (gnus-set-global-variables)
  (let ((articles (gnus-summary-work-articles n)))
    (while articles
      (gnus-summary-select-article t nil nil (car articles))
      (and (gnus-eval-in-buffer-window gnus-article-buffer (gnus-cancel-news))
	   (gnus-summary-mark-as-read (car articles) gnus-canceled-mark))
      (gnus-summary-remove-process-mark (car articles))
      (gnus-article-hide-headers-if-wanted)
      (setq articles (cdr articles)))))

(defun gnus-summary-supersede-article ()
  "Compose an article that will supersede a previous article.
This is done simply by taking the old article and adding a Supersedes
header line with the old Message-ID."
  (interactive)
  (gnus-set-global-variables)
  (gnus-summary-select-article t)
  (if (not
       (string-equal
	(downcase (mail-strip-quoted-names 
		   (mail-header-from gnus-current-headers)))
	(downcase (mail-strip-quoted-names (gnus-inews-user-name)))))
      (error "This article is not yours."))
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (search-forward "\n\n" nil t)
      (if (not (re-search-backward "^Message-ID: " nil t))
	  (error "No Message-ID in this article"))))
  (if (gnus-post-news 'post gnus-newsgroup-name)
      (progn
	(erase-buffer)
	(insert-buffer gnus-article-buffer)
	(if (search-forward "\n\n" nil t)
	    (forward-char -1)
	  (goto-char (point-max)))
	(narrow-to-region (point-min) (point))
	(goto-char (point-min))
	(and gnus-delete-supersedes-headers
	     (delete-matching-lines gnus-delete-supersedes-headers))
	(goto-char (point-min))
	(if (not (re-search-forward "^Message-ID: " nil t))
	    (error "No Message-ID in this article")
	  (replace-match "Supersedes: " t t))
	(goto-char (point-max))
	(insert mail-header-separator)
	(widen)
	(forward-line 1))))


;;;###autoload
(defalias 'sendnews 'gnus-post-news)

;;;###autoload
(defalias 'postnews 'gnus-post-news)

(defun gnus-copy-article-buffer (&optional article-buffer)
  ;; make a copy of the article buffer with all text properties removed
  ;; this copy is in the buffer gnus-article-copy.
  ;; if ARTICLE-BUFFER is nil, gnus-article-buffer is used
  ;; this buffer should be passed to all mail/news reply/post routines.
  (setq gnus-article-copy (get-buffer-create " *gnus article copy*"))
  (buffer-disable-undo gnus-article-copy)
  (or (memq gnus-article-copy gnus-buffer-list)
      (setq gnus-buffer-list (cons gnus-article-copy gnus-buffer-list)))
  (let ((article-buffer (or article-buffer gnus-article-buffer)))
    (if (and (get-buffer article-buffer)
	     (buffer-name (get-buffer article-buffer)))
	(save-excursion
	  (set-buffer article-buffer)
	  (widen)
	  (copy-to-buffer gnus-article-copy (point-min) (point-max))
	  (set-text-properties (point-min) (point-max) 
			       nil gnus-article-copy)))))

(defun gnus-post-news (post &optional group header article-buffer yank subject)
  "Begin editing a new USENET news article to be posted.
Type \\[describe-mode] in the buffer to get a list of commands."
  (interactive (list t))
  (gnus-copy-article-buffer article-buffer)
  (if (or (not gnus-novice-user)
	  gnus-expert-user
	  (not (eq 'post 
		   (nth 1 (assoc 
			   (format "%s" (car (gnus-find-method-for-group 
					      gnus-newsgroup-name)))
			   gnus-valid-select-methods))))
	  (and group
	       (assq 'to-address 
		     (nth 5 (nth 2 (gnus-gethash group gnus-newsrc-hashtb)))))
	  (gnus-y-or-n-p "Are you sure you want to post to all of USENET? "))
      (let ((sumart (if (not post)
			(save-excursion
			  (set-buffer gnus-summary-buffer)
			  (cons (current-buffer) gnus-current-article))))
	    (from (and header (mail-header-from header)))
	    (winconf (current-window-configuration))
	    real-group)
	(and gnus-interactive-post
	     (not gnus-expert-user)
	     post (not group)
	     (progn
	       (setq gnus-newsgroup-name
		     (setq group 
			   (completing-read "Group: " gnus-active-hashtb)))
	       (or subject
		   (setq subject (read-string "Subject: ")))))
	(setq mail-reply-buffer gnus-article-copy)

	(let ((newsgroup-name (or group gnus-newsgroup-name "")))
	  (setq real-group (and group (gnus-group-real-name group)))
	  (setq gnus-post-news-buffer 
		(gnus-request-post-buffer 
		 post real-group subject header gnus-article-copy
		 (nth 2 (and group (gnus-gethash group gnus-newsrc-hashtb)))
		 (or (cdr (assq 'to-group
				(nth 5 (nth 2 (gnus-gethash 
					       newsgroup-name
					       gnus-newsrc-hashtb)))))
		     (if (and (boundp 'gnus-followup-to-function)
			      gnus-followup-to-function
			      gnus-article-copy)
			 (save-excursion
			   (set-buffer gnus-article-copy)
			   (funcall gnus-followup-to-function group))))
		 gnus-use-followup-to))
	  (if post
	      (gnus-configure-windows 'post 'force)
	    (if yank
		(gnus-configure-windows 'followup-yank 'force)
	      (gnus-configure-windows 'followup 'force)))
	  (gnus-overload-functions)
	  (make-local-variable 'gnus-article-reply)
	  (make-local-variable 'gnus-article-check-size)
	  (make-local-variable 'gnus-reply-subject)
	  (setq gnus-reply-subject (and header (mail-header-subject header)))
	  (setq gnus-article-reply sumart)
	  ;; Handle `gnus-auto-mail-to-author'.
	  ;; Suggested by Daniel Quinlan <quinlan@best.com>.
	  ;; Revised to respect Reply-To by Ulrik Dickow <dickow@nbi.dk>.
          (let ((to (and (not post)
			 (if (if (eq gnus-auto-mail-to-author 'ask)
				 (y-or-n-p "Also send mail to author? ")
			       gnus-auto-mail-to-author)
			     (or (save-excursion
				   (set-buffer gnus-article-copy)
				   (gnus-fetch-field "reply-to"))
				 from)))))
	    (if to
		(if (mail-fetch-field "To")
		    (progn
		      (beginning-of-line)
		      (insert "Cc: " to "\n"))
		  (mail-position-on-field "To")
		  (insert to))))
	  ;; Handle author copy using BCC field.
	  (if (and gnus-mail-self-blind
		   (not (mail-fetch-field "bcc")))
	      (progn
		(mail-position-on-field "Bcc")
		(insert (if (stringp gnus-mail-self-blind)
			    gnus-mail-self-blind
			  (user-login-name)))))
	  ;; Handle author copy using FCC field.
	  (if gnus-author-copy
	      (progn
		(mail-position-on-field "Fcc")
		(insert gnus-author-copy)))
	  (goto-char (point-min))
	  (if post 
	      (cond ((not group)
		     (re-search-forward "^Newsgroup:" nil t)
		     (end-of-line))
		    ((not subject)
		     (re-search-forward "^Subject:" nil t)
		     (end-of-line))
		    (t
		     (re-search-forward 
		      (concat "^" (regexp-quote mail-header-separator) "$"))
		     (forward-line 1)))
	    (re-search-forward 
	     (concat "^" (regexp-quote mail-header-separator) "$"))
	    (forward-line 1)
	    (if (not yank)
		()
	      (save-excursion 
		(if (not (listp yank))
		    (news-reply-yank-original nil)
		  (setq yank (reverse yank))
		  (while yank
		    (save-excursion
		      (save-window-excursion
			(set-buffer gnus-summary-buffer)
			(gnus-summary-select-article nil nil nil (car yank))
			(gnus-summary-remove-process-mark (car yank)))
		      (let ((mail-reply-buffer gnus-article-copy))
			(gnus-copy-article-buffer)
			(let ((news-reply-yank-message-id
			       (save-excursion
				 (set-buffer gnus-article-copy)
				 (mail-fetch-field "message-id")))
			      (news-reply-yank-from
			       (save-excursion
				 (set-buffer gnus-article-copy)
				 (mail-fetch-field "from"))))
			  (news-reply-yank-original nil))
			(setq yank (cdr yank)))))))))
	  (if gnus-post-prepare-function
	      (funcall gnus-post-prepare-function group))
	  (run-hooks 'gnus-post-prepare-hook)
	  (make-local-variable 'gnus-prev-winconf)
	  (setq gnus-prev-winconf winconf))))
  (setq gnus-article-check-size (cons (buffer-size) (gnus-article-checksum)))
  (message "")
  t)

(defun gnus-inews-news (&optional use-group-method)
  "Send a news message.
If given a prefix, and the group is a foreign group, this function
will attempt to use the foreign server to post the article."
  (interactive "P")
  (or gnus-current-select-method
      (setq gnus-current-select-method gnus-select-method))
  (let* ((case-fold-search nil)
	 (server-running (gnus-server-opened gnus-current-select-method))
	 (reply gnus-article-reply)
	 error post-result)
    (save-excursion
      ;; Connect to default NNTP server if necessary.
      ;; Suggested by yuki@flab.fujitsu.junet.
      (gnus-start-news-server)		;Use default server.
      ;; NNTP server must be opened before current buffer is modified.
      (widen)
      (goto-char (point-min))
      (run-hooks 'news-inews-hook)
      (save-restriction
	(narrow-to-region
	 (point-min)
	 (progn
	   (goto-char (point-min))
	   (re-search-forward 
	    (concat "^" (regexp-quote mail-header-separator) "$"))
	   (match-beginning 0)))

	;; Correct newsgroups field: change sequence of spaces to comma and 
	;; eliminate spaces around commas.  Eliminate imbedded line breaks.
	(goto-char (point-min))
	(if (re-search-forward "^Newsgroups: +" nil t)
	    (save-restriction
	      (narrow-to-region
	       (point)
	       (if (re-search-forward "^[^ \t]" nil t)
		   (match-beginning 0)
		 (forward-line 1)
		 (point)))
	      (goto-char (point-min))
	      (while (re-search-forward "\n[ \t]+" nil t)
		(replace-match " " t t)) ;No line breaks (too confusing)
	      (goto-char (point-min))
	      (while (re-search-forward "[ \t\n]*,[ \t\n]*\\|[ \t]+" nil t)
		(replace-match "," t t))
	      (goto-char (point-min))
	      ;; Remove a trailing comma.
	      (if (re-search-forward ",$" nil t)
		  (replace-match "" t t))))

	;; Added by Per Abrahamsen <abraham@iesd.auc.dk>.
	;; Help save the the world!
	(or 
	 gnus-expert-user
	 (let ((newsgroups (mail-fetch-field "newsgroups"))
	       (followup-to (mail-fetch-field "followup-to"))
	       groups to)
	   (if (and newsgroups
		    (string-match "," newsgroups) (not followup-to))
	       (progn
		 (while (string-match "," newsgroups)
		   (setq groups
			 (cons (list (substring newsgroups
						0 (match-beginning 0)))
			       groups))
		   (setq newsgroups (substring newsgroups (match-end 0))))
		 (setq groups (nreverse (cons (list newsgroups) groups)))

		 (setq to
		       (completing-read "Followups to: (default all groups) "
					groups))
		 (if (> (length to) 0)
		     (progn
		       (goto-char (point-min))
		       (insert "Followup-To: " to "\n")))))))

	;; Cleanup Followup-To.
	(goto-char (point-min))
	(if (search-forward-regexp "^Followup-To: +" nil t)
	    (save-restriction
	      (narrow-to-region
	       (point)
	       (if (re-search-forward "^[^ \t]" nil 'end)
		   (match-beginning 0)
		 (point-max)))
	      (goto-char (point-min))
	      (replace-regexp "\n[ \t]+" " ") ;No line breaks (too confusing)
	      (goto-char (point-min))
	      (replace-regexp "[ \t\n]*,[ \t\n]*\\|[ \t]+" ",")))

	;; Mail the message too if To:, Bcc:. or Cc: exists.
	(let* ((types '("to" "bcc" "cc"))
	       (ty types)
	       fcc-line)
	  (while ty
	    (or (mail-fetch-field (car ty) nil t)
		(setq types (delete (car ty) types)))
	    (setq ty (cdr ty)))

	  (if (not types)
	      ;; We do not want to send mail.
	      ()
	    (if (not gnus-mail-send-method)
		(progn
		  (ding)
		  (gnus-message 
		   1 "No mailer defined.  To: and/or Cc: fields ignored.")
		  (sit-for 1))
	      (save-excursion
		;; We want to remove Fcc, because we want to handle
		;; that one ourselves...  
		  
		(goto-char (point-min))
		(if (re-search-forward "^Fcc: " nil t)
		    (progn
		      (setq fcc-line
			    (buffer-substring
			     (progn (beginning-of-line) (point))
			     (progn (forward-line 1) (point))))
		      (forward-line -1)
		      (gnus-delete-line)))

		;; We generate a Message-ID so that the mail and the
		;; news copy of the message both get the same ID.
		(or (mail-fetch-field "message-id")
		    (not (memq 'Message-ID gnus-required-headers))
		    (progn
		      (goto-char (point-max))
		      (insert "Message-ID: " (gnus-inews-message-id) "\n")))

		(save-restriction
		  (widen)
		  (gnus-message 5 "Sending via mail...")

		  (if (and gnus-mail-courtesy-message
			   (or (member "to" types)
			       (member "cc" types)))
		      ;; We only want to insert the courtesy mail
		      ;; message if we use to or cc; bcc should not
		      ;; have one. Well, if both bcc and to are
		      ;; present, it will get one anyway.
		      (progn
			;; Insert "courtesy" mail message.
			(goto-char (point-min))
			(re-search-forward
			 (concat "^" (regexp-quote
				      mail-header-separator) "$"))
			(forward-line 1)
			(insert gnus-mail-courtesy-message)
			(funcall gnus-mail-send-method)
			(goto-char (point-min))
			(search-forward gnus-mail-courtesy-message)
			(replace-match "" t t))
		    (funcall gnus-mail-send-method))

		  (gnus-message 5 "Sending via mail...done")
		      
		  (goto-char (point-min))
		  (narrow-to-region
		   (point) 
		   (re-search-forward 
		    (concat "^" (regexp-quote mail-header-separator) "$")))
		  (goto-char (point-min))
		  (while (re-search-forward "^BCC:" nil t)
		    (delete-region (match-beginning 0)
				   ;; There might be continuation headers. 
				   (if (re-search-forward "^[^ \t]" nil t)
				       (match-beginning 0)
				     ;; Uhm... or something like this.
				     (forward-line 1)
				     (point)))))
		(if fcc-line
		    (progn
		      (goto-char (point-max))
		      (insert fcc-line))))))))

      ;; Send to server. 
      (gnus-message 5 "Posting to USENET...")
      (setq post-result (funcall gnus-inews-article-function use-group-method))
      (cond ((eq post-result 'illegal)
	     (setq error t)
	     (ding))
	    (post-result
	     (gnus-message 5 "Posting to USENET...done")
	     (if (gnus-buffer-exists-p (car-safe reply))
		 (progn
		   (save-excursion
		     (set-buffer gnus-summary-buffer)
		     (gnus-summary-mark-article-as-replied 
		      (cdr reply)))))
	     (set-buffer-modified-p nil))
	    (t
	     ;; We cannot signal an error.
	     (setq error t)
	     (ding)
	     (gnus-message 1 "Article rejected: %s" 
			   (gnus-status-message gnus-select-method)))))
    ;; If NNTP server is opened by gnus-inews-news, close it by myself.
    (or server-running
	(gnus-close-server (gnus-find-method-for-group gnus-newsgroup-name)))
    (let ((conf gnus-prev-winconf))
      (if (not error)
	  (progn
	    (bury-buffer)
	    ;; Restore last window configuration.
	    (and conf (set-window-configuration conf)))))))

(defun gnus-inews-check-post ()
  "Check whether the post looks ok."
  (or
   (not gnus-check-before-posting)
   (and 
    ;; We narrow to the headers and check them first.
    (save-excursion
      (save-restriction
	(goto-char (point-min))
	(narrow-to-region 
	 (point) 
	 (progn
	   (re-search-forward 
	    (concat "^" (regexp-quote mail-header-separator) "$"))
	   (match-beginning 0)))
	(goto-char (point-min))
	(and 
	 ;; Check for commands in Subject.
	 (or 
	  (gnus-check-before-posting 'subject-cmsg)
	  (save-excursion
	    (if (string-match "^cmsg " (mail-fetch-field "subject"))
		(gnus-y-or-n-p
		 "The control code \"cmsg \" is in the subject. Really post? ")
	      t)))
	 ;; Check for multiple identical headers.
	 (or (gnus-check-before-posting 'multiple-headers)
	     (save-excursion
	       (let (found)
		 (while (and (not found) (re-search-forward "^[^ \t:]+: "
							    nil t))
		   (save-excursion
		     (or (re-search-forward 
			  (concat "^" (setq found
					    (buffer-substring 
					     (match-beginning 0) 
					     (- (match-end 0) 2))))
			  nil t)
			 (setq found nil))))
		 (if found
		     (gnus-y-or-n-p 
		      (format "Multiple %s headers. Really post? " found))
		   t))))
	 ;; Check for version and sendsys.
	 (or (gnus-check-before-posting 'sendsys)
	     (save-excursion
	       (if (re-search-forward "^Sendsys:\\|^Version:" nil t)
		   (gnus-y-or-n-p
		    (format "The article contains a %s command. Really post? "
			    (buffer-substring (match-beginning 0) 
					      (1- (match-end 0)))))
		 t)))
	 ;; Check the Message-Id header.
	 (or (gnus-check-before-posting 'message-id)
	     (save-excursion
	       (let* ((case-fold-search t)
		      (message-id (mail-fetch-field "message-id")))
		 (or (not message-id)
		     (and (string-match "@" message-id)
			  (string-match "@[^\\.]*\\." message-id))
		     (gnus-y-or-n-p
		      (format 
		       "The Message-ID looks strange: \"%s\". Really post? "
		       message-id))))))
	 ;; Check the From header.
	 (or (gnus-check-before-posting 'from)
	     (save-excursion
	       (let* ((case-fold-search t)
		      (from (mail-fetch-field "from")))
		 (cond
		  ((not from)
		   (gnus-y-or-n-p "There is no From line. Really post? "))
		  ((not (string-match "@[^\\.]*\\." from))
		   (gnus-y-or-n-p
		    (format 
		     "The address looks strange: \"%s\". Really post? " from)))
		  ((string-match "(.*).*(.*)" from)
		   (gnus-y-or-n-p
		    (format
		     "The From header looks strange: \"%s\". Really post? " 
		     from)))
		  (t t)))))
	 )))
    ;; Check for long lines.
    (or (gnus-check-before-posting 'long-lines)
	(save-excursion
	  (goto-char (point-min))
	  (re-search-forward
	   (concat "^" (regexp-quote mail-header-separator) "$"))
	  (while (and
		  (progn
		    (end-of-line)
		    (< (current-column) 80))
		  (zerop (forward-line 1))))
	  (or (bolp)
	      (eobp)
	      (gnus-y-or-n-p
	       (format
		"You have lines longer than 79 characters.  Really post? ")))))
    ;; Check for control characters.
    (or (gnus-check-before-posting 'control-chars)
	(save-excursion
	  (if (re-search-forward "[\000-\007\013\015-\037\200-\237]" nil t)
	      (gnus-y-or-n-p 
	       "The article contains control characters. Really post? ")
	    t)))
    ;; Check excessive size.
    (or (gnus-check-before-posting 'size)
	(if (> (buffer-size) 60000)
	    (gnus-y-or-n-p
	     (format "The article is %d octets long. Really post? "
		     (buffer-size)))
	  t))
    ;; Use the (size . checksum) variable to see whether the
    ;; article is empty or has only quoted text.
    (or
     (gnus-check-before-posting 'new-text)
     (if (and (= (buffer-size) (car gnus-article-check-size))
	      (= (gnus-article-checksum) (cdr gnus-article-check-size)))
	 (gnus-y-or-n-p
	  "It looks like there's no new text in your article. Really post? ")
       t))
    ;; Check the length of the signature.
    (or (gnus-check-before-posting 'signature)
	(progn
	  (goto-char (point-max))
	  (if (not (re-search-backward gnus-signature-separator nil t))
	      t
	    (if (> (count-lines (point) (point-max)) 5)
		(gnus-y-or-n-p
		 (format
		  "Your .sig is %d lines; it should be max 4.  Really post? "
		  (count-lines (point) (point-max))))
	      t)))))))

(defun gnus-article-checksum ()
  (let ((sum 0))
    (save-excursion
      (while (not (eobp))
	(setq sum (logxor sum (following-char)))
	(forward-char 1)))
    sum))

;; Returns non-nil if this type is not to be checked.
(defun gnus-check-before-posting (type)
  (not 
   (or (not gnus-check-before-posting)
       (if (listp gnus-check-before-posting)
	   (memq type gnus-check-before-posting)
	 t))))

(defun gnus-cancel-news ()
  "Cancel an article you posted."
  (interactive)
  (if (or gnus-expert-user
	  (gnus-yes-or-no-p "Do you really want to cancel this article? "))
      (let ((from nil)
	    (newsgroups nil)
	    (message-id nil)
	    (distribution nil))
	(or (gnus-member-of-valid 'post gnus-newsgroup-name)
	    (error "This backend does not support canceling"))
	(save-excursion
	  ;; Get header info. from original article.
	  (save-restriction
	    (gnus-article-show-all-headers)
	    (goto-char (point-min))
	    (search-forward "\n\n" nil 'move)
	    (narrow-to-region (point-min) (point))
	    (setq from (mail-fetch-field "from"))
	    (setq newsgroups (mail-fetch-field "newsgroups"))
	    (setq message-id (mail-fetch-field "message-id"))
	    (setq distribution (mail-fetch-field "distribution")))
	  ;; Verify if the article is absolutely user's by comparing
	  ;; user id with value of its From: field.
	  (if (not
	       (string-equal
		(downcase (mail-strip-quoted-names from))
		(downcase (mail-strip-quoted-names (gnus-inews-user-name)))))
	      (progn
		(ding) (gnus-message 3 "This article is not yours.")
		nil)
	    ;; Make control article.
	    (set-buffer (get-buffer-create " *Gnus-canceling*"))
	    (buffer-disable-undo (current-buffer))
	    (erase-buffer)
	    (insert "Newsgroups: " newsgroups "\n"
		    "Subject: cancel " message-id "\n"
		    "Control: cancel " message-id "\n"
		    (if distribution
			(concat "Distribution: " distribution "\n")
		      "")
		    mail-header-separator "\n"
		    "This is a cancel message from " from ".\n")
	    ;; Send the control article to NNTP server.
	    (gnus-message 5 "Canceling your article...")
	    (prog1
		(if (funcall gnus-inews-article-function)
		    (gnus-message 5 "Canceling your article...done")
		  (progn
		    (ding) 
		    (gnus-message 1 "Cancel failed; %s" 
				  (gnus-status-message gnus-newsgroup-name))
		    nil)
		  t)
	      ;; Kill the article buffer.
	      (kill-buffer (current-buffer))))))))


;;; Lowlevel inews interface

(defun gnus-inews-article (&optional use-group-method)
  "Post an article in current buffer using NNTP protocol."
  (let ((artbuf (current-buffer))
	(tmpbuf (get-buffer-create " *Gnus-posting*")))
    (widen)
    (goto-char (point-max))
    ;; require a newline at the end for inews to append .signature to
    (or (= (preceding-char) ?\n)
	(insert ?\n))
    ;; Prepare article headers.  All message body such as signature
    ;; must be inserted before Lines: field is prepared.
    (save-restriction
      (goto-char (point-min))
      (narrow-to-region 
       (point-min) 
       (save-excursion
	 (re-search-forward 
	  (concat "^" (regexp-quote mail-header-separator) "$"))
	 (match-beginning 0)))
      (gnus-inews-remove-headers)
      (gnus-inews-insert-headers)
      (run-hooks 'gnus-inews-article-header-hook)
      (widen))
    ;; Check whether the article is a good Net Citizen.
    (if (and gnus-article-check-size
	     (not (gnus-inews-check-post)))
	;; Aber nein!
	'illegal
      ;; Looks ok, so we do the nasty.
      (save-excursion
	(set-buffer tmpbuf)
	(buffer-disable-undo (current-buffer))
	(erase-buffer)
	(insert-buffer-substring artbuf)
	;; Remove the header separator.
	(goto-char (point-min))
	(re-search-forward
	 (concat "^" (regexp-quote mail-header-separator) "$"))
	(replace-match "" t t)
	;; This hook may insert a signature.
	(save-excursion
	  (goto-char (point-min))
	  (let ((gnus-newsgroup-name (or (mail-fetch-field "newsgroups")
					 gnus-newsgroup-name)))
	    (run-hooks 'gnus-prepare-article-hook)))
	;; Run final inews hooks.  This hook may do FCC.
	;; The article must be saved before being posted because
	;; `gnus-request-post' modifies the buffer.
	(run-hooks 'gnus-inews-article-hook)
	;; Post an article to NNTP server.
	;; Return NIL if post failed.
	(prog1
	    (gnus-request-post 
	     (if use-group-method
		 (gnus-find-method-for-group gnus-newsgroup-name)
	       gnus-select-method) use-group-method)
	  (kill-buffer (current-buffer)))))))

(defun gnus-inews-remove-headers ()
  (let ((case-fold-search t)
	(headers gnus-removable-headers))
    ;; Remove toxic headers.
    (while headers
      (goto-char (point-min))
      (and (re-search-forward 
	    (concat "^" (downcase (format "%s" (car headers))))
	    nil t)
	   (delete-region (progn (beginning-of-line) (point))
			  (progn (forward-line 1) (point))))
      (setq headers (cdr headers)))))
  
(defun gnus-inews-insert-headers ()
  "Prepare article headers.
Headers already prepared in the buffer are not modified.
Headers in `gnus-required-headers' will be generated."
  (let ((Date (gnus-inews-date))
	(Message-ID (gnus-inews-message-id))
	(Organization (gnus-inews-organization))
	(From (gnus-inews-user-name))
	(Path (gnus-inews-path))
	(Subject nil)
	(Newsgroups nil)
	(Distribution nil)
	(Lines (gnus-inews-lines))
	(X-Newsreader gnus-version)
	(headers gnus-required-headers)
	(case-fold-search t)
	header value elem)
    ;; First we remove any old generated headers.
    (let ((headers gnus-deletable-headers))
      (while headers
	(goto-char (point-min))
	(and (re-search-forward 
	      (concat "^" (symbol-name (car headers)) ": *") nil t)
	     (get-text-property (1+ (match-beginning 0)) 'gnus-deletable)
	     (gnus-delete-line))
	(setq headers (cdr headers))))
    ;; If there are References, and no "Re: ", then the thread has
    ;; changed name. See Son-of-1036.
    (if (and (mail-fetch-field "references")
	     (get-buffer gnus-article-buffer))
	(let ((psubject (gnus-simplify-subject-re
			 (mail-fetch-field "subject"))))
	  (or (and psubject gnus-reply-subject 
		   (string= (gnus-simplify-subject-re gnus-reply-subject)
			    psubject))
	      (progn
		(string-match "@" Message-ID)
		(setq Message-ID
		      (concat (substring Message-ID 0 (match-beginning 0))
			      "_-_" 
			      (substring Message-ID (match-beginning 0))))))))
    ;; Go through all the required headers and see if they are in the
    ;; articles already. If they are not, or are empty, they are
    ;; inserted automatically - except for Subject, Newsgroups and
    ;; Distribution. 
    (while headers
      (goto-char (point-min))
      (setq elem (car headers))
      (if (consp elem)
	  (setq header (car elem))
	(setq header elem))
      (if (or (not (re-search-forward 
		    (concat "^" (downcase (symbol-name header)) ":") nil t))
	      (progn
		;; The header was found. We insert a space after the
		;; colon, if there is none.
		(if (/= (following-char) ? ) (insert " "))
		;; Find out whether the header is empty...
		(looking-at "[ \t]*$")))
	  ;; So we find out what value we should insert.
	  (progn
 	    (setq value 
		  (or (if (consp elem)
			  ;; The element is a cons.  Either the cdr is
			  ;; a string to be inserted verbatim, or it
			  ;; is a function, and we insert the value
			  ;; returned from this function.
			  (or (and (stringp (cdr elem)) (cdr elem))
			      (and (fboundp (cdr elem)) (funcall (cdr elem))))
			;; The element is a symbol.  We insert the
			;; value of this symbol, if any.
			(and (boundp header) (symbol-value header)))
		      ;; We couldn't generate a value for this header,
		      ;; so we just ask the user.
		      (read-from-minibuffer
		       (format "Empty header for %s; enter value: " header))))
	    ;; Finally insert the header.
	    (save-excursion
	      (if (bolp)
		  (progn
		    (goto-char (point-max))
		    (insert (symbol-name header) ": " value "\n")
		    (forward-line -1))
		(replace-match value t t))
	      ;; Add the deletable property to the headers that require it.
	      (and (memq header gnus-deletable-headers)
		   (progn (beginning-of-line) (looking-at "[^:]+: "))
		   (add-text-properties 
		    (point) (match-end 0)
		    '(gnus-deletable t face italic) (current-buffer))))))
      (setq headers (cdr headers)))
    ;; Insert new Sender if the From is strange. 
    (let ((from (mail-fetch-field "from"))
	  (sender (mail-fetch-field "sender")))
      (if (and from 
	       (not (string=
		     (downcase (car (gnus-extract-address-components from)))
		     (downcase (gnus-inews-real-user-address))))
	       (or (null sender)
		   (not 
		    (string=
		     (downcase (car (gnus-extract-address-components sender)))
		     (downcase (gnus-inews-real-user-address))))))
	  (progn
	    (goto-char (point-min))    
	    (and (re-search-forward "^Sender:" nil t)
		 (progn
		   (beginning-of-line)
		   (insert "Original-")
		   (beginning-of-line)))
	    (insert "Sender: " (gnus-inews-real-user-address) "\n"))))))


(defun gnus-inews-insert-signature ()
  "Insert a signature file.
If `gnus-signature-function' is bound and returns a string, this
string is used instead of the variable `gnus-signature-file'.
In either case, if the string is a file name, this file is
inserted. If the string is not a file name, the string itself is
inserted. 

If you never want any signature inserted, set both of these variables to
nil."
  (save-excursion
    (let ((signature 
	   (or (and gnus-signature-function
		    (funcall gnus-signature-function gnus-newsgroup-name))
	       gnus-signature-file)))
      (if (and signature
	       (or (file-exists-p signature)
		   (string-match " " signature)
		   (not (string-match 
			 "^/[^/]+/" (expand-file-name signature)))))
	  (progn
	    (goto-char (point-max))
	    (if (and mail-signature (search-backward "\n-- \n" nil t))
		()
	      ;; Delete any previous signatures.
	      (if (search-backward "\n-- \n" nil t)
		  (delete-region (point) (point-max)))
	      (or (eolp) (insert "\n"))
	      (insert "-- \n")
	      (if (file-exists-p signature)
		  (insert-file-contents signature)
		(insert signature))
	      (goto-char (point-max))
	      (or (bolp) (insert "\n"))))))))

;; Written by "Mr. Per Persson" <pp@solace.mh.se>.
(defun gnus-inews-insert-mime-headers ()
  (let ((mail-header-separator ""))
    (or (mail-position-on-field "Mime-Version")
	(insert "1.0")
	(cond ((save-excursion
		 (beginning-of-buffer)
		 (re-search-forward "[\200-\377]" nil t))
	       (or (mail-position-on-field "Content-Type")
		   (insert "text/plain; charset=ISO-8859-1"))
	       (or (mail-position-on-field "Content-Transfer-Encoding")
		   (insert "8bit")))
	      (t (or (mail-position-on-field "Content-Type")
		     (insert "text/plain; charset=US-ASCII"))
		 (or (mail-position-on-field "Content-Transfer-Encoding")
		     (insert "7bit")))))))

(defun gnus-inews-do-fcc ()
  "Process FCC: fields in current article buffer.
Unless the first character of the field is `|', the article is saved
to the specified file using the function specified by the variable
gnus-author-copy-saver.  The default function rmail-output saves in
Unix mailbox format.
If the first character is `|', the contents of the article is send to
a program specified by the rest of the value."
  (let ((fcc-list nil)
	(fcc-file nil)
	(case-fold-search t))		;Should ignore case.
    (save-excursion
      (save-restriction
	(goto-char (point-min))
	(search-forward "\n\n")
	(narrow-to-region (point-min) (point))
	(goto-char (point-min))
	(while (re-search-forward "^FCC:[ \t]*" nil t)
	  (setq fcc-list
		(cons (buffer-substring
		       (point)
		       (progn
			 (end-of-line)
			 (skip-chars-backward " \t")
			 (point)))
		      fcc-list))
	  (delete-region (match-beginning 0)
			 (progn (forward-line 1) (point))))
	;; Process FCC operations.
	(widen)
	(while fcc-list
	  (setq fcc-file (car fcc-list))
	  (setq fcc-list (cdr fcc-list))
	  (cond ((string-match "^[ \t]*|[ \t]*\\(.*\\)[ \t]*$" fcc-file)
		 (let ((program (substring fcc-file
					   (match-beginning 1) (match-end 1))))
		   ;; Suggested by yuki@flab.fujitsu.junet.
		   ;; Send article to named program.
		   (call-process-region (point-min) (point-max) shell-file-name
					nil nil nil "-c" program)))
		(t
		 ;; Suggested by hyoko@flab.fujitsu.junet.
		 ;; Save article in Unix mail format by default.
		 (gnus-make-directory (file-name-directory fcc-file))
		 (if (and gnus-author-copy-saver
			  (not (eq gnus-author-copy-saver 'rmail-output)))
		     (funcall gnus-author-copy-saver fcc-file)
		   (if (and (file-readable-p fcc-file) 
			    (mail-file-babyl-p fcc-file))
		       (gnus-output-to-rmail fcc-file)
		     (rmail-output fcc-file 1 t t))))))))))

(defun gnus-inews-path ()
  "Return uucp path."
  (let ((login-name (gnus-inews-login-name)))
    (cond ((null gnus-use-generic-path)
	   (concat (nth 1 gnus-select-method) "!" login-name))
	  ((stringp gnus-use-generic-path)
	   ;; Support GENERICPATH.  Suggested by vixie@decwrl.dec.com.
	   (concat gnus-use-generic-path "!" login-name))
	  (t login-name))))

(defun gnus-inews-user-name ()
  "Return user's network address as \"NAME@DOMAIN (FULL-NAME)\"."
  (let ((full-name (gnus-inews-full-name))
	(address (if (or gnus-user-login-name gnus-use-generic-from
			 gnus-local-domain (getenv "DOMAINNAME"))
		     (concat (gnus-inews-login-name) "@"
			     (gnus-inews-domain-name gnus-use-generic-from))
		   user-mail-address))) 
    (or gnus-user-from-line
	(concat address
		;; User's full name.
		(cond ((string-equal full-name "&") ;Unix hack.
		       (concat " (" (user-login-name) ")"))
		      ((string-match "[^ ]+@[^ ]+ +(.*)" address)
		       "")
		      (t
		       (concat " (" full-name ")")))))))

(defun gnus-inews-real-user-address ()
  "Return the \"real\" user address.
This function tries to ignore all user modifications, and 
give as trustworthy answer as possible."
  (concat (user-login-name) "@" (gnus-inews-full-address)))

(defun gnus-inews-login-name ()
  "Return login name."
  (or gnus-user-login-name (getenv "LOGNAME") (user-login-name)))

(defun gnus-inews-full-name ()
  "Return full user name."
  (or gnus-user-full-name (getenv "NAME") (user-full-name)))

(defun gnus-inews-domain-name (&optional genericfrom)
  "Return user's domain name.
If optional argument GENERICFROM is a string, use it as the domain
name; if it is non-nil, strip off local host name from the domain name.
If the function `system-name' returns full internet name and the
domain is undefined, the domain name is got from it."
  (if (or genericfrom gnus-local-domain (getenv "DOMAINNAME"))
      (let* ((system-name (system-name))
	     (domain 
	      (or (if (stringp genericfrom) genericfrom)
		  (getenv "DOMAINNAME")
		  gnus-local-domain
		  ;; Function `system-name' may return full internet name.
		  ;; Suggested by Mike DeCorte <mrd@sun.soe.clarkson.edu>.
		  (if (string-match "\\." system-name)
		      (substring system-name (match-end 0)))
		  (read-string "Domain name (no host): ")))
	     (host (or (if (string-match "\\." system-name)
			   (substring system-name 0 (match-beginning 0)))
		       system-name)))
	(if (string-equal "." (substring domain 0 1))
	    (setq domain (substring domain 1)))
	;; Support GENERICFROM as same as standard Bnews system.
	;; Suggested by ohm@kaba.junet and vixie@decwrl.dec.com.
	(cond ((null genericfrom)
	       (concat host "." domain))
	      ;;((stringp genericfrom) genericfrom)
	      (t domain)))
    (if (string-match "\\." (system-name))
	(system-name)
      (substring user-mail-address 
		 (1+ (string-match "@" user-mail-address))))))

(defun gnus-inews-full-address ()
  (let ((domain (gnus-inews-domain-name))
	(system (system-name))
	(case-fold-search t))
    (if (string-match "\\." system) system
      (if (string-match (concat "^" (regexp-quote system)) domain) domain
	(concat system "." domain)))))

(defun gnus-inews-message-id ()
  "Generate unique Message-ID for user."
  ;; Message-ID should not contain a slash and should be terminated by
  ;; a number.  I don't know the reason why it is so.
  (concat "<" (gnus-inews-unique-id) "@" (gnus-inews-full-address) ">"))

(defvar gnus-unique-id-char nil)

;; If you ever change this function, make sure the new version
;; cannot generate IDs that the old version could.
;; You might for example insert a "." somewhere (not next to another dot
;; or string boundary), or modify the newsreader name to "Ding".
(defun gnus-inews-unique-id ()
  ;; Dont use microseconds from (current-time), they may be unsupported.
  ;; Instead we use this randomly inited counter.
  (setq gnus-unique-id-char
	(% (1+ (or gnus-unique-id-char (logand (random t) (1- (lsh 1 20)))))
	   ;; (current-time) returns 16-bit ints,
	   ;; and 2^16*25 just fits into 4 digits i base 36.
	   (* 25 25)))
  (let ((tm (if (fboundp 'current-time)
		(current-time) '(12191 46742 287898))))
    (concat
     (if (memq system-type '(ms-dos emx vax-vms))
	 (let ((user (downcase (gnus-inews-login-name))))
	   (while (string-match "[^a-z0-9_]" user)
	     (aset user (match-beginning 0) ?_))
	   user)
       (gnus-number-base36 (user-uid) -1))
     (gnus-number-base36 (+ (car   tm) (lsh (% gnus-unique-id-char 25) 16)) 4)
     (gnus-number-base36 (+ (nth 1 tm) (lsh (/ gnus-unique-id-char 25) 16)) 4)
     ;; Append the newsreader name, because while the generated
     ;; ID is unique to this newsreader, other newsreaders might
     ;; otherwise generate the same ID via another algorithm.
     ".fsf")))


(defun gnus-inews-date ()
  "Current time string."
  (timezone-make-date-arpa-standard 
   (current-time-string) (current-time-zone)))

(defun gnus-inews-organization ()
  "Return user's organization.
The ORGANIZATION environment variable is used if defined.
If not, the variable `gnus-local-organization' is used instead.
If it is a function, the function will be called with the current
newsgroup name as the argument.
If this is a file name, the contents of this file will be used as the
organization."
  (let* ((organization 
	  (or (getenv "ORGANIZATION")
	      (if gnus-local-organization
		  (if (and (symbolp gnus-local-organization)
			   (fboundp gnus-local-organization))
		      (funcall gnus-local-organization gnus-newsgroup-name)
		    gnus-local-organization))
	      gnus-organization-file
	      "~/.organization")))
    (and (stringp organization)
	 (> (length organization) 0)
	 (or (file-exists-p organization)
	     (string-match " " organization)
	     (not (string-match "^/usr/lib/" organization)))
	 (save-excursion
	   (gnus-set-work-buffer)
	   (if (file-exists-p organization)
	       (insert-file-contents organization)
	     (insert organization))
	   (goto-char (point-min))
	   (while (re-search-forward " *\n *" nil t)
	     (replace-match " " t t))
	   (buffer-substring (point-min) (point-max))))))

(defun gnus-inews-lines ()
  "Count the number of lines and return numeric string."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (re-search-forward 
       (concat "^" (regexp-quote mail-header-separator) "$"))
      (forward-line 1)
      (int-to-string (count-lines (point) (point-max))))))


;;;
;;; Gnus Mail Functions 
;;;

;;; Mail reply commands of Gnus summary mode

(defun gnus-summary-reply (yank &optional yank-articles)
  "Reply mail to news author.
If prefix argument YANK is non-nil, original article is yanked automatically.
Customize the variable gnus-mail-reply-method to use another mailer."
  (interactive "P")
  ;; Bug fix by jbw@bigbird.bu.edu (Joe Wells)
  ;; Stripping headers should be specified with mail-yank-ignored-headers.
  (gnus-set-global-variables)
  (if yank-articles (gnus-summary-goto-subject (car yank-articles)))
  (gnus-summary-select-article)
  (let ((gnus-newsgroup-name gnus-newsgroup-name))
    (bury-buffer gnus-article-buffer)
    (funcall gnus-mail-reply-method (or yank-articles (not (not yank))))))

(defun gnus-summary-reply-with-original (n)
  "Reply mail to news author with original article.
Customize the variable gnus-mail-reply-method to use another mailer."
  (interactive "P")
  (gnus-summary-reply t (gnus-summary-work-articles n)))

(defun gnus-summary-mail-forward (post)
  "Forward the current message to another user.
Customize the variable gnus-mail-forward-method to use another mailer."
  (interactive "P")
  (gnus-set-global-variables)
  (gnus-summary-select-article)
  (gnus-copy-article-buffer)
  (let ((gnus-newsgroup-name gnus-newsgroup-name))
    (if post
	(gnus-forward-using-post gnus-article-copy)
      (funcall gnus-mail-forward-method gnus-article-copy))))

(defun gnus-summary-post-forward ()
  "Forward the current article to a newsgroup."
  (interactive)
  (gnus-summary-mail-forward t))

(defvar gnus-nastygram-message 
  "The following article was inappropriately posted to %s.\n"
  "Format string to insert in nastygrams.
The current group name will be inserted at \"%s\".")

(defun gnus-summary-mail-nastygram (n)
  "Send a nastygram to the author of the current article."
  (interactive "P")
  (if (or gnus-expert-user
	  (gnus-y-or-n-p 
	   "Really send a nastygram to the author of the current article? "))
      (let ((group gnus-newsgroup-name))
	(gnus-summary-reply-with-original n)
	(set-buffer gnus-mail-buffer)
	(insert (format gnus-nastygram-message group))
	(gnus-mail-send-and-exit))))

(defun gnus-summary-mail-other-window ()
  "Compose mail in other window.
Customize the variable `gnus-mail-other-window-method' to use another
mailer."
  (interactive)
  (gnus-set-global-variables)
  (let ((gnus-newsgroup-name gnus-newsgroup-name))
    (funcall gnus-mail-other-window-method)))

(defun gnus-mail-reply-using-mail (&optional yank to-address)
  (save-excursion
    (set-buffer gnus-summary-buffer)
    (let ((group (gnus-group-real-name gnus-newsgroup-name))
	  (cur (cons (current-buffer) (cdr gnus-article-current)))
	  (winconf (current-window-configuration))
	  from subject date reply-to message-of
	  references message-id sender follow-to sendto elt)
      (set-buffer (get-buffer-create gnus-mail-buffer))
      (mail-mode)
      (make-local-variable 'gnus-article-reply)
      (setq gnus-article-reply cur)
      (make-local-variable 'gnus-prev-winconf)
      (setq gnus-prev-winconf winconf)
      (if (and (buffer-modified-p)
	       (> (buffer-size) 0)
	       (not (gnus-y-or-n-p 
		     "Unsent article being composed; erase it? ")))
	  ()
	(erase-buffer)
	(save-excursion
	  (gnus-copy-article-buffer)
	  (save-restriction
	    (set-buffer gnus-article-copy)
	    (gnus-narrow-to-headers)
	    (if (and (boundp 'gnus-reply-to-function)
		     gnus-reply-to-function)
		(setq follow-to (funcall gnus-reply-to-function group)))
	    (setq from (mail-fetch-field "from"))
	    (setq date (or (mail-fetch-field "date") 
			   (mail-header-date gnus-current-headers)))
	    (and from
		 (let ((stop-pos 
			(string-match "  *at \\|  *@ \\| *(\\| *<" from)))
		   (setq message-of
			 (concat (if stop-pos (substring from 0 stop-pos) from)
				 "'s message of " date))))
	    (setq sender (mail-fetch-field "sender"))
	    (setq subject (or (mail-fetch-field "subject")
			      "Re: none"))
	    (or (string-match "^[Rr][Ee]:" subject)
		(setq subject (concat "Re: " subject)))
	    (setq reply-to (mail-fetch-field "reply-to"))
	    (setq references (mail-fetch-field "references"))
	    (setq message-id (mail-fetch-field "message-id"))
	    (widen))
	  (setq news-reply-yank-from (or from "(nobody)")))
	(setq news-reply-yank-message-id
	      (or message-id "(unknown Message-ID)"))

	;; Gather the "to" addresses out of the follow-to list and remove
	;; them as we go.
	(if (and follow-to (listp follow-to))
	    (while (setq elt (assoc "To" follow-to))
	      (setq sendto (concat sendto (and sendto ", ") (cdr elt)))
	      (setq follow-to (delq elt follow-to))))

	(mail-setup (or to-address 
			(if (and follow-to (not (stringp follow-to))) sendto
			  (or follow-to reply-to from sender "")))
		    subject message-of nil gnus-article-copy nil)

	(auto-save-mode auto-save-default)
	(use-local-map (copy-keymap mail-mode-map))
	(local-set-key "\C-c\C-c" 'gnus-mail-send-and-exit)

	(if (and follow-to (listp follow-to))
	    (progn
	      (goto-char (point-min))
	      (re-search-forward "^To:" nil t)
	      (beginning-of-line)
	      (forward-line 1)
	      (while follow-to
		(insert (car (car follow-to)) ": " (cdr (car follow-to)) "\n")
		(setq follow-to (cdr follow-to)))))
	(nnheader-insert-references references message-id)
	(goto-char (point-min))
	(re-search-forward
	 (concat "^" (regexp-quote mail-header-separator) "$"))
	(forward-line 1)
	(if (not yank)
	    (gnus-configure-windows 'reply 'force)
	  (let ((last (point))
		end)
	    (if (not (listp yank))
		(progn
		  (save-excursion
		    (mail-yank-original nil))
		  (or mail-yank-hooks mail-citation-hook
		      (run-hooks 'news-reply-header-hook)))
	      (while yank
		(save-window-excursion
		  (set-buffer gnus-summary-buffer)
		  (gnus-summary-select-article nil nil nil (car yank))
		  (gnus-summary-remove-process-mark (car yank)))
		(save-excursion
		  (gnus-copy-article-buffer)
		  (mail-yank-original nil)
		  (setq end (point)))
		(or mail-yank-hooks mail-citation-hook
		    (run-hooks 'news-reply-header-hook))
		(goto-char end)
		(setq yank (cdr yank))))
	    (goto-char last))
	  (gnus-configure-windows 'reply-yank 'force))
	(run-hooks 'gnus-mail-hook)))))

(defun gnus-mail-yank-original ()
  (interactive)
  (save-excursion
    (mail-yank-original nil))
  (or mail-yank-hooks mail-citation-hook
      (run-hooks 'news-reply-header-hook)))

(defun gnus-mail-send-and-exit ()
  (interactive)
  (let ((reply gnus-article-reply)
	(winconf gnus-prev-winconf))
    (mail-send-and-exit nil)
    (if (get-buffer gnus-group-buffer)
	(progn
	  (if (gnus-buffer-exists-p (car-safe reply))
	      (progn
		(set-buffer (car reply))
		(and (cdr reply)
		     (gnus-summary-mark-article-as-replied 
		      (cdr reply)))))
	  (and winconf (set-window-configuration winconf))))))

(defun gnus-forward-make-subject (buffer)
  (save-excursion
    (set-buffer buffer)
    (concat "[" (if (memq 'mail (assoc (symbol-name 
					(car (gnus-find-method-for-group 
					      gnus-newsgroup-name)))
				       gnus-valid-select-methods))
		    (gnus-fetch-field "From")
		  gnus-newsgroup-name)
	    "] " (or (gnus-fetch-field "Subject") ""))))

(defun gnus-forward-insert-buffer (buffer)
  (let ((beg (goto-char (point-max))))
    (insert "------- Start of forwarded message -------\n")
    (insert-buffer buffer)
    (goto-char (point-max))
    (insert "------- End of forwarded message -------\n")
    ;; Suggested by Sudish Joseph <joseph@cis.ohio-state.edu>. 
    (goto-char beg)
    (while (setq beg (next-single-property-change (point) 'invisible))
      (goto-char beg)
      (delete-region beg (or (next-single-property-change 
			      (point) 'invisible)
			     (point-max))))))

(defun gnus-mail-forward-using-mail (&optional buffer)
  "Forward the current message to another user using mail."
  ;; This is almost a carbon copy of rmail-forward in rmail.el.
  (let* ((forward-buffer (or buffer (current-buffer)))
	 (winconf (current-window-configuration))
	 (subject (gnus-forward-make-subject forward-buffer)))
    (set-buffer forward-buffer)
    (mail nil nil subject)
    (use-local-map (copy-keymap (current-local-map)))
    (local-set-key "\C-c\C-c" 'gnus-mail-send-and-exit)
    (make-local-variable 'gnus-prev-winconf)
    (setq gnus-prev-winconf winconf)
    (gnus-forward-insert-buffer forward-buffer)
    (goto-char (point-min))
    (re-search-forward "^To: " nil t)
    (gnus-configure-windows 'mail-forward 'force)
    ;; You have a chance to arrange the message.
    (run-hooks 'gnus-mail-forward-hook)
    (run-hooks 'gnus-mail-hook)))

(defun gnus-forward-using-post (&optional buffer)
  (save-excursion
    (let* ((forward-buffer (or buffer (current-buffer))) 
	   (subject (gnus-forward-make-subject forward-buffer))
	   (gnus-newsgroup-name nil))
      (gnus-post-news 'post nil nil nil nil subject)
      (save-excursion
	(gnus-forward-insert-buffer forward-buffer)
	;; You have a chance to arrange the message.
	(run-hooks 'gnus-mail-forward-hook)))))

(defun gnus-mail-other-window-using-mail ()
  "Compose mail other window using mail."
  (let ((winconf (current-window-configuration)))
    (mail-other-window nil nil nil nil nil (get-buffer gnus-article-buffer))
    (use-local-map (copy-keymap (current-local-map)))
    (local-set-key "\C-c\C-c" 'gnus-mail-send-and-exit)
    (make-local-variable 'gnus-prev-winconf)
    (setq gnus-prev-winconf winconf)
    (run-hooks 'gnus-mail-hook)
    (gnus-configure-windows 'summary-mail 'force)))

(defun gnus-article-mail (yank)
  "Send a reply to the address near point.
If YANK is non-nil, include the original article."
  (interactive "P")
  (let ((address 
	 (buffer-substring
	  (save-excursion (re-search-backward "[ \t\n]" nil t) (1+ (point)))
	  (save-excursion (re-search-forward "[ \t\n]" nil t) (1- (point))))))
    (and address
	 (progn
	   (switch-to-buffer gnus-summary-buffer)
	   (funcall gnus-mail-reply-method yank address)))))

(defun gnus-bug ()
  "Send a bug report to the Gnus maintainers."
  (interactive)
  (let ((winconf (current-window-configuration)))
    (delete-other-windows)
    (switch-to-buffer "*Gnus Help Bug*")
    (erase-buffer)
    (insert gnus-bug-message)
    (goto-char (point-min))
    (pop-to-buffer "*Gnus Bug*")
    (erase-buffer)
    (mail-mode)
    (mail-setup gnus-maintainer nil nil nil nil nil)
    (auto-save-mode auto-save-default)
    (make-local-variable 'gnus-prev-winconf)
    (setq gnus-prev-winconf winconf)
    (use-local-map (copy-keymap mail-mode-map))
    (local-set-key "\C-c\C-c" 'gnus-bug-mail-send-and-exit)
    (goto-char (point-min))
    (re-search-forward (concat "^" (regexp-quote mail-header-separator) "$"))
    (forward-line 1)
    (insert (format "%s\n%s\n\n\n\n\n" (gnus-version) (emacs-version)))
    (gnus-debug)
    (goto-char (point-min))
    (search-forward "Subject: " nil t)
    (message "")))

(defun gnus-bug-mail-send-and-exit ()
  "Send the bug message and exit."
  (interactive)
  (and (get-buffer "*Gnus Help Bug*")
       (kill-buffer "*Gnus Help Bug*"))
  (gnus-mail-send-and-exit))

(defun gnus-debug ()
  "Attemps to go through the Gnus source file and report what variables have been changed.
The source file has to be in the Emacs load path."
  (interactive)
  (let ((files '("gnus.el" "gnus-msg.el" "gnus-score.el"))
	file dirs expr olist sym)
    (message "Please wait while we snoop your variables...")
    (sit-for 0)
    (save-excursion
      (set-buffer (get-buffer-create " *gnus bug info*"))
      (buffer-disable-undo (current-buffer))
      (while files
	(erase-buffer)
	(setq dirs load-path)
	(while dirs
	  (if (or (not (car dirs))
		  (not (stringp (car dirs)))
		  (not (file-exists-p 
			(setq file (concat (file-name-as-directory 
					    (car dirs)) (car files))))))
	      (setq dirs (cdr dirs))
	    (setq dirs nil)
	    (insert-file-contents file)
	    (goto-char (point-min))
	    (or (re-search-forward "^;;* *Internal variables" nil t)
		(error "Malformed sources in file %s" file))
	    (narrow-to-region (point-min) (point))
	    (goto-char (point-min))
	    (while (setq expr (condition-case () 
				  (read (current-buffer)) (error nil)))
	      (condition-case ()
		  (and (eq (car expr) 'defvar)
		       (stringp (nth 3 expr))
		       (or (not (boundp (nth 1 expr)))
			   (not (equal (eval (nth 2 expr))
				       (symbol-value (nth 1 expr)))))
		       (setq olist (cons (nth 1 expr) olist)))
		(error nil)))))
	(setq files (cdr files)))
      (kill-buffer (current-buffer)))
    (insert "------------------- Environment follows -------------------\n\n")
    (while olist
      (if (boundp (car olist))
	  (insert "(setq " (symbol-name (car olist)) 
		  (if (or (consp (setq sym (symbol-value (car olist))))
			  (and (symbolp sym)
			       (not (or (eq sym nil)
					(eq sym t)))))
		      " '" " ")
		  (prin1-to-string (symbol-value (car olist))) ")\n")
	(insert ";; (makeunbound '" (symbol-name (car olist)) ")\n"))
      (setq olist (cdr olist)))
    (insert "\n\n")
    ;; Remove any null chars - they seem to cause trouble for some
    ;; mailers. (Byte-compiled output from the stuff above.) 
    (goto-char (point-min))
    (while (re-search-forward "[\000\200]" nil t)
      (replace-match "" t t))))

(gnus-ems-redefine)

(provide 'gnus-msg)

;;; gnus-msg.el ends here
