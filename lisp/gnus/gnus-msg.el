;;; gnus-msg.el --- mail and post interface for Gnus
;; Copyright (C) 1995,96,97 Free Software Foundation, Inc.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))

(require 'gnus)
(require 'gnus-ems)
(require 'message)
(require 'gnus-art)

;; Added by Sudish Joseph <joseph@cis.ohio-state.edu>.
(defvar gnus-post-method nil
  "*Preferred method for posting USENET news.
If this variable is nil, Gnus will use the current method to decide
which method to use when posting.  If it is non-nil, it will override
the current method.  This method will not be used in mail groups and
the like, only in \"real\" newsgroups.

The value must be a valid method as discussed in the documentation of
`gnus-select-method'.  It can also be a list of methods.  If that is
the case, the user will be queried for what select method to use when
posting.")

(defvar gnus-outgoing-message-group nil
  "*All outgoing messages will be put in this group.
If you want to store all your outgoing mail and articles in the group
\"nnml:archive\", you set this variable to that value.  This variable
can also be a list of group names.

If you want to have greater control over what group to put each
message in, you can set this variable to a function that checks the
current newsgroup name and then returns a suitable group name (or list
of names).")

(defvar gnus-mailing-list-groups nil
  "*Regexp matching groups that are really mailing lists.
This is useful when you're reading a mailing list that has been
gatewayed to a newsgroup, and you want to followup to an article in
the group.")

(defvar gnus-add-to-list nil
  "*If non-nil, add a `to-list' parameter automatically.")

(defvar gnus-sent-message-ids-file
  (nnheader-concat gnus-directory "Sent-Message-IDs")
  "File where Gnus saves a cache of sent message ids.")

(defvar gnus-sent-message-ids-length 1000
  "The number of sent Message-IDs to save.")

(defvar gnus-crosspost-complaint
  "Hi,

You posted the article below with the following Newsgroups header:

Newsgroups: %s

The %s group, at least, was an inappropriate recipient
of this message.  Please trim your Newsgroups header to exclude this
group before posting in the future.

Thank you.

"
  "Format string to be inserted when complaining about crossposts.
The first %s will be replaced by the Newsgroups header;
the second with the current group name.")

(defvar gnus-message-setup-hook nil
  "Hook run after setting up a message buffer.")

;;; Internal variables.

(defvar gnus-message-buffer "*Mail Gnus*")
(defvar gnus-article-copy nil)
(defvar gnus-last-posting-server nil)

(defconst gnus-bug-message
  "Sending a bug report to the Gnus Towers.
========================================

The buffer below is a mail buffer.  When you press `C-c C-c', it will
be sent to the Gnus Bug Exterminators.

At the bottom of the buffer you'll see lots of variable settings.
Please do not delete those.  They will tell the Bug People what your
environment is, so that it will be easier to locate the bugs.

If you have found a bug that makes Emacs go \"beep\", set
debug-on-error to t (`M-x set-variable RET debug-on-error RET t RET')
and include the backtrace in your bug report.

Please describe the bug in annoying, painstaking detail.

Thank you for your help in stamping out bugs.
")

(eval-and-compile
  (autoload 'gnus-uu-post-news "gnus-uu" nil t)
  (autoload 'news-setup "rnewspost")
  (autoload 'news-reply-mode "rnewspost")
  (autoload 'rmail-dont-reply-to "mail-utils")
  (autoload 'rmail-output "rmailout"))


;;;
;;; Gnus Posting Functions
;;;

(gnus-define-keys (gnus-summary-send-map "S" gnus-summary-mode-map)
  "p" gnus-summary-post-news
  "f" gnus-summary-followup
  "F" gnus-summary-followup-with-original
  "c" gnus-summary-cancel-article
  "s" gnus-summary-supersede-article
  "r" gnus-summary-reply
  "R" gnus-summary-reply-with-original
  "w" gnus-summary-wide-reply
  "W" gnus-summary-wide-reply-with-original
  "n" gnus-summary-followup-to-mail
  "N" gnus-summary-followup-to-mail-with-original
  "m" gnus-summary-mail-other-window
  "u" gnus-uu-post-news
  "\M-c" gnus-summary-mail-crosspost-complaint
  "om" gnus-summary-mail-forward
  "op" gnus-summary-post-forward
  "Om" gnus-uu-digest-mail-forward
  "Op" gnus-uu-digest-post-forward)

(gnus-define-keys (gnus-send-bounce-map "D" gnus-summary-send-map)
  "b" gnus-summary-resend-bounced-mail
  ;; "c" gnus-summary-send-draft
  "r" gnus-summary-resend-message)

;;; Internal functions.

(defvar gnus-article-reply nil)
(defmacro gnus-setup-message (config &rest forms)
  (let ((winconf (make-symbol "winconf"))
	(buffer (make-symbol "buffer"))
	(article (make-symbol "article")))
    `(let ((,winconf (current-window-configuration))
	   (,buffer (buffer-name (current-buffer)))
	   (,article (and gnus-article-reply (gnus-summary-article-number)))
	   (message-header-setup-hook
	    (copy-sequence message-header-setup-hook)))
       (add-hook 'message-header-setup-hook 'gnus-inews-insert-gcc)
       (add-hook 'message-header-setup-hook 'gnus-inews-insert-archive-gcc)
       (unwind-protect
	   ,@forms
	 (gnus-inews-add-send-actions ,winconf ,buffer ,article)
	 (setq gnus-message-buffer (current-buffer))
	 (make-local-variable 'gnus-newsgroup-name)
	 (run-hooks 'gnus-message-setup-hook))
       (gnus-configure-windows ,config t)
       (set-buffer-modified-p nil))))

(defun gnus-inews-add-send-actions (winconf buffer article)
  (make-local-hook 'message-sent-hook)
  (add-hook 'message-sent-hook 'gnus-inews-do-gcc nil t)
  (setq message-post-method
	`(lambda (arg)
	   (gnus-post-method arg ,gnus-newsgroup-name)))
  (setq message-newsreader (setq message-mailer (gnus-extended-version)))
  (message-add-action
   `(set-window-configuration ,winconf) 'exit 'postpone 'kill)
  (message-add-action
   `(when (buffer-name (get-buffer ,buffer))
      (save-excursion
	(set-buffer (get-buffer ,buffer))
	,(when article
	   `(gnus-summary-mark-article-as-replied ,article))))
   'send))

(put 'gnus-setup-message 'lisp-indent-function 1)
(put 'gnus-setup-message 'edebug-form-spec '(form body))

;;; Post news commands of Gnus group mode and summary mode

(defun gnus-group-mail ()
  "Start composing a mail."
  (interactive)
  (gnus-setup-message 'message
    (message-mail)))

(defun gnus-group-post-news (&optional arg)
  "Start composing a news message.
If ARG, post to the group under point.
If ARG is 1, prompt for a group name."
  (interactive "P")
  ;; Bind this variable here to make message mode hooks
  ;; work ok.
  (let ((gnus-newsgroup-name
	 (if arg
	     (if (= 1 (prefix-numeric-value arg))
		 (completing-read "Newsgroup: " gnus-active-hashtb nil
				  (gnus-read-active-file-p))
	       (gnus-group-group-name))
	   "")))
    (gnus-post-news 'post gnus-newsgroup-name)))

(defun gnus-summary-post-news ()
  "Start composing a news message."
  (interactive)
  (gnus-set-global-variables)
  (gnus-post-news 'post gnus-newsgroup-name))

(defun gnus-summary-followup (yank &optional force-news)
  "Compose a followup to an article.
If prefix argument YANK is non-nil, original article is yanked automatically."
  (interactive
   (list (and current-prefix-arg
	      (gnus-summary-work-articles 1))))
  (gnus-set-global-variables)
  (when yank
    (gnus-summary-goto-subject (car yank)))
  (save-window-excursion
    (gnus-summary-select-article))
  (let ((headers (gnus-summary-article-header (gnus-summary-article-number)))
	(gnus-newsgroup-name gnus-newsgroup-name))
    ;; Send a followup.
    (gnus-post-news nil gnus-newsgroup-name
		    headers gnus-article-buffer
		    yank nil force-news)))

(defun gnus-summary-followup-with-original (n &optional force-news)
  "Compose a followup to an article and include the original article."
  (interactive "P")
  (gnus-summary-followup (gnus-summary-work-articles n) force-news))

(defun gnus-summary-followup-to-mail (&optional arg)
  "Followup to the current mail message via news."
  (interactive
   (list (and current-prefix-arg
	      (gnus-summary-work-articles 1))))
  (gnus-summary-followup arg t))

(defun gnus-summary-followup-to-mail-with-original (&optional arg)
  "Followup to the current mail message via news."
  (interactive "P")
  (gnus-summary-followup (gnus-summary-work-articles arg) t))

(defun gnus-inews-yank-articles (articles)
  (let (beg article)
    (message-goto-body)
    (while (setq article (pop articles))
      (save-window-excursion
	(set-buffer gnus-summary-buffer)
	(gnus-summary-select-article nil nil nil article)
	(gnus-summary-remove-process-mark article))
      (gnus-copy-article-buffer)
      (let ((message-reply-buffer gnus-article-copy)
	    (message-reply-headers gnus-current-headers))
	(message-yank-original)
	(setq beg (or beg (mark t))))
      (when articles
	(insert "\n")))
    (push-mark)
    (goto-char beg)))

(defun gnus-summary-cancel-article (n)
  "Cancel an article you posted."
  (interactive "P")
  (gnus-set-global-variables)
  (let ((articles (gnus-summary-work-articles n))
	(message-post-method
	 `(lambda (arg)
	    (gnus-post-method nil ,gnus-newsgroup-name)))
	article)
    (while (setq article (pop articles))
      (when (gnus-summary-select-article t nil nil article)
	(when (gnus-eval-in-buffer-window gnus-original-article-buffer
		(message-cancel-news))
	  (gnus-summary-mark-as-read article gnus-canceled-mark)
	  (gnus-cache-remove-article 1))
	(gnus-article-hide-headers-if-wanted))
      (gnus-summary-remove-process-mark article))))

(defun gnus-summary-supersede-article ()
  "Compose an article that will supersede a previous article.
This is done simply by taking the old article and adding a Supersedes
header line with the old Message-ID."
  (interactive)
  (gnus-set-global-variables)
  (let ((article (gnus-summary-article-number)))
    (gnus-setup-message 'reply-yank
      (gnus-summary-select-article t)
      (set-buffer gnus-original-article-buffer)
      (message-supersede)
      (push
       `((lambda ()
	   (when (buffer-name (get-buffer ,gnus-summary-buffer))
	     (save-excursion
	       (set-buffer (get-buffer ,gnus-summary-buffer))
	       (gnus-cache-possibly-remove-article ,article nil nil nil t)
	       (gnus-summary-mark-as-read ,article gnus-canceled-mark)))))
       message-send-actions))))



(defun gnus-copy-article-buffer (&optional article-buffer)
  ;; make a copy of the article buffer with all text properties removed
  ;; this copy is in the buffer gnus-article-copy.
  ;; if ARTICLE-BUFFER is nil, gnus-article-buffer is used
  ;; this buffer should be passed to all mail/news reply/post routines.
  (setq gnus-article-copy (get-buffer-create " *gnus article copy*"))
  (buffer-disable-undo gnus-article-copy)
  (or (memq gnus-article-copy gnus-buffer-list)
      (push gnus-article-copy gnus-buffer-list))
  (let ((article-buffer (or article-buffer gnus-article-buffer))
	end beg contents)
    (if (not (and (get-buffer article-buffer)
		  (buffer-name (get-buffer article-buffer))))
	(error "Can't find any article buffer")
      (save-excursion
	(set-buffer article-buffer)
	(save-restriction
	  ;; Copy over the (displayed) article buffer, delete
	  ;; hidden text and remove text properties.
	  (widen)
	  (copy-to-buffer gnus-article-copy (point-min) (point-max))
	  (set-buffer gnus-article-copy)
	  (gnus-article-delete-text-of-type 'annotation)
	  (gnus-remove-text-with-property 'gnus-prev)
	  (gnus-remove-text-with-property 'gnus-next)
	  (insert
	   (prog1
	       (format "%s" (buffer-string))
	     (erase-buffer)))
	  ;; Find the original headers.
	  (set-buffer gnus-original-article-buffer)
	  (goto-char (point-min))
	  (while (looking-at message-unix-mail-delimiter)
	    (forward-line 1))
	  (setq beg (point))
	  (setq end (or (search-forward "\n\n" nil t) (point)))
	  ;; Delete the headers from the displayed articles.
	  (set-buffer gnus-article-copy)
	  (delete-region (goto-char (point-min))
			 (or (search-forward "\n\n" nil t) (point)))
	  ;; Insert the original article headers.
	  (insert-buffer-substring gnus-original-article-buffer beg end)
	  (gnus-article-decode-rfc1522)))
      gnus-article-copy)))

(defun gnus-post-news (post &optional group header article-buffer yank subject
			    force-news)
  (when article-buffer
    (gnus-copy-article-buffer))
  (let ((gnus-article-reply article-buffer)
	(add-to-list gnus-add-to-list))
    (gnus-setup-message (cond (yank 'reply-yank)
			      (article-buffer 'reply)
			      (t 'message))
      (let* ((group (or group gnus-newsgroup-name))
	     (pgroup group)
	     to-address to-group mailing-list to-list
	     newsgroup-p)
	(when group
	  (setq to-address (gnus-group-find-parameter group 'to-address)
		to-group (gnus-group-find-parameter group 'to-group)
		to-list (gnus-group-find-parameter group 'to-list)
		newsgroup-p (gnus-group-find-parameter group 'newsgroup)
		mailing-list (when gnus-mailing-list-groups
			       (string-match gnus-mailing-list-groups group))
		group (gnus-group-real-name group)))
	(if (or (and to-group
		     (gnus-news-group-p to-group))
		newsgroup-p
		force-news
		(and (gnus-news-group-p
		      (or pgroup gnus-newsgroup-name)
		      (if header (mail-header-number header)
			gnus-current-article))
		     (not mailing-list)
		     (not to-list)
		     (not to-address)))
	    ;; This is news.
	    (if post
		(message-news (or to-group group))
	      (set-buffer gnus-article-copy)
	      (message-followup (if (or newsgroup-p force-news) nil to-group)))
	  ;; The is mail.
	  (if post
	      (progn
		(message-mail (or to-address to-list))
		;; Arrange for mail groups that have no `to-address' to
		;; get that when the user sends off the mail.
		(when (and (not to-list)
			   (not to-address)
			   add-to-list)
		  (push (list 'gnus-inews-add-to-address pgroup)
			message-send-actions)))
	    (set-buffer gnus-article-copy)
	    (message-wide-reply to-address
				(gnus-group-find-parameter
				 gnus-newsgroup-name 'broken-reply-to))))
	(when yank
	  (gnus-inews-yank-articles yank))))))

(defun gnus-post-method (arg group &optional silent)
  "Return the posting method based on GROUP and ARG.
If SILENT, don't prompt the user."
  (let ((group-method (gnus-find-method-for-group group)))
    (cond
     ;; If the group-method is nil (which shouldn't happen) we use
     ;; the default method.
     ((null group-method)
      (or gnus-post-method gnus-select-method message-post-method))
     ;; We want this group's method.
     ((and arg (not (eq arg 0)))
      group-method)
     ;; We query the user for a post method.
     ((or arg
	  (and gnus-post-method
	       (listp (car gnus-post-method))))
      (let* ((methods
	      ;; Collect all methods we know about.
	      (append
	       (when gnus-post-method
		 (if (listp (car gnus-post-method))
		     gnus-post-method
		   (list gnus-post-method)))
	       gnus-secondary-select-methods
	       (list gnus-select-method)
	       (list group-method)))
	     method-alist post-methods method)
	;; Weed out all mail methods.
	(while methods
	  (setq method (gnus-server-get-method "" (pop methods)))
	  (when (or (gnus-method-option-p method 'post)
		    (gnus-method-option-p method 'post-mail))
	    (push method post-methods)))
	;; Create a name-method alist.
	(setq method-alist
	      (mapcar
	       (lambda (m)
		 (list (concat (cadr m) " (" (symbol-name (car m)) ")") m))
	       post-methods))
	;; Query the user.
	(cadr
	 (assoc
	  (setq gnus-last-posting-server
		(if (and silent
			 gnus-last-posting-server)
		    ;; Just use the last value.
		    gnus-last-posting-server
		  (completing-read
		   "Posting method: " method-alist nil t
		   (cons (or gnus-last-posting-server "") 0))))
	  method-alist))))
     ;; Override normal method.
     (gnus-post-method
      gnus-post-method)
     ;; Use the normal select method.
     (t gnus-select-method))))

;;;
;;; Check whether the message has been sent already.
;;;

(defvar gnus-inews-sent-ids nil)

(defun gnus-inews-reject-message ()
  "Check whether this message has already been sent."
  (when gnus-sent-message-ids-file
    (let ((message-id (save-restriction (message-narrow-to-headers)
					(mail-fetch-field "message-id")))
	  end)
      (when message-id
	(unless gnus-inews-sent-ids
	  (ignore-errors
	    (load t t t)))
	(if (member message-id gnus-inews-sent-ids)
	    ;; Reject this message.
	    (not (gnus-yes-or-no-p
		  (format "Message %s already sent.  Send anyway? "
			  message-id)))
	  (push message-id gnus-inews-sent-ids)
	  ;; Chop off the last Message-IDs.
	  (when (setq end (nthcdr gnus-sent-message-ids-length
				  gnus-inews-sent-ids))
	    (setcdr end nil))
	  (nnheader-temp-write gnus-sent-message-ids-file
	    (gnus-prin1 `(setq gnus-inews-sent-ids ',gnus-inews-sent-ids)))
	  nil)))))



;; Dummy to avoid byte-compile warning.
(defvar nnspool-rejected-article-hook)
(defvar xemacs-codename)

;;; Since the X-Newsreader/X-Mailer are ``vanity'' headers, they might
;;; as well include the Emacs version as well.
;;; The following function works with later GNU Emacs, and XEmacs.
(defun gnus-extended-version ()
  "Stringified Gnus version and Emacs version"
  (interactive)
  (concat
   gnus-version
   "/"
   (cond
    ((string-match "^\\([0-9]+\\.[0-9]+\\)\\.[.0-9]+$" emacs-version)
     (concat "Emacs " (substring emacs-version
				 (match-beginning 1)
				 (match-end 1))))
    ((string-match "\\([A-Z]*[Mm][Aa][Cc][Ss]\\)[^(]*\\(\\((beta.*)\\|'\\)\\)?"
		   emacs-version)
     (concat (substring emacs-version
			(match-beginning 1)
			(match-end 1))
	     (format " %d.%d" emacs-major-version emacs-minor-version)
	     (if (match-beginning 3)
		 (substring emacs-version
			    (match-beginning 3)
			    (match-end 3))
	       "")
	     (if (boundp 'xemacs-codename)
		 (concat " - \"" xemacs-codename "\""))))
    (t emacs-version))))

;; Written by "Mr. Per Persson" <pp@gnu.ai.mit.edu>.
(defun gnus-inews-insert-mime-headers ()
  (goto-char (point-min))
  (let ((mail-header-separator
	 (progn
	   (goto-char (point-min))
	   (if (and (search-forward (concat "\n" mail-header-separator "\n")
				    nil t)
		    (not (search-backward "\n\n" nil t)))
	       mail-header-separator
	     ""))))
    (or (mail-position-on-field "Mime-Version")
	(insert "1.0")
	(cond ((save-restriction
		 (widen)
		 (goto-char (point-min))
		 (re-search-forward "[\200-\377]" nil t))
	       (or (mail-position-on-field "Content-Type")
		   (insert "text/plain; charset=ISO-8859-1"))
	       (or (mail-position-on-field "Content-Transfer-Encoding")
		   (insert "8bit")))
	      (t (or (mail-position-on-field "Content-Type")
		     (insert "text/plain; charset=US-ASCII"))
		 (or (mail-position-on-field "Content-Transfer-Encoding")
		     (insert "7bit")))))))


;;;
;;; Gnus Mail Functions
;;;

;;; Mail reply commands of Gnus summary mode

(defun gnus-summary-reply (&optional yank wide)
  "Start composing a reply mail to the current message.
If prefix argument YANK is non-nil, the original article is yanked
automatically."
  (interactive
   (list (and current-prefix-arg
	      (gnus-summary-work-articles 1))))
  ;; Stripping headers should be specified with mail-yank-ignored-headers.
  (gnus-set-global-variables)
  (when yank
    (gnus-summary-goto-subject (car yank)))
  (let ((gnus-article-reply t))
    (gnus-setup-message (if yank 'reply-yank 'reply)
      (gnus-summary-select-article)
      (set-buffer (gnus-copy-article-buffer))
      (message-reply nil wide (gnus-group-find-parameter
			       gnus-newsgroup-name 'broken-reply-to))
      (when yank
	(gnus-inews-yank-articles yank)))))

(defun gnus-summary-reply-with-original (n &optional wide)
  "Start composing a reply mail to the current message.
The original article will be yanked."
  (interactive "P")
  (gnus-summary-reply (gnus-summary-work-articles n) wide))

(defun gnus-summary-wide-reply (&optional yank)
  "Start composing a wide reply mail to the current message.
If prefix argument YANK is non-nil, the original article is yanked
automatically."
  (interactive
   (list (and current-prefix-arg
	      (gnus-summary-work-articles 1))))
  (gnus-summary-reply yank t))

(defun gnus-summary-wide-reply-with-original (n)
  "Start composing a wide reply mail to the current message.
The original article will be yanked."
  (interactive "P")
  (gnus-summary-reply-with-original n t))

(defun gnus-summary-mail-forward (&optional full-headers post)
  "Forward the current message to another user.
If FULL-HEADERS (the prefix), include full headers when forwarding."
  (interactive "P")
  (gnus-set-global-variables)
  (gnus-setup-message 'forward
    (gnus-summary-select-article)
    (set-buffer gnus-original-article-buffer)
    (let ((message-included-forward-headers
	   (if full-headers "" message-included-forward-headers)))
      (message-forward post))))

(defun gnus-summary-resend-message (address n)
  "Resend the current article to ADDRESS."
  (interactive "sResend message(s) to: \nP")
  (let ((articles (gnus-summary-work-articles n))
	article)
    (while (setq article (pop articles))
      (gnus-summary-select-article nil nil nil article)
      (save-excursion
	(set-buffer gnus-original-article-buffer)
	(message-resend address)))))

(defun gnus-summary-post-forward (&optional full-headers)
  "Forward the current article to a newsgroup.
If FULL-HEADERS (the prefix), include full headers when forwarding."
  (interactive "P")
  (gnus-summary-mail-forward full-headers t))

(defvar gnus-nastygram-message
  "The following article was inappropriately posted to %s.\n\n"
  "Format string to insert in nastygrams.
The current group name will be inserted at \"%s\".")

(defun gnus-summary-mail-nastygram (n)
  "Send a nastygram to the author of the current article."
  (interactive "P")
  (when (or gnus-expert-user
	    (gnus-y-or-n-p
	     "Really send a nastygram to the author of the current article? "))
    (let ((group gnus-newsgroup-name))
      (gnus-summary-reply-with-original n)
      (set-buffer gnus-message-buffer)
      (message-goto-body)
      (insert (format gnus-nastygram-message group))
      (message-send-and-exit))))

(defun gnus-summary-mail-crosspost-complaint (n)
  "Send a complaint about crossposting to the current article(s)."
  (interactive "P")
  (let ((articles (gnus-summary-work-articles n))
	article)
    (while (setq article (pop articles))
      (set-buffer gnus-summary-buffer)
      (gnus-summary-goto-subject article)
      (let ((group (gnus-group-real-name gnus-newsgroup-name))
	    newsgroups followup-to)
	(gnus-summary-select-article)
	(set-buffer gnus-original-article-buffer)
	(if (and (<= (length (message-tokenize-header
			      (setq newsgroups (mail-fetch-field "newsgroups"))
			      ", "))
		     1)
		 (or (not (setq followup-to (mail-fetch-field "followup-to")))
		     (not (member group (message-tokenize-header
					 followup-to ", ")))))
	    (if followup-to
		(gnus-message 1 "Followup-to restricted")
	      (gnus-message 1 "Not a crossposted article"))
	  (set-buffer gnus-summary-buffer)
	  (gnus-summary-reply-with-original 1)
	  (set-buffer gnus-message-buffer)
	  (message-goto-body)
	  (insert (format gnus-crosspost-complaint newsgroups group))
	  (message-goto-subject)
	  (re-search-forward " *$")
	  (replace-match " (crosspost notification)" t t)
	  (when (fboundp 'deactivate-mark)
	    (deactivate-mark))
	  (when (gnus-y-or-n-p "Send this complaint? ")
	    (message-send-and-exit)))))))

(defun gnus-summary-mail-other-window ()
  "Compose mail in other window."
  (interactive)
  (gnus-setup-message 'message
    (message-mail)))

(defun gnus-mail-parse-comma-list ()
  (let (accumulated
	beg)
    (skip-chars-forward " ")
    (while (not (eobp))
      (setq beg (point))
      (skip-chars-forward "^,")
      (while (zerop
	      (save-excursion
		(save-restriction
		  (let ((i 0))
		    (narrow-to-region beg (point))
		    (goto-char beg)
		    (logand (progn
			      (while (search-forward "\"" nil t)
				(incf i))
			      (if (zerop i) 2 i))
			    2)))))
	(skip-chars-forward ",")
	(skip-chars-forward "^,"))
      (skip-chars-backward " ")
      (push (buffer-substring beg (point))
	    accumulated)
      (skip-chars-forward "^,")
      (skip-chars-forward ", "))
    accumulated))

(defun gnus-inews-add-to-address (group)
  (let ((to-address (mail-fetch-field "to")))
    (when (and to-address
	       (gnus-alive-p))
      ;; This mail group doesn't have a `to-list', so we add one
      ;; here.  Magic!
      (when (gnus-y-or-n-p
	     (format "Do you want to add this as `to-list': %s " to-address))
	(gnus-group-add-parameter group (cons 'to-list to-address))))))

(defun gnus-put-message ()
  "Put the current message in some group and return to Gnus."
  (interactive)
  (let ((reply gnus-article-reply)
	(winconf gnus-prev-winconf)
	(group gnus-newsgroup-name))

    (or (and group (not (gnus-group-read-only-p group)))
	(setq group (read-string "Put in group: " nil
				 (gnus-writable-groups))))
    (when (gnus-gethash group gnus-newsrc-hashtb)
      (error "No such group: %s" group))

    (save-excursion
      (save-restriction
	(widen)
	(message-narrow-to-headers)
	(let (gnus-deletable-headers)
	  (if (message-news-p)
	      (message-generate-headers message-required-news-headers)
	    (message-generate-headers message-required-mail-headers)))
	(goto-char (point-max))
	(insert "Gcc: " group "\n")
	(widen)))

    (gnus-inews-do-gcc)

    (when (get-buffer gnus-group-buffer)
      (when (gnus-buffer-exists-p (car-safe reply))
	(set-buffer (car reply))
	(and (cdr reply)
	     (gnus-summary-mark-article-as-replied
	      (cdr reply))))
      (when winconf
	(set-window-configuration winconf)))))

(defun gnus-article-mail (yank)
  "Send a reply to the address near point.
If YANK is non-nil, include the original article."
  (interactive "P")
  (let ((address
	 (buffer-substring
	  (save-excursion (re-search-backward "[ \t\n]" nil t) (1+ (point)))
	  (save-excursion (re-search-forward "[ \t\n]" nil t) (1- (point))))))
    (when address
      (message-reply address)
      (when yank
	(gnus-inews-yank-articles (list (cdr gnus-article-current)))))))

(defvar nntp-server-type)
(defun gnus-bug ()
  "Send a bug report to the Gnus maintainers."
  (interactive)
  (unless (gnus-alive-p)
    (error "Gnus has been shut down"))
  (gnus-setup-message 'bug
    (delete-other-windows)
    (switch-to-buffer "*Gnus Help Bug*")
    (erase-buffer)
    (insert gnus-bug-message)
    (goto-char (point-min))
    (message-pop-to-buffer "*Gnus Bug*")
    (message-setup `((To . ,gnus-maintainer) (Subject . "")))
    (push `(gnus-bug-kill-buffer) message-send-actions)
    (goto-char (point-min))
    (re-search-forward (concat "^" (regexp-quote mail-header-separator) "$"))
    (forward-line 1)
    (insert (gnus-version) "\n")
    (insert (emacs-version) "\n")
    (when (and (boundp 'nntp-server-type)
	       (stringp nntp-server-type))
      (insert nntp-server-type))
    (insert "\n\n\n\n\n")
    (gnus-debug)
    (goto-char (point-min))
    (search-forward "Subject: " nil t)
    (message "")))

(defun gnus-bug-kill-buffer ()
  (when (get-buffer "*Gnus Help Bug*")
    (kill-buffer "*Gnus Help Bug*")))

(defun gnus-debug ()
  "Attempts to go through the Gnus source file and report what variables have been changed.
The source file has to be in the Emacs load path."
  (interactive)
  (let ((files '("gnus.el" "gnus-sum.el" "gnus-group.el"
		 "gnus-art.el" "gnus-start.el" "gnus-async.el"
		 "gnus-msg.el" "gnus-score.el" "gnus-win.el" "gnus-topic.el"
		 "nnmail.el" "message.el"))
	file expr olist sym)
    (gnus-message 4 "Please wait while we snoop your variables...")
    (sit-for 0)
    ;; Go through all the files looking for non-default values for variables.
    (save-excursion
      (set-buffer (get-buffer-create " *gnus bug info*"))
      (buffer-disable-undo (current-buffer))
      (while files
	(erase-buffer)
	(when (and (setq file (locate-library (pop files)))
		   (file-exists-p file))
	  (insert-file-contents file)
	  (goto-char (point-min))
	  (if (not (re-search-forward "^;;* *Internal variables" nil t))
	      (gnus-message 4 "Malformed sources in file %s" file)
	    (narrow-to-region (point-min) (point))
	    (goto-char (point-min))
	    (while (setq expr (ignore-errors (read (current-buffer))))
	      (ignore-errors
		(and (or (eq (car expr) 'defvar)
			 (eq (car expr) 'defcustom))
		     (stringp (nth 3 expr))
		     (or (not (boundp (nth 1 expr)))
			 (not (equal (eval (nth 2 expr))
				     (symbol-value (nth 1 expr)))))
		     (push (nth 1 expr) olist)))))))
      (kill-buffer (current-buffer)))
    (when (setq olist (nreverse olist))
      (insert "------------------ Environment follows ------------------\n\n"))
    (while olist
      (if (boundp (car olist))
	  (condition-case ()
	      (pp `(setq ,(car olist)
			 ,(if (or (consp (setq sym (symbol-value (car olist))))
				  (and (symbolp sym)
				       (not (or (eq sym nil)
						(eq sym t)))))
			      (list 'quote (symbol-value (car olist)))
			    (symbol-value (car olist))))
		  (current-buffer))
	    (error
	     (format "(setq %s 'whatever)\n" (car olist))))
	(insert ";; (makeunbound '" (symbol-name (car olist)) ")\n"))
      (setq olist (cdr olist)))
    (insert "\n\n")
    ;; Remove any null chars - they seem to cause trouble for some
    ;; mailers.  (Byte-compiled output from the stuff above.)
    (goto-char (point-min))
    (while (re-search-forward "[\000\200]" nil t)
      (replace-match "" t t))))

;;; Treatment of rejected articles.
;;; Bounced mail.

(defun gnus-summary-resend-bounced-mail (&optional fetch)
  "Re-mail the current message.
This only makes sense if the current message is a bounce message than
contains some mail you have written which has been bounced back to
you.
If FETCH, try to fetch the article that this is a reply to, if indeed
this is a reply."
  (interactive "P")
  (gnus-summary-select-article t)
  (set-buffer gnus-original-article-buffer)
  (gnus-setup-message 'compose-bounce
    (let* ((references (mail-fetch-field "references"))
	   (parent (and references (gnus-parent-id references))))
      (message-bounce)
      ;; If there are references, we fetch the article we answered to.
      (and fetch parent
	   (gnus-summary-refer-article parent)
	   (gnus-summary-show-all-headers)))))

;;; Gcc handling.

;; Do Gcc handling, which copied the message over to some group.
(defun gnus-inews-do-gcc (&optional gcc)
  (interactive)
  (when (gnus-alive-p)
    (save-excursion
      (save-restriction
	(message-narrow-to-headers)
	(let ((gcc (or gcc (mail-fetch-field "gcc" nil t)))
	      (cur (current-buffer))
	      groups group method)
	  (when gcc
	    (message-remove-header "gcc")
	    (widen)
	    (setq groups (message-tokenize-header gcc " ,"))
	    ;; Copy the article over to some group(s).
	    (while (setq group (pop groups))
	      (gnus-check-server
	       (setq method
		     (cond ((and (null (gnus-get-info group))
				 (eq (car gnus-message-archive-method)
				     (car
				      (gnus-server-to-method
				       (gnus-group-method group)))))
			    ;; If the group doesn't exist, we assume
			    ;; it's an archive group...
			    gnus-message-archive-method)
			   ;; Use the method.
			   ((gnus-info-method (gnus-get-info group))
			    (gnus-info-method (gnus-get-info group)))
			   ;; Find the method.
			   (t (gnus-group-method group)))))
	      (gnus-check-server method)
	      (unless (gnus-request-group group t method)
		(gnus-request-create-group group method))
	      (save-excursion
		(nnheader-set-temp-buffer " *acc*")
		(insert-buffer-substring cur)
		(goto-char (point-min))
		(when (re-search-forward
		       (concat "^" (regexp-quote mail-header-separator) "$")
		       nil t)
		  (replace-match "" t t ))
		(unless (gnus-request-accept-article group method t)
		  (gnus-message 1 "Couldn't store article in group %s: %s"
				group (gnus-status-message method))
		  (sit-for 2))
		(kill-buffer (current-buffer))))))))))

(defun gnus-inews-insert-gcc ()
  "Insert Gcc headers based on `gnus-outgoing-message-group'."
  (save-excursion
    (save-restriction
      (message-narrow-to-headers)
      (let* ((group gnus-outgoing-message-group)
	     (gcc (cond
		   ((gnus-functionp group)
		    (funcall group))
		   ((or (stringp group) (list group))
		    group))))
	(when gcc
	  (insert "Gcc: "
		  (if (stringp gcc) gcc
		    (mapconcat 'identity gcc " "))
		  "\n"))))))

(defun gnus-inews-insert-archive-gcc (&optional group)
  "Insert the Gcc to say where the article is to be archived."
  (let* ((var gnus-message-archive-group)
	 (group (or group gnus-newsgroup-name ""))
	 result
	 gcc-self-val
	 (groups
	  (cond
	   ((null gnus-message-archive-method)
	    ;; Ignore.
	    nil)
	   ((stringp var)
	    ;; Just a single group.
	    (list var))
	   ((null var)
	    ;; We don't want this.
	    nil)
	   ((and (listp var) (stringp (car var)))
	    ;; A list of groups.
	    var)
	   ((gnus-functionp var)
	    ;; A function.
	    (funcall var group))
	   (t
	    ;; An alist of regexps/functions/forms.
	    (while (and var
			(not
			 (setq result
			       (cond
				((stringp (caar var))
				 ;; Regexp.
				 (when (string-match (caar var) group)
				   (cdar var)))
				((gnus-functionp (car var))
				 ;; Function.
				 (funcall (car var) group))
				(t
				 (eval (car var)))))))
	      (setq var (cdr var)))
	    result)))
	 name)
    (when groups
      (when (stringp groups)
	(setq groups (list groups)))
      (save-excursion
	(save-restriction
	  (message-narrow-to-headers)
	  (goto-char (point-max))
	  (insert "Gcc: ")
	  (if (and gnus-newsgroup-name
		   (setq gcc-self-val
			 (gnus-group-find-parameter
			  gnus-newsgroup-name 'gcc-self)))
	      (progn
		(insert
		 (if (stringp gcc-self-val)
		     gcc-self-val
		   group))
		(if (not (eq gcc-self-val 'none))
		    (insert "\n")
		  (progn
		    (beginning-of-line)
		    (kill-line))))
	    (while (setq name (pop groups))
	      (insert (if (string-match ":" name)
			  name
			(gnus-group-prefixed-name
			 name gnus-message-archive-method)))
	      (when groups
		(insert " ")))
	    (insert "\n")))))))

(defun gnus-summary-send-draft ()
  "Enter a mail/post buffer to edit and send the draft."
  (interactive)
  (gnus-set-global-variables)
  (let (buf)
    (if (not (setq buf (gnus-request-restore-buffer
			(gnus-summary-article-number) gnus-newsgroup-name)))
	(error "Couldn't restore the article")
      (switch-to-buffer buf)
      (when (eq major-mode 'news-reply-mode)
	(local-set-key "\C-c\C-c" 'gnus-inews-news))
      ;; Insert the separator.
      (goto-char (point-min))
      (search-forward "\n\n")
      (forward-char -1)
      (insert mail-header-separator)
      ;; Configure windows.
      (let ((gnus-draft-buffer (current-buffer)))
	(gnus-configure-windows 'draft t)
	(goto-char (point))))))

(gnus-add-shutdown 'gnus-inews-close 'gnus)

(defun gnus-inews-close ()
  (setq gnus-inews-sent-ids nil))

;;; Allow redefinition of functions.

(gnus-ems-redefine)

(provide 'gnus-msg)

;;; gnus-msg.el ends here
