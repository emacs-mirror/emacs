;;; gnus-msg.el --- mail and post interface for Gnus
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000
;;        Free Software Foundation, Inc.

;; Author: Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;;	Lars Magne Ingebrigtsen <larsi@gnus.org>
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

(defcustom gnus-post-method 'current
  "*Preferred method for posting USENET news.

If this variable is `current', Gnus will use the \"current\" select
method when posting.  If it is nil (which is the default), Gnus will
use the native select method when posting.

This method will not be used in mail groups and the like, only in
\"real\" newsgroups.

If not nil nor `native', the value must be a valid method as discussed
in the documentation of `gnus-select-method'.  It can also be a list of
methods.  If that is the case, the user will be queried for what select
method to use when posting."
  :group 'gnus-group-foreign
  :type `(choice (const nil)
                 (const current)
		 (const native)
		 (sexp :tag "Methods" ,gnus-select-method)))

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

(defvar gnus-bug-create-help-buffer t
  "*Should we create the *Gnus Help Bug* buffer?")

(defvar gnus-posting-styles nil
  "*Alist of styles to use when posting.")

(defcustom gnus-group-posting-charset-alist
  '(("^\\(no\\|fr\\|dk\\)\\.[^,]*\\(,[ \t\n]*\\(no\\|fr\\|dk\\)\\.[^,]*\\)*$" iso-8859-1 (iso-8859-1))
    ("^\\(fido7\\|relcom\\)\\.[^,]*\\(,[ \t\n]*\\(fido7\\|relcom\\)\\.[^,]*\\)*$" koi8-r (koi8-r))
    (message-this-is-mail nil nil)
    (message-this-is-news nil t))
  "Alist of regexps and permitted unencoded charsets for posting.
Each element of the alist has the form (TEST HEADER BODY-LIST), where
TEST is either a regular expression matching the newsgroup header or a
variable to query,
HEADER is the charset which may be left unencoded in the header (nil
means encode all charsets),
BODY-LIST is a list of charsets which may be encoded using 8bit
content-transfer encoding in the body, or one of the special values
nil (always encode using quoted-printable) or t (always use 8bit).

Note that any value other than nil for HEADER infringes some RFCs, so
use this option with care."
  :type '(repeat (list :tag "Permitted unencoded charsets"
		  (choice :tag "Where"
		   (regexp :tag "Group")
		   (const :tag "Mail message" :value message-this-is-mail)
		   (const :tag "News article" :value message-this-is-news))
		  (choice :tag "Header"
		   (const :tag "None" nil)
		   (symbol :tag "Charset"))
		  (choice :tag "Body"
			  (const :tag "Any" :value t)
			  (const :tag "None" :value nil)
			  (repeat :tag "Charsets"
				  (symbol :tag "Charset")))))
  :group 'gnus-charset)

;;; Internal variables.

(defvar gnus-inhibit-posting-styles nil
  "Inhibit the use of posting styles.")

(defvar gnus-message-buffer "*Mail Gnus*")
(defvar gnus-article-copy nil)
(defvar gnus-last-posting-server nil)
(defvar gnus-message-group-art nil)

(defconst gnus-bug-message
  "Sending a bug report to the Gnus Towers.
========================================

The buffer below is a mail buffer.  When you press `C-c C-c', it will
be sent to the Gnus Bug Exterminators.

The thing near the bottom of the buffer is how the environment
settings will be included in the mail.  Please do not delete that.
They will tell the Bug People what your environment is, so that it
will be easier to locate the bugs.

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
  "y" gnus-summary-yank-message
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

;;;###autoload
(defun gnus-msg-mail (&rest args)
  "Start editing a mail message to be sent.
Like `message-mail', but with Gnus paraphernalia, particularly the
the Gcc: header for archiving purposes."
  (interactive)
  (gnus-setup-message 'message
    (apply 'message-mail args)))

;;;###autoload
(define-mail-user-agent 'gnus-user-agent
      'gnus-msg-mail 'message-send-and-exit
      'message-kill-buffer 'message-send-hook)

;;; Internal functions.

(defvar gnus-article-reply nil)
(defmacro gnus-setup-message (config &rest forms)
  (let ((winconf (make-symbol "gnus-setup-message-winconf"))
	(buffer (make-symbol "gnus-setup-message-buffer"))
	(article (make-symbol "gnus-setup-message-article"))
	(group (make-symbol "gnus-setup-message-group")))
    `(let ((,winconf (current-window-configuration))
	   (,buffer (buffer-name (current-buffer)))
	   (,article (and gnus-article-reply (gnus-summary-article-number)))
	   (,group gnus-newsgroup-name)
	   (message-header-setup-hook
	    (copy-sequence message-header-setup-hook))
	   (mbl mml-buffer-list)
	   (message-mode-hook (copy-sequence message-mode-hook)))
       (setq mml-buffer-list nil)
       (add-hook 'message-header-setup-hook 'gnus-inews-insert-gcc)
       (add-hook 'message-header-setup-hook 'gnus-inews-insert-archive-gcc)
       (add-hook 'message-mode-hook 'gnus-configure-posting-styles)
       (unwind-protect
	   (progn
	     ,@forms)
	 (gnus-inews-add-send-actions ,winconf ,buffer ,article)
	 (setq gnus-message-buffer (current-buffer))
	 (set (make-local-variable 'gnus-message-group-art)
	      (cons ,group ,article))
	 (set (make-local-variable 'gnus-newsgroup-name) ,group)
	 (gnus-run-hooks 'gnus-message-setup-hook)
	 (if (eq major-mode 'message-mode)
	     ;; Make mml-buffer-list local.
	     ;; Restore global mml-buffer-list value as mbl.
	     ;; What a hack! -- Shenghuo
	     (let ((mml-buffer-list mml-buffer-list))
	       (setq mml-buffer-list mbl)
	       (make-local-variable 'mml-buffer-list)
	       (add-hook 'kill-buffer-hook 'mml-destroy-buffers t t))
	   (mml-destroy-buffers)
	   (setq mml-buffer-list mbl)))
       (gnus-add-buffer)
       (gnus-configure-windows ,config t)
       (set-buffer-modified-p nil))))

(defun gnus-setup-posting-charset (group)
  (let ((alist gnus-group-posting-charset-alist)
	(group (or group ""))
	elem)
    (when group
      (catch 'found
	(while (setq elem (pop alist))
	  (when (or (and (stringp (car elem))
			 (string-match (car elem) group))
		    (and (gnus-functionp (car elem))
			 (funcall (car elem) group))
		    (and (symbolp (car elem))
			 (symbol-value (car elem))))
	    (throw 'found (cons (cadr elem) (caddr elem)))))))))

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
   `(when (gnus-buffer-exists-p ,buffer)
      (save-excursion
	(set-buffer ,buffer)
	,(when article
	   `(gnus-summary-mark-article-as-replied ,article))))
   'send))

(put 'gnus-setup-message 'lisp-indent-function 1)
(put 'gnus-setup-message 'edebug-form-spec '(form body))

;;; Post news commands of Gnus group mode and summary mode

(defun gnus-group-mail (&optional arg)
  "Start composing a mail.
If ARG, use the group under the point to find a posting style.
If ARG is 1, prompt for a group name to find the posting style."
  (interactive "P")
  ;; We can't `let' gnus-newsgroup-name here, since that leads
  ;; to local variables leaking.
  (let ((group gnus-newsgroup-name)
	(buffer (current-buffer)))
    (unwind-protect
	(progn
	  (setq gnus-newsgroup-name
		(if arg
		    (if (= 1 (prefix-numeric-value arg))
			(completing-read "Use posting style of group: "
					 gnus-active-hashtb nil
					 (gnus-read-active-file-p))
		      (gnus-group-group-name))
		  ""))
	  (gnus-setup-message 'message (message-mail)))
      (save-excursion
	(set-buffer buffer)
	(setq gnus-newsgroup-name group)))))

(defun gnus-group-post-news (&optional arg)
  "Start composing a news message.
If ARG, post to the group under point.
If ARG is 1, prompt for a group name."
  (interactive "P")
  ;; Bind this variable here to make message mode hooks work ok.
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
  (gnus-post-news 'post gnus-newsgroup-name))

(defun gnus-summary-followup (yank &optional force-news)
  "Compose a followup to an article.
If prefix argument YANK is non-nil, original article is yanked automatically."
  (interactive
   (list (and current-prefix-arg
	      (gnus-summary-work-articles 1))))
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

(defun gnus-summary-cancel-article (&optional n symp)
  "Cancel an article you posted.
Uses the process-prefix convention.  If given the symbolic
prefix `a', cancel using the standard posting method; if not
post using the current select method."
  (interactive (gnus-interactive "P\ny"))
  (let ((articles (gnus-summary-work-articles n))
	(message-post-method
	 `(lambda (arg)
	    (gnus-post-method (not (eq symp 'a)) ,gnus-newsgroup-name)))
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
  (let ((article (gnus-summary-article-number)))
    (gnus-setup-message 'reply-yank
      (gnus-summary-select-article t)
      (set-buffer gnus-original-article-buffer)
      (message-supersede)
      (push
       `((lambda ()
	   (when (gnus-buffer-exists-p ,gnus-summary-buffer)
	     (save-excursion
	       (set-buffer ,gnus-summary-buffer)
	       (gnus-cache-possibly-remove-article ,article nil nil nil t)
	       (gnus-summary-mark-as-read ,article gnus-canceled-mark)))))
       message-send-actions))))



(defun gnus-copy-article-buffer (&optional article-buffer)
  ;; make a copy of the article buffer with all text properties removed
  ;; this copy is in the buffer gnus-article-copy.
  ;; if ARTICLE-BUFFER is nil, gnus-article-buffer is used
  ;; this buffer should be passed to all mail/news reply/post routines.
  (setq gnus-article-copy (gnus-get-buffer-create " *gnus article copy*"))
  (save-excursion
    (set-buffer gnus-article-copy)
    (mm-enable-multibyte))
  (let ((article-buffer (or article-buffer gnus-article-buffer))
	end beg)
    (if (not (and (get-buffer article-buffer)
		  (gnus-buffer-exists-p article-buffer)))
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
	       (buffer-substring-no-properties (point-min) (point-max))
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
			 (or (search-forward "\n\n" nil t) (point-max)))
	  ;; Insert the original article headers.
	  (insert-buffer-substring gnus-original-article-buffer beg end)
	  (article-decode-encoded-words)))
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
	     (charset (gnus-group-name-charset nil group))
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
		group (gnus-group-name-decode (gnus-group-real-name group)
					      charset)))
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
	      (gnus-msg-treat-broken-reply-to)
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
	    (gnus-msg-treat-broken-reply-to)
	    (message-wide-reply to-address)))
	(when yank
	  (gnus-inews-yank-articles yank))))))

(defun gnus-msg-treat-broken-reply-to ()
  "Remove the Reply-to header iff broken-reply-to."
  (when (gnus-group-find-parameter
	 gnus-newsgroup-name 'broken-reply-to)
    (save-restriction
      (message-narrow-to-head)
      (message-remove-header "reply-to"))))

(defun gnus-post-method (arg group &optional silent)
  "Return the posting method based on GROUP and ARG.
If SILENT, don't prompt the user."
  (let ((group-method (gnus-find-method-for-group group)))
    (cond
     ;; If the group-method is nil (which shouldn't happen) we use
     ;; the default method.
     ((null group-method)
      (or (and (null (eq gnus-post-method 'active)) gnus-post-method)
	  gnus-select-method message-post-method))
     ;; We want the inverse of the default
     ((and arg (not (eq arg 0)))
      (if (eq gnus-post-method 'active)
	  gnus-select-method
	group-method))
     ;; We query the user for a post method.
     ((or arg
	  (and gnus-post-method
	       (not (eq gnus-post-method 'current))
	       (listp (car gnus-post-method))))
      (let* ((methods
	      ;; Collect all methods we know about.
	      (append
	       (when (and gnus-post-method
			  (not (eq gnus-post-method 'current)))
		 (if (listp (car gnus-post-method))
		     gnus-post-method
		   (list gnus-post-method)))
	       gnus-secondary-select-methods
	       (mapcar 'cdr gnus-server-alist)
	       (mapcar 'car gnus-opened-servers)
	       (list gnus-select-method)
	       (list group-method)))
	     method-alist post-methods method)
	;; Weed out all mail methods.
	(while methods
	  (setq method (gnus-server-get-method "" (pop methods)))
	  (when (and (or (gnus-method-option-p method 'post)
			 (gnus-method-option-p method 'post-mail))
		     (not (member method post-methods)))
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
     ((and (eq gnus-post-method 'current)
	   (not (eq (car group-method) 'nndraft))
	   (gnus-get-function group-method 'request-post t)
	   (not arg))
      group-method)
     ((and gnus-post-method
	   (not (eq gnus-post-method 'current)))
      gnus-post-method)
     ;; Use the normal select method.
     (t gnus-select-method))))



;; Dummies to avoid byte-compile warning.
(defvar nnspool-rejected-article-hook)
(defvar xemacs-codename)

(defun gnus-extended-version ()
  "Stringified Gnus version and Emacs version."
  (interactive)
  (concat
   "Gnus/" (prin1-to-string (gnus-continuum-version gnus-version) t)
   " (" gnus-version ")"
   " "
   (cond
    ((string-match "^\\(\\([.0-9]+\\)*\\)\\.[0-9]+$" emacs-version)
     (concat "Emacs/" (match-string 1 emacs-version)))
    ((string-match "\\([A-Z]*[Mm][Aa][Cc][Ss]\\)[^(]*\\(\\((beta.*)\\|'\\)\\)?"
		   emacs-version)
     (concat (match-string 1 emacs-version)
	     (format "/%d.%d" emacs-major-version emacs-minor-version)
	     (if (match-beginning 3)
		 (match-string 3 emacs-version)
	       "")
	     (if (boundp 'xemacs-codename)
		 (concat " (" xemacs-codename ")")
	       "")))
    (t emacs-version))))


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
  (when yank
    (gnus-summary-goto-subject (car yank)))
  (let ((gnus-article-reply t))
    (gnus-setup-message (if yank 'reply-yank 'reply)
      (gnus-summary-select-article)
      (set-buffer (gnus-copy-article-buffer))
      (gnus-msg-treat-broken-reply-to)
      (save-restriction
	(message-narrow-to-head)
	(goto-char (point-max)))
      (mml-quote-region (point) (point-max))
      (message-reply nil wide)
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

(defun gnus-summary-mail-forward (&optional arg post)
  "Forward the current message to another user.  
If ARG is nil, see `message-forward-as-mime' and `message-forward-show-mml';
if ARG is 1, decode the message and forward directly inline;
if ARG is 2, foward message as an rfc822 MIME section;
if ARG is 3, decode message and forward as an rfc822 MIME section;
if ARG is 4, foward message directly inline;
otherwise, use flipped `message-forward-as-mime'.
If POST, post instead of mail."
  (interactive "P")
  (let ((message-forward-as-mime message-forward-as-mime)
	(message-forward-show-mml message-forward-show-mml))
    (cond 
     ((null arg))
     ((eq arg 1) (setq message-forward-as-mime nil
		       message-forward-show-mml t))
     ((eq arg 2) (setq message-forward-as-mime t
		       message-forward-show-mml nil))
     ((eq arg 3) (setq message-forward-as-mime t
		       message-forward-show-mml t))
     ((eq arg 4) (setq message-forward-as-mime nil
		       message-forward-show-mml nil))
     (t (setq message-forward-as-mime (not message-forward-as-mime))))
    (gnus-setup-message 'forward
      (gnus-summary-select-article)
      (let ((mail-parse-charset gnus-newsgroup-charset)
	    (mail-parse-ignored-charsets gnus-newsgroup-ignored-charsets)
	    text)
	(save-excursion
	  (set-buffer gnus-original-article-buffer)
	  (mm-with-unibyte-current-buffer
	    (setq text (buffer-string))))
	(set-buffer 
	 (gnus-get-buffer-create
	  (generate-new-buffer-name " *Gnus forward*")))
	(erase-buffer)
	(mm-disable-multibyte)
	(insert text)
	(goto-char (point-min))
	(when (looking-at "From ")
	  (replace-match "X-From-Line: ") )
	(when message-forward-show-mml
	  (mm-enable-multibyte)
	  (mime-to-mml))
	(message-forward post)))))

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

(defun gnus-summary-post-forward (&optional arg)
  "Forward the current article to a newsgroup.
See `gnus-summary-mail-forward' for ARG."
  (interactive "P")
  (gnus-summary-mail-forward arg t))

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
			      (setq newsgroups
				    (mail-fetch-field "newsgroups"))
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
	  (gnus-deactivate-mark)
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
    (when gnus-bug-create-help-buffer
      (switch-to-buffer "*Gnus Help Bug*")
      (erase-buffer)
      (insert gnus-bug-message)
      (goto-char (point-min)))
    (message-pop-to-buffer "*Gnus Bug*")
    (message-setup `((To . ,gnus-maintainer) (Subject . "")))
    (when gnus-bug-create-help-buffer
      (push `(gnus-bug-kill-buffer) message-send-actions))
    (goto-char (point-min))
    (re-search-forward (concat "^" (regexp-quote mail-header-separator) "$"))
    (forward-line 1)
    (insert (gnus-version) "\n"
	    (emacs-version) "\n")
    (when (and (boundp 'nntp-server-type)
	       (stringp nntp-server-type))
      (insert nntp-server-type))
    (insert "\n\n\n\n\n")
    (let (text)
      (save-excursion
	(set-buffer (gnus-get-buffer-create " *gnus environment info*"))
	(gnus-debug)
	(setq text (buffer-string)))
      (insert "<#part type=application/x-emacs-lisp disposition=inline description=\"User settings\">\n" text "\n<#/part>"))
    (goto-char (point-min))
    (search-forward "Subject: " nil t)
    (message "")))

(defun gnus-bug-kill-buffer ()
  (when (get-buffer "*Gnus Help Bug*")
    (kill-buffer "*Gnus Help Bug*")))

(defun gnus-summary-yank-message (buffer n)
  "Yank the current article into a composed message."
  (interactive
   (list (completing-read "Buffer: " (mapcar 'list (message-buffers)) nil t)
	 current-prefix-arg))
  (gnus-summary-iterate n
    (let ((gnus-display-mime-function nil)
	  (gnus-inhibit-treatment t))
      (gnus-summary-select-article))
    (save-excursion
      (set-buffer buffer)
      (message-yank-buffer gnus-article-buffer))))

(defun gnus-debug ()
  "Attempts to go through the Gnus source file and report what variables have been changed.
The source file has to be in the Emacs load path."
  (interactive)
  (let ((files '("gnus.el" "gnus-sum.el" "gnus-group.el"
		 "gnus-art.el" "gnus-start.el" "gnus-async.el"
		 "gnus-msg.el" "gnus-score.el" "gnus-win.el" "gnus-topic.el"
		 "nnmail.el" "message.el"))
	(point (point))
	file expr olist sym)
    (gnus-message 4 "Please wait while we snoop your variables...")
    (sit-for 0)
    ;; Go through all the files looking for non-default values for variables.
    (save-excursion
      (set-buffer (gnus-get-buffer-create " *gnus bug info*"))
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
    ;; Remove any control chars - they seem to cause trouble for some
    ;; mailers.  (Byte-compiled output from the stuff above.)
    (goto-char point)
    (while (re-search-forward "[\000-\010\013-\037\200-\237]" nil t)
      (replace-match (format "\\%03o" (string-to-char (match-string 0)))
		     t t))))

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
	    (setq groups (message-unquote-tokens
                          (message-tokenize-header gcc " ,")))
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
		(message-encode-message-body)
		(save-restriction
		  (message-narrow-to-headers)
		  (let ((mail-parse-charset message-default-charset)
			(rfc2047-header-encoding-alist
			 (cons '("Newsgroups" . default)
			       rfc2047-header-encoding-alist)))
		    (mail-encode-encoded-word-buffer)))
		(goto-char (point-min))
		(when (re-search-forward
		       (concat "^" (regexp-quote mail-header-separator) "$")
		       nil t)
		  (replace-match "" t t ))
		(unless (gnus-request-accept-article group method t t)
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
	 (gcc-self-val
	  (and gnus-newsgroup-name
	       (not (equal gnus-newsgroup-name ""))
	       (gnus-group-find-parameter
		gnus-newsgroup-name 'gcc-self)))
	 result
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
    (when (or groups gcc-self-val)
      (when (stringp groups)
	(setq groups (list groups)))
      (save-excursion
	(save-restriction
	  (message-narrow-to-headers)
	  (goto-char (point-max))
	  (insert "Gcc: ")
	  (if gcc-self-val
	      ;; Use the `gcc-self' param value instead.
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
	    ;; Use the list of groups.
	    (while (setq name (pop groups))
	      (insert (if (string-match ":" name)
			  name
			(gnus-group-prefixed-name
			 name gnus-message-archive-method)))
	      (when groups
		(insert " ")))
	    (insert "\n")))))))

;;; Posting styles.

(defun gnus-configure-posting-styles ()
  "Configure posting styles according to `gnus-posting-styles'."
  (unless gnus-inhibit-posting-styles
    (let ((group (or gnus-newsgroup-name ""))
	  (styles gnus-posting-styles)
	  style match variable attribute value v results
	  filep name address element)
      ;; If the group has a posting-style parameter, add it at the end with a
      ;; regexp matching everything, to be sure it takes precedence over all
      ;; the others.
      (when gnus-newsgroup-name
	(let ((tmp-style (gnus-group-find-parameter group 'posting-style t)))
	  (when tmp-style
	    (setq styles (append styles (list (cons ".*" tmp-style)))))))
      ;; Go through all styles and look for matches.
      (dolist (style styles)
	(setq match (pop style))
	(goto-char (point-min))
	(when (cond
	       ((stringp match)
		;; Regexp string match on the group name.
		(string-match match group))
	       ((eq match 'header)
		(let ((header (message-fetch-field (pop style))))
		  (and header
		       (string-match (pop style) header))))
	       ((or (symbolp match)
		    (gnus-functionp match))
		(cond
		 ((gnus-functionp match)
		  ;; Function to be called.
		  (funcall match))
		 ((boundp match)
		  ;; Variable to be checked.
		  (symbol-value match))))
	       ((listp match)
		;; This is a form to be evaled.
		(eval match)))
	  ;; We have a match, so we set the variables.
	  (dolist (attribute style)
	    (setq element (pop attribute)
		  variable nil
		  filep nil)
	    (setq value
		  (cond
		   ((eq (car attribute) :file)
		    (setq filep t)
		    (cadr attribute))
		   ((eq (car attribute) :value)
		    (cadr attribute))
		   (t
		    (car attribute))))
	    ;; We get the value.
	    (setq v
		  (cond
		   ((stringp value)
		    value)
		   ((or (symbolp value)
			(gnus-functionp value))
		    (cond ((gnus-functionp value)
			   (funcall value))
			  ((boundp value)
			   (symbol-value value))))
		   ((listp value)
		    (eval value))))
	    ;; Translate obsolescent value.
	    (when (eq element 'signature-file)
	      (setq element 'signature
		    filep t))
	    ;; Get the contents of file elems.
	    (when (and filep v)
	      (setq v (with-temp-buffer
			(insert-file-contents v)
			(buffer-string))))
	    (setq results (delq (assoc element results) results))
	    (push (cons element v) results))))
      ;; Now we have all the styles, so we insert them.
      (setq name (assq 'name results)
	    address (assq 'address results))
      (setq results (delq name (delq address results)))
      (make-local-variable 'message-setup-hook)
      (dolist (result results)
	(add-hook 'message-setup-hook
		  (cond
		   ((eq 'eval (car result))
		    'ignore)
		   ((eq 'body (car result))
		    `(lambda ()
		       (save-excursion
			 (message-goto-body)
			 (insert ,(cdr result)))))
		   ((eq 'signature (car result))
		    (set (make-local-variable 'message-signature) nil)
		    (set (make-local-variable 'message-signature-file) nil)
		    (if (not (cdr result))
			'ignore
		      `(lambda ()
			 (save-excursion
			   (let ((message-signature ,(cdr result)))
			     (when message-signature
			       (message-insert-signature)))))))
		   (t
		    (let ((header
			   (if (symbolp (car result))
			       (capitalize (symbol-name (car result)))
			     (car result))))
		      `(lambda ()
			 (save-excursion
			   (message-remove-header ,header)
			   (let ((value ,(cdr result)))
			     (when value
			       (message-goto-eoh)
			       (insert ,header ": " value "\n"))))))))))
      (when (or name address)
	(add-hook 'message-setup-hook
		  `(lambda ()
 		     (set (make-local-variable 'user-mail-address)
 			  ,(or (cdr address) user-mail-address))
		     (let ((user-full-name ,(or (cdr name) (user-full-name)))
			   (user-mail-address
			    ,(or (cdr address) user-mail-address)))
		       (save-excursion
			 (message-remove-header "From")
			 (message-goto-eoh)
			 (insert "From: " (message-make-from) "\n")))))))))

;;; Allow redefinition of functions.

(gnus-ems-redefine)

(provide 'gnus-msg)

;;; gnus-msg.el ends here
