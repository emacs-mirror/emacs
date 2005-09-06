;;; gnus-msg.el --- mail and post interface for Gnus

;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
;;   2004, 2005 Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))

(require 'gnus)
(require 'gnus-ems)
(require 'message)
(require 'gnus-art)
(require 'gnus-util)

(defcustom gnus-post-method 'current
  "*Preferred method for posting USENET news.

If this variable is `current' (which is the default), Gnus will use
the \"current\" select method when posting.  If it is `native', Gnus
will use the native select method when posting.

This method will not be used in mail groups and the like, only in
\"real\" newsgroups.

If not `native' nor `current', the value must be a valid method as discussed
in the documentation of `gnus-select-method'.  It can also be a list of
methods.  If that is the case, the user will be queried for what select
method to use when posting."
  :group 'gnus-group-foreign
  :link '(custom-manual "(gnus)Posting Server")
  :type `(choice (const native)
		 (const current)
		 (sexp :tag "Methods" ,gnus-select-method)))

(defcustom gnus-outgoing-message-group nil
  "*All outgoing messages will be put in this group.
If you want to store all your outgoing mail and articles in the group
\"nnml:archive\", you set this variable to that value.  This variable
can also be a list of group names.

If you want to have greater control over what group to put each
message in, you can set this variable to a function that checks the
current newsgroup name and then returns a suitable group name (or list
of names)."
  :group 'gnus-message
  :type '(choice (const nil)
		 (function)
		 (string :tag "Group")
		 (repeat :tag "List of groups" (string :tag "Group"))))

(defcustom gnus-mailing-list-groups nil
  "*If non-nil a regexp matching groups that are really mailing lists.
This is useful when you're reading a mailing list that has been
gatewayed to a newsgroup, and you want to followup to an article in
the group."
  :group 'gnus-message
  :type '(choice (regexp)
		 (const nil)))

(defcustom gnus-add-to-list nil
  "*If non-nil, add a `to-list' parameter automatically."
  :group 'gnus-message
  :type 'boolean)

(defcustom gnus-crosspost-complaint
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
the second with the current group name."
  :group 'gnus-message
  :type 'string)

(defcustom gnus-message-setup-hook nil
  "Hook run after setting up a message buffer."
  :group 'gnus-message
  :type 'hook)

(defcustom gnus-bug-create-help-buffer t
  "*Should we create the *Gnus Help Bug* buffer?"
  :group 'gnus-message
  :type 'boolean)

(defcustom gnus-posting-styles nil
  "*Alist of styles to use when posting.
See Info node `(gnus)Posting Styles'."
  :group 'gnus-message
  :link '(custom-manual "(gnus)Posting Styles")
  :type '(repeat (cons (choice (regexp)
			       (variable)
			       (list (const header)
				     (string :tag "Header")
				     (regexp :tag "Regexp"))
			       (function)
			       (sexp))
		       (repeat (list
				(choice (const signature)
					(const signature-file)
					(const organization)
					(const address)
					(const x-face-file)
					(const name)
					(const body)
					(symbol)
					(string :tag "Header"))
				(choice (string)
					(function)
					(variable)
					(sexp)))))))

(defcustom gnus-gcc-mark-as-read nil
  "If non-nil, automatically mark Gcc articles as read."
  :version "22.1"
  :group 'gnus-message
  :type 'boolean)

(make-obsolete-variable 'gnus-inews-mark-gcc-as-read
			'gnus-gcc-mark-as-read)

(defcustom gnus-gcc-externalize-attachments nil
  "Should local-file attachments be included as external parts in Gcc copies?
If it is `all', attach files as external parts;
if a regexp and matches the Gcc group name, attach files as external parts;
if nil, attach files as normal parts."
  :version "22.1"
  :group 'gnus-message
  :type '(choice (const nil :tag "None")
		 (const all :tag "Any")
		 (string :tag "Regexp")))

(gnus-define-group-parameter
 posting-charset-alist
 :type list
 :function-document
 "Return the permitted unencoded charsets for posting of GROUP."
 :variable gnus-group-posting-charset-alist
 :variable-default
  '(("^\\(no\\|fr\\)\\.[^,]*\\(,[ \t\n]*\\(no\\|fr\\)\\.[^,]*\\)*$" iso-8859-1 (iso-8859-1))
    ("^\\(fido7\\|relcom\\)\\.[^,]*\\(,[ \t\n]*\\(fido7\\|relcom\\)\\.[^,]*\\)*$" koi8-r (koi8-r))
    (message-this-is-mail nil nil)
    (message-this-is-news nil t))
 :variable-document
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
 :variable-group gnus-charset
 :variable-type
 '(repeat (list :tag "Permitted unencoded charsets"
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
 :parameter-type '(choice :tag "Permitted unencoded charsets"
			  :value nil
			  (repeat (symbol)))
 :parameter-document       "\
List of charsets that are permitted to be unencoded.")

(defcustom gnus-debug-files
  '("gnus.el" "gnus-sum.el" "gnus-group.el"
    "gnus-art.el" "gnus-start.el" "gnus-async.el"
    "gnus-msg.el" "gnus-score.el" "gnus-win.el" "gnus-topic.el"
    "gnus-agent.el" "gnus-cache.el" "gnus-srvr.el"
    "mm-util.el" "mm-decode.el" "nnmail.el" "message.el")
  "Files whose variables will be reported in `gnus-bug'."
  :version "22.1"
  :group 'gnus-message
  :type '(repeat (string :tag "File")))

(defcustom gnus-debug-exclude-variables
  '(mm-mime-mule-charset-alist
    nnmail-split-fancy message-minibuffer-local-map)
  "Variables that should not be reported in `gnus-bug'."
  :version "22.1"
  :group 'gnus-message
  :type '(repeat (symbol :tag "Variable")))

(defcustom gnus-discouraged-post-methods
  '(nndraft nnml nnimap nnmaildir nnmh nnfolder nndir)
  "A list of back ends that are not used in \"real\" newsgroups.
This variable is used only when `gnus-post-method' is `current'."
  :version "22.1"
  :group 'gnus-group-foreign
  :type '(repeat (symbol :tag "Back end")))

(defcustom gnus-message-replysign
  nil
  "Automatically sign replies to signed messages.
See also the `mml-default-sign-method' variable."
  :group 'gnus-message
  :type 'boolean)

(defcustom gnus-message-replyencrypt
  nil
  "Automatically encrypt replies to encrypted messages.
See also the `mml-default-encrypt-method' variable."
  :group 'gnus-message
  :type 'boolean)

(defcustom gnus-message-replysignencrypted
  t
  "Setting this causes automatically encrypted messages to also be signed."
  :group 'gnus-message
  :type 'boolean)

(defcustom gnus-confirm-mail-reply-to-news nil
  "If non-nil, Gnus requests confirmation when replying to news.
This is done because new users often reply by mistake when reading
news.
This can also be a function receiving the group name as the only
parameter which should return non-nil iff a confirmation is needed, or
a regexp, in which case a confirmation is asked for iff the group name
matches the regexp."
  :version "22.1"
  :group 'gnus-message
  :type '(choice (const :tag "No" nil)
		 (const :tag "Yes" t)
		 (regexp :tag "Iff group matches regexp")
		 (function :tag "Iff function evaluates to non-nil")))

(defcustom gnus-confirm-treat-mail-like-news
  nil
  "If non-nil, Gnus will treat mail like news with regard to confirmation
when replying by mail.  See the `gnus-confirm-mail-reply-to-news' variable
for fine-tuning this.
If nil, Gnus will never ask for confirmation if replying to mail."
  :version "22.1"
  :group 'gnus-message
  :type 'boolean)

(defcustom gnus-summary-resend-default-address t
  "If non-nil, Gnus tries to suggest a default address to resend to.
If nil, the address field will always be empty after invoking
`gnus-summary-resend-message'."
  :version "22.1"
  :group 'gnus-message
  :type 'boolean)

;;; Internal variables.

(defvar gnus-inhibit-posting-styles nil
  "Inhibit the use of posting styles.")

(defvar gnus-article-yanked-articles nil)
(defvar gnus-message-buffer "*Mail Gnus*")
(defvar gnus-article-copy nil)
(defvar gnus-check-before-posting nil)
(defvar gnus-last-posting-server nil)
(defvar gnus-message-group-art nil)

(defvar gnus-msg-force-broken-reply-to nil)

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
  "i" gnus-summary-news-other-window
  "f" gnus-summary-followup
  "F" gnus-summary-followup-with-original
  "c" gnus-summary-cancel-article
  "s" gnus-summary-supersede-article
  "r" gnus-summary-reply
  "y" gnus-summary-yank-message
  "R" gnus-summary-reply-with-original
  "w" gnus-summary-wide-reply
  "W" gnus-summary-wide-reply-with-original
  "v" gnus-summary-very-wide-reply
  "V" gnus-summary-very-wide-reply-with-original
  "n" gnus-summary-followup-to-mail
  "N" gnus-summary-followup-to-mail-with-original
  "m" gnus-summary-mail-other-window
  "u" gnus-uu-post-news
  "\M-c" gnus-summary-mail-crosspost-complaint
  "Br" gnus-summary-reply-broken-reply-to
  "BR" gnus-summary-reply-broken-reply-to-with-original
  "om" gnus-summary-mail-forward
  "op" gnus-summary-post-forward
  "Om" gnus-uu-digest-mail-forward
  "Op" gnus-uu-digest-post-forward)

(gnus-define-keys (gnus-send-bounce-map "D" gnus-summary-send-map)
  "b" gnus-summary-resend-bounced-mail
  ;; "c" gnus-summary-send-draft
  "r" gnus-summary-resend-message
  "e" gnus-summary-resend-message-edit)

;;; Internal functions.

(defun gnus-inews-make-draft ()
  `(lambda ()
     (gnus-inews-make-draft-meta-information
      ,gnus-newsgroup-name ',gnus-article-reply)))

(defvar gnus-article-reply nil)
(defmacro gnus-setup-message (config &rest forms)
  (let ((winconf (make-symbol "gnus-setup-message-winconf"))
	(buffer (make-symbol "gnus-setup-message-buffer"))
	(article (make-symbol "gnus-setup-message-article"))
	(yanked (make-symbol "gnus-setup-yanked-articles"))
	(group (make-symbol "gnus-setup-message-group")))
    `(let ((,winconf (current-window-configuration))
	   (,buffer (buffer-name (current-buffer)))
	   (,article gnus-article-reply)
	   (,yanked gnus-article-yanked-articles)
	   (,group gnus-newsgroup-name)
	   (message-header-setup-hook
	    (copy-sequence message-header-setup-hook))
	   (mbl mml-buffer-list)
	   (message-mode-hook (copy-sequence message-mode-hook)))
       (setq mml-buffer-list nil)
       (add-hook 'message-header-setup-hook 'gnus-inews-insert-gcc)
       (add-hook 'message-header-setup-hook 'gnus-inews-insert-archive-gcc)
       ;; #### FIXME: for a reason that I did not manage to identify yet,
       ;; the variable `gnus-newsgroup-name' does not honor a dynamically
       ;; scoped or setq'ed value from a caller like `C-u gnus-summary-mail'.
       ;; After evaluation of @forms below, it gets the value we actually want
       ;; to override, and the posting styles are used. For that reason, I've
       ;; added an optional argument to `gnus-configure-posting-styles' to
       ;; make sure that the correct value for the group name is used. -- drv
       (add-hook 'message-mode-hook
		 (if (memq ,config '(reply-yank reply))
		     (lambda ()
		       (gnus-configure-posting-styles ,group))
		   (lambda ()
		     ;; There may be an old " *gnus article copy*" buffer.
		     (let (gnus-article-copy)
		       (gnus-configure-posting-styles ,group)))))
       (gnus-pull ',(intern gnus-draft-meta-information-header)
		  message-required-headers)
       (when (and ,group
		  (not (string= ,group "")))
	 (push (cons
		(intern gnus-draft-meta-information-header)
		(gnus-inews-make-draft))
	       message-required-headers))
       (unwind-protect
	   (progn
	     ,@forms)
	 (gnus-inews-add-send-actions ,winconf ,buffer ,article ,config
				      ,yanked)
	 (setq gnus-message-buffer (current-buffer))
	 (set (make-local-variable 'gnus-message-group-art)
	      (cons ,group ,article))
	 (set (make-local-variable 'gnus-newsgroup-name) ,group)
	 (gnus-run-hooks 'gnus-message-setup-hook)
	 (if (eq major-mode 'message-mode)
	     (let ((mbl1 mml-buffer-list))
	       (setq mml-buffer-list mbl)  ;; Global value
	       (set (make-local-variable 'mml-buffer-list) mbl1);; Local value
	       (gnus-make-local-hook 'kill-buffer-hook)
	       (gnus-make-local-hook 'change-major-mode-hook)
	       (add-hook 'change-major-mode-hook 'mml-destroy-buffers nil t)
	       (add-hook 'kill-buffer-hook 'mml-destroy-buffers t t))
	   (mml-destroy-buffers)
	   (setq mml-buffer-list mbl)))
       (message-hide-headers)
       (gnus-add-buffer)
       (gnus-configure-windows ,config t)
       (run-hooks 'post-command-hook)
       (set-buffer-modified-p nil))))

(defun gnus-inews-make-draft-meta-information (group article)
  (concat "(\"" group "\" "
	  (if article (number-to-string
		       (if (listp article)
			   (car article)
			 article)) "\"\"")
	  ")"))

;;;###autoload
(defun gnus-msg-mail (&optional to subject other-headers continue
				switch-action yank-action send-actions)
  "Start editing a mail message to be sent.
Like `message-mail', but with Gnus paraphernalia, particularly the
Gcc: header for archiving purposes."
  (interactive)
  (let ((buf (current-buffer))
	mail-buf)
    (gnus-setup-message 'message
      (message-mail to subject other-headers continue
		    nil yank-action send-actions))
    (when switch-action
      (setq mail-buf (current-buffer))
      (switch-to-buffer buf)
      (apply switch-action mail-buf nil)))
  ;; COMPOSEFUNC should return t if succeed.  Undocumented ???
  t)

;;;###autoload
(defun gnus-button-mailto (address)
  "Mail to ADDRESS."
  (set-buffer (gnus-copy-article-buffer))
  (gnus-setup-message 'message
    (message-reply address)))

;;;###autoload
(defun gnus-button-reply (&optional to-address wide)
  "Like `message-reply'."
  (interactive)
  (gnus-setup-message 'message
    (message-reply to-address wide)))

;;;###autoload
(define-mail-user-agent 'gnus-user-agent
  'gnus-msg-mail 'message-send-and-exit
  'message-kill-buffer 'message-send-hook)

(defun gnus-setup-posting-charset (group)
  (let ((alist gnus-group-posting-charset-alist)
	(group (or group ""))
	elem)
    (when group
      (catch 'found
	(while (setq elem (pop alist))
	  (when (or (and (stringp (car elem))
			 (string-match (car elem) group))
		    (and (functionp (car elem))
			 (funcall (car elem) group))
		    (and (symbolp (car elem))
			 (symbol-value (car elem))))
	    (throw 'found (cons (cadr elem) (caddr elem)))))))))

(defun gnus-inews-add-send-actions (winconf buffer article
					    &optional config yanked)
  (gnus-make-local-hook 'message-sent-hook)
  (add-hook 'message-sent-hook (if gnus-agent 'gnus-agent-possibly-do-gcc
				 'gnus-inews-do-gcc) nil t)
  (when gnus-agent
    (gnus-make-local-hook 'message-header-hook)
    (add-hook 'message-header-hook 'gnus-agent-possibly-save-gcc nil t))
  (setq message-post-method
	`(lambda (arg)
	   (gnus-post-method arg ,gnus-newsgroup-name)))
  (setq message-newsreader (setq message-mailer (gnus-extended-version)))
  (message-add-action
   `(when (gnus-buffer-exists-p ,buffer)
      (set-window-configuration ,winconf))
   'exit 'postpone 'kill)
  (let ((to-be-marked (cond
		       (yanked
			(mapcar
			 (lambda (x) (if (listp x) (car x) x)) yanked))
		       (article (if (listp article) article (list article)))
		       (t nil))))
    (message-add-action
     `(when (gnus-buffer-exists-p ,buffer)
	(save-excursion
	  (set-buffer ,buffer)
	  ,(when to-be-marked
	     (if (eq config 'forward)
		 `(gnus-summary-mark-article-as-forwarded ',to-be-marked)
	       `(gnus-summary-mark-article-as-replied ',to-be-marked)))))
     'send)))

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
	;; make sure last viewed article doesn't affect posting styles:
	(gnus-article-copy)
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
	  ;; #### see comment in gnus-setup-message -- drv
	  (gnus-setup-message 'message (message-mail)))
      (save-excursion
	(set-buffer buffer)
	(setq gnus-newsgroup-name group)))))

(defun gnus-group-news (&optional arg)
  "Start composing a news.
If ARG, post to group under point.
If ARG is 1, prompt for group name to post to.

This function prepares a news even when using mail groups.  This is useful
for posting messages to mail groups without actually sending them over the
network.  The corresponding back end must have a 'request-post method."
  (interactive "P")
  ;; We can't `let' gnus-newsgroup-name here, since that leads
  ;; to local variables leaking.
  (let ((group gnus-newsgroup-name)
	;; make sure last viewed article doesn't affect posting styles:
	(gnus-article-copy)
	(buffer (current-buffer)))
    (unwind-protect
	(progn
	  (setq gnus-newsgroup-name
		(if arg
		    (if (= 1 (prefix-numeric-value arg))
			(completing-read "Use group: "
					 gnus-active-hashtb nil
					 (gnus-read-active-file-p))
		      (gnus-group-group-name))
		  ""))
	  ;; #### see comment in gnus-setup-message -- drv
	  (gnus-setup-message 'message
	    (message-news (gnus-group-real-name gnus-newsgroup-name))))
      (save-excursion
	(set-buffer buffer)
	(setq gnus-newsgroup-name group)))))

(defun gnus-group-post-news (&optional arg)
  "Start composing a message (a news by default).
If ARG, post to group under point.  If ARG is 1, prompt for group name.
Depending on the selected group, the message might be either a mail or
a news."
  (interactive "P")
  ;; Bind this variable here to make message mode hooks work ok.
  (let ((gnus-newsgroup-name
	 (if arg
	     (if (= 1 (prefix-numeric-value arg))
		 (completing-read "Newsgroup: " gnus-active-hashtb nil
				  (gnus-read-active-file-p))
	       (gnus-group-group-name))
	   ""))
	;; make sure last viewed article doesn't affect posting styles:
	(gnus-article-copy))
    (gnus-post-news 'post gnus-newsgroup-name nil nil nil nil
		    (string= gnus-newsgroup-name ""))))

(defun gnus-summary-mail-other-window (&optional arg)
  "Start composing a mail in another window.
Use the posting of the current group by default.
If ARG, don't do that.  If ARG is 1, prompt for group name to find the
posting style."
  (interactive "P")
  ;; We can't `let' gnus-newsgroup-name here, since that leads
  ;; to local variables leaking.
  (let ((group gnus-newsgroup-name)
	;; make sure last viewed article doesn't affect posting styles:
	(gnus-article-copy)
	(buffer (current-buffer)))
    (unwind-protect
	(progn
	  (setq gnus-newsgroup-name
		(if arg
		    (if (= 1 (prefix-numeric-value arg))
			(completing-read "Use group: "
					 gnus-active-hashtb nil
					 (gnus-read-active-file-p))
		      "")
		  gnus-newsgroup-name))
	  ;; #### see comment in gnus-setup-message -- drv
	  (gnus-setup-message 'message (message-mail)))
      (save-excursion
	(set-buffer buffer)
	(setq gnus-newsgroup-name group)))))

(defun gnus-summary-news-other-window (&optional arg)
  "Start composing a news in another window.
Post to the current group by default.
If ARG, don't do that.  If ARG is 1, prompt for group name to post to.

This function prepares a news even when using mail groups.  This is useful
for posting messages to mail groups without actually sending them over the
network.  The corresponding back end must have a 'request-post method."
  (interactive "P")
  ;; We can't `let' gnus-newsgroup-name here, since that leads
  ;; to local variables leaking.
  (let ((group gnus-newsgroup-name)
	;; make sure last viewed article doesn't affect posting styles:
	(gnus-article-copy)
	(buffer (current-buffer)))
    (unwind-protect
	(progn
	  (setq gnus-newsgroup-name
		(if arg
		    (if (= 1 (prefix-numeric-value arg))
			(completing-read "Use group: "
					 gnus-active-hashtb nil
					 (gnus-read-active-file-p))
		      "")
		  gnus-newsgroup-name))
	  ;; #### see comment in gnus-setup-message -- drv
	  (gnus-setup-message 'message
	    (progn
	      (message-news (gnus-group-real-name gnus-newsgroup-name))
	      (set (make-local-variable 'gnus-discouraged-post-methods)
		   (delq
		    (car (gnus-find-method-for-group gnus-newsgroup-name))
		    (copy-sequence gnus-discouraged-post-methods))))))
      (save-excursion
	(set-buffer buffer)
	(setq gnus-newsgroup-name group)))))

(defun gnus-summary-post-news (&optional arg)
  "Start composing a message.  Post to the current group by default.
If ARG, don't do that.  If ARG is 1, prompt for a group name to post to.
Depending on the selected group, the message might be either a mail or
a news."
  (interactive "P")
  ;; Bind this variable here to make message mode hooks work ok.
  (let ((gnus-newsgroup-name
	 (if arg
	     (if (= 1 (prefix-numeric-value arg))
		 (completing-read "Newsgroup: " gnus-active-hashtb nil
				  (gnus-read-active-file-p))
	       "")
	   gnus-newsgroup-name))
	;; make sure last viewed article doesn't affect posting styles:
	(gnus-article-copy))
    (gnus-post-news 'post gnus-newsgroup-name)))


(defun gnus-summary-followup (yank &optional force-news)
  "Compose a followup to an article.
If prefix argument YANK is non-nil, the original article is yanked
automatically.
YANK is a list of elements, where the car of each element is the
article number, and the cdr is the string to be yanked."
  (interactive
   (list (and current-prefix-arg
	      (gnus-summary-work-articles 1))))
  (when yank
    (gnus-summary-goto-subject
     (if (listp (car yank))
	 (caar yank)
       (car yank))))
  (save-window-excursion
    (gnus-summary-select-article))
  (let ((headers (gnus-summary-article-header (gnus-summary-article-number)))
	(gnus-newsgroup-name gnus-newsgroup-name))
    ;; Send a followup.
    (gnus-post-news nil gnus-newsgroup-name
		    headers gnus-article-buffer
		    yank nil force-news)
    (gnus-summary-handle-replysign)))

(defun gnus-summary-followup-with-original (n &optional force-news)
  "Compose a followup to an article and include the original article.
The text in the region will be yanked.  If the region isn't
active, the entire article will be yanked."
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
  (let (beg article yank-string)
    (message-goto-body)
    (while (setq article (pop articles))
      (when (listp article)
	(setq yank-string (nth 1 article)
	      article (nth 0 article)))
      (save-window-excursion
	(set-buffer gnus-summary-buffer)
	(gnus-summary-select-article nil nil nil article)
	(gnus-summary-remove-process-mark article))
      (gnus-copy-article-buffer nil yank-string)
      (let ((message-reply-buffer gnus-article-copy)
	    (message-reply-headers
	     ;; The headers are decoded.
	     (with-current-buffer gnus-article-copy
	       (save-restriction
		 (nnheader-narrow-to-headers)
		 (nnheader-parse-naked-head)))))
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
	    (gnus-post-method (eq ',symp 'a) ,gnus-newsgroup-name)))
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



(defun gnus-copy-article-buffer (&optional article-buffer yank-string)
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
	(let ((gnus-newsgroup-charset (or gnus-article-charset
					  gnus-newsgroup-charset))
	      (gnus-newsgroup-ignored-charsets
	       (or gnus-article-ignored-charsets
		   gnus-newsgroup-ignored-charsets)))
	  (save-restriction
	    ;; Copy over the (displayed) article buffer, delete
	    ;; hidden text and remove text properties.
	    (widen)
	    (copy-to-buffer gnus-article-copy (point-min) (point-max))
	    (set-buffer gnus-article-copy)
	    (when yank-string
	      (message-goto-body)
	      (delete-region (point) (point-max))
	      (insert yank-string))
	    (gnus-article-delete-text-of-type 'annotation)
	    (gnus-remove-text-with-property 'gnus-prev)
	    (gnus-remove-text-with-property 'gnus-next)
	    (gnus-remove-text-with-property 'gnus-decoration)
	    (insert
	     (prog1
		 (buffer-substring-no-properties (point-min) (point-max))
	       (erase-buffer)))
	    ;; Find the original headers.
	    (set-buffer gnus-original-article-buffer)
	    (goto-char (point-min))
	    (while (looking-at message-unix-mail-delimiter)
	      (forward-line 1))
	    (let ((mail-header-separator ""))
	      (setq beg (point)
		    end (or (message-goto-body)
			    ;; There may be just a header.
			    (point-max))))
	    ;; Delete the headers from the displayed articles.
	    (set-buffer gnus-article-copy)
	    (let ((mail-header-separator ""))
	      (delete-region (goto-char (point-min))
			     (or (message-goto-body) (point-max))))
	    ;; Insert the original article headers.
	    (insert-buffer-substring gnus-original-article-buffer beg end)
	    ;; Decode charsets.
	    (let ((gnus-article-decode-hook
		   (delq 'article-decode-charset
			 (copy-sequence gnus-article-decode-hook)))
		  (rfc2047-quote-decoded-words-containing-tspecials t))
	      (run-hooks 'gnus-article-decode-hook)))))
      gnus-article-copy)))

(defun gnus-post-news (post &optional group header article-buffer yank subject
			    force-news)
  (when article-buffer
    (gnus-copy-article-buffer))
  (let ((gnus-article-reply (and article-buffer (gnus-summary-article-number)))
	(gnus-article-yanked-articles yank)
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
	  (setq to-address (gnus-parameter-to-address group)
		to-group (gnus-group-find-parameter group 'to-group)
		to-list (gnus-parameter-to-list group)
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
		      (or header gnus-current-article))
		     (not mailing-list)
		     (not to-list)
		     (not to-address)))
	    ;; This is news.
	    (if post
		(message-news
		 (or to-group
		     (and (not (gnus-virtual-group-p pgroup)) group)))
	      (set-buffer gnus-article-copy)
	      (gnus-msg-treat-broken-reply-to)
	      (message-followup (if (or newsgroup-p force-news)
				    (if (save-restriction
					  (article-narrow-to-head)
					  (message-fetch-field "newsgroups"))
					nil
				      "")
				  to-group)))
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

(defun gnus-msg-treat-broken-reply-to (&optional force)
  "Remove the Reply-to header if broken-reply-to."
  (when (or force
	    (gnus-group-find-parameter
	     gnus-newsgroup-name 'broken-reply-to))
    (save-restriction
      (message-narrow-to-head)
      (message-remove-header "reply-to"))))

(defun gnus-post-method (arg group &optional silent)
  "Return the posting method based on GROUP and ARG.
If SILENT, don't prompt the user."
  (let ((gnus-post-method (or (gnus-parameter-post-method group)
			      gnus-post-method))
	(group-method (gnus-find-method-for-group group)))
    (cond
     ;; If the group-method is nil (which shouldn't happen) we use
     ;; the default method.
     ((null group-method)
      (or (and (listp gnus-post-method)	;If not current/native/nil
	       (not (listp (car gnus-post-method))) ; and not a list of methods
	       gnus-post-method)	;then use it.
	  gnus-select-method
	  message-post-method))
     ;; We want the inverse of the default
     ((and arg (not (eq arg 0)))
      (if (eq gnus-post-method 'current)
	  gnus-select-method
	group-method))
     ;; We query the user for a post method.
     ((or arg
	  (and (listp gnus-post-method)
	       (listp (car gnus-post-method))))
      (let* ((methods
	      ;; Collect all methods we know about.
	      (append
	       (when (listp gnus-post-method)
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
		 (if (equal (cadr m) "")
		     (list (symbol-name (car m)) m)
		   (list (concat (cadr m) " (" (symbol-name (car m)) ")") m)))
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
	   (not (memq (car group-method) gnus-discouraged-post-methods))
	   (gnus-get-function group-method 'request-post t))
      (assert (not arg))
      group-method)
     ;; Use gnus-post-method.
     ((listp gnus-post-method)		;A method...
      (assert (not (listp (car gnus-post-method)))) ;... not a list of methods.
      gnus-post-method)
     ;; Use the normal select method (nil or native).
     (t gnus-select-method))))



(defun gnus-extended-version ()
  "Stringified Gnus version and Emacs version.
See the variable `gnus-user-agent'."
  (interactive)
  (if (stringp gnus-user-agent)
      gnus-user-agent
    ;; `gnus-user-agent' is a list:
    (let* ((float-output-format nil)
	   (gnus-v
	    (when (memq 'gnus gnus-user-agent)
	      (concat "Gnus/"
		      (prin1-to-string (gnus-continuum-version gnus-version) t)
		      " (" gnus-version ")")))
	   (emacs-v (gnus-emacs-version)))
      (concat gnus-v (when (and gnus-v emacs-v) " ")
	      emacs-v))))


;;;
;;; Gnus Mail Functions
;;;

;;; Mail reply commands of Gnus summary mode

(defun gnus-summary-reply (&optional yank wide very-wide)
  "Start composing a mail reply to the current message.
If prefix argument YANK is non-nil, the original article is yanked
automatically.
If WIDE, make a wide reply.
If VERY-WIDE, make a very wide reply."
  (interactive
   (list (and current-prefix-arg
	      (gnus-summary-work-articles 1))))
  ;; Allow user to require confirmation before replying by mail to the
  ;; author of a news article (or mail message).
  (when (or
	    (not (or (gnus-news-group-p gnus-newsgroup-name)
		     gnus-confirm-treat-mail-like-news))
	    (not (cond ((stringp gnus-confirm-mail-reply-to-news)
			(string-match gnus-confirm-mail-reply-to-news
				      gnus-newsgroup-name))
		       ((functionp gnus-confirm-mail-reply-to-news)
			(funcall gnus-confirm-mail-reply-to-news gnus-newsgroup-name))
		       (t gnus-confirm-mail-reply-to-news)))
	    (y-or-n-p "Really reply by mail to article author? "))
    (let* ((article
	    (if (listp (car yank))
		(caar yank)
	      (car yank)))
	   (gnus-article-reply (or article (gnus-summary-article-number)))
	   (gnus-article-yanked-articles yank)
	   (headers ""))
      ;; Stripping headers should be specified with mail-yank-ignored-headers.
      (when yank
	(gnus-summary-goto-subject article))
      (gnus-setup-message (if yank 'reply-yank 'reply)
	(if (not very-wide)
	    (gnus-summary-select-article)
	  (dolist (article very-wide)
	    (gnus-summary-select-article nil nil nil article)
	    (save-excursion
	      (set-buffer (gnus-copy-article-buffer))
	      (gnus-msg-treat-broken-reply-to)
	      (save-restriction
		(message-narrow-to-head)
		(setq headers (concat headers (buffer-string)))))))
	(set-buffer (gnus-copy-article-buffer))
	(gnus-msg-treat-broken-reply-to gnus-msg-force-broken-reply-to)
	(save-restriction
	  (message-narrow-to-head)
	  (when very-wide
	    (erase-buffer)
	    (insert headers))
	  (goto-char (point-max)))
	(mml-quote-region (point) (point-max))
	(message-reply nil wide)
	(when yank
	  (gnus-inews-yank-articles yank))
	(gnus-summary-handle-replysign)))))

(defun gnus-summary-handle-replysign ()
  "Check the various replysign variables and take action accordingly."
  (when (or gnus-message-replysign gnus-message-replyencrypt)
    (let (signed encrypted)
      (save-excursion
	(set-buffer gnus-article-buffer)
	(setq signed (memq 'signed gnus-article-wash-types))
	(setq encrypted (memq 'encrypted gnus-article-wash-types)))
      (cond ((and gnus-message-replyencrypt encrypted)
	     (mml-secure-message mml-default-encrypt-method
				 (if gnus-message-replysignencrypted
				     'signencrypt
				   'encrypt)))
	    ((and gnus-message-replysign signed)
	     (mml-secure-message mml-default-sign-method 'sign))))))

(defun gnus-summary-reply-with-original (n &optional wide)
  "Start composing a reply mail to the current message.
The original article will be yanked."
  (interactive "P")
  (gnus-summary-reply (gnus-summary-work-articles n) wide))

(defun gnus-summary-reply-broken-reply-to (&optional yank wide very-wide)
  "Like `gnus-summary-reply' except removing reply-to field.
If prefix argument YANK is non-nil, the original article is yanked
automatically.
If WIDE, make a wide reply.
If VERY-WIDE, make a very wide reply."
  (interactive
   (list (and current-prefix-arg
	      (gnus-summary-work-articles 1))))
  (let ((gnus-msg-force-broken-reply-to t))
    (gnus-summary-reply yank wide very-wide)))

(defun gnus-summary-reply-broken-reply-to-with-original (n &optional wide)
  "Like `gnus-summary-reply-with-original' except removing reply-to field.
The original article will be yanked."
  (interactive "P")
  (gnus-summary-reply-broken-reply-to (gnus-summary-work-articles n) wide))

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
The original article will be yanked.
Uses the process/prefix convention."
  (interactive "P")
  (gnus-summary-reply-with-original n t))

(defun gnus-summary-very-wide-reply (&optional yank)
  "Start composing a very wide reply mail to the current message.
If prefix argument YANK is non-nil, the original article is yanked
automatically."
  (interactive
   (list (and current-prefix-arg
	      (gnus-summary-work-articles 1))))
  (gnus-summary-reply yank t (gnus-summary-work-articles yank)))

(defun gnus-summary-very-wide-reply-with-original (n)
  "Start composing a very wide reply mail to the current message.
The original article will be yanked."
  (interactive "P")
  (gnus-summary-reply
   (gnus-summary-work-articles n) t (gnus-summary-work-articles n)))

(defun gnus-summary-mail-forward (&optional arg post)
  "Forward the current message(s) to another user.
If process marks exist, forward all marked messages;
if ARG is nil, see `message-forward-as-mime' and `message-forward-show-mml';
if ARG is 1, decode the message and forward directly inline;
if ARG is 2, forward message as an rfc822 MIME section;
if ARG is 3, decode message and forward as an rfc822 MIME section;
if ARG is 4, forward message directly inline;
otherwise, use flipped `message-forward-as-mime'.
If POST, post instead of mail.
For the `inline' alternatives, also see the variable
`message-forward-ignored-headers'."
  (interactive "P")
  (if (cdr (gnus-summary-work-articles nil))
      ;; Process marks are given.
      (gnus-uu-digest-mail-forward arg post)
    ;; No process marks.
    (let ((message-forward-as-mime message-forward-as-mime)
	  (message-forward-show-mml message-forward-show-mml))
      (cond
       ((null arg))
       ((eq arg 1)
	(setq message-forward-as-mime nil
	      message-forward-show-mml t))
       ((eq arg 2)
	(setq message-forward-as-mime t
	      message-forward-show-mml nil))
       ((eq arg 3)
	(setq message-forward-as-mime t
	      message-forward-show-mml t))
       ((eq arg 4)
	(setq message-forward-as-mime nil
	      message-forward-show-mml nil))
       (t
	(setq message-forward-as-mime (not message-forward-as-mime))))
      (let* ((gnus-article-reply (gnus-summary-article-number))
	     (gnus-article-yanked-articles (list gnus-article-reply)))
	(gnus-setup-message 'forward
	  (gnus-summary-select-article)
	  (let ((mail-parse-charset
		 (or (and (gnus-buffer-live-p gnus-article-buffer)
			  (with-current-buffer gnus-article-buffer
			    gnus-article-charset))
		     gnus-newsgroup-charset))
		(mail-parse-ignored-charsets
		 gnus-newsgroup-ignored-charsets))
	    (set-buffer gnus-original-article-buffer)
	    (message-forward post)))))))

(defun gnus-summary-resend-message (address n)
  "Resend the current article to ADDRESS."
  (interactive
   (list (message-read-from-minibuffer
	  "Resend message(s) to: "
	  (when (and gnus-summary-resend-default-address
		     (gnus-buffer-live-p gnus-original-article-buffer))
	    ;; If some other article is currently selected, the
	    ;; initial-contents is wrong. Whatever, it is just the
	    ;; initial-contents.
	    (with-current-buffer gnus-original-article-buffer
	      (nnmail-fetch-field "to"))))
	 current-prefix-arg))
  (let ((articles (gnus-summary-work-articles n))
	article)
    (while (setq article (pop articles))
      (gnus-summary-select-article nil nil nil article)
      (save-excursion
	(set-buffer gnus-original-article-buffer)
	(message-resend address))
      (gnus-summary-mark-article-as-forwarded article))))

;; From: Matthieu Moy <Matthieu.Moy@imag.fr>
(defun gnus-summary-resend-message-edit ()
  "Resend an article that has already been sent.
A new buffer will be created to allow the user to modify body and
contents of the message, and then, everything will happen as when
composing a new message."
  (interactive)
  (let ((article (gnus-summary-article-number)))
    (gnus-setup-message 'reply-yank
      (gnus-summary-select-article t)
      (set-buffer gnus-original-article-buffer)
      (let ((cur (current-buffer))
	    (to (message-fetch-field "to")))
	;; Get a normal message buffer.
	(message-pop-to-buffer (message-buffer-name "Resend" to))
	(insert-buffer-substring cur)
	(mime-to-mml)
	(message-narrow-to-head-1)
	;; Gnus will generate a new one when sending.
	(message-remove-header "Message-ID")
	(message-remove-header message-ignored-resent-headers t)
	;; Remove unwanted headers.
	(goto-char (point-max))
	(insert mail-header-separator)
	(goto-char (point-min))
	(when (re-search-forward "^To:\\|^Newsgroups:" nil 'move)
	  (forward-char 1))
	(widen)))))

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
	     (format "Do you want to add this as `to-list': %s? " to-address))
	(gnus-group-add-parameter group (cons 'to-list to-address))))))

(defun gnus-put-message ()
  "Put the current message in some group and return to Gnus."
  (interactive)
  (let ((reply gnus-article-reply)
	(winconf gnus-prev-winconf)
	(group gnus-newsgroup-name))
    (unless (and group
		 (not (gnus-group-read-only-p group)))
      (setq group (read-string "Put in group: " nil (gnus-writable-groups))))

    (when (gnus-gethash group gnus-newsrc-hashtb)
      (error "No such group: %s" group))
    (save-excursion
      (save-restriction
	(widen)
	(message-narrow-to-headers)
	(let ((gnus-deletable-headers nil))
	  (message-generate-headers
	   (if (message-news-p)
	       message-required-news-headers
	     message-required-mail-headers)))
	(goto-char (point-max))
	(if (string-match " " group)
	    (insert "Gcc: \"" group "\"\n")
	  (insert "Gcc: " group "\n"))
	(widen)))
    (gnus-inews-do-gcc)
    (when (and (get-buffer gnus-group-buffer)
	       (gnus-buffer-exists-p (car-safe reply))
	       (cdr reply))
      (set-buffer (car reply))
      (gnus-summary-mark-article-as-replied (cdr reply)))
    (when winconf
      (set-window-configuration winconf))))

(defun gnus-article-mail (yank)
  "Send a reply to the address near point.
If YANK is non-nil, include the original article."
  (interactive "P")
  (let ((address
	 (buffer-substring
	  (save-excursion (re-search-backward "[ \t\n]" nil t) (1+ (point)))
	  (save-excursion (re-search-forward "[ \t\n]" nil t) (1- (point))))))
    (when address
      (gnus-msg-mail address)
      (when yank
	(gnus-inews-yank-articles (list (cdr gnus-article-current)))))))

(defvar nntp-server-type)
(defun gnus-bug ()
  "Send a bug report to the Gnus maintainers."
  (interactive)
  (unless (gnus-alive-p)
    (error "Gnus has been shut down"))
  (gnus-setup-message (if (message-mail-user-agent) 'message 'bug)
    (unless (message-mail-user-agent)
      (delete-other-windows)
      (when gnus-bug-create-help-buffer
	(switch-to-buffer "*Gnus Help Bug*")
	(erase-buffer)
	(insert gnus-bug-message)
	(goto-char (point-min)))
      (message-pop-to-buffer "*Gnus Bug*"))
    (let ((message-this-is-mail t))
      (message-setup `((To . ,gnus-maintainer) (Subject . ""))))
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
	(erase-buffer)
	(gnus-debug)
	(setq text (buffer-string)))
      (insert "<#part type=application/emacs-lisp disposition=inline description=\"User settings\">\n" text "\n<#/part>"))
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
    (let ((gnus-inhibit-treatment t))
      (gnus-summary-select-article))
    (save-excursion
      (set-buffer buffer)
      (message-yank-buffer gnus-article-buffer))))

(defun gnus-debug ()
  "Attempts to go through the Gnus source file and report what variables have been changed.
The source file has to be in the Emacs load path."
  (interactive)
  (let ((files gnus-debug-files)
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
		     (not (memq (nth 1 expr) gnus-debug-exclude-variables))
		     (or (not (boundp (nth 1 expr)))
			 (not (equal (eval (nth 2 expr))
				     (symbol-value (nth 1 expr)))))
		     (push (nth 1 expr) olist)))))))
      (kill-buffer (current-buffer)))
    (when (setq olist (nreverse olist))
      (insert "------------------ Environment follows ------------------\n\n"))
    (while olist
      (if (boundp (car olist))
	  (ignore-errors
	   (gnus-pp
	    `(setq ,(car olist)
		   ,(if (or (consp (setq sym (symbol-value (car olist))))
			    (and (symbolp sym)
				 (not (or (eq sym nil)
					  (eq sym t)))))
			(list 'quote (symbol-value (car olist)))
		      (symbol-value (car olist))))))
	(insert ";; (makeunbound '" (symbol-name (car olist)) ")\n"))
      (setq olist (cdr olist)))
    (insert "\n\n")
    ;; Remove any control chars - they seem to cause trouble for some
    ;; mailers.  (Byte-compiled output from the stuff above.)
    (goto-char point)
    (while (re-search-forward (mm-string-as-multibyte
			       "[\000-\010\013-\037\200-\237]") nil t)
      (replace-match (format "\\%03o" (string-to-char (match-string 0)))
		     t t))))

;;; Treatment of rejected articles.
;;; Bounced mail.

(defun gnus-summary-resend-bounced-mail (&optional fetch)
  "Re-mail the current message.
This only makes sense if the current message is a bounce message that
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

(defun gnus-inews-group-method (group)
  (cond
   ;; If the group doesn't exist, we assume
   ;; it's an archive group...
   ((and (null (gnus-get-info group))
	 (eq (car (gnus-server-to-method gnus-message-archive-method))
	     (car (gnus-server-to-method (gnus-group-method group)))))
    gnus-message-archive-method)
   ;; Use the method.
   ((gnus-info-method (gnus-get-info group))
    (gnus-info-method (gnus-get-info group)))
   ;; Find the method.
   (t (gnus-server-to-method (gnus-group-method group)))))

;; Do Gcc handling, which copied the message over to some group.
(defun gnus-inews-do-gcc (&optional gcc)
  (interactive)
  (save-excursion
    (save-restriction
      (message-narrow-to-headers)
      (let ((gcc (or gcc (mail-fetch-field "gcc" nil t)))
	    (cur (current-buffer))
	    groups group method group-art
	    mml-externalize-attachments)
	(when gcc
	  (message-remove-header "gcc")
	  (widen)
	  (setq groups (message-unquote-tokens
			(message-tokenize-header gcc " ,")))
	  ;; Copy the article over to some group(s).
	  (while (setq group (pop groups))
	    (unless (gnus-check-server
		     (setq method (gnus-inews-group-method group)))
	      (error "Can't open server %s" (if (stringp method) method
					      (car method))))
	    (unless (gnus-request-group group nil method)
	      (gnus-request-create-group group method))
	    (setq mml-externalize-attachments
		  (if (stringp gnus-gcc-externalize-attachments)
		      (string-match gnus-gcc-externalize-attachments group)
		    gnus-gcc-externalize-attachments))
	    (save-excursion
	      (nnheader-set-temp-buffer " *acc*")
	      (insert-buffer-substring cur)
	      (message-encode-message-body)
	      (save-restriction
		(message-narrow-to-headers)
		(let* ((mail-parse-charset message-default-charset)
		       (newsgroups-field (save-restriction
					   (message-narrow-to-headers-or-head)
					   (message-fetch-field "Newsgroups")))
		       (followup-field (save-restriction
					 (message-narrow-to-headers-or-head)
					 (message-fetch-field "Followup-To")))
		       ;; BUG: We really need to get the charset for
		       ;; each name in the Newsgroups and Followup-To
		       ;; lines to allow crossposting between group
		       ;; namess with incompatible character sets.
		       ;; -- Per Abrahamsen <abraham@dina.kvl.dk> 2001-10-08.
		       (group-field-charset
			(gnus-group-name-charset
			 method (or newsgroups-field "")))
		       (followup-field-charset
			(gnus-group-name-charset
			 method (or followup-field "")))
		       (rfc2047-header-encoding-alist
			(append
			 (when group-field-charset
			   (list (cons "Newsgroups" group-field-charset)))
			 (when followup-field-charset
			   (list (cons "Followup-To" followup-field-charset)))
			 rfc2047-header-encoding-alist)))
		  (mail-encode-encoded-word-buffer)))
	      (goto-char (point-min))
	      (when (re-search-forward
		     (concat "^" (regexp-quote mail-header-separator) "$")
		     nil t)
		(replace-match "" t t ))
	      (unless (setq group-art
			    (gnus-request-accept-article group method t t))
		(gnus-message 1 "Couldn't store article in group %s: %s"
			      group (gnus-status-message method))
		(sit-for 2))
	      (when (and group-art
			 ;; FIXME: Should gcc-mark-as-read work when
			 ;; Gnus is not running?
			 (gnus-alive-p)
			 (or gnus-gcc-mark-as-read
			     (and
			      (boundp 'gnus-inews-mark-gcc-as-read)
			      (symbol-value 'gnus-inews-mark-gcc-as-read))))
		(gnus-group-mark-article-read group (cdr group-art)))
	      (kill-buffer (current-buffer)))))))))

(defun gnus-inews-insert-gcc ()
  "Insert Gcc headers based on `gnus-outgoing-message-group'."
  (save-excursion
    (save-restriction
      (message-narrow-to-headers)
      (let* ((group gnus-outgoing-message-group)
	     (gcc (cond
		   ((functionp group)
		    (funcall group))
		   ((or (stringp group) (listp group))
		    group))))
	(when gcc
	  (insert "Gcc: "
		  (if (stringp gcc)
		      (if (string-match " " gcc)
			  (concat "\"" gcc "\"")
			gcc)
		    (mapconcat (lambda (group)
				 (if (string-match " " group)
				     (concat "\"" group "\"")
				   group))
			       gcc " "))
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
	   ((functionp var)
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
				((functionp (car var))
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
		     (if (string-match " " gcc-self-val)
			 (concat "\"" gcc-self-val "\"")
		       gcc-self-val)
		   ;; In nndoc groups, we use the parent group name
		   ;; instead of the current group.
		   (let ((group (or (gnus-group-find-parameter
				     gnus-newsgroup-name 'parent-group)
				    group)))
		     (if (string-match " " group)
			 (concat "\"" group "\"")
		       group))))
		(if (not (eq gcc-self-val 'none))
		    (insert "\n")
		  (gnus-delete-line)))
	    ;; Use the list of groups.
	    (while (setq name (pop groups))
	      (let ((str (if (string-match ":" name)
			     name
			   (gnus-group-prefixed-name
			    name gnus-message-archive-method))))
		(insert (if (string-match " " str)
			    (concat "\"" str "\"")
			  str)))
	      (when groups
		(insert " ")))
	    (insert "\n")))))))

(defun gnus-mailing-list-followup-to ()
  "Look at the headers in the current buffer and return a Mail-Followup-To address."
  (let ((x-been-there (gnus-fetch-original-field "x-beenthere"))
	(list-post (gnus-fetch-original-field "list-post")))
    (when (and list-post
	       (string-match "mailto:\\([^>]+\\)" list-post))
      (setq list-post (match-string 1 list-post)))
    (or list-post
	x-been-there)))

;;; Posting styles.

(defun gnus-configure-posting-styles (&optional group-name)
  "Configure posting styles according to `gnus-posting-styles'."
  (unless gnus-inhibit-posting-styles
    (let ((group (or group-name gnus-newsgroup-name ""))
	  (styles gnus-posting-styles)
	  style match attribute value v results
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
		;; Obsolete format of header match.
		(and (gnus-buffer-live-p gnus-article-copy)
		     (with-current-buffer gnus-article-copy
		       (save-restriction
			 (nnheader-narrow-to-headers)
			 (let ((header (message-fetch-field (pop style))))
			   (and header
				(string-match (pop style) header)))))))
	       ((or (symbolp match)
		    (functionp match))
		(cond
		 ((functionp match)
		  ;; Function to be called.
		  (funcall match))
		 ((boundp match)
		  ;; Variable to be checked.
		  (symbol-value match))))
	       ((listp match)
		(cond
		 ((eq (car match) 'header)
		  ;; New format of header match.
		  (and (gnus-buffer-live-p gnus-article-copy)
		       (with-current-buffer gnus-article-copy
			 (save-restriction
			   (nnheader-narrow-to-headers)
			   (let ((header (message-fetch-field (nth 1 match))))
			     (and header
				  (string-match (nth 2 match) header)))))))
		 (t
		  ;; This is a form to be evaled.
		  (eval match)))))
	  ;; We have a match, so we set the variables.
	  (dolist (attribute style)
	    (setq element (pop attribute)
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
			(functionp value))
		    (cond ((functionp value)
			   (funcall value))
			  ((boundp value)
			   (symbol-value value))))
		   ((listp value)
		    (eval value))))
	    ;; Translate obsolescent value.
	    (cond
	     ((eq element 'signature-file)
	      (setq element 'signature
		    filep t))
	     ((eq element 'x-face-file)
	      (setq element 'x-face
		    filep t)))
	    ;; Get the contents of file elems.
	    (when (and filep v)
	      (setq v (with-temp-buffer
			(insert-file-contents v)
			(buffer-substring
			 (point-min)
			 (progn
			   (goto-char (point-max))
			   (if (zerop (skip-chars-backward "\n"))
			       (point)
			     (1+ (point))))))))
	    (setq results (delq (assoc element results) results))
	    (push (cons element v) results))))
      ;; Now we have all the styles, so we insert them.
      (setq name (assq 'name results)
	    address (assq 'address results))
      (setq results (delq name (delq address results)))
      (gnus-make-local-hook 'message-setup-hook)
      (setq results (sort results (lambda (x y)
				    (string-lessp (car x) (car y)))))
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
			       (insert ,header ": " value)
			       (unless (bolp)
				 (insert "\n")))))))))
		  nil 'local))
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
			 (insert "From: " (message-make-from) "\n"))))
		  nil 'local)))))

;;; Allow redefinition of functions.

(gnus-ems-redefine)

(provide 'gnus-msg)

;;; arch-tag: 9f22b2f5-1c0a-49de-916e-4c88e984852b
;;; gnus-msg.el ends here
