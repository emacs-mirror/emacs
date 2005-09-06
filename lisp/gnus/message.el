;;; message.el --- composing mail and news messages

;; Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: mail, news

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This mode provides mail-sending facilities from within Emacs.  It
;; consists mainly of large chunks of code from the sendmail.el,
;; gnus-msg.el and rnewspost.el files.

;;; Code:

(eval-when-compile
  (require 'cl)
  (defvar gnus-message-group-art)
  (defvar gnus-list-identifiers)) ; gnus-sum is required where necessary
(require 'canlock)
(require 'mailheader)
(require 'nnheader)
;; This is apparently necessary even though things are autoloaded.
;; Because we dynamically bind mail-abbrev-mode-regexp, we'd better
;; require mailabbrev here.
(if (featurep 'xemacs)
    (require 'mail-abbrevs)
  (require 'mailabbrev))
(require 'mail-parse)
(require 'mml)
(require 'rfc822)
(eval-and-compile
  (autoload 'gnus-find-method-for-group "gnus")
  (autoload 'nnvirtual-find-group-art "nnvirtual")
  (autoload 'gnus-group-decoded-name "gnus-group"))

(defgroup message '((user-mail-address custom-variable)
		    (user-full-name custom-variable))
  "Mail and news message composing."
  :link '(custom-manual "(message)Top")
  :group 'mail
  :group 'news)

(put 'user-mail-address 'custom-type 'string)
(put 'user-full-name 'custom-type 'string)

(defgroup message-various nil
  "Various Message Variables."
  :link '(custom-manual "(message)Various Message Variables")
  :group 'message)

(defgroup message-buffers nil
  "Message Buffers."
  :link '(custom-manual "(message)Message Buffers")
  :group 'message)

(defgroup message-sending nil
  "Message Sending."
  :link '(custom-manual "(message)Sending Variables")
  :group 'message)

(defgroup message-interface nil
  "Message Interface."
  :link '(custom-manual "(message)Interface")
  :group 'message)

(defgroup message-forwarding nil
  "Message Forwarding."
  :link '(custom-manual "(message)Forwarding")
  :group 'message-interface)

(defgroup message-insertion nil
  "Message Insertion."
  :link '(custom-manual "(message)Insertion")
  :group 'message)

(defgroup message-headers nil
  "Message Headers."
  :link '(custom-manual "(message)Message Headers")
  :group 'message)

(defgroup message-news nil
  "Composing News Messages."
  :group 'message)

(defgroup message-mail nil
  "Composing Mail Messages."
  :group 'message)

(defgroup message-faces nil
  "Faces used for message composing."
  :group 'message
  :group 'faces)

(defcustom message-directory "~/Mail/"
  "*Directory from which all other mail file variables are derived."
  :group 'message-various
  :type 'directory)

(defcustom message-max-buffers 10
  "*How many buffers to keep before starting to kill them off."
  :group 'message-buffers
  :type 'integer)

(defcustom message-send-rename-function nil
  "Function called to rename the buffer after sending it."
  :group 'message-buffers
  :type '(choice function (const nil)))

(defcustom message-fcc-handler-function 'message-output
  "*A function called to save outgoing articles.
This function will be called with the name of the file to store the
article in.  The default function is `message-output' which saves in Unix
mailbox format."
  :type '(radio (function-item message-output)
		(function :tag "Other"))
  :group 'message-sending)

(defcustom message-fcc-externalize-attachments nil
  "If non-nil, attachments are included as external parts in Fcc copies."
  :version "22.1"
  :type 'boolean
  :group 'message-sending)

(defcustom message-courtesy-message
  "The following message is a courtesy copy of an article\nthat has been posted to %s as well.\n\n"
  "*This is inserted at the start of a mailed copy of a posted message.
If the string contains the format spec \"%s\", the Newsgroups
the article has been posted to will be inserted there.
If this variable is nil, no such courtesy message will be added."
  :group 'message-sending
  :type '(radio string (const nil)))

(defcustom message-ignored-bounced-headers
  "^\\(Received\\|Return-Path\\|Delivered-To\\):"
  "*Regexp that matches headers to be removed in resent bounced mail."
  :group 'message-interface
  :type 'regexp)

;;;###autoload
(defcustom message-from-style 'default
  "*Specifies how \"From\" headers look.

If nil, they contain just the return address like:
	king@grassland.com
If `parens', they look like:
	king@grassland.com (Elvis Parsley)
If `angles', they look like:
	Elvis Parsley <king@grassland.com>

Otherwise, most addresses look like `angles', but they look like
`parens' if `angles' would need quoting and `parens' would not."
  :type '(choice (const :tag "simple" nil)
		 (const parens)
		 (const angles)
		 (const default))
  :group 'message-headers)

(defcustom message-insert-canlock t
  "Whether to insert a Cancel-Lock header in news postings."
  :version "22.1"
  :group 'message-headers
  :type 'boolean)

(defcustom message-syntax-checks
  (if message-insert-canlock '((sender . disabled)) nil)
  ;; Guess this one shouldn't be easy to customize...
  "*Controls what syntax checks should not be performed on outgoing posts.
To disable checking of long signatures, for instance, add
 `(signature . disabled)' to this list.

Don't touch this variable unless you really know what you're doing.

Checks include `subject-cmsg', `multiple-headers', `sendsys',
`message-id', `from', `long-lines', `control-chars', `size',
`new-text', `quoting-style', `redirected-followup', `signature',
`approved', `sender', `empty', `empty-headers', `message-id', `from',
`subject', `shorten-followup-to', `existing-newsgroups',
`buffer-file-name', `unchanged', `newsgroups', `reply-to',
`continuation-headers', `long-header-lines', `invisible-text' and
`illegible-text'."
  :group 'message-news
  :type '(repeat sexp))			; Fixme: improve this

(defcustom message-required-headers '((optional . References)
				      From)
  "*Headers to be generated or prompted for when sending a message.
Also see `message-required-news-headers' and
`message-required-mail-headers'."
  :version "22.1"
  :group 'message-news
  :group 'message-headers
  :link '(custom-manual "(message)Message Headers")
  :type '(repeat sexp))

(defcustom message-draft-headers '(References From)
  "*Headers to be generated when saving a draft message."
  :version "22.1"
  :group 'message-news
  :group 'message-headers
  :link '(custom-manual "(message)Message Headers")
  :type '(repeat sexp))

(defcustom message-required-news-headers
  '(From Newsgroups Subject Date Message-ID
	 (optional . Organization)
	 (optional . User-Agent))
  "*Headers to be generated or prompted for when posting an article.
RFC977 and RFC1036 require From, Date, Newsgroups, Subject,
Message-ID.  Organization, Lines, In-Reply-To, Expires, and
User-Agent are optional.  If don't you want message to insert some
header, remove it from this list."
  :group 'message-news
  :group 'message-headers
  :link '(custom-manual "(message)Message Headers")
  :type '(repeat sexp))

(defcustom message-required-mail-headers
  '(From Subject Date (optional . In-Reply-To) Message-ID
	 (optional . User-Agent))
  "*Headers to be generated or prompted for when mailing a message.
It is recommended that From, Date, To, Subject and Message-ID be
included.  Organization and User-Agent are optional."
  :group 'message-mail
  :group 'message-headers
  :link '(custom-manual "(message)Message Headers")
  :type '(repeat sexp))

(defcustom message-deletable-headers '(Message-ID Date Lines)
  "Headers to be deleted if they already exist and were generated by message previously."
  :group 'message-headers
  :link '(custom-manual "(message)Message Headers")
  :type 'sexp)

(defcustom message-ignored-news-headers
  "^NNTP-Posting-Host:\\|^Xref:\\|^[BGF]cc:\\|^Resent-Fcc:\\|^X-Draft-From:\\|^X-Gnus-Agent-Meta-Information:"
  "*Regexp of headers to be removed unconditionally before posting."
  :group 'message-news
  :group 'message-headers
  :link '(custom-manual "(message)Message Headers")
  :type '(repeat :value-to-internal (lambda (widget value)
				      (custom-split-regexp-maybe value))
		 :match (lambda (widget value)
			  (or (stringp value)
			      (widget-editable-list-match widget value)))
		 regexp))

(defcustom message-ignored-mail-headers
  "^[GF]cc:\\|^Resent-Fcc:\\|^Xref:\\|^X-Draft-From:\\|^X-Gnus-Agent-Meta-Information:"
  "*Regexp of headers to be removed unconditionally before mailing."
  :group 'message-mail
  :group 'message-headers
  :link '(custom-manual "(message)Mail Headers")
  :type 'regexp)

(defcustom message-ignored-supersedes-headers "^Path:\\|^Date\\|^NNTP-Posting-Host:\\|^Xref:\\|^Lines:\\|^Received:\\|^X-From-Line:\\|^X-Trace:\\|^X-Complaints-To:\\|Return-Path:\\|^Supersedes:\\|^NNTP-Posting-Date:\\|^X-Trace:\\|^X-Complaints-To:\\|^Cancel-Lock:\\|^Cancel-Key:\\|^X-Hashcash:\\|^X-Payment:"
  "*Header lines matching this regexp will be deleted before posting.
It's best to delete old Path and Date headers before posting to avoid
any confusion."
  :group 'message-interface
  :link '(custom-manual "(message)Superseding")
  :type '(repeat :value-to-internal (lambda (widget value)
				      (custom-split-regexp-maybe value))
		 :match (lambda (widget value)
			  (or (stringp value)
			      (widget-editable-list-match widget value)))
		 regexp))

(defcustom message-subject-re-regexp
  "^[ \t]*\\([Rr][Ee]\\(\\[[0-9]*\\]\\)*:[ \t]*\\)*[ \t]*"
  "*Regexp matching \"Re: \" in the subject line."
  :group 'message-various
  :link '(custom-manual "(message)Message Headers")
  :type 'regexp)

;;; Start of variables adopted from `message-utils.el'.

(defcustom message-subject-trailing-was-query 'ask
  "*What to do with trailing \"(was: <old subject>)\" in subject lines.
If nil, leave the subject unchanged.  If it is the symbol `ask', query
the user what do do.  In this case, the subject is matched against
`message-subject-trailing-was-ask-regexp'.  If
`message-subject-trailing-was-query' is t, always strip the trailing
old subject.  In this case, `message-subject-trailing-was-regexp' is
used."
  :version "22.1"
  :type '(choice (const :tag "never" nil)
		 (const :tag "always strip" t)
                 (const ask))
  :link '(custom-manual "(message)Message Headers")
  :group 'message-various)

(defcustom message-subject-trailing-was-ask-regexp
  "[ \t]*\\([[(]+[Ww][Aa][Ss][ \t]*.*[\])]+\\)"
  "*Regexp matching \"(was: <old subject>)\" in the subject line.

The function `message-strip-subject-trailing-was' uses this regexp if
`message-subject-trailing-was-query' is set to the symbol `ask'.  If
the variable is t instead of `ask', use
`message-subject-trailing-was-regexp' instead.

It is okay to create some false positives here, as the user is asked."
  :version "22.1"
  :group 'message-various
  :link '(custom-manual "(message)Message Headers")
  :type 'regexp)

(defcustom message-subject-trailing-was-regexp
  "[ \t]*\\((*[Ww][Aa][Ss]:[ \t]*.*)\\)"
  "*Regexp matching \"(was: <old subject>)\" in the subject line.

If `message-subject-trailing-was-query' is set to t, the subject is
matched against `message-subject-trailing-was-regexp' in
`message-strip-subject-trailing-was'.  You should use a regexp creating very
few false positives here."
  :version "22.1"
  :group 'message-various
  :link '(custom-manual "(message)Message Headers")
  :type 'regexp)

;;; marking inserted text

(defcustom message-mark-insert-begin
  "--8<---------------cut here---------------start------------->8---\n"
  "How to mark the beginning of some inserted text."
  :version "22.1"
  :type 'string
  :link '(custom-manual "(message)Insertion Variables")
  :group 'message-various)

(defcustom message-mark-insert-end
  "--8<---------------cut here---------------end--------------->8---\n"
  "How to mark the end of some inserted text."
  :version "22.1"
  :type 'string
  :link '(custom-manual "(message)Insertion Variables")
  :group 'message-various)

(defcustom message-archive-header "X-No-Archive: Yes\n"
  "Header to insert when you don't want your article to be archived.
Archives \(such as groups.google.com\) respect this header."
  :version "22.1"
  :type 'string
  :link '(custom-manual "(message)Header Commands")
  :group 'message-various)

(defcustom message-archive-note
  "X-No-Archive: Yes - save http://groups.google.com/"
  "Note to insert why you wouldn't want this posting archived.
If nil, don't insert any text in the body."
  :version "22.1"
  :type '(radio string (const nil))
  :link '(custom-manual "(message)Header Commands")
  :group 'message-various)

;;; Crossposts and Followups
;; inspired by JoH-followup-to by Jochem Huhman <joh  at gmx.de>
;; new suggestions by R. Weikusat <rw at another.de>

(defvar message-cross-post-old-target nil
  "Old target for cross-posts or follow-ups.")
(make-variable-buffer-local 'message-cross-post-old-target)

(defcustom message-cross-post-default t
  "When non-nil `message-cross-post-followup-to' will perform a crosspost.
If nil, `message-cross-post-followup-to' will only do a followup.  Note that
you can explicitly override this setting by calling
`message-cross-post-followup-to' with a prefix."
  :version "22.1"
  :type 'boolean
  :group 'message-various)

(defcustom message-cross-post-note "Crosspost & Followup-To: "
  "Note to insert before signature to notify of cross-post and follow-up."
  :version "22.1"
  :type 'string
  :group 'message-various)

(defcustom message-followup-to-note "Followup-To: "
  "Note to insert before signature to notify of follow-up only."
  :version "22.1"
  :type 'string
  :group 'message-various)

(defcustom message-cross-post-note-function 'message-cross-post-insert-note
  "Function to use to insert note about Crosspost or Followup-To.
The function will be called with four arguments.  The function should not only
insert a note, but also ensure old notes are deleted.  See the documentation
for `message-cross-post-insert-note'."
  :version "22.1"
  :type 'function
  :group 'message-various)

;;; End of variables adopted from `message-utils.el'.

;;;###autoload
(defcustom message-signature-separator "^-- *$"
  "Regexp matching the signature separator."
  :type 'regexp
  :link '(custom-manual "(message)Various Message Variables")
  :group 'message-various)

(defcustom message-elide-ellipsis "\n[...]\n\n"
  "*The string which is inserted for elided text."
  :type 'string
  :link '(custom-manual "(message)Various Commands")
  :group 'message-various)

(defcustom message-interactive t
  "Non-nil means when sending a message wait for and display errors.
nil means let mailer mail back a message to report errors."
  :group 'message-sending
  :group 'message-mail
  :link '(custom-manual "(message)Sending Variables")
  :type 'boolean)

(defcustom message-generate-new-buffers 'unique
  "*Non-nil means create a new message buffer whenever `message-setup' is called.
If this is a function, call that function with three parameters:  The type,
the to address and the group name.  (Any of these may be nil.)  The function
should return the new buffer name."
  :group 'message-buffers
  :link '(custom-manual "(message)Message Buffers")
  :type '(choice (const :tag "off" nil)
		 (const :tag "unique" unique)
		 (const :tag "unsent" unsent)
		 (function fun)))

(defcustom message-kill-buffer-on-exit nil
  "*Non-nil means that the message buffer will be killed after sending a message."
  :group 'message-buffers
  :link '(custom-manual "(message)Message Buffers")
  :type 'boolean)

(eval-when-compile
  (defvar gnus-local-organization))
(defcustom message-user-organization
  (or (and (boundp 'gnus-local-organization)
	   (stringp gnus-local-organization)
	   gnus-local-organization)
      (getenv "ORGANIZATION")
      t)
  "*String to be used as an Organization header.
If t, use `message-user-organization-file'."
  :group 'message-headers
  :type '(choice string
		 (const :tag "consult file" t)))

;;;###autoload
(defcustom message-user-organization-file "/usr/lib/news/organization"
  "*Local news organization file."
  :type 'file
  :link '(custom-manual "(message)News Headers")
  :group 'message-headers)

(defcustom message-make-forward-subject-function
  #'message-forward-subject-name-subject
  "*List of functions called to generate subject headers for forwarded messages.
The subject generated by the previous function is passed into each
successive function.

The provided functions are:

* `message-forward-subject-author-subject' Source of article (author or
      newsgroup), in brackets followed by the subject
* `message-forward-subject-name-subject' Source of article (name of author
      or newsgroup), in brackets followed by the subject
* `message-forward-subject-fwd' Subject of article with 'Fwd:' prepended
      to it."
  :group 'message-forwarding
  :link '(custom-manual "(message)Forwarding")
  :type '(radio (function-item message-forward-subject-author-subject)
		(function-item message-forward-subject-fwd)
		(function-item message-forward-subject-name-subject)
		(repeat :tag "List of functions" function)))

(defcustom message-forward-as-mime t
  "*Non-nil means forward messages as an inline/rfc822 MIME section.
Otherwise, directly inline the old message in the forwarded message."
  :version "21.1"
  :group 'message-forwarding
  :link '(custom-manual "(message)Forwarding")
  :type 'boolean)

(defcustom message-forward-show-mml 'best
  "*Non-nil means show forwarded messages as MML (decoded from MIME).
Otherwise, forwarded messages are unchanged.
Can also be the symbol `best' to indicate that MML should be
used, except when it is a bad idea to use MML.  One example where
it is a bad idea is when forwarding a signed or encrypted
message, because converting MIME to MML would invalidate the
digital signature."
  :version "21.1"
  :group 'message-forwarding
  :type '(choice (const :tag "use MML" t)
		 (const :tag "don't use MML " nil)
		 (const :tag "use MML when appropriate" best)))

(defcustom message-forward-before-signature t
  "*Non-nil means put forwarded message before signature, else after."
  :group 'message-forwarding
  :type 'boolean)

(defcustom message-wash-forwarded-subjects nil
  "*Non-nil means try to remove as much cruft as possible from the subject.
Done before generating the new subject of a forward."
  :group 'message-forwarding
  :link '(custom-manual "(message)Forwarding")
  :type 'boolean)

(defcustom message-ignored-resent-headers "^Return-receipt\\|^X-Gnus\\|^Gnus-Warning:\\|^>?From "
  "*All headers that match this regexp will be deleted when resending a message."
  :group 'message-interface
  :link '(custom-manual "(message)Resending")
  :type '(repeat :value-to-internal (lambda (widget value)
				      (custom-split-regexp-maybe value))
		 :match (lambda (widget value)
			  (or (stringp value)
			      (widget-editable-list-match widget value)))
		 regexp))

(defcustom message-forward-ignored-headers "^Content-Transfer-Encoding:\\|^X-Gnus"
  "*All headers that match this regexp will be deleted when forwarding a message."
  :version "21.1"
  :group 'message-forwarding
  :type '(repeat :value-to-internal (lambda (widget value)
				      (custom-split-regexp-maybe value))
		 :match (lambda (widget value)
			  (or (stringp value)
			      (widget-editable-list-match widget value)))
		 regexp))

(defcustom message-ignored-cited-headers "."
  "*Delete these headers from the messages you yank."
  :group 'message-insertion
  :link '(custom-manual "(message)Insertion Variables")
  :type 'regexp)

(defcustom message-cite-prefix-regexp
  (if (string-match "[[:digit:]]" "1") ;; support POSIX?
      "\\([ \t]*[-_.[:word:]]+>+\\|[ \t]*[]>|}+]\\)+"
    ;; ?-, ?_ or ?. MUST NOT be in syntax entry w.
    (let ((old-table (syntax-table))
	  non-word-constituents)
      (set-syntax-table text-mode-syntax-table)
      (setq non-word-constituents
	    (concat
	     (if (string-match "\\w" "-")  "" "-")
	     (if (string-match "\\w" "_")  "" "_")
	     (if (string-match "\\w" ".")  "" ".")))
      (set-syntax-table old-table)
      (if (equal non-word-constituents "")
	  "\\([ \t]*\\(\\w\\)+>+\\|[ \t]*[]>|}+]\\)+"
	(concat "\\([ \t]*\\(\\w\\|["
		non-word-constituents
		"]\\)+>+\\|[ \t]*[]>|}+]\\)+"))))
  "*Regexp matching the longest possible citation prefix on a line."
  :version "22.1"
  :group 'message-insertion
  :link '(custom-manual "(message)Insertion Variables")
  :type 'regexp)

(defcustom message-cancel-message "I am canceling my own article.\n"
  "Message to be inserted in the cancel message."
  :group 'message-interface
  :link '(custom-manual "(message)Canceling News")
  :type 'string)

;; Useful to set in site-init.el
;;;###autoload
(defcustom message-send-mail-function 'message-send-mail-with-sendmail
  "Function to call to send the current buffer as mail.
The headers should be delimited by a line whose contents match the
variable `mail-header-separator'.

Valid values include `message-send-mail-with-sendmail' (the default),
`message-send-mail-with-mh', `message-send-mail-with-qmail',
`message-smtpmail-send-it', `smtpmail-send-it' and `feedmail-send-it'.

See also `send-mail-function'."
  :type '(radio (function-item message-send-mail-with-sendmail)
		(function-item message-send-mail-with-mh)
		(function-item message-send-mail-with-qmail)
		(function-item message-smtpmail-send-it)
		(function-item smtpmail-send-it)
		(function-item feedmail-send-it)
		(function :tag "Other"))
  :group 'message-sending
  :link '(custom-manual "(message)Mail Variables")
  :group 'message-mail)

(defcustom message-send-news-function 'message-send-news
  "Function to call to send the current buffer as news.
The headers should be delimited by a line whose contents match the
variable `mail-header-separator'."
  :group 'message-sending
  :group 'message-news
  :link '(custom-manual "(message)News Variables")
  :type 'function)

(defcustom message-reply-to-function nil
  "If non-nil, function that should return a list of headers.
This function should pick out addresses from the To, Cc, and From headers
and respond with new To and Cc headers."
  :group 'message-interface
  :link '(custom-manual "(message)Reply")
  :type '(choice function (const nil)))

(defcustom message-wide-reply-to-function nil
  "If non-nil, function that should return a list of headers.
This function should pick out addresses from the To, Cc, and From headers
and respond with new To and Cc headers."
  :group 'message-interface
  :link '(custom-manual "(message)Wide Reply")
  :type '(choice function (const nil)))

(defcustom message-followup-to-function nil
  "If non-nil, function that should return a list of headers.
This function should pick out addresses from the To, Cc, and From headers
and respond with new To and Cc headers."
  :group 'message-interface
  :link '(custom-manual "(message)Followup")
  :type '(choice function (const nil)))

(defcustom message-use-followup-to 'ask
  "*Specifies what to do with Followup-To header.
If nil, always ignore the header.  If it is t, use its value, but
query before using the \"poster\" value.  If it is the symbol `ask',
always query the user whether to use the value.  If it is the symbol
`use', always use the value."
  :group 'message-interface
  :link '(custom-manual "(message)Followup")
  :type '(choice (const :tag "ignore" nil)
		 (const :tag "use & query" t)
		 (const use)
		 (const ask)))

(defcustom message-use-mail-followup-to 'use
  "*Specifies what to do with Mail-Followup-To header.
If nil, always ignore the header.  If it is the symbol `ask', always
query the user whether to use the value.  If it is the symbol `use',
always use the value."
  :version "22.1"
  :group 'message-interface
  :link '(custom-manual "(message)Mailing Lists")
  :type '(choice (const :tag "ignore" nil)
		 (const use)
		 (const ask)))

(defcustom message-subscribed-address-functions nil
  "*Specifies functions for determining list subscription.
If nil, do not attempt to determine list subscription with functions.
If non-nil, this variable contains a list of functions which return
regular expressions to match lists.  These functions can be used in
conjunction with `message-subscribed-regexps' and
`message-subscribed-addresses'."
  :version "22.1"
  :group 'message-interface
  :link '(custom-manual "(message)Mailing Lists")
  :type '(repeat sexp))

(defcustom message-subscribed-address-file nil
  "*A file containing addresses the user is subscribed to.
If nil, do not look at any files to determine list subscriptions.  If
non-nil, each line of this file should be a mailing list address."
  :version "22.1"
  :group 'message-interface
  :link '(custom-manual "(message)Mailing Lists")
  :type '(radio file (const nil)))

(defcustom message-subscribed-addresses nil
  "*Specifies a list of addresses the user is subscribed to.
If nil, do not use any predefined list subscriptions.  This list of
addresses can be used in conjunction with
`message-subscribed-address-functions' and `message-subscribed-regexps'."
  :version "22.1"
  :group 'message-interface
  :link '(custom-manual "(message)Mailing Lists")
  :type '(repeat string))

(defcustom message-subscribed-regexps nil
  "*Specifies a list of addresses the user is subscribed to.
If nil, do not use any predefined list subscriptions.  This list of
regular expressions can be used in conjunction with
`message-subscribed-address-functions' and `message-subscribed-addresses'."
  :version "22.1"
  :group 'message-interface
  :link '(custom-manual "(message)Mailing Lists")
  :type '(repeat regexp))

(defcustom message-allow-no-recipients 'ask
  "Specifies what to do when there are no recipients other than Gcc/Fcc.
If it is the symbol `always', the posting is allowed.  If it is the
symbol `never', the posting is not allowed.  If it is the symbol
`ask', you are prompted."
  :version "22.1"
  :group 'message-interface
  :link '(custom-manual "(message)Message Headers")
  :type '(choice (const always)
		 (const never)
		 (const ask)))

(defcustom message-sendmail-f-is-evil nil
  "*Non-nil means don't add \"-f username\" to the sendmail command line.
Doing so would be even more evil than leaving it out."
  :group 'message-sending
  :link '(custom-manual "(message)Mail Variables")
  :type 'boolean)

(defcustom message-sendmail-envelope-from nil
  "*Envelope-from when sending mail with sendmail.
If this is nil, use `user-mail-address'.  If it is the symbol
`header', use the From: header of the message."
  :version "22.1"
  :type '(choice (string :tag "From name")
		 (const :tag "Use From: header from message" header)
		 (const :tag "Use `user-mail-address'" nil))
  :link '(custom-manual "(message)Mail Variables")
  :group 'message-sending)

;; qmail-related stuff
(defcustom message-qmail-inject-program "/var/qmail/bin/qmail-inject"
  "Location of the qmail-inject program."
  :group 'message-sending
  :link '(custom-manual "(message)Mail Variables")
  :type 'file)

(defcustom message-qmail-inject-args nil
  "Arguments passed to qmail-inject programs.
This should be a list of strings, one string for each argument.  It
may also be a function.

For e.g., if you wish to set the envelope sender address so that bounces
go to the right place or to deal with listserv's usage of that address, you
might set this variable to '(\"-f\" \"you@some.where\")."
  :group 'message-sending
  :link '(custom-manual "(message)Mail Variables")
  :type '(choice (function)
		 (repeat string)))

(defvar message-cater-to-broken-inn t
  "Non-nil means Gnus should not fold the `References' header.
Folding `References' makes ancient versions of INN create incorrect
NOV lines.")

(eval-when-compile
  (defvar gnus-post-method)
  (defvar gnus-select-method))
(defcustom message-post-method
  (cond ((and (boundp 'gnus-post-method)
	      (listp gnus-post-method)
	      gnus-post-method)
	 gnus-post-method)
	((boundp 'gnus-select-method)
	 gnus-select-method)
	(t '(nnspool "")))
  "*Method used to post news.
Note that when posting from inside Gnus, for instance, this
variable isn't used."
  :group 'message-news
  :group 'message-sending
  ;; This should be the `gnus-select-method' widget, but that might
  ;; create a dependence to `gnus.el'.
  :type 'sexp)

;; FIXME: This should be a temporary workaround until someone implements a
;; proper solution.  If a crash happens while replying, the auto-save file
;; will *not* have a `References:' header if `message-generate-headers-first'
;; is nil.  See: http://article.gmane.org/gmane.emacs.gnus.general/51138
(defcustom message-generate-headers-first '(references)
  "Which headers should be generated before starting to compose a message.
If t, generate all required headers.  This can also be a list of headers to
generate.  The variables `message-required-news-headers' and
`message-required-mail-headers' specify which headers to generate.

Note that the variable `message-deletable-headers' specifies headers which
are to be deleted and then re-generated before sending, so this variable
will not have a visible effect for those headers."
  :group 'message-headers
  :link '(custom-manual "(message)Message Headers")
  :type '(choice (const :tag "None" nil)
                 (const :tag "References" '(references))
                 (const :tag "All" t)
                 (repeat (sexp :tag "Header"))))

(defcustom message-setup-hook nil
  "Normal hook, run each time a new outgoing message is initialized.
The function `message-setup' runs this hook."
  :group 'message-various
  :link '(custom-manual "(message)Various Message Variables")
  :type 'hook)

(defcustom message-cancel-hook nil
  "Hook run when cancelling articles."
  :group 'message-various
  :link '(custom-manual "(message)Various Message Variables")
  :type 'hook)

(defcustom message-signature-setup-hook nil
  "Normal hook, run each time a new outgoing message is initialized.
It is run after the headers have been inserted and before
the signature is inserted."
  :group 'message-various
  :link '(custom-manual "(message)Various Message Variables")
  :type 'hook)

(defcustom message-mode-hook nil
  "Hook run in message mode buffers."
  :group 'message-various
  :type 'hook)

(defcustom message-header-hook nil
  "Hook run in a message mode buffer narrowed to the headers."
  :group 'message-various
  :type 'hook)

(defcustom message-header-setup-hook nil
  "Hook called narrowed to the headers when setting up a message buffer."
  :group 'message-various
  :link '(custom-manual "(message)Various Message Variables")
  :type 'hook)

(defcustom message-minibuffer-local-map
  (let ((map (make-sparse-keymap 'message-minibuffer-local-map)))
    (set-keymap-parent map minibuffer-local-map)
    map)
  "Keymap for `message-read-from-minibuffer'."
  :version "22.1"
  :group 'message-various)

;;;###autoload
(defcustom message-citation-line-function 'message-insert-citation-line
  "*Function called to insert the \"Whomever writes:\" line.

Note that Gnus provides a feature where the reader can click on
`writes:' to hide the cited text.  If you change this line too much,
people who read your message will have to change their Gnus
configuration.  See the variable `gnus-cite-attribution-suffix'."
  :type 'function
  :link '(custom-manual "(message)Insertion Variables")
  :group 'message-insertion)

;;;###autoload
(defcustom message-yank-prefix "> "
  "*Prefix inserted on the lines of yanked messages.
Fix `message-cite-prefix-regexp' if it is set to an abnormal value.
See also `message-yank-cited-prefix'."
  :type 'string
  :link '(custom-manual "(message)Insertion Variables")
  :group 'message-insertion)

(defcustom message-yank-cited-prefix ">"
  "*Prefix inserted on cited or empty lines of yanked messages.
Fix `message-cite-prefix-regexp' if it is set to an abnormal value.
See also `message-yank-prefix'."
  :version "22.1"
  :type 'string
  :link '(custom-manual "(message)Insertion Variables")
  :group 'message-insertion)

(defcustom message-indentation-spaces 3
  "*Number of spaces to insert at the beginning of each cited line.
Used by `message-yank-original' via `message-yank-cite'."
  :group 'message-insertion
  :link '(custom-manual "(message)Insertion Variables")
  :type 'integer)

;;;###autoload
(defcustom message-cite-function 'message-cite-original
  "*Function for citing an original message.
Predefined functions include `message-cite-original' and
`message-cite-original-without-signature'.
Note that `message-cite-original' uses `mail-citation-hook' if that is non-nil."
  :type '(radio (function-item message-cite-original)
		(function-item message-cite-original-without-signature)
		(function-item sc-cite-original)
		(function :tag "Other"))
  :link '(custom-manual "(message)Insertion Variables")
  :group 'message-insertion)

;;;###autoload
(defcustom message-indent-citation-function 'message-indent-citation
  "*Function for modifying a citation just inserted in the mail buffer.
This can also be a list of functions.  Each function can find the
citation between (point) and (mark t).  And each function should leave
point and mark around the citation text as modified."
  :type 'function
  :link '(custom-manual "(message)Insertion Variables")
  :group 'message-insertion)

;;;###autoload
(defcustom message-signature t
  "*String to be inserted at the end of the message buffer.
If t, the `message-signature-file' file will be inserted instead.
If a function, the result from the function will be used instead.
If a form, the result from the form will be used instead."
  :type 'sexp
  :link '(custom-manual "(message)Insertion Variables")
  :group 'message-insertion)

;;;###autoload
(defcustom message-signature-file "~/.signature"
  "*Name of file containing the text inserted at end of message buffer.
Ignored if the named file doesn't exist.
If nil, don't insert a signature."
  :type '(choice file (const :tags "None" nil))
  :link '(custom-manual "(message)Insertion Variables")
  :group 'message-insertion)

;;;###autoload
(defcustom message-signature-insert-empty-line t
  "*If non-nil, insert an empty line before the signature separator."
  :version "22.1"
  :type 'boolean
  :link '(custom-manual "(message)Insertion Variables")
  :group 'message-insertion)

(defcustom message-distribution-function nil
  "*Function called to return a Distribution header."
  :group 'message-news
  :group 'message-headers
  :link '(custom-manual "(message)News Headers")
  :type '(choice function (const nil)))

(defcustom message-expires 14
  "Number of days before your article expires."
  :group 'message-news
  :group 'message-headers
  :link '(custom-manual "(message)News Headers")
  :type 'integer)

(defcustom message-user-path nil
  "If nil, use the NNTP server name in the Path header.
If stringp, use this; if non-nil, use no host name (user name only)."
  :group 'message-news
  :group 'message-headers
  :link '(custom-manual "(message)News Headers")
  :type '(choice (const :tag "nntp" nil)
		 (string :tag "name")
		 (sexp :tag "none" :format "%t" t)))

(defvar message-reply-buffer nil)
(defvar message-reply-headers nil
  "The headers of the current replied article.
It is a vector of the following headers:
\[number subject from date id references chars lines xref extra].")
(defvar message-newsreader nil)
(defvar message-mailer nil)
(defvar message-sent-message-via nil)
(defvar message-checksum nil)
(defvar message-send-actions nil
  "A list of actions to be performed upon successful sending of a message.")
(defvar message-exit-actions nil
  "A list of actions to be performed upon exiting after sending a message.")
(defvar message-kill-actions nil
  "A list of actions to be performed before killing a message buffer.")
(defvar message-postpone-actions nil
  "A list of actions to be performed after postponing a message.")

(define-widget 'message-header-lines 'text
  "All header lines must be LFD terminated."
  :format "%{%t%}:%n%v"
  :valid-regexp "^\\'"
  :error "All header lines must be newline terminated")

(defcustom message-default-headers ""
  "*A string containing header lines to be inserted in outgoing messages.
It is inserted before you edit the message, so you can edit or delete
these lines."
  :group 'message-headers
  :link '(custom-manual "(message)Message Headers")
  :type 'message-header-lines)

(defcustom message-default-mail-headers ""
  "*A string of header lines to be inserted in outgoing mails."
  :group 'message-headers
  :group 'message-mail
  :link '(custom-manual "(message)Mail Headers")
  :type 'message-header-lines)

(defcustom message-default-news-headers ""
  "*A string of header lines to be inserted in outgoing news articles."
  :group 'message-headers
  :group 'message-news
  :link '(custom-manual "(message)News Headers")
  :type 'message-header-lines)

;; Note: could use /usr/ucb/mail instead of sendmail;
;; options -t, and -v if not interactive.
(defcustom message-mailer-swallows-blank-line
  (if (and (string-match "sparc-sun-sunos\\(\\'\\|[^5]\\)"
			 system-configuration)
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
      ;; ASCII characters (i. e., characters that have decimal values between
      ;; 33 and 126, except colon)", i. e., any chars except ctl chars,
      ;; space, or colon.
      '(looking-at "[ \t]\\|[][!\"#$%&'()*+,-./0-9;<=>?@A-Z\\\\^_`a-z{|}~]+:"))
  "*Set this non-nil if the system's mailer runs the header and body together.
\(This problem exists on Sunos 4 when sendmail is run in remote mode.)
The value should be an expression to test whether the problem will
actually occur."
  :group 'message-sending
  :link '(custom-manual "(message)Mail Variables")
  :type 'sexp)

;;;###autoload
(define-mail-user-agent 'message-user-agent
  'message-mail 'message-send-and-exit
  'message-kill-buffer 'message-send-hook)

(defvar message-mh-deletable-headers '(Message-ID Date Lines Sender)
  "If non-nil, delete the deletable headers before feeding to mh.")

(defvar message-send-method-alist
  '((news message-news-p message-send-via-news)
    (mail message-mail-p message-send-via-mail))
  "Alist of ways to send outgoing messages.
Each element has the form

  \(TYPE PREDICATE FUNCTION)

where TYPE is a symbol that names the method; PREDICATE is a function
called without any parameters to determine whether the message is
a message of type TYPE; and FUNCTION is a function to be called if
PREDICATE returns non-nil.  FUNCTION is called with one parameter --
the prefix.")

(defcustom message-mail-alias-type 'abbrev
  "*What alias expansion type to use in Message buffers.
The default is `abbrev', which uses mailabbrev.  nil switches
mail aliases off."
  :group 'message
  :link '(custom-manual "(message)Mail Aliases")
  :type '(choice (const :tag "Use Mailabbrev" abbrev)
		 (const :tag "No expansion" nil)))

(defcustom message-auto-save-directory
  (file-name-as-directory (nnheader-concat message-directory "drafts"))
  "*Directory where Message auto-saves buffers if Gnus isn't running.
If nil, Message won't auto-save."
  :group 'message-buffers
  :link '(custom-manual "(message)Various Message Variables")
  :type '(choice directory (const :tag "Don't auto-save" nil)))

(defcustom message-default-charset
  (and (not (mm-multibyte-p)) 'iso-8859-1)
  "Default charset used in non-MULE Emacsen.
If nil, you might be asked to input the charset."
  :version "21.1"
  :group 'message
  :link '(custom-manual "(message)Various Message Variables")
  :type 'symbol)

(defcustom message-dont-reply-to-names
  (and (boundp 'rmail-dont-reply-to-names) rmail-dont-reply-to-names)
  "*A regexp specifying addresses to prune when doing wide replies.
A value of nil means exclude your own user name only."
  :version "21.1"
  :group 'message
  :link '(custom-manual "(message)Wide Reply")
  :type '(choice (const :tag "Yourself" nil)
		 regexp))

(defvar message-shoot-gnksa-feet nil
  "*A list of GNKSA feet you are allowed to shoot.
Gnus gives you all the opportunity you could possibly want for
shooting yourself in the foot.  Also, Gnus allows you to shoot the
feet of Good Net-Keeping Seal of Approval.  The following are foot
candidates:
`empty-article'     Allow you to post an empty article;
`quoted-text-only'  Allow you to post quoted text only;
`multiple-copies'   Allow you to post multiple copies;
`cancel-messages'   Allow you to cancel or supersede messages from
                    your other email addresses.")

(defsubst message-gnksa-enable-p (feature)
  (or (not (listp message-shoot-gnksa-feet))
      (memq feature message-shoot-gnksa-feet)))

(defcustom message-hidden-headers nil
  "Regexp of headers to be hidden when composing new messages.
This can also be a list of regexps to match headers.  Or a list
starting with `not' and followed by regexps."
  :version "22.1"
  :group 'message
  :link '(custom-manual "(message)Message Headers")
  :type '(repeat regexp))

;;; Internal variables.
;;; Well, not really internal.

(defvar message-mode-syntax-table
  (let ((table (copy-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?% ". " table)
    (modify-syntax-entry ?> ". " table)
    (modify-syntax-entry ?< ". " table)
    table)
  "Syntax table used while in Message mode.")

(defface message-header-to
  '((((class color)
      (background dark))
     (:foreground "green2" :bold t))
    (((class color)
      (background light))
     (:foreground "MidnightBlue" :bold t))
    (t
     (:bold t :italic t)))
  "Face used for displaying From headers."
  :group 'message-faces)
;; backward-compatibility alias
(put 'message-header-to-face 'face-alias 'message-header-to)

(defface message-header-cc
  '((((class color)
      (background dark))
     (:foreground "green4" :bold t))
    (((class color)
      (background light))
     (:foreground "MidnightBlue"))
    (t
     (:bold t)))
  "Face used for displaying Cc headers."
  :group 'message-faces)
;; backward-compatibility alias
(put 'message-header-cc-face 'face-alias 'message-header-cc)

(defface message-header-subject
  '((((class color)
      (background dark))
     (:foreground "green3"))
    (((class color)
      (background light))
     (:foreground "navy blue" :bold t))
    (t
     (:bold t)))
  "Face used for displaying subject headers."
  :group 'message-faces)
;; backward-compatibility alias
(put 'message-header-subject-face 'face-alias 'message-header-subject)

(defface message-header-newsgroups
  '((((class color)
      (background dark))
     (:foreground "yellow" :bold t :italic t))
    (((class color)
      (background light))
     (:foreground "blue4" :bold t :italic t))
    (t
     (:bold t :italic t)))
  "Face used for displaying newsgroups headers."
  :group 'message-faces)
;; backward-compatibility alias
(put 'message-header-newsgroups-face 'face-alias 'message-header-newsgroups)

(defface message-header-other
  '((((class color)
      (background dark))
     (:foreground "#b00000"))
    (((class color)
      (background light))
     (:foreground "steel blue"))
    (t
     (:bold t :italic t)))
  "Face used for displaying newsgroups headers."
  :group 'message-faces)
;; backward-compatibility alias
(put 'message-header-other-face 'face-alias 'message-header-other)

(defface message-header-name
  '((((class color)
      (background dark))
     (:foreground "DarkGreen"))
    (((class color)
      (background light))
     (:foreground "cornflower blue"))
    (t
     (:bold t)))
  "Face used for displaying header names."
  :group 'message-faces)
;; backward-compatibility alias
(put 'message-header-name-face 'face-alias 'message-header-name)

(defface message-header-xheader
  '((((class color)
      (background dark))
     (:foreground "blue"))
    (((class color)
      (background light))
     (:foreground "blue"))
    (t
     (:bold t)))
  "Face used for displaying X-Header headers."
  :group 'message-faces)
;; backward-compatibility alias
(put 'message-header-xheader-face 'face-alias 'message-header-xheader)

(defface message-separator
  '((((class color)
      (background dark))
     (:foreground "blue3"))
    (((class color)
      (background light))
     (:foreground "brown"))
    (t
     (:bold t)))
  "Face used for displaying the separator."
  :group 'message-faces)
;; backward-compatibility alias
(put 'message-separator-face 'face-alias 'message-separator)

(defface message-cited-text
  '((((class color)
      (background dark))
     (:foreground "red"))
    (((class color)
      (background light))
     (:foreground "red"))
    (t
     (:bold t)))
  "Face used for displaying cited text names."
  :group 'message-faces)
;; backward-compatibility alias
(put 'message-cited-text-face 'face-alias 'message-cited-text)

(defface message-mml
  '((((class color)
      (background dark))
     (:foreground "ForestGreen"))
    (((class color)
      (background light))
     (:foreground "ForestGreen"))
    (t
     (:bold t)))
  "Face used for displaying MML."
  :group 'message-faces)
;; backward-compatibility alias
(put 'message-mml-face 'face-alias 'message-mml)

(defun message-font-lock-make-header-matcher (regexp)
  (let ((form
	 `(lambda (limit)
	    (let ((start (point)))
	      (save-restriction
		(widen)
		(goto-char (point-min))
		(if (re-search-forward
		     (concat "^" (regexp-quote mail-header-separator) "$")
		     nil t)
		    (setq limit (min limit (match-beginning 0))))
		(goto-char start))
	      (and (< start limit)
		   (re-search-forward ,regexp limit t))))))
    (if (featurep 'bytecomp)
	(byte-compile form)
      form)))

(defvar message-font-lock-keywords
  (let ((content "[ \t]*\\(.+\\(\n[ \t].*\\)*\\)\n?"))
    `((,(message-font-lock-make-header-matcher
	 (concat "^\\([Tt]o:\\)" content))
       (1 'message-header-name)
       (2 'message-header-to nil t))
      (,(message-font-lock-make-header-matcher
	 (concat "^\\(^[GBF]?[Cc][Cc]:\\|^[Rr]eply-[Tt]o:\\)" content))
       (1 'message-header-name)
       (2 'message-header-cc nil t))
      (,(message-font-lock-make-header-matcher
	 (concat "^\\([Ss]ubject:\\)" content))
       (1 'message-header-name)
       (2 'message-header-subject nil t))
      (,(message-font-lock-make-header-matcher
	 (concat "^\\([Nn]ewsgroups:\\|Followup-[Tt]o:\\)" content))
       (1 'message-header-name)
       (2 'message-header-newsgroups nil t))
      (,(message-font-lock-make-header-matcher
	 (concat "^\\([A-Z][^: \n\t]+:\\)" content))
       (1 'message-header-name)
       (2 'message-header-other nil t))
      (,(message-font-lock-make-header-matcher
	 (concat "^\\(X-[A-Za-z0-9-]+:\\|In-Reply-To:\\)" content))
       (1 'message-header-name)
       (2 'message-header-name))
      ,@(if (and mail-header-separator
		 (not (equal mail-header-separator "")))
	    `((,(concat "^\\(" (regexp-quote mail-header-separator) "\\)$")
	       1 'message-separator))
	  nil)
      ((lambda (limit)
	 (re-search-forward (concat "^\\("
				    message-cite-prefix-regexp
				    "\\).*")
			    limit t))
       (0 'message-cited-text))
      ("<#/?\\(multipart\\|part\\|external\\|mml\\|secure\\)[^>]*>"
       (0 'message-mml))))
  "Additional expressions to highlight in Message mode.")


;; XEmacs does it like this.  For Emacs, we have to set the
;; `font-lock-defaults' buffer-local variable.
(put 'message-mode 'font-lock-defaults '(message-font-lock-keywords t))

(defvar message-face-alist
  '((bold . bold-region)
    (underline . underline-region)
    (default . (lambda (b e)
		 (unbold-region b e)
		 (ununderline-region b e))))
  "Alist of mail and news faces for facemenu.
The cdr of each entry is a function for applying the face to a region.")

(defcustom message-send-hook nil
  "Hook run before sending messages.
This hook is run quite early when sending."
  :group 'message-various
  :options '(ispell-message)
  :link '(custom-manual "(message)Various Message Variables")
  :type 'hook)

(defcustom message-send-mail-hook nil
  "Hook run before sending mail messages.
This hook is run very late -- just before the message is sent as
mail."
  :group 'message-various
  :link '(custom-manual "(message)Various Message Variables")
  :type 'hook)

(defcustom message-send-news-hook nil
  "Hook run before sending news messages.
This hook is run very late -- just before the message is sent as
news."
  :group 'message-various
  :link '(custom-manual "(message)Various Message Variables")
  :type 'hook)

(defcustom message-sent-hook nil
  "Hook run after sending messages."
  :group 'message-various
  :type 'hook)

(defvar message-send-coding-system 'binary
  "Coding system to encode outgoing mail.")

(defvar message-draft-coding-system
  mm-auto-save-coding-system
  "*Coding system to compose mail.
If you'd like to make it possible to share draft files between XEmacs
and Emacs, you may use `iso-2022-7bit' for this value at your own risk.
Note that the coding-system `iso-2022-7bit' isn't suitable to all data.")

(defcustom message-send-mail-partially-limit 1000000
  "The limitation of messages sent as message/partial.
The lower bound of message size in characters, beyond which the message
should be sent in several parts.  If it is nil, the size is unlimited."
  :version "21.1"
  :group 'message-buffers
  :link '(custom-manual "(message)Mail Variables")
  :type '(choice (const :tag "unlimited" nil)
		 (integer 1000000)))

(defcustom message-alternative-emails nil
  "A regexp to match the alternative email addresses.
The first matched address (not primary one) is used in the From field."
  :group 'message-headers
  :link '(custom-manual "(message)Message Headers")
  :type '(choice (const :tag "Always use primary" nil)
		 regexp))

(defcustom message-hierarchical-addresses nil
  "A list of hierarchical mail address definitions.

Inside each entry, the first address is the \"top\" address, and
subsequent addresses are subaddresses; this is used to indicate that
mail sent to the first address will automatically be delivered to the
subaddresses.  So if the first address appears in the recipient list
for a message, the subaddresses will be removed (if present) before
the mail is sent.  All addresses in this structure should be
downcased."
  :version "22.1"
  :group 'message-headers
  :type '(repeat (repeat string)))

(defcustom message-mail-user-agent nil
  "Like `mail-user-agent'.
Except if it is nil, use Gnus native MUA; if it is t, use
`mail-user-agent'."
  :version "22.1"
  :type '(radio (const :tag "Gnus native"
		       :format "%t\n"
		       nil)
		(const :tag "`mail-user-agent'"
		       :format "%t\n"
		       t)
		(function-item :tag "Default Emacs mail"
			       :format "%t\n"
			       sendmail-user-agent)
		(function-item :tag "Emacs interface to MH"
			       :format "%t\n"
			       mh-e-user-agent)
		(function :tag "Other"))
  :version "21.1"
  :group 'message)

(defcustom message-wide-reply-confirm-recipients nil
  "Whether to confirm a wide reply to multiple email recipients.
If this variable is nil, don't ask whether to reply to all recipients.
If this variable is non-nil, pose the question \"Reply to all
recipients?\" before a wide reply to multiple recipients.  If the user
answers yes, reply to all recipients as usual.  If the user answers
no, only reply back to the author."
  :version "22.1"
  :group 'message-headers
  :link '(custom-manual "(message)Wide Reply")
  :type 'boolean)

(defcustom message-user-fqdn nil
  "*Domain part of Message-Ids."
  :version "22.1"
  :group 'message-headers
  :link '(custom-manual "(message)News Headers")
  :type '(radio (const :format "%v  " nil)
		(string :format "FQDN: %v")))

(defcustom message-use-idna (and (condition-case nil (require 'idna)
				   (file-error))
				 (mm-coding-system-p 'utf-8)
				 (executable-find idna-program)
				 'ask)
  "Whether to encode non-ASCII in domain names into ASCII according to IDNA."
  :version "22.1"
  :group 'message-headers
  :link '(custom-manual "(message)IDNA")
  :type '(choice (const :tag "Ask" ask)
		 (const :tag "Never" nil)
		 (const :tag "Always" t)))

;;; Internal variables.

(defvar message-sending-message "Sending...")
(defvar message-buffer-list nil)
(defvar message-this-is-news nil)
(defvar message-this-is-mail nil)
(defvar message-draft-article nil)
(defvar message-mime-part nil)
(defvar message-posting-charset nil)
(defvar message-inserted-headers nil)

;; Byte-compiler warning
(eval-when-compile
  (defvar gnus-active-hashtb)
  (defvar gnus-read-active-file))

;;; Regexp matching the delimiter of messages in UNIX mail format
;;; (UNIX From lines), minus the initial ^.  It should be a copy
;;; of rmail.el's rmail-unix-mail-delimiter.
(defvar message-unix-mail-delimiter
  (let ((time-zone-regexp
	 (concat "\\([A-Z]?[A-Z]?[A-Z][A-Z]\\( DST\\)?"
		 "\\|[-+]?[0-9][0-9][0-9][0-9]"
		 "\\|"
		 "\\) *")))
    (concat
     "From "

     ;; Many things can happen to an RFC 822 mailbox before it is put into
     ;; a `From' line.  The leading phrase can be stripped, e.g.
     ;; `Joe <@w.x:joe@y.z>' -> `<@w.x:joe@y.z>'.  The <> can be stripped, e.g.
     ;; `<@x.y:joe@y.z>' -> `@x.y:joe@y.z'.  Everything starting with a CRLF
     ;; can be removed, e.g.
     ;;		From: joe@y.z (Joe	K
     ;;			User)
     ;; can yield `From joe@y.z (Joe	K Fri Mar 22 08:11:15 1996', and
     ;;		From: Joe User
     ;;			<joe@y.z>
     ;; can yield `From Joe User Fri Mar 22 08:11:15 1996'.
     ;; The mailbox can be removed or be replaced by white space, e.g.
     ;;		From: "Joe User"{space}{tab}
     ;;			<joe@y.z>
     ;; can yield `From {space}{tab} Fri Mar 22 08:11:15 1996',
     ;; where {space} and {tab} represent the Ascii space and tab characters.
     ;; We want to match the results of any of these manglings.
     ;; The following regexp rejects names whose first characters are
     ;; obviously bogus, but after that anything goes.
     "\\([^\0-\b\n-\r\^?].*\\)?"

     ;; The time the message was sent.
     "\\([^\0-\r \^?]+\\) +"		; day of the week
     "\\([^\0-\r \^?]+\\) +"		; month
     "\\([0-3]?[0-9]\\) +"		; day of month
     "\\([0-2][0-9]:[0-5][0-9]\\(:[0-6][0-9]\\)?\\) *" ; time of day

     ;; Perhaps a time zone, specified by an abbreviation, or by a
     ;; numeric offset.
     time-zone-regexp

     ;; The year.
     " \\([0-9][0-9]+\\) *"

     ;; On some systems the time zone can appear after the year, too.
     time-zone-regexp

     ;; Old uucp cruft.
     "\\(remote from .*\\)?"

     "\n"))
  "Regexp matching the delimiter of messages in UNIX mail format.")

(defvar message-unsent-separator
  (concat "^ *---+ +Unsent message follows +---+ *$\\|"
	  "^ *---+ +Returned message +---+ *$\\|"
	  "^Start of returned message$\\|"
	  "^ *---+ +Original message +---+ *$\\|"
	  "^ *--+ +begin message +--+ *$\\|"
	  "^ *---+ +Original message follows +---+ *$\\|"
	  "^ *---+ +Undelivered message follows +---+ *$\\|"
	  "^|? *---+ +Message text follows: +---+ *|?$")
  "A regexp that matches the separator before the text of a failed message.")

(defvar message-header-format-alist
  `((Newsgroups)
    (To . message-fill-address)
    (Cc . message-fill-address)
    (Subject)
    (In-Reply-To)
    (Fcc)
    (Bcc)
    (Date)
    (Organization)
    (Distribution)
    (Lines)
    (Expires)
    (Message-ID)
    (References . message-shorten-references)
    (User-Agent))
  "Alist used for formatting headers.")

(defvar	message-options nil
  "Some saved answers when sending message.")

(defvar message-send-mail-real-function nil
  "Internal send mail function.")

(defvar message-bogus-system-names "^localhost\\."
  "The regexp of bogus system names.")

(defcustom message-valid-fqdn-regexp
  (concat "[a-z0-9][-.a-z0-9]+\\." ;; [hostname.subdomain.]domain.
	  ;; valid TLDs:
	  "\\([a-z][a-z]" ;; two letter country TDLs
	  "\\|biz\\|com\\|edu\\|gov\\|int\\|mil\\|net\\|org"
	  "\\|aero\\|coop\\|info\\|name\\|museum"
	  "\\|arpa\\|pro\\|uucp\\|bitnet\\|bofh" ;; old style?
	  "\\)")
  "Regular expression that matches a valid FQDN."
  ;; see also: gnus-button-valid-fqdn-regexp
  :version "22.1"
  :group 'message-headers
  :type 'regexp)

(eval-and-compile
  (autoload 'idna-to-ascii "idna")
  (autoload 'message-setup-toolbar "messagexmas")
  (autoload 'mh-new-draft-name "mh-comp")
  (autoload 'mh-send-letter "mh-comp")
  (autoload 'gnus-point-at-eol "gnus-util")
  (autoload 'gnus-point-at-bol "gnus-util")
  (autoload 'gnus-output-to-rmail "gnus-util")
  (autoload 'gnus-output-to-mail "gnus-util")
  (autoload 'nndraft-request-associate-buffer "nndraft")
  (autoload 'nndraft-request-expire-articles "nndraft")
  (autoload 'gnus-open-server "gnus-int")
  (autoload 'gnus-request-post "gnus-int")
  (autoload 'gnus-alive-p "gnus-util")
  (autoload 'gnus-server-string "gnus")
  (autoload 'gnus-group-name-charset "gnus-group")
  (autoload 'gnus-group-name-decode "gnus-group")
  (autoload 'gnus-groups-from-server "gnus")
  (autoload 'rmail-output "rmailout")
  (autoload 'gnus-delay-article "gnus-delay")
  (autoload 'gnus-make-local-hook "gnus-util")
  (autoload 'gnus-extract-address-components "gnus-util"))



;;;
;;; Utility functions.
;;;

(defmacro message-y-or-n-p (question show &rest text)
  "Ask QUESTION, displaying remaining args in a temporary buffer if SHOW."
  `(message-talkative-question 'y-or-n-p ,question ,show ,@text))

(defmacro message-delete-line (&optional n)
  "Delete the current line (and the next N lines)."
  `(delete-region (progn (beginning-of-line) (point))
		  (progn (forward-line ,(or n 1)) (point))))

(defun message-mark-active-p ()
  "Non-nil means the mark and region are currently active in this buffer."
  mark-active)

(defun message-unquote-tokens (elems)
  "Remove double quotes (\") from strings in list ELEMS."
  (mapcar (lambda (item)
	    (while (string-match "^\\(.*\\)\"\\(.*\\)$" item)
	      (setq item (concat (match-string 1 item)
				 (match-string 2 item))))
	    item)
	  elems))

(defun message-tokenize-header (header &optional separator)
  "Split HEADER into a list of header elements.
SEPARATOR is a string of characters to be used as separators.  \",\"
is used by default."
  (if (not header)
      nil
    (let ((regexp (format "[%s]+" (or separator ",")))
	  (first t)
	  beg quoted elems paren)
      (with-temp-buffer
	(mm-enable-multibyte)
	(setq beg (point-min))
	(insert header)
	(goto-char (point-min))
	(while (not (eobp))
	  (if first
	      (setq first nil)
	    (forward-char 1))
	  (cond ((and (> (point) beg)
		      (or (eobp)
			  (and (looking-at regexp)
			       (not quoted)
			       (not paren))))
		 (push (buffer-substring beg (point)) elems)
		 (setq beg (match-end 0)))
		((eq (char-after) ?\")
		 (setq quoted (not quoted)))
		((and (eq (char-after) ?\()
		      (not quoted))
		 (setq paren t))
		((and (eq (char-after) ?\))
		      (not quoted))
		 (setq paren nil))))
	(nreverse elems)))))

(defun message-mail-file-mbox-p (file)
  "Say whether FILE looks like a Unix mbox file."
  (when (and (file-exists-p file)
	     (file-readable-p file)
	     (file-regular-p file))
    (with-temp-buffer
      (nnheader-insert-file-contents file)
      (goto-char (point-min))
      (looking-at message-unix-mail-delimiter))))

(defun message-fetch-field (header &optional not-all)
  "The same as `mail-fetch-field', only remove all newlines.
The buffer is expected to be narrowed to just the header of the message;
see `message-narrow-to-headers-or-head'."
  (let* ((inhibit-point-motion-hooks t)
	 (case-fold-search t)
	 (value (mail-fetch-field header nil (not not-all))))
    (when value
      (while (string-match "\n[\t ]+" value)
	(setq value (replace-match " " t t value)))
      (set-text-properties 0 (length value) nil value)
      value)))

(defun message-field-value (header &optional not-all)
  "The same as `message-fetch-field', only narrow to the headers first."
  (save-excursion
    (save-restriction
      (message-narrow-to-headers-or-head)
      (message-fetch-field header not-all))))

(defun message-narrow-to-field ()
  "Narrow the buffer to the header on the current line."
  (beginning-of-line)
  (narrow-to-region
   (point)
   (progn
     (forward-line 1)
     (if (re-search-forward "^[^ \n\t]" nil t)
	 (progn
	   (beginning-of-line)
	   (point))
       (point-max))))
  (goto-char (point-min)))

(defun message-add-header (&rest headers)
  "Add the HEADERS to the message header, skipping those already present."
  (while headers
    (let (hclean)
      (unless (string-match "^\\([^:]+\\):[ \t]*[^ \t]" (car headers))
	(error "Invalid header `%s'" (car headers)))
      (setq hclean (match-string 1 (car headers)))
      (save-restriction
	(message-narrow-to-headers)
	(unless (re-search-forward (concat "^" (regexp-quote hclean) ":") nil t)
	  (goto-char (point-max))
	  (if (string-match "\n$" (car headers))
	      (insert (car headers))
	    (insert (car headers) ?\n)))))
    (setq headers (cdr headers))))

(defmacro message-with-reply-buffer (&rest forms)
  "Evaluate FORMS in the reply buffer, if it exists."
  `(when (and message-reply-buffer
	      (buffer-name message-reply-buffer))
     (save-excursion
       (set-buffer message-reply-buffer)
       ,@forms)))

(put 'message-with-reply-buffer 'lisp-indent-function 0)
(put 'message-with-reply-buffer 'edebug-form-spec '(body))

(defun message-fetch-reply-field (header)
  "Fetch field HEADER from the message we're replying to."
  (message-with-reply-buffer
    (save-restriction
      (mail-narrow-to-head)
      (message-fetch-field header))))

(defun message-strip-list-identifiers (subject)
  "Remove list identifiers in `gnus-list-identifiers' from string SUBJECT."
  (require 'gnus-sum)			; for gnus-list-identifiers
  (let ((regexp (if (stringp gnus-list-identifiers)
		    gnus-list-identifiers
		  (mapconcat 'identity gnus-list-identifiers " *\\|"))))
    (if (string-match (concat "\\(\\(\\(Re: +\\)?\\(" regexp
			      " *\\)\\)+\\(Re: +\\)?\\)") subject)
	(concat (substring subject 0 (match-beginning 1))
		(or (match-string 3 subject)
		    (match-string 5 subject))
		(substring subject
			   (match-end 1)))
      subject)))

(defun message-strip-subject-re (subject)
  "Remove \"Re:\" from subject lines in string SUBJECT."
  (if (string-match message-subject-re-regexp subject)
      (substring subject (match-end 0))
    subject))

;;; Start of functions adopted from `message-utils.el'.

(defun message-strip-subject-trailing-was (subject)
  "Remove trailing \"(was: <old subject>)\" from SUBJECT lines.
Leading \"Re: \" is not stripped by this function.  Use the function
`message-strip-subject-re' for this."
  (let* ((query message-subject-trailing-was-query)
	 (new) (found))
    (setq found
	  (string-match
	   (if (eq query 'ask)
	       message-subject-trailing-was-ask-regexp
	     message-subject-trailing-was-regexp)
	   subject))
    (if found
	(setq new (substring subject 0 (match-beginning 0))))
    (if (or (not found) (eq query nil))
	subject
      (if (eq query 'ask)
	  (if (message-y-or-n-p
	       "Strip `(was: <old subject>)' in subject? " t
	       (concat
		"Strip `(was: <old subject>)' in subject "
		"and use the new one instead?\n\n"
		"Current subject is:   \""
		subject "\"\n\n"
		"New subject would be: \""
		new "\"\n\n"
		"See the variable `message-subject-trailing-was-query' "
		"to get rid of this query."
		))
	      new subject)
	new))))

;;; Suggested by Jonas Steverud  @  www.dtek.chalmers.se/~d4jonas/

;;;###autoload
(defun message-change-subject (new-subject)
  "Ask for NEW-SUBJECT header, append (was: <Old Subject>)."
  ;; <URL:http://www.landfield.com/usefor/drafts/draft-ietf-usefor-useage--1.02.unpaged>
  (interactive
   (list
    (read-from-minibuffer "New subject: ")))
  (cond ((and (not (or (null new-subject) ; new subject not empty
		       (zerop (string-width new-subject))
		       (string-match "^[ \t]*$" new-subject))))
	 (save-excursion
	   (let ((old-subject
		  (save-restriction
		    (message-narrow-to-headers)
		    (message-fetch-field "Subject"))))
	     (cond ((not old-subject)
		    (error "No current subject"))
		   ((not (string-match
			  (concat "^[ \t]*"
				  (regexp-quote new-subject)
				  " \t]*$")
			  old-subject))  ; yes, it really is a new subject
		    ;; delete eventual Re: prefix
		    (setq old-subject
			  (message-strip-subject-re old-subject))
		    (message-goto-subject)
		    (message-delete-line)
		    (insert (concat "Subject: "
				    new-subject
				    " (was: "
				    old-subject ")\n")))))))))

;;;###autoload
(defun message-mark-inserted-region (beg end)
  "Mark some region in the current article with enclosing tags.
See `message-mark-insert-begin' and `message-mark-insert-end'."
  (interactive "r")
  (save-excursion
    ;; add to the end of the region first, otherwise end would be invalid
    (goto-char end)
    (insert message-mark-insert-end)
    (goto-char beg)
    (insert message-mark-insert-begin)))

;;;###autoload
(defun message-mark-insert-file (file)
  "Insert FILE at point, marking it with enclosing tags.
See `message-mark-insert-begin' and `message-mark-insert-end'."
  (interactive "fFile to insert: ")
    ;; reverse insertion to get correct result.
  (let ((p (point)))
    (insert message-mark-insert-end)
    (goto-char p)
    (insert-file-contents file)
    (goto-char p)
    (insert message-mark-insert-begin)))

;;;###autoload
(defun message-add-archive-header ()
  "Insert \"X-No-Archive: Yes\" in the header and a note in the body.
The note can be customized using `message-archive-note'.  When called with a
prefix argument, ask for a text to insert.  If you don't want the note in the
body, set  `message-archive-note' to nil."
  (interactive)
  (if current-prefix-arg
      (setq message-archive-note
	    (read-from-minibuffer "Reason for No-Archive: "
				  (cons message-archive-note 0))))
    (save-excursion
      (if (message-goto-signature)
	  (re-search-backward message-signature-separator))
      (when message-archive-note
	(insert message-archive-note)
	(newline))
      (message-add-header message-archive-header)
      (message-sort-headers)))

;;;###autoload
(defun message-cross-post-followup-to-header (target-group)
  "Mangles FollowUp-To and Newsgroups header to point to TARGET-GROUP.
With prefix-argument just set Follow-Up, don't cross-post."
  (interactive
   (list ; Completion based on Gnus
    (completing-read "Followup To: "
		     (if (boundp 'gnus-newsrc-alist)
			 gnus-newsrc-alist)
		     nil nil '("poster" . 0)
		     (if (boundp 'gnus-group-history)
			 'gnus-group-history))))
  (message-remove-header "Follow[Uu]p-[Tt]o" t)
  (message-goto-newsgroups)
  (beginning-of-line)
  ;; if we already did a crosspost before, kill old target
  (if (and message-cross-post-old-target
	   (re-search-forward
	    (regexp-quote (concat "," message-cross-post-old-target))
	    nil t))
      (replace-match ""))
  ;; unless (followup is to poster or user explicitly asked not
  ;; to cross-post, or target-group is already in Newsgroups)
  ;; add target-group to Newsgroups line.
  (cond ((and (or
	       ;; def: cross-post, req:no
	       (and message-cross-post-default (not current-prefix-arg))
	       ;; def: no-cross-post, req:yes
	       (and (not message-cross-post-default) current-prefix-arg))
	      (not (string-match "poster" target-group))
	      (not (string-match (regexp-quote target-group)
				 (message-fetch-field "Newsgroups"))))
	 (end-of-line)
	 (insert (concat "," target-group))))
  (end-of-line) ; ensure Followup: comes after Newsgroups:
  ;; unless new followup would be identical to Newsgroups line
  ;; make a new Followup-To line
  (if (not (string-match (concat "^[ \t]*"
				 target-group
				 "[ \t]*$")
			 (message-fetch-field "Newsgroups")))
      (insert (concat "\nFollowup-To: " target-group)))
  (setq message-cross-post-old-target target-group))

;;;###autoload
(defun message-cross-post-insert-note (target-group cross-post in-old
						    old-groups)
  "Insert a in message body note about a set Followup or Crosspost.
If there have been previous notes, delete them.  TARGET-GROUP specifies the
group to Followup-To.  When CROSS-POST is t, insert note about
crossposting.  IN-OLD specifies whether TARGET-GROUP is a member of
OLD-GROUPS.  OLD-GROUPS lists the old-groups the posting would have
been made to before the user asked for a Crosspost."
  ;; start scanning body for previous uses
  (message-goto-signature)
  (let ((head (re-search-backward
	       (concat "^" mail-header-separator)
	       nil t))) ; just search in body
    (message-goto-signature)
    (while (re-search-backward
	    (concat "^" (regexp-quote message-cross-post-note) ".*")
	    head t)
      (message-delete-line))
    (message-goto-signature)
    (while (re-search-backward
	    (concat "^" (regexp-quote message-followup-to-note) ".*")
	    head t)
      (message-delete-line))
    ;; insert new note
    (if (message-goto-signature)
	(re-search-backward message-signature-separator))
    (if (or in-old
	    (not cross-post)
	    (string-match "^[ \t]*poster[ \t]*$" target-group))
	(insert (concat message-followup-to-note target-group "\n"))
      (insert (concat message-cross-post-note target-group "\n")))))

;;;###autoload
(defun message-cross-post-followup-to (target-group)
  "Crossposts message and set Followup-To to TARGET-GROUP.
With prefix-argument just set Follow-Up, don't cross-post."
  (interactive
   (list ; Completion based on Gnus
    (completing-read "Followup To: "
		     (if (boundp 'gnus-newsrc-alist)
			 gnus-newsrc-alist)
		     nil nil '("poster" . 0)
		     (if (boundp 'gnus-group-history)
			 'gnus-group-history))))
  (cond ((not (or (null target-group) ; new subject not empty
		  (zerop (string-width target-group))
		  (string-match "^[ \t]*$" target-group)))
	 (save-excursion
	   (let* ((old-groups (message-fetch-field "Newsgroups"))
		  (in-old (string-match
			   (regexp-quote target-group)
			   (or old-groups ""))))
	     ;; check whether target exactly matches old Newsgroups
	     (cond ((not old-groups)
		    (error "No current newsgroup"))
		   ((or (not in-old)
			(not (string-match
			      (concat "^[ \t]*"
				      (regexp-quote target-group)
				      "[ \t]*$")
			      old-groups)))
		    ;; yes, Newsgroups line must change
		    (message-cross-post-followup-to-header target-group)
		    ;; insert note whether we do cross-post or followup-to
		    (funcall message-cross-post-note-function
			     target-group
			     (if (or (and message-cross-post-default
					  (not current-prefix-arg))
				     (and (not message-cross-post-default)
					  current-prefix-arg)) t)
			     in-old old-groups))))))))

;;; Reduce To: to Cc: or Bcc: header

;;;###autoload
(defun message-reduce-to-to-cc ()
 "Replace contents of To: header with contents of Cc: or Bcc: header."
 (interactive)
 (let ((cc-content
	(save-restriction (message-narrow-to-headers)
			  (message-fetch-field "cc")))
       (bcc nil))
   (if (and (not cc-content)
	    (setq cc-content
		  (save-restriction
		    (message-narrow-to-headers)
		    (message-fetch-field "bcc"))))
       (setq bcc t))
   (cond (cc-content
	  (save-excursion
	    (message-goto-to)
	    (message-delete-line)
	    (insert (concat "To: " cc-content "\n"))
	    (save-restriction
	      (message-narrow-to-headers)
	      (message-remove-header (if bcc
					 "bcc"
				       "cc"))))))))

;;; End of functions adopted from `message-utils.el'.

(defun message-remove-header (header &optional is-regexp first reverse)
  "Remove HEADER in the narrowed buffer.
If IS-REGEXP, HEADER is a regular expression.
If FIRST, only remove the first instance of the header.
Return the number of headers removed."
  (goto-char (point-min))
  (let ((regexp (if is-regexp header (concat "^" (regexp-quote header) ":")))
	(number 0)
	(case-fold-search t)
	last)
    (while (and (not (eobp))
		(not last))
      (if (if reverse
	      (not (looking-at regexp))
	    (looking-at regexp))
	  (progn
	    (incf number)
	    (when first
	      (setq last t))
	    (delete-region
	     (point)
	     ;; There might be a continuation header, so we have to search
	     ;; until we find a new non-continuation line.
	     (progn
	       (forward-line 1)
	       (if (re-search-forward "^[^ \t]" nil t)
		   (goto-char (match-beginning 0))
		 (point-max)))))
	(forward-line 1)
	(if (re-search-forward "^[^ \t]" nil t)
	    (goto-char (match-beginning 0))
	  (goto-char (point-max)))))
    number))

(defun message-remove-first-header (header)
  "Remove the first instance of HEADER if there is more than one."
  (let ((count 0)
	(regexp (concat "^" (regexp-quote header) ":")))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
	(incf count)))
    (while (> count 1)
      (message-remove-header header nil t)
      (decf count))))

(defun message-narrow-to-headers ()
  "Narrow the buffer to the head of the message."
  (widen)
  (narrow-to-region
   (goto-char (point-min))
   (if (re-search-forward
	(concat "^" (regexp-quote mail-header-separator) "\n") nil t)
       (match-beginning 0)
     (point-max)))
  (goto-char (point-min)))

(defun message-narrow-to-head-1 ()
  "Like `message-narrow-to-head'.  Don't widen."
  (narrow-to-region
   (goto-char (point-min))
   (if (search-forward "\n\n" nil 1)
       (1- (point))
     (point-max)))
  (goto-char (point-min)))

(defun message-narrow-to-head ()
  "Narrow the buffer to the head of the message.
Point is left at the beginning of the narrowed-to region."
  (widen)
  (message-narrow-to-head-1))

(defun message-narrow-to-headers-or-head ()
  "Narrow the buffer to the head of the message."
  (widen)
  (narrow-to-region
   (goto-char (point-min))
   (cond
    ((re-search-forward
      (concat "^" (regexp-quote mail-header-separator) "\n") nil t)
     (match-beginning 0))
    ((search-forward "\n\n" nil t)
     (1- (point)))
    (t
     (point-max))))
  (goto-char (point-min)))

(defun message-news-p ()
  "Say whether the current buffer contains a news message."
  (and (not message-this-is-mail)
       (or message-this-is-news
	   (save-excursion
	     (save-restriction
	       (message-narrow-to-headers)
	       (and (message-fetch-field "newsgroups")
		    (not (message-fetch-field "posted-to"))))))))

(defun message-mail-p ()
  "Say whether the current buffer contains a mail message."
  (and (not message-this-is-news)
       (or message-this-is-mail
	   (save-excursion
	     (save-restriction
	       (message-narrow-to-headers)
	       (or (message-fetch-field "to")
		   (message-fetch-field "cc")
		   (message-fetch-field "bcc")))))))

(defun message-subscribed-p ()
  "Say whether we need to insert a MFT header."
  (or message-subscribed-regexps
      message-subscribed-addresses
      message-subscribed-address-file
      message-subscribed-address-functions))

(defun message-next-header ()
  "Go to the beginning of the next header."
  (beginning-of-line)
  (or (eobp) (forward-char 1))
  (not (if (re-search-forward "^[^ \t]" nil t)
	   (beginning-of-line)
	 (goto-char (point-max)))))

(defun message-sort-headers-1 ()
  "Sort the buffer as headers using `message-rank' text props."
  (goto-char (point-min))
  (require 'sort)
  (sort-subr
   nil 'message-next-header
   (lambda ()
     (message-next-header)
     (unless (bobp)
       (forward-char -1)))
   (lambda ()
     (or (get-text-property (point) 'message-rank)
	 10000))))

(defun message-sort-headers ()
  "Sort the headers of the current message according to `message-header-format-alist'."
  (interactive)
  (save-excursion
    (save-restriction
      (let ((max (1+ (length message-header-format-alist)))
	    rank)
	(message-narrow-to-headers)
	(while (re-search-forward "^[^ \n]+:" nil t)
	  (put-text-property
	   (match-beginning 0) (1+ (match-beginning 0))
	   'message-rank
	   (if (setq rank (length (memq (assq (intern (buffer-substring
						       (match-beginning 0)
						       (1- (match-end 0))))
					      message-header-format-alist)
					message-header-format-alist)))
	       (- max rank)
	     (1+ max)))))
      (message-sort-headers-1))))




;;;
;;; Message mode
;;;

;;; Set up keymap.

(defvar message-mode-map nil)

(unless message-mode-map
  (setq message-mode-map (make-keymap))
  (set-keymap-parent message-mode-map text-mode-map)
  (define-key message-mode-map "\C-c?" 'describe-mode)

  (define-key message-mode-map "\C-c\C-f\C-t" 'message-goto-to)
  (define-key message-mode-map "\C-c\C-f\C-o" 'message-goto-from)
  (define-key message-mode-map "\C-c\C-f\C-b" 'message-goto-bcc)
  (define-key message-mode-map "\C-c\C-f\C-w" 'message-goto-fcc)
  (define-key message-mode-map "\C-c\C-f\C-c" 'message-goto-cc)
  (define-key message-mode-map "\C-c\C-f\C-s" 'message-goto-subject)
  (define-key message-mode-map "\C-c\C-f\C-r" 'message-goto-reply-to)
  (define-key message-mode-map "\C-c\C-f\C-n" 'message-goto-newsgroups)
  (define-key message-mode-map "\C-c\C-f\C-d" 'message-goto-distribution)
  (define-key message-mode-map "\C-c\C-f\C-f" 'message-goto-followup-to)
  (define-key message-mode-map "\C-c\C-f\C-m" 'message-goto-mail-followup-to)
  (define-key message-mode-map "\C-c\C-f\C-k" 'message-goto-keywords)
  (define-key message-mode-map "\C-c\C-f\C-u" 'message-goto-summary)
  (define-key message-mode-map "\C-c\C-f\C-i"
    'message-insert-or-toggle-importance)
  (define-key message-mode-map "\C-c\C-f\C-a"
    'message-generate-unsubscribed-mail-followup-to)

  ;; modify headers (and insert notes in body)
  (define-key message-mode-map "\C-c\C-fs"    'message-change-subject)
  ;;
  (define-key message-mode-map "\C-c\C-fx"    'message-cross-post-followup-to)
  ;; prefix+message-cross-post-followup-to = same w/o cross-post
  (define-key message-mode-map "\C-c\C-ft"    'message-reduce-to-to-cc)
  (define-key message-mode-map "\C-c\C-fa"    'message-add-archive-header)
  ;; mark inserted text
  (define-key message-mode-map "\C-c\M-m" 'message-mark-inserted-region)
  (define-key message-mode-map "\C-c\M-f" 'message-mark-insert-file)

  (define-key message-mode-map "\C-c\C-b" 'message-goto-body)
  (define-key message-mode-map "\C-c\C-i" 'message-goto-signature)

  (define-key message-mode-map "\C-c\C-t" 'message-insert-to)
  (define-key message-mode-map "\C-c\C-fw" 'message-insert-wide-reply)
  (define-key message-mode-map "\C-c\C-n" 'message-insert-newsgroups)
  (define-key message-mode-map "\C-c\C-l" 'message-to-list-only)

  (define-key message-mode-map "\C-c\C-u" 'message-insert-or-toggle-importance)
  (define-key message-mode-map "\C-c\M-n"
    'message-insert-disposition-notification-to)

  (define-key message-mode-map "\C-c\C-y" 'message-yank-original)
  (define-key message-mode-map "\C-c\M-\C-y" 'message-yank-buffer)
  (define-key message-mode-map "\C-c\C-q" 'message-fill-yanked-message)
  (define-key message-mode-map "\C-c\C-w" 'message-insert-signature)
  (define-key message-mode-map "\C-c\M-h" 'message-insert-headers)
  (define-key message-mode-map "\C-c\C-r" 'message-caesar-buffer-body)
  (define-key message-mode-map "\C-c\C-o" 'message-sort-headers)
  (define-key message-mode-map "\C-c\M-r" 'message-rename-buffer)

  (define-key message-mode-map "\C-c\C-c" 'message-send-and-exit)
  (define-key message-mode-map "\C-c\C-s" 'message-send)
  (define-key message-mode-map "\C-c\C-k" 'message-kill-buffer)
  (define-key message-mode-map "\C-c\C-d" 'message-dont-send)
  (define-key message-mode-map "\C-c\n" 'gnus-delay-article)

  (define-key message-mode-map "\C-c\C-e" 'message-elide-region)
  (define-key message-mode-map "\C-c\C-v" 'message-delete-not-region)
  (define-key message-mode-map "\C-c\C-z" 'message-kill-to-signature)
  (define-key message-mode-map "\M-\r" 'message-newline-and-reformat)
  ;;(define-key message-mode-map "\M-q" 'message-fill-paragraph)
  (define-key message-mode-map [remap split-line]  'message-split-line)

  (define-key message-mode-map "\C-c\C-a" 'mml-attach-file)

  (define-key message-mode-map "\C-a" 'message-beginning-of-line)
  (define-key message-mode-map "\t" 'message-tab)
  (define-key message-mode-map "\M-;" 'comment-region))

(easy-menu-define
  message-mode-menu message-mode-map "Message Menu."
  `("Message"
    ["Yank Original" message-yank-original message-reply-buffer]
    ["Fill Yanked Message" message-fill-yanked-message t]
    ["Insert Signature" message-insert-signature t]
    ["Caesar (rot13) Message" message-caesar-buffer-body t]
    ["Caesar (rot13) Region" message-caesar-region (message-mark-active-p)]
    ["Elide Region" message-elide-region
     :active (message-mark-active-p)
     ,@(if (featurep 'xemacs) nil
	 '(:help "Replace text in region with an ellipsis"))]
    ["Delete Outside Region" message-delete-not-region
     :active (message-mark-active-p)
     ,@(if (featurep 'xemacs) nil
	 '(:help "Delete all quoted text outside region"))]
    ["Kill To Signature" message-kill-to-signature t]
    ["Newline and Reformat" message-newline-and-reformat t]
    ["Rename buffer" message-rename-buffer t]
    ["Spellcheck" ispell-message
     ,@(if (featurep 'xemacs) '(t)
	 '(:help "Spellcheck this message"))]
    "----"
    ["Insert Region Marked" message-mark-inserted-region
     :active (message-mark-active-p)
     ,@(if (featurep 'xemacs) nil
	 '(:help "Mark region with enclosing tags"))]
    ["Insert File Marked..." message-mark-insert-file
     ,@(if (featurep 'xemacs) '(t)
	 '(:help "Insert file at point marked with enclosing tags"))]
    "----"
    ["Send Message" message-send-and-exit
     ,@(if (featurep 'xemacs) '(t)
	 '(:help "Send this message"))]
    ["Postpone Message" message-dont-send
     ,@(if (featurep 'xemacs) '(t)
	 '(:help "File this draft message and exit"))]
    ["Send at Specific Time..." gnus-delay-article
     ,@(if (featurep 'xemacs) '(t)
	 '(:help "Ask, then arrange to send message at that time"))]
    ["Kill Message" message-kill-buffer
     ,@(if (featurep 'xemacs) '(t)
	 '(:help "Delete this message without sending"))]))

(easy-menu-define
  message-mode-field-menu message-mode-map ""
  `("Field"
    ["To" message-goto-to t]
    ["From" message-goto-from t]
    ["Subject" message-goto-subject t]
    ["Change subject..." message-change-subject t]
    ["Cc" message-goto-cc t]
    ["Bcc" message-goto-bcc t]
    ["Fcc" message-goto-fcc t]
    ["Reply-To" message-goto-reply-to t]
    ["Flag As Important" message-insert-importance-high
     ,@(if (featurep 'xemacs) '(t)
	 '(:help "Mark this message as important"))]
    ["Flag As Unimportant" message-insert-importance-low
     ,@(if (featurep 'xemacs) '(t)
	 '(:help "Mark this message as unimportant"))]
    ["Request Receipt"
     message-insert-disposition-notification-to
     ,@(if (featurep 'xemacs) '(t)
	 '(:help "Request a receipt notification"))]
    "----"
    ;; (typical) news stuff
    ["Summary" message-goto-summary t]
    ["Keywords" message-goto-keywords t]
    ["Newsgroups" message-goto-newsgroups t]
    ["Fetch Newsgroups" message-insert-newsgroups t]
    ["Followup-To" message-goto-followup-to t]
    ;; ["Followup-To (with note in body)" message-cross-post-followup-to t]
    ["Crosspost / Followup-To..." message-cross-post-followup-to t]
    ["Distribution" message-goto-distribution t]
    ["X-No-Archive:" message-add-archive-header t ]
    "----"
    ;; (typical) mailing-lists stuff
    ["Fetch To" message-insert-to
     ,@(if (featurep 'xemacs) '(t)
	 '(:help "Insert a To header that points to the author."))]
    ["Fetch To and Cc" message-insert-wide-reply
     ,@(if (featurep 'xemacs) '(t)
	 '(:help
	   "Insert To and Cc headers as if you were doing a wide reply."))]
    "----"
    ["Send to list only" message-to-list-only t]
    ["Mail-Followup-To" message-goto-mail-followup-to t]
    ["Unsubscribed list post" message-generate-unsubscribed-mail-followup-to
     ,@(if (featurep 'xemacs) '(t)
	 '(:help "Insert a reasonable `Mail-Followup-To:' header."))]
    ["Reduce To: to Cc:" message-reduce-to-to-cc t]
    "----"
    ["Sort Headers" message-sort-headers t]
    ["Encode non-ASCII domain names" message-idna-to-ascii-rhs t]
    ["Goto Body" message-goto-body t]
    ["Goto Signature" message-goto-signature t]))

(defvar message-tool-bar-map nil)

(eval-when-compile
  (defvar facemenu-add-face-function)
  (defvar facemenu-remove-face-function))

;;; Forbidden properties
;;
;; We use `after-change-functions' to keep special text properties
;; that interfer with the normal function of message mode out of the
;; buffer.

(defcustom message-strip-special-text-properties t
  "Strip special properties from the message buffer.

Emacs has a number of special text properties which can break message
composing in various ways.  If this option is set, message will strip
these properties from the message composition buffer.  However, some
packages requires these properties to be present in order to work.
If you use one of these packages, turn this option off, and hope the
message composition doesn't break too bad."
  :version "22.1"
  :group 'message-various
  :link '(custom-manual "(message)Various Message Variables")
  :type 'boolean)

(defconst message-forbidden-properties
  ;; No reason this should be clutter up customize.  We make it a
  ;; property list (rather than a list of property symbols), to be
  ;; directly useful for `remove-text-properties'.
  '(field nil read-only nil invisible nil intangible nil
	  mouse-face nil modification-hooks nil insert-in-front-hooks nil
	  insert-behind-hooks nil point-entered nil point-left nil)
  ;; Other special properties:
  ;; category, face, display: probably doesn't do any harm.
  ;; fontified: is used by font-lock.
  ;; syntax-table, local-map: I dunno.
  ;; We need to add XEmacs names to the list.
  "Property list of with properties forbidden in message buffers.
The values of the properties are ignored, only the property names are used.")

(defun message-tamago-not-in-use-p (pos)
  "Return t when tamago version 4 is not in use at the cursor position.
Tamago version 4 is a popular input method for writing Japanese text.
It uses the properties `intangible', `invisible', `modification-hooks'
and `read-only' when translating ascii or kana text to kanji text.
These properties are essential to work, so we should never strip them."
  (not (and (boundp 'egg-modefull-mode)
	    (symbol-value 'egg-modefull-mode)
	    (or (memq (get-text-property pos 'intangible)
		      '(its-part-1 its-part-2))
		(get-text-property pos 'egg-end)
		(get-text-property pos 'egg-lang)
		(get-text-property pos 'egg-start)))))

(defun message-strip-forbidden-properties (begin end &optional old-length)
  "Strip forbidden properties between BEGIN and END, ignoring the third arg.
This function is intended to be called from `after-change-functions'.
See also `message-forbidden-properties'."
  (when (and message-strip-special-text-properties
	     (message-tamago-not-in-use-p begin))
    (let ((buffer-read-only nil)
	  (inhibit-read-only t))
      (while (not (= begin end))
	(when (not (get-text-property begin 'message-hidden))
	  (remove-text-properties begin (1+ begin)
				  message-forbidden-properties))
	(incf begin)))))

;;;###autoload
(define-derived-mode message-mode text-mode "Message"
  "Major mode for editing mail and news to be sent.
Like Text Mode but with these additional commands:\\<message-mode-map>
C-c C-s  `message-send' (send the message)  C-c C-c  `message-send-and-exit'
C-c C-d  Postpone sending the message       C-c C-k  Kill the message
C-c C-f  move to a header field (and create it if there isn't):
	 C-c C-f C-t  move to To	C-c C-f C-s  move to Subject
	 C-c C-f C-c  move to Cc	C-c C-f C-b  move to Bcc
	 C-c C-f C-w  move to Fcc	C-c C-f C-r  move to Reply-To
	 C-c C-f C-u  move to Summary	C-c C-f C-n  move to Newsgroups
	 C-c C-f C-k  move to Keywords	C-c C-f C-d  move to Distribution
         C-c C-f C-o  move to From (\"Originator\")
	 C-c C-f C-f  move to Followup-To
	 C-c C-f C-m  move to Mail-Followup-To
	 C-c C-f C-i  cycle through Importance values
	 C-c C-f s    change subject and append \"(was: <Old Subject>)\"
	 C-c C-f x    crossposting with FollowUp-To header and note in body
	 C-c C-f t    replace To: header with contents of Cc: or Bcc:
	 C-c C-f a    Insert X-No-Archive: header and a note in the body
C-c C-t  `message-insert-to' (add a To header to a news followup)
C-c C-l  `message-to-list-only' (removes all but list address in to/cc)
C-c C-n  `message-insert-newsgroups' (add a Newsgroup header to a news reply)
C-c C-b  `message-goto-body' (move to beginning of message text).
C-c C-i  `message-goto-signature' (move to the beginning of the signature).
C-c C-w  `message-insert-signature' (insert `message-signature-file' file).
C-c C-y  `message-yank-original' (insert current message, if any).
C-c C-q  `message-fill-yanked-message' (fill what was yanked).
C-c C-e  `message-elide-region' (elide the text between point and mark).
C-c C-v  `message-delete-not-region' (remove the text outside the region).
C-c C-z  `message-kill-to-signature' (kill the text up to the signature).
C-c C-r  `message-caesar-buffer-body' (rot13 the message body).
C-c C-a  `mml-attach-file' (attach a file as MIME).
C-c C-u  `message-insert-or-toggle-importance'  (insert or cycle importance).
C-c M-n  `message-insert-disposition-notification-to'  (request receipt).
C-c M-m  `message-mark-inserted-region' (mark region with enclosing tags).
C-c M-f  `message-mark-insert-file' (insert file marked with enclosing tags).
M-RET    `message-newline-and-reformat' (break the line and reformat)."
  (setq local-abbrev-table text-mode-abbrev-table)
  (set (make-local-variable 'message-reply-buffer) nil)
  (set (make-local-variable 'message-inserted-headers) nil)
  (set (make-local-variable 'message-send-actions) nil)
  (set (make-local-variable 'message-exit-actions) nil)
  (set (make-local-variable 'message-kill-actions) nil)
  (set (make-local-variable 'message-postpone-actions) nil)
  (set (make-local-variable 'message-draft-article) nil)
  (setq buffer-offer-save t)
  (set (make-local-variable 'facemenu-add-face-function)
       (lambda (face end)
	 (let ((face-fun (cdr (assq face message-face-alist))))
	   (if face-fun
	       (funcall face-fun (point) end)
	     (error "Face %s not configured for %s mode" face mode-name)))
	 ""))
  (set (make-local-variable 'facemenu-remove-face-function) t)
  (set (make-local-variable 'message-reply-headers) nil)
  (make-local-variable 'message-newsreader)
  (make-local-variable 'message-mailer)
  (make-local-variable 'message-post-method)
  (set (make-local-variable 'message-sent-message-via) nil)
  (set (make-local-variable 'message-checksum) nil)
  (set (make-local-variable 'message-mime-part) 0)
  (message-setup-fill-variables)
  ;; Allow using comment commands to add/remove quoting.
  ;; (set (make-local-variable 'comment-start) message-yank-prefix)
  (when message-yank-prefix
    (set (make-local-variable 'comment-start) message-yank-prefix)
    (set (make-local-variable 'comment-start-skip)
	 (concat "^" (regexp-quote message-yank-prefix) "[ \t]*")))
  (if (featurep 'xemacs)
      (message-setup-toolbar)
    (set (make-local-variable 'font-lock-defaults)
	 '(message-font-lock-keywords t))
    (if (boundp 'tool-bar-map)
	(set (make-local-variable 'tool-bar-map) (message-tool-bar-map))))
  (easy-menu-add message-mode-menu message-mode-map)
  (easy-menu-add message-mode-field-menu message-mode-map)
  (gnus-make-local-hook 'after-change-functions)
  ;; Mmmm... Forbidden properties...
  (add-hook 'after-change-functions 'message-strip-forbidden-properties
	    nil 'local)
  ;; Allow mail alias things.
  (when (eq message-mail-alias-type 'abbrev)
    (if (fboundp 'mail-abbrevs-setup)
	(mail-abbrevs-setup)
      (if (fboundp 'mail-aliases-setup)	; warning avoidance
	  (mail-aliases-setup))))
  (unless buffer-file-name
    (message-set-auto-save-file-name))
  (unless (buffer-base-buffer)
    ;; Don't enable multibyte on an indirect buffer.  Maybe enabling
    ;; multibyte is not necessary at all. -- zsh
    (mm-enable-multibyte))
  (set (make-local-variable 'indent-tabs-mode) nil) ;No tabs for indentation.
  (mml-mode))

(defun message-setup-fill-variables ()
  "Setup message fill variables."
  (set (make-local-variable 'fill-paragraph-function)
       'message-fill-paragraph)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'adaptive-fill-regexp)
  (unless (boundp 'adaptive-fill-first-line-regexp)
    (setq adaptive-fill-first-line-regexp nil))
  (make-local-variable 'adaptive-fill-first-line-regexp)
  (let ((quote-prefix-regexp
	 ;; User should change message-cite-prefix-regexp if
	 ;; message-yank-prefix is set to an abnormal value.
	 (concat "\\(" message-cite-prefix-regexp "\\)[ \t]*")))
    (setq paragraph-start
	  (concat
	   (regexp-quote mail-header-separator) "$\\|"
	   "[ \t]*$\\|"			; blank lines
	   "-- $\\|"			; signature delimiter
	   "---+$\\|"		   ; delimiters for forwarded messages
	   page-delimiter "$\\|"	; spoiler warnings
	   ".*wrote:$\\|"		; attribution lines
	   quote-prefix-regexp "$\\|"	; empty lines in quoted text
					; mml tags
	   "<#!*/?\\(multipart\\|part\\|external\\|mml\\|secure\\)"))
    (setq paragraph-separate paragraph-start)
    (setq adaptive-fill-regexp
	  (concat quote-prefix-regexp "\\|" adaptive-fill-regexp))
    (setq adaptive-fill-first-line-regexp
	  (concat quote-prefix-regexp "\\|"
		  adaptive-fill-first-line-regexp)))
  (make-local-variable 'auto-fill-inhibit-regexp)
  ;;(setq auto-fill-inhibit-regexp "^[A-Z][^: \n\t]+:")
  (setq auto-fill-inhibit-regexp nil)
  (make-local-variable 'normal-auto-fill-function)
  (setq normal-auto-fill-function 'message-do-auto-fill)
  ;; KLUDGE: auto fill might already be turned on in `text-mode-hook'.
  ;; In that case, ensure that it uses the right function.  The real
  ;; solution would be not to use `define-derived-mode', and run
  ;; `text-mode-hook' ourself at the end of the mode.
  ;; -- Per Abrahamsen <abraham@dina.kvl.dk> Date: 2001-10-19.
  (when auto-fill-function
    (setq auto-fill-function normal-auto-fill-function)))



;;;
;;; Message mode commands
;;;

;;; Movement commands

(defun message-goto-to ()
  "Move point to the To header."
  (interactive)
  (message-position-on-field "To"))

(defun message-goto-from ()
  "Move point to the From header."
  (interactive)
  (message-position-on-field "From"))

(defun message-goto-subject ()
  "Move point to the Subject header."
  (interactive)
  (message-position-on-field "Subject"))

(defun message-goto-cc ()
  "Move point to the Cc header."
  (interactive)
  (message-position-on-field "Cc" "To"))

(defun message-goto-bcc ()
  "Move point to the Bcc  header."
  (interactive)
  (message-position-on-field "Bcc" "Cc" "To"))

(defun message-goto-fcc ()
  "Move point to the Fcc header."
  (interactive)
  (message-position-on-field "Fcc" "To" "Newsgroups"))

(defun message-goto-reply-to ()
  "Move point to the Reply-To header."
  (interactive)
  (message-position-on-field "Reply-To" "Subject"))

(defun message-goto-newsgroups ()
  "Move point to the Newsgroups header."
  (interactive)
  (message-position-on-field "Newsgroups"))

(defun message-goto-distribution ()
  "Move point to the Distribution header."
  (interactive)
  (message-position-on-field "Distribution"))

(defun message-goto-followup-to ()
  "Move point to the Followup-To header."
  (interactive)
  (message-position-on-field "Followup-To" "Newsgroups"))

(defun message-goto-mail-followup-to ()
  "Move point to the Mail-Followup-To header."
  (interactive)
  (message-position-on-field "Mail-Followup-To" "To"))

(defun message-goto-keywords ()
  "Move point to the Keywords header."
  (interactive)
  (message-position-on-field "Keywords" "Subject"))

(defun message-goto-summary ()
  "Move point to the Summary header."
  (interactive)
  (message-position-on-field "Summary" "Subject"))

(defun message-goto-body (&optional interactivep)
  "Move point to the beginning of the message body."
  (interactive (list t))
  (when (and interactivep
	     (looking-at "[ \t]*\n"))
    (expand-abbrev))
  (goto-char (point-min))
  (or (search-forward (concat "\n" mail-header-separator "\n") nil t)
      (search-forward-regexp "[^:]+:\\([^\n]\\|\n[ \t]\\)+\n\n" nil t)))

(defun message-goto-eoh ()
  "Move point to the end of the headers."
  (interactive)
  (message-goto-body)
  (forward-line -1))

(defun message-goto-signature ()
  "Move point to the beginning of the message signature.
If there is no signature in the article, go to the end and
return nil."
  (interactive)
  (goto-char (point-min))
  (if (re-search-forward message-signature-separator nil t)
      (forward-line 1)
    (goto-char (point-max))
    nil))

(defun message-generate-unsubscribed-mail-followup-to (&optional include-cc)
  "Insert a reasonable MFT header in a post to an unsubscribed list.
When making original posts to a mailing list you are not subscribed to,
you have to type in a MFT header by hand.  The contents, usually, are
the addresses of the list and your own address.  This function inserts
such a header automatically.  It fetches the contents of the To: header
in the current mail buffer, and appends the current `user-mail-address'.

If the optional argument INCLUDE-CC is non-nil, the addresses in the
Cc: header are also put into the MFT."

  (interactive "P")
  (let* (cc tos)
    (save-restriction
      (message-narrow-to-headers)
      (message-remove-header "Mail-Followup-To")
      (setq cc (and include-cc (message-fetch-field "Cc")))
      (setq tos (if cc
		    (concat (message-fetch-field "To") "," cc)
		  (message-fetch-field "To"))))
    (message-goto-mail-followup-to)
    (insert (concat tos ", " user-mail-address))))



(defun message-insert-to (&optional force)
  "Insert a To header that points to the author of the article being replied to.
If the original author requested not to be sent mail, don't insert unless the
prefix FORCE is given."
  (interactive "P")
  (let* ((mct (message-fetch-reply-field "mail-copies-to"))
         (dont (and mct (or (equal (downcase mct) "never")
			    (equal (downcase mct) "nobody"))))
         (to (or (message-fetch-reply-field "mail-reply-to")
                 (message-fetch-reply-field "reply-to")
                 (message-fetch-reply-field "from"))))
    (when (and dont to)
      (message
       (if force
	   "Ignoring the user request not to have copies sent via mail"
	 "Complying with the user request not to have copies sent via mail")))
    (when (and force (not to))
      (error "No mail address in the article"))
    (when (and to (or force (not dont)))
      (message-carefully-insert-headers (list (cons 'To to))))))

(defun message-insert-wide-reply ()
  "Insert To and Cc headers as if you were doing a wide reply."
  (interactive)
  (let ((headers (message-with-reply-buffer
		   (message-get-reply-headers t))))
    (message-carefully-insert-headers headers)))

(defcustom message-header-synonyms
  '((To Cc Bcc))
  "List of lists of header synonyms.
E.g., if this list contains a member list with elements `Cc' and `To',
then `message-carefully-insert-headers' will not insert a `To' header
when the message is already `Cc'ed to the recipient."
  :version "22.1"
  :group 'message-headers
  :link '(custom-manual "(message)Message Headers")
  :type '(repeat sexp))

(defun message-carefully-insert-headers (headers)
  "Insert the HEADERS, an alist, into the message buffer.
Does not insert the headers when they are already present there
or in the synonym headers, defined by `message-header-synonyms'."
  ;; FIXME: Should compare only the address and not the full name.  Comparison
  ;; should be done case-folded (and with `string=' rather than
  ;; `string-match').
  ;; (mail-strip-quoted-names "Foo Bar <foo@bar>, bla@fasel (Bla Fasel)")
  (dolist (header headers)
    (let* ((header-name (symbol-name (car header)))
           (new-header (cdr header))
           (synonyms (loop for synonym in message-header-synonyms
			   when (memq (car header) synonym) return synonym))
           (old-header
            (loop for synonym in synonyms
		  for old-header = (mail-fetch-field (symbol-name synonym))
		  when (and old-header (string-match new-header old-header))
		  return synonym)))
      (if old-header
          (message "already have `%s' in `%s'" new-header old-header)
	(when (and (message-position-on-field header-name)
                   (setq old-header (mail-fetch-field header-name))
                   (not (string-match "\\` *\\'" old-header)))
	  (insert ", "))
        (insert new-header)))))

(defun message-widen-reply ()
  "Widen the reply to include maximum recipients."
  (interactive)
  (let ((follow-to
	 (and message-reply-buffer
	      (buffer-name message-reply-buffer)
	      (save-excursion
		(set-buffer message-reply-buffer)
		(message-get-reply-headers t)))))
    (save-excursion
      (save-restriction
	(message-narrow-to-headers)
	(dolist (elem follow-to)
	  (message-remove-header (symbol-name (car elem)))
	  (goto-char (point-min))
	  (insert (symbol-name (car elem)) ": "
		  (cdr elem) "\n"))))))

(defun message-insert-newsgroups ()
  "Insert the Newsgroups header from the article being replied to."
  (interactive)
  (when (and (message-position-on-field "Newsgroups")
	     (mail-fetch-field "newsgroups")
	     (not (string-match "\\` *\\'" (mail-fetch-field "newsgroups"))))
    (insert ","))
  (insert (or (message-fetch-reply-field "newsgroups") "")))



;;; Various commands

(defun message-delete-not-region (beg end)
  "Delete everything in the body of the current message outside of the region."
  (interactive "r")
  (let (citeprefix)
    (save-excursion
      (goto-char beg)
      ;; snarf citation prefix, if appropriate
      (unless (eq (point) (progn (beginning-of-line) (point)))
	(when (looking-at message-cite-prefix-regexp)
	  (setq citeprefix (match-string 0))))
      (goto-char end)
      (delete-region (point) (if (not (message-goto-signature))
				 (point)
			       (forward-line -2)
			       (point)))
      (insert "\n")
      (goto-char beg)
      (delete-region beg (progn (message-goto-body)
				(forward-line 2)
				(point)))
      (when citeprefix
	(insert citeprefix))))
  (when (message-goto-signature)
    (forward-line -2)))

(defun message-kill-to-signature ()
  "Deletes all text up to the signature."
  (interactive)
  (let ((point (point)))
    (message-goto-signature)
    (unless (eobp)
      (end-of-line -1))
    (kill-region point (point))
    (unless (bolp)
      (insert "\n"))))

(defun message-newline-and-reformat (&optional arg not-break)
  "Insert four newlines, and then reformat if inside quoted text.
Prefix arg means justify as well."
  (interactive (list (if current-prefix-arg 'full)))
  (let (quoted point beg end leading-space bolp)
    (setq point (point))
    (beginning-of-line)
    (setq beg (point))
    (setq bolp (= beg point))
    ;; Find first line of the paragraph.
    (if not-break
	(while (and (not (eobp))
		    (not (looking-at message-cite-prefix-regexp))
		    (looking-at paragraph-start))
	  (forward-line 1)))
    ;; Find the prefix
    (when (looking-at message-cite-prefix-regexp)
      (setq quoted (match-string 0))
      (goto-char (match-end 0))
      (looking-at "[ \t]*")
      (setq leading-space (match-string 0)))
    (if (and quoted
	     (not not-break)
	     (not bolp)
	     (< (- point beg) (length quoted)))
	;; break inside the cite prefix.
	(setq quoted nil
	      end nil))
    (if quoted
	(progn
	  (forward-line 1)
	  (while (and (not (eobp))
		      (not (looking-at paragraph-separate))
		      (looking-at message-cite-prefix-regexp)
		      (equal quoted (match-string 0)))
	    (goto-char (match-end 0))
	    (looking-at "[ \t]*")
	    (if (> (length leading-space) (length (match-string 0)))
		(setq leading-space (match-string 0)))
	    (forward-line 1))
	  (setq end (point))
	  (goto-char beg)
	  (while (and (if (bobp) nil (forward-line -1) t)
		      (not (looking-at paragraph-start))
		      (looking-at message-cite-prefix-regexp)
		      (equal quoted (match-string 0)))
	    (setq beg (point))
	    (goto-char (match-end 0))
	    (looking-at "[ \t]*")
	    (if (> (length leading-space) (length (match-string 0)))
		(setq leading-space (match-string 0)))))
      (while (and (not (eobp))
		  (not (looking-at paragraph-separate))
		  (not (looking-at message-cite-prefix-regexp)))
	(forward-line 1))
      (setq end (point))
      (goto-char beg)
      (while (and (if (bobp) nil (forward-line -1) t)
		  (not (looking-at paragraph-start))
		  (not (looking-at message-cite-prefix-regexp)))
	(setq beg (point))))
    (goto-char point)
    (save-restriction
      (narrow-to-region beg end)
      (if not-break
	  (setq point nil)
	(if bolp
	    (newline)
	  (newline)
	  (newline))
	(setq point (point))
	;; (newline 2) doesn't mark both newline's as hard, so call
	;; newline twice. -jas
	(newline)
	(newline)
	(delete-region (point) (re-search-forward "[ \t]*"))
	(when (and quoted (not bolp))
	  (insert quoted leading-space)))
      (undo-boundary)
      (if quoted
	  (let* ((adaptive-fill-regexp
		  (regexp-quote (concat quoted leading-space)))
		 (adaptive-fill-first-line-regexp
		  adaptive-fill-regexp ))
	    (fill-paragraph arg))
	(fill-paragraph arg))
      (if point (goto-char point)))))

(defun message-fill-paragraph (&optional arg)
  "Like `fill-paragraph'."
  (interactive (list (if current-prefix-arg 'full)))
  (if (if (boundp 'filladapt-mode) filladapt-mode)
      nil
    (message-newline-and-reformat arg t)
    t))

;; Is it better to use `mail-header-end'?
(defun message-point-in-header-p ()
  "Return t if point is in the header."
  (save-excursion
    (let ((p (point)))
      (goto-char (point-min))
      (not (re-search-forward
	    (concat "^" (regexp-quote mail-header-separator) "\n")
	    p t)))))

(defun message-do-auto-fill ()
  "Like `do-auto-fill', but don't fill in message header."
  (unless (message-point-in-header-p)
    (do-auto-fill)))

(defun message-insert-signature (&optional force)
  "Insert a signature.  See documentation for variable `message-signature'."
  (interactive (list 0))
  (let* ((signature
	  (cond
	   ((and (null message-signature)
		 (eq force 0))
	    (save-excursion
	      (goto-char (point-max))
	      (not (re-search-backward message-signature-separator nil t))))
	   ((and (null message-signature)
		 force)
	    t)
	   ((functionp message-signature)
	    (funcall message-signature))
	   ((listp message-signature)
	    (eval message-signature))
	   (t message-signature)))
	 (signature
	  (cond ((stringp signature)
		 signature)
		((and (eq t signature)
		      message-signature-file
		      (file-exists-p message-signature-file))
		 signature))))
    (when signature
      (goto-char (point-max))
      ;; Insert the signature.
      (unless (bolp)
	(insert "\n"))
      (when message-signature-insert-empty-line
	(insert "\n"))
      (insert "-- \n")
      (if (eq signature t)
	  (insert-file-contents message-signature-file)
	(insert signature))
      (goto-char (point-max))
      (or (bolp) (insert "\n")))))

(defun message-insert-importance-high ()
  "Insert header to mark message as important."
  (interactive)
  (save-excursion
    (save-restriction
      (message-narrow-to-headers)
      (message-remove-header "Importance"))
    (message-goto-eoh)
    (insert "Importance: high\n")))

(defun message-insert-importance-low ()
  "Insert header to mark message as unimportant."
  (interactive)
  (save-excursion
    (save-restriction
      (message-narrow-to-headers)
      (message-remove-header "Importance"))
    (message-goto-eoh)
    (insert "Importance: low\n")))

(defun message-insert-or-toggle-importance ()
  "Insert a \"Importance: high\" header, or cycle through the header values.
The three allowed values according to RFC 1327 are `high', `normal'
and `low'."
  (interactive)
  (save-excursion
    (let ((valid '("high" "normal" "low"))
	  (new "high")
	  cur)
      (save-restriction
	(message-narrow-to-headers)
	(when (setq cur (message-fetch-field "Importance"))
	  (message-remove-header "Importance")
	  (setq new (cond ((string= cur "high")
			   "low")
			  ((string= cur "low")
			   "normal")
			  (t
			   "high")))))
      (message-goto-eoh)
      (insert (format "Importance: %s\n" new)))))

(defun message-insert-disposition-notification-to ()
  "Request a disposition notification (return receipt) to this message.
Note that this should not be used in newsgroups."
  (interactive)
  (save-excursion
    (save-restriction
      (message-narrow-to-headers)
      (message-remove-header "Disposition-Notification-To"))
    (message-goto-eoh)
    (insert (format "Disposition-Notification-To: %s\n"
		    (or (message-field-value "Reply-to")
			(message-field-value "From")
			(message-make-from))))))

(defun message-elide-region (b e)
  "Elide the text in the region.
An ellipsis (from `message-elide-ellipsis') will be inserted where the
text was killed."
  (interactive "r")
  (kill-region b e)
  (insert message-elide-ellipsis))

(defvar message-caesar-translation-table nil)

(defun message-caesar-region (b e &optional n)
  "Caesar rotate region B to E by N, default 13, for decrypting netnews."
  (interactive
   (list
    (min (point) (or (mark t) (point)))
    (max (point) (or (mark t) (point)))
    (when current-prefix-arg
      (prefix-numeric-value current-prefix-arg))))

  (setq n (if (numberp n) (mod n 26) 13)) ;canonize N
  (unless (or (zerop n)		        ; no action needed for a rot of 0
	      (= b e))			; no region to rotate
    ;; We build the table, if necessary.
    (when (or (not message-caesar-translation-table)
	      (/= (aref message-caesar-translation-table ?a) (+ ?a n)))
      (setq message-caesar-translation-table
	    (message-make-caesar-translation-table n)))
    (translate-region b e message-caesar-translation-table)))

(defun message-make-caesar-translation-table (n)
  "Create a rot table with offset N."
  (let ((i -1)
	(table (make-string 256 0)))
    (while (< (incf i) 256)
      (aset table i i))
    (concat
     (substring table 0 ?A)
     (substring table (+ ?A n) (+ ?A n (- 26 n)))
     (substring table ?A (+ ?A n))
     (substring table (+ ?A 26) ?a)
     (substring table (+ ?a n) (+ ?a n (- 26 n)))
     (substring table ?a (+ ?a n))
     (substring table (+ ?a 26) 255))))

(defun message-caesar-buffer-body (&optional rotnum)
  "Caesar rotate all letters in the current buffer by 13 places.
Used to encode/decode possibly offensive messages (commonly in rec.humor).
With prefix arg, specifies the number of places to rotate each letter forward.
Mail and USENET news headers are not rotated."
  (interactive (if current-prefix-arg
		   (list (prefix-numeric-value current-prefix-arg))
		 (list nil)))
  (save-excursion
    (save-restriction
      (when (message-goto-body)
	(narrow-to-region (point) (point-max)))
      (message-caesar-region (point-min) (point-max) rotnum))))

(defun message-pipe-buffer-body (program)
  "Pipe the message body in the current buffer through PROGRAM."
  (save-excursion
    (save-restriction
      (when (message-goto-body)
	(narrow-to-region (point) (point-max)))
      (shell-command-on-region
       (point-min) (point-max) program nil t))))

(defun message-rename-buffer (&optional enter-string)
  "Rename the *message* buffer to \"*message* RECIPIENT\".
If the function is run with a prefix, it will ask for a new buffer
name, rather than giving an automatic name."
  (interactive "Pbuffer name: ")
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      (narrow-to-region (point)
			(search-forward mail-header-separator nil 'end))
      (let* ((mail-to (or
		       (if (message-news-p) (message-fetch-field "Newsgroups")
			 (message-fetch-field "To"))
		       ""))
	     (mail-trimmed-to
	      (if (string-match "," mail-to)
		  (concat (substring mail-to 0 (match-beginning 0)) ", ...")
		mail-to))
	     (name-default (concat "*message* " mail-trimmed-to))
	     (name (if enter-string
		       (read-string "New buffer name: " name-default)
		     name-default)))
	(rename-buffer name t)))))

(defun message-fill-yanked-message (&optional justifyp)
  "Fill the paragraphs of a message yanked into this one.
Numeric argument means justify as well."
  (interactive "P")
  (save-excursion
    (goto-char (point-min))
    (search-forward (concat "\n" mail-header-separator "\n") nil t)
    (let ((fill-prefix message-yank-prefix))
      (fill-individual-paragraphs (point) (point-max) justifyp))))

(defun message-indent-citation ()
  "Modify text just inserted from a message to be cited.
The inserted text should be the region.
When this function returns, the region is again around the modified text.

Normally, indent each nonblank line `message-indentation-spaces' spaces.
However, if `message-yank-prefix' is non-nil, insert that prefix on each line."
  (let ((start (point)))
    ;; Remove unwanted headers.
    (when message-ignored-cited-headers
      (let (all-removed)
	(save-restriction
	  (narrow-to-region
	   (goto-char start)
	   (if (search-forward "\n\n" nil t)
	       (1- (point))
	     (point)))
	  (message-remove-header message-ignored-cited-headers t)
	  (when (= (point-min) (point-max))
	    (setq all-removed t))
	  (goto-char (point-max)))
	(if all-removed
	    (goto-char start)
	  (forward-line 1))))
    ;; Delete blank lines at the start of the buffer.
    (while (and (point-min)
		(eolp)
		(not (eobp)))
      (message-delete-line))
    ;; Delete blank lines at the end of the buffer.
    (goto-char (point-max))
    (unless (eolp)
      (insert "\n"))
    (while (and (zerop (forward-line -1))
		(looking-at "$"))
      (message-delete-line))
    ;; Do the indentation.
    (if (null message-yank-prefix)
	(indent-rigidly start (mark t) message-indentation-spaces)
      (save-excursion
	(goto-char start)
	(while (< (point) (mark t))
	  (if (or (looking-at ">") (looking-at "^$"))
	      (insert message-yank-cited-prefix)
	    (insert message-yank-prefix))
	  (forward-line 1))))
    (goto-char start)))

(defun message-yank-original (&optional arg)
  "Insert the message being replied to, if any.
Puts point before the text and mark after.
Normally indents each nonblank line ARG spaces (default 3).  However,
if `message-yank-prefix' is non-nil, insert that prefix on each line.

This function uses `message-cite-function' to do the actual citing.

Just \\[universal-argument] as argument means don't indent, insert no
prefix, and don't delete any headers."
  (interactive "P")
  (let ((modified (buffer-modified-p)))
    (when (and message-reply-buffer
	       message-cite-function)
      (delete-windows-on message-reply-buffer t)
      (push-mark (save-excursion
		   (insert-buffer-substring message-reply-buffer)
		   (point)))
      (unless arg
	(funcall message-cite-function))
      (message-exchange-point-and-mark)
      (unless (bolp)
	(insert ?\n))
      (unless modified
	(setq message-checksum (message-checksum))))))

(defun message-yank-buffer (buffer)
  "Insert BUFFER into the current buffer and quote it."
  (interactive "bYank buffer: ")
  (let ((message-reply-buffer (get-buffer buffer)))
    (save-window-excursion
      (message-yank-original))))

(defun message-buffers ()
  "Return a list of active message buffers."
  (let (buffers)
    (save-excursion
      (dolist (buffer (buffer-list t))
	(set-buffer buffer)
	(when (and (eq major-mode 'message-mode)
		   (null message-sent-message-via))
	  (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

(defun message-cite-original-without-signature ()
  "Cite function in the standard Message manner."
  (let* ((start (point))
	 (end (mark t))
	 (functions
	  (when message-indent-citation-function
	    (if (listp message-indent-citation-function)
		message-indent-citation-function
	      (list message-indent-citation-function))))
	 ;; This function may be called by `gnus-summary-yank-message' and
	 ;; may insert a different article from the original.  So, we will
	 ;; modify the value of `message-reply-headers' with that article.
	 (message-reply-headers
	  (save-restriction
	    (narrow-to-region start end)
	    (message-narrow-to-head-1)
	    (vector 0
		    (or (message-fetch-field "subject") "none")
		    (message-fetch-field "from")
		    (message-fetch-field "date")
		    (message-fetch-field "message-id" t)
		    (message-fetch-field "references")
		    0 0 ""))))
    (mml-quote-region start end)
    ;; Allow undoing.
    (undo-boundary)
    (goto-char end)
    (when (re-search-backward message-signature-separator start t)
      ;; Also peel off any blank lines before the signature.
      (forward-line -1)
      (while (looking-at "^[ \t]*$")
	(forward-line -1))
      (forward-line 1)
      (delete-region (point) end)
      (unless (search-backward "\n\n" start t)
	;; Insert a blank line if it is peeled off.
	(insert "\n")))
    (goto-char start)
    (while functions
      (funcall (pop functions)))
    (when message-citation-line-function
      (unless (bolp)
	(insert "\n"))
      (funcall message-citation-line-function))))

(eval-when-compile (defvar mail-citation-hook))	;Compiler directive
(defun message-cite-original ()
  "Cite function in the standard Message manner."
  (if (and (boundp 'mail-citation-hook)
	   mail-citation-hook)
      (run-hooks 'mail-citation-hook)
    (let* ((start (point))
	   (end (mark t))
	   (functions
	    (when message-indent-citation-function
	      (if (listp message-indent-citation-function)
		  message-indent-citation-function
		(list message-indent-citation-function))))
	   ;; This function may be called by `gnus-summary-yank-message' and
	   ;; may insert a different article from the original.  So, we will
	   ;; modify the value of `message-reply-headers' with that article.
	   (message-reply-headers
	    (save-restriction
	      (narrow-to-region start end)
	      (message-narrow-to-head-1)
	      (vector 0
		      (or (message-fetch-field "subject") "none")
		      (message-fetch-field "from")
		      (message-fetch-field "date")
		      (message-fetch-field "message-id" t)
		      (message-fetch-field "references")
		      0 0 ""))))
      (mml-quote-region start end)
      (goto-char start)
      (while functions
	(funcall (pop functions)))
      (when message-citation-line-function
	(unless (bolp)
	  (insert "\n"))
	(funcall message-citation-line-function)))))

(defun message-insert-citation-line ()
  "Insert a simple citation line."
  (when message-reply-headers
    (insert (mail-header-from message-reply-headers) " writes:\n\n")))

(defun message-position-on-field (header &rest afters)
  (let ((case-fold-search t))
    (save-restriction
      (narrow-to-region
       (goto-char (point-min))
       (progn
	 (re-search-forward
	  (concat "^" (regexp-quote mail-header-separator) "$"))
	 (match-beginning 0)))
      (goto-char (point-min))
      (if (re-search-forward (concat "^" (regexp-quote header) ":") nil t)
	  (progn
	    (re-search-forward "^[^ \t]" nil 'move)
	    (beginning-of-line)
	    (skip-chars-backward "\n")
	    t)
	(while (and afters
		    (not (re-search-forward
			  (concat "^" (regexp-quote (car afters)) ":")
			  nil t)))
	  (pop afters))
	(when afters
	  (re-search-forward "^[^ \t]" nil 'move)
	  (beginning-of-line))
	(insert header ": \n")
	(forward-char -1)
	nil))))

(defun message-remove-signature ()
  "Remove the signature from the text between point and mark.
The text will also be indented the normal way."
  (save-excursion
    (let ((start (point))
	  mark)
      (if (not (re-search-forward message-signature-separator (mark t) t))
	  ;; No signature here, so we just indent the cited text.
	  (message-indent-citation)
	;; Find the last non-empty line.
	(forward-line -1)
	(while (looking-at "[ \t]*$")
	  (forward-line -1))
	(forward-line 1)
	(setq mark (set-marker (make-marker) (point)))
	(goto-char start)
	(message-indent-citation)
	;; Enable undoing the deletion.
	(undo-boundary)
	(delete-region mark (mark t))
	(set-marker mark nil)))))



;;;
;;; Sending messages
;;;

(defun message-send-and-exit (&optional arg)
  "Send message like `message-send', then, if no errors, exit from mail buffer."
  (interactive "P")
  (let ((buf (current-buffer))
	(actions message-exit-actions))
    (when (and (message-send arg)
	       (buffer-name buf))
      (if message-kill-buffer-on-exit
	  (kill-buffer buf)
	(bury-buffer buf)
	(when (eq buf (current-buffer))
	  (message-bury buf)))
      (message-do-actions actions)
      t)))

(defun message-dont-send ()
  "Don't send the message you have been editing.
Instead, just auto-save the buffer and then bury it."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer)
  (let ((actions message-postpone-actions))
    (message-bury (current-buffer))
    (message-do-actions actions)))

(defun message-kill-buffer ()
  "Kill the current buffer."
  (interactive)
  (when (or (not (buffer-modified-p))
	    (yes-or-no-p "Message modified; kill anyway? "))
    (let ((actions message-kill-actions)
	  (draft-article message-draft-article)
	  (auto-save-file-name buffer-auto-save-file-name)
	  (file-name buffer-file-name)
	  (modified (buffer-modified-p)))
      (setq buffer-file-name nil)
      (kill-buffer (current-buffer))
      (when (and (or (and auto-save-file-name
			  (file-exists-p auto-save-file-name))
		     (and file-name
			  (file-exists-p file-name)))
		 (progn
		   ;; If the message buffer has lived in a dedicated window,
		   ;; `kill-buffer' has killed the frame.  Thus the
		   ;; `yes-or-no-p' may show up in a lowered frame.  Make sure
		   ;; that the user can see the question by raising the
		   ;; current frame:
		   (raise-frame)
		   (yes-or-no-p (format "Remove the backup file%s? "
					(if modified " too" "")))))
	(ignore-errors
	  (delete-file auto-save-file-name))
	(let ((message-draft-article draft-article))
	  (message-disassociate-draft)))
      (message-do-actions actions))))

(defun message-bury (buffer)
  "Bury this mail BUFFER."
  (let ((newbuf (other-buffer buffer)))
    (bury-buffer buffer)
    (if (and (window-dedicated-p (selected-window))
	     (not (null (delq (selected-frame) (visible-frame-list)))))
	(delete-frame (selected-frame))
      (switch-to-buffer newbuf))))

(defun message-send (&optional arg)
  "Send the message in the current buffer.
If `message-interactive' is non-nil, wait for success indication or
error messages, and inform user.
Otherwise any failure is reported in a message back to the user from
the mailer.
The usage of ARG is defined by the instance that called Message.
It should typically alter the sending method in some way or other."
  (interactive "P")
  ;; Make it possible to undo the coming changes.
  (undo-boundary)
  (let ((inhibit-read-only t))
    (put-text-property (point-min) (point-max) 'read-only nil))
  (message-fix-before-sending)
  (run-hooks 'message-send-hook)
  (message message-sending-message)
  (let ((alist message-send-method-alist)
	(success t)
	elem sent dont-barf-on-no-method
	(message-options message-options))
    (message-options-set-recipient)
    (while (and success
		(setq elem (pop alist)))
      (when (funcall (cadr elem))
	(when (and (or (not (memq (car elem)
				  message-sent-message-via))
		       (message-fetch-field "supersedes")
		       (if (or (message-gnksa-enable-p 'multiple-copies)
			       (not (eq (car elem) 'news)))
			   (y-or-n-p
			    (format
			     "Already sent message via %s; resend? "
			     (car elem)))
			 (error "Denied posting -- multiple copies")))
		   (setq success (funcall (caddr elem) arg)))
	  (setq sent t))))
    (unless (or sent
		(not success)
		(let ((fcc (message-fetch-field "Fcc"))
		      (gcc (message-fetch-field "Gcc")))
		  (when (or fcc gcc)
		    (or (eq message-allow-no-recipients 'always)
			(and (not (eq message-allow-no-recipients 'never))
			     (setq dont-barf-on-no-method
				   (gnus-y-or-n-p
				    (format "No receiver, perform %s anyway? "
					    (cond ((and fcc gcc) "Fcc and Gcc")
						  (fcc "Fcc")
						  (t "Gcc"))))))))))
      (error "No methods specified to send by"))
    (when (or dont-barf-on-no-method
	      (and success sent))
      (message-do-fcc)
      (save-excursion
	(run-hooks 'message-sent-hook))
      (message "Sending...done")
      ;; Mark the buffer as unmodified and delete auto-save.
      (set-buffer-modified-p nil)
      (delete-auto-save-file-if-necessary t)
      (message-disassociate-draft)
      ;; Delete other mail buffers and stuff.
      (message-do-send-housekeeping)
      (message-do-actions message-send-actions)
      ;; Return success.
      t)))

(defun message-send-via-mail (arg)
  "Send the current message via mail."
  (message-send-mail arg))

(defun message-send-via-news (arg)
  "Send the current message via news."
  (funcall message-send-news-function arg))

(defmacro message-check (type &rest forms)
  "Eval FORMS if TYPE is to be checked."
  `(or (message-check-element ,type)
       (save-excursion
	 ,@forms)))

(put 'message-check 'lisp-indent-function 1)
(put 'message-check 'edebug-form-spec '(form body))

(defun message-text-with-property (prop)
  "Return a list of all points where the text has PROP."
  (let ((points nil)
	(point (point-min)))
    (save-excursion
      (while (< point (point-max))
	(when (get-text-property point prop)
	  (push point points))
	(incf point)))
    (nreverse points)))

(defun message-fix-before-sending ()
  "Do various things to make the message nice before sending it."
  ;; Make sure there's a newline at the end of the message.
  (goto-char (point-max))
  (unless (bolp)
    (insert "\n"))
  ;; Make the hidden headers visible.
  (let ((points (message-text-with-property 'message-hidden)))
    (when points
      (goto-char (car points))
      (dolist (point points)
	(add-text-properties point (1+ point)
			     '(invisible nil intangible nil)))))
  ;; Make invisible text visible.
  ;; It doesn't seem as if this is useful, since the invisible property
  ;; is clobbered by an after-change hook anyhow.
  (message-check 'invisible-text
    (let ((points (message-text-with-property 'invisible)))
      (when points
	(goto-char (car points))
	(dolist (point points)
	  (put-text-property point (1+ point) 'invisible nil)
	  (message-overlay-put (message-make-overlay point (1+ point))
			       'face 'highlight))
	(unless (yes-or-no-p
		 "Invisible text found and made visible; continue sending? ")
	  (error "Invisible text found and made visible")))))
  (message-check 'illegible-text
    (let (found choice)
      (message-goto-body)
      (skip-chars-forward mm-7bit-chars)
      (while (not (eobp))
	(when (let ((char (char-after)))
		(or (< (mm-char-int char) 128)
		    (and (mm-multibyte-p)
			 (memq (char-charset char)
			       '(eight-bit-control eight-bit-graphic
						   control-1))
			 (not (get-text-property
			       (point) 'untranslated-utf-8)))))
	  (message-overlay-put (message-make-overlay (point) (1+ (point)))
			       'face 'highlight)
	  (setq found t))
	(forward-char)
	(skip-chars-forward mm-7bit-chars))
      (when found
	(setq choice
	      (gnus-multiple-choice
	       "Non-printable characters found.  Continue sending?"
	       '((?d "Remove non-printable characters and send")
		 (?r "Replace non-printable characters with dots and send")
		 (?i "Ignore non-printable characters and send")
		 (?e "Continue editing"))))
	(if (eq choice ?e)
	  (error "Non-printable characters"))
	(message-goto-body)
	(skip-chars-forward mm-7bit-chars)
	(while (not (eobp))
	  (when (let ((char (char-after)))
		  (or (< (mm-char-int char) 128)
		      (and (mm-multibyte-p)
			   ;; Fixme: Wrong for Emacs 23 and for things
			   ;; like undectable utf-8.  Should at least
			   ;; use find-coding-systems-region.
			   (memq (char-charset char)
				 '(eight-bit-control eight-bit-graphic
						     control-1))
			   (not (get-text-property
				 (point) 'untranslated-utf-8)))))
	    (if (eq choice ?i)
		(message-kill-all-overlays)
	      (delete-char 1)
	      (when (eq choice ?r)
		(insert "."))))
	  (forward-char)
	  (skip-chars-forward mm-7bit-chars))))))

(defun message-add-action (action &rest types)
  "Add ACTION to be performed when doing an exit of type TYPES."
  (while types
    (add-to-list (intern (format "message-%s-actions" (pop types)))
		 action)))

(defun message-delete-action (action &rest types)
  "Delete ACTION from lists of actions performed when doing an exit of type TYPES."
  (let (var)
    (while types
      (set (setq var (intern (format "message-%s-actions" (pop types))))
	   (delq action (symbol-value var))))))

(defun message-do-actions (actions)
  "Perform all actions in ACTIONS."
  ;; Now perform actions on successful sending.
  (while actions
    (ignore-errors
      (cond
       ;; A simple function.
       ((functionp (car actions))
	(funcall (car actions)))
       ;; Something to be evaled.
       (t
	(eval (car actions)))))
    (pop actions)))

(defun message-send-mail-partially ()
  "Send mail as message/partial."
  ;; replace the header delimiter with a blank line
  (goto-char (point-min))
  (re-search-forward
   (concat "^" (regexp-quote mail-header-separator) "\n"))
  (replace-match "\n")
  (run-hooks 'message-send-mail-hook)
  (let ((p (goto-char (point-min)))
	(tembuf (message-generate-new-buffer-clone-locals " message temp"))
	(curbuf (current-buffer))
	(id (message-make-message-id)) (n 1)
	plist total  header required-mail-headers)
    (while (not (eobp))
      (if (< (point-max) (+ p message-send-mail-partially-limit))
	  (goto-char (point-max))
	(goto-char (+ p message-send-mail-partially-limit))
	(beginning-of-line)
	(if (<= (point) p) (forward-line 1))) ;; In case of bad message.
      (push p plist)
      (setq p (point)))
    (setq total (length plist))
    (push (point-max) plist)
    (setq plist (nreverse plist))
    (unwind-protect
	(save-excursion
	  (setq p (pop plist))
	  (while plist
	    (set-buffer curbuf)
	    (copy-to-buffer tembuf p (car plist))
	    (set-buffer tembuf)
	    (goto-char (point-min))
	    (if header
		(progn
		  (goto-char (point-min))
		  (narrow-to-region (point) (point))
		  (insert header))
	      (message-goto-eoh)
	      (setq header (buffer-substring (point-min) (point)))
	      (goto-char (point-min))
	      (narrow-to-region (point) (point))
	      (insert header)
	      (message-remove-header "Mime-Version")
	      (message-remove-header "Content-Type")
	      (message-remove-header "Content-Transfer-Encoding")
	      (message-remove-header "Message-ID")
	      (message-remove-header "Lines")
	      (goto-char (point-max))
	      (insert "Mime-Version: 1.0\n")
	      (setq header (buffer-string)))
	    (goto-char (point-max))
	    (insert (format "Content-Type: message/partial; id=\"%s\"; number=%d; total=%d\n\n"
			    id n total))
	    (forward-char -1)
	    (let ((mail-header-separator ""))
	      (when (memq 'Message-ID message-required-mail-headers)
		(insert "Message-ID: " (message-make-message-id) "\n"))
	      (when (memq 'Lines message-required-mail-headers)
		(insert "Lines: " (message-make-lines) "\n"))
	      (message-goto-subject)
	      (end-of-line)
	      (insert (format " (%d/%d)" n total))
	      (widen)
	      (mm-with-unibyte-current-buffer
		(funcall (or message-send-mail-real-function
			     message-send-mail-function))))
	    (setq n (+ n 1))
	    (setq p (pop plist))
	    (erase-buffer)))
      (kill-buffer tembuf))))

(defun message-send-mail (&optional arg)
  (require 'mail-utils)
  (let* ((tembuf (message-generate-new-buffer-clone-locals " message temp"))
	 (case-fold-search nil)
	 (news (message-news-p))
	 (mailbuf (current-buffer))
	 (message-this-is-mail t)
	 (message-posting-charset
	  (if (fboundp 'gnus-setup-posting-charset)
	      (gnus-setup-posting-charset nil)
	    message-posting-charset))
	 (headers message-required-mail-headers))
    (save-restriction
      (message-narrow-to-headers)
      ;; Generate the Mail-Followup-To header if the header is not there...
      (if (and (message-subscribed-p)
	       (not (mail-fetch-field "mail-followup-to")))
	  (setq headers
		(cons
		 (cons "Mail-Followup-To" (message-make-mail-followup-to))
		 message-required-mail-headers))
	;; otherwise, delete the MFT header if the field is empty
	(when (equal "" (mail-fetch-field "mail-followup-to"))
	  (message-remove-header "^Mail-Followup-To:")))
      ;; Insert some headers.
      (let ((message-deletable-headers
	     (if news nil message-deletable-headers)))
	(message-generate-headers headers))
      ;; Let the user do all of the above.
      (run-hooks 'message-header-hook))
    (unwind-protect
	(save-excursion
	  (set-buffer tembuf)
	  (erase-buffer)
	  ;; Avoid copying text props (except hard newlines).
	  (insert (with-current-buffer mailbuf
		    (mml-buffer-substring-no-properties-except-hard-newlines
		     (point-min) (point-max))))
	  ;; Remove some headers.
	  (message-encode-message-body)
	  (save-restriction
	    (message-narrow-to-headers)
	    ;; We (re)generate the Lines header.
	    (when (memq 'Lines message-required-mail-headers)
	      (message-generate-headers '(Lines)))
	    ;; Remove some headers.
	    (message-remove-header message-ignored-mail-headers t)
	    (let ((mail-parse-charset message-default-charset))
	      (mail-encode-encoded-word-buffer)))
	  (goto-char (point-max))
	  ;; require one newline at the end.
	  (or (= (preceding-char) ?\n)
	      (insert ?\n))
	  (message-cleanup-headers)
	  ;; FIXME: we're inserting the courtesy copy after encoding.
	  ;; This is wrong if the courtesy copy string contains
	  ;; non-ASCII characters. -- jh
	  (when
	      (save-restriction
		(message-narrow-to-headers)
		(and news
		     (or (message-fetch-field "cc")
			 (message-fetch-field "bcc")
			 (message-fetch-field "to"))
		     (let ((content-type (message-fetch-field
					  "content-type")))
		       (and
			(or
			 (not content-type)
			 (string= "text/plain"
				  (car
				   (mail-header-parse-content-type
				    content-type))))
			(not
			 (string= "base64"
				  (message-fetch-field
				   "content-transfer-encoding")))))))
	    (message-insert-courtesy-copy))
	  (if (or (not message-send-mail-partially-limit)
		  (< (buffer-size) message-send-mail-partially-limit)
		  (not (message-y-or-n-p
			"The message size is too large, split? "
			t
			"\
The message size, "
			(/ (buffer-size) 1000) "KB, is too large.

Some mail gateways (MTA's) bounce large messages.  To avoid the
problem, answer `y', and the message will be split into several
smaller pieces, the size of each is about "
			(/ message-send-mail-partially-limit 1000)
			"KB except the last
one.

However, some mail readers (MUA's) can't read split messages, i.e.,
mails in message/partially format. Answer `n', and the message will be
sent in one piece.

The size limit is controlled by `message-send-mail-partially-limit'.
If you always want Gnus to send messages in one piece, set
`message-send-mail-partially-limit' to nil.
")))
	      (mm-with-unibyte-current-buffer
		(message "Sending via mail...")
		(funcall (or message-send-mail-real-function
			     message-send-mail-function)))
	    (message-send-mail-partially)))
      (kill-buffer tembuf))
    (set-buffer mailbuf)
    (push 'mail message-sent-message-via)))

(defun message-send-mail-with-sendmail ()
  "Send off the prepared buffer with sendmail."
  (let ((errbuf (if message-interactive
		    (message-generate-new-buffer-clone-locals
		     " sendmail errors")
		  0))
	resend-to-addresses delimline)
    (unwind-protect
	(progn
	  (let ((case-fold-search t))
	    (save-restriction
	      (message-narrow-to-headers)
	      (setq resend-to-addresses (message-fetch-field "resent-to")))
	    ;; Change header-delimiter to be what sendmail expects.
	    (goto-char (point-min))
	    (re-search-forward
	     (concat "^" (regexp-quote mail-header-separator) "\n"))
	    (replace-match "\n")
	    (backward-char 1)
	    (setq delimline (point-marker))
	    (run-hooks 'message-send-mail-hook)
	    ;; Insert an extra newline if we need it to work around
	    ;; Sun's bug that swallows newlines.
	    (goto-char (1+ delimline))
	    (when (eval message-mailer-swallows-blank-line)
	      (newline))
	    (when message-interactive
	      (save-excursion
		(set-buffer errbuf)
		(erase-buffer))))
	  (let* ((default-directory "/")
		 (coding-system-for-write message-send-coding-system)
		 (cpr (apply
		       'call-process-region
		       (append
			(list (point-min) (point-max)
			      (if (boundp 'sendmail-program)
				  sendmail-program
				"/usr/lib/sendmail")
			      nil errbuf nil "-oi")
			;; Always specify who from,
			;; since some systems have broken sendmails.
			;; But some systems are more broken with -f, so
			;; we'll let users override this.
			(if (null message-sendmail-f-is-evil)
			    (list "-f" (message-sendmail-envelope-from)))
			;; These mean "report errors by mail"
			;; and "deliver in background".
			(if (null message-interactive) '("-oem" "-odb"))
			;; Get the addresses from the message
			;; unless this is a resend.
			;; We must not do that for a resend
			;; because we would find the original addresses.
			;; For a resend, include the specific addresses.
			(if resend-to-addresses
			    (list resend-to-addresses)
			  '("-t"))))))
	    (unless (or (null cpr) (and (numberp cpr) (zerop cpr)))
	      (error "Sending...failed with exit value %d" cpr)))
	  (when message-interactive
	    (save-excursion
	      (set-buffer errbuf)
	      (goto-char (point-min))
	      (while (re-search-forward "\n\n* *" nil t)
		(replace-match "; "))
	      (if (not (zerop (buffer-size)))
		  (error "Sending...failed to %s"
			 (buffer-string))))))
      (when (bufferp errbuf)
	(kill-buffer errbuf)))))

(defun message-send-mail-with-qmail ()
  "Pass the prepared message buffer to qmail-inject.
Refer to the documentation for the variable `message-send-mail-function'
to find out how to use this."
  ;; replace the header delimiter with a blank line
  (goto-char (point-min))
  (re-search-forward
   (concat "^" (regexp-quote mail-header-separator) "\n"))
  (replace-match "\n")
  (run-hooks 'message-send-mail-hook)
  ;; send the message
  (case
      (let ((coding-system-for-write message-send-coding-system))
	(apply
	 'call-process-region (point-min) (point-max)
	 message-qmail-inject-program nil nil nil
	 ;; qmail-inject's default behaviour is to look for addresses on the
	 ;; command line; if there're none, it scans the headers.
	 ;; yes, it does The Right Thing w.r.t. Resent-To and it's kin.
	 ;;
	 ;; in general, ALL of qmail-inject's defaults are perfect for simply
	 ;; reading a formatted (i. e., at least a To: or Resent-To header)
	 ;; message from stdin.
	 ;;
	 ;; qmail also has the advantage of not having been raped by
	 ;; various vendors, so we don't have to allow for that, either --
	 ;; compare this with message-send-mail-with-sendmail and weep
	 ;; for sendmail's lost innocence.
	 ;;
	 ;; all this is way cool coz it lets us keep the arguments entirely
	 ;; free for -inject-arguments -- a big win for the user and for us
	 ;; since we don't have to play that double-guessing game and the user
	 ;; gets full control (no gestapo'ish -f's, for instance).  --sj
         (if (functionp message-qmail-inject-args)
             (funcall message-qmail-inject-args)
           message-qmail-inject-args)))
    ;; qmail-inject doesn't say anything on it's stdout/stderr,
    ;; we have to look at the retval instead
    (0 nil)
    (100 (error "qmail-inject reported permanent failure"))
    (111 (error "qmail-inject reported transient failure"))
    ;; should never happen
    (t   (error "qmail-inject reported unknown failure"))))

(defun message-send-mail-with-mh ()
  "Send the prepared message buffer with mh."
  (let ((mh-previous-window-config nil)
	(name (mh-new-draft-name)))
    (setq buffer-file-name name)
    ;; MH wants to generate these headers itself.
    (when message-mh-deletable-headers
      (let ((headers message-mh-deletable-headers))
	(while headers
	  (goto-char (point-min))
	  (and (re-search-forward
		(concat "^" (symbol-name (car headers)) ": *") nil t)
	       (message-delete-line))
	  (pop headers))))
    (run-hooks 'message-send-mail-hook)
    ;; Pass it on to mh.
    (mh-send-letter)))

(defun message-smtpmail-send-it ()
  "Send the prepared message buffer with `smtpmail-send-it'.
This only differs from `smtpmail-send-it' that this command evaluates
`message-send-mail-hook' just before sending a message.  It is useful
if your ISP requires the POP-before-SMTP authentication.  See the Gnus
manual for details."
  (run-hooks 'message-send-mail-hook)
  (smtpmail-send-it))

(defun message-canlock-generate ()
  "Return a string that is non-trivial to guess.
Do not use this for anything important, it is cryptographically weak."
  (require 'sha1)
  (let (sha1-maximum-internal-length)
    (sha1 (concat (message-unique-id)
		  (format "%x%x%x" (random) (random t) (random))
		  (prin1-to-string (recent-keys))
		  (prin1-to-string (garbage-collect))))))

(defun message-canlock-password ()
  "The password used by message for cancel locks.
This is the value of `canlock-password', if that option is non-nil.
Otherwise, generate and save a value for `canlock-password' first."
  (unless canlock-password
    (customize-save-variable 'canlock-password (message-canlock-generate))
    (setq canlock-password-for-verify canlock-password))
  canlock-password)

(defun message-insert-canlock ()
  (when message-insert-canlock
    (message-canlock-password)
    (canlock-insert-header)))

(defun message-send-news (&optional arg)
  (let* ((tembuf (message-generate-new-buffer-clone-locals " *message temp*"))
	 (case-fold-search nil)
	 (method (if (functionp message-post-method)
		     (funcall message-post-method arg)
		   message-post-method))
	 (newsgroups-field (save-restriction
			    (message-narrow-to-headers-or-head)
			    (message-fetch-field "Newsgroups")))
	 (followup-field (save-restriction
			   (message-narrow-to-headers-or-head)
			   (message-fetch-field "Followup-To")))
	 ;; BUG: We really need to get the charset for each name in the
	 ;; Newsgroups and Followup-To lines to allow crossposting
	 ;; between group namess with incompatible character sets.
	 ;; -- Per Abrahamsen <abraham@dina.kvl.dk> 2001-10-08.
	 (group-field-charset
	  (gnus-group-name-charset method newsgroups-field))
	 (followup-field-charset
	  (gnus-group-name-charset method (or followup-field "")))
	 (rfc2047-header-encoding-alist
	  (append (when group-field-charset
		    (list (cons "Newsgroups" group-field-charset)))
		  (when followup-field-charset
		    (list (cons "Followup-To" followup-field-charset)))
		  rfc2047-header-encoding-alist))
	 (messbuf (current-buffer))
	 (message-syntax-checks
	  (if (and arg
		   (listp message-syntax-checks))
	      (cons '(existing-newsgroups . disabled)
		    message-syntax-checks)
	    message-syntax-checks))
	 (message-this-is-news t)
	 (message-posting-charset
	  (gnus-setup-posting-charset newsgroups-field))
	 result)
    (if (not (message-check-news-body-syntax))
	nil
      (save-restriction
	(message-narrow-to-headers)
	;; Insert some headers.
	(message-generate-headers message-required-news-headers)
	(message-insert-canlock)
	;; Let the user do all of the above.
	(run-hooks 'message-header-hook))
      ;; Note: This check will be disabled by the ".*" default value for
      ;; gnus-group-name-charset-group-alist. -- Pa 2001-10-07.
      (when (and group-field-charset
		 (listp message-syntax-checks))
	(setq message-syntax-checks
	      (cons '(valid-newsgroups . disabled)
		    message-syntax-checks)))
      (message-cleanup-headers)
      (if (not (let ((message-post-method method))
		 (message-check-news-syntax)))
	  nil
	(unwind-protect
	    (save-excursion
	      (set-buffer tembuf)
	      (buffer-disable-undo)
	      (erase-buffer)
	      ;; Avoid copying text props (except hard newlines).
	      (insert
	       (with-current-buffer messbuf
		 (mml-buffer-substring-no-properties-except-hard-newlines
		  (point-min) (point-max))))
	      (message-encode-message-body)
	      ;; Remove some headers.
	      (save-restriction
		(message-narrow-to-headers)
		;; We (re)generate the Lines header.
		(when (memq 'Lines message-required-mail-headers)
		  (message-generate-headers '(Lines)))
		;; Remove some headers.
		(message-remove-header message-ignored-news-headers t)
		(let ((mail-parse-charset message-default-charset))
		  (mail-encode-encoded-word-buffer)))
	      (goto-char (point-max))
	      ;; require one newline at the end.
	      (or (= (preceding-char) ?\n)
		  (insert ?\n))
	      (let ((case-fold-search t))
		;; Remove the delimiter.
		(goto-char (point-min))
		(re-search-forward
		 (concat "^" (regexp-quote mail-header-separator) "\n"))
		(replace-match "\n")
		(backward-char 1))
	      (run-hooks 'message-send-news-hook)
	      (gnus-open-server method)
	      (message "Sending news via %s..." (gnus-server-string method))
	      (setq result (let ((mail-header-separator ""))
			     (gnus-request-post method))))
	  (kill-buffer tembuf))
	(set-buffer messbuf)
	(if result
	    (push 'news message-sent-message-via)
	  (message "Couldn't send message via news: %s"
		   (nnheader-get-report (car method)))
	  nil)))))

;;;
;;; Header generation & syntax checking.
;;;

(defun message-check-element (type)
  "Return non-nil if this TYPE is not to be checked."
  (if (eq message-syntax-checks 'dont-check-for-anything-just-trust-me)
      t
    (let ((able (assq type message-syntax-checks)))
      (and (consp able)
	   (eq (cdr able) 'disabled)))))

(defun message-check-news-syntax ()
  "Check the syntax of the message."
  (save-excursion
    (save-restriction
      (widen)
      ;; We narrow to the headers and check them first.
      (save-excursion
	(save-restriction
	  (message-narrow-to-headers)
	  (message-check-news-header-syntax))))))

(defun message-check-news-header-syntax ()
  (and
   ;; Check Newsgroups header.
   (message-check 'newsgroups
     (let ((group (message-fetch-field "newsgroups")))
       (or
	(and group
	     (not (string-match "\\`[ \t]*\\'" group)))
	(ignore
	 (message
	  "The newsgroups field is empty or missing.  Posting is denied.")))))
   ;; Check the Subject header.
   (message-check 'subject
     (let* ((case-fold-search t)
	    (subject (message-fetch-field "subject")))
       (or
	(and subject
	     (not (string-match "\\`[ \t]*\\'" subject)))
	(ignore
	 (message
	  "The subject field is empty or missing.  Posting is denied.")))))
   ;; Check for commands in Subject.
   (message-check 'subject-cmsg
     (if (string-match "^cmsg " (message-fetch-field "subject"))
	 (y-or-n-p
	  "The control code \"cmsg\" is in the subject.  Really post? ")
       t))
   ;; Check long header lines.
   (message-check 'long-header-lines
     (let ((start (point))
	   (header nil)
	   (length 0)
	   found)
       (while (and (not found)
		   (re-search-forward "^\\([^ \t:]+\\): " nil t))
	 (if (> (- (point) (match-beginning 0)) 998)
	     (setq found t
		   length (- (point) (match-beginning 0)))
	   (setq header (match-string-no-properties 1)))
	 (setq start (match-beginning 0))
	 (forward-line 1))
       (if found
	   (y-or-n-p (format "Your %s header is too long (%d).  Really post? "
			     header length))
	 t)))
   ;; Check for multiple identical headers.
   (message-check 'multiple-headers
     (let (found)
       (while (and (not found)
		   (re-search-forward "^[^ \t:]+: " nil t))
	 (save-excursion
	   (or (re-search-forward
		(concat "^"
			(regexp-quote
			 (setq found
			       (buffer-substring
				(match-beginning 0) (- (match-end 0) 2))))
			":")
		nil t)
	       (setq found nil))))
       (if found
	   (y-or-n-p (format "Multiple %s headers.  Really post? " found))
	 t)))
   ;; Check for Version and Sendsys.
   (message-check 'sendsys
     (if (re-search-forward "^Sendsys:\\|^Version:" nil t)
	 (y-or-n-p
	  (format "The article contains a %s command.  Really post? "
		  (buffer-substring (match-beginning 0)
				    (1- (match-end 0)))))
       t))
   ;; See whether we can shorten Followup-To.
   (message-check 'shorten-followup-to
     (let ((newsgroups (message-fetch-field "newsgroups"))
	   (followup-to (message-fetch-field "followup-to"))
	   to)
       (when (and newsgroups
		  (string-match "," newsgroups)
		  (not followup-to)
		  (not
		   (zerop
		    (length
		     (setq to (completing-read
			       "Followups to (default: no Followup-To header) "
			       (mapcar #'list
				       (cons "poster"
					     (message-tokenize-header
					      newsgroups)))))))))
	 (goto-char (point-min))
	 (insert "Followup-To: " to "\n"))
       t))
   ;; Check "Shoot me".
   (message-check 'shoot
     (if (re-search-forward
	  "Message-ID.*.i-did-not-set--mail-host-address--so-tickle-me" nil t)
	 (y-or-n-p "You appear to have a misconfigured system.  Really post? ")
       t))
   ;; Check for Approved.
   (message-check 'approved
     (if (re-search-forward "^Approved:" nil t)
	 (y-or-n-p "The article contains an Approved header.  Really post? ")
       t))
   ;; Check the Message-ID header.
   (message-check 'message-id
     (let* ((case-fold-search t)
	    (message-id (message-fetch-field "message-id" t)))
       (or (not message-id)
	   ;; Is there an @ in the ID?
	   (and (string-match "@" message-id)
		;; Is there a dot in the ID?
		(string-match "@[^.]*\\." message-id)
		;; Does the ID end with a dot?
		(not (string-match "\\.>" message-id)))
	   (y-or-n-p
	    (format "The Message-ID looks strange: \"%s\".  Really post? "
		    message-id)))))
   ;; Check the Newsgroups & Followup-To headers.
   (message-check 'existing-newsgroups
     (let* ((case-fold-search t)
	    (newsgroups (message-fetch-field "newsgroups"))
	    (followup-to (message-fetch-field "followup-to"))
	    (groups (message-tokenize-header
		     (if followup-to
			 (concat newsgroups "," followup-to)
		       newsgroups)))
	    (post-method (if (functionp message-post-method)
			     (funcall message-post-method)
			   message-post-method))
	    ;; KLUDGE to handle nnvirtual groups.  Doing this right
	    ;; would probably involve a new nnoo function.
	    ;; -- Per Abrahamsen <abraham@dina.kvl.dk>, 2001-10-17.
	    (method (if (and (consp post-method)
			     (eq (car post-method) 'nnvirtual)
			     gnus-message-group-art)
			(let ((group (car (nnvirtual-find-group-art
					   (car gnus-message-group-art)
					   (cdr gnus-message-group-art)))))
			  (gnus-find-method-for-group group))
		      post-method))
	    (known-groups
	     (mapcar (lambda (n)
		       (gnus-group-name-decode
			(gnus-group-real-name n)
			(gnus-group-name-charset method n)))
		     (gnus-groups-from-server method)))
	    errors)
       (while groups
	 (when (and (not (equal (car groups) "poster"))
		    (not (member (car groups) known-groups))
		    (not (member (car groups) errors)))
	   (push (car groups) errors))
	 (pop groups))
       (cond
	;; Gnus is not running.
	((or (not (and (boundp 'gnus-active-hashtb)
		       gnus-active-hashtb))
	     (not (boundp 'gnus-read-active-file)))
	 t)
	;; We don't have all the group names.
	((and (or (not gnus-read-active-file)
		  (eq gnus-read-active-file 'some))
	      errors)
	 (y-or-n-p
	  (format
	   "Really use %s possibly unknown group%s: %s? "
	   (if (= (length errors) 1) "this" "these")
	   (if (= (length errors) 1) "" "s")
	   (mapconcat 'identity errors ", "))))
	;; There were no errors.
	((not errors)
	 t)
	;; There are unknown groups.
	(t
	 (y-or-n-p
	  (format
	   "Really post to %s unknown group%s: %s? "
	   (if (= (length errors) 1) "this" "these")
	   (if (= (length errors) 1) "" "s")
	   (mapconcat 'identity errors ", ")))))))
   ;; Check continuation headers.
   (message-check 'continuation-headers
     (goto-char (point-min))
     (let ((do-posting t))
       (while (re-search-forward "^[^ \t\n][^:\n]*$" nil t)
	 (if (y-or-n-p "Fix continuation lines? ")
	     (progn
	       (goto-char (match-beginning 0))
	       (insert " "))
	   (unless (y-or-n-p "Send anyway? ")
	     (setq do-posting nil))))
       do-posting))
   ;; Check the Newsgroups & Followup-To headers for syntax errors.
   (message-check 'valid-newsgroups
     (let ((case-fold-search t)
	   (headers '("Newsgroups" "Followup-To"))
	   header error)
       (while (and headers (not error))
	 (when (setq header (mail-fetch-field (car headers)))
	   (if (or
		(not
		 (string-match
		  "\\`\\([-+_&.a-zA-Z0-9]+\\)?\\(,[-+_&.a-zA-Z0-9]+\\)*\\'"
		  header))
		(memq
		 nil (mapcar
		      (lambda (g)
			(not (string-match "\\.\\'\\|\\.\\." g)))
		      (message-tokenize-header header ","))))
	       (setq error t)))
	 (unless error
	   (pop headers)))
       (if (not error)
	   t
	 (y-or-n-p
	  (format "The %s header looks odd: \"%s\".  Really post? "
		  (car headers) header)))))
   (message-check 'repeated-newsgroups
     (let ((case-fold-search t)
	   (headers '("Newsgroups" "Followup-To"))
	   header error groups group)
       (while (and headers
		   (not error))
	 (when (setq header (mail-fetch-field (pop headers)))
	   (setq groups (message-tokenize-header header ","))
	   (while (setq group (pop groups))
	     (when (member group groups)
	       (setq error group
		     groups nil)))))
       (if (not error)
	   t
	 (y-or-n-p
	  (format "Group %s is repeated in headers.  Really post? " error)))))
   ;; Check the From header.
   (message-check 'from
     (let* ((case-fold-search t)
	    (from (message-fetch-field "from"))
	    ad)
       (cond
	((not from)
	 (message "There is no From line.  Posting is denied.")
	 nil)
	((or (not (string-match
		   "@[^\\.]*\\."
		   (setq ad (nth 1 (mail-extract-address-components
				    from))))) ;larsi@ifi
	     (string-match "\\.\\." ad)	;larsi@ifi..uio
	     (string-match "@\\." ad)	;larsi@.ifi.uio
	     (string-match "\\.$" ad)	;larsi@ifi.uio.
	     (not (string-match "^[^@]+@[^@]+$" ad)) ;larsi.ifi.uio
	     (string-match "(.*).*(.*)" from)) ;(lars) (lars)
	 (message
	  "Denied posting -- the From looks strange: \"%s\"." from)
	 nil)
	((let ((addresses (rfc822-addresses from)))
	   (while (and addresses
		       (not (eq (string-to-char (car addresses)) ?\()))
	     (setq addresses (cdr addresses)))
	   addresses)
	 (message
	  "Denied posting -- bad From address: \"%s\"." from)
	 nil)
	(t t))))
   ;; Check the Reply-To header.
   (message-check 'reply-to
     (let* ((case-fold-search t)
	    (reply-to (message-fetch-field "reply-to"))
	    ad)
       (cond
	((not reply-to)
	 t)
	((string-match "," reply-to)
	 (y-or-n-p
	  (format "Multiple Reply-To addresses: \"%s\". Really post? "
		  reply-to)))
	((or (not (string-match
		   "@[^\\.]*\\."
		   (setq ad (nth 1 (mail-extract-address-components
				    reply-to))))) ;larsi@ifi
	     (string-match "\\.\\." ad)	;larsi@ifi..uio
	     (string-match "@\\." ad)	;larsi@.ifi.uio
	     (string-match "\\.$" ad)	;larsi@ifi.uio.
	     (not (string-match "^[^@]+@[^@]+$" ad)) ;larsi.ifi.uio
	     (string-match "(.*).*(.*)" reply-to)) ;(lars) (lars)
	 (y-or-n-p
	  (format
	   "The Reply-To looks strange: \"%s\". Really post? "
	   reply-to)))
	(t t))))))

(defun message-check-news-body-syntax ()
  (and
   ;; Check for long lines.
   (message-check 'long-lines
     (goto-char (point-min))
     (re-search-forward
      (concat "^" (regexp-quote mail-header-separator) "$"))
     (forward-line 1)
     (while (and
	     (or (looking-at
		  "<#\\(/\\)?\\(multipart\\|part\\|external\\|mml\\)")
		 (let ((p (point)))
		   (end-of-line)
		   (< (- (point) p) 80)))
	     (zerop (forward-line 1))))
     (or (bolp)
	 (eobp)
	 (y-or-n-p
	  "You have lines longer than 79 characters.  Really post? ")))
   ;; Check whether the article is empty.
   (message-check 'empty
     (goto-char (point-min))
     (re-search-forward
      (concat "^" (regexp-quote mail-header-separator) "$"))
     (forward-line 1)
     (let ((b (point)))
       (goto-char (point-max))
       (re-search-backward message-signature-separator nil t)
       (beginning-of-line)
       (or (re-search-backward "[^ \n\t]" b t)
	   (if (message-gnksa-enable-p 'empty-article)
	       (y-or-n-p "Empty article.  Really post? ")
	     (message "Denied posting -- Empty article.")
	     nil))))
   ;; Check for control characters.
   (message-check 'control-chars
     (if (re-search-forward
	  (mm-string-as-multibyte "[\000-\007\013\015-\032\034-\037\200-\237]")
	  nil t)
	 (y-or-n-p
	  "The article contains control characters.  Really post? ")
       t))
   ;; Check excessive size.
   (message-check 'size
     (if (> (buffer-size) 60000)
	 (y-or-n-p
	  (format "The article is %d octets long.  Really post? "
		  (buffer-size)))
       t))
   ;; Check whether any new text has been added.
   (message-check 'new-text
     (or
      (not message-checksum)
      (not (eq (message-checksum) message-checksum))
      (if (message-gnksa-enable-p 'quoted-text-only)
	  (y-or-n-p
	   "It looks like no new text has been added.  Really post? ")
	(message "Denied posting -- no new text has been added.")
	nil)))
   ;; Check the length of the signature.
   (message-check 'signature
     (goto-char (point-max))
     (if (> (count-lines (point) (point-max)) 5)
	 (y-or-n-p
	  (format
	   "Your .sig is %d lines; it should be max 4.  Really post? "
	   (1- (count-lines (point) (point-max)))))
       t))
   ;; Ensure that text follows last quoted portion.
   (message-check 'quoting-style
     (goto-char (point-max))
     (let ((no-problem t))
       (when (search-backward-regexp "^>[^\n]*\n" nil t)
	 (setq no-problem (search-forward-regexp "^[ \t]*[^>\n]" nil t)))
       (if no-problem
	   t
	 (if (message-gnksa-enable-p 'quoted-text-only)
	     (y-or-n-p "Your text should follow quoted text.  Really post? ")
	   ;; Ensure that
	   (goto-char (point-min))
	   (re-search-forward
	    (concat "^" (regexp-quote mail-header-separator) "$"))
	   (if (search-forward-regexp "^[ \t]*[^>\n]" nil t)
	       (y-or-n-p "Your text should follow quoted text.  Really post? ")
	     (message "Denied posting -- only quoted text.")
	     nil)))))))

(defun message-checksum ()
  "Return a \"checksum\" for the current buffer."
  (let ((sum 0))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward
       (concat "^" (regexp-quote mail-header-separator) "$"))
      (while (not (eobp))
	(when (not (looking-at "[ \t\n]"))
	  (setq sum (logxor (ash sum 1) (if (natnump sum) 0 1)
			    (char-after))))
	(forward-char 1)))
    sum))

(defun message-do-fcc ()
  "Process Fcc headers in the current buffer."
  (let ((case-fold-search t)
	(buf (current-buffer))
	list file
	(mml-externalize-attachments message-fcc-externalize-attachments))
    (save-excursion
      (save-restriction
	(message-narrow-to-headers)
	(setq file (message-fetch-field "fcc" t)))
      (when file
	(set-buffer (get-buffer-create " *message temp*"))
	(erase-buffer)
	(insert-buffer-substring buf)
	(message-encode-message-body)
	(save-restriction
	  (message-narrow-to-headers)
	  (while (setq file (message-fetch-field "fcc" t))
	    (push file list)
	    (message-remove-header "fcc" nil t))
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
	;; Process FCC operations.
	(while list
	  (setq file (pop list))
	  (if (string-match "^[ \t]*|[ \t]*\\(.*\\)[ \t]*$" file)
	      ;; Pipe the article to the program in question.
	      (call-process-region (point-min) (point-max) shell-file-name
				   nil nil nil shell-command-switch
				   (match-string 1 file))
	    ;; Save the article.
	    (setq file (expand-file-name file))
	    (unless (file-exists-p (file-name-directory file))
	      (make-directory (file-name-directory file) t))
	    (if (and message-fcc-handler-function
		     (not (eq message-fcc-handler-function 'rmail-output)))
		(funcall message-fcc-handler-function file)
	      (if (and (file-readable-p file) (mail-file-babyl-p file))
		  (rmail-output file 1 nil t)
		(let ((mail-use-rfc822 t))
		  (rmail-output file 1 t t))))))
	(kill-buffer (current-buffer))))))

(defun message-output (filename)
  "Append this article to Unix/babyl mail file FILENAME."
  (if (and (file-readable-p filename)
	   (mail-file-babyl-p filename))
      (gnus-output-to-rmail filename t)
    (gnus-output-to-mail filename t)))

(defun message-cleanup-headers ()
  "Do various automatic cleanups of the headers."
  ;; Remove empty lines in the header.
  (save-restriction
    (message-narrow-to-headers)
    ;; Remove blank lines.
    (while (re-search-forward "^[ \t]*\n" nil t)
      (replace-match "" t t))

    ;; Correct Newsgroups and Followup-To headers:  Change sequence of
    ;; spaces to comma and eliminate spaces around commas.  Eliminate
    ;; embedded line breaks.
    (goto-char (point-min))
    (while (re-search-forward "^\\(Newsgroups\\|Followup-To\\): +" nil t)
      (save-restriction
	(narrow-to-region
	 (point)
	 (if (re-search-forward "^[^ \t]" nil t)
	     (match-beginning 0)
	   (forward-line 1)
	   (point)))
	(goto-char (point-min))
	(while (re-search-forward "\n[ \t]+" nil t)
	  (replace-match " " t t))     ;No line breaks (too confusing)
	(goto-char (point-min))
	(while (re-search-forward "[ \t\n]*,[ \t\n]*\\|[ \t]+" nil t)
	  (replace-match "," t t))
	(goto-char (point-min))
	;; Remove trailing commas.
	(when (re-search-forward ",+$" nil t)
	  (replace-match "" t t))))))

(eval-when-compile (require 'parse-time))
(defun message-make-date (&optional now)
  "Make a valid data header.
If NOW, use that time instead."
  (require 'parse-time)
  (let* ((now (or now (current-time)))
	 (zone (nth 8 (decode-time now)))
	 (sign "+"))
    (when (< zone 0)
      (setq sign "-")
      (setq zone (- zone)))
    (concat
     ;; The day name of the %a spec is locale-specific.  Pfff.
     (format "%s, " (capitalize (car (rassoc (nth 6 (decode-time now))
					     parse-time-weekdays))))
     (format-time-string "%d" now)
     ;; The month name of the %b spec is locale-specific.  Pfff.
     (format " %s "
	     (capitalize (car (rassoc (nth 4 (decode-time now))
				      parse-time-months))))
     (format-time-string "%Y %H:%M:%S " now)
     ;; We do all of this because XEmacs doesn't have the %z spec.
     (format "%s%02d%02d" sign (/ zone 3600) (/ (% zone 3600) 60)))))

(defun message-make-message-id ()
  "Make a unique Message-ID."
  (concat "<" (message-unique-id)
	  (let ((psubject (save-excursion (message-fetch-field "subject")))
		(psupersedes
		 (save-excursion (message-fetch-field "supersedes"))))
	    (if (or
		 (and message-reply-headers
		      (mail-header-references message-reply-headers)
		      (mail-header-subject message-reply-headers)
		      psubject
		      (not (string=
			    (message-strip-subject-re
			     (mail-header-subject message-reply-headers))
			    (message-strip-subject-re psubject))))
		 (and psupersedes
		      (string-match "_-_@" psupersedes)))
		"_-_" ""))
	  "@" (message-make-fqdn) ">"))

(defvar message-unique-id-char nil)

;; If you ever change this function, make sure the new version
;; cannot generate IDs that the old version could.
;; You might for example insert a "." somewhere (not next to another dot
;; or string boundary), or modify the "fsf" string.
(defun message-unique-id ()
  ;; Don't use microseconds from (current-time), they may be unsupported.
  ;; Instead we use this randomly inited counter.
  (setq message-unique-id-char
	(% (1+ (or message-unique-id-char (logand (random t) (1- (lsh 1 20)))))
	   ;; (current-time) returns 16-bit ints,
	   ;; and 2^16*25 just fits into 4 digits i base 36.
	   (* 25 25)))
  (let ((tm (current-time)))
    (concat
     (if (memq system-type '(ms-dos emx vax-vms))
	 (let ((user (downcase (user-login-name))))
	   (while (string-match "[^a-z0-9_]" user)
	     (aset user (match-beginning 0) ?_))
	   user)
       (message-number-base36 (user-uid) -1))
     (message-number-base36 (+ (car tm)
			       (lsh (% message-unique-id-char 25) 16)) 4)
     (message-number-base36 (+ (nth 1 tm)
			       (lsh (/ message-unique-id-char 25) 16)) 4)
     ;; Append a given name, because while the generated ID is unique
     ;; to this newsreader, other newsreaders might otherwise generate
     ;; the same ID via another algorithm.
     ".fsf")))

(defun message-number-base36 (num len)
  (if (if (< len 0)
	  (<= num 0)
	(= len 0))
      ""
    (concat (message-number-base36 (/ num 36) (1- len))
	    (char-to-string (aref "zyxwvutsrqponmlkjihgfedcba9876543210"
				  (% num 36))))))

(defun message-make-organization ()
  "Make an Organization header."
  (let* ((organization
	  (when message-user-organization
	    (if (functionp message-user-organization)
		(funcall message-user-organization)
	      message-user-organization))))
    (with-temp-buffer
      (mm-enable-multibyte)
      (cond ((stringp organization)
	     (insert organization))
	    ((and (eq t organization)
		  message-user-organization-file
		  (file-exists-p message-user-organization-file))
	     (insert-file-contents message-user-organization-file)))
      (goto-char (point-min))
      (while (re-search-forward "[\t\n]+" nil t)
	(replace-match "" t t))
      (unless (zerop (buffer-size))
	(buffer-string)))))

(defun message-make-lines ()
  "Count the number of lines and return numeric string."
  (save-excursion
    (save-restriction
      (widen)
      (message-goto-body)
      (int-to-string (count-lines (point) (point-max))))))

(defun message-make-references ()
  "Return the References header for this message."
  (when message-reply-headers
    (let ((message-id (mail-header-message-id message-reply-headers))
	  (references (mail-header-references message-reply-headers))
	  new-references)
      (if (or references message-id)
	  (concat (or references "") (and references " ")
		  (or message-id ""))
	nil))))

(defun message-make-in-reply-to ()
  "Return the In-Reply-To header for this message."
  (when message-reply-headers
    (let ((from (mail-header-from message-reply-headers))
	  (date (mail-header-date message-reply-headers))
	  (msg-id (mail-header-message-id message-reply-headers)))
      (when from
	(let ((name (mail-extract-address-components from)))
	  (concat msg-id (if msg-id " (")
		  (or (car name)
		      (nth 1 name))
		  "'s message of \""
		  (if (or (not date) (string= date ""))
		      "(unknown date)" date)
		  "\"" (if msg-id ")")))))))

(defun message-make-distribution ()
  "Make a Distribution header."
  (let ((orig-distribution (message-fetch-reply-field "distribution")))
    (cond ((functionp message-distribution-function)
	   (funcall message-distribution-function))
	  (t orig-distribution))))

(defun message-make-expires ()
  "Return an Expires header based on `message-expires'."
  (let ((current (current-time))
	(future (* 1.0 message-expires 60 60 24)))
    ;; Add the future to current.
    (setcar current (+ (car current) (round (/ future (expt 2 16)))))
    (setcar (cdr current) (+ (nth 1 current) (% (round future) (expt 2 16))))
    (message-make-date current)))

(defun message-make-path ()
  "Return uucp path."
  (let ((login-name (user-login-name)))
    (cond ((null message-user-path)
	   (concat (system-name) "!" login-name))
	  ((stringp message-user-path)
	   ;; Support GENERICPATH.  Suggested by vixie@decwrl.dec.com.
	   (concat message-user-path "!" login-name))
	  (t login-name))))

(defun message-make-from ()
  "Make a From header."
  (let* ((style message-from-style)
	 (login (message-make-address))
	 (fullname
	  (or (and (boundp 'user-full-name)
		   user-full-name)
	      (user-full-name))))
    (when (string= fullname "&")
      (setq fullname (user-login-name)))
    (with-temp-buffer
      (mm-enable-multibyte)
      (cond
       ((or (null style)
	    (equal fullname ""))
	(insert login))
       ((or (eq style 'angles)
	    (and (not (eq style 'parens))
		 ;; Use angles if no quoting is needed, or if parens would
		 ;; need quoting too.
		 (or (not (string-match "[^- !#-'*+/-9=?A-Z^-~]" fullname))
		     (let ((tmp (concat fullname nil)))
		       (while (string-match "([^()]*)" tmp)
			 (aset tmp (match-beginning 0) ?-)
			 (aset tmp (1- (match-end 0)) ?-))
		       (string-match "[\\()]" tmp)))))
	(insert fullname)
	(goto-char (point-min))
 	;; Look for a character that cannot appear unquoted
 	;; according to RFC 822.
 	(when (re-search-forward "[^- !#-'*+/-9=?A-Z^-~]" nil 1)
 	  ;; Quote fullname, escaping specials.
 	  (goto-char (point-min))
 	  (insert "\"")
 	  (while (re-search-forward "[\"\\]" nil 1)
 	    (replace-match "\\\\\\&" t))
 	  (insert "\""))
	(insert " <" login ">"))
       (t				; 'parens or default
	(insert login " (")
	(let ((fullname-start (point)))
	  (insert fullname)
	  (goto-char fullname-start)
	  ;; RFC 822 says \ and nonmatching parentheses
	  ;; must be escaped in comments.
	  ;; Escape every instance of ()\ ...
	  (while (re-search-forward "[()\\]" nil 1)
	    (replace-match "\\\\\\&" t))
	  ;; ... then undo escaping of matching parentheses,
	  ;; including matching nested parentheses.
	  (goto-char fullname-start)
	  (while (re-search-forward
		  "\\(\\=\\|[^\\]\\(\\\\\\\\\\)*\\)\\\\(\\(\\([^\\]\\|\\\\\\\\\\)*\\)\\\\)"
		  nil 1)
	    (replace-match "\\1(\\3)" t)
	    (goto-char fullname-start)))
	(insert ")")))
      (buffer-string))))

(defun message-make-sender ()
  "Return the \"real\" user address.
This function tries to ignore all user modifications, and
give as trustworthy answer as possible."
  (concat (user-login-name) "@" (system-name)))

(defun message-make-address ()
  "Make the address of the user."
  (or (message-user-mail-address)
      (concat (user-login-name) "@" (message-make-domain))))

(defun message-user-mail-address ()
  "Return the pertinent part of `user-mail-address'."
  (when (and user-mail-address
	     (string-match "@.*\\." user-mail-address))
    (if (string-match " " user-mail-address)
	(nth 1 (mail-extract-address-components user-mail-address))
      user-mail-address)))

(defun message-sendmail-envelope-from ()
  "Return the envelope from."
  (cond ((eq message-sendmail-envelope-from 'header)
	 (nth 1 (mail-extract-address-components
		 (message-fetch-field "from"))))
	((stringp message-sendmail-envelope-from)
	 message-sendmail-envelope-from)
	(t
	 (message-make-address))))

(defun message-make-fqdn ()
  "Return user's fully qualified domain name."
  (let* ((system-name (system-name))
	 (user-mail (message-user-mail-address))
	 (user-domain
	  (if (and user-mail
		   (string-match "@\\(.*\\)\\'" user-mail))
	      (match-string 1 user-mail)))
	 (case-fold-search t))
    (cond
     ((and message-user-fqdn
	   (stringp message-user-fqdn)
	   (string-match message-valid-fqdn-regexp message-user-fqdn)
	   (not (string-match message-bogus-system-names message-user-fqdn)))
      message-user-fqdn)
     ;; `message-user-fqdn' seems to be valid
     ((and (string-match message-valid-fqdn-regexp system-name)
	   (not (string-match message-bogus-system-names system-name)))
      ;; `system-name' returned the right result.
      system-name)
     ;; Try `mail-host-address'.
     ((and (boundp 'mail-host-address)
	   (stringp mail-host-address)
	   (string-match message-valid-fqdn-regexp mail-host-address)
	   (not (string-match message-bogus-system-names mail-host-address)))
      mail-host-address)
     ;; We try `user-mail-address' as a backup.
     ((and user-domain
	   (stringp user-domain)
	   (string-match message-valid-fqdn-regexp user-domain)
	   (not (string-match message-bogus-system-names user-domain)))
      user-domain)
     ;; Default to this bogus thing.
     (t
      (concat system-name
	      ".i-did-not-set--mail-host-address--so-tickle-me")))))

(defun message-make-host-name ()
  "Return the name of the host."
  (let ((fqdn (message-make-fqdn)))
    (string-match "^[^.]+\\." fqdn)
    (substring fqdn 0 (1- (match-end 0)))))

(defun message-make-domain ()
  "Return the domain name."
  (or mail-host-address
      (message-make-fqdn)))

(defun message-to-list-only ()
  "Send a message to the list only.
Remove all addresses but the list address from To and Cc headers."
  (interactive)
  (let ((listaddr (message-make-mail-followup-to t)))
    (when listaddr
      (save-excursion
	(message-remove-header "to")
	(message-remove-header "cc")
	(message-position-on-field "To" "X-Draft-From")
	(insert listaddr)))))

(defun message-make-mail-followup-to (&optional only-show-subscribed)
  "Return the Mail-Followup-To header.
If passed the optional argument ONLY-SHOW-SUBSCRIBED only return the
subscribed address (and not the additional To and Cc header contents)."
  (let* ((case-fold-search t)
	 (to (message-fetch-field "To"))
	 (cc (message-fetch-field "cc"))
	 (msg-recipients (concat to (and to cc ", ") cc))
	 (recipients
	  (mapcar 'mail-strip-quoted-names
		  (message-tokenize-header msg-recipients)))
	 (file-regexps
	  (if message-subscribed-address-file
	      (let (begin end item re)
		(save-excursion
		  (with-temp-buffer
		    (insert-file-contents message-subscribed-address-file)
		    (while (not (eobp))
		      (setq begin (point))
		      (forward-line 1)
		      (setq end (point))
		      (if (bolp) (setq end (1- end)))
		      (setq item (regexp-quote (buffer-substring begin end)))
		      (if re (setq re (concat re "\\|" item))
			(setq re (concat "\\`\\(" item))))
		    (and re (list (concat re "\\)\\'"))))))))
	 (mft-regexps (apply 'append message-subscribed-regexps
			     (mapcar 'regexp-quote
				     message-subscribed-addresses)
			     file-regexps
			     (mapcar 'funcall
				     message-subscribed-address-functions))))
    (save-match-data
      (let ((subscribed-lists nil)
	    (list
	     (loop for recipient in recipients
	       when (loop for regexp in mft-regexps
		      when (string-match regexp recipient) return t)
	       return recipient)))
	(when list
	  (if only-show-subscribed
	      list
	    msg-recipients))))))

(defun message-idna-to-ascii-rhs-1 (header)
  "Interactively potentially IDNA encode domain names in HEADER."
  (let ((field (message-fetch-field header))
	rhs ace  address)
    (when field
      (dolist (address (mail-header-parse-addresses field))
	(setq address (car address)
	      rhs (downcase (or (cadr (split-string address "@")) ""))
	      ace (downcase (idna-to-ascii rhs)))
	(when (and (not (equal rhs ace))
		   (or (not (eq message-use-idna 'ask))
		       (y-or-n-p (format "Replace %s with %s? " rhs ace))))
	  (goto-char (point-min))
	  (while (re-search-forward (concat "^" header ":") nil t)
	    (message-narrow-to-field)
	    (while (search-forward (concat "@" rhs) nil t)
	      (replace-match (concat "@" ace) t t))
	    (goto-char (point-max))
	    (widen)))))))

(defun message-idna-to-ascii-rhs ()
  "Possibly IDNA encode non-ASCII domain names in From:, To: and Cc: headers.
See `message-idna-encode'."
  (interactive)
  (when message-use-idna
    (save-excursion
      (save-restriction
	(message-narrow-to-head)
	(message-idna-to-ascii-rhs-1 "From")
	(message-idna-to-ascii-rhs-1 "To")
	(message-idna-to-ascii-rhs-1 "Cc")))))

(defun message-generate-headers (headers)
  "Prepare article HEADERS.
Headers already prepared in the buffer are not modified."
  (setq headers (append headers message-required-headers))
  (save-restriction
    (message-narrow-to-headers)
    (let* ((Date (message-make-date))
	   (Message-ID (message-make-message-id))
	   (Organization (message-make-organization))
	   (From (message-make-from))
	   (Path (message-make-path))
	   (Subject nil)
	   (Newsgroups nil)
	   (In-Reply-To (message-make-in-reply-to))
	   (References (message-make-references))
	   (To nil)
	   (Distribution (message-make-distribution))
	   (Lines (message-make-lines))
	   (User-Agent message-newsreader)
	   (Expires (message-make-expires))
	   (case-fold-search t)
	   (optionalp nil)
	   header value elem header-string)
      ;; First we remove any old generated headers.
      (let ((headers message-deletable-headers))
	(unless (buffer-modified-p)
	  (setq headers (delq 'Message-ID (copy-sequence headers))))
	(while headers
	  (goto-char (point-min))
	  (and (re-search-forward
		(concat "^" (symbol-name (car headers)) ": *") nil t)
	       (get-text-property (1+ (match-beginning 0)) 'message-deletable)
	       (message-delete-line))
	  (pop headers)))
      ;; Go through all the required headers and see if they are in the
      ;; articles already.  If they are not, or are empty, they are
      ;; inserted automatically - except for Subject, Newsgroups and
      ;; Distribution.
      (while headers
	(goto-char (point-min))
	(setq elem (pop headers))
	(if (consp elem)
	    (if (eq (car elem) 'optional)
		(setq header (cdr elem)
		      optionalp t)
	      (setq header (car elem)))
	  (setq header elem))
	(setq header-string  (if (stringp header)
				 header
			       (symbol-name header)))
	(when (or (not (re-search-forward
			(concat "^"
				(regexp-quote (downcase header-string))
				":")
			nil t))
		  (progn
		    ;; The header was found.  We insert a space after the
		    ;; colon, if there is none.
		    (if (/= (char-after) ? ) (insert " ") (forward-char 1))
		    ;; Find out whether the header is empty.
		    (looking-at "[ \t]*\n[^ \t]")))
	  ;; So we find out what value we should insert.
	  (setq value
		(cond
		 ((and (consp elem)
		       (eq (car elem) 'optional)
		       (not (member header-string message-inserted-headers)))
		  ;; This is an optional header.  If the cdr of this
		  ;; is something that is nil, then we do not insert
		  ;; this header.
		  (setq header (cdr elem))
		  (or (and (functionp (cdr elem))
			   (funcall (cdr elem)))
		      (and (boundp (cdr elem))
			   (symbol-value (cdr elem)))))
		 ((consp elem)
		  ;; The element is a cons.  Either the cdr is a
		  ;; string to be inserted verbatim, or it is a
		  ;; function, and we insert the value returned from
		  ;; this function.
		  (or (and (stringp (cdr elem))
			   (cdr elem))
		      (and (functionp (cdr elem))
			   (funcall (cdr elem)))))
		 ((and (boundp header)
		       (symbol-value header))
		  ;; The element is a symbol.  We insert the value
		  ;; of this symbol, if any.
		  (symbol-value header))
		 ((not (message-check-element header))
		  ;; We couldn't generate a value for this header,
		  ;; so we just ask the user.
		  (read-from-minibuffer
		   (format "Empty header for %s; enter value: " header)))))
	  ;; Finally insert the header.
	  (when (and value
		     (not (equal value "")))
	    (save-excursion
	      (if (bolp)
		  (progn
		    ;; This header didn't exist, so we insert it.
		    (goto-char (point-max))
		    (let ((formatter
			   (cdr (assq header message-header-format-alist))))
		      (if formatter
			  (funcall formatter header value)
			(insert header-string ": " value))
		      ;; We check whether the value was ended by a
		      ;; newline.  If now, we insert one.
		      (unless (bolp)
			(insert "\n"))
		      (forward-line -1)))
		;; The value of this header was empty, so we clear
		;; totally and insert the new value.
		(delete-region (point) (gnus-point-at-eol))
		;; If the header is optional, and the header was
		;; empty, we con't insert it anyway.
		(unless optionalp
		  (push header-string message-inserted-headers)
		  (insert value)))
	      ;; Add the deletable property to the headers that require it.
	      (and (memq header message-deletable-headers)
		   (progn (beginning-of-line) (looking-at "[^:]+: "))
		   (add-text-properties
		    (point) (match-end 0)
		    '(message-deletable t face italic) (current-buffer)))))))
      ;; Insert new Sender if the From is strange.
      (let ((from (message-fetch-field "from"))
	    (sender (message-fetch-field "sender"))
	    (secure-sender (message-make-sender)))
	(when (and from
		   (not (message-check-element 'sender))
		   (not (string=
			 (downcase
			  (cadr (mail-extract-address-components from)))
			 (downcase secure-sender)))
		   (or (null sender)
		       (not
			(string=
			 (downcase
			  (cadr (mail-extract-address-components sender)))
			 (downcase secure-sender)))))
	  (goto-char (point-min))
	  ;; Rename any old Sender headers to Original-Sender.
	  (when (re-search-forward "^\\(Original-\\)*Sender:" nil t)
	    (beginning-of-line)
	    (insert "Original-")
	    (beginning-of-line))
	  (when (or (message-news-p)
		    (string-match "@.+\\.." secure-sender))
	    (insert "Sender: " secure-sender "\n"))))
      ;; Check for IDNA
      (message-idna-to-ascii-rhs))))

(defun message-insert-courtesy-copy ()
  "Insert a courtesy message in mail copies of combined messages."
  (let (newsgroups)
    (save-excursion
      (save-restriction
	(message-narrow-to-headers)
	(when (setq newsgroups (message-fetch-field "newsgroups"))
	  (goto-char (point-max))
	  (insert "Posted-To: " newsgroups "\n")))
      (forward-line 1)
      (when message-courtesy-message
	(cond
	 ((string-match "%s" message-courtesy-message)
	  (insert (format message-courtesy-message newsgroups)))
	 (t
	  (insert message-courtesy-message)))))))

;;;
;;; Setting up a message buffer
;;;

(defun message-fill-address (header value)
  (save-restriction
    (narrow-to-region (point) (point))
    (insert (capitalize (symbol-name header))
	    ": "
	    (if (consp value) (car value) value)
	    "\n")
    (narrow-to-region (point-min) (1- (point-max)))
    (let (quoted last)
      (goto-char (point-min))
      (while (not (eobp))
	(skip-chars-forward "^,\"" (point-max))
	(if (or (eq (char-after) ?,)
		(eobp))
	    (when (not quoted)
	      (if (and (> (current-column) 78)
		       last)
		  (progn
		    (save-excursion
		      (goto-char last)
		      (insert "\n\t"))
		    (setq last (1+ (point))))
		(setq last (1+ (point)))))
	  (setq quoted (not quoted)))
	(unless (eobp)
	  (forward-char 1))))
    (goto-char (point-max))
    (widen)
    (forward-line 1)))

(defun message-split-line ()
  "Split current line, moving portion beyond point vertically down.
If the current line has `message-yank-prefix', insert it on the new line."
  (interactive "*")
  (condition-case nil
      (split-line message-yank-prefix) ;; Emacs 22.1+ supports arg.
    (error
     (split-line))))

(defun message-fill-header (header value)
  (let ((begin (point))
	(fill-column 78)
	(fill-prefix "\t"))
    (insert (capitalize (symbol-name header))
	    ": "
	    (if (consp value) (car value) value)
	    "\n")
    (save-restriction
      (narrow-to-region begin (point))
      (fill-region-as-paragraph begin (point))
      ;; Tapdance around looong Message-IDs.
      (forward-line -1)
      (when (looking-at "[ \t]*$")
	(message-delete-line))
      (goto-char begin)
      (re-search-forward ":" nil t)
      (when (looking-at "\n[ \t]+")
	(replace-match " " t t))
      (goto-char (point-max)))))

(defun message-shorten-1 (list cut surplus)
  "Cut SURPLUS elements out of LIST, beginning with CUTth one."
  (setcdr (nthcdr (- cut 2) list)
	  (nthcdr (+ (- cut 2) surplus 1) list)))

(defun message-shorten-references (header references)
  "Trim REFERENCES to be 21 Message-ID long or less, and fold them.
If folding is disallowed, also check that the REFERENCES are less
than 988 characters long, and if they are not, trim them until they are."
  (let ((maxcount 21)
	(count 0)
	(cut 2)
	refs)
    (with-temp-buffer
      (insert references)
      (goto-char (point-min))
      ;; Cons a list of valid references.
      (while (re-search-forward "<[^>]+>" nil t)
	(push (match-string 0) refs))
      (setq refs (nreverse refs)
	    count (length refs)))

    ;; If the list has more than MAXCOUNT elements, trim it by
    ;; removing the CUTth element and the required number of
    ;; elements that follow.
    (when (> count maxcount)
      (let ((surplus (- count maxcount)))
	(message-shorten-1 refs cut surplus)
	(decf count surplus)))

    ;; If folding is disallowed, make sure the total length (including
    ;; the spaces between) will be less than MAXSIZE characters.
    ;;
    ;; Only disallow folding for News messages. At this point the headers
    ;; have not been generated, thus we use message-this-is-news directly.
    (when (and message-this-is-news message-cater-to-broken-inn)
      (let ((maxsize 988)
	    (totalsize (+ (apply #'+ (mapcar #'length refs))
			  (1- count)))
	    (surplus 0)
	    (ptr (nthcdr (1- cut) refs)))
	;; Decide how many elements to cut off...
	(while (> totalsize maxsize)
	  (decf totalsize (1+ (length (car ptr))))
	  (incf surplus)
	  (setq ptr (cdr ptr)))
	;; ...and do it.
	(when (> surplus 0)
	  (message-shorten-1 refs cut surplus))))

    ;; Finally, collect the references back into a string and insert
    ;; it into the buffer.
    (let ((refstring (mapconcat #'identity refs " ")))
      (if (and message-this-is-news message-cater-to-broken-inn)
	  (insert (capitalize (symbol-name header)) ": "
		  refstring "\n")
	(message-fill-header header refstring)))))

(defun message-position-point ()
  "Move point to where the user probably wants to find it."
  (message-narrow-to-headers)
  (cond
   ((re-search-forward "^[^:]+:[ \t]*$" nil t)
    (search-backward ":" )
    (widen)
    (forward-char 1)
    (if (eq (char-after) ? )
	(forward-char 1)
      (insert " ")))
   (t
    (goto-char (point-max))
    (widen)
    (forward-line 1)
    (unless (looking-at "$")
      (forward-line 2)))
   (sit-for 0)))

(defcustom message-beginning-of-line t
  "Whether \\<message-mode-map>\\[message-beginning-of-line]\
 goes to beginning of header values."
  :version "22.1"
  :group 'message-buffers
  :link '(custom-manual "(message)Movement")
  :type 'boolean)

(defun message-beginning-of-line (&optional n)
  "Move point to beginning of header value or to beginning of line.
The prefix argument N is passed directly to `beginning-of-line'.

This command is identical to `beginning-of-line' if point is
outside the message header or if the option `message-beginning-of-line'
is nil.

If point is in the message header and on a (non-continued) header
line, move point to the beginning of the header value or the beginning of line,
whichever is closer.  If point is already at beginning of line, move point to
beginning of header value.  Therefore, repeated calls will toggle point
between beginning of field and beginning of line."
  (interactive "p")
  (let ((zrs 'zmacs-region-stays))
    (when (and (interactive-p) (boundp zrs))
      (set zrs t)))
  (if (and message-beginning-of-line
	   (message-point-in-header-p))
      (let* ((here (point))
	     (bol (progn (beginning-of-line n) (point)))
	     (eol (gnus-point-at-eol))
	     (eoh (re-search-forward ": *" eol t)))
	(goto-char
	 (if (and eoh (or (< eoh here) (= bol here)))
	     eoh bol)))
    (beginning-of-line n)))

(defun message-buffer-name (type &optional to group)
  "Return a new (unique) buffer name based on TYPE and TO."
  (cond
   ;; Generate a new buffer name The Message Way.
   ((eq message-generate-new-buffers 'unique)
    (generate-new-buffer-name
     (concat "*" type
	     (if to
		 (concat " to "
			 (or (car (mail-extract-address-components to))
			     to) "")
	       "")
	     (if (and group (not (string= group ""))) (concat " on " group) "")
	     "*")))
   ;; Check whether `message-generate-new-buffers' is a function,
   ;; and if so, call it.
   ((functionp message-generate-new-buffers)
    (funcall message-generate-new-buffers type to group))
   ((eq message-generate-new-buffers 'unsent)
    (generate-new-buffer-name
     (concat "*unsent " type
	     (if to
		 (concat " to "
			 (or (car (mail-extract-address-components to))
			     to) "")
	       "")
	     (if (and group (not (string= group ""))) (concat " on " group) "")
	     "*")))
   ;; Use standard name.
   (t
    (format "*%s message*" type))))

(defun message-pop-to-buffer (name)
  "Pop to buffer NAME, and warn if it already exists and is modified."
  (let ((buffer (get-buffer name)))
    (if (and buffer
	     (buffer-name buffer))
	(progn
	  (set-buffer (pop-to-buffer buffer))
	  (when (and (buffer-modified-p)
		     (not (y-or-n-p
			   "Message already being composed; erase? ")))
	    (error "Message being composed")))
      (set-buffer (pop-to-buffer name)))
    (erase-buffer)
    (message-mode)))

(defun message-do-send-housekeeping ()
  "Kill old message buffers."
  ;; We might have sent this buffer already.  Delete it from the
  ;; list of buffers.
  (setq message-buffer-list (delq (current-buffer) message-buffer-list))
  (while (and message-max-buffers
	      message-buffer-list
	      (>= (length message-buffer-list) message-max-buffers))
    ;; Kill the oldest buffer -- unless it has been changed.
    (let ((buffer (pop message-buffer-list)))
      (when (and (buffer-name buffer)
		 (not (buffer-modified-p buffer)))
	(kill-buffer buffer))))
  ;; Rename the buffer.
  (if message-send-rename-function
      (funcall message-send-rename-function)
    ;; Note: mail-abbrevs of XEmacs renames buffer name behind Gnus.
    (when (string-match
	   "\\`\\*\\(sent \\|unsent \\)?\\(.+\\)\\*[^\\*]*\\|\\`mail to "
	   (buffer-name))
      (let ((name (match-string 2 (buffer-name)))
	    to group)
	(if (not (or (null name)
		     (string-equal name "mail")
		     (string-equal name "posting")))
	    (setq name (concat "*sent " name "*"))
	  (message-narrow-to-headers)
	  (setq to (message-fetch-field "to"))
	  (setq group (message-fetch-field "newsgroups"))
	  (widen)
	  (setq name
		(cond
		 (to (concat "*sent mail to "
			     (or (car (mail-extract-address-components to))
				 to) "*"))
		 ((and group (not (string= group "")))
		  (concat "*sent posting on " group "*"))
		 (t "*sent mail*"))))
	(unless (string-equal name (buffer-name))
	  (rename-buffer name t)))))
  ;; Push the current buffer onto the list.
  (when message-max-buffers
    (setq message-buffer-list
	  (nconc message-buffer-list (list (current-buffer))))))

(defun message-mail-user-agent ()
  (let ((mua (cond
	      ((not message-mail-user-agent) nil)
	      ((eq message-mail-user-agent t) mail-user-agent)
	      (t message-mail-user-agent))))
    (if (memq mua '(message-user-agent gnus-user-agent))
	nil
      mua)))

(defun message-setup (headers &optional replybuffer actions switch-function)
  (let ((mua (message-mail-user-agent))
	subject to field yank-action)
    (if (not (and message-this-is-mail mua))
	(message-setup-1 headers replybuffer actions)
      (if replybuffer
	  (setq yank-action (list 'insert-buffer replybuffer)))
      (setq headers (copy-sequence headers))
      (setq field (assq 'Subject headers))
      (when field
	(setq subject (cdr field))
	(setq headers (delq field headers)))
      (setq field (assq 'To headers))
      (when field
	(setq to (cdr field))
	(setq headers (delq field headers)))
      (let ((mail-user-agent mua))
	(compose-mail to subject
		      (mapcar (lambda (item)
				(cons
				 (format "%s" (car item))
				 (cdr item)))
			      headers)
		      nil switch-function yank-action actions)))))

(defun message-headers-to-generate (headers included-headers excluded-headers)
  "Return a list that includes all headers from HEADERS.
If INCLUDED-HEADERS is a list, just include those headers.  If if is
t, include all headers.  In any case, headers from EXCLUDED-HEADERS
are not included."
  (let ((result nil)
	header-name)
    (dolist (header headers)
      (setq header-name (cond
			 ((and (consp header)
			       (eq (car header) 'optional))
			  ;; On the form (optional . Header)
			  (cdr header))
			 ((consp header)
			  ;; On the form (Header . function)
			  (car header))
			 (t
			  ;; Just a Header.
			  header)))
      (when (and (not (memq header-name excluded-headers))
		 (or (eq included-headers t)
		     (memq header-name included-headers)))
	(push header result)))
    (nreverse result)))

(defun message-setup-1 (headers &optional replybuffer actions)
  (dolist (action actions)
    (condition-case nil
	(add-to-list 'message-send-actions
		     `(apply ',(car action) ',(cdr action)))))
  (setq message-reply-buffer replybuffer)
  (goto-char (point-min))
  ;; Insert all the headers.
  (mail-header-format
   (let ((h headers)
	 (alist message-header-format-alist))
     (while h
       (unless (assq (caar h) message-header-format-alist)
	 (push (list (caar h)) alist))
       (pop h))
     alist)
   headers)
  (delete-region (point) (progn (forward-line -1) (point)))
  (when message-default-headers
    (insert message-default-headers)
    (or (bolp) (insert ?\n)))
  (put-text-property
   (point)
   (progn
     (insert mail-header-separator "\n")
     (1- (point)))
   'read-only nil)
  (forward-line -1)
  (when (message-news-p)
    (when message-default-news-headers
      (insert message-default-news-headers)
      (or (bolp) (insert ?\n)))
    (when message-generate-headers-first
      (message-generate-headers
       (message-headers-to-generate
	(append message-required-news-headers
		message-required-headers)
	message-generate-headers-first
	'(Lines Subject)))))
  (when (message-mail-p)
    (when message-default-mail-headers
      (insert message-default-mail-headers)
      (or (bolp) (insert ?\n)))
    (save-restriction
      (message-narrow-to-headers)
      (if message-alternative-emails
	  (message-use-alternative-email-as-from)))
    (when message-generate-headers-first
      (message-generate-headers
       (message-headers-to-generate
	(append message-required-mail-headers
		message-required-headers)
	message-generate-headers-first
	'(Lines Subject)))))
  (run-hooks 'message-signature-setup-hook)
  (message-insert-signature)
  (save-restriction
    (message-narrow-to-headers)
    (run-hooks 'message-header-setup-hook))
  (set-buffer-modified-p nil)
  (setq buffer-undo-list nil)
  (run-hooks 'message-setup-hook)
  (message-position-point)
  (undo-boundary))

(defun message-set-auto-save-file-name ()
  "Associate the message buffer with a file in the drafts directory."
  (when message-auto-save-directory
    (unless (file-directory-p
	     (directory-file-name message-auto-save-directory))
      (make-directory message-auto-save-directory t))
    (if (gnus-alive-p)
	(setq message-draft-article
	      (nndraft-request-associate-buffer "drafts"))
      (setq buffer-file-name (expand-file-name
			      (if (memq system-type
					'(ms-dos ms-windows windows-nt
						 cygwin cygwin32 win32 w32
						 mswindows))
				  "message"
				"*message*")
			      message-auto-save-directory))
      (setq buffer-auto-save-file-name (make-auto-save-file-name)))
    (clear-visited-file-modtime)
    (setq buffer-file-coding-system message-draft-coding-system)))

(defun message-disassociate-draft ()
  "Disassociate the message buffer from the drafts directory."
  (when message-draft-article
    (nndraft-request-expire-articles
     (list message-draft-article) "drafts" nil t)))

(defun message-insert-headers ()
  "Generate the headers for the article."
  (interactive)
  (save-excursion
    (save-restriction
      (message-narrow-to-headers)
      (when (message-news-p)
	(message-generate-headers
	 (delq 'Lines
	       (delq 'Subject
		     (copy-sequence message-required-news-headers)))))
      (when (message-mail-p)
	(message-generate-headers
	 (delq 'Lines
	       (delq 'Subject
		     (copy-sequence message-required-mail-headers))))))))



;;;
;;; Commands for interfacing with message
;;;

;;;###autoload
(defun message-mail (&optional to subject
			       other-headers continue switch-function
			       yank-action send-actions)
  "Start editing a mail message to be sent.
OTHER-HEADERS is an alist of header/value pairs."
  (interactive)
  (let ((message-this-is-mail t) replybuffer)
    (unless (message-mail-user-agent)
      (message-pop-to-buffer (message-buffer-name "mail" to)))
    ;; FIXME: message-mail should do something if YANK-ACTION is not
    ;; insert-buffer.
    (and (consp yank-action) (eq (car yank-action) 'insert-buffer)
	 (setq replybuffer (nth 1 yank-action)))
    (message-setup
     (nconc
      `((To . ,(or to "")) (Subject . ,(or subject "")))
      (when other-headers other-headers))
     replybuffer send-actions)
    ;; FIXME: Should return nil if failure.
    t))

;;;###autoload
(defun message-news (&optional newsgroups subject)
  "Start editing a news article to be sent."
  (interactive)
  (let ((message-this-is-news t))
    (message-pop-to-buffer (message-buffer-name "posting" nil newsgroups))
    (message-setup `((Newsgroups . ,(or newsgroups ""))
		     (Subject . ,(or subject ""))))))

(defun message-get-reply-headers (wide &optional to-address address-headers)
  (let (follow-to mct never-mct to cc author mft recipients)
    ;; Find all relevant headers we need.
    (save-restriction
      (message-narrow-to-headers-or-head)
      ;; Gmane renames "To".  Look at "Original-To", too, if it is present in
      ;; message-header-synonyms.
      (setq to (or (message-fetch-field "to")
		   (and (loop for synonym in message-header-synonyms
			      when (memq 'Original-To synonym)
			      return t)
			(message-fetch-field "original-to")))
	    cc (message-fetch-field "cc")
	    mct (message-fetch-field "mail-copies-to")
	    author (or (message-fetch-field "mail-reply-to")
		       (message-fetch-field "reply-to")
		       (message-fetch-field "from")
		       "")
	    mft (and message-use-mail-followup-to
		     (message-fetch-field "mail-followup-to"))))

    ;; Handle special values of Mail-Copies-To.
    (when mct
      (cond ((or (equal (downcase mct) "never")
		 (equal (downcase mct) "nobody"))
	     (setq never-mct t)
	     (setq mct nil))
	    ((or (equal (downcase mct) "always")
		 (equal (downcase mct) "poster"))
	     (setq mct author))))

    (save-match-data
      ;; Build (textual) list of new recipient addresses.
      (cond
       ((not wide)
	(setq recipients (concat ", " author)))
       (address-headers
	(dolist (header address-headers)
	  (let ((value (message-fetch-field header)))
	    (when value
	      (setq recipients (concat recipients ", " value))))))
       ((and mft
	     (string-match "[^ \t,]" mft)
	     (or (not (eq message-use-mail-followup-to 'ask))
		 (message-y-or-n-p "Obey Mail-Followup-To? " t "\
You should normally obey the Mail-Followup-To: header.  In this
article, it has the value of

" mft "

which directs your response to " (if (string-match "," mft)
				     "the specified addresses"
				   "that address only") ".

Most commonly, Mail-Followup-To is used by a mailing list poster to
express that responses should be sent to just the list, and not the
poster as well.

If a message is posted to several mailing lists, Mail-Followup-To may
also be used to direct the following discussion to one list only,
because discussions that are spread over several lists tend to be
fragmented and very difficult to follow.

Also, some source/announcement lists are not intended for discussion;
responses here are directed to other addresses.

You may customize the variable `message-use-mail-followup-to', if you
want to get rid of this query permanently.")))
	(setq recipients (concat ", " mft)))
       (to-address
	(setq recipients (concat ", " to-address))
	;; If the author explicitly asked for a copy, we don't deny it to them.
	(if mct (setq recipients (concat recipients ", " mct))))
       (t
	(setq recipients (if never-mct "" (concat ", " author)))
	(if to  (setq recipients (concat recipients ", " to)))
	(if cc  (setq recipients (concat recipients ", " cc)))
	(if mct (setq recipients (concat recipients ", " mct)))))
      (if (>= (length recipients) 2)
	  ;; Strip the leading ", ".
	  (setq recipients (substring recipients 2)))
      ;; Squeeze whitespace.
      (while (string-match "[ \t][ \t]+" recipients)
	(setq recipients (replace-match " " t t recipients)))
      ;; Remove addresses that match `rmail-dont-reply-to-names'.
      (let ((rmail-dont-reply-to-names message-dont-reply-to-names))
	(setq recipients (rmail-dont-reply-to recipients)))
      ;; Perhaps "Mail-Copies-To: never" removed the only address?
      (if (string-equal recipients "")
	  (setq recipients author))
      ;; Convert string to a list of (("foo@bar" . "Name <Foo@BAR>") ...).
      (setq recipients
	    (mapcar
	     (lambda (addr)
	       (cons (downcase (mail-strip-quoted-names addr)) addr))
	     (message-tokenize-header recipients)))
      ;; Remove first duplicates.  (Why not all duplicates?  Is this a bug?)
      (let ((s recipients))
	(while s
	  (setq recipients (delq (assoc (car (pop s)) s) recipients))))

      ;; Remove hierarchical lists that are contained within each other,
      ;; if message-hierarchical-addresses is defined.
      (when message-hierarchical-addresses
	(let ((plain-addrs (mapcar 'car recipients))
	      subaddrs recip)
	  (while plain-addrs
	    (setq subaddrs (assoc (car plain-addrs)
				  message-hierarchical-addresses)
		  plain-addrs (cdr plain-addrs))
	    (when subaddrs
	      (setq subaddrs (cdr subaddrs))
	      (while subaddrs
		(setq recip (assoc (car subaddrs) recipients)
		      subaddrs (cdr subaddrs))
		(if recip
		    (setq recipients (delq recip recipients))))))))

      ;; Build the header alist.  Allow the user to be asked whether
      ;; or not to reply to all recipients in a wide reply.
      (setq follow-to (list (cons 'To (cdr (pop recipients)))))
      (when (and recipients
		 (or (not message-wide-reply-confirm-recipients)
		     (y-or-n-p "Reply to all recipients? ")))
	(setq recipients (mapconcat
			  (lambda (addr) (cdr addr)) recipients ", "))
	(if (string-match "^ +" recipients)
	    (setq recipients (substring recipients (match-end 0))))
	(push (cons 'Cc recipients) follow-to)))
    follow-to))

;;;###autoload
(defun message-reply (&optional to-address wide)
  "Start editing a reply to the article in the current buffer."
  (interactive)
  (require 'gnus-sum)			; for gnus-list-identifiers
  (let ((cur (current-buffer))
	from subject date reply-to to cc
	references message-id follow-to
	(inhibit-point-motion-hooks t)
	(message-this-is-mail t)
	gnus-warning)
    (save-restriction
      (message-narrow-to-head-1)
      ;; Allow customizations to have their say.
      (if (not wide)
	  ;; This is a regular reply.
	  (when (functionp message-reply-to-function)
	    (save-excursion
	      (setq follow-to (funcall message-reply-to-function))))
	;; This is a followup.
	(when (functionp message-wide-reply-to-function)
	  (save-excursion
	    (setq follow-to
		  (funcall message-wide-reply-to-function)))))
      (setq message-id (message-fetch-field "message-id" t)
	    references (message-fetch-field "references")
	    date (message-fetch-field "date")
	    from (message-fetch-field "from")
	    subject (or (message-fetch-field "subject") "none"))
      (when gnus-list-identifiers
	(setq subject (message-strip-list-identifiers subject)))
      (setq subject (concat "Re: " (message-strip-subject-re subject)))
      (when message-subject-trailing-was-query
	(setq subject (message-strip-subject-trailing-was subject)))

      (when (and (setq gnus-warning (message-fetch-field "gnus-warning"))
		 (string-match "<[^>]+>" gnus-warning))
	(setq message-id (match-string 0 gnus-warning)))

      (unless follow-to
	(setq follow-to (message-get-reply-headers wide to-address))))

    (unless (message-mail-user-agent)
      (message-pop-to-buffer
       (message-buffer-name
	(if wide "wide reply" "reply") from
	(if wide to-address nil))))

    (setq message-reply-headers
	  (vector 0 subject from date message-id references 0 0 ""))

    (message-setup
     `((Subject . ,subject)
       ,@follow-to)
     cur)))

;;;###autoload
(defun message-wide-reply (&optional to-address)
  "Make a \"wide\" reply to the message in the current buffer."
  (interactive)
  (message-reply to-address t))

;;;###autoload
(defun message-followup (&optional to-newsgroups)
  "Follow up to the message in the current buffer.
If TO-NEWSGROUPS, use that as the new Newsgroups line."
  (interactive)
  (require 'gnus-sum)			; for gnus-list-identifiers
  (let ((cur (current-buffer))
	from subject date reply-to mrt mct
	references message-id follow-to
	(inhibit-point-motion-hooks t)
	(message-this-is-news t)
	followup-to distribution newsgroups gnus-warning posted-to)
    (save-restriction
      (narrow-to-region
       (goto-char (point-min))
       (if (search-forward "\n\n" nil t)
	   (1- (point))
	 (point-max)))
      (when (functionp message-followup-to-function)
	(setq follow-to
	      (funcall message-followup-to-function)))
      (setq from (message-fetch-field "from")
	    date (message-fetch-field "date")
	    subject (or (message-fetch-field "subject") "none")
	    references (message-fetch-field "references")
	    message-id (message-fetch-field "message-id" t)
	    followup-to (message-fetch-field "followup-to")
	    newsgroups (message-fetch-field "newsgroups")
	    posted-to (message-fetch-field "posted-to")
	    reply-to (message-fetch-field "reply-to")
	    mrt (message-fetch-field "mail-reply-to")
	    distribution (message-fetch-field "distribution")
	    mct (message-fetch-field "mail-copies-to"))
      (when (and (setq gnus-warning (message-fetch-field "gnus-warning"))
		 (string-match "<[^>]+>" gnus-warning))
	(setq message-id (match-string 0 gnus-warning)))
      ;; Remove bogus distribution.
      (when (and (stringp distribution)
		 (let ((case-fold-search t))
		   (string-match "world" distribution)))
	(setq distribution nil))
      (if gnus-list-identifiers
	  (setq subject (message-strip-list-identifiers subject)))
      (setq subject (concat "Re: " (message-strip-subject-re subject)))
      (when message-subject-trailing-was-query
	(setq subject (message-strip-subject-trailing-was subject)))
      (widen))

    (message-pop-to-buffer (message-buffer-name "followup" from newsgroups))

    (setq message-reply-headers
	  (vector 0 subject from date message-id references 0 0 ""))

    (message-setup
     `((Subject . ,subject)
       ,@(cond
	  (to-newsgroups
	   (list (cons 'Newsgroups to-newsgroups)))
	  (follow-to follow-to)
	  ((and followup-to message-use-followup-to)
	   (list
	    (cond
	     ((equal (downcase followup-to) "poster")
	      (if (or (eq message-use-followup-to 'use)
		      (message-y-or-n-p "Obey Followup-To: poster? " t "\
You should normally obey the Followup-To: header.

`Followup-To: poster' sends your response via e-mail instead of news.

A typical situation where `Followup-To: poster' is used is when the poster
does not read the newsgroup, so he wouldn't see any replies sent to it.

You may customize the variable `message-use-followup-to', if you
want to get rid of this query permanently."))
		  (progn
		    (setq message-this-is-news nil)
		    (cons 'To (or mrt reply-to from "")))
		(cons 'Newsgroups newsgroups)))
	     (t
	      (if (or (equal followup-to newsgroups)
		      (not (eq message-use-followup-to 'ask))
		      (message-y-or-n-p
		       (concat "Obey Followup-To: " followup-to "? ") t "\
You should normally obey the Followup-To: header.

	`Followup-To: " followup-to "'
directs your response to " (if (string-match "," followup-to)
			       "the specified newsgroups"
			     "that newsgroup only") ".

If a message is posted to several newsgroups, Followup-To is often
used to direct the following discussion to one newsgroup only,
because discussions that are spread over several newsgroup tend to
be fragmented and very difficult to follow.

Also, some source/announcement newsgroups are not intended for discussion;
responses here are directed to other newsgroups.

You may customize the variable `message-use-followup-to', if you
want to get rid of this query permanently."))
		  (cons 'Newsgroups followup-to)
		(cons 'Newsgroups newsgroups))))))
	  (posted-to
	   `((Newsgroups . ,posted-to)))
	  (t
	   `((Newsgroups . ,newsgroups))))
       ,@(and distribution (list (cons 'Distribution distribution)))
       ,@(when (and mct
		    (not (or (equal (downcase mct) "never")
			     (equal (downcase mct) "nobody"))))
	   (list (cons 'Cc (if (or (equal (downcase mct) "always")
				   (equal (downcase mct) "poster"))
			       (or mrt reply-to from "")
			     mct)))))

     cur)))

(defun message-is-yours-p ()
  "Non-nil means current article is yours.
If you have added 'cancel-messages to `message-shoot-gnksa-feet', all articles
are yours except those that have Cancel-Lock header not belonging to you.
Instead of shooting GNKSA feet, you should modify `message-alternative-emails'
regexp to match all of yours addresses."
  ;; Canlock-logic as suggested by Per Abrahamsen
  ;; <abraham@dina.kvl.dk>
  ;;
  ;; IF article has cancel-lock THEN
  ;;   IF we can verify it THEN
  ;;     issue cancel
  ;;   ELSE
  ;;     error: cancellock: article is not yours
  ;; ELSE
  ;;   Use old rules, comparing sender...
  (save-excursion
    (save-restriction
      (message-narrow-to-head-1)
      (if (message-fetch-field "Cancel-Lock")
	  (if (null (canlock-verify))
	      t
	    (error "Failed to verify Cancel-lock: This article is not yours"))
	(let (sender from)
	  (or
	   (message-gnksa-enable-p 'cancel-messages)
	   (and (setq sender (message-fetch-field "sender"))
		(string-equal (downcase sender)
			      (downcase (message-make-sender))))
	   ;; Email address in From field equals to our address
	   (and (setq from (message-fetch-field "from"))
		(string-equal
		 (downcase (cadr (mail-extract-address-components from)))
		 (downcase (cadr (mail-extract-address-components
				  (message-make-from))))))
	   ;; Email address in From field matches
	   ;; 'message-alternative-emails' regexp
	   (and from
		message-alternative-emails
		(string-match
		 message-alternative-emails
		 (cadr (mail-extract-address-components from))))))))))

;;;###autoload
(defun message-cancel-news (&optional arg)
  "Cancel an article you posted.
If ARG, allow editing of the cancellation message."
  (interactive "P")
  (unless (message-news-p)
    (error "This is not a news article; canceling is impossible"))
  (let (from newsgroups message-id distribution buf)
    (save-excursion
      ;; Get header info from original article.
      (save-restriction
	(message-narrow-to-head-1)
	(setq from (message-fetch-field "from")
	      newsgroups (message-fetch-field "newsgroups")
	      message-id (message-fetch-field "message-id" t)
	      distribution (message-fetch-field "distribution")))
      ;; Make sure that this article was written by the user.
      (unless (message-is-yours-p)
	(error "This article is not yours"))
      (when (yes-or-no-p "Do you really want to cancel this article? ")
	;; Make control message.
	(if arg
	    (message-news)
	  (setq buf (set-buffer (get-buffer-create " *message cancel*"))))
	(erase-buffer)
	(insert "Newsgroups: " newsgroups "\n"
		"From: " from "\n"
		"Subject: cmsg cancel " message-id "\n"
		"Control: cancel " message-id "\n"
		(if distribution
		    (concat "Distribution: " distribution "\n")
		  "")
		mail-header-separator "\n"
		message-cancel-message)
	(run-hooks 'message-cancel-hook)
	(unless arg
	  (message "Canceling your article...")
	  (if (let ((message-syntax-checks
		     'dont-check-for-anything-just-trust-me))
		(funcall message-send-news-function))
	      (message "Canceling your article...done"))
	  (kill-buffer buf))))))

;;;###autoload
(defun message-supersede ()
  "Start composing a message to supersede the current message.
This is done simply by taking the old article and adding a Supersedes
header line with the old Message-ID."
  (interactive)
  (let ((cur (current-buffer)))
    ;; Check whether the user owns the article that is to be superseded.
    (unless (message-is-yours-p)
      (error "This article is not yours"))
    ;; Get a normal message buffer.
    (message-pop-to-buffer (message-buffer-name "supersede"))
    (insert-buffer-substring cur)
    (mime-to-mml)
    (message-narrow-to-head-1)
    ;; Remove unwanted headers.
    (when message-ignored-supersedes-headers
      (message-remove-header message-ignored-supersedes-headers t))
    (goto-char (point-min))
    (if (not (re-search-forward "^Message-ID: " nil t))
	(error "No Message-ID in this article")
      (replace-match "Supersedes: " t t))
    (goto-char (point-max))
    (insert mail-header-separator)
    (widen)
    (forward-line 1)))

;;;###autoload
(defun message-recover ()
  "Reread contents of current buffer from its last auto-save file."
  (interactive)
  (let ((file-name (make-auto-save-file-name)))
    (cond ((save-window-excursion
	     (if (not (eq system-type 'vax-vms))
		 (with-output-to-temp-buffer "*Directory*"
		   (with-current-buffer standard-output
		     (fundamental-mode)) ; for Emacs 20.4+
		   (buffer-disable-undo standard-output)
		   (let ((default-directory "/"))
		     (call-process
		      "ls" nil standard-output nil "-l" file-name))))
	     (yes-or-no-p (format "Recover auto save file %s? " file-name)))
	   (let ((buffer-read-only nil))
	     (erase-buffer)
	     (insert-file-contents file-name nil)))
	  (t (error "message-recover cancelled")))))

;;; Washing Subject:

(defun message-wash-subject (subject)
  "Remove junk like \"Re:\", \"(fwd)\", etc. added to subject string SUBJECT.
Previous forwarders, replyers, etc. may add it."
  (with-temp-buffer
    (insert subject)
    (goto-char (point-min))
    ;; strip Re/Fwd stuff off the beginning
    (while (re-search-forward
	    "\\([Rr][Ee]:\\|[Ff][Ww][Dd]\\(\\[[0-9]*\\]\\)?:\\|[Ff][Ww]:\\)" nil t)
      (replace-match ""))

    ;; and gnus-style forwards [foo@bar.com] subject
    (goto-char (point-min))
    (while (re-search-forward "\\[[^ \t]*\\(@\\|\\.\\)[^ \t]*\\]" nil t)
      (replace-match ""))

    ;; and off the end
    (goto-char (point-max))
    (while (re-search-backward "([Ff][Ww][Dd])" nil t)
      (replace-match ""))

    ;; and finally, any whitespace that was left-over
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]+" nil t)
      (replace-match ""))
    (goto-char (point-max))
    (while (re-search-backward "[ \t]+$" nil t)
      (replace-match ""))

    (buffer-string)))

;;; Forwarding messages.

(defvar message-forward-decoded-p nil
  "Non-nil means the original message is decoded.")

(defun message-forward-subject-name-subject (subject)
  "Generate a SUBJECT for a forwarded message.
The form is: [Source] Subject, where if the original message was mail,
Source is the name of the sender, and if the original message was
news, Source is the list of newsgroups is was posted to."
  (let* ((group (message-fetch-field "newsgroups"))
	 (from (message-fetch-field "from"))
	 (prefix
	  (if group
	      (gnus-group-decoded-name group)
	    (or (and from (car (gnus-extract-address-components from)))
		"(nowhere)"))))
    (concat "["
	    (if message-forward-decoded-p
		prefix
	      (mail-decode-encoded-word-string prefix))
	    "] " subject)))

(defun message-forward-subject-author-subject (subject)
  "Generate a SUBJECT for a forwarded message.
The form is: [Source] Subject, where if the original message was mail,
Source is the sender, and if the original message was news, Source is
the list of newsgroups is was posted to."
  (let* ((group (message-fetch-field "newsgroups"))
	 (prefix
	  (if group
	      (gnus-group-decoded-name group)
	    (or (message-fetch-field "from")
		"(nowhere)"))))
    (concat "["
	    (if message-forward-decoded-p
		prefix
	      (mail-decode-encoded-word-string prefix))
	    "] " subject)))

(defun message-forward-subject-fwd (subject)
  "Generate a SUBJECT for a forwarded message.
The form is: Fwd: Subject, where Subject is the original subject of
the message."
  (if (string-match "^Fwd: " subject)
      subject
    (concat "Fwd: " subject)))

(defun message-make-forward-subject ()
  "Return a Subject header suitable for the message in the current buffer."
  (save-excursion
    (save-restriction
      (message-narrow-to-head-1)
      (let ((funcs message-make-forward-subject-function)
	    (subject (message-fetch-field "Subject")))
	(setq subject
	      (if subject
		  (if message-forward-decoded-p
		      subject
		    (mail-decode-encoded-word-string subject))
		""))
	(if message-wash-forwarded-subjects
	    (setq subject (message-wash-subject subject)))
	;; Make sure funcs is a list.
	(and funcs
	     (not (listp funcs))
	     (setq funcs (list funcs)))
	;; Apply funcs in order, passing subject generated by previous
	;; func to the next one.
	(while funcs
	  (when (functionp (car funcs))
	    (setq subject (funcall (car funcs) subject)))
	  (setq funcs (cdr funcs)))
	subject))))

(eval-when-compile
  (defvar gnus-article-decoded-p))


;;;###autoload
(defun message-forward (&optional news digest)
  "Forward the current message via mail.
Optional NEWS will use news to forward instead of mail.
Optional DIGEST will use digest to forward."
  (interactive "P")
  (let* ((cur (current-buffer))
	 (message-forward-decoded-p
	  (if (local-variable-p 'gnus-article-decoded-p (current-buffer))
	      gnus-article-decoded-p ;; In an article buffer.
	    message-forward-decoded-p))
	 (subject (message-make-forward-subject)))
    (if news
	(message-news nil subject)
      (message-mail nil subject))
    (message-forward-make-body cur digest)))

(defun message-forward-make-body-plain (forward-buffer)
  (insert
   "\n-------------------- Start of forwarded message --------------------\n")
  (let ((b (point)) e)
    (insert
     (with-temp-buffer
       (mm-disable-multibyte)
       (insert
	(with-current-buffer forward-buffer
	  (mm-with-unibyte-current-buffer (buffer-string))))
       (mm-enable-multibyte)
       (mime-to-mml)
       (goto-char (point-min))
       (when (looking-at "From ")
	 (replace-match "X-From-Line: "))
       (buffer-string)))
    (setq e (point))
    (insert
     "\n-------------------- End of forwarded message --------------------\n")
    (when message-forward-ignored-headers
      (save-restriction
	(narrow-to-region b e)
	(goto-char b)
	(narrow-to-region (point)
			  (or (search-forward "\n\n" nil t) (point)))
	(message-remove-header message-forward-ignored-headers t)))))

(defun message-forward-make-body-mime (forward-buffer)
  (insert "\n\n<#part type=message/rfc822 disposition=inline raw=t>\n")
  (let ((b (point)) e)
    (save-restriction
      (narrow-to-region (point) (point))
      (mml-insert-buffer forward-buffer)
      (goto-char (point-min))
      (when (looking-at "From ")
	(replace-match "X-From-Line: "))
      (goto-char (point-max)))
    (setq e (point))
    (insert "<#/part>\n")))

(defun message-forward-make-body-mml (forward-buffer)
  (insert "\n\n<#mml type=message/rfc822 disposition=inline>\n")
  (let ((b (point)) e)
    (if (not message-forward-decoded-p)
	(insert
	 (with-temp-buffer
	   (mm-disable-multibyte)
	   (insert
	    (with-current-buffer forward-buffer
	      (mm-with-unibyte-current-buffer (buffer-string))))
	   (mm-enable-multibyte)
	   (mime-to-mml)
	   (goto-char (point-min))
	   (when (looking-at "From ")
	     (replace-match "X-From-Line: "))
	   (buffer-string)))
      (save-restriction
	(narrow-to-region (point) (point))
	(mml-insert-buffer forward-buffer)
	(goto-char (point-min))
	(when (looking-at "From ")
	  (replace-match "X-From-Line: "))
	(goto-char (point-max))))
    (setq e (point))
    (insert "<#/mml>\n")
    (when (and (not message-forward-decoded-p)
	       message-forward-ignored-headers)
      (save-restriction
	(narrow-to-region b e)
	(goto-char b)
	(narrow-to-region (point)
			  (or (search-forward "\n\n" nil t) (point)))
	(message-remove-header message-forward-ignored-headers t)))))

(defun message-forward-make-body-digest-plain (forward-buffer)
  (insert
   "\n-------------------- Start of forwarded message --------------------\n")
  (let ((b (point)) e)
    (mml-insert-buffer forward-buffer)
    (setq e (point))
    (insert
     "\n-------------------- End of forwarded message --------------------\n")))

(defun message-forward-make-body-digest-mime (forward-buffer)
  (insert "\n<#multipart type=digest>\n")
  (let ((b (point)) e)
    (insert-buffer-substring forward-buffer)
    (setq e (point))
    (insert "<#/multipart>\n")
    (save-restriction
      (narrow-to-region b e)
      (goto-char b)
      (narrow-to-region (point)
			(or (search-forward "\n\n" nil t) (point)))
      (delete-region (point-min) (point-max)))))

(defun message-forward-make-body-digest (forward-buffer)
  (if message-forward-as-mime
      (message-forward-make-body-digest-mime forward-buffer)
    (message-forward-make-body-digest-plain forward-buffer)))

;;;###autoload
(defun message-forward-make-body (forward-buffer &optional digest)
  ;; Put point where we want it before inserting the forwarded
  ;; message.
  (if message-forward-before-signature
      (message-goto-body)
    (goto-char (point-max)))
  (if digest
      (message-forward-make-body-digest forward-buffer)
    (if message-forward-as-mime
	(if (and message-forward-show-mml
		 (not (and (eq message-forward-show-mml 'best)
			   (with-current-buffer forward-buffer
			     (goto-char (point-min))
			     (re-search-forward
			      "Content-Type: *multipart/\\(signed\\|encrypted\\)"
			      nil t)))))
	    (message-forward-make-body-mml forward-buffer)
	  (message-forward-make-body-mime forward-buffer))
      (message-forward-make-body-plain forward-buffer)))
  (message-position-point))

;;;###autoload
(defun message-forward-rmail-make-body (forward-buffer)
  (save-window-excursion
    (set-buffer forward-buffer)
    ;; Rmail doesn't have rmail-msg-restore-non-pruned-header in Emacs
    ;; 20.  FIXIT, or we drop support for rmail in Emacs 20.
    (if (rmail-msg-is-pruned)
	(rmail-msg-restore-non-pruned-header)))
  (message-forward-make-body forward-buffer))

(eval-when-compile (defvar rmail-enable-mime-composing))

;; Fixme: Should have defcustom.
;;;###autoload
(defun message-insinuate-rmail ()
  "Let RMAIL use message to forward."
  (interactive)
  (setq rmail-enable-mime-composing t)
  (setq rmail-insert-mime-forwarded-message-function
	'message-forward-rmail-make-body))

;;;###autoload
(defun message-resend (address)
  "Resend the current article to ADDRESS."
  (interactive
   (list (message-read-from-minibuffer "Resend message to: ")))
  (message "Resending message to %s..." address)
  (save-excursion
    (let ((cur (current-buffer))
	  beg)
      ;; We first set up a normal mail buffer.
      (unless (message-mail-user-agent)
	(set-buffer (get-buffer-create " *message resend*"))
	(erase-buffer))
      (let ((message-this-is-mail t)
	    message-setup-hook)
	(message-setup `((To . ,address))))
      ;; Insert our usual headers.
      (message-generate-headers '(From Date To Message-ID))
      (message-narrow-to-headers)
      ;; Remove X-Draft-From header etc.
      (message-remove-header message-ignored-mail-headers t)
      ;; Rename them all to "Resent-*".
      (goto-char (point-min))
      (while (re-search-forward "^[A-Za-z]" nil t)
	(forward-char -1)
	(insert "Resent-"))
      (widen)
      (forward-line)
      (delete-region (point) (point-max))
      (setq beg (point))
      ;; Insert the message to be resent.
      (insert-buffer-substring cur)
      (goto-char (point-min))
      (search-forward "\n\n")
      (forward-char -1)
      (save-restriction
	(narrow-to-region beg (point))
	(message-remove-header message-ignored-resent-headers t)
	(goto-char (point-max)))
      (insert mail-header-separator)
      ;; Rename all old ("Also-")Resent headers.
      (while (re-search-backward "^\\(Also-\\)*Resent-" beg t)
	(beginning-of-line)
	(insert "Also-"))
      ;; Quote any "From " lines at the beginning.
      (goto-char beg)
      (when (looking-at "From ")
	(replace-match "X-From-Line: "))
      ;; Send it.
      (let ((message-inhibit-body-encoding t)
	    message-required-mail-headers
	    rfc2047-encode-encoded-words)
	(message-send-mail))
      (kill-buffer (current-buffer)))
    (message "Resending message to %s...done" address)))

;;;###autoload
(defun message-bounce ()
  "Re-mail the current message.
This only makes sense if the current message is a bounce message that
contains some mail you have written which has been bounced back to
you."
  (interactive)
  (let ((handles (mm-dissect-buffer t))
	boundary)
    (message-pop-to-buffer (message-buffer-name "bounce"))
    (if (stringp (car handles))
	;; This is a MIME bounce.
	(mm-insert-part (car (last handles)))
      ;; This is a non-MIME bounce, so we try to remove things
      ;; manually.
      (mm-insert-part handles)
      (undo-boundary)
      (goto-char (point-min))
      (re-search-forward "\n\n+" nil t)
      (setq boundary (point))
      ;; We remove everything before the bounced mail.
      (if (or (re-search-forward message-unsent-separator nil t)
	      (progn
		(search-forward "\n\n" nil 'move)
		(re-search-backward "^Return-Path:.*\n" boundary t)))
	  (progn
	    (forward-line 1)
	    (delete-region (point-min)
			   (if (re-search-forward "^[^ \n\t]+:" nil t)
			       (match-beginning 0)
			     (point))))
	(goto-char boundary)
	(when (re-search-backward "^.?From .*\n" nil t)
	  (delete-region (match-beginning 0) (match-end 0)))))
    (mm-enable-multibyte)
    (save-restriction
      (message-narrow-to-head-1)
      (message-remove-header message-ignored-bounced-headers t)
      (goto-char (point-max))
      (insert mail-header-separator))
    (message-position-point)))

;;;
;;; Interactive entry points for new message buffers.
;;;

;;;###autoload
(defun message-mail-other-window (&optional to subject)
  "Like `message-mail' command, but display mail buffer in another window."
  (interactive)
  (unless (message-mail-user-agent)
    (let ((pop-up-windows t)
	  (special-display-buffer-names nil)
	  (special-display-regexps nil)
	  (same-window-buffer-names nil)
	  (same-window-regexps nil))
      (message-pop-to-buffer (message-buffer-name "mail" to))))
  (let ((message-this-is-mail t))
    (message-setup `((To . ,(or to "")) (Subject . ,(or subject "")))
		   nil nil 'switch-to-buffer-other-window)))

;;;###autoload
(defun message-mail-other-frame (&optional to subject)
  "Like `message-mail' command, but display mail buffer in another frame."
  (interactive)
  (unless (message-mail-user-agent)
    (let ((pop-up-frames t)
	  (special-display-buffer-names nil)
	  (special-display-regexps nil)
	  (same-window-buffer-names nil)
	  (same-window-regexps nil))
      (message-pop-to-buffer (message-buffer-name "mail" to))))
  (let ((message-this-is-mail t))
    (message-setup `((To . ,(or to "")) (Subject . ,(or subject "")))
		   nil nil 'switch-to-buffer-other-frame)))

;;;###autoload
(defun message-news-other-window (&optional newsgroups subject)
  "Start editing a news article to be sent."
  (interactive)
  (let ((pop-up-windows t)
	(special-display-buffer-names nil)
	(special-display-regexps nil)
	(same-window-buffer-names nil)
	(same-window-regexps nil))
    (message-pop-to-buffer (message-buffer-name "posting" nil newsgroups)))
  (let ((message-this-is-news t))
    (message-setup `((Newsgroups . ,(or newsgroups ""))
		     (Subject . ,(or subject ""))))))

;;;###autoload
(defun message-news-other-frame (&optional newsgroups subject)
  "Start editing a news article to be sent."
  (interactive)
  (let ((pop-up-frames t)
	(special-display-buffer-names nil)
	(special-display-regexps nil)
	(same-window-buffer-names nil)
	(same-window-regexps nil))
    (message-pop-to-buffer (message-buffer-name "posting" nil newsgroups)))
  (let ((message-this-is-news t))
    (message-setup `((Newsgroups . ,(or newsgroups ""))
		     (Subject . ,(or subject ""))))))

;;; underline.el

;; This code should be moved to underline.el (from which it is stolen).

;;;###autoload
(defun bold-region (start end)
  "Bold all nonblank characters in the region.
Works by overstriking characters.
Called from program, takes two arguments START and END
which specify the range to operate on."
  (interactive "r")
  (save-excursion
    (let ((end1 (make-marker)))
      (move-marker end1 (max start end))
      (goto-char (min start end))
      (while (< (point) end1)
	(or (looking-at "[_\^@- ]")
	    (insert (char-after) "\b"))
	(forward-char 1)))))

;;;###autoload
(defun unbold-region (start end)
  "Remove all boldness (overstruck characters) in the region.
Called from program, takes two arguments START and END
which specify the range to operate on."
  (interactive "r")
  (save-excursion
    (let ((end1 (make-marker)))
      (move-marker end1 (max start end))
      (goto-char (min start end))
      (while (re-search-forward "\b" end1 t)
	(if (eq (char-after) (char-after (- (point) 2)))
	    (delete-char -2))))))

(defun message-exchange-point-and-mark ()
  "Exchange point and mark, but don't activate region if it was inactive."
  (unless (prog1
	      (message-mark-active-p)
	    (exchange-point-and-mark))
    (setq mark-active nil)))

(defalias 'message-make-overlay 'make-overlay)
(defalias 'message-delete-overlay 'delete-overlay)
(defalias 'message-overlay-put 'overlay-put)
(defun message-kill-all-overlays ()
  (if (featurep 'xemacs)
      (map-extents (lambda (extent ignore) (delete-extent extent)))
    (mapcar #'delete-overlay (overlays-in (point-min) (point-max)))))

;; Support for toolbar
(eval-when-compile
  (defvar tool-bar-map)
  (defvar tool-bar-mode))

(defun message-tool-bar-local-item-from-menu (command icon in-map &optional from-map &rest props)
  ;; We need to make tool bar entries in local keymaps with
  ;; `tool-bar-local-item-from-menu' in Emacs > 21.3
  (if (fboundp 'tool-bar-local-item-from-menu)
      ;; This is for Emacs 21.3
      (tool-bar-local-item-from-menu command icon in-map from-map props)
    (tool-bar-add-item-from-menu command icon from-map props)))

(defun message-tool-bar-map ()
  (or message-tool-bar-map
      (setq message-tool-bar-map
	    (and
	     (condition-case nil (require 'tool-bar) (error nil))
	     (fboundp 'tool-bar-add-item-from-menu)
	     tool-bar-mode
	     (let ((tool-bar-map (copy-keymap tool-bar-map))
		   (load-path (mm-image-load-path)))
	       ;; Zap some items which aren't so relevant and take
	       ;; up space.
	       (dolist (key '(print-buffer kill-buffer save-buffer
					   write-file dired open-file))
		 (define-key tool-bar-map (vector key) nil))
	       (message-tool-bar-local-item-from-menu
		'message-send-and-exit "mail_send" tool-bar-map message-mode-map)
	       (message-tool-bar-local-item-from-menu
		'message-kill-buffer "close" tool-bar-map message-mode-map)
	       (message-tool-bar-local-item-from-menu
		    'message-dont-send "cancel" tool-bar-map message-mode-map)
	       (message-tool-bar-local-item-from-menu
		'mml-attach-file "attach" tool-bar-map mml-mode-map)
	       (message-tool-bar-local-item-from-menu
		'ispell-message "spell" tool-bar-map message-mode-map)
	       (message-tool-bar-local-item-from-menu
		'mml-preview "preview"
		tool-bar-map mml-mode-map)
	       (message-tool-bar-local-item-from-menu
		'message-insert-importance-high "important"
		tool-bar-map message-mode-map)
	       (message-tool-bar-local-item-from-menu
		'message-insert-importance-low "unimportant"
		tool-bar-map message-mode-map)
	       (message-tool-bar-local-item-from-menu
		'message-insert-disposition-notification-to "receipt"
		tool-bar-map message-mode-map)
	       tool-bar-map)))))

;;; Group name completion.

(defcustom message-newgroups-header-regexp
  "^\\(Newsgroups\\|Followup-To\\|Posted-To\\|Gcc\\):"
  "Regexp that match headers that lists groups."
  :group 'message
  :type 'regexp)

(defcustom message-completion-alist
  (list (cons message-newgroups-header-regexp 'message-expand-group)
	'("^\\(Resent-\\)?\\(To\\|B?Cc\\):" . message-expand-name)
	'("^\\(Reply-To\\|From\\|Mail-Followup-To\\|Mail-Copies-To\\):"
	  . message-expand-name)
	'("^\\(Disposition-Notification-To\\|Return-Receipt-To\\):"
	  . message-expand-name))
  "Alist of (RE . FUN).  Use FUN for completion on header lines matching RE."
  :version "22.1"
  :group 'message
  :type '(alist :key-type regexp :value-type function))

(defcustom message-tab-body-function nil
  "*Function to execute when `message-tab' (TAB) is executed in the body.
If nil, the function bound in `text-mode-map' or `global-map' is executed."
  :version "22.1"
  :group 'message
  :link '(custom-manual "(message)Various Commands")
  :type '(choice (const nil)
		 function))

(defun message-tab ()
  "Complete names according to `message-completion-alist'.
Execute function specified by `message-tab-body-function' when not in
those headers."
  (interactive)
  (let ((alist message-completion-alist))
    (while (and alist
		(let ((mail-abbrev-mode-regexp (caar alist)))
		  (not (mail-abbrev-in-expansion-header-p))))
      (setq alist (cdr alist)))
    (funcall (or (cdar alist) message-tab-body-function
		 (lookup-key text-mode-map "\t")
		 (lookup-key global-map "\t")
		 'indent-relative))))

(defun message-expand-group ()
  "Expand the group name under point."
  (let* ((b (save-excursion
	      (save-restriction
		(narrow-to-region
		 (save-excursion
		   (beginning-of-line)
		   (skip-chars-forward "^:")
		   (1+ (point)))
		 (point))
		(skip-chars-backward "^, \t\n") (point))))
	 (completion-ignore-case t)
	 (string (buffer-substring b (progn (skip-chars-forward "^,\t\n ")
					    (point))))
	 (hashtb (and (boundp 'gnus-active-hashtb) gnus-active-hashtb))
	 (completions (all-completions string hashtb))
	 comp)
    (delete-region b (point))
    (cond
     ((= (length completions) 1)
      (if (string= (car completions) string)
	  (progn
	    (insert string)
	    (message "Only matching group"))
	(insert (car completions))))
     ((and (setq comp (try-completion string hashtb))
	   (not (string= comp string)))
      (insert comp))
     (t
      (insert string)
      (if (not comp)
	  (message "No matching groups")
	(save-selected-window
	  (pop-to-buffer "*Completions*")
	  (buffer-disable-undo)
	  (let ((buffer-read-only nil))
	    (erase-buffer)
	    (let ((standard-output (current-buffer)))
	      (display-completion-list (sort completions 'string<)))
	    (goto-char (point-min))
	    (delete-region (point) (progn (forward-line 3) (point))))))))))

(defun message-expand-name ()
  (if (fboundp 'bbdb-complete-name)
      (bbdb-complete-name)
    (expand-abbrev)))

;;; Help stuff.

(defun message-talkative-question (ask question show &rest text)
  "Call FUNCTION with argument QUESTION; optionally display TEXT... args.
If SHOW is non-nil, the arguments TEXT... are displayed in a temp buffer.
The following arguments may contain lists of values."
  (if (and show
	   (setq text (message-flatten-list text)))
      (save-window-excursion
	(save-excursion
	  (with-output-to-temp-buffer " *MESSAGE information message*"
	    (set-buffer " *MESSAGE information message*")
	    (fundamental-mode)		; for Emacs 20.4+
	    (mapcar 'princ text)
	    (goto-char (point-min))))
	(funcall ask question))
    (funcall ask question)))

(defun message-flatten-list (list)
  "Return a new, flat list that contains all elements of LIST.

\(message-flatten-list '(1 (2 3 (4 5 (6))) 7))
=> (1 2 3 4 5 6 7)"
  (cond ((consp list)
	 (apply 'append (mapcar 'message-flatten-list list)))
	(list
	 (list list))))

(defun message-generate-new-buffer-clone-locals (name &optional varstr)
  "Create and return a buffer with name based on NAME using `generate-new-buffer'.
Then clone the local variables and values from the old buffer to the
new one, cloning only the locals having a substring matching the
regexp VARSTR."
  (let ((oldbuf (current-buffer)))
    (save-excursion
      (set-buffer (generate-new-buffer name))
      (message-clone-locals oldbuf varstr)
      (current-buffer))))

(defun message-clone-locals (buffer &optional varstr)
  "Clone the local variables from BUFFER to the current buffer."
  (let ((locals (save-excursion
		  (set-buffer buffer)
		  (buffer-local-variables)))
	(regexp "^gnus\\|^nn\\|^message\\|^sendmail\\|^smtp\\|^user-mail-address"))
    (mapcar
     (lambda (local)
       (when (and (consp local)
		  (car local)
		  (string-match regexp (symbol-name (car local)))
		  (or (null varstr)
		      (string-match varstr (symbol-name (car local)))))
	 (ignore-errors
	   (set (make-local-variable (car local))
		(cdr local)))))
     locals)))

;;;
;;; MIME functions
;;;

(defvar message-inhibit-body-encoding nil)

(defun message-encode-message-body ()
  (unless message-inhibit-body-encoding
    (let ((mail-parse-charset (or mail-parse-charset
				  message-default-charset))
	  (case-fold-search t)
	  lines content-type-p)
      (message-goto-body)
      (save-restriction
	(narrow-to-region (point) (point-max))
	(let ((new (mml-generate-mime)))
	  (when new
	    (delete-region (point-min) (point-max))
	    (insert new)
	    (goto-char (point-min))
	    (if (eq (aref new 0) ?\n)
		(delete-char 1)
	      (search-forward "\n\n")
	      (setq lines (buffer-substring (point-min) (1- (point))))
	      (delete-region (point-min) (point))))))
      (save-restriction
	(message-narrow-to-headers-or-head)
	(message-remove-header "Mime-Version")
	(goto-char (point-max))
	(insert "MIME-Version: 1.0\n")
	(when lines
	  (insert lines))
	(setq content-type-p
	      (or mml-boundary
		  (re-search-backward "^Content-Type:" nil t))))
      (save-restriction
	(message-narrow-to-headers-or-head)
	(message-remove-first-header "Content-Type")
	(message-remove-first-header "Content-Transfer-Encoding"))
      ;; We always make sure that the message has a Content-Type
      ;; header.  This is because some broken MTAs and MUAs get
      ;; awfully confused when confronted with a message with a
      ;; MIME-Version header and without a Content-Type header.  For
      ;; instance, Solaris' /usr/bin/mail.
      (unless content-type-p
	(goto-char (point-min))
	;; For unknown reason, MIME-Version doesn't exist.
	(when (re-search-forward "^MIME-Version:" nil t)
	  (forward-line 1)
	  (insert "Content-Type: text/plain; charset=us-ascii\n"))))))

(defun message-read-from-minibuffer (prompt &optional initial-contents)
  "Read from the minibuffer while providing abbrev expansion."
  (if (fboundp 'mail-abbrevs-setup)
      (let ((mail-abbrev-mode-regexp "")
	    (minibuffer-setup-hook 'mail-abbrevs-setup)
	    (minibuffer-local-map message-minibuffer-local-map))
	(read-from-minibuffer prompt initial-contents))
    (let ((minibuffer-setup-hook 'mail-abbrev-minibuffer-setup-hook)
	  (minibuffer-local-map message-minibuffer-local-map))
      (read-string prompt initial-contents))))

(defun message-use-alternative-email-as-from ()
  (require 'mail-utils)
  (let* ((fields '("To" "Cc"))
	 (emails
	  (split-string
	   (mail-strip-quoted-names
	    (mapconcat 'message-fetch-reply-field fields ","))
	   "[ \f\t\n\r\v,]+"))
	 email)
    (while emails
      (if (string-match message-alternative-emails (car emails))
	  (setq email (car emails)
		emails nil))
      (pop emails))
    (unless (or (not email) (equal email user-mail-address))
      (goto-char (point-max))
      (insert "From: " email "\n"))))

(defun message-options-get (symbol)
  (cdr (assq symbol message-options)))

(defun message-options-set (symbol value)
  (let ((the-cons (assq symbol message-options)))
    (if the-cons
	(if value
	    (setcdr the-cons value)
	  (setq message-options (delq the-cons message-options)))
      (and value
	   (push (cons symbol value) message-options))))
  value)

(defun message-options-set-recipient ()
  (save-restriction
    (message-narrow-to-headers-or-head)
    (message-options-set 'message-sender
			 (mail-strip-quoted-names
			  (message-fetch-field "from")))
    (message-options-set 'message-recipients
			 (mail-strip-quoted-names
			  (let ((to (message-fetch-field "to"))
				(cc (message-fetch-field "cc"))
				(bcc (message-fetch-field "bcc")))
			    (concat
			     (or to "")
			     (if (and to cc) ", ")
			     (or cc "")
			     (if (and (or to cc) bcc) ", ")
			     (or bcc "")))))))

(defun message-hide-headers ()
  "Hide headers based on the `message-hidden-headers' variable."
  (let ((regexps (if (stringp message-hidden-headers)
		     (list message-hidden-headers)
		   message-hidden-headers))
	(inhibit-point-motion-hooks t)
	(after-change-functions nil))
    (when regexps
      (save-excursion
	(save-restriction
	  (message-narrow-to-headers)
	  (goto-char (point-min))
	  (while (not (eobp))
	    (if (not (message-hide-header-p regexps))
		(message-next-header)
	      (let ((begin (point)))
		(message-next-header)
		(add-text-properties
		 begin (point)
		 '(invisible t message-hidden t))))))))))

(defun message-hide-header-p (regexps)
  (let ((result nil)
	(reverse nil))
    (when (eq (car regexps) 'not)
      (setq reverse t)
      (pop regexps))
    (dolist (regexp regexps)
      (setq result (or result (looking-at regexp))))
    (if reverse
	(not result)
      result)))

(when (featurep 'xemacs)
  (require 'messagexmas)
  (message-xmas-redefine))

(provide 'message)

(run-hooks 'message-load-hook)

;; Local Variables:
;; coding: iso-8859-1
;; End:

;; arch-tag: 94b32cac-4504-4b6c-8181-030ebf380ee0
;;; message.el ends here
