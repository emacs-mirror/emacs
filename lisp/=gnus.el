;;; gnus.el --- NNTP-based News Reader for GNU Emacs
;; Copyright (C) 1987,88,89,90,93,94,95 Free Software Foundation, Inc.

;; Author: Masanobu UMEDA <umerin@mse.kyutech.ac.jp>
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

;; How to Install GNUS:
;; (0) First of all, remove GNUS related OLD *.elc files (at least
;;     nntp.elc).
;; (1) Unshar gnus.el, gnuspost.el, gnusmail.el, gnusmisc.el, and
;;     nntp.el.
;; (2) byte-compile-file nntp.el, gnus.el, gnuspost.el, gnusmail.el,
;;     and gnusmisc.el.  If you have a local news spool,
;;     byte-compile-file nnspool.el, too.
;; (3) Define three environment variables in .login file as follows:
;;
;;     setenv	NNTPSERVER	flab
;;     setenv	DOMAINNAME	"stars.flab.Fujitsu.CO.JP"
;;     setenv	ORGANIZATION	"Fujitsu Laboratories Ltd., Kawasaki, Japan."
;;
;;     Or instead, define lisp variables in your .emacs, site-init.el,
;;     or default.el as follows:
;;
;;     (setq gnus-nntp-server "flab")
;;     (setq gnus-local-domain "stars.flab.Fujitsu.CO.JP")
;;     (setq gnus-local-organization "Fujitsu Laboratories Ltd., ...")
;;
;;     If the function (system-name) returns the full internet name,
;;     you don't have to define the domain.
;;
;; (4) You may have to define NNTP service name as number 119.
;;
;;     (setq gnus-nntp-service 119)
;;
;;     Or, if you'd like to use a local news spool directly in stead
;;     of NNTP, set the variable to nil as follows:
;;
;;     (setq gnus-nntp-service nil)
;;
;; (5) If you'd like to use the GENERICFROM feature like the Bnews,
;;     define the variable as follows:
;;
;;     (setq gnus-use-generic-from t)
;;
;; (6) Define autoload entries in .emacs file as follows:
;;
;;     (autoload 'gnus "gnus" "Read network news." t)
;;     (autoload 'gnus-post-news "gnuspost" "Post a news." t)
;;
;; (7) Read nntp.el if you have problems with NNTP or kanji handling.
;;
;; (8) Install mhspool.el, tcp.el, and tcp.c if it is necessary.
;;
;;     mhspool.el is a package for reading articles or mail in your
;;     private directory using GNUS.
;;
;;     tcp.el and tcp.c are necessary if and only if your Emacs does
;;     not have the function `open-network-stream' which is used for
;;     communicating with NNTP server inside Emacs.
;;
;; (9) Install an Info file generated from the texinfo manual gnus.texinfo.
;;
;;     If you are not allowed to create the Info file to the standard
;;     Info-directory, create it in your private directory and set the
;;     variable gnus-info-directory to that directory.
;;
;; For getting more information about GNUS, consult USENET newsgorup
;; gnu.emacs.gnus.

;; TO DO:
;; (1) Incremental update of active info.
;; (2) Asynchronous transmission of large messages.

;;; Code:

(require 'nntp)
(require 'mail-utils)
(require 'timezone)

(defvar gnus-default-nntp-server nil
  "*Specify default NNTP server.
This variable should be defined in `site-init.el'.")

(defvar gnus-nntp-server (or (getenv "NNTPSERVER") gnus-default-nntp-server)
  "*The name of the host running NNTP server.
If it is a string starting with a colon, as in as `:DIRECTORY', then the
directory ~/DIRECTORY is used as the news spool.
This variable is initialized from the NNTPSERVER environment variable
or from `gnus-default-nntp-server'.")

(defvar gnus-nntp-service "nntp"
  "*NNTP service name (\"nntp\" or 119).
Go to a local news spool if its value is nil.")

(defvar gnus-startup-file "~/.newsrc"
  "*Your `.newsrc' file.  Use `.newsrc-SERVER' instead if exists.")

(defvar gnus-signature-file "~/.signature"
  "*Your `.signature' file.  Use `.signature-DISTRIBUTION' instead if exists.")

(defvar gnus-use-cross-reference t
  "*Specifies what to do with cross references (Xref: field).
If nil, ignore cross references.  If t, mark articles as read in
subscribed newsgroups.  Otherwise, if not nil nor t, mark articles as
read in all newsgroups.")

(defvar gnus-use-followup-to t
  "*Specifies what to do with Followup-To: field.
If nil, ignore `Followup-to:' field.  If t, use its value except for
`poster'.  Otherwise, if not nil nor t, always use its value.")

(defvar gnus-large-newsgroup 50
  "*The number of articles which indicates a large newsgroup.
If the number of articles in a newsgroup is greater than the value,
confirmation is required for selecting the newsgroup.")

(defvar gnus-author-copy (getenv "AUTHORCOPY")
  "*File name saving a copy of an article posted using FCC: field.
Initialized from the AUTHORCOPY environment variable.

Articles are saved using a function specified by the the variable
`gnus-author-copy-saver' (`rmail-output' is default) if a file name is
given.  Instead, if the first character of the name is `|', the
contents of the article is piped out to the named program.  It is
possible to save an article in an MH folder as follows:

\(setq gnus-author-copy \"|/usr/local/lib/mh/rcvstore +Article\")")

(defvar gnus-author-copy-saver (function rmail-output)
  "*A function called with a file name to save an author copy to.
The default function is `rmail-output' which saves in  inbox format.")

(defvar gnus-use-long-file-name
  (not (memq system-type '(usg-unix-v xenix)))
  "*Non-nil means that a newsgroup name is used as a default file name
to save articles to.  If it's nil, the directory form of a newsgroup is
used instead.")

(defvar gnus-article-save-directory (getenv "SAVEDIR")
  "*A directory name to save articles to (default is `~/News').
Initialized from the SAVEDIR environment variable.")

(defvar gnus-kill-files-directory (getenv "SAVEDIR")
  "*A directory name to save kill files to (default to ~/News).
Initialized from the SAVEDIR environment variable.")

(defvar gnus-default-article-saver (function gnus-summary-save-in-rmail)
  "*A function to save articles in your favorite format.
The function must be interactively callable (in other words, it must
be an Emacs command).

GNUS provides the following functions:
	gnus-summary-save-in-rmail (in Rmail format)
	gnus-summary-save-in-mail (in Unix mail format)
	gnus-summary-save-in-folder (in an MH folder)
	gnus-summary-save-in-file (in article format).")

(defvar gnus-rmail-save-name (function gnus-plain-save-name)
  "*A function generating a file name to save articles in Rmail format.
The function is called with NEWSGROUP, HEADERS, and optional LAST-FILE.")

(defvar gnus-mail-save-name (function gnus-plain-save-name)
  "*A function generating a file name to save articles in Unix mail format.
The function is called with NEWSGROUP, HEADERS, and optional LAST-FILE.")

(defvar gnus-folder-save-name (function gnus-folder-save-name)
  "*A function generating a file name to save articles in MH folder.
The function is called with NEWSGROUP, HEADERS, and optional LAST-FOLDER.")

(defvar gnus-file-save-name (function gnus-numeric-save-name)
  "*A function generating a file name to save articles in article format.
The function is called with NEWSGROUP, HEADERS, and optional LAST-FILE.")

(defvar gnus-kill-file-name "KILL"
  "*File name of a KILL file.")

(defvar gnus-novice-user t
  "*Non-nil means that you are a novice to USENET.
If non-nil, verbose messages may be displayed
or your confirmations may be required.")

(defvar gnus-interactive-catchup t
  "*Require your confirmation when catching up a newsgroup if non-nil.")

(defvar gnus-interactive-post t
  "*Newsgroup, subject, and distribution will be asked for if non-nil.")

(defvar gnus-interactive-exit t
  "*Require your confirmation when exiting GNUS if non-nil.")

(defvar gnus-user-login-name nil
  "*The login name of the user.
Got from the function `user-login-name' if undefined.")

(defvar gnus-user-full-name nil
  "*The full name of the user.
Got from the NAME environment variable if undefined.")

(defvar gnus-show-mime nil
  "*Show MIME message if non-nil.")

(defvar gnus-show-threads t
  "*Show conversation threads in Summary Mode if non-nil.")

(defvar gnus-thread-hide-subject t
  "*Non-nil means hide subjects for thread subtrees.")

(defvar gnus-thread-hide-subtree nil
  "*Non-nil means hide thread subtrees initially.
If non-nil, you have to run the command `gnus-summary-show-thread' by
hand or by using `gnus-select-article-hook' to show hidden threads.")

(defvar gnus-thread-hide-killed t
  "*Non-nil means hide killed thread subtrees automatically.")

(defvar gnus-thread-ignore-subject nil
  "*Don't take care of subject differences, but only references if non-nil.
If it is non-nil, some commands work with subjects do not work properly.")

(defvar gnus-thread-indent-level 4
  "*Indentation of thread subtrees.")

(defvar gnus-ignored-newsgroups "^to\\..*$"
  "*A regexp to match uninteresting newsgroups in the active file.
Any lines in the active file matching this regular expression are
removed from the newsgroup list before anything else is done to it,
thus making them effectively invisible.")

(defvar gnus-ignored-headers
  "^Path:\\|^Posting-Version:\\|^Article-I.D.:\\|^Expires:\\|^Date-Received:\\|^References:\\|^Control:\\|^Xref:\\|^Lines:\\|^Posted:\\|^Relay-Version:\\|^Message-ID:\\|^Nf-ID:\\|^Nf-From:\\|^Approved:\\|^Sender:"
  "*Header fields not worth displaying.
Ordinarily GNUS excludes these when displaying an article.
If you want to see them, ask to see the message with \"the full header\"
\(also known as \"the original header\").")

(defvar gnus-required-headers
  '(From Date Newsgroups Subject Message-ID Path Organization Distribution)
  "*All required fields for articles you post.
RFC977 and RFC1036 require From, Date, Newsgroups, Subject, Message-ID
and Path fields.  Organization, Distribution and Lines are optional.
If you want GNUS not to insert some field, remove it from this list.")

(defvar gnus-show-all-headers nil
  "*Show all headers of an article if non-nil.")

(defvar gnus-save-all-headers t
  "*Save all headers of an article if non-nil.")

(defvar gnus-optional-headers (function gnus-optional-lines-and-from)
  "*A function generating a optional string displayed in GNUS Summary
mode buffer.  The function is called with an article HEADER.  The
result must be a string excluding `[' and `]'.")

(defvar gnus-auto-extend-newsgroup t
  "*Extend visible articles to forward and backward if non-nil.")

(defvar gnus-auto-select-first t
  "*Select the first unread article automagically if non-nil.
If you want to prevent automatic selection of the first unread article
in some newsgroups, set the variable to nil in `gnus-select-group-hook'
or `gnus-apply-kill-hook'.")

(defvar gnus-auto-select-next t
  "*Select the next newsgroup automagically if non-nil.
If the value is t and the next newsgroup is empty, GNUS will exit
Summary mode and go back to Group mode.  If the value is neither nil
nor t, GNUS will select the following unread newsgroup.  Especially, if
the value is the symbol `quietly', the next unread newsgroup will be
selected without any confirmations.")

(defvar gnus-auto-select-same nil
  "*Select the next article with the same subject automagically if non-nil.")

(defvar gnus-auto-center-summary t
  "*Always center the current summary in GNUS Summary window if non-nil.")

(defvar gnus-auto-mail-to-author nil
  "*Insert `To: author' of the article when following up if non-nil.
Mail is sent using the function specified by the variable
`gnus-mail-send-method'.")

(defvar gnus-break-pages t
  "*Break an article into pages if non-nil.
Page delimiter is specified by the variable `gnus-page-delimiter'.")

(defvar gnus-page-delimiter "^\^L"
  "*Regexp describing line-beginnings that separate pages of news article.")

(defvar gnus-digest-show-summary t
  "*Show a summary of undigestified messages if non-nil.")

(defvar gnus-digest-separator "^Subject:[ \t]"
  "*Regexp that separates messages in a digest article.")

(defvar gnus-use-full-window t
  "*Non-nil means to take up the entire screen of Emacs.")

(defvar gnus-window-configuration
  '((summary (0 1 0))
    (newsgroups   (1 0 0))
    (article   (0 3 10)))
  "*Specify window configurations for each action.
The format of the variable is a list of (ACTION (G S A)), where G, S,
and A are the relative height of Group, Summary, and Article windows,
respectively.  ACTION is `summary', `newsgroups', or `article'.")

(defvar gnus-show-mime-method (function metamail-buffer)
  "*Function to process a MIME message.
The function is expected to process current buffer as a MIME message.")

(defvar gnus-mail-reply-method
  (function gnus-mail-reply-using-mail)
  "*Function to compose reply mail.
The function `gnus-mail-reply-using-mail' uses usual sendmail mail
program.  The function `gnus-mail-reply-using-mhe' uses the MH-E mail
program.  You can use yet another program by customizing this variable.")

(defvar gnus-mail-forward-method
  (function gnus-mail-forward-using-mail)
  "*Function to forward current message to another user.
The function `gnus-mail-reply-using-mail' uses usual sendmail mail
program.  You can use yet another program by customizing this variable.")

(defvar gnus-mail-other-window-method
  (function gnus-mail-other-window-using-mail)
  "*Function to compose mail in other window.
The function `gnus-mail-other-window-using-mail' uses the usual sendmail
mail program.  The function `gnus-mail-other-window-using-mhe' uses the MH-E
mail program.  You can use yet another program by customizing this variable.")

(defvar gnus-mail-send-method send-mail-function
  "*Function to mail a message too which is being posted as an article.
The message must have To: or Cc: field.  The default is copied from
the variable `send-mail-function'.")

(defvar gnus-subscribe-newsgroup-method
  (function gnus-subscribe-alphabetically)
  "*Function called with a newsgroup name when new newsgroup is found.
The function `gnus-subscribe-randomly' inserts a new newsgroup a the
beginning of newsgroups.  The function `gnus-subscribe-alphabetically'
inserts it in strict alphabetic order.  The function
`gnus-subscribe-hierarchically' inserts it in hierarchical newsgroup
order.  The function `gnus-subscribe-interactively' asks for your decision.")

(defvar gnus-group-mode-hook nil
  "*A hook for GNUS Group Mode.")

(defvar gnus-summary-mode-hook nil
  "*A hook for GNUS Summary Mode.")

(defvar gnus-article-mode-hook nil
  "*A hook for GNUS Article Mode.")

(defvar gnus-kill-file-mode-hook nil
  "*A hook for GNUS KILL File Mode.")

(defvar gnus-open-server-hook nil
  "*A hook called just before opening connection to news server.")

(defvar gnus-startup-hook nil
  "*A hook called at start up time.
This hook is called after GNUS is connected to the NNTP server.  So, it
is possible to change the behavior of GNUS according to the selected
NNTP server.")

(defvar gnus-group-prepare-hook nil
  "*A hook called after newsgroup list is created in the Newsgroup buffer.
If you want to modify the Newsgroup buffer, you can use this hook.")

(defvar gnus-summary-prepare-hook nil
  "*A hook called after summary list is created in the Summary buffer.
If you want to modify the Summary buffer, you can use this hook.")

(defvar gnus-article-prepare-hook nil
  "*A hook called after an article is prepared in the Article buffer.
If you want to run a special decoding program like nkf, use this hook.")

(defvar gnus-select-group-hook nil
  "*A hook called when a newsgroup is selected.
If you want to sort Summary buffer by date and then by subject, you
can use the following hook:

\(add-hook 'gnus-select-group-hook
       (function
	(lambda ()
	  ;; First of all, sort by date.
	  (gnus-keysort-headers
	   (function string-lessp)
	   (function
	    (lambda (a)
	      (gnus-sortable-date (gnus-header-date a)))))
	  ;; Then sort by subject string ignoring `Re:'.
	  ;; If case-fold-search is non-nil, case of letters is ignored.
	  (gnus-keysort-headers
	   (function string-lessp)
	   (function
	    (lambda (a)
	      (if case-fold-search
		  (downcase (gnus-simplify-subject (gnus-header-subject a) t))
		(gnus-simplify-subject (gnus-header-subject a) t)))))
	  )))

If you'd like to simplify subjects like the
`gnus-summary-next-same-subject' command does, you can use the
following hook:

\(add-hook 'gnus-select-group-hook
       (function
	(lambda ()
	  (mapcar (function
		   (lambda (header)
		     (nntp-set-header-subject
		      header
		      (gnus-simplify-subject
		       (gnus-header-subject header) 're-only))))
		  gnus-newsgroup-headers))))

In some newsgroups author name is meaningless.  It is possible to
prevent listing author names in GNUS Summary buffer as follows:

\(add-hook 'gnus-select-group-hook
       (function
	(lambda ()
	  (cond ((string-equal \"comp.sources.unix\" gnus-newsgroup-name)
		 (setq gnus-optional-headers
		       (function gnus-optional-lines)))
		(t
		 (setq gnus-optional-headers
		       (function gnus-optional-lines-and-from)))))))")

(defvar gnus-select-article-hook
  '(gnus-summary-show-thread)
  "*A hook called when an article is selected.
The default hook shows conversation thread subtrees of the selected
article automatically using `gnus-summary-show-thread'.

If you'd like to run Rmail on a digest article automagically, you can
use the following hook:

\(add-hook 'gnus-select-article-hook
       (function
	(lambda ()
	  (cond ((string-equal \"comp.sys.sun\" gnus-newsgroup-name)
		 (gnus-summary-rmail-digest))
		((and (string-equal \"comp.text\" gnus-newsgroup-name)
		      (string-match \"^TeXhax Digest\"
				    (gnus-header-subject gnus-current-headers)))
		 (gnus-summary-rmail-digest)
		 ))))
       t)")

(defvar gnus-select-digest-hook
  (list
   (function
    (lambda ()
      ;; Reply-To: is required by `undigestify-rmail-message'.
      (or (mail-position-on-field "Reply-to" t)
	  (progn
	    (mail-position-on-field "Reply-to")
	    (insert (gnus-fetch-field "From")))))))
  "*A hook called when reading digest messages using Rmail.
This hook can be used to modify incomplete digest articles as follows
\(this is the default):

\(add-hook 'gnus-select-digest-hook
       (function
	(lambda ()
	  ;; Reply-To: is required by `undigestify-rmail-message'.
	  (or (mail-position-on-field \"Reply-to\" t)
	      (progn
		(mail-position-on-field \"Reply-to\")
		(insert (gnus-fetch-field \"From\")))))))")

(defvar gnus-rmail-digest-hook nil
  "*A hook called when reading digest messages using Rmail.
This hook is intended to customize Rmail mode for reading digest articles.")

(defvar gnus-apply-kill-hook '(gnus-apply-kill-file)
  "*A hook called when a newsgroup is selected and summary list is prepared.
This hook is intended to apply a KILL file to the selected newsgroup.
The function `gnus-apply-kill-file' is called by default.

Since a general KILL file is too heavy to use only for a few
newsgroups, I recommend you to use a lighter hook function.  For
example, if you'd like to apply a KILL file to articles which contains
a string `rmgroup' in subject in newsgroup `control', you can use the
following hook:

\(setq gnus-apply-kill-hook
      (list
       (function
	(lambda ()
	  (cond ((string-match \"control\" gnus-newsgroup-name)
		 (gnus-kill \"Subject\" \"rmgroup\")
		 (gnus-expunge \"X\")))))))")

(defvar gnus-mark-article-hook
  (list
   (function
    (lambda ()
      (or (memq gnus-current-article gnus-newsgroup-marked)
	  (gnus-summary-mark-as-read gnus-current-article))
      (gnus-summary-set-current-mark "+"))))
  "*A hook called when an article is selected at the first time.
The hook is intended to mark an article as read (or unread)
automatically when it is selected.

If you'd like to mark as unread (-) instead, use the following hook:

\(setq gnus-mark-article-hook
      (list
       (function
        (lambda ()
	  (gnus-summary-mark-as-unread gnus-current-article)
	  (gnus-summary-set-current-mark \"+\")))))")

(defvar gnus-prepare-article-hook (list (function gnus-inews-insert-signature))
  "*A hook called after preparing body, but before preparing header fields.
The default hook (`gnus-inews-insert-signature') inserts a signature
file specified by the variable `gnus-signature-file'.")

(defvar gnus-inews-article-hook (list (function gnus-inews-do-fcc))
  "*A hook called before finally posting an article.
The default hook (`gnus-inews-do-fcc') does FCC processing (save article
to a file).")

(defvar gnus-exit-group-hook nil
  "*A hook called when exiting (not quitting) Summary mode.
If your machine is so slow that exiting from Summary mode takes very
long time, set the variable `gnus-use-cross-reference' to nil.  This
inhibits marking articles as read using cross-reference information.")

(defvar gnus-suspend-gnus-hook nil
  "*A hook called when suspending (not exiting) GNUS.")

(defvar gnus-exit-gnus-hook nil
  "*A hook called when exiting (not suspending) GNUS.")

(defvar gnus-save-newsrc-hook nil
  "*A hook called when saving the newsrc file.
This hook is called before saving the `.newsrc' file.")


;; Site dependent variables.  You have to define these variables in
;;  site-init.el, default.el or your .emacs.

(defvar gnus-local-timezone nil
  "*Local time zone.
This value is used only if `current-time-zone' does not work in your Emacs.
It specifies the GMT offset, i.e. a decimal integer
of the form +-HHMM giving the hours and minutes ahead of (i.e. east of) GMT.
For example, +0900 should be used in Japan, since it is 9 hours ahead of GMT.

For backwards compatibility, it may also be a string like \"JST\",
but strings are obsolescent: you should use numeric offsets instead.")

(defvar gnus-local-domain nil
  "*Local domain name without a host name like: \"stars.flab.Fujitsu.CO.JP\"
The `DOMAINNAME' environment variable is used instead if defined.  If
the function (system-name) returns the full internet name, there is no
need to define the name.")

(defvar gnus-local-organization nil
  "*Local organization like: \"Fujitsu Laboratories Ltd., Kawasaki, Japan.\"
The `ORGANIZATION' environment variable is used instead if defined.")

(defvar gnus-local-distributions '("local" "world")
  "*List of distributions.
The first element in the list is used as default.  If distributions
file is available, its content is also used.")

(defvar gnus-use-generic-from nil
  "*If nil, prepend local host name to the defined domain in the From:
field; if a string, use this; if non-nil, strip of the local host name.")

(defvar gnus-use-generic-path nil
  "*If nil, use the NNTP server name in the Path: field; if stringp,
use this; if non-nil, use no host name (user name only)")

(defvar gnus-newsgroups-regex "^\\([^ \t\n]+\\)[ \t]+\\(.*\\)$"
  "Regex to retrieve the group name and the group description from
the output of the newsgroups listing.

If you have ^M at the end of lines try \"^\\([^ \t\n]+\\)[ \t]+\\([^\r]+\\)[\r]*$\"")

(defvar gnus-newsgroups-display t
  "*display the newsgroup description in *Newsgroup* buffer if not nil")

(defvar gnus-newsgroups-alist nil
  "alist (groupname . description)")

(defvar gnus-newsgroups-hashtb nil
  "hashtable of gnus-newsgroups-alist")

(defvar gnus-newsgroups-showall nil
  "non nil if we display all the groups")


;; Internal variables.

(defconst gnus-version "GNUS 4.1"
  "Version numbers of this version of GNUS.")

(defconst gnus-emacs-version
  (progn
    (string-match "[0-9]*" emacs-version)
    (string-to-int (substring emacs-version
			      (match-beginning 0) (match-end 0))))
  "Major version number of this emacs.")

(defvar gnus-info-nodes
  '((gnus-group-mode		"(gnus)Newsgroup Commands")
    (gnus-summary-mode		"(gnus)Summary Commands")
    (gnus-article-mode		"(gnus)Article Commands")
    (gnus-kill-file-mode	"(gnus)Kill File")
    (gnus-browse-killed-mode	"(gnus)Maintaining Subscriptions"))
  "Assoc list of major modes and related Info nodes.")

;; Alist syntax is different from that of 3.14.3.
(defvar gnus-access-methods
  '((nntp
     (gnus-retrieve-headers		nntp-retrieve-headers)
     (gnus-open-server			nntp-open-server)
     (gnus-close-server			nntp-close-server)
     (gnus-server-opened		nntp-server-opened)
     (gnus-status-message		nntp-status-message)
     (gnus-request-article		nntp-request-article)
     (gnus-request-group		nntp-request-group)
     (gnus-request-list			nntp-request-list)
     (gnus-request-list-newsgroups	nntp-request-list-newsgroups)
     (gnus-request-list-distributions	nntp-request-list-distributions)
     (gnus-request-post			nntp-request-post))
    (nnspool
     (gnus-retrieve-headers		nnspool-retrieve-headers)
     (gnus-open-server			nnspool-open-server)
     (gnus-close-server			nnspool-close-server)
     (gnus-server-opened		nnspool-server-opened)
     (gnus-status-message		nnspool-status-message)
     (gnus-request-article		nnspool-request-article)
     (gnus-request-group		nnspool-request-group)
     (gnus-request-list			nnspool-request-list)
     (gnus-request-list-newsgroups	nnspool-request-list-newsgroups)
     (gnus-request-list-distributions	nnspool-request-list-distributions)
     (gnus-request-post			nnspool-request-post))
    (mhspool
     (gnus-retrieve-headers		mhspool-retrieve-headers)
     (gnus-open-server			mhspool-open-server)
     (gnus-close-server			mhspool-close-server)
     (gnus-server-opened		mhspool-server-opened)
     (gnus-status-message		mhspool-status-message)
     (gnus-request-article		mhspool-request-article)
     (gnus-request-group		mhspool-request-group)
     (gnus-request-list			mhspool-request-list)
     (gnus-request-list-newsgroups	mhspool-request-list-newsgroups)
     (gnus-request-list-distributions	mhspool-request-list-distributions)
     (gnus-request-post			mhspool-request-post)))
  "Access method for NNTP, nnspool, and mhspool.")

(defvar gnus-group-buffer "*Newsgroup*")
(defvar gnus-summary-buffer "*Summary*")
(defvar gnus-article-buffer "*Article*")
(defvar gnus-digest-buffer "GNUS Digest")
(defvar gnus-digest-summary-buffer "GNUS Digest-summary")

(defvar gnus-buffer-list
  (list gnus-group-buffer gnus-summary-buffer gnus-article-buffer
	gnus-digest-buffer gnus-digest-summary-buffer)
  "GNUS buffer names which should be killed when exiting.")

(defvar gnus-variable-list
  '(gnus-newsrc-options
    gnus-newsrc-options-n-yes gnus-newsrc-options-n-no
    gnus-newsrc-assoc gnus-killed-assoc gnus-marked-assoc)
  "GNUS variables saved in the quick startup file.")

(defvar gnus-overload-functions
  '((news-inews gnus-inews-news "rnewspost")
    (caesar-region gnus-caesar-region "rnews"))
  "Functions overloaded by gnus.
It is a list of `(original overload &optional file)'.")

(defvar gnus-distribution-list nil)

(defvar gnus-newsrc-options nil
  "Options line in the `.newsrc' file.")

(defvar gnus-newsrc-options-n-yes nil
  "Regexp representing subscribed newsgroups.")

(defvar gnus-newsrc-options-n-no nil
  "Regexp representing unsubscribed newsgroups.")

(defvar gnus-newsrc-assoc nil
  "Assoc list of read articles.
`gnus-newsrc-hashtb' should be kept so that both hold the same information.")

(defvar gnus-newsrc-hashtb nil
  "Hashtable of `gnus-newsrc-assoc'.")

(defvar gnus-killed-assoc nil
  "Assoc list of newsgroups removed from `gnus-newsrc-assoc'.
`gnus-killed-hashtb' should be kept so that both hold the same information.")

(defvar gnus-killed-hashtb nil
  "Hashtable of `gnus-killed-assoc'.")

(defvar gnus-marked-assoc nil
  "Assoc list of articles marked as unread.
`gnus-marked-hashtb' should be kept so that both hold the same information.")

(defvar gnus-marked-hashtb nil
  "Hashtable of `gnus-marked-assoc'.")

(defvar gnus-unread-hashtb nil
  "Hashtable of unread articles.")

(defvar gnus-active-hashtb nil
  "Hashtable of active articles.")

(defvar gnus-octive-hashtb nil
  "Hashtable of OLD active articles.")

(defvar gnus-current-startup-file nil
  "Startup file for the current host.")

(defvar gnus-last-search-regexp nil
  "Default regexp for article search command.")

(defvar gnus-last-shell-command nil
  "Default shell command on article.")

(defvar gnus-have-all-newsgroups nil)

(defvar gnus-newsgroup-name nil)
(defvar gnus-newsgroup-begin nil)
(defvar gnus-newsgroup-end nil)
(defvar gnus-newsgroup-last-rmail nil)
(defvar gnus-newsgroup-last-mail nil)
(defvar gnus-newsgroup-last-folder nil)
(defvar gnus-newsgroup-last-file nil)

(defvar gnus-newsgroup-unreads nil
  "List of unread articles in the current newsgroup.")

(defvar gnus-newsgroup-unselected nil
  "List of unselected unread articles in the current newsgroup.")

(defvar gnus-newsgroup-marked nil
  "List of marked articles in the current newsgroup (a subset of unread art).")

(defvar gnus-newsgroup-headers nil
  "List of article headers in the current newsgroup.
If you modify the variable, you must call the function
`gnus-clear-hashtables-for-newsgroup-headers' to clear the hash tables.")
(defvar gnus-newsgroup-headers-hashtb-by-id nil)
(defvar gnus-newsgroup-headers-hashtb-by-number nil)

(defvar gnus-current-article nil)
(defvar gnus-current-headers nil)
(defvar gnus-current-history nil)
(defvar gnus-have-all-headers nil "Must be either T or NIL.")
(defvar gnus-last-article nil)
(defvar gnus-current-kill-article nil)

;; Save window configuration.
(defvar gnus-winconf-kill-file nil)

(defvar gnus-group-mode-map nil)
(defvar gnus-summary-mode-map nil)
(defvar gnus-article-mode-map nil)
(defvar gnus-kill-file-mode-map nil)

(defvar rmail-default-file (expand-file-name "~/XMBOX"))
(defvar rmail-default-rmail-file (expand-file-name "~/XNEWS"))

;; Define GNUS Subsystems.
(autoload 'gnus-group-post-news "gnuspost"
	  "Post an article." t)
(autoload 'gnus-summary-post-news "gnuspost"
	  "Post an article." t)
(autoload 'gnus-summary-followup "gnuspost"
	  "Post a reply article." t)
(autoload 'gnus-summary-followup-with-original "gnuspost"
	  "Post a reply article with original article." t)
(autoload 'gnus-summary-cancel-article "gnuspost"
	  "Cancel an article you posted." t)

(autoload 'gnus-summary-reply "gnusmail"
	  "Reply mail to news author." t)
(autoload 'gnus-summary-reply-with-original "gnusmail"
	  "Reply mail to news author with original article." t)
(autoload 'gnus-summary-mail-forward "gnusmail"
	  "Forward the current message to another user." t)
(autoload 'gnus-summary-mail-other-window "gnusmail"
	  "Compose mail in other window." t)

(autoload 'gnus-group-kill-group "gnusmisc"
	  "Kill newsgroup on current line." t)
(autoload 'gnus-group-yank-group "gnusmisc"
	  "Yank the last killed newsgroup on current line." t)
(autoload 'gnus-group-kill-region "gnusmisc"
	  "Kill newsgroups in current region." t)
(autoload 'gnus-group-transpose-groups "gnusmisc"
	  "Exchange current newsgroup and previous newsgroup." t)
(autoload 'gnus-list-killed-groups "gnusmisc"
	  "List the killed newsgroups." t)
(autoload 'gnus-gmt-to-local "gnusmisc"
	  "Rewrite Date field in GMT to local in current buffer.")

(autoload 'metamail-buffer "metamail"
	  "Process current buffer through `metamail'." t)

(autoload 'rmail-output "rmailout"
	  "Append this message to Unix mail file named FILE-NAME." t)
(autoload 'mail-position-on-field "sendmail")
(autoload 'mh-find-path "mh-e")
(autoload 'mh-prompt-for-folder "mh-e")

(put 'gnus-group-mode 'mode-class 'special)
(put 'gnus-summary-mode 'mode-class 'special)
(put 'gnus-article-mode 'mode-class 'special)

(autoload 'gnus-uu-ctl-map "gnus-uu" nil nil 'keymap)
(autoload 'gnus-uu-mark-article "gnus-uu" nil t)

;;(put 'gnus-eval-in-buffer-window 'lisp-indent-hook 1)

(defmacro gnus-eval-in-buffer-window (buffer &rest forms)
  "Pop to BUFFER, evaluate FORMS, and then returns to original window."
  (` (let ((GNUSStartBufferWindow (selected-window)))
       (unwind-protect
	   (progn
	     (pop-to-buffer (, buffer))
	     (,@ forms))
	 (select-window GNUSStartBufferWindow)))))

(defmacro gnus-make-hashtable (&optional hashsize)
  "Make a hash table (default and minimum size is 200).
Optional argument HASHSIZE specifies the table size."
  (` (make-vector (, (if hashsize (` (max (, hashsize) 200)) 200)) 0)))

(defmacro gnus-gethash (string hashtable)
  "Get hash value of STRING in HASHTABLE."
  ;;(` (symbol-value (abbrev-symbol (, string) (, hashtable))))
  ;;(` (abbrev-expansion (, string) (, hashtable)))
  (` (symbol-value (intern-soft (, string) (, hashtable)))))

(defmacro gnus-sethash (string value hashtable)
  "Set hash value.  Arguments are STRING, VALUE, and HASHTABLE."
  ;; We cannot use define-abbrev since it only accepts string as value.
  (` (set (intern (, string) (, hashtable)) (, value))))

;; Note: Macros defined here are also defined in nntp.el.  I don't like
;; to put them here, but many users got troubled with the old
;; definitions in nntp.elc.  These codes are NNTP 3.10 version.

(defmacro nntp-header-number (header)
  "Return article number in HEADER."
  (` (aref (, header) 0)))

(defmacro nntp-set-header-number (header number)
  "Set article number of HEADER to NUMBER."
  (` (aset (, header) 0 (, number))))

(defmacro nntp-header-subject (header)
  "Return subject string in HEADER."
  (` (aref (, header) 1)))

(defmacro nntp-set-header-subject (header subject)
  "Set article subject of HEADER to SUBJECT."
  (` (aset (, header) 1 (, subject))))

(defmacro nntp-header-from (header)
  "Return author string in HEADER."
  (` (aref (, header) 2)))

(defmacro nntp-set-header-from (header from)
  "Set article author of HEADER to FROM."
  (` (aset (, header) 2 (, from))))

(defmacro nntp-header-xref (header)
  "Return xref string in HEADER."
  (` (aref (, header) 3)))

(defmacro nntp-set-header-xref (header xref)
  "Set article xref of HEADER to xref."
  (` (aset (, header) 3 (, xref))))

(defmacro nntp-header-lines (header)
  "Return lines in HEADER."
  (` (aref (, header) 4)))

(defmacro nntp-set-header-lines (header lines)
  "Set article lines of HEADER to LINES."
  (` (aset (, header) 4 (, lines))))

(defmacro nntp-header-date (header)
  "Return date in HEADER."
  (` (aref (, header) 5)))

(defmacro nntp-set-header-date (header date)
  "Set article date of HEADER to DATE."
  (` (aset (, header) 5 (, date))))

(defmacro nntp-header-id (header)
  "Return Id in HEADER."
  (` (aref (, header) 6)))

(defmacro nntp-set-header-id (header id)
  "Set article Id of HEADER to ID."
  (` (aset (, header) 6 (, id))))

(defmacro nntp-header-references (header)
  "Return references in HEADER."
  (` (aref (, header) 7)))

(defmacro nntp-set-header-references (header ref)
  "Set article references of HEADER to REF."
  (` (aset (, header) 7 (, ref))))


;;;
;;; GNUS Group Mode
;;;

(if gnus-group-mode-map
    nil
  (setq gnus-group-mode-map (make-keymap))
  (suppress-keymap gnus-group-mode-map)
  (define-key gnus-group-mode-map " " 'gnus-group-read-group)
  (define-key gnus-group-mode-map "=" 'gnus-group-select-group)
  (define-key gnus-group-mode-map "j" 'gnus-group-jump-to-group)
  (define-key gnus-group-mode-map "n" 'gnus-group-next-unread-group)
  (define-key gnus-group-mode-map "p" 'gnus-group-prev-unread-group)
  (define-key gnus-group-mode-map "\177" 'gnus-group-prev-unread-group)
  (define-key gnus-group-mode-map "N" 'gnus-group-next-group)
  (define-key gnus-group-mode-map "P" 'gnus-group-prev-group)
  (define-key gnus-group-mode-map "\C-n" 'gnus-group-next-group)
  (define-key gnus-group-mode-map "\C-p" 'gnus-group-prev-group)
  (define-key gnus-group-mode-map [down] 'gnus-group-next-group)
  (define-key gnus-group-mode-map [up] 'gnus-group-prev-group)
  (define-key gnus-group-mode-map "\r" 'next-line)
  ;;(define-key gnus-group-mode-map "/" 'isearch-forward)
  (define-key gnus-group-mode-map "<" 'beginning-of-buffer)
  (define-key gnus-group-mode-map ">" 'end-of-buffer)
  (define-key gnus-group-mode-map "u" 'gnus-group-unsubscribe-current-group)
  (define-key gnus-group-mode-map "U" 'gnus-group-unsubscribe-group)
  (define-key gnus-group-mode-map "c" 'gnus-group-catchup)
  (define-key gnus-group-mode-map "C" 'gnus-group-catchup-all)
  (define-key gnus-group-mode-map "l" 'gnus-group-list-groups)
  (define-key gnus-group-mode-map "L" 'gnus-group-list-all-groups)
  (define-key gnus-group-mode-map "g" 'gnus-group-get-new-news)
  (define-key gnus-group-mode-map "R" 'gnus-group-restart)
  (define-key gnus-group-mode-map "b" 'gnus-group-check-bogus-groups)
  (define-key gnus-group-mode-map "r" 'gnus-group-restrict-groups)
  (define-key gnus-group-mode-map "a" 'gnus-group-post-news)
  (define-key gnus-group-mode-map "\ek" 'gnus-group-edit-local-kill)
  (define-key gnus-group-mode-map "\eK" 'gnus-group-edit-global-kill)
  (define-key gnus-group-mode-map "\C-k" 'gnus-group-kill-group)
  (define-key gnus-group-mode-map "\C-y" 'gnus-group-yank-group)
  (define-key gnus-group-mode-map "\C-w" 'gnus-group-kill-region)
  (define-key gnus-group-mode-map "\C-x\C-t" 'gnus-group-transpose-groups)
  (define-key gnus-group-mode-map "\C-c\C-l" 'gnus-list-killed-groups)
  (define-key gnus-group-mode-map "V" 'gnus-version)
  ;;(define-key gnus-group-mode-map "x" 'gnus-group-force-update)
  (define-key gnus-group-mode-map "s" 'gnus-group-force-update)
  (define-key gnus-group-mode-map "z" 'gnus-group-suspend)
  (define-key gnus-group-mode-map "q" 'gnus-group-exit)
  (define-key gnus-group-mode-map "Q" 'gnus-group-quit)
  (define-key gnus-group-mode-map "?" 'gnus-group-describe-briefly)
  (define-key gnus-group-mode-map "\C-c\C-i" 'gnus-info-find-node)
  (define-key gnus-group-mode-map   [mouse-2] 'gnus-mouse-pick-group)
  (define-key gnus-group-mode-map "t" 'gnus-newsgroups-display-toggle)

  ;; Make a menu bar item.
  (define-key gnus-group-mode-map [menu-bar GNUS]
	(cons "GNUS" (make-sparse-keymap "GNUS")))

  (define-key gnus-group-mode-map [menu-bar GNUS force-update]
	'("Force Update" . gnus-group-force-update))
  (define-key gnus-group-mode-map [menu-bar GNUS quit]
	'("Quit" . gnus-group-quit))
  (define-key gnus-group-mode-map [menu-bar GNUS exit]
	'("Exit" . gnus-group-exit))
  (define-key gnus-group-mode-map [menu-bar GNUS restart]
	'("Restart" . gnus-group-restart))
  (define-key gnus-group-mode-map [menu-bar GNUS suspend]
	'("Suspend" . gnus-group-suspend))
  (define-key gnus-group-mode-map [menu-bar GNUS get-new-news]
	'("Get New News" . gnus-group-get-new-news))

  ;; Make a menu bar item.
  (define-key gnus-group-mode-map [menu-bar groups]
	(cons "Groups" (make-sparse-keymap "Groups")))

  (define-key gnus-group-mode-map [menu-bar groups catchup]
	'("Catchup" . gnus-group-catchup))
  (define-key gnus-group-mode-map [menu-bar groups edit-global-kill]
	'("Edit Kill File" . gnus-group-edit-global-kill))

  (define-key gnus-group-mode-map [menu-bar groups separator-2]
	'("--"))

  (define-key gnus-group-mode-map [menu-bar groups yank-group]
	'("Yank Group" . gnus-group-yank-group))
  (define-key gnus-group-mode-map [menu-bar groups kill-group]
	'("Kill Group" . gnus-group-kill-group))

  (define-key gnus-group-mode-map [menu-bar groups separator-1]
	'("--"))

  (define-key gnus-group-mode-map [menu-bar groups newsgroups-update-description]
        '("Update descriptions" . gnus-newsgroups-update-description))
  (define-key gnus-group-mode-map [menu-bar groups newsgroups-display-toggle]
        '("Toggle descriptions" . gnus-newsgroups-display-toggle))
  (define-key gnus-group-mode-map [menu-bar groups jump-to-group]
	'("Jump to Group..." . gnus-group-jump-to-group))
  (define-key gnus-group-mode-map [menu-bar groups list-all-groups]
	'("List All Groups" . gnus-group-list-all-groups))
  (define-key gnus-group-mode-map [menu-bar groups list-groups]
	'("List Groups" . gnus-group-list-groups))
  (define-key gnus-group-mode-map [menu-bar groups unsub-current-group]
	'("Unsubscribe Group" . gnus-group-unsubscribe-current-group))
  )

(defun gnus-group-mode ()
  "Major mode for reading network news.
All normal editing commands are turned off.
Instead, these commands are available:

SPC	Read articles in this newsgroup.
=	Select this newsgroup.
j	Move to the specified newsgroup.
n	Move to the next unread newsgroup.
p	Move to the previous unread newsgroup.
C-n	Move to the next newsgroup.
C-p	Move to the previous newsgroup.
<	Move point to the beginning of this buffer.
>	Move point to the end of this buffer.
u	Unsubscribe from (subscribe to) this newsgroup.
U	Unsubscribe from (subscribe to) the specified newsgroup.
c	Mark all articles as read, preserving marked articles.
C	Mark all articles in this newsgroup as read.
l	Revert this buffer.
L	List all newsgroups.
g	Get new news.
R	Force to read the raw .newsrc file and get new news.
b	Check bogus newsgroups.
r	Restrict visible newsgroups to the current region.
a	Post a new article.
ESC k	Edit a local KILL file applied to this newsgroup.
ESC K	Edit a global KILL file applied to all newsgroups.
C-k	Kill this newsgroup.
C-y	Yank killed newsgroup here.
C-w	Kill newsgroups in current region (excluding current point).
C-x C-t	Exchange this newsgroup and previous newsgroup.
C-c C-l	list killed newsgroups.
s	Save .newsrc file.
z	Suspend reading news.
q	Quit reading news.
Q	Quit reading news without saving .newsrc file.
V	Show the version number of this GNUS.
?	Describe Group Mode commands briefly.
C-h m	Describe Group Mode.
C-c C-i	Read Info about Group Mode.
t       Toggle displaying newsgroup descriptions.

  The name of the host running NNTP server is asked for if no default
host is specified.  It is also possible to choose another NNTP server
even when the default server is defined by giving a prefix argument to
the command `\\[gnus]'.

  If the NNTP server name starts with a colon, as in `:Mail', the user's
own directory `~/Mail' is used as a news spool.  This makes it
possible to read mail stored in MH folders or articles saved by GNUS.
File names of mail or articles must consist of only numeric
characters.  Otherwise, they are ignored.

  If there is a file named `~/.newsrc-SERVER', it is used as the
startup file instead of standard one when talking to SERVER.  It is
possible to talk to many hosts by using different startup files for
each.

  Option `-n' of the options line in the startup file is recognized
properly the same as the Bnews system.  For example, if the options
line is `options -n !talk talk.rumors', newsgroups under the `talk'
hierarchy except for `talk.rumors' are ignored while checking new
newsgroups.

  If there is a file named `~/.signature-DISTRIBUTION', it is used as
signature file instead of standard one when posting a news in
DISTRIBUTION.

  If an Info file generated from `gnus.texinfo' is installed, you can
read an appropriate Info node of the Info file according to the
current major mode of GNUS by \\[gnus-info-find-node].

  The variable `gnus-version', `nntp-version', `nnspool-version', and
`mhspool-version' have the version numbers of this version of gnus.el,
nntp.el, nnspool.el, and mhspoo.el, respectively.

User customizable variables:
 gnus-nntp-server
    Specifies the name of the host running the NNTP server.  If its
    value is a string such as `:DIRECTORY', the user's private
    DIRECTORY is used as a news spool.  The variable is initialized
    from the NNTPSERVER environment variable.

 gnus-nntp-service
    Specifies a NNTP service name.  It is usually \"nntp\" or 119.
    Nil forces GNUS to use a local news spool if the variable
    `gnus-nntp-server' is set to the local host name.

 gnus-startup-file
    Specifies a startup file (.newsrc).  If there is a file named
    `.newsrc-SERVER', it's used instead when talking to SERVER.  I
    recommend you to use the server specific file, if you'd like to
    talk to many servers.  Especially if you'd like to read your
    private directory, the name of the file must be
    `.newsrc-:DIRECTORY'.

 gnus-signature-file
    Specifies a signature file (.signature).  If there is a file named
    `.signature-DISTRIBUTION', it's used instead when posting an
    article in DISTRIBUTION.  Set the variable to nil to prevent
    appending the file automatically.  If you use an NNTP inews which
    comes with the NNTP package, you may have to set the variable to
    nil.

 gnus-use-cross-reference
    Specifies what to do with cross references (Xref: field).  If it
    is nil, cross references are ignored.  If it is t, articles in
    subscribed newsgroups are only marked as read.  Otherwise, if it
    is not nil nor t, articles in all newsgroups are marked as read.

 gnus-use-followup-to
    Specifies what to do with followup-to: field.  If it is nil, its
    value is ignored.  If it is non-nil, its value is used as followup
    newsgroups.  Especially, if it is t and field value is `poster',
    your confirmation is required.

 gnus-author-copy
    Specifies a file name to save a copy of article you posted using
    FCC: field.  If the first character of the value is `|', the
    contents of the article is piped out to a program specified by the
    rest of the value.  The variable is initialized from the
    AUTHORCOPY environment variable.

 gnus-author-copy-saver
    Specifies a function to save an author copy.  The function is
    called with a file name.  The default function `rmail-output'
    saves in Unix mail format.

 gnus-kill-file-name
    Use specified file name as a KILL file (default to `KILL').

 gnus-novice-user
    Non-nil means that you are a novice to USENET.  If non-nil,
    verbose messages may be displayed or your confirmations may be
    required.

 gnus-interactive-post
    Non-nil means that newsgroup, subject and distribution are asked
    for interactively when posting a new article.

 gnus-use-full-window
    Non-nil means to take up the entire screen of Emacs.

 gnus-window-configuration
    Specifies the configuration of Group, Summary, and Article
    windows.  It is a list of (ACTION (G S A)), where G, S, and A are
    the relative height of Group, Summary, and Article windows,
    respectively.  ACTION is `summary', `newsgroups', or `article'.

 gnus-subscribe-newsgroup-method
    Specifies a function called with a newsgroup name when new
    newsgroup is found.  The default definition adds new newsgroup at
    the beginning of other newsgroups.

  And more and more.  Please refer to texinfo documentation.

Various hooks for customization:
 gnus-group-mode-hook
    Entry to this mode calls the value with no arguments, if that
    value is non-nil.  This hook is called before GNUS is connected to
    the NNTP server.  So, you can change or define the NNTP server in
    this hook.

 gnus-startup-hook
    Called with no arguments after the NNTP server is selected.  It is
    possible to change the behavior of GNUS or initialize the
    variables according to the selected NNTP server.

 gnus-group-prepare-hook
    Called with no arguments after a newsgroup list is created in the
    Newsgroup buffer, if that value is non-nil.

 gnus-save-newsrc-hook
    Called with no arguments when saving newsrc file if that value is
    non-nil.

 gnus-prepare-article-hook
    Called with no arguments after preparing message body, but before
    preparing header fields which is automatically generated if that
    value is non-nil.  The default hook (gnus-inews-insert-signature)
    inserts a signature file.

 gnus-inews-article-hook
    Called with no arguments when posting an article if that value is
    non-nil.  This hook is called just before posting an article.  The
    default hook does FCC (save an article to the specified file).

 gnus-suspend-gnus-hook
    Called with no arguments when suspending (not exiting) GNUS, if
    that value is non-nil.

 gnus-exit-gnus-hook
    Called with no arguments when exiting (not suspending) GNUS, if
    that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  ;; Gee.  Why don't you upgrade?
  (cond ((boundp 'mode-line-modified)
	 (setq mode-line-modified "--- "))
	((listp (default-value 'mode-line-format))
	 (setq mode-line-format
	       (cons "--- " (cdr (default-value 'mode-line-format)))))
	(t
	 (setq mode-line-format
	       "--- GNUS: List of Newsgroups  %[(%m)%]----%3p-%-")))
  (setq major-mode 'gnus-group-mode)
  (setq mode-name "Newsgroup")
  (setq mode-line-buffer-identification	"GNUS: List of Newsgroups")
  (setq mode-line-process nil)
  (use-local-map gnus-group-mode-map)
  (buffer-flush-undo (current-buffer))
  (setq buffer-read-only t)		;Disable modification
  (run-hooks 'gnus-group-mode-hook))

(defun gnus-mouse-pick-group (e)
  (interactive "e")
  (mouse-set-point e)
  (gnus-group-read-group nil))

;;;###autoload
(defun gnus (&optional confirm)
  "Read network news.
If optional argument CONFIRM is non-nil, ask NNTP server."
  (interactive "P")
  (unwind-protect
      (progn
	(switch-to-buffer (get-buffer-create gnus-group-buffer))
	(gnus-group-mode)
	(gnus-start-news-server confirm))
    (if (not (gnus-server-opened))
	(gnus-group-quit)
      ;; NNTP server is successfully open. 
      (setq mode-line-process (format " {%s}" gnus-nntp-server))
      (let ((buffer-read-only nil))
	(erase-buffer)
	(gnus-group-startup-message)
	(sit-for 0))
      (run-hooks 'gnus-startup-hook)
      (gnus-setup-news)
      (if gnus-novice-user
	  (gnus-group-describe-briefly)) ;Show brief help message.
      (gnus-group-list-groups nil)
      )))

(defun gnus-group-startup-message ()
  "Insert startup message in current buffer."
  ;; Insert the message.
  (insert
   (format "
                   %s

         NNTP-based News Reader for GNU Emacs


If you have any trouble with this software, please let me
know.  I will fix your problems in the next release.

Comments, suggestions, and bug fixes are welcome.

Masanobu UMEDA
umerin@mse.kyutech.ac.jp" gnus-version))
  ;; And then hack it.
  ;; 57 is the longest line.
  (indent-rigidly (point-min) (point-max) (/ (max (- (window-width) 57) 0) 2))
  (goto-char (point-min))
  ;; +4 is fuzzy factor.
  (insert-char ?\n (/ (max (- (window-height) 18) 0) 2)))

(defun gnus-group-list-groups (show-all)
  "List newsgroups in the Newsgroup buffer.
If argument SHOW-ALL is non-nil, unsubscribed groups are also listed."
  (interactive "P")
  (setq gnus-newsgroups-showall show-all)
  (let ((case-fold-search nil)
	(last-group			;Current newsgroup.
	 (gnus-group-group-name))
	(next-group			;Next possible newsgroup.
	 (progn
	   (gnus-group-search-forward nil nil)
	   (gnus-group-group-name)))
	(prev-group			;Previous possible newsgroup.
	 (progn
	   (gnus-group-search-forward t nil)
	   (gnus-group-group-name))))
    (set-buffer gnus-group-buffer)	;May call from out of Group buffer
    (gnus-group-prepare show-all)
    (if (zerop (buffer-size))
	(message "No news is good news")
      ;; Go to last newsgroup if possible.  If cannot, try next and
      ;; previous.  If all fail, go to first unread newsgroup.
      (goto-char (point-min))
      (or (and last-group
	       (re-search-forward (gnus-group-make-regexp last-group) nil t))
	  (and next-group
	       (re-search-forward (gnus-group-make-regexp next-group) nil t))
	  (and prev-group
	       (re-search-forward (gnus-group-make-regexp prev-group) nil t))
	  (gnus-group-search-forward nil nil t))
      ;; Adjust cursor point.
      (beginning-of-line)
      (search-forward ":" nil t)
      )))

(defun gnus-group-prepare (&optional all)
  "Prepare list of newsgroups in current buffer.
If optional argument ALL is non-nil, unsubscribed groups are also listed."
  (let ((buffer-read-only nil)
	(newsrc gnus-newsrc-assoc)
	(group-info nil)
	(group-name nil)
	(group-description nil)
	(unread-count 0)
	(nb-tab 0)
	;; This specifies the format of Group buffer.
	(cntl "%s%s%5d: %s"))
    (erase-buffer)
    ;; List newsgroups.
    (while newsrc
      (setq group-info (car newsrc))
      (setq group-name (car group-info))
      (if gnus-newsgroups-display
	  (progn (setq group-description (gnus-gethash group-name gnus-newsgroups-hashtb))
		 (setq nb-tab (/ (- 38 (length group-name)) tab-width))))
      (setq unread-count (nth 1 (gnus-gethash group-name gnus-unread-hashtb)))
      (if (or all
	      (and (nth 1 group-info)	;Subscribed.
		   (> unread-count 0)))	;There are unread articles.
	  ;; Yes, I can use gnus-group-prepare-line, but this is faster.
	  (insert
	   (format (concat cntl (make-string (if (> nb-tab 0) nb-tab 1) ?\t)
			   "%s\n")
		   ;; Subscribed or not.
		   (if (nth 1 group-info) " " "U")
		   ;; Has new news?
		   (if (and (> unread-count 0)
			    (>= 0
				(- unread-count
				   (length
				    (cdr (gnus-gethash group-name
						       gnus-marked-hashtb))))))
		       "*" " ")
		   ;; Number of unread articles.
		   unread-count
		   ;; Newsgroup name.
		   group-name
		   ;; Newsgroup description
		   (if group-description (cdr group-description) "")
		   ))
	)
      (setq newsrc (cdr newsrc))
      )
    (setq gnus-have-all-newsgroups all)
    (goto-char (point-min))
    (run-hooks 'gnus-group-prepare-hook)
    ))

(defun gnus-group-prepare-line (info)
  "Return a string for the Newsgroup buffer from INFO.
INFO is an element of `gnus-newsrc-assoc' or `gnus-killed-assoc'."
  (let* ((group-name (car info))
	 (group-description nil)
	 (nb-tab 0)
	 (unread-count
	  (or (nth 1 (gnus-gethash group-name gnus-unread-hashtb))
	      ;; Not in hash table, so compute it now.
	      (gnus-number-of-articles
	       (gnus-difference-of-range
		(nth 2 (gnus-gethash group-name gnus-active-hashtb))
		(nthcdr 2 info)))))
	 ;; This specifies the format of Group buffer.
	 (cntl "%s%s%5d: %s"))
    (if gnus-newsgroups-display
	(progn
	  (setq group-description (gnus-gethash group-name gnus-newsgroups-hashtb))
	  (setq nb-tab (/ (- 38 (length group-name)) tab-width))))
    (format (concat cntl (make-string (if (> nb-tab 0) nb-tab 1) ?\t)
		    "%s\n")
	    ;; Subscribed or not.
	    (if (nth 1 info) " " "U")
	    ;; Has new news?
	    (if (and (> unread-count 0)
		     (>= 0
			 (- unread-count
			    (length
			     (cdr (gnus-gethash group-name
						gnus-marked-hashtb))))))
		"*" " ")
	    ;; Number of unread articles.
	    unread-count
	    ;; Newsgroup name.
	    group-name
	    ;; Newsgroup description
	    (if group-description (cdr group-description) "")
	    )))

(defun gnus-group-update-group (group &optional visible-only)
  "Update newsgroup info of GROUP.
If optional argument VISIBLE-ONLY is non-nil, non displayed group is ignored."
  (let ((buffer-read-only nil)
	(case-fold-search nil)		;appleIIgs vs. appleiigs
	(regexp (gnus-group-make-regexp group))
	(visible nil))
    ;; Buffer may be narrowed.
    (save-restriction
      (widen)
      ;; Search a line to modify.  If the buffer is large, the search
      ;; takes long time.  In most cases, current point is on the line
      ;; we are looking for.  So, first of all, check current line. 
      ;; And then if current point is in the first half, search from
      ;; the beginning.  Otherwise, search from the end.
      (if (cond ((progn
		   (beginning-of-line)
		   (looking-at regexp)))
		((and (> (/ (buffer-size) 2) (point)) ;In the first half.
		      (progn
			(goto-char (point-min))
			(re-search-forward regexp nil t))))
		((progn
		   (goto-char (point-max))
		   (re-search-backward regexp nil t))))
	  ;; GROUP is listed in current buffer.  So, delete old line.
	  (progn
	    (setq visible t)
	    (beginning-of-line)
	    (delete-region (point) (progn (forward-line 1) (point)))
	    )
	;; No such line in the buffer, so insert it at the top.
	(goto-char (point-min)))
      (if (or visible (not visible-only))
	  (progn
	    (insert (gnus-group-prepare-line
		     (gnus-gethash group gnus-newsrc-hashtb)))
	    (forward-line -1)		;Move point on that line.
	    ))
      )))

(defun gnus-group-group-name ()
  "Get newsgroup name around point."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^..[0-9 \t]+:[ \t]+\\([^ \t\n]+\\)\\([ \t].*\\|$\\)")
        (let ((group-name (buffer-substring (match-beginning 1) (match-end 1))))
          (set-text-properties 0 (length group-name) nil group-name)
          group-name))))

(defun gnus-group-make-regexp (newsgroup)
  "Return regexp that matches for a line of NEWSGROUP."
  (concat "^.+: " (regexp-quote newsgroup) "\\([ \t].*\\|$\\)"))

(defun gnus-group-search-forward (backward norest &optional heretoo)
  "Search for the next (or previous) newsgroup.
If 1st argument BACKWARD is non-nil, search backward instead.
If 2nd argument NOREST is non-nil, don't care about newsgroup property.
If optional argument HERETOO is non-nil, current line is searched for, too."
  (let ((case-fold-search nil)
	(func
	 (if backward
	     (function re-search-backward) (function re-search-forward)))
	(regexp
	 (format "^%s[ \t]*\\(%s\\):"
		 (if norest ".." " [ \t]")
		 (if norest "[0-9]+" "[1-9][0-9]*")))
	(found nil))
    (if backward
	(if heretoo
	    (end-of-line)
	  (beginning-of-line))
      (if heretoo
	  (beginning-of-line)
	(end-of-line)))
    (setq found (funcall func regexp nil t))
    ;; Adjust cursor point.
    (beginning-of-line)
    (search-forward ":" nil t)
    ;; Return T if found.
    found
    ))

;; GNUS Group mode command

(defun gnus-group-read-group (all &optional no-article)
  "Read news in this newsgroup.
If argument ALL is non-nil, already read articles become readable.
If optional argument NO-ARTICLE is non-nil, no article body is displayed."
  (interactive "P")
  (let ((group (gnus-group-group-name))) ;Newsgroup name to read.
    (if group
	(gnus-summary-read-group
	 group
	 (or all
	     ;;(not (nth 1 (gnus-gethash group gnus-newsrc-hashtb))) ;Unsubscribed
	     (zerop
	      (nth 1 (gnus-gethash group gnus-unread-hashtb))))	;No unread
	 no-article
	 ))
    ))

(defun gnus-group-select-group (all)
  "Select this newsgroup.
No article is selected automatically.
If argument ALL is non-nil, already read articles become readable."
  (interactive "P")
  (gnus-group-read-group all t))

(defun gnus-group-jump-to-group (group)
  "Jump to newsgroup GROUP."
  (interactive
   (list (completing-read "Newsgroup: " gnus-newsrc-assoc nil 'require-match)))
  (let ((case-fold-search nil))
    (goto-char (point-min))
    (or (re-search-forward (gnus-group-make-regexp group) nil t)
	(if (gnus-gethash group gnus-newsrc-hashtb)
	    ;; Add GROUP entry, then seach again.
	    (gnus-group-update-group group)))
    ;; Adjust cursor point.
    (beginning-of-line)
    (search-forward ":" nil t)
    ))

(defun gnus-group-next-group (n)
  "Go to Nth following newsgroup."
  (interactive "p")
  (while (and (> n 1)
	      (gnus-group-search-forward nil t))
    (setq n (1- n)))
  (or (gnus-group-search-forward nil t)
      (message "No more newsgroups")))

(defun gnus-group-next-unread-group (n)
  "Go to Nth following unread newsgroup."
  (interactive "p")
  (while (and (> n 1)
	      (gnus-group-search-forward nil nil))
    (setq n (1- n)))
  (or (gnus-group-search-forward nil nil)
      (message "No more unread newsgroups")))

(defun gnus-group-prev-group (n)
  "Go to Nth previous newsgroup."
  (interactive "p")
  (while (and (> n 1)
	      (gnus-group-search-forward t t))
    (setq n (1- n)))
  (or (gnus-group-search-forward t t)
      (message "No more newsgroups")))

(defun gnus-group-prev-unread-group (n)
  "Go to Nth previous unread newsgroup."
  (interactive "p")
  (while (and (> n 1)
	      (gnus-group-search-forward t nil))	      
    (setq n (1- n)))
  (or (gnus-group-search-forward t nil)
      (message "No more unread newsgroups")))

(defun gnus-group-catchup (all)
  "Mark all articles not marked as unread in current newsgroup as read.
If prefix argument ALL is non-nil, all articles are marked as read.
Cross references (Xref: field) of articles are ignored."
  (interactive "P")
  (let* ((group (gnus-group-group-name))
         (marked (if (not all)
		     (cdr (gnus-gethash group gnus-marked-hashtb)))))
    (and group
	 (or (not gnus-interactive-catchup) ;Without confirmation?
	     (y-or-n-p
	      (if all
		  "Do you really want to mark everything as read? "
		"Delete all articles not marked as read? ")))
	 (progn
	   (message "")			;Clear "Yes or No" question.
	   ;; Any marked articles will be preserved.
	   (gnus-update-unread-articles group marked marked)
	   (gnus-group-update-group group)
	   (gnus-group-next-group 1)))
    ))

(defun gnus-group-catchup-all ()
  "Mark all articles in current newsgroup as read.
Cross references (Xref: field) of articles are ignored."
  (interactive)
  (gnus-group-catchup t))

(defun gnus-group-unsubscribe-current-group ()
  "Toggle subscribe from/to unsubscribe current group."
  (interactive)
  (let ((group (gnus-group-group-name)))
    (if group
	 (progn
	   (gnus-group-unsubscribe-group group)
	   (gnus-group-next-group 1))
      (message "No Newsgroup found to \(un\)subscribe"))))

(defun gnus-group-unsubscribe-group (group)
  "Toggle subscribe from/to unsubscribe GROUP.
\(If GROUP is new, it is added to `.newsrc' automatically.)"
  (interactive
   (list (completing-read "Newsgroup: "
			  gnus-active-hashtb nil 'require-match)))
  (let ((newsrc (gnus-gethash group gnus-newsrc-hashtb)))
    (cond ((not (null newsrc))
	   ;; Toggle subscription flag.
	   (setcar (nthcdr 1 newsrc) (not (nth 1 newsrc)))
	   (gnus-update-newsrc-buffer group)
	   (gnus-group-update-group group)
	   ;; Adjust cursor point.
	   (beginning-of-line)
	   (search-forward ":" nil t))
	  ((and (stringp group)
		(gnus-gethash group gnus-active-hashtb))
	   ;; Add new newsgroup.
	   (gnus-add-newsgroup group)
	   (gnus-group-update-group group)
	   ;; Adjust cursor point.
	   (beginning-of-line)
	   (search-forward ":" nil t))
	  (t (error "No such newsgroup: %s" group)))
    ))

(defun gnus-group-list-all-groups ()
  "List all of newsgroups in the Newsgroup buffer."
  (interactive)
  (message "Listing all groups...")
  (gnus-group-list-groups t)
  (message "Listing all groups...done"))

(defun gnus-group-get-new-news ()
  "Get newly arrived articles.  In fact, read the active file again."
  (interactive)
  (gnus-setup-news)
  (gnus-group-list-groups gnus-have-all-newsgroups))

(defun gnus-group-restart ()
  "Force GNUS to read the raw startup file."
  (interactive)
  (gnus-save-newsrc-file)
  (gnus-setup-news t)			;Force to read the raw startup file.
  (gnus-group-list-groups gnus-have-all-newsgroups))

(defun gnus-group-check-bogus-groups ()
  "Check bogus newsgroups."
  (interactive)
  (gnus-check-bogus-newsgroups t)	;Require confirmation.
  (gnus-group-list-groups gnus-have-all-newsgroups))

(defun gnus-group-restrict-groups (start end)
  "Restrict visible newsgroups to the current region (START and END).
Type \\[widen] to remove restriction."
  (interactive "r")
  (save-excursion
    (narrow-to-region (progn
			(goto-char start)
			(beginning-of-line)
			(point))
		      (progn
			(goto-char end)
			(forward-line 1)
			(point))))
  (message (substitute-command-keys "Type \\[widen] to remove restriction")))

(defun gnus-group-edit-global-kill ()
  "Edit a global KILL file."
  (interactive)
  (setq gnus-current-kill-article nil)	;No articles selected.
  (gnus-kill-file-edit-file nil) 	;Nil stands for global KILL file.
  (message
   (substitute-command-keys
    "Editing a global KILL file (Type \\[gnus-kill-file-exit] to exit)")))

(defun gnus-group-edit-local-kill ()
  "Edit a local KILL file."
  (interactive)
  (setq gnus-current-kill-article nil)	;No articles selected.
  (gnus-kill-file-edit-file (gnus-group-group-name))
  (message
   (substitute-command-keys
    "Editing a local KILL file (Type \\[gnus-kill-file-exit] to exit)")))

(defun gnus-group-force-update ()
  "Update `.newsrc' file."
  (interactive)
  (gnus-save-newsrc-file))

(defun gnus-group-suspend ()
  "Suspend the current GNUS session.
In fact, cleanup buffers except for Group Mode buffer.
The hook `gnus-suspend-gnus-hook' is called before actually suspending."
  (interactive)
  (run-hooks 'gnus-suspend-gnus-hook)
  ;; Kill GNUS buffers except for Group Mode buffer.
  (let ((buffers gnus-buffer-list)
	(group-buf (get-buffer gnus-group-buffer)))
    (while buffers
      (and (not (eq (car buffers) gnus-group-buffer))
	   (get-buffer (car buffers))
	   (kill-buffer (car buffers)))
      (setq buffers (cdr buffers))
      )
    (bury-buffer group-buf)
    (delete-windows-on group-buf t)))

(defun gnus-group-exit ()
  "Quit reading news after updating `.newsrc'.
The hook `gnus-exit-gnus-hook' is called before actually quitting."
  (interactive)
  (if (or noninteractive		;For gnus-batch-kill
	  (zerop (buffer-size))		;No news is good news.
	  (not (gnus-server-opened))	;NNTP connection closed.
	  (not gnus-interactive-exit)	;Without confirmation
	  (y-or-n-p "Are you sure you want to quit reading news? "))
      (progn
	(message "")			;Erase "Yes or No" question.
	(run-hooks 'gnus-exit-gnus-hook)
	(gnus-save-newsrc-file)
	(gnus-clear-system)
	(gnus-close-server))
    ))

(defun gnus-group-quit ()
  "Quit reading news without updating `.newsrc'.
The hook `gnus-exit-gnus-hook' is called before actually quitting."
  (interactive)
  (if (or noninteractive		;For gnus-batch-kill
	  (zerop (buffer-size))
	  (not (gnus-server-opened))
	  (yes-or-no-p
	   (format "Quit reading news without saving %s? "
		   (file-name-nondirectory gnus-current-startup-file))))
      (progn
	(message "")			;Erase "Yes or No" question.
	(run-hooks 'gnus-exit-gnus-hook)
	(gnus-clear-system)
	(gnus-close-server))
    ))

(defun gnus-group-describe-briefly ()
  "Describe Group mode commands briefly."
  (interactive)
  (message
   (concat
    (substitute-command-keys "\\[gnus-group-read-group]:Select  ")
    (substitute-command-keys "\\[gnus-group-next-unread-group]:Forward  ")
    (substitute-command-keys "\\[gnus-group-prev-unread-group]:Backward  ")
    (substitute-command-keys "\\[gnus-group-exit]:Exit  ")
    (substitute-command-keys "\\[gnus-info-find-node]:Run Info  ")
    (substitute-command-keys "\\[gnus-group-describe-briefly]:This help")
    )))


;;;
;;; GNUS Summary Mode
;;;

(if gnus-summary-mode-map
    nil
  (setq gnus-summary-mode-map (make-keymap))
  (suppress-keymap gnus-summary-mode-map)
  (define-key gnus-summary-mode-map "\C-c\C-v" 'gnus-uu-ctl-map)
  (define-key gnus-summary-mode-map "#" 'gnus-uu-mark-article)
  (define-key gnus-summary-mode-map " " 'gnus-summary-next-page)
  (define-key gnus-summary-mode-map "\177" 'gnus-summary-prev-page)
  (define-key gnus-summary-mode-map "\r" 'gnus-summary-scroll-up)
  (define-key gnus-summary-mode-map "n" 'gnus-summary-next-unread-article)
  (define-key gnus-summary-mode-map "p" 'gnus-summary-prev-unread-article)
  (define-key gnus-summary-mode-map "N" 'gnus-summary-next-article)
  (define-key gnus-summary-mode-map "P" 'gnus-summary-prev-article)
  (define-key gnus-summary-mode-map "\e\C-n" 'gnus-summary-next-same-subject)
  (define-key gnus-summary-mode-map "\e\C-p" 'gnus-summary-prev-same-subject)
  ;;(define-key gnus-summary-mode-map "\e\C-n" 'gnus-summary-next-unread-same-subject)
  ;;(define-key gnus-summary-mode-map "\e\C-p" 'gnus-summary-prev-unread-same-subject)
  (define-key gnus-summary-mode-map "\C-c\C-n" 'gnus-summary-next-digest)
  (define-key gnus-summary-mode-map "\C-c\C-p" 'gnus-summary-prev-digest)
  (define-key gnus-summary-mode-map "\C-n" 'gnus-summary-next-subject)
  (define-key gnus-summary-mode-map "\C-p" 'gnus-summary-prev-subject)
  (define-key gnus-summary-mode-map [down] 'gnus-summary-next-subject)
  (define-key gnus-summary-mode-map [up] 'gnus-summary-prev-subject)
  (define-key gnus-summary-mode-map "\en" 'gnus-summary-next-unread-subject)
  (define-key gnus-summary-mode-map "\ep" 'gnus-summary-prev-unread-subject)
  ;;(define-key gnus-summary-mode-map "\C-cn" 'gnus-summary-next-group)
  ;;(define-key gnus-summary-mode-map "\C-cp" 'gnus-summary-prev-group)
  (define-key gnus-summary-mode-map "." 'gnus-summary-first-unread-article)
  ;;(define-key gnus-summary-mode-map "/" 'isearch-forward)
  (define-key gnus-summary-mode-map "s" 'gnus-summary-isearch-article)
  (define-key gnus-summary-mode-map "\es" 'gnus-summary-search-article-forward)
  ;;(define-key gnus-summary-mode-map "\eS" 'gnus-summary-search-article-backward)
  (define-key gnus-summary-mode-map "\er" 'gnus-summary-search-article-backward)
  (define-key gnus-summary-mode-map "<" 'gnus-summary-beginning-of-article)
  (define-key gnus-summary-mode-map ">" 'gnus-summary-end-of-article)
  (define-key gnus-summary-mode-map "j" 'gnus-summary-goto-subject)
  ;;(define-key gnus-summary-mode-map "J" 'gnus-summary-goto-article)
  (define-key gnus-summary-mode-map "l" 'gnus-summary-goto-last-article)
  (define-key gnus-summary-mode-map "^" 'gnus-summary-refer-parent-article)
  ;;(define-key gnus-summary-mode-map "\er" 'gnus-summary-refer-article)
  (define-key gnus-summary-mode-map "\e^" 'gnus-summary-refer-article)
  (define-key gnus-summary-mode-map "u" 'gnus-summary-mark-as-unread-forward)
  (define-key gnus-summary-mode-map "U" 'gnus-summary-mark-as-unread-backward)
  (define-key gnus-summary-mode-map "d" 'gnus-summary-mark-as-read-forward)
  (define-key gnus-summary-mode-map "D" 'gnus-summary-mark-as-read-backward)
  (define-key gnus-summary-mode-map "\eu" 'gnus-summary-clear-mark-forward)
  (define-key gnus-summary-mode-map "\eU" 'gnus-summary-clear-mark-backward)
  (define-key gnus-summary-mode-map "k" 'gnus-summary-kill-same-subject-and-select)
  (define-key gnus-summary-mode-map "\C-k" 'gnus-summary-kill-same-subject)
  (define-key gnus-summary-mode-map "\e\C-t" 'gnus-summary-toggle-threads)
  (define-key gnus-summary-mode-map "\e\C-s" 'gnus-summary-show-thread)
  (define-key gnus-summary-mode-map "\e\C-h" 'gnus-summary-hide-thread)
  (define-key gnus-summary-mode-map "\e\C-f" 'gnus-summary-next-thread)
  (define-key gnus-summary-mode-map "\e\C-b" 'gnus-summary-prev-thread)
  (define-key gnus-summary-mode-map "\e\C-u" 'gnus-summary-up-thread)
  (define-key gnus-summary-mode-map "\e\C-d" 'gnus-summary-down-thread)
  (define-key gnus-summary-mode-map "\e\C-k" 'gnus-summary-kill-thread)
  (define-key gnus-summary-mode-map "&" 'gnus-summary-execute-command)
  ;;(define-key gnus-summary-mode-map "c" 'gnus-summary-catchup)
  ;;(define-key gnus-summary-mode-map "c" 'gnus-summary-catchup-all)
  (define-key gnus-summary-mode-map "c" 'gnus-summary-catchup-and-exit)
  ;;(define-key gnus-summary-mode-map "c" 'gnus-summary-catchup-all-and-exit)
  (define-key gnus-summary-mode-map "\C-t" 'gnus-summary-toggle-truncation)
  (define-key gnus-summary-mode-map "x" 'gnus-summary-delete-marked-as-read)
  (define-key gnus-summary-mode-map "X" 'gnus-summary-delete-marked-with)
  (define-key gnus-summary-mode-map "\C-c\C-sn" 'gnus-summary-sort-by-number)
  (define-key gnus-summary-mode-map "\C-c\C-sa" 'gnus-summary-sort-by-author)
  (define-key gnus-summary-mode-map "\C-c\C-ss" 'gnus-summary-sort-by-subject)
  (define-key gnus-summary-mode-map "\C-c\C-sd" 'gnus-summary-sort-by-date)
  (define-key gnus-summary-mode-map "\C-c\C-s\C-n" 'gnus-summary-sort-by-number)
  (define-key gnus-summary-mode-map "\C-c\C-s\C-a" 'gnus-summary-sort-by-author)
  (define-key gnus-summary-mode-map "\C-c\C-s\C-s" 'gnus-summary-sort-by-subject)
  (define-key gnus-summary-mode-map "\C-c\C-s\C-d" 'gnus-summary-sort-by-date)
  (define-key gnus-summary-mode-map "=" 'gnus-summary-expand-window)
  ;;(define-key gnus-summary-mode-map "G" 'gnus-summary-reselect-current-group)
  (define-key gnus-summary-mode-map "\C-x\C-s" 'gnus-summary-reselect-current-group)
  (define-key gnus-summary-mode-map "w" 'gnus-summary-stop-page-breaking)
  (define-key gnus-summary-mode-map "\C-c\C-r" 'gnus-summary-caesar-message)
  (define-key gnus-summary-mode-map "g" 'gnus-summary-show-article)
  (define-key gnus-summary-mode-map "t" 'gnus-summary-toggle-header)
  ;;(define-key gnus-summary-mode-map "v" 'gnus-summary-show-all-headers)
  (define-key gnus-summary-mode-map "\et" 'gnus-summary-toggle-mime)
  (define-key gnus-summary-mode-map "\C-d" 'gnus-summary-rmail-digest)
  (define-key gnus-summary-mode-map "a" 'gnus-summary-post-news)
  (define-key gnus-summary-mode-map "f" 'gnus-summary-followup)
  (define-key gnus-summary-mode-map "F" 'gnus-summary-followup-with-original)
  (define-key gnus-summary-mode-map "C" 'gnus-summary-cancel-article)
  (define-key gnus-summary-mode-map "r" 'gnus-summary-reply)
  (define-key gnus-summary-mode-map "R" 'gnus-summary-reply-with-original)
  (define-key gnus-summary-mode-map "\C-c\C-f" 'gnus-summary-mail-forward)
  (define-key gnus-summary-mode-map "m" 'gnus-summary-mail-other-window)
  (define-key gnus-summary-mode-map "o" 'gnus-summary-save-article)
  (define-key gnus-summary-mode-map "\C-o" 'gnus-summary-save-in-mail)
  (define-key gnus-summary-mode-map "|" 'gnus-summary-pipe-output)
  (define-key gnus-summary-mode-map "\ek" 'gnus-summary-edit-local-kill)
  (define-key gnus-summary-mode-map "\eK" 'gnus-summary-edit-global-kill)
  (define-key gnus-summary-mode-map "V" 'gnus-version)
  (define-key gnus-summary-mode-map "q" 'gnus-summary-exit)
  (define-key gnus-summary-mode-map "Q" 'gnus-summary-quit)
  (define-key gnus-summary-mode-map "?" 'gnus-summary-describe-briefly)
  (define-key gnus-summary-mode-map "\C-c\C-i" 'gnus-info-find-node)
  (define-key gnus-summary-mode-map [mouse-2] 'gnus-mouse-pick-article)

  (define-key gnus-summary-mode-map [menu-bar misc]
	(cons "Misc" (make-sparse-keymap "misc")))

  (define-key gnus-summary-mode-map [menu-bar misc caesar-message]
	'("Caesar Message" . gnus-summary-caesar-message))
  (define-key gnus-summary-mode-map [menu-bar misc cancel-article]
	'("Cancel Article" . gnus-summary-cancel-article))
  (define-key gnus-summary-mode-map [menu-bar misc edit-local-kill]
	'("Edit Kill File" . gnus-summary-edit-local-kill))

  (define-key gnus-summary-mode-map [menu-bar misc mark-as-unread]
	'("Mark as Unread" . gnus-summary-mark-as-unread-forward))
  (define-key gnus-summary-mode-map [menu-bar misc mark-as-read]
	'("Mark as Read" . gnus-summary-mark-as-read))

  (define-key gnus-summary-mode-map [menu-bar misc quit]
	'("Quit Group" . gnus-summary-quit))
  (define-key gnus-summary-mode-map [menu-bar misc exit]
	'("Exit Group" . gnus-summary-exit))

  (define-key gnus-summary-mode-map [menu-bar sort]
	(cons "Sort" (make-sparse-keymap "sort")))

  (define-key gnus-summary-mode-map [menu-bar sort sort-by-author]
	'("Sort by Author" . gnus-summary-sort-by-author))
  (define-key gnus-summary-mode-map [menu-bar sort sort-by-date]
	'("Sort by Date" . gnus-summary-sort-by-date))
  (define-key gnus-summary-mode-map [menu-bar sort sort-by-number]
	'("Sort by Number" . gnus-summary-sort-by-number))
  (define-key gnus-summary-mode-map [menu-bar sort sort-by-subject]
	'("Sort by Subject" . gnus-summary-sort-by-subject))

  (define-key gnus-summary-mode-map [menu-bar show/hide]
	(cons "Show/Hide" (make-sparse-keymap "show/hide")))

  (define-key gnus-summary-mode-map [menu-bar show/hide hide-all-threads]
	'("Hide All Threads" . gnus-summary-hide-all-threads))
  (define-key gnus-summary-mode-map [menu-bar show/hide hide-thread]
	'("Hide Thread" . gnus-summary-hide-thread))
  (define-key gnus-summary-mode-map [menu-bar show/hide show-all-threads]
	'("Show All Threads" . gnus-summary-show-all-threads))
  (define-key gnus-summary-mode-map [menu-bar show/hide show-all-headers]
	'("Show All Headers" . gnus-summary-show-all-headers))
  (define-key gnus-summary-mode-map [menu-bar show/hide show-thread]
	'("Show Thread" . gnus-summary-show-thread))
  (define-key gnus-summary-mode-map [menu-bar show/hide show-article]
	'("Show Article" . gnus-summary-show-article))
  (define-key gnus-summary-mode-map [menu-bar show/hide toggle-truncation]
	'("Toggle Truncation" . gnus-summary-toggle-truncation))
  (define-key gnus-summary-mode-map [menu-bar show/hide toggle-mime]
	'("Toggle Mime" . gnus-summary-toggle-mime))
  (define-key gnus-summary-mode-map [menu-bar show/hide toggle-header]
	'("Toggle Header" . gnus-summary-toggle-header))

  (define-key gnus-summary-mode-map [menu-bar action]
	(cons "Action" (make-sparse-keymap "action")))

  (define-key gnus-summary-mode-map [menu-bar action kill-same-subject]
	'("Kill Same Subject" . gnus-summary-kill-same-subject))
  (define-key gnus-summary-mode-map [menu-bar action kill-thread]
	'("Kill Thread" . gnus-summary-kill-thread))
  (define-key gnus-summary-mode-map [menu-bar action delete-marked-with]
	'("Delete Marked With" . gnus-summary-delete-marked-with))
  (define-key gnus-summary-mode-map [menu-bar action delete-marked-as-read]
	'("Delete Marked As Read" . gnus-summary-delete-marked-as-read))
  (define-key gnus-summary-mode-map [menu-bar action catchup-and-exit]
	'("Catchup And Exit" . gnus-summary-catchup-and-exit))
  (define-key gnus-summary-mode-map [menu-bar action catchup-to-here]
	'("Catchup to Here" . gnus-summary-catchup-to-here))

  (define-key gnus-summary-mode-map [menu-bar action ignore]
    '("---"))

  (define-key gnus-summary-mode-map [menu-bar action save-in-file]
	'("Save in File" . gnus-summary-save-in-file))
  (define-key gnus-summary-mode-map [menu-bar action save-article]
	'("Save Article" . gnus-summary-save-article))

  (define-key gnus-summary-mode-map [menu-bar action lambda]
    '("---"))

  (define-key gnus-summary-mode-map [menu-bar action forward]
	'("Forward" . gnus-summary-mail-forward))
  (define-key gnus-summary-mode-map [menu-bar action followup-with-original]
	'("Followup with Original" . gnus-summary-followup-with-original))
  (define-key gnus-summary-mode-map [menu-bar action followup]
	'("Followup" . gnus-summary-followup))
  (define-key gnus-summary-mode-map [menu-bar action reply-with-original]
	'("Reply with Original" . gnus-summary-reply-with-original))
  (define-key gnus-summary-mode-map [menu-bar action reply]
	'("Reply" . gnus-summary-reply))
  (define-key gnus-summary-mode-map [menu-bar action post]
	'("Post News" . gnus-summary-post-news))

  (define-key gnus-summary-mode-map [menu-bar move]
	(cons "Move" (make-sparse-keymap "move")))

  (define-key gnus-summary-mode-map [menu-bar move isearch-article]
	'("Search in Article" . gnus-summary-isearch-article))
  (define-key gnus-summary-mode-map [menu-bar move search-through-articles]
	'("Search through Articles" . gnus-summary-search-article-forward))
  (define-key gnus-summary-mode-map [menu-bar move down-thread]
	'("Down Thread" . gnus-summary-down-thread))
  (define-key gnus-summary-mode-map [menu-bar move prev-same-subject]
	'("Prev Same Subject" . gnus-summary-prev-same-subject))
  (define-key gnus-summary-mode-map [menu-bar move prev-group]
	'("Prev Group" . gnus-summary-prev-group))
  (define-key gnus-summary-mode-map [menu-bar move next-unread-same-subject]
	'("Next Unread Same Subject" . gnus-summary-next-unread-same-subject))
  (define-key gnus-summary-mode-map [menu-bar move next-unread-article]
	'("Next Unread Article" . gnus-summary-next-unread-article))
  (define-key gnus-summary-mode-map [menu-bar move next-thread]
	'("Next Thread" . gnus-summary-next-thread))
  (define-key gnus-summary-mode-map [menu-bar move next-group]
	'("Next Group" . gnus-summary-next-group))
  (define-key gnus-summary-mode-map [menu-bar move first-unread-article]
	'("First Unread Article" . gnus-summary-first-unread-article))
  )


(defun gnus-summary-mode ()
  "Major mode for reading articles in this newsgroup.
All normal editing commands are turned off.
Instead, these commands are available:

SPC	Scroll to the next page of the current article.  The next unread
	article is selected automatically at the end of the message.
DEL	Scroll to the previous page of the current article.
RET	Scroll up (or down) one line the current article.
n	Move to the next unread article.
p	Move to the previous unread article.
N	Move to the next article.
P	Move to the previous article.
ESC C-n	Move to the next article which has the same subject as the
	current article.
ESC C-p	Move to the previous article which has the same subject as the
	current article.
\\[gnus-summary-next-unread-same-subject]
	Move to the next unread article which has the same subject as the
	current article.
\\[gnus-summary-prev-unread-same-subject]
	Move to the previous unread article which has the same subject as
	the current article.
C-c C-n	Scroll to the next digested message of the current article.
C-c C-p	Scroll to the previous digested message of the current article.
C-n	Move to the next subject.
C-p	Move to the previous subject.
ESC n	Move to the next unread subject.
ESC p	Move to the previous unread subject.
\\[gnus-summary-next-group]
	Exit the current newsgroup and select the next unread newsgroup.
\\[gnus-summary-prev-group]
	Exit the current newsgroup and select the previous unread newsgroup.
.	Jump to the first unread article in the current newsgroup.
s	Do an incremental search forward on the current article.
ESC s	Search for an article containing a regexp forward.
ESC r	Search for an article containing a regexp backward.
<	Move point to the beginning of the current article.
>	Move point to the end of the current article.
j	Jump to the article specified by the numeric article ID.
l	Jump to the article you read last.
^	Refer to parent of the current article.
ESC ^	Refer to the article specified by the Message-ID.
u	Mark the current article as unread, and go forward.
U	Mark the current article as unread, and go backward.
d	Mark the current article as read, and go forward.
D	Mark the current article as read, and go backward.
ESC u	Clear the current article's mark, and go forward.
ESC U	Clear the current article's mark, and go backward.
k	Mark articles which has the same subject as the current article as
	read, and then select the next unread article.
C-k	Mark articles which has the same subject as the current article as
	read.
ESC k	Edit a local KILL file applied to the current newsgroup.
ESC K	Edit a global KILL file applied to all newsgroups.
ESC C-t	Toggle showing conversation threads.
ESC C-s	Show thread subtrees.
ESC C-h	Hide thread subtrees.
\\[gnus-summary-show-all-threads]	Show all thread subtrees.
\\[gnus-summary-hide-all-threads]	Hide all thread subtrees.
ESC C-f	Go to the same level next thread.
ESC C-b	Go to the same level previous thread.
ESC C-d	Go downward current thread.
ESC C-u	Go upward current thread.
ESC C-k	Mark articles under current thread as read.
&	Execute a command for each article conditionally.
\\[gnus-summary-catchup]
	Mark all articles as read in the current newsgroup, preserving
	articles marked as unread.
\\[gnus-summary-catchup-all]
	Mark all articles as read in the current newsgroup.
\\[gnus-summary-catchup-and-exit]
	Catch up all articles not marked as unread, and then exit the
	current newsgroup.
\\[gnus-summary-catchup-all-and-exit]
	Catch up all articles, and then exit the current newsgroup.
C-t	Toggle truncations of subject lines.
x	Delete subject lines marked as read.
X	Delete subject lines with the specific marks.
C-c C-s C-n	Sort subjects by article number.
C-c C-s C-a	Sort subjects by article author.
C-c C-s C-s	Sort subjects alphabetically.
C-c C-s C-d	Sort subjects by date.
=	Expand Summary window to show headers full window.
C-x C-s	Reselect the current newsgroup.  Prefix argument means to select all.
w	Stop page breaking by linefeed.
C-c C-r	Caesar rotates letters by 13/47 places.
g	Force to show the current article.
t	Show original article header if pruned header currently shown, or
	vice versa.
ESC-t	Toggle MIME processing.
C-d	Run RMAIL on the current digest article.
a	Post a new article.
f	Post a reply article.
F	Post a reply article with original article.
C	Cancel the current article.
r	Mail a message to the author.
R	Mail a message to the author with original author.
C-c C-f	Forward the current message to another user.
m	Mail a message in other window.
o	Save the current article in your favorite format.
C-o	Append the current article to a file in Unix mail format.
|	Pipe the contents of the current article to a subprocess.
q	Quit reading news in the current newsgroup.
Q	Quit reading news without recording unread articles information.
V	Show the version number of this GNUS.
?	Describe Summary mode commands briefly.
C-h m	Describe Summary mode.
C-c C-i	Read Info about Summary mode.

User customizable variables:
 gnus-large-newsgroup
    The number of articles which indicates a large newsgroup.  If the
    number of articles in a newsgroup is greater than the value, the
    number of articles to be selected is asked for.  If the given value
    N is positive, the last N articles is selected.  If N is negative,
    the first N articles are selected.  An empty string means to select
    all articles.

 gnus-use-long-file-name
    Non-nil means that a newsgroup name is used as a default file name
    to save articles to.  If it's nil, the directory form of a
    newsgroup is used instead.

 gnus-default-article-saver
    Specifies your favorite article saver which is interactively
    funcallable.  Following functions are available:

	gnus-summary-save-in-rmail (in Rmail format)
	gnus-summary-save-in-mail (in Unix mail format)
	gnus-summary-save-in-folder (in MH folder)
	gnus-summary-save-in-file (in article format).

 gnus-rmail-save-name
 gnus-mail-save-name
 gnus-folder-save-name
 gnus-file-save-name
    Specifies a function generating a file name to save articles in
    specified format.  The function is called with NEWSGROUP, HEADERS,
    and optional LAST-FILE.  Access macros to the headers are defined
    as `nntp-header-FIELD', and functions are defined as
    `gnus-header-FIELD'.

 gnus-article-save-directory
    Specifies a directory name to save articles to using the commands
    `gnus-summary-save-in-rmail', `gnus-summary-save-in-mail' and
    `gnus-summary-save-in-file'.  The variable is initialized from the
    SAVEDIR environment variable.

 gnus-kill-files-directory
    Specifies a directory name to save KILL files to using the commands
    `gnus-edit-global-kill', and `gnus-edit-local-kill'.  The variable is
    initialized from the SAVEDIR environment variable.

 gnus-show-all-headers
    Non-nil means that all headers of an article are shown.

 gnus-save-all-headers
    Non-nil means that all headers of an article are saved in a file.

 gnus-show-mime
    Non-nil means that show a MIME message.

 gnus-show-threads
    Non-nil means that conversation threads are shown in tree structure.

 gnus-thread-hide-subject
    Non-nil means that subjects for thread subtrees are hidden.

 gnus-thread-hide-subtree
    Non-nil means that thread subtrees are hidden initially.

 gnus-thread-hide-killed
    Non-nil means that killed thread subtrees are hidden automatically.

 gnus-thread-ignore-subject
    Non-nil means that subject differences are ignored in constructing
    thread trees.

 gnus-thread-indent-level
    Indentation of thread subtrees.

 gnus-optional-headers
    Specifies a function which generates an optional string displayed
    in the Summary buffer.  The function is called with an article
    HEADERS.  The result must be a string excluding `[' and `]'.  The
    default function returns a string like NNN:AUTHOR, where NNN is
    the number of lines in an article and AUTHOR is the name of the
    author.

 gnus-auto-extend-newsgroup
    Non-nil means visible articles are extended to forward and
    backward automatically if possible.

 gnus-auto-select-first
    Non-nil means the first unread article is selected automagically
    when a newsgroup is selected normally (by `gnus-group-read-group').
    If you'd like to prevent automatic selection of the first unread
    article in some newsgroups, set the variable to nil in
    `gnus-select-group-hook' or `gnus-apply-kill-hook'.

 gnus-auto-select-next
    Non-nil means the next newsgroup is selected automagically at the
    end of the newsgroup.  If the value is t and the next newsgroup is
    empty (no unread articles), GNUS will exit Summary mode and go
    back to Group mode.  If the value is neither nil nor t, GNUS won't
    exit Summary mode but select the following unread newsgroup.
    Especially, if the value is the symbol `quietly', the next unread
    newsgroup will be selected without any confirmations.

 gnus-auto-select-same
    Non-nil means an article with the same subject as the current
    article is selected automagically like `rn -S'.

 gnus-auto-center-summary
    Non-nil means the point of Summary Mode window is always kept
    centered.

 gnus-break-pages
    Non-nil means an article is broken into pages at page delimiters.
    This may not work with some versions of GNU Emacs earlier than
    version 18.50.

 gnus-page-delimiter
    Specifies a regexp describing line-beginnings that separate pages
    of news article.

 gnus-digest-show-summary
    Non-nil means that a summary of digest messages is shown when
    reading a digest article using `gnus-summary-rmail-digest'
    command.

 gnus-digest-separator
    Specifies a regexp separating messages in a digest article.

 gnus-mail-reply-method
 gnus-mail-other-window-method
    Specifies a function to begin composing mail message using
    commands `gnus-summary-reply' and `gnus-summary-mail-other-window'.
    Functions `gnus-mail-reply-using-mail' and `gnus-mail-reply-using-mhe'
    are available for the value of `gnus-mail-reply-method'.  And
    functions `gnus-mail-other-window-using-mail' and
    `gnus-mail-other-window-using-mhe' are available for the value of
    `gnus-mail-other-window-method'.

 gnus-mail-send-method
    Specifies a function to mail a message too which is being posted
    as an article.  The message must have To: or Cc: field.  The value
    of the variable `send-mail-function' is the default function, which
    uses sendmail mail program.

Various hooks for customization:
 gnus-summary-mode-hook
    Entry to this mode calls the value with no arguments, if that
    value is non-nil.

 gnus-select-group-hook
    Called with no arguments when newsgroup is selected, if that value
    is non-nil.  It is possible to sort subjects in this hook.  See the
    documentation of this variable for more information.

 gnus-summary-prepare-hook
    Called with no arguments after a summary list is created in the
    Summary buffer, if that value is non-nil.  If you'd like to modify
    the buffer, you can use this hook.

 gnus-select-article-hook
    Called with no arguments when an article is selected, if that
    value is non-nil.  See the documentation of this variable for more
    information.

 gnus-select-digest-hook
    Called with no arguments when reading digest messages using Rmail,
    if that value is non-nil.  This hook can be used to modify an
    article so that Rmail can work with it.  See the documentation of
    the variable for more information.

 gnus-rmail-digest-hook
    Called with no arguments when reading digest messages using Rmail,
    if that value is non-nil.  This hook is intended to customize Rmail
    mode.

 gnus-apply-kill-hook
    Called with no arguments when a newsgroup is selected and the
    Summary buffer is prepared.  This hook is intended to apply a KILL
    file to the selected newsgroup.  The format of KILL file is
    completely different from that of version 3.8.  You have to rewrite
    them in the new format.  See the documentation of Kill file mode
    for more information.

 gnus-mark-article-hook
    Called with no arguments when an article is selected at the first
    time.  The hook is intended to mark an article as read (or unread)
    automatically when it is selected.  See the documentation of the
    variable for more information.

 gnus-exit-group-hook
    Called with no arguments when exiting the current newsgroup, if
    that value is non-nil.  If your machine is so slow that exiting
    from Summary mode takes very long time, inhibit marking articles
    as read using cross-references by setting the variable
    gnus-use-cross-reference to nil in this hook."
  (interactive)
  (kill-all-local-variables)
  ;; Gee.  Why don't you upgrade?
  (cond ((boundp 'mode-line-modified)
	 (setq mode-line-modified "--- "))
	((listp (default-value 'mode-line-format))
	 (setq mode-line-format
	       (cons "--- " (cdr (default-value 'mode-line-format))))))
  ;; To disable display-time facility.
  ;;(make-local-variable 'global-mode-string)
  ;;(setq global-mode-string nil)
  (setq major-mode 'gnus-summary-mode)
  (setq mode-name "Summary")
  ;;(setq mode-line-process '(" " gnus-newsgroup-name))
  (make-local-variable 'minor-mode-alist)
  (or (assq 'gnus-show-threads minor-mode-alist)
      (setq minor-mode-alist
	    (cons (list 'gnus-show-threads " Thread") minor-mode-alist)))
  (gnus-summary-set-mode-line)
  (use-local-map gnus-summary-mode-map)
  (buffer-flush-undo (current-buffer))
  (setq buffer-read-only t)		;Disable modification
  (setq truncate-lines t)		;Stop line folding
  (setq selective-display t)
  (setq selective-display-ellipses t)	;Display `...'
  ;;(setq case-fold-search t)
  (run-hooks 'gnus-summary-mode-hook))

(defun gnus-mouse-pick-article (e)
  (interactive "e")
  (mouse-set-point e)
  (gnus-summary-next-page nil))

(defun gnus-summary-setup-buffer ()
  "Initialize Summary buffer."
  (if (get-buffer gnus-summary-buffer)
      (set-buffer gnus-summary-buffer)
    (set-buffer (get-buffer-create gnus-summary-buffer))
    (gnus-summary-mode)
    ))

(defun gnus-summary-read-group (group &optional show-all no-article)
  "Start reading news in newsgroup GROUP.
If optional 1st argument SHOW-ALL is non-nil, already read articles are
also listed.
If optional 2nd argument NO-ARTICLE is non-nil, no article is selected
initially."
  (message "Retrieving newsgroup: %s..." group)
  (if (gnus-select-newsgroup group show-all)
      (progn
	;; Don't switch-to-buffer to prevent displaying old contents
	;;  of the buffer until new subjects list is created.
	;; Suggested by Juha Heinanen <jh@tut.fi>
	(gnus-summary-setup-buffer)
	;; You can change the order of subjects in this hook.
	(run-hooks 'gnus-select-group-hook)
	(gnus-summary-prepare)
	;; Function `gnus-apply-kill-file' must be called in this hook.
	(run-hooks 'gnus-apply-kill-hook)
	(if (zerop (buffer-size))
	    ;; This newsgroup is empty.
	    (progn
	      (gnus-summary-catchup-and-exit nil t) ;Without confirmations.
	      (message "No unread news"))
	  ;; Hide conversation thread subtrees.  We cannot do this in
	  ;; gnus-summary-prepare-hook since kill processing may not
	  ;; work with hidden articles.
	  (and gnus-show-threads
	       gnus-thread-hide-subtree
	       (gnus-summary-hide-all-threads))
	  ;; Show first unread article if requested.
	  (goto-char (point-min))
	  (if (and (not no-article)
		   gnus-auto-select-first
		   (gnus-summary-first-unread-article))
	      ;; Window is configured automatically.
	      ;; Current buffer may be changed as a result of hook
	      ;; evaluation, especially by gnus-summary-rmail-digest
	      ;; command, so we should adjust cursor point carefully.
	      (if (eq (current-buffer) (get-buffer gnus-summary-buffer))
		  (progn
		    ;; Adjust cursor point.
		    (beginning-of-line)
		    (search-forward ":" nil t)))
	    (gnus-configure-windows 'summary)
	    (pop-to-buffer gnus-summary-buffer)
	    (gnus-summary-set-mode-line)
	    ;; I sometime get confused with the old Article buffer.
	    (if (get-buffer gnus-article-buffer)
		(if (get-buffer-window gnus-article-buffer)
		    (save-excursion
		      (set-buffer gnus-article-buffer)
		      (let ((buffer-read-only nil))
			(erase-buffer)))
		  (kill-buffer gnus-article-buffer)))
	    ;; Adjust cursor point.
	    (beginning-of-line)
	    (search-forward ":" nil t))
	  ))
    ;; Cannot select newsgroup GROUP.
    (if (gnus-gethash group gnus-active-hashtb)
	(progn
	  ;; If NNTP is used, nntp_access file may not be installed
	  ;; properly.  Otherwise, may be active file problem.
	  (ding)
	  (message
	   (gnus-nntp-message
	    (format "Cannot select %s.  May be security or active file problem." group)))
	  (sit-for 0))
      ;; Check bogus newsgroups.
      ;; We must be in Group Mode buffer.
      (gnus-group-check-bogus-groups))
    ))

(defun gnus-summary-prepare ()
  "Prepare summary list of current newsgroup in Summary buffer."
  (let ((buffer-read-only nil))
    ;; Note: The next codes are not actually used because the user who
    ;; want it can define them in gnus-select-group-hook.
    ;; Print verbose messages if too many articles are selected.
    ;;    (and (numberp gnus-large-newsgroup)
    ;;       (> (length gnus-newsgroup-headers) gnus-large-newsgroup)
    ;;       (message "Preparing headers..."))
    (erase-buffer)
    (gnus-summary-prepare-threads
     (if gnus-show-threads
	 (gnus-make-threads gnus-newsgroup-headers)
       gnus-newsgroup-headers) 0)
    ;; Erase header retrieval message.
    (message "")
    ;; Call hooks for modifying Summary buffer.
    ;; Suggested by sven@tde.LTH.Se (Sven Mattisson).
    (goto-char (point-min))
    (run-hooks 'gnus-summary-prepare-hook)
    ))

;; Basic ideas by Paul Dworkin <paul@media-lab.media.mit.edu>
;; Subject bug fix by jbw@bigbird.bu.edu (Joe Wells)

(defun gnus-summary-prepare-threads (threads level &optional parent-subject)
  "Prepare Summary buffer from THREADS and indentation LEVEL.
THREADS is a list of `(PARENT [(CHILD1 [(GRANDCHILD ...]...) ...]).'
Optional PARENT-SUBJECT specifies the subject of the parent."
  (let ((thread nil)
	(header nil)
	(number nil)
	(subject nil)
	(child-subject nil)
	(parent-subject (or parent-subject ""))
	;; `M Indent NUM: [OPT] SUBJECT'
	(cntl (format "%%s %%s%%%dd: [%%s] %%s\n"
		      (length (prin1-to-string gnus-newsgroup-end)))))
    (while threads
      (setq thread (car threads))
      (setq threads (cdr threads))
      ;; If thread is a cons, hierarchical threads is given.
      ;; Otherwise, thread itself is header.
      (if (consp thread)
	  (setq header (car thread))
	(setq header thread))
      ;; Print valid header only.
      (if (vectorp header)		;Depends on nntp.el.
	  (progn
	    (setq number (nntp-header-number header))
	    (setq subject (nntp-header-subject header))
	    (setq child-subject (gnus-simplify-subject subject 're-only))
	    (insert
	     (format cntl
		     ;; Read or not.
		     (cond ((memq number gnus-newsgroup-marked)  "-")
			   ((memq number gnus-newsgroup-unreads) " ")
			   (t "D"))
		     ;; Thread level.
		     (make-string (* level gnus-thread-indent-level) ? )
		     ;; Article number.
		     number
		     ;; Optional headers.
		     (or (and gnus-optional-headers
			      (funcall gnus-optional-headers header)) "")
		     ;; Its subject string.
		     (concat (if (or (zerop level)
				     (not gnus-thread-hide-subject)
				     ;; Subject is different from the parent.
				     (not (string-equal
					   parent-subject child-subject)))
				 nil
			       (make-string (window-width) ? ))
			     subject)
		     ))
	    ))
      ;; Print subthreads.
      (and (consp thread)
	   (cdr thread)
	   (gnus-summary-prepare-threads
	    (cdr thread) (1+ level) child-subject))
      )))

;;(defun gnus-summary-set-mode-line ()
;;  "Set Summary mode line string."
;;  ;; The value must be a string to escape %-constructs.
;;  (let ((subject
;;	 (if gnus-current-headers
;;	     (nntp-header-subject gnus-current-headers) gnus-newsgroup-name)))
;;    (setq mode-line-buffer-identification
;;	  (concat "GNUS: "
;;		  subject
;;		  ;; Enough spaces to pad subject to 17 positions.
;;		  (make-string (max 0 (- 17 (length subject))) ? ))))
;;  (set-buffer-modified-p t))

;; New implementation in gnus 3.14.3

(defun gnus-summary-set-mode-line ()
  "Set Summary mode line string.
If you don't like it, define your own `gnus-summary-set-mode-line'."
  (let ((unmarked
	 (- (length gnus-newsgroup-unreads)
	    (length (gnus-intersection
		     gnus-newsgroup-unreads gnus-newsgroup-marked))))
	(unselected
	 (- (length gnus-newsgroup-unselected)
	    (length (gnus-intersection
		     gnus-newsgroup-unselected gnus-newsgroup-marked)))))
    (setq mode-line-buffer-identification
	  (list 17
		(format "GNUS: %s%s %s"
			gnus-newsgroup-name
			(if gnus-current-article
			    (format "/%d" gnus-current-article) "")
			;; Basic ideas by tale@pawl.rpi.edu.
			(cond ((and (zerop unmarked)
				    (zerop unselected))
			       "")
			      ((zerop unselected)
			       (format "{%d more}" unmarked))
			      (t
			       (format "{%d(+%d) more}" unmarked unselected)))
			))))
  (set-buffer-modified-p t))

;; GNUS Summary mode command.

(defun gnus-summary-search-group (&optional backward)
  "Search for next unread newsgroup.
If optional argument BACKWARD is non-nil, search backward instead."
  (save-excursion
    (set-buffer gnus-group-buffer)
    (save-excursion
      ;; We don't want to alter current point of Group mode buffer.
      (if (gnus-group-search-forward backward nil)
	  (gnus-group-group-name))
      )))

(defun gnus-summary-search-subject (backward unread subject)
  "Search for article forward.
If 1st argument BACKWARD is non-nil, search backward.
If 2nd argument UNREAD is non-nil, only unread article is selected.
If 3rd argument SUBJECT is non-nil, the article which has
the same subject will be searched for."
  (let ((func
	 (if backward
	     (function re-search-backward) (function re-search-forward)))
	(article nil)
	;; We have to take care of hidden lines.
	(regexp 
	 (format "^%s[ \t]+\\([0-9]+\\):.\\[[^]\r\n]*\\][ \t]+%s"
		 ;;(if unread " " ".")
		 (cond ((eq unread t) " ") (unread "[- ]") (t "."))
		 (if subject
		     (concat "\\([Rr][Ee]:[ \t]+\\)*"
			     (regexp-quote (gnus-simplify-subject subject))
			     ;; Ignore words in parentheses.
			     "\\([ \t]*([^\r\n]*)\\)*[ \t]*\\(\r\\|$\\)")
		   "")
		 )))
    (if backward
	(beginning-of-line)
      (end-of-line))
    (if (funcall func regexp nil t)
	(setq article
	      (string-to-int
	       (buffer-substring (match-beginning 1) (match-end 1)))))
    ;; Adjust cursor point.
    (beginning-of-line)
    (search-forward ":" nil t)
    ;; This is the result.
    article
    ))

(defun gnus-summary-search-forward (&optional unread subject)
  "Search for article forward.
If 1st optional argument UNREAD is non-nil, only unread article is selected.
If 2nd optional argument SUBJECT is non-nil, the article which has
the same subject will be searched for."
  (gnus-summary-search-subject nil unread subject))

(defun gnus-summary-search-backward (&optional unread subject)
  "Search for article backward.
If 1st optional argument UNREAD is non-nil, only unread article is selected.
If 2nd optional argument SUBJECT is non-nil, the article which has
the same subject will be searched for."
  (gnus-summary-search-subject t unread subject))

(defun gnus-summary-article-number ()
  "Return the Article number around point.
If none, return current article number."
  (save-excursion
    (beginning-of-line)
    (if (looking-at ".[ \t]+\\([0-9]+\\):")
	(string-to-int
	 (buffer-substring (match-beginning 1) (match-end 1)))
      ;; If search fail, return current article number.
      gnus-current-article
      )))

(defun gnus-summary-subject-string ()
  "Return current subject string or nil if nothing."
  (save-excursion
    ;; It is possible to implement this function using
    ;;  `gnus-summary-article-number' and `gnus-newsgroup-headers'.
    (beginning-of-line)
    ;; We have to take care of hidden lines.
    (if (looking-at ".[ \t]+[0-9]+:.\\[[^]\r\n]*\\][ \t]+\\([^\r\n]*\\)[\r\n]")
	(buffer-substring (match-beginning 1) (match-end 1)))
    ))

(defun gnus-summary-goto-subject (article)
  "Move point to ARTICLE's subject."
  (interactive
   (list
    (string-to-int
     (completing-read "Article number: "
		      (mapcar
		       (function
			(lambda (headers)
			  (list
			   (int-to-string (nntp-header-number headers)))))
		       gnus-newsgroup-headers)
		      nil 'require-match))))
  (let ((current (point)))
    (goto-char (point-min))
    (or (and article (re-search-forward (format "^.[ \t]+%d:" article) nil t))
	(progn (goto-char current) nil))
    ))

(defun gnus-summary-recenter ()
  "Center point in Summary window."
  ;; Scroll window so as to cursor comes center of Summary window
  ;;  only when article is displayed.
  ;; Suggested by earle@mahendo.JPL.NASA.GOV (Greg Earle).
  ;; Recenter only when requested.
  ;; Subbested by popovich@park.cs.columbia.edu
  (and gnus-auto-center-summary
       (get-buffer-window gnus-article-buffer)
       (< (/ (- (window-height) 1) 2)
	  (count-lines (point) (point-max)))
       (recenter (/ (- (window-height) 2) 2))))

;; Walking around Group mode buffer.

(defun gnus-summary-jump-to-group (newsgroup)
  "Move point to NEWSGROUP in Group mode buffer."
  ;; Keep update point of Group mode buffer if visible.
  (if (eq (current-buffer)
	  (get-buffer gnus-group-buffer))
      (save-window-excursion
	;; Take care of tree window mode.
	(if (get-buffer-window gnus-group-buffer)
	    (pop-to-buffer gnus-group-buffer))
	(gnus-group-jump-to-group newsgroup))
    (save-excursion
      ;; Take care of tree window mode.
      (if (get-buffer-window gnus-group-buffer)
	  (pop-to-buffer gnus-group-buffer)
	(set-buffer gnus-group-buffer))
      (gnus-group-jump-to-group newsgroup))))

(defun gnus-summary-next-group (no-article)
  "Exit current newsgroup and then select next unread newsgroup.
If prefix argument NO-ARTICLE is non-nil, no article is selected initially."
  (interactive "P")
  ;; Make sure Group mode buffer point is on current newsgroup.
  (gnus-summary-jump-to-group gnus-newsgroup-name)
  (let ((group (gnus-summary-search-group)))
    (if (null group)
	(progn
	  (message "Exiting %s..." gnus-newsgroup-name)  
	  (gnus-summary-exit)
	  (message ""))
      (message "Selecting %s..." group)
      (gnus-summary-exit t)		;Exit Summary mode temporary.
      ;; We are now in Group mode buffer.
      ;; Make sure Group mode buffer point is on GROUP.
      (gnus-summary-jump-to-group group)
      (gnus-summary-read-group group nil no-article)
      (or (eq (current-buffer)
	      (get-buffer gnus-summary-buffer))
	  (eq gnus-auto-select-next t)
	  ;; Expected newsgroup has nothing to read since the articles
	  ;; are marked as read by cross-referencing.  So, try next
	  ;; newsgroup.  (Make sure we are in Group mode buffer now.)
	  (and (eq (current-buffer)
		   (get-buffer gnus-group-buffer))
	       (gnus-group-group-name)
	       (gnus-summary-read-group
		(gnus-group-group-name) nil no-article))
	  )
      )))

(defun gnus-summary-prev-group (no-article)
  "Exit current newsgroup and then select previous unread newsgroup.
If prefix argument NO-ARTICLE is non-nil, no article is selected initially."
  (interactive "P")
  ;; Make sure Group mode buffer point is on current newsgroup.
  (gnus-summary-jump-to-group gnus-newsgroup-name)
  (let ((group (gnus-summary-search-group t)))
    (if (null group)
	(progn
	  (message "Exiting %s..." gnus-newsgroup-name)  
	  (gnus-summary-exit)
	  (message ""))
      (message "Selecting %s..." group)
      (gnus-summary-exit t)		;Exit Summary mode temporary.
      ;; We are now in Group mode buffer.
      ;; We have to adjust point of Group mode buffer because current
      ;; point is moved to next unread newsgroup by exiting.
      (gnus-summary-jump-to-group group)
      (gnus-summary-read-group group nil no-article)
      (or (eq (current-buffer)
	      (get-buffer gnus-summary-buffer))
	  (eq gnus-auto-select-next t)
	  ;; Expected newsgroup has nothing to read since the articles
	  ;; are marked as read by cross-referencing.  So, try next
	  ;; newsgroup.  (Make sure we are in Group mode buffer now.)
	  (and (eq (current-buffer)
		   (get-buffer gnus-group-buffer))
	       (gnus-summary-search-group t)
	       (gnus-summary-read-group
		(gnus-summary-search-group t) nil no-article))
	  )
      )))

;; Walking around summary lines.

(defun gnus-summary-next-subject (n &optional unread)
  "Go to Nth following summary line.
If optional argument UNREAD is non-nil, only unread article is selected."
  (interactive "p")
  (while (and (> n 1)
	      (gnus-summary-search-forward unread))
    (setq n (1- n)))
  (cond ((gnus-summary-search-forward unread)
	 (gnus-summary-recenter))
	(unread
	 (message "No more unread articles"))
	(t
	 (message "No more articles"))
	))

(defun gnus-summary-next-unread-subject (n)
  "Go to Nth following unread summary line."
  (interactive "p")
  (gnus-summary-next-subject n t))

(defun gnus-summary-prev-subject (n &optional unread)
  "Go to Nth previous summary line.
If optional argument UNREAD is non-nil, only unread article is selected."
  (interactive "p")
  (while (and (> n 1)
	      (gnus-summary-search-backward unread))
    (setq n (1- n)))
  (cond ((gnus-summary-search-backward unread)
	 (gnus-summary-recenter))
	(unread
	 (message "No more unread articles"))
	(t
	 (message "No more articles"))
	))

(defun gnus-summary-prev-unread-subject (n)
  "Go to Nth previous unread summary line."
  (interactive "p")
  (gnus-summary-prev-subject n t))

;; Walking around summary lines with displaying articles.

(defun gnus-summary-expand-window ()
  "Expand Summary window to show headers full window."
  (interactive)
  (gnus-configure-windows 'summary)
  (pop-to-buffer gnus-summary-buffer))

(defun gnus-summary-display-article (article &optional all-header)
  "Display ARTICLE in Article buffer."
  (if (null article)
      nil
    (gnus-configure-windows 'article)
    (pop-to-buffer gnus-summary-buffer)
    (gnus-article-prepare article all-header)
    (gnus-summary-recenter)
    (gnus-summary-set-mode-line)
    (run-hooks 'gnus-select-article-hook)
    ;; Successfully display article.
    t
    ))

(defun gnus-summary-select-article (&optional all-headers force)
  "Select the current article.
Optional first argument ALL-HEADERS is non-nil, show all header fields.
Optional second argument FORCE is nil, the article is only selected
again when current header does not match with ALL-HEADERS option."
  (let ((article (gnus-summary-article-number))
	(all-headers (not (not all-headers)))) ;Must be T or NIL.
    (if (or (null gnus-current-article)
	    (/= article gnus-current-article)
	    (and force (not (eq all-headers gnus-have-all-headers))))
	;; The selected one is different from that of the current article.
	(gnus-summary-display-article article all-headers)
      (gnus-configure-windows 'article)
      (pop-to-buffer gnus-summary-buffer))
    ))

(defun gnus-summary-set-current-mark (&optional current-mark)
  "Put `+' at the current article.
Optional argument specifies CURRENT-MARK instead of `+'."
  (save-excursion
    (set-buffer gnus-summary-buffer)
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      ;; First of all clear mark at last article.
      (if (re-search-forward "^.[ \t]+[0-9]+:[^ \t]" nil t)
	  (progn
	    (delete-char -1)
	    (insert " ")
	    (goto-char (point-min))))
      (if (re-search-forward (format "^.[ \t]+%d:" gnus-current-article) nil t)
	  (progn
	    (delete-char 1)
	    (insert (or current-mark "+"))))
      )))

;;(defun gnus-summary-next-article (unread &optional subject)
;;  "Select article after current one.
;;If argument UNREAD is non-nil, only unread article is selected."
;;  (interactive "P")
;;  (cond ((gnus-summary-display-article
;;	  (gnus-summary-search-forward unread subject)))
;;	(unread
;;	 (message "No more unread articles"))
;;	(t
;;	 (message "No more articles"))
;;	))

(defun gnus-summary-next-article (unread &optional subject)
  "Select article after current one.
If argument UNREAD is non-nil, only unread article is selected."
  (interactive "P")
  (let ((header nil))
    (cond ((gnus-summary-display-article
	    (gnus-summary-search-forward unread subject)))
	  ((and subject
		gnus-auto-select-same
		(gnus-set-difference gnus-newsgroup-unreads
				     gnus-newsgroup-marked)
		(memq this-command
		      '(gnus-summary-next-unread-article
			gnus-summary-next-page
			gnus-summary-kill-same-subject-and-select
			;;gnus-summary-next-article
			;;gnus-summary-next-same-subject
			;;gnus-summary-next-unread-same-subject
			)))
	   ;; Wrap article pointer if there are unread articles.
	   ;; Hook function, such as gnus-summary-rmail-digest, may
	   ;; change current buffer, so need check.
	   (let ((buffer (current-buffer))
		 (last-point (point)))
	     ;; No more articles with same subject, so jump to the first
	     ;; unread article.
	     (gnus-summary-first-unread-article)
	     ;;(and (eq buffer (current-buffer))
	     ;;	(= (point) last-point)
	     ;;	;; Ignore given SUBJECT, and try again.
	     ;;	(gnus-summary-next-article unread nil))
	     (and (eq buffer (current-buffer))
		  (< (point) last-point)
		  (message "Wrapped"))
	     ))
	  ((and gnus-auto-extend-newsgroup
		(not unread)		;Not unread only
		(not subject)		;Only if subject is not specified.
		(setq header (gnus-more-header-forward)))
	   ;; Extend to next article if possible.
	   ;; Basic ideas by himacdonald@watdragon.waterloo.edu
	   (gnus-extend-newsgroup header nil)
	   ;; Threads feature must be turned off.
	   (let ((buffer-read-only nil))
	     (goto-char (point-max))
	     (gnus-summary-prepare-threads (list header) 0))
	   (gnus-summary-goto-article gnus-newsgroup-end))
	  (t
	   ;; Select next newsgroup automatically if requested.
	   (let ((cmd (aref (this-command-keys) 0))
		 (group (gnus-summary-search-group))
		 (auto-select
		  (and gnus-auto-select-next
		       ;;(null (gnus-set-difference gnus-newsgroup-unreads
		       ;;				gnus-newsgroup-marked))
		       (memq this-command
			     '(gnus-summary-next-unread-article
			       gnus-summary-next-article
			       gnus-summary-next-page
			       gnus-summary-next-same-subject
			       gnus-summary-next-unread-same-subject
			       gnus-summary-kill-same-subject
			       gnus-summary-kill-same-subject-and-select
			       ))
		       ;; Ignore characters typed ahead.
		       (not (input-pending-p))
		       )))
	     ;; Keep just the event type of CMD.
	     (if (listp cmd)
		 (setq cmd (car cmd)))
	     (message "No more%s articles%s"
		      (if unread " unread" "")
		      (if (and auto-select
			       (not (eq gnus-auto-select-next 'quietly)))
			  (if group
			      (format " (Type %s for %s [%d])"
				      (single-key-description cmd)
				      group
				      (nth 1 (gnus-gethash group
							   gnus-unread-hashtb)))
			    (format " (Type %s to exit %s)"
				    (single-key-description cmd)
				    gnus-newsgroup-name))
			""))
	     ;; Select next unread newsgroup automagically.
	     (cond ((and auto-select
			 (eq gnus-auto-select-next 'quietly))
		    ;; Select quietly.
		    (gnus-summary-next-group nil))
		   (auto-select
		    ;; Confirm auto selection.
		    (let* ((event (read-event))
			   (type
			    (if (listp event)
				(car event)
			      event)))
		      (if (and (eq event type) (eq event cmd))
			  (gnus-summary-next-group nil)
			(setq unread-command-events (list event)))))
		   )
	     ))
	  )))

(defun gnus-summary-next-unread-article ()
  "Select unread article after current one."
  (interactive)
  (gnus-summary-next-article t (and gnus-auto-select-same
				    (gnus-summary-subject-string))))

(defun gnus-summary-prev-article (unread &optional subject)
  "Select article before current one.
If argument UNREAD is non-nil, only unread article is selected."
  (interactive "P")
  (let ((header nil))
    (cond ((gnus-summary-display-article
	    (gnus-summary-search-backward unread subject)))
	  ((and subject
		gnus-auto-select-same
		(gnus-set-difference gnus-newsgroup-unreads
				     gnus-newsgroup-marked)
		(memq this-command
		      '(gnus-summary-prev-unread-article
			;;gnus-summary-prev-page
			;;gnus-summary-prev-article
			;;gnus-summary-prev-same-subject
			;;gnus-summary-prev-unread-same-subject
			)))
	   ;; Ignore given SUBJECT, and try again.
	   (gnus-summary-prev-article unread nil))
	  (unread
	   (message "No more unread articles"))
	  ((and gnus-auto-extend-newsgroup
		(not subject)		;Only if subject is not specified.
		(setq header (gnus-more-header-backward)))
	   ;; Extend to previous article if possible.
	   ;; Basic ideas by himacdonald@watdragon.waterloo.edu
	   (gnus-extend-newsgroup header t)
	   (let ((buffer-read-only nil))
	     (goto-char (point-min))
	     (gnus-summary-prepare-threads (list header) 0))
	   (gnus-summary-goto-article gnus-newsgroup-begin))
	  (t
	   (message "No more articles"))
	  )))

(defun gnus-summary-prev-unread-article ()
  "Select unread article before current one."
  (interactive)
  (gnus-summary-prev-article t (and gnus-auto-select-same
				    (gnus-summary-subject-string))))

(defun gnus-summary-next-page (lines)
  "Show next page of selected article.
If end of article, select next article.
Argument LINES specifies lines to be scrolled up."
  (interactive "P")
  (let ((article (gnus-summary-article-number))
	(endp nil))
    (if (or (null gnus-current-article)
	    (/= article gnus-current-article))
	;; Selected subject is different from current article's.
	(gnus-summary-display-article article)
      (gnus-configure-windows 'article)
      (pop-to-buffer gnus-summary-buffer)
      (gnus-eval-in-buffer-window gnus-article-buffer
	(setq endp (gnus-article-next-page lines)))
      (cond ((and endp lines)
	     (message "End of message"))
	    ((and endp (null lines))
	     (gnus-summary-next-unread-article)))
      )))

(defun gnus-summary-prev-page (lines)
  "Show previous page of selected article.
Argument LINES specifies lines to be scrolled down."
  (interactive "P")
  (let ((article (gnus-summary-article-number)))
    (if (or (null gnus-current-article)
	    (/= article gnus-current-article))
	;; Selected subject is different from current article's.
	(gnus-summary-display-article article)
      (gnus-configure-windows 'article)
      (pop-to-buffer gnus-summary-buffer)
      (gnus-eval-in-buffer-window gnus-article-buffer
	(gnus-article-prev-page lines))
      )))

(defun gnus-summary-scroll-up (lines)
  "Scroll up (or down) one line current article.
Argument LINES specifies lines to be scrolled up (or down if negative)."
  (interactive "p")
  (gnus-summary-select-article)
  (gnus-eval-in-buffer-window gnus-article-buffer
    (cond ((> lines 0)
	   (if (gnus-article-next-page lines)
	       (message "End of message")))
	  ((< lines 0)
	   (gnus-article-prev-page (- 0 lines))))
    ))

(defun gnus-summary-next-same-subject ()
  "Select next article which has the same subject as current one."
  (interactive)
  (gnus-summary-next-article nil (gnus-summary-subject-string)))

(defun gnus-summary-prev-same-subject ()
  "Select previous article which has the same subject as current one."
  (interactive)
  (gnus-summary-prev-article nil (gnus-summary-subject-string)))

(defun gnus-summary-next-unread-same-subject ()
  "Select next unread article which has the same subject as current one."
  (interactive)
  (gnus-summary-next-article t (gnus-summary-subject-string)))

(defun gnus-summary-prev-unread-same-subject ()
  "Select previous unread article which has the same subject as current one."
  (interactive)
  (gnus-summary-prev-article t (gnus-summary-subject-string)))

(defun gnus-summary-refer-parent-article (child)
  "Refer parent article of current article.
If a prefix argument CHILD is non-nil, go back to the child article
using internally maintained articles history.
NOTE: This command may not work with `nnspool.el'."
  (interactive "P")
  (gnus-summary-select-article t t)	;Request all headers.
  (let ((referenced-id nil))		;Message-id of parent or child article.
    (if child
	;; Go back to child article using history.
	(gnus-summary-refer-article nil)
      (gnus-eval-in-buffer-window gnus-article-buffer
	;; Look for parent Message-ID.
	;; We cannot use gnus-current-headers to get references
	;; because we may be looking at parent or referred article.
	(let ((references (gnus-fetch-field "References")))
	  ;; Get the last message-id in the references.
	  (and references
	       (string-match "\\(<[^<>]+>\\)[^>]*\\'" references)
	       (setq referenced-id
		     (substring references
				(match-beginning 1) (match-end 1))))
	  ))
      (if (stringp referenced-id)
	  (gnus-summary-refer-article referenced-id)
	(error "No more parents"))
      )))

(defun gnus-summary-refer-article (message-id)
  "Refer article specified by MESSAGE-ID.
If the MESSAGE-ID is nil or an empty string, Message-ID is poped from
internally maintained articles history.
NOTE: This command may not work with `nnspool.el' nor `mhspool.el'."
  (interactive "sMessage-ID: ")
  ;; Make sure that this command depends on the fact that article
  ;; related information is not updated when an article is retrieved
  ;; by Message-ID.
  (gnus-summary-select-article t t)	;Request all headers.
  (if (and (stringp message-id)
	   (> (length message-id) 0))
      (gnus-eval-in-buffer-window gnus-article-buffer
	;; Construct the correct Message-ID if necessary.
	;; Suggested by tale@pawl.rpi.edu.
	(or (string-match "^<" message-id)
	    (setq message-id (concat "<" message-id)))
	(or (string-match ">$" message-id)
	    (setq message-id (concat message-id ">")))
	;; Push current message-id on history.
	;; We cannot use gnus-current-headers to get current
	;; message-id because we may be looking at parent or referred
	;; article.
	(let ((current (gnus-fetch-field "Message-ID")))
	  (or (equal current message-id) ;Nothing to do.
	      (equal current (car gnus-current-history))
	      (setq gnus-current-history
		    (cons current gnus-current-history)))
	  ))
    ;; Pop message-id from history.
    (setq message-id (car gnus-current-history))
    (setq gnus-current-history (cdr gnus-current-history)))
  (if (stringp message-id)
      ;; Retrieve article by message-id.  This may not work with
      ;; nnspool nor mhspool.
      (gnus-article-prepare message-id t)
    (error "No such references"))
  )

(defun gnus-summary-next-digest (n)
  "Move to head of Nth next digested message."
  (interactive "p")
  (gnus-summary-select-article)
  (gnus-eval-in-buffer-window gnus-article-buffer
    (gnus-article-next-digest (or n 1))
    ))

(defun gnus-summary-prev-digest (n)
  "Move to head of Nth previous digested message."
  (interactive "p")
  (gnus-summary-select-article)
  (gnus-eval-in-buffer-window gnus-article-buffer
    (gnus-article-prev-digest (or n 1))))

(defun gnus-summary-first-unread-article ()
  "Select first unread article.  Return non-nil if successfully selected."
  (interactive)
  (let ((begin (point)))
    (goto-char (point-min))
    (if (re-search-forward "^ [ \t]+[0-9]+:" nil t)
	(gnus-summary-display-article (gnus-summary-article-number))
      ;; If there is no unread articles, stay there.
      (goto-char begin)
      ;;(gnus-summary-display-article (gnus-summary-article-number))
      (message "No more unread articles")
      nil
      )
    ))

(defun gnus-summary-isearch-article ()
  "Do incremental search forward on current article."
  (interactive)
  (gnus-summary-select-article)
  (gnus-eval-in-buffer-window gnus-article-buffer
			      (isearch-forward)))

(defun gnus-summary-search-article-forward (regexp)
  "Search for an article containing REGEXP forward.
`gnus-select-article-hook' is not called for articles examined
by searching search."
  (interactive
   (list (read-string
	  (concat "Search forward (regexp): "
		  (if gnus-last-search-regexp
		      (concat "(default " gnus-last-search-regexp ") "))))))
  (if (string-equal regexp "")
      (setq regexp (or gnus-last-search-regexp ""))
    (setq gnus-last-search-regexp regexp))
  (if (gnus-summary-search-article regexp nil)
      (gnus-eval-in-buffer-window gnus-article-buffer
	(recenter 0)
	;;(sit-for 1)
	)
    (error "Search failed: \"%s\"" regexp)
    ))

(defun gnus-summary-search-article-backward (regexp)
  "Search for an article containing REGEXP backward.
`gnus-select-article-hook' is not called for articles examined
by searching search."
  (interactive
   (list (read-string
	  (concat "Search backward (regexp): "
		  (if gnus-last-search-regexp
		      (concat "(default " gnus-last-search-regexp ") "))))))
  (if (string-equal regexp "")
      (setq regexp (or gnus-last-search-regexp ""))
    (setq gnus-last-search-regexp regexp))
  (if (gnus-summary-search-article regexp t)
      (gnus-eval-in-buffer-window gnus-article-buffer
	(recenter 0)
	;;(sit-for 1)
	)
    (error "Search failed: \"%s\"" regexp)
    ))

(defun gnus-summary-search-article (regexp &optional backward)
  "Search for an article containing REGEXP.
Optional argument BACKWARD means do search for backward.
`gnus-select-article-hook' is not called for articles examined
by searching search."
  (let ((gnus-select-article-hook nil)	;Disable hook.
	(gnus-mark-article-hook nil)	;Inhibit marking as read.
	(re-search
	 (if backward
	     (function re-search-backward) (function re-search-forward)))
	(found nil)
	(last nil))
    ;; Hidden thread subtrees must be searched for ,too.
    (gnus-summary-show-all-threads)
    ;; First of all, search current article.
    ;; We don't want to read article again from NNTP server nor reset
    ;; current point.
    (gnus-summary-select-article)
    (message "Searching article: %d..." gnus-current-article)
    (setq last gnus-current-article)
    (gnus-eval-in-buffer-window gnus-article-buffer
      (save-restriction
	(widen)
	;; Begin search from current point.
	(setq found (funcall re-search regexp nil t))))
    ;; Then search next articles.
    (while (and (not found)
		(gnus-summary-display-article 
		 (gnus-summary-search-subject backward nil nil)))
      (message "Searching article: %d..." gnus-current-article)
      (gnus-eval-in-buffer-window gnus-article-buffer
	(save-restriction
	  (widen)
	  (goto-char (if backward (point-max) (point-min)))
	  (setq found (funcall re-search regexp nil t)))
	))
    (message "")
    ;; Adjust article pointer.
    (or (eq last gnus-current-article)
	(setq gnus-last-article last))
    ;; Return T if found such article.
    found
    ))

(defun gnus-summary-execute-command (field regexp command &optional backward)
  "If FIELD of article header matches REGEXP, execute a COMMAND string.
If FIELD is an empty string (or nil), entire article body is searched for.
If optional (prefix) argument BACKWARD is non-nil, do backward instead."
  (interactive
   (list (let ((completion-ignore-case t))
	   (completing-read "Field name: "
			    '(("Number")("Subject")("From")
			      ("Lines")("Date")("Id")
			      ("Xref")("References"))
			    nil 'require-match))
	 (read-string "Regexp: ")
	 (read-key-sequence "Command: ")
	 current-prefix-arg))
  ;; Hidden thread subtrees must be searched for ,too.
  (gnus-summary-show-all-threads)
  ;; We don't want to change current point nor window configuration.
  (save-excursion
    (save-window-excursion
      (message "Executing %s..." (key-description command))
      ;; We'd like to execute COMMAND interactively so as to give arguments.
      (gnus-execute field regexp
		    (` (lambda ()
			 (call-interactively '(, (key-binding command)))))
		    backward)
      (message "Executing %s...done" (key-description command)))))

(defun gnus-summary-beginning-of-article ()
  "Go to beginning of article body."
  (interactive)
  (gnus-summary-select-article)
  (gnus-eval-in-buffer-window gnus-article-buffer
    (widen)
    (beginning-of-buffer)
    (if gnus-break-pages
	(gnus-narrow-to-page))
    ))

(defun gnus-summary-end-of-article ()
  "Go to end of article body."
  (interactive)
  (gnus-summary-select-article)
  (gnus-eval-in-buffer-window gnus-article-buffer
    (widen)
    (end-of-buffer)
    (if gnus-break-pages
	(gnus-narrow-to-page))
    ))

(defun gnus-summary-goto-article (article &optional all-headers)
  "Read article number ARTICLE if it exists.
Optional argument ALL-HEADERS means show the full header."
  (interactive
   (list
    (string-to-int
     (completing-read "Article number: "
		      (mapcar
		       (function
			(lambda (headers)
			  (list
			   (int-to-string (nntp-header-number headers)))))
		       gnus-newsgroup-headers)
		      nil 'require-match))))
  (if (gnus-summary-goto-subject article)
      (gnus-summary-display-article article all-headers)))

(defun gnus-summary-goto-last-article ()
  "Go to last subject line."
  (interactive)
  (if gnus-last-article
      (gnus-summary-goto-article gnus-last-article)))

(defun gnus-summary-show-article ()
  "Force to show current article."
  (interactive)
  ;; The following is a trick to force to read the current article again.
  (setq gnus-have-all-headers (not gnus-have-all-headers))
  (gnus-summary-select-article (not gnus-have-all-headers) t))

(defun gnus-summary-toggle-header (arg)
  "Show original header if pruned header currently shown, or vice versa.
With arg, show original header iff arg is positive."
  (interactive "P")
  ;; Variable gnus-show-all-headers must be NIL to toggle really.
  (let ((gnus-show-all-headers nil)
	(all-headers
	 (if (null arg) (not gnus-have-all-headers)
	   (> (prefix-numeric-value arg) 0))))
    (gnus-summary-select-article all-headers t)))

(defun gnus-summary-show-all-headers ()
  "Show original article header."
  (interactive)
  (gnus-summary-select-article t t))

(defun gnus-summary-toggle-mime (arg)
  "Toggle MIME processing.
With arg, turn MIME processing on iff arg is positive."
  (interactive "P")
  (setq gnus-show-mime
	(if (null arg) (not gnus-show-mime)
	  (> (prefix-numeric-value arg) 0)))
  ;; The following is a trick to force to read the current article again.
  (setq gnus-have-all-headers (not gnus-have-all-headers))
  (gnus-summary-select-article (not gnus-have-all-headers) t))

(defun gnus-summary-stop-page-breaking ()
  "Stop page breaking by linefeed temporary (widen article buffer)."
  (interactive)
  (gnus-summary-select-article)
  (gnus-eval-in-buffer-window gnus-article-buffer
    (widen)
    ))

(defun gnus-summary-kill-same-subject-and-select (unmark)
  "Mark articles which has the same subject as read, and then select next.
If argument UNMARK is positive, remove any kinds of marks.
If argument UNMARK is negative, mark articles as unread instead."
  (interactive "P")
  (if unmark
      (setq unmark (prefix-numeric-value unmark)))
  (let ((count
	 (gnus-summary-mark-same-subject
	  (gnus-summary-subject-string) unmark)))
    ;; Select next unread article.  If auto-select-same mode, should
    ;; select the first unread article.
    (gnus-summary-next-article t (and gnus-auto-select-same
				      (gnus-summary-subject-string)))
    (message "%d articles are marked as %s"
	     count (if unmark "unread" "read"))
    ))

(defun gnus-summary-kill-same-subject (unmark)
  "Mark articles which has the same subject as read. 
If argument UNMARK is positive, remove any kinds of marks.
If argument UNMARK is negative, mark articles as unread instead."
  (interactive "P")
  (if unmark
      (setq unmark (prefix-numeric-value unmark)))
  (let ((count
	 (gnus-summary-mark-same-subject
	  (gnus-summary-subject-string) unmark)))
    ;; If marked as read, go to next unread subject.
    (if (null unmark)
	;; Go to next unread subject.
	(gnus-summary-next-subject 1 t))
    (message "%d articles are marked as %s"
	     count (if unmark "unread" "read"))
    ))

(defun gnus-summary-mark-same-subject (subject &optional unmark)
  "Mark articles with same SUBJECT as read, and return marked number.
If optional argument UNMARK is positive, remove any kinds of marks.
If optional argument UNMARK is negative, mark articles as unread instead."
  (let ((count 1))
    (save-excursion
      (cond ((null unmark)
	     (gnus-summary-mark-as-read nil "K"))
	    ((> unmark 0)
	     (gnus-summary-mark-as-unread nil t))
	    (t
	     (gnus-summary-mark-as-unread)))
      (while (and subject
		  (gnus-summary-search-forward nil subject))
	(cond ((null unmark)
	       (gnus-summary-mark-as-read nil "K"))
	      ((> unmark 0)
	       (gnus-summary-mark-as-unread nil t))
	      (t
	       (gnus-summary-mark-as-unread)))
	(setq count (1+ count))
	))
    ;; Hide killed thread subtrees.  Does not work properly always.
    ;;(and (null unmark)
    ;;     gnus-thread-hide-killed
    ;;	   (gnus-summary-hide-thread))
    ;; Return number of articles marked as read.
    count
    ))

(defun gnus-summary-mark-as-unread-forward (count)
  "Mark current article as unread, and then go forward.
Argument COUNT specifies number of articles marked as unread."
  (interactive "p")
  (while (> count 0)
    (gnus-summary-mark-as-unread nil nil)
    (gnus-summary-next-subject 1 nil)
    (setq count (1- count))))

(defun gnus-summary-mark-as-unread-backward (count)
  "Mark current article as unread, and then go backward.
Argument COUNT specifies number of articles marked as unread."
  (interactive "p")
  (while (> count 0)
    (gnus-summary-mark-as-unread nil nil)
    (gnus-summary-prev-subject 1 nil)
    (setq count (1- count))))

(defun gnus-summary-mark-as-unread (&optional article clear-mark)
  "Mark current article as unread.
Optional 1st argument ARTICLE specifies article number to be marked as unread.
Optional 2nd argument CLEAR-MARK remove any kinds of mark."
  (save-excursion
    (set-buffer gnus-summary-buffer)
    ;; First of all, show hidden thread subtrees.
    (gnus-summary-show-thread)
    (let* ((buffer-read-only nil)
	   (current (gnus-summary-article-number))
	   (article (or article current)))
      (gnus-mark-article-as-unread article clear-mark)
      (if (or (eq article current)
	      (gnus-summary-goto-subject article))
	  (progn
	    (beginning-of-line)
	    (delete-char 1)
	    (insert (if clear-mark " " "-"))))
      )))

(defun gnus-summary-mark-as-read-forward (count)
  "Mark current article as read, and then go forward.
Argument COUNT specifies number of articles marked as read."
  (interactive "p")
  (while (> count 0)
    (gnus-summary-mark-as-read)
    (gnus-summary-next-subject 1 'unread-only)
    (setq count (1- count))))

(defun gnus-summary-mark-as-read-backward (count)
  "Mark current article as read, and then go backward.
Argument COUNT specifies number of articles marked as read."
  (interactive "p")
  (while (> count 0)
    (gnus-summary-mark-as-read)
    (gnus-summary-prev-subject 1 'unread-only)
    (setq count (1- count))))

(defun gnus-summary-mark-as-read (&optional article mark)
  "Mark current article as read.
Optional 1st argument ARTICLE specifies article number to be marked as read.
Optional 2nd argument MARK specifies a string inserted at beginning of line.
Any kind of string (length 1) except for a space and `-' is ok."
  (save-excursion
    (set-buffer gnus-summary-buffer)
    ;; First of all, show hidden thread subtrees.
    (gnus-summary-show-thread)
    (let* ((buffer-read-only nil)
	   (mark (or mark "D"))		;Default mark is `D'.
	   (current (gnus-summary-article-number))
	   (article (or article current)))
      (gnus-mark-article-as-read article)
      (if (or (eq article current)
	      (gnus-summary-goto-subject article))
	  (progn
	    (beginning-of-line)
	    (delete-char 1)
	    (insert mark)))
      )))

(defun gnus-summary-clear-mark-forward (count)
  "Remove current article's mark, and go forward.
Argument COUNT specifies number of articles unmarked."
  (interactive "p")
  (while (> count 0)
    (gnus-summary-mark-as-unread nil t)
    (gnus-summary-next-subject 1 nil)
    (setq count (1- count))))

(defun gnus-summary-clear-mark-backward (count)
  "Remove current article's mark, and go backward.
Argument COUNT specifies number of articles unmarked."
  (interactive "p")
  (while (> count 0)
    (gnus-summary-mark-as-unread nil t)
    (gnus-summary-prev-subject 1 nil)
    (setq count (1- count))))

(defun gnus-summary-delete-marked-as-read ()
  "Delete summary lines for articles that are marked as read."
  (interactive)
  (if gnus-newsgroup-unreads
      (let ((buffer-read-only nil))
	(save-excursion
	  (goto-char (point-min))
	  (delete-non-matching-lines "^[- ]"))
	;; Adjust point.
	(if (eobp)
	    (gnus-summary-prev-subject 1)
	  (beginning-of-line)
	  (search-forward ":" nil t)))
    ;; It is not so good idea to make the buffer empty.
    (message "All articles are marked as read")
    ))

(defun gnus-summary-delete-marked-with (marks)
  "Delete lines which are marked with MARKS (e.g. \"DK\")."
  (interactive "sMarks: ")
  (let ((buffer-read-only nil))
    (save-excursion
      (goto-char (point-min))
      (delete-matching-lines (concat "^[" marks "]")))
    ;; Adjust point.
    (or (zerop (buffer-size))
	(if (eobp)
	    (gnus-summary-prev-subject 1)
	  (beginning-of-line)
	  (search-forward ":" nil t)))
    ))

;; Thread-based commands.

(defun gnus-summary-toggle-threads (arg)
  "Toggle showing conversation threads.
With arg, turn showing conversation threads on iff arg is positive."
  (interactive "P")
  (let ((current (gnus-summary-article-number)))
    (setq gnus-show-threads
	  (if (null arg) (not gnus-show-threads)
	    (> (prefix-numeric-value arg) 0)))
    (gnus-summary-prepare)
    (gnus-summary-goto-subject current)
    ))

(defun gnus-summary-show-all-threads ()
  "Show all thread subtrees."
  (interactive)
  (if gnus-show-threads
      (save-excursion
	(let ((buffer-read-only nil))
	  (subst-char-in-region (point-min) (point-max) ?\^M ?\n t)
	  ))))

(defun gnus-summary-show-thread ()
  "Show thread subtrees."
  (interactive)
  (if gnus-show-threads
      (save-excursion
	(let ((buffer-read-only nil))
	  (subst-char-in-region (progn
				  (beginning-of-line) (point))
				(progn
				  (end-of-line) (point))
				?\^M ?\n t)
	  ))))

(defun gnus-summary-hide-all-threads ()
  "Hide all thread subtrees."
  (interactive)
  (if gnus-show-threads
      (save-excursion
	;; Adjust cursor point.
	(goto-char (point-min))
	(search-forward ":" nil t)
	(let ((level (current-column)))
	  (gnus-summary-hide-thread)
	  (while (gnus-summary-search-forward)
	    (and (>= level (current-column))
		 (gnus-summary-hide-thread)))
	  ))))

(defun gnus-summary-hide-thread ()
  "Hide thread subtrees."
  (interactive)
  (if gnus-show-threads
      (save-excursion
	;; Adjust cursor point.
	(beginning-of-line)
	(search-forward ":" nil t)
	(let ((buffer-read-only nil)
	      (init (point))
	      (last (point))
	      (level (current-column)))
	  (while (and (gnus-summary-search-forward)
		      (< level (current-column)))
	    ;; Interested in lower levels.
	    (if (< level (current-column))
		(progn
		  (setq last (point))
		  ))
	    )
	  (subst-char-in-region init last ?\n ?\^M t)
	  ))))

(defun gnus-summary-next-thread (n)
  "Go to the same level next thread.
Argument N specifies the number of threads."
  (interactive "p")
  ;; Adjust cursor point.
  (beginning-of-line)
  (search-forward ":" nil t)
  (let ((init (point))
	(last (point))
	(level (current-column)))
    (while (and (> n 0)
		(gnus-summary-search-forward)
		(<= level (current-column)))
      ;; We have to skip lower levels.
      (if (= level (current-column))
	  (progn
	    (setq last (point))
	    (setq n (1- n))
	    ))
      )
    ;; Return non-nil if successfully move to the next.
    (prog1 (not (= init last))
      (goto-char last))
    ))

(defun gnus-summary-prev-thread (n)
  "Go to the same level previous thread.
Argument N specifies the number of threads."
  (interactive "p")
  ;; Adjust cursor point.
  (beginning-of-line)
  (search-forward ":" nil t)
  (let ((init (point))
	(last (point))
	(level (current-column)))
    (while (and (> n 0)
		(gnus-summary-search-backward)
		(<= level (current-column)))
      ;; We have to skip lower levels.
      (if (= level (current-column))
	  (progn
	    (setq last (point))
	    (setq n (1- n))
	    ))
      )
    ;; Return non-nil if successfully move to the previous.
    (prog1 (not (= init last))
      (goto-char last))
    ))

(defun gnus-summary-down-thread (d)
  "Go downward current thread.
Argument D specifies the depth goes down."
  (interactive "p")
  ;; Adjust cursor point.
  (beginning-of-line)
  (search-forward ":" nil t)
  (let ((last (point))
	(level (current-column)))
    (while (and (> d 0)
		(gnus-summary-search-forward)
		(<= level (current-column))) ;<= can be <.  Which do you like?
      ;; We have to skip the same levels.
      (if (< level (current-column))
	  (progn
	    (setq last (point))
	    (setq level (current-column))
	    (setq d (1- d))
	    ))
      )
    (goto-char last)
    ))

(defun gnus-summary-up-thread (d)
  "Go upward current thread.
Argument D specifies the depth goes up."
  (interactive "p")
  ;; Adjust cursor point.
  (beginning-of-line)
  (search-forward ":" nil t)
  (let ((last (point))
	(level (current-column)))
    (while (and (> d 0)
		(gnus-summary-search-backward))
      ;; We have to skip the same levels.
      (if (> level (current-column))
	  (progn
	    (setq last (point))
	    (setq level (current-column))
	    (setq d (1- d))
	    ))
      )
    (goto-char last)
    ))

(defun gnus-summary-kill-thread (unmark)
  "Mark articles under current thread as read.
If argument UNMARK is positive, remove any kinds of marks.
If argument UNMARK is negative, mark articles as unread instead."
  (interactive "P")
  (if unmark
      (setq unmark (prefix-numeric-value unmark)))
  ;; Adjust cursor point.
  (beginning-of-line)
  (search-forward ":" nil t)
  (save-excursion
    (let ((level (current-column)))
      ;; Mark current article.
      (cond ((null unmark)
	     (gnus-summary-mark-as-read nil "K"))
	    ((> unmark 0)
	     (gnus-summary-mark-as-unread nil t))
	    (t
	     (gnus-summary-mark-as-unread))
	    )
      ;; Mark following articles.
      (while (and (gnus-summary-search-forward)
		  (< level (current-column)))
	(cond ((null unmark)
	       (gnus-summary-mark-as-read nil "K"))
	      ((> unmark 0)
	       (gnus-summary-mark-as-unread nil t))
	      (t
	       (gnus-summary-mark-as-unread))
	      ))
      ))
  ;; Hide killed subtrees.
  (and (null unmark)
       gnus-thread-hide-killed
       (gnus-summary-hide-thread))
  ;; If marked as read, go to next unread subject.
  (if (null unmark)
      ;; Go to next unread subject.
      (gnus-summary-next-subject 1 t))
  )

(defun gnus-summary-toggle-truncation (arg)
  "Toggle truncation of summary lines.
With arg, turn line truncation on iff arg is positive."
  (interactive "P")
  (setq truncate-lines
	(if (null arg) (not truncate-lines)
	  (> (prefix-numeric-value arg) 0)))
  (redraw-display))

(defun gnus-summary-sort-by-number (reverse)
  "Sort Summary buffer by article number.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-summary-keysort-summary
   (function <)
   (function
    (lambda (a)
      (nntp-header-number a)))
   reverse
   ))

(defun gnus-summary-sort-by-author (reverse)
  "Sort Summary buffer by author name alphabetically.
If case-fold-search is non-nil, case of letters is ignored.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-summary-keysort-summary
   (function string-lessp)
   (function
    (lambda (a)
      (if case-fold-search
	  (downcase (nntp-header-from a))
	(nntp-header-from a))))
   reverse
   ))

(defun gnus-summary-sort-by-subject (reverse)
  "Sort Summary buffer by subject alphabetically.  `Re:'s are ignored.
If case-fold-search is non-nil, case of letters is ignored.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-summary-keysort-summary
   (function string-lessp)
   (function
    (lambda (a)
      (if case-fold-search
	  (downcase (gnus-simplify-subject (nntp-header-subject a) 're-only))
	(gnus-simplify-subject (nntp-header-subject a) 're-only))))
   reverse
   ))

(defun gnus-summary-sort-by-date (reverse)
  "Sort Summary buffer by date.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-summary-keysort-summary
   (function string-lessp)
   (function
    (lambda (a)
      (gnus-sortable-date (nntp-header-date a))))
   reverse
   ))

(defun gnus-summary-keysort-summary (predicate key &optional reverse)
  "Sort Summary buffer by PREDICATE using a value passed by KEY.
Optional argument REVERSE means reverse order."
  (let ((current (gnus-summary-article-number)))
    (gnus-keysort-headers predicate key reverse)
    (gnus-summary-prepare)
    (gnus-summary-goto-subject current)
    ))

(defun gnus-summary-sort-summary (predicate &optional reverse)
  "Sort Summary buffer by PREDICATE.
Optional argument REVERSE means reverse order."
  (let ((current (gnus-summary-article-number)))
    (gnus-sort-headers predicate reverse)
    (gnus-summary-prepare)
    (gnus-summary-goto-subject current)
    ))

(defun gnus-summary-reselect-current-group (show-all)
  "Once exit and then reselect the current newsgroup.
Prefix argument SHOW-ALL means to select all articles."
  (interactive "P")
  (let ((current-subject (gnus-summary-article-number)))
    (gnus-summary-exit t)
    ;; We have to adjust the point of Group mode buffer because the
    ;; current point was moved to the next unread newsgroup by
    ;; exiting.
    (gnus-summary-jump-to-group gnus-newsgroup-name)
    (gnus-group-read-group show-all t)
    (gnus-summary-goto-subject current-subject)
    ))

(defun gnus-summary-caesar-message (rotnum)
  "Caesar rotates all letters of current message by 13/47 places.
With prefix arg, specifies the number of places to rotate each letter forward.
Caesar rotates Japanese letters by 47 places in any case."
  (interactive "P")
  (gnus-summary-select-article)
  (gnus-overload-functions)
  (gnus-eval-in-buffer-window gnus-article-buffer
    (save-restriction
      (widen)
      ;; We don't want to jump to the beginning of the message.
      ;; `save-excursion' does not do its job.
      (move-to-window-line 0)
      (let ((last (point)))
	(news-caesar-buffer-body rotnum)
	(goto-char last)
	(recenter 0)
	))
    ))

(defun gnus-summary-rmail-digest ()
  "Run RMAIL on current digest article.
`gnus-select-digest-hook' will be called with no arguments, if that
value is non-nil.  It is possible to modify the article so that Rmail
can work with it.
`gnus-rmail-digest-hook' will be called with no arguments, if that value
is non-nil.  The hook is intended to customize Rmail mode."
  (interactive)
  (gnus-summary-select-article)
  (require 'rmail)
  (let ((artbuf gnus-article-buffer)
	(digbuf (get-buffer-create gnus-digest-buffer))
	(mail-header-separator ""))
    (set-buffer digbuf)
    (buffer-flush-undo (current-buffer))
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert-buffer-substring artbuf)
    (run-hooks 'gnus-select-digest-hook)
    (gnus-convert-article-to-rmail)
    (goto-char (point-min))
    ;; Rmail initializations.
    (rmail-insert-rmail-file-header)
    (rmail-mode)
    (rmail-set-message-counters)
    (rmail-show-message)
    (condition-case ()
	(progn
	  (undigestify-rmail-message)
	  (rmail-expunge)		;Delete original message.
	  ;; File name is meaningless but `save-buffer' requires it.
	  (setq buffer-file-name "GNUS Digest")
	  (setq mode-line-buffer-identification
		(concat "Digest: "
			(nntp-header-subject gnus-current-headers)))
	  ;; There is no need to write this buffer to a file.
	  (make-local-variable 'write-file-hooks)
	  (setq write-file-hooks
		(list (function
		       (lambda ()
			 (set-buffer-modified-p nil)
			 (message "(No changes need to be saved)")
			 'no-need-to-write-this-buffer))))
	  ;; Default file name saving digest messages.
	  (setq rmail-default-rmail-file
		(funcall gnus-rmail-save-name
			 gnus-newsgroup-name
			 gnus-current-headers
			 gnus-newsgroup-last-rmail
			 ))
	  (setq rmail-default-file
		(funcall gnus-mail-save-name
			 gnus-newsgroup-name
			 gnus-current-headers
			 gnus-newsgroup-last-mail
			 ))
	  ;; Prevent generating new buffer named ***<N> each time.
	  (setq rmail-summary-buffer
		(get-buffer-create gnus-digest-summary-buffer))
	  (run-hooks 'gnus-rmail-digest-hook)
	  ;; Take all windows safely.
	  (gnus-configure-windows '(1 0 0))
	  (pop-to-buffer gnus-group-buffer)
	  ;; Use Summary Article windows for Digest summary and
	  ;; Digest buffers.
	  (if gnus-digest-show-summary
	      (let ((gnus-summary-buffer gnus-digest-summary-buffer)
		    (gnus-article-buffer gnus-digest-buffer))
		(gnus-configure-windows 'article)
		(pop-to-buffer gnus-digest-buffer)
		(rmail-summary)
		(pop-to-buffer gnus-digest-summary-buffer)
		(message (substitute-command-keys
			  "Type \\[rmail-summary-quit] to return to GNUS")))
	    (let ((gnus-summary-buffer gnus-digest-buffer))
	      (gnus-configure-windows 'summary)
	      (pop-to-buffer gnus-digest-buffer)
	      (message (substitute-command-keys
			"Type \\[rmail-quit] to return to GNUS")))
	    )
	  ;; Move the buffers to the end of buffer list.
	  (bury-buffer gnus-article-buffer)
	  (bury-buffer gnus-group-buffer)
	  (bury-buffer gnus-digest-summary-buffer)
	  (bury-buffer gnus-digest-buffer))
      (error (set-buffer-modified-p nil)
	     (kill-buffer digbuf)
	     ;; This command should not signal an error because the
	     ;; command is called from hooks.
	     (ding) (message "Article is not a digest")))
    ))

(defun gnus-summary-save-article ()
  "Save this article using default saver function.
The variable `gnus-default-article-saver' specifies the saver function."
  (interactive)
  (gnus-summary-select-article gnus-save-all-headers gnus-save-all-headers)
  (if gnus-default-article-saver
      (call-interactively gnus-default-article-saver)
    (error "No default saver is defined.")))

(defun gnus-summary-save-in-rmail (&optional filename)
  "Append this article to Rmail file.
Optional argument FILENAME specifies file name.
Directory to save to is default to `gnus-article-save-directory' which
is initialized from the SAVEDIR environment variable."
  (interactive)
  (gnus-summary-select-article gnus-save-all-headers gnus-save-all-headers)
  (gnus-eval-in-buffer-window gnus-article-buffer
    (save-excursion
      (save-restriction
	(widen)
	(let ((default-name
		(funcall gnus-rmail-save-name
			 gnus-newsgroup-name
			 gnus-current-headers
			 gnus-newsgroup-last-rmail
			 )))
	  (or filename
	      (setq filename
		    (read-file-name
		     (concat "Save article in Rmail file: (default "
			     (file-name-nondirectory default-name)
			     ") ")
		     (file-name-directory default-name)
		     default-name)))
	  (gnus-make-directory (file-name-directory filename))
	  (gnus-output-to-rmail filename)
	  ;; Remember the directory name to save articles.
	  (setq gnus-newsgroup-last-rmail filename)
	  )))
    ))

(defun gnus-summary-save-in-mail (&optional filename)
  "Append this article to Unix mail file.
Optional argument FILENAME specifies file name.
Directory to save to is default to `gnus-article-save-directory' which
is initialized from the SAVEDIR environment variable."
  (interactive)
  (gnus-summary-select-article gnus-save-all-headers gnus-save-all-headers)
  (gnus-eval-in-buffer-window gnus-article-buffer
    (save-excursion
      (save-restriction
	(widen)
	(let ((default-name
		(funcall gnus-mail-save-name
			 gnus-newsgroup-name
			 gnus-current-headers
			 gnus-newsgroup-last-mail
			 )))
	  (or filename
	      (setq filename
		    (read-file-name
		     (concat "Save article in Unix mail file: (default "
			     (file-name-nondirectory default-name)
			     ") ")
		     (file-name-directory default-name)
		     default-name)))
	  (setq filename
		(expand-file-name filename
				  (and default-name
				       (file-name-directory default-name))))
	  (gnus-make-directory (file-name-directory filename))
	  (if (and (file-readable-p filename) (rmail-file-p filename))
	      (gnus-output-to-rmail filename)
	    (rmail-output filename 1 t t))
	  ;; Remember the directory name to save articles.
	  (setq gnus-newsgroup-last-mail filename)
	  )))
    ))

(defun gnus-summary-save-in-file (&optional filename)
  "Append this article to file.
Optional argument FILENAME specifies file name.
Directory to save to is default to `gnus-article-save-directory' which
is initialized from the SAVEDIR environment variable."
  (interactive)
  (gnus-summary-select-article gnus-save-all-headers gnus-save-all-headers)
  (gnus-eval-in-buffer-window gnus-article-buffer
    (save-excursion
      (save-restriction
	(widen)
	(let ((default-name
		(funcall gnus-file-save-name
			 gnus-newsgroup-name
			 gnus-current-headers
			 gnus-newsgroup-last-file
			 )))
	  (or filename
	      (setq filename
		    (read-file-name
		     (concat "Save article in file: (default "
			     (file-name-nondirectory default-name)
			     ") ")
		     (file-name-directory default-name)
		     default-name)))
	  (gnus-make-directory (file-name-directory filename))
	  (gnus-output-to-file filename)
	  ;; Remember the directory name to save articles.
	  (setq gnus-newsgroup-last-file filename)
	  )))
    ))

(defun gnus-summary-save-in-folder (&optional folder)
  "Save this article to MH folder (using `rcvstore' in MH library).
Optional argument FOLDER specifies folder name."
  (interactive)
  (gnus-summary-select-article gnus-save-all-headers gnus-save-all-headers)
  (gnus-eval-in-buffer-window gnus-article-buffer
    (save-restriction
      (widen)
      ;; Thanks to yuki@flab.Fujitsu.JUNET and ohm@kaba.junet.
      (mh-find-path)
      (let ((folder
	     (or folder
		 (mh-prompt-for-folder "Save article in"
				       (funcall gnus-folder-save-name
						gnus-newsgroup-name
						gnus-current-headers
						gnus-newsgroup-last-folder
						)
				       t
				       )))
	    (errbuf (get-buffer-create " *GNUS rcvstore*")))
	(unwind-protect
	    (call-process-region (point-min) (point-max)
				 (expand-file-name "rcvstore" mh-lib)
				 nil errbuf nil folder)
	  (set-buffer errbuf)
	  (if (zerop (buffer-size))
	      (message "Article saved in folder: %s" folder)
	    (message "%s" (buffer-string)))
	  (kill-buffer errbuf)
	  (setq gnus-newsgroup-last-folder folder))
	))
    ))

(defun gnus-summary-pipe-output ()
  "Pipe this article to subprocess."
  (interactive)
  ;; Ignore `gnus-save-all-headers' since this is not save command.
  ;;(gnus-summary-select-article)
  ;; Huuum.  Is this right?
  (gnus-summary-select-article gnus-save-all-headers gnus-save-all-headers)
  (gnus-eval-in-buffer-window gnus-article-buffer
    (save-restriction
      (widen)
      (let ((command (read-string "Shell command on article: "
				  gnus-last-shell-command)))
	(if (string-equal command "")
	    (setq command gnus-last-shell-command))
	(shell-command-on-region (point-min) (point-max) command nil)
	(setq gnus-last-shell-command command)
	))
    ))

(defun gnus-summary-catchup (all &optional quietly)
  "Mark all articles not marked as unread in this newsgroup as read.
If prefix argument ALL is non-nil, all articles are marked as read."
  (interactive "P")
  (if (or quietly
	  (not gnus-interactive-catchup) ;Without confirmation?
	  (y-or-n-p
	   (if all
	       "Do you really want to mark everything as read? "
	     "Delete all articles not marked as unread? ")))
      (let ((unmarked
	     (gnus-set-difference gnus-newsgroup-unreads
				  (if (not all) gnus-newsgroup-marked))))
        (message "")			;Erase "Yes or No" question.
	;; Hidden thread subtrees must be searched for ,too.
	(gnus-summary-show-all-threads)
	(while unmarked
          (gnus-summary-mark-as-read (car unmarked) "C")
	  (setq unmarked (cdr unmarked))
	  ))
    ))

(defun gnus-summary-catchup-to-here ()
  "Mark all articles before the current one in this newsgroup as read."
  (interactive)
  (beginning-of-line)
  (let ((current (gnus-summary-article-number)))
	(beginning-of-buffer)
	(while (not (= (gnus-summary-article-number) current))
	  (gnus-summary-mark-as-read)
	  (gnus-summary-next-subject 1))))

(defun gnus-summary-catchup-all (&optional quietly)
  "Mark all articles in this newsgroup as read."
  (interactive)
  (gnus-summary-catchup t quietly))

(defun gnus-summary-catchup-and-exit (all &optional quietly)
  "Mark all articles not marked as unread in this newsgroup as read, then exit.
If prefix argument ALL is non-nil, all articles are marked as read."
  (interactive "P")
  (if (or quietly
	  (not gnus-interactive-catchup) ;Without confirmation?
	  (y-or-n-p
	   (if all
	       "Do you really want to mark everything as read? "
	     "Delete all articles not marked as unread? ")))
      (let ((unmarked
             (gnus-set-difference gnus-newsgroup-unreads
                                  (if (not all) gnus-newsgroup-marked))))
        (message "")			;Erase "Yes or No" question.
	(while unmarked
          (gnus-mark-article-as-read (car unmarked))
	  (setq unmarked (cdr unmarked)))
	;; Select next newsgroup or exit.
	(cond ((eq gnus-auto-select-next 'quietly)
	       ;; Select next newsgroup quietly.
	       (gnus-summary-next-group nil))
	      (t
	       (gnus-summary-exit)))
	)))

(defun gnus-summary-catchup-all-and-exit (&optional quietly)
  "Mark all articles in this newsgroup as read, and then exit."
  (interactive)
  (gnus-summary-catchup-and-exit t quietly))

(defun gnus-summary-edit-global-kill ()
  "Edit a global KILL file."
  (interactive)
  (setq gnus-current-kill-article (gnus-summary-article-number))
  (gnus-kill-file-edit-file nil)	;Nil stands for global KILL file.
  (message
   (substitute-command-keys
    "Editing a global KILL file (Type \\[gnus-kill-file-exit] to exit)")))

(defun gnus-summary-edit-local-kill ()
  "Edit a local KILL file applied to the current newsgroup."
  (interactive)
  (setq gnus-current-kill-article (gnus-summary-article-number))
  (gnus-kill-file-edit-file gnus-newsgroup-name)
  (message
   (substitute-command-keys
    "Editing a local KILL file (Type \\[gnus-kill-file-exit] to exit)")))

(defun gnus-summary-exit (&optional temporary)
  "Exit reading current newsgroup, and then return to group selection mode.
`gnus-exit-group-hook' is called with no arguments if that value is non-nil."
  (interactive)
  (let ((updated nil)
	(gnus-newsgroup-headers gnus-newsgroup-headers)
	(gnus-newsgroup-unreads gnus-newsgroup-unreads)
	(gnus-newsgroup-unselected gnus-newsgroup-unselected)
	(gnus-newsgroup-marked gnus-newsgroup-marked))
    ;; Important internal variables are saved, so we can reenter
    ;; Summary buffer even if hook changes them.
    (run-hooks 'gnus-exit-group-hook)
    (gnus-update-unread-articles gnus-newsgroup-name
				 (append gnus-newsgroup-unselected
					 gnus-newsgroup-unreads)
				 gnus-newsgroup-marked)
    ;; T means ignore unsubscribed newsgroups.
    (if gnus-use-cross-reference
	(setq updated
	      (gnus-mark-as-read-by-xref gnus-newsgroup-name
					 gnus-newsgroup-headers
					 gnus-newsgroup-unreads
					 (eq gnus-use-cross-reference t)
					 )))
    ;; Do not switch windows but change the buffer to work.
    (set-buffer gnus-group-buffer)
    ;; Update cross referenced group info.
    (while updated
      (gnus-group-update-group (car updated) t) ;Ignore invisible group.
      (setq updated (cdr updated)))
    (gnus-group-update-group gnus-newsgroup-name))
  ;; Make sure where I was, and go to next newsgroup.
  (gnus-group-jump-to-group gnus-newsgroup-name)
  (gnus-group-next-unread-group 1)
  (if temporary
      ;; If exiting temporary, caller should adjust Group mode
      ;; buffer point by itself.
      nil				;Nothing to do.
    ;; Return to Group mode buffer.
    (if (get-buffer gnus-summary-buffer)
	(bury-buffer gnus-summary-buffer))
    (if (get-buffer gnus-article-buffer)
	(bury-buffer gnus-article-buffer))
    (gnus-configure-windows 'newsgroups)
    (pop-to-buffer gnus-group-buffer)))

(defun gnus-summary-quit ()
  "Quit reading current newsgroup without updating read article info."
  (interactive)
  (if (y-or-n-p "Do you really wanna quit reading this group? ")
      (progn
	(message "")			;Erase "Yes or No" question.
	;; Return to Group selection mode.
	(if (get-buffer gnus-summary-buffer)
	    (bury-buffer gnus-summary-buffer))
	(if (get-buffer gnus-article-buffer)
	    (bury-buffer gnus-article-buffer))
	(gnus-configure-windows 'newsgroups)
	(pop-to-buffer gnus-group-buffer)
	(gnus-group-jump-to-group gnus-newsgroup-name) ;Make sure where I was.
	(gnus-group-next-group 1)	;(gnus-group-next-unread-group 1)
	)))

(defun gnus-summary-describe-briefly ()
  "Describe Summary mode commands briefly."
  (interactive)
  (message
   (concat
    (substitute-command-keys "\\[gnus-summary-next-page]:Select  ")
    (substitute-command-keys "\\[gnus-summary-next-unread-article]:Forward  ")
    (substitute-command-keys "\\[gnus-summary-prev-unread-article]:Backward  ")
    (substitute-command-keys "\\[gnus-summary-exit]:Exit  ")
    (substitute-command-keys "\\[gnus-info-find-node]:Run Info  ")
    (substitute-command-keys "\\[gnus-summary-describe-briefly]:This help")
    )))


;;;
;;; GNUS Article Mode
;;;

(if gnus-article-mode-map
    nil
  (setq gnus-article-mode-map (make-keymap))
  (suppress-keymap gnus-article-mode-map)
  (define-key gnus-article-mode-map " " 'gnus-article-next-page)
  (define-key gnus-article-mode-map "\177" 'gnus-article-prev-page)
  (define-key gnus-article-mode-map "r" 'gnus-article-refer-article)
  (define-key gnus-article-mode-map "o" 'gnus-article-pop-article)
  (define-key gnus-article-mode-map "h" 'gnus-article-show-summary)
  (define-key gnus-article-mode-map "s" 'gnus-article-show-summary)
  (define-key gnus-article-mode-map "?" 'gnus-article-describe-briefly)
  (define-key gnus-article-mode-map "\C-c\C-i" 'gnus-info-find-node))

(defun gnus-article-mode ()
  "Major mode for browsing through an article.
All normal editing commands are turned off.
Instead, these commands are available:
\\{gnus-article-mode-map}

Various hooks for customization:
 gnus-article-mode-hook
    Entry to this mode calls the value with no arguments, if that
    value is non-nil.

 gnus-article-prepare-hook
    Called with no arguments after an article is prepared for reading,
    if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  ;; Gee.  Why don't you upgrade?
  (cond ((boundp 'mode-line-modified)
	 (setq mode-line-modified "--- "))
	((listp (default-value 'mode-line-format))
	 (setq mode-line-format
	       (cons "--- " (cdr (default-value 'mode-line-format))))))
  ;; To disable display-time facility.
  ;;(make-local-variable 'global-mode-string)
  ;;(setq global-mode-string nil)
  (setq major-mode 'gnus-article-mode)
  (setq mode-name "Article")
  (make-local-variable 'minor-mode-alist)
  (or (assq 'gnus-show-mime minor-mode-alist)
      (setq minor-mode-alist
	    (cons (list 'gnus-show-mime " MIME") minor-mode-alist)))
  (gnus-article-set-mode-line)
  (use-local-map gnus-article-mode-map)
  (make-local-variable 'page-delimiter)
  (setq page-delimiter gnus-page-delimiter)
  (make-local-variable 'mail-header-separator)
  (setq mail-header-separator "")	;For caesar function.
  (buffer-flush-undo (current-buffer))
  (setq buffer-read-only t)		;Disable modification
  (run-hooks 'gnus-article-mode-hook))

(defun gnus-article-setup-buffer ()
  "Initialize Article mode buffer."
  (or (get-buffer gnus-article-buffer)
      (save-excursion
	(set-buffer (get-buffer-create gnus-article-buffer))
	(gnus-article-mode))
      ))

(defun gnus-article-prepare (article &optional all-headers)
  "Prepare ARTICLE in Article mode buffer.
ARTICLE can be either a article number or Message-ID.
If optional argument ALL-HEADERS is non-nil,
include the article's whole original header."
  ;; Make sure a connection to NNTP server is alive.
  (if (not (gnus-server-opened))
      (progn
	(gnus-start-news-server)
	(gnus-request-group gnus-newsgroup-name)))
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil))
      (erase-buffer)
      ;; mhspool does not work with Message-ID.  So, let's translate
      ;; it into an article number as possible as can.  This may help
      ;; nnspool too.
      ;; Note: this conversion must be done here since if the article
      ;; is specified by number or message-id has a different meaning
      ;; in the following.
      (if (let* ((header
		  (and (stringp article)
		       (gnus-get-header-by-id article)))
		 (article
		  (if header
		      (nntp-header-number header) article)))
	    (gnus-request-article article))
	  (progn
	    ;; Prepare article buffer
	    (insert-buffer-substring nntp-server-buffer)
	    ;; gnus-have-all-headers must be either T or NIL.
	    (setq gnus-have-all-headers
		  (not (not (or all-headers gnus-show-all-headers))))
	    (if (and (numberp article)
		     (not (eq article gnus-current-article)))
		;; Seems me that a new article has been selected.
		(progn
		  ;; gnus-current-article must be an article number.
		  (setq gnus-last-article gnus-current-article)
		  (setq gnus-current-article article)
;;		  (setq gnus-current-headers
;;			(gnus-find-header-by-number gnus-newsgroup-headers
;;						    gnus-current-article))
		  (setq gnus-current-headers
			(gnus-get-header-by-number gnus-current-article))
		  (run-hooks 'gnus-mark-article-hook)
		  ))
	    ;; Clear article history only when the article is
	    ;; retrieved by the article number.
	    (if (numberp article)
		(setq gnus-current-history nil))
	    ;; Hooks for modifying contents of the article.  This hook
	    ;; must be called before being narrowed.
	    (run-hooks 'gnus-article-prepare-hook)
	    ;; Decode MIME message.
	    (if (and gnus-show-mime
		     (gnus-fetch-field "Mime-Version"))
		(funcall gnus-show-mime-method))
	    ;; Delete unnecessary headers.
	    (or gnus-have-all-headers
		(gnus-article-delete-headers))
	    ;; Do page break.
	    (goto-char (point-min))
	    (if gnus-break-pages
		(gnus-narrow-to-page))
	    ;; Next function must be called after setting
	    ;;  `gnus-current-article' variable and narrowed to page.
	    (gnus-article-set-mode-line)
	    )
	;; There is no such article.
	(if (numberp article)
	    (gnus-summary-mark-as-read article))
	(ding) (message "No such article (may be canceled)"))
      )))

(defun gnus-article-show-all-headers ()
  "Show all article headers in Article mode buffer."
  (or gnus-have-all-headers
      (gnus-article-prepare gnus-current-article t)))

;;(defun gnus-article-set-mode-line ()
;;  "Set Article mode line string."
;;  (setq mode-line-buffer-identification
;;	(list 17
;;	      (format "GNUS: %s {%d-%d} %d"
;;		      gnus-newsgroup-name
;;		      gnus-newsgroup-begin
;;		      gnus-newsgroup-end
;;		      gnus-current-article
;;                    )))
;;  (set-buffer-modified-p t))

;;(defun gnus-article-set-mode-line ()
;;  "Set Article mode line string."
;;  (let ((unmarked
;;	 (- (length gnus-newsgroup-unreads)
;;	    (length (gnus-intersection
;;		     gnus-newsgroup-unreads gnus-newsgroup-marked))))
;;	(unselected
;;	 (- (length gnus-newsgroup-unselected)
;;	    (length (gnus-intersection
;;		     gnus-newsgroup-unselected gnus-newsgroup-marked)))))
;;    (setq mode-line-buffer-identification
;;	  (list 17
;;		(format "GNUS: %s{%d} %s"
;;			gnus-newsgroup-name
;;			gnus-current-article
;;			;; This is proposed by tale@pawl.rpi.edu.
;;			(cond ((and (zerop unmarked)
;;				    (zerop unselected))
;;			       "      ")
;;			      ((zerop unselected)
;;			       (format "%d more" unmarked))
;;			      (t
;;			       (format "%d(+%d) more" unmarked unselected)))
;;			))))
;;  (set-buffer-modified-p t))

;; New implementation in gnus 3.14.3

(defun gnus-article-set-mode-line ()
  "Set Article mode line string.
If you don't like it, define your own `gnus-article-set-mode-line'."
  (let ((maxlen 15)			;Maximum subject length
	(subject
	 (if gnus-current-headers
	     (nntp-header-subject gnus-current-headers) "")))
    ;; The value must be a string to escape %-constructs because of subject.
    (setq mode-line-buffer-identification
	  (format "GNUS: %s%s %s%s%s"
		  gnus-newsgroup-name
		  (if gnus-current-article
		      (format "/%d" gnus-current-article) "")
		  (substring subject 0 (min (length subject) maxlen))
		  (if (> (length subject) maxlen) "..." "")
		  (make-string (max 0 (- 17 (length subject))) ? )
		  )))
  (set-buffer-modified-p t))

(defun gnus-article-delete-headers ()
  "Delete unnecessary headers."
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      (narrow-to-region (point-min)
			(progn (search-forward "\n\n" nil 'move) (point)))
      (goto-char (point-min))
      (and (stringp gnus-ignored-headers)
	   (while (re-search-forward gnus-ignored-headers nil t)
	     (beginning-of-line)
	     (delete-region (point)
			    (progn (re-search-forward "\n[^ \t]")
				   (forward-char -1)
				   (point)))))
      )))

;; Working on article's buffer

(defun gnus-article-next-page (lines)
  "Show next page of current article.
If end of article, return non-nil.  Otherwise return nil.
Argument LINES specifies lines to be scrolled up."
  (interactive "P")
  (move-to-window-line -1)
  ;; Fixed by enami@ptgd.sony.co.jp (enami tsugutomo)
  (if (save-excursion
	(end-of-line)
	(and (pos-visible-in-window-p)	;Not continuation line.
	     (eobp)))
      ;; Nothing in this page.
      (if (or (not gnus-break-pages)
	      (save-excursion
		(save-restriction
		  (widen) (forward-line 1) (eobp)))) ;Real end-of-buffer?
	  t				;Nothing more.
	(gnus-narrow-to-page 1)		;Go to next page.
	nil
	)
    ;; More in this page.
    (condition-case ()
	(scroll-up lines)
      (end-of-buffer
       ;; Long lines may cause an end-of-buffer error.
       (goto-char (point-max))))
    nil
    ))

(defun gnus-article-prev-page (lines)
  "Show previous page of current article.
Argument LINES specifies lines to be scrolled down."
  (interactive "P")
  (move-to-window-line 0)
  (if (and gnus-break-pages
	   (bobp)
	   (not (save-restriction (widen) (bobp)))) ;Real beginning-of-buffer?
      (progn
	(gnus-narrow-to-page -1) ;Go to previous page.
	(goto-char (point-max))
	(recenter -1))
    (scroll-down lines)))

(defun gnus-article-next-digest (nth)
  "Move to head of NTH next digested message.
Set mark at end of digested message."
  ;; Stop page breaking in digest mode.
  (widen)
  (end-of-line)
  ;; Skip NTH - 1 digest.
  ;; Suggested by Khalid Sattar <admin@cs.exeter.ac.uk>.
  ;; Digest separator is customizable.
  ;; Suggested by Skip Montanaro <montanaro@sprite.crd.ge.com>.
  (while (and (> nth 1)
	      (re-search-forward gnus-digest-separator nil 'move))
    (setq nth (1- nth)))
  (if (re-search-forward gnus-digest-separator nil t)
      (let ((begin (point)))
	;; Search for end of this message.
	(end-of-line)
	(if (re-search-forward gnus-digest-separator nil t)
	    (progn
	      (search-backward "\n\n")	;This may be incorrect.
	      (forward-line 1))
	  (goto-char (point-max)))
	(push-mark)			;Set mark at end of digested message.
	(goto-char begin)
	(beginning-of-line)
	;; Show From: and Subject: fields.
	(recenter 1))
    (message "End of message")
    ))

(defun gnus-article-prev-digest (n)
  "Move to head of Nth previous digested message."
  ;; Stop page breaking in digest mode.
  (widen)
  (beginning-of-line)
  ;; Skip N - 1 digest.
  ;; Suggested by Khalid Sattar <admin@cs.exeter.ac.uk>.
  ;; Digest separator is customizable.
  ;; Suggested by Skip Montanaro <montanaro@sprite.crd.ge.com>.
  (while (and (> n 1)
	      (re-search-backward gnus-digest-separator nil 'move))
    (setq n (1- n)))
  (if (re-search-backward gnus-digest-separator nil t)
      (let ((begin (point)))
	;; Search for end of this message.
	(end-of-line)
	(if (re-search-forward gnus-digest-separator nil t)
	    (progn
	      (search-backward "\n\n")	;This may be incorrect.
	      (forward-line 1))
	  (goto-char (point-max)))
	(push-mark)			;Set mark at end of digested message.
	(goto-char begin)
	;; Show From: and Subject: fields.
	(recenter 1))
    (goto-char (point-min))
    (message "Top of message")
    ))

(defun gnus-article-refer-article ()
  "Read article specified by message-id around point."
  (interactive)
  (save-window-excursion
    (save-excursion
      (re-search-forward ">" nil t)	;Move point to end of "<....>".
      (if (re-search-backward "\\(<[^<> \t\n]+>\\)" nil t)
	  (let ((message-id
		 (buffer-substring (match-beginning 1) (match-end 1))))
	    (set-buffer gnus-summary-buffer)
	    (gnus-summary-refer-article message-id))
	(error "No references around point"))
      )))

(defun gnus-article-pop-article ()
  "Pop up article history."
  (interactive)
  (save-window-excursion
    (set-buffer gnus-summary-buffer)
    (gnus-summary-refer-article nil)))

(defun gnus-article-show-summary ()
  "Reconfigure windows to show Summary buffer."
  (interactive)
  (gnus-configure-windows 'article)
  (pop-to-buffer gnus-summary-buffer)
  (gnus-summary-goto-subject gnus-current-article))

(defun gnus-article-describe-briefly ()
  "Describe Article mode commands briefly."
  (interactive)
  (message
   (concat
    (substitute-command-keys "\\[gnus-article-next-page]:Next page  ")
    (substitute-command-keys "\\[gnus-article-prev-page]:Prev page  ")
    (substitute-command-keys "\\[gnus-article-show-summary]:Show Summary  ")
    (substitute-command-keys "\\[gnus-info-find-node]:Run Info  ")
    (substitute-command-keys "\\[gnus-article-describe-briefly]:This help")
    )))


;;;
;;; GNUS KILL-File Mode
;;;

(if gnus-kill-file-mode-map
    nil
  (setq gnus-kill-file-mode-map (copy-keymap emacs-lisp-mode-map))
  (define-key gnus-kill-file-mode-map "\C-c\C-k\C-s" 'gnus-kill-file-kill-by-subject)
  (define-key gnus-kill-file-mode-map "\C-c\C-k\C-a" 'gnus-kill-file-kill-by-author)
  (define-key gnus-kill-file-mode-map "\C-c\C-a" 'gnus-kill-file-apply-buffer)
  (define-key gnus-kill-file-mode-map "\C-c\C-e" 'gnus-kill-file-apply-last-sexp)
  (define-key gnus-kill-file-mode-map "\C-c\C-c" 'gnus-kill-file-exit)
  (define-key gnus-kill-file-mode-map "\C-c\C-i" 'gnus-info-find-node))

(defun gnus-kill-file-mode ()
  "Major mode for editing KILL file.

In addition to Emacs-Lisp Mode, the following commands are available:

\\[gnus-kill-file-kill-by-subject]	Insert KILL command for current subject.
\\[gnus-kill-file-kill-by-author]	Insert KILL command for current author.
\\[gnus-kill-file-apply-buffer]	Apply current buffer to selected newsgroup.
\\[gnus-kill-file-apply-last-sexp]	Apply sexp before point to selected newsgroup.
\\[gnus-kill-file-exit]	Save file and exit editing KILL file.
\\[gnus-info-find-node]	Read Info about KILL file.

  A KILL file contains Lisp expressions to be applied to a selected
newsgroup.  The purpose is to mark articles as read on the basis of
some set of regexps.  A global KILL file is applied to every newsgroup,
and a local KILL file is applied to a specified newsgroup.  Since a
global KILL file is applied to every newsgroup, for better performance
use a local one.

  A KILL file can contain any kind of Emacs Lisp expressions expected
to be evaluated in the Summary buffer.  Writing Lisp programs for this
purpose is not so easy because the internal working of GNUS must be
well-known.  For this reason, GNUS provides a general function which
does this easily for non-Lisp programmers.

  The `gnus-kill' function executes commands available in Summary Mode
by their key sequences.  `gnus-kill' should be called with FIELD,
REGEXP and optional COMMAND and ALL.  FIELD is a string representing
the header field or an empty string.  If FIELD is an empty string, the
entire article body is searched for.  REGEXP is a string which is
compared with FIELD value.  COMMAND is a string representing a valid
key sequence in Summary mode or Lisp expression.  COMMAND defaults to
\(gnus-summary-mark-as-read nil \"X\").  Make sure that COMMAND is
executed in the Summary buffer.  If the second optional argument ALL
is non-nil, the COMMAND is applied to articles which are already
marked as read or unread.  Articles which are marked are skipped over
by default.

  For example, if you want to mark articles of which subjects contain
the string `AI' as read, a possible KILL file may look like:

	(gnus-kill \"Subject\" \"AI\")

  If you want to mark articles with `D' instead of `X', you can use
the following expression:

	(gnus-kill \"Subject\" \"AI\" \"d\")

\(Here we assume the command `gnus-summary-mark-as-read-forward' is
assigned to `d' in Summary Mode.)

  It is possible to delete unnecessary headers which are marked with
`X' in a KILL file as follows:

	(gnus-expunge \"X\")

  If the Summary buffer is empty after applying KILL files, GNUS will
exit the selected newsgroup normally.  If headers which are marked
with `D' are deleted in a KILL file, it is impossible to read articles
which are marked as read in the previous GNUS sessions.  Marks other
than `D' should be used for articles which should really be deleted.

Entry to this mode calls `emacs-lisp-mode-hook' and
`gnus-kill-file-mode-hook' with no arguments, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map gnus-kill-file-mode-map)
  (set-syntax-table emacs-lisp-mode-syntax-table)
  (setq major-mode 'gnus-kill-file-mode)
  (setq mode-name "KILL-File")
  (lisp-mode-variables nil)
  (run-hooks 'emacs-lisp-mode-hook 'gnus-kill-file-mode-hook))

(defun gnus-kill-file-edit-file (newsgroup)
  "Begin editing a KILL file of NEWSGROUP.
If NEWSGROUP is nil, the global KILL file is selected."
  (interactive "sNewsgroup: ")
  (let ((file (gnus-newsgroup-kill-file newsgroup)))
    (gnus-make-directory (file-name-directory file))
    ;; Save current window configuration if this is first invocation.
    (or (and (get-file-buffer file)
	     (get-buffer-window (get-file-buffer file)))
	(setq gnus-winconf-kill-file (current-window-configuration)))
    ;; Hack windows.
    (let ((buffer (find-file-noselect file)))
      (cond ((get-buffer-window buffer)
	     (pop-to-buffer buffer))
	    ((eq major-mode 'gnus-group-mode)
	     (gnus-configure-windows '(1 0 0)) ;Take all windows.
	     (pop-to-buffer gnus-group-buffer)
	     (let ((gnus-summary-buffer buffer))
	       (gnus-configure-windows '(1 1 0)) ;Split into two.
	       (pop-to-buffer buffer)))
	    ((eq major-mode 'gnus-summary-mode)
	     (gnus-configure-windows 'article)
	     (pop-to-buffer gnus-article-buffer)
	     (bury-buffer gnus-article-buffer)
	     (switch-to-buffer buffer))
	    (t				;No good rules.
	     (find-file-other-window file))
	    ))
    (gnus-kill-file-mode)
    ))

(defun gnus-kill-file-kill-by-subject ()
  "Insert KILL command for current subject."
  (interactive)
  (insert
   (format "(gnus-kill \"Subject\" %s)\n"
	   (prin1-to-string
	    (if gnus-current-kill-article
		(regexp-quote
		 (nntp-header-subject
		  ;; No need to speed up this command.
		  ;;(gnus-get-header-by-number gnus-current-kill-article)
		  (gnus-find-header-by-number gnus-newsgroup-headers
					      gnus-current-kill-article)))
	      "")))))

(defun gnus-kill-file-kill-by-author ()
  "Insert KILL command for current author."
  (interactive)
  (insert
   (format "(gnus-kill \"From\" %s)\n"
	   (prin1-to-string
	    (if gnus-current-kill-article
		(regexp-quote
		 (nntp-header-from
		  ;; No need to speed up this command.
		  ;;(gnus-get-header-by-number gnus-current-kill-article)
		  (gnus-find-header-by-number gnus-newsgroup-headers
					      gnus-current-kill-article)))
	      "")))))

(defun gnus-kill-file-apply-buffer ()
  "Apply current buffer to current newsgroup."
  (interactive)
  (if (and gnus-current-kill-article
	   (get-buffer gnus-summary-buffer))
      ;; Assume newsgroup is selected.
      (let ((string (concat "(progn \n" (buffer-string) "\n)" )))
	(save-excursion
	  (save-window-excursion
	    (pop-to-buffer gnus-summary-buffer)
	    (eval (car (read-from-string string))))))
    (ding) (message "No newsgroup is selected.")))

(defun gnus-kill-file-apply-last-sexp ()
  "Apply sexp before point in current buffer to current newsgroup."
  (interactive)
  (if (and gnus-current-kill-article
	   (get-buffer gnus-summary-buffer))
      ;; Assume newsgroup is selected.
      (let ((string
	     (buffer-substring
	      (save-excursion (forward-sexp -1) (point)) (point))))
	(save-excursion
	  (save-window-excursion
	    (pop-to-buffer gnus-summary-buffer)
	    (eval (car (read-from-string string))))))
    (ding) (message "No newsgroup is selected.")))

(defun gnus-kill-file-exit ()
  "Save a KILL file, then return to the previous buffer."
  (interactive)
  (save-buffer)
  (let ((killbuf (current-buffer)))
    ;; We don't want to return to Article buffer.
    (and (get-buffer gnus-article-buffer)
	 (bury-buffer (get-buffer gnus-article-buffer)))
    ;; Delete the KILL file windows.
    (delete-windows-on killbuf)
    ;; Restore last window configuration if available.
    (and gnus-winconf-kill-file
	 (set-window-configuration gnus-winconf-kill-file))
    (setq gnus-winconf-kill-file nil)
    ;; Kill the KILL file buffer.  Suggested by tale@pawl.rpi.edu.
    (kill-buffer killbuf)))


;;;
;;; Utility functions
;;;

;; Basic ideas by emv@math.lsa.umich.edu (Edward Vielmetti)

(defun gnus-batch-kill ()
  "Run batched KILL.
Usage: emacs -batch -l gnus -f gnus-batch-kill NEWSGROUP ..."
  (if (not noninteractive)
      (error "gnus-batch-kill is to be used only with -batch"))
  (let* ((group nil)
	 (subscribed nil)
	 (newsrc nil)
	 (yes-and-no
	  (gnus-parse-n-options
	   (apply (function concat)
		  (mapcar (function (lambda (g) (concat g " ")))
			  command-line-args-left))))
	 (yes (car yes-and-no))
	 (no  (cdr yes-and-no))
	 ;; Disable verbose message.
	 (gnus-novice-user nil)
	 (gnus-large-newsgroup nil)
	 (nntp-large-newsgroup nil))
    ;; Eat all arguments.
    (setq command-line-args-left nil)
    ;; Startup GNUS.
    (gnus)
    ;; Apply kills to specified newsgroups in command line arguments.
    (setq newsrc (copy-sequence gnus-newsrc-assoc))
    (while newsrc
      (setq group (car (car newsrc)))
      (setq subscribed (nth 1 (car newsrc)))
      (setq newsrc (cdr newsrc))
      (if (and subscribed
	       (not (zerop (nth 1 (gnus-gethash group gnus-unread-hashtb))))
	       (if yes
		   (string-match yes group) t)
	       (or (null no)
		   (not (string-match no group))))
	  (progn
	    (gnus-summary-read-group group nil t)
	    (if (eq (current-buffer) (get-buffer gnus-summary-buffer))
		(gnus-summary-exit t))
	    ))
      )
    ;; Finally, exit Emacs.
    (set-buffer gnus-group-buffer)
    (gnus-group-exit)
    ))

;; For saving articles

(defun gnus-Numeric-save-name (newsgroup headers &optional last-file)
  "Generate file name from NEWSGROUP, HEADERS, and optional LAST-FILE.
If variable `gnus-use-long-file-name' is nil, it is ~/News/News.group/num.
Otherwise, it is like ~/News/news/group/num."
  (let ((default
	  (expand-file-name
	   (concat (if gnus-use-long-file-name
		       (gnus-capitalize-newsgroup newsgroup)
		     (gnus-newsgroup-directory-form newsgroup))
		   "/" (int-to-string (nntp-header-number headers)))
	   (or gnus-article-save-directory "~/News"))))
    (if (and last-file
	     (string-equal (file-name-directory default)
			   (file-name-directory last-file))
	     (string-match "^[0-9]+$" (file-name-nondirectory last-file)))
	default
      (or last-file default))))

(defun gnus-numeric-save-name (newsgroup headers &optional last-file)
  "Generate file name from NEWSGROUP, HEADERS, and optional LAST-FILE.
If variable `gnus-use-long-file-name' is nil, it is ~/News/news.group/num.
Otherwise, it is like ~/News/news/group/num."
  (let ((default
	  (expand-file-name
	   (concat (if gnus-use-long-file-name
		       newsgroup
		     (gnus-newsgroup-directory-form newsgroup))
		   "/" (int-to-string (nntp-header-number headers)))
	   (or gnus-article-save-directory "~/News"))))
    (if (and last-file
	     (string-equal (file-name-directory default)
			   (file-name-directory last-file))
	     (string-match "^[0-9]+$" (file-name-nondirectory last-file)))
	default
      (or last-file default))))

(defun gnus-Plain-save-name (newsgroup headers &optional last-file)
  "Generate file name from NEWSGROUP, HEADERS, and optional LAST-FILE.
If variable `gnus-use-long-file-name' is nil, it is ~/News/News.group.
Otherwise, it is like ~/News/news/group/news."
  (or last-file
      (expand-file-name
       (if gnus-use-long-file-name
	   (gnus-capitalize-newsgroup newsgroup)
	 (concat (gnus-newsgroup-directory-form newsgroup) "/news"))
       (or gnus-article-save-directory "~/News"))))

(defun gnus-plain-save-name (newsgroup headers &optional last-file)
  "Generate file name from NEWSGROUP, HEADERS, and optional LAST-FILE.
If variable `gnus-use-long-file-name' is nil, it is ~/News/news.group.
Otherwise, it is like ~/News/news/group/news."
  (or last-file
      (expand-file-name
       (if gnus-use-long-file-name
	   newsgroup
	 (concat (gnus-newsgroup-directory-form newsgroup) "/news"))
       (or gnus-article-save-directory "~/News"))))

(defun gnus-Folder-save-name (newsgroup headers &optional last-folder)
  "Generate folder name from NEWSGROUP, HEADERS, and optional LAST-FOLDER.
If variable `gnus-use-long-file-name' is nil, it is +News.group.
Otherwise, it is like +news/group."
  (or last-folder
      (concat "+"
	      (if gnus-use-long-file-name
		  (gnus-capitalize-newsgroup newsgroup)
		(gnus-newsgroup-directory-form newsgroup)))))

(defun gnus-folder-save-name (newsgroup headers &optional last-folder)
  "Generate folder name from NEWSGROUP, HEADERS, and optional LAST-FOLDER.
If variable `gnus-use-long-file-name' is nil, it is +news.group.
Otherwise, it is like +news/group."
  (or last-folder
      (concat "+"
	      (if gnus-use-long-file-name
		  newsgroup
		(gnus-newsgroup-directory-form newsgroup)))))

;; For KILL files

(defun gnus-apply-kill-file ()
  "Apply KILL file to the current newsgroup."
  ;; Apply the global KILL file.
  (load (gnus-newsgroup-kill-file nil) t nil t)
  ;; And then apply the local KILL file.
  (load (gnus-newsgroup-kill-file gnus-newsgroup-name) t nil t))

(defun gnus-Newsgroup-kill-file (newsgroup)
  "Return the name of a KILL file of NEWSGROUP.
If NEWSGROUP is nil, return the global KILL file instead."
  (cond ((or (null newsgroup)
	     (string-equal newsgroup ""))
	 ;; The global KILL file is placed at top of the directory.
	 (expand-file-name gnus-kill-file-name
			   (or gnus-kill-files-directory "~/News")))
	(gnus-use-long-file-name
	 ;; Append ".KILL" to capitalized newsgroup name.
	 (expand-file-name (concat (gnus-capitalize-newsgroup newsgroup)
				   "." gnus-kill-file-name)
			   (or gnus-kill-files-directory "~/News")))
	(t
	 ;; Place "KILL" under the hierarchical directory.
	 (expand-file-name (concat (gnus-newsgroup-directory-form newsgroup)
				   "/" gnus-kill-file-name)
			   (or gnus-kill-files-directory "~/News")))
	))

(defun gnus-newsgroup-kill-file (newsgroup)
  "Return the name of a KILL file of NEWSGROUP.
If NEWSGROUP is nil, return the global KILL file instead."
  (cond ((or (null newsgroup)
	     (string-equal newsgroup ""))
	 ;; The global KILL file is placed at top of the directory.
	 (expand-file-name gnus-kill-file-name
			   (or gnus-kill-files-directory "~/News")))
	(gnus-use-long-file-name
	 ;; Append ".KILL" to newsgroup name.
	 (expand-file-name (concat newsgroup "." gnus-kill-file-name)
			   (or gnus-kill-files-directory "~/News")))
	(t
	 ;; Place "KILL" under the hierarchical directory.
	 (expand-file-name (concat (gnus-newsgroup-directory-form newsgroup)
				   "/" gnus-kill-file-name)
			   (or gnus-kill-files-directory "~/News")))
	))

;; For subscribing new newsgroup

(defun gnus-subscribe-randomly (newsgroup)
  "Subscribe new NEWSGROUP and insert it at the beginning of newsgroups."
  (gnus-subscribe-newsgroup newsgroup
			    (car (car gnus-newsrc-assoc))))

(defun gnus-subscribe-alphabetically (newgroup)
  "Subscribe new NEWSGROUP and insert it in strict alphabetic order."
  ;; Basic ideas by mike-w@cs.aukuni.ac.nz (Mike Williams)
  (let ((groups gnus-newsrc-assoc)
	(before nil))
    (while (and (not before) groups)
      (if (string< newgroup (car (car groups)))
	  (setq before (car (car groups)))
	(setq groups (cdr groups))))
    (gnus-subscribe-newsgroup newgroup before)
    ))

(defun gnus-subscribe-hierarchically (newgroup)
  "Subscribe new NEWSGROUP and insert it in hierarchical newsgroup order."
  ;; Basic ideas by mike-w@cs.aukuni.ac.nz (Mike Williams)
  (save-excursion
    (set-buffer (find-file-noselect gnus-current-startup-file))
    (let ((groupkey newgroup)
	  (before nil))
      (while (and (not before) groupkey)
	(goto-char (point-min))
	(let ((groupkey-re
	       (concat "^\\(" (regexp-quote groupkey) ".*\\)[!:]")))
	  (while (and (re-search-forward groupkey-re nil t)
		      (progn
			(setq before (buffer-substring
				      (match-beginning 1) (match-end 1)))
			(string< before newgroup)))
	    ))
	;; Remove tail of newsgroup name (eg. a.b.c -> a.b)
	(setq groupkey
	      (if (string-match "^\\(.*\\)\\.[^.]+$" groupkey)
		  (substring groupkey (match-beginning 1) (match-end 1)))))
      (gnus-subscribe-newsgroup newgroup before)
      )))

(defun gnus-subscribe-interactively (newsgroup)
  "Subscribe new NEWSGROUP interactively.
It is inserted in hierarchical newsgroup order if subscribed.
Unless, it is killed."
  (if (y-or-n-p (format "Subscribe new newsgroup: %s " newsgroup))
      (gnus-subscribe-hierarchically newsgroup)
    ;; Save in kill-ring
    (gnus-subscribe-newsgroup newsgroup)
    (gnus-kill-newsgroup newsgroup)))

(defun gnus-subscribe-newsgroup (newsgroup &optional next)
  "Subscribe new NEWSGROUP.
If optional argument NEXT is non-nil, it is inserted before NEXT."
  (gnus-insert-newsgroup (list newsgroup t) next)
  (message "Subscribe newsgroup: %s" newsgroup))

;; For directories

(defun gnus-newsgroup-directory-form (newsgroup)
  "Make hierarchical directory name from NEWSGROUP name."
  (let ((newsgroup (substring newsgroup 0)) ;Copy string.
	(len (length newsgroup))
	(idx 0))
    ;; Replace all occurrences of `.' with `/'.
    (while (< idx len)
      (if (= (aref newsgroup idx) ?.)
	  (aset newsgroup idx ?/))
      (setq idx (1+ idx)))
    newsgroup
    ))

(defun gnus-make-directory (directory)
  "Make DIRECTORY recursively."
  (let ((directory (expand-file-name directory default-directory)))
    (or (file-exists-p directory)
	(gnus-make-directory-1 "" directory))
    ))

(defun gnus-make-directory-1 (head tail)
  (cond ((string-match "^/\\([^/]+\\)" tail)
	 ;; ange-ftp interferes with calling match-* after
	 ;; calling file-name-as-directory.
	 (let ((beg (match-beginning 1))
	       (end (match-end 1)))
	   (setq head (concat (file-name-as-directory head)
			      (substring tail beg end)))
	   (or (file-exists-p head)
	       (call-process "mkdir" nil nil nil head))
	   (gnus-make-directory-1 head (substring tail end))))
	((string-equal tail "") t)
	))

(defun gnus-capitalize-newsgroup (newsgroup)
  "Capitalize NEWSGROUP name with treating `.' and `-' as part of words."
  ;; Suggested by "Jonathan I. Kamens" <jik@pit-manager.MIT.EDU>.
  (let ((current-syntax-table (syntax-table)))
    (unwind-protect
	(progn
	  (set-syntax-table (copy-syntax-table current-syntax-table))
	  (modify-syntax-entry ?- "w")
	  (modify-syntax-entry ?. "w")
	  (capitalize newsgroup))
      (set-syntax-table current-syntax-table))))

(defun gnus-simplify-subject (subject &optional re-only)
  "Remove `Re:' and words in parentheses.
If optional argument RE-ONLY is non-nil, strip `Re:' only."
  (let ((case-fold-search t))		;Ignore case.
    ;; Remove `Re:' and `Re^N:'.
    (if (string-match "\\`\\(re\\(\\^[0-9]+\\)?:[ \t]+\\)+" subject)
	(setq subject (substring subject (match-end 0))))
    ;; Remove words in parentheses from end.
    (or re-only
	(while (string-match "[ \t\n]*([^()]*)[ \t\n]*\\'" subject)
	  (setq subject (substring subject 0 (match-beginning 0)))))
    ;; Return subject string.
    subject
    ))

(defun gnus-optional-lines-and-from (header)
  "Return a string like `NNN:AUTHOR' from HEADER."
  (let ((name-length (length "umerin@photon")))
    (substring (format "%3d:%s"
		       ;; Lines of the article.
		       ;; Suggested by dana@bellcore.com.
		       (nntp-header-lines header)
		       ;; Its author.
		       (concat (mail-strip-quoted-names
				(nntp-header-from header))
			       (make-string name-length ? )))
	       ;; 4 stands for length of `NNN:'.
	       0 (+ 4 name-length))))

(defun gnus-optional-lines (header)
  "Return a string like `NNN' from HEADER."
  (format "%4d" (nntp-header-lines header)))

;; Basic ideas by flee@cs.psu.edu (Felix Lee)

(defun gnus-keysort-headers (predicate key &optional reverse)
  "Sort current headers by PREDICATE using a value passed by KEY safely.
*Safely* means C-g quitting is disabled during sort.
Optional argument REVERSE means reverse order."
  (let ((inhibit-quit t))
    (setq gnus-newsgroup-headers
	  (if reverse
	      (nreverse
	       (gnus-keysort (nreverse gnus-newsgroup-headers) predicate key))
	    (gnus-keysort gnus-newsgroup-headers predicate key)))
    ;; Make sure we don't have to call
    ;; gnus-clear-hashtables-for-newsgroup-headers to clear hash
    ;; tables for the variable gnus-newsgroup-headers since no new
    ;; entry is added to nor deleted from the variable.
    ))

(defun gnus-keysort (list predicate key)
  "Sort LIST by PREDICATE using a value passed by KEY."
  (mapcar (function cdr)
	  (sort (mapcar (function (lambda (a) (cons (funcall key a) a))) list)
		(function (lambda (a b)
			    (funcall predicate (car a) (car b)))))))

(defun gnus-sort-headers (predicate &optional reverse)
  "Sort current headers by PREDICATE safely.
*Safely* means C-g quitting is disabled during sort.
Optional argument REVERSE means reverse order."
  (let ((inhibit-quit t))
    (setq gnus-newsgroup-headers
	  (if reverse
	      (nreverse (sort (nreverse gnus-newsgroup-headers) predicate))
	    (sort gnus-newsgroup-headers predicate)))
    ;; Make sure we don't have to call
    ;; gnus-clear-hashtables-for-newsgroup-headers to clear hash
    ;; tables for the variable gnus-newsgroup-headers since no new
    ;; entry is added to nor deleted from the variable.
    ))

(defun gnus-string-lessp (a b)
  "Return T if first arg string is less than second in lexicographic order.
If `case-fold-search' is non-nil, case of letters is ignored."
  (if case-fold-search
      (string-lessp (downcase a) (downcase b))
    (string-lessp a b)))

(defun gnus-date-lessp (date1 date2)
  "Return T if DATE1 is earlyer than DATE2."
  (string-lessp (gnus-sortable-date date1)
		(gnus-sortable-date date2)))

(defun gnus-sortable-date (date)
  "Convert DATE into a string that can be sorted with `string-lessp'.
Timezone package is used."
  (let* ((date   (timezone-fix-time date nil nil)) ;[Y M D H M S]
	 (year   (aref date 0))
	 (month  (aref date 1))
	 (day    (aref date 2)))
    (timezone-make-sortable-date year month day 
				 (timezone-make-time-string
				  (aref date 3) (aref date 4) (aref date 5)))
    ))

;;(defun gnus-sortable-date (date)
;;  "Make sortable string by string-lessp from DATE."
;;  (let ((month '(("JAN" . " 1")("FEB" . " 2")("MAR" . " 3")
;;		 ("APR" . " 4")("MAY" . " 5")("JUN" . " 6")
;;		 ("JUL" . " 7")("AUG" . " 8")("SEP" . " 9")
;;		 ("OCT" . "10")("NOV" . "11")("DEC" . "12")))
;;	(date (or date "")))
;;    ;; Can understand the following styles:
;;    ;; (1) 14 Apr 89 03:20:12 GMT
;;    ;; (2) Fri, 17 Mar 89 4:01:33 GMT
;;    (if (string-match
;;	 "\\([0-9]+\\) \\([^ ,]+\\) \\([0-9]+\\) \\([0-9:]+\\)" date)
;;	(concat
;;	 ;; Year
;;	 (substring date (match-beginning 3) (match-end 3))
;;	 ;; Month
;;	 (cdr
;;	  (assoc
;;	   (upcase (substring date (match-beginning 2) (match-end 2))) month))
;;	 ;; Day
;;	 (format "%2d" (string-to-int
;;			(substring date
;;				   (match-beginning 1) (match-end 1))))
;;	 ;; Time
;;	 (substring date (match-beginning 4) (match-end 4)))
;;      ;; Cannot understand DATE string.
;;      date
;;      )
;;    ))

(defun gnus-fetch-field (field)
  "Return the value of the header FIELD of current article."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (narrow-to-region (point-min)
			(progn (search-forward "\n\n" nil 'move) (point)))
      (mail-fetch-field field))))

(fset 'gnus-expunge 'gnus-summary-delete-marked-with)

(defun gnus-kill (field regexp &optional command all)
  "If FIELD of an article matches REGEXP, execute COMMAND.
Optional 1st argument COMMAND is default to
	(gnus-summary-mark-as-read nil \"X\").
If optional 2nd argument ALL is non-nil, articles marked are also applied to.
If FIELD is an empty string (or nil), entire article body is searched for.
COMMAND must be a Lisp expression or a string representing a key sequence."
  ;; We don't want to change current point nor window configuration.
  (save-excursion
    (save-window-excursion
      ;; Selected window must be Summary buffer to execute keyboard
      ;; macros correctly.  See command_loop_1.
      (switch-to-buffer gnus-summary-buffer 'norecord)
      (goto-char (point-min))		;From the beginning.
      (if (null command)
	  (setq command '(gnus-summary-mark-as-read nil "X")))
      (gnus-execute field regexp command nil (not all))
      )))

(defun gnus-execute (field regexp form &optional backward ignore-marked)
  "If FIELD of article header matches REGEXP, execute lisp FORM (or a string).
If FIELD is an empty string (or nil), entire article body is searched for.
If optional 1st argument BACKWARD is non-nil, do backward instead.
If optional 2nd argument IGNORE-MARKED is non-nil, ignore articles
marked as read or unread."
  (let ((function nil)
	(header nil)
	(article nil))
    (if (string-equal field "")
	(setq field nil))
    (if (null field)
	nil
      (or (stringp field)
	  (setq field (symbol-name field)))
      ;; Get access function of header filed.
      (setq function (intern-soft (concat "gnus-header-" (downcase field))))
      (if (and function (fboundp function))
	  (setq function (symbol-function function))
	(error "Unknown header field: \"%s\"" field)))
    ;; Make FORM funcallable.
    (if (and (listp form) (not (eq (car form) 'lambda)))
	(setq form (list 'lambda nil form)))
    ;; Starting from the current article.
    (or (and ignore-marked
	     ;; Articles marked as read and unread should be ignored.
	     (setq article (gnus-summary-article-number))
	     (or (not (memq article gnus-newsgroup-unreads)) ;Marked as read.
		 (memq article gnus-newsgroup-marked) ;Marked as unread.
		 ))
	(gnus-execute-1 function regexp form))
    (while (gnus-summary-search-subject backward ignore-marked nil)
      (gnus-execute-1 function regexp form))
    ))

(defun gnus-execute-1 (function regexp form)
  (save-excursion
    ;; The point of Summary buffer must be saved during execution.
    (let ((article (gnus-summary-article-number)))
      (if (null article)
	  nil				;Nothing to do.
	(if function
	    ;; Compare with header field.
	    (let (;;(header (gnus-find-header-by-number
		  ;;	    gnus-newsgroup-headers article))
		  (header (gnus-get-header-by-number article))
		  (value nil))
	      (and header
		   (progn
		     (setq value (funcall function header))
		     ;; Number (Lines:) or symbol must be converted to string.
		     (or (stringp value)
			 (setq value (prin1-to-string value)))
		     (string-match regexp value))
		   (if (stringp form)	;Keyboard macro.
		       (execute-kbd-macro form)
		     (funcall form))))
	  ;; Search article body.
	  (let ((gnus-current-article nil) ;Save article pointer.
		(gnus-last-article nil)
		(gnus-break-pages nil)	;No need to break pages.
		(gnus-mark-article-hook nil)) ;Inhibit marking as read.
	    (message "Searching for article: %d..." article)
	    (gnus-article-setup-buffer)
	    (gnus-article-prepare article t)
	    (if (save-excursion
		  (set-buffer gnus-article-buffer)
		  (goto-char (point-min))
		  (re-search-forward regexp nil t))
		(if (stringp form)	;Keyboard macro.
		    (execute-kbd-macro form)
		  (funcall form))))
	  ))
      )))

;;; caesar-region written by phr@prep.ai.mit.edu  Nov 86
;;; modified by tower@prep Nov 86
;;; Modified by umerin@flab.flab.Fujitsu.JUNET for ROT47.

(defun gnus-caesar-region (&optional n)
  "Caesar rotation of region by N, default 13, for decrypting netnews.
ROT47 will be performed for Japanese text in any case."
  (interactive (if current-prefix-arg	; Was there a prefix arg?
		   (list (prefix-numeric-value current-prefix-arg))
		 (list nil)))
  (cond ((not (numberp n)) (setq n 13))
	(t (setq n (mod n 26))))	;canonicalize N
  (if (not (zerop n))		; no action needed for a rot of 0
      (progn
	(if (or (not (boundp 'caesar-translate-table))
		(/= (aref caesar-translate-table ?a) (+ ?a n)))
	    (let ((i 0) (lower "abcdefghijklmnopqrstuvwxyz") upper)
	      (message "Building caesar-translate-table...")
	      (setq caesar-translate-table (make-vector 256 0))
	      (while (< i 256)
		(aset caesar-translate-table i i)
		(setq i (1+ i)))
	      (setq lower (concat lower lower) upper (upcase lower) i 0)
	      (while (< i 26)
		(aset caesar-translate-table (+ ?a i) (aref lower (+ i n)))
		(aset caesar-translate-table (+ ?A i) (aref upper (+ i n)))
		(setq i (1+ i)))
	      ;; ROT47 for Japanese text.
	      ;; Thanks to ichikawa@flab.fujitsu.junet.
	      (setq i 161)
	      (let ((t1 (logior ?O 128))
		    (t2 (logior ?! 128))
		    (t3 (logior ?~ 128)))
		(while (< i 256)
		  (aset caesar-translate-table i
			(let ((v (aref caesar-translate-table i)))
			  (if (<= v t1) (if (< v t2) v (+ v 47))
			    (if (<= v t3) (- v 47) v))))
		  (setq i (1+ i))))
	      (message "Building caesar-translate-table...done")))
	(let ((from (region-beginning))
	      (to (region-end))
	      (i 0) str len)
	  (setq str (buffer-substring from to))
	  (setq len (length str))
	  (while (< i len)
	    (aset str i (aref caesar-translate-table (aref str i)))
	    (setq i (1+ i)))
	  (goto-char from)
	  (delete-region from to)
	  (insert str)))))

;; Functions accessing headers.
;; Functions are more convenient than macros in some case.

(defun gnus-header-number (header)
  "Return article number in HEADER."
  (nntp-header-number header))

(defun gnus-header-subject (header)
  "Return subject string in HEADER."
  (nntp-header-subject header))

(defun gnus-header-from (header)
  "Return author string in HEADER."
  (nntp-header-from header))

(defun gnus-header-xref (header)
  "Return xref string in HEADER."
  (nntp-header-xref header))

(defun gnus-header-lines (header)
  "Return lines in HEADER."
  (nntp-header-lines header))

(defun gnus-header-date (header)
  "Return date in HEADER."
  (nntp-header-date header))

(defun gnus-header-id (header)
  "Return Id in HEADER."
  (nntp-header-id header))

(defun gnus-header-references (header)
  "Return references in HEADER."
  (nntp-header-references header))


;;;
;;; Article savers.
;;;

(defun gnus-output-to-rmail (file-name)
  "Append the current article to an Rmail file named FILE-NAME."
  (require 'rmail)
  ;; Most of these codes are borrowed from rmailout.el.
  (setq file-name (expand-file-name file-name))
  (setq rmail-default-rmail-file file-name)
  (let ((artbuf (current-buffer))
	(tmpbuf (get-buffer-create " *GNUS-output*")))
    (save-excursion
      (or (get-file-buffer file-name)
	  (file-exists-p file-name)
	  (if (yes-or-no-p
	       (concat "\"" file-name "\" does not exist, create it? "))
	      (let ((file-buffer (create-file-buffer file-name)))
		(save-excursion
		  (set-buffer file-buffer)
		  (rmail-insert-rmail-file-header)
		  (let ((require-final-newline nil))
		    (write-region (point-min) (point-max) file-name t 1)))
		(kill-buffer file-buffer))
	    (error "Output file does not exist")))
      (set-buffer tmpbuf)
      (buffer-flush-undo (current-buffer))
      (erase-buffer)
      (insert-buffer-substring artbuf)
      (gnus-convert-article-to-rmail)
      ;; Decide whether to append to a file or to an Emacs buffer.
      (let ((outbuf (get-file-buffer file-name)))
	(if (not outbuf)
	    (append-to-file (point-min) (point-max) file-name)
	  ;; File has been visited, in buffer OUTBUF.
	  (set-buffer outbuf)
	  (let ((buffer-read-only nil)
		(msg (and (boundp 'rmail-current-message)
			  rmail-current-message)))
	    ;; If MSG is non-nil, buffer is in RMAIL mode.
	    (if msg
		(progn (widen)
		       (narrow-to-region (point-max) (point-max))))
	    (insert-buffer-substring tmpbuf)
	    (if msg
		(progn
		  (goto-char (point-min))
		  (widen)
		  (search-backward "\^_")
		  (narrow-to-region (point) (point-max))
		  (goto-char (1+ (point-min)))
		  (rmail-count-new-messages t)
		  (rmail-show-message msg))))))
      )
    (kill-buffer tmpbuf)
    ))

(defun gnus-output-to-file (file-name)
  "Append the current article to a file named FILE-NAME."
  (setq file-name (expand-file-name file-name))
  (let ((artbuf (current-buffer))
	(tmpbuf (get-buffer-create " *GNUS-output*")))
    (save-excursion
      (set-buffer tmpbuf)
      (buffer-flush-undo (current-buffer))
      (erase-buffer)
      (insert-buffer-substring artbuf)
      ;; Append newline at end of the buffer as separator, and then
      ;; save it to file.
      (goto-char (point-max))
      (insert "\n")
      (append-to-file (point-min) (point-max) file-name))
    (kill-buffer tmpbuf)
    ))

(defun gnus-convert-article-to-rmail ()
  "Convert article in current buffer to Rmail message format."
  (let ((buffer-read-only nil))
    ;; Convert article directly into Babyl format.
    ;; Suggested by Rob Austein <sra@lcs.mit.edu>
    (goto-char (point-min))
    (insert "\^L\n0, unseen,,\n*** EOOH ***\n")
    (while (search-forward "\n\^_" nil t) ;single char
      (replace-match "\n^_"))		;2 chars: "^" and "_"
    (goto-char (point-max))
    (insert "\^_")))

;;(defun gnus-convert-article-to-rmail ()
;;  "Convert article in current buffer to Rmail message format."
;;  (let ((buffer-read-only nil))
;;    ;; Insert special header of Unix mail.
;;    (goto-char (point-min))
;;    (insert "From "
;;	    (or (mail-strip-quoted-names (mail-fetch-field "from"))
;;		"unknown")
;;	    " " (current-time-string) "\n")
;;    ;; Stop quoting `From' since this seems unnecessary in most cases.
;;    ;; ``Quote'' "\nFrom " as "\n>From "
;;    ;;(while (search-forward "\nFrom " nil t)
;;    ;;  (forward-char -5)
;;    ;;  (insert ?>))
;;    ;; Convert article to babyl format.
;;    (rmail-convert-to-babyl-format)
;;    ))


;;;
;;; Internal functions.
;;;

(defun gnus-start-news-server (&optional confirm)
  "Open network stream to remote NNTP server.
If optional argument CONFIRM is non-nil, ask you host that NNTP server
is running even if it is defined.
Run `gnus-open-server-hook' just before opening news server."
  (if (gnus-server-opened)
      ;; Stream is already opened.
      nil
    ;; Open NNTP server.
    (if (or confirm
	    (null gnus-nntp-server))
	;; If someone has set the service to nil, then this should always
	;; be the local host.
	(if gnus-nntp-service
	    (if (and (boundp 'gnus-secondary-servers) gnus-secondary-servers)
		;; Read server name with completion.
		(setq gnus-nntp-server
		      (completing-read "NNTP server: "
				       (cons (list gnus-nntp-server)
					     gnus-secondary-servers)
				       nil nil gnus-nntp-server))
	      (setq gnus-nntp-server
		    (read-string "NNTP server: " gnus-nntp-server)))
	  (setq gnus-nntp-server "")))
    ;; If no server name is given, local host is assumed.
    (if (or (string-equal gnus-nntp-server "")
	    (string-equal gnus-nntp-server "::")) ;RMS preference.
	(setq gnus-nntp-server (system-name)))
    ;; gnus-nntp-server must be either (system-name), ':DIRECTORY', or
    ;; nntp server name.  I mean '::' cannot be a value of
    ;; gnus-nntp-server.
    (cond ((and (null gnus-nntp-service)
		(string-equal gnus-nntp-server (system-name)))
	   (require 'nnspool)
	   (gnus-define-access-method 'nnspool)
	   (message "Looking up local news spool..."))
	  ((string-match ":" gnus-nntp-server)
	   ;; :DIRECTORY
	   (require 'mhspool)
	   (gnus-define-access-method 'mhspool)
	   (message "Looking up private directory..."))
	  (t
	   (gnus-define-access-method 'nntp)
	   (message "Connecting to NNTP server on %s..." gnus-nntp-server)))
    (run-hooks 'gnus-open-server-hook)
    (cond ((gnus-server-opened)		;Maybe opened in gnus-open-server-hook.
	   (message ""))
	  ((gnus-open-server gnus-nntp-server gnus-nntp-service)
	   (message ""))
	  (t
	   (error "%s"
	    (gnus-nntp-message
	     (format "Cannot open NNTP server on %s" gnus-nntp-server)))))
    ))

;; Dummy functions used only once.  Should return nil.
(defun gnus-server-opened () nil)
(defun gnus-close-server () nil)

(defun gnus-nntp-message (&optional message)
  "Return a message returned from NNTP server.
If no message is available and optional MESSAGE is given, return it."
  (let ((status (gnus-status-message))
	(message (or message "")))
    (if (and (stringp status)
	     (> (length status) 0))
	status message)))

(defun gnus-define-access-method (method &optional access-methods)
  "Define access functions for the access METHOD.
Methods definition is taken from optional argument ACCESS-METHODS or
the variable `gnus-access-methods'."
  (let ((bindings
	 (cdr (assoc method (or access-methods gnus-access-methods)))))
    (if (null bindings)
	(error "Unknown access method: %s" method)
      ;; Should not use symbol-function here since overload does not work.
      (while bindings
	;; Alist syntax is different from that of 3.14.3.
	(fset (car (car bindings)) (car (cdr (car bindings))))
	(setq bindings (cdr bindings)))
      )))

(defun gnus-select-newsgroup (group &optional show-all)
  "Select newsgroup GROUP.
If optional argument SHOW-ALL is non-nil, all of articles in the group
are selected."
  ;; Make sure a connection to NNTP server is alive.
  (gnus-start-news-server)
  (if (gnus-request-group group)
      (let ((articles nil))
	(setq gnus-newsgroup-name group)
	(setq gnus-newsgroup-unreads
	      (gnus-uncompress-sequence
	       (nthcdr 2 (gnus-gethash group gnus-unread-hashtb))))
	(cond (show-all
	       ;; Select all active articles.
	       (setq articles
		     (gnus-uncompress-sequence
		      (nthcdr 2 (gnus-gethash group gnus-active-hashtb)))))
	      (t
	       ;; Select unread articles only.
	       (setq articles gnus-newsgroup-unreads)))
	;; Require confirmation if selecting large newsgroup.
	(setq gnus-newsgroup-unselected nil)
	(if (not (numberp gnus-large-newsgroup))
	    nil
	  (let ((selected nil)
		(number (length articles)))
	    (if (> number gnus-large-newsgroup)
		(progn
		  (condition-case ()
		      (let ((input
			     (read-string
			      (format
			       "How many articles from %s (default %d): "
			       gnus-newsgroup-name number))))
			(setq selected
			      (if (string-equal input "")
				  number (string-to-int input))))
		    (quit
		     (setq selected 0)))
		  (cond ((and (> selected 0)
			      (< selected number))
			 ;; Select last N articles.
			 (setq articles (nthcdr (- number selected) articles)))
			((and (< selected 0)
			      (< (- 0 selected) number))
			 ;; Select first N articles.
			 (setq selected (- 0 selected))
			 (setq articles (copy-sequence articles))
			 (setcdr (nthcdr (1- selected) articles) nil))
			((zerop selected)
			 (setq articles nil))
			;; Otherwise select all.
			)
		  ;; Get unselected unread articles.
		  (setq gnus-newsgroup-unselected
			(gnus-set-difference gnus-newsgroup-unreads articles))
		  ))
	    ))
	;; Get headers list.
	(setq gnus-newsgroup-headers (gnus-retrieve-headers articles))
	;; UNREADS may contain expired articles, so we have to remove
	;;  them from the list.
	(setq gnus-newsgroup-unreads
	      (gnus-intersection gnus-newsgroup-unreads
				 (mapcar
				  (function
				   (lambda (header)
				     (nntp-header-number header)))
				  gnus-newsgroup-headers)))
	;; Marked article must be a subset of unread articles.
	(setq gnus-newsgroup-marked
	      (gnus-intersection (append gnus-newsgroup-unselected
					 gnus-newsgroup-unreads)
				 (cdr
				  (gnus-gethash group gnus-marked-hashtb))))
	;; First and last article in this newsgroup.
	(setq gnus-newsgroup-begin
	      (if gnus-newsgroup-headers
		  (nntp-header-number (car gnus-newsgroup-headers))
		0
		))
	(setq gnus-newsgroup-end
	      (if gnus-newsgroup-headers
		  (nntp-header-number
		   (gnus-last-element gnus-newsgroup-headers))
		0
		))
	;; File name that an article was saved last.
	(setq gnus-newsgroup-last-rmail nil)
	(setq gnus-newsgroup-last-mail nil)
	(setq gnus-newsgroup-last-folder nil)
	(setq gnus-newsgroup-last-file nil)
	;; Reset article pointer etc.
	(setq gnus-current-article nil)
	(setq gnus-current-headers nil)
	(setq gnus-current-history nil)
	(setq gnus-have-all-headers nil)
	(setq gnus-last-article nil)
	;; Clear old hash tables for the variable gnus-newsgroup-headers.
	(gnus-clear-hashtables-for-newsgroup-headers)
	;; GROUP is successfully selected.
	t
	)
    ))

;; Hacking for making header search much faster.

(defun gnus-get-header-by-number (number)
  "Return a header specified by a NUMBER.
If you update the variable `gnus-newsgroup-headers', you must set the
hash table `gnus-newsgroup-headers-hashtb-by-number' to nil to indicate
rehash is necessary."
  (or gnus-newsgroup-headers-hashtb-by-number
      (gnus-make-headers-hashtable-by-number))
  (gnus-gethash (int-to-string number)
		gnus-newsgroup-headers-hashtb-by-number))

(defun gnus-get-header-by-id (id)
  "Return a header specified by an ID.
If you update the variable `gnus-newsgroup-headers', you must set the
hash table `gnus-newsgroup-headers-hashtb-by-id' to nil to indicate
rehash is necessary."
  (or gnus-newsgroup-headers-hashtb-by-id
      (gnus-make-headers-hashtable-by-id))
  (and (stringp id)
       (gnus-gethash id gnus-newsgroup-headers-hashtb-by-id)))

(defun gnus-make-headers-hashtable-by-number ()
  "Make hashtable for the variable `gnus-newsgroup-headers' by number."
  (let ((header nil)
	(headers gnus-newsgroup-headers))
    (setq gnus-newsgroup-headers-hashtb-by-number
	  (gnus-make-hashtable (length headers)))
    (while headers
      (setq header (car headers))
      (gnus-sethash (int-to-string (nntp-header-number header))
		    header gnus-newsgroup-headers-hashtb-by-number)
      (setq headers (cdr headers))
      )))

(defun gnus-make-headers-hashtable-by-id ()
  "Make hashtable for the variable `gnus-newsgroup-headers' by id."
  (let ((header nil)
	(headers gnus-newsgroup-headers))
    (setq gnus-newsgroup-headers-hashtb-by-id
	  (gnus-make-hashtable (length headers)))
    (while headers
      (setq header (car headers))
      (gnus-sethash (nntp-header-id header)
		    header gnus-newsgroup-headers-hashtb-by-id)
      (setq headers (cdr headers))
      )))

(defun gnus-clear-hashtables-for-newsgroup-headers ()
  "Clear hash tables created for the variable `gnus-newsgroup-headers'."
  (setq gnus-newsgroup-headers-hashtb-by-id nil)
  (setq gnus-newsgroup-headers-hashtb-by-number nil))

(defun gnus-more-header-backward ()
  "Find new header backward."
  (let ((first
	 (car (nth 2 (gnus-gethash gnus-newsgroup-name gnus-active-hashtb))))
	(artnum gnus-newsgroup-begin)
	(header nil))
    (while (and (not header)
		(> artnum first))
      (setq artnum (1- artnum))
      (setq header (car (gnus-retrieve-headers (list artnum)))))
    header
    ))

(defun gnus-more-header-forward ()
  "Find new header forward."
  (let ((last
	 (cdr (nth 2 (gnus-gethash gnus-newsgroup-name gnus-active-hashtb))))
	(artnum gnus-newsgroup-end)
	(header nil))
    (while (and (not header)
		(< artnum last))
      (setq artnum (1+ artnum))
      (setq header (car (gnus-retrieve-headers (list artnum)))))
    header
    ))

(defun gnus-extend-newsgroup (header &optional backward)
  "Extend newsgroup selection with HEADER.
Optional argument BACKWARD means extend toward backward."
  (if header
      (let ((artnum (nntp-header-number header)))
	(setq gnus-newsgroup-headers
	      (if backward
		  (cons header gnus-newsgroup-headers)
		(append gnus-newsgroup-headers (list header))))
	;; Clear current hash tables for the variable gnus-newsgroup-headers.
	(gnus-clear-hashtables-for-newsgroup-headers)
	;; We have to update unreads and unselected, but don't have to
	;; care about gnus-newsgroup-marked.
	(if (memq artnum gnus-newsgroup-unselected)
	    (setq gnus-newsgroup-unreads
		  (cons artnum gnus-newsgroup-unreads)))
	(setq gnus-newsgroup-unselected
	      (delq artnum gnus-newsgroup-unselected))
	(setq gnus-newsgroup-begin (min gnus-newsgroup-begin artnum))
	(setq gnus-newsgroup-end (max gnus-newsgroup-end artnum))
	)))

(defun gnus-mark-article-as-read (article)
  "Remember that ARTICLE is marked as read."
  ;; Remove from unread and marked list.
  (setq gnus-newsgroup-unreads
	(delq article gnus-newsgroup-unreads))
  (setq gnus-newsgroup-marked
	(delq article gnus-newsgroup-marked)))

(defun gnus-mark-article-as-unread (article &optional clear-mark)
  "Remember that ARTICLE is marked as unread.
Optional argument CLEAR-MARK means ARTICLE should not be remembered
that it was marked as read once."
  ;; Add to unread list.
  (or (memq article gnus-newsgroup-unreads)
      (setq gnus-newsgroup-unreads
	    (cons article gnus-newsgroup-unreads)))
  ;; If CLEAR-MARK is non-nil, the article must be removed from marked
  ;; list.  Otherwise, it must be added to the list.
  (if clear-mark
      (setq gnus-newsgroup-marked
	    (delq article gnus-newsgroup-marked))
    (or (memq article gnus-newsgroup-marked)
	(setq gnus-newsgroup-marked
	      (cons article gnus-newsgroup-marked)))))

(defun gnus-clear-system ()
  "Clear all variables and buffer."
  ;; Clear GNUS variables.
  (let ((variables gnus-variable-list))
    (while variables
      (set (car variables) nil)
      (setq variables (cdr variables))))
  ;; Clear other internal variables.
  (setq gnus-newsrc-hashtb nil)
  (setq gnus-marked-hashtb nil)
  (setq gnus-killed-hashtb nil)
  (setq gnus-active-hashtb nil)
  (setq gnus-octive-hashtb nil)
  (setq gnus-unread-hashtb nil)
  (setq gnus-newsgroup-headers nil)
  (setq gnus-newsgroup-headers-hashtb-by-id nil)
  (setq gnus-newsgroup-headers-hashtb-by-number nil)
  ;; Kill the startup file.
  (and gnus-current-startup-file
       (get-file-buffer gnus-current-startup-file)
       (kill-buffer (get-file-buffer gnus-current-startup-file)))
  (setq gnus-current-startup-file nil)
  ;; Kill GNUS buffers.
  (let ((buffers gnus-buffer-list))
    (while buffers
      (if (get-buffer (car buffers))
	  (kill-buffer (car buffers)))
      (setq buffers (cdr buffers))
      )))

(defun gnus-configure-windows (action)
  "Configure GNUS windows according to the next ACTION.
The ACTION is either a symbol, such as `summary', or a
configuration list such as `(1 1 2)'.  If ACTION is not a list,
configuration list is got from the variable `gnus-window-configuration'."
  (let* ((windows
	  (if (listp action)
	      action (car (cdr (assq action gnus-window-configuration)))))
	 (grpwin (get-buffer-window gnus-group-buffer))
	 (subwin (get-buffer-window gnus-summary-buffer))
	 (artwin (get-buffer-window gnus-article-buffer))
	 (winsum nil)
	 (height nil)
	 (grpheight 0)
	 (subheight 0)
	 (artheight 0)
	 ;; Make split-window-vertically leave focus in upper window.
	 (split-window-keep-point t))
    (if (or (null windows)		;No configuration is specified.
	    (and (eq (null grpwin)
		     (zerop (nth 0 windows)))
		 (eq (null subwin)
		     (zerop (nth 1 windows)))
		 (eq (null artwin)
		     (zerop (nth 2 windows)))))
	;; No need to change window configuration.
	nil
      (select-window (or grpwin subwin artwin (selected-window)))
      ;; First of all, compute the height of each window.
      (cond (gnus-use-full-window
	     ;; Take up the entire screen.
	     (delete-other-windows)
	     (setq height (window-height (selected-window))))
	    (t
	     (setq height (+ (if grpwin (window-height grpwin) 0)
			     (if subwin (window-height subwin) 0)
			     (if artwin (window-height artwin) 0)))))
      ;; The Newsgroup buffer exits always.  So, use it to extend the
      ;; Group window so as to get enough window space.
      (switch-to-buffer gnus-group-buffer 'norecord)
      (and (get-buffer gnus-summary-buffer)
	   (delete-windows-on gnus-summary-buffer))
      (and (get-buffer gnus-article-buffer)
	   (delete-windows-on gnus-article-buffer))
      ;; Compute expected window height.
      (setq winsum (apply (function +) windows))
      (if (not (zerop (nth 0 windows)))
	  (setq grpheight (max window-min-height
			       (/ (* height (nth 0 windows)) winsum))))
      (if (not (zerop (nth 1 windows)))
	  (setq subheight (max window-min-height
			       (/ (* height (nth 1 windows)) winsum))))
      (if (not (zerop (nth 2 windows)))
	  (setq artheight (max window-min-height
			       (/ (* height (nth 2 windows)) winsum))))
      (setq height (+ grpheight subheight artheight))
      (enlarge-window (max 0 (- height (window-height (selected-window)))))
      ;; Then split the window.
      (and (not (zerop artheight))
	   (or (not (zerop grpheight))
	       (not (zerop subheight)))
	   (split-window-vertically (+ grpheight subheight)))
      (and (not (zerop grpheight))
	   (not (zerop subheight))
	   (split-window-vertically grpheight))
      ;; Then select buffers in each window.
      (and (not (zerop grpheight))
	   (progn
	     (switch-to-buffer gnus-group-buffer 'norecord)
	     (other-window 1)))
      (and (not (zerop subheight))
	   (progn
	     (switch-to-buffer gnus-summary-buffer 'norecord)
	     (other-window 1)))
      (and (not (zerop artheight))
	   (progn
	     ;; If Article buffer does not exist, it will be created
	     ;; and initialized.
	     (gnus-article-setup-buffer)
	     (switch-to-buffer gnus-article-buffer 'norecord)))
      )
    ))

(defun gnus-find-header-by-number (headers number)
  "Return a header which is a element of HEADERS and has NUMBER."
  (let ((found nil))
    (while (and headers (not found))
      ;; We cannot use `=' to accept non-numeric NUMBER.
      (if (eq number (nntp-header-number (car headers)))
	  (setq found (car headers)))
      (setq headers (cdr headers)))
    found
    ))

(defun gnus-find-header-by-id (headers id)
  "Return a header which is a element of HEADERS and has Message-ID."
  (let ((found nil))
    (while (and headers (not found))
      (if (string-equal id (nntp-header-id (car headers)))
	  (setq found (car headers)))
      (setq headers (cdr headers)))
    found
    ))

(defun gnus-version ()
  "Version numbers of this version of GNUS."
  (interactive)
  (cond ((and (boundp 'mhspool-version) (boundp 'nnspool-version))
	 (message "%s; %s; %s; %s"
		  gnus-version nntp-version nnspool-version mhspool-version))
	((boundp 'mhspool-version)
	 (message "%s; %s; %s"
		  gnus-version nntp-version mhspool-version))
	((boundp 'nnspool-version)
	 (message "%s; %s; %s"
		  gnus-version nntp-version nnspool-version))
	(t
	 (message "%s; %s" gnus-version nntp-version))))

(defun gnus-info-find-node ()
  "Find Info documentation of GNUS."
  (interactive)
  (require 'info)
  ;; Enlarge info window if needed.
  (cond ((eq major-mode 'gnus-group-mode)
	 (gnus-configure-windows '(1 0 0)) ;Take all windows.
	 (pop-to-buffer gnus-group-buffer))
	((eq major-mode 'gnus-summary-mode)
	 (gnus-configure-windows '(0 1 0)) ;Take all windows.
	 (pop-to-buffer gnus-summary-buffer)))
  (Info-goto-node (car (cdr (assq major-mode gnus-info-nodes)))))

(defun gnus-overload-functions (&optional overloads)
  "Overload functions specified by optional argument OVERLOADS.
If nothing is specified, use the variable `gnus-overload-functions'."
  (let ((defs nil)
	(overloads (or overloads gnus-overload-functions)))
    (while overloads
      (setq defs (car overloads))
      (setq overloads (cdr overloads))
      ;; Load file before overloading function if necessary.  Make
      ;; sure we cannot use `require' always.
      (and (not (fboundp (car defs)))
	   (car (cdr (cdr defs)))
	   (load (car (cdr (cdr defs))) nil 'nomessage))
      (fset (car defs) (car (cdr defs)))
      )))

(defun gnus-make-threads (newsgroup-headers)
  "Make conversation threads tree from NEWSGROUP-HEADERS."
  (let ((headers newsgroup-headers)
	(refer nil)
	(h nil)
	(d nil)
	(roots nil)
	(dependencies nil))
    ;; Make message dependency alist.
    (while headers
      (setq h (car headers))
      (setq headers (cdr headers))
      ;; Ignore invalid headers.
      (if (vectorp h)			;Depends on nntp.el.
	  (progn
	    ;; Ignore broken references, e.g "<123@a.b.c".
	    (setq refer (nntp-header-references h))
	    (setq d (and refer
			 (string-match "\\(<[^<>]+>\\)[^>]*$" refer)
;;			 (gnus-find-header-by-id
;;			  newsgroup-headers
;;			  (substring refer (match-beginning 1) (match-end 1)))
			 ;; In fact if the variable newsgroup-headers
			 ;; is not 'equal' to the variable
			 ;; gnus-newsgroup-headers, the following
			 ;; function call may return bogus value.
			 (gnus-get-header-by-id
			  (substring refer (match-beginning 1) (match-end 1)))
			 ))
	    ;; Check subject equality.
	    (or gnus-thread-ignore-subject
		(null d)
		(string-equal (gnus-simplify-subject
			       (nntp-header-subject h) 're)
			      (gnus-simplify-subject
			       (nntp-header-subject d) 're))
		;; H should be a thread root.
		(setq d nil))
	    ;; H depends on D.
	    (setq dependencies
		  (cons (cons h d) dependencies))
	    ;; H is a thread root.
	    (if (null d)
		(setq roots (cons h roots)))
	    ))
      )
    ;; Make complete threads from the roots.
    ;; Note: dependencies are in reverse order, but
    ;; gnus-make-threads-1 processes it in reverse order again.  So,
    ;; we don't have to worry about it.
    (mapcar
     (function
      (lambda (root)
	(gnus-make-threads-1 root dependencies))) (nreverse roots))
    ))

(defun gnus-make-threads-1 (parent dependencies)
  (let ((children nil)
	(d nil)
	(depends dependencies))
    ;; Find children.
    (while depends
      (setq d (car depends))
      (setq depends (cdr depends))
      (and (cdr d)
	   (eq (nntp-header-id parent) (nntp-header-id (cdr d)))
	   (setq children (cons (car d) children))))
    ;; Go down.
    (cons parent
	  (mapcar
	   (function
	    (lambda (child)
	      (gnus-make-threads-1 child dependencies))) children))
    ))

(defun gnus-narrow-to-page (&optional arg)
  "Make text outside current page invisible except for page delimiter.
A numeric arg specifies to move forward or backward by that many pages,
thus showing a page other than the one point was originally in."
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) 0))
  (save-excursion
    (forward-page -1)			;Beginning of current page.
    (widen)
    (if (> arg 0)
	(forward-page arg)
      (if (< arg 0)
	  (forward-page (1- arg))))
    ;; Find the end of the page.
    (forward-page)
    ;; If we stopped due to end of buffer, stay there.
    ;; If we stopped after a page delimiter, put end of restriction
    ;; at the beginning of that line.
    ;; These are commented out.
    ;;    (if (save-excursion (beginning-of-line)
    ;;			(looking-at page-delimiter))
    ;;	(beginning-of-line))
    (narrow-to-region (point)
		      (progn
			;; Find the top of the page.
			(forward-page -1)
			;; If we found beginning of buffer, stay there.
			;; If extra text follows page delimiter on same line,
			;; include it.
			;; Otherwise, show text starting with following line.
			(if (and (eolp) (not (bobp)))
			    (forward-line 1))
			(point)))
    ))

;; Create hash table for alist, such as gnus-newsrc-assoc,
;; gnus-killed-assoc, and gnus-marked-assoc.

(defun gnus-make-hashtable-from-alist (alist &optional hashsize)
  "Return hash table for ALIST.
Optional argument HASHSIZE specifies the hashtable size.
Hash key is a car of alist element, which must be a string."
  (let ((hashtb (gnus-make-hashtable (or hashsize (length alist)))))
    (while alist
      (gnus-sethash (car (car alist))	;Newsgroup name
		    (car alist)		;Alist element
		    hashtb)
      (setq alist (cdr alist)))
    hashtb
    ))

(defun gnus-last-element (list)
  "Return last element of LIST."
  (let ((last nil))
    (while list
      (if (null (cdr list))
	  (setq last (car list)))
      (setq list (cdr list)))
    last
    ))

(defun gnus-set-difference (list1 list2)
  "Return a list of elements of LIST1 that do not appear in LIST2."
  (let ((list1 (copy-sequence list1)))
    (while list2
      (setq list1 (delq (car list2) list1))
      (setq list2 (cdr list2)))
    list1
    ))

(defun gnus-intersection (list1 list2)
  "Return a list of elements that appear in both LIST1 and LIST2."
  (let ((result nil))
    (while list2
      (if (memq (car list2) list1)
	  (setq result (cons (car list2) result)))
      (setq list2 (cdr list2)))
    result
    ))


;;;
;;; Get information about active articles, already read articles, and
;;;  still unread articles.
;;;

;; GNUS internal format of gnus-newsrc-assoc and gnus-killed-assoc:
;; (("general" t (1 . 1))
;;  ("misc"    t (1 . 10) (12 . 15))
;;  ("test"  nil (1 . 99)) ...)
;; GNUS internal format of gnus-marked-assoc:
;; (("general" 1 2 3)
;;  ("misc" 2) ...)
;; GNUS internal format of gnus-active-hashtb:
;; (("general" t (1 . 1))
;;  ("misc"    t (1 . 10))
;;  ("test"  nil (1 . 99)) ...)
;; GNUS internal format of gnus-unread-hashtb:
;; (("general" 1 (1 . 1))
;;  ("misc"   14 (1 . 10) (12 . 15))
;;  ("test"   99 (1 . 99)) ...)

(defun gnus-setup-news (&optional rawfile)
  "Setup news information.
If optional argument RAWFILE is non-nil, force to read raw startup file."
  (let ((init (not (and gnus-newsrc-assoc
			gnus-active-hashtb
			gnus-unread-hashtb
			(not rawfile)
			))))
    ;; We have to clear some variables to re-initialize news info.
    (if init
	(setq gnus-newsrc-assoc nil
	      gnus-active-hashtb nil
	      gnus-unread-hashtb nil))
    (gnus-read-active-file)
    ;; Initialize only once.
    (if init
	(progn
	  ;; Get distributions only once.
	  (gnus-read-distributions-file)
	  ;; newsrc file must be read after reading active file since
	  ;; its size is used to guess the size of gnus-newsrc-hashtb.
	  (gnus-read-newsrc-file rawfile)
	  ))
    (gnus-expire-marked-articles)
    (gnus-get-unread-articles)
    
    ;; newsgroups description
    (if gnus-newsgroups-display
	(if (not gnus-newsgroups-alist)
	    ;; Get newsgroups file only once.
	    (gnus-newsgroups-retrieve-description)))
	
    (setq gnus-newsgroups-hashtb (gnus-make-hashtable-from-alist gnus-newsgroups-alist))
    
    ;; Check new newsgroups and subscribe them.
    (if init
	(let ((new-newsgroups (gnus-find-new-newsgroups)))
	  (while new-newsgroups
	    (funcall gnus-subscribe-newsgroup-method (car new-newsgroups))
	    (setq new-newsgroups (cdr new-newsgroups))
	    )))
    ))

(defun gnus-add-newsgroup (newsgroup)
  "Subscribe new NEWSGROUP safely and put it at top."
  (and (null (gnus-gethash newsgroup gnus-newsrc-hashtb)) ;Really new?
       (gnus-gethash newsgroup gnus-active-hashtb) ;Really exist?
       (gnus-insert-newsgroup (or (gnus-gethash newsgroup gnus-killed-hashtb)
				  (list newsgroup t))
			      (car (car gnus-newsrc-assoc)))))

(defun gnus-find-new-newsgroups ()
  "Looking for new newsgroups and return names.
`-n' option of options line in `.newsrc' file is recognized."
  (let ((group nil)
	(new-newsgroups nil))
    (mapatoms
     (function
      (lambda (sym)
	(setq group (symbol-name sym))
	;; Taking account of `-n' option.
	(and (or (null gnus-newsrc-options-n-no)
		 (not (string-match gnus-newsrc-options-n-no group))
		 (and gnus-newsrc-options-n-yes
		      (string-match gnus-newsrc-options-n-yes group)))
	     (null (gnus-gethash group gnus-killed-hashtb)) ;Ignore killed.
	     (null (gnus-gethash group gnus-newsrc-hashtb)) ;Really new.
	     ;; Find new newsgroup.
	     (setq new-newsgroups
		   (cons group new-newsgroups)))
	))
     gnus-active-hashtb)
    ;; Return new newsgroups.
    new-newsgroups
    ))

(defun gnus-kill-newsgroup (group)
  "Kill GROUP from `gnus-newsrc-assoc', `.newsrc' and `gnus-unread-hashtb'."
  (let ((info (gnus-gethash group gnus-newsrc-hashtb)))
    (if (null info)
	nil
      ;; Delete from gnus-newsrc-assoc and gnus-newsrc-hashtb.
      (setq gnus-newsrc-assoc (delq info gnus-newsrc-assoc))
      (gnus-sethash group nil gnus-newsrc-hashtb)
      ;; Add to gnus-killed-assoc and gnus-killed-hashtb.
      (setq gnus-killed-assoc
	    (cons info
		  (delq (gnus-gethash group gnus-killed-hashtb)
			gnus-killed-assoc)))
      (gnus-sethash group info gnus-killed-hashtb)
      ;; Clear unread hashtable.
      ;; Thanks cwitty@csli.Stanford.EDU (Carl Witty).
      (gnus-sethash group nil gnus-unread-hashtb)
      ;; Then delete from .newsrc
      (gnus-update-newsrc-buffer group 'delete)
      ;; Return the deleted newsrc entry.
      info
      )))

(defun gnus-insert-newsgroup (info &optional next)
  "Insert newsrc INFO entry before NEXT.
If optional argument NEXT is nil, appended to the last."
  (if (null info)
      (error "Invalid argument: %s" info))
  (let* ((group (car info))		;Newsgroup name.
	 (range
	  (gnus-difference-of-range
	   (nth 2 (gnus-gethash group gnus-active-hashtb)) (nthcdr 2 info))))
    ;; Check duplication.
    (if (gnus-gethash group gnus-newsrc-hashtb)
	(error "Duplicated: %s" group))
    ;; Insert to gnus-newsrc-assoc and gnus-newsrc-hashtb.
    (if (string-equal next (car (car gnus-newsrc-assoc)))
	(setq gnus-newsrc-assoc
	      (cons info gnus-newsrc-assoc))
      (let ((found nil)
	    (rest (cdr gnus-newsrc-assoc))
	    (tail gnus-newsrc-assoc))
	;; Seach insertion point.
	(while (and (not found) rest)
	  (if (string-equal next (car (car rest)))
	      (setq found t)
	    (setq rest (cdr rest))
	    (setq tail (cdr tail))
	    ))
	;; Find it.
	(if (consp tail)
	    (setcdr tail (cons info rest))
	  ;; gnus-newsrc-assoc must be nil.
	  (setq gnus-newsrc-assoc
		(append gnus-newsrc-assoc (cons info rest))))
	))
    (gnus-sethash group info gnus-newsrc-hashtb)
    ;; Delete from gnus-killed-assoc and gnus-killed-hashtb.
    (setq gnus-killed-assoc
	  (delq (gnus-gethash group gnus-killed-hashtb) gnus-killed-assoc))
    (gnus-sethash group nil gnus-killed-hashtb)
    ;; Then insert to .newsrc.
    (gnus-update-newsrc-buffer group nil next)
    ;; Add to gnus-unread-hashtb.
    (gnus-sethash group
		  (cons group		;Newsgroup name.
			(cons (gnus-number-of-articles range) range))
		  gnus-unread-hashtb)
    ))

(defun gnus-check-killed-newsgroups ()
  "Update `gnus-killed-assoc' based on `gnus-newsrc-assoc'.
Update `gnus-killed-hashtb' also."
  (let ((group nil)
	(new-killed nil)
	(old-killed gnus-killed-assoc))
    (while old-killed
      (setq group (car (car old-killed)))
      (and (or (null gnus-newsrc-options-n-no)
	       (not (string-match gnus-newsrc-options-n-no group))
	       (and gnus-newsrc-options-n-yes
		    (string-match gnus-newsrc-options-n-yes group)))
	   (null (gnus-gethash group gnus-newsrc-hashtb)) ;No duplication.
	   ;; Subscribed in options line and not in gnus-newsrc-assoc.
	   (setq new-killed
		 (cons (car old-killed) new-killed)))
      (setq old-killed (cdr old-killed))
      )
    (setq gnus-killed-assoc (nreverse new-killed))
    (setq gnus-killed-hashtb
	  (gnus-make-hashtable-from-alist gnus-killed-assoc))
    ))

(defun gnus-check-bogus-newsgroups (&optional confirm)
  "Delete bogus newsgroups.
If optional argument CONFIRM is non-nil, confirm deletion of newsgroups."
  (let ((group nil)			;Newsgroup name temporary used.
	(old-newsrc gnus-newsrc-assoc)
	(new-newsrc nil)
	(bogus nil)			;List of bogus newsgroups.
	(old-killed gnus-killed-assoc)
	(new-killed nil)
	(old-marked gnus-marked-assoc)
	(new-marked nil))
    (message "Checking bogus newsgroups...")
    ;; Update gnus-newsrc-assoc and gnus-newsrc-hashtb.
    (while old-newsrc
      (setq group (car (car old-newsrc)))
      (if (or (gnus-gethash group gnus-active-hashtb)
	      (and confirm
		   (not (y-or-n-p
			 (format "Delete bogus newsgroup: %s " group)))))
	  ;; Active newsgroup.
	  (setq new-newsrc (cons (car old-newsrc) new-newsrc))
	;; Found a bogus newsgroup.
	(setq bogus (cons group bogus)))
      (setq old-newsrc (cdr old-newsrc))
      )
    (setq gnus-newsrc-assoc (nreverse new-newsrc))
    (setq gnus-newsrc-hashtb
	  (gnus-make-hashtable-from-alist gnus-newsrc-assoc))
    ;; Update gnus-killed-assoc and gnus-killed-hashtb.
    ;; The killed newsgroups are deleted without any confirmations.
    (while old-killed
      (setq group (car (car old-killed)))
      (and (gnus-gethash group gnus-active-hashtb)
	   (null (gnus-gethash group gnus-newsrc-hashtb))
	   ;; Active and really killed newsgroup.
	   (setq new-killed (cons (car old-killed) new-killed)))
      (setq old-killed (cdr old-killed))
      )
    (setq gnus-killed-assoc (nreverse new-killed))
    (setq gnus-killed-hashtb
	  (gnus-make-hashtable-from-alist gnus-killed-assoc))
    ;; Remove BOGUS from .newsrc file.
    (while bogus
      (gnus-update-newsrc-buffer (car bogus) 'delete)
      (setq bogus (cdr bogus)))
    ;; Update gnus-marked-assoc and gnus-marked-hashtb.
    (while old-marked
      (setq group (car (car old-marked)))
      (if (and (cdr (car old-marked))	;Non-empty?
	       (gnus-gethash group gnus-newsrc-hashtb))	;Not bogus?
	  (setq new-marked (cons (car old-marked) new-marked)))
      (setq old-marked (cdr old-marked)))
    (setq gnus-marked-assoc new-marked)
    (setq gnus-marked-hashtb
	  (gnus-make-hashtable-from-alist gnus-marked-assoc))
    (message "Checking bogus newsgroups...done")
    ))

(defun gnus-get-unread-articles ()
  "Compute diffs between active and read articles."
  (let ((read gnus-newsrc-assoc)
	(group-info nil)
	(group-name nil)
	(active nil)
	(range nil))
    (message "Checking new news...")
    (or gnus-unread-hashtb
	(setq gnus-unread-hashtb
	      (gnus-make-hashtable (length gnus-active-hashtb))))
    (while read
      (setq group-info (car read))	;About one newsgroup
      (setq group-name (car group-info))
      (setq active (nth 2 (gnus-gethash group-name gnus-active-hashtb)))
      (if (and gnus-octive-hashtb
	       ;; Is nothing changed?
	       (equal active
		      (nth 2 (gnus-gethash group-name gnus-octive-hashtb)))
	       ;; Is this newsgroup in the unread hash table?
	       (gnus-gethash group-name gnus-unread-hashtb)
	       )
	  nil				;Nothing to do.
	(setq range (gnus-difference-of-range active (nthcdr 2 group-info)))
	(gnus-sethash group-name
		      (cons group-name	;Group name
			    (cons (gnus-number-of-articles range)
				  range)) ;Range of unread articles
		      gnus-unread-hashtb)
	)
      (setq read (cdr read))
      )
    (message "Checking new news...done")
    ))

(defun gnus-expire-marked-articles ()
  "Check expired article which is marked as unread."
  (let ((marked-assoc gnus-marked-assoc)
	(updated-assoc nil)
	(marked nil)			;Current marked info.
	(articles nil)			;List of marked articles.
	(updated nil)			;List of real marked.
	(begin nil))
    (while marked-assoc
      (setq marked (car marked-assoc))
      (setq articles (cdr marked))
      (setq updated nil)
      (setq begin
	    (car (nth 2 (gnus-gethash (car marked) gnus-active-hashtb))))
      (while (and begin articles)
	(if (>= (car articles) begin)
	    ;; This article is still active.
	    (setq updated (cons (car articles) updated)))
	(setq articles (cdr articles)))
      (if updated
	  (setq updated-assoc
		(cons (cons (car marked) updated) updated-assoc)))
      (setq marked-assoc (cdr marked-assoc)))
    (setq gnus-marked-assoc updated-assoc)
    (setq gnus-marked-hashtb
	  (gnus-make-hashtable-from-alist gnus-marked-assoc))
    ))

(defun gnus-mark-as-read-by-xref
  (group headers unreads &optional subscribed-only)
  "Mark articles as read using cross references and return updated newsgroups.
Arguments are GROUP, HEADERS, UNREADS, and optional SUBSCRIBED-ONLY."
  (let ((xref-list nil)
	(header nil)
	(xrefs nil)			;One Xref: field info.
	(xref nil)			;(NEWSGROUP . ARTICLE)
	(gname nil)			;Newsgroup name
	(article nil))			;Article number
    (while headers
      (setq header (car headers))
      (if (memq (nntp-header-number header) unreads)
	  ;; This article is not yet marked as read.
	  nil
	(setq xrefs (gnus-parse-xref-field (nntp-header-xref header)))
	;; For each cross reference info. in one Xref: field.
	(while xrefs
	  (setq xref (car xrefs))
	  (setq gname (car xref))	;Newsgroup name
	  (setq article (cdr xref))	;Article number
	  (or (string-equal group gname) ;Ignore current newsgroup.
	      ;; Ignore unsubscribed newsgroup if requested.
	      (and subscribed-only
		   (not (nth 1 (gnus-gethash gname gnus-newsrc-hashtb))))
	      ;; Ignore article marked as unread.
	      (memq article (cdr (gnus-gethash gname gnus-marked-hashtb)))
	      (let ((group-xref (assoc gname xref-list)))
		(if group-xref
		    (if (memq article (cdr group-xref))
			nil		;Alread marked.
		      (setcdr group-xref (cons article (cdr group-xref))))
		  ;; Create new assoc entry for GROUP.
		  (setq xref-list (cons (list gname article) xref-list)))
		))
	  (setq xrefs (cdr xrefs))
	  ))
      (setq headers (cdr headers)))
    ;; Mark cross referenced articles as read.
    (gnus-mark-xrefed-as-read xref-list)
    ;;(message "%s %s" (prin1-to-string unreads) (prin1-to-string xref-list))
    ;; Return list of updated group name.
    (mapcar (function car) xref-list)
    ))

(defun gnus-parse-xref-field (xref-value)
  "Parse Xref: field value, and return list of `(group . article-id)'."
  (let ((xref-list nil)
	(xref-value (or xref-value "")))
    ;; Remove server host name.
    (if (string-match "^[ \t]*[^ \t,]+[ \t,]+\\(.*\\)$" xref-value)
	(setq xref-value (substring xref-value (match-beginning 1)))
      (setq xref-value nil))
    ;; Process each xref info.
    (while xref-value
      (if (string-match
	   "^[ \t,]*\\([^ \t,]+\\):\\([0-9]+\\)[^0-9]*" xref-value)
	  (progn
	    (setq xref-list
		  (cons
		   (cons
		    ;; Group name
		    (substring xref-value (match-beginning 1) (match-end 1))
		    ;; Article-ID
		    (string-to-int
		     (substring xref-value (match-beginning 2) (match-end 2))))
		   xref-list))
	    (setq xref-value (substring xref-value (match-end 2))))
	(setq xref-value nil)))
    ;; Return alist.
    xref-list
    ))

(defun gnus-mark-xrefed-as-read (xrefs)
  "Update unread article information using XREFS alist."
  (let ((group nil)
	(idlist nil)
	(unread nil))
    (while xrefs
      (setq group (car (car xrefs)))
      (setq idlist (cdr (car xrefs)))
      (setq unread (gnus-uncompress-sequence
		    (nthcdr 2 (gnus-gethash group gnus-unread-hashtb))))
      (while idlist
	(setq unread (delq (car idlist) unread))
	(setq idlist (cdr idlist)))
      (gnus-update-unread-articles group unread 'ignore)
      (setq xrefs (cdr xrefs))
      )))

(defun gnus-update-unread-articles (group unread-list marked-list)
  "Update unread articles of GROUP using UNREAD-LIST and MARKED-LIST."
  (let ((active (nth 2 (gnus-gethash group gnus-active-hashtb)))
	(unread (gnus-gethash group gnus-unread-hashtb)))
    (if (or (null active) (null unread))
	;; Ignore unknown newsgroup.
	nil
      ;; Update gnus-unread-hashtb.
      (if unread-list
	  (setcdr (cdr unread)
		  (gnus-compress-sequence unread-list))
	;; All of the articles are read.
	(setcdr (cdr unread) '((0 . 0))))
      ;; Number of unread articles.
      (setcar (cdr unread)
	      (gnus-number-of-articles (nthcdr 2 unread)))
      ;; Update gnus-newsrc-assoc.
      (if (> (car active) 0)
	  ;; Articles from 1 to N are not active.
	  (setq active (cons 1 (cdr active))))
      (setcdr (cdr (gnus-gethash group gnus-newsrc-hashtb))
	      (gnus-difference-of-range active (nthcdr 2 unread)))
      ;; Update .newsrc buffer.
      (gnus-update-newsrc-buffer group)
      ;; Update gnus-marked-assoc.
      (if (listp marked-list)		;Includes NIL.
	  (let ((marked (gnus-gethash group gnus-marked-hashtb)))
	    (cond (marked		;There is an entry.
		   (setcdr marked marked-list))
		  (marked-list		;Non-NIL.
		   (let ((info (cons group marked-list)))
		     ;; hashtb must share the same cons cell.
		     (setq gnus-marked-assoc
			   (cons info gnus-marked-assoc))
		     (gnus-sethash group info gnus-marked-hashtb)
		     ))
		  )))
      )))

(defun gnus-read-active-file ()
  "Get active file from NNTP server."
  ;; Make sure a connection to NNTP server is alive.
  (gnus-start-news-server)
  (message "Reading active file...")
  (if (gnus-request-list)		;Get active file from server
      (save-excursion
	(set-buffer nntp-server-buffer)
	(gnus-active-to-gnus-format)
	(message "Reading active file...done"))
    (error "Cannot read active file from NNTP server.")))

(defun gnus-active-to-gnus-format ()
  "Convert active file format to internal format.
Lines matching `gnus-ignored-newsgroups' are ignored."
  ;; Delete unnecessary lines.
  (goto-char (point-min))
  ;;(delete-matching-lines "^to\\..*$")
  (delete-matching-lines gnus-ignored-newsgroups)
  ;; Save OLD active info.
  (setq gnus-octive-hashtb gnus-active-hashtb)
  ;; Make large enough hash table.
  (setq gnus-active-hashtb
	(gnus-make-hashtable (count-lines (point-min) (point-max))))
  ;; Store active file in hashtable.
  (goto-char (point-min))
  (while
      (re-search-forward
       "^\\([^ \t]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([ymn]\\).*$"
       nil t)
    (gnus-sethash
     (buffer-substring (match-beginning 1) (match-end 1))
     (list (buffer-substring (match-beginning 1) (match-end 1))
	   (string-equal
	    "y" (buffer-substring (match-beginning 4) (match-end 4)))
	   (cons (string-to-int
		  (buffer-substring (match-beginning 3) (match-end 3)))
		 (string-to-int
		  (buffer-substring (match-beginning 2) (match-end 2)))))
     gnus-active-hashtb)
    ))

(defun gnus-read-newsrc-file (&optional rawfile)
  "Read startup FILE.
If optional argument RAWFILE is non-nil, the raw startup file is read."
  (setq gnus-current-startup-file (gnus-make-newsrc-file gnus-startup-file))
  ;; Reset variables which may be included in the quick startup file.
  (let ((variables gnus-variable-list))
    (while variables
      (set (car variables) nil)
      (setq variables (cdr variables))))
  (let* ((newsrc-file gnus-current-startup-file)
	 (quick-file (concat newsrc-file ".el"))
	 (quick-loaded nil))
    (save-excursion
      ;; Prepare .newsrc buffer.
      (set-buffer (find-file-noselect newsrc-file))
      ;; It is not so good idea turning off undo.
      ;;(buffer-flush-undo (current-buffer))
      ;; Load quick .newsrc to restore gnus-marked-assoc and
      ;; gnus-killed-assoc even if gnus-newsrc-assoc is out of date.
      (condition-case nil
	  (progn
	    (setq quick-loaded (load quick-file t t t))
	    ;; Recreate hashtables.
	    (setq gnus-killed-hashtb
		  (gnus-make-hashtable-from-alist gnus-killed-assoc))
	    (setq gnus-marked-hashtb
		  (gnus-make-hashtable-from-alist gnus-marked-assoc))
	    )
	(error nil))
      (cond ((and (not rawfile)		;Not forced to read the raw file.
		  ;; .newsrc.el is newer than .newsrc.
		  ;; Do it this way in case timestamps are identical
		  ;; (on fast machines/disks).
		  (not (file-newer-than-file-p newsrc-file quick-file))
		  quick-loaded
		  gnus-newsrc-assoc	;Really loaded?
		  )
	     ;; We don't have to read the raw startup file.
	     ;; gnus-newsrc-assoc may be defined in the quick startup file.
	     ;; So, we have to define the hashtable here.
	     (setq gnus-newsrc-hashtb
		   (gnus-make-hashtable-from-alist gnus-newsrc-assoc)))
	    (t
	     ;; Since .newsrc file is newer than quick file, read it.
	     (message "Reading %s..." newsrc-file)
	     (gnus-newsrc-to-gnus-format)
	     (gnus-check-killed-newsgroups)
	     (message "Reading %s...done" newsrc-file)))
      )))

(defun gnus-make-newsrc-file (file)
  "Make server dependent file name by catenating FILE and server host name."
  (let* ((file (expand-file-name file nil))
	 (real-file (concat file "-" gnus-nntp-server)))
    (if (file-exists-p real-file)
	real-file file)
    ))

(defun gnus-newsrc-to-gnus-format ()
  "Parse current buffer as `.newsrc' file."
  (let ((newsgroup nil)
	(subscribe nil)
	(ranges nil)
	(subrange nil)
	(read-list nil))
    ;; We have to re-initialize these variable (except for
    ;; gnus-marked-assoc and gnus-killed-assoc) because quick startup
    ;; file may contain bogus values.
    (setq gnus-newsrc-options nil)
    (setq gnus-newsrc-options-n-yes nil)
    (setq gnus-newsrc-options-n-no nil)
    (setq gnus-newsrc-assoc nil)
    ;; Make large enough hash table.
    (setq gnus-newsrc-hashtb
	  (gnus-make-hashtable
	   (max (length gnus-active-hashtb)
		(count-lines (point-min) (point-max)))))
    ;; Save options line to variable.
    ;; Lines beginning with white spaces are treated as continuation
    ;; line.  Refer man page of newsrc(5).
    (goto-char (point-min))
    (if (re-search-forward
	 "^[ \t]*options[ \t]*\\(.*\\(\n[ \t]+.*\\)*\\)[ \t]*$" nil t)
	(progn
	  ;; Save entire options line.
	  (setq gnus-newsrc-options
		(buffer-substring (match-beginning 1) (match-end 1)))
	  ;; Compile "-n" option.
	  (if (string-match "\\(^\\|[ \t\n]\\)-n" gnus-newsrc-options)
	      (let ((yes-and-no
		     (gnus-parse-n-options
		      (substring gnus-newsrc-options (match-end 0)))))
		(setq gnus-newsrc-options-n-yes (car yes-and-no))
		(setq gnus-newsrc-options-n-no  (cdr yes-and-no))
		))
	  ))
    ;; Parse body of .newsrc file
    ;; Options line continuation lines must be also considered here.
    ;; Before supporting continuation lines, " newsgroup ! 1-5" was
    ;; okay, but now it is invalid.  It should be "newsgroup! 1-5".
    (goto-char (point-min))
    ;; We used this regexp, but it caused overflows.
    ;; "^\\([^:! \t\n]+\\)\\([:!]\\)[ \t]*\\(.*\\)$"
    ;; Suggested by composer@bucsf.bu.edu (Jeff Kellem)
    ;; but no longer viable because of extensive backtracking in Emacs 19:
    ;; "^\\([^:! \t\n]+\\)\\([:!]\\)[ \t]*\\(\\(...\\)*.*\\)$"
    ;; but, the following causes trouble on some case:
    ;; "^\\([^:! \t\n]+\\)\\([:!]\\)[ \t]*\\(\\|[^ \t\n].*\\)$"
    ;; So now we don't try to match the tail of the line at all.
    ;; It's just as easy to extract it later.
    (while (re-search-forward "^\\([^:! \t\n]+\\)\\([:!]\\)"
			      nil t)
      (setq newsgroup (buffer-substring (match-beginning 1) (match-end 1)))
      ;; Check duplications of newsgroups.
      ;; Note: Checking the duplications takes very long time.
      (if (gnus-gethash newsgroup gnus-newsrc-hashtb)
	  (message "Ignore duplicated newsgroup: %s" newsgroup)
	(setq subscribe
	      (string-equal
	       ":" (buffer-substring (match-beginning 2) (match-end 2))))
	(skip-chars-forward " \t")
	(setq ranges (buffer-substring (point) (save-excursion
						 (end-of-line) (point))))
	(setq read-list nil)
	(while (string-match "^[, \t]*\\([0-9-]+\\)" ranges)
	  (setq subrange (substring ranges (match-beginning 1) (match-end 1)))
	  (setq ranges (substring ranges (match-end 1)))
	  (cond ((string-match "^\\([0-9]+\\)-\\([0-9]+\\)$" subrange)
		 (setq read-list
		       (cons
			(cons (string-to-int
			       (substring subrange
					  (match-beginning 1) (match-end 1)))
			      (string-to-int
			       (substring subrange
					  (match-beginning 2) (match-end 2))))
			read-list)))
		((string-match "^[0-9]+$" subrange)
		 (setq read-list
		       (cons (cons (string-to-int subrange)
				   (string-to-int subrange))
			     read-list)))
		(t
		 (ding) (message "Ignoring bogus lines of %s" newsgroup)
		 (sit-for 0))
		))
	(setq gnus-newsrc-assoc
	      (cons (cons newsgroup (cons subscribe (nreverse read-list)))
		    gnus-newsrc-assoc))
	;; Update gnus-newsrc-hashtb one by one.
	(gnus-sethash newsgroup (car gnus-newsrc-assoc) gnus-newsrc-hashtb)
	))
    (setq gnus-newsrc-assoc (nreverse gnus-newsrc-assoc))
    ))

(defun gnus-parse-n-options (options)
  "Parse -n NEWSGROUPS options and return a cons of YES and NO regexps."
  (let ((yes nil)
	(no nil)
	(yes-or-no nil)			;`!' or not.
	(newsgroup nil))
    ;; Parse each newsgroup description such as "comp.all".  Commas
    ;; and white spaces can be a newsgroup separator.
    (while
	(string-match "^[ \t\n,]*\\(!?\\)\\([^- \t\n,][^ \t\n,]*\\)" options)
      (setq yes-or-no
	    (substring options (match-beginning 1) (match-end 1)))
      (setq newsgroup
	    (regexp-quote
	     (substring options
			(match-beginning 2) (match-end 2))))
      (setq options (substring options (match-end 2)))
      ;; Rewrite "all" to ".+" not ".*".  ".+" requires at least one
      ;; character.
      (while (string-match "\\(^\\|\\\\[.]\\)all\\(\\\\[.]\\|$\\)" newsgroup)
	(setq newsgroup
	      (concat (substring newsgroup 0 (match-end 1))
		      ".+"
		      (substring newsgroup (match-beginning 2)))))
      ;; It is yes or no.
      (cond ((string-equal yes-or-no "!")
	     (setq no (cons newsgroup no)))
	    ((string-equal newsgroup ".+")) ;Ignore `all'.
	    (t
	     (setq yes (cons newsgroup yes))))
      )
    ;; Make a cons of regexps from parsing result.
    ;; We have to append \(\.\|$\) to prevent matching substring of
    ;; newsgroup.  For example, "jp.net" should not match with
    ;; "jp.network".
    ;; Fixes for large regexp problems are from yonezu@nak.math.keio.ac.jp.
    (cons (if yes
	      (concat "^\\("
		      (apply (function concat)
			     (mapcar
			      (function
			       (lambda (newsgroup)
				 (concat newsgroup "\\|")))
			      (cdr yes)))
		      (car yes) "\\)\\(\\.\\|$\\)"))
	  (if no
	      (concat "^\\("
		      (apply (function concat)
			     (mapcar
			      (function
			       (lambda (newsgroup)
				 (concat newsgroup "\\|")))
			      (cdr no)))
		      (car no) "\\)\\(\\.\\|$\\)")))
    ))

(defun gnus-save-newsrc-file ()
  "Save current status in the `.newsrc' file."
  ;; Note: We cannot save .newsrc file if all newsgroups are removed
  ;; from the variable gnus-newsrc-assoc.
  (and (or gnus-newsrc-assoc gnus-killed-assoc)
       gnus-current-startup-file
       (save-excursion
	 ;; A buffer containing .newsrc file may be deleted.
	 (set-buffer (find-file-noselect gnus-current-startup-file))
	 (if (not (buffer-modified-p))
	     (message "(No changes need to be saved)")
	   (message "Saving %s..." gnus-current-startup-file)
	   (let ((make-backup-files t)
		 (version-control nil)
		 (require-final-newline t)) ;Don't ask even if requested.
	     ;; Make backup file of master newsrc.
	     ;; You can stop or change version control of backup file.
	     ;; Suggested by jason@violet.berkeley.edu.
	     (run-hooks 'gnus-save-newsrc-hook)
	     (save-buffer))
	   ;; Quickly loadable .newsrc.
	   (set-buffer (get-buffer-create " *GNUS-newsrc*"))
	   (buffer-flush-undo (current-buffer))
	   (erase-buffer)
	   (gnus-gnus-to-quick-newsrc-format)
	   (let ((make-backup-files nil)
		 (version-control nil)
		 (require-final-newline t)) ;Don't ask even if requested.
	     (write-file (concat gnus-current-startup-file ".el")))
	   (kill-buffer (current-buffer))
	   (message "Saving %s...done" gnus-current-startup-file)
	   ))
    ))

(defun gnus-update-newsrc-buffer (group &optional delete next)
  "Incrementally update `.newsrc' buffer about GROUP.
If optional 1st argument DELETE is non-nil, delete the group.
If optional 2nd argument NEXT is non-nil, inserted before it."
  (save-excursion
    ;; Taking account of the killed startup file.
    ;; Suggested by tale@pawl.rpi.edu.
    (set-buffer (or (get-file-buffer gnus-current-startup-file)
		    (find-file-noselect gnus-current-startup-file)))
    ;; Options line continuation lines must be also considered here.
    ;; Before supporting continuation lines, " newsgroup ! 1-5" was
    ;; okay, but now it is invalid.  It should be "newsgroup! 1-5".
    (let ((deleted nil)
	  (case-fold-search nil)	;Should NOT ignore case.
	  (buffer-read-only nil))	;May be not modifiable.
      ;; Delete ALL entries which match for GROUP.
      (goto-char (point-min))
      (while (re-search-forward
	      (concat "^" (regexp-quote group) "[:!]") nil t)
	(beginning-of-line)
	(delete-region (point) (progn (forward-line 1) (point)))
	(setq deleted t)		;Old entry is deleted.
	)
      (if delete
	  nil
	;; Insert group entry.
	(let ((newsrc (gnus-gethash group gnus-newsrc-hashtb)))
	  (if (null newsrc)
	      nil
	    ;; Find insertion point.
	    (cond (deleted nil)		;Insert here.
		  ((and (stringp next)
			(progn
			  (goto-char (point-min))
			  (re-search-forward
			   (concat "^" (regexp-quote next) "[:!]") nil t)))
		   (beginning-of-line))
		  (t
		   (goto-char (point-max))
		   (or (bolp)
		       (insert "\n"))))
	    ;; Insert after options line.
	    (if (looking-at "^[ \t]*options\\([ \t]\\|$\\)")
		(progn
		  (forward-line 1)
		  ;; Skip continuation lines.
		  (while (and (not (eobp))
			      (looking-at "^[ \t]+"))
		    (forward-line 1))))
	    (insert group		;Group name
		    (if (nth 1 newsrc) ": " "! ")) ;Subscribed?
	    (gnus-ranges-to-newsrc-format (nthcdr 2 newsrc)) ;Read articles
	    (insert "\n")
	    )))
      )))

(defun gnus-gnus-to-quick-newsrc-format ()
  "Insert GNUS variables such as `gnus-newsrc-assoc' in Lisp format."
  (insert ";; GNUS internal format of .newsrc.\n")
  (insert ";; Touch .newsrc instead if you think to remove this file.\n")
  (let ((variable nil)
	(variables (cons 'gnus-newsgroups-alist gnus-variable-list))
	;; Temporary rebind to make changes
	;; gnus-check-killed-newsgroups in invisible.
	(gnus-killed-assoc gnus-killed-assoc)
	(gnus-killed-hashtb gnus-killed-hashtb))
    ;; Remove duplicated or unsubscribed newsgroups in
    ;; gnus-killed-assoc (and gnus-killed-hashtb).
    (gnus-check-killed-newsgroups)
    ;; Then, insert lisp expressions.
    (while variables
      (setq variable (car variables))
      (and (boundp variable)
	   (symbol-value variable)
	   (insert "(setq " (symbol-name variable) " '"
		   (prin1-to-string (symbol-value variable))
		   ")\n"))
      (setq variables (cdr variables)))
    ))

(defun gnus-ranges-to-newsrc-format (ranges)
  "Insert ranges of read articles."
  (let ((range nil))			;Range is a pair of BEGIN and END.
    (while ranges
      (setq range (car ranges))
      (setq ranges (cdr ranges))
      (cond ((= (car range) (cdr range))
	     (if (= (car range) 0)
		 (setq ranges nil)	;No unread articles.
	       (insert (int-to-string (car range)))
	       (if ranges (insert ","))
	       ))
	    (t
	     (insert (int-to-string (car range))
		     "-"
		     (int-to-string (cdr range)))
	     (if ranges (insert ","))
	     ))
      )))

(defun gnus-compress-sequence (numbers)
  "Convert list of sorted numbers to ranges."
  (let* ((numbers (sort (copy-sequence numbers) (function <)))
	 (first (car numbers))
	 (last (car numbers))
	 (result nil))
    (while numbers
      (cond ((= last (car numbers)) nil) ;Omit duplicated number
	    ((= (1+ last) (car numbers)) ;Still in sequence
	     (setq last (car numbers)))
	    (t				;End of one sequence
	     (setq result (cons (cons first last) result))
	     (setq first (car numbers))
	     (setq last  (car numbers)))
	    )
      (setq numbers (cdr numbers))
      )
    (nreverse (cons (cons first last) result))
    ))

(defun gnus-uncompress-sequence (ranges)
  "Expand compressed format of sequence."
  (let ((first nil)
	(last  nil)
	(result nil))
    (while ranges
      (setq first (car (car ranges)))
      (setq last  (cdr (car ranges)))
      (while (< first last)
	(setq result (cons first result))
	(setq first (1+ first)))
      (setq result (cons first result))
      (setq ranges (cdr ranges))
      )
    (nreverse result)
    ))

(defun gnus-number-of-articles (range)
  "Compute number of articles from RANGE `((beg1 . end1) (beg2 . end2) ...)'."
  (let ((count 0))
    (while range
      (if (/= (cdr (car range)) 0)
	  ;; If end1 is 0, it must be skipped.  Usually no articles in
	  ;;  this group.
	  (setq count (+ count 1 (- (cdr (car range)) (car (car range))))))
      (setq range (cdr range))
      )
    count				;Result
    ))

(defun gnus-difference-of-range (src obj)
  "Compute (SRC - OBJ) on range.
Range of SRC is expressed as `(beg . end)'.
Range of OBJ is expressed as `((beg1 . end1) (beg2 . end2) ...)."
  (let ((beg (car src))
	(end (cdr src))
	(range nil))			;This is result.
    ;; Src may be nil.
    (while (and src obj)
      (let ((beg1 (car (car obj)))
	    (end1 (cdr (car obj))))
	(cond ((> beg end)
	       (setq obj nil))		;Terminate loop
	      ((< beg beg1)
	       (setq range (cons (cons beg (min (1- beg1) end)) range))
	       (setq beg (1+ end1)))
	      ((>= beg beg1)
	       (setq beg (max beg (1+ end1))))
	      )
	(setq obj (cdr obj))		;Next OBJ
	))
    ;; Src may be nil.
    (if (and src (<= beg end))
	(setq range (cons (cons beg end) range)))
    ;; Result
    (if range
	(nreverse range)
      (list (cons 0 0)))
    ))

(defun gnus-read-distributions-file ()
  "Get distributions file from NNTP server (NNTP2 functionality)."
  ;; Make sure a connection to NNTP server is alive.
  (gnus-start-news-server)
  (message "Reading distributions file...")
  (setq gnus-distribution-list nil)
  (if (gnus-request-list-distributions)
      (save-excursion
	(set-buffer nntp-server-buffer)
	(gnus-distributions-to-gnus-format)
	(message "Reading distributions file...done"))
    ;; It's not a fatal error.
    ;;(error "Cannot read distributions file from NNTP server.")
    )
  ;; Merge with user supplied default distributions.
  (let ((defaults (reverse gnus-local-distributions))
	(dist nil))
    (while defaults
      (setq dist (assoc (car defaults) gnus-distribution-list))
      (if dist
	  (setq gnus-distribution-list
		(delq dist gnus-distribution-list)))
      (setq gnus-distribution-list
	    (cons (list (car defaults)) gnus-distribution-list))
      (setq defaults (cdr defaults))
      )))

(defun gnus-distributions-to-gnus-format ()
  "Convert distributions file format to internal format."
  (setq gnus-distribution-list nil)
  (goto-char (point-min))
  (while (re-search-forward "^\\([^ \t\n]+\\).*$" nil t)
    (setq gnus-distribution-list
	  (cons (list (buffer-substring (match-beginning 1) (match-end 1)))
		gnus-distribution-list)))
  (setq gnus-distribution-list
	(nreverse gnus-distribution-list)))

(defun gnus-newsgroups-retrieve-description ()
  "Retrieve newsgroups description and build gnus-newsgroups-alist"
  (message "Reading newsgroups file...")
  (if (gnus-request-list-newsgroups)
      (save-excursion
	(setq gnus-newsgroups-alist nil)
	(set-buffer nntp-server-buffer)
	(goto-char (point-min))
	(while (re-search-forward gnus-newsgroups-regex nil t)
	  (setq gnus-newsgroups-alist
		(cons (cons (buffer-substring (match-beginning 1) (match-end 1))
			    (buffer-substring (match-beginning 2) (match-end 2)))
		      gnus-newsgroups-alist)))
	(message "Reading newsgroups file...done"))
    (message "Cannot read newsgroups file")))

(defun gnus-newsgroups-update-description ()
  "Update the newsgroups description"
  (interactive)
  (gnus-newsgroups-retrieve-description)
  (setq gnus-newsgroups-hashtb (gnus-make-hashtable-from-alist gnus-newsgroups-alist)))

(defun gnus-newsgroups-display-toggle ()
  "Toggle displaying newsgroup descriptions in *Newsgroup* buffer."
  (interactive)
  (setq gnus-newsgroups-display (not gnus-newsgroups-display))
  (if gnus-newsgroups-showall
        (gnus-group-list-groups t)
    (gnus-group-list-groups nil)))

(provide 'gnus)

;;Local variables:
;;eval: (put 'gnus-eval-in-buffer-window 'lisp-indent-hook 1)
;;end:

;;; gnus.el ends here
