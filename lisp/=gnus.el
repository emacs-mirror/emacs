;;; GNUS: an NNTP-based News Reader for GNU Emacs
;; Copyright (C) 1987, 1988, 1989 Fujitsu Laboratories LTD.
;; Copyright (C) 1987, 1988, 1989, 1990 Masanobu UMEDA
;; $Header: gnus.el,v 3.13 90/03/23 13:24:27 umerin Locked $

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; GNUS Mailing List:
;; There are two mailing lists for GNUS lovers in the world:
;;
;;	info-gnus@flab.fujitsu.co.jp, and
;;	info-gnus-english@tut.cis.ohio-state.edu.
;;
;; They are intended to exchange useful information about GNUS, such
;; as bug fixes, useful hooks, and extensions.  The major difference
;; between the lists is what the official language is.  Both Japanese
;; and English are available in info-gnus, while English is only
;; available in info-gnus-english. There is no need to subscribe to
;; info-gnus if you cannot read Japanese messages, because most of the
;; discussion and important announcements will be sent to
;; info-gnus-english. Moreover, if you can read gnu.emacs.gnus
;; newsgroup of USENET, you need not, either. info-gnus-english and
;; gnu.emacs.gnus are linked each other.
;;
;; Please send subscription request to:
;;
;; 	info-gnus-request@flab.fujitsu.co.jp, or
;;	info-gnus-english-request@cis.ohio-state.edu

;; TO DO:
;; (1) Incremental update of active info.
;; (2) GNUS own poster.
;; (3) Multi-GNUS (Talking to many hosts same time).
;; (4) Asynchronous transmission of large messages.

(provide 'gnus)
(require 'nntp)
(require 'mail-utils)

(defvar gnus-nntp-server (or (getenv "NNTPSERVER") gnus-default-nntp-server)
  "The name of the host running NNTP server.
If it is a string such as `:DIRECTORY', the user's private DIRECTORY
is used as a news spool.
Initialized from the NNTPSERVER environment variable.")

(defvar gnus-signature-file "~/.signature"
  "*Your .signature file. Use `.signature-DISTRIBUTION' instead if exists.")

(defvar gnus-use-cross-reference t
  "Specifies what to do with cross references (Xref: field).
If nil, ignore cross references.  If t, mark articles as read in subscribed
newsgroups.  Otherwise, mark articles as read in all newsgroups.")

(defvar gnus-use-followup-to t
  "*Specifies what to do with Followup-To: field.
If nil, ignore followup-to: field.  If t, use its value execpt for
`poster'.  Otherewise, if not nil nor t, always use its value.")

(defvar gnus-large-newsgroup 50
  "*The number of articles which indicates a large newsgroup.
If the number of articles in a newsgroup is greater than the value,
confirmation is required for selecting the newsgroup.")

(defvar gnus-author-copy (getenv "AUTHORCOPY")
  "*Filename for saving a copy of an article posted using FCC: field.
Initialized from the AUTHORCOPY environment variable.

Articles are saved using a function specified by the the variable
`gnus-author-copy-saver' (`rmail-output' is the default) if a file name
is given.  Instead, if the first character of the name is `|', the
contents of the article is piped out to the named program.  It is
possible to save an article in an MH folder as follows:

    (setq gnus-author-copy \"|/usr/local/lib/mh/rcvstore +Article\")")

(defvar gnus-author-copy-saver (function rmail-output)
  "*A function called with a file name to save an author copy to.
The default function is `rmail-output' which saves in Unix mailbox format.")

(defvar gnus-use-long-file-name
  (not (memq system-type '(usg-unix-v xenix)))
  "Non-nil means that a newsgroup name is used as a default file name
to save articles to.  If nil, the directory form of a newsgroup is
used instead.")

(defvar gnus-article-save-directory (getenv "SAVEDIR")
  "*The directory in which to save articles; defaults to ~/News.
Initialized from the SAVEDIR environment variable.")

(defvar gnus-default-article-saver (function gnus-Subject-save-in-rmail)
  "A function used to save articles in your favorite format.
The function must be interactively callable (in other words, it must
be an Emacs command).

GNUS provides the following functions:
	gnus-Subject-save-in-rmail (in Rmail format)
	gnus-Subject-save-in-mail (in Unix mail format)
	gnus-Subject-save-in-folder (in an MH folder)
	gnus-Subject-save-in-file (in article format).")

(defvar gnus-rmail-save-name (function gnus-plain-save-name)
  "A function generating a file name to save articles in Rmail format.
The function is called with NEWSGROUP, HEADERS, and optional LAST-FILE.")

(defvar gnus-mail-save-name (function gnus-plain-save-name)
  "A function generating a file name to save articles in Unix mail format.
The function is called with NEWSGROUP, HEADERS, and optional LAST-FILE.")

(defvar gnus-folder-save-name (function gnus-folder-save-name)
  "A function generating a file name to save articles in MH folder.
The function is called with NEWSGROUP, HEADERS, and optional LAST-FOLDER.")

(defvar gnus-file-save-name (function gnus-numeric-save-name)
  "A function generating a file name to save articles in article format.
The function is called with NEWSGROUP, HEADERS, and optional LAST-FILE.")

(defvar gnus-kill-file-name "KILL"
  "File name of a KILL file.")

(defvar gnus-default-distribution "local"
  "*Use this value as distribution if no distribution is specified.")

(defvar gnus-novice-user t
  "*Non-nil means that you are a novice to USENET.
If non-nil, verbose messages may be displayed or your confirmation
may be required.")

(defvar gnus-interactive-post t
  "*Newsgroup, subject, and distribution will be asked for if non-nil.")

(defvar gnus-user-login-name nil
  "*The login name of the user.
Uses USER and LOGNAME environment variables if undefined.")

(defvar gnus-user-full-name nil
  "*The full name of the user.
Uses from the NAME environment variable if undefined.")

(defvar gnus-show-threads t
  "*Show conversation threads in Subject Mode if non-nil.")

(defvar gnus-thread-hide-subject t
  "*Non-nil means hide subjects for thread subtrees.")

(defvar gnus-thread-hide-subtree nil
  "*Non-nil means hide thread subtrees initially.
If non-nil, you have to run the command `gnus-Subject-show-thread' by
hand or by using `gnus-Select-article-hook' to show hidden threads.")

(defvar gnus-thread-hide-killed t
  "*Non-nil means hide killed thread subtrees automatically.")

(defvar gnus-thread-ignore-subject nil
  "*Don't take care of subject differences, but only references if non-nil.
If it is non-nil, some commands work with subjects do not work properly.")

(defvar gnus-thread-indent-level 4
  "Indentation of thread subtrees.")

(defvar gnus-ignored-headers
  "^Path:\\|^Posting-Version:\\|^Article-I.D.:\\|^Expires:\\|^Date-Received:\\|^References:\\|^Control:\\|^Xref:\\|^Lines:\\|^Posted:\\|^Relay-Version:\\|^Message-ID:\\|^Nf-ID:\\|^Nf-From:\\|^Approved:\\|^Sender:\\|^In-Reply-To:"
  "Regexp matching headers not to display in messages.")

(defvar gnus-show-all-headers nil
  "*Show all headers of an article if non-nil.")

(defvar gnus-save-all-headers nil
  "*Save all headers of an article if non-nil.")

(defvar gnus-optional-headers (function gnus-optional-lines-and-from)
  "A function generating a optional string displayed in GNUS Subject
mode buffer.  The function is called with an article HEADER. The
result must be a string excluding `[' and `]'.")

(defvar gnus-auto-extend-newsgroup t
  "*Extend visible articles to forward and backward if non-nil.")

(defvar gnus-auto-select-first t
  "*Select the first unread article automagically if non-nil.
If you want to prevent automatic selection of the first unread article
in some newsgroups, set the variable to nil in `gnus-Select-group-hook'
or `gnus-Apply-kill-hook'.")

(defvar gnus-auto-select-next t
  "*Select the next newsgroup automagically if non-nil.
If the value is t and the next newsgroup is empty, GNUS will exit
Subject mode and go back to Group mode.  If the value is neither nil
nor t, GNUS will select the following unread newsgroup. Especially, if
the value is the symbol `quietly', the next unread newsgroup will be
selected without any confirmations.")

(defvar gnus-auto-select-same nil
  "*Select the next article with the same subject automagically if non-nil.")

(defvar gnus-auto-center-subject t
  "*Always center the current subject in GNUS Subject mode window if non-nil.")

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
  '((SelectNewsgroup (0 1 0))
    (ExitNewsgroup   (1 0 0))
    (SelectArticle   (0 3 10))
    (ExpandSubject   (0 1 0)))
  "Specify window configurations for each action.
The format of the variable is a list of (ACTION (G S A)), where
G, S, and A are the relative height of Group, Subject, and Article
windows, respectively.  ACTION is `SelectNewsgroup', `ExitNewsgroup',
`SelectArticle', or `ExpandSubject'.")

(defvar gnus-mail-reply-method
  (function gnus-mail-reply-using-mail)
  "A function to compose reply mail.
The function `gnus-mail-reply-using-mail' uses usual the sendmail mail
program.  The function `gnus-mail-reply-using-mhe' uses the mh-e mail
program.  You can use yet another program by customizing this variable.")

(defvar gnus-mail-other-window-method
  (function gnus-mail-other-window-using-mail)
  "A function to compose mail in other window.
The function `gnus-mail-other-window-using-mail' uses usual sendmail
mail program.  The function `gnus-mail-other-window-using-mhe' uses mh-e
mail program.  You can use yet another program by customizing this variable.")

(defvar gnus-subscribe-newsgroup-method
  (function
   (lambda (newsgroup)
     (gnus-subscribe-newsgroup newsgroup
			       (car (car gnus-newsrc-assoc)))))
  "A function called with a newsgroup name when it is created.")

(defvar gnus-Group-mode-hook nil
  "A hook for GNUS Group Mode.")

(defvar gnus-Subject-mode-hook nil
  "A hook for GNUS Subject Mode.")

(defvar gnus-Article-mode-hook nil
  "A hook for GNUS Article Mode.")

(defvar gnus-Kill-file-mode-hook nil
  "A hook for GNUS KILL File Mode.")

(defvar gnus-Open-server-hook nil
  "A hook called just before opening connection to news server.")

(defvar gnus-Startup-hook nil
  "A hook called at start up time.
This hook is called after GNUS is connected to the NNTP server.
So, it is possible to change the behavior of GNUS according to the
selected NNTP server.")

(defvar gnus-Group-prepare-hook nil
  "A hook called after newsgroup list is created in the Newsgroup buffer.
If you want to modify the Newsgroup buffer, you can use this hook.")

(defvar gnus-Subject-prepare-hook nil
  "A hook called after subject list is created in the Subject buffer.
If you want to modify the Subject buffer, you can use this hook.")

(defvar gnus-Article-prepare-hook nil
  "A hook called after an article is prepared in the Article buffer.
If you want to run a special decoding program like nkf, use this hook.")

(defvar gnus-Select-group-hook nil
  "A hook called when a newsgroup is selected.
If you want to sort Subject buffer by date and then by subject, you
can use the following hook:

(setq gnus-Select-group-hook
      '(lambda ()
	 ;; First of all, sort by date.
	 (gnus-sort-headers
	  '(lambda (a b)
	     (gnus-date-lessp (gnus-header-date a)
			      (gnus-header-date b))))
	 ;; Then sort by subject string ignoring `Re:'.
	 ;; If case-fold-search is non-nil, case of letters is ignored.
	 (gnus-sort-headers
	  '(lambda (a b)
	     (gnus-string-lessp
	      (gnus-simplify-subject (gnus-header-subject a) 're)
	      (gnus-simplify-subject (gnus-header-subject b) 're)
	      )))))

If you'd like to simplify subjects like the `gnus-Subject-next-same-subject'
command does, you can use the following hook:

(setq gnus-Select-group-hook
      '(lambda ()
	 (mapcar (function
		  (lambda (header)
		    (nntp-set-header-subject
		     header
		     (gnus-simplify-subject
		      (gnus-header-subject header) 're-only))))
		 gnus-newsgroup-headers)))

In some newsgroups author name is meaningless.  It is possible to
prevent listing author names in the GNUS Subject buffer as follows:

(setq gnus-Select-group-hook
      '(lambda ()
	 (cond ((string-equal \"comp.sources.unix\" gnus-newsgroup-name)
		(setq gnus-optional-headers
		      (function gnus-optional-lines)))
	       (t
		(setq gnus-optional-headers
		      (function gnus-optional-lines-and-from))))))")

(defvar gnus-Select-article-hook
  (function (lambda () (gnus-Subject-show-thread)))
  "Hook called when an article is selected.
The default hook automatically shows conversation thread subtrees
of the selected article as follows:

(setq gnus-Select-article-hook
      '(lambda ()
	 (gnus-Subject-show-thread)))

If you'd like to run RMAIL on a digest article automagically, you can
use the following hook:

(setq gnus-Select-article-hook
      '(lambda ()
	 (gnus-Subject-show-thread)
	 (cond ((string-equal \"comp.sys.sun\" gnus-newsgroup-name)
		(gnus-Subject-rmail-digest))
	       ((and (string-equal \"comp.text\" gnus-newsgroup-name)
		     (string-match \"^TeXhax Digest\"
				   (gnus-header-subject gnus-current-headers)))
		(gnus-Subject-rmail-digest)
		))))")

(defvar gnus-Select-digest-hook
  (function
   (lambda ()
     ;; Reply-To: is required by `undigestify-rmail-message'.
     (or (mail-position-on-field "Reply-to" t)
	 (progn
	   (mail-position-on-field "Reply-to")
	   (insert (gnus-fetch-field "From"))))))
  "A hook called when reading digest messages using Rmail.
This hook can be used to modify incomplete digest articles as follows
(this is the default):

(setq gnus-Select-digest-hook
      '(lambda ()
	 ;; Reply-To: is required by `undigestify-rmail-message'.
	 (or (mail-position-on-field \"Reply-to\" t)
	     (progn
	       (mail-position-on-field \"Reply-to\")
	       (insert (gnus-fetch-field \"From\"))))))")

(defvar gnus-Rmail-digest-hook nil
  "A hook called when reading digest messages using Rmail.
This hook is intended to customize Rmail mode for reading digest articles.")

(defvar gnus-Apply-kill-hook (function gnus-apply-kill-file)
  "A hook called when a newsgroup is selected and subject list is prepared.
This hook is intended to apply a KILL file to the selected newsgroup.
The function `gnus-apply-kill-file' is called defaultly.

Since a general KILL file is too heavy to use for only a few
newsgroups, we recommend you use a lighter hook function.  For
example, if you'd like to apply a KILL file to articles which contains
a string `rmgroup' in subject in newsgroup `control', you can use the
following hook:

(setq gnus-Apply-kill-hook
      '(lambda ()
	 (cond ((string-match \"control\" gnus-newsgroup-name)
		(gnus-kill \"Subject\" \"rmgroup\")
		(gnus-expunge \"X\")))))")

(defvar gnus-Mark-article-hook
  (function
   (lambda ()
     (or (memq gnus-current-article gnus-newsgroup-marked)
	 (gnus-Subject-mark-as-read gnus-current-article))
     (gnus-Subject-set-current-mark "+")))
  "A hook called when an article is selected for the first time.
The hook is intended to mark an article as read when it is selected.
If you'd like to mark as unread (-) instead, use the following hook:

(setq gnus-Mark-article-hook
      '(lambda ()
	 (gnus-Subject-mark-as-unread gnus-current-article)
	 (gnus-Subject-set-current-mark \"+\")))")

(defvar gnus-Inews-article-hook nil
  "A hook called before posting an article.
If you'd like to run a special encoding program, use this hook.")

(defvar gnus-Exit-group-hook nil
  "A hook called when exiting (not quitting) Subject mode.
If your machine is so slow that exiting from Subject mode takes a
long time, set the variable `gnus-newsgroup-headers' to nil.  This
inhibits marking articles as read using cross-reference information.")

(defvar gnus-Suspend-gnus-hook nil
  "A hook called when suspending (not exiting) GNUS.")

(defvar gnus-Exit-gnus-hook nil
  "A hook called when exiting (not suspending) GNUS.")

(defvar gnus-Save-newsrc-hook nil
  "A hook called when saving the newsrc file.
This hook is called before saving .newsrc file.")

(defvar gnus-your-domain nil
  "*Your domain name without your host name like: \"stars.flab.Fujitsu.CO.JP\"
The environment variable DOMAINNAME is used instead if defined.  If
the function `system-name' returns the full internet name, there is no
need to define this variable.")

(defvar gnus-your-organization nil
  "*Your organization like: \"Fujitsu Laboratories Ltd., Kawasaki, Japan.\"
The `ORGANIZATION' environment variable is used instead if defined.")

(defvar gnus-use-generic-from nil
  "*If nil, prepend local host name to the defined domain in the From:
field; if stringp, use this; if non-nil, strip of the local host name.")

(defvar gnus-use-generic-path nil
  "*If nil, use the NNTP server name in the Path: field; if stringp,
use this; if non-nil, use no host name (user name only)")

;; Internal variables.

(defconst gnus-version "GNUS 3.13"
  "Version numbers of this version of GNUS.")

(defvar gnus-Info-nodes
  '((gnus-Group-mode . "(gnus)Newsgroup Commands")
    (gnus-Subject-mode . "(gnus)Subject Commands")
    (gnus-Article-mode . "(gnus)Article Commands")
    (gnus-Kill-file-mode . "(gnus)KILL File")
    (gnus-Browse-killed-mode . "(gnus)Maintenance"))
  "Assoc list of major modes and related Info nodes.")

(defvar gnus-access-methods
  '((nntp
     (gnus-retrieve-headers .	nntp-retrieve-headers)
     (gnus-open-server .	nntp-open-server)
     (gnus-close-server .	nntp-close-server)
     (gnus-server-opened .	nntp-server-opened)
     (gnus-status-message .	nntp-status-message)
     (gnus-request-article .	nntp-request-article)
     (gnus-request-group .	nntp-request-group)
     (gnus-request-list .	nntp-request-list)
     (gnus-request-post .	nntp-request-post))
    (nnspool
     (gnus-retrieve-headers .	nnspool-retrieve-headers)
     (gnus-open-server .	nnspool-open-server)
     (gnus-close-server .	nnspool-close-server)
     (gnus-server-opened .	nnspool-server-opened)
     (gnus-status-message .	nnspool-status-message)
     (gnus-request-article .	nnspool-request-article)
     (gnus-request-group .	nnspool-request-group)
     (gnus-request-list .	nnspool-request-list)
     (gnus-request-post .	nnspool-request-post))
    (mhspool
     (gnus-retrieve-headers .	mhspool-retrieve-headers)
     (gnus-open-server .	mhspool-open-server)
     (gnus-close-server .	mhspool-close-server)
     (gnus-server-opened .	mhspool-server-opened)
     (gnus-status-message .	mhspool-status-message)
     (gnus-request-article .	mhspool-request-article)
     (gnus-request-group .	mhspool-request-group)
     (gnus-request-list .	mhspool-request-list)
     (gnus-request-post .	mhspool-request-post)))
  "Access method for NNTP, nnspool, and mhspool.")

(defvar gnus-Group-buffer "*Newsgroup*")
(defvar gnus-Subject-buffer "*Subject*")
(defvar gnus-Article-buffer "*Article*")
(defvar gnus-Digest-buffer "GNUS Digest")
(defvar gnus-Digest-summary-buffer "GNUS Digest-summary")

(defvar gnus-buffer-list
  (list gnus-Group-buffer gnus-Subject-buffer gnus-Article-buffer
	gnus-Digest-buffer gnus-Digest-summary-buffer)
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

(defvar gnus-newsrc-options nil
  "Options line in the .newsrc file.")

(defvar gnus-newsrc-options-n-yes nil
  "Regexp representing subscribed newsgroups.")

(defvar gnus-newsrc-options-n-no nil
  "Regexp representing unsubscribed newsgroups.")

(defvar gnus-newsrc-assoc nil
  "Assoc list of read articles.")

(defvar gnus-killed-assoc nil
  "Assoc list of newsgroups removed from `gnus-newsrc-assoc'.")

(defvar gnus-marked-assoc nil
  "Assoc list of articles marked as unread.")

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
  "List of article headers in the current newsgroup.")

(defvar gnus-current-article nil)
(defvar gnus-current-headers nil)
(defvar gnus-current-history nil)
(defvar gnus-have-all-headers nil)
(defvar gnus-last-article nil)
(defvar gnus-current-kill-article nil)

;; Save window configuration.
(defvar gnus-winconf-kill-file nil)

(defvar gnus-Group-mode-map nil)
(defvar gnus-Subject-mode-map nil)
(defvar gnus-Article-mode-map nil)
(defvar gnus-Kill-file-mode-map nil)

(defvar rmail-last-file (expand-file-name "~/XMBOX"))
(defvar rmail-last-rmail-file (expand-file-name "~/XNEWS"))

;; Define GNUS Subsystems.
(autoload 'gnus-Group-post-news "gnuspost"
	  "Post an article." t)
(autoload 'gnus-Subject-post-news "gnuspost"
	  "Post an article." t)
(autoload 'gnus-Subject-post-reply "gnuspost"
	  "Post a reply article." t)
(autoload 'gnus-Subject-post-reply-with-original "gnuspost"
	  "Post a reply article with original article." t)
(autoload 'gnus-Subject-cancel-article "gnuspost"
	  "Cancel an article you posted." t)

(autoload 'gnus-Subject-mail-reply "gnusmail"
	  "Reply mail to news author." t)
(autoload 'gnus-Subject-mail-reply-with-original "gnusmail"
	  "Reply mail to news author with original article." t)
(autoload 'gnus-Subject-mail-other-window "gnusmail"
	  "Compose mail in other window." t)

(autoload 'gnus-Group-kill-group "gnusmisc"
	  "Kill newsgroup on current line." t)
(autoload 'gnus-Group-yank-group "gnusmisc"
	  "Yank the last killed newsgroup on current line." t)
(autoload 'gnus-Browse-killed-groups "gnusmisc"
	  "Browse the killed newsgroups." t)

(autoload 'rmail-output "rmailout"
	  "Append this message to Unix mail file named FILE-NAME." t)
(autoload 'mail-position-on-field "sendmail")
(autoload 'mh-find-path "mh-e")
(autoload 'mh-prompt-for-folder "mh-e")

(put 'gnus-Group-mode 'mode-class 'special)
(put 'gnus-Subject-mode 'mode-class 'special)
(put 'gnus-Article-mode 'mode-class 'special)


;;(put 'gnus-eval-in-buffer-window 'lisp-indent-function 1)

(defmacro gnus-eval-in-buffer-window (buffer &rest forms)
  "Pop to BUFFER, evaluate FORMS, and then returns to original window."
  (` (let ((GNUSStartBufferWindow (selected-window)))
       (unwind-protect
	   (progn
	     (pop-to-buffer (, buffer))
	     (,@ forms))
	 (select-window GNUSStartBufferWindow)))))

(defmacro gnus-make-hashtable ()
  '(make-abbrev-table))

(defmacro gnus-gethash (string hashtable)
  "Get hash value of STRING in HASHTABLE."
  ;;(` (symbol-value (abbrev-symbol (, string) (, hashtable))))
  (` (abbrev-expansion (, string) (, hashtable))))

(defmacro gnus-sethash (string value hashtable)
  "Set hash value. Arguments are STRING, VALUE, and HASHTABLE."
  ;; We cannot use define-abbrev since it only accepts string as value.
  (` (set (intern (, string) (, hashtable)) (, value))))

;; Note: Macros defined here are also defined in nntp.el. I don't like
;; to put them here, but many users got troubled with the old
;; definitions in nntp.elc. These codes are NNTP 3.10 version.

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

(if gnus-Group-mode-map
    nil
  (setq gnus-Group-mode-map (make-keymap))
  (suppress-keymap gnus-Group-mode-map)
  (define-key gnus-Group-mode-map " " 'gnus-Group-read-group)
  (define-key gnus-Group-mode-map "=" 'gnus-Group-select-group)
  (define-key gnus-Group-mode-map "j" 'gnus-Group-jump-to-group)
  (define-key gnus-Group-mode-map "n" 'gnus-Group-next-unread-group)
  (define-key gnus-Group-mode-map "p" 'gnus-Group-prev-unread-group)
  (define-key gnus-Group-mode-map "\177" 'gnus-Group-prev-unread-group)
  (define-key gnus-Group-mode-map "N" 'gnus-Group-next-group)
  (define-key gnus-Group-mode-map "P" 'gnus-Group-prev-group)
  (define-key gnus-Group-mode-map "\C-n" 'gnus-Group-next-group)
  (define-key gnus-Group-mode-map "\C-p" 'gnus-Group-prev-group)
  (define-key gnus-Group-mode-map "\r" 'next-line)
  (define-key gnus-Group-mode-map "/" 'isearch-forward)
  (define-key gnus-Group-mode-map "<" 'beginning-of-buffer)
  (define-key gnus-Group-mode-map ">" 'end-of-buffer)
  (define-key gnus-Group-mode-map "u" 'gnus-Group-unsubscribe-current-group)
  (define-key gnus-Group-mode-map "U" 'gnus-Group-unsubscribe-group)
  (define-key gnus-Group-mode-map "c" 'gnus-Group-catch-up)
  (define-key gnus-Group-mode-map "C" 'gnus-Group-catch-up-all)
  (define-key gnus-Group-mode-map "l" 'gnus-Group-list-groups)
  (define-key gnus-Group-mode-map "L" 'gnus-Group-list-all-groups)
  (define-key gnus-Group-mode-map "g" 'gnus-Group-get-new-news)
  (define-key gnus-Group-mode-map "R" 'gnus-Group-restart)
  (define-key gnus-Group-mode-map "b" 'gnus-Group-check-bogus-groups)
  (define-key gnus-Group-mode-map "r" 'gnus-Group-restrict-groups)
  (define-key gnus-Group-mode-map "a" 'gnus-Group-post-news)
  (define-key gnus-Group-mode-map "\ek" 'gnus-Group-edit-local-kill)
  (define-key gnus-Group-mode-map "\eK" 'gnus-Group-edit-global-kill)
  (define-key gnus-Group-mode-map "\C-k" 'gnus-Group-kill-group)
  (define-key gnus-Group-mode-map "\C-y" 'gnus-Group-yank-group)
  (define-key gnus-Group-mode-map "\C-c\C-y" 'gnus-Browse-killed-groups)
  (define-key gnus-Group-mode-map "V" 'gnus-version)
  (define-key gnus-Group-mode-map "x" 'gnus-Group-force-update)
  (define-key gnus-Group-mode-map "s" 'gnus-Group-force-update)
  (define-key gnus-Group-mode-map "z" 'gnus-Group-suspend)
  (define-key gnus-Group-mode-map "q" 'gnus-Group-exit)
  (define-key gnus-Group-mode-map "Q" 'gnus-Group-quit)
  (define-key gnus-Group-mode-map "?" 'gnus-Group-describe-briefly)
  (define-key gnus-Group-mode-map "\C-c\C-i" 'gnus-Info-find-node))

(defun gnus-Group-mode ()
  "Major mode for reading network news.
All normal editing commands are turned off.
Instead, these commands are available:
\\{gnus-Group-mode-map}

  The name of the host running NNTP server is asked for if no default
host is specified. It is also possible to choose another NNTP server
even when the default server is defined by giving a prefix argument to
the command `\\[gnus]'.

  If an NNTP server is preceded by a colon such as `:Mail', the user's
private directory `~/Mail' is used as a news spool. This makes it
possible to read mail stored in MH folders or articles saved by GNUS.
File names of mail or articles must consist of only numeric
characters. Otherwise, they are ignored.

  If there is a file named `~/.newsrc-SERVER', it is used as the
startup file instead of standard one when talking to SERVER.  It is
possible to talk to many hosts by using different startup files for
each.

  Option `-n' of the options line in the startup file is recognized
properly the same as the Bnews system. For example, if the options
line is `options -n !talk talk.rumors', newsgroups under the `talk'
hierarchy except for `talk.rumors' are ignored while checking new
newsgroups.

  If there is a file named `~/.signature-DISTRIBUTION', it is used as
signature file instead of standard one when posting a news in
DISTRIBUTION.

  If an Info file generated from `gnus.texinfo' is installed, you can
read an appropriate Info node of the Info file according to the
current major mode of GNUS by \\[gnus-Info-find-node].

  The variable `gnus-version', `nntp-version', `nnspool-version', and
`mhspool-version' have the version numbers of this version of gnus.el,
nntp.el, nnspool.el, and mhspoo.el, respectively.

User customizable variables:
 gnus-nntp-server
    Specifies the name of the host running the NNTP server. If its
    value is a string such as `:DIRECTORY', the user's private
    DIRECTORY is used as a news spool. The variable is initialized
    from the NNTPSERVER environment variable.

 gnus-nntp-service
    Specifies a NNTP service name. It is usually \"nntp\" or 119.  Nil
    forces GNUS to use a local news spool if the variable
    `gnus-nntp-server' is set to the local host name.

 gnus-startup-file
    Specifies a startup file (.newsrc). If there is a file named
    `.newsrc-SERVER', it's used instead when talking to SERVER. I
    recommend you to use the server specific file, if you'd like to
    talk to many servers.  Especially if you'd like to read your
    private directory, the name of the file must be
    `.newsrc-:DIRECTORY'.

 gnus-signature-file
    Specifies a signature file (.signature). If there is a file named
    `.signature-DISTRIBUTION', it's used instead when posting an
    article in DISTRIBUTION. Set the variable to nil to prevent
    appending the file automatically. If you use an NNTP inews which
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
    Specifies the configuration of Group, Subject, and Article
    windows.  It is a list of (ACTION (G S A)), where G, S, and A are
    the relative height of Group, Subject, and Article windows,
    respectively.  ACTION is `SelectNewsgroup', `ExitNewsgroup',
    `SelectArticle', or `ExpandSubject'.

 gnus-subscribe-newsgroup-method
    Specifies a function called with a newsgroup name when new
    newsgroup is found.  The default definition adds new newsgroup at
    the beginning of other newsgroups.

Various hooks for customization:
 gnus-Group-mode-hook
    Entry to this mode calls the value with no arguments, if that
    value is non-nil. This hook is called before GNUS is connected to
    the NNTP server. So, you can change or define the NNTP server in
    this hook.

 gnus-Startup-hook
    Called with no arguments after the NNTP server is selected. It is
    possible to change the behavior of GNUS or initialize the
    variables according to the selected NNTP server.

 gnus-Group-prepare-hook
    Called with no arguments after a newsgroup list is created in the
    Newsgroup buffer, if that value is non-nil.

 gnus-Save-newsrc-hook
    Called with no arguments when saving newsrc file if that value is
    non-nil.

 gnus-Inews-article-hook
    Called with no arguments when posting an article if that value is
    non-nil. This hook is called just before posting an article, while
    `news-inews-hook' is called before preparing article headers. If
    you'd like to convert kanji code of the article, this hook is recommended.

 gnus-Suspend-gnus-hook
    Called with no arguments when suspending (not exiting) GNUS, if
    that value is non-nil.

 gnus-Exit-gnus-hook
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
  (setq major-mode 'gnus-Group-mode)
  (setq mode-name "Newsgroup")
  (setq mode-line-buffer-identification	"GNUS: List of Newsgroups")
  (setq mode-line-process nil)
  (use-local-map gnus-Group-mode-map)
  (buffer-flush-undo (current-buffer))
  (setq buffer-read-only t)		;Disable modification
  (run-hooks 'gnus-Group-mode-hook))

;;;###autoload
(defun gnus (&optional confirm)
  "Read network news.
If optional argument CONFIRM is non-nil, ask NNTP server."
  (interactive "P")
  (unwind-protect
      (progn
	(switch-to-buffer (get-buffer-create gnus-Group-buffer))
	(gnus-Group-mode)
	(gnus-start-news-server confirm))
    (if (not (gnus-server-opened))
	(gnus-Group-quit)
      ;; NNTP server is successfully open. 
      (setq mode-line-process (format " {%s}" gnus-nntp-server))
      (let ((buffer-read-only nil))
	(erase-buffer)
	(gnus-Group-startup-message)
	(sit-for 0))
      (run-hooks 'gnus-Startup-hook)
      (gnus-setup-news-info)
      (if gnus-novice-user
	  (gnus-Group-describe-briefly)) ;Show brief help message.
      (gnus-Group-list-groups nil)
      )))

(defun gnus-Group-startup-message ()
  "Insert startup message in current buffer."
  ;; Insert the message.
  (insert "
                   GNUS Version 3.13

         NNTP-based News Reader for GNU Emacs


If you have any trouble with this software, please let me
know. I will fix your problems in the next release.

Comments, suggestions, and bug fixes are welcome.

Masanobu UMEDA
umerin@tc.Nagasaki.GO.JP")
  ;; And then hack it.
  ;; 57 is the longest line.
  (indent-rigidly (point-min) (point-max) (/ (max (- (window-width) 57) 0) 2))
  (goto-char (point-min))
  ;; +4 is fuzzy factor.
  (insert-char ?\n (/ (max (- (window-height) 18) 0) 2)))

(defun gnus-Group-list-groups (show-all)
  "List newsgroups in the Newsgroup buffer.
If argument SHOW-ALL is non-nil, unsubscribed groups are also listed."
  (interactive "P")
  (let ((last-group			;Current newsgroup.
	 (gnus-Group-group-name))
	(next-group			;Next possible newsgroup.
	 (progn
	   (gnus-Group-search-forward nil nil)
	   (gnus-Group-group-name)))
	(prev-group			;Previous possible newsgroup.
	 (progn
	   (gnus-Group-search-forward t nil)
	   (gnus-Group-group-name))))
    (gnus-Group-prepare show-all)
    (if (zerop (buffer-size))
	(message "No news is good news")
      ;; Go to last newsgroup if possible.  If cannot, try next and
      ;; previous.  If all fail, go to first unread newsgroup.
      (goto-char (point-min))
      (or (and last-group
	       (re-search-forward
		(concat "^.+: " (regexp-quote last-group) "$") nil t))
	  (and next-group
	       (re-search-forward
		(concat "^.+: " (regexp-quote next-group) "$") nil t))
	  (and prev-group
	       (re-search-forward
		(concat "^.+: " (regexp-quote prev-group) "$") nil t))
	  (re-search-forward "^[ \t]+[1-9][0-9]*:" nil t))
      ;; Adjust cursor point.
      (beginning-of-line)
      (search-forward ":" nil t)
      )))

(defun gnus-Group-prepare (&optional all)
  "Prepare list of newsgroups in current buffer.
If optional argument ALL is non-nil, unsubscribed groups are also listed."
  (let ((buffer-read-only nil)
	(newsrc gnus-newsrc-assoc)
	(group-info nil)
	(group-name nil)
	(unread-count 0)
	;; This specifies the format of Group buffer.
	(cntl "%s%s%5d: %s\n"))
    (erase-buffer)
    ;; List newsgroups.
    (while newsrc
      (setq group-info (car newsrc))
      (setq group-name (car group-info))
      (setq unread-count (nth 1 (gnus-gethash group-name gnus-unread-hashtb)))
      (if (or all
	      (and (nth 1 group-info)	;Subscribed.
		   (> unread-count 0)))	;There are unread articles.
	  ;; Yes, I can use gnus-Group-prepare-line, but this is faster.
	  (insert
	   (format cntl
		   ;; Subscribed or not.
		   (if (nth 1 group-info) " " "U")
		   ;; Has new news?
		   (if (and (> unread-count 0)
			    (>= 0
				(- unread-count
				   (length
				    (cdr (assoc group-name
						gnus-marked-assoc))))))
		       "*" " ")
		   ;; Number of unread articles.
		   unread-count
		   ;; Newsgroup name.
		   group-name))
	)
      (setq newsrc (cdr newsrc))
      )
    (setq gnus-have-all-newsgroups all)
    (goto-char (point-min))
    (run-hooks 'gnus-Group-prepare-hook)
    ))

(defun gnus-Group-prepare-line (info)
  "Return a string for the Newsgroup buffer from INFO.
INFO is an element of gnus-newsrc-assoc or gnus-killed-assoc."
  (let* ((group-name (car info))
	 (unread-count
	  (or (nth 1 (gnus-gethash group-name gnus-unread-hashtb))
	      ;; Not in hash table, so compute it now.
	      (gnus-number-of-articles
	       (gnus-difference-of-range
		(nth 2 (gnus-gethash group-name gnus-active-hashtb))
		(nthcdr 2 info)))))
	 ;; This specifies the format of Group buffer.
	 (cntl "%s%s%5d: %s\n"))
    (format cntl
	    ;; Subscribed or not.
	    (if (nth 1 info) " " "U")
	    ;; Has new news?
	    (if (and (> unread-count 0)
		     (>= 0
			 (- unread-count
			    (length
			     (cdr (assoc group-name gnus-marked-assoc))))))
		"*" " ")
	    ;; Number of unread articles.
	    unread-count
	    ;; Newsgroup name.
	    group-name
	    )))

(defun gnus-Group-update-group (group &optional visible-only)
  "Update newsgroup info of GROUP.
If optional argument VISIBLE-ONLY is non-nil, non displayed group is ignored."
  (let ((buffer-read-only nil)
	(visible nil))
    ;; Buffer may be narrowed.
    (save-restriction
      (widen)
      ;; Search point to modify.
      (goto-char (point-min))
      (if (re-search-forward (concat "^.+: " (regexp-quote group) "$") nil t)
	  ;; GROUP is listed in current buffer. So, delete old line.
	  (progn
	    (setq visible t)
	    (beginning-of-line)
	    (delete-region (point) (progn (forward-line 1) (point)))
	    ))
      (if (or visible (not visible-only))
	  (progn
	    (insert (gnus-Group-prepare-line (assoc group gnus-newsrc-assoc)))
	    (forward-line -1)		;Move point on that line.
	    ))
      )))

;; GNUS Group mode command

(defun gnus-Group-group-name ()
  "Get newsgroup name around point."
  (save-excursion
    (beginning-of-line)
    (if (looking-at ".[* \t]*[0-9]+:[ \t]+\\([^ \t\n]+\\)$")
	(buffer-substring (match-beginning 1) (match-end 1))
      )))

(defun gnus-Group-read-group (all &optional no-article)
  "Read news in this newsgroup.
If argument ALL is non-nil, already read articles become readable.
If optional argument NO-ARTICLE is non-nil, no article body is displayed."
  (interactive "P")
  (let ((group (gnus-Group-group-name))) ;Newsgroup name to read.
    (if group
	(gnus-Subject-read-group
	 group
	 (or all
	     ;;(not (nth 1 (assoc group gnus-newsrc-assoc)))	;Unsubscribed
	     (zerop
	      (nth 1 (gnus-gethash group gnus-unread-hashtb))))	;No unread
	 no-article
	 ))
    ))

(defun gnus-Group-select-group (all)
  "Select this newsgroup.
No article is selected automatically.
If argument ALL is non-nil, already read articles become readable."
  (interactive "P")
  (gnus-Group-read-group all t))

(defun gnus-Group-jump-to-group (group)
  "Jump to newsgroup GROUP."
  (interactive
   (list (completing-read "Newsgroup: " gnus-newsrc-assoc nil 'require-match)))
  (goto-char (point-min))
  (or (re-search-forward (concat "^.+: " (regexp-quote group) "$") nil t)
      (if (assoc group gnus-newsrc-assoc)
	  ;; Add GROUP entry, then seach again.
	  (gnus-Group-update-group group)))
  ;; Adjust cursor point.
  (beginning-of-line)
  (search-forward ":" nil t))

(defun gnus-Group-search-forward (backward any-group)
  "Search for newsgroup forward.
If first argument BACKWARD is non-nil, search backward instead.
If second argument ANY-GROUP is non-nil, unsubscribed or empty
group may be selected."
  (let ((func (if backward 're-search-backward 're-search-forward))
	(regexp 
	 (format "^%s[ \t]*\\(%s\\):"
		 (if any-group ".." " [ \t]")
		 (if any-group "[0-9]+" "[1-9][0-9]*")))
	(found nil))
    (if backward
	(beginning-of-line)
      (end-of-line))
    (setq found (funcall func regexp nil t))
    ;; Adjust cursor point.
    (beginning-of-line)
    (search-forward ":" nil t)
    ;; Return T if found.
    found
    ))

(defun gnus-Group-next-group (n)
  "Go to next N'th newsgroup."
  (interactive "p")
  (while (and (> n 1)
	      (gnus-Group-search-forward nil t))
    (setq n (1- n)))
  (or (gnus-Group-search-forward nil t)
      (message "No more newsgroups")))

(defun gnus-Group-next-unread-group (n)
  "Go to next N'th unread newsgroup."
  (interactive "p")
  (while (and (> n 1)
	      (gnus-Group-search-forward nil nil))
    (setq n (1- n)))
  (or (gnus-Group-search-forward nil nil)
      (message "No more unread newsgroups")))

(defun gnus-Group-prev-group (n)
  "Go to previous N'th newsgroup."
  (interactive "p")
  (while (and (> n 1)
	      (gnus-Group-search-forward t t))
    (setq n (1- n)))
  (or (gnus-Group-search-forward t t)
      (message "No more newsgroups")))

(defun gnus-Group-prev-unread-group (n)
  "Go to previous N'th unread newsgroup."
  (interactive "p")
  (while (and (> n 1)
	      (gnus-Group-search-forward t nil))	      
    (setq n (1- n)))
  (or (gnus-Group-search-forward t nil)
      (message "No more unread newsgroups")))

(defun gnus-Group-catch-up (all &optional quietly)
  "Mark all articles not marked as unread in current newsgroup as read.
If prefix argument ALL is non-nil, all articles are marked as read.
Cross references (Xref: field) of articles are ignored."
  (interactive "P")
  (let* ((group (gnus-Group-group-name))
         (marked (if (not all)
		     (cdr (assoc group gnus-marked-assoc)))))
    (and group
	 (or quietly
	     (y-or-n-p
	      (if all
		  "Do you really want to mark everything as read? "
		"Delete all articles not marked as read? ")))
	 (progn
	   (message "")			;Erase "Yes or No" question.
	   ;; Any marked articles will be preserved.
	   (gnus-update-unread-articles group marked marked)
	   (gnus-Group-update-group group)
	   (gnus-Group-next-group 1)))
    ))

(defun gnus-Group-catch-up-all (&optional quietly)
  "Mark all articles in current newsgroup as read.
Cross references (Xref: field) of articles are ignored."
  (interactive)
  (gnus-Group-catch-up t quietly))

(defun gnus-Group-unsubscribe-current-group ()
  "Toggle subscribe from/to unsubscribe current group."
  (interactive)
  (gnus-Group-unsubscribe-group (gnus-Group-group-name))
  (gnus-Group-next-group 1))

(defun gnus-Group-unsubscribe-group (group)
  "Toggle subscribe from/to unsubscribe GROUP.
New newsgroup is added to .newsrc automatically."
  (interactive
   (list (completing-read "Newsgroup: "
			  gnus-active-hashtb nil 'require-match)))
  (let ((newsrc (assoc group gnus-newsrc-assoc)))
    (cond ((not (null newsrc))
	   ;; Toggle subscription flag.
	   (setcar (nthcdr 1 newsrc) (not (nth 1 newsrc)))
	   (gnus-update-newsrc-buffer group)
	   (gnus-Group-update-group group)
	   ;; Adjust cursor point.
	   (beginning-of-line)
	   (search-forward ":" nil t))
	  ((and (stringp group)
		(gnus-gethash group gnus-active-hashtb))
	   ;; Add new newsgroup.
	   (gnus-add-newsgroup group)
	   (gnus-Group-update-group group)
	   ;; Adjust cursor point.
	   (beginning-of-line)
	   (search-forward ":" nil t))
	  (t (error "No such newsgroup: %s" group)))
    ))

(defun gnus-Group-list-all-groups ()
  "List all of newsgroups in the Newsgroup buffer."
  (interactive)
  (gnus-Group-list-groups t))

(defun gnus-Group-get-new-news ()
  "Get newly arrived articles.  In fact, read the active file again."
  (interactive)
  (gnus-setup-news-info)
  (gnus-Group-list-groups gnus-have-all-newsgroups))

(defun gnus-Group-restart ()
  "Force GNUS to read the raw startup file."
  (interactive)
  (gnus-save-newsrc-file)
  (gnus-setup-news-info t)		;Force to read the raw startup file.
  (gnus-Group-list-groups gnus-have-all-newsgroups))

(defun gnus-Group-check-bogus-groups ()
  "Check bogus newsgroups."
  (interactive)
  (gnus-check-bogus-newsgroups t)	;Require confirmation.
  (gnus-Group-list-groups gnus-have-all-newsgroups))

(defun gnus-Group-restrict-groups (start end)
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

(defun gnus-Group-edit-global-kill ()
  "Edit a global KILL file."
  (interactive)
  (setq gnus-current-kill-article nil)	;No articles selected.
  (gnus-Kill-file-edit-file nil) 	;Nil stands for global KILL file.
  (message
   (substitute-command-keys
    "Editing a global KILL file (Type \\[gnus-Kill-file-exit] to exit)")))

(defun gnus-Group-edit-local-kill ()
  "Edit a local KILL file."
  (interactive)
  (setq gnus-current-kill-article nil)	;No articles selected.
  (gnus-Kill-file-edit-file (gnus-Group-group-name))
  (message
   (substitute-command-keys
    "Editing a local KILL file (Type \\[gnus-Kill-file-exit] to exit)")))

(defun gnus-Group-force-update ()
  "Update .newsrc file."
  (interactive)
  (gnus-save-newsrc-file))

(defun gnus-Group-suspend ()
  "Suspend the current GNUS session.
In fact, cleanup buffers except for Group Mode buffer.
The hook `gnus-Suspend-gnus-hook' is called before actually suspending."
  (interactive)
  (run-hooks 'gnus-Suspend-gnus-hook)
  ;; Kill GNUS buffers except for Group Mode buffer.
  (let ((buffers gnus-buffer-list))
    (while buffers
      (and (not (eq (car buffers) gnus-Group-buffer))
	   (get-buffer (car buffers))
	   (kill-buffer (car buffers)))
      (setq buffers (cdr buffers))
      ))
  (bury-buffer))

(defun gnus-Group-exit ()
  "Quit reading news after updating .newsrc.
The hook `gnus-Exit-gnus-hook' is called before actually quitting."
  (interactive)
  (if (or noninteractive		;For gnus-batch-kill
	  (zerop (buffer-size))		;No news is good news.
	  (not (gnus-server-opened))	;NNTP connection closed.
	  (y-or-n-p "Are you sure you want to quit reading news? "))
      (progn
	(message "")			;Erase "Yes or No" question.
	(run-hooks 'gnus-Exit-gnus-hook)
	(gnus-save-newsrc-file)
	(gnus-clear-system)
	(gnus-close-server))
    ))

(defun gnus-Group-quit ()
  "Quit reading news without updating .newsrc.
The hook `gnus-Exit-gnus-hook' is called before actually quitting."
  (interactive)
  (if (or (zerop (buffer-size))
	  (not (gnus-server-opened))
	  (yes-or-no-p
	   (format "Quit reading news without saving %s? "
		   (file-name-nondirectory gnus-current-startup-file))))
      (progn
	(message "")			;Erase "Yes or No" question.
	(run-hooks 'gnus-Exit-gnus-hook)
	(gnus-clear-system)
	(gnus-close-server))
    ))

(defun gnus-Group-describe-briefly ()
  "Describe Group mode commands briefly."
  (interactive)
  (message
   (concat
    (substitute-command-keys "\\[gnus-Group-read-group]:Select  ")
    (substitute-command-keys "\\[gnus-Group-next-unread-group]:Forward  ")
    (substitute-command-keys "\\[gnus-Group-prev-unread-group]:Backward  ")
    (substitute-command-keys "\\[gnus-Group-exit]:Exit  ")
    (substitute-command-keys "\\[gnus-Info-find-node]:Run Info  ")
    (substitute-command-keys "\\[gnus-Group-describe-briefly]:This help")
    )))


;;;
;;; GNUS Subject Mode
;;;

(if gnus-Subject-mode-map
    nil
  (setq gnus-Subject-mode-map (make-keymap))
  (suppress-keymap gnus-Subject-mode-map)
  (define-key gnus-Subject-mode-map " " 'gnus-Subject-next-page)
  (define-key gnus-Subject-mode-map "\177" 'gnus-Subject-prev-page)
  (define-key gnus-Subject-mode-map "\r" 'gnus-Subject-scroll-up)
  (define-key gnus-Subject-mode-map "n" 'gnus-Subject-next-unread-article)
  (define-key gnus-Subject-mode-map "p" 'gnus-Subject-prev-unread-article)
  (define-key gnus-Subject-mode-map "N" 'gnus-Subject-next-article)
  (define-key gnus-Subject-mode-map "P" 'gnus-Subject-prev-article)
  (define-key gnus-Subject-mode-map "\e\C-n" 'gnus-Subject-next-same-subject)
  (define-key gnus-Subject-mode-map "\e\C-p" 'gnus-Subject-prev-same-subject)
  ;;(define-key gnus-Subject-mode-map "\e\C-n" 'gnus-Subject-next-unread-same-subject)
  ;;(define-key gnus-Subject-mode-map "\e\C-p" 'gnus-Subject-prev-unread-same-subject)
  (define-key gnus-Subject-mode-map "\C-c\C-n" 'gnus-Subject-next-digest)
  (define-key gnus-Subject-mode-map "\C-c\C-p" 'gnus-Subject-prev-digest)
  (define-key gnus-Subject-mode-map "\C-n" 'gnus-Subject-next-subject)
  (define-key gnus-Subject-mode-map "\C-p" 'gnus-Subject-prev-subject)
  (define-key gnus-Subject-mode-map "\en" 'gnus-Subject-next-unread-subject)
  (define-key gnus-Subject-mode-map "\ep" 'gnus-Subject-prev-unread-subject)
  ;;(define-key gnus-Subject-mode-map "\C-cn" 'gnus-Subject-next-group)
  ;;(define-key gnus-Subject-mode-map "\C-cp" 'gnus-Subject-prev-group)
  (define-key gnus-Subject-mode-map "." 'gnus-Subject-first-unread-article)
  (define-key gnus-Subject-mode-map "/" 'isearch-forward)
  (define-key gnus-Subject-mode-map "s" 'gnus-Subject-isearch-article)
  (define-key gnus-Subject-mode-map "\es" 'gnus-Subject-search-article-forward)
  (define-key gnus-Subject-mode-map "\eS" 'gnus-Subject-search-article-backward)
  (define-key gnus-Subject-mode-map "<" 'gnus-Subject-beginning-of-article)
  (define-key gnus-Subject-mode-map ">" 'gnus-Subject-end-of-article)
  (define-key gnus-Subject-mode-map "j" 'gnus-Subject-goto-subject)
  (define-key gnus-Subject-mode-map "J" 'gnus-Subject-goto-article)
  (define-key gnus-Subject-mode-map "l" 'gnus-Subject-goto-last-article)
  (define-key gnus-Subject-mode-map "^" 'gnus-Subject-refer-parent-article)
  (define-key gnus-Subject-mode-map "\er" 'gnus-Subject-refer-article)
  (define-key gnus-Subject-mode-map "u" 'gnus-Subject-mark-as-unread-forward)
  (define-key gnus-Subject-mode-map "U" 'gnus-Subject-mark-as-unread-backward)
  (define-key gnus-Subject-mode-map "d" 'gnus-Subject-mark-as-read-forward)
  (define-key gnus-Subject-mode-map "D" 'gnus-Subject-mark-as-read-backward)
  (define-key gnus-Subject-mode-map "\eu" 'gnus-Subject-clear-mark-forward)
  (define-key gnus-Subject-mode-map "\eU" 'gnus-Subject-clear-mark-backward)
  (define-key gnus-Subject-mode-map "k" 'gnus-Subject-kill-same-subject-and-select)
  (define-key gnus-Subject-mode-map "\C-k" 'gnus-Subject-kill-same-subject)
  (define-key gnus-Subject-mode-map "\e\C-t" 'gnus-Subject-toggle-threads)
  (define-key gnus-Subject-mode-map "\e\C-s" 'gnus-Subject-show-thread)
  (define-key gnus-Subject-mode-map "\e\C-h" 'gnus-Subject-hide-thread)
  (define-key gnus-Subject-mode-map "\e\C-f" 'gnus-Subject-next-thread)
  (define-key gnus-Subject-mode-map "\e\C-b" 'gnus-Subject-prev-thread)
  (define-key gnus-Subject-mode-map "\e\C-u" 'gnus-Subject-up-thread)
  (define-key gnus-Subject-mode-map "\e\C-d" 'gnus-Subject-down-thread)
  (define-key gnus-Subject-mode-map "\e\C-k" 'gnus-Subject-kill-thread)
  (define-key gnus-Subject-mode-map "&" 'gnus-Subject-execute-command)
  ;;(define-key gnus-Subject-mode-map "c" 'gnus-Subject-catch-up)
  ;;(define-key gnus-Subject-mode-map "c" 'gnus-Subject-catch-up-all)
  (define-key gnus-Subject-mode-map "c" 'gnus-Subject-catch-up-and-exit)
  ;;(define-key gnus-Subject-mode-map "c" 'gnus-Subject-catch-up-all-and-exit)
  (define-key gnus-Subject-mode-map "\C-t" 'gnus-Subject-toggle-truncation)
  (define-key gnus-Subject-mode-map "x" 'gnus-Subject-delete-marked-as-read)
  (define-key gnus-Subject-mode-map "X" 'gnus-Subject-delete-marked-with)
  (define-key gnus-Subject-mode-map "\C-c\C-sn" 'gnus-Subject-sort-by-number)
  (define-key gnus-Subject-mode-map "\C-c\C-sa" 'gnus-Subject-sort-by-author)
  (define-key gnus-Subject-mode-map "\C-c\C-ss" 'gnus-Subject-sort-by-subject)
  (define-key gnus-Subject-mode-map "\C-c\C-sd" 'gnus-Subject-sort-by-date)
  (define-key gnus-Subject-mode-map "\C-c\C-s\C-n" 'gnus-Subject-sort-by-number)
  (define-key gnus-Subject-mode-map "\C-c\C-s\C-a" 'gnus-Subject-sort-by-author)
  (define-key gnus-Subject-mode-map "\C-c\C-s\C-s" 'gnus-Subject-sort-by-subject)
  (define-key gnus-Subject-mode-map "\C-c\C-s\C-d" 'gnus-Subject-sort-by-date)
  (define-key gnus-Subject-mode-map "=" 'gnus-Subject-expand-window)
  (define-key gnus-Subject-mode-map "G" 'gnus-Subject-reselect-current-group)
  (define-key gnus-Subject-mode-map "w" 'gnus-Subject-stop-page-breaking)
  (define-key gnus-Subject-mode-map "\C-c\C-r" 'gnus-Subject-caesar-message)
  (define-key gnus-Subject-mode-map "g" 'gnus-Subject-show-article)
  (define-key gnus-Subject-mode-map "t" 'gnus-Subject-toggle-header)
  (define-key gnus-Subject-mode-map "v" 'gnus-Subject-show-all-headers)
  (define-key gnus-Subject-mode-map "\C-d" 'gnus-Subject-rmail-digest)
  (define-key gnus-Subject-mode-map "a" 'gnus-Subject-post-news)
  (define-key gnus-Subject-mode-map "f" 'gnus-Subject-post-reply)
  (define-key gnus-Subject-mode-map "F" 'gnus-Subject-post-reply-with-original)
  (define-key gnus-Subject-mode-map "C" 'gnus-Subject-cancel-article)
  (define-key gnus-Subject-mode-map "r" 'gnus-Subject-mail-reply)
  (define-key gnus-Subject-mode-map "R" 'gnus-Subject-mail-reply-with-original)
  (define-key gnus-Subject-mode-map "m" 'gnus-Subject-mail-other-window)
  (define-key gnus-Subject-mode-map "o" 'gnus-Subject-save-article)
  (define-key gnus-Subject-mode-map "\C-o" 'gnus-Subject-save-in-mail)
  (define-key gnus-Subject-mode-map "|" 'gnus-Subject-pipe-output)
  (define-key gnus-Subject-mode-map "\ek" 'gnus-Subject-edit-local-kill)
  (define-key gnus-Subject-mode-map "\eK" 'gnus-Subject-edit-global-kill)
  (define-key gnus-Subject-mode-map "V" 'gnus-version)
  (define-key gnus-Subject-mode-map "q" 'gnus-Subject-exit)
  (define-key gnus-Subject-mode-map "Q" 'gnus-Subject-quit)
  (define-key gnus-Subject-mode-map "?" 'gnus-Subject-describe-briefly)
  (define-key gnus-Subject-mode-map "\C-c\C-i" 'gnus-Info-find-node))

(defun gnus-Subject-mode ()
  "Major mode for reading articles in this newsgroup.
All normal editing commands are turned off.
Instead, these commands are available:
\\{gnus-Subject-mode-map}

User customizable variables:
 gnus-large-newsgroup
    The number of articles which indicates a large newsgroup. If the
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

	gnus-Subject-save-in-rmail (in Rmail format)
	gnus-Subject-save-in-mail (in Unix mail format)
	gnus-Subject-save-in-folder (in MH folder)
	gnus-Subject-save-in-file (in article format).

 gnus-rmail-save-name
 gnus-mail-save-name
 gnus-folder-save-name
 gnus-file-save-name
    Specifies a function generating a file name to save articles in
    specified format.  The function is called with NEWSGROUP, HEADERS,
    and optional LAST-FILE.  Access macros to the headers are defined
    as nntp-header-FIELD, and functions are defined as `gnus-header-FIELD'.

 gnus-article-save-directory
    Specifies a directory name to save articles to using the commands
    `gnus-Subject-save-in-rmail', `gnus-Subject-save-in-mail' and
    `gnus-Subject-save-in-file'.  The variable is initialized from the
    SAVEDIR environment variable.

 gnus-show-all-headers
    Non-nil means that all headers of an article are shown.

 gnus-save-all-headers
    Non-nil means that all headers of an article are saved in a file.

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
    in the Subject buffer.  The function is called with an article
    HEADERS.  The result must be a string excluding `[' and `]'.  The
    default function returns a string like NNN:AUTHOR, where NNN is
    the number of lines in an article and AUTHOR is the name of the
    author.

 gnus-auto-extend-newsgroup
    Non-nil means visible articles are extended to forward and
    backward automatically if possible.

 gnus-auto-select-first
    Non-nil means the first unread article is selected automagically
    when a newsgroup is selected normally (by gnus-Group-read-group).
    If you'd like to prevent automatic selection of the first unread
    article in some newsgroups, set the variable to nil in
    gnus-Select-group-hook or gnus-Apply-kill-hook.

 gnus-auto-select-next
    Non-nil means the next newsgroup is selected automagically at the
    end of the newsgroup. If the value is t and the next newsgroup is
    empty (no unread articles), GNUS will exit Subject mode and go
    back to Group mode. If the value is neither nil nor t, GNUS won't
    exit Subject mode but select the following unread newsgroup.
    Especially, if the value is the symbol `quietly', the next unread
    newsgroup will be selected without any confirmations.

 gnus-auto-select-same
    Non-nil means an article with the same subject as the current
    article is selected automagically like `rn -S'.

 gnus-auto-center-subject
    Non-nil means the point of Subject Mode window is always kept
    centered.

 gnus-break-pages
    Non-nil means an article is broken into pages at page delimiters.
    This may not work with some versions of GNU Emacs earlier than
    version 18.50.

 gnus-page-delimiter
    Specifies a regexp describing line-beginnings that separate pages
    of news article.

 [gnus-more-message is obsolete.  overlay-arrow-string interfares
    with other subsystems, such as dbx mode.]

 gnus-digest-show-summary
    Non-nil means that a summary of digest messages is shown when
    reading a digest article using `gnus-Subject-rmail-digest' command.

 gnus-digest-separator
    Specifies a regexp separating messages in a digest article.

 gnus-mail-reply-method
 gnus-mail-other-window-method
    Specifies a function to begin composing mail message using
    commands gnus-Subject-mail-reply and
    gnus-Subject-mail-other-window.  Functions
    gnus-mail-reply-using-mail and gnus-mail-reply-using-mhe are
    available for the value of gnus-mail-reply-method.  And functions
    gnus-mail-other-window-using-mail and
    gnus-mail-other-window-using-mhe are available for the value of
    gnus-mail-other-window-method.

Various hooks for customization:
 gnus-Subject-mode-hook
    Entry to this mode calls the value with no arguments, if that
    value is non-nil.

 gnus-Select-group-hook
    Called with no arguments when newsgroup is selected, if that value
    is non-nil.  It is possible to sort subjects in this hook.  See the
    documentation of this variable for more information.

 gnus-Subject-prepare-hook
    Called with no arguments after a subject list is created in the
    Subject buffer, if that value is non-nil.  If you'd like to modify
    the buffer, you can use this hook.

 gnus-Select-article-hook
    Called with no arguments when an article is selected, if that
    value is non-nil.  See the documentation of this variable for
    more information.

 gnus-Select-digest-hook
    Called with no arguments when reading digest messages using Rmail,
    if that value is non-nil.  This hook can be used to modify an
    article so that Rmail can work with it.  See the documentation of
    the variable for more information.

 gnus-Rmail-digest-hook
    Called with no arguments when reading digest messages using Rmail,
    if that value is non-nil.  This hook is intended to customize Rmail
    mode.

 gnus-Apply-kill-hook
    Called with no arguments when a newsgroup is selected and the
    Subject buffer is prepared.  This hook is intended to apply a KILL
    file to the selected newsgroup.  The format of KILL file is
    completely different from that of version 3.8. You have to rewrite
    them in the new format.  See the documentation of Kill file mode
    for more information.

 gnus-Mark-article-hook
    Called with no arguments when an article is selected at the first
    time.  The hook is intended to mark an article as read (or unread)
    automatically when it is selected.  See the documentation of the
    variable for more information.

 gnus-Exit-group-hook
    Called with no arguments when exiting the current newsgroup, if
    that value is non-nil.  If your machine is so slow that exiting
    from Subject mode takes very long time, inhibit marking articles
    as read using cross-references by setting the variable
    `gnus-newsgroup-headers' to nil in this hook."
  (interactive)
  (kill-all-local-variables)
  ;; Gee.  Why don't you upgrade?
  (cond ((boundp 'mode-line-modified)
	 (setq mode-line-modified "--- "))
	((listp (default-value 'mode-line-format))
	 (setq mode-line-format
	       (cons "--- " (cdr (default-value 'mode-line-format))))))
  (make-local-variable 'global-mode-string)
  (setq global-mode-string nil)
  (setq major-mode 'gnus-Subject-mode)
  (setq mode-name "Subject")
  ;;(setq mode-line-process '(" " gnus-newsgroup-name))
  (make-local-variable 'minor-mode-alist)
  (or (assq 'gnus-show-threads minor-mode-alist)
      (setq minor-mode-alist
	    (cons (list 'gnus-show-threads " Thread") minor-mode-alist)))
  (gnus-Subject-set-mode-line)
  (use-local-map gnus-Subject-mode-map)
  (buffer-flush-undo (current-buffer))
  (setq buffer-read-only t)		;Disable modification
  (setq truncate-lines t)		;Stop line folding
  (setq selective-display t)
  (setq selective-display-ellipses t)	;Display `...'
  ;;(setq case-fold-search t)
  (run-hooks 'gnus-Subject-mode-hook))

(defun gnus-Subject-setup-buffer ()
  "Initialize subject display buffer."
  (if (get-buffer gnus-Subject-buffer)
      (set-buffer gnus-Subject-buffer)
    (set-buffer (get-buffer-create gnus-Subject-buffer))
    (gnus-Subject-mode)
    ))

(defun gnus-Subject-read-group (group &optional show-all no-article)
  "Start reading news in newsgroup GROUP.
If optional first argument SHOW-ALL is non-nil, already read articles are
also listed.
If optional second argument NO-ARTICLE is non-nil, no article is selected
initially."
  (message "Retrieving newsgroup: %s..." group)
  (if (gnus-select-newsgroup group show-all)
      (progn
	;; Don't switch-to-buffer to prevent displaying old contents
	;;  of the buffer until new subjects list is created.
	;; Suggested by Juha Heinanen <jh@tut.fi>
	(gnus-Subject-setup-buffer)
	;; You can change the order of subjects in this hook.
	(run-hooks 'gnus-Select-group-hook)
	(gnus-Subject-prepare)
	;; Function `gnus-apply-kill-file' must be called in this hook.
	(run-hooks 'gnus-Apply-kill-hook)
	(if (zerop (buffer-size))
	    ;; This newsgroup is empty.
	    (progn
	      (gnus-Subject-catch-up-and-exit nil t) ;Without confirmations.
	      (message "No unread news"))
	  ;; Hide conversation thread subtrees.  We cannot do this in
	  ;; gnus-Subject-prepare-hook since kill processing may not
	  ;; work with hidden articles.
	  (and gnus-show-threads
	       gnus-thread-hide-subtree
	       (gnus-Subject-hide-all-threads))
	  ;; Show first unread article if requested.
	  (goto-char (point-min))
	  (if (and (not no-article)
		   gnus-auto-select-first
		   (gnus-Subject-first-unread-article))
	      ;; Window is configured automatically.
	      ;; Current buffer may be changed as a result of hook
	      ;; evaluation, especially by gnus-Subject-rmail-digest
	      ;; command, so we should adjust cursor point carefully.
	      (if (eq (current-buffer) (get-buffer gnus-Subject-buffer))
		  (progn
		    ;; Adjust cursor point.
		    (beginning-of-line)
		    (search-forward ":" nil t)))
	    (gnus-configure-windows 'SelectNewsgroup)
	    (pop-to-buffer gnus-Subject-buffer)
	    (gnus-Subject-set-mode-line)
	    ;; I sometime get confused with the old Article buffer.
	    (if (get-buffer gnus-Article-buffer)
		(if (get-buffer-window gnus-Article-buffer)
		    (save-excursion
		      (set-buffer gnus-Article-buffer)
		      (let ((buffer-read-only nil))
			(erase-buffer)))
		  (kill-buffer gnus-Article-buffer)))
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
	  (message "Cannot select %s.  May be security or active file problem." group)
	  (sit-for 0))
      ;; Check bogus newsgroups.
      ;; We must be in Group Mode buffer.
      (gnus-Group-check-bogus-groups))
    ))

(defun gnus-Subject-prepare ()
  "Prepare subject list of current newsgroup in Subject mode buffer."
  (let ((buffer-read-only nil))
    ;; Note: The next codes are not actually used because the user who
    ;; want it can define them in gnus-Select-group-hook.
    ;; Print verbose messages if too many articles are selected.
    ;;    (and (numberp gnus-large-newsgroup)
    ;;       (> (length gnus-newsgroup-headers) gnus-large-newsgroup)
    ;;       (message "Preparing headers..."))
    (erase-buffer)
    (gnus-Subject-prepare-threads
     (if gnus-show-threads
	 (gnus-make-threads gnus-newsgroup-headers)
       gnus-newsgroup-headers) 0)
    ;; Erase header retrieval message.
    (message "")
    ;; Call hooks for modifying Subject mode buffer.
    ;; Suggested by sven@tde.LTH.Se (Sven Mattisson).
    (goto-char (point-min))
    (run-hooks 'gnus-Subject-prepare-hook)
    ))

;; Basic ideas by Paul Dworkin <paul@media-lab.media.mit.edu>

(defun gnus-Subject-prepare-threads (threads level)
  "Prepare Subject buffer from THREADS and indentation LEVEL.
THREADS is a list of `(PARENT [(CHILD1 [(GRANDCHILD ...]...) ...]).'"
  (let ((thread nil)
	(header nil)
	(number nil)
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
				     (not gnus-thread-hide-subject))
				 nil
			       (make-string (window-width) ? ))
			     (nntp-header-subject header))
		     ))
	    ))
      ;; Print subthreads.
      (and (consp thread)
	   (cdr thread)
	   (gnus-Subject-prepare-threads (cdr thread) (1+ level)))
      )))

(defun gnus-Subject-set-mode-line ()
  "Set Subject mode line string."
  ;; The value must be a string to escape %-constructs.
  (let ((subject
	 (if gnus-current-headers
	     (nntp-header-subject gnus-current-headers) gnus-newsgroup-name)))
    (setq mode-line-buffer-identification
	  (concat "GNUS: "
		  subject
		  ;; Enough spaces to pad subject to 17 positions.
		  (make-string (max 0 (- 17 (length subject))) ? ))))
  (set-buffer-modified-p t))

;; GNUS Subject mode command.

(defun gnus-Subject-search-group (&optional backward)
  "Search for next unread newsgroup.
If optional argument BACKWARD is non-nil, search backward instead."
  (save-excursion
    (set-buffer gnus-Group-buffer)
    (save-excursion
      ;; We don't want to alter current point of Group mode buffer.
      (if (gnus-Group-search-forward backward nil)
	  (gnus-Group-group-name))
      )))

(defun gnus-Subject-search-subject (backward unread subject)
  "Search for article forward.
If first argument BACKWARD is non-nil, search backward.
If second argument UNREAD is non-nil, only unread article is selected.
If third argument SUBJECT is non-nil, the article which has
the same subject will be searched for."
  (let ((func (if backward 're-search-backward 're-search-forward))
	(article nil)
	;; We have to take care of hidden lines.
	(regexp 
	 (format "^%s[ \t]+\\([0-9]+\\):.\\[[^]\r\n]*\\][ \t]+%s"
		 ;;(if unread " " ".")
		 (cond ((eq unread t) " ") (unread "[ ---]") (t "."))
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

(defun gnus-Subject-search-forward (&optional unread subject)
  "Search for article forward.
If first optional argument UNREAD is non-nil, only unread article is selected.
If second optional argument SUBJECT is non-nil, the article which has
the same subject will be searched for."
  (gnus-Subject-search-subject nil unread subject))

(defun gnus-Subject-search-backward (&optional unread subject)
  "Search for article backward.
If first optional argument UNREAD is non-nil, only unread article is selected.
If second optional argument SUBJECT is non-nil, the article which has
the same subject will be searched for."
  (gnus-Subject-search-subject t unread subject))

(defun gnus-Subject-article-number ()
  "Article number around point. If nothing, return current number."
  (save-excursion
    (beginning-of-line)
    (if (looking-at ".[ \t]+\\([0-9]+\\):")
	(string-to-int
	 (buffer-substring (match-beginning 1) (match-end 1)))
      ;; If search fail, return current article number.
      gnus-current-article
      )))

(defun gnus-Subject-subject-string ()
  "Return current subject string or nil if nothing."
  (save-excursion
    ;; It is possible to implement this function using
    ;;  `gnus-Subject-article-number' and `gnus-newsgroup-headers'.
    (beginning-of-line)
    ;; We have to take care of hidden lines.
    (if (looking-at ".[ \t]+[0-9]+:.\\[[^]\r\n]*\\][ \t]+\\([^\r\n]*\\)[\r\n]")
	(buffer-substring (match-beginning 1) (match-end 1)))
    ))

(defun gnus-Subject-goto-subject (article)
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

(defun gnus-Subject-recenter ()
  "Center point in Subject mode window."
  ;; Scroll window so as to cursor comes center of Subject mode window
  ;;  only when article is displayed.
  ;; Suggested by earle@mahendo.JPL.NASA.GOV (Greg Earle).
  ;; Recenter only when requested.
  ;; Suggested by popovich@park.cs.columbia.edu
  (and gnus-auto-center-subject
       (get-buffer-window gnus-Article-buffer)
       (< (/ (- (window-height) 1) 2)
	  (count-lines (point) (point-max)))
       (recenter (/ (- (window-height) 2) 2))))

;; Walking around Group mode buffer.

(defun gnus-Subject-jump-to-group (newsgroup)
  "Move point to NEWSGROUP in Group mode buffer."
  ;; Keep update point of Group mode buffer if visible.
  (if (eq (current-buffer)
	  (get-buffer gnus-Group-buffer))
      (save-window-excursion
	;; Take care of tree window mode.
	(if (get-buffer-window gnus-Group-buffer)
	    (pop-to-buffer gnus-Group-buffer))
	(gnus-Group-jump-to-group newsgroup))
    (save-excursion
      ;; Take care of tree window mode.
      (if (get-buffer-window gnus-Group-buffer)
	  (pop-to-buffer gnus-Group-buffer)
	(set-buffer gnus-Group-buffer))
      (gnus-Group-jump-to-group newsgroup))))

(defun gnus-Subject-next-group (no-article)
  "Exit current newsgroup and then select next unread newsgroup.
If prefix argument NO-ARTICLE is non-nil, no article is selected initially."
  (interactive "P")
  ;; Make sure Group mode buffer point is on current newsgroup.
  (gnus-Subject-jump-to-group gnus-newsgroup-name)
  (let ((group (gnus-Subject-search-group)))
    (if (null group)
	(progn
	  (message "Exiting %s..." gnus-newsgroup-name)  
	  (gnus-Subject-exit)
	  (message ""))
      (message "Selecting %s..." group)
      (gnus-Subject-exit t)		;Exit Subject mode temporary.
      ;; We are now in Group mode buffer.
      ;; Make sure Group mode buffer point is on GROUP.
      (gnus-Subject-jump-to-group group)
      (gnus-Subject-read-group group nil no-article)
      (or (eq (current-buffer)
	      (get-buffer gnus-Subject-buffer))
	  (eq gnus-auto-select-next t)
	  ;; Expected newsgroup has nothing to read since the articles
	  ;; are marked as read by cross-referencing. So, try next
	  ;; newsgroup. (Make sure we are in Group mode buffer now.)
	  (and (eq (current-buffer)
		   (get-buffer gnus-Group-buffer))
	       (gnus-Group-group-name)
	       (gnus-Subject-read-group
		(gnus-Group-group-name) nil no-article))
	  )
      )))

(defun gnus-Subject-prev-group (no-article)
  "Exit current newsgroup and then select previous unread newsgroup.
If prefix argument NO-ARTICLE is non-nil, no article is selected initially."
  (interactive "P")
  ;; Make sure Group mode buffer point is on current newsgroup.
  (gnus-Subject-jump-to-group gnus-newsgroup-name)
  (let ((group (gnus-Subject-search-group t)))
    (if (null group)
	(progn
	  (message "Exiting %s..." gnus-newsgroup-name)  
	  (gnus-Subject-exit)
	  (message ""))
      (message "Selecting %s..." group)
      (gnus-Subject-exit t)		;Exit Subject mode temporary.
      ;; We are now in Group mode buffer.
      ;; We have to adjust point of Group mode buffer because current
      ;; point is moved to next unread newsgroup by exiting.
      (gnus-Subject-jump-to-group group)
      (gnus-Subject-read-group group nil no-article)
      (or (eq (current-buffer)
	      (get-buffer gnus-Subject-buffer))
	  (eq gnus-auto-select-next t)
	  ;; Expected newsgroup has nothing to read since the articles
	  ;; are marked as read by cross-referencing. So, try next
	  ;; newsgroup. (Make sure we are in Group mode buffer now.)
	  (and (eq (current-buffer)
		   (get-buffer gnus-Group-buffer))
	       (gnus-Subject-search-group t)
	       (gnus-Subject-read-group
		(gnus-Subject-search-group t) nil no-article))
	  )
      )))

;; Walking around subject lines.

(defun gnus-Subject-next-subject (n &optional unread)
  "Go to next N'th subject line.
If optional argument UNREAD is non-nil, only unread article is selected."
  (interactive "p")
  (while (and (> n 1)
	      (gnus-Subject-search-forward unread))
    (setq n (1- n)))
  (cond ((gnus-Subject-search-forward unread)
	 (gnus-Subject-recenter))
	(unread
	 (message "No more unread articles"))
	(t
	 (message "No more articles"))
	))

(defun gnus-Subject-next-unread-subject (n)
  "Go to next N'th unread subject line."
  (interactive "p")
  (gnus-Subject-next-subject n t))

(defun gnus-Subject-prev-subject (n &optional unread)
  "Go to previous N'th subject line.
If optional argument UNREAD is non-nil, only unread article is selected."
  (interactive "p")
  (while (and (> n 1)
	      (gnus-Subject-search-backward unread))
    (setq n (1- n)))
  (cond ((gnus-Subject-search-backward unread)
	 (gnus-Subject-recenter))
	(unread
	 (message "No more unread articles"))
	(t
	 (message "No more articles"))
	))

(defun gnus-Subject-prev-unread-subject (n)
  "Go to previous N'th unread subject line."
  (interactive "p")
  (gnus-Subject-prev-subject n t))

;; Walking around subject lines with displaying articles.

(defun gnus-Subject-expand-window ()
  "Expand Subject window to show headers full window."
  (interactive)
  (gnus-configure-windows 'ExpandSubject)
  (pop-to-buffer gnus-Subject-buffer))

(defun gnus-Subject-display-article (article &optional all-header)
  "Display ARTICLE in Article buffer."
  (if (null article)
      nil
    (gnus-configure-windows 'SelectArticle)
    (pop-to-buffer gnus-Subject-buffer)
    (gnus-Article-prepare article all-header)
    (gnus-Subject-recenter)
    (gnus-Subject-set-mode-line)
    (run-hooks 'gnus-Select-article-hook)
    ;; Successfully display article.
    t
    ))

(defun gnus-Subject-select-article (&optional all-headers force)
  "Select the current article.
Optional argument ALL-HEADERS is non-nil, show all headers."
  (let ((article (gnus-Subject-article-number)))
    (if (or (null gnus-current-article)
	    (/= article gnus-current-article)
	    (and force (not (eq all-headers gnus-have-all-headers))))
	;; The selected subject is different from that of the current article.
	(gnus-Subject-display-article article all-headers)
      (gnus-configure-windows 'SelectArticle)
      (pop-to-buffer gnus-Subject-buffer))
    ))

(defun gnus-Subject-set-current-mark (&optional current-mark)
  "Put `+' at the current article.
Optional argument specifies CURRENT-MARK instead of `+'."
  (save-excursion
    (set-buffer gnus-Subject-buffer)
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

(defun gnus-Subject-next-article (unread &optional subject)
  "Select article after current one.
If argument UNREAD is non-nil, only unread article is selected."
  (interactive "P")
  (let ((header nil))
    (cond ((gnus-Subject-display-article
	    (gnus-Subject-search-forward unread subject)))
	  ((and subject
		gnus-auto-select-same
		(gnus-set-difference gnus-newsgroup-unreads
				     gnus-newsgroup-marked)
		(memq this-command
		      '(gnus-Subject-next-unread-article
			gnus-Subject-next-page
			gnus-Subject-kill-same-subject-and-select
			;;gnus-Subject-next-article
			;;gnus-Subject-next-same-subject
			;;gnus-Subject-next-unread-same-subject
			)))
	   ;; Wrap article pointer if there are unread articles.
	   ;; Hook function, such as gnus-Subject-rmail-digest, may
	   ;; change current buffer, so need check.
	   (let ((buffer (current-buffer))
		 (last-point (point)))
	     ;; No more articles with same subject, so jump to the first
	     ;; unread article.
	     (gnus-Subject-first-unread-article)
	     ;;(and (eq buffer (current-buffer))
	     ;;	(= (point) last-point)
	     ;;	;; Ignore given SUBJECT, and try again.
	     ;;	(gnus-Subject-next-article unread nil))
	     (and (eq buffer (current-buffer))
		  (< (point) last-point)
		  (message "Wrapped"))
	     ))
	  ((and (not unread)
		gnus-auto-extend-newsgroup
		(setq header (gnus-more-header-forward)))
	   ;; Extend to next article if possible.
	   ;; Basic ideas by himacdonald@watdragon.waterloo.edu
	   (gnus-extend-newsgroup header nil)
	   ;; Threads feature must be turned off.
	   (let ((buffer-read-only nil))
	     (goto-char (point-max))
	     (gnus-Subject-prepare-threads (list header) 0))
	   (gnus-Subject-goto-article gnus-newsgroup-end))
	  (t
	   ;; Select next newsgroup automatically if requested.
	   (let ((cmd (string-to-char (this-command-keys)))
		 (group (gnus-Subject-search-group))
		 (auto-select
		  (and gnus-auto-select-next
		       ;;(null (gnus-set-difference gnus-newsgroup-unreads
		       ;;				gnus-newsgroup-marked))
		       (memq this-command
			     '(gnus-Subject-next-unread-article
			       gnus-Subject-next-article
			       gnus-Subject-next-page
			       gnus-Subject-next-same-subject
			       gnus-Subject-next-unread-same-subject
			       gnus-Subject-kill-same-subject
			       gnus-Subject-kill-same-subject-and-select
			       ))
		       ;; Ignore characters typed ahead.
		       (not (input-pending-p))
		       )))
	     (message "No more%s articles%s"
		      (if unread " unread" "")
		      (if (and auto-select
			       (not (eq gnus-auto-select-next 'quietly)))
			  (if group
			      (format " (Type %s to %s [%d])"
				      (key-description (char-to-string cmd))
				      group
				      (nth 1 (gnus-gethash group
							   gnus-unread-hashtb)))
			    (format " (Type %s to exit %s)"
				    (key-description (char-to-string cmd))
				    gnus-newsgroup-name
				    ))
			""))
	     ;; Select next unread newsgroup automagically.
	     (cond ((and auto-select
			 (eq gnus-auto-select-next 'quietly))
		    ;; Select quietly.
		    (gnus-Subject-next-group nil))
		   (auto-select
		    ;; Confirm auto selection.
		    (let ((char (read-char)))
		      (if (= char cmd)
			  (gnus-Subject-next-group nil)
			(setq unread-command-char char))))
		   )
	     ))
	  )))

(defun gnus-Subject-next-unread-article ()
  "Select unread article after current one."
  (interactive)
  (gnus-Subject-next-article t (and gnus-auto-select-same
				    (gnus-Subject-subject-string))))

(defun gnus-Subject-prev-article (unread &optional subject)
  "Select article before current one.
If argument UNREAD is non-nil, only unread article is selected."
  (interactive "P")
  (let ((header nil))
    (cond ((gnus-Subject-display-article
	    (gnus-Subject-search-backward unread subject)))
	  ((and subject
		gnus-auto-select-same
		(gnus-set-difference gnus-newsgroup-unreads
				     gnus-newsgroup-marked)
		(memq this-command
		      '(gnus-Subject-prev-unread-article
			;;gnus-Subject-prev-page
			;;gnus-Subject-prev-article
			;;gnus-Subject-prev-same-subject
			;;gnus-Subject-prev-unread-same-subject
			)))
	   ;; Ignore given SUBJECT, and try again.
	   (gnus-Subject-prev-article unread nil))
	  (unread
	   (message "No more unread articles"))
	  ((and gnus-auto-extend-newsgroup
		(setq header (gnus-more-header-backward)))
	   ;; Extend to previous article if possible.
	   ;; Basic ideas by himacdonald@watdragon.waterloo.edu
	   (gnus-extend-newsgroup header t)
	   (let ((buffer-read-only nil))
	     (goto-char (point-min))
	     (gnus-Subject-prepare-threads (list header) 0))
	   (gnus-Subject-goto-article gnus-newsgroup-begin))
	  (t
	   (message "No more articles"))
	  )))

(defun gnus-Subject-prev-unread-article ()
  "Select unred article before current one."
  (interactive)
  (gnus-Subject-prev-article t (and gnus-auto-select-same
				    (gnus-Subject-subject-string))))

(defun gnus-Subject-next-page (lines)
  "Show next page of selected article.
If end of artile, select next article.
Argument LINES specifies lines to be scrolled up."
  (interactive "P")
  (let ((article (gnus-Subject-article-number))
	(endp nil))
    (if (or (null gnus-current-article)
	    (/= article gnus-current-article))
	;; Selected subject is different from current article's.
	(gnus-Subject-display-article article)
      (gnus-configure-windows 'SelectArticle)
      (pop-to-buffer gnus-Subject-buffer)
      (gnus-eval-in-buffer-window gnus-Article-buffer
	(setq endp (gnus-Article-next-page lines)))
      (cond ((and endp lines)
	     (message "End of message"))
	    ((and endp (null lines))
	     (gnus-Subject-next-unread-article)))
      )))

(defun gnus-Subject-prev-page (lines)
  "Show previous page of selected article.
Argument LINES specifies lines to be scrolled down."
  (interactive "P")
  (let ((article (gnus-Subject-article-number)))
    (if (or (null gnus-current-article)
	    (/= article gnus-current-article))
	;; Selected subject is different from current article's.
	(gnus-Subject-display-article article)
      (gnus-configure-windows 'SelectArticle)
      (pop-to-buffer gnus-Subject-buffer)
      (gnus-eval-in-buffer-window gnus-Article-buffer
	(gnus-Article-prev-page lines))
      )))

(defun gnus-Subject-scroll-up (lines)
  "Scroll up (or down) one line current article.
Argument LINES specifies lines to be scrolled up (or down if negative)."
  (interactive "p")
  (gnus-Subject-select-article)
  (gnus-eval-in-buffer-window gnus-Article-buffer
    (cond ((> lines 0)
	   (if (gnus-Article-next-page lines)
	       (message "End of message")))
	  ((< lines 0)
	   (gnus-Article-prev-page (- 0 lines))))
    ))

(defun gnus-Subject-next-same-subject ()
  "Select next article which has the same subject as current one."
  (interactive)
  (gnus-Subject-next-article nil (gnus-Subject-subject-string)))

(defun gnus-Subject-prev-same-subject ()
  "Select previous article which has the same subject as current one."
  (interactive)
  (gnus-Subject-prev-article nil (gnus-Subject-subject-string)))

(defun gnus-Subject-next-unread-same-subject ()
  "Select next unread article which has the same subject as current one."
  (interactive)
  (gnus-Subject-next-article t (gnus-Subject-subject-string)))

(defun gnus-Subject-prev-unread-same-subject ()
  "Select previous unread article which has the same subject as current one."
  (interactive)
  (gnus-Subject-prev-article t (gnus-Subject-subject-string)))

(defun gnus-Subject-refer-parent-article (child)
  "Refer parent article of current article.
If a prefix argument CHILD is non-nil, go back to the child article
using internally maintained articles history.
NOTE: This command may not work with nnspool.el."
  (interactive "P")
  (gnus-Subject-select-article t t)	;Request all headers.
  (let ((referenced-id nil))		;Message-id of parent or child article.
    (if child
	;; Go back to child article using history.
	(gnus-Subject-refer-article nil)
      (gnus-eval-in-buffer-window gnus-Article-buffer
	;; Look for parent Message-ID.
	;; We cannot use gnus-current-headers to get references
	;; because we may be looking at parent or refered article.
	(let ((references (gnus-fetch-field "References")))
	  ;; Get the last message-id in the references.
	  (and references
	       (string-match "\\(<[^<>]+>\\)[^>]*\\'" references)
	       (setq referenced-id
		     (substring references
				(match-beginning 1) (match-end 1))))
	  ))
      (if (stringp referenced-id)
	  (gnus-Subject-refer-article referenced-id)
	(error "No more parents"))
      )))

(defun gnus-Subject-refer-article (message-id)
  "Refer article specified by MESSAGE-ID.
If MESSAGE-ID is nil or an empty string, it is popped from an
internally maintained articles history.
NOTE: This command may not work with nnspool.el."
  (interactive "sMessage-ID: ")
  ;; Make sure that this command depends on the fact that article
  ;; related information is not updated when an article is retrieved
  ;; by Message-ID.
  (gnus-Subject-select-article t t)	;Request all headers.
  (if (and (stringp message-id)
	   (> (length message-id) 0))
      (gnus-eval-in-buffer-window gnus-Article-buffer
	;; Construct the correct Message-ID if necessary.
	;; Suggested by tale@pawl.rpi.edu.
	(or (string-match "^<" message-id)
	    (setq message-id (concat "<" message-id)))
	(or (string-match ">$" message-id)
	    (setq message-id (concat message-id ">")))
	;; Push current message-id on history.
	;; We cannot use gnus-current-headers to get current
	;; message-id because we may be looking at parent or refered
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
      ;; Retrieve article by message-id.  This may not work with nnspool.
      (gnus-Article-prepare message-id t)
    (error "No such references"))
  )

(defun gnus-Subject-next-digest (nth)
  "Move to head of NTH next digested message."
  (interactive "p")
  (gnus-Subject-select-article)
  (gnus-eval-in-buffer-window gnus-Article-buffer
    (gnus-Article-next-digest (or nth 1))
    ))

(defun gnus-Subject-prev-digest (nth)
  "Move to head of NTH previous digested message."
  (interactive "p")
  (gnus-Subject-select-article)
  (gnus-eval-in-buffer-window gnus-Article-buffer
    (gnus-Article-prev-digest (or nth 1))
    ))

(defun gnus-Subject-first-unread-article ()
  "Select first unread article.  Return non-nil if successfully selected."
  (interactive)
  (let ((begin (point)))
    (goto-char (point-min))
    (if (re-search-forward "^ [ \t]+[0-9]+:" nil t)
	(gnus-Subject-display-article (gnus-Subject-article-number))
      ;; If there is no unread articles, stay there.
      (goto-char begin)
      ;;(gnus-Subject-display-article (gnus-Subject-article-number))
      (message "No more unread articles")
      nil
      )
    ))

(defun gnus-Subject-isearch-article ()
  "Do incremental search forward on current article."
  (interactive)
  (gnus-Subject-select-article)
  (gnus-eval-in-buffer-window gnus-Article-buffer
    (call-interactively 'isearch-forward)
    ))

(defun gnus-Subject-search-article-forward (regexp)
  "Search for an article containing REGEXP forward.
`gnus-Select-article-hook' is not called during the search."
  (interactive
   (list (read-string
	  (concat "Search forward (regexp): "
		  (if gnus-last-search-regexp
		      (concat "(default " gnus-last-search-regexp ") "))))))
  (if (string-equal regexp "")
      (setq regexp (or gnus-last-search-regexp ""))
    (setq gnus-last-search-regexp regexp))
  (if (gnus-Subject-search-article regexp nil)
      (gnus-eval-in-buffer-window gnus-Article-buffer
	(recenter 0)
	;;(sit-for 1)
	)
    (error "Search failed: \"%s\"" regexp)
    ))

(defun gnus-Subject-search-article-backward (regexp)
  "Search for an article containing REGEXP backward.
`gnus-Select-article-hook' is not called during the search."
  (interactive
   (list (read-string
	  (concat "Search backward (regexp): "
		  (if gnus-last-search-regexp
		      (concat "(default " gnus-last-search-regexp ") "))))))
  (if (string-equal regexp "")
      (setq regexp (or gnus-last-search-regexp ""))
    (setq gnus-last-search-regexp regexp))
  (if (gnus-Subject-search-article regexp t)
      (gnus-eval-in-buffer-window gnus-Article-buffer
	(recenter 0)
	;;(sit-for 1)
	)
    (error "Search failed: \"%s\"" regexp)
    ))

(defun gnus-Subject-search-article (regexp &optional backward)
  "Search for an article containing REGEXP.
Optional argument BACKWARD means do search for backward.
`gnus-Select-article-hook' is not called during the search."
  (let ((gnus-Select-article-hook nil)	;Disable hook.
	(gnus-Mark-article-hook nil)	;Inhibit marking as read.
	(re-search
	 (if backward
	     (function re-search-backward) (function re-search-forward)))
	(found nil)
	(last nil))
    ;; Hidden thread subtrees must be searched for ,too.
    (gnus-Subject-show-all-threads)
    ;; First of all, search current article.
    ;; We don't want to read article again from NNTP server nor reset
    ;; current point.
    (gnus-Subject-select-article)
    (message "Searching article: %d..." gnus-current-article)
    (setq last gnus-current-article)
    (gnus-eval-in-buffer-window gnus-Article-buffer
      (save-restriction
	(widen)
	;; Begin search from current point.
	(setq found (funcall re-search regexp nil t))))
    ;; Then search next articles.
    (while (and (not found)
		(gnus-Subject-display-article 
		 (gnus-Subject-search-subject backward nil nil)))
      (message "Searching article: %d..." gnus-current-article)
      (gnus-eval-in-buffer-window gnus-Article-buffer
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

(defun gnus-Subject-execute-command (field regexp command &optional backward)
  "If FIELD of article header matches REGEXP, execute COMMAND string.
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
  (gnus-Subject-show-all-threads)
  ;; We don't want to change current point nor window configuration.
  (save-excursion
    (save-window-excursion
      (message "Executing %s..." (key-description command))
      ;; We'd like to execute COMMAND interactively so as to give arguments.
      (gnus-execute field regexp
		    (` (lambda ()
			 (call-interactively '(, (key-binding command)))))
		    backward)
      (message "Executing %s... done" (key-description command)))))

(defun gnus-Subject-beginning-of-article ()
  "Go to beginning of article body"
  (interactive)
  (gnus-Subject-select-article)
  (gnus-eval-in-buffer-window gnus-Article-buffer
    (widen)
    (beginning-of-buffer)
    (if gnus-break-pages
	(gnus-narrow-to-page))
    ))

(defun gnus-Subject-end-of-article ()
  "Go to end of article body"
  (interactive)
  (gnus-Subject-select-article)
  (gnus-eval-in-buffer-window gnus-Article-buffer
    (widen)
    (end-of-buffer)
    (if gnus-break-pages
	(gnus-narrow-to-page))
    ))

(defun gnus-Subject-goto-article (article &optional all-headers)
  "Read ARTICLE if exists.
Optional argument ALL-HEADERS means all headers are shown."
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
  (if (gnus-Subject-goto-subject article)
      (gnus-Subject-display-article article all-headers)))

(defun gnus-Subject-goto-last-article ()
  "Go to last subject line."
  (interactive)
  (if gnus-last-article
      (gnus-Subject-goto-article gnus-last-article)))

(defun gnus-Subject-show-article ()
  "Force to show current article."
  (interactive)
  ;; The following is a trick to force to read the current article again.
  (setq gnus-have-all-headers (not gnus-have-all-headers))
  (gnus-Subject-select-article (not gnus-have-all-headers) t))

(defun gnus-Subject-toggle-header (arg)
  "Show original header if pruned header currently shown, or vice versa.
With arg, show original header iff arg is positive."
  (interactive "P")
  ;; Variable gnus-show-all-headers must be NIL to toggle really.
  (let ((gnus-show-all-headers nil)
	(all-headers
	 (if (null arg) (not gnus-have-all-headers)
	   (> (prefix-numeric-value arg) 0))))
    (gnus-Subject-select-article all-headers t)))

(defun gnus-Subject-show-all-headers ()
  "Show original article header."
  (interactive)
  (gnus-Subject-select-article t t))

(defun gnus-Subject-stop-page-breaking ()
  "Stop page breaking by linefeed temporary (Widen article buffer)."
  (interactive)
  (gnus-Subject-select-article)
  (gnus-eval-in-buffer-window gnus-Article-buffer
    (widen)))

(defun gnus-Subject-kill-same-subject-and-select (unmark)
  "Mark articles which has the same subject as read, and then select next.
If argument UNMARK is positive, remove any kinds of marks.
If argument UNMARK is negative, mark articles as unread instead."
  (interactive "P")
  (if unmark
      (setq unmark (prefix-numeric-value unmark)))
  (let ((count
	 (gnus-Subject-mark-same-subject
	  (gnus-Subject-subject-string) unmark)))
    ;; Select next unread article. If auto-select-same mode, should
    ;; select the first unread article.
    (gnus-Subject-next-article t (and gnus-auto-select-same
				      (gnus-Subject-subject-string)))
    (message "%d articles are marked as %s"
	     count (if unmark "unread" "read"))
    ))

(defun gnus-Subject-kill-same-subject (unmark)
  "Mark articles which has the same subject as read. 
If argument UNMARK is positive, remove any kinds of marks.
If argument UNMARK is negative, mark articles as unread instead."
  (interactive "P")
  (if unmark
      (setq unmark (prefix-numeric-value unmark)))
  (let ((count
	 (gnus-Subject-mark-same-subject
	  (gnus-Subject-subject-string) unmark)))
    ;; If marked as read, go to next unread subject.
    (if (null unmark)
	;; Go to next unread subject.
	(gnus-Subject-next-subject 1 t))
    (message "%d articles are marked as %s"
	     count (if unmark "unread" "read"))
    ))

(defun gnus-Subject-mark-same-subject (subject &optional unmark)
  "Mark articles with same SUBJECT as read, and return marked number.
If optional argument UNMARK is positive, remove any kinds of marks.
If optional argument UNMARK is negative, mark articles as unread instead."
  (let ((count 1))
    (save-excursion
      (cond ((null unmark)
	     (gnus-Subject-mark-as-read nil "K"))
	    ((> unmark 0)
	     (gnus-Subject-mark-as-unread nil t))
	    (t
	     (gnus-Subject-mark-as-unread)))
      (while (and subject
		  (gnus-Subject-search-forward nil subject))
	(cond ((null unmark)
	       (gnus-Subject-mark-as-read nil "K"))
	      ((> unmark 0)
	       (gnus-Subject-mark-as-unread nil t))
	      (t
	       (gnus-Subject-mark-as-unread)))
	(setq count (1+ count))
	))
    ;; Hide killed thread subtrees.  Does not work properly always.
    ;;(and (null unmark)
    ;;     gnus-thread-hide-killed
    ;;	   (gnus-Subject-hide-thread))
    ;; Return number of articles marked as read.
    count
    ))

(defun gnus-Subject-mark-as-unread-forward (count)
  "Mark current article as unread, and then go forward.
Argument COUNT specifies number of articles marked as unread."
  (interactive "p")
  (while (> count 0)
    (gnus-Subject-mark-as-unread nil nil)
    (gnus-Subject-next-subject 1 nil)
    (setq count (1- count))))

(defun gnus-Subject-mark-as-unread-backward (count)
  "Mark current article as unread, and then go backward.
Argument COUNT specifies number of articles marked as unread."
  (interactive "p")
  (while (> count 0)
    (gnus-Subject-mark-as-unread nil nil)
    (gnus-Subject-prev-subject 1 nil)
    (setq count (1- count))))

(defun gnus-Subject-mark-as-unread (&optional article clear-mark)
  "Mark current article as unread.
Optional first argument ARTICLE specifies article number to be
marked as unread.  Optional second argument CLEAR-MARK removes
any kind of mark."
  (save-excursion
    (set-buffer gnus-Subject-buffer)
    ;; First of all, show hidden thread subtrees.
    (gnus-Subject-show-thread)
    (let* ((buffer-read-only nil)
	   (current (gnus-Subject-article-number))
	   (article (or article current)))
      (gnus-mark-article-as-unread article clear-mark)
      (if (or (eq article current)
	      (gnus-Subject-goto-subject article))
	  (progn
	    (beginning-of-line)
	    (delete-char 1)
	    (insert (if clear-mark " " "-"))))
      )))

(defun gnus-Subject-mark-as-read-forward (count)
  "Mark current article as read, and then go forward.
Argument COUNT specifies number of articles marked as read"
  (interactive "p")
  (while (> count 0)
    (gnus-Subject-mark-as-read)
    (gnus-Subject-next-subject 1 'unread-only)
    (setq count (1- count))))

(defun gnus-Subject-mark-as-read-backward (count)
  "Mark current article as read, and then go backward.
Argument COUNT specifies number of articles marked as read"
  (interactive "p")
  (while (> count 0)
    (gnus-Subject-mark-as-read)
    (gnus-Subject-prev-subject 1 'unread-only)
    (setq count (1- count))))

(defun gnus-Subject-mark-as-read (&optional article mark)
  "Mark current article as read.
Optional first argument ARTICLE specifies article number to be marked as read.
Optional second argument MARK specifies a string inserted at beginning of line.
Any kind of string (length 1) except for a space and `-' is ok."
  (save-excursion
    (set-buffer gnus-Subject-buffer)
    ;; First of all, show hidden thread subtrees.
    (gnus-Subject-show-thread)
    (let* ((buffer-read-only nil)
	   (mark (or mark "D"))		;Default mark is `D'.
	   (current (gnus-Subject-article-number))
	   (article (or article current)))
      (gnus-mark-article-as-read article)
      (if (or (eq article current)
	      (gnus-Subject-goto-subject article))
	  (progn
	    (beginning-of-line)
	    (delete-char 1)
	    (insert mark)))
      )))

(defun gnus-Subject-clear-mark-forward (count)
  "Remove current article's mark, and go forward.
Argument COUNT specifies number of articles unmarked"
  (interactive "p")
  (while (> count 0)
    (gnus-Subject-mark-as-unread nil t)
    (gnus-Subject-next-subject 1 nil)
    (setq count (1- count))))

(defun gnus-Subject-clear-mark-backward (count)
  "Remove current article's mark, and go backward.
Argument COUNT specifies number of articles unmarked"
  (interactive "p")
  (while (> count 0)
    (gnus-Subject-mark-as-unread nil t)
    (gnus-Subject-prev-subject 1 nil)
    (setq count (1- count))))

(defun gnus-Subject-delete-marked-as-read ()
  "Delete lines which are marked as read."
  (interactive)
  (if gnus-newsgroup-unreads
      (let ((buffer-read-only nil))
	(save-excursion
	  (goto-char (point-min))
	  (delete-non-matching-lines "^[ ---]"))
	;; Adjust point.
	(if (eobp)
	    (gnus-Subject-prev-subject 1)
	  (beginning-of-line)
	  (search-forward ":" nil t)))
    ;; It is not so good idea to make the buffer empty.
    (message "All articles are marked as read")
    ))

(defun gnus-Subject-delete-marked-with (marks)
  "Delete lines which are marked with MARKS (e.g. \"DK\")."
  (interactive "sMarks: ")
  (let ((buffer-read-only nil))
    (save-excursion
      (goto-char (point-min))
      (delete-matching-lines (concat "^[" marks "]")))
    ;; Adjust point.
    (or (zerop (buffer-size))
	(if (eobp)
	    (gnus-Subject-prev-subject 1)
	  (beginning-of-line)
	  (search-forward ":" nil t)))
    ))

;; Thread-based commands.

(defun gnus-Subject-toggle-threads (arg)
  "Toggle showing conversation threads.
With arg, turn showing conversation threads on iff arg is positive."
  (interactive "P")
  (let ((current (gnus-Subject-article-number)))
    (setq gnus-show-threads
	  (if (null arg) (not gnus-show-threads)
	    (> (prefix-numeric-value arg) 0)))
    (gnus-Subject-prepare)
    (gnus-Subject-goto-subject current)
    ))

(defun gnus-Subject-show-all-threads ()
  "Show all thread subtrees."
  (interactive)
  (if gnus-show-threads
      (save-excursion
	(let ((buffer-read-only nil))
	  (subst-char-in-region (point-min) (point-max) ?\^M ?\n t)
	  ))))

(defun gnus-Subject-show-thread ()
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

(defun gnus-Subject-hide-all-threads ()
  "Hide all thread subtrees."
  (interactive)
  (if gnus-show-threads
      (save-excursion
	;; Adjust cursor point.
	(goto-char (point-min))
	(search-forward ":" nil t)
	(let ((level (current-column)))
	  (gnus-Subject-hide-thread)
	  (while (gnus-Subject-search-forward)
	    (and (>= level (current-column))
		 (gnus-Subject-hide-thread)))
	  ))))

(defun gnus-Subject-hide-thread ()
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
	  (while (and (gnus-Subject-search-forward)
		      (< level (current-column)))
	    ;; Interested in lower levels.
	    (if (< level (current-column))
		(progn
		  (setq last (point))
		  ))
	    )
	  (subst-char-in-region init last ?\n ?\^M t)
	  ))))

(defun gnus-Subject-next-thread (n)
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
		(gnus-Subject-search-forward)
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

(defun gnus-Subject-prev-thread (n)
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
		(gnus-Subject-search-backward)
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

(defun gnus-Subject-down-thread (d)
  "Go downward current thread.
Argument D specifies the depth goes down."
  (interactive "p")
  ;; Adjust cursor point.
  (beginning-of-line)
  (search-forward ":" nil t)
  (let ((last (point))
	(level (current-column)))
    (while (and (> d 0)
		(gnus-Subject-search-forward)
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

(defun gnus-Subject-up-thread (d)
  "Go upward current thread.
Argument D specifies the depth goes up."
  (interactive "p")
  ;; Adjust cursor point.
  (beginning-of-line)
  (search-forward ":" nil t)
  (let ((last (point))
	(level (current-column)))
    (while (and (> d 0)
		(gnus-Subject-search-backward))
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

(defun gnus-Subject-kill-thread (unmark)
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
	     (gnus-Subject-mark-as-read nil "K"))
	    ((> unmark 0)
	     (gnus-Subject-mark-as-unread nil t))
	    (t
	     (gnus-Subject-mark-as-unread))
	    )
      ;; Mark following articles.
      (while (and (gnus-Subject-search-forward)
		  (< level (current-column)))
	(cond ((null unmark)
	       (gnus-Subject-mark-as-read nil "K"))
	      ((> unmark 0)
	       (gnus-Subject-mark-as-unread nil t))
	      (t
	       (gnus-Subject-mark-as-unread))
	      ))
      ))
  ;; Hide killed subtrees.
  (and (null unmark)
       gnus-thread-hide-killed
       (gnus-Subject-hide-thread))
  ;; If marked as read, go to next unread subject.
  (if (null unmark)
      ;; Go to next unread subject.
      (gnus-Subject-next-subject 1 t))
  )

(defun gnus-Subject-toggle-truncation (arg)
  "Toggle truncation of subject lines.
With ARG, turn line truncation on iff ARG is positive."
  (interactive "P")
  (setq truncate-lines
	(if (null arg) (not truncate-lines)
	  (> (prefix-numeric-value arg) 0)))
  (redraw-display))

(defun gnus-Subject-sort-by-number (reverse)
  "Sort subject display buffer by article number.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-Subject-sort-subjects
   (function
    (lambda (a b)
      (< (nntp-header-number a) (nntp-header-number b))))
   reverse
   ))

(defun gnus-Subject-sort-by-author (reverse)
  "Sort subject display buffer by author name alphabetically.
If case-fold-search is non-nil, case of letters is ignored.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-Subject-sort-subjects
   (function
    (lambda (a b)
      (gnus-string-lessp (nntp-header-from a) (nntp-header-from b))))
   reverse
   ))

(defun gnus-Subject-sort-by-subject (reverse)
  "Sort subject display buffer by subject alphabetically. `Re:'s are ignored.
If case-fold-search is non-nil, case of letters is ignored.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-Subject-sort-subjects
   (function
    (lambda (a b)
      (gnus-string-lessp
       (gnus-simplify-subject (nntp-header-subject a) 're-only)
       (gnus-simplify-subject (nntp-header-subject b) 're-only))))
   reverse
   ))

(defun gnus-Subject-sort-by-date (reverse)
  "Sort subject display buffer by posted date.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-Subject-sort-subjects
   (function
    (lambda (a b)
      (gnus-date-lessp (nntp-header-date a) (nntp-header-date b))))
   reverse
   ))

(defun gnus-Subject-sort-subjects (predicate &optional reverse)
  "Sort subject display buffer by PREDICATE.
Optional argument REVERSE means reverse order."
  (let ((current (gnus-Subject-article-number)))
    (gnus-sort-headers predicate reverse)
    (gnus-Subject-prepare)
    (gnus-Subject-goto-subject current)
    ))

(defun gnus-Subject-reselect-current-group (show-all)
  "Once exit and then reselect the current newsgroup.
Prefix argument SHOW-ALL means to select all articles."
  (interactive "P")
  (let ((current-subject (gnus-Subject-article-number)))
    (gnus-Subject-exit t)
    ;; We have to adjust the point of Group mode buffer because the
    ;; current point was moved to the next unread newsgroup by
    ;; exiting.
    (gnus-Subject-jump-to-group gnus-newsgroup-name)
    (gnus-Group-read-group show-all t)
    (gnus-Subject-goto-subject current-subject)
    ))

(defun gnus-Subject-caesar-message (rotnum)
  "Caesar rotates all letters of current message by 13/47 places.
With prefix arg, specifies the number of places to rotate each letter forward.
Caesar rotates Japanese letters by 47 places in any case."
  (interactive "P")
  (gnus-Subject-select-article)
  (gnus-overload-functions)
  (gnus-eval-in-buffer-window gnus-Article-buffer
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

(defun gnus-Subject-rmail-digest ()
  "Run RMAIL on current digest article.
`gnus-Select-digest-hook' will be called with no arguments, if that
value is non-nil.  It is possible to modify the article so that Rmail
can work with it.

`gnus-Rmail-digest-hook' will be called with no arguments, if that value
is non-nil.  The hook is intended to customize Rmail mode."
  (interactive)
  (gnus-Subject-select-article)
  (require 'rmail)
  (let ((artbuf gnus-Article-buffer)
	(digbuf (get-buffer-create gnus-Digest-buffer))
	(mail-header-separator ""))
    (set-buffer digbuf)
    (buffer-flush-undo (current-buffer))
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert-buffer-substring artbuf)
    (run-hooks 'gnus-Select-digest-hook)
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
	  (setq rmail-last-rmail-file
		(funcall gnus-rmail-save-name
			 gnus-newsgroup-name
			 gnus-current-headers
			 gnus-newsgroup-last-rmail
			 ))
	  (setq rmail-last-file
		(funcall gnus-mail-save-name
			 gnus-newsgroup-name
			 gnus-current-headers
			 gnus-newsgroup-last-mail
			 ))
	  ;; Prevent generating new buffer named ***<N> each time.
	  (setq rmail-summary-buffer
		(get-buffer-create gnus-Digest-summary-buffer))
	  (run-hooks 'gnus-Rmail-digest-hook)
	  ;; Take all windows safely.
	  (gnus-configure-windows '(1 0 0))
	  (pop-to-buffer gnus-Group-buffer)
	  ;; Use Subject and Article windows for Digest summary and
	  ;; Digest buffers.
	  (if gnus-digest-show-summary
	      (let ((gnus-Subject-buffer gnus-Digest-summary-buffer)
		    (gnus-Article-buffer gnus-Digest-buffer))
		(gnus-configure-windows 'SelectArticle)
		(pop-to-buffer gnus-Digest-buffer)
		(rmail-summary)
		(pop-to-buffer gnus-Digest-summary-buffer)
		(message (substitute-command-keys
			  "Type \\[rmail-summary-quit] to return to GNUS")))
	    (let ((gnus-Subject-buffer gnus-Digest-buffer))
	      (gnus-configure-windows 'ExpandSubject)
	      (pop-to-buffer gnus-Digest-buffer)
	      (message (substitute-command-keys
			"Type \\[rmail-quit] to return to GNUS")))
	    )
	  ;; Move the buffers to the end of buffer list.
	  (bury-buffer gnus-Article-buffer)
	  (bury-buffer gnus-Group-buffer)
	  (bury-buffer gnus-Digest-summary-buffer)
	  (bury-buffer gnus-Digest-buffer))
      (error (set-buffer-modified-p nil)
	     (kill-buffer digbuf)
	     ;; This command should not signal an error because the
	     ;; command is called from hooks.
	     (ding) (message "Article is not a digest")))
    ))

(defun gnus-Subject-save-article ()
  "Save this article using default saver function.
Variable `gnus-default-article-saver' specifies the saver function."
  (interactive)
  (gnus-Subject-select-article
   (not (null gnus-save-all-headers)) gnus-save-all-headers)
  (if gnus-default-article-saver
      (call-interactively gnus-default-article-saver)
    (error "No default saver is defined.")))

(defun gnus-Subject-save-in-rmail (&optional filename)
  "Append this article to Rmail file.
Optional argument FILENAME specifies file name.
Directory to save to is default to `gnus-article-save-directory' which
is initialized from the SAVEDIR environment variable."
  (interactive)
  (gnus-Subject-select-article
   (not (null gnus-save-all-headers)) gnus-save-all-headers)
  (gnus-eval-in-buffer-window gnus-Article-buffer
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

(defun gnus-Subject-save-in-mail (&optional filename)
  "Append this article to Unix mail file.
Optional argument FILENAME specifies file name.
Directory to save to is default to `gnus-article-save-directory' which
is initialized from the SAVEDIR environment variable."
  (interactive)
  (gnus-Subject-select-article
   (not (null gnus-save-all-headers)) gnus-save-all-headers)
  (gnus-eval-in-buffer-window gnus-Article-buffer
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
	  (gnus-make-directory (file-name-directory filename))
	  (rmail-output filename)
	  ;; Remember the directory name to save articles.
	  (setq gnus-newsgroup-last-mail filename)
	  )))
    ))

(defun gnus-Subject-save-in-file (&optional filename)
  "Append this article to file.
Optional argument FILENAME specifies file name.
Directory to save to is default to `gnus-article-save-directory' which
is initialized from the SAVEDIR environment variable."
  (interactive)
  (gnus-Subject-select-article
   (not (null gnus-save-all-headers)) gnus-save-all-headers)
  (gnus-eval-in-buffer-window gnus-Article-buffer
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

(defun gnus-Subject-save-in-folder (&optional folder)
  "Save this article to MH folder (using `rcvstore' in MH library).
Optional argument FOLDER specifies folder name."
  (interactive)
  (gnus-Subject-select-article
   (not (null gnus-save-all-headers)) gnus-save-all-headers)
  (gnus-eval-in-buffer-window gnus-Article-buffer
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

(defun gnus-Subject-pipe-output ()
  "Pipe this article to subprocess."
  (interactive)
  ;; Ignore `gnus-save-all-headers' since this is not save command.
  (gnus-Subject-select-article)
  (gnus-eval-in-buffer-window gnus-Article-buffer
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

(defun gnus-Subject-catch-up (all &optional quietly)
  "Mark all articles not marked as unread in this newsgroup as read.
If prefix argument ALL is non-nil, all articles are marked as read."
  (interactive "P")
  (if (or quietly
	  (y-or-n-p
	   (if all
	       "Do you really want to mark everything as read? "
	     "Delete all articles not marked as unread? ")))
      (let ((unmarked
	     (gnus-set-difference gnus-newsgroup-unreads
				  (if (not all) gnus-newsgroup-marked))))
        (message "")			;Erase "Yes or No" question.
	(while unmarked
          (gnus-Subject-mark-as-read (car unmarked) "C")
	  (setq unmarked (cdr unmarked))
	  ))
    ))

(defun gnus-Subject-catch-up-all (&optional quietly)
  "Mark all articles in this newsgroup as read."
  (interactive)
  (gnus-Subject-catch-up t quietly))

(defun gnus-Subject-catch-up-and-exit (all &optional quietly)
  "Mark all articles not marked as unread in this newsgroup as read, then exit.
If prefix argument ALL is non-nil, all articles are marked as read."
  (interactive "P")
  (if (or quietly
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
	       (gnus-Subject-next-group nil))
	      (t
	       (gnus-Subject-exit)))
	)))

(defun gnus-Subject-catch-up-all-and-exit (&optional quietly)
  "Mark all articles in this newsgroup as read, and then exit."
  (interactive)
  (gnus-Subject-catch-up-and-exit t quietly))

(defun gnus-Subject-edit-global-kill ()
  "Edit a global KILL file."
  (interactive)
  (setq gnus-current-kill-article (gnus-Subject-article-number))
  (gnus-Kill-file-edit-file nil)	;Nil stands for global KILL file.
  (message
   (substitute-command-keys
    "Editing a global KILL file (Type \\[gnus-Kill-file-exit] to exit)")))

(defun gnus-Subject-edit-local-kill ()
  "Edit a local KILL file applied to the current newsgroup."
  (interactive)
  (setq gnus-current-kill-article (gnus-Subject-article-number))
  (gnus-Kill-file-edit-file gnus-newsgroup-name)
  (message
   (substitute-command-keys
    "Editing a local KILL file (Type \\[gnus-Kill-file-exit] to exit)")))

(defun gnus-Subject-exit (&optional temporary)
  "Exit reading current newsgroup, and then return to group selection mode.
gnus-Exit-group-hook is called with no arguments if that value is non-nil."
  (interactive)
  (let ((updated nil)
	(gnus-newsgroup-headers gnus-newsgroup-headers)
	(gnus-newsgroup-unreads gnus-newsgroup-unreads)
	(gnus-newsgroup-unselected gnus-newsgroup-unselected)
	(gnus-newsgroup-marked gnus-newsgroup-marked))
    ;; Important internal variables are save, so we can reenter
    ;; Subject Mode buffer even if hook changes them.
    (run-hooks 'gnus-Exit-group-hook)
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
    (set-buffer gnus-Group-buffer)
    ;; Update cross referenced group info.
    (while updated
      (gnus-Group-update-group (car updated) t) ;Ignore invisible group.
      (setq updated (cdr updated)))
    (gnus-Group-update-group gnus-newsgroup-name))
  ;; Make sure where I was, and go to next newsgroup.
  (gnus-Group-jump-to-group gnus-newsgroup-name)
  (gnus-Group-next-unread-group 1)
  (if temporary
      ;; If exiting temporary, caller should adjust Group mode
      ;; buffer point by itself.
      nil				;Nothing to do.
    ;; Return to Group mode buffer.
    (if (get-buffer gnus-Subject-buffer)
	(bury-buffer gnus-Subject-buffer))
    (if (get-buffer gnus-Article-buffer)
	(bury-buffer gnus-Article-buffer))
    (gnus-configure-windows 'ExitNewsgroup)
    (pop-to-buffer gnus-Group-buffer)))

(defun gnus-Subject-quit ()
  "Quit reading current newsgroup without updating read article info."
  (interactive)
  (if (y-or-n-p "Do you really wanna quit reading this group? ")
      (progn
	(message "")			;Erase "Yes or No" question.
	;; Return to Group selection mode.
	(if (get-buffer gnus-Subject-buffer)
	    (bury-buffer gnus-Subject-buffer))
	(if (get-buffer gnus-Article-buffer)
	    (bury-buffer gnus-Article-buffer))
	(gnus-configure-windows 'ExitNewsgroup)
	(pop-to-buffer gnus-Group-buffer)
	(gnus-Group-jump-to-group gnus-newsgroup-name) ;Make sure where I was.
	(gnus-Group-next-group 1)	;(gnus-Group-next-unread-group 1)
	)))

(defun gnus-Subject-describe-briefly ()
  "Describe Subject mode commands briefly."
  (interactive)
  (message
   (concat
    (substitute-command-keys "\\[gnus-Subject-next-page]:Select  ")
    (substitute-command-keys "\\[gnus-Subject-next-unread-article]:Forward  ")
    (substitute-command-keys "\\[gnus-Subject-prev-unread-article]:Backward  ")
    (substitute-command-keys "\\[gnus-Subject-exit]:Exit  ")
    (substitute-command-keys "\\[gnus-Info-find-node]:Run Info  ")
    (substitute-command-keys "\\[gnus-Subject-describe-briefly]:This help")
    )))


;;;
;;; GNUS Article Mode
;;;

(if gnus-Article-mode-map
    nil
  (setq gnus-Article-mode-map (make-keymap))
  (suppress-keymap gnus-Article-mode-map)
  (define-key gnus-Article-mode-map " " 'gnus-Article-next-page)
  (define-key gnus-Article-mode-map "\177" 'gnus-Article-prev-page)
  (define-key gnus-Article-mode-map "r" 'gnus-Article-refer-article)
  (define-key gnus-Article-mode-map "o" 'gnus-Article-pop-article)
  (define-key gnus-Article-mode-map "h" 'gnus-Article-show-subjects)
  (define-key gnus-Article-mode-map "s" 'gnus-Article-show-subjects)
  (define-key gnus-Article-mode-map "?" 'gnus-Article-describe-briefly)
  (define-key gnus-Article-mode-map "\C-c\C-i" 'gnus-Info-find-node))

(defun gnus-Article-mode ()
  "Major mode for browsing through an article.
All normal editing commands are turned off.
Instead, these commands are available:
\\{gnus-Article-mode-map}

Various hooks for customization:
 gnus-Article-mode-hook
    Entry to this mode calls the value with no arguments, if that
    value is non-nil.

 gnus-Article-prepare-hook
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
  (make-local-variable 'global-mode-string)
  (setq global-mode-string nil)
  (setq major-mode 'gnus-Article-mode)
  (setq mode-name "Article")
  (gnus-Article-set-mode-line)
  (use-local-map gnus-Article-mode-map)
  (make-local-variable 'page-delimiter)
  (setq page-delimiter gnus-page-delimiter)
  (make-local-variable 'mail-header-separator)
  (setq mail-header-separator "")	;For caesar function.
  (buffer-flush-undo (current-buffer))
  (setq buffer-read-only t)		;Disable modification
  (run-hooks 'gnus-Article-mode-hook))

(defun gnus-Article-setup-buffer ()
  "Initialize Article mode buffer."
  (or (get-buffer gnus-Article-buffer)
      (save-excursion
	(set-buffer (get-buffer-create gnus-Article-buffer))
	(gnus-Article-mode))
      ))

(defun gnus-Article-prepare (article &optional all-headers)
  "Prepare ARTICLE in Article mode buffer.
If optional argument ALL-HEADERS is non-nil, all headers are inserted."
  (save-excursion
    (set-buffer gnus-Article-buffer)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (if (gnus-request-article article)
	  (progn
	    ;; Prepare article buffer
	    (insert-buffer-substring nntp-server-buffer)
	    (setq gnus-have-all-headers (or all-headers gnus-show-all-headers))
	    (if (and (numberp article)
		     (not (eq article gnus-current-article)))
		;; Seems me that a new article is selected.
		(progn
		  ;; gnus-current-article must be an article number.
		  (setq gnus-last-article gnus-current-article)
		  (setq gnus-current-article article)
		  (setq gnus-current-headers
			(gnus-find-header-by-number gnus-newsgroup-headers
						    gnus-current-article))
		  ;; Clear articles history only when articles are
		  ;; retrieved by article numbers.
		  (setq gnus-current-history nil)
		  (run-hooks 'gnus-Mark-article-hook)
		  ))
	    ;; Hooks for modifying contents of the article. This hook
	    ;; must be called before being narrowed.
	    (run-hooks 'gnus-Article-prepare-hook)
	    ;; Delete unnecessary headers.
	    (or gnus-have-all-headers
		(gnus-Article-delete-headers))
	    ;; Do page break.
	    (goto-char (point-min))
	    (if gnus-break-pages
		(gnus-narrow-to-page))
	    ;; Next function must be called after setting
	    ;;  `gnus-current-article' variable and narrowed to page.
	    (gnus-Article-set-mode-line)
	    )
	(if (numberp article)
	    (gnus-Subject-mark-as-read article))
	(ding) (message "No such article (may be canceled)"))
      )))

(defun gnus-Article-show-all-headers ()
  "Show all article headers in Article mode buffer."
  (or gnus-have-all-headers
      (gnus-Article-prepare gnus-current-article t)))

;;(defun gnus-Article-set-mode-line ()
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

(defun gnus-Article-set-mode-line ()
  "Set Article mode line string."
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
		(format "GNUS: %s{%d} %s"
			gnus-newsgroup-name
			gnus-current-article
			;; This is proposed by tale@pawl.rpi.edu.
			(cond ((and (zerop unmarked)
				    (zerop unselected))
			       "      ")
			      ((zerop unselected)
			       (format "%d more" unmarked))
			      (t
			       (format "%d(+%d) more" unmarked unselected)))
			))))
  (set-buffer-modified-p t))

(defun gnus-Article-delete-headers ()
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

(defun gnus-Article-next-page (lines)
  "Show next page of current article.
If end of article, return non-nil. Otherwise return nil.
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

(defun gnus-Article-prev-page (lines)
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

(defun gnus-Article-next-digest (nth)
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

(defun gnus-Article-prev-digest (nth)
  "Move to head of NTH previous digested message."
  ;; Stop page breaking in digest mode.
  (widen)
  (beginning-of-line)
  ;; Skip NTH - 1 digest.
  ;; Suggested by Khalid Sattar <admin@cs.exeter.ac.uk>.
  ;; Digest separator is customizable.
  ;; Suggested by Skip Montanaro <montanaro@sprite.crd.ge.com>.
  (while (and (> nth 1)
	      (re-search-backward gnus-digest-separator nil 'move))
    (setq nth (1- nth)))
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

(defun gnus-Article-refer-article ()
  "Read article specified by message-id around point."
  (interactive)
  (save-window-excursion
    (save-excursion
      (re-search-forward ">" nil t)	;Move point to end of "<....>".
      (if (re-search-backward "\\(<[^<> \t\n]+>\\)" nil t)
	  (let ((message-id
		 (buffer-substring (match-beginning 1) (match-end 1))))
	    (set-buffer gnus-Subject-buffer)
	    (gnus-Subject-refer-article message-id))
	(error "No references around point"))
      )))

(defun gnus-Article-pop-article ()
  "Pop up article history."
  (interactive)
  (save-window-excursion
    (set-buffer gnus-Subject-buffer)
    (gnus-Subject-refer-article nil)))

(defun gnus-Article-show-subjects ()
  "Reconfigure windows to show headers."
  (interactive)
  (gnus-configure-windows 'SelectArticle)
  (pop-to-buffer gnus-Subject-buffer)
  (gnus-Subject-goto-subject gnus-current-article))

(defun gnus-Article-describe-briefly ()
  "Describe Article mode commands briefly."
  (interactive)
  (message
   (concat
    (substitute-command-keys "\\[gnus-Article-next-page]:Next page  ")
    (substitute-command-keys "\\[gnus-Article-prev-page]:Prev page  ")
    (substitute-command-keys "\\[gnus-Article-show-subjects]:Show headers  ")
    (substitute-command-keys "\\[gnus-Info-find-node]:Run Info  ")
    (substitute-command-keys "\\[gnus-Article-describe-briefly]:This help")
    )))


;;;
;;; GNUS KILL-File Mode
;;;

(if gnus-Kill-file-mode-map
    nil
  (setq gnus-Kill-file-mode-map (copy-keymap emacs-lisp-mode-map))
  (define-key gnus-Kill-file-mode-map "\C-c\C-k\C-s" 'gnus-Kill-file-kill-by-subject)
  (define-key gnus-Kill-file-mode-map "\C-c\C-k\C-a" 'gnus-Kill-file-kill-by-author)
  (define-key gnus-Kill-file-mode-map "\C-c\C-a" 'gnus-Kill-file-apply-buffer)
  (define-key gnus-Kill-file-mode-map "\C-c\C-e" 'gnus-Kill-file-apply-last-sexp)
  (define-key gnus-Kill-file-mode-map "\C-c\C-c" 'gnus-Kill-file-exit)
  (define-key gnus-Kill-file-mode-map "\C-c\C-i" 'gnus-Info-find-node))

(defun gnus-Kill-file-mode ()
  "Major mode for editing KILL file.

In addition to Emacs-Lisp Mode, the following commands are available:

\\[gnus-Kill-file-kill-by-subject]	Insert KILL command for current subject.
\\[gnus-Kill-file-kill-by-author]	Insert KILL command for current author.
\\[gnus-Kill-file-apply-buffer]	Apply current buffer to selected newsgroup.
\\[gnus-Kill-file-apply-last-sexp]	Apply sexp before point to selected newsgroup.
\\[gnus-Kill-file-exit]	Save file and exit editing KILL file.
\\[gnus-Info-find-node]	Read Info about KILL file.

  A KILL file contains lisp expressions to be applied to a selected
newsgroup. The purpose is to mark articles as read on the basis of
some set of regexps. A global KILL file is applied to every newsgroup,
and a local KILL file is applied to a specified newsgroup. Since a
global KILL file is applied to every newsgroup, for better performance
use a local one.

  A KILL file can contain any kind of Emacs lisp expressions expected
to be evaluated in the Subject buffer. Writing lisp programs for this
purpose is not so easy because the internal working of GNUS must be
well-known. For this reason, GNUS provides a general function which
does this easily for non-Lisp programmers.

  The `gnus-kill' function executes commands available in Subject Mode
by their key sequences. `gnus-kill' should be called with FIELD,
REGEXP and optional COMMAND and ALL. FIELD is a string representing
the header field or an empty string. If FIELD is an empty string, the
entire article body is searched for. REGEXP is a string which is
compared with FIELD value. COMMAND is a string representing a valid
key sequence in Subject Mode or Lisp expression. COMMAND is default to
'(gnus-Subject-mark-as-read nil \"X\"). Make sure that COMMAND is
executed in the Subject buffer.  If the second optional argument ALL
is non-nil, the COMMAND is applied to articles which are already
marked as read or unread.  Articles which are marked are skipped over
by default.

  For example, if you want to mark articles of which subjects contain
the string `AI' as read, a possible KILL file may look like:

	(gnus-kill \"Subject\" \"AI\")

  If you want to mark articles with `D' instead of `X', you can use
the following expression:

	(gnus-kill \"Subject\" \"AI\" \"d\")

In this example it is assumed that the command
`gnus-Subject-mark-as-read-forward' is assigned to `d' in Subject Mode.

  It is possible to delete unnecessary headers which are marked with
`X' in a KILL file as follows:

	(gnus-expunge \"X\")

  If the Subject buffer is empty after applying KILL files, GNUS will
exit the selected newsgroup normally.  If headers which are marked
with `D' are deleted in a KILL file, it is impossible to read articles
which are marked as read in the previous GNUS sessions.  Marks other
than `D' should be used for articles which should really be deleted.

Entry to this mode calls emacs-lisp-mode-hook and
gnus-Kill-file-mode-hook with no arguments, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map gnus-Kill-file-mode-map)
  (set-syntax-table emacs-lisp-mode-syntax-table)
  (setq major-mode 'gnus-Kill-file-mode)
  (setq mode-name "KILL-File")
  (lisp-mode-variables nil)
  (run-hooks 'emacs-lisp-mode-hook 'gnus-Kill-file-mode-hook))

(defun gnus-Kill-file-edit-file (newsgroup)
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
	    ((eq major-mode 'gnus-Group-mode)
	     (gnus-configure-windows '(1 0 0)) ;Take all windows.
	     (pop-to-buffer gnus-Group-buffer)
	     (let ((gnus-Subject-buffer buffer))
	       (gnus-configure-windows '(1 1 0)) ;Split into two.
	       (pop-to-buffer buffer)))
	    ((eq major-mode 'gnus-Subject-mode)
	     (gnus-configure-windows 'SelectArticle)
	     (pop-to-buffer gnus-Article-buffer)
	     (bury-buffer gnus-Article-buffer)
	     (switch-to-buffer buffer))
	    (t				;No good rules.
	     (find-file-other-window file))
	    ))
    (gnus-Kill-file-mode)
    ))

(defun gnus-Kill-file-kill-by-subject ()
  "Insert KILL command for current subject."
  (interactive)
  (insert
   (format "(gnus-kill \"Subject\" %s)\n"
	   (prin1-to-string
	    (if gnus-current-kill-article
		(regexp-quote
		 (nntp-header-subject
		  (gnus-find-header-by-number gnus-newsgroup-headers
					      gnus-current-kill-article)))
	      "")))))

(defun gnus-Kill-file-kill-by-author ()
  "Insert KILL command for current author."
  (interactive)
  (insert
   (format "(gnus-kill \"From\" %s)\n"
	   (prin1-to-string
	    (if gnus-current-kill-article
		(regexp-quote
		 (nntp-header-from
		  (gnus-find-header-by-number gnus-newsgroup-headers
					      gnus-current-kill-article)))
	      "")))))

(defun gnus-Kill-file-apply-buffer ()
  "Apply current buffer to current newsgroup."
  (interactive)
  (if (and gnus-current-kill-article
	   (get-buffer gnus-Subject-buffer))
      ;; Assume newsgroup is selected.
      (let ((string (concat "(progn \n" (buffer-string) "\n)" )))
	(save-excursion
	  (save-window-excursion
	    (pop-to-buffer gnus-Subject-buffer)
	    (eval (car (read-from-string string))))))
    (ding) (message "No newsgroup is selected.")))

(defun gnus-Kill-file-apply-last-sexp ()
  "Apply sexp before point in current buffer to current newsgroup."
  (interactive)
  (if (and gnus-current-kill-article
	   (get-buffer gnus-Subject-buffer))
      ;; Assume newsgroup is selected.
      (let ((string
	     (buffer-substring
	      (save-excursion (forward-sexp -1) (point)) (point))))
	(save-excursion
	  (save-window-excursion
	    (pop-to-buffer gnus-Subject-buffer)
	    (eval (car (read-from-string string))))))
    (ding) (message "No newsgroup is selected.")))

(defun gnus-Kill-file-exit ()
  "Save a KILL file, then return to the previous buffer."
  (interactive)
  (save-buffer)
  (let ((killbuf (current-buffer)))
    ;; We don't want to return to Article buffer.
    (and (get-buffer gnus-Article-buffer)
	 (bury-buffer (get-buffer gnus-Article-buffer)))
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
	    (gnus-Subject-read-group group nil t)
	    (if (eq (current-buffer) (get-buffer gnus-Subject-buffer))
		(gnus-Subject-exit t))
	    ))
      )
    ;; Finally, exit Emacs.
    (set-buffer gnus-Group-buffer)
    (gnus-Group-exit)
    ))

(defun gnus-Numeric-save-name (newsgroup headers &optional last-file)
  "Generate file name from NEWSGROUP, HEADERS, and optional LAST-FILE.
If variable `gnus-use-long-file-name' is nil, it is ~/News/News.group/num.
Otherwise, it is like ~/News/news/group/num."
  (let ((default
	  (expand-file-name
	   (concat (if gnus-use-long-file-name
		       (capitalize newsgroup)
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
	   (capitalize newsgroup)
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
		  (capitalize newsgroup)
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
			   (or gnus-article-save-directory "~/News")))
	(gnus-use-long-file-name
	 ;; Append ".KILL" to capitalized newsgroup name.
	 (expand-file-name (concat (capitalize newsgroup)
				   "." gnus-kill-file-name)
			   (or gnus-article-save-directory "~/News")))
	(t
	 ;; Place "KILL" under the hierarchical directory.
	 (expand-file-name (concat (gnus-newsgroup-directory-form newsgroup)
				   "/" gnus-kill-file-name)
			   (or gnus-article-save-directory "~/News")))
	))

(defun gnus-newsgroup-kill-file (newsgroup)
  "Return the name of a KILL file of NEWSGROUP.
If NEWSGROUP is nil, return the global KILL file instead."
  (cond ((or (null newsgroup)
	     (string-equal newsgroup ""))
	 ;; The global KILL file is placed at top of the directory.
	 (expand-file-name gnus-kill-file-name
			   (or gnus-article-save-directory "~/News")))
	(gnus-use-long-file-name
	 ;; Append ".KILL" to newsgroup name.
	 (expand-file-name (concat newsgroup "." gnus-kill-file-name)
			   (or gnus-article-save-directory "~/News")))
	(t
	 ;; Place "KILL" under the hierarchical directory.
	 (expand-file-name (concat (gnus-newsgroup-directory-form newsgroup)
				   "/" gnus-kill-file-name)
			   (or gnus-article-save-directory "~/News")))
	))

(defun gnus-newsgroup-directory-form (newsgroup)
  "Make hierarchical directory name from NEWSGROUP name."
  (let ((newsgroup (substring newsgroup 0)) ;Copy string.
	(len (length newsgroup))
	(idx 0))
    ;; Replace all occurence of `.' with `/'.
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
	 (setq head
	       (concat (file-name-as-directory head)
		       (substring tail (match-beginning 1) (match-end 1))))
	 (or (file-exists-p head)
	     (call-process "mkdir" nil nil nil head))
	 (gnus-make-directory-1 head (substring tail (match-end 1))))
	((string-equal tail "") t)
	))

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

(defun gnus-sort-headers (predicate &optional reverse)
  "Sort current group headers by PREDICATE safely.
*Safely* means C-g quitting is disabled during sorting.
Optional argument REVERSE means reverse order."
  (let ((inhibit-quit t))
    (setq gnus-newsgroup-headers
	  (if reverse
	      (nreverse (sort (nreverse gnus-newsgroup-headers) predicate))
	    (sort gnus-newsgroup-headers predicate)))
    ))

(defun gnus-string-lessp (a b)
  "Return T if first arg string is less than second in lexicographic order.
If case-fold-search is non-nil, case of letters is ignored."
  (if case-fold-search
      (string-lessp (downcase a) (downcase b)) (string-lessp a b)))

(defun gnus-date-lessp (date1 date2)
  "Return T if DATE1 is earlyer than DATE2."
  (string-lessp (gnus-comparable-date date1)
		(gnus-comparable-date date2)))

(defun gnus-comparable-date (date)
  "Make comparable string by string-lessp from DATE."
  (let ((month '(("JAN" . " 1")("FEB" . " 2")("MAR" . " 3")
		 ("APR" . " 4")("MAY" . " 5")("JUN" . " 6")
		 ("JUL" . " 7")("AUG" . " 8")("SEP" . " 9")
		 ("OCT" . "10")("NOV" . "11")("DEC" . "12")))
	(date (or date "")))
    ;; Can understand the following styles:
    ;; (1) 14 Apr 89 03:20:12 GMT
    ;; (2) Fri, 17 Mar 89 4:01:33 GMT
    (if (string-match
	 "\\([0-9]+\\) \\([^ ,]+\\) \\([0-9]+\\) \\([0-9:]+\\)" date)
	(concat
	 ;; Year
	 (substring date (match-beginning 3) (match-end 3))
	 ;; Month
	 (cdr
	  (assoc
	   (upcase (substring date (match-beginning 2) (match-end 2))) month))
	 ;; Day
	 (format "%2d" (string-to-int
			(substring date
				   (match-beginning 1) (match-end 1))))
	 ;; Time
	 (substring date (match-beginning 4) (match-end 4)))
      ;; Cannot understand DATE string.
      date
      )
    ))

(defun gnus-fetch-field (field)
  "Return the value of the header FIELD of current article."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (narrow-to-region (point-min)
			(progn (search-forward "\n\n" nil 'move) (point)))
      (mail-fetch-field field))))

(fset 'gnus-expunge 'gnus-Subject-delete-marked-with)

(defun gnus-kill (field regexp &optional command all)
  "If FIELD of an article matches REGEXP, execute COMMAND.
Optional third argument COMMAND is default to
	(gnus-Subject-mark-as-read nil \"X\").
If optional fourth argument ALL is non-nil, articles marked are also applied
to.  If FIELD is an empty string (or nil), entire article body is searched for.
COMMAND must be a lisp expression or a string representing a key sequence."
  ;; We don't want to change current point nor window configuration.
  (save-excursion
    (save-window-excursion
      ;; Selected window must be Subject mode buffer to execute
      ;; keyboard macros correctly. See command_loop_1.
      (switch-to-buffer gnus-Subject-buffer 'norecord)
      (goto-char (point-min))		;From the beginning.
      (if (null command)
	  (setq command '(gnus-Subject-mark-as-read nil "X")))
      (gnus-execute field regexp command nil (not all))
      )))

(defun gnus-execute (field regexp form &optional backward ignore-marked)
  "If FIELD of article header matches REGEXP, execute lisp FORM (or a string).
If FIELD is an empty string (or nil), entire article body is searched for.
If optional fifth argument BACKWARD is non-nil, do backward instead.
If optional sixth argument IGNORE-MARKED is non-nil, articles which are
marked as read or unread are ignored."
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
	     (setq article (gnus-Subject-article-number))
	     (or (not (memq article gnus-newsgroup-unreads)) ;Marked as read.
		 (memq article gnus-newsgroup-marked) ;Marked as unread.
		 ))
	(gnus-execute-1 function regexp form))
    (while (gnus-Subject-search-subject backward ignore-marked nil)
      (gnus-execute-1 function regexp form))
    ))

(defun gnus-execute-1 (function regexp form)
  (save-excursion
    ;; The point of Subject mode buffer must be saved during execution.
    (let ((article (gnus-Subject-article-number)))
      (if (null article)
	  nil				;Nothing to do.
	(if function
	    ;; Compare with header field.
	    (let ((header (gnus-find-header-by-number
			   gnus-newsgroup-headers article))
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
		(gnus-Mark-article-hook nil)) ;Inhibit marking as read.
	    (message "Searching for article: %d..." article)
	    (gnus-Article-setup-buffer)
	    (gnus-Article-prepare article t)
	    (if (save-excursion
		  (set-buffer gnus-Article-buffer)
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
	((< n 0) (setq n (- 26 (% (- n) 26))))
	(t (setq n (% n 26))))		;canonicalize N
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
	      (message "Building caesar-translate-table... done")))
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
  (setq rmail-last-rmail-file file-name)
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
Run gnus-Open-server-hook just before opening news server."
  (if (gnus-server-opened)
      ;; Stream is already opened.
      nil
    ;; Open NNTP server.
    (if (or confirm
	    (null gnus-nntp-server))
	(if (and (boundp 'gnus-secondary-servers) gnus-secondary-servers)
	    ;; Read server name with completion.
	    (setq gnus-nntp-server
		  (completing-read "NNTP server: "
				   (cons (list gnus-nntp-server)
					 gnus-secondary-servers)
				   nil nil gnus-nntp-server))
	  (setq gnus-nntp-server
		(read-string "NNTP server: " gnus-nntp-server))))
    ;; If no server name is given, local host is assumed.
    (if (string-equal gnus-nntp-server "")
	(setq gnus-nntp-server (system-name)))
    (cond ((string-match ":" gnus-nntp-server)
	   ;; :DIRECTORY
	   (require 'mhspool)
	   (gnus-define-access-method 'mhspool)
	   (message "Looking up private directory..."))
	  ((and (null gnus-nntp-service)
	        (string-equal gnus-nntp-server (system-name)))
	   (require 'nnspool)
	   (gnus-define-access-method 'nnspool)
	   (message "Looking up local news spool..."))
	  (t
	   (gnus-define-access-method 'nntp)
	   (message "Connecting to NNTP server on %s..." gnus-nntp-server)))
    (run-hooks 'gnus-Open-server-hook)
    (cond ((gnus-open-server gnus-nntp-server gnus-nntp-service))
	  ((and (stringp (gnus-status-message))
		(> (length (gnus-status-message)) 0))
	   ;; Show valuable message if available.
	   (error (gnus-status-message)))
	  (t (error "Cannot open NNTP server on %s" gnus-nntp-server)))
    ))

;; Dummy functions used only once. Should return nil.
(defun gnus-server-opened () nil)
(defun gnus-close-server () nil)

(defun gnus-define-access-method (method &optional access-methods)
  "Define access functions for the access METHOD.
Methods defintion is taken from optional argument ACCESS-METHODS or
the variable gnus-access-methods."
  (let ((bindings
	 (cdr (assoc method (or access-methods gnus-access-methods)))))
    (if (null bindings)
	(error "Unknown access method: %s" method)
      ;; Should not use symbol-function here since overload does not work.
      (while bindings
	(fset (car (car bindings)) (cdr (car bindings)))
	(setq bindings (cdr bindings)))
      )))

(defun gnus-select-newsgroup (group &optional show-all)
  "Select newsgroup GROUP.
If optional argument SHOW-ALL is non-nil, all of articles in the group
are selected."
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
				 (cdr (assoc group gnus-marked-assoc))))
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
	;; GROUP is successfully selected.
	t
	)
    ))

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
  (setq gnus-active-hashtb nil)
  (setq gnus-unread-hashtb nil)
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
The ACTION is either a symbol, such as `SelectNewsgroup', or a
configuration list such as `(1 1 2)'.  If ACTION is not a list,
configuration list is got from the variable gnus-window-configuration."
  (let* ((windows
	  (if (listp action)
	      action (car (cdr (assq action gnus-window-configuration)))))
	 (grpwin (get-buffer-window gnus-Group-buffer))
	 (subwin (get-buffer-window gnus-Subject-buffer))
	 (artwin (get-buffer-window gnus-Article-buffer))
	 (winsum nil)
	 (height nil)
	 (grpheight 0)
	 (subheight 0)
	 (artheight 0))
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
      ;; The Newsgroup buffer exits always. So, use it to extend the
      ;; Group window so as to get enough window space.
      (switch-to-buffer gnus-Group-buffer 'norecord)
      (and (get-buffer gnus-Subject-buffer)
	   (delete-windows-on gnus-Subject-buffer))
      (and (get-buffer gnus-Article-buffer)
	   (delete-windows-on gnus-Article-buffer))
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
	     (switch-to-buffer gnus-Group-buffer 'norecord)
	     (other-window 1)))
      (and (not (zerop subheight))
	   (progn
	     (switch-to-buffer gnus-Subject-buffer 'norecord)
	     (other-window 1)))
      (and (not (zerop artheight))
	   (progn
	     ;; If Article buffer does not exist, it will be created
	     ;; and initialized.
	     (gnus-Article-setup-buffer)
	     (switch-to-buffer gnus-Article-buffer 'norecord)))
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

(defun gnus-Info-find-node ()
  "Find Info documentation of GNUS."
  (interactive)
  (require 'info)
  ;; Enlarge info window if needed.
  (cond ((eq major-mode 'gnus-Group-mode)
	 (gnus-configure-windows '(1 0 0)) ;Take all windows.
	 (pop-to-buffer gnus-Group-buffer))
	((eq major-mode 'gnus-Subject-mode)
	 (gnus-configure-windows '(0 1 0)) ;Take all windows.
	 (pop-to-buffer gnus-Subject-buffer)))
  (Info-goto-node (cdr (assq major-mode gnus-Info-nodes))))

(defun gnus-overload-functions (&optional overloads)
  "Overload functions specified by optional argument OVERLOADS.
If nothing is specified, use the variable gnus-overload-functions."
  (let ((defs nil)
	(overloads (or overloads gnus-overload-functions)))
    (while overloads
      (setq defs (car overloads))
      (setq overloads (cdr overloads))
      ;; Load file before overloading function if necessary.  Make
      ;; sure we cannot use `requre' always.
      (and (not (fboundp (car defs)))
	   (car (cdr (cdr defs)))
	   (load (car (cdr (cdr defs))) nil 'nomessage))
      (fset (car defs) (car (cdr defs)))
      )))

(defun gnus-make-threads (newsgroup-headers)
  "Make conversation threads tree from NEWSGROUP-HEADERS."
  (let ((headers newsgroup-headers)
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
	    (setq d (and (nntp-header-references h)
			 (string-match "\\(<[^<>]+>\\)[^>]*$"
				       (nntp-header-references h))
			 (gnus-find-header-by-id
			  newsgroup-headers
			  (substring (nntp-header-references h)
				     (match-beginning 1) (match-end 1)))))
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

(defun gnus-setup-news-info (&optional rawfile)
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
    (if init
	(gnus-read-newsrc-file rawfile))
    (gnus-read-active-file)
    (gnus-expire-marked-articles)
    (gnus-get-unread-articles)
    ;; Check new newsgroups and subscribe them.
    (if init
	(let ((new-newsgroups (gnus-find-new-newsgroups)))
	  (while new-newsgroups
	    (funcall gnus-subscribe-newsgroup-method (car new-newsgroups))
	    (setq new-newsgroups (cdr new-newsgroups))
	    )))
    ))

(defun gnus-subscribe-newsgroup (newsgroup &optional next)
  "Subscribe new NEWSGROUP.
If optional argument NEXT is non-nil, it is inserted before NEXT."
  (gnus-insert-newsgroup (list newsgroup t) next)
  (message "Newsgroup %s is subscribed" newsgroup))

(defun gnus-add-newsgroup (newsgroup)
  "Subscribe new NEWSGROUP safely and put it at top."
  (and (null (assoc newsgroup gnus-newsrc-assoc)) ;Really new?
       (gnus-gethash newsgroup gnus-active-hashtb) ;Really exist?
       (gnus-insert-newsgroup (or (assoc newsgroup gnus-killed-assoc)
				  (list newsgroup t))
			      (car (car gnus-newsrc-assoc)))))

(defun gnus-find-new-newsgroups ()
  "Looking for new newsgroups and return names.
`-n' option of options line in .newsrc file is recognized."
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
	     (null (assoc group gnus-killed-assoc)) ;Ignore killed.
	     (null (assoc group gnus-newsrc-assoc)) ;Really new.
	     ;; Find new newsgroup.
	     (setq new-newsgroups
		   (cons group new-newsgroups)))
	))
     gnus-active-hashtb)
    ;; Return new newsgroups.
    new-newsgroups
    ))

(defun gnus-kill-newsgroup (group)
  "Kill GROUP from gnus-newsrc-assoc, .newsrc and gnus-unread-hashtb."
  (let ((info (assoc group gnus-newsrc-assoc)))
    (if (null info)
	nil
      ;; Delete from gnus-newsrc-assoc
      (setq gnus-newsrc-assoc (delq info gnus-newsrc-assoc))
      ;; Add to gnus-killed-assoc.
      (setq gnus-killed-assoc
	    (cons info
		  (delq (assoc group gnus-killed-assoc) gnus-killed-assoc)))
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
    (if (assoc group gnus-newsrc-assoc)
	(error "Duplicated: %s" group))
    ;; Insert to gnus-newsrc-assoc.
    (if (string-equal next (car (car gnus-newsrc-assoc)))
	(setq gnus-newsrc-assoc
	      (cons info gnus-newsrc-assoc))
      (let ((found nil)
	    (rest gnus-newsrc-assoc)
	    (tail (cons nil gnus-newsrc-assoc)))
	;; Seach insertion point.
	(while (and (not found) rest)
	  (if (string-equal next (car (car rest)))
	      (setq found t)
	    (setq rest (cdr rest))
	    (setq tail (cdr tail))
	    ))
	;; Find it.
	(setcdr tail nil)
	(setq gnus-newsrc-assoc
	      (append gnus-newsrc-assoc (cons info rest)))
	))
    ;; Delete from gnus-killed-assoc.
    (setq gnus-killed-assoc
	  (delq (assoc group gnus-killed-assoc) gnus-killed-assoc))
    ;; Then insert to .newsrc.
    (gnus-update-newsrc-buffer group nil next)
    ;; Add to gnus-unread-hashtb.
    (gnus-sethash group
		  (cons group		;Newsgroup name.
			(cons (gnus-number-of-articles range) range))
		  gnus-unread-hashtb)
    ))

(defun gnus-check-killed-newsgroups ()
  "Check consistency between gnus-newsrc-assoc and gnus-killed-assoc."
  (let ((group nil)
	(new-killed nil)
	(old-killed gnus-killed-assoc))
    (while old-killed
      (setq group (car (car old-killed)))
      (and (or (null gnus-newsrc-options-n-no)
	       (not (string-match gnus-newsrc-options-n-no group))
	       (and gnus-newsrc-options-n-yes
		    (string-match gnus-newsrc-options-n-yes group)))
	   (null (assoc group gnus-newsrc-assoc)) ;No duplication.
	   ;; Subscribed in options line and not in gnus-newsrc-assoc.
	   (setq new-killed
		 (cons (car old-killed) new-killed)))
      (setq old-killed (cdr old-killed))
      )
    (setq gnus-killed-assoc (nreverse new-killed))
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
    ;; Update gnus-newsrc-assoc.
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
    ;; Update gnus-killed-assoc.
    ;; The killed newsgroups are deleted without any confirmations.
    (while old-killed
      (setq group (car (car old-killed)))
      (and (gnus-gethash group gnus-active-hashtb)
	   (null (assoc group gnus-newsrc-assoc))
	   ;; Active and really killed newsgroup.
	   (setq new-killed (cons (car old-killed) new-killed)))
      (setq old-killed (cdr old-killed))
      )
    (setq gnus-killed-assoc (nreverse new-killed))
    ;; Remove BOGUS from .newsrc file.
    (while bogus
      (gnus-update-newsrc-buffer (car bogus) 'delete)
      (setq bogus (cdr bogus)))
    ;; Update gnus-marked-assoc.
    (while old-marked
      (setq group (car (car old-marked)))
      (if (and (cdr (car old-marked))	;Non-empty?
	       (assoc group gnus-newsrc-assoc))	;Not bogus?
	  (setq new-marked (cons (car old-marked) new-marked)))
      (setq old-marked (cdr old-marked)))
    (setq gnus-marked-assoc new-marked)
    (message "Checking bogus newsgroups... done")
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
	(setq gnus-unread-hashtb (gnus-make-hashtable)))
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
    (message "Checking new news... done")
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
		   (not (nth 1 (assoc gname gnus-newsrc-assoc))))
	      ;; Ignore article marked as unread.
	      (memq article (cdr (assoc gname gnus-marked-assoc)))
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
      (setcdr (cdr (assoc group gnus-newsrc-assoc))
	      (gnus-difference-of-range active (nthcdr 2 unread)))
      ;; Update .newsrc buffer.
      (gnus-update-newsrc-buffer group)
      ;; Update gnus-marked-assoc.
      (if (listp marked-list)		;Includes NIL.
	  (let ((marked (assoc group gnus-marked-assoc)))
	    (cond (marked
		   (setcdr marked marked-list))
		  (marked-list		;Non-NIL.
		   (setq gnus-marked-assoc
			 (cons (cons group marked-list)
			       gnus-marked-assoc)))
		  )))
      )))

(defun gnus-read-active-file ()
  "Get active file from NNTP server."
  (message "Reading active file...")
  (if (gnus-request-list)		;Get active file from server
      (save-excursion
	(set-buffer nntp-server-buffer)
	;; Save OLD active info.
	(setq gnus-octive-hashtb gnus-active-hashtb)
	(setq gnus-active-hashtb (gnus-make-hashtable))
	(gnus-active-to-gnus-format)
	(message "Reading active file... done"))
    (error "Cannot read active file from NNTP server.")))

(defun gnus-active-to-gnus-format ()
  "Convert active file format to internal format."
  ;; Delete unnecessary lines.
  (goto-char (point-min))
  (delete-matching-lines "^to\\..*$")
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
     gnus-active-hashtb)))

(defun gnus-read-newsrc-file (&optional rawfile)
  "Read startup FILE.
If optional argument RAWFILE is non-nil, the raw startup file is read."
  (setq gnus-current-startup-file
	(let* ((file (expand-file-name gnus-startup-file nil))
	       (real-file (concat file "-" gnus-nntp-server)))
	  (if (file-exists-p real-file)
	      real-file file)))
  ;; Reset variables which may be included in the quick startup file.
  (let ((variables gnus-variable-list))
    (while variables
      (set (car variables) nil)
      (setq variables (cdr variables))))
  (let* ((newsrc-file gnus-current-startup-file)
	 (quick-file (concat newsrc-file ".el"))
	 (quick-loaded nil)
	 (newsrc-mod (nth 5 (file-attributes newsrc-file)))
	 (quick-mod (nth 5 (file-attributes quick-file))))
    (save-excursion
      ;; Prepare .newsrc buffer.
      (set-buffer (find-file-noselect newsrc-file))
      ;; It is not so good idea turning off undo.
      ;;(buffer-flush-undo (current-buffer))
      ;; Load quick .newsrc to restore gnus-marked-assoc and
      ;; gnus-killed-assoc even if gnus-newsrc-assoc is out of date.
      (condition-case nil
	  (setq quick-loaded (load quick-file t t t))
	(error nil))
      (cond ((and (not rawfile)		;Not forced to read the raw file.
		  (or (and (fboundp 'file-newer-than-file-p)
			   (file-newer-than-file-p quick-file newsrc-file))
		      (and newsrc-mod quick-mod
			   ;; .newsrc.el is newer than .newsrc.
			   ;; Some older version does not support function
			   ;; `file-newer-than-file-p'.
			   (or (< (car newsrc-mod) (car quick-mod))
			       (and (= (car newsrc-mod) (car quick-mod))
				    (<= (nth 1 newsrc-mod) (nth 1 quick-mod))))
			   ))
		  quick-loaded
		  gnus-newsrc-assoc	;Really loaded?
		  )
	     ;; We don't have to read the raw startup file.
	     )
	    (t
	     ;; Since .newsrc file is newer than quick file, read it.
	     (message "Reading %s..." newsrc-file)
	     (gnus-newsrc-to-gnus-format)
	     (gnus-check-killed-newsgroups)
	     (message "Reading %s... Done" newsrc-file)))
      )))

(defun gnus-make-newsrc-file (file)
  "Make server dependent file name by catenating FILE and server host name."
  (let* ((file (expand-file-name file nil))
	 (real-file (concat file "-" gnus-nntp-server)))
    (if (file-exists-p real-file)
	real-file file)
    ))

(defun gnus-newsrc-to-gnus-format ()
  "Parse current buffer as .newsrc file."
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
    ;; Due to overflows in regex.c, change the following regexp:
    ;; "^\\([^:! \t\n]+\\)\\([:!]\\)[ \t]*\\(.*\\)$"
    ;; Suggested by composer@bucsf.bu.edu (Jeff Kellem).
    (while (re-search-forward
	    "^\\([^:! \t\n]+\\)\\([:!]\\)[ \t]*\\(\\(...\\)*.*\\)$" nil t)
      (setq newsgroup (buffer-substring (match-beginning 1) (match-end 1)))
      ;; Check duplications of newsgroups.
      ;; Note: Checking the duplications takes very long time.
      (if (assoc newsgroup gnus-newsrc-assoc)
	  (message "Ignore duplicated newsgroup: %s" newsgroup)
	(setq subscribe
	      (string-equal
	       ":" (buffer-substring (match-beginning 2) (match-end 2))))
	(setq ranges (buffer-substring (match-beginning 3) (match-end 3)))
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
	))
    (setq gnus-newsrc-assoc
	  (nreverse gnus-newsrc-assoc))
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
	(string-match "^[ \t\n,]*\\(!?\\)\\([^--- \t\n,][^ \t\n,]*\\)" options)
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
      (cond ((string-equal yes-or-no "!")
	     (setq no (cons newsgroup no)))
	    ((string-equal newsgroup ".+")) ;Ignore `all'.
	    (t
	     (setq yes (cons newsgroup yes)))
	    ))
    ;; Make a cons of regexps from parsing result.
    (cons (if yes
	      (concat "^\\("
		      (apply (function concat)
			     (mapcar
			      (function
			       (lambda (newsgroup)
				 (concat newsgroup "\\|")))
			      (cdr yes)))
		      (car yes) "\\)"))
	  (if no
	      (concat "^\\("
		      (apply (function concat)
			     (mapcar
			      (function
			       (lambda (newsgroup)
				 (concat newsgroup "\\|")))
			      (cdr no)))
		      (car no) "\\)")))
    ))

(defun gnus-save-newsrc-file ()
  "Save to .newsrc FILE."
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
	     (run-hooks 'gnus-Save-newsrc-hook)
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
	   (message "Saving %s... Done" gnus-current-startup-file)
	   ))
    ))

(defun gnus-update-newsrc-buffer (group &optional delete next)
  "Incrementally update .newsrc buffer about GROUP.
If optional second argument DELETE is non-nil, delete the group.
If optional third argument NEXT is non-nil, inserted before it."
  (save-excursion
    ;; Taking account of the killed startup file.
    ;; Suggested by tale@pawl.rpi.edu.
    (set-buffer (or (get-file-buffer gnus-current-startup-file)
		    (find-file-noselect gnus-current-startup-file)))
    ;; Options line continuation lines must be also considered here.
    ;; Before supporting continuation lines, " newsgroup ! 1-5" was
    ;; okay, but now it is invalid.  It should be "newsgroup! 1-5".
    (let ((deleted nil)
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
	(let ((newsrc (assoc group gnus-newsrc-assoc)))
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
	(variables gnus-variable-list)
	;; Temporary rebind to make changes invisible.
	(gnus-killed-assoc gnus-killed-assoc))
    ;; Remove duplicated or unsubscribed newsgroups in gnus-killed-assoc.
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
	  ;; If end1 is 0, it must be skipped. Usually no articles in
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


;;Local variables:
;;eval: (put 'gnus-eval-in-buffer-window 'lisp-indent-function 1)
;;end:
