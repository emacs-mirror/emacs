;;; nnimap.el --- imap backend for Gnus
;; Copyright (C) 1998, 1999, 2000, 2001 Free Software Foundation, Inc.

;; Author: Simon Josefsson <jas@pdc.kth.se>
;;         Jim Radford <radford@robby.caltech.edu>
;; Keywords: mail

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Todo, major things:
;;
;;   o Fix Gnus to view correct number of unread/total articles in group buffer
;;   o Fix Gnus to handle leading '.' in group names (fixed?)
;;   o Finish disconnected mode (moving articles between mailboxes unplugged)
;;   o Sieve
;;   o MIME (partial article fetches)
;;   o Split to other backends, different split rules for different
;;     servers/inboxes
;;
;; Todo, minor things:
;;
;;   o Don't require half of Gnus -- backends should be standalone
;;   o Verify that we don't use IMAP4rev1 specific things (RFC2060 App B)
;;   o Dont uid fetch 1,* in nnimap-retrive-groups (slow)
;;   o Split up big fetches (1,* header especially) in smaller chunks
;;   o What do I do with gnus-newsgroup-*?
;;   o Tell Gnus about new groups (how can we tell?)
;;   o Respooling (fix Gnus?) (unnecessery?)
;;   o Add support for the following: (if applicable)
;;       request-list-newsgroups, request-regenerate
;;       list-active-group,
;;       request-associate-buffer, request-restore-buffer,
;;   o Do The Right Thing when UIDVALIDITY changes (what's the right thing?)
;;   o Support RFC2221 (Login referrals)
;;   o IMAP2BIS compatibility? (RFC2061)
;;   o ACAP stuff (perhaps a different project, would be nice to ACAPify
;;     .newsrc.eld)
;;   o What about Gnus's article editing, can we support it?  NO!
;;   o Use \Draft to support the draft group??
;;   o Duplicate suppression

;;; Code:

(eval-and-compile
  (require 'cl)
  (require 'imap))

(require 'nnoo)
(require 'nnmail)
(require 'nnheader)
(require 'mm-util)
(require 'gnus)
(require 'gnus-range)
(require 'gnus-start)
(require 'gnus-int)

(nnoo-declare nnimap)

(defconst nnimap-version "nnimap 0.131")

(defvoo nnimap-address nil
  "Address of physical IMAP server.  If nil, use the virtual server's name.")

(defvoo nnimap-server-port nil
  "Port number on physical IMAP server.
If nil, defaults to 993 for SSL connections and 143 otherwise.")

;; Splitting variables

(defvar nnimap-split-crosspost t
  "If non-nil, do crossposting if several split methods match the mail.
If nil, the first match found will be used.")

(defvar nnimap-split-inbox nil
  "*Name of mailbox to split mail from.

Mail is read from this mailbox and split according to rules in
`nnimap-split-rules'.

This can be a string or a list of strings.")

(defvar nnimap-split-rule nil
  "*Mail will be split according to theese rules.

Mail is read from mailbox(es) specified in `nnimap-split-inbox'.

If you'd like, for instance, one mail group for mail from the
\"gnus-imap\" mailing list, one group for junk mail and leave
everything else in the incoming mailbox, you could do something like
this:

(setq nnimap-split-rule '((\"INBOX.gnus-imap\"   \"From:.*gnus-imap\")
			  (\"INBOX.junk\"        \"Subject:.*buy\")))

As you can see, `nnimap-split-rule' is a list of lists, where the first
element in each \"rule\" is the name of the IMAP mailbox, and the
second is a regexp that nnimap will try to match on the header to find
a fit.

The second element can also be a function.  In that case, it will be
called narrowed to the headers with the first element of the rule as
the argument.  It should return a non-nil value if it thinks that the
mail belongs in that group.

This variable can also have a function as its value, the function will
be called with the headers narrowed and should return a group where it
thinks the article should be splitted to.  See `nnimap-split-fancy'.

To allow for different split rules on different virtual servers, and
even different split rules in different inboxes on the same server,
the syntax of this variable have been extended along the lines of:

(setq nnimap-split-rule
      '((\"my1server\"    (\".*\"    ((\"ding\"    \"ding@gnus.org\")
                                  (\"junk\"    \"From:.*Simon\")))
        (\"my2server\"    (\"INBOX\" nnimap-split-fancy))
        (\"my[34]server\" (\".*\"    ((\"private\" \"To:.*Simon\")
                                  (\"junk\"    my-junk-func)))))

The virtual server name is in fact a regexp, so that the same rules
may apply to several servers.  In the example, the servers
\"my3server\" and \"my4server\" both use the same rules.  Similarly,
the inbox string is also a regexp.  The actual splitting rules are as
before, either a function, or a list with group/regexp or
group/function elements.")

(defvar nnimap-split-predicate "UNSEEN UNDELETED"
  "The predicate used to find articles to split.
If you use another IMAP client to peek on articles but always would
like nnimap to split them once it's started, you could change this to
\"UNDELETED\". Other available predicates are available in
RFC2060 section 6.4.4.")

(defvar nnimap-split-fancy nil
  "Like `nnmail-split-fancy', which see.")

;; Authorization / Privacy variables

(defvoo nnimap-auth-method nil
  "Obsolete.")

(defvoo nnimap-stream nil
  "How nnimap will connect to the server.

The default, nil, will try to use the \"best\" method the server can
handle.

Change this if

1) you want to connect with SSL.  The SSL integration with IMAP is
   brain-dead so you'll have to tell it specifically.

2) your server is more capable than your environment -- i.e. your
   server accept Kerberos login's but you haven't installed the
   `imtest' program or your machine isn't configured for Kerberos.

Possible choices: kerberos4, ssl, network")

(defvoo nnimap-authenticator nil
  "How nnimap authenticate itself to the server.

The default, nil, will try to use the \"best\" method the server can
handle.

There is only one reason for fiddling with this variable, and that is
if your server is more capable than your environment -- i.e. you
connect to a server that accept Kerberos login's but you haven't
installed the `imtest' program or your machine isn't configured for
Kerberos.

Possible choices: kerberos4, cram-md5, login, anonymous.")

(defvoo nnimap-directory (nnheader-concat gnus-directory "overview/")
  "Directory to keep NOV cache files for nnimap groups.
See also `nnimap-nov-file-name'.")

(defvoo nnimap-nov-file-name "nnimap."
  "NOV cache base filename.
The group name and `nnimap-nov-file-name-suffix' will be appended.  A
typical complete file name would be
~/News/overview/nnimap.pdc.INBOX.ding.nov, or
~/News/overview/nnimap/pdc/INBOX/ding/nov if
`nnmail-use-long-file-names' is nil")

(defvoo nnimap-nov-file-name-suffix ".novcache"
  "Suffix for NOV cache base filename.")

(defvoo nnimap-nov-is-evil nil
  "If non-nil, nnimap will never generate or use a local nov database for this backend.
Using nov databases will speed up header fetching considerably.
Unlike other backends, you do not need to take special care if you
flip this variable.")

(defvoo nnimap-expunge-on-close 'always ; 'ask, 'never
  "Whether to expunge a group when it is closed.
When a IMAP group with articles marked for deletion is closed, this
variable determine if nnimap should actually remove the articles or
not.

If always, nnimap always perform a expunge when closing the group.
If never, nnimap never expunges articles marked for deletion.
If ask, nnimap will ask you if you wish to expunge marked articles.

When setting this variable to `never', you can only expunge articles
by using `G x' (gnus-group-nnimap-expunge) from the Group buffer.")

(defvoo nnimap-list-pattern "*"
  "A string LIMIT or list of strings with mailbox wildcards used to limit available groups.
See below for available wildcards.

The LIMIT string can be a cons cell (REFERENCE . LIMIT), where
REFERENCE will be passed as the first parameter to LIST/LSUB.  The
semantics of this are server specific, on the University of Washington
server you can specify a directory.

Example:
 '(\"INBOX\" \"mail/*\" (\"~friend/mail/\" . \"list/*\"))

There are two wildcards * and %. * matches everything, % matches
everything in the current hierarchy.")

(defvoo nnimap-news-groups nil
  "IMAP support a news-like mode, also known as bulletin board mode, where replies is sent via IMAP instead of SMTP.

This variable should contain a regexp matching groups where you wish
replies to be stored to the mailbox directly.

Example:
  '(\"^[^I][^N][^B][^O][^X].*$\")

This will match all groups not beginning with \"INBOX\".

Note that there is nothing technically different between mail-like and
news-like mailboxes.  If you wish to have a group with todo items or
similar which you wouldn't want to set up a mailing list for, you can
use this to make replies go directly to the group.")

(defvoo nnimap-server-address nil
  "Obsolete.  Use `nnimap-address'.")

(defcustom nnimap-authinfo-file "~/.authinfo"
  "Authorization information for IMAP servers.  In .netrc format."
  :type
  '(choice file
	   (repeat :tag "Entries"
		   :menu-tag "Inline"
		   (list :format "%v"
			 :value ("" ("login" . "") ("password" . ""))
			 (string :tag "Host")
			 (checklist :inline t
				    (cons :format "%v"
					  (const :format "" "login")
					  (string :format "Login: %v"))
				    (cons :format "%v"
					  (const :format "" "password")
					  (string :format "Password: %v")))))))

(defcustom nnimap-prune-cache t
  "If non-nil, nnimap check whether articles still exist on server before using data stored in NOV cache."
  :type 'boolean)

(defvar nnimap-request-list-method 'imap-mailbox-list
  "Method to use to request a list of all folders from the server.
If this is 'imap-mailbox-lsub, then use a server-side subscription list to
restrict visible folders.")

;; Internal variables:

(defvar nnimap-debug nil
  "Name of buffer to record debugging info.
For example: (setq nnimap-debug \"*nnimap-debug*\")")
(defvar nnimap-current-move-server nil)
(defvar nnimap-current-move-group nil)
(defvar nnimap-current-move-article nil)
(defvar nnimap-length)
(defvar nnimap-progress-chars '(?| ?/ ?- ?\\))
(defvar nnimap-progress-how-often 20)
(defvar nnimap-counter)
(defvar nnimap-callback-callback-function nil
  "Gnus callback the nnimap asynchronous callback should call.")
(defvar nnimap-callback-buffer nil
  "Which buffer the asynchronous article prefetch callback should work in.")
(defvar nnimap-server-buffer-alist nil)	;; Map server name to buffers.
(defvar nnimap-current-server nil)	;; Current server
(defvar nnimap-server-buffer nil)	;; Current servers' buffer



(nnoo-define-basics nnimap)

;; Utility functions:

(defsubst nnimap-get-server-buffer (server)
  "Return buffer for SERVER, if nil use current server."
  (cadr (assoc (or server nnimap-current-server) nnimap-server-buffer-alist)))

(defun nnimap-possibly-change-server (server)
  "Return buffer for SERVER, changing the current server as a side-effect.
If SERVER is nil, uses the current server."
  (setq nnimap-current-server (or server nnimap-current-server)
	nnimap-server-buffer (nnimap-get-server-buffer nnimap-current-server)))

(defun nnimap-verify-uidvalidity (group server)
  "Verify stored uidvalidity match current one in GROUP on SERVER."
  (let* ((gnusgroup (gnus-group-prefixed-name
		     group (gnus-server-to-method
			    (format "nnimap:%s" server))))
	 (new-uidvalidity (imap-mailbox-get 'uidvalidity))
	 (old-uidvalidity (gnus-group-get-parameter gnusgroup 'uidvalidity))
	 (dir (file-name-as-directory (expand-file-name nnimap-directory)))
         (nameuid (nnheader-translate-file-chars
                   (concat nnimap-nov-file-name
                           (if (equal server "")
                               "unnamed"
                             server) "." group "." old-uidvalidity
                             nnimap-nov-file-name-suffix) t))
         (file (if (or nnmail-use-long-file-names
		       (file-exists-p (expand-file-name nameuid dir)))
		   (expand-file-name nameuid dir)
		 (expand-file-name
		  (mm-encode-coding-string
		   (nnheader-replace-chars-in-string nameuid ?. ?/)
		   nnmail-pathname-coding-system)
		  dir))))
    (if old-uidvalidity
	(if (not (equal old-uidvalidity new-uidvalidity))
	    ;; uidvalidity clash
	    (gnus-delete-file file)
	  (gnus-group-set-parameter gnusgroup 'uidvalidity new-uidvalidity)
	  t)
      (gnus-group-add-parameter gnusgroup (cons 'uidvalidity new-uidvalidity))
      t)))

(defun nnimap-before-find-minmax-bugworkaround ()
  "Function called before iterating through mailboxes with
`nnimap-find-minmax-uid'."
  ;; XXX this is for UoW imapd problem, it doesn't notice new mail in
  ;; currently selected mailbox without a re-select/examine.
  (or (null (imap-current-mailbox nnimap-server-buffer))
      (imap-mailbox-unselect nnimap-server-buffer)))

(defun nnimap-find-minmax-uid (group &optional examine)
  "Find lowest and highest active article nummber in GROUP.
If EXAMINE is non-nil the group is selected read-only."
  (with-current-buffer nnimap-server-buffer
    (when (imap-mailbox-select group examine)
      (let (minuid maxuid)
	(when (> (imap-mailbox-get 'exists) 0)
	  (imap-fetch "1,*" "UID" nil 'nouidfetch)
	  (imap-message-map (lambda (uid Uid)
			      (setq minuid (if minuid (min minuid uid) uid)
				    maxuid (if maxuid (max maxuid uid) uid)))
			    'UID))
	(list (imap-mailbox-get 'exists) minuid maxuid)))))
  
(defun nnimap-possibly-change-group (group &optional server)
  "Make GROUP the current group, and SERVER the current server."
  (when (nnimap-possibly-change-server server)
    (with-current-buffer nnimap-server-buffer
      (if (or (null group) (imap-current-mailbox-p group))
	  imap-current-mailbox
	(if (imap-mailbox-select group)
	    (if (or (nnimap-verify-uidvalidity
		     group (or server nnimap-current-server))
		    (zerop (imap-mailbox-get 'exists group))
		    (yes-or-no-p
		     (format
		      "nnimap: Group %s is not uidvalid.  Continue? " group)))
		imap-current-mailbox
	      (imap-mailbox-unselect)
	      (error "nnimap: Group %s is not uid-valid" group))
	  (nnheader-report 'nnimap (imap-error-text)))))))

(defun nnimap-replace-whitespace (string)
  "Return STRING with all whitespace replaced with space."
  (when string
    (while (string-match "[\r\n\t]+" string)
      (setq string (replace-match " " t t string)))
    string))

;; Required backend functions

(defun nnimap-retrieve-headers-progress ()
  "Hook to insert NOV line for current article into `nntp-server-buffer'."
  (and (numberp nnmail-large-newsgroup)
       (zerop (% (incf nnimap-counter) nnimap-progress-how-often))
       (> nnimap-length nnmail-large-newsgroup)
       (nnheader-message 6 "nnimap: Retrieving headers... %c"
			 (nth (/ (% nnimap-counter
				    (* (length nnimap-progress-chars)
				       nnimap-progress-how-often))
				 nnimap-progress-how-often)
			      nnimap-progress-chars)))
  (with-current-buffer nntp-server-buffer
    (let (headers lines chars uid mbx)
      (with-current-buffer nnimap-server-buffer
	(setq uid imap-current-message
	      mbx imap-current-mailbox
	      headers (nnimap-demule
		       (if (imap-capability 'IMAP4rev1)
			   ;; xxx don't just use car? alist doesn't contain
			   ;; anything else now, but it might...
			   (nth 2 (car (imap-message-get uid 'BODYDETAIL)))
			 (imap-message-get uid 'RFC822.HEADER)))
	      lines (imap-body-lines (imap-message-body imap-current-message))
	      chars (imap-message-get imap-current-message 'RFC822.SIZE)))
      (nnheader-insert-nov
       (with-temp-buffer
	 (buffer-disable-undo)
	 (insert headers)
	 (nnheader-ms-strip-cr)
	 (nnheader-fold-continuation-lines)
	 (subst-char-in-region (point-min) (point-max) ?\t ? )
	 (let ((head (nnheader-parse-head 'naked)))
	   (mail-header-set-number head uid)
	   (mail-header-set-chars head chars)
	   (mail-header-set-lines head lines)
	   (mail-header-set-xref
	    head (format "%s %s:%d" (system-name) mbx uid))
	   head))))))

(defun nnimap-retrieve-which-headers (articles fetch-old)
  "Get a range of articles to fetch based on ARTICLES and FETCH-OLD."
  (with-current-buffer nnimap-server-buffer
    (if (numberp (car-safe articles))
	(imap-search
	 (concat "UID "
		 (imap-range-to-message-set
		  (gnus-compress-sequence
		   (append (gnus-uncompress-sequence
			    (and fetch-old
				 (cons (if (numberp fetch-old)
					   (max 1 (- (car articles) fetch-old))
					 1)
				       (1- (car articles)))))
			   articles)))))
      (mapcar (lambda (msgid)
		(imap-search
		 (format "HEADER Message-Id %s" msgid)))
	      articles))))

(defun nnimap-group-overview-filename (group server)
  "Make pathname for GROUP on SERVER."
  (let* ((dir (file-name-as-directory (expand-file-name nnimap-directory)))
         (uidvalidity (gnus-group-get-parameter
                       (gnus-group-prefixed-name
                        group (gnus-server-to-method
                               (format "nnimap:%s" server)))
                       'uidvalidity))
         (name (nnheader-translate-file-chars
                (concat nnimap-nov-file-name
                        (if (equal server "")
                            "unnamed"
                          server) "." group nnimap-nov-file-name-suffix) t))
         (nameuid (nnheader-translate-file-chars
                   (concat nnimap-nov-file-name
                           (if (equal server "")
                               "unnamed"
                             server) "." group "." uidvalidity
                             nnimap-nov-file-name-suffix) t))
         (oldfile (if (or nnmail-use-long-file-names
                          (file-exists-p (expand-file-name name dir)))
                      (expand-file-name name dir)
                    (expand-file-name
                     (mm-encode-coding-string
                      (nnheader-replace-chars-in-string name ?. ?/)
                      nnmail-pathname-coding-system)
                     dir)))
         (newfile (if (or nnmail-use-long-file-names
                          (file-exists-p (expand-file-name nameuid dir)))
                      (expand-file-name nameuid dir)
                    (expand-file-name
                     (mm-encode-coding-string
                      (nnheader-replace-chars-in-string nameuid ?. ?/)
                      nnmail-pathname-coding-system)
                     dir))))
    (when (and (file-exists-p oldfile) (not (file-exists-p newfile)))
      (message "nnimap: Upgrading novcache filename...")
      (sit-for 1)
      (gnus-make-directory (file-name-directory newfile))
      (unless (ignore-errors (rename-file oldfile newfile) t)
	(if (ignore-errors (copy-file oldfile newfile) t)
	    (delete-file oldfile)
	  (error "Can't rename `%s' to `%s'" oldfile newfile))))
    newfile))

(defun nnimap-retrieve-headers-from-file (group server)
  (with-current-buffer nntp-server-buffer
    (let ((nov (nnimap-group-overview-filename group server)))
      (when (file-exists-p nov)
	(mm-insert-file-contents nov)
	(set-buffer-modified-p nil)
	(let ((min (ignore-errors (goto-char (point-min))
				  (read (current-buffer))))
	      (max (ignore-errors (goto-char (point-max))
				  (forward-line -1)
				  (read (current-buffer)))))
	  (if (and (numberp min) (numberp max))
	      (cons min max)
	    ;; junk, remove it, it's saved later
	    (erase-buffer)
	    nil))))))

(defun nnimap-retrieve-headers-from-server (articles group server)
  (with-current-buffer nnimap-server-buffer
    (let ((imap-fetch-data-hook '(nnimap-retrieve-headers-progress))
	  (nnimap-length (gnus-range-length articles))
	  (nnimap-counter 0))
      (imap-fetch (imap-range-to-message-set articles)
		  (concat "(UID RFC822.SIZE BODY "
			  (let ((headers
				 (append '(Subject From Date Message-Id
						   References In-Reply-To Xref)
					 (copy-sequence
					  nnmail-extra-headers))))
			    (if (imap-capability 'IMAP4rev1)
				(format "BODY.PEEK[HEADER.FIELDS %s])" headers)
			      (format "RFC822.HEADER.LINES %s)" headers)))))
      (and (numberp nnmail-large-newsgroup)
	   (> nnimap-length nnmail-large-newsgroup)
	   (nnheader-message 6 "nnimap: Retrieving headers...done")))))

(defun nnimap-use-nov-p (group server)
  (or gnus-nov-is-evil nnimap-nov-is-evil
      (unless (and (gnus-make-directory
		    (file-name-directory
		     (nnimap-group-overview-filename group server)))
		   (file-writable-p
		    (nnimap-group-overview-filename group server)))
	(message "nnimap: Nov cache not writable, %s"
		 (nnimap-group-overview-filename group server)))))

(deffoo nnimap-retrieve-headers (articles &optional group server fetch-old)
  (when (nnimap-possibly-change-group group server)
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (if (nnimap-use-nov-p group server)
	  (nnimap-retrieve-headers-from-server
	   (gnus-compress-sequence articles) group server)
	(let (uids cached low high)
	  (when (setq uids (nnimap-retrieve-which-headers articles fetch-old)
		      low (car uids)
		      high (car (last uids)))
	    (if (setq cached (nnimap-retrieve-headers-from-file group server))
		(progn
		  ;; fetch articles with uids before cache block
		  (when (< low (car cached))
		    (goto-char (point-min))
		    (nnimap-retrieve-headers-from-server
		     (cons low (1- (car cached))) group server))
		  ;; fetch articles with uids after cache block
		  (when (> high (cdr cached))
		    (goto-char (point-max))
		    (nnimap-retrieve-headers-from-server
		     (cons (1+ (cdr cached)) high) group server))
		  (when nnimap-prune-cache
		    ;; remove nov's for articles which has expired on server
		    (goto-char (point-min))
		    (dolist (uid (gnus-set-difference articles uids))
                      (when (re-search-forward (format "^%d\t" uid) nil t)
                        (gnus-delete-line)))))
	      ;; nothing cached, fetch whole range from server
	      (nnimap-retrieve-headers-from-server
	       (cons low high) group server))
	    (when (buffer-modified-p)
	      (nnmail-write-region
	       (point-min) (point-max)
	       (nnimap-group-overview-filename group server) nil 'nomesg))
	    (nnheader-nov-delete-outside-range low high))))
      'nov)))

(defun nnimap-open-connection (server)
  (if (not (imap-open nnimap-address nnimap-server-port nnimap-stream
		      nnimap-authenticator nnimap-server-buffer))
      (nnheader-report 'nnimap "Can't open connection to server %s" server)
    (unless (or (imap-capability 'IMAP4 nnimap-server-buffer)
		(imap-capability 'IMAP4rev1 nnimap-server-buffer))
      (imap-close nnimap-server-buffer)
      (nnheader-report 'nnimap "Server %s is not IMAP4 compliant" server))
    (let* ((list (gnus-parse-netrc nnimap-authinfo-file))
	   (port (if nnimap-server-port
		     (int-to-string nnimap-server-port)
		   "imap"))
	   (alist (gnus-netrc-machine list (or nnimap-server-address 
                                               nnimap-address server)
                                      port "imap"))
	   (user (gnus-netrc-get alist "login"))
	   (passwd (gnus-netrc-get alist "password")))
      (if (imap-authenticate user passwd nnimap-server-buffer)
	  (prog1
	      (push (list server nnimap-server-buffer)
		    nnimap-server-buffer-alist)
	    (nnimap-possibly-change-server server))
	(imap-close nnimap-server-buffer)
	(kill-buffer nnimap-server-buffer)
	(nnheader-report 'nnimap "Could not authenticate to %s" server)))))

(deffoo nnimap-open-server (server &optional defs)
  (nnheader-init-server-buffer)
  (if (nnimap-server-opened server)
      t
    (unless (assq 'nnimap-server-buffer defs)
      (push (list 'nnimap-server-buffer (concat " *nnimap* " server)) defs))
    ;; translate `nnimap-server-address' to `nnimap-address' in defs
    ;; for people that configured nnimap with a very old version
    (unless (assq 'nnimap-address defs)
      (if (assq 'nnimap-server-address defs)
	  (push (list 'nnimap-address
		      (cadr (assq 'nnimap-server-address defs))) defs)
	(push (list 'nnimap-address server) defs)))
    (nnoo-change-server 'nnimap server defs)
    (with-current-buffer (get-buffer-create nnimap-server-buffer)
      (nnoo-change-server 'nnimap server defs))
    (or (and nnimap-server-buffer
	     (imap-opened nnimap-server-buffer))
	(nnimap-open-connection server))))

(deffoo nnimap-server-opened (&optional server)
  "Whether SERVER is opened.
If SERVER is the current virtual server, and the connection to the
physical server is alive, this function return a non-nil value.  If
SERVER is nil, it is treated as the current server."
  ;; clean up autologouts??
  (and (or server nnimap-current-server)
       (nnoo-server-opened 'nnimap (or server nnimap-current-server))
       (imap-opened (nnimap-get-server-buffer server))))

(deffoo nnimap-close-server (&optional server)
  "Close connection to server and free all resources connected to it.
Return nil if the server couldn't be closed for some reason."
  (let ((server (or server nnimap-current-server)))
    (when (or (nnimap-server-opened server)
	      (imap-opened (nnimap-get-server-buffer server)))
      (imap-close (nnimap-get-server-buffer server))
      (kill-buffer (nnimap-get-server-buffer server))
      (setq nnimap-server-buffer nil
	    nnimap-current-server nil
	    nnimap-server-buffer-alist
	    (delq server nnimap-server-buffer-alist)))
    (nnoo-close-server 'nnimap server)))

(deffoo nnimap-request-close ()
  "Close connection to all servers and free all resources that the backend have reserved.
All buffers that have been created by that
backend should be killed.  (Not the nntp-server-buffer, though.) This
function is generally only called when Gnus is shutting down."
  (mapcar (lambda (server) (nnimap-close-server (car server)))
	  nnimap-server-buffer-alist)
  (setq nnimap-server-buffer-alist nil))

(deffoo nnimap-status-message (&optional server)
  "This function returns the last error message from server."
  (when (nnimap-possibly-change-server server)
    (nnoo-status-message 'nnimap server)))

(defun nnimap-demule (string)
  (funcall (if (and (fboundp 'string-as-multibyte)
		    (subrp (symbol-function 'string-as-multibyte)))
	       'string-as-multibyte
	     'identity)
	   (or string "")))

(defun nnimap-callback ()
  (remove-hook 'imap-fetch-data-hook 'nnimap-callback)
  (with-current-buffer nnimap-callback-buffer
    (insert
     (with-current-buffer nnimap-server-buffer
       (nnimap-demule
        (if (imap-capability 'IMAP4rev1) 
            ;; xxx don't just use car? alist doesn't contain
            ;; anything else now, but it might...
            (nth 2 (car (imap-message-get (imap-current-message) 'BODYDETAIL)))
          (imap-message-get (imap-current-message) 'RFC822)))))
    (nnheader-ms-strip-cr)
    (funcall nnimap-callback-callback-function t)))

(defun nnimap-request-article-part (article part prop &optional
                                            group server to-buffer detail)
  (when (nnimap-possibly-change-group group server)
    (let ((article (if (stringp article)
		       (car-safe (imap-search
				  (format "HEADER Message-Id %s" article)
				  nnimap-server-buffer))
		     article)))
      (when article
	(gnus-message 10 "nnimap: Fetching (part of) article %d..." article)
	(if (not nnheader-callback-function)
	    (with-current-buffer (or to-buffer nntp-server-buffer)
	      (erase-buffer)
              (let ((data (imap-fetch article part prop nil
                                      nnimap-server-buffer)))
                (insert (nnimap-demule (if detail
                                           (nth 2 (car data))
                                         data))))
              (nnheader-ms-strip-cr)
	      (gnus-message 10 "nnimap: Fetching (part of) article %d...done"
			    article)
	      (if (bobp)
		  (nnheader-report 'nnimap "No such article: %s"
				   (imap-error-text nnimap-server-buffer))
		(cons group article)))
	  (add-hook 'imap-fetch-data-hook 'nnimap-callback)
	  (setq nnimap-callback-callback-function nnheader-callback-function
		nnimap-callback-buffer nntp-server-buffer)
	  (imap-fetch-asynch article part nil nnimap-server-buffer)
	  (cons group article))))))

(deffoo nnimap-asynchronous-p ()
  t)

(deffoo nnimap-request-article (article &optional group server to-buffer)
  (if (imap-capability 'IMAP4rev1 nnimap-server-buffer)
      (nnimap-request-article-part
       article "BODY.PEEK[]" 'BODYDETAIL group server to-buffer 'detail)
    (nnimap-request-article-part
     article "RFC822.PEEK" 'RFC822 group server to-buffer)))

(deffoo nnimap-request-head (article &optional group server to-buffer)
  (if (imap-capability 'IMAP4rev1 nnimap-server-buffer)
      (nnimap-request-article-part
       article "BODY.PEEK[HEADER]" 'BODYDETAIL group server to-buffer 'detail)
    (nnimap-request-article-part
     article "RFC822.HEADER" 'RFC822.HEADER group server to-buffer)))

(deffoo nnimap-request-body (article &optional group server to-buffer)
  (if (imap-capability 'IMAP4rev1 nnimap-server-buffer)
      (nnimap-request-article-part
       article "BODY.PEEK[TEXT]" 'BODYDETAIL group server to-buffer 'detail)
    (nnimap-request-article-part
     article "RFC822.TEXT.PEEK" 'RFC822.TEXT group server to-buffer)))

(deffoo nnimap-request-group (group &optional server fast)
  (nnimap-request-update-info-internal
   group
   (gnus-get-info (gnus-group-prefixed-name
		   group (gnus-server-to-method (format "nnimap:%s" server))))
   server)
  (when (nnimap-possibly-change-group group server)
    (nnimap-before-find-minmax-bugworkaround)
    (let (info)
      (cond (fast group)
	    ((null (setq info (nnimap-find-minmax-uid group t)))
	     (nnheader-report 'nnimap "Could not get active info for %s"
			      group))
	    (t
	     (nnheader-insert "211 %d %d %d %s\n" (or (nth 0 info) 0)
			      (max 1 (or (nth 1 info) 1))
			      (or (nth 2 info) 0) group)
	     (nnheader-report 'nnimap "Group %s selected" group)
	     t)))))

(defun nnimap-close-group (group &optional server)
  (with-current-buffer nnimap-server-buffer
    (when (and (imap-opened)
	       (nnimap-possibly-change-group group server))
      (case nnimap-expunge-on-close
	('always (imap-mailbox-expunge)
		 (imap-mailbox-close))
	('ask (if (and (imap-search "DELETED")
		       (gnus-y-or-n-p (format
				       "Expunge articles in group `%s'? "
				       imap-current-mailbox)))
		  (progn (imap-mailbox-expunge)
			 (imap-mailbox-close))
		(imap-mailbox-unselect)))
	(t (imap-mailbox-unselect)))
      (not imap-current-mailbox))))

(defun nnimap-pattern-to-list-arguments (pattern)
  (mapcar (lambda (p)
	    (cons (car-safe p) (or (cdr-safe p) p)))
	  (if (and (listp pattern)
		   (listp (cdr pattern)))
	      pattern
	    (list pattern))))

(deffoo nnimap-request-list (&optional server)
  (when (nnimap-possibly-change-server server)
    (with-current-buffer nntp-server-buffer
      (erase-buffer))
    (gnus-message 5 "nnimap: Generating active list%s..."
		  (if (> (length server) 0) (concat " for " server) ""))
    (nnimap-before-find-minmax-bugworkaround)
    (with-current-buffer nnimap-server-buffer
      (dolist (pattern (nnimap-pattern-to-list-arguments nnimap-list-pattern))
	(dolist (mbx (funcall nnimap-request-list-method
			      (cdr pattern) (car pattern)))
	  (or (member "\\NoSelect" (imap-mailbox-get 'list-flags mbx))
	      (let ((info (nnimap-find-minmax-uid mbx 'examine)))
		(when info
		  (with-current-buffer nntp-server-buffer
                   (insert (format "\"%s\" %d %d y\n"
                                   mbx (or (nth 2 info) 0)
                                   (max 1 (or (nth 1 info) 1)))))))))))
    (gnus-message 5 "nnimap: Generating active list%s...done"
		  (if (> (length server) 0) (concat " for " server) ""))
    t))

(deffoo nnimap-request-post (&optional server)
  (let ((success t))
    (dolist (mbx (message-unquote-tokens
                  (message-tokenize-header
                   (message-fetch-field "Newsgroups") ", ")) success)
      (let ((to-newsgroup (gnus-group-prefixed-name mbx gnus-command-method)))
	(or (gnus-active to-newsgroup)
	    (gnus-activate-group to-newsgroup)
	    (if (gnus-y-or-n-p (format "No such group: %s.  Create it? "
				       to-newsgroup))
		(or (and (gnus-request-create-group
			  to-newsgroup gnus-command-method)
			 (gnus-activate-group to-newsgroup nil nil
					      gnus-command-method))
		    (error "Couldn't create group %s" to-newsgroup)))
	    (error "No such group: %s" to-newsgroup))
	(unless (nnimap-request-accept-article mbx (nth 1 gnus-command-method))
	  (setq success nil))))))

;; Optional backend functions

(deffoo nnimap-retrieve-groups (groups &optional server)
  (when (nnimap-possibly-change-server server)
    (gnus-message 5 "nnimap: Checking mailboxes...")
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (nnimap-before-find-minmax-bugworkaround)
      (dolist (group groups)
	(gnus-message 7 "nnimap: Checking mailbox %s" group)
	(or (member "\\NoSelect"
		    (imap-mailbox-get 'list-flags group nnimap-server-buffer))
	    (let ((info (nnimap-find-minmax-uid group 'examine)))
	      (insert (format "\"%s\" %d %d y\n" group
			      (or (nth 2 info) 0)
			      (max 1 (or (nth 1 info) 1))))))))
    (gnus-message 5 "nnimap: Checking mailboxes...done")
    'active))

(deffoo nnimap-request-update-info-internal (group info &optional server)
  (when (nnimap-possibly-change-group group server)
    (when info;; xxx what does this mean? should we create a info?
      (with-current-buffer nnimap-server-buffer
	(gnus-message 5 "nnimap: Updating info for %s..."
		      (gnus-info-group info))
	
	(when (nnimap-mark-permanent-p 'read)
	  (let (seen unseen)
	    ;; read info could contain articles marked unread by other
	    ;; imap clients!  we correct this
	    (setq seen (gnus-uncompress-range (gnus-info-read info))
		  unseen (imap-search "UNSEEN UNDELETED")
		  seen (gnus-set-difference seen unseen)
		  ;; seen might lack articles marked as read by other
		  ;; imap clients! we correct this
		  seen (append seen (imap-search "SEEN"))
		  ;; remove dupes
		  seen (sort seen '<)
		  seen (gnus-compress-sequence seen t)
		  ;; we can't return '(1) since this isn't a "list of ranges",
		  ;; and we can't return '((1)) since g-list-of-unread-articles
		  ;; is buggy so we return '((1 . 1)).
		  seen (if (and (integerp (car seen))
				(null (cdr seen)))
			   (list (cons (car seen) (car seen)))
			 seen))
	    (gnus-info-set-read info seen)))

	(mapcar (lambda (pred)
		  (when (and (nnimap-mark-permanent-p (cdr pred))
			     (member (nnimap-mark-to-flag (cdr pred))
				     (imap-mailbox-get 'flags)))
		    (gnus-info-set-marks
		     info
		     (nnimap-update-alist-soft
		      (cdr pred)
		      (gnus-compress-sequence
		       (imap-search (nnimap-mark-to-predicate (cdr pred))))
		      (gnus-info-marks info))
		     t)))
		gnus-article-mark-lists)

	;; nnimap mark dormant article as ticked too (for other clients)
	;; so we remove that mark for gnus since we support dormant
	(gnus-info-set-marks
	 info 
	 (nnimap-update-alist-soft
	  'tick
	  (gnus-remove-from-range
	   (cdr-safe (assoc 'tick (gnus-info-marks info)))
	   (cdr-safe (assoc 'dormant (gnus-info-marks info))))
	  (gnus-info-marks info))
	 t)
	
	(gnus-message 5 "nnimap: Updating info for %s...done"
		      (gnus-info-group info))

	info))))

(deffoo nnimap-request-type (group &optional article)
  (if (and nnimap-news-groups (string-match nnimap-news-groups group))
      'news
    'mail))

(deffoo nnimap-request-set-mark (group actions &optional server)
  (when (nnimap-possibly-change-group group server)
    (with-current-buffer nnimap-server-buffer
      (let (action)
	(gnus-message 7 "nnimap: Setting marks in %s..." group)
	(while (setq action (pop actions))
	  (let ((range (nth 0 action))
		(what  (nth 1 action))
		(cmdmarks (nth 2 action))
		marks)
	    ;; cache flags are pointless on the server
	    (setq cmdmarks (delq 'cache cmdmarks))
	    ;; flag dormant articles as ticked
	    (if (memq 'dormant cmdmarks)
		(setq cmdmarks (cons 'tick cmdmarks)))
	    ;; remove stuff we are forbidden to store
	    (mapcar (lambda (mark)
		      (if (imap-message-flag-permanent-p
			   (nnimap-mark-to-flag mark))
			  (setq marks (cons mark marks))))
		    cmdmarks)
	    (when (and range marks)
	      (cond ((eq what 'del)
		     (imap-message-flags-del
		      (imap-range-to-message-set range)
		      (nnimap-mark-to-flag marks nil t)))
		    ((eq what 'add)
		     (imap-message-flags-add
		      (imap-range-to-message-set range)
		      (nnimap-mark-to-flag marks nil t)))
		    ((eq what 'set)
		     (imap-message-flags-set
		      (imap-range-to-message-set range)
		      (nnimap-mark-to-flag marks nil t)))))))
	(gnus-message 7 "nnimap: Setting marks in %s...done" group))))
  nil)

(defun nnimap-split-fancy ()
  "Like nnmail-split-fancy, but uses nnimap-split-fancy."
  (let ((nnmail-split-fancy nnimap-split-fancy))
    (nnmail-split-fancy)))

(defun nnimap-split-to-groups (rules)
  ;; tries to match all rules in nnimap-split-rule against content of
  ;; nntp-server-buffer, returns a list of groups that matched.
  (with-current-buffer nntp-server-buffer
    ;; Fold continuation lines.
    (goto-char (point-min))
    (while (re-search-forward "\\(\r?\n[ \t]+\\)+" nil t)
      (replace-match " " t t))
    (if (functionp rules)
	(funcall rules)
      (let (to-groups regrepp)
	(catch 'split-done
	  (dolist (rule rules to-groups)
	    (let ((group (car rule))
		  (regexp (cadr rule)))
	      (goto-char (point-min))
	      (when (and (if (stringp regexp)
			     (progn
			       (setq regrepp (string-match "\\\\[0-9&]" group))
			       (re-search-forward regexp nil t))
			   (funcall regexp group))
			 ;; Don't enter the article into the same group twice.
			 (not (assoc group to-groups)))
		(push (if regrepp
			  (nnmail-expand-newtext group)
			group)
		      to-groups)
		(or nnimap-split-crosspost
		    (throw 'split-done to-groups))))))))))
  
(defun nnimap-assoc-match (key alist)
  (let (element)
    (while (and alist (not element))
      (if (string-match (car (car alist)) key)
	  (setq element (car alist)))
      (setq alist (cdr alist)))
    element))

(defun nnimap-split-find-rule (server inbox)
  (if (and (listp nnimap-split-rule) (listp (car nnimap-split-rule))
           (list (cdar nnimap-split-rule)) (listp (cadar nnimap-split-rule)))
      ;; extended format
      (cadr (nnimap-assoc-match inbox (cdr (nnimap-assoc-match 
					    server nnimap-split-rule))))
    nnimap-split-rule))

(defun nnimap-split-find-inbox (server)
  (if (listp nnimap-split-inbox)
      nnimap-split-inbox
    (list nnimap-split-inbox)))

(defun nnimap-split-articles (&optional group server)
  (when (nnimap-possibly-change-server server)
    (with-current-buffer nnimap-server-buffer
      (let (rule inbox removeorig (inboxes (nnimap-split-find-inbox server)))
	;; iterate over inboxes
	(while (and (setq inbox (pop inboxes))
		    (nnimap-possibly-change-group inbox));; SELECT
	  ;; find split rule for this server / inbox
	  (when (setq rule (nnimap-split-find-rule server inbox))
	    ;; iterate over articles
	    (dolist (article (imap-search nnimap-split-predicate))
	      (when (nnimap-request-head article)
		;; copy article to right group(s)
		(setq removeorig nil)
		(dolist (to-group (nnimap-split-to-groups rule))
		  (if (imap-message-copy (number-to-string article)
					 to-group nil 'nocopyuid)
		      (progn
			(message "IMAP split moved %s:%s:%d to %s" server inbox
				 article to-group)
			(setq removeorig t)
			;; Add the group-art list to the history list.
			(push (list (cons to-group 0)) nnmail-split-history))
		    (message "IMAP split failed to move %s:%s:%d to %s" server
			     inbox article to-group)))
		;; remove article if it was successfully copied somewhere
		(and removeorig
		     (imap-message-flags-add (format "%d" article)
					     "\\Seen \\Deleted")))))
	  (when (imap-mailbox-select inbox);; just in case
	    ;; todo: UID EXPUNGE (if available) to remove splitted articles
	    (imap-mailbox-expunge)
	    (imap-mailbox-close)))
	t))))

(deffoo nnimap-request-scan (&optional group server)
  (nnimap-split-articles group server))

(deffoo nnimap-request-newgroups (date &optional server)
  (when (nnimap-possibly-change-server server)
    (with-current-buffer nntp-server-buffer
      (gnus-message 5 "nnimap: Listing subscribed mailboxes%s%s..."
		    (if (> (length server) 0) " on " "") server)
      (erase-buffer)
      (nnimap-before-find-minmax-bugworkaround)
      (dolist (pattern (nnimap-pattern-to-list-arguments
			nnimap-list-pattern))
	(dolist (mbx (imap-mailbox-lsub "*" (car pattern) nil 
					nnimap-server-buffer))
	  (or (catch 'found
		(dolist (mailbox (imap-mailbox-get 'list-flags mbx
						   nnimap-server-buffer))
		  (if (string= (downcase mailbox) "\\noselect")
		      (throw 'found t)))
		nil)
	      (let ((info (nnimap-find-minmax-uid mbx 'examine)))
		(when info
                 (insert (format "\"%s\" %d %d y\n"
                                 mbx (or (nth 2 info) 0)
				 (max 1 (or (nth 1 info) 1)))))))))
      (gnus-message 5 "nnimap: Listing subscribed mailboxes%s%s...done"
		    (if (> (length server) 0) " on " "") server))
    t))
      
(deffoo nnimap-request-create-group (group &optional server args)
  (when (nnimap-possibly-change-server server)
    (or (imap-mailbox-status group 'uidvalidity nnimap-server-buffer)
	(imap-mailbox-create group nnimap-server-buffer))))

(defun nnimap-time-substract (time1 time2)
  "Return TIME for TIME1 - TIME2."
  (let* ((ms (- (car time1) (car time2)))
	 (ls (- (nth 1 time1) (nth 1 time2))))
    (if (< ls 0)
	(list (- ms 1) (+ (expt 2 16) ls))
      (list ms ls))))

(defun nnimap-date-days-ago (daysago)
  "Return date, in format \"3-Aug-1998\", for DAYSAGO days ago."
  (let* ((time (nnimap-time-substract (current-time) (days-to-time daysago)))
	 (date (format-time-string
		(format "%%d-%s-%%Y"
			(capitalize (car (rassoc (nth 4 (decode-time time))
						 parse-time-months))))
		time)))
    (if (eq ?0 (string-to-char date))
	(substring date 1)
      date)))

(defun nnimap-request-expire-articles-progress ()
  (gnus-message 5 "nnimap: Marking article %d for deletion..."
		imap-current-message))

;; Notice that we don't actually delete anything, we just mark them deleted.
(deffoo nnimap-request-expire-articles (articles group &optional server force)
  (let ((artseq (gnus-compress-sequence articles)))
    (when (and artseq (nnimap-possibly-change-group group server))
      (with-current-buffer nnimap-server-buffer
	(if force
	    (and (imap-message-flags-add
		  (imap-range-to-message-set artseq) "\\Deleted")
		 (setq articles nil))
	  (let ((days (or (and nnmail-expiry-wait-function
			       (funcall nnmail-expiry-wait-function group))
			  nnmail-expiry-wait)))
	    (cond ((eq days 'immediate)
		   (and (imap-message-flags-add
			 (imap-range-to-message-set artseq) "\\Deleted")
			(setq articles nil)))
		  ((numberp days)
		   (let ((oldarts (imap-search
				   (format "UID %s NOT SINCE %s"
					   (imap-range-to-message-set artseq)
					   (nnimap-date-days-ago days))))
			 (imap-fetch-data-hook
			  '(nnimap-request-expire-articles-progress)))
		     (and oldarts
			  (imap-message-flags-add
			   (imap-range-to-message-set
			    (gnus-compress-sequence oldarts))
			   "\\Deleted")
			  (setq articles (gnus-set-difference
					  articles oldarts)))))))))))
  ;; return articles not deleted
  articles)

(deffoo nnimap-request-move-article (article group server
					     accept-form &optional last)
  (when (nnimap-possibly-change-server server)
    (save-excursion
      (let ((buf (get-buffer-create " *nnimap move*"))
	    (nnimap-current-move-article article)
	    (nnimap-current-move-group group)
	    (nnimap-current-move-server nnimap-current-server)
	    result)
	(and (nnimap-request-article article group server)
	     (save-excursion
	       (set-buffer buf)
	       (buffer-disable-undo (current-buffer))
	       (insert-buffer-substring nntp-server-buffer)
	       (setq result (eval accept-form))
	       (kill-buffer buf)
	       result)
	     (nnimap-request-expire-articles (list article) group server t))
	result))))
  
(deffoo nnimap-request-accept-article (group &optional server last)
  (when (nnimap-possibly-change-server server)
    (let (uid)
      (if (setq uid
		(if (string= nnimap-current-server nnimap-current-move-server)
		    ;; moving article within same server, speed it up...
		    (and (nnimap-possibly-change-group
			  nnimap-current-move-group)
			 (imap-message-copy (number-to-string
					     nnimap-current-move-article)
					    group 'dontcreate nil
					    nnimap-server-buffer))
		  (with-current-buffer (current-buffer)
		    (goto-char (point-min))
		    ;; remove any 'From blabla' lines, some IMAP servers
		    ;; reject the entire message otherwise.
		    (when (looking-at "^From[^:]")
		      (kill-region (point) (progn (forward-line) (point))))
		    ;; turn into rfc822 format (\r\n eol's)
		    (while (search-forward "\n" nil t)
		      (replace-match "\r\n")))
                  ;; this 'or' is for Cyrus server bug
                  (or (null (imap-current-mailbox nnimap-server-buffer))
                      (imap-mailbox-unselect nnimap-server-buffer))
		  (imap-message-append group (current-buffer) nil nil
				       nnimap-server-buffer)))
	  (cons group (nth 1 uid))
	(nnheader-report 'nnimap (imap-error-text nnimap-server-buffer))))))

(deffoo nnimap-request-delete-group (group force &optional server)
  (when (nnimap-possibly-change-server server)
    (with-current-buffer nnimap-server-buffer
      (if force
	  (or (null (imap-mailbox-status group 'uidvalidity))
	      (imap-mailbox-delete group))
	;; UNSUBSCRIBE?
	t))))

(deffoo nnimap-request-rename-group (group new-name &optional server)
  (when (nnimap-possibly-change-server server)
    (imap-mailbox-rename group new-name nnimap-server-buffer)))

(defun nnimap-expunge (mailbox server)
  (when (nnimap-possibly-change-group mailbox server)
    (imap-mailbox-expunge nnimap-server-buffer)))

(defun nnimap-acl-get (mailbox server)
  (when (nnimap-possibly-change-server server)
    (and (imap-capability 'ACL nnimap-server-buffer)
	 (imap-mailbox-acl-get mailbox nnimap-server-buffer))))

(defun nnimap-acl-edit (mailbox method old-acls new-acls)
  (when (nnimap-possibly-change-server (cadr method))
    (unless (imap-capability 'ACL nnimap-server-buffer)
      (error "Your server does not support ACL editing"))
    (with-current-buffer nnimap-server-buffer
      ;; delete all removed identifiers
      (mapcar (lambda (old-acl)
		(unless (assoc (car old-acl) new-acls)
		  (or (imap-mailbox-acl-delete (car old-acl) mailbox)
		      (error "Can't delete ACL for %s" (car old-acl)))))
	      old-acls)
      ;; set all changed acl's
      (mapcar (lambda (new-acl)
		(let ((new-rights (cdr new-acl))
		      (old-rights (cdr (assoc (car new-acl) old-acls))))
		  (unless (and old-rights new-rights
			       (string= old-rights new-rights))
		    (or (imap-mailbox-acl-set (car new-acl) new-rights mailbox)
			(error "Can't set ACL for %s to %s" (car new-acl)
			       new-rights)))))
	      new-acls)
      t)))


;;; Internal functions

;;
;; This is confusing.
;;
;; mark      => read, tick, draft, reply etc
;; flag      => "\\Seen", "\\Flagged", "\\Draft", "gnus-expire" etc
;; predicate => "SEEN", "FLAGGED", "DRAFT", "KEYWORD gnus-expire" etc
;;
;; Mark should not really contain 'read since it's not a "mark" in the Gnus
;; world, but we cheat.  Mark == gnus-article-mark-lists + '(read . read).
;;

(defconst nnimap-mark-to-predicate-alist
  (mapcar
   (lambda (pair)			; cdr is the mark
     (or (assoc (cdr pair)
                '((read . "SEEN")
                  (tick . "FLAGGED")
                  (draft . "DRAFT")
                  (reply . "ANSWERED")))
         (cons (cdr pair)
               (format "KEYWORD gnus-%s" (symbol-name (cdr pair))))))
   (cons '(read . read) gnus-article-mark-lists)))

(defun nnimap-mark-to-predicate (pred)
  "Convert a Gnus mark (a symbol such as read, tick, expire) to a IMAP predicate.
This is a string such as \"SEEN\", \"FLAGGED\", \"KEYWORD gnus-expire\",
to be used within a IMAP SEARCH query."
  (cdr (assq pred nnimap-mark-to-predicate-alist)))

(defconst nnimap-mark-to-flag-alist
  (mapcar
   (lambda (pair)
     (or (assoc (cdr pair)
                '((read . "\\Seen")
                  (tick . "\\Flagged")
                  (draft . "\\Draft")
                  (reply . "\\Answered")))
         (cons (cdr pair)
               (format "gnus-%s" (symbol-name (cdr pair))))))
   (cons '(read . read) gnus-article-mark-lists)))

(defun nnimap-mark-to-flag-1 (preds)
  (if (and (not (null preds)) (listp preds))
      (cons (nnimap-mark-to-flag (car preds))
	    (nnimap-mark-to-flag (cdr preds)))
    (cdr (assoc preds nnimap-mark-to-flag-alist))))

(defun nnimap-mark-to-flag (preds &optional always-list make-string)
  "Convert a Gnus mark (a symbol such as read, tick, expire) to a IMAP flag.
This is a string such as \"\\Seen\", \"\\Flagged\", \"gnus-expire\", to
be used in a STORE FLAGS command."
  (let ((result (nnimap-mark-to-flag-1 preds)))
    (setq result (if (and (or make-string always-list)
			  (not (listp result)))
		     (list result)
		   result))
    (if make-string
	(mapconcat (lambda (flag)
		     (if (listp flag)
			 (mapconcat 'identity flag " ")
		       flag))
		   result " ")
      result)))

(defun nnimap-mark-permanent-p (mark &optional group)
  "Return t iff MARK can be permanently (between IMAP sessions) saved on articles, in GROUP."
  (imap-message-flag-permanent-p (nnimap-mark-to-flag mark)))

(defun nnimap-remassoc (key alist)
  "Delete by side effect any elements of LIST whose car is `equal' to KEY.
The modified LIST is returned.  If the first member
of LIST has a car that is `equal' to KEY, there is no way to remove it
by side effect; therefore, write `(setq foo (remassoc key foo))' to be
sure of changing the value of `foo'."
  (when alist
    (if (equal key (caar alist))
	(cdr alist)
      (setcdr alist (nnimap-remassoc key (cdr alist)))
      alist)))
  
(defun nnimap-update-alist-soft (key value alist)
  (if value
      (cons (cons key value) (nnimap-remassoc key alist))
    (nnimap-remassoc key alist)))

(when nnimap-debug
  (require 'trace)
  (buffer-disable-undo (get-buffer-create nnimap-debug))
  (mapcar (lambda (f) (trace-function-background f nnimap-debug))
        '(
	  nnimap-possibly-change-server
	  nnimap-verify-uidvalidity
	  nnimap-find-minmax-uid
	  nnimap-before-find-minmax-bugworkaround
	  nnimap-possibly-change-group
	  ;;nnimap-replace-whitespace
	  nnimap-retrieve-headers-progress
	  nnimap-retrieve-which-headers
	  nnimap-group-overview-filename
	  nnimap-retrieve-headers-from-file
	  nnimap-retrieve-headers-from-server
	  nnimap-retrieve-headers
	  nnimap-open-connection
	  nnimap-open-server
	  nnimap-server-opened
	  nnimap-close-server
	  nnimap-request-close
	  nnimap-status-message
	  ;;nnimap-demule
	  nnimap-request-article-part
	  nnimap-request-article
	  nnimap-request-head
	  nnimap-request-body
	  nnimap-request-group
	  nnimap-close-group
	  nnimap-pattern-to-list-arguments
	  nnimap-request-list
	  nnimap-request-post
	  nnimap-retrieve-groups
	  nnimap-request-update-info-internal
	  nnimap-request-type
	  nnimap-request-set-mark
	  nnimap-split-to-groups
	  nnimap-split-find-rule
	  nnimap-split-find-inbox
	  nnimap-split-articles
	  nnimap-request-scan
	  nnimap-request-newgroups
	  nnimap-request-create-group
	  nnimap-time-substract
	  nnimap-date-days-ago
	  nnimap-request-expire-articles-progress
	  nnimap-request-expire-articles
	  nnimap-request-move-article
	  nnimap-request-accept-article
	  nnimap-request-delete-group
	  nnimap-request-rename-group
	  gnus-group-nnimap-expunge
	  gnus-group-nnimap-edit-acl
	  gnus-group-nnimap-edit-acl-done
	  nnimap-group-mode-hook
	  nnimap-mark-to-predicate
	  nnimap-mark-to-flag-1
	  nnimap-mark-to-flag
	  nnimap-mark-permanent-p
	  nnimap-remassoc
	  nnimap-update-alist-soft
          )))

(provide 'nnimap)

;;; nnimap.el ends here
