;;; gnus-start.el --- startup functions for Gnus
;; Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002
;;        Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

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

;;; Code:

(require 'gnus)
(require 'gnus-win)
(require 'gnus-int)
(require 'gnus-spec)
(require 'gnus-range)
(require 'gnus-util)
(require 'message)
(eval-when-compile (require 'cl))

(defcustom gnus-startup-file (nnheader-concat gnus-home-directory ".newsrc")
  "Your `.newsrc' file.
`.newsrc-SERVER' will be used instead if that exists."
  :group 'gnus-start
  :type 'file)

(defcustom gnus-init-file (nnheader-concat gnus-home-directory ".gnus")
  "Your Gnus Emacs-Lisp startup file name.
If a file with the `.el' or `.elc' suffixes exists, it will be read instead."
  :group 'gnus-start
  :type 'file)

(defcustom gnus-site-init-file
  (condition-case nil
      (concat (file-name-directory
	       (directory-file-name installation-directory))
	      "site-lisp/gnus-init")
    (error nil))
  "The site-wide Gnus Emacs-Lisp startup file name, or nil if none.
If a file with the `.el' or `.elc' suffixes exists, it will be read instead."
  :group 'gnus-start
  :type '(choice file (const nil)))

(defcustom gnus-default-subscribed-newsgroups nil
  "List of newsgroups to subscribe, when a user runs Gnus the first time.
The value should be a list of strings.
If it is t, Gnus will not do anything special the first time it is
started; it'll just use the normal newsgroups subscription methods."
  :group 'gnus-start
  :type '(choice (repeat string) (const :tag "Nothing special" t)))

(defcustom gnus-use-dribble-file t
  "*Non-nil means that Gnus will use a dribble file to store user updates.
If Emacs should crash without saving the .newsrc files, complete
information can be restored from the dribble file."
  :group 'gnus-dribble-file
  :type 'boolean)

(defcustom gnus-dribble-directory nil
  "*The directory where dribble files will be saved.
If this variable is nil, the directory where the .newsrc files are
saved will be used."
  :group 'gnus-dribble-file
  :type '(choice directory (const nil)))

(defcustom gnus-check-new-newsgroups 'ask-server
  "*Non-nil means that Gnus will run `gnus-find-new-newsgroups' at startup.
This normally finds new newsgroups by comparing the active groups the
servers have already reported with those Gnus already knows, either alive
or killed.

When any of the following are true, `gnus-find-new-newsgroups' will instead
ask the servers (primary, secondary, and archive servers) to list new
groups since the last time it checked:
  1. This variable is `ask-server'.
  2. This variable is a list of select methods (see below).
  3. `gnus-read-active-file' is nil or `some'.
  4. A prefix argument is given to `gnus-find-new-newsgroups' interactively.

Thus, if this variable is `ask-server' or a list of select methods or
`gnus-read-active-file' is nil or `some', then the killed list is no
longer necessary, so you could safely set `gnus-save-killed-list' to nil.

This variable can be a list of select methods which Gnus will query with
the `ask-server' method in addition to the primary, secondary, and archive
servers.

Eg.
  (setq gnus-check-new-newsgroups
	'((nntp \"some.server\") (nntp \"other.server\")))

If this variable is nil, then you have to tell Gnus explicitly to
check for new newsgroups with \\<gnus-group-mode-map>\\[gnus-find-new-newsgroups]."
  :group 'gnus-start
  :type '(choice (const :tag "no" nil)
		 (const :tag "by brute force" t)
		 (const :tag "ask servers" ask-server)
		 (repeat :menu-tag "ask additional servers"
			 :tag "ask additional servers"
			 :value ((nntp ""))
			 (sexp :format "%v"))))

(defcustom gnus-check-bogus-newsgroups nil
  "*Non-nil means that Gnus will check and remove bogus newsgroup at startup.
If this variable is nil, then you have to tell Gnus explicitly to
check for bogus newsgroups with \\<gnus-group-mode-map>\\[gnus-group-check-bogus-groups]."
  :group 'gnus-start-server
  :type 'boolean)

(defcustom gnus-read-active-file 'some
  "*Non-nil means that Gnus will read the entire active file at startup.
If this variable is nil, Gnus will only know about the groups in your
`.newsrc' file.

If this variable is `some', Gnus will try to only read the relevant
parts of the active file from the server.  Not all servers support
this, and it might be quite slow with other servers, but this should
generally be faster than both the t and nil value.

If you set this variable to nil or `some', you probably still want to
be told about new newsgroups that arrive.  To do that, set
`gnus-check-new-newsgroups' to `ask-server'.  This may not work
properly with all servers."
  :group 'gnus-start-server
  :type '(choice (const nil)
		 (const some)
		 (const t)))

(defconst gnus-level-subscribed 5
  "Groups with levels less than or equal to this variable are subscribed.")

(defconst gnus-level-unsubscribed 7
  "Groups with levels less than or equal to this variable are unsubscribed.
Groups with levels less than `gnus-level-subscribed', which should be
less than this variable, are subscribed.")

(defconst gnus-level-zombie 8
  "Groups with this level are zombie groups.")

(defconst gnus-level-killed 9
  "Groups with this level are killed.")

(defcustom gnus-level-default-subscribed 3
  "*New subscribed groups will be subscribed at this level."
  :group 'gnus-group-levels
  :type 'integer)

(defcustom gnus-level-default-unsubscribed 6
  "*New unsubscribed groups will be unsubscribed at this level."
  :group 'gnus-group-levels
  :type 'integer)

(defcustom gnus-activate-level (1+ gnus-level-subscribed)
  "*Groups higher than this level won't be activated on startup.
Setting this variable to something low might save lots of time when
you have many groups that you aren't interested in."
  :group 'gnus-group-levels
  :type 'integer)

(defcustom gnus-activate-foreign-newsgroups 4
  "*If nil, Gnus will not check foreign newsgroups at startup.
If it is non-nil, it should be a number between one and nine.  Foreign
newsgroups that have a level lower or equal to this number will be
activated on startup.  For instance, if you want to active all
subscribed newsgroups, but not the rest, you'd set this variable to
`gnus-level-subscribed'.

If you subscribe to lots of newsgroups from different servers, startup
might take a while.  By setting this variable to nil, you'll save time,
but you won't be told how many unread articles there are in the
groups."
  :group 'gnus-group-levels
  :type '(choice integer
		 (const :tag "none" nil)))

(defcustom gnus-read-newsrc-file t
  "*Non-nil means that Gnus will read the `.newsrc' file.
Gnus always reads its own startup file, which is called
\".newsrc.eld\".  The file called \".newsrc\" is in a format that can
be readily understood by other newsreaders.  If you don't plan on
using other newsreaders, set this variable to nil to save some time on
entry."
  :version "21.1"
  :group 'gnus-newsrc
  :type 'boolean)

(defcustom gnus-save-newsrc-file t
  "*Non-nil means that Gnus will save the `.newsrc' file.
Gnus always saves its own startup file, which is called
\".newsrc.eld\".  The file called \".newsrc\" is in a format that can
be readily understood by other newsreaders.  If you don't plan on
using other newsreaders, set this variable to nil to save some time on
exit."
  :group 'gnus-newsrc
  :type 'boolean)

(defcustom gnus-save-killed-list t
  "*If non-nil, save the list of killed groups to the startup file.
If you set this variable to nil, you'll save both time (when starting
and quitting) and space (both memory and disk), but it will also mean
that Gnus has no record of which groups are new and which are old, so
the automatic new newsgroups subscription methods become meaningless.

You should always set `gnus-check-new-newsgroups' to `ask-server' or
nil if you set this variable to nil.

This variable can also be a regexp.  In that case, all groups that do
not match this regexp will be removed before saving the list."
  :group 'gnus-newsrc
  :type 'boolean)

(defcustom gnus-ignored-newsgroups
  (mapconcat 'identity
	     '("^to\\."			; not "real" groups
	       "^[0-9. \t]+ "		; all digits in name
	       "^[\"][]\"[#'()]"	; bogus characters
	       )
	     "\\|")
  "*A regexp to match uninteresting newsgroups in the active file.
Any lines in the active file matching this regular expression are
removed from the newsgroup list before anything else is done to it,
thus making them effectively non-existent."
  :group 'gnus-group-new
  :type 'regexp)

(defcustom gnus-subscribe-newsgroup-method 'gnus-subscribe-zombies
  "*Function called with a group name when new group is detected.
A few pre-made functions are supplied: `gnus-subscribe-randomly'
inserts new groups at the beginning of the list of groups;
`gnus-subscribe-alphabetically' inserts new groups in strict
alphabetic order; `gnus-subscribe-hierarchically' inserts new groups
in hierarchical newsgroup order; `gnus-subscribe-interactively' asks
for your decision; `gnus-subscribe-killed' kills all new groups;
`gnus-subscribe-zombies' will make all new groups into zombies;
`gnus-subscribe-topics' will enter groups into the topics that
claim them."
  :group 'gnus-group-new
  :type '(radio (function-item gnus-subscribe-randomly)
		(function-item gnus-subscribe-alphabetically)
		(function-item gnus-subscribe-hierarchically)
		(function-item gnus-subscribe-interactively)
		(function-item gnus-subscribe-killed)
		(function-item gnus-subscribe-zombies)
		(function-item gnus-subscribe-topics)
		function))

(defcustom gnus-subscribe-options-newsgroup-method
  'gnus-subscribe-alphabetically
  "*This function is called to subscribe newsgroups mentioned on \"options -n\" lines.
If, for instance, you want to subscribe to all newsgroups in the
\"no\" and \"alt\" hierarchies, you'd put the following in your
.newsrc file:

options -n no.all alt.all

Gnus will the subscribe all new newsgroups in these hierarchies with
the subscription method in this variable."
  :group 'gnus-group-new
  :type '(radio (function-item gnus-subscribe-randomly)
		(function-item gnus-subscribe-alphabetically)
		(function-item gnus-subscribe-hierarchically)
		(function-item gnus-subscribe-interactively)
		(function-item gnus-subscribe-killed)
		(function-item gnus-subscribe-zombies)
		function))

(defcustom gnus-subscribe-hierarchical-interactive nil
  "*If non-nil, Gnus will offer to subscribe hierarchically.
When a new hierarchy appears, Gnus will ask the user:

'alt.binaries': Do you want to subscribe to this hierarchy? ([d]ys):

If the user pressed `d', Gnus will descend the hierarchy, `y' will
subscribe to all newsgroups in the hierarchy and `s' will skip this
hierarchy in its entirety."
  :group 'gnus-group-new
  :type 'boolean)

(defcustom gnus-auto-subscribed-groups
  "nnml\\|^nnfolder\\|^nnmbox\\|^nnmh\\|^nnbabyl"
  "*All new groups that match this regexp will be subscribed automatically.
Note that this variable only deals with new groups.  It has no effect
whatsoever on old groups.

New groups that match this regexp will not be handled by
`gnus-subscribe-newsgroup-method'.  Instead, they will
be subscribed using `gnus-subscribe-options-newsgroup-method'."
  :group 'gnus-group-new
  :type 'regexp)

(defcustom gnus-options-subscribe nil
  "*All new groups matching this regexp will be subscribed unconditionally.
Note that this variable deals only with new newsgroups.	 This variable
does not affect old newsgroups.

New groups that match this regexp will not be handled by
`gnus-subscribe-newsgroup-method'.  Instead, they will
be subscribed using `gnus-subscribe-options-newsgroup-method'."
  :group 'gnus-group-new
  :type '(choice regexp
		 (const :tag "none" nil)))

(defcustom gnus-options-not-subscribe nil
  "*All new groups matching this regexp will be ignored.
Note that this variable deals only with new newsgroups.	 This variable
does not affect old (already subscribed) newsgroups."
  :group 'gnus-group-new
  :type '(choice regexp
		 (const :tag "none" nil)))

(defcustom gnus-modtime-botch nil
  "*Non-nil means .newsrc should be deleted prior to save.
Its use is due to the bogus appearance that .newsrc was modified on
disc."
  :group 'gnus-newsrc
  :type 'boolean)

(defcustom gnus-check-bogus-groups-hook nil
  "A hook run after removing bogus groups."
  :group 'gnus-start-server
  :type 'hook)

(defcustom gnus-startup-hook nil
  "A hook called at startup.
This hook is called after Gnus is connected to the NNTP server."
  :group 'gnus-start
  :type 'hook)

(defcustom gnus-before-startup-hook nil
  "A hook called at before startup.
This hook is called as the first thing when Gnus is started."
  :group 'gnus-start
  :type 'hook)

(defcustom gnus-started-hook nil
  "A hook called as the last thing after startup."
  :group 'gnus-start
  :type 'hook)

(defcustom gnus-setup-news-hook nil
  "A hook after reading the .newsrc file, but before generating the buffer."
  :group 'gnus-start
  :type 'hook)

(defcustom gnus-get-new-news-hook nil
  "A hook run just before Gnus checks for new news."
  :group 'gnus-group-new
  :type 'hook)

(defcustom gnus-after-getting-new-news-hook
  (when (gnus-boundp 'display-time-timer)
    '(display-time-event-handler))
  "*A hook run after Gnus checks for new news when Gnus is already running."
  :group 'gnus-group-new
  :type 'hook)

(defcustom gnus-save-newsrc-hook nil
  "A hook called before saving any of the newsrc files."
  :group 'gnus-newsrc
  :type 'hook)

(defcustom gnus-save-quick-newsrc-hook nil
  "A hook called just before saving the quick newsrc file.
Can be used to turn version control on or off."
  :group 'gnus-newsrc
  :type 'hook)

(defcustom gnus-save-standard-newsrc-hook nil
  "A hook called just before saving the standard newsrc file.
Can be used to turn version control on or off."
  :group 'gnus-newsrc
  :type 'hook)

(defcustom gnus-always-read-dribble-file nil
  "Unconditionally read the dribble file."
  :group 'gnus-newsrc
  :type 'boolean)

;;; Internal variables

(defvar gnus-ding-file-coding-system mm-universal-coding-system
  "Coding system for ding file.")

(defvar gnus-newsrc-file-version nil)
(defvar gnus-override-subscribe-method nil)
(defvar gnus-dribble-buffer nil)
(defvar gnus-newsrc-options nil
  "Options line in the .newsrc file.")

(defvar gnus-newsrc-options-n nil
  "List of regexps representing groups to be subscribed/ignored unconditionally.")

(defvar gnus-newsrc-last-checked-date nil
  "Date Gnus last asked server for new newsgroups.")

(defvar gnus-current-startup-file nil
  "Startup file for the current host.")

;; Byte-compiler warning.
(defvar gnus-group-line-format)

;; Suggested by Brian Edmonds <edmonds@cs.ubc.ca>.
(defvar gnus-init-inhibit nil)
(defun gnus-read-init-file (&optional inhibit-next)
  ;; Don't load .gnus if the -q option was used.
  (when init-file-user
    (if gnus-init-inhibit
	(setq gnus-init-inhibit nil)
      (setq gnus-init-inhibit inhibit-next)
      (dolist (file (list gnus-site-init-file gnus-init-file))
	(when (and file
		   (locate-library file))
	  (if (or debug-on-error debug-on-quit)
	      (load file nil t)
	    (condition-case var
		(load file nil t)
	      (error
	       (error "Error in %s: %s" file var)))))))))

;; For subscribing new newsgroup

(defun gnus-subscribe-hierarchical-interactive (groups)
  (let ((groups (sort groups 'string<))
	prefixes prefix start ans group starts)
    (while groups
      (setq prefixes (list "^"))
      (while (and groups prefixes)
	(while (not (string-match (car prefixes) (car groups)))
	  (setq prefixes (cdr prefixes)))
	(setq prefix (car prefixes))
	(setq start (1- (length prefix)))
	(if (and (string-match "[^\\.]\\." (car groups) start)
		 (cdr groups)
		 (setq prefix
		       (concat "^" (substring (car groups) 0 (match-end 0))))
		 (string-match prefix (cadr groups)))
	    (progn
	      (push prefix prefixes)
	      (message "Descend hierarchy %s? ([y]nsq): "
		       (substring prefix 1 (1- (length prefix))))
	      (while (not (memq (setq ans (read-char-exclusive))
				'(?y ?\n ?\r ?n ?s ?q)))
		(ding)
		(message "Descend hierarchy %s? ([y]nsq): "
			 (substring prefix 1 (1- (length prefix)))))
	      (cond ((= ans ?n)
		     (while (and groups
				 (string-match prefix
					       (setq group (car groups))))
		       (push group gnus-killed-list)
		       (gnus-sethash group group gnus-killed-hashtb)
		       (setq groups (cdr groups)))
		     (setq starts (cdr starts)))
		    ((= ans ?s)
		     (while (and groups
				 (string-match prefix
					       (setq group (car groups))))
		       (gnus-sethash group group gnus-killed-hashtb)
		       (gnus-subscribe-alphabetically (car groups))
		       (setq groups (cdr groups)))
		     (setq starts (cdr starts)))
		    ((= ans ?q)
		     (while groups
		       (setq group (car groups))
		       (push group gnus-killed-list)
		       (gnus-sethash group group gnus-killed-hashtb)
		       (setq groups (cdr groups))))
		    (t nil)))
	  (message "Subscribe %s? ([n]yq)" (car groups))
	  (while (not (memq (setq ans (read-char-exclusive))
			    '(?y ?\n ?\r ?q ?n)))
	    (ding)
	    (message "Subscribe %s? ([n]yq)" (car groups)))
	  (setq group (car groups))
	  (cond ((= ans ?y)
		 (gnus-subscribe-alphabetically (car groups))
		 (gnus-sethash group group gnus-killed-hashtb))
		((= ans ?q)
		 (while groups
		   (setq group (car groups))
		   (push group gnus-killed-list)
		   (gnus-sethash group group gnus-killed-hashtb)
		   (setq groups (cdr groups))))
		(t
		 (push group gnus-killed-list)
		 (gnus-sethash group group gnus-killed-hashtb)))
	  (setq groups (cdr groups)))))))

(defun gnus-subscribe-randomly (newsgroup)
  "Subscribe new NEWSGROUP by making it the first newsgroup."
  (gnus-subscribe-newsgroup newsgroup))

(defun gnus-subscribe-alphabetically (newgroup)
  "Subscribe new NEWSGROUP and insert it in alphabetical order."
  (let ((groups (cdr gnus-newsrc-alist))
	before)
    (while (and (not before) groups)
      (if (string< newgroup (caar groups))
	  (setq before (caar groups))
	(setq groups (cdr groups))))
    (gnus-subscribe-newsgroup newgroup before)))

(defun gnus-subscribe-hierarchically (newgroup)
  "Subscribe new NEWSGROUP and insert it in hierarchical newsgroup order."
  ;; Basic ideas by mike-w@cs.aukuni.ac.nz (Mike Williams)
  (save-excursion
    (set-buffer (nnheader-find-file-noselect gnus-current-startup-file))
    (let ((groupkey newgroup)
	  before)
      (while (and (not before) groupkey)
	(goto-char (point-min))
	(let ((groupkey-re
	       (concat "^\\(" (regexp-quote groupkey) ".*\\)[!:]")))
	  (while (and (re-search-forward groupkey-re nil t)
		      (progn
			(setq before (match-string 1))
			(string< before newgroup)))))
	;; Remove tail of newsgroup name (eg. a.b.c -> a.b)
	(setq groupkey
	      (when (string-match "^\\(.*\\)\\.[^.]+$" groupkey)
		(substring groupkey (match-beginning 1) (match-end 1)))))
      (gnus-subscribe-newsgroup newgroup before))
    (kill-buffer (current-buffer))))

(defun gnus-subscribe-interactively (group)
  "Subscribe the new GROUP interactively.
It is inserted in hierarchical newsgroup order if subscribed.  If not,
it is killed."
  (if (gnus-y-or-n-p (format "Subscribe new newsgroup: %s " group))
      (gnus-subscribe-hierarchically group)
    (push group gnus-killed-list)))

(defun gnus-subscribe-zombies (group)
  "Make the new GROUP into a zombie group."
  (push group gnus-zombie-list))

(defun gnus-subscribe-killed (group)
  "Make the new GROUP a killed group."
  (push group gnus-killed-list))

(defun gnus-subscribe-newsgroup (newsgroup &optional next)
  "Subscribe new NEWSGROUP.
If NEXT is non-nil, it is inserted before NEXT.	 Otherwise it is made
the first newsgroup."
  (save-excursion
    (goto-char (point-min))
    ;; We subscribe the group by changing its level to `subscribed'.
    (gnus-group-change-level
     newsgroup gnus-level-default-subscribed
     gnus-level-killed (gnus-gethash (or next "dummy.group")
				     gnus-newsrc-hashtb))
    (gnus-message 5 "Subscribe newsgroup: %s" newsgroup)))

(defun gnus-read-active-file-p ()
  "Say whether the active file has been read from `gnus-select-method'."
  (memq gnus-select-method gnus-have-read-active-file))

;;; General various misc type functions.

;; Silence byte-compiler.
(defvar gnus-current-headers)
(defvar gnus-thread-indent-array)
(defvar gnus-newsgroup-name)
(defvar gnus-newsgroup-headers)
(defvar gnus-group-list-mode)
(defvar gnus-group-mark-positions)
(defvar gnus-newsgroup-data)
(defvar gnus-newsgroup-unreads)
(defvar nnoo-state-alist)
(defvar gnus-current-select-method)

(defun gnus-clear-system ()
  "Clear all variables and buffers."
  ;; Clear Gnus variables.
  (let ((variables gnus-variable-list))
    (while variables
      (set (car variables) nil)
      (setq variables (cdr variables))))
  ;; Clear other internal variables.
  (setq gnus-list-of-killed-groups nil
	gnus-have-read-active-file nil
	gnus-newsrc-alist nil
	gnus-newsrc-hashtb nil
	gnus-killed-list nil
	gnus-zombie-list nil
	gnus-killed-hashtb nil
	gnus-active-hashtb nil
	gnus-moderated-hashtb nil
	gnus-description-hashtb nil
	gnus-current-headers nil
	gnus-thread-indent-array nil
	gnus-newsgroup-headers nil
	gnus-newsgroup-name nil
	gnus-server-alist nil
	gnus-group-list-mode nil
	gnus-opened-servers nil
	gnus-group-mark-positions nil
	gnus-newsgroup-data nil
	gnus-newsgroup-unreads nil
	nnoo-state-alist nil
	gnus-current-select-method nil
	nnmail-split-history nil
	gnus-ephemeral-servers nil)
  (gnus-shutdown 'gnus)
  ;; Kill the startup file.
  (and gnus-current-startup-file
       (get-file-buffer gnus-current-startup-file)
       (kill-buffer (get-file-buffer gnus-current-startup-file)))
  ;; Clear the dribble buffer.
  (gnus-dribble-clear)
  ;; Kill global KILL file buffer.
  (when (get-file-buffer (gnus-newsgroup-kill-file nil))
    (kill-buffer (get-file-buffer (gnus-newsgroup-kill-file nil))))
  (gnus-kill-buffer nntp-server-buffer)
  ;; Kill Gnus buffers.
  (let ((buffers (gnus-buffers)))
    (when buffers
      (mapcar 'kill-buffer buffers)))
  ;; Remove Gnus frames.
  (gnus-kill-gnus-frames))

(defun gnus-no-server-1 (&optional arg slave)
  "Read network news.
If ARG is a positive number, Gnus will use that as the
startup level.	If ARG is nil, Gnus will be started at level 2.
If ARG is non-nil and not a positive number, Gnus will
prompt the user for the name of an NNTP server to use.
As opposed to `gnus', this command will not connect to the local server."
  (interactive "P")
  (let ((val (or arg (1- gnus-level-default-subscribed))))
    (gnus val t slave)
    (make-local-variable 'gnus-group-use-permanent-levels)
    (setq gnus-group-use-permanent-levels val)))

(defun gnus-1 (&optional arg dont-connect slave)
  "Read network news.
If ARG is non-nil and a positive number, Gnus will use that as the
startup level.	If ARG is non-nil and not a positive number, Gnus will
prompt the user for the name of an NNTP server to use."
  (interactive "P")

  (if (gnus-alive-p)
      (progn
	(switch-to-buffer gnus-group-buffer)
	(gnus-group-get-new-news
	 (and (numberp arg)
	      (> arg 0)
	      (max (car gnus-group-list-mode) arg))))

    (gnus-clear-system)
    (gnus-splash)
    (gnus-run-hooks 'gnus-before-startup-hook)
    (nnheader-init-server-buffer)
    (setq gnus-slave slave)
    (gnus-read-init-file)

    (when gnus-simple-splash
      (setq gnus-simple-splash nil)
      (cond
       ((featurep 'xemacs)
	(gnus-xmas-splash))
       ((and window-system
	     (= (frame-height) (1+ (window-height))))
	(gnus-x-splash))))

    (let ((level (and (numberp arg) (> arg 0) arg))
	  did-connect)
      (unwind-protect
	  (progn
	    (unless dont-connect
	      (setq did-connect
		    (gnus-start-news-server (and arg (not level))))))
	(if (and (not dont-connect)
		 (not did-connect))
	    (gnus-group-quit)
	  (gnus-run-hooks 'gnus-startup-hook)
	  ;; NNTP server is successfully open.

	  ;; Find the current startup file name.
	  (setq gnus-current-startup-file
		(gnus-make-newsrc-file gnus-startup-file))

	  ;; Read the dribble file.
	  (when (or gnus-slave gnus-use-dribble-file)
	    (gnus-dribble-read-file))

	  ;; Allow using GroupLens predictions.
	  (when gnus-use-grouplens
	    (bbb-login)
	    (add-hook 'gnus-summary-mode-hook 'gnus-grouplens-mode))

	  ;; Do the actual startup.
	  (gnus-setup-news nil level dont-connect)
	  (gnus-run-hooks 'gnus-setup-news-hook)
	  (gnus-start-draft-setup)
	  ;; Generate the group buffer.
	  (gnus-group-list-groups level)
	  (gnus-group-first-unread-group)
	  (gnus-configure-windows 'group)
	  (gnus-group-set-mode-line)
	  (gnus-run-hooks 'gnus-started-hook))))))

(defun gnus-start-draft-setup ()
  "Make sure the draft group exists."
  (gnus-request-create-group "drafts" '(nndraft ""))
  (unless (gnus-gethash "nndraft:drafts" gnus-newsrc-hashtb)
    (let ((gnus-level-default-subscribed 1))
      (gnus-subscribe-group "nndraft:drafts" nil '(nndraft "")))
    (gnus-group-set-parameter
     "nndraft:drafts" 'gnus-dummy '((gnus-draft-mode)))))

;;;###autoload
(defun gnus-unload ()
  "Unload all Gnus features.
\(For some value of `all' or `Gnus'.)  Currently, features whose names
have prefixes `gnus-', `nn', `mm-' or `rfc' are unloaded.  Use
cautiously -- unloading may cause trouble."
  (interactive)
  (dolist (feature features)
    (if (string-match "^\\(gnus-\\|nn\\|mm-\\|rfc\\)" (symbol-name feature))
	(unload-feature feature 'force))))


;;;
;;; Dribble file
;;;

(defvar gnus-dribble-ignore nil)
(defvar gnus-dribble-eval-file nil)

(defun gnus-dribble-file-name ()
  "Return the dribble file for the current .newsrc."
  (concat
   (if gnus-dribble-directory
       (concat (file-name-as-directory gnus-dribble-directory)
	       (file-name-nondirectory gnus-current-startup-file))
     gnus-current-startup-file)
   "-dribble"))

(defun gnus-dribble-enter (string)
  "Enter STRING into the dribble buffer."
  (when (and (not gnus-dribble-ignore)
	     gnus-dribble-buffer
	     (buffer-name gnus-dribble-buffer))
    (let ((obuf (current-buffer)))
      (set-buffer gnus-dribble-buffer)
      (goto-char (point-max))
      (insert string "\n")
      (set-window-point (get-buffer-window (current-buffer)) (point-max))
      (bury-buffer gnus-dribble-buffer)
      (save-excursion
	(set-buffer gnus-group-buffer)
	(gnus-group-set-mode-line))
      (set-buffer obuf))))

(defun gnus-dribble-touch ()
  "Touch the dribble buffer."
  (gnus-dribble-enter ""))

(defun gnus-dribble-read-file ()
  "Read the dribble file from disk."
  (let ((dribble-file (gnus-dribble-file-name)))
    (save-excursion
      (set-buffer (setq gnus-dribble-buffer
			(gnus-get-buffer-create
			 (file-name-nondirectory dribble-file))))
      (erase-buffer)
      (setq buffer-file-name dribble-file)
      (auto-save-mode t)
      (buffer-disable-undo)
      (bury-buffer (current-buffer))
      (set-buffer-modified-p nil)
      (let ((auto (make-auto-save-file-name))
	    (gnus-dribble-ignore t)
	    modes)
	(when (or (file-exists-p auto) (file-exists-p dribble-file))
	  ;; Load whichever file is newest -- the auto save file
	  ;; or the "real" file.
	  (if (file-newer-than-file-p auto dribble-file)
	      (nnheader-insert-file-contents auto)
	    (nnheader-insert-file-contents dribble-file))
	  (unless (zerop (buffer-size))
	    (set-buffer-modified-p t))
	  ;; Set the file modes to reflect the .newsrc file modes.
	  (save-buffer)
	  (when (and (file-exists-p gnus-current-startup-file)
		     (file-exists-p dribble-file)
		     (setq modes (file-modes gnus-current-startup-file)))
	    (set-file-modes dribble-file modes))
	  ;; Possibly eval the file later.
	  (when (or gnus-always-read-dribble-file
		    (gnus-y-or-n-p
		     "Gnus auto-save file exists.  Do you want to read it? "))
	    (setq gnus-dribble-eval-file t)))))))

(defun gnus-dribble-eval-file ()
  (when gnus-dribble-eval-file
    (setq gnus-dribble-eval-file nil)
    (save-excursion
      (let ((gnus-dribble-ignore t))
	(set-buffer gnus-dribble-buffer)
	(eval-buffer (current-buffer))))))

(defun gnus-dribble-delete-file ()
  (when (file-exists-p (gnus-dribble-file-name))
    (delete-file (gnus-dribble-file-name)))
  (when gnus-dribble-buffer
    (save-excursion
      (set-buffer gnus-dribble-buffer)
      (let ((auto (make-auto-save-file-name)))
	(when (file-exists-p auto)
	  (delete-file auto))
	(erase-buffer)
	(set-buffer-modified-p nil)))))

(defun gnus-dribble-save ()
  (when (and gnus-dribble-buffer
	     (buffer-name gnus-dribble-buffer))
    (save-excursion
      (set-buffer gnus-dribble-buffer)
      (save-buffer))))

(defun gnus-dribble-clear ()
  (when (gnus-buffer-exists-p gnus-dribble-buffer)
    (save-excursion
      (set-buffer gnus-dribble-buffer)
      (erase-buffer)
      (set-buffer-modified-p nil)
      (setq buffer-saved-size (buffer-size)))))


;;;
;;; Active & Newsrc File Handling
;;;

(defun gnus-setup-news (&optional rawfile level dont-connect)
  "Setup news information.
If RAWFILE is non-nil, the .newsrc file will also be read.
If LEVEL is non-nil, the news will be set up at level LEVEL."
  (require 'nnmail)
  (let ((init (not (and gnus-newsrc-alist gnus-active-hashtb (not rawfile))))
	;; Binding this variable will inhibit multiple fetchings
	;; of the same mail source.
	(nnmail-fetched-sources (list t)))

    (when init
      ;; Clear some variables to re-initialize news information.
      (setq gnus-newsrc-alist nil
	    gnus-active-hashtb nil)
      ;; Read the newsrc file and create `gnus-newsrc-hashtb'.
      (gnus-read-newsrc-file rawfile))

    ;; Make sure the archive server is available to all and sundry.
    (when gnus-message-archive-method
      (setq gnus-server-alist (delq (assoc "archive" gnus-server-alist)
				    gnus-server-alist))
      (push (cons "archive" gnus-message-archive-method)
	    gnus-server-alist))

    ;; If we don't read the complete active file, we fill in the
    ;; hashtb here.
    (when (or (null gnus-read-active-file)
	      (eq gnus-read-active-file 'some))
      (gnus-update-active-hashtb-from-killed))

    ;; Read the active file and create `gnus-active-hashtb'.
    ;; If `gnus-read-active-file' is nil, then we just create an empty
    ;; hash table.  The partial filling out of the hash table will be
    ;; done in `gnus-get-unread-articles'.
    (and gnus-read-active-file
	 (not level)
	 (gnus-read-active-file nil dont-connect))

    (unless gnus-active-hashtb
      (setq gnus-active-hashtb (gnus-make-hashtable 4096)))

    ;; Initialize the cache.
    (when gnus-use-cache
      (gnus-cache-open))

    ;; Possibly eval the dribble file.
    (and init
	 (or gnus-use-dribble-file gnus-slave)
	 (gnus-dribble-eval-file))

    ;; Slave Gnusii should then clear the dribble buffer.
    (when (and init gnus-slave)
      (gnus-dribble-clear))

    (gnus-update-format-specifications)

    ;; See whether we need to read the description file.
    (when (and (boundp 'gnus-group-line-format)
	       (let ((case-fold-search nil))
		 (string-match "%[-,0-9]*D" gnus-group-line-format))
	       (not gnus-description-hashtb)
	       (not dont-connect)
	       gnus-read-active-file)
      (gnus-read-all-descriptions-files))

    ;; Find new newsgroups and treat them.
    (when (and init gnus-check-new-newsgroups (not level)
	       (gnus-check-server gnus-select-method)
	       (not gnus-slave)
	       gnus-plugged)
      (gnus-find-new-newsgroups))

    ;; We might read in new NoCeM messages here.
    (when (and gnus-use-nocem
	       (not level)
	       (not dont-connect))
      (gnus-nocem-scan-groups))

    ;; Read any slave files.
    (gnus-master-read-slave-newsrc)

    ;; Find the number of unread articles in each non-dead group.
    (let ((gnus-read-active-file (and (not level) gnus-read-active-file)))
      (gnus-get-unread-articles level))

    (when (and init gnus-check-bogus-newsgroups
	       gnus-read-active-file (not level)
	       (gnus-server-opened gnus-select-method))
      (gnus-check-bogus-newsgroups))))

(defun gnus-find-new-newsgroups (&optional arg)
  "Search for new newsgroups and add them.
Each new newsgroup will be treated with `gnus-subscribe-newsgroup-method'.
The `-n' option line from .newsrc is respected.

With 1 C-u, use the `ask-server' method to query the server for new
groups.
With 2 C-u's, use most complete method possible to query the server
for new groups, and subscribe the new groups as zombies."
  (interactive "p")
  (let* ((gnus-subscribe-newsgroup-method
	  gnus-subscribe-newsgroup-method)
	 (check (cond
		 ((or (and (= (or arg 1) 4)
			   (not (listp gnus-check-new-newsgroups)))
		      (null gnus-read-active-file)
		      (eq gnus-read-active-file 'some))
		  'ask-server)
		 ((= (or arg 1) 16)
		  (setq gnus-subscribe-newsgroup-method
			'gnus-subscribe-zombies)
		  t)
		 (t gnus-check-new-newsgroups))))
    (unless (gnus-check-first-time-used)
      (if (or (consp check)
	      (eq check 'ask-server))
	  ;; Ask the server for new groups.
	  (gnus-ask-server-for-new-groups)
	;; Go through the active hashtb and look for new groups.
	(let ((groups 0)
	      group new-newsgroups)
	  (gnus-message 5 "Looking for new newsgroups...")
	  (unless gnus-have-read-active-file
	    (gnus-read-active-file))
	  (setq gnus-newsrc-last-checked-date (message-make-date))
	  (unless gnus-killed-hashtb
	    (gnus-make-hashtable-from-killed))
	  ;; Go though every newsgroup in `gnus-active-hashtb' and compare
	  ;; with `gnus-newsrc-hashtb' and `gnus-killed-hashtb'.
	  (mapatoms
	   (lambda (sym)
	     (if (or (null (setq group (symbol-name sym)))
		     (not (boundp sym))
		     (null (symbol-value sym))
		     (gnus-gethash group gnus-killed-hashtb)
		     (gnus-gethash group gnus-newsrc-hashtb))
		 ()
	       (let ((do-sub (gnus-matches-options-n group)))
		 (cond
		  ((eq do-sub 'subscribe)
		   (setq groups (1+ groups))
		   (gnus-sethash group group gnus-killed-hashtb)
		   (funcall gnus-subscribe-options-newsgroup-method group))
		  ((eq do-sub 'ignore)
		   nil)
		  (t
		   (setq groups (1+ groups))
		   (gnus-sethash group group gnus-killed-hashtb)
		   (if gnus-subscribe-hierarchical-interactive
		       (push group new-newsgroups)
		     (funcall gnus-subscribe-newsgroup-method group)))))))
	   gnus-active-hashtb)
	  (when new-newsgroups
	    (gnus-subscribe-hierarchical-interactive new-newsgroups))
	  (if (> groups 0)
	      (gnus-message 5 "%d new newsgroup%s arrived."
			    groups (if (> groups 1) "s have" " has"))
	    (gnus-message 5 "No new newsgroups.")))))))

(defun gnus-matches-options-n (group)
  ;; Returns `subscribe' if the group is to be unconditionally
  ;; subscribed, `ignore' if it is to be ignored, and nil if there is
  ;; no match for the group.

  ;; First we check the two user variables.
  (cond
   ((and gnus-options-subscribe
	 (string-match gnus-options-subscribe group))
    'subscribe)
   ((and gnus-auto-subscribed-groups
	 (string-match gnus-auto-subscribed-groups group))
    'subscribe)
   ((and gnus-options-not-subscribe
	 (string-match gnus-options-not-subscribe group))
    'ignore)
   ;; Then we go through the list that was retrieved from the .newsrc
   ;; file.  This list has elements on the form
   ;; `(REGEXP . {ignore,subscribe})'.  The first match found (the list
   ;; is in the reverse order of the options line) is returned.
   (t
    (let ((regs gnus-newsrc-options-n))
      (while (and regs
		  (not (string-match (caar regs) group)))
	(setq regs (cdr regs)))
      (and regs (cdar regs))))))

(defun gnus-ask-server-for-new-groups ()
  (let* ((new-date (message-make-date))
	 (date (or gnus-newsrc-last-checked-date new-date))
	 (methods (cons gnus-select-method
			(nconc
			 (when (gnus-archive-server-wanted-p)
			   (list "archive"))
			 (append
			  (and (consp gnus-check-new-newsgroups)
			       gnus-check-new-newsgroups)
			  gnus-secondary-select-methods))))
	 (groups 0)
	 group new-newsgroups got-new method hashtb
	 gnus-override-subscribe-method)
    (unless gnus-killed-hashtb
      (gnus-make-hashtable-from-killed))
    ;; Go through both primary and secondary select methods and
    ;; request new newsgroups.
    (while (setq method (gnus-server-get-method nil (pop methods)))
      (setq new-newsgroups nil
	    gnus-override-subscribe-method method)
      (when (and (gnus-check-server method)
		 (gnus-request-newgroups date method))
	(save-excursion
	  (setq got-new t
		hashtb (gnus-make-hashtable 100))
	  (set-buffer nntp-server-buffer)
	  ;; Enter all the new groups into a hashtable.
	  (gnus-active-to-gnus-format method hashtb 'ignore))
	;; Now all new groups from `method' are in `hashtb'.
	(mapatoms
	 (lambda (group-sym)
	   (if (or (null (setq group (symbol-name group-sym)))
		   (not (boundp group-sym))
		   (null (symbol-value group-sym))
		   (gnus-gethash group gnus-newsrc-hashtb)
		   (member group gnus-zombie-list)
		   (member group gnus-killed-list))
	       ;; The group is already known.
	       ()
	     ;; Make this group active.
	     (when (symbol-value group-sym)
	       (gnus-set-active group (symbol-value group-sym)))
	     ;; Check whether we want it or not.
	     (let ((do-sub (gnus-matches-options-n group)))
	       (cond
		((eq do-sub 'subscribe)
		 (incf groups)
		 (gnus-sethash group group gnus-killed-hashtb)
		 (funcall gnus-subscribe-options-newsgroup-method group))
		((eq do-sub 'ignore)
		 nil)
		(t
		 (incf groups)
		 (gnus-sethash group group gnus-killed-hashtb)
		 (if gnus-subscribe-hierarchical-interactive
		     (push group new-newsgroups)
		   (funcall gnus-subscribe-newsgroup-method group)))))))
	 hashtb))
      (when new-newsgroups
	(gnus-subscribe-hierarchical-interactive new-newsgroups)))
    (if (> groups 0)
	(gnus-message 5 "%d new newsgroup%s arrived"
		      groups (if (> groups 1) "s have" " has"))
      (gnus-message 5 "No new newsgroups"))
    (when got-new
      (setq gnus-newsrc-last-checked-date new-date))
    got-new))

(defun gnus-check-first-time-used ()
  (catch 'ended
    ;; First check if any of the following files exist.  If they do,
    ;; it's not the first time the user has used Gnus.
    (dolist (file (list gnus-current-startup-file
			(concat gnus-current-startup-file ".el")
			(concat gnus-current-startup-file ".eld")
			gnus-startup-file
			(concat gnus-startup-file ".el")
			(concat gnus-startup-file ".eld")))
      (when (file-exists-p file)
	(throw 'ended nil)))
    (gnus-message 6 "First time user; subscribing you to default groups")
    (unless (gnus-read-active-file-p)
      (let ((gnus-read-active-file t))
	(gnus-read-active-file)))
    (setq gnus-newsrc-last-checked-date (message-make-date))
    ;; Subscribe to the default newsgroups.
    (let ((groups (or gnus-default-subscribed-newsgroups
		      gnus-backup-default-subscribed-newsgroups))
	  group)
      (when (eq groups t)
	;; If t, we subscribe (or not) all groups as if they were new.
	(mapatoms
	 (lambda (sym)
	   (when (setq group (symbol-name sym))
	     (let ((do-sub (gnus-matches-options-n group)))
	       (cond
		((eq do-sub 'subscribe)
		 (gnus-sethash group group gnus-killed-hashtb)
		 (funcall gnus-subscribe-options-newsgroup-method group))
		((eq do-sub 'ignore)
		 nil)
		(t
		 (push group gnus-killed-list))))))
	 gnus-active-hashtb)
	(dolist (group groups)
	  ;; Only subscribe the default groups that are activated.
	  (when (gnus-active group)
	    (gnus-group-change-level
	     group gnus-level-default-subscribed gnus-level-killed)))
	(save-excursion
	  (set-buffer gnus-group-buffer)
	  (gnus-group-make-help-group))
	(when gnus-novice-user
	  (gnus-message 7 "`A k' to list killed groups"))))))

(defun gnus-subscribe-group (group &optional previous method)
  "Subcribe GROUP and put it after PREVIOUS."
  (gnus-group-change-level
   (if method
       (list t group gnus-level-default-subscribed nil nil method)
     group)
   gnus-level-default-subscribed gnus-level-killed previous t)
  t)

;; `gnus-group-change-level' is the fundamental function for changing
;; subscription levels of newsgroups.  This might mean just changing
;; from level 1 to 2, which is pretty trivial, from 2 to 6 or back
;; again, which subscribes/unsubscribes a group, which is equally
;; trivial.  Changing from 1-7 to 8-9 means that you kill a group, and
;; from 8-9 to 1-7 means that you remove the group from the list of
;; killed (or zombie) groups and add them to the (kinda) subscribed
;; groups.  And last but not least, moving from 8 to 9 and 9 to 8,
;; which is trivial.
;; ENTRY can either be a string (newsgroup name) or a list (if
;; FROMKILLED is t, it's a list on the format (NUM INFO-LIST),
;; otherwise it's a list in the format of the `gnus-newsrc-hashtb'
;; entries.
;; LEVEL is the new level of the group, OLDLEVEL is the old level and
;; PREVIOUS is the group (in hashtb entry format) to insert this group
;; after.
(defun gnus-group-change-level (entry level &optional oldlevel
				      previous fromkilled)
  (let (group info active num)
    ;; Glean what info we can from the arguments
    (if (consp entry)
	(if fromkilled (setq group (nth 1 entry))
	  (setq group (car (nth 2 entry))))
      (setq group entry))
    (when (and (stringp entry)
	       oldlevel
	       (< oldlevel gnus-level-zombie))
      (setq entry (gnus-gethash entry gnus-newsrc-hashtb)))
    (if (and (not oldlevel)
	     (consp entry))
	(setq oldlevel (gnus-info-level (nth 2 entry)))
      (setq oldlevel (or oldlevel gnus-level-killed)))
    (when (stringp previous)
      (setq previous (gnus-gethash previous gnus-newsrc-hashtb)))

    (if (and (>= oldlevel gnus-level-zombie)
	     (gnus-gethash group gnus-newsrc-hashtb))
	;; We are trying to subscribe a group that is already
	;; subscribed.
	()				; Do nothing.

      (unless (gnus-ephemeral-group-p group)
	(gnus-dribble-enter
	 (format "(gnus-group-change-level %S %S %S %S %S)"
		 group level oldlevel (car (nth 2 previous)) fromkilled)))

      ;; Then we remove the newgroup from any old structures, if needed.
      ;; If the group was killed, we remove it from the killed or zombie
      ;; list.  If not, and it is in fact going to be killed, we remove
      ;; it from the newsrc hash table and assoc.
      (cond
       ((>= oldlevel gnus-level-zombie)
	(if (= oldlevel gnus-level-zombie)
	    (setq gnus-zombie-list (delete group gnus-zombie-list))
	  (setq gnus-killed-list (delete group gnus-killed-list))))
       (t
	(when (and (>= level gnus-level-zombie)
		   entry)
	  (gnus-sethash (car (nth 2 entry)) nil gnus-newsrc-hashtb)
	  (when (nth 3 entry)
	    (setcdr (gnus-gethash (car (nth 3 entry))
				  gnus-newsrc-hashtb)
		    (cdr entry)))
	  (setcdr (cdr entry) (cdddr entry)))))

      ;; Finally we enter (if needed) the list where it is supposed to
      ;; go, and change the subscription level.  If it is to be killed,
      ;; we enter it into the killed or zombie list.
      (cond
       ((>= level gnus-level-zombie)
	;; Remove from the hash table.
	(gnus-sethash group nil gnus-newsrc-hashtb)
	;; We do not enter foreign groups into the list of dead
	;; groups.
	(unless (gnus-group-foreign-p group)
	  (if (= level gnus-level-zombie)
	      (push group gnus-zombie-list)
	    (push group gnus-killed-list))))
       (t
	;; If the list is to be entered into the newsrc assoc, and
	;; it was killed, we have to create an entry in the newsrc
	;; hashtb format and fix the pointers in the newsrc assoc.
	(if (< oldlevel gnus-level-zombie)
	    ;; It was alive, and it is going to stay alive, so we
	    ;; just change the level and don't change any pointers or
	    ;; hash table entries.
	    (setcar (cdaddr entry) level)
	  (if (listp entry)
	      (setq info (cdr entry)
		    num (car entry))
	    (setq active (gnus-active group))
	    (setq num
		  (if active (- (1+ (cdr active)) (car active)) t))
	    ;; Shorten the select method if possible, if we need to
	    ;; store it at all (native groups).
	    (let ((method (gnus-method-simplify
			   (or gnus-override-subscribe-method
			       (gnus-group-method group)))))
	      (if method
		  (setq info (list group level nil nil method))
		(setq info (list group level nil)))))
	  (unless previous
	    (setq previous
		  (let ((p gnus-newsrc-alist))
		    (while (cddr p)
		      (setq p (cdr p)))
		    p)))
	  (setq entry (cons info (cddr previous)))
	  (if (cdr previous)
	      (progn
		(setcdr (cdr previous) entry)
		(gnus-sethash group (cons num (cdr previous))
			      gnus-newsrc-hashtb))
	    (setcdr previous entry)
	    (gnus-sethash group (cons num previous)
			  gnus-newsrc-hashtb))
	  (when (cdr entry)
	    (setcdr (gnus-gethash (caadr entry) gnus-newsrc-hashtb) entry))
	  (gnus-dribble-enter
	   (format
	    "(gnus-group-set-info '%S)" info)))))
      (when gnus-group-change-level-function
	(funcall gnus-group-change-level-function
		 group level oldlevel previous)))))

(defun gnus-kill-newsgroup (newsgroup)
  "Obsolete function.  Kills a newsgroup."
  (gnus-group-change-level
   (gnus-gethash newsgroup gnus-newsrc-hashtb) gnus-level-killed))

(defun gnus-check-bogus-newsgroups (&optional confirm)
  "Remove bogus newsgroups.
If CONFIRM is non-nil, the user has to confirm the deletion of every
newsgroup."
  (let ((newsrc (cdr gnus-newsrc-alist))
	bogus group entry info)
    (gnus-message 5 "Checking bogus newsgroups...")
    (unless (gnus-read-active-file-p)
      (gnus-read-active-file t))
    (when (gnus-read-active-file-p)
      ;; Find all bogus newsgroup that are subscribed.
      (while newsrc
	(setq info (pop newsrc)
	      group (gnus-info-group info))
	(unless (or (gnus-active group)	; Active
		    (gnus-info-method info)) ; Foreign
	  ;; Found a bogus newsgroup.
	  (push group bogus)))
      (if confirm
	  (map-y-or-n-p
	   "Remove bogus group %s? "
	   (lambda (group)
	     ;; Remove all bogus subscribed groups by first killing them, and
	     ;; then removing them from the list of killed groups.
	     (when (setq entry (gnus-gethash group gnus-newsrc-hashtb))
	       (gnus-group-change-level entry gnus-level-killed)
	       (setq gnus-killed-list (delete group gnus-killed-list))))
	   bogus '("group" "groups" "remove"))
	(while (setq group (pop bogus))
	  ;; Remove all bogus subscribed groups by first killing them, and
	  ;; then removing them from the list of killed groups.
	  (when (setq entry (gnus-gethash group gnus-newsrc-hashtb))
	    (gnus-group-change-level entry gnus-level-killed)
	    (setq gnus-killed-list (delete group gnus-killed-list)))))
      ;; Then we remove all bogus groups from the list of killed and
      ;; zombie groups.  They are removed without confirmation.
      (let ((dead-lists '(gnus-killed-list gnus-zombie-list))
	    killed)
	(while dead-lists
	  (setq killed (symbol-value (car dead-lists)))
	  (while killed
	    (unless (gnus-active (setq group (pop killed)))
	      ;; The group is bogus.
	      ;; !!!Slow as hell.
	      (set (car dead-lists)
		   (delete group (symbol-value (car dead-lists))))))
	  (setq dead-lists (cdr dead-lists))))
      (gnus-run-hooks 'gnus-check-bogus-groups-hook)
      (gnus-message 5 "Checking bogus newsgroups...done"))))

(defun gnus-check-duplicate-killed-groups ()
  "Remove duplicates from the list of killed groups."
  (interactive)
  (let ((killed gnus-killed-list))
    (while killed
      (gnus-message 9 "%d" (length killed))
      (setcdr killed (delete (car killed) (cdr killed)))
      (setq killed (cdr killed)))))

;; We want to inline a function from gnus-cache, so we cheat here:
(eval-when-compile
  (defvar gnus-cache-active-hashtb)
  (defun gnus-cache-possibly-alter-active (group active)
    "Alter the ACTIVE info for GROUP to reflect the articles in the cache."
    (when gnus-cache-active-hashtb
      (let ((cache-active (gnus-gethash group gnus-cache-active-hashtb)))
	(when cache-active
	  (when (< (car cache-active) (car active))
	    (setcar active (car cache-active)))
	  (when (> (cdr cache-active) (cdr active))
	    (setcdr active (cdr cache-active))))))))

(defun gnus-activate-group (group &optional scan dont-check method)
  ;; Check whether a group has been activated or not.
  ;; If SCAN, request a scan of that group as well.
  (let ((method (or method (inline (gnus-find-method-for-group group))))
	active)
    (and (inline (gnus-check-server method))
	 ;; We escape all bugs and quit here to make it possible to
	 ;; continue if a group is so out-there that it reports bugs
	 ;; and stuff.
	 (progn
	   (and scan
		(gnus-check-backend-function 'request-scan (car method))
		(gnus-request-scan group method))
	   t)
	 (condition-case ()
	     (inline (gnus-request-group group dont-check method))
	   ;;(error nil)
	   (quit
	    (message "Quit activating %s" group)
	    nil))
	 (setq active (gnus-parse-active))
	 ;; If there are no articles in the group, the GROUP
	 ;; command may have responded with the `(0 . 0)'.  We
	 ;; ignore this if we already have an active entry
	 ;; for the group.
	 (if (and (zerop (car active))
		  (zerop (cdr active))
		  (gnus-active group))
	     (gnus-active group)
	   (gnus-set-active group active)
	   ;; Return the new active info.
	   active))))

(defun gnus-get-unread-articles-in-group (info active &optional update)
  (when active
    ;; Allow the backend to update the info in the group.
    (when (and update
	       (gnus-request-update-info
		info (inline (gnus-find-method-for-group
			      (gnus-info-group info)))))
      (gnus-activate-group (gnus-info-group info) nil t))

    (let* ((range (gnus-info-read info))
	   (num 0))
      ;; If a cache is present, we may have to alter the active info.
      (when (and gnus-use-cache info)
	(inline (gnus-cache-possibly-alter-active
		 (gnus-info-group info) active)))
      ;; Modify the list of read articles according to what articles
      ;; are available; then tally the unread articles and add the
      ;; number to the group hash table entry.
      (cond
       ((zerop (cdr active))
	(setq num 0))
       ((not range)
	(setq num (- (1+ (cdr active)) (car active))))
       ((not (listp (cdr range)))
	;; Fix a single (num . num) range according to the
	;; active hash table.
	;; Fix by Carsten Bormann <cabo@Informatik.Uni-Bremen.DE>.
	(and (< (cdr range) (car active)) (setcdr range (1- (car active))))
	(and (> (cdr range) (cdr active)) (setcdr range (cdr active)))
	;; Compute number of unread articles.
	(setq num (max 0 (- (cdr active) (- (1+ (cdr range)) (car range))))))
       (t
	;; The read list is a list of ranges.  Fix them according to
	;; the active hash table.
	;; First peel off any elements that are below the lower
	;; active limit.
	(while (and (cdr range)
		    (>= (car active)
			(or (and (atom (cadr range)) (cadr range))
			    (caadr range))))
	  (if (numberp (car range))
	      (setcar range
		      (cons (car range)
			    (or (and (numberp (cadr range))
				     (cadr range))
				(cdadr range))))
	    (setcdr (car range)
		    (or (and (numberp (nth 1 range)) (nth 1 range))
			(cdadr range))))
	  (setcdr range (cddr range)))
	;; Adjust the first element to be the same as the lower limit.
	(when (and (not (atom (car range)))
		   (< (cdar range) (car active)))
	  (setcdr (car range) (1- (car active))))
	;; Then we want to peel off any elements that are higher
	;; than the upper active limit.
	(let ((srange range))
	  ;; Go past all valid elements.
	  (while (and (cdr srange)
		      (<= (or (and (atom (cadr srange))
				   (cadr srange))
			      (caadr srange))
			  (cdr active)))
	    (setq srange (cdr srange)))
	  (when (cdr srange)
	    ;; Nuke all remaining invalid elements.
	    (setcdr srange nil))

	  ;; Adjust the final element.
	  (when (and (not (atom (car srange)))
		     (> (cdar srange) (cdr active)))
	    (setcdr (car srange) (cdr active))))
	;; Compute the number of unread articles.
	(while range
	  (setq num (+ num (- (1+ (or (and (atom (car range)) (car range))
				      (cdar range)))
			      (or (and (atom (car range)) (car range))
				  (caar range)))))
	  (setq range (cdr range)))
	(setq num (max 0 (- (cdr active) num)))))
      ;; Set the number of unread articles.
      (when info
	(setcar (gnus-gethash (gnus-info-group info) gnus-newsrc-hashtb) num))
      num)))

;; Go though `gnus-newsrc-alist' and compare with `gnus-active-hashtb'
;; and compute how many unread articles there are in each group.
(defun gnus-get-unread-articles (&optional level)
  (let* ((newsrc (cdr gnus-newsrc-alist))
	 (level (or level gnus-activate-level (1+ gnus-level-subscribed)))
	 (foreign-level
	  (min
	   (cond ((and gnus-activate-foreign-newsgroups
		       (not (numberp gnus-activate-foreign-newsgroups)))
		  (1+ gnus-level-subscribed))
		 ((numberp gnus-activate-foreign-newsgroups)
		  gnus-activate-foreign-newsgroups)
		 (t 0))
	   level))
	 scanned-methods info group active method retrievegroups)
    (gnus-message 5 "Checking new news...")

    (while newsrc
      (setq active (gnus-active (setq group (gnus-info-group
					     (setq info (pop newsrc))))))

      ;; Check newsgroups.  If the user doesn't want to check them, or
      ;; they can't be checked (for instance, if the news server can't
      ;; be reached) we just set the number of unread articles in this
      ;; newsgroup to t.  This means that Gnus thinks that there are
      ;; unread articles, but it has no idea how many.

      ;; To be more explicit:
      ;; >0 for an active group with messages
      ;; 0 for an active group with no unread messages
      ;; nil for non-foreign groups that the user has requested not be checked
      ;; t for unchecked foreign groups or bogus groups, or groups that can't
      ;;   be checked, for one reason or other.
      (if (and (setq method (gnus-info-method info))
	       (not (inline
		      (gnus-server-equal
		       gnus-select-method
		       (setq method (gnus-server-get-method nil method)))))
	       (not (gnus-secondary-method-p method)))
	  ;; These groups are foreign.  Check the level.
	  (when (and (<= (gnus-info-level info) foreign-level)
                     (setq active (gnus-activate-group group 'scan)))
	    ;; Let the Gnus agent save the active file.
	    (when (and gnus-agent gnus-plugged active)
	      (gnus-agent-save-group-info
	       method (gnus-group-real-name group) active))
	    (unless (inline (gnus-virtual-group-p group))
	      (inline (gnus-close-group group)))
	    (when (fboundp (intern (concat (symbol-name (car method))
					   "-request-update-info")))
	      (inline (gnus-request-update-info info method))))
	;; These groups are native or secondary.
	(cond
	 ;; We don't want these groups.
	 ((> (gnus-info-level info) level)
	  (setq active 'ignore))
	 ;; Activate groups.
	 ((not gnus-read-active-file)
	  (if (gnus-check-backend-function 'retrieve-groups group)
	      ;; if server support gnus-retrieve-groups we push
	      ;; the group onto retrievegroups for later checking
	      (if (assoc method retrievegroups)
		  (setcdr (assoc method retrievegroups)
			  (cons group (cdr (assoc method retrievegroups))))
		(push (list method group) retrievegroups))
	    ;; hack: `nnmail-get-new-mail' changes the mail-source depending
	    ;; on the group, so we must perform a scan for every group
	    ;; if the users has any directory mail sources.
	    ;; hack: if `nnmail-scan-directory-mail-source-once' is non-nil,
	    ;; for it scan all spool files even when the groups are
	    ;; not required.
	    (if (and
		 (or nnmail-scan-directory-mail-source-once
		     (null (assq 'directory
				 (or mail-sources
				     (if (listp nnmail-spool-file)
					 nnmail-spool-file
				       (list nnmail-spool-file))))))
		 (member method scanned-methods))
		(setq active (gnus-activate-group group))
	      (setq active (gnus-activate-group group 'scan))
	      (push method scanned-methods))
            (when active
              (gnus-close-group group))))))

      ;; Get the number of unread articles in the group.
      (cond
       ((eq active 'ignore)
	;; Don't do anything.
	)
       (active
	(inline (gnus-get-unread-articles-in-group info active t)))
       (t
	;; The group couldn't be reached, so we nix out the number of
	;; unread articles and stuff.
	(gnus-set-active group nil)
	(let ((tmp (gnus-gethash group gnus-newsrc-hashtb)))
	  (if tmp (setcar tmp t))))))

    ;; iterate through groups on methods which support gnus-retrieve-groups
    ;; and fetch a partial active file and use it to find new news.
    (while retrievegroups
      (let* ((mg (pop retrievegroups))
	     (method (or (car mg) gnus-select-method))
	     (groups (cdr mg)))
	(when (gnus-check-server method)
          ;; Request that the backend scan its incoming messages.
          (when (gnus-check-backend-function 'request-scan (car method))
            (gnus-request-scan nil method))
          (gnus-read-active-file-2 (mapcar (lambda (group)
                                             (gnus-group-real-name group))
                                           groups) method)
          (dolist (group groups)
            (cond
             ((setq active (gnus-active (gnus-info-group
                                         (setq info (gnus-get-info group)))))
              (inline (gnus-get-unread-articles-in-group info active t)))
             (t
              ;; The group couldn't be reached, so we nix out the number of
              ;; unread articles and stuff.
              (gnus-set-active group nil)
              (setcar (gnus-gethash group gnus-newsrc-hashtb) t)))))))

    (gnus-message 5 "Checking new news...done")))

;; Create a hash table out of the newsrc alist.  The `car's of the
;; alist elements are used as keys.
(defun gnus-make-hashtable-from-newsrc-alist ()
  (let ((alist gnus-newsrc-alist)
	(ohashtb gnus-newsrc-hashtb)
	prev)
    (setq gnus-newsrc-hashtb (gnus-make-hashtable (length alist)))
    (setq alist
	  (setq prev (setq gnus-newsrc-alist
			   (if (equal (caar gnus-newsrc-alist)
				      "dummy.group")
			       gnus-newsrc-alist
			     (cons (list "dummy.group" 0 nil) alist)))))
    (while alist
      (gnus-sethash
       (caar alist)
       ;; Preserve number of unread articles in groups.
       (cons (and ohashtb (car (gnus-gethash (caar alist) ohashtb)))
	     prev)
       gnus-newsrc-hashtb)
      (setq prev alist
	    alist (cdr alist)))))

(defun gnus-make-hashtable-from-killed ()
  "Create a hash table from the killed and zombie lists."
  (let ((lists '(gnus-killed-list gnus-zombie-list))
	list)
    (setq gnus-killed-hashtb
	  (gnus-make-hashtable
	   (+ (length gnus-killed-list) (length gnus-zombie-list))))
    (while lists
      (setq list (symbol-value (pop lists)))
      (while list
	(gnus-sethash (car list) (pop list) gnus-killed-hashtb)))))

(defun gnus-parse-active ()
  "Parse active info in the nntp server buffer."
  (save-excursion
    (set-buffer nntp-server-buffer)
    (goto-char (point-min))
    ;; Parse the result we got from `gnus-request-group'.
    (when (looking-at "[0-9]+ [0-9]+ \\([0-9]+\\) [0-9]+")
      (goto-char (match-beginning 1))
      (cons (read (current-buffer))
	    (read (current-buffer))))))

(defun gnus-make-articles-unread (group articles)
  "Mark ARTICLES in GROUP as unread."
  (let* ((info (nth 2 (or (gnus-gethash group gnus-newsrc-hashtb)
			  (gnus-gethash (gnus-group-real-name group)
					gnus-newsrc-hashtb))))
	 (ranges (gnus-info-read info))
	 news article)
    (while articles
      (when (gnus-member-of-range
	     (setq article (pop articles)) ranges)
	(push article news)))
    (when news
      (gnus-info-set-read
       info (gnus-remove-from-range (gnus-info-read info) (nreverse news)))
      (gnus-group-update-group group t))))

;; Enter all dead groups into the hashtb.
(defun gnus-update-active-hashtb-from-killed ()
  (let ((hashtb (setq gnus-active-hashtb (gnus-make-hashtable 4096)))
	(lists (list gnus-killed-list gnus-zombie-list))
	killed)
    (while lists
      (setq killed (car lists))
      (while killed
	(gnus-sethash (car killed) nil hashtb)
	(setq killed (cdr killed)))
      (setq lists (cdr lists)))))

(defun gnus-get-killed-groups ()
  "Go through the active hashtb and mark all unknown groups as killed."
  ;; First make sure active file has been read.
  (unless (gnus-read-active-file-p)
    (let ((gnus-read-active-file t))
      (gnus-read-active-file)))
  (unless gnus-killed-hashtb
    (gnus-make-hashtable-from-killed))
  ;; Go through all newsgroups that are known to Gnus - enlarge kill list.
  (mapatoms
   (lambda (sym)
     (let ((groups 0)
	   (group (symbol-name sym)))
       (if (or (null group)
	       (gnus-gethash group gnus-killed-hashtb)
	       (gnus-gethash group gnus-newsrc-hashtb))
	   ()
	 (let ((do-sub (gnus-matches-options-n group)))
	   (if (or (eq do-sub 'subscribe) (eq do-sub 'ignore))
	       ()
	     (setq groups (1+ groups))
	     (push group gnus-killed-list)
	     (gnus-sethash group group gnus-killed-hashtb))))))
   gnus-active-hashtb)
  (gnus-dribble-touch))

;; Get the active file(s) from the backend(s).
(defun gnus-read-active-file (&optional force not-native)
  (gnus-group-set-mode-line)
  (let ((methods
	 (mapcar
	  (lambda (m) (if (stringp m) (gnus-server-get-method nil m) m))
	  (append
	   (if (and (not not-native)
		    (gnus-check-server gnus-select-method))
	       ;; The native server is available.
	       (cons gnus-select-method gnus-secondary-select-methods)
	     ;; The native server is down, so we just do the
	     ;; secondary ones.
	     gnus-secondary-select-methods)
	   ;; Also read from the archive server.
	   (when (gnus-archive-server-wanted-p)
	     (list "archive")))))
	method)
    (setq gnus-have-read-active-file nil)
    (save-excursion
      (set-buffer nntp-server-buffer)
      (while (setq method (pop methods))
	;; Only do each method once, in case the methods appear more
	;; than once in this list.
	(unless (member method methods)
	  (condition-case ()
	      (gnus-read-active-file-1 method force)
	    ;; We catch C-g so that we can continue past servers
	    ;; that do not respond.
	    (quit
	     (message "Quit reading the active file")
	     nil)))))))

(defun gnus-read-active-file-1 (method force)
  (let (where mesg)
    (setq where (nth 1 method)
	  mesg (format "Reading active file%s via %s..."
		       (if (and where (not (zerop (length where))))
			   (concat " from " where) "")
		       (car method)))
    (gnus-message 5 mesg)
    (when (gnus-check-server method)
      ;; Request that the backend scan its incoming messages.
      (when (gnus-check-backend-function 'request-scan (car method))
	(gnus-request-scan nil method))
      (cond
       ((and (eq gnus-read-active-file 'some)
	     (gnus-check-backend-function 'retrieve-groups (car method))
	     (not force))
	(let ((newsrc (cdr gnus-newsrc-alist))
	      (gmethod (gnus-server-get-method nil method))
	      groups info)
	  (while (setq info (pop newsrc))
	    (when (inline
		    (gnus-server-equal
		     (inline
		       (gnus-find-method-for-group
			(gnus-info-group info) info))
		     gmethod))
	      (push (gnus-group-real-name (gnus-info-group info))
		    groups)))
	  (gnus-read-active-file-2 groups method)))
       ((null method)
	t)
       (t
	(if (not (gnus-request-list method))
	    (unless (equal method gnus-message-archive-method)
	      (gnus-error 1 "Cannot read active file from %s server"
			  (car method)))
	  (gnus-message 5 mesg)
	  (gnus-active-to-gnus-format method gnus-active-hashtb nil t)
	  ;; We mark this active file as read.
	  (push method gnus-have-read-active-file)
	  (gnus-message 5 "%sdone" mesg)))))))

(defun gnus-read-active-file-2 (groups method)
  "Read an active file for GROUPS in METHOD using gnus-retrieve-groups."
  (when groups
    (save-excursion
      (set-buffer nntp-server-buffer)
      (gnus-check-server method)
      (let ((list-type (gnus-retrieve-groups groups method)))
	(cond ((not list-type)
	       (gnus-error
		1.2 "Cannot read partial active file from %s server."
		(car method)))
	      ((eq list-type 'active)
	       (gnus-active-to-gnus-format method gnus-active-hashtb nil t))
	      (t
	       (gnus-groups-to-gnus-format method gnus-active-hashtb t)))))))

;; Read an active file and place the results in `gnus-active-hashtb'.
(defun gnus-active-to-gnus-format (&optional method hashtb ignore-errors
					     real-active)
  (unless method
    (setq method gnus-select-method))
  (let ((cur (current-buffer))
	(hashtb (or hashtb
		    (if (and gnus-active-hashtb
			     (not (equal method gnus-select-method)))
			gnus-active-hashtb
		      (setq gnus-active-hashtb
			    (if (equal method gnus-select-method)
				(gnus-make-hashtable
				 (count-lines (point-min) (point-max)))
			      (gnus-make-hashtable 4096)))))))
    ;; Delete unnecessary lines.
    (goto-char (point-min))
    (cond
     ((string= gnus-ignored-newsgroups "")
      (delete-matching-lines "^to\\."))
     (t
      (delete-matching-lines (concat "^to\\.\\|" gnus-ignored-newsgroups))))

    (goto-char (point-min))
    (unless (re-search-forward "[\\\"]" nil t)
      ;; Make the group names readable as a lisp expression even if they
      ;; contain special characters.
      (goto-char (point-max))
      (while (re-search-backward "[][';?()#]" nil t)
	(insert ?\\)))

    ;; Let the Gnus agent save the active file.
    (when (and gnus-agent real-active gnus-plugged)
      (gnus-agent-save-active method))

    ;; If these are groups from a foreign select method, we insert the
    ;; group prefix in front of the group names.
    (when (not (gnus-server-equal
		(gnus-server-get-method nil method)
		(gnus-server-get-method nil gnus-select-method)))
      (let ((prefix (gnus-group-prefixed-name "" method)))
	(goto-char (point-min))
	(while (and (not (eobp))
		    (progn
		      (when (= (following-char) ?\")
			(forward-char 1))
		      (insert prefix)
		      (zerop (forward-line 1)))))))
    ;; Store the active file in a hash table.
    (goto-char (point-min))
    (let (group max min)
      (while (not (eobp))
	(condition-case err
	    (progn
	      (narrow-to-region (point) (gnus-point-at-eol))
	      ;; group gets set to a symbol interned in the hash table
	      ;; (what a hack!!) - jwz
	      (setq group (let ((obarray hashtb)) (read cur)))
	      ;; ### The extended group name scheme makes
	      ;; the previous optimization strategy sort of pointless...
	      (when (stringp group)
		(setq group (intern group hashtb)))
	      (if (and (numberp (setq max (read cur)))
		       (numberp (setq min (read cur)))
		       (progn
			 (skip-chars-forward " \t")
			 (not
			  (or (eq (char-after) ?=)
			      (eq (char-after) ?x)
			      (eq (char-after) ?j)))))
		  (progn
		    (set group (cons min max))
		    ;; if group is moderated, stick in moderation table
		    (when (eq (char-after) ?m)
		      (unless gnus-moderated-hashtb
			(setq gnus-moderated-hashtb (gnus-make-hashtable)))
		      (gnus-sethash (symbol-name group) t
				    gnus-moderated-hashtb)))
		(set group nil)))
	  (error
	   (and group
		(symbolp group)
		(set group nil))
	   (unless ignore-errors
	     (gnus-message 3 "Warning - invalid active: %s"
			   (buffer-substring
			    (gnus-point-at-bol) (gnus-point-at-eol))))))
	(widen)
	(forward-line 1)))))

(defun gnus-groups-to-gnus-format (method &optional hashtb real-active)
  ;; Parse a "groups" active file.
  (let ((cur (current-buffer))
	(hashtb (or hashtb
		    (if (and method gnus-active-hashtb)
			gnus-active-hashtb
		      (setq gnus-active-hashtb
			    (gnus-make-hashtable
			     (count-lines (point-min) (point-max)))))))
	(prefix (and method
		     (not (gnus-server-equal
			   (gnus-server-get-method nil method)
			   (gnus-server-get-method nil gnus-select-method)))
		     (gnus-group-prefixed-name "" method))))

    ;; Let the Gnus agent save the active file.
    (if (and gnus-agent
	     real-active
	     gnus-plugged
	     (gnus-agent-method-p method))
	(progn
	  (gnus-agent-save-groups method)
	  (gnus-active-to-gnus-format method hashtb nil real-active))

      (goto-char (point-min))
      ;; We split this into to separate loops, one with the prefix
      ;; and one without to speed the reading up somewhat.
      (if prefix
	  (let (min max opoint group)
	    (while (not (eobp))
	      (condition-case ()
		  (progn
		    (read cur) (read cur)
		    (setq min (read cur)
			  max (read cur)
			  opoint (point))
		    (skip-chars-forward " \t")
		    (insert prefix)
		    (goto-char opoint)
		    (set (let ((obarray hashtb)) (read cur))
			 (cons min max)))
		(error (and group (symbolp group) (set group nil))))
	      (forward-line 1)))
	(let (min max group)
	  (while (not (eobp))
	    (condition-case ()
		(when (eq (char-after) ?2)
		  (read cur) (read cur)
		  (setq min (read cur)
			max (read cur))
		  (set (setq group (let ((obarray hashtb)) (read cur)))
		       (cons min max)))
	      (error (and group (symbolp group) (set group nil))))
	    (forward-line 1)))))))

(defun gnus-read-newsrc-file (&optional force)
  "Read startup file.
If FORCE is non-nil, the .newsrc file is read."
  ;; Reset variables that might be defined in the .newsrc.eld file.
  (let ((variables gnus-variable-list))
    (while variables
      (set (car variables) nil)
      (setq variables (cdr variables))))
  (let* ((newsrc-file gnus-current-startup-file)
	 (quick-file (concat newsrc-file ".el")))
    (save-excursion
      ;; We always load the .newsrc.eld file.  If always contains
      ;; much information that can not be gotten from the .newsrc
      ;; file (ticked articles, killed groups, foreign methods, etc.)
      (gnus-read-newsrc-el-file quick-file)

      (when (and gnus-read-newsrc-file
		 (file-exists-p gnus-current-startup-file)
		 (or force
		     (and (file-newer-than-file-p newsrc-file quick-file)
			  (file-newer-than-file-p newsrc-file
						  (concat quick-file "d")))
		     (not gnus-newsrc-alist)))
	;; We read the .newsrc file.  Note that if there if a
	;; .newsrc.eld file exists, it has already been read, and
	;; the `gnus-newsrc-hashtb' has been created.  While reading
	;; the .newsrc file, Gnus will only use the information it
	;; can find there for changing the data already read -
	;; i. e., reading the .newsrc file will not trash the data
	;; already read (except for read articles).
	(save-excursion
	  (gnus-message 5 "Reading %s..." newsrc-file)
	  (set-buffer (nnheader-find-file-noselect newsrc-file))
	  (buffer-disable-undo)
	  (gnus-newsrc-to-gnus-format)
	  (kill-buffer (current-buffer))
	  (gnus-message 5 "Reading %s...done" newsrc-file)))

      ;; Convert old to new.
      (gnus-convert-old-newsrc))))

(defun gnus-convert-old-newsrc ()
  "Convert old newsrc into the new format, if needed."
  (let ((fcv (and gnus-newsrc-file-version
		  (gnus-continuum-version gnus-newsrc-file-version))))
    (cond
     ;; No .newsrc.eld file was loaded.
     ((null fcv) nil)
     ;; Gnus 5 .newsrc.eld was loaded.
     ((< fcv (gnus-continuum-version "September Gnus v0.1"))
      (gnus-convert-old-ticks)))))

(defun gnus-convert-old-ticks ()
  (let ((newsrc (cdr gnus-newsrc-alist))
	marks info dormant ticked)
    (while (setq info (pop newsrc))
      (when (setq marks (gnus-info-marks info))
	(setq dormant (cdr (assq 'dormant marks))
	      ticked (cdr (assq 'tick marks)))
	(when (or dormant ticked)
	  (gnus-info-set-read
	   info
	   (gnus-add-to-range
	    (gnus-info-read info)
	    (nconc (gnus-uncompress-range dormant)
		   (gnus-uncompress-range ticked)))))))))

(defun gnus-read-newsrc-el-file (file)
  (let ((ding-file (concat file "d")))
    ;; We always, always read the .eld file.
    (gnus-message 5 "Reading %s..." ding-file)
    (let (gnus-newsrc-assoc)
      (condition-case nil
	  (let ((coding-system-for-read gnus-ding-file-coding-system))
	    (load ding-file t t t))
	(error
	 (ding)
	 (unless (gnus-yes-or-no-p
		  (format "Error in %s; continue? " ding-file))
	   (error "Error in %s" ding-file))))
      (when gnus-newsrc-assoc
	(setq gnus-newsrc-alist gnus-newsrc-assoc)))
    (gnus-make-hashtable-from-newsrc-alist)
    (when (file-newer-than-file-p file ding-file)
      ;; Old format quick file
      (gnus-message 5 "Reading %s..." file)
      ;; The .el file is newer than the .eld file, so we read that one
      ;; as well.
      (gnus-read-old-newsrc-el-file file))))

;; Parse the old-style quick startup file
(defun gnus-read-old-newsrc-el-file (file)
  (let (newsrc killed marked group m info)
    (prog1
	(let ((gnus-killed-assoc nil)
	      gnus-marked-assoc gnus-newsrc-alist gnus-newsrc-assoc)
	  (prog1
	      (ignore-errors
		(load file t t t))
	    (setq newsrc gnus-newsrc-assoc
		  killed gnus-killed-assoc
		  marked gnus-marked-assoc)))
      (setq gnus-newsrc-alist nil)
      (while (setq group (pop newsrc))
	(if (setq info (gnus-get-info (car group)))
	    (progn
	      (gnus-info-set-read info (cddr group))
	      (gnus-info-set-level
	       info (if (nth 1 group) gnus-level-default-subscribed
		      gnus-level-default-unsubscribed))
	      (push info gnus-newsrc-alist))
	  (push (setq info
		      (list (car group)
			    (if (nth 1 group) gnus-level-default-subscribed
			      gnus-level-default-unsubscribed)
			    (cddr group)))
		gnus-newsrc-alist))
	;; Copy marks into info.
	(when (setq m (assoc (car group) marked))
	  (unless (nthcdr 3 info)
	    (nconc info (list nil)))
	  (gnus-info-set-marks
	   info (list (cons 'tick (gnus-compress-sequence
				   (sort (cdr m) '<) t))))))
      (setq newsrc killed)
      (while newsrc
	(setcar newsrc (caar newsrc))
	(setq newsrc (cdr newsrc)))
      (setq gnus-killed-list killed))
    ;; The .el file version of this variable does not begin with
    ;; "options", while the .eld version does, so we just add it if it
    ;; isn't there.
    (when
	gnus-newsrc-options
      (when (not (string-match "^ *options" gnus-newsrc-options))
	(setq gnus-newsrc-options (concat "options " gnus-newsrc-options)))
      (when (not (string-match "\n$" gnus-newsrc-options))
	(setq gnus-newsrc-options (concat gnus-newsrc-options "\n")))
      ;; Finally, if we read some options lines, we parse them.
      (unless (string= gnus-newsrc-options "")
	(gnus-newsrc-parse-options gnus-newsrc-options)))

    (setq gnus-newsrc-alist (nreverse gnus-newsrc-alist))
    (gnus-make-hashtable-from-newsrc-alist)))

(defun gnus-make-newsrc-file (file)
  "Make server dependent file name by catenating FILE and server host name."
  (let* ((file (expand-file-name file nil))
	 (real-file (concat file "-" (nth 1 gnus-select-method))))
    (if (or (file-exists-p real-file)
	    (file-exists-p (concat real-file ".el"))
	    (file-exists-p (concat real-file ".eld")))
	real-file
      file)))

(defun gnus-newsrc-to-gnus-format ()
  (setq gnus-newsrc-options "")
  (setq gnus-newsrc-options-n nil)

  (unless gnus-active-hashtb
    (setq gnus-active-hashtb (gnus-make-hashtable 4096)))
  (let ((buf (current-buffer))
	(already-read (> (length gnus-newsrc-alist) 1))
	group subscribed options-symbol newsrc Options-symbol
	symbol reads num1)
    (goto-char (point-min))
    ;; We intern the symbol `options' in the active hashtb so that we
    ;; can `eq' against it later.
    (set (setq options-symbol (intern "options" gnus-active-hashtb)) nil)
    (set (setq Options-symbol (intern "Options" gnus-active-hashtb)) nil)

    (while (not (eobp))
      ;; We first read the first word on the line by narrowing and
      ;; then reading into `gnus-active-hashtb'.  Most groups will
      ;; already exist in that hashtb, so this will save some string
      ;; space.
      (narrow-to-region
       (point)
       (progn (skip-chars-forward "^ \t!:\n") (point)))
      (goto-char (point-min))
      (setq symbol
	    (and (/= (point-min) (point-max))
		 (let ((obarray gnus-active-hashtb)) (read buf))))
      (widen)
      ;; Now, the symbol we have read is either `options' or a group
      ;; name.  If it is an options line, we just add it to a string.
      (cond
       ((or (eq symbol options-symbol)
	    (eq symbol Options-symbol))
	(setq gnus-newsrc-options
	      ;; This concating is quite inefficient, but since our
	      ;; thorough studies show that approx 99.37% of all
	      ;; .newsrc files only contain a single options line, we
	      ;; don't give a damn, frankly, my dear.
	      (concat gnus-newsrc-options
		      (buffer-substring
		       (gnus-point-at-bol)
		       ;; Options may continue on the next line.
		       (or (and (re-search-forward "^[^ \t]" nil 'move)
				(progn (beginning-of-line) (point)))
			   (point)))))
	(forward-line -1))
       (symbol
	;; Group names can be just numbers.
	(when (numberp symbol)
	  (setq symbol (intern (int-to-string symbol) gnus-active-hashtb)))
	(unless (boundp symbol)
	  (set symbol nil))
	;; It was a group name.
	(setq subscribed (eq (char-after) ?:)
	      group (symbol-name symbol)
	      reads nil)
	(if (eolp)
	    ;; If the line ends here, this is clearly a buggy line, so
	    ;; we put point a the beginning of line and let the cond
	    ;; below do the error handling.
	    (beginning-of-line)
	  ;; We skip to the beginning of the ranges.
	  (skip-chars-forward "!: \t"))
	;; We are now at the beginning of the list of read articles.
	;; We read them range by range.
	(while
	    (cond
	     ((looking-at "[0-9]+")
	      ;; We narrow and read a number instead of buffer-substring/
	      ;; string-to-int because it's faster.  narrow/widen is
	      ;; faster than save-restriction/narrow, and save-restriction
	      ;; produces a garbage object.
	      (setq num1 (progn
			   (narrow-to-region (match-beginning 0) (match-end 0))
			   (read buf)))
	      (widen)
	      ;; If the next character is a dash, then this is a range.
	      (if (eq (char-after) ?-)
		  (progn
		    ;; We read the upper bound of the range.
		    (forward-char 1)
		    (if (not (looking-at "[0-9]+"))
			;; This is a buggy line, by we pretend that
			;; it's kinda OK.  Perhaps the user should be
			;; dinged?
			(push num1 reads)
		      (push
		       (cons num1
			     (progn
			       (narrow-to-region (match-beginning 0)
						 (match-end 0))
			       (read buf)))
		       reads)
		      (widen)))
		;; It was just a simple number, so we add it to the
		;; list of ranges.
		(push num1 reads))
	      ;; If the next char in ?\n, then we have reached the end
	      ;; of the line and return nil.
	      (not (eq (char-after) ?\n)))
	     ((eq (char-after) ?\n)
	      ;; End of line, so we end.
	      nil)
	     (t
	      ;; Not numbers and not eol, so this might be a buggy
	      ;; line...
	      (unless (eobp)
		;; If it was eob instead of ?\n, we allow it.
		;; The line was buggy.
		(setq group nil)
		(gnus-error 3.1 "Mangled line: %s"
			    (buffer-substring (gnus-point-at-bol)
					      (gnus-point-at-eol))))
	      nil))
	  ;; Skip past ", ".  Spaces are invalid in these ranges, but
	  ;; we allow them, because it's a common mistake to put a
	  ;; space after the comma.
	  (skip-chars-forward ", "))

	;; We have already read .newsrc.eld, so we gently update the
	;; data in the hash table with the information we have just
	;; read.
	(when group
	  (let ((info (gnus-get-info group))
		level)
	    (if info
		;; There is an entry for this file in the alist.
		(progn
		  (gnus-info-set-read info (nreverse reads))
		  ;; We update the level very gently.  In fact, we
		  ;; only change it if there's been a status change
		  ;; from subscribed to unsubscribed, or vice versa.
		  (setq level (gnus-info-level info))
		  (cond ((and (<= level gnus-level-subscribed)
			      (not subscribed))
			 (setq level (if reads
					 gnus-level-default-unsubscribed
				       (1+ gnus-level-default-unsubscribed))))
			((and (> level gnus-level-subscribed) subscribed)
			 (setq level gnus-level-default-subscribed)))
		  (gnus-info-set-level info level))
	      ;; This is a new group.
	      (setq info (list group
			       (if subscribed
				   gnus-level-default-subscribed
				 (if reads
				     (1+ gnus-level-subscribed)
				   gnus-level-default-unsubscribed))
			       (nreverse reads))))
	    (push info newsrc)))))
      (forward-line 1))

    (setq newsrc (nreverse newsrc))

    (if (not already-read)
	()
      ;; We now have two newsrc lists - `newsrc', which is what we
      ;; have read from .newsrc, and `gnus-newsrc-alist', which is
      ;; what we've read from .newsrc.eld.  We have to merge these
      ;; lists.  We do this by "attaching" any (foreign) groups in the
      ;; gnus-newsrc-alist to the (native) group that precedes them.
      (let ((rc (cdr gnus-newsrc-alist))
	    (prev gnus-newsrc-alist)
	    entry mentry)
	(while rc
	  (or (null (nth 4 (car rc)))	; It's a native group.
	      (assoc (caar rc) newsrc)	; It's already in the alist.
	      (if (setq entry (assoc (caar prev) newsrc))
		  (setcdr (setq mentry (memq entry newsrc))
			  (cons (car rc) (cdr mentry)))
		(push (car rc) newsrc)))
	  (setq prev rc
		rc (cdr rc)))))

    (setq gnus-newsrc-alist newsrc)
    ;; We make the newsrc hashtb.
    (gnus-make-hashtable-from-newsrc-alist)

    ;; Finally, if we read some options lines, we parse them.
    (unless (string= gnus-newsrc-options "")
      (gnus-newsrc-parse-options gnus-newsrc-options))))

;; Parse options lines to find "options -n !all rec.all" and stuff.
;; The return value will be a list on the form
;; ((regexp1 . ignore)
;;  (regexp2 . subscribe)...)
;; When handling new newsgroups, groups that match a `ignore' regexp
;; will be ignored, and groups that match a `subscribe' regexp will be
;; subscribed.  A line like
;; options -n !all rec.all
;; will lead to a list that looks like
;; (("^rec\\..+" . subscribe)
;;  ("^.+" . ignore))
;; So all "rec.*" groups will be subscribed, while all the other
;; groups will be ignored.  Note that "options -n !all rec.all" is very
;; different from "options -n rec.all !all".
(defun gnus-newsrc-parse-options (options)
  (let (out eol)
    (save-excursion
      (gnus-set-work-buffer)
      (insert (regexp-quote options))
      ;; First we treat all continuation lines.
      (goto-char (point-min))
      (while (re-search-forward "\n[ \t]+" nil t)
	(replace-match " " t t))
      ;; Then we transform all "all"s into ".+"s.
      (goto-char (point-min))
      (while (re-search-forward "\\ball\\b" nil t)
	(replace-match ".+" t t))
      (goto-char (point-min))
      ;; We remove all other options than the "-n" ones.
      (while (re-search-forward "[ \t]-[^n][^-]*" nil t)
	(replace-match " ")
	(forward-char -1))
      (goto-char (point-min))

      ;; We are only interested in "options -n" lines - we
      ;; ignore the other option lines.
      (while (re-search-forward "[ \t]-n" nil t)
	(setq eol
	      (or (save-excursion
		    (and (re-search-forward "[ \t]-n" (gnus-point-at-eol) t)
			 (- (point) 2)))
		  (gnus-point-at-eol)))
	;; Search for all "words"...
	(while (re-search-forward "[^ \t,\n]+" eol t)
	  (if (eq (char-after (match-beginning 0)) ?!)
	      ;; If the word begins with a bang (!), this is a "not"
	      ;; spec.  We put this spec (minus the bang) and the
	      ;; symbol `ignore' into the list.
	      (push (cons (concat
			   "^" (buffer-substring
				(1+ (match-beginning 0))
				(match-end 0))
			   "\\($\\|\\.\\)")
			  'ignore)
		    out)
	    ;; There was no bang, so this is a "yes" spec.
	    (push (cons (concat "^" (match-string 0) "\\($\\|\\.\\)")
			'subscribe)
		  out))))

      (setq gnus-newsrc-options-n out))))

(defun gnus-save-newsrc-file (&optional force)
  "Save .newsrc file."
  ;; Note: We cannot save .newsrc file if all newsgroups are removed
  ;; from the variable gnus-newsrc-alist.
  (when (and (or gnus-newsrc-alist gnus-killed-list)
	     gnus-current-startup-file)
    (save-excursion
      (if (and (or gnus-use-dribble-file gnus-slave)
	       (not force)
	       (or (not gnus-dribble-buffer)
		   (not (buffer-name gnus-dribble-buffer))
		   (zerop (save-excursion
			    (set-buffer gnus-dribble-buffer)
			    (buffer-size)))))
	  (gnus-message 4 "(No changes need to be saved)")
	(gnus-run-hooks 'gnus-save-newsrc-hook)
	(if gnus-slave
	    (gnus-slave-save-newsrc)
	  ;; Save .newsrc.
	  (when gnus-save-newsrc-file
	    (gnus-message 8 "Saving %s..." gnus-current-startup-file)
	    (gnus-gnus-to-newsrc-format)
	    (gnus-message 8 "Saving %s...done" gnus-current-startup-file))
	  ;; Save .newsrc.eld.
	  (set-buffer (gnus-get-buffer-create " *Gnus-newsrc*"))
	  (make-local-variable 'version-control)
	  (setq version-control 'never)
	  (setq buffer-file-name
		(concat gnus-current-startup-file ".eld"))
	  (setq default-directory (file-name-directory buffer-file-name))
	  (buffer-disable-undo)
	  (erase-buffer)
	  (gnus-message 5 "Saving %s.eld..." gnus-current-startup-file)
	  (gnus-gnus-to-quick-newsrc-format)
	  (gnus-run-hooks 'gnus-save-quick-newsrc-hook)
	  (let ((coding-system-for-write gnus-ding-file-coding-system))
	    (save-buffer))
	  (kill-buffer (current-buffer))
	  (gnus-message
	   5 "Saving %s.eld...done" gnus-current-startup-file))
	(gnus-dribble-delete-file)
	(gnus-group-set-mode-line)))))

(defun gnus-gnus-to-quick-newsrc-format ()
  "Insert Gnus variables such as gnus-newsrc-alist in lisp format."
  (let ((print-quoted t)
	(print-escape-newlines t))

    (insert ";; -*- emacs-lisp -*-\n")
    (insert ";; Gnus startup file.\n")
    (insert "\
;; Never delete this file -- if you want to force Gnus to read the
;; .newsrc file (if you have one), touch .newsrc instead.\n")
    (insert "(setq gnus-newsrc-file-version "
	    (prin1-to-string gnus-version) ")\n")
    (let* ((gnus-killed-list
	    (if (and gnus-save-killed-list
		     (stringp gnus-save-killed-list))
		(gnus-strip-killed-list)
	      gnus-killed-list))
	   (variables
	    (if gnus-save-killed-list gnus-variable-list
	      ;; Remove the `gnus-killed-list' from the list of variables
	      ;; to be saved, if required.
	      (delq 'gnus-killed-list (copy-sequence gnus-variable-list))))
	   ;; Peel off the "dummy" group.
	   (gnus-newsrc-alist (cdr gnus-newsrc-alist))
	   variable)
      ;; Insert the variables into the file.
      (while variables
	(when (and (boundp (setq variable (pop variables)))
		   (symbol-value variable))
	  (insert "(setq " (symbol-name variable) " '")
	  (gnus-prin1 (symbol-value variable))
	  (insert ")\n"))))))

(defun gnus-strip-killed-list ()
  "Return the killed list minus the groups that match `gnus-save-killed-list'."
  (let ((list gnus-killed-list)
	olist)
    (while list
      (when (string-match gnus-save-killed-list (car list))
	(push (car list) olist))
      (pop list))
    (nreverse olist)))

(defun gnus-gnus-to-newsrc-format ()
  ;; Generate and save the .newsrc file.
  (save-excursion
    (set-buffer (create-file-buffer gnus-current-startup-file))
    (let ((newsrc (cdr gnus-newsrc-alist))
	  (standard-output (current-buffer))
	  info ranges range method)
      (setq buffer-file-name gnus-current-startup-file)
      (setq default-directory (file-name-directory buffer-file-name))
      (buffer-disable-undo)
      (erase-buffer)
      ;; Write options.
      (when gnus-newsrc-options
	(insert gnus-newsrc-options))
      ;; Write subscribed and unsubscribed.
      (while (setq info (pop newsrc))
	;; Don't write foreign groups to .newsrc.
	(when (or (null (setq method (gnus-info-method info)))
		  (equal method "native")
		  (inline (gnus-server-equal method gnus-select-method)))
	  (insert (gnus-info-group info)
		  (if (> (gnus-info-level info) gnus-level-subscribed)
		      "!" ":"))
	  (when (setq ranges (gnus-info-read info))
	    (insert " ")
	    (if (not (listp (cdr ranges)))
		(if (= (car ranges) (cdr ranges))
		    (princ (car ranges))
		  (princ (car ranges))
		  (insert "-")
		  (princ (cdr ranges)))
	      (while (setq range (pop ranges))
		(if (or (atom range) (= (car range) (cdr range)))
		    (princ (or (and (atom range) range) (car range)))
		  (princ (car range))
		  (insert "-")
		  (princ (cdr range)))
		(when ranges
		  (insert ",")))))
	  (insert "\n")))
      (make-local-variable 'version-control)
      (setq version-control 'never)
      ;; It has been reported that sometime the modtime on the .newsrc
      ;; file seems to be off.  We really do want to overwrite it, so
      ;; we clear the modtime here before saving.  It's a bit odd,
      ;; though...
      ;; sometimes the modtime clear isn't sufficient.  most brute force:
      ;; delete the silly thing entirely first.  but this fails to provide
      ;; such niceties as .newsrc~ creation.
      (if gnus-modtime-botch
	  (delete-file gnus-startup-file)
	(clear-visited-file-modtime))
      (gnus-run-hooks 'gnus-save-standard-newsrc-hook)
      (save-buffer)
      (kill-buffer (current-buffer)))))


;;;
;;; Slave functions.
;;;

(defvar gnus-slave-mode nil)

(defun gnus-slave-mode ()
  "Minor mode for slave Gnusae."
  (gnus-add-minor-mode 'gnus-slave-mode " Slave" (make-sparse-keymap))
  (gnus-run-hooks 'gnus-slave-mode-hook))

(defun gnus-slave-save-newsrc ()
  (save-excursion
    (set-buffer gnus-dribble-buffer)
    (let ((slave-name
	   (mm-make-temp-file (concat gnus-current-startup-file "-slave-")))
	  (modes (ignore-errors
		   (file-modes (concat gnus-current-startup-file ".eld")))))
      (let ((coding-system-for-write gnus-ding-file-coding-system))
	(gnus-write-buffer slave-name))
      (when modes
	(set-file-modes slave-name modes)))))

(defun gnus-master-read-slave-newsrc ()
  (let ((slave-files
	 (directory-files
	  (file-name-directory gnus-current-startup-file)
	  t (concat
	     "^" (regexp-quote
		  (concat
		   (file-name-nondirectory gnus-current-startup-file)
		   "-slave-")))
	  t))
	file)
    (if (not slave-files)
	()				; There are no slave files to read.
      (gnus-message 7 "Reading slave newsrcs...")
      (save-excursion
	(set-buffer (gnus-get-buffer-create " *gnus slave*"))
	(setq slave-files
	      (sort (mapcar (lambda (file)
			      (list (nth 5 (file-attributes file)) file))
			    slave-files)
		    (lambda (f1 f2)
		      (or (< (caar f1) (caar f2))
			  (< (nth 1 (car f1)) (nth 1 (car f2)))))))
	(while slave-files
	  (erase-buffer)
	  (setq file (nth 1 (car slave-files)))
	  (nnheader-insert-file-contents file)
	  (when (condition-case ()
		    (progn
		      (eval-buffer (current-buffer))
		      t)
		  (error
		   (gnus-error 3.2 "Possible error in %s" file)
		   nil))
	    (unless gnus-slave		; Slaves shouldn't delete these files.
	      (ignore-errors
		(delete-file file))))
	  (setq slave-files (cdr slave-files))))
      (gnus-dribble-touch)
      (gnus-message 7 "Reading slave newsrcs...done"))))


;;;
;;; Group description.
;;;

(defun gnus-read-all-descriptions-files ()
  (let ((methods (cons gnus-select-method
		       (nconc
			(when (gnus-archive-server-wanted-p)
			  (list "archive"))
			gnus-secondary-select-methods))))
    (while methods
      (gnus-read-descriptions-file (car methods))
      (setq methods (cdr methods)))
    t))

(defun gnus-read-descriptions-file (&optional method)
  (let ((method (or method gnus-select-method))
	group)
    (when (stringp method)
      (setq method (gnus-server-to-method method)))
    ;; We create the hashtable whether we manage to read the desc file
    ;; to avoid trying to re-read after a failed read.
    (unless gnus-description-hashtb
      (setq gnus-description-hashtb
	    (gnus-make-hashtable (length gnus-active-hashtb))))
    ;; Mark this method's desc file as read.
    (gnus-sethash (gnus-group-prefixed-name "" method) "Has read"
		  gnus-description-hashtb)

    (gnus-message 5 "Reading descriptions file via %s..." (car method))
    (cond
     ((null (gnus-get-function method 'request-list-newsgroups t))
      t)
     ((not (gnus-check-server method))
      (gnus-message 1 "Couldn't open server")
      nil)
     ((not (gnus-request-list-newsgroups method))
      (gnus-message 1 "Couldn't read newsgroups descriptions")
      nil)
     (t
      (save-excursion
	(save-restriction
	  (set-buffer nntp-server-buffer)
	  (goto-char (point-min))
	  (when (or (search-forward "\n.\n" nil t)
		    (goto-char (point-max)))
	    (beginning-of-line)
	    (narrow-to-region (point-min) (point)))
	  ;; If these are groups from a foreign select method, we insert the
	  ;; group prefix in front of the group names.
	  (and method (not (inline
			     (gnus-server-equal
			      (gnus-server-get-method nil method)
			      (gnus-server-get-method
			       nil gnus-select-method))))
	       (let ((prefix (gnus-group-prefixed-name "" method)))
		 (goto-char (point-min))
		 (while (and (not (eobp))
			     (progn (insert prefix)
				    (zerop (forward-line 1)))))))
	  (goto-char (point-min))
	  (while (not (eobp))
	    ;; If we get an error, we set group to 0, which is not a
	    ;; symbol...
	    (setq group
		  (condition-case ()
		      (let ((obarray gnus-description-hashtb))
			;; Group is set to a symbol interned in this
			;; hash table.
			(read nntp-server-buffer))
		    (error 0)))
	    (skip-chars-forward " \t")
	    ;; ...  which leads to this line being effectively ignored.
	    (when (symbolp group)
	      (let ((str (buffer-substring
			  (point) (progn (end-of-line) (point))))
		    (coding
		     (and (or (featurep 'xemacs)
			      (and (boundp 'enable-multibyte-characters)
				   enable-multibyte-characters))
			  (fboundp 'gnus-mule-get-coding-system)
			  (gnus-mule-get-coding-system (symbol-name group)))))
		(when coding
		  (setq str (mm-decode-coding-string str (car coding))))
		(set group str)))
	    (forward-line 1))))
      (gnus-message 5 "Reading descriptions file...done")
      t))))

(defun gnus-group-get-description (group)
  "Get the description of a group by sending XGTITLE to the server."
  (when (gnus-request-group-description group)
    (save-excursion
      (set-buffer nntp-server-buffer)
      (goto-char (point-min))
      (when (looking-at "[^ \t]+[ \t]+\\(.*\\)")
	(match-string 1)))))

;;;###autoload
(defun gnus-declare-backend (name &rest abilities)
  "Declare backend NAME with ABILITIES as a Gnus backend."
  (setq gnus-valid-select-methods
	(nconc gnus-valid-select-methods
	       (list (apply 'list name abilities))))
  (gnus-redefine-select-method-widget))

(defun gnus-set-default-directory ()
  "Set the default directory in the current buffer to `gnus-default-directory'.
If this variable is nil, don't do anything."
  (setq default-directory
	(if (and gnus-default-directory
		 (file-exists-p gnus-default-directory))
	    (file-name-as-directory (expand-file-name gnus-default-directory))
	  default-directory)))

(provide 'gnus-start)

;;; gnus-start.el ends here
