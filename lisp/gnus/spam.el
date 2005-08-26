;;; spam.el --- Identifying spam
;; Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: network

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

;;; This module addresses a few aspects of spam control under Gnus.  Page
;;; breaks are used for grouping declarations and documentation relating to
;;; each particular aspect.

;;; The integration with Gnus is not yet complete.  See various `FIXME'
;;; comments, below, for supplementary explanations or discussions.

;;; Several TODO items are marked as such

;; TODO: spam scores, detection of spam in newsgroups, cross-server splitting,
;; remote processing, training through files

;;; Code:

(eval-when-compile (require 'cl))

(require 'gnus-sum)

(require 'gnus-uu)			; because of key prefix issues
;;; for the definitions of group content classification and spam processors
(require 'gnus)
(require 'message)		;for the message-fetch-field functions

;; for nnimap-split-download-body-default
(eval-when-compile (require 'nnimap))

;; autoload executable-find
(eval-and-compile
  ;; executable-find is not autoloaded in Emacs 20
  (autoload 'executable-find "executable"))

;; autoload query-dig
(eval-and-compile
  (autoload 'query-dig "dig"))

;; autoload spam-report
(eval-and-compile
  (autoload 'spam-report-gmane "spam-report"))

;; autoload gnus-registry
(eval-and-compile
  (autoload 'gnus-registry-group-count "gnus-registry")
  (autoload 'gnus-registry-add-group "gnus-registry")
  (autoload 'gnus-registry-store-extra-entry "gnus-registry")
  (autoload 'gnus-registry-fetch-extra "gnus-registry"))

;; autoload query-dns
(eval-and-compile
  (autoload 'query-dns "dns"))

;;; Main parameters.

(defgroup spam nil
  "Spam configuration."
  :version "22.1"
  :group 'mail
  :group 'news)

(defcustom spam-directory (nnheader-concat gnus-directory "spam/")
  "Directory for spam whitelists and blacklists."
  :type 'directory
  :group 'spam)

(defcustom spam-move-spam-nonspam-groups-only t
  "Whether spam should be moved in non-spam groups only.
When t, only ham and unclassified groups will have their spam moved
to the spam-process-destination.  When nil, spam will also be moved from
spam groups."
  :type 'boolean
  :group 'spam)

(defcustom spam-process-ham-in-nonham-groups nil
  "Whether ham should be processed in non-ham groups."
  :type 'boolean
  :group 'spam)

(defcustom spam-log-to-registry nil
  "Whether spam/ham processing should be logged in the registry."
  :type 'boolean
  :group 'spam)

(defcustom spam-split-symbolic-return nil
  "Whether `spam-split' should work with symbols or group names."
  :type 'boolean
  :group 'spam)

(defcustom spam-split-symbolic-return-positive nil
  "Whether `spam-split' should ALWAYS work with symbols or group names.
Do not set this if you use `spam-split' in a fancy split
  method."
  :type 'boolean
  :group 'spam)

(defcustom spam-process-ham-in-spam-groups nil
  "Whether ham should be processed in spam groups."
  :type 'boolean
  :group 'spam)

(defcustom spam-mark-only-unseen-as-spam t
  "Whether only unseen articles should be marked as spam in spam groups.
When nil, all unread articles in a spam group are marked as
spam.  Set this if you want to leave an article unread in a spam group
without losing it to the automatic spam-marking process."
  :type 'boolean
  :group 'spam)

(defcustom spam-mark-ham-unread-before-move-from-spam-group nil
  "Whether ham should be marked unread before it's moved.
The article is moved out of a spam group according to ham-process-destination.
This variable is an official entry in the international Longest Variable Name
Competition."
  :type 'boolean
  :group 'spam)

(defcustom spam-disable-spam-split-during-ham-respool nil
  "Whether `spam-split' should be ignored while resplitting ham in a process
destination.  This is useful to prevent ham from ending up in the same spam
group after the resplit.  Don't set this to t if you have spam-split as the
last rule in your split configuration."
  :type 'boolean
  :group 'spam)

(defcustom spam-autodetect-recheck-messages nil
  "Should spam.el recheck all meessages when autodetecting?
Normally this is nil, so only unseen messages will be checked."
  :type 'boolean
  :group 'spam)

(defcustom spam-whitelist (expand-file-name "whitelist" spam-directory)
  "The location of the whitelist.
The file format is one regular expression per line.
The regular expression is matched against the address."
  :type 'file
  :group 'spam)

(defcustom spam-blacklist (expand-file-name "blacklist" spam-directory)
  "The location of the blacklist.
The file format is one regular expression per line.
The regular expression is matched against the address."
  :type 'file
  :group 'spam)

(defcustom spam-use-dig t
  "Whether `query-dig' should be used instead of `query-dns'."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-blacklist nil
  "Whether the blacklist should be used by `spam-split'."
  :type 'boolean
  :group 'spam)

(defcustom spam-blacklist-ignored-regexes nil
  "Regular expressions that the blacklist should ignore."
  :type '(repeat (regexp :tag "Regular expression to ignore when blacklisting"))
  :group 'spam)

(defcustom spam-use-whitelist nil
  "Whether the whitelist should be used by `spam-split'."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-whitelist-exclusive nil
  "Whether whitelist-exclusive should be used by `spam-split'.
Exclusive whitelisting means that all messages from senders not in the whitelist
are considered spam."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-blackholes nil
  "Whether blackholes should be used by `spam-split'."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-hashcash nil
  "Whether hashcash payments should be detected by `spam-split'."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-regex-headers nil
  "Whether a header regular expression match should be used by `spam-split'.
Also see the variables `spam-regex-headers-spam' and `spam-regex-headers-ham'."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-regex-body nil
  "Whether a body regular expression match should be used by `spam-split'.
Also see the variables `spam-regex-body-spam' and `spam-regex-body-ham'."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-bogofilter-headers nil
  "Whether bogofilter headers should be used by `spam-split'.
Enable this if you pre-process messages with Bogofilter BEFORE Gnus sees them."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-bogofilter nil
  "Whether bogofilter should be invoked by `spam-split'.
Enable this if you want Gnus to invoke Bogofilter on new messages."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-BBDB nil
  "Whether BBDB should be used by `spam-split'."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-BBDB-exclusive nil
  "Whether BBDB-exclusive should be used by `spam-split'.
Exclusive BBDB means that all messages from senders not in the BBDB are
considered spam."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-ifile nil
  "Whether ifile should be used by `spam-split'."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-stat nil
  "Whether `spam-stat' should be used by `spam-split'."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-spamoracle nil
  "Whether spamoracle should be used by `spam-split'."
  :type 'boolean
  :group 'spam)

(defcustom spam-install-hooks (or
			       spam-use-dig
			       spam-use-blacklist
			       spam-use-whitelist
			       spam-use-whitelist-exclusive
			       spam-use-blackholes
			       spam-use-hashcash
			       spam-use-regex-headers
			       spam-use-regex-body
			       spam-use-bogofilter-headers
			       spam-use-bogofilter
			       spam-use-BBDB
			       spam-use-BBDB-exclusive
			       spam-use-ifile
			       spam-use-stat
			       spam-use-spamoracle)
  "Whether the spam hooks should be installed.
Default to t if one of the spam-use-* variables is set."
  :group 'spam
  :type 'boolean)

(defcustom spam-split-group "spam"
  "Group name where incoming spam should be put by `spam-split'."
  :type 'string
  :group 'spam)

;;; TODO: deprecate this variable, it's confusing since it's a list of strings,
;;; not regular expressions
(defcustom spam-junk-mailgroups (cons
				 spam-split-group
				 '("mail.junk" "poste.pourriel"))
  "Mailgroups with spam contents.
All unmarked article in such group receive the spam mark on group entry."
  :type '(repeat (string :tag "Group"))
  :group 'spam)

(defcustom spam-blackhole-servers '("bl.spamcop.net" "relays.ordb.org"
				    "dev.null.dk" "relays.visi.com")
  "List of blackhole servers."
  :type '(repeat (string :tag "Server"))
  :group 'spam)

(defcustom spam-blackhole-good-server-regex nil
  "String matching IP addresses that should not be checked in the blackholes."
  :type '(radio (const nil) regexp)
  :group 'spam)

(defface spam
  '((((class color) (type tty) (background dark))
     (:foreground "gray80" :background "gray50"))
    (((class color) (type tty) (background light))
     (:foreground "gray50" :background "gray80"))
    (((class color) (background dark))
     (:foreground "ivory2"))
    (((class color) (background light))
     (:foreground "ivory4"))
    (t :inverse-video t))
  "Face for spam-marked articles."
  :group 'spam)
;; backward-compatibility alias
(put 'spam-face 'face-alias 'spam)

(defcustom spam-face 'spam
  "Face for spam-marked articles."
  :type 'face
  :group 'spam)

(defcustom spam-regex-headers-spam '("^X-Spam-Flag: YES")
  "Regular expression for positive header spam matches."
  :type '(repeat (regexp :tag "Regular expression to match spam header"))
  :group 'spam)

(defcustom spam-regex-headers-ham '("^X-Spam-Flag: NO")
  "Regular expression for positive header ham matches."
  :type '(repeat (regexp :tag "Regular expression to match ham header"))
  :group 'spam)

(defcustom spam-regex-body-spam '()
  "Regular expression for positive body spam matches."
  :type '(repeat (regexp :tag "Regular expression to match spam body"))
  :group 'spam)

(defcustom spam-regex-body-ham '()
  "Regular expression for positive body ham matches."
  :type '(repeat (regexp :tag "Regular expression to match ham body"))
  :group 'spam)

(defgroup spam-ifile nil
  "Spam ifile configuration."
  :group 'spam)

(defcustom spam-ifile-path (executable-find "ifile")
  "File path of the ifile executable program."
  :type '(choice (file :tag "Location of ifile")
		 (const :tag "ifile is not installed"))
  :group 'spam-ifile)

(defcustom spam-ifile-database-path nil
  "File path of the ifile database."
  :type '(choice (file :tag "Location of the ifile database")
		 (const :tag "Use the default"))
  :group 'spam-ifile)

(defcustom spam-ifile-spam-category "spam"
  "Name of the spam ifile category."
  :type 'string
  :group 'spam-ifile)

(defcustom spam-ifile-ham-category nil
  "Name of the ham ifile category.
If nil, the current group name will be used."
  :type '(choice (string :tag "Use a fixed category")
		 (const :tag "Use the current group name"))
  :group 'spam-ifile)

(defcustom spam-ifile-all-categories nil
  "Whether the ifile check will return all categories, or just spam.
Set this to t if you want to use the `spam-split' invocation of ifile as
your main source of newsgroup names."
  :type 'boolean
  :group 'spam-ifile)

(defgroup spam-bogofilter nil
  "Spam bogofilter configuration."
  :group 'spam)

(defcustom spam-bogofilter-path (executable-find "bogofilter")
  "File path of the Bogofilter executable program."
  :type '(choice (file :tag "Location of bogofilter")
		 (const :tag "Bogofilter is not installed"))
  :group 'spam-bogofilter)

(defcustom spam-bogofilter-header "X-Bogosity"
  "The header that Bogofilter inserts in messages."
  :type 'string
  :group 'spam-bogofilter)

(defcustom spam-bogofilter-spam-switch "-s"
  "The switch that Bogofilter uses to register spam messages."
  :type 'string
  :group 'spam-bogofilter)

(defcustom spam-bogofilter-ham-switch "-n"
  "The switch that Bogofilter uses to register ham messages."
  :type 'string
  :group 'spam-bogofilter)

(defcustom spam-bogofilter-spam-strong-switch "-S"
  "The switch that Bogofilter uses to unregister ham messages."
  :type 'string
  :group 'spam-bogofilter)

(defcustom spam-bogofilter-ham-strong-switch "-N"
  "The switch that Bogofilter uses to unregister spam messages."
  :type 'string
  :group 'spam-bogofilter)

(defcustom spam-bogofilter-bogosity-positive-spam-header "^\\(Yes\\|Spam\\)"
  "The regex on `spam-bogofilter-header' for positive spam identification."
  :type 'regexp
  :group 'spam-bogofilter)

(defcustom spam-bogofilter-database-directory nil
  "Directory path of the Bogofilter databases."
  :type '(choice (directory
		  :tag "Location of the Bogofilter database directory")
		 (const :tag "Use the default"))
  :group 'spam-bogofilter)

(defgroup spam-spamoracle nil
  "Spam spamoracle configuration."
  :group 'spam)

(defcustom spam-spamoracle-database nil
  "Location of spamoracle database file. When nil, use the default
spamoracle database."
  :type '(choice (directory :tag "Location of spamoracle database file.")
		 (const :tag "Use the default"))
  :group 'spam-spamoracle)

(defcustom spam-spamoracle-binary (executable-find "spamoracle")
  "Location of the spamoracle binary."
  :type '(choice (directory :tag "Location of the spamoracle binary")
		 (const :tag "Use the default"))
  :group 'spam-spamoracle)

;;; Key bindings for spam control.

(gnus-define-keys gnus-summary-mode-map
  "St" spam-bogofilter-score
  "Sx" gnus-summary-mark-as-spam
  "Mst" spam-bogofilter-score
  "Msx" gnus-summary-mark-as-spam
  "\M-d" gnus-summary-mark-as-spam)

(defvar spam-old-ham-articles nil
  "List of old ham articles, generated when a group is entered.")

(defvar spam-old-spam-articles nil
  "List of old spam articles, generated when a group is entered.")

(defvar spam-split-disabled nil
  "If non-nil, `spam-split' is disabled, and always returns nil.")

(defvar spam-split-last-successful-check nil
  "`spam-split' will set this to nil or a spam-use-XYZ check if it
  finds ham or spam.")

;; convenience functions
(defun spam-xor (a b)
  "Logical exclusive `or'."
  (and (or a b) (not (and a b))))

(defun spam-group-ham-mark-p (group mark &optional spam)
  (when (stringp group)
    (let* ((marks (spam-group-ham-marks group spam))
	   (marks (if (symbolp mark)
		      marks
		    (mapcar 'symbol-value marks))))
      (memq mark marks))))

(defun spam-group-spam-mark-p (group mark)
  (spam-group-ham-mark-p group mark t))

(defun spam-group-ham-marks (group &optional spam)
  (when (stringp group)
    (let* ((marks (if spam
		      (gnus-parameter-spam-marks group)
		    (gnus-parameter-ham-marks group)))
	   (marks (car marks))
	   (marks (if (listp (car marks)) (car marks) marks)))
      marks)))

(defun spam-group-spam-marks (group)
  (spam-group-ham-marks group t))

(defun spam-group-spam-contents-p (group)
  (if (stringp group)
      (or (member group spam-junk-mailgroups)
	  (memq 'gnus-group-spam-classification-spam
		(gnus-parameter-spam-contents group)))
    nil))

(defun spam-group-ham-contents-p (group)
  (if (stringp group)
      (memq 'gnus-group-spam-classification-ham
	    (gnus-parameter-spam-contents group))
    nil))

(defvar spam-list-of-processors
  '((gnus-group-spam-exit-processor-report-gmane spam spam-use-gmane)
    (gnus-group-spam-exit-processor-bogofilter   spam spam-use-bogofilter)
    (gnus-group-spam-exit-processor-blacklist    spam spam-use-blacklist)
    (gnus-group-spam-exit-processor-ifile        spam spam-use-ifile)
    (gnus-group-spam-exit-processor-stat         spam spam-use-stat)
    (gnus-group-spam-exit-processor-spamoracle   spam spam-use-spamoracle)
    (gnus-group-ham-exit-processor-ifile         ham spam-use-ifile)
    (gnus-group-ham-exit-processor-bogofilter    ham spam-use-bogofilter)
    (gnus-group-ham-exit-processor-stat          ham spam-use-stat)
    (gnus-group-ham-exit-processor-whitelist     ham spam-use-whitelist)
    (gnus-group-ham-exit-processor-BBDB          ham spam-use-BBDB)
    (gnus-group-ham-exit-processor-copy          ham spam-use-ham-copy)
    (gnus-group-ham-exit-processor-spamoracle    ham spam-use-spamoracle))
  "The spam-list-of-processors list contains pairs associating a
ham/spam exit processor variable with a classification and a
spam-use-* variable.")

(defun spam-group-processor-p (group processor)
  (if (and (stringp group)
	   (symbolp processor))
      (or (member processor (nth 0 (gnus-parameter-spam-process group)))
	  (spam-group-processor-multiple-p
	   group
	   (cdr-safe (assoc processor spam-list-of-processors))))
    nil))

(defun spam-group-processor-multiple-p (group processor-info)
  (let* ((classification (nth 0 processor-info))
	 (check (nth 1 processor-info))
	 (parameters (nth 0 (gnus-parameter-spam-process group)))
	 found)
    (dolist (parameter parameters)
      (when (and (null found)
		 (listp parameter)
		 (eq classification (nth 0 parameter))
		 (eq check (nth 1 parameter)))
	(setq found t)))
    found))

(defun spam-group-spam-processor-report-gmane-p (group)
  (spam-group-processor-p group 'gnus-group-spam-exit-processor-report-gmane))

(defun spam-group-spam-processor-bogofilter-p (group)
  (spam-group-processor-p group 'gnus-group-spam-exit-processor-bogofilter))

(defun spam-group-spam-processor-blacklist-p (group)
  (spam-group-processor-p group 'gnus-group-spam-exit-processor-blacklist))

(defun spam-group-spam-processor-ifile-p (group)
  (spam-group-processor-p group 'gnus-group-spam-exit-processor-ifile))

(defun spam-group-ham-processor-ifile-p (group)
  (spam-group-processor-p group 'gnus-group-ham-exit-processor-ifile))

(defun spam-group-spam-processor-spamoracle-p (group)
  (spam-group-processor-p group 'gnus-group-spam-exit-processor-spamoracle))

(defun spam-group-ham-processor-bogofilter-p (group)
  (spam-group-processor-p group 'gnus-group-ham-exit-processor-bogofilter))

(defun spam-group-spam-processor-stat-p (group)
  (spam-group-processor-p group 'gnus-group-spam-exit-processor-stat))

(defun spam-group-ham-processor-stat-p (group)
  (spam-group-processor-p group 'gnus-group-ham-exit-processor-stat))

(defun spam-group-ham-processor-whitelist-p (group)
  (spam-group-processor-p group 'gnus-group-ham-exit-processor-whitelist))

(defun spam-group-ham-processor-BBDB-p (group)
  (spam-group-processor-p group 'gnus-group-ham-exit-processor-BBDB))

(defun spam-group-ham-processor-copy-p (group)
  (spam-group-processor-p group 'gnus-group-ham-exit-processor-copy))

(defun spam-group-ham-processor-spamoracle-p (group)
  (spam-group-processor-p group 'gnus-group-ham-exit-processor-spamoracle))

;;; Summary entry and exit processing.

(defun spam-summary-prepare ()
  (setq spam-old-ham-articles
	(spam-list-articles gnus-newsgroup-articles 'ham))
  (setq spam-old-spam-articles
	(spam-list-articles gnus-newsgroup-articles 'spam))
  (spam-mark-junk-as-spam-routine))

;; The spam processors are invoked for any group, spam or ham or neither
(defun spam-summary-prepare-exit ()
  (unless gnus-group-is-exiting-without-update-p
    (gnus-message 6 "Exiting summary buffer and applying spam rules")

    ;; first of all, unregister any articles that are no longer ham or spam
    ;; we have to iterate over the processors, or else we'll be too slow
    (dolist (classification '(spam ham))
      (let* ((old-articles (if (eq classification 'spam)
			       spam-old-spam-articles
			     spam-old-ham-articles))
	     (new-articles (spam-list-articles
			    gnus-newsgroup-articles
			    classification))
	     (changed-articles (gnus-set-difference old-articles new-articles)))
	;; now that we have the changed articles, we go through the processors
	(dolist (processor-param spam-list-of-processors)
	  (let ((processor (nth 0 processor-param))
		(processor-classification (nth 1 processor-param))
		(check (nth 2 processor-param))
		unregister-list)
	    (dolist (article changed-articles)
	      (let ((id (spam-fetch-field-message-id-fast article)))
		(when (spam-log-unregistration-needed-p
		       id 'process classification check)
		  (push article unregister-list))))
	    ;; call spam-register-routine with specific articles to unregister,
	    ;; when there are articles to unregister and the check is enabled
	    (when (and unregister-list (symbol-value check))
	      (spam-register-routine classification check t unregister-list))))))

    ;; find all the spam processors applicable to this group
    (dolist (processor-param spam-list-of-processors)
      (let ((processor (nth 0 processor-param))
	    (classification (nth 1 processor-param))
	    (check (nth 2 processor-param)))
	(when (and (eq 'spam classification)
		   (spam-group-processor-p gnus-newsgroup-name processor))
	  (spam-register-routine classification check))))

    (if spam-move-spam-nonspam-groups-only
	(when (not (spam-group-spam-contents-p gnus-newsgroup-name))
	  (spam-mark-spam-as-expired-and-move-routine
	   (gnus-parameter-spam-process-destination gnus-newsgroup-name)))
      (gnus-message 5 "Marking spam as expired and moving it to %s"
		    gnus-newsgroup-name)
      (spam-mark-spam-as-expired-and-move-routine
       (gnus-parameter-spam-process-destination gnus-newsgroup-name)))

    ;; now we redo spam-mark-spam-as-expired-and-move-routine to only
    ;; expire spam, in case the above did not expire them
    (gnus-message 5 "Marking spam as expired without moving it")
    (spam-mark-spam-as-expired-and-move-routine nil)

    (when (or (spam-group-ham-contents-p gnus-newsgroup-name)
	      (and (spam-group-spam-contents-p gnus-newsgroup-name)
		   spam-process-ham-in-spam-groups)
	      spam-process-ham-in-nonham-groups)
      ;; find all the ham processors applicable to this group
      (dolist (processor-param spam-list-of-processors)
	(let ((processor (nth 0 processor-param))
	      (classification (nth 1 processor-param))
	      (check (nth 2 processor-param)))
	  (when (and (eq 'ham classification)
		     (spam-group-processor-p gnus-newsgroup-name processor))
	    (spam-register-routine classification check)))))

    (when (spam-group-ham-processor-copy-p gnus-newsgroup-name)
      (gnus-message 5 "Copying ham")
      (spam-ham-copy-routine
       (gnus-parameter-ham-process-destination gnus-newsgroup-name)))

    ;; now move all ham articles out of spam groups
    (when (spam-group-spam-contents-p gnus-newsgroup-name)
      (gnus-message 5 "Moving ham messages from spam group")
      (spam-ham-move-routine
       (gnus-parameter-ham-process-destination gnus-newsgroup-name))))

  (setq spam-old-ham-articles nil)
  (setq spam-old-spam-articles nil))

(defun spam-mark-junk-as-spam-routine ()
  ;; check the global list of group names spam-junk-mailgroups and the
  ;; group parameters
  (when (spam-group-spam-contents-p gnus-newsgroup-name)
    (gnus-message 5 "Marking %s articles as spam"
		  (if spam-mark-only-unseen-as-spam
		      "unseen"
		    "unread"))
    (let ((articles (if spam-mark-only-unseen-as-spam
			gnus-newsgroup-unseen
		      gnus-newsgroup-unreads)))
      (dolist (article articles)
	(gnus-summary-mark-article article gnus-spam-mark)))))

(defun spam-mark-spam-as-expired-and-move-routine (&rest groups)
  (if (and (car-safe groups) (listp (car-safe groups)))
      (apply 'spam-mark-spam-as-expired-and-move-routine (car groups))
    (gnus-summary-kill-process-mark)
    (let ((articles gnus-newsgroup-articles)
	  (backend-supports-deletions
	   (gnus-check-backend-function
	    'request-move-article gnus-newsgroup-name))
	  article tomove deletep)
      (dolist (article articles)
	(when (eq (gnus-summary-article-mark article) gnus-spam-mark)
	  (gnus-summary-mark-article article gnus-expirable-mark)
	  (push article tomove)))

      ;; now do the actual copies
      (dolist (group groups)
	(when (and tomove
		   (stringp group))
	  (dolist (article tomove)
	    (gnus-summary-set-process-mark article))
	  (when tomove
	    (if (or (not backend-supports-deletions)
		    (> (length groups) 1))
		(progn
		  (gnus-summary-copy-article nil group)
		  (setq deletep t))
	      (gnus-summary-move-article nil group)))))

      ;; now delete the articles, if there was a copy done, and the
      ;; backend allows it
      (when (and deletep backend-supports-deletions)
	(dolist (article tomove)
	  (gnus-summary-set-process-mark article))
	(when tomove
	  (let ((gnus-novice-user nil))	; don't ask me if I'm sure
	    (gnus-summary-delete-article nil))))

      (gnus-summary-yank-process-mark))))

(defun spam-ham-copy-or-move-routine (copy groups)
  (gnus-summary-kill-process-mark)
  (let ((todo (spam-list-articles gnus-newsgroup-articles 'ham))
	(backend-supports-deletions
	 (gnus-check-backend-function
	  'request-move-article gnus-newsgroup-name))
	(respool-method (gnus-find-method-for-group gnus-newsgroup-name))
	article mark todo deletep respool)

    (when (member 'respool groups)
      (setq respool t)			; boolean for later
      (setq groups '("fake"))) ; when respooling, groups are dynamic so fake it

    ;; now do the actual move
    (dolist (group groups)
      (when (and todo (stringp group))
	(dolist (article todo)
	  (when spam-mark-ham-unread-before-move-from-spam-group
	    (gnus-summary-mark-article article gnus-unread-mark))
	  (gnus-summary-set-process-mark article))

	(if respool			   ; respooling is with a "fake" group
	    (let ((spam-split-disabled
		   (or spam-split-disabled
		       spam-disable-spam-split-during-ham-respool)))
	      (gnus-summary-respool-article nil respool-method))
	  (if (or (not backend-supports-deletions) ; else, we are not respooling
		  (> (length groups) 1))
	      (progn		    ; if copying, copy and set deletep
		(gnus-summary-copy-article nil group)
		(setq deletep t))
	    (gnus-summary-move-article nil group))))) ; else move articles

    ;; now delete the articles, unless a) copy is t, and there was a copy done
    ;;                                 b) a move was done to a single group
    ;;                                 c) backend-supports-deletions is nil
    (unless copy
      (when (and deletep backend-supports-deletions)
	(dolist (article todo)
	  (gnus-summary-set-process-mark article))
	(when todo
	  (let ((gnus-novice-user nil))	; don't ask me if I'm sure
	    (gnus-summary-delete-article nil))))))

  (gnus-summary-yank-process-mark))

(defun spam-ham-copy-routine (&rest groups)
  (if (and (car-safe groups) (listp (car-safe groups)))
      (apply 'spam-ham-copy-routine (car groups))
    (spam-ham-copy-or-move-routine t groups)))

(defun spam-ham-move-routine (&rest groups)
  (if (and (car-safe groups) (listp (car-safe groups)))
      (apply 'spam-ham-move-routine (car groups))
    (spam-ham-copy-or-move-routine nil groups)))

(eval-and-compile
  (defalias 'spam-point-at-eol (if (fboundp 'point-at-eol)
				   'point-at-eol
				 'line-end-position)))

(defun spam-get-article-as-string (article)
  (let ((article-buffer (spam-get-article-as-buffer article))
	article-string)
    (when article-buffer
      (save-window-excursion
	(set-buffer article-buffer)
	(setq article-string (buffer-string))))
    article-string))

(defun spam-get-article-as-buffer (article)
  (let ((article-buffer))
    (when (numberp article)
      (save-window-excursion
	(gnus-summary-goto-subject article)
	(gnus-summary-show-article t)
	(setq article-buffer (get-buffer gnus-article-buffer))))
    article-buffer))

;; disabled for now
;; (defun spam-get-article-as-filename (article)
;;   (let ((article-filename))
;;     (when (numberp article)
;;       (nnml-possibly-change-directory
;;        (gnus-group-real-name gnus-newsgroup-name))
;;       (setq article-filename (expand-file-name
;;	  		        (int-to-string article) nnml-current-directory)))
;;     (if (file-exists-p article-filename)
;; 	article-filename
;;       nil)))

(defun spam-fetch-field-from-fast (article)
  "Fetch the `from' field quickly, using the internal gnus-data-list function"
  (if (and (numberp article)
	   (assoc article (gnus-data-list nil)))
      (mail-header-from
       (gnus-data-header (assoc article (gnus-data-list nil))))
    nil))

(defun spam-fetch-field-subject-fast (article)
  "Fetch the `subject' field quickly, using the internal
  gnus-data-list function"
  (if (and (numberp article)
	   (assoc article (gnus-data-list nil)))
      (mail-header-subject
       (gnus-data-header (assoc article (gnus-data-list nil))))
    nil))

(defun spam-fetch-field-message-id-fast (article)
  "Fetch the `Message-ID' field quickly, using the internal
  gnus-data-list function"
  (if (and (numberp article)
	   (assoc article (gnus-data-list nil)))
      (mail-header-message-id
       (gnus-data-header (assoc article (gnus-data-list nil))))
    nil))


;;;; Spam determination.

(defvar spam-list-of-checks
  '((spam-use-blacklist  	 . spam-check-blacklist)
    (spam-use-regex-headers  	 . spam-check-regex-headers)
    (spam-use-regex-body  	 . spam-check-regex-body)
    (spam-use-whitelist  	 . spam-check-whitelist)
    (spam-use-BBDB	 	 . spam-check-BBDB)
    (spam-use-ifile	 	 . spam-check-ifile)
    (spam-use-spamoracle         . spam-check-spamoracle)
    (spam-use-stat	 	 . spam-check-stat)
    (spam-use-blackholes 	 . spam-check-blackholes)
    (spam-use-hashcash  	 . spam-check-hashcash)
    (spam-use-bogofilter-headers . spam-check-bogofilter-headers)
    (spam-use-bogofilter 	 . spam-check-bogofilter))
  "The spam-list-of-checks list contains pairs associating a
parameter variable with a spam checking function.  If the
parameter variable is true, then the checking function is called,
and its value decides what happens.  Each individual check may
return nil, t, or a mailgroup name.  The value nil means that the
check does not yield a decision, and so, that further checks are
needed.  The value t means that the message is definitely not
spam, and that further spam checks should be inhibited.
Otherwise, a mailgroup name or the symbol 'spam (depending on
spam-split-symbolic-return) is returned where the mail should go,
and further checks are also inhibited.  The usual mailgroup name
is the value of `spam-split-group', meaning that the message is
definitely a spam.")

(defvar spam-list-of-statistical-checks
  '(spam-use-ifile
    spam-use-regex-body
    spam-use-stat
    spam-use-bogofilter
    spam-use-spamoracle)
  "The spam-list-of-statistical-checks list contains all the mail
splitters that need to have the full message body available.")

;;;TODO: modify to invoke self with each check if invoked without specifics
(defun spam-split (&rest specific-checks)
  "Split this message into the `spam' group if it is spam.
This function can be used as an entry in the variable `nnmail-split-fancy',
for example like this: (: spam-split).  It can take checks as
parameters.  A string as a parameter will set the
spam-split-group to that string.

See the Info node `(gnus)Fancy Mail Splitting' for more details."
  (interactive)
  (setq spam-split-last-successful-check nil)
  (unless spam-split-disabled
    (let ((spam-split-group-choice spam-split-group))
      (dolist (check specific-checks)
	(when (stringp check)
	  (setq spam-split-group-choice check)
	  (setq specific-checks (delq check specific-checks))))

      (let ((spam-split-group spam-split-group-choice))
	(save-excursion
	  (save-restriction
	    (dolist (check spam-list-of-statistical-checks)
	      (when (and (symbolp check) (symbol-value check))
		(widen)
		(gnus-message 8 "spam-split: widening the buffer (%s requires it)"
			      (symbol-name check))
		(return)))
	    ;;   (progn (widen) (debug (buffer-string)))
	    (let ((list-of-checks spam-list-of-checks)
		  decision)
	      (while (and list-of-checks (not decision))
		(let ((pair (pop list-of-checks)))
		  (when (and (symbol-value (car pair))
			     (or (null specific-checks)
				 (memq (car pair) specific-checks)))
		    (gnus-message 5 "spam-split: calling the %s function"
				  (symbol-name (cdr pair)))
		    (setq decision (funcall (cdr pair)))
		    ;; if we got a decision at all, save the current check
		    (when decision
		      (setq spam-split-last-successful-check (car pair)))

		    (when (eq decision 'spam)
		      (if spam-split-symbolic-return
			  (setq decision spam-split-group)
			(gnus-error
			 5
			 (format "spam-split got %s but %s is nil"
				 (symbol-name decision)
				 (symbol-name spam-split-symbolic-return))))))))
	      (if (eq decision t)
		  (if spam-split-symbolic-return-positive 'ham nil)
		decision))))))))

(defun spam-find-spam ()
  "This function will detect spam in the current newsgroup using spam-split."
  (interactive)

  (let* ((group gnus-newsgroup-name)
	 (autodetect (gnus-parameter-spam-autodetect group))
	 (methods (gnus-parameter-spam-autodetect-methods group))
	 (first-method (nth 0 methods)))
  (when (and autodetect
	     (not (equal first-method 'none)))
    (mapcar
     (lambda (article)
       (let ((id (spam-fetch-field-message-id-fast article))
	     (subject (spam-fetch-field-subject-fast article))
	     (sender (spam-fetch-field-from-fast article)))
	 (unless (and spam-log-to-registry
		      (spam-log-registered-p id 'incoming))
	   (let* ((spam-split-symbolic-return t)
		  (spam-split-symbolic-return-positive t)
		  (split-return
		   (with-temp-buffer
		     (gnus-request-article-this-buffer
		      article
		      group)
		     (if (or (null first-method)
			     (equal first-method 'default))
			 (spam-split)
		       (apply 'spam-split methods)))))
	     (if (equal split-return 'spam)
		 (gnus-summary-mark-article article gnus-spam-mark))

	     (when (and split-return spam-log-to-registry)
	       (when (zerop (gnus-registry-group-count id))
		 (gnus-registry-add-group
		  id group subject sender))

	       (spam-log-processing-to-registry
		id
		'incoming
		split-return
		spam-split-last-successful-check
		group))))))
     (if spam-autodetect-recheck-messages
	 gnus-newsgroup-articles
       gnus-newsgroup-unseen)))))

(defvar spam-registration-functions
  ;; first the ham register, second the spam register function
  ;; third the ham unregister, fourth the spam unregister function
  '((spam-use-blacklist  nil
			 spam-blacklist-register-routine
			 nil
			 spam-blacklist-unregister-routine)
    (spam-use-whitelist  spam-whitelist-register-routine
			 nil
			 spam-whitelist-unregister-routine
			 nil)
    (spam-use-BBDB	 spam-BBDB-register-routine
			 nil
			 spam-BBDB-unregister-routine
			 nil)
    (spam-use-ifile	 spam-ifile-register-ham-routine
			 spam-ifile-register-spam-routine
			 spam-ifile-unregister-ham-routine
			 spam-ifile-unregister-spam-routine)
    (spam-use-spamoracle spam-spamoracle-learn-ham
			 spam-spamoracle-learn-spam
			 spam-spamoracle-unlearn-ham
			 spam-spamoracle-unlearn-spam)
    (spam-use-stat	 spam-stat-register-ham-routine
			 spam-stat-register-spam-routine
			 spam-stat-unregister-ham-routine
			 spam-stat-unregister-spam-routine)
    ;; note that spam-use-gmane is not a legitimate check
    (spam-use-gmane      nil
			 spam-report-gmane-register-routine
			 ;; does Gmane support unregistration?
			 nil
			 nil)
    (spam-use-bogofilter spam-bogofilter-register-ham-routine
			 spam-bogofilter-register-spam-routine
			 spam-bogofilter-unregister-ham-routine
			 spam-bogofilter-unregister-spam-routine))
  "The spam-registration-functions list contains pairs
associating a parameter variable with the ham and spam
registration functions, and the ham and spam unregistration
functions")

(defun spam-classification-valid-p (classification)
  (or  (eq classification 'spam)
       (eq classification 'ham)))

(defun spam-process-type-valid-p (process-type)
  (or  (eq process-type 'incoming)
       (eq process-type 'process)))

(defun spam-registration-check-valid-p (check)
  (assoc check spam-registration-functions))

(defun spam-unregistration-check-valid-p (check)
  (assoc check spam-registration-functions))

(defun spam-registration-function (classification check)
  (let ((flist (cdr-safe (assoc check spam-registration-functions))))
    (if (eq classification 'spam)
	(nth 1 flist)
      (nth 0 flist))))

(defun spam-unregistration-function (classification check)
  (let ((flist (cdr-safe (assoc check spam-registration-functions))))
    (if (eq classification 'spam)
	(nth 3 flist)
      (nth 2 flist))))

(defun spam-list-articles (articles classification)
  (let ((mark-check (if (eq classification 'spam)
			'spam-group-spam-mark-p
		      'spam-group-ham-mark-p))
	list mark-cache-yes mark-cache-no)
    (dolist (article articles)
      (let ((mark (gnus-summary-article-mark article)))
	(unless (memq mark mark-cache-no)
	  (if (memq mark mark-cache-yes)
	      (push article list)
	    ;; else, we have to actually check the mark
	    (if (funcall mark-check
			 gnus-newsgroup-name
			 mark)
		(progn
		  (push article list)
		  (push mark mark-cache-yes))
	      (push mark mark-cache-no))))))
    list))

(defun spam-register-routine (classification
			      check
			      &optional unregister
			      specific-articles)
  (when (and (spam-classification-valid-p classification)
	     (spam-registration-check-valid-p check))
    (let* ((register-function
	    (spam-registration-function classification check))
	   (unregister-function
	    (spam-unregistration-function classification check))
	   (run-function (if unregister
			     unregister-function
			   register-function))
	   (log-function (if unregister
			     'spam-log-undo-registration
			   'spam-log-processing-to-registry))
	   article articles)

      (when run-function
	;; make list of articles, using specific-articles if given
	(setq articles (or specific-articles
			   (spam-list-articles
			    gnus-newsgroup-articles
			    classification)))
	;; process them
	(gnus-message 5 "%s %d %s articles with classification %s, check %s"
		      (if unregister "Unregistering" "Registering")
		      (length articles)
		      (if specific-articles "specific" "")
		      (symbol-name classification)
		      (symbol-name check))
	(funcall run-function articles)
	;; now log all the registrations (or undo them, depending on unregister)
	(dolist (article articles)
	  (funcall log-function
		   (spam-fetch-field-message-id-fast article)
		   'process
		   classification
		   check
		   gnus-newsgroup-name))))))

;;; log a ham- or spam-processor invocation to the registry
(defun spam-log-processing-to-registry (id type classification check group)
  (when spam-log-to-registry
    (if (and (stringp id)
	     (stringp group)
	     (spam-process-type-valid-p type)
	     (spam-classification-valid-p classification)
	     (spam-registration-check-valid-p check))
	(let ((cell-list (cdr-safe (gnus-registry-fetch-extra id type)))
	      (cell (list classification check group)))
	  (push cell cell-list)
	  (gnus-registry-store-extra-entry
	   id
	   type
	   cell-list))

      (gnus-message 5 (format "%s called with bad ID, type, classification, check, or group"
			      "spam-log-processing-to-registry")))))

;;; check if a ham- or spam-processor registration has been done
(defun spam-log-registered-p (id type)
  (when spam-log-to-registry
    (if (and (stringp id)
	     (spam-process-type-valid-p type))
	(cdr-safe (gnus-registry-fetch-extra id type))
      (progn
	(gnus-message 5 (format "%s called with bad ID, type, classification, or check"
				"spam-log-registered-p"))
	nil))))

;;; check if a ham- or spam-processor registration needs to be undone
(defun spam-log-unregistration-needed-p (id type classification check)
  (when spam-log-to-registry
    (if (and (stringp id)
	     (spam-process-type-valid-p type)
	     (spam-classification-valid-p classification)
	     (spam-registration-check-valid-p check))
	(let ((cell-list (cdr-safe (gnus-registry-fetch-extra id type)))
	      found)
	  (dolist (cell cell-list)
	    (unless found
	      (when (and (eq classification (nth 0 cell))
			 (eq check (nth 1 cell)))
		(setq found t))))
	  found)
      (progn
	(gnus-message 5 (format "%s called with bad ID, type, classification, or check"
				"spam-log-unregistration-needed-p"))
	nil))))


;;; undo a ham- or spam-processor registration (the group is not used)
(defun spam-log-undo-registration (id type classification check &optional group)
  (when (and spam-log-to-registry
	     (spam-log-unregistration-needed-p id type classification check))
    (if (and (stringp id)
	     (spam-process-type-valid-p type)
	     (spam-classification-valid-p classification)
	     (spam-registration-check-valid-p check))
	(let ((cell-list (cdr-safe (gnus-registry-fetch-extra id type)))
	      new-cell-list found)
	  (dolist (cell cell-list)
	    (unless (and (eq classification (nth 0 cell))
			 (eq check (nth 1 cell)))
	      (push cell new-cell-list)))
	  (gnus-registry-store-extra-entry
	   id
	   type
	   new-cell-list))
      (progn
	(gnus-message 5 (format "%s called with bad ID, type, check, or group"
				"spam-log-undo-registration"))
	nil))))

;;; set up IMAP widening if it's necessary
(defun spam-setup-widening ()
  (dolist (check spam-list-of-statistical-checks)
    (when (symbol-value check)
      (setq nnimap-split-download-body-default t))))


;;;; Regex body

(defun spam-check-regex-body ()
  (let ((spam-regex-headers-ham spam-regex-body-ham)
	(spam-regex-headers-spam spam-regex-body-spam))
    (spam-check-regex-headers t)))


;;;; Regex headers

(defun spam-check-regex-headers (&optional body)
  (let ((type (if body "body" "header"))
	(spam-split-group (if spam-split-symbolic-return
			      'spam
			    spam-split-group))
	ret found)
    (dolist (h-regex spam-regex-headers-ham)
      (unless found
	(goto-char (point-min))
	(when (re-search-forward h-regex nil t)
	  (message "Ham regex %s search positive." type)
	  (setq found t))))
    (dolist (s-regex spam-regex-headers-spam)
      (unless found
	(goto-char (point-min))
	(when (re-search-forward s-regex nil t)
	  (message "Spam regex %s search positive." type)
	  (setq found t)
	  (setq ret spam-split-group))))
    ret))


;;;; Blackholes.

(defun spam-reverse-ip-string (ip)
  (when (stringp ip)
    (mapconcat 'identity
	       (nreverse (split-string ip "\\."))
	       ".")))

(defun spam-check-blackholes ()
  "Check the Received headers for blackholed relays."
  (let ((headers (nnmail-fetch-field "received"))
	(spam-split-group (if spam-split-symbolic-return
			      'spam
			    spam-split-group))
	ips matches)
    (when headers
      (with-temp-buffer
	(insert headers)
	(goto-char (point-min))
	(gnus-message 5 "Checking headers for relay addresses")
	(while (re-search-forward
		"\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\)" nil t)
	  (gnus-message 9 "Blackhole search found host IP %s." (match-string 1))
	  (push (spam-reverse-ip-string (match-string 1))
		ips)))
      (dolist (server spam-blackhole-servers)
	(dolist (ip ips)
	  (unless (and spam-blackhole-good-server-regex
		       ;; match the good-server-regex against the reversed (again) IP string
		       (string-match
			spam-blackhole-good-server-regex
			(spam-reverse-ip-string ip)))
	    (unless matches
	      (let ((query-string (concat ip "." server)))
		(if spam-use-dig
		    (let ((query-result (query-dig query-string)))
		      (when query-result
			(gnus-message 5 "(DIG): positive blackhole check '%s'"
				      query-result)
			(push (list ip server query-result)
			      matches)))
		  ;; else, if not using dig.el
		  (when (query-dns query-string)
		    (gnus-message 5 "positive blackhole check")
		    (push (list ip server (query-dns query-string 'TXT))
			  matches)))))))))
    (when matches
      spam-split-group)))

;;;; Hashcash.

(eval-when-compile
  (autoload 'mail-check-payment "hashcash"))

(condition-case nil
    (progn
      (require 'hashcash)

      (defun spam-check-hashcash ()
	"Check the headers for hashcash payments."
	(mail-check-payment)))	 ;mail-check-payment returns a boolean

  (file-error))

;;;; BBDB

;;; original idea for spam-check-BBDB from Alexander Kotelnikov
;;; <sacha@giotto.sj.ru>

;; all this is done inside a condition-case to trap errors

(eval-when-compile
  (autoload 'bbdb-buffer "bbdb")
  (autoload 'bbdb-create-internal "bbdb")
  (autoload 'bbdb-search-simple "bbdb"))

(eval-and-compile
  (when (condition-case nil
	    (progn
	      (require 'bbdb)
	      (require 'bbdb-com))
	  (file-error
	   (defalias 'spam-BBDB-register-routine 'ignore)
	   (defalias 'spam-enter-ham-BBDB 'ignore)
	   nil))

    (defun spam-enter-ham-BBDB (addresses &optional remove)
      "Enter an address into the BBDB; implies ham (non-spam) sender"
      (dolist (from addresses)
	(when (stringp from)
	  (let* ((parsed-address (gnus-extract-address-components from))
		 (name (or (nth 0 parsed-address) "Ham Sender"))
		 (remove-function (if remove
				      'bbdb-delete-record-internal
				    'ignore))
		 (net-address (nth 1 parsed-address))
		 (record (and net-address
			      (bbdb-search-simple nil net-address))))
	    (when net-address
	      (gnus-message 5 "%s address %s %s BBDB"
			    (if remove "Deleting" "Adding")
			    from
			    (if remove "from" "to"))
	      (if record
		  (funcall remove-function record)
		(bbdb-create-internal name nil net-address nil nil
				      "ham sender added by spam.el")))))))

    (defun spam-BBDB-register-routine (articles &optional unregister)
      (let (addresses)
	(dolist (article articles)
	  (when (stringp (spam-fetch-field-from-fast article))
	    (push (spam-fetch-field-from-fast article) addresses)))
	;; now do the register/unregister action
	(spam-enter-ham-BBDB addresses unregister)))

    (defun spam-BBDB-unregister-routine (articles)
      (spam-BBDB-register-routine articles t))

    (defun spam-check-BBDB ()
      "Mail from people in the BBDB is classified as ham or non-spam"
      (let ((who (nnmail-fetch-field "from"))
	    (spam-split-group (if spam-split-symbolic-return
				  'spam
				spam-split-group)))
	(when who
	  (setq who (nth 1 (gnus-extract-address-components who)))
	  (if (bbdb-search-simple nil who)
	      t
	    (if spam-use-BBDB-exclusive
		spam-split-group
	      nil)))))))


;;;; ifile

;;; check the ifile backend; return nil if the mail was NOT classified
;;; as spam

(defun spam-get-ifile-database-parameter ()
  "Get the command-line parameter for ifile's database from
  spam-ifile-database-path."
  (if spam-ifile-database-path
      (format "--db-file=%s" spam-ifile-database-path)
    nil))

(defun spam-check-ifile ()
  "Check the ifile backend for the classification of this message."
  (let ((article-buffer-name (buffer-name))
	(spam-split-group (if spam-split-symbolic-return
			      'spam
			    spam-split-group))
	category return)
    (with-temp-buffer
      (let ((temp-buffer-name (buffer-name))
	    (db-param (spam-get-ifile-database-parameter)))
	(save-excursion
	  (set-buffer article-buffer-name)
	  (apply 'call-process-region
		 (point-min) (point-max) spam-ifile-path
		 nil temp-buffer-name nil "-c"
		 (if db-param `(,db-param "-q") `("-q"))))
	;; check the return now (we're back in the temp buffer)
	(goto-char (point-min))
	(if (not (eobp))
	    (setq category (buffer-substring (point) (spam-point-at-eol))))
	(when (not (zerop (length category))) ; we need a category here
	  (if spam-ifile-all-categories
	      (setq return category)
	    ;; else, if spam-ifile-all-categories is not set...
	    (when (string-equal spam-ifile-spam-category category)
	      (setq return spam-split-group)))))) ; note return is nil otherwise
    return))

(defun spam-ifile-register-with-ifile (articles category &optional unregister)
  "Register an article, given as a string, with a category.
Uses `gnus-newsgroup-name' if category is nil (for ham registration)."
  (let ((category (or category gnus-newsgroup-name))
	(add-or-delete-option (if unregister "-d" "-i"))
	(db (spam-get-ifile-database-parameter))
	parameters)
    (with-temp-buffer
      (dolist (article articles)
	(let ((article-string (spam-get-article-as-string article)))
	  (when (stringp article-string)
	    (insert article-string))))
      (apply 'call-process-region
	     (point-min) (point-max) spam-ifile-path
	     nil nil nil
	     add-or-delete-option category
	     (if db `(,db "-h") `("-h"))))))

(defun spam-ifile-register-spam-routine (articles &optional unregister)
  (spam-ifile-register-with-ifile articles spam-ifile-spam-category unregister))

(defun spam-ifile-unregister-spam-routine (articles)
  (spam-ifile-register-spam-routine articles t))

(defun spam-ifile-register-ham-routine (articles &optional unregister)
  (spam-ifile-register-with-ifile articles spam-ifile-ham-category unregister))

(defun spam-ifile-unregister-ham-routine (articles)
  (spam-ifile-register-ham-routine articles t))


;;;; spam-stat

(eval-when-compile
  (autoload 'spam-stat-buffer-change-to-non-spam "spam-stat")
  (autoload 'spam-stat-buffer-change-to-spam "spam-stat")
  (autoload 'spam-stat-buffer-is-non-spam "spam-stat")
  (autoload 'spam-stat-buffer-is-spam "spam-stat")
  (autoload 'spam-stat-load "spam-stat")
  (autoload 'spam-stat-save "spam-stat")
  (autoload 'spam-stat-split-fancy "spam-stat"))

(eval-and-compile
  (when (condition-case nil
	    (let ((spam-stat-install-hooks nil))
	      (require 'spam-stat))
	  (file-error
	   (defalias 'spam-stat-register-ham-routine 'ignore)
	   (defalias 'spam-stat-register-spam-routine 'ignore)
	   nil))

    (defun spam-check-stat ()
      "Check the spam-stat backend for the classification of this message"
      (let ((spam-split-group (if spam-split-symbolic-return
				  'spam
				spam-split-group))
	    (spam-stat-split-fancy-spam-group spam-split-group) ; override
	    (spam-stat-buffer (buffer-name)) ; stat the current buffer
	    category return)
	(spam-stat-split-fancy)))

    (defun spam-stat-register-spam-routine (articles &optional unregister)
      (dolist (article articles)
	(let ((article-string (spam-get-article-as-string article)))
	  (with-temp-buffer
	    (insert article-string)
	    (if unregister
		(spam-stat-buffer-change-to-non-spam)
	      (spam-stat-buffer-is-spam))))))

    (defun spam-stat-unregister-spam-routine (articles)
      (spam-stat-register-spam-routine articles t))

    (defun spam-stat-register-ham-routine (articles &optional unregister)
      (dolist (article articles)
	(let ((article-string (spam-get-article-as-string article)))
	  (with-temp-buffer
	    (insert article-string)
	    (if unregister
		(spam-stat-buffer-change-to-spam)
	      (spam-stat-buffer-is-non-spam))))))

    (defun spam-stat-unregister-ham-routine (articles)
      (spam-stat-register-ham-routine articles t))

    (defun spam-maybe-spam-stat-load ()
      (when spam-use-stat (spam-stat-load)))

    (defun spam-maybe-spam-stat-save ()
      (when spam-use-stat (spam-stat-save)))))



;;;; Blacklists and whitelists.

(defvar spam-whitelist-cache nil)
(defvar spam-blacklist-cache nil)

(defun spam-kill-whole-line ()
  (beginning-of-line)
  (let ((kill-whole-line t))
    (kill-line)))

;;; address can be a list, too
(defun spam-enter-whitelist (address &optional remove)
  "Enter ADDRESS (list or single) into the whitelist.
With a non-nil REMOVE, remove them."
  (interactive "sAddress: ")
  (spam-enter-list address spam-whitelist remove)
  (setq spam-whitelist-cache nil))

;;; address can be a list, too
(defun spam-enter-blacklist (address &optional remove)
  "Enter ADDRESS (list or single) into the blacklist.
With a non-nil REMOVE, remove them."
  (interactive "sAddress: ")
  (spam-enter-list address spam-blacklist remove)
  (setq spam-blacklist-cache nil))

(defun spam-enter-list (addresses file &optional remove)
  "Enter ADDRESSES into the given FILE.
Either the whitelist or the blacklist files can be used.  With
REMOVE not nil, remove the ADDRESSES."
  (if (stringp addresses)
      (spam-enter-list (list addresses) file remove)
    ;; else, we have a list of addresses here
    (unless (file-exists-p (file-name-directory file))
      (make-directory (file-name-directory file) t))
    (save-excursion
      (set-buffer
       (find-file-noselect file))
      (dolist (a addresses)
	(when (stringp a)
	  (goto-char (point-min))
	  (if (re-search-forward (regexp-quote a) nil t)
	      ;; found the address
	      (when remove
		(spam-kill-whole-line))
	    ;; else, the address was not found
	    (unless remove
	      (goto-char (point-max))
	      (unless (bobp)
		(insert "\n"))
	      (insert a "\n")))))
      (save-buffer))))

;;; returns t if the sender is in the whitelist, nil or
;;; spam-split-group otherwise
(defun spam-check-whitelist ()
  ;; FIXME!  Should it detect when file timestamps change?
  (let ((spam-split-group (if spam-split-symbolic-return
			      'spam
			    spam-split-group)))
    (unless spam-whitelist-cache
      (setq spam-whitelist-cache (spam-parse-list spam-whitelist)))
    (if (spam-from-listed-p spam-whitelist-cache)
	t
      (if spam-use-whitelist-exclusive
	  spam-split-group
	nil))))

(defun spam-check-blacklist ()
  ;; FIXME!  Should it detect when file timestamps change?
  (let ((spam-split-group (if spam-split-symbolic-return
			      'spam
			    spam-split-group)))
    (unless spam-blacklist-cache
      (setq spam-blacklist-cache (spam-parse-list spam-blacklist)))
    (and (spam-from-listed-p spam-blacklist-cache) spam-split-group)))

(defun spam-parse-list (file)
  (when (file-readable-p file)
    (let (contents address)
      (with-temp-buffer
	(insert-file-contents file)
	(while (not (eobp))
	  (setq address (buffer-substring (point) (spam-point-at-eol)))
	  (forward-line 1)
	  ;; insert the e-mail address if detected, otherwise the raw data
	  (unless (zerop (length address))
	    (let ((pure-address (nth 1 (gnus-extract-address-components address))))
	      (push (or pure-address address) contents)))))
      (nreverse contents))))

(defun spam-from-listed-p (cache)
  (let ((from (nnmail-fetch-field "from"))
	found)
    (while cache
      (let ((address (pop cache)))
	(unless (zerop (length address)) ; 0 for a nil address too
	  (setq address (regexp-quote address))
	  ;; fix regexp-quote's treatment of user-intended regexes
	  (while (string-match "\\\\\\*" address)
	    (setq address (replace-match ".*" t t address))))
	(when (and address (string-match address from))
	  (setq found t
		cache nil))))
    found))

(defun spam-filelist-register-routine (articles blacklist &optional unregister)
  (let ((de-symbol (if blacklist 'spam-use-whitelist 'spam-use-blacklist))
	(declassification (if blacklist 'ham 'spam))
	(enter-function
	 (if blacklist 'spam-enter-blacklist 'spam-enter-whitelist))
	(remove-function
	 (if blacklist 'spam-enter-whitelist 'spam-enter-blacklist))
	from addresses unregister-list)
    (dolist (article articles)
      (let ((from (spam-fetch-field-from-fast article))
	    (id (spam-fetch-field-message-id-fast article))
	    sender-ignored)
	(when (stringp from)
	  (dolist (ignore-regex spam-blacklist-ignored-regexes)
	    (when (and (not sender-ignored)
		       (stringp ignore-regex)
		       (string-match ignore-regex from))
	      (setq sender-ignored t)))
	  ;; remember the messages we need to unregister, unless remove is set
	  (when (and
		 (null unregister)
		 (spam-log-unregistration-needed-p
		  id 'process declassification de-symbol))
	    (push from unregister-list))
	  (unless sender-ignored
	    (push from addresses)))))

    (if unregister
	(funcall enter-function addresses t) ; unregister all these addresses
      ;; else, register normally and unregister what we need to
      (funcall remove-function unregister-list t)
      (dolist (article unregister-list)
	(spam-log-undo-registration
	 (spam-fetch-field-message-id-fast article)
	 'process
	 declassification
	 de-symbol))
      (funcall enter-function addresses nil))))

(defun spam-blacklist-unregister-routine (articles)
  (spam-blacklist-register-routine articles t))

(defun spam-blacklist-register-routine (articles &optional unregister)
  (spam-filelist-register-routine articles t unregister))

(defun spam-whitelist-unregister-routine (articles)
  (spam-whitelist-register-routine articles t))

(defun spam-whitelist-register-routine (articles &optional unregister)
  (spam-filelist-register-routine articles nil unregister))


;;;; Spam-report glue
(defun spam-report-gmane-register-routine (articles)
  (when articles
    (apply 'spam-report-gmane articles)))


;;;; Bogofilter
(defun spam-check-bogofilter-headers (&optional score)
  (let ((header (nnmail-fetch-field spam-bogofilter-header))
	(spam-split-group (if spam-split-symbolic-return
			      'spam
			    spam-split-group)))
    (when header			; return nil when no header
      (if score				; scoring mode
	  (if (string-match "spamicity=\\([0-9.]+\\)" header)
	      (match-string 1 header)
	    "0")
	;; spam detection mode
	(when (string-match spam-bogofilter-bogosity-positive-spam-header
			    header)
	  spam-split-group)))))

;; return something sensible if the score can't be determined
(defun spam-bogofilter-score ()
  "Get the Bogofilter spamicity score"
  (interactive)
  (save-window-excursion
    (gnus-summary-show-article t)
    (set-buffer gnus-article-buffer)
    (let ((score (or (spam-check-bogofilter-headers t)
		     (spam-check-bogofilter t))))
      (message "Spamicity score %s" score)
      (or score "0"))
    (gnus-summary-show-article)))

(defun spam-check-bogofilter (&optional score)
  "Check the Bogofilter backend for the classification of this message"
  (let ((article-buffer-name (buffer-name))
	(db spam-bogofilter-database-directory)
	return)
    (with-temp-buffer
      (let ((temp-buffer-name (buffer-name)))
	(save-excursion
	  (set-buffer article-buffer-name)
	  (apply 'call-process-region
		 (point-min) (point-max)
		 spam-bogofilter-path
		 nil temp-buffer-name nil
		 (if db `("-d" ,db "-v") `("-v"))))
	(setq return (spam-check-bogofilter-headers score))))
    return))

(defun spam-bogofilter-register-with-bogofilter (articles
						 spam
						 &optional unregister)
  "Register an article, given as a string, as spam or non-spam."
  (dolist (article articles)
    (let ((article-string (spam-get-article-as-string article))
	  (db spam-bogofilter-database-directory)
	  (switch (if unregister
		      (if spam
			  spam-bogofilter-spam-strong-switch
			spam-bogofilter-ham-strong-switch)
		    (if spam
			spam-bogofilter-spam-switch
		      spam-bogofilter-ham-switch))))
      (when (stringp article-string)
	(with-temp-buffer
	  (insert article-string)

	  (apply 'call-process-region
		 (point-min) (point-max)
		 spam-bogofilter-path
		 nil nil nil switch
		 (if db `("-d" ,db "-v") `("-v"))))))))

(defun spam-bogofilter-register-spam-routine (articles &optional unregister)
  (spam-bogofilter-register-with-bogofilter articles t unregister))

(defun spam-bogofilter-unregister-spam-routine (articles)
  (spam-bogofilter-register-spam-routine articles t))

(defun spam-bogofilter-register-ham-routine (articles &optional unregister)
  (spam-bogofilter-register-with-bogofilter articles nil unregister))

(defun spam-bogofilter-unregister-ham-routine (articles)
  (spam-bogofilter-register-ham-routine articles t))



;;;; spamoracle
(defun spam-check-spamoracle ()
  "Run spamoracle on an article to determine whether it's spam."
  (let ((article-buffer-name (buffer-name))
	(spam-split-group (if spam-split-symbolic-return
			      'spam
			    spam-split-group)))
    (with-temp-buffer
      (let ((temp-buffer-name (buffer-name)))
	(save-excursion
	  (set-buffer article-buffer-name)
	  (let ((status
		 (apply 'call-process-region
			(point-min) (point-max)
			spam-spamoracle-binary
			nil temp-buffer-name nil
			(if spam-spamoracle-database
			    `("-f" ,spam-spamoracle-database "mark")
			  '("mark")))))
	    (if (eq 0 status)
		(progn
		  (set-buffer temp-buffer-name)
		  (goto-char (point-min))
		  (when (re-search-forward "^X-Spam: yes;" nil t)
		    spam-split-group))
	      (error "Error running spamoracle: %s" status))))))))

(defun spam-spamoracle-learn (articles article-is-spam-p &optional unregister)
  "Run spamoracle in training mode."
  (with-temp-buffer
    (let ((temp-buffer-name (buffer-name)))
      (save-excursion
	(goto-char (point-min))
	(dolist (article articles)
	  (insert (spam-get-article-as-string article)))
	(let* ((arg (if (spam-xor unregister article-is-spam-p)
			"-spam"
		      "-good"))
	       (status
		(apply 'call-process-region
		       (point-min) (point-max)
		       spam-spamoracle-binary
		       nil temp-buffer-name nil
		       (if spam-spamoracle-database
			   `("-f" ,spam-spamoracle-database
			     "add" ,arg)
			 `("add" ,arg)))))
	  (unless (eq 0 status)
	    (error "Error running spamoracle: %s" status)))))))

(defun spam-spamoracle-learn-ham (articles &optional unregister)
  (spam-spamoracle-learn articles nil unregister))

(defun spam-spamoracle-unlearn-ham (articles &optional unregister)
  (spam-spamoracle-learn-ham articles t))

(defun spam-spamoracle-learn-spam (articles &optional unregister)
  (spam-spamoracle-learn articles t unregister))

(defun spam-spamoracle-unlearn-spam (articles &optional unregister)
  (spam-spamoracle-learn-spam articles t))


;;;; Hooks

;;;###autoload
(defun spam-initialize ()
  "Install the spam.el hooks and do other initialization"
  (interactive)
  (setq spam-install-hooks t)
  ;; TODO: How do we redo this every time the `spam' face is customized?
  (push '((eq mark gnus-spam-mark) . spam)
	gnus-summary-highlight)
  ;; Add hooks for loading and saving the spam stats
  (add-hook 'gnus-save-newsrc-hook 'spam-maybe-spam-stat-save)
  (add-hook 'gnus-get-top-new-news-hook 'spam-maybe-spam-stat-load)
  (add-hook 'gnus-startup-hook 'spam-maybe-spam-stat-load)
  (add-hook 'gnus-summary-prepare-exit-hook 'spam-summary-prepare-exit)
  (add-hook 'gnus-summary-prepare-hook 'spam-summary-prepare)
  (add-hook 'gnus-get-new-news-hook 'spam-setup-widening)
  (add-hook 'gnus-summary-prepare-hook 'spam-find-spam))

(defun spam-unload-hook ()
  "Uninstall the spam.el hooks"
  (interactive)
  (remove-hook 'gnus-save-newsrc-hook 'spam-maybe-spam-stat-save)
  (remove-hook 'gnus-get-top-new-news-hook 'spam-maybe-spam-stat-load)
  (remove-hook 'gnus-startup-hook 'spam-maybe-spam-stat-load)
  (remove-hook 'gnus-summary-prepare-exit-hook 'spam-summary-prepare-exit)
  (remove-hook 'gnus-summary-prepare-hook 'spam-summary-prepare)
  (remove-hook 'gnus-get-new-news-hook 'spam-setup-widening)
  (remove-hook 'gnus-summary-prepare-hook 'spam-find-spam))

(add-hook 'spam-unload-hook 'spam-unload-hook)

(when spam-install-hooks
  (spam-initialize))

(provide 'spam)

;;; arch-tag: 07e6e0ca-ab0a-4412-b445-1f6c72a4f27f
;;; spam.el ends here
