;;; nnmail.el --- mail support functions for the Gnus mail backends
;; Copyright (C) 1995,96,97 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; Keywords: news, mail

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

(require 'nnheader)
(require 'timezone)
(require 'message)
(require 'custom)

(eval-and-compile
  (autoload 'gnus-error "gnus-util"))

(defgroup nnmail nil
  "Reading mail with Gnus."
  :group 'gnus)

(defgroup nnmail-retrieve nil
  "Retrieving new mail."
  :group 'nnmail)

(defgroup nnmail-prepare nil
  "Preparing (or mangling) new mail after retrival."
  :group 'nnmail)

(defgroup nnmail-duplicate nil
  "Handling of duplicate mail messages."
  :group 'nnmail)

(defgroup nnmail-split nil
  "Organizing the incomming mail in folders."
  :group 'nnmail)

(defgroup nnmail-files nil
  "Mail files."
  :group 'gnus-files
  :group 'nnmail)

(defgroup nnmail-expire nil
  "Expiring old mail."
  :group 'nnmail)

(defgroup nnmail-procmail nil
  "Interfacing with procmail and other mail agents."
  :group 'nnmail)

(defgroup nnmail-various nil
  "Various mail options."
  :group 'nnmail)

(defcustom nnmail-split-methods
  '(("mail.misc" ""))
  "Incoming mail will be split according to this variable.

If you'd like, for instance, one mail group for mail from the
\"4ad-l\" mailing list, one group for junk mail and one for everything
else, you could do something like this:

 (setq nnmail-split-methods
       '((\"mail.4ad\" \"From:.*4ad\")
         (\"mail.junk\" \"From:.*Lars\\\\|Subject:.*buy\")
         (\"mail.misc\" \"\")))

As you can see, this variable is a list of lists, where the first
element in each \"rule\" is the name of the group (which, by the way,
does not have to be called anything beginning with \"mail\",
\"yonka.zow\" is a fine, fine name), and the second is a regexp that
nnmail will try to match on the header to find a fit.

The second element can also be a function.  In that case, it will be
called narrowed to the headers with the first element of the rule as
the argument.  It should return a non-nil value if it thinks that the
mail belongs in that group.

The last element should always have \"\" as the regexp.

This variable can also have a function as its value."
  :group 'nnmail-split
  :type '(choice (repeat :tag "Alist" (group (string :tag "Name") regexp))
		 (function-item nnmail-split-fancy)
		 (function :tag "Other")))

;; Suggested by Erik Selberg <speed@cs.washington.edu>.
(defcustom nnmail-crosspost t
  "If non-nil, do crossposting if several split methods match the mail.
If nil, the first match found will be used."
  :group 'nnmail-split
  :type 'boolean)

;; Added by gord@enci.ucalgary.ca (Gordon Matzigkeit).
(defcustom nnmail-keep-last-article nil
  "If non-nil, nnmail will never delete/move a group's last article.
It can be marked expirable, so it will be deleted when it is no longer last.

You may need to set this variable if other programs are putting
new mail into folder numbers that Gnus has marked as expired."
  :group 'nnmail-procmail
  :group 'nnmail-various
  :type 'boolean)

(defcustom nnmail-use-long-file-names nil
  "If non-nil the mail backends will use long file and directory names.
If nil, groups like \"mail.misc\" will end up in directories like
\"mail/misc/\"."
  :group 'nnmail-files
  :type 'boolean)

(defcustom nnmail-default-file-modes 384
  "Set the mode bits of all new mail files to this integer."
  :group 'nnmail-files
  :type 'integer)

(defcustom nnmail-expiry-wait 7
  "*Expirable articles that are older than this will be expired.
This variable can either be a number (which will be interpreted as a
number of days) -- this doesn't have to be an integer.  This variable
can also be `immediate' and `never'."
  :group 'nnmail-expire
  :type '(choice (const immediate)
		 (integer :tag "days")
		 (const never)))

(defcustom nnmail-expiry-wait-function nil
  "Variable that holds function to specify how old articles should be before they are expired.
  The function will be called with the name of the group that the
expiry is to be performed in, and it should return an integer that
says how many days an article can be stored before it is considered
\"old\".  It can also return the values `never' and `immediate'.

Eg.:

\(setq nnmail-expiry-wait-function
      (lambda (newsgroup)
        (cond ((string-match \"private\" newsgroup) 31)
              ((string-match \"junk\" newsgroup) 1)
	      ((string-match \"important\" newsgroup) 'never)
	      (t 7))))"
  :group 'nnmail-expire
  :type '(choice (const :tag "nnmail-expiry-wait" nil)
		 (function :format "%v" nnmail-)))

(defcustom nnmail-cache-accepted-message-ids nil
  "If non-nil, put Message-IDs of Gcc'd articles into the duplicate cache."
  :group 'nnmail
  :type 'boolean)

(defcustom nnmail-spool-file
  (or (getenv "MAIL")
      (concat "/usr/spool/mail/" (user-login-name)))
  "Where the mail backends will look for incoming mail.
This variable is \"/usr/spool/mail/$user\" by default.
If this variable is nil, no mail backends will read incoming mail.
If this variable is a list, all files mentioned in this list will be
used as incoming mailboxes.
If this variable is a directory (i. e., it's name ends with a \"/\"),
treat all files in that directory as incoming spool files."
  :group 'nnmail-files
  :type 'file)

(defcustom nnmail-crash-box "~/.gnus-crash-box"
  "File where Gnus will store mail while processing it."
  :group 'nnmail-files
  :type 'file)

(defcustom nnmail-use-procmail nil
  "*If non-nil, the mail backends will look in `nnmail-procmail-directory' for spool files.
The file(s) in `nnmail-spool-file' will also be read."
  :group 'nnmail-procmail
  :type 'boolean)

(defcustom nnmail-procmail-directory "~/incoming/"
  "*When using procmail (and the like), incoming mail is put in this directory.
The Gnus mail backends will read the mail from this directory."
  :group 'nnmail-procmail
  :type 'directory)

(defcustom nnmail-procmail-suffix "\\.spool"
  "*Suffix of files created by procmail (and the like).
This variable might be a suffix-regexp to match the suffixes of
several files - eg. \".spool[0-9]*\"."
  :group 'nnmail-procmail
  :type 'regexp)

(defcustom nnmail-resplit-incoming nil
  "*If non-nil, re-split incoming procmail sorted mail."
  :group 'nnmail-procmail
  :type 'boolean)

(defcustom nnmail-delete-file-function 'delete-file
  "Function called to delete files in some mail backends."
  :group 'nnmail-files
  :type 'function)

(defcustom nnmail-crosspost-link-function
  (if (string-match "windows-nt\\|emx" (format "%s" system-type))
      'copy-file
    'add-name-to-file)
  "Function called to create a copy of a file.
This is `add-name-to-file' by default, which means that crossposts
will use hard links.  If your file system doesn't allow hard
links, you could set this variable to `copy-file' instead."
  :group 'nnmail-files
  :type '(radio (function-item add-name-to-file)
		(function-item copy-file)
		(function :tag "Other")))

(defcustom nnmail-movemail-program "movemail"
  "*A command to be executed to move mail from the inbox.
The default is \"movemail\".

This can also be a function.  In that case, the function will be
called with two parameters -- the name of the INBOX file, and the file
to be moved to."
  :group 'nnmail-files
  :group 'nnmail-retrieve
  :type 'string)

(defcustom nnmail-pop-password-required nil
  "*Non-nil if a password is required when reading mail using POP."
  :group 'nnmail-retrieve
  :type 'boolean)

(defcustom nnmail-read-incoming-hook
  (if (eq system-type 'windows-nt)
      '(nnheader-ms-strip-cr)
    nil)
  "Hook that will be run after the incoming mail has been transferred.
The incoming mail is moved from `nnmail-spool-file' (which normally is
something like \"/usr/spool/mail/$user\") to the user's home
directory.  This hook is called after the incoming mail box has been
emptied, and can be used to call any mail box programs you have
running (\"xwatch\", etc.)

Eg.

\(add-hook 'nnmail-read-incoming-hook
	   (lambda ()
	     (start-process \"mailsend\" nil
			    \"/local/bin/mailsend\" \"read\" \"mbox\")))

If you have xwatch running, this will alert it that mail has been
read.

If you use `display-time', you could use something like this:

\(add-hook 'nnmail-read-incoming-hook
	  (lambda ()
	    ;; Update the displayed time, since that will clear out
	    ;; the flag that says you have mail.
	    (when (eq (process-status \"display-time\") 'run)
	      (display-time-filter display-time-process \"\"))))"
  :group 'nnmail-prepare
  :type 'hook)

;; Suggested by Erik Selberg <speed@cs.washington.edu>.
(defcustom nnmail-prepare-incoming-hook nil
  "Hook called before treating incoming mail.
The hook is run in a buffer with all the new, incoming mail."
  :group 'nnmail-prepare
  :type 'hook)

(defcustom nnmail-prepare-incoming-header-hook nil
  "Hook called narrowed to the headers of each message.
This can be used to remove excessive spaces (and stuff like
that) from the headers before splitting and saving the messages."
  :group 'nnmail-prepare
  :type 'hook)

(defcustom nnmail-prepare-incoming-message-hook nil
  "Hook called narrowed to each message."
  :group 'nnmail-prepare
  :type 'hook)

(defcustom nnmail-list-identifiers nil
  "Regexp that matches list identifiers to be removed.
This can also be a list of regexps."
  :group 'nnmail-prepare
  :type '(choice (const :tag "none" nil)
		 regexp
		 (repeat regexp)))

(defcustom nnmail-pre-get-new-mail-hook nil
  "Hook called just before starting to handle new incoming mail."
  :group 'nnmail-retrieve
  :type 'hook)

(defcustom nnmail-post-get-new-mail-hook nil
  "Hook called just after finishing handling new incoming mail."
  :group 'nnmail-retrieve
  :type 'hook)

(defcustom nnmail-split-hook nil
  "Hook called before deciding where to split an article.
The functions in this hook are free to modify the buffer
contents in any way they choose -- the buffer contents are
discarded after running the split process."
  :group 'nnmail-split
  :type 'hook)

;; Suggested by Mejia Pablo J <pjm9806@usl.edu>.
(defcustom nnmail-tmp-directory nil
  "*If non-nil, use this directory for temporary storage.
Used when reading incoming mail."
  :group 'nnmail-files
  :group 'nnmail-retrieve
  :type '(choice (const :tag "default" nil)
		 (directory :format "%v")))

(defcustom nnmail-large-newsgroup 50
  "*The number of the articles which indicates a large newsgroup.
If the number of the articles is greater than the value, verbose
messages will be shown to indicate the current status."
  :group 'nnmail-various
  :type 'integer)

(defcustom nnmail-split-fancy "mail.misc"
  "Incoming mail can be split according to this fancy variable.
To enable this, set `nnmail-split-methods' to `nnmail-split-fancy'.

The format is this variable is SPLIT, where SPLIT can be one of
the following:

GROUP: Mail will be stored in GROUP (a string).

\(FIELD VALUE SPLIT): If the message field FIELD (a regexp) contains
  VALUE (a regexp), store the messages as specified by SPLIT.

\(| SPLIT...): Process each SPLIT expression until one of them matches.
  A SPLIT expression is said to match if it will cause the mail
  message to be stored in one or more groups.

\(& SPLIT...): Process each SPLIT expression.

\(: FUNCTION optional args): Call FUNCTION with the optional args, in
  the buffer containing the message headers.  The return value FUNCTION
  should be a split, which is then recursively processed.

FIELD must match a complete field name.  VALUE must match a complete
word according to the `nnmail-split-fancy-syntax-table' syntax table.
You can use \".*\" in the regexps to match partial field names or words.

FIELD and VALUE can also be lisp symbols, in that case they are expanded
as specified in `nnmail-split-abbrev-alist'.

GROUP can contain \\& and \\N which will substitute from matching
\\(\\) patterns in the previous VALUE.

Example:

\(setq nnmail-split-methods 'nnmail-split-fancy
      nnmail-split-fancy
      ;; Messages from the mailer daemon are not crossposted to any of
      ;; the ordinary groups.  Warnings are put in a separate group
      ;; from real errors.
      '(| (\"from\" mail (| (\"subject\" \"warn.*\" \"mail.warning\")
			  \"mail.misc\"))
	  ;; Non-error messages are crossposted to all relevant
	  ;; groups, but we don't crosspost between the group for the
	  ;; (ding) list and the group for other (ding) related mail.
	  (& (| (any \"ding@ifi\\\\.uio\\\\.no\" \"ding.list\")
		(\"subject\" \"ding\" \"ding.misc\"))
	     ;; Other mailing lists...
	     (any \"procmail@informatik\\\\.rwth-aachen\\\\.de\" \"procmail.list\")
	     (any \"SmartList@informatik\\\\.rwth-aachen\\\\.de\" \"SmartList.list\")
	     ;; People...
	     (any \"larsi@ifi\\\\.uio\\\\.no\" \"people.Lars Magne Ingebrigtsen\"))
	  ;; Unmatched mail goes to the catch all group.
	  \"misc.misc\"))"
  :group 'nnmail-split
  ;; Sigh!
  :type 'sexp)

(defcustom nnmail-split-abbrev-alist
  '((any . "from\\|to\\|cc\\|sender\\|apparently-to\\|resent-from\\|resent-to\\|resent-cc")
    (mail . "mailer-daemon\\|postmaster\\|uucp")
    (to . "to\\|cc\\|apparently-to\\|resent-to\\|resent-cc")
    (from . "from\\|sender\\|resent-from")
    (nato . "to\\|cc\\|resent-to\\|resent-cc")
    (naany . "from\\|to\\|cc\\|sender\\|resent-from\\|resent-to\\|resent-cc"))
  "Alist of abbreviations allowed in `nnmail-split-fancy'."
  :group 'nnmail-split
  :type '(repeat (cons :format "%v" symbol regexp)))

(defcustom nnmail-delete-incoming t
  "*If non-nil, the mail backends will delete incoming files after
splitting."
  :group 'nnmail-retrieve
  :type 'boolean)

(defcustom nnmail-message-id-cache-length 1000
  "*The approximate number of Message-IDs nnmail will keep in its cache.
If this variable is nil, no checking on duplicate messages will be
performed."
  :group 'nnmail-duplicate
  :type '(choice (const :tag "disable" nil)
		 (integer :format "%v")))

(defcustom nnmail-message-id-cache-file "~/.nnmail-cache"
  "*The file name of the nnmail Message-ID cache."
  :group 'nnmail-duplicate
  :group 'nnmail-files
  :type 'file)

(defcustom nnmail-treat-duplicates 'warn
  "*If non-nil, nnmail keep a cache of Message-IDs to discover mail duplicates.
Three values are legal: nil, which means that nnmail is not to keep a
Message-ID cache; `warn', which means that nnmail should insert extra
headers to warn the user about the duplication (this is the default);
and `delete', which means that nnmail will delete duplicated mails.

This variable can also be a function.  It will be called from a buffer
narrowed to the article in question with the Message-ID as a
parameter.  It should return nil, `warn' or `delete'."
  :group 'nnmail-duplicate
  :type '(choice (const :tag "off" nil)
		 (const warn)
		 (const delete)))

;;; Internal variables.

(defvar nnmail-split-history nil
  "List of group/article elements that say where the previous split put messages.")

(defvar nnmail-pop-password nil
  "*Password to use when reading mail from a POP server, if required.")

(defvar nnmail-split-fancy-syntax-table nil
  "Syntax table used by `nnmail-split-fancy'.")
(unless (syntax-table-p nnmail-split-fancy-syntax-table)
  (setq nnmail-split-fancy-syntax-table
	(copy-syntax-table (standard-syntax-table)))
  ;; support the %-hack
  (modify-syntax-entry ?\% "." nnmail-split-fancy-syntax-table))

(defvar nnmail-prepare-save-mail-hook nil
  "Hook called before saving mail.")

(defvar nnmail-moved-inboxes nil
  "List of inboxes that have been moved.")

(defvar nnmail-internal-password nil)



(defconst nnmail-version "nnmail 1.0"
  "nnmail version.")



(defun nnmail-request-post (&optional server)
  (mail-send-and-exit nil))

;; 1997/5/4 by MORIOKA Tomohiko <morioka@jaist.ac.jp>
(defvar nnmail-file-coding-system nil
  "Coding system used in nnmail.")

(defun nnmail-find-file (file)
  "Insert FILE in server buffer safely."
  (set-buffer nntp-server-buffer)
  (erase-buffer)
  (let ((format-alist nil)
        (after-insert-file-functions nil))
    (condition-case ()
	;; 1997/5/4 by MORIOKA Tomohiko <morioka@jaist.ac.jp>
	(let ((coding-system-for-read nnmail-file-coding-system)
	      ;; 1997/8/12 by MORIOKA Tomohiko
	      ;;	for XEmacs/mule.
	      (pathname-coding-system 'binary))
	  (insert-file-contents file)
	  t)
      (file-error nil))))

;; 1997/8/10 by MORIOKA Tomohiko
(defvar nnmail-pathname-coding-system
  'iso-8859-1
  "*Coding system for pathname.")

(defun nnmail-group-pathname (group dir &optional file)
  "Make pathname for GROUP."
  (concat
   (let ((dir (file-name-as-directory (expand-file-name dir))))
     ;; If this directory exists, we use it directly.
     (if (or nnmail-use-long-file-names
	     (file-directory-p (concat dir group)))
	 (concat dir group "/")
       ;; If not, we translate dots into slashes.
       (concat dir
	       (gnus-encode-coding-string
		(nnheader-replace-chars-in-string group ?. ?/)
		nnmail-pathname-coding-system)
	       "/")))
   (or file "")))

(defun nnmail-date-to-time (date)
  "Convert DATE into time."
  (condition-case ()
      (let* ((d1 (timezone-parse-date date))
	     (t1 (timezone-parse-time (aref d1 3))))
	(apply 'encode-time
	       (mapcar (lambda (el)
			 (and el (string-to-number el)))
		       (list
			(aref t1 2) (aref t1 1) (aref t1 0)
			(aref d1 2) (aref d1 1) (aref d1 0)
			(number-to-string
			 (* 60 (timezone-zone-to-minute (aref d1 4))))))))
    ;; If we get an error, then we just return a 0 time.
    (error (list 0 0))))

(defun nnmail-time-less (t1 t2)
  "Say whether time T1 is less than time T2."
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
	   (< (nth 1 t1) (nth 1 t2)))))

(defun nnmail-days-to-time (days)
  "Convert DAYS into time."
  (let* ((seconds (* 1.0 days 60 60 24))
	 (rest (expt 2 16))
	 (ms (condition-case nil (round (/ seconds rest))
	       (range-error (expt 2 16)))))
    (list ms (condition-case nil (round (- seconds (* ms rest)))
	       (range-error (expt 2 16))))))

(defun nnmail-time-since (time)
  "Return the time since TIME, which is either an internal time or a date."
  (when (stringp time)
    ;; Convert date strings to internal time.
    (setq time (nnmail-date-to-time time)))
  (let* ((current (current-time))
	 (rest (when (< (nth 1 current) (nth 1 time))
		 (expt 2 16))))
    (list (- (+ (car current) (if rest -1 0)) (car time))
	  (- (+ (or rest 0) (nth 1 current)) (nth 1 time)))))

;; Function rewritten from rmail.el.
(defun nnmail-move-inbox (inbox)
  "Move INBOX to `nnmail-crash-box'."
  (if (not (file-writable-p nnmail-crash-box))
      (gnus-error 1 "Can't write to crash box %s.  Not moving mail"
		  nnmail-crash-box)
    ;; If the crash box exists and is empty, we delete it.
    (when (and (file-exists-p nnmail-crash-box)
	       (zerop (nnheader-file-size (file-truename nnmail-crash-box))))
      (delete-file nnmail-crash-box))
    (let ((tofile (file-truename (expand-file-name nnmail-crash-box)))
	  (popmail (string-match "^po:" inbox))
	  movemail errors result)
      (unless popmail
	(setq inbox (file-truename (expand-file-name inbox)))
	(setq movemail t)
	;; On some systems, /usr/spool/mail/foo is a directory
	;; and the actual inbox is /usr/spool/mail/foo/foo.
	(when (file-directory-p inbox)
	  (setq inbox (expand-file-name (user-login-name) inbox))))
      (if (member inbox nnmail-moved-inboxes)
	  ;; We don't try to move an already moved inbox.
	  nil
	(if popmail
	    (progn
	      (when (and nnmail-pop-password
			 (not nnmail-internal-password))
		(setq nnmail-internal-password nnmail-pop-password))
	      (when (and nnmail-pop-password-required
			 (not nnmail-internal-password))
		(setq nnmail-internal-password
		      (nnmail-read-passwd
		       (format "Password for %s: "
			       (substring inbox (+ popmail 3))))))
	      (message "Getting mail from the post office..."))
	  (when (or (and (file-exists-p tofile)
			 (/= 0 (nnheader-file-size tofile)))
		    (and (file-exists-p inbox)
			 (/= 0 (nnheader-file-size inbox))))
	    (message "Getting mail from %s..." inbox)))
	;; Set TOFILE if have not already done so, and
	;; rename or copy the file INBOX to TOFILE if and as appropriate.
	(cond
	 ((file-exists-p tofile)
	  ;; The crash box exists already.
	  t)
	 ((and (not popmail)
	       (not (file-exists-p inbox)))
	  ;; There is no inbox.
	  (setq tofile nil))
	 (t
	  ;; If getting from mail spool directory, use movemail to move
	  ;; rather than just renaming, so as to interlock with the
	  ;; mailer.
	  (unwind-protect
	      (save-excursion
		(setq errors (generate-new-buffer " *nnmail loss*"))
		(buffer-disable-undo errors)
		(let ((default-directory "/"))
		  (if (nnheader-functionp nnmail-movemail-program)
		      (condition-case err
			  (progn
			    (funcall nnmail-movemail-program inbox tofile)
			    (setq result 0))
			(error
			 (save-excursion
			   (set-buffer errors)
			   (insert (prin1-to-string err))
			   (setq result 255))))
		    (setq result
			  (apply
			   'call-process
			   (append
			    (list
			     (expand-file-name
			      nnmail-movemail-program exec-directory)
			     nil errors nil inbox tofile)
			    (when nnmail-internal-password
			      (list nnmail-internal-password)))))))
		(if (and (not (buffer-modified-p errors))
			 (zerop result))
		    ;; No output => movemail won
		    (progn
		      (unless popmail
			(when (file-exists-p tofile)
			  (set-file-modes tofile nnmail-default-file-modes)))
		      (push inbox nnmail-moved-inboxes))
		  (set-buffer errors)
		  ;; There may be a warning about older revisions.  We
		  ;; ignore those.
		  (goto-char (point-min))
		  (if (search-forward "older revision" nil t)
		      (progn
			(unless popmail
			  (when (file-exists-p tofile)
			    (set-file-modes tofile nnmail-default-file-modes)))
			(push inbox nnmail-moved-inboxes))
		    ;; Probably a real error.
		    (subst-char-in-region (point-min) (point-max) ?\n ?\  )
		    (goto-char (point-max))
		    (skip-chars-backward " \t")
		    (delete-region (point) (point-max))
		    (goto-char (point-min))
		    (when (looking-at "movemail: ")
		      (delete-region (point-min) (match-end 0)))
		    (unless (yes-or-no-p
			     (format "movemail: %s (%d return).  Continue? "
				     (buffer-string) result))
		      (error "%s" (buffer-string)))
		    (setq tofile nil)))))))
	(message "Getting mail from %s...done" inbox)
	(and errors
	     (buffer-name errors)
	     (kill-buffer errors))
	tofile))))

(defun nnmail-get-active ()
  "Returns an assoc of group names and active ranges.
nn*-request-list should have been called before calling this function."
  (let (group-assoc)
    ;; Go through all groups from the active list.
    (save-excursion
      (set-buffer nntp-server-buffer)
      (goto-char (point-min))
      (while (re-search-forward
	      "^\\([^ \t]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+\\)" nil t)
	;; We create an alist with `(GROUP (LOW . HIGH))' elements.
	(push (list (match-string 1)
		    (cons (string-to-int (match-string 3))
			  (string-to-int (match-string 2))))
	      group-assoc)))
    group-assoc))

;; 1997/8/12 by MORIOKA Tomohiko
(defvar nnmail-active-file-coding-system
  'iso-8859-1
  "*Coding system for active file.")

(defun nnmail-save-active (group-assoc file-name)
  "Save GROUP-ASSOC in ACTIVE-FILE."
  (let ((coding-system-for-write nnmail-active-file-coding-system))
    (when file-name
      (nnheader-temp-write file-name
	(nnmail-generate-active group-assoc)))))

(defun nnmail-generate-active (alist)
  "Generate an active file from group-alist ALIST."
  (erase-buffer)
  (let (group)
    (while (setq group (pop alist))
      (insert (format "%s %d %d y\n" (car group) (cdadr group)
		      (caadr group))))))

(defun nnmail-get-split-group (file group)
  "Find out whether this FILE is to be split into GROUP only.
If GROUP is non-nil and we are using procmail, return the group name
only when the file is the correct procmail file.  When GROUP is nil,
return nil if FILE is a spool file or the procmail group for which it
is a spool.  If not using procmail, return GROUP."
  (if (or (eq nnmail-spool-file 'procmail)
	  nnmail-use-procmail)
      (if (string-match (concat "^" (expand-file-name
				     (file-name-as-directory
				      nnmail-procmail-directory))
				"\\([^/]*\\)" nnmail-procmail-suffix "$")
			(expand-file-name file))
	  (let ((procmail-group (substring (expand-file-name file)
					   (match-beginning 1)
					   (match-end 1))))
	    (if group
		(if (string-equal group procmail-group)
		    group
		  nil)
	      procmail-group))
	nil)
    group))

(defun nnmail-process-babyl-mail-format (func artnum-func)
  (let ((case-fold-search t)
	start message-id content-length do-search end)
    (goto-char (point-min))
    (while (not (eobp))
      (re-search-forward
       "\n0, *unseen,+\n\\(\\*\\*\\* EOOH \\*\\*\\*\n\\)?" nil t)
      (goto-char (match-end 0))
      (delete-region (match-beginning 0) (match-end 0))
      (narrow-to-region
       (setq start (point))
       (progn
	 ;; Skip all the headers in case there are more "From "s...
	 (or (search-forward "\n\n" nil t)
	     (search-forward-regexp "^[^:]*\\( .*\\|\\)$" nil t)
	     (search-forward ""))
	 (point)))
      ;; Unquote the ">From " line, if any.
      (goto-char (point-min))
      (when (looking-at ">From ")
	(replace-match "X-From-Line: ") )
      (run-hooks 'nnmail-prepare-incoming-header-hook)
      (goto-char (point-max))
      ;; Find the Message-ID header.
      (save-excursion
	(if (re-search-backward
	     "^Message-ID[ \t]*:[ \n\t]*\\(<[^>]*>\\)" nil t)
	    (setq message-id (buffer-substring (match-beginning 1)
					       (match-end 1)))
	  ;; There is no Message-ID here, so we create one.
	  (save-excursion
	    (when (re-search-backward "^Message-ID[ \t]*:" nil t)
	      (beginning-of-line)
	      (insert "Original-")))
	  (forward-line -1)
	  (insert "Message-ID: " (setq message-id (nnmail-message-id))
		  "\n")))
      ;; Look for a Content-Length header.
      (if (not (save-excursion
		 (and (re-search-backward
		       "^Content-Length:[ \t]*\\([0-9]+\\)" start t)
		      (setq content-length (string-to-int
					    (buffer-substring
					     (match-beginning 1)
					     (match-end 1))))
		      ;; We destroy the header, since none of
		      ;; the backends ever use it, and we do not
		      ;; want to confuse other mailers by having
		      ;; a (possibly) faulty header.
		      (progn (insert "X-") t))))
	  (setq do-search t)
	(widen)
	(if (or (= (+ (point) content-length) (point-max))
		(save-excursion
		  (goto-char (+ (point) content-length))
		  (looking-at "")))
	    (progn
	      (goto-char (+ (point) content-length))
	      (setq do-search nil))
	  (setq do-search t)))
      (widen)
      ;; Go to the beginning of the next article - or to the end
      ;; of the buffer.
      (when do-search
	(if (re-search-forward "^" nil t)
	    (goto-char (match-beginning 0))
	  (goto-char (1- (point-max)))))
      (delete-char 1)			; delete ^_
      (save-excursion
	(save-restriction
	  (narrow-to-region start (point))
	  (goto-char (point-min))
	  (nnmail-check-duplication message-id func artnum-func)
	  (setq end (point-max))))
      (goto-char end))))

(defsubst nnmail-search-unix-mail-delim ()
  "Put point at the beginning of the next Unix mbox message."
  ;; Algorithm used to find the the next article in the
  ;; brain-dead Unix mbox format:
  ;;
  ;; 1) Search for "^From ".
  ;; 2) If we find it, then see whether the previous
  ;;    line is blank and the next line looks like a header.
  ;; Then it's possible that this is a mail delim, and we use it.
  (let ((case-fold-search nil)
	found)
    (while (not found)
      (if (not (re-search-forward "^From " nil t))
	  (setq found 'no)
	(save-excursion
	  (beginning-of-line)
	  (when (and (or (bobp)
			 (save-excursion
			   (forward-line -1)
			   (= (following-char) ?\n)))
		     (save-excursion
		       (forward-line 1)
		       (while (looking-at ">From \\|From ")
			 (forward-line 1))
		       (looking-at "[^ \n\t:]+[ \n\t]*:")))
	    (setq found 'yes)))))
    (beginning-of-line)
    (eq found 'yes)))

(defun nnmail-search-unix-mail-delim-backward ()
  "Put point at the beginning of the current Unix mbox message."
  ;; Algorithm used to find the the next article in the
  ;; brain-dead Unix mbox format:
  ;;
  ;; 1) Search for "^From ".
  ;; 2) If we find it, then see whether the previous
  ;;    line is blank and the next line looks like a header.
  ;; Then it's possible that this is a mail delim, and we use it.
  (let ((case-fold-search nil)
	found)
    (while (not found)
      (if (not (re-search-backward "^From " nil t))
	  (setq found 'no)
	(save-excursion
	  (beginning-of-line)
	  (when (and (or (bobp)
			 (save-excursion
			   (forward-line -1)
			   (= (following-char) ?\n)))
		     (save-excursion
		       (forward-line 1)
		       (while (looking-at ">From \\|From ")
			 (forward-line 1))
		       (looking-at "[^ \n\t:]+[ \n\t]*:")))
	    (setq found 'yes)))))
    (beginning-of-line)
    (eq found 'yes)))

(defun nnmail-process-unix-mail-format (func artnum-func)
  (let ((case-fold-search t)
	start message-id content-length end skip head-end)
    (goto-char (point-min))
    (if (not (and (re-search-forward "^From " nil t)
		  (goto-char (match-beginning 0))))
	;; Possibly wrong format?
	(error "Error, unknown mail format! (Possibly corrupted.)")
      ;; Carry on until the bitter end.
      (while (not (eobp))
	(setq start (point)
	      end nil)
	;; Find the end of the head.
	(narrow-to-region
	 start
	 (if (search-forward "\n\n" nil t)
	     (1- (point))
	   ;; This will never happen, but just to be on the safe side --
	   ;; if there is no head-body delimiter, we search a bit manually.
	   (while (and (looking-at "From \\|[^ \t]+:")
		       (not (eobp)))
	     (forward-line 1))
	   (point)))
	;; Find the Message-ID header.
	(goto-char (point-min))
	(if (re-search-forward "^Message-ID[ \t]*:[ \n\t]*\\(<[^>]+>\\)" nil t)
	    (setq message-id (match-string 1))
	  (save-excursion
	    (when (re-search-forward "^Message-ID[ \t]*:" nil t)
	      (beginning-of-line)
	      (insert "Original-")))
	  ;; There is no Message-ID here, so we create one.
	  (forward-line 1)
	  (insert "Message-ID: " (setq message-id (nnmail-message-id)) "\n"))
	;; Look for a Content-Length header.
	(goto-char (point-min))
	(if (not (re-search-forward
		  "^Content-Length:[ \t]*\\([0-9]+\\)" nil t))
	    (setq content-length nil)
	  (setq content-length (string-to-int (match-string 1)))
	  ;; We destroy the header, since none of the backends ever
	  ;; use it, and we do not want to confuse other mailers by
	  ;; having a (possibly) faulty header.
	  (beginning-of-line)
	  (insert "X-"))
	(run-hooks 'nnmail-prepare-incoming-header-hook)
	;; Find the end of this article.
	(goto-char (point-max))
	(widen)
	(setq head-end (point))
	;; We try the Content-Length value.  The idea: skip over the header
	;; separator, then check what happens content-length bytes into the
	;; message body.  This should be either the end ot the buffer, the
	;; message separator or a blank line followed by the separator.
	;; The blank line should probably be deleted.  If neither of the
	;; three is met, the content-length header is probably invalid.
	(when content-length
	  (forward-line 1)
	  (setq skip (+ (point) content-length))
	  (goto-char skip)
	  (cond ((or (= skip (point-max))
		     (= (1+ skip) (point-max)))
		 (setq end (point-max)))
		((looking-at "From ")
		 (setq end skip))
		((looking-at "[ \t]*\n\\(From \\)")
		 (setq end (match-beginning 1)))
		(t (setq end nil))))
	(if end
	    (goto-char end)
	  ;; No Content-Length, so we find the beginning of the next
	  ;; article or the end of the buffer.
	  (goto-char head-end)
	  (or (nnmail-search-unix-mail-delim)
	      (goto-char (point-max))))
	;; Allow the backend to save the article.
	(save-excursion
	  (save-restriction
	    (narrow-to-region start (point))
	    (goto-char (point-min))
	    (nnmail-check-duplication message-id func artnum-func)
	    (setq end (point-max))))
	(goto-char end)))))

(defun nnmail-process-mmdf-mail-format (func artnum-func)
  (let ((delim "^\^A\^A\^A\^A$")
	(case-fold-search t)
	start message-id end)
    (goto-char (point-min))
    (if (not (and (re-search-forward delim nil t)
		  (forward-line 1)))
	;; Possibly wrong format?
	(error "Error, unknown mail format! (Possibly corrupted.)")
      ;; Carry on until the bitter end.
      (while (not (eobp))
	(setq start (point))
	;; Find the end of the head.
	(narrow-to-region
	 start
	 (if (search-forward "\n\n" nil t)
	     (1- (point))
	   ;; This will never happen, but just to be on the safe side --
	   ;; if there is no head-body delimiter, we search a bit manually.
	   (while (and (looking-at "From \\|[^ \t]+:")
		       (not (eobp)))
	     (forward-line 1))
	   (point)))
	;; Find the Message-ID header.
	(goto-char (point-min))
	(if (re-search-forward "^Message-ID[ \t]*:[ \n\t]*\\(<[^>]+>\\)" nil t)
	    (setq message-id (match-string 1))
	  ;; There is no Message-ID here, so we create one.
	  (save-excursion
	    (when (re-search-backward "^Message-ID[ \t]*:" nil t)
	      (beginning-of-line)
	      (insert "Original-")))
	  (forward-line 1)
	  (insert "Message-ID: " (setq message-id (nnmail-message-id)) "\n"))
	(run-hooks 'nnmail-prepare-incoming-header-hook)
	;; Find the end of this article.
	(goto-char (point-max))
	(widen)
	(if (re-search-forward delim nil t)
	    (beginning-of-line)
	  (goto-char (point-max)))
	;; Allow the backend to save the article.
	(save-excursion
	  (save-restriction
	    (narrow-to-region start (point))
	    (goto-char (point-min))
	    (nnmail-check-duplication message-id func artnum-func)
	    (setq end (point-max))))
	(goto-char end)
	(forward-line 2)))))

(defun nnmail-split-incoming (incoming func &optional exit-func
				       group artnum-func)
  "Go through the entire INCOMING file and pick out each individual mail.
FUNC will be called with the buffer narrowed to each mail."
  (let (;; If this is a group-specific split, we bind the split
	;; methods to just this group.
	(nnmail-split-methods (if (and group
				       (or (eq nnmail-spool-file 'procmail)
					   nnmail-use-procmail)
				       (not nnmail-resplit-incoming))
				  (list (list group ""))
				nnmail-split-methods)))
    (save-excursion
      ;; Insert the incoming file.
      (set-buffer (get-buffer-create " *nnmail incoming*"))
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (nnheader-insert-file-contents incoming)
      (unless (zerop (buffer-size))
	(goto-char (point-min))
	(save-excursion (run-hooks 'nnmail-prepare-incoming-hook))
	;; Handle both babyl, MMDF and unix mail formats, since movemail will
	;; use the former when fetching from a mailbox, the latter when
	;; fetching from a file.
	(cond ((or (looking-at "\^L")
		   (looking-at "BABYL OPTIONS:"))
	       (nnmail-process-babyl-mail-format func artnum-func))
	      ((looking-at "\^A\^A\^A\^A")
	       (nnmail-process-mmdf-mail-format func artnum-func))
	      (t
	       (nnmail-process-unix-mail-format func artnum-func))))
      (when exit-func
	(funcall exit-func))
      (kill-buffer (current-buffer)))))

;; Mail crossposts suggested by Brian Edmonds <edmonds@cs.ubc.ca>.
(defun nnmail-article-group (func)
  "Look at the headers and return an alist of groups that match.
FUNC will be called with the group name to determine the article number."
  (let ((methods nnmail-split-methods)
	(obuf (current-buffer))
	(beg (point-min))
	end group-art method)
    (if (and (sequencep methods) (= (length methods) 1))
	;; If there is only just one group to put everything in, we
	;; just return a list with just this one method in.
	(setq group-art
	      (list (cons (caar methods) (funcall func (caar methods)))))
      ;; We do actual comparison.
      (save-excursion
	;; Find headers.
	(goto-char beg)
	(setq end (if (search-forward "\n\n" nil t) (point) (point-max)))
	(set-buffer nntp-server-buffer)
	(erase-buffer)
	;; Copy the headers into the work buffer.
	(insert-buffer-substring obuf beg end)
	;; Fold continuation lines.
	(goto-char (point-min))
	(while (re-search-forward "\\(\r?\n[ \t]+\\)+" nil t)
	  (replace-match " " t t))
	;; Allow washing.
	(run-hooks 'nnmail-split-hook)
	(if (and (symbolp nnmail-split-methods)
		 (fboundp nnmail-split-methods))
	    (let ((split
		   (condition-case nil
		       ;; `nnmail-split-methods' is a function, so we
		       ;; just call this function here and use the
		       ;; result.
		       (or (funcall nnmail-split-methods)
			   '("bogus"))
		     (error
		      (message
		       "Error in `nnmail-split-methods'; using `bogus' mail group")
		      (sit-for 1)
		      '("bogus")))))
	      ;; The article may be "cross-posted" to `junk'.  What
	      ;; to do?  Just remove the `junk' spec.  Don't really
	      ;; see anything else to do...
	      (let (elem)
		(while (setq elem (car (memq 'junk split)))
		  (setq split (delq elem split))))
	      (when split
		(setq group-art
		      (mapcar
		       (lambda (group) (cons group (funcall func group)))
		       split))))
	  ;; Go through the split methods to find a match.
	  (while (and methods (or nnmail-crosspost (not group-art)))
	    (goto-char (point-max))
	    (setq method (pop methods))
	    (if (or methods
		    (not (equal "" (nth 1 method))))
		(when (and
		       (ignore-errors
			 (if (stringp (nth 1 method))
			     (re-search-backward (cadr method) nil t)
			   ;; Function to say whether this is a match.
			   (funcall (nth 1 method) (car method))))
		       ;; Don't enter the article into the same
		       ;; group twice.
		       (not (assoc (car method) group-art)))
		  (push (cons (car method) (funcall func (car method)))
			group-art))
	      ;; This is the final group, which is used as a
	      ;; catch-all.
	      (unless group-art
		(setq group-art
		      (list (cons (car method)
				  (funcall func (car method)))))))))
	;; See whether the split methods returned `junk'.
	(if (equal group-art '(junk))
	    nil
	  ;; The article may be "cross-posted" to `junk'.  What
	  ;; to do?  Just remove the `junk' spec.  Don't really
	  ;; see anything else to do...
	  (let (elem)
	    (while (setq elem (car (memq 'junk group-art)))
	      (setq group-art (delq elem group-art)))
	    (nreverse group-art)))))))

(defun nnmail-insert-lines ()
  "Insert how many lines there are in the body of the mail.
Return the number of characters in the body."
  (let (lines chars)
    (save-excursion
      (goto-char (point-min))
      (when (search-forward "\n\n" nil t)
	(setq chars (- (point-max) (point)))
	(setq lines (count-lines (point) (point-max)))
	(forward-char -1)
	(save-excursion
	  (when (re-search-backward "^Lines: " nil t)
	    (delete-region (point) (progn (forward-line 1) (point)))))
	(beginning-of-line)
	(insert (format "Lines: %d\n" (max lines 0)))
	chars))))

(defun nnmail-insert-xref (group-alist)
  "Insert an Xref line based on the (group . article) alist."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "\n\n" nil t)
      (forward-char -1)
      (when (re-search-backward "^Xref: " nil t)
	(delete-region (match-beginning 0)
		       (progn (forward-line 1) (point))))
      (insert (format "Xref: %s" (system-name)))
      (while group-alist
	(insert (format " %s:%d"
			(gnus-encode-coding-string (caar group-alist)
					      nnmail-pathname-coding-system)
			(cdar group-alist)))
	(setq group-alist (cdr group-alist)))
      (insert "\n"))))

;;; Message washing functions

(defun nnmail-remove-leading-whitespace ()
  "Remove excessive whitespace from all headers."
  (goto-char (point-min))
  (while (re-search-forward "^\\([^ :]+: \\) +" nil t)
    (replace-match "\\1" t)))

(defun nnmail-remove-list-identifiers ()
  "Remove list identifiers from Subject headers."
  (let ((regexp (if (stringp nnmail-list-identifiers) nnmail-list-identifiers
		  (mapconcat 'identity nnmail-list-identifiers "\\|"))))
    (when regexp
      (goto-char (point-min))
      (when (re-search-forward
	     (concat "^Subject: +\\(Re: +\\)?\\(" regexp "\\) *")
	     nil t)
	(delete-region (match-beginning 2) (match-end 0))))))

(defun nnmail-remove-tabs ()
  "Translate TAB characters into SPACE characters."
  (subst-char-in-region (point-min) (point-max) ?\t ?  t))

;;; Utility functions

;; Written by byer@mv.us.adobe.com (Scott Byer).
(defun nnmail-make-complex-temp-name (prefix)
  (let ((newname (make-temp-name prefix))
	(newprefix prefix))
    (while (file-exists-p newname)
      (setq newprefix (concat newprefix "x"))
      (setq newname (make-temp-name newprefix)))
    newname))

;; Written by Per Abrahamsen <amanda@iesd.auc.dk>.

(defun nnmail-split-fancy ()
  "Fancy splitting method.
See the documentation for the variable `nnmail-split-fancy' for documentation."
  (let ((syntab (syntax-table)))
    (unwind-protect
	(progn
	  (set-syntax-table nnmail-split-fancy-syntax-table)
	  (nnmail-split-it nnmail-split-fancy))
      (set-syntax-table syntab))))

(defvar nnmail-split-cache nil)
;; Alist of split expressions their equivalent regexps.

(defun nnmail-split-it (split)
  ;; Return a list of groups matching SPLIT.
  (cond
   ;; nil split
   ((null split)
    nil)

   ;; A group name.  Do the \& and \N subs into the string.
   ((stringp split)
    (list (nnmail-expand-newtext split)))

   ;; Junk the message.
   ((eq split 'junk)
    (list 'junk))

   ;; Builtin & operation.
   ((eq (car split) '&)
    (apply 'nconc (mapcar 'nnmail-split-it (cdr split))))

   ;; Builtin | operation.
   ((eq (car split) '|)
    (let (done)
      (while (and (not done) (cdr split))
	(setq split (cdr split)
	      done (nnmail-split-it (car split))))
      done))

   ;; Builtin : operation.
   ((eq (car split) ':)
    (nnmail-split-it (eval (cdr split))))

   ;; Check the cache for the regexp for this split.
   ;; FIX FIX FIX could avoid calling assq twice here
   ((assq split nnmail-split-cache)
    (goto-char (point-max))
    ;; FIX FIX FIX problem with re-search-backward is that if you have
    ;; a split: (from "foo-\\(bar\\|baz\\)@gnus.org "mail.foo.\\1")
    ;; and someone mails a message with 'To: foo-bar@gnus.org' and
    ;; 'CC: foo-baz@gnus.org', we'll pick 'mail.foo.baz' as the group
    ;; if the cc line is a later header, even though the other choice
    ;; is probably better.  Also, this routine won't do a crosspost
    ;; when there are two different matches.
    ;; I guess you could just make this more determined, and it could
    ;; look for still more matches prior to this one, and recurse
    ;; on each of the multiple matches hit.  Of course, then you'd
    ;; want to make sure that nnmail-article-group or nnmail-split-fancy
    ;; removed duplicates, since there might be more of those.
    ;; I guess we could also remove duplicates in the & split case, since
    ;; that's the only thing that can introduce them.
    (when (re-search-backward (cdr (assq split nnmail-split-cache)) nil t)
      ;; Someone might want to do a \N sub on this match, so get the
      ;; correct match positions.
      (goto-char (match-end 0))
      (let ((value (nth 1 split)))
	(re-search-backward (if (symbolp value)
				(cdr (assq value nnmail-split-abbrev-alist))
			      value)
			    (match-end 1)))
      (nnmail-split-it (nth 2 split))))

   ;; Not in cache, compute a regexp for the field/value pair.
   (t
    (let* ((field (nth 0 split))
	   (value (nth 1 split))
	   (regexp (concat "^\\(\\("
			   (if (symbolp field)
			       (cdr (assq field nnmail-split-abbrev-alist))
			     field)
			   "\\):.*\\)\\<\\("
			   (if (symbolp value)
			       (cdr (assq value nnmail-split-abbrev-alist))
			     value)
			   "\\)\\>")))
      (push (cons split regexp) nnmail-split-cache)
      ;; Now that it's in the cache, just call nnmail-split-it again
      ;; on the same split, which will find it immediately in the cache.
      (nnmail-split-it split)))))

(defun nnmail-expand-newtext (newtext)
  (let ((len (length newtext))
	(pos 0)
	c expanded beg N did-expand)
    (while (< pos len)
      (setq beg pos)
      (while (and (< pos len)
		  (not (= (aref newtext pos) ?\\)))
	(setq pos (1+ pos)))
      (unless (= beg pos)
	(push (substring newtext beg pos) expanded))
      (when (< pos len)
	;; we hit a \, expand it.
	(setq did-expand t)
	(setq pos (1+ pos))
	(setq c (aref newtext pos))
	(if (not (or (= c ?\&)
		     (and (>= c ?1)
			  (<= c ?9))))
	    ;; \ followed by some character we don't expand
	    (push (char-to-string c) expanded)
	  ;; \& or \N
	  (if (= c ?\&)
	      (setq N 0)
	    (setq N (- c ?0)))
	  (when (match-beginning N)
	    (push (buffer-substring (match-beginning N) (match-end N))
		  expanded))))
      (setq pos (1+ pos)))
    (if did-expand
	(apply 'concat (nreverse expanded))
      newtext)))

;; Get a list of spool files to read.
(defun nnmail-get-spool-files (&optional group)
  (if (null nnmail-spool-file)
      ;; No spool file whatsoever.
      nil
    (let* ((procmails
	    ;; If procmail is used to get incoming mail, the files
	    ;; are stored in this directory.
	    (and (file-exists-p nnmail-procmail-directory)
		 (or (eq nnmail-spool-file 'procmail)
		     nnmail-use-procmail)
		 (directory-files
		  nnmail-procmail-directory
		  t (concat (if group (concat "^" group) "")
			    nnmail-procmail-suffix "$"))))
	   (p procmails)
	   (crash (when (and (file-exists-p nnmail-crash-box)
			     (> (nnheader-file-size
				 (file-truename nnmail-crash-box))
				0))
		    (list nnmail-crash-box))))
      ;; Remove any directories that inadvertently match the procmail
      ;; suffix, which might happen if the suffix is "".
      (while p
	(when (file-directory-p (car p))
	  (setq procmails (delete (car p) procmails)))
	(setq p (cdr p)))
      ;; Return the list of spools.
      (append
       crash
       (cond ((and group
		   (or (eq nnmail-spool-file 'procmail)
		       nnmail-use-procmail)
		   procmails)
	      procmails)
	     ((and group
		   (eq nnmail-spool-file 'procmail))
	      nil)
	     ((listp nnmail-spool-file)
	      (nconc
	       (apply
		'nconc
		(mapcar
		 (lambda (file)
		   (if (and (not (string-match "^po:" file))
			    (file-directory-p file))
		       (nnheader-directory-regular-files file)
		     (list file)))
		 nnmail-spool-file))
	       procmails))
	     ((stringp nnmail-spool-file)
	      (if (and (not (string-match "^po:" nnmail-spool-file))
		       (file-directory-p nnmail-spool-file))
		  (nconc
		   (nnheader-directory-regular-files nnmail-spool-file)
		   procmails)
		(cons nnmail-spool-file procmails)))
	     ((eq nnmail-spool-file 'pop)
	      (cons (format "po:%s" (user-login-name)) procmails))
	     (t
	      procmails))))))

;; Activate a backend only if it isn't already activated.
;; If FORCE, re-read the active file even if the backend is
;; already activated.
(defun nnmail-activate (backend &optional force)
  (let (file timestamp file-time)
    (if (or (not (symbol-value (intern (format "%s-group-alist" backend))))
	    force
	    (and (setq file (ignore-errors
			      (symbol-value (intern (format "%s-active-file"
							    backend)))))
		 (setq file-time (nth 5 (file-attributes file)))
		 (or (not
		      (setq timestamp
			    (condition-case ()
				(symbol-value (intern
					       (format "%s-active-timestamp"
						       backend)))
			      (error 'none))))
		     (not (consp timestamp))
		     (equal timestamp '(0 0))
		     (> (nth 0 file-time) (nth 0 timestamp))
		     (and (= (nth 0 file-time) (nth 0 timestamp))
			  (> (nth 1 file-time) (nth 1 timestamp))))))
	(save-excursion
	  (or (eq timestamp 'none)
	      (set (intern (format "%s-active-timestamp" backend))
		   file-time))
	  (funcall (intern (format "%s-request-list" backend)))))
    t))

(defun nnmail-message-id ()
  (concat "<" (message-unique-id) "@totally-fudged-out-message-id>"))

;;;
;;; nnmail duplicate handling
;;;

(defvar nnmail-cache-buffer nil)

(defun nnmail-cache-open ()
  (if (or (not nnmail-treat-duplicates)
	  (and nnmail-cache-buffer
	       (buffer-name nnmail-cache-buffer)))
      ()				; The buffer is open.
    (save-excursion
      (set-buffer
       (setq nnmail-cache-buffer
	     (get-buffer-create " *nnmail message-id cache*")))
      (buffer-disable-undo (current-buffer))
      (when (file-exists-p nnmail-message-id-cache-file)
	(nnheader-insert-file-contents nnmail-message-id-cache-file))
      (set-buffer-modified-p nil)
      (current-buffer))))

(defun nnmail-cache-close ()
  (when (and nnmail-cache-buffer
	     nnmail-treat-duplicates
	     (buffer-name nnmail-cache-buffer)
	     (buffer-modified-p nnmail-cache-buffer))
    (save-excursion
      (set-buffer nnmail-cache-buffer)
      ;; Weed out the excess number of Message-IDs.
      (goto-char (point-max))
      (when (search-backward "\n" nil t nnmail-message-id-cache-length)
	(progn
	  (beginning-of-line)
	  (delete-region (point-min) (point))))
      ;; Save the buffer.
      (or (file-exists-p (file-name-directory nnmail-message-id-cache-file))
	  (make-directory (file-name-directory nnmail-message-id-cache-file)
			  t))
      (nnmail-write-region (point-min) (point-max)
			   nnmail-message-id-cache-file nil 'silent)
      (set-buffer-modified-p nil)
      (setq nnmail-cache-buffer nil)
      (kill-buffer (current-buffer)))))

(defun nnmail-cache-insert (id)
  (when nnmail-treat-duplicates
    (unless (gnus-buffer-live-p nnmail-cache-buffer)
      (nnmail-cache-open))
    (save-excursion
      (set-buffer nnmail-cache-buffer)
      (goto-char (point-max))
      (insert id "\n"))))

(defun nnmail-cache-id-exists-p (id)
  (when nnmail-treat-duplicates
    (save-excursion
      (set-buffer nnmail-cache-buffer)
      (goto-char (point-max))
      (search-backward id nil t))))

(defun nnmail-fetch-field (header)
  (save-excursion
    (save-restriction
      (message-narrow-to-head)
      (message-fetch-field header))))

(defun nnmail-check-duplication (message-id func artnum-func)
  (run-hooks 'nnmail-prepare-incoming-message-hook)
  ;; If this is a duplicate message, then we do not save it.
  (let* ((duplication (nnmail-cache-id-exists-p message-id))
	 (case-fold-search t)
	 (action (when duplication
		   (cond
		    ((memq nnmail-treat-duplicates '(warn delete))
		     nnmail-treat-duplicates)
		    ((nnheader-functionp nnmail-treat-duplicates)
		     (funcall nnmail-treat-duplicates message-id))
		    (t
		     nnmail-treat-duplicates))))
	 group-art)
    ;; Let the backend save the article (or not).
    (cond
     ((not duplication)
      (nnmail-cache-insert message-id)
      (funcall func (setq group-art
			  (nreverse (nnmail-article-group artnum-func)))))
     ((eq action 'delete)
      (setq group-art nil))
     ((eq action 'warn)
      ;; We insert a warning.
      (let ((case-fold-search t))
	(goto-char (point-min))
	(re-search-forward "^message-id[ \t]*:" nil t)
	(beginning-of-line)
	(insert
	 "Gnus-Warning: This is a duplicate of message " message-id "\n")
	(funcall func (setq group-art
			    (nreverse (nnmail-article-group artnum-func))))))
     (t
      (funcall func (setq group-art
			  (nreverse (nnmail-article-group artnum-func))))))
    ;; Add the group-art list to the history list.
    (if group-art
	(push group-art nnmail-split-history)
      (delete-region (point-min) (point-max)))))

;;; Get new mail.

(defun nnmail-get-value (&rest args)
  (let ((sym (intern (apply 'format args))))
    (when (boundp sym)
      (symbol-value sym))))

(defun nnmail-get-new-mail (method exit-func temp
				   &optional group spool-func)
  "Read new incoming mail."
  ;; Nix out the previous split history.
  (unless group
    (setq nnmail-split-history nil))
  (let* ((spools (nnmail-get-spool-files group))
	 (group-in group)
	 incoming incomings spool)
    (when (and (nnmail-get-value "%s-get-new-mail" method)
	       nnmail-spool-file)
      ;; We first activate all the groups.
      (nnmail-activate method)
      ;; Allow the user to hook.
      (run-hooks 'nnmail-pre-get-new-mail-hook)
      ;; Open the message-id cache.
      (nnmail-cache-open)
      ;; The we go through all the existing spool files and split the
      ;; mail from each.
      (while spools
	(setq spool (pop spools))
	;; We read each spool file if either the spool is a POP-mail
	;; spool, or the file exists.  We can't check for the
	;; existence of POPped mail.
	(when (or (string-match "^po:" spool)
		  (and (file-exists-p (file-truename spool))
		       (> (nnheader-file-size (file-truename spool)) 0)))
	  (nnheader-message 3 "%s: Reading incoming mail..." method)
	  (when (and (nnmail-move-inbox spool)
		     (file-exists-p nnmail-crash-box))
	    ;; There is new mail.  We first find out if all this mail
	    ;; is supposed to go to some specific group.
	    (setq group (nnmail-get-split-group spool group-in))
	    ;; We split the mail
	    (nnmail-split-incoming
	     nnmail-crash-box (intern (format "%s-save-mail" method))
	     spool-func group (intern (format "%s-active-number" method)))
	    ;; Check whether the inbox is to be moved to the special tmp dir.
	    (setq incoming
		  (nnmail-make-complex-temp-name
		   (expand-file-name
		    (if nnmail-tmp-directory
			(concat
			 (file-name-as-directory nnmail-tmp-directory)
			 (file-name-nondirectory
			  (concat (file-name-as-directory temp) "Incoming")))
		      (concat (file-name-as-directory temp) "Incoming")))))
	    (rename-file nnmail-crash-box incoming t)
	    (push incoming incomings))))
      ;; If we did indeed read any incoming spools, we save all info.
      (when incomings
	(nnmail-save-active
	 (nnmail-get-value "%s-group-alist" method)
	 (nnmail-get-value "%s-active-file" method))
	(when exit-func
	  (funcall exit-func))
	(run-hooks 'nnmail-read-incoming-hook)
	(nnheader-message 3 "%s: Reading incoming mail...done" method))
      ;; Close the message-id cache.
      (nnmail-cache-close)
      ;; Allow the user to hook.
      (run-hooks 'nnmail-post-get-new-mail-hook)
      ;; Delete all the temporary files.
      (while incomings
	(setq incoming (pop incomings))
	(and nnmail-delete-incoming
	     (file-exists-p incoming)
	     (file-writable-p incoming)
	     (delete-file incoming))))))

(defun nnmail-expired-article-p (group time force &optional inhibit)
  "Say whether an article that is TIME old in GROUP should be expired."
  (if force
      t
    (let ((days (or (and nnmail-expiry-wait-function
			 (funcall nnmail-expiry-wait-function group))
		    nnmail-expiry-wait)))
      (cond ((or (eq days 'never)
		 (and (not force)
		      inhibit))
	     ;; This isn't an expirable group.
	     nil)
	    ((eq days 'immediate)
	     ;; We expire all articles on sight.
	     t)
	    ((equal time '(0 0))
	     ;; This is an ange-ftp group, and we don't have any dates.
	     nil)
	    ((numberp days)
	     (setq days (nnmail-days-to-time days))
	     ;; Compare the time with the current time.
	     (nnmail-time-less days (nnmail-time-since time)))))))

(defvar nnmail-read-passwd nil)
(defun nnmail-read-passwd (prompt &rest args)
  "Read a password using PROMPT.
If ARGS, PROMPT is used as an argument to `format'."
  (let ((prompt
	 (if args
	     (apply 'format prompt args)
	   prompt)))
    (unless nnmail-read-passwd
      (if (load "passwd" t)
	  (setq nnmail-read-passwd 'read-passwd)
	(unless (fboundp 'ange-ftp-read-passwd)
	  (autoload 'ange-ftp-read-passwd "ange-ftp"))
	(setq nnmail-read-passwd 'ange-ftp-read-passwd)))
    (funcall nnmail-read-passwd prompt)))

(defun nnmail-check-syntax ()
  "Check (and modify) the syntax of the message in the current buffer."
  (save-restriction
    (message-narrow-to-head)
    (let ((case-fold-search t))
      (unless (re-search-forward "^Message-ID[ \t]*:" nil t)
	(insert "Message-ID: " (nnmail-message-id) "\n")))))

(defun nnmail-write-region (start end filename &optional append visit lockname)
  "Do a `write-region', and then set the file modes."
  ;; 1997/5/4 by MORIOKA Tomohiko <morioka@jaist.ac.jp>
  (let ((coding-system-for-write nnmail-file-coding-system)
	;; 1997/8/12 by MORIOKA Tomohiko
	;;	for XEmacs/mule.
	(pathname-coding-system 'binary))
    (write-region start end filename append visit lockname)
    (set-file-modes filename nnmail-default-file-modes)))

;;;
;;; Status functions
;;;

(defun nnmail-replace-status (name value)
  "Make status NAME and VALUE part of the current status line."
  (save-restriction
    (message-narrow-to-head)
    (let ((status (nnmail-decode-status)))
      (setq status (delq (member name status) status))
      (when value
	(push (cons name value) status))
      (message-remove-header "status")
      (goto-char (point-max))
      (insert "Status: " (nnmail-encode-status status) "\n"))))

(defun nnmail-decode-status ()
  "Return a status-value alist from STATUS."
  (goto-char (point-min))
  (when (re-search-forward "^Status: " nil t)
    (let (name value status)
      (save-restriction
	;; Narrow to the status.
	(narrow-to-region
	 (point)
	 (if (re-search-forward "^[^ \t]" nil t)
	     (1- (point))
	   (point-max)))
	;; Go through all elements and add them to the list.
	(goto-char (point-min))
	(while (re-search-forward "[^ \t=]+" nil t)
	  (setq name (match-string 0))
	  (if (not (= (following-char) ?=))
	      ;; Implied "yes".
	      (setq value "yes")
	    (forward-char 1)
	    (if (not (= (following-char) ?\"))
		(if (not (looking-at "[^ \t]"))
		    ;; Implied "no".
		    (setq value "no")
		  ;; Unquoted value.
		  (setq value (match-string 0))
		  (goto-char (match-end 0)))
	      ;; Quoted value.
	      (setq value (read (current-buffer)))))
	  (push (cons name value) status)))
      status)))

(defun nnmail-encode-status (status)
  "Return a status string from STATUS."
  (mapconcat
   (lambda (elem)
     (concat
      (car elem) "="
      (if (string-match "[ \t]" (cdr elem))
	  (prin1-to-string (cdr elem))
	(cdr elem))))
   status " "))

(defun nnmail-split-history ()
  "Generate an overview of where the last mail split put articles."
  (interactive)
  (unless nnmail-split-history
    (error "No current split history"))
  (with-output-to-temp-buffer "*nnmail split history*"
    (let ((history nnmail-split-history)
	  elem)
      (while (setq elem (pop history))
	(princ (mapconcat (lambda (ga)
			    (concat (car ga) ":" (int-to-string (cdr ga))))
			  elem
			  ", "))
	(princ "\n")))))

(defun nnmail-new-mail-p (group)
  "Say whether GROUP has new mail."
  (let ((his nnmail-split-history)
	found)
    (while his
      (when (assoc group (pop his))
	(setq found t
	      his nil)))
    found))

(eval-and-compile
  (autoload 'pop3-movemail "pop3"))

(defun nnmail-pop3-movemail (inbox crashbox)
  "Function to move mail from INBOX on a pop3 server to file CRASHBOX."
  (let ((pop3-maildrop
         (substring inbox (match-end (string-match "^po:" inbox)))))
    (pop3-movemail crashbox)))

(run-hooks 'nnmail-load-hook)

(provide 'nnmail)

;;; nnmail.el ends here
