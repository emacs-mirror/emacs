;;; nnmail.el --- mail support functions for the Gnus mail backends
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2002
;;        Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
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
(require 'message)
(require 'custom)
(require 'gnus-util)
(require 'mail-source)
(require 'mm-util)

(eval-and-compile
  (autoload 'gnus-error "gnus-util")
  (autoload 'gnus-buffer-live-p "gnus-util"))

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
  "*Incoming mail will be split according to this variable.

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

(defcustom nnmail-expiry-target 'delete
  "*Variable that says where expired messages should end up.
The default value is `delete' (which says to delete the messages),
but it can also be a string or a function.  If it is a string, expired
messages end up in that group.  If it is a function, the function is
called in a buffer narrowed to the message in question.  The function
receives one argument, the name of the group the message comes from.
The return value should be `delete' or a group name (a string)."
  :version "21.1"
    :group 'nnmail-expire
    :type '(choice (const delete)
		   (function :format "%v" nnmail-)
		   string))

(defcustom nnmail-cache-accepted-message-ids nil
  "If non-nil, put Message-IDs of Gcc'd articles into the duplicate cache."
  :group 'nnmail
  :type 'boolean)

(defcustom nnmail-spool-file '((file))
  "*Where the mail backends will look for incoming mail.
This variable is a list of mail source specifiers.
This variable is obsolete; `mail-sources' should be used instead."
  :group 'nnmail-files
  :type 'sexp)

(defcustom nnmail-resplit-incoming nil
  "*If non-nil, re-split incoming procmail sorted mail."
  :group 'nnmail-procmail
  :type 'boolean)

(defcustom nnmail-scan-directory-mail-source-once nil
  "*If non-nil, scan all incoming procmail sorted mails once.
It scans low-level sorted spools even when not required."
  :version "21.1"
  :group 'nnmail-procmail
  :type 'boolean)

(defcustom nnmail-delete-file-function 'delete-file
  "Function called to delete files in some mail backends."
  :group 'nnmail-files
  :type 'function)

(defcustom nnmail-crosspost-link-function
  (if (string-match "windows-nt\\|emx" (symbol-name system-type))
      'copy-file
    'add-name-to-file)
  "*Function called to create a copy of a file.
This is `add-name-to-file' by default, which means that crossposts
will use hard links.  If your file system doesn't allow hard
links, you could set this variable to `copy-file' instead."
  :group 'nnmail-files
  :type '(radio (function-item add-name-to-file)
		(function-item copy-file)
		(function :tag "Other")))

(defcustom nnmail-read-incoming-hook
  (if (eq system-type 'windows-nt)
      '(nnheader-ms-strip-cr)
    nil)
  "*Hook that will be run after the incoming mail has been transferred.
The incoming mail is moved from the specified spool file (which normally is
something like \"/usr/spool/mail/$user\") to the user's home
directory.  This hook is called after the incoming mail box has been
emptied, and can be used to call any mail box programs you have
running (\"xwatch\", etc.)

Eg.

\(add-hook 'nnmail-read-incoming-hook
          (lambda ()
            (call-process \"/local/bin/mailsend\" nil nil nil
                          \"read\" nnmail-spool-file)))

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
		 (regexp :value ".*")
		 (repeat :value (".*") regexp)))

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

(defcustom nnmail-large-newsgroup 50
  "*The number of the articles which indicates a large newsgroup.
If the number of the articles is greater than the value, verbose
messages will be shown to indicate the current status."
  :group 'nnmail-various
  :type 'integer)

(defcustom nnmail-split-fancy "mail.misc"
  "Incoming mail can be split according to this fancy variable.
To enable this, set `nnmail-split-methods' to `nnmail-split-fancy'.

The format of this variable is SPLIT, where SPLIT can be one of
the following:

GROUP: Mail will be stored in GROUP (a string).

\(FIELD VALUE [- RESTRICT [- RESTRICT [...]]] SPLIT): If the message
  field FIELD (a regexp) contains VALUE (a regexp), store the messages 
  as specified by SPLIT.  If RESTRICT (a regexp) matches some string
  after FIELD and before the end of the matched VALUE, return nil,
  otherwise process SPLIT.  Multiple RESTRICTs add up, further
  restricting the possibility of processing SPLIT.

\(| SPLIT...): Process each SPLIT expression until one of them matches.
  A SPLIT expression is said to match if it will cause the mail
  message to be stored in one or more groups.

\(& SPLIT...): Process each SPLIT expression.

\(: FUNCTION optional args): Call FUNCTION with the optional args, in
  the buffer containing the message headers.  The return value FUNCTION
  should be a split, which is then recursively processed.

\(! FUNCTION SPLIT): Call FUNCTION with the result of SPLIT.  The
  return value FUNCTION should be a split, which is then recursively
  processed.

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
             ;; Both lists below have the same suffix, so prevent
             ;; cross-posting to mkpkg.list of messages posted only to 
             ;; the bugs- list, but allow cross-posting when the
             ;; message was really cross-posted.
             (any \"bugs-mypackage@somewhere\" \"mypkg.bugs\")
             (any \"mypackage@somewhere\" - \"bugs-mypackage\" \"mypkg.list\")
             ;; 
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
  "*Alist of abbreviations allowed in `nnmail-split-fancy'."
  :group 'nnmail-split
  :type '(repeat (cons :format "%v" symbol regexp)))

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
Three values are valid: nil, which means that nnmail is not to keep a
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

(defcustom nnmail-extra-headers nil
  "*Extra headers to parse."
  :version "21.1"
  :group 'nnmail
  :type '(repeat symbol))

(defcustom nnmail-split-header-length-limit 512
  "Header lines longer than this limit are excluded from the split function."
  :version "21.1"
  :group 'nnmail
  :type 'integer)

;;; Internal variables.

(defvar nnmail-split-history nil
  "List of group/article elements that say where the previous split put messages.")

(defvar nnmail-split-fancy-syntax-table nil
  "Syntax table used by `nnmail-split-fancy'.")
(unless (syntax-table-p nnmail-split-fancy-syntax-table)
  (setq nnmail-split-fancy-syntax-table
	(copy-syntax-table (standard-syntax-table)))
  ;; support the %-hack
  (modify-syntax-entry ?\% "." nnmail-split-fancy-syntax-table))

(defvar nnmail-prepare-save-mail-hook nil
  "Hook called before saving mail.")

(defvar nnmail-split-tracing nil)
(defvar nnmail-split-trace nil)



(defconst nnmail-version "nnmail 1.0"
  "nnmail version.")



(defun nnmail-request-post (&optional server)
  (mail-send-and-exit nil))

(defvar nnmail-file-coding-system 'raw-text
  "Coding system used in nnmail.")

(defvar nnmail-incoming-coding-system
  mm-text-coding-system
  "Coding system used in reading inbox")

(defvar nnmail-pathname-coding-system nil
  "*Coding system for pathname.")

(defun nnmail-find-file (file)
  "Insert FILE in server buffer safely."
  (set-buffer nntp-server-buffer)
  (delete-region (point-min) (point-max))
  (let ((format-alist nil)
        (after-insert-file-functions nil))
    (condition-case ()
	(let ((coding-system-for-read nnmail-file-coding-system)
	      (auto-mode-alist (mm-auto-mode-alist))
	      (file-name-coding-system nnmail-pathname-coding-system))
	  (insert-file-contents file)
	  t)
      (file-error nil))))

(defun nnmail-group-pathname (group dir &optional file)
  "Make pathname for GROUP."
  (concat
   (let ((dir (file-name-as-directory (expand-file-name dir))))
     (setq group (nnheader-replace-duplicate-chars-in-string
		  (nnheader-replace-chars-in-string group ?/ ?_)
		  ?. ?_))
     (setq group (nnheader-translate-file-chars group))
     ;; If this directory exists, we use it directly.
     (file-name-as-directory
      (if (or nnmail-use-long-file-names
	      (file-directory-p (concat dir group)))
	  (expand-file-name group dir)
	;; If not, we translate dots into slashes.
	(expand-file-name
	 (mm-encode-coding-string
	  (nnheader-replace-chars-in-string group ?. ?/)
	  nnmail-pathname-coding-system)
	 dir))))
   (or file "")))

(defun nnmail-get-active ()
  "Returns an assoc of group names and active ranges.
nn*-request-list should have been called before calling this function."
  ;; Go through all groups from the active list.
  (save-excursion
    (set-buffer nntp-server-buffer)
    (nnmail-parse-active)))

(defun nnmail-parse-active ()
  "Parse the active file in the current buffer and return an alist."
  (goto-char (point-min))
  (unless (re-search-forward "[\\\"]" nil t)
    (goto-char (point-max))
    (while (re-search-backward "[][';?()#]" nil t)
      (insert ?\\)))
  (goto-char (point-min))
  (let ((buffer (current-buffer))
	group-assoc group max min)
    (while (not (eobp))
      (condition-case err
	  (progn
	    (narrow-to-region (point) (gnus-point-at-eol))
	    (setq group (read buffer))
	    (unless (stringp group)
	      (setq group (symbol-name group)))
	    (if (and (numberp (setq max (read nntp-server-buffer)))
		     (numberp (setq min (read nntp-server-buffer))))
		(push (list group (cons min max))
		      group-assoc)))
	(error nil))
      (widen)
      (forward-line 1))
    group-assoc))

(defvar nnmail-active-file-coding-system 'raw-text
  "*Coding system for active file.")

(defun nnmail-save-active (group-assoc file-name)
  "Save GROUP-ASSOC in ACTIVE-FILE."
  (let ((coding-system-for-write nnmail-active-file-coding-system))
    (when file-name
      (with-temp-file file-name
	(nnmail-generate-active group-assoc)))))

(defun nnmail-generate-active (alist)
  "Generate an active file from group-alist ALIST."
  (erase-buffer)
  (let (group)
    (while (setq group (pop alist))
      (insert (format "%S %d %d y\n" (intern (car group)) (cdadr group)
		      (caadr group))))
    (goto-char (point-max))
    (while (search-backward "\\." nil t)
      (delete-char 1))))

(defun nnmail-get-split-group (file source)
  "Find out whether this FILE is to be split into GROUP only.
If SOURCE is a directory spec, try to return the group name component."
  (if (eq (car source) 'directory)
      (let ((file (file-name-nondirectory file)))
	(mail-source-bind (directory source)
	  (if (string-match (concat (regexp-quote suffix) "$") file)
	      (substring file 0 (match-beginning 0))
	    nil)))
    nil))

(defun nnmail-process-babyl-mail-format (func artnum-func)
  (let ((case-fold-search t)
	(count 0)
	start message-id content-length do-search end)
    (while (not (eobp))
      (goto-char (point-min))
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
	  (incf count)
	  (setq end (point-max))))
      (goto-char end))
    count))

(defsubst nnmail-search-unix-mail-delim ()
  "Put point at the beginning of the next Unix mbox message."
  ;; Algorithm used to find the next article in the
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
			   (eq (char-after) ?\n)))
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
  ;; Algorithm used to find the next article in the
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
			   (eq (char-after) ?\n)))
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
	(count 0)
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
	    (incf count)
	    (nnmail-check-duplication message-id func artnum-func)
	    (setq end (point-max))))
	(goto-char end)))
    count))

(defun nnmail-process-mmdf-mail-format (func artnum-func)
  (let ((delim "^\^A\^A\^A\^A$")
	(case-fold-search t)
	(count 0)
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
	    (incf count)
	    (nnmail-check-duplication message-id func artnum-func)
	    (setq end (point-max))))
	(goto-char end)
	(forward-line 2)))
    count))

(defun nnmail-process-maildir-mail-format (func artnum-func)
  ;; In a maildir, every file contains exactly one mail.
  (let ((case-fold-search t)
	message-id)
    (goto-char (point-min))
    ;; Find the end of the head.
    (narrow-to-region
     (point-min)
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
    (if (re-search-forward "^Message-ID:[ \t]*\\(<[^>]+>\\)" nil t)
	(setq message-id (match-string 1))
      ;; There is no Message-ID here, so we create one.
      (save-excursion
	(when (re-search-backward "^Message-ID[ \t]*:" nil t)
	  (beginning-of-line)
	  (insert "Original-")))
      (forward-line 1)
      (insert "Message-ID: " (setq message-id (nnmail-message-id)) "\n"))
    (run-hooks 'nnmail-prepare-incoming-header-hook)
    ;; Allow the backend to save the article.
    (widen)
    (save-excursion
      (goto-char (point-min))
      (nnmail-check-duplication message-id func artnum-func))
    1))

(defun nnmail-split-incoming (incoming func &optional exit-func
				       group artnum-func)
  "Go through the entire INCOMING file and pick out each individual mail.
FUNC will be called with the buffer narrowed to each mail."
  (let (;; If this is a group-specific split, we bind the split
	;; methods to just this group.
	(nnmail-split-methods (if (and group
				       (not nnmail-resplit-incoming))
				  (list (list group ""))
				nnmail-split-methods)))
    (save-excursion
      ;; Insert the incoming file.
      (set-buffer (get-buffer-create " *nnmail incoming*"))
      (erase-buffer)
      (let ((coding-system-for-read nnmail-incoming-coding-system))
	(mm-insert-file-contents incoming))
      (prog1
	  (if (zerop (buffer-size))
	      0
	    (goto-char (point-min))
	    (save-excursion (run-hooks 'nnmail-prepare-incoming-hook))
	    ;; Handle both babyl, MMDF and unix mail formats, since
	    ;; movemail will use the former when fetching from a
	    ;; mailbox, the latter when fetching from a file.
	    (cond ((or (looking-at "\^L")
		       (looking-at "BABYL OPTIONS:"))
		   (nnmail-process-babyl-mail-format func artnum-func))
		  ((looking-at "\^A\^A\^A\^A")
		   (nnmail-process-mmdf-mail-format func artnum-func))
		  ((looking-at "Return-Path:")
		   (nnmail-process-maildir-mail-format func artnum-func))
		  (t
		   (nnmail-process-unix-mail-format func artnum-func))))
	(when exit-func
	  (funcall exit-func))
	(kill-buffer (current-buffer))))))

(defun nnmail-article-group (func &optional trace)
  "Look at the headers and return an alist of groups that match.
FUNC will be called with the group name to determine the article number."
  (let ((methods nnmail-split-methods)
	(obuf (current-buffer))
	(beg (point-min))
	end group-art method grp)
    (if (and (sequencep methods)
	     (= (length methods) 1))
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
	;; Nuke pathologically long headers.  Since Gnus applies
	;; pathologically complex regexps to the buffer, lines
	;; that are looong will take longer than the Universe's
	;; existence to process.
	(goto-char (point-min))
	(while (not (eobp))
	  (unless (< (move-to-column nnmail-split-header-length-limit)
		     nnmail-split-header-length-limit)
	    (delete-region (point) (progn (end-of-line) (point))))
	  (forward-line 1))
	;; Allow washing.
	(goto-char (point-min))
	(run-hooks 'nnmail-split-hook)
	(when (setq nnmail-split-tracing trace)
	  (setq nnmail-split-trace nil))
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
		      (nnheader-message 5
					"Error in `nnmail-split-methods'; using `bogus' mail group")
		      (sit-for 1)
		      '("bogus")))))
	      (setq split (gnus-remove-duplicates split))
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
	  (while (and methods
		      (or nnmail-crosspost
			  (not group-art)))
	    (goto-char (point-max))
	    (setq method (pop methods)
		  grp (car method))
	    (if (or methods
		    (not (equal "" (nth 1 method))))
		(when (and
		       (ignore-errors
			 (if (stringp (nth 1 method))
			     (let ((expand (string-match "\\\\[0-9&]" grp))
				   (pos (re-search-backward (cadr method)
							    nil t)))
			       (and expand
				    (setq grp (nnmail-expand-newtext grp)))
			       pos)
			   ;; Function to say whether this is a match.
			   (funcall (nth 1 method) grp)))
		       ;; Don't enter the article into the same
		       ;; group twice.
		       (not (assoc grp group-art)))
		  (push (cons grp (funcall func grp))
			group-art))
	      ;; This is the final group, which is used as a
	      ;; catch-all.
	      (unless group-art
		(setq group-art
		      (list (cons (car method)
				  (funcall func (car method)))))))))
	;; Produce a trace if non-empty.
	(when (and trace nnmail-split-trace)
	  (let ((trace (nreverse nnmail-split-trace))
		(restore (current-buffer)))
	    (nnheader-set-temp-buffer "*Split Trace*")
	    (gnus-add-buffer)
	    (while trace
	      (insert (car trace) "\n")
	      (setq trace (cdr trace)))
	    (goto-char (point-min))
	    (gnus-configure-windows 'split-trace)
	    (set-buffer restore)))
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
      (unless (search-forward "\n\n" nil t) 
	(goto-char (point-max))
	(insert "\n"))
      (setq chars (- (point-max) (point)))
      (setq lines (count-lines (point) (point-max)))
      (forward-char -1)
      (save-excursion
	(when (re-search-backward "^Lines: " nil t)
	  (delete-region (point) (progn (forward-line 1) (point)))))
      (beginning-of-line)
      (insert (format "Lines: %d\n" (max lines 0)))
      chars)))

(defun nnmail-insert-xref (group-alist)
  "Insert an Xref line based on the (group . article) alist."
  (save-excursion
    (goto-char (point-min))
    (unless (search-forward "\n\n" nil t)
      (goto-char (point-max))
      (insert "\n"))
    (forward-char -1)
    (when (re-search-backward "^Xref: " nil t)
      (delete-region (match-beginning 0)
		     (progn (forward-line 1) (point))))
    (insert (format "Xref: %s" (system-name)))
    (while group-alist
      (insert (format " %s:%d"
		      (mm-encode-coding-string
		       (caar group-alist)
		       nnmail-pathname-coding-system)
		      (cdar group-alist)))
      (setq group-alist (cdr group-alist)))
    (insert "\n")))

;;; Message washing functions

(defun nnmail-remove-leading-whitespace ()
  "Remove excessive whitespace from all headers."
  (goto-char (point-min))
  (while (re-search-forward "^\\([^ :]+: \\) +" nil t)
    (replace-match "\\1" t)))

(defun nnmail-remove-list-identifiers ()
  "Remove list identifiers from Subject headers."
  (let ((regexp (if (stringp nnmail-list-identifiers) nnmail-list-identifiers
		  (mapconcat 'identity nnmail-list-identifiers " *\\|"))))
    (when regexp
      (goto-char (point-min))
      (when (re-search-forward
	     (concat "^Subject: +\\(Re: +\\)?\\(" regexp " *\\)")
	     nil t)
	(delete-region (match-beginning 2) (match-end 0))))))

(defun nnmail-remove-tabs ()
  "Translate TAB characters into SPACE characters."
  (subst-char-in-region (point-min) (point-max) ?\t ?  t))

(defun nnmail-fix-eudora-headers ()
  "Eudora has a broken References line, but an OK In-Reply-To."
  (goto-char (point-min))
  (when (re-search-forward "^X-Mailer:.*Eudora" nil t)
    (goto-char (point-min))
    (when (re-search-forward "^References:" nil t)
      (beginning-of-line)
      (insert "X-Gnus-Broken-Eudora-"))
    (goto-char (point-min))
    (when (re-search-forward "^In-Reply-To:[^\n]+\\(\n[ \t]+\\)" nil t)
      (replace-match "" t t nil 1))))

(custom-add-option 'nnmail-prepare-incoming-header-hook
		   'nnmail-fix-eudora-headers)

;;; Utility functions

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
  (let (cached-pair)
    (cond
     ;; nil split
     ((null split)
      nil)

     ;; A group name.  Do the \& and \N subs into the string.
     ((stringp split)
      (when nnmail-split-tracing
	(push (format "\"%s\"" split) nnmail-split-trace))
      (list (nnmail-expand-newtext split)))

     ;; Junk the message.
     ((eq split 'junk)
      (when nnmail-split-tracing
	(push "junk" nnmail-split-trace))
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
      (nnmail-split-it (save-excursion (eval (cdr split)))))

     ;; Builtin ! operation.
     ((eq (car split) '!)
      (funcall (cadr split) (nnmail-split-it (caddr split))))

     ;; Check the cache for the regexp for this split.
     ((setq cached-pair (assq split nnmail-split-cache))
      (let (split-result
	    (end-point (point-max))
	    (value (nth 1 split)))
	(if (symbolp value)
	    (setq value (cdr (assq value nnmail-split-abbrev-alist))))
	(while (and (goto-char end-point)
		    (re-search-backward (cdr cached-pair) nil t))
	  (when nnmail-split-tracing
	    (push (cdr cached-pair) nnmail-split-trace))
	  (let ((split-rest (cddr split))
		(end (match-end 0))
		;; The searched regexp is \(\(FIELD\).*\)\(VALUE\).  So,
		;; start-of-value is the point just before the
		;; beginning of the value, whereas after-header-name is
		;; the point just after the field name.
		(start-of-value (match-end 1))
		(after-header-name (match-end 2)))
	    ;; Start the next search just before the beginning of the
	    ;; VALUE match.
	    (setq end-point (1- start-of-value))
	    ;; Handle - RESTRICTs
	    (while (eq (car split-rest) '-)
	      ;; RESTRICT must start after-header-name and
	      ;; end after start-of-value, so that, for
	      ;; (any "foo" - "x-foo" "foo.list")
	      ;; we do not exclude foo.list just because
	      ;; the header is: ``To: x-foo, foo''
	      (goto-char end)
	      (if (and (re-search-backward (cadr split-rest)
					   after-header-name t)
		       (> (match-end 0) start-of-value))
		  (setq split-rest nil)
		(setq split-rest (cddr split-rest))))
	    (when split-rest
	      (goto-char end)
	      (let ((value (nth 1 split)))
		(if (symbolp value)
		    (setq value (cdr (assq value nnmail-split-abbrev-alist))))
		;; Someone might want to do a \N sub on this match, so get the
		;; correct match positions.
		(re-search-backward value start-of-value))
	      (dolist (sp (nnmail-split-it (car split-rest)))
		(unless (memq sp split-result)
		  (push sp split-result))))))
	split-result))

     ;; Not in cache, compute a regexp for the field/value pair.
     (t
      (let* ((field (nth 0 split))
	     (value (nth 1 split))
	     partial regexp)
	(if (symbolp value)
	    (setq value (cdr (assq value nnmail-split-abbrev-alist))))
	(if (and (>= (length value) 2)
		 (string= ".*" (substring value 0 2)))
	    (setq value (substring value 2)
		  partial ""))
	(setq regexp (concat "^\\(\\("
			     (if (symbolp field)
				 (cdr (assq field nnmail-split-abbrev-alist))
			       field)
			     "\\):.*\\)"
			     (or partial "\\<")
			     "\\("
			     value
			     "\\)\\>"))
	(push (cons split regexp) nnmail-split-cache)
	;; Now that it's in the cache, just call nnmail-split-it again
	;; on the same split, which will find it immediately in the cache.
	(nnmail-split-it split))))))

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
	;; We hit a \; expand it.
	(setq did-expand t
	      pos (1+ pos)
	      c (aref newtext pos))
	(if (not (or (= c ?\&)
		     (and (>= c ?1)
			  (<= c ?9))))
	    ;; \ followed by some character we don't expand.
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

;; Activate a backend only if it isn't already activated.
;; If FORCE, re-read the active file even if the backend is
;; already activated.
(defun nnmail-activate (backend &optional force)
  (nnheader-init-server-buffer)
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

;; Compiler directives.
(defvar group)
(defvar group-art-list)
(defvar group-art)
(defun nnmail-cache-insert (id)
  (when nnmail-treat-duplicates
    ;; Store some information about the group this message is written
    ;; to.  This function might have been called from various places.
    ;; Sometimes, a function up in the calling sequence has an
    ;; argument GROUP which is bound to a string, the group name.  At
    ;; other times, there is a function up in the calling sequence
    ;; which has an argument GROUP-ART which is a list of pairs, and
    ;; the car of a pair is a group name.  Should we check that the
    ;; length of the list is equal to 1? -- kai
    (let ((g nil))
      (cond ((and (boundp 'group) group)
             (setq g group))
            ((and (boundp 'group-art-list) group-art-list
                  (listp group-art-list))
             (setq g (caar group-art-list)))
            ((and (boundp 'group-art) group-art (listp group-art))
             (setq g (caar group-art)))
            (t (setq g "")))
      (unless (gnus-buffer-live-p nnmail-cache-buffer)
        (nnmail-cache-open))
      (save-excursion
        (set-buffer nnmail-cache-buffer)
        (goto-char (point-max))
        (if (and g (not (string= "" g))
                 (gnus-methods-equal-p gnus-command-method
                                       (nnmail-cache-primary-mail-backend)))
            (insert id "\t" g "\n")
          (insert id "\n"))))))

(defun nnmail-cache-primary-mail-backend ()
  (let ((be-list (cons gnus-select-method gnus-secondary-select-methods))
        (be nil)
        (res nil))
    (while (and (null res) be-list)
      (setq be (car be-list))
      (setq be-list (cdr be-list))
      (when (and (gnus-method-option-p be 'respool)
                 (eval (intern (format "%s-get-new-mail" (car be)))))
        (setq res be)))
    res))

;; Fetch the group name corresponding to the message id stored in the
;; cache.
(defun nnmail-cache-fetch-group (id)
  (when (and nnmail-treat-duplicates nnmail-cache-buffer)
    (save-excursion
      (set-buffer nnmail-cache-buffer)
      (goto-char (point-max))
      (when (search-backward id nil t)
        (beginning-of-line)
        (skip-chars-forward "^\n\r\t")
        (unless (eolp)
          (forward-char 1)
          (buffer-substring (point)
                            (progn (end-of-line) (point))))))))

;; Function for nnmail-split-fancy: look up all references in the
;; cache and if a match is found, return that group.
(defun nnmail-split-fancy-with-parent ()
  (let* ((refstr (or (message-fetch-field "references")
                     (message-fetch-field "in-reply-to")))
         (references nil)
         (res nil))
    (when refstr
      (setq references (nreverse (gnus-split-references refstr)))
      (unless (gnus-buffer-live-p nnmail-cache-buffer)
        (nnmail-cache-open))
      (mapcar (lambda (x)
                (setq res (or (nnmail-cache-fetch-group x) res))
                (when (string= "drafts" res)
                  (setq res nil)))
              references)
      res)))

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
    ;; We insert a line that says what the mail source is.
    (let ((case-fold-search t))
      (goto-char (point-min))
      (re-search-forward "^message-id[ \t]*:" nil t)
      (beginning-of-line)
      (insert (format "X-Gnus-Mail-Source: %s\n" mail-source-string)))

    ;; Let the backend save the article (or not).
    (cond
     ((not duplication)
      (funcall func (setq group-art
			  (nreverse (nnmail-article-group artnum-func))))
      (nnmail-cache-insert message-id))
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

(defvar nnmail-fetched-sources nil)

(defun nnmail-get-value (&rest args)
  (let ((sym (intern (apply 'format args))))
    (when (boundp sym)
      (symbol-value sym))))

(defun nnmail-get-new-mail (method exit-func temp
				   &optional group spool-func)
  "Read new incoming mail."
  (let* ((sources (or mail-sources
		      (if (listp nnmail-spool-file) nnmail-spool-file
			(list nnmail-spool-file))))
	 fetching-sources
	 (group-in group)
	 (i 0)
	 (new 0)
	 (total 0)
	 incoming incomings source)
    (when (and (nnmail-get-value "%s-get-new-mail" method)
	       sources)
      (while (setq source (pop sources))
	;; Be compatible with old values.
	(cond
	 ((stringp source)
	  (setq source
		(cond
		 ((string-match "^po:" source)
		  (list 'pop :user (substring source (match-end 0))))
		 ((file-directory-p source)
		  (list 'directory :path source))
		 (t
		  (list 'file :path source)))))
	 ((eq source 'procmail)
	  (message "Invalid value for nnmail-spool-file: `procmail'")
	  nil))
	;; Hack to only fetch the contents of a single group's spool file.
	(when (and (eq (car source) 'directory)
		   (null nnmail-scan-directory-mail-source-once)
		   group)
	  (mail-source-bind (directory source)
	    (setq source (append source
				 (list
				  :predicate
				  `(lambda (file)
				     (string-match
				      ,(concat
					(regexp-quote (concat group suffix))
					"$")
				      file)))))))
	(when nnmail-fetched-sources
	  (if (member source nnmail-fetched-sources)
	      (setq source nil)
	    (push source nnmail-fetched-sources)
	    (push source fetching-sources)))))
    (when fetching-sources
      ;; We first activate all the groups.
      (nnmail-activate method)
      ;; Allow the user to hook.
      (run-hooks 'nnmail-pre-get-new-mail-hook)
      ;; Open the message-id cache.
      (nnmail-cache-open)
      ;; The we go through all the existing mail source specification
      ;; and fetch the mail from each.
      (while (setq source (pop fetching-sources))
	(nnheader-message 4 "%s: Reading incoming mail from %s..."
			  method (car source))
	(when (setq new
		    (mail-source-fetch
		     source
		     `(lambda (file orig-file)
			(nnmail-split-incoming
			 file ',(intern (format "%s-save-mail" method))
			 ',spool-func
			 (if (equal file orig-file)
			     nil
			   (nnmail-get-split-group orig-file ',source))
			 ',(intern (format "%s-active-number" method))))))
	  (incf total new)
	  (incf i)))
      ;; If we did indeed read any incoming spools, we save all info.
      (if (zerop total)
	  (nnheader-message 4 "%s: Reading incoming mail (no new mail)...done"
			    method (car source))
	(nnmail-save-active
	 (nnmail-get-value "%s-group-alist" method)
	 (nnmail-get-value "%s-active-file" method))
	(when exit-func
	  (funcall exit-func))
	(run-hooks 'nnmail-read-incoming-hook)
	(nnheader-message 4 "%s: Reading incoming mail (%d new)...done" method
			  total))
      ;; Close the message-id cache.
      (nnmail-cache-close)
      ;; Allow the user to hook.
      (run-hooks 'nnmail-post-get-new-mail-hook))))

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
	     (setq days (days-to-time days))
	     ;; Compare the time with the current time.
	     (ignore-errors (time-less-p days (time-since time))))))))

(defun nnmail-expiry-target-group (target group)
  (when (nnheader-functionp target)
    (setq target (funcall target group)))
  (unless (eq target 'delete)
    (gnus-request-accept-article target nil nil t)))

(defun nnmail-check-syntax ()
  "Check (and modify) the syntax of the message in the current buffer."
  (save-restriction
    (message-narrow-to-head)
    (let ((case-fold-search t))
      (unless (re-search-forward "^Message-ID[ \t]*:" nil t)
	(insert "Message-ID: " (nnmail-message-id) "\n")))))

(defun nnmail-write-region (start end filename &optional append visit lockname)
  "Do a `write-region', and then set the file modes."
  (let ((coding-system-for-write nnmail-file-coding-system)
	(file-name-coding-system nnmail-pathname-coding-system))
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
	  (if (not (eq (char-after) ?=))
	      ;; Implied "yes".
	      (setq value "yes")
	    (forward-char 1)
	    (if (not (eq (char-after) ?\"))
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
    (with-current-buffer standard-output
      (fundamental-mode))		; for Emacs 20.4+
    (let ((history nnmail-split-history)
	  elem)
      (while (setq elem (pop history))
	(princ (mapconcat (lambda (ga)
			    (concat (car ga) ":" (int-to-string (cdr ga))))
			  elem
			  ", "))
	(princ "\n")))))

(defun nnmail-purge-split-history (group)
  "Remove all instances of GROUP from `nnmail-split-history'."
  (let ((history nnmail-split-history))
    (while history
      (setcar history (gnus-delete-if (lambda (e) (string= (car e) group))
				      (car history)))
      (pop history))
    (setq nnmail-split-history (delq nil nnmail-split-history))))

(defun nnmail-new-mail-p (group)
  "Say whether GROUP has new mail."
  (let ((his nnmail-split-history)
	found)
    (while his
      (when (assoc group (pop his))
	(setq found t
	      his nil)))
    found))

(defun nnmail-within-headers-p ()
  "Check to see if point is within the headers of a unix mail message.
Doesn't change point."
  (let ((pos (point)))
    (save-excursion
      (and (nnmail-search-unix-mail-delim-backward)
	   (not (search-forward "\n\n" pos t))))))

(run-hooks 'nnmail-load-hook)

(provide 'nnmail)

;;; nnmail.el ends here
