;;; nnmail.el --- mail support functions for the Gnus mail backends
;; Copyright (C) 1995,96 Free Software Foundation, Inc.

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

(require 'nnheader)
(require 'timezone)
(require 'message)
(eval-when-compile (require 'cl))

(defvar nnmail-split-methods
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

This variable can also have a function as its value.")

;; Suggested by Erik Selberg <speed@cs.washington.edu>.
(defvar nnmail-crosspost t
  "*If non-nil, do crossposting if several split methods match the mail.
If nil, the first match found will be used.")

;; Added by gord@enci.ucalgary.ca (Gordon Matzigkeit).
(defvar nnmail-keep-last-article nil
  "*If non-nil, nnmail will never delete the last expired article in a directory.  
You may need to set this variable if other programs are putting
new mail into folder numbers that Gnus has marked as expired.")

(defvar nnmail-use-long-file-names nil
  "*If non-nil the mail backends will use long file and directory names.
If nil, groups like \"mail.misc\" will end up in directories like
\"mail/misc/\".")

(defvar nnmail-expiry-wait 7
  "*Expirable articles that are older than this will be expired.
This variable can either be a number (which will be interpreted as a
number of days) -- this doesn't have to be an integer.  This variable
can also be `immediate' and `never'.")

(defvar nnmail-expiry-wait-function nil
  "*Variable that holds function to specify how old articles should be before they are expired.
  The function will be called with the name of the group that the
expiry is to be performed in, and it should return an integer that
says how many days an article can be stored before it is considered
\"old\".  It can also return the values `never' and `immediate'.

Eg.:

(setq nnmail-expiry-wait-function
      (lambda (newsgroup)
        (cond ((string-match \"private\" newsgroup) 31)
              ((string-match \"junk\" newsgroup) 1)
	      ((string-match \"important\" newsgroup) 'never)
	      (t 7))))")

(defvar nnmail-spool-file 
  (or (getenv "MAIL")
      (concat "/usr/spool/mail/" (user-login-name)))
  "Where the mail backends will look for incoming mail.
This variable is \"/usr/spool/mail/$user\" by default.
If this variable is nil, no mail backends will read incoming mail.
If this variable is a list, all files mentioned in this list will be
used as incoming mailboxes.")

(defvar nnmail-crash-box "~/.gnus-crash-box"
  "*File where Gnus will store mail while processing it.")

(defvar nnmail-use-procmail nil
  "*If non-nil, the mail backends will look in `nnmail-procmail-directory' for spool files.
The file(s) in `nnmail-spool-file' will also be read.")

(defvar nnmail-procmail-directory "~/incoming/"
  "*When using procmail (and the like), incoming mail is put in this directory.
The Gnus mail backends will read the mail from this directory.")

(defvar nnmail-procmail-suffix "\\.spool"
  "*Suffix of files created by procmail (and the like).
This variable might be a suffix-regexp to match the suffixes of
several files - eg. \".spool[0-9]*\".")

(defvar nnmail-resplit-incoming nil
  "*If non-nil, re-split incoming procmail sorted mail.")

(defvar nnmail-delete-file-function 'delete-file
  "Function called to delete files in some mail backends.")

(defvar nnmail-crosspost-link-function 'add-name-to-file
  "Function called to create a copy of a file.
This is `add-name-to-file' by default, which means that crossposts
will use hard links.  If your file system doesn't allow hard
links, you could set this variable to `copy-file' instead.")

(defvar nnmail-movemail-program "movemail"
  "*A command to be executed to move mail from the inbox.
The default is \"movemail\".")

(defvar nnmail-pop-password-required nil
  "*Non-nil if a password is required when reading mail using POP.")

(defvar nnmail-read-incoming-hook nil
  "*Hook that will be run after the incoming mail has been transferred.
The incoming mail is moved from `nnmail-spool-file' (which normally is
something like \"/usr/spool/mail/$user\") to the user's home
directory. This hook is called after the incoming mail box has been
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
	    (if (eq (process-status \"display-time\") 'run)
		(display-time-filter display-time-process \"\"))))") 

(when (eq system-type 'windows-nt)
  (add-hook 'nnmail-prepare-incoming-hook 'nnheader-ms-strip-cr))

;; Suggested by Erik Selberg <speed@cs.washington.edu>.
(defvar nnmail-prepare-incoming-hook nil
  "*Hook called before treating incoming mail.
The hook is run in a buffer with all the new, incoming mail.")

(defvar nnmail-pre-get-new-mail-hook nil
  "Hook called just before starting to handle new incoming mail.")

(defvar nnmail-post-get-new-mail-hook nil
  "Hook called just after finishing handling new incoming mail.")

;; Suggested by Mejia Pablo J <pjm9806@usl.edu>.
(defvar nnmail-tmp-directory nil
  "*If non-nil, use this directory for temporary storage when reading incoming mail.")

(defvar nnmail-large-newsgroup 50
  "*The number of the articles which indicates a large newsgroup.
If the number of the articles is greater than the value, verbose
messages will be shown to indicate the current status.")

(defvar nnmail-split-fancy "mail.misc"
  "*Incoming mail can be split according to this fancy variable.
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

FIELD must match a complete field name.  VALUE must match a complete
word according to the `nnmail-split-fancy-syntax-table' syntax table.
You can use .* in the regexps to match partial field names or words.

FIELD and VALUE can also be lisp symbols, in that case they are expanded
as specified in `nnmail-split-abbrev-alist'.

Example:

\(setq nnmail-split-methods 'nnmail-split-fancy
      nnmail-split-fancy
      ;; Messages from the mailer deamon are not crossposted to any of
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
	  \"misc.misc\"))")

(defvar nnmail-split-abbrev-alist
  '((any . "from\\|to\\|cc\\|sender\\|apparently-to")
    (mail . "mailer-daemon\\|postmaster"))
  "*Alist of abbreviations allowed in `nnmail-split-fancy'.")

(defvar nnmail-delete-incoming t
  "*If non-nil, the mail backends will delete incoming files after splitting.")

(defvar nnmail-message-id-cache-length 1000
  "*The approximate number of Message-IDs nnmail will keep in its cache.
If this variable is nil, no checking on duplicate messages will be
performed.")

(defvar nnmail-message-id-cache-file "~/.nnmail-cache"
  "*The file name of the nnmail Message-ID cache.")

(defvar nnmail-treat-duplicates 'warn
  "*If non-nil, nnmail keep a cache of Message-IDs to discover mail duplicates.
Three values are legal: nil, which means that nnmail is not to keep a
Message-ID cache; `warn', which means that nnmail should insert extra
headers to warn the user about the duplication (this is the default);
and `delete', which means that nnmail will delete duplicated mails.

This variable can also be a function.  It will be called from a buffer
narrowed to the article in question with the Message-ID as a
parameter.  It should return nil, `warn' or `delete'.")

;;; Internal variables.

(defvar nnmail-pop-password nil
  "*Password to use when reading mail from a POP server, if required.")

(defvar nnmail-split-fancy-syntax-table
  (copy-syntax-table (standard-syntax-table))
  "Syntax table used by `nnmail-split-fancy'.")

(defvar nnmail-prepare-save-mail-hook nil
  "Hook called before saving mail.")

(defvar nnmail-moved-inboxes nil
  "List of inboxes that have been moved.")

(defvar nnmail-internal-password nil)



(defconst nnmail-version "nnmail 1.0"
  "nnmail version.")



(defun nnmail-request-post (&optional server)
  (mail-send-and-exit nil))

(defun nnmail-find-file (file)
  "Insert FILE in server buffer safely."
  (set-buffer nntp-server-buffer)
  (erase-buffer)
  (let ((format-alist nil)
        (after-insert-file-functions nil))
    (condition-case ()
	(progn (insert-file-contents file) t)
      (file-error nil))))

(defun nnmail-group-pathname (group dir &optional file)
  "Make pathname for GROUP."
  (concat
   (let ((dir (file-name-as-directory (expand-file-name dir))))
     ;; If this directory exists, we use it directly.
     (if (or nnmail-use-long-file-names 
	     (file-directory-p (concat dir group)))
	 (concat dir group "/")
       ;; If not, we translate dots into slashes.
       (concat dir (nnheader-replace-chars-in-string group ?. ?/) "/")))
   (or file "")))
  
(defun nnmail-date-to-time (date)
  "Convert DATE into time."
  (let* ((d1 (timezone-parse-date date))
	 (t1 (timezone-parse-time (aref d1 3))))
    (apply 'encode-time
	   (mapcar (lambda (el)
		     (and el (string-to-number el)))
		   (list
		    (aref t1 2) (aref t1 1) (aref t1 0)
		    (aref d1 2) (aref d1 1) (aref d1 0)
		    (aref d1 4))))))

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
	 (rest (if (< (nth 1 current) (nth 1 time)) (expt 2 16))))
    (list (- (+ (car current) (if rest -1 0)) (car time))
	  (- (+ (or rest 0) (nth 1 current)) (nth 1 time)))))

;; Function rewritten from rmail.el.
(defun nnmail-move-inbox (inbox)
  "Move INBOX to `nnmail-crash-box'."
  (let ((inbox (file-truename
		(expand-file-name (substitute-in-file-name inbox))))
	(tofile (file-truename (expand-file-name 
				(substitute-in-file-name nnmail-crash-box))))
	movemail popmail errors password)
    ;; If getting from mail spool directory,
    ;; use movemail to move rather than just renaming,
    ;; so as to interlock with the mailer.
    (unless (setq popmail (string-match "^po:" (file-name-nondirectory inbox)))
      (setq movemail t))
    (when popmail 
      (setq inbox (file-name-nondirectory inbox)))
    (when (and movemail
	       ;; On some systems, /usr/spool/mail/foo is a directory
	       ;; and the actual inbox is /usr/spool/mail/foo/foo.
	       (file-directory-p inbox))
      (setq inbox (expand-file-name (user-login-name) inbox)))
    (if (member inbox nnmail-moved-inboxes)
	nil
      (if popmail
	  (progn
	    (setq nnmail-internal-password nnmail-pop-password)
	    (when (and nnmail-pop-password-required (not nnmail-pop-password))
	      (setq nnmail-internal-password
		    (nnmail-read-passwd
		     (format "Password for %s: "
			     (substring inbox (+ popmail 3))))))
	    (message "Getting mail from post office ..."))
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
       ((and (not movemail) (not popmail))
	;; Try copying.  If that fails (perhaps no space),
	;; rename instead.
	(condition-case nil
	    (copy-file inbox tofile nil)
	  (error
	   ;; Third arg is t so we can replace existing file TOFILE.
	   (rename-file inbox tofile t)))
	(push inbox nnmail-moved-inboxes)
	;; Make the real inbox file empty.
	;; Leaving it deleted could cause lossage
	;; because mailers often won't create the file.
	(condition-case ()
	    (write-region (point) (point) inbox)
	  (file-error nil)))
       (t
	;; Use movemail.
	(unwind-protect
	    (save-excursion
	      (setq errors (generate-new-buffer " *nnmail loss*"))
	      (buffer-disable-undo errors)
	      (let ((default-directory "/"))
		(apply 
		 'call-process
		 (append
		  (list
		   (expand-file-name nnmail-movemail-program exec-directory)
		   nil errors nil inbox tofile)
		  (when nnmail-internal-password
		    (list nnmail-internal-password)))))
	      (if (not (buffer-modified-p errors))
		  ;; No output => movemail won
		  (push inbox nnmail-moved-inboxes)
		(set-buffer errors)
		(subst-char-in-region (point-min) (point-max) ?\n ?\  )
		(goto-char (point-max))
		(skip-chars-backward " \t")
		(delete-region (point) (point-max))
		(goto-char (point-min))
		(if (looking-at "movemail: ")
		    (delete-region (point-min) (match-end 0)))
		(beep t)
		(message (concat "movemail: "
				 (buffer-substring (point-min)
						   (point-max))))
		(sit-for 3)
		(setq tofile nil))))))
      (and errors
	   (buffer-name errors)
	   (kill-buffer errors))
      tofile)))

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

(defun nnmail-save-active (group-assoc file-name)
  "Save GROUP-ASSOC in ACTIVE-FILE."
  (when file-name
    (let (group)
      (save-excursion
	(set-buffer (get-buffer-create " *nnmail active*"))
	(buffer-disable-undo (current-buffer))
	(erase-buffer)
	(while group-assoc
	  (setq group (pop group-assoc))
	  (insert (format "%s %d %d y\n" (car group) (cdadr group) 
			  (caadr group))))
	(unless (file-exists-p (file-name-directory file-name))
	  (make-directory (file-name-directory file-name) t))
	(write-region 1 (point-max) (expand-file-name file-name) nil 'nomesg)
	(kill-buffer (current-buffer))))))

(defun nnmail-get-split-group (file group)
  (if (or (eq nnmail-spool-file 'procmail)
	  nnmail-use-procmail)
      (cond (group group)
	    ((string-match (concat "^" (expand-file-name
					(file-name-as-directory
					 nnmail-procmail-directory))
				   "\\([^/]*\\)" nnmail-procmail-suffix "$")
			   (expand-file-name file))
	     (substring (expand-file-name file)
			(match-beginning 1) (match-end 1)))
	    (t
	     group))
    group))

(defun nnmail-process-babyl-mail-format (func)
  (let ((case-fold-search t)
	start message-id content-length do-search end)
    (while (not (eobp))
      (goto-char (point-min))
      (re-search-forward
       "\n0, *unseen,+\n\\(\\*\\*\\* EOOH \\*\\*\\*\n\\)?" nil t)
      (goto-char (match-end 0))
      (delete-region (match-beginning 0) (match-end 0))
      (setq start (point))
      ;; Skip all the headers in case there are more "From "s...
      (or (search-forward "\n\n" nil t)
	  (search-forward-regexp "^[^:]*\\( .*\\|\\)$" nil t)
	  (search-forward ""))
      ;; Find the Message-ID header.
      (save-excursion
	(if (re-search-backward "^Message-ID:[ \t]*\\(<[^>]*>\\)" nil t)
	    (setq message-id (buffer-substring (match-beginning 1)
					       (match-end 1)))
	  ;; There is no Message-ID here, so we create one.
	  (save-excursion
	    (when (re-search-backward "^Message-ID:" nil t)
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
	(if (or (= (+ (point) content-length) (point-max))
		(save-excursion
		  (goto-char (+ (point) content-length))
		  (looking-at "")))
	    (progn
	      (goto-char (+ (point) content-length))
	      (setq do-search nil))
	  (setq do-search t)))
      ;; Go to the beginning of the next article - or to the end
      ;; of the buffer.  
      (if do-search
	  (if (re-search-forward "^" nil t)
	      (goto-char (match-beginning 0))
	    (goto-char (1- (point-max)))))
      (delete-char 1)			; delete ^_
      (save-excursion
	(save-restriction
	  (narrow-to-region start (point))
	  (goto-char (point-min))
	  (nnmail-check-duplication message-id func)
	  (setq end (point-max))))
      (goto-char end))))

(defun nnmail-search-unix-mail-delim ()
  "Put point at the beginning of the next message."
  (let ((case-fold-search t)
	(delim (concat "^" message-unix-mail-delimiter))
	found)
    (while (not found)
      (if (re-search-forward delim nil t)
	  (when (or (looking-at "[^\n :]+ *:")
		    (looking-at delim)
		    (looking-at (concat ">" message-unix-mail-delimiter)))
	    (forward-line -1)
	    (setq found 'yes))
	(setq found 'no)))
    (eq found 'yes)))

(defun nnmail-process-unix-mail-format (func)
  (let ((case-fold-search t)
	(delim (concat "^" message-unix-mail-delimiter))
	start message-id content-length end skip head-end)
    (goto-char (point-min))
    (if (not (and (re-search-forward delim nil t)
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
	     (forward-line 1)
	     (point))))
	;; Find the Message-ID header.
	(goto-char (point-min))
	(if (re-search-forward "^Message-ID:[ \t]*\\(<[^>]+>\\)" nil t)
	    (setq message-id (match-string 1))
	  (save-excursion
	    (when (re-search-forward "^Message-ID:" nil t)
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
		((looking-at delim)
		 (setq end skip))
		((looking-at
		  (concat "[ \t]*\n\\(" delim "\\)"))
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
	    (nnmail-check-duplication message-id func)
	    (setq end (point-max))))
	(goto-char end)))))

(defun nnmail-process-mmdf-mail-format (func)
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
	     (forward-line 1)
	     (point))))
	;; Find the Message-ID header.
	(goto-char (point-min))
	(if (re-search-forward "^Message-ID:[ \t]*\\(<[^>]+>\\)" nil t)
	    (setq message-id (match-string 1))
	  ;; There is no Message-ID here, so we create one.
	  (save-excursion
	    (when (re-search-backward "^Message-ID:" nil t)
	      (beginning-of-line)
	      (insert "Original-")))
	  (forward-line 1)
	  (insert "Message-ID: " (setq message-id (nnmail-message-id)) "\n"))
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
	    (nnmail-check-duplication message-id func)
	    (setq end (point-max))))
	(goto-char end)
	(forward-line 2)))))

(defun nnmail-split-incoming (incoming func &optional exit-func group)
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
      (nnheader-insert-file-contents-literally incoming)
      (unless (zerop (buffer-size))
	(goto-char (point-min))
	(save-excursion (run-hooks 'nnmail-prepare-incoming-hook))
	;; Handle both babyl, MMDF and unix mail formats, since movemail will
	;; use the former when fetching from a mailbox, the latter when
	;; fetches from a file.
	(cond ((or (looking-at "\^L")
		   (looking-at "BABYL OPTIONS:"))
	       (nnmail-process-babyl-mail-format func))
	      ((looking-at "\^A\^A\^A\^A")
	       (nnmail-process-mmdf-mail-format func))
	      (t
	       (nnmail-process-unix-mail-format func))))
      (if exit-func (funcall exit-func))
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
	(if (and (symbolp nnmail-split-methods)
		 (fboundp nnmail-split-methods))
	    ;; `nnmail-split-methods' is a function, so we just call 
	    ;; this function here and use the result.
	    (setq group-art
		  (mapcar
		   (lambda (group) (cons group (funcall func group)))
		   (condition-case nil
		       (or (funcall nnmail-split-methods)
			   '("bogus"))
		     (error
		      (message 
		       "Error in `nnmail-split-methods'; using `bogus' mail group")
		      (sit-for 1)
		      '("bogus")))))
	  ;; Go through the split methods to find a match.
	  (while (and methods (or nnmail-crosspost (not group-art)))
	    (goto-char (point-max))
	    (setq method (pop methods))
	    (if (or methods
		    (not (equal "" (nth 1 method))))
		(when (and
		       (condition-case () 
			   (if (stringp (nth 1 method))
			       (re-search-backward (cadr method) nil t)
			     ;; Function to say whether this is a match.
			     (funcall (nth 1 method) (car method)))
			 (error nil))
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
	group-art))))

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
	(insert (format " %s:%d" (caar group-alist) (cdar group-alist)))
	(setq group-alist (cdr group-alist)))
      (insert "\n"))))

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
  (cond ((stringp split)
	 ;; A group.
	 (list split))
	((eq (car split) '&)
	 (apply 'nconc (mapcar 'nnmail-split-it (cdr split))))
	((eq (car split) '|)
	 (let (done)
	   (while (and (not done) (cdr split))
	     (setq split (cdr split)
		   done (nnmail-split-it (car split))))
	   done))
	((assq split nnmail-split-cache)
	 ;; A compiled match expression.
	 (goto-char (point-max))
	 (if (re-search-backward (cdr (assq split nnmail-split-cache)) nil t)
	     (nnmail-split-it (nth 2 split))))
	(t
	 ;; An uncompiled match.
	 (let* ((field (nth 0 split))
		(value (nth 1 split))
		(regexp (concat "^\\(" 
				(if (symbolp field)
				    (cdr (assq field 
					       nnmail-split-abbrev-alist))
				  field)
				"\\):.*\\<\\("
				(if (symbolp value)
				    (cdr (assq value
					       nnmail-split-abbrev-alist))
				  value)
				"\\)\\>")))
	   (setq nnmail-split-cache 
		 (cons (cons split regexp) nnmail-split-cache))
	   (goto-char (point-max))
	   (if (re-search-backward regexp nil t)
	       (nnmail-split-it (nth 2 split)))))))

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
			    nnmail-procmail-suffix "$") t)))
	   (p procmails)
	   (crash (when (and (file-exists-p nnmail-crash-box)
			     (> (nnheader-file-size
				 (file-truename nnmail-crash-box)) 0))
		    (list nnmail-crash-box))))
      ;; Remove any directories that inadvertantly match the procmail
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
	      (append nnmail-spool-file procmails))
	     ((stringp nnmail-spool-file)
	      (cons nnmail-spool-file procmails))
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
	    (and (setq file (condition-case ()
				(symbol-value (intern (format "%s-active-file" 
							      backend)))
			      (error nil)))
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
		   (current-time)))
	  (funcall (intern (format "%s-request-list" backend)))
	  (set (intern (format "%s-group-alist" backend)) 
	       (nnmail-get-active))))
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
      (and (file-exists-p nnmail-message-id-cache-file)
	   (insert-file-contents nnmail-message-id-cache-file))
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
      (and (search-backward "\n" nil t nnmail-message-id-cache-length)
	   (progn
	     (beginning-of-line)
	     (delete-region (point-min) (point))))
      ;; Save the buffer.
      (or (file-exists-p (file-name-directory nnmail-message-id-cache-file))
	  (make-directory (file-name-directory nnmail-message-id-cache-file)
			  t))
      (write-region (point-min) (point-max)
		    nnmail-message-id-cache-file nil 'silent)
      (set-buffer-modified-p nil)
      (setq nnmail-cache-buffer nil)
      ;;(kill-buffer (current-buffer))
      )))

(defun nnmail-cache-insert (id)
  (when nnmail-treat-duplicates
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

(defun nnmail-check-duplication (message-id func)
  ;; If this is a duplicate message, then we do not save it.
  (let* ((duplication (nnmail-cache-id-exists-p message-id))
	 (action (when duplication
		   (cond
		    ((memq nnmail-treat-duplicates '(warn delete))
		     nnmail-treat-duplicates)
		    ((nnheader-functionp nnmail-treat-duplicates)
		     (funcall nnmail-treat-duplicates message-id))
		    (t
		     nnmail-treat-duplicates)))))
    (cond
     ((not duplication)
      (nnmail-cache-insert message-id)
      (funcall func))
     ((eq action 'delete)
      (delete-region (point-min) (point-max)))
     ((eq action 'warn)
      ;; We insert a warning.
      (let ((case-fold-search t)
	    (newid (nnmail-message-id)))
	(goto-char (point-min))
	(when (re-search-forward "^message-id:" nil t)
	  (beginning-of-line)
	  (insert "Original-"))
	(beginning-of-line)
	(insert 
	 "Message-ID: " newid "\n"
	 "Gnus-Warning: This is a duplicate of message " message-id "\n")
	(nnmail-cache-insert newid)
	(funcall func)))
     (t
      (funcall func)))))

;;; Get new mail.

(defun nnmail-get-value (&rest args)
  (let ((sym (intern (apply 'format args))))
    (when (boundp sym)
      (symbol-value sym))))

(defun nnmail-get-new-mail (method exit-func temp
				   &optional group spool-func)
  "Read new incoming mail."
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
	;; existance of POPped mail.
	(when (or (string-match "^po:" spool)
		  (and (file-exists-p spool)
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
	     spool-func group)
	    ;; Check whether the inbox is to be moved to the special tmp dir. 
	    (setq incoming
		  (nnmail-make-complex-temp-name 
		   (expand-file-name 
		    (if nnmail-tmp-directory
			(concat 
			 (file-name-as-directory nnmail-tmp-directory)
			 (file-name-nondirectory (concat temp "Incoming")))
		      (concat temp "Incoming")))))
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
(defun nnmail-read-passwd (prompt)
  (unless nnmail-read-passwd
    (if (load "passwd" t)
	(setq nnmail-read-passwd 'read-passwd)
      (autoload 'ange-ftp-read-passwd "ange-ftp")
      (setq nnmail-read-passwd 'ange-ftp-read-passwd)))
  (funcall nnmail-read-passwd prompt))

(defun nnmail-check-syntax ()
  "Check (and modify) the syntax of the message in the current buffer."
  (save-restriction
    (message-narrow-to-head)
    (let ((case-fold-search t))
      (unless (re-search-forward "^Message-Id:" nil t)
	(insert "Message-ID: " (nnmail-message-id) "\n")))))

(run-hooks 'nnmail-load-hook)
	    
(provide 'nnmail)

;;; nnmail.el ends here
