;;; nnmail.el --- mail support functions for the Gnus mail backends

;; Copyright (C) 1995 Free Software Foundation, Inc.

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
(require 'rmail)
(require 'timezone)
(require 'sendmail)

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
  "*If non-nil, nnmail will never delete the last expired article in a
directory.  You may need to set this variable if other programs are putting
new mail into folder numbers that Gnus has marked as expired.")

(defvar nnmail-expiry-wait 7
  "*Articles that are older than `nnmail-expiry-wait' days will be expired.")

(defvar nnmail-expiry-wait-function nil
  "*Variable that holds function to specify how old articles should be before they are expired.
  The function will be called with the name of the group that the
expiry is to be performed in, and it should return an integer that
says how many days an article can be stored before it is considered
'old'. 

Eg.:

(setq nnmail-expiry-wait-function
      (lambda (newsgroup)
        (cond ((string-match \"private\" newsgroup) 31)
              ((string-match \"junk\" newsgroup) 1)
	      (t 7))))")

(defvar nnmail-spool-file 
  (or (getenv "MAIL")
      (concat "/usr/spool/mail/" (user-login-name)))
  "Where the mail backends will look for incoming mail.
This variable is \"/usr/spool/mail/$user\" by default.
If this variable is nil, no mail backends will read incoming mail.
If this variable is a list, all files mentioned in this list will be
used as incoming mailboxes.")

(defvar nnmail-use-procmail nil
  "*If non-nil, the mail backends will look in `nnmail-procmail-directory' for spool files.
The file(s) in `nnmail-spool-file' will also be read.")

(defvar nnmail-procmail-directory "~/incoming/"
  "*When using procmail (and the like), incoming mail is put in this directory.
The Gnus mail backends will read the mail from this directory.")

(defvar nnmail-procmail-suffix ".spool"
  "*Suffix of files created by procmail (and the like).
This variable might be a suffix-regexp to match the suffixes of
several files - eg. \".spool[0-9]*\".")

(defvar nnmail-resplit-incoming nil
  "*If non-nil, re-split incoming procmail sorted mail.")

(defvar nnmail-movemail-program "movemail"
  "*A command to be executed to move mail from the inbox.
The default is \"movemail\".")

(defvar nnmail-read-incoming-hook nil
  "*Hook that will be run after the incoming mail has been transferred.
The incoming mail is moved from `nnmail-spool-file' (which normally is
something like \"/usr/spool/mail/$user\") to the user's home
directory. This hook is called after the incoming mail box has been
emptied, and can be used to call any mail box programs you have
running (\"xwatch\", etc.)

Eg.

(add-hook 'nnmail-read-incoming-hook 
	   (lambda () 
	     (start-process \"mailsend\" nil 
			    \"/local/bin/mailsend\" \"read\" \"mbox\")))")

;; Suggested by Erik Selberg <speed@cs.washington.edu>.
(defvar nnmail-prepare-incoming-hook nil
  "*Hook called before treating incoming mail.
The hook is run in a buffer with all the new, incoming mail.")

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
word according to the fundamental mode syntax table.  You can use .*
in the regexps to match partial field names or words.

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

(defvar nnmail-delete-duplicates nil
  "*If non-nil, nnmail will delete any duplicate mails it sees.")



(defconst nnmail-version "nnmail 1.0"
  "nnmail version.")



(defun nnmail-request-post (&optional server)
  (mail-send-and-exit nil))

(defun nnmail-request-post-buffer (post group subject header article-buffer
					info follow-to respect-poster)
  (let ((method-address (cdr (assq 'to-address (nth 5 info))))
	from date to reply-to message-of
	references message-id cc new-cc sendto elt)
    (setq method-address
	  (if (and (stringp method-address) 
		   (string= method-address ""))
	      nil method-address))
    (save-excursion
      (set-buffer (get-buffer-create "*mail*"))
      (mail-mode)
      (local-set-key "\C-c\C-c" 'gnus-mail-send-and-exit)
      (if (and (buffer-modified-p)
	       (> (buffer-size) 0)
	       (not (y-or-n-p "Unsent mail being composed; erase it? ")))
	  ()
	(erase-buffer)
	(if post
	    (progn
	      (mail-setup method-address subject nil nil nil nil)
	      (auto-save-mode auto-save-default))
	  (save-excursion
	    (set-buffer article-buffer)
	    (goto-char (point-min))
	    (narrow-to-region (point-min)
			      (progn (search-forward "\n\n") (point)))
	    (let ((buffer-read-only nil))
	      (set-text-properties (point-min) (point-max) nil))
	    (setq from (mail-header-from header))
	    (setq date (mail-header-date header))
	    (and from
		 (let ((stop-pos 
			(string-match "  *at \\|  *@ \\| *(\\| *<" from)))
		   (setq message-of
			 (concat (if stop-pos (substring from 0 stop-pos) from)
				 "'s message of " date))))
	    (setq cc (mail-strip-quoted-names (or (mail-fetch-field "cc") "")))
	    (setq to (mail-strip-quoted-names (or (mail-fetch-field "to") "")))
	    (setq new-cc (rmail-dont-reply-to 
			  (concat (or to "")
				  (if cc (concat (if to ", " "") cc) ""))))
	    (let ((rmail-dont-reply-to-names 
		   (regexp-quote (mail-strip-quoted-names
				  (or method-address reply-to from "")))))
	      (setq new-cc (rmail-dont-reply-to new-cc)))
	    (setq subject (mail-header-subject header))
	    (or (string-match "^[Rr][Ee]:" subject)
		(setq subject (concat "Re: " subject)))
	    (setq reply-to (mail-fetch-field "reply-to"))
	    (setq references (mail-header-references header))
	    (setq message-id (mail-header-id header))
	    (widen))
	  (setq news-reply-yank-from from)
	  (setq news-reply-yank-message-id message-id)
	  
	  ;; Gather the "to" addresses out of the follow-to list and remove
	  ;; them as we go.
	  (if (and follow-to (listp follow-to))
	      (while (setq elt (assoc "To" follow-to))
		(setq sendto (concat sendto (and sendto ", ") (cdr elt)))
		(setq follow-to (delq elt follow-to))))
	  (mail-setup (if (and follow-to (listp follow-to)) 
			  sendto
			(or method-address reply-to from ""))
		      subject message-of 
		      (if (zerop (length new-cc)) nil new-cc)
		      article-buffer nil)
	  (auto-save-mode auto-save-default)
	  ;; Note that "To" elements should already be in the message.
	  (if (and follow-to (listp follow-to))
	      (progn
		(goto-char (point-min))
		(re-search-forward "^To:" nil t)
		(beginning-of-line)
		(forward-line 1)
		(while follow-to
		  (insert 
		   (car (car follow-to)) ": " (cdr (car follow-to)) "\n")
		  (setq follow-to (cdr follow-to)))))
	  (nnheader-insert-references references message-id)))
      (current-buffer))))

(defun nnmail-find-file (file)
  "Insert FILE in server buffer safely."
  (set-buffer nntp-server-buffer)
  (erase-buffer)
  (condition-case ()
      (progn (insert-file-contents file) t)
    (file-error nil)))

(defun nnmail-article-pathname (group mail-dir)
  "Make pathname for GROUP."
  (concat (file-name-as-directory (expand-file-name mail-dir))
	  (nnmail-replace-chars-in-string group ?. ?/) "/"))

(defun nnmail-replace-chars-in-string (string from to)
  "Replace characters in STRING from FROM to TO."
  (let ((string (substring string 0))	;Copy string.
	(len (length string))
	(idx 0))
    ;; Replace all occurrences of FROM with TO.
    (while (< idx len)
      (if (= (aref string idx) from)
	  (aset string idx to))
      (setq idx (1+ idx)))
    string))

(defun nnmail-days-between (date1 date2)
  ;; Return the number of days between date1 and date2.
  (let ((d1 (mapcar (lambda (s) (and s (string-to-int s)) )
		    (timezone-parse-date date1)))
	(d2 (mapcar (lambda (s) (and s (string-to-int s)) )
		    (timezone-parse-date date2))))
    (- (timezone-absolute-from-gregorian 
	(nth 1 d1) (nth 2 d1) (car d1))
       (timezone-absolute-from-gregorian 
	(nth 1 d2) (nth 2 d2) (car d2)))))

;; Function taken from rmail.el.
(defun nnmail-move-inbox (inbox tofile)
  (let ((inbox (file-truename
		(expand-file-name (substitute-in-file-name inbox))))
	movemail popmail errors)
    ;; Check whether the inbox is to be moved to the special tmp dir. 
    (if nnmail-tmp-directory
	(setq tofile (concat (file-name-as-directory nnmail-tmp-directory)
			     (file-name-nondirectory tofile))))
    ;; Make the filename unique.
    (setq tofile (nnmail-make-complex-temp-name (expand-file-name tofile)))
    ;; We create the directory the tofile is to reside in if it
    ;; doesn't exist.
    (or (file-exists-p (file-name-directory tofile))
	(make-directory tofile 'parents))
    ;; If getting from mail spool directory,
    ;; use movemail to move rather than just renaming,
    ;; so as to interlock with the mailer.
    (or (setq popmail (string-match "^po:" (file-name-nondirectory inbox)))
	(setq movemail t))
    (if popmail (setq inbox (file-name-nondirectory inbox)))
    (if movemail
	;; On some systems, /usr/spool/mail/foo is a directory
	;; and the actual inbox is /usr/spool/mail/foo/foo.
	(if (file-directory-p inbox)
	    (setq inbox (expand-file-name (user-login-name) inbox))))
    (if popmail
	(message "Getting mail from post office ...")
      (if (or (and (file-exists-p tofile)
		   (/= 0 (nth 7 (file-attributes tofile))))
	      (and (file-exists-p inbox)
		   (/= 0 (nth 7 (file-attributes inbox)))))
	  (message "Getting mail from %s..." inbox)))
    ;; Set TOFILE if have not already done so, and
    ;; rename or copy the file INBOX to TOFILE if and as appropriate.
    (cond ((or (file-exists-p tofile) (and (not popmail)
					   (not (file-exists-p inbox))))
	   nil)
	  ((and (not movemail) (not popmail))
	   ;; Try copying.  If that fails (perhaps no space),
	   ;; rename instead.
	   (condition-case nil
	       (copy-file inbox tofile nil)
	     (error
	      ;; Third arg is t so we can replace existing file TOFILE.
	      (rename-file inbox tofile t)))
	   ;; Make the real inbox file empty.
	   ;; Leaving it deleted could cause lossage
	   ;; because mailers often won't create the file.
	   (condition-case ()
	       (write-region (point) (point) inbox)
	     (file-error nil)))
	  (t
	   (unwind-protect
	       (save-excursion
		 (setq errors (generate-new-buffer " *nnmail loss*"))
		 (buffer-disable-undo errors)
		 (call-process
		  (expand-file-name nnmail-movemail-program exec-directory)
		  nil errors nil inbox tofile)
		 (if (not (buffer-modified-p errors))
		     ;; No output => movemail won
		     nil
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
		   nil)))))
    (and errors
	 (buffer-name errors)
	 (kill-buffer errors))
    tofile))


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
	(setq group-assoc
	      (cons (list (buffer-substring (match-beginning 1) 
					    (match-end 1))
			  (cons (string-to-int 
				 (buffer-substring (match-beginning 3)
						   (match-end 3)))
				(string-to-int 
				 (buffer-substring (match-beginning 2)
						   (match-end 2)))))
		    group-assoc))))

    ;;    ;; In addition, add all groups mentioned in `nnmail-split-methods'.
    ;;    (let ((methods (and (not (symbolp nnmail-split-methods))
    ;;			nnmail-split-methods)))
    ;;      (while methods
    ;;	(if (not (assoc (car (car methods)) group-assoc))
    ;;	    (setq group-assoc
    ;;		  (cons (list (car (car methods)) (cons 1 0)) 
    ;;			group-assoc)))
    ;;	(setq methods (cdr methods)))
    
    group-assoc))

(defun nnmail-save-active (group-assoc file-name)
  (let (group)
    (save-excursion
      (set-buffer (get-buffer-create " *nnmail active*"))
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (while group-assoc
	(setq group (car group-assoc))
	(insert (format "%s %d %d y\n" (car group) (cdr (car (cdr group)) )
			(car (car (cdr group)))))
	(setq group-assoc (cdr group-assoc)))
      (write-region 1 (point-max) (expand-file-name file-name) nil 'nomesg)
      (kill-buffer (current-buffer)))))

(defun nnmail-get-split-group (file group)
  (if (or (eq nnmail-spool-file 'procmail)
	  nnmail-use-procmail)
      (cond (group group)
	    ((string-match (concat "^" (expand-file-name
					(file-name-as-directory
					 nnmail-procmail-directory))
				   "\\(.*\\)" nnmail-procmail-suffix "$")
			   (expand-file-name file))
	     (substring (expand-file-name file)
			(match-beginning 1) (match-end 1)))
	    (t
	     group))
    group))

(defun nnmail-split-incoming (incoming func &optional dont-kill group)
  "Go through the entire INCOMING file and pick out each individual mail.
FUNC will be called with the buffer narrowed to each mail."
  (let ((delim (concat "^" rmail-unix-mail-delimiter))
	;; If this is a group-specific split, we bind the split
	;; methods to just this group.
	(nnmail-split-methods (if (and group
				       (or (eq nnmail-spool-file 'procmail)
					   nnmail-use-procmail)
				       (not nnmail-resplit-incoming))
				  (list (list group ""))
				nnmail-split-methods))
	start end content-length do-search message-id)
    (save-excursion
      ;; Open the message-id cache.
      (nnmail-cache-open)
      ;; Insert the incoming file.
      (set-buffer (get-buffer-create " *nnmail incoming*"))
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (insert-file-contents incoming)
      (goto-char (point-min))
      (save-excursion (run-hooks 'nnmail-prepare-incoming-hook))
      ;; Go to the beginning of the first mail...
      (if (and (re-search-forward delim nil t)
	       (goto-char (match-beginning 0)))
	  ;; and then carry on until the bitter end.
	  (while (not (eobp))
	    (setq start (point))
	    ;; Skip all the headers in case there are more "From "s...
	    (if (not (search-forward "\n\n" nil t))
		(forward-line 1))
	    ;; Find the Message-ID header.
	    (save-excursion
	      (if (re-search-backward "^Message-ID:[ \t]*\\(<[^>]*>\\)" nil t)
		  (setq message-id (buffer-substring (match-beginning 1)
						     (match-end 1)))
		;; There is no Message-ID here, so we create one.
		(forward-line -1)
		(insert "Message-ID: " (setq message-id (nnmail-message-id))
			"\n")))
	    ;; Look for a Content-Length header.
	    (if (not (save-excursion
		       (and (re-search-backward 
			     "^Content-Length: \\([0-9]+\\)" start t)
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
			(looking-at delim)))
		  (progn
		    (goto-char (+ (point) content-length))
		    (setq do-search nil))
		(setq do-search t)))
	    ;; Go to the beginning of the next article - or to the end
	    ;; of the buffer.  
	    (if do-search
		(if (re-search-forward delim nil t)
		    (goto-char (match-beginning 0))
		  (goto-char (point-max))))
	    (save-excursion
	      (save-restriction
		(narrow-to-region start (point))
		(goto-char (point-min))
		;; If this is a duplicate message, then we do not save it.
		(if (nnmail-cache-id-exists-p message-id)
		    (delete-region (point-min) (point-max))
		  (nnmail-cache-insert message-id)
		  (funcall func))
		(setq end (point-max))))
	    (goto-char end)))
      ;; Close the message-id cache.
      (nnmail-cache-close)
      (if dont-kill
	  (current-buffer)
	(kill-buffer (current-buffer))))))

;; Mail crossposts suggested by Brian Edmonds <edmonds@cs.ubc.ca>. 
(defun nnmail-article-group (func)
  "Look at the headers and return an alist of groups that match.
FUNC will be called with the group name to determine the article number."
  (let ((methods nnmail-split-methods)
	(obuf (current-buffer))
	(beg (point-min))
	end group-art)
    (if (and (sequencep methods) (= (length methods) 1))
	;; If there is only just one group to put everything in, we
	;; just return a list with just this one method in.
	(setq group-art
	      (list (cons (car (car methods))
			  (funcall func (car (car methods))))))
      ;; We do actual comparison.
      (save-excursion
	;; Find headers.
	(goto-char beg)
	(setq end (if (search-forward "\n\n" nil t) (point) (point-max)))
	(set-buffer (get-buffer-create " *nnmail work*"))
	(buffer-disable-undo (current-buffer))
	(erase-buffer)
	;; Copy the headers into the work buffer.
	(insert-buffer-substring obuf beg end)
	;; Fold continuation lines.
	(goto-char (point-min))
	(while (re-search-forward "\\(\r?\n[ \t]+\\)+" nil t)
	  (replace-match " " t t))
	(if (and (symbolp nnmail-split-methods)
		 (fboundp nnmail-split-methods))
	    (setq group-art
		  (mapcar
		   (lambda (group) (cons group (funcall func group)))
		   (condition-case nil
		       (funcall nnmail-split-methods)
		     (error
		      (message "\
Problems with `nnmail-split-methods', using `bogus' mail group")
		      (sit-for 1)
		      '("bogus")))))
	  ;; Go through the split methods to find a match.
	  (while (and methods (or nnmail-crosspost (not group-art)))
	    (goto-char (point-max))
	    (if (or (cdr methods)
		    (not (equal "" (nth 1 (car methods)))))
		(if (and (condition-case () 
			     (if (stringp (nth 1 (car methods)))
				 (re-search-backward
				  (car (cdr (car methods))) nil t)
			       ;; Suggested by Brian Edmonds 
			       ;; <edmonds@cs.ubc.ca>.
			       (funcall (nth 1 (car methods)) 
					(car (car methods))))
			   (error nil))
			 ;; Don't enter the article into the same group twice.
			 (not (assoc (car (car methods)) group-art)))
		    (setq group-art
			  (cons (cons (car (car methods))
				      (funcall func (car (car methods)))) 
				group-art)))
	      (or group-art
		  (setq group-art 
			(list (cons (car (car methods)) 
				    (funcall func (car (car methods))))))))
	    (setq methods (cdr methods))))
	(kill-buffer (current-buffer))
	group-art))))

(defun nnmail-insert-lines ()
  "Insert how many lines and chars there are in the body of the mail."
  (let (lines chars)
    (save-excursion
      (goto-char (point-min))
      (if (search-forward "\n\n" nil t) 
	  (progn
	    (setq chars (- (point-max) (point)))
	    (setq lines (- (count-lines (point) (point-max)) 1))
	    (forward-char -1)
	    (save-excursion
	      (if (re-search-backward "^Lines: " nil t)
		  (delete-region (point) (progn (forward-line 1) (point)))))
	    (insert (format "Lines: %d\n" lines))
	    chars)))))

(defun nnmail-insert-xref (group-alist)
  "Insert an Xref line based on the (group . article) alist."
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "\n\n" nil t) 
	(progn
	  (forward-char -1)
	  (if (re-search-backward "^Xref: " nil t)
	      (delete-region (match-beginning 0) 
			     (progn (forward-line 1) (point))))
	  (insert (format "Xref: %s" (system-name)))
	  (while group-alist
	    (insert (format " %s:%d" (car (car group-alist)) 
			    (cdr (car group-alist))))
	    (setq group-alist (cdr group-alist)))
	  (insert "\n")))))

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
  (nnmail-split-it nnmail-split-fancy))

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
	   done))	((assq split nnmail-split-cache)
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
				"\\>\\)")))
	   (setq nnmail-split-cache 
		 (cons (cons split regexp) nnmail-split-cache))
	   (goto-char (point-max))
	   (if (re-search-backward regexp nil t)
	       (nnmail-split-it (nth 2 split)))))))

;; Get a list of spool files to read.
(defun nnmail-get-spool-files (&optional group)
  (if (null nnmail-spool-file)
      ;; No spool file whatsoever.
      nil)
  (let* ((procmails 
	  ;; If procmail is used to get incoming mail, the files
	  ;; are stored in this directory.
	  (and (file-exists-p nnmail-procmail-directory)
	       (directory-files 
		nnmail-procmail-directory 
		t (concat (if group group "")
			  nnmail-procmail-suffix "$") t)))
	 (p procmails))
    ;; Remove any directories that inadvertently match the procmail
    ;; suffix, which might happen if the suffix is "".
    (while p
      (and (or (file-directory-p (car p))
	       (file-symlink-p (car p)))
	   (setq procmails (delete (car p) procmails)))
      (setq p (cdr p)))
    (cond ((listp nnmail-spool-file)
	   (append nnmail-spool-file procmails))
	  ((stringp nnmail-spool-file)
	   (cons nnmail-spool-file procmails))
	  (t
	   procmails))))

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
  (concat "<" (nnmail-unique-id) "@totally-fudged-out-message-id>"))

(defvar nnmail-unique-id-char nil)

(defun nnmail-number-base36 (num len)
  (if (if (< len 0) (<= num 0) (= len 0))
      ""
    (concat (nnmail-number-base36 (/ num 36) (1- len))
	    (char-to-string (aref "zyxwvutsrqponmlkjihgfedcba9876543210"
				  (% num 36))))))

(defun nnmail-unique-id ()
  (setq nnmail-unique-id-char
	(% (1+ (or nnmail-unique-id-char (logand (random t) (1- (lsh 1 20)))))
	   ;; (current-time) returns 16-bit ints,
	   ;; and 2^16*25 just fits into 4 digits i base 36.
	   (* 25 25)))
  (let ((tm (if (fboundp 'current-time)
		(current-time) '(12191 46742 287898))))
    (concat
     (nnmail-number-base36 (+ (car   tm) 
			      (lsh (% nnmail-unique-id-char 25) 16)) 4)
     (nnmail-number-base36 (+ (nth 1 tm) 
			      (lsh (/ nnmail-unique-id-char 25) 16)) 4))))

;;;
;;; nnmail duplicate handling
;;;

(defvar nnmail-cache-buffer nil)

(defun nnmail-cache-open ()
  (if (or (not nnmail-delete-duplicates)
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
      (current-buffer))))

(defun nnmail-cache-close ()
  (if (or (not nnmail-cache-buffer)
	  (not nnmail-delete-duplicates)
	  (not (buffer-name nnmail-cache-buffer))
	  (not (buffer-modified-p nnmail-cache-buffer)))
      ()				; The buffer is closed.
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
      (set-buffer-modified-p nil))))

(defun nnmail-cache-insert (id)
  (and nnmail-delete-duplicates
       (save-excursion
	 (set-buffer nnmail-cache-buffer)
	 (goto-char (point-max))
	 (insert id "\n"))))

(defun nnmail-cache-id-exists-p (id)
  (and nnmail-delete-duplicates
       (save-excursion
	 (set-buffer nnmail-cache-buffer)
	 (goto-char (point-max))
	 (search-backward id nil t))))


(provide 'nnmail)

;;; nnmail.el ends here
