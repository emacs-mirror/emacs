;;; gnus-registry.el --- article registry for Gnus

;; Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Keywords: news

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;; This is the gnus-registry.el package, which works with all
;; backends, not just nnmail (e.g. NNTP).  The major issue is that it
;; doesn't go across backends, so for instance if an article is in
;; nnml:sys and you see a reference to it in nnimap splitting, the
;; article will end up in nnimap:sys

;; gnus-registry.el intercepts article respooling, moving, deleting,
;; and copying for all backends.  If it doesn't work correctly for
;; you, submit a bug report and I'll be glad to fix it.  It needs
;; documentation in the manual (also on my to-do list).

;; Put this in your startup file (~/.gnus.el for instance)

;; (setq gnus-registry-max-entries 2500
;;       gnus-registry-use-long-group-names t)

;; (gnus-registry-initialize)

;; Then use this in your fancy-split:

;; (: gnus-registry-split-fancy-with-parent)

;; TODO:

;; - get the correct group on spool actions

;; - articles that are spooled to a different backend should be handled

;;; Code:

(eval-when-compile (require 'cl))

(require 'gnus)
(require 'gnus-int)
(require 'gnus-sum)
(require 'gnus-util)
(require 'nnmail)

(defvar gnus-adaptive-word-syntax-table)

(defvar gnus-registry-dirty t
 "Boolean set to t when the registry is modified")

(defgroup gnus-registry nil
  "The Gnus registry."
  :version "22.1"
  :group 'gnus)

(defvar gnus-registry-hashtb (make-hash-table 			    
			      :size 256
			      :test 'equal)
  "*The article registry by Message ID.")

(defcustom gnus-registry-marks
  '((Important
     :char ?i
     :image "summary_important")
    (Work
     :char ?w
     :image "summary_work")
    (Personal
     :char ?p
     :image "summary_personal")
    (To-Do
     :char ?t
     :image "summary_todo")
    (Later
     :char ?l
     :image "summary_later"))

  "List of registry marks and their options.

`gnus-registry-mark-article' will offer symbols from this list
for completion.  

Each entry must have a character to be useful for summary mode
line display and for keyboard shortcuts.

Each entry must have an image string to be useful for visual
display."
  :group 'gnus-registry
  :type '(repeat :tag "Registry Marks"
		 (cons :tag "Mark"
		       (symbol :tag "Name")
		       (checklist :tag "Options" :greedy t
				  (group :inline t
					 (const :format "" :value :char)
					 (character :tag "Character code"))
				  (group :inline t
					 (const :format "" :value :image)
					 (string :tag "Image"))))))

(defcustom gnus-registry-default-mark 'To-Do
  "The default mark.  Should be a valid key for `gnus-registry-marks'."
  :group 'gnus-registry
  :type 'symbol)

(defcustom gnus-registry-unfollowed-groups 
  '("delayed$" "drafts$" "queue$" "INBOX$")
  "List of groups that gnus-registry-split-fancy-with-parent won't return.
The group names are matched, they don't have to be fully
qualified.  This parameter tells the Registry 'never split a
message into a group that matches one of these, regardless of
references.'"
  :group 'gnus-registry
  :type '(repeat regexp))

(defcustom gnus-registry-install 'ask
  "Whether the registry should be installed."
  :group 'gnus-registry
  :type '(choice (const :tag "Never Install" nil)
		 (const :tag "Always Install" t)
		 (const :tag "Ask Me" ask)))

(defcustom gnus-registry-clean-empty t
  "Whether the empty registry entries should be deleted.
Registry entries are considered empty when they have no groups
and no extra data."
  :group 'gnus-registry
  :type 'boolean)

(defcustom gnus-registry-use-long-group-names nil
  "Whether the registry should use long group names (BUGGY)."
  :group 'gnus-registry
  :type 'boolean)

(defcustom gnus-registry-track-extra nil
  "Whether the registry should track extra data about a message.
The Subject and Sender (From:) headers are currently tracked this
way."
  :group 'gnus-registry
  :type
  '(set :tag "Tracking choices"
    (const :tag "Track by subject (Subject: header)" subject)
    (const :tag "Track by sender (From: header)"  sender)))

(defcustom gnus-registry-split-strategy nil
  "Whether the registry should track extra data about a message.
The Subject and Sender (From:) headers are currently tracked this
way."
  :group 'gnus-registry
  :type
  '(choice :tag "Tracking choices"
	   (const :tag "Only use single choices, discard multiple matches" nil)
	   (const :tag "Majority of matches wins" majority)
	   (const :tag "First found wins"  first)))

(defcustom gnus-registry-entry-caching t
  "Whether the registry should cache extra information."
  :group 'gnus-registry
  :type 'boolean)

(defcustom gnus-registry-minimum-subject-length 5
  "The minimum length of a subject before it's considered trackable."
  :group 'gnus-registry
  :type 'integer)

(defcustom gnus-registry-trim-articles-without-groups t
  "Whether the registry should clean out message IDs without groups."
  :group 'gnus-registry
  :type 'boolean)

(defcustom gnus-registry-extra-entries-precious '(marks)
  "What extra entries are precious, meaning they won't get trimmed.
When you save the Gnus registry, it's trimmed to be no longer
than `gnus-registry-max-entries' (which is nil by default, so no
trimming happens).  Any entries with extra data in this list (by
default, marks are included, so articles with marks are
considered precious) will not be trimmed."
  :group 'gnus-registry
  :type '(repeat symbol))

(defcustom gnus-registry-cache-file 
  (nnheader-concat 
   (or gnus-dribble-directory gnus-home-directory "~/") 
   ".gnus.registry.eld")
  "File where the Gnus registry will be stored."
  :group 'gnus-registry
  :type 'file)

(defcustom gnus-registry-max-entries nil
  "Maximum number of entries in the registry, nil for unlimited."
  :group 'gnus-registry
  :type '(radio (const :format "Unlimited " nil)
		(integer :format "Maximum number: %v")))

(defun gnus-registry-track-subject-p ()
  (memq 'subject gnus-registry-track-extra))

(defun gnus-registry-track-sender-p ()
  (memq 'sender gnus-registry-track-extra))

(defun gnus-registry-cache-read ()
  "Read the registry cache file."
  (interactive)
  (let ((file gnus-registry-cache-file))
    (when (file-exists-p file)
      (gnus-message 5 "Reading %s..." file)
      (gnus-load file)
      (gnus-message 5 "Reading %s...done" file))))

;; FIXME: Get rid of duplicated code, cf. `gnus-save-newsrc-file' in
;; `gnus-start.el'.  --rsteib
(defun gnus-registry-cache-save ()
  "Save the registry cache file."
  (interactive)
  (let ((file gnus-registry-cache-file))
    (save-excursion
      (set-buffer (gnus-get-buffer-create " *Gnus-registry-cache*"))
      (make-local-variable 'version-control)
    (setq version-control gnus-backup-startup-file)
    (setq buffer-file-name file)
    (setq default-directory (file-name-directory buffer-file-name))
    (buffer-disable-undo)
    (erase-buffer)
    (gnus-message 5 "Saving %s..." file)
    (if gnus-save-startup-file-via-temp-buffer
	(let ((coding-system-for-write gnus-ding-file-coding-system)
	      (standard-output (current-buffer)))
	  (gnus-gnus-to-quick-newsrc-format 
	   t "gnus registry startup file" 'gnus-registry-alist)
	  (gnus-registry-cache-whitespace file)
	  (save-buffer))
      (let ((coding-system-for-write gnus-ding-file-coding-system)
	    (version-control gnus-backup-startup-file)
	    (startup-file file)
	    (working-dir (file-name-directory file))
	    working-file
	    (i -1))
	;; Generate the name of a non-existent file.
	(while (progn (setq working-file
			    (format
			     (if (and (eq system-type 'ms-dos)
				      (not (gnus-long-file-names)))
				 "%s#%d.tm#" ; MSDOS limits files to 8+3
			       (if (memq system-type '(vax-vms axp-vms))
				   "%s$tmp$%d"
				 "%s#tmp#%d"))
			     working-dir (setq i (1+ i))))
		      (file-exists-p working-file)))

	(unwind-protect
	    (progn
	      (gnus-with-output-to-file working-file
		(gnus-gnus-to-quick-newsrc-format 
		 t "gnus registry startup file" 'gnus-registry-alist))

	      ;; These bindings will mislead the current buffer
	      ;; into thinking that it is visiting the startup
	      ;; file.
	      (let ((buffer-backed-up nil)
		    (buffer-file-name startup-file)
		    (file-precious-flag t)
		    (setmodes (file-modes startup-file)))
		;; Backup the current version of the startup file.
		(backup-buffer)

		;; Replace the existing startup file with the temp file.
		(rename-file working-file startup-file t)
		(gnus-set-file-modes startup-file setmodes)))
	  (condition-case nil
	      (delete-file working-file)
	    (file-error nil)))))

    (gnus-kill-buffer (current-buffer))
    (gnus-message 5 "Saving %s...done" file))))

;; Idea from Dan Christensen <jdc@chow.mat.jhu.edu>
;; Save the gnus-registry file with extra line breaks.
(defun gnus-registry-cache-whitespace (filename)
  (gnus-message 7 "Adding whitespace to %s" filename)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^(\\|(\\\"" nil t)
      (replace-match "\n\\&" t))
    (goto-char (point-min))
    (while (re-search-forward " $" nil t)
      (replace-match "" t t))))

(defun gnus-registry-save (&optional force)
  (when (or gnus-registry-dirty force)
    (let ((caching gnus-registry-entry-caching))
      ;; turn off entry caching, so mtime doesn't get recorded
      (setq gnus-registry-entry-caching nil)
      ;; remove entry caches
      (maphash
       (lambda (key value)
	 (if (hash-table-p value)
	     (remhash key gnus-registry-hashtb)))
       gnus-registry-hashtb)
      ;; remove empty entries
      (when gnus-registry-clean-empty
	(gnus-registry-clean-empty-function))
      ;; now trim and clean text properties from the registry appropriately
      (setq gnus-registry-alist 
	    (gnus-registry-remove-alist-text-properties
	     (gnus-registry-trim
	      (gnus-hashtable-to-alist
	       gnus-registry-hashtb))))
      ;; really save
      (gnus-registry-cache-save)
      (setq gnus-registry-entry-caching caching)
      (setq gnus-registry-dirty nil))))

(defun gnus-registry-clean-empty-function ()
  "Remove all empty entries from the registry.  Returns count thereof."
  (let ((count 0))

    (maphash
     (lambda (key value)
       (when (stringp key)
	 (dolist (group (gnus-registry-fetch-groups key))
	   (when (gnus-parameter-registry-ignore group)
	     (gnus-message
	      10 
	      "gnus-registry: deleted ignored group %s from key %s"
	      group key)
	     (gnus-registry-delete-group key group)))

	 (unless (gnus-registry-group-count key)
	   (gnus-registry-delete-id key))

	 (unless (or
		  (gnus-registry-fetch-group key)
		  ;; TODO: look for specific extra data here!
		  ;; in this example, we look for 'label
		  (gnus-registry-fetch-extra key 'label))
	   (incf count)
	   (gnus-registry-delete-id key))
	 
	 (unless (stringp key)
	   (gnus-message 
	    10 
	    "gnus-registry key %s was not a string, removing" 
	    key)
	   (gnus-registry-delete-id key))))
       
     gnus-registry-hashtb)
    count))

(defun gnus-registry-read ()
  (gnus-registry-cache-read)
  (setq gnus-registry-hashtb (gnus-alist-to-hashtable gnus-registry-alist))
  (setq gnus-registry-dirty nil))

(defun gnus-registry-remove-alist-text-properties (v)
  "Remove text properties from all strings in alist."
  (if (stringp v)
      (gnus-string-remove-all-properties v)
    (if (and (listp v) (listp (cdr v)))
	(mapcar 'gnus-registry-remove-alist-text-properties v)
      (if (and (listp v) (stringp (cdr v)))
	  (cons (gnus-registry-remove-alist-text-properties (car v))
		(gnus-registry-remove-alist-text-properties (cdr v)))
      v))))

(defun gnus-registry-trim (alist)
  "Trim alist to size, using gnus-registry-max-entries.
Any entries with extra data (marks, currently) are left alone."
  (if (null gnus-registry-max-entries)      
      alist                             ; just return the alist
    ;; else, when given max-entries, trim the alist
    (let* ((timehash (make-hash-table
		      :size 20000
		      :test 'equal))
	   (precious (make-hash-table
		      :size 20000
		      :test 'equal))
	   (trim-length (- (length alist) gnus-registry-max-entries))
	   (trim-length (if (natnump trim-length) trim-length 0))
	   precious-list junk-list)
      (maphash
       (lambda (key value)
	 (let ((extra (gnus-registry-fetch-extra key)))
	   (dolist (item gnus-registry-extra-entries-precious)
	     (dolist (e extra)
	       (when (equal (nth 0 e) item)
		 (puthash key t precious)
		 (return))))
	   (puthash key (gnus-registry-fetch-extra key 'mtime) timehash)))
       gnus-registry-hashtb)

      (dolist (item alist)
	(let ((key (nth 0 item)))
	  (if (gethash key precious)
	      (push item precious-list)
	    (push item junk-list))))

      (sort 
       junk-list
       (lambda (a b)
	 (let ((t1 (or (cdr (gethash (car a) timehash)) 
		       '(0 0 0)))
	       (t2 (or (cdr (gethash (car b) timehash)) 
		       '(0 0 0))))
	   (time-less-p t1 t2))))

      ;; we use the return value of this setq, which is the trimmed alist
      (setq alist (append precious-list
			  (nthcdr trim-length junk-list))))))
  
(defun gnus-registry-action (action data-header from &optional to method)
  (let* ((id (mail-header-id data-header))
	 (subject (gnus-string-remove-all-properties
		   (gnus-registry-simplify-subject
		    (mail-header-subject data-header))))
	 (sender (gnus-string-remove-all-properties 
		  (mail-header-from data-header)))
	 (from (gnus-group-guess-full-name-from-command-method from))
	 (to (if to (gnus-group-guess-full-name-from-command-method to) nil))
	 (to-name (if to to "the Bit Bucket"))
	 (old-entry (gethash id gnus-registry-hashtb)))
    (gnus-message 7 "Registry: article %s %s from %s to %s"
		  id
		  (if method "respooling" "going")
		  from
		  to)

    ;; All except copy will need a delete
    (gnus-registry-delete-group id from)

    (when (equal 'copy action)
      (gnus-registry-add-group id from subject sender)) ; undo the delete

    (gnus-registry-add-group id to subject sender)))

(defun gnus-registry-spool-action (id group &optional subject sender)
  (let ((group (gnus-group-guess-full-name-from-command-method group)))
    (when (and (stringp id) (string-match "\r$" id))
      (setq id (substring id 0 -1)))
    (gnus-message 7 "Registry: article %s spooled to %s"
		  id
		  group)
    (gnus-registry-add-group id group subject sender)))

;; Function for nn{mail|imap}-split-fancy: look up all references in
;; the cache and if a match is found, return that group.
(defun gnus-registry-split-fancy-with-parent ()
  "Split this message into the same group as its parent.  The parent
is obtained from the registry.  This function can be used as an entry
in `nnmail-split-fancy' or `nnimap-split-fancy', for example like
this: (: gnus-registry-split-fancy-with-parent)

This function tracks ALL backends, unlike
`nnmail-split-fancy-with-parent' which tracks only nnmail
messages.

For a message to be split, it looks for the parent message in the
References or In-Reply-To header and then looks in the registry
to see which group that message was put in.  This group is
returned, unless `gnus-registry-follow-group-p' return nil for
that group.

See the Info node `(gnus)Fancy Mail Splitting' for more details."
  (let* ((refstr (or (message-fetch-field "references") "")) ; guaranteed
	 (reply-to (message-fetch-field "in-reply-to"))	     ; may be nil
	 ;; now, if reply-to is valid, append it to the References
	 (refstr (if reply-to 
		     (concat refstr " " reply-to)
		   refstr))
	 ;; these may not be used, but the code is cleaner having them up here
	 (sender (gnus-string-remove-all-properties
		  (message-fetch-field "from")))
	 (subject (gnus-string-remove-all-properties
		   (gnus-registry-simplify-subject
		    (message-fetch-field "subject"))))

	 (nnmail-split-fancy-with-parent-ignore-groups
	  (if (listp nnmail-split-fancy-with-parent-ignore-groups)
	      nnmail-split-fancy-with-parent-ignore-groups
	    (list nnmail-split-fancy-with-parent-ignore-groups)))
	 (log-agent "gnus-registry-split-fancy-with-parent")
	 found found-full)

    ;; this is a big if-else statement.  it uses
    ;; gnus-registry-post-process-groups to filter the results after
    ;; every step.
    (cond
     ;; the references string must be valid and parse to valid references
     ((and refstr (gnus-extract-references refstr))
      (dolist (reference (nreverse (gnus-extract-references refstr)))
	(gnus-message
	 9
	 "%s is looking for matches for reference %s from [%s]"
	 log-agent reference refstr)
	(dolist (group (gnus-registry-fetch-groups reference))
	  (when (and group (gnus-registry-follow-group-p group))
	    (gnus-message
	     7
	     "%s traced the reference %s from [%s] to group %s"
	     log-agent reference refstr group)
	    (push group found))))
      ;; filter the found groups and return them
      ;; the found groups are the full groups
      (setq found (gnus-registry-post-process-groups 
		   "references" refstr found found)))
     
     ;; else: there were no matches, now try the extra tracking by sender
     ((and (gnus-registry-track-sender-p) 
	   sender)
      (maphash
       (lambda (key value)
	 (let ((this-sender (cdr
			     (gnus-registry-fetch-extra key 'sender)))
	       matches)
	   (when (and this-sender
		      (equal sender this-sender))
	     (let ((groups (gnus-registry-fetch-groups key)))
	       (dolist (group groups)
		 (push group found-full)
		 (setq found (append (list group) (delete group found)))))
	     (push key matches)
	     (gnus-message
	      ;; raise level of messaging if gnus-registry-track-extra
	      (if gnus-registry-track-extra 7 9)
	      "%s (extra tracking) traced sender %s to groups %s (keys %s)"
	      log-agent sender found matches))))
       gnus-registry-hashtb)
      ;; filter the found groups and return them
      ;; the found groups are NOT the full groups
      (setq found (gnus-registry-post-process-groups 
		   "sender" sender found found-full)))
      
     ;; else: there were no matches, now try the extra tracking by subject
     ((and (gnus-registry-track-subject-p)
	   subject
	   (< gnus-registry-minimum-subject-length (length subject)))
      (maphash
       (lambda (key value)
	 (let ((this-subject (cdr
			      (gnus-registry-fetch-extra key 'subject)))
	       matches)
	   (when (and this-subject
		      (equal subject this-subject))
	     (let ((groups (gnus-registry-fetch-groups key)))
	       (dolist (group groups)
		 (push group found-full)
		 (setq found (append (list group) (delete group found)))))
	     (push key matches)
	     (gnus-message
	      ;; raise level of messaging if gnus-registry-track-extra
	      (if gnus-registry-track-extra 7 9)
	      "%s (extra tracking) traced subject %s to groups %s (keys %s)"
	      log-agent subject found matches))))
       gnus-registry-hashtb)
      ;; filter the found groups and return them
      ;; the found groups are NOT the full groups
      (setq found (gnus-registry-post-process-groups 
		   "subject" subject found found-full))))
    ;; after the (cond) we extract the actual value safely
    (car-safe found)))

(defun gnus-registry-post-process-groups (mode key groups groups-full)
  "Modifies GROUPS found by MODE for KEY to determine which ones to follow.

MODE can be 'subject' or 'sender' for example.  The KEY is the
value by which MODE was searched.

Transforms each group name to the equivalent short name.

Checks if the current Gnus method (from `gnus-command-method' or
from `gnus-newsgroup-name') is the same as the group's method.
This is not possible if gnus-registry-use-long-group-names is
false.  Foreign methods are not supported so they are rejected.

Reduces the list to a single group, or complains if that's not
possible.  Uses `gnus-registry-split-strategy' and GROUPS-FULL if
necessary."
  (let ((log-agent "gnus-registry-post-process-group")
	out)

    ;; the strategy can be 'first, 'majority, or nil
    (when (eq gnus-registry-split-strategy 'first)
      (when groups
	(setq groups (list (car-safe groups)))))

    (when (eq gnus-registry-split-strategy 'majority)
      (let ((freq (make-hash-table
		   :size 256
		   :test 'equal)))
	(mapc (lambda(x) (puthash x (1+ (gethash x freq 0)) freq)) groups-full)
	(setq groups (list (car-safe
			    (sort
			     groups
			     (lambda (a b)
			       (> (gethash a freq 0)
				  (gethash b freq 0)))))))))
    
    (if gnus-registry-use-long-group-names
	(dolist (group groups)
	  (let ((m1 (gnus-find-method-for-group group))
		(m2 (or gnus-command-method
			(gnus-find-method-for-group gnus-newsgroup-name)))
		(short-name (gnus-group-short-name group)))
	    (if (gnus-methods-equal-p m1 m2)
		(progn
		  ;; this is REALLY just for debugging
		  (gnus-message
		   10
		   "%s stripped group %s to %s"
		   log-agent group short-name)
		  (unless (member short-name out)
		    (push short-name out)))
	      ;; else...
	      (gnus-message
	       7
	       "%s ignored foreign group %s"
	       log-agent group))))
      (setq out groups))
    (when (cdr-safe out)
	(gnus-message
	 5
	 "%s: too many extra matches (%s) for %s %s.  Returning none."
	 log-agent out mode key)
	(setq out nil))
    out))

(defun gnus-registry-follow-group-p (group)
  "Determines if a group name should be followed.
Consults `gnus-registry-unfollowed-groups' and
`nnmail-split-fancy-with-parent-ignore-groups'."
  (not (or (gnus-registry-grep-in-list
	    group
	    gnus-registry-unfollowed-groups)
	   (gnus-registry-grep-in-list
	    group
	    nnmail-split-fancy-with-parent-ignore-groups))))

(defun gnus-registry-wash-for-keywords (&optional force)
  (interactive)
  (let ((id (gnus-registry-fetch-message-id-fast gnus-current-article))
	word words)
    (if (or (not (gnus-registry-fetch-extra id 'keywords))
	    force)
	(save-excursion
	  (set-buffer gnus-article-buffer)
	  (article-goto-body)
	  (save-window-excursion
	    (save-restriction
	      (narrow-to-region (point) (point-max))
	      (with-syntax-table gnus-adaptive-word-syntax-table
		(while (re-search-forward "\\b\\w+\\b" nil t)
		  (setq word (gnus-registry-remove-alist-text-properties
			      (downcase (buffer-substring
					 (match-beginning 0) (match-end 0)))))
		  (if (> (length word) 3)
		      (push word words))))))
	  (gnus-registry-store-extra-entry id 'keywords words)))))

(defun gnus-registry-find-keywords (keyword)
  (interactive "skeyword: ")
  (let (articles)
    (maphash
     (lambda (key value)
       (when (gnus-registry-grep-in-list
	      keyword
	      (cdr (gnus-registry-fetch-extra key 'keywords)))
	 (push key articles)))
     gnus-registry-hashtb)
    articles))

(defun gnus-registry-register-message-ids ()
  "Register the Message-ID of every article in the group"
  (unless (gnus-parameter-registry-ignore gnus-newsgroup-name)
    (dolist (article gnus-newsgroup-articles)
      (let ((id (gnus-registry-fetch-message-id-fast article)))
	(unless (member gnus-newsgroup-name (gnus-registry-fetch-groups id))
	  (gnus-message 9 "Registry: Registering article %d with group %s"
			article gnus-newsgroup-name)
	  (gnus-registry-add-group 
	   id 
	   gnus-newsgroup-name
	   (gnus-registry-fetch-simplified-message-subject-fast article)
	   (gnus-registry-fetch-sender-fast article)))))))

(defun gnus-registry-fetch-message-id-fast (article)
  "Fetch the Message-ID quickly, using the internal gnus-data-list function"
  (if (and (numberp article)
	   (assoc article (gnus-data-list nil)))
      (mail-header-id (gnus-data-header (assoc article (gnus-data-list nil))))
    nil))

(defun gnus-registry-simplify-subject (subject)
  (if (stringp subject)
      (gnus-simplify-subject subject)
    nil))

(defun gnus-registry-fetch-simplified-message-subject-fast (article)
  "Fetch the Subject quickly, using the internal gnus-data-list function"
  (if (and (numberp article)
	   (assoc article (gnus-data-list nil)))
      (gnus-string-remove-all-properties
       (gnus-registry-simplify-subject
	(mail-header-subject (gnus-data-header
			      (assoc article (gnus-data-list nil))))))
    nil))

(defun gnus-registry-fetch-sender-fast (article)
  "Fetch the Sender quickly, using the internal gnus-data-list function"
  (if (and (numberp article)
	   (assoc article (gnus-data-list nil)))
      (gnus-string-remove-all-properties
       (mail-header-from (gnus-data-header
			  (assoc article (gnus-data-list nil)))))
    nil))

;;; this should be redone with catch/throw
(defun gnus-registry-grep-in-list (word list)
  (when word
    (memq nil
	  (mapcar 'not
		  (mapcar
		   (lambda (x)
		     (string-match word x))
		   list)))))

(defun gnus-registry-do-marks (type function)
  "For each known mark, call FUNCTION for each cell of type TYPE.

FUNCTION should take two parameters, a mark symbol and the cell value."
  (dolist (mark-info gnus-registry-marks)
    (let* ((mark (car-safe mark-info))
	   (data (cdr-safe mark-info))
	   (cell-data (plist-get data type)))
      (when cell-data
	(funcall function mark cell-data)))))

;;; this is ugly code, but I don't know how to do it better
(defun gnus-registry-install-shortcuts ()
  "Install the keyboard shortcuts and menus for the registry.
Uses `gnus-registry-marks' to find what shortcuts to install."
  (let (keys-plist)
    (gnus-registry-do-marks 
     :char
     (lambda (mark data)
       (let ((function-format
	      (format "gnus-registry-%%s-article-%s-mark" mark)))

;;; The following generates these functions:
;;; (defun gnus-registry-set-article-Important-mark (&rest articles)
;;;   "Apply the Important mark to process-marked ARTICLES."
;;;   (interactive (gnus-summary-work-articles current-prefix-arg))
;;;   (gnus-registry-set-article-mark-internal 'Important articles nil t))
;;; (defun gnus-registry-remove-article-Important-mark (&rest articles)
;;;   "Apply the Important mark to process-marked ARTICLES."
;;;   (interactive (gnus-summary-work-articles current-prefix-arg))
;;;   (gnus-registry-set-article-mark-internal 'Important articles t t))

	 (dolist (remove '(t nil))
	   (let* ((variant-name (if remove "remove" "set"))
		  (function-name (format function-format variant-name))
		  (shortcut (format "%c" data))
		  (shortcut (if remove (upcase shortcut) shortcut)))
	     (unintern function-name)
	     (eval
	      `(defun 
		 ;; function name
		 ,(intern function-name) 
		 ;; parameter definition
		 (&rest articles)
		 ;; documentation
		 ,(format 
		   "%s the %s mark over process-marked ARTICLES."
		   (upcase-initials variant-name)
		   mark)
		 ;; interactive definition
		 (interactive 
		  (gnus-summary-work-articles current-prefix-arg))
		 ;; actual code

		 ;; if this is called and the user doesn't want the
		 ;; registry enabled, we'll ask anyhow
		 (when (eq gnus-registry-install nil)
		   (setq gnus-registry-install 'ask))

		 ;; now the user is asked if gnus-registry-install is 'ask
		 (when (gnus-registry-install-p)
		   (gnus-registry-set-article-mark-internal 
		    ;; all this just to get the mark, I must be doing it wrong
		    (intern ,(symbol-name mark))
		    articles ,remove t)
		   (dolist (article articles)
		     (gnus-summary-update-article 
		      article 
		      (assoc article (gnus-data-list nil)))))))
	     (push (intern function-name) keys-plist)
	     (push shortcut keys-plist)
	     (gnus-message 
	      9 
	      "Defined mark handling function %s" 
	      function-name))))))
    (gnus-define-keys-1
     '(gnus-registry-mark-map "M" gnus-summary-mark-map) 
     keys-plist)))

;;; use like this:
;;; (defalias 'gnus-user-format-function-M 
;;;           'gnus-registry-user-format-function-M)
(defun gnus-registry-user-format-function-M (headers)
  (let* ((id (mail-header-message-id headers))
	 (marks (when id (gnus-registry-fetch-extra-marks id))))
    (apply 'concat (mapcar (lambda(mark)
			     (let ((c 
				    (plist-get
				     (cdr-safe 
				      (assoc mark gnus-registry-marks))
				     :char)))
			       (if c
				   (list c)
				 nil)))
			   marks))))

(defun gnus-registry-read-mark ()
  "Read a mark name from the user with completion."
  (let ((mark (gnus-completing-read-with-default 
	       (symbol-name gnus-registry-default-mark)
	       "Label" 
	       (mapcar (lambda (x)	; completion list
			 (cons (symbol-name (car-safe x)) (car-safe x)))
		       gnus-registry-marks))))
    (when (stringp mark)
      (intern mark))))

(defun gnus-registry-set-article-mark (&rest articles)
  "Apply a mark to process-marked ARTICLES."
  (interactive (gnus-summary-work-articles current-prefix-arg))
  (gnus-registry-set-article-mark-internal (gnus-registry-read-mark) articles nil t))

(defun gnus-registry-remove-article-mark (&rest articles)
  "Remove a mark from process-marked ARTICLES."
  (interactive (gnus-summary-work-articles current-prefix-arg))
  (gnus-registry-set-article-mark-internal (gnus-registry-read-mark) articles t t))

(defun gnus-registry-set-article-mark-internal (mark articles &optional remove show-message)
  "Apply a mark to a list of ARTICLES."
  (let ((article-id-list
	 (mapcar 'gnus-registry-fetch-message-id-fast articles)))
    (dolist (id article-id-list)
      (let* (
	     ;; all the marks for this article without the mark of
	     ;; interest
	     (marks
	      (delq mark (gnus-registry-fetch-extra-marks id)))
	     ;; the new marks we want to use
	     (new-marks (if remove
			    marks
			  (cons mark marks))))
	(when show-message
	  (gnus-message 1 "%s mark %s with message ID %s, resulting in %S"
			(if remove "Removing" "Adding")
			mark id new-marks))
	
	(apply 'gnus-registry-store-extra-marks ; set the extra marks
	       id				; for the message ID
	       new-marks)))))

(defun gnus-registry-get-article-marks (&rest articles)
  "Get the Gnus registry marks for ARTICLES and show them if interactive.
Uses process/prefix conventions.  For multiple articles,
only the last one's marks are returned."
  (interactive (gnus-summary-work-articles 1))
  (let (marks)
    (dolist (article articles)
      (let ((article-id
	     (gnus-registry-fetch-message-id-fast article)))
	(setq marks (gnus-registry-fetch-extra-marks article-id))))
    (when (interactive-p)
	(gnus-message 1 "Marks are %S" marks))
    marks))

;;; if this extends to more than 'marks, it should be improved to be more generic.
(defun gnus-registry-fetch-extra-marks (id)
  "Get the marks of a message, based on the message ID.
Returns a list of symbol marks or nil."
  (car-safe (cdr (gnus-registry-fetch-extra id 'marks))))

(defun gnus-registry-has-extra-mark (id mark)
  "Checks if a message has `mark', based on the message ID `id'."
  (memq mark (gnus-registry-fetch-extra-marks id)))

(defun gnus-registry-store-extra-marks (id &rest mark-list)
  "Set the marks of a message, based on the message ID.
The `mark-list' can be nil, in which case no marks are left."
  (gnus-registry-store-extra-entry id 'marks (list mark-list)))

(defun gnus-registry-delete-extra-marks (id &rest mark-delete-list)
  "Delete the message marks in `mark-delete-list', based on the message ID."
  (let ((marks (gnus-registry-fetch-extra-marks id)))
    (when marks
      (dolist (mark mark-delete-list)
	(setq marks (delq mark marks))))
    (gnus-registry-store-extra-marks id (car marks))))

(defun gnus-registry-delete-all-extra-marks (id)
  "Delete all the marks for a message ID."
  (gnus-registry-store-extra-marks id nil))

(defun gnus-registry-fetch-extra (id &optional entry)
  "Get the extra data of a message, based on the message ID.
Returns the first place where the trail finds a nonstring."
  (let ((entry-cache (gethash entry gnus-registry-hashtb)))
    (if (and entry
	     (hash-table-p entry-cache)
	     (gethash id entry-cache))
	(gethash id entry-cache)
      ;; else, if there is no caching possible...
      (let ((trail (gethash id gnus-registry-hashtb)))
	(when (listp trail)
	  (dolist (crumb trail)
	    (unless (stringp crumb)
	      (return (gnus-registry-fetch-extra-entry crumb entry id)))))))))

(defun gnus-registry-fetch-extra-entry (alist &optional entry id)
  "Get the extra data of a message, or a specific entry in it.
Update the entry cache if needed."
  (if (and entry id)
      (let ((entry-cache (gethash entry gnus-registry-hashtb))
	    entree)
	(when gnus-registry-entry-caching
	  ;; create the hash table
	  (unless (hash-table-p entry-cache)
	    (setq entry-cache (make-hash-table
			       :size 4096
			       :test 'equal))
	    (puthash entry entry-cache gnus-registry-hashtb))

	  ;; get the entree from the hash table or from the alist
	  (setq entree (gethash id entry-cache)))

	(unless entree
	  (setq entree (assq entry alist))
	  (when gnus-registry-entry-caching
	    (puthash id entree entry-cache)))
	entree)
    alist))

(defun gnus-registry-store-extra (id extra)
  "Store the extra data of a message, based on the message ID.
The message must have at least one group name."
  (when (gnus-registry-group-count id)
    ;; we now know the trail has at least 1 group name, so it's not empty
    (let ((trail (gethash id gnus-registry-hashtb))
	  (old-extra (gnus-registry-fetch-extra id))
	  entry-cache)
      (dolist (crumb trail)
	(unless (stringp crumb)
	  (dolist (entry crumb)
	    (setq entry-cache (gethash (car entry) gnus-registry-hashtb))
	  (when entry-cache
	    (remhash id entry-cache))))
      (puthash id (cons extra (delete old-extra trail))
	       gnus-registry-hashtb)
      (setq gnus-registry-dirty t)))))

(defun gnus-registry-delete-extra-entry (id key)
  "Delete a specific entry in the extras field of the registry entry for id."
  (gnus-registry-store-extra-entry id key nil))

(defun gnus-registry-store-extra-entry (id key value)
  "Put a specific entry in the extras field of the registry entry for id."
  (let* ((extra (gnus-registry-fetch-extra id))
	 ;; all the entries except the one for `key'
	 (the-rest (gnus-assq-delete-all key (gnus-registry-fetch-extra id))) 
	 (alist (if value
		    (gnus-registry-remove-alist-text-properties
		     (cons (cons key value)
			   the-rest))
		  the-rest)))
    (gnus-registry-store-extra id alist)))

(defun gnus-registry-fetch-group (id)
  "Get the group of a message, based on the message ID.
Returns the first place where the trail finds a group name."
  (when (gnus-registry-group-count id)
    ;; we now know the trail has at least 1 group name
    (let ((trail (gethash id gnus-registry-hashtb)))
      (dolist (crumb trail)
	(when (stringp crumb)
	  (return (if gnus-registry-use-long-group-names
		       crumb
		     (gnus-group-short-name crumb))))))))

(defun gnus-registry-fetch-groups (id)
  "Get the groups of a message, based on the message ID."
  (let ((trail (gethash id gnus-registry-hashtb))
	groups)
    (dolist (crumb trail)
      (when (stringp crumb)
	;; push the group name into the list
	(setq 
	 groups
	 (cons
	  (if (or (not (stringp crumb)) gnus-registry-use-long-group-names)
	      crumb
	    (gnus-group-short-name crumb))
	 groups))))
    ;; return the list of groups
    groups))

(defun gnus-registry-group-count (id)
  "Get the number of groups of a message, based on the message ID."
  (let ((trail (gethash id gnus-registry-hashtb)))
    (if (and trail (listp trail))
	(apply '+ (mapcar (lambda (x) (if (stringp x) 1 0)) trail))
      0)))

(defun gnus-registry-delete-group (id group)
  "Delete a group for a message, based on the message ID."
  (when (and group id)
      (let ((trail (gethash id gnus-registry-hashtb))
	    (short-group (gnus-group-short-name group)))
	(puthash id (if trail
			(delete short-group (delete group trail))
		      nil)
		 gnus-registry-hashtb))
      ;; now, clear the entry if there are no more groups
      (when gnus-registry-trim-articles-without-groups
	(unless (gnus-registry-group-count id)
	  (gnus-registry-delete-id id)))
      ;; is this ID still in the registry?
      (when (gethash id gnus-registry-hashtb)
	(gnus-registry-store-extra-entry id 'mtime (current-time)))))

(defun gnus-registry-delete-id (id)
  "Delete a message ID from the registry."
  (when (stringp id)
    (remhash id gnus-registry-hashtb)
    (maphash
     (lambda (key value)
       (when (hash-table-p value)
	 (remhash id value)))
     gnus-registry-hashtb)))

(defun gnus-registry-add-group (id group &optional subject sender)
  "Add a group for a message, based on the message ID."
  (when group
    (when (and id
	       (not (string-match "totally-fudged-out-message-id" id)))
      (let ((full-group group)
	    (group (if gnus-registry-use-long-group-names
		       group
		     (gnus-group-short-name group))))
	(gnus-registry-delete-group id group)

	(unless gnus-registry-use-long-group-names ;; unnecessary in this case
	  (gnus-registry-delete-group id full-group))

	(let ((trail (gethash id gnus-registry-hashtb)))
	  (puthash id (if trail
			  (cons group trail)
			(list group))
		   gnus-registry-hashtb)

	  (when (and (gnus-registry-track-subject-p)
		     subject)
	    (gnus-registry-store-extra-entry
	     id
	     'subject
	     (gnus-registry-simplify-subject subject)))
	  (when (and (gnus-registry-track-sender-p)
		     sender)
	    (gnus-registry-store-extra-entry
	     id
	     'sender
	     sender))

	  (gnus-registry-store-extra-entry id 'mtime (current-time)))))))

(defun gnus-registry-clear ()
  "Clear the Gnus registry."
  (interactive)
  (setq gnus-registry-alist nil)
  (setq gnus-registry-hashtb (gnus-alist-to-hashtable gnus-registry-alist))
  (setq gnus-registry-dirty t))

;;;###autoload
(defun gnus-registry-initialize ()
"Initialize the Gnus registry."
  (interactive)
  (gnus-message 5 "Initializing the registry")
  (setq gnus-registry-install t)	; in case it was 'ask or nil
  (gnus-registry-install-hooks)
  (gnus-registry-install-shortcuts)
  (gnus-registry-read))

;;;###autoload
(defun gnus-registry-install-hooks ()
  "Install the registry hooks."
  (interactive)
  (add-hook 'gnus-summary-article-move-hook 'gnus-registry-action)
  (add-hook 'gnus-summary-article-delete-hook 'gnus-registry-action)
  (add-hook 'gnus-summary-article-expire-hook 'gnus-registry-action)
  (add-hook 'nnmail-spool-hook 'gnus-registry-spool-action)

  (add-hook 'gnus-save-newsrc-hook 'gnus-registry-save)
  (add-hook 'gnus-read-newsrc-el-hook 'gnus-registry-read)

  (add-hook 'gnus-summary-prepare-hook 'gnus-registry-register-message-ids))

(defun gnus-registry-unload-hook ()
  "Uninstall the registry hooks."
  (interactive)
  (remove-hook 'gnus-summary-article-move-hook 'gnus-registry-action)
  (remove-hook 'gnus-summary-article-delete-hook 'gnus-registry-action)
  (remove-hook 'gnus-summary-article-expire-hook 'gnus-registry-action)
  (remove-hook 'nnmail-spool-hook 'gnus-registry-spool-action)

  (remove-hook 'gnus-save-newsrc-hook 'gnus-registry-save)
  (remove-hook 'gnus-read-newsrc-el-hook 'gnus-registry-read)

  (remove-hook 'gnus-summary-prepare-hook 'gnus-registry-register-message-ids))

(add-hook 'gnus-registry-unload-hook 'gnus-registry-unload-hook)

(defun gnus-registry-install-p ()
  (interactive)
  (when (eq gnus-registry-install 'ask)
    (setq gnus-registry-install
	  (gnus-y-or-n-p
	   (concat "Enable the Gnus registry?  "
		   "See the variable `gnus-registry-install' "
		   "to get rid of this query permanently. ")))
    (when gnus-registry-install
      ;; we just set gnus-registry-install to t, so initialize the registry!
      (gnus-registry-initialize)))
;;; we could call it here: (customize-variable 'gnus-registry-install)
  gnus-registry-install)

(when (or (eq gnus-registry-install t)
	  (gnus-registry-install-p))
  (gnus-registry-initialize))

;; TODO: a few things

(provide 'gnus-registry)

;; arch-tag: 5cba0a32-718a-4a97-8c91-0a15af21da94
;;; gnus-registry.el ends here
