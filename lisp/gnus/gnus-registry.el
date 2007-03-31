;;; gnus-registry.el --- article registry for Gnus

;; Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This is the gnus-registry.el package, works with other backends
;; besides nnmail.  The major issue is that it doesn't go across
;; backends, so for instance if an article is in nnml:sys and you see
;; a reference to it in nnimap splitting, the article will end up in
;; nnimap:sys

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

(defvar gnus-registry-dirty t
 "Boolean set to t when the registry is modified")

(defgroup gnus-registry nil
  "The Gnus registry."
  :version "22.1"
  :group 'gnus)

(defvar gnus-registry-hashtb nil
  "*The article registry by Message ID.")

(defcustom gnus-registry-unfollowed-groups '("delayed" "drafts" "queue")
  "List of groups that gnus-registry-split-fancy-with-parent won't follow.
The group names are matched, they don't have to be fully qualified."
  :group 'gnus-registry
  :type '(repeat string))

(defcustom gnus-registry-install nil
  "Whether the registry should be installed."
  :group 'gnus-registry
  :type 'boolean)

(defcustom gnus-registry-clean-empty t
  "Whether the empty registry entries should be deleted.
Registry entries are considered empty when they have no groups."
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

(defcustom gnus-registry-cache-file "~/.gnus.registry.eld"
  "File where the Gnus registry will be stored."
  :group 'gnus-registry
  :type 'file)

(defcustom gnus-registry-max-entries nil
  "Maximum number of entries in the registry, nil for unlimited."
  :group 'gnus-registry
  :type '(radio (const :format "Unlimited " nil)
		(integer :format "Maximum number: %v")))

;; Function(s) missing in Emacs 20
(when (memq nil (mapcar 'fboundp '(puthash)))
  (require 'cl)
  (unless (fboundp 'puthash)
    ;; alias puthash is missing from Emacs 20 cl-extra.el
    (defalias 'puthash 'cl-puthash)))

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
	  (gnus-gnus-to-quick-newsrc-format t "gnus registry startup file" 'gnus-registry-alist)
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
		(gnus-gnus-to-quick-newsrc-format t "gnus registry startup file" 'gnus-registry-alist))

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
		(set-file-modes startup-file setmodes)))
	  (condition-case nil
	      (delete-file working-file)
	    (file-error nil)))))

    (gnus-kill-buffer (current-buffer))
    (gnus-message 5 "Saving %s...done" file))))

;; Idea from Dan Christensen <jdc@chow.mat.jhu.edu>
;; Save the gnus-registry file with extra line breaks.
(defun gnus-registry-cache-whitespace (filename)
  (gnus-message 5 "Adding whitespace to %s" filename)
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
      ;; now trim the registry appropriately
      (setq gnus-registry-alist (gnus-registry-trim
				 (gnus-hashtable-to-alist
				  gnus-registry-hashtb)))
      ;; really save
      (gnus-registry-cache-save)
      (setq gnus-registry-entry-caching caching)
      (setq gnus-registry-dirty nil))))

(defun gnus-registry-clean-empty-function ()
  "Remove all empty entries from the registry.  Returns count thereof."
  (let ((count 0))
    (maphash
     (lambda (key value)
       (unless (gnus-registry-fetch-group key)
	 (incf count)
	 (remhash key gnus-registry-hashtb)))
     gnus-registry-hashtb)
    count))

(defun gnus-registry-read ()
  (gnus-registry-cache-read)
  (setq gnus-registry-hashtb (gnus-alist-to-hashtable gnus-registry-alist))
  (setq gnus-registry-dirty nil))

(defun gnus-registry-trim (alist)
  "Trim alist to size, using gnus-registry-max-entries."
  (if (null gnus-registry-max-entries)
      alist                             ; just return the alist
    ;; else, when given max-entries, trim the alist
    (let* ((timehash (make-hash-table
		      :size 4096
		      :test 'equal))
	   (trim-length (- (length alist) gnus-registry-max-entries))
	   (trim-length (if (natnump trim-length) trim-length 0)))
      (maphash
       (lambda (key value)
         (puthash key (gnus-registry-fetch-extra key 'mtime) timehash))
       gnus-registry-hashtb)

      ;; we use the return value of this setq, which is the trimmed alist
      (setq alist
            (nthcdr
             trim-length
             (sort alist
                   (lambda (a b)
                     (time-less-p
                      (cdr (gethash (car a) timehash))
                      (cdr (gethash (car b) timehash))))))))))

(defun gnus-registry-action (action data-header from &optional to method)
  (let* ((id (mail-header-id data-header))
	 (subject (gnus-registry-simplify-subject
		   (mail-header-subject data-header)))
	 (sender (mail-header-from data-header))
	 (from (gnus-group-guess-full-name-from-command-method from))
	 (to (if to (gnus-group-guess-full-name-from-command-method to) nil))
	 (to-name (if to to "the Bit Bucket"))
	 (old-entry (gethash id gnus-registry-hashtb)))
    (gnus-message 5 "Registry: article %s %s from %s to %s"
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
    (gnus-message 5 "Registry: article %s spooled to %s"
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

For a message to be split, it looks for the parent message in the
References or In-Reply-To header and then looks in the registry to
see which group that message was put in.  This group is returned.

See the Info node `(gnus)Fancy Mail Splitting' for more details."
  (let ((refstr (or (message-fetch-field "references")
		    (message-fetch-field "in-reply-to")))
	(nnmail-split-fancy-with-parent-ignore-groups
	 (if (listp nnmail-split-fancy-with-parent-ignore-groups)
	     nnmail-split-fancy-with-parent-ignore-groups
	   (list nnmail-split-fancy-with-parent-ignore-groups)))
	references res)
    (if refstr
	(progn
	  (setq references (nreverse (gnus-split-references refstr)))
	  (mapcar (lambda (x)
		    (setq res (or (gnus-registry-fetch-group x) res))
		    (when (or (gnus-registry-grep-in-list
			       res
			       gnus-registry-unfollowed-groups)
			      (gnus-registry-grep-in-list
			       res
			       nnmail-split-fancy-with-parent-ignore-groups))
		      (setq res nil)))
		  references))

      ;; else: there were no references, now try the extra tracking
      (let ((sender (message-fetch-field "from"))
	    (subject (gnus-registry-simplify-subject
		      (message-fetch-field "subject")))
	    (single-match t))
	(when (and single-match
		   (gnus-registry-track-sender-p)
		   sender)
	  (maphash
	   (lambda (key value)
	     (let ((this-sender (cdr
				 (gnus-registry-fetch-extra key 'sender))))
	       (when (and single-match
			  this-sender
			  (equal sender this-sender))
		 ;; too many matches, bail
		 (unless (equal res (gnus-registry-fetch-group key))
		   (setq single-match nil))
		 (setq res (gnus-registry-fetch-group key))
		 (gnus-message
		  ;; raise level of messaging if gnus-registry-track-extra
		  (if gnus-registry-track-extra 5 9)
		  "%s (extra tracking) traced sender %s to group %s"
		  "gnus-registry-split-fancy-with-parent"
		  sender
		  (if res res "nil")))))
	   gnus-registry-hashtb))
	(when (and single-match
		   (gnus-registry-track-subject-p)
		   subject
		   (< gnus-registry-minimum-subject-length (length subject)))
	  (maphash
	   (lambda (key value)
	     (let ((this-subject (cdr
				  (gnus-registry-fetch-extra key 'subject))))
	       (when (and single-match
			  this-subject
			  (equal subject this-subject))
		 ;; too many matches, bail
		 (unless (equal res (gnus-registry-fetch-group key))
		   (setq single-match nil))
		 (setq res (gnus-registry-fetch-group key))
		 (gnus-message
		  ;; raise level of messaging if gnus-registry-track-extra
		  (if gnus-registry-track-extra 5 9)
		  "%s (extra tracking) traced subject %s to group %s"
		  "gnus-registry-split-fancy-with-parent"
		  subject
		  (if res res "nil")))))
	   gnus-registry-hashtb))
	(unless single-match
	  (gnus-message
	   5
	   "gnus-registry-split-fancy-with-parent: too many extra matches for %s"
	   refstr)
	  (setq res nil))))
    (gnus-message
     5
     "gnus-registry-split-fancy-with-parent traced %s to group %s"
     refstr (if res res "nil"))

    (when (and res gnus-registry-use-long-group-names)
      (let ((m1 (gnus-find-method-for-group res))
	    (m2 (or gnus-command-method
		    (gnus-find-method-for-group gnus-newsgroup-name)))
	    (short-res (gnus-group-short-name res)))
      (if (gnus-methods-equal-p m1 m2)
	  (progn
	    (gnus-message
	     9
	     "gnus-registry-split-fancy-with-parent stripped group %s to %s"
	     res
	     short-res)
	    (setq res short-res))
	;; else...
	(gnus-message
	 5
	 "gnus-registry-split-fancy-with-parent ignored foreign group %s"
	 res)
	(setq res nil))))
    res))

(defun gnus-registry-register-message-ids ()
  "Register the Message-ID of every article in the group"
  (unless (gnus-parameter-registry-ignore gnus-newsgroup-name)
    (dolist (article gnus-newsgroup-articles)
      (let ((id (gnus-registry-fetch-message-id-fast article)))
	(unless (gnus-registry-fetch-group id)
	  (gnus-message 9 "Registry: Registering article %d with group %s"
			article gnus-newsgroup-name)
	  (gnus-registry-add-group
	   (gnus-registry-fetch-message-id-fast article)
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
      (gnus-registry-simplify-subject
       (mail-header-subject (gnus-data-header
			     (assoc article (gnus-data-list nil)))))
    nil))

(defun gnus-registry-fetch-sender-fast (article)
  "Fetch the Sender quickly, using the internal gnus-data-list function"
  (if (and (numberp article)
	   (assoc article (gnus-data-list nil)))
      (mail-header-from (gnus-data-header
			 (assoc article (gnus-data-list nil))))
    nil))

(defun gnus-registry-grep-in-list (word list)
  (when word
    (memq nil
	  (mapcar 'not
		  (mapcar
		   (lambda (x)
		     (string-match x word))
		   list)))))

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

(defun gnus-registry-store-extra-entry (id key value)
  "Put a specific entry in the extras field of the registry entry for id."
  (let* ((extra (gnus-registry-fetch-extra id))
	 (alist (cons (cons key value)
		 (gnus-assq-delete-all key (gnus-registry-fetch-extra id)))))
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

(defun gnus-registry-group-count (id)
  "Get the number of groups of a message, based on the message ID."
  (let ((trail (gethash id gnus-registry-hashtb)))
    (if (and trail (listp trail))
	(apply '+ (mapcar (lambda (x) (if (stringp x) 1 0)) trail))
      0)))

(defun gnus-registry-delete-group (id group)
  "Delete a group for a message, based on the message ID."
  (when group
    (when id
      (let ((trail (gethash id gnus-registry-hashtb))
	    (group (gnus-group-short-name group)))
	(puthash id (if trail
			(delete group trail)
		      nil)
		 gnus-registry-hashtb))
      ;; now, clear the entry if there are no more groups
      (when gnus-registry-trim-articles-without-groups
	(unless (gnus-registry-group-count id)
	  (gnus-registry-delete-id id)))
      ;; is this ID still in the registry?
      (when (gethash id gnus-registry-hashtb)
	(gnus-registry-store-extra-entry id 'mtime (current-time))))))

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
  (interactive)
  (setq gnus-registry-install t)
  (gnus-registry-install-hooks)
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

(when gnus-registry-install
  (gnus-registry-install-hooks)
  (gnus-registry-read))

;; TODO: a lot of things

(provide 'gnus-registry)

;;; arch-tag: 5cba0a32-718a-4a97-8c91-0a15af21da94
;;; gnus-registry.el ends here
