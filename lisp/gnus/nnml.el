;;; nnml.el --- mail spool access for Gnus
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000
;;        Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; 	Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
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

;; Based on nnspool.el by Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>.
;; For an overview of what the interface functions do, please see the
;; Gnus sources.

;;; Code:

(require 'nnheader)
(require 'nnmail)
(require 'nnoo)
(eval-when-compile (require 'cl))
(eval-and-compile
  (autoload 'gnus-sorted-intersection "gnus-range"))

(nnoo-declare nnml)

(defvoo nnml-directory message-directory
  "Spool directory for the nnml mail backend.")

(defvoo nnml-active-file
    (expand-file-name "active" nnml-directory)
  "Mail active file.")

(defvoo nnml-newsgroups-file
    (expand-file-name "newsgroups" nnml-directory)
  "Mail newsgroups description file.")

(defvoo nnml-get-new-mail t
  "If non-nil, nnml will check the incoming mail file and split the mail.")

(defvoo nnml-nov-is-evil nil
  "If non-nil, Gnus will never generate and use nov databases for mail groups.
Using nov databases will speed up header fetching considerably.
This variable shouldn't be flipped much.  If you have, for some reason,
set this to t, and want to set it to nil again, you should always run
the `nnml-generate-nov-databases' command.  The function will go
through all nnml directories and generate nov databases for them
all.  This may very well take some time.")

(defvoo nnml-prepare-save-mail-hook nil
  "Hook run narrowed to an article before saving.")

(defvoo nnml-inhibit-expiry nil
  "If non-nil, inhibit expiry.")




(defconst nnml-version "nnml 1.0"
  "nnml version.")

(defvoo nnml-nov-file-name ".overview")

(defvoo nnml-current-directory nil)
(defvoo nnml-current-group nil)
(defvoo nnml-status-string "")
(defvoo nnml-nov-buffer-alist nil)
(defvoo nnml-group-alist nil)
(defvoo nnml-active-timestamp nil)
(defvoo nnml-article-file-alist nil)

(defvoo nnml-generate-active-function 'nnml-generate-active-info)

(defvar nnml-nov-buffer-file-name nil)

(defvoo nnml-file-coding-system nnmail-file-coding-system)



;;; Interface functions.

(nnoo-define-basics nnml)

(deffoo nnml-retrieve-headers (sequence &optional group server fetch-old)
  (when (nnml-possibly-change-directory group server)
    (save-excursion
      (set-buffer nntp-server-buffer)
      (erase-buffer)
      (let ((file nil)
	    (number (length sequence))
	    (count 0)
	    (file-name-coding-system nnmail-pathname-coding-system)
	    beg article)
	(if (stringp (car sequence))
	    'headers
	  (if (nnml-retrieve-headers-with-nov sequence fetch-old)
	      'nov
	    (while sequence
	      (setq article (car sequence))
	      (setq file (nnml-article-to-file article))
	      (when (and file
			 (file-exists-p file)
			 (not (file-directory-p file)))
		(insert (format "221 %d Article retrieved.\n" article))
		(setq beg (point))
		(nnheader-insert-head file)
		(goto-char beg)
		(if (search-forward "\n\n" nil t)
		    (forward-char -1)
		  (goto-char (point-max))
		  (insert "\n\n"))
		(insert ".\n")
		(delete-region (point) (point-max)))
	      (setq sequence (cdr sequence))
	      (setq count (1+ count))
	      (and (numberp nnmail-large-newsgroup)
		   (> number nnmail-large-newsgroup)
		   (zerop (% count 20))
		   (nnheader-message 6 "nnml: Receiving headers... %d%%"
				     (/ (* count 100) number))))

	    (and (numberp nnmail-large-newsgroup)
		 (> number nnmail-large-newsgroup)
		 (nnheader-message 6 "nnml: Receiving headers...done"))

	    (nnheader-fold-continuation-lines)
	    'headers))))))

(deffoo nnml-open-server (server &optional defs)
  (nnoo-change-server 'nnml server defs)
  (when (not (file-exists-p nnml-directory))
    (ignore-errors (make-directory nnml-directory t)))
  (cond
   ((not (file-exists-p nnml-directory))
    (nnml-close-server)
    (nnheader-report 'nnml "Couldn't create directory: %s" nnml-directory))
   ((not (file-directory-p (file-truename nnml-directory)))
    (nnml-close-server)
    (nnheader-report 'nnml "Not a directory: %s" nnml-directory))
   (t
    (nnheader-report 'nnml "Opened server %s using directory %s"
		     server nnml-directory)
    t)))

(defun nnml-request-regenerate (server)
  (nnml-possibly-change-directory nil server)
  (nnml-generate-nov-databases)
  t)

(deffoo nnml-request-article (id &optional group server buffer)
  (nnml-possibly-change-directory group server)
  (let* ((nntp-server-buffer (or buffer nntp-server-buffer))
	 (file-name-coding-system nnmail-pathname-coding-system)
	 path gpath group-num)
    (if (stringp id)
	(when (and (setq group-num (nnml-find-group-number id))
		   (cdr
		    (assq (cdr group-num)
			  (nnheader-article-to-file-alist
			   (setq gpath
				 (nnmail-group-pathname
				  (car group-num)
				  nnml-directory))))))
	  (setq path (concat gpath (int-to-string (cdr group-num)))))
      (setq path (nnml-article-to-file id)))
    (cond
     ((not path)
      (nnheader-report 'nnml "No such article: %s" id))
     ((not (file-exists-p path))
      (nnheader-report 'nnml "No such file: %s" path))
     ((file-directory-p path)
      (nnheader-report 'nnml "File is a directory: %s" path))
     ((not (save-excursion (let ((nnmail-file-coding-system
				  nnml-file-coding-system))
			     (nnmail-find-file path))))
      (nnheader-report 'nnml "Couldn't read file: %s" path))
     (t
      (nnheader-report 'nnml "Article %s retrieved" id)
      ;; We return the article number.
      (cons (if group-num (car group-num) group)
	    (string-to-int (file-name-nondirectory path)))))))

(deffoo nnml-request-group (group &optional server dont-check)
  (let ((file-name-coding-system nnmail-pathname-coding-system))
    (cond
     ((not (nnml-possibly-change-directory group server))
      (nnheader-report 'nnml "Invalid group (no such directory)"))
     ((not (file-exists-p nnml-current-directory))
      (nnheader-report 'nnml "Directory %s does not exist"
		       nnml-current-directory))
     ((not (file-directory-p nnml-current-directory))
      (nnheader-report 'nnml "%s is not a directory" nnml-current-directory))
     (dont-check
      (nnheader-report 'nnml "Group %s selected" group)
      t)
     (t
      (nnheader-re-read-dir nnml-current-directory)
      (nnmail-activate 'nnml)
      (let ((active (nth 1 (assoc group nnml-group-alist))))
	(if (not active)
	    (nnheader-report 'nnml "No such group: %s" group)
	  (nnheader-report 'nnml "Selected group %s" group)
	  (nnheader-insert "211 %d %d %d %s\n"
			   (max (1+ (- (cdr active) (car active))) 0)
			   (car active) (cdr active) group)))))))

(deffoo nnml-request-scan (&optional group server)
  (setq nnml-article-file-alist nil)
  (nnml-possibly-change-directory group server)
  (nnmail-get-new-mail 'nnml 'nnml-save-nov nnml-directory group))

(deffoo nnml-close-group (group &optional server)
  (setq nnml-article-file-alist nil)
  t)

(deffoo nnml-request-create-group (group &optional server args)
  (nnml-possibly-change-directory nil server)
  (nnmail-activate 'nnml)
  (cond
   ((assoc group nnml-group-alist)
    t)
   ((and (file-exists-p (nnmail-group-pathname group nnml-directory))
	 (not (file-directory-p (nnmail-group-pathname group nnml-directory))))
    (nnheader-report 'nnml "%s is a file"
		     (nnmail-group-pathname group nnml-directory)))
   (t
    (let (active)
      (push (list group (setq active (cons 1 0)))
	    nnml-group-alist)
      (nnml-possibly-create-directory group)
      (nnml-possibly-change-directory group server)
      (let ((articles (nnheader-directory-articles nnml-current-directory)))
	(when articles
	  (setcar active (apply 'min articles))
	  (setcdr active (apply 'max articles))))
      (nnmail-save-active nnml-group-alist nnml-active-file)
      t))))

(deffoo nnml-request-list (&optional server)
  (save-excursion
    (let ((nnmail-file-coding-system nnmail-active-file-coding-system)
	  (file-name-coding-system nnmail-pathname-coding-system))
      (nnmail-find-file nnml-active-file))
    (setq nnml-group-alist (nnmail-get-active))
    t))

(deffoo nnml-request-newgroups (date &optional server)
  (nnml-request-list server))

(deffoo nnml-request-list-newsgroups (&optional server)
  (save-excursion
    (nnmail-find-file nnml-newsgroups-file)))

(deffoo nnml-request-expire-articles (articles group &optional server force)
  (nnml-possibly-change-directory group server)
  (let ((active-articles
	 (nnheader-directory-articles nnml-current-directory))
	(is-old t)
	article rest mod-time number)
    (nnmail-activate 'nnml)

    (setq active-articles (sort active-articles '<))
    ;; Articles not listed in active-articles are already gone,
    ;; so don't try to expire them.
    (setq articles (gnus-sorted-intersection articles active-articles))

    (while (and articles is-old)
      (when (setq article (nnml-article-to-file (setq number (pop articles))))
	(when (setq mod-time (nth 5 (file-attributes article)))
	  (if (and (nnml-deletable-article-p group number)
		   (setq is-old
			 (nnmail-expired-article-p group mod-time force
						   nnml-inhibit-expiry)))
	      (progn
		;; Allow a special target group.
		(unless (eq nnmail-expiry-target 'delete)
		  (with-temp-buffer
		    (nnml-request-article number group server
					  (current-buffer))
		    (let ((nnml-current-directory nil))
		      (nnmail-expiry-target-group
		       nnmail-expiry-target group))))
		(nnheader-message 5 "Deleting article %s in %s"
				  number group)
		(condition-case ()
		    (funcall nnmail-delete-file-function article)
		  (file-error
		   (push number rest)))
		(setq active-articles (delq number active-articles))
		(nnml-nov-delete-article group number))
	    (push number rest)))))
    (let ((active (nth 1 (assoc group nnml-group-alist))))
      (when active
	(setcar active (or (and active-articles
				(apply 'min active-articles))
			   (1+ (cdr active)))))
      (nnmail-save-active nnml-group-alist nnml-active-file))
    (nnml-save-nov)
    (nconc rest articles)))

(deffoo nnml-request-move-article
    (article group server accept-form &optional last)
  (let ((buf (get-buffer-create " *nnml move*"))
	result)
    (nnml-possibly-change-directory group server)
    (nnml-update-file-alist)
    (and
     (nnml-deletable-article-p group article)
     (nnml-request-article article group server)
     (let (nnml-current-directory 
	   nnml-current-group 
	   nnml-article-file-alist)
       (save-excursion
	 (set-buffer buf)
	 (insert-buffer-substring nntp-server-buffer)
	 (setq result (eval accept-form))
	 (kill-buffer (current-buffer))
	 result))
     (progn
       (nnml-possibly-change-directory group server)
       (condition-case ()
	   (funcall nnmail-delete-file-function
		    (nnml-article-to-file  article))
	 (file-error nil))
       (nnml-nov-delete-article group article)
       (when last
	 (nnml-save-nov)
	 (nnmail-save-active nnml-group-alist nnml-active-file))))
    result))

(deffoo nnml-request-accept-article (group &optional server last)
  (nnml-possibly-change-directory group server)
  (nnmail-check-syntax)
  (let (result)
    (when nnmail-cache-accepted-message-ids
      (nnmail-cache-insert (nnmail-fetch-field "message-id")))
    (if (stringp group)
	(and
	 (nnmail-activate 'nnml)
	 (setq result (car (nnml-save-mail
			    (list (cons group (nnml-active-number group))))))
	 (progn
	   (nnmail-save-active nnml-group-alist nnml-active-file)
	   (and last (nnml-save-nov))))
      (and
       (nnmail-activate 'nnml)
       (if (and (not (setq result (nnmail-article-group 'nnml-active-number)))
		(yes-or-no-p "Moved to `junk' group; delete article? "))
	   (setq result 'junk)
	 (setq result (car (nnml-save-mail result))))
       (when last
	 (nnmail-save-active nnml-group-alist nnml-active-file)
	 (when nnmail-cache-accepted-message-ids
	   (nnmail-cache-close))
	 (nnml-save-nov))))
    result))

(deffoo nnml-request-replace-article (article group buffer)
  (nnml-possibly-change-directory group)
  (save-excursion
    (set-buffer buffer)
    (nnml-possibly-create-directory group)
    (let ((chars (nnmail-insert-lines))
	  (art (concat (int-to-string article) "\t"))
	  headers)
      (when (ignore-errors
	      (nnmail-write-region
	       (point-min) (point-max)
	       (or (nnml-article-to-file article)
		   (expand-file-name (int-to-string article)
				     nnml-current-directory))
	       nil (if (nnheader-be-verbose 5) nil 'nomesg))
	      t)
	(setq headers (nnml-parse-head chars article))
	;; Replace the NOV line in the NOV file.
	(save-excursion
	  (set-buffer (nnml-open-nov group))
	  (goto-char (point-min))
	  (if (or (looking-at art)
		  (search-forward (concat "\n" art) nil t))
	      ;; Delete the old NOV line.
	      (delete-region (progn (beginning-of-line) (point))
			     (progn (forward-line 1) (point)))
	    ;; The line isn't here, so we have to find out where
	    ;; we should insert it.  (This situation should never
	    ;; occur, but one likes to make sure...)
	    (while (and (looking-at "[0-9]+\t")
			(< (string-to-int
			    (buffer-substring
			     (match-beginning 0) (match-end 0)))
			   article)
			(zerop (forward-line 1)))))
	  (beginning-of-line)
	  (nnheader-insert-nov headers)
	  (nnml-save-nov)
	  t)))))

(deffoo nnml-request-delete-group (group &optional force server)
  (nnml-possibly-change-directory group server)
  (when force
    ;; Delete all articles in GROUP.
    (let ((articles
	   (directory-files
	    nnml-current-directory t
	    (concat nnheader-numerical-short-files
		    "\\|" (regexp-quote nnml-nov-file-name) "$")))
	  article)
      (while articles
	(setq article (pop articles))
	(when (file-writable-p article)
	  (nnheader-message 5 "Deleting article %s in %s..." article group)
	  (funcall nnmail-delete-file-function article))))
    ;; Try to delete the directory itself.
    (ignore-errors (delete-directory nnml-current-directory)))
  ;; Remove the group from all structures.
  (setq nnml-group-alist
	(delq (assoc group nnml-group-alist) nnml-group-alist)
	nnml-current-group nil
	nnml-current-directory nil)
  ;; Save the active file.
  (nnmail-save-active nnml-group-alist nnml-active-file)
  t)

(deffoo nnml-request-rename-group (group new-name &optional server)
  (nnml-possibly-change-directory group server)
  (let ((new-dir (nnmail-group-pathname new-name nnml-directory))
	(old-dir (nnmail-group-pathname group nnml-directory)))
    (when (ignore-errors
	    (make-directory new-dir t)
	    t)
      ;; We move the articles file by file instead of renaming
      ;; the directory -- there may be subgroups in this group.
      ;; One might be more clever, I guess.
      (let ((files (nnheader-article-to-file-alist old-dir)))
	(while files
	  (rename-file
	   (concat old-dir (cdar files))
	   (concat new-dir (cdar files)))
	  (pop files)))
      ;; Move .overview file.
      (let ((overview (concat old-dir nnml-nov-file-name)))
	(when (file-exists-p overview)
	  (rename-file overview (concat new-dir nnml-nov-file-name))))
      (when (<= (length (directory-files old-dir)) 2)
	(ignore-errors (delete-directory old-dir)))
      ;; That went ok, so we change the internal structures.
      (let ((entry (assoc group nnml-group-alist)))
	(when entry
	  (setcar entry new-name))
	(setq nnml-current-directory nil
	      nnml-current-group nil)
	;; Save the new group alist.
	(nnmail-save-active nnml-group-alist nnml-active-file)
	t))))

(deffoo nnml-set-status (article name value &optional group server)
  (nnml-possibly-change-directory group server)
  (let ((file (nnml-article-to-file article)))
    (cond
     ((not (file-exists-p file))
      (nnheader-report 'nnml "File %s does not exist" file))
     (t
      (with-temp-file file
	(nnheader-insert-file-contents file)
	(nnmail-replace-status name value))
      t))))


;;; Internal functions.

(defun nnml-article-to-file (article)
  (nnml-update-file-alist)
  (let (file)
    (if (setq file (cdr (assq article nnml-article-file-alist)))
	(expand-file-name file nnml-current-directory)
      ;; Just to make sure nothing went wrong when reading over NFS --
      ;; check once more.
      (when (file-exists-p
	     (setq file (expand-file-name (number-to-string article)
					  nnml-current-directory)))
	(nnml-update-file-alist t)
	file))))

(defun nnml-deletable-article-p (group article)
  "Say whether ARTICLE in GROUP can be deleted."
  (let (path)
    (when (setq path (nnml-article-to-file article))
      (when (file-writable-p path)
	(or (not nnmail-keep-last-article)
	    (not (eq (cdr (nth 1 (assoc group nnml-group-alist)))
		     article)))))))

;; Find an article number in the current group given the Message-ID.
(defun nnml-find-group-number (id)
  (save-excursion
    (set-buffer (get-buffer-create " *nnml id*"))
    (let ((alist nnml-group-alist)
	  number)
      ;; We want to look through all .overview files, but we want to
      ;; start with the one in the current directory.  It seems most
      ;; likely that the article we are looking for is in that group.
      (if (setq number (nnml-find-id nnml-current-group id))
	  (cons nnml-current-group number)
	;; It wasn't there, so we look through the other groups as well.
	(while (and (not number)
		    alist)
	  (or (string= (caar alist) nnml-current-group)
	      (setq number (nnml-find-id (caar alist) id)))
	  (or number
	      (setq alist (cdr alist))))
	(and number
	     (cons (caar alist) number))))))

(defun nnml-find-id (group id)
  (erase-buffer)
  (let ((nov (expand-file-name nnml-nov-file-name
			       (nnmail-group-pathname group nnml-directory)))
	number found)
    (when (file-exists-p nov)
      (nnheader-insert-file-contents nov)
      (while (and (not found)
		  (search-forward id nil t)) ; We find the ID.
	;; And the id is in the fourth field.
	(if (not (and (search-backward "\t" nil t 4)
		      (not (search-backward"\t" (gnus-point-at-bol) t))))
	    (forward-line 1)
	  (beginning-of-line)
	  (setq found t)
	  ;; We return the article number.
	  (setq number
		(ignore-errors (read (current-buffer))))))
      number)))

(defun nnml-retrieve-headers-with-nov (articles &optional fetch-old)
  (if (or gnus-nov-is-evil nnml-nov-is-evil)
      nil
    (let ((nov (expand-file-name nnml-nov-file-name nnml-current-directory)))
      (when (file-exists-p nov)
	(save-excursion
	  (set-buffer nntp-server-buffer)
	  (erase-buffer)
	  (nnheader-insert-file-contents nov)
	  (if (and fetch-old
		   (not (numberp fetch-old)))
	      t				; Don't remove anything.
	    (nnheader-nov-delete-outside-range
	     (if fetch-old (max 1 (- (car articles) fetch-old))
	       (car articles))
	     (car (last articles)))
	    t))))))

(defun nnml-possibly-change-directory (group &optional server)
  (when (and server
	     (not (nnml-server-opened server)))
    (nnml-open-server server))
  (if (not group)
      t
    (let ((pathname (nnmail-group-pathname group nnml-directory))
	  (file-name-coding-system nnmail-pathname-coding-system))
      (when (not (equal pathname nnml-current-directory))
	(setq nnml-current-directory pathname
	      nnml-current-group group
	      nnml-article-file-alist nil))
      (file-exists-p nnml-current-directory))))

(defun nnml-possibly-create-directory (group)
  (let ((dir (nnmail-group-pathname group nnml-directory)))
    (unless (file-exists-p dir)
      (make-directory (directory-file-name dir) t)
      (nnheader-message 5 "Creating mail directory %s" dir))))

(defun nnml-save-mail (group-art)
  "Called narrowed to an article."
  (let (chars headers)
    (setq chars (nnmail-insert-lines))
    (nnmail-insert-xref group-art)
    (run-hooks 'nnmail-prepare-save-mail-hook)
    (run-hooks 'nnml-prepare-save-mail-hook)
    (goto-char (point-min))
    (while (looking-at "From ")
      (replace-match "X-From-Line: ")
      (forward-line 1))
    ;; We save the article in all the groups it belongs in.
    (let ((ga group-art)
	  first)
      (while ga
	(nnml-possibly-create-directory (caar ga))
	(let ((file (concat (nnmail-group-pathname
			     (caar ga) nnml-directory)
			    (int-to-string (cdar ga)))))
	  (if first
	      ;; It was already saved, so we just make a hard link.
	      (funcall nnmail-crosspost-link-function first file t)
	    ;; Save the article.
	    (nnmail-write-region (point-min) (point-max) file nil
				 (if (nnheader-be-verbose 5) nil 'nomesg))
	    (setq first file)))
	(setq ga (cdr ga))))
    ;; Generate a nov line for this article.  We generate the nov
    ;; line after saving, because nov generation destroys the
    ;; header.
    (setq headers (nnml-parse-head chars))
    ;; Output the nov line to all nov databases that should have it.
    (let ((ga group-art))
      (while ga
	(nnml-add-nov (caar ga) (cdar ga) headers)
	(setq ga (cdr ga))))
    group-art))

(defun nnml-active-number (group)
  "Compute the next article number in GROUP."
  (let ((active (cadr (assoc group nnml-group-alist))))
    ;; The group wasn't known to nnml, so we just create an active
    ;; entry for it.
    (unless active
      ;; Perhaps the active file was corrupt?  See whether
      ;; there are any articles in this group.
      (nnml-possibly-create-directory group)
      (nnml-possibly-change-directory group)
      (unless nnml-article-file-alist
	(setq nnml-article-file-alist
	      (sort
	       (nnheader-article-to-file-alist nnml-current-directory)
	       'car-less-than-car)))
      (setq active
	    (if nnml-article-file-alist
		(cons (caar nnml-article-file-alist)
		      (caar (last nnml-article-file-alist)))
	      (cons 1 0)))
      (push (list group active) nnml-group-alist))
    (setcdr active (1+ (cdr active)))
    (while (file-exists-p
	    (expand-file-name (int-to-string (cdr active))
			      (nnmail-group-pathname group nnml-directory)))
      (setcdr active (1+ (cdr active))))
    (cdr active)))

(defun nnml-add-nov (group article headers)
  "Add a nov line for the GROUP base."
  (save-excursion
    (set-buffer (nnml-open-nov group))
    (goto-char (point-max))
    (mail-header-set-number headers article)
    (nnheader-insert-nov headers)))

(defsubst nnml-header-value ()
  (buffer-substring (match-end 0) (progn (end-of-line) (point))))

(defun nnml-parse-head (chars &optional number)
  "Parse the head of the current buffer."
  (save-excursion
    (save-restriction
      (unless (zerop (buffer-size))
	(narrow-to-region
	 (goto-char (point-min))
	 (if (search-forward "\n\n" nil t) (1- (point)) (point-max))))
      ;; Fold continuation lines.
      (goto-char (point-min))
      (while (re-search-forward "\\(\r?\n[ \t]+\\)+" nil t)
	(replace-match " " t t))
      ;; Remove any tabs; they are too confusing.
      (subst-char-in-region (point-min) (point-max) ?\t ? )
      (let ((headers (nnheader-parse-head t)))
	(mail-header-set-chars headers chars)
	(mail-header-set-number headers number)
	headers))))

(defun nnml-open-nov (group)
  (or (cdr (assoc group nnml-nov-buffer-alist))
      (let ((buffer (get-buffer-create (format " *nnml overview %s*" group))))
	(save-excursion
	  (set-buffer buffer)
	  (set (make-local-variable 'nnml-nov-buffer-file-name)
	       (expand-file-name
		nnml-nov-file-name
		(nnmail-group-pathname group nnml-directory)))
	  (erase-buffer)
	  (when (file-exists-p nnml-nov-buffer-file-name)
	    (nnheader-insert-file-contents nnml-nov-buffer-file-name)))
	(push (cons group buffer) nnml-nov-buffer-alist)
	buffer)))

(defun nnml-save-nov ()
  (save-excursion
    (while nnml-nov-buffer-alist
      (when (buffer-name (cdar nnml-nov-buffer-alist))
	(set-buffer (cdar nnml-nov-buffer-alist))
	(when (buffer-modified-p)
	  (nnmail-write-region (point-min) (point-max)
			       nnml-nov-buffer-file-name nil 'nomesg))
	(set-buffer-modified-p nil)
	(kill-buffer (current-buffer)))
      (setq nnml-nov-buffer-alist (cdr nnml-nov-buffer-alist)))))

;;;###autoload
(defun nnml-generate-nov-databases ()
  "Generate NOV databases in all nnml directories."
  (interactive)
  ;; Read the active file to make sure we don't re-use articles
  ;; numbers in empty groups.
  (nnmail-activate 'nnml)
  (nnml-open-server (or (nnoo-current-server 'nnml) ""))
  (setq nnml-directory (expand-file-name nnml-directory))
  ;; Recurse down the directories.
  (nnml-generate-nov-databases-1 nnml-directory nil t)
  ;; Save the active file.
  (nnmail-save-active nnml-group-alist nnml-active-file))

(defun nnml-generate-nov-databases-1 (dir &optional seen no-active)
  "Regenerate the NOV database in DIR."
  (interactive "DRegenerate NOV in: ")
  (setq dir (file-name-as-directory dir))
  ;; Only scan this sub-tree if we haven't been here yet.
  (unless (member (file-truename dir) seen)
    (push (file-truename dir) seen)
    ;; We descend recursively
    (let ((dirs (directory-files dir t nil t))
	  dir)
      (while (setq dir (pop dirs))
	(when (and (not (string-match "^\\." (file-name-nondirectory dir)))
		   (file-directory-p dir))
	  (nnml-generate-nov-databases-1 dir seen))))
    ;; Do this directory.
    (let ((files (sort (nnheader-article-to-file-alist dir)
		       'car-less-than-car)))
      (if (not files)
	  (let* ((group (nnheader-file-to-group
			 (directory-file-name dir) nnml-directory))
		 (info (cadr (assoc group nnml-group-alist))))
	    (when info
	      (setcar info (1+ (cdr info)))))
	(funcall nnml-generate-active-function dir)
	;; Generate the nov file.
	(nnml-generate-nov-file dir files)
	(unless no-active
	  (nnmail-save-active nnml-group-alist nnml-active-file))))))

(eval-when-compile (defvar files))
(defun nnml-generate-active-info (dir)
  ;; Update the active info for this group.
  (let ((group (nnheader-file-to-group
		(directory-file-name dir) nnml-directory)))
    (setq nnml-group-alist
	  (delq (assoc group nnml-group-alist) nnml-group-alist))
    (push (list group
		(cons (caar files)
		      (let ((f files))
			(while (cdr f) (setq f (cdr f)))
			(caar f))))
	  nnml-group-alist)))

(defun nnml-generate-nov-file (dir files)
  (let* ((dir (file-name-as-directory dir))
	 (nov (concat dir nnml-nov-file-name))
	 (nov-buffer (get-buffer-create " *nov*"))
	 chars file headers)
    (save-excursion
      ;; Init the nov buffer.
      (set-buffer nov-buffer)
      (buffer-disable-undo)
      (erase-buffer)
      (set-buffer nntp-server-buffer)
      ;; Delete the old NOV file.
      (when (file-exists-p nov)
	(funcall nnmail-delete-file-function nov))
      (while files
	(unless (file-directory-p (setq file (concat dir (cdar files))))
	  (erase-buffer)
	  (nnheader-insert-file-contents file)
	  (narrow-to-region
	   (goto-char (point-min))
	   (progn
	     (search-forward "\n\n" nil t)
	     (setq chars (- (point-max) (point)))
	     (max (point-min) (1- (point)))))
	  (unless (zerop (buffer-size))
	    (goto-char (point-min))
	    (setq headers (nnml-parse-head chars (caar files)))
	    (save-excursion
	      (set-buffer nov-buffer)
	      (goto-char (point-max))
	      (nnheader-insert-nov headers)))
	  (widen))
	(setq files (cdr files)))
      (save-excursion
	(set-buffer nov-buffer)
	(nnmail-write-region (point-min) (point-max) nov nil 'nomesg)
	(kill-buffer (current-buffer))))))

(defun nnml-nov-delete-article (group article)
  (save-excursion
    (set-buffer (nnml-open-nov group))
    (when (nnheader-find-nov-line article)
      (delete-region (point) (progn (forward-line 1) (point)))
      (when (bobp)
	(let ((active (cadr (assoc group nnml-group-alist)))
	      num)
	  (when active
	    (if (eobp)
		(setf (car active) (1+ (cdr active)))
	      (when (and (setq num (ignore-errors (read (current-buffer))))
			 (numberp num))
		(setf (car active) num)))))))
    t))

(defun nnml-update-file-alist (&optional force)
  (when (or (not nnml-article-file-alist)
	    force)
    (setq nnml-article-file-alist
	  (nnheader-article-to-file-alist nnml-current-directory))))

(provide 'nnml)

;;; nnml.el ends here
