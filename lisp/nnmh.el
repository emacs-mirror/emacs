;;; nnmh.el --- mhspool access for Gnus
;; Copyright (C) 1995 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Based on nnspool.el by Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>.
;; For an overview of what the interface functions do, please see the
;; Gnus sources.  

;;; Code:

(require 'nnheader)
(require 'rmail)
(require 'nnmail)
(require 'gnus)

(defvar nnmh-directory "~/Mail/"
  "*Mail spool directory.")

(defvar nnmh-get-new-mail t
  "*If non-nil, nnmh will check the incoming mail file and split the mail.")

(defvar nnmh-prepare-save-mail-hook nil
  "*Hook run narrowed to an article before saving.")

(defvar nnmh-be-safe nil
  "*If non-nil, nnmh will check all articles to make sure whether they are new or not.")



(defconst nnmh-version "nnmh 1.0"
  "nnmh version.")

(defvar nnmh-current-directory nil
  "Current news group directory.")

(defvar nnmh-status-string "")
(defvar nnmh-group-alist nil)



(defvar nnmh-current-server nil)
(defvar nnmh-server-alist nil)
(defvar nnmh-server-variables 
  (list
   (list 'nnmh-directory nnmh-directory)
   (list 'nnmh-get-new-mail nnmh-get-new-mail)
   '(nnmh-current-directory nil)
   '(nnmh-status-string "")
   '(nnmh-group-alist)))



;;; Interface functions.

(defun nnmh-retrieve-headers (sequence &optional newsgroup server)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (let* ((file nil)
	   (number (length sequence))
	   (large (and (numberp nnmail-large-newsgroup)
		       (> number nnmail-large-newsgroup)))
	   (count 0)
	   beg article)
      (nnmh-possibly-change-directory newsgroup)
      (if (stringp (car sequence))
	  'headers
	(while sequence
	  (setq article (car sequence))
	  (setq file
		(concat nnmh-current-directory (int-to-string article)))
	  (if (and (file-exists-p file)
		   (not (file-directory-p file)))
	      (progn
		(insert (format "221 %d Article retrieved.\n" article))
		(setq beg (point))
		(nnheader-insert-head file)
		(goto-char beg)
		(if (search-forward "\n\n" nil t)
		    (forward-char -1)
		  (goto-char (point-max))
		  (insert "\n\n"))
		(insert ".\n")
		(delete-region (point) (point-max))))
	  (setq sequence (cdr sequence))
	  (setq count (1+ count))

	  (and large
	       (zerop (% count 20))
	       (message "nnmh: Receiving headers... %d%%"
			(/ (* count 100) number))))

	(and large (message "nnmh: Receiving headers...done"))

	;; Fold continuation lines.
	(goto-char (point-min))
	(while (re-search-forward "\\(\r?\n[ \t]+\\)+" nil t)
	  (replace-match " " t t))
	'headers))))

(defun nnmh-open-server (server &optional defs)
  (nnheader-init-server-buffer)
  (if (equal server nnmh-current-server)
      t
    (if nnmh-current-server
	(setq nnmh-server-alist 
	      (cons (list nnmh-current-server
			  (nnheader-save-variables nnmh-server-variables))
		    nnmh-server-alist)))
    (let ((state (assoc server nnmh-server-alist)))
      (if state 
	  (progn
	    (nnheader-restore-variables (nth 1 state))
	    (setq nnmh-server-alist (delq state nnmh-server-alist)))
	(nnheader-set-init-variables nnmh-server-variables defs)))
    (setq nnmh-current-server server)))

(defun nnmh-close-server (&optional server)
  t)

(defun nnmh-server-opened (&optional server)
  (and (equal server nnmh-current-server)
       nntp-server-buffer
       (buffer-name nntp-server-buffer)))

(defun nnmh-status-message (&optional server)
  nnmh-status-string)

(defun nnmh-request-article (id &optional newsgroup server buffer)
  (nnmh-possibly-change-directory newsgroup)
  (let ((file (if (stringp id)
		  nil
		(concat nnmh-current-directory (int-to-string id))))
	(nntp-server-buffer (or buffer nntp-server-buffer)))
    (and (stringp file)
	 (file-exists-p file)
	 (not (file-directory-p file))
	 (save-excursion (nnmail-find-file file)))))

(defun nnmh-request-group (group &optional server dont-check)
  (and nnmh-get-new-mail (or dont-check (nnmh-get-new-mail group)))
  (let ((pathname (nnmh-article-pathname group nnmh-directory))
	dir)
    (if (file-directory-p pathname)
	(progn
	  (setq nnmh-current-directory pathname)
	  (and nnmh-get-new-mail 
	       nnmh-be-safe
	       (nnmh-update-gnus-unreads group))
	  (or dont-check
	      (progn
		(setq dir 
		      (sort
		       (mapcar
			(function
			 (lambda (name)
			   (string-to-int name)))
			(directory-files pathname nil "^[0-9]+$" t))
		       '<))
		(save-excursion
		  (set-buffer nntp-server-buffer)
		  (erase-buffer)
		  (if dir
		      (insert (format "211 %d %d %d %s\n" (length dir) 
				      (car dir)
				      (progn (while (cdr dir)
					       (setq dir (cdr dir)))
					     (car dir))
				      group))
		    (insert (format "211 0 1 0 %s\n" group))))))
	  t)
      (setq nnmh-status-string "No such group")
      nil)))

(defun nnmh-request-list (&optional server dir)
  (or dir
      (save-excursion
	(set-buffer nntp-server-buffer)
	(erase-buffer)
	(setq dir (file-truename (file-name-as-directory nnmh-directory)))))
  (setq dir (expand-file-name dir))
  ;; Recurse down all directories.
  (let ((dirs (and (file-readable-p dir)
		   (> (nth 1 (file-attributes (file-chase-links dir))) 2)
		   (directory-files dir t nil t))))
    (while dirs 
      (if (and (not (string-match "/\\.\\.?$" (car dirs)))
	       (file-directory-p (car dirs))
	       (file-readable-p (car dirs)))
	  (nnmh-request-list nil (car dirs)))
      (setq dirs (cdr dirs))))
  ;; For each directory, generate an active file line.
  (if (not (string= (expand-file-name nnmh-directory) dir))
      (let ((files (mapcar
		    (lambda (name) (string-to-int name))
		    (directory-files dir nil "^[0-9]+$" t))))
	(if (null files)
	    ()
	  (save-excursion
	    (set-buffer nntp-server-buffer)
	    (goto-char (point-max))
	    (insert 
	     (format 
	      "%s %d %d y\n" 
	      (progn
		(string-match 
		 (file-truename (file-name-as-directory 
				 (expand-file-name nnmh-directory))) dir)
		(nnmail-replace-chars-in-string
		 (substring dir (match-end 0)) ?/ ?.))
	      (apply (function max) files) 
	      (apply (function min) files)))))))
  (setq nnmh-group-alist (nnmail-get-active))
  (and server nnmh-get-new-mail (nnmh-get-new-mail))
  t)

(defun nnmh-request-newgroups (date &optional server)
  (nnmh-request-list server))

(defun nnmh-request-post (&optional server)
  (mail-send-and-exit nil))

(defalias 'nnmh-request-post-buffer 'nnmail-request-post-buffer)

(defun nnmh-request-expire-articles (articles newsgroup &optional server force)
  (nnmh-possibly-change-directory newsgroup)
  (let* ((days (or (and nnmail-expiry-wait-function
			(funcall nnmail-expiry-wait-function newsgroup))
		   nnmail-expiry-wait))
	 (active-articles 
	  (mapcar
	   (function
	    (lambda (name)
	      (string-to-int name)))
	   (directory-files nnmh-current-directory nil "^[0-9]+$" t)))
	 (max-article (and active-articles (apply 'max active-articles)))
	 (is-old t)
	 article rest mod-time)
    (nnmail-activate 'nnmh)

    (while (and articles is-old)
      (setq article (concat nnmh-current-directory 
			    (int-to-string (car articles))))
      (if (setq mod-time (nth 5 (file-attributes article)))
	  (if (and (or (not nnmail-keep-last-article)
		       (not max-article)
		       (not (= (car articles) max-article)))
		   (not (equal mod-time '(0 0)))
		   (or force
		       (setq is-old
			     (> (nnmail-days-between
				 (current-time-string)
				 (current-time-string mod-time))
				days))))
	      (progn
		(and gnus-verbose-backends 
		     (message "Deleting article %s..." article))
		(condition-case ()
		    (delete-file article)
		  (file-error
		   (setq rest (cons (car articles) rest)))))
	    (setq rest (cons (car articles) rest))))
      (setq articles (cdr articles)))
    (message "")
    (nconc rest articles)))

(defun nnmh-close-group (group &optional server)
  t)

(defun nnmh-request-move-article 
  (article group server accept-form &optional last)
  (let ((buf (get-buffer-create " *nnmh move*"))
	result)
    (and 
     (nnmh-request-article article group server)
     (save-excursion
       (set-buffer buf)
       (insert-buffer-substring nntp-server-buffer)
       (setq result (eval accept-form))
       (kill-buffer (current-buffer))
       result)
     (condition-case ()
	 (delete-file (concat nnmh-current-directory 
			      (int-to-string article)))
       (file-error nil)))
    result))

(defun nnmh-request-accept-article (group &optional last)
  (if (stringp group)
      (and 
       (nnmail-activate 'nnmh)
       ;; We trick the choosing function into believing that only one
       ;; group is available.  
       (let ((nnmail-split-methods (list (list group ""))))
	 (car (nnmh-save-mail))))
    (and
     (nnmail-activate 'nnmh)
     (car (nnmh-save-mail)))))

(defun nnmh-request-replace-article (article group buffer)
  (nnmh-possibly-change-directory group)
  (save-excursion
    (set-buffer buffer)
    (nnmh-possibly-create-directory group)
    (condition-case ()
	(progn
	  (write-region (point-min) (point-max)
			(concat nnmh-current-directory (int-to-string article))
			nil (if gnus-verbose-backends nil 'nomesg))
	  t)
      (error nil))))


;;; Internal functions.

(defun nnmh-possibly-change-directory (newsgroup)
  (if newsgroup
      (let ((pathname (nnmh-article-pathname newsgroup nnmh-directory)))
	(if (file-directory-p pathname)
	    (setq nnmh-current-directory pathname)
	  (error "No such newsgroup: %s" newsgroup)))))

(defun nnmh-possibly-create-directory (group)
  (let (dir dirs)
    (setq dir (nnmh-article-pathname group nnmh-directory))
    (while (not (file-directory-p dir))
      (setq dirs (cons dir dirs))
      (setq dir (file-name-directory (directory-file-name dir))))
    (while dirs
      (if (make-directory (directory-file-name (car dirs)))
	  (error "Could not create directory %s" (car dirs)))
      (and gnus-verbose-backends 
	   (message "Creating mail directory %s" (car dirs)))
      (setq dirs (cdr dirs)))))
	     
(defun nnmh-save-mail ()
  "Called narrowed to an article."
  (let ((group-art (nreverse (nnmail-article-group 'nnmh-active-number))))
    (nnmail-insert-lines)
    (nnmail-insert-xref group-art)
    (run-hooks 'nnmh-prepare-save-mail-hook)
    (goto-char (point-min))
    (while (looking-at "From ")
      (replace-match "X-From-Line: ")
      (forward-line 1))
    ;; We save the article in all the newsgroups it belongs in.
    (let ((ga group-art)
	  first)
      (while ga
	(nnmh-possibly-create-directory (car (car ga)))
	(let ((file (concat (nnmh-article-pathname 
			     (car (car ga)) nnmh-directory) 
			    (int-to-string (cdr (car ga))))))
	  (if first
	      ;; It was already saved, so we just make a hard link.
	      (add-name-to-file first file t)
	    ;; Save the article.
	    (write-region (point-min) (point-max) file nil nil)
	    (setq first file)))
	(setq ga (cdr ga))))
    group-art))

(defun nnmh-active-number (group)
  "Compute the next article number in GROUP."
  (let ((active (car (cdr (assoc group nnmh-group-alist)))))
    ;; The group wasn't known to nnmh, so we just create an active
    ;; entry for it.   
    (or active
	(progn
	  (setq active (cons 1 0))
	  (setq nnmh-group-alist (cons (list group active) nnmh-group-alist))))
    (setcdr active (1+ (cdr active)))
    (while (file-exists-p
	    (concat (nnmh-article-pathname group nnmh-directory)
		    (int-to-string (cdr active))))
      (setcdr active (1+ (cdr active))))
    (cdr active)))

(defun nnmh-article-pathname (group mail-dir)
  "Make pathname for GROUP."
  (let ((mail-dir (file-name-as-directory (expand-file-name mail-dir))))
    (if (file-directory-p (concat mail-dir group))
	(concat mail-dir group "/")
      (concat mail-dir (nnmail-replace-chars-in-string group ?. ?/) "/"))))

(defun nnmh-get-new-mail (&optional group)
  "Read new incoming mail."
  (let* ((spools (nnmail-get-spool-files group))
	 (group-in group)
	 incoming incomings)
    (if (or (not nnmh-get-new-mail) (not nnmail-spool-file))
	()
      ;; We first activate all the groups.
      (or nnmh-group-alist
	  (nnmh-request-list))
      ;; The we go through all the existing spool files and split the
      ;; mail from each.
      (while spools
	(and
	 (file-exists-p (car spools))
	 (> (nth 7 (file-attributes (car spools))) 0)
	 (progn
	   (and gnus-verbose-backends 
		(message "nnmh: Reading incoming mail..."))
	   (if (not (setq incoming 
			  (nnmail-move-inbox 
			   (car spools) 
			   (concat (file-name-as-directory nnmh-directory)
				   "Incoming"))))
	       ()
	     (setq incomings (cons incoming incomings))
	     (setq group (nnmail-get-split-group (car spools) group-in))
	     (nnmail-split-incoming incoming 'nnmh-save-mail nil group))))
	(setq spools (cdr spools)))
      ;; If we did indeed read any incoming spools, we save all info. 
      (if incoming 
	  (message "nnmh: Reading incoming mail...done"))
      (while incomings
	(setq incoming (car incomings))
	(and nnmail-delete-incoming
	     (file-exists-p incoming)
	     (file-writable-p incoming)
	     (delete-file incoming))
	(setq incomings (cdr incomings))))))
      

(defun nnmh-update-gnus-unreads (group)
  ;; Go through the .nnmh-articles file and compare with the actual
  ;; articles in this folder. The articles that are "new" will be
  ;; marked as unread by Gnus.
  (let* ((dir nnmh-current-directory)
	 (files (sort (mapcar (function (lambda (name) (string-to-int name)))
			      (directory-files nnmh-current-directory 
					       nil "^[0-9]+$" t)) '<))
	 (nnmh-file (concat dir ".nnmh-articles"))
	 new articles)
    ;; Load the .nnmh-articles file.
    (if (file-exists-p nnmh-file)
	(setq articles 
	      (let (nnmh-newsgroup-articles)
		(condition-case nil (load nnmh-file nil t t) (error nil))
		nnmh-newsgroup-articles)))
    ;; Add all new articles to the `new' list.
    (let ((art files))
      (while art
	(if (not (assq (car art) articles)) (setq new (cons (car art) new)))
	(setq art (cdr art))))
    ;; Remove all deleted articles.
    (let ((art articles))
      (while art
	(if (not (memq (car (car art)) files))
	    (setq articles (delq (car art) articles)))
	(setq art (cdr art))))
    ;; Check whether the highest-numbered articles really are the ones
    ;; that Gnus thinks they are by looking at the time-stamps.
    (let ((art articles))
      (while (and art 
		  (not (equal 
			(nth 5 (file-attributes 
				(concat dir (int-to-string (car (car art))))))
			(cdr (car art)))))
	(setq articles (delq (car art) articles))
	(setq new (cons (car (car art)) new))
	(setq art (cdr art))))
    ;; Go through all the new articles and add them, and their
    ;; time-stamps to the list.
    (let ((n new))
      (while n
	(setq articles 
	      (cons (cons 
		     (car n)
		     (nth 5 (file-attributes 
			     (concat dir (int-to-string (car n))))))
		    articles))
	(setq n (cdr n))))
    ;; Make Gnus mark all new articles as unread.
    (or (zerop (length new))
	(gnus-make-articles-unread 
	 (gnus-group-prefixed-name group (list 'nnmh ""))
	 (setq new (sort new '<))))
    ;; Sort the article list with highest numbers first.
    (setq articles (sort articles (lambda (art1 art2) 
				    (> (car art1) (car art2)))))
    ;; Finally write this list back to the .nnmh-articles file.
    (save-excursion
      (set-buffer (get-buffer-create "*nnmh out*"))
      (insert ";; Gnus article active file for " group "\n\n")
      (insert "(setq nnmh-newsgroup-articles '")
      (insert (prin1-to-string articles) ")\n")
      (write-region (point-min) (point-max) nnmh-file nil 'nomesg)
      (kill-buffer (current-buffer)))))

(provide 'nnmh)

;;; nnmh.el ends here
