;;; nnml.el --- mail spool access for Gnus

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

(defvar nnml-directory "~/Mail/"
  "Mail spool directory.")

(defvar nnml-active-file (concat nnml-directory "active")
  "Mail active file.")

(defvar nnml-newsgroups-file (concat nnml-directory "newsgroups")
  "Mail newsgroups description file.")

(defvar nnml-get-new-mail t
  "If non-nil, nnml will check the incoming mail file and split the mail.")

(defvar nnml-nov-is-evil nil
  "If non-nil, Gnus will never generate and use nov databases for mail groups.
Using nov databases will speed up header fetching considerably.
This variable shouldn't be flipped much. If you have, for some reason,
set this to t, and want to set it to nil again, you should always run
the `nnml-generate-nov-databases' command. The function will go
through all nnml directories and generate nov databases for them
all. This may very well take some time.")

(defvar nnml-prepare-save-mail-hook nil
  "Hook run narrowed to an article before saving.")



(defconst nnml-version "nnml 1.0"
  "nnml version.")

(defvar nnml-nov-file-name ".overview")

(defvar nnml-current-directory nil)
(defvar nnml-status-string "")
(defvar nnml-nov-buffer-alist nil)
(defvar nnml-group-alist nil)
(defvar nnml-active-timestamp nil)



;; Server variables.

(defvar nnml-current-server nil)
(defvar nnml-server-alist nil)
(defvar nnml-server-variables 
  (list 
   (list 'nnml-directory nnml-directory)
   (list 'nnml-active-file nnml-active-file)
   (list 'nnml-newsgroups-file nnml-newsgroups-file)
   (list 'nnml-get-new-mail nnml-get-new-mail)
   (list 'nnml-nov-is-evil nnml-nov-is-evil)
   (list 'nnml-nov-file-name nnml-nov-file-name)
   '(nnml-current-directory nil)
   '(nnml-status-string "")
   '(nnml-nov-buffer-alist nil)
   '(nnml-group-alist nil)
   '(nnml-active-timestamp nil)))



;;; Interface functions.

(defun nnml-retrieve-headers (sequence &optional newsgroup server)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (let ((file nil)
	  (number (length sequence))
	  (count 0)
	  beg article)
      (if (stringp (car sequence))
	  'headers
	(nnml-possibly-change-directory newsgroup)
	(if (nnml-retrieve-headers-with-nov sequence)
	    'nov
	  (while sequence
	    (setq article (car sequence))
	    (setq file
		  (concat nnml-current-directory (int-to-string article)))
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
	    (and (numberp nnmail-large-newsgroup)
		 (> number nnmail-large-newsgroup)
		 (zerop (% count 20))
		 gnus-verbose-backends
		 (message "nnml: Receiving headers... %d%%"
			  (/ (* count 100) number))))

	  (and (numberp nnmail-large-newsgroup)
	       (> number nnmail-large-newsgroup)
	       gnus-verbose-backends
	       (message "nnml: Receiving headers...done"))

	  ;; Fold continuation lines.
	  (goto-char (point-min))
	  (while (re-search-forward "\\(\r?\n[ \t]+\\)+" nil t)
	    (replace-match " " t t))
	  'headers)))))

(defun nnml-open-server (server &optional defs)
  (nnheader-init-server-buffer)
  (if (equal server nnml-current-server)
      t
    (if nnml-current-server
	(setq nnml-server-alist 
	      (cons (list nnml-current-server
			  (nnheader-save-variables nnml-server-variables))
		    nnml-server-alist)))
    (let ((state (assoc server nnml-server-alist)))
      (if state 
	  (progn
	    (nnheader-restore-variables (nth 1 state))
	    (setq nnml-server-alist (delq state nnml-server-alist)))
	(nnheader-set-init-variables nnml-server-variables defs)))
    (setq nnml-current-server server)))

(defun nnml-close-server (&optional server)
  t)

(defun nnml-server-opened (&optional server)
  (and (equal server nnml-current-server)
       nntp-server-buffer
       (buffer-name nntp-server-buffer)))

(defun nnml-status-message (&optional server)
  nnml-status-string)

(defun nnml-request-article (id &optional newsgroup server buffer)
  (nnml-possibly-change-directory newsgroup)
  (let ((file (if (stringp id)
		  nil
		(concat nnml-current-directory (int-to-string id))))
	(nntp-server-buffer (or buffer nntp-server-buffer)))
    (if (and (stringp file)
	     (file-exists-p file)
	     (not (file-directory-p file)))
	(save-excursion
	  (nnmail-find-file file)))))

(defun nnml-request-group (group &optional server dont-check)
  (if (not (nnml-possibly-change-directory group))
      (progn
	(setq nnml-status-string "Invalid group (no such directory)")
	nil)
    (if dont-check 
	t
      (nnml-get-new-mail group)
      (nnmail-activate 'nnml)
      (let ((active (nth 1 (assoc group nnml-group-alist))))
	(save-excursion
	  (set-buffer nntp-server-buffer)
	  (erase-buffer)
	  (if (not active)
	      ()
	    (insert (format "211 %d %d %d %s\n" 
			    (max (1+ (- (cdr active) (car active))) 0)
			    (car active) (cdr active) group))
	    t))))))

(defun nnml-close-group (group &optional server)
  t)

(defun nnml-request-close ()
  (setq nnml-current-server nil)
  (setq nnml-server-alist nil)
  t)

(defun nnml-request-create-group (group &optional server) 
  (nnmail-activate 'nnml)
  (or (assoc group nnml-group-alist)
      (let (active)
	(setq nnml-group-alist (cons (list group (setq active (cons 1 0)))
				     nnml-group-alist))
	(nnml-possibly-create-directory group)
	(nnml-possibly-change-directory group)
	(let ((articles (mapcar
			 (lambda (file)
			   (string-to-int file))
			 (directory-files 
			  nnml-current-directory nil "^[0-9]+$"))))
	  (and articles
	       (progn
		 (setcar active (apply 'min articles))
		 (setcdr active (apply 'max articles)))))
	(nnmail-save-active nnml-group-alist nnml-active-file)))
  t)

(defun nnml-request-list (&optional server)
  (if server (nnml-get-new-mail))
  (save-excursion
    (nnmail-find-file nnml-active-file)
    (setq nnml-group-alist (nnmail-get-active))))

(defun nnml-request-newgroups (date &optional server)
  (nnml-request-list server))

(defun nnml-request-list-newsgroups (&optional server)
  (save-excursion
    (nnmail-find-file nnml-newsgroups-file)))

(defun nnml-request-post (&optional server)
  (mail-send-and-exit nil))

(defalias 'nnml-request-post-buffer 'nnmail-request-post-buffer)

(defun nnml-request-expire-articles (articles newsgroup &optional server force)
  (nnml-possibly-change-directory newsgroup)
  (let* ((days (or (and nnmail-expiry-wait-function
			(funcall nnmail-expiry-wait-function newsgroup))
		   nnmail-expiry-wait))
	 (active-articles 
	  (mapcar
	   (function
	    (lambda (name)
	      (string-to-int name)))
	   (directory-files nnml-current-directory nil "^[0-9]+$" t)))
	 (max-article (and active-articles (apply 'max active-articles)))
	 (is-old t)
	 article rest mod-time)
    (nnmail-activate 'nnml)

    (while (and articles is-old)
      (setq article (concat nnml-current-directory 
			    (int-to-string (car articles))))
      (if (setq mod-time (nth 5 (file-attributes article)))
	  (if (and (or (not nnmail-keep-last-article)
		       (not max-article)
		       (not (= (car articles) max-article)))
		   (or force
		       (and (not (equal mod-time '(0 0)))
			    (setq is-old
				  (> (nnmail-days-between
				      (current-time-string)
				      (current-time-string mod-time))
				     days)))))
	      (progn
		(and gnus-verbose-backends 
		     (message "Deleting article %s..." article))
		(condition-case ()
		    (delete-file article)
		  (file-error
		   (setq rest (cons (car articles) rest))))
		(setq active-articles (delq (car articles) active-articles))
		(nnml-nov-delete-article newsgroup (car articles)))
	    (setq rest (cons (car articles) rest))))
      (setq articles (cdr articles)))
    (let ((active (nth 1 (assoc newsgroup nnml-group-alist))))
      (and active
	   (setcar active (or (and active-articles
				   (apply 'min active-articles))
			      0)))
      (nnmail-save-active nnml-group-alist nnml-active-file))
    (nnml-save-nov)
    (message "")
    (nconc rest articles)))

(defun nnml-request-move-article 
  (article group server accept-form &optional last)
  (let ((buf (get-buffer-create " *nnml move*"))
	result)
    (and 
     (nnml-request-article article group server)
     (save-excursion
       (set-buffer buf)
       (insert-buffer-substring nntp-server-buffer)
       (setq result (eval accept-form))
       (kill-buffer (current-buffer))
       result)
     (progn
       (condition-case ()
	   (delete-file (concat nnml-current-directory 
				(int-to-string article)))
	 (file-error nil))
       (nnml-nov-delete-article group article)
       (and last (nnml-save-nov))))
    result))

(defun nnml-request-accept-article (group &optional last)
  (let (result)
    (if (stringp group)
	(and 
	 (nnmail-activate 'nnml)
	 ;; We trick the choosing function into believing that only one
	 ;; group is available.  
	 (let ((nnmail-split-methods (list (list group ""))))
	   (setq result (car (nnml-save-mail))))
	 (progn
	   (nnmail-save-active nnml-group-alist nnml-active-file)
	   (and last (nnml-save-nov))))
      (and
       (nnmail-activate 'nnml)
       (setq result (car (nnml-save-mail)))
       (progn
	 (nnmail-save-active nnml-group-alist nnml-active-file)
	 (and last (nnml-save-nov)))))
    result))

(defun nnml-request-replace-article (article group buffer)
  (nnml-possibly-change-directory group)
  (save-excursion
    (set-buffer buffer)
    (nnml-possibly-create-directory group)
    (if (not (condition-case ()
		 (progn
		   (write-region (point-min) (point-max)
				 (concat nnml-current-directory 
					 (int-to-string article))
				 nil (if gnus-verbose-backends nil 'nomesg))
		   t)
	       (error nil)))
	()
      (let ((chars (nnmail-insert-lines))
	    (art (concat (int-to-string article) "\t"))
	    nov-line)
	(setq nov-line (nnml-make-nov-line chars))
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
	    ;; we should insert it. (This situation should never
	    ;; occur, but one likes to make sure...)
	    (while (and (looking-at "[0-9]+\t")
			(< (string-to-int 
			    (buffer-substring 
			     (match-beginning 0) (match-end 0)))
			   article)
			(zerop (forward-line 1)))))
	  (beginning-of-line)
	  (insert (int-to-string article) nov-line)
	  (nnml-save-nov)
	  t)))))



;;; Internal functions

(defun nnml-retrieve-headers-with-nov (articles)
  (if (or gnus-nov-is-evil nnml-nov-is-evil)
      nil
    (let ((first (car articles))
	  (last (progn (while (cdr articles) (setq articles (cdr articles)))
		       (car articles)))
	  (nov (concat nnml-current-directory nnml-nov-file-name)))
      (if (file-exists-p nov)
	  (save-excursion
	    (set-buffer nntp-server-buffer)
	    (erase-buffer)
	    (insert-file-contents nov)
	    (goto-char (point-min))
	    (while (and (not (eobp)) (< first (read (current-buffer))))
	      (forward-line 1))
	    (beginning-of-line)
	    (if (not (eobp)) (delete-region 1 (point)))
	    (while (and (not (eobp)) (>= last (read (current-buffer))))
	      (forward-line 1))
	    (beginning-of-line)
	    (if (not (eobp)) (delete-region (point) (point-max)))
	    t)))))

(defun nnml-possibly-change-directory (newsgroup &optional force)
  (if newsgroup
      (let ((pathname (nnmail-article-pathname newsgroup nnml-directory)))
	(and (or force (file-directory-p pathname))
	     (setq nnml-current-directory pathname)))
    t))

(defun nnml-possibly-create-directory (group)
  (let (dir dirs)
    (setq dir (nnmail-article-pathname group nnml-directory))
    (while (not (file-directory-p dir))
      (setq dirs (cons dir dirs))
      (setq dir (file-name-directory (directory-file-name dir))))
    (while dirs
      (make-directory (directory-file-name (car dirs)))
      (and gnus-verbose-backends 
	   (message "Creating mail directory %s" (car dirs)))
      (setq dirs (cdr dirs)))))
	     
(defun nnml-save-mail ()
  "Called narrowed to an article."
  (let ((group-art (nreverse (nnmail-article-group 'nnml-active-number)))
	chars nov-line)
    (setq chars (nnmail-insert-lines))
    (nnmail-insert-xref group-art)
    (run-hooks 'nnml-prepare-save-mail-hook)
    (goto-char (point-min))
    (while (looking-at "From ")
      (replace-match "X-From-Line: ")
      (forward-line 1))
    ;; We save the article in all the newsgroups it belongs in.
    (let ((ga group-art)
	  first)
      (while ga
	(nnml-possibly-create-directory (car (car ga)))
	(let ((file (concat (nnmail-article-pathname 
			     (car (car ga)) nnml-directory)
			    (int-to-string (cdr (car ga))))))
	  (if first
	      ;; It was already saved, so we just make a hard link.
	      (add-name-to-file first file t)
	    ;; Save the article.
	    (write-region (point-min) (point-max) file nil 
			  (if gnus-verbose-backends nil 'nomesg))
	    (setq first file)))
	(setq ga (cdr ga))))
    ;; Generate a nov line for this article. We generate the nov
    ;; line after saving, because nov generation destroys the
    ;; header. 
    (setq nov-line (nnml-make-nov-line chars))
    ;; Output the nov line to all nov databases that should have it.
    (let ((ga group-art))
      (while ga
	(nnml-add-nov (car (car ga)) (cdr (car ga)) nov-line)
	(setq ga (cdr ga))))
    group-art))

(defun nnml-active-number (group)
  "Compute the next article number in GROUP."
  (let ((active (car (cdr (assoc group nnml-group-alist)))))
    ;; The group wasn't known to nnml, so we just create an active
    ;; entry for it.   
    (or active
	(progn
	  (setq active (cons 1 0))
	  (setq nnml-group-alist (cons (list group active) nnml-group-alist))))
    (setcdr active (1+ (cdr active)))
    (while (file-exists-p
	    (concat (nnmail-article-pathname group nnml-directory)
		    (int-to-string (cdr active))))
      (setcdr active (1+ (cdr active))))
    (cdr active)))

(defun nnml-get-new-mail (&optional group)
  "Read new incoming mail."
  (let* ((spools (nnmail-get-spool-files group))
	 (group-in group)
	 incoming incomings)
    (if (or (not nnml-get-new-mail) (not nnmail-spool-file))
	()
      ;; We first activate all the groups.
      (nnmail-activate 'nnml)
      ;; The we go through all the existing spool files and split the
      ;; mail from each.
      (while spools
	(and
	 (file-exists-p (car spools))
	 (> (nth 7 (file-attributes (car spools))) 0)
	 (progn
	   (and gnus-verbose-backends 
		(message "nnml: Reading incoming mail..."))
	   (if (not (setq incoming 
			  (nnmail-move-inbox 
			   (car spools) (concat nnml-directory "Incoming"))))
	       ()
	     (setq group (nnmail-get-split-group (car spools) group-in))
	     (nnmail-split-incoming incoming 'nnml-save-mail nil group)
	     (setq incomings (cons incoming incomings)))))
	(setq spools (cdr spools)))
      ;; If we did indeed read any incoming spools, we save all info. 
      (if incoming 
	  (progn
	    (nnmail-save-active nnml-group-alist nnml-active-file)
	    (nnml-save-nov)
	    (run-hooks 'nnmail-read-incoming-hook)
	    (and gnus-verbose-backends
		 (message "nnml: Reading incoming mail...done"))))
      (while incomings
	(setq incoming (car incomings))
	(and nnmail-delete-incoming
	     (file-exists-p incoming)
	     (file-writable-p incoming)
	     (delete-file incoming))
	(setq incomings (cdr incomings))))))


(defun nnml-add-nov (group article line)
  "Add a nov line for the GROUP base."
  (save-excursion 
    (set-buffer (nnml-open-nov group))
    (goto-char (point-max))
    (insert (int-to-string article) line)))

(defsubst nnml-header-value ()
  (buffer-substring (match-end 0) (save-excursion (end-of-line) (point))))

(defun nnml-make-nov-line (chars)
  "Create a nov from the current headers."
  (let ((case-fold-search t)
	subject from date id references lines xref in-reply-to char)
    (save-excursion
      (save-restriction
	(goto-char (point-min))
	(narrow-to-region 
	 (point)
	 (1- (or (search-forward "\n\n" nil t) (point-max))))
	;; Fold continuation lines.
	(goto-char (point-min))
	(while (re-search-forward "\\(\r?\n[ \t]+\\)+" nil t)
	  (replace-match " " t t))
	(subst-char-in-region (point-min) (point-max) ?\t ? )
	;; [number subject from date id references chars lines xref]
	(save-excursion
	  (goto-char (point-min))
	  (while (re-search-forward "^\\(from\\|subject\\|message-id\\|date\\|lines\\|xref\\|references\\|in-reply-to\\): "
				    nil t)
	    (beginning-of-line)
	    (setq char (downcase (following-char))) 
	    (cond
	     ((eq char ?s)
	      (setq subject (nnml-header-value)))
	     ((eq char ?f)
	      (setq from (nnml-header-value)))
	     ((eq char ?x)
	      (setq xref (nnml-header-value)))
	     ((eq char ?l)
	      (setq lines (nnml-header-value)))
	     ((eq char ?d)
	      (setq date (nnml-header-value)))
	     ((eq char ?m)
	      (setq id (setq id (nnml-header-value))))
	     ((eq char ?r)
	      (setq references (nnml-header-value)))
	     ((eq char ?i)
	      (setq in-reply-to (nnml-header-value))))
	    (forward-line 1))
      
	  (and (not references)
	       in-reply-to
	       (string-match "<[^>]+>" in-reply-to)
	       (setq references
		     (substring in-reply-to (match-beginning 0)
				(match-end 0)))))
	;; [number subject from date id references chars lines xref]
	(format "\t%s\t%s\t%s\t%s\t%s\t%d\t%s\t%s\t\n"
		(or subject "(none)")
		(or from "(nobody)") (or date "")
		(or id (concat "nnml-dummy-id-" 
			       (mapconcat 
				(lambda (time) (int-to-string time))
				(current-time) "-")))
		(or references "")
		(or chars 0) (or lines "0") (or xref ""))))))

(defun nnml-open-nov (group)
  (or (cdr (assoc group nnml-nov-buffer-alist))
      (let ((buffer (find-file-noselect 
		     (concat (nnmail-article-pathname 
			      group nnml-directory) nnml-nov-file-name))))
	(save-excursion
	  (set-buffer buffer)
	  (buffer-disable-undo (current-buffer)))
	(setq nnml-nov-buffer-alist 
	      (cons (cons group buffer) nnml-nov-buffer-alist))
	buffer)))

(defun nnml-save-nov ()
  (save-excursion
    (while nnml-nov-buffer-alist
      (if (buffer-name (cdr (car nnml-nov-buffer-alist)))
	  (progn
	    (set-buffer (cdr (car nnml-nov-buffer-alist)))
	    (and (buffer-modified-p)
		 (write-region 
		  1 (point-max) (buffer-file-name) nil 'nomesg))
	    (set-buffer-modified-p nil)
	    (kill-buffer (current-buffer))))
      (setq nnml-nov-buffer-alist (cdr nnml-nov-buffer-alist)))))

;;;###autoload
(defun nnml-generate-nov-databases (dir)
  "Generate nov databases in all nnml mail newsgroups."
  (interactive 
   (progn   
     (setq nnml-group-alist nil)
     (list nnml-directory)))
  (nnml-open-server (or nnml-current-server ""))
  (let ((dirs (directory-files dir t nil t)))
    (while dirs 
      (if (and (not (string-match "/\\.\\.$" (car dirs)))
	       (not (string-match "/\\.$" (car dirs)))
	       (file-directory-p (car dirs)))
	  (nnml-generate-nov-databases (car dirs)))
      (setq dirs (cdr dirs))))
  (let ((files (sort
		(mapcar
		 (function
		  (lambda (name)
		    (string-to-int name)))
		 (directory-files dir nil "^[0-9]+$" t))
		(function <)))
	(nov (concat dir "/" nnml-nov-file-name))
	(nov-buffer (get-buffer-create "*nov*"))
	nov-line chars)
    (if files
	(setq nnml-group-alist 
	      (cons (list (nnmail-replace-chars-in-string 
			   (substring (expand-file-name dir)
				      (length (expand-file-name 
					       nnml-directory)))
			   ?/ ?.)
			  (cons (car files)
				(let ((f files))
				  (while (cdr f) (setq f (cdr f)))
				  (car f))))
		    nnml-group-alist)))
    (if files
	(save-excursion
	  (set-buffer nntp-server-buffer)
	  (if (file-exists-p nov)
	      (delete-file nov))
	  (save-excursion
	    (set-buffer nov-buffer)
	    (buffer-disable-undo (current-buffer))
	    (erase-buffer))
	  (while files
	    (erase-buffer)
	    (insert-file-contents (concat dir "/" (int-to-string (car files))))
	    (goto-char (point-min))
	    (narrow-to-region 1 (save-excursion (search-forward "\n\n" nil t)
						(setq chars (- (point-max) 
							       (point)))
						(point)))
 	    (if (not (= 0 chars))	; none of them empty files...
 		(progn
		  (setq nov-line (nnml-make-nov-line chars))
		  (save-excursion
		    (set-buffer nov-buffer)
		    (goto-char (point-max))
		    (insert (int-to-string (car files)) nov-line))))
	    (widen)
	    (setq files (cdr files)))
	  (save-excursion
	    (set-buffer nov-buffer)
	    (write-region 1 (point-max) (expand-file-name nov) nil
			  'nomesg)
	    (kill-buffer (current-buffer)))))
    (nnmail-save-active nnml-group-alist nnml-active-file)))

(defun nnml-nov-delete-article (group article)
  (save-excursion
    (set-buffer (nnml-open-nov group))
    (goto-char (point-min))
    (if (re-search-forward (concat "^" (int-to-string article) "\t") nil t)
	(delete-region (match-beginning 0) (progn (forward-line 1) (point))))
    t))

(provide 'nnml)

;;; nnml.el ends here
