;;; nnmbox.el --- mail mbox access for Gnus

;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000
;;	Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; 	Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;; Keywords: news, mail

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; For an overview of what the interface functions do, please see the
;; Gnus sources.

;;; Code:

(require 'nnheader)
(require 'message)
(require 'nnmail)
(require 'nnoo)
(eval-when-compile (require 'cl))

(nnoo-declare nnmbox)

(defvoo nnmbox-mbox-file (expand-file-name "~/mbox")
  "The name of the mail box file in the user's home directory.")

(defvoo nnmbox-active-file (expand-file-name "~/.mbox-active")
  "The name of the active file for the mail box.")

(defvoo nnmbox-get-new-mail t
  "If non-nil, nnmbox will check the incoming mail file and split the mail.")

(defvoo nnmbox-prepare-save-mail-hook nil
  "Hook run narrowed to an article before saving.")



(defconst nnmbox-version "nnmbox 1.0"
  "nnmbox version.")

(defvoo nnmbox-current-group nil
  "Current nnmbox news group directory.")

(defconst nnmbox-mbox-buffer nil)

(defvoo nnmbox-status-string "")

(defvoo nnmbox-group-alist nil)
(defvoo nnmbox-active-timestamp nil)

(defvoo nnmbox-file-coding-system mm-binary-coding-system)
(defvoo nnmbox-file-coding-system-for-write nil)
(defvoo nnmbox-active-file-coding-system mm-binary-coding-system)
(defvoo nnmbox-active-file-coding-system-for-write nil)



;;; Interface functions

(nnoo-define-basics nnmbox)

(deffoo nnmbox-retrieve-headers (sequence &optional newsgroup server fetch-old)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (let ((number (length sequence))
	  (count 0)
	  article art-string start stop)
      (nnmbox-possibly-change-newsgroup newsgroup server)
      (while sequence
	(setq article (car sequence))
	(setq art-string (nnmbox-article-string article))
	(set-buffer nnmbox-mbox-buffer)
	(when (or (search-forward art-string nil t)
		  (progn (goto-char (point-min))
			 (search-forward art-string nil t)))
	  (setq start
		(save-excursion
		  (re-search-backward
		   (concat "^" message-unix-mail-delimiter) nil t)
		  (point)))
	  (search-forward "\n\n" nil t)
	  (setq stop (1- (point)))
	  (set-buffer nntp-server-buffer)
	  (insert (format "221 %d Article retrieved.\n" article))
	  (insert-buffer-substring nnmbox-mbox-buffer start stop)
	  (goto-char (point-max))
	  (insert ".\n"))
	(setq sequence (cdr sequence))
	(setq count (1+ count))
	(and (numberp nnmail-large-newsgroup)
	     (> number nnmail-large-newsgroup)
	     (zerop (% count 20))
	     (nnheader-message 5 "nnmbox: Receiving headers... %d%%"
			       (/ (* count 100) number))))

      (and (numberp nnmail-large-newsgroup)
	   (> number nnmail-large-newsgroup)
	   (nnheader-message 5 "nnmbox: Receiving headers...done"))

      (set-buffer nntp-server-buffer)
      (nnheader-fold-continuation-lines)
      'headers)))

(deffoo nnmbox-open-server (server &optional defs)
  (nnoo-change-server 'nnmbox server defs)
  (nnmbox-create-mbox)
  (cond
   ((not (file-exists-p nnmbox-mbox-file))
    (nnmbox-close-server)
    (nnheader-report 'nnmbox "No such file: %s" nnmbox-mbox-file))
   ((file-directory-p nnmbox-mbox-file)
    (nnmbox-close-server)
    (nnheader-report 'nnmbox "Not a regular file: %s" nnmbox-mbox-file))
   (t
    (nnheader-report 'nnmbox "Opened server %s using mbox %s" server
		     nnmbox-mbox-file)
    t)))

(deffoo nnmbox-close-server (&optional server)
  (when (and nnmbox-mbox-buffer
	     (buffer-name nnmbox-mbox-buffer))
    (kill-buffer nnmbox-mbox-buffer))
  (nnoo-close-server 'nnmbox server)
  t)

(deffoo nnmbox-server-opened (&optional server)
  (and (nnoo-current-server-p 'nnmbox server)
       nnmbox-mbox-buffer
       (buffer-name nnmbox-mbox-buffer)
       nntp-server-buffer
       (buffer-name nntp-server-buffer)))

(deffoo nnmbox-request-article (article &optional newsgroup server buffer)
  (nnmbox-possibly-change-newsgroup newsgroup server)
  (save-excursion
    (set-buffer nnmbox-mbox-buffer)
    (goto-char (point-min))
    (when (search-forward (nnmbox-article-string article) nil t)
      (let (start stop)
	(re-search-backward (concat "^" message-unix-mail-delimiter) nil t)
	(setq start (point))
	(forward-line 1)
	(or (and (re-search-forward
		  (concat "^" message-unix-mail-delimiter) nil t)
		 (forward-line -1))
	    (goto-char (point-max)))
	(setq stop (point))
	(let ((nntp-server-buffer (or buffer nntp-server-buffer)))
	  (set-buffer nntp-server-buffer)
	  (erase-buffer)
	  (insert-buffer-substring nnmbox-mbox-buffer start stop)
	  (goto-char (point-min))
	  (while (looking-at "From ")
	    (delete-char 5)
	    (insert "X-From-Line: ")
	    (forward-line 1))
	  (if (numberp article)
	      (cons nnmbox-current-group article)
	    (nnmbox-article-group-number)))))))

(deffoo nnmbox-request-group (group &optional server dont-check)
  (nnmbox-possibly-change-newsgroup nil server)
  (let ((active (cadr (assoc group nnmbox-group-alist))))
    (cond
     ((or (null active)
	  (null (nnmbox-possibly-change-newsgroup group server)))
      (nnheader-report 'nnmbox "No such group: %s" group))
     (dont-check
      (nnheader-report 'nnmbox "Selected group %s" group)
      (nnheader-insert ""))
     (t
      (nnheader-report 'nnmbox "Selected group %s" group)
      (nnheader-insert "211 %d %d %d %s\n"
		       (1+ (- (cdr active) (car active)))
		       (car active) (cdr active) group)))))

(defun nnmbox-save-buffer ()
  (let ((coding-system-for-write 
	 (or nnmbox-file-coding-system-for-write
	     nnmbox-file-coding-system)))
    (save-buffer)))

(defun nnmbox-save-active (group-alist active-file)
  (let ((nnmail-active-file-coding-system
	 (or nnmbox-active-file-coding-system-for-write
	     nnmbox-active-file-coding-system)))
    (nnmail-save-active group-alist active-file)))

(deffoo nnmbox-request-scan (&optional group server)
  (nnmbox-possibly-change-newsgroup group server)
  (nnmbox-read-mbox)
  (nnmail-get-new-mail
   'nnmbox
   (lambda ()
     (save-excursion
       (set-buffer nnmbox-mbox-buffer)
       (nnmbox-save-buffer)))
   (file-name-directory nnmbox-mbox-file)
   group
   (lambda ()
     (save-excursion
       (let ((in-buf (current-buffer)))
	 (set-buffer nnmbox-mbox-buffer)
	 (goto-char (point-max))
	 (insert-buffer-substring in-buf)))
     (nnmbox-save-active nnmbox-group-alist nnmbox-active-file))))

(deffoo nnmbox-close-group (group &optional server)
  t)

(deffoo nnmbox-request-create-group (group &optional server args)
  (nnmail-activate 'nnmbox)
  (unless (assoc group nnmbox-group-alist)
    (push (list group (cons 1 0))
	  nnmbox-group-alist)
    (nnmbox-save-active nnmbox-group-alist nnmbox-active-file))
  t)

(deffoo nnmbox-request-list (&optional server)
  (save-excursion
    (let ((nnmail-file-coding-system
	   nnmbox-active-file-coding-system))
      (nnmail-find-file nnmbox-active-file))
    (setq nnmbox-group-alist (nnmail-get-active))
    t))

(deffoo nnmbox-request-newgroups (date &optional server)
  (nnmbox-request-list server))

(deffoo nnmbox-request-list-newsgroups (&optional server)
  (nnheader-report 'nnmbox "LIST NEWSGROUPS is not implemented."))

(deffoo nnmbox-request-expire-articles
    (articles newsgroup &optional server force)
  (nnmbox-possibly-change-newsgroup newsgroup server)
  (let* ((is-old t)
	 rest)
    (nnmail-activate 'nnmbox)

    (save-excursion
      (set-buffer nnmbox-mbox-buffer)
      (while (and articles is-old)
	(goto-char (point-min))
	(when (search-forward (nnmbox-article-string (car articles)) nil t)
	  (if (setq is-old
		    (nnmail-expired-article-p
		     newsgroup
		     (buffer-substring
		      (point) (progn (end-of-line) (point))) force))
	      (progn
		(nnheader-message 5 "Deleting article %d in %s..."
				  (car articles) newsgroup)
		(nnmbox-delete-mail))
	    (push (car articles) rest)))
	(setq articles (cdr articles)))
      (nnmbox-save-buffer)
      ;; Find the lowest active article in this group.
      (let ((active (nth 1 (assoc newsgroup nnmbox-group-alist))))
	(goto-char (point-min))
	(while (and (not (search-forward
			  (nnmbox-article-string (car active)) nil t))
		    (<= (car active) (cdr active)))
	  (setcar active (1+ (car active)))
	  (goto-char (point-min))))
      (nnmbox-save-active nnmbox-group-alist nnmbox-active-file)
      (nconc rest articles))))

(deffoo nnmbox-request-move-article
    (article group server accept-form &optional last)
  (let ((buf (get-buffer-create " *nnmbox move*"))
	result)
    (and
     (nnmbox-request-article article group server)
     (save-excursion
       (set-buffer buf)
       (erase-buffer)
       (insert-buffer-substring nntp-server-buffer)
       (goto-char (point-min))
       (while (re-search-forward
	       "^X-Gnus-Newsgroup:"
	       (save-excursion (search-forward "\n\n" nil t) (point)) t)
	 (delete-region (progn (beginning-of-line) (point))
			(progn (forward-line 1) (point))))
       (setq result (eval accept-form))
       (kill-buffer buf)
       result)
     (save-excursion
       (nnmbox-possibly-change-newsgroup group server)
       (set-buffer nnmbox-mbox-buffer)
       (goto-char (point-min))
       (when (search-forward (nnmbox-article-string article) nil t)
	 (nnmbox-delete-mail))
       (and last (nnmbox-save-buffer))))
    result))

(deffoo nnmbox-request-accept-article (group &optional server last)
  (nnmbox-possibly-change-newsgroup group server)
  (nnmail-check-syntax)
  (let ((buf (current-buffer))
	result)
    (goto-char (point-min))
    ;; The From line may have been quoted by movemail.
    (when (looking-at (concat ">" message-unix-mail-delimiter))
      (delete-char 1))
    (if (looking-at "X-From-Line: ")
	(replace-match "From ")
      (insert "From nobody " (current-time-string) "\n"))
    (and
     (nnmail-activate 'nnmbox)
     (progn
       (set-buffer buf)
       (goto-char (point-min))
       (search-forward "\n\n" nil t)
       (forward-line -1)
       (while (re-search-backward "^X-Gnus-Newsgroup: " nil t)
	 (delete-region (point) (progn (forward-line 1) (point))))
       (when nnmail-cache-accepted-message-ids
	 (nnmail-cache-insert (nnmail-fetch-field "message-id")))
       (setq result (if (stringp group)
			(list (cons group (nnmbox-active-number group)))
		      (nnmail-article-group 'nnmbox-active-number)))
       (if (and (null result)
		(yes-or-no-p "Moved to `junk' group; delete article? "))
	   (setq result 'junk)
	 (setq result (car (nnmbox-save-mail result)))))
     (save-excursion
       (set-buffer nnmbox-mbox-buffer)
       (goto-char (point-max))
       (insert-buffer-substring buf)
       (when last
	 (when nnmail-cache-accepted-message-ids
	   (nnmail-cache-close))
	 (nnmbox-save-active nnmbox-group-alist nnmbox-active-file)
	 (nnmbox-save-buffer))))
    result))

(deffoo nnmbox-request-replace-article (article group buffer)
  (nnmbox-possibly-change-newsgroup group)
  (save-excursion
    (set-buffer nnmbox-mbox-buffer)
    (goto-char (point-min))
    (if (not (search-forward (nnmbox-article-string article) nil t))
	nil
      (nnmbox-delete-mail t t)
      (insert-buffer-substring buffer)
      (nnmbox-save-buffer)
      t)))

(deffoo nnmbox-request-delete-group (group &optional force server)
  (nnmbox-possibly-change-newsgroup group server)
  ;; Delete all articles in GROUP.
  (if (not force)
      ()				; Don't delete the articles.
    (save-excursion
      (set-buffer nnmbox-mbox-buffer)
      (goto-char (point-min))
      ;; Delete all articles in this group.
      (let ((ident (concat "\nX-Gnus-Newsgroup: " nnmbox-current-group ":"))
	    found)
	(while (search-forward ident nil t)
	  (setq found t)
	  (nnmbox-delete-mail))
	(when found
	  (nnmbox-save-buffer)))))
  ;; Remove the group from all structures.
  (setq nnmbox-group-alist
	(delq (assoc group nnmbox-group-alist) nnmbox-group-alist)
	nnmbox-current-group nil)
  ;; Save the active file.
  (nnmbox-save-active nnmbox-group-alist nnmbox-active-file)
  t)

(deffoo nnmbox-request-rename-group (group new-name &optional server)
  (nnmbox-possibly-change-newsgroup group server)
  (save-excursion
    (set-buffer nnmbox-mbox-buffer)
    (goto-char (point-min))
    (let ((ident (concat "\nX-Gnus-Newsgroup: " nnmbox-current-group ":"))
	  (new-ident (concat "\nX-Gnus-Newsgroup: " new-name ":"))
	  found)
      (while (search-forward ident nil t)
	(replace-match new-ident t t)
	(setq found t))
      (when found
	(nnmbox-save-buffer))))
  (let ((entry (assoc group nnmbox-group-alist)))
    (when entry
      (setcar entry new-name))
    (setq nnmbox-current-group nil)
    ;; Save the new group alist.
    (nnmbox-save-active nnmbox-group-alist nnmbox-active-file)
    t))


;;; Internal functions.

;; If FORCE, delete article no matter how many X-Gnus-Newsgroup
;; headers there are.  If LEAVE-DELIM, don't delete the Unix mbox
;; delimiter line.
(defun nnmbox-delete-mail (&optional force leave-delim)
  ;; Delete the current X-Gnus-Newsgroup line.
  (or force
      (delete-region
       (progn (beginning-of-line) (point))
       (progn (forward-line 1) (point))))
  ;; Beginning of the article.
  (save-excursion
    (save-restriction
      (narrow-to-region
       (save-excursion
	 (re-search-backward (concat "^" message-unix-mail-delimiter) nil t)
	 (if leave-delim (progn (forward-line 1) (point))
	   (match-beginning 0)))
       (progn
	 (forward-line 1)
	 (or (and (re-search-forward (concat "^" message-unix-mail-delimiter)
				     nil t)
		  (if (and (not (bobp)) leave-delim)
		      (progn (forward-line -2) (point))
		    (match-beginning 0)))
	     (point-max))))
      (goto-char (point-min))
      ;; Only delete the article if no other groups owns it as well.
      (when (or force (not (re-search-forward "^X-Gnus-Newsgroup: " nil t)))
	(delete-region (point-min) (point-max))))))

(defun nnmbox-possibly-change-newsgroup (newsgroup &optional server)
  (when (and server
	     (not (nnmbox-server-opened server)))
    (nnmbox-open-server server))
  (when (or (not nnmbox-mbox-buffer)
	    (not (buffer-name nnmbox-mbox-buffer)))
    (save-excursion
      (set-buffer (setq nnmbox-mbox-buffer
			(let ((nnheader-file-coding-system
			       nnmbox-file-coding-system))
			  (nnheader-find-file-noselect
			   nnmbox-mbox-file nil t))))
      (mm-enable-multibyte)
      (buffer-disable-undo)))
  (when (not nnmbox-group-alist)
    (nnmail-activate 'nnmbox))
  (if newsgroup
      (when (assoc newsgroup nnmbox-group-alist)
	(setq nnmbox-current-group newsgroup))
    t))

(defun nnmbox-article-string (article)
  (if (numberp article)
      (concat "\nX-Gnus-Newsgroup: " nnmbox-current-group ":"
	      (int-to-string article) " ")
    (concat "\nMessage-ID: " article)))

(defun nnmbox-article-group-number ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^X-Gnus-Newsgroup: +\\([^:]+\\):\\([0-9]+\\) "
			     nil t)
      (cons (buffer-substring (match-beginning 1) (match-end 1))
	    (string-to-int
	     (buffer-substring (match-beginning 2) (match-end 2)))))))

(defun nnmbox-save-mail (group-art)
  "Called narrowed to an article."
  (let ((delim (concat "^" message-unix-mail-delimiter)))
    (goto-char (point-min))
    ;; This might come from somewhere else.
    (unless (looking-at delim)
      (insert "From nobody " (current-time-string) "\n")
      (goto-char (point-min)))
    ;; Quote all "From " lines in the article.
    (forward-line 1)
    (while (re-search-forward delim nil t)
      (beginning-of-line)
      (insert "> "))
    (nnmail-insert-lines)
    (nnmail-insert-xref group-art)
    (nnmbox-insert-newsgroup-line group-art)
    (run-hooks 'nnmail-prepare-save-mail-hook)
    (run-hooks 'nnmbox-prepare-save-mail-hook)
    group-art))

(defun nnmbox-insert-newsgroup-line (group-art)
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "\n\n" nil t)
      (forward-char -1)
      (while group-art
	(insert (format "X-Gnus-Newsgroup: %s:%d   %s\n"
			(caar group-art) (cdar group-art)
			(current-time-string)))
	(setq group-art (cdr group-art))))
    t))

(defun nnmbox-active-number (group)
  ;; Find the next article number in GROUP.
  (let ((active (cadr (assoc group nnmbox-group-alist))))
    (if active
	(setcdr active (1+ (cdr active)))
      ;; This group is new, so we create a new entry for it.
      ;; This might be a bit naughty... creating groups on the drop of
      ;; a hat, but I don't know...
      (push (list group (setq active (cons 1 1)))
	    nnmbox-group-alist))
    (cdr active)))

(defun nnmbox-create-mbox ()
  (when (not (file-exists-p nnmbox-mbox-file))
    (let ((nnmail-file-coding-system
	   (or nnmbox-file-coding-system-for-write
	       nnmbox-file-coding-system)))
      (nnmail-write-region (point-min) (point-min)
			   nnmbox-mbox-file t 'nomesg))))

(defun nnmbox-read-mbox ()
  (nnmail-activate 'nnmbox)
  (nnmbox-create-mbox)
  (if (and nnmbox-mbox-buffer
	   (buffer-name nnmbox-mbox-buffer)
	   (save-excursion
	     (set-buffer nnmbox-mbox-buffer)
	     (= (buffer-size) (nnheader-file-size nnmbox-mbox-file))))
      ()
    (save-excursion
      (let ((delim (concat "^" message-unix-mail-delimiter))
	    (alist nnmbox-group-alist)
	    start end number)
	(set-buffer (setq nnmbox-mbox-buffer
			  (let ((nnheader-file-coding-system
				 nnmbox-file-coding-system))
			    (nnheader-find-file-noselect
			     nnmbox-mbox-file nil t))))
	(mm-enable-multibyte)
	(buffer-disable-undo)

	;; Go through the group alist and compare against
	;; the mbox file.
	(while alist
	  (goto-char (point-max))
	  (when (and (re-search-backward
		      (format "^X-Gnus-Newsgroup: %s:\\([0-9]+\\) "
			      (caar alist)) nil t)
		     (> (setq number
			      (string-to-number
			       (buffer-substring
				(match-beginning 1) (match-end 1))))
			(cdadar alist)))
	    (setcdr (cadar alist) number))
	  (setq alist (cdr alist)))

	(goto-char (point-min))
	(while (re-search-forward delim nil t)
	  (setq start (match-beginning 0))
	  (unless (search-forward
		   "\nX-Gnus-Newsgroup: "
		   (save-excursion
		     (setq end
			   (or
			    (and
			     ;; skip to end of headers first, since mail
			     ;; which has been respooled has additional
			     ;; "From nobody" lines.
			     (search-forward "\n\n" nil t)
			     (re-search-forward delim nil t)
			     (match-beginning 0))
			    (point-max))))
		   t)
	    (save-excursion
	      (save-restriction
		(narrow-to-region start end)
		(nnmbox-save-mail
		 (nnmail-article-group 'nnmbox-active-number)))))
	  (goto-char end))))))

(provide 'nnmbox)

;;; nnmbox.el ends here
