;;; nnfolder.el --- mail folder access for Gnus
;; Copyright (C) 1995,96 Free Software Foundation, Inc.

;; Author: Scott Byer <byer@mv.us.adobe.com>
;;	Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; 	Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;; Keywords: mail

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

;; For an overview of what the interface functions do, please see the
;; Gnus sources.  

;; Various enhancements by byer@mv.us.adobe.com (Scott Byer).

;;; Code:

(require 'nnheader)
(require 'message)
(require 'nnmail)
(require 'nnoo)
(eval-when-compile (require 'cl))

(nnoo-declare nnfolder)

(defvoo nnfolder-directory (expand-file-name message-directory)
  "The name of the nnfolder directory.")

(defvoo nnfolder-active-file 
  (nnheader-concat nnfolder-directory "active")
  "The name of the active file.")

;; I renamed this variable to something more in keeping with the general GNU
;; style. -SLB

(defvoo nnfolder-ignore-active-file nil
  "If non-nil, causes nnfolder to do some extra work in order to determine
the true active ranges of an mbox file.  Note that the active file is still
saved, but it's values are not used.  This costs some extra time when 
scanning an mbox when opening it.")

(defvoo nnfolder-distrust-mbox nil
  "If non-nil, causes nnfolder to not trust the user with respect to
inserting unaccounted for mail in the middle of an mbox file.  This can greatly
slow down scans, which now must scan the entire file for unmarked messages.
When nil, scans occur forward from the last marked message, a huge
time saver for large mailboxes.")

(defvoo nnfolder-newsgroups-file 
  (concat (file-name-as-directory nnfolder-directory) "newsgroups")
  "Mail newsgroups description file.")

(defvoo nnfolder-get-new-mail t
  "If non-nil, nnfolder will check the incoming mail file and split the mail.")

(defvoo nnfolder-prepare-save-mail-hook nil
  "Hook run narrowed to an article before saving.")

(defvoo nnfolder-save-buffer-hook nil
  "Hook run before saving the nnfolder mbox buffer.")

(defvoo nnfolder-inhibit-expiry nil
  "If non-nil, inhibit expiry.")



(defconst nnfolder-version "nnfolder 1.0"
  "nnfolder version.")

(defconst nnfolder-article-marker "X-Gnus-Article-Number: "
  "String used to demarcate what the article number for a message is.")

(defvoo nnfolder-current-group nil)
(defvoo nnfolder-current-buffer nil)
(defvoo nnfolder-status-string "")
(defvoo nnfolder-group-alist nil)
(defvoo nnfolder-buffer-alist nil)
(defvoo nnfolder-scantime-alist nil)



;;; Interface functions

(nnoo-define-basics nnfolder)

(deffoo nnfolder-retrieve-headers (articles &optional group server fetch-old)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (let ((delim-string (concat "^" message-unix-mail-delimiter))
	  article art-string start stop)
      (nnfolder-possibly-change-group group server)
      (when nnfolder-current-buffer
	(set-buffer nnfolder-current-buffer)
	(goto-char (point-min))
	(if (stringp (car articles))
	    'headers
	  (while articles
	    (setq article (car articles))
	    (setq art-string (nnfolder-article-string article))
	    (set-buffer nnfolder-current-buffer)
	    (if (or (search-forward art-string nil t)
		    ;; Don't search the whole file twice!  Also, articles
		    ;; probably have some locality by number, so searching
		    ;; backwards will be faster.  Especially if we're at the
		    ;; beginning of the buffer :-). -SLB
		    (search-backward art-string nil t))
		(progn
		  (setq start (or (re-search-backward delim-string nil t)
				  (point)))
		  (search-forward "\n\n" nil t)
		  (setq stop (1- (point)))
		  (set-buffer nntp-server-buffer)
		  (insert (format "221 %d Article retrieved.\n" article))
		  (insert-buffer-substring nnfolder-current-buffer start stop)
		  (goto-char (point-max))
		  (insert ".\n")))
	    (setq articles (cdr articles)))

	  (set-buffer nntp-server-buffer)
	  (nnheader-fold-continuation-lines)
	  'headers)))))

(deffoo nnfolder-open-server (server &optional defs)
  (nnoo-change-server 'nnfolder server defs)
  (when (not (file-exists-p nnfolder-directory))
    (condition-case ()
	(make-directory nnfolder-directory t)
      (error t)))
  (cond 
   ((not (file-exists-p nnfolder-directory))
    (nnfolder-close-server)
    (nnheader-report 'nnfolder "Couldn't create directory: %s"
		     nnfolder-directory))
   ((not (file-directory-p (file-truename nnfolder-directory)))
    (nnfolder-close-server)
    (nnheader-report 'nnfolder "Not a directory: %s" nnfolder-directory))
   (t
    (nnheader-report 'nnfolder "Opened server %s using directory %s"
		     server nnfolder-directory)
    t)))

(deffoo nnfolder-request-close ()
  (let ((alist nnfolder-buffer-alist))
    (while alist
      (nnfolder-close-group (caar alist) nil t)
      (setq alist (cdr alist))))
  (nnoo-close-server 'nnfolder)
  (setq nnfolder-buffer-alist nil
	nnfolder-group-alist nil))

(deffoo nnfolder-request-article (article &optional group server buffer)
  (nnfolder-possibly-change-group group server)
  (save-excursion
    (set-buffer nnfolder-current-buffer)
    (goto-char (point-min))
    (if (search-forward (nnfolder-article-string article) nil t)
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
	    (insert-buffer-substring nnfolder-current-buffer start stop)
	    (goto-char (point-min))
	    (while (looking-at "From ")
	      (delete-char 5)
	      (insert "X-From-Line: ")
	      (forward-line 1))
	    (if (numberp article) 
		(cons nnfolder-current-group article)
	      (goto-char (point-min))
	      (search-forward (concat "\n" nnfolder-article-marker))
	      (cons nnfolder-current-group
		    (string-to-int 
		     (buffer-substring 
		      (point) (progn (end-of-line) (point)))))))))))

(deffoo nnfolder-request-group (group &optional server dont-check)
  (save-excursion
    (nnmail-activate 'nnfolder)
    (if (not (assoc group nnfolder-group-alist))
	(nnheader-report 'nnfolder "No such group: %s" group)
      (nnfolder-possibly-change-group group server)
      (if dont-check
	  (progn 
	    (nnheader-report 'nnfolder "Selected group %s" group)
	    t)
	(let* ((active (assoc group nnfolder-group-alist))
	       (group (car active))
	       (range (cadr active)))
	  (cond 
	   ((null active)
	    (nnheader-report 'nnfolder "No such group: %s" group))
	   ((null nnfolder-current-group)
	    (nnheader-report 'nnfolder "Empty group: %s" group))
	   (t
	    (nnheader-report 'nnfolder "Selected group %s" group)
	    (nnheader-insert "211 %d %d %d %s\n" 
			     (1+ (- (cdr range) (car range)))
			     (car range) (cdr range) group))))))))

(deffoo nnfolder-request-scan (&optional group server)
  (nnfolder-possibly-change-group group server t)
  (nnmail-get-new-mail
   'nnfolder 
   (lambda ()
     (let ((bufs nnfolder-buffer-alist))
       (save-excursion
	 (while bufs
	   (if (not (buffer-name (nth 1 (car bufs))))
	       (setq nnfolder-buffer-alist 
		     (delq (car bufs) nnfolder-buffer-alist))
	     (set-buffer (nth 1 (car bufs)))
	     (nnfolder-save-buffer)
	     (kill-buffer (current-buffer)))
	   (setq bufs (cdr bufs))))))
   nnfolder-directory
   group))

;; Don't close the buffer if we're not shutting down the server.  This way,
;; we can keep the buffer in the group buffer cache, and not have to grovel
;; over the buffer again unless we add new mail to it or modify it in some
;; way.

(deffoo nnfolder-close-group (group &optional server force)
  ;; Make sure we _had_ the group open.
  (when (or (assoc group nnfolder-buffer-alist)
	    (equal group nnfolder-current-group))
    (let ((inf (assoc group nnfolder-buffer-alist)))
      (when inf
	(when nnfolder-current-group
	  (push (list nnfolder-current-group nnfolder-current-buffer)
		nnfolder-buffer-alist))
	(setq nnfolder-buffer-alist
	      (delq inf nnfolder-buffer-alist))
	(setq nnfolder-current-buffer (cadr inf)
	      nnfolder-current-group (car inf))))
    (when (and nnfolder-current-buffer
	       (buffer-name nnfolder-current-buffer))
      (save-excursion
	(set-buffer nnfolder-current-buffer)
	;; If the buffer was modified, write the file out now.
	(nnfolder-save-buffer)
	;; If we're shutting the server down, we need to kill the
	;; buffer and remove it from the open buffer list.  Or, of
	;; course, if we're trying to minimize our space impact.
	(kill-buffer (current-buffer))
	(setq nnfolder-buffer-alist (delq (assoc group nnfolder-buffer-alist)
					  nnfolder-buffer-alist)))))
  (setq nnfolder-current-group nil
	nnfolder-current-buffer nil)
  t)

(deffoo nnfolder-request-create-group (group &optional server) 
  (nnfolder-possibly-change-group nil server)
  (nnmail-activate 'nnfolder)
  (when group 
    (unless (assoc group nnfolder-group-alist)
      (push (list group (cons 1 0)) nnfolder-group-alist)
      (nnmail-save-active nnfolder-group-alist nnfolder-active-file)))
  t)

(deffoo nnfolder-request-list (&optional server)
  (nnfolder-possibly-change-group nil server)
  (save-excursion
    (nnmail-find-file nnfolder-active-file)
    (setq nnfolder-group-alist (nnmail-get-active))))

(deffoo nnfolder-request-newgroups (date &optional server)
  (nnfolder-possibly-change-group nil server)
  (nnfolder-request-list server))

(deffoo nnfolder-request-list-newsgroups (&optional server)
  (nnfolder-possibly-change-group nil server)
  (save-excursion
    (nnmail-find-file nnfolder-newsgroups-file)))

(deffoo nnfolder-request-expire-articles 
  (articles newsgroup &optional server force)
  (nnfolder-possibly-change-group newsgroup server)
  (let* ((is-old t)
	 rest)
    (nnmail-activate 'nnfolder)

    (save-excursion 
      (set-buffer nnfolder-current-buffer)
      (while (and articles is-old)
	(goto-char (point-min))
	(if (search-forward (nnfolder-article-string (car articles)) nil t)
	    (if (setq is-old
		      (nnmail-expired-article-p 
		       newsgroup
		       (buffer-substring 
			(point) (progn (end-of-line) (point))) 
		       force nnfolder-inhibit-expiry))
		(progn
		  (nnheader-message 5 "Deleting article %d..." 
				    (car articles) newsgroup)
		  (nnfolder-delete-mail))
	      (setq rest (cons (car articles) rest))))
	(setq articles (cdr articles)))
      (nnfolder-save-buffer)
      ;; Find the lowest active article in this group.
      (let* ((active (cadr (assoc newsgroup nnfolder-group-alist)))
	     (marker (concat "\n" nnfolder-article-marker))
	     (number "[0-9]+")
	     (activemin (cdr active)))
	(goto-char (point-min))
	(while (and (search-forward marker nil t)
		    (re-search-forward number nil t))
	  (setq activemin (min activemin
			       (string-to-number (buffer-substring
						  (match-beginning 0)
						  (match-end 0))))))
	(setcar active activemin))
      (nnmail-save-active nnfolder-group-alist nnfolder-active-file)
      (nconc rest articles))))

(deffoo nnfolder-request-move-article
  (article group server accept-form &optional last)
  (nnfolder-possibly-change-group group server)
  (let ((buf (get-buffer-create " *nnfolder move*"))
	result)
    (and 
     (nnfolder-request-article article group server)
     (save-excursion
       (set-buffer buf)
       (buffer-disable-undo (current-buffer))
       (erase-buffer)
       (insert-buffer-substring nntp-server-buffer)
       (goto-char (point-min))
       (while (re-search-forward 
	       (concat "^" nnfolder-article-marker)
	       (save-excursion (search-forward "\n\n" nil t) (point)) t)
	 (delete-region (progn (beginning-of-line) (point))
			(progn (forward-line 1) (point))))
       (setq result (eval accept-form))
       (kill-buffer buf)
       result)
     (save-excursion
       (nnfolder-possibly-change-group group server)
       (set-buffer nnfolder-current-buffer)
       (goto-char (point-min))
       (if (search-forward (nnfolder-article-string article) nil t)
	   (nnfolder-delete-mail))
       (and last (nnfolder-save-buffer))))
    result))

(deffoo nnfolder-request-accept-article (group &optional server last)
  (nnfolder-possibly-change-group group server)
  (nnmail-check-syntax)
  (and (stringp group) (nnfolder-possibly-change-group group))
  (let ((buf (current-buffer))
	result)
    (goto-char (point-min))
    (when (looking-at "X-From-Line: ")
      (replace-match "From "))
    (and 
     (nnfolder-request-list)
     (save-excursion
       (set-buffer buf)
       (goto-char (point-min))
       (search-forward "\n\n" nil t)
       (forward-line -1)
       (while (re-search-backward (concat "^" nnfolder-article-marker) nil t)
	 (delete-region (point) (progn (forward-line 1) (point))))
       (setq result (car (nnfolder-save-mail (and (stringp group) group)))))
     (save-excursion
       (set-buffer nnfolder-current-buffer)
       (and last (nnfolder-save-buffer))))
    (nnmail-save-active nnfolder-group-alist nnfolder-active-file)
    (unless result
      (nnheader-report 'nnfolder "Couldn't store article"))
    result))

(deffoo nnfolder-request-replace-article (article group buffer)
  (nnfolder-possibly-change-group group)
  (save-excursion
    (set-buffer nnfolder-current-buffer)
    (goto-char (point-min))
    (if (not (search-forward (nnfolder-article-string article) nil t))
	nil
      (nnfolder-delete-mail t t)
      (insert-buffer-substring buffer)
      (nnfolder-save-buffer)
      t)))

(deffoo nnfolder-request-delete-group (group &optional force server)
  (nnfolder-close-group group server t)
  ;; Delete all articles in GROUP.
  (if (not force)
      ()				; Don't delete the articles.
    ;; Delete the file that holds the group.
    (condition-case nil
	(delete-file (nnfolder-group-pathname group))
      (error nil)))
  ;; Remove the group from all structures.
  (setq nnfolder-group-alist 
	(delq (assoc group nnfolder-group-alist) nnfolder-group-alist)
	nnfolder-current-group nil
	nnfolder-current-buffer nil)
  ;; Save the active file.
  (nnmail-save-active nnfolder-group-alist nnfolder-active-file)
  t)

(deffoo nnfolder-request-rename-group (group new-name &optional server)
  (nnfolder-possibly-change-group group server)
  (save-excursion
    (set-buffer nnfolder-current-buffer)
    (and (file-writable-p buffer-file-name)
	 (condition-case ()
	     (progn
	       (rename-file 
		buffer-file-name
		(nnfolder-group-pathname new-name))
	       t)
	   (error nil))
	 ;; That went ok, so we change the internal structures.
	 (let ((entry (assoc group nnfolder-group-alist)))
	   (and entry (setcar entry new-name))
	   (setq nnfolder-current-buffer nil
		 nnfolder-current-group nil)
	   ;; Save the new group alist.
	   (nnmail-save-active nnfolder-group-alist nnfolder-active-file)
	   ;; We kill the buffer instead of renaming it and stuff.
	   (kill-buffer (current-buffer))
	   t))))


;;; Internal functions.

(defun nnfolder-article-string (article)
  (if (numberp article)
      (concat "\n" nnfolder-article-marker (int-to-string article) " ")
    (concat "\nMessage-ID: " article)))

(defun nnfolder-delete-mail (&optional force leave-delim)
  "Delete the message that point is in."
  (save-excursion
    (delete-region
     (save-excursion
       (re-search-backward (concat "^" message-unix-mail-delimiter) nil t)
       (if leave-delim (progn (forward-line 1) (point))
	 (match-beginning 0)))
     (progn
       (forward-line 1)
       (if (re-search-forward (concat "^" message-unix-mail-delimiter) nil t)
	   (if (and (not (bobp)) leave-delim)
	       (progn (forward-line -2) (point))
	     (match-beginning 0))
	 (point-max))))))

;; When scanning, we're not looking t immediately switch into the group - if
;; we know our information is up to date, don't even bother reading the file.
(defun nnfolder-possibly-change-group (group &optional server scanning)
  (when (and server
	     (not (nnfolder-server-opened server)))
    (nnfolder-open-server server))
  (when (and group (or nnfolder-current-buffer
		       (not (equal group nnfolder-current-group))))
    (unless (file-exists-p nnfolder-directory)
      (make-directory (directory-file-name nnfolder-directory) t))
    (nnfolder-possibly-activate-groups nil)
    (or (assoc group nnfolder-group-alist)
	(not (file-exists-p
	      (nnfolder-group-pathname group)))
	(progn
	  (setq nnfolder-group-alist 
		(cons (list group (cons 1 0)) nnfolder-group-alist))
	  (nnmail-save-active nnfolder-group-alist nnfolder-active-file)))
    (let (inf file)
      (if (and (equal group nnfolder-current-group)
	       nnfolder-current-buffer
	       (buffer-name nnfolder-current-buffer))
	  ()
	(setq nnfolder-current-group group)

	;; If we have to change groups, see if we don't already have the mbox
	;; in memory.  If we do, verify the modtime and destroy the mbox if
	;; needed so we can rescan it.
	(if (setq inf (assoc group nnfolder-buffer-alist))
	    (setq nnfolder-current-buffer (nth 1 inf)))

	;; If the buffer is not live, make sure it isn't in the alist.  If it
	;; is live, verify that nobody else has touched the file since last
	;; time.
	(if (or (not (and nnfolder-current-buffer
			  (buffer-name nnfolder-current-buffer)))
		(not (and (bufferp nnfolder-current-buffer)
			  (verify-visited-file-modtime 
			   nnfolder-current-buffer))))
	    (progn
	      (if (and nnfolder-current-buffer
		       (buffer-name nnfolder-current-buffer)
		       (bufferp nnfolder-current-buffer))
		  (kill-buffer nnfolder-current-buffer))
	      (setq nnfolder-buffer-alist (delq inf nnfolder-buffer-alist))
	      (setq inf nil)))
      
	(if inf
	    ()
	  (save-excursion
	    (setq file (nnfolder-group-pathname group))
	    (if (file-directory-p (file-truename file))
		()
	      (unless (file-exists-p file)
		(unless (file-exists-p (file-name-directory file))
		  (make-directory (file-name-directory file) t))
		(write-region 1 1 file t 'nomesg))
	      (setq nnfolder-current-buffer
		    (nnfolder-read-folder file scanning))
	      (if nnfolder-current-buffer 
		  (progn
		    (set-buffer nnfolder-current-buffer)
		    (setq nnfolder-buffer-alist 
			  (cons (list group nnfolder-current-buffer)
				nnfolder-buffer-alist)))))))))
    (setq nnfolder-current-group group)))

(defun nnfolder-save-mail (&optional group)
  "Called narrowed to an article."
  (let* ((nnmail-split-methods 
	  (if group (list (list group "")) nnmail-split-methods))
	 (group-art-list
	  (nreverse (nnmail-article-group 'nnfolder-active-number)))
	 (delim (concat "^" message-unix-mail-delimiter))
	 save-list group-art)
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
    (setq save-list group-art-list)
    (nnmail-insert-lines)
    (nnmail-insert-xref group-art-list)
    (run-hooks 'nnmail-prepare-save-mail-hook)
    (run-hooks 'nnfolder-prepare-save-mail-hook)

    ;; Insert the mail into each of the destination groups.
    (while group-art-list
      (setq group-art (car group-art-list)
	    group-art-list (cdr group-art-list))

      ;; Kill the previous newsgroup markers.
      (goto-char (point-min))
      (search-forward "\n\n" nil t)
      (forward-line -1)
      (while (search-backward (concat "\n" nnfolder-article-marker) nil t)
	(delete-region (1+ (point)) (progn (forward-line 2) (point))))

      (nnfolder-possibly-change-group (car group-art))
      ;; Insert the new newsgroup marker.
      (nnfolder-insert-newsgroup-line group-art)
      (unless nnfolder-current-buffer
	(nnfolder-close-group (car group-art))
	(nnfolder-request-create-group (car group-art))
	(nnfolder-possibly-change-group (car group-art)))
      (let ((beg (point-min))
	    (end (point-max))
	    (obuf (current-buffer)))
	(set-buffer nnfolder-current-buffer)
	(goto-char (point-max))
	(unless (eolp)
	  (insert "\n"))
	(insert "\n")
	(insert-buffer-substring obuf beg end)
	(set-buffer obuf)))

    ;; Did we save it anywhere?
    save-list))

(defun nnfolder-insert-newsgroup-line (group-art)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "\n\n" nil t)
	(progn
	  (forward-char -1)
	  (insert (format (concat nnfolder-article-marker "%d   %s\n")
			  (cdr group-art) (current-time-string)))))))

(defun nnfolder-possibly-activate-groups (&optional group)
  (save-excursion
    ;; If we're looking for the activation of a specific group, find out
    ;; its real name and switch to it.
    (if group (nnfolder-possibly-change-group group))
    ;; If the group alist isn't active, activate it now.
    (nnmail-activate 'nnfolder)))

(defun nnfolder-active-number (group)
  (when group
    (save-excursion 
      ;; Find the next article number in GROUP.
      (prog1
	  (let ((active (cadr (assoc group nnfolder-group-alist))))
	    (if active
		(setcdr active (1+ (cdr active)))
	      ;; This group is new, so we create a new entry for it.
	      ;; This might be a bit naughty... creating groups on the drop of
	      ;; a hat, but I don't know...
	      (setq nnfolder-group-alist 
		    (cons (list group (setq active (cons 1 1)))
			  nnfolder-group-alist)))
	    (cdr active))
	(nnmail-save-active nnfolder-group-alist nnfolder-active-file)
	(nnfolder-possibly-activate-groups group)))))


;; This method has a problem if you've accidentally let the active list get
;; out of sync with the files.  This could happen, say, if you've
;; accidentally gotten new mail with something other than Gnus (but why
;; would _that_ ever happen? :-).  In that case, we will be in the middle of
;; processing the file, ready to add new X-Gnus article number markers, and
;; we'll run across a message with no ID yet - the active list _may_not_ be
;; ready for us yet.

;; To handle this, I'm modifying this routine to maintain the maximum ID seen
;; so far, and when we hit a message with no ID, we will _manually_ scan the
;; rest of the message looking for any more, possibly higher IDs.  We'll
;; assume the maximum that we find is the highest active.  Note that this
;; shouldn't cost us much extra time at all, but will be a lot less
;; vulnerable to glitches between the mbox and the active file.

(defun nnfolder-read-folder (file &optional scanning)
  ;; This is an attempt at a serious shortcut - don't even read in the file
  ;; if we know we've seen it since the last time it was touched.
  (let ((scantime (cadr (assoc nnfolder-current-group 
			       nnfolder-scantime-alist)))
	(modtime (nth 5 (or (file-attributes file) '(nil nil nil nil nil)))))
    (if (and scanning scantime
	     (eq (car scantime) (car modtime))
	     (eq (cdr scantime) (cadr modtime)))
	nil
      (save-excursion
	(nnfolder-possibly-activate-groups nil)
	;; Read in the file.
	(set-buffer (setq nnfolder-current-buffer 
			  (nnheader-find-file-noselect file nil 'raw)))
	(buffer-disable-undo (current-buffer))
	;; If the file hasn't been touched since the last time we scanned it,
	;; don't bother doing anything with it.
	(let ((delim (concat "^" message-unix-mail-delimiter))
	      (marker (concat "\n" nnfolder-article-marker))
	      (number "[0-9]+")
	      (active (or (cadr (assoc nnfolder-current-group 
				       nnfolder-group-alist))
			  (cons 1 0)))
	      (scantime (assoc nnfolder-current-group nnfolder-scantime-alist))
	      (minid (lsh -1 -1))
	      maxid start end newscantime)

	  (setq maxid (or (cdr active) 0))
	  (goto-char (point-min))

	  ;; Anytime the active number is 1 or 0, it is suspect.  In that
	  ;; case, search the file manually to find the active number.  Or,
	  ;; of course, if we're being paranoid.  (This would also be the
	  ;; place to build other lists from the header markers, such as
	  ;; expunge lists, etc., if we ever desired to abandon the active
	  ;; file entirely for mboxes.)
	  (when (or nnfolder-ignore-active-file
		    (< maxid 2))
		(while (and (search-forward marker nil t)
			    (re-search-forward number nil t))
		  (let ((newnum (string-to-number (match-string 0))))
		    (setq maxid (max maxid newnum))
		    (setq minid (min minid newnum))))
		(setcar active (max 1 (min minid maxid)))
		(setcdr active (max maxid (cdr active)))
		(goto-char (point-min)))

	  ;; As long as we trust that the user will only insert unmarked mail
	  ;; at the end, go to the end and search backwards for the last
	  ;; marker.  Find the start of that message, and begin to search for
	  ;; unmarked messages from there.
	  (if (not (or nnfolder-distrust-mbox
		       (< maxid 2)))
	      (progn
		(goto-char (point-max))
		(if (not (re-search-backward marker nil t))
		    (goto-char (point-min))
		  (if (not (re-search-backward delim nil t))
		      (goto-char (point-min))))))

	  ;; Keep track of the active number on our own, and insert it back
	  ;; into the active list when we're done. Also, prime the pump to
	  ;; cut down on the number of searches we do.
	  (setq end (point-marker))
	  (set-marker end (or (and (re-search-forward delim nil t)
				   (match-beginning 0))
			      (point-max)))
	  (while (not (= end (point-max)))
	    (setq start (marker-position end))
	    (goto-char end)
	    ;; There may be more than one "From " line, so we skip past
	    ;; them.  
	    (while (looking-at delim) 
	      (forward-line 1))
	    (set-marker end (or (and (re-search-forward delim nil t)
				     (match-beginning 0))
				(point-max)))
	    (goto-char start)
	    (if (not (search-forward marker end t))
		(progn
		  (narrow-to-region start end)
		  (nnmail-insert-lines)
		  (nnfolder-insert-newsgroup-line
		   (cons nil (nnfolder-active-number nnfolder-current-group)))
		  (widen))))

	  ;; Make absolutely sure that the active list reflects reality!
	  (nnmail-save-active nnfolder-group-alist nnfolder-active-file)
	  ;; Set the scantime for this group.
	  (setq newscantime (visited-file-modtime))
	  (if scantime
	      (setcdr scantime (list newscantime))
	    (push (list nnfolder-current-group newscantime) 
		  nnfolder-scantime-alist))
	  (current-buffer))))))

;;;###autoload
(defun nnfolder-generate-active-file ()
  "Look for mbox folders in the nnfolder directory and make them into groups."
  (interactive)
  (nnmail-activate 'nnfolder)
  (let ((files (directory-files nnfolder-directory))
	file)
    (while (setq file (pop files))
      (when (and (not (backup-file-name-p file))
		 (nnheader-mail-file-mbox-p file))
	(nnheader-message 5 "Adding group %s..." file)
	(push (list file (cons 1 0)) nnfolder-group-alist)
	(nnfolder-possibly-change-group file)
;;	(nnfolder-read-folder file)
	(nnfolder-close-group file))
      (message ""))))

(defun nnfolder-group-pathname (group)
  "Make pathname for GROUP."
  (let ((dir (file-name-as-directory (expand-file-name nnfolder-directory))))
    ;; If this file exists, we use it directly.
    (if (or nnmail-use-long-file-names 
	    (file-exists-p (concat dir group)))
	(concat dir group)
      ;; If not, we translate dots into slashes.
      (concat dir (nnheader-replace-chars-in-string group ?. ?/)))))

(defun nnfolder-save-buffer ()
  "Save the buffer."
  (when (buffer-modified-p)
    (run-hooks 'nnfolder-save-buffer-hook)
    (save-buffer)))

(provide 'nnfolder)

;;; nnfolder.el ends here
