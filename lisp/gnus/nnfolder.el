;;; nnfolder.el --- mail folder access for Gnus
;; Copyright (C) 1995,96,97 Free Software Foundation, Inc.

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

;;; Code:

(require 'nnheader)
(require 'message)
(require 'nnmail)
(require 'nnoo)
(require 'cl)
(require 'gnus-util)

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
(defvoo nnfolder-active-timestamp nil)



;;; Interface functions

(nnoo-define-basics nnfolder)

(deffoo nnfolder-retrieve-headers (articles &optional group server fetch-old)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (let (article art-string start stop)
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
	    (when (or (search-forward art-string nil t)
		      ;; Don't search the whole file twice!  Also, articles
		      ;; probably have some locality by number, so searching
		      ;; backwards will be faster.  Especially if we're at the
		      ;; beginning of the buffer :-). -SLB
		      (search-backward art-string nil t))
	      (nnmail-search-unix-mail-delim-backward)
	      (setq start (point))
	      (search-forward "\n\n" nil t)
	      (setq stop (1- (point)))
	      (set-buffer nntp-server-buffer)
	      (insert (format "221 %d Article retrieved.\n" article))
	      (insert-buffer-substring nnfolder-current-buffer start stop)
	      (goto-char (point-max))
	      (insert ".\n"))
	    (setq articles (cdr articles)))

	  (set-buffer nntp-server-buffer)
	  (nnheader-fold-continuation-lines)
	  'headers)))))

(deffoo nnfolder-open-server (server &optional defs)
  (nnoo-change-server 'nnfolder server defs)
  (nnmail-activate 'nnfolder t)
  (gnus-make-directory nnfolder-directory)
  (cond
   ((not (file-exists-p nnfolder-directory))
    (nnfolder-close-server)
    (nnheader-report 'nnfolder "Couldn't create directory: %s"
		     nnfolder-directory))
   ((not (file-directory-p (file-truename nnfolder-directory)))
    (nnfolder-close-server)
    (nnheader-report 'nnfolder "Not a directory: %s" nnfolder-directory))
   (t
    (nnmail-activate 'nnfolder)
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
    (when (search-forward (nnfolder-article-string article) nil t)
      (let (start stop)
	(nnmail-search-unix-mail-delim-backward)
	(setq start (point))
	(forward-line 1)
	(unless (and (nnmail-search-unix-mail-delim)
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
  (nnfolder-possibly-change-group group server t)
  (save-excursion
    (if (not (assoc group nnfolder-group-alist))
	(nnheader-report 'nnfolder "No such group: %s" group)
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
  (nnfolder-possibly-change-group nil server)
  (when nnfolder-get-new-mail
    (nnfolder-possibly-change-group group server)
    (nnmail-get-new-mail
     'nnfolder
     (lambda ()
       (let ((bufs nnfolder-buffer-alist))
	 (save-excursion
	   (while bufs
	     (if (not (gnus-buffer-live-p (nth 1 (car bufs))))
		 (setq nnfolder-buffer-alist
		       (delq (car bufs) nnfolder-buffer-alist))
	       (set-buffer (nth 1 (car bufs)))
	       (nnfolder-save-buffer)
	       (kill-buffer (current-buffer)))
	     (setq bufs (cdr bufs))))))
     nnfolder-directory
     group)))

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
	(when (and nnfolder-current-group
		   nnfolder-current-buffer)
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

(deffoo nnfolder-request-create-group (group &optional server args)
  (nnfolder-possibly-change-group nil server)
  (nnmail-activate 'nnfolder)
  (when group
    (unless (assoc group nnfolder-group-alist)
      (push (list group (cons 1 0)) nnfolder-group-alist)
      (nnmail-save-active nnfolder-group-alist nnfolder-active-file)
      (nnfolder-read-folder group)))
  t)

(deffoo nnfolder-request-list (&optional server)
  (nnfolder-possibly-change-group nil server)
  (save-excursion
    ;; 1997/8/14 by MORIOKA Tomohiko
    ;;    for XEmacs/mule.
    (let ((nnmail-file-coding-system nnmail-active-file-coding-system)
	  (file-name-coding-system 'binary) ; for Emacs 20
	  (pathname-coding-system 'binary)) ; for XEmacs/mule
      (nnmail-find-file nnfolder-active-file)
      (setq nnfolder-group-alist (nnmail-get-active)))
    t))

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
	(when (search-forward (nnfolder-article-string (car articles)) nil t)
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
	    (push (car articles) rest)))
	(setq articles (cdr articles)))
      (unless nnfolder-inhibit-expiry
	(nnheader-message 5 "Deleting articles...done"))
      (nnfolder-save-buffer)
      (nnfolder-adjust-min-active newsgroup)
      (nnmail-save-active nnfolder-group-alist nnfolder-active-file)
      (nconc rest articles))))

(deffoo nnfolder-request-move-article
  (article group server accept-form &optional last)
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
       (when (search-forward (nnfolder-article-string article) nil t)
	 (nnfolder-delete-mail))
       (when last
	 (nnfolder-save-buffer)
	 (nnfolder-adjust-min-active group)
	 (nnmail-save-active nnfolder-group-alist nnfolder-active-file))))
    result))

(deffoo nnfolder-request-accept-article (group &optional server last)
  (nnfolder-possibly-change-group group server)
  (nnmail-check-syntax)
  (let ((buf (current-buffer))
	result art-group)
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
       (when nnmail-cache-accepted-message-ids
	 (nnmail-cache-insert (nnmail-fetch-field "message-id")))
       (setq result (if (stringp group)
			(list (cons group (nnfolder-active-number group)))
		      (setq art-group
			    (nnmail-article-group 'nnfolder-active-number))))
       (if (and (null result)
		(yes-or-no-p "Moved to `junk' group; delete article? "))
	   (setq result 'junk)
	 (setq result
	       (car (nnfolder-save-mail result)))))
     (when last
       (save-excursion
	 (nnfolder-possibly-change-folder (or (caar art-group) group))
	 (nnfolder-save-buffer)
	 (when nnmail-cache-accepted-message-ids
	   (nnmail-cache-close)))))
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
    (ignore-errors
      (delete-file (nnfolder-group-pathname group))))
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
	 (ignore-errors
	   (rename-file
	    buffer-file-name
	    (nnfolder-group-pathname new-name))
	   t)
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

(defun nnfolder-request-regenerate (server)
  (nnfolder-possibly-change-group nil server)
  (nnfolder-generate-active-file)
  t)


;;; Internal functions.

(defun nnfolder-adjust-min-active (group)
  ;; Find the lowest active article in this group.
  (let* ((active (cadr (assoc group nnfolder-group-alist)))
	 (marker (concat "\n" nnfolder-article-marker))
	 (number "[0-9]+")
	 (activemin (cdr active)))
    (save-excursion
      (set-buffer nnfolder-current-buffer)
      (goto-char (point-min))
      (while (and (search-forward marker nil t)
		  (re-search-forward number nil t))
	(setq activemin (min activemin
			     (string-to-number (buffer-substring
						(match-beginning 0)
						(match-end 0))))))
      (setcar active activemin))))

(defun nnfolder-article-string (article)
  (if (numberp article)
      (concat "\n" nnfolder-article-marker (int-to-string article) " ")
    (concat "\nMessage-ID: " article)))

(defun nnfolder-delete-mail (&optional force leave-delim)
  "Delete the message that point is in."
  (save-excursion
    (delete-region
     (save-excursion
       (nnmail-search-unix-mail-delim-backward)
       (if leave-delim (progn (forward-line 1) (point))
	 (point)))
     (progn
       (forward-line 1)
       (if (nnmail-search-unix-mail-delim)
	   (if (and (not (bobp)) leave-delim)
	       (progn (forward-line -2) (point))
	     (point))
	 (point-max))))))

(defun nnfolder-possibly-change-group (group &optional server dont-check)
  ;; Change servers.
  (when (and server
	     (not (nnfolder-server-opened server)))
    (nnfolder-open-server server))
  (unless (gnus-buffer-live-p nnfolder-current-buffer)
    (setq nnfolder-current-buffer nil
	  nnfolder-current-group nil))
  ;; Change group.
  (when (and group
	     (not (equal group nnfolder-current-group)))
    (let ((file-name-coding-system 'binary)
	  (pathname-coding-system 'binary))
      (nnmail-activate 'nnfolder)
      (when (and (not (assoc group nnfolder-group-alist))
		 (not (file-exists-p
		       (nnfolder-group-pathname group))))
	;; The group doesn't exist, so we create a new entry for it.
	(push (list group (cons 1 0)) nnfolder-group-alist)
	(nnmail-save-active nnfolder-group-alist nnfolder-active-file))

      (if dont-check
	  (setq nnfolder-current-group group
		nnfolder-current-buffer nil)
	(let (inf file)
	  ;; If we have to change groups, see if we don't already have the
	  ;; folder in memory.  If we do, verify the modtime and destroy
	  ;; the folder if needed so we can rescan it.
	  (setq nnfolder-current-buffer
		(nth 1 (assoc group nnfolder-buffer-alist)))

	  ;; If the buffer is not live, make sure it isn't in the alist.  If it
	  ;; is live, verify that nobody else has touched the file since last
	  ;; time.
	  (when (and nnfolder-current-buffer
		     (not (gnus-buffer-live-p nnfolder-current-buffer)))
	    (setq nnfolder-buffer-alist (delq inf nnfolder-buffer-alist)
		  nnfolder-current-buffer nil))

	  (setq nnfolder-current-group group)

	  (when (or (not nnfolder-current-buffer)
		    (not (verify-visited-file-modtime nnfolder-current-buffer)))
	    (save-excursion
	      (setq file (nnfolder-group-pathname group))
	      ;; See whether we need to create the new file.
	      (unless (file-exists-p file)
		(gnus-make-directory (file-name-directory file))
		(nnmail-write-region 1 1 file t 'nomesg))
	      (when (setq nnfolder-current-buffer (nnfolder-read-folder group))
		(set-buffer nnfolder-current-buffer)
		(push (list group nnfolder-current-buffer)
		      nnfolder-buffer-alist)))))))))

(defun nnfolder-save-mail (group-art-list)
  "Called narrowed to an article."
  (let* (save-list group-art)
    (goto-char (point-min))
    ;; The From line may have been quoted by movemail.
    (when (looking-at (concat ">" message-unix-mail-delimiter))
      (delete-char 1))
    ;; This might come from somewhere else.
    (unless (looking-at message-unix-mail-delimiter)
      (insert "From nobody " (current-time-string) "\n")
      (goto-char (point-min)))
    ;; Quote all "From " lines in the article.
    (forward-line 1)
    (let (case-fold-search)
      (while (re-search-forward "^From " nil t)
	(beginning-of-line)
	(insert "> ")))
    (setq save-list group-art-list)
    (nnmail-insert-lines)
    (nnmail-insert-xref group-art-list)
    (run-hooks 'nnmail-prepare-save-mail-hook)
    (run-hooks 'nnfolder-prepare-save-mail-hook)

    ;; Insert the mail into each of the destination groups.
    (while (setq group-art (pop group-art-list))
      ;; Kill any previous newsgroup markers.
      (goto-char (point-min))
      (search-forward "\n\n" nil t)
      (forward-line -1)
      (while (search-backward (concat "\n" nnfolder-article-marker) nil t)
	(delete-region (1+ (point)) (progn (forward-line 2) (point))))

      ;; Insert the new newsgroup marker.
      (nnfolder-insert-newsgroup-line group-art)

      (save-excursion
	(let ((beg (point-min))
	      (end (point-max))
	      (obuf (current-buffer)))
	  (nnfolder-possibly-change-folder (car group-art))
	  (let ((buffer-read-only nil))
	    (goto-char (point-max))
	    (unless (eolp)
	      (insert "\n"))
	    (unless (bobp)
	      (insert "\n"))
	    (insert-buffer-substring obuf beg end)))))

    ;; Did we save it anywhere?
    save-list))

(defun nnfolder-insert-newsgroup-line (group-art)
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "\n\n" nil t)
      (forward-char -1)
      (insert (format (concat nnfolder-article-marker "%d   %s\n")
		      (cdr group-art) (current-time-string))))))

(defun nnfolder-active-number (group)
  ;; Find the next article number in GROUP.
  (let ((active (cadr (assoc group nnfolder-group-alist))))
    (if active
	(setcdr active (1+ (cdr active)))
      ;; This group is new, so we create a new entry for it.
      ;; This might be a bit naughty... creating groups on the drop of
      ;; a hat, but I don't know...
      (push (list group (setq active (cons 1 1)))
	    nnfolder-group-alist))
    (cdr active)))

(defun nnfolder-possibly-change-folder (group)
  (let ((inf (assoc group nnfolder-buffer-alist)))
    (if (and inf
	     (gnus-buffer-live-p (cadr inf)))
	(set-buffer (cadr inf))
      (when inf
	(setq nnfolder-buffer-alist (delq inf nnfolder-buffer-alist)))
      (when nnfolder-group-alist
	(nnmail-save-active nnfolder-group-alist nnfolder-active-file))
      (push (list group (nnfolder-read-folder group))
	    nnfolder-buffer-alist))))

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

(defun nnfolder-read-folder (group)
  (let* ((file (nnfolder-group-pathname group))
	 (buffer (set-buffer (nnheader-find-file-noselect file))))
    (if (equal (cadr (assoc group nnfolder-scantime-alist))
	       (nth 5 (file-attributes file)))
	;; This looks up-to-date, so we don't do any scanning.
	buffer
      ;; Parse the damn thing.
      (save-excursion
	(nnmail-activate 'nnfolder)
	;; Read in the file.
	(let ((delim (concat "^" message-unix-mail-delimiter))
	      (marker (concat "\n" nnfolder-article-marker))
	      (number "[0-9]+")
	      (active (or (cadr (assoc group nnfolder-group-alist))
			  (cons 1 0)))
	      (scantime (assoc group nnfolder-scantime-alist))
	      (minid (lsh -1 -1))
	      maxid start end newscantime
	      buffer-read-only)
	  (buffer-disable-undo (current-buffer))
	  (setq maxid (cdr active))
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
	  (when (not (or nnfolder-distrust-mbox
			 (< maxid 2)))
	    (goto-char (point-max))
	    (unless (re-search-backward marker nil t)
	      (goto-char (point-min)))
	    (when (nnmail-search-unix-mail-delim)
	      (goto-char (point-min))))

	  ;; Keep track of the active number on our own, and insert it back
	  ;; into the active list when we're done.  Also, prime the pump to
	  ;; cut down on the number of searches we do.
	  (unless (nnmail-search-unix-mail-delim)
	    (goto-char (point-max)))
	  (setq end (point-marker))
	  (while (not (= end (point-max)))
	    (setq start (marker-position end))
	    (goto-char end)
	    ;; There may be more than one "From " line, so we skip past
	    ;; them.
	    (while (looking-at delim)
	      (forward-line 1))
	    (set-marker end (if (nnmail-search-unix-mail-delim)
				(point)
			      (point-max)))
	    (goto-char start)
	    (when (not (search-forward marker end t))
	      (narrow-to-region start end)
	      (nnmail-insert-lines)
	      (nnfolder-insert-newsgroup-line
	       (cons nil (nnfolder-active-number nnfolder-current-group)))
	      (widen)))

	  (set-marker end nil)
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
                 (message-mail-file-mbox-p
		  (nnheader-concat nnfolder-directory file)))
        (let ((oldgroup (assoc file nnfolder-group-alist)))
          (if oldgroup
              (nnheader-message 5 "Refreshing group %s..." file)
            (nnheader-message 5 "Adding group %s..." file))
	  (if oldgroup
	      (setq nnfolder-group-alist
		    (delq oldgroup (copy-sequence nnfolder-group-alist))))
          (push (list file (cons 1 0)) nnfolder-group-alist)
          (nnfolder-possibly-change-folder file)
          (nnfolder-possibly-change-group file)
          (nnfolder-close-group file))))
    (message "")))

(defun nnfolder-group-pathname (group)
  "Make pathname for GROUP."
  (setq group (gnus-encode-coding-string group nnmail-pathname-coding-system))
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
    (gnus-make-directory (file-name-directory (buffer-file-name)))
    (save-buffer)))

(provide 'nnfolder)

;;; nnfolder.el ends here
