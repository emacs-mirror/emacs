;;; nnmbox.el --- mail mbox access for Gnus

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

;; For an overview of what the interface functions do, please see the
;; Gnus sources.  

;;; Code:

(require 'nnheader)
(require 'rmail)
(require 'nnmail)

(defvar nnmbox-mbox-file (expand-file-name "~/mbox")
  "The name of the mail box file in the user's home directory.")

(defvar nnmbox-active-file (expand-file-name "~/.mbox-active")
  "The name of the active file for the mail box.")

(defvar nnmbox-get-new-mail t
  "If non-nil, nnmbox will check the incoming mail file and split the mail.")

(defvar nnmbox-prepare-save-mail-hook nil
  "Hook run narrowed to an article before saving.")



(defconst nnmbox-version "nnmbox 1.0"
  "nnmbox version.")

(defvar nnmbox-current-group nil
  "Current nnmbox news group directory.")

(defconst nnmbox-mbox-buffer nil)

(defvar nnmbox-status-string "")

(defvar nnmbox-group-alist nil)
(defvar nnmbox-active-timestamp nil)



(defvar nnmbox-current-server nil)
(defvar nnmbox-server-alist nil)
(defvar nnmbox-server-variables 
  (list
   (list 'nnmbox-mbox-file nnmbox-mbox-file)
   (list 'nnmbox-active-file nnmbox-active-file)
   (list 'nnmbox-get-new-mail nnmbox-get-new-mail)
   '(nnmbox-current-group nil)
   '(nnmbox-status-string "")
   '(nnmbox-group-alist nil)))



;;; Interface functions

(defun nnmbox-retrieve-headers (sequence &optional newsgroup server)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (let ((number (length sequence))
	  (count 0)
	  article art-string start stop)
      (nnmbox-possibly-change-newsgroup newsgroup)
      (if (stringp (car sequence))
	  'headers
	(while sequence
	  (setq article (car sequence))
	  (setq art-string (nnmbox-article-string article))
	  (set-buffer nnmbox-mbox-buffer)
	  (if (or (search-forward art-string nil t)
		  (progn (goto-char (point-min))
			 (search-forward art-string nil t)))
	      (progn
		(setq start 
		      (save-excursion
			(re-search-backward 
			 (concat "^" rmail-unix-mail-delimiter) nil t)
			(point)))
		(search-forward "\n\n" nil t)
		(setq stop (1- (point)))
		(set-buffer nntp-server-buffer)
		(insert (format "221 %d Article retrieved.\n" article))
		(insert-buffer-substring nnmbox-mbox-buffer start stop)
		(goto-char (point-max))
		(insert ".\n")))
	  (setq sequence (cdr sequence))
	  (setq count (1+ count))
	  (and (numberp nnmail-large-newsgroup)
	       (> number nnmail-large-newsgroup)
	       (zerop (% count 20))
	       gnus-verbose-backends
	       (message "nnmbox: Receiving headers... %d%%"
			(/ (* count 100) number))))

	(and (numberp nnmail-large-newsgroup)
	     (> number nnmail-large-newsgroup)
	     gnus-verbose-backends
	     (message "nnmbox: Receiving headers...done"))

	;; Fold continuation lines.
	(set-buffer nntp-server-buffer)
	(goto-char (point-min))
	(while (re-search-forward "\\(\r?\n[ \t]+\\)+" nil t)
	  (replace-match " " t t))
	'headers))))

(defun nnmbox-open-server (server &optional defs)
  (nnheader-init-server-buffer)
  (if (equal server nnmbox-current-server)
      t
    (if nnmbox-current-server
	(setq nnmbox-server-alist 
	      (cons (list nnmbox-current-server
			  (nnheader-save-variables nnmbox-server-variables))
		    nnmbox-server-alist)))
    (let ((state (assoc server nnmbox-server-alist)))
      (if state 
	  (progn
	    (nnheader-restore-variables (nth 1 state))
	    (setq nnmbox-server-alist (delq state nnmbox-server-alist)))
	(nnheader-set-init-variables nnmbox-server-variables defs)))
    (setq nnmbox-current-server server)))

(defun nnmbox-close-server (&optional server)
  t)

(defun nnmbox-server-opened (&optional server)
  (and (equal server nnmbox-current-server)
       nnmbox-mbox-buffer
       (buffer-name nnmbox-mbox-buffer)
       nntp-server-buffer
       (buffer-name nntp-server-buffer)))

(defun nnmbox-status-message (&optional server)
  nnmbox-status-string)

(defun nnmbox-request-article (article &optional newsgroup server buffer)
  (nnmbox-possibly-change-newsgroup newsgroup)
  (if (stringp article)
      nil
    (save-excursion
      (set-buffer nnmbox-mbox-buffer)
      (goto-char (point-min))
      (if (search-forward (nnmbox-article-string article) nil t)
	  (let (start stop)
	    (re-search-backward (concat "^" rmail-unix-mail-delimiter) nil t)
	    (setq start (point))
	    (forward-line 1)
	    (or (and (re-search-forward 
		      (concat "^" rmail-unix-mail-delimiter) nil t)
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
	      t))))))

(defun nnmbox-request-group (group &optional server dont-check)
  (save-excursion
    (if (nnmbox-possibly-change-newsgroup group)
	(if dont-check
	    t
	  (nnmbox-get-new-mail group)
	  (save-excursion
	    (set-buffer nntp-server-buffer)
	    (erase-buffer)
	    (let ((active (assoc group nnmbox-group-alist)))
	      (insert (format "211 %d %d %d %s\n" 
			      (1+ (- (cdr (car (cdr active)))
				     (car (car (cdr active)))))
			      (car (car (cdr active)))
			      (cdr (car (cdr active)))
			      (car active))))
	    t)))))

(defun nnmbox-close-group (group &optional server)
  t)

(defun nnmbox-request-list (&optional server)
  (if server (nnmbox-get-new-mail))
  (save-excursion
    (or (nnmail-find-file nnmbox-active-file)
	(progn
	  (setq nnmbox-group-alist (nnmail-get-active))
	  (nnmail-save-active nnmbox-group-alist nnmbox-active-file)
	  (nnmail-find-file nnmbox-active-file)))))

(defun nnmbox-request-newgroups (date &optional server)
  (nnmbox-request-list server))

(defun nnmbox-request-list-newsgroups (&optional server)
  (setq nnmbox-status-string "nnmbox: LIST NEWSGROUPS is not implemented.")
  nil)

(defun nnmbox-request-post (&optional server)
  (mail-send-and-exit nil))

(defalias 'nnmbox-request-post-buffer 'nnmail-request-post-buffer)

(defun nnmbox-request-expire-articles 
  (articles newsgroup &optional server force)
  (nnmbox-possibly-change-newsgroup newsgroup)
  (let* ((days (or (and nnmail-expiry-wait-function
			(funcall nnmail-expiry-wait-function newsgroup))
		   nnmail-expiry-wait))
	 (is-old t)
	 rest)
    (nnmail-activate 'nnmbox)

    (save-excursion 
      (set-buffer nnmbox-mbox-buffer)
      (while (and articles is-old)
	(goto-char (point-min))
	(if (search-forward (nnmbox-article-string (car articles)) nil t)
	    (if (or force
		    (setq is-old
			  (> (nnmail-days-between 
			      (current-time-string)
			      (buffer-substring 
			       (point) (progn (end-of-line) (point))))
			     days)))
		(progn
		  (and gnus-verbose-backends
		       (message "Deleting article %s..." (car articles)))
		  (nnmbox-delete-mail))
	      (setq rest (cons (car articles) rest))))
	(setq articles (cdr articles)))
      (save-buffer)
      ;; Find the lowest active article in this group.
      (let ((active (nth 1 (assoc newsgroup nnmbox-group-alist))))
	(goto-char (point-min))
	(while (and (not (search-forward
			  (nnmbox-article-string (car active)) nil t))
		    (<= (car active) (cdr active)))
	  (setcar active (1+ (car active)))
	  (goto-char (point-min))))
      (nnmail-save-active nnmbox-group-alist nnmbox-active-file)
      (nconc rest articles))))

(defun nnmbox-request-move-article
  (article group server accept-form &optional last)
  (nnmbox-possibly-change-newsgroup group)
  (let ((buf (get-buffer-create " *nnmbox move*"))
	result)
    (and 
     (nnmbox-request-article article group server)
     (save-excursion
       (set-buffer buf)
       (buffer-disable-undo (current-buffer))
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
       (set-buffer nnmbox-mbox-buffer)
       (goto-char (point-min))
       (if (search-forward (nnmbox-article-string article) nil t)
	   (nnmbox-delete-mail))
       (and last (save-buffer))))
    result))

(defun nnmbox-request-accept-article (group &optional last)
  (let ((buf (current-buffer))
	result)
    (goto-char (point-min))
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
       (setq result (nnmbox-save-mail (and (stringp group) group))))
     (save-excursion
       (set-buffer nnmbox-mbox-buffer)
       (insert-buffer-substring buf)
       (and last (save-buffer))
       result)
     (nnmail-save-active nnmbox-group-alist nnmbox-active-file))
    (car result)))

(defun nnmbox-request-replace-article (article group buffer)
  (nnmbox-possibly-change-newsgroup group)
  (save-excursion
    (set-buffer nnmbox-mbox-buffer)
    (goto-char (point-min))
    (if (not (search-forward (nnmbox-article-string article) nil t))
	nil
      (nnmbox-delete-mail t t)
      (insert-buffer-substring buffer)
      (save-buffer)
      t)))


;;; Internal functions.

;; If FORCE, delete article no matter how many X-Gnus-Newsgroup
;; headers there are. If LEAVE-DELIM, don't delete the Unix mbox
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
	 (re-search-backward (concat "^" rmail-unix-mail-delimiter) nil t)
	 (if leave-delim (progn (forward-line 1) (point))
	   (match-beginning 0)))
       (progn
	 (forward-line 1)
	 (or (and (re-search-forward (concat "^" rmail-unix-mail-delimiter) 
				     nil t)
		  (if (and (not (bobp)) leave-delim)
		      (progn (forward-line -2) (point))
		    (match-beginning 0)))
	     (point-max))))
      (goto-char (point-min))
      ;; Only delete the article if no other groups owns it as well.
      (if (or force (not (re-search-forward "^X-Gnus-Newsgroup: " nil t)))
	  (delete-region (point-min) (point-max))))))

(defun nnmbox-possibly-change-newsgroup (newsgroup)
  (if (or (not nnmbox-mbox-buffer)
	  (not (buffer-name nnmbox-mbox-buffer)))
      (save-excursion
	(set-buffer (setq nnmbox-mbox-buffer 
			  (nnheader-find-file-noselect
			   nnmbox-mbox-file nil 'raw)))
	(buffer-disable-undo (current-buffer))))
  (if (not nnmbox-group-alist)
      (nnmail-activate 'nnmbox))
  (if newsgroup
      (if (assoc newsgroup nnmbox-group-alist)
	  (setq nnmbox-current-group newsgroup))))

(defun nnmbox-article-string (article)
  (concat "\nX-Gnus-Newsgroup: " nnmbox-current-group ":" 
	  (int-to-string article) " "))

(defun nnmbox-save-mail (&optional group)
  "Called narrowed to an article."
  (let* ((nnmail-split-methods 
	  (if group (list (list group "")) nnmail-split-methods))
	 (group-art (nreverse (nnmail-article-group 'nnmbox-active-number))))
    (nnmail-insert-lines)
    (nnmail-insert-xref group-art)
    (nnmbox-insert-newsgroup-line group-art)
    (run-hooks 'nnml-prepare-save-mail-hook)
    group-art))

(defun nnmbox-insert-newsgroup-line (group-art)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "\n\n" nil t)
	(progn
	  (forward-char -1)
	  (while group-art
	    (insert (format "X-Gnus-Newsgroup: %s:%d   %s\n" 
			    (car (car group-art)) (cdr (car group-art))
			    (current-time-string)))
	    (setq group-art (cdr group-art)))))
    t))

(defun nnmbox-active-number (group)
  ;; Find the next article number in GROUP.
  (let ((active (car (cdr (assoc group nnmbox-group-alist)))))
    (if active
	(setcdr active (1+ (cdr active)))
      ;; This group is new, so we create a new entry for it.
      ;; This might be a bit naughty... creating groups on the drop of
      ;; a hat, but I don't know...
      (setq nnmbox-group-alist (cons (list group (setq active (cons 1 1)))
				     nnmbox-group-alist)))
    (cdr active)))

(defun nnmbox-read-mbox ()
  (nnmail-activate 'nnmbox)
  (if (not (file-exists-p nnmbox-mbox-file))
      (write-region 1 1 nnmbox-mbox-file t 'nomesg))
  (if (and nnmbox-mbox-buffer
	   (buffer-name nnmbox-mbox-buffer)
	   (save-excursion
	     (set-buffer nnmbox-mbox-buffer)
	     (= (buffer-size) (nth 7 (file-attributes nnmbox-mbox-file)))))
      ()
    (save-excursion
      (let ((delim (concat "^" rmail-unix-mail-delimiter))
	    start end)
	(set-buffer (setq nnmbox-mbox-buffer 
			  (nnheader-find-file-noselect 
			   nnmbox-mbox-file nil 'raw)))
	(buffer-disable-undo (current-buffer))
	(goto-char (point-min))
	(while (re-search-forward delim nil t)
	  (setq start (match-beginning 0))
	  (if (not (search-forward "\nX-Gnus-Newsgroup: " 
				   (save-excursion 
				     (setq end
					   (or
					    (and
					     (re-search-forward delim nil t)
					     (match-beginning 0))
					    (point-max))))
				   t))
	      (save-excursion
		(save-restriction
		  (narrow-to-region start end)
		  (nnmbox-save-mail))))
	  (goto-char end))))))

(defun nnmbox-get-new-mail (&optional group)
  "Read new incoming mail."
  (let* ((spools (nnmail-get-spool-files group))
	 (group-in group)
	 incoming incomings)
    (nnmbox-read-mbox)
    (if (or (not nnmbox-get-new-mail) (not nnmail-spool-file))
	()
      ;; We go through all the existing spool files and split the
      ;; mail from each.
      (while spools
	(and
	 (file-exists-p (car spools))
	 (> (nth 7 (file-attributes (car spools))) 0)
	 (progn
	   (and gnus-verbose-backends 
		(message "nnmbox: Reading incoming mail..."))
	   (if (not (setq incoming 
			  (nnmail-move-inbox 
			   (car spools) 
			   (concat nnmbox-mbox-file "-Incoming"))))
	       ()
	     (setq incomings (cons incoming incomings))
	     (save-excursion
	       (setq group (nnmail-get-split-group (car spools) group-in))
	       (let ((in-buf (nnmail-split-incoming 
			      incoming 'nnmbox-save-mail t group)))
		 (set-buffer nnmbox-mbox-buffer)
		 (goto-char (point-max))
		 (insert-buffer-substring in-buf)
		 (kill-buffer in-buf))))))
	(setq spools (cdr spools)))
      ;; If we did indeed read any incoming spools, we save all info. 
      (and (buffer-modified-p nnmbox-mbox-buffer) 
	   (save-excursion
	     (nnmail-save-active nnmbox-group-alist nnmbox-active-file)
	     (set-buffer nnmbox-mbox-buffer)
	     (save-buffer)))
      (if incomings (run-hooks 'nnmail-read-incoming-hook))
      (while incomings
	(setq incoming (car incomings))
	(and nnmail-delete-incoming
	     (file-exists-p incoming) 
	     (file-writable-p incoming) 
	     (delete-file incoming))
	(setq incomings (cdr incomings))))))


(provide 'nnmbox)

;;; nnmbox.el ends here
