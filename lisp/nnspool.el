;;; nnspool.el --- spool access for GNU Emacs
;; Copyright (C) 1988,89,90,93,94,95,96 Free Software Foundation, Inc.

;; Author: Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;; 	Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; Keywords: news

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
(require 'nntp)
(require 'timezone)
(require 'nnoo)
(eval-when-compile (require 'cl))

(nnoo-declare nnspool)

(defvoo nnspool-inews-program news-inews-program
  "Program to post news.
This is most commonly `inews' or `injnews'.")

(defvoo nnspool-inews-switches '("-h" "-S")
  "Switches for nnspool-request-post to pass to `inews' for posting news.
If you are using Cnews, you probably should set this variable to nil.")

(defvoo nnspool-spool-directory (file-name-as-directory news-path)
  "Local news spool directory.")

(defvoo nnspool-nov-directory (concat nnspool-spool-directory "over.view/")
  "Local news nov directory.")

(defvoo nnspool-lib-dir "/usr/lib/news/"
  "Where the local news library files are stored.")

(defvoo nnspool-active-file (concat nnspool-lib-dir "active")
  "Local news active file.")

(defvoo nnspool-newsgroups-file (concat nnspool-lib-dir "newsgroups")
  "Local news newsgroups file.")

(defvoo nnspool-distributions-file (concat nnspool-lib-dir "distribs.pat")
  "Local news distributions file.")

(defvoo nnspool-history-file (concat nnspool-lib-dir "history")
  "Local news history file.")

(defvoo nnspool-active-times-file (concat nnspool-lib-dir "active.times")
  "Local news active date file.")

(defvoo nnspool-large-newsgroup 50
  "The number of the articles which indicates a large newsgroup.
If the number of the articles is greater than the value, verbose
messages will be shown to indicate the current status.")

(defvoo nnspool-nov-is-evil nil
  "Non-nil means that nnspool will never return NOV lines instead of headers.")

(defconst nnspool-sift-nov-with-sed nil
  "If non-nil, use sed to get the relevant portion from the overview file.
If nil, nnspool will load the entire file into a buffer and process it
there.")

(defvoo nnspool-rejected-article-hook nil
  "*A hook that will be run when an article has been rejected by the server.")



(defconst nnspool-version "nnspool 2.0"
  "Version numbers of this version of NNSPOOL.")

(defvoo nnspool-current-directory nil
  "Current news group directory.")

(defvoo nnspool-current-group nil)
(defvoo nnspool-status-string "")


;;; Interface functions.

(nnoo-define-basics nnspool)

(deffoo nnspool-retrieve-headers (articles &optional group server fetch-old)
  "Retrieve the headers of ARTICLES."
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (when (nnspool-possibly-change-directory group)
      (let* ((number (length articles))
	     (count 0)
	     (default-directory nnspool-current-directory)
	     (do-message (and (numberp nnspool-large-newsgroup)
			      (> number nnspool-large-newsgroup)))
	     file beg article ag)
	(if (and (numberp (car articles))
		 (nnspool-retrieve-headers-with-nov articles fetch-old))
	    ;; We successfully retrieved the NOV headers.
	    'nov
	  ;; No NOV headers here, so we do it the hard way.
	  (while (setq article (pop articles))
	    (if (stringp article)
		;; This is a Message-ID.
		(setq ag (nnspool-find-id article)
		      file (and ag (nnspool-article-pathname 
				    (car ag) (cdr ag)))
		      article (cdr ag))
	      ;; This is an article in the current group.
	      (setq file (int-to-string article)))
	    ;; Insert the head of the article.
	    (when (and file
		       (file-exists-p file))
	      (insert "221 ")
	      (princ article (current-buffer))
	      (insert " Article retrieved.\n")
	      (setq beg (point))
	      (inline (nnheader-insert-head file))
	      (goto-char beg)
	      (search-forward "\n\n" nil t)
	      (forward-char -1)
	      (insert ".\n")
	      (delete-region (point) (point-max)))
	    
	    (and do-message
		 (zerop (% (incf count) 20))
		 (message "nnspool: Receiving headers... %d%%"
			  (/ (* count 100) number))))
	  
	  (and do-message
	       (message "nnspool: Receiving headers...done"))
	  
	  ;; Fold continuation lines.
	  (nnheader-fold-continuation-lines)
	  'headers)))))

(deffoo nnspool-open-server (server &optional defs)
  (nnoo-change-server 'nnspool server defs)
  (cond 
   ((not (file-exists-p nnspool-spool-directory))
    (nnspool-close-server)
    (nnheader-report 'nnspool "Spool directory doesn't exist: %s"
		     nnspool-spool-directory))
   ((not (file-directory-p
	  (directory-file-name
	   (file-truename nnspool-spool-directory))))
    (nnspool-close-server)
    (nnheader-report 'nnspool "Not a directory: %s" nnspool-spool-directory))
   ((not (file-exists-p nnspool-active-file))
    (nnheader-report 'nnspool "The active file doesn't exist: %s" 
		     nnspool-active-file))
   (t
    (nnheader-report 'nnspool "Opened server %s using directory %s"
		     server nnspool-spool-directory)
    t)))

(deffoo nnspool-request-article (id &optional group server buffer)
  "Select article by message ID (or number)."
  (nnspool-possibly-change-directory group)
  (let ((nntp-server-buffer (or buffer nntp-server-buffer))
	file ag)
    (if (stringp id)
	;; This is a Message-ID.	
	(when (setq ag (nnspool-find-id id))
	  (setq file (nnspool-article-pathname (car ag) (cdr ag))))
      (setq file (nnspool-article-pathname nnspool-current-group id)))
    (and file
	 (file-exists-p file)
	 (not (file-directory-p file))
	 (save-excursion (nnspool-find-file file))
	 ;; We return the article number and group name.
	 (if (numberp id)
	     (cons nnspool-current-group id)
	   ag))))
	    
(deffoo nnspool-request-body (id &optional group server)
  "Select article body by message ID (or number)."
  (nnspool-possibly-change-directory group)
  (let ((res (nnspool-request-article id)))
    (when res
      (save-excursion
	(set-buffer nntp-server-buffer)
	(goto-char (point-min))
	(when (search-forward "\n\n" nil t)
	  (delete-region (point-min) (point)))
	res))))

(deffoo nnspool-request-head (id &optional group server)
  "Select article head by message ID (or number)."
  (nnspool-possibly-change-directory group)
  (let ((res (nnspool-request-article id)))
    (when res
      (save-excursion
	(set-buffer nntp-server-buffer)
	(goto-char (point-min))
	(when (search-forward "\n\n" nil t)
	  (delete-region (1- (point)) (point-max)))
	(nnheader-fold-continuation-lines)))
    res))

(deffoo nnspool-request-group (group &optional server dont-check)
  "Select news GROUP."
  (let ((pathname (nnspool-article-pathname group))
	dir)
    (if (not (file-directory-p pathname))
	(nnheader-report 
	 'nnspool "Invalid group name (no such directory): %s" group)
      (setq nnspool-current-directory pathname)
      (nnheader-report 'nnspool "Selected group %s" group)
      (if dont-check
	  (progn
	    (nnheader-report 'nnspool "Selected group %s" group)
	    t)
	;; Yes, completely empty spool directories *are* possible.
	;; Fix by Sudish Joseph <joseph@cis.ohio-state.edu>
	(when (setq dir (directory-files pathname nil "^[0-9]+$" t))
	  (setq dir 
		(sort (mapcar (lambda (name) (string-to-int name)) dir) '<)))
	(if dir
	    (nnheader-insert
	     "211 %d %d %d %s\n" (length dir) (car dir)
	     (progn (while (cdr dir) (setq dir (cdr dir))) (car dir))
	     group)
	  (nnheader-report 'nnspool "Empty group %s" group)
	  (nnheader-insert "211 0 0 0 %s\n" group))))))

(deffoo nnspool-request-type (group &optional article)
  'news)

(deffoo nnspool-close-group (group &optional server)
  t)

(deffoo nnspool-request-list (&optional server)
  "List active newsgroups."
  (save-excursion
    (or (nnspool-find-file nnspool-active-file)
	(nnheader-report 'nnspool (nnheader-file-error nnspool-active-file)))))

(deffoo nnspool-request-list-newsgroups (&optional server)
  "List newsgroups (defined in NNTP2)."
  (save-excursion
    (or (nnspool-find-file nnspool-newsgroups-file)
	(nnheader-report 'nnspool (nnheader-file-error 
				   nnspool-newsgroups-file)))))

(deffoo nnspool-request-list-distributions (&optional server)
  "List distributions (defined in NNTP2)."
  (save-excursion
    (or (nnspool-find-file nnspool-distributions-file)
	(nnheader-report 'nnspool (nnheader-file-error 
				   nnspool-distributions-file)))))

;; Suggested by Hallvard B Furuseth <h.b.furuseth@usit.uio.no>.
(deffoo nnspool-request-newgroups (date &optional server)
  "List groups created after DATE."
  (if (nnspool-find-file nnspool-active-times-file)
      (save-excursion
	;; Find the last valid line.
	(goto-char (point-max))
	(while (and (not (looking-at 
			  "\\([^ ]+\\) +\\([0-9]+\\)[0-9][0-9][0-9] "))
		    (zerop (forward-line -1))))
	(let ((seconds (nnspool-seconds-since-epoch date))
	      groups)
	  ;; Go through lines and add the latest groups to a list.
	  (while (and (looking-at "\\([^ ]+\\) +[0-9]+ ")
		      (progn
			;; We insert a .0 to make the list reader
			;; interpret the number as a float. It is far
			;; too big to be stored in a lisp integer. 
			(goto-char (1- (match-end 0)))
			(insert ".0")
			(> (progn
			     (goto-char (match-end 1))
			     (read (current-buffer)))
			   seconds))
		      (setq groups (cons (buffer-substring
					  (match-beginning 1) (match-end 1))
					 groups))
		      (zerop (forward-line -1))))
	  (erase-buffer)
	  (while groups
	    (insert (car groups) " 0 0 y\n")
	    (setq groups (cdr groups))))
	t)
    nil))

(deffoo nnspool-request-post (&optional server)
  "Post a new news in current buffer."
  (save-excursion
    (let* ((process-connection-type nil) ; t bugs out on Solaris
	   (inews-buffer (generate-new-buffer " *nnspool post*"))
	   (proc 
	    (condition-case err
		(apply 'start-process "*nnspool inews*" inews-buffer
		       nnspool-inews-program nnspool-inews-switches)
	      (error
	       (nnheader-report 'nnspool "inews error: %S" err)))))
      (if (not proc)
	  ;; The inews program failed.
	  ()
	(nnheader-report 'nnspool "")
	(set-process-sentinel proc 'nnspool-inews-sentinel)
	(process-send-region proc (point-min) (point-max))
	;; We slap a condition-case around this, because the process may
	;; have exited already...
	(condition-case nil
	    (process-send-eof proc)
	  (error nil))
	t))))



;;; Internal functions.

(defun nnspool-inews-sentinel (proc status)
  (save-excursion
    (set-buffer (process-buffer proc))
    (goto-char (point-min))
    (if (or (zerop (buffer-size))
	    (search-forward "spooled" nil t))
	(kill-buffer (current-buffer))
      ;; Make status message by folding lines.
      (while (re-search-forward "[ \t\n]+" nil t)
	(replace-match " " t t))
      (nnheader-report 'nnspool "%s" (buffer-string))
      (message "nnspool: %s" nnspool-status-string)
      (ding)
      (run-hooks 'nnspool-rejected-article-hook))))

(defun nnspool-retrieve-headers-with-nov (articles &optional fetch-old)
  (if (or gnus-nov-is-evil nnspool-nov-is-evil)
      nil
    (let ((nov (nnheader-group-pathname 
		nnspool-current-group nnspool-nov-directory ".overview"))
	  (arts articles)
	  last)
      (if (not (file-exists-p nov))
	  ()
	(save-excursion
	  (set-buffer nntp-server-buffer)
	  (erase-buffer)
	  (if nnspool-sift-nov-with-sed
	      (nnspool-sift-nov-with-sed articles nov)
	    (insert-file-contents nov)
	    (if (and fetch-old
		     (not (numberp fetch-old)))
		t			; We want all the headers.
	      (condition-case ()
		  (progn
		    ;; First we find the first wanted line.
		    (nnspool-find-nov-line
		     (if fetch-old (max 1 (- (car articles) fetch-old))
		       (car articles)))
		    (delete-region (point-min) (point))
		    ;; Then we find the last wanted line. 
		    (if (nnspool-find-nov-line 
			 (progn (while (cdr articles)
				  (setq articles (cdr articles)))
				(car articles)))
			(forward-line 1))
		    (delete-region (point) (point-max))
		    ;; If the buffer is empty, this wasn't very successful.
		    (unless (zerop (buffer-size))
		      ;; We check what the last article number was.  
		      ;; The NOV file may be out of sync with the articles
		      ;; in the group.
		      (forward-line -1)
		      (setq last (read (current-buffer)))
		      (if (= last (car articles))
			  ;; Yup, it's all there.
			  t
			;; Perhaps not.  We try to find the missing articles.
			(while (and arts
				    (<= last (car arts)))
			  (pop arts))
			;; The articles in `arts' are missing from the buffer.
			(while arts
			  (nnspool-insert-nov-head (pop arts)))
			t)))
		;; The NOV file was corrupted.
		(error nil)))))))))

(defun nnspool-insert-nov-head (article)
  "Read the head of ARTICLE, convert to NOV headers, and insert."
  (save-excursion
    (let ((cur (current-buffer))
	  buf)
      (setq buf (nnheader-set-temp-buffer " *nnspool head*"))
      (when (nnheader-insert-head
	     (nnspool-article-pathname nnspool-current-group article))
	(nnheader-insert-article-line article)
	(let ((headers (nnheader-parse-head)))
	  (set-buffer cur)
	  (goto-char (point-max))
	  (nnheader-insert-nov headers)))
      (kill-buffer buf))))

(defun nnspool-find-nov-line (article)
  (let ((max (point-max))
	(min (goto-char (point-min)))
	(cur (current-buffer))
	(prev (point-min))
	num found)
    (while (not found)
      (goto-char (/ (+ max min) 2))
      (beginning-of-line)
      (if (or (= (point) prev)
	      (eobp))
	  (setq found t)
	(setq prev (point))
	(cond ((> (setq num (read cur)) article)
	       (setq max (point)))
	      ((< num article)
	       (setq min (point)))
	      (t
	       (setq found 'yes)))))
    ;; Now we may have found the article we're looking for, or we
    ;; may be somewhere near it.
    (when (and (not (eq found 'yes))
	       (not (eq num article)))
      (setq found (point))
      (while (and (< (point) max)
		  (or (not (numberp num))
		      (< num article)))
	(forward-line 1)
	(setq found (point))
	(or (eobp)
	    (= (setq num (read cur)) article)))
      (unless (eq num article)
	(goto-char found)))
    (beginning-of-line)
    (eq num article)))
    
(defun nnspool-sift-nov-with-sed (articles file)
  (let ((first (car articles))
	(last (progn (while (cdr articles) (setq articles (cdr articles)))
		     (car articles))))
    (call-process "awk" nil t nil 
		  (format "BEGIN {firstmsg=%d; lastmsg=%d;}\n $1 >= firstmsg && $1 <= lastmsg {print;}"
			  (1- first) (1+ last))
		  file)))

;; Fixed by fdc@cliwe.ping.de (Frank D. Cringle). 
;; Find out what group an article identified by a Message-ID is in.
(defun nnspool-find-id (id)
  (save-excursion
    (set-buffer (get-buffer-create " *nnspool work*"))
    (buffer-disable-undo (current-buffer))
    (erase-buffer)
    (condition-case ()
	(call-process "grep" nil t nil (regexp-quote id) nnspool-history-file)
      (error nil))
    (goto-char (point-min))
    (prog1
	(if (looking-at "<[^>]+>[ \t]+[-0-9~]+[ \t]+\\([^ /\t\n]+\\)/\\([0-9]+\\)[ \t\n]")
	    (cons (match-string 1) (string-to-int (match-string 2))))
      (kill-buffer (current-buffer)))))

(defun nnspool-find-file (file)
  "Insert FILE in server buffer safely."
  (set-buffer nntp-server-buffer)
  (erase-buffer)
  (condition-case ()
      (progn (nnheader-insert-file-contents-literally file) t)
    (file-error nil)))

(defun nnspool-possibly-change-directory (group)
  (if (not group)
      t
    (let ((pathname (nnspool-article-pathname group)))
      (if (file-directory-p pathname)
	  (setq nnspool-current-directory pathname
		nnspool-current-group group)
	(nnheader-report 'nnspool "No such newsgroup: %s" group)))))

(defun nnspool-article-pathname (group &optional article)
  "Find the path for GROUP."
  (nnheader-group-pathname group nnspool-spool-directory article))

(defun nnspool-seconds-since-epoch (date)
  (let* ((tdate (mapcar (lambda (ti) (and ti (string-to-int ti)))
			(timezone-parse-date date)))
	 (ttime (mapcar (lambda (ti) (and ti (string-to-int ti)))
			(timezone-parse-time
			 (aref (timezone-parse-date date) 3))))
	 (unix (encode-time (nth 2 ttime) (nth 1 ttime) (nth 0 ttime)
			    (nth 2 tdate) (nth 1 tdate) (nth 0 tdate) 
			    (nth 4 tdate))))
    (+ (* (car unix) 65536.0)
       (cadr unix))))

(provide 'nnspool)

;;; nnspool.el ends here
