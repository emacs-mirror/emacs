;;; nnwfm.el --- interfacing with a web forum

;; Copyright (C) 2000, 2002, 2003, 2004, 2005,
;;   2006, 2007 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Note: You need to have `url' and `w3' installed for this
;; backend to work.

;;; Code:

(eval-when-compile (require 'cl))

(require 'nnoo)
(require 'message)
(require 'gnus-util)
(require 'gnus)
(require 'nnmail)
(require 'mm-util)
(require 'mm-url)
(require 'nnweb)
(autoload 'w3-parse-buffer "w3-parse")

(nnoo-declare nnwfm)

(defvoo nnwfm-directory (nnheader-concat gnus-directory "wfm/")
  "Where nnwfm will save its files.")

(defvoo nnwfm-address ""
  "The address of the Ultimate bulletin board.")

;;; Internal variables

(defvar nnwfm-groups-alist nil)
(defvoo nnwfm-groups nil)
(defvoo nnwfm-headers nil)
(defvoo nnwfm-articles nil)
(defvar nnwfm-table-regexp
  "postings.*editpost\\|forumdisplay\\|Forum[0-9]+/HTML\\|getbio")

;;; Interface functions

(nnoo-define-basics nnwfm)

(deffoo nnwfm-retrieve-headers (articles &optional group server fetch-old)
  (nnwfm-possibly-change-server group server)
  (unless gnus-nov-is-evil
    (let* ((last (car (last articles)))
	   (did nil)
	   (start 1)
	   (entry (assoc group nnwfm-groups))
	   (sid (nth 2 entry))
	   (topics (nth 4 entry))
	   (mapping (nth 5 entry))
	   (old-total (or (nth 6 entry) 1))
	   (nnwfm-table-regexp "Thread.asp")
	   headers article subject score from date lines parent point
	   contents tinfo fetchers map elem a href garticles topic old-max
	   inc datel table string current-page total-contents pages
	   farticles forum-contents parse furl-fetched mmap farticle
	   thread-id tables hstuff bstuff time)
      (setq map mapping)
      (while (and (setq article (car articles))
		  map)
	(while (and map
		    (or (> article (caar map))
			(< (cadar map) (caar map))))
	  (pop map))
	(when (setq mmap (car map))
	  (setq farticle -1)
	  (while (and article
		      (<= article (nth 1 mmap)))
	    ;; Do we already have a fetcher for this topic?
	    (if (setq elem (assq (nth 2 mmap) fetchers))
		;; Yes, so we just add the spec to the end.
		(nconc elem (list (cons article
					(+ (nth 3 mmap) (incf farticle)))))
	      ;; No, so we add a new one.
	      (push (list (nth 2 mmap)
			  (cons article
				(+ (nth 3 mmap) (incf farticle))))
		    fetchers))
	    (pop articles)
	    (setq article (car articles)))))
      ;; Now we have the mapping from/to Gnus/nnwfm article numbers,
      ;; so we start fetching the topics that we need to satisfy the
      ;; request.
      (if (not fetchers)
	  (save-excursion
	    (set-buffer nntp-server-buffer)
	    (erase-buffer))
	(setq nnwfm-articles nil)
	(mm-with-unibyte-buffer
	  (dolist (elem fetchers)
	    (erase-buffer)
	    (setq subject (nth 2 (assq (car elem) topics))
		  thread-id (nth 0 (assq (car elem) topics)))
	    (mm-url-insert
	     (concat nnwfm-address
		     (format "Item.asp?GroupID=%d&ThreadID=%d" sid
			     thread-id)))
	    (goto-char (point-min))
	    (setq tables (caddar
			  (caddar
			   (cdr (caddar
				 (caddar
				  (ignore-errors
				    (w3-parse-buffer (current-buffer)))))))))
	    (setq tables (cdr (caddar (memq (assq 'div tables) tables))))
	    (setq contents nil)
	    (dolist (table tables)
	      (when (eq (car table) 'table)
		(setq table (caddar (caddar (caddr table)))
		      hstuff (delete ":link" (nnweb-text (car table)))
		      bstuff (car (caddar (cdr table)))
		      from (car hstuff))
		(when (nth 2 hstuff)
		  (setq time (nnwfm-date-to-time (nth 2 hstuff)))
		  (push (list from time bstuff) contents))))
	    (setq contents (nreverse contents))
	    (dolist (art (cdr elem))
		(push (list (car art)
			    (nth (1- (cdr art)) contents)
			    subject)
		      nnwfm-articles))))
	(setq nnwfm-articles
	      (sort nnwfm-articles 'car-less-than-car))
	;; Now we have all the articles, conveniently in an alist
	;; where the key is the Gnus article number.
	(dolist (articlef nnwfm-articles)
	  (setq article (nth 0 articlef)
		contents (nth 1 articlef)
		subject (nth 2 articlef))
	  (setq from (nth 0 contents)
		date (message-make-date (nth 1 contents)))
	  (push
	   (cons
	    article
	    (make-full-mail-header
	     article subject
	     from (or date "")
	     (concat "<" (number-to-string sid) "%"
		     (number-to-string article)
		     "@wfm>")
	     "" 0
	     (/ (length (mapconcat 'identity (nnweb-text (nth 2 contents)) ""))
		70)
	     nil nil))
	   headers))
	(setq nnwfm-headers (sort headers 'car-less-than-car))
	(save-excursion
	  (set-buffer nntp-server-buffer)
	  (mm-with-unibyte-current-buffer
	    (erase-buffer)
	    (dolist (header nnwfm-headers)
	      (nnheader-insert-nov (cdr header))))))
      'nov)))

(deffoo nnwfm-request-group (group &optional server dont-check)
  (nnwfm-possibly-change-server nil server)
  (when (not nnwfm-groups)
    (nnwfm-request-list))
  (unless dont-check
    (nnwfm-create-mapping group))
  (let ((elem (assoc group nnwfm-groups)))
    (cond
     ((not elem)
      (nnheader-report 'nnwfm "Group does not exist"))
     (t
      (nnheader-report 'nnwfm "Opened group %s" group)
      (nnheader-insert
       "211 %d %d %d %s\n" (cadr elem) 1 (cadr elem)
       (prin1-to-string group))))))

(deffoo nnwfm-request-close ()
  (setq nnwfm-groups-alist nil
	nnwfm-groups nil))

(deffoo nnwfm-request-article (article &optional group server buffer)
  (nnwfm-possibly-change-server group server)
  (let ((contents (cdr (assq article nnwfm-articles))))
    (when (setq contents (nth 2 (car contents)))
      (save-excursion
	(set-buffer (or buffer nntp-server-buffer))
	(erase-buffer)
	(nnweb-insert-html contents)
	(goto-char (point-min))
	(insert "Content-Type: text/html\nMIME-Version: 1.0\n")
	(let ((header (cdr (assq article nnwfm-headers))))
	  (mm-with-unibyte-current-buffer
	    (nnheader-insert-header header)))
	(nnheader-report 'nnwfm "Fetched article %s" article)
	(cons group article)))))

(deffoo nnwfm-request-list (&optional server)
  (nnwfm-possibly-change-server nil server)
  (mm-with-unibyte-buffer
    (mm-url-insert
     (if (string-match "/$" nnwfm-address)
	 (concat nnwfm-address "Group.asp")
       nnwfm-address))
    (let* ((nnwfm-table-regexp "Thread.asp")
	   (contents (w3-parse-buffer (current-buffer)))
	   sid elem description articles a href group forum
	   a1 a2)
      (dolist (row (cdr (nth 2 (car (nth 2 (nnwfm-find-forum-table
					    contents))))))
	(setq row (nth 2 row))
	(when (setq a (nnweb-parse-find 'a row))
	  (setq group (car (last (nnweb-text a)))
		href (cdr (assq 'href (nth 1 a))))
	  (setq description (car (last (nnweb-text (nth 1 row)))))
	  (setq articles
		(string-to-number
		 (gnus-replace-in-string
		  (car (last (nnweb-text (nth 3 row)))) "," "")))
	  (when (and href
		     (string-match "GroupId=\\([0-9]+\\)" href))
	    (setq forum (string-to-number (match-string 1 href)))
	    (if (setq elem (assoc group nnwfm-groups))
		(setcar (cdr elem) articles)
	      (push (list group articles forum description nil nil nil nil)
		    nnwfm-groups))))))
    (nnwfm-write-groups)
    (nnwfm-generate-active)
    t))

(deffoo nnwfm-request-newgroups (date &optional server)
  (nnwfm-possibly-change-server nil server)
  (nnwfm-generate-active)
  t)

(nnoo-define-skeleton nnwfm)

;;; Internal functions

(defun nnwfm-new-threads-p (group time)
  "See whether we want to fetch the threads for GROUP written before TIME."
  (let ((old-time (nth 7 (assoc group nnwfm-groups))))
    (or (null old-time)
	(time-less-p old-time time))))

(defun nnwfm-create-mapping (group)
  (let* ((entry (assoc group nnwfm-groups))
	 (sid (nth 2 entry))
	 (topics (nth 4 entry))
	 (mapping (nth 5 entry))
	 (old-total (or (nth 6 entry) 1))
	 (current-time (current-time))
	 (nnwfm-table-regexp "Thread.asp")
	 (furls (list (concat nnwfm-address
			      (format "Thread.asp?GroupId=%d" sid))))
	 fetched-urls
	 contents forum-contents a subject href
	 garticles topic tinfo old-max inc parse elem date
	 url time)
    (mm-with-unibyte-buffer
      (while furls
	(erase-buffer)
	(push (car furls) fetched-urls)
	(mm-url-insert (pop furls))
	(goto-char (point-min))
	(while (re-search-forward "  wr(" nil t)
	  (forward-char -1)
	  (setq elem (message-tokenize-header
		      (gnus-replace-in-string
		       (buffer-substring
			(1+ (point))
			(progn
			  (forward-sexp 1)
			  (1- (point))))
		       "\\\\[\"\\\\]" "")))
	  (push (list
		 (string-to-number (nth 1 elem))
		 (gnus-replace-in-string (nth 2 elem) "\"" "")
		 (string-to-number (nth 5 elem)))
		forum-contents))
	(when (re-search-forward "href=\"\\(Thread.*DateLast=\\([^\"]+\\)\\)"
				 nil t)
	  (setq url (match-string 1)
		time (nnwfm-date-to-time (gnus-url-unhex-string
					  (match-string 2))))
	  (when (and (nnwfm-new-threads-p group time)
		     (not (member
			   (setq url (concat
				      nnwfm-address
				      (mm-url-decode-entities-string url)))
			   fetched-urls)))
	    (push url furls))))
      ;; The main idea here is to map Gnus article numbers to
      ;; nnwfm article numbers.  Say there are three topics in
      ;; this forum, the first with 4 articles, the seconds with 2,
      ;; and the third with 1.  Then this will translate into 7 Gnus
      ;; article numbers, where 1-4 comes from the first topic, 5-6
      ;; from the second and 7 from the third.  Now, then next time
      ;; the group is entered, there's 2 new articles in topic one
      ;; and 1 in topic three.  Then Gnus article number 8-9 be 5-6
      ;; in topic one and 10 will be the 2 in topic three.
      (dolist (elem (nreverse forum-contents))
	(setq subject (nth 1 elem)
	      topic (nth 0 elem)
	      garticles (nth 2 elem))
	(if (setq tinfo (assq topic topics))
	    (progn
	      (setq old-max (cadr tinfo))
	      (setcar (cdr tinfo) garticles))
	  (setq old-max 0)
	  (push (list topic garticles subject) topics)
	  (setcar (nthcdr 4 entry) topics))
	(when (not (= old-max garticles))
	  (setq inc (- garticles old-max))
	  (setq mapping (nconc mapping
			       (list
				(list
				 old-total (1- (incf old-total inc))
				 topic (1+ old-max)))))
	  (incf old-max inc)
	  (setcar (nthcdr 5 entry) mapping)
	  (setcar (nthcdr 6 entry) old-total))))
    (setcar (nthcdr 7 entry) current-time)
    (setcar (nthcdr 1 entry) (1- old-total))
    (nnwfm-write-groups)
    mapping))

(defun nnwfm-possibly-change-server (&optional group server)
  (nnwfm-init server)
  (when (and server
	     (not (nnwfm-server-opened server)))
    (nnwfm-open-server server))
  (unless nnwfm-groups-alist
    (nnwfm-read-groups)
    (setq nnwfm-groups (cdr (assoc nnwfm-address
					nnwfm-groups-alist)))))

(deffoo nnwfm-open-server (server &optional defs connectionless)
  (nnheader-init-server-buffer)
  (if (nnwfm-server-opened server)
      t
    (unless (assq 'nnwfm-address defs)
      (setq defs (append defs (list (list 'nnwfm-address server)))))
    (nnoo-change-server 'nnwfm server defs)))

(defun nnwfm-read-groups ()
  (setq nnwfm-groups-alist nil)
  (let ((file (expand-file-name "groups" nnwfm-directory)))
    (when (file-exists-p file)
      (mm-with-unibyte-buffer
	(insert-file-contents file)
	(goto-char (point-min))
	(setq nnwfm-groups-alist (read (current-buffer)))))))

(defun nnwfm-write-groups ()
  (setq nnwfm-groups-alist
	(delq (assoc nnwfm-address nnwfm-groups-alist)
	      nnwfm-groups-alist))
  (push (cons nnwfm-address nnwfm-groups)
	nnwfm-groups-alist)
  (with-temp-file (expand-file-name "groups" nnwfm-directory)
    (prin1 nnwfm-groups-alist (current-buffer))))

(defun nnwfm-init (server)
  "Initialize buffers and such."
  (unless (file-exists-p nnwfm-directory)
    (gnus-make-directory nnwfm-directory)))

(defun nnwfm-generate-active ()
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (dolist (elem nnwfm-groups)
      (insert (prin1-to-string (car elem))
	      " " (number-to-string (cadr elem)) " 1 y\n"))))

(defun nnwfm-find-forum-table (contents)
  (catch 'found
    (nnwfm-find-forum-table-1 contents)))

(defun nnwfm-find-forum-table-1 (contents)
  (dolist (element contents)
    (unless (stringp element)
      (when (and (eq (car element) 'table)
		 (nnwfm-forum-table-p element))
	(throw 'found element))
      (when (nth 2 element)
	(nnwfm-find-forum-table-1 (nth 2 element))))))

(defun nnwfm-forum-table-p (parse)
  (when (not (apply 'gnus-or
		    (mapcar
		     (lambda (p)
		       (nnweb-parse-find 'table p))
		     (nth 2 parse))))
    (let ((href (cdr (assq 'href (nth 1 (nnweb-parse-find 'a parse 20)))))
	  case-fold-search)
      (when (and href (string-match nnwfm-table-regexp href))
	t))))

(defun nnwfm-date-to-time (date)
  (let ((time (mapcar #'string-to-number (split-string date "[\\.\\+ :]"))))
    (encode-time 0 (nth 4 time) (nth 3 time)
		 (nth 0 time) (nth 1 time)
		 (if (< (nth 2 time) 70)
		     (+ 2000 (nth 2 time))
		   (+ 1900 (nth 2 time))))))

(provide 'nnwfm)

;; Local Variables:
;; coding: iso-8859-1
;; End:

;;; arch-tag: d813966a-4211-4557-ad11-d1ac2bc86536
;;; nnwfm.el ends here
