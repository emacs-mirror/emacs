;;; nnweb.el --- retrieving articles via web search engines
;; Copyright (C) 1996, 1997, 1998, 1999, 2000
;;        Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

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
(eval-when-compile
  (ignore-errors
    (require 'w3)
    (require 'url)
    (require 'w3-forms)))

;; Report failure to find w3 at load time if appropriate.
(unless noninteractive
  (eval '(progn
	   (require 'w3)
	   (require 'url)
	   (require 'w3-forms))))

(nnoo-declare nnweb)

(defvoo nnweb-directory (nnheader-concat gnus-directory "nnweb/")
  "Where nnweb will save its files.")

(defvoo nnweb-type 'dejanews
  "What search engine type is being used.
Valid types include `dejanews', `dejanewsold', `reference',
and `altavista'.")

(defvar nnweb-type-definition
  '((dejanews
     (article . ignore)
     (id . "http://search.dejanews.com/msgid.xp?MID=%s&fmt=text")
     (map . nnweb-dejanews-create-mapping)
     (search . nnweb-dejanews-search)
     (address . "http://www.deja.com/=dnc/qs.xp")
     (identifier . nnweb-dejanews-identity))
    (dejanewsold
     (article . ignore)
     (map . nnweb-dejanews-create-mapping)
     (search . nnweb-dejanewsold-search)
     (address . "http://www.deja.com/dnquery.xp")
     (identifier . nnweb-dejanews-identity))
    (reference
     (article . nnweb-reference-wash-article)
     (map . nnweb-reference-create-mapping)
     (search . nnweb-reference-search)
     (address . "http://www.reference.com/cgi-bin/pn/go")
     (identifier . identity))
    (altavista
     (article . nnweb-altavista-wash-article)
     (map . nnweb-altavista-create-mapping)
     (search . nnweb-altavista-search)
     (address . "http://www.altavista.digital.com/cgi-bin/query")
     (id . "/cgi-bin/news?id@%s")
     (identifier . identity)))
  "Type-definition alist.")

(defvoo nnweb-search nil
  "Search string to feed to DejaNews.")

(defvoo nnweb-max-hits 999
  "Maximum number of hits to display.")

(defvoo nnweb-ephemeral-p nil
  "Whether this nnweb server is ephemeral.")

;;; Internal variables

(defvoo nnweb-articles nil)
(defvoo nnweb-buffer nil)
(defvoo nnweb-group-alist nil)
(defvoo nnweb-group nil)
(defvoo nnweb-hashtb nil)

;;; Interface functions

(nnoo-define-basics nnweb)

(deffoo nnweb-retrieve-headers (articles &optional group server fetch-old)
  (nnweb-possibly-change-server group server)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (let (article header)
      (mm-with-unibyte-current-buffer
	(while (setq article (pop articles))
	  (when (setq header (cadr (assq article nnweb-articles)))
	    (nnheader-insert-nov header))))
      'nov)))

(deffoo nnweb-request-scan (&optional group server)
  (nnweb-possibly-change-server group server)
  (funcall (nnweb-definition 'map))
  (unless nnweb-ephemeral-p
    (nnweb-write-active)
    (nnweb-write-overview group)))

(deffoo nnweb-request-group (group &optional server dont-check)
  (nnweb-possibly-change-server nil server)
  (when (and group
	     (not (equal group nnweb-group))
	     (not nnweb-ephemeral-p))
    (let ((info (assoc group nnweb-group-alist)))
      (when info
	(setq nnweb-group group)
	(setq nnweb-type (nth 2 info))
	(setq nnweb-search (nth 3 info))
	(unless dont-check
	  (nnweb-read-overview group)))))
  (cond
   ((not nnweb-articles)
    (nnheader-report 'nnweb "No matching articles"))
   (t
    (let ((active (if nnweb-ephemeral-p
		      (cons (caar nnweb-articles)
			    (caar (last nnweb-articles)))
		    (cadr (assoc group nnweb-group-alist)))))
      (nnheader-report 'nnweb "Opened group %s" group)
      (nnheader-insert
       "211 %d %d %d %s\n" (length nnweb-articles)
       (car active) (cdr active) group)))))

(deffoo nnweb-close-group (group &optional server)
  (nnweb-possibly-change-server group server)
  (when (gnus-buffer-live-p nnweb-buffer)
    (save-excursion
      (set-buffer nnweb-buffer)
      (set-buffer-modified-p nil)
      (kill-buffer nnweb-buffer)))
  t)

(deffoo nnweb-request-article (article &optional group server buffer)
  (nnweb-possibly-change-server group server)
  (save-excursion
    (set-buffer (or buffer nntp-server-buffer))
    (let* ((header (cadr (assq article nnweb-articles)))
	   (url (and header (mail-header-xref header))))
      (when (or (and url
		     (mm-with-unibyte-current-buffer
		       (nnweb-fetch-url url)))
		(and (stringp article)
		     (nnweb-definition 'id t)
		     (let ((fetch (nnweb-definition 'id))
			   art)
		       (when (string-match "^<\\(.*\\)>$" article)
			 (setq art (match-string 1 article)))
		       (and fetch
			    art
			    (mm-with-unibyte-current-buffer
			      (nnweb-fetch-url
			       (format fetch article)))))))
	(unless nnheader-callback-function
	  (funcall (nnweb-definition 'article))
	  (nnweb-decode-entities))
	(nnheader-report 'nnweb "Fetched article %s" article)
	(cons group (and (numberp article) article))))))

(deffoo nnweb-close-server (&optional server)
  (when (and (nnweb-server-opened server)
	     (gnus-buffer-live-p nnweb-buffer))
    (save-excursion
      (set-buffer nnweb-buffer)
      (set-buffer-modified-p nil)
      (kill-buffer nnweb-buffer)))
  (nnoo-close-server 'nnweb server))

(deffoo nnweb-request-list (&optional server)
  (nnweb-possibly-change-server nil server)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (nnmail-generate-active nnweb-group-alist)
    t))

(deffoo nnweb-request-update-info (group info &optional server)
  (nnweb-possibly-change-server group server))

(deffoo nnweb-asynchronous-p ()
  t)

(deffoo nnweb-request-create-group (group &optional server args)
  (nnweb-possibly-change-server nil server)
  (nnweb-request-delete-group group)
  (push `(,group ,(cons 1 0) ,@args) nnweb-group-alist)
  (nnweb-write-active)
  t)

(deffoo nnweb-request-delete-group (group &optional force server)
  (nnweb-possibly-change-server group server)
  (gnus-pull group nnweb-group-alist t)
  (nnweb-write-active)
  (gnus-delete-file (nnweb-overview-file group))
  t)

(nnoo-define-skeleton nnweb)

;;; Internal functions

(defun nnweb-read-overview (group)
  "Read the overview of GROUP and build the map."
  (when (file-exists-p (nnweb-overview-file group))
    (mm-with-unibyte-buffer
      (nnheader-insert-file-contents (nnweb-overview-file group))
      (goto-char (point-min))
      (let (header)
	(while (not (eobp))
	  (setq header (nnheader-parse-nov))
	  (forward-line 1)
	  (push (list (mail-header-number header)
		      header (mail-header-xref header))
		nnweb-articles)
	  (nnweb-set-hashtb header (car nnweb-articles)))))))

(defun nnweb-write-overview (group)
  "Write the overview file for GROUP."
  (with-temp-file (nnweb-overview-file group)
    (let ((articles nnweb-articles))
      (while articles
	(nnheader-insert-nov (cadr (pop articles)))))))

(defun nnweb-set-hashtb (header data)
  (gnus-sethash (nnweb-identifier (mail-header-xref header))
		data nnweb-hashtb))

(defun nnweb-get-hashtb (url)
  (gnus-gethash (nnweb-identifier url) nnweb-hashtb))

(defun nnweb-identifier (ident)
  (funcall (nnweb-definition 'identifier) ident))

(defun nnweb-overview-file (group)
  "Return the name of the overview file of GROUP."
  (nnheader-concat nnweb-directory group ".overview"))

(defun nnweb-write-active ()
  "Save the active file."
  (gnus-make-directory nnweb-directory)
  (with-temp-file (nnheader-concat nnweb-directory "active")
    (prin1 `(setq nnweb-group-alist ',nnweb-group-alist) (current-buffer))))

(defun nnweb-read-active ()
  "Read the active file."
  (load (nnheader-concat nnweb-directory "active") t t t))

(defun nnweb-definition (type &optional noerror)
  "Return the definition of TYPE."
  (let ((def (cdr (assq type (assq nnweb-type nnweb-type-definition)))))
    (when (and (not def)
	       (not noerror))
      (error "Undefined definition %s" type))
    def))

(defun nnweb-possibly-change-server (&optional group server)
  (nnweb-init server)
  (when server
    (unless (nnweb-server-opened server)
      (nnweb-open-server server)))
  (unless nnweb-group-alist
    (nnweb-read-active))
  (when group
    (when (and (not nnweb-ephemeral-p)
	       (not (equal group nnweb-group)))
      (setq nnweb-hashtb (gnus-make-hashtable 4095))
      (nnweb-request-group group nil t))))

(defun nnweb-init (server)
  "Initialize buffers and such."
  (unless (gnus-buffer-live-p nnweb-buffer)
    (setq nnweb-buffer
	  (save-excursion
	    (mm-with-unibyte
	      (nnheader-set-temp-buffer
	       (format " *nnweb %s %s %s*"
		       nnweb-type nnweb-search server))
	      (current-buffer))))))

(defun nnweb-fetch-url (url)
  (let (buf)
    (save-excursion
      (if (not nnheader-callback-function)
	  (progn
	    (with-temp-buffer
	      (mm-enable-multibyte)
	      (let ((coding-system-for-read 'binary)
		    (coding-system-for-write 'binary)
		    (default-process-coding-system 'binary))
		(nnweb-insert url))
	      (setq buf (buffer-string)))
	    (erase-buffer)
	    (insert buf)
	    t)
	(nnweb-url-retrieve-asynch
	 url 'nnweb-callback (current-buffer) nnheader-callback-function)
	t))))

(defun nnweb-callback (buffer callback)
  (when (gnus-buffer-live-p url-working-buffer)
    (save-excursion
      (set-buffer url-working-buffer)
      (funcall (nnweb-definition 'article))
      (nnweb-decode-entities)
      (set-buffer buffer)
      (goto-char (point-max))
      (insert-buffer-substring url-working-buffer))
    (funcall callback t)
    (gnus-kill-buffer url-working-buffer)))

(defun nnweb-url-retrieve-asynch (url callback &rest data)
  (let ((url-request-method "GET")
	(old-asynch url-be-asynchronous)
	(url-request-data nil)
	(url-request-extra-headers nil)
	(url-working-buffer (generate-new-buffer-name " *nnweb*")))
    (setq-default url-be-asynchronous t)
    (save-excursion
      (set-buffer (get-buffer-create url-working-buffer))
      (setq url-current-callback-data data
	    url-be-asynchronous t
	    url-current-callback-func callback)
      (url-retrieve url nil))
    (setq-default url-be-asynchronous old-asynch)))

(if (fboundp 'url-retrieve-synchronously)
    (defun nnweb-url-retrieve-asynch (url callback &rest data)
      (url-retrieve url callback data)))

;;;
;;; DejaNews functions.
;;;

(defun nnweb-dejanews-create-mapping ()
  "Perform the search and create an number-to-url alist."
  (save-excursion
    (set-buffer nnweb-buffer)
    (erase-buffer)
    (when (funcall (nnweb-definition 'search) nnweb-search)
      (let ((i 0)
	    (more t)
	    (case-fold-search t)
	    (active (or (cadr (assoc nnweb-group nnweb-group-alist))
			(cons 1 0)))
	    subject date from
	    map url parse a table group text)
	(while more
	  ;; Go through all the article hits on this page.
	  (goto-char (point-min))
	  (setq parse (w3-parse-buffer (current-buffer))
		table (nth 1 (nnweb-parse-find-all 'table parse)))
	  (dolist (row (nth 2 (car (nth 2 table))))
	    (setq a (nnweb-parse-find 'a row)
		  url (cdr (assq 'href (nth 1 a)))
		  text (nreverse (nnweb-text row)))
	    (when a
	      (setq subject (nth 4 text)
		    group (nth 2 text)
		    date (nth 1 text)
		    from (nth 0 text))
	      (if (string-match "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)" date)
		  (setq date (format "%s %s 00:00:00 %s"
				     (car (rassq (string-to-number
						  (match-string 2 date))
						 parse-time-months))
				     (match-string 3 date) 
				     (match-string 1 date)))
		(setq date "Jan 1 00:00:00 0000"))
	      (incf i)
	      (setq url (concat url "&fmt=text"))
	      (when (string-match "&context=[^&]+" url)
		(setq url (replace-match "" t t url)))
	      (unless (nnweb-get-hashtb url)
		(push
		 (list
		  (incf (cdr active))
		  (make-full-mail-header
		   (cdr active) (concat subject " (" group ")") from date
		   (concat "<" (nnweb-identifier url) "@dejanews>")
		   nil 0 0 url))
		 map)
		(nnweb-set-hashtb (cadar map) (car map)))))
	  ;; See whether there is a "Get next 20 hits" button here.
	  (goto-char (point-min))
	  (if (or (not (re-search-forward
			"HREF=\"\\([^\"]+\\)\"[<>b]+Next result" nil t))
		  (>= i nnweb-max-hits))
	      (setq more nil)
	    ;; Yup -- fetch it.
	    (setq more (match-string 1))
	    (erase-buffer)
	    (url-insert-file-contents more)))
	;; Return the articles in the right order.
	(setq nnweb-articles
	      (sort (nconc nnweb-articles map) 'car-less-than-car))))))

(defun nnweb-dejanews-search (search)
  (nnweb-insert
   (concat
    (nnweb-definition 'address)
    "?"
    (nnweb-encode-www-form-urlencoded
     `(("ST" . "PS")
       ("svcclass" . "dnyr")
       ("QRY" . ,search)
       ("defaultOp" . "AND")
       ("DBS" . "1")
       ("OP" . "dnquery.xp")
       ("LNG" . "ALL")
       ("maxhits" . "100")
       ("threaded" . "0")
       ("format" . "verbose2")
       ("showsort" . "date")
       ("agesign" . "1")
       ("ageweight" . "1")))))
  t)

(defun nnweb-dejanewsold-search (search)
  (nnweb-fetch-form
   (nnweb-definition 'address)
   `(("query" . ,search)
     ("defaultOp" . "AND")
     ("svcclass" . "dnold")
     ("maxhits" . "100")
     ("format" . "verbose2")
     ("threaded" . "0")
     ("showsort" . "date")
     ("agesign" . "1")
     ("ageweight" . "1")))
  t)

(defun nnweb-dejanews-identity (url)
  "Return an unique identifier based on URL."
  (if (string-match "AN=\\([0-9]+\\)" url)
      (match-string 1 url)
    url))

;;;
;;; InReference
;;;

(defun nnweb-reference-create-mapping ()
  "Perform the search and create an number-to-url alist."
  (save-excursion
    (set-buffer nnweb-buffer)
    (erase-buffer)
    (when (funcall (nnweb-definition 'search) nnweb-search)
      (let ((i 0)
	    (more t)
	    (case-fold-search t)
	    (active (or (cadr (assoc nnweb-group nnweb-group-alist))
			(cons 1 0)))
	    Subject Score Date Newsgroups From Message-ID
	    map url)
	(while more
	  ;; Go through all the article hits on this page.
	  (goto-char (point-min))
	  (search-forward "</pre><hr>" nil t)
	  (delete-region (point-min) (point))
	  (goto-char (point-min))
	  (while (re-search-forward "^ +[0-9]+\\." nil t)
	    (narrow-to-region
	     (point)
	     (if (re-search-forward "^$" nil t)
		 (match-beginning 0)
	       (point-max)))
	    (goto-char (point-min))
	    (when (looking-at ".*href=\"\\([^\"]+\\)\"")
	      (setq url (match-string 1)))
	    (nnweb-remove-markup)
	    (goto-char (point-min))
	    (while (search-forward "\t" nil t)
	      (replace-match " "))
	    (goto-char (point-min))
	    (while (re-search-forward "^\\([^:]+\\): \\(.*\\)$" nil t)
	      (set (intern (match-string 1)) (match-string 2)))
	    (widen)
	    (search-forward "</pre>" nil t)
	    (incf i)
	    (unless (nnweb-get-hashtb url)
	      (push
	       (list
		(incf (cdr active))
		(make-full-mail-header
		 (cdr active) (concat  "(" Newsgroups ") " Subject) From Date
		 Message-ID
		 nil 0 (string-to-int Score) url))
	       map)
	      (nnweb-set-hashtb (cadar map) (car map))))
	  (setq more nil))
	;; Return the articles in the right order.
	(setq nnweb-articles
	      (sort (nconc nnweb-articles map) 'car-less-than-car))))))

(defun nnweb-reference-wash-article ()
  (let ((case-fold-search t))
    (goto-char (point-min))
    (re-search-forward "^</center><hr>" nil t)
    (delete-region (point-min) (point))
    (search-forward "<pre>" nil t)
    (forward-line -1)
    (let ((body (point-marker)))
      (search-forward "</pre>" nil t)
      (delete-region (point) (point-max))
      (nnweb-remove-markup)
      (goto-char (point-min))
      (while (looking-at " *$")
	(gnus-delete-line))
      (narrow-to-region (point-min) body)
      (while (and (re-search-forward "^$" nil t)
		  (not (eobp)))
	(gnus-delete-line))
      (goto-char (point-min))
      (while (looking-at "\\(^[^ ]+:\\) *")
	(replace-match "\\1 " t)
	(forward-line 1))
      (goto-char (point-min))
      (when (re-search-forward "^References:" nil t)
	(narrow-to-region
	 (point) (if (re-search-forward "^$\\|^[^:]+:" nil t)
		     (match-beginning 0)
		   (point-max)))
	(goto-char (point-min))
	(while (not (eobp))
	  (unless (looking-at "References")
	    (insert "\t")
	    (forward-line 1)))
	(goto-char (point-min))
	(while (search-forward "," nil t)
	  (replace-match " " t t)))
      (widen)
      (set-marker body nil))))

(defun nnweb-reference-search (search)
  (url-insert-file-contents
   (concat
    (nnweb-definition 'address)
    "?"
    (nnweb-encode-www-form-urlencoded
     `(("search" . "advanced")
       ("querytext" . ,search)
       ("subj" . "")
       ("name" . "")
       ("login" . "")
       ("host" . "")
       ("organization" . "")
       ("groups" . "")
       ("keywords" . "")
       ("choice" . "Search")
       ("startmonth" . "Jul")
       ("startday" . "25")
       ("startyear" . "1996")
       ("endmonth" . "Aug")
       ("endday" . "24")
       ("endyear" . "1996")
       ("mode" . "Quick")
       ("verbosity" . "Verbose")
       ("ranking" . "Relevance")
       ("first" . "1")
       ("last" . "25")
       ("score" . "50")))))
  (setq buffer-file-name nil)
  t)

;;;
;;; Alta Vista
;;;

(defun nnweb-altavista-create-mapping ()
  "Perform the search and create an number-to-url alist."
  (save-excursion
    (set-buffer nnweb-buffer)
    (erase-buffer)
    (let ((part 0))
      (when (funcall (nnweb-definition 'search) nnweb-search part)
	(let ((i 0)
	      (more t)
	      (case-fold-search t)
	      (active (or (cadr (assoc nnweb-group nnweb-group-alist))
			  (cons 1 0)))
	      subject date from id group
	      map url)
	  (while more
	    ;; Go through all the article hits on this page.
	    (goto-char (point-min))
	    (search-forward "<dt>" nil t)
	    (delete-region (point-min) (match-beginning 0))
	    (goto-char (point-min))
	    (while (search-forward "<dt>" nil t)
	      (replace-match "\n<blubb>"))
	    (nnweb-decode-entities)
	    (goto-char (point-min))
	    (while (re-search-forward "<blubb>.*href=\"\\([^\"]+\\)\"><strong>\\([^>]*\\)</strong></a><dd>\\([^-]+\\)- <b>\\([^<]+\\)<.*href=\"news:\\([^\"]+\\)\">.*\">\\(.+\\)</a><P>"
				      nil t)
	      (setq url (match-string 1)
		    subject (match-string 2)
		    date (match-string 3)
		    group (match-string 4)
		    id (concat "<" (match-string 5) ">")
		    from (match-string 6))
	      (incf i)
	      (unless (nnweb-get-hashtb url)
		(push
		 (list
		  (incf (cdr active))
		  (make-full-mail-header
		   (cdr active) (concat  "(" group ") " subject) from date
		   id nil 0 0 url))
		 map)
		(nnweb-set-hashtb (cadar map) (car map))))
	    ;; See if we want more.
	    (when (or (not nnweb-articles)
		      (>= i nnweb-max-hits)
		      (not (funcall (nnweb-definition 'search)
				    nnweb-search (incf part))))
	      (setq more nil)))
	  ;; Return the articles in the right order.
	  (setq nnweb-articles
		(sort (nconc nnweb-articles map) 'car-less-than-car)))))))

(defun nnweb-altavista-wash-article ()
  (goto-char (point-min))
  (let ((case-fold-search t))
    (when (re-search-forward "^<strong>" nil t)
      (delete-region (point-min) (match-beginning 0)))
    (goto-char (point-min))
    (while (looking-at "<strong>\\([^ ]+\\) +</strong> +\\(.*\\)$")
      (replace-match "\\1: \\2" t)
      (forward-line 1))
    (when (re-search-backward "^References:" nil t)
      (narrow-to-region (point) (progn (forward-line 1) (point)))
      (goto-char (point-min))
      (while (re-search-forward "<A.*\\?id@\\([^\"]+\\)\">[0-9]+</A>" nil t)
	(replace-match "&lt;\\1&gt; " t)))
    (widen)
    (nnweb-remove-markup)))

(defun nnweb-altavista-search (search &optional part)
  (url-insert-file-contents
   (concat
    (nnweb-definition 'address)
    "?"
    (nnweb-encode-www-form-urlencoded
     `(("pg" . "aq")
       ("what" . "news")
       ,@(when part `(("stq" . ,(int-to-string (* part 30)))))
       ("fmt" . "d")
       ("q" . ,search)
       ("r" . "")
       ("d0" . "")
       ("d1" . "")))))
  (setq buffer-file-name nil)
  t)

;;;
;;; General web/w3 interface utility functions
;;;

(defun nnweb-insert-html (parse)
  "Insert HTML based on a w3 parse tree."
  (if (stringp parse)
      (insert parse)
    (insert "<" (symbol-name (car parse)) " ")
    (insert (mapconcat
	     (lambda (param)
	       (concat (symbol-name (car param)) "="
		       (prin1-to-string
			(if (consp (cdr param))
			    (cadr param)
			  (cdr param)))))
	     (nth 1 parse)
	     " "))
    (insert ">\n")
    (mapcar 'nnweb-insert-html (nth 2 parse))
    (insert "</" (symbol-name (car parse)) ">\n")))

(defun nnweb-encode-www-form-urlencoded (pairs)
  "Return PAIRS encoded for forms."
  (mapconcat
   (function
    (lambda (data)
      (concat (w3-form-encode-xwfu (car data)) "="
	      (w3-form-encode-xwfu (cdr data)))))
   pairs "&"))

(defun nnweb-fetch-form (url pairs)
  "Fetch a form from URL with PAIRS as the data using the POST method."
  (let ((url-request-data (nnweb-encode-www-form-urlencoded pairs))
	(url-request-method "POST")
	(url-request-extra-headers
	 '(("Content-type" . "application/x-www-form-urlencoded"))))
    (url-insert-file-contents url)
    (setq buffer-file-name nil))
  t)

(defun nnweb-decode-entities ()
  "Decode all HTML entities."
  (goto-char (point-min))
  (while (re-search-forward "&\\(#[0-9]+\\|[a-z]+\\);" nil t)
    (let ((elem (if (eq (aref (match-string 1) 0) ?\#)
			(let ((c
			       (string-to-number (substring 
						  (match-string 1) 1))))
			  (if (mm-char-or-char-int-p c) c 32))
		      (or (cdr (assq (intern (match-string 1))
				     w3-html-entities))
			  ?#))))
      (unless (stringp elem)
	(setq elem (char-to-string elem)))
      (replace-match elem t t))))

(defun nnweb-decode-entities-string (str)
  (with-temp-buffer
    (insert str)
    (nnweb-decode-entities)
    (buffer-substring (point-min) (point-max))))

(defun nnweb-remove-markup ()
  "Remove all HTML markup, leaving just plain text."
  (goto-char (point-min))
  (while (search-forward "<!--" nil t)
    (delete-region (match-beginning 0)
		   (or (search-forward "-->" nil t)
		       (point-max))))
  (goto-char (point-min))
  (while (re-search-forward "<[^>]+>" nil t)
    (replace-match "" t t)))

(defun nnweb-insert (url &optional follow-refresh)
  "Insert the contents from an URL in the current buffer.
If FOLLOW-REFRESH is non-nil, redirect refresh url in META."
  (let ((name buffer-file-name))
    (if follow-refresh
	(save-restriction
	  (narrow-to-region (point) (point))
	  (url-insert-file-contents url)
	  (goto-char (point-min))
	  (when (re-search-forward 
		 "<meta[ \t\r\n]*http-equiv=\"Refresh\"[^>]*URL=\\([^\"]+\\)\"" nil t)
	    (let ((url (match-string 1)))
	      (delete-region (point-min) (point-max))
	      (nnweb-insert url t))))
      (url-insert-file-contents url))
    (setq buffer-file-name name)))

(defun nnweb-parse-find (type parse &optional maxdepth)
  "Find the element of TYPE in PARSE."
  (catch 'found
    (nnweb-parse-find-1 type parse maxdepth)))

(defun nnweb-parse-find-1 (type contents maxdepth)
  (when (or (null maxdepth)
	    (not (zerop maxdepth)))
    (when (consp contents)
      (when (eq (car contents) type)
	(throw 'found contents))
      (when (listp (cdr contents))
	(dolist (element contents)
	  (when (consp element)
	    (nnweb-parse-find-1 type element
				(and maxdepth (1- maxdepth)))))))))

(defun nnweb-parse-find-all (type parse)
  "Find all elements of TYPE in PARSE."
  (catch 'found
    (nnweb-parse-find-all-1 type parse)))

(defun nnweb-parse-find-all-1 (type contents)
  (let (result)
    (when (consp contents)
      (if (eq (car contents) type)
	  (push contents result)
	(when (listp (cdr contents))
	  (dolist (element contents)
	    (when (consp element)
	      (setq result
		    (nconc result (nnweb-parse-find-all-1 type element))))))))
    result))

(defvar nnweb-text)
(defun nnweb-text (parse)
  "Return a list of text contents in PARSE."
  (let ((nnweb-text nil))
    (nnweb-text-1 parse)
    (nreverse nnweb-text)))

(defun nnweb-text-1 (contents)
  (dolist (element contents)
    (if (stringp element)
	(push element nnweb-text)
      (when (and (consp element)
		 (listp (cdr element)))
	(nnweb-text-1 element)))))

(provide 'nnweb)

;;; nnweb.el ends here
