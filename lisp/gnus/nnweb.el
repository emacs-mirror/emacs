;;; nnweb.el --- retrieving articles via web search engines
;; Copyright (C) 1996,97 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
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
(require 'w3)
(require 'url)
(require 'nnmail)
(eval-when-compile
  (ignore-errors
   (require 'w3)
   (require 'url)
   (require 'w3-forms)))
;; Report failure to find w3 at load time if appropriate.
(eval '(progn
	 (require 'w3)
	 (require 'url)
	 (require 'w3-forms)))

(nnoo-declare nnweb)

(defvoo nnweb-directory (nnheader-concat gnus-directory "nnweb/")
  "Where nnweb will save its files.")

(defvoo nnweb-type 'dejanews
  "What search engine type is being used.")

(defvar nnweb-type-definition
  '((dejanews
     (article . nnweb-dejanews-wash-article)
     (map . nnweb-dejanews-create-mapping)
     (search . nnweb-dejanews-search)
     (address . "http://xp9.dejanews.com/dnquery.xp")
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

(defvoo nnweb-max-hits 100
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
      (while (setq article (pop articles))
	(when (setq header (cadr (assq article nnweb-articles)))
	  (nnheader-insert-nov header)))
      'nov)))

(deffoo nnweb-request-scan (&optional group server)
  (nnweb-possibly-change-server group server)
  (setq nnweb-hashtb (gnus-make-hashtable 4095))
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
      (setq nnweb-group group)
      (setq nnweb-type (nth 2 info))
      (setq nnweb-search (nth 3 info))
      (unless dont-check
	(nnweb-read-overview group))))
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
		     (nnweb-fetch-url url))
		(and (stringp article)
		     (nnweb-definition 'id t)
		     (let ((fetch (nnweb-definition 'id))
			   art)
		       (when (string-match "^<\\(.*\\)>$" article)
			 (setq art (match-string 1 article)))
		       (and fetch
			    art
			    (nnweb-fetch-url
			     (format fetch article))))))
	(unless nnheader-callback-function
	  (funcall (nnweb-definition 'article))
	  (nnweb-decode-entities))
	(nnheader-report 'nnweb "Fetched article %s" article)
	t))))

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
  (nnweb-possibly-change-server group server)
  ;;(setcar (cddr info) nil)
  )

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
  (gnus-delete-assoc group nnweb-group-alist)
  (gnus-delete-file (nnweb-overview-file group))
  t)

(nnoo-define-skeleton nnweb)

;;; Internal functions

(defun nnweb-read-overview (group)
  "Read the overview of GROUP and build the map."
  (when (file-exists-p (nnweb-overview-file group))
    (nnheader-temp-write nil
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
  (nnheader-temp-write (nnweb-overview-file group)
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
  (nnheader-temp-write (nnheader-concat nnweb-directory "active")
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
      (nnweb-request-group group nil t))))

(defun nnweb-init (server)
  "Initialize buffers and such."
  (unless (gnus-buffer-live-p nnweb-buffer)
    (setq nnweb-buffer
	  (save-excursion
	    (nnheader-set-temp-buffer
	     (format " *nnweb %s %s %s*" nnweb-type nnweb-search server))))))

(defun nnweb-fetch-url (url)
  (save-excursion
    (if (not nnheader-callback-function)
	(let ((buf (current-buffer)))
	  (save-excursion
	    (set-buffer nnweb-buffer)
	    (erase-buffer)
	    (url-insert-file-contents url)
	    (copy-to-buffer buf (point-min) (point-max))
	    t))
      (nnweb-url-retrieve-asynch
       url 'nnweb-callback (current-buffer) nnheader-callback-function)
      t)))

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
      (url-retrieve url))
    (setq-default url-be-asynchronous old-asynch)))

(defun nnweb-encode-www-form-urlencoded (pairs)
  "Return PAIRS encoded for forms."
  (mapconcat
   (function
    (lambda (data)
      (concat (w3-form-encode-xwfu (car data)) "="
	      (w3-form-encode-xwfu (cdr data)))))
   pairs "&"))

(defun nnweb-fetch-form (url pairs)
  (let ((url-request-data (nnweb-encode-www-form-urlencoded pairs))
	(url-request-method "POST")
	(url-request-extra-headers
	 '(("Content-type" . "application/x-www-form-urlencoded"))))
    (url-insert-file-contents url)
    (setq buffer-file-name nil))
  t)

(defun nnweb-decode-entities ()
  (goto-char (point-min))
  (while (re-search-forward "&\\([a-z]+\\);" nil t)
    (replace-match (char-to-string (or (cdr (assq (intern (match-string 1))
						  w3-html-entities))
				       ?#))
		   t t)))

(defun nnweb-remove-markup ()
  (goto-char (point-min))
  (while (search-forward "<!--" nil t)
    (delete-region (match-beginning 0)
		   (or (search-forward "-->" nil t)
		       (point-max))))
  (goto-char (point-min))
  (while (re-search-forward "<[^>]+>" nil t)
    (replace-match "" t t)))

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
	    Subject Score Date Newsgroup Author
	    map url)
	(while more
	  ;; Go through all the article hits on this page.
	  (goto-char (point-min))
	  (nnweb-decode-entities)
	  (goto-char (point-min))
	  (while (re-search-forward "^ +[0-9]+\\." nil t)
	    (narrow-to-region
	     (point)
	     (cond ((re-search-forward "^ +[0-9]+\\." nil t)
		    (match-beginning 0))
		   ((search-forward "\n\n" nil t)
		    (point))
		   (t
		    (point-max))))
	    (goto-char (point-min))
	    (when (looking-at ".*HREF=\"\\([^\"]+\\)\"")
	      (setq url (match-string 1)))
	    (nnweb-remove-markup)
	    (goto-char (point-min))
	    (while (search-forward "\t" nil t)
	      (replace-match " "))
	    (goto-char (point-min))
	    (while (re-search-forward "^ +\\([^:]+\\): +\\(.*\\)$" nil t)
	      (set (intern (match-string 1)) (match-string 2)))
	    (widen)
	    (when (string-match "#[0-9]+/[0-9]+ *$" Subject)
	      (setq Subject (substring Subject 0 (match-beginning 0))))
	    (incf i)
	    (unless (nnweb-get-hashtb url)
	      (push
	       (list
		(incf (cdr active))
		(make-full-mail-header
		 (cdr active) (concat  "(" Newsgroup ") " Subject) Author Date
		 (concat "<" (nnweb-identifier url) "@dejanews>")
		 nil 0 (string-to-int Score) url))
	       map)
	      (nnweb-set-hashtb (cadar map) (car map))))
	  ;; See whether there is a "Get next 20 hits" button here.
	  (if (or (not (re-search-forward
			"HREF=\"\\([^\"]+\\)\">Get next" nil t))
		  (>= i nnweb-max-hits))
	      (setq more nil)
	    ;; Yup -- fetch it.
	    (setq more (match-string 1))
	    (erase-buffer)
	    (url-insert-file-contents more)))
	;; Return the articles in the right order.
	(setq nnweb-articles
	      (sort (nconc nnweb-articles map)
		    (lambda (s1 s2) (< (car s1) (car s2)))))))))

(defun nnweb-dejanews-wash-article ()
  (let ((case-fold-search t))
    (goto-char (point-min))
    (re-search-forward "<PRE>" nil t)
    (delete-region (point-min) (point))
    (re-search-forward "</PRE>" nil t)
    (delete-region (point) (point-max))
    (nnweb-remove-markup)
    (goto-char (point-min))
    (while (and (looking-at " *$")
		(not (eobp)))
      (gnus-delete-line))
    (while (looking-at "\\(^[^ ]+:\\) *")
      (replace-match "\\1 " t)
      (forward-line 1))
    (when (re-search-forward "\n\n+" nil t)
      (replace-match "\n" t t))
    (goto-char (point-min))
    (when (search-forward "[More Headers]" nil t)
      (replace-match "" t t))))

(defun nnweb-dejanews-search (search)
  (nnweb-fetch-form
   (nnweb-definition 'address)
   `(("query" . ,search)
     ("defaultOp" . "AND")
     ("svcclass" . "dncurrent")
     ("maxhits" . "100")
     ("format" . "verbose")
     ("threaded" . "0")
     ("showsort" . "score")
     ("agesign" . "1")
     ("ageweight" . "1")))
  t)

(defun nnweb-dejanews-identity (url)
  "Return an unique identifier based on URL."
  (if (string-match "recnum=\\([0-9]+\\)" url)
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
					;(nnweb-decode-entities)
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
	      (sort (nconc nnweb-articles map)
		    (lambda (s1 s2) (< (car s1) (car s2)))))))))

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
		(sort (nconc nnweb-articles map)
		      (lambda (s1 s2) (< (car s1) (car s2))))))))))

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

(provide 'nnweb)

;;; nnweb.el ends here
