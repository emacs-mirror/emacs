;;; nnrss.el --- interfacing with RSS
;; Copyright (C) 2001, 2002, 2003  Free Software Foundation, Inc.

;; Author: Shenghuo Zhu <zsh@cs.rochester.edu>
;; Keywords: RSS

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))

(require 'gnus)
(require 'nnoo)
(require 'nnmail)
(require 'message)
(require 'mm-util)
(require 'gnus-util)
(require 'time-date)
(require 'rfc2231)
(require 'mm-url)
(eval-when-compile
  (ignore-errors
    (require 'xml)))
(eval '(require 'xml))

(nnoo-declare nnrss)

(defvoo nnrss-directory (nnheader-concat gnus-directory "rss/")
  "Where nnrss will save its files.")

;; (group max rss-url)
(defvoo nnrss-server-data nil)

;; (num timestamp url subject author date extra)
(defvoo nnrss-group-data nil)
(defvoo nnrss-group-max 0)
(defvoo nnrss-group-min 1)
(defvoo nnrss-group nil)
(defvoo nnrss-group-hashtb nil)
(defvoo nnrss-status-string "")

(defconst nnrss-version "nnrss 1.0")

(defvar nnrss-group-alist '()
  "List of RSS addresses.")

(defvar nnrss-use-local nil)

(defvar nnrss-description-field 'X-Gnus-Description
  "Field name used for DESCRIPTION.
To use the description in headers, put this name into `nnmail-extra-headers'.")

(defvar nnrss-url-field 'X-Gnus-Url
  "Field name used for URL.
To use the description in headers, put this name into `nnmail-extra-headers'.")

(defvar nnrss-content-function nil
  "A function which is called in `nnrss-request-article'.
The arguments are (ENTRY GROUP ARTICLE).
ENTRY is the record of the current headline. GROUP is the group name.
ARTICLE is the article number of the current headline.")

(nnoo-define-basics nnrss)

;;; Interface functions

(eval-when-compile
  (defmacro nnrss-string-as-multibyte (string)
    (if (featurep 'xemacs)
	string
      `(string-as-multibyte ,string))))

(deffoo nnrss-retrieve-headers (articles &optional group server fetch-old)
  (nnrss-possibly-change-group group server)
  (let (e)
    (save-excursion
      (set-buffer nntp-server-buffer)
      (erase-buffer)
      (dolist (article articles)
	(if (setq e (assq article nnrss-group-data))
	    (insert (number-to-string (car e)) "\t" ;; number
		    (if (nth 3 e)
			(nnrss-format-string (nth 3 e)) "")
		    "\t" ;; subject
		    (if (nth 4 e)
			(nnrss-format-string (nth 4 e))
		      "(nobody)")
		    "\t" ;;from
		    (or (nth 5 e) "")
		    "\t" ;; date
		    (format "<%d@%s.nnrss>" (car e) group)
		    "\t" ;; id
		    "\t" ;; refs
		    "-1" "\t" ;; chars
		    "-1" "\t" ;; lines
		    "" "\t" ;; Xref
		    (if (and (nth 6 e)
			     (memq nnrss-description-field
				   nnmail-extra-headers))
			(concat (symbol-name nnrss-description-field)
				": "
				(nnrss-format-string (nth 6 e))
				"\t")
		      "")
		    (if (and (nth 2 e)
			     (memq nnrss-url-field
				   nnmail-extra-headers))
			(concat (symbol-name nnrss-url-field)
				": "
				(nnrss-format-string (nth 2 e))
				"\t")
		      "")
		    "\n")))))
  'nov)

(deffoo nnrss-request-group (group &optional server dont-check)
  (nnrss-possibly-change-group group server)
  (if dont-check
      t
    (nnrss-check-group group server)
    (nnheader-report 'nnrss "Opened group %s" group)
    (nnheader-insert
     "211 %d %d %d %s\n" nnrss-group-max nnrss-group-min nnrss-group-max
     (prin1-to-string group)
     t)))

(deffoo nnrss-close-group (group &optional server)
  t)

(deffoo nnrss-request-article (article &optional group server buffer)
  (nnrss-possibly-change-group group server)
  (let ((e (assq article nnrss-group-data))
	(boundary "=-=-=-=-=-=-=-=-=-")
	(nntp-server-buffer (or buffer nntp-server-buffer))
	post err)
    (when e
      (catch 'error
	(with-current-buffer nntp-server-buffer
	  (erase-buffer)
	  (goto-char (point-min))
	  (insert "Mime-Version: 1.0\nContent-Type: multipart/alternative; boundary=\"" boundary "\"\n")
	  (if group
	      (insert "Newsgroups: " group "\n"))
	  (if (nth 3 e)
	      (insert "Subject: " (nnrss-format-string (nth 3 e)) "\n"))
	  (if (nth 4 e)
	      (insert "From: " (nnrss-format-string (nth 4 e)) "\n"))
	  (if (nth 5 e)
	      (insert "Date: " (nnrss-format-string (nth 5 e)) "\n"))
	  (insert "Message-ID: " (format "<%d@%s.nnrss>" (car e) group) "\n")
	  (insert "\n")
	  (let ((text (if (nth 6 e)
			  (nnrss-string-as-multibyte (nth 6 e))))
		(link (if (nth 2 e)
			  (nth 2 e))))
	    (insert "\n\n--" boundary "\nContent-Type: text/plain\n\n")
	    (let ((point (point)))
	      (if text
		  (progn (insert text)
			 (goto-char point)
			 (while (re-search-forward "\n" nil t)
			   (replace-match " "))
			 (goto-char (point-max))
			 (insert "\n\n")))
	      (if link
		  (insert link)))
	    (insert "\n\n--" boundary "\nContent-Type: text/html\n\n")
	    (let ((point (point)))
	      (if text
		  (progn (insert "<html><head></head><body>\n" text "\n</body></html>")
			 (goto-char point)
			 (while (re-search-forward "\n" nil t)
			   (replace-match " "))
			 (goto-char (point-max))
			 (insert "\n\n")))
	      (if link
		  (insert "<p><a href=\"" link "\">link</a></p>\n"))))
	  (if nnrss-content-function
	      (funcall nnrss-content-function e group article)))))
    (cond
     (err
      (nnheader-report 'nnrss err))
     ((not e)
      (nnheader-report 'nnrss "no such id: %d" article))
     (t
      (nnheader-report 'nnrss "article %s retrieved" (car e))
      ;; we return the article number.
      (cons nnrss-group (car e))))))

(deffoo nnrss-request-list (&optional server)
  (nnrss-possibly-change-group nil server)
  (nnrss-generate-active)
  t)

(deffoo nnrss-open-server (server &optional defs connectionless)
  (nnrss-read-server-data server)
  (nnoo-change-server 'nnrss server defs)
  t)

(deffoo nnrss-request-expire-articles
    (articles group &optional server force)
  (nnrss-possibly-change-group group server)
  (let (e days not-expirable changed)
    (dolist (art articles)
      (if (and (setq e (assq art nnrss-group-data))
	       (nnmail-expired-article-p
		group
		(if (listp (setq days (nth 1 e))) days
		  (days-to-time (- days (time-to-days '(0 0)))))
		force))
	  (setq nnrss-group-data (delq e nnrss-group-data)
		changed t)
	(push art not-expirable)))
    (if changed
	(nnrss-save-group-data group server))
    not-expirable))

(deffoo nnrss-request-delete-group (group &optional force server)
  (nnrss-possibly-change-group group server)
  (setq nnrss-server-data
	(delq (assoc group nnrss-server-data) nnrss-server-data))
  (nnrss-save-server-data server)
  (let ((file (expand-file-name
	       (nnrss-translate-file-chars
		(concat group (and server
				   (not (equal server ""))
				   "-")
			server ".el")) nnrss-directory)))
    (ignore-errors
      (delete-file file)))
  t)

(deffoo nnrss-request-list-newsgroups (&optional server)
  (nnrss-possibly-change-group nil server)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (dolist (elem nnrss-group-alist)
      (if (third elem)
	  (insert (car elem) "\t" (third elem) "\n"))))
  t)

(nnoo-define-skeleton nnrss)

;;; Internal functions
(eval-when-compile (defun xml-rpc-method-call (&rest args)))
(defun nnrss-fetch (url &optional local)
  "Fetch the url and put it in a the expected lisp structure."
  (with-temp-buffer
  ;some CVS versions of url.el need this to close the connection quickly
    (let* (xmlform htmlform)
      ;; bit o' work necessary for w3 pre-cvs and post-cvs
      (if local
	  (let ((coding-system-for-read 'binary))
	    (insert-file-contents url))
	(mm-url-insert url))

;; Because xml-parse-region can't deal with anything that isn't
;; xml and w3-parse-buffer can't deal with some xml, we have to
;; parse with xml-parse-region first and, if that fails, parse
;; with w3-parse-buffer.  Yuck.  Eventually, someone should find out
;; why w3-parse-buffer fails to parse some well-formed xml and
;; fix it.

    (condition-case err
	(setq xmlform (xml-parse-region (point-min) (point-max)))
      (error (if (fboundp 'w3-parse-buffer)
		 (setq htmlform (caddar (w3-parse-buffer
					 (current-buffer))))
	       (message "nnrss: Not valid XML and w3 parse not available (%s)"
			url))))
    (if htmlform
	htmlform
      xmlform))))

(defun nnrss-possibly-change-group (&optional group server)
  (when (and server
	     (not (nnrss-server-opened server)))
    (nnrss-open-server server))
  (when (and group (not (equal group nnrss-group)))
    (nnrss-read-group-data group server)
    (setq nnrss-group group)))

(defvar nnrss-extra-categories '(nnrss-snarf-moreover-categories))

(defun nnrss-generate-active ()
  (if (y-or-n-p "fetch extra categories? ")
      (dolist (func nnrss-extra-categories)
	(funcall func)))
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (dolist (elem nnrss-group-alist)
      (insert (prin1-to-string (car elem)) " 0 1 y\n"))
    (dolist (elem nnrss-server-data)
      (unless (assoc (car elem) nnrss-group-alist)
	(insert (prin1-to-string (car elem)) " 0 1 y\n")))))

;;; data functions

(defun nnrss-read-server-data (server)
  (setq nnrss-server-data nil)
  (let ((file (expand-file-name
	       (nnrss-translate-file-chars
		(concat "nnrss" (and server
				     (not (equal server ""))
				     "-")
			server
			".el"))
	       nnrss-directory)))
    (when (file-exists-p file)
      (with-temp-buffer
	(let ((coding-system-for-read 'binary)
	      emacs-lisp-mode-hook)
	  (insert-file-contents file)
	  (emacs-lisp-mode)
	  (goto-char (point-min))
	  (eval-buffer))))))

(defun nnrss-save-server-data (server)
  (gnus-make-directory nnrss-directory)
  (let ((file (expand-file-name
	       (nnrss-translate-file-chars
		(concat "nnrss" (and server
				     (not (equal server ""))
				     "-")
			server ".el"))
	       nnrss-directory)))
    (let ((coding-system-for-write 'binary)
	  print-level print-length)
      (with-temp-file file
	(insert "(setq nnrss-group-alist '"
		(prin1-to-string nnrss-group-alist)
		")\n")
	(insert "(setq nnrss-server-data '"
		(prin1-to-string nnrss-server-data)
		")\n")))))

(defun nnrss-read-group-data (group server)
  (setq nnrss-group-data nil)
  (setq nnrss-group-hashtb (gnus-make-hashtable))
  (let ((pair (assoc group nnrss-server-data)))
    (setq nnrss-group-max (or (cadr pair) 0))
    (setq nnrss-group-min (+ nnrss-group-max 1)))
  (let ((file (expand-file-name
	       (nnrss-translate-file-chars
		(concat group (and server
				   (not (equal server ""))
				   "-")
			server ".el"))
	       nnrss-directory)))
    (when (file-exists-p file)
      (with-temp-buffer
	(let ((coding-system-for-read 'binary)
	      emacs-lisp-mode-hook)
	  (insert-file-contents file)
	  (emacs-lisp-mode)
	  (goto-char (point-min))
	  (eval-buffer)))
      (dolist (e nnrss-group-data)
	(gnus-sethash (nth 2 e) e nnrss-group-hashtb)
	(if (and (car e) (> nnrss-group-min (car e)))
	    (setq nnrss-group-min (car e)))
	(if (and (car e) (< nnrss-group-max (car e)))
	    (setq nnrss-group-max (car e)))))))

(defun nnrss-save-group-data (group server)
  (gnus-make-directory nnrss-directory)
  (let ((file (expand-file-name
	       (nnrss-translate-file-chars
		(concat group (and server
				   (not (equal server ""))
				   "-")
			server ".el"))
	       nnrss-directory)))
    (let ((coding-system-for-write 'binary)
	  print-level print-length)
      (with-temp-file file
	(insert "(setq nnrss-group-data '"
		(prin1-to-string nnrss-group-data)
		")\n")))))

;;; URL interface

(defun nnrss-no-cache (url)
  "")

(defun nnrss-insert-w3 (url)
  (mm-with-unibyte-current-buffer
    (mm-url-insert url)))

(defun nnrss-decode-entities-unibyte-string (string)
  (if string
      (mm-with-unibyte-buffer
	(insert string)
	(mm-url-decode-entities-nbsp)
	(buffer-string))))

(defalias 'nnrss-insert 'nnrss-insert-w3)

;;; Snarf functions

(defun nnrss-check-group (group server)
  (let (file xml subject url extra changed author
	     date rss-ns rdf-ns content-ns dc-ns)
    (if (and nnrss-use-local
	     (file-exists-p (setq file (expand-file-name
					(nnrss-translate-file-chars
					 (concat group ".xml"))
					nnrss-directory))))
	(setq xml (nnrss-fetch file t))
      (setq url (or (nth 2 (assoc group nnrss-server-data))
		    (second (assoc group nnrss-group-alist))))
      (unless url
	(setq url
             (cdr
              (assoc 'href
                     (nnrss-discover-feed
                      (read-string
                       (format "URL to search for %s: " group) "http://")))))
	(let ((pair (assoc group nnrss-server-data)))
	  (if pair
	      (setcdr (cdr pair) (list url))
	    (push (list group nnrss-group-max url) nnrss-server-data)))
	(setq changed t))
      (setq xml (nnrss-fetch url)))
    ;; See
    ;; http://feeds.archive.org/validator/docs/howto/declare_namespaces.html
    ;; for more RSS namespaces.
    (setq dc-ns (nnrss-get-namespace-prefix xml "http://purl.org/dc/elements/1.1/")
	  rdf-ns (nnrss-get-namespace-prefix xml "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
	  rss-ns (nnrss-get-namespace-prefix xml "http://purl.org/rss/1.0/")
	  content-ns (nnrss-get-namespace-prefix xml "http://purl.org/rss/1.0/modules/content/"))
    (dolist (item (nreverse (nnrss-find-el (intern (concat rss-ns "item")) xml)))
      (when (and (listp item)
		 (eq (intern (concat rss-ns "item")) (car item))
		 (setq url (nnrss-decode-entities-unibyte-string
			    (nnrss-node-text rss-ns 'link (cddr item))))
		 (not (gnus-gethash url nnrss-group-hashtb)))
	(setq subject (nnrss-node-text rss-ns 'title item))
	(setq extra (or (nnrss-node-text content-ns 'encoded item)
			(nnrss-node-text rss-ns 'description item)))
	(setq author (or (nnrss-node-text rss-ns 'author item)
			 (nnrss-node-text dc-ns 'creator item)
			 (nnrss-node-text dc-ns 'contributor item)))
	(setq date (or (nnrss-node-text dc-ns 'date item)
		       (nnrss-node-text rss-ns 'pubDate item)
		       (message-make-date)))
	(push
	 (list
	  (incf nnrss-group-max)
	  (current-time)
	  url
	  (and subject (nnrss-decode-entities-unibyte-string subject))
	  (and author (nnrss-decode-entities-unibyte-string author))
	  date
	  (and extra (nnrss-decode-entities-unibyte-string extra)))
	 nnrss-group-data)
	(gnus-sethash url (car nnrss-group-data) nnrss-group-hashtb)
	(setq changed t)))
    (when changed
      (nnrss-save-group-data group server)
      (let ((pair (assoc group nnrss-server-data)))
	(if pair
	    (setcar (cdr pair) nnrss-group-max)
	  (push (list group nnrss-group-max) nnrss-server-data)))
      (nnrss-save-server-data server))))

(defun nnrss-generate-download-script ()
  "Generate a download script in the current buffer.
It is useful when `(setq nnrss-use-local t)'."
  (interactive)
  (insert "#!/bin/sh\n")
  (insert "WGET=wget\n")
  (insert "RSSDIR='" (expand-file-name nnrss-directory) "'\n")
  (dolist (elem nnrss-server-data)
    (let ((url (or (nth 2 elem)
		   (second (assoc (car elem) nnrss-group-alist)))))
      (insert "$WGET -q -O \"$RSSDIR\"/'"
	      (nnrss-translate-file-chars (concat (car elem) ".xml"))
	      "' '" url "'\n"))))

(defun nnrss-translate-file-chars (name)
  (let ((nnheader-file-name-translation-alist
	 (append nnheader-file-name-translation-alist '((?' . ?_)))))
    (nnheader-translate-file-chars name)))

(defvar nnrss-moreover-url
  "http://w.moreover.com/categories/category_list_rss.html"
  "The url of moreover.com categories.")

(defun nnrss-snarf-moreover-categories ()
  "Snarf RSS links from moreover.com."
  (interactive)
  (let (category name url changed)
    (with-temp-buffer
      (nnrss-insert nnrss-moreover-url)
      (goto-char (point-min))
      (while (re-search-forward
	      "<a name=\"\\([^\"]+\\)\">\\|<a href=\"\\(http://[^\"]*moreover\\.com[^\"]+page\\?c=\\([^\"&]+\\)&o=rss\\)" nil t)
	(if (match-string 1)
	    (setq category (match-string 1))
	  (setq url (match-string 2)
		name (mm-url-decode-entities-string
		      (rfc2231-decode-encoded-string
		       (match-string 3))))
	  (if category
	      (setq name (concat category "." name)))
	  (unless (assoc name nnrss-server-data)
	    (setq changed t)
	    (push (list name 0 url) nnrss-server-data)))))
    (if changed
	(nnrss-save-server-data ""))))

(defun nnrss-format-string (string)
  (gnus-replace-in-string (nnrss-string-as-multibyte string) " *\n *" " "))

(defun nnrss-node-text (namespace local-name element)
  (let* ((node (assq (intern (concat namespace (symbol-name local-name)))
		     element))
	 (text (if (and node (listp node))
		   (nnrss-node-just-text node)
		 node))
	 (cleaned-text (if text (gnus-replace-in-string
				 text "^[\000-\037\177]+\\|^ +\\| +$" ""))))
    (if (string-equal "" cleaned-text)
	nil
      cleaned-text)))

(defun nnrss-node-just-text (node)
  (if (and node (listp node))
      (mapconcat 'nnrss-node-just-text (cddr node) " ")
    node))

(defun nnrss-find-el (tag data &optional found-list)
  "Find the all matching elements in the data.  Careful with this on
large documents!"
  (if (listp data)
      (mapcar (lambda (bit)
		(if (car-safe bit)
		    (progn (if (equal tag (car bit))
			       (setq found-list
				     (append found-list
					     (list bit))))
			   (if (and (listp (car-safe (caddr bit)))
				    (not (stringp (caddr bit))))
			       (setq found-list
				     (append found-list
					     (nnrss-find-el
					      tag (caddr bit))))
			     (setq found-list
				   (append found-list
					   (nnrss-find-el
					    tag (cddr bit))))))))
		data))
  found-list)

(defun nnrss-rsslink-p (el)
  "Test if the element we are handed is an RSS autodiscovery link."
  (and (eq (car-safe el) 'link)
       (string-equal (cdr (assoc 'rel (cadr el))) "alternate")
       (or (string-equal (cdr (assoc 'type (cadr el))) 
			 "application/rss+xml")
	   (string-equal (cdr (assoc 'type (cadr el))) "text/xml"))))

(defun nnrss-get-rsslinks (data)
  "Extract the <link> elements that are links to RSS from the parsed data."
  (delq nil (mapcar 
	     (lambda (el)
	       (if (nnrss-rsslink-p el) el))
	     (nnrss-find-el 'link data))))

(defun nnrss-extract-hrefs (data)
  "Recursively extract hrefs from a page's source.  DATA should be
the output of xml-parse-region or w3-parse-buffer."
  (mapcar (lambda (ahref)
	    (cdr (assoc 'href (cadr ahref))))
	  (nnrss-find-el 'a data)))

(defmacro nnrss-match-macro (base-uri item 
					   onsite-list offsite-list)
  `(cond ((or (string-match (concat "^" ,base-uri) ,item)
	       (not (string-match "://" ,item)))
	   (setq ,onsite-list (append ,onsite-list (list ,item))))
	  (t (setq ,offsite-list (append ,offsite-list (list ,item))))))

(defun nnrss-order-hrefs (base-uri hrefs)
  "Given a list of hrefs, sort them using the following priorities:
  1. links ending in .rss
  2. links ending in .rdf
  3. links ending in .xml
  4. links containing the above
  5. offsite links

BASE-URI is used to determine the location of the links and
whether they are `offsite' or `onsite'."
  (let (rss-onsite-end  rdf-onsite-end  xml-onsite-end
	rss-onsite-in   rdf-onsite-in   xml-onsite-in
	rss-offsite-end rdf-offsite-end xml-offsite-end
	rss-offsite-in rdf-offsite-in xml-offsite-in)
    (mapcar (lambda (href)
	      (if (not (null href))
	      (cond ((string-match "\\.rss$" href)
		     (nnrss-match-macro
		      base-uri href rss-onsite-end rss-offsite-end))
		    ((string-match "\\.rdf$" href)
		     (nnrss-match-macro 
		      base-uri href rdf-onsite-end rdf-offsite-end))
		    ((string-match "\\.xml$" href)
		     (nnrss-match-macro
		      base-uri href xml-onsite-end xml-offsite-end))
		    ((string-match "rss" href)
		     (nnrss-match-macro
		      base-uri href rss-onsite-in rss-offsite-in))
		    ((string-match "rdf" href)
		     (nnrss-match-macro
		      base-uri href rdf-onsite-in rdf-offsite-in))
		    ((string-match "xml" href)
		     (nnrss-match-macro
		      base-uri href xml-onsite-in xml-offsite-in)))))
	    hrefs)
    (append 
     rss-onsite-end  rdf-onsite-end  xml-onsite-end
     rss-onsite-in   rdf-onsite-in   xml-onsite-in
     rss-offsite-end rdf-offsite-end xml-offsite-end
     rss-offsite-in rdf-offsite-in xml-offsite-in)))

(defun nnrss-discover-feed (url)
  "Given a page, find an RSS feed using Mark Pilgrim's
`ultra-liberal rss locator' (http://diveintomark.org/2002/08/15.html)."

  (let ((parsed-page (nnrss-fetch url)))

;;    1. if this url is the rss, use it.
    (if (nnrss-rss-p parsed-page)
	(let ((rss-ns (nnrss-get-namespace-prefix parsed-page "http://purl.org/rss/1.0/")))
	  (nnrss-rss-title-description rss-ns parsed-page url))

;;    2. look for the <link rel="alternate"
;;    type="application/rss+xml" and use that if it is there.
      (let ((links (nnrss-get-rsslinks parsed-page)))
	(if links
	    (let* ((xml (nnrss-fetch
			 (cdr (assoc 'href (cadar links)))))
		   (rss-ns (nnrss-get-namespace-prefix xml "http://purl.org/rss/1.0/")))
	      (nnrss-rss-title-description rss-ns xml (cdr (assoc 'href (cadar links)))))

;;    3. look for links on the site in the following order:
;;       - onsite links ending in .rss, .rdf, or .xml
;;       - onsite links containing any of the above
;;       - offsite links ending in .rss, .rdf, or .xml
;;       - offsite links containing any of the above
	  (let* ((base-uri (progn (string-match ".*://[^/]+/?" url)
				  (match-string 0 url)))
		 (hrefs (nnrss-order-hrefs 
			 base-uri (nnrss-extract-hrefs parsed-page)))
		 (rss-link nil))
	  (while (and (eq rss-link nil) (not (eq hrefs nil)))
	    (let ((href-data (nnrss-fetch (car hrefs))))
	      (if (nnrss-rss-p href-data)
		  (let* ((rss-ns (nnrss-get-namespace-prefix href-data "http://purl.org/rss/1.0/")))
		    (setq rss-link (nnrss-rss-title-description
				    rss-ns href-data (car hrefs))))
		(setq hrefs (cdr hrefs)))))
	  (if rss-link rss-link

;;    4. check syndic8
	    (nnrss-find-rss-via-syndic8 url))))))))

(defun nnrss-find-rss-via-syndic8 (url)
  "query syndic8 for the rss feeds it has for the url."
  (if (not (locate-library "xml-rpc"))
      (progn
	(message "XML-RPC is not available... not checking Syndic8.")
	nil)
    (require 'xml-rpc)
    (let ((feedid (xml-rpc-method-call
		   "http://www.syndic8.com/xmlrpc.php"
		   'syndic8.FindSites
		   url)))
      (when feedid
	(let* ((feedinfo (xml-rpc-method-call 
			  "http://www.syndic8.com/xmlrpc.php"
			  'syndic8.GetFeedInfo
			  feedid))
	       (urllist
		(delq nil 
		      (mapcar
		       (lambda (listinfo)
			 (if (string-equal 
			      (cdr (assoc "status" listinfo))
			      "Syndicated")
			     (cons
			      (cdr (assoc "sitename" listinfo))
			      (list
			       (cons 'title
				     (cdr (assoc 
					   "sitename" listinfo)))
			       (cons 'href
				     (cdr (assoc
					   "dataurl" listinfo)))))))
		       feedinfo))))
	  (if (not (> (length urllist) 1))
	      (cdar urllist)
	    (let ((completion-ignore-case t)
		  (selection 
		   (mapcar (lambda (listinfo)
			     (cons (cdr (assoc "sitename" listinfo)) 
				   (string-to-int 
				    (cdr (assoc "feedid" listinfo)))))
			   feedinfo)))
	      (cdr (assoc 
		    (completing-read
		     "Multiple feeds found.  Select one: "
		     selection nil t) urllist)))))))))

(defun nnrss-rss-p (data)
  "Test if data is an RSS feed.  Simply ensures that the first
element is rss or rdf."
  (or (eq (caar data) 'rss)
      (eq (caar data) 'rdf:RDF)))

(defun nnrss-rss-title-description (rss-namespace data url)
  "Return the title of an RSS feed."
  (if (nnrss-rss-p data)
      (let ((description (intern (concat rss-namespace "description")))
	    (title (intern (concat rss-namespace "title")))
	    (channel (nnrss-find-el (intern (concat rss-namespace "channel"))
				    data)))
	(list
	 (cons 'description (caddr (nth 0 (nnrss-find-el description channel))))
	 (cons 'title (caddr (nth 0 (nnrss-find-el title channel))))
	 (cons 'href url)))))

(defun nnrss-get-namespace-prefix (el uri)
  "Given EL (containing a parsed element) and URI (containing a string
that gives the URI for which you want to retrieve the namespace
prefix), return the prefix."
  (let* ((prefix (car (rassoc uri (cadar el))))
	 (nslist (if prefix 
		     (split-string (symbol-name prefix) ":")))
	 (ns (cond ((eq (length nslist) 1) ; no prefix given
		    "")
		   ((eq (length nslist) 2) ; extract prefix
		    (cadr nslist)))))
    (if (and ns (not (eq ns "")))
	(concat ns ":")
      ns)))

(provide 'nnrss)


;;; nnrss.el ends here

;;; arch-tag: 12910c07-0cdf-44fb-8d2c-416ded64c267
