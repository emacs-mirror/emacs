;;; nnwarchive.el --- interfacing with web archives
;; Copyright (C) 1999, 2000 Free Software Foundation, Inc.

;; Author: Shenghuo Zhu <zsh@cs.rochester.edu>
;; Keywords: news egroups mail-archive

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

;; Note: You need to have `url' (w3 0.46) or greater version
;; installed for this backend to work.

;; Todo: 
;; 1. To support more web archives.
;; 2. Generalize webmail to other MHonArc archive.

;;; Code:

(eval-when-compile (require 'cl))

(require 'nnoo)
(require 'message)
(require 'gnus-util)
(require 'gnus)
(require 'gnus-bcklg)
(require 'nnmail)
(require 'mm-util)
(require 'mail-source)
(eval-when-compile
  (ignore-errors
    (require 'w3)
    (require 'url)
    (require 'w3-forms)
    (require 'nnweb)))
;; Report failure to find w3 at load time if appropriate.
(eval '(progn
	 (require 'w3)
	 (require 'url)
	 (require 'w3-forms)
	 (require 'nnweb)))

(nnoo-declare nnwarchive)

(defvar nnwarchive-type-definition
  '((egroups
     (address . "www.egroups.com")
     (open-url 
      "http://www.egroups.com/login.cgi?&login_email=%s&login_password=%s" 
      nnwarchive-login nnwarchive-passwd)
     (list-url 
      "http://www.egroups.com/mygroups")
     (list-dissect . nnwarchive-egroups-list)
     (list-groups . nnwarchive-egroups-list-groups)
     (xover-url 
      "http://www.egroups.com/messages/%s/%d" group aux)
     (xover-last-url 
      "http://www.egroups.com/messages/%s/" group)
     (xover-page-size . 13)
     (xover-dissect . nnwarchive-egroups-xover)
     (article-url 
      "http://www.egroups.com/message/%s/%d?source=1" group article)
     (article-dissect . nnwarchive-egroups-article)
     (authentication . t)
     (article-offset . 0)
     (xover-files . nnwarchive-egroups-xover-files))
    (mail-archive
     (address . "www.mail-archive.com")
     (open-url)
     (list-url 
      "http://www.mail-archive.com/lists.html")
     (list-dissect . nnwarchive-mail-archive-list)
     (list-groups . nnwarchive-mail-archive-list-groups)
     (xover-url 
      "http://www.mail-archive.com/%s/mail%d.html" group aux)
     (xover-last-url 
      "http://www.mail-archive.com/%s/maillist.html" group)
     (xover-page-size)
     (xover-dissect . nnwarchive-mail-archive-xover)
     (article-url 
      "http://www.mail-archive.com/%s/msg%05d.html" group article1)
     (article-dissect . nnwarchive-mail-archive-article)
     (xover-files . nnwarchive-mail-archive-xover-files)
     (authentication)
     (article-offset . 1))))

(defvar nnwarchive-default-type 'egroups)

(defvoo nnwarchive-directory (nnheader-concat gnus-directory "warchive/")
  "Where nnwarchive will save its files.")

(defvoo nnwarchive-type nil
    "The type of nnwarchive.")

(defvoo nnwarchive-address ""
  "The address of nnwarchive.")

(defvoo nnwarchive-login nil
  "Your login name for the group.")

(defvoo nnwarchive-passwd nil
  "Your password for the group.")

(defvoo nnwarchive-groups nil)

(defvoo nnwarchive-headers-cache nil)

(defvoo nnwarchive-authentication nil)

(defvoo nnwarchive-nov-is-evil nil)

(defconst nnwarchive-version "nnwarchive 1.0")

;;; Internal variables

(defvoo nnwarchive-open-url nil)
(defvoo nnwarchive-open-dissect nil)

(defvoo nnwarchive-list-url nil)
(defvoo nnwarchive-list-dissect nil)
(defvoo nnwarchive-list-groups nil)

(defvoo nnwarchive-xover-files nil)
(defvoo nnwarchive-xover-url nil)
(defvoo nnwarchive-xover-last-url nil)
(defvoo nnwarchive-xover-dissect nil)
(defvoo nnwarchive-xover-page-size nil)

(defvoo nnwarchive-article-url nil)
(defvoo nnwarchive-article-dissect nil)
(defvoo nnwarchive-xover-files nil)
(defvoo nnwarchive-article-offset 0)

(defvoo nnwarchive-buffer nil)

(defvoo nnwarchive-keep-backlog 300)
(defvar nnwarchive-backlog-articles nil)
(defvar nnwarchive-backlog-hashtb nil)

(defvoo nnwarchive-headers nil)


;;; Interface functions

(nnoo-define-basics nnwarchive)

(defun nnwarchive-set-default (type)
  (let ((defs (cdr (assq type nnwarchive-type-definition)))
	def)
    (dolist (def defs)
      (set (intern (concat "nnwarchive-" (symbol-name (car def)))) 
	   (cdr def)))))

(defmacro nnwarchive-backlog (&rest form)
  `(let ((gnus-keep-backlog nnwarchive-keep-backlog)
	 (gnus-backlog-buffer 
	  (format " *nnwarchive backlog %s*" nnwarchive-address))
	 (gnus-backlog-articles nnwarchive-backlog-articles)
	 (gnus-backlog-hashtb nnwarchive-backlog-hashtb))
     (unwind-protect
	 (progn ,@form)
       (setq nnwarchive-backlog-articles gnus-backlog-articles
	     nnwarchive-backlog-hashtb gnus-backlog-hashtb))))
(put 'nnwarchive-backlog 'lisp-indent-function 0)
(put 'nnwarchive-backlog 'edebug-form-spec '(form body))

(defun nnwarchive-backlog-enter-article (group number buffer)
  (nnwarchive-backlog
    (gnus-backlog-enter-article group number buffer)))

(defun nnwarchive-get-article (article &optional group server buffer) 
  (if (numberp article)
      (if (nnwarchive-backlog
	    (gnus-backlog-request-article group article 
					  (or buffer nntp-server-buffer)))
	  (cons group article)
	(let (contents)
	  (save-excursion
	    (set-buffer nnwarchive-buffer)
	    (goto-char (point-min))
	    (let ((article1 (- article nnwarchive-article-offset)))
	      (nnwarchive-url nnwarchive-article-url))
	    (setq contents (funcall nnwarchive-article-dissect group article)))
	  (when contents
	    (save-excursion
	      (set-buffer (or buffer nntp-server-buffer))
	      (erase-buffer)
	      (insert contents)
	      (nnwarchive-backlog-enter-article group article (current-buffer))
	      (nnheader-report 'nnwarchive "Fetched article %s" article)
	      (cons group article)))))
    nil))

(deffoo nnwarchive-retrieve-headers (articles &optional group server fetch-old)
  (nnwarchive-possibly-change-server group server)
  (if (or gnus-nov-is-evil nnwarchive-nov-is-evil)
      (with-temp-buffer
	(with-current-buffer nntp-server-buffer
	  (erase-buffer))
	(let ((buf (current-buffer)) b e)
	  (dolist (art articles)
	    (nnwarchive-get-article art group server buf)
	    (setq b (goto-char (point-min)))
	    (if (search-forward "\n\n" nil t)
		(forward-char -1)
	      (goto-char (point-max)))
	    (setq e (point))
	    (with-current-buffer nntp-server-buffer
	      (insert (format "221 %d Article retrieved.\n" art))
	      (insert-buffer-substring buf b e)
	      (insert ".\n"))))
	'headers)
    (setq nnwarchive-headers (cdr (assoc group nnwarchive-headers-cache)))
    (save-excursion
      (set-buffer nnwarchive-buffer)
      (erase-buffer)
      (funcall nnwarchive-xover-files group articles))
    (save-excursion
      (set-buffer nntp-server-buffer)
      (erase-buffer)
      (let (header)
      (dolist (art articles)
	(if (setq header (assq art nnwarchive-headers))
	    (nnheader-insert-nov (cdr header))))))
    (let ((elem (assoc group nnwarchive-headers-cache)))
      (if elem
	  (setcdr elem nnwarchive-headers)
	(push (cons group nnwarchive-headers) nnwarchive-headers-cache)))
    'nov))

(deffoo nnwarchive-request-group (group &optional server dont-check)
  (nnwarchive-possibly-change-server nil server)
  (when (and (not dont-check) nnwarchive-list-groups)
    (funcall nnwarchive-list-groups (list group))
    (nnwarchive-write-groups))
  (let ((elem (assoc group nnwarchive-groups)))
    (cond
     ((not elem)
      (nnheader-report 'nnwarchive "Group does not exist"))
     (t
      (nnheader-report 'nnwarchive "Opened group %s" group)
      (nnheader-insert
       "211 %d %d %d %s\n" (or (cadr elem) 0) 1 (or (cadr elem) 0)
       (prin1-to-string group))
      t))))

(deffoo nnwarchive-request-article (article &optional group server buffer)
  (nnwarchive-possibly-change-server group server)
  (nnwarchive-get-article article group server buffer))

(deffoo nnwarchive-close-server (&optional server)
  (when (and (nnwarchive-server-opened server)
	     (gnus-buffer-live-p nnwarchive-buffer))
    (save-excursion
      (set-buffer nnwarchive-buffer)
      (kill-buffer nnwarchive-buffer)))
  (nnwarchive-backlog
    (gnus-backlog-shutdown))
  (nnoo-close-server 'nnwarchive server))

(deffoo nnwarchive-request-list (&optional server)
  (nnwarchive-possibly-change-server nil server)
  (save-excursion
    (set-buffer nnwarchive-buffer)
    (erase-buffer)
    (if nnwarchive-list-url
	(nnwarchive-url nnwarchive-list-url))
    (if nnwarchive-list-dissect
	(funcall nnwarchive-list-dissect))
    (nnwarchive-write-groups)
    (nnwarchive-generate-active))
  t)

(deffoo nnwarchive-open-server (server &optional defs connectionless)
  (nnoo-change-server 'nnwarchive server defs)
  (nnwarchive-init server)
  (when nnwarchive-authentication
    (setq nnwarchive-login
	  (or nnwarchive-login
	      (read-string
		 (format "Login at %s: " server)
		 user-mail-address)))
    (setq nnwarchive-passwd
	  (or nnwarchive-passwd
	      (mail-source-read-passwd
	       (format "Password for %s at %s: " 
		       nnwarchive-login server)))))
  (unless nnwarchive-groups
    (nnwarchive-read-groups))
  (save-excursion
    (set-buffer nnwarchive-buffer)
    (erase-buffer)
    (if nnwarchive-open-url
	(nnwarchive-url nnwarchive-open-url))
    (if nnwarchive-open-dissect
	(funcall nnwarchive-open-dissect)))
  t)

(nnoo-define-skeleton nnwarchive)

;;; Internal functions

(defun nnwarchive-possibly-change-server (&optional group server)
  (nnwarchive-init server)
  (when (and server
	     (not (nnwarchive-server-opened server)))
    (nnwarchive-open-server server)))

(defun nnwarchive-read-groups ()
  (let ((file (expand-file-name (concat "groups-" nnwarchive-address) 
				nnwarchive-directory)))
    (when (file-exists-p file)
      (with-temp-buffer
	(insert-file-contents file)
	(goto-char (point-min))
	(setq nnwarchive-groups (read (current-buffer)))))))

(defun nnwarchive-write-groups ()
  (with-temp-file (expand-file-name (concat "groups-" nnwarchive-address) 
				    nnwarchive-directory)
    (prin1 nnwarchive-groups (current-buffer))))

(defun nnwarchive-init (server)
  "Initialize buffers and such."
  (let ((type (intern server)) (defs nnwarchive-type-definition) def)
    (cond 
     ((equal server "")
      (setq type nnwarchive-default-type))
     ((assq type nnwarchive-type-definition) t)
     (t
      (setq type nil)
      (while (setq def (pop defs))
	(when (equal (cdr (assq 'address (cdr def))) server)
	  (setq defs nil)
	  (setq type (car def))))
      (unless type
	(error "Undefined server %s" server))))
    (setq nnwarchive-type type))
  (unless (file-exists-p nnwarchive-directory)
    (gnus-make-directory nnwarchive-directory))
  (unless (gnus-buffer-live-p nnwarchive-buffer)
    (setq nnwarchive-buffer
	  (save-excursion
	    (nnheader-set-temp-buffer
	     (format " *nnwarchive %s %s*" nnwarchive-type server)))))
  (nnwarchive-set-default nnwarchive-type))

(defun nnwarchive-encode-www-form-urlencoded (pairs)
  "Return PAIRS encoded for forms."
  (mapconcat
   (function
    (lambda (data)
      (concat (w3-form-encode-xwfu (car data)) "="
	      (w3-form-encode-xwfu (cdr data)))))
   pairs "&"))

(defun nnwarchive-fetch-form (url pairs)
  (let ((url-request-data (nnwarchive-encode-www-form-urlencoded pairs))
	(url-request-method "POST")
	(url-request-extra-headers
	 '(("Content-type" . "application/x-www-form-urlencoded"))))
    (nnweb-insert url))
  t)

(defun nnwarchive-eval (expr)
  (cond
   ((consp expr)
    (cons (nnwarchive-eval (car expr)) (nnwarchive-eval (cdr expr))))
   ((symbolp expr)
    (eval expr))
   (t
    expr)))

(defun nnwarchive-url (xurl)
  (mm-with-unibyte-current-buffer
    (let ((url-confirmation-func 'identity)
	  (url-cookie-multiple-line nil))
      (cond 
       ((eq (car xurl) 'post)
	(pop xurl)
	(nnwarchive-fetch-form (car xurl) (nnwarchive-eval (cdr xurl))))
       (t
	(nnweb-insert (apply 'format (nnwarchive-eval xurl))))))))
  
(defun nnwarchive-generate-active ()
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (dolist (elem nnwarchive-groups)
      (insert (prin1-to-string (car elem))
	      " " (number-to-string (or (cadr elem) 0)) " 1 y\n"))))

(defun nnwarchive-paged (articles)
  (let (art narts next)
    (while (setq art (pop articles))
      (when (and (>= art (or next 0))
		 (not (assq art nnwarchive-headers)))
	(push art narts)
	(setq next (+ art nnwarchive-xover-page-size))))
    narts))

;; egroups

(defun nnwarchive-egroups-list-groups (groups)
  (save-excursion
    (let (articles)
      (set-buffer nnwarchive-buffer)
      (dolist (group groups) 
	(erase-buffer)
	(nnwarchive-url nnwarchive-xover-last-url)
	(goto-char (point-min))
	(when (re-search-forward "of \\([0-9]+\\)[ \t\n\r]*</title>" nil t)
	  (setq articles (string-to-number (match-string 1)))) 
	(let ((elem (assoc group nnwarchive-groups)))
	  (if elem
	      (setcar (cdr elem) articles)
	    (push (list group articles "") nnwarchive-groups)))
	(setq nnwarchive-headers (cdr (assoc group nnwarchive-headers-cache)))
	(nnwarchive-egroups-xover group)
	(let ((elem (assoc group nnwarchive-headers-cache)))
	  (if elem
	      (setcdr elem nnwarchive-headers)
	    (push (cons group nnwarchive-headers) nnwarchive-headers-cache)))))))

(defun nnwarchive-egroups-list ()
  (let ((case-fold-search t)
	group description elem articles)
    (goto-char (point-min))
    (while 
	(re-search-forward "href=\"/group/\\([^/\"\> ]+\\)" nil t)
      (setq group (match-string 1)
	    description (match-string 2))
      (if (setq elem (assoc group nnwarchive-groups))
	  (setcar (cdr elem) 0)
	(push (list group articles description) nnwarchive-groups))))
  t)

(defun nnwarchive-egroups-xover (group)
  (let (article subject from date)
    (goto-char (point-min))
    (while (re-search-forward
	    "<a href=\"/group/\\([^/]+\\)/\\([0-9]+\\)[^>]+>\\([^<]+\\)<"
	    nil t)
      (setq group  (match-string 1)
	    article (string-to-number (match-string 2))
	    subject (match-string 3))
      (forward-line 1)
      (unless (assq article nnwarchive-headers)
	(if (looking-at "<td[^>]+><font[^>]+>\\([^<]+\\)</font>")
	    (setq from (match-string 1)))
	(forward-line 1)
	(if (looking-at "<td[^>]+><font[^>]+>\\([^<]+\\)</font>")
	    (setq date (identity (match-string 1))))
	(push (cons
	       article
	       (make-full-mail-header
		article 
		(nnweb-decode-entities-string subject)
		(nnweb-decode-entities-string from)
		date
		(concat "<" group "%"
			(number-to-string article) 
			"@egroup.com>")
		""
		0 0 "")) nnwarchive-headers))))
  nnwarchive-headers)

(defun nnwarchive-egroups-article (group articles)
  (goto-char (point-min))
  (if (search-forward "<pre>" nil t)
      (delete-region (point-min) (point)))
  (goto-char (point-max))
  (if (search-backward "</pre>" nil t)
      (delete-region (point) (point-max)))
  (goto-char (point-min))
  (while (re-search-forward "<a[^>]+>\\([^<]+\\)</a>" nil t)
    (replace-match "\\1"))
  (nnweb-decode-entities)
  (buffer-string))

(defun nnwarchive-egroups-xover-files (group articles)
  (let (aux auxs)
    (setq auxs (nnwarchive-paged (sort articles '<)))
    (while (setq aux (pop auxs))
      (goto-char (point-max))
      (nnwarchive-url nnwarchive-xover-url))
    (if nnwarchive-xover-dissect
	(nnwarchive-egroups-xover group))))

;; mail-archive

(defun nnwarchive-mail-archive-list-groups (groups)
  (save-excursion
    (let (articles)
      (set-buffer nnwarchive-buffer)
      (dolist (group groups)
	(erase-buffer)
	(nnwarchive-url nnwarchive-xover-last-url)
	(goto-char (point-min))
	(when (re-search-forward "msg\\([0-9]+\\)\\.html" nil t)
	  (setq articles (1+ (string-to-number (match-string 1)))))
	(let ((elem (assoc group nnwarchive-groups)))
	  (if elem
	      (setcar (cdr elem) articles)
	    (push (list group articles "") nnwarchive-groups)))
	(setq nnwarchive-headers (cdr (assoc group nnwarchive-headers-cache)))
	(nnwarchive-mail-archive-xover group)
	(let ((elem (assoc group nnwarchive-headers-cache)))
	  (if elem
	      (setcdr elem nnwarchive-headers)
	    (push (cons group nnwarchive-headers) 
		  nnwarchive-headers-cache)))))))

(defun nnwarchive-mail-archive-list ()
  (let ((case-fold-search t)
	group description elem articles)
    (goto-char (point-min))
    (while (re-search-forward "<a href=\"\\([^/]+\\)/\">\\([^>]+\\)<" nil t)
      (setq group (match-string 1)
	    description (match-string 2))
      (forward-line 1)
      (setq articles 0)
      (if (setq elem (assoc group nnwarchive-groups))
	  (setcar (cdr elem) articles)
	(push (list group articles description) nnwarchive-groups))))
  t)

(defun nnwarchive-mail-archive-xover (group)
  (let (article subject from date)
    (goto-char (point-min))
    (while (re-search-forward
	    "<A[^>]*HREF=\"msg\\([0-9]+\\)\\.html[^>]+>\\([^<]+\\)<"
	    nil t)
      (setq article (1+ (string-to-number (match-string 1)))
	    subject (match-string 2))
      (forward-line 1)
      (unless (assq article nnwarchive-headers)
	(if (looking-at "<UL><LI><EM>From</EM>:\\([^&]+\\)<\\([^&]+\\)>")
	    (progn
	      (setq from (match-string 1)
		    date (identity (match-string 2))))
	  (setq from "" date ""))
	(push (cons
	       article
	       (make-full-mail-header
		article 
		(nnweb-decode-entities-string subject)
		(nnweb-decode-entities-string from)
		date
		(format "<%05d%%%s>\n" (1- article) group)
		""
		0 0 "")) nnwarchive-headers))))
  nnwarchive-headers)

(defun nnwarchive-mail-archive-xover-files (group articles)
  (unless nnwarchive-headers
    (erase-buffer)
    (nnwarchive-url nnwarchive-xover-last-url)
    (goto-char (point-min))
    (nnwarchive-mail-archive-xover group))
  (let ((minart (apply 'min articles))
	(min (apply 'min (mapcar 'car nnwarchive-headers)))
	(aux 2))
    (while (> min minart)
      (erase-buffer)
      (nnwarchive-url nnwarchive-xover-url)
      (nnwarchive-mail-archive-xover group)
      (setq min (apply 'min (mapcar 'car nnwarchive-headers))))))

(defvar nnwarchive-caesar-translation-table nil
  "Modified rot13 table. tr/@A-Z[a-z/N-Z[@A-Mn-za-m/.")

(defun nnwarchive-make-caesar-translation-table ()
  "Create modified rot13 table. tr/@A-Z[a-z/N-Z[@A-Mn-za-m/."
  (let ((i -1)
	(table (make-string 256 0))
	(a (mm-char-int ?a))
	(A (mm-char-int ?A)))
    (while (< (incf i) 256)
      (aset table i i))
    (concat
     (substring table 0 (1- A))
     (substring table (+ A 13) (+ A 27))
     (substring table (1- A) (+ A 13))
     (substring table (+ A 27) a)
     (substring table (+ a 13) (+ a 26))
     (substring table a (+ a 13))
     (substring table (+ a 26) 255))))

(defun nnwarchive-from-r13 (from-r13)
  (when from-r13
    (with-temp-buffer
      (insert from-r13)
      (let ((message-caesar-translation-table
	     (or nnwarchive-caesar-translation-table
		 (setq nnwarchive-caesar-translation-table 
		       (nnwarchive-make-caesar-translation-table)))))
	(message-caesar-region (point-min) (point-max))
	(buffer-string)))))

(defun nnwarchive-mail-archive-article (group article)
  (let (p refs url mime e 
	  from subject date id 
	  done
	  (case-fold-search t))
    (save-restriction
      (goto-char (point-min))
      (when (search-forward "X-Head-End" nil t)
	(beginning-of-line)
	(narrow-to-region (point-min) (point))
	(nnweb-decode-entities)
	(goto-char (point-min))
	(while (search-forward "<!--X-" nil t)
	  (replace-match ""))
	(goto-char (point-min))
	(while (search-forward " -->" nil t)
	  (replace-match ""))
	(setq from 
	      (or (mail-fetch-field "from")
		  (nnwarchive-from-r13 
		   (mail-fetch-field "from-r13"))))
	(setq date (mail-fetch-field "date"))
	(setq id (mail-fetch-field "message-id"))
	(setq subject (mail-fetch-field "subject"))
	(goto-char (point-max))
	(widen))
      (when (search-forward "<ul>" nil t)
	(forward-line)
	(delete-region (point-min) (point))
	(search-forward "</ul>" nil t)
	(end-of-line)
	(narrow-to-region (point-min) (point))
	(nnweb-remove-markup)
	(nnweb-decode-entities)
	(goto-char (point-min))
	(delete-blank-lines)
	(when from
	  (message-remove-header "from")
	  (goto-char (point-max))
	  (insert "From: " from "\n"))
	(when subject
	  (message-remove-header "subject")
	  (goto-char (point-max))
	  (insert "Subject: " subject "\n"))
	(when id
	  (goto-char (point-max))
	  (insert "X-Message-ID: <" id ">\n"))
	(when date
	  (message-remove-header "date")
	  (goto-char (point-max))
	  (insert "Date: " date "\n"))
	(goto-char (point-max))
	(widen)
	(insert "\n"))
      (setq p (point)) 
      (when (search-forward "X-Body-of-Message" nil t)
	(forward-line)
	(delete-region p (point))
	(search-forward "X-Body-of-Message-End" nil t)
	(beginning-of-line)
	(save-restriction
	  (narrow-to-region p (point))
	  (goto-char (point-min))
	  (if (> (skip-chars-forward "\040\n\r\t") 0)
	      (delete-region (point-min) (point)))
	  (while (not (eobp))
	    (cond 
	     ((looking-at "<PRE>\r?\n?") 
	      (delete-region (match-beginning 0) (match-end 0))
	      (setq p (point))
	      (when (search-forward "</PRE>" nil t)
		(delete-region (match-beginning 0) (match-end 0))
		(save-restriction
		  (narrow-to-region p (point))
		  (nnweb-remove-markup)
		  (nnweb-decode-entities)
		  (goto-char (point-max)))))
	     ((looking-at "<P><A HREF=\"\\([^\"]+\\)")
	      (setq url (match-string 1))
	      (delete-region (match-beginning 0) 
			     (progn (forward-line) (point)))
	      ;; I hate to download the url encode it, then immediately 
	      ;; decode it.
	      ;; FixMe: Find a better solution to attach the URL.
	      ;; Maybe do some hack in external part of mml-generate-mim-1.
	      (insert "<#part>"
		      "\n--\nExternal: \n"
		      (format "<URL:http://www.mail-archive.com/%s/%s>" 
			      group url)
		      "\n--\n"
		      "<#/part>")
	      (setq mime t))
	     (t
	      (setq p (point))
	      (insert "<#part type=\"text/html\" disposition=inline>")
	      (goto-char
	       (if (re-search-forward 
		    "[\040\n\r\t]*<PRE>\\|[\040\n\r\t]*<P><A HREF=\"" 
		    nil t)
		   (match-beginning 0)
		 (point-max)))
	      (insert "<#/part>")
	      (setq mime t)))
	    (setq p (point))
	    (if (> (skip-chars-forward "\040\n\r\t") 0)
		(delete-region p (point))))
	  (goto-char (point-max))))
      (setq p (point))
      (when (search-forward "X-References-End" nil t)
	(setq e (point))
	(beginning-of-line)
	(search-backward "X-References" p t)
	(while (re-search-forward "msg\\([0-9]+\\)\\.html" e t)
	  (push (concat "<" (match-string 1) "%" group ">") refs)))
      (delete-region p (point-max))
      (goto-char (point-min))
      (insert (format "Message-ID: <%05d%%%s>\n" (1- article) group))
      (when refs
	(insert "References:")
	(while refs
	  (insert " " (pop refs)))
	(insert "\n"))
      (when mime
	(unless (looking-at "$") 
	  (search-forward "\n\n" nil t)
	  (forward-line -1))
	(narrow-to-region (point) (point-max))
	(insert "MIME-Version: 1.0\n"
		(prog1
		    (mml-generate-mime)
		  (delete-region (point-min) (point-max))))
	(widen)))
    (buffer-string)))

(provide 'nnwarchive)

;;; nnwarchive.el ends here
