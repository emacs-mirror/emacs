;;; nnlistserv.el --- retrieving articles via web mailing list archives

;; Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news, mail

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;;; Code:

(eval-when-compile (require 'cl))

(require 'nnoo)
(require 'mm-url)
(require 'nnweb)

(nnoo-declare nnlistserv
  nnweb)

(defvoo nnlistserv-directory (nnheader-concat gnus-directory "nnlistserv/")
  "Where nnlistserv will save its files."
  nnweb-directory)

(defvoo nnlistserv-name 'kk
  "What search engine type is being used."
  nnweb-type)

(defvoo nnlistserv-type-definition
    '((kk
       (article . nnlistserv-kk-wash-article)
       (map . nnlistserv-kk-create-mapping)
       (search . nnlistserv-kk-search)
       (address . "http://www.itk.ntnu.no/ansatte/Andresen_Trond/kk-f/%s/")
       (pages "fra160396" "fra160796" "fra061196" "fra160197"
	      "fra090997" "fra040797" "fra130397" "nye")
       (index . "date.html")
       (identifier . nnlistserv-kk-identity)))
  "Type-definition alist."
  nnweb-type-definition)

(defvoo nnlistserv-search nil
  "Search string to feed to DejaNews."
  nnweb-search)

(defvoo nnlistserv-ephemeral-p nil
  "Whether this nnlistserv server is ephemeral."
  nnweb-ephemeral-p)

;;; Internal variables

;;; Interface functions

(nnoo-define-basics nnlistserv)

(nnoo-import nnlistserv
  (nnweb))

;;; Internal functions

;;;
;;; KK functions.
;;;

(defun nnlistserv-kk-create-mapping ()
  "Perform the search and create a number-to-url alist."
  (save-excursion
    (set-buffer nnweb-buffer)
    (let ((case-fold-search t)
	  (active (or (cadr (assoc nnweb-group nnweb-group-alist))
		      (cons 1 0)))
	  (pages (nnweb-definition 'pages))
	  map url page subject from )
      (while (setq page (pop pages))
	(erase-buffer)
	(when (funcall (nnweb-definition 'search) page)
	  ;; Go through all the article hits on this page.
	  (goto-char (point-min))
	  (mm-url-decode-entities)
	  (goto-char (point-min))
	  (while (re-search-forward "^<li> *<a href=\"\\([^\"]+\\)\"><b>\\([^\\>]+\\)</b></a> *<[^>]+><i>\\([^>]+\\)<" nil t)
	    (setq url (match-string 1)
		  subject (match-string 2)
		  from (match-string 3))
	    (setq url (concat (format (nnweb-definition 'address) page) url))
	    (unless (nnweb-get-hashtb url)
	      (push
	       (list
		(incf (cdr active))
		(make-full-mail-header
		 (cdr active) subject from ""
		 (concat "<" (nnweb-identifier url) "@kk>")
		 nil 0 0 url))
	       map)
	      (nnweb-set-hashtb (cadar map) (car map))
	      (nnheader-message 5 "%s %s %s" (cdr active) (point) pages)))))
      ;; Return the articles in the right order.
      (setq nnweb-articles
	    (sort (nconc nnweb-articles map) 'car-less-than-car)))))

(defun nnlistserv-kk-wash-article ()
  (let ((case-fold-search t)
	(headers '(sent name email subject id))
	sent name email subject id)
    (mm-url-decode-entities)
    (while headers
      (goto-char (point-min))
      (re-search-forward (format "<!-- %s=\"\\([^\"]+\\)" (car headers)) nil t)
      (set (pop headers) (match-string 1)))
    (goto-char (point-min))
    (search-forward "<!-- body" nil t)
    (delete-region (point-min) (progn (forward-line 1) (point)))
    (goto-char (point-max))
    (search-backward "<!-- body" nil t)
    (delete-region (point-max) (progn (beginning-of-line) (point)))
    (mm-url-remove-markup)
    (goto-char (point-min))
    (insert (format "From: %s <%s>\n" name email)
	    (format "Subject: %s\n" subject)
	    (format "Message-ID: %s\n" id)
	    (format "Date: %s\n\n" sent))))

(defun nnlistserv-kk-search (search)
  (mm-url-insert
   (concat (format (nnweb-definition 'address) search)
	   (nnweb-definition 'index)))
  t)

(defun nnlistserv-kk-identity (url)
  "Return an unique identifier based on URL."
  url)

(provide 'nnlistserv)

;; arch-tag: 7705176f-d332-4a5e-a520-d0d319445617
;;; nnlistserv.el ends here
