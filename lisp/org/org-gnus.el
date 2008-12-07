;;; org-gnus.el --- Support for links to Gnus groups and messages from within Org-mode

;; Copyright (C) 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;;         Tassilo Horn <tassilo at member dot fsf dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 6.14
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file implements links to Gnus groups and messages from within Org-mode.
;; Org-mode loads this module by default - if this is not what you want,
;; configure the variable `org-modules'.

;;; Code:

(require 'org)
(eval-when-compile
  (require 'gnus-sum))

;; Customization variables

(when (fboundp 'defvaralias)
  (defvaralias 'org-usenet-links-prefer-google 'org-gnus-prefer-web-links))

(defcustom org-gnus-prefer-web-links nil
  "Non-nil means, `org-store-link' will create web links to Google groups.
When nil, Gnus will be used for such links.
Using a prefix arg to the command \\[org-store-link] (`org-store-link')
negates this setting for the duration of the command."
  :group 'org-link-store
  :type 'boolean)

;; Declare external functions and variables
(declare-function gnus-article-show-summary "gnus-art" ())
(declare-function gnus-summary-last-subject "gnus-sum" ())
(defvar gnus-other-frame-object)
(defvar gnus-group-name)
(defvar gnus-article-current)

;; Install the link type
(org-add-link-type "gnus" 'org-gnus-open)
(add-hook 'org-store-link-functions 'org-gnus-store-link)

;; Implementation

(defun org-gnus-group-link (group)
  "Create a link to the Gnus group GROUP.
If GROUP is a newsgroup and `org-gnus-prefer-web-links' is
non-nil, create a link to groups.google.com or gmane.org.
Otherwise create a link to the group inside Gnus.

If `org-store-link' was called with a prefix arg the meaning of
`org-gnus-prefer-web-links' is reversed."
  (let ((unprefixed-group (replace-regexp-in-string "^[^:]+:" "" group)))
    (if (and (string-match "^nntp" group) ;; Only for nntp groups
	     (org-xor current-prefix-arg
		      org-gnus-prefer-web-links))
	(concat (if (string-match "gmane" unprefixed-group)
		    "http://news.gmane.org/"
		  "http://groups.google.com/group/")
		unprefixed-group)
      (concat "gnus:" group))))

(defun org-gnus-article-link (group newsgroups message-id x-no-archive)
  "Create a link to a Gnus article.
The article is specified by its MESSAGE-ID.  Additional
parameters are the Gnus GROUP, the NEWSGROUPS the article was
posted to and the X-NO-ARCHIVE header value of that article.

If GROUP is a newsgroup and `org-gnus-prefer-web-links' is
non-nil, create a link to groups.google.com or gmane.org.
Otherwise create a link to the article inside Gnus.

If `org-store-link' was called with a prefix arg the meaning of
`org-gnus-prefer-web-links' is reversed."
  (if (and (org-xor current-prefix-arg org-gnus-prefer-web-links)
	   newsgroups	  ;; Make web links only for nntp groups
	   (not x-no-archive)) ;; and if X-No-Archive isn't set.
      (format (if (string-match "gmane\\." newsgroups)
		  "http://mid.gmane.org/%s"
		"http://groups.google.com/groups/search?as_umsgid=%s")
	      (org-fixup-message-id-for-http
	       (replace-regexp-in-string "[<>]" "" message-id)))
    (org-make-link "gnus:" group "#" message-id)))

(defun org-gnus-store-link ()
  "Store a link to a Gnus folder or message."
  (cond
   ((eq major-mode 'gnus-group-mode)
    (let* ((group (cond ((fboundp 'gnus-group-group-name) ; depending on Gnus
			 (gnus-group-group-name))         ; version
			((fboundp 'gnus-group-name)
			 (gnus-group-name))
			(t "???")))
	   desc link)
      (unless group (error "Not on a group"))
      (org-store-link-props :type "gnus" :group group)
      (setq desc (org-gnus-group-link group)
	    link (org-make-link desc))
      (org-add-link-props :link link :description desc)
      link))

   ((memq major-mode '(gnus-summary-mode gnus-article-mode))
    (and (eq major-mode 'gnus-summary-mode) (gnus-summary-show-article))
    (let* ((group gnus-newsgroup-name)
	   (header (with-current-buffer gnus-article-buffer
		     (gnus-summary-toggle-header 1)
		     (goto-char (point-min))
		     (mail-header-extract-no-properties)))
	   (from (mail-header 'from header))
	   (message-id (mail-header 'message-id header))
	   (date (mail-header 'date header))
	   (to (mail-header 'to header))
	   (newsgroups (mail-header 'newsgroups header))
	   (x-no-archive (mail-header 'x-no-archive header))
	   (subject (gnus-summary-subject-string))
	   desc link)
      (org-store-link-props :type "gnus" :from from :subject subject
			    :message-id message-id :group group :to to)
      (setq desc (org-email-link-description)
	    link (org-gnus-article-link group newsgroups message-id x-no-archive))
      (org-add-link-props :link link :description desc)
      (gnus-summary-toggle-header -1)
      link))))

(defun org-gnus-open (path)
  "Follow the Gnus message or folder link specified by PATH."
  (let (group article)
    (if (not (string-match "\\`\\([^#]+\\)\\(#\\(.*\\)\\)?" path))
	(error "Error in Gnus link"))
    (setq group (match-string 1 path)
	  article (match-string 3 path))
    (org-gnus-follow-link group article)))

(defun org-gnus-follow-link (&optional group article)
  "Follow a Gnus link to GROUP and ARTICLE."
  (require 'gnus)
  (funcall (cdr (assq 'gnus org-link-frame-setup)))
  (if gnus-other-frame-object (select-frame gnus-other-frame-object))
  (cond ((and group article)
	 (gnus-group-read-group 1 nil group)
	 (gnus-summary-goto-article
	  (if (string-match "[^0-9]" article)
	      article
	    (string-to-number article))
	  nil t))
	(group (gnus-group-jump-to-group group))))

(defun org-gnus-no-new-news ()
  "Like `M-x gnus' but doesn't check for new news."
  (if (not (gnus-alive-p)) (gnus)))

(provide 'org-gnus)

;; arch-tag: 512e0840-58fa-45b3-b456-71e10fa2376d

;;; org-gnus.el ends here
