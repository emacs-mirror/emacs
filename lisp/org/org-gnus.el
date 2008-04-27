;;; org-gnus.el --- Support for links to Gnus groups and messages from within Org-mode

;; Copyright (C) 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 6.02b
;;
;; This file is part of GNU Emacs.
;;
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

(defcustom org-usenet-links-prefer-google nil
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
(defun org-gnus-store-link ()
  "Store a link to a Gnus folder or message."
  (cond
   ((eq major-mode 'gnus-group-mode)
    (let ((group (cond ((fboundp 'gnus-group-group-name) ; depending on Gnus
			(gnus-group-group-name))         ; version
		       ((fboundp 'gnus-group-name)
			(gnus-group-name))
		       (t "???")))
	  desc link)
      (unless group (error "Not on a group"))
      (org-store-link-props :type "gnus" :group group)
      (setq desc (concat
		  (if (org-xor current-prefix-arg
			       org-usenet-links-prefer-google)
		      "http://groups.google.com/groups?group="
		    "gnus:")
		  group)
	    link (org-make-link desc))
      (org-add-link-props :link link :description desc)
      link))

   ((memq major-mode '(gnus-summary-mode gnus-article-mode))
    (and (eq major-mode 'gnus-article-mode) (gnus-article-show-summary))
    (let* ((group gnus-newsgroup-name)
	   (article (gnus-summary-article-number))
	   (header (gnus-summary-article-header article))
	   (from (mail-header-from header))
	   (message-id (mail-header-id header))
	   (date (mail-header-date header))
	   (subject (gnus-summary-subject-string))
	   desc link)
      (org-store-link-props :type "gnus" :from from :subject subject
			    :message-id message-id :group group)
      (setq desc (org-email-link-description))
      (if (org-xor current-prefix-arg org-usenet-links-prefer-google)
	  (setq link
		(concat
		 desc "\n  "
		 (format "http://groups.google.com/groups?as_umsgid=%s"
			 (org-fixup-message-id-for-http message-id))))
	(setq link (org-make-link "gnus:" group
				  "#" (number-to-string article))))
      (org-add-link-props :link link :description desc)
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
	 (gnus-summary-goto-article (string-to-number article) nil t))
	(group (gnus-group-jump-to-group group))))

(provide 'org-gnus)

;;; org-gnus.el ends here
