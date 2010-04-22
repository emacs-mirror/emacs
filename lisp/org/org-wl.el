;;; org-wl.el --- Support for links to Wanderlust messages from within Org-mode

;; Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010
;;   Free Software Foundation, Inc.

;; Author: Tokuya Kameshima <kames at fa2 dot so-net dot ne dot jp>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 6.35i
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

;; This file implements links to Wanderlust messages from within Org-mode.
;; Org-mode loads this module by default - if this is not what you want,
;; configure the variable `org-modules'.

;;; Code:

(require 'org)

(defgroup org-wl nil
 "Options concerning the Wanderlust link."
 :tag "Org Startup"
 :group 'org-link)

(defcustom org-wl-link-to-refile-destination t
 "Create a link to the refile destination if the message is marked as refile."
 :group 'org-wl
 :type 'boolean)

;; Declare external functions and variables
(declare-function elmo-folder-exists-p "ext:elmo" (folder) t)
(declare-function elmo-message-entity-field "ext:elmo-msgdb"
		  (entity field &optional type))
(declare-function elmo-message-field "ext:elmo"
		  (folder number field &optional type) t)
(declare-function elmo-msgdb-overview-get-entity "ext:elmo" (id msgdb) t)
;; Backward compatibility to old version of wl
(declare-function wl "ext:wl" () t)
(declare-function wl-summary-buffer-msgdb "ext:wl-folder" () t)
(declare-function wl-summary-jump-to-msg-by-message-id "ext:wl-summary"
		  (&optional id))
(declare-function wl-summary-line-from "ext:wl-summary" ())
(declare-function wl-summary-line-subject "ext:wl-summary" ())
(declare-function wl-summary-message-number "ext:wl-summary" ())
(declare-function wl-summary-redisplay "ext:wl-summary" (&optional arg))
(declare-function wl-summary-registered-temp-mark "ext:wl-action" (number))
(declare-function wl-folder-goto-folder-subr "ext:wl-folder"
		  (&optional folder sticky))
(defvar wl-init)
(defvar wl-summary-buffer-elmo-folder)
(defvar wl-summary-buffer-folder-name)

;; Install the link type
(org-add-link-type "wl" 'org-wl-open)
(add-hook 'org-store-link-functions 'org-wl-store-link)

;; Implementation
(defun org-wl-store-link ()
 "Store a link to a WL folder or message."
 (when (eq major-mode 'wl-summary-mode)
   (let* ((msgnum (wl-summary-message-number))
	   (mark-info (wl-summary-registered-temp-mark msgnum))
	   (folder-name
	    (if (and org-wl-link-to-refile-destination
		     mark-info
		     (equal (nth 1 mark-info) "o")) ; marked as refile
		(nth 2 mark-info)
	      wl-summary-buffer-folder-name))
	   (message-id (elmo-message-field wl-summary-buffer-elmo-folder
					   msgnum 'message-id))
	   (wl-message-entity
	    (if (fboundp 'elmo-message-entity)
		(elmo-message-entity
		 wl-summary-buffer-elmo-folder msgnum)
	      (elmo-msgdb-overview-get-entity
	       msgnum (wl-summary-buffer-msgdb))))
	   (from (let ((from-field (elmo-message-entity-field wl-message-entity
							      'from)))
		   (if (listp from-field)
		       (car from-field)
		     from-field)))
	   (to (let ((to-field (elmo-message-entity-field wl-message-entity
							  'to)))
		 (if (listp to-field)
		     (car to-field)
		   to-field)))
	   (subject (let (wl-thr-indent-string wl-parent-message-entity)
		      (wl-summary-line-subject)))
	   desc link)
     ;; remove text properties of subject string to avoid possible bug
     ;; when formatting the subject
     (set-text-properties 0 (length subject) nil subject)

     (org-store-link-props :type "wl" :from from :to to
			    :subject subject :message-id message-id)
     (setq message-id (org-remove-angle-brackets message-id))
     (setq desc (org-email-link-description))
     (setq link (org-make-link "wl:" folder-name
				"#" message-id))
     (org-add-link-props :link link :description desc)
     link)))

(defun org-wl-open (path)
 "Follow the WL message link specified by PATH."
 (require 'wl)
 (unless wl-init (wl))
 ;; XXX: The imap-uw's MH folder names start with "%#".
 (if (not (string-match "\\`\\(\\(?:%#\\)?[^#]+\\)\\(#\\(.*\\)\\)?" path))
     (error "Error in Wanderlust link"))
 (let ((folder (match-string 1 path))
	(article (match-string 3 path)))
   (if (not (elmo-folder-exists-p (org-no-warnings
				   (wl-folder-get-elmo-folder folder))))
	(error "No such folder: %s" folder))
   (let ((old-buf (current-buffer))
	  (old-point (point-marker)))
     (wl-folder-goto-folder-subr folder)
     (save-excursion
	;; XXX: `wl-folder-goto-folder-subr' moves point to the
	;; beginning of the current line.  So, restore the point
	;; in the old buffer.
	(set-buffer old-buf)
	(goto-char old-point))
     (and (wl-summary-jump-to-msg-by-message-id (org-add-angle-brackets
						  article))
	   (wl-summary-redisplay)))))

(provide 'org-wl)

;; arch-tag: 29b75a0f-ef2e-430b-8abc-acff75bde54a

;;; org-wl.el ends here
