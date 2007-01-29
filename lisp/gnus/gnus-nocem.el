;;; gnus-nocem.el --- NoCeM pseudo-cancellation treatment

;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007 Free Software Foundation, Inc.

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

;;; Code:

(eval-when-compile (require 'cl))

(require 'gnus)
(require 'nnmail)
(require 'gnus-art)
(require 'gnus-sum)
(require 'gnus-range)

(defgroup gnus-nocem nil
  "NoCeM pseudo-cancellation treatment."
  :group 'gnus-score)

(defcustom gnus-nocem-groups
  '("news.lists.filters" "news.admin.net-abuse.bulletins"
    "alt.nocem.misc" "news.admin.net-abuse.announce")
  "*List of groups that will be searched for NoCeM messages."
  :group 'gnus-nocem
  :type '(repeat (string :tag "Group")))

(defcustom gnus-nocem-issuers
  '("AutoMoose-1"			; CancelMoose[tm]
    "clewis@ferret.ocunix"		; Chris Lewis
    "cosmo.roadkill"
    "SpamHippo"
    "hweede@snafu.de")
  "*List of NoCeM issuers to pay attention to.

This can also be a list of `(ISSUER CONDITION ...)' elements.

See <URL:http://www.xs4all.nl/~rosalind/nocemreg/nocemreg.html> for an
issuer registry."
  :group 'gnus-nocem
  :link '(url-link "http://www.xs4all.nl/~rosalind/nocemreg/nocemreg.html")
  :type '(repeat (choice string sexp)))

(defcustom gnus-nocem-directory
  (nnheader-concat gnus-article-save-directory "NoCeM/")
  "*Directory where NoCeM files will be stored."
  :group 'gnus-nocem
  :type 'directory)

(defcustom gnus-nocem-expiry-wait 15
  "*Number of days to keep NoCeM headers in the cache."
  :group 'gnus-nocem
  :type 'integer)

(defcustom gnus-nocem-verifyer 'pgg-verify
  "*Function called to verify that the NoCeM message is valid.
One likely value is `pgg-verify'.  If the function in this variable
isn't bound, the message will be used unconditionally."
  :group 'gnus-nocem
  :type '(radio (function-item pgg-verify)
		(function-item mc-verify)
		(function :tag "other")))

(defcustom gnus-nocem-liberal-fetch nil
  "*If t try to fetch all messages which have @@NCM in the subject.
Otherwise don't fetch messages which have references or whose message-id
matches a previously scanned and verified nocem message."
  :group 'gnus-nocem
  :type 'boolean)

(defcustom gnus-nocem-check-article-limit 500
  "*If non-nil, the maximum number of articles to check in any NoCeM group."
  :group 'gnus-nocem
  :version "21.1"
  :type '(choice (const :tag "unlimited" nil)
		 (integer 1000)))

(defcustom gnus-nocem-check-from t
  "Non-nil means check for valid issuers in message bodies.
Otherwise don't bother fetching articles unless their author matches a
valid issuer, which is much faster if you are selective about the issuers."
  :group 'gnus-nocem
  :version "21.1"
  :type 'boolean)

;;; Internal variables

(defvar gnus-nocem-active nil)
(defvar gnus-nocem-alist nil)
(defvar gnus-nocem-touched-alist nil)
(defvar gnus-nocem-hashtb nil)
(defvar gnus-nocem-seen-message-ids nil)

;;; Functions

(defun gnus-nocem-active-file ()
  (concat (file-name-as-directory gnus-nocem-directory) "active"))

(defun gnus-nocem-cache-file ()
  (concat (file-name-as-directory gnus-nocem-directory) "cache"))

;;
;; faster lookups for group names:
;;

(defvar gnus-nocem-real-group-hashtb nil
  "Real-name mappings of subscribed groups.")

(defun gnus-fill-real-hashtb ()
  "Fill up a hash table with the real-name mappings from the user's active file."
  (setq gnus-nocem-real-group-hashtb (gnus-make-hashtable
				      (length gnus-newsrc-alist)))
  (mapcar (lambda (group)
	    (setq group (gnus-group-real-name (car group)))
	    (gnus-sethash group t gnus-nocem-real-group-hashtb))
	  gnus-newsrc-alist))

;;;###autoload
(defun gnus-nocem-scan-groups ()
  "Scan all NoCeM groups for new NoCeM messages."
  (interactive)
  (let ((groups gnus-nocem-groups)
	(gnus-inhibit-demon t)
	group active gactive articles check-headers)
    (gnus-make-directory gnus-nocem-directory)
    ;; Load any previous NoCeM headers.
    (gnus-nocem-load-cache)
    ;; Get the group name mappings:
    (gnus-fill-real-hashtb)
    ;; Read the active file if it hasn't been read yet.
    (and (file-exists-p (gnus-nocem-active-file))
	 (not gnus-nocem-active)
	 (ignore-errors
	   (load (gnus-nocem-active-file) t t t)))
    ;; Go through all groups and see whether new articles have
    ;; arrived.
    (while (setq group (pop groups))
      (if (not (setq gactive (gnus-activate-group group)))
	  ()				; This group doesn't exist.
	(setq active (nth 1 (assoc group gnus-nocem-active)))
	(when (and (not (< (cdr gactive) (car gactive))) ; Empty group.
		   (or (not active)
		       (< (cdr active) (cdr gactive))))
	  ;; Ok, there are new articles in this group, se we fetch the
	  ;; headers.
	  (save-excursion
	    (let ((dependencies (make-vector 10 nil))
		  headers header)
	      (with-temp-buffer
		(setq headers
		      (if (eq 'nov
			      (gnus-retrieve-headers
			       (setq articles
				     (gnus-uncompress-range
				      (cons
				       (if active (1+ (cdr active))
					 (car gactive))
				       (cdr gactive))))
			       group))
			  (gnus-get-newsgroup-headers-xover
			   articles nil dependencies)
			(gnus-get-newsgroup-headers dependencies)))
		(while (setq header (pop headers))
		  ;; We take a closer look on all articles that have
		  ;; "@@NCM" in the subject.  Unless we already read
		  ;; this cross posted message.  Nocem messages
		  ;; are not allowed to have references, so we can
		  ;; ignore scanning followups.
		  (and (string-match "@@NCM" (mail-header-subject header))
		       (and gnus-nocem-check-from
			    (let ((case-fold-search t))
			      (catch 'ok
				(mapcar
				 (lambda (author)
				   (if (consp author)
				       (setq author (car author)))
				   (if (string-match
					author (mail-header-from header))
				       (throw 'ok t)))
				 gnus-nocem-issuers)
				nil)))
		       (or gnus-nocem-liberal-fetch
			   (and (or (string= "" (mail-header-references
						 header))
				    (null (mail-header-references header)))
				(not (member (mail-header-message-id header)
					     gnus-nocem-seen-message-ids))))
		       (push header check-headers)))
		(setq check-headers (last (nreverse check-headers)
					  gnus-nocem-check-article-limit))
		(let ((i 0)
		      (len (length check-headers)))
		  (dolist (h check-headers)
		    (gnus-message
		     7 "Checking article %d in %s for NoCeM (%d of %d)..."
		     (mail-header-number h) group (incf i) len)
		    (gnus-nocem-check-article group h)))))))
	(setq gnus-nocem-active
	      (cons (list group gactive)
		    (delq (assoc group gnus-nocem-active)
			  gnus-nocem-active)))))
    ;; Save the results, if any.
    (gnus-nocem-save-cache)
    (gnus-nocem-save-active)))

(defun gnus-nocem-check-article (group header)
  "Check whether the current article is an NCM article and that we want it."
  ;; Get the article.
  (let ((date (mail-header-date header))
	(gnus-newsgroup-name group)
	issuer b e type)
    (when (or (not date)
	      (time-less-p
	       (time-since (date-to-time date))
	       (days-to-time gnus-nocem-expiry-wait)))
      (gnus-request-article-this-buffer (mail-header-number header) group)
      (goto-char (point-min))
      (when (re-search-forward
	     "-----BEGIN PGP\\( SIGNED\\)? MESSAGE-----"
	     nil t)
	(delete-region (point-min) (match-beginning 0)))
      (when (re-search-forward
	     "-----END PGP \\(MESSAGE\\|SIGNATURE\\)-----\n?"
	     nil t)
	(delete-region (match-end 0) (point-max)))
      (goto-char (point-min))
      ;; The article has to have proper NoCeM headers.
      (when (and (setq b (search-forward "\n@@BEGIN NCM HEADERS\n" nil t))
		 (setq e (search-forward "\n@@BEGIN NCM BODY\n" nil t)))
	;; We get the name of the issuer.
	(narrow-to-region b e)
	(setq issuer (mail-fetch-field "issuer")
	      type (mail-fetch-field "type"))
	(widen)
	(if (not (gnus-nocem-message-wanted-p issuer type))
	    (message "invalid NoCeM issuer: %s" issuer)
	  (and (gnus-nocem-verify-issuer issuer) ; She is who she says she is.
	       (gnus-nocem-enter-article) ; We gobble the message.
	       (push (mail-header-message-id header) ; But don't come back for
		     gnus-nocem-seen-message-ids))))))) ; second helpings.

(defun gnus-nocem-message-wanted-p (issuer type)
  (let ((issuers gnus-nocem-issuers)
	wanted conditions condition)
    (cond
     ;; Do the quick check first.
     ((member issuer issuers)
      t)
     ((setq conditions (cdr (assoc issuer issuers)))
      ;; Check whether we want this type.
      (while (setq condition (pop conditions))
	(cond
	 ((stringp condition)
	  (when (string-match condition type)
	    (setq wanted t)))
	 ((and (consp condition)
	       (eq (car condition) 'not)
	       (stringp (cadr condition)))
	  (when (string-match (cadr condition) type)
	    (setq wanted nil)))
	 (t
	  (error "Invalid NoCeM condition: %S" condition))))
      wanted))))

(defun gnus-nocem-verify-issuer (person)
  "Verify using PGP that the canceler is who she says she is."
  (if (functionp gnus-nocem-verifyer)
      (ignore-errors
	(funcall gnus-nocem-verifyer))
    ;; If we don't have Mailcrypt, then we use the message anyway.
    t))

(defun gnus-nocem-enter-article ()
  "Enter the current article into the NoCeM cache."
  (goto-char (point-min))
  (let ((b (search-forward "\n@@BEGIN NCM BODY\n" nil t))
	(e (search-forward "\n@@END NCM BODY\n" nil t))
	(buf (current-buffer))
	ncm id group)
    (when (and b e)
      (narrow-to-region b (1+ (match-beginning 0)))
      (goto-char (point-min))
      (while (search-forward "\t" nil t)
	(cond
	 ((not (ignore-errors
		 (setq group (let ((obarray gnus-nocem-real-group-hashtb))
			       (read buf)))))
	  ;; An error.
	  )
	 ((not (symbolp group))
	  ;; Ignore invalid entries.
	  )
	 ((not (boundp group))
	  ;; Make sure all entries in the hashtb are bound.
	  (set group nil))
	 (t
	  (when (gnus-gethash (gnus-group-real-name (symbol-name group))
			      gnus-nocem-real-group-hashtb)
	    ;; Valid group.
	    (beginning-of-line)
	    (while (eq (char-after) ?\t)
	      (forward-line -1))
	    (setq id (buffer-substring (point) (1- (search-forward "\t"))))
	    (unless (if gnus-nocem-hashtb
			(gnus-gethash id gnus-nocem-hashtb)
		      (setq gnus-nocem-hashtb (gnus-make-hashtable))
		      nil)
	      ;; only store if not already present
	      (gnus-sethash id t gnus-nocem-hashtb)
	      (push id ncm))
	    (forward-line 1)
	    (while (eq (char-after) ?\t)
	      (forward-line 1))))))
      (when ncm
	(setq gnus-nocem-touched-alist t)
	(push (cons (let ((time (current-time))) (setcdr (cdr time) nil) time)
		    ncm)
	      gnus-nocem-alist))
      t)))

;;;###autoload
(defun gnus-nocem-load-cache ()
  "Load the NoCeM cache."
  (interactive)
  (unless gnus-nocem-alist
    ;; The buffer doesn't exist, so we create it and load the NoCeM
    ;; cache.
    (when (file-exists-p (gnus-nocem-cache-file))
      (load (gnus-nocem-cache-file) t t t)
      (gnus-nocem-alist-to-hashtb))))

(defun gnus-nocem-save-cache ()
  "Save the NoCeM cache."
  (when (and gnus-nocem-alist
	     gnus-nocem-touched-alist)
    (with-temp-file (gnus-nocem-cache-file)
      (gnus-prin1 `(setq gnus-nocem-alist ',gnus-nocem-alist)))
    (setq gnus-nocem-touched-alist nil)))

(defun gnus-nocem-save-active ()
  "Save the NoCeM active file."
  (with-temp-file (gnus-nocem-active-file)
    (gnus-prin1 `(setq gnus-nocem-active ',gnus-nocem-active))))

(defun gnus-nocem-alist-to-hashtb ()
  "Create a hashtable from the Message-IDs we have."
  (let* ((alist gnus-nocem-alist)
	 (pprev (cons nil alist))
	 (prev pprev)
	 (expiry (days-to-time gnus-nocem-expiry-wait))
	 entry)
    (setq gnus-nocem-hashtb (gnus-make-hashtable (* (length alist) 51)))
    (while (setq entry (car alist))
      (if (not (time-less-p (time-since (car entry)) expiry))
	  ;; This entry has expired, so we remove it.
	  (setcdr prev (cdr alist))
	(setq prev alist)
	;; This is ok, so we enter it into the hashtable.
	(setq entry (cdr entry))
	(while entry
	  (gnus-sethash (car entry) t gnus-nocem-hashtb)
	  (setq entry (cdr entry))))
      (setq alist (cdr alist)))))

(gnus-add-shutdown 'gnus-nocem-close 'gnus)

(defun gnus-nocem-close ()
  "Clear internal NoCeM variables."
  (setq gnus-nocem-alist nil
	gnus-nocem-hashtb nil
	gnus-nocem-active nil
	gnus-nocem-touched-alist nil
	gnus-nocem-seen-message-ids nil
	gnus-nocem-real-group-hashtb nil))

(defun gnus-nocem-unwanted-article-p (id)
  "Say whether article ID in the current group is wanted."
  (and gnus-nocem-hashtb
       (gnus-gethash id gnus-nocem-hashtb)))

(provide 'gnus-nocem)

;;; arch-tag: 0e0c74ea-2f8e-4f3e-8fff-09f767c1adef
;;; gnus-nocem.el ends here
