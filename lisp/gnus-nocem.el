;;; gnus-nocem.el --- NoCeM pseudo-cancellation treatment
;; Copyright (C) 1995,96 Free Software Foundation, Inc.

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

;;; Code:

(require 'gnus)
(require 'nnmail)
(eval-when-compile (require 'cl))

(defvar gnus-nocem-groups 
  '("alt.nocem.misc" "news.admin.net-abuse.announce")
  "*List of groups that will be searched for NoCeM messages.")

(defvar gnus-nocem-issuers 
  '("Automoose-1" ; The CancelMoose[tm] on autopilot.
    "clewis@ferret.ocunix.on.ca;" ; Chris Lewis -- Canadian angel & despammer.
    "jem@xpat.com;"  ; John Milburn -- despammer in Korea.
    "red@redpoll.mrfs.oh.us (Richard E. Depew)" ; Spew/bincancel guy.
    )
  "*List of NoCeM issuers to pay attention to.")

(defvar gnus-nocem-directory 
  (concat (file-name-as-directory gnus-article-save-directory) "NoCeM/")
  "*Directory where NoCeM files will be stored.")

(defvar gnus-nocem-expiry-wait 15
  "*Number of days to keep NoCeM headers in the cache.")

(defvar gnus-nocem-verifyer nil
  "*Function called to verify that the NoCeM message is valid.
One likely value is `mc-verify'.  If the function in this variable
isn't bound, the message will be used unconditionally.")

;;; Internal variables

(defvar gnus-nocem-active nil)
(defvar gnus-nocem-alist nil)
(defvar gnus-nocem-touched-alist nil)
(defvar gnus-nocem-hashtb nil)

;;; Functions

(defun gnus-nocem-active-file ()
  (concat (file-name-as-directory gnus-nocem-directory) "active"))

(defun gnus-nocem-cache-file ()
  (concat (file-name-as-directory gnus-nocem-directory) "cache"))

(defun gnus-nocem-scan-groups ()
  "Scan all NoCeM groups for new NoCeM messages."
  (interactive)
  (let ((groups gnus-nocem-groups)
	group active gactive articles)
    (or (file-exists-p gnus-nocem-directory)
	(make-directory gnus-nocem-directory t))
    ;; Load any previous NoCeM headers.
    (gnus-nocem-load-cache)
    ;; Read the active file if it hasn't been read yet.
    (and (file-exists-p (gnus-nocem-active-file))
	 (not gnus-nocem-active)
	 (condition-case ()
	     (load (gnus-nocem-active-file) t t t)
	   (error nil)))
    ;; Go through all groups and see whether new articles have
    ;; arrived.  
    (while (setq group (pop groups))
      (if (not (setq gactive (gnus-activate-group group)))
	  () ; This group doesn't exist.
	(setq active (nth 1 (assoc group gnus-nocem-active)))
	(when (and (not (< (cdr gactive) (car gactive))) ; Empty group.
		   (or (not active)
		       (< (cdr active) (cdr gactive))))
	  ;; Ok, there are new articles in this group, se we fetch the
	  ;; headers.
	  (save-excursion
	    (let ((dependencies (make-vector 10 nil))
		  (buffer (nnheader-set-temp-buffer " *Gnus NoCeM*"))
		  headers)
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
	      (while headers
		;; We take a closer look on all articles that have
		;; "@@NCM" in the subject.  
		(when (string-match "@@NCM"
				    (mail-header-subject (car headers)))
		  (gnus-nocem-check-article group (car headers)))
		(setq headers (cdr headers)))
	      (kill-buffer (current-buffer)))))
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
  (gnus-message 7 "Checking article %d in %s for NoCeM..."
		(mail-header-number header) group)
  (let ((date (mail-header-date header))
	issuer b e)
    (when (or (not date)
	      (nnmail-time-less 
	       (nnmail-time-since (nnmail-date-to-time date))
	       (nnmail-days-to-time gnus-nocem-expiry-wait)))
      (gnus-request-article-this-buffer (mail-header-number header) group)
      (goto-char (point-min))
      ;; The article has to have proper NoCeM headers.
      (when (and (setq b (search-forward "\n@@BEGIN NCM HEADERS\n" nil t))
		 (setq e (search-forward "\n@@BEGIN NCM BODY\n" nil t)))
	;; We get the name of the issuer.
	(narrow-to-region b e)
	(setq issuer (mail-fetch-field "issuer"))
	(and (member issuer gnus-nocem-issuers) ; We like her...
	     (gnus-nocem-verify-issuer issuer) ; She is who she says she is..
	     (gnus-nocem-enter-article)))))) ; We gobble the message.
  
(defun gnus-nocem-verify-issuer (person)
  "Verify using PGP that the canceler is who she says she is."
  (widen)
  (if (fboundp gnus-nocem-verifyer)
      (funcall gnus-nocem-verifyer)
    ;; If we don't have MailCrypt, then we use the message anyway.
    t))

(defun gnus-nocem-enter-article ()
  "Enter the current article into the NoCeM cache."
  (goto-char (point-min))
  (let ((b (search-forward "\n@@BEGIN NCM BODY\n" nil t))
	(e (search-forward "\n@@END NCM BODY\n" nil t))
	(buf (current-buffer))
	ncm id)
    (when (and b e)
      (narrow-to-region b (1+ (match-beginning 0)))
      (goto-char (point-min))
      (while (search-forward "\t" nil t)
	(when (condition-case nil
		  (boundp (let ((obarray gnus-active-hashtb)) (read buf)))
		(error nil))
	  (beginning-of-line)
	  (while (= (following-char) ?\t)
	    (forward-line -1))
	  (setq id (buffer-substring (point) (1- (search-forward "\t"))))
	  (push id ncm)
	  (gnus-sethash id t gnus-nocem-hashtb)
	  (forward-line 1)
	  (while (= (following-char) ?\t)
	    (forward-line 1))))
      (when ncm
	(setq gnus-nocem-touched-alist t)
	(push (cons (let ((time (current-time))) (setcdr (cdr time) nil) time)
		    ncm) 
	      gnus-nocem-alist)))))

(defun gnus-nocem-load-cache ()
  "Load the NoCeM cache."
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
    (nnheader-temp-write (gnus-nocem-cache-file)
      (prin1 `(setq gnus-nocem-alist ',gnus-nocem-alist) (current-buffer)))
    (setq gnus-nocem-touched-alist nil)))

(defun gnus-nocem-save-active ()
  "Save the NoCeM active file."
  (nnheader-temp-write (gnus-nocem-active-file)
    (prin1 `(setq gnus-nocem-active ',gnus-nocem-active) (current-buffer))))

(defun gnus-nocem-alist-to-hashtb ()
  "Create a hashtable from the Message-IDs we have."
  (let* ((alist gnus-nocem-alist)
	 (pprev (cons nil alist))
	 (prev pprev)
	 (expiry (nnmail-days-to-time gnus-nocem-expiry-wait))
	 entry)
    (setq gnus-nocem-hashtb (gnus-make-hashtable (* (length alist) 51)))
    (while (setq entry (car alist))
      (if (not (nnmail-time-less (nnmail-time-since (car entry)) expiry))
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
	gnus-nocem-touched-alist nil))

(defun gnus-nocem-unwanted-article-p (id)
  "Say whether article ID in the current group is wanted."
  (gnus-gethash id gnus-nocem-hashtb))

(provide 'gnus-nocem)

;;; gnus-nocem.el ends here
