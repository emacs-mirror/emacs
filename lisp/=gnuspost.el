;;; gnuspost.el --- post news commands for GNUS newsreader

;; Author: Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;; Last-Modified: 10 Jun 1992

;; $Header: gnuspost.el,v 1.2 90/03/23 13:25:16 umerin Locked $

;; Copyright (C) 1989, 1990 Free Software Foundation, Inc.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Code:

(require 'gnus)

(defvar gnus-organization-file "/usr/lib/news/organization"
  "*Local news organization file.")

(defvar gnus-post-news-buffer "*post-news*")
(defvar gnus-winconf-post-news nil)

(autoload 'news-reply-mode "rnewspost")

;;; Post news commands of GNUS Group Mode and Subject Mode

(defun gnus-Group-post-news ()
  "Post an article."
  (interactive)
  ;; Save window configuration.
  (setq gnus-winconf-post-news (current-window-configuration))
  (unwind-protect
      (gnus-post-news)
    (or (and (eq (current-buffer) (get-buffer gnus-post-news-buffer))
	     (not (zerop (buffer-size))))
	;; Restore last window configuration.
	(set-window-configuration gnus-winconf-post-news)))
  ;; We don't want to return to Subject buffer nor Article buffer later.
  (if (get-buffer gnus-Subject-buffer)
      (bury-buffer gnus-Subject-buffer))
  (if (get-buffer gnus-Article-buffer)
      (bury-buffer gnus-Article-buffer)))

(defun gnus-Subject-post-news ()
  "Post an article."
  (interactive)
  (gnus-Subject-select-article t nil)
  ;; Save window configuration.
  (setq gnus-winconf-post-news (current-window-configuration))
  (unwind-protect
      (progn
	(switch-to-buffer gnus-Article-buffer)
	(widen)
	(delete-other-windows)
	(gnus-post-news))
    (or (and (eq (current-buffer) (get-buffer gnus-post-news-buffer))
	     (not (zerop (buffer-size))))
	;; Restore last window configuration.
	(set-window-configuration gnus-winconf-post-news)))
  ;; We don't want to return to Article buffer later.
  (bury-buffer gnus-Article-buffer))

(defun gnus-Subject-post-reply (yank)
  "Post a reply article.
If prefix argument YANK is non-nil, original article is yanked automatically."
  (interactive "P")
  (gnus-Subject-select-article t nil)
  ;; Check Followup-To: poster.
  (set-buffer gnus-Article-buffer)
  (if (and gnus-use-followup-to
	   (string-equal "poster" (gnus-fetch-field "followup-to"))
	   (or (not (eq gnus-use-followup-to t))
	       (not (y-or-n-p "Do you want to ignore `Followup-To: poster'? "))))
      ;; Mail to the poster.  GNUS is now RFC1036 compliant.
      (gnus-Subject-mail-reply yank)
    ;; Save window configuration.
    (setq gnus-winconf-post-news (current-window-configuration))
    (unwind-protect
	(progn
	  (switch-to-buffer gnus-Article-buffer)
	  (widen)
	  (delete-other-windows)
	  (gnus-news-reply yank))
      (or (and (eq (current-buffer) (get-buffer gnus-post-news-buffer))
	       (not (zerop (buffer-size))))
	  ;; Restore last window configuration.
	  (set-window-configuration gnus-winconf-post-news)))
    ;; We don't want to return to Article buffer later.
    (bury-buffer gnus-Article-buffer)))

(defun gnus-Subject-post-reply-with-original ()
  "Post a reply article with original article."
  (interactive)
  (gnus-Subject-post-reply t))

(defun gnus-Subject-cancel-article ()
  "Cancel an article you posted."
  (interactive)
  (gnus-Subject-select-article t nil)
  (gnus-eval-in-buffer-window gnus-Article-buffer
    (gnus-cancel-news)))


;;; Post a News using NNTP

;;;###autoload
(fset 'sendnews 'gnus-post-news)
;;;###autoload
(fset 'postnews 'gnus-post-news)
;;;###autoload
(defun gnus-post-news ()
  "Begin editing a new USENET news article to be posted.
Type \\[describe-mode] once editing the article to get a list of commands."
  (interactive)
  (if (or (not gnus-novice-user)
	  (y-or-n-p "Are you sure you want to post to all of USENET? "))
      (let ((artbuf (current-buffer))
	    (newsgroups			;Default newsgroup.
	     (if (eq major-mode 'gnus-Article-mode) gnus-newsgroup-name))
	    (subject nil)
	    (distribution nil))
	(save-restriction
	  (and (not (zerop (buffer-size)))
	       ;;(equal major-mode 'news-mode)
	       (equal major-mode 'gnus-Article-mode)
	       (progn
		 ;;(news-show-all-headers)
		 (gnus-Article-show-all-headers)
		 (narrow-to-region (point-min)
				   (progn (goto-char (point-min))
					  (search-forward "\n\n")
					  (point)))))
	  (setq news-reply-yank-from (mail-fetch-field "from"))
	  (setq news-reply-yank-message-id (mail-fetch-field "message-id")))
	(pop-to-buffer gnus-post-news-buffer)
	(news-reply-mode)
	(gnus-overload-functions)
	(if (and (buffer-modified-p)
		 (> (buffer-size) 0)
		 (not (y-or-n-p "Unsent article being composed; erase it? ")))
	    ;; Continue composition.
	    ;; Make news-reply-yank-original work on the current article.
	    (setq mail-reply-buffer artbuf)
	  (erase-buffer)
	  (if gnus-interactive-post
	      ;; Newsgroups, subject and distribution are asked for.
	      ;; Suggested by yuki@flab.fujitsu.junet.
	      (progn
		;; Subscribed newsgroup names are required for
		;; completing read of newsgroup.
		(or gnus-newsrc-assoc
		    (gnus-read-newsrc-file))
		;; Which do you like? (UMERIN)
		;; (setq newsgroups (read-string "Newsgroups: " "general"))
		(or newsgroups		;Use the default newsgroup.
		    (setq newsgroups
			  (completing-read "Newsgroup: " gnus-newsrc-assoc
					   nil 'require-match
					   newsgroups ;Default newsgroup.
					   )))
		(setq subject (read-string "Subject: "))
		(setq distribution
		      (substring newsgroups 0 (string-match "\\." newsgroups)))
		(if (string-equal distribution newsgroups)
		    ;; Newsgroup may be general or control. In this
		    ;; case, use default distribution.
		    (setq distribution gnus-default-distribution))
		(setq distribution
		      (read-string "Distribution: " distribution))
		;; An empty string is ok to ignore gnus-default-distribution.
		;;(if (string-equal distribution "")
		;;    (setq distribution nil))
		))
	  (news-setup () subject () newsgroups artbuf)
	  ;; Make sure the article is posted by GNUS.
	  ;;(mail-position-on-field "Posting-Software")
	  ;;(insert "GNUS: NNTP-based News Reader for GNU Emacs")
	  ;; Insert Distribution: field.
	  ;; Suggested by ichikawa@flab.fujitsu.junet.
	  (mail-position-on-field "Distribution")
	  (insert (or distribution gnus-default-distribution ""))
	  ;; Handle author copy using FCC field.
	  (if gnus-author-copy
	      (progn
		(mail-position-on-field "FCC")
		(insert gnus-author-copy)))
	  (if gnus-interactive-post
	      ;; All fields are filled in.
	      (goto-char (point-max))
	    ;; Move point to Newsgroup: field.
	    (goto-char (point-min))
	    (end-of-line))
	  ))
    (message "")))

(defun gnus-news-reply (&optional yank)
  "Compose and post a reply (aka a followup) to the current article on USENET.
While composing the followup, use \\[news-reply-yank-original] to yank the
original message into it."
  (interactive)
  (if (or (not gnus-novice-user)
	  (y-or-n-p "Are you sure you want to followup to all of USENET? "))
      (let (from cc subject date to followup-to newsgroups message-of
		 references distribution message-id
		 (artbuf (current-buffer)))
	(save-restriction
	  (and (not (zerop (buffer-size)))
	       ;;(equal major-mode 'news-mode)
	       (equal major-mode 'gnus-Article-mode)
	       (progn
		 ;; (news-show-all-headers)
		 (gnus-Article-show-all-headers)
		 (narrow-to-region (point-min)
				   (progn (goto-char (point-min))
					  (search-forward "\n\n")
					  (point)))))
	  (setq from (mail-fetch-field "from"))
	  (setq	news-reply-yank-from from)
	  (setq	subject (mail-fetch-field "subject"))
	  (setq	date (mail-fetch-field "date"))
	  (setq followup-to (mail-fetch-field "followup-to"))
	  ;; Ignore Followup-To: poster.
	  (if (or (null gnus-use-followup-to) ;Ignore followup-to: field.
		  (string-equal "" followup-to)	;Bogus header.
		  (string-equal "poster" followup-to))
	      (setq followup-to nil))
	  (setq	newsgroups (or followup-to (mail-fetch-field "newsgroups")))
	  (setq	references (mail-fetch-field "references"))
	  (setq	distribution (mail-fetch-field "distribution"))
	  (setq	message-id (mail-fetch-field "message-id"))
	  (setq	news-reply-yank-message-id message-id))
	(pop-to-buffer gnus-post-news-buffer)
	(news-reply-mode)
	(gnus-overload-functions)
	(if (and (buffer-modified-p)
		 (> (buffer-size) 0)
		 (not (y-or-n-p "Unsent article being composed; erase it? ")))
	    ;; Continue composition.
	    ;; Make news-reply-yank-original work on current article.
	    (setq mail-reply-buffer artbuf)
	  (erase-buffer)
	  (and subject
	       (setq subject
		     (concat "Re: " (gnus-simplify-subject subject 're-only))))
	  (and from
	       (progn
		 (let ((stop-pos
			(string-match "  *at \\|  *@ \\| *(\\| *<" from)))
		   (setq message-of
			 (concat
			  (if stop-pos (substring from 0 stop-pos) from)
			  "'s message of "
			  date)))))
	  (news-setup nil subject message-of newsgroups artbuf)
	  (if followup-to
	      (progn (news-reply-followup-to)
		     (insert followup-to)))
	  ;; Fold long references line to follow RFC1036.
	  (mail-position-on-field "References")
	  (let ((begin (point))
		(fill-column 79)
		(fill-prefix "\t"))
	    (if references
		(insert references))
	    (if (and references message-id)
		(insert " "))
	    (if message-id
		(insert message-id))
	    ;; The region must end with a newline to fill the region
	    ;; without inserting extra newline.
	    (fill-region-as-paragraph begin (1+ (point))))
	  ;; Make sure the article is posted by GNUS.
	  ;;(mail-position-on-field "Posting-Software")
	  ;;(insert "GNUS: NNTP-based News Reader for GNU Emacs")
	  ;; Distribution must be the same as original article.
	  (mail-position-on-field "Distribution")
	  (insert (or distribution ""))
	  ;; Handle author copy using FCC field.
	  (if gnus-author-copy
	      (progn
		(mail-position-on-field "FCC")
		(insert gnus-author-copy)))
	  (goto-char (point-max)))
	;; Yank original article automatically.
	(if yank
	    (let ((last (point)))
	      (goto-char (point-max))
	      (news-reply-yank-original nil)
	      (goto-char last)))
	)
    (message "")))

(defun gnus-inews-news ()
  "Send a news message."
  (interactive)
  (let* ((case-fold-search nil)
	 (server-running (gnus-server-opened)))
    (save-excursion
      ;; It is possible to post a news without reading news using
      ;; `gnus' before.
      ;; Suggested by yuki@flab.fujitsu.junet.
      (gnus-start-news-server)		;Use default server.
      ;; NNTP server must be opened before current buffer is modified.
      (widen)
      (goto-char (point-min))
      (run-hooks 'news-inews-hook)
      (goto-char (point-min))
      (search-forward (concat "\n" mail-header-separator "\n"))
      (replace-match "\n\n")
      (goto-char (point-max))
      ;; require a newline at the end for inews to append .signature to
      (or (= (preceding-char) ?\n)
	  (insert ?\n))
      (message "Posting to USENET...")
      ;; Post to NNTP server. 
      (if (gnus-inews-article)
	  (message "Posting to USENET... done")
	;; We cannot signal an error.
	(ding) (message "Article rejected: %s" (gnus-status-message)))
      (goto-char (point-min))		;restore internal header separator
      (search-forward "\n\n")
      (replace-match (concat "\n" mail-header-separator "\n"))
      (set-buffer-modified-p nil))
    ;; If NNTP server is opened by gnus-inews-news, close it by myself.
    (or server-running
	(gnus-close-server))
    (and (fboundp 'bury-buffer) (bury-buffer))
    ;; Restore last window configuration.
    (and gnus-winconf-post-news
	 (set-window-configuration gnus-winconf-post-news))
    (setq gnus-winconf-post-news nil)
    ))

(defun gnus-cancel-news ()
  "Cancel an article you posted."
  (interactive)
  (if (yes-or-no-p "Do you really want to cancel this article? ")
      (let ((from nil)
	    (newsgroups nil)
	    (message-id nil)
	    (distribution nil))
	(save-excursion
	  ;; Get header info. from original article.
	  (save-restriction
	    (gnus-Article-show-all-headers)
	    (goto-char (point-min))
	    (search-forward "\n\n")
	    (narrow-to-region (point-min) (point))
	    (setq from (mail-fetch-field "from"))
	    (setq newsgroups (mail-fetch-field "newsgroups"))
	    (setq message-id (mail-fetch-field "message-id"))
	    (setq distribution (mail-fetch-field "distribution")))
	  ;; Verify if the article is absolutely user's by comparing
	  ;; user id with value of its From: field.
	  (if (not
	       (string-equal
		(downcase (mail-strip-quoted-names from))
		(downcase (mail-strip-quoted-names (gnus-inews-user-name)))))
	      (progn
		(ding) (message "This article is not yours"))
	    ;; Make control article.
	    (set-buffer (get-buffer-create " *GNUS-posting*"))
	    (buffer-flush-undo (current-buffer))
	    (erase-buffer)
	    (insert "Newsgroups: " newsgroups "\n"
		    "Subject: cancel " message-id "\n"
		    "Control: cancel " message-id "\n"
		    ;; We should not use the value of
		    ;;  `gnus-default-distribution' as default value,
		    ;;  because distribution must be as same as original
		    ;;  article.
		    "Distribution: " (or distribution "") "\n"
		    )
	    ;; Prepare article headers.
	    (gnus-inews-insert-headers)
	    (goto-char (point-max))
	    ;; Insert empty line.
	    (insert "\n")
	    ;; Send the control article to NNTP server.
	    (message "Canceling your article...")
	    (if (gnus-request-post)
		(message "Canceling your article... done")
	      (ding) (message "Failed to cancel your article"))
	    (kill-buffer (current-buffer))
	    )))
    ))


;;; Lowlevel inews interface

(defun gnus-inews-article ()
  "NNTP inews interface."
  (let ((signature
	 (if gnus-signature-file
	     (expand-file-name gnus-signature-file nil)))
	(distribution nil)
	(artbuf (current-buffer))
	(tmpbuf (get-buffer-create " *GNUS-posting*")))
    (save-excursion
      (set-buffer tmpbuf)
      (buffer-flush-undo (current-buffer))
      (erase-buffer)
      (insert-buffer-substring artbuf)
      ;; Get distribution.
      (save-restriction
	(goto-char (point-min))
	(search-forward "\n\n")
	(narrow-to-region (point-min) (point))
	(setq distribution (mail-fetch-field "distribution")))
      (widen)
      (if signature
	  (progn
	    ;; Change signature file by distribution.
	    ;; Suggested by hyoko@flab.fujitsu.junet.
	    (if (file-exists-p (concat signature "-" distribution))
		(setq signature (concat signature "-" distribution)))
	    ;; Insert signature.
	    (if (file-exists-p signature)
		(progn
		  (goto-char (point-max))
		  (insert "--\n")
		  (insert-file-contents signature)))
	    ))
      ;; Prepare article headers.
      (save-restriction
	(goto-char (point-min))
	(search-forward "\n\n")
	(narrow-to-region (point-min) (point))
	(gnus-inews-insert-headers)
	;; Save author copy of posted article. The article must be
	;;  copied before being posted because `gnus-request-post'
	;;  modifies the buffer.
	(let ((case-fold-search t))
	  ;; Find and handle any FCC fields.
	  (goto-char (point-min))
	  (if (re-search-forward "^FCC:" nil t)
	      (gnus-inews-do-fcc))))
      (widen)
      ;; Run final inews hooks.
      (run-hooks 'gnus-Inews-article-hook)
      ;; Post an article to NNTP server.
      ;; Return NIL if post failed.
      (prog1
	  (gnus-request-post)
	(kill-buffer (current-buffer)))
      )))

(defun gnus-inews-do-fcc ()
  "Process FCC: fields."
  (let ((fcc-list nil)
	(fcc-file nil)
	(case-fold-search t))		;Should ignore case.
    (save-excursion
      (save-restriction
	(goto-char (point-min))
	(while (re-search-forward "^FCC:[ \t]*" nil t)
	  (setq fcc-list (cons (buffer-substring (point)
						 (progn
						   (end-of-line)
						   (skip-chars-backward " \t")
						   (point)))
			       fcc-list))
	  (delete-region (match-beginning 0)
			 (progn (forward-line 1) (point))))
	;; Process FCC operations.
	(widen)
	(while fcc-list
	  (setq fcc-file (car fcc-list))
	  (setq fcc-list (cdr fcc-list))
	  (cond ((string-match "^[ \t]*|[ \t]*\\(.*\\)[ \t]*$" fcc-file)
		 (let ((program (substring fcc-file
					   (match-beginning 1) (match-end 1))))
		   ;; Suggested by yuki@flab.fujitsu.junet.
		   ;; Send article to named program.
		   (call-process-region (point-min) (point-max) shell-file-name
					nil nil nil "-c" program)
		   ))
		(t
		 ;; Suggested by hyoko@flab.fujitsu.junet.
		 ;; Save article in Unix mail format by default.
		 (funcall (or gnus-author-copy-saver 'rmail-output) fcc-file)
		 ))
	  )
	))
    ))

(defun gnus-inews-insert-headers ()
  "Prepare article headers.
Path:, From:, Subject: and Distribution: are generated.
Message-ID:, Date: and Organization: are optional."
  (save-excursion
    (let ((date (gnus-inews-date))
	  (message-id (gnus-inews-message-id))
	  (organization (gnus-inews-organization)))
      ;; Insert from the top of headers.
      (goto-char (point-min))
      (insert "Path: " (gnus-inews-path) "\n")
      (insert "From: " (gnus-inews-user-name) "\n")
      ;; If there is no subject, make Subject: field.
      (or (mail-fetch-field "subject")
	  (insert "Subject: \n"))
      ;; Insert random headers.
      (if message-id
	  (insert "Message-ID: " message-id "\n"))
      (if date
	  (insert "Date: " date "\n"))
      (if organization
	  (let ((begin (point))
		(fill-column 79)
		(fill-prefix "\t"))
	    (insert "Organization: " organization "\n")
	    (fill-region-as-paragraph begin (point))))
      (or (mail-fetch-field "distribution")
	  (insert "Distribution: \n"))
      )))

(defun gnus-inews-path ()
  "Return uucp path."
  (let ((login-name (gnus-inews-login-name)))
    (cond ((null gnus-use-generic-path)
	   (concat gnus-nntp-server "!" login-name))
	  ((stringp gnus-use-generic-path)
	   ;; Support GENERICPATH.  Suggested by vixie@decwrl.dec.com.
	   (concat gnus-use-generic-path "!" login-name))
	  (t login-name))
    ))

(defun gnus-inews-user-name ()
  "Return user's network address as `NAME@DOMAIN (FULL NAME)'."
  (let ((login-name (gnus-inews-login-name))
	(full-name (gnus-inews-full-name)))
    (concat login-name "@" (gnus-inews-domain-name gnus-use-generic-from)
	    ;; User's full name.
	    (cond ((string-equal full-name "") "")
		  ((string-equal full-name "&")	;Unix hack.
		   (concat " (" login-name ")"))
		  (t
		   (concat " (" full-name ")")))
	    )))

(defun gnus-inews-login-name ()
  "Return user login name.
Got from the variable `gnus-user-login-name', the environment variables
USER and LOGNAME, and the function `user-login-name'."
  (or gnus-user-login-name
      (getenv "USER") (getenv "LOGNAME") (user-login-name)))

(defun gnus-inews-full-name ()
  "Return user full name.
Got from the variable `gnus-user-full-name', the environment variable
NAME, and the function `user-full-name'."
  (or gnus-user-full-name
      (getenv "NAME") (user-full-name)))

(defun gnus-inews-domain-name (&optional genericfrom)
  "Return user's domain name.
If optional argument GENERICFROM is a string, use it as the domain
name; if it is non-nil, strip of local host name from the domain name.
If the function `system-name' returns full internet name and the
domain is undefined, the domain name is got from it."
  (let ((domain (or (if (stringp genericfrom) genericfrom)
		    (getenv "DOMAINNAME")
		    gnus-your-domain
		    ;; Function `system-name' may return full internet name.
		    ;; Suggested by Mike DeCorte <mrd@sun.soe.clarkson.edu>.
		    (if (string-match "\\." (system-name))
			(substring (system-name) (match-end 0)))
		    (read-string "Domain name (no host): ")))
	(host (or (if (string-match "\\." (system-name))
		      (substring (system-name) 0 (match-beginning 0)))
		  (system-name))))
    (if (string-equal "." (substring domain 0 1))
	(setq domain (substring domain 1)))
    (if (null gnus-your-domain)
	(setq gnus-your-domain domain))
    ;; Support GENERICFROM as same as standard Bnews system.
    ;; Suggested by ohm@kaba.junet and vixie@decwrl.dec.com.
    (cond ((null genericfrom)
	   (concat host "." domain))
	  ;;((stringp genericfrom) genericfrom)
	  (t domain))
    ))

(defun gnus-inews-message-id ()
  "Generate unique Message-ID for user."
  ;; Message-ID should not contain a slash and should be terminated by
  ;; a number.  I don't know the reason why it is so.
  (concat "<" (gnus-inews-unique-id) "@" (gnus-inews-domain-name) ">"))

(defun gnus-inews-unique-id ()
  "Generate unique ID from user name and current time."
  (let ((date (current-time-string))
	(name (gnus-inews-login-name)))
    (if (string-match "^[^ ]+ \\([^ ]+\\)[ ]+\\([0-9]+\\) \\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\) [0-9][0-9]\\([0-9][0-9]\\)"
		      date)
	(concat (upcase name) "."
		(substring date (match-beginning 6) (match-end 6)) ;Year
		(substring date (match-beginning 1) (match-end 1)) ;Month
		(substring date (match-beginning 2) (match-end 2)) ;Day
		(substring date (match-beginning 3) (match-end 3)) ;Hour
		(substring date (match-beginning 4) (match-end 4)) ;Minute
		(substring date (match-beginning 5) (match-end 5)) ;Second
		)
      (error "Cannot understand current-time-string: %s." date))
    ))

(defun gnus-inews-date ()
  "Bnews date format string of today.  Time zone is ignored."
  ;; Insert buggy date (time zone is ignored), but I don't worry about
  ;; it since inews will rewrite it.
  (let ((date (current-time-string)))
    (if (string-match "^[^ ]+ \\([^ ]+\\)[ ]+\\([0-9]+\\) \\([0-9:]+\\) [0-9][0-9]\\([0-9][0-9]\\)"
		      date)
	(concat (substring date (match-beginning 2) (match-end 2)) ;Day
		" "
		(substring date (match-beginning 1) (match-end 1)) ;Month
		" "
		(substring date (match-beginning 4) (match-end 4)) ;Year
		" "
		(substring date (match-beginning 3) (match-end 3))) ;Time
      (error "Cannot understand current-time-string: %s." date))
    ))

(defun gnus-inews-organization ()
  "Return user's organization.
The ORGANIZATION environment variable is used if defined.
If not, the variable `gnus-your-organization' is used instead.
If the value begins with a slash, it is taken as the name of a file
containing the organization."
  ;; The organization must be got in this order since the ORGANIZATION
  ;; environment variable is intended for user specific while
  ;; gnus-your-organization is for machine or organization specific.
  (let ((organization (or (getenv "ORGANIZATION")
			  gnus-your-organization
			  (expand-file-name "~/.organization" nil))))
    (and (stringp organization)
	 (string-equal (substring organization 0 1) "/")
	 ;; Get it from the user and system file.
	 ;; Suggested by roland@wheaties.ai.mit.edu (Roland McGrath).
	 (let ((dist (mail-fetch-field "distribution")))
	   (setq organization
		 (cond ((file-exists-p (concat organization "-" dist))
			(concat organization "-" dist))
		       ((file-exists-p organization) organization)
		       ((file-exists-p gnus-organization-file)
			gnus-organization-file)
		       (t organization)))
	   ))
    (cond ((not (stringp organization)) nil)
	  ((and (string-equal (substring organization 0 1) "/")
		(file-exists-p organization))
	   ;; If the first character is `/', assume it is the name of
	   ;; a file containing the organization.
	   (save-excursion
	     (let ((tmpbuf (get-buffer-create " *GNUS organization*")))
	       (set-buffer tmpbuf)
	       (erase-buffer)
	       (insert-file-contents organization)
	       (prog1 (buffer-string)
		 (kill-buffer tmpbuf))
	       )))
	  (t organization))
    ))

(provide 'gnuspost)

;;; gnuspost.el ends here
