;;; rnewspost.el --- USENET news poster/mailer for GNU Emacs

;; Copyright (C) 1985, 1986, 1987, 1995 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: mail, news

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

;;; Change Log:

;; moved posting and mail code from rnews.el
;;	tower@gnu.org Wed Oct 29 1986
;; brought posting code almost up to the revision of RFC 850 for News 2.11
;; - couldn't see handling the special meaning of the Keyword: poster
;; - not worth the code space to support the old A news Title: (which
;;   Subject: replaced) and Article-I.D.: (which Message-ID: replaced)
;;	tower@gnu.org Nov 86
;; changed C-c C-r key-binding due to rename of news-caesar-buffer-body
;;	tower@gnu.org 21 Nov 86
;; added (require 'rnews)	tower@gnu.org 22 Apr 87
;; restricted call of news-show-all-headers in news-post-news & news-reply
;;	tower@gnu.org 28 Apr 87
;; commented out Posting-Front-End to save USENET bytes tower@gnu.org Jul 31 87
;; commented out -n and -t args in news-inews     tower@gnu.org 15 Oct 87

;Now in paths.el.
;(defvar news-inews-program "inews"
;  "Function to post news.")

;; Replying and posting news items are done by these functions.
;; imported from rmail and modified to work with rnews ...
;; Mon Mar 25,1985 at 03:07:04 ads@mit-hermes.
;; this is done so that rnews can operate independently from rmail.el and
;; sendmail and doesn't have to autoload these functions.
;;
;;; >> Nuked by Mly to autoload those functions again, as the duplication of
;;; >>  code was making maintenance too difficult.

;;; Commentary:

;;; Code:

(require 'sendmail)
(require 'rnews)

(defvar news-reply-mode-map () "Mode map used by news-reply.")

(or news-reply-mode-map
    (progn
      (setq news-reply-mode-map (make-keymap))
      (define-key news-reply-mode-map "\C-c\C-f\C-d" 'news-reply-distribution)
      (define-key news-reply-mode-map "\C-c\C-f\C-k" 'news-reply-keywords)
      (define-key news-reply-mode-map "\C-c\C-f\C-n" 'news-reply-newsgroups)
      (define-key news-reply-mode-map "\C-c\C-f\C-f" 'news-reply-followup-to)
      (define-key news-reply-mode-map "\C-c\C-f\C-s" 'mail-subject)
      (define-key news-reply-mode-map "\C-c\C-f\C-a" 'news-reply-summary)
      (define-key news-reply-mode-map "\C-c\C-t" 'mail-text)
      (define-key news-reply-mode-map "\C-c\C-r" 'news-caesar-buffer-body)
      (define-key news-reply-mode-map "\C-c\C-w" 'news-reply-signature)
      (define-key news-reply-mode-map "\C-c\C-y" 'news-reply-yank-original)
      (define-key news-reply-mode-map "\C-c\C-q" 'mail-fill-yanked-message)
      (define-key news-reply-mode-map "\C-c\C-c" 'news-inews)
      (define-key news-reply-mode-map "\C-c\C-s" 'news-inews)
      (define-key news-reply-mode-map [menu-bar] (make-sparse-keymap))
      (define-key news-reply-mode-map [menu-bar fields]
	(cons "Fields" (make-sparse-keymap "Fields")))
      (define-key news-reply-mode-map [menu-bar fields news-reply-distribution]
	'("Distribution" . news-reply-distribution))
      (define-key news-reply-mode-map [menu-bar fields news-reply-keywords]
	'("Keywords" . news-reply-keywords))
      (define-key news-reply-mode-map [menu-bar fields news-reply-newsgroups]
	'("Newsgroups" . news-reply-newsgroups))
      (define-key news-reply-mode-map [menu-bar fields news-reply-followup-to]
	'("Followup-to" . news-reply-followup-to))
      (define-key news-reply-mode-map [menu-bar fields mail-subject]
	'("Subject" . mail-subject))
      (define-key news-reply-mode-map [menu-bar fields news-reply-summary]
	'("Summary" . news-reply-summary))
      (define-key news-reply-mode-map [menu-bar fields mail-text]
	'("Text" . mail-text))
      (define-key news-reply-mode-map [menu-bar news]
	(cons "News" (make-sparse-keymap "News")))
      (define-key news-reply-mode-map [menu-bar news news-caesar-buffer-body]
	'("Rot13" . news-caesar-buffer-body))
      (define-key news-reply-mode-map [menu-bar news news-reply-yank-original]
	'("Yank Original" . news-reply-yank-original))
      (define-key news-reply-mode-map [menu-bar news mail-fill-yanked-message]
	'("Fill Yanked Messages" . mail-fill-yanked-message))
      (define-key news-reply-mode-map [menu-bar news news-inews]
	'("Send" . news-inews))))

(defun news-reply-mode ()
  "Major mode for editing news to be posted on USENET.
First-time posters are asked to please read the articles in newsgroup:
                                                     news.announce.newusers .
Like Text Mode but with these additional commands:

C-c C-s  news-inews (post the message)    C-c C-c  news-inews
C-c C-f	 move to a header field (and create it if there isn't):
	 C-c C-f C-n  move to Newsgroups:	C-c C-f C-s  move to Subj:
	 C-c C-f C-f  move to Followup-To:      C-c C-f C-k  move to Keywords:
	 C-c C-f C-d  move to Distribution:	C-c C-f C-a  move to Summary:
C-c C-y  news-reply-yank-original (insert current message, in NEWS).
C-c C-q  mail-fill-yanked-message (fill what was yanked).
C-c C-r  caesar rotate all letters by 13 places in the article's body (rot13)."
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'mail-reply-buffer)
  (setq mail-reply-buffer nil)
  (set-syntax-table text-mode-syntax-table)
  (use-local-map news-reply-mode-map)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq major-mode 'news-reply-mode)
  (setq mode-name "News Reply")
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-start)
  (run-mode-hooks 'text-mode-hook 'news-reply-mode-hook))

(defvar news-reply-yank-from ""
  "Save `From:' field for `news-reply-yank-original'.")

(defvar news-reply-yank-message-id ""
  "Save `Message-Id:' field for `news-reply-yank-original'.")

(defun news-reply-yank-original (arg)
  "Insert the message being replied to, if any (in Mail mode).
Puts point before the text and mark after.
Indents each nonblank line ARG spaces (default 3).
Just \\[universal-argument] as argument means don't indent
and don't delete any header fields."
  (interactive "P")
  (mail-yank-original arg)
  (exchange-point-and-mark)
  (run-hooks 'news-reply-header-hook))

(defvar news-reply-header-hook
  (lambda ()
    (insert "In article " news-reply-yank-message-id
	    " " news-reply-yank-from " writes:\n\n"))
  "Hook for inserting a header at the top of a yanked message.")

(defun news-reply-newsgroups ()
  "Move point to end of `Newsgroups:' field.
RFC 850 constrains the `Newsgroups:' field to be a comma-separated list
of valid newsgroup names at your site.  For example,
   Newsgroups: news.misc,comp.misc,rec.misc"
  (interactive)
  (expand-abbrev)
  (goto-char (point-min))
  (mail-position-on-field "Newsgroups"))

(defun news-reply-followup-to ()
  "Move point to end of `Followup-To:' field.  Create the field if none.
One usually requests followups to only one newsgroup.
RFC 850 constrains the `Followup-To:' field to be a comma-separated list
of valid newsgroups names at your site, and it must be a subset of the
`Newsgroups:' field.  For example:
   Newsgroups: news.misc,comp.misc,rec.misc,misc.misc,soc.misc
   Followup-To: news.misc,comp.misc,rec.misc"
  (interactive)
  (expand-abbrev)
  (or (mail-position-on-field "Followup-To" t)
      (progn (mail-position-on-field "newsgroups")
	     (insert "\nFollowup-To: ")))
	 ;; @@ could do a completing read based on the Newsgroups: field to
	 ;; @@ fill in the Followup-To: field
)

(defun news-reply-distribution ()
  "Move point to end of `Distribution:' optional field.
Create the field if none.  Without this field the posting goes to all of
USENET.  The field is used to restrict the posting to parts of USENET."
  (interactive)
  (expand-abbrev)
  (mail-position-on-field "Distribution")
  ;; @@could do a completing read based on the news library file:
  ;; @@    ../distributions  to fill in the field.
  )

(defun news-reply-keywords ()
  "Move point to end of `Keywords:' optional field.  Create the field if none.
Used as an aid to the news reader, it can contain a few, well selected keywords
identifying the message."
  (interactive)
  (expand-abbrev)
  (mail-position-on-field "Keywords"))

(defun news-reply-summary ()
  "Move point to end of `Summary:' optional field.  Create the field if none.
Used as an aid to the news reader, it can contain a succinct
summary (abstract) of the message."
  (interactive)
  (expand-abbrev)
  (mail-position-on-field "Summary"))

(defun news-reply-signature ()
  "The inews program appends `~/.signature' automatically."
  (interactive)
  (message "Posting news will append your signature automatically."))

(defun news-setup (to subject in-reply-to newsgroups replybuffer)
  "Set up the news reply or posting buffer with the proper headers and mode."
  (setq mail-reply-buffer replybuffer)
  (let ((mail-setup-hook nil)
	;; Avoid inserting a signature.
       	(mail-signature))
    (if (null to)
	;; this hack is needed so that inews wont be confused by
	;; the fcc: and bcc: fields
	(let ((mail-self-blind nil)
	      (mail-archive-file-name nil))
	  (mail-setup to subject in-reply-to nil replybuffer nil)
	  (beginning-of-line)
	  (delete-region (point) (progn (forward-line 1) (point)))
	  (goto-char (point-max)))
      (mail-setup to subject in-reply-to nil replybuffer nil))
    ;;;(mail-position-on-field "Posting-Front-End")
    ;;;(insert (emacs-version))
    (goto-char (point-max))
    (if (let ((case-fold-search t))
	  (re-search-backward "^Subject:" (point-min) t))
	(progn (beginning-of-line)
	       (insert "Newsgroups: " (or newsgroups "") "\n")
	       (if (not newsgroups)
		   (backward-char 1)
		 (goto-char (point-max)))))
    (let (actual-header-separator)
      (rfc822-goto-eoh)
      (setq actual-header-separator (buffer-substring
				     (point)
				     (save-excursion (end-of-line) (point))))
      (setq paragraph-start
	    (concat "^" actual-header-separator "$\\|" paragraph-start))
      (setq paragraph-separate
	    (concat "^" actual-header-separator "$\\|" paragraph-separate)))
    (run-hooks 'news-setup-hook)))

(defun news-inews ()
  "Send a news message using inews."
  (interactive)
  (let* (newsgroups subject
		    (case-fold-search nil))
    (save-excursion
      (save-restriction
	(narrow-to-region (point-min) (mail-header-end))
	(setq newsgroups (mail-fetch-field "newsgroups")
	      subject (mail-fetch-field "subject")))
      (widen)
      (goto-char (point-min))
      (run-hooks 'news-inews-hook)
      (mail-sendmail-undelimit-header)
      (goto-char (point-max))
      ;; require a newline at the end for inews to append .signature to
      (or (= (preceding-char) ?\n)
	  (insert ?\n))
      (message "Posting to USENET...")
      (unwind-protect
	  (if (not (eq 0
		       (call-process-region (point-min) (point-max)
					    news-inews-program nil 0 nil
					    "-h"))) ; take all header lines!
					;@@ setting of subject and newsgroups still needed?
					;"-t" subject
					;"-n" newsgroups
	      (error "Posting to USENET failed")
	    (message "Posting to USENET... done"))
	(mail-sendmail-delimit-header)
	(set-buffer-modified-p nil)))
    (bury-buffer)))

;@@ shares some code with news-reply and news-post-news
(defun news-mail-reply ()
  "Mail a reply to the author of the current article.
While composing the reply, use \\[news-reply-yank-original] to yank the
original message into it."
  (interactive)
  (let (from cc subject date to reply-to message-id
	     (buffer (current-buffer)))
    (save-restriction
      (narrow-to-region (point-min) (progn (goto-line (point-min))
					   (search-forward "\n\n")
					   (- (point) 1)))
      (setq from (mail-fetch-field "from")
	    subject (mail-fetch-field "subject")
	    reply-to (mail-fetch-field "reply-to")
	    date (mail-fetch-field "date")
	    message-id (mail-fetch-field "message-id")))
    (setq to from)
    (pop-to-buffer "*mail*")
    (mail nil
	  (if reply-to reply-to to)
	  subject
	  (let ((stop-pos (string-match "  *at \\|  *@ \\| *(\\| *<" from)))
	    (concat (if stop-pos (substring from 0 stop-pos) from)
		    "'s message "
		    (if message-id
			(concat message-id " of ")
		      "of ")
		    date))
	  nil
	 buffer)))

;@@ the guts of news-reply and news-post-news should be combined. -tower
(defun news-reply ()
  "Compose and post a reply (aka a followup) to the current article on USENET.
While composing the followup, use \\[news-reply-yank-original] to yank the
original message into it."
  (interactive)
  (if (y-or-n-p "Are you sure you want to followup to all of USENET? ")
      (let (from cc subject date to followup-to newsgroups message-of
		 references distribution message-id
		 (buffer (current-buffer)))
	(save-restriction
	  (and (not (= 0 (buffer-size))) ;@@real problem is non-existence of
					;@@	of article file
	       (equal major-mode 'news-mode) ;@@ if rmail-mode,
					;@@	should show full headers
	       (progn
		 (news-show-all-headers) ;@@ should save/restore header state,
					;@@	but rnews.el lacks support
		 (narrow-to-region (point-min) (progn (goto-char (point-min))
						      (search-forward "\n\n")
						      (- (point) 1)))))
	  (setq from (mail-fetch-field "from")
		news-reply-yank-from from
		;; @@ not handling old Title: field
		subject (mail-fetch-field "subject")
		date (mail-fetch-field "date")
		followup-to (mail-fetch-field "followup-to")
		newsgroups (or followup-to
			       (mail-fetch-field "newsgroups"))
		references (mail-fetch-field "references")
		;; @@ not handling old Article-I.D.: field
		distribution (mail-fetch-field "distribution")
		message-id (mail-fetch-field "message-id")
		news-reply-yank-message-id message-id)
	  (pop-to-buffer "*post-news*")
	  (news-reply-mode)
	  (if (and (buffer-modified-p)
		   (not
		    (y-or-n-p "Unsent article being composed; erase it? ")))
	      ()
	    (progn
	      (erase-buffer)
	      (and subject
		   (progn (if (string-match "\\`Re: " subject)
			      (while (string-match "\\`Re: " subject)
				(setq subject (substring subject 4))))
			  (setq subject (concat "Re: " subject))))
	      (and from
		   (progn
		     (let ((stop-pos
			    (string-match "  *at \\|  *@ \\| *(\\| *<" from)))
		       (setq message-of
			     (concat
			      (if stop-pos (substring from 0 stop-pos) from)
			      "'s message "
			      (if message-id
				  (concat message-id " of ")
				"of ")
			      date)))))
	      (news-setup
	       nil
	       subject
	       message-of
	       newsgroups
	       buffer)
	      (if followup-to
		  (progn (news-reply-followup-to)
			 (insert followup-to)))
	      (if distribution
		  (progn
		    (mail-position-on-field "Distribution")
		    (insert distribution)))
	      (mail-position-on-field "References")
	      (if references
		  (insert references))
	      (if (and references message-id)
		  (insert " "))
	      (if message-id
		  (insert message-id))
	      (goto-char (point-max))))))
    (message "")))

;@@ the guts of news-reply and news-post-news should be combined. -tower
;;;###autoload
(defun news-post-news (&optional noquery)
  "Begin editing a new USENET news article to be posted.
Type \\[describe-mode] once editing the article to get a list of commands.
If NOQUERY is non-nil, we do not query before doing the work."
  (interactive)
  (if (or noquery
	  (y-or-n-p "Are you sure you want to post to all of USENET? "))
      (let ((buffer (current-buffer)))
	(save-restriction
	  (and (not (= 0 (buffer-size))) ;@@real problem is non-existence of
					;@@	of article file
	       (equal major-mode 'news-mode) ;@@ if rmail-mode,
					;@@	should show full headers
	       (progn
		 (news-show-all-headers) ;@@ should save/restore header state,
					;@@	but rnews.el lacks support
		 (narrow-to-region (point-min) (progn (goto-char (point-min))
						      (search-forward "\n\n")
						      (- (point) 1)))))
	  (setq news-reply-yank-from (mail-fetch-field "from")
		;; @@ not handling old Article-I.D.: field
		news-reply-yank-message-id (mail-fetch-field "message-id")))
	(pop-to-buffer "*post-news*")
	(news-reply-mode)
	(if (and (buffer-modified-p)
		 (not (y-or-n-p "Unsent article being composed; erase it? ")))
	    ()				;@@ not saving point from last time
	  (progn (erase-buffer)
		 (news-setup () () () () buffer))))
  (message "")))

(defun news-mail-other-window ()
  "Send mail in another window.
While composing the message, use \\[news-reply-yank-original] to yank the
original message into it."
  (interactive)
  (mail-other-window nil nil nil nil nil (current-buffer)))

(provide 'rnewspost)

;;; arch-tag: 18f7b2af-cf9a-49e4-878b-71eb49913e00
;;; rnewspost.el ends here
