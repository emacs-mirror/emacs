;;; gnus-mh.el --- mh-e interface for Gnus

;; Copyright (C) 1994,95 Free Software Foundation, Inc.

;; Author: Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;;	Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
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

;;; Send mail using mh-e.

;; The following mh-e interface is all cooperative works of
;; tanaka@flab.fujitsu.CO.JP (TANAKA Hiroshi), kawabe@sra.CO.JP
;; (Yoshikatsu Kawabe), and shingu@casund.cpr.canon.co.jp (Toshiaki
;; SHINGU).

;;; Code:

(require 'mh-e)
(require 'mh-comp)
(require 'gnus)
(require 'gnus-msg)

(defun gnus-summary-save-article-folder (&optional arg)
  "Append the current article to an mh folder.
If N is a positive number, save the N next articles.
If N is a negative number, save the N previous articles.
If N is nil and any articles have been marked with the process mark,
save those articles instead."
  (interactive "P")
  (let ((gnus-default-article-saver 'gnus-summary-save-in-folder))
    (gnus-summary-save-article arg)))

(defun gnus-summary-save-in-folder (&optional folder)
  "Save this article to MH folder (using `rcvstore' in MH library).
Optional argument FOLDER specifies folder name."
  ;; Thanks to yuki@flab.Fujitsu.JUNET and ohm@kaba.junet.
  (mh-find-path)
  (let ((folder
	 (or folder
	     (mh-prompt-for-folder 
	      "Save article in"
	      (funcall gnus-folder-save-name gnus-newsgroup-name
		       gnus-current-headers gnus-newsgroup-last-folder)
	      t)))
	(errbuf (get-buffer-create " *Gnus rcvstore*")))
    (gnus-eval-in-buffer-window 
     gnus-article-buffer
     (save-restriction
       (widen)
       (unwind-protect
	   (call-process-region (point-min) (point-max)
				(expand-file-name "rcvstore" mh-lib)
				nil errbuf nil folder)
	 (set-buffer errbuf)
	 (if (zerop (buffer-size))
	     (message "Article saved in folder: %s" folder)
	   (message "%s" (buffer-string)))
	 (kill-buffer errbuf))))
    (setq gnus-newsgroup-last-folder folder)))

(defun gnus-mail-reply-using-mhe (&optional yank)
  "Compose reply mail using mh-e.
Optional argument YANK means yank original article.
The command \\[mh-yank-cur-msg] yank the original message into current buffer."
  (let (from cc subject date to reply-to to-userid orig-to
	     references message-id
	     (config (current-window-configuration))
	     buffer)
    (pop-to-buffer gnus-article-buffer)
    (setq buffer (current-buffer))
    (save-excursion
      (save-restriction
	(or gnus-user-login-name	; we need this
	    (setq gnus-user-login-name (or (getenv "USER")
					   (getenv "LOGNAME"))))

	(gnus-article-show-all-headers);; so colors are happy
	;; lots of junk to avoid mh-send deleting other windows
	(setq from (or (gnus-fetch-field "from") "")
	      subject (let ((subject (or (gnus-fetch-field "subject")
					 "(None)")))
			(if (and subject
				 (not (string-match "^[Rr][Ee]:.+$" subject)))
			    (concat "Re: " subject) subject))
	      reply-to (gnus-fetch-field "reply-to")
	      cc (gnus-fetch-field "cc")
	      orig-to (or (gnus-fetch-field "to") "")
	      date (gnus-fetch-field "date")
	      references (gnus-fetch-field "references")
	      message-id (gnus-fetch-field "message-id"))
	(setq to (or reply-to from))
	(setq to-userid (mail-strip-quoted-names orig-to))
	(if (or (string-match "," orig-to)
		(not (string-match (substring to-userid 0 
					      (string-match "@" to-userid))
				   gnus-user-login-name)))
	    (setq cc (concat (if cc (concat cc ", ") "") orig-to)))
        ;; mh-yank-cur-msg needs to have mh-show-buffer set in the 
        ;; *Article* buffer
	(setq mh-show-buffer buffer)))

    (mh-find-path)
    (mh-send-sub (or to "") (or cc "") 
		 (or subject "(None)") config);; Erik Selberg 1/23/94

    (let ((draft (current-buffer))
	  (gnus-mail-buffer (current-buffer))
	  mail-buf)
      (if (not yank)
	  (gnus-configure-windows 'reply 'force)
	(gnus-configure-windows 'reply-yank 'force))
      (setq mail-buf gnus-mail-buffer)
      (pop-to-buffer mail-buf);; always in the display, so won't have window probs
      (switch-to-buffer draft))

    ;;    (mh-send to (or cc "") subject);; shouldn't use according to mhe
    
    ;; note - current buffer is now draft!
    (save-excursion
      (mh-insert-fields
       "In-reply-to:"
       (concat
	(substring from 0 (string-match "  *at \\|  *@ \\| *(\\| *<" from))
	"'s message of " date))
      (nnheader-insert-references references message-id))

    ;; need this for mh-yank-cur-msg
    (setq mh-sent-from-folder buffer)
    (setq mh-sent-from-msg 1)
    (setq mh-show-buffer buffer)
    (setq mh-previous-window-config config))

  ;; Then, yank original article if requested.
  (if yank
      (let ((last (point)))
	(mh-yank-cur-msg)
	(goto-char last)))

  (run-hooks 'gnus-mail-hook))


;; gnus-mail-forward-using-mhe is contributed by Jun-ichiro Itoh
;; <itojun@ingram.mt.cs.keio.ac.jp>

(defun gnus-mail-forward-using-mhe (&optional buffer)
  "Forward the current message to another user using mh-e."
  ;; First of all, prepare mhe mail buffer.
  (let* ((to (read-string "To: "))
	 (cc (read-string "Cc: "))
	 (buffer (or buffer gnus-article-buffer))
	 (config (current-window-configuration));; need to add this - erik
	 (subject (gnus-forward-make-subject buffer)))
    (setq mh-show-buffer buffer)
    (mh-find-path)
    (mh-send-sub to (or cc "") (or subject "(None)") config);; Erik Selberg 1/23/94
    (let ((draft (current-buffer))
	  (gnus-mail-buffer (current-buffer))
	  mail-buf)
      (gnus-configure-windows 'reply-yank 'force)
      (setq mail-buf (eval (cdr (assq 'mail gnus-window-to-buffer))))
      (pop-to-buffer mail-buf);; always in the display, so won't have window probs
      (switch-to-buffer draft)
      )
    (save-excursion
      (goto-char (point-max))
      (insert "\n------- Forwarded Message\n\n")
      (insert-buffer buffer)
      (goto-char (point-max))
      (insert "\n------- End of Forwarded Message\n")
      (setq mh-sent-from-folder buffer)
      (setq mh-sent-from-msg 1)
      (setq mh-previous-window-config config)
      (run-hooks 'gnus-mail-hook)
      )))

(defun gnus-mail-other-window-using-mhe ()
  "Compose mail other window using mh-e."
  (let ((to (read-string "To: "))
	(cc (read-string "Cc: "))
	(subject (read-string "Subject: ")))
    (gnus-article-show-all-headers)	;I don't think this is really needed.
    (setq mh-show-buffer (current-buffer))
    (mh-find-path)
    (mh-send-other-window to cc subject)
    (setq mh-sent-from-folder (current-buffer))
    (setq mh-sent-from-msg 1)
    (run-hooks 'gnus-mail-hook)))

(defun gnus-Folder-save-name (newsgroup headers &optional last-folder)
  "Generate folder name from NEWSGROUP, HEADERS, and optional LAST-FOLDER.
If variable `gnus-use-long-file-name' is nil, it is +News.group.
Otherwise, it is like +news/group."
  (or last-folder
      (concat "+"
	      (if gnus-use-long-file-name
		  (gnus-capitalize-newsgroup newsgroup)
		(gnus-newsgroup-directory-form newsgroup)))))

(defun gnus-folder-save-name (newsgroup headers &optional last-folder)
  "Generate folder name from NEWSGROUP, HEADERS, and optional LAST-FOLDER.
If variable `gnus-use-long-file-name' is nil, it is +news.group.
Otherwise, it is like +news/group."
  (or last-folder
      (concat "+"
	      (if gnus-use-long-file-name
		  newsgroup
		(gnus-newsgroup-directory-form newsgroup)))))

;;; gnus-mh.el ends here
