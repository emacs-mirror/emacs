;;; gnusmail.el --- mail reply commands for GNUS newsreader

;; Copyright (C) 1990, 1993 Free Software Foundation, Inc.

;; Author: Masanobu UMEDA <umerin@mse.kyutech.ac.jp>
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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Provides mail reply and mail other window command using usual mail
;; interface and mh-e interface.
;; 
;; To use MAIL: set the variables gnus-mail-reply-method and
;; gnus-mail-other-window-method to gnus-mail-reply-using-mail and
;; gnus-mail-other-window-using-mail, respectively.
;;
;; To use MH-E: set the variables gnus-mail-reply-method and
;; gnus-mail-other-window-method to gnus-mail-reply-using-mhe and
;; gnus-mail-other-window-using-mhe, respectively.

;;; Code:

(require 'gnus)

(autoload 'news-mail-reply "rnewspost")
(autoload 'news-mail-other-window "rnewspost")

(autoload 'mh-send "mh-e")
(autoload 'mh-send-other-window "mh-e")
(autoload 'mh-find-path "mh-e")
(autoload 'mh-yank-cur-msg "mh-e")

;;; Mail reply commands of GNUS Summary Mode

(defun gnus-summary-reply (yank)
  "Reply mail to news author.
If prefix argument YANK is non-nil, original article is yanked automatically.
Customize the variable gnus-mail-reply-method to use another mailer."
  (interactive "P")
  ;; Bug fix by jbw@bigbird.bu.edu (Joe Wells)
  ;; Stripping headers should be specified with mail-yank-ignored-headers.
  (gnus-summary-select-article t t)
  (switch-to-buffer gnus-article-buffer)
  (widen)
  (delete-other-windows)
  (bury-buffer gnus-article-buffer)
  (funcall gnus-mail-reply-method yank))

(defun gnus-summary-reply-with-original ()
  "Reply mail to news author with original article.
Customize the variable gnus-mail-reply-method to use another mailer."
  (interactive)
  (gnus-summary-reply t))

(defun gnus-summary-mail-forward ()
  "Forward the current message to another user.
Customize the variable gnus-mail-forward-method to use another mailer."
  (interactive)
  (gnus-summary-select-article)
  (switch-to-buffer gnus-article-buffer)
  (widen)
  (delete-other-windows)
  (bury-buffer gnus-article-buffer)
  (funcall gnus-mail-forward-method))

(defun gnus-summary-mail-other-window ()
  "Compose mail in other window.
Customize the variable gnus-mail-other-window-method to use another mailer."
  (interactive)
  (gnus-summary-select-article)
  (switch-to-buffer gnus-article-buffer)
  (widen)
  (delete-other-windows)
  (bury-buffer gnus-article-buffer)
  (funcall gnus-mail-other-window-method))


;;; Send mail using sendmail mail mode.

(defun gnus-mail-reply-using-mail (&optional yank)
  "Compose reply mail using mail.
Optional argument YANK means yank original article."
  (news-mail-reply)
  (gnus-overload-functions)
  (if yank
      (mail-yank-original nil)))

(defun gnus-mail-forward-using-mail ()
  "Forward the current message to another user using mail."
  ;; This is almost a carbon copy of rmail-forward in rmail.el.
  (let ((forward-buffer (current-buffer))
	(subject
	 (concat "[" gnus-newsgroup-name "] "
		 ;;(mail-strip-quoted-names (gnus-fetch-field "From")) ": "
		 (or (gnus-fetch-field "Subject") ""))))
    ;; If only one window, use it for the mail buffer.
    ;; Otherwise, use another window for the mail buffer
    ;; so that the Rmail buffer remains visible
    ;; and sending the mail will get back to it.
    (if (if (one-window-p t)
	    (mail nil nil subject)
	  (mail-other-window nil nil subject))
	(save-excursion
	  (goto-char (point-max))
	  (insert "------- Start of forwarded message -------\n")
	  (insert-buffer forward-buffer)
	  (goto-char (point-max))
	  (insert "------- End of forwarded message -------\n")
	  ;; You have a chance to arrange the message.
	  (run-hooks 'gnus-mail-forward-hook)
	  ))))

(defun gnus-mail-other-window-using-mail ()
  "Compose mail other window using mail."
  (news-mail-other-window)
  (gnus-overload-functions))


;;; Send mail using mh-e.

;; The following mh-e interface is all cooperative works of
;; tanaka@flab.fujitsu.CO.JP (TANAKA Hiroshi), kawabe@sra.CO.JP
;; (Yoshikatsu Kawabe), and shingu@casund.cpr.canon.co.jp (Toshiaki
;; SHINGU).

(defun gnus-mail-reply-using-mhe (&optional yank)
  "Compose reply mail using mh-e.
Optional argument YANK means yank original article.
The command \\[mh-yank-cur-msg] yank the original message into current buffer."
  ;; First of all, prepare mhe mail buffer.
  (let (from cc subject date to reply-to (buffer (current-buffer)))
    (save-restriction
      (gnus-article-show-all-headers)	;I don't think this is really needed.
      (setq from (gnus-fetch-field "from")
	    subject (let ((subject (or (gnus-fetch-field "subject")
				       "(None)")))
		      (if (and subject
			       (not (string-match "^[Rr][Ee]:.+$" subject)))
			  (concat "Re: " subject) subject))
	    reply-to (gnus-fetch-field "reply-to")
	    cc (gnus-fetch-field "cc")
	    date (gnus-fetch-field "date"))
      (setq mh-show-buffer buffer)
      (setq to (or reply-to from))
      (mh-find-path)
      (mh-send to (or cc "") subject)
      (save-excursion
	(mh-insert-fields
	 "In-reply-to:"
	 (concat
	  (substring from 0 (string-match "  *at \\|  *@ \\| *(\\| *<" from))
	  "'s message of " date)))
      (setq mh-sent-from-folder buffer)
      (setq mh-sent-from-msg 1)
      ))
  ;; Then, yank original article if requested.
  (if yank
      (let ((last (point)))
	(mh-yank-cur-msg)
	(goto-char last)
	)))

;; gnus-mail-forward-using-mhe is contributed by Jun-ichiro Itoh
;; <itojun@ingram.mt.cs.keio.ac.jp>

(defun gnus-mail-forward-using-mhe ()
  "Forward the current message to another user using mh-e."
  ;; First of all, prepare mhe mail buffer.
  (let ((to (read-string "To: "))
 	(cc (read-string "Cc: "))
 	(buffer (current-buffer))
 	subject)
    ;;(gnus-article-show-all-headers)
    (setq subject
	  (concat "[" gnus-newsgroup-name "] "
		  ;;(mail-strip-quoted-names (gnus-fetch-field "From")) ": "
		  (or (gnus-fetch-field "subject") "")))
    (setq mh-show-buffer buffer)
    (mh-find-path)
    (mh-send to (or cc "") subject)
    (save-excursion
      (goto-char (point-max))
      (insert "\n------- Forwarded Message\n\n")
      (insert-buffer buffer)
      (goto-char (point-max))
      (insert "\n------- End of Forwarded Message\n")
      (setq mh-sent-from-folder buffer)
      (setq mh-sent-from-msg 1))))

(defun gnus-mail-other-window-using-mhe ()
  "Compose mail other window using mh-e."
  (let ((to (read-string "To: "))
	(cc (read-string "Cc: "))
	(subject (read-string "Subject: " (gnus-fetch-field "subject"))))
    (gnus-article-show-all-headers)	;I don't think this is really needed.
    (setq mh-show-buffer (current-buffer))
    (mh-find-path)
    (mh-send-other-window to cc subject)
    (setq mh-sent-from-folder (current-buffer))
    (setq mh-sent-from-msg 1)))

(provide 'gnusmail)

;;; gnusmail.el ends here
