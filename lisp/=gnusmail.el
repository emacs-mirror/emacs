;;; Mail reply commands for GNUS newsreader
;; Copyright (C) 1990 Masanobu UMEDA
;; $Header: gnusmail.el,v 1.1 90/03/23 13:24:39 umerin Locked $

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

(require 'gnus)

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

(autoload 'news-mail-reply "rnewspost")
(autoload 'news-mail-other-window "rnewspost")

(autoload 'mh-send "mh-e")
(autoload 'mh-send-other-window "mh-e")
(autoload 'mh-find-path "mh-e")
(autoload 'mh-yank-cur-msg "mh-e")

;;; Mail reply commands of GNUS Subject Mode

(defun gnus-Subject-mail-reply (yank)
  "Reply mail to news author.
If prefix arg YANK is non-nil, original article is yanked automatically.
Customize the variable `gnus-mail-reply-method' to use another mailer."
  (interactive "P")
  (gnus-Subject-select-article)
  (switch-to-buffer gnus-Article-buffer)
  (widen)
  (delete-other-windows)
  (bury-buffer gnus-Article-buffer)
  (funcall gnus-mail-reply-method yank))

(defun gnus-Subject-mail-reply-with-original ()
  "Reply mail to news author with original article."
  (interactive)
  (gnus-Subject-mail-reply t))

(defun gnus-Subject-mail-other-window ()
  "Compose mail in other window.
Customize the variable `gnus-mail-other-window-method' to use another mailer."
  (interactive)
  (gnus-Subject-select-article)
  (switch-to-buffer gnus-Article-buffer)
  (widen)
  (delete-other-windows)
  (bury-buffer gnus-Article-buffer)
  (funcall gnus-mail-other-window-method))


;;; Send mail using sendmail mail mode.

(defun gnus-mail-reply-using-mail (&optional yank)
  "Compose reply mail using mail.
Optional argument YANK means yank original article."
  (news-mail-reply)
  (gnus-overload-functions)
  (if yank
      (let ((last (point)))
	(goto-char (point-max))
	(mail-yank-original nil)
	(goto-char last)
	)))

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
The command \\[mh-yank-cur-msg] yanks the original message into current buffer."
  ;; First of all, prepare mhe mail buffer.
  (let (from cc subject date to reply-to (buffer (current-buffer)))
    (save-restriction
      (gnus-Article-show-all-headers)	;I don't think this is really needed.
      (setq from (gnus-fetch-field "from")
	    subject (let ((subject (gnus-fetch-field "subject")))
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

(defun gnus-mail-other-window-using-mhe ()
  "Compose mail other window using MH-E Mail."
  (let ((to (read-string "To: "))
	(cc (read-string "Cc: "))
	(subject (read-string "Subject: " (gnus-fetch-field "subject"))))
    (gnus-Article-show-all-headers)	;I don't think this is really needed.
    (setq mh-show-buffer (current-buffer))
    (mh-find-path)
    (mh-send-other-window to cc subject)
    (setq mh-sent-from-folder (current-buffer))
    (setq mh-sent-from-msg 1)))

(provide 'gnusmail)
