;;; gnus-draft.el --- draft message support for Gnus
;; Copyright (C) 1997, 1998, 1999, 2000
;;        Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'gnus)
(require 'gnus-sum)
(require 'message)
(require 'gnus-msg)
(require 'nndraft)
(require 'gnus-agent)
(eval-when-compile (require 'cl))

;;; Draft minor mode

(defvar gnus-draft-mode nil
  "Minor mode for providing a draft summary buffers.")

(defvar gnus-draft-mode-map nil)

(unless gnus-draft-mode-map
  (setq gnus-draft-mode-map (make-sparse-keymap))

  (gnus-define-keys gnus-draft-mode-map
    "Dt" gnus-draft-toggle-sending
    "De" gnus-draft-edit-message
    "Ds" gnus-draft-send-message
    "DS" gnus-draft-send-all-messages))

(defun gnus-draft-make-menu-bar ()
  (unless (boundp 'gnus-draft-menu)
    (easy-menu-define
     gnus-draft-menu gnus-draft-mode-map ""
     '("Drafts"
       ["Toggle whether to send" gnus-draft-toggle-sending t]
       ["Edit" gnus-draft-edit-message t]
       ["Send selected message(s)" gnus-draft-send-message t]
       ["Send all messages" gnus-draft-send-all-messages t]
       ["Delete draft" gnus-summary-delete-article t]))))

(defun gnus-draft-mode (&optional arg)
  "Minor mode for providing a draft summary buffers.

\\{gnus-draft-mode-map}"
  (interactive "P")
  (when (eq major-mode 'gnus-summary-mode)
    (when (set (make-local-variable 'gnus-draft-mode)
	       (if (null arg) (not gnus-draft-mode)
		 (> (prefix-numeric-value arg) 0)))
      ;; Set up the menu.
      (when (gnus-visual-p 'draft-menu 'menu)
	(gnus-draft-make-menu-bar))
      (gnus-add-minor-mode 'gnus-draft-mode " Draft" gnus-draft-mode-map)
      (mml-mode)
      (gnus-run-hooks 'gnus-draft-mode-hook))))

;;; Commands

(defun gnus-draft-toggle-sending (article)
  "Toggle whether to send an article or not."
  (interactive (list (gnus-summary-article-number)))
  (if (gnus-draft-article-sendable-p article)
      (progn
	(push article gnus-newsgroup-unsendable)
	(gnus-summary-mark-article article gnus-unsendable-mark))
    (setq gnus-newsgroup-unsendable
	  (delq article gnus-newsgroup-unsendable))
    (gnus-summary-mark-article article gnus-unread-mark))
  (gnus-summary-position-point))

(defun gnus-draft-edit-message ()
  "Enter a mail/post buffer to edit and send the draft."
  (interactive)
  (let ((article (gnus-summary-article-number)))
    (gnus-summary-mark-as-read article gnus-canceled-mark)
    (gnus-draft-setup article gnus-newsgroup-name t)
    (set-buffer-modified-p t)
    (save-buffer)
    (let ((gnus-verbose-backends nil))
      (gnus-request-expire-articles (list article) gnus-newsgroup-name t))
    (push
     `((lambda ()
	 (when (gnus-buffer-exists-p ,gnus-summary-buffer)
	   (save-excursion
	     (set-buffer ,gnus-summary-buffer)
	     (gnus-cache-possibly-remove-article ,article nil nil nil t)))))
     message-send-actions)))

(defun gnus-draft-send-message (&optional n)
  "Send the current draft."
  (interactive "P")
  (let ((articles (gnus-summary-work-articles n))
	article)
    (while (setq article (pop articles))
      (gnus-summary-remove-process-mark article)
      (unless (memq article gnus-newsgroup-unsendable)
	(gnus-draft-send article gnus-newsgroup-name t)
	(gnus-summary-mark-article article gnus-canceled-mark)))))

(defun gnus-draft-send (article &optional group interactive)
  "Send message ARTICLE."
  (let ((message-syntax-checks (if interactive nil
				 'dont-check-for-anything-just-trust-me))
	(message-inhibit-body-encoding (or (not group) 
					   (equal group "nndraft:queue")
					   message-inhibit-body-encoding))
	(message-send-hook (and group (not (equal group "nndraft:queue"))
				message-send-hook))
	(message-setup-hook (and group (not (equal group "nndraft:queue"))
				 message-setup-hook))
	type method)
    (gnus-draft-setup article (or group "nndraft:queue"))
    ;; We read the meta-information that says how and where
    ;; this message is to be sent.
    (save-restriction
      (message-narrow-to-head)
      (when (re-search-forward
	     (concat "^" (regexp-quote gnus-agent-meta-information-header) ":")
	     nil t)
	(setq type (ignore-errors (read (current-buffer)))
	      method (ignore-errors (read (current-buffer))))
	(message-remove-header gnus-agent-meta-information-header)))
    ;; Then we send it.  If we have no meta-information, we just send
    ;; it and let Message figure out how.
    (when (and (or (null method)
		   (gnus-server-opened method)
		   (gnus-open-server method))
	       (if type
		   (let ((message-this-is-news (eq type 'news))
			 (message-this-is-mail (eq type 'mail))
			 (gnus-post-method method)
			 (message-post-method method))
		     (message-send-and-exit))
		 (message-send-and-exit)))
      (let ((gnus-verbose-backends nil))
	(gnus-request-expire-articles
	 (list article) (or group "nndraft:queue") t)))))

(defun gnus-draft-send-all-messages ()
  "Send all the sendable drafts."
  (interactive)
  (gnus-uu-mark-buffer)
  (gnus-draft-send-message))

(defun gnus-group-send-drafts ()
  "Send all sendable articles from the queue group."
  (interactive)
  (gnus-activate-group "nndraft:queue")
  (save-excursion
    (let ((articles (nndraft-articles))
	  (unsendable (gnus-uncompress-range
		       (cdr (assq 'unsend
				  (gnus-info-marks
				   (gnus-get-info "nndraft:queue"))))))
	  article)
      (while (setq article (pop articles))
	(unless (memq article unsendable)
	  (gnus-draft-send article))))))

;;; Utility functions

;;;!!!If this is byte-compiled, it fails miserably.
;;;!!!This is because `gnus-setup-message' uses uninterned symbols.
;;;!!!This has been fixed in recent versions of Emacs and XEmacs,
;;;!!!but for the time being, we'll just run this tiny function uncompiled.

(progn
  (defun gnus-draft-setup (narticle group &optional restore)
    (gnus-setup-message 'forward
      (let ((article narticle))
	(message-mail)
	(erase-buffer)
	(if (not (gnus-request-restore-buffer article group))
	    (error "Couldn't restore the article")
	  (if (and restore (equal group "nndraft:queue"))
	      (mime-to-mml))
	  ;; Insert the separator.
	  (goto-char (point-min))
	  (search-forward "\n\n")
	  (forward-char -1)
	  (insert mail-header-separator)
	  (forward-line 1)
	  (message-set-auto-save-file-name))))))

(defun gnus-draft-article-sendable-p (article)
  "Say whether ARTICLE is sendable."
  (not (memq article gnus-newsgroup-unsendable)))

(provide 'gnus-draft)

;;; gnus-draft.el ends here
