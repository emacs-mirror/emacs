;;; gnus-draft.el --- draft message support for Gnus
;; Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003
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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

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
    "e"  gnus-draft-edit-message ;; Use `B w' for `gnus-summary-edit-article'
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
  (let ((article (gnus-summary-article-number))
	(group gnus-newsgroup-name))
    (gnus-summary-mark-as-read article gnus-canceled-mark)
    (gnus-draft-setup article group t)
    (set-buffer-modified-p t)
    (save-excursion
      (save-restriction
	(message-narrow-to-headers)
	(message-remove-header "date")))
    (save-buffer)
    (let ((gnus-verbose-backends nil))
      (gnus-request-expire-articles (list article) group t))
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
  (let* ((articles (gnus-summary-work-articles n))
	 (total (length articles))
	 article)
    (while (setq article (pop articles))
      (gnus-summary-remove-process-mark article)
      (unless (memq article gnus-newsgroup-unsendable)
	(let ((message-sending-message
	       (format "Sending message %d of %d..."
		       (- total (length articles)) total)))
	  (gnus-draft-send article gnus-newsgroup-name t))
	(gnus-summary-mark-article article gnus-canceled-mark)))))

(defun gnus-draft-send (article &optional group interactive)
  "Send message ARTICLE."
  (let* ((is-queue (or (not group)
                       (equal group "nndraft:queue")))
         (message-syntax-checks (if interactive message-syntax-checks
                                  'dont-check-for-anything-just-trust-me))
         (message-hidden-headers nil)
         (message-inhibit-body-encoding (or is-queue
                                            message-inhibit-body-encoding))
         (message-send-hook (and (not is-queue)
                                 message-send-hook))
         (message-setup-hook (and (not is-queue)
                                  message-setup-hook))
         (gnus-agent-queue-mail (and (not is-queue)
                                     gnus-agent-queue-mail))
	 (rfc2047-encode-encoded-words nil)
         type method move-to)
    (gnus-draft-setup article (or group "nndraft:queue"))
    ;; We read the meta-information that says how and where
    ;; this message is to be sent.
    (save-restriction
      (message-narrow-to-head)
      (when (re-search-forward
	     (concat "^" (regexp-quote gnus-agent-target-move-group-header)
		     ":") nil t)
	(skip-syntax-forward "-")
	(setq move-to (buffer-substring (point) (gnus-point-at-eol)))
	(message-remove-header gnus-agent-target-move-group-header))
      (goto-char (point-min))
      (when (re-search-forward
	     (concat "^" (regexp-quote gnus-agent-meta-information-header) ":")
	     nil t)
	(setq type (ignore-errors (read (current-buffer)))
	      method (ignore-errors (read (current-buffer))))
	(message-remove-header gnus-agent-meta-information-header)))
    ;; Let Agent restore any GCC lines and have message.el perform them.
    (gnus-agent-restore-gcc)
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
		     (if move-to
			 (gnus-inews-do-gcc move-to)
		       (message-send-and-exit)))
		 (if move-to
		     (gnus-inews-do-gcc move-to)
		   (message-send-and-exit))))
      (let ((gnus-verbose-backends nil))
	(gnus-request-expire-articles
	 (list article) (or group "nndraft:queue") t)))))

(defun gnus-draft-send-all-messages ()
  "Send all the sendable drafts."
  (interactive)
  (when (or
	 gnus-expert-user
	 (gnus-y-or-n-p
	  "Send all drafts? "))
    (gnus-uu-mark-buffer)
    (gnus-draft-send-message)))

(defun gnus-group-send-queue ()
  "Send all sendable articles from the queue group."
  (interactive)
  (when (or gnus-plugged
	    (not gnus-agent-prompt-send-queue)
	    (gnus-y-or-n-p "Gnus is unplugged; really send queue? "))
    (gnus-activate-group "nndraft:queue")
    (save-excursion
      (let* ((articles (nndraft-articles))
	     (unsendable (gnus-uncompress-range
			  (cdr (assq 'unsend
				     (gnus-info-marks
				      (gnus-get-info "nndraft:queue"))))))
	     (gnus-posting-styles nil)
	     (total (length articles))
	     article)
	(while (setq article (pop articles))
	  (unless (memq article unsendable)
	    (let ((message-sending-message
		   (format "Sending message %d of %d..."
			   (- total (length articles)) total)))
	      (gnus-draft-send article))))))))

;;;###autoload
(defun gnus-draft-reminder ()
  "Reminder user if there are unsent drafts."
  (interactive)
  (if (gnus-alive-p)
      (let (active)
	(catch 'continue
	  (dolist (group '("nndraft:drafts" "nndraft:queue"))
	    (setq active (gnus-activate-group group))
	    (if (and active (>= (cdr active) (car active)))
		(if (y-or-n-p "There are unsent drafts.  Confirm to exit? ")
		    (throw 'continue t)
		  (error "Stop!"))))))))

;;; Utility functions

;;;!!!If this is byte-compiled, it fails miserably.
;;;!!!This is because `gnus-setup-message' uses uninterned symbols.
;;;!!!This has been fixed in recent versions of Emacs and XEmacs,
;;;!!!but for the time being, we'll just run this tiny function uncompiled.

(progn
  (defun gnus-draft-setup (narticle group &optional restore)
    (let (ga)
      (gnus-setup-message 'forward
	(let ((article narticle))
	  (message-mail)
	  (erase-buffer)
	  (if (not (gnus-request-restore-buffer article group))
	      (error "Couldn't restore the article")
	    (when (and restore
		       (equal group "nndraft:queue"))
	      (mime-to-mml))
	    ;; Insert the separator.
	    (goto-char (point-min))
	    (search-forward "\n\n")
	    (forward-char -1)
	    (insert mail-header-separator)
	    (forward-line 1)
	    (setq ga (message-fetch-field gnus-draft-meta-information-header))
	    (message-set-auto-save-file-name))))
      (gnus-backlog-remove-article group narticle)
      (when (and ga
		 (ignore-errors (setq ga (car (read-from-string ga)))))
	(setq gnus-newsgroup-name
	      (if (equal (car ga) "") nil (car ga)))
	(gnus-configure-posting-styles)
	(setq gnus-message-group-art (cons gnus-newsgroup-name (cadr ga)))
	(setq message-post-method
	      `(lambda (arg)
		 (gnus-post-method arg ,(car ga))))
	(unless (equal (cadr ga) "")
	  (dolist (article (cdr ga))
	    (message-add-action
	     `(progn
		(gnus-add-mark ,(car ga) 'replied ,article)
		(gnus-request-set-mark ,(car ga) (list (list (list ,article)
							     'add '(reply)))))
	     'send)))))))

(defun gnus-draft-article-sendable-p (article)
  "Say whether ARTICLE is sendable."
  (not (memq article gnus-newsgroup-unsendable)))

(provide 'gnus-draft)

;;; arch-tag: 3d92af58-8c97-4a5c-9db4-a98e85198022
;;; gnus-draft.el ends here
