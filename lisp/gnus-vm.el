;;; gnus-vm.el --- vm interface for Gnus

;; Copyright (C) 1994,95 Free Software Foundation, Inc.

;; Author: Per Persson <pp@solace.mh.se>
;; Keywords: news, mail

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

;; Major contributors: 
;;	Christian Limpach <Christian.Limpach@nice.ch>
;; Some code stolen from: 
;;	Rick Sladkey <jrs@world.std.com>

;;; Code:

(require 'sendmail)
(require 'gnus)
(require 'gnus-msg)

(eval-when-compile
  (autoload 'vm-mode "vm")
  (autoload 'vm-save-message "vm")
  (autoload 'vm-forward-message "vm")
  (autoload 'vm-reply "vm")
  (autoload 'vm-mail "vm"))

(defvar gnus-vm-inhibit-window-system nil
  "Inhibit loading `win-vm' if using a window-system.
Has to be set before gnus-vm is loaded.")

(or gnus-vm-inhibit-window-system
    (condition-case nil
	(if window-system
	    (require 'win-vm))
      (error nil)))

(if (not (featurep 'vm))
    (load "vm"))

(defun gnus-vm-make-folder (&optional buffer)
  (let ((article (or buffer (current-buffer)))
	(tmp-folder (generate-new-buffer " *tmp-folder*"))
	(start (point-min))
	(end (point-max)))
    (set-buffer tmp-folder)
    (insert-buffer-substring article start end)
    (goto-char (point-min))
    (if (looking-at "^\\(From [^ ]+ \\).*$")
	(replace-match (concat "\\1" (current-time-string)))
      (insert "From " gnus-newsgroup-name " "
	      (current-time-string) "\n"))
    (while (re-search-forward "\n\nFrom " nil t)
      (replace-match "\n\n>From "))
    ;; insert a newline, otherwise the last line gets lost
    (goto-char (point-max))
    (insert "\n")
    (vm-mode)
    tmp-folder))
  
(defun gnus-summary-save-article-vm (&optional arg)
  "Append the current article to a vm folder.
If N is a positive number, save the N next articles.
If N is a negative number, save the N previous articles.
If N is nil and any articles have been marked with the process mark,
save those articles instead."
  (interactive "P")
  (let ((gnus-default-article-saver 'gnus-summary-save-in-vm))
    (gnus-summary-save-article arg)))

(defun gnus-summary-save-in-vm (&optional folder)
  (interactive)
  (let ((default-name
	  (funcall gnus-mail-save-name gnus-newsgroup-name
		   gnus-current-headers gnus-newsgroup-last-mail)))
    (or folder
	(setq folder
	      (read-file-name
	       (concat "Save article in VM folder: (default "
		       (file-name-nondirectory default-name) ") ")
	       (file-name-directory default-name)
	       default-name)))
    (setq folder
	  (expand-file-name folder
			    (and default-name
				 (file-name-directory default-name))))
    (gnus-make-directory (file-name-directory folder))
    (set-buffer gnus-article-buffer)
    (save-excursion
      (save-restriction
	(widen)
	(let ((vm-folder (gnus-vm-make-folder)))
	  (vm-save-message folder)
	  (kill-buffer vm-folder))))
    ;; Remember the directory name to save articles.
    (setq gnus-newsgroup-last-mail folder)))
  
(defun gnus-mail-forward-using-vm (&optional buffer)
  "Forward the current message to another user using vm."
  (let* ((gnus-buffer (or buffer (current-buffer)))
	 (subject (gnus-forward-make-subject gnus-buffer)))
    (or (featurep 'win-vm)
	(if gnus-use-full-window
	    (pop-to-buffer gnus-article-buffer)
	  (switch-to-buffer gnus-article-buffer)))
    (gnus-copy-article-buffer)
    (set-buffer gnus-article-copy)
    (save-excursion
      (save-restriction
	(widen)
	(let ((vm-folder (gnus-vm-make-folder))
	      (vm-forward-message-hook
	       (append (symbol-value 'vm-forward-message-hook)
		       '((lambda ()
			   (save-excursion
			     (mail-position-on-field "Subject")
			     (beginning-of-line)
			     (looking-at "^\\(Subject: \\).*$")
			     (replace-match (concat "\\1" subject))))))))
	  (vm-forward-message)
	  (gnus-vm-init-reply-buffer gnus-buffer)
	  (run-hooks 'gnus-mail-hook)
	  (kill-buffer vm-folder))))))

(defun gnus-vm-init-reply-buffer (buffer)
  (make-local-variable 'gnus-summary-buffer)
  (setq gnus-summary-buffer buffer)
  (set 'vm-mail-buffer nil)
  (use-local-map (copy-keymap (current-local-map)))
  (local-set-key "\C-c\C-y" 'gnus-yank-article))
  
(defun gnus-mail-reply-using-vm (&optional yank)
  "Compose reply mail using vm.
Optional argument YANK means yank original article.
The command \\[vm-yank-message] yank the original message into current buffer."
  (let ((gnus-buffer (current-buffer)))
    (gnus-copy-article-buffer)
    (set-buffer gnus-article-copy)
    (save-excursion
      (save-restriction
	(widen)
	(let ((vm-folder (gnus-vm-make-folder gnus-article-copy)))
	  (vm-reply 1)
	  (gnus-vm-init-reply-buffer gnus-buffer)
	  (setq gnus-buffer (current-buffer))
	  (and yank
	       ;; nil will (magically :-)) yank the current article
	       (gnus-yank-article nil))
	  (kill-buffer vm-folder))))
    (if (featurep 'win-vm) nil
      (pop-to-buffer gnus-buffer))
    (run-hooks 'gnus-mail-hook)))

(defun gnus-mail-other-window-using-vm ()
  "Compose mail in the other window using VM."
  (interactive)
  (let ((gnus-buffer (current-buffer)))
    (vm-mail)
    (gnus-vm-init-reply-buffer gnus-buffer))
  (run-hooks 'gnus-mail-hook))

(defun gnus-yank-article (article &optional prefix)
  ;; Based on vm-yank-message by Kyle Jones.
  "Yank article number N into the current buffer at point.
When called interactively N is read from the minibuffer.

This command is meant to be used in GNUS created Mail mode buffers;
the yanked article comes from the newsgroup containing the article
you are replying to or forwarding.

All article headers are yanked along with the text.  Point is left
before the inserted text, the mark after.  Any hook functions bound to
`mail-citation-hook' are run, after inserting the text and setting
point and mark.

Prefix arg means to ignore `mail-citation-hook', don't set the mark,
prepend the value of `vm-included-text-prefix' to every yanked line.
For backwards compatibility, if `mail-citation-hook' is set to nil,
`mail-yank-hooks' is run instead.  If that is also nil, a default
action is taken."
  (interactive
   (list
    (let ((result 0)
	  default prompt)
      (setq default (and gnus-summary-buffer
			 (save-excursion
			   (set-buffer gnus-summary-buffer)
			   (and gnus-current-article
				(int-to-string gnus-current-article))))
	    prompt (if default
		       (format "Yank article number: (default %s) " default)
		     "Yank article number: "))
      (while (and (not (stringp result)) (zerop result))
	(setq result (read-string prompt))
	(and (string= result "") default (setq result default))
	(or (string-match "^<.*>$" result)
	    (setq result (string-to-int result))))
      result)
    current-prefix-arg))
  (if gnus-summary-buffer
      (save-excursion
	(let ((message (current-buffer))
	      (start (point)) end
	      (tmp (generate-new-buffer " *tmp-yank*")))
	  (set-buffer gnus-summary-buffer)
	  ;; Make sure the connection to the server is alive.
	  (or (gnus-server-opened (gnus-find-method-for-group
				   gnus-newsgroup-name))
	      (progn
		(gnus-check-server 
		 (gnus-find-method-for-group gnus-newsgroup-name))
		(gnus-request-group gnus-newsgroup-name t)))
	  (and (stringp article) 
	       (let ((gnus-override-method gnus-refer-article-method))
		 (gnus-read-header article)))
	  (gnus-request-article (or article
				    gnus-current-article)
				gnus-newsgroup-name tmp)
	  (set-buffer tmp)
	  (run-hooks 'gnus-article-prepare-hook)
	  ;; Decode MIME message.
	  (if (and gnus-show-mime
		   (gnus-fetch-field "Mime-Version"))
	      (funcall gnus-show-mime-method))
	  ;; Perform the article display hooks.
	  (let ((buffer-read-only nil))
	    (run-hooks 'gnus-article-display-hook))
	  (append-to-buffer message (point-min) (point-max))
	  (kill-buffer tmp)
	  (set-buffer message)
	  (setq end (point))
	  (goto-char start)
	  (if (or prefix
		  (not (or mail-citation-hook mail-yank-hooks)))
	      (save-excursion
		(while (< (point) end)
		  (insert (symbol-value 'vm-included-text-prefix))
		  (forward-line 1)))
	    (push-mark end)
	    (cond
	     (mail-citation-hook (run-hooks 'mail-citation-hook))
	     (mail-yank-hooks (run-hooks 'mail-yank-hooks))))))))

(provide 'gnus-vm)

;;; gnus-vm.el ends here.
