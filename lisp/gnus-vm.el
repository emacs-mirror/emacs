;;; gnus-vm.el --- vm interface for Gnus
;; Copyright (C) 1994,95,96 Free Software Foundation, Inc.

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
(require 'message)
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
    (setq folder
	  (cond ((eq folder 'default) default-name)
		(folder folder)
		(t (gnus-read-save-file-name 
		    "Save article in VM folder:" default-name))))
    (gnus-make-directory (file-name-directory folder))
    (set-buffer gnus-original-article-buffer)
    (save-excursion
      (save-restriction
	(widen)
	(let ((vm-folder (gnus-vm-make-folder)))
	  (vm-save-message folder)
	  (kill-buffer vm-folder))))
    ;; Remember the directory name to save articles.
    (setq gnus-newsgroup-last-mail folder)))

(provide 'gnus-vm)

;;; gnus-vm.el ends here.
