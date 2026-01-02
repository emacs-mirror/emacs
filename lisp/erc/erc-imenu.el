;;; erc-imenu.el --- Imenu support for ERC  -*- lexical-binding: t; -*-

;; Copyright (C) 2001-2026 Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@delysid.org>
;; Maintainer: Amin Bandali <bandali@gnu.org>, F. Jason Park <jp@neverwas.me>
;; Keywords: comm
;; URL: https://www.emacswiki.org/emacs/ErcImenu

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains code related to Ibuffer and ERC.  Totally alpha,
;; needs work.  Usage:  Type / C-e C-h when in Ibuffer-mode to see new
;; limiting commands

;;; Code:

;;; Commentary:

;; This package defines the function `erc-create-imenu-index'.  ERC
;; uses this for `imenu-create-index-function', and autoloads it.
;; Therefore, nothing needs to be done to use this package.

;;; Code:

(require 'erc)
(require 'imenu)

(defgroup erc-imenu nil
  "Imenu integration for ERC."
  :group 'erc)

(defun erc-unfill-notice ()
  "Return text from point to a computed end as a string unfilled.
Don't rely on this function, read it first!"
  (let ((str (buffer-substring
	      (save-excursion
		(re-search-forward (regexp-quote erc-notice-prefix)))
	      (progn
		(while (save-excursion
			 (forward-line 1)
			 (looking-at "    "))
		  (forward-line 1))
		(end-of-line) (point))))
	(inhibit-read-only t))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (re-search-forward "\n +" nil t)
	(replace-match " "))
      (buffer-substring (point-min) (point-max)))))

;;;###autoload
(defun erc-create-imenu-index ()
  (let ((index-alist '())
	(notice-alist '())
	(join-alist '())
	(left-alist '())
	(quit-alist '())
	(message-alist '())
	(mode-change-alist '())
	(topic-change-alist '())
	) ;; prev-pos
    (goto-char (point-max))
    (while (if (bolp)
	       (> (forward-line -1)
		  -1)
	     (progn (forward-line 0)
		    t))
      (save-match-data
	(when (looking-at (concat (regexp-quote erc-notice-prefix)
				  "\\(.+\\)$"))
	  (let ((notice-text  ;; Ugly hack, but seems to work.
		 (save-excursion (erc-unfill-notice)))
		(pos (point)))
	    (push (cons notice-text pos) notice-alist)
	    (or
	     (when (string-match "^\\(.*\\) has joined channel" notice-text)
	       (push (cons (match-string 1 notice-text) pos) join-alist))
	     (when (string-match "^\\(.+\\) has left channel" notice-text)
	       (push (cons (match-string 1 notice-text) pos) left-alist))
	     (when (string-match "^\\(.+\\) has quit\\(.*\\)$" notice-text)
	       (push (cons (concat (match-string 1 notice-text)
				   (match-string 2 notice-text))
			   (point))
		     quit-alist))
	     (when (string-match
		    "^\\(\\S-+\\) (.+) has changed mode for \\S-+ to \\(.*\\)$"
		    notice-text)
	       (push (cons (concat (match-string 1 notice-text) ": "
				   (match-string 2 notice-text))
			   (point))
		     mode-change-alist))
	     (when (string-match
		    "^\\(\\S-+\\) (.+) has set the topic for \\S-+: \\(.*\\)$"
		    notice-text)
	       (push (cons (concat (match-string 1 notice-text) ": "
				   (match-string 2 notice-text))
			   pos)
		     topic-change-alist)))))
	(when (looking-at "<\\(\\S-+\\)> \\(.+\\)$")
	  (let ((from (match-string 1))
		(message-text (match-string 2)))
	    (push (cons (concat from ": " message-text) (point))
		  message-alist)))))
    (and notice-alist (push (cons "notices" notice-alist) index-alist))
    (and join-alist (push (cons "joined" join-alist) index-alist))
    (and left-alist (push (cons "parted" left-alist) index-alist))
    (and quit-alist (push (cons "quit" quit-alist) index-alist))
    (and mode-change-alist (push (cons "mode-change" mode-change-alist)
				 index-alist))
    (and message-alist (push (cons "messages" message-alist) index-alist))
    (and topic-change-alist (push (cons "topic-change" topic-change-alist)
				  index-alist))
    index-alist))

(defvar-local erc-imenu--create-index-function nil
  "Previous local value of `imenu-create-index-function', if any.")

;;;###autoload(autoload 'erc-imenu-mode "erc-imenu" nil t)
(define-erc-module imenu nil
  "Simple Imenu integration for ERC."
  ((add-hook 'erc-mode-hook #'erc-imenu-setup)
   (add-hook 'which-function-mode-hook #'erc-imenu--disable-which-func)
   (unless erc--updating-modules-p (erc-buffer-do #'erc-imenu-setup)))
  ((remove-hook 'erc-mode-hook #'erc-imenu-setup)
   (remove-hook 'which-function-mode-hook #'erc-imenu--disable-which-func)
   (erc-buffer-do #'erc-imenu-setup)))

(defun erc-imenu-setup ()
  "Set up or tear down Imenu integration."
  (if erc-imenu-mode
      (progn
        (when (and (local-variable-p 'imenu-create-index-function)
                   imenu-create-index-function)
          (setq erc-imenu--create-index-function imenu-create-index-function))
        (setq imenu-create-index-function #'erc-create-imenu-index)
        (when (boundp 'which-func-mode)
          (setq which-func-mode nil)))
    (when erc-imenu--create-index-function
      (setq imenu-create-index-function erc-imenu--create-index-function))
    (kill-local-variable 'erc-imenu--create-index-function)
    (kill-local-variable 'which-func-mode)))

(defun erc-imenu--disable-which-func ()
  "Silence `which-function-mode' in ERC buffers."
  (defvar which-func-mode)
  (erc-with-all-buffers-of-server nil nil
    (setq which-func-mode nil)))

(provide 'erc-imenu)

;;; erc-imenu.el ends here
;;
;; Local Variables:
;; generated-autoload-file: "erc-loaddefs.el"
;; End:
