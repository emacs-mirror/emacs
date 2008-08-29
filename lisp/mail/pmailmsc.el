;;; pmailmsc.el --- miscellaneous support functions for the PMAIL mail reader

;; Copyright (C) 1985, 2002, 2003, 2004, 2005,
;;   2006, 2007, 2008 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: mail

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'pmail))

(defvar pmail-current-message)
(defvar pmail-inbox-list)

(declare-function mail-parse-comma-list "mail-utils" ())
(declare-function pmail-parse-file-inboxes "pmail" ())
(declare-function pmail-show-message "pmail" (&optional n no-summary))

;;;###autoload
(defun set-pmail-inbox-list (file-name)
  "Set the inbox list of the current PMAIL file to FILE-NAME.
You can specify one file name, or several names separated by commas.
If FILE-NAME is empty, remove any existing inbox list."
  (interactive "sSet mailbox list to (comma-separated list of filenames): ")
  (unless (eq major-mode 'pmail-mode)
    (error "set-pmail-inbox-list works only for an Pmail file"))
  (let ((inbox-list
	 (with-temp-buffer
	   (insert file-name)
	   (goto-char (point-min))
	   (nreverse (mail-parse-comma-list)))))
    (when (or (not pmail-inbox-list)
	      (y-or-n-p (concat "Replace "
				(mapconcat 'identity
					   pmail-inbox-list
					   ", ")
				"? ")))
      (message "Setting the inbox list for %s for this session"
	       (file-name-nondirectory (buffer-file-name)))
      (setq pmail-inbox-list inbox-list)))
  (pmail-show-message pmail-current-message))

;; arch-tag: 94614a62-2a0a-4e25-bac9-06f461ed4c60
;;; pmailmsc.el ends here
