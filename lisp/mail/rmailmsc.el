;;; rmailmsc.el --- miscellaneous support functions for the RMAIL mail reader

;; Copyright (C) 1985, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: mail

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

;;; Commentary:

;;; Code:

;;;###autoload
(defun set-rmail-inbox-list (file-name)
  "Set the inbox list of the current RMAIL file to FILE-NAME.
You can specify one file name, or several names separated by commas.
If FILE-NAME is empty, remove any existing inbox list."
  (interactive "sSet mailbox list to (comma-separated list of filenames): ")

  (unless (eq major-mode 'rmail-mode)
    (error "set-rmail-inbox-list works only for an Rmail file"))

  (save-excursion
    (let ((names (rmail-parse-file-inboxes))
	  (standard-output nil))
      (if (or (not names)
	      (y-or-n-p (concat "Replace "
				(mapconcat 'identity names ", ")
				"? ")))
	  (let ((buffer-read-only nil))
	    (widen)
	    (goto-char (point-min))
	    (search-forward "\n\^_")
	    (re-search-backward "^Mail" nil t)
	    (forward-line 0)
	    (if (looking-at "Mail:")
		(delete-region (point)
			       (progn (forward-line 1)
				      (point))))
	    (if (not (string= file-name ""))
		(insert-before-markers "Mail: " file-name "\n"))))))
  (setq rmail-inbox-list (rmail-parse-file-inboxes))
  (rmail-show-message rmail-current-message))

;;; arch-tag: 74ed1d50-2c25-4cbd-b5ae-d29ed8aba6e4
;;; rmailmsc.el ends here
