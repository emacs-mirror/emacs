;;; rmailmsc.el --- miscellaneous support functions for the RMAIL mail reader

;; Copyright (C) 1985, 2002, 2003, 2004, 2005, 2006, 2007, 2008,
;;   2009  Free Software Foundation, Inc.

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
  (require 'rmail))

(defvar rmail-current-message)
(defvar rmail-inbox-list)

(declare-function mail-parse-comma-list "mail-utils" ())
(declare-function rmail-show-message "rmail" (&optional msg))

;;;###autoload
(defun set-rmail-inbox-list (file-name)
  "Set the inbox list of the current RMAIL file to FILE-NAME.
You can specify one file name, or several names separated by commas.
If FILE-NAME is empty, remove any existing inbox list."
  (interactive "sSet mailbox list to (comma-separated list of filenames): ")
  (unless (eq major-mode 'rmail-mode)
    (error "set-rmail-inbox-list works only for an Rmail file"))
  (let ((inbox-list
	 (with-temp-buffer
	   (insert file-name)
	   (goto-char (point-min))
	   (nreverse (mail-parse-comma-list)))))
    (when (or (not rmail-inbox-list)
	      (y-or-n-p (concat "Replace "
				(mapconcat 'identity
					   rmail-inbox-list
					   ", ")
				"? ")))
      (message "Setting the inbox list for %s for this session"
	       (file-name-nondirectory (buffer-file-name)))
      (setq rmail-inbox-list inbox-list)))
  (rmail-show-message rmail-current-message))

;; Local Variables:
;; change-log-default-name: "ChangeLog.rmail"
;; End:

;; arch-tag: 94614a62-2a0a-4e25-bac9-06f461ed4c60
;;; rmailmsc.el ends here
