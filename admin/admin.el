;;; admin.el --- utilities for Emacs administration

;; Copyright (C) 2001, 2005 Free Software Foundation, Inc.

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

;; add-release-logs	Add ``Version X released'' change log entries.
;; set-version		Change Emacs version number in source tree.

;;; Code:

(defun process-lines (program &rest args)
  "Execute PROGRAM with ARGS, returning its output as a list of lines.
Signal an error if the program returns with a non-zero exit status."
  (with-temp-buffer
    (let ((status (apply 'call-process program nil (current-buffer) nil args)))
      (unless (eq status 0)
	(error "%s exited with status %s" program status))
      (goto-char (point-min))
      (let (lines)
	(while (not (eobp))
	  (setq lines (cons (buffer-substring-no-properties
			     (line-beginning-position)
			     (line-end-position))
			    lines))
	  (forward-line 1))
	(nreverse lines)))))

(defun add-release-logs (root version)
  "Add \"Version VERSION released.\" change log entries in ROOT.
Root must be the root of an Emacs source tree."
  (interactive "DEmacs root directory: \nNVersion number: ")
  (setq root (expand-file-name root))
  (unless (file-exists-p (expand-file-name "src/emacs.c" root))
    (error "%s doesn't seem to be the root of an Emacs source tree" root))
  (require 'add-log)
  (let* ((logs (process-lines "find" root "-name" "ChangeLog"))
	 (entry (format "%s  %s  <%s>\n\n\t* Version %s released.\n\n"
			(funcall add-log-time-format)
			(or add-log-full-name (user-full-name))
			(or add-log-mailing-address user-mail-address)
			version)))
    (dolist (log logs)
      (unless (string-match "/gnus/" log)
	(find-file log)
	(goto-char (point-min))
	(insert entry)))))

(defun set-version-in-file (root file version rx)
  (find-file (expand-file-name file root))
  (goto-char (point-min))
  (unless (re-search-forward rx nil t)
    (error "Version not found in %s" file))
  (replace-match (format "%s" version) nil nil nil 1))

(defun set-version (root version)
  "Set Emacs version to VERSION in relevant files under ROOT.
Root must be the root of an Emacs source tree."
  (interactive "DEmacs root directory: \nsVersion number: ")
  (unless (file-exists-p (expand-file-name "src/emacs.c" root))
    (error "%s doesn't seem to be the root of an Emacs source tree" root))
  (set-version-in-file root "lisp/version.el" version
		       (rx (and "emacs-version" (0+ space)
				?\" (submatch (1+ (not (in ?\")))) ?\")))
  (set-version-in-file root "README" version
		       (rx (and "version" (1+ space)
				(submatch (1+ (in "0-9."))))))
  (set-version-in-file root "man/emacs.texi" version
		       (rx (and "EMACSVER" (1+ space)
				(submatch (1+ (in "0-9."))))))
  (set-version-in-file root "lispref/elisp.texi" version
		       (rx (and "EMACSVER" (1+ space)
				(submatch (1+ (in "0-9.")))))))

;;; arch-tag: 4ea83636-2293-408b-884e-ad64f22a3bf5
;; admin.el ends here.
