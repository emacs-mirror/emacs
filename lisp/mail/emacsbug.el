;;; emacsbug.el --- command to report Emacs bugs to appropriate mailing list.

;; Copyright (C) 1985, 1994 Free Software Foundation, Inc.

;; Author: K. Shane Hartman
;; Maintainer: FSF
;; Keywords: maint

;; Not fully installed because it can work only on Internet hosts.
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

;; `M-x report-emacs-bug ' starts an email note to the Emacs maintainers
;; describing a problem.  Here's how it's done...

;;; Code:

;; >> This should be an address which is accessible to your machine,
;; >> otherwise you can't use this file.  It will only work on the
;; >> internet with this address.

(require 'sendmail)

(defvar bug-gnu-emacs "bug-gnu-emacs@prep.ai.mit.edu"
  "Address of mailing list for GNU Emacs bugs.")

(defvar report-emacs-bug-pretest-address "emacs-pretest-bug@gnu.ai.mit.edu"
  "Address of mailing list for GNU Emacs pretest bugs.")

(defvar report-emacs-bug-orig-text nil
  "The automatically-created initial text of bug report.")

;;;###autoload
(defun report-emacs-bug (topic)
  "Report a bug in GNU Emacs.
Prompts for bug subject.  Leaves you in a mail buffer."
  (interactive "sBug Subject: ")
  (if (mail nil
	    (if (string-match "\\..*\\..*\\." emacs-version)
		;; If there are four numbers in emacs-version,
		;; this is a pretest version.
		report-emacs-bug-pretest-address
	      bug-gnu-emacs)
	    topic)
      (let (user-point)
	;; The rest of this does not execute
	;; if the user was asked to confirm and said no.
	(goto-char (point-min))
	(re-search-forward (concat "^" (regexp-quote mail-header-separator) "\n"))
	(insert "In " (emacs-version) "\n")
	(if (and system-configuration-options
		 (not (equal system-configuration-options "")))
	    (insert "configured using `configure "
		    system-configuration-options "'\n"))
	(insert "\n")
	(insert "Please describe exactly what actions triggered the bug\n"
		"and the precise symptoms of the bug:\n\n") 
	(setq user-point (point))
	(insert "\n\n\n"
		"Recent input:\n")
	(let ((before-keys (point)))
	  (insert (mapconcat (lambda (key)
			       (if (or (integerp key)
				       (symbolp key)
				       (listp key))
				   (single-key-description key)
				 (prin1-to-string key nil)))
			     (recent-keys)
			     " "))
	  (save-restriction
	    (narrow-to-region before-keys (point))
	    (goto-char before-keys)
	    (while (progn (move-to-column 50) (not (eobp)))
	      (search-forward " " nil t)
	      (insert "\n"))))
	(let ((message-buf (get-buffer "*Messages*")))
	  (if message-buf
	      (progn
		(insert "\n\nRecent messages:\n")
		(insert-buffer-substring message-buf
					 (save-excursion
					   (set-buffer message-buf)
					   (goto-char (point-max))
					   (forward-line -10)
					   (point))
					 (save-excursion
					   (set-buffer message-buf)
					   (point-max))))))
	;; This is so the user has to type something
	;; in order to send easily.
	(use-local-map (nconc (make-sparse-keymap) (current-local-map)))
	(define-key (current-local-map) "\C-c\C-i" 'report-emacs-bug-info)
	(with-output-to-temp-buffer "*Bug Help*"
	  (princ (substitute-command-keys
		  "Type \\[mail-send-and-exit] to send the bug report.\n"))
	  (princ (substitute-command-keys
		  "Type \\[kill-buffer] RET to cancel (don't send it).\n"))
	  (terpri)
	  (princ (substitute-command-keys
		  "Type \\[report-emacs-bug-info] to visit in Info the Emacs Manual section
about when and how to write a bug report,
and what information to supply so that the bug can be fixed.
Type SPC to scroll through this section and its subsections.")))
	;; Make it less likely people will send empty messages.
	(make-local-variable 'mail-send-hook)
	(add-hook 'mail-send-hook 'report-emacs-bug-hook)
	(save-excursion
	  (goto-char (point-max))
	  (skip-chars-backward " \t\n")
	  (make-local-variable 'report-emacs-bug-orig-text)
	  (setq report-emacs-bug-orig-text (buffer-substring (point-min) (point))))
	(goto-char user-point))))

(defun report-emacs-bug-info ()
  "Go to the Info node on reporting Emacs bugs."
  (interactive)
  (info)
  (Info-directory)
  (Info-menu "emacs")
  (Info-goto-node "Bugs"))

(defun report-emacs-bug-hook ()
  (save-excursion
    (goto-char (point-max))
    (skip-chars-backward " \t\n")
    (if (and (= (- (point) (point-min))
		(length report-emacs-bug-orig-text))
	     (equal (buffer-substring (point-min) (point))
		    report-emacs-bug-orig-text))
	(error "No text entered in bug report"))))

(provide 'emacsbug)

;;; emacsbug.el ends here
