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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; `M-x report-emacs-bug ' starts an email note to the Emacs maintainers
;; describing a problem.  Here's how it's done...

;;; Code:

;; >> This should be an address which is accessible to your machine,
;; >> otherwise you can't use this file.  It will only work on the
;; >> internet with this address.

(require 'sendmail)

(defvar bug-gnu-emacs "bug-gnu-emacs@prep.ai.mit.edu"
  "Address of site maintaining mailing list for GNU Emacs bugs.")

(defvar report-emacs-bug-orig-text nil
  "The automatically-created initial text of bug report.")

;;;###autoload
(defun report-emacs-bug (topic)
  "Report a bug in GNU Emacs.
Prompts for bug subject.  Leaves you in a mail buffer."
  (interactive "sBug Subject: ")
  (mail nil bug-gnu-emacs topic)
  (goto-char (point-min))
  (re-search-forward (concat "^" (regexp-quote mail-header-separator) "\n"))
  (insert "In " (emacs-version) "\n\n")
  (message (substitute-command-keys "Type \\[mail-send-and-exit] to send bug report."))
  ;; Make it less likely people will send empty messages.
  (make-local-variable 'mail-send-hook)
  (add-hook 'mail-send-hook 'report-emacs-bug-hook)
  (save-excursion
    (goto-char (point-max))
    (skip-chars-backward " \t\n")
    (make-local-variable 'report-emacs-bug-orig-text)
    (setq report-emacs-bug-orig-text (buffer-substring (point-min) (point)))))

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
