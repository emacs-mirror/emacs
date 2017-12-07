;;; trampver.el --- Transparent Remote Access, Multiple Protocol  -*- lexical-binding:t -*-
;;; lisp/trampver.el.  Generated from trampver.el.in by configure.

;; Copyright (C) 2003-2017 Free Software Foundation, Inc.

;; Author: Kai Großjohann <kai.grossjohann@gmx.net>
;; Maintainer: Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, processes
;; Package: tramp
;; Version: 2.3.3-pre

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

;;; Code:

;; In the Tramp GIT repository, the version number and the bug report
;; address are auto-frobbed from configure.ac, so you should edit that
;; file and run "autoconf && ./configure" to change them.  Emacs
;; version check is defined in macro AC_EMACS_INFO of aclocal.m4;
;; should be changed only there.

;;;###tramp-autoload
(defconst tramp-version "2.3.3-pre"
  "This version of Tramp.")

;;;###tramp-autoload
(defconst tramp-bug-report-address "tramp-devel@gnu.org"
  "Email address to send bug reports to.")

(defun tramp-repository-get-version ()
  "Try to return as a string the repository revision of the Tramp sources."
  (let ((dir (locate-dominating-file (locate-library "tramp") ".git")))
    (when dir
      (with-temp-buffer
	(let ((default-directory (file-name-as-directory dir)))
	  (and (zerop
		(ignore-errors
		  (call-process "git" nil '(t nil) nil "rev-parse" "HEAD")))
	       (not (zerop (buffer-size)))
	       (replace-regexp-in-string "\n" "" (buffer-string))))))))

;; Check for Emacs version.
(let ((x (if (>= emacs-major-version 24)
    "ok"
  (format "Tramp 2.3.3-pre is not fit for %s"
	  (when (string-match "^.*$" (emacs-version))
	    (match-string 0 (emacs-version)))))))
  (unless (string-match "\\`ok\\'" x) (error "%s" x)))

;; Tramp versions integrated into Emacs.
(add-to-list
 'customize-package-emacs-version-alist
 '(Tramp ("2.0.55" . "22.1") ("2.0.57" . "22.2") ("2.0.58-pre" . "22.3")
	 ("2.1.15" . "23.1") ("2.1.18-23.2" . "23.2")
	 ("2.1.20" . "23.3") ("2.1.21-pre" . "23.4")
	 ("2.2.3-24.1" . "24.1") ("2.2.3-24.1" . "24.2") ("2.2.6-24.3" . "24.3")
	 ("2.2.9-24.4" . "24.4") ("2.2.11-24.5" . "24.5")
	 ("2.2.13.25.1" . "25.1") ("2.2.13.25.2" . "25.2")
	 ("2.2.13.25.2" . "25.3")
	 ("2.3.3.26.1" . "26.1")))

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'trampver 'force)))

(provide 'trampver)

;;; trampver.el ends here

;; Local Variables:
;; mode: Emacs-Lisp
;; coding: utf-8
;; End:
