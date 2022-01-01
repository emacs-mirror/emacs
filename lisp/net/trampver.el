;;; trampver.el --- Transparent Remote Access, Multiple Protocol  -*- lexical-binding:t -*-
;;; lisp/trampver.el.  Generated from trampver.el.in by configure.

;; Copyright (C) 2003-2022 Free Software Foundation, Inc.

;; Author: Kai Großjohann <kai.grossjohann@gmx.net>
;; Maintainer: Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, processes
;; Package: tramp
;; Version: 2.5.2.28.1
;; Package-Requires: ((emacs "25.1"))
;; Package-Type: multi
;; URL: https://www.gnu.org/software/tramp/

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

;; Convenience functions around the Tramp version.  Partly generated
;; during Tramp configuration.

;;; Code:

;; In the Tramp GIT repository, the version number, the bug report
;; address and the required Emacs version are auto-frobbed from
;; configure.ac, so you should edit that file and run "autoconf &&
;; ./configure" to change them.

;;;###tramp-autoload
(defconst tramp-version "2.5.2.28.1"
  "This version of Tramp.")

;;;###tramp-autoload
(defconst tramp-bug-report-address "tramp-devel@gnu.org"
  "Email address to send bug reports to.")

(defconst tramp-repository-branch
  (ignore-errors
    ;; Suppress message from `emacs-repository-get-branch'.  We must
    ;; also handle out-of-tree builds.
    (let ((inhibit-message t)
	  (debug-on-error nil)
	  (dir (or (locate-dominating-file (locate-library "tramp") ".git")
		   source-directory)))
      ;; `emacs-repository-get-branch' has been introduced with Emacs 27.1.
      (with-no-warnings
	(and (stringp dir) (file-directory-p dir)
	     (emacs-repository-get-branch dir)))))
  "The repository branch of the Tramp sources.")

(defconst tramp-repository-version
  (ignore-errors
    ;; Suppress message from `emacs-repository-get-version'.  We must
    ;; also handle out-of-tree builds.
    (let ((inhibit-message t)
	  (debug-on-error nil)
	  (dir (or (locate-dominating-file (locate-library "tramp") ".git")
		   source-directory)))
      (and (stringp dir) (file-directory-p dir)
	   (emacs-repository-get-version dir))))
  "The repository revision of the Tramp sources.")

;; Check for Emacs version.
(let ((x   (if (not (string-lessp emacs-version "25.1"))
      "ok"
    (format "Tramp 2.5.2.28.1 is not fit for %s"
            (replace-regexp-in-string "\n" "" (emacs-version))))))
  (unless (string-equal "ok" x) (error "%s" x)))

(defun tramp-inside-emacs ()
  "Version string provided by INSIDE_EMACS enmvironment variable."
  (concat (or (getenv "INSIDE_EMACS") emacs-version)
	  ",tramp:" tramp-version))

;; Tramp versions integrated into Emacs.  If a user option declares a
;; `:package-version' which doesn't belong to an integrated Tramp
;; version, it must be added here as well (see `tramp-syntax', for
;; example).  This can be checked by something like
;; (customize-changed "26.1")
(add-to-list
 'customize-package-emacs-version-alist
 '(Tramp ("2.0.55" . "22.1") ("2.0.57" . "22.2") ("2.0.58-pre" . "22.3")
	 ("2.1.15" . "23.1") ("2.1.18-23.2" . "23.2")
	 ("2.1.20" . "23.3") ("2.1.21-pre" . "23.4")
	 ("2.2.3-24.1" . "24.1") ("2.2.3-24.1" . "24.2") ("2.2.6-24.3" . "24.3")
	 ("2.2.9-24.4" . "24.4") ("2.2.11-24.5" . "24.5")
	 ("2.2.13.25.1" . "25.1") ("2.2.13.25.2" . "25.2")
	 ("2.2.13.25.2" . "25.3")
         ("2.3.3" . "26.1") ("2.3.3.26.1" . "26.1") ("2.3.5.26.2" . "26.2")
         ("2.3.5.26.3" . "26.3")
         ("2.4.3.27.1" . "27.1") ("2.4.5.27.2" . "27.2")
         ("2.5.2.28.1" . "28.1")))

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'trampver 'force)))

(provide 'trampver)

;;; trampver.el ends here
