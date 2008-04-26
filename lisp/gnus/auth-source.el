;;; auth-source.el --- authentication sources for Gnus and Emacs

;; Copyright (C) 2008 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Keywords: news

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This is the auth-source.el package.  It lets users tell Gnus how to
;; authenticate in a single place.  Simplicity is the goal.  Instead
;; of providing 5000 options, we'll stick to simple, easy to
;; understand options.

;; Easy setup:
;; (require 'auth-source)
;; (customize-variable 'auth-sources) ;; optional

;; now, whatever sources you've defined for password have to be available

;; if you want encrypted sources, which is strongly recommended, do
;; (require 'epa-file)
;; (epa-file-mode)

;; before you put some data in ~/.authinfo.gpg (the default place)

;;; Code:

(eval-when-compile (require 'cl))
(eval-when-compile (require 'netrc))

(defgroup auth-source nil
  "Authentication sources."
  :version "23.1" ;; No Gnus
  :group 'gnus)

(defcustom auth-source-protocols '((imap "imap" "imaps" "143" "993")
				   (pop3 "pop3" "pop" "pop3s" "110" "995")
				   (ssh  "ssh" "22")
				   (sftp "sftp" "115")
				   (smtp "smtp" "25"))
  "List of authentication protocols and their names"

  :group 'auth-source
  :version "23.1" ;; No Gnus
  :type '(repeat :tag "Authentication Protocols"
		 (cons :tag "Protocol Entry"
		       (symbol :tag "Protocol")
		       (repeat :tag "Names"
			       (string :tag "Name")))))

;;; generate all the protocols in a format Customize can use
(defconst auth-source-protocols-customize
  (mapcar (lambda (a)
	    (let ((p (car-safe a)))
	      (list 'const 
		    :tag (upcase (symbol-name p))
		    p)))
	  auth-source-protocols))

;;; this default will be changed to ~/.authinfo.gpg
(defcustom auth-sources '((:source "~/.authinfo.enc" :host t :protocol t))
  "List of authentication sources.

Each entry is the authentication type with optional properties."
  :group 'auth-source
  :version "23.1" ;; No Gnus
  :type `(repeat :tag "Authentication Sources"
		 (list :tag "Source definition"
		       (const :format "" :value :source)
		       (string :tag "Authentication Source")
		       (const :format "" :value :host)
		       (choice :tag "Host (machine) choice"
			       (const :tag "Any" t)
			       (regexp :tag "Host (machine) regular expression (TODO)")
			       (const :tag "Fallback" nil))
		       (const :format "" :value :protocol)
		       (choice :tag "Protocol"
			       (const :tag "Any" t)
			       (const :tag "Fallback" nil)
			       ,@auth-source-protocols-customize))))

;; temp for debugging
;; (unintern 'auth-source-protocols)
;; (unintern 'auth-sources)
;; (customize-variable 'auth-sources)
;; (setq auth-sources nil)
;; (format "%S" auth-sources)
;; (customize-variable 'auth-source-protocols)
;; (setq auth-source-protocols nil)
;; (format "%S" auth-source-protocols)
;; (auth-source-pick "a" 'imap)
;; (auth-source-user-or-password "login" "imap.myhost.com" 'imap)
;; (auth-source-user-or-password "password" "imap.myhost.com" 'imap)
;; (auth-source-user-or-password-imap "login" "imap.myhost.com")
;; (auth-source-user-or-password-imap "password" "imap.myhost.com")
;; (auth-source-protocol-defaults 'imap)

(defun auth-source-pick (host protocol &optional fallback)
  "Parse `auth-sources' for HOST, and PROTOCOL matches.

Returns fallback choices (where PROTOCOL or HOST are nil) with FALLBACK t."
  (interactive "sHost: \nsProtocol: \n") ;for testing
  (let (choices)
    (dolist (choice auth-sources)
      (let ((h (plist-get choice :host))
	    (p (plist-get choice :protocol)))
	(when (and
	       (or (equal t h)
		   (and (stringp h) (string-match h host))
		   (and fallback (equal h nil)))
	       (or (equal t p)
		   (and (symbolp p) (equal p protocol))
		   (and fallback (equal p nil))))
	  (push choice choices))))
    (if choices
	choices
      (unless fallback
	(auth-source-pick host protocol t)))))

(defun auth-source-user-or-password (mode host protocol)
  "Find user or password (from the string MODE) matching HOST and PROTOCOL."
  (let (found)
    (dolist (choice (auth-source-pick host protocol))
      (setq found (netrc-machine-user-or-password 
		   mode
		   (plist-get choice :source)
		   (list host)
		   (list (format "%s" protocol))
		   (auth-source-protocol-defaults protocol)))
      (when found
	(return found)))))

(defun auth-source-protocol-defaults (protocol)
  "Return a list of default ports and names for PROTOCOL."
  (cdr-safe (assoc protocol auth-source-protocols)))

(defun auth-source-user-or-password-imap (mode host)
  (auth-source-user-or-password mode host 'imap))

(defun auth-source-user-or-password-pop3 (mode host)
  (auth-source-user-or-password mode host 'pop3))

(defun auth-source-user-or-password-ssh (mode host)
  (auth-source-user-or-password mode host 'ssh))

(defun auth-source-user-or-password-sftp (mode host)
  (auth-source-user-or-password mode host 'sftp))

(defun auth-source-user-or-password-smtp (mode host)
  (auth-source-user-or-password mode host 'smtp))

(provide 'auth-source)

;; arch-tag: ff1afe78-06e9-42c2-b693-e9f922cbe4ab
;;; auth-source.el ends here
