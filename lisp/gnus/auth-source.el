;;; auth-source.el --- authentication sources for Gnus and Emacs

;; Copyright (C) 2008, 2009, 2010, 2011, 2012 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Keywords: news

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

;; This is the auth-source.el package.  It lets users tell Gnus how to
;; authenticate in a single place.  Simplicity is the goal.  Instead
;; of providing 5000 options, we'll stick to simple, easy to
;; understand options.

;; See the auth.info Info documentation for details.

;;; Code:

(require 'gnus-util)

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

(defvar auth-source-cache (make-hash-table :test 'equal)
  "Cache for auth-source data")

(defcustom auth-source-do-cache t
  "Whether auth-source should cache information."
  :group 'auth-source
  :version "23.1" ;; No Gnus
  :type `boolean)

(defcustom auth-source-debug nil
  "Whether auth-source should log debug messages.
Also see `auth-source-hide-passwords'.

If the value is nil, debug messages are not logged.
If the value is t, debug messages are logged with `message'.
 In that case, your authentication data will be in the
 clear (except for passwords, which are always stripped out).
If the value is a function, debug messages are logged by calling
 that function using the same arguments as `message'."
  :group 'auth-source
  :version "23.1" ;; No Gnus
  :type	`(choice 
	  :tag "auth-source debugging mode"
	  (const :tag "Log using `message' to the *Messages* buffer" t)
	  (function :tag "Function that takes arguments like `message'")
	  (const :tag "Don't log anything" nil)))

(defcustom auth-source-hide-passwords t
  "Whether auth-source should hide passwords in log messages.
Only relevant if `auth-source-debug' is not nil."
  :group 'auth-source
  :version "23.1" ;; No Gnus
  :type `boolean)

(defcustom auth-sources '((:source "~/.authinfo.gpg" :host t :protocol t))
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

;; (let ((auth-source-debug 'debug)) (auth-source-debug "hello"))
;; (let ((auth-source-debug t)) (auth-source-debug "hello"))
;; (let ((auth-source-debug nil)) (auth-source-debug "hello"))
(defun auth-source-do-debug (&rest msg)
  ;; set logger to either the function in auth-source-debug or 'message
  ;; note that it will be 'message if auth-source-debug is nil, so
  ;; we also check the value
  (when auth-source-debug
    (let ((logger (if (functionp auth-source-debug)
		      auth-source-debug 
		    'message)))
      (apply logger msg))))

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

(defun auth-source-forget-user-or-password (mode host protocol)
  (interactive "slogin/password: \nsHost: \nsProtocol: \n") ;for testing
  (remhash (format "%s %s:%s" mode host protocol) auth-source-cache))

(defun auth-source-forget-all-cached ()
  "Forget all cached auth-source authentication tokens."
  (interactive)
  (setq auth-source-cache (make-hash-table :test 'equal)))

(defun auth-source-user-or-password (mode host protocol)
  "Find MODE (string or list of strings) matching HOST and PROTOCOL.
MODE can be \"login\" or \"password\" for example."
  (auth-source-do-debug
   "auth-source-user-or-password: get %s for %s (%s)"
   mode host protocol)
  (let* ((listy (listp mode))
	 (mode (if listy mode (list mode)))
	 (cname (format "%s %s:%s" mode host protocol))
	 (found (gethash cname auth-source-cache)))
    (if found
	(progn
	  (auth-source-do-debug
	   "auth-source-user-or-password: cached %s=%s for %s (%s)"
	   mode
	   ;; don't show the password
	   (if (and (member "password" mode) auth-source-hide-passwords) "SECRET" found)
	   host protocol)
	  found)
      (dolist (choice (auth-source-pick host protocol))
	(setq found (netrc-machine-user-or-password
		     mode
		     (plist-get choice :source)
		     (list host)
		     (list (format "%s" protocol))
		     (auth-source-protocol-defaults protocol)))
	(when found
	  (auth-source-do-debug
	   "auth-source-user-or-password: found %s=%s for %s (%s)"
	   mode
	   ;; don't show the password
	   (if (and (member "password" mode) auth-source-hide-passwords) "SECRET" found)
	   host protocol)
	  (setq found (if listy found (car-safe found)))
	  (when auth-source-do-cache
	    (puthash cname found auth-source-cache)))
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
