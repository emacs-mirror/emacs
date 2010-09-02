;;; auth-source.el --- authentication sources for Gnus and Emacs

;; Copyright (C) 2008, 2009, 2010 Free Software Foundation, Inc.

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
(autoload 'netrc-machine-user-or-password "netrc")
(autoload 'secrets-create-item "secrets")
(autoload 'secrets-delete-item "secrets")
(autoload 'secrets-get-alias "secrets")
(autoload 'secrets-get-attribute "secrets")
(autoload 'secrets-get-secret "secrets")
(autoload 'secrets-list-collections "secrets")
(autoload 'secrets-search-items "secrets")

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
  :version "23.2" ;; No Gnus
  :type '(repeat :tag "Authentication Protocols"
		 (cons :tag "Protocol Entry"
		       (symbol :tag "Protocol")
		       (repeat :tag "Names"
			       (string :tag "Name")))))

;;; generate all the protocols in a format Customize can use
;;; TODO: generate on the fly from auth-source-protocols
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
  :version "23.2" ;; No Gnus
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
  :version "23.2" ;; No Gnus
  :type	`(choice
	  :tag "auth-source debugging mode"
	  (const :tag "Log using `message' to the *Messages* buffer" t)
	  (function :tag "Function that takes arguments like `message'")
	  (const :tag "Don't log anything" nil)))

(defcustom auth-source-hide-passwords t
  "Whether auth-source should hide passwords in log messages.
Only relevant if `auth-source-debug' is not nil."
  :group 'auth-source
  :version "23.2" ;; No Gnus
  :type `boolean)

(defcustom auth-sources '((:source "~/.authinfo.gpg"))
  "List of authentication sources.

The default will get login and password information from a .gpg
file, which you should set up with the EPA/EPG packages to be
encrypted.  See the auth.info manual for details.

Each entry is the authentication type with optional properties.

It's best to customize this with `M-x customize-variable' because the choices
can get pretty complex."
  :group 'auth-source
  :version "23.2" ;; No Gnus
  :type `(repeat :tag "Authentication Sources"
		 (list :tag "Source definition"
		       (const :format "" :value :source)
		       (choice :tag "Authentication backend choice"
		               (string :tag "Authentication Source (file)")
		               (list :tag "secrets.el (Secret Service API/KWallet/GNOME Keyring)"
                                     (const :format "" :value :secrets)
                                     (choice :tag "Collection to use"
                                             (string :tag "Collection name")
                                             (const :tag "Default" 'default)
                                             (const :tag "Login" "login")
                                             (const :tag "Temporary" "session"))))
		       (repeat :tag "Extra Parameters" :inline t
			       (choice :tag "Extra parameter"
				       (list :tag "Host (omit to match as a fallback)"
					     (const :format "" :value :host)
					     (choice :tag "Host (machine) choice"
						     (const :tag "Any" t)
						     (regexp :tag "Host (machine) regular expression")))
				       (list :tag "Protocol (omit to match as a fallback)"
					     (const :format "" :value :protocol)
					     (choice :tag "Protocol"
						     (const :tag "Any" t)
						     ,@auth-source-protocols-customize))
				       (list :tag "User  (omit to match as a fallback)" :inline t
					     (const :format "" :value :user)
					     (choice :tag "Personality or username"
						     (const :tag "Any" t)
						     (string :tag "Specific user name"))))))))

;; temp for debugging
;; (unintern 'auth-source-protocols)
;; (unintern 'auth-sources)
;; (customize-variable 'auth-sources)
;; (setq auth-sources nil)
;; (format "%S" auth-sources)
;; (customize-variable 'auth-source-protocols)
;; (setq auth-source-protocols nil)
;; (format "%S" auth-source-protocols)
;; (auth-source-pick nil :host "a" :port 'imap)
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

;; (auth-source-pick nil :host "any" :protocol 'imap :user "joe")
;; (auth-source-pick t :host "any" :protocol 'imap :user "joe")
;; (setq auth-sources '((:source (:secrets default) :host t :protocol t :user "joe")
;; 		     (:source (:secrets "session") :host t :protocol t :user "joe")
;; 		     (:source (:secrets "login") :host t :protocol t)
;; 		     (:source "~/.authinfo.gpg" :host t :protocol t)))

;; (setq auth-sources '((:source (:secrets default) :host t :protocol t :user "joe")
;; 		     (:source (:secrets "session") :host t :protocol t :user "joe")
;; 		     (:source (:secrets "login") :host t :protocol t)
;; 		     ))

;; (setq auth-sources '((:source "~/.authinfo.gpg" :host t :protocol t)))

(defun auth-get-source (entry)
  "Return the source string of ENTRY, which is one entry in `auth-sources'.
If it is a Secret Service API, return the collection name, otherwise
the file name."
  (let ((source (plist-get entry :source)))
    (if (stringp source)
	source
      ;; Secret Service API.
      (setq source (plist-get source :secrets))
      (when (eq source 'default)
	(setq source (or (secrets-get-alias "default") "login")))
      (or source "session"))))

(defun auth-source-pick (&rest spec)
  "Parse `auth-sources' for matches of the SPEC plist.

Common keys are :host, :protocol, and :user.  A value of t in
SPEC means to always succeed in the match.  A string value is
matched as a regex."
  (let ((keys (loop for i below (length spec) by 2 collect (nth i spec)))
	choices)
    (dolist (choice (copy-tree auth-sources) choices)
      (let ((source (plist-get choice :source))
	    (match t))
	(when
	    (and
	     ;; Check existence of source.
	     (if (consp source)
		 ;; Secret Service API.
		 (member (auth-get-source choice) (secrets-list-collections))
	       ;; authinfo file.
	       (file-exists-p source))

	     ;; Check keywords.
	     (dolist (k keys match)
	       (let* ((v (plist-get spec k))
		      (choicev (if (plist-member choice k)
				   (plist-get choice k) t)))
		 (setq match
		       (and match
			    (or
			     ;; source always matches spec key
			     (eq t choicev)
			     ;; source key gives regex to match against spec
			     (and (stringp choicev) (string-match choicev v))
			     ;; source key gives symbol to match against spec
			     (and (symbolp choicev) (eq choicev v))))))))

	  (add-to-list 'choices choice 'append))))))

(defun auth-source-retrieve (mode entry &rest spec)
  "Retrieve MODE credentials according to SPEC from ENTRY."
  (catch 'no-password
    (let ((host (plist-get spec :host))
	  (user (plist-get spec :user))
	  (prot (plist-get spec :protocol))
	  (source (plist-get entry :source))
	  result)
      (cond
       ;; Secret Service API.
       ((consp source)
	(let ((coll (auth-get-source entry))
	      item)
	  ;; Loop over candidates with a matching host attribute.
	  (dolist (elt (secrets-search-items coll :host host) item)
	    (when (and (or (not user)
			   (string-equal
			    user (secrets-get-attribute coll elt :user)))
		       (or (not prot)
			   (string-equal
			    prot (secrets-get-attribute coll elt :protocol))))
	      (setq item elt)
	      (return elt)))
	  ;; Compose result.
	  (when item
	    (setq result
		  (mapcar (lambda (m)
			    (if (string-equal "password" m)
				(or (secrets-get-secret coll item)
				    ;; When we do not find a password,
				    ;; we return nil anyway.
				    (throw 'no-password nil))
			      (or (secrets-get-attribute coll item :user)
				  user)))
			  (if (consp mode) mode (list mode)))))
	  (if (consp mode) result (car result))))
       ;; Anything else is netrc.
       (t
	(let ((search (list source (list host) (list (format "%s" prot))
			    (auth-source-protocol-defaults prot))))
	  (setq result
		(mapcar (lambda (m)
			  (if (string-equal "password" m)
			      (or (apply
				   'netrc-machine-user-or-password m search)
				  ;; When we do not find a password, we
				  ;; return nil anyway.
				  (throw 'no-password nil))
			    (or (apply
				 'netrc-machine-user-or-password m search)
				user)))
			(if (consp mode) mode (list mode)))))
	(if (consp mode) result (car result)))))))

(defun auth-source-create (mode entry &rest spec)
  "Create interactively credentials according to SPEC in ENTRY.
Return structure as specified by MODE."
  (let* ((host (plist-get spec :host))
	 (user (plist-get spec :user))
	 (prot (plist-get spec :protocol))
	 (source (plist-get entry :source))
	 (name (concat (if user (format "%s@" user))
		       host
		       (if prot (format ":%s" prot))))
	 result)
    (setq result
	  (mapcar
	   (lambda (m)
	     (if (equal "password" m)
		 (let ((passwd (read-passwd "Password: ")))
		   (cond
		    ;; Secret Service API.
		    ((consp source)
		     (apply
		      'secrets-create-item
		      (auth-get-source entry) name passwd spec))
		    (t)) ;; netrc not implemented yes.
		   passwd)
	       (or
		;; the originally requested :user
		user
		"unknown-user")))
	   (if (consp mode) mode (list mode))))
    (if (consp mode) result (car result))))

(defun auth-source-delete (entry &rest spec)
  "Delete credentials according to SPEC in ENTRY."
  (let ((host (plist-get spec :host))
	(user (plist-get spec :user))
	(prot (plist-get spec :protocol))
	(source (plist-get entry :source)))
    (cond
     ;; Secret Service API.
     ((consp source)
      (let ((coll (auth-get-source entry)))
	;; Loop over candidates with a matching host attribute.
	(dolist (elt (secrets-search-items coll :host host))
	  (when (and (or (not user)
			 (string-equal
			  user (secrets-get-attribute coll elt :user)))
		     (or (not prot)
			 (string-equal
			  prot (secrets-get-attribute coll elt :protocol))))
	    (secrets-delete-item coll elt)))))
     (t)))) ;; netrc not implemented yes.

(defun auth-source-forget-user-or-password
  (mode host protocol &optional username)
  "Remove cached authentication token."
  (interactive "slogin/password: \nsHost: \nsProtocol: \n") ;for testing
  (remhash
   (if username
       (format "%s %s:%s %s" mode host protocol username)
     (format "%s %s:%s" mode host protocol))
   auth-source-cache))

(defun auth-source-forget-all-cached ()
  "Forget all cached auth-source authentication tokens."
  (interactive)
  (setq auth-source-cache (make-hash-table :test 'equal)))

;; (progn
;;   (auth-source-forget-all-cached)
;;   (list
;;    (auth-source-user-or-password '("login" "password") "imap.myhost.com" "other")
;;    (auth-source-user-or-password '("login" "password") "imap.myhost.com" "other" "tzz")
;;    (auth-source-user-or-password '("login" "password") "imap.myhost.com" "other" "joe")))

(defun auth-source-user-or-password
  (mode host protocol &optional username create-missing delete-existing)
  "Find MODE (string or list of strings) matching HOST and PROTOCOL.

USERNAME is optional and will be used as \"login\" in a search
across the Secret Service API (see secrets.el) if the resulting
items don't have a username.  This means that if you search for
username \"joe\" and it matches an item but the item doesn't have
a :user attribute, the username \"joe\" will be returned.

A non nil DELETE-EXISTING means deleting any matching password
entry in the respective sources.  This is useful only when
CREATE-MISSING is non nil as well; the intended use case is to
remove wrong password entries.

If no matching entry is found, and CREATE-MISSING is non nil,
the password will be retrieved interactively, and it will be
stored in the password database which matches best (see
`auth-sources').

MODE can be \"login\" or \"password\"."
  (auth-source-do-debug
   "auth-source-user-or-password: get %s for %s (%s) + user=%s"
   mode host protocol username)
  (let* ((listy (listp mode))
	 (mode (if listy mode (list mode)))
	 (cname (if username
		    (format "%s %s:%s %s" mode host protocol username)
		  (format "%s %s:%s" mode host protocol)))
	 (search (list :host host :protocol protocol))
	 (search (if username (append search (list :user username)) search))
	 (found (if (not delete-existing)
		    (gethash cname auth-source-cache)
		  (remhash cname auth-source-cache)
		  nil)))
    (if found
	(progn
	  (auth-source-do-debug
	   "auth-source-user-or-password: cached %s=%s for %s (%s) + %s"
	   mode
	   ;; don't show the password
	   (if (and (member "password" mode) auth-source-hide-passwords)
	       "SECRET"
	     found)
	   host protocol username)
	  found)			; return the found data
      ;; else, if not found
      (let ((choices (apply 'auth-source-pick search)))
	(dolist (choice choices)
	  (if delete-existing
	      (apply 'auth-source-delete choice search)
	    (setq found (apply 'auth-source-retrieve mode choice search)))
	  (and found (return found)))

	;; We haven't found something, so we will create it interactively.
	(when (and (not found) choices create-missing)
	  (setq found (apply 'auth-source-create mode (car choices) search)))

	;; Cache the result.
	(when found
	  (auth-source-do-debug
	   "auth-source-user-or-password: found %s=%s for %s (%s) + %s"
	   mode
	   ;; don't show the password
	   (if (and (member "password" mode) auth-source-hide-passwords)
	       "SECRET" found)
	   host protocol username)
	  (setq found (if listy found (car-safe found)))
	  (when auth-source-do-cache
	    (puthash cname found auth-source-cache)))

	found))))

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

;;; auth-source.el ends here
