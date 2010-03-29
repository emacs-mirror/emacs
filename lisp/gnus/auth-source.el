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
(autoload 'secrets-search-items "secrets")
(autoload 'secrets-get-alias "secrets")
(autoload 'secrets-get-attribute "secrets")
(autoload 'secrets-get-secret "secrets")

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
		               (list :tag "secrets.el (Secret Service API/KWallet/GNOME KeyRing)" 
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

(defun auth-source-pick (&rest spec)
  "Parse `auth-sources' for matches of the SPEC plist.

Common keys are :host, :protocol, and :user.  A value of t in
SPEC means to always succeed in the match.  A string value is
matched as a regex.

The first pass skips fallback choices.  If no choices are found
on the first pass, a second pass is made including the fallback
choices.

For string (filename) sources, fallback choices are those where
PROTOCOL or HOST are nil.

For secrets.el collections, the :host and :protocol keys are not
checked for fallback choices."
  (let (choices)
    (dolist (fallback '(nil t))
      (let ((keys (loop for i below (length spec) by 2
			collect (nth i spec)))
	    (default-session-fallback "login"))
	(dolist (choice auth-sources)
	  (let* ((s (plist-get choice :source))
		 ;; this is only set for Secret Service API specs (see secrets.el)
		 (coll (and (consp s) (plist-get s :secrets)))
		 (score 0))
	    (cond
	     (coll				; use secrets.el here
	      (when (eq coll 'default)
		(setq coll (secrets-get-alias "default"))
		(unless coll 
		  (auth-source-do-debug
		   "No 'default' alias.  Trying collection '%s'."
		   default-session-fallback)
		  (setq coll default-session-fallback)))
	      (let* ((coll-search (cond
				   ((stringp coll) coll)
				   
				   ;; when the collection is nil:
				   ;; in fallback mode, accept it as any
				   ;; otherwise, hope to fail
				   ((null coll) (if fallback
						    nil
						  " *fallback-fail*"))))
		     ;; assemble a search query for secrets-search-items
		     ;; in fallback mode, host and protocol are not checked
		     (other-search (loop for k
					 in (if fallback
						(remove :host 
							(remove :protocol keys))
					      keys)
					 append (list
						 k
						 ;; convert symbols to a string
						 (let ((v (plist-get spec k)))
						   (if (stringp v)
						       v
						     (prin1-to-string v))))))
		     ;; the score is based on how exact the search was, 
		     ;; plus base score = 1 for any match
		     (score (1+ (length other-search)))
		     (results (apply 'secrets-search-items
				     coll-search
				     other-search)))
		(auth-source-do-debug
		 "auth-source-pick: got items %s in collection '%s' + %s"
		 results coll-search other-search)
		;; put the results in the choices variable
		(dolist (result results)
		  (setq choices (cons (list score
					    `(:source secrets
						      :item ,result
						      :collection ,coll
						      :search ,coll-search
						      ,@other-search))
				      choices)))))
	     ;; this is any non-secrets spec (currently means a string filename)
	     (t
	      (let ((match t))
		(dolist (k keys)
		  (let* ((v (plist-get spec k))
			 (choicev (plist-get choice k)))
		    (setq match
			  (and match
			       (or (eq t choicev) ; source always matches spec key
				   ;; source key gives regex to match against spec
				   (and (stringp choicev) (string-match choicev v))
				   ;; source key gives symbol to match against spec
				   (and (symbolp choicev) (eq choicev v))
				   ;; in fallback mode, missing source key is OK
				   fallback)))
		    (when match (incf score)))) ; increment the score for each match

		;; now if the whole iteration resulted in a match:
		(when match
		  (setq choices (cons (list score choice) choices))))))))
	;; when there were matches, skip the second pass
	(when choices (return choices))))

      ;; return the results sorted by score
      (mapcar 'cadr (sort choices (lambda (x y) (> (car x) (car y)))))))

(defun auth-source-forget-user-or-password (mode host protocol)
  (interactive "slogin/password: \nsHost: \nsProtocol: \n") ;for testing
  (remhash (format "%s %s:%s" mode host protocol) auth-source-cache))

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

(defun auth-source-user-or-password (mode host protocol &optional username)
  "Find MODE (string or list of strings) matching HOST and PROTOCOL.

USERNAME is optional and will be used as \"login\" in a search
across the Secret Service API (see secrets.el) if the resulting
items don't have a username.  This means that if you search for
username \"joe\" and it matches an item but the item doesn't have
a :user attribute, the username \"joe\" will be returned.

MODE can be \"login\" or \"password\" for example."
  (auth-source-do-debug
   "auth-source-user-or-password: get %s for %s (%s) + user=%s"
   mode host protocol username)
  (let* ((listy (listp mode))
	 (mode (if listy mode (list mode)))
	 (extras (when username `(:user ,username)))
	 (cname (format "%s %s:%s %s" mode host protocol extras))
	 (search (list :host host :protocol protocol))
	 (search (if username (append search (list :user username)) search))
	 (found (gethash cname auth-source-cache)))
    (if found
	(progn
	  (auth-source-do-debug
	   "auth-source-user-or-password: cached %s=%s for %s (%s) + %s"
	   mode
	   ;; don't show the password
	   (if (and (member "password" mode) auth-source-hide-passwords)
	       "SECRET"
	     found)
	   host protocol extras)
	  found)			; return the found data
      ;; else, if not found
      (dolist (choice (apply 'auth-source-pick search))
	(setq found (cond
		     ;; the secrets.el spec
		     ((eq (plist-get choice :source) 'secrets)
		      (let ((coll (plist-get choice :search))
			    (item (plist-get choice :item)))
			(mapcar (lambda (m)
				  (if (equal "password" m)
				      (secrets-get-secret coll item)
				    ;; the user name is either
				    (or
				     ;; the secret's attribute :user, or
				     (secrets-get-attribute coll item :user)
				     ;; the originally requested :user
				     username
				     "unknown-user")))
				mode)))
		     (t		; anything else is netrc
		      (netrc-machine-user-or-password
		       mode
		       (plist-get choice :source)
		       (list host)
		       (list (format "%s" protocol))
		       (auth-source-protocol-defaults protocol)))))
	(when found
	  (auth-source-do-debug
	   "auth-source-user-or-password: found %s=%s for %s (%s) + %s"
	   mode
	   ;; don't show the password
	   (if (and (member "password" mode) auth-source-hide-passwords) "SECRET" found)
	   host protocol extras)
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
