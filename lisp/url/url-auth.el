;;; url-auth.el --- Uniform Resource Locator authorization modules -*- lexical-binding: t -*-

;; Copyright (C) 1996-1999, 2004-2020 Free Software Foundation, Inc.

;; Keywords: comm, data, processes, hypermedia

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

(require 'url-vars)
(require 'url-parse)
(autoload 'url-warn "url")
(autoload 'auth-source-search "auth-source")

(defsubst url-auth-user-prompt (url realm)
  "String to usefully prompt for a username."
  (concat "Username [for "
	  (or realm (url-truncate-url-for-viewing
		     (url-recreate-url url)
		     (- (window-width) 10 20)))
	  "]: "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic authorization code
;;; ------------------------
;;; This implements the BASIC authorization type.  See the online
;;; documentation at
;;; http://www.w3.org/hypertext/WWW/AccessAuthorization/Basic.html
;;; for the complete documentation on this type.
;;;
;;; This is very insecure, but it works as a proof-of-concept
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar url-basic-auth-storage 'url-http-real-basic-auth-storage
  "Where usernames and passwords are stored.

Must be a symbol pointing to another variable that will actually store
the information.  The value of this variable is an assoc list of assoc
lists.  The first assoc list is keyed by the server name.  The cdr of
this is an assoc list based on the \"directory\" specified by the URL we
are looking up.")

(defun url-basic-auth (url &optional prompt overwrite realm _args)
  "Get the username/password for the specified URL.
If optional argument PROMPT is non-nil, ask for the username/password
to use for the url and its descendants.  If optional third argument
OVERWRITE is non-nil, overwrite the old username/password pair if it
is found in the assoc list.  If REALM is specified, use that as the realm
instead of the filename inheritance method."
  (let* ((href (if (stringp url)
		   (url-generic-parse-url url)
		 url))
	 (server (url-host href))
	 (type (url-type href))
	 (port (url-port href))
	 (file (url-filename href))
	 (user (url-user href))
	 (pass (url-password href))
	 (enable-recursive-minibuffers t) ; for url-handler-mode (bug#10298)
	 byserv retval data)
    (setq server (format "%s:%d" server port)
	  file (cond
		(realm realm)
		((string= "" file) "/")
		((string-match "/$" file) file)
		(t (url-file-directory file)))
	  byserv (cdr-safe (assoc server
				  (symbol-value url-basic-auth-storage))))
    (cond
     ((and user pass)
      ;; Explicit http://user:pass@foo/ URL.  Just return the credentials.
      (setq retval (base64-encode-string (format "%s:%s" user pass) t)))
     ((and prompt (not byserv))
      (setq user (or
		  (url-do-auth-source-search server type :user)
		  (read-string (url-auth-user-prompt href realm)
			       (or user (user-real-login-name))))
	    pass (or
		  (url-do-auth-source-search server type :secret)
		  (read-passwd "Password: " nil (or pass ""))))
      (set url-basic-auth-storage
	   (cons (list server
		       (cons file
			     (setq retval
				   (base64-encode-string
				    (format "%s:%s" user
					    (encode-coding-string pass 'utf-8))
                                    t))))
		 (symbol-value url-basic-auth-storage))))
     (byserv
      (setq retval (cdr-safe (assoc file byserv)))
      (if (and (not retval)
	       (string-match "/" file))
 	  (while (and byserv (not retval))
	    (setq data (car (car byserv)))
	    (if (or (not (string-match "/" data)) ; It's a realm - take it!
		    (and
		     (>= (length file) (length data))
		     (string= data (substring file 0 (length data)))))
		(setq retval (cdr (car byserv))))
	    (setq byserv (cdr byserv))))
      (if (or (and (not retval) prompt) overwrite)
	  (progn
	    (setq user (or
			(url-do-auth-source-search server type :user)
			(read-string (url-auth-user-prompt href realm)
				     (user-real-login-name)))
		  pass (or
			(url-do-auth-source-search server type :secret)
			(read-passwd "Password: "))
		  retval (base64-encode-string (format "%s:%s" user pass) t)
		  byserv (assoc server (symbol-value url-basic-auth-storage)))
	    (setcdr byserv
		    (cons (cons file retval) (cdr byserv))))))
     (t (setq retval nil)))
    (if retval (setq retval (concat "Basic " retval)))
    retval))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Digest authorization code
;;; ------------------------
;;; This implements the DIGEST authorization type.  See RFC 2617
;;; https://www.ietf.org/rfc/rfc2617.txt
;;; for the complete documentation on this type.
;;;
;;; This is very secure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar url-digest-auth-storage nil
  "Where usernames and passwords are stored.
Its value is an assoc list of assoc lists.  The first assoc list is
keyed by the server name.  The cdr of this is an assoc list based
on the \"directory\" specified by the url we are looking up.")

(defsubst url-digest-auth-colonjoin (&rest args)
  "Concatenate ARGS as strings with colon as a separator."
  (mapconcat 'identity args ":"))

(defsubst url-digest-auth-kd (data secret)
  "Apply digest algorithm to DATA using SECRET and return the result."
  (md5 (url-digest-auth-colonjoin secret data)))

(defsubst url-digest-auth-make-ha1 (user realm password)
  "Compute checksum out of strings USER, REALM, and PASSWORD."
  (md5 (url-digest-auth-colonjoin user realm password)))

(defsubst url-digest-auth-make-ha2 (method digest-uri)
  "Compute checksum out of strings METHOD and DIGEST-URI."
  (md5 (url-digest-auth-colonjoin method digest-uri)))

(defsubst url-digest-auth-make-request-digest (ha1 ha2 nonce)
  "Construct the request-digest from hash strings HA1, HA2, and NONCE.
This is the value that server receives as a proof that user knows
a password."
  (url-digest-auth-kd (url-digest-auth-colonjoin nonce ha2) ha1))

(defsubst url-digest-auth-make-request-digest-qop (qop ha1 ha2 nonce nc cnonce)
  "Construct the request-digest with qop.
QOP describes the \"quality of protection\" and algorithm to use.
All of the strings QOP, HA1, HA2, NONCE, NC, and CNONCE are
combined into a single hash value that proves to a server the
user knows a password.  It's worth noting that HA2 already
depends on value of QOP."
  (url-digest-auth-kd (url-digest-auth-colonjoin
                       nonce nc cnonce qop ha2) ha1))

(defsubst url-digest-auth-directory-id (url realm)
  "Make an identifier for selecting a key in key cache.
The identifier is made either from URL or REALM.  It represents a
protection space within a server so that one server can have
multiple authorizations."
  (or realm (or (url-file-directory (url-filename url)) "/")))

(defsubst url-digest-auth-server-id (url)
  "Make an identifier for selecting a server in key cache.
The identifier is made from URL's host and port.  Together with
`url-digest-auth-directory-id' these identify a single key in the
key cache `url-digest-auth-storage'."
  (format "%s:%d" (url-host url) (url-port url)))

(defun url-digest-auth-make-cnonce ()
  "Compute a new unique client nonce value."
  (base64-encode-string
   (format "%016x%016x" (random) (car (time-convert nil t)))
   t))

(defun url-digest-auth-nonce-count (_nonce)
  "The number requests sent to server with the given NONCE.
This count includes the request we're preparing here.

Currently, this is not implemented and will always return 1.

Value returned is in string format with leading zeroes, such as
\"00000001\"."
  (format "%08x" 1))

(defun url-digest-auth-name-value-string (pairs)
  "Concatenate name-value pairs in association list PAIRS.

Output is formatted as \"name1=\\\"value1\\\", name2=\\\"value2\\\", ...\""
  (mapconcat (lambda (pair)
               (format "%s=\"%s\""
                       (symbol-name (car pair))
                       (cdr pair)))
             pairs ", "))

(defun url-digest-auth-source-creds (url)
  "Find credentials for URL object from the Emacs auth-source.
Return value is a plist that has `:user' and `:secret' properties
if credentials were found.  Otherwise nil."
  (let ((server (url-digest-auth-server-id url))
        (type (url-type url)))
    (list :user (url-do-auth-source-search server type :user)
          :secret (url-do-auth-source-search server type :secret))))

(defun url-digest-prompt-creds (url realm &optional creds)
  "Prompt credentials for URL and REALM, defaulting to CREDS.
CREDS is a plist that may have properties `:user' and `:secret'."
  ;; Set explicitly in case creds were nil.  This makes the second
  ;; plist-put modify the same plist.
  (setq creds
        (plist-put creds :user
                   (read-string (url-auth-user-prompt url realm)
                                (or (plist-get creds :user)
                                    (user-real-login-name)))))
  (plist-put creds :secret
             (read-passwd "Password: " nil (plist-get creds :secret))))

(defun url-digest-auth-directory-id-assoc (dirkey keylist)
  "Find the best match for DIRKEY in key alist KEYLIST.

The string DIRKEY should be obtained using
`url-digest-auth-directory-id'.  The key list to search through
is the alist KEYLIST where car of each element may match DIRKEY.
If DIRKEY represents a realm, the list is searched only for an
exact match.  For directory names, an ancestor is sufficient for
a match."
  (or
   ;; Check exact match first.
   (assoc dirkey keylist)
   ;; No exact match found.  Continue to look for partial match if
   ;; dirkey is not a realm.
   (and (string-match "/" dirkey)
        (let (match)
          (while (and (null match) keylist)
            (if (or
                 ;; Any realm candidate matches.  Why?
                 (not (string-match "/" (caar keylist)))
                 ;; Parent directory matches.
                 (string-prefix-p (caar keylist) dirkey))
                (setq match (car keylist))
              (setq keylist (cdr keylist))))
          match))))

(defun url-digest-cached-key (url realm)
  "Find best match for URL and REALM from `url-digest-auth-storage'.
The return value is a list consisting of a realm (or a directory)
a user name, and hashed authentication tokens HA1 and HA2.
Modifying the contents of the returned list will modify the cache
variable `url-digest-auth-storage' itself."
  (url-digest-auth-directory-id-assoc
   (url-digest-auth-directory-id url realm)
   (cdr (assoc (url-digest-auth-server-id url) url-digest-auth-storage))))

(defun url-digest-cache-key (key url)
  "Add key to `url-digest-auth-storage'.
KEY has the same format as returned by `url-digest-cached-key'.
The key is added to cache hierarchy under server id, deduced from
URL."
  (let ((serverid (url-digest-auth-server-id url)))
    (push (list serverid key) url-digest-auth-storage)))

(defun url-digest-auth-create-key (username password realm method uri)
  "Create a key for digest authentication method.
The USERNAME and PASSWORD are the credentials for REALM and are
used in making a hashed value named HA1.  The HTTP METHOD and URI
makes a second hashed value HA2.  These hashes are used in making
the authentication key that can be stored without saving the
password in plain text.  The return value is a list (HA1 HA2).

For backward compatibility, URI is allowed to be a URL cl-struct
object."
  (and username password realm
       (list (url-digest-auth-make-ha1 username realm password)
             (url-digest-auth-make-ha2 method (cond ((stringp uri) uri)
                                                    (t (url-filename uri)))))))

(defun url-digest-auth-build-response (key url realm attrs)
  "Compute authorization string for the given challenge using KEY.

The string looks like 'Digest username=\"John\", realm=\"The
Realm\", ...'

Part of the challenge is already solved in a pre-computed KEY
which is list of a realm (or a directory), user name, and hash
tokens HA1 and HA2.

Some fields are filled as is from the given URL, REALM, and
using the contents of alist ATTRS.

ATTRS is expected to contain at least the server's \"nonce\"
value.  It also might contain the optional \"opaque\" value.
Newer implementations conforming to RFC 2617 should also contain
qop (Quality Of Protection) and related attributes.

Restrictions on Quality of Protection scheme: The qop value
\"auth-int\" or algorithm any other than \"MD5\" are not
implemented."

  (when key
    (let ((user (nth 1 key))
          (ha1 (nth 2 key))
          (ha2 (nth 3 key))
          (digest-uri (url-filename url))
          (qop (cdr-safe (assoc "qop" attrs)))
          (nonce (cdr-safe (assoc "nonce" attrs)))
          (opaque (cdr-safe (assoc "opaque" attrs))))

      (concat
       "Digest "
       (url-digest-auth-name-value-string
        (append (list (cons 'username user)
                      (cons 'realm realm)
                      (cons 'nonce nonce)
                      (cons 'uri digest-uri))

                (cond
                 ((null qop)
                  (list (cons 'response (url-digest-auth-make-request-digest
                                         ha1 ha2 nonce))))
                 ((string= qop "auth")
                  (let ((nc (url-digest-auth-nonce-count nonce))
                        (cnonce (url-digest-auth-make-cnonce)))
                    (list (cons 'qop qop)
                          (cons 'nc nc)
                          (cons 'cnonce cnonce)
                          (cons 'response
                                (url-digest-auth-make-request-digest-qop
                                 qop ha1 ha2 nonce nc cnonce)))))
                 (t (message "Quality of protection \"%s\" is not implemented." qop)
                    nil))


                (if opaque (list (cons 'opaque opaque)))))))))

(defun url-digest-find-creds (url prompt &optional realm)
  "Find or ask credentials for URL.

Primary method for finding credentials is from Emacs auth-source.
If password isn't found, and PROMPT is non-nil, query credentials
via minibuffer.  Optional REALM may be used when prompting as a
hint to the user.

Return value is nil in case either user name or password wasn't
found.  Otherwise, it's a plist containing `:user' and `:secret'.
Additional `:source' property denotes the origin of the
credentials and its value can be either symbol `authsource' or
`interactive'."
  (let ((creds (url-digest-auth-source-creds url)))

    ;; If credentials weren't found and prompting is allowed, prompt
    ;; the user.
    (if (and prompt
             (or (null creds)
                 (null (plist-get creds :secret))))
        (progn
          (setq creds (url-digest-prompt-creds url realm creds))
          (plist-put creds :source 'interactive))
      (plist-put creds :source 'authsource))

    (and (plist-get creds :user)
         (plist-get creds :secret)
         creds)))

(defun url-digest-find-new-key (url realm prompt)
  "Find credentials and create a new authorization key for given URL and REALM.

Return value is the new key, or nil if credentials weren't found.
\"New\" in this context means a key that's not yet found in cache
variable `url-digest-auth-storage'.  You may use `url-digest-cache-key'
to put it there.

This function uses `url-digest-find-creds' to find the
credentials.  It first looks in auth-source.  If not found, and
PROMPT is non-nil, user is asked for credentials interactively
via minibuffer."
  (let (creds)
    (unwind-protect
        (if (setq creds (url-digest-find-creds url prompt realm))
            (cons (url-digest-auth-directory-id url realm)
                  (cons (plist-get creds :user)
                        (url-digest-auth-create-key
                         (plist-get creds :user)
                         (plist-get creds :secret)
                         realm
                         (or url-request-method "GET")
                         (url-filename url)))))
      (if (and creds
               ;; Don't clear secret for `authsource' since it will
               ;; corrupt any future fetches for it.
               (not (eq (plist-get creds :source) 'authsource)))
          (clear-string (plist-get creds :secret))))))

(defun url-digest-auth (url &optional prompt overwrite realm attrs)
  "Get the HTTP Digest response string for the specified URL.

If optional argument PROMPT is non-nil, ask for the username and
password to use for the URL and its descendants but only if one
cannot be found from cache.  Look also in Emacs auth-source.

If optional third argument OVERWRITE is non-nil, overwrite the
old credentials, if they're found in cache, with new ones from
user prompt or from Emacs auth-source.

If REALM is specified, use that instead of the URL descendant
method to match cached credentials.

Alist ATTRS contains additional attributes for the authentication
challenge such as nonce and opaque."
  (if attrs
      (let* ((href (if (stringp url) (url-generic-parse-url url) url))
             (enable-recursive-minibuffers t)
             (key (url-digest-cached-key href realm)))

        (if (or (null key) overwrite)
            (let ((newkey (url-digest-find-new-key href realm (cond
                                                               (key nil)
                                                               (t prompt)))))
              (if (and newkey key overwrite)
                  (setcdr key (cdr newkey))
                (if (and newkey (null key))
                    (url-digest-cache-key (setq key newkey) href)))))

        (if key
            (url-digest-auth-build-response key href realm attrs)))))

(defvar url-registered-auth-schemes nil
  "A list of the registered authorization schemes and various and sundry
information associated with them.")

(defun url-do-auth-source-search (server type parameter)
  (let* ((auth-info (auth-source-search :max 1 :host server :port type))
         (auth-info (nth 0 auth-info))
         (token (plist-get auth-info parameter))
         (token (if (functionp token) (funcall token) token)))
    token))

;;;###autoload
(defun url-get-authentication (url realm type prompt &optional args)
  "Return an authorization string suitable for use in the WWW-Authenticate
header in an HTTP/1.0 request.

URL    is the url you are requesting authorization to.  This can be either a
       string representing the URL, or the parsed representation returned by
       `url-generic-parse-url'
REALM  is the realm at a specific site we are looking for.  This should be a
       string specifying the exact realm, or nil or the symbol `any' to
       specify that the filename portion of the URL should be used as the
       realm
TYPE   is the type of authentication to be returned.  This is either a string
       representing the type (basic, digest, etc), or nil or the symbol `any'
       to specify that any authentication is acceptable.  If requesting `any'
       the strongest matching authentication will be returned.  If this is
       wrong, it's no big deal, the error from the server will specify exactly
       what type of auth to use
PROMPT is boolean - specifies whether to ask the user for a username/password
       if one cannot be found in the cache"
  (if (not realm)
      (setq realm (cdr-safe (assoc "realm" args))))
  (if (equal realm "")
      (setq realm nil))
  (if (stringp url)
      (setq url (url-generic-parse-url url)))
  (if (or (null type) (eq type 'any))
      ;; Whooo doogies!
      ;; Go through and get _all_ the authorization strings that could apply
      ;; to this URL, store them along with the 'rating' we have in the list
      ;; of schemes, then sort them so that the 'best' is at the front of the
      ;; list, then get the car, then get the cdr.
      ;; Zooom zooom zoooooom
      (cdr-safe
       (car-safe
	(sort
	 (mapcar
	  (function
	   (lambda (scheme)
	     (if (fboundp (car (cdr scheme)))
		 (cons (cdr (cdr scheme))
		       (funcall (car (cdr scheme)) url nil nil realm))
	       (cons 0 nil))))
	  url-registered-auth-schemes)
	 (function
	  (lambda (x y)
	    (cond
	     ((null (cdr x)) nil)
	     ((and (cdr x) (null (cdr y))) t)
	     ((and (cdr x) (cdr y))
	      (>= (car x) (car y)))
	     (t nil)))))))
    (if (symbolp type) (setq type (symbol-name type)))
    (let* ((scheme (car-safe
		    (cdr-safe (assoc (downcase type)
				     url-registered-auth-schemes)))))
      (if (and scheme (fboundp scheme))
	  (funcall scheme url prompt
		   (and prompt
			(funcall scheme url nil nil realm args))
		   realm args)))))

;;;###autoload
(defun url-register-auth-scheme (type &optional function rating)
  "Register an HTTP authentication method.

TYPE     is a string or symbol specifying the name of the method.
         This should be the same thing you expect to get returned in
         an Authenticate header in HTTP/1.0 - it will be downcased.
FUNCTION is the function to call to get the authorization information.
         This defaults to `url-?-auth', where ? is TYPE.
RATING   a rating between 1 and 10 of the strength of the authentication.
         This is used when asking for the best authentication for a specific
         URL.  The item with the highest rating is returned."
  (let* ((type (cond
		((stringp type) (downcase type))
		((symbolp type) (downcase (symbol-name type)))
		(t (error "Bad call to `url-register-auth-scheme'"))))
	 (function (or function (intern (concat "url-" type "-auth"))))
	 (rating (cond
		  ((null rating) 2)
		  ((stringp rating) (string-to-number rating))
		  (t rating)))
	 (node (assoc type url-registered-auth-schemes)))
    (if (not (fboundp function))
	(url-warn
	 'security
	 (format-message
	  "Tried to register `%s' as an auth scheme, but it is not a function!"
	  function)))
    (if node
	(setcdr node (cons function rating))
      (setq url-registered-auth-schemes
	    (cons (cons type (cons function rating))
		  url-registered-auth-schemes)))))

(defun url-auth-registered (scheme)
  "Return non-nil if SCHEME is registered as an auth type."
  (assoc scheme url-registered-auth-schemes))

(provide 'url-auth)

;;; url-auth.el ends here
