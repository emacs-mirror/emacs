;;; auth-source.el --- authentication sources for Gnus and Emacs

;; Copyright (C) 2008-2011 Free Software Foundation, Inc.

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

;; TODO:

;; - never decode the backend file unless it's necessary
;; - a more generic way to match backends and search backend contents
;; - absorb netrc.el and simplify it
;; - protect passwords better
;; - allow creating and changing netrc lines (not files) e.g. change a password

;;; Code:

(require 'password-cache)
(require 'gnus-util)
(require 'netrc)
(require 'assoc)
(eval-when-compile (require 'cl))
(require 'eieio)

(autoload 'secrets-create-item "secrets")
(autoload 'secrets-delete-item "secrets")
(autoload 'secrets-get-alias "secrets")
(autoload 'secrets-get-attributes "secrets")
(autoload 'secrets-get-secret "secrets")
(autoload 'secrets-list-collections "secrets")
(autoload 'secrets-search-items "secrets")

(defvar secrets-enabled)

(defgroup auth-source nil
  "Authentication sources."
  :version "23.1" ;; No Gnus
  :group 'gnus)

(defclass auth-source-backend ()
  ((type :initarg :type
         :initform 'netrc
         :type symbol
         :custom symbol
         :documentation "The backend type.")
   (source :initarg :source
           :type string
           :custom string
           :documentation "The backend source.")
   (host :initarg :host
         :initform t
         :type t
         :custom string
         :documentation "The backend host.")
   (user :initarg :user
         :initform t
         :type t
         :custom string
         :documentation "The backend user.")
   (protocol :initarg :protocol
             :initform t
             :type t
             :custom string
             :documentation "The backend protocol.")
   (create-function :initarg :create-function
                    :initform ignore
                    :type function
                    :custom function
                    :documentation "The create function.")
   (search-function :initarg :search-function
                    :initform ignore
                    :type function
                    :custom function
                    :documentation "The search function.")))

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

(defvar auth-source-creation-defaults nil
  "Defaults for creating token values.  Usually let-bound.")

(make-obsolete 'auth-source-hide-passwords nil "Emacs 24.1")

(defvar auth-source-magic "auth-source-magic ")

(defcustom auth-source-do-cache t
  "Whether auth-source should cache information with `password-cache'."
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
  :type `(choice
          :tag "auth-source debugging mode"
          (const :tag "Log using `message' to the *Messages* buffer" t)
          (function :tag "Function that takes arguments like `message'")
          (const :tag "Don't log anything" nil)))

(defcustom auth-sources '("~/.authinfo.gpg" "~/.authinfo")
  "List of authentication sources.

The default will get login and password information from
\"~/.authinfo.gpg\", which you should set up with the EPA/EPG
packages to be encrypted.  If that file doesn't exist, it will
try the unencrypted version \"~/.authinfo\".

See the auth.info manual for details.

Each entry is the authentication type with optional properties.

It's best to customize this with `M-x customize-variable' because the choices
can get pretty complex."
  :group 'auth-source
  :version "24.1" ;; No Gnus
  :type `(repeat :tag "Authentication Sources"
                 (choice
                  (string :tag "Just a file")
                  (const :tag "Default Secrets API Collection" 'default)
                  (const :tag "Login Secrets API Collection" "secrets:login")
                  (const :tag "Temp Secrets API Collection" "secrets:session")
                  (list :tag "Source definition"
                        (const :format "" :value :source)
                        (choice :tag "Authentication backend choice"
                                (string :tag "Authentication Source (file)")
                                (list
                                 :tag "Secret Service API/KWallet/GNOME Keyring"
                                 (const :format "" :value :secrets)
                                 (choice :tag "Collection to use"
                                         (string :tag "Collection name")
                                         (const :tag "Default" 'default)
                                         (const :tag "Login" "login")
                                         (const
                                          :tag "Temporary" "session"))))
                        (repeat :tag "Extra Parameters" :inline t
                                (choice :tag "Extra parameter"
                                        (list
                                         :tag "Host"
                                         (const :format "" :value :host)
                                         (choice :tag "Host (machine) choice"
                                                 (const :tag "Any" t)
                                                 (regexp
                                                  :tag "Regular expression")))
                                        (list
                                         :tag "Protocol"
                                         (const :format "" :value :protocol)
                                         (choice
                                          :tag "Protocol"
                                          (const :tag "Any" t)
                                          ,@auth-source-protocols-customize))
                                        (list :tag "User" :inline t
                                              (const :format "" :value :user)
                                              (choice :tag "Personality/Username"
                                                      (const :tag "Any" t)
                                                      (string :tag "Name")))))))))

(defcustom auth-source-gpg-encrypt-to t
  "List of recipient keys that `authinfo.gpg' encrypted to.
If the value is not a list, symmetric encryption will be used."
  :group 'auth-source
  :version "24.1" ;; No Gnus
  :type '(choice (const :tag "Symmetric encryption" t)
                 (repeat :tag "Recipient public keys"
                         (string :tag "Recipient public key"))))

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
;;                   (:source (:secrets "session") :host t :protocol t :user "joe")
;;                   (:source (:secrets "login") :host t :protocol t)
;;                   (:source "~/.authinfo.gpg" :host t :protocol t)))

;; (setq auth-sources '((:source (:secrets default) :host t :protocol t :user "joe")
;;                   (:source (:secrets "session") :host t :protocol t :user "joe")
;;                   (:source (:secrets "login") :host t :protocol t)
;;                   ))

;; (setq auth-sources '((:source "~/.authinfo.gpg" :host t :protocol t)))

;; (auth-source-backend-parse "myfile.gpg")
;; (auth-source-backend-parse 'default)
;; (auth-source-backend-parse "secrets:login")

(defun auth-source-backend-parse (entry)
  "Creates an auth-source-backend from an ENTRY in `auth-sources'."
  (auth-source-backend-parse-parameters
   entry
   (cond
    ;; take 'default and recurse to get it as a Secrets API default collection
    ;; matching any user, host, and protocol
    ((eq entry 'default)
     (auth-source-backend-parse '(:source (:secrets default))))
    ;; take secrets:XYZ and recurse to get it as Secrets API collection "XYZ"
    ;; matching any user, host, and protocol
    ((and (stringp entry) (string-match "^secrets:\\(.+\\)" entry))
     (auth-source-backend-parse `(:source (:secrets ,(match-string 1 entry)))))
    ;; take just a file name and recurse to get it as a netrc file
    ;; matching any user, host, and protocol
    ((stringp entry)
     (auth-source-backend-parse `(:source ,entry)))

    ;; a file name with parameters
    ((stringp (plist-get entry :source))
     (auth-source-backend
      (plist-get entry :source)
      :source (plist-get entry :source)
      :type 'netrc
      :search-function 'auth-source-netrc-search
      :create-function 'auth-source-netrc-create))

    ;; the Secrets API.  We require the package, in order to have a
    ;; defined value for `secrets-enabled'.
    ((and
      (not (null (plist-get entry :source))) ; the source must not be nil
      (listp (plist-get entry :source))      ; and it must be a list
      (require 'secrets nil t)               ; and we must load the Secrets API
      secrets-enabled)                       ; and that API must be enabled

     ;; the source is either the :secrets key in ENTRY or
     ;; if that's missing or nil, it's "session"
     (let ((source (or (plist-get (plist-get entry :source) :secrets)
                       "session")))

       ;; if the source is a symbol, we look for the alias named so,
       ;; and if that alias is missing, we use "login"
       (when (symbolp source)
         (setq source (or (secrets-get-alias (symbol-name source))
                          "login")))

       (auth-source-backend
        (format "Secrets API (%s)" source)
        :source source
        :type 'secrets
        :search-function 'auth-source-secrets-search
        :create-function 'auth-source-secrets-create)))

    ;; none of them
    (t
     (auth-source-do-debug
      "auth-source-backend-parse: invalid backend spec: %S" entry)
     (auth-source-backend
      "Empty"
      :source ""
      :type 'ignore)))))

(defun auth-source-backend-parse-parameters (entry backend)
  "Fills in the extra auth-source-backend parameters of ENTRY.
Using the plist ENTRY, get the :host, :protocol, and :user search
parameters.  Accepts :port as an alias to :protocol.  Sets all
the parameters to t if they are missing."
  (let (val)
    (when (setq val (plist-get entry :host))
      (oset backend host val))
    (when (setq val (plist-get entry :user))
      (oset backend user val))
    ;; accept :port as an alias for :protocol
    (when (setq val (or (plist-get entry :protocol) (plist-get entry :port)))
      (oset backend protocol val)))
  backend)

;; (mapcar 'auth-source-backend-parse auth-sources)

(defun* auth-source-search (&rest spec
                                  &key type max host user protocol secret
                                  create delete
                                  &allow-other-keys)
  "Search or modify authentication backends according to SPEC.

This function parses `auth-sources' for matches of the SPEC
plist.  It can optionally create or update an authentication
token if requested.  A token is just a standard Emacs property
list with a :secret property that can be a function; all the
other properties will always hold scalar values.

Typically the :secret property, if present, contains a password.

Common search keys are :max, :host, :protocol, and :user.  In
addition, :create specifies how tokens will be or created.
Finally, :type can specify which backend types you want to check.

A string value is always matched literally.  A symbol is matched
as its string value, literally.  All the SPEC values can be
single values (symbol or string) or lists thereof (in which case
any of the search terms matches).

:create t means to create a token if possible.

A new token will be created if no matching tokens were found.
The new token will have only the keys the backend requires.  For
the netrc backend, for instance, that's the user, host, and
protocol keys.

Here's an example:

\(let ((auth-source-creation-defaults '((user . \"defaultUser\")
                                        (A    . \"default A\"))))
  (auth-source-search :host \"mine\" :type 'netrc :max 1
                      :P \"pppp\" :Q \"qqqq\"
                      :create t))

which says:

\"Search for any entry matching host 'mine' in backends of type
 'netrc', maximum one result.

 Create a new entry if you found none.  The netrc backend will
 automatically require host, user, and protocol.  The host will be
 'mine'.  We prompt for the user with default 'defaultUser' and
 for the protocol without a default.  We will not prompt for A, Q,
 or P.  The resulting token will only have keys user, host, and
 protocol.\"

:create '(A B C) also means to create a token if possible.

The behavior is like :create t but if the list contains any
parameter, that parameter will be required in the resulting
token.  The value for that parameter will be obtained from the
search parameters or from user input.  If any queries are needed,
the alist `auth-source-creation-defaults' will be checked for the
default prompt.

Here's an example:

\(let ((auth-source-creation-defaults '((user . \"defaultUser\")
                                        (A    . \"default A\"))))
  (auth-source-search :host '(\"nonesuch\" \"twosuch\") :type 'netrc :max 1
                      :P \"pppp\" :Q \"qqqq\"
                      :create '(A B Q)))

which says:

\"Search for any entry matching host 'nonesuch'
 or 'twosuch' in backends of type 'netrc', maximum one result.

 Create a new entry if you found none.  The netrc backend will
 automatically require host, user, and protocol.  The host will be
 'nonesuch' and Q will be 'qqqq'.  We prompt for A with default
 'default A', for B and protocol with default nil, and for the
 user with default 'defaultUser'.  We will not prompt for Q.  The
 resulting token will have keys user, host, protocol, A, B, and Q.
 It will not have P with any value, even though P is used in the
 search to find only entries that have P set to 'pppp'.\"

When multiple values are specified in the search parameter, the
first one is used for creation.  So :host (X Y Z) would create a
token for host X, for instance.

This creation can fail if the search was not specific enough to
create a new token (it's up to the backend to decide that).  You
should `catch' the backend-specific error as usual.  Some
backends (netrc, at least) will prompt the user rather than throw
an error.

:delete t means to delete any found entries.  nil by default.
Use `auth-source-delete' in ELisp code instead of calling
`auth-source-search' directly with this parameter.

:type (X Y Z) will check only those backend types.  'netrc and
'secrets are the only ones supported right now.

:max N means to try to return at most N items (defaults to 1).
When 0 the function will return just t or nil to indicate if any
matches were found.  More than N items may be returned, depending
on the search and the backend.

:host (X Y Z) means to match only hosts X, Y, or Z according to
the match rules above.  Defaults to t.

:user (X Y Z) means to match only users X, Y, or Z according to
the match rules above.  Defaults to t.

:protocol (P Q R) means to match only protocols P, Q, or R.
Defaults to t.

:K (V1 V2 V3) for any other key K will match values V1, V2, or
V3 (note the match rules above).

The return value is a list with at most :max tokens.  Each token
is a plist with keys :backend :host :protocol :user, plus any other
keys provided by the backend (notably :secret).  But note the
exception for :max 0, which see above.

The token's :secret key can hold a function.  In that case you
must call it to obtain the actual value."
  (let* ((backends (mapcar 'auth-source-backend-parse auth-sources))
         (max (or max 1))
         (ignored-keys '(:create :delete :max))
         (keys (loop for i below (length spec) by 2
                     unless (memq (nth i spec) ignored-keys)
                     collect (nth i spec)))
         (found (auth-source-recall spec))
         filtered-backends accessor-key found-here goal)

    (if (and found auth-source-do-cache)
        (auth-source-do-debug
         "auth-source-search: found %d CACHED results matching %S"
         (length found) spec)

      (assert
       (or (eq t create) (listp create)) t
       "Invalid auth-source :create parameter (must be nil, t, or a list)")

      (setq filtered-backends (copy-list backends))
      (dolist (backend backends)
        (dolist (key keys)
          ;; ignore invalid slots
          (condition-case signal
              (unless (eval `(auth-source-search-collection
                              (plist-get spec key)
                              (oref backend ,key)))
                (setq filtered-backends (delq backend filtered-backends))
                (return))
            (invalid-slot-name))))

      (auth-source-do-debug
       "auth-source-search: found %d backends matching %S"
       (length filtered-backends) spec)

      ;; (debug spec "filtered" filtered-backends)
      (setq goal max)
      (dolist (backend filtered-backends)
        (setq found-here (apply
                          (slot-value backend 'search-function)
                          :backend backend
                          :create create
                          :delete delete
                          spec))

        ;; if max is 0, as soon as we find something, return it
        (when (and (zerop max) (> 0 (length found-here)))
          (return t))

        ;; decrement the goal by the number of new results
        (decf goal (length found-here))
        ;; and append the new results to the full list
        (setq found (append found found-here))

        (auth-source-do-debug
         "auth-source-search: found %d results (max %d/%d) in %S matching %S"
         (length found-here) max goal backend spec)

        ;; return full list if the goal is 0 or negative
        (when (zerop (max 0 goal))
          (return found))

        ;; change the :max parameter in the spec to the goal
        (setq spec (plist-put spec :max goal)))

      (when (and found auth-source-do-cache)
        (auth-source-remember spec found)))

      found))

;;; (auth-source-search :max 1)
;;; (funcall (plist-get (nth 0 (auth-source-search :max 1)) :secret))
;;; (auth-source-search :host "nonesuch" :type 'netrc :K 1)
;;; (auth-source-search :host "nonesuch" :type 'secrets)

(defun* auth-source-delete (&rest spec
                                  &key delete
                                  &allow-other-keys)
  "Delete entries from the authentication backends according to SPEC.
Calls `auth-source-search' with the :delete property in SPEC set to t.
The backend may not actually delete the entries.

Returns the deleted entries."
  (auth-source-search (plist-put spec :delete t)))

(defun auth-source-search-collection (collection value)
  "Returns t is VALUE is t or COLLECTION is t or contains VALUE."
  (when (and (atom collection) (not (eq t collection)))
    (setq collection (list collection)))

  ;; (debug :collection collection :value value)
  (or (eq collection t)
      (eq value t)
      (equal collection value)
      (member value collection)))

(defun auth-source-forget-all-cached ()
  "Forget all cached auth-source data."
  (interactive)
  (loop for sym being the symbols of password-data
        ;; when the symbol name starts with auth-source-magic
        when (string-match (concat "^" auth-source-magic)
                           (symbol-name sym))
        ;; remove that key
        do (password-cache-remove (symbol-name sym))))

(defun auth-source-remember (spec found)
  "Remember FOUND search results for SPEC."
  (password-cache-add
   (concat auth-source-magic (format "%S" spec)) found))

(defun auth-source-recall (spec)
  "Recall FOUND search results for SPEC."
  (password-read-from-cache
   (concat auth-source-magic (format "%S" spec))))

(defun auth-source-forget (spec)
  "Forget any cached data matching SPEC exactly.

This is the same SPEC you passed to `auth-source-search'.
Returns t or nil for forgotten or not found."
  (password-cache-remove (concat auth-source-magic (format "%S" spec))))

;;; (loop for sym being the symbols of password-data when (string-match (concat "^" auth-source-magic) (symbol-name sym)) collect (symbol-name sym))

;;; (auth-source-remember '(:host "wedd") '(4 5 6))
;;; (auth-source-remember '(:host "xedd") '(1 2 3))
;;; (auth-source-recall '(:host "xedd"))
;;; (auth-source-recall '(:host t))
;;; (auth-source-forget+ :host t)

(defun* auth-source-forget+ (&rest spec &allow-other-keys)
  "Forget any cached data matching SPEC.  Returns forgotten count.

This is not a full `auth-source-search' spec but works similarly.
For instance, \(:host \"myhost\" \"yourhost\") would find all the
cached data that was found with a search for those two hosts,
while \(:host t) would find all host entries."
  (let ((count 0)
        sname)
    (loop for sym being the symbols of password-data
          ;; when the symbol name matches with auth-source-magic
          when (and (setq sname (symbol-name sym))
                    (string-match (concat "^" auth-source-magic "\\(.+\\)")
                                  sname)
                    ;; and the spec matches what was stored in the cache
                    (auth-source-specmatchp spec (read (match-string 1 sname))))
          ;; remove that key
          do (progn
               (password-cache-remove sname)
               (incf count)))
    count))

(defun auth-source-specmatchp (spec stored)
  (let ((keys (loop for i below (length spec) by 2
                   collect (nth i spec))))
    (not (eq
          (dolist (key keys)
            (unless (auth-source-search-collection (plist-get stored key)
                                                   (plist-get spec key))
              (return 'no)))
          'no))))

;;; Backend specific parsing: netrc/authinfo backend

;;; (auth-source-netrc-parse "~/.authinfo.gpg")
(defun* auth-source-netrc-parse (&rest
                                 spec
                                 &key file max host user protocol delete
                                 &allow-other-keys)
  "Parse FILE and return a list of all entries in the file.
Note that the MAX parameter is used so we can exit the parse early."
  (if (listp file)
      ;; We got already parsed contents; just return it.
      file
    (when (file-exists-p file)
      (with-temp-buffer
        (let ((tokens '("machine" "host" "default" "login" "user"
                        "password" "account" "macdef" "force"
                        "port" "protocol"))
              (max (or max 5000))       ; sanity check: default to stop at 5K
              (modified 0)
              alist elem result pair)
          (insert-file-contents file)
          (goto-char (point-min))
          ;; Go through the file, line by line.
          (while (and (not (eobp))
                      (> max 0))

            (narrow-to-region (point) (point-at-eol))
            ;; For each line, get the tokens and values.
            (while (not (eobp))
              (skip-chars-forward "\t ")
              ;; Skip lines that begin with a "#".
              (if (eq (char-after) ?#)
                  (goto-char (point-max))
                (unless (eobp)
                  (setq elem
                        (if (= (following-char) ?\")
                            (read (current-buffer))
                          (buffer-substring
                           (point) (progn (skip-chars-forward "^\t ")
                                          (point)))))
                  (cond
                   ((equal elem "macdef")
                    ;; We skip past the macro definition.
                    (widen)
                    (while (and (zerop (forward-line 1))
                                (looking-at "$")))
                    (narrow-to-region (point) (point)))
                   ((member elem tokens)
                    ;; Tokens that don't have a following value are ignored,
                    ;; except "default".
                    (when (and pair (or (cdr pair)
                                        (equal (car pair) "default")))
                      (push pair alist))
                    (setq pair (list elem)))
                   (t
                    ;; Values that haven't got a preceding token are ignored.
                    (when pair
                      (setcdr pair elem)
                      (push pair alist)
                      (setq pair nil)))))))

            (when (and alist
                       (> max 0)
                       (auth-source-search-collection
                        host
                        (or
                         (aget alist "machine")
                         (aget alist "host")))
                       (auth-source-search-collection
                        user
                        (or
                         (aget alist "login")
                         (aget alist "account")
                         (aget alist "user")))
                       (auth-source-search-collection
                        protocol
                        (or
                         (aget alist "port")
                         (aget alist "protocol"))))
              (decf max)
              (push (nreverse alist) result)
              ;; to delete a line, we just comment it out
              (when delete
                (goto-char (point-min))
                (insert "#")
                (incf modified)))
            (setq alist nil
                  pair nil)
            (widen)
            (forward-line 1))

          (when (< 0 modified)
            (when auth-source-gpg-encrypt-to
              ;; (see bug#7487) making `epa-file-encrypt-to' local to
              ;; this buffer lets epa-file skip the key selection query
              ;; (see the `local-variable-p' check in
              ;; `epa-file-write-region').
              (unless (local-variable-p 'epa-file-encrypt-to (current-buffer))
                (make-local-variable 'epa-file-encrypt-to))
              (if (listp auth-source-gpg-encrypt-to)
                  (setq epa-file-encrypt-to auth-source-gpg-encrypt-to)))

            ;; ask AFTER we've successfully opened the file
            (when (y-or-n-p (format "Save file %s? (%d modifications)"
                                    file modified))
              (write-region (point-min) (point-max) file nil 'silent)
              (auth-source-do-debug
               "auth-source-netrc-parse: modified %d lines in %s"
               modified file)))

          (nreverse result))))))

(defun auth-source-netrc-normalize (alist)
  (mapcar (lambda (entry)
            (let (ret item)
              (while (setq item (pop entry))
                (let ((k (car item))
                      (v (cdr item)))

                  ;; apply key aliases
                  (setq k (cond ((member k '("machine")) "host")
                                ((member k '("login" "account")) "user")
                                ((member k '("protocol")) "port")
                                ((member k '("password")) "secret")
                                (t k)))

                  ;; send back the secret in a function (lexical binding)
                  (when (equal k "secret")
                    (setq v (lexical-let ((v v))
                              (lambda () v))))

                  (setq ret (plist-put ret
                                       (intern (concat ":" k))
                                       v))
                  ))
              ret))
          alist))

;;; (setq secret (plist-get (nth 0 (auth-source-search :host t :type 'netrc :K 1 :max 1)) :secret))
;;; (funcall secret)

(defun* auth-source-netrc-search (&rest
                                  spec
                                  &key backend create delete
                                  type max host user protocol
                                  &allow-other-keys)
"Given a property list SPEC, return search matches from the :backend.
See `auth-source-search' for details on SPEC."
  ;; just in case, check that the type is correct (null or same as the backend)
  (assert (or (null type) (eq type (oref backend type)))
          t "Invalid netrc search")

  (let ((results (auth-source-netrc-normalize
                  (auth-source-netrc-parse
                   :max max
                   :delete delete
                   :file (oref backend source)
                   :host (or host t)
                   :user (or user t)
                   :protocol (or protocol t)))))

    ;; if we need to create an entry AND none were found to match
    (when (and create
               (= 0 (length results)))

      ;; create based on the spec
      (apply (slot-value backend 'create-function) spec)
      ;; turn off the :create key
      (setq spec (plist-put spec :create nil))
      ;; run the search again to get the updated data
      ;; the result will be returned, even if the search fails
      (setq results (apply 'auth-source-netrc-search spec)))

    results))

;;; (auth-source-search :host "nonesuch" :type 'netrc :max 1 :create t)
;;; (auth-source-search :host "nonesuch" :type 'netrc :max 1 :create t :create-extra-keys '((A "default A") (B)))

(defun* auth-source-netrc-create (&rest spec
                                        &key backend
                                        secret host user protocol create
                                        &allow-other-keys)
  (let* ((base-required '(host user protocol secret))
         ;; we know (because of an assertion in auth-source-search) that the
         ;; :create parameter is either t or a list (which includes nil)
         (create-extra (if (eq t create) nil create))
         (required (append base-required create-extra))
         (file (oref backend source))
         (add "")
         ;; `valist' is an alist
         valist)

    ;; only for base required elements (defined as function parameters):
    ;; fill in the valist with whatever data we may have from the search
    ;; we take the first value if it's a list, the whole value otherwise
    (dolist (br base-required)
      (when (symbol-value br)
        (aput 'valist br (if (listp (symbol-value br))
                             (nth 0 (symbol-value br))
                           (symbol-value br)))))

    ;; for extra required elements, see if the spec includes a value for them
    (dolist (er create-extra)
      (let ((name (concat ":" (symbol-name er)))
            (keys (loop for i below (length spec) by 2
                        collect (nth i spec))))
        (dolist (k keys)
          (when (equal (symbol-name k) name)
            (aput 'valist er (plist-get spec k))))))

    ;; for each required element
    (dolist (r required)
      (let* ((data (aget valist r))
             (given-default (aget auth-source-creation-defaults r))
             ;; the defaults are simple
             (default (cond
                       ((and (not given-default) (eq r 'user))
                        (user-login-name))
                       ;; note we need this empty string
                       ((and (not given-default) (eq r 'protocol))
                        "")
                       (t given-default)))
             ;; the prompt's default string depends on the data so far
             (default-string (if (and default (< 0 (length default)))
                                 (format " (default %s)" default)
                               " (no default)"))
             ;; the prompt should also show what's entered so far
             (user-value (aget valist 'user))
             (host-value (aget valist 'host))
             (protocol-value (aget valist 'protocol))
             (info-so-far (concat (if user-value
                                      (format "%s@" user-value)
                                    "[USER?]")
                                  (if host-value
                                      (format "%s" host-value)
                                    "[HOST?]")
                                  (if protocol-value
                                      ;; this distinguishes protocol between
                                      (if (zerop (length protocol-value))
                                          "" ; 'entered as "no default"' vs.
                                        (format ":%s" protocol-value)) ; given
                                    ;; and this is when the protocol is unknown
                                    "[PROTOCOL?]"))))

        ;; now prompt if the search SPEC did not include a required key;
        ;; take the result and put it in `data' AND store it in `valist'
        (aput 'valist r
              (setq data
                    (cond
                     ((and (null data) (eq r 'secret))
                      ;; special case prompt for passwords
                      (read-passwd (format "Password for %s: " info-so-far)))
                     ((null data)
                      (read-string
                       (format "Enter %s for %s%s: "
                               r info-so-far default-string)
                       nil nil default))
                     (t data))))

        ;; when r is not an empty string...
        (when (and (stringp data)
                   (< 0 (length data)))
          ;; append the key (the symbol name of r) and the value in r
          (setq add (concat add
                            (format "%s%s %S"
                                    ;; prepend a space
                                    (if (zerop (length add)) "" " ")
                                    ;; remap auth-source tokens to netrc
                                    (case r
                                     ('user "login")
                                     ('host "machine")
                                     ('secret "password")
                                     ('protocol "port")
                                     (t (symbol-name r)))
                                    ;; the value will be printed in %S format
                                    data))))))

    (with-temp-buffer
      (when (file-exists-p file)
        (insert-file-contents file))
      (when auth-source-gpg-encrypt-to
        ;; (see bug#7487) making `epa-file-encrypt-to' local to
        ;; this buffer lets epa-file skip the key selection query
        ;; (see the `local-variable-p' check in
        ;; `epa-file-write-region').
        (unless (local-variable-p 'epa-file-encrypt-to (current-buffer))
          (make-local-variable 'epa-file-encrypt-to))
        (if (listp auth-source-gpg-encrypt-to)
            (setq epa-file-encrypt-to auth-source-gpg-encrypt-to)))
      (goto-char (point-max))

      ;; ask AFTER we've successfully opened the file
      (when (y-or-n-p (format "Add to file %s: line [%s]" file add))
        (unless (bolp)
          (insert "\n"))
        (insert add "\n")
        (write-region (point-min) (point-max) file nil 'silent)
        (auth-source-do-debug
         "auth-source-netrc-create: wrote 1 new line to %s"
         file)))))

;;; Backend specific parsing: Secrets API backend

;;; (let ((auth-sources '(default))) (auth-source-search :max 1 :create t))
;;; (let ((auth-sources '(default))) (auth-source-search :max 1 :delete t))
;;; (let ((auth-sources '(default))) (auth-source-search :max 1))
;;; (let ((auth-sources '(default))) (auth-source-search))
;;; (let ((auth-sources '("secrets:login"))) (auth-source-search :max 1))
;;; (let ((auth-sources '("secrets:login"))) (auth-source-search :max 1 :signon_realm "https://git.gnus.org/Git"))

(defun* auth-source-secrets-search (&rest
                                    spec
                                    &key backend create delete label
                                    type max host user protocol
                                    &allow-other-keys)
  "Search the Secrets API; spec is like `auth-source'.

The :label key specifies the item's label.  It is the only key
that can specify a substring.  Any :label value besides a string
will allow any label.

All other search keys must match exactly.  If you need substring
matching, do a wider search and narrow it down yourself.

You'll get back all the properties of the token as a plist.

Here's an example that looks for the first item in the 'login'
Secrets collection:

 \(let ((auth-sources '(\"secrets:login\")))
    (auth-source-search :max 1)

Here's another that looks for the first item in the 'login'
Secrets collection whose label contains 'gnus':

 \(let ((auth-sources '(\"secrets:login\")))
    (auth-source-search :max 1 :label \"gnus\")

And this one looks for the first item in the 'login' Secrets
collection that's a Google Chrome entry for the git.gnus.org site
login:

 \(let ((auth-sources '(\"secrets:login\")))
    (auth-source-search :max 1 :signon_realm \"https://git.gnus.org/Git\"))
"

  ;; TODO
  (assert (not create) nil
          "The Secrets API auth-source backend doesn't support creation yet")
  ;; TODO
  ;; (secrets-delete-item coll elt)
  (assert (not delete) nil
          "The Secrets API auth-source backend doesn't support deletion yet")

  (let* ((coll (oref backend source))
         (max (or max 5000))     ; sanity check: default to stop at 5K
         (ignored-keys '(:create :delete :max :backend :label))
         (search-keys (loop for i below (length spec) by 2
                            unless (memq (nth i spec) ignored-keys)
                            collect (nth i spec)))
         ;; build a search spec without the ignored keys
         ;; if a search key is nil or t (match anything), we skip it
         (search-spec (mapcan (lambda (k) (if (or (null (plist-get spec k))
                                             (eq t (plist-get spec k)))
                                         nil
                                       (list k (plist-get spec k))))
                              search-keys))
         ;; needed keys (always including host, login, protocol, and secret)
         (returned-keys (remove-duplicates (append
                                            '(:host :login :protocol :secret)
                                            search-keys)))
         (items (loop for item in (apply 'secrets-search-items coll search-spec)
                      unless (and (stringp label)
                                  (not (string-match label item)))
                      collect item))
         ;; TODO: respect max in `secrets-search-items', not after the fact
         (items (subseq items 0 (min (length items) max)))
         ;; convert the item name to a full plist
         (items (mapcar (lambda (item)
                          (append
                           ;; make an entry for the secret (password) element
                           (list
                            :secret
                            (lexical-let ((v (secrets-get-secret coll item)))
                              (lambda () v)))
                           ;; rewrite the entry from ((k1 v1) (k2 v2)) to plist
                           (mapcan (lambda (entry)
                                     (list (car entry) (cdr entry)))
                                   (secrets-get-attributes coll item))))
                        items))
         ;; ensure each item has each key in `returned-keys'
         (items (mapcar (lambda (plist)
                          (append
                           (mapcan (lambda (req)
                                     (if (plist-get plist req)
                                         nil
                                       (list req nil)))
                                   returned-keys)
                           plist))
                        items)))
    items))

(defun* auth-source-secrets-create (&rest
                                    spec
                                    &key backend type max host user protocol
                                    &allow-other-keys)
  ;; TODO
  ;; (apply 'secrets-create-item (auth-get-source entry) name passwd spec)
  (debug spec))

;;; older API

;;; (auth-source-user-or-password '("login" "password") "imap.myhost.com" t "tzz")

;; deprecate the old interface
(make-obsolete 'auth-source-user-or-password
               'auth-source-search "Emacs 24.1")
(make-obsolete 'auth-source-forget-user-or-password
               'auth-source-forget "Emacs 24.1")

(defun auth-source-user-or-password
  (mode host protocol &optional username create-missing delete-existing)
  "Find MODE (string or list of strings) matching HOST and PROTOCOL.

DEPRECATED in favor of `auth-source-search'!

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
   "auth-source-user-or-password: DEPRECATED get %s for %s (%s) + user=%s"
   mode host protocol username)

  (let* ((listy (listp mode))
         (mode (if listy mode (list mode)))
         (cname (if username
                    (format "%s %s:%s %s" mode host protocol username)
                  (format "%s %s:%s" mode host protocol)))
         (search (list :host host :protocol protocol))
         (search (if username (append search (list :user username)) search))
         (search (if create-missing
                     (append search (list :create t))
                   search))
         (search (if delete-existing
                     (append search (list :delete t))
                   search))
         ;; (found (if (not delete-existing)
         ;;            (gethash cname auth-source-cache)
         ;;          (remhash cname auth-source-cache)
         ;;          nil)))
         (found nil))
    (if found
        (progn
          (auth-source-do-debug
           "auth-source-user-or-password: DEPRECATED cached %s=%s for %s (%s) + %s"
           mode
           ;; don't show the password
           (if (and (member "password" mode) t)
               "SECRET"
             found)
           host protocol username)
          found)                        ; return the found data
      ;; else, if not found, search with a max of 1
      (let ((choice (nth 0 (apply 'auth-source-search
                                  (append '(:max 1) search)))))
        (when choice
          (dolist (m mode)
            (cond
             ((equal "password" m)
              (push (if (plist-get choice :secret)
                      (funcall (plist-get choice :secret))
                    nil) found))
             ((equal "login" m)
              (push (plist-get choice :user) found)))))
        (setq found (nreverse found))
        (setq found (if listy found (car-safe found)))))

        found))

(provide 'auth-source)

;;; auth-source.el ends here
