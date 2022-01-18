;;; auth-source.el --- authentication sources for Gnus and Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2008-2022 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

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

(require 'json)
(require 'password-cache)

(require 'cl-lib)
(require 'eieio)

(autoload 'secrets-create-item "secrets")
(autoload 'secrets-delete-item "secrets")
(autoload 'secrets-get-alias "secrets")
(autoload 'secrets-get-attributes "secrets")
(autoload 'secrets-get-secret "secrets")
(autoload 'secrets-list-collections "secrets")
(autoload 'secrets-search-items "secrets")

(autoload 'rfc2104-hash "rfc2104")

(autoload 'plstore-open "plstore")
(autoload 'plstore-find "plstore")
(autoload 'plstore-put "plstore")
(autoload 'plstore-delete "plstore")
(autoload 'plstore-save "plstore")
(autoload 'plstore-get-file "plstore")

(eval-when-compile (require 'epg)) ;; setf-method for `epg-context-armor'
(autoload 'epg-make-context "epg")
(autoload 'epg-context-set-passphrase-callback "epg")
(autoload 'epg-decrypt-string "epg")
(autoload 'epg-encrypt-string "epg")

(autoload 'help-mode "help-mode" nil t)

(defvar secrets-enabled)

(defgroup auth-source nil
  "Authentication sources."
  :version "23.1" ;; No Gnus
  :group 'gnus)

;;;###autoload
(defcustom auth-source-cache-expiry 7200
  "How many seconds passwords are cached, or nil to disable expiring.
Overrides `password-cache-expiry' through a let-binding."
  :version "24.1"
  :type '(choice (const :tag "Never" nil)
                 (const :tag "All Day" 86400)
                 (const :tag "2 Hours" 7200)
                 (const :tag "30 Minutes" 1800)
                 (integer :tag "Seconds")))

;; The slots below correspond with the `auth-source-search' spec,
;; so a backend with :host set, for instance, would match only
;; searches for that host.  Normally they are nil.
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
   (port :initarg :port
         :initform t
         :type t
         :custom string
         :documentation "The backend protocol.")
   (data :initarg :data
         :initform nil
         :documentation "Internal backend data.")
   (create-function :initarg :create-function
                    :initform #'ignore
                    :type function
                    :custom function
                    :documentation "The create function.")
   (search-function :initarg :search-function
                    :initform #'ignore
                    :type function
                    :custom function
                    :documentation "The search function.")))

(defcustom auth-source-protocols '((imap "imap" "imaps" "143" "993")
                                   (pop3 "pop3" "pop" "pop3s" "110" "995")
                                   (ssh  "ssh" "22")
                                   (sftp "sftp" "115")
                                   (smtp "smtp" "25"))
  "List of authentication protocols and their names."

  :version "23.2" ;; No Gnus
  :type '(repeat :tag "Authentication Protocols"
                 (cons :tag "Protocol Entry"
                       (symbol :tag "Protocol")
                       (repeat :tag "Names"
                               (string :tag "Name")))))

;; Generate all the protocols in a format Customize can use.
;; TODO: generate on the fly from auth-source-protocols
(defconst auth-source-protocols-customize
  (mapcar (lambda (a)
            (let ((p (car-safe a)))
              (list 'const
                    :tag (upcase (symbol-name p))
                    p)))
          auth-source-protocols))

(defvar auth-source-creation-defaults nil
  ;; FIXME: AFAICT this is not set (or let-bound) anywhere!
  "Defaults for creating token values.  Usually let-bound.")

(defvar auth-source-creation-prompts nil
  "Default prompts for token values.  Usually let-bound.")

(make-obsolete 'auth-source-hide-passwords nil "24.1")

(defcustom auth-source-save-behavior 'ask
  "If set, auth-source will respect it for save behavior."
  :version "23.2" ;; No Gnus
  :type '(choice
          :tag "auth-source new token save behavior"
          (const :tag "Always save" t)
          (const :tag "Never save" nil)
          (const :tag "Ask" ask)))

;; TODO: make the default (setq auth-source-netrc-use-gpg-tokens `((,(if (boundp 'epa-file-auto-mode-alist-entry) (car epa-file-auto-mode-alist-entry) "\\.gpg\\'") never) (t gpg)))
;; TODO: or maybe leave as (setq auth-source-netrc-use-gpg-tokens 'never)

(defcustom auth-source-netrc-use-gpg-tokens 'never
  "Set this to tell `auth-source' when to create GPG password
tokens in netrc files.  It's either an alist or `never'.
Note that if EPA/EPG is not available, this should NOT be used."
  :version "23.2" ;; No Gnus
  :type `(choice
          (const :tag "Always use GPG password tokens" (t gpg))
          (const :tag "Never use GPG password tokens" never)
          (repeat :tag "Use a lookup list"
                  (list
                   (choice :tag "Matcher"
                           (const :tag "Match anything" t)
                           (const :tag "The EPA encrypted file extensions"
                                  ,(if (boundp 'epa-file-auto-mode-alist-entry)
                                       (car epa-file-auto-mode-alist-entry)
                                     "\\.gpg\\'"))
                           (regexp :tag "Regular expression"))
                   (choice :tag "What to do"
                           (const :tag "Save GPG-encrypted password tokens" gpg)
                           (const :tag "Don't encrypt tokens" never))))))

(defcustom auth-source-do-cache t
  "Whether auth-source should cache information with `password-cache'."
  :version "23.2" ;; No Gnus
  :type 'boolean)

(defcustom auth-source-debug nil
  "Whether auth-source should log debug messages.

If the value is nil, debug messages are not logged.

If the value is t, debug messages are logged with `message'.  In
that case, your authentication data will be in the clear (except
for passwords).

If the value is a function, debug messages are logged by calling
 that function using the same arguments as `message'."
  :version "23.2" ;; No Gnus
  :type '(choice
          :tag "auth-source debugging mode"
          (const :tag "Log using `message' to the *Messages* buffer" t)
          (const :tag "Log all trivia with `message' to the *Messages* buffer"
                 trivia)
          (function :tag "Function that takes arguments like `message'")
          (const :tag "Don't log anything" nil)))

(defcustom auth-sources '("~/.authinfo" "~/.authinfo.gpg" "~/.netrc")
  "List of authentication sources.
Each entry is the authentication type with optional properties.
Entries are tried in the order in which they appear.
See Info node `(auth)Help for users' for details.

If an entry names a file with the \".gpg\" extension and you have
EPA/EPG set up, the file will be encrypted and decrypted
automatically.  See Info node `(epa)Encrypting/decrypting gpg files'
for details.

It's best to customize this with `\\[customize-variable]' because the choices
can get pretty complex."
  :version "26.1" ; neither new nor changed default
  :type `(repeat :tag "Authentication Sources"
                 (choice
                  (string :tag "Just a file")
                  (const :tag "Default Secrets API Collection" default)
                  (const :tag "Login Secrets API Collection" "secrets:Login")
                  (const :tag "Temp Secrets API Collection" "secrets:session")

                  (const :tag "Default internet Mac OS Keychain"
                         macos-keychain-internet)

                  (const :tag "Default generic Mac OS Keychain"
                         macos-keychain-generic)

                  (list :tag "Source definition"
                        (const :format "" :value :source)
                        (choice :tag "Authentication backend choice"
                                (string :tag "Authentication Source (file)")
                                (list
                                 :tag "Secret Service API/KWallet/GNOME Keyring"
                                 (const :format "" :value :secrets)
                                 (choice :tag "Collection to use"
                                         (string :tag "Collection name")
                                         (const :tag "Default" default)
                                         (const :tag "Login" "Login")
                                         (const
                                          :tag "Temporary" "session")))
                                (list
                                 :tag "Mac OS internet Keychain"
                                 (const :format ""
                                        :value :macos-keychain-internet)
                                 (choice :tag "Collection to use"
                                         (string :tag "internet Keychain path")
                                         (const :tag "default" default)))
                                (list
                                 :tag "Mac OS generic Keychain"
                                 (const :format ""
                                        :value :macos-keychain-generic)
                                 (choice :tag "Collection to use"
                                         (string :tag "generic Keychain path")
                                         (const :tag "default" default))))
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
                                         (const :format "" :value :port)
                                         (choice
                                          :tag "Protocol"
                                          (const :tag "Any" t)
                                          ,@auth-source-protocols-customize))
                                        (list :tag "User" :inline t
                                              (const :format "" :value :user)
                                              (choice
                                               :tag "Personality/Username"
                                               (const :tag "Any" t)
                                               (string
                                                :tag "Name"))))))
                  (sexp :tag "A data structure (external provider)"))))

(defcustom auth-source-gpg-encrypt-to t
  "List of recipient keys that `authinfo.gpg' encrypted to.
If the value is not a list, symmetric encryption will be used."
  :version "24.1" ;; No Gnus
  :type '(choice (const :tag "Symmetric encryption" t)
                 (repeat :tag "Recipient public keys"
                         (string :tag "Recipient public key"))))

(defun auth-source-do-debug (&rest msg)
  (when auth-source-debug
    (apply #'auth-source-do-warn msg)))

(defun auth-source-do-trivia (&rest msg)
  (when (or (eq auth-source-debug 'trivia)
            (functionp auth-source-debug))
    (apply #'auth-source-do-warn msg)))

(defun auth-source-do-warn (&rest msg)
  (apply
   ;; set logger to either the function in auth-source-debug or 'message
   ;; note that it will be 'message if auth-source-debug is nil
   (if (functionp auth-source-debug)
       auth-source-debug
     'message)
   msg))

(defun auth-source-read-char-choice (prompt choices)
  "Read one of CHOICES by `read-char-choice', or `read-char'.
`dropdown-list' support is disabled because it doesn't work reliably.
Only one of CHOICES will be returned.  The PROMPT is augmented
with \"[a/b/c] \" if CHOICES is \(?a ?b ?c)."
  (when choices
    (let* ((prompt-choices
            (apply #'concat
                   (cl-loop for c in choices collect (format "%c/" c))))
           (prompt-choices (concat "[" (substring prompt-choices 0 -1) "] "))
           (full-prompt (concat prompt prompt-choices))
           k)

      (while (not (memq k choices))
        (setq k (read-char-choice full-prompt choices)))
      k)))

(defvar auth-source-backend-parser-functions nil
  "List of auth-source parser functions.
Each function takes an entry from `auth-sources' as parameter and
returns a backend or nil if the entry is not supported.  Add a
parser function to this list with `add-hook'.  Searching for a
backend starts with the first element on the list and stops as
soon as a function returns non-nil.")

(defun auth-source-backend-parse (entry)
  "Create an `auth-source-backend' from an ENTRY in `auth-sources'."

  (let ((backend
         (run-hook-with-args-until-success 'auth-source-backend-parser-functions
                                           entry)))

    (unless backend
      ;; none of the parsers worked
      (auth-source-do-warn
       "auth-source-backend-parse: invalid backend spec: %S" entry)
      (setq backend (make-instance 'auth-source-backend
                                   :source ""
                                   :type 'ignore)))
    (auth-source-backend-parse-parameters entry backend)))

(defun auth-source-backends-parser-file (entry)
  ;; take just a file name use it as a netrc/plist file
  ;; matching any user, host, and protocol
  (when (stringp entry)
    (setq entry (list :source entry)))
  (let* ((source (plist-get entry :source))
         (source-without-gpg
          (if (and (stringp source)
                   (equal (file-name-extension source) "gpg"))
              (file-name-sans-extension source)
            (or source "")))
         (extension (or (and (stringp source-without-gpg)
                             (file-name-extension source-without-gpg))
                        "")))
    (when (stringp source)
      (cond
       ((equal extension "plist")
        (auth-source-backend
         source
         :source source
         :type 'plstore
         :search-function #'auth-source-plstore-search
         :create-function #'auth-source-plstore-create
         :data (plstore-open source)))
       ((member-ignore-case extension '("json"))
        (auth-source-backend
         source
         :source source
         :type 'json
         :search-function #'auth-source-json-search))
       (t
        (auth-source-backend
         source
         :source source
         :type 'netrc
         :search-function #'auth-source-netrc-search
         :create-function #'auth-source-netrc-create))))))

;; Note this function should be last in the parser functions, so we add it first
(add-hook 'auth-source-backend-parser-functions #'auth-source-backends-parser-file)

(defun auth-source-backends-parser-macos-keychain (entry)
  ;; take macos-keychain-{internet,generic}:XYZ and use it as macOS
  ;; Keychain "XYZ" matching any user, host, and protocol
  (when (and (stringp entry) (string-match "^macos-keychain-internet:\\(.+\\)"
                                           entry))
    (setq entry `(:source (:macos-keychain-internet
                           ,(match-string 1 entry)))))
  (when (and (stringp entry) (string-match "^macos-keychain-generic:\\(.+\\)"
                                           entry))
    (setq entry `(:source (:macos-keychain-generic
                           ,(match-string 1 entry)))))
  ;; take 'macos-keychain-internet or generic and use it as a Mac OS
  ;; Keychain collection matching any user, host, and protocol
  (when (eq entry 'macos-keychain-internet)
    (setq entry '(:source (:macos-keychain-internet default))))
  (when (eq entry 'macos-keychain-generic)
    (setq entry '(:source (:macos-keychain-generic default))))
  (cond
   ;; the macOS Keychain
   ((and
     (not (null (plist-get entry :source))) ; the source must not be nil
     (listp (plist-get entry :source))      ; and it must be a list
     (or
      (plist-get (plist-get entry :source) :macos-keychain-generic)
      (plist-get (plist-get entry :source) :macos-keychain-internet)))

    (let* ((source-spec (plist-get entry :source))
           (keychain-generic (plist-get source-spec :macos-keychain-generic))
           (keychain-type (if keychain-generic
                              'macos-keychain-generic
                            'macos-keychain-internet))
           (source (plist-get source-spec (if keychain-generic
                                              :macos-keychain-generic
                                            :macos-keychain-internet))))

      (when (symbolp source)
        (setq source (symbol-name source)))

      (auth-source-backend
       (format "Mac OS Keychain (%s)" source)
       :source source
       :type keychain-type
       :search-function #'auth-source-macos-keychain-search
       :create-function #'auth-source-macos-keychain-create)))))

(add-hook 'auth-source-backend-parser-functions #'auth-source-backends-parser-macos-keychain)

(defun auth-source-backends-parser-secrets (entry)
  ;; take secrets:XYZ and use it as Secrets API collection "XYZ"
  ;; matching any user, host, and protocol
  (when (and (stringp entry) (string-match "^secrets:\\(.+\\)" entry))
    (setq entry `(:source (:secrets ,(match-string 1 entry)))))
  ;; take 'default and use it as a Secrets API default collection
  ;; matching any user, host, and protocol
  (when (eq entry 'default)
    (setq entry '(:source (:secrets default))))
  (cond
   ;; the Secrets API.  We require the package, in order to have a
   ;; defined value for `secrets-enabled'.
   ((and
     (not (null (plist-get entry :source))) ; the source must not be nil
     (listp (plist-get entry :source))      ; and it must be a list
     (not (null (plist-get
                 (plist-get entry :source)
                 :secrets))) ; the source must have :secrets
     (require 'secrets nil t)               ; and we must load the Secrets API
     secrets-enabled)                       ; and that API must be enabled

    ;; the source is either the :secrets key in ENTRY or
    ;; if that's missing or nil, it's "session"
    (let ((source (plist-get (plist-get entry :source) :secrets)))

      ;; if the source is a symbol, we look for the alias named so,
      ;; and if that alias is missing, we use "Login"
      (when (symbolp source)
        (setq source (or (secrets-get-alias (symbol-name source))
                         "Login")))

      (if (featurep 'secrets)
          (auth-source-backend
           (format "Secrets API (%s)" source)
           :source source
           :type 'secrets
           :search-function #'auth-source-secrets-search
           :create-function #'auth-source-secrets-create)
        (auth-source-do-warn
         "auth-source-backend-parse: no Secrets API, ignoring spec: %S" entry)
        (auth-source-backend
         (format "Ignored Secrets API (%s)" source)
         :source ""
         :type 'ignore))))))

(add-hook 'auth-source-backend-parser-functions #'auth-source-backends-parser-secrets)

(defun auth-source-backend-parse-parameters (entry backend)
  "Fill in the extra `auth-source-backend' parameters of ENTRY.
Using the plist ENTRY, get the :host, :port, and :user search
parameters."
  (let ((entry (if (stringp entry)
                   nil
                 entry))
        val)
    (when (setq val (plist-get entry :host))
      (oset backend host val))
    (when (setq val (plist-get entry :user))
      (oset backend user val))
    (when (setq val (plist-get entry :port))
      (oset backend port val)))
  backend)

;; (mapcar #'auth-source-backend-parse auth-sources)

(cl-defun auth-source-search (&rest spec
                              &key max require create delete
                              &allow-other-keys)
  "Search or modify authentication backends according to SPEC.

This function parses `auth-sources' for matches of the SPEC
plist.  It can optionally create or update an authentication
token if requested.  A token is just a standard Emacs property
list with a :secret property that can be a function; all the
other properties will always hold scalar values.

Typically the :secret property, if present, contains a password.

Common search keys are :max, :host, :port, and :user.  In
addition, :create specifies if and how tokens will be created.
Finally, :type can specify which backend types you want to check.

A string value is always matched literally.  A symbol is matched
as its string value, literally.  All the SPEC values can be
single values (symbol or string) or lists thereof (in which case
any of the search terms matches).

:create t means to create a token if possible.

A new token will be created if no matching tokens were found.
The new token will have only the keys the backend requires.  For
the netrc backend, for instance, that's the user, host, and
port keys.

Here's an example:

\(let ((auth-source-creation-defaults \\='((user . \"defaultUser\")
                                        (A    . \"default A\"))))
  (auth-source-search :host \"mine\" :type \\='netrc :max 1
                      :P \"pppp\" :Q \"qqqq\"
                      :create t))

which says:

\"Search for any entry matching host `mine' in backends of type
 `netrc', maximum one result.

 Create a new entry if you found none.  The netrc backend will
 automatically require host, user, and port.  The host will be
 `mine'.  We prompt for the user with default `defaultUser' and
 for the port without a default.  We will not prompt for A, Q,
 or P.  The resulting token will only have keys user, host, and
 port.\"

:create \\='(A B C) also means to create a token if possible.

The behavior is like :create t but if the list contains any
parameter, that parameter will be required in the resulting
token.  The value for that parameter will be obtained from the
search parameters or from user input.  If any queries are needed,
the alist `auth-source-creation-defaults' will be checked for the
default value.  If the user, host, or port are missing, the alist
`auth-source-creation-prompts' will be used to look up the
prompts IN THAT ORDER (so the `user' prompt will be queried first,
then `host', then `port', and finally `secret').  Each prompt string
can use %u, %h, and %p to show the user, host, and port.  The prompt
is formatted with `format-prompt', a trailing \": \" is removed.

Here's an example:

\(let ((auth-source-creation-defaults \\='((user . \"defaultUser\")
                                        (A    . \"default A\")))
       (auth-source-creation-prompts
        \\='((secret . \"Enter IMAP password for %h:%p\"))))
  (auth-source-search :host \\='(\"nonesuch\" \"twosuch\") :type \\='netrc :max 1
                      :P \"pppp\" :Q \"qqqq\"
                      :create \\='(A B Q)))

which says:

\"Search for any entry matching host `nonesuch'
 or `twosuch' in backends of type `netrc', maximum one result.

 Create a new entry if you found none.  The netrc backend will
 automatically require host, user, and port.  The host will be
 `nonesuch' and Q will be `qqqq'.  We prompt for the password
 with the shown prompt.  We will not prompt for Q.  The resulting
 token will have keys user, host, port, A, B, and Q.  It will not
 have P with any value, even though P is used in the search to
 find only entries that have P set to `pppp'.\"

When multiple values are specified in the search parameter, the
user is prompted for which one.  So :host (X Y Z) would ask the
user to choose between X, Y, and Z.

This creation can fail if the search was not specific enough to
create a new token (it's up to the backend to decide that).  You
should `catch' the backend-specific error as usual.  Some
backends (netrc, at least) will prompt the user rather than throw
an error.

:require (A B C) means that only results that contain those
tokens will be returned.  Thus for instance requiring :secret
will ensure that any results will actually have a :secret
property.

:delete t means to delete any found entries.  nil by default.
Use `auth-source-delete' in ELisp code instead of calling
`auth-source-search' directly with this parameter.

:type (X Y Z) will check only those backend types.  `netrc' and
`secrets' are the only ones supported right now.

:max N means to try to return at most N items (defaults to 1).
More than N items may be returned, depending on the search and
the backend.

When :max is 0 the function will return just t or nil to indicate
if any matches were found.

:host (X Y Z) means to match only hosts X, Y, or Z according to
the match rules above.  Defaults to t.

:user (X Y Z) means to match only users X, Y, or Z according to
the match rules above.  Defaults to t.

:port (P Q R) means to match only protocols P, Q, or R.
Defaults to t.

:K (V1 V2 V3) for any other key K will match values V1, V2, or
V3 (note the match rules above).

The return value is a list with at most :max tokens.  Each token
is a plist with keys :backend :host :port :user, plus any other
keys provided by the backend (notably :secret).  But note the
exception for :max 0, which see above.

The token can hold a :save-function key.  If you call that, the
user will be prompted to save the data to the backend.  You can't
request that this should happen right after creation, because
`auth-source-search' has no way of knowing if the token is
actually useful.  So the caller must arrange to call this function.

The token's :secret key can hold a function.  In that case you
must call it to obtain the actual value."
  (let* ((backends (mapcar #'auth-source-backend-parse auth-sources))
         (max (or max 1))
         (ignored-keys '(:require :create :delete :max))
         (keys (cl-loop for i below (length spec) by 2
                        unless (memq (nth i spec) ignored-keys)
                        collect (nth i spec)))
         (cached (auth-source-remembered-p spec))
         ;; note that we may have cached results but found is still nil
         ;; (there were no results from the search)
         (found (auth-source-recall spec))
         filtered-backends)

    (if (and cached auth-source-do-cache)
        (auth-source-do-debug
         "auth-source-search: found %d CACHED results matching %S"
         (length found) spec)

      (cl-assert
       (or (eq t create) (listp create)) t
       "Invalid auth-source :create parameter (must be t or a list): %s %s")

      (cl-assert
       (listp require) t
       "Invalid auth-source :require parameter (must be a list): %s")

      (setq filtered-backends (copy-sequence backends))
      (dolist (backend backends)
        (cl-dolist (key keys)
          ;; ignore invalid slots
          (condition-case nil
              (unless (auth-source-search-collection
                       (plist-get spec key)
                       (slot-value backend key))
                (setq filtered-backends (delq backend filtered-backends))
                (cl-return))
            (invalid-slot-name nil))))

      (auth-source-do-trivia
       "auth-source-search: found %d backends matching %S"
       (length filtered-backends) spec)

      ;; (debug spec "filtered" filtered-backends)
      ;; First go through all the backends without :create, so we can
      ;; query them all.
      (setq found (auth-source-search-backends filtered-backends
                                               spec
                                               ;; to exit early
                                               max
                                               ;; create is always nil here
                                               nil delete
                                               require))

      (auth-source-do-debug
       "auth-source-search: found %d results (max %d) matching %S"
       (length found) max spec)

      ;; If we didn't find anything, then we allow the backend(s) to
      ;; create the entries.
      (when (and create
                 (not found))
        (setq found (auth-source-search-backends filtered-backends
                                                 spec
                                                 ;; to exit early
                                                 max
                                                 create delete
                                                 require))
        (auth-source-do-debug
         "auth-source-search: CREATED %d results (max %d) matching %S"
         (length found) max spec))

      ;; note we remember the lack of result too, if it's applicable
      (when auth-source-do-cache
        (auth-source-remember spec found)))

    (if (zerop max)
        (not (null found))
      found)))

(defun auth-source-search-backends (backends spec max create delete require)
  (let ((max (if (zerop max) 1 max)) ; stop with 1 match if we're asked for zero
        matches)
    (dolist (backend backends)
      (when (> max (length matches)) ; if we need more matches...
        (let* ((bmatches (apply
                          (slot-value backend 'search-function)
                          :backend backend
                          :type (slot-value backend 'type)
                          ;; note we're overriding whatever the spec
                          ;; has for :max, :require, :create, and :delete
                          :max max
                          :require require
                          :create create
                          :delete delete
                          spec)))
          (when bmatches
            (auth-source-do-trivia
             "auth-source-search-backend: got %d (max %d) in %s:%s matching %S"
             (length bmatches) max
             (slot-value backend 'type)
             (slot-value backend 'source)
             spec)
            (setq matches (append matches bmatches))))))
    matches))

(defun auth-source-delete (&rest spec)
  "Delete entries from the authentication backends according to SPEC.
Calls `auth-source-search' with the :delete property in SPEC set to t.
The backend may not actually delete the entries.

Returns the deleted entries."
  (apply #'auth-source-search (plist-put spec :delete t)))

(defun auth-source-search-collection (collection value)
  "Return t if VALUE is t or COLLECTION is t or COLLECTION contains VALUE."
  (when (and (atom collection) (not (eq t collection)))
    (setq collection (list collection)))

  ;; (debug :collection collection :value value)
  (or (eq collection t)
      (eq value t)
      (equal collection value)
      (member value collection)))

(defvar auth-source-netrc-cache nil)

(defun auth-source-forget-all-cached ()
  "Forget all cached auth-source data."
  (interactive)
  (maphash (lambda (key _password)
             (when (eq 'auth-source (car-safe key))
               ;; remove that key
               (password-cache-remove key)))
           password-data)
  (setq auth-source-netrc-cache nil))

(defun auth-source-format-cache-entry (spec)
  "Format SPEC entry to put it in the password cache."
  `(auth-source . ,spec))

(defun auth-source-remember (spec found)
  "Remember FOUND search results for SPEC."
  (let ((password-cache-expiry auth-source-cache-expiry))
    (password-cache-add
     (auth-source-format-cache-entry spec) found)))

(defun auth-source-recall (spec)
  "Recall FOUND search results for SPEC."
  (password-read-from-cache (auth-source-format-cache-entry spec)))

(defun auth-source-remembered-p (spec)
  "Check if SPEC is remembered."
  (password-in-cache-p
   (auth-source-format-cache-entry spec)))

(defun auth-source-forget (spec)
  "Forget any cached data matching SPEC exactly.

This is the same SPEC you passed to `auth-source-search'.
Returns t or nil for forgotten or not found."
  (password-cache-remove (auth-source-format-cache-entry spec)))

(defun auth-source-forget+ (&rest spec)
  "Forget any cached data matching SPEC.  Return forgotten count.

This is not a full `auth-source-search' spec but works similarly.
For instance, \(:host \"myhost\" \"yourhost\") would find all the
cached data that was found with a search for those two hosts,
while \(:host t) would find all host entries."
  (let ((count 0))
    (maphash
     (lambda (key _password)
       (when (and (eq 'auth-source (car-safe key))
                  ;; and the spec matches what was stored in the cache
                  (auth-source-specmatchp spec (cdr key)))
         ;; remove that key
         (password-cache-remove key)
         (cl-incf count)))
     password-data)
    count))

(defun auth-source-specmatchp (spec stored)
  (let ((keys (cl-loop for i below (length spec) by 2
                       collect (nth i spec))))
    (not (eq
          (cl-dolist (key keys)
            (unless (auth-source-search-collection (plist-get stored key)
                                                   (plist-get spec key))
              (cl-return 'no)))
          'no))))

(defun auth-source-pick-first-password (&rest spec)
  "Pick the first secret found from applying SPEC to `auth-source-search'."
  (let* ((result (nth 0 (apply #'auth-source-search (plist-put spec :max 1))))
         (secret (plist-get result :secret)))

    (if (functionp secret)
        (funcall secret)
      secret)))

(defun auth-source-format-prompt (prompt alist)
  "Format PROMPT using %x (for any character x) specifiers in ALIST.
Remove trailing \": \"."
  (setq prompt (replace-regexp-in-string ":\\s-*$" "" prompt))
  (dolist (cell alist)
    (let ((c (nth 0 cell))
          (v (nth 1 cell)))
      (when (and c v)
        (setq prompt (replace-regexp-in-string (format "%%%c" c)
                                               (format "%s" v)
                                               prompt nil t)))))
  prompt)

(defun auth-source-ensure-strings (values)
  (if (eq values t)
      values
    (unless (listp values)
      (setq values (list values)))
    (mapcar (lambda (value)
	      (if (numberp value)
		  (format "%s" value)
		value))
	    values)))

;;; Backend specific parsing: netrc/authinfo backend

(defun auth-source--aput-1 (alist key val)
  (let ((seen ())
        (rest alist))
    (while (and (consp rest) (not (equal key (caar rest))))
      (push (pop rest) seen))
    (cons (cons key val)
          (if (null rest) alist
            (nconc (nreverse seen)
                   (if (equal key (caar rest)) (cdr rest) rest))))))
(defmacro auth-source--aput (var key val)
  `(setq ,var (auth-source--aput-1 ,var ,key ,val)))

(defun auth-source--aget (alist key)
  (cdr (assoc key alist)))

;; (auth-source-netrc-parse :file "~/.authinfo.gpg")
(cl-defun auth-source-netrc-parse (&key file max host user port require
                                   &allow-other-keys)
  "Parse FILE and return a list of all entries in the file.
Note that the MAX parameter is used so we can exit the parse early."
  (if (listp file)
      ;; We got already parsed contents; just return it.
      file
    (when (file-exists-p file)
      (setq port (auth-source-ensure-strings port))
      (with-temp-buffer
        (let* ((max (or max 5000))       ; sanity check: default to stop at 5K
               (modified 0)
               (cached (cdr-safe (assoc file auth-source-netrc-cache)))
               (cached-mtime (plist-get cached :mtime))
               (cached-secrets (plist-get cached :secret))
               (check (lambda(alist)
                        (and alist
                             (auth-source-search-collection
                              host
                              (or
                               (auth-source--aget alist "machine")
                               (auth-source--aget alist "host")
                               t))
                             (auth-source-search-collection
                              user
                              (or
                               (auth-source--aget alist "login")
                               (auth-source--aget alist "account")
                               (auth-source--aget alist "user")
                               t))
                             (auth-source-search-collection
                              port
                              (or
                               (auth-source--aget alist "port")
                               (auth-source--aget alist "protocol")
                               t))
                             (or
                              ;; the required list of keys is nil, or
                              (null require)
                              ;; every element of require is in n (normalized)
                              (let ((n (nth 0 (auth-source-netrc-normalize
                                               (list alist) file))))
                                (cl-loop for req in require
                                         always (plist-get n req)))))))
               result)

          (if (and (functionp cached-secrets)
                   (equal cached-mtime
                          (file-attribute-modification-time
                           (file-attributes file))))
              (progn
                (auth-source-do-trivia
                 "auth-source-netrc-parse: using CACHED file data for %s"
                 file)
                (insert (funcall cached-secrets)))
            (insert-file-contents file)
            ;; cache all netrc files (used to be just .gpg files)
            ;; Store the contents of the file obfuscated in memory.
            (auth-source--aput
             auth-source-netrc-cache file
             (list :mtime (file-attribute-modification-time
                           (file-attributes file))
                   :secret (let ((v (auth-source--obfuscate (buffer-string))))
                             (lambda () (auth-source--deobfuscate v))))))
          (goto-char (point-min))
          (let ((entries (auth-source-netrc-parse-entries check max))
                alist)
            (while (setq alist (pop entries))
                (push (nreverse alist) result)))

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
            (when (y-or-n-p (format "Save file %s? (%d deletions)"
                                    file modified))
              (write-region (point-min) (point-max) file nil 'silent)
              (auth-source-do-debug
               "auth-source-netrc-parse: modified %d lines in %s"
               modified file)))

          (nreverse result))))))

(defun auth-source-netrc-parse-next-interesting ()
  "Advance to the next interesting position in the current buffer."
  (skip-chars-forward "\t ")
  ;; If we're looking at a comment or are at the end of the line, move forward
  (while (or (eq (char-after) ?#)
             (and (eolp)
                  (not (eobp))))
    (forward-line 1)
    (skip-chars-forward "\t ")))

(defun auth-source-netrc-looking-at-token ()
  "Say whether the next think in the buffer is a token (password, etc).
Match data is altered to reflect the token."
  (or (looking-at "'\\([^']*\\)'")
      (looking-at "\"\\([^\"]*\\)\"")
      (looking-at "\\([^ \t\n]+\\)")))

(defun auth-source-netrc-parse-one ()
  "Read one thing from the current buffer."
  (auth-source-netrc-parse-next-interesting)

  (when (auth-source-netrc-looking-at-token)
    (forward-char (length (match-string 0)))
    (prog1
        (match-string-no-properties 1)
      (auth-source-netrc-parse-next-interesting))))

;; with thanks to org-mode
(defsubst auth-source-current-line (&optional pos)
  (save-excursion
    (and pos (goto-char pos))
    ;; works also in narrowed buffer, because we start at 1, not point-min
    (+ (if (bolp) 1 0) (count-lines 1 (point)))))

(defun auth-source-netrc-parse-entries(check max)
  "Parse up to MAX netrc entries, passed by CHECK, from the current buffer."
  (let ((adder (lambda(check alist all)
                 (when (and
                        alist
                        (> max (length all))
                        (funcall check alist))
                   (push alist all))
                 all))
        item item2 all alist default)
    (while (setq item (auth-source-netrc-parse-one))
      (setq default (equal item "default"))
      ;; We're starting a new machine.  Save the old one.
      (when (and alist
                 (or default
                     (equal item "machine")))
        ;; (auth-source-do-trivia
        ;;  "auth-source-netrc-parse-entries: got entry %S" alist)
        (setq all (funcall adder check alist all)
              alist nil))
      ;; In default entries, we don't have a next token.
      ;; We store them as ("machine" . t)
      (if default
          (push (cons "machine" t) alist)
        ;; Not a default entry.  Grab the next item.
        (when (setq item2 (auth-source-netrc-parse-one))
          ;; Did we get a "machine" value?
          (if (equal item2 "machine")
	      (error
	       "%s: Unexpected `machine' token at line %d"
	       "auth-source-netrc-parse-entries"
	       (auth-source-current-line))
            (push (cons item item2) alist)))))

    ;; Clean up: if there's an entry left over, use it.
    (when alist
      (setq all (funcall adder check alist all))
      ;; (auth-source-do-trivia
      ;;  "auth-source-netrc-parse-entries: got2 entry %S" alist)
      )
    (nreverse all)))

(defvar auth-source-passphrase-alist nil)

(defun auth-source-token-passphrase-callback-function (_context _key-id file)
  (let* ((file (file-truename file))
	 (entry (assoc file auth-source-passphrase-alist))
	 passphrase)
    ;; return the saved passphrase, calling a function if needed
    (or (copy-sequence (if (functionp (cdr entry))
			   (funcall (cdr entry))
			 (cdr entry)))
	(progn
	  (unless entry
	    (setq entry (list file))
	    (push entry auth-source-passphrase-alist))
	  (setq passphrase
		(read-passwd
		 (format "Passphrase for %s tokens: " file)
		 t))
	  (setcdr entry (let ((p (copy-sequence passphrase)))
			  (lambda () p)))
	  passphrase))))

(defun auth-source-epa-extract-gpg-token (secret file)
  "Pass either the decoded SECRET or the gpg:BASE64DATA version.
FILE is the file from which we obtained this token."
  (when (string-match "^gpg:\\(.+\\)" secret)
    (setq secret (base64-decode-string (match-string 1 secret))))
  (let ((context (epg-make-context 'OpenPGP)))
    (epg-context-set-passphrase-callback
     context
     (cons #'auth-source-token-passphrase-callback-function
           file))
    (epg-decrypt-string context secret)))

(defvar pp-escape-newlines)

(defun auth-source-epa-make-gpg-token (secret file)
  (let ((context (epg-make-context 'OpenPGP))
        (pp-escape-newlines nil)
        cipher)
    (setf (epg-context-armor context) t)
    (epg-context-set-passphrase-callback
     context
     (cons #'auth-source-token-passphrase-callback-function
           file))
    (setq cipher (epg-encrypt-string context secret nil))
    (with-temp-buffer
      (insert cipher)
      (base64-encode-region (point-min) (point-max) t)
      (concat "gpg:" (buffer-substring-no-properties
                      (point-min)
                      (point-max))))))

(defun auth-source--symbol-keyword (symbol)
  (intern (format ":%s" symbol)))

(defun auth-source-netrc-normalize (alist filename)
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

                  ;; Send back the secret in a function (lexical
                  ;; binding).  We slightly obfuscate the passwords
                  ;; (that's the "(mapcar #+' ..)" stuff) to avoid
                  ;; showing the passwords in clear text in backtraces
                  ;; and the like.
                  (when (equal k "secret")
                    (setq v (let ((lexv (auth-source--obfuscate v))
                                  (token-decoder nil))
                              (when (string-match "^gpg:" v)
                                ;; it's a GPG token: create a token decoder
                                ;; which unsets itself once
                                (setq token-decoder
                                      (lambda (val)
                                        (prog1
                                            (auth-source-epa-extract-gpg-token
                                             val
                                             filename)
                                          (setq token-decoder nil)))))
                              (lambda ()
                                (if token-decoder
                                    (funcall token-decoder
                                             (auth-source--deobfuscate lexv))
                                  (auth-source--deobfuscate lexv))))))
                  (setq ret (plist-put ret
                                       (auth-source--symbol-keyword k)
                                       v))))
              ret))
          alist))

;; Never change this variable.
(defvar auth-source--session-nonce nil)

(defun auth-source--obfuscate (string)
  ;; We want to keep passwords out of backtraces and bug reports and
  ;; the like, so if we have GnuTLS available, we encrypt them with a
  ;; nonce that we just keep in memory.  If somebody has access to the
  ;; current Emacs session, they can be decrypted, but if not, little
  ;; useful information is leaked.  If you reset the nonce, you also
  ;; have to call `auth-source-forget-all-cached'.
  (unless auth-source--session-nonce
    (setq auth-source--session-nonce
          (apply #'string (cl-loop repeat 16
                                   collect (random 128)))))
  (if (and (fboundp 'gnutls-symmetric-encrypt)
           (gnutls-available-p))
      (let ((cdata (car (last (gnutls-ciphers)))))
        (mapconcat
         #'base64-encode-string
         (gnutls-symmetric-encrypt
          (pop cdata)
          (auth-source--pad auth-source--session-nonce
                            (plist-get cdata :cipher-keysize))
          (list 'iv-auto (plist-get cdata :cipher-ivsize))
          (auth-source--pad (encode-coding-string string 'utf-8)
                            (plist-get cdata :cipher-blocksize)))
         "-"))
    (mapcar #'1- string)))

(defun auth-source--pad (string length)
  "Pad STRING to a modulo of LENGTH."
  (let ((pad (- length (mod (length string) length))))
    (concat string (make-string pad pad))))

(defun auth-source--unpad (string)
  "Remove PKCS#7 padding from STRING."
  (substring string 0 (- (length string)
			 (aref string (1- (length string))))))

(defun auth-source--deobfuscate (data)
  (if (and (fboundp 'gnutls-symmetric-encrypt)
           (gnutls-available-p))
      (let ((cdata (car (last (gnutls-ciphers))))
            (bits (split-string data "-")))
        (decode-coding-string
         (auth-source--unpad
          (car
           (gnutls-symmetric-decrypt
            (pop cdata)
            (auth-source--pad auth-source--session-nonce
                              (plist-get cdata :cipher-keysize))
            (base64-decode-string (cadr bits))
            (base64-decode-string (car bits)))))
         'utf-8))
    (apply #'string (mapcar #'1+ data))))

(cl-defun auth-source-netrc-search (&rest spec
                                    &key backend require create
                                    type max host user port
                                    &allow-other-keys)
  "Given a property list SPEC, return search matches from the `:backend'.
See `auth-source-search' for details on SPEC."
  ;; just in case, check that the type is correct (null or same as the backend)
  (cl-assert (or (null type) (eq type (oref backend type)))
             t "Invalid netrc search: %s %s")

  (let ((results (auth-source-netrc-normalize
                  (auth-source-netrc-parse
                   :max max
                   :require require
                   :file (oref backend source)
                   :host (or host t)
                   :user (or user t)
                   :port (or port t))
                  (oref backend source))))

    ;; if we need to create an entry AND none were found to match
    (when (and create
               (not results))

      ;; create based on the spec and record the value
      (setq results (or
                     ;; if the user did not want to create the entry
                     ;; in the file, it will be returned
                     (apply (slot-value backend 'create-function) spec)
                     ;; if not, we do the search again without :create
                     ;; to get the updated data.

                     ;; the result will be returned, even if the search fails
                     (apply #'auth-source-netrc-search
                            (plist-put spec :create nil)))))
    results))

(defun auth-source-netrc-element-or-first (v)
  (if (listp v)
      (nth 0 v)
    v))

;; (auth-source-search :host "nonesuch" :type 'netrc :max 1 :create t)
;; (auth-source-search :host "nonesuch" :type 'netrc :max 1 :create t :create-extra-keys '((A "default A") (B)))

(cl-defun auth-source-netrc-create (&rest spec
                                    &key backend host port create user
                                    &allow-other-keys)
  (let* ((base-required '(host user port secret))
         ;; we know (because of an assertion in auth-source-search) that the
         ;; :create parameter is either t or a list (which includes nil)
         (create-extra (if (eq t create) nil create))
         (current-data (car (auth-source-search :max 1
                                                :host host
                                                :user user
                                                :port port)))
         (required (append base-required create-extra))
         (file (oref backend source))
         (add "")
         ;; Whether to set save-function.
         save-function
         ;; `valist' is an alist
         valist
         ;; `artificial' will be returned if no creation is needed
         artificial)

    ;; only for base required elements (defined as function parameters):
    ;; fill in the valist with whatever data we may have from the search
    ;; we complete the first value if it's a list and use the value otherwise
    (dolist (br base-required)
      (let ((val (plist-get spec (auth-source--symbol-keyword br))))
        (when val
          (let ((br-choice (cond
                            ;; all-accepting choice (predicate is t)
                            ((eq t val) nil)
                            ;; just the value otherwise
                            (t val))))
            (when br-choice
              (auth-source--aput valist br br-choice))))))

    ;; for extra required elements, see if the spec includes a value for them
    (dolist (er create-extra)
      (let ((k (auth-source--symbol-keyword er))
            (keys (cl-loop for i below (length spec) by 2
                           collect (nth i spec))))
        (when (memq k keys)
          (auth-source--aput valist er (plist-get spec k)))))

    ;; for each required element
    (dolist (r required)
      (let* ((data (auth-source--aget valist r))
             ;; take the first element if the data is a list
             (data (or (auth-source-netrc-element-or-first data)
                       (plist-get current-data
                                  (auth-source--symbol-keyword r))))
             ;; this is the default to be offered
             (given-default (auth-source--aget
                             auth-source-creation-defaults r))
             ;; the default supplementals are simple:
             ;; for the user, try `given-default' and then (user-login-name);
             ;; otherwise take `given-default'
             (default (cond
                       ((and (not given-default) (eq r 'user))
                        (user-login-name))
                       (t given-default)))
             (printable-defaults (list
                                  (cons 'user
                                        (or
                                         (auth-source-netrc-element-or-first
                                          (auth-source--aget valist 'user))
                                         (plist-get artificial :user)
                                         "[any user]"))
                                  (cons 'host
                                        (or
                                         (auth-source-netrc-element-or-first
                                          (auth-source--aget valist 'host))
                                         (plist-get artificial :host)
                                         "[any host]"))
                                  (cons 'port
                                        (or
                                         (auth-source-netrc-element-or-first
                                          (auth-source--aget valist 'port))
                                         (plist-get artificial :port)
                                         "[any port]"))))
             (prompt (or (auth-source--aget auth-source-creation-prompts r)
                         (cl-case r
                           (secret "%p password for %u@%h")
                           (user "%p user name for %h")
                           (host "%p host name for user %u")
                           (port "%p port for %u@%h"))
                         (format "Enter %s (%%u@%%h:%%p)" r)))
             (prompt (auth-source-format-prompt
                      prompt
                      `((?u ,(auth-source--aget printable-defaults 'user))
                        (?h ,(auth-source--aget printable-defaults 'host))
                        (?p ,(auth-source--aget printable-defaults 'port))))))

        ;; Store the data, prompting for the password if needed.
        (setq data (or data
                       (if (eq r 'secret)
                           ;; Special case prompt for passwords.
                           ;; TODO: make the default (setq auth-source-netrc-use-gpg-tokens `((,(if (boundp 'epa-file-auto-mode-alist-entry) (car epa-file-auto-mode-alist-entry) "\\.gpg\\'") nil) (t gpg)))
                           ;; TODO: or maybe leave as (setq auth-source-netrc-use-gpg-tokens 'never)
                           (let* ((ep (format "Use GPG password tokens in %s?" file))
                                  (gpg-encrypt
                                   (cond
                                    ((eq auth-source-netrc-use-gpg-tokens 'never)
                                     'never)
                                    ((listp auth-source-netrc-use-gpg-tokens)
                                     (let ((check (copy-sequence
                                                   auth-source-netrc-use-gpg-tokens))
                                           item ret)
                                       (while check
                                         (setq item (pop check))
                                         (when (or (eq (car item) t)
                                                   (string-match (car item) file))
                                           (setq ret (cdr item))
                                           (setq check nil)))
                                       ret))
                                    (t 'never)))
                                  (plain
                                   (or (eval default)
                                       (read-passwd (format-prompt prompt nil)))))
                             ;; ask if we don't know what to do (in which case
                             ;; auth-source-netrc-use-gpg-tokens must be a list)
                             (unless gpg-encrypt
                               (setq gpg-encrypt (if (y-or-n-p ep) 'gpg 'never))
                               ;; TODO: save the defcustom now? or ask?
                               (setq auth-source-netrc-use-gpg-tokens
                                     (cons `(,file ,gpg-encrypt)
                                           auth-source-netrc-use-gpg-tokens)))
                             (if (eq gpg-encrypt 'gpg)
                                 (auth-source-epa-make-gpg-token plain file)
                               plain))
                         (if (and (stringp default) auth-source-save-behavior)
                             (read-string
                              (format-prompt prompt default) nil nil default)
                           (eval default)))))

        (when data
          (setq artificial (plist-put artificial
                                      (auth-source--symbol-keyword r)
                                      (if (eq r 'secret)
                                          (let ((data data))
                                            (lambda () data))
                                        data))))

        ;; When r is not an empty string...
        (when (and (stringp data)
                   (< 0 (length data)))
          (when (eq r 'secret)
            (setq save-function t))
          ;; this function is not strictly necessary but I think it
          ;; makes the code clearer -tzz
          (let ((printer (lambda ()
                           ;; append the key (the symbol name of r)
                           ;; and the value in r
                           (format "%s%s %s"
                                   ;; prepend a space
                                   (if (zerop (length add)) "" " ")
                                   ;; remap auth-source tokens to netrc
                                   (cl-case r
                                     (user   "login")
                                     (host   "machine")
                                     (secret "password")
                                     (port   "port") ; redundant but clearer
                                     (t (symbol-name r)))
                                   (if (string-match "[\"# ]" data)
                                       (format "%S" data)
                                     data)))))
            (setq add (concat add (funcall printer)))))))

    (when save-function
      (plist-put
       artificial
       :save-function
       (let ((file file)
             (add add))
         (lambda () (auth-source-netrc-saver file add)))))

    (list artificial)))

(defun auth-source-netrc-saver (file add)
  "Save a line ADD in FILE, prompting along the way.
Respects `auth-source-save-behavior'.  Uses
`auth-source-netrc-cache' to avoid prompting more than once."
  (let* ((key (format "%s %s" file (rfc2104-hash 'md5 64 16 file add)))
         (cached (assoc key auth-source-netrc-cache)))

    (if cached
        (auth-source-do-trivia
         "auth-source-netrc-saver: found previous run for key %s, returning"
         key)
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
        ;; we want the new data to be found first, so insert at beginning
        (goto-char (point-min))

        ;; Ask AFTER we've successfully opened the file.
        (let ((prompt (format "Save auth info to file %s? " file))
              (done (not (eq auth-source-save-behavior 'ask)))
              (bufname "*auth-source Help*")
              k)
          (while (not done)
            (setq k (auth-source-read-char-choice prompt '(?y ?n ?N ?e ??)))
            (cl-case k
              (?y (setq done t))
              (?? (save-excursion
                    (with-output-to-temp-buffer bufname
                      (princ
                       (concat "(y)es, save\n"
                               "(n)o but use the info\n"
                               "(N)o and don't ask to save again\n"
                               "(e)dit the line\n"
                               "(?) for help as you can see.\n"))
                      ;; Why?  Doesn't with-output-to-temp-buffer already do
                      ;; the exact same thing anyway?  --Stef
                      (set-buffer standard-output)
                      (help-mode))))
              (?n (setq add ""
                        done t))
              (?N
               (setq add ""
                     done t)
               (customize-save-variable 'auth-source-save-behavior nil))
              (?e (setq add (read-string "Line to add: " add)))
              (t nil)))

          (when (get-buffer-window bufname)
            (delete-window (get-buffer-window bufname)))

          ;; Make sure the info is not saved.
          (when (null auth-source-save-behavior)
            (setq add ""))

          (when (< 0 (length add))
            (progn
              (unless (bolp)
                (insert "\n"))
              (insert add "\n")
              (write-region (point-min) (point-max) file nil 'silent)
	      ;; Make the .authinfo file non-world-readable.
	      (set-file-modes file #o600)
              (auth-source-do-debug
               "auth-source-netrc-create: wrote 1 new line to %s"
               file)
              (message "Saved new authentication information to %s" file)
              nil))))
      (auth-source--aput auth-source-netrc-cache key "ran"))))

;;; Backend specific parsing: Secrets API backend

(defun auth-source-secrets-listify-pattern (pattern)
  "Convert a PATTERN with lists to a list of string patterns.

auth-source patterns can have values of the form :foo (\"bar\"
\"qux\"), which means to match any secret with :foo equal to
\"bar\" or :foo equal to \"qux\".  The secrets backend supports
only string values for patterns, so this routine returns a list
of patterns that is equivalent to the single original PATTERN
when interpreted such that if a secret matches any pattern in the
list, it matches the original PATTERN."
  (if (null pattern)
      '(nil)
    (let* ((key (pop pattern))
           (value (pop pattern))
           (tails (auth-source-secrets-listify-pattern pattern))
           (heads (if (stringp value)
                      (list (list key value))
                    (mapcar (lambda (v) (list key v)) value))))
      (cl-loop for h in heads
               nconc (cl-loop for tl in tails collect (append h tl))))))

(cl-defun auth-source-secrets-search (&rest spec
                                      &key backend create delete label max
                                      &allow-other-keys)
  "Search the Secrets API; spec is like `auth-source'.

The :label key specifies the item's label.  It is the only key
that can specify a substring.  Any :label value besides a string
will allow any label.

All other search keys must match exactly.  If you need substring
matching, do a wider search and narrow it down yourself.

You'll get back all the properties of the token as a plist.

Here's an example that looks for the first item in the `Login'
Secrets collection:

 (let ((auth-sources \\='(\"secrets:Login\")))
    (auth-source-search :max 1))

Here's another that looks for the first item in the `Login'
Secrets collection whose label contains `gnus':

 (let ((auth-sources \\='(\"secrets:Login\")))
    (auth-source-search :max 1 :label \"gnus\"))

And this one looks for the first item in the `Login' Secrets
collection that's a Google Chrome entry for the git.gnus.org site
authentication tokens:

 (let ((auth-sources \\='(\"secrets:Login\")))
    (auth-source-search :max 1 :signon_realm \"https://git.gnus.org/Git\"))"

  ;; TODO
  ;; (secrets-delete-item coll elt)
  (cl-assert (not delete) nil
             "The Secrets API auth-source backend doesn't support deletion yet")

  (let* ((coll (oref backend source))
         (max (or max 5000))     ; sanity check: default to stop at 5K
         (ignored-keys '(:create :delete :max :backend :label :require :type))
         (search-keys (cl-loop for i below (length spec) by 2
                               unless (memq (nth i spec) ignored-keys)
                               collect (nth i spec)))
         ;; build a search spec without the ignored keys
         ;; if a search key is nil or t (match anything), we skip it
         (search-specs (auth-source-secrets-listify-pattern
                        (apply #'append (mapcar
                                      (lambda (k)
                                        (if (or (null (plist-get spec k))
                                                (eq t (plist-get spec k)))
                                            nil
                                          (list k (plist-get spec k))))
                                      search-keys))))
         ;; needed keys (always including host, login, port, and secret)
         (returned-keys (delete-dups (append
				      '(:host :login :port :secret)
				      search-keys)))
         (items
          (cl-loop
           for search-spec in search-specs
           nconc
           (cl-loop for item in (apply #'secrets-search-items coll search-spec)
                    unless (and (stringp label)
                                (not (string-match label item)))
                    collect item)))
         ;; TODO: respect max in `secrets-search-items', not after the fact
         (items (butlast items (- (length items) max)))
         ;; convert the item name to a full plist
         (items (mapcar (lambda (item)
                          (append
                           ;; make an entry for the secret (password) element
                           (list
                            :secret
                            (let ((v (secrets-get-secret coll item)))
                              (lambda () v)))
                           ;; rewrite the entry from ((k1 v1) (k2 v2)) to plist
                           (apply #'append
                                  (mapcar (lambda (entry)
                                            (list (car entry) (cdr entry)))
                                          (secrets-get-attributes coll item)))))
                        items))
         ;; ensure each item has each key in `returned-keys'
         (items (mapcar (lambda (plist)
                          (append
                           (apply #'append
                                  (mapcar (lambda (req)
                                            (if (plist-get plist req)
                                                nil
                                              (list req nil)))
                                          returned-keys))
                           plist))
                        items)))
    (cond
     ;; if we need to create an entry AND none were found to match
     ((and create
           (not items))

      ;; create based on the spec and record the value
      (setq items (or
                   ;; if the user did not want to create the entry
                   ;; in the file, it will be returned
                   (apply (slot-value backend 'create-function) spec)
                   ;; if not, we do the search again without :create
                   ;; to get the updated data.

                   ;; the result will be returned, even if the search fails
                   (apply #'auth-source-secrets-search
                          (plist-put spec :create nil))))))
    items))

(cl-defun auth-source-secrets-create (&rest spec
                                      &key backend host port create
                                      &allow-other-keys)
  (let* ((base-required '(host user port secret label))
         ;; we know (because of an assertion in auth-source-search) that the
         ;; :create parameter is either t or a list (which includes nil)
         (create-extra (if (eq t create) nil create))
         (current-data (car (auth-source-search :max 1
                                                :host host
                                                :port port)))
         (required (append base-required create-extra))
         (collection (oref backend source))
         ;; Whether to set save-function.
         save-function
         ;; `args' are the arguments for `secrets-create-item'.
         args
         ;; `valist' is an alist
         valist
         ;; `artificial' will be returned if no creation is needed
         artificial)

    ;; only for base required elements (defined as function parameters):
    ;; fill in the valist with whatever data we may have from the search
    ;; we complete the first value if it's a list and use the value otherwise
    (dolist (br base-required)
      (let ((val (plist-get spec (auth-source--symbol-keyword br))))
        (when val
          (let ((br-choice (cond
                            ;; all-accepting choice (predicate is t)
                            ((eq t val) nil)
                            ;; just the value otherwise
                            (t val))))
            (when br-choice
              (auth-source--aput valist br br-choice))))))

    ;; for extra required elements, see if the spec includes a value for them
    (dolist (er create-extra)
      (let ((k (auth-source--symbol-keyword er))
            (keys (cl-loop for i below (length spec) by 2
                           collect (nth i spec))))
        (when (memq k keys)
          (auth-source--aput valist er (plist-get spec k)))))

    ;; for each required element
    (dolist (r required)
      (let* ((data (auth-source--aget valist r))
             ;; take the first element if the data is a list
             (data (or (auth-source-netrc-element-or-first data)
                       (plist-get current-data
                                  (auth-source--symbol-keyword r))))
             ;; this is the default to be offered
             (given-default (auth-source--aget
                             auth-source-creation-defaults r))
             ;; the default supplementals are simple:
             ;; for the user, try `given-default' and then (user-login-name);
             ;; for the label, try `given-default' and then user@host;
             ;; otherwise take `given-default'
             (default (cond
                       ((and (not given-default) (eq r 'user))
                        (user-login-name))
                       ((and (not given-default) (eq r 'label))
                        (format "%s@%s"
                                (or (auth-source-netrc-element-or-first
                                     (auth-source--aget valist 'user))
                                    (plist-get artificial :user))
                                (or (auth-source-netrc-element-or-first
                                     (auth-source--aget valist 'host))
                                    (plist-get artificial :host))))
                       (t given-default)))
             (printable-defaults (list
                                  (cons 'user
                                        (or
                                         (auth-source-netrc-element-or-first
                                          (auth-source--aget valist 'user))
                                         (plist-get artificial :user)
                                         "[any user]"))
                                  (cons 'host
                                        (or
                                         (auth-source-netrc-element-or-first
                                          (auth-source--aget valist 'host))
                                         (plist-get artificial :host)
                                         "[any host]"))
                                  (cons 'port
                                        (or
                                         (auth-source-netrc-element-or-first
                                          (auth-source--aget valist 'port))
                                         (plist-get artificial :port)
                                         "[any port]"))
                                  (cons 'label
                                        (or
                                         (auth-source-netrc-element-or-first
                                          (auth-source--aget valist 'label))
                                         (plist-get artificial :label)
                                         "[any label]"))))
             (prompt (or (auth-source--aget auth-source-creation-prompts r)
                         (cl-case r
                           (secret "%p password for %u@%h")
                           (user "%p user name for %h")
                           (host "%p host name for user %u")
                           (port "%p port for %u@%h")
                           (label "Enter label for %u@%h"))
                         (format "Enter %s (%%u@%%h:%%p)" r)))
             (prompt (auth-source-format-prompt
                      prompt
                      `((?u ,(auth-source--aget printable-defaults 'user))
                        (?h ,(auth-source--aget printable-defaults 'host))
                        (?p ,(auth-source--aget printable-defaults 'port))))))

        ;; Store the data, prompting for the password if needed.
        (setq data (or data
                       (if (eq r 'secret)
                           (or (eval default)
                               (read-passwd  (format-prompt prompt nil)))
                         (if (and (stringp default) auth-source-save-behavior)
                             (read-string
                              (format-prompt prompt default) nil nil default)
                           (eval default)))))

        (when data
          (setq artificial (plist-put artificial
                                      (auth-source--symbol-keyword r)
                                      (if (eq r 'secret)
                                          (let ((data data))
                                            (lambda () data))
                                        data))))

        ;; When r is not an empty string...
        (when (and (stringp data)
                   (< 0 (length data)))
          (if (eq r 'secret)
              (setq save-function t)
            (if (not (eq r 'label))
                ;; append the key (the symbol name of r)
                ;; and the value in r
                (setq args (append args (list (auth-source--symbol-keyword r) data))))))))

    (when save-function
      (plist-put
       artificial
       :save-function
       (let* ((collection collection)
              (item (plist-get artificial :label))
              (secret (plist-get artificial :secret))
              (secret (if (functionp secret) (funcall secret) secret)))
         (lambda ()
	   (auth-source-secrets-saver collection item secret args)))))

    (list artificial)))

(defun auth-source-secrets-saver (collection item secret args)
  "Wrapper around `secrets-create-item', prompting along the way.
Respects `auth-source-save-behavior'."
  (let ((prompt (format "Save auth info to secrets collection %s? " collection))
        (done (not (eq auth-source-save-behavior 'ask)))
        (doit (eq auth-source-save-behavior t))
        (bufname "*auth-source Help*")
        k)
    (while (not done)
      (setq k (auth-source-read-char-choice prompt '(?y ?n ?N ??)))
      (cl-case k
        (?y (setq done t doit t))
        (?? (save-excursion
              (with-output-to-temp-buffer bufname
                (princ
                 (concat "(y)es, save\n"
                         "(n)o but use the info\n"
                         "(N)o and don't ask to save again\n"
                         "(?) for help as you can see.\n"))
                ;; Why?  Doesn't with-output-to-temp-buffer already do
                ;; the exact same thing anyway?  --Stef
                (set-buffer standard-output)
                (help-mode))))
        (?n (setq done t doit nil))
        (?N (setq done t doit nil)
            (customize-save-variable 'auth-source-save-behavior nil))
        (t nil)))

    (when doit
      (progn
        (auth-source-do-debug
         "secrets-create-item: wrote 1 new item to %s" collection)
        (message "Saved new authentication information to %s" collection)
	(apply 'secrets-create-item collection item secret args)))))

;;; Backend specific parsing: Mac OS Keychain (using /usr/bin/security) backend

(cl-defun auth-source-macos-keychain-search (&rest spec
                                             &key backend create delete type max
                                             &allow-other-keys)
  "Search the macOS Keychain; spec is like `auth-source'.

All search keys must match exactly.  If you need substring
matching, do a wider search and narrow it down yourself.

You'll get back all the properties of the token as a plist.

The :type key is either `macos-keychain-internet' or
`macos-keychain-generic'.

For the internet keychain type, the :label key searches the
item's labels (\"-l LABEL\" passed to \"/usr/bin/security\").
Similarly, :host maps to \"-s HOST\", :user maps to \"-a USER\",
and :port maps to \"-P PORT\" or \"-r PROT\"
\(note PROT has to be a 4-character string).

For the generic keychain type, the :label key searches the item's
labels (\"-l LABEL\" passed to \"/usr/bin/security\").
Similarly, :host maps to \"-c HOST\" (the \"creator\" keychain
field), :user maps to \"-a USER\", and :port maps to \"-s PORT\".

Here's an example that looks for the first item in the default
generic macOS Keychain:

 (let ((auth-sources \\='(macos-keychain-generic)))
    (auth-source-search :max 1)

Here's another that looks for the first item in the internet
macOS Keychain collection whose label is `gnus':

 (let ((auth-sources \\='(macos-keychain-internet)))
    (auth-source-search :max 1 :label \"gnus\")

And this one looks for the first item in the internet keychain
entries for git.gnus.org:

 (let ((auth-sources \\='(macos-keychain-internet\")))
    (auth-source-search :max 1 :host \"git.gnus.org\"))"
  ;; TODO
  (cl-assert (not create) nil
          "The macOS Keychain auth-source backend doesn't support creation yet")
  ;; TODO
  ;; (macos-keychain-delete-item coll elt)
  (cl-assert (not delete) nil
          "The macOS Keychain auth-source backend doesn't support deletion yet")

  (let* ((coll (oref backend source))
         (max (or max 5000))     ; sanity check: default to stop at 5K
         ;; Filter out ignored keys from the spec
         (ignored-keys '(:create :delete :max :backend :label :host :port))
         ;; Build a search spec without the ignored keys
         ;; FIXME make this loop a function? it's used in at least 3 places
         (search-keys (cl-loop for i below (length spec) by 2
                               unless (memq (nth i spec) ignored-keys)
                               collect (nth i spec)))
         ;; If a search key value is nil or t (match anything), we skip it
         (search-spec (apply #'append (mapcar
                                      (lambda (k)
                                        (if (or (null (plist-get spec k))
                                                (eq t (plist-get spec k)))
                                            nil
                                          (list k (plist-get spec k))))
                                      search-keys)))
         ;; needed keys (always including host, login, port, and secret)
         (returned-keys (delete-dups (append
				      '(:host :login :port :secret)
				      search-keys)))
         ;; Extract host and port from spec
         (hosts (plist-get spec :host))
         (hosts (if (and hosts (listp hosts)) hosts `(,hosts)))
         (ports (plist-get spec :port))
         (ports (if (and ports (listp ports)) ports `(,ports)))
         ;; Loop through all combinations of host/port and pass each of these to
         ;; auth-source-macos-keychain-search-items
         (items (catch 'match
                  (dolist (host hosts)
                    (dolist (port ports)
                      (let* ((port (if port (format "%S" port)))
                             (items (apply #'auth-source-macos-keychain-search-items
                                           coll
                                           type
                                           max
                                           host port
                                           search-spec)))
                        (when items
                          (throw 'match items)))))))

         ;; ensure each item has each key in `returned-keys'
         (items (mapcar (lambda (plist)
                          (append
                           (apply #'append
                                  (mapcar (lambda (req)
                                            (if (plist-get plist req)
                                                nil
                                              (list req nil)))
                                          returned-keys))
                           plist))
                        items)))
    items))


(defun auth-source--decode-octal-string (string)
  "Convert octal STRING to utf-8 string.  E.g: 'a\134b' to 'a\b'."
  (let ((list (string-to-list string))
        (size (length string)))
    (decode-coding-string
     (apply #'unibyte-string
            (cl-loop for i = 0 then (+ i (if (eq (nth i list) ?\\) 4 1))
                     for var = (nth i list)
                     while (< i size)
                     if (eq var ?\\)
                     collect (string-to-number
                              (concat (cl-subseq list (+ i 1) (+ i 4))) 8)
                     else
                     collect var))
     'utf-8)))

(cl-defun auth-source-macos-keychain-search-items (coll _type _max host port
                                                   &key label type user
                                                   &allow-other-keys)
  (let* ((keychain-generic (eq type 'macos-keychain-generic))
         (args `(,(if keychain-generic
                      "find-generic-password"
                    "find-internet-password")
                 "-g"))
         (ret (list :type type)))
    (when label
      (setq args (append args (list "-l" label))))
    (when host
      (setq args (append args (list (if keychain-generic "-c" "-s") host))))
    (when user
      (setq args (append args (list "-a" user))))

    (when port
      (if keychain-generic
          (setq args (append args (list "-s" port)))
        (setq args (append args (list
                                 (if (string-match "[0-9]+" port) "-P" "-r")
                                 port)))))

      (unless (equal coll "default")
        (setq args (append args (list coll))))

      (with-temp-buffer
        (apply #'call-process "/usr/bin/security" nil t nil args)
        (goto-char (point-min))
        (while (not (eobp))
          (cond
           ((looking-at "^password: \\(?:0x[0-9A-F]+\\)? *\"\\(.+\\)\"")
            (setq ret (auth-source-macos-keychain-result-append
                       ret
                       keychain-generic
                       "secret"
                       (let ((v (auth-source--decode-octal-string
                                 (match-string 1))))
                         (lambda () v)))))
           ;; TODO: check if this is really the label
           ;; match 0x00000007 <blob>="AppleID"
           ((looking-at
             "^[ ]+0x00000007 <blob>=\\(?:0x[0-9A-F]+\\)? *\"\\(.+\\)\"")
            (setq ret (auth-source-macos-keychain-result-append
                       ret
                       keychain-generic
                       "label"
                       (auth-source--decode-octal-string (match-string 1)))))
           ;; match "crtr"<uint32>="aapl"
           ;; match "svce"<blob>="AppleID"
           ((looking-at
             "^[ ]+\"\\([a-z]+\\)\"[^=]+=\\(?:0x[0-9A-F]+\\)? *\"\\(.+\\)\"")
            (setq ret (auth-source-macos-keychain-result-append
                       ret
                       keychain-generic
                       (auth-source--decode-octal-string (match-string 1))
                       (auth-source--decode-octal-string (match-string 2))))))
          (forward-line)))
      ;; return `ret' iff it has the :secret key
      (and (plist-get ret :secret) (list ret))))

(defun auth-source-macos-keychain-result-append (result generic k v)
  (push v result)
  (push (auth-source--symbol-keyword
         (cond
          ((equal k "acct") "user")
          ;; for generic keychains, creator is host, service is port
          ((and generic (equal k "crtr")) "host")
          ((and generic (equal k "svce")) "port")
          ;; for internet keychains, protocol is port, server is host
          ((and (not generic) (equal k "ptcl")) "port")
          ((and (not generic) (equal k "srvr")) "host")
          (t k)))
        result))

(defun auth-source-macos-keychain-create (&rest spec)
  ;; TODO
  (debug spec))

;;; Backend specific parsing: PLSTORE backend

(cl-defun auth-source-plstore-search (&rest spec
                                      &key backend create delete max
                                      &allow-other-keys)
  "Search the PLSTORE; SPEC is like `auth-source'."
  (let* ((store (oref backend data))
         (max (or max 5000))     ; sanity check: default to stop at 5K
         (ignored-keys '(:create :delete :max :backend :label :require :type))
         (search-keys (cl-loop for i below (length spec) by 2
                               unless (memq (nth i spec) ignored-keys)
                               collect (nth i spec)))
         ;; build a search spec without the ignored keys
         ;; if a search key is nil or t (match anything), we skip it
         (search-spec (apply #'append (mapcar
                                      (lambda (k)
                                        (let ((v (plist-get spec k)))
                                          (if (or (null v)
                                                  (eq t v))
                                              nil
                                            (list
                                             k
                                             (auth-source-ensure-strings v)))))
                                      search-keys)))
         ;; needed keys (always including host, login, port, and secret)
         (returned-keys (delete-dups (append
				      '(:host :login :port :secret)
				      search-keys)))
         (items (plstore-find store search-spec))
         (item-names (mapcar #'car items))
         (items (butlast items (- (length items) max)))
         ;; convert the item to a full plist
         (items (mapcar (lambda (item)
                          (let* ((plist (copy-tree (cdr item)))
                                 (secret (plist-member plist :secret)))
                            (if secret
                                (setcar
                                 (cdr secret)
                                 (let ((v (car (cdr secret))))
                                   (if (functionp v)
                                       (lambda () (funcall v plist))
                                   (lambda () v)))))
                            plist))
                        items))
         ;; ensure each item has each key in `returned-keys'
         (items (mapcar (lambda (plist)
                          (append
                           (apply #'append
                                  (mapcar (lambda (req)
                                            (if (plist-get plist req)
                                                nil
                                              (list req nil)))
                                          returned-keys))
                           plist))
                        items)))
    (cond
     ;; if we need to create an entry AND none were found to match
     ((and create
           (not items))

      ;; create based on the spec and record the value
      (setq items (or
                   ;; if the user did not want to create the entry
                   ;; in the file, it will be returned
                   (apply (slot-value backend 'create-function) spec)
                   ;; if not, we do the search again without :create
                   ;; to get the updated data.

                   ;; the result will be returned, even if the search fails
                   (apply #'auth-source-plstore-search
                          (plist-put spec :create nil)))))
     ((and delete
           item-names)
      (dolist (item-name item-names)
        (plstore-delete store item-name))
      (plstore-save store)))
    items))

(cl-defun auth-source-plstore-create (&rest spec
                                      &key backend host port create
                                      &allow-other-keys)
  (let* ((base-required '(host user port secret))
         (base-secret '(secret))
         ;; we know (because of an assertion in auth-source-search) that the
         ;; :create parameter is either t or a list (which includes nil)
         (create-extra (if (eq t create) nil create))
         (current-data (car (auth-source-search :max 1
                                                :host host
                                                :port port)))
         (required (append base-required create-extra))
         ;; `valist' is an alist
         valist
         ;; `artificial' will be returned if no creation is needed
         artificial
         secret-artificial)

    ;; only for base required elements (defined as function parameters):
    ;; fill in the valist with whatever data we may have from the search
    ;; we complete the first value if it's a list and use the value otherwise
    (dolist (br base-required)
      (let ((val (plist-get spec (auth-source--symbol-keyword br))))
        (when val
          (let ((br-choice (cond
                            ;; all-accepting choice (predicate is t)
                            ((eq t val) nil)
                            ;; just the value otherwise
                            (t val))))
            (when br-choice
              (auth-source--aput valist br br-choice))))))

    ;; for extra required elements, see if the spec includes a value for them
    (dolist (er create-extra)
      (let ((k (auth-source--symbol-keyword er))
            (keys (cl-loop for i below (length spec) by 2
                           collect (nth i spec))))
        (when (memq k keys)
          (auth-source--aput valist er (plist-get spec k)))))

    ;; for each required element
    (dolist (r required)
      (let* ((data (auth-source--aget valist r))
             ;; take the first element if the data is a list
             (data (or (auth-source-netrc-element-or-first data)
                       (plist-get current-data
                                  (auth-source--symbol-keyword r))))
             ;; this is the default to be offered
             (given-default (auth-source--aget
                             auth-source-creation-defaults r))
             ;; the default supplementals are simple:
             ;; for the user, try `given-default' and then (user-login-name);
             ;; otherwise take `given-default'
             (default (cond
                       ((and (not given-default) (eq r 'user))
                        (user-login-name))
                       (t given-default)))
             (printable-defaults (list
                                  (cons 'user
                                        (or
                                         (auth-source-netrc-element-or-first
                                          (auth-source--aget valist 'user))
                                         (plist-get artificial :user)
                                         "[any user]"))
                                  (cons 'host
                                        (or
                                         (auth-source-netrc-element-or-first
                                          (auth-source--aget valist 'host))
                                         (plist-get artificial :host)
                                         "[any host]"))
                                  (cons 'port
                                        (or
                                         (auth-source-netrc-element-or-first
                                          (auth-source--aget valist 'port))
                                         (plist-get artificial :port)
                                         "[any port]"))))
             (prompt (or (auth-source--aget auth-source-creation-prompts r)
                         (cl-case r
                           (secret "%p password for %u@%h")
                           (user "%p user name for %h")
                           (host "%p host name for user %u")
                           (port "%p port for %u@%h"))
                         (format "Enter %s (%%u@%%h:%%p)" r)))
             (prompt (auth-source-format-prompt
                      prompt
                      `((?u ,(auth-source--aget printable-defaults 'user))
                        (?h ,(auth-source--aget printable-defaults 'host))
                        (?p ,(auth-source--aget printable-defaults 'port))))))

        ;; Store the data, prompting for the password if needed.
        (setq data (or data
                       (if (eq r 'secret)
                           (or (eval default)
                               (read-passwd (format-prompt prompt nil)))
                         (if (and (stringp default) auth-source-save-behavior)
                             (read-string
                              (format-prompt prompt default) nil nil default)
                           (eval default)))))

        (when data
          (if (member r base-secret)
              (setq secret-artificial
                    (plist-put secret-artificial
                               (auth-source--symbol-keyword r)
                               data))
            (setq artificial (plist-put artificial
                                        (auth-source--symbol-keyword r)
                                        data))))))
    (plstore-put (oref backend data)
                 (sha1 (format "%s@%s:%s"
                               (plist-get artificial :user)
                               (plist-get artificial :host)
                               (plist-get artificial :port)))
                 artificial secret-artificial)
    (if (y-or-n-p (format "Save auth info to file %s? "
                          (plstore-get-file (oref backend data))))
        (plstore-save (oref backend data)))))

;;; Backend specific parsing: JSON backend
;; (auth-source-search :max 1 :machine "imap.gmail.com")
;; (auth-source-search :max 1 :host '("my-gmail" "imap.gmail.com") :port '(993 "imaps" "imap" "993" "143") :user nil :require '(:user :secret))

(defun auth-source-json-check (host user port require item)
  (and item
       (auth-source-search-collection
        (or host t)
        (or
         (plist-get item :machine)
         (plist-get item :host)
         t))
       (auth-source-search-collection
        (or user t)
        (or
         (plist-get item :login)
         (plist-get item :account)
         (plist-get item :user)
         t))
       (auth-source-search-collection
        (or port t)
        (or
         (plist-get item :port)
         (plist-get item :protocol)
         t))
       (or
        ;; the required list of keys is nil, or
        (null require)
        ;; every element of require is in
        (cl-loop for req in require
                 always (plist-get item req)))))

(cl-defun auth-source-json-search (&rest spec
                                    &key backend require
                                    type max host user port
                                    &allow-other-keys)
  "Given a property list SPEC, return search matches from the `:backend'.
See `auth-source-search' for details on SPEC."
  ;; just in case, check that the type is correct (null or same as the backend)
  (cl-assert (or (null type) (eq type (oref backend type)))
             t "Invalid JSON search: %s %s")

  ;; Hide the secrets early to avoid accidental exposure.
  (let* ((jdata
          (mapcar (lambda (entry)
                    (let (ret)
                      (while entry
                        (let* ((item (pop entry))
                               (k (auth-source--symbol-keyword (car item)))
                               (v (cdr item)))
                          (setq k (cond ((memq k '(:machine)) :host)
                                        ((memq k '(:login :account)) :user)
                                        ((memq k '(:protocol)) :port)
                                        ((memq k '(:password)) :secret)
                                        (t k)))
                          ;; send back the secret in a function (lexical binding)
                          (when (eq k :secret)
                            (setq v (let ((lexv v))
                                      (lambda () lexv))))
                          (setq ret (plist-put ret k v))))
                      ret))
                  (json-read-file (oref backend source))))
         (max (or max 5000))       ; sanity check: default to stop at 5K
         all)
    (dolist (item jdata)
      (when (and item
                 (> max (length all))
                 (auth-source-json-check host user port require item))
        (push item all)))
    (nreverse all)))

;;; older API

;; (auth-source-user-or-password '("login" "password") "imap.myhost.com" t "tzz")

;; deprecate the old interface
(make-obsolete 'auth-source-user-or-password
               'auth-source-search "24.1")
(make-obsolete 'auth-source-forget-user-or-password
               'auth-source-forget "24.1")

(defun auth-source-user-or-password
  (mode host port &optional username create-missing delete-existing)
  "Find MODE (string or list of strings) matching HOST and PORT.

DEPRECATED in favor of `auth-source-search'!

USERNAME is optional and will be used as \"login\" in a search
across the Secret Service API (see secrets.el) if the resulting
items don't have a username.  This means that if you search for
username \"joe\" and it matches an item but the item doesn't have
a :user attribute, the username \"joe\" will be returned.

A non-nil DELETE-EXISTING means deleting any matching password
entry in the respective sources.  This is useful only when
CREATE-MISSING is non-nil as well; the intended use case is to
remove wrong password entries.

If no matching entry is found, and CREATE-MISSING is non-nil,
the password will be retrieved interactively, and it will be
stored in the password database which matches best (see
`auth-sources').

MODE can be \"login\" or \"password\"."
  (auth-source-do-debug
   "auth-source-user-or-password: DEPRECATED get %s for %s (%s) + user=%s"
   mode host port username)

  (let* ((listy (listp mode))
         (mode (if listy mode (list mode)))
         ;; (cname (if username
         ;;            (format "%s %s:%s %s" mode host port username)
         ;;          (format "%s %s:%s" mode host port)))
         (search (list :host host :port port))
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
           host port username)
          found)                        ; return the found data
      ;; else, if not found, search with a max of 1
      (let ((choice (nth 0 (apply #'auth-source-search
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

(defun auth-source-user-and-password (host &optional user)
  (let* ((auth-info (car
                     (if user
                         (auth-source-search
                          :host host
                          :user user
                          :max 1
                          :require '(:user :secret)
                          :create nil)
                       (auth-source-search
                        :host host
                        :max 1
                        :require '(:user :secret)
                        :create nil))))
         (user (plist-get auth-info :user))
         (password (plist-get auth-info :secret)))
    (when (functionp password)
      (setq password (funcall password)))
    (list user password auth-info)))

;;; Tiny mode for editing .netrc/.authinfo modes (that basically just
;;; hides passwords and adds basic syntax highlighting).

(defcustom authinfo-hidden "password"
  "Regexp matching elements in .authinfo/.netrc files that should be hidden."
  :type 'regexp
  :version "27.1")

(defcustom authinfo-hide-elements t
  "Whether to use `authinfo-hidden' to hide elements in authinfo files."
  :type 'boolean
  :version "28.1")

(defvar authinfo--keywords
  '(("^#.*" . font-lock-comment-face)
    ("^\\(machine\\)[ \t]+\\([^ \t\n]+\\)"
     (1 font-lock-variable-name-face)
     (2 font-lock-builtin-face))
    ("\\(login\\)[ \t]+\\([^ \t\n]+\\)"
     (1 font-lock-comment-delimiter-face)
     (2 font-lock-keyword-face))
    ("\\(password\\)[ \t]+\\([^ \t\n]+\\)"
     (1 font-lock-comment-delimiter-face)
     (2 font-lock-doc-face))
    ("\\(port\\)[ \t]+\\([^ \t\n]+\\)"
     (1 font-lock-comment-delimiter-face)
     (2 font-lock-type-face))
    ("\\([^ \t\n]+\\)[, \t]+\\([^ \t\n]+\\)"
     (1 font-lock-constant-face)
     (2 nil))))

;;;###autoload
(define-derived-mode authinfo-mode fundamental-mode "Authinfo"
  "Mode for editing .authinfo/.netrc files.

This is just like `fundamental-mode', but has basic syntax
highlighting and hides passwords.  Passwords are revealed when
point is moved into the passwords (see `authinfo-hide-elements').

\\{authinfo-mode-map}"
  (font-lock-add-keywords nil authinfo--keywords)
  (setq-local comment-start "#")
  (setq-local comment-end "")
  (when authinfo-hide-elements
    (authinfo--hide-passwords (point-min) (point-max))
    (reveal-mode)))

(defun authinfo--hide-passwords (start end)
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (while (re-search-forward (format "\\(\\s-\\|^\\)\\(%s\\)\\s-+"
                                        authinfo-hidden)
                                nil t)
        (when (auth-source-netrc-looking-at-token)
          (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
            (overlay-put overlay 'display
                         (propertize "****" 'face 'font-lock-doc-face))
            (overlay-put overlay 'reveal-toggle-invisible
                         #'authinfo--toggle-display)))))))

(defun authinfo--toggle-display (overlay hide)
  (if hide
      (overlay-put overlay 'display
                   (propertize "****" 'face 'font-lock-doc-face))
    (overlay-put overlay 'display nil)))

(provide 'auth-source)

;;; auth-source.el ends here
