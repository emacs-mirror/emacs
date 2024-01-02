;;; erc-sasl.el --- SASL for ERC -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This "non-IRCv3" implementation resembles others that have surfaced
;; over the years, the first possibly being from Joseph Gay:
;;
;; https://lists.gnu.org/archive/html/erc-discuss/2012-02/msg00001.html
;;
;; See M-x customize-group RET erc-sasl RET and (info "(erc) SASL")
;; for usage.
;;
;; TODO:
;;
;; - Obfuscate non-auth-source passwords in memory.  They're currently
;;   visible in backtraces.
;;
;; - Implement a proxy mechanism that chooses the strongest available
;;   mechanism for you.  Requires CAP 3.2 (see bug#49860).
;;
;; - Integrate with whatever solution ERC eventually settles on to
;;   handle user options for different network contexts.  At the
;;   moment, this does its own thing for stashing and restoring
;;   session options, but ERC should make abstractions available for
;;   all local modules to use, possibly based on connection-local
;;   variables.

;;; Code:
(require 'erc)
(require 'rx)
(require 'sasl)
(require 'sasl-scram-rfc)
(require 'sasl-scram-sha256 nil t) ; not present in Emacs 27

(defgroup erc-sasl nil
  "SASL for ERC."
  :group 'erc
  :package-version '(ERC . "5.5"))

(defcustom erc-sasl-mechanism 'plain
  "SASL mechanism to connect with.
Note that any value other than nil or `external' likely requires
`erc-sasl-user' and `erc-sasl-password'."
  :type '(choice (const plain)
                 (const external)
                 (const scram-sha-1)
                 (const scram-sha-256)
                 (const scram-sha-512)
                 (const ecdsa-nist256p-challenge)))

(defcustom erc-sasl-user :user
  "Account username to send when authenticating.
This option specifies the SASL authentication identity, or
\"authcid\".  A value of `:user' or `:nick' indicates that the
corresponding connection parameter on file should be used.  ERC
typically obtains these from arguments given to its entry-point
commands, `erc' and `erc-tls'."
  :type '(choice string (const :user) (const :nick)))

(defcustom erc-sasl-password :password
  "Optional account password to send when authenticating.
When `erc-sasl-auth-source-function' is a function, ERC attempts
an auth-source query and prompts for input if it fails.
Otherwise, when the value of this option is a nonempty string,
ERC uses it unconditionally for most mechanisms.  Likewise with a
value of `:password', except ERC instead uses the \"session
password\" on file, if any, which often originates from the
entry-point commands `erc' or `erc-tls'.  As with auth-source,
ERC prompts for input as a fallback.

Note that, with `:password', ERC forgoes sending a traditional
server password via the IRC \"PASS\" command.  Also, when
`erc-sasl-mechanism' is set to `ecdsa-nist256p-challenge', ERC
expects this option to hold the file name of the key."
  :type '(choice (const nil) (const :password) string symbol))

(defcustom erc-sasl-auth-source-function nil
  "Function to query auth-source for an SASL password.
If provided, this function should expect to be called with any
number of keyword params known to `auth-source-search', even
though ERC itself only specifies `:user' paired with a
\"resolved\" `erc-sasl-user' value.  When calling this function,
ERC binds all options defined in this library, such as
`erc-sasl-password', to their values from entry-point invocation.
In return, ERC expects a string to send as the SASL password, or
nil, in which case, ERC prompts for input.  See Info node `(erc)
auth-source' for details on ERC's auth-source integration."
  :type '(choice (function-item erc-sasl-auth-source-password-as-host)
                 (function-item erc-auth-source-search)
                 (const nil)
                 function))

(defcustom erc-sasl-authzid nil
  "SASL authorization identity, likely unneeded for everyday use."
  :type '(choice (const nil) string))


;; Analogous to what erc-backend does to persist opening params.
(defvar-local erc-sasl--options nil)

;; Session-local (server buffer) SASL subproto state
(defvar-local erc-sasl--state nil)

(cl-defstruct erc-sasl--state
  "Holder for client object and subproto state."
  (client nil :type vector)
  (step nil :type vector)
  (pending nil :type string))

(defun erc-sasl--get-user ()
  (pcase (alist-get 'user erc-sasl--options)
    (:user erc-session-username)
    (:nick (erc-current-nick))
    (v v)))

(defun erc-sasl-auth-source-password-as-host (&rest plist)
  "Call `erc-auth-source-search' with `erc-sasl-password' as `:host'.
But only do so when it's a string or a non-nil symbol, unless
that symbol is `:password', in which case, use a non-nil
`erc-session-password' instead.  Otherwise, just defer to
`erc-auth-source-search' to pick a suitable `:host'.  Expect
PLIST to contain keyword params known to `auth-source-search'."
  (when erc-sasl-password
    (when-let ((host (if (eq :password erc-sasl-password)
                         (and (not (functionp erc-session-password))
                              erc-session-password)
                       erc-sasl-password)))
      (setq plist `(,@plist :host ,(format "%s" host)))))
  (apply #'erc-auth-source-search plist))

(defun erc-sasl--read-password (prompt)
  "Return configured option or server password.
If necessary, pass PROMPT to `read-passwd'."
  (if-let ((found (pcase (alist-get 'password erc-sasl--options)
                    ((guard (alist-get 'authfn erc-sasl--options))
                     (let-alist erc-sasl--options
                       (let ((erc-sasl-user .user)
                             (erc-sasl-password .password)
                             (erc-sasl-mechanism .mechanism)
                             (erc-sasl-authzid .authzid)
                             (erc-sasl-auth-source-function .authfn))
                         (funcall .authfn :user (erc-sasl--get-user)))))
                    (:password erc-session-password)
                    ((and (pred stringp) v) (unless (string-empty-p v) v)))))
      (copy-sequence (erc--unfun found))
    (read-passwd prompt)))

(defun erc-sasl--plain-response (client steps)
  (let ((sasl-read-passphrase #'erc-sasl--read-password))
    (sasl-plain-response client steps)))

(declare-function erc-compat--29-sasl-scram--client-final-message "erc-compat"
                  (hash-fun block-length hash-length client step))

(defun erc-sasl--scram-sha-hack-client-final-message (&rest args)
  ;; In the future (29+), we'll hopefully be able to call
  ;; `sasl-scram--client-final-message' directly
  (require 'erc-compat)
  (let ((sasl-read-passphrase #'erc-sasl--read-password))
    (apply #'erc-compat--29-sasl-scram--client-final-message args)))

(defun erc-sasl--scram-sha-1-client-final-message (client step)
  (erc-sasl--scram-sha-hack-client-final-message 'sha1 64 20 client step))

(defun erc-sasl--scram-sha-256-client-final-message (client step)
  (erc-sasl--scram-sha-hack-client-final-message 'sasl-scram-sha256 64 32
                                                 client step))

(defun erc-sasl--scram-sha512 (object &optional start end binary)
  (secure-hash 'sha512 object start end binary))

(defun erc-sasl--scram-sha-512-client-final-message (client step)
  (erc-sasl--scram-sha-hack-client-final-message #'erc-sasl--scram-sha512
                                                 128 64 client step))

(defun erc-sasl--scram-sha-512-authenticate-server (client step)
  (sasl-scram--authenticate-server #'erc-sasl--scram-sha512
                                   128 64 client step))

(defun erc-sasl--ecdsa-first (client _step)
  "Return CLIENT name."
  (sasl-client-name client))

;; FIXME do this with gnutls somehow
(defun erc-sasl--ecdsa-sign (client step)
  "Return signed challenge for CLIENT and current STEP."
  (let ((challenge (sasl-step-data step)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert challenge)
      (call-process-region (point-min) (point-max)
                           "openssl" 'delete t nil "pkeyutl" "-inkey"
                           (sasl-client-property client 'ecdsa-keyfile)
                           "-sign")
      (buffer-string))))

(pcase-dolist
    (`(,name . ,steps)
     '(("PLAIN"
        erc-sasl--plain-response)
       ("EXTERNAL"
        ignore)
       ("SCRAM-SHA-1"
        erc-compat--29-sasl-scram-client-first-message
        erc-sasl--scram-sha-1-client-final-message
        sasl-scram-sha-1-authenticate-server)
       ("SCRAM-SHA-256"
        erc-compat--29-sasl-scram-client-first-message
        erc-sasl--scram-sha-256-client-final-message
        sasl-scram-sha-256-authenticate-server)
       ("SCRAM-SHA-512"
        erc-compat--29-sasl-scram-client-first-message
        erc-sasl--scram-sha-512-client-final-message
        erc-sasl--scram-sha-512-authenticate-server)
       ("ECDSA-NIST256P-CHALLENGE"
        erc-sasl--ecdsa-first
        erc-sasl--ecdsa-sign)))
  (let ((feature (intern (concat "erc-sasl-" (downcase name)))))
    (put feature 'sasl-mechanism (sasl-make-mechanism name steps))
    (provide feature)))

(cl-defgeneric erc-sasl--create-client (mechanism)
  "Create and return a new SASL client object for MECHANISM."
  (let ((sasl-mechanism-alist (copy-sequence sasl-mechanism-alist))
        (sasl-mechanisms sasl-mechanisms)
        (name (upcase (symbol-name mechanism)))
        (feature (intern-soft (concat "erc-sasl-" (symbol-name mechanism))))
        client)
    (when feature
      (setf (alist-get name sasl-mechanism-alist nil nil #'equal) `(,feature))
      (cl-pushnew name sasl-mechanisms :test #'equal)
      (setq client (sasl-make-client (sasl-find-mechanism (list name))
                                     (erc-sasl--get-user)
                                     "N/A" "N/A"))
      (sasl-client-set-property client 'authenticator-name
                                (alist-get 'authzid erc-sasl--options))
      client)))

(cl-defmethod erc-sasl--create-client ((_ (eql plain)))
  "Create and return a new PLAIN client object."
  ;; https://tools.ietf.org/html/rfc4616#section-2.
  (let* ((sans (remq (assoc "PLAIN" sasl-mechanism-alist)
                     sasl-mechanism-alist))
         (sasl-mechanism-alist (cons '("PLAIN" erc-sasl-plain) sans))
         (authc (erc-sasl--get-user))
         (port (if (numberp erc-session-port)
                   (number-to-string erc-session-port)
                 "0"))
         ;; In most cases, `erc-server-announced-name' won't be known.
         (host (or erc-server-announced-name erc-session-server))
         (mech (sasl-find-mechanism '("PLAIN")))
         (client (sasl-make-client mech authc port host)))
    (sasl-client-set-property client 'authenticator-name
                              (alist-get 'authzid erc-sasl--options))
    client))

(cl-defmethod erc-sasl--create-client ((_ (eql scram-sha-256)))
  "Create and return a new SCRAM-SHA-256 client."
  (when (featurep 'sasl-scram-sha256)
    (cl-call-next-method)))

(cl-defmethod erc-sasl--create-client ((_ (eql scram-sha-512)))
  "Create and return a new SCRAM-SHA-512 client."
  (when (featurep 'sasl-scram-sha256)
    (cl-call-next-method)))

(cl-defmethod erc-sasl--create-client ((_ (eql ecdsa-nist256p-challenge)))
  "Create and return a new ECDSA-NIST256P-CHALLENGE client."
  (let ((keyfile (cdr (assq 'password erc-sasl--options))))
    ;; Better to signal usage errors now than inside a process filter.
    (cond ((or (not (stringp keyfile)) (not (file-readable-p keyfile)))
           (erc-display-error-notice
            nil "`erc-sasl-password' not accessible as a file")
           nil)
          ((not (executable-find "openssl"))
           (erc-display-error-notice nil "Could not find openssl program")
           nil)
          (t
           (let ((client (cl-call-next-method)))
             (sasl-client-set-property client 'ecdsa-keyfile keyfile)
             client)))))

;; This stands alone because it's also used by bug#49860.
(defun erc-sasl--init ()
  (setq erc-sasl--state (make-erc-sasl--state))
  ;; If the previous attempt failed during registration, this may be
  ;; non-nil and contain erroneous values, but how can we detect that?
  ;; What if the server dropped the connection for some other reason?
  (setq erc-sasl--options
        (or (and erc--server-reconnecting
                 (alist-get 'erc-sasl--options erc--server-reconnecting))
            `((user . ,erc-sasl-user)
              (password . ,erc-sasl-password)
              (mechanism . ,erc-sasl-mechanism)
              (authfn . ,erc-sasl-auth-source-function)
              (authzid . ,erc-sasl-authzid)))))

(defun erc-sasl--mechanism-offered-p (offered)
  "Return non-nil when OFFERED appears among a list of mechanisms."
  (string-match-p (rx-to-string
                   `(: (| bot ",")
                       ,(symbol-name (alist-get 'mechanism erc-sasl--options))
                       (| eot ",")))
                  (downcase offered)))

(erc-define-catalog
 'english
 '((s902 . "ERR_NICKLOCKED nick %n unavailable: %s")
   (s904 . "ERR_SASLFAIL (authentication failed) %s")
   (s905 . "ERR SASLTOOLONG (credentials too long) %s")
   (s906 . "ERR_SASLABORTED (authentication aborted) %s")
   (s907 . "ERR_SASLALREADY (already authenticated) %s")
   (s908 . "RPL_SASLMECHS (unsupported mechanism: %m) %s")))

(define-erc-module sasl nil
  "Non-IRCv3 SASL support for ERC.
This doesn't solicit or validate a suite of supported mechanisms."
  ;; See bug#49860 for a CAP 3.2-aware WIP implementation.
  ((unless erc--target
     (erc-sasl--init)
     (let* ((mech (alist-get 'mechanism erc-sasl--options))
            (client (erc-sasl--create-client mech)))
       (unless client
         (erc-display-error-notice
          nil (format "Unknown or unsupported SASL mechanism: %s" mech))
         (erc-error "Unknown or unsupported SASL mechanism: %s" mech))
       (setf (erc-sasl--state-client erc-sasl--state) client))))
  ((kill-local-variable 'erc-sasl--state)
   (kill-local-variable 'erc-sasl--options))
  'local)

(define-erc-response-handler (AUTHENTICATE)
  "Begin or resume an SASL session." nil
  (if-let* ((response (car (erc-response.command-args parsed)))
            ((= 400 (length response))))
      (cl-callf (lambda (s) (concat s response))
          (erc-sasl--state-pending erc-sasl--state))
    (cl-assert response t)
    (when (string= "+" response)
      (setq response ""))
    (setf response (base64-decode-string
                    (concat (erc-sasl--state-pending erc-sasl--state)
                            response))
          (erc-sasl--state-pending erc-sasl--state) nil)
    (let ((client (erc-sasl--state-client erc-sasl--state))
          (step (erc-sasl--state-step erc-sasl--state))
          data)
      (when step
        (sasl-step-set-data step response))
      (setq step (setf (erc-sasl--state-step erc-sasl--state)
                       (sasl-next-step client step))
            data (sasl-step-data step))
      (when (string= data "")
        (setq data nil))
      (when data
        (setq data (erc--unfun (base64-encode-string data t))))
      (erc-server-send (concat "AUTHENTICATE " (or data "+"))))))

(defun erc-sasl--destroy (proc)
  (run-hook-with-args 'erc-quit-hook proc)
  (delete-process proc)
  (erc-error "Disconnected from %s; please review SASL settings" proc))

(define-erc-response-handler (902)
  "Handle an ERR_NICKLOCKED response." nil
  (erc-display-message parsed '(notice error) 'active 's902
                       ?n (car (erc-response.command-args parsed))
                       ?s (erc-response.contents parsed))
  (erc-sasl--destroy proc))

(define-erc-response-handler (903)
  "Handle a RPL_SASLSUCCESS response." nil
  (when erc-sasl-mode
    (unless erc-server-connected
      (erc-server-send "CAP END")))
  (erc-display-message parsed 'notice proc (erc-response.contents parsed)))

(define-erc-response-handler (907)
  "Handle a RPL_SASLALREADY response." nil
  (erc-display-message parsed '(notice error) 'active 's907
                       ?s (erc-response.contents parsed)))

(define-erc-response-handler (904 905 906)
  "Handle various SASL-related error responses." nil
  (erc-display-message parsed '(notice error) 'active
                       (intern (format "s%s" (erc-response.command parsed)))
                       ?s (erc-response.contents parsed))
  (erc-sasl--destroy proc))

(define-erc-response-handler (908)
  "Handle a RPL_SASLMECHS response." nil
  (erc-display-message parsed '(notice error) 'active 's908
                       ?m (alist-get 'mechanism erc-sasl--options)
                       ?s (string-join (cdr (erc-response.command-args parsed))
                                       " "))
  (erc-sasl--destroy proc))

(defvar erc-sasl--send-cap-ls nil
  "Whether to send an opening \"CAP LS\" command.
This is an escape hatch for picky servers.  If you need it turned
into a user option, please let ERC know via \\[erc-bug].
Otherwise, expect it to disappear in subsequent versions.")

(cl-defmethod erc--register-connection (&context (erc-sasl-mode (eql t)))
  "Send speculative CAP and pipelined AUTHENTICATE and hope for the best."
  (if-let* ((c (erc-sasl--state-client erc-sasl--state))
            (m (sasl-mechanism-name (sasl-client-mechanism c))))
      (progn
        (erc-server-send (if erc-sasl--send-cap-ls "CAP LS" "CAP REQ :sasl"))
        (let ((erc-session-password
               (and erc-session-password
                    (not (eq :password
                             (alist-get 'password erc-sasl--options)))
                    erc-session-password))
              (erc-session-username
               ;; The username may contain a colon or a space
               (if (eq :user (alist-get 'user erc-sasl--options))
                   (erc-current-nick)
                 erc-session-username)))
          (cl-call-next-method))
        (when erc-sasl--send-cap-ls
          (erc-server-send "CAP REQ :sasl"))
        (erc-server-send (format "AUTHENTICATE %s" m)))
    (erc-sasl--destroy erc-server-process)))

(provide 'erc-sasl)
;;; erc-sasl.el ends here
;;
;; Local Variables:
;; generated-autoload-file: "erc-loaddefs.el"
;; End:
