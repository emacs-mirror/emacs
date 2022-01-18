;;; epg.el --- the EasyPG Library -*- lexical-binding: t -*-

;; Copyright (C) 1999-2000, 2002-2022 Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Keywords: PGP, GnuPG
;; Version: 1.0.0

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

;;; Code:
;;; Prelude

(require 'epg-config)
(require 'rfc6068)
(eval-when-compile (require 'cl-lib))

(define-error 'epg-error "GPG error")

;;; Variables

(defvar epg-user-id nil
  "GnuPG ID of your default identity.")

(defvar epg-user-id-alist nil
  "An alist mapping from key ID to user ID.")

(defvar epg-last-status nil)
(defvar epg-read-point nil)
(defvar epg-process-filter-running nil)
(defvar epg-pending-status-list nil)
(defvar epg-key-id nil)
(defvar epg-context nil)
(defvar epg-debug-buffer nil)
(defvar epg-agent-file nil)
(defvar epg-agent-mtime nil)

;;; Enums

;; from gnupg/common/openpgpdefs.h
(defconst epg-cipher-algorithm-alist
  '((0 . "NONE")
    (1 . "IDEA")
    (2 . "3DES")
    (3 . "CAST5")
    (4 . "BLOWFISH")
    (7 . "AES")
    (8 . "AES192")
    (9 . "AES256")
    (10 . "TWOFISH")
    (11 . "CAMELLIA128")
    (12 . "CAMELLIA256")
    (110 . "DUMMY")))

;; from gnupg/common/openpgpdefs.h
(defconst epg-pubkey-algorithm-alist
  '((1 . "RSA")
    (2 . "RSA_E")
    (3 . "RSA_S")
    (16 . "ELGAMAL_E")
    (17 . "DSA")
    (18 . "ECDH")
    (19 . "ECDSA")
    (20 . "ELGAMAL")
    (22 . "EDDSA")
    (110 . "PRIVATE10")))

;; from gnupg/common/openpgpdefs.h
(defconst epg-digest-algorithm-alist
  '((1 . "MD5")
    (2 . "SHA1")
    (3 . "RIPEMD160")
    (8 . "SHA256")
    (9 . "SHA384")
    (10 . "SHA512")
    (11 . "SHA224")
    (110 . "PRIVATE10")))

;; from gnupg/common/openpgpdefs.h
(defconst epg-compress-algorithm-alist
  '((0 . "NONE")
    (1 . "ZIP")
    (2 . "ZLIB")
    (3 . "BZIP2")
    (110 . "PRIVATE10")))

(defconst epg-invalid-recipients-reason-alist
  '((0 . "No specific reason given")
    (1 . "Not Found")
    (2 . "Ambiguous specification")
    (3 . "Wrong key usage")
    (4 . "Key revoked")
    (5 . "Key expired")
    (6 . "No CRL known")
    (7 . "CRL too old")
    (8 . "Policy mismatch")
    (9 . "Not a secret key")
    (10 . "Key not trusted")))

(defconst epg-delete-problem-reason-alist
  '((1 . "No such key")
    (2 . "Must delete secret key first")
    (3 . "Ambiguous specification")))

(defconst epg-import-ok-reason-alist
  '((0 . "Not actually changed")
    (1 . "Entirely new key")
    (2 . "New user IDs")
    (4 . "New signatures")
    (8 . "New subkeys")
    (16 . "Contains private key")))

(defconst epg-import-problem-reason-alist
  '((0 . "No specific reason given")
    (1 . "Invalid Certificate")
    (2 . "Issuer Certificate missing")
    (3 . "Certificate Chain too long")
    (4 . "Error storing certificate")))

(defconst epg-no-data-reason-alist
  '((1 . "No armored data")
    (2 . "Expected a packet but did not find one")
    (3 . "Invalid packet found, this may indicate a non OpenPGP message")
    (4 . "Signature expected but not found")))

(defconst epg-unexpected-reason-alist nil)

(defvar epg-key-validity-alist
  '((?o . unknown)
    (?i . invalid)
    (?d . disabled)
    (?r . revoked)
    (?e . expired)
    (?- . none)
    (?q . undefined)
    (?n . never)
    (?m . marginal)
    (?f . full)
    (?u . ultimate)))

(defvar epg-key-capability-alist
  '((?e . encrypt)
    (?s . sign)
    (?c . certify)
    (?a . authentication)
    (?D . disabled)))

(defvar epg-new-signature-type-alist
  '((?D . detached)
    (?C . clear)
    (?S . normal)))

(defvar epg-dn-type-alist
  '(("1.2.840.113549.1.9.1" . "EMail")
    ("2.5.4.12" . "T")
    ("2.5.4.42" . "GN")
    ("2.5.4.4" . "SN")
    ("0.2.262.1.10.7.20" . "NameDistinguisher")
    ("2.5.4.16" . "ADDR")
    ("2.5.4.15" . "BC")
    ("2.5.4.13" . "D")
    ("2.5.4.17" . "PostalCode")
    ("2.5.4.65" . "Pseudo")
    ("2.5.4.5" . "SerialNumber")))

(defvar epg-prompt-alist nil)

;;; Structs
;;;; Data Struct

(cl-defstruct (epg-data
               (:constructor nil)
               (:constructor epg-make-data-from-file (file))
               (:constructor epg-make-data-from-string (string))
               (:copier nil)
               (:predicate nil))
  (file nil :read-only t)
  (string nil :read-only t))

;;;; Context Struct
(declare-function epa-passphrase-callback-function "epa.el")

(cl-defstruct (epg-context
               (:constructor nil)
               (:constructor epg-context--make
                (protocol &optional armor textmode include-certs
                          cipher-algorithm digest-algorithm
                          compress-algorithm
                 &aux
                 (program
                  (let ((configuration (epg-find-configuration protocol)))
                    (unless configuration
                      (signal 'epg-error
                              (list "no usable configuration" protocol)))
                    (alist-get 'program configuration)))))
               (:copier nil)
               (:predicate nil))
  protocol
  program
  (home-directory epg-gpg-home-directory)
  armor
  textmode
  include-certs
  cipher-algorithm
  digest-algorithm
  compress-algorithm
  (passphrase-callback (list #'epa-passphrase-callback-function))
  progress-callback
  edit-callback
  signers
  sender
  sig-notations
  process
  output-file
  result
  operation
  (pinentry-mode epg-pinentry-mode)
  (error-output "")
  error-buffer)

;;;; Context Methods

;; This is not an alias, just so we can mark it as autoloaded.
;;;###autoload
(defun epg-make-context (&optional protocol armor textmode include-certs
				   cipher-algorithm digest-algorithm
				   compress-algorithm)
  "Return a context object."
  (epg-context--make (or protocol 'OpenPGP)
                     armor textmode include-certs
                     cipher-algorithm digest-algorithm
                     compress-algorithm))

(defun epg-context-set-armor (context armor)
  "Specify if the output should be ASCII armored in CONTEXT."
  (declare (obsolete setf "25.1"))
  (setf (epg-context-armor context) armor))

(defun epg-context-set-textmode (context textmode)
  "Specify if canonical text mode should be used in CONTEXT."
  (declare (obsolete setf "25.1"))
  (setf (epg-context-textmode context) textmode))

(defun epg-context-set-passphrase-callback (context
					    passphrase-callback)
  "Set the function used to query passphrase.

PASSPHRASE-CALLBACK is either a function, or a cons-cell whose
car is a function and cdr is a callback data.

The function gets three arguments: the context, the key-id in
question, and the callback data (if any).

The callback may not be called if you use GnuPG 2.x, which relies
on the external program called `gpg-agent' for passphrase query.
If you really want to intercept passphrase query, consider
installing GnuPG 1.x _along with_ GnuPG 2.x, which does passphrase
query by itself and Emacs can intercept them."
  ;; (declare (obsolete setf "25.1"))
  (setf (epg-context-passphrase-callback context)
        (if (functionp passphrase-callback)
	    (list passphrase-callback)
	  passphrase-callback)))

(defun epg-context-set-progress-callback (context
					  progress-callback)
  "Set the function which handles progress update.

PROGRESS-CALLBACK is either a function, or a cons-cell whose
car is a function and cdr is a callback data.

The function gets six arguments: the context, the operation
description, the character to display a progress unit, the
current amount done, the total amount to be done, and the
callback data (if any)."
  (setf (epg-context-progress-callback context)
        (if (functionp progress-callback)
            (list progress-callback)
          progress-callback)))

(defun epg-context-set-signers (context signers)
  "Set the list of key-id for signing."
  (declare (obsolete setf "25.1"))
  (setf (epg-context-signers context) signers))

;;;; Other Structs

(cl-defstruct (epg-signature
               (:constructor nil)
               (:constructor epg-make-signature
                (status &optional key-id))
               (:copier nil)
               (:predicate nil))
  status
  key-id
  validity
  fingerprint
  creation-time
  expiration-time
  pubkey-algorithm
  digest-algorithm
  class
  version
  notations)

(cl-defstruct (epg-new-signature
               (:constructor nil)
               (:constructor epg-make-new-signature
                (type pubkey-algorithm digest-algorithm
                      class creation-time fingerprint))
               (:copier nil)
               (:predicate nil))
  (type nil :read-only t)
  (pubkey-algorithm nil :read-only t)
  (digest-algorithm nil :read-only t)
  (class nil :read-only t)
  (creation-time nil :read-only t)
  (fingerprint nil :read-only t))

(cl-defstruct (epg-key
               (:constructor nil)
               (:constructor epg-make-key (owner-trust))
               (:predicate nil))
  (owner-trust nil :read-only t)
  sub-key-list user-id-list)

(cl-defstruct (epg-sub-key
               (:constructor nil)
               (:constructor epg-make-sub-key
                (validity capability secret-p algorithm length id
                          creation-time expiration-time))
               (:copier nil)
               (:predicate nil))
  validity capability secret-p algorithm length id
  creation-time expiration-time fingerprint)

(cl-defstruct (epg-user-id
               (:constructor nil)
               (:constructor epg-make-user-id (validity string))
               (:copier nil)
               (:predicate nil))
  validity string signature-list)

(cl-defstruct (epg-key-signature
               (:constructor nil)
               (:constructor epg-make-key-signature
                (validity pubkey-algorithm key-id creation-time
                          expiration-time user-id class
                          exportable-p))
               (:copier nil)
               (:predicate nil))
  validity pubkey-algorithm key-id creation-time
  expiration-time user-id class
  exportable-p)

(cl-defstruct (epg-sig-notation
               (:constructor nil)
               (:constructor epg-make-sig-notation
                (name value &optional human-readable critical))
               (:copier nil)
               (:predicate nil))
  name value human-readable critical)

(cl-defstruct (epg-import-status
               (:constructor nil)
               (:constructor epg-make-import-status
                (fingerprint
                 &optional reason new user-id signature sub-key secret))
               (:copier nil)
               (:predicate nil))
  fingerprint reason new user-id signature sub-key secret)

(cl-defstruct (epg-import-result
               (:constructor nil)
               (:constructor epg-make-import-result
                (considered no-user-id imported imported-rsa
                            unchanged new-user-ids new-sub-keys
                            new-signatures new-revocations
                            secret-read secret-imported
                            secret-unchanged not-imported
                            imports))
               (:copier nil)
               (:predicate nil))
  considered no-user-id imported imported-rsa
  unchanged new-user-ids new-sub-keys
  new-signatures new-revocations
  secret-read secret-imported
  secret-unchanged not-imported
  imports)

;;; Functions

(defun epg-context-result-for (context name)
  "Return the result of CONTEXT associated with NAME."
  (cdr (assq name (epg-context-result context))))

(defun epg-context-set-result-for (context name value)
  "Set the result of CONTEXT associated with NAME to VALUE."
  (let* ((result (epg-context-result context))
	 (entry (assq name result)))
    (if entry
	(setcdr entry value)
      (setf (epg-context-result context) (cons (cons name value) result)))))

(defun epg-signature-to-string (signature)
  "Convert SIGNATURE to a human readable string."
  (let* ((user-id (cdr (assoc (epg-signature-key-id signature)
			      epg-user-id-alist)))
	 (pubkey-algorithm (epg-signature-pubkey-algorithm signature))
	 (key-id (epg-signature-key-id signature)))
    (concat
     (cl-case (epg-signature-status signature)
       (good "Good signature from ")
       (bad "Bad signature from ")
       (expired "Expired signature from ")
       (expired-key "Signature made by expired key ")
       (revoked-key "Signature made by revoked key ")
       (no-pubkey "No public key for "))
     key-id
     (and user-id
	  (concat " "
		  (if (stringp user-id)
                      (if (= (length user-id) (string-bytes user-id))
                          ;; This is ASCII, possibly %-encoded.
		          (rfc6068-unhexify-string user-id)
                        ;; Non-ASCII, return as is.
                        user-id)
		    (epg-decode-dn user-id))))
     (and (epg-signature-validity signature)
	  (format " (trust %s)"  (epg-signature-validity signature)))
     (and (epg-signature-creation-time signature)
	  (format-time-string " created at %Y-%m-%dT%T%z"
			      (epg-signature-creation-time signature)))
     (and pubkey-algorithm
	  (concat " using "
		  (or (cdr (assq pubkey-algorithm epg-pubkey-algorithm-alist))
		      (format "(unknown algorithm %d)" pubkey-algorithm)))))))

(defun epg-verify-result-to-string (verify-result)
  "Convert VERIFY-RESULT to a human readable string."
  (mapconcat #'epg-signature-to-string verify-result "\n"))

(defun epg-new-signature-to-string (new-signature)
  "Convert NEW-SIGNATURE to a human readable string."
  (concat
   (cond ((eq (epg-new-signature-type new-signature) 'detached)
	  "Detached signature ")
	 ((eq (epg-new-signature-type new-signature) 'clear)
	  "Cleartext signature ")
	 (t
	  "Signature "))
   (cdr (assq (epg-new-signature-pubkey-algorithm new-signature)
	      epg-pubkey-algorithm-alist))
   "/"
   (cdr (assq (epg-new-signature-digest-algorithm new-signature)
	      epg-digest-algorithm-alist))
   " "
   (format "%02X " (epg-new-signature-class new-signature))
   (epg-new-signature-fingerprint new-signature)))

(defun epg-import-result-to-string (import-result)
  "Convert IMPORT-RESULT to a human readable string."
  (concat (format "Total number processed: %d\n"
		  (epg-import-result-considered import-result))
	  (if (> (epg-import-result-not-imported import-result) 0)
	      (format "      skipped new keys: %d\n"
		      (epg-import-result-not-imported import-result)))
	  (if (> (epg-import-result-no-user-id import-result) 0)
	      (format "          w/o user IDs: %d\n"
		      (epg-import-result-no-user-id import-result)))
	  (if (> (epg-import-result-imported import-result) 0)
	      (concat (format "              imported: %d"
			      (epg-import-result-imported import-result))
		      (if (> (epg-import-result-imported-rsa import-result) 0)
			  (format "  (RSA: %d)"
				  (epg-import-result-imported-rsa
				   import-result)))
		      "\n"))
	  (if (> (epg-import-result-unchanged import-result) 0)
	      (format "             unchanged: %d\n"
		      (epg-import-result-unchanged import-result)))
	  (if (> (epg-import-result-new-user-ids import-result) 0)
	      (format "          new user IDs: %d\n"
		      (epg-import-result-new-user-ids import-result)))
	  (if (> (epg-import-result-new-sub-keys import-result) 0)
	      (format "           new subkeys: %d\n"
		      (epg-import-result-new-sub-keys import-result)))
	  (if (> (epg-import-result-new-signatures import-result) 0)
	      (format "        new signatures: %d\n"
		      (epg-import-result-new-signatures import-result)))
	  (if (> (epg-import-result-new-revocations import-result) 0)
	      (format "   new key revocations: %d\n"
		      (epg-import-result-new-revocations import-result)))
	  (if (> (epg-import-result-secret-read import-result) 0)
	      (format "      secret keys read: %d\n"
		      (epg-import-result-secret-read import-result)))
	  (if (> (epg-import-result-secret-imported import-result) 0)
	      (format "  secret keys imported: %d\n"
		      (epg-import-result-secret-imported import-result)))
	  (if (> (epg-import-result-secret-unchanged import-result) 0)
	      (format " secret keys unchanged: %d\n"
		      (epg-import-result-secret-unchanged import-result)))))

(defun epg-error-to-string (error)
  (cond
   ((eq (car error) 'exit)
    "Exit")
   ((eq (car error) 'quit)
    "Canceled")
   ((eq (car error) 'no-data)
    (let ((entry (assq (cdr error) epg-no-data-reason-alist)))
      (if entry
	  (format "No data (%s)" (downcase (cdr entry)))
	"No data")))
   ((eq (car error) 'unexpected)
    (let ((entry (assq (cdr error) epg-unexpected-reason-alist)))
      (if entry
	  (format "Unexpected (%s)" (downcase (cdr entry)))
	"Unexpected")))
   ((eq (car error) 'bad-armor)
    "Bad armor")
   ((memq (car error) '(invalid-recipient invalid-signer))
    (concat
     (if (eq (car error) 'invalid-recipient)
	 "Unusable public key"
       "Unusable secret key")
     (let ((entry (assq 'requested (cdr error))))
       (if entry
	   (format ": %s" (cdr entry))
	 ": <unknown>"))
     (let ((entry (assq 'reason (cdr error))))
       (if (and entry
		(> (cdr entry) 0)	;no specific reason given
		(setq entry (assq (cdr entry)
				  epg-invalid-recipients-reason-alist)))
	   (format " (%s)" (downcase (cdr entry)))
	 ""))))
   ((eq (car error) 'no-pubkey)
    (format "No public key: %s" (cdr error)))
   ((eq (car error) 'no-seckey)
    (format "No secret key: %s" (cdr error)))
   ((eq (car error) 'no-recipients)
    "No recipients")
   ((eq (car error) 'no-signers)
    "No signers")
   ((eq (car error) 'delete-problem)
    (let ((entry (assq (cdr error) epg-delete-problem-reason-alist)))
      (if entry
	  (format "Delete problem (%s)" (downcase (cdr entry)))
	"Delete problem")))
   ((eq (car error) 'key-not-created)
    "Key not created")))

(defun epg-errors-to-string (errors)
  (mapconcat #'epg-error-to-string errors "; "))

(defun epg--start (context args)
  "Start `epg-gpg-program' in a subprocess with given ARGS."
  (if (and (epg-context-process context)
	   (eq (process-status (epg-context-process context)) 'run))
      (error "%s is already running in this context"
	     (epg-context-program context)))
  (let* ((agent-info (getenv "GPG_AGENT_INFO"))
	 (args (append (list "--no-tty"
			     "--status-fd" "1"
			     "--yes")
		       (if (and (not (eq (epg-context-protocol context) 'CMS))
				(string-search ":" (or agent-info "")))
			   '("--use-agent"))
		       (if (and (not (eq (epg-context-protocol context) 'CMS))
				(epg-context-progress-callback context))
			   '("--enable-progress-filter"))
		       (if (epg-context-home-directory context)
			   (list "--homedir"
				 (epg-context-home-directory context)))
		       (unless (eq (epg-context-protocol context) 'CMS)
			 '("--command-fd" "0"))
		       (if (epg-context-armor context) '("--armor"))
		       (if (epg-context-textmode context) '("--textmode"))
		       (if (epg-context-output-file context)
			   (list "--output" (epg-context-output-file context)))
		       (if (epg-context-pinentry-mode context)
			   (list "--pinentry-mode"
				 (symbol-name (epg-context-pinentry-mode
					       context))))
		       args))
	 (process-environment process-environment)
	 (buffer (generate-new-buffer " *epg*"))
	 error-process
	 process
	 terminal-name
	 agent-file
	 (agent-mtime '(0 0 0 0)))
    ;; Set GPG_TTY and TERM for pinentry-curses.  Note that we can't
    ;; use `terminal-name' here to get the real pty name for the child
    ;; process, though /dev/fd/0" is not portable.
    (unless (memq system-type '(ms-dos windows-nt))
      (with-temp-buffer
	(condition-case nil
	    (when (= (call-process "tty" "/dev/fd/0" t) 0)
	      (delete-char -1)
	      (setq terminal-name (buffer-string)))
	  (file-error))))
    (when terminal-name
      (setq process-environment
	    (cons (concat "GPG_TTY=" terminal-name)
		  (cons "TERM=xterm" process-environment))))
    (setq process-environment
	  (cons (format "INSIDE_EMACS=%s,epg" emacs-version)
		process-environment))
    ;; Record modified time of gpg-agent socket to restore the Emacs
    ;; frame on text terminal in `epg-wait-for-completion'.
    ;; See
    ;; <https://lists.gnu.org/r/emacs-devel/2007-02/msg00755.html>
    ;; for more details.
    (when (and agent-info (string-match "\\(.*\\):[0-9]+:[0-9]+" agent-info))
      (setq agent-file (match-string 1 agent-info)
	    agent-mtime (or (file-attribute-modification-time
			     (file-attributes agent-file))
			    '(0 0 0 0))))
    (if epg-debug
	(save-excursion
	  (unless epg-debug-buffer
	    (setq epg-debug-buffer (generate-new-buffer " *epg-debug*")))
	  (set-buffer epg-debug-buffer)
	  (goto-char (point-max))
	  (insert (if agent-info
		      (format "GPG_AGENT_INFO=%s\n" agent-info)
		    "GPG_AGENT_INFO is not set\n")
		  (format "%s %s\n"
			  (epg-context-program context)
			  (mapconcat #'identity args " ")))))
    (with-current-buffer buffer
      (if (fboundp 'set-buffer-multibyte)
	  (set-buffer-multibyte nil))
      (setq-local epg-last-status nil)
      (setq-local epg-read-point (point-min))
      (setq-local epg-process-filter-running nil)
      (setq-local epg-pending-status-list nil)
      (setq-local epg-key-id nil)
      (setq-local epg-context context)
      (setq-local epg-agent-file agent-file)
      (setq-local epg-agent-mtime agent-mtime))
    (setq error-process
	  (make-pipe-process :name "epg-error"
			     :buffer (generate-new-buffer " *epg-error*")
			     ;; Suppress "XXX finished" line.
			     :sentinel #'ignore
			     :noquery t))
    (setf (epg-context-error-buffer context) (process-buffer error-process))
    (with-existing-directory
      (with-file-modes 448
        (setq process (make-process :name "epg"
				    :buffer buffer
				    :command (cons (epg-context-program context)
						   args)
				    :connection-type 'pipe
				    :coding 'raw-text
				    :filter #'epg--process-filter
				    :stderr error-process
				    :noquery t))))
    (setf (epg-context-process context) process)))

(defun epg--process-filter (process input)
  (if epg-debug
      (with-current-buffer
          (or epg-debug-buffer
              (setq epg-debug-buffer (generate-new-buffer " *epg-debug*")))
	(goto-char (point-max))
	(insert input)))
  (if (buffer-live-p (process-buffer process))
      (with-current-buffer (process-buffer process)
        (save-excursion
          (goto-char (point-max))
          (insert input)
          (unless epg-process-filter-running
            (let ((epg-process-filter-running t))
              (goto-char epg-read-point)
              (beginning-of-line)
              (while (looking-at ".*\n") ;the input line finished
                (if (looking-at "\\[GNUPG:] \\([A-Z_]+\\) ?\\(.*\\)")
                    (let ((status (match-string 1))
			  (string (match-string 2))
			  symbol)
                      (if (member status epg-pending-status-list)
                          (setq epg-pending-status-list nil))
		      ;; When editing a key, delegate all interaction
		      ;; to edit-callback.
		      (if (eq (epg-context-operation epg-context) 'edit-key)
			  (funcall (car (epg-context-edit-callback
					 epg-context))
				   epg-context
				   status
				   string
				   (cdr (epg-context-edit-callback
					 epg-context)))
			;; Otherwise call epg--status-STATUS function.
			(setq symbol (intern-soft (concat "epg--status-"
							  status)))
			(if (and symbol
				 (fboundp symbol))
			    (funcall symbol epg-context string)))
                      (setq epg-last-status (cons status string))))
                (forward-line)
                (setq epg-read-point (point)))))))))

(defun epg-read-output (context)
  "Read the output file CONTEXT and return the content as a string."
  (with-temp-buffer
    (if (fboundp 'set-buffer-multibyte)
	(set-buffer-multibyte nil))
    (if (file-exists-p (epg-context-output-file context))
	(let ((coding-system-for-read 'binary))
	  (insert-file-contents (epg-context-output-file context))
	  (buffer-string)))))

(defun epg-wait-for-status (context status-list)
  "Wait until one of elements in STATUS-LIST arrives."
  (with-current-buffer (process-buffer (epg-context-process context))
    (setq epg-pending-status-list status-list)
    (while (and (eq (process-status (epg-context-process context)) 'run)
		epg-pending-status-list)
      (accept-process-output (epg-context-process context) 1))
    (if epg-pending-status-list
	(epg-context-set-result-for
	 context 'error
	 (cons '(exit)
	       (epg-context-result-for context 'error))))))

(defun epg-wait-for-completion (context)
  "Wait until the `epg-gpg-program' process completes."
  (while (eq (process-status (epg-context-process context)) 'run)
    (accept-process-output (epg-context-process context) 1))
  ;; This line is needed to run the process-filter right now.
  (sleep-for 0.1)
  ;; Restore Emacs frame on text terminal, when pinentry-curses has terminated.
  (if (with-current-buffer (process-buffer (epg-context-process context))
	(and epg-agent-file
	     (time-less-p epg-agent-mtime
			  (or (file-attribute-modification-time
			       (file-attributes epg-agent-file))
			      0))))
      (redraw-frame))
  (epg-context-set-result-for
   context 'error
   (nreverse (epg-context-result-for context 'error)))
  (setf (epg-context-error-output context)
	(with-current-buffer (epg-context-error-buffer context)
	  (buffer-string))))

(defun epg-reset (context)
  "Reset the CONTEXT."
  (if (and (epg-context-process context)
	   (buffer-live-p (process-buffer (epg-context-process context))))
      (kill-buffer (process-buffer (epg-context-process context))))
  (if (buffer-live-p (epg-context-error-buffer context))
      (kill-buffer (epg-context-error-buffer context)))
  (setf (epg-context-process context) nil)
  (setf (epg-context-edit-callback context) nil))

(defun epg-delete-output-file (context)
  "Delete the output file of CONTEXT."
  (if (and (epg-context-output-file context)
	   (file-exists-p (epg-context-output-file context)))
      (delete-file (epg-context-output-file context))))

(defun epg--status-USERID_HINT (_context string)
  (if (string-match "\\`\\([^ ]+\\) \\(.*\\)" string)
      (let* ((key-id (match-string 1 string))
	     (user-id (match-string 2 string))
	     (entry (assoc key-id epg-user-id-alist)))
	(condition-case nil
	    (setq user-id (rfc6068-unhexify-string user-id))
	  (error))
	(if entry
	    (setcdr entry user-id)
	  (setq epg-user-id-alist (cons (cons key-id user-id)
					epg-user-id-alist))))))

(defun epg--status-NEED_PASSPHRASE (_context string)
  (if (string-match "\\`\\([^ ]+\\)" string)
      (setq epg-key-id (match-string 1 string))))

(defun epg--status-NEED_PASSPHRASE_SYM (_context _string)
  (setq epg-key-id 'SYM))

(defun epg--status-NEED_PASSPHRASE_PIN (_context _string)
  (setq epg-key-id 'PIN))

(defun epg--status-GET_HIDDEN (context string)
  (when (and epg-key-id
	     (string-match "\\`passphrase\\." string))
    (unless (epg-context-passphrase-callback context)
      (error "Variable `passphrase-callback' not set"))
    (let (inhibit-quit
	  passphrase
	  passphrase-with-new-line
	  encoded-passphrase-with-new-line)
      (unwind-protect
	  (condition-case nil
	      (progn
		(setq passphrase
		      (funcall
		       (car (epg-context-passphrase-callback context))
		       context
		       epg-key-id
		       (cdr (epg-context-passphrase-callback context))))
		(when passphrase
		  (setq passphrase-with-new-line (concat passphrase "\n"))
		  (clear-string passphrase)
		  (setq passphrase nil)
		  (if epg-passphrase-coding-system
		      (progn
			(setq encoded-passphrase-with-new-line
			      (encode-coding-string
			       passphrase-with-new-line
			       (coding-system-change-eol-conversion
				epg-passphrase-coding-system 'unix)))
			(clear-string passphrase-with-new-line)
			(setq passphrase-with-new-line nil))
		    (setq encoded-passphrase-with-new-line
			  passphrase-with-new-line
			  passphrase-with-new-line nil))
		  (process-send-string (epg-context-process context)
				       encoded-passphrase-with-new-line)))
	    (quit
	     (epg-context-set-result-for
	      context 'error
	      (cons '(quit)
		    (epg-context-result-for context 'error)))
	     (delete-process (epg-context-process context))))
	(if passphrase
	    (clear-string passphrase))
	(if passphrase-with-new-line
	    (clear-string passphrase-with-new-line))
	(if encoded-passphrase-with-new-line
	    (clear-string encoded-passphrase-with-new-line))))))

(defun epg--prompt-GET_BOOL (_context string)
  (let ((entry (assoc string epg-prompt-alist)))
    (y-or-n-p (if entry (cdr entry) (concat string "? ")))))

(defun epg--prompt-GET_BOOL-untrusted_key.override (_context _string)
  (y-or-n-p (if (and (equal (car epg-last-status) "USERID_HINT")
		     (string-match "\\`\\([^ ]+\\) \\(.*\\)"
				   (cdr epg-last-status)))
		(let* ((key-id (match-string 1 (cdr epg-last-status)))
		       (user-id (match-string 2 (cdr epg-last-status)))
		       (entry (assoc key-id epg-user-id-alist)))
		  (if entry
		      (setq user-id (cdr entry)))
		  (format "Untrusted key %s %s.  Use anyway? " key-id user-id))
	      "Use untrusted key anyway? ")))

;;; Status Functions

(defun epg--status-GET_BOOL (context string)
  (let (inhibit-quit)
    (condition-case nil
	(if (funcall (or (intern-soft (concat "epg--prompt-GET_BOOL-" string))
			 #'epg--prompt-GET_BOOL)
		     context string)
	    (process-send-string (epg-context-process context) "y\n")
	  (process-send-string (epg-context-process context) "n\n"))
      (quit
       (epg-context-set-result-for
	context 'error
	(cons '(quit)
	      (epg-context-result-for context 'error)))
       (delete-process (epg-context-process context))))))

(defun epg--status-GET_LINE (context string)
  (let ((entry (assoc string epg-prompt-alist))
	inhibit-quit)
    (condition-case nil
	(process-send-string (epg-context-process context)
			     (concat (read-string
				      (if entry
					  (cdr entry)
					(concat string ": ")))
				     "\n"))
      (quit
       (epg-context-set-result-for
	context 'error
	(cons '(quit)
	      (epg-context-result-for context 'error)))
       (delete-process (epg-context-process context))))))

(defun epg--status-*SIG (context status string)
  (if (string-match "\\`\\([^ ]+\\) \\(.*\\)" string)
      (let* ((key-id (match-string 1 string))
	     (user-id (match-string 2 string))
	     (entry (assoc key-id epg-user-id-alist)))
	(epg-context-set-result-for
	 context
	 'verify
	 (cons (epg-make-signature status key-id)
	       (epg-context-result-for context 'verify)))
	(condition-case nil
	    (if (eq (epg-context-protocol context) 'CMS)
		(setq user-id (epg-dn-from-string user-id))
	      (setq user-id (rfc6068-unhexify-string user-id)))
	  (error))
	(if entry
	    (setcdr entry user-id)
	  (setq epg-user-id-alist
		(cons (cons key-id user-id) epg-user-id-alist))))
    (epg-context-set-result-for
     context
     'verify
     (cons (epg-make-signature status)
	   (epg-context-result-for context 'verify)))))

(defun epg--status-GOODSIG (context string)
  (epg--status-*SIG context 'good string))

(defun epg--status-EXPSIG (context string)
  (epg--status-*SIG context 'expired string))

(defun epg--status-EXPKEYSIG (context string)
  (epg--status-*SIG context 'expired-key string))

(defun epg--status-REVKEYSIG (context string)
  (epg--status-*SIG context 'revoked-key string))

(defun epg--status-BADSIG (context string)
  (epg--status-*SIG context 'bad string))

(defun epg--status-NO_PUBKEY (context string)
  (if (eq (epg-context-operation context) 'verify)
      (let ((signature (car (epg-context-result-for context 'verify))))
	(if (and signature
		 (eq (epg-signature-status signature) 'error)
		 (equal (epg-signature-key-id signature) string))
	    (setf (epg-signature-status signature) 'no-pubkey)))
    (epg-context-set-result-for
     context 'error
     (cons (cons 'no-pubkey string)
	   (epg-context-result-for context 'error)))))

(defun epg--status-NO_SECKEY (context string)
  (epg-context-set-result-for
   context 'error
   (cons (cons 'no-seckey string)
	 (epg-context-result-for context 'error))))

(defalias 'epg--time-from-seconds #'string-to-number)

(defun epg--status-ERRSIG (context string)
  (if (string-match "\\`\\([^ ]+\\) \\([0-9]+\\) \\([0-9]+\\) \
\\([[:xdigit:]][[:xdigit:]]\\) \\([^ ]+\\) \\([0-9]+\\)"
		    string)
      (let ((signature (epg-make-signature 'error)))
	(epg-context-set-result-for
	 context
	 'verify
	 (cons signature
	       (epg-context-result-for context 'verify)))
	(setf (epg-signature-key-id signature)
              (match-string 1 string))
	(setf (epg-signature-pubkey-algorithm signature)
              (string-to-number (match-string 2 string)))
	(setf (epg-signature-digest-algorithm signature)
              (string-to-number (match-string 3 string)))
	(setf (epg-signature-class signature)
              (string-to-number (match-string 4 string) 16))
	(setf (epg-signature-creation-time signature)
              (epg--time-from-seconds (match-string 5 string))))))

(defun epg--status-VALIDSIG (context string)
  (let ((signature (car (epg-context-result-for context 'verify))))
    (when (and signature
	       (eq (epg-signature-status signature) 'good)
	       (string-match "\\`\\([^ ]+\\) [^ ]+ \\([^ ]+\\) \\([^ ]+\\) \
\\([0-9]+\\) [^ ]+ \\([0-9]+\\) \\([0-9]+\\) \\([[:xdigit:]][[:xdigit:]]\\) \
\\(.*\\)"
			   string))
      (setf (epg-signature-fingerprint signature)
            (match-string 1 string))
      (setf (epg-signature-creation-time signature)
            (epg--time-from-seconds (match-string 2 string)))
      (unless (equal (match-string 3 string) "0")
	(setf (epg-signature-expiration-time signature)
              (epg--time-from-seconds (match-string 3 string))))
      (setf (epg-signature-version signature)
            (string-to-number (match-string 4 string)))
      (setf (epg-signature-pubkey-algorithm signature)
            (string-to-number (match-string 5 string)))
      (setf (epg-signature-digest-algorithm signature)
            (string-to-number (match-string 6 string)))
      (setf (epg-signature-class signature)
            (string-to-number (match-string 7 string) 16)))))

(defun epg--status-TRUST_UNDEFINED (context _string)
  (let ((signature (car (epg-context-result-for context 'verify))))
    (if (and signature
	     (eq (epg-signature-status signature) 'good))
	(setf (epg-signature-validity signature) 'undefined))))

(defun epg--status-TRUST_NEVER (context _string)
  (let ((signature (car (epg-context-result-for context 'verify))))
    (if (and signature
	     (eq (epg-signature-status signature) 'good))
	(setf (epg-signature-validity signature) 'never))))

(defun epg--status-TRUST_MARGINAL (context _string)
  (let ((signature (car (epg-context-result-for context 'verify))))
    (if (and signature
	     (eq (epg-signature-status signature) 'good))
	(setf (epg-signature-validity signature) 'marginal))))

(defun epg--status-TRUST_FULLY (context _string)
  (let ((signature (car (epg-context-result-for context 'verify))))
    (if (and signature
	     (eq (epg-signature-status signature) 'good))
	(setf (epg-signature-validity signature) 'full))))

(defun epg--status-TRUST_ULTIMATE (context _string)
  (let ((signature (car (epg-context-result-for context 'verify))))
    (if (and signature
	     (eq (epg-signature-status signature) 'good))
	(setf (epg-signature-validity signature) 'ultimate))))

(defun epg--status-NOTATION_NAME (context string)
  (let ((signature (car (epg-context-result-for context 'verify))))
    (if signature
        (push (epg-make-sig-notation string nil t nil)
              (epg-signature-notations signature)))))

(defun epg--status-NOTATION_DATA (context string)
  (let ((signature (car (epg-context-result-for context 'verify)))
	notation)
    (if (and signature
	     (setq notation (car (epg-signature-notations signature))))
	(setf (epg-sig-notation-value notation) string))))

(defun epg--status-POLICY_URL (context string)
  (let ((signature (car (epg-context-result-for context 'verify))))
    (if signature
        (push (epg-make-sig-notation nil string t nil)
              (epg-signature-notations signature)))))

(defun epg--status-PROGRESS (context string)
  (if (and (epg-context-progress-callback context)
	   (string-match "\\`\\([^ ]+\\) \\([^ ]\\) \\([0-9]+\\) \\([0-9]+\\)"
			 string))
      (funcall (car (epg-context-progress-callback context))
	       context
	       (match-string 1 string)
	       (match-string 2 string)
	       (string-to-number (match-string 3 string))
	       (string-to-number (match-string 4 string))
	       (cdr (epg-context-progress-callback context)))))

(defun epg--status-ENC_TO (context string)
  (if (string-match "\\`\\([0-9A-Za-z]+\\) \\([0-9]+\\) \\([0-9]+\\)" string)
      (epg-context-set-result-for
       context 'encrypted-to
       (cons (list (match-string 1 string)
		   (string-to-number (match-string 2 string))
		   (string-to-number (match-string 3 string)))
	     (epg-context-result-for context 'encrypted-to)))))

(defun epg--status-DECRYPTION_FAILED (context _string)
  (epg-context-set-result-for context 'decryption-failed t))

(defun epg--status-DECRYPTION_OKAY (context _string)
  (epg-context-set-result-for context 'decryption-okay t))

(defun epg--status-NODATA (context string)
  (epg-context-set-result-for
   context 'error
   (cons (cons 'no-data (string-to-number string))
	 (epg-context-result-for context 'error))))

(defun epg--status-UNEXPECTED (context string)
  (epg-context-set-result-for
   context 'error
   (cons (cons 'unexpected (string-to-number string))
	 (epg-context-result-for context 'error))))

(defun epg--status-KEYEXPIRED (context string)
  (epg-context-set-result-for
   context 'key
   (cons (list 'key-expired (cons 'expiration-time
				  (epg--time-from-seconds string)))
	 (epg-context-result-for context 'key))))

(defun epg--status-KEYREVOKED (context _string)
  (epg-context-set-result-for
   context 'key
   (cons '(key-revoked)
	 (epg-context-result-for context 'key))))

(defun epg--status-BADARMOR (context _string)
  (epg-context-set-result-for
   context 'error
   (cons '(bad-armor)
	 (epg-context-result-for context 'error))))

(defun epg--status-INV_RECP (context string)
  (if (string-match "\\`\\([0-9]+\\) \\(.*\\)" string)
      (epg-context-set-result-for
       context 'error
       (cons (list 'invalid-recipient
		   (cons 'reason
			 (string-to-number (match-string 1 string)))
		   (cons 'requested
			 (match-string 2 string)))
	     (epg-context-result-for context 'error)))))

(defun epg--status-INV_SGNR (context string)
  (if (string-match "\\`\\([0-9]+\\) \\(.*\\)" string)
      (epg-context-set-result-for
       context 'error
       (cons (list 'invalid-signer
		   (cons 'reason
			 (string-to-number (match-string 1 string)))
		   (cons 'requested
			 (match-string 2 string)))
	     (epg-context-result-for context 'error)))))

(defun epg--status-NO_RECP (context _string)
  (epg-context-set-result-for
   context 'error
   (cons '(no-recipients)
	 (epg-context-result-for context 'error))))

(defun epg--status-NO_SGNR (context _string)
  (epg-context-set-result-for
   context 'error
   (cons '(no-signers)
	 (epg-context-result-for context 'error))))

(defun epg--status-DELETE_PROBLEM (context string)
  (if (string-match "\\`\\([0-9]+\\)" string)
      (epg-context-set-result-for
       context 'error
       (cons (cons 'delete-problem
		   (string-to-number (match-string 1 string)))
	     (epg-context-result-for context 'error)))))

(defun epg--status-SIG_CREATED (context string)
  (if (string-match "\\`\\([DCS]\\) \\([0-9]+\\) \\([0-9]+\\) \
\\([[:xdigit:]][[:xdigit:]]\\) \\(.*\\) " string)
      (epg-context-set-result-for
       context 'sign
       (cons (epg-make-new-signature
	      (cdr (assq (aref (match-string 1 string) 0)
			 epg-new-signature-type-alist))
	      (string-to-number (match-string 2 string))
	      (string-to-number (match-string 3 string))
	      (string-to-number (match-string 4 string) 16)
	      (epg--time-from-seconds (match-string 5 string))
	      (substring string (match-end 0)))
	     (epg-context-result-for context 'sign)))))

(defun epg--status-KEY_CREATED (context string)
  (if (string-match "\\`\\([BPS]\\) \\([^ ]+\\)" string)
      (epg-context-set-result-for
       context 'generate-key
       (cons (list (cons 'type (string-to-char (match-string 1 string)))
		   (cons 'fingerprint (match-string 2 string)))
	     (epg-context-result-for context 'generate-key)))))

(defun epg--status-KEY_NOT_CREATED (context _string)
  (epg-context-set-result-for
   context 'error
   (cons '(key-not-created)
	 (epg-context-result-for context 'error))))

(defun epg--status-IMPORTED (_context string)
  (if (string-match "\\`\\([^ ]+\\) \\(.*\\)" string)
      (let* ((key-id (match-string 1 string))
	     (user-id (match-string 2 string))
	     (entry (assoc key-id epg-user-id-alist)))
	(condition-case nil
	    (setq user-id (rfc6068-unhexify-string user-id))
	  (error))
	(if entry
	    (setcdr entry user-id)
	  (setq epg-user-id-alist (cons (cons key-id user-id)
					epg-user-id-alist))))))

(defun epg--status-IMPORT_OK (context string)
  (if (string-match "\\`\\([0-9]+\\)\\( \\(.+\\)\\)?" string)
      (let ((reason (string-to-number (match-string 1 string))))
	(epg-context-set-result-for
	 context 'import-status
	 (cons (epg-make-import-status (if (match-beginning 2)
					   (match-string 3 string))
				       nil
				       (/= (logand reason 1) 0)
				       (/= (logand reason 2) 0)
				       (/= (logand reason 4) 0)
				       (/= (logand reason 8) 0)
				       (/= (logand reason 16) 0))
	       (epg-context-result-for context 'import-status))))))

(defun epg--status-IMPORT_PROBLEM (context string)
  (if (string-match "\\`\\([0-9]+\\)\\( \\(.+\\)\\)?" string)
      (epg-context-set-result-for
       context 'import-status
       (cons (epg-make-import-status
	      (if (match-beginning 2)
		  (match-string 3 string))
	      (string-to-number (match-string 1 string)))
	     (epg-context-result-for context 'import-status)))))

(defun epg--status-IMPORT_RES (context string)
  (when (string-match "\\`\\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) \
\\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) \
\\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)" string)
    (epg-context-set-result-for
     context 'import
     (epg-make-import-result (string-to-number (match-string 1 string))
			     (string-to-number (match-string 2 string))
			     (string-to-number (match-string 3 string))
			     (string-to-number (match-string 4 string))
			     (string-to-number (match-string 5 string))
			     (string-to-number (match-string 6 string))
			     (string-to-number (match-string 7 string))
			     (string-to-number (match-string 8 string))
			     (string-to-number (match-string 9 string))
			     (string-to-number (match-string 10 string))
			     (string-to-number (match-string 11 string))
			     (string-to-number (match-string 12 string))
			     (string-to-number (match-string 13 string))
			     (epg-context-result-for context 'import-status)))
    (epg-context-set-result-for context 'import-status nil)))

;;; Functions

(defun epg--list-keys-1 (context name mode)
  (let ((args (append (if (epg-context-home-directory context)
			  (list "--homedir"
				(epg-context-home-directory context)))
		      '("--with-colons" "--no-greeting" "--batch"
			"--with-fingerprint" "--with-fingerprint")
		      (unless (eq (epg-context-protocol context) 'CMS)
			'("--fixed-list-mode"))))
	(list-keys-option (if (memq mode '(t secret))
			      "--list-secret-keys"
			    (if (memq mode '(nil public))
				"--list-keys"
			      "--list-sigs")))
	(coding-system-for-read 'binary)
	keys string field index)
    (if name
	(progn
	  (unless (listp name)
	    (setq name (list name)))
	  (while name
	    (setq args (append args (list list-keys-option (car name)))
		  name (cdr name))))
      (setq args (append args (list list-keys-option))))
    (with-temp-buffer
      (apply #'call-process
	     (epg-context-program context)
	     nil (list t nil) nil args)
      (goto-char (point-min))
      (while (re-search-forward "^[a-z][a-z][a-z]:.*" nil t)
	(setq keys (cons (make-vector 15 nil) keys)
	      string (match-string 0)
	      index 0
	      field 0)
	(while (and (< field (length (car keys)))
		    (eq index
			(string-match "\\([^:]+\\)?:" string index)))
	  (setq index (match-end 0))
	  (aset (car keys) field (match-string 1 string))
	  (setq field (1+ field))))
      (nreverse keys))))

(defun epg--make-sub-key-1 (line)
  (epg-make-sub-key
   (if (aref line 1)
       (cdr (assq (string-to-char (aref line 1)) epg-key-validity-alist)))
   (delq nil
	 (mapcar (lambda (char) (cdr (assq char epg-key-capability-alist)))
		 (aref line 11)))
   (member (aref line 0) '("sec" "ssb"))
   (string-to-number (aref line 3))
   (string-to-number (aref line 2))
   (aref line 4)
   (epg--time-from-seconds (aref line 5))
   (if (aref line 6)
       (epg--time-from-seconds (aref line 6)))))

;;; Public Functions

(defun epg-list-keys (context &optional name mode)
  "Return a list of epg-key objects matched with NAME.
If MODE is nil or `public', only public keyring should be searched.
If MODE is t or `secret', only secret keyring should be searched.
Otherwise, only public keyring should be searched and the key
signatures should be included.
NAME is either a string or a list of strings."
  (let ((lines (epg--list-keys-1 context name mode))
	keys cert pointer pointer-1 index string)
    (while lines
      (cond
       ((member (aref (car lines) 0) '("pub" "sec" "crt" "crs"))
	(setq cert (member (aref (car lines) 0) '("crt" "crs"))
	      keys (cons (epg-make-key
			  (if (aref (car lines) 8)
			      (cdr (assq (string-to-char (aref (car lines) 8))
					 epg-key-validity-alist))))
			 keys))
        (push (epg--make-sub-key-1 (car lines))
              (epg-key-sub-key-list (car keys))))
       ((member (aref (car lines) 0) '("sub" "ssb"))
        (push (epg--make-sub-key-1 (car lines))
              (epg-key-sub-key-list (car keys))))
       ((equal (aref (car lines) 0) "uid")
	;; Decode the UID name as a backslash escaped UTF-8 string,
	;; generated by GnuPG/GpgSM.
	(setq string (copy-sequence (aref (car lines) 9))
	      index 0)
	(while (string-match "\"" string index)
	  (setq string (replace-match "\\\"" t t string)
		index (1+ (match-end 0))))
	(condition-case nil
	    (setq string (decode-coding-string
			  (car (read-from-string (concat "\"" string "\"")))
			  'utf-8))
	  (error
	   (setq string (aref (car lines) 9))))
        (push (epg-make-user-id
               (if (aref (car lines) 1)
                   (cdr (assq (string-to-char (aref (car lines) 1))
                              epg-key-validity-alist)))
               (if cert
                   (condition-case nil
                       (epg-dn-from-string string)
                     (error string))
                 string))
              (epg-key-user-id-list (car keys))))
       ((equal (aref (car lines) 0) "fpr")
	(setf (epg-sub-key-fingerprint (car (epg-key-sub-key-list (car keys))))
              (aref (car lines) 9)))
       ((equal (aref (car lines) 0) "sig")
        (push
         (epg-make-key-signature
          (if (aref (car lines) 1)
              (cdr (assq (string-to-char (aref (car lines) 1))
                         epg-key-validity-alist)))
          (string-to-number (aref (car lines) 3))
          (aref (car lines) 4)
          (epg--time-from-seconds (aref (car lines) 5))
          (epg--time-from-seconds (aref (car lines) 6))
          (aref (car lines) 9)
          (string-to-number (aref (car lines) 10) 16)
          (eq (aref (aref (car lines) 10) 2) ?x))
         (epg-user-id-signature-list
          (car (epg-key-user-id-list (car keys)))))))
      (setq lines (cdr lines)))
    (setq keys (nreverse keys)
	  pointer keys)
    (while pointer
      (cl-callf nreverse (epg-key-sub-key-list (car pointer)))
      (setq pointer-1 (cl-callf nreverse (epg-key-user-id-list (car pointer))))
      (while pointer-1
	(cl-callf nreverse (epg-user-id-signature-list (car pointer-1)))
	(setq pointer-1 (cdr pointer-1)))
      (setq pointer (cdr pointer)))
    keys))

(defun epg--filter-revoked-keys (keys)
  (mapcar
   (lambda (key)
     ;; We have something revoked, so copy the key and remove the
     ;; revoked bits.
     (if (seq-find (lambda (user)
                     (eq (epg-user-id-validity user) 'revoked))
                   (epg-key-user-id-list key))
         (let ((copy (copy-epg-key key)))
           (setf (epg-key-user-id-list copy)
                 (seq-remove (lambda (user)
                               (eq (epg-user-id-validity user) 'revoked))
                             (epg-key-user-id-list copy)))
           copy)
       ;; Nothing to delete; return the key.
       key))
   keys))

(defun epg--args-from-sig-notations (notations)
  (apply #'nconc
	 (mapcar
	  (lambda (notation)
	    (if (and (epg-sig-notation-name notation)
		     (not (epg-sig-notation-human-readable notation)))
		(error "Unreadable"))
	    (if (epg-sig-notation-name notation)
		(list "--sig-notation"
		      (if (epg-sig-notation-critical notation)
			  (concat "!" (epg-sig-notation-name notation)
				  "=" (epg-sig-notation-value notation))
			(concat (epg-sig-notation-name notation)
				"=" (epg-sig-notation-value notation))))
	      (list "--sig-policy-url"
		    (if (epg-sig-notation-critical notation)
			(concat "!" (epg-sig-notation-value notation))
		      (epg-sig-notation-value notation)))))
	  notations)))

(defun epg-cancel (context)
  (if (buffer-live-p (process-buffer (epg-context-process context)))
      (with-current-buffer (process-buffer (epg-context-process context))
	(epg-context-set-result-for
	 epg-context 'error
	 (cons '(quit)
	       (epg-context-result-for epg-context 'error)))))
  (if (eq (process-status (epg-context-process context)) 'run)
      (delete-process (epg-context-process context))))

(defun epg-start-decrypt (context cipher)
  "Initiate a decrypt operation on CIPHER.
CIPHER must be a file data object.

If you use this function, you will need to wait for the completion of
`epg-gpg-program' by using `epg-wait-for-completion' and call
`epg-reset' to clear a temporary output file.
If you are unsure, use synchronous version of this function
`epg-decrypt-file' or `epg-decrypt-string' instead."
  (unless (epg-data-file cipher)
    (error "Not a file"))
  (setf (epg-context-operation context) 'decrypt)
  (setf (epg-context-result context) nil)
  (epg--start context (list "--decrypt" "--" (epg-data-file cipher)))
  ;; `gpgsm' does not read passphrase from stdin, so waiting is not needed.
  (unless (eq (epg-context-protocol context) 'CMS)
    (epg-wait-for-status context '("BEGIN_DECRYPTION"))))

(defun epg--check-error-for-decrypt (context)
  (let ((errors (epg-context-result-for context 'error)))
    (if (epg-context-result-for context 'decryption-failed)
	(signal 'epg-error
		(list "Decryption failed" (epg-errors-to-string errors))))
    (unless (epg-context-result-for context 'decryption-okay)
      (signal 'epg-error
	      (list "Can't decrypt" (epg-errors-to-string errors))))))

(defun epg-decrypt-file (context cipher plain)
  "Decrypt a file CIPHER and store the result to a file PLAIN.
If PLAIN is nil, it returns the result as a string."
  (unwind-protect
      (progn
	(setf (epg-context-output-file context)
              (or plain (make-temp-file "epg-output")))
	(epg-start-decrypt context (epg-make-data-from-file cipher))
	(epg-wait-for-completion context)
	(epg--check-error-for-decrypt context)
	(unless plain
	  (epg-read-output context)))
    (unless plain
      (epg-delete-output-file context))
    (epg-reset context)))

(defun epg-decrypt-string (context cipher)
  "Decrypt a string CIPHER and return the plain text."
  (let ((input-file (make-temp-file "epg-input"))
	(coding-system-for-write 'binary))
    (unwind-protect
	(progn
	  (write-region cipher nil input-file nil 'quiet)
	  (setf (epg-context-output-file context)
                (make-temp-file "epg-output"))
	  (epg-start-decrypt context (epg-make-data-from-file input-file))
	  (epg-wait-for-completion context)
	  (epg--check-error-for-decrypt context)
	  (epg-read-output context))
      (epg-delete-output-file context)
      (if (file-exists-p input-file)
	  (delete-file input-file))
      (epg-reset context))))

(defun epg-start-verify (context signature &optional signed-text)
  "Initiate a verify operation on SIGNATURE.
SIGNATURE and SIGNED-TEXT are a data object if they are specified.

For a detached signature, both SIGNATURE and SIGNED-TEXT should be set.
For a normal or a cleartext signature, SIGNED-TEXT should be nil.

If you use this function, you will need to wait for the completion of
`epg-gpg-program' by using `epg-wait-for-completion' and call
`epg-reset' to clear a temporary output file.
If you are unsure, use synchronous version of this function
`epg-verify-file' or `epg-verify-string' instead."
  (setf (epg-context-operation context) 'verify)
  (setf (epg-context-result context) nil)
  (if signed-text
      ;; Detached signature.
      (if (epg-data-file signed-text)
	  (epg--start context (list "--verify" "--" (epg-data-file signature)
				   (epg-data-file signed-text)))
	(epg--start context (list "--verify" "--" (epg-data-file signature)
				  "-"))
	(if (eq (process-status (epg-context-process context)) 'run)
	    (process-send-string (epg-context-process context)
				 (epg-data-string signed-text)))
	(if (eq (process-status (epg-context-process context)) 'run)
	    (process-send-eof (epg-context-process context))))
    ;; Normal (or cleartext) signature.
    (if (epg-data-file signature)
	(epg--start context (if (eq (epg-context-protocol context) 'CMS)
				(list "--verify" "--" (epg-data-file signature))
			      (list "--" (epg-data-file signature))))
      (epg--start context (if (eq (epg-context-protocol context) 'CMS)
			      '("--verify" "-")
			    '("-")))
      (if (eq (process-status (epg-context-process context)) 'run)
	  (process-send-string (epg-context-process context)
			       (epg-data-string signature)))
      (if (eq (process-status (epg-context-process context)) 'run)
	  (process-send-eof (epg-context-process context))))))

(defun epg-verify-file (context signature &optional signed-text plain)
  "Verify a file SIGNATURE.
SIGNED-TEXT and PLAIN are also a file if they are specified.

For a detached signature, both SIGNATURE and SIGNED-TEXT should be
string.  For a normal or a cleartext signature, SIGNED-TEXT should be
nil.  In the latter case, if PLAIN is specified, the plaintext is
stored into the file after successful verification.

Note that this function does not return verification result as t
or nil, nor signal error on failure.  That's a design decision to
handle the case where SIGNATURE has multiple signature.

To check the verification results, use `epg-context-result-for' as follows:

\(epg-context-result-for context \\='verify)

which will return a list of `epg-signature' object."
  (unwind-protect
      (progn
        (setf (epg-context-output-file context)
              (or plain (make-temp-file "epg-output")))
	(if signed-text
	    (epg-start-verify context
			      (epg-make-data-from-file signature)
			      (epg-make-data-from-file signed-text))
	  (epg-start-verify context
			    (epg-make-data-from-file signature)))
	(epg-wait-for-completion context)
	(unless plain
	  (epg-read-output context)))
    (unless plain
      (epg-delete-output-file context))
    (epg-reset context)))

(defun epg-verify-string (context signature &optional signed-text)
  "Verify a string SIGNATURE.
SIGNED-TEXT is a string if it is specified.

For a detached signature, both SIGNATURE and SIGNED-TEXT should be
string.  For a normal or a cleartext signature, SIGNED-TEXT should be
nil.  In the latter case, this function returns the plaintext after
successful verification.

Note that this function does not return verification result as t
or nil, nor signal error on failure.  That's a design decision to
handle the case where SIGNATURE has multiple signature.

To check the verification results, use `epg-context-result-for' as follows:

\(epg-context-result-for context \\='verify)

which will return a list of `epg-signature' object."
  (let ((coding-system-for-write 'binary)
	input-file)
    (unwind-protect
	(progn
	  (setf (epg-context-output-file context)
                (make-temp-file "epg-output"))
	  (if signed-text
	      (progn
		(setq input-file (make-temp-file "epg-signature"))
		(write-region signature nil input-file nil 'quiet)
		(epg-start-verify context
				  (epg-make-data-from-file input-file)
				  (epg-make-data-from-string signed-text)))
	    (epg-start-verify context (epg-make-data-from-string signature)))
	  (epg-wait-for-completion context)
	  (epg-read-output context))
      (epg-delete-output-file context)
      (if (and input-file
	       (file-exists-p input-file))
	  (delete-file input-file))
      (epg-reset context))))

(defun epg-start-sign (context plain &optional mode)
  "Initiate a sign operation on PLAIN.
PLAIN is a data object.

If optional 3rd argument MODE is t or `detached', it makes a detached signature.
If it is nil or `normal', it makes a normal signature.
Otherwise, it makes a cleartext signature.

If you use this function, you will need to wait for the completion of
`epg-gpg-program' by using `epg-wait-for-completion' and call
`epg-reset' to clear a temporary output file.
If you are unsure, use synchronous version of this function
`epg-sign-file' or `epg-sign-string' instead."
  (setf (epg-context-operation context) 'sign)
  (setf (epg-context-result context) nil)
  (unless (memq mode '(t detached nil normal)) ;i.e. cleartext
    (setf (epg-context-armor context) nil)
    (setf (epg-context-textmode context) nil))
  (epg--start context
	     (append (list (if (memq mode '(t detached))
			       "--detach-sign"
			     (if (memq mode '(nil normal))
				 "--sign"
			       "--clearsign")))
		     (apply #'nconc
			    (mapcar
			     (lambda (signer)
			       (list "-u"
				     (epg-sub-key-id
				      (car (epg-key-sub-key-list signer)))))
			     (epg-context-signers context)))
                     (let ((sender (epg-context-sender context)))
                       (when (and (eql 'OpenPGP (epg-context-protocol context))
                                  (epg-required-version-p 'OpenPGP "2.1.15")
                                  (stringp sender))
                         (list "--sender" sender)))
		     (epg--args-from-sig-notations
		      (epg-context-sig-notations context))
		     (if (epg-data-file plain)
			 (list "--" (epg-data-file plain)))))
  ;; `gpgsm' does not read passphrase from stdin, so waiting is not needed.
  (unless (eq (epg-context-protocol context) 'CMS)
    (epg-wait-for-status context '("BEGIN_SIGNING")))
  (when (epg-data-string plain)
    (if (eq (process-status (epg-context-process context)) 'run)
	(process-send-string (epg-context-process context)
			     (epg-data-string plain)))
    (if (eq (process-status (epg-context-process context)) 'run)
	(process-send-eof (epg-context-process context)))))

(defun epg-sign-file (context plain signature &optional mode)
  "Sign a file PLAIN and store the result to a file SIGNATURE.
If SIGNATURE is nil, it returns the result as a string.
If optional 3rd argument MODE is t or `detached', it makes a detached signature.
If it is nil or `normal', it makes a normal signature.
Otherwise, it makes a cleartext signature."
  (unwind-protect
      (progn
        (setf (epg-context-output-file context)
              (or signature (make-temp-file "epg-output")))
	(epg-start-sign context (epg-make-data-from-file plain) mode)
	(epg-wait-for-completion context)
	(unless (epg-context-result-for context 'sign)
	  (let ((errors (epg-context-result-for context 'error)))
	    (signal 'epg-error
		    (list "Sign failed" (epg-errors-to-string errors)))))
	(unless signature
	  (epg-read-output context)))
    (unless signature
      (epg-delete-output-file context))
    (epg-reset context)))

(defun epg-sign-string (context plain &optional mode)
  "Sign a string PLAIN and return the output as string.
If optional 3rd argument MODE is t or `detached', it makes a detached signature.
If it is nil or `normal', it makes a normal signature.
Otherwise, it makes a cleartext signature."
  (let ((input-file
	 (unless (eq (epg-context-protocol context) 'CMS)
	   (make-temp-file "epg-input")))
	(coding-system-for-write 'binary))
    (unwind-protect
	(progn
	  (setf (epg-context-output-file context)
                (make-temp-file "epg-output"))
	  (if input-file
	      (write-region plain nil input-file nil 'quiet))
	  (epg-start-sign context
			  (if input-file
			      (epg-make-data-from-file input-file)
			    (epg-make-data-from-string plain))
			  mode)
	  (epg-wait-for-completion context)
	  (unless (epg-context-result-for context 'sign)
	    (if (epg-context-result-for context 'error)
		(let ((errors (epg-context-result-for context 'error)))
		  (signal 'epg-error
			  (list "Sign failed" (epg-errors-to-string errors))))
              (signal 'epg-error '("Signing failed (unknown reason)"))))
	  (epg-read-output context))
      (epg-delete-output-file context)
      (if input-file
	  (delete-file input-file))
      (epg-reset context))))

(defun epg-start-encrypt (context plain recipients
				  &optional sign always-trust)
  "Initiate an encrypt operation on PLAIN.
PLAIN is a data object.
If RECIPIENTS is nil, it performs symmetric encryption.

If you use this function, you will need to wait for the completion of
`epg-gpg-program' by using `epg-wait-for-completion' and call
`epg-reset' to clear a temporary output file.
If you are unsure, use synchronous version of this function
`epg-encrypt-file' or `epg-encrypt-string' instead."
  (setf (epg-context-operation context) 'encrypt)
  (setf (epg-context-result context) nil)
  (epg--start context
	     (append (if always-trust '("--always-trust"))
		     (if recipients '("--encrypt") '("--symmetric"))
		     (if sign '("--sign"))
		     (if sign
			 (apply #'nconc
				(mapcar
				 (lambda (signer)
				   (list "-u"
					 (epg-sub-key-id
					  (car (epg-key-sub-key-list
						signer)))))
				 (epg-context-signers context))))
		     (if (and sign
                              (eql 'OpenPGP (epg-context-protocol context)))
                         (let ((sender (epg-context-sender context)))
                           (when (and (epg-required-version-p 'OpenPGP "2.1.15")
                                      (stringp sender))
                             (list "--sender" sender))))
                     (if sign
			 (epg--args-from-sig-notations
			  (epg-context-sig-notations context)))
		     (apply #'nconc
			    (mapcar
			     (lambda (recipient)
			       (list "-r"
				     (epg-sub-key-id
				      (car (epg-key-sub-key-list recipient)))))
			     recipients))
		     (if (epg-data-file plain)
			 (list "--" (epg-data-file plain)))))
  ;; `gpgsm' does not read passphrase from stdin, so waiting is not needed.
  (unless (eq (epg-context-protocol context) 'CMS)
    (epg-wait-for-status context
                         (if sign '("BEGIN_SIGNING") '("BEGIN_ENCRYPTION"))))
  (when (epg-data-string plain)
    (if (eq (process-status (epg-context-process context)) 'run)
	(process-send-string (epg-context-process context)
			     (epg-data-string plain)))
    (if (eq (process-status (epg-context-process context)) 'run)
	(process-send-eof (epg-context-process context)))))

(defun epg-encrypt-file (context plain recipients
				 cipher &optional sign always-trust)
  "Encrypt a file PLAIN and store the result to a file CIPHER.
If CIPHER is nil, it returns the result as a string.
If RECIPIENTS is nil, it performs symmetric encryption."
  (unwind-protect
      (progn
        (setf (epg-context-output-file context)
              (or cipher (make-temp-file "epg-output")))
	(epg-start-encrypt context (epg-make-data-from-file plain)
			   recipients sign always-trust)
	(epg-wait-for-completion context)
	(let ((errors (epg-context-result-for context 'error)))
	  (if (and sign
		   (not (epg-context-result-for context 'sign)))
	      (signal 'epg-error
		      (list "Sign failed" (epg-errors-to-string errors))))
	  (if errors
	      (signal 'epg-error
		      (list "Encrypt failed" (epg-errors-to-string errors)))))
	(unless cipher
	  (epg-read-output context)))
    (unless cipher
      (epg-delete-output-file context))
    (epg-reset context)))

(defun epg-encrypt-string (context plain recipients
				   &optional sign always-trust)
  "Encrypt a string PLAIN.
If RECIPIENTS is nil, it performs symmetric encryption."
  (let ((input-file
	 (unless (or (not sign)
		     (eq (epg-context-protocol context) 'CMS))
	   (make-temp-file "epg-input")))
	(coding-system-for-write 'binary))
    (unwind-protect
	(progn
	  (setf (epg-context-output-file context)
                (make-temp-file "epg-output"))
	  (if input-file
	      (write-region plain nil input-file nil 'quiet))
	  (epg-start-encrypt context
			     (if input-file
				 (epg-make-data-from-file input-file)
			       (epg-make-data-from-string plain))
			     recipients sign always-trust)
	  (epg-wait-for-completion context)
	  (let ((errors (epg-context-result-for context 'error)))
	    (if (and sign
		     (not (epg-context-result-for context 'sign)))
		(signal 'epg-error
			(list "Sign failed" (epg-errors-to-string errors))))
	    (if errors
		(signal 'epg-error
			(list "Encrypt failed" (epg-errors-to-string errors)))))
	  (epg-read-output context))
      (epg-delete-output-file context)
      (if input-file
	  (delete-file input-file))
      (epg-reset context))))

(defun epg-start-export-keys (context keys)
  "Initiate an export keys operation.

If you use this function, you will need to wait for the completion of
`epg-gpg-program' by using `epg-wait-for-completion' and call
`epg-reset' to clear a temporary output file.
If you are unsure, use synchronous version of this function
`epg-export-keys-to-file' or `epg-export-keys-to-string' instead."
  (setf (epg-context-operation context) 'export-keys)
  (setf (epg-context-result context) nil)
  (epg--start context (cons "--export"
			   (mapcar
			    (lambda (key)
			      (epg-sub-key-id
			       (car (epg-key-sub-key-list key))))
			    keys))))

(defun epg-export-keys-to-file (context keys file)
  "Extract public KEYS."
  (unwind-protect
      (progn
	(setf (epg-context-output-file context)
              (or file (make-temp-file "epg-output")))
	(epg-start-export-keys context keys)
	(epg-wait-for-completion context)
	(let ((errors (epg-context-result-for context 'error)))
	  (if errors
	      (signal 'epg-error
		      (list "Export keys failed"
			    (epg-errors-to-string errors)))))
	(unless file
	  (epg-read-output context)))
    (unless file
      (epg-delete-output-file context))
    (epg-reset context)))

(defun epg-export-keys-to-string (context keys)
  "Extract public KEYS and return them as a string."
  (epg-export-keys-to-file context keys nil))

(defun epg-start-import-keys (context keys)
  "Initiate an import keys operation.
KEYS is a data object.

If you use this function, you will need to wait for the completion of
`epg-gpg-program' by using `epg-wait-for-completion' and call
`epg-reset' to clear a temporary output file.
If you are unsure, use synchronous version of this function
`epg-import-keys-from-file' or `epg-import-keys-from-string' instead."
  (setf (epg-context-operation context) 'import-keys)
  (setf (epg-context-result context) nil)
  (epg--start context (if (epg-data-file keys)
			  (list "--import" "--" (epg-data-file keys))
			(list "--import")))
  (when (epg-data-string keys)
    (if (eq (process-status (epg-context-process context)) 'run)
	(process-send-string (epg-context-process context)
			     (epg-data-string keys)))
    (if (eq (process-status (epg-context-process context)) 'run)
	(process-send-eof (epg-context-process context)))))

(defun epg--import-keys-1 (context keys)
  (unwind-protect
      (progn
	(epg-start-import-keys context keys)
	(epg-wait-for-completion context)
	(let ((errors (epg-context-result-for context 'error)))
	  (if errors
	      (signal 'epg-error
		      (list "Import keys failed"
			    (epg-errors-to-string errors))))))
    (epg-reset context)))

(defun epg-import-keys-from-file (context keys)
  "Add keys from a file KEYS."
  (epg--import-keys-1 context (epg-make-data-from-file keys)))

(defun epg-import-keys-from-string (context keys)
  "Add keys from a string KEYS."
  (epg--import-keys-1 context (epg-make-data-from-string keys)))

(defun epg-start-receive-keys (context key-id-list)
  "Initiate a receive key operation.
KEY-ID-LIST is a list of key IDs.

If you use this function, you will need to wait for the completion of
`epg-gpg-program' by using `epg-wait-for-completion' and call
`epg-reset' to clear a temporary output file.
If you are unsure, use synchronous version of this function
`epg-receive-keys' instead."
  (setf (epg-context-operation context) 'receive-keys)
  (setf (epg-context-result context) nil)
  (epg--start context (cons "--recv-keys" key-id-list)))

(defun epg-receive-keys (context keys)
  "Add keys from server.
KEYS is a list of key IDs."
  (unwind-protect
      (progn
	(epg-start-receive-keys context keys)
	(epg-wait-for-completion context)
	(let ((errors (epg-context-result-for context 'error)))
	  (if errors
	      (signal 'epg-error
		      (list "Receive keys failed"
			    (epg-errors-to-string errors))))))
    (epg-reset context)))

(defalias 'epg-import-keys-from-server 'epg-receive-keys)

(defun epg-start-delete-keys (context keys &optional allow-secret)
  "Initiate a delete keys operation.

If you use this function, you will need to wait for the completion of
`epg-gpg-program' by using `epg-wait-for-completion' and call
`epg-reset' to clear a temporary output file.
If you are unsure, use synchronous version of this function
`epg-delete-keys' instead."
  (setf (epg-context-operation context) 'delete-keys)
  (setf (epg-context-result context) nil)
  (epg--start context (cons (if allow-secret
			       "--delete-secret-key"
			     "--delete-key")
			    (mapcar
			     (lambda (key)
			       (epg-sub-key-id
				(car (epg-key-sub-key-list key))))
			     keys))))

(defun epg-delete-keys (context keys &optional allow-secret)
  "Delete KEYS from the key ring."
  (unwind-protect
      (progn
	(epg-start-delete-keys context keys allow-secret)
	(epg-wait-for-completion context)
	(let ((errors (epg-context-result-for context 'error)))
	  (if errors
	      (signal 'epg-error
		      (list "Delete keys failed"
			    (epg-errors-to-string errors))))))
    (epg-reset context)))

(defun epg-start-generate-key (context parameters)
  "Initiate a key generation.
PARAMETERS is a string which specifies parameters of the generated key.
See Info node `(gnupg) Unattended GPG key generation' in the
GnuPG manual for the format.

If you use this function, you will need to wait for the completion of
`epg-gpg-program' by using `epg-wait-for-completion' and call
`epg-reset' to clear a temporary output file.
If you are unsure, use synchronous version of this function
`epg-generate-key-from-file' or `epg-generate-key-from-string' instead."
  (setf (epg-context-operation context) 'generate-key)
  (setf (epg-context-result context) nil)
  (if (epg-data-file parameters)
      (epg--start context (list "--batch" "--gen-key" "--"
			       (epg-data-file parameters)))
    (epg--start context '("--batch" "--gen-key"))
    (if (eq (process-status (epg-context-process context)) 'run)
	(process-send-string (epg-context-process context)
			     (epg-data-string parameters)))
    (if (eq (process-status (epg-context-process context)) 'run)
	(process-send-eof (epg-context-process context)))))

(defun epg-generate-key-from-file (context parameters)
  "Generate a new key pair.
PARAMETERS is a file which tells how to create the key."
  (unwind-protect
      (progn
	(epg-start-generate-key context (epg-make-data-from-file parameters))
	(epg-wait-for-completion context)
	(let ((errors (epg-context-result-for context 'error)))
	  (if errors
	      (signal 'epg-error
		      (list "Generate key failed"
			    (epg-errors-to-string errors))))))
    (epg-reset context)))

(defun epg-generate-key-from-string (context parameters)
  "Generate a new key pair.
PARAMETERS is a string which tells how to create the key."
  (unwind-protect
      (progn
	(epg-start-generate-key context (epg-make-data-from-string parameters))
	(epg-wait-for-completion context)
	(let ((errors (epg-context-result-for context 'error)))
	  (if errors
	      (signal 'epg-error
		      (list "Generate key failed"
			    (epg-errors-to-string errors))))))
    (epg-reset context)))

(defun epg-start-edit-key (context key edit-callback handback)
  "Initiate an edit operation on KEY.

EDIT-CALLBACK is called from process filter and takes four
arguments: the context, a status, an argument string, and the
handback argument.

If you use this function, you will need to wait for the completion of
`epg-gpg-program' by using `epg-wait-for-completion' and call
`epg-reset' to clear a temporary output file.
If you are unsure, use synchronous version of this function
`epg-edit-key' instead."
  (setf (epg-context-operation context) 'edit-key)
  (setf (epg-context-result context) nil)
  (setf (epg-context-edit-callback context) (cons edit-callback handback))
  (epg--start context (list "--edit-key"
			    (epg-sub-key-id
			     (car (epg-key-sub-key-list key))))))

(defun epg-edit-key (context key edit-callback handback)
  "Edit KEY in the keyring."
  (unwind-protect
      (progn
	(epg-start-edit-key context key edit-callback handback)
	(epg-wait-for-completion context)
	(let ((errors (epg-context-result-for context 'error)))
	  (if errors
	      (signal 'epg-error
		      (list "Edit key failed"
			    (epg-errors-to-string errors))))))
    (epg-reset context)))

;;; Decode Functions

(defun epg--decode-percent-escape (string)
  (setq string (encode-coding-string string 'raw-text))
  (let ((index 0))
    (while (string-match "%\\(\\(%\\)\\|\\([[:xdigit:]][[:xdigit:]]\\)\\)"
			 string index)
      (if (match-beginning 2)
	  (setq string (replace-match "%" t t string)
		index (1- (match-end 0)))
	(setq string (replace-match
		      (byte-to-string
                       (string-to-number (match-string 3 string) 16))
		      t t string)
	      index (- (match-end 0) 2))))
    string))

(defun epg--decode-percent-escape-as-utf-8 (string)
  (declare (obsolete rfc6068-unhexify-string "28.1"))
  (decode-coding-string (epg--decode-percent-escape string) 'utf-8))

(defun epg--decode-hexstring (string)
  (declare (obsolete rfc6068-unhexify-string "28.1"))
  (let ((index 0))
    (while (eq index (string-match "[[:xdigit:]][[:xdigit:]]" string index))
      (setq string (replace-match (string (string-to-number
					   (match-string 0 string) 16))
				  t t string)
	    index (1- (match-end 0))))
    string))

(defun epg--decode-quotedstring (string)
  (let ((index 0))
    (while (string-match "\\\\\\(\\([,=+<>#;\\\"]\\)\\|\
\\([[:xdigit:]][[:xdigit:]]\\)\\)"
			 string index)
      (if (match-beginning 2)
	  (setq string (replace-match "\\2" t nil string)
		index (1- (match-end 0)))
	(if (match-beginning 3)
	    (setq string (replace-match (string (string-to-number
						 (match-string 0 string) 16))
					t t string)
		  index (- (match-end 0) 2)))))
    string))

(defun epg-dn-from-string (string)
  "Parse STRING as LADPv3 Distinguished Names (RFC2253).
The return value is an alist mapping from types to values."
  (let ((index 0)
	(length (length string))
	alist type value group)
    (while (< index length)
      (if (eq index (string-match "[ \t\n\r]*" string index))
	  (setq index (match-end 0)))
      (if (eq index (string-match
		     "\\([0-9]+\\(\\.[0-9]+\\)*\\)[ \t\n\r]*=[ \t\n\r]*"
		     string index))
	  (setq type (match-string 1 string)
		index (match-end 0))
	(if (eq index (string-match "\\([0-9A-Za-z]+\\)[ \t\n\r]*=[ \t\n\r]*"
				    string index))
	    (setq type (match-string 1 string)
		  index (match-end 0))))
      (unless type
	(error "Invalid type"))
      (if (eq index (string-match
		     "\\([^,=+<>#;\\\"]\\|\\\\.\\)+"
		     string index))
	  (setq index (match-end 0)
		value (epg--decode-quotedstring (match-string 0 string)))
	(if (eq index (string-match "#\\([[:xdigit:]]+\\)" string index))
	    (setq index (match-end 0)
		  value (rfc6068-unhexify-string (match-string 1 string) t))
	  (if (eq index (string-match "\"\\([^\\\"]\\|\\\\.\\)*\""
				      string index))
	      (setq index (match-end 0)
		    value (epg--decode-quotedstring
			   (match-string 0 string))))))
      (if group
	  (if (stringp (car (car alist)))
	      (setcar alist (list (cons type value) (car alist)))
	    (setcar alist (cons (cons type value) (car alist))))
	(if (consp (car (car alist)))
	    (setcar alist (nreverse (car alist))))
	(setq alist (cons (cons type value) alist)
	      type nil
	      value nil))
      (if (eq index (string-match "[ \t\n\r]*\\([,;+]\\)" string index))
	  (setq index (match-end 0)
		group (eq (aref string (match-beginning 1)) ?+))))
    (nreverse alist)))

(defun epg-decode-dn (alist)
  "Convert ALIST returned by `epg-dn-from-string' to a human readable form.
Type names are resolved using `epg-dn-type-alist'."
  (mapconcat
   (lambda (rdn)
     (if (stringp (car rdn))
	 (let ((entry (assoc (car rdn) epg-dn-type-alist)))
	   (if entry
	       (format "%s=%s" (cdr entry) (cdr rdn))
	     (format "%s=%s" (car rdn) (cdr rdn))))
       (concat "(" (epg-decode-dn rdn) ")")))
   alist
   ", "))

(provide 'epg)

;;; epg.el ends here
