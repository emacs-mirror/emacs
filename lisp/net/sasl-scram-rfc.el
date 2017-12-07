;;; sasl-scram-rfc.el --- SCRAM-SHA-1 module for the SASL client framework  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2017 Free Software Foundation, Inc.

;; Author: Magnus Henoch <magnus.henoch@gmail.com>
;; Package: sasl

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

;; This program is implemented from RFC 5802.  It implements the
;; SCRAM-SHA-1 SASL mechanism.
;;
;; RFC 5802 foresees "hash agility", i.e. new mechanisms based on the
;; same protocol but using a different hash function.  Likewise, this
;; module attempts to separate generic and specific functions, which
;; should make it easy to implement any future SCRAM-* SASL mechanism.
;; It should be as simple as copying the SCRAM-SHA-1 section below and
;; replacing all SHA-1 references.
;;
;; This module does not yet implement the variants with channel
;; binding, i.e. SCRAM-*-PLUS.  That would require cooperation from
;; the TLS library.

;;; Code:

(require 'cl-lib)
(require 'sasl)
(require 'hex-util)
(require 'rfc2104)

;;; Generic for SCRAM-*

(defun sasl-scram-client-first-message (client _step)
  (let ((c-nonce (sasl-unique-id)))
    (sasl-client-set-property client 'c-nonce c-nonce))
  (concat
   ;; n = client doesn't support channel binding
   "n,"
   ;; TODO: where would we get authorization id from?
   ","
   (sasl-scram--client-first-message-bare client)))

(defun sasl-scram--client-first-message-bare (client)
  (let ((c-nonce (sasl-client-property client 'c-nonce)))
    (concat
     ;; TODO: saslprep username or disallow non-ASCII characters
     "n=" (sasl-client-name client) ","
     "r=" c-nonce)))

(defun sasl-scram--client-final-message (hash-fun block-length hash-length client step)
  (unless (string-match
	   "^r=\\([^,]+\\),s=\\([^,]+\\),i=\\([0-9]+\\)\\(?:$\\|,\\)"
	   (sasl-step-data step))
    (sasl-error "Unexpected server response"))
  (let* ((hmac-fun (lambda (text key)
		     (decode-hex-string
		      (rfc2104-hash hash-fun block-length hash-length key text))))
	 (step-data (sasl-step-data step))
	 (nonce (match-string 1 step-data))
	 (salt-base64 (match-string 2 step-data))
	 (iteration-count (string-to-number (match-string 3 step-data)))

	 (c-nonce (sasl-client-property client 'c-nonce))
	 ;; no channel binding, no authorization id
	 (cbind-input "n,,"))
    (unless (string-prefix-p c-nonce nonce)
      (sasl-error "Invalid nonce from server"))
    (let* ((client-final-message-without-proof
	    (concat "c=" (base64-encode-string cbind-input) ","
		    "r=" nonce))
	   (password
	    ;; TODO: either apply saslprep or disallow non-ASCII characters
	    (sasl-read-passphrase
	     (format "%s passphrase for %s: "
		     (sasl-mechanism-name (sasl-client-mechanism client))
		     (sasl-client-name client))))
	   (salt (base64-decode-string salt-base64))
	   (salted-password
	    ;; Hi(str, salt, i):
	    (let ((digest (concat salt (string 0 0 0 1)))
		  (xored nil))
	      (dotimes (_i iteration-count xored)
		(setq digest (funcall hmac-fun digest password))
		(setq xored (if (null xored)
				digest
			      (cl-map 'string 'logxor xored digest))))))
	   (client-key
	    (funcall hmac-fun "Client Key" salted-password))
	   (stored-key (decode-hex-string (funcall hash-fun client-key)))
	   (auth-message
	    (concat
	     (sasl-scram--client-first-message-bare client) ","
	     step-data ","
	     client-final-message-without-proof))
	   (client-signature (funcall hmac-fun (encode-coding-string auth-message 'utf-8) stored-key))
	   (client-proof (cl-map 'string 'logxor client-key client-signature))
	   (client-final-message
	    (concat client-final-message-without-proof ","
		    "p=" (base64-encode-string client-proof))))
      (sasl-client-set-property client 'auth-message auth-message)
      (sasl-client-set-property client 'salted-password salted-password)
      client-final-message)))

(defun sasl-scram--authenticate-server (hash-fun block-length hash-length client step)
  (cond
   ((string-match "^e=\\([^,]+\\)" (sasl-step-data step))
    (sasl-error (format "Server error: %s" (match-string 1 (sasl-step-data step)))))
   ((string-match "^v=\\([^,]+\\)" (sasl-step-data step))
    (let* ((hmac-fun (lambda (text key)
		       (decode-hex-string
			(rfc2104-hash hash-fun block-length hash-length key text))))
	   (verifier (base64-decode-string (match-string 1 (sasl-step-data step))))
	   (auth-message (sasl-client-property client 'auth-message))
	   (salted-password (sasl-client-property client 'salted-password))
	   (server-key (funcall hmac-fun "Server Key" salted-password))
	   (expected-server-signature
	    (funcall hmac-fun (encode-coding-string auth-message 'utf-8) server-key)))
      (unless (string= expected-server-signature verifier)
	(sasl-error "Server not authenticated"))))
   (t
    (sasl-error "Invalid response from server"))))

;;; SCRAM-SHA-1

(defconst sasl-scram-sha-1-steps
  '(sasl-scram-client-first-message
    sasl-scram-sha-1-client-final-message
    sasl-scram-sha-1-authenticate-server))

(defun sasl-scram-sha-1-client-final-message (client step)
  (sasl-scram--client-final-message
   ;; HMAC-SHA1 uses block length 64 and hash length 20; see RFC 2104.
   'sha1 64 20 client step))

(defun sasl-scram-sha-1-authenticate-server (client step)
  (sasl-scram--authenticate-server
   'sha1 64 20 client step))

;; This needs to be at the end, because of how `sasl-make-mechanism'
;; handles step function names.
(put 'sasl-scram-sha-1 'sasl-mechanism
     (sasl-make-mechanism "SCRAM-SHA-1" sasl-scram-sha-1-steps))

(put 'sasl-scram-rfc 'sasl-mechanism (get 'sasl-scram-sha-1 'sasl-mechanism))

(provide 'sasl-scram-sha-1)

(provide 'sasl-scram-rfc)
;;; sasl-scram-rfc.el ends here
