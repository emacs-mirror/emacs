;;; gnutls.el --- Support SSL/TLS connections through GnuTLS  -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2022 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Keywords: comm, tls, ssl, encryption
;; Originally-By: Simon Josefsson (See https://josefsson.org/emacs-security/)
;; Thanks-To: Lars Magne Ingebrigtsen <larsi@gnus.org>

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

;; This package provides language bindings for the GnuTLS library
;; using the corresponding core functions in gnutls.c.  It should NOT
;; be used directly, only through open-network-stream.

;; Simple test:
;;
;; (open-gnutls-stream "tls" "tls-buffer" "yourserver.com" "https")
;; (open-gnutls-stream "tls" "tls-buffer" "imap.gmail.com" "imaps")

;;; Code:

(require 'cl-lib)
(require 'puny)

(declare-function network-stream-certificate "network-stream"
                  (host service parameters))

(defgroup gnutls nil
  "Emacs interface to the GnuTLS library."
  :version "24.1"
  :prefix "gnutls-"
  :group 'comm)

(defcustom gnutls-algorithm-priority nil
  "If non-nil, this should be a TLS priority string.
For instance, if you want to skip the \"dhe-rsa\" algorithm,
set this variable to \"normal:-dhe-rsa\".

This variable can be useful for modifying low-level TLS
connection parameters (for instance if you need to connect to a
host that only accepts a specific algorithm).  However, in
general, Emacs network security is handled by the Network
Security Manager (NSM), and the default value of nil delegates
the job of checking the connection security to the NSM.
See Info node `(emacs) Network Security'."
  :type '(choice (const nil)
                 string))

(defcustom gnutls-verify-error nil
  "If non-nil, this should be t or a list of checks per hostname regex.
If nil, the default, failures in certificate verification will be
logged (subject to `gnutls-log-level'), but the connection will be
allowed to proceed.
If the value is a list, it should have the form

   ((HOST-REGEX FLAGS...) (HOST-REGEX FLAGS...) ...)

where each HOST-REGEX is a regular expression to be matched
against the hostname, on a first-match basis, and FLAGS is either
t or a list of one or more verification flags.  The supported
flags and the corresponding conditions to be tested are:

  :trustfiles -- certificate must be issued by a trusted authority.
  :hostname   -- hostname must match presented certificate's host name.
  t           -- all of the above conditions are tested.

If the condition test fails, an error will be signaled.

If the value of this variable is t, every connection will be subjected
to all of the tests described above.

The default value of this variable is nil, which means that no
checks are performed at the gnutls level.  Instead the checks are
performed via `open-network-stream' at a higher level by the
Network Security Manager.  See Info node `(emacs) Network
Security'."
  :version "24.4"
  :type '(choice
          (const t)
          (repeat :tag "List of hostname regexps with flags for each"
           (list
            (choice :tag "Hostname"
                    (const ".*" :tag "Any hostname")
                    regexp)
            (set (const :trustfiles)
                 (const :hostname))))))

(defcustom gnutls-trustfiles
  '(
    "/etc/ssl/certs/ca-certificates.crt"     ; Debian, Ubuntu, Gentoo,
                                             ; Arch, Guix, Parabola
    "/etc/pki/tls/certs/ca-bundle.crt"       ; Fedora and RHEL
    "/etc/ssl/ca-bundle.pem"                 ; Suse
    "/usr/ssl/certs/ca-bundle.crt"           ; Cygwin
    "/usr/local/share/certs/ca-root-nss.crt" ; FreeBSD
    "/etc/ssl/cert.pem"                      ; macOS, Dragora, Parabola
    "/etc/certs/ca-certificates.crt"         ; OpenIndiana
    )
  "List of CA bundle location filenames or a function returning said list.
If a file path contains glob wildcards, they will be expanded.
The files may be in PEM or DER format, as per the GnuTLS documentation.
The files may not exist, in which case they will be ignored."
  :type '(choice (function :tag "Function to produce list of bundle filenames")
                 (repeat (file :tag "Bundle filename"))))

(defcustom gnutls-min-prime-bits nil
  "Minimum number of prime bits accepted by GnuTLS for key exchange.
During a Diffie-Hellman handshake, if the server sends a prime
number with fewer than this number of bits, the handshake is
rejected.  \(The smaller the prime number, the less secure the
key exchange is against man-in-the-middle attacks.)

A value of nil says to use the default GnuTLS value.

The default value of this variable is such that virtually any
connection can be established, whether this connection can be
considered cryptographically \"safe\" or not.  However, Emacs
network security is handled at a higher level via
`open-network-stream' and the Network Security Manager.  See Info
node `(emacs) Network Security'."
  :type '(choice (const :tag "Use default value" nil)
                 (integer :tag "Number of bits" 2048))
  :version "27.1")

(defcustom gnutls-crlfiles
  '(
    "/etc/grid-security/certificates/*.crl.pem"
    )
  "List of CRL file paths or a function returning said list.
If a file path contains glob wildcards, they will be expanded.
The files may be in PEM or DER format, as per the GnuTLS documentation.
The files may not exist, in which case they will be ignored."
  :type '(choice (function :tag "Function to produce list of CRL filenames")
                 (repeat (file :tag "CRL filename")))
  :version "27.1")

(defun open-gnutls-stream (name buffer host service &optional parameters)
  "Open a SSL/TLS connection for a service to a host.
Returns a subprocess-object to represent the connection.
Input and output work as for subprocesses; `delete-process' closes it.
Args are NAME BUFFER HOST SERVICE.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer (or `buffer-name') to associate with the process.
 Process output goes at end of that buffer, unless you specify
 a filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer
Third arg HOST is the name of the host to connect to, or its IP address.
Fourth arg SERVICE is the name of the service desired, or an integer
specifying a port number to connect to.
Fifth arg PARAMETERS is an optional list of keyword/value pairs.
Only :client-certificate, :nowait, and :coding keywords are
recognized, and have the same meaning as for
`open-network-stream'.
For historical reasons PARAMETERS can also be a symbol, which is
interpreted the same as passing a list containing :nowait and the
value of that symbol.

Usage example:

  (with-temp-buffer
    (open-gnutls-stream \"tls\"
                        (current-buffer)
                        \"your server goes here\"
                        \"imaps\"))

This is a very simple wrapper around `gnutls-negotiate'.  See its
documentation for the specific parameters you can use to open a
GnuTLS connection, including specifying the credential type,
trust and key files, and priority string."
  (let* ((parameters
          (cond ((symbolp parameters)
                 (list :nowait parameters))
                ((not (cl-evenp (length parameters)))
                 (error "Malformed keyword list"))
                ((consp parameters)
                 parameters)
                (t
                 (error "Unknown parameter type"))))
         (cert (network-stream-certificate host service parameters))
         (keylist (and cert (list cert)))
         (nowait (plist-get parameters :nowait))
         (process (open-network-stream
                   name buffer host service
                   :nowait nowait
                   :tls-parameters
                   (and nowait
                        (cons 'gnutls-x509pki
                              (gnutls-boot-parameters
                               :type 'gnutls-x509pki
                               :keylist keylist
                               :hostname (puny-encode-domain host))))
                   :coding (plist-get parameters :coding))))
    (if nowait
        process
      (gnutls-negotiate :process process
                        :type 'gnutls-x509pki
                        :keylist keylist
                        :hostname (puny-encode-domain host)))))

(define-error 'gnutls-error "GnuTLS error")

(declare-function gnutls-boot "gnutls.c" (proc type proplist))
(declare-function gnutls-errorp "gnutls.c" (error))
(defvar gnutls-log-level)               ; gnutls.c

(cl-defun gnutls-negotiate
    (&rest spec
           &key process type hostname priority-string
           trustfiles crlfiles keylist min-prime-bits
           verify-flags verify-error verify-hostname-error
           &allow-other-keys)
  "Negotiate a SSL/TLS connection.  Return proc.  Signal gnutls-error.

Note that arguments are passed CL style, :type TYPE instead of just TYPE.

PROCESS is a process returned by `open-network-stream'.
For the meaning of the rest of the parameters, see `gnutls-boot-parameters'."
  (let* ((type (or type 'gnutls-x509pki))
	 ;; The gnutls library doesn't understand files delivered via
	 ;; the special handlers, so ignore all files found via those.
	 (file-name-handler-alist nil)
         (params (gnutls-boot-parameters
                  :type type
                  :hostname hostname
                  :priority-string priority-string
                  :trustfiles trustfiles
                  :crlfiles crlfiles
                  :keylist keylist
                  :min-prime-bits min-prime-bits
                  :verify-flags verify-flags
                  :verify-error verify-error
                  :verify-hostname-error verify-hostname-error))
         ret)
    (gnutls-message-maybe
     (setq ret (gnutls-boot process type
                            (append (list :complete-negotiation t)
                                    params)))
     "boot: %s" params)

    (when (gnutls-errorp ret)
      ;; This is an error from the underlying C code.
      (signal 'gnutls-error (list process ret)))

    process))

(cl-defun gnutls-boot-parameters
    (&rest spec
           &key type hostname priority-string
           trustfiles crlfiles keylist min-prime-bits
           verify-flags verify-error verify-hostname-error
           &allow-other-keys)
  "Return a keyword list of parameters suitable for passing to `gnutls-boot'.

TYPE is `gnutls-x509pki' (default) or `gnutls-anon'.  Use nil for the default.
HOSTNAME is the remote hostname.  It must be a valid string.
PRIORITY-STRING is as per the GnuTLS docs, default is based on \"NORMAL\".
TRUSTFILES is a list of CA bundles.  It defaults to `gnutls-trustfiles'.
CRLFILES is a list of CRL files.
KEYLIST is an alist of (client key file, client cert file) pairs.
MIN-PRIME-BITS is the minimum acceptable size of Diffie-Hellman keys
\(see `gnutls-min-prime-bits' for more information).  Use nil for the
default.

VERIFY-HOSTNAME-ERROR is a backwards compatibility option for
putting `:hostname' in VERIFY-ERROR.

When VERIFY-ERROR is t or a list containing `:trustfiles', an
error will be raised when the peer certificate verification fails
as per GnuTLS' gnutls_certificate_verify_peers2.  Otherwise, only
warnings will be shown about the verification failure.

When VERIFY-ERROR is t or a list containing `:hostname', an error
will be raised when the hostname does not match the presented
certificate's host name.  The exact verification algorithm is a
basic implementation of the matching described in
RFC2818 (HTTPS), which takes into account wildcards, and the
DNSName/IPAddress subject alternative name PKIX extension.  See
GnuTLS' gnutls_x509_crt_check_hostname for details.  Otherwise,
only a warning will be issued.

Note that the list in `gnutls-verify-error', matched against the
HOSTNAME, is the default VERIFY-ERROR.

VERIFY-FLAGS is a numeric OR of verification flags only for
`gnutls-x509pki' connections.  See GnuTLS' x509.h for details;
here's a recent version of the list.

    GNUTLS_VERIFY_DISABLE_CA_SIGN = 1,
    GNUTLS_VERIFY_ALLOW_X509_V1_CA_CRT = 2,
    GNUTLS_VERIFY_DO_NOT_ALLOW_SAME = 4,
    GNUTLS_VERIFY_ALLOW_ANY_X509_V1_CA_CRT = 8,
    GNUTLS_VERIFY_ALLOW_SIGN_RSA_MD2 = 16,
    GNUTLS_VERIFY_ALLOW_SIGN_RSA_MD5 = 32,
    GNUTLS_VERIFY_DISABLE_TIME_CHECKS = 64,
    GNUTLS_VERIFY_DISABLE_TRUSTED_TIME_CHECKS = 128,
    GNUTLS_VERIFY_DO_NOT_ALLOW_X509_V1_CA_CRT = 256

It must be omitted, a number, or nil; if omitted or nil it
defaults to GNUTLS_VERIFY_ALLOW_X509_V1_CA_CRT."
  (let* ((trustfiles (or trustfiles (gnutls-trustfiles)))
         (crlfiles (or crlfiles (gnutls-crlfiles)))
         (maybe-dumbfw (if (memq 'ClientHello\ Padding (gnutls-available-p))
                           ":%DUMBFW"
                         ""))
         (priority-string (or priority-string
                              (cond
                               ((eq type 'gnutls-anon)
                                (concat "NORMAL:+ANON-DH:!ARCFOUR-128"
                                        maybe-dumbfw))
                               ((eq type 'gnutls-x509pki)
                                (if gnutls-algorithm-priority
                                    (upcase gnutls-algorithm-priority)
                                  (concat "NORMAL" maybe-dumbfw))))))
         (verify-error (or verify-error
                           ;; this uses the value of `gnutls-verify-error'
                           (cond
                            ;; if t, pass it on
                            ((eq gnutls-verify-error t)
                             t)
                            ;; if a list, look for hostname matches
                            ((listp gnutls-verify-error)
                             (cadr (cl-find-if (lambda (x)
                                                 (string-match (car x) hostname))
                                               gnutls-verify-error)))
                            ;; else it's nil
                            (t nil))))
         (min-prime-bits (or min-prime-bits gnutls-min-prime-bits)))

    ;; Only add :hostname if `verify-error' is not t, since t
    ;; means "include :hostname" Bug#38602.
    (and verify-hostname-error
         (not (eq verify-error t))
         (push :hostname verify-error))

    `(:priority ,priority-string
                :hostname ,hostname
                :loglevel ,gnutls-log-level
                :min-prime-bits ,min-prime-bits
                :trustfiles ,trustfiles
                :crlfiles ,crlfiles
                :keylist ,keylist
                :verify-flags ,verify-flags
                :verify-error ,verify-error
                :callbacks nil)))

(defun gnutls--get-files (files)
  (cl-loop for f in files
           if f do (setq f (if (functionp f) (funcall f) f))
           append (cl-delete-if-not #'file-exists-p (file-expand-wildcards f t))))

(defun gnutls-trustfiles ()
  "Return a list of usable trustfiles."
  (gnutls--get-files gnutls-trustfiles))

(defun gnutls-crlfiles ()
  "Return a list of usable CRL files."
  (gnutls--get-files gnutls-crlfiles))

(declare-function gnutls-error-string "gnutls.c" (error))

(defun gnutls-message-maybe (doit format &rest params)
  "When DOIT, message with the caller name followed by FORMAT on PARAMS."
  ;; (apply 'debug format (or params '(nil)))
  (when (gnutls-errorp doit)
    (message "%s: (err=[%s] %s) %s"
             "gnutls.el"
             doit (gnutls-error-string doit)
             (apply #'format-message format (or params '(nil))))))

(provide 'gnutls)

;;; gnutls.el ends here
