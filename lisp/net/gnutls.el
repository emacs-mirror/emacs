;;; gnutls.el --- Support SSL/TLS connections through GnuTLS
;; Copyright (C) 2010 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Keywords: comm, tls, ssl, encryption
;; Originally-By: Simon Josefsson (See http://josefsson.org/emacs-security/)
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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides language bindings for the GnuTLS library
;; using the corresponding core functions in gnutls.c.

;; Simple test:
;;
;; (open-gnutls-stream "tls" "tls-buffer" "yourserver.com" "https")
;; (open-gnutls-stream "tls" "tls-buffer" "imap.gmail.com" "imaps")

;;; Code:

(defgroup gnutls nil
  "Emacs interface to the GnuTLS library."
  :prefix "gnutls-"
  :group 'net-utils)

(defcustom gnutls-log-level 0
  "Logging level to be used by `starttls-negotiate' and GnuTLS."
  :type 'integer
  :group 'gnutls)

(defun open-gnutls-stream (name buffer host service)
  "Open a SSL/TLS connection for a service to a host.
Returns a subprocess-object to represent the connection.
Input and output work as for subprocesses; `delete-process' closes it.
Args are NAME BUFFER HOST SERVICE.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer (or `buffer-name') to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer
Third arg is name of the host to connect to, or its IP address.
Fourth arg SERVICE is name of the service desired, or an integer
specifying a port number to connect to.

This is a very simple wrapper around `gnutls-negotiate'.  See its
documentation for the specific parameters you can use to open a
GnuTLS connection, including specifying the credential type,
trust and key files, and priority string."
  (let ((proc (open-network-stream name buffer host service)))
    (gnutls-negotiate proc 'gnutls-x509pki)))

(defun gnutls-negotiate (proc type &optional priority-string
                              trustfiles keyfiles)
  "Negotiate a SSL/TLS connection.
TYPE is `gnutls-x509pki' (default) or `gnutls-anon'.  Use nil for the default.
PROC is a process returned by `open-network-stream'.
PRIORITY-STRING is as per the GnuTLS docs, default is \"NORMAL\".
TRUSTFILES is a list of CA bundles.
KEYFILES is a list of client keys."
  (let* ((type (or type 'gnutls-x509pki))
         (trusfiles (or trustfiles
                        '("/etc/ssl/certs/ca-certificates.crt")))
         (priority-string (or priority-string
                              (cond
                               ((eq type 'gnutls-anon)
                                "NORMAL:+ANON-DH:!ARCFOUR-128")
                               ((eq type 'gnutls-x509pki)
                                "NORMAL"))))
         (params `(:priority ,priority-string
                             :loglevel ,gnutls-log-level
                             :trustfiles ,trustfiles
                             :keyfiles ,keyfiles
                             :callbacks nil))
         ret)

    (gnutls-message-maybe
     (setq ret (gnutls-boot proc type params))
     "boot: %s")

    proc))

(defun gnutls-message-maybe (doit format &rest params)
  "When DOIT, message with the caller name followed by FORMAT on PARAMS."
  ;; (apply 'debug format (or params '(nil)))
  (when (gnutls-errorp doit)
    (message "%s: (err=[%s] %s) %s"
             "gnutls.el"
             doit (gnutls-error-string doit)
             (apply 'format format (or params '(nil))))))

(provide 'gnutls)

;;; gnutls.el ends here
