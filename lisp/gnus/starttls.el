;;; starttls.el --- STARTTLS functions

;; Copyright (C) 1999, 2000 Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Created: 1999/11/20
;; Keywords: TLS, SSL, OpenSSL

;; This file is not part of any package.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This module defines some utility functions for STARTTLS profiles.

;; [RFC 2595] "Using TLS with IMAP, POP3 and ACAP"
;;	by Chris Newman <chris.newman@innosoft.com> (1999/06)

;;; Code:

(defgroup starttls nil
  "Support for `Transport Layer Security' protocol."
  :group 'ssl)

(defcustom starttls-program "starttls"
  "The program to run in a subprocess to open an TLSv1 connection."
  :group 'starttls)

(defcustom starttls-extra-args nil
  "Extra arguments to `starttls-program'"
  :group 'starttls)

(defun starttls-negotiate (process)
  (signal-process (process-id process) 'SIGALRM))

(defun starttls-open-stream (name buffer host service)
  "Open a TLS connection for a service to a host.
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
specifying a port number to connect to."
  (let* ((process-connection-type nil)
	 (process (apply #'start-process
			 name buffer starttls-program
			 host (format "%s" service)
			 starttls-extra-args)))
    (process-kill-without-query process)
    process))

(provide 'starttls)

;;; starttls.el ends here
