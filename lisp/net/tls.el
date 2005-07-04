;;; tls.el --- TLS/SSL support via wrapper around GnuTLS

;; Copyright (C) 1996-1999, 2003, 2005 Free Software Foundation, Inc.

;; Author: Simon Josefsson <simon@josefsson.org>
;; Keywords: comm, tls, gnutls, ssl

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package implements a simple wrapper around "gnutls-cli" to
;; make Emacs support TLS/SSL.
;;
;; Usage is the same as `open-network-stream', i.e.:
;;
;; (setq tmp (open-tls-stream "test" (current-buffer) "news.mozilla.org" 563))
;; ...
;; #<process test>
;; (process-send-string tmp "mode reader\n")
;; 200 secnews.netscape.com Netscape-Collabra/3.52 03615 NNRP ready ...
;; nil
;; (process-send-string tmp "quit\n")
;; 205
;; nil

;; To use this package as a replacement for ssl.el by William M. Perry
;; <wmperry@cs.indiana.edu>, you need to evaluate the following:
;;
;; (defalias 'open-ssl-stream 'open-tls-stream)

;;; Code:

(eval-and-compile
  (autoload 'format-spec "format-spec")
  (autoload 'format-spec-make "format-spec"))

(defgroup tls nil
  "Transport Layer Security (TLS) parameters."
  :group 'comm)

(defcustom tls-program '("gnutls-cli -p %p %h"
			 "gnutls-cli -p %p %h --protocols ssl3")
  "List of strings containing commands to start TLS stream to a host.
Each entry in the list is tried until a connection is successful.
%s is replaced with server hostname, %p with port to connect to.
The program should read input on stdin and write output to
stdout.  Also see `tls-success' for what the program should output
after successful negotiation."
  :type '(repeat string)
  :group 'tls)

(defcustom tls-process-connection-type nil
  "*Value for `process-connection-type' to use when starting TLS process."
  :version "22.1"
  :type 'boolean
  :group 'tls)

(defcustom tls-success "- Handshake was completed"
  "*Regular expression indicating completed TLS handshakes.
The default is what GNUTLS's \"gnutls-cli\" outputs."
  :version "22.1"
  :type 'regexp
  :group 'tls)

(defcustom tls-certtool-program (executable-find "certtool")
  "Name of  GnuTLS certtool.
Used by `tls-certificate-information'."
  :version "22.1"
  :type '(repeat string)
  :group 'tls)

(defun tls-certificate-information (der)
  "Parse X.509 certificate in DER format into an assoc list."
  (let ((certificate (concat "-----BEGIN CERTIFICATE-----\n"
			     (base64-encode-string der)
			     "\n-----END CERTIFICATE-----\n"))
	(exit-code 0))
    (with-current-buffer (get-buffer-create " *certtool*")
      (erase-buffer)
      (insert certificate)
      (setq exit-code (condition-case ()
			  (call-process-region (point-min) (point-max)
					       tls-certtool-program
					       t (list (current-buffer) nil) t
					       "--certificate-info")
			(error -1)))
      (if (/= exit-code 0)
	  nil
	(let ((vals nil))
	  (goto-char (point-min))
	  (while (re-search-forward "^\\([^:]+\\): \\(.*\\)" nil t)
	    (push (cons (match-string 1) (match-string 2)) vals))
	  (nreverse vals))))))

(defun open-tls-stream (name buffer host service)
  "Open a TLS connection for a service to a host.
Returns a subprocess-object to represent the connection.
Input and output work as for subprocesses; `delete-process' closes it.
Args are NAME BUFFER HOST SERVICE.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer (or buffer-name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer
Third arg is name of the host to connect to, or its IP address.
Fourth arg SERVICE is name of the service desired, or an integer
specifying a port number to connect to."
  (let ((cmds tls-program) cmd done)
    (message "Opening TLS connection to `%s'..." host)
    (while (and (not done) (setq cmd (pop cmds)))
      (message "Opening TLS connection with `%s'..." cmd)
      (let* ((process-connection-type tls-process-connection-type)
	     (process (start-process
		       name buffer shell-file-name shell-command-switch
		       (format-spec
			cmd
			(format-spec-make
			 ?h host
			 ?p (if (integerp service)
				(int-to-string service)
			      service)))))
	     response)
	(while (and process
		    (memq (process-status process) '(open run))
		    (save-excursion
		      (set-buffer buffer) ;; XXX "blue moon" nntp.el bug
		      (goto-char (point-min))
		      (not (setq done (re-search-forward tls-success nil t)))))
	  (accept-process-output process 1)
	  (sit-for 1))
	(message "Opening TLS connection with `%s'...%s" cmd
		 (if done "done" "failed"))
	(if done
	    (setq done process)
	  (delete-process process))))
    (message "Opening TLS connection to `%s'...%s"
	     host (if done "done" "failed"))
    done))

(provide 'tls)

;;; arch-tag: 5596d1c4-facc-4bc4-94a9-9863b928d7ac
;;; tls.el ends here
