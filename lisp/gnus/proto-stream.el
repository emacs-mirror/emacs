;;; proto-stream.el --- negotiating TLS, STARTTLS and other connections

;; Copyright (C) 2010-2011 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: network

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

;; This library is meant to provide the glue between modules that want
;; to establish a network connection to a server for protocols such as
;; IMAP, NNTP, SMTP and POP3.

;; The main problem is that there's more than a couple of interfaces
;; towards doing this.  You have normal, plain connections, which are
;; no trouble at all, but you also have TLS/SSL connections, and you
;; have STARTTLS.  Negotiating this for each protocol can be rather
;; tedious, so this library provides a single entry point, and hides
;; much of the ugliness.

;; Usage example:

;; (open-protocol-stream
;;  "*nnimap*" buffer address port
;;  :type 'network
;;  :capability-command "1 CAPABILITY\r\n"
;;  :success " OK "
;;  :starttls-function
;;  (lambda (capabilities)
;;    (if (not (string-match "STARTTLS" capabilities))
;;        nil
;;      "1 STARTTLS\r\n")))

;;; Code:

(require 'tls)
(require 'starttls)

(declare-function gnutls-negotiate "gnutls"
		  (proc type &optional priority-string trustfiles keyfiles))

;;;###autoload
(defun open-protocol-stream (name buffer host service &rest parameters)
  "Open a network stream to HOST, possibly with encryption.
Normally, return a network process object; with a non-nil
:return-list parameter, return a list instead (see below).

The first four parameters, NAME, BUFFER, HOST, and SERVICE, have
the same meanings as in `open-network-stream'.  The remaining
PARAMETERS should be a sequence of keywords and values:

:type specifies the connection type, one of the following:
  nil or `network'
             -- Begin with an ordinary network connection, and if
                the parameters :success and :capability-command
                are also supplied, try to upgrade to an encrypted
                connection via STARTTLS.  Even if that
                fails (e.g. if HOST does not support TLS), retain
                an unencrypted connection.
  `plain'    -- An ordinary, unencrypted network connection.
  `starttls' -- Begin with an ordinary connection, and try
                upgrading via STARTTLS.  If that fails for any
                reason, drop the connection; in that case the
                returned object is a killed process.
  `tls'      -- A TLS connection.
  `ssl'      -- Equivalent to `tls'.
  `shell'    -- A shell connection.

:return-list specifies this function's return value.
  If omitted or nil, return a process object.  A non-nil means to
  return (PROC . PROPS), where PROC is a process object and PROPS
  is a plist of connection properties, with these keywords:
   :greeting -- the greeting returned by HOST (a string), or nil.
   :capabilities -- a string representing HOST's capabilities,
                    or nil if none could be found.
   :type -- the resulting connection type; `plain' (unencrypted)
            or `tls' (TLS-encrypted).

:end-of-command specifies a regexp matching the end of a command.
  If non-nil, it defaults to \"\\n\".

:success specifies a regexp matching a message indicating a
  successful STARTTLS negotiation.  For instance, the default
  should be \"^3\" for an NNTP connection.

:capability-command specifies a command used to query the HOST
  for its capabilities.  For instance, for IMAP this should be
  \"1 CAPABILITY\\r\\n\".

:starttls-function specifies a function for handling STARTTLS.
  This function should take one parameter, the response to the
  capability command, and should return the command to switch on
  STARTTLS if the server supports STARTTLS, and nil otherwise."
  (let ((type (plist-get parameters :type))
	(return-list (plist-get parameters :return-list)))
    (if (and (not return-list)
	     (or (eq type 'plain)
		 (and (memq type '(nil network))
		      (not (and (plist-get parameters :success)
				(plist-get parameters :capability-command))))))
	;; The simplest case is equivalent to `open-network-stream'.
	(open-network-stream name buffer host service)
      ;; For everything else, refer to proto-stream-open-*.
      (unless (plist-get parameters :end-of-command)
	(setq parameters (append '(:end-of-command "\r\n") parameters)))
      (let* ((connection-function
	      (cond
	       ((eq type 'plain) 'proto-stream-open-plain)
	       ((memq type '(nil network starttls))
		'proto-stream-open-starttls)
	       ((memq type '(tls ssl)) 'proto-stream-open-tls)
	       ((eq type 'shell) 'proto-stream-open-shell)
	       (t (error "Invalid connection type %s" type))))
	     (result (funcall connection-function
			      name buffer host service parameters)))
	(if return-list
	    (list (car result)
		  :greeting     (nth 1 result)
		  :capabilities (nth 2 result)
		  :type         (nth 3 result))
	  (car result))))))

(defun proto-stream-open-plain (name buffer host service parameters)
  (let ((start (with-current-buffer buffer (point)))
	(stream (open-network-stream name buffer host service)))
    (list stream
	  (proto-stream-get-response stream start
				     (plist-get parameters :end-of-command))
	  nil
	  'plain)))

(defun proto-stream-open-starttls (name buffer host service parameters)
  (let* ((start (with-current-buffer buffer (point)))
	 (require-tls    (eq (plist-get parameters :type) 'starttls))
	 (starttls-function  (plist-get parameters :starttls-function))
	 (success-string     (plist-get parameters :success))
	 (capability-command (plist-get parameters :capability-command))
	 (eoc                (plist-get parameters :end-of-command))
	 ;; Return (STREAM GREETING CAPABILITIES RESULTING-TYPE)
	 (stream (open-network-stream name buffer host service))
	 (greeting (proto-stream-get-response stream start eoc))
	 (capabilities (when capability-command
			 (proto-stream-command stream
					       capability-command eoc)))
	 (resulting-type 'plain)
	 starttls-command)

    ;; If we have STARTTLS support, try to upgrade the connection.
    (when (and (or (fboundp 'open-gnutls-stream)
		   (executable-find "gnutls-cli"))
	       capabilities success-string starttls-function
	       (setq starttls-command
		     (funcall starttls-function capabilities)))
      ;; If using external STARTTLS, drop this connection and start
      ;; anew with `starttls-open-stream'.
      (unless (fboundp 'open-gnutls-stream)
	(delete-process stream)
	(setq start (with-current-buffer buffer (point-max)))
	(let* ((starttls-use-gnutls t)
	       (starttls-extra-arguments
		(if require-tls
		    starttls-extra-arguments
		  ;; For opportunistic TLS upgrades, we don't really
		  ;; care about the identity of the peer.
		  (cons "--insecure" starttls-extra-arguments))))
	  (setq stream (starttls-open-stream name buffer host service)))
	(proto-stream-get-response stream start eoc))
      (when (string-match success-string
			  (proto-stream-command stream starttls-command eoc))
	;; The server said it was OK to begin STARTTLS negotiations.
	(if (fboundp 'open-gnutls-stream)
	    (gnutls-negotiate stream nil)
	  (unless (starttls-negotiate stream)
	    (delete-process stream)))
	(if (memq (process-status stream) '(open run))
	    (setq resulting-type 'tls)
	  ;; We didn't successfully negotiate STARTTLS; if TLS
	  ;; isn't demanded, reopen an unencrypted connection.
	  (unless require-tls
	    (setq stream (open-network-stream name buffer host service))
	    (proto-stream-get-response stream start eoc)))
	;; Re-get the capabilities, which may have now changed.
	(setq capabilities
	      (proto-stream-command stream capability-command eoc))))

    ;; If TLS is mandatory, close the connection if it's unencrypted.
    (and require-tls
	 (eq resulting-type 'plain)
	 (delete-process stream))
    ;; Return value:
    (list stream greeting capabilities resulting-type)))

(defun proto-stream-command (stream command eoc)
  (let ((start (with-current-buffer (process-buffer stream) (point-max))))
    (process-send-string stream command)
    (proto-stream-get-response stream start eoc)))

(defun proto-stream-get-response (stream start end-of-command)
  (with-current-buffer (process-buffer stream)
    (save-excursion
      (goto-char start)
      (while (and (memq (process-status stream)
			'(open run))
		  (not (re-search-forward end-of-command nil t)))
	(accept-process-output stream 0 50)
	(goto-char start))
      (if (= start (point))
	  ;; The process died; return nil.
	  nil
	;; Return the data we got back.
	(buffer-substring start (point))))))

(defun proto-stream-open-tls (name buffer host service parameters)
  (with-current-buffer buffer
    (let ((start (point-max))
	  (stream
	   (funcall (if (fboundp 'open-gnutls-stream)
			'open-gnutls-stream
		      'open-tls-stream)
		    name buffer host service))
	  (eoc (plist-get parameters :end-of-command)))
      (if (null stream)
	  (list nil nil nil 'plain)
	;; If we're using tls.el, we have to delete the output from
	;; openssl/gnutls-cli.
	(unless (fboundp 'open-gnutls-stream)
	  (proto-stream-get-response stream start eoc)
	  (goto-char (point-min))
	  (when (re-search-forward eoc nil t)
	    (goto-char (match-beginning 0))
	    (delete-region (point-min) (line-beginning-position))))
	(proto-stream-capability-open start stream parameters 'tls)))))

(defun proto-stream-open-shell (name buffer host service parameters)
  (require 'format-spec)
  (proto-stream-capability-open
   (with-current-buffer buffer (point))
   (let ((process-connection-type nil))
     (start-process name buffer shell-file-name
		    shell-command-switch
		    (format-spec
		     (plist-get parameters :shell-command)
		     (format-spec-make
		      ?s host
		      ?p service))))
   parameters 'plain))

(defun proto-stream-capability-open (start stream parameters stream-type)
  (let* ((capability-command (plist-get parameters :capability-command))
	 (eoc 		     (plist-get parameters :end-of-command))
	 (greeting (proto-stream-get-response stream start eoc)))
    (list stream greeting
	  (and capability-command
	       (proto-stream-command stream capability-command eoc))
	  stream-type)))

(provide 'proto-stream)

;;; proto-stream.el ends here
