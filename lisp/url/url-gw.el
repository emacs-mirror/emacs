;;; url-gw.el --- Gateway munging for URL loading
;; Author: Bill Perry <wmperry@gnu.org>
;; Created: $Date: 2002/04/22 09:26:46 $
;; $Revision: 1.8 $
;; Keywords: comm, data, processes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1997, 1998 Free Software Foundation, Inc.
;;;
;;; This file is part of GNU Emacs.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when-compile (require 'cl))
(require 'url-vars)

;; Fixme: support SSH explicitly or via a url-gateway-rlogin-program?

(autoload 'socks-open-network-stream "socks")
(autoload 'open-ssl-stream "ssl")

(defgroup url-gateway nil
  "URL gateway variables"
  :group 'url)

(defcustom url-gateway-local-host-regexp nil
  "*A regular expression specifying local hostnames/machines."
  :type '(choice (const nil) regexp)
  :group 'url-gateway)

(defcustom url-gateway-prompt-pattern
  "^[^#$%>;]*[#$%>;] *" ;; "bash\\|\$ *\r?$\\|> *\r?"
  "*A regular expression matching a shell prompt."
  :type 'regexp
  :group 'url-gateway)

(defcustom url-gateway-rlogin-host nil
  "*What hostname to actually rlog into before doing a telnet."
  :type '(choice (const nil) string)
  :group 'url-gateway)

(defcustom url-gateway-rlogin-user-name nil
  "*Username to log into the remote machine with when using rlogin."
  :type '(choice (const nil) string)
  :group 'url-gateway)

(defcustom url-gateway-rlogin-parameters '("telnet" "-8")
  "*Parameters to `url-open-rlogin'.
This list will be used as the parameter list given to rsh."
  :type '(repeat string)
  :group 'url-gateway)

(defcustom url-gateway-telnet-host nil
  "*What hostname to actually login to before doing a telnet."
  :type '(choice (const nil) string)
  :group 'url-gateway)

(defcustom url-gateway-telnet-parameters '("exec" "telnet" "-8")
  "*Parameters to `url-open-telnet'.
This list will be executed as a command after logging in via telnet."
  :type '(repeat string)
  :group 'url-gateway)

(defcustom url-gateway-telnet-login-prompt "^\r*.?login:"
  "*Prompt that tells us we should send our username when loggin in w/telnet."
  :type 'regexp
  :group 'url-gateway)

(defcustom url-gateway-telnet-password-prompt "^\r*.?password:"
  "*Prompt that tells us we should send our password when loggin in w/telnet."
  :type 'regexp
  :group 'url-gateway)

(defcustom url-gateway-telnet-user-name nil
  "User name to log in via telnet with."
  :type '(choice (const nil) string)
  :group 'url-gateway)

(defcustom url-gateway-telnet-password nil
  "Password to use to log in via telnet with."
  :type '(choice (const nil) string)
  :group 'url-gateway)

(defcustom url-gateway-broken-resolution nil
  "*Whether to use nslookup to resolve hostnames.
This should be used when your version of Emacs cannot correctly use DNS,
but your machine can.  This usually happens if you are running a statically
linked Emacs under SunOS 4.x"
  :type 'boolean
  :group 'url-gateway)

(defcustom url-gateway-nslookup-program "nslookup"
  "*If non-NIL then a string naming nslookup program."
  :type '(choice (const :tag "None" :value nil) string)
  :group 'url-gateway)

;; Stolen from ange-ftp
;;;###autoload
(defun url-gateway-nslookup-host (host)
  "Attempt to resolve the given HOST using nslookup if possible."
  (interactive "sHost:  ")
  (if url-gateway-nslookup-program
      (let ((proc (start-process " *nslookup*" " *nslookup*"
				 url-gateway-nslookup-program host))
	    (res host))
	(process-kill-without-query proc)
	(save-excursion
	  (set-buffer (process-buffer proc))
	  (while (memq (process-status proc) '(run open))
	    (accept-process-output proc))
	  (goto-char (point-min))
	  (if (re-search-forward "Name:.*\nAddress: *\\(.*\\)$" nil t)
	      (setq res (buffer-substring (match-beginning 1)
					  (match-end 1))))
	  (kill-buffer (current-buffer)))
	res)
    host))

;; Stolen from red gnus nntp.el
(defun url-wait-for-string (regexp proc)
  "Wait until string matching REGEXP arrives in process PROC's buffer."
  (let ((buf (current-buffer)))
    (goto-char (point-min))
    (while (not (re-search-forward regexp nil t))
      (accept-process-output proc)
      (set-buffer buf)
      (goto-char (point-min)))))

;; Stolen from red gnus nntp.el
(defun url-open-rlogin (name buffer host service)
  "Open a connection using rsh."
  (if (not (stringp service))
      (setq service (int-to-string service)))
  (let ((proc (if url-gateway-rlogin-user-name
		  (start-process
		   name buffer "rsh"
		   url-gateway-rlogin-host "-l" url-gateway-rlogin-user-name
		   (mapconcat 'identity
			      (append url-gateway-rlogin-parameters
				      (list host service)) " "))
		(start-process
		 name buffer "rsh" url-gateway-rlogin-host
		 (mapconcat 'identity
			    (append url-gateway-rlogin-parameters
				    (list host service))
			    " ")))))
    (set-buffer buffer)
    (url-wait-for-string "^\r*200" proc)
    (beginning-of-line)
    (delete-region (point-min) (point))
    proc))

;; Stolen from red gnus nntp.el
(defun url-open-telnet (name buffer host service)
  (if (not (stringp service))
      (setq service (int-to-string service)))
  (save-excursion
    (set-buffer (get-buffer-create buffer))
    (erase-buffer)
    (let ((proc (start-process name buffer "telnet" "-8"))
	  (case-fold-search t))
      (when (memq (process-status proc) '(open run))
	(process-send-string proc "set escape \^X\n")
	(process-send-string proc (concat
				   "open " url-gateway-telnet-host "\n"))
	(url-wait-for-string url-gateway-telnet-login-prompt proc)
	(process-send-string
	 proc (concat
	       (or url-gateway-telnet-user-name
		   (setq url-gateway-telnet-user-name (read-string "login: ")))
	       "\n"))
	(url-wait-for-string url-gateway-telnet-password-prompt proc)
	(process-send-string
	 proc (concat
	       (or url-gateway-telnet-password
		   (setq url-gateway-telnet-password
			 (funcall url-passwd-entry-func "Password: ")))
	       "\n"))
	(erase-buffer)
	(url-wait-for-string url-gateway-prompt-pattern proc)
	(process-send-string
	 proc (concat (mapconcat 'identity
				 (append url-gateway-telnet-parameters
					 (list host service)) " ") "\n"))
	(url-wait-for-string "^\r*Escape character.*\r*\n+" proc)
	(delete-region (point-min) (match-end 0))
	(process-send-string proc "\^]\n")
	(url-wait-for-string "^telnet" proc)
	(process-send-string proc "mode character\n")
	(accept-process-output proc 1)
	(sit-for 1)
	(goto-char (point-min))
	(forward-line 1)
	(delete-region (point) (point-max)))
      proc)))

;;;###autoload
(defun url-open-stream (name buffer host service)
  "Open a stream to HOST, possibly via a gateway.
Args per `open-network-stream'.
Will not make a connexion if `url-gateway-unplugged' is non-nil."
  (unless url-gateway-unplugged
    (let ((gw-method (if (and url-gateway-local-host-regexp
			      (not (eq 'ssl url-gateway-method))
			      (string-match
			       url-gateway-local-host-regexp
			       host))
			 'native
		       url-gateway-method))
;;; 	;; This hack is for OS/2 Emacs so that it will not do bogus CRLF
;;; 	;; conversions while trying to be 'helpful'
;;; 	(tcp-binary-process-output-services (if (stringp service)
;;; 						(list service)
;;; 					      (list service
;;; 						    (int-to-string service))))

	  ;; An attempt to deal with denied connections, and attempt
	  ;; to reconnect
	  (cur-retries 0)
	  (retry t)
	  (errobj nil)
	  (conn nil))

      ;; If the user told us to do DNS for them, do it.
      (if url-gateway-broken-resolution
	  (setq host (url-gateway-nslookup-host host)))

      (condition-case errobj
	  ;; This is a clean way to ensure the new process inherits the
	  ;; right coding systems in both Emacs and XEmacs.
	  (let ((coding-system-for-read 'binary)
		(coding-system-for-write 'binary))
	    (setq conn (case gw-method
			 (ssl
			  (open-ssl-stream name buffer host service))
			 ((native)
			  (open-network-stream name buffer host service))
			 (socks
			  (socks-open-network-stream name buffer host service))
			 (telnet
			  (url-open-telnet name buffer host service))
			 (rlogin
			  (url-open-rlogin name buffer host service))
			 (otherwise
			  (error "Bad setting of url-gateway-method: %s"
				 url-gateway-method)))))
	(error
	 (setq conn nil)))
      conn)))

(provide 'url-gw)
