;;; sieve-manage.el --- Implementation of the managesive protocol in elisp
;; Copyright (C) 2001, 2003 Free Software Foundation, Inc.

;; Author: Simon Josefsson <simon@josefsson.org>

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

;; This library provides an elisp API for the managesieve network
;; protocol.
;;
;; Currently only the CRAM-MD5 authentication mechanism is supported.
;;
;; The API should be fairly obvious for anyone familiar with the
;; managesieve protocol, interface functions include:
;;
;; `sieve-manage-open'
;; open connection to managesieve server, returning a buffer to be
;; used by all other API functions.
;;
;; `sieve-manage-opened'
;; check if a server is open or not
;;
;; `sieve-manage-close'
;; close a server connection.
;;
;; `sieve-manage-authenticate'
;; `sieve-manage-listscripts'
;; `sieve-manage-deletescript'
;; `sieve-manage-getscript'
;; performs managesieve protocol actions
;;
;; and that's it.  Example of a managesieve session in *scratch*:
;;
;; (setq my-buf (sieve-manage-open "my.server.com"))
;; " *sieve* my.server.com:2000*"
;;
;; (sieve-manage-authenticate "myusername" "mypassword" my-buf)
;; 'auth
;;
;; (sieve-manage-listscripts my-buf)
;; ("vacation" "testscript" ("splitmail") "badscript")
;;
;; References:
;;
;; draft-martin-managesieve-02.txt,
;; "A Protocol for Remotely Managing Sieve Scripts",
;; by Tim Martin.
;;
;; Release history:
;;
;; 2001-10-31 Committed to Oort Gnus.
;; 2002-07-27 Added DELETESCRIPT.  Suggested by Ned Ludd.

;;; Code:

(require 'rfc2104)
(or (fboundp 'md5)
    (require 'md5))
(eval-and-compile
  (autoload 'starttls-open-stream "starttls")
  (autoload 'starttls-negotiate "starttls"))

;; User customizable variables:

(defgroup sieve-manage nil
  "Low-level Managesieve protocol issues."
  :group 'mail
  :prefix "sieve-")

(defcustom sieve-manage-log "*sieve-manage-log*"
  "Name of buffer for managesieve session trace."
  :type 'string
  :group 'sieve-manage)

(defcustom sieve-manage-default-user (user-login-name)
  "Default username to use."
  :type 'string
  :group 'sieve-manage)

(defcustom sieve-manage-server-eol "\r\n"
  "The EOL string sent from the server."
  :type 'string
  :group 'sieve-manage)

(defcustom sieve-manage-client-eol "\r\n"
  "The EOL string we send to the server."
  :type 'string
  :group 'sieve-manage)

(defcustom sieve-manage-streams '(network starttls shell)
  "Priority of streams to consider when opening connection to server."
  :group 'sieve-manage)

(defcustom sieve-manage-stream-alist
  '((network   sieve-manage-network-p          sieve-manage-network-open)
    (shell     sieve-manage-shell-p            sieve-manage-shell-open)
    (starttls  sieve-manage-starttls-p         sieve-manage-starttls-open))
  "Definition of network streams.

\(NAME CHECK OPEN)

NAME names the stream, CHECK is a function returning non-nil if the
server support the stream and OPEN is a function for opening the
stream."
  :group 'sieve-manage)

(defcustom sieve-manage-authenticators '(cram-md5 plain)
  "Priority of authenticators to consider when authenticating to server."
  :group 'sieve-manage)

(defcustom sieve-manage-authenticator-alist
  '((cram-md5   sieve-manage-cram-md5-p       sieve-manage-cram-md5-auth)
    (plain      sieve-manage-plain-p          sieve-manage-plain-auth))
  "Definition of authenticators.

\(NAME CHECK AUTHENTICATE)

NAME names the authenticator.  CHECK is a function returning non-nil if
the server support the authenticator and AUTHENTICATE is a function
for doing the actual authentication."
  :group 'sieve-manage)

(defcustom sieve-manage-default-port 2000
  "Default port number for managesieve protocol."
  :type 'integer
  :group 'sieve-manage)

;; Internal variables:

(defconst sieve-manage-local-variables '(sieve-manage-server
					 sieve-manage-port
					 sieve-manage-auth
					 sieve-manage-stream
					 sieve-manage-username
					 sieve-manage-password
					 sieve-manage-process
					 sieve-manage-client-eol
					 sieve-manage-server-eol
					 sieve-manage-capability))
(defconst sieve-manage-default-stream 'network)
(defconst sieve-manage-coding-system-for-read 'binary)
(defconst sieve-manage-coding-system-for-write 'binary)
(defvar sieve-manage-stream nil)
(defvar sieve-manage-auth nil)
(defvar sieve-manage-server nil)
(defvar sieve-manage-port nil)
(defvar sieve-manage-username nil)
(defvar sieve-manage-password nil)
(defvar sieve-manage-state 'closed
  "Managesieve state.
Valid states are `closed', `initial', `nonauth', and `auth'.")
(defvar sieve-manage-process nil)
(defvar sieve-manage-capability nil)

;; Internal utility functions

(defsubst sieve-manage-disable-multibyte ()
  "Enable multibyte in the current buffer."
  (when (fboundp 'set-buffer-multibyte)
    (set-buffer-multibyte nil)))

;; Uses the dynamically bound `reason' variable.
(defvar reason)
(defun sieve-manage-interactive-login (buffer loginfunc)
  "Login to server in BUFFER.
LOGINFUNC is passed a username and a password, it should return t if
it where sucessful authenticating itself to the server, nil otherwise.
Returns t if login was successful, nil otherwise."
  (with-current-buffer buffer
    (make-variable-buffer-local 'sieve-manage-username)
    (make-variable-buffer-local 'sieve-manage-password)
    (let (user passwd ret reason)
      ;;      (condition-case ()
      (while (or (not user) (not passwd))
	(setq user (or sieve-manage-username
		       (read-from-minibuffer
			(concat "Managesieve username for "
				sieve-manage-server ": ")
			(or user sieve-manage-default-user))))
	(setq passwd (or sieve-manage-password
			 (read-passwd
			  (concat "Managesieve password for " user "@"
				  sieve-manage-server ": "))))
	(when (and user passwd)
	  (if (funcall loginfunc user passwd)
	      (progn
		(setq ret t
		      sieve-manage-username user)
		(if (and (not sieve-manage-password)
			 (y-or-n-p "Store password for this session? "))
		    (setq sieve-manage-password passwd)))
	    (if reason
		(message "Login failed (reason given: %s)..." reason)
	      (message "Login failed..."))
	    (setq reason nil)
	    (setq passwd nil)
	    (sit-for 1))))
      ;;	(quit (with-current-buffer buffer
      ;;		(setq user nil
      ;;		      passwd nil)))
      ;;	(error (with-current-buffer buffer
      ;;		 (setq user nil
      ;;		       passwd nil))))
      ret)))

(defun sieve-manage-erase (&optional p buffer)
  (let ((buffer (or buffer (current-buffer))))
    (and sieve-manage-log
	 (with-current-buffer (get-buffer-create sieve-manage-log)
	   (sieve-manage-disable-multibyte)
	   (buffer-disable-undo)
	   (goto-char (point-max))
	   (insert-buffer-substring buffer (with-current-buffer buffer
					     (point-min))
				    (or p (with-current-buffer buffer
					    (point-max)))))))
  (delete-region (point-min) (or p (point-max))))

(defun sieve-manage-open-1 (buffer)
  (with-current-buffer buffer
    (sieve-manage-erase)
    (setq sieve-manage-state 'initial
	  sieve-manage-process
	  (condition-case ()
	      (funcall (nth 2 (assq sieve-manage-stream
				    sieve-manage-stream-alist))
		       "sieve" buffer sieve-manage-server sieve-manage-port)
	    ((error quit) nil)))
    (when sieve-manage-process
      (while (and (eq sieve-manage-state 'initial)
		  (memq (process-status sieve-manage-process) '(open run)))
	(message "Waiting for response from %s..." sieve-manage-server)
	(accept-process-output sieve-manage-process 1))
      (message "Waiting for response from %s...done" sieve-manage-server)
      (and (memq (process-status sieve-manage-process) '(open run))
	   sieve-manage-process))))

;; Streams

(defun sieve-manage-network-p (buffer)
  t)

(defun sieve-manage-network-open (name buffer server port)
  (let* ((port (or port sieve-manage-default-port))
	 (coding-system-for-read sieve-manage-coding-system-for-read)
	 (coding-system-for-write sieve-manage-coding-system-for-write)
	 (process (open-network-stream name buffer server port)))
    (when process
      (while (and (memq (process-status process) '(open run))
		  (set-buffer buffer) ;; XXX "blue moon" nntp.el bug
		  (goto-char (point-min))
		  (not (sieve-manage-parse-greeting-1)))
	(accept-process-output process 1)
	(sit-for 1))
      (sieve-manage-erase nil buffer)
      (when (memq (process-status process) '(open run))
	process))))

(defun imap-starttls-p (buffer)
  ;;  (and (imap-capability 'STARTTLS buffer)
  (condition-case ()
      (progn
	(require 'starttls)
	(call-process "starttls"))
    (error nil)))

(defun imap-starttls-open (name buffer server port)
  (let* ((port (or port sieve-manage-default-port))
	 (coding-system-for-read sieve-manage-coding-system-for-read)
	 (coding-system-for-write sieve-manage-coding-system-for-write)
	 (process (starttls-open-stream name buffer server port))
	 done)
    (when process
      (while (and (memq (process-status process) '(open run))
		  (set-buffer buffer) ;; XXX "blue moon" nntp.el bug
		  (goto-char (point-min))
		  (not (sieve-manage-parse-greeting-1)))
	(accept-process-output process 1)
	(sit-for 1))
      (sieve-manage-erase nil buffer)
      (sieve-manage-send "STARTTLS")
      (starttls-negotiate process))
    (when (memq (process-status process) '(open run))
      process)))

;; Authenticators

(defun sieve-manage-plain-p (buffer)
  (sieve-manage-capability "SASL" "PLAIN" buffer))

(defun sieve-manage-plain-auth (buffer)
  "Login to managesieve server using the PLAIN SASL method."
  (let* ((done (sieve-manage-interactive-login
		buffer
		(lambda (user passwd)
		  (sieve-manage-send (concat "AUTHENTICATE \"PLAIN\" \""
					     (base64-encode-string
					      (concat (char-to-string 0)
						      user
						      (char-to-string 0)
						      passwd))
					     "\""))
		  (let ((rsp (sieve-manage-parse-okno)))
		    (if (sieve-manage-ok-p rsp)
			t
		      (setq reason (cdr-safe rsp))
		      nil))))))
    (if done
	(message "sieve: Authenticating using PLAIN...done")
      (message "sieve: Authenticating using PLAIN...failed"))))

(defun sieve-manage-cram-md5-p (buffer)
  (sieve-manage-capability "SASL" "CRAM-MD5" buffer))

(defun sieve-manage-cram-md5-auth (buffer)
  "Login to managesieve server using the CRAM-MD5 SASL method."
  (message "sieve: Authenticating using CRAM-MD5...")
  (let* ((done (sieve-manage-interactive-login
		buffer
		(lambda (user passwd)
		  (sieve-manage-send "AUTHENTICATE \"CRAM-MD5\"")
		  (sieve-manage-send
		   (concat
		    "\""
		    (base64-encode-string
		     (concat
		      user " "
		      (rfc2104-hash 'md5 64 16 passwd
				    (base64-decode-string
				     (prog1
					 (sieve-manage-parse-string)
				       (sieve-manage-erase))))))
		    "\""))
		  (let ((rsp (sieve-manage-parse-okno)))
		    (if (sieve-manage-ok-p rsp)
			t
		      (setq reason (cdr-safe rsp))
		      nil))))))
    (if done
	(message "sieve: Authenticating using CRAM-MD5...done")
      (message "sieve: Authenticating using CRAM-MD5...failed"))))

;; Managesieve API

(defun sieve-manage-open (server &optional port stream auth buffer)
  "Open a network connection to a managesieve SERVER (string).
Optional variable PORT is port number (integer) on remote server.
Optional variable STREAM is any of `sieve-manage-streams' (a symbol).
Optional variable AUTH indicates authenticator to use, see
`sieve-manage-authenticators' for available authenticators.  If nil, chooses
the best stream the server is capable of.
Optional variable BUFFER is buffer (buffer, or string naming buffer)
to work in."
  (setq buffer (or buffer (format " *sieve* %s:%d" server (or port 2000))))
  (with-current-buffer (get-buffer-create buffer)
    (mapcar 'make-variable-buffer-local sieve-manage-local-variables)
    (sieve-manage-disable-multibyte)
    (buffer-disable-undo)
    (setq sieve-manage-server (or server sieve-manage-server))
    (setq sieve-manage-port (or port sieve-manage-port))
    (setq sieve-manage-stream (or stream sieve-manage-stream))
    (message "sieve: Connecting to %s..." sieve-manage-server)
    (if (let ((sieve-manage-stream
	       (or sieve-manage-stream sieve-manage-default-stream)))
	  (sieve-manage-open-1 buffer))
	;; Choose stream.
	(let (stream-changed)
	  (message "sieve: Connecting to %s...done" sieve-manage-server)
	  (when (null sieve-manage-stream)
	    (let ((streams sieve-manage-streams))
	      (while (setq stream (pop streams))
		(if (funcall (nth 1 (assq stream
					  sieve-manage-stream-alist)) buffer)
		    (setq stream-changed
			  (not (eq (or sieve-manage-stream
				       sieve-manage-default-stream)
				   stream))
			  sieve-manage-stream stream
			  streams nil)))
	      (unless sieve-manage-stream
		(error "Couldn't figure out a stream for server"))))
	  (when stream-changed
	    (message "sieve: Reconnecting with stream `%s'..."
		     sieve-manage-stream)
	    (sieve-manage-close buffer)
	    (if (sieve-manage-open-1 buffer)
		(message "sieve: Reconnecting with stream `%s'...done"
			 sieve-manage-stream)
	      (message "sieve: Reconnecting with stream `%s'...failed"
		       sieve-manage-stream))
	    (setq sieve-manage-capability nil))
	  (if (sieve-manage-opened buffer)
	      ;; Choose authenticator
	      (when (and (null sieve-manage-auth)
			 (not (eq sieve-manage-state 'auth)))
		(let ((auths sieve-manage-authenticators))
		  (while (setq auth (pop auths))
		    (if (funcall (nth 1 (assq
					 auth
					 sieve-manage-authenticator-alist))
				 buffer)
			(setq sieve-manage-auth auth
			      auths nil)))
		  (unless sieve-manage-auth
		    (error "Couldn't figure out authenticator for server"))))))
      (message "sieve: Connecting to %s...failed" sieve-manage-server))
    (when (sieve-manage-opened buffer)
      (sieve-manage-erase)
      buffer)))

(defun sieve-manage-opened (&optional buffer)
  "Return non-nil if connection to managesieve server in BUFFER is open.
If BUFFER is nil then the current buffer is used."
  (and (setq buffer (get-buffer (or buffer (current-buffer))))
       (buffer-live-p buffer)
       (with-current-buffer buffer
	 (and sieve-manage-process
	      (memq (process-status sieve-manage-process) '(open run))))))

(defun sieve-manage-close (&optional buffer)
  "Close connection to managesieve server in BUFFER.
If BUFFER is nil, the current buffer is used."
  (with-current-buffer (or buffer (current-buffer))
    (when (sieve-manage-opened)
      (sieve-manage-send "LOGOUT")
      (sit-for 1))
    (when (and sieve-manage-process
	       (memq (process-status sieve-manage-process) '(open run)))
      (delete-process sieve-manage-process))
    (setq sieve-manage-process nil)
    (sieve-manage-erase)
    t))

(defun sieve-manage-authenticate (&optional user passwd buffer)
  "Authenticate to server in BUFFER, using current buffer if nil.
It uses the authenticator specified when opening the server.  If the
authenticator requires username/passwords, they are queried from the
user and optionally stored in the buffer.  If USER and/or PASSWD is
specified, the user will not be questioned and the username and/or
password is remembered in the buffer."
  (with-current-buffer (or buffer (current-buffer))
    (if (not (eq sieve-manage-state 'nonauth))
	(eq sieve-manage-state 'auth)
      (make-variable-buffer-local 'sieve-manage-username)
      (make-variable-buffer-local 'sieve-manage-password)
      (if user (setq sieve-manage-username user))
      (if passwd (setq sieve-manage-password passwd))
      (if (funcall (nth 2 (assq sieve-manage-auth
				sieve-manage-authenticator-alist)) buffer)
	  (setq sieve-manage-state 'auth)))))

(defun sieve-manage-capability (&optional name value buffer)
  (with-current-buffer (or buffer (current-buffer))
    (if (null name)
	sieve-manage-capability
      (if (null value)
	  (nth 1 (assoc name sieve-manage-capability))
	(when (string-match value (nth 1 (assoc name sieve-manage-capability)))
	  (nth 1 (assoc name sieve-manage-capability)))))))

(defun sieve-manage-listscripts (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (sieve-manage-send "LISTSCRIPTS")
    (sieve-manage-parse-listscripts)))

(defun sieve-manage-havespace (name size &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (sieve-manage-send (format "HAVESPACE \"%s\" %s" name size))
    (sieve-manage-parse-okno)))

(eval-and-compile
  (if (fboundp 'string-bytes)
      (defalias 'sieve-string-bytes 'string-bytes)
    (defalias 'sieve-string-bytes 'length)))

(defun sieve-manage-putscript (name content &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (sieve-manage-send (format "PUTSCRIPT \"%s\" {%d+}%s%s" name
			       (sieve-string-bytes content)
			       sieve-manage-client-eol content))
    (sieve-manage-parse-okno)))

(defun sieve-manage-deletescript (name &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (sieve-manage-send (format "DELETESCRIPT \"%s\"" name))
    (sieve-manage-parse-okno)))

(defun sieve-manage-getscript (name output-buffer &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (sieve-manage-send (format "GETSCRIPT \"%s\"" name))
    (let ((script (sieve-manage-parse-string)))
      (sieve-manage-parse-crlf)
      (with-current-buffer output-buffer
	(insert script))
      (sieve-manage-parse-okno))))

(defun sieve-manage-setactive (name &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (sieve-manage-send (format "SETACTIVE \"%s\"" name))
    (sieve-manage-parse-okno)))

;; Protocol parsing routines

(defun sieve-manage-ok-p (rsp)
  (string= (downcase (or (car-safe rsp) "")) "ok"))

(defsubst sieve-manage-forward ()
  (or (eobp) (forward-char)))

(defun sieve-manage-is-okno ()
  (when (looking-at (concat
		     "^\\(OK\\|NO\\)\\( (\\([^)]+\\))\\)?\\( \\(.*\\)\\)?"
		     sieve-manage-server-eol))
    (let ((status (match-string 1))
	  (resp-code (match-string 3))
	  (response (match-string 5)))
      (when response
	(goto-char (match-beginning 5))
	(setq response (sieve-manage-is-string)))
      (list status resp-code response))))

(defun sieve-manage-parse-okno ()
  (let (rsp)
    (while (null rsp)
      (accept-process-output (get-buffer-process (current-buffer)) 1)
      (goto-char (point-min))
      (setq rsp (sieve-manage-is-okno)))
    (sieve-manage-erase)
    rsp))

(defun sieve-manage-parse-capability-1 ()
  "Accept a managesieve greeting."
  (let (str)
    (while (setq str (sieve-manage-is-string))
      (if (eq (char-after) ? )
	  (progn
	    (sieve-manage-forward)
	    (push (list str (sieve-manage-is-string))
		  sieve-manage-capability))
	(push (list str) sieve-manage-capability))
      (forward-line)))
  (when (re-search-forward (concat "^OK" sieve-manage-server-eol) nil t)
    (setq sieve-manage-state 'nonauth)))

(defalias 'sieve-manage-parse-greeting-1 'sieve-manage-parse-capability-1)

(defun sieve-manage-is-string ()
  (cond ((looking-at "\"\\([^\"]+\\)\"")
	 (prog1
	     (match-string 1)
	   (goto-char (match-end 0))))
	((looking-at (concat "{\\([0-9]+\\)}" sieve-manage-server-eol))
	 (let ((pos (match-end 0))
	       (len (string-to-number (match-string 1))))
	   (if (< (point-max) (+ pos len))
	       nil
	     (goto-char (+ pos len))
	     (buffer-substring pos (+ pos len)))))))

(defun sieve-manage-parse-string ()
  (let (rsp)
    (while (null rsp)
      (accept-process-output (get-buffer-process (current-buffer)) 1)
      (goto-char (point-min))
      (setq rsp (sieve-manage-is-string)))
    (sieve-manage-erase (point))
    rsp))

(defun sieve-manage-parse-crlf ()
  (when (looking-at sieve-manage-server-eol)
    (sieve-manage-erase (match-end 0))))

(defun sieve-manage-parse-listscripts ()
  (let (tmp rsp data)
    (while (null rsp)
      (while (null (or (setq rsp (sieve-manage-is-okno))
		       (setq tmp (sieve-manage-is-string))))
	(accept-process-output (get-buffer-process (current-buffer)) 1)
	(goto-char (point-min)))
      (when tmp
	(while (not (looking-at (concat "\\( ACTIVE\\)?"
					sieve-manage-server-eol)))
	  (accept-process-output (get-buffer-process (current-buffer)) 1)
	  (goto-char (point-min)))
	(if (match-string 1)
	    (push (cons 'active tmp) data)
	  (push tmp data))
	(goto-char (match-end 0))
	(setq tmp nil)))
    (sieve-manage-erase)
    (if (sieve-manage-ok-p rsp)
	data
      rsp)))

(defun sieve-manage-send (cmdstr)
  (setq cmdstr (concat cmdstr sieve-manage-client-eol))
  (and sieve-manage-log
       (with-current-buffer (get-buffer-create sieve-manage-log)
	 (sieve-manage-disable-multibyte)
	 (buffer-disable-undo)
	 (goto-char (point-max))
	 (insert cmdstr)))
  (process-send-string sieve-manage-process cmdstr))

(provide 'sieve-manage)

;;; arch-tag: 321c4640-1371-4495-9baf-8ccb71dd5bd1
;; sieve-manage.el ends here
