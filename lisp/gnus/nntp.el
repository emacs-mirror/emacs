;;; nntp.el --- nntp access for Gnus
;;; Copyright (C) 1987,88,89,90,92,93,94,95,96,97 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; Keywords: news

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Code:

(require 'nnheader)
(require 'nnoo)
(require 'gnus-util)

(nnoo-declare nntp)

(eval-and-compile
  (unless (fboundp 'open-network-stream)
    (require 'tcp)))

(eval-when-compile (require 'cl))

(defvoo nntp-address nil
  "Address of the physical nntp server.")

(defvoo nntp-port-number "nntp"
  "Port number on the physical nntp server.")

(defvoo nntp-server-opened-hook '(nntp-send-mode-reader)
  "*Hook used for sending commands to the server at startup.
The default value is `nntp-send-mode-reader', which makes an innd
server spawn an nnrpd server.  Another useful function to put in this
hook might be `nntp-send-authinfo', which will prompt for a password
to allow posting from the server.  Note that this is only necessary to
do on servers that use strict access control.")

(defvoo nntp-authinfo-function 'nntp-send-authinfo
  "Function used to send AUTHINFO to the server.")

(defvoo nntp-server-action-alist
  '(("nntpd 1\\.5\\.11t"
     (remove-hook 'nntp-server-opened-hook 'nntp-send-mode-reader))
    ("NNRP server Netscape"
     (setq nntp-server-list-active-group nil)))
  "Alist of regexps to match on server types and actions to be taken.
For instance, if you want Gnus to beep every time you connect
to innd, you could say something like:

\(setq nntp-server-action-alist
       '((\"innd\" (ding))))

You probably don't want to do that, though.")

(defvoo nntp-open-connection-function 'nntp-open-network-stream
  "*Function used for connecting to a remote system.
It will be called with the buffer to output in.

Two pre-made functions are `nntp-open-network-stream', which is the
default, and simply connects to some port or other on the remote
system (see nntp-port-number).  The other are `nntp-open-rlogin',
which does an rlogin on the remote system, and then does a telnet to
the NNTP server available there (see nntp-rlogin-parameters) and
`nntp-open-telnet' which telnets to a remote system, logs in and does
the same.")

(defvoo nntp-rlogin-parameters '("telnet" "-8" "${NNTPSERVER:=news}" "nntp")
  "*Parameters to `nntp-open-login'.
That function may be used as `nntp-open-connection-function'.  In that
case, this list will be used as the parameter list given to rsh.")

(defvoo nntp-rlogin-user-name nil
  "*User name on remote system when using the rlogin connect method.")

(defvoo nntp-telnet-parameters '("exec" "telnet" "-8" "${NNTPSERVER:=news}" "nntp")
  "*Parameters to `nntp-open-telnet'.
That function may be used as `nntp-open-connection-function'.  In that
case, this list will be executed as a command after logging in
via telnet.")

(defvoo nntp-telnet-user-name nil
  "User name to log in via telnet with.")

(defvoo nntp-telnet-passwd nil
  "Password to use to log in via telnet with.")

(defvoo nntp-telnet-command "telnet"
  "Command used to start telnet.")

(defvoo nntp-telnet-switches '("-8")
  "Switches given to the telnet command.")

(defvoo nntp-end-of-line "\r\n"
  "String to use on the end of lines when talking to the NNTP server.
This is \"\\r\\n\" by default, but should be \"\\n\" when
using rlogin or telnet to communicate with the server.")

(defvoo nntp-large-newsgroup 50
  "*The number of the articles which indicates a large newsgroup.
If the number of the articles is greater than the value, verbose
messages will be shown to indicate the current status.")

(defvoo nntp-maximum-request 400
  "*The maximum number of the requests sent to the NNTP server at one time.
If Emacs hangs up while retrieving headers, set the variable to a
lower value.")

(defvoo nntp-nov-is-evil nil
  "*If non-nil, nntp will never attempt to use XOVER when talking to the server.")

(defvoo nntp-xover-commands '("XOVER" "XOVERVIEW")
  "*List of strings that are used as commands to fetch NOV lines from a server.
The strings are tried in turn until a positive response is gotten.  If
none of the commands are successful, nntp will just grab headers one
by one.")

(defvoo nntp-nov-gap 5
  "*Maximum allowed gap between two articles.
If the gap between two consecutive articles is bigger than this
variable, split the XOVER request into two requests.")

(defvoo nntp-connection-timeout nil
  "*Number of seconds to wait before an nntp connection times out.
If this variable is nil, which is the default, no timers are set.")

(defvoo nntp-prepare-server-hook nil
  "*Hook run before a server is opened.
If can be used to set up a server remotely, for instance.  Say you
have an account at the machine \"other.machine\".  This machine has
access to an NNTP server that you can't access locally.  You could
then use this hook to rsh to the remote machine and start a proxy NNTP
server there that you can connect to.  See also `nntp-open-connection-function'")

(defvoo nntp-warn-about-losing-connection t
  "*If non-nil, beep when a server closes connection.")

;; 1997/5/4 by MORIOKA Tomohiko <morioka@jaist.ac.jp>
(defvoo nntp-coding-system-for-read 'binary
  "*Coding system to read from NNTP.")

(defvoo nntp-coding-system-for-write 'binary
  "*Coding system to write to NNTP.")



;;; Internal variables.

(defvar nntp-have-messaged nil)

(defvar nntp-process-wait-for nil)
(defvar nntp-process-to-buffer nil)
(defvar nntp-process-callback nil)
(defvar nntp-process-decode nil)
(defvar nntp-process-start-point nil)
(defvar nntp-inside-change-function nil)

(defvar nntp-connection-list nil)

(defvoo nntp-server-type nil)
(defvoo nntp-connection-alist nil)
(defvoo nntp-status-string "")
(defconst nntp-version "nntp 5.0")
(defvoo nntp-inhibit-erase nil)
(defvoo nntp-inhibit-output nil)

(defvoo nntp-server-xover 'try)
(defvoo nntp-server-list-active-group 'try)

(eval-and-compile
  (autoload 'nnmail-read-passwd "nnmail"))



;;; Internal functions.

(defsubst nntp-send-string (process string)
  "Send STRING to PROCESS."
  (process-send-string process (concat string nntp-end-of-line)))

(defsubst nntp-wait-for (process wait-for buffer &optional decode discard)
  "Wait for WAIT-FOR to arrive from PROCESS."
  (save-excursion
    (set-buffer (process-buffer process))
    (goto-char (point-min))
    (while (or (not (memq (char-after (point)) '(?2 ?3 ?4 ?5)))
	       (looking-at "480"))
      (when (looking-at "480")
	(erase-buffer)
	(funcall nntp-authinfo-function))
      (nntp-accept-process-output process)
      (goto-char (point-min)))
    (prog1
	(if (looking-at "[45]")
	    (progn
	      (nntp-snarf-error-message)
	      nil)
	  (goto-char (point-max))
	  (let ((limit (point-min)))
	    (while (not (re-search-backward wait-for limit t))
	      ;; We assume that whatever we wait for is less than 1000
	      ;; characters long.
	      (setq limit (max (- (point-max) 1000) (point-min)))
	      (nntp-accept-process-output process)
	      (goto-char (point-max))))
	  (nntp-decode-text (not decode))
	  (unless discard
	    (save-excursion
	      (set-buffer buffer)
	      (goto-char (point-max))
	      (insert-buffer-substring (process-buffer process))
	      ;; Nix out "nntp reading...." message.
	      (when nntp-have-messaged
		(setq nntp-have-messaged nil)
		(message ""))
	      t)))
      (unless discard
	(erase-buffer)))))

(defsubst nntp-find-connection (buffer)
  "Find the connection delivering to BUFFER."
  (let ((alist nntp-connection-alist)
	(buffer (if (stringp buffer) (get-buffer buffer) buffer))
	process entry)
    (while (setq entry (pop alist))
      (when (eq buffer (cadr entry))
	(setq process (car entry)
	      alist nil)))
    (when process
      (if (memq (process-status process) '(open run))
	  process
	(when (buffer-name (process-buffer process))
	  (kill-buffer (process-buffer process)))
	(setq nntp-connection-alist (delq entry nntp-connection-alist))
	nil))))

(defsubst nntp-find-connection-entry (buffer)
  "Return the entry for the connection to BUFFER."
  (assq (nntp-find-connection buffer) nntp-connection-alist))

(defun nntp-find-connection-buffer (buffer)
  "Return the process connection buffer tied to BUFFER."
  (let ((process (nntp-find-connection buffer)))
    (when process
      (process-buffer process))))

(defsubst nntp-retrieve-data (command address port buffer
				   &optional wait-for callback decode)
  "Use COMMAND to retrieve data into BUFFER from PORT on ADDRESS."
  (let ((process (or (nntp-find-connection buffer)
		     (nntp-open-connection buffer))))
    (if (not process)
	(nnheader-report 'nntp "Couldn't open connection to %s" address)
      (unless (or nntp-inhibit-erase nnheader-callback-function)
	(save-excursion
	  (set-buffer (process-buffer process))
	  (erase-buffer)))
      (when command
	(nntp-send-string process command))
      (cond
       ((eq callback 'ignore)
	t)
       ((and callback wait-for)
	(save-excursion
	  (set-buffer (process-buffer process))
	  (unless nntp-inside-change-function
	    (erase-buffer))
	  (setq nntp-process-decode decode
		nntp-process-to-buffer buffer
		nntp-process-wait-for wait-for
		nntp-process-callback callback
		nntp-process-start-point (point-max)
		after-change-functions
		(list 'nntp-after-change-function-callback)))
	t)
       (wait-for
	(nntp-wait-for process wait-for buffer decode))
       (t t)))))

(defsubst nntp-send-command (wait-for &rest strings)
  "Send STRINGS to server and wait until WAIT-FOR returns."
  (when (and (not nnheader-callback-function)
	     (not nntp-inhibit-output))
    (save-excursion
      (set-buffer nntp-server-buffer)
      (erase-buffer)))
  (nntp-retrieve-data
   (mapconcat 'identity strings " ")
   nntp-address nntp-port-number nntp-server-buffer
   wait-for nnheader-callback-function))

(defun nntp-send-command-nodelete (wait-for &rest strings)
  "Send STRINGS to server and wait until WAIT-FOR returns."
  (nntp-retrieve-data
   (mapconcat 'identity strings " ")
   nntp-address nntp-port-number nntp-server-buffer
   wait-for nnheader-callback-function))

(defun nntp-send-command-and-decode (wait-for &rest strings)
  "Send STRINGS to server and wait until WAIT-FOR returns."
  (when (and (not nnheader-callback-function)
	     (not nntp-inhibit-output))
    (save-excursion
      (set-buffer nntp-server-buffer)
      (erase-buffer)))
  (nntp-retrieve-data
   (mapconcat 'identity strings " ")
   nntp-address nntp-port-number nntp-server-buffer
   wait-for nnheader-callback-function t))

(defun nntp-send-buffer (wait-for)
  "Send the current buffer to server and wait until WAIT-FOR returns."
  (when (and (not nnheader-callback-function)
	     (not nntp-inhibit-output))
    (save-excursion
      (set-buffer (nntp-find-connection-buffer nntp-server-buffer))
      (erase-buffer)))
  (nntp-encode-text)
  (process-send-region (nntp-find-connection nntp-server-buffer)
		       (point-min) (point-max))
  (nntp-retrieve-data
   nil nntp-address nntp-port-number nntp-server-buffer
   wait-for nnheader-callback-function))



;;; Interface functions.

(nnoo-define-basics nntp)

(deffoo nntp-retrieve-headers (articles &optional group server fetch-old)
  "Retrieve the headers of ARTICLES."
  (nntp-possibly-change-group group server)
  (save-excursion
    (set-buffer (nntp-find-connection-buffer nntp-server-buffer))
    (erase-buffer)
    (if (and (not gnus-nov-is-evil)
	     (not nntp-nov-is-evil)
	     (nntp-retrieve-headers-with-xover articles fetch-old))
	;; We successfully retrieved the headers via XOVER.
        'nov
      ;; XOVER didn't work, so we do it the hard, slow and inefficient
      ;; way.
      (let ((number (length articles))
	    (count 0)
	    (received 0)
	    (last-point (point-min))
	    (buf (nntp-find-connection-buffer nntp-server-buffer))
	    (nntp-inhibit-erase t))
	;; Send HEAD command.
	(while articles
	  (nntp-send-command
	   nil
	   "HEAD" (if (numberp (car articles))
		      (int-to-string (car articles))
		    ;; `articles' is either a list of article numbers
		    ;; or a list of article IDs.
		    (car articles)))
	  (setq articles (cdr articles)
		count (1+ count))
	  ;; Every 400 header requests we have to read the stream in
	  ;; order to avoid deadlocks.
	  (when (or (null articles)	;All requests have been sent.
		    (zerop (% count nntp-maximum-request)))
	    (nntp-accept-response)
	    (while (progn
		     (progn
		       (set-buffer buf)
		       (goto-char last-point))
		     ;; Count replies.
		     (while (re-search-forward "^[0-9]" nil t)
		       (incf received))
		     (setq last-point (point))
		     (< received count))
	      ;; If number of headers is greater than 100, give
	      ;;  informative messages.
	      (and (numberp nntp-large-newsgroup)
		   (> number nntp-large-newsgroup)
		   (zerop (% received 20))
		   (nnheader-message 6 "NNTP: Receiving headers... %d%%"
				     (/ (* received 100) number)))
	      (nntp-accept-response))))
	;; Wait for text of last command.
	(goto-char (point-max))
	(re-search-backward "^[0-9]" nil t)
	(when (looking-at "^[23]")
	  (while (progn
		   (goto-char (point-max))
		   (forward-line -1)
		   (not (looking-at "^\\.\r?\n")))
	    (nntp-accept-response)))
	(and (numberp nntp-large-newsgroup)
	     (> number nntp-large-newsgroup)
	     (nnheader-message 6 "NNTP: Receiving headers...done"))

	;; Now all of replies are received.  Fold continuation lines.
	(nnheader-fold-continuation-lines)
	;; Remove all "\r"'s.
	(nnheader-strip-cr)
	(copy-to-buffer nntp-server-buffer (point-min) (point-max))
	'headers))))

(deffoo nntp-retrieve-groups (groups &optional server)
  "Retrieve group info on GROUPS."
  (nntp-possibly-change-group nil server)
  (save-excursion
    (set-buffer (nntp-find-connection-buffer nntp-server-buffer))
    ;; The first time this is run, this variable is `try'.  So we
    ;; try.
    (when (eq nntp-server-list-active-group 'try)
      (nntp-try-list-active (car groups)))
    (erase-buffer)
    (let ((count 0)
	  (received 0)
	  (last-point (point-min))
	  (nntp-inhibit-erase t)
	  (command (if nntp-server-list-active-group "LIST ACTIVE" "GROUP")))
      (while groups
	;; Send the command to the server.
	(nntp-send-command nil command (pop groups))
	(incf count)
	;; Every 400 requests we have to read the stream in
	;; order to avoid deadlocks.
	(when (or (null groups)		;All requests have been sent.
		  (zerop (% count nntp-maximum-request)))
	  (nntp-accept-response)
	  (while (progn
		   (goto-char last-point)
		   ;; Count replies.
		   (while (re-search-forward "^[0-9]" nil t)
		     (incf received))
		   (setq last-point (point))
		   (< received count))
	    (nntp-accept-response))))

      ;; Wait for the reply from the final command.
      (goto-char (point-max))
      (re-search-backward "^[0-9]" nil t)
      (when (looking-at "^[23]")
	(while (progn
		 (goto-char (point-max))
		 (if (not nntp-server-list-active-group)
		     (not (re-search-backward "\r?\n" (- (point) 3) t))
		   (not (re-search-backward "^\\.\r?\n" (- (point) 4) t))))
	  (nntp-accept-response)))

      ;; Now all replies are received.  We remove CRs.
      (goto-char (point-min))
      (while (search-forward "\r" nil t)
	(replace-match "" t t))

      (if (not nntp-server-list-active-group)
	  (progn
	    (copy-to-buffer nntp-server-buffer (point-min) (point-max))
	    'group)
	;; We have read active entries, so we just delete the
	;; superfluous gunk.
	(goto-char (point-min))
	(while (re-search-forward "^[.2-5]" nil t)
	  (delete-region (match-beginning 0)
			 (progn (forward-line 1) (point))))
	(copy-to-buffer nntp-server-buffer (point-min) (point-max))
	'active))))

(deffoo nntp-retrieve-articles (articles &optional group server)
  (nntp-possibly-change-group group server)
  (save-excursion
    (let ((number (length articles))
	  (count 0)
	  (received 0)
	  (last-point (point-min))
	  (buf (nntp-find-connection-buffer nntp-server-buffer))
	  (nntp-inhibit-erase t)
	  (map (apply 'vector articles))
	  (point 1)
	  article alist)
      (set-buffer buf)
      (erase-buffer)
      ;; Send HEAD command.
      (while (setq article (pop articles))
	(nntp-send-command
	 nil
	 "ARTICLE" (if (numberp article)
		       (int-to-string article)
		     ;; `articles' is either a list of article numbers
		     ;; or a list of article IDs.
		     article))
	(incf count)
	;; Every 400 requests we have to read the stream in
	;; order to avoid deadlocks.
	(when (or (null articles)	;All requests have been sent.
		  (zerop (% count nntp-maximum-request)))
	  (nntp-accept-response)
	  (while (progn
		   (progn
		     (set-buffer buf)
		     (goto-char last-point))
		   ;; Count replies.
		   (while (nntp-next-result-arrived-p)
		     (aset map received (cons (aref map received) (point)))
		     (incf received))
		   (setq last-point (point))
		   (< received count))
	    ;; If number of headers is greater than 100, give
	    ;;  informative messages.
	    (and (numberp nntp-large-newsgroup)
		 (> number nntp-large-newsgroup)
		 (zerop (% received 20))
		 (nnheader-message 6 "NNTP: Receiving articles... %d%%"
				   (/ (* received 100) number)))
	    (nntp-accept-response))))
      (and (numberp nntp-large-newsgroup)
	   (> number nntp-large-newsgroup)
	   (nnheader-message 6 "NNTP: Receiving headers...done"))

      ;; Now we have all the responses.  We go through the results,
      ;; washes it and copies it over to the server buffer.
      (set-buffer nntp-server-buffer)
      (erase-buffer)
      (mapcar
       (lambda (entry)
	 (narrow-to-region
	  (setq point (goto-char (point-max)))
	  (progn
	    (insert-buffer-substring buf last-point (cdr entry))
	    (point-max)))
	 (nntp-decode-text)
	 (widen)
	 (cons (car entry) point))
       map))))

(defun nntp-next-result-arrived-p ()
  (let ((point (point)))
    (cond
     ((looking-at "2")
      (if (re-search-forward "\n.\r?\n" nil t)
	  t
	(goto-char point)
	nil))
     ((looking-at "[34]")
      (forward-line 1)
      t)
     (t
      nil))))

(defun nntp-try-list-active (group)
  (nntp-list-active-group group)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (goto-char (point-min))
    (cond ((or (eobp)
	       (looking-at "5[0-9]+"))
	   (setq nntp-server-list-active-group nil))
	  (t
	   (setq nntp-server-list-active-group t)))))

(deffoo nntp-list-active-group (group &optional server)
  "Return the active info on GROUP (which can be a regexp."
  (nntp-possibly-change-group nil server)
  (nntp-send-command "^.*\r?\n" "LIST ACTIVE" group))

(deffoo nntp-request-article (article &optional group server buffer command)
  (nntp-possibly-change-group group server)
  (when (nntp-send-command-and-decode
	 "\r?\n\\.\r?\n" "ARTICLE"
	 (if (numberp article) (int-to-string article) article))
    (if (and buffer
	     (not (equal buffer nntp-server-buffer)))
	(save-excursion
	  (set-buffer nntp-server-buffer)
	  (copy-to-buffer buffer (point-min) (point-max))
	  (nntp-find-group-and-number))
      (nntp-find-group-and-number))))

(deffoo nntp-request-head (article &optional group server)
  (nntp-possibly-change-group group server)
  (when (nntp-send-command
	 "\r?\n\\.\r?\n" "HEAD"
	 (if (numberp article) (int-to-string article) article))
    (prog1
	(nntp-find-group-and-number)
      (nntp-decode-text))))

(deffoo nntp-request-body (article &optional group server)
  (nntp-possibly-change-group group server)
  (nntp-send-command-and-decode
   "\r?\n\\.\r?\n" "BODY"
   (if (numberp article) (int-to-string article) article)))

(deffoo nntp-request-group (group &optional server dont-check)
  (nntp-possibly-change-group nil server)
  (when (nntp-send-command "^2.*\n" "GROUP" group)
    (let ((entry (nntp-find-connection-entry nntp-server-buffer)))
      (setcar (cddr entry) group))))

(deffoo nntp-close-group (group &optional server)
  t)

(deffoo nntp-server-opened (&optional server)
  "Say whether a connection to SERVER has been opened."
  (and (nnoo-current-server-p 'nntp server)
       nntp-server-buffer
       (gnus-buffer-live-p nntp-server-buffer)
       (nntp-find-connection nntp-server-buffer)))

(deffoo nntp-open-server (server &optional defs connectionless)
  (nnheader-init-server-buffer)
  (if (nntp-server-opened server)
      t
    (when (or (stringp (car defs))
	      (numberp (car defs)))
      (setq defs (cons (list 'nntp-port-number (car defs)) (cdr defs))))
    (unless (assq 'nntp-address defs)
      (setq defs (append defs (list (list 'nntp-address server)))))
    (nnoo-change-server 'nntp server defs)
    (unless connectionless
      (or (nntp-find-connection nntp-server-buffer)
	  (nntp-open-connection nntp-server-buffer)))))

(deffoo nntp-close-server (&optional server)
  (nntp-possibly-change-group nil server t)
  (let (process)
    (while (setq process (car (pop nntp-connection-alist)))
      (when (memq (process-status process) '(open run))
	(set-process-sentinel process nil)
	(nntp-send-string process "QUIT"))
      (when (buffer-name (process-buffer process))
	(kill-buffer (process-buffer process))))
    (nnoo-close-server 'nntp)))

(deffoo nntp-request-close ()
  (let (process)
    (while (setq process (pop nntp-connection-list))
      (when (memq (process-status process) '(open run))
	(set-process-sentinel process nil)
	(ignore-errors
	  (nntp-send-string process "QUIT")))
      (when (buffer-name (process-buffer process))
	(kill-buffer (process-buffer process))))))

(deffoo nntp-request-list (&optional server)
  (nntp-possibly-change-group nil server)
  (nntp-send-command-and-decode "\r?\n\\.\r?\n" "LIST"))

(deffoo nntp-request-list-newsgroups (&optional server)
  (nntp-possibly-change-group nil server)
  (nntp-send-command "\r?\n\\.\r?\n" "LIST NEWSGROUPS"))

(deffoo nntp-request-newgroups (date &optional server)
  (nntp-possibly-change-group nil server)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (let* ((date (timezone-parse-date date))
	   (time-string
	    (format "%s%02d%02d %s%s%s"
		    (substring (aref date 0) 2) (string-to-int (aref date 1))
		    (string-to-int (aref date 2)) (substring (aref date 3) 0 2)
		    (substring
		     (aref date 3) 3 5) (substring (aref date 3) 6 8))))
      (prog1
	  (nntp-send-command "^\\.\r?\n" "NEWGROUPS" time-string)
	(nntp-decode-text)))))

(deffoo nntp-request-post (&optional server)
  (nntp-possibly-change-group nil server)
  (when (nntp-send-command "^[23].*\r?\n" "POST")
    (nntp-send-buffer "^[23].*\n")))

(deffoo nntp-request-type (group article)
  'news)

(deffoo nntp-asynchronous-p ()
  t)

;;; Hooky functions.

(defun nntp-send-mode-reader ()
  "Send the MODE READER command to the nntp server.
This function is supposed to be called from `nntp-server-opened-hook'.
It will make innd servers spawn an nnrpd process to allow actual article
reading."
  (nntp-send-command "^.*\r?\n" "MODE READER"))

(defun nntp-send-nosy-authinfo ()
  "Send the AUTHINFO to the nntp server.
This function is supposed to be called from `nntp-server-opened-hook'.
It will prompt for a password."
  (nntp-send-command
   "^.*\r?\n" "AUTHINFO USER"
   (read-string (format "NNTP (%s) user name: " nntp-address)))
  (nntp-send-command
   "^.*\r?\n" "AUTHINFO PASS"
   (nnmail-read-passwd "NNTP (%s) password: " nntp-address)))

(defun nntp-send-authinfo ()
  "Send the AUTHINFO to the nntp server.
This function is supposed to be called from `nntp-server-opened-hook'.
It will prompt for a password."
  (nntp-send-command "^.*\r?\n" "AUTHINFO USER" (user-login-name))
  (nntp-send-command
   "^.*\r?\n" "AUTHINFO PASS"
   (nnmail-read-passwd (format "NNTP (%s) password: " nntp-address))))

(defun nntp-send-authinfo-from-file ()
  "Send the AUTHINFO to the nntp server.
This function is supposed to be called from `nntp-server-opened-hook'."
  (when (file-exists-p "~/.nntp-authinfo")
    (nnheader-temp-write nil
      (insert-file-contents "~/.nntp-authinfo")
      (goto-char (point-min))
      (nntp-send-command "^.*\r?\n" "AUTHINFO USER" (user-login-name))
      (nntp-send-command
       "^.*\r?\n" "AUTHINFO PASS"
       (buffer-substring (point) (progn (end-of-line) (point)))))))

;;; Internal functions.

(defun nntp-make-process-buffer (buffer)
  "Create a new, fresh buffer usable for nntp process connections."
  (save-excursion
    (set-buffer
     (generate-new-buffer
      (format " *server %s %s %s*"
	      nntp-address nntp-port-number
	      (buffer-name (get-buffer buffer)))))
    (buffer-disable-undo (current-buffer))
    (set (make-local-variable 'after-change-functions) nil)
    (set (make-local-variable 'nntp-process-wait-for) nil)
    (set (make-local-variable 'nntp-process-callback) nil)
    (set (make-local-variable 'nntp-process-to-buffer) nil)
    (set (make-local-variable 'nntp-process-start-point) nil)
    (set (make-local-variable 'nntp-process-decode) nil)
    (current-buffer)))

(defun nntp-open-connection (buffer)
  "Open a connection to PORT on ADDRESS delivering output to BUFFER."
  (run-hooks 'nntp-prepare-server-hook)
  (let* ((pbuffer (nntp-make-process-buffer buffer))
	 (process
	  (condition-case ()
	      ;; 1997/5/4 by MORIOKA Tomohiko <morioka@jaist.ac.jp>
	      (let ((coding-system-for-read nntp-coding-system-for-read)
		    (coding-system-for-write nntp-coding-system-for-write))
		(funcall nntp-open-connection-function pbuffer))
	    (error nil)
	    (quit nil))))
    (when process
      (process-kill-without-query process)
      (nntp-wait-for process "^.*\n" buffer nil t)
      (if (memq (process-status process) '(open run))
	  (prog1
	      (caar (push (list process buffer nil) nntp-connection-alist))
	    (push process nntp-connection-list)
	    (save-excursion
	      (set-buffer pbuffer)
	      (nntp-read-server-type)
	      (erase-buffer)
	      (set-buffer nntp-server-buffer)
	      (let ((nnheader-callback-function nil))
		(run-hooks 'nntp-server-opened-hook))))
	(when (buffer-name (process-buffer process))
	  (kill-buffer (process-buffer process)))
	nil))))

(defun nntp-open-network-stream (buffer)
  (open-network-stream "nntpd" buffer nntp-address nntp-port-number))

(defun nntp-read-server-type ()
  "Find out what the name of the server we have connected to is."
  ;; Wait for the status string to arrive.
  (setq nntp-server-type (buffer-string))
  (let ((alist nntp-server-action-alist)
	(case-fold-search t)
	entry)
    ;; Run server-specific commands.
    (while alist
      (setq entry (pop alist))
      (when (string-match (car entry) nntp-server-type)
	(if (and (listp (cadr entry))
		 (not (eq 'lambda (caadr entry))))
	    (eval (cadr entry))
	  (funcall (cadr entry)))))))

(defun nntp-after-change-function-callback (beg end len)
  (when nntp-process-callback
    (save-match-data
      (if (and (= beg (point-min))
	       (memq (char-after beg) '(?4 ?5)))
	  ;; Report back error messages.
	  (save-excursion
	    (goto-char beg)
	    (if (looking-at "480")
		(funcall nntp-authinfo-function)
	      (nntp-snarf-error-message)
	      (funcall nntp-process-callback nil)))
	(goto-char end)
	(when (and (> (point) nntp-process-start-point)
		   (re-search-backward nntp-process-wait-for
				       nntp-process-start-point t))
	  (when (buffer-name (get-buffer nntp-process-to-buffer))
	    (let ((cur (current-buffer))
		  (start nntp-process-start-point))
	      (save-excursion
		(set-buffer (get-buffer nntp-process-to-buffer))
		(goto-char (point-max))
		(let ((b (point)))
		  (insert-buffer-substring cur start)
		  (narrow-to-region b (point-max))
		  (nntp-decode-text)
		  (widen)))))
	  (goto-char end)
	  (let ((callback nntp-process-callback)
		(nntp-inside-change-function t))
	    (setq nntp-process-callback nil)
	    (save-excursion
	      (funcall callback (buffer-name
				 (get-buffer nntp-process-to-buffer))))))))))

(defun nntp-snarf-error-message ()
  "Save the error message in the current buffer."
  (let ((message (buffer-string)))
    (while (string-match "[\r\n]+" message)
      (setq message (replace-match " " t t message)))
    (nnheader-report 'nntp message)
    message))

(defun nntp-accept-process-output (process)
  "Wait for output from PROCESS and message some dots."
  (save-excursion
    (set-buffer (or (nntp-find-connection-buffer nntp-server-buffer)
		    nntp-server-buffer))
    (let ((len (/ (point-max) 1024))
	  message-log-max)
      (unless (< len 10)
	(setq nntp-have-messaged t)
	(nnheader-message 7 "nntp read: %dk" len)))
    (accept-process-output process 1)))

(defun nntp-accept-response ()
  "Wait for output from the process that outputs to BUFFER."
  (nntp-accept-process-output (nntp-find-connection nntp-server-buffer)))

(defun nntp-possibly-change-group (group server &optional connectionless)
  (let ((nnheader-callback-function nil))
    (when server
      (or (nntp-server-opened server)
	  (nntp-open-server server nil connectionless)))

    (unless connectionless
      (or (nntp-find-connection nntp-server-buffer)
	  (nntp-open-connection nntp-server-buffer))))

  (when group
    (let ((entry (nntp-find-connection-entry nntp-server-buffer)))
      (when (not (equal group (caddr entry)))
	(save-excursion
	  (set-buffer (process-buffer (car entry)))
	  (erase-buffer)
	  (nntp-send-string (car entry) (concat "GROUP " group))
	  (nntp-wait-for-string "^2.*\n")
	  (setcar (cddr entry) group)
	  (erase-buffer))))))

(defun nntp-decode-text (&optional cr-only)
  "Decode the text in the current buffer."
  (goto-char (point-min))
  (while (search-forward "\r" nil t)
    (delete-char -1))
  (unless cr-only
    ;; Remove trailing ".\n" end-of-transfer marker.
    (goto-char (point-max))
    (forward-line -1)
    (when (looking-at ".\n")
      (delete-char 2))
    ;; Delete status line.
    (goto-char (point-min))
    (delete-region (point) (progn (forward-line 1) (point)))
    ;; Remove "." -> ".." encoding.
    (while (search-forward "\n.." nil t)
      (delete-char -1))))

(defun nntp-encode-text ()
  "Encode the text in the current buffer."
  (save-excursion
    ;; Replace "." at beginning of line with "..".
    (goto-char (point-min))
    (while (re-search-forward "^\\." nil t)
      (insert "."))
    (goto-char (point-max))
    ;; Insert newline at the end of the buffer.
    (unless (bolp)
      (insert "\n"))
    ;; Insert `.' at end of buffer (end of text mark).
    (goto-char (point-max))
    (insert "." nntp-end-of-line)))

(defun nntp-retrieve-headers-with-xover (articles &optional fetch-old)
  (set-buffer nntp-server-buffer)
  (erase-buffer)
  (cond

   ;; This server does not talk NOV.
   ((not nntp-server-xover)
    nil)

   ;; We don't care about gaps.
   ((or (not nntp-nov-gap)
	fetch-old)
    (nntp-send-xover-command
     (if fetch-old
	 (if (numberp fetch-old)
	     (max 1 (- (car articles) fetch-old))
	   1)
       (car articles))
     (car (last articles)) 'wait)

    (goto-char (point-min))
    (when (looking-at "[1-5][0-9][0-9] ")
      (delete-region (point) (progn (forward-line 1) (point))))
    (while (search-forward "\r" nil t)
      (replace-match "" t t))
    (goto-char (point-max))
    (forward-line -1)
    (when (looking-at "\\.")
      (delete-region (point) (progn (forward-line 1) (point)))))

   ;; We do it the hard way.  For each gap, an XOVER command is sent
   ;; to the server.  We do not wait for a reply from the server, we
   ;; just send them off as fast as we can.  That means that we have
   ;; to count the number of responses we get back to find out when we
   ;; have gotten all we asked for.
   ((numberp nntp-nov-gap)
    (let ((count 0)
	  (received 0)
	  (last-point (point-min))
	  (buf nntp-server-buffer)
	  ;;(process-buffer (nntp-find-connection (current-buffer))))
	  first)
      ;; We have to check `nntp-server-xover'.  If it gets set to nil,
      ;; that means that the server does not understand XOVER, but we
      ;; won't know that until we try.
      (while (and nntp-server-xover articles)
	(setq first (car articles))
	;; Search forward until we find a gap, or until we run out of
	;; articles.
	(while (and (cdr articles)
		    (< (- (nth 1 articles) (car articles)) nntp-nov-gap))
	  (setq articles (cdr articles)))

	(when (nntp-send-xover-command first (car articles))
	  (setq articles (cdr articles)
		count (1+ count))

	  ;; Every 400 requests we have to read the stream in
	  ;; order to avoid deadlocks.
	  (when (or (null articles)	;All requests have been sent.
		    (zerop (% count nntp-maximum-request)))
	    (accept-process-output)
	    ;; On some Emacs versions the preceding function has
	    ;; a tendency to change the buffer.  Perhaps.  It's
	    ;; quite difficult to reproduce, because it only
	    ;; seems to happen once in a blue moon.
	    (set-buffer buf)
	    (while (progn
		     (goto-char last-point)
		     ;; Count replies.
		     (while (re-search-forward "^[0-9][0-9][0-9] " nil t)
		       (setq received (1+ received)))
		     (setq last-point (point))
		     (< received count))
	      (accept-process-output)
	      (set-buffer buf)))))

      (when nntp-server-xover
	;; Wait for the reply from the final command.
	(goto-char (point-max))
	(re-search-backward "^[0-9][0-9][0-9] " nil t)
	(when (looking-at "^[23]")
	  (while (progn
		   (goto-char (point-max))
		   (forward-line -1)
		   (not (looking-at "^\\.\r?\n")))
	    (nntp-accept-response)))

	;; We remove any "." lines and status lines.
	(goto-char (point-min))
	(while (search-forward "\r" nil t)
	  (delete-char -1))
	(goto-char (point-min))
	(delete-matching-lines "^\\.$\\|^[1-5][0-9][0-9] ")
	;;(copy-to-buffer nntp-server-buffer (point-min) (point-max))
	t))))

  nntp-server-xover)

(defun nntp-send-xover-command (beg end &optional wait-for-reply)
  "Send the XOVER command to the server."
  (let ((range (format "%d-%d" beg end))
	(nntp-inhibit-erase t))
    (if (stringp nntp-server-xover)
	;; If `nntp-server-xover' is a string, then we just send this
	;; command.
	(if wait-for-reply
	    (nntp-send-command-nodelete
	     "\r?\n\\.\r?\n" nntp-server-xover range)
	  ;; We do not wait for the reply.
	  (nntp-send-command-nodelete "\r?\n\\.\r?\n" nntp-server-xover range))
      (let ((commands nntp-xover-commands))
	;; `nntp-xover-commands' is a list of possible XOVER commands.
	;; We try them all until we get at positive response.
	(while (and commands (eq nntp-server-xover 'try))
	  (nntp-send-command-nodelete "\r?\n\\.\r?\n" (car commands) range)
	  (save-excursion
	    (set-buffer nntp-server-buffer)
	    (goto-char (point-min))
	    (and (looking-at "[23]")	; No error message.
		 ;; We also have to look at the lines.  Some buggy
		 ;; servers give back simple lines with just the
		 ;; article number.  How... helpful.
		 (progn
		   (forward-line 1)
		   (looking-at "[0-9]+\t...")) ; More text after number.
		 (setq nntp-server-xover (car commands))))
	  (setq commands (cdr commands)))
	;; If none of the commands worked, we disable XOVER.
	(when (eq nntp-server-xover 'try)
	  (save-excursion
	    (set-buffer nntp-server-buffer)
	    (erase-buffer)
	    (setq nntp-server-xover nil)))
	nntp-server-xover))))

;;; Alternative connection methods.

(defun nntp-wait-for-string (regexp)
  "Wait until string arrives in the buffer."
  (let ((buf (current-buffer)))
    (goto-char (point-min))
    (while (not (re-search-forward regexp nil t))
      (accept-process-output (nntp-find-connection nntp-server-buffer))
      (set-buffer buf)
      (goto-char (point-min)))))

(defun nntp-open-telnet (buffer)
  (save-excursion
    (set-buffer buffer)
    (erase-buffer)
    (let ((proc (apply
		 'start-process
		 "nntpd" buffer nntp-telnet-command nntp-telnet-switches))
	  (case-fold-search t))
      (when (memq (process-status proc) '(open run))
	(process-send-string proc "set escape \^X\n")
	(process-send-string proc (concat "open " nntp-address "\n"))
	(nntp-wait-for-string "^\r*.?login:")
	(process-send-string
	 proc (concat
	       (or nntp-telnet-user-name
		   (setq nntp-telnet-user-name (read-string "login: ")))
	       "\n"))
	(nntp-wait-for-string "^\r*.?password:")
	(process-send-string
	 proc (concat
	       (or nntp-telnet-passwd
		   (setq nntp-telnet-passwd
			 (nnmail-read-passwd "Password: ")))
	       "\n"))
	(erase-buffer)
	(nntp-wait-for-string "bash\\|\$ *\r?$\\|> *\r?")
	(process-send-string
	 proc (concat (mapconcat 'identity nntp-telnet-parameters " ") "\n"))
	(nntp-wait-for-string "^\r*200")
	(beginning-of-line)
	(delete-region (point-min) (point))
	(process-send-string proc "\^]")
	(nntp-wait-for-string "^telnet")
	(process-send-string proc "mode character\n")
	(accept-process-output proc 1)
	(sit-for 1)
	(goto-char (point-min))
	(forward-line 1)
	(delete-region (point) (point-max)))
      proc)))

(defun nntp-open-rlogin (buffer)
  "Open a connection to SERVER using rsh."
  (let ((proc (if nntp-rlogin-user-name
		  (start-process
		   "nntpd" buffer "rsh"
		   nntp-address "-l" nntp-rlogin-user-name
		   (mapconcat 'identity
			      nntp-rlogin-parameters " "))
		(start-process
		 "nntpd" buffer "rsh" nntp-address
		 (mapconcat 'identity
			    nntp-rlogin-parameters " ")))))
    (set-buffer buffer)
    (nntp-wait-for-string "^\r*200")
    (beginning-of-line)
    (delete-region (point-min) (point))
    proc))

(defun nntp-find-group-and-number ()
  (save-excursion
    (save-restriction
      (set-buffer nntp-server-buffer)
      (narrow-to-region (goto-char (point-min))
			(or (search-forward "\n\n" nil t) (point-max)))
      (goto-char (point-min))
      ;; We first find the number by looking at the status line.
      (let ((number (and (looking-at "2[0-9][0-9] +\\([0-9]+\\) ")
			 (string-to-int
			  (buffer-substring (match-beginning 1)
					    (match-end 1)))))
	    group newsgroups xref)
	(and number (zerop number) (setq number nil))
	;; Then we find the group name.
	(setq group
	      (cond
	       ;; If there is only one group in the Newsgroups header,
	       ;; then it seems quite likely that this article comes
	       ;; from that group, I'd say.
	       ((and (setq newsgroups (mail-fetch-field "newsgroups"))
		     (not (string-match "," newsgroups)))
		newsgroups)
	       ;; If there is more than one group in the Newsgroups
	       ;; header, then the Xref header should be filled out.
	       ;; We hazard a guess that the group that has this
	       ;; article number in the Xref header is the one we are
	       ;; looking for.  This might very well be wrong if this
	       ;; article happens to have the same number in several
	       ;; groups, but that's life.
	       ((and (setq xref (mail-fetch-field "xref"))
		     number
		     (string-match (format "\\([^ :]+\\):%d" number) xref))
		(substring xref (match-beginning 1) (match-end 1)))
	       (t "")))
	(when (string-match "\r" group)
	  (setq group (substring group 0 (match-beginning 0))))
	(cons group number)))))

(provide 'nntp)

;;; nntp.el ends here
