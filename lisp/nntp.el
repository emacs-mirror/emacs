;;; nntp.el --- nntp access for Gnus
;; Copyright (C) 1987,88,89,90,92,93,94,95,96 Free Software Foundation, Inc.

;; Author: Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;; 	Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'nnheader)
(require 'nnoo)
(eval-when-compile (require 'cl))

(nnoo-declare nntp)

(eval-and-compile
  (unless (fboundp 'open-network-stream)
    (require 'tcp)))

(eval-when-compile (require 'cl))

(eval-and-compile
  (autoload 'cancel-timer "timer")
  (autoload 'telnet "telnet" nil t)
  (autoload 'telnet-send-input "telnet" nil t)
  (autoload 'timezone-parse-date "timezone"))

(defvoo nntp-server-hook nil
  "*Hooks for the NNTP server.
If the kanji code of the NNTP server is different from the local kanji
code, the correct kanji code of the buffer associated with the NNTP
server must be specified as follows:

\(setq nntp-server-hook
      (function
       (lambda ()
	 ;; Server's Kanji code is EUC (NEmacs hack).
	 (make-local-variable 'kanji-fileio-code)
	 (setq kanji-fileio-code 0))))

If you'd like to change something depending on the server in this
hook, use the variable `nntp-address'.")

(defvoo nntp-server-opened-hook '(nntp-send-mode-reader)
  "*Hook used for sending commands to the server at startup.  
The default value is `nntp-send-mode-reader', which makes an innd
server spawn an nnrpd server.  Another useful function to put in this
hook might be `nntp-send-authinfo', which will prompt for a password
to allow posting from the server.  Note that this is only necessary to
do on servers that use strict access control.")  
(add-hook 'nntp-server-opened-hook 'nntp-send-mode-reader)

(defvoo nntp-server-action-alist 
  '(("nntpd 1\\.5\\.11t" 
     (remove-hook 'nntp-server-opened-hook 'nntp-send-mode-reader)))
  "Alist of regexps to match on server types and actions to be taken.
For instance, if you want Gnus to beep every time you connect
to innd, you could say something like:

\(setq nntp-server-action-alist
       '((\"innd\" (ding))))

You probably don't want to do that, though.")

(defvoo nntp-open-server-function 'nntp-open-network-stream
  "*Function used for connecting to a remote system.
It will be called with the address of the remote system.

Two pre-made functions are `nntp-open-network-stream', which is the
default, and simply connects to some port or other on the remote
system (see nntp-port-number).  The other is `nntp-open-rlogin', which
does an rlogin on the remote system, and then does a telnet to the
NNTP server available there (see nntp-rlogin-parameters).")

(defvoo nntp-rlogin-parameters '("telnet" "${NNTPSERVER:=localhost}" "nntp")
  "*Parameters to `nntp-open-login'.
That function may be used as `nntp-open-server-function'.  In that
case, this list will be used as the parameter list given to rsh.")

(defvoo nntp-rlogin-user-name nil
  "*User name on remote system when using the rlogin connect method.")

(defvoo nntp-address nil
  "*The name of the NNTP server.")

(defvoo nntp-port-number "nntp"
  "*Port number to connect to.")

(defvoo nntp-end-of-line "\r\n"
  "String to use on the end of lines when talking to the NNTP server.
This is \"\\r\\n\" by default, but should be \"\\n\" when
using rlogin to communicate with the server.")

(defvoo nntp-large-newsgroup 50
  "*The number of the articles which indicates a large newsgroup.
If the number of the articles is greater than the value, verbose
messages will be shown to indicate the current status.")

(defvoo nntp-buggy-select (memq system-type '(fujitsu-uts))
  "*t if your select routine is buggy.
If the select routine signals error or fall into infinite loop while
waiting for the server response, the variable must be set to t.  In
case of Fujitsu UTS, it is set to T since `accept-process-output'
doesn't work properly.")

(defvoo nntp-maximum-request 400
  "*The maximum number of the requests sent to the NNTP server at one time.
If Emacs hangs up while retrieving headers, set the variable to a
lower value.")

(defvoo nntp-debug-read 10000
  "*Display '...' every 10Kbytes of a message being received if it is non-nil.
If it is a number, dots are displayed per the number.")

(defvoo nntp-nov-is-evil nil
  "*If non-nil, nntp will never attempt to use XOVER when talking to the server.")

(defvoo nntp-xover-commands '("XOVER" "XOVERVIEW")
  "*List of strings that are used as commands to fetch NOV lines from a server.
The strings are tried in turn until a positive response is gotten. If
none of the commands are successful, nntp will just grab headers one
by one.")

(defvoo nntp-nov-gap 20
  "*Maximum allowed gap between two articles.
If the gap between two consecutive articles is bigger than this
variable, split the XOVER request into two requests.")

(defvoo nntp-connection-timeout nil
  "*Number of seconds to wait before an nntp connection times out.
If this variable is nil, which is the default, no timers are set.")

(defvoo nntp-command-timeout nil
  "*Number of seconds to wait for a response when sending a command.
If this variable is nil, which is the default, no timers are set.")

(defvoo nntp-retry-on-break nil
  "*If non-nil, re-send the command when the user types `C-g'.")

(defvoo nntp-news-default-headers nil
  "*If non-nil, override `mail-default-headers' when posting news.")

(defvoo nntp-prepare-server-hook nil
  "*Hook run before a server is opened.
If can be used to set up a server remotely, for instance.  Say you
have an account at the machine \"other.machine\".  This machine has
access to an NNTP server that you can't access locally.  You could
then use this hook to rsh to the remote machine and start a proxy NNTP
server there that you can connect to.")

(defvoo nntp-async-number 5
  "*How many articles should be prefetched when in asynchronous mode.")

(defvoo nntp-warn-about-losing-connection t
  "*If non-nil, beep when a server closes connection.")



(defconst nntp-version "nntp 4.0"
  "Version numbers of this version of NNTP.")

(defvar nntp-server-buffer nil
  "Buffer associated with the NNTP server process.")

(defvoo nntp-server-process nil
  "The NNTP server process.
You'd better not use this variable in NNTP front-end program, but
instead use `nntp-server-buffer'.")

(defvoo nntp-status-string nil
  "Save the server response message.")

(defvar nntp-opened-connections nil
  "All (possibly) opened connections.")

(defvoo nntp-server-xover 'try)
(defvoo nntp-server-list-active-group 'try)
(defvoo nntp-current-group "")
(defvoo nntp-server-type nil)

(defvoo nntp-async-process nil)
(defvoo nntp-async-buffer nil)
(defvoo nntp-async-articles nil)
(defvoo nntp-async-fetched nil)
(defvoo nntp-async-group-alist nil)


;;; Interface functions.

(nnoo-define-basics nntp)

(deffoo nntp-retrieve-headers (articles &optional group server fetch-old)
  "Retrieve the headers of ARTICLES."
  (nntp-possibly-change-server group server)
  (save-excursion
    (set-buffer nntp-server-buffer)
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
	    (message-log-max nil)
	    (last-point (point-min)))
	;; Send HEAD command.
	(while articles
	  (nntp-send-strings-to-server 
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
		     (goto-char last-point)
		     ;; Count replies.
		     (while (re-search-forward "^[0-9]" nil t)
		       (setq received (1+ received)))
		     (setq last-point (point))
		     (< received count))
	      ;; If number of headers is greater than 100, give
	      ;;  informative messages.
	      (and (numberp nntp-large-newsgroup)
		   (> number nntp-large-newsgroup)
		   (zerop (% received 20))
		   (nnheader-message 7 "NNTP: Receiving headers... %d%%"
				     (/ (* received 100) number)))
	      (nntp-accept-response))))
	;; Wait for text of last command.
	(goto-char (point-max))
	(re-search-backward "^[0-9]" nil t)
	(when (looking-at "^[23]")
	  (while (progn
		   (goto-char (- (point-max) 3))
		   (not (looking-at "^\\.\r?\n")))
	    (nntp-accept-response)))
	(and (numberp nntp-large-newsgroup)
	     (> number nntp-large-newsgroup)
	     (nnheader-message 7 "NNTP: Receiving headers...done"))

	;; Now all of replies are received.  Fold continuation lines.
	(nnheader-fold-continuation-lines)
	;; Remove all "\r"'s.
	(goto-char (point-min))
	(while (search-forward "\r" nil t)
	  (replace-match "" t t))
	'headers))))


(deffoo nntp-retrieve-groups (groups &optional server)
  "Retrieve group info on GROUPS."
  (nntp-possibly-change-server nil server)
  (save-excursion
    (set-buffer nntp-server-buffer)
    ;; The first time this is run, this variable is `try'.  So we
    ;; try.   
    (when (eq nntp-server-list-active-group 'try)
      (nntp-try-list-active (car groups)))
    (erase-buffer)
    (let ((count 0)
	  (received 0)
	  (last-point (point-min))
	  (command (if nntp-server-list-active-group "LIST ACTIVE" "GROUP")))
      (while groups
	;; Send the command to the server.
	(nntp-send-strings-to-server command (car groups))
	(setq groups (cdr groups))
	(setq count (1+ count))
	;; Every 400 requests we have to read the stream in
	;; order to avoid deadlocks.
	(when (or (null groups)		;All requests have been sent.
		  (zerop (% count nntp-maximum-request)))
	  (nntp-accept-response)
	  (while (progn
		   (goto-char last-point)
		   ;; Count replies.
		   (while (re-search-forward "^[0-9]" nil t)
		     (setq received (1+ received)))
		   (setq last-point (point))
		   (< received count))
	    (nntp-accept-response))))

      ;; Wait for the reply from the final command.
      (when nntp-server-list-active-group
	(goto-char (point-max))
	(re-search-backward "^[0-9]" nil t)
	(when (looking-at "^[23]")
	  (while (progn
		   (goto-char (- (point-max) 3))
		   (not (looking-at "^\\.\r?\n")))
	    (nntp-accept-response))))

      ;; Now all replies are received. We remove CRs.
      (goto-char (point-min))
      (while (search-forward "\r" nil t)
	(replace-match "" t t))

      (if (not nntp-server-list-active-group)
	  'group
	;; We have read active entries, so we just delete the
	;; superfluos gunk.
	(goto-char (point-min))
	(while (re-search-forward "^[.2-5]" nil t)
	  (delete-region (match-beginning 0) 
			 (progn (forward-line 1) (point))))
	'active))))

(deffoo nntp-open-server (server &optional defs connectionless)
  "Open the virtual server SERVER.
If CONNECTIONLESS is non-nil, don't attempt to connect to any physical
servers."
  ;; Called with just a port number as the defs.
  (when (or (stringp (car defs))
	    (numberp (car defs)))
    (setq defs `((nntp-port-number ,(car defs)))))
  (unless (assq 'nntp-address defs)
    (setq defs (append defs `((nntp-address ,server)))))
  (nnoo-change-server 'nntp server defs)
  (if (nntp-server-opened server)
      t
    (or (nntp-server-opened server)
	connectionless
	(prog2
	    (run-hooks 'nntp-prepare-server-hook)
	    (nntp-open-server-semi-internal nntp-address nntp-port-number)
	  (nnheader-insert "")))))

(deffoo nntp-close-server (&optional server)
  "Close connection to SERVER."
  (nntp-possibly-change-server nil server t)
  (unwind-protect
      (progn
	;; Un-set default sentinel function before closing connection.
	(and nntp-server-process
	     (eq 'nntp-default-sentinel
		 (process-sentinel nntp-server-process))
	     (set-process-sentinel nntp-server-process nil))
	;; We cannot send QUIT command unless the process is running.
	(when (nntp-server-opened server)
	  (nntp-send-command nil "QUIT")
	  ;; Give the QUIT time to arrive.
	  (sleep-for 1)))
    (nntp-close-server-internal server)))

(deffoo nntp-request-close ()
  "Close all server connections."
  (let (proc)
    (while nntp-opened-connections
      (when (setq proc (pop nntp-opened-connections))
	;; Un-set default sentinel function before closing connection.
	(when (eq 'nntp-default-sentinel (process-sentinel proc))
	  (set-process-sentinel proc nil))
	(condition-case ()
	    (process-send-string proc (concat "QUIT" nntp-end-of-line))
	  (error nil))
	;; Give the QUIT time to reach the server before we close
	;; down the process.
	(sleep-for 1)
	(delete-process proc)))
    (and nntp-async-buffer
	 (buffer-name nntp-async-buffer)
	 (kill-buffer nntp-async-buffer))
    (let ((alist (cddr (assq 'nntp nnoo-state-alist)))
	  entry)
      (while (setq entry (pop alist))
	(and (setq proc (cdr (assq 'nntp-async-buffer entry)))
	     (buffer-name proc)
	     (kill-buffer proc))))
    (nnoo-close-server 'nntp)
    (setq nntp-async-group-alist nil
	  nntp-async-articles nil)))

(deffoo nntp-server-opened (&optional server)
  "Say whether a connection to SERVER has been opened."
  (and (nnoo-current-server-p 'nntp server)
       nntp-server-buffer
       (buffer-name nntp-server-buffer)
       nntp-server-process
       (memq (process-status nntp-server-process) '(open run))))

(deffoo nntp-status-message (&optional server)
  "Return server status as a string."
  (if (and nntp-status-string
	   ;; NNN MESSAGE
	   (string-match "[0-9][0-9][0-9][ \t]+\\([^\r]*\\).*$"
			 nntp-status-string))
      (substring nntp-status-string (match-beginning 1) (match-end 1))
    ;; Empty message if nothing.
    (or nntp-status-string "")))

(deffoo nntp-request-article (id &optional group server buffer)
  "Request article ID (Message-ID or number)."
  (nntp-possibly-change-server group server)

  (let (found)

    ;; First we see whether we can get the article from the async buffer. 
    (when (and (numberp id)
	       nntp-async-articles
	       (memq id nntp-async-fetched))
      (save-excursion
	(set-buffer nntp-async-buffer)
	(let ((opoint (point))
	      (art (if (numberp id) (int-to-string id) id))
	      beg end)
	  (when (and (or (re-search-forward (concat "^2.. +" art) nil t)
			 (progn
			   (goto-char (point-min))
			   (re-search-forward (concat "^2.. +" art) opoint t)))
		     (progn
		       (beginning-of-line)
		       (setq beg (point)
			     end (re-search-forward "^\\.\r?\n" nil t))))
	    (setq found t)
	    (save-excursion
	      (set-buffer (or buffer nntp-server-buffer))
	      (erase-buffer)
	      (insert-buffer-substring nntp-async-buffer beg end)
	      (let ((nntp-server-buffer (current-buffer)))
		(nntp-decode-text)))
	    (delete-region beg end)
	    (when nntp-async-articles
	      (nntp-async-fetch-articles id))))))

    (if found 
	id
      ;; The article was not in the async buffer, so we fetch it now.
      (unwind-protect
	  (progn
	    (if buffer (set-process-buffer nntp-server-process buffer))
	    (let ((nntp-server-buffer (or buffer nntp-server-buffer))
		  (art (or (and (numberp id) (int-to-string id)) id)))
	      (prog1
		  (and (nntp-send-command 
			;; A bit odd regexp to ensure working over rlogin.
			"^\\.\r?\n" "ARTICLE" art)
		       (if (numberp id) 
			   (cons nntp-current-group id)
			 ;; We find out what the article number was.
			 (nntp-find-group-and-number)))
		(nntp-decode-text)
		(and nntp-async-articles (nntp-async-fetch-articles id)))))
	(when buffer 
	  (set-process-buffer nntp-server-process nntp-server-buffer))))))

(deffoo nntp-request-body (id &optional group server)
  "Request body of article ID (Message-ID or number)."
  (nntp-possibly-change-server group server)
  (prog1
      ;; If NEmacs, end of message may look like: "\256\215" (".^M")
      (nntp-send-command
       "^\\.\r?\n" "BODY" (or (and (numberp id) (int-to-string id)) id))
    (nntp-decode-text)))

(deffoo nntp-request-head (id &optional group server)
  "Request head of article ID (Message-ID or number)."
  (nntp-possibly-change-server group server)
  (prog1
      (when (nntp-send-command 
	     "^\\.\r?\n" "HEAD" (if (numberp id) (int-to-string id) id))
	(if (numberp id) id
	  ;; We find out what the article number was.
	  (nntp-find-group-and-number)))
    (nntp-decode-text)
    (save-excursion
      (set-buffer nntp-server-buffer)
      (nnheader-fold-continuation-lines))))

(deffoo nntp-request-stat (id &optional group server)
  "Request STAT of article ID (Message-ID or number)."
  (nntp-possibly-change-server group server)
  (nntp-send-command 
   "^[23].*\r?\n" "STAT" (or (and (numberp id) (int-to-string id)) id)))

(deffoo nntp-request-type (group &optional article)
  'news)

(deffoo nntp-request-group (group &optional server dont-check)
  "Select GROUP."
  (nntp-possibly-change-server nil server)
  (setq nntp-current-group
	(when (nntp-send-command "^2.*\r?\n" "GROUP" group)
	  group)))

(deffoo nntp-request-asynchronous (group &optional server articles)
  "Enable pre-fetch in GROUP."
  (when nntp-async-articles
    (nntp-async-request-group group))
  (when nntp-async-number
    (if (not (or (nntp-async-server-opened)
		 (nntp-async-open-server)))
	;; Couldn't open the second connection
	(progn
	  (message "Can't open second connection to %s" nntp-address)
	  (ding)
	  (setq nntp-async-articles nil)
	  (sit-for 2))
      ;; We opened the second connection (or it was opened already).  
      (setq nntp-async-articles articles)
      (setq nntp-async-fetched nil)
      ;; Clear any old data.
      (save-excursion
	(set-buffer nntp-async-buffer)
	(erase-buffer))
      ;; Select the correct current group on this server.
      (nntp-async-send-strings "GROUP" group)
      t)))

(deffoo nntp-list-active-group (group &optional server)
  "Return the active info on GROUP (which can be a regexp."
  (nntp-possibly-change-server group server)
  (nntp-send-command "^.*\r?\n" "LIST ACTIVE" group))

(deffoo nntp-request-group-description (group &optional server)
  "Get the description of GROUP."
  (nntp-possibly-change-server nil server)
  (prog1
      (nntp-send-command "^.*\r?\n" "XGTITLE" group)
    (nntp-decode-text)))

(deffoo nntp-close-group (group &optional server)
  "Close GROUP."
  (setq nntp-current-group nil)
  t)

(deffoo nntp-request-list (&optional server)
  "List all active groups."
  (nntp-possibly-change-server nil server)
  (prog1
      (nntp-send-command "^\\.\r?\n" "LIST")
    (nntp-decode-text)))

(deffoo nntp-request-list-newsgroups (&optional server)
  "Get descriptions on all groups on SERVER."
  (nntp-possibly-change-server nil server)
  (prog1
      (nntp-send-command "^\\.\r?\n" "LIST NEWSGROUPS")
    (nntp-decode-text)))

(deffoo nntp-request-newgroups (date &optional server)
  "List groups that have arrived since DATE."
  (nntp-possibly-change-server nil server)
  (let* ((date (timezone-parse-date date))
	 (time-string
	  (format "%s%02d%02d %s%s%s"
		  (substring (aref date 0) 2) (string-to-int (aref date 1)) 
		  (string-to-int (aref date 2)) (substring (aref date 3) 0 2)
		  (substring 
		   (aref date 3) 3 5) (substring (aref date 3) 6 8))))
    (prog1
	(nntp-send-command "^\\.\r?\n" "NEWGROUPS" time-string)
      (nntp-decode-text))))

(deffoo nntp-request-list-distributions (&optional server)
  "List distributions."
  (nntp-possibly-change-server nil server)
  (prog1
      (nntp-send-command "^\\.\r?\n" "LIST DISTRIBUTIONS")
    (nntp-decode-text)))

(deffoo nntp-request-last (&optional group server)
  "Decrease the current article pointer."
  (nntp-possibly-change-server group server)
  (nntp-send-command "^[23].*\r?\n" "LAST"))

(deffoo nntp-request-next (&optional group server)
  "Advance the current article pointer."
  (nntp-possibly-change-server group server)
  (nntp-send-command "^[23].*\r?\n" "NEXT"))

(deffoo nntp-request-post (&optional server)
  "Post the current buffer."
  (nntp-possibly-change-server nil server)
  (when (nntp-send-command "^[23].*\r?\n" "POST")
    (nnheader-insert "")
    (nntp-encode-text)
    (nntp-send-region-to-server (point-min) (point-max))
    ;; 1.2a NNTP's post command is buggy. "^M" (\r) is not
    ;;  appended to end of the status message.
    (nntp-wait-for-response "^[23].*\n")))

;;; Internal functions.

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
  (nntp-send-command "^.*\r?\n" "AUTHINFO USER"
		     (read-string "NNTP user name: "))
  (nntp-send-command "^.*\r?\n" "AUTHINFO PASS" 
		     (read-string "NNTP password: ")))

(defun nntp-send-authinfo ()
  "Send the AUTHINFO to the nntp server.
This function is supposed to be called from `nntp-server-opened-hook'.
It will prompt for a password."
  (nntp-send-command "^.*\r?\n" "AUTHINFO USER" (user-login-name))
  (nntp-send-command "^.*\r?\n" "AUTHINFO PASS" 
		     (read-string "NNTP password: ")))

(defun nntp-send-authinfo-from-file ()
  "Send the AUTHINFO to the nntp server.
This function is supposed to be called from `nntp-server-opened-hook'.
It will prompt for a password."
  (when (file-exists-p "~/.nntp-authinfo")
    (save-excursion
      (set-buffer (get-buffer-create " *authinfo*"))
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (insert-file-contents "~/.nntp-authinfo")
      (goto-char (point-min))
      (nntp-send-command "^.*\r?\n" "AUTHINFO USER" (user-login-name))
      (nntp-send-command 
       "^.*\r?\n" "AUTHINFO PASS" 
       (buffer-substring (point) (progn (end-of-line) (point))))
      (kill-buffer (current-buffer)))))

(defun nntp-default-sentinel (proc status)
  "Default sentinel function for NNTP server process."
  (let ((servers (cddr (assq 'nntp nnoo-state-alist)))
	server)
    ;; Go through the alist of server names and find the name of the
    ;; server that the process that sent the signal is connected to.
    ;; If you get my drift.
    (if (equal proc nntp-server-process)
	(setq server nntp-address)
      (while (and servers 
		  (not (equal proc (cdr (assq 'nntp-server-process
					      (car servers))))))
	(setq servers (cdr servers)))
      (setq server (caar servers)))
    (when (and server
	       nntp-warn-about-losing-connection)
      (nnheader-message 3 "nntp: Connection closed to server %s" server)
      (setq nntp-current-group "")
      (ding))))

(defun nntp-kill-connection (server)
  "Choke the connection to SERVER."
  (let ((proc (cdr (assq 'nntp-server-process 
			 (assoc server (cddr 
					(assq 'nntp nnoo-state-alist)))))))
    (when proc 
      (delete-process (process-name proc)))
    (nntp-close-server server)
    (nnheader-report
     'nntp (message "Connection timed out to server %s" server))
    (ding)
    (sit-for 1)))

;; Encoding and decoding of NNTP text.

(defun nntp-decode-text ()
  "Decode text transmitted by NNTP.
0. Delete status line.
1. Delete `^M' at end of line.
2. Delete `.' at end of buffer (end of text mark).
3. Delete `.' at beginning of line."
  (save-excursion
    (set-buffer nntp-server-buffer)
    ;; Insert newline at end of buffer.
    (goto-char (point-max))
    (or (bolp) (insert "\n"))
    ;; Delete status line.
    (delete-region (goto-char (point-min)) (progn (forward-line 1) (point)))
    ;; Delete `^M's.
    (while (search-forward "\r" nil t)
      (replace-match "" t t))
    ;; Delete `.' at end of the buffer (end of text mark).
    (goto-char (point-max))
    (forward-line -1)
    (when (looking-at "^\\.\n")
      (delete-region (point) (progn (forward-line 1) (point))))
    ;; Replace `..' at beginning of line with `.'.
    (goto-char (point-min))
    ;; (replace-regexp "^\\.\\." ".")
    (while (search-forward "\n.." nil t)
      (delete-char -1))))

(defun nntp-encode-text ()
  "Encode text in current buffer for NNTP transmission.
1. Insert `.' at beginning of line.
2. Insert `.' at end of buffer (end of text mark)."
  (save-excursion
    ;; Replace `.' at beginning of line with `..'.
    (goto-char (point-min))
    (while (search-forward "\n." nil t)
      (insert "."))
    (goto-char (point-max))
    ;; Insert newline at end of buffer.
    (or (bolp) (insert "\n"))
    ;; Insert `.' at end of buffer (end of text mark).
    (insert "." nntp-end-of-line)))


;;;
;;; Synchronous Communication with NNTP servers.
;;;

(defvar nntp-retry-command)

(defun nntp-send-command (response cmd &rest args)
  "Wait for server RESPONSE after sending CMD and optional ARGS to server."
  (let ((timer 
	 (and nntp-command-timeout 
	      (nnheader-run-at-time
	       nntp-command-timeout nil 'nntp-kill-command 
	       (nnoo-current-server 'nntp))))
	(nntp-retry-command t)
	result)
    (unwind-protect
	(save-excursion
	  (while nntp-retry-command
	    (setq nntp-retry-command nil)
	    ;; Clear communication buffer.
	    (set-buffer nntp-server-buffer)
	    (widen)
	    (erase-buffer)
	    (if nntp-retry-on-break
		(condition-case ()
		    (progn
		      (apply 'nntp-send-strings-to-server cmd args)
		      (setq result
			    (if response
				(nntp-wait-for-response response)
			      t)))
		  (quit (setq nntp-retry-command t)))
	      (apply 'nntp-send-strings-to-server cmd args)
	      (setq result
		    (if response
			(nntp-wait-for-response response)
		      t))))
	  result)
      (when timer 
	(nnheader-cancel-timer timer)))))

(defun nntp-kill-command (server)
  "Kill and restart the connection to SERVER."
  (let ((proc (cdr (assq 
		    'nntp-server-process 
		    (assoc server (cddr (assq 'nntp nnoo-state-alist)))))))
    (when proc 
      (delete-process (process-name proc)))
    (nntp-close-server server)
    (nntp-open-server server)
    (when nntp-current-group
      (nntp-request-group nntp-current-group))
    (setq nntp-retry-command t)))

(defun nntp-send-command-old (response cmd &rest args)
  "Wait for server RESPONSE after sending CMD and optional ARGS to server."
  (save-excursion
    ;; Clear communication buffer.
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (apply 'nntp-send-strings-to-server cmd args)
    (if response
	(nntp-wait-for-response response)
      t)))

(defun nntp-wait-for-response (regexp &optional slow)
  "Wait for server response which matches REGEXP."
  (save-excursion
    (let ((status t)
	  (wait t)
	  (dotnum 0)			;Number of "." being displayed.
	  (dotsize			;How often "." displayed.
	   (if (numberp nntp-debug-read) nntp-debug-read 10000)))
      (set-buffer nntp-server-buffer)
      ;; Wait for status response (RFC977).
      ;; 1xx - Informative message.
      ;; 2xx - Command ok.
      ;; 3xx - Command ok so far, send the rest of it.
      ;; 4xx - Command was correct, but couldn't be performed for some
      ;;       reason.
      ;; 5xx - Command unimplemented, or incorrect, or a serious
      ;;       program error occurred.
      (nntp-accept-response)
      (while wait
	(goto-char (point-min))
	(if slow
	    (progn
	      (cond ((re-search-forward "^[23][0-9][0-9]" nil t)
		     (setq wait nil))
		    ((re-search-forward "^[45][0-9][0-9]" nil t)
		     (setq status nil)
		     (setq wait nil))
		    (t (nntp-accept-response)))
	      (if (not wait) (delete-region (point-min) 
					    (progn (beginning-of-line)
						   (point)))))
	  (cond ((looking-at "[23]")
		 (setq wait nil))
		((looking-at "[45]")
		 (setq status nil)
		 (setq wait nil))
		(t (nntp-accept-response)))))
      ;; Save status message.
      (end-of-line)
      (setq nntp-status-string
	    (nnheader-replace-chars-in-string 
	     (buffer-substring (point-min) (point)) ?\r ? ))
      (when status
	(setq wait t)
	(while wait
	  (goto-char (point-max))
	  (if (bolp) (forward-line -1) (beginning-of-line))
	  (if (looking-at regexp)
	      (setq wait nil)
	    (when nntp-debug-read
	      (let ((newnum (/ (buffer-size) dotsize))
		    (message-log-max nil))
		(unless (= dotnum newnum)
		  (setq dotnum newnum)
		  (nnheader-message 7 "NNTP: Reading %s"
				    (make-string dotnum ?.)))))
	    (nntp-accept-response)))
	;; Remove "...".
	(when (and nntp-debug-read (> dotnum 0))
	  (message ""))
	;; Successfully received server response.
	t))))



;;;
;;; Low-Level Interface to NNTP Server.
;;; 

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

(defun nntp-retrieve-headers-with-xover (articles &optional fetch-old)
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
     (nntp-last-element articles) 'wait)

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
	  (buf (current-buffer))
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
	    ;; a tendency to change the buffer. Perhaps. It's
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
	(delete-matching-lines "^\\.$\\|^[1-5][0-9][0-9] ")))))

  nntp-server-xover)

(defun nntp-send-xover-command (beg end &optional wait-for-reply)
  "Send the XOVER command to the server."
  (let ((range (format "%d-%d" (or beg 1) (or end beg 1))))
    (if (stringp nntp-server-xover)
	;; If `nntp-server-xover' is a string, then we just send this
	;; command.
	(if wait-for-reply
	    (nntp-send-command "^\\.\r?\n" nntp-server-xover range)
	  ;; We do not wait for the reply.
	  (nntp-send-strings-to-server nntp-server-xover range))
      (let ((commands nntp-xover-commands))
	;; `nntp-xover-commands' is a list of possible XOVER commands.
	;; We try them all until we get at positive response. 
	(while (and commands (eq nntp-server-xover 'try))
	  (nntp-send-command "^\\.\r?\n" (car commands) range)
	  (save-excursion
	    (set-buffer nntp-server-buffer)
	    (goto-char (point-min))
	    (and (looking-at "[23]") ; No error message.
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

(defun nntp-send-strings-to-server (&rest strings)
  "Send STRINGS to the server."
  (let ((cmd (concat (mapconcat 'identity strings " ") nntp-end-of-line)))
    ;; We open the nntp server if it is down.
    (or (nntp-server-opened (nnoo-current-server 'nntp))
	(nntp-open-server (nnoo-current-server 'nntp))
	(error (nntp-status-message)))
    ;; Send the strings.
    (process-send-string nntp-server-process cmd)
    t))

(defun nntp-send-region-to-server (begin end)
  "Send the current buffer region (from BEGIN to END) to the server."
  (save-excursion
    (let ((cur (current-buffer)))
      ;; Copy the buffer over to the send buffer.
      (nnheader-set-temp-buffer " *nntp send*")
      (insert-buffer-substring cur begin end)
      (save-excursion
	(set-buffer cur)
	(erase-buffer))
      ;; `process-send-region' does not work if the text to be sent is very
      ;; large, so we send it piecemeal.
      (let ((last (point-min))
	    (size 100))			;Size of text sent at once.
	(while (and (/= last (point-max))
		    (memq (process-status nntp-server-process) '(open run)))
	  (process-send-region 
	   nntp-server-process
	   last (setq last (min (+ last size) (point-max))))
	  ;; Read any output from the server.  May be unnecessary.
	  (accept-process-output)))
      (kill-buffer (current-buffer)))))

(defun nntp-open-server-semi-internal (server &optional service)
  "Open SERVER.
If SERVER is nil, use value of environment variable `NNTPSERVER'.
If SERVICE, this this as the port number."
  (nnheader-insert "")
  (let ((server (or server (getenv "NNTPSERVER")))
	(status nil)
	(timer 
	 (and nntp-connection-timeout 
	      (nnheader-run-at-time nntp-connection-timeout
				    nil 'nntp-kill-connection server))))
    (save-excursion
      (set-buffer nntp-server-buffer)
      (setq nntp-status-string "")
      (nnheader-message 5 "nntp: Connecting to server on %s..." nntp-address)
      (cond ((and server (nntp-open-server-internal server service))
	     (setq nntp-address server)
	     (setq status
		   (condition-case nil
		       (nntp-wait-for-response "^[23].*\r?\n" 'slow)
		     (error nil)
		     (quit nil)))
	     (unless status
	       (nntp-close-server-internal server)
	       (nnheader-report 
		'nntp "Couldn't open connection to %s" 
		(if (and nntp-address
			 (not (equal nntp-address "")))
		    nntp-address server)))
	     (when nntp-server-process
	       (set-process-sentinel 
		nntp-server-process 'nntp-default-sentinel)
	       ;; You can send commands at startup like AUTHINFO here.
	       ;; Added by Hallvard B Furuseth <h.b.furuseth@usit.uio.no>
	       (run-hooks 'nntp-server-opened-hook)))
	    ((null server)
	     (nnheader-report 'nntp "NNTP server is not specified."))
	    (t				; We couldn't open the server.
	     (nnheader-report 
	      'nntp (buffer-substring (point-min) (point-max)))))
      (when timer 
	(nnheader-cancel-timer timer))
      (message "")
      (unless status
	(nnoo-close-server 'nntp server)
	(setq nntp-async-number nil))
      status)))

(defvar nntp-default-directories '("~" "/tmp" "/")
  "Directories to as current directory in the nntp server buffer.")

(defun nntp-open-server-internal (server &optional service)
  "Open connection to news server on SERVER by SERVICE (default is nntp)."
  (let (proc)
    (save-excursion
      (set-buffer nntp-server-buffer)
      ;; Make sure we have a valid current directory for the
      ;; nntp server buffer.
      (unless (file-exists-p default-directory)
	(let ((dirs nntp-default-directories))
	  (while dirs
	    (when (file-exists-p (car dirs))
	      (setq default-directory (car dirs)
		    dirs nil))
	    (setq dirs (cdr dirs)))))
      (cond
       ((and (setq proc
		   (condition-case nil
		       (funcall nntp-open-server-function server)
		     (error nil)))
	     (memq (process-status proc) '(open run)))
	(setq nntp-server-process proc)
	(setq nntp-address server)
	;; Suggested by Hallvard B Furuseth <h.b.furuseth@usit.uio.no>.
	(process-kill-without-query proc)
	(run-hooks 'nntp-server-hook)
	(push proc nntp-opened-connections)
	(condition-case ()
	    (nntp-read-server-type)
	  (error 
	   (nnheader-report 'nntp "Couldn't open server %s" server)
	   (nntp-close-server)))
	nntp-server-process)
       (t
	(nnheader-report 'nntp "Couldn't open server %s" server))))))

(defun nntp-read-server-type ()
  "Find out what the name of the server we have connected to is."
  ;; Wait for the status string to arrive.
  (nntp-wait-for-response "^.*\n" t)
  (setq nntp-server-type (buffer-string))
  (let ((alist nntp-server-action-alist)
	entry)
    ;; Run server-specific commmands.
    (while alist
      (setq entry (pop alist))
      (when (string-match (car entry) nntp-server-type)
	(if (and (listp (cadr entry))
		 (not (eq 'lambda (caadr entry))))
	    (eval (cadr entry))
	  (funcall (cadr entry)))))))

(defun nntp-open-network-stream (server)
  (open-network-stream 
   "nntpd" nntp-server-buffer server nntp-port-number))

(defun nntp-open-rlogin (server)
  (let ((proc (if nntp-rlogin-user-name
		  (start-process
		   "nntpd" nntp-server-buffer "rsh"
		   "-l" nntp-rlogin-user-name server
		   (mapconcat 'identity
			      nntp-rlogin-parameters " "))
		(start-process
		 "nntpd" nntp-server-buffer "rsh" server
		 (mapconcat 'identity
			    nntp-rlogin-parameters " ")))))
    proc))

(defun nntp-telnet-to-machine ()
  (let (b)
    (telnet "localhost")
    (goto-char (point-min))
    (while (not (re-search-forward "^login: *" nil t))
      (sit-for 1)
      (goto-char (point-min)))
    (goto-char (point-max))
    (insert "larsi")
    (telnet-send-input)
    (setq b (point))
    (while (not (re-search-forward ">" nil t))
      (sit-for 1)
      (goto-char b))
    (goto-char (point-max))
    (insert "ls")
    (telnet-send-input)))

(defun nntp-close-server-internal (&optional server)
  "Close connection to news server."
  (nntp-possibly-change-server nil server)
  (if nntp-server-process
      (delete-process nntp-server-process))
  (setq nntp-server-process nil)
  (setq nntp-address ""))

(defun nntp-accept-response ()
  "Read response of server.
It is well-known that the communication speed will be much improved by
defining this function as macro."
  ;; To deal with server process exiting before
  ;;  accept-process-output is called.
  ;; Suggested by Jason Venner <jason@violet.berkeley.edu>.
  ;; This is a copy of `nntp-default-sentinel'.
  (let ((buf (current-buffer)))
    (prog1
	(if (or (not nntp-server-process)
		(not (memq (process-status nntp-server-process) '(open run))))
	    (error "nntp: Process connection closed; %s" (nntp-status-message))
	  (if nntp-buggy-select
	      (progn
		;; We cannot use `accept-process-output'.
		;; Fujitsu UTS requires messages during sleep-for.
		;; I don't know why.
		(nnheader-message 5 "NNTP: Reading...")
		(sleep-for 1)
		(nnheader-message 5 ""))
	    (condition-case errorcode
		(accept-process-output nntp-server-process 1)
	      (error
	       (cond ((string-equal "select error: Invalid argument" 
				    (nth 1 errorcode))
		      ;; Ignore select error.
		      nil)
		     (t
		      (signal (car errorcode) (cdr errorcode))))))))
      (set-buffer buf))))

(defun nntp-last-element (list)
  "Return last element of LIST."
  (while (cdr list)
    (setq list (cdr list)))
  (car list))

(defun nntp-possibly-change-server (newsgroup server &optional connectionless)
  "Check whether the virtual server needs changing."
  (when (and server
	     (not (nntp-server-opened server)))
    ;; This virtual server isn't open, so we (re)open it here.
    (nntp-open-server server nil t))
  (when (and newsgroup 
	     (not (equal newsgroup nntp-current-group)))
    ;; Set the proper current group.
    (nntp-request-group newsgroup server)))
 
(defun nntp-try-list-active (group)
  (nntp-list-active-group group)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (goto-char (point-min))
    (cond ((looking-at "5[0-9]+")
	   (setq nntp-server-list-active-group nil))
	  (t
	   (setq nntp-server-list-active-group t)))))

(defun nntp-async-server-opened ()
  (and nntp-async-process
       (memq (process-status nntp-async-process) '(open run))))

(defun nntp-async-open-server ()
  (save-excursion
    (set-buffer (generate-new-buffer " *async-nntp*"))
    (setq nntp-async-buffer (current-buffer))
    (buffer-disable-undo (current-buffer)))
  (let ((nntp-server-process nil)
	(nntp-server-buffer nntp-async-buffer))
    (nntp-open-server-semi-internal nntp-address nntp-port-number)
    (if (not (setq nntp-async-process nntp-server-process))
	(progn
	  (setq nntp-async-number nil))
      (set-process-buffer nntp-async-process nntp-async-buffer))))

(defun nntp-async-fetch-articles (article)
  (if (stringp article)
      ()
    (let ((articles (cdr (memq (assq article nntp-async-articles)
			       nntp-async-articles)))
	  (max (cond ((numberp nntp-async-number)
		      nntp-async-number) 
		     ((eq nntp-async-number t)
		      (length nntp-async-articles))
		     (t 0)))
	  nart)
      (while (and (>= (setq max (1- max)) 0)
		  articles)
	(or (memq (setq nart (caar articles)) nntp-async-fetched)
	    (progn
	      (nntp-async-send-strings "ARTICLE " (int-to-string nart))
	      (setq nntp-async-fetched (cons nart nntp-async-fetched))))
	(setq articles (cdr articles))))))

(defun nntp-async-send-strings (&rest strings)
  (let ((cmd (concat (mapconcat 'identity strings " ") nntp-end-of-line)))
    (or (nntp-async-server-opened)
	(nntp-async-open-server)
	(error (nntp-status-message)))
    (process-send-string nntp-async-process cmd)))

(defun nntp-async-request-group (group)
  (if (equal group nntp-current-group)
      ()
    (let ((asyncs (assoc group nntp-async-group-alist)))
      ;; A new group has been selected, so we push the current state
      ;; of async articles on an alist, and pull the old state off.
      (setq nntp-async-group-alist 
	    (cons (list nntp-current-group
			nntp-async-articles nntp-async-fetched
			nntp-async-process)
		  (delq asyncs nntp-async-group-alist)))
      (and asyncs
	   (progn
	     (setq nntp-async-articles (nth 1 asyncs))
	     (setq nntp-async-fetched (nth 2 asyncs))
	     (setq nntp-async-process (nth 3 asyncs)))))))

(provide 'nntp)

;;; nntp.el ends here
