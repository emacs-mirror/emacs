;;; nntp.el --- nntp access for Gnus

;; Copyright (C) 1987,88,89,90,92,93,94,95 Free Software Foundation, Inc.

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

(require 'rnews)
(require 'sendmail)
(require 'nnheader)

(eval-when-compile (require 'cl))

(eval-and-compile
  (autoload 'news-setup "rnewspost")
  (autoload 'news-reply-mode "rnewspost")
  (autoload 'nnmail-request-post-buffer "nnmail")
  (autoload 'cancel-timer "timer")
  (autoload 'telnet "telnet" nil t)
  (autoload 'telnet-send-input "telnet" nil t)
  (autoload 'timezone-parse-date "timezone"))

(defvar nntp-server-hook nil
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

(defvar nntp-server-opened-hook nil
  "*Hook used for sending commands to the server at startup.  
The default value is `nntp-send-mode-reader', which makes an innd
server spawn an nnrpd server.  Another useful function to put in this
hook might be `nntp-send-authinfo', which will prompt for a password
to allow posting from the server.  Note that this is only necessary to
do on servers that use strict access control.")  
(add-hook 'nntp-server-opened-hook 'nntp-send-mode-reader)

(defvar nntp-open-server-function 'nntp-open-network-stream
  "*Function used for connecting to a remote system.
It will be called with the address of the remote system.

Two pre-made functions are `nntp-open-network-stream', which is the
default, and simply connects to some port or other on the remote
system (see nntp-port-number).  The other is `nntp-open-rlogin', which
does an rlogin on the remote system, and then does a telnet to the
NNTP server available there (see nntp-rlogin-parameters).")

(defvar nntp-rlogin-parameters '("telnet" "${NNTPSERVER:=localhost}" "nntp")
  "*Parameters to `nntp-open-login'.
That function may be used as `nntp-open-server-function'.  In that
case, this list will be used as the parameter list given to rsh.")

(defvar nntp-rlogin-user-name nil
  "*User name on remote system when using the rlogin connect method.")

(defvar nntp-address nil
  "*The name of the NNTP server.")

(defvar nntp-port-number "nntp"
  "*Port number to connect to.")

(defvar nntp-large-newsgroup 50
  "*The number of the articles which indicates a large newsgroup.
If the number of the articles is greater than the value, verbose
messages will be shown to indicate the current status.")

(defvar nntp-buggy-select (memq system-type '(fujitsu-uts))
  "*t if your select routine is buggy.
If the select routine signals error or fall into infinite loop while
waiting for the server response, the variable must be set to t.  In
case of Fujitsu UTS, it is set to T since `accept-process-output'
doesn't work properly.")

(defvar nntp-maximum-request 400
  "*The maximum number of the requests sent to the NNTP server at one time.
If Emacs hangs up while retrieving headers, set the variable to a
lower value.")

(defvar nntp-debug-read 10000
  "*Display '...' every 10Kbytes of a message being received if it is non-nil.
If it is a number, dots are displayed per the number.")

(defvar nntp-nov-is-evil nil
  "*If non-nil, nntp will never attempt to use XOVER when talking to the server.")

(defvar nntp-xover-commands '("XOVER" "XOVERVIEW")
  "*List of strings that are used as commands to fetch NOV lines from a server.
The strings are tried in turn until a positive response is gotten. If
none of the commands are successful, nntp will just grab headers one
by one.")

(defvar nntp-nov-gap 20
  "*Maximum allowed gap between two articles.
If the gap between two consecutive articles is bigger than this
variable, split the XOVER request into two requests.")

(defvar nntp-connection-timeout nil
  "*Number of seconds to wait before an nntp connection times out.
If this variable is nil, which is the default, no timers are set.")

(defvar nntp-news-default-headers nil
  "*If non-nil, override `mail-default-headers' when posting news.")

(defvar nntp-prepare-server-hook nil
  "*Hook run before a server is opened.
If can be used to set up a server remotely, for instance.  Say you
have an account at the machine \"other.machine\".  This machine has
access to an NNTP server that you can't access locally.  You could
then use this hook to rsh to the remote machine and start a proxy NNTP
server there that you can connect to.")

(defvar nntp-async-number 5
  "*How many articles should be prefetched when in asynchronous mode.")




(defconst nntp-version "nntp 4.0"
  "Version numbers of this version of NNTP.")

(defvar nntp-server-buffer nil
  "Buffer associated with the NNTP server process.")

(defvar nntp-server-process nil
  "The NNTP server process.
You'd better not use this variable in NNTP front-end program, but
instead use `nntp-server-buffer'.")

(defvar nntp-status-string nil
  "Save the server response message.
You'd better not use this variable in NNTP front-end program but
instead call function `nntp-status-message' to get status message.")

(defvar nntp-opened-connections nil
  "All (possibly) opened connections.")

(defvar nntp-server-xover 'try)
(defvar nntp-server-list-active-group 'try)
(defvar nntp-current-group "")
(defvar nntp-timeout-servers nil)

(defvar nntp-async-process nil)
(defvar nntp-async-buffer nil)
(defvar nntp-async-articles nil)
(defvar nntp-async-fetched nil)
(defvar nntp-async-group-alist nil)



(defvar nntp-current-server nil)
(defvar nntp-server-alist nil)
(defvar nntp-server-variables 
  (list
   (list 'nntp-server-hook nntp-server-hook)
   (list 'nntp-server-opened-hook nntp-server-opened-hook)
   (list 'nntp-port-number nntp-port-number)
   (list 'nntp-address nntp-address)
   (list 'nntp-large-newsgroup nntp-large-newsgroup)
   (list 'nntp-buggy-select nntp-buggy-select)
   (list 'nntp-maximum-request nntp-maximum-request)
   (list 'nntp-debug-read nntp-debug-read)
   (list 'nntp-nov-is-evil nntp-nov-is-evil)
   (list 'nntp-xover-commands nntp-xover-commands)
   (list 'nntp-connection-timeout nntp-connection-timeout)
   (list 'nntp-news-default-headers nntp-news-default-headers)
   (list 'nntp-prepare-server-hook nntp-prepare-server-hook) 
   (list 'nntp-async-number nntp-async-number)
   '(nntp-async-process nil)
   '(nntp-async-buffer nil)
   '(nntp-async-articles nil)
   '(nntp-async-fetched nil)
   '(nntp-async-group-alist nil)
   '(nntp-server-process nil)
   '(nntp-status-string nil)
   '(nntp-server-xover try)
   '(nntp-server-list-active-group try)
   '(nntp-current-group "")))


;;; Interface functions.

(defun nntp-retrieve-headers (sequence &optional newsgroup server)
  "Retrieve the headers to the articles in SEQUENCE."
  (nntp-possibly-change-server newsgroup server)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (if (and (not gnus-nov-is-evil) 
	     (not nntp-nov-is-evil)
	     (nntp-retrieve-headers-with-xover sequence))
        'nov
      (let ((number (length sequence))
	    (count 0)
	    (received 0)
	    (last-point (point-min)))
	;; Send HEAD command.
	(while sequence
	  (nntp-send-strings-to-server 
	   "HEAD" (if (numberp (car sequence)) (int-to-string (car sequence))
		    (car sequence)))
	  (setq sequence (cdr sequence)
		count (1+ count))
	  ;; Every 400 header requests we have to read stream in order
	  ;;  to avoid deadlock.
	  (if (or (null sequence)	;All requests have been sent.
		  (zerop (% count nntp-maximum-request)))
	      (progn
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
		       (message "NNTP: Receiving headers... %d%%"
				(/ (* received 100) number)))
		  (nntp-accept-response)))))
	;; Wait for text of last command.
	(goto-char (point-max))
	(re-search-backward "^[0-9]" nil t)
	(if (looking-at "^[23]")
	    (while (progn
		     (goto-char (- (point-max) 3))
		     (not (looking-at "^\\.\r?\n")))
	      (nntp-accept-response)))
	(and (numberp nntp-large-newsgroup)
	     (> number nntp-large-newsgroup)
	     (message "NNTP: Receiving headers...done"))

	;; Now all of replies are received.
	(setq received number)
	;; First, fold continuation lines.
	(goto-char (point-min))
	(while (re-search-forward "\\(\r?\n[ \t]+\\)+" nil t)
	  (replace-match " "))
	;; Remove all "\r"'s
	(goto-char (point-min))
	(while (search-forward "\r" nil t)
	  (replace-match ""))
	'headers))))


(defun nntp-retrieve-groups (groups &optional server)
  (nntp-possibly-change-server nil server)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (and (eq nntp-server-list-active-group 'try)
	 (nntp-try-list-active (car groups)))
    (erase-buffer)
    (let ((count 0)
	  (received 0)
	  (last-point (point-min))
	  (command (if nntp-server-list-active-group
		       "LIST ACTIVE" "GROUP")))
      (while groups
	(nntp-send-strings-to-server command (car groups))
	(setq groups (cdr groups))
	(setq count (1+ count))
	;; Every 400 requests we have to read the stream in
	;; order to avoid deadlocks.
	(if (or (null groups)		;All requests have been sent.
		(zerop (% count nntp-maximum-request)))
	    (progn
	      (nntp-accept-response)
	      (while (progn
		       (goto-char last-point)
		       ;; Count replies.
		       (while (re-search-forward "^[0-9]" nil t)
			 (setq received (1+ received)))
		       (setq last-point (point))
		       (< received count))
		(nntp-accept-response)))))

      ;; Wait for the reply from the final command.
      (if nntp-server-list-active-group
	  (progn
	    (goto-char (point-max))
	    (re-search-backward "^[0-9]" nil t)
	    (if (looking-at "^[23]")
		(while (progn
			 (goto-char (- (point-max) 3))
			 (not (looking-at "^\\.\r?\n")))
		  (nntp-accept-response)))))

      ;; Now all replies are received. We remove CRs.
      (goto-char (point-min))
      (while (search-forward "\r" nil t)
	(replace-match "" t t))

      (if nntp-server-list-active-group
	  (progn
	    ;; We have read active entries, so we just delete the
	    ;; superfluous gunk.
	    (goto-char (point-min))
	    (while (re-search-forward "^[.2-5]" nil t)
	      (delete-region (match-beginning 0) 
			     (progn (forward-line 1) (point))))
	    'active)
	'group))))

(defun nntp-open-server (server &optional defs connectionless)
  "Open the virtual server SERVER.
If CONNECTIONLESS is non-nil, don't attempt to connect to any physical
servers."
  (nnheader-init-server-buffer)
  (if (nntp-server-opened server)
      t
    (if (or (stringp (car defs))
	    (numberp (car defs)))
	(setq defs (cons (list 'nntp-port-number (car defs)) (cdr defs))))
    (or (assq 'nntp-address defs)
	(setq defs (append defs (list (list 'nntp-address server)))))
    (if (and nntp-current-server
	     (not (equal server nntp-current-server)))
	(setq nntp-server-alist 
	      (cons (list nntp-current-server
			  (nnheader-save-variables nntp-server-variables))
		    nntp-server-alist)))
    (let ((state (assoc server nntp-server-alist)))
      (if state 
	  (progn
	    (nnheader-restore-variables (nth 1 state))
	    (setq nntp-server-alist (delq state nntp-server-alist)))
	(nnheader-set-init-variables nntp-server-variables defs)))
    (setq nntp-current-server server)
    ;; We have now changed to the proper virtual server.  We then
    ;; check that the physical server is opened.
    (if (or (nntp-server-opened server)
	    connectionless)
	t
      (if (member nntp-address nntp-timeout-servers)
	  nil
	;; We open a connection to the physical nntp server.
	(run-hooks 'nntp-prepare-server-hook)
	(nntp-open-server-semi-internal nntp-address nntp-port-number)))))

(defun nntp-close-server (&optional server)
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
	(if (nntp-server-opened)
	    (nntp-send-command nil "QUIT")))
    (nntp-close-server-internal server)
    (setq nntp-timeout-servers (delete server nntp-timeout-servers))))

(defalias 'nntp-request-quit (symbol-function 'nntp-close-server))

(defun nntp-request-close ()
  "Close all server connections."
  (let (proc)
    (while nntp-opened-connections
      (setq proc (pop nntp-opened-connections))
      (and proc (delete-process proc)))
    (and nntp-async-buffer
	 (get-buffer nntp-async-buffer)
	 (kill-buffer nntp-async-buffer))
    (while nntp-server-alist
      (and (setq proc (nth 1 (assq 'nntp-async-buffer
				   (car nntp-server-alist))))
	   (buffer-name proc)
	   (kill-buffer proc))
      (setq nntp-server-alist (cdr nntp-server-alist)))
    (setq nntp-current-server nil
	  nntp-timeout-servers nil
	  nntp-async-group-alist nil)))

(defun nntp-server-opened (&optional server)
  "Say whether a connection to SERVER has been opened."
  (and (equal server nntp-current-server)
       nntp-server-buffer
       (buffer-name nntp-server-buffer)
       nntp-server-process
       (memq (process-status nntp-server-process) '(open run))))

(defun nntp-status-message (&optional server)
  "Return server status as a string."
  (if (and nntp-status-string
	   ;; NNN MESSAGE
	   (string-match "[0-9][0-9][0-9][ \t]+\\([^\r]*\\).*$"
			 nntp-status-string))
      (substring nntp-status-string (match-beginning 1) (match-end 1))
    ;; Empty message if nothing.
    (or nntp-status-string "")))

(defun nntp-request-article (id &optional newsgroup server buffer)
  "Request article ID (message-id or number)."
  (nntp-possibly-change-server newsgroup server)

  (let (found)

    ;; First we see whether we can get the article from the async buffer. 
    (if (and (numberp id)
	     nntp-async-articles
	     (memq id nntp-async-fetched))
	(save-excursion
	  (set-buffer nntp-async-buffer)
	  (let ((opoint (point))
		(art (if (numberp id) (int-to-string id) id))
		beg end)
	    (if (and (or (re-search-forward (concat "^2.. +" art) nil t)
			 (progn
			   (goto-char (point-min))
			   (re-search-forward (concat "^2.. +" art) opoint t)))
		     (progn
		       (beginning-of-line)
		       (setq beg (point)
			     end (re-search-forward "^\\.\r?\n" nil t))))
		(progn
		  (setq found t)
		  (save-excursion
		    (set-buffer (or buffer nntp-server-buffer))
		    (erase-buffer)
		    (insert-buffer-substring nntp-async-buffer beg end)
		    (let ((nntp-server-buffer (current-buffer)))
		      (nntp-decode-text)))
		  (delete-region beg end)
		  (and nntp-async-articles
		       (nntp-async-fetch-articles id)))))))

    (if found 
	t
      ;; The article was not in the async buffer, so we fetch it now.
      (unwind-protect
	  (progn
	    (if buffer (set-process-buffer nntp-server-process buffer))
	    (let ((nntp-server-buffer (or buffer nntp-server-buffer))
		  (art (or (and (numberp id) (int-to-string id)) id)))
	      ;; If NEmacs, end of message may look like: "\256\215" (".^M")
	      (prog1
		  (nntp-send-command "^\\.\r?\n" "ARTICLE" art)
		(nntp-decode-text)
		(and nntp-async-articles (nntp-async-fetch-articles id)))))
	(if buffer (set-process-buffer 
		    nntp-server-process nntp-server-buffer))))))

(defun nntp-request-body (id &optional newsgroup server)
  "Request body of article ID (message-id or number)."
  (nntp-possibly-change-server newsgroup server)
  (prog1
      ;; If NEmacs, end of message may look like: "\256\215" (".^M")
      (nntp-send-command
       "^\\.\r?\n" "BODY" (or (and (numberp id) (int-to-string id)) id))
    (nntp-decode-text)))

(defun nntp-request-head (id &optional newsgroup server)
  "Request head of article ID (message-id or number)."
  (nntp-possibly-change-server newsgroup server)
  (prog1
      (nntp-send-command 
       "^\\.\r?\n" "HEAD" (or (and (numberp id) (int-to-string id)) id))
    (nntp-decode-text)))

(defun nntp-request-stat (id &optional newsgroup server)
  "Request STAT of article ID (message-id or number)."
  (nntp-possibly-change-server newsgroup server)
  (nntp-send-command 
   "^[23].*\r?\n" "STAT" (or (and (numberp id) (int-to-string id)) id)))

(defun nntp-request-group (group &optional server dont-check)
  "Select GROUP."
  (nntp-send-command "^.*\r?\n" "GROUP" group)
  (setq nntp-current-group group)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (goto-char (point-min))
    (looking-at "[23]")))

(defun nntp-request-asynchronous (group &optional server articles)
  (and nntp-async-articles (nntp-async-request-group group))
  (and 
   nntp-async-number
   (if (not (or (nntp-async-server-opened)
		(nntp-async-open-server)))
       (progn
	 (message "Can't open second connection to %s" nntp-address)
	 (ding)
	 (setq nntp-async-articles nil)
	 (sit-for 2))
     (setq nntp-async-articles articles)
     (setq nntp-async-fetched nil)
     (save-excursion
       (set-buffer nntp-async-buffer)
       (erase-buffer))
     (nntp-async-send-strings "GROUP" group)
     t)))

(defun nntp-list-active-group (group &optional server)
  (nntp-send-command "^.*\r?\n" "LIST ACTIVE" group))

(defun nntp-request-group-description (group &optional server)
  "Get description of GROUP."
  (nntp-possibly-change-server nil server)
  (prog1
      (nntp-send-command "^.*\r?\n" "XGTITLE" group)
    (nntp-decode-text)))

(defun nntp-close-group (group &optional server)
  (setq nntp-current-group nil)
  t)

(defun nntp-request-list (&optional server)
  "List active groups."
  (nntp-possibly-change-server nil server)
  (prog1
      (nntp-send-command "^\\.\r?\n" "LIST")
    (nntp-decode-text)))

(defun nntp-request-list-newsgroups (&optional server)
  "List groups."
  (nntp-possibly-change-server nil server)
  (prog1
      (nntp-send-command "^\\.\r?\n" "LIST NEWSGROUPS")
    (nntp-decode-text)))

(defun nntp-request-newgroups (date &optional server)
  "List new groups."
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

(defun nntp-request-list-distributions (&optional server)
  "List distributions."
  (nntp-possibly-change-server nil server)
  (prog1
      (nntp-send-command "^\\.\r?\n" "LIST DISTRIBUTIONS")
    (nntp-decode-text)))

(defun nntp-request-last (&optional newsgroup server)
  "Decrease the current article pointer."
  (nntp-possibly-change-server newsgroup server)
  (nntp-send-command "^[23].*\r?\n" "LAST"))

(defun nntp-request-next (&optional newsgroup server)
  "Advance the current article pointer."
  (nntp-possibly-change-server newsgroup server)
  (nntp-send-command "^[23].*\r?\n" "NEXT"))

(defun nntp-request-post (&optional server)
  "Post the current buffer."
  (nntp-possibly-change-server nil server)
  (if (nntp-send-command "^[23].*\r?\n" "POST")
      (progn
	(nntp-encode-text)
	(nntp-send-region-to-server (point-min) (point-max))
	;; 1.2a NNTP's post command is buggy. "^M" (\r) is not
	;;  appended to end of the status message.
	(nntp-wait-for-response "^[23].*\n"))))

(defun nntp-request-post-buffer 
  (post group subject header article-buffer info follow-to respect-poster)
  "Request a buffer suitable for composing an article.
If POST, this is an original article; otherwise it's a followup.
GROUP is the group to be posted to, the article should have subject
SUBJECT.  HEADER is a Gnus header vector.  ARTICLE-BUFFER contains the
article being followed up.  INFO is a Gnus info list.  If FOLLOW-TO,
post to this group instead.  If RESPECT-POSTER, heed the special
\"poster\" value of the Followup-to header."
  (if (assq 'to-address (nth 5 info))
      (nnmail-request-post-buffer 
       post group subject header article-buffer info follow-to respect-poster)
    (let ((mail-default-headers 
	   (or nntp-news-default-headers mail-default-headers))
	  from date to followup-to newsgroups message-of
	  references distribution message-id)
      (save-excursion
	(set-buffer (get-buffer-create "*post-news*"))
	(news-reply-mode)
	(if (and (buffer-modified-p)
		 (> (buffer-size) 0)
		 (not (y-or-n-p "Unsent article being composed; erase it? ")))
	    ()
	  (erase-buffer)
	  (if post
	      (news-setup nil subject nil group nil)
	    (save-excursion
	      (set-buffer article-buffer)
	      (goto-char (point-min))
	      (narrow-to-region (point-min)
				(progn (search-forward "\n\n") (point)))
	      (setq from (mail-header-from header))
	      (setq date (mail-header-date header))
	      (and from
		   (let ((stop-pos 
			  (string-match "  *at \\|  *@ \\| *(\\| *<" from)))
		     (setq 
		      message-of
		      (concat (if stop-pos (substring from 0 stop-pos) from) 
			      "'s message of " date))))
	      (setq subject (or subject (mail-header-subject header)))
	      (or (string-match "^[Rr][Ee]:" subject)
		  (setq subject (concat "Re: " subject)))
	      (setq followup-to (mail-fetch-field "followup-to"))
	      (if (or (null respect-poster) ;Ignore followup-to: field.
		      (string-equal "" followup-to) ;Bogus header.
		      (string-equal "poster" followup-to);Poster
		      (and (eq respect-poster 'ask)
			   followup-to
			   (not (y-or-n-p (concat "Followup to " 
						  followup-to "? ")))))
		  (setq followup-to nil))
	      (setq newsgroups
		    (or follow-to followup-to (mail-fetch-field "newsgroups")))
	      (setq references (mail-header-references header))
	      (setq distribution (mail-fetch-field "distribution"))
	      ;; Remove bogus distribution.
	      (and (stringp distribution)
		   (string-match "world" distribution)
		   (setq distribution nil))
	      (setq message-id (mail-header-id header))
	      (widen))
	    (setq news-reply-yank-from from)
	    (setq news-reply-yank-message-id message-id)
	    (news-setup to subject message-of 
			(if (stringp newsgroups) newsgroups "") 
			article-buffer)
	    (if (and newsgroups (listp newsgroups))
		(progn
		  (goto-char (point-min))
		  (while newsgroups
		    (insert (car (car newsgroups)) ": " 
			    (cdr (car newsgroups)) "\n")
		    (setq newsgroups (cdr newsgroups)))))
	    (nnheader-insert-references references message-id)
	    (if distribution
		(progn
		  (mail-position-on-field "Distribution")
		  (insert distribution)))))
	(current-buffer)))))

;;; Internal functions.

(defun nntp-send-mode-reader ()
  "Send the MODE READER command to the nntp server.
This function is supposed to be called from `nntp-server-opened-hook'.
It will make innd servers spawn an nnrpd process to allow actual article
reading."
  (nntp-send-command "^.*\r?\n" "MODE READER"))

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
  (and (file-exists-p "~/.nntp-authinfo")
       (save-excursion
	 (set-buffer (get-buffer-create " *tull*"))
	 (insert-file-contents "~/.nntp-authinfo")
	 (goto-char (point-min))
	 (nntp-send-command "^.*\r?\n" "AUTHINFO USER" (user-login-name))
	 (nntp-send-command "^.*\r?\n" "AUTHINFO PASS" 
			    (buffer-substring (point)
					      (progn (end-of-line) (point))))
	 (kill-buffer (current-buffer)))))

(defun nntp-default-sentinel (proc status)
  "Default sentinel function for NNTP server process."
  (let ((servers nntp-server-alist)
	server)
    ;; Go through the alist of server names and find the name of the
    ;; server that the process that sent the signal is connected to.
    ;; If you get my drift.
    (if (equal proc nntp-server-process)
	(setq server nntp-address)
      (while (and servers 
		  (not (equal proc (nth 1 (assq 'nntp-server-process
						(car servers))))))
	(setq servers (cdr servers)))
      (setq server (car (car servers))))
    (and server
	 (progn
	   (message "nntp: Connection closed to server %s" server)
	   (ding)))))

(defun nntp-kill-connection (server)
  (let ((proc (nth 1 (assq 'nntp-server-process 
			   (assoc server nntp-server-alist)))))
    (and proc (delete-process (process-name proc)))
    (nntp-close-server server)
    (setq nntp-timeout-servers (cons server nntp-timeout-servers))
    (setq nntp-status-string 
	  (message "Connection timed out to server %s." server))
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
    (goto-char (point-min))
    (delete-region (point) (progn (forward-line 1) (point)))
    ;; Delete `^M' at the end of lines.
    (while (not (eobp))
      (end-of-line)
      (and (= (preceding-char) ?\r)
	   (delete-char -1))
      (forward-line 1))
    ;; Delete `.' at end of the buffer (end of text mark).
    (goto-char (point-max))
    (forward-line -1)
    (if (looking-at "^\\.\n")
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
    ;; Insert newline at end of buffer.
    (goto-char (point-max))
    (or (bolp) (insert "\n"))
    ;; Replace `.' at beginning of line with `..'.
    (goto-char (point-min))
    ;; (replace-regexp "^\\." "..")
    (while (search-forward "\n." nil t)
      (insert "."))
    ;; Insert `.' at end of buffer (end of text mark).
    (goto-char (point-max))
    (insert ".\r\n")))


;;;
;;; Synchronous Communication with NNTP Server.
;;;

(defun nntp-send-command (response cmd &rest args)
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
	    (buffer-substring (point-min) (point)))
      (if status
	  (progn
	    (setq wait t)
	    (while wait
	      (goto-char (point-max))
	      (forward-line -1)		;(beginning-of-line)
	      ;;(message (buffer-substring
	      ;;	 (point)
	      ;;	 (save-excursion (end-of-line) (point))))
	      (if (looking-at regexp)
		  (setq wait nil)
		(if nntp-debug-read
		    (let ((newnum (/ (buffer-size) dotsize)))
		      (if (not (= dotnum newnum))
			  (progn
			    (setq dotnum newnum)
			    (message "NNTP: Reading %s"
				     (make-string dotnum ?.))))))
		(nntp-accept-response)))
	    ;; Remove "...".
	    (if (and nntp-debug-read (> dotnum 0))
		(message ""))
	    ;; Successfully received server response.
	    t)))))



;;;
;;; Low-Level Interface to NNTP Server.
;;; 

(defun nntp-retrieve-headers-with-xover (sequence)
  (erase-buffer)
  (cond 

   ;; This server does not talk NOV.
   ((not nntp-server-xover)
    nil)

   ;; We don't care about gaps.
   ((not nntp-nov-gap)
    (nntp-send-xover-command 
     (car sequence) (nntp-last-element sequence) 'wait)

    (goto-char (point-min))
    (if (looking-at "[1-5][0-9][0-9] ")
	(delete-region (point) (progn (forward-line 1) (point))))
    (while (search-forward "\r" nil t)
      (replace-match "" t t))
    (goto-char (point-max))
    (forward-line -1)
    (if (looking-at "\\.")
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
      (while (and nntp-server-xover sequence)
	(setq first (car sequence))
	;; Search forward until we find a gap, or until we run out of
	;; articles. 
	(while (and (cdr sequence) 
		    (< (- (nth 1 sequence) (car sequence)) nntp-nov-gap))
	  (setq sequence (cdr sequence)))

	(if (not (nntp-send-xover-command first (car sequence)))
	    ()
	  (setq sequence (cdr sequence)
		count (1+ count))

	  ;; Every 400 requests we have to read the stream in
	  ;; order to avoid deadlocks.
	  (if (or (null sequence)	;All requests have been sent.
		  (zerop (% count nntp-maximum-request)))
	      (progn
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
		  (set-buffer buf))))))

      (if (not nntp-server-xover)
	  ()
	;; Wait for the reply from the final command.
	(goto-char (point-max))
	(re-search-backward "^[0-9][0-9][0-9] " nil t)
	(if (looking-at "^[23]")
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
  (let ((range (format "%d-%d" beg end)))
    (if (stringp nntp-server-xover)
	;; If `nntp-server-xover' is a string, then we just send this
	;; command.
	(if wait-for-reply
	    (nntp-send-command "^\\.\r?\n" nntp-server-xover range)
	  ;; We do not wait for the reply.
	  (progn
	    (nntp-send-strings-to-server nntp-server-xover range)
	    t))
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
	(if (eq nntp-server-xover 'try)
	    (save-excursion
	      (set-buffer nntp-server-buffer)
	      (erase-buffer)
	      (setq nntp-server-xover nil)))
	nntp-server-xover))))

(defun nntp-send-strings-to-server (&rest strings)
  "Send list of STRINGS to news server as command and its arguments."
  (let ((cmd (concat (mapconcat 'identity strings " ") "\r\n")))
    ;; We open the nntp server if it is down.
    (or (nntp-server-opened nntp-current-server)
	(nntp-open-server nntp-current-server)
	(error "%s" (nntp-status-message)))
    ;; Send the strings.
    (process-send-string nntp-server-process cmd)))

(defun nntp-send-region-to-server (begin end)
  "Send current buffer region (from BEGIN to END) to news server."
  (save-excursion
    ;; We have to work in the buffer associated with NNTP server
    ;;  process because of NEmacs hack.
    (copy-to-buffer nntp-server-buffer begin end)
    (set-buffer nntp-server-buffer)
    (setq begin (point-min))
    (setq end (point-max))
    ;; `process-send-region' does not work if text to be sent is very
    ;;  large. I don't know maximum size of text sent correctly.
    (let ((last nil)
	  (size 100))			;Size of text sent at once.
      (save-restriction
	(narrow-to-region begin end)
	(goto-char begin)
	(while (not (eobp))
	  ;;(setq last (min end (+ (point) size)))
	  ;; NEmacs gets confused if character at `last' is Kanji.
	  (setq last (save-excursion
		       (goto-char (min end (+ (point) size)))
		       (or (eobp) (forward-char 1)) ;Adjust point
		       (point)))
	  (process-send-region nntp-server-process (point) last)
	  ;; I don't know whether the next codes solve the known
	  ;;  problem of communication error of GNU Emacs.
	  (accept-process-output)
	  ;;(sit-for 0)
	  (goto-char last))))
    ;; We cannot erase buffer, because reply may be received.
    (delete-region begin end)))

(defun nntp-open-server-semi-internal (server &optional service)
  "Open SERVER.
If SERVER is nil, use value of environment variable `NNTPSERVER'.
If SERVICE, this this as the port number."
  (let ((server (or server (getenv "NNTPSERVER")))
	(status nil)
	(timer 
	 (and nntp-connection-timeout 
   	      (cond
   	       ((fboundp 'run-at-time)
		(run-at-time nntp-connection-timeout
   			     nil 'nntp-kill-connection server))
   	       ((fboundp 'start-itimer)
   		;; Not sure if this will work or not, only one way to
   		;; find out
   		(eval '(start-itimer "nntp-timeout"
				     (lambda ()
				       (nntp-kill-connection server))
				     nntp-connection-timeout nil)))))))
    (save-excursion
      (set-buffer nntp-server-buffer)
      (setq nntp-status-string "")
      (message "nntp: Connecting to server on %s..." server)
      (cond ((and server (nntp-open-server-internal server service))
	     (setq nntp-address server)
	     (setq status
		   (condition-case nil
		       (nntp-wait-for-response "^[23].*\r?\n" 'slow)
		     (error nil)
		     (quit nil)))
	     (or status (nntp-close-server-internal server))
	     (and nntp-server-process
		  (progn
		    (set-process-sentinel 
		     nntp-server-process 'nntp-default-sentinel)
		    ;; You can send commands at startup like AUTHINFO here.
		    ;; Added by Hallvard B Furuseth <h.b.furuseth@usit.uio.no>
		    (run-hooks 'nntp-server-opened-hook))))
	    ((null server)
	     (setq nntp-status-string "NNTP server is not specified."))
	    (t				; We couldn't open the server.
	     (setq nntp-status-string 
		   (buffer-substring (point-min) (point-max)))
	     (setq nntp-timeout-servers (cons server nntp-timeout-servers))))
      (and timer (cancel-timer timer))
      (message "")
      (or status
	  (setq nntp-current-server nil
		nntp-async-number nil))
      status)))

(defun nntp-open-server-internal (server &optional service)
  "Open connection to news server on SERVER by SERVICE (default is nntp)."
  (let (proc)
    (save-excursion
      ;; Use TCP/IP stream emulation package if needed.
      (or (fboundp 'open-network-stream)
	  (require 'tcp))
      ;; Initialize communication buffer.
      (nnheader-init-server-buffer)
      (set-buffer nntp-server-buffer)
      (if (setq proc
		(condition-case nil
		    (funcall nntp-open-server-function server)
		  (error nil)))
	  (progn
	    (setq nntp-server-process proc)
	    ;; Suggested by Hallvard B Furuseth <h.b.furuseth@usit.uio.no>.
	    (process-kill-without-query proc)
	    (setq nntp-address server)
	    ;; It is possible to change kanji-fileio-code in this hook.
	    (run-hooks 'nntp-server-hook)
	    (push proc nntp-opened-connections)
	    nntp-server-process)))))

(defun nntp-open-network-stream (server)
  (open-network-stream 
   "nntpd" nntp-server-buffer server nntp-port-number))

(defun nntp-open-rlogin (server)
  (let ((proc (start-process "nntpd" nntp-server-buffer "rsh" server)))
    (process-send-string proc (mapconcat 'identity nntp-rlogin-parameters
					 " "))
    (process-send-string proc "\n")))

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
		(message "NNTP: Reading...")
		(sleep-for 1)
		(message ""))
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
  (if (and server
	   (not (nntp-server-opened server)))
      ;; This virtual server isn't open, so we (re)open it here.
      (nntp-open-server server nil t))
  (if (and newsgroup 
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
	(or (memq (setq nart (car (car articles))) nntp-async-fetched)
	    (progn
	      (nntp-async-send-strings "ARTICLE " (int-to-string nart))
	      (setq nntp-async-fetched (cons nart nntp-async-fetched))))
	(setq articles (cdr articles))))))

(defun nntp-async-send-strings (&rest strings)
  (let ((cmd (concat (mapconcat 'identity strings " ") "\r\n")))
    (or (nntp-async-server-opened)
	(nntp-async-open-server)
	(error "%s" (nntp-status-message)))
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
