;;; nntp.el --- nntp access for Gnus

;; Copyright (C) 1987, 1988, 1989, 1990, 1992, 1993,
;;   1994, 1995, 1996, 1997, 1998, 2000, 2001, 2002,
;;   2003, 2004, 2005 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'nnheader)
(require 'nnoo)
(require 'gnus-util)

(nnoo-declare nntp)

(eval-when-compile (require 'cl))

(defgroup nntp nil
  "NNTP access for Gnus."
  :group 'gnus)

(defvoo nntp-address nil
  "Address of the physical nntp server.")

(defvoo nntp-port-number "nntp"
  "Port number on the physical nntp server.")

(defvoo nntp-server-opened-hook '(nntp-send-mode-reader)
  "*Hook used for sending commands to the server at startup.
The default value is `nntp-send-mode-reader', which makes an innd
server spawn an nnrpd server.")

(defvoo nntp-authinfo-function 'nntp-send-authinfo
  "Function used to send AUTHINFO to the server.
It is called with no parameters.")

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
It will be called with the buffer to output in as argument.

Currently, five such functions are provided (please refer to their
respective doc string for more information), three of them establishing
direct connections to the nntp server, and two of them using an indirect
host.

Direct connections:
- `nntp-open-network-stream' (the default),
- `nntp-open-ssl-stream',
- `nntp-open-tls-stream',
- `nntp-open-telnet-stream'.

Indirect connections:
- `nntp-open-via-rlogin-and-telnet',
- `nntp-open-via-telnet-and-telnet'.")

(defvoo nntp-pre-command nil
  "*Pre-command to use with the various nntp-open-via-* methods.
This is where you would put \"runsocks\" or stuff like that.")

(defvoo nntp-telnet-command "telnet"
  "*Telnet command used to connect to the nntp server.
This command is used by the various nntp-open-via-* methods.")

(defvoo nntp-telnet-switches '("-8")
  "*Switches given to the telnet command `nntp-telnet-command'.")

(defvoo nntp-end-of-line "\r\n"
  "*String to use on the end of lines when talking to the NNTP server.
This is \"\\r\\n\" by default, but should be \"\\n\" when
using and indirect connection method (nntp-open-via-*).")

(defvoo nntp-via-rlogin-command "rsh"
  "*Rlogin command used to connect to an intermediate host.
This command is used by the `nntp-open-via-rlogin-and-telnet' method.
The default is \"rsh\", but \"ssh\" is a popular alternative.")

(defvoo nntp-via-rlogin-command-switches nil
  "*Switches given to the rlogin command `nntp-via-rlogin-command'.
If you use \"ssh\" for `nntp-via-rlogin-command', you may set this to
\(\"-C\") in order to compress all data connections, otherwise set this
to \(\"-t\" \"-e\" \"none\") or (\"-C\" \"-t\" \"-e\" \"none\") if the telnet
command requires a pseudo-tty allocation on an intermediate host.")

(defvoo nntp-via-telnet-command "telnet"
  "*Telnet command used to connect to an intermediate host.
This command is used by the `nntp-open-via-telnet-and-telnet' method.")

(defvoo nntp-via-telnet-switches '("-8")
  "*Switches given to the telnet command `nntp-via-telnet-command'.")

(defvoo nntp-via-user-name nil
  "*User name to log in on an intermediate host with.
This variable is used by the `nntp-open-via-telnet-and-telnet' method.")

(defvoo nntp-via-user-password nil
  "*Password to use to log in on an intermediate host with.
This variable is used by the `nntp-open-via-telnet-and-telnet' method.")

(defvoo nntp-via-address nil
  "*Address of an intermediate host to connect to.
This variable is used by the `nntp-open-via-rlogin-and-telnet' and
`nntp-open-via-telnet-and-telnet' methods.")

(defvoo nntp-via-envuser nil
  "*Whether both telnet client and server support the ENVIRON option.
If non-nil, there will be no prompt for a login name.")

(defvoo nntp-via-shell-prompt "bash\\|\$ *\r?$\\|> *\r?"
  "*Regular expression to match the shell prompt on an intermediate host.
This variable is used by the `nntp-open-via-telnet-and-telnet' method.")

(defvoo nntp-large-newsgroup 50
  "*The number of articles which indicates a large newsgroup.
If the number of articles is greater than the value, verbose
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

(defvoo nntp-prepare-server-hook nil
  "*Hook run before a server is opened.
If can be used to set up a server remotely, for instance.  Say you
have an account at the machine \"other.machine\".  This machine has
access to an NNTP server that you can't access locally.  You could
then use this hook to rsh to the remote machine and start a proxy NNTP
server there that you can connect to.  See also
`nntp-open-connection-function'")

(defvoo nntp-warn-about-losing-connection t
  "*If non-nil, beep when a server closes connection.")

(defvoo nntp-coding-system-for-read 'binary
  "*Coding system to read from NNTP.")

(defvoo nntp-coding-system-for-write 'binary
  "*Coding system to write to NNTP.")

(defcustom nntp-authinfo-file "~/.authinfo"
  ".netrc-like file that holds nntp authinfo passwords."
  :group 'nntp
  :type
  '(choice file
	   (repeat :tag "Entries"
		   :menu-tag "Inline"
		   (list :format "%v"
			 :value ("" ("login" . "") ("password" . ""))
			 (string :tag "Host")
			 (checklist :inline t
				    (cons :format "%v"
					  (const :format "" "login")
					  (string :format "Login: %v"))
				    (cons :format "%v"
					  (const :format "" "password")
					  (string :format "Password: %v")))))))



(defvoo nntp-connection-timeout nil
  "*Number of seconds to wait before an nntp connection times out.
If this variable is nil, which is the default, no timers are set.
NOTE: This variable is never seen to work in Emacs 20 and XEmacs 21.")

(defvoo nntp-prepare-post-hook nil
  "*Hook run just before posting an article.  It is supposed to be used
to insert Cancel-Lock headers.")

;;; Internal variables.

(defvar nntp-record-commands nil
  "*If non-nil, nntp will record all commands in the \"*nntp-log*\" buffer.")

(defvar nntp-have-messaged nil)

(defvar nntp-process-wait-for nil)
(defvar nntp-process-to-buffer nil)
(defvar nntp-process-callback nil)
(defvar nntp-process-decode nil)
(defvar nntp-process-start-point nil)
(defvar nntp-inside-change-function nil)
(defvoo nntp-last-command-time nil)
(defvoo nntp-last-command nil)
(defvoo nntp-authinfo-password nil)
(defvoo nntp-authinfo-user nil)

(defvar nntp-connection-list nil)

(defvoo nntp-server-type nil)
(defvoo nntp-connection-alist nil)
(defvoo nntp-status-string "")
(defconst nntp-version "nntp 5.0")
(defvoo nntp-inhibit-erase nil)
(defvoo nntp-inhibit-output nil)

(defvoo nntp-server-xover 'try)
(defvoo nntp-server-list-active-group 'try)

(defvar nntp-async-needs-kluge
  (string-match "^GNU Emacs 20\\.3\\." (emacs-version))
  "*When non-nil, nntp will poll asynchronous connections
once a second.  By default, this is turned on only for Emacs
20.3, which has a bug that breaks nntp's normal method of
noticing asynchronous data.")

(defvar nntp-async-timer nil)
(defvar nntp-async-process-list nil)

(defvar nntp-ssl-program
  "openssl s_client -quiet -ssl3 -connect %s:%p"
"A string containing commands for SSL connections.
Within a string, %s is replaced with the server address and %p with
port number on server.  The program should accept IMAP commands on
stdin and return responses to stdout.")



;;; Internal functions.

(defsubst nntp-send-string (process string)
  "Send STRING to PROCESS."
  ;; We need to store the time to provide timeouts, and
  ;; to store the command so the we can replay the command
  ;; if the server gives us an AUTHINFO challenge.
  (setq nntp-last-command-time (current-time)
	nntp-last-command string)
  (when nntp-record-commands
    (nntp-record-command string))
  (process-send-string process (concat string nntp-end-of-line))
  (or (memq (process-status process) '(open run))
      (nntp-report "Server closed connection")))

(defun nntp-record-command (string)
  "Record the command STRING."
  (save-excursion
    (set-buffer (get-buffer-create "*nntp-log*"))
    (goto-char (point-max))
    (let ((time (current-time)))
      (insert (format-time-string "%Y%m%dT%H%M%S" time)
	      "." (format "%03d" (/ (nth 2 time) 1000))
	      " " nntp-address " " string "\n"))))

(defun nntp-report (&rest args)
  "Report an error from the nntp backend.  The first string in ARGS
can be a format string.  For some commands, the failed command may be
retried once before actually displaying the error report."

  (when nntp-record-commands
    (nntp-record-command "*** CALLED nntp-report ***"))

  (nnheader-report 'nntp args)

  (apply 'error args))

(defun nntp-report-1 (&rest args)
  "Throws out to nntp-with-open-group-error so that the connection may
be restored and the command retried."

  (when nntp-record-commands
    (nntp-record-command "*** CONNECTION LOST ***"))

  (throw 'nntp-with-open-group-error t))

(defsubst nntp-wait-for (process wait-for buffer &optional decode discard)
  "Wait for WAIT-FOR to arrive from PROCESS."
  (save-excursion
    (set-buffer (process-buffer process))
    (goto-char (point-min))
    (while (and (or (not (memq (char-after (point)) '(?2 ?3 ?4 ?5)))
		    (looking-at "480"))
		(memq (process-status process) '(open run)))
      (when (looking-at "480")
	(nntp-handle-authinfo process))
      (when (looking-at "^.*\n")
	(delete-region (point) (progn (forward-line 1) (point))))
      (nntp-accept-process-output process)
      (goto-char (point-min)))
    (prog1
	(cond
	 ((looking-at "[45]")
	  (progn
	    (nntp-snarf-error-message)
	    nil))
	 ((not (memq (process-status process) '(open run)))
	  (nntp-report "Server closed connection"))
	 (t
	  (goto-char (point-max))
	  (let ((limit (point-min))
		response)
	    (while (not (re-search-backward wait-for limit t))
	      (nntp-accept-process-output process)
	      ;; We assume that whatever we wait for is less than 1000
	      ;; characters long.
	      (setq limit (max (- (point-max) 1000) (point-min)))
	      (goto-char (point-max)))
	    (setq response (match-string 0))
	    (with-current-buffer nntp-server-buffer
	      (setq nntp-process-response response)))
	  (nntp-decode-text (not decode))
	  (unless discard
	    (save-excursion
	      (set-buffer buffer)
	      (goto-char (point-max))
	      (insert-buffer-substring (process-buffer process))
	      ;; Nix out "nntp reading...." message.
	      (when nntp-have-messaged
		(setq nntp-have-messaged nil)
		(nnheader-message 5 ""))))
	  t))
      (unless discard
	(erase-buffer)))))

(defun nntp-kill-buffer (buffer)
  (when (buffer-name buffer)
    (kill-buffer buffer)
    (nnheader-init-server-buffer)))

(defsubst nntp-find-connection (buffer)
  "Find the connection delivering to BUFFER."
  (let ((alist nntp-connection-alist)
	(buffer (if (stringp buffer) (get-buffer buffer) buffer))
	process entry)
    (while (and alist (setq entry (pop alist)))
      (when (eq buffer (cadr entry))
	(setq process (car entry)
	      alist nil)))
    (when process
      (if (memq (process-status process) '(open run))
	  process
	(nntp-kill-buffer (process-buffer process))
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
    (if process
        (progn
          (unless (or nntp-inhibit-erase nnheader-callback-function)
            (save-excursion
              (set-buffer (process-buffer process))
              (erase-buffer)))
          (condition-case err
              (progn
                (when command
                  (nntp-send-string process command))
                (cond
                 ((eq callback 'ignore)
                  t)
                 ((and callback wait-for)
                  (nntp-async-wait process wait-for buffer decode callback)
                  t)
                 (wait-for
                  (nntp-wait-for process wait-for buffer decode))
                 (t t)))
            (error
             (nnheader-report 'nntp "Couldn't open connection to %s: %s"
                              address err))
            (quit
             (message "Quit retrieving data from nntp")
             (signal 'quit nil)
             nil)))
      (nnheader-report 'nntp "Couldn't open connection to %s" address))))

(defsubst nntp-send-command (wait-for &rest strings)
  "Send STRINGS to server and wait until WAIT-FOR returns."
  (when (and (not nnheader-callback-function)
	     (not nntp-inhibit-output))
    (save-excursion
      (set-buffer nntp-server-buffer)
      (erase-buffer)))
  (let* ((command (mapconcat 'identity strings " "))
	 (process (nntp-find-connection nntp-server-buffer))
	 (buffer (and process (process-buffer process)))
	 (pos (and buffer (with-current-buffer buffer (point)))))
    (if process
	(prog1
	    (nntp-retrieve-data command
				nntp-address nntp-port-number
				nntp-server-buffer
				wait-for nnheader-callback-function)
	  ;; If nothing to wait for, still remove possibly echo'ed commands.
	  ;; We don't have echos if nntp-open-connection-function
	  ;; is `nntp-open-network-stream', so we skip this in that case.
	  (unless (or wait-for
		      (equal nntp-open-connection-function
			     'nntp-open-network-stream))
	    (nntp-accept-response)
	    (save-excursion
	      (set-buffer buffer)
	      (goto-char pos)
	      (if (looking-at (regexp-quote command))
		  (delete-region pos (progn (forward-line 1)
					    (gnus-point-at-bol))))
	      )))
      (nnheader-report 'nntp "Couldn't open connection to %s."
		       nntp-address))))

(defun nntp-send-command-nodelete (wait-for &rest strings)
  "Send STRINGS to server and wait until WAIT-FOR returns."
  (let* ((command (mapconcat 'identity strings " "))
	 (process (nntp-find-connection nntp-server-buffer))
	 (buffer (and process (process-buffer process)))
	 (pos (and buffer (with-current-buffer buffer (point)))))
    (if process
	(prog1
	    (nntp-retrieve-data command
				nntp-address nntp-port-number
				nntp-server-buffer
				wait-for nnheader-callback-function)
	  ;; If nothing to wait for, still remove possibly echo'ed commands
	  (unless wait-for
	    (nntp-accept-response)
	    (save-excursion
	      (set-buffer buffer)
	      (goto-char pos)
	      (if (looking-at (regexp-quote command))
		  (delete-region pos (progn (forward-line 1)
					    (gnus-point-at-bol)))))))
      (nnheader-report 'nntp "Couldn't open connection to %s."
		       nntp-address))))

(defun nntp-send-command-and-decode (wait-for &rest strings)
  "Send STRINGS to server and wait until WAIT-FOR returns."
  (when (and (not nnheader-callback-function)
	     (not nntp-inhibit-output))
    (save-excursion
      (set-buffer nntp-server-buffer)
      (erase-buffer)))
  (let* ((command (mapconcat 'identity strings " "))
	 (process (nntp-find-connection nntp-server-buffer))
	 (buffer (and process (process-buffer process)))
	 (pos (and buffer (with-current-buffer buffer (point)))))
    (if process
	(prog1
	    (nntp-retrieve-data command
				nntp-address nntp-port-number
				nntp-server-buffer
				wait-for nnheader-callback-function t)
	  ;; If nothing to wait for, still remove possibly echo'ed commands
	  (unless wait-for
	    (nntp-accept-response)
	    (save-excursion
	  (set-buffer buffer)
	  (goto-char pos)
	  (if (looking-at (regexp-quote command))
	      (delete-region pos (progn (forward-line 1) (gnus-point-at-bol))))
	  )))
      (nnheader-report 'nntp "Couldn't open connection to %s."
		       nntp-address))))


(defun nntp-send-buffer (wait-for)
  "Send the current buffer to server and wait until WAIT-FOR returns."
  (when (and (not nnheader-callback-function)
	     (not nntp-inhibit-output))
    (save-excursion
      (set-buffer (nntp-find-connection-buffer nntp-server-buffer))
      (erase-buffer)))
  (nntp-encode-text)
  (mm-with-unibyte-current-buffer
    ;; Some encoded unicode text contains character 0x80-0x9f e.g. Euro.
    (process-send-region (nntp-find-connection nntp-server-buffer)
			 (point-min) (point-max)))
  (nntp-retrieve-data
   nil nntp-address nntp-port-number nntp-server-buffer
   wait-for nnheader-callback-function))



;;; Interface functions.

(nnoo-define-basics nntp)

(defsubst nntp-next-result-arrived-p ()
  (cond
   ;; A result that starts with a 2xx code is terminated by
   ;; a line with only a "." on it.
   ((eq (char-after) ?2)
    (if (re-search-forward "\n\\.\r?\n" nil t)
	t
      nil))
   ;; A result that starts with a 3xx or 4xx code is terminated
   ;; by a newline.
   ((looking-at "[34]")
    (if (search-forward "\n" nil t)
	t
      nil))
   ;; No result here.
   (t
    nil)))

(eval-when-compile
  (defvar nntp-with-open-group-internal nil)
  (defvar nntp-report-n nil))

(defmacro nntp-with-open-group (group server &optional connectionless &rest forms)
  "Protect against servers that don't like clients that keep idle connections opens.
The problem being that these servers may either close a connection or
simply ignore any further requests on a connection.  Closed
connections are not detected until accept-process-output has updated
the process-status.  Dropped connections are not detected until the
connection timeouts (which may be several minutes) or
nntp-connection-timeout has expired.  When these occur
nntp-with-open-group, opens a new connection then re-issues the NNTP
command whose response triggered the error."
  (when (and (listp connectionless)
	     (not (eq connectionless nil)))
    (setq forms (cons connectionless forms)
	  connectionless nil))
  `(letf ((nntp-report-n (symbol-function 'nntp-report))
	  ((symbol-function 'nntp-report) (symbol-function 'nntp-report-1))
	  (nntp-with-open-group-internal nil))
     (while (catch 'nntp-with-open-group-error
	      ;; Open the connection to the server
	      ;; NOTE: Existing connections are NOT tested.
	      (nntp-possibly-change-group ,group ,server ,connectionless)

	      (let ((timer
		     (and nntp-connection-timeout
			  (nnheader-run-at-time
			   nntp-connection-timeout nil
			   '(lambda ()
			      (let ((process (nntp-find-connection
					      nntp-server-buffer))
				    (buffer  (and process
						  (process-buffer process))))
				;; When I an able to identify the
				;; connection to the server AND I've
				;; received NO reponse for
				;; nntp-connection-timeout seconds.
				(when (and buffer (eq 0 (buffer-size buffer)))
				  ;; Close the connection.  Take no
				  ;; other action as the accept input
				  ;; code will handle the closed
				  ;; connection.
				  (nntp-kill-buffer buffer))))))))
		(unwind-protect
		    (setq nntp-with-open-group-internal
                          (condition-case nil
			      (progn ,@forms)
			    (quit
			     (nntp-close-server)
                             (signal 'quit nil))))
		  (when timer
		    (nnheader-cancel-timer timer)))
		nil))
       (setf (symbol-function 'nntp-report) nntp-report-n))
     nntp-with-open-group-internal))

(deffoo nntp-retrieve-headers (articles &optional group server fetch-old)
  "Retrieve the headers of ARTICLES."
  (nntp-with-open-group
   group server
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
             (articles articles)
             (count 0)
             (received 0)
             (last-point (point-min))
             (buf (nntp-find-connection-buffer nntp-server-buffer))
             (nntp-inhibit-erase t)
             article)
         ;; Send HEAD commands.
         (while (setq article (pop articles))
           (nntp-send-command
            nil
            "HEAD" (if (numberp article)
                       (int-to-string article)
                     ;; `articles' is either a list of article numbers
                     ;; or a list of article IDs.
                     article))
           (incf count)
           ;; Every 400 requests we have to read the stream in
           ;; order to avoid deadlocks.
           (when (or (null articles)    ;All requests have been sent.
                     (zerop (% count nntp-maximum-request)))
             (nntp-accept-response)
             (while (progn
                      (set-buffer buf)
                      (goto-char last-point)
                      ;; Count replies.
                      (while (nntp-next-result-arrived-p)
                        (setq last-point (point))
                        (incf received))
                      (< received count))
               ;; If number of headers is greater than 100, give
               ;;  informative messages.
               (and (numberp nntp-large-newsgroup)
                    (> number nntp-large-newsgroup)
                    (zerop (% received 20))
                    (nnheader-message 6 "NNTP: Receiving headers... %d%%"
                                      (/ (* received 100) number)))
               (nntp-accept-response))))
         (and (numberp nntp-large-newsgroup)
              (> number nntp-large-newsgroup)
              (nnheader-message 6 "NNTP: Receiving headers...done"))

         ;; Now all of replies are received.  Fold continuation lines.
         (nnheader-fold-continuation-lines)
         ;; Remove all "\r"'s.
         (nnheader-strip-cr)
         (copy-to-buffer nntp-server-buffer (point-min) (point-max))
         'headers)))))

(deffoo nntp-retrieve-groups (groups &optional server)
  "Retrieve group info on GROUPS."
  (nntp-with-open-group
   nil server
   (when (nntp-find-connection-buffer nntp-server-buffer)
     (catch 'done
       (save-excursion
         ;; Erase nntp-server-buffer before nntp-inhibit-erase.
         (set-buffer nntp-server-buffer)
         (erase-buffer)
         (set-buffer (nntp-find-connection-buffer nntp-server-buffer))
         ;; The first time this is run, this variable is `try'.  So we
         ;; try.
         (when (eq nntp-server-list-active-group 'try)
           (nntp-try-list-active (car groups)))
         (erase-buffer)
         (let ((count 0)
               (groups groups)
               (received 0)
               (last-point (point-min))
               (nntp-inhibit-erase t)
               (buf (nntp-find-connection-buffer nntp-server-buffer))
               (command (if nntp-server-list-active-group
                            "LIST ACTIVE" "GROUP")))
           (while groups
             ;; Timeout may have killed the buffer.
             (unless (gnus-buffer-live-p buf)
               (nnheader-report 'nntp "Connection to %s is closed." server)
               (throw 'done nil))
             ;; Send the command to the server.
             (nntp-send-command nil command (pop groups))
             (incf count)
             ;; Every 400 requests we have to read the stream in
             ;; order to avoid deadlocks.
             (when (or (null groups)    ;All requests have been sent.
                       (zerop (% count nntp-maximum-request)))
               (nntp-accept-response)
               (while (and (gnus-buffer-live-p buf)
                           (progn
                             ;; Search `blue moon' in this file for the
                             ;; reason why set-buffer here.
                             (set-buffer buf)
                             (goto-char last-point)
                             ;; Count replies.
                             (while (re-search-forward "^[0-9]" nil t)
                               (incf received))
                             (setq last-point (point))
                             (< received count)))
                 (nntp-accept-response))))

           ;; Wait for the reply from the final command.
           (unless (gnus-buffer-live-p buf)
             (nnheader-report 'nntp "Connection to %s is closed." server)
             (throw 'done nil))
           (set-buffer buf)
           (goto-char (point-max))
           (re-search-backward "^[0-9]" nil t)
           (when (looking-at "^[23]")
             (while (and (gnus-buffer-live-p buf)
                         (progn
                           (set-buffer buf)
                           (goto-char (point-max))
                           (if (not nntp-server-list-active-group)
                               (not (re-search-backward "\r?\n"
							(- (point) 3) t))
                             (not (re-search-backward "^\\.\r?\n"
                                                      (- (point) 4) t)))))
               (nntp-accept-response)))

           ;; Now all replies are received.  We remove CRs.
           (unless (gnus-buffer-live-p buf)
             (nnheader-report 'nntp "Connection to %s is closed." server)
             (throw 'done nil))
           (set-buffer buf)
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
             'active)))))))

(deffoo nntp-retrieve-articles (articles &optional group server)
  (nntp-with-open-group
    group server
   (save-excursion
     (let ((number (length articles))
           (articles articles)
           (count 0)
           (received 0)
           (last-point (point-min))
           (buf (nntp-find-connection-buffer nntp-server-buffer))
           (nntp-inhibit-erase t)
           (map (apply 'vector articles))
           (point 1)
           article)
       (set-buffer buf)
       (erase-buffer)
       ;; Send ARTICLE command.
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
                    (set-buffer buf)
                    (goto-char last-point)
                    ;; Count replies.
                    (while (nntp-next-result-arrived-p)
                      (aset map received (cons (aref map received) (point)))
                      (setq last-point (point))
                      (incf received))
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
            (nnheader-message 6 "NNTP: Receiving articles...done"))

       ;; Now we have all the responses.  We go through the results,
       ;; wash it and copy it over to the server buffer.
       (set-buffer nntp-server-buffer)
       (erase-buffer)
       (setq last-point (point-min))
       (mapcar
        (lambda (entry)
          (narrow-to-region
           (setq point (goto-char (point-max)))
           (progn
             (insert-buffer-substring buf last-point (cdr entry))
             (point-max)))
          (setq last-point (cdr entry))
          (nntp-decode-text)
          (widen)
          (cons (car entry) point))
        map)))))

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
  "Return the active info on GROUP (which can be a regexp)."
  (nntp-with-open-group
   nil server
   (nntp-send-command "^\\.*\r?\n" "LIST ACTIVE" group)))

(deffoo nntp-request-group-articles (group &optional server)
  "Return the list of existing articles in GROUP."
  (nntp-with-open-group
   nil server
   (nntp-send-command "^\\.*\r?\n" "LISTGROUP" group)))

(deffoo nntp-request-article (article &optional group server buffer command)
  (nntp-with-open-group
    group server
    (when (nntp-send-command-and-decode
           "\r?\n\\.\r?\n" "ARTICLE"
           (if (numberp article) (int-to-string article) article))
      (if (and buffer
               (not (equal buffer nntp-server-buffer)))
          (save-excursion
            (set-buffer nntp-server-buffer)
            (copy-to-buffer buffer (point-min) (point-max))
            (nntp-find-group-and-number group))
        (nntp-find-group-and-number group)))))

(deffoo nntp-request-head (article &optional group server)
  (nntp-with-open-group
   group server
   (when (nntp-send-command
          "\r?\n\\.\r?\n" "HEAD"
          (if (numberp article) (int-to-string article) article))
     (prog1
         (nntp-find-group-and-number group)
       (nntp-decode-text)))))

(deffoo nntp-request-body (article &optional group server)
  (nntp-with-open-group
   group server
   (nntp-send-command-and-decode
    "\r?\n\\.\r?\n" "BODY"
    (if (numberp article) (int-to-string article) article))))

(deffoo nntp-request-group (group &optional server dont-check)
  (nntp-with-open-group
    nil server
    (when (nntp-send-command "^[245].*\n" "GROUP" group)
      (let ((entry (nntp-find-connection-entry nntp-server-buffer)))
        (setcar (cddr entry) group)))))

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
  (let ((process (nntp-find-connection nntp-server-buffer)))
    (while process
      (when (memq (process-status process) '(open run))
	(ignore-errors
	  (nntp-send-string process "QUIT")
	  (unless (eq nntp-open-connection-function 'nntp-open-network-stream)
	    ;; Ok, this is evil, but when using telnet and stuff
	    ;; as the connection method, it's important that the
	    ;; QUIT command actually is sent out before we kill
	    ;; the process.
	    (sleep-for 1))))
      (nntp-kill-buffer (process-buffer process))
      (setq process (car (pop nntp-connection-alist))))
    (nnoo-close-server 'nntp)))

(deffoo nntp-request-close ()
  (let (process)
    (while (setq process (pop nntp-connection-list))
      (when (memq (process-status process) '(open run))
	(ignore-errors
	  (nntp-send-string process "QUIT")
	  (unless (eq nntp-open-connection-function 'nntp-open-network-stream)
	    ;; Ok, this is evil, but when using telnet and stuff
	    ;; as the connection method, it's important that the
	    ;; QUIT command actually is sent out before we kill
	    ;; the process.
	    (sleep-for 1))))
      (nntp-kill-buffer (process-buffer process)))))

(deffoo nntp-request-list (&optional server)
  (nntp-with-open-group
   nil server
   (nntp-send-command-and-decode "\r?\n\\.\r?\n" "LIST")))

(deffoo nntp-request-list-newsgroups (&optional server)
  (nntp-with-open-group
   nil server
   (nntp-send-command "\r?\n\\.\r?\n" "LIST NEWSGROUPS")))

(deffoo nntp-request-newgroups (date &optional server)
  (nntp-with-open-group
   nil server
   (save-excursion
     (set-buffer nntp-server-buffer)
     (let* ((time (date-to-time date))
            (ls (- (cadr time) (nth 8 (decode-time time)))))
       (cond ((< ls 0)
              (setcar time (1- (car time)))
              (setcar (cdr time) (+ ls 65536)))
             ((>= ls 65536)
              (setcar time (1+ (car time)))
              (setcar (cdr time) (- ls 65536)))
             (t
              (setcar (cdr time) ls)))
       (prog1
           (nntp-send-command
            "^\\.\r?\n" "NEWGROUPS"
            (format-time-string "%y%m%d %H%M%S" time)
            "GMT")
         (nntp-decode-text))))))

(deffoo nntp-request-post (&optional server)
  (nntp-with-open-group
   nil server
   (when (nntp-send-command "^[23].*\r?\n" "POST")
     (let ((response (with-current-buffer nntp-server-buffer
                       nntp-process-response))
           server-id)
       (when (and response
                  (string-match "^[23].*\\(<[^\t\n @<>]+@[^\t\n @<>]+>\\)"
                                response))
         (setq server-id (match-string 1 response))
         (narrow-to-region (goto-char (point-min))
                           (if (search-forward "\n\n" nil t)
                               (1- (point))
                             (point-max)))
         (unless (mail-fetch-field "Message-ID")
           (goto-char (point-min))
           (insert "Message-ID: " server-id "\n"))
         (widen))
       (run-hooks 'nntp-prepare-post-hook)
       (nntp-send-buffer "^[23].*\n")))))

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
  (nntp-send-command "^.*\n" "MODE READER"))

(defun nntp-send-authinfo (&optional send-if-force)
  "Send the AUTHINFO to the nntp server.
It will look in the \"~/.authinfo\" file for matching entries.  If
nothing suitable is found there, it will prompt for a user name
and a password.

If SEND-IF-FORCE, only send authinfo to the server if the
.authinfo file has the FORCE token."
  (let* ((list (gnus-parse-netrc nntp-authinfo-file))
	 (alist (gnus-netrc-machine list nntp-address "nntp"))
	 (force (gnus-netrc-get alist "force"))
	 (user (or (gnus-netrc-get alist "login") nntp-authinfo-user))
	 (passwd (gnus-netrc-get alist "password")))
    (when (or (not send-if-force)
	      force)
      (unless user
	(setq user (read-string (format "NNTP (%s) user name: " nntp-address))
	      nntp-authinfo-user user))
      (unless (member user '(nil ""))
	(nntp-send-command "^3.*\r?\n" "AUTHINFO USER" user)
	(when t				;???Should check if AUTHINFO succeeded
	  (nntp-send-command
	   "^2.*\r?\n" "AUTHINFO PASS"
	   (or passwd
	       nntp-authinfo-password
	       (setq nntp-authinfo-password
		     (read-passwd (format "NNTP (%s@%s) password: "
					  user nntp-address))))))))))

(defun nntp-send-nosy-authinfo ()
  "Send the AUTHINFO to the nntp server."
  (let ((user (read-string (format "NNTP (%s) user name: " nntp-address))))
    (unless (member user '(nil ""))
      (nntp-send-command "^3.*\r?\n" "AUTHINFO USER" user)
      (when t				;???Should check if AUTHINFO succeeded
	(nntp-send-command "^2.*\r?\n" "AUTHINFO PASS"
			   (read-passwd (format "NNTP (%s@%s) password: "
						user nntp-address)))))))

(defun nntp-send-authinfo-from-file ()
  "Send the AUTHINFO to the nntp server.

The authinfo login name is taken from the user's login name and the
password contained in '~/.nntp-authinfo'."
  (when (file-exists-p "~/.nntp-authinfo")
    (with-temp-buffer
      (insert-file-contents "~/.nntp-authinfo")
      (goto-char (point-min))
      (nntp-send-command "^3.*\r?\n" "AUTHINFO USER" (user-login-name))
      (nntp-send-command
       "^2.*\r?\n" "AUTHINFO PASS"
       (buffer-substring (point) (gnus-point-at-eol))))))

;;; Internal functions.

(defun nntp-handle-authinfo (process)
  "Take care of an authinfo response from the server."
  (let ((last nntp-last-command))
    (funcall nntp-authinfo-function)
    ;; We have to re-send the function that was interrupted by
    ;; the authinfo request.
    (save-excursion
      (set-buffer nntp-server-buffer)
      (erase-buffer))
    (nntp-send-string process last)))

(defun nntp-make-process-buffer (buffer)
  "Create a new, fresh buffer usable for nntp process connections."
  (save-excursion
    (set-buffer
     (generate-new-buffer
      (format " *server %s %s %s*"
	      nntp-address nntp-port-number
	      (gnus-buffer-exists-p buffer))))
    (mm-enable-multibyte)
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
	 (timer
	  (and nntp-connection-timeout
	       (nnheader-run-at-time
		nntp-connection-timeout nil
		`(lambda ()
		   (nntp-kill-buffer ,pbuffer)))))
	 (process
	  (condition-case ()
	      (let ((coding-system-for-read nntp-coding-system-for-read)
		    (coding-system-for-write nntp-coding-system-for-write))
		(funcall nntp-open-connection-function pbuffer))
	    (error nil)
	    (quit
	     (message "Quit opening connection")
	     (nntp-kill-buffer pbuffer)
	     (signal 'quit nil)
	     nil))))
    (when timer
      (nnheader-cancel-timer timer))
    (unless process
      (nntp-kill-buffer pbuffer))
    (when (and (buffer-name pbuffer)
	       process)
      (gnus-set-process-query-on-exit-flag process nil)
      (if (and (nntp-wait-for process "^2.*\n" buffer nil t)
	       (memq (process-status process) '(open run)))
	  (prog1
	      (caar (push (list process buffer nil) nntp-connection-alist))
	    (push process nntp-connection-list)
	    (save-excursion
	      (set-buffer pbuffer)
	      (nntp-read-server-type)
	      (erase-buffer)
	      (set-buffer nntp-server-buffer)
	      (let ((nnheader-callback-function nil))
		(run-hooks 'nntp-server-opened-hook)
		(nntp-send-authinfo t))))
	(nntp-kill-buffer (process-buffer process))
	nil))))

(defun nntp-open-network-stream (buffer)
  (open-network-stream "nntpd" buffer nntp-address nntp-port-number))

(eval-and-compile
  (autoload 'format-spec "format-spec")
  (autoload 'format-spec-make "format-spec")
  (autoload 'open-tls-stream "tls"))

(defun nntp-open-ssl-stream (buffer)
  (let* ((process-connection-type nil)
	 (proc (start-process "nntpd" buffer
			      shell-file-name
			      shell-command-switch
			      (format-spec nntp-ssl-program
					   (format-spec-make
					    ?s nntp-address
					    ?p nntp-port-number)))))
    (gnus-set-process-query-on-exit-flag proc nil)
    (save-excursion
      (set-buffer buffer)
      (let ((nntp-connection-alist (list proc buffer nil)))
	(nntp-wait-for-string "^\r*20[01]"))
      (beginning-of-line)
      (delete-region (point-min) (point))
      proc)))

(defun nntp-open-tls-stream (buffer)
  (let ((proc (open-tls-stream "nntpd" buffer nntp-address nntp-port-number)))
    (gnus-set-process-query-on-exit-flag proc nil)
    (save-excursion
      (set-buffer buffer)
      (let ((nntp-connection-alist (list proc buffer nil)))
	(nntp-wait-for-string "^\r*20[01]"))
      (beginning-of-line)
      (delete-region (point-min) (point))
      proc)))

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

(defun nntp-async-wait (process wait-for buffer decode callback)
  (save-excursion
    (set-buffer (process-buffer process))
    (unless nntp-inside-change-function
      (erase-buffer))
    (setq nntp-process-wait-for wait-for
	  nntp-process-to-buffer buffer
	  nntp-process-decode decode
	  nntp-process-callback callback
	  nntp-process-start-point (point-max))
    (setq after-change-functions '(nntp-after-change-function))
    (if nntp-async-needs-kluge
	(nntp-async-kluge process))))

(defun nntp-async-kluge (process)
  ;; emacs 20.3 bug: process output with encoding 'binary
  ;; doesn't trigger after-change-functions.
  (unless nntp-async-timer
    (setq nntp-async-timer
	  (nnheader-run-at-time 1 1 'nntp-async-timer-handler)))
  (add-to-list 'nntp-async-process-list process))

(defun nntp-async-timer-handler ()
  (mapcar
   (lambda (proc)
     (if (memq (process-status proc) '(open run))
	 (nntp-async-trigger proc)
       (nntp-async-stop proc)))
   nntp-async-process-list))

(defun nntp-async-stop (proc)
  (setq nntp-async-process-list (delq proc nntp-async-process-list))
  (when (and nntp-async-timer (not nntp-async-process-list))
    (nnheader-cancel-timer nntp-async-timer)
    (setq nntp-async-timer nil)))

(defun nntp-after-change-function (beg end len)
  (unwind-protect
      ;; we only care about insertions at eob
      (when (and (eq 0 len) (eq (point-max) end))
	(save-match-data
	  (let ((proc (get-buffer-process (current-buffer))))
	    (when proc
	      (nntp-async-trigger proc)))))
    ;; any throw from after-change-functions will leave it
    ;; set to nil.  so we reset it here, if necessary.
    (when quit-flag
      (setq after-change-functions '(nntp-after-change-function)))))

(defun nntp-async-trigger (process)
  (save-excursion
    (set-buffer (process-buffer process))
    (when nntp-process-callback
      ;; do we have an error message?
      (goto-char nntp-process-start-point)
      (if (memq (following-char) '(?4 ?5))
	  ;; wants credentials?
	  (if (looking-at "480")
	      (nntp-handle-authinfo process)
	    ;; report error message.
	    (nntp-snarf-error-message)
	    (nntp-do-callback nil))

	;; got what we expect?
	(goto-char (point-max))
	(when (re-search-backward
	       nntp-process-wait-for nntp-process-start-point t)
	  (let ((response (match-string 0)))
	    (with-current-buffer nntp-server-buffer
	      (setq nntp-process-response response)))
	  (nntp-async-stop process)
	  ;; convert it.
	  (when (gnus-buffer-exists-p nntp-process-to-buffer)
	    (let ((buf (current-buffer))
		  (start nntp-process-start-point)
		  (decode nntp-process-decode))
	      (save-excursion
		(set-buffer nntp-process-to-buffer)
		(goto-char (point-max))
		(save-restriction
		  (narrow-to-region (point) (point))
		  (insert-buffer-substring buf start)
		  (when decode
		    (nntp-decode-text))))))
	  ;; report it.
	  (goto-char (point-max))
	  (nntp-do-callback
	   (buffer-name (get-buffer nntp-process-to-buffer))))))))

(defun nntp-do-callback (arg)
  (let ((callback nntp-process-callback)
	(nntp-inside-change-function t))
    (setq nntp-process-callback nil)
    (funcall callback arg)))

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
    (let ((len (/ (buffer-size) 1024))
	  message-log-max)
      (unless (< len 10)
	(setq nntp-have-messaged t)
	(nnheader-message 7 "nntp read: %dk" len)))
    (nnheader-accept-process-output process)
    ;; accept-process-output may update status of process to indicate
    ;; that the server has closed the connection.  This MUST be
    ;; handled here as the buffer restored by the save-excursion may
    ;; be the process's former output buffer (i.e. now killed)
    (or (and process
	     (memq (process-status process) '(open run)))
        (nntp-report "Server closed connection"))))

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
      (cond ((not entry)
             (nntp-report "Server closed connection"))
            ((not (equal group (caddr entry)))
             (save-excursion
               (set-buffer (process-buffer (car entry)))
               (erase-buffer)
               (nntp-send-command "^[245].*\n" "GROUP" group)
               (setcar (cddr entry) group)
               (erase-buffer)
               (save-excursion
                 (set-buffer nntp-server-buffer)
                 (erase-buffer))))))))

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
    (while (looking-at "[1-5][0-9][0-9] .*\n")
      ;; For some unknown reason, there is more than one status line.
      (delete-region (point) (progn (forward-line 1) (point))))
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
    (insert ".\n")
    (goto-char (point-min))
    (while (not (eobp))
      (end-of-line)
      (delete-char 1)
      (insert nntp-end-of-line))))

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
    (when (looking-at "[1-5][0-9][0-9] .*\n")
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
	  last-point
	  in-process-buffer-p
	  (buf nntp-server-buffer)
	  (process-buffer (nntp-find-connection-buffer nntp-server-buffer))
	  first last status)
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

	(setq in-process-buffer-p (stringp nntp-server-xover))
        (nntp-send-xover-command first (setq last (car articles)))
        (setq articles (cdr articles))

	(when (and nntp-server-xover in-process-buffer-p)
	  ;; Don't count tried request.
	  (setq count (1+ count))

	  ;; Every 400 requests we have to read the stream in
	  ;; order to avoid deadlocks.
	  (when (or (null articles)	;All requests have been sent.
		    (= 1 (% count nntp-maximum-request)))

	    (nntp-accept-response)
	    ;; On some Emacs versions the preceding function has a
	    ;; tendency to change the buffer.  Perhaps.  It's quite
	    ;; difficult to reproduce, because it only seems to happen
	    ;; once in a blue moon.
	    (set-buffer process-buffer)
	    (while (progn
		     (goto-char (or last-point (point-min)))
		     ;; Count replies.
		     (while (re-search-forward "^\\([0-9][0-9][0-9]\\) .*\n"
					       nil t)
		       (incf received)
		       (setq status (match-string 1))
		       (if (string-match "^[45]" status)
			   (setq status 'error)
			 (setq status 'ok)))
		     (setq last-point (point))
		     (or (< received count)
			 (if (eq status 'error)
			     nil
			   ;; I haven't started reading the final response
			   (progn
			     (goto-char (point-max))
			     (forward-line -1)
			     (not (looking-at "^\\.\r?\n"))))))
	      ;; I haven't read the end of the final response
	      (nntp-accept-response)
	      (set-buffer process-buffer))))

        ;; Some nntp servers seem to have an extension to the XOVER
        ;; extension.  On these servers, requesting an article range
        ;; preceeding the active range does not return an error as
        ;; specified in the RFC.  What we instead get is the NOV entry
        ;; for the first available article.  Obviously, a client can
        ;; use that entry to avoid making unnecessary requests.  The
        ;; only problem is for a client that assumes that the response
        ;; will always be within the requested ranage.  For such a
        ;; client, we can get N copies of the same entry (one for each
        ;; XOVER command sent to the server).

        (when (<= count 1)
          (goto-char (point-min))
          (when (re-search-forward "^[0-9][0-9][0-9] .*\n\\([0-9]+\\)" nil t)
            (let ((low-limit (string-to-number
			      (buffer-substring (match-beginning 1)
						(match-end 1)))))
              (while (and articles (<= (car articles) low-limit))
                (setq articles (cdr articles))))))
        (set-buffer buf))

      (when nntp-server-xover
	(when in-process-buffer-p
	  (set-buffer buf)
	  (goto-char (point-max))
	  (insert-buffer-substring process-buffer)
	  (set-buffer process-buffer)
	  (erase-buffer)
	  (set-buffer buf))

	;; We remove any "." lines and status lines.
	(goto-char (point-min))
	(while (search-forward "\r" nil t)
	  (delete-char -1))
	(goto-char (point-min))
	(delete-matching-lines "^\\.$\\|^[1-5][0-9][0-9] ")
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
	  (nntp-send-command-nodelete nil nntp-server-xover range))
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

(defun nntp-find-group-and-number (&optional group)
  (save-excursion
    (save-restriction
      (set-buffer nntp-server-buffer)
      (narrow-to-region (goto-char (point-min))
			(or (search-forward "\n\n" nil t) (point-max)))
      (goto-char (point-min))
      ;; We first find the number by looking at the status line.
      (let ((number (and (looking-at "2[0-9][0-9] +\\([0-9]+\\) ")
			 (string-to-number
			  (buffer-substring (match-beginning 1)
					    (match-end 1)))))
	    newsgroups xref)
	(and number (zerop number) (setq number nil))
	(if number
	    ;; Then we find the group name.
	    (setq group
		  (cond
		   ;; If there is only one group in the Newsgroups
		   ;; header, then it seems quite likely that this
		   ;; article comes from that group, I'd say.
		   ((and (setq newsgroups
			       (mail-fetch-field "newsgroups"))
			 (not (string-match "," newsgroups)))
		    newsgroups)
		   ;; If there is more than one group in the
		   ;; Newsgroups header, then the Xref header should
		   ;; be filled out.  We hazard a guess that the group
		   ;; that has this article number in the Xref header
		   ;; is the one we are looking for.  This might very
		   ;; well be wrong if this article happens to have
		   ;; the same number in several groups, but that's
		   ;; life.
		   ((and (setq xref (mail-fetch-field "xref"))
			 number
			 (string-match
			  (format "\\([^ :]+\\):%d" number) xref))
		    (match-string 1 xref))
		   (t "")))
	  (cond
	   ((and (setq xref (mail-fetch-field "xref"))
		 (string-match
		  (if group
		      (concat "\\(" (regexp-quote group) "\\):\\([0-9]+\\)")
		    "\\([^ :]+\\):\\([0-9]+\\)")
		  xref))
	    (setq group (match-string 1 xref)
		  number (string-to-number (match-string 2 xref))))
	   ((and (setq newsgroups
		       (mail-fetch-field "newsgroups"))
		 (not (string-match "," newsgroups)))
	    (setq group newsgroups))
	   (group)
	   (t (setq group ""))))
	(when (string-match "\r" group)
	  (setq group (substring group 0 (match-beginning 0))))
	(cons group number)))))

(defun nntp-wait-for-string (regexp)
  "Wait until string arrives in the buffer."
  (let ((buf (current-buffer))
	proc)
    (goto-char (point-min))
    (while (and (setq proc (get-buffer-process buf))
		(memq (process-status proc) '(open run))
		(not (re-search-forward regexp nil t)))
      (accept-process-output proc)
      (set-buffer buf)
      (goto-char (point-min)))))


;; ==========================================================================
;; Obsolete nntp-open-* connection methods -- drv
;; ==========================================================================

(defvoo nntp-open-telnet-envuser nil
  "*If non-nil, telnet session (client and server both) will support the ENVIRON option and not prompt for login name.")

(defvoo nntp-telnet-shell-prompt "bash\\|\$ *\r?$\\|> *\r?"
  "*Regular expression to match the shell prompt on the remote machine.")

(defvoo nntp-rlogin-program "rsh"
  "*Program used to log in on remote machines.
The default is \"rsh\", but \"ssh\" is a popular alternative.")

(defvoo nntp-rlogin-parameters '("telnet" "-8" "${NNTPSERVER:=news}" "nntp")
  "*Parameters to `nntp-open-rlogin'.
That function may be used as `nntp-open-connection-function'.  In that
case, this list will be used as the parameter list given to rsh.")

(defvoo nntp-rlogin-user-name nil
  "*User name on remote system when using the rlogin connect method.")

(defvoo nntp-telnet-parameters
    '("exec" "telnet" "-8" "${NNTPSERVER:=news}" "nntp")
  "*Parameters to `nntp-open-telnet'.
That function may be used as `nntp-open-connection-function'.  In that
case, this list will be executed as a command after logging in
via telnet.")

(defvoo nntp-telnet-user-name nil
  "User name to log in via telnet with.")

(defvoo nntp-telnet-passwd nil
  "Password to use to log in via telnet with.")

(defun nntp-open-telnet (buffer)
  (save-excursion
    (set-buffer buffer)
    (erase-buffer)
    (let ((proc (apply
		 'start-process
		 "nntpd" buffer nntp-telnet-command nntp-telnet-switches))
	  (case-fold-search t))
      (when (memq (process-status proc) '(open run))
	(nntp-wait-for-string "^r?telnet")
	(process-send-string proc "set escape \^X\n")
	(cond
	 ((and nntp-open-telnet-envuser nntp-telnet-user-name)
	  (process-send-string proc (concat "open " "-l" nntp-telnet-user-name
					    nntp-address "\n")))
	 (t
	  (process-send-string proc (concat "open " nntp-address "\n"))))
	(cond
	 ((not nntp-open-telnet-envuser)
	  (nntp-wait-for-string "^\r*.?login:")
	  (process-send-string
	   proc (concat
		 (or nntp-telnet-user-name
		     (setq nntp-telnet-user-name (read-string "login: ")))
		 "\n"))))
	(nntp-wait-for-string "^\r*.?password:")
	(process-send-string
	 proc (concat
	       (or nntp-telnet-passwd
		   (setq nntp-telnet-passwd
			 (read-passwd "Password: ")))
	       "\n"))
	(nntp-wait-for-string nntp-telnet-shell-prompt)
	(process-send-string
	 proc (concat (mapconcat 'identity nntp-telnet-parameters " ") "\n"))
	(nntp-wait-for-string "^\r*20[01]")
	(beginning-of-line)
	(delete-region (point-min) (point))
	(process-send-string proc "\^]")
	(nntp-wait-for-string "^r?telnet")
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
		  (apply 'start-process
			 "nntpd" buffer nntp-rlogin-program
			 nntp-address "-l" nntp-rlogin-user-name
			 nntp-rlogin-parameters)
		(apply 'start-process
		       "nntpd" buffer nntp-rlogin-program nntp-address
		       nntp-rlogin-parameters))))
    (save-excursion
      (set-buffer buffer)
      (nntp-wait-for-string "^\r*20[01]")
      (beginning-of-line)
      (delete-region (point-min) (point))
      proc)))


;; ==========================================================================
;; Replacements for the nntp-open-* functions -- drv
;; ==========================================================================

(defun nntp-open-telnet-stream (buffer)
  "Open a nntp connection by telnet'ing the news server.

Please refer to the following variables to customize the connection:
- `nntp-pre-command',
- `nntp-telnet-command',
- `nntp-telnet-switches',
- `nntp-address',
- `nntp-port-number',
- `nntp-end-of-line'."
  (let ((command `(,nntp-telnet-command
		   ,@nntp-telnet-switches
		   ,nntp-address ,nntp-port-number))
	proc)
    (and nntp-pre-command
	 (push nntp-pre-command command))
    (setq proc (apply 'start-process "nntpd" buffer command))
    (save-excursion
      (set-buffer buffer)
      (nntp-wait-for-string "^\r*20[01]")
      (beginning-of-line)
      (delete-region (point-min) (point))
      proc)))

(defun nntp-open-via-rlogin-and-telnet (buffer)
  "Open a connection to an nntp server through an intermediate host.
First rlogin to the remote host, and then telnet the real news server
from there.

Please refer to the following variables to customize the connection:
- `nntp-pre-command',
- `nntp-via-rlogin-command',
- `nntp-via-rlogin-command-switches',
- `nntp-via-user-name',
- `nntp-via-address',
- `nntp-telnet-command',
- `nntp-telnet-switches',
- `nntp-address',
- `nntp-port-number',
- `nntp-end-of-line'."
  (let ((command `(,nntp-via-address
		   ,nntp-telnet-command
		   ,@nntp-telnet-switches))
	proc)
    (when nntp-via-user-name
      (setq command `("-l" ,nntp-via-user-name ,@command)))
    (when nntp-via-rlogin-command-switches
      (setq command (append nntp-via-rlogin-command-switches command)))
    (push nntp-via-rlogin-command command)
    (and nntp-pre-command
	 (push nntp-pre-command command))
    (setq proc (apply 'start-process "nntpd" buffer command))
    (save-excursion
      (set-buffer buffer)
      (nntp-wait-for-string "^r?telnet")
      (process-send-string proc (concat "open " nntp-address
					" " nntp-port-number "\n"))
      (nntp-wait-for-string "^\r*20[01]")
      (beginning-of-line)
      (delete-region (point-min) (point))
      (process-send-string proc "\^]")
      (nntp-wait-for-string "^r?telnet")
      (process-send-string proc "mode character\n")
      (accept-process-output proc 1)
      (sit-for 1)
      (goto-char (point-min))
      (forward-line 1)
      (delete-region (point) (point-max)))
    proc))

(defun nntp-open-via-telnet-and-telnet (buffer)
  "Open a connection to an nntp server through an intermediate host.
First telnet the remote host, and then telnet the real news server
from there.

Please refer to the following variables to customize the connection:
- `nntp-pre-command',
- `nntp-via-telnet-command',
- `nntp-via-telnet-switches',
- `nntp-via-address',
- `nntp-via-envuser',
- `nntp-via-user-name',
- `nntp-via-user-password',
- `nntp-via-shell-prompt',
- `nntp-telnet-command',
- `nntp-telnet-switches',
- `nntp-address',
- `nntp-port-number',
- `nntp-end-of-line'."
  (save-excursion
    (set-buffer buffer)
    (erase-buffer)
    (let ((command `(,nntp-via-telnet-command ,@nntp-via-telnet-switches))
	  (case-fold-search t)
	  proc)
      (and nntp-pre-command (push nntp-pre-command command))
      (setq proc (apply 'start-process "nntpd" buffer command))
      (when (memq (process-status proc) '(open run))
	(nntp-wait-for-string "^r?telnet")
	(process-send-string proc "set escape \^X\n")
	(cond
	 ((and nntp-via-envuser nntp-via-user-name)
	  (process-send-string proc (concat "open " "-l" nntp-via-user-name
					    nntp-via-address "\n")))
	 (t
	  (process-send-string proc (concat "open " nntp-via-address
					    "\n"))))
	(when (not nntp-via-envuser)
	  (nntp-wait-for-string "^\r*.?login:")
	  (process-send-string proc
			       (concat
				(or nntp-via-user-name
				    (setq nntp-via-user-name
					  (read-string "login: ")))
				"\n")))
	(nntp-wait-for-string "^\r*.?password:")
	(process-send-string proc
			     (concat
			      (or nntp-via-user-password
				  (setq nntp-via-user-password
					(read-passwd "Password: ")))
			      "\n"))
	(nntp-wait-for-string nntp-via-shell-prompt)
	(let ((real-telnet-command `("exec"
				     ,nntp-telnet-command
				     ,@nntp-telnet-switches
				     ,nntp-address
				     ,nntp-port-number)))
	  (process-send-string proc
			       (concat (mapconcat 'identity
						  real-telnet-command " ")
				       "\n")))
	(nntp-wait-for-string "^\r*20[01]")
	(beginning-of-line)
	(delete-region (point-min) (point))
	(process-send-string proc "\^]")
	(nntp-wait-for-string "^r?telnet")
	(process-send-string proc "mode character\n")
	(accept-process-output proc 1)
	(sit-for 1)
	(goto-char (point-min))
	(forward-line 1)
	(delete-region (point) (point-max)))
      proc)))

(provide 'nntp)

;;; arch-tag: 8655466a-b1b5-4929-9c45-7b1b2e767271
;;; nntp.el ends here
