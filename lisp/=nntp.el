;;; nntp.el --- NNTP (RFC977) Interface for GNU Emacs

;; Copyright (C) 1987, 1988, 1989, 1990, 1992, 1993 Free Software Foundation, Inc.

;; Author: Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
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

;; This implementation is tested on both 1.2a and 1.5 version of the
;; NNTP package.

;; Troubleshooting of NNTP
;;
;; (1) Select routine may signal an error or fall into infinite loop
;;  while waiting for the server response. In this case, you'd better
;;  not use byte-compiled codes but original source. If you still have
;;  a problems with it, set the variable `nntp-buggy-select' to t.
;;
;; (2) Emacs may hang up while retrieving headers since too many
;;  requests have been sent to the NNTP server without reading their
;;  replies. In this case, reduce the number of the requests sent to
;;  the server at one time by setting the variable
;;  `nntp-maximum-request' to a lower value.
;;
;; (3) If the TCP/IP stream (open-network-stream) is not supported by
;;  emacs, compile and install `tcp.el' and `tcp.c' which is an
;;  emulation program of the stream. If you modified `tcp.c' for your
;;  system, please send me the diffs. I'll include some of them in the
;;  future releases.

;;; Code:

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
hook, use the variable `nntp-server-name'.")

(defvar nntp-large-newsgroup 50
  "*The number of the articles which indicates a large newsgroup.
If the number of the articles is greater than the value, verbose
messages will be shown to indicate the current status.")


(defvar nntp-buggy-select (memq system-type '(fujitsu-uts))
  "*Non-nil if your select routine is buggy.
If the select routine signals error or fall into infinite loop while
waiting for the server response, the variable must be set to t.  In
case of Fujitsu UTS, it is set to t since `accept-process-output'
doesn't work properly.")

(defvar nntp-maximum-request 400
  "*The maximum number of the requests sent to the NNTP server at one time.
If Emacs hangs up while retrieving headers, set the variable to a
lower value.")

(defvar nntp-debug-read 10000
  "*Display '...' every 10Kbytes of a message being received if it is non-nil.
If it is a number, dots are displayed per the number.")


(defconst nntp-version "NNTP 3.12"
  "Version numbers of this version of NNTP.")

(defvar nntp-server-name nil
  "The name of the host running NNTP server.")

(defvar nntp-server-buffer nil
  "Buffer associated with NNTP server process.")

(defvar nntp-server-process nil
  "The NNTP server process.
You'd better not use this variable in NNTP front-end program but
instead use `nntp-server-buffer'.")

(defvar nntp-status-string nil
  "Save the server response message.
You'd better not use this variable in NNTP front-end program but
instead call function `nntp-status-message' to get status message.")

;;;
;;; Extended Command for retrieving many headers.
;;;
;; Retrieving lots of headers by sending command asynchronously.
;; Access functions to headers are defined as macro.

(defmacro nntp-header-number (header)
  "Return article number in HEADER."
  (` (aref (, header) 0)))

(defmacro nntp-set-header-number (header number)
  "Set article number of HEADER to NUMBER."
  (` (aset (, header) 0 (, number))))

(defmacro nntp-header-subject (header)
  "Return subject string in HEADER."
  (` (aref (, header) 1)))

(defmacro nntp-set-header-subject (header subject)
  "Set article subject of HEADER to SUBJECT."
  (` (aset (, header) 1 (, subject))))

(defmacro nntp-header-from (header)
  "Return author string in HEADER."
  (` (aref (, header) 2)))

(defmacro nntp-set-header-from (header from)
  "Set article author of HEADER to FROM."
  (` (aset (, header) 2 (, from))))

(defmacro nntp-header-xref (header)
  "Return xref string in HEADER."
  (` (aref (, header) 3)))

(defmacro nntp-set-header-xref (header xref)
  "Set article xref of HEADER to xref."
  (` (aset (, header) 3 (, xref))))

(defmacro nntp-header-lines (header)
  "Return lines in HEADER."
  (` (aref (, header) 4)))

(defmacro nntp-set-header-lines (header lines)
  "Set article lines of HEADER to LINES."
  (` (aset (, header) 4 (, lines))))

(defmacro nntp-header-date (header)
  "Return date in HEADER."
  (` (aref (, header) 5)))

(defmacro nntp-set-header-date (header date)
  "Set article date of HEADER to DATE."
  (` (aset (, header) 5 (, date))))

(defmacro nntp-header-id (header)
  "Return Id in HEADER."
  (` (aref (, header) 6)))

(defmacro nntp-set-header-id (header id)
  "Set article Id of HEADER to ID."
  (` (aset (, header) 6 (, id))))

(defmacro nntp-header-references (header)
  "Return references (or in-reply-to) in HEADER."
  (` (aref (, header) 7)))

(defmacro nntp-set-header-references (header ref)
  "Set article references of HEADER to REF."
  (` (aset (, header) 7 (, ref))))

(defun nntp-retrieve-headers (sequence)
  "Return list of article headers specified by SEQUENCE of article id.
The format of list is
 `([NUMBER SUBJECT FROM XREF LINES DATE MESSAGE-ID REFERENCES] ...)'.
If there is no References: field, In-Reply-To: field is used instead.
Reader macros for the vector are defined as `nntp-header-FIELD'.
Writer macros for the vector are defined as `nntp-set-header-FIELD'.
Newsgroup must be selected before calling this."
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (let ((number (length sequence))
	  (last-point (point-min))
	  (received 0)
	  (count 0)
	  (headers nil)			;Result list.
	  (article 0)
	  (subject nil)
	  (message-id)
	  (from nil)
	  (xref nil)
	  (lines 0)
	  (date nil)
	  (references nil))
      ;; Send HEAD command.
      (while sequence
	(nntp-send-strings-to-server "HEAD" (car sequence))
	(setq sequence (cdr sequence))
	(setq count (1+ count))
	;; Every 400 header requests we have to read stream in order
	;;  to avoid deadlock.
	(if (or (null sequence)		;All requests have been sent.
		(zerop (% count nntp-maximum-request)))
	    (progn
	      (accept-process-output)
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
		(nntp-accept-response))
	      ))
	)
      ;; Wait for text of last command.
      (goto-char (point-max))
      (re-search-backward "^[0-9]" nil t)
      (if (looking-at "^[23]")
	  (while (progn
		   (goto-char (- (point-max) 3))
		   (not (looking-at "^\\.\r$")))
	    (nntp-accept-response)))
      (and (numberp nntp-large-newsgroup)
	   (> number nntp-large-newsgroup)
	   (message "NNTP: Receiving headers... done"))
      ;; Now all of replies are received.
      (setq received number)
      ;; First, fold continuation lines.
      (goto-char (point-min))
      (while (re-search-forward "\\(\r?\n[ \t]+\\)+" nil t)
        (replace-match " " t t))
      ;;(delete-non-matching-lines
      ;; "^Subject:\\|^Xref:\\|^From:\\|^Lines:\\|^Date:\\|^References:\\|^[23]")
      (and (numberp nntp-large-newsgroup)
	   (> number nntp-large-newsgroup)
	   (message "NNTP: Parsing headers..."))
      ;; Then examines replies.
      (goto-char (point-min))
      (while (not (eobp))
	(cond ((looking-at "^[23][0-9][0-9][ \t]+\\([0-9]+\\)[ \t]+\\(<[^>]+>\\)")
	       (setq article
		     (string-to-int
		      (buffer-substring (match-beginning 1) (match-end 1))))
	       (setq message-id
		     (buffer-substring (match-beginning 2) (match-end 2)))
	       (forward-line 1)
	       ;; Set default value.
	       (setq subject nil)
	       (setq xref nil)
	       (setq from nil)
	       (setq lines 0)
	       (setq date nil)
	       (setq references nil)
	       ;; Thanks go to mly@AI.MIT.EDU (Richard Mlynarik)
	       (while (and (not (eobp))
			   (not (memq (following-char) '(?2 ?3))))
		 (if (looking-at "\\(From\\|Subject\\|Date\\|Lines\\|Xref\\|References\\|In-Reply-To\\):[ \t]+\\([^ \t\n]+.*\\)\r$")
		     (let ((s (buffer-substring
			       (match-beginning 2) (match-end 2)))
			   (c (char-after (match-beginning 0))))
		       ;; We don't have to worry about letter case.
		       (cond ((char-equal c ?F)	;From:
			      (setq from s))
			     ((char-equal c ?S)	;Subject:
			      (setq subject s))
			     ((char-equal c ?D)	;Date:
			      (setq date s))
			     ((char-equal c ?L)	;Lines:
			      (setq lines (string-to-int s)))
			     ((char-equal c ?X)	;Xref:
			      (setq xref s))
			     ((char-equal c ?R)	;References:
			      (setq references s))
			     ;; In-Reply-To: should be used only when
			     ;; there is no References: field.
			     ((and (char-equal c ?I) ;In-Reply-To:
				   (null references))
			      (setq references s))
			     )))
		 (forward-line 1))
	       ;; Finished to parse one header.
	       (if (null subject)
		   (setq subject "(None)"))
	       (if (null from)
		   (setq from "(Unknown User)"))
	       ;; Collect valid article only.
	       (and article
		    message-id
		    (setq headers
			  (cons (vector article subject from
					xref lines date
					message-id references) headers)))
	       )
	      (t (forward-line 1))
	      )
	(setq received (1- received))
	(and (numberp nntp-large-newsgroup)
	     (> number nntp-large-newsgroup)
	     (zerop (% received 20))
	     (message "NNTP: Parsing headers... %d%%"
		      (/ (* received 100) number)))
	)
      (and (numberp nntp-large-newsgroup)
	   (> number nntp-large-newsgroup)
	   (message "NNTP: Parsing headers... done"))
      (nreverse headers)
      )))


;;;
;;; Raw Interface to Network News Transfer Protocol (RFC977).
;;;

(defun nntp-open-server (host &optional service)
  "Open news server on HOST.
If HOST is nil, use value of environment variable `NNTPSERVER'.
If optional argument SERVICE is non-nil, open by the service name."
  (let ((host (or host (getenv "NNTPSERVER")))
	(status nil))
    (setq nntp-status-string "")
    (cond ((and host (nntp-open-server-internal host service))
	   (setq status (nntp-wait-for-response "^[23].*\r$"))
	   ;; Do check unexpected close of connection.
	   ;; Suggested by feldmark@hanako.stars.flab.fujitsu.junet.
	   (if status
	       (progn (set-process-sentinel nntp-server-process
					    'nntp-default-sentinel)
		      (nntp-send-command "^[25].*\r$" "MODE" "READER"))
	     ;; We have to close connection here, since function
	     ;;  `nntp-server-opened' may return incorrect status.
	     (nntp-close-server-internal)
	     ))
	  ((null host)
	   (setq nntp-status-string "NNTP server is not specified."))
	  )
    status
    ))

(defun nntp-close-server ()
  "Close news server."
  (unwind-protect
      (progn
	;; Un-set default sentinel function before closing connection.
	(and nntp-server-process
	     (eq 'nntp-default-sentinel
		 (process-sentinel nntp-server-process))
	     (set-process-sentinel nntp-server-process nil))
	;; We cannot send QUIT command unless the process is running.
	(if (nntp-server-opened)
	    (nntp-send-command nil "QUIT"))
	)
    (nntp-close-server-internal)
    ))

(fset 'nntp-request-quit (symbol-function 'nntp-close-server))

(defun nntp-server-opened ()
  "Return server process status, T or NIL.
If the stream is opened, return T, otherwise return NIL."
  (and nntp-server-process
       (memq (process-status nntp-server-process) '(open run))))

(defun nntp-status-message ()
  "Return server status response as string."
  (if (and nntp-status-string
	   ;; NNN MESSAGE
	   (string-match "[0-9][0-9][0-9][ \t]+\\([^\r]*\\).*$"
			 nntp-status-string))
      (substring nntp-status-string (match-beginning 1) (match-end 1))
    ;; Empty message if nothing.
    ""
    ))

(defun nntp-request-article (id)
  "Select article by message ID (or number)."
  (prog1
      ;; If NEmacs, end of message may look like: "\256\215" (".^M")
      (nntp-send-command "^\\.\r$" "ARTICLE" id)
    (nntp-decode-text)
    ))

(defun nntp-request-body (id)
  "Select article body by message ID (or number)."
  (prog1
      ;; If NEmacs, end of message may look like: "\256\215" (".^M")
      (nntp-send-command "^\\.\r$" "BODY" id)
    (nntp-decode-text)
    ))

(defun nntp-request-head (id)
  "Select article head by message ID (or number)."
  (prog1
      (nntp-send-command "^\\.\r$" "HEAD" id)
    (nntp-decode-text)
    ))

(defun nntp-request-stat (id)
  "Select article by message ID (or number)."
  (nntp-send-command "^[23].*\r$" "STAT" id))

(defun nntp-request-group (group)
  "Select news GROUP."
  ;; 1.2a NNTP's group command is buggy. "^M" (\r) is not appended to
  ;;  end of the status message.
  (nntp-send-command "^[23].*$" "GROUP" group))

(defun nntp-request-list ()
  "List active newsgroups."
  (prog1
      (nntp-send-command "^\\.\r$" "LIST")
    (nntp-decode-text)
    ))

(defun nntp-request-list-newsgroups ()
  "List newsgroups (defined in NNTP2)."
  (prog1
      (nntp-send-command "^\\.\r$" "LIST NEWSGROUPS")
    (nntp-decode-text)
    ))

(defun nntp-request-list-distributions ()
  "List distributions (defined in NNTP2)."
  (prog1
      (nntp-send-command "^\\.\r$" "LIST DISTRIBUTIONS")
    (nntp-decode-text)
    ))

(defun nntp-request-last ()
  "Set current article pointer to the previous article
in the current news group."
  (nntp-send-command "^[23].*\r$" "LAST"))

(defun nntp-request-next ()
  "Advance current article pointer."
  (nntp-send-command "^[23].*\r$" "NEXT"))

(defun nntp-request-post ()
  "Post a new news in current buffer."
  (if (nntp-send-command "^[23].*\r$" "POST")
      (progn
	(nntp-encode-text)
	(nntp-send-region-to-server (point-min) (point-max))
	;; 1.2a NNTP's post command is buggy. "^M" (\r) is not
	;;  appended to end of the status message.
	(nntp-wait-for-response "^[23].*$")
	)))

(defun nntp-default-sentinel (proc status)
  "Default sentinel function for NNTP server process."
  (if (and nntp-server-process
	   (not (nntp-server-opened)))
      (error "NNTP: Connection closed.")
    ))

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
    (if (not (bolp))
	(insert "\n"))
    ;; Delete status line.
    (goto-char (point-min))
    (delete-region (point) (progn (forward-line 1) (point)))
    ;; Delete `^M' at end of line.
    ;; (replace-regexp "\r$" "")
    (while (not (eobp))
      (end-of-line)
      (if (= (preceding-char) ?\r)
	  (delete-char -1))
      (forward-line 1)
      )
    ;; Delete `.' at end of buffer (end of text mark).
    (goto-char (point-max))
    (forward-line -1)			;(beginning-of-line)
    (if (looking-at "^\\.$")
	(delete-region (point) (progn (forward-line 1) (point))))
    ;; Replace `..' at beginning of line with `.'.
    (goto-char (point-min))
    ;; (replace-regexp "^\\.\\." ".")
    (while (search-forward "\n.." nil t)
      (delete-char -1))
    ))

(defun nntp-encode-text ()
  "Encode text in current buffer for NNTP transmission.
1. Insert `.' at beginning of line.
2. Insert `.' at end of buffer (end of text mark)."
  (save-excursion
    ;; Insert newline at end of buffer.
    (goto-char (point-max))
    (if (not (bolp))
	(insert "\n"))
    ;; Replace `.' at beginning of line with `..'.
    (goto-char (point-min))
    ;; (replace-regexp "^\\." "..")
    (while (search-forward "\n." nil t)
      (insert "."))
    ;; Insert `.' at end of buffer (end of text mark).
    (goto-char (point-max))
    (insert ".\r\n")
    ))


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
      t)
    ))

(defun nntp-wait-for-response (regexp)
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
	(cond ((looking-at "[23]")
	       (setq wait nil))
	      ((looking-at "[45]")
	       (setq status nil)
	       (setq wait nil))
	      (t (nntp-accept-response))
	      ))
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
		(nntp-accept-response)
		;;(if nntp-debug-read (message ""))
		))
	    ;; Remove "...".
	    (if (and nntp-debug-read (> dotnum 0))
		(message ""))
	    ;; Successfully received server response.
	    t
	    ))
      )))


;;;
;;; Low-Level Interface to NNTP Server.
;;; 

(defun nntp-send-strings-to-server (&rest strings)
  "Send list of STRINGS to news server as command and its arguments."
  (let ((cmd (car strings))
	(strings (cdr strings)))
    ;; Command and each argument must be separated by one or more spaces.
    (while strings
      (setq cmd (concat cmd " " (car strings)))
      (setq strings (cdr strings)))
    ;; Command line must be terminated by a CR-LF.
    (process-send-string nntp-server-process (concat cmd "\r\n"))
    ))

(defun nntp-send-region-to-server (begin end)
  "Send current buffer region (from BEGIN to END) to news server."
  (save-excursion
    ;; We have to work in the buffer associated with NNTP server
    ;;  process because of NEmacs hack.
    (copy-to-buffer nntp-server-buffer begin end)
    (set-buffer nntp-server-buffer)
    (process-send-region nntp-server-process (point-min) (point-max))
    ;; We cannot erase buffer, because reply may be received.
    (delete-region begin end)
    ))

(defun nntp-open-server-internal (host &optional service)
  "Open connection to news server on HOST by SERVICE (default is nntp)."
  (save-excursion
    ;; Use TCP/IP stream emulation package if needed.
    (or (fboundp 'open-network-stream)
	(require 'tcp))
    ;; Initialize communication buffer.
    (setq nntp-server-buffer (get-buffer-create " *nntpd*"))
    (set-buffer nntp-server-buffer)
    (buffer-flush-undo (current-buffer))
    (erase-buffer)
    (kill-all-local-variables)
    (setq case-fold-search t)		;Should ignore case.
    (setq nntp-server-process
	  (open-network-stream "nntpd" (current-buffer)
			       host (or service "nntp")))
    (setq nntp-server-name host)
    ;; It is possible to change kanji-fileio-code in this hook.
    (run-hooks 'nntp-server-hook)
    ;; Return the server process.
    nntp-server-process
    ))

(defun nntp-close-server-internal ()
  "Close connection to news server."
  (if nntp-server-process
      (delete-process nntp-server-process))
  (if nntp-server-buffer
      (kill-buffer nntp-server-buffer))
  (setq nntp-server-buffer nil)
  (setq nntp-server-process nil))

(defun nntp-accept-response ()
  "Read response of server.
It is well-known that the communication speed will be much improved by
defining this function as macro."
  ;; To deal with server process exiting before
  ;;  accept-process-output is called.
  ;; Suggested by Jason Venner <jason@violet.berkeley.edu>.
  ;; This is a copy of `nntp-default-sentinel'.
  (or (memq (process-status nntp-server-process) '(open run))
      (error "NNTP: Connection closed."))
  (if nntp-buggy-select
      (progn
	;; We cannot use `accept-process-output'.
	;; Fujitsu UTS requires messages during sleep-for. I don't know why.
	(message "NNTP: Reading...")
	(sleep-for 1)
	(message ""))
    (condition-case errorcode
	(accept-process-output nntp-server-process)
      (error
       (cond ((string-equal "select error: Invalid argument" (nth 1 errorcode))
	      ;; Ignore select error.
	      nil
	      )
	     (t
	      (signal (car errorcode) (cdr errorcode))))
       ))
    ))

(provide 'nntp)

;;; nntp.el ends here
