;;; server.el --- Lisp code for GNU Emacs running as server process

;; Copyright (C) 1986, 87, 92, 94, 95, 96, 97, 98, 99, 2000, 2001
;;	 Free Software Foundation, Inc.

;; Author: William Sommerfeld <wesommer@athena.mit.edu>
;; Maintainer: FSF
;; Keywords: processes

;; Changes by peck@sun.com and by rms.

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

;; This Lisp code is run in Emacs when it is to operate as
;; a server for other processes.

;; Load this library and do M-x server-edit to enable Emacs as a server.
;; Emacs runs the program ../arch-lib/emacsserver as a subprocess
;; for communication with clients.  If there are no client buffers to edit, 
;; server-edit acts like (switch-to-buffer (other-buffer))

;; When some other program runs "the editor" to edit a file,
;; "the editor" can be the Emacs client program ../lib-src/emacsclient.
;; This program transmits the file names to Emacs through
;; the server subprocess, and Emacs visits them and lets you edit them.

;; Note that any number of clients may dispatch files to emacs to be edited.

;; When you finish editing a Server buffer, again call server-edit
;; to mark that buffer as done for the client and switch to the next 
;; Server buffer.  When all the buffers for a client have been edited 
;; and exited with server-edit, the client "editor" will return
;; to the program that invoked it.  

;; Your editing commands and Emacs's display output go to and from
;; the terminal in the usual way.  Thus, server operation is possible
;; only when Emacs can talk to the terminal at the time you invoke
;; the client.  This is possible in four cases:

;; 1. On a window system, where Emacs runs in one window and the
;; program that wants to use "the editor" runs in another.

;; 2. On a multi-terminal system, where Emacs runs on one terminal and the
;; program that wants to use "the editor" runs on another.

;; 3. When the program that wants to use "the editor" is running
;; as a subprocess of Emacs.

;; 4. On a system with job control, when Emacs is suspended, the program
;; that wants to use "the editor" will stop and display
;; "Waiting for Emacs...".  It can then be suspended, and Emacs can be
;; brought into the foreground for editing.  When done editing, Emacs is
;; suspended again, and the client program is brought into the foreground.

;; The buffer local variable "server-buffer-clients" lists 
;; the clients who are waiting for this buffer to be edited.  
;; The global variable "server-clients" lists all the waiting clients,
;; and which files are yet to be edited for each.

;;; Code:

(defgroup server nil
  "Emacs running as a server process."
  :group 'external)

(defcustom server-program (expand-file-name "emacsserver" exec-directory)
  "*The program to use as the edit server."
  :group 'server
  :type 'string)

(defcustom server-visit-hook nil
  "*List of hooks to call when visiting a file for the Emacs server."
  :group 'server
  :type '(repeat function))

(defcustom server-switch-hook nil
  "*List of hooks to call when switching to a buffer for the Emacs server."
  :group 'server
  :type '(repeat function))

(defcustom server-done-hook nil
  "*List of hooks to call when done editing a buffer for the Emacs server."
  :group 'server
  :type '(repeat function))

(defvar server-process nil 
  "The current server process")

(defvar server-previous-string "")

(defvar server-clients nil
  "List of current server clients.
Each element is (CLIENTID BUFFERS...) where CLIENTID is a string
that can be given to the server process to identify a client.
When a buffer is marked as \"done\", it is removed from this list.")

(defvar server-buffer-clients nil
  "List of client ids for clients requesting editing of current buffer.")
(make-variable-buffer-local 'server-buffer-clients)
;; Changing major modes should not erase this local.
(put 'server-buffer-clients 'permanent-local t)

(defvar server-window nil
  "*The window to use for selecting Emacs server buffers.
If nil, use the selected window.
If it is a frame, use the frame's selected window.")

(defcustom server-temp-file-regexp "^/tmp/Re\\|/draft$"
  "*Regexp which should match filenames of temporary files
which are deleted and reused after each edit
by the programs that invoke the Emacs server."
  :group 'server
  :type 'regexp)

(defcustom server-kill-new-buffers t
  "*Whether to kill buffers when done with them.
If non-nil, kill a buffer unless it already existed before editing
it with Emacs server. If nil, kill only buffers as specified by
`server-temp-file-regexp'.
Please note that only buffers are killed that still have a client,
i.e. buffers visited which \"emacsclient --no-wait\" are never killed in
this way."
  :group 'server
  :type 'boolean
  :version "21.1")

(or (assq 'server-buffer-clients minor-mode-alist)
    (setq minor-mode-alist (cons '(server-buffer-clients " Server") minor-mode-alist)))

(defvar server-existing-buffer nil
  "Non-nil means a buffer existed before the Emacs server was asked visit it.
This means that the server should not kill the buffer when you say you
are done with it in the server.  This variable is local in each buffer
where it is set.")
(make-variable-buffer-local 'server-existing-buffer)

;; If a *server* buffer exists,
;; write STRING to it for logging purposes.
(defun server-log (string)
  (if (get-buffer "*server*")
      (save-excursion
	(set-buffer "*server*")
	(goto-char (point-max))
	(insert (current-time-string) " " string)
	(or (bolp) (newline)))))

(defun server-sentinel (proc msg)
  (cond ((eq (process-status proc) 'exit)
	 (server-log (message "Server subprocess exited")))
	((eq (process-status proc) 'signal)
	 (server-log (message "Server subprocess killed")))))

;;;###autoload
(defun server-start (&optional leave-dead)
  "Allow this Emacs process to be a server for client processes.
This starts a server communications subprocess through which
client \"editors\" can send your editing commands to this Emacs job.
To use the server, set up the program `emacsclient' in the
Emacs distribution as your standard \"editor\".

Prefix arg means just kill any existing server communications subprocess."
  (interactive "P")
  ;; kill it dead!
  (if server-process
      (progn
	(set-process-sentinel server-process nil)
	(condition-case () (delete-process server-process) (error nil))))
  ;; Delete the socket files made by previous server invocations.
  (let* ((sysname (system-name))
	 (dot-index (string-match "\\." sysname)))
    (condition-case ()
	(delete-file (format "~/.emacs-server-%s" sysname))
      (error nil))
    (condition-case ()
	(delete-file (format "/tmp/esrv%d-%s" (user-uid) sysname))
      (error nil))
    ;; In case the server file name was made with a domainless hostname,
    ;; try deleting that name too.
    (if dot-index
	(let ((shortname (substring sysname 0 dot-index)))
	  (condition-case ()
	      (delete-file (format "~/.emacs-server-%s" shortname))
	    (error nil))
	  (condition-case ()
	      (delete-file (format "/tmp/esrv%d-%s" (user-uid) shortname))
	    (error nil)))))
  ;; If this Emacs already had a server, clear out associated status.
  (while server-clients
    (let ((buffer (nth 1 (car server-clients))))
      (server-buffer-done buffer)))
  (if leave-dead
      nil
    (if server-process
	(server-log (message "Restarting server")))
    ;; Using a pty is wasteful, and the separate session causes
    ;; annoyance sometimes (some systems kill idle sessions).
    (let ((process-connection-type nil))
      (setq server-process (start-process "server" nil server-program)))
    (set-process-sentinel server-process 'server-sentinel)
    (set-process-filter server-process 'server-process-filter)
    ;; We must receive file names without being decoded.  Those are
    ;; decoded by server-process-filter accoding to
    ;; file-name-coding-system.
    (set-process-coding-system server-process 'raw-text 'raw-text)
    (process-kill-without-query server-process)))

;Process a request from the server to edit some files.
;Format of STRING is "Client: CLIENTID PATH PATH PATH... \n"
(defun server-process-filter (proc string)
  (server-log string)
  (setq string (concat server-previous-string string))
  ;; If the input is multiple lines,
  ;; process each line individually.
  (while (string-match "\n" string)
    (let ((request (substring string 0 (match-beginning 0)))
	  (coding-system (and default-enable-multibyte-characters
			      (or file-name-coding-system
				  default-file-name-coding-system)))
	  client nowait
	  (files nil)
	  (lineno 1)
	  (columnno 0))
      ;; Remove this line from STRING.
      (setq string (substring string (match-end 0)))	  
      (if (string-match "^Error: " request)
	  (message "Server error: %s" (substring request (match-end 0)))
	(if (string-match "^Client: " request)
	    (progn
	      (setq request (substring request (match-end 0)))
	      (setq client (list (substring request 0 (string-match " " request))))
	      (setq request (substring request (match-end 0)))
	      (while (string-match "[^ ]+ " request)
		(let ((arg
		       (substring request (match-beginning 0) (1- (match-end 0))))
		      (pos 0))
		  (setq request (substring request (match-end 0)))
		  (if (string-match "\\`-nowait" arg)
		      (setq nowait t)
		    (cond
    			;; ARG is a line number option.
		     ((string-match "\\`\\+[0-9]+\\'" arg)
		      (setq lineno (string-to-int (substring arg 1))))
		     ;; ARG is line number:column option. 
		     ((string-match "\\`+\\([0-9]+\\):\\([0-9]+\\)\\'" arg)
		      (setq lineno (string-to-int (match-string 1 arg))
			    columnno (string-to-int (match-string 2 arg))))
		     (t
		      ;; ARG is a file name.
		      ;; Collapse multiple slashes to single slashes.
		      (setq arg (command-line-normalize-file-name arg))
		      ;; Undo the quoting that emacsclient does
		      ;; for certain special characters.
		      (while (string-match "&." arg pos)
			(setq pos (1+ (match-beginning 0)))
			(let ((nextchar (aref arg pos)))
			  (cond ((= nextchar ?&)
				 (setq arg (replace-match "&" t t arg)))
				((= nextchar ?-)
				 (setq arg (replace-match "-" t t arg)))
				(t
				 (setq arg (replace-match " " t t arg))))))
		      ;; Now decode the file name if necessary.
		      (if coding-system
			  (setq arg (decode-coding-string arg coding-system)))
		      (setq files
			    (cons (list arg lineno columnno)
				  files))
		      (setq lineno 1)
		      (setq columnno 0))))))
	      (run-hooks 'pre-command-hook)
	      (server-visit-files files client nowait)
	      (run-hooks 'post-command-hook)
	      ;; CLIENT is now a list (CLIENTNUM BUFFERS...)
	      (if (null (cdr client))
		  ;; This client is empty; get rid of it immediately.
		  (progn
		    (send-string server-process 
				 (format "Close: %s Done\n" (car client)))
		    (server-log (format "Close empty client: %s Done\n" (car client))))
		;; We visited some buffer for this client.
		(or nowait
		    (setq server-clients (cons client server-clients)))
		(server-switch-buffer (nth 1 client))
		(run-hooks 'server-switch-hook)
		(message (substitute-command-keys
			  "When done with a buffer, type \\[server-edit]"))))))))
  ;; Save for later any partial line that remains.
  (setq server-previous-string string))

(defun server-visit-files (files client &optional nowait)
  "Finds FILES and returns the list CLIENT with the buffers nconc'd.
FILES is an alist whose elements are (FILENAME LINENUMBER COLUMNNUMBER).
NOWAIT non-nil means this client is not waiting for the results,
so don't mark these buffers specially, just visit them normally."
  ;; Bind last-nonmenu-event to force use of keyboard, not mouse, for queries.
  (let (client-record (last-nonmenu-event t) (obuf (current-buffer)))
    ;; Restore the current buffer afterward, but not using save-excursion,
    ;; because we don't want to save point in this buffer
    ;; if it happens to be one of those specified by the server.
    (unwind-protect
	(while files
	  ;; If there is an existing buffer modified or the file is
	  ;; modified, revert it.  If there is an existing buffer with
	  ;; deleted file, offer to write it.
	  (let* ((filen (car (car files)))
		 (obuf (get-file-buffer filen)))
	    (push filen file-name-history)
	    (if (and obuf (set-buffer obuf))
		(progn
		  (cond ((file-exists-p filen)
			 (if (or (not (verify-visited-file-modtime obuf))
				 (buffer-modified-p obuf))
			     (revert-buffer t nil)))
			(t
			 (if (y-or-n-p
			      (concat "File no longer exists: "
				      filen
				      ", write buffer to file? "))
			     (write-file filen))))
		  (setq server-existing-buffer t)
		  (goto-line (nth 1 (car files))))
	      (set-buffer (find-file-noselect filen))
	      (goto-line (nth 1 (car files)))
	      (let ((column-number (nth 2 (car files))))
		(when (> column-number 0)
		  (move-to-column (1- column-number))))
	      (run-hooks 'server-visit-hook)))
	  (if (not nowait)
	      (setq server-buffer-clients
		    (cons (car client) server-buffer-clients)))
	  (setq client-record (cons (current-buffer) client-record))
	  (setq files (cdr files)))
      (set-buffer obuf))
    (nconc client client-record)))

(defun server-buffer-done (buffer &optional for-killing)
  "Mark BUFFER as \"done\" for its client(s).
This buries the buffer, then returns a list of the form (NEXT-BUFFER KILLED).
NEXT-BUFFER is another server buffer, as a suggestion for what to select next,
or nil.  KILLED is t if we killed BUFFER
\(typically, because it was visiting a temp file)."
  (let ((running (eq (process-status server-process) 'run))
	(next-buffer nil)
	(killed nil)
	(first t)
	(old-clients server-clients))
    (while old-clients
      (let ((client (car old-clients)))
	(or next-buffer 
	    (setq next-buffer (nth 1 (memq buffer client))))
	(delq buffer client)
	;; Delete all dead buffers from CLIENT.
	(let ((tail client))
	  (while tail
	    (and (bufferp (car tail))
		 (null (buffer-name (car tail)))
		 (delq (car tail) client))
	    (setq tail (cdr tail))))
	;; If client now has no pending buffers,
	;; tell it that it is done, and forget it entirely.
	(if (cdr client) nil
	  (if running
	      (progn
		;; Don't send emacsserver two commands in close succession.
		;; It cannot handle that.
		(or first (sit-for 1))
		(setq first nil)
		(send-string server-process 
			     (format "Close: %s Done\n" (car client)))
		(server-log (format "Close: %s Done\n" (car client)))))
	  (setq server-clients (delq client server-clients))))
      (setq old-clients (cdr old-clients)))
    (if (and (bufferp buffer) (buffer-name buffer))
	;; We may or may not kill this buffer;
	;; if we do, do not call server-buffer-done recursively
	;; from kill-buffer-hook.
	(let ((server-kill-buffer-running t))
	  (save-excursion
	    (set-buffer buffer)
	    (setq server-buffer-clients nil)
	    (run-hooks 'server-done-hook))
	  ;; Notice whether server-done-hook killed the buffer.
	  (if (null (buffer-name buffer))
	      (setq killed t)
	    ;; Don't bother killing or burying the buffer
	    ;; when we are called from kill-buffer.
	    (unless for-killing
	      (when (and (not killed)
			 server-kill-new-buffers
			 (with-current-buffer buffer
			   (not server-existing-buffer)))
		(setq killed t)
		(bury-buffer buffer)
		(kill-buffer buffer))
	      (unless killed
		(if (server-temp-file-p buffer)
		    (progn
		      (kill-buffer buffer)
		      (setq killed t))
		  (bury-buffer buffer)))))))
    (list next-buffer killed)))

(defun server-temp-file-p (buffer)
  "Return non-nil if BUFFER contains a file considered temporary.
These are files whose names suggest they are repeatedly
reused to pass information to another program.

The variable `server-temp-file-regexp' controls which filenames
are considered temporary."
  (and (buffer-file-name buffer)
       (string-match server-temp-file-regexp (buffer-file-name buffer))))

(defun server-done ()
  "Offer to save current buffer, mark it as \"done\" for clients.
This kills or buries the buffer, then returns a list
of the form (NEXT-BUFFER KILLED).  NEXT-BUFFER is another server buffer,
as a suggestion for what to select next, or nil.
KILLED is t if we killed BUFFER, which happens if it was created
specifically for the clients and did not exist before their request for it."
  (let ((buffer (current-buffer)))
    (if server-buffer-clients
	(progn
 	  (if (server-temp-file-p buffer)
	      ;; For a temp file, save, and do make a non-numeric backup
	      ;; (unless make-backup-files is nil).
	      (let ((version-control nil)
		    (buffer-backed-up nil))
		(save-buffer))
	    (if (and (buffer-modified-p)
		     buffer-file-name
		     (y-or-n-p (concat "Save file " buffer-file-name "? ")))
		(save-buffer buffer)))
	  (server-buffer-done buffer)))))

;; Ask before killing a server buffer.
;; It was suggested to release its client instead,
;; but I think that is dangerous--the client would proceed
;; using whatever is on disk in that file. -- rms.
(defun server-kill-buffer-query-function ()
  (or (not server-buffer-clients)
      (yes-or-no-p (format "Buffer `%s' still has clients; kill it? "
			   (buffer-name (current-buffer))))))

(add-hook 'kill-buffer-query-functions
 	  'server-kill-buffer-query-function)

(defun server-kill-emacs-query-function ()
  (let (live-client
	(tail server-clients))
    ;; See if any clients have any buffers that are still alive.
    (while tail
      (if (memq t (mapcar 'stringp (mapcar 'buffer-name (cdr (car tail)))))
	  (setq live-client t))
      (setq tail (cdr tail)))
    (or (not live-client)
	(yes-or-no-p "Server buffers still have clients; exit anyway? "))))

(add-hook 'kill-emacs-query-functions 'server-kill-emacs-query-function)

(defvar server-kill-buffer-running nil
  "Non-nil while `server-kill-buffer' or `server-buffer-done' is running.")

;; When a buffer is killed, inform the clients.
(add-hook 'kill-buffer-hook 'server-kill-buffer)
(defun server-kill-buffer ()
  ;; Prevent infinite recursion if user has made server-done-hook
  ;; call kill-buffer.
  (or server-kill-buffer-running
      (and server-buffer-clients
	   (let ((server-kill-buffer-running t))
	     (when server-process
	       (server-buffer-done (current-buffer) t))))))

(defun server-edit (&optional arg)
  "Switch to next server editing buffer; say \"Done\" for current buffer.
If a server buffer is current, it is marked \"done\" and optionally saved.
The buffer is also killed if it did not exist before the clients asked for it.
When all of a client's buffers are marked as \"done\", the client is notified.

Temporary files such as MH <draft> files are always saved and backed up,
no questions asked.  (The variable `make-backup-files', if nil, still
inhibits a backup; you can set it locally in a particular buffer to
prevent a backup for it.)  The variable `server-temp-file-regexp' controls
which filenames are considered temporary.

If invoked with a prefix argument, or if there is no server process running, 
starts server process and that is all.  Invoked by \\[server-edit]."
  (interactive "P")
  (if (or arg
	  (not server-process)
	  (memq (process-status server-process) '(signal exit)))
      (server-start nil)
    (apply 'server-switch-buffer (server-done))))

(defun server-switch-buffer (&optional next-buffer killed-one)
  "Switch to another buffer, preferably one that has a client.
Arg NEXT-BUFFER is a suggestion; if it is a live buffer, use it."
  ;; KILLED-ONE is t in a recursive call
  ;; if we have already killed one temp-file server buffer.
  ;; This means we should avoid the final "switch to some other buffer"
  ;; since we've already effectively done that.
  (cond ((and (windowp server-window)
	      (window-live-p server-window))
	 (select-window server-window))
	((framep server-window)
	 (if (not (frame-live-p server-window))
	     (setq server-window (make-frame)))
	 (select-window (frame-selected-window server-window))))
  (if (window-minibuffer-p (selected-window))
      (select-window (next-window nil 'nomini 0)))
  ;; Move to a non-dedicated window, if we have one.
  (when (window-dedicated-p (selected-window))
    (select-window (get-window-with-predicate
		    (lambda (w) (not (window-dedicated-p w)))
		    'nomini 'visible (selected-window))))
  (set-window-dedicated-p (selected-window) nil)
  (if next-buffer
      (if (and (bufferp next-buffer)
	       (buffer-name next-buffer))
	  (switch-to-buffer next-buffer)
	;; If NEXT-BUFFER is a dead buffer,
	;; remove the server records for it
	;; and try the next surviving server buffer.
	(apply 'server-switch-buffer
	       (server-buffer-done next-buffer)))
    (if server-clients
	(server-switch-buffer (nth 1 (car server-clients)) killed-one)
      (if (not killed-one)
	  (switch-to-buffer (other-buffer))))))

(global-set-key "\C-x#" 'server-edit)

(provide 'server)

;;; server.el ends here
