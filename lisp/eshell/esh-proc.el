;;; esh-proc.el --- process management  -*- lexical-binding:t -*-

;; Copyright (C) 1999-2024 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'esh-io)

(defgroup eshell-proc nil
  "When Eshell invokes external commands, it always does so
asynchronously, so that Emacs isn't tied up waiting for the process to
finish."
  :tag "Process management"
  :group 'eshell)

;;; User Variables:

(defcustom eshell-proc-load-hook nil
  "A hook that gets run when `eshell-proc' is loaded."
  :version "24.1"			; removed eshell-proc-initialize
  :type 'hook)

(defcustom eshell-process-wait-seconds 0
  "The number of seconds to delay waiting for a synchronous process."
  :type 'integer)

(defcustom eshell-process-wait-milliseconds 50
  "The number of milliseconds to delay waiting for a synchronous process."
  :type 'integer)

(defcustom eshell-done-messages-in-minibuffer t
  "If non-nil, subjob \"Done\" messages will display in minibuffer."
  :type 'boolean)

(defcustom eshell-delete-exited-processes t
  "If nil, process entries will stick around until `jobs' is run.
This variable sets the buffer-local value of `delete-exited-processes'
in Eshell buffers.

This variable causes Eshell to mimic the behavior of bash when set to
nil.  It allows the user to view the exit status of a completed subjob
\(process) at their leisure, because the process entry remains in
memory until the user examines it using \\[list-processes].

Otherwise, if `eshell-done-messages-in-minibuffer' is nil, and this
variable is set to t, the only indication the user will have that a
subjob is done is that it will no longer appear in the
\\[list-processes\\] display.

Note that Eshell will have to be restarted for a change in this
variable's value to take effect."
  :type 'boolean)

(defcustom eshell-reset-signals
  "^\\(interrupt\\|killed\\|quit\\|stopped\\)"
  "If a termination signal matches this regexp, the terminal will be reset."
  :type 'regexp)

(defcustom eshell-exec-hook nil
  "Called each time a process is exec'd by `eshell-gather-process-output'.
It is passed one argument, which is the process that was just started.
It is useful for things that must be done each time a process is
executed in an eshell mode buffer (e.g., `set-process-query-on-exit-flag').
In contrast, `eshell-mode-hook' is only executed once, when the buffer
is created."
  :type 'hook)

(defcustom eshell-kill-hook nil
  "Called when a process run by `eshell-gather-process-output' has ended.
It is passed two arguments: the process that was just ended, and the
termination status (as a string).  Note that the first argument may be
nil, in which case the user attempted to send a signal, but there was
no relevant process.  This can be used for displaying help
information, for example."
  :version "24.1"			; removed eshell-reset-after-proc
  :type 'hook)

;;; Internal Variables:

(defvar eshell-current-subjob-p nil)

(defvar eshell-process-list nil
  "A list of the current status of subprocesses.
Each element has the form (PROC . SUBJOB-P), where PROC is the
process object and SUBJOB-P is non-nil if the process is a
subjob.

To add or remove elements of this list, see
`eshell-record-process-object' and `eshell-remove-process-entry'.")

(declare-function eshell-send-eof-to-process "esh-mode")
(declare-function eshell-tail-process "esh-cmd")

(defvar-keymap eshell-proc-mode-map
  "C-c M-i"  #'eshell-insert-process
  "C-c C-c"  #'eshell-interrupt-process
  "C-c C-k"  #'eshell-kill-process
  "C-c C-d"  #'eshell-send-eof-to-process
  "C-c C-s"  #'list-processes
  "C-c C-\\" #'eshell-quit-process)

;;; Functions:

(defun eshell-kill-process-function (proc status)
  "Function run when killing a process.
Runs `eshell-reset-after-proc' and `eshell-kill-hook', passing arguments
PROC and STATUS to functions on the latter."
  ;; Was there till 24.1, but it is not optional.
  (remove-hook 'eshell-kill-hook #'eshell-reset-after-proc)
  ;; Only reset the prompt if this process is running interactively.
  (when (eq proc (eshell-tail-process))
    (eshell-reset-after-proc status))
  (run-hook-with-args 'eshell-kill-hook proc status))

(define-minor-mode eshell-proc-mode
  "Minor mode for the proc eshell module.

\\{eshell-proc-mode-map}"
  :keymap eshell-proc-mode-map)

(defun eshell-proc-initialize ()    ;Called from `eshell-mode' via intern-soft!
  "Initialize the process handling code."
  (make-local-variable 'eshell-process-list)
  (eshell-proc-mode))

(defun eshell-reset-after-proc (status)
  "Reset the command input location after a process terminates.
The signals which will cause this to happen are matched by
`eshell-reset-signals'."
  (when (and (stringp status)
	     (string-match eshell-reset-signals status))
    (require 'esh-mode)
    (declare-function eshell-reset "esh-mode" (&optional no-hooks))
    (eshell-reset)))

(defun eshell-wait-for-process (&rest procs)
  "Wait until PROC has successfully completed."
  (while procs
    (let ((proc (car procs)))
      (when (eshell-processp proc)
	;; NYI: If the process gets stopped here, that's bad.
	(while (assq proc eshell-process-list)
	  (if (input-pending-p)
	      (discard-input))
	  (sit-for eshell-process-wait-seconds
		   eshell-process-wait-milliseconds))))
    (setq procs (cdr procs))))

(defalias 'eshell/wait #'eshell-wait-for-process)

(defun eshell/jobs (&rest _args)
  "List processes, if there are any."
  (and (fboundp 'process-list)
       (process-list)
       (list-processes)))

(defun eshell/kill (&rest args)
  "Kill processes.
Usage: kill [-<signal>] <pid>|<process> ...
Accepts PIDs and process objects.  Optionally accept signals
and signal names."
  ;; If the first argument starts with a dash, treat it as the signal
  ;; specifier.
  (let ((signum 'SIGINT))
    (let ((arg (car args))
          (case-fold-search nil))
      (when (stringp arg)
        (cond
         ((string-match "\\`-[[:digit:]]+\\'" arg)
          (setq signum (abs (string-to-number arg))))
         ((string-match "\\`-\\([[:upper:]]+\\|[[:lower:]]+\\)\\'" arg)
          (setq signum (intern (substring arg 1)))))
        (setq args (cdr args))))
    (while args
      (let ((arg (if (eshell-processp (car args))
                     (process-id (car args))
                   (string-to-number (car args)))))
        (when arg
          (cond
           ((null arg)
            (error "kill: null pid.  Process may actually be a network connection."))
           ((not (numberp arg))
            (error "kill: invalid argument type: %s" (type-of arg)))
           ((and (numberp arg)
                 (<= arg 0))
            (error "kill: bad pid: %d" arg))
           (t
            (signal-process arg signum)))))
      (setq args (cdr args))))
  nil)

(put 'eshell/kill 'eshell-no-numeric-conversions t)

(defun eshell-read-process-name (prompt)
  "Read the name of a process from the minibuffer, using completion.
The prompt will be set to PROMPT."
  (completing-read prompt
		   (mapcar
                    (lambda (proc)
                      (cons (process-name proc) t))
		    (process-list))
                   nil t))

(defun eshell-insert-process (process)
  "Insert the name of PROCESS into the current buffer at point."
  (interactive
   (list (get-process
	  (eshell-read-process-name "Name of process: "))))
  (insert-and-inherit "#<process " (process-name process) ">"))

(defsubst eshell-record-process-object (object)
  "Record OBJECT as now running."
  (when (and (eshell-processp object)
	     eshell-current-subjob-p)
    (require 'esh-mode)
    (declare-function eshell-interactive-print "esh-mode" (string))
    (eshell-interactive-print
     (format "[%s] %d\n" (process-name object) (process-id object))))
  (push (cons object eshell-current-subjob-p) eshell-process-list))

(defun eshell-remove-process-entry (entry)
  "Record the process ENTRY as fully completed."
  (if (and (eshell-processp (car entry))
	   (cdr entry)
	   eshell-done-messages-in-minibuffer)
      (message "[%s]+ Done %s" (process-name (car entry))
	       (process-command (car entry))))
  (setq eshell-process-list
	(delq entry eshell-process-list)))

(defun eshell-record-process-properties (process &optional index)
  "Record Eshell bookkeeping properties for PROCESS.
`eshell-insertion-filter' and `eshell-sentinel' will use these to
do their jobs.

INDEX is the index of the output handle to use for writing; if
nil, write to `eshell-output-handle'."
  (process-put process :eshell-handles eshell-current-handles)
  (process-put process :eshell-handle-index (or index eshell-output-handle))
  (process-put process :eshell-pending nil)
  (process-put process :eshell-busy nil))

(defvar eshell-scratch-buffer " *eshell-scratch*"
  "Scratch buffer for holding Eshell's input/output.")
(defvar eshell-last-sync-output-start nil
  "A marker that tracks the beginning of output of the last subprocess.
Used only on systems which do not support async subprocesses.")

(defvar tramp-remote-path)

(defun eshell-gather-process-output (command args)
  "Gather the output from COMMAND + ARGS."
  (require 'esh-var)
  (declare-function eshell-environment-variables "esh-var" ())
  (unless (and (file-executable-p command)
	       (file-regular-p (file-truename command)))
    (error "%s: not an executable file" command))
  (let* ((real-path (getenv "PATH"))
         (tramp-remote-path (bound-and-true-p tramp-remote-path))
         (delete-exited-processes
	  (if eshell-current-subjob-p
	      eshell-delete-exited-processes
	    delete-exited-processes))
	 (process-environment (eshell-environment-variables))
         (coding-system-for-read coding-system-for-read)
         (coding-system-for-write coding-system-for-write)
	 proc stderr-proc decoding encoding changed)
    ;; HACK: We want to supply our subprocess with the all the
    ;; environment variables we've set in Eshell.  However, supplying
    ;; a remote PATH this way can break Tramp, which needs the *local*
    ;; PATH for calling "ssh", etc.  Instead, set the local path in
    ;; our `process-environment' and pass the remote PATH via
    ;; `tramp-remote-path'.  (If we handle this some better way in the
    ;; future, remember to remove `tramp-remote-path' above, too.)
    (when (file-remote-p default-directory)
      (push (concat "PATH=" real-path) process-environment)
      (setq tramp-remote-path (eshell-get-path t)))
    ;; MS-Windows needs special setting of encoding/decoding, because
    ;; (a) non-ASCII text in command-line arguments needs to be
    ;; encoded in the system's codepage; and (b) because many Windows
    ;; programs will always interpret any non-ASCII input as encoded
    ;; in the system codepage.
    (when (eq system-type 'windows-nt)
      (or coding-system-for-read        ; Honor manual decoding settings
          (setq coding-system-for-read
                (coding-system-change-eol-conversion locale-coding-system
                                                     'dos)))
      (or coding-system-for-write       ; Honor manual encoding settings
          (setq coding-system-for-write
                (coding-system-change-eol-conversion locale-coding-system
                                                     'unix))))
    (cond
     ((fboundp 'make-process)
      (unless (or ;; FIXME: It's not currently possible to use a
                  ;; stderr process for remote files.
                  (file-remote-p default-directory)
                  (equal (car (aref eshell-current-handles
                                    eshell-output-handle))
                         (car (aref eshell-current-handles
                                    eshell-error-handle))))
        (eshell-protect-handles eshell-current-handles)
        (setq stderr-proc
              (make-pipe-process
               :name (concat (file-name-nondirectory command) "-stderr")
               :buffer (current-buffer)
               :filter (if (eshell-interactive-output-p eshell-error-handle)
                           #'eshell-output-filter
                         #'eshell-insertion-filter)
               :sentinel #'eshell-sentinel))
        (eshell-record-process-properties stderr-proc eshell-error-handle))
      (setq proc
            (let ((command (file-local-name (expand-file-name command)))
                  (conn-type (pcase (bound-and-true-p eshell-in-pipeline-p)
                               ('first '(nil . pipe))
                               ('last  '(pipe . nil))
                               ('t     'pipe)
                               ('nil   nil))))
              (make-process
               :name (file-name-nondirectory command)
               :buffer (current-buffer)
               :command (cons command args)
               :filter (if (eshell-interactive-output-p)
                           #'eshell-output-filter
                         #'eshell-insertion-filter)
               :sentinel #'eshell-sentinel
               :connection-type conn-type
               :stderr stderr-proc
               :file-handler t)))
      (eshell-record-process-object proc)
      (eshell-record-process-properties proc)
      (run-hook-with-args 'eshell-exec-hook proc)
      (when (fboundp 'process-coding-system)
	(let ((coding-systems (process-coding-system proc)))
	  (setq decoding (car coding-systems)
		encoding (cdr coding-systems)))
	;; If `make-process' decided to use some coding system for
	;; decoding data sent from the process and the coding system
	;; doesn't specify EOL conversion, we had better convert CRLF
	;; to LF.
	(if (vectorp (coding-system-eol-type decoding))
	    (setq decoding (coding-system-change-eol-conversion decoding 'dos)
		  changed t))
	;; Even if `make-process' left the coding system for encoding
	;; data sent to the process undecided, we had better use the
	;; same one as what we use for decoding.  But, we should
	;; suppress EOL conversion.
	(if (and decoding (not encoding))
	    (setq encoding (coding-system-change-eol-conversion decoding 'unix)
		  changed t))
	(if changed
	    (set-process-coding-system proc decoding encoding))))
     (t
      ;; No async subprocesses...
      (let ((oldbuf (current-buffer))
	    (interact-p (eshell-interactive-output-p))
	    lbeg lend line proc-buf exit-status)
	(and (not (markerp eshell-last-sync-output-start))
	     (setq eshell-last-sync-output-start (point-marker)))
	(setq proc-buf
	      (set-buffer (get-buffer-create eshell-scratch-buffer)))
	(erase-buffer)
	(set-buffer oldbuf)
	(run-hook-with-args 'eshell-exec-hook command)
	(setq exit-status
	      (apply #'call-process-region
		     (append (list eshell-last-sync-output-start (point)
				   command t
				   eshell-scratch-buffer nil)
			     args)))
	;; When in a pipeline, record the place where the output of
	;; this process will begin.
	(and (bound-and-true-p eshell-in-pipeline-p)
	     (set-marker eshell-last-sync-output-start (point)))
	;; Simulate the effect of the process filter.
	(when (numberp exit-status)
	  (set-buffer proc-buf)
	  (goto-char (point-min))
	  (setq lbeg (point))
	  (while (eq 0 (forward-line 1))
	    (setq lend (point)
		  line (buffer-substring-no-properties lbeg lend))
	    (set-buffer oldbuf)
	    (if interact-p
		(eshell-output-filter nil line)
	      (eshell-output-object line))
	    (setq lbeg lend)
	    (set-buffer proc-buf))
	  (set-buffer oldbuf))
        (require 'esh-mode)
        (declare-function eshell-update-markers "esh-mode" (pmark))
        (defvar eshell-last-output-end)         ;Defined in esh-mode.el.
	(eshell-update-markers eshell-last-output-end)
	;; Simulate the effect of eshell-sentinel.
	(eshell-close-handles
         (if (numberp exit-status) exit-status -1)
         (list 'quote (and (numberp exit-status) (= exit-status 0))))
	(eshell-kill-process-function command exit-status)
	(or (bound-and-true-p eshell-in-pipeline-p)
	    (setq eshell-last-sync-output-start nil))
	(if (not (numberp exit-status))
	  (error "%s: external command failed: %s" command exit-status))
	(setq proc t))))
    proc))

(defun eshell-insertion-filter (proc string)
  "Insert a string into the eshell buffer, or a process/file/buffer.
PROC is the process for which we're inserting output.  STRING is the
output."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (process-put proc :eshell-pending
                   (concat (process-get proc :eshell-pending)
                           string))
      (unless (process-get proc :eshell-busy) ; Already being handled?
        (while (process-get proc :eshell-pending)
          (let ((handles (process-get proc :eshell-handles))
                (index (process-get proc :eshell-handle-index))
                (data (process-get proc :eshell-pending)))
            (process-put proc :eshell-pending nil)
            (process-put proc :eshell-busy t)
            (unwind-protect
                (condition-case nil
                    (eshell-output-object data index handles)
                  ;; FIXME: We want to send SIGPIPE to the process
                  ;; here.  However, remote processes don't currently
                  ;; support that, and not all systems have SIGPIPE in
                  ;; the first place (e.g. MS Windows).  In these
                  ;; cases, just delete the process; this is
                  ;; reasonably close to the right behavior, since the
                  ;; default action for SIGPIPE is to terminate the
                  ;; process.  For use cases where SIGPIPE is truly
                  ;; needed, using an external pipe operator (`*|')
                  ;; may work instead (e.g. when working with remote
                  ;; processes).
                  (eshell-pipe-broken
                   (if (or (process-get proc 'remote-pid)
                           (eq system-type 'windows-nt))
                       (delete-process proc)
                     (signal-process proc 'SIGPIPE))))
              (process-put proc :eshell-busy nil))))))))

(defun eshell-sentinel (proc string)
  "Generic sentinel for command processes.  Reports only signals.
PROC is the process that's exiting.  STRING is the exit message."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (unwind-protect
          (unless (string= string "run")
            ;; Write the exit message if the status is abnormal and
            ;; the process is already writing to the terminal.
            (when (and (eq proc (eshell-tail-process))
                       (not (string-match "^\\(finished\\|exited\\)"
                                          string)))
              (funcall (process-filter proc) proc string))
            (let* ((handles (process-get proc :eshell-handles))
                   (index (process-get proc :eshell-handle-index))
                   (data (process-get proc :eshell-pending))
                   ;; Only get the status for the primary subprocess,
                   ;; not the pipe process (if any).
                   (status (when (= index eshell-output-handle)
                            (process-exit-status proc))))
              (process-put proc :eshell-pending nil)
              ;; If we're in the middle of handling output from this
              ;; process then schedule the EOF for later.
              (letrec ((finish-io
                        (lambda ()
                          (if (process-get proc :eshell-busy)
                              (run-at-time 0 nil finish-io)
                            (when data
                              (ignore-error 'eshell-pipe-broken
                                (eshell-output-object
                                 data index handles)))
                            (eshell-close-handles
                             status
                             (when status (list 'quote (= status 0)))
                             handles)))))
                (funcall finish-io))))
        (when-let ((entry (assq proc eshell-process-list)))
          (eshell-remove-process-entry entry))
        (eshell-kill-process-function proc string)))))

(defun eshell-process-interact (func &optional all query)
  "Interact with a process, using PROMPT if more than one, via FUNC.
If ALL is non-nil, background processes will be interacted with as well.
If QUERY is non-nil, query the user with QUERY before calling FUNC."
  (let (defunct result)
    (dolist (entry eshell-process-list)
      (if (and (memq (process-status (car entry))
		    '(run stop open closed))
	       (or all
		   (not (cdr entry)))
	       (or (not query)
		   (y-or-n-p (format-message query
					     (process-name (car entry))))))
	  (setq result (funcall func (car entry))))
      (unless (memq (process-status (car entry))
		    '(run stop open closed))
	(setq defunct (cons entry defunct))))
    ;; clean up the process list; this can get dirty if an error
    ;; occurred that brought the user into the debugger, and then they
    ;; quit, so that the sentinel was never called.
    (dolist (d defunct)
      (eshell-remove-process-entry d))
    result))

(defcustom eshell-kill-process-wait-time 5
  "Seconds to wait between sending termination signals to a subprocess."
  :type 'integer)

(defcustom eshell-kill-process-signals '(SIGINT SIGQUIT SIGKILL)
  "Signals used to kill processes when an Eshell buffer exits.
Eshell calls each of these signals in order when an Eshell buffer is
killed; if the process is still alive afterwards, Eshell waits a
number of seconds defined by `eshell-kill-process-wait-time', and
tries the next signal in the list."
  :type '(repeat symbol))

(defcustom eshell-kill-processes-on-exit nil
  "If non-nil, kill active processes when exiting an Eshell buffer.
Emacs will only kill processes owned by that Eshell buffer.

If nil, ownership of background and foreground processes reverts to
Emacs itself, and will die only if the user exits Emacs, calls
`kill-process', or terminates the processes externally.

If `ask', Emacs prompts the user before killing any processes.

If `every', it prompts once for every process.

If t, it kills all buffer-owned processes without asking.

Processes are first sent SIGHUP, then SIGINT, then SIGQUIT, then
SIGKILL.  The variable `eshell-kill-process-wait-time' specifies how
long to delay between signals."
  :type '(choice (const :tag "Kill all, don't ask" t)
		 (const :tag "Ask before killing" ask)
		 (const :tag "Ask for each process" every)
		 (const :tag "Don't kill subprocesses" nil)))

(defun eshell-round-robin-kill (&optional query)
  "Kill current process by trying various signals in sequence.
See the variable `eshell-kill-processes-on-exit'."
  (let ((sigs eshell-kill-process-signals))
    (while sigs
      (eshell-process-interact
       (lambda (proc)
         (signal-process (process-id proc) (car sigs))) t query)
      (setq query nil)
      (if (not eshell-process-list)
	  (setq sigs nil)
	(sleep-for eshell-kill-process-wait-time)
	(setq sigs (cdr sigs))))))

(defun eshell-query-kill-processes ()
  "Kill processes belonging to the current Eshell buffer, possibly with query."
  (when (and eshell-kill-processes-on-exit
	     eshell-process-list)
    (save-window-excursion
      (list-processes)
      (if (or (not (eq eshell-kill-processes-on-exit 'ask))
	      (y-or-n-p (format-message "Kill processes owned by `%s'? "
					(buffer-name))))
	  (eshell-round-robin-kill
	   (if (eq eshell-kill-processes-on-exit 'every)
	       "Kill Eshell child process `%s'? ")))
      (let ((buf (get-buffer "*Process List*")))
	(if (and buf (buffer-live-p buf))
	    (kill-buffer buf)))
      (message nil))))

(defun eshell-interrupt-process ()
  "Interrupt a process."
  (interactive)
  (unless (eshell-process-interact 'interrupt-process)
    (eshell-kill-process-function nil "interrupt")))

(defun eshell-kill-process ()
  "Kill a process."
  (interactive)
  (unless (eshell-process-interact 'kill-process)
    (eshell-kill-process-function nil "killed")))

(defun eshell-quit-process ()
  "Send quit signal to process."
  (interactive)
  (unless (eshell-process-interact 'quit-process)
    (eshell-kill-process-function nil "quit")))

;(defun eshell-stop-process ()
;  "Send STOP signal to process."
;  (interactive)
;  (unless (eshell-process-interact 'stop-process)
;    (eshell-kill-process-function nil "stopped")))

;(defun eshell-continue-process ()
;  "Send CONTINUE signal to process."
;  (interactive)
;  (unless (eshell-process-interact 'continue-process)
;    ;; jww (1999-09-17): this signal is not dealt with yet.  For
;    ;; example, `eshell-reset' will be called, and so will
;    ;; `eshell-resume-eval'.
;    (eshell-kill-process-function nil "continue")))

(provide 'esh-proc)
;;; esh-proc.el ends here
