;;; esh-proc.el --- process management  -*- lexical-binding:t -*-

;; Copyright (C) 1999-2026 Free Software Foundation, Inc.

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

(require 'esh-arg)
(require 'esh-io)
(require 'esh-opt)
(require 'esh-util)

(require 'pcomplete)

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

(defcustom eshell-process-wait-time 0.05
  "The number of seconds to delay waiting for a synchronous process."
  :version "30.1"
  :type 'number)

(defcustom eshell-process-wait-seconds 0
  "The number of seconds to delay waiting for a synchronous process."
  :type 'integer)
(make-obsolete-variable 'eshell-process-wait-seconds
                        'eshell-process-wait-time "30.1")

(defcustom eshell-process-wait-milliseconds 50
  "The number of milliseconds to delay waiting for a synchronous process."
  :type 'integer)
(make-obsolete-variable 'eshell-process-wait-milliseconds
                        'eshell-process-wait-time "30.1")

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

(defvar eshell-supports-asynchronous-processes (fboundp 'make-process)
  "Non-nil if Eshell can create asynchronous processes.")

(defvar eshell-subjob-messages t
  "Non-nil if we should print process start/end messages for subjobs.")
(defvar eshell-current-subjob-p nil)

(defvar eshell-process-list nil
  "A list of the current status of subprocesses.
Each element has the form (PROC . SUBJOB-P), where PROC is the
process object and SUBJOB-P is non-nil if the process is a
subjob.

To add or remove elements of this list, see
`eshell-record-process-object' and `eshell-remove-process-entry'.")

(declare-function eshell-reset "esh-mode" (&optional no-hooks))
(declare-function eshell-send-eof-to-process "esh-mode")
(declare-function eshell-interactive-output-filter "esh-mode" (buffer string))
(declare-function eshell-set-exit-info "esh-cmd" (status result))
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
  (declare (obsolete nil "30.1"))
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
  (setq-local eshell-special-ref-alist
              (cons
               `("process"
                 (creation-function   get-process)
                 (insertion-function  eshell-insert-process)
                 (completion-function eshell-complete-process-ref))
               eshell-special-ref-alist))

  (eshell-proc-mode))

(define-obsolete-function-alias 'eshell-reset-after-proc
  'eshell--reset-after-signal "30.1")

(defun eshell-process-active-p (process)
  "Return non-nil if PROCESS is active.
This is like `process-live-p', but additionally checks whether
`eshell-sentinel' has finished all of its work yet."
  (or (process-live-p process)
      ;; If we have handles, this is an Eshell-managed
      ;; process.  Wait until we're 100% done and have
      ;; cleared out the handles (see `eshell-sentinel').
      (process-get process :eshell-handles)))

(defun eshell-wait-for-processes (&optional procs timeout)
  "Wait until PROCS have completed execution.
If TIMEOUT is non-nil, wait at most that many seconds.  Return non-nil
if all the processes finished executing before the timeout expired."
  (let ((expiration (when timeout (time-add (current-time) timeout))))
    (catch 'timeout
      (dolist (proc procs)
        (while (if (processp proc)
                   (eshell-process-active-p proc)
                 (process-attributes proc))
          (when (input-pending-p)
            (discard-input))
          (when (and expiration
                     (not (time-less-p (current-time) expiration)))
            (throw 'timeout nil))
          (sit-for eshell-process-wait-time)))
      t)))

(defun eshell-wait-for-process (&rest procs)
  "Wait until PROCS have completed execution."
  (declare (obsolete 'eshell-wait-for-processes "31.1"))
  (eshell-wait-for-processes procs))

(defun eshell/wait (&rest args)
  "Wait until processes have completed execution."
  (eshell-eval-using-options
   "wait" args
   '((?h "help" nil nil "show this usage screen")
     (?t "timeout" t timeout "timeout in seconds")
     :preserve-args
     :show-usage
     :usage "[OPTION] PROCESS...
Wait until PROCESS(es) have completed execution.")
   (when (stringp timeout)
     (setq timeout (string-to-number timeout)))
   (dolist (arg args)
     (unless (or (processp arg) (natnump arg))
       (error "wait: invalid argument type: %s" (type-of arg))))
   (unless (eshell-wait-for-processes args timeout)
     (error "wait: timed out after %s seconds" timeout))))

(defun eshell/jobs ()
  "List processes, if there are any."
  (and (fboundp 'process-list)
       (process-list)
       (list-processes)))

(defun eshell/kill (&rest args)
  "Kill processes.
Usage: kill [-<signal>] <pid>|<process> ...
Accepts PIDs and process objects.  Optionally accept signals
and signal names."
  (let ((signum 'SIGINT))
    (let ((arg (car args))
          (case-fold-search nil))
      (when (stringp arg)
        ;; If the first argument starts with a dash, treat it as the
        ;; signal specifier.
        (cond
         ((string-match "\\`-[[:digit:]]+\\'" arg)
          (setq signum (abs (string-to-number arg)))
          (pop args))
         ((string-match "\\`-\\([[:upper:]]+\\|[[:lower:]]+\\)\\'" arg)
          (setq signum (intern (substring arg 1)))
          (pop args)))))
    (dolist (proc args)
      (when (stringp proc)
        (setq proc (string-to-number proc)))
      (let ((result
             (cond
              ((numberp proc)
               (when (<= proc 0)
                 (error "kill: bad pid: %d" proc))
               (signal-process proc signum (file-remote-p default-directory)))
              ((eshell-processp proc)
               (signal-process proc signum))
              (t
               (error "kill: invalid argument type: %s" (type-of proc))))))
        (when (= result -1)
          (error "kill: failed to kill process %s" proc))))))

(put 'eshell/kill 'eshell-no-numeric-conversions t)

(defsubst eshell-record-process-object (object)
  "Record OBJECT as now running."
  (when (and eshell-subjob-messages
             eshell-current-subjob-p
             (eshell-processp object))
    (require 'esh-mode)
    (declare-function eshell-interactive-print "esh-mode" (string))
    (eshell-interactive-print
     (format "[%s] %d\n" (process-name object) (process-id object))))
  (push (cons object eshell-current-subjob-p) eshell-process-list))

(defun eshell-remove-process-entry (entry)
  "Record the process ENTRY as fully completed."
  (when (and eshell-subjob-messages
             eshell-done-messages-in-minibuffer
             (eshell-processp (car entry))
             (cdr entry))
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
     (eshell-supports-asynchronous-processes
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
                           #'eshell-interactive-process-filter
                         #'eshell-insertion-filter)
               :sentinel #'eshell-sentinel))
        (eshell-record-process-properties stderr-proc eshell-error-handle))
      (eshell-protect-handles eshell-current-handles)
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
                           #'eshell-interactive-process-filter
                         #'eshell-insertion-filter)
               :sentinel #'eshell-sentinel
               :connection-type conn-type
               :stderr stderr-proc
               :file-handler t)))
      (eshell-debug-command 'process
        "started external process `%s'\n\n%s" proc
        (mapconcat #'shell-quote-argument (process-command proc) " "))
      (eshell-record-process-object proc)
      (eshell-record-process-properties proc)
      ;; Don't set exit info for processes being piped elsewhere.
      (when (memq (bound-and-true-p eshell-in-pipeline-p) '(nil last))
        (process-put proc :eshell-set-exit-info t))
      (when stderr-proc
        ;; Provide a shared flag between the primary and stderr
        ;; processes.  This lets the primary process wait to clean up
        ;; until stderr is totally finished (see `eshell-sentinel').
        (let ((stderr-live (list t)))
          (process-put proc :eshell-stderr-live stderr-live)
          (process-put stderr-proc :eshell-stderr-live stderr-live)))
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
        ;; XXX: This doesn't support sending stdout and stderr to
        ;; separate places.
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
		(eshell-interactive-process-filter nil line)
	      (eshell-output-object line))
	    (setq lbeg lend)
	    (set-buffer proc-buf))
	  (set-buffer oldbuf))
        ;; Simulate the effect of `eshell-sentinel'.
        (eshell-set-exit-info
         (if (numberp exit-status) exit-status -1)
         (and (numberp exit-status) (= exit-status 0)))
	(run-hook-with-args 'eshell-kill-hook command exit-status)
	(or (bound-and-true-p eshell-in-pipeline-p)
	    (setq eshell-last-sync-output-start nil))
	(if (not (numberp exit-status))
	  (error "%s: external command failed: %s" command exit-status))
	(setq proc t))))
    proc))

(defun eshell-interactive-process-filter (process string)
  "Send the output from PROCESS (STRING) to the interactive display.
This is done after all necessary filtering has been done."
  (when string
    (eshell-debug-command 'process
      "received output from process `%s'\n\n%s" process string)
    (eshell-interactive-output-filter (if process (process-buffer process)
                                        (current-buffer))
                                      string)))

(define-obsolete-function-alias 'eshell-output-filter
  #'eshell-interactive-process-filter "30.1")

(defun eshell-insertion-filter (proc string)
  "Insert a string into the eshell buffer, or a process/file/buffer.
PROC is the process for which we're inserting output.  STRING is the
output."
  (eshell-debug-command 'process
    "received output from process `%s'\n\n%s" proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (process-put proc :eshell-pending
                   (concat (process-get proc :eshell-pending)
                           string))
      (if (process-get proc :eshell-busy)
          (eshell-debug-command 'process "i/o busy for process `%s'" proc)
        (unwind-protect
            (let ((handles (process-get proc :eshell-handles))
                  (index (process-get proc :eshell-handle-index))
                  data)
              (while (setq data (process-get proc :eshell-pending))
                (process-put proc :eshell-pending nil)
                (eshell-debug-command 'process
                  "forwarding output from process `%s'\n\n%s" proc data)
                (condition-case nil
                    (eshell-output-object data index handles)
                  (eshell-pipe-broken
                   ;; The output pipe broke, so send SIGPIPE to the
                   ;; process.  NOTE: Due to the additional indirection
                   ;; of Emacs process filters, the process will likely
                   ;; see the SIGPIPE later than it would in a regular
                   ;; shell, which could cause problems.  For cases
                   ;; where this matters, using an external pipe
                   ;; operator (`*|') may work instead.
                   (cond
                    ;; Delay signaling remote processes to prevent
                    ;; "Forbidden reentrant call of Tramp".
                    ((process-get proc 'remote-pid)
                     (run-at-time 0 nil #'signal-process proc 'SIGPIPE))
                    ;; MS-Windows doesn't support SIGPIPE, so send
                    ;; SIGTERM there instead; this is reasonably close
                    ;; to the right behavior, since the default action
                    ;; for SIGPIPE is to terminate the process.
                    ((eq system-type 'windows-nt)
                     (signal-process proc 'SIGTERM))
                    (t
                     (signal-process proc 'SIGPIPE)))))))
          (process-put proc :eshell-busy nil))))))

(defun eshell-sentinel (proc string)
  "Generic sentinel for command processes.  Reports only signals.
PROC is the process that's exiting.  STRING is the exit message."
  (eshell-debug-command 'process
    "sentinel for external process `%s': %S" proc string)
  (when (and (buffer-live-p (process-buffer proc))
             (not (string= string "run")))
    (with-current-buffer (process-buffer proc)
      (unwind-protect
          (let* ((handles (process-get proc :eshell-handles))
                 (index (process-get proc :eshell-handle-index))
                 (primary (= index eshell-output-handle))
                 (set-exit-info (process-get proc :eshell-set-exit-info))
                 (data (process-get proc :eshell-pending))
                 (stderr-live (process-get proc :eshell-stderr-live)))
            ;; Write the exit message for the last process in the
            ;; foreground pipeline if its status is abnormal and
            ;; stderr is already writing to the terminal.
            (when (and (eq proc (eshell-tail-process))
                       (eshell-interactive-output-p eshell-error-handle handles)
                       (not (string-match "^\\(finished\\|exited\\)"
                                          string)))
              (eshell-interactive-output-filter (process-buffer proc) string))
            (process-put proc :eshell-pending nil)
            ;; If we're in the middle of handling output from this
            ;; process then schedule the EOF for later.
            (letrec ((wait-for-stderr (and primary
                                           (not (process-live-p proc))))
                     (finish-io
                      (lambda ()
                        (if (buffer-live-p (process-buffer proc))
                            (with-current-buffer (process-buffer proc)
                              (if (or (process-get proc :eshell-busy)
                                      (and wait-for-stderr (car stderr-live)))
                                  (progn
                                    (eshell-debug-command 'process
                                      "i/o busy for process `%s'" proc)
                                    (run-at-time 0 nil finish-io))
                                (when data
                                  (ignore-error eshell-pipe-broken
                                    (eshell-output-object
                                     data index handles)))
                                (when set-exit-info
                                  (let ((status (process-exit-status proc)))
                                    (eshell-set-exit-info status (= status 0))))
                                (eshell-close-handles handles)
                                ;; Clear the handles to mark that we're 100%
                                ;; finished with the I/O for this process.
                                (process-put proc :eshell-handles nil)
                                (eshell-debug-command 'process
                                  "finished external process `%s'" proc)
                                (if primary
                                    (run-hook-with-args 'eshell-kill-hook
                                                        proc string)
                                  (setcar stderr-live nil))))
                          (eshell-debug-command 'process
                            "buffer for external process `%s' already killed"
                            proc)))))
              (funcall finish-io)))
        (when-let* ((entry (assq proc eshell-process-list)))
          (eshell-remove-process-entry entry))))))

(defun eshell-process-interact (func &optional all query)
  "Interact with a process, using PROMPT if more than one, via FUNC.
If ALL is non-nil, background processes will be interacted with as well.
If QUERY is non-nil, query the user with QUERY before calling FUNC."
  (let (defunct result)
    (dolist (entry eshell-process-list)
      (if (and (process-live-p (car entry))
	       (or all
		   (not (cdr entry)))
	       (or (not query)
		   (y-or-n-p (format-message query
					     (process-name (car entry))))))
	  (setq result (funcall func (car entry))))
      (unless (process-live-p (car entry))
	(setq defunct (cons entry defunct))))
    ;; clean up the process list; this can get dirty if an error
    ;; occurred that brought the user into the debugger, and then they
    ;; quit, so that the sentinel was never called.
    (dolist (d defunct)
      (eshell-remove-process-entry d))
    result))

(defcustom eshell-kill-process-wait-time 5
  "Seconds to wait between sending termination signals to a subprocess."
  :type 'number)

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
  (catch 'done
    (dolist (sig eshell-kill-process-signals)
      (eshell-process-interact
       (lambda (proc) (signal-process proc sig)) t query)
      (when (eshell-wait-for-processes (mapcar #'car eshell-process-list)
                                       eshell-kill-process-wait-time)
        (throw 'done nil))
      (setq query nil))))

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

(defun eshell--reset-after-signal (status)
  "Reset the prompt after a signal when necessary.
STATUS is the status associated with the signal; if
`eshell-reset-signals' matches status, reset the prompt.

This is really only useful when \"signaling\" while there's no
foreground process.  Otherwise, `eshell-resume-command' handles
everything."
  (when (and (stringp status)
	     (string-match eshell-reset-signals status))
    (eshell-reset)))

(defun eshell-interrupt-process ()
  "Interrupt a process."
  (interactive)
  (unless (eshell-process-interact 'interrupt-process)
    (eshell--reset-after-signal "interrupt\n")))

(defun eshell-kill-process ()
  "Kill a process."
  (interactive)
  (unless (eshell-process-interact 'kill-process)
    (eshell--reset-after-signal "killed\n")))

(defun eshell-quit-process ()
  "Send quit signal to process."
  (interactive)
  (unless (eshell-process-interact 'quit-process)
    (eshell--reset-after-signal "quit\n")))

;(defun eshell-stop-process ()
;  "Send STOP signal to process."
;  (interactive)
;  (unless (eshell-process-interact 'stop-process)
;    (eshell--reset-after-signal "stopped\n")))

;(defun eshell-continue-process ()
;  "Send CONTINUE signal to process."
;  (interactive)
;  (unless (eshell-process-interact 'continue-process)
;    ;; jww (1999-09-17): this signal is not dealt with yet.  For
;    ;; example, `eshell-reset' will be called, and so will
;    ;; `eshell-resume-eval'.
;    (eshell--reset-after-signal "continue\n")))

;;; Special references

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
  (insert-and-inherit "#<process "
                      (eshell-quote-argument (process-name process))
                      ">"))

(defun eshell-complete-process-ref ()
  "Perform completion for process references."
  (pcomplete-here (mapcar #'process-name (process-list))))

(provide 'esh-proc)
;;; esh-proc.el ends here
