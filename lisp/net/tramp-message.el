;;; tramp-message.el --- Tramp messages  -*- lexical-binding:t -*-

;; Copyright (C) 2023-2024 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, processes
;; Package: tramp

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

;; This package collects all Tramp functions to trace.  This is driven
;; by the user option `tramp-verbose'.  The following buffers are
;; created:
;;
;; - *debug tramp/method user@host*
;;
;;   This buffer is created when `tramp-verbose' is greater than or
;;   equal 4.  It contains all messages with a level up to `tramp-verbose'.
;;
;;   When `tramp-debug-command-messages' is non-nil, the buffer
;;   contains all messages with level 6 and the entry/exit messages of
;;   `tramp-file-name-handler'.  This is intended to analyze which
;;   remote commands are sent for a given file name operation.
;;
;; - *trace tramp/method user@host*
;;
;;   This buffer is created by the trace.el package when
;;   `tramp-verbose' is is greater than or equal 11.  It traces all
;;   functions with suffix "tramp-" except those function with the
;;   property `tramp-suppress-trace'.

;;; Code:

(require 'tramp-compat)
(require 'help-mode)

(declare-function tramp-file-name-equal-p "tramp")
(declare-function tramp-file-name-host-port "tramp")
(declare-function tramp-file-name-user-domain "tramp")
(declare-function tramp-get-default-directory "tramp")

;;;###tramp-autoload
(defcustom tramp-verbose 3
  "Verbosity level for Tramp messages.
Any level x includes messages for all levels 1 .. x-1.  The levels are

 0  silent (no tramp messages at all)
 1  errors
 2  warnings
 3  connection to remote hosts (default level)
 4  activities
 5  internal
 6  sent and received strings
 7  connection properties
 8  file caching
 9  test commands
10  traces (huge)
11  call traces (maintainer only)."
  :group 'tramp
  :type 'integer)

(defcustom tramp-debug-to-file nil
  "Whether Tramp debug messages shall be saved to file.
The debug file has the same name as the debug buffer, written to
`tramp-compat-temporary-file-directory'."
  :group 'tramp
  :version "28.1"
  :type 'boolean)

(defcustom tramp-debug-command-messages nil
  "Whether to write only command messages to the debug buffer.
This increases `tramp-verbose' to 6 if necessary."
  :group 'tramp
  :version "30.1"
  :type 'boolean)

(defconst tramp-debug-outline-regexp
  (rx ;; Timestamp.
      (+ digit) ":" (+ digit) ":" (+ digit) "." (+ digit) blank
      ;; Thread.
      (? (group "#<thread " (+ nonl) ">") blank)
      ;; Function name, verbosity.
      (group (+ (any "-" alnum))) " (" (group (+ digit)) ") #")
  "Used for highlighting Tramp debug buffers in `outline-mode'.
When it is used for regexp matching, the regexp groups are

  1 for the thread name (optional)
  2 for the function name
  3 for the verbosity level.")

(defconst tramp-debug-font-lock-keywords
  ;; FIXME: Make it a function instead of an ELisp expression, so you
  ;; can evaluate it with `funcall' rather than `eval'!
  ;; Also, in `font-lock-defaults' you can specify a function name for
  ;; the "KEYWORDS" part, so font-lock calls it to get the actual keywords!
  '(list
    (rx bol (regexp tramp-debug-outline-regexp) (+ nonl))
    '(1 font-lock-warning-face t t)
    '(0 (outline-font-lock-face) keep t))
  "Used for highlighting Tramp debug buffers in `outline-mode'.")

(defun tramp-debug-outline-level ()
  "Return the depth to which a statement is nested in the outline.
Point must be at the beginning of a header line.

The outline level is equal to the verbosity of the Tramp message."
  (declare (tramp-suppress-trace t))
  (1+ (string-to-number (match-string 3))))

;; This function takes action since Emacs 28.1, when
;; `read-extended-command-predicate' is set to
;; `command-completion-default-include-p'.
(defun tramp-debug-buffer-command-completion-p (_symbol buffer)
  "A predicate for Tramp interactive commands.
They are completed by \"M-x TAB\" only in Tramp debug buffers."
  (declare (tramp-suppress-trace t))
  (with-current-buffer buffer
    (string-equal
     (buffer-substring (point-min) (min (+ (point-min) 10) (point-max)))
     ";; Emacs:")))

(defun tramp-setup-debug-buffer ()
  "Function to setup debug buffers."
  (declare (tramp-suppress-trace t))
  ;; (declare (completion tramp-debug-buffer-command-completion-p)
  ;; 	   (tramp-suppress-trace t))
  (interactive)
  (set-buffer-file-coding-system 'utf-8)
  (setq buffer-undo-list t)
  ;; Activate `outline-mode'.  This runs `text-mode-hook' and
  ;; `outline-mode-hook'.  We must prevent that local processes die.
  ;; Yes: I've seen `flyspell-mode', which starts "ispell".
  ;; `(custom-declare-variable outline-minor-mode-prefix ...)'  raises
  ;; on error in `(outline-mode)', we don't want to see it in the
  ;; traces.
  (let ((default-directory tramp-compat-temporary-file-directory))
    (outline-mode))
  (setq-local outline-level 'tramp-debug-outline-level)
  (setq-local font-lock-keywords
              ;; FIXME: This `(t FOO . BAR)' representation in
              ;; `font-lock-keywords' is supposed to be an internal
              ;; implementation "detail".  Don't abuse it here!
              `(t (eval ,tramp-debug-font-lock-keywords t)
                  ,(eval tramp-debug-font-lock-keywords t)))
  ;; Do not edit the debug buffer.
  (use-local-map special-mode-map)
  (set-buffer-modified-p nil)
  ;; For debugging purposes.
  (local-set-key "\M-n" 'clone-buffer)
  (add-hook 'clone-buffer-hook #'tramp-setup-debug-buffer nil 'local))

(function-put
 #'tramp-setup-debug-buffer 'completion-predicate
 #'tramp-debug-buffer-command-completion-p)

(defun tramp-debug-buffer-name (vec)
  "A name for the debug buffer of VEC."
  (declare (tramp-suppress-trace t))
  (let ((method (tramp-file-name-method vec))
	(user-domain (tramp-file-name-user-domain vec))
	(host-port (tramp-file-name-host-port vec)))
    (if (tramp-string-empty-or-nil-p user-domain)
	(format "*debug tramp/%s %s*" method host-port)
      (format "*debug tramp/%s %s@%s*" method user-domain host-port))))

(defun tramp-get-debug-buffer (vec)
  "Get the debug buffer of VEC."
  (declare (tramp-suppress-trace t))
  (with-current-buffer (get-buffer-create (tramp-debug-buffer-name vec))
    (when (bobp)
      (tramp-setup-debug-buffer))
    (current-buffer)))

(defun tramp-get-debug-file-name (vec)
  "Get the debug file name for VEC."
  (declare (tramp-suppress-trace t))
  (expand-file-name
   (tramp-compat-string-replace "/" " " (tramp-debug-buffer-name vec))
   tramp-compat-temporary-file-directory))

(defun tramp-trace-buffer-name (vec)
  "A name for the trace buffer for VEC."
  (declare (tramp-suppress-trace t))
   (tramp-compat-string-replace "*debug" "*trace" (tramp-debug-buffer-name vec)))

(defvar tramp-trace-functions nil
  "A list of non-Tramp functions to be traced with `tramp-verbose' > 10.")

(defun tramp-debug-message (vec fmt-string &rest arguments)
  "Append message to debug buffer of VEC.
Message is formatted with FMT-STRING as control string and the remaining
ARGUMENTS to actually emit the message (if applicable)."
  (declare (tramp-suppress-trace t))
  (let ((inhibit-message t)
	create-lockfiles file-name-handler-alist message-log-max
	signal-hook-function)
    (with-current-buffer (tramp-get-debug-buffer vec)
      (goto-char (point-max))
      (let ((point (point)))
	(when (bobp)
	  ;; Headline.
	  (insert
	   (format
	    ";; Emacs: %s Tramp: %s -*- mode: outline; coding: utf-8; -*-"
	    emacs-version tramp-version))
	  (when (>= tramp-verbose 10)
	    (let ((tramp-verbose 0))
	      (insert
	       (format
		"\n;; Location: %s Git: %s/%s"
		(locate-library "tramp")
		(or tramp-repository-branch "")
		(or tramp-repository-version "")))))
	  ;; Traces.
	  (when (>= tramp-verbose 11)
	    (dolist
		(elt
		 (append
		  (mapcar
		   #'intern (all-completions "tramp-" obarray #'functionp))
		  tramp-trace-functions))
	      (unless (get elt 'tramp-suppress-trace)
		(trace-function-background elt (tramp-trace-buffer-name vec)))))
	  ;; Delete debug file.  Announce its further existence.
	  (when (and tramp-debug-to-file (tramp-get-debug-file-name vec))
	    (ignore-errors (delete-file (tramp-get-debug-file-name vec)))
	    (let ((message-log-max t))
	      (message
	       "Tramp debug file is %s" (tramp-get-debug-file-name vec)))))
	(unless (bolp)
	  (insert "\n"))
	;; Timestamp.
	(insert (format-time-string "%T.%6N "))
	;; Threads.  `current-thread' might not exist when Emacs is
	;; configured --without-threads.
	;; (unless (eq (tramp-compat-funcall 'current-thread) main-thread)
	;;   (insert (format "%s " (tramp-compat-funcall 'current-thread))))
	;; Calling Tramp function.  We suppress compat and trace
	;; functions from being displayed.
	(let ((frames (backtrace-frames))
	      btf fn)
	  (while (not fn)
	    (setq btf (cadadr frames))
	    (if (not btf)
		(setq fn "")
	      (and (symbolp btf) (setq fn (symbol-name btf))
		   (or (not (string-prefix-p "tramp" fn))
		       (get btf 'tramp-suppress-trace))
		   (setq fn nil))
	      (setq frames (cdr frames))))
	  ;; The following code inserts filename and line number.
	  ;; Should be inactive by default, because it is time consuming.
	  ;; (let ((ffn (find-function-noselect (intern fn))))
	  ;;   (insert
	  ;;    (format
	  ;;     "%s:%d: "
	  ;;     (file-name-nondirectory (buffer-file-name (car ffn)))
	  ;;     (with-current-buffer (car ffn)
	  ;;       (1+ (count-lines (point-min) (cdr ffn)))))))
	  (insert (format "%s " fn)))
	;; The message.
	(insert (apply #'format-message fmt-string arguments))
	(if tramp-debug-command-messages
	    ;; Add help function.
	    (tramp-debug-message-buttonize point)
	  ;; Write message to debug file.
	  (when tramp-debug-to-file
	    (ignore-errors
	      (write-region
	       point (point-max) (tramp-get-debug-file-name vec) 'append))))))))

;;;###tramp-autoload
(defun tramp-message (vec-or-proc level fmt-string &rest arguments)
  "Emit a message depending on verbosity level.
VEC-OR-PROC identifies the Tramp buffer to use.  It can be either a
vector or a process.  LEVEL says to be quiet if `tramp-verbose' is
less than LEVEL.  The message is emitted only if `tramp-verbose' is
greater than or equal to LEVEL.

The message is also logged into the debug buffer when `tramp-verbose'
is greater than or equal 4.

Calls functions `message' and `tramp-debug-message' with FMT-STRING as
control string and the remaining ARGUMENTS to actually emit the message (if
applicable)."
  ;; (declare (tramp-suppress-trace t))
  (ignore-errors
    (let ((tramp-verbose
	   (if tramp-debug-command-messages
	       (max tramp-verbose 6) tramp-verbose)))
      (when (<= level tramp-verbose)
	;; Log only when there is a minimum level.
	(when (>= tramp-verbose 4)
	  (let ((tramp-verbose 0))
	    ;; Append connection buffer for error messages, if exists.
	    (when (= level 1)
	      (ignore-errors
		(setq fmt-string (concat fmt-string "\n%s")
		      arguments
		      (append
		       arguments
		       `(,(tramp-get-buffer-string
			   (if (processp vec-or-proc)
			       (process-buffer vec-or-proc)
			     (tramp-get-connection-buffer
			      vec-or-proc 'dont-create))))))))
	    ;; Translate proc to vec.  Handle nil vec.
	    (when (processp vec-or-proc)
	      (setq vec-or-proc (process-get vec-or-proc 'tramp-vector)))
	    (setq vec-or-proc (tramp-file-name-unify vec-or-proc)))
	  ;; Do it.
	  (when (and (tramp-file-name-p vec-or-proc)
		     (or (null tramp-debug-command-messages) (= level 6)))
	    (apply #'tramp-debug-message
		   vec-or-proc
		   (concat (format "(%d) # " level) fmt-string)
		   arguments)))
	;; Display only when there is a minimum level, and the
	;; progress reporter doesn't suppress further messages.
	(when (and (<= level 3) (null tramp-inhibit-progress-reporter))
	  (apply #'message
		 (concat
		  (cond
		   ((= level 0) "")
		   ((= level 1) "")
		   ((= level 2) "Warning: ")
		   (t           "Tramp: "))
		  fmt-string)
		 arguments))))))

;; We cannot use the `declare' form for `tramp-suppress-trace' in
;; autoloaded functions, because the tramp-loaddefs.el generation
;; would fail.
(function-put #'tramp-message 'tramp-suppress-trace t)

(defsubst tramp-backtrace (&optional vec-or-proc force)
  "Dump a backtrace into the debug buffer.
If VEC-OR-PROC is nil, the buffer *debug tramp* is used.  FORCE
forces the backtrace even if `tramp-verbose' is less than 10.
This function is meant for debugging purposes."
  (let ((tramp-verbose (if force 10 tramp-verbose)))
    (when (>= tramp-verbose 10)
      (tramp-message
       vec-or-proc 10 "\n%s" (with-output-to-string (backtrace))))))

(defsubst tramp-error (vec-or-proc signal fmt-string &rest arguments)
  "Emit an error.
VEC-OR-PROC identifies the connection to use, SIGNAL is the
signal identifier to be raised, remaining arguments passed to
`tramp-message'.  Finally, signal SIGNAL is raised with
FMT-STRING and ARGUMENTS."
  (let (signal-hook-function)
    (tramp-backtrace vec-or-proc)
    (unless arguments
      ;; FMT-STRING could be just a file name, as in
      ;; `file-already-exists' errors.  It could contain the ?\%
      ;; character, as in smb domain spec.
      (setq arguments (list fmt-string)
	    fmt-string "%s"))
    (tramp-message
     vec-or-proc 1 "%s"
     (error-message-string
      (list signal
	    (get signal 'error-message)
	    (apply #'format-message fmt-string arguments))))
    (signal signal (list (substring-no-properties
			  (apply #'format-message fmt-string arguments))))))

(defvar tramp-error-show-message-timeout 30
  "Time to show the Tramp buffer in case of an error.
If it is bound to nil, the buffer is not shown.  This is used in
tramp-tests.el.")

(defsubst tramp-error-with-buffer
  (buf vec-or-proc signal fmt-string &rest arguments)
  "Emit an error, and show BUF.
If BUF is nil, show the connection buf.  Wait for 30\", or until
an input event arrives.  The other arguments are passed to `tramp-error'."
  (save-window-excursion
    (let* ((buf (or (and (bufferp buf) buf)
		    (and (processp vec-or-proc) (process-buffer vec-or-proc))
		    (and (tramp-file-name-p vec-or-proc)
			 (tramp-get-connection-buffer vec-or-proc))))
	   (vec (or (and (tramp-file-name-p vec-or-proc) vec-or-proc)
		    (and buf (tramp-dissect-file-name
			      (tramp-get-default-directory buf))))))
      (unwind-protect
	  (apply #'tramp-error vec-or-proc signal fmt-string arguments)
	;; Save exit.
	(when (and buf
		   (natnump tramp-error-show-message-timeout)
		   (not (zerop tramp-verbose))
		   ;; Do not show when flagged from outside.
		   (not non-essential)
		   ;; Show only when Emacs has started already.
		   (current-message))
	  (let ((enable-recursive-minibuffers t)
		inhibit-message)
	    ;; `tramp-error' does not show messages.  So we must do it
	    ;; ourselves.
	    (apply #'message fmt-string arguments)
	    ;; Show buffer.
	    (pop-to-buffer buf)
	    (discard-input)
	    (sit-for tramp-error-show-message-timeout)))
	;; Reset timestamp.  It would be wrong after waiting for a while.
	(when (tramp-file-name-equal-p vec (car tramp-current-connection))
	  (setcdr tramp-current-connection (current-time)))))))

(defsubst tramp-user-error (vec-or-proc fmt-string &rest arguments)
  "Signal a user error (or \"pilot error\")."
  (unwind-protect
      (apply #'tramp-error vec-or-proc 'user-error fmt-string arguments)
    ;; Save exit.
    (when (and (natnump tramp-error-show-message-timeout)
	       (not (zerop tramp-verbose))
	       ;; Do not show when flagged from outside.
	       (not non-essential)
	       ;; Show only when Emacs has started already.
	       (current-message))
      (let ((enable-recursive-minibuffers t)
	    inhibit-message)
	;; `tramp-error' does not show messages.  So we must do it ourselves.
	(apply #'message fmt-string arguments)
	(discard-input)
	(sit-for tramp-error-show-message-timeout)
	;; Reset timestamp.  It would be wrong after waiting for a while.
	(when
	    (tramp-file-name-equal-p vec-or-proc (car tramp-current-connection))
	  (setcdr tramp-current-connection (current-time)))))))

(defmacro tramp-with-demoted-errors (vec-or-proc format &rest body)
  "Execute BODY while redirecting the error message to `tramp-message'.
BODY is executed like wrapped by `with-demoted-errors'.  FORMAT
is a format-string containing a %-sequence meaning to substitute
the resulting error message."
  (declare (indent 2) (debug (symbolp form body)))
  (let ((err (make-symbol "err")))
    `(condition-case-unless-debug ,err
         (progn ,@body)
       (error (tramp-message ,vec-or-proc 3 ,format ,err) nil))))

(defun tramp-test-message (fmt-string &rest arguments)
  "Emit a Tramp message according `default-directory'."
  (declare (tramp-suppress-trace t))
  (cond
   ((tramp-tramp-file-p default-directory)
    (apply #'tramp-message
	   (tramp-dissect-file-name default-directory) 0 fmt-string arguments))
   ((tramp-file-name-p (car tramp-current-connection))
    (apply #'tramp-message
	   (car tramp-current-connection) 0 fmt-string arguments))
   (t (apply #'message fmt-string arguments))))

(defun tramp-debug-button-action (button)
  "Goto the linked message in debug buffer at place."
  (declare (tramp-suppress-trace t))
  (when (mouse-event-p last-input-event) (mouse-set-point last-input-event))
  (when-let ((point (button-get button 'position)))
    (goto-char point)))

(define-button-type 'tramp-debug-button-type
  'follow-link t
  'mouse-face 'highlight
  'action #'tramp-debug-button-action)

(defun tramp-debug-link-messages (pos1 pos2)
  "Set links for two messages in current buffer.
The link buttons are in the verbositiy level substrings."
  (declare (tramp-suppress-trace t))
  (save-excursion
    (let (beg1 end1 beg2 end2)
      (goto-char pos1)
      ;; Find positions.
      (while (not (search-forward-regexp
		   tramp-debug-outline-regexp (line-end-position) t))
	(forward-line))
      (setq beg1 (1- (match-beginning 3)) end1 (1+ (match-end 3)))
      (goto-char pos2)
      (while (not (search-forward-regexp
		   tramp-debug-outline-regexp (line-end-position) t))
	(forward-line))
      (setq beg2 (1- (match-beginning 3)) end2 (1+ (match-end 3)))
      ;; Create text buttons.
      (make-text-button
       beg1 end1 :type 'tramp-debug-button-type
       'position (set-marker (make-marker) beg2)
       'help-echo "mouse-2, RET: goto exit message")
      (make-text-button
       beg2 end2 :type 'tramp-debug-button-type
       'position (set-marker (make-marker) beg1)
       'help-echo "mouse-2, RET: goto entry message"))))

(defvar tramp-debug-nesting ""
  "Indicator for debug messages nested level.
This shouldn't be changed globally, but let-bind where needed.")

(defvar tramp-debug-message-fnh-function nil
  "The used file name handler operation.
Bound in `tramp-*-file-name-handler' functions.")

(defun tramp-debug-message-buttonize (position)
  "Buttonize function in current buffer, at next line starting after POSITION."
  (declare (tramp-suppress-trace t))
  (save-excursion
    (goto-char position)
    (while (not (search-forward-regexp
		 tramp-debug-outline-regexp (line-end-position) t))
      (forward-line))
    (let ((fun (intern (match-string 2))))
      (make-text-button
       (match-beginning 2) (match-end 2)
       :type 'help-function-def
       'help-args (list fun (symbol-file fun))))))

;; This is used in `tramp-file-name-handler' and `tramp-*-maybe-open-connection'.
(defmacro with-tramp-debug-message (vec message &rest body)
  "Execute BODY, embedded with MESSAGE in the debug buffer of VEC.
If BODY does not raise a debug message, MESSAGE is ignored."
  (declare (indent 2) (debug t))
  (let ((result (make-symbol "result")))
    `(if tramp-debug-command-messages
	 (save-match-data
	   (let ((tramp-debug-nesting
		  (concat tramp-debug-nesting "#"))
		 (buf (tramp-get-debug-buffer ,vec))
		 beg end ,result)
	     ;; Insert entry message.
	     (with-current-buffer buf
	       (setq beg (point))
	       (tramp-debug-message
		,vec "(4) %s %s ..." tramp-debug-nesting ,message)
	       (setq end (point)))
	     (unwind-protect
		 ;; Run BODY.
		 (setq tramp-debug-message-fnh-function nil
		       ,result (progn ,@body))
	       (with-current-buffer buf
		 (if (= end (point-max))
		     (progn
		       (delete-region beg end)
		       (when (bobp) (kill-buffer)))
		   ;; Insert exit message.
		   (tramp-debug-message
		    ,vec "(5) %s %s ... %s" tramp-debug-nesting ,message ,result)
		   ;; Adapt file name handler function.
		   (dolist (pos (list (point-max) end))
		     (goto-char pos)
		     (when (and tramp-debug-message-fnh-function
				(search-backward
				 "tramp-file-name-handler"
				 (line-beginning-position) t))
		       (replace-match
			(symbol-name tramp-debug-message-fnh-function))
		       (tramp-debug-message-buttonize
			(line-beginning-position))))
		   ;; Link related messages.
		   (goto-char (point-max))
		   (tramp-debug-link-messages beg (line-beginning-position)))))))

       ;; No special messages.
       ,@body)))

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-message 'force)))

(provide 'tramp-message)
