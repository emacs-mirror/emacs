;;; debug.el --- debuggers and related commands for Emacs

;; Copyright (C) 1985, 1986, 1994, 2001 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: lisp, tools, maint

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

;; This is a major mode documented in the Emacs manual.

;;; Code:

(require 'button)

(defgroup debugger nil
  "Debuggers and related commands for Emacs."
  :prefix "debugger-"
  :group 'debug)

(defcustom debugger-mode-hook nil
  "*Hooks run when `debugger-mode' is turned on."
  :type 'hook
  :group 'debugger
  :version "20.3")

(defcustom debugger-batch-max-lines 40
  "*Maximum lines to show in debugger buffer in a noninteractive Emacs.
When the debugger is entered and Emacs is running in batch mode,
if the backtrace text has more than this many lines,
the middle is discarded, and just the beginning and end are displayed."
  :type 'integer
  :group 'debugger
  :version "21.1")

(defcustom debug-function-list nil
  "List of functions currently set for debug on entry."
  :type '(repeat function)
  :group 'debugger)

(defcustom debugger-step-after-exit nil
  "Non-nil means \"single-step\" after the debugger exits."
  :type 'boolean
  :group 'debugger)

(defvar debugger-value nil
  "This is the value for the debugger to return, when it returns.")

(defvar debugger-old-buffer nil
  "This is the buffer that was current when the debugger was entered.")

(defvar debugger-previous-backtrace nil
  "The contents of the previous backtrace (including text properties).
This is to optimize `debugger-make-xrefs'.")

(defvar debugger-outer-match-data)
(defvar debugger-outer-load-read-function)
(defvar debugger-outer-overriding-local-map)
(defvar debugger-outer-overriding-terminal-local-map)
(defvar debugger-outer-track-mouse)
(defvar debugger-outer-last-command)
(defvar debugger-outer-this-command)
(defvar debugger-outer-unread-command-char)
(defvar debugger-outer-unread-command-events)
(defvar debugger-outer-unread-post-input-method-events)
(defvar debugger-outer-last-input-event)
(defvar debugger-outer-last-command-event)
(defvar debugger-outer-last-nonmenu-event)
(defvar debugger-outer-last-event-frame)
(defvar debugger-outer-standard-input)
(defvar debugger-outer-standard-output)
(defvar debugger-outer-inhibit-redisplay)
(defvar debugger-outer-cursor-in-echo-area)

;;;###autoload
(setq debugger 'debug)
;;;###autoload
(defun debug (&rest debugger-args)
  "Enter debugger.  To return, type \\<debugger-mode-map>`\\[debugger-continue]'.
Arguments are mainly for use when this is called from the internals
of the evaluator.

You may call with no args, or you may pass nil as the first arg and
any other args you like.  In that case, the list of args after the
first will be printed into the backtrace buffer."
  (interactive)
  (unless noninteractive
    (message "Entering debugger..."))
  (let (debugger-value
	(debug-on-error nil)
	(debug-on-quit nil)
	(debugger-buffer (let ((default-major-mode 'fundamental-mode))
			   (get-buffer-create "*Backtrace*")))
	(debugger-old-buffer (current-buffer))
	(debugger-step-after-exit nil)
	;; Don't keep reading from an executing kbd macro!
	(executing-kbd-macro nil)
	;; Save the outer values of these vars for the `e' command
	;; before we replace the values.
	(debugger-outer-match-data (match-data))
	(debugger-outer-load-read-function load-read-function)
	(debugger-outer-overriding-local-map overriding-local-map)
	(debugger-outer-overriding-terminal-local-map
	 overriding-terminal-local-map)
	(debugger-outer-track-mouse track-mouse)
	(debugger-outer-last-command last-command)
	(debugger-outer-this-command this-command)
	(debugger-outer-unread-command-char unread-command-char)
	(debugger-outer-unread-command-events unread-command-events)
	(debugger-outer-unread-post-input-method-events
	 unread-post-input-method-events)
	(debugger-outer-last-input-event last-input-event)
	(debugger-outer-last-command-event last-command-event)
	(debugger-outer-last-nonmenu-event last-nonmenu-event)
	(debugger-outer-last-event-frame last-event-frame)
	(debugger-outer-standard-input standard-input)
	(debugger-outer-standard-output standard-output)
	(debugger-outer-inhibit-redisplay inhibit-redisplay)
	(debugger-outer-cursor-in-echo-area cursor-in-echo-area))
    ;; Set this instead of binding it, so that `q'
    ;; will not restore it.
    (setq overriding-terminal-local-map nil)
    ;; Don't let these magic variables affect the debugger itself.
    (let ((last-command nil) this-command track-mouse
	  (unread-command-char -1) unread-command-events
	  unread-post-input-method-events
	  last-input-event last-command-event last-nonmenu-event
	  last-event-frame
	  overriding-local-map
	  load-read-function
	  ;; If we are inside a minibuffer, allow nesting
	  ;; so that we don't get an error from the `e' command.
	  (enable-recursive-minibuffers
	   (or enable-recursive-minibuffers (> (minibuffer-depth) 0)))
	  (standard-input t) (standard-output t)
	  inhibit-redisplay
	  (cursor-in-echo-area nil))
      (unwind-protect
	  (save-excursion
	    (save-window-excursion
	      (pop-to-buffer debugger-buffer)
	      (debugger-mode)
	      (debugger-setup-buffer debugger-args)
	      (when noninteractive
		;; If the backtrace is long, save the beginning
		;; and the end, but discard the middle.
		(when (> (count-lines (point-min) (point-max))
			 debugger-batch-max-lines)
		  (goto-char (point-min))
		  (forward-line (/ 2 debugger-batch-max-lines))
		  (let ((middlestart (point)))
		    (goto-char (point-max))
		    (forward-line (- (/ 2 debugger-batch-max-lines)
				     debugger-batch-max-lines))
		    (delete-region middlestart (point)))
		  (insert "...\n"))
		(goto-char (point-min))
		(message (buffer-string))
		(kill-emacs))
	      (if (eq (car debugger-args) 'debug)
		  ;; Skip the frames for backtrace-debug, byte-code, and debug.
		  (backtrace-debug 3 t))
	      (debugger-reenable)
	      (message "")
	      (let ((inhibit-trace t)
		    (standard-output nil)
		    (buffer-read-only t))
		(message "")
		;; Make sure we unbind buffer-read-only in the right buffer.
		(save-excursion
		  (recursive-edit)))))
	;; Kill or at least neuter the backtrace buffer, so that users
	;; don't try to execute debugger commands in an invalid context.
	(if (get-buffer-window debugger-buffer 'visible)
	    ;; Still visible despite the save-window-excursion?  Maybe it
	    ;; it's in a pop-up frame.  It would be annoying to delete and
	    ;; recreate it every time the debugger stops, so instead we'll
	    ;; erase it but leave it visible.
	    (save-excursion
	      (set-buffer debugger-buffer)
	      (erase-buffer)
	      (fundamental-mode))
	  (kill-buffer debugger-buffer))
	(set-match-data debugger-outer-match-data)))
    ;; Put into effect the modified values of these variables
    ;; in case the user set them with the `e' command.
    (setq load-read-function debugger-outer-load-read-function)
    (setq overriding-local-map debugger-outer-overriding-local-map)
    (setq overriding-terminal-local-map
	  debugger-outer-overriding-terminal-local-map)
    (setq track-mouse debugger-outer-track-mouse)
    (setq last-command debugger-outer-last-command)
    (setq this-command debugger-outer-this-command)
    (setq unread-command-char debugger-outer-unread-command-char)
    (setq unread-command-events debugger-outer-unread-command-events)
    (setq unread-post-input-method-events
	  debugger-outer-unread-post-input-method-events)
    (setq last-input-event debugger-outer-last-input-event)
    (setq last-command-event debugger-outer-last-command-event)
    (setq last-nonmenu-event debugger-outer-last-nonmenu-event)
    (setq last-event-frame debugger-outer-last-event-frame)
    (setq standard-input debugger-outer-standard-input)
    (setq standard-output debugger-outer-standard-output)
    (setq inhibit-redisplay debugger-outer-inhibit-redisplay)
    (setq cursor-in-echo-area debugger-outer-cursor-in-echo-area)
    (setq debug-on-next-call debugger-step-after-exit)
    debugger-value))

(defun debugger-setup-buffer (debugger-args)
  "Initialize the `*Backtrace*' buffer for entry to the debugger.
That buffer should be current already."
  (setq buffer-read-only nil)
  (erase-buffer)
  (set-buffer-multibyte nil)
  (let ((standard-output (current-buffer))
	(print-escape-newlines t)
	(print-level 8)
	(print-length 50))
    (backtrace))
  (goto-char (point-min))
  (delete-region (point)
		 (progn
		   (search-forward "\n  debug(")
		   (forward-line 1)
		   (point)))
  (insert "Debugger entered")
  ;; lambda is for debug-on-call when a function call is next.
  ;; debug is for debug-on-entry function called.
  (cond ((memq (car debugger-args) '(lambda debug))
	 (insert "--entering a function:\n")
	 (if (eq (car debugger-args) 'debug)
	     (progn
	       (delete-char 1)
	       (insert ?*)
	       (beginning-of-line))))
	;; Exiting a function.
	((eq (car debugger-args) 'exit)
	 (insert "--returning value: ")
	 (setq debugger-value (nth 1 debugger-args))
	 (prin1 debugger-value (current-buffer))
	 (insert ?\n)
	 (delete-char 1)
	 (insert ? )
	 (beginning-of-line))
	;; Debugger entered for an error.
	((eq (car debugger-args) 'error)
	 (insert "--Lisp error: ")
	 (prin1 (nth 1 debugger-args) (current-buffer))
	 (insert ?\n))
	;; debug-on-call, when the next thing is an eval.
	((eq (car debugger-args) t)
	 (insert "--beginning evaluation of function call form:\n"))
	;; User calls debug directly.
	(t
	 (insert ": ")
	 (prin1 (if (eq (car debugger-args) 'nil)
		    (cdr debugger-args) debugger-args)
		(current-buffer))
	 (insert ?\n)))
  ;; After any frame that uses eval-buffer,
  ;; insert a line that states the buffer position it's reading at.
  (save-excursion
    (while (re-search-forward "^  eval-buffer(" nil t)
      (end-of-line)
      (insert (format "\n  ;;; Reading at buffer position %d"
		      (with-current-buffer (nth 2 (backtrace-frame (debugger-frame-number)))
			(point))))))
  (debugger-make-xrefs))

(defun debugger-make-xrefs (&optional buffer)
  "Attach cross-references to symbol names in the `*Backtrace*' buffer."
  (interactive "b")
  (save-excursion
    (set-buffer (or buffer (current-buffer)))
    (setq buffer (current-buffer))
    (let ((buffer-read-only nil)
	  (old-end 1) (new-end 1))
      ;; If we saved an old backtrace, find the common part
      ;; between the new and the old.
      ;; Compare line by line, starting from the end,
      ;; because that's the part that is likely to be unchanged.
      (if debugger-previous-backtrace
	  (let (old-start new-start (all-match t))
	    (goto-char (point-max))
	    (with-temp-buffer
	      (insert debugger-previous-backtrace)
	      (while (and all-match (not (bobp)))
		(setq old-end (point))
		(forward-line -1)
		(setq old-start (point))
		(with-current-buffer buffer
		  (setq new-end (point))
		  (forward-line -1)
		  (setq new-start (point)))
		(if (not (zerop
			  (compare-buffer-substrings
			   (current-buffer) old-start old-end
			   buffer new-start new-end)))
		    (setq all-match nil))))
	    ;; Now new-end is the position of the start of the
	    ;; unchanged part in the current buffer, and old-end is
	    ;; the position of that same text in the saved old
	    ;; backtrace.  But we must subtract 1 since strings are
	    ;; indexed in origin 0.

	    ;; Replace the unchanged part of the backtrace
	    ;; with the text from debugger-previous-backtrace,
	    ;; since that already has the proper xrefs.
	    ;; With this optimization, we only need to scan
	    ;; the changed part of the backtrace.
	    (delete-region new-end (point-max))
	    (goto-char (point-max))
	    (insert (substring debugger-previous-backtrace (1- old-end)))
	    ;; Make the unchanged part of the backtrace inaccessible
	    ;; so it won't be scanned.
	    (narrow-to-region (point-min) new-end)))
	    
      ;; Scan the new part of the backtrace, inserting xrefs.
      (goto-char (point-min))
      (while (progn
	       (skip-syntax-forward "^w_")
	       (not (eobp)))
	(let* ((beg (point))
	       (end (progn (skip-syntax-forward "w_") (point)))
	       (sym (intern-soft (buffer-substring-no-properties
				  beg end)))
	       (file (and sym (symbol-file sym))))
	  (when file
	    (goto-char beg)
	    ;; help-xref-button needs to operate on something matched
	    ;; by a regexp, so set that up for it.
	    (re-search-forward "\\(\\(\\sw\\|\\s_\\)+\\)")
	    (help-xref-button 1 'help-function-def sym file)))
	(forward-line 1))
      (widen))
    (setq debugger-previous-backtrace (buffer-string))))

(defun debugger-step-through ()
  "Proceed, stepping through subexpressions of this expression.
Enter another debugger on next entry to eval, apply or funcall."
  (interactive)
  (setq debugger-step-after-exit t)
  (message "Proceeding, will debug on next eval or call.")
  (exit-recursive-edit))

(defun debugger-continue ()
  "Continue, evaluating this expression without stopping."
  (interactive)
  (unless debugger-may-continue
    (error "Cannot continue"))
  (message "Continuing.")
  (exit-recursive-edit))

(defun debugger-return-value (val)
  "Continue, specifying value to return.
This is only useful when the value returned from the debugger
will be used, such as in a debug on exit from a frame."
  (interactive "XReturn value (evaluated): ")
  (setq debugger-value val)
  (princ "Returning " t)
  (prin1 debugger-value)
  (exit-recursive-edit))

(defun debugger-jump ()
  "Continue to exit from this frame, with all debug-on-entry suspended."
  (interactive)
  (debugger-frame)
  ;; Turn off all debug-on-entry functions
  ;; but leave them in the list.
  (let ((list debug-function-list))
    (while list
      (fset (car list)
	    (debug-on-entry-1 (car list) (symbol-function (car list)) nil))
      (setq list (cdr list))))
  (message "Continuing through this frame")
  (exit-recursive-edit))

(defun debugger-reenable ()
  "Turn all debug-on-entry functions back on."
  (let ((list debug-function-list))
    (while list
      (or (consp (symbol-function (car list)))
	  (debug-convert-byte-code (car list)))
      (fset (car list)
	    (debug-on-entry-1 (car list) (symbol-function (car list)) t))
      (setq list (cdr list)))))

(defun debugger-frame-number ()
  "Return number of frames in backtrace before the one point points at."
  (save-excursion
    (beginning-of-line)
    (let ((opoint (point))
	  (count 0))
      (while (not (eq (cadr (backtrace-frame count)) 'debug))
	(setq count (1+ count)))
      (goto-char (point-min))
      (if (or (equal (buffer-substring (point) (+ (point) 6))
		     "Signal")
	      (equal (buffer-substring (point) (+ (point) 6))
		     "Return"))
	  (progn
	    (search-forward ":")
	    (forward-sexp 1)))
      (forward-line 1)
      (while (progn
	       (forward-char 2)
	       (if (= (following-char) ?\()
		   (forward-sexp 1)
		 (forward-sexp 2))
	       (forward-line 1)
	       (<= (point) opoint))
	(if (looking-at " *;;;")
	    (forward-line 1))
	(setq count (1+ count)))
      count)))

(defun debugger-frame ()
  "Request entry to debugger when this frame exits.
Applies to the frame whose line point is on in the backtrace."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at " *;;;\\|[a-z]")
	(error "This line is not a function call")))
  (beginning-of-line)
  (backtrace-debug (debugger-frame-number) t)
  (if (= (following-char) ? )
      (let ((buffer-read-only nil))
	(delete-char 1)
	(insert ?*)))
  (beginning-of-line))

(defun debugger-frame-clear ()
  "Do not enter debugger when this frame exits.
Applies to the frame whose line point is on in the backtrace."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at " *;;;\\|[a-z]")
	(error "This line is not a function call")))
  (beginning-of-line)
  (backtrace-debug (debugger-frame-number) nil)
  (if (= (following-char) ?*)
      (let ((buffer-read-only nil))
	(delete-char 1)
	(insert ? )))
  (beginning-of-line))



(put 'debugger-env-macro 'lisp-indent-function 0)
(defmacro debugger-env-macro (&rest body)
  "Run BODY in original environment."
  `(save-excursion
    (if (null (buffer-name debugger-old-buffer))
        ;; old buffer deleted
        (setq debugger-old-buffer (current-buffer)))
    (set-buffer debugger-old-buffer)
    (let ((load-read-function debugger-outer-load-read-function)
          (overriding-terminal-local-map
           debugger-outer-overriding-terminal-local-map)
          (overriding-local-map debugger-outer-overriding-local-map)
          (track-mouse debugger-outer-track-mouse)
          (last-command debugger-outer-last-command)
          (this-command debugger-outer-this-command)
          (unread-command-char debugger-outer-unread-command-char)
          (unread-command-events debugger-outer-unread-command-events)
          (unread-post-input-method-events
           debugger-outer-unread-post-input-method-events)
          (last-input-event debugger-outer-last-input-event)
          (last-command-event debugger-outer-last-command-event)
          (last-nonmenu-event debugger-outer-last-nonmenu-event)
          (last-event-frame debugger-outer-last-event-frame)
          (standard-input debugger-outer-standard-input)
          (standard-output debugger-outer-standard-output)
          (inhibit-redisplay debugger-outer-inhibit-redisplay)
          (cursor-in-echo-area debugger-outer-cursor-in-echo-area))
      (set-match-data debugger-outer-match-data)
      (prog1 (progn ,@body)
        (setq debugger-outer-match-data (match-data))
        (setq debugger-outer-load-read-function load-read-function)
        (setq debugger-outer-overriding-terminal-local-map
              overriding-terminal-local-map)
        (setq debugger-outer-overriding-local-map overriding-local-map)
        (setq debugger-outer-track-mouse track-mouse)
        (setq debugger-outer-last-command last-command)
        (setq debugger-outer-this-command this-command)
        (setq debugger-outer-unread-command-char unread-command-char)
        (setq debugger-outer-unread-command-events unread-command-events)
        (setq debugger-outer-unread-post-input-method-events
              unread-post-input-method-events)
        (setq debugger-outer-last-input-event last-input-event)
        (setq debugger-outer-last-command-event last-command-event)
        (setq debugger-outer-last-nonmenu-event last-nonmenu-event)
        (setq debugger-outer-last-event-frame last-event-frame)
        (setq debugger-outer-standard-input standard-input)
        (setq debugger-outer-standard-output standard-output)
        (setq debugger-outer-inhibit-redisplay inhibit-redisplay)
        (setq debugger-outer-cursor-in-echo-area cursor-in-echo-area)
        ))))

(defun debugger-eval-expression (exp)
  "Eval an expression, in an environment like that outside the debugger."
  (interactive
   (list (read-from-minibuffer "Eval: "
			       nil read-expression-map t
			       'read-expression-history)))
  (debugger-env-macro (eval-expression exp)))

(defvar debugger-mode-map nil)
(unless debugger-mode-map
  (let ((loop ? ))
    (setq debugger-mode-map (make-keymap))
    (set-keymap-parent debugger-mode-map button-buffer-map)
    (suppress-keymap debugger-mode-map)
    (define-key debugger-mode-map "-" 'negative-argument)
    (define-key debugger-mode-map "b" 'debugger-frame)
    (define-key debugger-mode-map "c" 'debugger-continue)
    (define-key debugger-mode-map "j" 'debugger-jump)
    (define-key debugger-mode-map "r" 'debugger-return-value)
    (define-key debugger-mode-map "u" 'debugger-frame-clear)
    (define-key debugger-mode-map "d" 'debugger-step-through)
    (define-key debugger-mode-map "l" 'debugger-list-functions)
    (define-key debugger-mode-map "h" 'describe-mode)
    (define-key debugger-mode-map "q" 'top-level)
    (define-key debugger-mode-map "e" 'debugger-eval-expression)
    (define-key debugger-mode-map " " 'next-line)
    (define-key debugger-mode-map "R" 'debugger-record-expression)
    (define-key debugger-mode-map "\C-m" 'help-follow)
    (define-key debugger-mode-map [mouse-2] 'push-button)
    ))


(defcustom debugger-record-buffer "*Debugger-record*"
  "*Buffer name for expression values, for \\[debugger-record-expression]."
  :type 'string
  :group 'debugger
  :version "20.3")

(defun debugger-record-expression  (exp)
  "Display a variable's value and record it in `*Backtrace-record*' buffer."
  (interactive
   (list (read-from-minibuffer
	  "Record Eval: "
	  nil
	  read-expression-map t
	  'read-expression-history)))
  (let* ((buffer (get-buffer-create debugger-record-buffer))
	 (standard-output buffer))
    (princ (format "Debugger Eval (%s): " exp))
    (princ (debugger-eval-expression exp))
    (terpri))

  (with-current-buffer (get-buffer debugger-record-buffer)
    (save-excursion
      (forward-line -1)
      (message
       (buffer-substring (point) (progn (end-of-line) (point)))))))

(put 'debugger-mode 'mode-class 'special)

(defun debugger-mode ()
  "Mode for backtrace buffers, selected in debugger.
\\<debugger-mode-map>
A line starts with `*' if exiting that frame will call the debugger.
Type \\[debugger-frame] or \\[debugger-frame-clear] to set or remove the `*'.

When in debugger due to frame being exited,
use the \\[debugger-return-value] command to override the value
being returned from that frame.

Use \\[debug-on-entry] and \\[cancel-debug-on-entry] to control
which functions will enter the debugger when called.

Complete list of commands:
\\{debugger-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'debugger-mode)
  (setq mode-name "Debugger")
  (setq truncate-lines t)
  (set-syntax-table emacs-lisp-mode-syntax-table)
  (use-local-map debugger-mode-map)
  (run-hooks 'debugger-mode-hook))

;;;###autoload
(defun debug-on-entry (function)
  "Request FUNCTION to invoke debugger each time it is called.
If you tell the debugger to continue, FUNCTION's execution proceeds.
This works by modifying the definition of FUNCTION,
which must be written in Lisp, not predefined.
Use \\[cancel-debug-on-entry] to cancel the effect of this command.
Redefining FUNCTION also cancels it."
  (interactive "aDebug on entry (to function): ")
  (debugger-reenable)
  ;; Handle a function that has been aliased to some other function.
  (if (symbolp (symbol-function function))
      (fset function `(lambda (&rest debug-on-entry-args)
			(apply ',(symbol-function function)
			       debug-on-entry-args))))
  (if (subrp (symbol-function function))
      (error "Function %s is a primitive" function))
  (or (consp (symbol-function function))
      (debug-convert-byte-code function))
  (or (consp (symbol-function function))
      (error "Definition of %s is not a list" function))
  (fset function (debug-on-entry-1 function (symbol-function function) t))
  (or (memq function debug-function-list)
      (setq debug-function-list (cons function debug-function-list)))
  function)

;;;###autoload
(defun cancel-debug-on-entry (&optional function)
  "Undo effect of \\[debug-on-entry] on FUNCTION.
If argument is nil or an empty string, cancel for all functions."
  (interactive
   (list (let ((name
		(completing-read "Cancel debug on entry (to function): "
				 ;; Make an "alist" of the functions
				 ;; that now have debug on entry.
				 (mapcar 'list
					 (mapcar 'symbol-name
						 debug-function-list))
				 nil t nil)))
	   (if name (intern name)))))
  (debugger-reenable)
  (if (and function (not (string= function "")))
      (progn
	(fset function
	      (debug-on-entry-1 function (symbol-function function) nil))
	(setq debug-function-list (delq function debug-function-list))
	function)
    (message "Cancelling debug-on-entry for all functions")
    (mapcar 'cancel-debug-on-entry debug-function-list)))

(defun debug-convert-byte-code (function)
  (let ((defn (symbol-function function)))
    (if (not (consp defn))
	;; Assume a compiled code object.
	(let* ((contents (append defn nil))
	       (body
		(list (list 'byte-code (nth 1 contents)
			    (nth 2 contents) (nth 3 contents)))))
	  (if (nthcdr 5 contents)
	      (setq body (cons (list 'interactive (nth 5 contents)) body)))
	  (if (nth 4 contents)
	      ;; Use `documentation' here, to get the actual string,
	      ;; in case the compiled function has a reference
	      ;; to the .elc file.
	      (setq body (cons (documentation function) body)))
	  (fset function (cons 'lambda (cons (car contents) body)))))))

(defun debug-on-entry-1 (function defn flag)
  (if (subrp defn)
      (error "%s is a built-in function" function)
    (if (eq (car defn) 'macro)
	(debug-on-entry-1 function (cdr defn) flag)
      (or (eq (car defn) 'lambda)
	  (error "%s not user-defined Lisp function" function))
      (let (tail prec)
	(if (stringp (car (nthcdr 2 defn)))
	    (setq tail (nthcdr 3 defn)
		  prec (list (car defn) (car (cdr defn))
			     (car (cdr (cdr defn)))))
	  (setq tail (nthcdr 2 defn)
		prec (list (car defn) (car (cdr defn)))))
	(if (eq flag (equal (car tail) '(debug 'debug)))
	    defn
	  (if flag
	      (nconc prec (cons '(debug 'debug) tail))
	    (nconc prec (cdr tail))))))))

(defun debugger-list-functions ()
  "Display a list of all the functions now set to debug on entry."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (if (null debug-function-list)
	(princ "No debug-on-entry functions now\n")
      (princ "Functions set to debug on entry:\n\n")
      (let ((list debug-function-list))
	(while list
	  (prin1 (car list))
	  (terpri)
	  (setq list (cdr list))))
      (princ "Note: if you have redefined a function, then it may no longer\n")
      (princ "be set to debug on entry, even if it is in the list."))
    (save-excursion
      (set-buffer standard-output)
      (help-mode))))

(provide 'debug)

;;; debug.el ends here
