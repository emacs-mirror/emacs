;;; -*-Emacs-Lisp-*- General command interpreter in a window stuff
;;; Copyright (C) 1989 Free Software Foundation, Inc.
;;; Original author: Olin Shivers <olin.shivers@cs.cmu.edu>  Aug 1988

;;; This file is part of GNU Emacs.

;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.

;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; This file defines a general command-interpreter-in-a-buffer package
;;; (comint mode). The idea is that you can build specific process-in-a-buffer
;;; modes on top of comint mode -- e.g., lisp, shell, scheme, T, soar, ....
;;; This way, all these specific packages share a common base functionality, 
;;; and a common set of bindings, which makes them easier to use (and
;;; saves code, implementation time, etc., etc.).

;;; For documentation on the functionality provided by comint mode, and
;;; the hooks available for customising it, see the comments below.
;;; For further information on the standard derived modes (shell, 
;;; inferior-lisp, inferior-scheme, ...), see the relevant source files.

;;; For hints on converting existing process modes to use comint-mode
;;; instead of shell-mode, see the notes at the end of this file.

(require 'history)
(provide 'comint)
(defconst comint-version "2.01")


;;; Not bound by default in comint-mode
;;; send-invisible			Read a line w/o echo, and send to proc
;;; (These are bound in shell-mode)
;;; comint-dynamic-complete		Complete filename at point.
;;; comint-dynamic-list-completions	List completions in help buffer.
;;; comint-replace-by-expanded-filename	Expand and complete filename at point;
;;;					replace with expanded/completed name.
(defvar comint-mode-map nil)

(if comint-mode-map
    nil
  (setq comint-mode-map (make-sparse-keymap))
  (define-key comint-mode-map "\C-a" 'comint-bol)
  (define-key comint-mode-map "\C-d" 'comint-delchar-or-maybe-eof)
  (define-key comint-mode-map "\C-m" 'comint-send-input)
  (define-key comint-mode-map "\M-p" 'comint-previous-input)
  (define-key comint-mode-map "\M-n" 'comint-next-input)
  (define-key comint-mode-map "\M-s" 'comint-previous-similar-input)
  (define-key comint-mode-map "\C-c\C-c" 'comint-interrupt-subjob) ; tty ^C
  (define-key comint-mode-map "\C-c\C-f" 'comint-continue-subjob)  ; shell "fg"
  (define-key comint-mode-map "\C-c\C-l" 'comint-show-output)
  (define-key comint-mode-map "\C-c\C-o" 'comint-flush-output)     ; tty ^O
  (define-key comint-mode-map "\C-c\C-r" 'comint-history-search-backward)
  (define-key comint-mode-map "\C-c\C-s" 'comint-history-search-forward)
  (define-key comint-mode-map "\C-c\C-u" 'comint-kill-input)       ; tty ^U
  (define-key comint-mode-map "\C-c\C-w" 'backward-kill-word)      ; tty ^W
  (define-key comint-mode-map "\C-c\C-z" 'comint-stop-subjob)      ; tty ^Z
  (define-key comint-mode-map "\C-c\C-\\" 'comint-quit-subjob))    ; tty ^\

;;; Buffer Local Variables:
;;;============================================================================
;;; Comint mode buffer local variables:
;;;     comint-prompt-regexp    - string       comint-bol uses to match prompt.
;;;     comint-last-input-end   - marker       For comint-flush-output command
;;;     input-ring-size         - integer      For the input history
;;;     input-ring              - ring             mechanism
;;;     input-ring-index        - marker           ...
;;;     comint-last-input-match - string           ...
;;;     comint-get-old-input    - function     Hooks for specific 
;;;     comint-input-sentinel   - function         process-in-a-buffer
;;;     comint-input-filter     - function         modes.
;;;     comint-input-send	- function
;;;     comint-eol-on-send	- boolean

(make-variable-buffer-local
 (defvar comint-prompt-regexp "^"
  "*Regexp to recognise prompts in the inferior process.  Defaults to \"^\".

Good choices:
  Canonical Lisp: \"^[^> \n]*>+:? *\" (Lucid, Franz, KCL, T, cscheme, oaklisp)
  Lucid Common Lisp: \"^\\(>\\|\\(->\\)+\\) *\"
  Franz: \"^\\(->\\|<[0-9]*>:\\) *\"
  KCL and T: \"^>+ *\"
  shell: \"^[^#$%>\n]*[#$%>] *\"

This is a good thing to set in mode hooks."))

(make-variable-buffer-local
 (defvar input-ring-size 30 "Size of input history ring."))

;;; Here are the per-interpreter hooks.
(make-variable-buffer-local
 (defvar comint-get-old-input (function comint-get-old-input-default)
   "Function that submits old text in comint mode.
This function is called when return is typed while the point is in old text.
It returns the text to be submitted as process input.  The default is
comint-get-old-input-default, which grabs the current line, and strips off
leading text matching comint-prompt-regexp."))

(make-variable-buffer-local
 (defvar comint-input-sentinel (function ignore)
   "Called on each input submitted to comint mode process by comint-send-input.
Thus it can, for instance, track cd/pushd/popd commands issued to the csh."))

(make-variable-buffer-local
 (defvar comint-input-filter
   (function (lambda (str) (not (string-match "\\`\\s *\\'" str))))
   "Predicate for filtering additions to input history.
Only inputs answering true to this function are saved on the input
history list. Default is to save anything that isn't all whitespace"))

(defvar comint-mode-hook '()
  "Called upon entry into comint-mode")

(defun comint-mode ()
  "Major mode for interacting with an inferior interpreter.
Interpreter name is same as buffer name, sans the asterisks.
Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.

This mode is typically customised to create inferior-lisp-mode,
shell-mode, et cetera.  This can be done by setting the hooks
comint-input-sentinel, comint-input-filter, and comint-get-old-input
to appropriate functions, and the variable comint-prompt-regexp
to the appropriate regular expression.

An input history is maintained of size input-ring-size, and
can be accessed with the commands comint-next-input [\\[comint-next-input]] and 
comint-previous-input [\\[comint-previous-input]]. Commands not keybound by
default are send-invisible, comint-dynamic-complete, and 
comint-list-dynamic-completions.

If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it.

\\{comint-mode-map}

Entry to this mode runs the hooks on comint-mode-hook."
  (interactive)
  (make-local-variable 'input-ring)
  (put 'input-ring 'preserved t)
  (kill-all-local-variables)
  (setq major-mode 'comint-mode
        mode-name "Comint"
        mode-line-process '(": %s"))
  (use-local-map comint-mode-map)
  (set (make-local-variable 'comint-last-input-match) "")
  (set (make-local-variable 'comint-last-similar--string) "")
  (set (make-local-variable 'input-ring-index) 0)
  (set (make-local-variable 'comint-last-input-end) (make-marker))
  (set-marker comint-last-input-end (point-max))
  (run-hooks 'comint-mode-hook))

(defun comint-check-proc (buffer-name)
  "True if there is a running or stopped process associated with BUFFER."
  (let ((proc (get-buffer-process buffer-name)))
    (and proc (memq (process-status proc) '(run stop)))))

(defun comint-mark ()
  ;; Returns the process-mark of the current-buffer
  (process-mark (get-buffer-process (current-buffer))))

;;; Note that this guy, unlike shell.el's make-shell, barfs if you pass it ()
;;; for the second argument (program).
(defun make-comint (name program &optional startfile &rest switches)
  (let* ((buffer (get-buffer-create (concat "*" name "*")))
	 (proc (get-buffer-process buffer)))
    ;; If no process, or nuked process, crank up a new one and put buffer in
    ;; comint mode. Otherwise, leave buffer and existing process alone.
    (cond ((not (comint-check-proc))
           (save-excursion
	     (set-buffer buffer)
	     (comint-mode)) ; Install local vars, mode, keymap, ...
	   (comint-exec buffer name program startfile switches)))
    buffer))

(defun comint-exec (buffer name command startfile switches)
  "Fires up a process in buffer for comint modes.
Blasts any old process running in the buffer.  Doesn't set the buffer mode.
You can use this to cheaply run a series of processes in the same buffer."
  (or command (error "No program for comint process"))
  (save-excursion
    (set-buffer buffer)
    (let ((proc (get-buffer-process buffer)))	; Blast any old process.
      (if proc (delete-process proc)))
    ;; Crank up a new process
    (let ((proc (comint-exec-1 name buffer command switches)))
      ;; Jump to the end, and set the process mark.
      (set-marker (comint-mark) (goto-char (point-max)))
      ;; Feed it the startfile.
      (cond (startfile
	     ;;This is guaranteed to wait long enough
	     ;;but has bad results if the comint does not prompt at all
	     ;;	     (while (= size (buffer-size))
	     ;;	       (sleep-for 1))
	     ;;I hope 1 second is enough!
	     (sleep-for 1)
	     (goto-char (point-max))
	     (insert-file-contents startfile)
	     (setq startfile (buffer-substring (point) (point-max)))
	     (delete-region (point) (point-max))
	     (comint-send-string proc startfile)))
    buffer))

;;; This auxiliary function cranks up the process for comint-exec in
;;; the appropriate environment. It is twice as long as it should be
;;; because emacs has two distinct mechanisms for manipulating the
;;; process environment, selected at compile time with the
;;; MAINTAIN-ENVIRONMENT #define. In one case, process-environment
;;; is bound; in the other it isn't.

(defun comint-exec-1 (name buffer command switches)
  (if (boundp 'process-environment) ; Not a completely reliable test.
      (let ((process-environment
	     (comint-update-env process-environment
				(list (format "TERMCAP=emacs:co#%d:tc=unknown"
					      (screen-width))
				      "TERM=emacs"
				      "EMACS=t"))))
	(apply 'start-process name buffer command switches))
      (let ((tcapv (getenv "TERMCAP"))
	    (termv (getenv "TERM"))
	    (emv   (getenv "EMACS")))
	(unwind-protect
	     (progn (setenv "TERMCAP" (format "emacs:co#%d:tc=unknown"
					      (screen-width)))
		    (setenv "TERM" "emacs")
		    (setenv "EMACS" "t")
		    (apply 'start-process name buffer command switches))
	  (setenv "TERMCAP" tcapv)
	  (setenv "TERM"    termv)
	  (setenv "EMACS"   emv)))))

;; This is just (append new old-env) that compresses out shadowed entries.
;; It's also pretty ugly, mostly due to elisp's horrible iteration structures.
(defun comint-update-env (old-env new)
  (let ((ans (reverse new))
	(vars (mapcar (function (lambda (vv)
			(and (string-match "^[^=]*=" vv)
			     (substring vv 0 (match-end 0)))))
		      new)))
    (while old-env
      (let* ((vv (car old-env)) ; vv is var=value
	     (var (and (string-match "^[^=]*=" vv)
		       (substring vv 0 (match-end 0)))))
	(setq old-env (cdr old-env))
	(cond ((not (and var (member var vars)))
	       (if var (setq var (cons var vars)))
	       (setq ans (cons vv ans))))))
    (nreverse ans)))

;;; Input history retrieval commands
;;; M-p -- previous input    M-n -- next input
;;; C-c r -- previous input matching
;;; ===========================================================================

(defun comint-previous-input (arg)
  "Cycle backwards through input history."
  (interactive "*p")
  (let ((len (ring-length input-ring)))
    (if (<= len 0) (error "Empty input ring"))
    (if (< (point) (comint-mark))
        (delete-region (comint-mark) (goto-char (point-max))))
    (cond ((eq last-command 'comint-previous-input)
           (delete-region (mark) (point)))
          ((eq last-command 'comint-previous-similar-input)
           (delete-region (comint-mark) (point)))
          (t                          
           (setq input-ring-index
                 (if (> arg 0) -1
                   (if (< arg 0) 1 0)))
           (push-mark (point))))
    (setq input-ring-index (comint-mod (+ input-ring-index arg) len))
    (message "%d" (1+ input-ring-index))
    (insert (ring-ref input-ring input-ring-index))
    (setq this-command 'comint-previous-input)))

(defun comint-next-input (arg)
  "Cycle forwards through input history."
  (interactive "*p")
  (comint-previous-input (- arg)))

(defun comint-previous-input-matching (str)
  "Searches backwards through input history for substring match."
  (interactive (let* ((last-command last-command) ; preserve around r-f-m
		      (s (read-from-minibuffer 
			 (format "Command substring (default %s): "
				 comint-last-input-match))))
		 (list (if (string= s "") comint-last-input-match s))))
; (interactive "sCommand substring: ")
  (setq comint-last-input-match str) ; update default
  (if (not (eq last-command 'comint-previous-input))
      (setq input-ring-index -1))
  (let ((str (regexp-quote str))
        (len (ring-length input-ring))
	(n (+ input-ring-index 1)))
    (while (and (< n len) (not (string-match str (ring-ref input-ring n))))
      (setq n (+ n 1)))
    (cond ((< n len)
	   (comint-previous-input (- n input-ring-index)))
	  (t (if (eq last-command 'comint-previous-input) 
		 (setq this-command 'comint-previous-input))
             (error "Not found")))))

;;;
;;; Similar input -- contributed by ccm and highly winning.
;;;
;;; Reenter input, removing back to the last insert point if it exists. 
;;;
(defun comint-previous-similar-input (arg)
  "Reenters the last input that matches the string typed so far.  If repeated 
successively older inputs are reentered.  If arg is 1, it will go back
in the history, if -1 it will go forward."
  (interactive "p")
  (if (< (point) (comint-mark))
      (error "Not after process mark"))
  (if (not (eq last-command 'comint-previous-similar-input))
      (setq input-ring-index -1
	    comint-last-similar-string
            (buffer-substring (comint-mark) (point))))
  (let* ((size (length comint-last-similar-string))
	 (len (ring-length input-ring))
	 (n (+ input-ring-index arg))
	 entry)
    (while (and (< n len) 
		(or (< (length (setq entry (ring-ref input-ring n))) size)
		    (not (equal comint-last-similar-string 
				(substring entry 0 size)))))
      (setq n (+ n arg)))
    (cond ((< n len)
	   (setq input-ring-index n)
	   (if (eq last-command 'comint-previous-similar-input)
	       (delete-region (comint-mark) (point)))
	   (insert (substring entry size)))
	  (t (error "Not found")))))

(defun comint-send-input (&optional terminator delete)
  "Send input to process, followed by a linefeed or optional TERMINATOR.
After the process output mark, sends all text from the process mark to
end of buffer as input to the process.  Before the process output mark, calls
value of variable comint-get-old-input to retrieve old input, replaces it in
the input region (from the end of process output to the end of the buffer) and
then sends it.  In either case, the value of variable comint-input-sentinel is
called on the input before sending it.  The input is entered into the input
history ring, if value of variable comint-input-filter returns non-nil when
called on the input.

If optional second argument DELETE is non-nil, then the input is deleted from
the end of the buffer.  This is useful if the process unconditionally echoes
input.  Processes which use TERMINATOR or DELETE should have a command wrapper
which provides them bound to RET; see telnet.el for an example.

comint-get-old-input, comint-input-sentinel, and comint-input-filter are chosen
according to the command interpreter running in the buffer.  For example,

If the interpreter is the csh,
 comint-get-old-input defaults:  takes the current line, discard any
   initial string matching regexp comint-prompt-regexp.
 comint-input-sentinel:  monitors input for \"cd\", \"pushd\", and \"popd\" 
   commands.  When it sees one, it changes the default directory of the buffer.
 comint-input-filter defaults:  returns t if the input isn't all whitespace.

If the comint is Lucid Common Lisp, 
 comint-get-old-input:  snarfs the sexp ending at point.
 comint-input-sentinel:  does nothing.
 comint-input-filter:  returns nil if the input matches input-filter-regexp,
        which matches (1) all whitespace (2) :a, :c, etc.

Similar functions are used for other process modes."
  (interactive)
  ;; Note that the input string does not include its terminal newline.
  (if (not (get-buffer-process (current-buffer)))
      (error "Current buffer has no process")
    (let* ((pmark (comint-mark))
           (input (if (>= (point) pmark)
                      (buffer-substring pmark (goto-char (point-max)))
                    (let ((copy (funcall comint-get-old-input)))
                      (delete-region pmark (goto-char (point-max)))
                      (insert copy)
                      copy))))
      (set-marker comint-last-input-end (point))
      (setq input-ring-index 0)
      (if (funcall comint-input-filter input) (ring-insert input-ring input))
      (funcall comint-input-sentinel input)
      (comint-send-string nil (concat input (or terminator "\n")))
      (if delete (delete-region mark (point))
        (insert "\n"))
      (set-marker (comint-mark) (point)))))

(defun comint-get-old-input-default ()
  "Default for comint-get-old-input:  use the current line sans prompt."
  (save-excursion
    (comint-bol)
    (buffer-substring (point) (progn (end-of-line) (point)))))

(defun comint-bol (arg)
  "Goes to the beginning of line, then skips past the prompt, if any.
With a prefix argument, (\\[universal-argument]), then doesn't skip prompt.

The prompt skip is done by passing over text matching the regular expression
comint-prompt-regexp, a buffer local variable."
  (interactive "P")
  (beginning-of-line)
  (or arg (if (looking-at comint-prompt-regexp) (goto-char (match-end 0)))))

;;; These two functions are for entering text you don't want echoed or
;;; saved -- typically passwords to ftp, telnet, or somesuch.
;;; Just enter M-x send-invisible and type in your line.
(defun comint-read-noecho (prompt)
  "Prompting with PROMPT, read a single line of text without echoing.
The text can still be recovered (temporarily) with \\[view-lossage].  This
may be a security bug for some applications."
  (let ((echo-keystrokes 0)
	(answ "")
	tem)
    (if (and (stringp prompt) (not (string= (message prompt) "")))
	(message prompt))
    (while (not (or (= (setq tem (read-char)) ?\^m)
		    (= tem ?\n)))
      (setq answ (concat answ (char-to-string tem))))
    (message "")
    answ))

(defun send-invisible (str)
  "Read a string without echoing, and send it to the current buffer's process.
A newline is also sent.  String is not saved on comint input history list.
Security bug: your string can still be temporarily recovered with \\[view-lossage]."
; (interactive (list (comint-read-noecho "Enter non-echoed text")))
  (interactive "P") ; Defeat snooping via C-x esc
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc) (error "Current buffer has no process")
	(comint-send-string proc
			    (if (stringp str) str
				(comint-read-noecho "Enter non-echoed text")))
	(comint-send-string proc "\n"))))


;;; Low-level process communication

(defvar comint-input-chunk-size 512
  "*Long inputs send to comint processes are broken up into chunks of this size.
If your process is choking on big inputs, try lowering the value.")

(defun comint-send-string (proc str)
  "Send PROCESS the contents of STRING as input.
This is equivalent to process-send-string, except that long input strings
are broken up into chunks of size comint-input-chunk-size. Processes
are given a chance to output between chunks. This can help prevent processes
from hanging when you send them long inputs on some OS's."
  (let* ((len (length str))
	 (i (min len comint-input-chunk-size)))
    (process-send-string proc (substring str 0 i))
    (while (< i len)
      (let ((next-i (+ i comint-input-chunk-size)))
	(accept-process-output)
	(process-send-string proc (substring str i (min len next-i)))
	(setq i next-i)))))

(defun comint-send-region (proc start end)
  "Sends to PROC the region delimited by START and END.
This is a replacement for process-send-region that tries to keep
your process from hanging on long inputs. See comint-send-string."
  (comint-send-string proc (buffer-substring start end)))


;;; Random input hackage

(defun comint-flush-output ()
  "Kill all output from interpreter since last input."
  (interactive)
  (save-excursion
    (goto-char (comint-mark))
    (beginning-of-line)
    (delete-region (1+ comint-last-input-end) (point))
    (insert "*** output flushed ***\n")))

(defun comint-show-output ()
  "Start display of the current window at line preceding start of last output.
\"Last output\" is considered to start at the line following the last command
entered to the process."
  (interactive)
  (goto-char comint-last-input-end)
  (beginning-of-line)
  (set-window-start (selected-window) (point))
  (comint-bol))

(defun comint-interrupt-subjob ()
  "Sent an interrupt signal to the current subprocess.
If the process-connection-type is via ptys, the signal is sent to the current
process group of the pseudoterminal which Emacs is using to communicate with
the subprocess.  If the process is a job-control shell, this means the
shell's current subjob.  If the process connection is via pipes, the signal is
sent to the immediate subprocess."
  (interactive)
  (interrupt-process nil t))

(defun comint-kill-subjob ()
  "Send a kill signal to the current subprocess.
See comint-interrupt-subjob for a description of \"current subprocess\"."
  (interactive)
  (kill-process nil t))

(defun comint-quit-subjob ()
  "Send a quit signal to the current subprocess.
See comint-interrupt-subjob for a description of \"current subprocess\"."
  (interactive)
  (quit-process nil t))

(defun comint-stop-subjob ()
  "Stop the current subprocess.
See comint-interrupt-subjob for a description of \"current subprocess\".

WARNING: if there is no current subjob, you can end up suspending
the top-level process running in the buffer.  If you accidentally do
this, use \\[comint-continue-subjob] to resume the process.   (This is not a
problem with most shells, since they ignore this signal.)"
  (interactive)
  (stop-process nil t))

(defun comint-continue-subjob ()
  "Send a continue signal to current subprocess.
See comint-interrupt-subjob for a description of \"current subprocess\".
Useful if you accidentally suspend the top-level process."
  (interactive)
  (continue-process nil t))

(defun comint-kill-input ()
  "Kill from current command through point."
  (interactive)
  (let ((pmark (comint-mark)))
    (if (> (point) pmark)
        (kill-region pmark (point))
      (error "Nothing to kill"))))

(defun comint-delchar-or-maybe-eof (arg)
  "Delete ARG characters forward, or send an EOF to process if at end of buffer."
  (interactive "p")
  (if (eobp)
      (process-send-eof)
    (delete-char arg)))

;;; Support for source-file processing commands.
;;;============================================================================
;;; Many command-interpreters (e.g., Lisp, Scheme, Soar) have
;;; commands that process files of source text (e.g. loading or compiling
;;; files). So the corresponding process-in-a-buffer modes have commands
;;; for doing this (e.g., lisp-load-file). The functions below are useful
;;; for defining these commands.
;;;
;;; Alas, these guys don't do exactly the right thing for Lisp, Scheme
;;; and Soar, in that they don't know anything about file extensions.
;;; So the compile/load interface gets the wrong default occasionally.
;;; The load-file/compile-file default mechanism could be smarter -- it
;;; doesn't know about the relationship between filename extensions and
;;; whether the file is source or executable. If you compile foo.lisp
;;; with compile-file, then the next load-file should use foo.bin for
;;; the default, not foo.lisp. This is tricky to do right, particularly
;;; because the extension for executable files varies so much (.o, .bin,
;;; .lbin, .mo, .vo, .ao, ...).


;;; COMINT-SOURCE-DEFAULT -- determines defaults for source-file processing
;;; commands.
;;;
;;; COMINT-CHECK-SOURCE -- if FNAME is in a modified buffer, asks you if you
;;; want to save the buffer before issuing any process requests to the command
;;; interpreter.
;;;
;;; COMINT-GET-SOURCE -- used by the source-file processing commands to prompt
;;; for the file to process.

;;; (COMINT-SOURCE-DEFAULT previous-dir/file source-modes)
;;;============================================================================
;;; This function computes the defaults for the load-file and compile-file
;;; commands for tea, soar, lisp, and scheme modes. 
;;; 
;;; - PREVIOUS-DIR/FILE is a pair (directory . filename) from the last 
;;; source-file processing command. NIL if there hasn't been one yet.
;;; - SOURCE-MODES is a list used to determine what buffers contain source
;;; files: if the major mode of the buffer is in SOURCE-MODES, it's source.
;;; Typically, (lisp-mode) or (scheme-mode).
;;; 
;;; If the command is given while the cursor is inside a string, *and*
;;; the string is an existing filename, *and* the filename is not a directory,
;;; then the string is taken as default. This allows you to just position
;;; your cursor over a string that's a filename and have it taken as default.
;;;
;;; If the command is given in a file buffer whose major mode is in
;;; SOURCE-MODES, then the the filename is the default file, and the
;;; file's directory is the default directory.
;;; 
;;; If the buffer isn't a source file buffer (e.g., it's the process buffer),
;;; then the default directory & file are what was used in the last source-file
;;; processing command (i.e., PREVIOUS-DIR/FILE).  If this is the first time
;;; the command has been run (PREVIOUS-DIR/FILE is nil), the default directory
;;; is the cwd, with no default file. (\"no default file\" = nil)
;;; 
;;; SOURCE-REGEXP is typically going to be something like (tea-mode)
;;; for T programs, (lisp-mode) for Lisp programs, (soar-mode lisp-mode)
;;; for Soar programs, etc.
;;; 
;;; The function returns a pair: (default-directory . default-file).

(defun comint-source-default (previous-dir/file source-modes)
  (cond ((and buffer-file-name (memq major-mode source-modes))
	 (cons (file-name-directory    buffer-file-name)
	       (file-name-nondirectory buffer-file-name)))
	(previous-dir/file)
	(t
	 (cons default-directory nil))))

;;; (COMINT-CHECK-SOURCE fname)
;;;============================================================================
;;; Prior to loading or compiling (or otherwise processing) a file (in the
;;; process-in-a-buffer modes), this function can be called on the filename.
;;; If the file is loaded into a buffer, and the buffer is modified, the user
;;; is queried to see if he wants to save the buffer before proceeding with
;;; the load or compile.

(defun comint-check-source (fname)
  (let ((buff (get-file-buffer fname)))
    (if (and buff
	     (buffer-modified-p buff)
	     (y-or-n-p (format "Save buffer %s first? "
			       (buffer-name buff))))
	;; save BUFF.
	(let ((old-buffer (current-buffer)))
	  (set-buffer buff)
	  (save-buffer)
	  (set-buffer old-buffer)))))

;;; (COMINT-GET-SOURCE prompt prev-dir/file source-modes mustmatch-p)
;;;============================================================================
;;; COMINT-GET-SOURCE is used to prompt for filenames in command-interpreter
;;; commands that process source files (like loading or compiling a file).
;;; It prompts for the filename, provides a default, if there is one,
;;; and returns the result filename.
;;; 
;;; See COMINT-SOURCE-DEFAULT for more on determining defaults.
;;; 
;;; PROMPT is the prompt string. PREV-DIR/FILE is the (directory . file) pair
;;; from the last source processing command.  SOURCE-MODES is a list of major
;;; modes used to determine what file buffers contain source files.  (These
;;; two arguments are used for determining defaults). If MUSTMATCH-P is true,
;;; then the filename reader will only accept a file that exists.
;;; 
;;; A typical use:
;;; (interactive (comint-get-source "Compile file: " prev-lisp-dir/file
;;;                                 '(lisp-mode) t))

;;; This is pretty stupid about strings. It decides we're in a string
;;; if there's a quote on both sides of point on the current line.
(defun comint-extract-string ()
  "Returns string around point that starts the current line or nil." 
  (save-excursion
    (let* ((point (point))
	   (bol (progn (beginning-of-line) (point)))
	   (eol (progn (end-of-line) (point)))
	   (start (progn (goto-char point) 
			 (and (search-backward "\"" bol t) 
			      (1+ (point)))))
	   (end (progn (goto-char point)
		       (and (search-forward "\"" eol t)
			    (1- (point))))))
      (and start end
	   (buffer-substring start end)))))

(defun comint-get-source (prompt prev-dir/file source-modes mustmatch-p)
  (let* ((def (comint-source-default prev-dir/file source-modes))
         (stringfile (comint-extract-string))
	 (sfile-p (and stringfile
		       (file-exists-p stringfile)
		       (not (file-directory-p stringfile))))
	 (defdir  (if sfile-p (file-name-directory stringfile)
                      (car def)))
	 (deffile (if sfile-p (file-name-nondirectory stringfile)
                      (cdr def)))
	 (ans (read-file-name (if deffile (format "%s(default %s) "
						  prompt    deffile)
				  prompt)
			      defdir
			      (concat defdir deffile)
			      mustmatch-p)))
    (list (expand-file-name (substitute-in-file-name ans)))))


;;; Simple process query facility.
;;; ===========================================================================
;;; This function is for commands that want to send a query to the process
;;; and show the response to the user. For example, a command to get the
;;; arglist for a Common Lisp function might send a "(arglist 'foo)" query
;;; to an inferior Common Lisp process.
;;; 
;;; This simple facility just sends strings to the inferior process and pops
;;; up a window for the process buffer so you can see what the process
;;; responds with.  We don't do anything fancy like try to intercept what the
;;; process responds with and put it in a pop-up window or on the message
;;; line. We just display the buffer. Low tech. Simple. Works good.

;;; Send to the inferior process PROC the string STR. Pop-up but do not select
;;; a window for the inferior process so that its response can be seen.
(defun comint-proc-query (proc str)
  (let* ((proc-buf (process-buffer proc))
	 (proc-mark (process-mark proc)))
    (display-buffer proc-buf)
    (set-buffer proc-buf) ; but it's not the selected *window*
    (let ((proc-win (get-buffer-window proc-buf))
	  (proc-pt (marker-position proc-mark)))
      (comint-send-string proc str) ; send the query
      (accept-process-output proc)  ; wait for some output
      ;; Try to position the proc window so you can see the answer.
      ;; This is bogus code. If you delete the (sit-for 0), it breaks.
      ;; I don't know why. Wizards invited to improve it.
      (if (not (pos-visible-in-window-p proc-pt proc-win))
	  (let ((opoint (window-point proc-win)))
	    (set-window-point proc-win proc-mark) (sit-for 0)
	    (if (not (pos-visible-in-window-p opoint proc-win))
		(push-mark opoint)
		(set-window-point proc-win opoint)))))))


;;; Filename completion in a buffer
;;; ===========================================================================
;;; Useful completion functions, courtesy of the Ergo group.
;;; M-<Tab> will complete the filename at the cursor as much as possible
;;; M-? will display a list of completions in the help buffer.

;;; Three commands:
;;; comint-dynamic-complete		Complete filename at point.
;;; comint-dynamic-list-completions	List completions in help buffer.
;;; comint-replace-by-expanded-filename	Expand and complete filename at point;
;;;					replace with expanded/completed name.

;;; These are not installed in the comint-mode keymap. But they are
;;; available for people who want them.  Shell-mode-map uses them, though.

(defun comint-match-partial-pathname ()
  "Returns the string of an existing filename or causes an error."
  (if (save-excursion (backward-char 1) (looking-at "\\s ")) ""
      (save-excursion
	(re-search-backward "[^~/A-Za-z0-9---_.$#,]+")
	(re-search-forward "[~/A-Za-z0-9---_.$#,]+")
	(substitute-in-file-name 
         (buffer-substring (match-beginning 0) (match-end 0))))))

(defun comint-replace-by-expanded-filename ()
  "Replace the filename at point with its expanded, canonicalised completion.
\"Expanded\" means environment variables (e.g., $HOME) and ~'s are
replaced with the corresponding directories.  \"Canonicalised\" means ..
and . are removed, and the filename is made absolute instead of relative.
See functions expand-file-name and substitute-in-file-name.  See also
comint-dynamic-complete."
  (interactive)
  (let* ((pathname (comint-match-partial-pathname))
	 (pathdir (file-name-directory pathname))
	 (pathnondir (file-name-nondirectory pathname))
	 (completion (file-name-completion pathnondir
					   (or pathdir default-directory))))
    (cond ((null completion)
           (error "No completions"))
	  ((eql completion t)
	   (message "Sole completion"))
	  (t				; this means a string was returned.
	   (delete-region (match-beginning 0) (match-end 0))
	   (insert (expand-file-name (concat pathdir completion)))))))

(defun comint-dynamic-complete ()
  "Complete the filename at point.
This function is similar to comint-replace-by-expanded-filename, except
that it won't change parts of the filename already entered in the buffer; 
it just adds completion characters to the end of the filename."
  (interactive)
  (let* ((pathname (comint-match-partial-pathname))
	 (pathdir (file-name-directory pathname))
	 (pathnondir (file-name-nondirectory pathname))
	 (completion (file-name-completion pathnondir
					   (or pathdir default-directory))))
    (cond ((null completion)
           (error "No completions"))
	  ((eql completion t)
           (error "Sole completion"))
	  (t				; this means a string was returned.
	   (goto-char (match-end 0))
	   (insert (substring completion (length pathnondir)))))))

(defun comint-dynamic-list-completions ()
  "List all possible completions of the filename at point."
  (interactive)
  (let* ((pathname (comint-match-partial-pathname))
	 (pathdir (file-name-directory pathname))
	 (pathnondir (file-name-nondirectory pathname))
	 (completions
	  (file-name-all-completions pathnondir
				     (or pathdir default-directory))))
    (cond ((null completions)
           (error "No completions"))
	  (t
	   (let ((conf (current-window-configuration)))
	     (with-output-to-temp-buffer " *Completions*"
	       (display-completion-list completions))
	     (sit-for 0)
	     (message "Hit space to flush.")
	     (let ((ch (read-char)))
	       (if (= ch ?\ )
		   (set-window-configuration conf)
		   (setq unread-command-char ch))))))))


;;; Converting process modes to use comint mode
;;; ===========================================================================
;;; Renaming variables
;;;   Most of the work is renaming variables and functions.
;;;   These are the common ones.

;;; Local variables --
;;; 	last-input-end			comint-last-input-end
;;;	last-input-start		<unnecessary>
;;;	shell-prompt-pattern		comint-prompt-regexp
;;;     shell-set-directory-error-hook  <no equivalent>
;;; Miscellaneous --
;;;	shell-set-directory	<unnecessary>
;;; 	shell-mode-map		comint-mode-map
;;; Commands --
;;;	shell-send-input	comint-send-input
;;;	shell-send-eof		comint-delchar-or-maybe-eof
;;; 	kill-shell-input	comint-kill-input
;;;	interrupt-shell-subjob	comint-interrupt-subjob
;;;	stop-shell-subjob	comint-stop-subjob
;;;	quit-shell-subjob	comint-quit-subjob
;;;	kill-shell-subjob	comint-kill-subjob
;;;	kill-output-from-shell	comint-kill-output
;;;	show-output-from-shell	comint-show-output
;;;	copy-last-shell-input	Use comint-previous-input/comint-next-input
;;;
;;; LAST-INPUT-START is no longer necessary because inputs are stored on the
;;; input history ring. SHELL-SET-DIRECTORY is gone, its functionality taken
;;; over by SHELL-DIRECTORY-TRACKER, the shell mode's comint-input-sentinel.
;;; Comint mode does not provide functionality equivalent to 
;;; shell-set-directory-error-hook; it is gone.
;;; 
;;; If you are implementing some process-in-a-buffer mode, called foo-mode, do
;;; *not* create the comint-mode local variables in your foo-mode function.
;;; This is not modular.  Instead, call comint-mode, and let *it* create the
;;; necessary comint-specific local variables. Then create the
;;; foo-mode-specific local variables in foo-mode.  Set the buffer's keymap to
;;; be foo-mode-map, and its mode to be foo-mode.  Set the comint-mode hooks
;;; (comint-prompt-regexp, comint-input-filter, comint-input-sentinel,
;;; comint-get-old-input) that need to be different from the defaults.  Call
;;; foo-mode-hook, and you're done. Don't run the comint-mode hook yourself;
;;; comint-mode will take care of it. 
;;;
;;; Note that make-comint is different from make-shell in that it
;;; doesn't have a default program argument. If you give make-shell
;;; a program name of NIL, it cleverly chooses one of explicit-shell-name,
;;; $ESHELL, $SHELL, or /bin/sh. If you give make-comint a program argument
;;; of NIL, it barfs. Adjust your code accordingly...
