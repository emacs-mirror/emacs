;;; comint.el --- general command interpreter in a window stuff

;; Copyright (C) 1988, 1990, 1992, 1993 Free Software Foundation, Inc.

;; Author: Olin Shivers <shivers@cs.cmu.edu>
;; Keywords: processes

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

;;; The changelog is at the end of this file.

;;; Please send me bug reports, bug fixes, and extensions, so that I can
;;; merge them into the master source.
;;;     - Olin Shivers (shivers@cs.cmu.edu)

;;; This file defines a general command-interpreter-in-a-buffer package
;;; (comint mode). The idea is that you can build specific process-in-a-buffer
;;; modes on top of comint mode -- e.g., lisp, shell, scheme, T, soar, ....
;;; This way, all these specific packages share a common base functionality, 
;;; and a common set of bindings, which makes them easier to use (and
;;; saves code, implementation time, etc., etc.).

;;; Several packages are already defined using comint mode:
;;; - shell.el defines a shell-in-a-buffer mode.
;;; - cmulisp.el defines a simple lisp-in-a-buffer mode.
;;;
;;; - The file cmuscheme.el defines a scheme-in-a-buffer mode.
;;; - The file tea.el tunes scheme and inferior-scheme modes for T.
;;; - The file soar.el tunes lisp and inferior-lisp modes for Soar.
;;; - cmutex.el defines tex and latex modes that invoke tex, latex, bibtex,
;;;   previewers, and printers from within emacs.
;;; - background.el allows csh-like job control inside emacs.
;;; It is pretty easy to make new derived modes for other processes.

;;; For documentation on the functionality provided by comint mode, and
;;; the hooks available for customising it, see the comments below.
;;; For further information on the standard derived modes (shell, 
;;; inferior-lisp, inferior-scheme, ...), see the relevant source files.

;;; For hints on converting existing process modes (e.g., tex-mode,
;;; background, dbx, gdb, kermit, prolog, telnet) to use comint-mode
;;; instead of shell-mode, see the notes at the end of this file.


;;; Brief Command Documentation:
;;;============================================================================
;;; Comint Mode Commands: (common to all derived modes, like shell & cmulisp
;;; mode)
;;;
;;; m-p	    comint-previous-input    	    Cycle backwards in input history
;;; m-n	    comint-next-input  	    	    Cycle forwards
;;; m-r     comint-previous-matching-input   Previous input matching a regexp
;;; m-s     comint-next-matching-input       Next input that matches
;;; return  comint-send-input
;;; c-a     comint-bol                      Beginning of line; skip prompt.
;;; c-d	    comint-delchar-or-maybe-eof     Delete char unless at end of buff.
;;; c-c c-u comint-kill-input	    	    ^u
;;; c-c c-w backward-kill-word    	    ^w
;;; c-c c-c comint-interrupt-subjob 	    ^c
;;; c-c c-z comint-stop-subjob	    	    ^z
;;; c-c c-\ comint-quit-subjob	    	    ^\
;;; c-c c-o comint-kill-output		    Delete last batch of process output
;;; c-c c-r comint-show-output		    Show last batch of process output
;;;
;;; Not bound by default in comint-mode
;;; send-invisible			Read a line w/o echo, and send to proc
;;; (These are bound in shell-mode)
;;; comint-dynamic-complete		Complete filename at point.
;;; comint-dynamic-list-completions	List completions in help buffer.
;;; comint-replace-by-expanded-filename	Expand and complete filename at point;
;;;					replace with expanded/completed name.
;;; comint-kill-subjob			No mercy.
;;; comint-continue-subjob		Send CONT signal to buffer's process
;;;					group. Useful if you accidentally
;;;					suspend your process (with C-c C-z).
;;;
;;; These used to be bound for RMS -- I prefer the input history stuff,
;;; but you might like 'em.
;;; m-P	   comint-msearch-input		Search backwards for prompt
;;; m-N    comint-psearch-input		Search forwards for prompt
;;; C-cR   comint-msearch-input-matching Search backwards for prompt & string

;;; comint-mode-hook is the comint mode hook. Basically for your keybindings.
;;; comint-load-hook is run after loading in this package.

;;; Code:

(defconst comint-version "2.03")

(require 'ring)

;;; Buffer Local Variables:
;;;============================================================================
;;; Comint mode buffer local variables:
;;;     comint-prompt-regexp    - string       comint-bol uses to match prompt.
;;;     comint-last-input-start - marker       Handy if inferior always echos
;;;     comint-last-input-end   - marker       For comint-kill-output command
;;;     comint-input-ring-size  - integer      For the input history
;;;     comint-input-ring       - ring             mechanism
;;;     comint-input-ring-index - number           ...
;;;     comint-last-input-match - string           ...
;;;     comint-get-old-input    - function     Hooks for specific 
;;;     comint-input-sentinel   - function         process-in-a-buffer
;;;     comint-input-filter     - function         modes.
;;;     comint-input-send	- function
;;;     comint-eol-on-send	- boolean
;;;     comint-process-echoes   - boolean

(defvar comint-prompt-regexp "^"
  "Regexp to recognise prompts in the inferior process.
Defaults to \"^\", the null string at BOL.

Good choices:
  Canonical Lisp: \"^[^> ]*>+:? *\" (Lucid, franz, kcl, T, cscheme, oaklisp)
  Lucid Common Lisp: \"^\\(>\\|\\(->\\)+\\) *\"
  franz: \"^\\(->\\|<[0-9]*>:\\) *\"
  kcl: \"^>+ *\"
  shell: \"^[^#$%>]*[#$%>] *\"
  T: \"^>+ *\"

This is a good thing to set in mode hooks.")

(defvar comint-input-ring-size 30
  "Size of input history ring.")

(defvar comint-process-echoes nil
  "*If non-`nil', assume that the subprocess echoes any input.
If so, delete one copy of the input so that only one copy eventually
appears in the buffer. 

This variable is buffer-local.")

;;; Here are the per-interpreter hooks.
(defvar comint-get-old-input (function comint-get-old-input-default)
  "Function that submits old text in comint mode.
This function is called when return is typed while the point is in old text.
It returns the text to be submitted as process input.  The default is
comint-get-old-input-default, which grabs the current line, and strips off
leading text matching comint-prompt-regexp")

(defvar comint-input-sentinel (function ignore)
  "Called on each input submitted to comint mode process by comint-send-input.
Thus it can, for instance, track cd/pushd/popd commands issued to the csh.")

(defvar comint-input-filter
  (function (lambda (str) (not (string-match "\\`\\s *\\'" str))))
  "Predicate for filtering additions to input history.
Only inputs answering true to this function are saved on the input
history list. Default is to save anything that isn't all whitespace")

(defvar comint-input-sender (function comint-simple-send)
  "Function to actually send to PROCESS the STRING submitted by user.
Usually this is just 'comint-simple-send, but if your mode needs to 
massage the input string, this is your hook. This is called from
the user command comint-send-input. comint-simple-send just sends
the string plus a newline.")

(defvar comint-eol-on-send 'T
  "If non-nil, then jump to the end of the line before sending input to process.
See comint-send-input")

(defvar comint-mode-hook '()
  "Called upon entry into comint-mode
This is run before the process is cranked up.")

(defvar comint-exec-hook '()
  "Called each time a process is exec'd by comint-exec.
This is called after the process is cranked up.  It is useful for things that
must be done each time a process is executed in a comint-mode buffer (e.g.,
(process-kill-without-query)). In contrast, the comint-mode-hook is only
executed once when the buffer is created.")

(defvar comint-mode-map nil)

(defvar comint-ptyp t
  "True if communications via pty; false if by pipe.  Buffer local.
This is to work around a bug in emacs process signalling.")

;;(defvar comint-last-input-match ""
;;  "Last string searched for by comint input history search, for defaulting.
;;Buffer local variable.") 

(defvar comint-input-ring nil)
(defvar comint-last-input-start)
(defvar comint-last-input-end)
(defvar comint-input-ring-index)
(put 'comint-input-ring 'permanent-local t)
(put 'comint-ptyp 'permanent-local t)

(defun comint-mode ()
  "Major mode for interacting with an inferior interpreter.
Interpreter name is same as buffer name, sans the asterisks.
Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.
Setting mode variable comint-eol-on-send means jump to the end of the line
before submitting new input.

This mode is typically customised to create inferior-lisp-mode,
shell-mode, etc.. This can be done by setting the hooks
comint-input-sentinel, comint-input-filter, comint-input-sender and
comint-get-old-input to appropriate functions, and the variable
comint-prompt-regexp to the appropriate regular expression.

An input history is maintained of size comint-input-ring-size, and
can be accessed with the commands comint-next-input [\\[comint-next-input]] and 
comint-previous-input [\\[comint-previous-input]]. Commands not keybound by
default are send-invisible, comint-dynamic-complete, and 
comint-list-dynamic-completions.

If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it.

\\{comint-mode-map}

Entry to this mode runs the hooks on comint-mode-hook"
  (interactive)
    ;; Do not remove this.  All major modes must do this.
    (kill-all-local-variables)
    (setq major-mode 'comint-mode)
    (setq mode-name "Comint")
    (setq mode-line-process '(": %s"))
    (use-local-map comint-mode-map)
    (make-local-variable 'comint-last-input-start)
    (setq comint-last-input-start (make-marker))
    (make-local-variable 'comint-last-input-end)
    (setq comint-last-input-end (make-marker))
;;;    (make-local-variable 'comint-last-input-match)
;;;    (setq comint-last-input-match "")
    (make-local-variable 'comint-prompt-regexp) ; Don't set; default
    (make-local-variable 'comint-input-ring-size)      ; ...to global val.
    (make-local-variable 'comint-input-ring)
    (make-local-variable 'comint-input-ring-index)
    (setq comint-input-ring-index 0)
    (make-local-variable 'comint-get-old-input)
    (make-local-variable 'comint-input-sentinel)
    (make-local-variable 'comint-input-filter)  
    (make-local-variable 'comint-input-sender)
    (make-local-variable 'comint-eol-on-send)
    (make-local-variable 'comint-ptyp)
    (make-local-variable 'comint-exec-hook)
    (make-local-variable 'comint-process-echoes)
    (run-hooks 'comint-mode-hook)
    (or comint-input-ring
	(setq comint-input-ring (make-ring comint-input-ring-size))))

(if comint-mode-map
    nil
  (setq comint-mode-map (make-sparse-keymap))
  (define-key comint-mode-map "\ep" 'comint-previous-input)
  (define-key comint-mode-map "\en" 'comint-next-input)
  (define-key comint-mode-map "\er" 'comint-previous-matching-input)
  (define-key comint-mode-map "\es" 'comint-next-matching-input)
  (define-key comint-mode-map "\C-m" 'comint-send-input)
  (define-key comint-mode-map "\C-d" 'comint-delchar-or-maybe-eof)
  (define-key comint-mode-map "\C-a" 'comint-bol)
  (define-key comint-mode-map "\C-c\C-u" 'comint-kill-input)
  (define-key comint-mode-map "\C-c\C-w" 'backward-kill-word)
  (define-key comint-mode-map "\C-c\C-c" 'comint-interrupt-subjob)
  (define-key comint-mode-map "\C-c\C-z" 'comint-stop-subjob)
  (define-key comint-mode-map "\C-c\C-\\" 'comint-quit-subjob)
  (define-key comint-mode-map "\C-c\C-o" 'comint-kill-output)
  (define-key comint-mode-map "\C-c\C-r" 'comint-show-output)
  ;;; prompt-search commands commented out 3/90 -Olin
; (define-key comint-mode-map "\eP" 'comint-msearch-input)
; (define-key comint-mode-map "\eN" 'comint-psearch-input)
; (define-key comint-mode-map "\C-cR" 'comint-msearch-input-matching)
  )


;;; This function is used to make a full copy of the comint mode map,
;;; so that client modes won't interfere with each other. This function
;;; isn't necessary in emacs 18.5x, but we keep it around for 18.4x versions.
(defun full-copy-sparse-keymap (km)
  "Recursively copy the sparse keymap KM"
  (cond ((consp km)
	 (cons (full-copy-sparse-keymap (car km))
	       (full-copy-sparse-keymap (cdr km))))
	(t km)))

(defun comint-check-proc (buffer)
  "True if there is a process associated w/buffer BUFFER, and
it is alive (status RUN or STOP). BUFFER can be either a buffer or the
name of one"
  (let ((proc (get-buffer-process buffer)))
    (and proc (memq (process-status proc) '(run stop)))))

;;; Note that this guy, unlike shell.el's make-shell, barfs if you pass it ()
;;; for the second argument (program).
;;;###autoload
(defun make-comint (name program &optional startfile &rest switches)
  "Make a comint process NAME in a buffer, running PROGRAM.
The name of the buffer is made by surrounding NAME with `*'s.
If there is already a running process in that buffer, it is not restarted.
Optional third arg STARTFILE is the name of a file to send the contents of to 
the process.  Any more args are arguments to PROGRAM."
  (let ((buffer (get-buffer-create (concat "*" name "*"))))
    ;; If no process, or nuked process, crank up a new one and put buffer in
    ;; comint mode.  Otherwise, leave buffer and existing process alone.
    (cond ((not (comint-check-proc buffer))
	   (save-excursion
	     (set-buffer buffer)
	     (comint-mode)) ; Install local vars, mode, keymap, ...
	   (comint-exec buffer name program startfile switches)))
    buffer))

(defun comint-exec (buffer name command startfile switches)
  "Fires up a process in buffer for comint modes.
Blasts any old process running in the buffer.  Doesn't set the buffer mode.
You can use this to cheaply run a series of processes in the same comint
buffer.  The hook comint-exec-hook is run after each exec."
  (save-excursion
    (set-buffer buffer)
    (let ((proc (get-buffer-process buffer)))	; Blast any old process.
      (if proc (delete-process proc)))
    ;; Crank up a new process
    (let ((proc (comint-exec-1 name buffer command switches)))
      (set-process-filter proc 'comint-filter)
      (make-local-variable 'comint-ptyp)
      (setq comint-ptyp process-connection-type) ; T if pty, NIL if pipe.
      ;; Jump to the end, and set the process mark.
      (goto-char (point-max))
      (set-marker (process-mark proc) (point))
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
    (run-hooks 'comint-exec-hook)
    buffer)))

;;; This auxiliary function cranks up the process for comint-exec in
;;; the appropriate environment.

(defun comint-exec-1 (name buffer command switches)
  (let ((process-environment
	 (comint-update-env process-environment
			    (list (format "TERMCAP=emacs:co#%d:tc=unknown"
					  (frame-width))
				  "TERM=emacs"
				  "EMACS=t"))))
    (apply 'start-process name buffer command switches)))
	     


;; This is just (append new old-env) that compresses out shadowed entries.
;; It's also pretty ugly, mostly due to lisp's horrible iteration structures.
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
;;; M-C-r -- previous input matching
;;; ===========================================================================

(defun comint-previous-input (arg)
  "Cycle backwards through input history."
  (interactive "*p")
  (let ((len (ring-length comint-input-ring)))
    (cond ((<= len 0)
	   (message "Empty input ring")
	   (ding))
	  ((not (comint-after-pmark-p))
	   (message "Not after process mark")
	   (ding))
	  (t
	   (delete-region (point)
			  (process-mark (get-buffer-process (current-buffer))))
	   ;; Initialize the index on the first use of this command
	   ;; so that the first M-p gets index 0, and the first M-n gets
	   ;; index -1.
	   (if (null comint-input-ring-index)
	       (setq comint-input-ring-index
		     (if (> arg 0) -1
			 (if (< arg 0) 1 0))))
	   (setq comint-input-ring-index
		 (ring-mod (+ comint-input-ring-index arg) len))
	   (message "%d" (1+ comint-input-ring-index))
	   (insert (ring-ref comint-input-ring comint-input-ring-index))))))

(defun comint-next-input (arg)
  "Cycle forwards through input history."
  (interactive "*p")
  (comint-previous-input (- arg)))

(defun comint-previous-matching-input (regexp arg)
  "Search backwards through input history for match for REGEXP.
\(Previous history elements are earlier commands.)
With prefix argument N, search for Nth previous match.
If N is negative, find the next or Nth next match."
  (interactive
   (let* ((minibuffer-history-sexp-flag nil)
	  ;; Don't clobber this.
	  (last-command last-command)
	  (regexp (read-from-minibuffer "Previous input matching (regexp): "
					nil
					minibuffer-local-map
					nil
					'minibuffer-history-search-history)))
     (list (if (string= regexp "")
	       (setcar minibuffer-history-search-history
		       (nth 1 minibuffer-history-search-history))
	     regexp)
	   (prefix-numeric-value current-prefix-arg))))
  (if (null comint-input-ring-index)
      (setq comint-input-ring-index -1))
  (let* ((len (ring-length comint-input-ring))
	 (motion (if (> arg 0) 1 -1))
	 (n comint-input-ring-index))
    ;; Do the whole search as many times as the argument says.
    (while (/= arg 0)
      (let ((prev n))
	;; Step once.
	(setq n (ring-mod (+ n motion) len))
	;; If we haven't reached a match, step some more.
	(while (and (< n len)
		    (not (string-match regexp (ring-ref comint-input-ring n))))
	  (setq n (ring-mod (+ n motion) len))
	  ;; If we have gone all the way around in this search, error.
	  (if (= n prev)
	      (error "Not found"))))
      (setq arg (if (> arg 0) (1- arg) (1+ arg))))
    ;; Now that we know which ring element to use,
    ;; substitute that for the current input.
    (comint-previous-input (- n comint-input-ring-index))))

(defun comint-next-matching-input (regexp arg)
  "Search forwards through input history for match for REGEXP.
\(Later history elements are more recent commands.)
With prefix argument N, search for Nth following match.
If N is negative, find the previous or Nth previous match."
  (interactive
   (let ((minibuffer-history-sexp-flag nil)
	 ;; Don't clobber this.
	 (last-command last-command)
	 (regexp (read-from-minibuffer "Previous input matching (regexp): "
				       nil
				       minibuffer-local-map
				       nil
				       'minibuffer-history-search-history)))
     (list (if (string= regexp "")
	       (setcar minibuffer-history-search-history
		       (nth 1 minibuffer-history-search-history))
	     regexp)
	   (prefix-numeric-value current-prefix-arg))))
  (comint-previous-matching-input regexp (- arg)))

;;; These next three commands are alternatives to the input history commands
;;; They search through the process buffer
;;; text looking for occurrences of the prompt.  Bound to M-P, M-N, and C-c R
;;; (uppercase P, N, and R) for now. Try'em out. Go with what you like...

;;; comint-msearch-input-matching prompts for a string, not a regexp.
;;; This could be considered to be the wrong thing. I decided to keep it
;;; simple, and not make the user worry about regexps. This, of course,
;;; limits functionality.

;;; These commands were deemed non-winning and have been commented out.
;;; Feel free to re-enable them if you like. -Olin 3/91

;(defun comint-psearch-input ()
;  "Search forwards for next occurrence of prompt and skip to end of line.
;\(prompt is anything matching regexp comint-prompt-regexp)"
;  (interactive)
;  (if (re-search-forward comint-prompt-regexp (point-max) t)
;      (end-of-line)
;      (error "No occurrence of prompt found")))
;
;(defun comint-msearch-input ()
;  "Search backwards for previous occurrence of prompt and skip to end of line.
;Search starts from beginning of current line."
;  (interactive)
;  (let ((p (save-excursion
;	     (beginning-of-line)
;	     (cond ((re-search-backward comint-prompt-regexp (point-min) t)
;		    (end-of-line)
;		    (point))
;		   (t nil)))))
;    (if p (goto-char p)
;	(error "No occurrence of prompt found"))))
;
;(defun comint-msearch-input-matching (str)
;  "Search backwards for occurrence of prompt followed by STRING.
;STRING is prompted for, and is NOT a regular expression."
;  (interactive (let ((s (read-from-minibuffer 
;			 (format "Command (default %s): "
;				 comint-last-input-match))))
;		 (list (if (string= s "") comint-last-input-match s))))
;; (interactive "sCommand: ")
;  (setq comint-last-input-match str) ; update default
;  (let* ((r (concat comint-prompt-regexp (regexp-quote str)))
;	 (p (save-excursion
;	      (beginning-of-line)
;	      (cond ((re-search-backward r (point-min) t)
;		     (end-of-line)
;		     (point))
;		    (t nil)))))
;    (if p (goto-char p)
;	(error "No match"))))

;;;
;;; Similar input -- contributed by ccm and highly winning.
;;;
;;; Reenter input, removing back to the last insert point if it exists. 
;;;
;;(defvar comint-last-similar-string "" 
;;  "The string last used in a similar string search.")

;;(defun comint-previous-similar-input (arg)
;;  "Fetch the previous (older) input that matches the string typed so far.
;;Successive repetitions find successively older matching inputs.
;;A prefix argument serves as a repeat count; a negative argument
;;fetches following (more recent) inputs."
;;  (interactive "p")
;;  (if (not (comint-after-pmark-p))
;;      (error "Not after process mark"))
;;  (if (null comint-input-ring-index)
;;      (setq comint-input-ring-index
;;	    (if (> arg 0) -1
;;	      (if (< arg 0) 1 0))))
;;  (if (not (or (eq last-command 'comint-previous-similar-input)
;;	       (eq last-command 'comint-next-similar-input)))
;;      (setq comint-last-similar-string 
;;	    (buffer-substring 
;;	     (process-mark (get-buffer-process (current-buffer)))
;;	     (point))))
;;  (let* ((size (length comint-last-similar-string))
;;	 (len (ring-length comint-input-ring))
;;	 (n (+ comint-input-ring-index arg))
;;	 entry)
;;    (while (and (< n len) 
;;		(or (< (length (setq entry (ring-ref comint-input-ring n))) size)
;;		    (not (equal comint-last-similar-string 
;;				(substring entry 0 size)))))
;;      (setq n (+ n arg)))
;;    (cond ((< n len)
;;	   (setq comint-input-ring-index n)
;;	   (if (or (eq last-command 'comint-previous-similar-input)
;;		   (eq last-command 'comint-next-similar-input))
;;	       (delete-region (mark) (point)) ; repeat
;;	       (push-mark (point)))	      ; 1st time
;;	   (insert (substring entry size)))
;;	  (t (error "Not found")))
;;    (message "%d" (1+ comint-input-ring-index))))

;;(defun comint-next-similar-input (arg)
;;  "Fetch the next (newer) input that matches the string typed so far.
;;Successive repetitions find successively newer matching inputs.
;;A prefix argument serves as a repeat count; a negative argument
;;fetches previous (older) inputs."
;;  (interactive "p")
;;  (comint-previous-similar-input (- arg)))

(defun comint-send-input () 
  "Send input to process.
After the process output mark, sends all text from the process mark to
point as input to the process.  Before the process output mark, calls value
of variable `comint-get-old-input' to retrieve old input, copies it to the
process mark, and sends it.  If variable `comint-process-echoes' is `nil',
a terminal newline is also inserted into the buffer and sent to the process
\(if it is non-`nil', all text from the process mark to point is deleted,
since it is assumed the remote process will re-echo it).  The value of
variable `comint-input-sentinel' is called on the input before sending it.
The input is entered into the input history ring, if the value of variable
`comint-input-filter' returns non-`nil' when called on the input.

If variable `comint-eol-on-send' is non-`nil', then point is moved to the
end of line before sending the input.

`comint-get-old-input', `comint-input-sentinel', and `comint-input-filter'
are chosen according to the command interpreter running in the buffer.  E.g.,
If the interpreter is the csh,
    comint-get-old-input is the default: take the current line, discard any
        initial string matching regexp comint-prompt-regexp.
    comint-input-sentinel monitors input for \"cd\", \"pushd\", and \"popd\" 
        commands. When it sees one, it cd's the buffer.
    comint-input-filter is the default: returns T if the input isn't all white
	space.

If the comint is Lucid Common Lisp, 
    comint-get-old-input snarfs the sexp ending at point.
    comint-input-sentinel does nothing.
    comint-input-filter returns NIL if the input matches input-filter-regexp,
        which matches (1) all whitespace (2) :a, :c, etc.

Similarly for Soar, Scheme, etc."
  (interactive)
  ;; Note that the input string does not include its terminal newline.
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc) (error "Current buffer has no process")
	(let* ((pmark (process-mark proc))
	       (pmark-val (marker-position pmark))
	       (input (if (>= (point) pmark-val)
			  (progn (if comint-eol-on-send (end-of-line))
				 (buffer-substring pmark (point)))
			(let ((copy (funcall comint-get-old-input)))
			  (goto-char pmark)
			  (insert copy)
			  copy))))
          (if comint-process-echoes
              (delete-region pmark (point))
            (insert ?\n))
	  (if (funcall comint-input-filter input)
	      (ring-insert comint-input-ring input))
	  (funcall comint-input-sentinel input)
	  (funcall comint-input-sender proc input)
	  (setq comint-input-ring-index nil)
	  (set-marker comint-last-input-start pmark)
	  (set-marker comint-last-input-end (point))
	  (set-marker (process-mark proc) (point))))))

;; The sole purpose of using this filter for comint processes
;; is to keep comint-last-input-end from moving forward
;; when output is inserted.
(defun comint-filter (process string)
  (let ((obuf (current-buffer))
	ordonly
	opoint obeg oend)
    (set-buffer (process-buffer process))
    (setq opoint (point))
    (setq obeg (point-min))
    (setq oend (point-max))
    (setq ordonly buffer-read-only)
    (let ((buffer-read-only nil)
	  (nchars (length string)))
      (widen)
      (goto-char (process-mark process))
      (if (<= (point) opoint)
	  (setq opoint (+ opoint nchars)))
      ;; Insert after old_begv, but before old_zv.
      (if (< (point) obeg)
	  (setq obeg (+ obeg nchars)))
      (if (<= (point) oend)
	  (setq oend (+ oend nchars)))

      (insert-before-markers string)
      (and comint-last-input-end
	   (marker-buffer comint-last-input-end)
	   (= (point) comint-last-input-end)
	   (set-marker comint-last-input-end
		       (- comint-last-input-end nchars)))
      (set-marker (process-mark process) (point) nil)
      (force-mode-line-update)

      (narrow-to-region obeg oend)
      (setq buffer-read-only ordonly)
      (goto-char opoint)
      (set-buffer obuf))))

(defun comint-get-old-input-default ()
  "Default for comint-get-old-input.
Take the current line, and discard any initial text matching
comint-prompt-regexp."
  (save-excursion
    (beginning-of-line)
    (comint-skip-prompt)
    (let ((beg (point)))
      (end-of-line)
      (buffer-substring beg (point)))))

(defun comint-skip-prompt ()
  "Skip past the text matching regexp comint-prompt-regexp. 
If this takes us past the end of the current line, don't skip at all."
  (let ((eol (save-excursion (end-of-line) (point))))
    (if (and (looking-at comint-prompt-regexp)
	     (<= (match-end 0) eol))
	(goto-char (match-end 0)))))


(defun comint-after-pmark-p ()
  "Is point after the process output marker?"
  ;; Since output could come into the buffer after we looked at the point
  ;; but before we looked at the process marker's value, we explicitly 
  ;; serialise. This is just because I don't know whether or not emacs
  ;; services input during execution of lisp commands.
  (let ((proc-pos (marker-position
		   (process-mark (get-buffer-process (current-buffer))))))
    (<= proc-pos (point))))

(defun comint-simple-send (proc string)
  "Default function for sending to PROC input STRING.
This just sends STRING plus a newline. To override this,
set the hook COMINT-INPUT-SENDER."
  (comint-send-string proc string)
  (comint-send-string proc "\n"))

(defun comint-bol (arg)
  "Goes to the beginning of line, then skips past the prompt, if any.
If a prefix argument is given (\\[universal-argument]), then no prompt skip 
-- go straight to column 0.

The prompt skip is done by skipping text matching the regular expression
comint-prompt-regexp, a buffer local variable.

If you don't like this command, reset c-a to beginning-of-line 
in your hook, comint-mode-hook."
  (interactive "P")
  (beginning-of-line)
  (if (null arg) (comint-skip-prompt)))

;;; These two functions are for entering text you don't want echoed or
;;; saved -- typically passwords to ftp, telnet, or somesuch.
;;; Just enter m-x send-invisible and type in your line.

(defun comint-read-noecho (prompt &optional stars)
  "Read a single line of text from user without echoing, and return it. 
Prompt with argument PROMPT, a string.  Optional argument STARS causes
input to be echoed with '*' characters on the prompt line.  Input ends with
RET, LFD, or ESC.  DEL or C-h rubs out.  C-u kills line.  C-g aborts (if
inhibit-quit is set because e.g. this function was called from a process
filter and C-g is pressed, this function will return `nil', rather than a
string).

Note that the keystrokes comprising the text can still be recovered
(temporarily) with \\[view-lossage].  This may be a security bug for some
applications."
  (let ((ans "")
	(c 0)
	(echo-keystrokes 0)
	(cursor-in-echo-area t)
        (done nil))
    (while (not done)
      (if stars
          (message "%s%s" prompt (make-string (length ans) ?*))
        (message prompt))
      (setq c (read-char))
      (cond ((= c ?\C-g)
             ;; This function may get called from a process filter, where
             ;; inhibit-quit is set.  In later versions of emacs read-char
             ;; may clear quit-flag itself and return C-g.  That would make
             ;; it impossible to quit this loop in a simple way, so
             ;; re-enable it here (for backward-compatibility the check for
             ;; quit-flag below would still be necessary, so this is seems
             ;; like the simplest way to do things).
             (setq quit-flag t
                   done t))
            ((or (= c ?\r) (= c ?\n) (= c ?\e))
             (setq done t))
            ((= c ?\C-u)
             (setq ans ""))
            ((and (/= c ?\b) (/= c ?\177))
             (setq ans (concat ans (char-to-string c))))
            ((> (length ans) 0)
             (setq ans (substring ans 0 -1)))))
    (if quit-flag
        ;; Emulate a true quit, except that we have to return a value.
        (prog1
            (setq quit-flag nil)
          (message "Quit")
          (beep t))
      (message "")
      ans)))

(defun send-invisible (str)
  "Read a string without echoing.
Then send it to the process running in the current buffer. A new-line
is additionally sent. String is not saved on comint input history list.
Security bug: your string can still be temporarily recovered with
\\[view-lossage]."
; (interactive (list (comint-read-noecho "Enter non-echoed text")))
  (interactive "P") ; Defeat snooping via C-x esc
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc) (error "Current buffer has no process")
	(comint-send-string proc
			    (if (stringp str) str
				(comint-read-noecho "Non-echoed text: " t)))
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

(defun comint-kill-output ()
  "Kill all output from interpreter since last input."
  (interactive)
  (let ((pmark (process-mark (get-buffer-process (current-buffer)))))
    (kill-region comint-last-input-end pmark)
    (goto-char pmark)    
    (insert "*** output flushed ***\n")
    (set-marker pmark (point))))

(defun comint-show-output ()
  "Display start of this batch of interpreter output at top of window.
Also put cursor there."
  (interactive)
  (goto-char comint-last-input-end)
  (backward-char)
  (beginning-of-line)
  (set-window-start (selected-window) (point))
  (end-of-line))

(defun comint-interrupt-subjob ()
  "Interrupt the current subjob."
  (interactive)
  (interrupt-process nil comint-ptyp))

(defun comint-kill-subjob ()
  "Send kill signal to the current subjob."
  (interactive)
  (kill-process nil comint-ptyp))

(defun comint-quit-subjob ()
  "Send quit signal to the current subjob."
  (interactive)
  (quit-process nil comint-ptyp))

(defun comint-stop-subjob ()
  "Stop the current subjob.
WARNING: if there is no current subjob, you can end up suspending
the top-level process running in the buffer. If you accidentally do
this, use \\[comint-continue-subjob] to resume the process. (This
is not a problem with most shells, since they ignore this signal.)"
  (interactive)
  (stop-process nil comint-ptyp))

(defun comint-continue-subjob ()
  "Send CONT signal to process buffer's process group.
Useful if you accidentally suspend the top-level process."
  (interactive)
  (continue-process nil comint-ptyp))

(defun comint-kill-input ()
  "Kill all text from last stuff output by interpreter to point."
  (interactive)
  (let* ((pmark (process-mark (get-buffer-process (current-buffer))))
	 (p-pos (marker-position pmark)))
    (if (> (point) p-pos)
	(kill-region pmark (point)))))

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
;;; commands for tea, soar, cmulisp, and cmuscheme modes. 
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
;;; Prior to loading or compiling (or otherwise processing) a file (in the CMU
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
  "Returns string around POINT that starts the current line or nil." 
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
		       (condition-case ()
			   (file-exists-p stringfile)
			 (error nil))
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

;;; I am somewhat divided on this string-default feature. It seems
;;; to violate the principle-of-least-astonishment, in that it makes
;;; the default harder to predict, so you actually have to look and see
;;; what the default really is before choosing it. This can trip you up.
;;; On the other hand, it can be useful, I guess. I would appreciate feedback
;;; on this.
;;;     -Olin


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

;;; Three commands:
;;; comint-dynamic-complete		Complete filename at point.
;;; comint-dynamic-list-completions	List completions in help buffer.
;;; comint-replace-by-expanded-filename	Expand and complete filename at point;
;;;					replace with expanded/completed name.

;;; These are not installed in the comint-mode keymap. But they are
;;; available for people who want them. Shell-mode installs them:
;;; (define-key shell-mode-map "\t" 'comint-dynamic-complete)
;;; (define-key shell-mode-map "\M-?"  'comint-dynamic-list-completions)))
;;;
;;; Commands like this are fine things to put in load hooks if you
;;; want them present in specific modes.


(defun comint-match-partial-pathname ()
  "Returns the filename at point or causes an error."
  (save-excursion
    (if (re-search-backward "[^~/A-Za-z0-9_.$#,=-]" nil 'move)
	(forward-char 1))
    ;; Anchor the search forwards.
    (if (not (looking-at "[~/A-Za-z0-9_.$#,=-]")) (error ""))
    (re-search-forward "[~/A-Za-z0-9_.$#,=-]+")
    (substitute-in-file-name
     (buffer-substring (match-beginning 0) (match-end 0)))))


(defun comint-replace-by-expanded-filename ()
"Expand the filename at point.
Replace the filename with an expanded, canonicalised, and completed
 replacement.
\"Expanded\" means environment variables (e.g., $HOME) and ~'s are
replaced with the corresponding directories.  \"Canonicalised\" means ..
and \. are removed, and the filename is made absolute instead of relative.
See functions expand-file-name and substitute-in-file-name. See also
comint-dynamic-complete."
  (interactive)
  (let* ((pathname (comint-match-partial-pathname))
	 (pathdir (file-name-directory pathname))
	 (pathnondir (file-name-nondirectory pathname))
	 (completion (file-name-completion pathnondir
					   (or pathdir default-directory))))
    (cond ((null completion)
	   (message "No completions of %s" pathname)
	   (ding))
	  ((eql completion t)
	   (message "Unique completion"))
	  (t				; this means a string was returned.
	   (delete-region (match-beginning 0) (match-end 0))
	   (insert (expand-file-name (concat pathdir completion)))))))


(defun comint-dynamic-complete ()
  "Dynamically complete the filename at point.
This function is similar to `comint-replace-by-expanded-filename', except
that it won't change parts of the filename already entered in the buffer; 
it just adds completion characters to the end of the filename."
  (interactive)
  (let* ((pathname (comint-match-partial-pathname))
	 (pathdir (file-name-directory pathname))
	 (pathnondir (file-name-nondirectory pathname))
	 (completion (file-name-completion  pathnondir
					   (or pathdir default-directory))))
    (cond ((null completion)
	   (message "No completions of %s" pathname)
	   (ding))
	  ((eql completion t)
	   (message "Unique completion"))
	  (t				; this means a string was returned.
	   (goto-char (match-end 0))
	   (insert (substring completion (length pathnondir)))))))

(defun comint-dynamic-list-completions ()
  "List in help buffer all possible completions of the filename at point."
  (interactive)
  (let* ((pathname (comint-match-partial-pathname))
	 (pathdir (file-name-directory pathname))
	 (pathnondir (file-name-nondirectory pathname))
	 (completions
	  (file-name-all-completions pathnondir
				     (or pathdir default-directory))))
    (cond ((null completions)
	   (message "No completions of %s" pathname)
	   (ding))
	  (t
	   (let ((conf (current-window-configuration)))
	     (with-output-to-temp-buffer "*Help*"
	       (display-completion-list completions))
	     (sit-for 0)
	     (message "Hit space to flush")
	     (let ((ch (read-event)))
	       (if (eq ch ?\ )
		   (set-window-configuration conf)
		 (setq unread-command-events (list ch)))))))))

;;; Converting process modes to use comint mode
;;; ===========================================================================
;;; The code in the Emacs 19 distribution has all been modified to use comint
;;; where needed.  However, there are `third-party' packages out there that
;;; still use the old shell mode.  Here's a guide to conversion.
;;;
;;; Renaming variables
;;; Most of the work is renaming variables and functions. These are the common
;;; ones:
;;; Local variables:
;;;	last-input-start	comint-last-input-start
;;; 	last-input-end		comint-last-input-end
;;;	shell-prompt-pattern	comint-prompt-regexp
;;;     shell-set-directory-error-hook <no equivalent>
;;; Miscellaneous:
;;;	shell-set-directory	<unnecessary>
;;; 	shell-mode-map		comint-mode-map
;;; Commands:
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
;;; SHELL-SET-DIRECTORY is gone, its functionality taken over by
;;; SHELL-DIRECTORY-TRACKER, the shell mode's comint-input-sentinel.
;;; Comint mode does not provide functionality equivalent to
;;; shell-set-directory-error-hook; it is gone.
;;;
;;; comint-last-input-start is provided for modes which want to munge
;;; the buffer after input is sent, perhaps because the inferior
;;; insists on echoing the input.  The LAST-INPUT-START variable in
;;; the old shell package was used to implement a history mechanism,
;;; but you should think twice before using comint-last-input-start
;;; for this; the input history ring often does the job better.
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
;;; comint-mode will take care of it. The following example, from shell.el,
;;; is typical:
;;; 
;;; (defun shell-mode ()
;;;   (interactive)
;;;   (comint-mode)
;;;   (setq comint-prompt-regexp shell-prompt-pattern)
;;;   (setq major-mode 'shell-mode)
;;;   (setq mode-name "Shell")
;;;   (cond ((not shell-mode-map)
;;; 	     (setq shell-mode-map (full-copy-sparse-keymap comint-mode-map))
;;; 	     (define-key shell-mode-map "\M-\t" 'comint-dynamic-complete)
;;; 	     (define-key shell-mode-map "\M-?"
;;;                      'comint-dynamic-list-completions)))
;;;   (use-local-map shell-mode-map)
;;;   (make-local-variable 'shell-directory-stack)
;;;   (setq shell-directory-stack nil)
;;;   (setq comint-input-sentinel 'shell-directory-tracker)
;;;   (run-hooks 'shell-mode-hook))
;;;
;;;
;;; Note that make-comint is different from make-shell in that it
;;; doesn't have a default program argument. If you give make-shell
;;; a program name of NIL, it cleverly chooses one of explicit-shell-name,
;;; $ESHELL, $SHELL, or /bin/sh. If you give make-comint a program argument
;;; of NIL, it barfs. Adjust your code accordingly...
;;;

;;; Do the user's customisation...

(defvar comint-load-hook nil
  "This hook is run when comint is loaded in.
This is a good place to put keybindings.")
	
(run-hooks 'comint-load-hook)

;;; Change log:
;;; 9/12/89 
;;;  - Souped up the filename expansion procedures.
;;;    Doc strings are much clearer and more detailed.
;;;    Fixed a bug where doing a filename completion when the point
;;;    was in the middle of the filename instead of at the end would lose.
;;;
;;; 2/17/90 
;;;  - Souped up the command history stuff so that text inserted
;;;    by comint-previous-input-matching is removed by following
;;;    command history recalls. comint-next/previous-input-matching
;;;    is now much more smoothly integrated w/the command history stuff.
;;;  - Added comint-eol-on-send flag and comint-input-sender hook.
;;;    Comint-input-sender based on code contributed by Jeff Peck
;;;    (peck@sun.com).
;;;
;;; 3/13/90 ccm@cmu.cs.edu
;;;  - Added comint-previous-similar-input for looking up similar inputs.
;;;  - Added comint-send-and-get-output to allow snarfing input from
;;;    buffer. 
;;;  - Added the ability to pick up a source file by positioning over
;;;    a string in comint-get-source.
;;;  - Added add-hook to make it a little easier for the user to use
;;;    multiple hooks.
;;;  
;;; 5/22/90 shivers
;;; - Moved Chris' multiplexed ipc stuff to comint-ipc.el.
;;; - Altered Chris' comint-get-source string feature. The string
;;;   is only offered as a default if it names an existing file.
;;; - Changed comint-exec to directly crank up the process, instead
;;;   of calling the env program. This made background.el happy.
;;; - Added new buffer-local var comint-ptyp. The problem is that
;;;   the signalling functions don't work as advertised. If you are
;;;   communicating via pipes, the CURRENT-GROUP arg is supposed to
;;;   be ignored, but, unfortunately it seems to be the case that you
;;;   must pass a NIL for this arg in the pipe case. COMINT-PTYP
;;;   is a flag that tells whether the process is communicating
;;;   via pipes or a pty. The comint signalling functions use it
;;;   to determine the necessary CURRENT-GROUP arg value. The bug
;;;   has been reported to the Gnu folks.
;;; - comint-dynamic-complete flushes the help window if you hit space
;;;   after you execute it.
;;; - Added functions comint-send-string, comint-send-region and var 
;;;   comint-input-chunk-size.  comint-send-string tries to prevent processes
;;;   from hanging when you send them long strings by breaking them into
;;;   chunks and allowing process output between chunks. I got the idea from
;;;   Eero Simoncelli's Common Lisp package. Note that using
;;;   comint-send-string means that the process buffer's contents can change
;;;   during a call!  If you depend on process output only happening between
;;;   toplevel commands, this could be a problem. In such a case, use
;;;   process-send-string instead. If this is a problem for people, I'd like
;;;   to hear about it.
;;; - Added comint-proc-query as a simple mechanism for commands that
;;;   want to query an inferior process and display its response. For a
;;;   typical use, see lisp-show-arglist in cmulisp.el.
;;; - Added constant comint-version, which is now "2.01".
;;;
;;; 6/14/90 shivers
;;; - Had comint-update-env defined twice. Removed extra copy. Also
;;;   renamed mem to be comint-mem, for modularity. The duplication
;;;   was reported by Michael Meissner.
;;; 6/16/90 shivers
;;; - Emacs has two different mechanisms for maintaining the process
;;;   environment, determined at compile time by the MAINTAIN-ENVIRONMENT
;;;   #define. One uses the process-environment global variable, and
;;;   one uses a getenv/setenv interface. comint-exec assumed the
;;;   process-environment interface; it has been generalised (with
;;;   comint-exec-1) to handle both cases. Pretty bogus. We could,
;;;   of course, skip all this and just use the etc/env program to
;;;   handle the environment tweaking, but that obscures process
;;;   queries that other modules (like background.el) depend on. etc/env
;;;   is also fairly bogus. This bug, and some of the fix code was
;;;   reported by Dan Pierson.
;;;
;;; 9/5/90 shivers
;;; - Changed make-variable-buffer-local's to make-local-variable's.
;;;   This leaves non-comint-mode buffers alone. Stephane Payrard
;;;   reported the sloppy useage.
;;; - You can now go from comint-previous-similar-input to
;;;   comint-previous-input with no problem.
;;;
;;; 12/21/90 shivers
;;; - Added a condition-case to comint-get-source. Bogus strings
;;;   beginning with ~ were making the file-exists-p barf.
;;; - Added "=" to the set of chars recognised by file completion
;;;   as constituting a filename.
;;;
;;; 1/90 shivers
;;; These changes comprise release 2.02:
;;; - Removed the kill-all-local-variables in comint-mode. This
;;;   made it impossible for client modes to set things before calling
;;;   comint-mode. (In particular, it messed up ilisp.el) In general,
;;;   the client mode should be responsible for a k-a-l-v's.
;;; - Fixed comint-match-partial-pathname so that it works in
;;;   more cases: if the filename begins at the start-of-buffer;
;;;   if point is on the first char of the filename. Just a question
;;;   of getting the tricky bits right.
;;; - Added a hook, comint-exec-hook that is run each time a process
;;;   is cranked up. Useful for things like process-kill-without-query.
;;;
;;; These two were pointed out by tale:
;;; - Improved the doc string in comint-send-input a little bit.
;;; - Tweaked make-comint to check process status with comint-check-proc
;;;   instead of equivalent inline code. 
;;;
;;; - Prompt-search history commands have been commented out. I never
;;;   liked them; I don't think anyone used them.
;;; - Made comint-exec-hook a local var, as it should have been.
;;;   (This way, for instance, you can have shell procs kill-w/o-query,
;;;    but let Scheme procs be default.)
;;;
;;; 7/91 Shivers
;;; - Souped up comint-read-noecho with an optional argument, STARS.
;;;   Suggested by mjlx@EAGLE.CNSF.CORNELL.EDU.
;;; - Moved comint-previous-input-matching from C-c r to C-M-r.
;;;   C-c <letter> bindings are reserved for the user.
;;;   These bindings were done by Jim Blandy.
;;; These changes comprise version 2.03.

(provide 'comint)

;;; comint.el ends here
