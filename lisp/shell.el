;;; shell.el --- specialized comint.el for running the shell.
;;; Copyright (C) 1988, 1993 Free Software Foundation, Inc.

;; Author: Olin Shivers <shivers@cs.cmu.edu>
;; Adapted-by: Simon Marshall <s.marshall@dcs.hull.ac.uk>
;; Keywords: processes

;;; This file is part of GNU Emacs.

;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Please send me bug reports, bug fixes, and extensions, so that I can
;;; merge them into the master source.
;;;     - Olin Shivers (shivers@cs.cmu.edu)
;;;     - Simon Marshall (s.marshall@dcs.hull.ac.uk)

;;; This file defines a a shell-in-a-buffer package (shell mode) built
;;; on top of comint mode.  This is actually cmushell with things
;;; renamed to replace its counterpart in Emacs 18.  cmushell is more
;;; featureful, robust, and uniform than the Emacs 18 version.

;;; Since this mode is built on top of the general command-interpreter-in-
;;; a-buffer mode (comint mode), it shares a common base functionality, 
;;; and a common set of bindings, with all modes derived from comint mode.
;;; This makes these modes easier to use.

;;; For documentation on the functionality provided by comint mode, and
;;; the hooks available for customising it, see the file comint.el.
;;; For further information on shell mode, see the comments below.

;;; Needs fixin:
;;; When sending text from a source file to a subprocess, the process-mark can 
;;; move off the window, so you can lose sight of the process interactions.
;;; Maybe I should ensure the process mark is in the window when I send
;;; text to the process? Switch selectable?

;; YOUR .EMACS FILE
;;=============================================================================
;; Some suggestions for your .emacs file.
;;
;; ;; Define C-c t to run my favorite command in shell mode:
;; (setq shell-mode-hook
;;       '((lambda () 
;;           (define-key shell-mode-map "\C-ct" 'favorite-cmd))))


;;; Brief Command Documentation:
;;;============================================================================
;;; Comint Mode Commands: (common to shell and all comint-derived modes)
;;;
;;; m-p	    comint-previous-input    	    Cycle backwards in input history
;;; m-n	    comint-next-input  	    	    Cycle forwards
;;; m-r     comint-previous-matching-input  Previous input matching a regexp
;;; m-R     comint-previous-matching-input-from-input -"- matching input
;;; m-s     comint-next-matching-input      Next input that matches
;;; m-S     comint-next-matching-input-from-input     -"- matching input
;;; m-c-r   comint-previous-input-matching  Search backwards in input history
;;; return  comint-send-input
;;; c-a     comint-bol                      Beginning of line; skip prompt
;;; c-d	    comint-delchar-or-maybe-eof	    Delete char unless at end of buff.
;;; c-c c-u comint-kill-input	    	    ^u
;;; c-c c-w backward-kill-word    	    ^w
;;; c-c c-c comint-interrupt-subjob 	    ^c
;;; c-c c-z comint-stop-subjob	    	    ^z
;;; c-c c-\ comint-quit-subjob	    	    ^\
;;; c-c c-o comint-kill-output		    Delete last batch of process output
;;; c-c c-r comint-show-output		    Show last batch of process output
;;; c-c c-h comint-dynamic-list-input-ring  List input history
;;;         send-invisible                  Read line w/o echo & send to proc
;;;         comint-continue-subjob	    Useful if you accidentally suspend
;;;					        top-level job
;;; comint-mode-hook is the comint mode hook.

;;; Shell Mode Commands:
;;;         shell			    Fires up the shell process
;;; tab     comint-dynamic-complete	    Complete filename/command/history
;;; m-?     comint-dynamic-list-filename-completions List completions in help buffer
;;; m-c-f   shell-forward-command           Forward a shell command
;;; m-c-b   shell-backward-command          Backward a shell command
;;; 	    dirs    			    Resync the buffer's dir stack
;;; 	    dirtrack-toggle                 Turn dir tracking on/off
;;;
;;; The shell mode hook is shell-mode-hook
;;; comint-prompt-regexp is initialised to shell-prompt-pattern, for backwards
;;; compatibility.

;;; Read the rest of this file for more information.

;;; SHELL.EL COMPATIBILITY
;;; Notes from when this was called cmushell, and was not the standard emacs
;;; shell package.
;;;============================================================================
;;; In brief: this package should have no trouble coexisting with shell.el.
;;; 
;;; Most customising variables -- e.g., explicit-shell-file-name -- are the
;;; same, so the users shouldn't have much trouble. Hooks have different
;;; names, however, so you can customise shell mode differently from cmushell
;;; mode. You basically just have to remember to type M-x cmushell instead of
;;; M-x shell.
;;; 
;;; It would be nice if this file was completely plug-compatible with the old
;;; shell package -- if you could just name this file shell.el, and have it
;;; transparently replace the old one. But you can't.  Several other packages
;;; (tex-mode, background, dbx, gdb, kermit, monkey, prolog, telnet) are also
;;; clients of shell mode. These packages assume detailed knowledge of shell
;;; mode internals in ways that are incompatible with cmushell mode (mostly
;;; because of cmushell mode's greater functionality).  So, unless we are
;;; willing to port all of these packages, we can't have this file be a
;;; complete replacement for shell.el -- that is, we can't name this file
;;; shell.el, and its main entry point (shell), because dbx.el will break
;;; when it loads it in and tries to use it.
;;; 
;;; There are two ways to fix this. One: rewrite these other modes to use the
;;; new package. This is a win, but can't be assumed. The other, backwards
;;; compatible route, is to make this package non-conflict with shell.el, so
;;; both files can be loaded in at the same time. And *that* is why some
;;; functions and variables have different names: (cmushell),
;;; cmushell-mode-map, that sort of thing. All the names have been carefully
;;; chosen so that shell.el and cmushell.el won't tromp on each other.

;;; Customization and Buffer Variables
;;; ===========================================================================
;;; 

;;; Code:

(require 'comint)

;;;###autoload
(defvar shell-prompt-pattern "^[^#$%>\n]*[#$%>] *"
  "Regexp to match prompts in the inferior shell.
Defaults to \"^[^#$%>\\n]*[#$%>] *\", which works pretty well.
This variable is used to initialise `comint-prompt-regexp' in the 
shell buffer.

The pattern should probably not match more than one line.  If it does,
shell-mode may become confused trying to distinguish prompt from input
on lines which don't start with a prompt.

This is a fine thing to set in your `.emacs' file.")

(defvar shell-delimiter-argument-list '("|" "&" "<" ">" "(" ")" ";")
  "List of characters to recognise as separate arguments.
Defaults to \(\"|\" \"&\" \"\(\" \")\" \";\"), which works pretty well.
This variable is used to initialise `comint-delimiter-argument-list' in the
shell buffer.

This is a fine thing to set in your `.emacs' file.")

(defvar shell-command-regexp "\\((.*)\\|[^;&|]\\)+"
  "*Regexp to match shell commands.
Elements of pipes are considered as separate commands, forks and redirections
as part of one command.")

(defvar shell-completion-execonly t
  "*If non-nil, use executable files only for completion candidates.
This mirrors the optional behavior of tcsh.

Detecting executability of files may slow command completion considerably.")

(defvar shell-popd-regexp "popd"
  "*Regexp to match subshell commands equivalent to popd.")

(defvar shell-pushd-regexp "pushd"
  "*Regexp to match subshell commands equivalent to pushd.")

(defvar shell-pushd-tohome nil
  "*If non-nil, make pushd with no arg behave as \"pushd ~\" (like cd).
This mirrors the optional behavior of tcsh.")

(defvar shell-pushd-dextract nil
  "*If non-nil, make \"pushd +n\" pop the nth dir to the stack top.
This mirrors the optional behavior of tcsh.")

(defvar shell-pushd-dunique nil
  "*If non-nil, make pushd only add unique directories to the stack.
This mirrors the optional behavior of tcsh.")

(defvar shell-cd-regexp "cd"
  "*Regexp to match subshell commands equivalent to cd.")

(defvar explicit-shell-file-name nil
  "*If non-nil, is file name to use for explicitly requested inferior shell.")

(defvar explicit-csh-args
  (if (eq system-type 'hpux)
      ;; -T persuades HP's csh not to think it is smarter
      ;; than us about what terminal modes to use.
      '("-i" "-T")
    '("-i"))
  "*Args passed to inferior shell by M-x shell, if the shell is csh.
Value is a list of strings, which may be nil.")

;;; All the above vars aren't prefixed "cmushell-" to make them
;;; backwards compatible w/shell.el and old .emacs files.

(defvar shell-dirstack nil
  "List of directories saved by pushd in this buffer's shell.
Thus, this does not include the shell's current directory.")

(defvar shell-last-dir nil
  "Keep track of last directory for ksh `cd -' command.")

(defvar shell-dirstack-query "dirs"
  "Command used by `shell-resync-dir' to query the shell.")

(defvar shell-mode-map '())
(cond ((not shell-mode-map)
       (setq shell-mode-map (full-copy-sparse-keymap comint-mode-map))
       (define-key shell-mode-map "\C-c\C-f" 'shell-forward-command)
       (define-key shell-mode-map "\C-c\C-b" 'shell-backward-command)
       (define-key shell-mode-map "\t" 'comint-dynamic-complete)
       (define-key shell-mode-map "\M-?"
	 'comint-dynamic-list-filename-completions)))

(defvar shell-mode-hook '()
  "*Hook for customising Shell mode.")


;;; Basic Procedures
;;; ===========================================================================
;;;

(defun shell-mode ()
  "Major mode for interacting with an inferior shell.
Return after the end of the process' output sends the text from the 
    end of process to the end of the current line.
Return before end of process output copies the current line (except
    for the prompt) to the end of the buffer and sends it.
M-x send-invisible reads a line of text without echoing it, and sends it to
    the shell.  This is useful for entering passwords.

If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it.

cd, pushd and popd commands given to the shell are watched by Emacs to keep
this buffer's default directory the same as the shell's working directory.
M-x dirs queries the shell and resyncs Emacs' idea of what the current 
    directory stack is.
M-x dirtrack-toggle turns directory tracking on and off.

\\{shell-mode-map}
Customization: Entry to this mode runs the hooks on `comint-mode-hook' and
`shell-mode-hook' (in that order).  After each shell output, the hooks on
`comint-output-filter-functions' are run.

Variables `shell-cd-regexp', `shell-pushd-regexp' and `shell-popd-regexp'
are used to match their respective commands, while `shell-pushd-tohome',
`shell-pushd-dextract' and `shell-pushd-dunique' control the behavior of the
relevant command.

Variables `comint-completion-autolist', `comint-completion-addsuffix' and
`comint-completion-recexact' control the behavior of file name, command name
and variable name completion.  Variable `shell-completion-execonly' controls
the behavior of command name completion.

Variables `comint-input-ring-file-name' and `comint-input-autoexpand' control
the initialisation of the input ring history, and history expansion.

Variables `comint-output-filter-functions', `comint-scroll-to-bottom-on-input',
and `comint-scroll-to-bottom-on-output' control whether input and output
cause the window to scroll to the end of the buffer."
  (interactive)
  (comint-mode)
  (setq major-mode 'shell-mode)
  (setq mode-name "Shell")
  (use-local-map shell-mode-map)
  (setq comint-prompt-regexp shell-prompt-pattern)
  (setq comint-delimiter-argument-list shell-delimiter-argument-list)
  (setq comint-after-partial-filename-command 'shell-after-partial-filename)
  (setq comint-get-current-command 'shell-get-current-command)
  (setq comint-dynamic-complete-command-command 'shell-dynamic-complete-command)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start comint-prompt-regexp)
  (make-local-variable 'shell-dirstack)
  (setq shell-dirstack nil)
  (setq shell-last-dir nil)
  (make-local-variable 'shell-dirtrackp)
  (setq shell-dirtrackp t)
  (setq comint-input-sentinel 'shell-directory-tracker)
  ;; shell-dependent assignments.
  (let ((shell (car (process-command (get-buffer-process (current-buffer))))))
    (setq comint-input-ring-file-name
	  (or (getenv "HISTFILE")
	      (cond ((string-match "csh$" shell) "~/.history")
		    ((string-match "bash$" shell) "~/.bash_history")
		    ((string-match "ksh$" shell) "~/.sh_history")
		    (t "~/.history")))))
  (run-hooks 'shell-mode-hook)
  (comint-read-input-ring)
  (shell-dirstack-message))

;;;###autoload
(defun shell ()
  "Run an inferior shell, with I/O through buffer *shell*.
If buffer exists but shell process is not running, make new shell.
If buffer exists and shell process is running, just switch to buffer `*shell*'.
Program used comes from variable `explicit-shell-file-name',
 or (if that is nil) from the ESHELL environment variable,
 or else from SHELL if there is no ESHELL.
If a file `~/.emacs_SHELLNAME' exists, it is given as initial input
 (Note that this may lose due to a timing error if the shell
  discards input when it starts up.)
The buffer is put in Shell mode, giving commands for sending input
and controlling the subjobs of the shell.  See `shell-mode'.
See also the variable `shell-prompt-pattern'.

The shell file name (sans directories) is used to make a symbol name
such as `explicit-csh-args'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the shell.
Otherwise, one argument `-i' is passed to the shell.

\(Type \\[describe-mode] in the shell buffer for a list of commands.)"
  (interactive)
  (if (not (comint-check-proc "*shell*"))
      (let* ((prog (or explicit-shell-file-name
		       (getenv "ESHELL")
		       (getenv "SHELL")
		       "/bin/sh"))		     
	     (name (file-name-nondirectory prog))
	     (startfile (concat "~/.emacs_" name))
	     (xargs-name (intern-soft (concat "explicit-" name "-args"))))
	(set-buffer (apply 'make-comint "shell" prog
			   (if (file-exists-p startfile) startfile)
			   (if (and xargs-name (boundp xargs-name))
			       (symbol-value xargs-name)
			     '("-i"))))
	(shell-mode)))
  (switch-to-buffer "*shell*"))

;;; Directory tracking
;;; ===========================================================================
;;; This code provides the shell mode input sentinel
;;;     SHELL-DIRECTORY-TRACKER
;;; that tracks cd, pushd, and popd commands issued to the shell, and
;;; changes the current directory of the shell buffer accordingly.
;;;
;;; This is basically a fragile hack, although it's more accurate than
;;; the version in Emacs 18's shell.el. It has the following failings:
;;; 1. It doesn't know about the cdpath shell variable.
;;; 2. It cannot infallibly deal with command sequences, though it does well
;;;    with these and with ignoring commands forked in another shell with ()s.
;;; 3. More generally, any complex command is going to throw it. Otherwise,
;;;    you'd have to build an entire shell interpreter in emacs lisp.  Failing
;;;    that, there's no way to catch shell commands where cd's are buried
;;;    inside conditional expressions, aliases, and so forth.
;;;
;;; The whole approach is a crock. Shell aliases mess it up. File sourcing
;;; messes it up. You run other processes under the shell; these each have
;;; separate working directories, and some have commands for manipulating
;;; their w.d.'s (e.g., the lcd command in ftp). Some of these programs have
;;; commands that do *not* affect the current w.d. at all, but look like they
;;; do (e.g., the cd command in ftp).  In shells that allow you job
;;; control, you can switch between jobs, all having different w.d.'s. So
;;; simply saying %3 can shift your w.d..
;;;
;;; The solution is to relax, not stress out about it, and settle for
;;; a hack that works pretty well in typical circumstances. Remember
;;; that a half-assed solution is more in keeping with the spirit of Unix, 
;;; anyway. Blech.
;;;
;;; One good hack not implemented here for users of programmable shells
;;; is to program up the shell w.d. manipulation commands to output
;;; a coded command sequence to the tty. Something like
;;;     ESC | <cwd> |
;;; where <cwd> is the new current working directory. Then trash the
;;; directory tracking machinery currently used in this package, and
;;; replace it with a process filter that watches for and strips out
;;; these messages.

(defun shell-directory-tracker (str)
  "Tracks cd, pushd and popd commands issued to the shell.
This function is called on each input passed to the shell.
It watches for cd, pushd and popd commands and sets the buffer's
default directory to track these commands.

You may toggle this tracking on and off with M-x dirtrack-toggle.
If emacs gets confused, you can resync with the shell with M-x dirs.

See variables `shell-cd-regexp', `shell-pushd-regexp', and `shell-popd-regexp',
while `shell-pushd-tohome', `shell-pushd-dextract' and `shell-pushd-dunique'
control the behavior of the relevant command.

Environment variables are expanded, see function `substitute-in-file-name'."
  (if shell-dirtrackp
      ;; We fail gracefully if we think the command will fail in the shell.
      (condition-case chdir-failure
	  (let ((start (progn (string-match "^[;\\s ]*" str) ; skip whitespace
			      (match-end 0)))
		end cmd arg1)
	    (while (string-match shell-command-regexp str start)
	      (setq end (match-end 0)
		    cmd (comint-arguments (substring str start end) 0 0)
		    arg1 (comint-arguments (substring str start end) 1 1))
	      (cond ((eq (string-match shell-popd-regexp cmd) 0)
		     (shell-process-popd (substitute-in-file-name arg1)))
		    ((eq (string-match shell-pushd-regexp cmd) 0)
		     (shell-process-pushd (substitute-in-file-name arg1)))
		    ((eq (string-match shell-cd-regexp cmd) 0)
		     (shell-process-cd (substitute-in-file-name arg1))))
	      (setq start (progn (string-match "[;\\s ]*" str end) ; skip again
				 (match-end 0)))))
	(error (message "Couldn't cd")))))


;; Like `cd', but prepends comint-file-name-prefix to absolute names.
(defsubst shell-cd (directory)
  (if (file-name-absolute-p directory)
      (cd-absolute (concat comint-file-name-prefix directory))
    (cd directory)))

;;; popd [+n]
(defun shell-process-popd (arg)
  (let ((num (or (shell-extract-num arg) 0)))
    (cond ((and num (= num 0) shell-dirstack)
	   (shell-cd (car shell-dirstack))
	   (setq shell-dirstack (cdr shell-dirstack))
	   (shell-dirstack-message))
	  ((and num (> num 0) (<= num (length shell-dirstack)))
	   (let* ((ds (cons nil shell-dirstack))
		  (cell (nthcdr (1- num) ds)))
	     (rplacd cell (cdr (cdr cell)))
	     (setq shell-dirstack (cdr ds))
	     (shell-dirstack-message)))
	  (t
	   (error (message "Couldn't popd."))))))

;;; cd [dir]
(defun shell-process-cd (arg)
  (let ((new-dir (cond ((zerop (length arg)) (getenv "HOME"))
		       ((string-equal "-" arg) shell-last-dir)
		       (t arg))))
    (setq shell-last-dir default-directory)
    (shell-cd new-dir)
    (shell-dirstack-message)))

;;; pushd [+n | dir]
(defun shell-process-pushd (arg)
  (let ((num (shell-extract-num arg)))
    (cond ((zerop (length arg))
	   ;; no arg -- swap pwd and car of stack unless shell-pushd-tohome
	   (cond (shell-pushd-tohome
		  (shell-process-pushd "~"))
		 (shell-dirstack
		  (let ((old default-directory))
		    (shell-cd (car shell-dirstack))
		    (setq shell-dirstack
			  (cons old (cdr shell-dirstack)))
		    (shell-dirstack-message)))
		 (t
		  (message "Directory stack empty."))))
	  ((numberp num)
	   ;; pushd +n
	   (cond ((> num (length shell-dirstack))
		  (message "Directory stack not that deep."))
		 ((= num 0)
		  (error (message "Couldn't cd.")))
		 (shell-pushd-dextract
		  (let ((dir (nth (1- num) shell-dirstack)))
		    (shell-process-popd arg)
		    (shell-process-pushd default-directory)
		    (shell-cd dir)
		    (shell-dirstack-message)))
		 (t
		  (let* ((ds (cons default-directory shell-dirstack))
			 (dslen (length ds))
			 (front (nthcdr num ds))
			 (back (reverse (nthcdr (- dslen num) (reverse ds))))
			 (new-ds (append front back)))
		    (shell-cd (car new-ds))
		    (setq shell-dirstack (cdr new-ds))
		    (shell-dirstack-message)))))
	  (t
	   ;; pushd <dir>
	   (let ((old-wd default-directory))
	     (shell-cd arg)
	     (if (or (null shell-pushd-dunique)
		     (not (member old-wd shell-dirstack)))
		 (setq shell-dirstack (cons old-wd shell-dirstack)))
	     (shell-dirstack-message))))))

;; If STR is of the form +n, for n>0, return n. Otherwise, nil.
(defun shell-extract-num (str)
  (and (string-match "^\\+[1-9][0-9]*$" str)
       (string-to-int str)))


(defun shell-dirtrack-toggle ()
  "Turn directory tracking on and off in a shell buffer."
  (interactive)
  (setq shell-dirtrackp (not shell-dirtrackp))
  (message "Directory tracking %s" (if shell-dirtrackp "ON" "OFF")))

;;; For your typing convenience:
(defalias 'dirtrack-toggle 'shell-dirtrack-toggle)


(defun shell-resync-dirs ()
  "Resync the buffer's idea of the current directory stack.
This command queries the shell with the command bound to 
`shell-dirstack-query' (default \"dirs\"), reads the next
line output and parses it to form the new directory stack.
DON'T issue this command unless the buffer is at a shell prompt.
Also, note that if some other subprocess decides to do output
immediately after the query, its output will be taken as the
new directory stack -- you lose. If this happens, just do the
command again."
  (interactive)
  (let* ((proc (get-buffer-process (current-buffer)))
	 (pmark (process-mark proc)))
    (goto-char pmark)
    (insert shell-dirstack-query) (insert "\n")
    (sit-for 0) ; force redisplay
    (comint-send-string proc shell-dirstack-query) 
    (comint-send-string proc "\n")
    (set-marker pmark (point))
    (let ((pt (point))) ; wait for 1 line
      ;; This extra newline prevents the user's pending input from spoofing us.
      (insert "\n") (backward-char 1)
      (while (not (looking-at ".+\n"))
	(accept-process-output proc)
	(goto-char pt)))
    (goto-char pmark) (delete-char 1) ; remove the extra newline
    ;; That's the dirlist. grab it & parse it.
    (let* ((dl (buffer-substring (match-beginning 0) (1- (match-end 0))))
	   (dl-len (length dl))
	   (ds '())			; new dir stack
	   (i 0))
      (while (< i dl-len)
	;; regexp = optional whitespace, (non-whitespace), optional whitespace
	(string-match "\\s *\\(\\S +\\)\\s *" dl i) ; pick off next dir
	(setq ds (cons (substring dl (match-beginning 1) (match-end 1))
		       ds))
	(setq i (match-end 0)))
      (let ((ds (reverse ds)))
	(condition-case nil
	    (progn (cd (car ds))
		   (setq shell-dirstack (cdr ds))
		   (shell-dirstack-message))
	  (error (message "Couldn't cd.")))))))

;;; For your typing convenience:
(defalias 'dirs 'shell-resync-dirs)


;;; Show the current dirstack on the message line.
;;; Pretty up dirs a bit by changing "/usr/jqr/foo" to "~/foo".
;;; (This isn't necessary if the dirlisting is generated with a simple "dirs".)
;;; All the commands that mung the buffer's dirstack finish by calling
;;; this guy.
(defun shell-dirstack-message ()
  (let* ((msg "")
	 (ds (cons default-directory shell-dirstack))
	 (home (expand-file-name (concat comint-file-name-prefix "~/")))
	 (homelen (length home)))
    (while ds
      (let ((dir (car ds)))
	(and (>= (length dir) homelen) (string= home (substring dir 0 homelen))
	    (setq dir (concat "~/" (substring dir homelen))))
	;; Strip off comint-file-name-prefix if present.
	(and comint-file-name-prefix
	     (>= (length dir) (length comint-file-name-prefix))
	     (string= comint-file-name-prefix
		      (substring dir 0 (length comint-file-name-prefix)))
	     (setq dir (substring dir (length comint-file-name-prefix)))
	     (setcar ds dir))
	(setq msg (concat msg (directory-file-name dir) " "))
	(setq ds (cdr ds))))
    (message msg)))

(defun shell-forward-command (&optional arg)
  "Move forward across ARG shell command(s).  Does not cross lines.
See `shell-command-regexp'."
  (interactive "p")
  (let ((limit (save-excursion (end-of-line nil) (point))))
    (if (re-search-forward (concat shell-command-regexp "\\([;&|][\\s ]*\\)+")
			   limit 'move arg)
	(skip-syntax-backward "^\\s "))))


(defun shell-backward-command (&optional arg)
  "Move backward across ARG shell command(s).  Does not cross lines.
See `shell-command-regexp'."
  (interactive "p")
  (let ((limit (save-excursion (comint-bol nil) (point))))
    (skip-syntax-backward "\\s " limit)
    (if (re-search-backward
	 (format "[;&|]+[\\s ]*\\(%s\\)" shell-command-regexp) limit 'move arg)
	(progn (goto-char (match-beginning 1))
	       (skip-syntax-backward "^\\s ")))))


(defun shell-get-current-command ()
  "Function that returns the current command including arguments."
  (save-excursion
    (if (looking-at "\\s *[^;&|]")
	(goto-char (match-end 0)))
    (buffer-substring
     (progn (shell-backward-command 1) (point))
     (progn (shell-forward-command 1) (if (eolp) (point) (match-end 1))))))


(defun shell-after-partial-filename ()
  "Returns t if point is after a file name.
File names are assumed to contain `/'s or not be the first item in the command.

See also `shell-backward-command'."
  (let ((filename (comint-match-partial-filename)))
    (or (save-match-data (string-match "/" filename))
	(not (eq (match-beginning 0)
		 (save-excursion (shell-backward-command 1) (point)))))))


(defun shell-dynamic-complete-command ()
  "Dynamically complete the command at point.
This function is similar to `comint-dynamic-complete-filename', except that it
searches `exec-path' (minus the trailing emacs library path) for completion
candidates.  Note that this may not be the same as the shell's idea of the
path.

Completion is dependent on the value of `shell-completion-execonly', plus
those that effect file completion.  See `comint-dynamic-complete-filename'."
  (interactive)
  (let* ((completion-ignore-case nil)
	 (filename (comint-match-partial-filename))
	 (pathnondir (file-name-nondirectory filename))
	 (paths (cdr (reverse exec-path)))
	 (cwd (file-name-as-directory (expand-file-name default-directory)))
	 (ignored-extensions
	  (mapconcat (function (lambda (x) (concat (regexp-quote x) "$")))
		     completion-ignored-extensions "\\|"))
	 (path "") (comps-in-path ()) (file "") (filepath "") (completions ()))
    ;; Go thru each path in the search path, finding completions.
    (while paths
      (setq path (file-name-as-directory (comint-directory (or (car paths) ".")))
	    comps-in-path (and (file-accessible-directory-p path)
			       (file-name-all-completions pathnondir path)))
      ;; Go thru each completion found, to see whether it should be used.
      (while comps-in-path
	(setq file (car comps-in-path)
	      filepath (concat path file))
	(if (and (not (member file completions))
		 (not (string-match ignored-extensions file))
		 (or (string-equal path cwd)
		     (not (file-directory-p filepath)))
		 (or (null shell-completion-execonly)
		     (file-executable-p filepath)))
	    (setq completions (cons file completions)))
	(setq comps-in-path (cdr comps-in-path)))
      (setq paths (cdr paths)))
    ;; OK, we've got a list of completions.
    (cond ((null completions)
 	   (message "No completions of %s" filename)
 	   (ding))
 	  ((= 1 (length completions))	; Gotcha!
 	   (let ((completion (car completions)))
 	     (if (string-equal completion pathnondir)
 		 (message "Sole completion")
 	       (insert (substring (directory-file-name completion)
 				  (length pathnondir)))
 	       (message "Completed"))
 	     (if comint-completion-addsuffix
 		 (insert (if (file-directory-p completion) "/" " ")))))
 	  (t				; There's no unique completion.
 	   (let ((completion
 		  (try-completion pathnondir (mapcar (function (lambda (x)
 								 (list x)))
 						     completions))))
 	     ;; Insert the longest substring.
 	     (insert (substring (directory-file-name completion)
 				(length pathnondir)))
 	     (cond ((and comint-completion-recexact comint-completion-addsuffix
 			 (string-equal pathnondir completion)
 			 (member completion completions))
 		    ;; It's not unique, but user wants shortest match.
 		    (insert (if (file-directory-p completion) "/" " "))
 		    (message "Completed shortest"))
 		   ((or comint-completion-autolist
 			(string-equal pathnondir completion))
 		    ;; It's not unique, list possible completions.
 		    (comint-dynamic-list-completions completions))
 		   (t
 		    (message "Partially completed"))))))))

;;; Do the user's customization...
;;;
;;; Isn't this what eval-after-load is for?
;;;(defvar shell-load-hook nil
;;;  "This hook is run when shell is loaded in.
;;;This is a good place to put keybindings.")
;;;	
;;;(run-hooks 'shell-load-hook)

(provide 'shell)

;;; shell.el ends here
