;;; old-shell.el --- run a shell in an Emacs window

;; Copyright (C) 1985, 1986, 1987, 1990 Free Software Foundation, Inc.

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

;;; Hacked from tea.el and shell.el by Olin Shivers (shivers@cs.cmu.edu). 8/88

;;; Since this mode is built on top of the general command-interpreter-in-
;;; a-buffer mode (comint mode), it shares a common base functionality, 
;;; and a common set of bindings, with all modes derived from comint mode.

;;; For documentation on the functionality provided by comint mode, and
;;; the hooks available for customising it, see the file comint.el.

;;; Needs fixin:
;;; When sending text from a source file to a subprocess, the process-mark can 
;;; move off the window, so you can lose sight of the process interactions.
;;; Maybe I should ensure the process mark is in the window when I send
;;; text to the process? Switch selectable?

;;; Code:

(require 'comint)
(defvar shell-popd-regexp "popd"
  "*Regexp to match subshell commands equivalent to popd.")

(defvar shell-pushd-regexp "pushd"
  "*Regexp to match subshell commands equivalent to pushd.")

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

(defvar shell-dirstack nil
  "List of directories saved by pushd in this buffer's shell.")

(defvar shell-dirstack-query "dirs"
  "Command used by shell-resync-dirlist to query shell.")

(defvar shell-mode-map ())
(cond ((not shell-mode-map)
       (setq shell-mode-map (copy-keymap comint-mode-map))
       (define-key shell-mode-map "\t" 'comint-dynamic-complete)
       (define-key shell-mode-map "\M-?"  'comint-dynamic-list-completions)))

(defvar shell-mode-hook '()
  "*Hook for customising shell mode")


;;; Basic Procedures
;;; ===========================================================================
;;;

(defun shell-mode ()
  "Major mode for interacting with an inferior shell.
Return after the end of the process' output sends the text from the 
    end of process to the end of the current line.
Return before end of process output copies rest of line to end (skipping
    the prompt) and sends it.
M-x send-invisible reads a line of text without echoing it, and sends it to
    the shell.

If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it.

cd, pushd and popd commands given to the shell are watched by Emacs to keep
this buffer's default directory the same as the shell's working directory.
M-x dirs queries the shell and resyncs Emacs' idea of what the current 
    directory stack is.
M-x dirtrack-toggle turns directory tracking on and off.

\\{shell-mode-map}
Customisation: Entry to this mode runs the hooks on comint-mode-hook and
shell-mode-hook (in that order).

Variables shell-cd-regexp, shell-pushd-regexp and shell-popd-regexp are used
to match their respective commands."
  (interactive)
  (comint-mode)
  (setq major-mode 'shell-mode
        mode-name "Shell"
        comint-prompt-regexp shell-prompt-pattern
        comint-input-sentinel 'shell-directory-tracker)
  (use-local-map shell-mode-map)
  (make-local-variable 'shell-dirstack)
  (set (make-local-variable 'shell-dirtrackp) t)
  (run-hooks 'shell-mode-hook))


(defun shell ()
  "Run an inferior shell, with I/O through buffer *shell*.
If buffer exists but shell process is not running, make new shell.
If buffer exists and shell process is running, just switch to buffer *shell*.

The shell to use comes from the first non-nil variable found from these:
explicit-shell-file-name in Emacs, ESHELL in the environment or SHELL in the
environment.  If none is found, /bin/sh is used.

If a file ~/.emacs_SHELLNAME exists, it is given as initial input, simulating
a start-up file for the shell like .profile or .cshrc.  Note that this may
lose due to a timing error if the shell discards input when it starts up.

The buffer is put in shell-mode, giving commands for sending input
and controlling the subjobs of the shell.

The shell file name, sans directories, is used to make a symbol name
such as `explicit-csh-arguments'.  If that symbol is a variable,
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
;;; the original version in shell.el. It has the following failings:
;;; 1. It doesn't know about the cdpath shell variable.
;;; 2. It only spots the first command in a command sequence. E.g., it will
;;;    miss the cd in "ls; cd foo"
;;; 3. More generally, any complex command (like ";" sequencing) is going to
;;;    throw it. Otherwise, you'd have to build an entire shell interpreter in
;;;    emacs lisp.  Failing that, there's no way to catch shell commands where
;;;    cd's are buried inside conditional expressions, aliases, and so forth.
;;;
;;; The whole approach is a crock. Shell aliases mess it up. File sourcing
;;; messes it up. You run other processes under the shell; these each have
;;; separate working directories, and some have commands for manipulating
;;; their w.d.'s (e.g., the lcd command in ftp). Some of these programs have
;;; commands that do *not* effect the current w.d. at all, but look like they
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

;;; REGEXP is a regular expression. STR is a string. START is a fixnum.
;;; Returns T if REGEXP matches STR where the match is anchored to start
;;; at position START in STR. Sort of like LOOKING-AT for strings.
(defun shell-front-match (regexp str start)
  (eq start (string-match regexp str start)))

(defun shell-directory-tracker (str)
  "Tracks cd, pushd and popd commands issued to the shell.
This function is called on each input passed to the shell.
It watches for cd, pushd and popd commands and sets the buffer's
default directory to track these commands.

You may toggle this tracking on and off with M-x dirtrack-toggle.
If emacs gets confused, you can resync with the shell with M-x dirs.

See variables shell-cd-regexp, shell-pushd-regexp, and shell-popd-regexp.
Environment variables are expanded, see function substitute-in-file-name."
  (condition-case err
    (cond (shell-dirtrackp
	   (string-match "^\\s *" str) ; skip whitespace
	   (let ((bos (match-end 0))
		 (x nil))
	     (cond ((setq x (shell-match-cmd-w/optional-arg shell-popd-regexp
							       str bos))
		    (shell-process-popd (substitute-in-file-name x)))
		   ((setq x (shell-match-cmd-w/optional-arg shell-pushd-regexp
							       str bos))
		    (shell-process-pushd (substitute-in-file-name x)))
		   ((setq x (shell-match-cmd-w/optional-arg shell-cd-regexp
							       str bos))
		    (shell-process-cd (substitute-in-file-name x)))))))
    (error (message (car (cdr err))))))


;;; Try to match regexp CMD to string, anchored at position START.
;;; CMD may be followed by a single argument. If a match, then return
;;; the argument, if there is one, or the empty string if not. If
;;; no match, return nil.

(defun shell-match-cmd-w/optional-arg (cmd str start)
  (and (shell-front-match cmd str start)
       (let ((eoc (match-end 0))) ; end of command
	 (cond ((shell-front-match "\\s *\\(\;\\|$\\)" str eoc)
		"")			; no arg
	       ((shell-front-match "\\s +\\([^ \t\;]+\\)\\s *\\(\;\\|$\\)"
				      str eoc)
		(substring str (match-beginning 1) (match-end 1))) ; arg
	       (t nil))))) ; something else.
;;; The first regexp is [optional whitespace, (";" or the end of string)].
;;; The second regexp is [whitespace, (an arg), optional whitespace,
;;;     (";" or end of string)].


;;; popd [+n]
(defun shell-process-popd (arg)
  (let ((num (if (zerop (length arg)) 0 ; no arg means +0
		 (shell-extract-num arg))))
    (if (and num (< num (length shell-dirstack)))
	(if (= num 0) ; condition-case because the CD could lose.
	    (condition-case nil (progn (cd (car shell-dirstack))
				       (setq shell-dirstack
					     (cdr shell-dirstack))
				       (shell-dirstack-message))
	      (error (message "Couldn't cd.")))
	    (let* ((ds (cons nil shell-dirstack))
		   (cell (nthcdr (- num 1) ds)))
	      (rplacd cell (cdr (cdr cell)))
	      (setq shell-dirstack (cdr ds))
	      (shell-dirstack-message)))
	(message "Bad popd."))))


;;; cd [dir]
(defun shell-process-cd (arg)
  (condition-case nil (progn (cd (if (zerop (length arg)) (getenv "HOME")
				     arg))
			     (shell-dirstack-message))
	   (error (message "Couldn't cd."))))


;;; pushd [+n | dir]
(defun shell-process-pushd (arg)
  (if (zerop (length arg))
      ;; no arg -- swap pwd and car of shell stack
      (condition-case nil (if shell-dirstack
			      (let ((old default-directory))
				(cd (car shell-dirstack))
				(setq shell-dirstack
				      (cons old (cdr shell-dirstack)))
				(shell-dirstack-message))
			      (message "Directory stack empty."))
	(message "Couldn't cd."))

      (let ((num (shell-extract-num arg)))
	(if num				; pushd +n
	    (if (> num (length shell-dirstack))
		(message "Directory stack not that deep.")
		(let* ((ds (cons default-directory shell-dirstack))
		       (dslen (length ds))
		       (front (nthcdr num ds))
		       (back (reverse (nthcdr (- dslen num) (reverse ds))))
		       (new-ds (append front back)))
		  (condition-case nil
		      (progn (cd (car new-ds))
			     (setq shell-dirstack (cdr new-ds))
			     (shell-dirstack-message))
		    (error (message "Couldn't cd.")))))
	       
	    ;; pushd <dir>
	    (let ((old-wd default-directory))
	      (condition-case nil
		  (progn (cd arg)
			 (setq shell-dirstack
			       (cons old-wd shell-dirstack))
			 (shell-dirstack-message))
		(error (message "Couldn't cd."))))))))

;; If STR is of the form +n, for n>0, return n. Otherwise, nil.
(defun shell-extract-num (str)
  (and (string-match "^\\+[1-9][0-9]*$" str)
       (string-to-int str)))


(defun shell-dirtrack-toggle ()
  "Turn directory tracking on and off in a shell buffer."
  (interactive)
  (setq shell-dirtrackp (not shell-dirtrackp))
  (message "directory tracking %s."
	   (if shell-dirtrackp "ON" "OFF")))

;;; For your typing convenience:
(fset 'dirtrack-toggle 'shell-dirtrack-toggle)


(defun shell-resync-dirs ()
  "Resync the buffer's idea of the current directory stack.
This command queries the shell with the command bound to 
shell-dirstack-query (default \"dirs\"), reads the next
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
    (let* ((dl (buffer-substring (match-beginning 0) (- (match-end 0) 1)))
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
(fset 'dirs 'shell-resync-dirs)


;;; Show the current dirstack on the message line.
;;; Pretty up dirs a bit by changing "/usr/jqr/foo" to "~/foo".
;;; (This isn't necessary if the dirlisting is generated with a simple "dirs".)
;;; All the commands that mung the buffer's dirstack finish by calling
;;; this guy.
(defun shell-dirstack-message ()
  (let ((msg "")
	(ds (cons default-directory shell-dirstack)))
    (while ds
      (let ((dir (car ds)))
	(if (string-match (format "^%s\\(/\\|$\\)" (getenv "HOME")) dir)
	    (setq dir (concat "~/" (substring dir (match-end 0)))))
	(if (string-equal dir "~/") (setq dir "~"))
	(setq msg (concat msg dir " "))
	(setq ds (cdr ds))))
    (message msg)))

(provide 'shell)

;;; old-shell.el ends here
