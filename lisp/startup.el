;;; startup.el --- process Emacs shell arguments

;; Copyright (C) 1985, 1986, 1992 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: internal

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

; These are processed only at the beginning of the argument list.
; -batch		execute noninteractively (messages go to stdout,
;			 variable noninteractive set to t)
;			 This option must be the first in the arglist.
;			 Processed by `main' in emacs.c -- never seen by lisp
; -t file		Specify to use file rather than stdin/stdout
;			 as the terminal.
;			 This option must be the first in the arglist.
;			 Processed by `main' in emacs.c -- never seen by lisp
; -nw			Inhibit the use of any window-system-specific display
;			 code; use the current virtual terminal.
;			 This option must be the first in the arglist.
;			 Processed by `main' in emacs.c -- never seen by lisp
; -q			load no init file
; -no-init-file		same
; -u user		load user's init file
; -user user		same
; -debug-init		Don't catch errors in init file; let debugger run.

; These are processed in the order encountered.
; -f function		execute function
; -funcall function	same
; -l file		load file
; -load file		same
; -insert file		same
; file			visit file
; -kill			kill (exit) emacs

;;; Code:

(setq top-level '(normal-top-level))

(defvar command-line-processed nil "t once command line has been processed")

(defconst inhibit-startup-message nil
  "*Non-nil inhibits the initial startup messages.
This is for use in your personal init file, once you are familiar
with the contents of the startup message.")

(defconst inhibit-default-init nil
  "*Non-nil inhibits loading the `default' library.")

(defconst command-switch-alist nil
  "Alist of command-line switches.
Elements look like (SWITCH-STRING . HANDLER-FUNCTION).
HANDLER-FUNCTION receives switch name as sole arg;
remaining command-line args are in the variable `command-line-args-left'.")

(defvar command-line-functions nil    ;; lrs 7/31/89
  "List of functions to process unrecognized command-line arguments.
Each function should access the dynamically bound variables
argi (the current argument) and command-line-args-left (the remaining
arguments).  The function should return non-nil only if it recognizes and
processes argi.  If it does so, it may consume successive arguments by
altering command-line-args-left to remove them.")

(defvar before-init-hook nil
  "Functions to call after handling urgent options but before loading init file.
The frame system uses this to open frames to display messages while
Emacs loads the user's initialization file.")

(defvar after-init-hook nil
  "Functions to call after loading the init file (`~/.emacs').
The call is not protected by a condition-case, so you can set `debug-on-error'
in `.emacs', and put all the actual code on `after-init-hook'.")

(defvar term-setup-hook nil
  "Functions to be called after loading terminal-specific lisp code.
See `run-hooks'.  This variable exists for users to set,
so as to override the definitions made by the terminal-specific file.
Emacs never sets this variable itself.")

(defvar keyboard-type nil
  "The brand of keyboard you are using.  This variable is used to define
the proper function and keypad keys for use under X.  It is used in a
fashion analogous to the environment value TERM.")

(defvar window-setup-hook nil
  "Function called to initialize window system display.
Emacs calls this after processing the command line arguments and loading
the user's init file.

Users should not set this variable; use term-setup-hook instead.")

(defconst initial-major-mode 'lisp-interaction-mode
  "Major mode command symbol to use for the initial *scratch* buffer.")

(defvar init-file-user nil
  "Identity of user whose `.emacs' file is or was read.
The value may be the null string or a string containing a user's name.
If the value is a null string, it means that the init file was taken from
the user that originally logged in.

In all cases, `(concat \"~\" init-file-user \"/\")' evaluates to the
directory name of the directory where the `.emacs' file was looked for.")

(defvar site-run-file "site-start"
  "File containing site-wide run-time initializations.
This file is loaded at run-time before `~/.emacs'.  It contains inits
that need to be in place for the entire site, but which, due to their
higher incidence of change, don't make sense to load into emacs'
dumped image.  Thus, the run-time load order is: 1. file described in
this variable, if non-nil; 2. `~/.emacs'; 3. `default.el'.")

(defvar init-file-debug nil)

(defvar init-file-had-error nil)

(defun normal-top-level ()
  (if command-line-processed
      (message "Back to top level.")
    (setq command-line-processed t)
    (if (not (eq system-type 'vax-vms))
	(progn
	  ;; If the PWD environment variable isn't accurate, delete it.
	  (let ((pwd (getenv "PWD")))
	    (and (stringp pwd)
		 ;; Use FOO/., so that if FOO is a symlink, file-attributes
		 ;; describes the directory linked to, not FOO itself.
		 (or (equal (file-attributes
			     (concat (file-name-as-directory pwd) "."))
			    (file-attributes
			     (concat (file-name-as-directory default-directory)
				     ".")))
		     (setq process-environment
			   (delete (concat "PWD=" pwd)
				   process-environment)))))))
    (setq default-directory (abbreviate-file-name default-directory))
    (unwind-protect
	(command-line)
      ;; Do this again, in case .emacs defined more abbreviations.
      (setq default-directory (abbreviate-file-name default-directory))
      (run-hooks 'emacs-startup-hook)
      (and term-setup-hook
	   (run-hooks 'term-setup-hook))
      ;; Modify the initial frame based on what .emacs puts into
      ;; ...-frame-alist.
      (if (fboundp 'frame-notice-user-settings)
	  (frame-notice-user-settings))
      ;; Now we know the user's default font, so add it to the menu.
      (if (fboundp 'font-menu-add-default)
	  (font-menu-add-default))
      (and window-setup-hook
	   (run-hooks 'window-setup-hook)))))

(defun command-line ()
  ;; See if we should import version-control from the environment variable.
  (let ((vc (getenv "VERSION_CONTROL")))
    (cond ((eq vc nil))			;don't do anything if not set
	  ((or (string= vc "t")
	       (string= vc "numbered"))
	   (setq version-control t))
	  ((or (string= vc "nil")
	       (string= vc "existing"))
	   (setq version-control nil))
	  ((or (string= vc "never")
	       (string= vc "simple"))
	   (setq version-control 'never))))

  ;;! This has been commented out; I currently find the behavior when
  ;;! split-window-keep-point is nil disturbing, but if I can get used
  ;;! to it, then it would be better to eliminate the option.
  ;;! ;; Choose a good default value for split-window-keep-point.
  ;;! (setq split-window-keep-point (> baud-rate 2400))

  ;; Read window system's init file if using a window system.
  (if (and window-system (not noninteractive))
      (load (concat term-file-prefix
		    (symbol-name window-system)
		    "-win")
	    ;; Every window system should have a startup file;
	    ;; barf if we can't find it.
	    nil t))

  (let ((done nil)
	(args (cdr command-line-args)))

    ;; Figure out which user's init file to load,
    ;; either from the environment or from the options.
    (setq init-file-user (if noninteractive nil (user-login-name)))
    ;; If user has not done su, use current $HOME to find .emacs.
    (and init-file-user (string= init-file-user (user-real-login-name))
	 (setq init-file-user ""))

    ;; Process the command-line args, and delete the arguments
    ;; processed.  This is consistent with the way main in emacs.c
    ;; does things.
    (while (and (not done) args)
      (let ((argi (car args)))
	(cond
	 ((or (string-equal argi "-q")
	      (string-equal argi "-no-init-file"))
	  (setq init-file-user nil
		args (cdr args)))
	 ((or (string-equal argi "-u")
	      (string-equal argi "-user"))
	  (setq args (cdr args)
		init-file-user (car args)
		args (cdr args)))
	 ((string-equal argi "-no-site-file")
	  (setq site-run-file nil
		args (cdr args)))
	 ((string-equal argi "-debug-init")
	  (setq init-file-debug t
		args (cdr args)))
	 (t (setq done t)))))
    
    ;; Re-attach the program name to the front of the arg list.
    (setcdr command-line-args args))

  ;; Under X Windows, this creates the X frame and deletes the terminal frame.
  (if (fboundp 'frame-initialize)
      (frame-initialize))
  (if (fboundp 'face-initialize)
      (face-initialize))

  (run-hooks 'before-init-hook)

  ;; Run the site-start library if it exists.  The point of this file is
  ;; that it is run before .emacs.  There is no point in doing this after
  ;; .emacs; that is useless.
  (if site-run-file 
      (load site-run-file t t))

  ;; Sites should not disable this.  Only individuals should disable
  ;; the startup message.
  (setq inhibit-startup-message nil)

  ;; Load that user's init file, or the default one, or none.
  (let ((debug-on-error init-file-debug)
	;; This function actually reads the init files.
	(inner
	 (function
	  (lambda ()
	    (if init-file-user
		(progn (load (if (eq system-type 'vax-vms)
				 "sys$login:.emacs"
			       (concat "~" init-file-user "/.emacs"))
			     t t t)
		       (or inhibit-default-init
			   (let ((inhibit-startup-message nil))
			     ;; Users are supposed to be told their rights.
			     ;; (Plus how to get help and how to undo.)
			     ;; Don't you dare turn this off for anyone
			     ;; except yourself.
			     (load "default" t t)))))))))
    (if init-file-debug
	;; Do this without a condition-case if the user wants to debug.
	(funcall inner)
      (condition-case error
	  (progn
	    (funcall inner)
	    (setq init-file-had-error nil))
	(error (message "Error in init file: %s%s%s"
			(get (car error) 'error-message)
			(if (cdr error) ": ")
			(mapconcat 'prin1-to-string (cdr error) ", "))
	       (setq init-file-had-error t)))))

  (run-hooks 'after-init-hook)

  ;; If *scratch* exists and init file didn't change its mode, initialize it.
  (if (get-buffer "*scratch*")
      (save-excursion
	(set-buffer "*scratch*")
	(if (eq major-mode 'fundamental-mode)
	    (funcall initial-major-mode))))
  ;; Load library for our terminal type.
  ;; User init file can set term-file-prefix to nil to prevent this.
  (and term-file-prefix (not noninteractive) (not window-system)
       (let ((term (getenv "TERM"))
	     hyphend)
	 (while (and term
		     (not (load (concat term-file-prefix term) t t)))
	   ;; Strip off last hyphen and what follows, then try again
	   (if (setq hyphend (string-match "[-_][^-_]+$" term))
	       (setq term (substring term 0 hyphend))
	     (setq term nil)))))

  ;; Process the remaining args.
  (command-line-1 (cdr command-line-args))

  ;; If -batch, terminate after processing the command options.
  (if noninteractive (kill-emacs t)))

(defun command-line-1 (command-line-args-left)
  (or noninteractive (input-pending-p) init-file-had-error
      (message (if (eq (key-binding "\C-h\C-p") 'describe-project)
		   "For information about the GNU Project and its goals, type C-h C-p."
		 (substitute-command-keys
		  "For information about the GNU Project and its goals, type \\[describe-project]."))))
  (if (null command-line-args-left)
      (cond ((and (not inhibit-startup-message) (not noninteractive)
		  ;; Don't clobber a non-scratch buffer if init file
		  ;; has selected it.
		  (string= (buffer-name) "*scratch*")
		  (not (input-pending-p)))
	     ;; If there are no switches to process, we might as well
	     ;; run this hook now, and there may be some need to do it
	     ;; before doing any output.
	     (and term-setup-hook
		  (run-hooks 'term-setup-hook))
	     ;; Don't let the hook be run twice.
	     (setq term-setup-hook nil)

	     ;; It's important to notice the user settings before we
	     ;; display the startup message; otherwise, the settings
	     ;; won't take effect until the user gives the first
	     ;; keystroke, and that's distracting.
	     (if (fboundp 'frame-notice-user-settings)
		 (frame-notice-user-settings))

	     (and window-setup-hook
		  (run-hooks 'window-setup-hook))
	     (setq window-setup-hook nil)
	     (unwind-protect
		 (progn
		   (insert (emacs-version)
			   "
Copyright (C) 1993 Free Software Foundation, Inc.\n\n")
		   ;; If keys have their default meanings,
		   ;; use precomputed string to save lots of time.
		   (if (and (eq (key-binding "\C-h") 'help-command)
			    (eq (key-binding "\C-xu") 'advertised-undo)
			    (eq (key-binding "\C-x\C-c") 'save-buffers-kill-emacs)
			    (eq (key-binding "\C-h\C-c") 'describe-copying)
			    (eq (key-binding "\C-h\C-d") 'describe-distribution)
			    (eq (key-binding "\C-h\C-w") 'describe-no-warranty)
			    (eq (key-binding "\C-ht") 'help-with-tutorial))
		       (insert 
       "Type C-h for help; C-x u to undo changes.  (`C-' means use CTRL key.)
To kill the Emacs job, type C-x C-c.
Type C-h t for a tutorial on using Emacs.
Type C-h i to enter Info, which you can use to read GNU documentation.

GNU Emacs comes with ABSOLUTELY NO WARRANTY; type C-h C-w for full details.
You may give out copies of Emacs; type C-h C-c to see the conditions.
Type C-h C-d for information on getting the latest version.")
		     (insert (substitute-command-keys
       "Type \\[help-command] for help; \\[advertised-undo] to undo changes.  (`C-' means use CTRL key.)
To kill the Emacs job, type \\[save-buffers-kill-emacs].
Type \\[help-with-tutorial] for a tutorial on using Emacs.
Type \\[info] to enter Info, which you can use to read GNU documentation.

GNU Emacs comes with ABSOLUTELY NO WARRANTY; type \\[describe-no-warranty] for full details.
You may give out copies of Emacs; type \\[describe-copying] to see the conditions.
Type \\[describe-distribution] for information on getting the latest version.")))
		   (set-buffer-modified-p nil)
		   (sit-for 120))
	       (save-excursion
		 ;; In case the Emacs server has already selected
		 ;; another buffer, erase the one our message is in.
		 (set-buffer (get-buffer "*scratch*"))
		 (erase-buffer)
		 (set-buffer-modified-p nil)))))
    (let ((dir default-directory)
	  (file-count 0)
	  first-file-buffer
	  (line 0))
      (while command-line-args-left
	(let ((argi (car command-line-args-left))
	      tem)
	  (setq command-line-args-left (cdr command-line-args-left))
	  (cond ((setq tem (assoc argi command-switch-alist))
		 (funcall (cdr tem) argi))
		((or (string-equal argi "-f")  ;what the manual claims
		     (string-equal argi "-funcall")
		     (string-equal argi "-e")) ; what the source used to say
		 (setq tem (intern (car command-line-args-left)))
		 (setq command-line-args-left (cdr command-line-args-left))
		 (funcall tem))
		((or (string-equal argi "-l")
		     (string-equal argi "-load"))
		 (let ((file (car command-line-args-left)))
		   ;; Take file from default dir if it exists there;
		   ;; otherwise let `load' search for it.
		   (if (file-exists-p (expand-file-name file))
		       (setq file (expand-file-name file)))
		   (load file nil t))
		 (setq command-line-args-left (cdr command-line-args-left)))
		((string-equal argi "-insert")
		 (or (stringp (car command-line-args-left))
		     (error "filename omitted from `-insert' option"))
		 (insert-file-contents (car command-line-args-left))
		 (setq command-line-args-left (cdr command-line-args-left)))
		((string-equal argi "-kill")
		 (kill-emacs t))
		((string-match "^\\+[0-9]+\\'" argi)
		 (setq line (string-to-int argi)))
		(t
		 ;; We have almost exhausted our options. See if the
		 ;; user has made any other command-line options available
		 (let ((hooks command-line-functions);; lrs 7/31/89
		       (did-hook nil))
		   (while (and hooks
			       (not (setq did-hook (funcall (car hooks)))))
		     (setq hooks (cdr hooks)))
		   (if (not did-hook)
		       ;; Ok, presume that the argument is a file name
		       (progn
			 (setq file-count (1+ file-count))
			 (cond ((= file-count 1)
				(setq first-file-buffer
				      (find-file (expand-file-name argi dir))))
			       (t
				(find-file-other-window (expand-file-name argi dir))))
			 (or (zerop line)
			     (goto-line line))
			 (setq line 0))))))))
      ;; If 3 or more files visited, and not all visible,
      ;; show user what they all are.
      (if (> file-count 2)
	  (or (get-buffer-window first-file-buffer)
	      (progn (other-window 1)
		     (buffer-menu)))))))

;;; startup.el ends here
