;;; startup.el --- process Emacs shell arguments

;; Copyright (C) 1985, 1986, 1992, 1994 Free Software Foundation, Inc.

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

;; This is a list of options processed in this file.
;; There are also -- forms, but they aren't listed here.
;; There are also options for X windows, not listed here;
;; see term/x-win.el.

; These options are processed first, no matter where they appear.
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
; -q			load no init file; don't load default.el either.
;			  But this has no effect on site-start.el.
; -no-init-file		same
; -u user		load USER's init file instead of your own.
; -user user		same
; -debug-init		Don't catch errors in init file; let debugger run.
; -no-site-file		Don't load site-start.el.
;			  (This is the ONLY way to prevent loading that file.)

; These are processed in the order encountered.
; -f function		execute function
; -funcall function	same
; -l file		load file
; -load file		same
; -insert file		insert file into buffer
; -L dir		add dir to load-path
; file			visit file

; This is always processed last, no matter where it appears.
; -kill			kill (exit) emacs

;;; Code:

(setq top-level '(normal-top-level))

(defvar command-line-processed nil "t once command line has been processed")

(defconst inhibit-startup-message nil
  "*Non-nil inhibits the initial startup message.
This is for use in your personal init file, once you are familiar
with the contents of the startup message.")

(defconst inhibit-startup-echo-area-message nil
  "*Non-nil inhibits the initial startup echo area message.
Inhibition takes effect only if your `.emacs' file contains
a line of this form:
 (setq inhibit-startup-echo-area-message \"YOUR-USER-NAME\")
If your `.emacs' file is byte-compiled, use the following form instead:
 (eval '(setq inhibit-startup-echo-area-message \"YOUR-USER-NAME\"))
Thus, someone else using a copy of your `.emacs' file will see
the startup message unless he personally acts to inhibit it.")

(defconst inhibit-default-init nil
  "*Non-nil inhibits loading the `default' library.")

(defconst command-switch-alist nil
  "Alist of command-line switches.
Elements look like (SWITCH-STRING . HANDLER-FUNCTION).
HANDLER-FUNCTION receives switch name as sole arg;
remaining command-line args are in the variable `command-line-args-left'.")

(defvar command-line-args-left nil
  "List of command-line args not yet processed.")

(defvar command-line-functions nil    ;; lrs 7/31/89
  "List of functions to process unrecognized command-line arguments.
Each function should access the dynamically bound variables
`argi' (the current argument) and `command-line-args-left' (the remaining
arguments).  The function should return non-nil only if it recognizes and
processes `argi'.  If it does so, it may consume successive arguments by
altering `command-line-args-left' to remove them.")

(defvar command-line-default-directory nil
  "Default directory to use for command line arguments.
This is normally copied from `default-directory' when Emacs starts.")

(defvar before-init-hook nil
  "Functions to call after handling urgent options but before init files.
The frame system uses this to open frames to display messages while
Emacs loads the user's initialization file.")

(defvar after-init-hook nil
  "Functions to call after loading the init file (`~/.emacs').
The call is not protected by a condition-case, so you can set `debug-on-error'
in `.emacs', and put all the actual code on `after-init-hook'.")

(defvar term-setup-hook nil
  "Functions to be called after loading terminal-specific Lisp code.
See `run-hooks'.  This variable exists for users to set,
so as to override the definitions made by the terminal-specific file.
Emacs never sets this variable itself.")

(defvar keyboard-type nil
  "The brand of keyboard you are using.
This variable is used to define
the proper function and keypad keys for use under X.  It is used in a
fashion analogous to the environment value TERM.")

(defvar window-setup-hook nil
  "Normal hook run to initialize window system display.
Emacs runs this hook after processing the command line arguments and loading
the user's init file.")

(defconst initial-major-mode 'lisp-interaction-mode
  "Major mode command symbol to use for the initial *scratch* buffer.")

(defvar init-file-user nil
  "Identity of user whose `.emacs' file is or was read.
The value is nil if no init file is being used; otherwise, it may be either
the null string, meaning that the init file was taken from the user that
originally logged in, or it may be a string containing a user's name.

In either of the latter cases, `(concat \"~\" init-file-user \"/\")'
evaluates to the name of the directory where the `.emacs' file was
looked for.

Setting `init-file-user' does not prevent Emacs from loading
`site-start.el'.  The only way to do that is to use `--no-site-file'.")

(defvar site-run-file "site-start"
  "File containing site-wide run-time initializations.
This file is loaded at run-time before `~/.emacs'.  It contains inits
that need to be in place for the entire site, but which, due to their
higher incidence of change, don't make sense to load into emacs'
dumped image.  Thus, the run-time load order is: 1. file described in
this variable, if non-nil; 2. `~/.emacs'; 3. `default.el'.

Don't use the `site-start.el' file for things some users may not like.
Put them in `default.el' instead, so that users can more easily
override them.  Users can prevent loading `default.el' with the `-q'
option or by setting `inhibit-default-init' in their own init files,
but inhibiting `site-start.el' requires `--no-site-file', which
is less convenient.")

(defconst iso-8859-1-locale-regexp "8859[-_]?1"
  "Regexp that specifies when to enable the ISO 8859-1 character set.
We do that if this regexp matches the locale name
specified by the LC_ALL, LC_CTYPE and LANG environment variables.")

(defvar mail-host-address nil
  "*Name of this machine, for purposes of naming users.")

(defvar user-mail-address nil
  "*Full mailing address of this user.")

(defvar init-file-debug nil)

(defvar init-file-had-error nil)

;; This function is called from the subdirs.el file.
(defun normal-top-level-add-to-load-path (dirs)
  (let ((tail (member default-directory load-path)))
    (setcdr tail (append (mapcar 'expand-file-name dirs) (cdr tail)))))

(defun normal-top-level ()
  (if command-line-processed
      (message "Back to top level.")
    (setq command-line-processed t)
    ;; Give *Messages* the same default-directory as *scratch*,
    ;; just to keep things predictable.
    (let ((dir default-directory))
      (save-excursion
	(set-buffer (get-buffer "*Messages*"))
	(setq default-directory dir)))
    ;; Look in each dir in load-path for a subdirs.el file.
    ;; If we find one, load it, which will add the appropriate subdirs
    ;; of that dir into load-path,
    (let ((tail load-path)
	  new)
      (while tail
	(setq new (cons (car tail) new))
	(let ((default-directory (car tail)))
	  (load (expand-file-name "subdirs.el" (car tail)) t t t))
	(setq tail (cdr tail))))
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
    (setq user-mail-address (concat (user-login-name) "@"
				    (or mail-host-address
					(system-name))))
    ;; Specify the file for recording all the auto save files of this session.
    ;; This is used by multiple-recover.
    (setq auto-save-list-file-name
	  (expand-file-name
	   (format "~/.saves-%d-%s"
		   (emacs-pid)
		   (or mail-host-address (system-name)))))
    (let ((menubar-bindings-done nil))
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
	     (run-hooks 'window-setup-hook))
	(or menubar-bindings-done
	    (precompute-menubar-bindings))))))

;; Precompute the keyboard equivalents in the menu bar items.
(defun precompute-menubar-bindings ()
  (if (eq window-system 'x)
      (let ((submap (lookup-key global-map [menu-bar])))
	(while submap
	  (and (consp (car submap))
	       (symbolp (car (car submap)))
	       (stringp (car-safe (cdr (car submap))))
	       (keymapp (cdr (cdr (car submap))))
	       (x-popup-menu nil (cdr (cdr (car submap)))))
	  (setq submap (cdr submap))))))

(defun command-line ()
  (setq command-line-default-directory default-directory)

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

  (if (let ((ctype
	     ;; Use the first of these three envvars that has a nonempty value.
	     (or (let ((string (getenv "LC_ALL")))
		   (and (not (equal string "")) string))
		 (let ((string (getenv "LC_CTYPE")))
		   (and (not (equal string "")) string))
		 (let ((string (getenv "LANG")))
		   (and (not (equal string "")) string)))))
	(and ctype
	     (string-match iso-8859-1-locale-regexp ctype)))
      (progn 
	(require 'disp-table)
	(standard-display-european t)
	(require 'iso-syntax)))

  ;;! This has been commented out; I currently find the behavior when
  ;;! split-window-keep-point is nil disturbing, but if I can get used
  ;;! to it, then it would be better to eliminate the option.
  ;;! ;; Choose a good default value for split-window-keep-point.
  ;;! (setq split-window-keep-point (> baud-rate 2400))

  ;; Read window system's init file if using a window system.
  (condition-case error
      (if (and window-system (not noninteractive))
	  (load (concat term-file-prefix
			(symbol-name window-system)
			"-win")
		;; Every window system should have a startup file;
		;; barf if we can't find it.
		nil t))
    ;; If we can't read it, print the error message and exit.
    (error
     (princ
      (if (eq (car error) 'error)
	  (apply 'concat (cdr error))
	(if (memq 'file-error (get (car error) 'error-conditions))
	    (format "%s: %s"
		     (nth 1 error)
		     (mapconcat '(lambda (obj) (prin1-to-string obj t))
				(cdr (cdr error)) ", "))
	  (format "%s: %s"
		   (get (car error) 'error-message)
		   (mapconcat '(lambda (obj) (prin1-to-string obj t))
			      (cdr error) ", "))))
      'external-debugging-output)
     (setq window-system nil)
     (kill-emacs)))

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
      (let ((longopts '(("--no-init-file") ("--no-site-file") ("--user")
			("--debug-init") ("--iconic") ("--icon-type")))
	    (argi (car args))
	    (argval nil))
	(if (string-match "=" argi)
	    (setq argval (substring argi (match-end 0))
		  argi (substring argi 0 (match-beginning 0))))
	(let ((completion (try-completion argi longopts)))
	  (if (eq completion t)
	      (setq argi (substring argi 1))
	    (if (stringp completion)
		(let ((elt (assoc completion longopts)))
		  (or elt
		      (error "Option `%s' is ambiguous" argi))
		  (setq argi (substring (car elt) 1)))
	      (setq argval nil))))
	(cond
	 ((or (string-equal argi "-q")
	      (string-equal argi "-no-init-file"))
	  (setq init-file-user nil
		args (cdr args)))
	 ((or (string-equal argi "-u")
	      (string-equal argi "-user"))
	  (or argval
	      (setq argval (car args)
		    args (cdr args)))
	  (setq init-file-user argval
		argval nil
		args (cdr args)))
	 ((string-equal argi "-no-site-file")
	  (setq site-run-file nil
		args (cdr args)))
	 ((string-equal argi "-debug-init")
	  (setq init-file-debug t
		args (cdr args)))
	 ((string-equal argi "-iconic")
	  (setq initial-frame-alist
		(cons '(visibility . icon) initial-frame-alist))
	  (setq args (cdr args)))
	 ((or (string-equal argi "-icon-type")
	      (string-equal argi "-i")
	      (string-equal argi "-itype"))
	  (setq default-frame-alist
		(cons '(icon-type . t) default-frame-alist))
	  (setq args (cdr args)))
	 (t (setq done t)))
	;; Was argval set but not used?
	(and argval
	     (error "Option `%s' doesn't allow an argument" argi))))

    ;; Re-attach the program name to the front of the arg list.
    (setcdr command-line-args args))

  ;; Under X Windows, this creates the X frame and deletes the terminal frame.
  (if (fboundp 'face-initialize)
      (face-initialize))
  (if (fboundp 'frame-initialize)
      (frame-initialize))
  ;; If frame was created with a menu bar, set menu-bar-mode on.
  (if (and (eq window-system 'x)
	   (> (cdr (assq 'menu-bar-lines (frame-parameters))) 0))
      (menu-bar-mode t))

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
  (let (debug-on-error-from-init-file
	debug-on-error-should-be-set
	(debug-on-error-initial
	 (if (eq init-file-debug t) 'startup init-file-debug)))
    (let ((debug-on-error debug-on-error-initial)
	  ;; This function actually reads the init files.
	  (inner
	   (function
	    (lambda ()
	      (if init-file-user
		  (progn
		    (setq user-init-file 
			  (cond 
			   ((eq system-type 'ms-dos)
			    (concat "~" init-file-user "/_emacs"))
			   ((eq system-type 'windows-nt)
			    "~/_emacs")
			   ((eq system-type 'vax-vms) 
			    "sys$login:.emacs")
			   (t 
			    (concat "~" init-file-user "/.emacs"))))
		    (load user-init-file t t t)
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
			  (if (cdr error) ": " "")
			  (mapconcat 'prin1-to-string (cdr error) ", "))
		 (setq init-file-had-error t))))
      ;; If we can tell that the init file altered debug-on-error,
      ;; arrange to preserve the value that it set up.
      (or (eq debug-on-error debug-on-error-initial)
	  (setq debug-on-error-should-be-set t
		debug-on-error-from-init-file debug-on-error)))
    (if debug-on-error-should-be-set
	(setq debug-on-error debug-on-error-from-init-file)))

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
      (and inhibit-startup-echo-area-message
	   (let ((buffer (get-buffer-create " *temp*")))
	     (prog1
		 (condition-case nil
		     (save-excursion
		       (set-buffer buffer)
		       (insert-file-contents user-init-file)
		       (re-search-forward
			(concat
			 "([ \t\n]*setq[ \t\n]+"
			 "inhibit-startup-echo-area-message[ \t\n]+"
			 (regexp-quote
			  (prin1-to-string
			   (if (string= init-file-user "")
			       (user-login-name)
			     init-file-user)))
			 "[ \t\n]*)")
			nil t))
		   (error nil))
	       (kill-buffer buffer))))
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
	     ;; Do this now to avoid an annoying delay if the user
	     ;; clicks the menu bar during the sit-for.
	     (precompute-menubar-bindings)
	     (setq menubar-bindings-done t)
	     (unwind-protect
		 (progn
		   (insert (emacs-version)
			   "
Copyright (C) 1994 Free Software Foundation, Inc.\n\n")
		   ;; If keys have their default meanings,
		   ;; use precomputed string to save lots of time.
		   (if (and (eq (key-binding "\C-h") 'help-command)
			    (eq (key-binding "\C-xu") 'advertised-undo)
			    (eq (key-binding "\C-x\C-c") 'save-buffers-kill-emacs)
			    (eq (key-binding "\C-ht") 'help-with-tutorial)
			    (eq (key-binding "\C-hi") 'info))
		       (insert 
       "Type C-h for help; C-x u to undo changes.  (`C-' means use CTRL key.)
To kill the Emacs job, type C-x C-c.
Type C-h t for a tutorial on using Emacs.
Type C-h i to enter Info, which you can use to read GNU documentation.")
		     (insert (substitute-command-keys
			      (format "Type %s for help; \\[advertised-undo] to undo changes.  (`C-' means use CTRL key.)
To kill the Emacs job, type \\[save-buffers-kill-emacs].
Type \\[help-with-tutorial] for a tutorial on using Emacs.
Type \\[info] to enter Info, which you can use to read GNU documentation."
				      (let ((where (where-is-internal
						    'help-command nil t)))
					(if where
					    (key-description where)
					  "M-x help"))))))

		   ;; Windows and MSDOS (currently) do not count as
		   ;; window systems, but do have mouse support.
		   (if (or (memq system-type '(msdos windowsnt))
			   window-system)
		       (insert "
C-mouse-3 (third mouse button, with Control) gets a mode-specific menu."))
		   (insert "\n")
		   (if (and (eq (key-binding "\C-h\C-c") 'describe-copying)
			    (eq (key-binding "\C-h\C-d") 'describe-distribution)
			    (eq (key-binding "\C-h\C-w") 'describe-no-warranty))
		       (insert 
			"
GNU Emacs comes with ABSOLUTELY NO WARRANTY; type C-h C-w for full details.
You may give out copies of Emacs; type C-h C-c to see the conditions.
Type C-h C-d for information on getting the latest version.")
		     (insert (substitute-command-keys
			      "
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
    ;; Delay 2 seconds after the init file error message
    ;; was displayed, so user can read it.
    (if init-file-had-error
	(sit-for 2))
    (let ((dir command-line-default-directory)
	  (file-count 0)
	  first-file-buffer
	  (line 0))
      (while command-line-args-left
	(let* ((argi (car command-line-args-left))
	       (orig-argi argi)
	       ;; This includes our standard options' long versions
	       ;; and long versions of what's on command-switch-alist.
	       (longopts
	        (append '(("--funcall") ("--load") ("--insert") ("--kill")
			  ("--directory"))
			(mapcar '(lambda (elt)
				   (list (concat "-" (car elt))))
				command-switch-alist)))
	       tem argval completion
	       ;; List of directories specified in -L/--directory,
	       ;; in reverse of the order specified.
	       extra-load-path
	       (initial-load-path load-path))
	  (setq command-line-args-left (cdr command-line-args-left))

	  ;; Convert long options to ordinary options
	  ;; and separate out an attached option argument into argval.
	  (if (string-match "^--[^=]*=" argi)
	      (setq argval (substring argi (match-end 0))
		    argi (substring argi 0 (1- (match-end 0)))))
	  (setq completion (try-completion argi longopts))
	  (if (eq completion t)
	      (setq argi (substring argi 1))
	    (if (stringp completion)
		(let ((elt (assoc completion longopts)))
		  (or elt
		      (error "Option `%s' is ambiguous" argi))
		  (setq argi (substring (car elt) 1)))
	      (setq argval nil argi orig-argi)))

	  ;; Execute the option.
	  (cond ((setq tem (assoc argi command-switch-alist))
		 (if argval
		     (let ((command-line-args-left
			    (cons argval command-line-args-left)))
		       (funcall (cdr tem) argi))
		   (funcall (cdr tem) argi)))
		((or (string-equal argi "-f")  ;what the manual claims
		     (string-equal argi "-funcall")
		     (string-equal argi "-e")) ; what the source used to say
		 (if argval
		     (setq tem (intern argval))
		   (setq tem (intern (car command-line-args-left)))
		   (setq command-line-args-left (cdr command-line-args-left)))
		 (if (arrayp (symbol-function tem))
		     (command-execute tem)
		   (funcall tem)))
		;; Set the default directory as specified in -L.
		((or (string-equal argi "-L")
		     (string-equal argi "-directory"))
		 (if argval
		     (setq tem argval)
		   (setq tem (car command-line-args-left)
			 command-line-args-left (cdr command-line-args-left)))
		 (setq extra-load-path
		       (cons (expand-file-name tem) extra-load-path))
		 (setq load-path (append (nreverse extra-load-path)
					 initial-load-path)))
		((or (string-equal argi "-l")
		     (string-equal argi "-load"))
		 (if argval
		     (setq tem argval)
		   (setq tem (car command-line-args-left)
			 command-line-args-left (cdr command-line-args-left)))
		 (let ((file tem))
		   ;; Take file from default dir if it exists there;
		   ;; otherwise let `load' search for it.
		   (if (file-exists-p (expand-file-name file))
		       (setq file (expand-file-name file)))
		   (load file nil t)))
		((string-equal argi "-insert")
		 (or (stringp (car command-line-args-left))
		     (error "File name omitted from `-insert' option"))
		 (if argval
		     (setq tem argval)
		   (setq tem (car command-line-args-left)
			 command-line-args-left (cdr command-line-args-left)))
		 (insert-file-contents tem))
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
