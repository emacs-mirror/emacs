;;; startup.el --- process Emacs shell arguments

;; Copyright (C) 1985, 86, 92, 94, 95, 96, 97, 98, 99, 2000, 2001, 2002
;;   Free Software Foundation, Inc.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file parses the command line and gets Emacs running.  Options on
;; the command line are handled in precedence order.  The order is the
;; one in the list below; first described means first handled.  Options
;; within each category (delimited by a bar) are handled in the order
;; encountered on the command line.

;; -------------------------
;; -version                  Print Emacs version to stderr, then exit
;; --version                 successfully right away.
;;                           This option is handled by emacs.c
;; -------------------------
;; -help                     Print a short usage description and exit
;; --help                    successfully right away.
;;                           This option is handled by emacs.c
;; -------------------------
;; -nl                       Do not use shared memory (for systems that
;; -no-shared-memory         support this) for the dumped Emacs data.
;;                           This option is handled by emacs.c
;;
;; -map                      For VMS.
;; --map-data                This option is handled by emacs.c
;; -------------------------
;; -t FILE                   Use FILE as the name of the terminal.
;; --terminal FILE           Using this implies "-nw" also.
;;                           This option is handled by emacs.c
;; -------------------------
;; -d DISPNAME               Use DISPNAME as the name of the X
;; -display DISPNAME         display for the initial frame.
;; --display DISPNAME        This option is handled by emacs.c
;; -------------------------
;; -nw                       Do not use a windows system (but use the
;; --no-window-system        terminal instead.)
;;                           This option is handled by emacs.c
;; -------------------------
;; -batch                    Execute noninteractively (messages go to stdout,
;; --batch                   variable noninteractive set to t)
;;                           This option is handled by emacs.c
;; -------------------------
;; -q                        Do not load user's init file and do not load
;; -no-init-file             "default.el".  Regardless of this switch,
;; --no-init-file            "site-start" is still loaded.
;; -------------------------
;; -no-site-file             Do not load "site-start.el".  (This is the ONLY
;; --no-site-file            way to prevent loading that file.)
;; -------------------------
;; -u USER                   Load USER's init file instead of the init
;; -user USER                file belonging to the user starting Emacs.
;; --user USER
;; -------------------------
;; -debug-init               Don't catch errors in init files; let the
;; --debug-init              debugger run.
;; -------------------------
;; -i ICONTYPE               Set type of icon using when Emacs is
;; -itype ICONTYPE           iconified under X.
;; --icon-type ICONTYPE      This option is passed on to term/x-win.el
;;
;; -iconic                   Start Emacs iconified.
;; --iconic                  This option is passed on to term/x-win.el
;; -------------------------
;; Various X options for colors/fonts/geometry/title etc.
;; These options are passed on to term/x-win.el which see.
;; -------------------------
;; FILE                      Visit FILE.
;; -visit FILE
;; --visit FILE
;; -file FILE
;; --file FILE
;;
;; -L DIRNAME                Add DIRNAME to load-path
;; -directory DIRNAME
;; --directory DIRNAME
;;
;; -l FILE                   Load and execute the Emacs lisp code
;; -load FILE                in FILE.
;; --load FILE
;;
;; -f FUNC                   Execute Emacs lisp function FUNC with
;; -funcall FUNC             no arguments.  The "-e" form is outdated
;; --funcall FUNC            and should not be used.  (It's a typo
;; -e FUNC                   promoted to a feature.)
;;
;; -eval FORM                Execute Emacs lisp form FORM.
;; --eval FORM
;; -execute EXPR
;; --execute EXPR
;;
;; -insert FILE              Insert the contents of FILE into buffer.
;; --insert FILE
;; -------------------------
;; -kill                     Kill (exit) Emacs right away.
;; --kill
;; -------------------------

;;; Code:

(setq top-level '(normal-top-level))

(defvar command-line-processed nil
  "Non-nil once command line has been processed.")

(defgroup initialization nil
  "Emacs start-up procedure"
  :group 'internal)

(defcustom inhibit-startup-message nil
  "*Non-nil inhibits the initial startup message.
This is for use in your personal init file, once you are familiar
with the contents of the startup message."
  :type 'boolean
  :group 'initialization)

(defcustom inhibit-startup-echo-area-message nil
  "*Non-nil inhibits the initial startup echo area message.
Setting this variable takes effect
only if you do it with the customization buffer
or if your `.emacs' file contains a line of this form:
 (setq inhibit-startup-echo-area-message \"YOUR-USER-NAME\")
If your `.emacs' file is byte-compiled, use the following form instead:
 (eval '(setq inhibit-startup-echo-area-message \"YOUR-USER-NAME\"))
Thus, someone else using a copy of your `.emacs' file will see
the startup message unless he personally acts to inhibit it."
  :type '(choice (const :tag "Don't inhibit")
		 (string :tag "Enter your user name, to inhibit"))
  :group 'initialization)

(defcustom inhibit-default-init nil
  "*Non-nil inhibits loading the `default' library."
  :type 'boolean
  :group 'initialization)

(defcustom inhibit-startup-buffer-menu nil
  "*Non-nil inhibits display of buffer list when more than 2 files are loaded."
  :type 'boolean
  :group 'initialization)

(defvar command-switch-alist nil
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

;;; This is here, rather than in x-win.el, so that we can ignore these
;;; options when we are not using X.
(defconst command-line-x-option-alist
  '(("-bw" 1 x-handle-numeric-switch border-width)
    ("-d" 1 x-handle-display)
    ("-display" 1 x-handle-display)
    ("-name" 1 x-handle-name-switch)
    ("-title" 1 x-handle-switch title)
    ("-T" 1 x-handle-switch title)
    ("-r" 0 x-handle-switch reverse t)
    ("-rv" 0 x-handle-switch reverse t)
    ("-reverse" 0 x-handle-switch reverse t)
    ("-reverse-video" 0 x-handle-switch reverse t)
    ("-fn" 1 x-handle-switch font)
    ("-font" 1 x-handle-switch font)
    ("-fs" 0 x-handle-initial-switch fullscreen fullboth)
    ("-fw" 0 x-handle-initial-switch fullscreen fullwidth)
    ("-fh" 0 x-handle-initial-switch fullscreen fullheight)
    ("-ib" 1 x-handle-numeric-switch internal-border-width)
    ("-g" 1 x-handle-geometry)
    ("-lsp" 1 x-handle-numeric-switch line-spacing)
    ("-geometry" 1 x-handle-geometry)
    ("-fg" 1 x-handle-switch foreground-color)
    ("-foreground" 1 x-handle-switch foreground-color)
    ("-bg" 1 x-handle-switch background-color)
    ("-background" 1 x-handle-switch background-color)
    ("-ms" 1 x-handle-switch mouse-color)
    ("-itype" 0 x-handle-switch icon-type t)
    ("-i" 0 x-handle-switch icon-type t)
    ("-iconic" 0 x-handle-iconic)
    ("-xrm" 1 x-handle-xrm-switch)
    ("-cr" 1 x-handle-switch cursor-color)
    ("-vb" 0 x-handle-switch vertical-scroll-bars t)
    ("-hb" 0 x-handle-switch horizontal-scroll-bars t)
    ("-bd" 1 x-handle-switch)
    ("--border-width" 1 x-handle-numeric-switch border-width)
    ("--display" 1 x-handle-display)
    ("--name" 1 x-handle-name-switch)
    ("--title" 1 x-handle-switch title)
    ("--reverse-video" 0 x-handle-switch reverse t)
    ("--font" 1 x-handle-switch font)
    ("--fullscreen" 0 x-handle-initial-switch fullscreen fullboth)
    ("--fullwidth" 0 x-handle-initial-switch fullscreen fullwidth)
    ("--fullheight" 0 x-handle-initial-switch fullscreen fullheight)
    ("--internal-border" 1 x-handle-numeric-switch internal-border-width)
    ("--geometry" 1 x-handle-geometry)
    ("--foreground-color" 1 x-handle-switch foreground-color)
    ("--background-color" 1 x-handle-switch background-color)
    ("--mouse-color" 1 x-handle-switch mouse-color)
    ("--icon-type" 0 x-handle-switch icon-type t)
    ("--iconic" 0 x-handle-iconic)
    ("--xrm" 1 x-handle-xrm-switch)
    ("--cursor-color" 1 x-handle-switch cursor-color)
    ("--vertical-scroll-bars" 0 x-handle-switch vertical-scroll-bars t)
    ("--line-spacing" 1 x-handle-numeric-switch line-spacing)
    ("--border-color" 1 x-handle-switch border-width)
    ("--smid" 1 x-handle-smid))
  "Alist of X Windows options.
Each element has the form
  (NAME NUMARGS HANDLER FRAME-PARAM VALUE)
where NAME is the option name string, NUMARGS is the number of arguments
that the option accepts, HANDLER is a function to call to handle the option.
FRAME-PARAM (optional) is the frame parameter this option specifies,
and VALUE is the value which is given to that frame parameter
\(most options use the argument for this, so VALUE is not present).")

(defvar before-init-hook nil
  "Normal hook run after handling urgent options but before loading init files.")

(defvar after-init-hook nil
  "Normal hook run after loading the init files, `~/.emacs' and `default.el'.
There is no `condition-case' around the running of these functions;
therefore, if you set `debug-on-error' non-nil in `.emacs',
an error in one of these functions will invoke the debugger.")

(defvar emacs-startup-hook nil
  "Normal hook run after loading init files and handling the command line.")

(defvar term-setup-hook nil
  "Normal hook run after loading terminal-specific Lisp code.
It also follows `emacs-startup-hook'.  This hook exists for users to set,
so as to override the definitions made by the terminal-specific file.
Emacs never sets this variable itself.")

(defvar keyboard-type nil
  "The brand of keyboard you are using.
This variable is used to define
the proper function and keypad keys for use under X.  It is used in a
fashion analogous to the environment variable TERM.")

(defvar window-setup-hook nil
  "Normal hook run to initialize window system display.
Emacs runs this hook after processing the command line arguments and loading
the user's init file.")

(defcustom initial-major-mode 'lisp-interaction-mode
  "Major mode command symbol to use for the initial *scratch* buffer."
  :type 'function
  :group 'initialization)

(defcustom init-file-user nil
  "Identity of user whose `.emacs' file is or was read.
The value is nil if `-q' or `--no-init-file' was specified,
meaning do not load any init file.

Otherwise, the value may be the null string, meaning use the init file
for the user that originally logged in, or it may be a
string containing a user's name meaning use that person's init file.

In either of the latter cases, `(concat \"~\" init-file-user \"/\")'
evaluates to the name of the directory where the `.emacs' file was
looked for.

Setting `init-file-user' does not prevent Emacs from loading
`site-start.el'.  The only way to do that is to use `--no-site-file'."
  :type '(choice (const :tag "none" nil) string)
  :group 'initialization)

(defcustom site-run-file "site-start"
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
is less convenient."
  :type '(choice (const :tag "none" nil) string)
  :group 'initialization)

(defcustom mail-host-address nil
  "*Name of this machine, for purposes of naming users."
  :type '(choice (const nil) string)
  :group 'mail)

(defcustom user-mail-address nil
  "*Full mailing address of this user.
This is initialized based on `mail-host-address',
after your init file is read, in case it sets `mail-host-address'."
  :type 'string
  :group 'mail)

(defcustom auto-save-list-file-prefix
  (cond ((eq system-type 'ms-dos)
	 ;; MS-DOS cannot have initial dot, and allows only 8.3 names
	 "~/_emacs.d/auto-save.list/_s")
	(t
	 "~/.emacs.d/auto-save-list/.saves-"))
  "Prefix for generating `auto-save-list-file-name'.
This is used after reading your `.emacs' file to initialize
`auto-save-list-file-name', by appending Emacs's pid and the system name,
if you have not already set `auto-save-list-file-name' yourself.
Directories in the prefix will be created if necessary.
Set this to nil if you want to prevent `auto-save-list-file-name'
from being initialized."
  :type '(choice (const :tag "Don't record a session's auto save list" nil)
		 string)
  :group 'auto-save)

(defvar init-file-debug nil)

(defvar init-file-had-error nil)

(defvar normal-top-level-add-subdirs-inode-list nil)

(defun normal-top-level-add-subdirs-to-load-path ()
  "Add all subdirectories of current directory to `load-path'.
More precisely, this uses only the subdirectories whose names
start with letters or digits; it excludes any subdirectory named `RCS'
or `CVS', and any subdirectory that contains a file named `.nosearch'."
  (let (dirs 
	attrs
	(pending (list default-directory)))
    ;; This loop does a breadth-first tree walk on DIR's subtree,
    ;; putting each subdir into DIRS as its contents are examined.
    (while pending
      (push (pop pending) dirs)
      (let* ((this-dir (car dirs))
	     (contents (directory-files this-dir))
	     (default-directory this-dir)
	     (canonicalized (and (eq system-type 'windows-nt)
				 (untranslated-canonical-name this-dir))))
	;; The Windows version doesn't report meaningful inode
	;; numbers, so use the canonicalized absolute file name of the
	;; directory instead.
	(setq attrs (or canonicalized
			(nthcdr 10 (file-attributes this-dir))))
	(unless (member attrs normal-top-level-add-subdirs-inode-list)
	  (push attrs normal-top-level-add-subdirs-inode-list)
	  (dolist (file contents)
	    ;; The lower-case variants of RCS and CVS are for DOS/Windows.
	    (unless (member file '("." ".." "RCS" "CVS" "rcs" "cvs"))
	      (when (and (string-match "\\`[[:alnum:]]" file)
			 ;; Avoid doing a `stat' when it isn't necessary
			 ;; because that can cause trouble when an NFS server
			 ;; is down.
			 (not (string-match "\\.elc?\\'" file))
			 (file-directory-p file))
		(let ((expanded (expand-file-name file)))
		  (unless (file-exists-p (expand-file-name ".nosearch"
							   expanded))
		    (setq pending (nconc pending (list expanded)))))))))))
    (normal-top-level-add-to-load-path (cdr (nreverse dirs)))))

;; This function is called from a subdirs.el file.
;; It assumes that default-directory is the directory
;; in which the subdirs.el file exists,
;; and it adds to load-path the subdirs of that directory
;; as specified in DIRS.  Normally the elements of DIRS are relative.
(defun normal-top-level-add-to-load-path (dirs)
  (let ((tail load-path)
	(thisdir (directory-file-name default-directory)))
    (while (and tail
		;;Don't go all the way to the nil terminator.
		(cdr tail)
		(not (equal thisdir (car tail)))
		(not (and (memq system-type '(ms-dos windows-nt))
			  (equal (downcase thisdir) (downcase (car tail))))))
      (setq tail (cdr tail)))
    ;;Splice the new section in.
    (when tail
      (setcdr tail (append (mapcar 'expand-file-name dirs) (cdr tail))))))

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
    ;; For root, preserve owner and group when editing files.
    (if (equal (user-uid) 0)
	(setq backup-by-copying-when-mismatch t))
    ;; Look in each dir in load-path for a subdirs.el file.
    ;; If we find one, load it, which will add the appropriate subdirs
    ;; of that dir into load-path,
    ;; Look for a leim-list.el file too.  Loading it will register
    ;; available input methods.
    (let ((tail load-path)
	  new)
      (while tail
	(push (car tail) new)
	(condition-case nil
	    (let ((default-directory (car tail)))
	      (load (expand-file-name "subdirs.el" (car tail)) t t t)))
	(condition-case nil
	    (let ((default-directory (car tail)))
	      (load (expand-file-name "leim-list.el" (car tail)) t t t)))
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
    (let ((menubar-bindings-done nil))
      (unwind-protect
	  (command-line)
	;; Do this again, in case .emacs defined more abbreviations.
	(setq default-directory (abbreviate-file-name default-directory))
	;; Specify the file for recording all the auto save files of this session.
	;; This is used by recover-session.
	(or auto-save-list-file-name
	    (and auto-save-list-file-prefix
		 (setq auto-save-list-file-name
		       ;; Under MS-DOS our PID is almost always reused between
		       ;; Emacs invocations.  We need something more unique.
		       (cond ((eq system-type 'ms-dos)
			      ;; We are going to access the auto-save
			      ;; directory, so make sure it exists.
			      (make-directory
			       (file-name-directory auto-save-list-file-prefix)
			       t)
			      (concat 
			       (make-temp-name
				(expand-file-name
				 auto-save-list-file-prefix))
			       "~"))
			     (t
			      (expand-file-name
			       (format "%s%d-%s~"
				       auto-save-list-file-prefix
				       (emacs-pid)
				       (system-name))))))))
	(run-hooks 'emacs-startup-hook)
	(and term-setup-hook
	     (run-hooks 'term-setup-hook))

	;; Don't do this if we failed to create the initial frame,
	;; for instance due to a dense colormap.
	(when (or frame-initial-frame
		  ;; If frame-initial-frame has no meaning, do this anyway.
		  (not (and window-system
			    (not noninteractive)
			    (not (eq window-system 'pc)))))
	  ;; Modify the initial frame based on what .emacs puts into
	  ;; ...-frame-alist.
	  (if (fboundp 'frame-notice-user-settings)
	      (frame-notice-user-settings))
	  (if (fboundp 'frame-set-background-mode)
	      ;; Set the faces for the initial background mode even if
	      ;; frame-notice-user-settings didn't (such as on a tty).
	      ;; frame-set-background-mode is idempotent, so it won't
	      ;; cause any harm if it's already been done.
	      (let ((frame-background-mode frame-background-mode)
		    (frame (selected-frame))
		    term)
		(when (and (null window-system)
			   ;; Don't override a possibly customized value.
			   (null frame-background-mode)
			   ;; Don't override user specifications.
			   (null (frame-parameter frame 'reverse))
			   (let ((bg (frame-parameter frame 'background-color)))
			     (or (null bg)
				 (member bg '(unspecified "unspecified-bg")))))
		  (setq term (getenv "TERM"))
		  ;; Some files in lisp/term do a better job with the
		  ;; background mode, but we leave this here anyway, in
		  ;; case they remove those files.
		  (if (string-match "^\\(xterm\\|rxvt\\|dtterm\\|eterm\\)"
				    term)
		      (setq frame-background-mode 'light)))
		(frame-set-background-mode (selected-frame)))))

	;; Now we know the user's default font, so add it to the menu.
	(if (fboundp 'font-menu-add-default)
	    (font-menu-add-default))
	(and window-setup-hook
	     (run-hooks 'window-setup-hook))
	(or menubar-bindings-done
	    (if (display-popup-menus-p)
		(precompute-menubar-bindings)))))))

;; Precompute the keyboard equivalents in the menu bar items.
(defun precompute-menubar-bindings ()
  (let ((submap (lookup-key global-map [menu-bar])))
    (while submap
      (and (consp (car submap))
	   (symbolp (car (car submap)))
	   (stringp (car-safe (cdr (car submap))))
	   (keymapp (cdr (cdr (car submap))))
	   (progn
	     (x-popup-menu nil (cdr (cdr (car submap))))
	     (if purify-flag
		 (garbage-collect))))
      (setq submap (cdr submap))))
    (setq define-key-rebound-commands t))

;; Command-line options supported by tty's:
(defconst tty-long-option-alist
  '(("--name"		  . "-name")
    ("--title"		  . "-T")
    ("--reverse-video"	  . "-reverse")
    ("--foreground-color" . "-fg")
    ("--background-color" . "-bg")
    ("--color"		  . "-color")))

(defconst tool-bar-images-pixel-height 24
  "Height in pixels of images in the tool bar.")

(defvar tool-bar-originally-present nil
  "Non-nil if tool-bars are present before user and site init files are read.")

;; Handle the X-like command line parameters "-fg", "-bg", "-name", etc.
(defun tty-handle-args (args)
  (let ((rest nil))
    (message "%s" args)
    (while (and args
		(not (equal (car args) "--")))
      (let* ((this (car args))
	     (orig-this this)
	     completion argval)
	(setq args (cdr args))
	;; Check for long options with attached arguments
	;; and separate out the attached option argument into argval.
	(if (string-match "^--[^=]*=" this)
	    (setq argval (substring this (match-end 0))
		  this (substring this 0 (1- (match-end 0)))))
	(when (string-match "^--" this)
	  (setq completion (try-completion this tty-long-option-alist))
	  (if (eq completion t)
	      ;; Exact match for long option.
	      (setq this (cdr (assoc this tty-long-option-alist)))
	    (if (stringp completion)
		(let ((elt (assoc completion tty-long-option-alist)))
		  ;; Check for abbreviated long option.
		  (or elt
		      (error "Option `%s' is ambiguous" this))
		  (setq this (cdr elt)))
	      ;; Check for a short option.
	      (setq argval nil this orig-this))))
	(cond ((or (string= this "-fg") (string= this "-foreground"))
	       (or argval (setq argval (car args) args (cdr args)))
	       (setq default-frame-alist
		     (cons (cons 'foreground-color argval)
			   default-frame-alist)))
	      ((or (string= this "-bg") (string= this "-background"))
	       (or argval (setq argval (car args) args (cdr args)))
	       (setq default-frame-alist
		     (cons (cons 'background-color argval)
			   default-frame-alist)))
	      ((or (string= this "-T") (string= this "-name"))
	       (or argval (setq argval (car args) args (cdr args)))
	       (setq default-frame-alist
		     (cons
		      (cons 'title
			    (if (stringp argval)
				argval
			      (let ((case-fold-search t)
				    i)
				(setq argval (invocation-name))

				;; Change any . or * characters in name to
				;; hyphens, so as to emulate behavior on X.
				(while
				    (setq i (string-match "[.*]" argval))
				  (aset argval i ?-))
				argval)))
		      default-frame-alist)))
	      ((or (string= this "-r")
		   (string= this "-rv")
		   (string= this "-reverse"))
	       (setq default-frame-alist
		     (cons '(reverse . t)
			   default-frame-alist)))
	      ((string= this "-color")
	       (if (null argval)
		   (setq argval 8))	; default --color means 8 ANSI colors
	       (setq default-frame-alist
		     (cons (cons 'tty-color-mode
				 (cond
				  ((numberp argval) argval)
				  ((string-match "-?[0-9]+" argval)
				   (string-to-number argval))
				  (t (intern argval))))
			   default-frame-alist)))
	      (t (setq rest (cons this rest))))))
      (nreverse rest)))

(defun command-line ()
  (setq command-line-default-directory default-directory)

  ;; Choose a reasonable location for temporary files.
  (setq temporary-file-directory
	(file-name-as-directory
	 (cond ((memq system-type '(ms-dos windows-nt))
		(or (getenv "TEMP") (getenv "TMPDIR") (getenv "TMP") "c:/temp"))
	       ((memq system-type '(vax-vms axp-vms))
		(or (getenv "TMPDIR") (getenv "TMP") (getenv "TEMP") "SYS$SCRATCH:"))
	       (t
		(or (getenv "TMPDIR") (getenv "TMP") (getenv "TEMP") "/tmp")))))
  (setq small-temporary-file-directory
	(if (eq system-type 'ms-dos)
	    (getenv "TMPDIR")))
  (setq auto-save-file-name-transforms
	(list (list "\\`/[^/]*:\\(.+/\\)*\\(.*\\)"
		    ;; Don't put "\\2" inside expand-file-name, since
		    ;; it will be transformed to "/2" on DOS/Windows.
		    (concat temporary-file-directory "\\2") t)))

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

  ;; Set the default strings to display in mode line for
  ;; end-of-line formats that aren't native to this platform.
  (cond
   ((memq system-type '(ms-dos windows-nt emx))
    (setq eol-mnemonic-unix "(Unix)")
    (setq eol-mnemonic-mac  "(Mac)"))
   ;; Both Mac and Unix EOLs are now "native" on Mac OS so keep the
   ;; abbreviated strings `/' and `:' set in coding.c for them.
   ((eq system-type 'macos)
    (setq eol-mnemonic-dos  "(DOS)"))
   (t	; this is for Unix/GNU/Linux systems
    (setq eol-mnemonic-dos  "(DOS)")
    (setq eol-mnemonic-mac  "(Mac)")))

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
		     (mapconcat (lambda (obj) (prin1-to-string obj t))
				(cdr (cdr error)) ", "))
	  (format "%s: %s"
		   (get (car error) 'error-message)
		   (mapconcat (lambda (obj) (prin1-to-string obj t))
			      (cdr error) ", "))))
      'external-debugging-output)
     (terpri 'external-debugging-output)
     (setq window-system nil)
     (kill-emacs)))

  ;; Windowed displays do this inside their *-win.el.
  (when (and (not (display-graphic-p))
	     (not noninteractive))
    (setq command-line-args (tty-handle-args command-line-args)))

  (set-locale-environment nil)

  ;; Convert the arguments to Emacs internal representation.
  (let ((args (cdr command-line-args)))
    (while args
      (setcar args
	      (decode-coding-string (car args) locale-coding-system t))
      (setq args (cdr args))))

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
	    (argi (pop args))
	    (argval nil))
	;; Handle --OPTION=VALUE format.
	(if (and (string-match "\\`--" argi)
		 (string-match "=" argi))
	    (setq argval (substring argi (match-end 0))
		  argi (substring argi 0 (match-beginning 0))))
	(or (equal argi "--")
	    (let ((completion (try-completion argi longopts)))
	      (if (eq completion t)
		  (setq argi (substring argi 1))
		(if (stringp completion)
		    (let ((elt (assoc completion longopts)))
		      (or elt
			  (error "Option `%s' is ambiguous" argi))
		      (setq argi (substring (car elt) 1)))
		  (setq argval nil)))))
	(cond
	 ((member argi '("-q" "-no-init-file"))
	  (setq init-file-user nil))
	 ((member argi '("-u" "-user"))
	  (or argval
	      (setq argval (pop args)))
	  (setq init-file-user argval
		argval nil))
	 ((string-equal argi "-no-site-file")
	  (setq site-run-file nil))
	 ((string-equal argi "-debug-init")
	  (setq init-file-debug t))
	 ((string-equal argi "-iconic")
	  (push '(visibility . icon) initial-frame-alist))
	 ((or (string-equal argi "-icon-type")
	      (string-equal argi "-i")
	      (string-equal argi "-itype"))
	  (push '(icon-type . t) default-frame-alist))
	 ;; Push the popped arg back on the list of arguments.
	 (t (push argi args) (setq done t)))
	;; Was argval set but not used?
	(and argval
	     (error "Option `%s' doesn't allow an argument" argi))))

    ;; Re-attach the program name to the front of the arg list.
    (and command-line-args (setcdr command-line-args args)))

  ;; Under X Windows, this creates the X frame and deletes the terminal frame.
  (when (fboundp 'frame-initialize)
    (frame-initialize))

  ;; If frame was created with a menu bar, set menu-bar-mode on.
  (if (and (not noninteractive)
	   (or (not (memq window-system '(x w32)))
	       (> (frame-parameter nil 'menu-bar-lines) 0)))
      (menu-bar-mode t))

  ;; If frame was created with a tool bar, switch tool-bar-mode on.
  (when (and (not noninteractive)
	     (display-graphic-p)
	     (> (frame-parameter nil 'tool-bar-lines) 0))
    (tool-bar-mode 1))

  ;; Can't do this init in defcustom because window-system isn't set.
  (when (and (not noninteractive)
	     (not (eq system-type 'ms-dos))
	     (memq window-system '(x w32)))
    (setq-default blink-cursor t)
    (blink-cursor-mode 1))

  (unless noninteractive
    ;; DOS/Windows systems have a PC-type keyboard which has both
    ;; <delete> and <backspace> keys.
    (when (or (memq system-type '(ms-dos windows-nt))
	      (and (memq window-system '(x))
		   (fboundp 'x-backspace-delete-keys-p)
		   (x-backspace-delete-keys-p))
	      ;; If the terminal Emacs is running on has erase char
	      ;; set to ^H, use the Backspace key for deleting
	      ;; backward and, and the Delete key for deleting forward.
	      (and (null window-system)
		   (eq tty-erase-char 8)))
      (setq-default normal-erase-is-backspace t)
      (normal-erase-is-backspace-mode 1)))

  (when (and (not noninteractive)
	     (display-graphic-p)
	     (fboundp 'x-show-tip))
    (setq-default tooltip-mode t)
    (tooltip-mode 1))

  ;; Register default TTY colors for the case the terminal hasn't a
  ;; terminal init file.
  (or (memq window-system '(x w32))
      ;; We do this regardles of whether the terminal supports colors
      ;; or not, since they can switch that support on or off in
      ;; mid-session by setting the tty-color-mode frame parameter.
      (tty-register-default-colors))

  ;; Record whether the tool-bar is present before the user and site
  ;; init files are processed.  frame-notice-user-settings uses this
  ;; to determine if the tool-bar has been disabled by the init files,
  ;; and the frame needs to be resized.
  (when (fboundp 'frame-notice-user-settings)
    (let ((tool-bar-lines (or (assq 'tool-bar-lines initial-frame-alist)
                              (assq 'tool-bar-lines default-frame-alist))))
      (setq tool-bar-originally-present
            (not (or (null tool-bar-lines)
                     (null (cdr tool-bar-lines))
                     (eq 0 (cdr tool-bar-lines)))))))

  (let ((old-scalable-fonts-allowed scalable-fonts-allowed)
	(old-font-list-limit font-list-limit)
	(old-face-ignored-fonts face-ignored-fonts))

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
	   (if (eq init-file-debug t) 'startup init-file-debug))
	  (orig-enable-multibyte default-enable-multibyte-characters))
      (let ((debug-on-error debug-on-error-initial)
	    ;; This function actually reads the init files.
	    (inner
	     (function
	      (lambda ()
		(if init-file-user
		    (let ((user-init-file-1
			   (cond
			    ((eq system-type 'ms-dos)
			     (concat "~" init-file-user "/_emacs"))
			    ((eq system-type 'windows-nt)
			     (if (directory-files "~" nil "^\\.emacs\\(\\.elc?\\)?$")
				 "~/.emacs"
			       "~/_emacs"))
			    ((eq system-type 'vax-vms) 
			     "sys$login:.emacs")
			    (t
			     (concat "~" init-file-user "/.emacs")))))
		      ;; This tells `load' to store the file name found
		      ;; into user-init-file.
		      (setq user-init-file t)
		      (load user-init-file-1 t t)
		      
		      ;; If we did not find the user's init file,
		      ;; set user-init-file conclusively to nil;
		      ;; don't let it be set from default.el.
		      (if (eq user-init-file t)
			  (setq user-init-file user-init-file-1))
		      
		      ;; If we loaded a compiled file, set
		      ;; `user-init-file' to the source version if that
		      ;; exists.
		      (when (and user-init-file
				 (equal (file-name-extension user-init-file)
					"elc"))
			(let* ((source (file-name-sans-extension user-init-file))
			       (alt (concat source ".el")))
			  (setq source (cond ((file-exists-p alt) alt)
					     ((file-exists-p source) source)
					     (t nil)))
			  (when source
			    (when (file-newer-than-file-p source user-init-file)
			      (message "Warning: %s is newer than %s"
				       source user-init-file)
			      (sit-for 1))
			    (setq user-init-file source))))
		      
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
	    (error
	     (let ((message-log-max nil))
	       (save-excursion
		 (set-buffer (get-buffer-create "*Messages*"))
		 (insert "\n\n"
			 (format "An error has occurred while loading `%s':\n\n"
				 user-init-file)
			 (format "%s%s%s"
				 (get (car error) 'error-message)
				 (if (cdr error) ": " "")
				 (mapconcat 'prin1-to-string (cdr error) ", "))
			 "\n\n"
			 "To ensure normal operation, you should investigate the cause\n"
			 "of the error in your initialization file and remove it.  Start\n"
			 "Emacs with the `--debug-init' option to view a complete error\n"
			 "backtrace\n"))
	       (message "Error in init file: %s%s%s"
			(get (car error) 'error-message)
			(if (cdr error) ": " "")
			(mapconcat 'prin1-to-string (cdr error) ", "))
	       (pop-to-buffer "*Messages*")
	       (setq init-file-had-error t)))))

	;; If the user has a file of abbrevs, read it.
	(if (file-exists-p abbrev-file-name)
	    (quietly-read-abbrev-file abbrev-file-name))

	;; If the abbrevs came entirely from the init file or the
	;; abbrevs file, they do not need saving.
	(setq abbrevs-changed nil)

	;; If we can tell that the init file altered debug-on-error,
	;; arrange to preserve the value that it set up.
	(or (eq debug-on-error debug-on-error-initial)
	    (setq debug-on-error-should-be-set t
		  debug-on-error-from-init-file debug-on-error)))
      (if debug-on-error-should-be-set
	  (setq debug-on-error debug-on-error-from-init-file))
      (unless (or default-enable-multibyte-characters
		  (eq orig-enable-multibyte default-enable-multibyte-characters))
	;; Init file changed to unibyte.  Reset existing multibyte
	;; buffers (probably *scratch*, *Messages*, *Minibuff-0*).
	;; Arguably this should only be done if they're free of
	;; multibyte characters.
	(mapcar (lambda (buffer)
		  (with-current-buffer buffer
		    (if enable-multibyte-characters
			(set-buffer-multibyte nil))))
		(buffer-list))
	;; Also re-set the language environment in case it was
	;; originally done before unibyte was set and is sensitive to
	;; unibyte (display table, terminal coding system &c).
	(set-language-environment current-language-environment)))
    
    ;; Do this here in case the init file sets mail-host-address.
    (or user-mail-address
	(setq user-mail-address (concat (user-login-name) "@"
					(or mail-host-address
					    (system-name)))))

    ;; If parameter have been changed in the init file which influence
    ;; face realization, clear the face cache so that new faces will
    ;; be realized.
    (unless (and (eq scalable-fonts-allowed old-scalable-fonts-allowed)
		 (eq font-list-limit old-font-list-limit)
		 (eq face-ignored-fonts old-face-ignored-fonts))
      (clear-face-cache)))
    
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
  (if noninteractive (kill-emacs t))

  ;; Run emacs-session-restore (session management) if started by
  ;; the session manager and we have a session manager connection.
  (if (and (boundp 'x-session-previous-id) (stringp x-session-previous-id))
      (emacs-session-restore x-session-previous-id)))

(defcustom initial-scratch-message (purecopy "\
;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

")
  "Initial message displayed in *scratch* buffer at startup.
If this is nil, no message will be displayed."
  :type '(choice (text :tag "Message")
		 (const :tag "none" nil))
  :group 'initialization)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fancy splash screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar fancy-splash-text
  '((:face variable-pitch
	   "You can do basic editing with the menu bar and scroll bar \
using the mouse.\n\n"
	   :face (variable-pitch :weight bold)
	   "Important Help menu items:\n"
	   :face variable-pitch "\
Emacs Tutorial\tLearn-by-doing tutorial for using Emacs efficiently
Emacs FAQ\tFrequently asked questions and answers
\(Non)Warranty\tGNU Emacs comes with "
	   :face (variable-pitch :slant oblique)
	   "ABSOLUTELY NO WARRANTY\n"
	   :face variable-pitch
	   "\
Copying Conditions\tConditions for redistributing and changing Emacs
Ordering Manuals\tHow to order Emacs manuals from the Free Software Foundation\n")
  (:face variable-pitch
	   "You can do basic editing with the menu bar and scroll bar \
using the mouse.\n\n"
	   :face (variable-pitch :weight bold)
	   "Useful File menu items:\n"
	   :face variable-pitch "\
Exit Emacs\t(Or type Control-x followed by Control-c)
Recover Session\tRecover files you were editing before a crash



"
	   ))
  "A list of texts to show in the middle part of splash screens.
Each element in the list should be a list of strings or pairs
`:face FACE', like `fancy-splash-insert' accepts them.")


(defgroup fancy-splash-screen ()
  "Fancy splash screen when Emacs starts."
  :version "21.1"
  :group 'initialization)


(defcustom fancy-splash-delay 10
  "*Delay in seconds between splash screens."
  :group 'fancy-splash-screen
  :type 'integer)


(defcustom fancy-splash-max-time 60
  "*Show splash screens for at most this number of seconds.
Values less than 60 seconds are ignored."
  :group 'fancy-splash-screen
  :type 'integer)


(defcustom fancy-splash-image nil
  "*The image to show in the splash screens, or nil for defaults."
  :group 'fancy-splash-screen
  :type '(choice (const :tag "Default" nil)
		 (file :tag "File")))


;; These are temporary storage areas for the splash screen display.

(defvar fancy-current-text nil)
(defvar fancy-splash-help-echo nil)
(defvar fancy-splash-stop-time nil)
(defvar fancy-splash-outer-buffer nil)

(defun fancy-splash-insert (&rest args)
  "Insert text into the current buffer, with faces.
Arguments from ARGS should be either strings or pairs `:face FACE',
where FACE is a valid face specification, as it can be used with
`put-text-properties'."
  (let ((current-face nil))
    (while args
      (if (eq (car args) :face)
	  (setq args (cdr args) current-face (car args))
	(insert (propertize (car args)
			    'face current-face
			    'help-echo fancy-splash-help-echo)))
      (setq args (cdr args)))))


(defun fancy-splash-head ()
  "Insert the head part of the splash screen into the current buffer."
  (let* ((image-file (cond ((stringp fancy-splash-image)
			    fancy-splash-image)
			   ((and (display-color-p)
				 (image-type-available-p 'xpm))
			    (if (and (fboundp 'x-display-planes)
				     (= (funcall 'x-display-planes) 8))
				"splash8.xpm"
			      "splash.xpm"))
			   (t "splash.pbm")))
	 (img (create-image image-file))
	 (image-width (and img (car (image-size img))))
	 (window-width (window-width (selected-window))))
    (when img
      (when (> window-width image-width)
	;; Center the image in the window.
	(let ((pos (/ (- window-width image-width) 2)))
	  (insert (propertize " " 'display `(space :align-to ,pos))))

	;; Change the color of the XPM version of the splash image
	;; so that it is visible with a dark frame background.
	(when (and (memq 'xpm img)
		   (eq (frame-parameter nil 'background-mode) 'dark))
	  (setq img (append img '(:color-symbols (("#000000" . "gray30"))))))

	;; Insert the image with a help-echo and a keymap.
	(let ((map (make-sparse-keymap))
	      (help-echo "mouse-2: browse http://www.gnu.org/"))
	  (define-key map [mouse-2]
	    (lambda ()
	      (interactive)
	      (browse-url "http://www.gnu.org/")
	      (throw 'exit nil)))
	  (define-key map [down-mouse-2] 'ignore)
	  (define-key map [up-mouse-2] 'ignore)
	  (insert-image img (propertize "xxx" 'help-echo help-echo
					'keymap map)))
	(insert "\n"))))
  (if (eq system-type 'gnu/linux)
      (fancy-splash-insert
       :face '(variable-pitch :foreground "red")
       "GNU Emacs is one component of a Linux-based GNU system.")
    (fancy-splash-insert
     :face '(variable-pitch :foreground "red")
     "GNU Emacs is one component of the GNU operating system."))
  (insert "\n")
  (unless (equal (buffer-name fancy-splash-outer-buffer) "*scratch*")
    (fancy-splash-insert :face 'variable-pitch
			 (substitute-command-keys
			  "Type \\[recenter] to begin editing your file.\n"))))


(defun fancy-splash-tail ()
  "Insert the tail part of the splash screen into the current buffer."
  (let ((fg (if (eq (frame-parameter nil 'background-mode) 'dark)
		"cyan" "darkblue")))
    (fancy-splash-insert :face `(variable-pitch :foreground ,fg)
			 "\nThis is "
			 (emacs-version)
			 "\n"
			 :face '(variable-pitch :height 0.5)
			 "Copyright (C) 2002 Free Software Foundation, Inc.")
    (and auto-save-list-file-prefix
	 ;; Don't signal an error if the
	 ;; directory for auto-save-list files
	 ;; does not yet exist.
	 (file-directory-p (file-name-directory
			    auto-save-list-file-prefix))
	 (directory-files
	  (file-name-directory auto-save-list-file-prefix)
	  nil
	  (concat "\\`"
		  (regexp-quote (file-name-nondirectory
				 auto-save-list-file-prefix)))
	  t)
	 (fancy-splash-insert :face '(variable-pitch :foreground "red")
			      "\n\nIf an Emacs session crashed recently, "
			      "type M-x recover-session RET\nto recover"
			      " the files you were editing."))))

(defun fancy-splash-screens-1 (buffer)
  "Timer function displaying a splash screen."
  (when (> (float-time) fancy-splash-stop-time)
    (throw 'stop-splashing nil))
  (unless fancy-current-text
    (setq fancy-current-text fancy-splash-text))
  (let ((text (car fancy-current-text)))
    (set-buffer buffer)
    (erase-buffer)
    (fancy-splash-head)
    (apply #'fancy-splash-insert text)
    (fancy-splash-tail)
    (unless (current-message)
      (message fancy-splash-help-echo))
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (force-mode-line-update)
    (setq fancy-current-text (cdr fancy-current-text))))


(defun fancy-splash-default-action ()
  "Default action for events in the splash screen buffer."
  (interactive)
  (push last-command-event unread-command-events)
  (throw 'exit nil))


(defun fancy-splash-screens ()
  "Display fancy splash screens when Emacs starts."
  (setq fancy-splash-help-echo (startup-echo-area-message))
  (let ((old-hourglass display-hourglass)
	(fancy-splash-outer-buffer (current-buffer))
	splash-buffer
	(old-minor-mode-map-alist minor-mode-map-alist)
	timer)
    (switch-to-buffer "GNU Emacs")
    (setq tab-width 20)
    (setq splash-buffer (current-buffer))
    (catch 'stop-splashing
      (unwind-protect
	  (let ((map (make-sparse-keymap)))
	    (use-local-map map)
	    (define-key map [t] 'fancy-splash-default-action)
	    (define-key map [mouse-movement] 'ignore)
	    (define-key map [mode-line t] 'ignore)
	    (setq cursor-type nil
		  display-hourglass nil
		  minor-mode-map-alist nil
		  buffer-undo-list t
		  mode-line-format (propertize "---- %b %-" 
					       'face '(:weight bold))
		  fancy-splash-stop-time (+ (float-time)
					    (max 60 fancy-splash-max-time))
		  timer (run-with-timer 0 fancy-splash-delay
					#'fancy-splash-screens-1
					splash-buffer))
	    (recursive-edit))
	  (cancel-timer timer)
	  (setq display-hourglass old-hourglass
		minor-mode-map-alist old-minor-mode-map-alist)
	  (kill-buffer splash-buffer)))))


(defun use-fancy-splash-screens-p ()
  "Return t if fancy splash screens should be used."
  (when (or (and (display-color-p)
		 (image-type-available-p 'xpm))
	    (image-type-available-p 'pbm))
    (let* ((img (create-image (or fancy-splash-image
				  (if (and (display-color-p)
					   (image-type-available-p 'xpm))
				      "splash.xpm" "splash.pbm"))))
	   (image-height (and img (cdr (image-size img))))
	   (window-height (1- (window-height (selected-window)))))
      (> window-height (+ image-height 19)))))


(defun normal-splash-screen ()
  "Display splash screen when Emacs starts."
  (with-current-buffer (get-buffer-create "GNU Emacs")
    (let ((tab-width 8)
	  (mode-line-format (propertize "---- %b %-" 
					'face '(:weight bold))))

      ;; The convention for this piece of code is that
      ;; each piece of output starts with one or two newlines
      ;; and does not end with any newlines.
      (insert "Welcome to GNU Emacs")
      (if (eq system-type 'gnu/linux)
	  (insert ", one component of a Linux-based GNU system."))
      (insert "\n")

      (unless (equal (buffer-name (current-buffer)) "*scratch*")
	(insert (substitute-command-keys
		 "\nType \\[recenter] to begin editing your file.\n")))

      (if (display-mouse-p)
	  ;; The user can use the mouse to activate menus
	  ;; so give help in terms of menu items.
	  (progn
	    (insert "\
You can do basic editing with the menu bar and scroll bar using the mouse.

Useful File menu items:
Exit Emacs		(or type Control-x followed by Control-c)
Recover Session		recover files you were editing before a crash

Important Help menu items:
Emacs Tutorial		Learn-by-doing tutorial for using Emacs efficiently.
Emacs FAQ		Frequently asked questions and answers
\(Non)Warranty		GNU Emacs comes with ABSOLUTELY NO WARRANTY
Copying Conditions	Conditions for redistributing and changing Emacs.
Getting New Versions	How to obtain the latest version of Emacs.
Ordering Manuals	How to order manuals from the FSF.
")
	    (insert "\n\n" (emacs-version)
			    "
Copyright (C) 2002 Free Software Foundation, Inc."))

	;; No mouse menus, so give help using kbd commands.

	;; If keys have their default meanings,
	;; use precomputed string to save lots of time.
	(if (and (eq (key-binding "\C-h") 'help-command)
		 (eq (key-binding "\C-xu") 'advertised-undo)
		 (eq (key-binding "\C-x\C-c") 'save-buffers-kill-emacs)
		 (eq (key-binding "\C-ht") 'help-with-tutorial)
		 (eq (key-binding "\C-hi") 'info)
		 (eq (key-binding "\C-h\C-n") 'view-emacs-news))
	    (insert "
Get help	   C-h  (Hold down CTRL and press h)
Undo changes	   C-x u       Exit Emacs		C-x C-c
Get a tutorial	   C-h t       Use Info to read docs	C-h i
Ordering manuals   C-h RET")
	  (insert (substitute-command-keys
		   (format "\n
Get help	   %s
Undo changes	   \\[advertised-undo]
Exit Emacs	   \\[save-buffers-kill-emacs]
Get a tutorial	   \\[help-with-tutorial]
Use Info to read docs	\\[info]
Ordering manuals   \\[view-order-manuals]"
			   (let ((where (where-is-internal
					 'help-command nil t)))
			     (if where
				 (key-description where)
			       "M-x help"))))))

	;; Say how to use the menu bar with the keyboard.
	(if (and (eq (key-binding "\M-`") 'tmm-menubar)
		 (eq (key-binding [f10]) 'tmm-menubar))
	    (insert "
Activate menubar   F10  or  ESC `  or   M-`")
	  (insert (substitute-command-keys "
Activate menubar     \\[tmm-menubar]")))

	;; Many users seem to have problems with these.
	(insert "
\(`C-' means use the CTRL key.  `M-' means use the Meta (or Alt) key.
If you have no Meta key, you may instead type ESC followed by the character.)")

	(insert "\n\n" (emacs-version)
			"
Copyright (C) 2002 Free Software Foundation, Inc.")

	(if (and (eq (key-binding "\C-h\C-c") 'describe-copying)
		 (eq (key-binding "\C-h\C-d") 'describe-distribution)
		 (eq (key-binding "\C-h\C-w") 'describe-no-warranty))
	    (insert 
		     "\n
GNU Emacs comes with ABSOLUTELY NO WARRANTY; type C-h C-w for full details.
Emacs is Free Software--Free as in Freedom--so you can redistribute copies
of Emacs and modify it; type C-h C-c to see the conditions.
Type C-h C-d for information on getting the latest version.")
	  (insert (substitute-command-keys
		   "\n
GNU Emacs comes with ABSOLUTELY NO WARRANTY; type \\[describe-no-warranty] for full details.
Emacs is Free Software--Free as in Freedom--so you can redistribute copies
of Emacs and modify it; type \\[describe-copying] to see the conditions.
Type \\[describe-distribution] for information on getting the latest version."))))

      ;; The rest of the startup screen is the same on all
      ;; kinds of terminals.

      ;; Give information on recovering, if there was a crash.
      (and auto-save-list-file-prefix
	   ;; Don't signal an error if the
	   ;; directory for auto-save-list files
	   ;; does not yet exist.
	   (file-directory-p (file-name-directory
			      auto-save-list-file-prefix))
	   (directory-files
	    (file-name-directory auto-save-list-file-prefix)
	    nil
	    (concat "\\`"
		    (regexp-quote (file-name-nondirectory
				   auto-save-list-file-prefix)))
	    t)
	   (insert "\n\nIf an Emacs session crashed recently, "
		   "type M-x recover-session RET\nto recover"
		   " the files you were editing."))

      ;; Display the input that we set up in the buffer.
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (save-window-excursion
	(switch-to-buffer (current-buffer))
	(sit-for 120))))
  (kill-buffer "GNU Emacs"))

(defun startup-echo-area-message ()
  (if (eq (key-binding "\C-h\C-p") 'describe-project)
      "For information about the GNU Project and its goals, type C-h C-p."
    (substitute-command-keys
     "For information about the GNU Project and its goals, type \
\\[describe-project].")))


(defun display-startup-echo-area-message ()
  (let ((resize-mini-windows t))
    (message (startup-echo-area-message))))


(defun display-splash-screen ()
  "Display splash screen according to display.
Fancy splash screens are used on graphic displays,
normal otherwise."
  (interactive)
  (if (and (display-graphic-p)
	   (use-fancy-splash-screens-p))
      (fancy-splash-screens)
    (normal-splash-screen)))


(defun command-line-1 (command-line-args-left)
  (or noninteractive (input-pending-p) init-file-had-error
      ;; t if the init file says to inhibit the echo area startup message.
      (and inhibit-startup-echo-area-message
	   user-init-file
	   (or (and (get 'inhibit-startup-echo-area-message 'saved-value)
		    (equal inhibit-startup-echo-area-message
			   (if (string= init-file-user "")
			       (user-login-name)
			     init-file-user)))
	       ;; Wasn't set with custom; see if .emacs has a setq.
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
		   (kill-buffer buffer)))))
      (display-startup-echo-area-message))

  ;; Delay 2 seconds after an init file error message
  ;; was displayed, so user can read it.
  (if init-file-had-error
      (sit-for 2))

  (if command-line-args-left
      ;; We have command args; process them.
      (let ((dir command-line-default-directory)
	    (file-count 0)
	    first-file-buffer
	    tem
	    just-files ;; t if this follows the magic -- option.
	    ;; This includes our standard options' long versions
	    ;; and long versions of what's on command-switch-alist.
	    (longopts
	     (append '(("--funcall") ("--load") ("--insert") ("--kill")
		       ("--directory") ("--eval") ("--execute")
		       ("--find-file") ("--visit") ("--file"))
		     (mapcar (lambda (elt)
			       (list (concat "-" (car elt))))
			     command-switch-alist)))
	    (line 0)
	    (column 0))

	;; Add the long X options to longopts.
	(dolist (tem command-line-x-option-alist)
	  (if (string-match "^--" (car tem))
	      (push (list (car tem)) longopts)))

	;; Loop, processing options.
	(while (and command-line-args-left)
	  (let* ((argi (car command-line-args-left))
		 (orig-argi argi)
		 argval completion
		 ;; List of directories specified in -L/--directory,
		 ;; in reverse of the order specified.
		 extra-load-path
		 (initial-load-path load-path))
	    (setq command-line-args-left (cdr command-line-args-left))

	    ;; Do preliminary decoding of the option.
	    (if just-files
		;; After --, don't look for options; treat all args as files.
		(setq argi "")
	      ;; Convert long options to ordinary options
	      ;; and separate out an attached option argument into argval.
	      (if (string-match "^--[^=]*=" argi)
		  (setq argval (substring argi (match-end 0))
			argi (substring argi 0 (1- (match-end 0)))))
	      (if (equal argi "--")
		  (setq completion nil)
		(setq completion (try-completion argi longopts)))
	      (if (eq completion t)
		  (setq argi (substring argi 1))
		(if (stringp completion)
		    (let ((elt (assoc completion longopts)))
		      (or elt
			  (error "Option `%s' is ambiguous" argi))
		      (setq argi (substring (car elt) 1)))
		  (setq argval nil argi orig-argi))))

	    ;; Execute the option.
	    (cond ((setq tem (assoc argi command-switch-alist))
		   (if argval
		       (let ((command-line-args-left
			      (cons argval command-line-args-left)))
			 (funcall (cdr tem) argi))
		     (funcall (cdr tem) argi)))

		  ((member argi '("-f"	;what the manual claims
				  "-funcall"
				  "-e")) ; what the source used to say
		   (if argval
		       (setq tem (intern argval))
		     (setq tem (intern (car command-line-args-left)))
		     (setq command-line-args-left (cdr command-line-args-left)))
		   (if (arrayp (symbol-function tem))
		       (command-execute tem)
		     (funcall tem)))

		  ((member argi '("-eval" "-execute"))
		   (if argval
		       (setq tem argval)
		     (setq tem (car command-line-args-left))
		     (setq command-line-args-left (cdr command-line-args-left)))
		   (eval (read tem)))
		  ;; Set the default directory as specified in -L.

		  ((member argi '("-L" "-directory"))
		   (if argval
		       (setq tem argval)
		     (setq tem (car command-line-args-left)
			   command-line-args-left (cdr command-line-args-left)))
		   (setq tem (command-line-normalize-file-name tem))
		   (setq extra-load-path
			 (cons (expand-file-name tem) extra-load-path))
		   (setq load-path (append (nreverse extra-load-path)
					   initial-load-path)))

		  ((member argi '("-l" "-load"))
		   (if argval
		       (setq tem argval)
		     (setq tem (car command-line-args-left)
			   command-line-args-left (cdr command-line-args-left)))
		   (let ((file (command-line-normalize-file-name tem)))
		     ;; Take file from default dir if it exists there;
		     ;; otherwise let `load' search for it.
		     (if (file-exists-p (expand-file-name file))
			 (setq file (expand-file-name file)))
		     (load file nil t)))

		  ((string-equal argi "-insert")
		   (if argval
		       (setq tem argval)
		     (setq tem (car command-line-args-left)
			   command-line-args-left (cdr command-line-args-left)))
		   (or (stringp tem)
		       (error "File name omitted from `-insert' option"))
		   (insert-file-contents (command-line-normalize-file-name tem)))

		  ((string-equal argi "-kill")
		   (kill-emacs t))

		  ((string-match "^\\+[0-9]+\\'" argi)
		   (setq line (string-to-int argi)))

		  ((string-match "^\\+\\([0-9]+\\):\\([0-9]+\\)\\'" argi)
		   (setq line (string-to-int (match-string 1 argi))
			 column (string-to-int (match-string 2 argi))))

		  ((setq tem (assoc argi command-line-x-option-alist))
		   ;; Ignore X-windows options and their args if not using X.
		   (setq command-line-args-left
			 (nthcdr (nth 1 tem) command-line-args-left)))

		  ((member argi '("-find-file" "-file" "-visit"))
		   ;; An explicit option to specify visiting a file.
		   (if argval
		       (setq tem argval)
		     (setq tem (car command-line-args-left)
			   command-line-args-left (cdr command-line-args-left)))
		   (unless (stringp tem)
		     (error "File name omitted from `%s' option" argi))
		   (setq file-count (1+ file-count))
		   (let ((file (expand-file-name
				(command-line-normalize-file-name tem) dir)))
		     (if (= file-count 1)
			 (setq first-file-buffer (find-file file))
		       (find-file-other-window file)))
		   (or (zerop line)
		       (goto-line line))
		   (setq line 0)
		   (unless (< column 1)
		     (move-to-column (1- column)))
		   (setq column 0))

		  ((equal argi "--")
		   (setq just-files t))
		  (t
		   ;; We have almost exhausted our options. See if the
		   ;; user has made any other command-line options available
		   (let ((hooks command-line-functions)	;; lrs 7/31/89
			 (did-hook nil))
		     (while (and hooks
				 (not (setq did-hook (funcall (car hooks)))))
		       (setq hooks (cdr hooks)))
		     (if (not did-hook)
		       ;; Ok, presume that the argument is a file name
			 (progn
			   (if (string-match "\\`-" argi)
			       (error "Unknown option `%s'" argi))
			   (setq file-count (1+ file-count))
			   (let ((file
				  (expand-file-name
				   (command-line-normalize-file-name orig-argi)
				   dir)))
			     (if (= file-count 1)
				 (setq first-file-buffer (find-file file))
			       (find-file-other-window file)))
			   (or (zerop line)
			       (goto-line line))
			   (setq line 0)
			   (unless (< column 1)
			     (move-to-column (1- column)))
			   (setq column 0))))))))
	;; If 3 or more files visited, and not all visible,
	;; show user what they all are.  But leave the last one current.
	(and (> file-count 2)
	     (not noninteractive)
	     (not inhibit-startup-buffer-menu)
	     (or (get-buffer-window first-file-buffer)
		 (list-buffers)))))

  ;; Maybe display a startup screen.
  (when (and (not inhibit-startup-message) (not noninteractive)
	     ;; Don't display startup screen if init file
	     ;; has started some sort of server.
	     (not (and (fboundp 'process-list)
		       (process-list))))
    ;; Display a startup screen, after some preparations.

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
    (when (fboundp 'frame-notice-user-settings)
      (frame-notice-user-settings))

    ;; If there are no switches to process, we might as well
    ;; run this hook now, and there may be some need to do it
    ;; before doing any output.
    (when window-setup-hook
      (run-hooks 'window-setup-hook)
      ;; Don't let the hook be run twice.
      (setq window-setup-hook nil))

    ;; Do this now to avoid an annoying delay if the user
    ;; clicks the menu bar during the sit-for.
    (when (display-popup-menus-p)
      (precompute-menubar-bindings))
    (setq menubar-bindings-done t)

    ;; If *scratch* is selected and it is empty, insert an
    ;; initial message saying not to create a file there.
    (when (and initial-scratch-message
	       (string= (buffer-name) "*scratch*")
	       (= 0 (buffer-size)))
      (insert initial-scratch-message)
      (set-buffer-modified-p nil))

    ;; If user typed input during all that work,
    ;; abort the startup screen.  Otherwise, display it now.
    (unless (input-pending-p)
      (display-splash-screen))))


(defun command-line-normalize-file-name (file)
  "Collapse multiple slashes to one, to handle non-Emacs file names."
  (save-match-data
    ;; Use arg 1 so that we don't collapse // at the start of the file name.
    ;; That is significant on some systems.
    ;; However, /// at the beginning is supposed to mean just /, not //.
    (if (string-match "^///+" file)
	(setq file (replace-match "/" t t file)))
    (while (string-match "//+" file 1)
      (setq file (replace-match "/" t t file)))
    file))

;;; startup.el ends here
