;;; startup.el --- process Emacs shell arguments

;; Copyright (C) 1985, 1986, 1992, 1994, 1995, 1996, 1997, 1998, 1999, 2000,
;;   2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file parses the command line and gets Emacs running.  Options
;; on the command line are handled in precedence order.  For priorities
;; see the structure standard_args in the emacs.c file.

;;; Code:

(setq top-level '(normal-top-level))

(defvar command-line-processed nil
  "Non-nil once command line has been processed.")

(defvar window-system initial-window-system
  "Name of window system the selected frame is displaying through.
The value is a symbol--for instance, `x' for X windows.
The value is nil if the selected frame is on a text-only-terminal.")

(make-variable-frame-local 'window-system)

(defgroup initialization nil
  "Emacs start-up procedure."
  :group 'internal)

(defcustom inhibit-startup-message nil
  "*Non-nil inhibits the initial startup message.
This is for use in your personal init file, once you are familiar
with the contents of the startup message."
  :type 'boolean
  :group 'initialization)

(defvaralias 'inhibit-splash-screen 'inhibit-startup-message)

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
HANDLER-FUNCTION receives the switch string as its sole argument;
the remaining command-line args are in the variable `command-line-args-left'.")

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
    ("-nbi" 0 x-handle-switch icon-type nil)
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
    ("--no-bitmap-icon" 0 x-handle-switch icon-type nil)
    ("--iconic" 0 x-handle-iconic)
    ("--xrm" 1 x-handle-xrm-switch)
    ("--cursor-color" 1 x-handle-switch cursor-color)
    ("--vertical-scroll-bars" 0 x-handle-switch vertical-scroll-bars t)
    ("--line-spacing" 1 x-handle-numeric-switch line-spacing)
    ("--border-color" 1 x-handle-switch border-color)
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

(defvar inhibit-startup-hooks nil
  "Non-nil means don't run `term-setup-hook' and `emacs-startup-hook'.
This is because we already did so.")

(defvar keyboard-type nil
  "The brand of keyboard you are using.
This variable is used to define the proper function and keypad
keys for use under X.  It is used in a fashion analogous to the
environment variable TERM.")

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
higher incidence of change, don't make sense to load into Emacs's
dumped image.  Thus, the run-time load order is: 1. file described in
this variable, if non-nil; 2. `~/.emacs'; 3. `default.el'.

Don't use the `site-start.el' file for things some users may not like.
Put them in `default.el' instead, so that users can more easily
override them.  Users can prevent loading `default.el' with the `-q'
option or by setting `inhibit-default-init' in their own init files,
but inhibiting `site-start.el' requires `--no-site-file', which
is less convenient.

This variable is defined for customization so as to make
it visible in the relevant context.  However, actually customizing it
is not allowed, since it would not work anyway.  The only way to set
this variable usefully is to set it while building and dumping Emacs."
  :type '(choice (const :tag "none" nil) string)
  :group 'initialization
  :initialize 'custom-initialize-default
  :set '(lambda (variable value)
	  (error "Customizing `site-run-file' does not work")))

(defcustom mail-host-address nil
  "*Name of this machine, for purposes of naming users."
  :type '(choice (const nil) string)
  :group 'mail)

(defcustom user-mail-address (if command-line-processed
				 (concat (user-login-name) "@"
					 (or mail-host-address
					     (system-name)))
			       ;; Empty string means "not set yet".
			       "")
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

(defvar emacs-quick-startup nil)

(defvar emacs-basic-display nil)

(defvar init-file-debug nil)

(defvar init-file-had-error nil)

(defvar normal-top-level-add-subdirs-inode-list nil)

(defvar no-blinking-cursor nil)

(defvar default-frame-background-mode)

(defvar pure-space-overflow nil
  "Non-nil if building Emacs overflowed pure space.")

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
	     (canonicalized (if (fboundp 'untranslated-canonical-name)
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
      (with-current-buffer "*Messages*"
	(setq default-directory dir)))
    ;; `user-full-name' is now known; reset its standard-value here.
    (put 'user-full-name 'standard-value
	 (list (default-value 'user-full-name)))
    ;; For root, preserve owner and group when editing files.
    (if (equal (user-uid) 0)
	(setq backup-by-copying-when-mismatch t))
    ;; Look in each dir in load-path for a subdirs.el file.
    ;; If we find one, load it, which will add the appropriate subdirs
    ;; of that dir into load-path,
    ;; Look for a leim-list.el file too.  Loading it will register
    ;; available input methods.
    (let ((tail load-path) dir)
      (while tail
        (setq dir (car tail))
        (let ((default-directory dir))
          (load (expand-file-name "subdirs.el") t t t))
        (let ((default-directory dir))
          (load (expand-file-name "leim-list.el") t t t))
        ;; We don't use a dolist loop and we put this "setq-cdr" command at
        ;; the end, because the subdirs.el files may add elements to the end
        ;; of load-path and we want to take it into account.
        (setq tail (cdr tail))))
    (unless (eq system-type 'vax-vms)
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
			       process-environment))))))
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
	(unless inhibit-startup-hooks
	  (run-hooks 'emacs-startup-hook)
	  (and term-setup-hook
	       (run-hooks 'term-setup-hook)))

	;; Don't do this if we failed to create the initial frame,
	;; for instance due to a dense colormap.
	(when (or frame-initial-frame
		  ;; If frame-initial-frame has no meaning, do this anyway.
		  (not (and initial-window-system
			    (not noninteractive)
			    (not (eq initial-window-system 'pc)))))
	  ;; Modify the initial frame based on what .emacs puts into
	  ;; ...-frame-alist.
	  (if (fboundp 'frame-notice-user-settings)
	      (frame-notice-user-settings))
	  ;; Set the faces for the initial background mode even if
	  ;; frame-notice-user-settings didn't (such as on a tty).
	  ;; frame-set-background-mode is idempotent, so it won't
	  ;; cause any harm if it's already been done.
	  (if (fboundp 'frame-set-background-mode)
	      (frame-set-background-mode (selected-frame))))

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

(defvar handle-args-function-alist '((nil . tty-handle-args))
  "Functions for processing window-system dependent command-line arguments.
Window system startup files should add their own function to this
alist, which should parse the command line arguments.  Those
pertaining to the window system should be processed and removed
from the returned command line.")

(defvar window-system-initialization-alist '((nil . ignore))
  "Alist of window-system initialization functions.
Window-system startup files should add their own initialization
function to this list.  The function should take no arguments,
and initialize the window system environment to prepare for
opening the first frame (e.g. open a connection to an X server).")

;; Handle the X-like command-line arguments "-fg", "-bg", "-name", etc.
(defun tty-handle-args (args)
  (let (rest)
    (message "%s" args)
    (while (and args
		(not (equal (car args) "--")))
      (let* ((argi (pop args))
	     (orig-argi argi)
	     argval completion)
	;; Check for long options with attached arguments
	;; and separate out the attached option argument into argval.
	(when (string-match "^\\(--[^=]*\\)=" argi)
          (setq argval (substring argi (match-end 0))
                argi (match-string 1 argi)))
	(when (string-match "^--" argi)
	  (setq completion (try-completion argi tty-long-option-alist))
	  (if (eq completion t)
	      ;; Exact match for long option.
	      (setq argi (cdr (assoc argi tty-long-option-alist)))
	    (if (stringp completion)
		(let ((elt (assoc completion tty-long-option-alist)))
		  ;; Check for abbreviated long option.
		  (or elt
		      (error "Option `%s' is ambiguous" argi))
		  (setq argi (cdr elt)))
	      ;; Check for a short option.
	      (setq argval nil
                    argi orig-argi))))
	(cond ((member argi '("-fg" "-foreground"))
	       (push (cons 'foreground-color (or argval (pop args)))
                     default-frame-alist))
	      ((member argi '("-bg" "-background"))
	       (push (cons 'background-color (or argval (pop args)))
                     default-frame-alist))
	      ((member argi '("-T" "-name"))
	       (unless argval (setq argval (pop args)))
	       (push (cons 'title
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
                     default-frame-alist))
	      ((member argi '("-r" "-rv" "-reverse"))
	       (push '(reverse . t)
                     default-frame-alist))
	      ((equal argi "-color")
	       (unless argval (setq argval 8)) ; default --color means 8 ANSI colors
	       (push (cons 'tty-color-mode
                           (cond
                            ((numberp argval) argval)
                            ((string-match "-?[0-9]+" argval)
                             (string-to-number argval))
                            (t (intern argval))))
                     default-frame-alist))
	      (t
               (push argi rest)))))
    (nreverse rest)))

(defun command-line ()
  (setq command-line-default-directory default-directory)

  ;; Choose a reasonable location for temporary files.
  (custom-reevaluate-setting 'temporary-file-directory)
  (custom-reevaluate-setting 'small-temporary-file-directory)
  (custom-reevaluate-setting 'auto-save-file-name-transforms)

  ;; See if we should import version-control from the environment variable.
  (let ((vc (getenv "VERSION_CONTROL")))
    (cond ((eq vc nil))			;don't do anything if not set
	  ((member vc '("t" "numbered"))
	   (setq version-control t))
	  ((member vc '("nil" "existing"))
	   (setq version-control nil))
	  ((member vc '("never" "simple"))
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
    (setq eol-mnemonic-unix "(Unix)"
          eol-mnemonic-mac  "(Mac)"))
   ;; Both Mac and Unix EOLs are now "native" on Mac OS so keep the
   ;; abbreviated strings `/' and `:' set in coding.c for them.
   ((eq system-type 'macos)
    (setq eol-mnemonic-dos  "(DOS)"))
   (t                                   ; this is for Unix/GNU/Linux systems
    (setq eol-mnemonic-dos  "(DOS)"
          eol-mnemonic-mac  "(Mac)")))

  ;; Make sure window system's init file was loaded in loadup.el if using a window system.
  (condition-case error
    (unless noninteractive
      (if (and initial-window-system
	       (not (featurep
		     (intern (concat (symbol-name initial-window-system) "-win")))))
	  (error "Unsupported window system `%s'" initial-window-system))
      ;; Process window-system specific command line parameters.
      (setq command-line-args
	    (funcall (or (cdr (assq initial-window-system handle-args-function-alist))
			 (error "Unsupported window system `%s'" initial-window-system))
		     command-line-args))
      ;; Initialize the window system. (Open connection, etc.)
      (funcall (or (cdr (assq initial-window-system window-system-initialization-alist))
		   (error "Unsupported window system `%s'" initial-window-system))))
    ;; If there was an error, print the error message and exit.
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
     (setq initial-window-system nil)
     (kill-emacs)))

  (set-locale-environment nil)

  ;; Convert preloaded file names to absolute.
  (setq load-history
	(mapcar (lambda (elt)
		  (if (and (stringp (car elt))
			   (not (file-name-absolute-p (car elt))))
		      (cons (locate-file (car elt) load-path
					 load-suffixes)
			    (cdr elt))
		    elt))
		load-history))

  ;; Convert the arguments to Emacs internal representation.
  (let ((args (cdr command-line-args)))
    (while args
      (setcar args
	      (decode-coding-string (car args) locale-coding-system t))
      (pop args)))

  (let ((done nil)
	(args (cdr command-line-args)))

    ;; Figure out which user's init file to load,
    ;; either from the environment or from the options.
    (setq init-file-user (if noninteractive nil (user-login-name)))
    ;; If user has not done su, use current $HOME to find .emacs.
    (and init-file-user
         (equal init-file-user (user-real-login-name))
	 (setq init-file-user ""))

    ;; Process the command-line args, and delete the arguments
    ;; processed.  This is consistent with the way main in emacs.c
    ;; does things.
    (while (and (not done) args)
      (let* ((longopts '(("--no-init-file") ("--no-site-file") ("--debug-init")
                         ("--user") ("--iconic") ("--icon-type") ("--quick")
			 ("--no-blinking-cursor") ("--basic-display")))
             (argi (pop args))
             (orig-argi argi)
             argval)
	;; Handle --OPTION=VALUE format.
	(when (string-match "^\\(--[^=]*\\)=" argi)
	  (setq argval (substring argi (match-end 0))
                argi (match-string 1 argi)))
	(unless (equal argi "--")
	  (let ((completion (try-completion argi longopts)))
	    (if (eq completion t)
		(setq argi (substring argi 1))
	      (if (stringp completion)
		  (let ((elt (assoc completion longopts)))
		    (or elt
			(error "Option `%s' is ambiguous" argi))
		    (setq argi (substring (car elt) 1)))
		(setq argval nil
                      argi orig-argi)))))
	(cond
	 ((member argi '("-Q" "-quick"))
	  (setq init-file-user nil
		site-run-file nil
		emacs-quick-startup t))
	 ((member argi '("-D" "-basic-display"))
	  (setq no-blinking-cursor t
		emacs-basic-display t)
	  (push '(vertical-scroll-bars . nil) initial-frame-alist))
	 ((member argi '("-q" "-no-init-file"))
	  (setq init-file-user nil))
	 ((member argi '("-u" "-user"))
	  (setq init-file-user (or argval (pop args))
		argval nil))
	 ((equal argi "-no-site-file")
	  (setq site-run-file nil))
	 ((equal argi "-debug-init")
	  (setq init-file-debug t))
	 ((equal argi "-iconic")
	  (push '(visibility . icon) initial-frame-alist))
	 ((member argi '("-icon-type" "-i" "-itype"))
	  (push '(icon-type . t) default-frame-alist))
	 ((member argi '("-nbc" "-no-blinking-cursor"))
	  (setq no-blinking-cursor t))
	 ;; Push the popped arg back on the list of arguments.
	 (t
          (push argi args)
          (setq done t)))
	;; Was argval set but not used?
	(and argval
	     (error "Option `%s' doesn't allow an argument" argi))))

    ;; Re-attach the program name to the front of the arg list.
    (and command-line-args
         (setcdr command-line-args args)))

  ;; Under X Window, this creates the X frame and deletes the terminal frame.
  (when (fboundp 'frame-initialize)
    (frame-initialize))

  ;; Turn off blinking cursor if so specified in X resources.  This is here
  ;; only because all other settings of no-blinking-cursor are here.
  (unless (or noninteractive
	      emacs-basic-display
	      (and (memq window-system '(x w32 mac))
		   (not (member (x-get-resource "cursorBlink" "CursorBlink")
				'("off" "false")))))
    (setq no-blinking-cursor t))

  ;; If frame was created with a menu bar, set menu-bar-mode on.
  (unless (or noninteractive
	      emacs-basic-display
              (and (memq initial-window-system '(x w32))
                   (<= (frame-parameter nil 'menu-bar-lines) 0)))
    (menu-bar-mode 1))

  ;; If frame was created with a tool bar, switch tool-bar-mode on.
  (unless (or noninteractive
	      emacs-basic-display
              (not (display-graphic-p))
              (<= (frame-parameter nil 'tool-bar-lines) 0))
    (tool-bar-mode 1))

  ;; Can't do this init in defcustom because the relevant variables
  ;; are not set.
  (custom-reevaluate-setting 'blink-cursor-mode)
  (custom-reevaluate-setting 'tooltip-mode)

  (normal-erase-is-backspace-setup-frame)

  ;; Register default TTY colors for the case the terminal hasn't a
  ;; terminal init file.
  ;; We do this regardles of whether the terminal supports colors
  ;; or not, since they can switch that support on or off in
  ;; mid-session by setting the tty-color-mode frame parameter.
  (tty-register-default-colors)

  ;; Record whether the tool-bar is present before the user and site
  ;; init files are processed.  frame-notice-user-settings uses this
  ;; to determine if the tool-bar has been disabled by the init files,
  ;; and the frame needs to be resized.
  (when (fboundp 'frame-notice-user-settings)
    (let ((tool-bar-lines (or (assq 'tool-bar-lines initial-frame-alist)
                              (assq 'tool-bar-lines default-frame-alist))))
      (setq tool-bar-originally-present
            (and tool-bar-lines
                 (cdr tool-bar-lines)
                 (not (eq 0 (cdr tool-bar-lines)))))))

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

    ;; Warn for invalid user name.
    (and init-file-user
	 (not (file-directory-p (expand-file-name (concat "~" init-file-user))))
	 (display-warning 'initialization
			  (format "User %s has no home directory"
				  init-file-user)
			  :error))

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
			     ;; Prefer .emacs on Windows.
			     (if (directory-files "~" nil "^\\.emacs\\(\\.elc?\\)?$")
				 "~/.emacs"
			       ;; Also support _emacs for compatibility.
			       (if (directory-files "~" nil "^_emacs\\(\\.elc?\\)?$")
				   "~/_emacs"
				 ;; But default to .emacs if _emacs does not exist.
				 "~/.emacs")))
			    ((eq system-type 'vax-vms)
			     "sys$login:.emacs")
			    (t
			     (concat "~" init-file-user "/.emacs")))))
		      ;; This tells `load' to store the file name found
		      ;; into user-init-file.
		      (setq user-init-file t)
		      (load user-init-file-1 t t)

		      (when (eq user-init-file t)
			;; If we did not find ~/.emacs, try
			;; ~/.emacs.d/.emacs.
			(let ((otherfile
			       (expand-file-name
				(file-name-nondirectory user-init-file-1)
				(file-name-as-directory
				 (expand-file-name
				  ".emacs.d"
				  (file-name-directory user-init-file-1))))))
			  (load otherfile t t)

			  ;; If we did not find the user's init file,
			  ;; set user-init-file conclusively.
			  ;; Don't let it be set from default.el.
			  (when (eq user-init-file t)
			    (setq user-init-file user-init-file-1))))

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

		      (unless inhibit-default-init
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
				 (mapconcat (lambda (s) (prin1-to-string s t)) (cdr error) ", "))
			 "\n\n"
			 "To ensure normal operation, you should investigate and remove the\n"
			 "cause of the error in your initialization file.  Start Emacs with\n"
			 "the `--debug-init' option to view a complete error backtrace.\n\n"))
	       (message "Error in init file: %s%s%s"
			(get (car error) 'error-message)
			(if (cdr error) ": " "")
			(mapconcat 'prin1-to-string (cdr error) ", "))
	       (let ((pop-up-windows nil))
		 (pop-to-buffer "*Messages*"))
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
    (if (equal user-mail-address "")
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

  ;; Decode all default-directory.
  (if (and default-enable-multibyte-characters locale-coding-system)
      (save-excursion
	(dolist (elt (buffer-list))
	  (set-buffer elt)
	  (if default-directory
	      (setq default-directory
		    (decode-coding-string default-directory
					  locale-coding-system t))))
	(setq command-line-default-directory
	      (decode-coding-string command-line-default-directory
				    locale-coding-system t))))

  ;; If *scratch* exists and init file didn't change its mode, initialize it.
  (if (get-buffer "*scratch*")
      (with-current-buffer "*scratch*"
	(if (eq major-mode 'fundamental-mode)
	    (funcall initial-major-mode))))

  ;; Load library for our terminal type.
  ;; User init file can set term-file-prefix to nil to prevent this.
  (unless (or noninteractive
              initial-window-system)
    (tty-run-terminal-initialization (selected-frame)))

  ;; Update the out-of-memory error message based on user's key bindings
  ;; for save-some-buffers.
  (setq memory-signal-data
	(list 'error
	      (substitute-command-keys "Memory exhausted--use \\[save-some-buffers] then exit and restart Emacs")))

  ;; Process the remaining args.
  (command-line-1 (cdr command-line-args))

  ;; If -batch, terminate after processing the command options.
  (if noninteractive (kill-emacs t))

  ;; Run emacs-session-restore (session management) if started by
  ;; the session manager and we have a session manager connection.
  (if (and (boundp 'x-session-previous-id)
           (stringp x-session-previous-id))
      (with-no-warnings
	(emacs-session-restore x-session-previous-id))))

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
	   :face variable-pitch
           (lambda ()
             (let* ((en "TUTORIAL")
                    (tut (or (get-language-info current-language-environment
                                                'tutorial)
                             en))
                    (title (with-temp-buffer
                             (insert-file-contents
                              (expand-file-name tut data-directory)
                              nil 0 256)
                             (search-forward ".")
                             (buffer-substring (point-min) (1- (point))))))
               ;; If there is a specific tutorial for the current language
               ;; environment and it is not English, append its title.
               (concat
                "Emacs Tutorial\tLearn how to use Emacs efficiently"
                (if (string= en tut)
                    ""
                  (concat " (" title ")"))
                "\n")))
           :face variable-pitch "\
Emacs FAQ\tFrequently asked questions and answers
Read the Emacs Manual\tView the Emacs manual using Info
\(Non)Warranty\tGNU Emacs comes with "
	   :face (variable-pitch :slant oblique)
	   "ABSOLUTELY NO WARRANTY\n"
	   :face variable-pitch
	   "\
Copying Conditions\tConditions for redistributing and changing Emacs
Getting New Versions\tHow to obtain the latest version of Emacs
More Manuals / Ordering Manuals       Buying printed manuals from the FSF\n")
  (:face variable-pitch
	   "You can do basic editing with the menu bar and scroll bar \
using the mouse.\n\n"
	   :face (variable-pitch :weight bold)
	   "Useful File menu items:\n"
	   :face variable-pitch "\
Exit Emacs\t(Or type Control-x followed by Control-c)
Recover Crashed Session\tRecover files you were editing before a crash




"
	   ))
  "A list of texts to show in the middle part of splash screens.
Each element in the list should be a list of strings or pairs
`:face FACE', like `fancy-splash-insert' accepts them.")


(defgroup fancy-splash-screen ()
  "Fancy splash screen when Emacs starts."
  :version "21.1"
  :group 'initialization)


(defcustom fancy-splash-delay 7
  "*Delay in seconds between splash screens."
  :group 'fancy-splash-screen
  :type 'integer)


(defcustom fancy-splash-max-time 30
  "*Show splash screens for at most this number of seconds.
Values less than twice `fancy-splash-delay' are ignored."
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
Arguments from ARGS should be either strings, functions called
with no args that return a string, or pairs `:face FACE',
where FACE is a valid face specification, as it can be used with
`put-text-property'."
  (let ((current-face nil))
    (while args
      (if (eq (car args) :face)
	  (setq args (cdr args) current-face (car args))
	(insert (propertize (let ((it (car args)))
                              (if (functionp it)
                                  (funcall it)
                                it))
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
	(insert (propertize " " 'display
			    `(space :align-to (+ center (-0.5 . ,img)))))

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
  (fancy-splash-insert
   :face '(variable-pitch :foreground "red")
   (if (eq system-type 'gnu/linux)
       "GNU Emacs is one component of the GNU/Linux operating system."
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
			 "Copyright (C) 2005 Free Software Foundation, Inc.")
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
    (if pure-space-overflow
	(insert "Warning Warning  Pure space overflow   Warning Warning\n"))
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
  "Stop displaying the splash screen buffer.
This is an internal function used to turn off the splash screen after
the user caused an input event by hitting a key or clicking with the
mouse."
  (interactive)
  (if (and (consp last-command-event)
	   (eq (posn-window (event-start last-command-event))
	       (selected-window)))
      ;; This is a mouse-down event in the spash screen window.
      ;; Ignore it and consume the corresponding mouse-up event.
      (read-event)
    (push last-command-event unread-command-events))
  (throw 'exit nil))


(defun fancy-splash-screens ()
  "Display fancy splash screens when Emacs starts."
  (setq fancy-splash-help-echo (startup-echo-area-message))
  (let ((old-hourglass display-hourglass)
	(fancy-splash-outer-buffer (current-buffer))
	splash-buffer
	(old-minor-mode-map-alist minor-mode-map-alist)
	(frame (fancy-splash-frame))
	timer)
    (save-selected-window
      (select-frame frame)
      (switch-to-buffer "GNU Emacs")
      (setq tab-width 20)
      (setq splash-buffer (current-buffer))
      (catch 'stop-splashing
	(unwind-protect
	    (let ((map (make-sparse-keymap)))
	      (use-local-map map)
	      (define-key map [switch-frame] 'ignore)
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
					      fancy-splash-max-time)
		    timer (run-with-timer 0 fancy-splash-delay
					  #'fancy-splash-screens-1
					  splash-buffer))
	      (recursive-edit))
	  (cancel-timer timer)
	  (setq display-hourglass old-hourglass
		minor-mode-map-alist old-minor-mode-map-alist)
	  (kill-buffer splash-buffer))))))

(defun fancy-splash-frame ()
  "Return the frame to use for the fancy splash screen.
Returning non-nil does not mean we should necessarily
use the fancy splash screen, but if we do use it,
we put it on this frame."
  (let (chosen-frame)
    (dolist (frame (append (frame-list) (list (selected-frame))))
      (if (and (frame-visible-p frame)
	       (not (window-minibuffer-p (frame-selected-window frame))))
	  (setq chosen-frame frame)))
    chosen-frame))

(defun use-fancy-splash-screens-p ()
  "Return t if fancy splash screens should be used."
  (when (and (display-graphic-p)
             (or (and (display-color-p)
		 (image-type-available-p 'xpm))
                 (image-type-available-p 'pbm)))
    (let ((frame (fancy-splash-frame)))
      (when frame
	(let* ((img (create-image (or fancy-splash-image
				      (if (and (display-color-p)
					       (image-type-available-p 'xpm))
					  "splash.xpm" "splash.pbm"))))
	       (image-height (and img (cdr (image-size img))))
	       (window-height (1- (window-height (frame-selected-window frame)))))
	  (> window-height (+ image-height 19)))))))


(defun normal-splash-screen ()
  "Display splash screen when Emacs starts."
  (let ((prev-buffer (current-buffer)))
    (unwind-protect
	(with-current-buffer (get-buffer-create "GNU Emacs")
	  (let ((tab-width 8)
		(mode-line-format (propertize "---- %b %-"
					      'face '(:weight bold))))

	    (if pure-space-overflow
		(insert "Warning Warning  Pure space overflow   Warning Warning\n"))

	    ;; The convention for this piece of code is that
	    ;; each piece of output starts with one or two newlines
	    ;; and does not end with any newlines.
	    (insert "Welcome to GNU Emacs")
	    (insert
	     (if (eq system-type 'gnu/linux)
		 ", one component of the GNU/Linux operating system.\n"
	       ", a part of the GNU operating system.\n"))

	    (unless (equal (buffer-name prev-buffer) "*scratch*")
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
Recover Crashed Session	Recover files you were editing before a crash

Important Help menu items:
Emacs Tutorial		Learn how to use Emacs efficiently
Emacs FAQ		Frequently asked questions and answers
Read the Emacs Manual	View the Emacs manual using Info
\(Non)Warranty		GNU Emacs comes with ABSOLUTELY NO WARRANTY
Copying Conditions	Conditions for redistributing and changing Emacs
Getting New Versions	How to obtain the latest version of Emacs
More Manuals / Ordering Manuals    How to order printed manuals from the FSF
")
		  (insert "\n\n" (emacs-version)
			  "
Copyright (C) 2005 Free Software Foundation, Inc."))

	      ;; No mouse menus, so give help using kbd commands.

	      ;; If keys have their default meanings,
	      ;; use precomputed string to save lots of time.
	      (if (and (eq (key-binding "\C-h") 'help-command)
		       (eq (key-binding "\C-xu") 'advertised-undo)
		       (eq (key-binding "\C-x\C-c") 'save-buffers-kill-emacs)
		       (eq (key-binding "\C-ht") 'help-with-tutorial)
		       (eq (key-binding "\C-hi") 'info)
		       (eq (key-binding "\C-hr") 'info-emacs-manual)
		       (eq (key-binding "\C-h\C-n") 'view-emacs-news))
		  (insert "
Get help	   C-h  (Hold down CTRL and press h)
Emacs manual	   C-h r
Emacs tutorial	   C-h t           Undo changes     C-x u
Buy manuals        C-h C-m         Exit Emacs	    C-x C-c
Browse manuals     C-h i")

		(insert (substitute-command-keys
			 (format "\n
Get help	   %s
Emacs manual	   \\[info-emacs-manual]
Emacs tutorial	   \\[help-with-tutorial]\tUndo changes\t\\[advertised-undo]
Buy manuals        \\[view-order-manuals]\tExit Emacs\t\\[save-buffers-kill-emacs]
Browse manuals     \\[info]"
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
Copyright (C) 2005 Free Software Foundation, Inc.")

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
      ;; Unwind ... ensure splash buffer is killed
      (kill-buffer "GNU Emacs"))))


(defun startup-echo-area-message ()
  (if (eq (key-binding "\C-h\C-p") 'describe-project)
      "For information about the GNU Project and its goals, type C-h C-p."
    (substitute-command-keys
     "For information about the GNU Project and its goals, type \
\\[describe-project].")))


(defun display-startup-echo-area-message ()
  (let ((resize-mini-windows t))
    (message "%s" (startup-echo-area-message))))


(defun display-splash-screen ()
  "Display splash screen according to display.
Fancy splash screens are used on graphic displays,
normal otherwise."
  (interactive)
  (if (use-fancy-splash-screens-p)
      (fancy-splash-screens)
    (normal-splash-screen)))


(defun command-line-1 (command-line-args-left)
  (or noninteractive (input-pending-p) init-file-had-error
      ;; t if the init file says to inhibit the echo area startup message.
      (and inhibit-startup-echo-area-message
	   user-init-file
	   (or (and (get 'inhibit-startup-echo-area-message 'saved-value)
		    (equal inhibit-startup-echo-area-message
			   (if (equal init-file-user "")
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
			       (if (equal init-file-user "")
				   (user-login-name)
				 init-file-user)))
			     "[ \t\n]*)")
			    nil t))
		       (error nil))
		   (kill-buffer buffer)))))
      ;; display-splash-screen at the end of command-line-1 calls
      ;; use-fancy-splash-screens-p. This can cause image.el to be
      ;; loaded, putting "Loading image... done" in the echo area.
      ;; This hides startup-echo-area-message. So
      ;; use-fancy-splash-screens-p is called here simply to get the
      ;; loading of image.el (if needed) out of the way before
      ;; display-startup-echo-area-message runs.
      (progn
        (use-fancy-splash-screens-p)
        (display-startup-echo-area-message)))

  ;; Delay 2 seconds after an init file error message
  ;; was displayed, so user can read it.
  (when init-file-had-error
    (sit-for 2))

  (when command-line-args-left
    ;; We have command args; process them.
    (let ((dir command-line-default-directory)
          (file-count 0)
          first-file-buffer
          tem
          ;; This approach loses for "-batch -L DIR --eval "(require foo)",
          ;; if foo is intended to be found in DIR.
          ;;
          ;; ;; The directories listed in --directory/-L options will *appear*
          ;; ;; at the front of `load-path' in the order they appear on the
          ;; ;; command-line.  We cannot do this by *placing* them at the front
          ;; ;; in the order they appear, so we need this variable to hold them,
          ;; ;; temporarily.
          ;; extra-load-path
          ;;
          ;; To DTRT we keep track of the splice point and modify `load-path'
          ;; straight away upon any --directory/-L option.
          splice
          just-files ;; t if this follows the magic -- option.
          ;; This includes our standard options' long versions
          ;; and long versions of what's on command-switch-alist.
          (longopts
           (append '(("--funcall") ("--load") ("--insert") ("--kill")
                     ("--directory") ("--eval") ("--execute") ("--no-splash")
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
      (while command-line-args-left
        (let* ((argi (car command-line-args-left))
               (orig-argi argi)
               argval completion)
          (setq command-line-args-left (cdr command-line-args-left))

          ;; Do preliminary decoding of the option.
          (if just-files
              ;; After --, don't look for options; treat all args as files.
              (setq argi "")
            ;; Convert long options to ordinary options
            ;; and separate out an attached option argument into argval.
            (when (string-match "^\\(--[^=]*\\)=" argi)
              (setq argval (substring argi (match-end 0))
                    argi (match-string 1 argi)))
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
                (setq argval nil
                      argi orig-argi))))

          ;; Execute the option.
          (cond ((setq tem (assoc argi command-switch-alist))
                 (if argval
                     (let ((command-line-args-left
                            (cons argval command-line-args-left)))
                       (funcall (cdr tem) argi))
                   (funcall (cdr tem) argi)))

                ((equal argi "-no-splash")
                 (setq inhibit-startup-message t))

                ((member argi '("-f"	; what the manual claims
                                "-funcall"
                                "-e"))  ; what the source used to say
                 (setq tem (intern (or argval (pop command-line-args-left))))
                 (if (commandp tem)
                     (command-execute tem)
                   (funcall tem)))

                ((member argi '("-eval" "-execute"))
                 (eval (read (or argval (pop command-line-args-left)))))

                ((member argi '("-L" "-directory"))
                 (setq tem (expand-file-name
                            (command-line-normalize-file-name
                             (or argval (pop command-line-args-left)))))
                 (cond (splice (setcdr splice (cons tem (cdr splice)))
                               (setq splice (cdr splice)))
                       (t (setq load-path (cons tem load-path)
                                splice load-path))))

                ((member argi '("-l" "-load"))
                 (let* ((file (command-line-normalize-file-name
                               (or argval (pop command-line-args-left))))
                        ;; Take file from default dir if it exists there;
                        ;; otherwise let `load' search for it.
                        (file-ex (expand-file-name file)))
                   (when (file-exists-p file-ex)
                     (setq file file-ex))
                   (load file nil t)))

		;; This is used to handle -script.  It's not clear
		;; we need to document it.
                ((member argi '("-scriptload"))
                 (let* ((file (command-line-normalize-file-name
                               (or argval (pop command-line-args-left))))
                        ;; Take file from default dir.
                        (file-ex (expand-file-name file)))
                   (load file-ex nil t t)))

                ((equal argi "-insert")
                 (setq tem (or argval (pop command-line-args-left)))
                 (or (stringp tem)
                     (error "File name omitted from `-insert' option"))
                 (insert-file-contents (command-line-normalize-file-name tem)))

                ((equal argi "-kill")
                 (kill-emacs t))

                ((string-match "^\\+[0-9]+\\'" argi)
                 (setq line (string-to-number argi)))

                ((string-match "^\\+\\([0-9]+\\):\\([0-9]+\\)\\'" argi)
                 (setq line (string-to-number (match-string 1 argi))
                       column (string-to-number (match-string 2 argi))))

                ((setq tem (assoc argi command-line-x-option-alist))
                 ;; Ignore X-windows options and their args if not using X.
                 (setq command-line-args-left
                       (nthcdr (nth 1 tem) command-line-args-left)))

                ((member argi '("-find-file" "-file" "-visit"))
                 ;; An explicit option to specify visiting a file.
                 (setq tem (or argval (pop command-line-args-left)))
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
                 (let ((hooks command-line-functions) ;; lrs 7/31/89
                       (did-hook nil))
                   (while (and hooks
                               (not (setq did-hook (funcall (car hooks)))))
                     (setq hooks (cdr hooks)))
                   (if (not did-hook)
                       ;; Presume that the argument is a file name.
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
  (unless (or inhibit-startup-message
	      noninteractive
	      emacs-quick-startup)
    ;; Display a startup screen, after some preparations.

    ;; If there are no switches to process, we might as well
    ;; run this hook now, and there may be some need to do it
    ;; before doing any output.
    (run-hooks 'emacs-startup-hook)
    (and term-setup-hook
	 (run-hooks 'term-setup-hook))
    (setq inhibit-startup-hooks t)

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
    (with-no-warnings
     (setq menubar-bindings-done t))

    ;; If *scratch* is selected and it is empty, insert an
    ;; initial message saying not to create a file there.
    (when (and initial-scratch-message
	       (equal (buffer-name) "*scratch*")
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

;; arch-tag: 7e294698-244d-4758-984b-4047f887a5db
;;; startup.el ends here
