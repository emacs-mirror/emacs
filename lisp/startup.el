;;; startup.el --- process Emacs shell arguments

;; Copyright (C) 1985, 86, 92, 94, 95, 96, 97, 1998, 1999 Free Software Foundation, Inc.

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
;; -d DISPNAME               Use DISPNAME as the name of the X-windows
;; -display DISPNAME         display for the initial frame.
;; --display DISPNAME        This option is handled by emacs.c
;; -------------------------
;; -nw                       Do not use a windows system (but use the
;; --no-windows              terminal instead.)
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
;; -itype ICONTYPE           iconified under X-windows.
;; --icon-type ICONTYPE      This option is passed on to term/x-win.el
;;
;; -iconic                   Start Emacs iconified under X-windows.
;; --iconic                  This option is passed on to term/x-win.el
;; -------------------------
;; Various X-windows options for colors/fonts/geometry/title etc.
;; These options are passed on to term/x-win.el which see.  Certain
;; of these are also found in term/pc-win.el
;; -------------------------
;; FILE                      Visit FILE.
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
  "Non-nil once command line has been processed")

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
(defvar command-line-x-option-alist
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
    ("-ib" 1 x-handle-numeric-switch internal-border-width)
    ("-g" 1 x-handle-geometry)
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
    ("--border-color" 1 x-handle-switch border-width))
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

(defconst iso-8859-n-locale-regexp "8859[-_]?\\([1-49]\\)\\>"
  "Regexp that specifies when to enable an ISO 8859-N character set.
We do that if this regexp matches the locale name specified by
one of the environment variables LC_ALL, LC_CTYPE, or LANG.
The paren group in the regexp should match the specific character
set number, N.  Currently only Latin-[12345] are supported.
\(Note that Latin-5 is ISO 8859-9, because 8859-[678] are non-Latin
alphabets; hence, supported values of N are [12349].\)")

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
  (if (eq system-type 'ms-dos)
      "~/_s"  ; MS-DOS cannot have initial dot, and allows only 8.3 names
    "~/.saves-")
  "Prefix for generating `auto-save-list-file-name'.
This is used after reading your `.emacs' file to initialize
`auto-save-list-file-name', by appending Emacs's pid and the system name,
if you have not already set `auto-save-list-file-name' yourself.
Set this to nil if you want to prevent `auto-save-list-file-name'
from being initialized."
  :type '(choice (const :tag "Don't record a session's auto save list" nil)
		 string)
  :group 'auto-save)

(defvar locale-translation-file-name
  (let ((files '("/usr/lib/X11/locale/locale.alias" ; e.g. X11R6.4
		 "/usr/X11R6/lib/X11/locale/locale.alias" ; e.g. RedHat 4.2
		 "/usr/openwin/lib/locale/locale.alias" ; e.g. Solaris 2.6
		 ;;
		 ;; The following name appears after the X-related names above,
		 ;; since the X-related names are what X actually uses.
		 "/usr/share/locale/locale.alias" ; GNU/Linux sans X
		 )))
    (while (and files (not (file-exists-p (car files))))
      (setq files (cdr files)))
    (car files))
  "*File name for the system's file of locale-name aliases, or nil if none.")

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
      (setq dirs (cons (car pending) dirs))
      (setq pending (cdr pending))
      (setq attrs (nthcdr 10 (file-attributes (car dirs))))
      (let ((contents (directory-files (car dirs)))
	    (default-directory (car dirs)))
	(unless (member attrs normal-top-level-add-subdirs-inode-list)
	  (setq normal-top-level-add-subdirs-inode-list
		(cons attrs normal-top-level-add-subdirs-inode-list))
	  (while contents
	    (unless (member (car contents) '("." ".." "RCS" "CVS"))
	      (when (and (string-match "\\`[a-zA-Z0-9]" (car contents))
			 ;; Avoid doing a `stat' when it isn't necessary
			 ;; because that can cause trouble when an NFS server
			 ;; is down.
			 (not (string-match "\\.elc?\\'" (car contents)))
			 (file-directory-p (car contents)))
		(let ((expanded (expand-file-name (car contents))))
		  (unless (file-exists-p (expand-file-name ".nosearch"
							   expanded))
		    (setq pending (nconc pending (list expanded)))))))
	    (setq contents (cdr contents))))))
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
		(not (equal thisdir (car tail)))
		(not (and (memq system-type '(ms-dos windows-nt))
			  (equal (downcase thisdir) (downcase (car tail))))))
      (setq tail (cdr tail)))
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
    ;; For root, preserve owner and group when editing files.
    (if (equal (user-uid) 0)
	(setq backup-by-copying-when-mismatch t))
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
		       (if (eq system-type 'ms-dos)
			   (concat 
			    (make-temp-name
			     (expand-file-name auto-save-list-file-prefix))
			    "~")

			 (expand-file-name (format "%s%d-%s~"
						   auto-save-list-file-prefix
						   (emacs-pid)
						   (system-name)))))))
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
	    (if (memq window-system '(x w32))
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

  (let ((ctype
	 ;; Use the first of these three envvars that has a nonempty value.
	 (or (let ((string (getenv "LC_ALL")))
	       (and (not (equal string "")) string))
	     (let ((string (getenv "LC_CTYPE")))
	       (and (not (equal string "")) string))
	     (let ((string (getenv "LANG")))
	       (and (not (equal string "")) string)))))
    ;; Translate "swedish" into "sv_SE.ISO-8859-1", and so on,
    ;; using the translation file that GNU/Linux systems have.
    (and ctype
	 locale-translation-file-name
	 (not (string-match iso-8859-n-locale-regexp ctype))
	 (with-temp-buffer
	   (insert-file-contents locale-translation-file-name)
	   (if (re-search-forward
		(concat "^" (regexp-quote ctype) ":?[ \t]+") nil t)
	       (setq ctype (buffer-substring (point)
					     (progn (end-of-line) (point)))))))
    ;; Now see if the locale specifies an ISO 8859 character set.
    (when (and ctype
	       (string-match iso-8859-n-locale-regexp ctype))
      (let (charset (which (match-string 1 ctype)))
	(if (equal "9" which)
	    (setq which "5"))
	(setq charset (concat "latin-" which))
	(when (string-match "latin-[12345]" charset)
	  ;; Set up for this character set.
	  ;; This is now the right way to do it
	  ;; for both unibyte and multibyte modes.
	  (set-language-environment charset)
	  (unless (or noninteractive (eq window-system 'x))
	    ;; Send those codes literally to a non-X terminal.
	    (when default-enable-multibyte-characters
	      ;; If this is nil, we are using single-byte characters,
	      ;; so the terminal coding system is irrelevant.
	      (set-terminal-coding-system
	       (intern (downcase charset)))))
	  (standard-display-european-internal)))))

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
   ;; Mac-specific settings should come here, once there's a
   ;; system-type symbol specific to MacOS.
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
	 ((or (string-equal argi "-q")
	      (string-equal argi "-no-init-file"))
	  (setq init-file-user nil
		args (cdr args)))
	 ((or (string-equal argi "-u")
	      (string-equal argi "-user"))
	  (or argval
	      (setq args (cdr args)
		    argval (car args)))
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
    (and command-line-args (setcdr command-line-args args)))

  ;; Under X Windows, this creates the X frame and deletes the terminal frame.
  (if (fboundp 'frame-initialize)
      (frame-initialize))
  ;; If frame was created with a menu bar, set menu-bar-mode on.
  (if (or (not (memq window-system '(x w32)))
	  (> (cdr (assq 'menu-bar-lines (frame-parameters))) 0))
      (menu-bar-mode t))

  (run-hooks 'before-init-hook)

  ;; Run the site-start library if it exists.  The point of this file is
  ;; that it is run before .emacs.  There is no point in doing this after
  ;; .emacs; that is useless.
  (if site-run-file 
      (load site-run-file t t))

  ;; Register available input methods by loading LEIM list file.
  (load "leim-list.el" 'noerror 'nomessage 'nosuffix)

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
			(setq user-init-file nil))
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

  (run-hooks 'after-init-hook)

  ;; If *scratch* exists and init file didn't change its mode, initialize it.
  (if (get-buffer "*scratch*")
      (save-excursion
	(set-buffer "*scratch*")
	(if (eq major-mode 'fundamental-mode)
	    (funcall initial-major-mode))))

  ;; Register default TTY colors for the case the terminal hasn't a
  ;; terminal init file.  The colors are good for xterm-color and the
  ;; FreeBSD console (cons.*).  They should be sufficient for Linux
  ;; too, I guess.
  (or (eq window-system 'pc)	; pc-win.el did this already
      (let ((colors '("black" "red" "green" "yellow" "blue" "magenta"
		      "cyan" "white"))
	    (i 0))
	(while colors
	  (face-register-tty-color (car colors) i)
	  (setq colors (cdr colors) i (1+ i)))))
  
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

(defcustom initial-scratch-message "\
This buffer is for notes you don't want to save, and for Lisp evaluation.
If you want to create a file, visit that file with C-x C-f,
then enter the text in that file's own buffer.

"
  "Initial message displayed in *scratch* buffer at startup.
If this is nil, no message will be displayed."
  :type 'string)

(defun command-line-1 (command-line-args-left)
  (or noninteractive (input-pending-p) init-file-had-error
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
      (message (if (eq (key-binding "\C-h\C-p") 'describe-project)
		   "For information about the GNU Project and its goals, type C-h C-p."
		 (substitute-command-keys
		  "For information about the GNU Project and its goals, type \\[describe-project]."))))
  (when (and (not noninteractive)
	     (memq window-system '(x w32)))
    (make-mode-line-mouse-sensitive))
  (if (null command-line-args-left)
      (cond ((and (not inhibit-startup-message) (not noninteractive)
		  ;; Don't clobber a non-scratch buffer if init file
		  ;; has selected it.
		  (string= (buffer-name) "*scratch*"))
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
	     (when (memq window-system '(x w32))
	       (precompute-menubar-bindings))
	     (setq menubar-bindings-done t)
	     (when (= (buffer-size) 0)
	       (let ((buffer-undo-list t))
		 (unwind-protect
		     (when (not (input-pending-p))
		       (goto-char (point-max))
		       ;; The convention for this piece of code is that
		       ;; each piece of output starts with one or two newlines
		       ;; and does not end with any newlines.
		       (insert "Welcome to GNU Emacs")
		       (if (eq system-type 'gnu/linux)
			   (insert ", one component of a Linux-based GNU system."))
		       (insert "\n")
		       (if (assq 'display (frame-parameters))
			   (progn
			     (insert "\
The menu bar and scroll bar are sufficient for basic editing with the mouse.

Useful Files menu items:
Exit Emacs		(or type Control-x followed by Control-c)
Recover Session		recover files you were editing before a crash

Important Help menu items:
Emacs Tutorial		Learn-by-doing tutorial for using Emacs efficiently.
\(Non)Warranty		GNU Emacs comes with ABSOLUTELY NO WARRANTY
Copying Conditions	Conditions for redistributing and changing Emacs.
Getting New Versions	How to obtain the latest version of Emacs.
")
			     (insert "\n\n" (emacs-version)
				     "
Copyright (C) 1999 Free Software Foundation, Inc."))
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
Get a tutorial	   C-h t       Use Info to read docs	C-h i")
			   (insert (substitute-command-keys
				    (format "\n
Get help	   %s
Undo changes	   \\[advertised-undo]
Exit Emacs	   \\[save-buffers-kill-emacs]
Get a tutorial	   \\[help-with-tutorial]
Use Info to read docs	\\[info]"
					    (let ((where (where-is-internal
							  'help-command nil t)))
					      (if where
						  (key-description where)
						"M-x help"))))))
			 ;; Say how to use the menu bar
			 ;; if that is not with the mouse.
			 (if (and (eq (key-binding "\M-`") 'tmm-menubar)
				  (eq (key-binding [f10]) 'tmm-menubar))
			     (insert "
Activate menubar   F10  or  ESC `  or   M-`")
			   (insert (substitute-command-keys "
Activate menubar     \\[tmm-menubar]")))

		       ;; Windows and MSDOS (currently) do not count as
		       ;; window systems, but do have mouse support.
			 (if window-system
			     (insert "
Mode-specific menu   C-mouse-3 (third button, with CTRL)"))
			 ;; Many users seem to have problems with these.
			 (insert "
\(`C-' means use the CTRL key.  `M-' means use the Meta (or Alt) key.
If you have no Meta key, you may instead type ESC followed by the character.)")
			 (and auto-save-list-file-prefix
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

			 (insert "\n\n" (emacs-version)
				 "
Copyright (C) 1999 Free Software Foundation, Inc.")
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
		       (goto-char (point-min))

		       (set-buffer-modified-p nil)
		       (sit-for 120))
		   (with-current-buffer (get-buffer "*scratch*")
		     (erase-buffer)
		     (and initial-scratch-message
			  (insert initial-scratch-message))
		     (set-buffer-modified-p nil)))))))
    ;; Delay 2 seconds after the init file error message
    ;; was displayed, so user can read it.
    (if init-file-had-error
	(sit-for 2))
    (let ((dir command-line-default-directory)
	  (file-count 0)
	  first-file-buffer
	  tem
	  just-files  ;; t if this follows the magic -- option.
	  ;; This includes our standard options' long versions
	  ;; and long versions of what's on command-switch-alist.
	  (longopts
	   (append '(("--funcall") ("--load") ("--insert") ("--kill")
		     ("--directory") ("--eval") ("--find-file") ("--visit"))
		   (mapcar '(lambda (elt)
			      (list (concat "-" (car elt))))
			   command-switch-alist)))
	  (line 0))

      ;; Add the long X options to longopts.
      (setq tem command-line-x-option-alist)
      (while tem
	(if (string-match "^--" (car (car tem)))
	    (setq longopts (cons (list (car (car tem))) longopts)))
	(setq tem (cdr tem)))

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
		((string-equal argi "-eval")
		 (if argval
		     (setq tem argval)
		   (setq tem (car command-line-args-left))
		   (setq command-line-args-left (cdr command-line-args-left)))
		 (eval (read tem)))
		;; Set the default directory as specified in -L.
		((or (string-equal argi "-L")
		     (string-equal argi "-directory"))
		 (if argval
		     (setq tem argval)
		   (setq tem (car command-line-args-left)
			 command-line-args-left (cdr command-line-args-left)))
		 (setq tem (command-line-normalize-file-name tem))
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
		((setq tem (assoc argi command-line-x-option-alist))
		 ;; Ignore X-windows options and their args if not using X.
		 (setq command-line-args-left
		       (nthcdr (nth 1 tem) command-line-args-left)))
		((or (string-equal argi "-find-file")
		     (string-equal argi "-visit"))
		 ;; An explicit option to specify visiting a file.
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
		 (setq line 0))
		((equal argi "--")
		 (setq just-files t))
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
			 (setq line 0))))))))
      ;; If 3 or more files visited, and not all visible,
      ;; show user what they all are.
      (and (> file-count 2)
	   (not noninteractive)
	   (or (get-buffer-window first-file-buffer)
	       (progn (other-window 1)
		      (buffer-menu)))))))

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
