;;; startup.el --- process Emacs shell arguments  -*- lexical-binding: t -*-

;; Copyright (C) 1985-1986, 1992, 1994-2019 Free Software Foundation,
;; Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: internal
;; Package: emacs

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file parses the command line and gets Emacs running.  Options
;; on the command line are handled in precedence order.  For priorities
;; see the structure standard_args in the emacs.c file.

;;; Code:

(setq top-level '(normal-top-level))

(defvar command-line-processed nil
  "Non-nil once command line has been processed.")

(defgroup initialization nil
  "Emacs start-up procedure."
  :group 'environment)

(defcustom initial-buffer-choice nil
  "Buffer to show after starting Emacs.
If the value is nil and `inhibit-startup-screen' is nil, show the
startup screen.  If the value is a string, switch to a buffer
visiting the file or directory that the string specifies.  If the
value is a function, call it with no arguments and switch to the buffer
that it returns.  If t, open the `*scratch*' buffer.

When `initial-buffer-choice' is non-nil, the startup screen is
inhibited.

If you use `emacsclient' with no target file, then it obeys any
string or function value that this variable has."
  :type '(choice
	  (const     :tag "Startup screen" nil)
	  (directory :tag "Directory" :value "~/")
	  (file      :tag "File" :value "~/.emacs")
	  ;; Note sure about hard-coding this as an option...
	  (const     :tag "Remember Mode notes buffer" remember-notes)
	  (function  :tag "Function")
	  (const     :tag "Lisp scratch buffer" t))
  :version "23.1")

(defvaralias 'inhibit-splash-screen 'inhibit-startup-screen)
(defvaralias 'inhibit-startup-message 'inhibit-startup-screen)

(defcustom inhibit-startup-screen nil
  "Non-nil inhibits the startup screen.

This is for use in your personal init file (but NOT site-start.el),
once you are familiar with the contents of the startup screen."
  :type 'boolean)

(defvar startup-screen-inhibit-startup-screen nil)

;; The mechanism used to ensure that only end users can disable this
;; message is not complex.  Clearly, it is possible for a determined
;; system administrator to inhibit this message anyway, but at least
;; they will do so with knowledge of why the Emacs developers think
;; this is a bad idea.
(defcustom inhibit-startup-echo-area-message nil
  "Non-nil inhibits the initial startup echo area message.

The startup message is in the echo area as it provides information
about GNU Emacs and the GNU system in general, which we want all
users to see.  As this is the least intrusive startup message,
this variable gets specialized treatment to prevent the message
from being disabled site-wide by systems administrators, while
still allowing individual users to do so.

Setting this variable takes effect only if you do it with the
customization buffer or if your init file contains a line of this
form:
 (setq inhibit-startup-echo-area-message \"YOUR-USER-NAME\")
If your init file is byte-compiled, use the following form
instead:
 (eval \\='(setq inhibit-startup-echo-area-message \"YOUR-USER-NAME\"))
Thus, someone else using a copy of your init file will see the
startup message unless he personally acts to inhibit it."
  :type '(choice (const :tag "Don't inhibit")
		 (string :tag "Enter your user name, to inhibit")))

(defcustom inhibit-default-init nil
  "Non-nil inhibits loading the `default' library."
  :type 'boolean)

(defcustom inhibit-startup-buffer-menu nil
  "Non-nil inhibits display of buffer list when more than 2 files are loaded."
  :type 'boolean)

(defvar command-switch-alist nil
  "Alist of command-line switches.
Elements look like (SWITCH-STRING . HANDLER-FUNCTION).
HANDLER-FUNCTION receives the switch string as its sole argument;
the remaining command-line args are in the variable `command-line-args-left'.")

(with-no-warnings
  (defvaralias 'argv 'command-line-args-left
    "List of command-line args not yet processed.
This is a convenience alias, so that one can write (pop argv)
inside of --eval command line arguments in order to access
following arguments."))
(internal-make-var-non-special 'argv)

(defvar command-line-args-left nil
  "List of command-line args not yet processed.")

(with-no-warnings
  (defvar argi nil
    "Current command-line argument."))
(internal-make-var-non-special 'argi)

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

;; This is here, rather than in x-win.el, so that we can ignore these
;; options when we are not using X.
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
    ("-mm" 0 x-handle-initial-switch fullscreen maximized)
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
    ("--maximized" 0 x-handle-initial-switch fullscreen maximized)
    ("--internal-border" 1 x-handle-numeric-switch internal-border-width)
    ("--geometry" 1 x-handle-geometry)
    ("--foreground-color" 1 x-handle-switch foreground-color)
    ("--background-color" 1 x-handle-switch background-color)
    ("--mouse-color" 1 x-handle-switch mouse-color)
    ("--no-bitmap-icon" 0 x-handle-no-bitmap-icon)
    ("--iconic" 0 x-handle-iconic)
    ("--xrm" 1 x-handle-xrm-switch)
    ("--cursor-color" 1 x-handle-switch cursor-color)
    ("--vertical-scroll-bars" 0 x-handle-switch vertical-scroll-bars t)
    ("--line-spacing" 1 x-handle-numeric-switch line-spacing)
    ("--border-color" 1 x-handle-switch border-color)
    ("--smid" 1 x-handle-smid)
    ("--parent-id" 1 x-handle-parent-id))
  "Alist of X Windows options.
Each element has the form
  (NAME NUMARGS HANDLER FRAME-PARAM VALUE)
where NAME is the option name string, NUMARGS is the number of arguments
that the option accepts, HANDLER is a function to call to handle the option.
FRAME-PARAM (optional) is the frame parameter this option specifies,
and VALUE is the value which is given to that frame parameter
\(most options use the argument for this, so VALUE is not present).")

(defconst command-line-ns-option-alist
  '(("-NSAutoLaunch" 1 ns-ignore-1-arg)
    ("-NXAutoLaunch" 1 ns-ignore-1-arg)
    ("-macosx" 0 ignore)
    ("-NSHost" 1 ns-ignore-1-arg)
    ("-_NSMachLaunch" 1 ns-ignore-1-arg)
    ("-MachLaunch" 1 ns-ignore-1-arg)
    ("-NXOpen" 1 ns-ignore-1-arg)
    ("-NSOpen" 1 ns-handle-nxopen)
    ("-NXOpenTemp" 1 ns-ignore-1-arg)
    ("-NSOpenTemp" 1 ns-handle-nxopentemp)
    ("-GSFilePath" 1 ns-handle-nxopen)
    ;;("-bw" .              x-handle-numeric-switch)
    ;;("-d" .               x-handle-display)
    ;;("-display" .         x-handle-display)
    ("-name" 1 x-handle-name-switch)
    ("-title" 1 x-handle-switch title)
    ("-T" 1 x-handle-switch title)
    ("-r" 0 x-handle-switch reverse t)
    ("-rv" 0 x-handle-switch reverse t)
    ("-reverse" 0 x-handle-switch reverse t)
    ("-fn" 1 x-handle-switch font)
    ("-font" 1 x-handle-switch font)
    ("-ib" 1 x-handle-numeric-switch internal-border-width)
    ("-g" 1 x-handle-geometry)
    ("-geometry" 1 x-handle-geometry)
    ("-fg" 1 x-handle-switch foreground-color)
    ("-foreground" 1 x-handle-switch foreground-color)
    ("-bg" 1 x-handle-switch background-color)
    ("-background" 1 x-handle-switch background-color)
;    ("-ms" 1 x-handle-switch mouse-color)
    ("-itype" 0 x-handle-switch icon-type t)
    ("-i" 0 x-handle-switch icon-type t)
    ("-iconic" 0 x-handle-iconic icon-type t)
    ;;("-xrm" .             x-handle-xrm-switch)
    ("-cr" 1 x-handle-switch cursor-color)
    ("-vb" 0 x-handle-switch vertical-scroll-bars t)
    ("-hb" 0 x-handle-switch horizontal-scroll-bars t)
    ("-bd" 1 x-handle-switch)
    ;; ("--border-width" 1 x-handle-numeric-switch border-width)
    ;; ("--display" 1 ns-handle-display)
    ("--name" 1 x-handle-name-switch)
    ("--title" 1 x-handle-switch title)
    ("--reverse-video" 0 x-handle-switch reverse t)
    ("--font" 1 x-handle-switch font)
    ("--internal-border" 1 x-handle-numeric-switch internal-border-width)
    ;; ("--geometry" 1 ns-handle-geometry)
    ("--foreground-color" 1 x-handle-switch foreground-color)
    ("--background-color" 1 x-handle-switch background-color)
    ("--mouse-color" 1 x-handle-switch mouse-color)
    ("--icon-type" 0 x-handle-switch icon-type t)
    ("--iconic" 0 x-handle-iconic)
    ;; ("--xrm" 1 ns-handle-xrm-switch)
    ("--cursor-color" 1 x-handle-switch cursor-color)
    ("--vertical-scroll-bars" 0 x-handle-switch vertical-scroll-bars t)
    ("--border-color" 1 x-handle-switch border-width))
  "Alist of NS options.
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
  "Normal hook run after initializing the Emacs session.
It is run after Emacs loads the init file, `default' library, the
abbrevs file, and additional Lisp packages (if any), and setting
the value of `after-init-time'.

There is no `condition-case' around the running of this hook;
therefore, if `debug-on-error' is non-nil, an error in one of
these functions will invoke the debugger.")

(defvar emacs-startup-hook nil
  "Normal hook run after loading init files and handling the command line.")

(defvar term-setup-hook nil
  "Normal hook run immediately after `emacs-startup-hook'.
In new code, there is no reason to use this instead of `emacs-startup-hook'.
If you want to execute terminal-specific Lisp code, for example
to override the definitions made by the terminal-specific file,
see `tty-setup-hook'.")

(make-obsolete-variable 'term-setup-hook
			"use either `emacs-startup-hook' or \
`tty-setup-hook' instead." "24.4")

(defvar inhibit-startup-hooks nil
  "Non-nil means don't run some startup hooks, because we already did.
Currently this applies to: `emacs-startup-hook', `term-setup-hook',
and `window-setup-hook'.")

(defvar early-init-file nil
  "File name, including directory, of user's early init file.
See `user-init-file'.  The only difference is that
`early-init-file' is not set during the course of evaluating the
early init file.")

(defvar keyboard-type nil
  "The brand of keyboard you are using.
This variable is used to define the proper function and keypad
keys for use under X.  It is used in a fashion analogous to the
environment variable TERM.")

(defvar window-setup-hook nil
  "Normal hook run after loading init files and handling the command line.
This is very similar to `emacs-startup-hook'.  The only difference
is that this hook runs after frame parameters have been set up in
response to any settings from your init file.  Unless this matters
to you, use `emacs-startup-hook' instead.  (The name of this hook
is due to historical reasons, and does not reflect its purpose very well.)")

(defcustom initial-major-mode 'lisp-interaction-mode
  "Major mode command symbol to use for the initial `*scratch*' buffer."
  :type 'function)

(defvar init-file-user nil
  "Identity of user whose init file is or was read.
The value is nil if `-q' or `--no-init-file' was specified,
meaning do not load any init file.

Otherwise, the value may be an empty string, meaning
use the init file for the user who originally logged in,
or it may be a string containing a user's name meaning
use that person's init file.

In either of the latter cases, `(concat \"~\" init-file-user \"/\")'
evaluates to the name of the directory where the init file was
looked for.

Setting `init-file-user' does not prevent Emacs from loading
`site-start.el'.  The only way to do that is to use `--no-site-file'.")

(defcustom site-run-file (purecopy "site-start")
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
  :initialize #'custom-initialize-default
  :set (lambda (_variable _value)
	  (error "Customizing `site-run-file' does not work")))

(make-obsolete-variable 'system-name "use (system-name) instead" "25.1")

(defcustom mail-host-address nil
  "The name of this machine, for use in constructing email addresses.
If this is nil, Emacs uses `system-name'."
  :type '(choice (const nil) string)
  :group 'mail)

(defcustom user-mail-address
  (or (getenv "EMAIL")
      (concat (user-login-name) "@" (or mail-host-address (system-name))))
  "The email address of the current user.
This defaults to either: the value of EMAIL environment variable; or
user@host, using `user-login-name' and `mail-host-address' (or `system-name')."
  :initialize 'custom-initialize-delay
  :set-after '(mail-host-address)
  :type 'string
  :group 'mail)

(defcustom auto-save-list-file-prefix
  (cond ((eq system-type 'ms-dos)
	 ;; MS-DOS cannot have initial dot, and allows only 8.3 names
	 (concat user-emacs-directory "auto-save.list/_s"))
	(t
	 (concat user-emacs-directory "auto-save-list/.saves-")))
  "Prefix for generating `auto-save-list-file-name'.
This is used after reading your init file to initialize
`auto-save-list-file-name', by appending Emacs's pid and the system name,
if you have not already set `auto-save-list-file-name' yourself.
Directories in the prefix will be created if necessary.
Set this to nil if you want to prevent `auto-save-list-file-name'
from being initialized."
  :type '(choice (const :tag "Don't record a session's auto save list" nil)
		 string)
  :group 'auto-save)

(defvar emacs-basic-display nil)

(defvar init-file-debug nil)

(defvar init-file-had-error nil
  "Non-nil if there was an error loading the user's init file.")

(defvar normal-top-level-add-subdirs-inode-list nil)

(defvar no-blinking-cursor nil)

(defvar pure-space-overflow nil
  "Non-nil if building Emacs overflowed pure space.")

(defvar pure-space-overflow-message (purecopy "\
Warning Warning!!!  Pure space overflow    !!!Warning Warning
\(See the node Pure Storage in the Lisp manual for details.)\n"))

(defcustom tutorial-directory
  (file-name-as-directory (expand-file-name "tutorials" data-directory))
  "Directory containing the Emacs TUTORIAL files."
  :group 'installation
  :type 'directory
  :initialize #'custom-initialize-delay)

(defun normal-top-level-add-subdirs-to-load-path ()
  "Recursively add all subdirectories of `default-directory' to `load-path'.
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
	     (canonicalized (if (fboundp 'w32-untranslated-canonical-name)
				(w32-untranslated-canonical-name this-dir))))
	;; The Windows version doesn't report meaningful inode numbers, so
	;; use the canonicalized absolute file name of the directory instead.
	(setq attrs (or canonicalized
			(nthcdr 10 (file-attributes this-dir))))
	(unless (member attrs normal-top-level-add-subdirs-inode-list)
	  (push attrs normal-top-level-add-subdirs-inode-list)
	  (dolist (file contents)
	    (and (string-match "\\`[[:alnum:]]" file)
		 ;; The lower-case variants of RCS and CVS are for DOS/Windows.
		 (not (member file '("RCS" "CVS" "rcs" "cvs")))
		 ;; Avoid doing a `stat' when it isn't necessary because
		 ;; that can cause trouble when an NFS server is down.
		 (not (string-match "\\.elc?\\'" file))
		 (file-directory-p file)
		 (let ((expanded (expand-file-name file)))
		   (or (file-exists-p (expand-file-name ".nosearch" expanded))
		       (setq pending (nconc pending (list expanded))))))))))
    (normal-top-level-add-to-load-path (cdr (nreverse dirs)))))

(defun normal-top-level-add-to-load-path (dirs)
  "This function is called from a subdirs.el file.
It assumes that `default-directory' is the directory in which the
subdirs.el file exists, and it adds to `load-path' the subdirs of
that directory as specified in DIRS.  Normally the elements of
DIRS are relative."
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
  "Emacs calls this function when it first starts up.
It sets `command-line-processed', processes the command-line,
reads the initialization files, etc.
It is the default value of the variable `top-level'."
  (if command-line-processed
      (message internal--top-level-message)
    (setq command-line-processed t)

    ;; Look in each dir in load-path for a subdirs.el file.  If we
    ;; find one, load it, which will add the appropriate subdirs of
    ;; that dir into load-path.  This needs to be done before setting
    ;; the locale environment, because the latter might need to load
    ;; some support files.
    ;; Look for a leim-list.el file too.  Loading it will register
    ;; available input methods.
    (let ((tail load-path)
          (lispdir (expand-file-name "../lisp" data-directory))
          dir)
      (while tail
        (setq dir (car tail))
        (let ((default-directory dir))
          (load (expand-file-name "subdirs.el") t t t))
        ;; Do not scan standard directories that won't contain a leim-list.el.
        ;; https://lists.gnu.org/r/emacs-devel/2009-10/msg00502.html
        ;; (Except the preloaded one in lisp/leim.)
        (or (string-prefix-p lispdir dir)
            (let ((default-directory dir))
              (load (expand-file-name "leim-list.el") t t t)))
        ;; We don't use a dolist loop and we put this "setq-cdr" command at
        ;; the end, because the subdirs.el files may add elements to the end
        ;; of load-path and we want to take it into account.
        (setq tail (cdr tail))))

    ;; Set the default strings to display in mode line for end-of-line
    ;; formats that aren't native to this platform.  This should be
    ;; done before calling set-locale-environment, as the latter might
    ;; use these mnemonics.
    (cond
     ((memq system-type '(ms-dos windows-nt))
      (setq eol-mnemonic-unix "(Unix)"
	    eol-mnemonic-mac  "(Mac)"))
     (t                                   ; this is for Unix/GNU/Linux systems
      (setq eol-mnemonic-dos  "(DOS)"
	    eol-mnemonic-mac  "(Mac)")))

    (set-locale-environment nil)
    ;; Decode all default-directory's (probably, only *scratch* exists
    ;; at this point).  default-directory of *scratch* is the basis
    ;; for many other file-name variables and directory lists, so it
    ;; is important to decode it ASAP.
    (when locale-coding-system
      (let ((coding (if (eq system-type 'windows-nt)
			;; MS-Windows build converts all file names to
			;; UTF-8 during startup.
			'utf-8
		      locale-coding-system)))
	(save-excursion
	  (dolist (elt (buffer-list))
	    (set-buffer elt)
	    (if default-directory
		(setq default-directory
                      (if (eq system-type 'windows-nt)
                          ;; We pass the decoded default-directory as
                          ;; the 2nd arg to expand-file-name to make
                          ;; sure it sees a multibyte string as the
                          ;; default directory; this avoids the side
                          ;; effect of returning a unibyte string from
                          ;; expand-file-name because it still sees
                          ;; the undecoded value of default-directory.
                          (let ((defdir (decode-coding-string default-directory
                                                              coding t)))
                            ;; Convert backslashes to forward slashes.
                            (expand-file-name defdir defdir))
                        (decode-coding-string default-directory coding t))))))

	;; Decode all the important variables and directory lists, now
	;; that we know the locale's encoding.  This is because the
	;; values of these variables are until here unibyte undecoded
	;; strings created by build_unibyte_string.  data-directory in
	;; particular is used to construct many other standard
	;; directory names, so it must be decoded ASAP.  Note that
	;; charset-map-path cannot be decoded here, since we could
	;; then be trapped in infinite recursion below, when we load
	;; subdirs.el, because encoding a directory name might need to
	;; load a charset map, which will want to encode
	;; charset-map-path, which will want to load the same charset
	;; map...  So decoding of charset-map-path is delayed until
	;; further down below.
	(dolist (pathsym '(load-path exec-path))
	  (let ((path (symbol-value pathsym)))
	    (if (listp path)
		(set pathsym (mapcar (lambda (dir)
				       (decode-coding-string dir coding t))
				     path)))))
	(dolist (filesym '(data-directory doc-directory exec-directory
					  installation-directory
					  invocation-directory invocation-name
					  source-directory
					  shared-game-score-directory))
	  (let ((file (symbol-value filesym)))
	    (if (stringp file)
		(set filesym (decode-coding-string file coding t)))))))

    (let ((dir default-directory))
      (with-current-buffer "*Messages*"
        (messages-buffer-mode)
        ;; Make it easy to do like "tail -f".
        (set (make-local-variable 'window-point-insertion-type) t)
        ;; Give *Messages* the same default-directory as *scratch*,
        ;; just to keep things predictable.
	(setq default-directory (or dir (expand-file-name "~/")))))
    ;; `user-full-name' is now known; reset its standard-value here.
    (put 'user-full-name 'standard-value
	 (list (default-value 'user-full-name)))
    ;; If the PWD environment variable isn't accurate, delete it.
    (let ((pwd (getenv "PWD")))
      (and (stringp pwd)
	   ;; Use FOO/., so that if FOO is a symlink, file-attributes
	   ;; describes the directory linked to, not FOO itself.
	   (or (and default-directory
		    (equal (file-attributes
		       (concat (file-name-as-directory pwd) "."))
		      (file-attributes
		       (concat (file-name-as-directory default-directory)
			       "."))))
	       (setq process-environment
		     (delete (concat "PWD=" pwd)
			     process-environment)))))
    ;; Now, that other directories were searched, and any charsets we
    ;; need for encoding them are already loaded, we are ready to
    ;; decode charset-map-path.
    (if (listp charset-map-path)
	(let ((coding (if (eq system-type 'windows-nt)
			  'utf-8
			locale-coding-system)))
	  (setq charset-map-path
		(mapcar (lambda (dir)
			  (decode-coding-string dir coding t))
			charset-map-path))))
    (if default-directory
	(setq default-directory (abbreviate-file-name default-directory))
      (display-warning 'initialization "Error setting default-directory"))
    (let ((old-face-font-rescale-alist face-font-rescale-alist))
      (unwind-protect
	  (command-line)
	;; Do this again, in case .emacs defined more abbreviations.
	(if default-directory
	    (setq default-directory (abbreviate-file-name default-directory)))
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
	  (run-hooks 'emacs-startup-hook 'term-setup-hook))

	;; Don't do this if we failed to create the initial frame,
	;; for instance due to a dense colormap.
	(when (or frame-initial-frame
		  ;; If frame-initial-frame has no meaning, do this anyway.
		  (not (and initial-window-system
			    (not noninteractive)
			    (not (eq initial-window-system 'pc)))))

	  ;; FIXME: The user's init file may change
	  ;; face-font-rescale-alist.  However, the default face
	  ;; already has an assigned font object, which does not take
	  ;; face-font-rescale-alist into account.  For such
	  ;; situations, we ought to have a way to find all font
	  ;; objects and regenerate them; currently we do not.  As a
	  ;; workaround, we specifically reset te default face's :font
	  ;; attribute here.  See bug#1785.
	  (unless (eq face-font-rescale-alist
		      old-face-font-rescale-alist)
	    (set-face-attribute 'default nil :font (font-spec)))

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
	(unless inhibit-startup-hooks
	  (run-hooks 'window-setup-hook))))
    ;; Subprocesses of Emacs do not have direct access to the terminal, so
    ;; unless told otherwise they should only assume a dumb terminal.
    ;; We are careful to do it late (after term-setup-hook), although the
    ;; new multi-tty code does not use $TERM any more there anyway.
    (setenv "TERM" "dumb")
    ;; Remove DISPLAY from the process-environment as well.  This allows
    ;; `callproc.c' to give it a useful adaptive default which is either
    ;; the value of the `display' frame-parameter or the DISPLAY value
    ;; from initial-environment.
    (let ((display (frame-parameter nil 'display)))
      ;; Be careful which DISPLAY to remove from process-environment: follow
      ;; the logic of `callproc.c'.
      (if (stringp display) (setq display (concat "DISPLAY=" display))
        (dolist (varval initial-environment)
          (if (string-match "\\`DISPLAY=" varval)
              (setq display varval))))
      (when display
        (delete display process-environment)))))

;; Precompute the keyboard equivalents in the menu bar items.
;; Command-line options supported by tty's:
(defconst tty-long-option-alist
  '(("--name"		  . "-name")
    ("--title"		  . "-T")
    ("--reverse-video"	  . "-reverse")
    ("--foreground-color" . "-fg")
    ("--background-color" . "-bg")
    ("--color"		  . "-color")))

(defconst tool-bar-images-pixel-height 24
  "Height in pixels of images in the tool-bar.")

(cl-defgeneric handle-args-function (args)
  "Method for processing window-system dependent command-line arguments.
Window system startup files should add their own function to this
method, which should parse the command line arguments.  Those
pertaining to the window system should be processed and removed
from the returned command line.")
(cl-defmethod handle-args-function (args &context (window-system nil))
  (tty-handle-args args))

(cl-defgeneric window-system-initialization (&optional _display)
  "Method for window-system initialization.
Window-system startup files should add their own implementation
to this method.  The function should initialize the window system environment
to prepare for opening the first frame (e.g. open a connection to an X server)."
  nil)

(defun tty-handle-args (args)
  "Handle the X-like command-line arguments \"-fg\", \"-bg\", \"-name\", etc."
  (let (rest)
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
                               (setq argval (copy-sequence invocation-name))

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
    (nconc (nreverse rest) args)))

(declare-function x-get-resource "frame.c"
		  (attribute class &optional component subclass))
(declare-function tool-bar-mode "tool-bar" (&optional arg))
(declare-function tool-bar-setup "tool-bar")

(defvar server-name)
(defvar server-process)

(defun startup--setup-quote-display (&optional style)
  "If needed, display ASCII approximations to curved quotes.
Do this by modifying `standard-display-table'.  Optional STYLE
specifies the desired quoting style, as in `text-quoting-style'.
If STYLE is nil, display appropriately for the terminal."
  (let ((repls (let ((style-repls (assq style '((grave . "`'\"\"")
                                                (straight . "''\"\"")))))
                 (if style-repls (cdr style-repls) (make-vector 4 nil))))
        glyph-count)
    ;; REPLS is a sequence of the four replacements for "‘’“”", respectively.
    ;; If STYLE is nil, infer REPLS from terminal characteristics.
    (unless style
      ;; On a terminal that supports glyph codes,
      ;; GLYPH-COUNT[i] is the number of times that glyph code I
      ;; represents either an ASCII character or one of the 4
      ;; quote characters.  This assumes glyph codes are valid
      ;; Elisp characters, which is a safe assumption in practice.
      (when (integerp (internal-char-font nil (max-char)))
        (setq glyph-count (make-char-table nil 0))
        (dotimes (i 132)
          (let ((glyph (internal-char-font
                        nil (if (< i 128) i (aref "‘’“”" (- i 128))))))
            (when (<= 0 glyph)
              (aset glyph-count glyph (1+ (aref glyph-count glyph)))))))
      (dotimes (i 2)
        (let ((lq (aref "‘“" i)) (rq (aref "’”" i))
              (lr (aref "`\"" i)) (rr (aref "'\"" i))
              (i2 (* i 2)))
          (unless (if glyph-count
                      ;; On a terminal that supports glyph codes, use
                      ;; ASCII replacements unless both quotes are displayable.
                      ;; If not using ASCII replacements, highlight
                      ;; quotes unless they are both unique among the
                      ;; 128 + 4 characters of concern.
                      (let ((lglyph (internal-char-font nil lq))
                            (rglyph (internal-char-font nil rq)))
                        (when (and (<= 0 lglyph) (<= 0 rglyph))
                          (setq lr lq rr rq)
                          (and (= 1 (aref glyph-count lglyph))
                               (= 1 (aref glyph-count rglyph)))))
                    ;; On a terminal that does not support glyph codes, use
                    ;; ASCII replacements unless both quotes are displayable.
                    (and (char-displayable-p lq)
                         (char-displayable-p rq)))
            (aset repls i2 lr)
            (aset repls (1+ i2) rr)))))
    (dotimes (i 4)
      (let ((char (aref "‘’“”" i))
            (repl (aref repls i)))
        (if repl
            (aset (or standard-display-table
                      (setq standard-display-table (make-display-table)))
                  char (vector (make-glyph-code repl 'homoglyph)))
          (when standard-display-table
            (aset standard-display-table char nil)))))))

(defun startup--load-user-init-file
    (filename-function &optional alternate-filename-function load-defaults)
  "Load a user init-file.
FILENAME-FUNCTION is called with no arguments and should return
the name of the init-file to load.  If this file cannot be
loaded, and ALTERNATE-FILENAME-FUNCTION is non-nil, then it is
called with no arguments and should return the name of an
alternate init-file to load.  If LOAD-DEFAULTS is non-nil, then
load default.el after the init-file.

This function sets `user-init-file' to the name of the loaded
init-file, or to a default value if loading is not possible."
  (let ((debug-on-error-from-init-file nil)
        (debug-on-error-should-be-set nil)
        (debug-on-error-initial
         (if (eq init-file-debug t)
             'startup
           init-file-debug)))
    (let ((debug-on-error debug-on-error-initial))
      (condition-case-unless-debug error
          (when init-file-user
            (let ((init-file-name (funcall filename-function)))

              ;; If `user-init-file' is t, then `load' will store
              ;; the name of the file that it loads into
              ;; `user-init-file'.
              (setq user-init-file t)
              (load (if (equal (file-name-extension init-file-name)
                               "el")
                        (file-name-sans-extension init-file-name)
                      init-file-name)
                    'noerror 'nomessage)

              (when (and (eq user-init-file t) alternate-filename-function)
                (let ((alt-file (funcall alternate-filename-function)))
                  (and (equal (file-name-extension alt-file) "el")
                       (setq alt-file (file-name-sans-extension alt-file)))
                  (load alt-file 'noerror 'nomessage)))

              ;; If we did not find the user's init file, set
              ;; user-init-file conclusively.  Don't let it be
              ;; set from default.el.
              (when (eq user-init-file t)
                (setq user-init-file init-file-name)))

            ;; If we loaded a compiled file, set `user-init-file' to
            ;; the source version if that exists.
            (when (equal (file-name-extension user-init-file)
                         "elc")
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

            (when load-defaults

              ;; Prevent default.el from changing the value of
              ;; `inhibit-startup-screen'.
              (let ((inhibit-startup-screen nil))
                (load "default" 'noerror 'nomessage))))
        (error
         (display-warning
          'initialization
          (format-message "\
An error occurred while loading `%s':\n\n%s%s%s\n\n\
To ensure normal operation, you should investigate and remove the
cause of the error in your initialization file.  Start Emacs with
the `--debug-init' option to view a complete error backtrace."
                          user-init-file
                          (get (car error) 'error-message)
                          (if (cdr error) ": " "")
                          (mapconcat (lambda (s) (prin1-to-string s t))
                                     (cdr error) ", "))
          :warning)
         (setq init-file-had-error t)))

      ;; If we can tell that the init file altered debug-on-error,
      ;; arrange to preserve the value that it set up.
      (or (eq debug-on-error debug-on-error-initial)
          (setq debug-on-error-should-be-set t
                debug-on-error-from-init-file debug-on-error)))

    (when debug-on-error-should-be-set
      (setq debug-on-error debug-on-error-from-init-file))))

(defun command-line ()
  "A subroutine of `normal-top-level'.
Amongst another things, it parses the command-line arguments."
  (setq before-init-time (current-time)
	after-init-time nil
        command-line-default-directory default-directory)

  ;; Force recomputation, in case it was computed during the dump.
  (setq abbreviated-home-dir nil)

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

  ;; Convert preloaded file names in load-history to absolute.
  (let ((simple-file-name
	 ;; Look for simple.el or simple.elc and use their directory
	 ;; as the place where all Lisp files live.
	 (locate-file "simple" load-path (get-load-suffixes)))
	lisp-dir)
    ;; Don't abort if simple.el cannot be found, but print a warning.
    ;; Although in most usage we are going to cryptically abort a moment
    ;; later anyway, due to missing required bidi data files (eg bug#13430).
    (if (null simple-file-name)
	(let ((standard-output 'external-debugging-output)
	      (lispdir (expand-file-name "../lisp" data-directory)))
	  (princ "Warning: Could not find simple.el or simple.elc")
	  (terpri)
	  (when (getenv "EMACSLOADPATH")
	    (princ "The EMACSLOADPATH environment variable is set, \
please check its value")
	    (terpri))
	  (unless (file-readable-p lispdir)
	    (princ (format "Lisp directory %s not readable?" lispdir))
	    (terpri)))
      (setq lisp-dir (file-truename (file-name-directory simple-file-name)))
      (setq load-history
	    (mapcar (lambda (elt)
		      (if (and (stringp (car elt))
			       (not (file-name-absolute-p (car elt))))
			  (cons (concat lisp-dir
					(car elt))
				(cdr elt))
			elt))
		    load-history))))

  ;; Convert the arguments to Emacs internal representation.
  (let ((args command-line-args))
    (while args
      (setcar args
	      (decode-coding-string (car args) locale-coding-system t))
      (pop args)))

  (let ((done nil)
	(args (cdr command-line-args))
	display-arg)

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
      (let* ((longopts '(("--no-init-file") ("--no-site-file")
                         ("--no-x-resources") ("--debug-init")
                         ("--user") ("--iconic") ("--icon-type") ("--quick")
			 ("--no-blinking-cursor") ("--basic-display")
                         ("--dump-file") ("--temacs")))
             (argi (pop args))
             (orig-argi argi)
             argval)
	;; Handle --OPTION=VALUE format.
	(when (string-match "\\`\\(--[^=]*\\)=" argi)
	  (setq argval (substring argi (match-end 0))
                argi (match-string 1 argi)))
	(when (string-match "\\`--." orig-argi)
	  (let ((completion (try-completion argi longopts)))
	    (cond ((eq completion t)
		   (setq argi (substring argi 1)))
		  ((stringp completion)
		   (let ((elt (assoc completion longopts)))
		     (unless elt
		       (error "Option `%s' is ambiguous" argi))
		     (setq argi (substring (car elt) 1))))
		  (t
		   (setq argval nil
			 argi orig-argi)))))
	(cond
	 ;; The --display arg is handled partly in C, partly in Lisp.
	 ;; When it shows up here, we just put it back to be handled
	 ;; by `command-line-1'.
	 ((member argi '("-d" "-display"))
	  (setq display-arg (list argi (pop args))))
	 ((member argi '("-Q" "-quick"))
	  (setq init-file-user nil
		site-run-file nil
		inhibit-x-resources t)
	  ;; Stop it showing up in emacs -Q's customize-rogue.
	  (put 'site-run-file 'standard-value '(nil)))
         ((member argi '("-no-x-resources"))
          (setq inhibit-x-resources t))
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
	  (setq site-run-file nil)
	  (put 'site-run-file 'standard-value '(nil)))
	 ((equal argi "-debug-init")
	  (setq init-file-debug t))
	 ((equal argi "-iconic")
	  (push '(visibility . icon) initial-frame-alist))
	 ((member argi '("-nbc" "-no-blinking-cursor"))
	  (setq no-blinking-cursor t))
         ((member argi '("-dump-file" "-temacs"))  ; Handled in C
          (or argval (pop args))
          (setq argval nil))
	 ;; Push the popped arg back on the list of arguments.
	 (t
          (push argi args)
          (setq done t)))
	;; Was argval set but not used?
	(and argval
	     (error "Option `%s' doesn't allow an argument" argi))))

    ;; Re-attach the --display arg.
    (and display-arg (setq args (append display-arg args)))

    ;; Re-attach the program name to the front of the arg list.
    (and command-line-args
         (setcdr command-line-args args)))

  ;; Re-evaluate predefined variables whose initial value depends on
  ;; the runtime context.
  (let (current-load-list) ; c-r-s may call defvar, and hence LOADHIST_ATTACH
    (setq custom-delayed-init-variables
          ;; Initialize them in the same order they were loaded, in case there
          ;; are dependencies between them.
          (nreverse custom-delayed-init-variables))
    (mapc 'custom-reevaluate-setting custom-delayed-init-variables))

  ;; Warn for invalid user name.
  (when init-file-user
    (if (string-match "[~/:\n]" init-file-user)
        (display-warning 'initialization
                         (format "Invalid user name %s"
                                 init-file-user)
                         :error)
      (if (file-directory-p (expand-file-name
                             ;; We don't support ~USER on MS-Windows
                             ;; and MS-DOS except for the current
                             ;; user, and always load .emacs from
                             ;; the current user's home directory
                             ;; (see below).  So always check "~",
                             ;; even if invoked with "-u USER", or
                             ;; if $USER or $LOGNAME are set to
                             ;; something different.
                             (if (memq system-type '(windows-nt ms-dos))
                                 "~"
                               (concat "~" init-file-user))))
          nil
        (display-warning 'initialization
                         (format "User %s has no home directory"
                                 (if (equal init-file-user "")
                                     (user-real-login-name)
                                   init-file-user))
                         :error))))

  ;; Load the early init file, if found.
  (startup--load-user-init-file
   (lambda ()
     (expand-file-name
      ;; We use an explicit .el extension here to force
      ;; startup--load-user-init-file to set user-init-file to "early-init.el",
      ;; with the .el extension, if the file doesn't exist, not just
      ;; "early-init" without an extension, as it does for ".emacs".
      "early-init.el"
      (file-name-as-directory
       (concat "~" init-file-user "/.emacs.d")))))
  (setq early-init-file user-init-file)

  ;; If any package directory exists, initialize the package system.
  (and user-init-file
       package-enable-at-startup
       (catch 'package-dir-found
	 (let (dirs)
	   (if (boundp 'package-directory-list)
	       (setq dirs package-directory-list)
	     (dolist (f load-path)
	       (and (stringp f)
		    (equal (file-name-nondirectory f) "site-lisp")
		    (push (expand-file-name "elpa" f) dirs))))
	   (push (if (boundp 'package-user-dir)
		     package-user-dir
		   (locate-user-emacs-file "elpa"))
		 dirs)
	   (dolist (dir dirs)
	     (when (file-directory-p dir)
	       (dolist (subdir (directory-files dir))
		 (when (let ((subdir (expand-file-name subdir dir)))
                         (and (file-directory-p subdir)
                              (file-exists-p
                               (expand-file-name
                                (package--description-file subdir)
                                subdir))))
		   (throw 'package-dir-found t)))))))
       (package-activate-all))

  ;; Make sure window system's init file was loaded in loadup.el if
  ;; using a window system.
  ;; Initialize the window-system only after processing the command-line
  ;; args so that -Q can influence this initialization.
  (condition-case error
    (unless noninteractive
      (if (and initial-window-system
	       (not (featurep
		     (intern
		      (concat (symbol-name initial-window-system) "-win")))))
	  (error "Unsupported window system `%s'" initial-window-system))
      ;; Process window-system specific command line parameters.
      (setq command-line-args
            (let ((window-system initial-window-system)) ;Hack attack!
              (handle-args-function command-line-args)))
      ;; Initialize the window system. (Open connection, etc.)
      (let ((window-system initial-window-system)) ;Hack attack!
        (window-system-initialization))
      (put initial-window-system 'window-system-initialized t))
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

  (run-hooks 'before-init-hook)

  ;; Under X, create the X frame and delete the terminal frame.
  (unless (daemonp)
    (if (or noninteractive emacs-basic-display)
	(setq menu-bar-mode nil
	      tool-bar-mode nil
	      no-blinking-cursor t))
    (frame-initialize))

  (when (fboundp 'x-create-frame)
    ;; Set up the tool-bar (even in tty frames, since Emacs might open a
    ;; graphical frame later).
    (unless noninteractive
      (tool-bar-setup)))

  ;; Turn off blinking cursor if so specified in X resources.  This is here
  ;; only because all other settings of no-blinking-cursor are here.
  (unless (or noninteractive
	      emacs-basic-display
	      (and (memq window-system '(x w32 ns))
		   (not (member (x-get-resource "cursorBlink" "CursorBlink")
				'("no" "off" "false" "0")))))
    (setq no-blinking-cursor t))

  (unless noninteractive
    (startup--setup-quote-display)
    (setq internal--text-quoting-flag t))

  ;; Re-evaluate again the predefined variables whose initial value
  ;; depends on the runtime context, in case some of them depend on
  ;; the window-system features.  Example: blink-cursor-mode.
  (let (current-load-list) ; c-r-s may call defvar, and hence LOADHIST_ATTACH
    (mapc 'custom-reevaluate-setting custom-delayed-init-variables)
    (setq custom-delayed-init-variables nil))

  (normal-erase-is-backspace-setup-frame)

  ;; Register default TTY colors for the case the terminal hasn't a
  ;; terminal init file.  We do this regardless of whether the terminal
  ;; supports colors or not and regardless the current display type,
  ;; since users can connect to color-capable terminals and also
  ;; switch color support on or off in mid-session by setting the
  ;; tty-color-mode frame parameter.
  ;; Exception: the `pc' ``window system'' has only 16 fixed colors,
  ;; and they are already set at this point by a suitable method of
  ;; window-system-initialization.
  (or (eq initial-window-system 'pc)
      (tty-register-default-colors))

  (let ((old-scalable-fonts-allowed scalable-fonts-allowed)
	(old-face-ignored-fonts face-ignored-fonts))

    ;; Run the site-start library if it exists.  The point of this file is
    ;; that it is run before .emacs.  There is no point in doing this after
    ;; .emacs; that is useless.
    ;; Note that user-init-file is nil at this point.  Code that might
    ;; be loaded from site-run-file and wants to test if -q was given
    ;; should check init-file-user instead, since that is already set.
    ;; See cus-edit.el for an example.
    (if site-run-file
        ;; Sites should not disable the startup screen.
        ;; Only individuals should disable the startup screen.
        (let ((inhibit-startup-screen inhibit-startup-screen))
	  (load site-run-file t t)))

    ;; Load that user's init file, or the default one, or none.
    (startup--load-user-init-file
     (lambda ()
       (cond
        ((eq system-type 'ms-dos)
         (concat "~" init-file-user "/_emacs"))
        ((not (eq system-type 'windows-nt))
         (concat "~" init-file-user "/.emacs"))
        ;; Else deal with the Windows situation.
        ((directory-files "~" nil "^\\.emacs\\(\\.elc?\\)?$")
         ;; Prefer .emacs on Windows.
         "~/.emacs")
        ((directory-files "~" nil "^_emacs\\(\\.elc?\\)?$")
         ;; Also support _emacs for compatibility, but warn about it.
         (push `(initialization
                 ,(format-message
                   "`_emacs' init file is deprecated, please use `.emacs'"))
               delayed-warnings-list)
         "~/_emacs")
        (t ;; But default to .emacs if _emacs does not exist.
         "~/.emacs")))
     (lambda ()
       (expand-file-name
        "init"
        (file-name-as-directory
         (concat "~" init-file-user "/.emacs.d"))))
     (not inhibit-default-init))

    (when (and deactivate-mark transient-mark-mode)
      (with-current-buffer (window-buffer)
        (deactivate-mark)))

    ;; If the user has a file of abbrevs, read it (unless -batch).
    (when (and (not noninteractive)
               (file-exists-p abbrev-file-name)
               (file-readable-p abbrev-file-name))
      (quietly-read-abbrev-file abbrev-file-name))

    ;; If the abbrevs came entirely from the init file or the
    ;; abbrevs file, they do not need saving.
    (setq abbrevs-changed nil)

    ;; Do this here in case the init file sets mail-host-address.
    (and mail-host-address
	 ;; Check that user-mail-address has not been set by hand.
	 ;; Yes, this is ugly, but slightly less so than leaving
	 ;; user-mail-address uninitialized during init file processing.
	 ;; Perhaps we should make :set-after do something like this?
	 ;; Ie, extend it to also mean (re)initialize-after.  See etc/TODO.
	 (equal user-mail-address
		(let (mail-host-address)
		  (ignore-errors
		    (eval (car (get 'user-mail-address 'standard-value))))))
	 (custom-reevaluate-setting 'user-mail-address))

    ;; If parameter have been changed in the init file which influence
    ;; face realization, clear the face cache so that new faces will
    ;; be realized.
    (unless (and (eq scalable-fonts-allowed old-scalable-fonts-allowed)
		 (eq face-ignored-fonts old-face-ignored-fonts))
      (clear-face-cache)))

  (setq after-init-time (current-time))
  ;; Display any accumulated warnings after all functions in
  ;; `after-init-hook' like `desktop-read' have finalized possible
  ;; changes in the window configuration.
  (run-hooks 'after-init-hook 'delayed-warnings-hook)

  ;; If *scratch* exists and init file didn't change its mode, initialize it.
  (if (get-buffer "*scratch*")
      (with-current-buffer "*scratch*"
	(if (eq major-mode 'fundamental-mode)
	    (funcall initial-major-mode))))

  ;; Load library for our terminal type.
  ;; User init file can set term-file-prefix to nil to prevent this.
  (unless (or noninteractive
              initial-window-system
              (daemonp))
    (tty-run-terminal-initialization (selected-frame) nil t))

  ;; Update the out-of-memory error message based on user's key bindings
  ;; for save-some-buffers.
  (setq memory-signal-data
	(list 'error
	      (substitute-command-keys "Memory exhausted--use \\[save-some-buffers] then exit and restart Emacs")))

  ;; Process the remaining args.
  (command-line-1 (cdr command-line-args))

  ;; This is a problem because, e.g. if emacs.d/gnus.el exists,
  ;; trying to load gnus could load the wrong file.
  ;; OK, it would not matter if .emacs.d were at the end of load-path.
  ;; but for the sake of simplicity, we discourage it full-stop.
  ;; Ref eg https://lists.gnu.org/r/emacs-devel/2012-03/msg00056.html
  ;;
  ;; A bad element could come from user-emacs-file, the command line,
  ;; or EMACSLOADPATH, so we basically always have to check.
  (let (warned)
    (dolist (dir load-path)
      (and (not warned)
	   (stringp dir)
	   (string-equal (file-name-as-directory (expand-file-name dir))
			 (expand-file-name user-emacs-directory))
	   (setq warned t)
	   (display-warning 'initialization
			    (format-message "\
Your `load-path' seems to contain\n\
your `.emacs.d' directory: %s\n\
This is likely to cause problems...\n\
Consider using a subdirectory instead, e.g.: %s"
                                    dir (expand-file-name
                                         "lisp" user-emacs-directory))
                            :warning))))

  ;; If -batch, terminate after processing the command options.
  (if noninteractive (kill-emacs t))

  ;; In daemon mode, start the server to allow clients to connect.
  ;; This is done after loading the user's init file and after
  ;; processing all command line arguments to allow e.g. `server-name'
  ;; to be changed before the server starts.
  (let ((dn (daemonp)))
    (when dn
      (when (stringp dn) (setq server-name dn))
      (server-start)
      (if server-process
	  (daemon-initialized)
	(if (stringp dn)
	    (message
	     "Unable to start daemon: Emacs server named %S already running"
	     server-name)
	  (message "Unable to start the daemon.\nAnother instance of Emacs is running the server, either as daemon or interactively.\nYou can use emacsclient to connect to that Emacs process."))
	(kill-emacs 1))))

  ;; Run emacs-session-restore (session management) if started by
  ;; the session manager and we have a session manager connection.
  (if (and (boundp 'x-session-previous-id)
           (stringp x-session-previous-id))
      (with-no-warnings
	(emacs-session-restore x-session-previous-id))))

(defun x-apply-session-resources ()
  "Apply X resources which specify initial values for Emacs variables.
This is called from a window-system initialization function, such
as `x-initialize-window-system' for X, either at startup (prior
to reading the init file), or afterwards when the user first
opens a graphical frame.

This can set the values of `menu-bar-mode', `tool-bar-mode', and
`no-blinking-cursor', as well as the `cursor' face.  Changed
settings will be marked as \"CHANGED outside of Customize\"."
  (let ((no-vals  '("no" "off" "false" "0"))
	(settings '(("menuBar" "MenuBar" menu-bar-mode nil)
		    ("toolBar" "ToolBar" tool-bar-mode nil)
		    ("scrollBar" "ScrollBar" scroll-bar-mode nil)
		    ("cursorBlink" "CursorBlink" no-blinking-cursor t))))
    (dolist (x settings)
      (if (member (x-get-resource (nth 0 x) (nth 1 x)) no-vals)
	  (set (nth 2 x) (nth 3 x)))))
  (let ((color (x-get-resource "cursorColor" "Foreground")))
    (when color
      (put 'cursor 'theme-face
	   `((changed ((t :background ,color)))))
      (put 'cursor 'face-modified t))))

(defcustom initial-scratch-message (purecopy "\
;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with \\[find-file] and enter text in its buffer.

")
  "Initial documentation displayed in *scratch* buffer at startup.
If this is nil, no message will be displayed."
  :type '(choice (text :tag "Message")
		 (const :tag "none" nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fancy splash screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst fancy-startup-text
  `((:face (variable-pitch font-lock-comment-face)
     "Welcome to "
     :link ("GNU Emacs"
	    ,(lambda (_button) (browse-url "https://www.gnu.org/software/emacs/"))
	    "Browse https://www.gnu.org/software/emacs/")
     ", one component of the "
     :link
     ,(lambda ()
       (if (eq system-type 'gnu/linux)
            `("GNU/Linux"
              ,(lambda (_button) (browse-url "https://www.gnu.org/gnu/linux-and-gnu.html"))
	     "Browse https://www.gnu.org/gnu/linux-and-gnu.html")
          `("GNU" ,(lambda (_button)
		     (browse-url "https://www.gnu.org/gnu/thegnuproject.html"))
	    "Browse https://www.gnu.org/gnu/thegnuproject.html")))
     " operating system.\n\n"
     :face variable-pitch
     :link ("Emacs Tutorial" ,(lambda (_button) (help-with-tutorial)))
     "\tLearn basic keystroke commands"
     ,(lambda ()
       (let* ((en "TUTORIAL")
	      (tut (or (get-language-info current-language-environment
					  'tutorial)
		       en))
	      (title (with-temp-buffer
		       (insert-file-contents
			(expand-file-name tut tutorial-directory)
			;; We used to read only the first 256 bytes of
			;; the tutorial, but that prevents the coding:
			;; setting, if any, in file-local variables
			;; section to be seen by insert-file-contents,
			;; and results in gibberish when the language
			;; environment's preferred encoding is
			;; different from what the file-local variable
			;; says.  One case in point is Hebrew.
			nil)
		       (search-forward ".")
		       (buffer-substring (point-min) (1- (point))))))
	 ;; If there is a specific tutorial for the current language
	 ;; environment and it is not English, append its title.
	 (if (string= en tut)
	     ""
	   (concat " (" title ")"))))
     "\n"
     :link ("Emacs Guided Tour"
	    ,(lambda (_button)
               (browse-url "https://www.gnu.org/software/emacs/tour/"))
	    "Browse https://www.gnu.org/software/emacs/tour/")
     "\tOverview of Emacs features at gnu.org\n"
     :link ("View Emacs Manual" ,(lambda (_button) (info-emacs-manual)))
     "\tView the Emacs manual using Info\n"
     :link ("Absence of Warranty" ,(lambda (_button) (describe-no-warranty)))
     "\tGNU Emacs comes with "
     :face (variable-pitch (:slant oblique))
     "ABSOLUTELY NO WARRANTY\n"
     :face variable-pitch
     :link ("Copying Conditions" ,(lambda (_button) (describe-copying)))
     "\tConditions for redistributing and changing Emacs\n"
     :link ("Ordering Manuals" ,(lambda (_button) (view-order-manuals)))
     "\tPurchasing printed copies of manuals\n"
     "\n"))
  "A list of texts to show in the middle part of splash screens.
Each element in the list should be a list of strings or pairs
`:face FACE', like `fancy-splash-insert' accepts them.")

(defconst fancy-about-text
  `((:face (variable-pitch font-lock-comment-face)
     "This is "
     :link ("GNU Emacs"
	    ,(lambda (_button) (browse-url "https://www.gnu.org/software/emacs/"))
	    "Browse https://www.gnu.org/software/emacs/")
     ", one component of the "
     :link
     ,(lambda ()
       (if (eq system-type 'gnu/linux)
	   `("GNU/Linux"
	     ,(lambda (_button)
                (browse-url "https://www.gnu.org/gnu/linux-and-gnu.html"))
	     "Browse https://www.gnu.org/gnu/linux-and-gnu.html")
	 `("GNU" ,(lambda (_button) (describe-gnu-project))
	   "Display info on the GNU project.")))
     " operating system.\n"
     :face (variable-pitch font-lock-builtin-face)
     "\n"
     ,(lambda () (emacs-version))
     "\n"
     :face (variable-pitch (:height 0.8))
     ,(lambda () emacs-copyright)
     "\n\n"
     :face variable-pitch
     :link ("Authors"
	    ,(lambda (_button)
	      (view-file (expand-file-name "AUTHORS" data-directory))
	      (goto-char (point-min))))
     "\tMany people have contributed code included in GNU Emacs\n"
     :link ("Contributing"
	    ,(lambda (_button) (info "(emacs)Contributing")))
     "\tHow to contribute improvements to Emacs\n"
     "\n"
     :link ("GNU and Freedom" ,(lambda (_button) (describe-gnu-project)))
     "\tWhy we developed GNU Emacs, and the GNU operating system\n"
     :link ("Absence of Warranty" ,(lambda (_button) (describe-no-warranty)))
     "\tGNU Emacs comes with "
     :face (variable-pitch (:slant oblique))
     "ABSOLUTELY NO WARRANTY\n"
     :face variable-pitch
     :link ("Copying Conditions" ,(lambda (_button) (describe-copying)))
     "\tConditions for redistributing and changing Emacs\n"
     :link ("Getting New Versions" ,(lambda (_button) (describe-distribution)))
     "\tHow to obtain the latest version of Emacs\n"
     :link ("Ordering Manuals" ,(lambda (_button) (view-order-manuals)))
     "\tBuying printed manuals from the FSF\n"
     "\n"
     :link ("Emacs Tutorial" ,(lambda (_button) (help-with-tutorial)))
     "\tLearn basic Emacs keystroke commands"
     ,(lambda ()
       (let* ((en "TUTORIAL")
	      (tut (or (get-language-info current-language-environment
					  'tutorial)
		       en))
	      (title (with-temp-buffer
		       (insert-file-contents
			(expand-file-name tut tutorial-directory)
			;; Read the entire file, to make sure any
			;; coding cookies and other local variables
			;; get acted upon.
			nil)
		       (search-forward ".")
		       (buffer-substring (point-min) (1- (point))))))
	 ;; If there is a specific tutorial for the current language
	 ;; environment and it is not English, append its title.
	 (if (string= en tut)
	     ""
	   (concat " (" title ")"))))
     "\n"
     :link ("Emacs Guided Tour"
	    ,(lambda (_button)
               (browse-url "https://www.gnu.org/software/emacs/tour/"))
	    "Browse https://www.gnu.org/software/emacs/tour/")
     "\tSee an overview of Emacs features at gnu.org"))
  "A list of texts to show in the middle part of the About screen.
Each element in the list should be a list of strings or pairs
`:face FACE', like `fancy-splash-insert' accepts them.")


(defgroup fancy-splash-screen ()
  ;; FIXME: Do we really need this group with a single custom var?
  "Fancy splash screen when Emacs starts."
  :version "21.1"
  :group 'initialization)

(defcustom fancy-splash-image nil
  "The image to show in the splash screens, or nil for defaults."
  :type '(choice (const :tag "Default" nil)
		 (file :tag "File")))


(defvar splash-screen-keymap
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (set-keymap-parent map button-buffer-map)
    (define-key map "\C-?" 'scroll-down-command)
    (define-key map [?\S-\ ] 'scroll-down-command)
    (define-key map " " 'scroll-up-command)
    (define-key map "q" 'exit-splash-screen)
    map)
  "Keymap for splash screen buffer.")

;; These are temporary storage areas for the splash screen display.

(defun fancy-splash-insert (&rest args)
  "Insert text into the current buffer, with faces.
Arguments from ARGS should be either strings; functions called
with no args that return a string; pairs `:face FACE', where FACE
is a face specification usable with `put-text-property'; or pairs
`:link LINK' where LINK is a list of arguments to pass to
`insert-button', of the form (LABEL ACTION [HELP-ECHO]), which
specifies the button's label, `action' property and help-echo string.
FACE and LINK can also be functions, which are evaluated to obtain
a face or button specification."
  (let ((current-face nil))
    (while args
      (cond ((eq (car args) :face)
	     (setq args (cdr args) current-face (car args))
	     (if (functionp current-face)
		 (setq current-face (funcall current-face))))
	    ((eq (car args) :link)
	     (setq args (cdr args))
	     (let ((spec (car args)))
	       (if (functionp spec)
		   (setq spec (funcall spec)))
	       (insert-button (car spec)
			      'face (list 'link current-face)
			      'action (cadr spec)
			      'help-echo (concat "mouse-2, RET: "
						 (or (nth 2 spec)
						     "Follow this link"))
			      'follow-link t)))
	    (t (insert (propertize (let ((it (car args)))
				     (if (functionp it)
					 (funcall it)
				       it))
				   'face current-face
				   'help-echo (startup-echo-area-message)))))
      (setq args (cdr args)))))

(declare-function image-size "image.c" (spec &optional pixels frame))

(defun fancy-splash-image-file ()
  (cond ((stringp fancy-splash-image) fancy-splash-image)
	((display-color-p)
	 (cond ((<= (display-planes) 8)
		(if (image-type-available-p 'xpm)
		    "splash.xpm"
		  "splash.pbm"))
	       ((or (image-type-available-p 'svg)
		    (image-type-available-p 'imagemagick))
		"splash.svg")
	       ((image-type-available-p 'png)
		"splash.png")
	       ((image-type-available-p 'xpm)
		"splash.xpm")
	       (t "splash.pbm")))
	(t "splash.pbm")))

(defun fancy-splash-head ()
  "Insert the head part of the splash screen into the current buffer."
  (let* ((image-file (fancy-splash-image-file))
	 (img (create-image image-file))
	 (image-width (and img (car (image-size img))))
	 (window-width (window-width)))
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

	;; Insert the image with a help-echo and a link.
	(make-button (prog1 (point) (insert-image img)) (point)
		     'face 'default
		     'help-echo "mouse-2, RET: Browse https://www.gnu.org/"
		     'action (lambda (_button) (browse-url "https://www.gnu.org/"))
		     'follow-link t)
	(insert "\n\n")))))

(defun fancy-startup-tail (&optional concise)
  "Insert the tail part of the splash screen into the current buffer."
  (unless concise
    (fancy-splash-insert
     :face 'variable-pitch
     "\nTo start...     "
     :link `("Open a File"
	     ,(lambda (_button) (call-interactively 'find-file))
	     "Specify a new file's name, to edit the file")
     "     "
     :link `("Open Home Directory"
	     ,(lambda (_button) (dired "~"))
	     "Open your home directory, to operate on its files")
     "     "
     :link `("Customize Startup"
	     ,(lambda (_button) (customize-group 'initialization))
	     "Change initialization settings including this screen")
     "\n"))
  (fancy-splash-insert
   :face 'variable-pitch "To quit a partially entered command, type "
   :face 'default "Control-g"
   :face 'variable-pitch ".\n")
  (fancy-splash-insert :face '(variable-pitch font-lock-builtin-face)
		       "\nThis is "
		       (emacs-version)
		       "\n"
		       :face '(variable-pitch (:height 0.8))
		       emacs-copyright
		       "\n")
  (when auto-save-list-file-prefix
    (let ((dir  (file-name-directory auto-save-list-file-prefix))
	  (name (file-name-nondirectory auto-save-list-file-prefix))
	  files)
      ;; Don't warn if the directory for auto-save-list files does not
      ;; yet exist.
      (and (file-directory-p dir)
	   (setq files (directory-files dir nil (concat "\\`" name) t))
	   (fancy-splash-insert :face '(variable-pitch font-lock-comment-face)
				(if (= (length files) 1)
				    "\nAn auto-save file list was found.  "
				  "\nAuto-save file lists were found.  ")
				"If an Emacs session crashed recently,\ntype "
				:link `("M-x recover-session RET"
					,(lambda (_button)
					   (call-interactively
					    'recover-session)))
				" to recover the files you were editing."))))

  (when concise
    (fancy-splash-insert
     :face 'variable-pitch "\n"
     :link `("Dismiss this startup screen"
	     ,(lambda (_button)
		(when startup-screen-inhibit-startup-screen
		  (customize-set-variable 'inhibit-startup-screen t)
		  (customize-mark-to-save 'inhibit-startup-screen)
		  (custom-save-all))
		(let ((w (get-buffer-window "*GNU Emacs*")))
		  (and w (not (one-window-p)) (delete-window w)))
		(kill-buffer "*GNU Emacs*")))
     "  ")
    (when (or user-init-file custom-file)
      (let ((checked (create-image "checked.xpm"
				   nil nil :ascent 'center))
	    (unchecked (create-image "unchecked.xpm"
				     nil nil :ascent 'center)))
	(insert-button
	 " "
	 :on-glyph checked
	 :off-glyph unchecked
	 'checked nil 'display unchecked 'follow-link t
	 'action (lambda (button)
		   (if (overlay-get button 'checked)
		       (progn (overlay-put button 'checked nil)
			      (overlay-put button 'display
					   (overlay-get button :off-glyph))
			      (setq startup-screen-inhibit-startup-screen
				    nil))
		     (overlay-put button 'checked t)
		     (overlay-put button 'display
				  (overlay-get button :on-glyph))
		     (setq startup-screen-inhibit-startup-screen t)))))
      (fancy-splash-insert :face '(variable-pitch (:height 0.9))
			   " Never show it again."))))

(defun exit-splash-screen ()
  "Stop displaying the splash screen buffer."
  (interactive)
  (quit-window t))

(defun fancy-startup-screen (&optional concise)
  "Display fancy startup screen.
If CONCISE is non-nil, display a concise version of the
splash screen in another window."
  (let ((splash-buffer (get-buffer-create "*GNU Emacs*")))
    (with-current-buffer splash-buffer
      (let ((inhibit-read-only t))
	(erase-buffer)
	(setq default-directory command-line-default-directory)
	(make-local-variable 'startup-screen-inhibit-startup-screen)
	(if pure-space-overflow
	    (insert pure-space-overflow-message))
	(unless concise
	  (fancy-splash-head))
	(dolist (text fancy-startup-text)
	  (apply #'fancy-splash-insert text)
	  (insert "\n"))
	(skip-chars-backward "\n")
	(delete-region (point) (point-max))
	(insert "\n")
	(fancy-startup-tail concise))
      (use-local-map splash-screen-keymap)
      (setq-local browse-url-browser-function 'eww-browse-url)
      (setq tab-width 22
	    buffer-read-only t)
      (set-buffer-modified-p nil)
      (if (and view-read-only (not view-mode))
	  (view-mode-enter nil 'kill-buffer))
      (goto-char (point-min))
      (forward-line (if concise 2 4)))
    (if concise
	(progn
	  (display-buffer splash-buffer)
	  ;; If the splash screen is in a split window, fit it.
	  (let ((window (get-buffer-window splash-buffer t)))
	    (or (null window)
		(eq window (selected-window))
		(eq window (next-window window))
		(fit-window-to-buffer window))))
      (switch-to-buffer splash-buffer))))

(defun fancy-about-screen ()
  "Display fancy About screen."
  (let ((frame (fancy-splash-frame)))
    (save-selected-window
      (select-frame frame)
      (switch-to-buffer "*About GNU Emacs*")
      (setq buffer-undo-list t)
      (let ((inhibit-read-only t))
	(erase-buffer)
	(if pure-space-overflow
	    (insert pure-space-overflow-message))
	(fancy-splash-head)
	(dolist (text fancy-about-text)
	  (apply #'fancy-splash-insert text)
	  (insert "\n"))
	(set-buffer-modified-p nil)
	(goto-char (point-min))
	(force-mode-line-update))
      (use-local-map splash-screen-keymap)
      (setq-local browse-url-browser-function 'eww-browse-url)
      (setq tab-width 22)
      (setq buffer-read-only t)
      (goto-char (point-min))
      (forward-line 3))))

(defun fancy-splash-frame ()
  "Return the frame to use for the fancy splash screen.
Returning non-nil does not mean we should necessarily
use the fancy splash screen, but if we do use it,
we put it on this frame."
  (let (chosen-frame)
    ;; MS-Windows needs this to have a chance to make the initial
    ;; frame visible.
    (if (eq (window-system) 'w32)
	(sit-for 0 t))
    (dolist (frame (append (frame-list) (list (selected-frame))))
      (if (and (frame-visible-p frame)
	       (not (window-minibuffer-p (frame-selected-window frame))))
	  (setq chosen-frame frame)))
    ;; If there are no visible frames yet, try the selected one.
    (or chosen-frame (selected-frame))))

(defun use-fancy-splash-screens-p ()
  "Return t if fancy splash screens should be used."
  (when (and (display-graphic-p)
             (or (and (display-color-p)
		      (image-type-available-p 'xpm))
                 (image-type-available-p 'pbm)))
    (let ((frame (fancy-splash-frame)))
      (when frame
	(let* ((img (create-image (fancy-splash-image-file)))
	       (image-height (and img (cdr (image-size img nil frame))))
	       ;; We test frame-height and not window-height so that,
	       ;; if the frame is split by displaying a warning, that
	       ;; doesn't cause the normal splash screen to be used.
	       ;; We subtract 2 from frame-height to account for the
	       ;; echo area and the mode line.
	       (frame-height (- (frame-height frame) 2)))
	  (> frame-height (+ image-height 19)))))))


(defun normal-splash-screen (&optional startup concise)
  "Display non-graphic splash screen.
If optional argument STARTUP is non-nil, display the startup screen
after Emacs starts.  If STARTUP is nil, display the About screen.
If CONCISE is non-nil, display a concise version of the
splash screen in another window."
  (let ((splash-buffer (get-buffer-create "*About GNU Emacs*")))
    (with-current-buffer splash-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq default-directory command-line-default-directory)
      (set (make-local-variable 'tab-width) 8)

      (if pure-space-overflow
	  (insert pure-space-overflow-message))

      ;; The convention for this piece of code is that
      ;; each piece of output starts with one or two newlines
      ;; and does not end with any newlines.
      (insert (if startup "Welcome to GNU Emacs" "This is GNU Emacs"))
      (insert
       (if (eq system-type 'gnu/linux)
	   ", one component of the GNU/Linux operating system.\n"
	 ", a part of the GNU operating system.\n"))

      (if startup
	  (if (display-mouse-p)
	      ;; The user can use the mouse to activate menus
	      ;; so give help in terms of menu items.
	      (normal-mouse-startup-screen)

	    ;; No mouse menus, so give help using kbd commands.
	    (normal-no-mouse-startup-screen))

	(normal-about-screen))

      ;; The rest of the startup screen is the same on all
      ;; kinds of terminals.

      ;; Give information on recovering, if there was a crash.
      (and startup
	   auto-save-list-file-prefix
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
		   " the files you were editing.\n"))

      (use-local-map splash-screen-keymap)

      ;; Display the input that we set up in the buffer.
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (if (and view-read-only (not view-mode))
	  (view-mode-enter nil 'kill-buffer))
      (if startup (rename-buffer "*GNU Emacs*" t))
      (goto-char (point-min)))
    (if concise
	(display-buffer splash-buffer)
      (switch-to-buffer splash-buffer))))

(defun normal-mouse-startup-screen ()
  ;; The user can use the mouse to activate menus
  ;; so give help in terms of menu items.
  (insert "\
To follow a link, click Mouse-1 on it, or move to it and type RET.
To quit a partially entered command, type Control-g.\n")

  (insert "\nImportant Help menu items:\n")
  (insert-button "Emacs Tutorial"
		 'action (lambda (_button) (help-with-tutorial))
		 'follow-link t)
  (insert "\t\tLearn basic Emacs keystroke commands\n")
  (insert-button "Read the Emacs Manual"
		 'action (lambda (_button) (info-emacs-manual))
		 'follow-link t)
  (insert "\tView the Emacs manual using Info\n")
  (insert-button "(Non)Warranty"
		 'action (lambda (_button) (describe-no-warranty))
		 'follow-link t)
  (insert "\t\tGNU Emacs comes with ABSOLUTELY NO WARRANTY\n")
  (insert-button "Copying Conditions"
		 'action (lambda (_button) (describe-copying))
		 'follow-link t)
  (insert "\tConditions for redistributing and changing Emacs\n")
  (insert-button "More Manuals / Ordering Manuals"
		 'action (lambda (_button) (view-order-manuals))
		 'follow-link t)
  (insert "  How to order printed manuals from the FSF\n")

  (insert "\nUseful tasks:\n")
  (insert-button "Visit New File"
		 'action (lambda (_button) (call-interactively 'find-file))
		 'follow-link t)
  (insert (substitute-command-keys
	   "\t\tSpecify a new file's name, to edit the file\n"))
  (insert-button "Open Home Directory"
		 'action (lambda (_button) (dired "~"))
		 'follow-link t)
  (insert "\tOpen your home directory, to operate on its files\n")
  (insert-button "Customize Startup"
		 'action (lambda (_button) (customize-group 'initialization))
		 'follow-link t)
  (insert "\tChange initialization settings including this screen\n")

  (insert "\n" (emacs-version)
	  "\n" emacs-copyright))

(defun normal-no-mouse-startup-screen ()
  "Show a splash screen suitable for displays without mouse support."
  (let* ((c-h-accessible
          ;; If normal-erase-is-backspace is used on a tty, there's
          ;; no way to invoke C-h and you have to use F1 instead.
          (or (not (char-table-p keyboard-translate-table))
              (eq (aref keyboard-translate-table ?\C-h) ?\C-h)))
         (minor-mode-overriding-map-alist
          (cons (cons (not c-h-accessible)
                      ;; If C-h can't be invoked, temporarily disable its
                      ;; binding, so where-is uses alternative bindings.
                      (let ((map (make-sparse-keymap)))
                        (define-key map [?\C-h] 'undefined)
                        map))
                minor-mode-overriding-map-alist)))

    (insert (format "\nGet help\t   %s\n"
                    (let ((where (where-is-internal 'help-command nil t)))
                      (cond
                       ((equal where [?\C-h])
                        "C-h  (Hold down CTRL and press h)")
                       (where (key-description where))
                       (t "M-x help")))))
    (insert-button "Emacs manual"
                   'action (lambda (_button) (info-emacs-manual))
                   'follow-link t)
    (insert (substitute-command-keys"\t   \\[info-emacs-manual]\t"))
    (insert-button "Browse manuals"
                   'action (lambda (_button) (Info-directory))
                   'follow-link t)
    (insert (substitute-command-keys "\t   \\[info]\n"))
    (insert-button "Emacs tutorial"
                   'action (lambda (_button) (help-with-tutorial))
                   'follow-link t)
    (insert (substitute-command-keys
             "\t   \\[help-with-tutorial]\tUndo changes\t   \\[undo]\n"))
    (insert-button "Buy manuals"
                   'action (lambda (_button) (view-order-manuals))
                   'follow-link t)
    (insert (substitute-command-keys
             "\t   \\[view-order-manuals]\tExit Emacs\t   \\[save-buffers-kill-terminal]")))

  ;; Say how to use the menu bar with the keyboard.
  (insert "\n")
  (insert-button "Activate menubar"
		 'action (lambda (_button) (tmm-menubar))
		 'follow-link t)
  (if (and (eq (key-binding "\M-`") 'tmm-menubar)
	   (eq (key-binding [f10]) 'tmm-menubar))
      (insert "   F10  or  ESC `  or   M-`")
    (insert (substitute-command-keys "   \\[tmm-menubar]")))

  ;; Many users seem to have problems with these.
  (insert (substitute-command-keys "
\(`C-' means use the CTRL key.  `M-' means use the Meta (or Alt) key.
If you have no Meta key, you may instead type ESC followed by the character.)"))

  ;; Insert links to useful tasks
  (insert "\nUseful tasks:\n")

  (insert-button "Visit New File"
		 'action (lambda (_button) (call-interactively 'find-file))
		 'follow-link t)
  (insert "\t\t\t")
  (insert-button "Open Home Directory"
		 'action (lambda (_button) (dired "~"))
		 'follow-link t)
  (insert "\n")

  (insert-button "Customize Startup"
		 'action (lambda (_button) (customize-group 'initialization))
		 'follow-link t)
  (insert "\t\t")
  (insert-button "Open *scratch* buffer"
		 'action (lambda (_button) (switch-to-buffer
                                       (get-buffer-create "*scratch*")))
		 'follow-link t)
  (insert "\n")
  (insert "\n" (emacs-version) "\n" emacs-copyright "\n")
  (insert (substitute-command-keys
	   "
GNU Emacs comes with ABSOLUTELY NO WARRANTY; type \\[describe-no-warranty] for "))
  (insert-button "full details"
		 'action (lambda (_button) (describe-no-warranty))
		 'follow-link t)
  (insert (substitute-command-keys ".
Emacs is Free Software--Free as in Freedom--so you can redistribute copies
of Emacs and modify it; type \\[describe-copying] to see "))
  (insert-button "the conditions"
		 'action (lambda (_button) (describe-copying))
		 'follow-link t)
  (insert (substitute-command-keys".
Type \\[describe-distribution] for information on "))
  (insert-button "getting the latest version"
		 'action (lambda (_button) (describe-distribution))
		 'follow-link t)
  (insert "."))

(defun normal-about-screen ()
  (insert "\n" (emacs-version) "\n" emacs-copyright "\n\n")

  (insert "To follow a link, click Mouse-1 on it, or move to it and type RET.\n\n")

  (insert-button "Authors"
		 'action
		 (lambda (_button)
		   (view-file (expand-file-name "AUTHORS" data-directory))
		   (goto-char (point-min)))
		 'follow-link t)
  (insert "\t\tMany people have contributed code included in GNU Emacs\n")

  (insert-button "Contributing"
		 'action
		 (lambda (_button) (info "(emacs)Contributing"))
		 'follow-link t)
  (insert "\tHow to contribute improvements to Emacs\n\n")

  (insert-button "GNU and Freedom"
		 'action (lambda (_button) (describe-gnu-project))
		 'follow-link t)
  (insert "\t\tWhy we developed GNU Emacs and the GNU system\n")

  (insert-button "Absence of Warranty"
		 'action (lambda (_button) (describe-no-warranty))
		 'follow-link t)
  (insert "\tGNU Emacs comes with ABSOLUTELY NO WARRANTY\n")

  (insert-button "Copying Conditions"
		 'action (lambda (_button) (describe-copying))
		 'follow-link t)
  (insert "\tConditions for redistributing and changing Emacs\n")

  (insert-button "Getting New Versions"
		 'action (lambda (_button) (describe-distribution))
		 'follow-link t)
  (insert "\tHow to get the latest version of GNU Emacs\n")

  (insert-button "More Manuals / Ordering Manuals"
		 'action (lambda (_button) (view-order-manuals))
		 'follow-link t)
  (insert "\tBuying printed manuals from the FSF\n"))

(defun startup-echo-area-message ()
  (if (daemonp)
      "Starting Emacs daemon."
    (substitute-command-keys
     "For information about GNU Emacs and the GNU system, type \
\\[about-emacs].")))

(defun display-startup-echo-area-message ()
  (let ((resize-mini-windows t))
    (or noninteractive                  ;(input-pending-p) init-file-had-error
	;; t if the init file says to inhibit the echo area startup message.
	(and inhibit-startup-echo-area-message
	     user-init-file
	     (or (and (get 'inhibit-startup-echo-area-message 'saved-value)
		      (equal inhibit-startup-echo-area-message
			     (if (equal init-file-user "")
				 (user-login-name)
			       init-file-user)))
		 ;; Wasn't set with custom; see if .emacs has a setq.
                 (condition-case nil
                     (with-temp-buffer
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
                   (error nil))))
	(message "%s" (startup-echo-area-message)))))

(defun display-startup-screen (&optional concise)
  "Display startup screen according to display.
A fancy display is used on graphic displays, normal otherwise.

If CONCISE is non-nil, display a concise version of the startup
screen."
  ;; Prevent recursive calls from server-process-filter.
  (if (not (get-buffer "*GNU Emacs*"))
      (if (use-fancy-splash-screens-p)
      	  (fancy-startup-screen concise)
      	(normal-splash-screen t concise))))

(defun display-about-screen ()
  "Display the *About GNU Emacs* buffer.
A fancy display is used on graphic displays, normal otherwise."
  (interactive)
  (if (use-fancy-splash-screens-p)
      (fancy-about-screen)
    (normal-splash-screen nil)))

(defalias 'about-emacs 'display-about-screen)
(defalias 'display-splash-screen 'display-startup-screen)

(defun command-line-1 (args-left)
  "A subroutine of `command-line'."
  (display-startup-echo-area-message)
  (when (and pure-space-overflow
	     (not noninteractive))
    (display-warning
     'initialization
     "Building Emacs overflowed pure space.\
  (See the node Pure Storage in the Lisp manual for details.)"
     :warning))

  ;; `displayable-buffers' is a list of buffers that may be displayed,
  ;; which includes files parsed from the command line arguments and
  ;; `initial-buffer-choice'.  All of the display logic happens at the
  ;; end of this `let'.  As files as processed from the command line
  ;; arguments, their buffers are prepended to `displayable-buffers'.
  ;; In order for options like "--eval" to work with the "--file" arg,
  ;; the file buffers are set as the current buffer as they are seen
  ;; on the command line (so "emacs --batch --file a --file b
  ;; --eval='(message "%s" (buffer-name))'" will print "b"), but this
  ;; does not affect the final displayed state of the buffers.
  (let ((displayable-buffers nil))
    ;; This `let' processes the command line arguments.
    (let ((command-line-args-left args-left))
      (when command-line-args-left
        ;; We have command args; process them.
        (let* ((dir command-line-default-directory)
               tem
               ;; This approach loses for "-batch -L DIR --eval "(require foo)",
               ;; if foo is intended to be found in DIR.
               ;;
               ;; The directories listed in --directory/-L options will *appear*
               ;; at the front of `load-path' in the order they appear on the
               ;; command-line.  We cannot do this by *placing* them at the front
               ;; in the order they appear, so we need this variable to hold them,
               ;; temporarily.
               ;;
               ;; To DTRT we keep track of the splice point and modify `load-path'
               ;; straight away upon any --directory/-L option.
               splice
               just-files ;; t if this follows the magic -- option.
               ;; This includes our standard options' long versions
               ;; and long versions of what's on command-switch-alist.
               (longopts
                (append '("--funcall" "--load" "--insert" "--kill"
                          "--directory" "--eval" "--execute" "--no-splash"
                          "--find-file" "--visit" "--file" "--no-desktop")
                        (mapcar (lambda (elt) (concat "-" (car elt)))
                                command-switch-alist)))
               (line 0)
               (column 0)
               ;; `process-file-arg' opens a file buffer for `name',
               ;; sets that buffer as the current buffer without
               ;; displaying it, adds the buffer to
               ;; `displayable-buffers', and puts the point at
               ;; `line':`column'.  `line' and `column' are both reset
               ;; to zero when `process-file-arg' returns.
               (process-file-arg
                (lambda (name)
		  ;; This can only happen if PWD is deleted.
		  (if (not (or dir (file-name-absolute-p name)))
		      (message "Ignoring relative file name (%s) due to \
nil default-directory" name)
		    (let* ((file (expand-file-name
				  (command-line-normalize-file-name name)
				  dir))
			   (buf (find-file-noselect file)))
		      (setq displayable-buffers (cons buf displayable-buffers))
                      ;; Set the file buffer to the current buffer so
                      ;; that it will be used with "--eval" and
                      ;; similar options.
                      (set-buffer buf)
                      ;; Put the point at `line':`column' in the file
                      ;; buffer, and reset `line' and `column' to 0.
                      (unless (zerop line)
                        (goto-char (point-min))
                        (forward-line (1- line)))
                      (setq line 0)
                      (unless (< column 1)
                        (move-to-column (1- column)))
                      (setq column 0))))))

          ;; Add the long X options to longopts.
          (dolist (tem command-line-x-option-alist)
            (if (string-match "^--" (car tem))
                (push (car tem) longopts)))

          ;; Add the long NS options to longopts.
          (dolist (tem command-line-ns-option-alist)
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
                (when (string-match "\\`\\(--[^=]*\\)=" argi)
                  (setq argval (substring argi (match-end 0))
                        argi (match-string 1 argi)))
                (when (string-match "\\`--?[^-]" orig-argi)
                  (setq completion (try-completion argi longopts))
                  (if (eq completion t)
                      (setq argi (substring argi 1))
                    (if (stringp completion)
                        (let ((elt (member completion longopts)))
                          (or elt
                              (error "Option `%s' is ambiguous" argi))
                          (setq argi (substring (car elt) 1)))
                      (setq argval nil
                            argi orig-argi)))))

              ;; Execute the option.
              (cond ((setq tem (assoc argi command-switch-alist))
                     (if argval
                         (let ((command-line-args-left
                                (cons argval command-line-args-left)))
                           (funcall (cdr tem) argi))
                       (funcall (cdr tem) argi)))

                    ((equal argi "-no-splash")
                     (setq inhibit-startup-screen t))

                    ((member argi '("-f"	; what the manual claims
                                    "-funcall"
                                    "-e"))  ; what the source used to say
                     (setq inhibit-startup-screen t)
                     (setq tem (intern (or argval (pop command-line-args-left))))
                     (if (commandp tem)
                         (command-execute tem)
                       (funcall tem)))

                    ((member argi '("-eval" "-execute"))
                     (setq inhibit-startup-screen t)
                     (let* ((str-expr (or argval (pop command-line-args-left)))
                            (read-data (read-from-string str-expr))
                            (expr (car read-data))
                            (end (cdr read-data)))
                       (unless (= end (length str-expr))
                         (error "Trailing garbage following expression: %s"
                                (substring str-expr end)))
                       (eval expr)))

                    ((member argi '("-L" "-directory"))
                     ;; -L :/foo adds /foo to the _end_ of load-path.
                     (let (append)
                       (if (string-match-p
                            (format "\\`%s" path-separator)
                            (setq tem (or argval (pop command-line-args-left))))
                           (setq tem (substring tem 1)
                                 append t))
                       (setq tem (expand-file-name
                                  (command-line-normalize-file-name tem)))
                       (cond (append (setq load-path
                                           (append load-path (list tem)))
                                     (if splice (setq splice load-path)))
                             (splice (setcdr splice (cons tem (cdr splice)))
                                     (setq splice (cdr splice)))
                             (t (setq load-path (cons tem load-path)
                                      splice load-path)))))

                    ((member argi '("-l" "-load"))
                     (let* ((file (command-line-normalize-file-name
                                   (or argval (pop command-line-args-left))))
                            ;; Take file from default dir if it exists there;
                            ;; otherwise let `load' search for it.
                            (file-ex (expand-file-name file)))
                       (when (file-regular-p file-ex)
                         (setq file file-ex))
                       (load file nil t)))

                    ;; This is used to handle -script.  It's not clear
                    ;; we need to document it (it is totally internal).
                    ((member argi '("-scriptload"))
                     (let* ((file (command-line-normalize-file-name
                                   (or argval (pop command-line-args-left))))
                            ;; Take file from default dir.
                            (file-ex (expand-file-name file)))
                       (load file-ex nil t t)))

                    ((equal argi "-insert")
                     (setq inhibit-startup-screen t)
                     (setq tem (or argval (pop command-line-args-left)))
                     (or (stringp tem)
                         (error "File name omitted from `-insert' option"))
                     (insert-file-contents (command-line-normalize-file-name tem)))

                    ((equal argi "-kill")
                     (kill-emacs t))

                    ;; This is for when they use --no-desktop with -q, or
                    ;; don't load Desktop in their .emacs.  If desktop.el
                    ;; _is_ loaded, it will handle this switch, and we
                    ;; won't see it by the time we get here.
                    ((equal argi "-no-desktop")
                     (message "\"--no-desktop\" ignored because the Desktop package is not loaded"))

                    ((string-match "^\\+[0-9]+\\'" argi)
                     (setq line (string-to-number argi)))

                    ((string-match "^\\+\\([0-9]+\\):\\([0-9]+\\)\\'" argi)
                     (setq line (string-to-number (match-string 1 argi))
                           column (string-to-number (match-string 2 argi))))

                    ((setq tem (assoc orig-argi command-line-x-option-alist))
                     ;; Ignore X-windows options and their args if not using X.
                     (setq command-line-args-left
                           (nthcdr (nth 1 tem) command-line-args-left)))

                    ((setq tem (assoc orig-argi command-line-ns-option-alist))
                     ;; Ignore NS-windows options and their args if not using NS.
                     (setq command-line-args-left
                           (nthcdr (nth 1 tem) command-line-args-left)))

                    ((member argi '("-find-file" "-file" "-visit"))
                     (setq inhibit-startup-screen t)
                     ;; An explicit option to specify visiting a file.
                     (setq tem (or argval (pop command-line-args-left)))
                     (unless (stringp tem)
                       (error "File name omitted from `%s' option" argi))
                     (funcall process-file-arg tem))

                    ;; These command lines now have no effect.
                    ((string-match "\\`--?\\(no-\\)?\\(uni\\|multi\\)byte$" argi)
                     (display-warning 'initialization
                                      (format "Ignoring obsolete arg %s" argi)))

                    ((equal argi "--")
                     (setq just-files t))
                    (t
                     ;; We have almost exhausted our options. See if the
                     ;; user has made any other command-line options available
                     (let ((hooks command-line-functions)
                           (did-hook nil))
                       (while (and hooks
                                   (not (setq did-hook (funcall (car hooks)))))
                         (setq hooks (cdr hooks)))
                       (unless did-hook
                         ;; Presume that the argument is a file name.
                         (if (string-match "\\`-" argi)
                             (error "Unknown option `%s'" argi))
                         ;; FIXME: Why do we only inhibit the startup
                         ;; screen for -nw?
                         (unless initial-window-system
                           (setq inhibit-startup-screen t))
                         (funcall process-file-arg orig-argi)))))

              ;; In unusual circumstances, the execution of Lisp code due
              ;; to command-line options can cause the last visible frame
              ;; to be deleted.  In this case, kill emacs to avoid an
              ;; abort later.
              (unless (frame-live-p (selected-frame)) (kill-emacs nil)))))))

    (when (eq initial-buffer-choice t)
      ;; When `initial-buffer-choice' equals t make sure that *scratch*
      ;; exists.
      (get-buffer-create "*scratch*"))

    ;; If *scratch* exists and is empty, insert initial-scratch-message.
    ;; Do this before switching to *scratch* below to handle bug#9605.
    (and initial-scratch-message
	 (get-buffer "*scratch*")
	 (with-current-buffer "*scratch*"
	   (when (zerop (buffer-size))
	     (insert (substitute-command-keys initial-scratch-message))
	     (set-buffer-modified-p nil))))

    ;; Prepend `initial-buffer-choice' to `displayable-buffers'. If
    ;; the buffer is already a member of that list then shift the
    ;; buffer to the head of the list. The shift behavior is intended
    ;; to prevent the same buffer being displayed in two windows when
    ;; an `initial-buffer-choice' function happens to return the head
    ;; of `displayable-buffers'.
    (when initial-buffer-choice
      (let ((buf
             (cond ((stringp initial-buffer-choice)
		    (find-file-noselect initial-buffer-choice))
		   ((functionp initial-buffer-choice)
		    (funcall initial-buffer-choice))
                   ((eq initial-buffer-choice t)
                    (get-buffer-create "*scratch*"))
                   (t
                    (error "`initial-buffer-choice' must be a string, a function, or t")))))
        (unless (buffer-live-p buf)
          (error "Value returned by `initial-buffer-choice' is not a live buffer: %S" buf))
        (setq displayable-buffers (cons buf (delq buf displayable-buffers)))))

    ;; Display the first two buffers in `displayable-buffers'.  If
    ;; `initial-buffer-choice' is non-nil, its buffer will be the
    ;; first buffer in `displayable-buffers'.  The first buffer will
    ;; be focused.
    (let ((displayable-buffers-len (length displayable-buffers))
          ;; `nondisplayed-buffers-p' is true if there exist buffers
          ;; in `displayable-buffers' that were not displayed to the
          ;; user.
          (nondisplayed-buffers-p nil))
      (when (> displayable-buffers-len 0)
        (switch-to-buffer (car displayable-buffers)))
      (when (> displayable-buffers-len 1)
        (switch-to-buffer-other-window (car (cdr displayable-buffers)))
        ;; Focus on the first buffer.
        (other-window -1))
      (when (> displayable-buffers-len 2)
        (setq nondisplayed-buffers-p t))

      (if (or inhibit-startup-screen
              initial-buffer-choice
              noninteractive
              (daemonp)
              inhibit-x-resources)

          ;; Not displaying a startup screen.  Display *Buffer List* if
          ;; there exist buffers that were not displayed.
          (when (and nondisplayed-buffers-p
                     (not noninteractive)
                     (not inhibit-startup-buffer-menu))
            (list-buffers))

        ;; Display a startup screen, after some preparations.

        ;; If there are no switches to process, we might as well
        ;; run this hook now, and there may be some need to do it
        ;; before doing any output.
        (run-hooks 'emacs-startup-hook 'term-setup-hook)

        ;; It's important to notice the user settings before we
        ;; display the startup message; otherwise, the settings
        ;; won't take effect until the user gives the first
        ;; keystroke, and that's distracting.
        (when (fboundp 'frame-notice-user-settings)
          (frame-notice-user-settings))

        ;; If there are no switches to process, we might as well
        ;; run this hook now, and there may be some need to do it
        ;; before doing any output.
        (run-hooks 'window-setup-hook)

        (setq inhibit-startup-hooks t)

        ;; ;; Do this now to avoid an annoying delay if the user
        ;; ;; clicks the menu bar during the sit-for.
        ;; (when (display-popup-menus-p)
        ;; 	(precompute-menubar-bindings))
        ;; (with-no-warnings
        ;; 	(setq menubar-bindings-done t))

        (display-startup-screen (> displayable-buffers-len 0))))))

(defun command-line-normalize-file-name (file)
  "Collapse multiple slashes to one, to handle non-Emacs file names."
  (save-match-data
    ;; Use arg 1 so that we don't collapse // at the start of the file name.
    ;; That is significant on some systems.
    ;; However, /// at the beginning is supposed to mean just /, not //.
    (if (string-match
	 (if (memq system-type '(ms-dos windows-nt))
	     "^\\([\\/][\\/][\\/]\\)+"
	   "^///+")
	 file)
	(setq file (replace-match "/" t t file)))
    (if (memq system-type '(ms-dos windows-nt))
	(while (string-match "\\([\\/][\\/]\\)+" file 1)
	  (setq file (replace-match "/" t t file)))
      (while (string-match "//+" file 1)
	(setq file (replace-match "/" t t file))))
    file))

;;; startup.el ends here
