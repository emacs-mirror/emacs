;;; esh-ext.el --- commands external to Eshell  -*- lexical-binding:t -*-

;; Copyright (C) 1999-2026 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>

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

;; To force a command to invoked external, either provide an explicit
;; pathname for the command argument, or prefix the command name with
;; an asterix character.  Example:
;;
;;   grep        ; make invoke `grep' Lisp function, or `eshell/grep'
;;   /bin/grep   ; will definitely invoke /bin/grep
;;   *grep        ; will also invoke /bin/grep

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'esh-io)
(require 'esh-arg)
(require 'esh-opt)
(require 'esh-proc)
(require 'esh-util)

(defgroup eshell-ext nil
  "External commands are invoked when operating system executables are
loaded into memory, thus beginning a new process."
  :tag "External commands"
  :group 'eshell)

;;; User Variables:

(defcustom eshell-ext-load-hook nil
  "A hook that gets run when `eshell-ext' is loaded."
  :version "24.1"			; removed eshell-ext-initialize
  :type 'hook
  :group 'eshell-ext)

(defcustom eshell-binary-suffixes exec-suffixes
  "A list of suffixes used when searching for executable files."
  :type '(repeat string)
  :group 'eshell-ext)

(defcustom eshell-force-execution
  (not (null (memq system-type '(windows-nt ms-dos))))
  "If non-nil, try to execute files regardless of execute permissions.
This can be useful on systems like Windows, where the operating system
doesn't support the execution bit for shell scripts; or in cases where
you want to associate an interpreter with a particular kind of script
file, but the language won't let you but a `#!' interpreter line in
the file, and you don't want to make it executable since nothing else
but Eshell will be able to understand
`eshell-interpreter-alist'."
  :type 'boolean
  :group 'eshell-ext)

(defun eshell-search-path (name)
  "Search the environment path for NAME."
  (if (file-name-absolute-p name)
      name
    (let ((list (eshell-get-path))
	  suffixes n1 n2 file)
      (while list
	(setq n1 (file-name-concat (car list) name))
	(setq suffixes eshell-binary-suffixes)
	(while suffixes
	  (setq n2 (concat n1 (car suffixes)))
	  (if (and (or (file-executable-p n2)
		       (and eshell-force-execution
			    (file-readable-p n2)))
		   (not (file-directory-p n2)))
	      (setq file n2 suffixes nil list nil))
	  (setq suffixes (cdr suffixes)))
	(setq list (cdr list)))
      file)))

(defcustom eshell-windows-shell-file
  (if (eshell-under-windows-p)
      (if (string-match "\\(cmdproxy\\|sh\\)\\.\\(com\\|exe\\)"
			shell-file-name)
	  (or (eshell-search-path "cmd.exe")
	      (eshell-search-path "command.com"))
	shell-file-name))
  "The name of the shell command to use for DOS/Windows batch files.
This defaults to nil on non-Windows systems, where this variable is
wholly ignored."
  :type '(choice file (const nil))
  :group 'eshell-ext)

(autoload 'eshell-parse-command "esh-cmd")

(defsubst eshell-invoke-batch-file (&rest args)
  "Invoke a .BAT or .CMD file on MS-DOS/MS-Windows systems."
  ;; since CMD.EXE can't handle forward slashes in the initial
  ;; argument...
  (setcar args (subst-char-in-string ?/ ?\\ (car args)))
  (throw 'eshell-replace-command
	 (eshell-parse-command
	  (eshell-quote-argument eshell-windows-shell-file)
	  (cons "/c" args))))

(defcustom eshell-interpreter-alist
  (if (eshell-under-windows-p)
      '(("\\.\\(bat\\|cmd\\)\\'" . eshell-invoke-batch-file)))
  "An alist defining interpreter substitutions.
Each member is a cons cell of the form:

  (MATCH . INTERPRETER)

MATCH should be a regexp, which is matched against the command
name, or a function of arity 2 receiving the COMMAND and its
ARGS (a list).  If either returns a non-nil value, then
INTERPRETER will be used for that command.

If INTERPRETER is a string, it will be called as the command name,
with the original command name passed as the first argument, with all
subsequent arguments following.  If INTERPRETER is a function, it will
be called with all of those arguments.  Note that interpreter
functions should throw `eshell-replace-command' with the alternate
command form, or they should return a value compatible with the
possible return values of `eshell-external-command', which see."
  :type '(repeat (cons (choice regexp (function :tag "Predicate"))
		       (choice string (function :tag "Interpreter"))))
  :group 'eshell-ext)

(defcustom eshell-alternate-command-hook nil
  "A hook run whenever external command lookup fails.
If a functions wishes to provide an alternate command, they must throw
it using the tag `eshell-replace-command'.  This is done because the
substituted command need not be external at all, and therefore must be
passed up to a higher level for re-evaluation.

Or, if the function returns a filename, that filename will be invoked
with the current command arguments rather than the command specified
by the user on the command line."
  :type 'hook
  :group 'eshell-ext)

(defcustom eshell-command-interpreter-max-length 256
  "The maximum length of any command interpreter string, plus args."
  :type 'integer
  :group 'eshell-ext)

(defcustom eshell-explicit-command-char ?*
  "If this char occurs before a command name, call it externally.
That is, although `vi' may be an alias, `*vi' will always call the
external version."
  :type 'character
  :group 'eshell-ext)

(defcustom eshell-explicit-remote-commands t
  "If non-nil, support explicitly-remote commands.
These are commands with a full remote file name, such as
\"/ssh:host:whoami\".  If this is enabled, you can also explicitly run
commands on your local host by using the \"/local:\" prefix, like
\"/local:whoami\"."
  :type 'boolean
  :version "30.1"
  :group 'eshell-ext)

;;; Functions:

(defconst eshell--local-prefix "/local:")

(defun eshell-ext-initialize ()     ;Called from `eshell-mode' via intern-soft!
  "Initialize the external command handling code."
  (add-hook 'eshell-named-command-hook #'eshell-quoted-file-command nil t)
  (add-hook 'eshell-named-command-hook #'eshell-explicit-command nil t))

(defun eshell-explicit-command--which (command)
  (when (and (> (length command) 1)
             (eq (aref command 0) eshell-explicit-command-char))
    (eshell-external-command--which (substring command 1))))

(defun eshell-explicit-command (command args)
  "If a command name begins with \"*\", always call it externally.
This bypasses all Lisp functions and aliases."
  (when (and (> (length command) 1)
	     (eq (aref command 0) eshell-explicit-command-char))
    (let ((cmd (eshell-search-path (substring command 1))))
      (if cmd
	  (or (eshell-external-command cmd args)
	      (error "%s: external command failed" cmd))
	(error "%s: external command not found"
	       (substring command 1))))))

(put 'eshell-explicit-command 'eshell-which-function
     #'eshell-explicit-command--which)

(defun eshell-quoted-file-command--which (command)
  (when (file-name-quoted-p command)
    (eshell-external-command--which (file-name-unquote command))))

(defun eshell-quoted-file-command (command args)
  "If a command name begins with \"/:\", always call it externally.
Similar to `eshell-explicit-command', this bypasses all Lisp functions
and aliases, but it also ignores file name handlers."
  (when (file-name-quoted-p command)
    (eshell-external-command (file-name-unquote command) args)))

(put 'eshell-quoted-file-command 'eshell-which-function
     #'eshell-quoted-file-command--which)

(defun eshell-remote-command (command args)
  "Insert output from a remote COMMAND, using ARGS.
A \"remote\" command in Eshell is something that executes on a different
machine.  If COMMAND is a remote file name, run it on the host for that
file; if COMMAND is a local file name, run it locally."
  (let* ((cwd-connection (file-remote-p default-directory))
         (command-connection (file-remote-p command))
         (default-directory (if (equal cwd-connection command-connection)
                                default-directory
                              (or command-connection (expand-file-name "~"))))
         ;; Never use the remote connection here.  We don't want to
         ;; expand the local name!  Instead, we want it as the user
         ;; typed, so that if COMMAND is "/ssh:host:cat", we just get
         ;; "cat" as the result.
         (command-localname (or (file-remote-p command 'localname 'never)
                                command)))
    (eshell-connection-local-command command-localname args)))

(defun eshell-connection-local-command (command args)
  "Insert output from an external COMMAND, using ARGS.
This always runs COMMAND using the connection associated with the
current working directory."
  (setq args (eshell-stringify-list (flatten-tree args)))
  (let ((interp (eshell-find-interpreter
		 command
		 args
		 ;; `eshell-find-interpreter' does not work correctly
		 ;; for Tramp file name syntax.  But we don't need to
		 ;; know the interpreter in that case, therefore the
		 ;; check is suppressed.
		 (or (and (stringp command) (file-remote-p command))
		     (file-remote-p default-directory)))))
    (cl-assert interp)
    (if (functionp (car interp))
	(apply (car interp) (append (cdr interp) args))
      (eshell-gather-process-output
       (car interp) (append (cdr interp) args)))))

(defun eshell-external-command--which (command)
  (or (eshell-search-path command)
      (error "no %s in (%s)" command
             (string-join (eshell-get-path t) (path-separator)))))

(defun eshell-external-command (command args)
  "Insert output from an external COMMAND, using ARGS."
  (cond
   ((and eshell-explicit-remote-commands
         (file-remote-p command))
    (eshell-remote-command command args))
   ((and eshell-explicit-remote-commands
         (string-prefix-p eshell--local-prefix command))
    (eshell-remote-command
     (substring command (length eshell--local-prefix)) args))
   (t
    (eshell-connection-local-command command args))))

(defun eshell/addpath (&rest args)
  "Add a set of paths to PATH."
  (eshell-eval-using-options
   "addpath" args
   '((?b "begin" nil prepend "add to beginning of $PATH")
     (?h "help" nil nil  "display this usage message")
     :usage "[-b] DIR...
Adds the given DIR to $PATH.")
   (let ((path (eshell-get-path t)))
     (if args
         (progn
           (setq path (if prepend
                          (append args path)
                        (append path args)))
           (eshell-set-path path)
           (string-join path (path-separator)))
       (dolist (dir path)
         (eshell-printn dir))))))

(put 'eshell/addpath 'eshell-no-numeric-conversions t)
(put 'eshell/addpath 'eshell-filename-arguments t)

(defun eshell-script-interpreter (file)
  "Extract the script to run from FILE, if it has #!<interp> in it.
Return nil, or a list of the form:

  (INTERPRETER [ARGS] FILE)"
  (let ((maxlen eshell-command-interpreter-max-length))
    (if (and (file-readable-p file)
	     (file-regular-p file)
             ;; If the file is zero bytes, it can't possibly have a
             ;; shebang.  This check may seem redundant, but we can
             ;; encounter files that Emacs considers both readable and
             ;; regular, but which aren't *actually* readable.  This can
             ;; happen, for example, with certain kinds of reparse
             ;; points like APPEXECLINK on NTFS filesystems (MS-Windows
             ;; uses these for "app execution aliases").  In these
             ;; cases, the file size is 0, so this check protects us
             ;; from errors.
             (> (file-attribute-size (file-attributes file)) 0))
	(with-temp-buffer
	  (insert-file-contents-literally file nil 0 maxlen)
	  (if (looking-at "#![ \t]*\\([^ \r\t\n]+\\)\\([ \t]+\\(.+\\)\\)?")
	      (if (match-string 3)
		  (list (match-string 1)
			(match-string 3)
			file)
		(list (match-string 1)
		      file)))))))

(defun eshell-find-interpreter (file args &optional no-examine-p)
  "Find the command interpreter with which to execute FILE.
If NO-EXAMINE-P is non-nil, FILE will not be inspected for a script
line of the form #!<interp>."
  (let ((finterp
	 (catch 'found
	   (ignore
	    (dolist (possible eshell-interpreter-alist)
	      (cond
	       ((functionp (car possible))
		(let ((fn (car possible)))
		  (and (funcall fn file args)
		       (throw 'found (cdr possible)))))
	       ((stringp (car possible))
		(and (string-match (car possible) file)
		     (throw 'found (cdr possible))))
	       (t
		(error "Invalid interpreter-alist test"))))))))
    (if finterp                         ; first check
	(list finterp file)
      (let ((fullname (if (file-name-directory file) file
			(eshell-search-path file)))
	    (suffixes eshell-binary-suffixes))
	(when (and fullname
                   (not (file-remote-p fullname))
                   (file-remote-p default-directory))
          (setq fullname
                (if (file-name-absolute-p fullname)
                    (concat (file-remote-p default-directory) fullname)
                  (expand-file-name fullname default-directory))))
	(if (and fullname (not (or eshell-force-execution
				   (file-executable-p fullname))))
	    (while suffixes
	      (let ((try (concat fullname (car suffixes))))
		(if (or (file-executable-p try)
			(and eshell-force-execution
			     (file-readable-p try)))
		    (setq fullname try suffixes nil)
		  (setq suffixes (cdr suffixes))))))
	(cond ((not (and fullname (file-exists-p fullname)))
	       (let ((name (or fullname file)))
		 (unless (setq fullname
			       (run-hook-with-args-until-success
				'eshell-alternate-command-hook file))
		   (error "%s: command not found" name))))
	      ((not (or eshell-force-execution
			(file-executable-p fullname)))
	       (error "%s: Permission denied" fullname)))
	(let (interp)
	  (unless no-examine-p
	    (setq interp (eshell-script-interpreter fullname))
	    (if interp
		(setq interp
		      (cons (car (eshell-find-interpreter (car interp) args t))
			    (cdr interp)))))
	  (or interp (list fullname)))))))

(provide 'esh-ext)
;;; esh-ext.el ends here
