;;; files.el --- file input and output commands for Emacs

;; Copyright (C) 1985, 86, 87, 92, 93, 94, 95, 96, 97, 98, 99, 2000, 2001, 2002
;;;   Free Software Foundation, Inc.

;; Maintainer: FSF

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

;; Defines most of Emacs's file- and directory-handling functions,
;; including basic file visiting, backup generation, link handling,
;; ITS-id version control, load- and write-hook handling, and the like.

;;; Code:

(defgroup backup nil
  "Backups of edited data files."
  :group 'files)

(defgroup find-file nil
  "Finding files."
  :group 'files)


(defcustom delete-auto-save-files t
  "*Non-nil means delete auto-save file when a buffer is saved or killed.

Note that auto-save file will not be deleted if the buffer is killed
when it has unsaved changes."
  :type 'boolean
  :group 'auto-save)

(defcustom directory-abbrev-alist
  nil
  "*Alist of abbreviations for file directories.
A list of elements of the form (FROM . TO), each meaning to replace
FROM with TO when it appears in a directory name.  This replacement is
done when setting up the default directory of a newly visited file.
*Every* FROM string should start with `^'.

Do not use `~' in the TO strings.
They should be ordinary absolute directory names.

Use this feature when you have directories which you normally refer to
via absolute symbolic links.  Make TO the name of the link, and FROM
the name it is linked to."
  :type '(repeat (cons :format "%v"
		       :value ("" . "")
		       (regexp :tag "From")
		       (regexp :tag "To")))
  :group 'abbrev
  :group 'find-file)

;; Turn off backup files on VMS since it has version numbers.
(defcustom make-backup-files (not (eq system-type 'vax-vms))
  "*Non-nil means make a backup of a file the first time it is saved.
This can be done by renaming the file or by copying.

Renaming means that Emacs renames the existing file so that it is a
backup file, then writes the buffer into a new file.  Any other names
that the old file had will now refer to the backup file.  The new file
is owned by you and its group is defaulted.

Copying means that Emacs copies the existing file into the backup
file, then writes the buffer on top of the existing file.  Any other
names that the old file had will now refer to the new (edited) file.
The file's owner and group are unchanged.

The choice of renaming or copying is controlled by the variables
`backup-by-copying', `backup-by-copying-when-linked',
`backup-by-copying-when-mismatch' and
`backup-by-copying-when-privileged-mismatch'.  See also `backup-inhibited'."
  :type 'boolean
  :group 'backup)

;; Do this so that local variables based on the file name
;; are not overridden by the major mode.
(defvar backup-inhibited nil
  "Non-nil means don't make a backup, regardless of the other parameters.
This variable is intended for use by making it local to a buffer.
But it is local only if you make it local.")
(put 'backup-inhibited 'permanent-local t)

(defcustom backup-by-copying nil
 "*Non-nil means always use copying to create backup files.
See documentation of variable `make-backup-files'."
 :type 'boolean
 :group 'backup)

(defcustom backup-by-copying-when-linked nil
 "*Non-nil means use copying to create backups for files with multiple names.
This causes the alternate names to refer to the latest version as edited.
This variable is relevant only if `backup-by-copying' is nil."
 :type 'boolean
 :group 'backup)

(defcustom backup-by-copying-when-mismatch nil
  "*Non-nil means create backups by copying if this preserves owner or group.
Renaming may still be used (subject to control of other variables)
when it would not result in changing the owner or group of the file;
that is, for files which are owned by you and whose group matches
the default for a new file created there by you.
This variable is relevant only if `backup-by-copying' is nil."
  :type 'boolean
  :group 'backup)

(defcustom backup-by-copying-when-privileged-mismatch 200
  "*Non-nil means create backups by copying to preserve a privileged owner.
Renaming may still be used (subject to control of other variables)
when it would not result in changing the owner of the file or if the owner
has a user id greater than the value of this variable.  This is useful
when low-numbered uid's are used for special system users (such as root)
that must maintain ownership of certain files.
This variable is relevant only if `backup-by-copying' and
`backup-by-copying-when-mismatch' are nil."
  :type '(choice (const nil) integer)
  :group 'backup)

(defvar backup-enable-predicate 'normal-backup-enable-predicate
  "Predicate that looks at a file name and decides whether to make backups.
Called with an absolute file name as argument, it returns t to enable backup.")

(defcustom buffer-offer-save nil
  "*Non-nil in a buffer means always offer to save buffer on exit.
Do so even if the buffer is not visiting a file.
Automatically local in all buffers."
  :type 'boolean
  :group 'backup)
(make-variable-buffer-local 'buffer-offer-save)

(defcustom find-file-existing-other-name t
  "*Non-nil means find a file under alternative names, in existing buffers.
This means if any existing buffer is visiting the file you want
under another name, you get the existing buffer instead of a new buffer."
  :type 'boolean
  :group 'find-file)

(defcustom find-file-visit-truename nil
  "*Non-nil means visit a file under its truename.
The truename of a file is found by chasing all links
both at the file level and at the levels of the containing directories."
  :type 'boolean
  :group 'find-file)

(defcustom revert-without-query
  nil
  "*Specify which files should be reverted without query.
The value is a list of regular expressions.
If the file name matches one of these regular expressions,
then `revert-buffer' reverts the file without querying
if the file has changed on disk and you have not edited the buffer."
  :type '(repeat regexp)
  :group 'find-file)

(defvar buffer-file-number nil
  "The device number and file number of the file visited in the current buffer.
The value is a list of the form (FILENUM DEVNUM).
This pair of numbers uniquely identifies the file.
If the buffer is visiting a new file, the value is nil.")
(make-variable-buffer-local 'buffer-file-number)
(put 'buffer-file-number 'permanent-local t)

(defvar buffer-file-numbers-unique (not (memq system-type '(windows-nt)))
  "Non-nil means that buffer-file-number uniquely identifies files.")

(defvar buffer-file-read-only nil
  "Non-nil if visited file was read-only when visited.")
(make-variable-buffer-local 'buffer-file-read-only)

(defcustom temporary-file-directory
  (file-name-as-directory
   (cond ((memq system-type '(ms-dos windows-nt))
	  (or (getenv "TEMP") (getenv "TMPDIR") (getenv "TMP") "c:/temp"))
	 ((memq system-type '(vax-vms axp-vms))
	  (or (getenv "TMPDIR") (getenv "TMP") (getenv "TEMP") "SYS$SCRATCH:"))
	 (t
	  (or (getenv "TMPDIR") (getenv "TMP") (getenv "TEMP") "/tmp"))))
  "The directory for writing temporary files."
  :group 'files
  :type 'directory)

(defcustom small-temporary-file-directory
  (if (eq system-type 'ms-dos) (getenv "TMPDIR"))
  "The directory for writing small temporary files.
If non-nil, this directory is used instead of `temporary-file-directory'
by programs that create small temporary files.  This is for systems that
have fast storage with limited space, such as a RAM disk."
  :group 'files
  :type 'directory)

;; The system null device. (Should reference NULL_DEVICE from C.)
(defvar null-device "/dev/null" "The system null device.")

(defvar file-name-invalid-regexp
  (cond ((and (eq system-type 'ms-dos) (not (msdos-long-file-names)))
	 (concat "^\\([^A-Z[-`a-z]\\|..+\\)?:\\|" ; colon except after drive
		 "[+, ;=|<>\"?*]\\|\\[\\|\\]\\|"  ; invalid characters
		 "[\000-\031]\\|"		  ; control characters
		 "\\(/\\.\\.?[^/]\\)\\|"	  ; leading dots
		 "\\(/[^/.]+\\.[^/.]*\\.\\)"))	  ; more than a single dot
	((memq system-type '(ms-dos windows-nt))
	 (concat "^\\([^A-Z[-`a-z]\\|..+\\)?:\\|" ; colon except after drive
		 "[|<>\"?*\000-\031]"))		  ; invalid characters
	(t "[\000]"))
  "Regexp recognizing file names which aren't allowed by the filesystem.")

(defcustom file-precious-flag nil
  "*Non-nil means protect against I/O errors while saving files.
Some modes set this non-nil in particular buffers.

This feature works by writing the new contents into a temporary file
and then renaming the temporary file to replace the original.
In this way, any I/O error in writing leaves the original untouched,
and there is never any instant where the file is nonexistent.

Note that this feature forces backups to be made by copying.
Yet, at the same time, saving a precious file
breaks any hard links between it and other files."
  :type 'boolean
  :group 'backup)

(defcustom version-control nil
  "*Control use of version numbers for backup files.
t means make numeric backup versions unconditionally.
nil means make them for files that have some already.
`never' means do not make them."
  :type '(choice (const :tag "Never" never)
		 (const :tag "If existing" nil)
		 (other :tag "Always" t))
  :group 'backup
  :group 'vc)

(defcustom dired-kept-versions 2
  "*When cleaning directory, number of versions to keep."
  :type 'integer
  :group 'backup
  :group 'dired)

(defcustom delete-old-versions nil
  "*If t, delete excess backup versions silently.
If nil, ask confirmation.  Any other value prevents any trimming."
  :type '(choice (const :tag "Delete" t)
		 (const :tag "Ask" nil)
		 (other :tag "Leave" other))
  :group 'backup)

(defcustom kept-old-versions 2
  "*Number of oldest versions to keep when a new numbered backup is made."
  :type 'integer
  :group 'backup)

(defcustom kept-new-versions 2
  "*Number of newest versions to keep when a new numbered backup is made.
Includes the new backup.  Must be > 0"
  :type 'integer
  :group 'backup)

(defcustom require-final-newline nil
  "*Value of t says silently ensure a file ends in a newline when it is saved.
Non-nil but not t says ask user whether to add a newline when there isn't one.
nil means don't add newlines."
  :type '(choice (const :tag "Off" nil)
		 (const :tag "Add" t)
		 (other :tag "Ask" ask))
  :group 'editing-basics)

(defcustom auto-save-default t
  "*Non-nil says by default do auto-saving of every file-visiting buffer."
  :type 'boolean
  :group 'auto-save)

(defcustom auto-save-visited-file-name nil
  "*Non-nil says auto-save a buffer in the file it is visiting, when practical.
Normally auto-save files are written under other names."
  :type 'boolean
  :group 'auto-save)

(defcustom auto-save-file-name-transforms
  `(("\\`/[^/]*:\\(.+/\\)*\\(.*\\)"
     ;; Don't put "\\2" inside expand-file-name, since it will be
     ;; transformed to "/2" on DOS/Windows.
     ,(concat temporary-file-directory "\\2")))
  "*Transforms to apply to buffer file name before making auto-save file name.
Each transform is a list (REGEXP REPLACEMENT):
REGEXP is a regular expression to match against the file name.
If it matches, `replace-match' is used to replace the
matching part with REPLACEMENT.
All the transforms in the list are tried, in the order they are listed.
When one transform applies, its result is final;
no further transforms are tried.

The default value is set up to put the auto-save file into the
temporary directory (see the variable `temporary-file-directory') for
editing a remote file."
  :group 'auto-save
  :type '(repeat (list (string :tag "Regexp") (string :tag "Replacement")))
  :version "21.1")

(defcustom save-abbrevs t
  "*Non-nil means save word abbrevs too when files are saved.
If `silently', don't ask the user before saving."
  :type '(choice (const t) (const nil) (const silently))
  :group 'abbrev)

(defcustom find-file-run-dired t
  "*Non-nil means allow `find-file' to visit directories.
To visit the directory, `find-file' runs `find-directory-functions'."
  :type 'boolean
  :group 'find-file)

(defcustom find-directory-functions '(cvs-dired-noselect dired-noselect)
  "*List of functions to try in sequence to visit a directory.
Each function is called with the directory name as the sole argument
and should return either a buffer or nil."
  :type '(hook :options (cvs-dired-noselect dired-noselect))
  :group 'find-file)

;;;It is not useful to make this a local variable.
;;;(put 'find-file-not-found-hooks 'permanent-local t)
(defvar find-file-not-found-hooks nil
  "List of functions to be called for `find-file' on nonexistent file.
These functions are called as soon as the error is detected.
Variable `buffer-file-name' is already set up.
The functions are called in the order given until one of them returns non-nil.")

;;;It is not useful to make this a local variable.
;;;(put 'find-file-hooks 'permanent-local t)
(defvar find-file-hooks nil
  "List of functions to be called after a buffer is loaded from a file.
The buffer's local variables (if any) will have been processed before the
functions are called.")

(defvar write-file-hooks nil
  "List of functions to be called before writing out a buffer to a file.
If one of them returns non-nil, the file is considered already written
and the rest are not called.
These hooks are considered to pertain to the visited file.
So any buffer-local binding of `write-file-hooks' is
discarded if you change the visited file name with \\[set-visited-file-name].

Don't make this variable buffer-local; instead, use `local-write-file-hooks'.
See also `write-contents-hooks'.")
;;; However, in case someone does make it local...
(put 'write-file-hooks 'permanent-local t)

(defvar local-write-file-hooks nil
  "Just like `write-file-hooks', except intended for per-buffer use.
The functions in this list are called before the ones in
`write-file-hooks'.

This variable is meant to be used for hooks that have to do with a
particular visited file.  Therefore, it is a permanent local, so that
changing the major mode does not clear it.  However, calling
`set-visited-file-name' does clear it.")
(make-variable-buffer-local 'local-write-file-hooks)
(put 'local-write-file-hooks 'permanent-local t)

(defvar write-contents-hooks nil
  "List of functions to be called before writing out a buffer to a file.
If one of them returns non-nil, the file is considered already written
and the rest are not called.

This variable is meant to be used for hooks that pertain to the
buffer's contents, not to the particular visited file; thus,
`set-visited-file-name' does not clear this variable; but changing the
major mode does clear it.

This variable automatically becomes buffer-local whenever it is set.
If you use `add-hook' to add elements to the list, use nil for the
LOCAL argument.

See also `write-file-hooks'.")
(make-variable-buffer-local 'write-contents-hooks)

(defcustom enable-local-variables t
  "*Control use of local variables in files you visit.
The value can be t, nil or something else.
A value of t means file local variables specifications are obeyed;
nil means they are ignored; anything else means query.
This variable also controls use of major modes specified in
a -*- line.

The command \\[normal-mode], when used interactively,
always obeys file local variable specifications and the -*- line,
and ignores this variable."
  :type '(choice (const :tag "Obey" t)
		 (const :tag "Ignore" nil)
		 (other :tag "Query" other))
  :group 'find-file)

(defvar local-enable-local-variables t
  "Like `enable-local-variables' but meant for buffer-local bindings.
The meaningful values are nil and non-nil.  The default is non-nil.
If a major mode sets this to nil, buffer-locally, then any local
variables list in the file will be ignored.

This variable does not affect the use of major modes
specified in a -*- line.")

(defcustom enable-local-eval 'maybe
  "*Control processing of the \"variable\" `eval' in a file's local variables.
The value can be t, nil or something else.
A value of t means obey `eval' variables;
nil means ignore them; anything else means query.

The command \\[normal-mode] always obeys local-variables lists
and ignores this variable."
  :type '(choice (const :tag "Obey" t)
		 (const :tag "Ignore" nil)
		 (other :tag "Query" other))
  :group 'find-file)

;; Avoid losing in versions where CLASH_DETECTION is disabled.
(or (fboundp 'lock-buffer)
    (defalias 'lock-buffer 'ignore))
(or (fboundp 'unlock-buffer)
    (defalias 'unlock-buffer 'ignore))
(or (fboundp 'file-locked-p)
    (defalias 'file-locked-p 'ignore))

(defvar view-read-only nil
  "*Non-nil means buffers visiting files read-only, do it in view mode.")

(defun ange-ftp-completion-hook-function (op &rest args)
  "Provides support for ange-ftp host name completion.
Runs the usual ange-ftp hook, but only for completion operations."
  ;; Having this here avoids the need to load ange-ftp when it's not
  ;; really in use.
  (if (memq op '(file-name-completion file-name-all-completions))
      (apply 'ange-ftp-hook-function op args)
    (let ((inhibit-file-name-handlers
	   (cons 'ange-ftp-completion-hook-function
		 (and (eq inhibit-file-name-operation op)
		      inhibit-file-name-handlers)))
	  (inhibit-file-name-operation op))
      (apply op args))))

(defun convert-standard-filename (filename)
  "Convert a standard file's name to something suitable for the current OS.
This function's standard definition is trivial; it just returns the argument.
However, on some systems, the function is redefined with a definition
that really does change some file names to canonicalize certain
patterns and to guarantee valid names."
  filename)

(defun pwd ()
  "Show the current default directory."
  (interactive nil)
  (message "Directory %s" default-directory))

(defvar cd-path nil
  "Value of the CDPATH environment variable, as a list.
Not actually set up until the first time you use it.")

(defun parse-colon-path (cd-path)
  "Explode a colon-separated search path into a list of directory names.
\(For values of `colon' equal to `path-separator'.)"
  ;; We could use split-string here.
  (and cd-path
       (let (cd-prefix cd-list (cd-start 0) cd-colon)
	 (setq cd-path (concat cd-path path-separator))
	 (while (setq cd-colon (string-match path-separator cd-path cd-start))
	   (setq cd-list
		 (nconc cd-list
			(list (if (= cd-start cd-colon)
				   nil
				(substitute-in-file-name
				 (file-name-as-directory
				  (substring cd-path cd-start cd-colon)))))))
	   (setq cd-start (+ cd-colon 1)))
	 cd-list)))

(defun cd-absolute (dir)
  "Change current directory to given absolute file name DIR."
  ;; Put the name into directory syntax now,
  ;; because otherwise expand-file-name may give some bad results.
  (if (not (eq system-type 'vax-vms))
      (setq dir (file-name-as-directory dir)))
  (setq dir (abbreviate-file-name (expand-file-name dir)))
  (if (not (file-directory-p dir))
      (if (file-exists-p dir)
	  (error "%s is not a directory" dir)
	(error "%s: no such directory" dir))
    (if (file-executable-p dir)
	(setq default-directory dir)
      (error "Cannot cd to %s:  Permission denied" dir))))

(defun cd (dir)
  "Make DIR become the current buffer's default directory.
If your environment includes a `CDPATH' variable, try each one of that
colon-separated list of directories when resolving a relative directory name."
  (interactive
   (list (read-file-name "Change default directory: "
			 default-directory default-directory
			 (and (member cd-path '(nil ("./")))
			      (null (getenv "CDPATH"))))))
  (if (file-name-absolute-p dir)
      (cd-absolute (expand-file-name dir))
    (if (null cd-path)
	(let ((trypath (parse-colon-path (getenv "CDPATH"))))
	  (setq cd-path (or trypath (list "./")))))
    (if (not (catch 'found
	       (mapcar
		(function (lambda (x)
			    (let ((f (expand-file-name (concat x dir))))
			      (if (file-directory-p f)
				  (progn
				    (cd-absolute f)
				    (throw 'found t))))))
		cd-path)
	       nil))
	(error "No such directory found via CDPATH environment variable"))))

(defun load-file (file)
  "Load the Lisp file named FILE."
  ;; This is a case where .elc makes a lot of sense.
  (interactive (list (let ((completion-ignored-extensions
			    (remove ".elc" completion-ignored-extensions)))
		       (read-file-name "Load file: "))))
  (load (expand-file-name file) nil nil t))

(defun load-completion (string predicate action)
  (if (file-name-absolute-p string)
      (read-file-name-internal string predicate action)
    (let ((names nil)
	  (suffix (concat (regexp-opt load-suffixes t) "\\'"))
	  (string-dir (file-name-directory string)))
      (dolist (dir load-path)
	(if string-dir (setq dir (expand-file-name string-dir dir)))
	(when (file-directory-p dir)
	  (dolist (file (file-name-all-completions
			 (file-name-nondirectory string) dir))
	    (push (if string-dir (concat string-dir file) file) names)
	    (when (string-match suffix file)
	      (setq file (substring file 0 (match-beginning 0)))
	      (push (if string-dir (concat string-dir file) file) names)))))
      (if action
	  (all-completions string (mapcar 'list names) predicate)
	(try-completion string (mapcar 'list names) predicate)))))

(defun load-library (library)
  "Load the library named LIBRARY.
This is an interface to the function `load'."
  (interactive (list (completing-read "Load library: " 'load-completion)))
  (load library))

(defun file-local-copy (file)
  "Copy the file FILE into a temporary file on this machine.
Returns the name of the local copy, or nil, if FILE is directly
accessible."
  ;; This formerly had an optional BUFFER argument that wasn't used by
  ;; anything.
  (let ((handler (find-file-name-handler file 'file-local-copy)))
    (if handler
	(funcall handler 'file-local-copy file)
      nil)))

(defun file-truename (filename &optional counter prev-dirs)
  "Return the truename of FILENAME, which should be absolute.
The truename of a file name is found by chasing symbolic links
both at the level of the file and at the level of the directories
containing it, until no links are left at any level.

The arguments COUNTER and PREV-DIRS are used only in recursive calls.
Do not specify them in other calls."
  ;; COUNTER can be a cons cell whose car is the count of how many more links
  ;; to chase before getting an error.
  ;; PREV-DIRS can be a cons cell whose car is an alist
  ;; of truenames we've just recently computed.

  ;; The last test looks dubious, maybe `+' is meant here?  --simon.
  (if (or (string= filename "") (string= filename "~")
	  (and (string= (substring filename 0 1) "~")
	       (string-match "~[^/]*" filename)))
      (progn
	(setq filename (expand-file-name filename))
	(if (string= filename "")
	    (setq filename "/"))))
  (or counter (setq counter (list 100)))
  (let (done
	;; For speed, remove the ange-ftp completion handler from the list.
	;; We know it's not needed here.
	;; For even more speed, do this only on the outermost call.
	(file-name-handler-alist
	 (if prev-dirs file-name-handler-alist
	   (let ((tem (copy-sequence file-name-handler-alist)))
	     (delq (rassq 'ange-ftp-completion-hook-function tem) tem)))))
    (or prev-dirs (setq prev-dirs (list nil)))

    ;; andrewi@harlequin.co.uk - none of the following code (except for
    ;; invoking the file-name handler) currently applies on Windows
    ;; (ie. there are no native symlinks), but there is an issue with
    ;; case differences being ignored by the OS, and short "8.3 DOS"
    ;; name aliases existing for all files.  (The short names are not
    ;; reported by directory-files, but can be used to refer to files.)
    ;; It seems appropriate for file-truename to resolve these issues in
    ;; the most natural way, which on Windows is to call the function
    ;; `w32-long-file-name' - this returns the exact name of a file as
    ;; it is stored on disk (expanding short name aliases with the full
    ;; name in the process).
    (if (eq system-type 'windows-nt)
      (let ((handler (find-file-name-handler filename 'file-truename))
	    newname)
	;; For file name that has a special handler, call handler.
	;; This is so that ange-ftp can save time by doing a no-op.
	(if handler
	    (setq filename (funcall handler 'file-truename filename))
	  ;; If filename contains a wildcard, newname will be the old name.
	  (if (string-match "[[*?]" filename)
	      (setq newname filename)
	    ;; If filename doesn't exist, newname will be nil.
	    (setq newname (w32-long-file-name filename)))
	  (setq filename (or newname filename)))
	(setq done t)))

    ;; If this file directly leads to a link, process that iteratively
    ;; so that we don't use lots of stack.
    (while (not done)
      (setcar counter (1- (car counter)))
      (if (< (car counter) 0)
	  (error "Apparent cycle of symbolic links for %s" filename))
      (let ((handler (find-file-name-handler filename 'file-truename)))
	;; For file name that has a special handler, call handler.
	;; This is so that ange-ftp can save time by doing a no-op.
	(if handler
	    (setq filename (funcall handler 'file-truename filename)
		  done t)
	  (let ((dir (or (file-name-directory filename) default-directory))
		target dirfile)
	    ;; Get the truename of the directory.
	    (setq dirfile (directory-file-name dir))
	    ;; If these are equal, we have the (or a) root directory.
	    (or (string= dir dirfile)
		;; If this is the same dir we last got the truename for,
		;; save time--don't recalculate.
		(if (assoc dir (car prev-dirs))
		    (setq dir (cdr (assoc dir (car prev-dirs))))
		  (let ((old dir)
			(new (file-name-as-directory (file-truename dirfile counter prev-dirs))))
		    (setcar prev-dirs (cons (cons old new) (car prev-dirs)))
		    (setq dir new))))
	    (if (equal ".." (file-name-nondirectory filename))
		(setq filename
		      (directory-file-name (file-name-directory (directory-file-name dir)))
		      done t)
	      (if (equal "." (file-name-nondirectory filename))
		  (setq filename (directory-file-name dir)
			done t)
		;; Put it back on the file name.
		(setq filename (concat dir (file-name-nondirectory filename)))
		;; Is the file name the name of a link?
		(setq target (file-symlink-p filename))
		(if target
		    ;; Yes => chase that link, then start all over
		    ;; since the link may point to a directory name that uses links.
		    ;; We can't safely use expand-file-name here
		    ;; since target might look like foo/../bar where foo
		    ;; is itself a link.  Instead, we handle . and .. above.
		    (setq filename
			  (if (file-name-absolute-p target)
			      target
			    (concat dir target))
			  done nil)
		  ;; No, we are done!
		  (setq done t))))))))
    filename))

(defun file-chase-links (filename)
  "Chase links in FILENAME until a name that is not a link.
Does not examine containing directories for links,
unlike `file-truename'."
  (let (tem (count 100) (newname filename))
    (while (setq tem (file-symlink-p newname))
      (save-match-data
	(if (= count 0)
	    (error "Apparent cycle of symbolic links for %s" filename))
	;; In the context of a link, `//' doesn't mean what Emacs thinks.
	(while (string-match "//+" tem)
	  (setq tem (replace-match "/" nil nil tem)))
	;; Handle `..' by hand, since it needs to work in the
	;; target of any directory symlink.
	;; This code is not quite complete; it does not handle
	;; embedded .. in some cases such as ./../foo and foo/bar/../../../lose.
	(while (string-match "\\`\\.\\./" tem)
	  (setq tem (substring tem 3))
	  (setq newname (expand-file-name newname))
	  ;; Chase links in the default dir of the symlink.
	  (setq newname
		(file-chase-links
		 (directory-file-name (file-name-directory newname))))
	  ;; Now find the parent of that dir.
	  (setq newname (file-name-directory newname)))
	(setq newname (expand-file-name tem (file-name-directory newname)))
	(setq count (1- count))))
    newname))

(defun switch-to-buffer-other-window (buffer &optional norecord)
  "Select buffer BUFFER in another window.
Optional second arg NORECORD non-nil means
do not put this buffer at the front of the list of recently selected ones.

This uses the function `display-buffer' as a subroutine; see its
documentation for additional customization information."
  (interactive "BSwitch to buffer in other window: ")
  (let ((pop-up-windows t))
    (pop-to-buffer buffer t norecord)))

(defun switch-to-buffer-other-frame (buffer &optional norecord)
  "Switch to buffer BUFFER in another frame.
Optional second arg NORECORD non-nil means
do not put this buffer at the front of the list of recently selected ones.

This uses the function `display-buffer' as a subroutine; see its
documentation for additional customization information."
  (interactive "BSwitch to buffer in other frame: ")
  (let ((pop-up-frames t))
    (pop-to-buffer buffer t norecord)
    (raise-frame (window-frame (selected-window)))))

(defun find-file (filename &optional wildcards)
  "Edit file FILENAME.
Switch to a buffer visiting file FILENAME,
creating one if none already exists.
Interactively, or if WILDCARDS is non-nil in a call from Lisp,
expand wildcards (if any) and visit multiple files.  Wildcard expansion
can be suppressed by setting `find-file-wildcards'."
  (interactive "FFind file: \np")
  (let ((value (find-file-noselect filename nil nil wildcards)))
    (if (listp value)
	(mapcar 'switch-to-buffer (nreverse value))
      (switch-to-buffer value))))

(defun find-file-other-window (filename &optional wildcards)
  "Edit file FILENAME, in another window.
May create a new window, or reuse an existing one.
See the function `display-buffer'.
Interactively, or if WILDCARDS is non-nil in a call from Lisp,
expand wildcards (if any) and visit multiple files."
  (interactive "FFind file in other window: \np")
  (let ((value (find-file-noselect filename nil nil wildcards)))
    (if (listp value)
	(progn
	  (setq value (nreverse value))
	  (switch-to-buffer-other-window (car value))
	  (mapcar 'switch-to-buffer (cdr value)))
      (switch-to-buffer-other-window value))))

(defun find-file-other-frame (filename &optional wildcards)
  "Edit file FILENAME, in another frame.
May create a new frame, or reuse an existing one.
See the function `display-buffer'.
Interactively, or if WILDCARDS is non-nil in a call from Lisp,
expand wildcards (if any) and visit multiple files."
  (interactive "FFind file in other frame: \np")
  (let ((value (find-file-noselect filename nil nil wildcards)))
    (if (listp value)
	(progn
	  (setq value (nreverse value))
	  (switch-to-buffer-other-frame (car value))
	  (mapcar 'switch-to-buffer (cdr value)))
      (switch-to-buffer-other-frame value))))

(defun find-file-read-only (filename &optional wildcards)
  "Edit file FILENAME but don't allow changes.
Like `find-file' but marks buffer as read-only.
Use \\[toggle-read-only] to permit editing."
  (interactive "fFind file read-only: \np")
  (find-file filename wildcards)
  (toggle-read-only 1)
  (current-buffer))

(defun find-file-read-only-other-window (filename &optional wildcards)
  "Edit file FILENAME in another window but don't allow changes.
Like \\[find-file-other-window] but marks buffer as read-only.
Use \\[toggle-read-only] to permit editing."
  (interactive "fFind file read-only other window: \np")
  (find-file-other-window filename wildcards)
  (toggle-read-only 1)
  (current-buffer))

(defun find-file-read-only-other-frame (filename &optional wildcards)
  "Edit file FILENAME in another frame but don't allow changes.
Like \\[find-file-other-frame] but marks buffer as read-only.
Use \\[toggle-read-only] to permit editing."
  (interactive "fFind file read-only other frame: \np")
  (find-file-other-frame filename wildcards)
  (toggle-read-only 1)
  (current-buffer))

(defun find-alternate-file-other-window (filename)
  "Find file FILENAME as a replacement for the file in the next window.
This command does not select that window."
  (interactive
   (save-selected-window
     (other-window 1)
     (let ((file buffer-file-name)
	   (file-name nil)
	   (file-dir nil))
       (and file
	    (setq file-name (file-name-nondirectory file)
		  file-dir (file-name-directory file)))
       (list (read-file-name
	      "Find alternate file: " file-dir nil nil file-name)))))
  (if (one-window-p)
      (find-file-other-window filename)
    (save-selected-window
      (other-window 1)
      (find-alternate-file filename))))

(defun find-alternate-file (filename)
  "Find file FILENAME, select its buffer, kill previous buffer.
If the current buffer now contains an empty file that you just visited
\(presumably by mistake), use this command to visit the file you really want."
  (interactive
   (let ((file buffer-file-name)
	 (file-name nil)
	 (file-dir nil))
     (and file
	  (setq file-name (file-name-nondirectory file)
		file-dir (file-name-directory file)))
     (list (read-file-name
	    "Find alternate file: " file-dir nil nil file-name))))
  (and (buffer-modified-p) (buffer-file-name)
       ;; (not buffer-read-only)
       (not (yes-or-no-p (format "Buffer %s is modified; kill anyway? "
				 (buffer-name))))
       (error "Aborted"))
  (let ((obuf (current-buffer))
	(ofile buffer-file-name)
	(onum buffer-file-number)
	(otrue buffer-file-truename)
	(oname (buffer-name)))
    (if (get-buffer " **lose**")
	(kill-buffer " **lose**"))
    (rename-buffer " **lose**")
    (unwind-protect
	(progn
	  (unlock-buffer)
	  (setq buffer-file-name nil)
	  (setq buffer-file-number nil)
	  (setq buffer-file-truename nil)
	  (find-file filename))
      (cond ((eq obuf (current-buffer))
	     (setq buffer-file-name ofile)
	     (setq buffer-file-number onum)
	     (setq buffer-file-truename otrue)
	     (lock-buffer)
	     (rename-buffer oname))))
    (or (eq (current-buffer) obuf)
	(kill-buffer obuf))))

(defun create-file-buffer (filename)
  "Create a suitably named buffer for visiting FILENAME, and return it.
FILENAME (sans directory) is used unchanged if that name is free;
otherwise a string <2> or <3> or ... is appended to get an unused name."
  (let ((lastname (file-name-nondirectory filename)))
    (if (string= lastname "")
	(setq lastname filename))
    (generate-new-buffer lastname)))

(defun generate-new-buffer (name)
  "Create and return a buffer with a name based on NAME.
Choose the buffer's name using `generate-new-buffer-name'."
  (get-buffer-create (generate-new-buffer-name name)))

(defcustom automount-dir-prefix "^/tmp_mnt/"
  "Regexp to match the automounter prefix in a directory name."
  :group 'files
  :type 'regexp)

(defvar abbreviated-home-dir nil
  "The user's homedir abbreviated according to `directory-abbrev-alist'.")

(defun abbreviate-file-name (filename)
  "Return a version of FILENAME shortened using `directory-abbrev-alist'.
This also substitutes \"~\" for the user's home directory.
Type \\[describe-variable] directory-abbrev-alist RET for more information."
  ;; Get rid of the prefixes added by the automounter.
  (if (and automount-dir-prefix
	   (string-match automount-dir-prefix filename)
	   (file-exists-p (file-name-directory
			   (substring filename (1- (match-end 0))))))
      (setq filename (substring filename (1- (match-end 0)))))
  (let ((tail directory-abbrev-alist))
    ;; If any elt of directory-abbrev-alist matches this name,
    ;; abbreviate accordingly.
    (while tail
      (if (string-match (car (car tail)) filename)
	  (setq filename
		(concat (cdr (car tail)) (substring filename (match-end 0)))))
      (setq tail (cdr tail)))
    ;; Compute and save the abbreviated homedir name.
    ;; We defer computing this until the first time it's needed, to
    ;; give time for directory-abbrev-alist to be set properly.
    ;; We include a slash at the end, to avoid spurious matches
    ;; such as `/usr/foobar' when the home dir is `/usr/foo'.
    (or abbreviated-home-dir
	(setq abbreviated-home-dir
	      (let ((abbreviated-home-dir "$foo"))
		(concat "^" (abbreviate-file-name (expand-file-name "~"))
			"\\(/\\|$\\)"))))

    ;; If FILENAME starts with the abbreviated homedir,
    ;; make it start with `~' instead.
    (if (and (string-match abbreviated-home-dir filename)
	     ;; If the home dir is just /, don't change it.
	     (not (and (= (match-end 0) 1)
		       (= (aref filename 0) ?/)))
	     ;; MS-DOS root directories can come with a drive letter;
	     ;; Novell Netware allows drive letters beyond `Z:'.
	     (not (and (or (eq system-type 'ms-dos)
			   (eq system-type 'windows-nt))
		       (save-match-data
			 (string-match "^[a-zA-`]:/$" filename)))))
	(setq filename
	      (concat "~"
		      (substring filename (match-beginning 1) (match-end 1))
		      (substring filename (match-end 0)))))
    filename))

(defcustom find-file-not-true-dirname-list nil
  "*List of logical names for which visiting shouldn't save the true dirname.
On VMS, when you visit a file using a logical name that searches a path,
you may or may not want the visited file name to record the specific
directory where the file was found.  If you *do not* want that, add the logical
name to this list as a string."
  :type '(repeat (string :tag "Name"))
  :group 'find-file)

(defun find-buffer-visiting (filename)
  "Return the buffer visiting file FILENAME (a string).
This is like `get-file-buffer', except that it checks for any buffer
visiting the same file, possibly under a different name.
If there is no such live buffer, return nil."
  (let ((buf (get-file-buffer filename))
	(truename (abbreviate-file-name (file-truename filename))))
    (or buf
	(let ((list (buffer-list)) found)
	  (while (and (not found) list)
	    (save-excursion
	      (set-buffer (car list))
	      (if (and buffer-file-name
		       (string= buffer-file-truename truename))
		  (setq found (car list))))
	    (setq list (cdr list)))
	  found)
	(let* ((attributes (file-attributes truename))
	       (number (nthcdr 10 attributes))
	       (list (buffer-list)) found)
	  (and buffer-file-numbers-unique
	       number
	       (while (and (not found) list)
		 (with-current-buffer (car list)
		   (if (and buffer-file-name
			    (equal buffer-file-number number)
			    ;; Verify this buffer's file number
			    ;; still belongs to its file.
			    (file-exists-p buffer-file-name)
			    (equal (file-attributes buffer-file-name)
				   attributes))
		       (setq found (car list))))
		 (setq list (cdr list))))
	  found))))

(defcustom find-file-wildcards t
  "*Non-nil means file-visiting commands should handle wildcards.
For example, if you specify `*.c', that would visit all the files
whose names match the pattern."
  :group 'files
  :version "20.4"
  :type 'boolean)

(defcustom find-file-suppress-same-file-warnings nil
  "*Non-nil means suppress warning messages for symlinked files.
When nil, Emacs prints a warning when visiting a file that is already
visited, but with a different name.  Setting this option to t
suppresses this warning."
  :group 'files
  :version "21.1"
  :type 'boolean)

(defun find-file-noselect (filename &optional nowarn rawfile wildcards)
  "Read file FILENAME into a buffer and return the buffer.
If a buffer exists visiting FILENAME, return that one, but
verify that the file has not changed since visited or saved.
The buffer is not selected, just returned to the caller.
Optional first arg NOWARN non-nil means suppress any warning messages.
Optional second arg RAWFILE non-nil means the file is read literally.
Optional third arg WILDCARDS non-nil means do wildcard processing
and visit all the matching files.  When wildcards are actually
used and expanded, the value is a list of buffers
that are visiting the various files."
  (setq filename
	(abbreviate-file-name
	 (expand-file-name filename)))
  (if (file-directory-p filename)
      (or (and find-file-run-dired
	       (run-hook-with-args-until-success
		'find-directory-functions
		(if find-file-visit-truename
		    (abbreviate-file-name (file-truename filename))
		  filename)))
	  (error "%s is a directory" filename))
    (if (and wildcards
	     find-file-wildcards
	     (not (string-match "\\`/:" filename))
	     (string-match "[[*?]" filename))
	(let ((files (condition-case nil
			 (file-expand-wildcards filename t)
		       (error (list filename))))
	      (find-file-wildcards nil))
	  (if (null files)
	      (find-file-noselect filename)
	    (mapcar #'find-file-noselect files)))
      (let* ((buf (get-file-buffer filename))
	     (truename (abbreviate-file-name (file-truename filename)))
	     (number (nthcdr 10 (file-attributes truename)))
	     ;; Find any buffer for a file which has same truename.
	     (other (and (not buf) (find-buffer-visiting filename))))
	;; Let user know if there is a buffer with the same truename.
	(if other
	    (progn
	      (or nowarn
		  find-file-suppress-same-file-warnings
		  (string-equal filename (buffer-file-name other))
		  (message "%s and %s are the same file"
			   filename (buffer-file-name other)))
	      ;; Optionally also find that buffer.
	      (if (or find-file-existing-other-name find-file-visit-truename)
		  (setq buf other))))
	(if buf
	    ;; We are using an existing buffer.
	    (progn
	      (or nowarn
		  (verify-visited-file-modtime buf)
		  (cond ((not (file-exists-p filename))
			 (error "File %s no longer exists!" filename))
			;; Certain files should be reverted automatically
			;; if they have changed on disk and not in the buffer.
			((and (not (buffer-modified-p buf))
			      (let ((tail revert-without-query)
				    (found nil))
				(while tail
				  (if (string-match (car tail) filename)
				      (setq found t))
				  (setq tail (cdr tail)))
				found))
			 (with-current-buffer buf
			   (message "Reverting file %s..." filename)
			   (revert-buffer t t)
			   (message "Reverting file %s...done" filename)))
			((yes-or-no-p
			  (if (string= (file-name-nondirectory filename)
				       (buffer-name buf))
			      (format
			       (if (buffer-modified-p buf)
				   "File %s changed on disk.  Discard your edits? "
				 "File %s changed on disk.  Reread from disk? ")
			       (file-name-nondirectory filename))
			    (format
			     (if (buffer-modified-p buf)
				 "File %s changed on disk.  Discard your edits in %s? "
			       "File %s changed on disk.  Reread from disk into %s? ")
			     (file-name-nondirectory filename)
			     (buffer-name buf))))
			 (with-current-buffer buf
			   (revert-buffer t t)))))
	      (with-current-buffer buf

		;; Check if a formerly read-only file has become
		;; writable and vice versa, but if the buffer agrees
		;; with the new state of the file, that is ok too.
		(let ((read-only (not (file-writable-p buffer-file-name))))
		  (unless (or (eq read-only buffer-file-read-only)
			      (eq read-only buffer-read-only))
		    (when (or nowarn
			      (let ((question 
				     (format "File %s is %s on disk.  Change buffer mode? "
					     buffer-file-name
					     (if read-only "read-only" "writable"))))
				(y-or-n-p question)))
		      (setq buffer-read-only read-only)))
		  (setq buffer-file-read-only read-only))

		(when (and (not (eq (not (null rawfile))
				    (not (null find-file-literally))))
			   ;; It is confusing to ask whether to visit
			   ;; non-literally if they have the file in
			   ;; hexl-mode.
			   (not (eq major-mode 'hexl-mode)))
		  (if (buffer-modified-p)
		      (if (y-or-n-p (if rawfile
					"Save file and revisit literally? "
				      "Save file and revisit non-literally? "))
			  (progn
			    (save-buffer)
			    (find-file-noselect-1 buf filename nowarn
						  rawfile truename number))
			(if (y-or-n-p (if rawfile
					  "Discard your edits and revisit file literally? "
					"Discard your edits and revisit file non-literally? "))
			    (find-file-noselect-1 buf filename nowarn
						  rawfile truename number)
			  (error (if rawfile "File already visited non-literally"
				   "File already visited literally"))))
		    (if (y-or-n-p (if rawfile
				      "Revisit file literally? "
				    "Revisit file non-literally? "))
			(find-file-noselect-1 buf filename nowarn
					      rawfile truename number)
		      (error (if rawfile "File already visited non-literally"
			       "File already visited literally"))))))
	      ;; Return the buffer we are using.
	      buf)
	  ;; Create a new buffer.
	  (setq buf (create-file-buffer filename))
	  (set-buffer-major-mode buf)
	  ;; find-file-noselect-1 may use a different buffer.
	  (find-file-noselect-1 buf filename nowarn
				rawfile truename number))))))

(defun find-file-noselect-1 (buf filename nowarn rawfile truename number)
  (let ((inhibit-read-only t)
	error)
    (with-current-buffer buf
      (kill-local-variable 'find-file-literally)
      ;; Needed in case we are re-visiting the file with a different
      ;; text representation.
      (kill-local-variable 'buffer-file-coding-system)
      (erase-buffer)
      (and (default-value 'enable-multibyte-characters)
	   (not rawfile)
	   (set-buffer-multibyte t))
      (if rawfile
	  (condition-case ()
	      (insert-file-contents-literally filename t)
	    (file-error
	     (when (and (file-exists-p filename)
			(not (file-readable-p filename)))
	       (kill-buffer buf)
	       (signal 'file-error (list "File is not readable"
					 filename)))
	     ;; Unconditionally set error
	     (setq error t)))
	(condition-case ()
	    (insert-file-contents filename t)
	  (file-error
	   (when (and (file-exists-p filename)
		      (not (file-readable-p filename)))
	     (kill-buffer buf)
	     (signal 'file-error (list "File is not readable"
				       filename)))
	   ;; Run find-file-not-found-hooks until one returns non-nil.
	   (or (run-hook-with-args-until-success 'find-file-not-found-hooks)
	       ;; If they fail too, set error.
	       (setq error t)))))
      ;; Record the file's truename, and maybe use that as visited name.
      (if (equal filename buffer-file-name)
	  (setq buffer-file-truename truename)
	(setq buffer-file-truename
	      (abbreviate-file-name (file-truename buffer-file-name))))
      (setq buffer-file-number number)
      ;; On VMS, we may want to remember which directory in a search list
      ;; the file was found in.
      (and (eq system-type 'vax-vms)
	   (let (logical)
	     (if (string-match ":" (file-name-directory filename))
		 (setq logical (substring (file-name-directory filename)
					  0 (match-beginning 0))))
	     (not (member logical find-file-not-true-dirname-list)))
	   (setq buffer-file-name buffer-file-truename))
      (if find-file-visit-truename
	  (setq buffer-file-name
		(setq filename
		      (expand-file-name buffer-file-truename))))
      ;; Set buffer's default directory to that of the file.
      (setq default-directory (file-name-directory buffer-file-name))
      ;; Turn off backup files for certain file names.  Since
      ;; this is a permanent local, the major mode won't eliminate it.
      (and (not (funcall backup-enable-predicate buffer-file-name))
	   (progn
	     (make-local-variable 'backup-inhibited)
	     (setq backup-inhibited t)))
      (if rawfile
	  (progn
	    (set-buffer-multibyte nil)
	    (setq buffer-file-coding-system 'no-conversion)
	    (make-local-variable 'find-file-literally)
	    (setq find-file-literally t))
	(after-find-file error (not nowarn)))
      (current-buffer))))

(defun insert-file-contents-literally (filename &optional visit beg end replace)
  "Like `insert-file-contents', but only reads in the file literally.
A buffer may be modified in several ways after reading into the buffer,
to Emacs features such as format decoding, character code
conversion, `find-file-hooks', automatic uncompression, etc.

This function ensures that none of these modifications will take place."
  (let ((format-alist nil)
	(after-insert-file-functions nil)
	(coding-system-for-read 'no-conversion)
	(coding-system-for-write 'no-conversion)
	(find-buffer-file-type-function
	 (if (fboundp 'find-buffer-file-type)
	     (symbol-function 'find-buffer-file-type)
	   nil))
	(inhibit-file-name-handlers '(jka-compr-handler image-file-handler))
	(inhibit-file-name-operation 'insert-file-contents))
    (unwind-protect
	(progn
	  (fset 'find-buffer-file-type (lambda (filename) t))
	  (insert-file-contents filename visit beg end replace))
      (if find-buffer-file-type-function
	  (fset 'find-buffer-file-type find-buffer-file-type-function)
	(fmakunbound 'find-buffer-file-type)))))

(defun insert-file-literally (filename)
  "Insert contents of file FILENAME into buffer after point with no conversion.

This function is meant for the user to run interactively.
Don't call it from programs!  Use `insert-file-contents-literally' instead.
\(Its calling sequence is different; see its documentation)."
  (interactive "*fInsert file literally: ")
  (if (file-directory-p filename)
      (signal 'file-error (list "Opening input file" "file is a directory"
				filename)))
  (let ((tem (insert-file-contents-literally filename)))
    (push-mark (+ (point) (car (cdr tem))))))

(defvar find-file-literally nil
  "Non-nil if this buffer was made by `find-file-literally' or equivalent.
This is a permanent local.")
(put 'find-file-literally 'permanent-local t)

(defun find-file-literally (filename)
  "Visit file FILENAME with no conversion of any kind.
Format conversion and character code conversion are both disabled,
and multibyte characters are disabled in the resulting buffer.
The major mode used is Fundamental mode regardless of the file name,
and local variable specifications in the file are ignored.
Automatic uncompression and adding a newline at the end of the
file due to `require-final-newline' is also disabled.

You cannot absolutely rely on this function to result in
visiting the file literally.  If Emacs already has a buffer
which is visiting the file, you get the existing buffer,
regardless of whether it was created literally or not.

In a Lisp program, if you want to be sure of accessing a file's
contents literally, you should create a temporary buffer and then read
the file contents into it using `insert-file-contents-literally'."
  (interactive "FFind file literally: ")
  (switch-to-buffer (find-file-noselect filename nil t)))

(defvar after-find-file-from-revert-buffer nil)

(defun after-find-file (&optional error warn noauto
				  after-find-file-from-revert-buffer
				  nomodes)
  "Called after finding a file and by the default revert function.
Sets buffer mode, parses local variables.
Optional args ERROR, WARN, and NOAUTO: ERROR non-nil means there was an
error in reading the file.  WARN non-nil means warn if there
exists an auto-save file more recent than the visited file.
NOAUTO means don't mess with auto-save mode.
Fourth arg AFTER-FIND-FILE-FROM-REVERT-BUFFER non-nil
 means this call was from `revert-buffer'.
Fifth arg NOMODES non-nil means don't alter the file's modes.
Finishes by calling the functions in `find-file-hooks'
unless NOMODES is non-nil."
  (setq buffer-read-only (not (file-writable-p buffer-file-name)))
  (if noninteractive
      nil
    (let* (not-serious
	   (msg
	    (cond
	     ((not warn) nil)
	     ((and error (file-attributes buffer-file-name))
	      (setq buffer-read-only t)
	      "File exists, but cannot be read")
	     ((not buffer-read-only)
	      (if (and warn
		       ;; No need to warn if buffer is auto-saved
		       ;; under the name of the visited file.
		       (not (and buffer-file-name
				 auto-save-visited-file-name))
		       (file-newer-than-file-p (or buffer-auto-save-file-name
						   (make-auto-save-file-name))
					       buffer-file-name))
		  (format "%s has auto save data; consider M-x recover-file"
			  (file-name-nondirectory buffer-file-name))
		(setq not-serious t)
		(if error "(New file)" nil)))
	     ((not error)
	      (setq not-serious t)
	      "Note: file is write protected")
	     ((file-attributes (directory-file-name default-directory))
	      "File not found and directory write-protected")
	     ((file-exists-p (file-name-directory buffer-file-name))
	      (setq buffer-read-only nil))
	     (t
	      (setq buffer-read-only nil)
	      (if (file-exists-p (file-name-directory (directory-file-name (file-name-directory buffer-file-name))))
		  "Use M-x make-directory RET RET to create the directory"
		"Use C-u M-x make-directory RET RET to create directory and its parents")))))
      (when msg
	(message msg)
	(or not-serious (sit-for 1 nil t))))
    (when (and auto-save-default (not noauto))
      (auto-save-mode t)))
  ;; Make people do a little extra work (C-x C-q)
  ;; before altering a backup file.
  (when (backup-file-name-p buffer-file-name)
    (setq buffer-read-only t))
  (unless nomodes
    (when (and view-read-only view-mode)
      (view-mode-disable))
    (normal-mode t)
    (when (and buffer-read-only
	       view-read-only
	       (not (eq (get major-mode 'mode-class) 'special)))
      (view-mode-enter))
    (run-hooks 'find-file-hooks)))

(defun normal-mode (&optional find-file)
  "Choose the major mode for this buffer automatically.
Also sets up any specified local variables of the file.
Uses the visited file name, the -*- line, and the local variables spec.

This function is called automatically from `find-file'.  In that case,
we may set up the file-specified mode and local variables,
depending on the value of `enable-local-variables': if it is t, we do;
if it is nil, we don't; otherwise, we query.
In addition, if `local-enable-local-variables' is nil, we do
not set local variables (though we do notice a mode specified with -*-.)

`enable-local-variables' is ignored if you run `normal-mode' interactively,
or from Lisp without specifying the optional argument FIND-FILE;
in that case, this function acts as if `enable-local-variables' were t."
  (interactive)
  (or find-file (funcall (or default-major-mode 'fundamental-mode)))
  (condition-case err
      (set-auto-mode)
    (error (message "File mode specification error: %s"
		    (prin1-to-string err))))
  (condition-case err
      (let ((enable-local-variables (or (not find-file)
					enable-local-variables)))
	(hack-local-variables))
    (error (message "File local-variables error: %s"
		    (prin1-to-string err)))))

(defvar auto-mode-alist
  (mapc
   (lambda (elt)
     (cons (purecopy (car elt)) (cdr elt)))
   '(("\\.te?xt\\'" . text-mode)
     ("\\.c\\'" . c-mode)
     ("\\.h\\'" . c-mode)
     ("\\.tex\\'" . tex-mode)
     ("\\.ltx\\'" . latex-mode)
     ("\\.el\\'" . emacs-lisp-mode)
     ("\\.scm\\'" . scheme-mode)
     ("\\.l\\'" . lisp-mode)
     ("\\.lisp\\'" . lisp-mode)
     ("\\.f\\'" . fortran-mode)
     ("\\.F\\'" . fortran-mode)
     ("\\.for\\'" . fortran-mode)
     ("\\.p\\'" . pascal-mode)
     ("\\.pas\\'" . pascal-mode)
     ("\\.ad[abs]\\'" . ada-mode)
     ("\\.\\([pP]\\([Llm]\\|erl\\)\\|al\\)\\'" . perl-mode)
     ("\\.s?html?\\'" . html-mode)
     ("\\.cc\\'" . c++-mode)
     ("\\.hh\\'" . c++-mode)
     ("\\.hpp\\'" . c++-mode)
     ("\\.C\\'" . c++-mode)
     ("\\.H\\'" . c++-mode)
     ("\\.cpp\\'" . c++-mode)
     ("\\.cxx\\'" . c++-mode)
     ("\\.hxx\\'" . c++-mode)
     ("\\.c\\+\\+\\'" . c++-mode)
     ("\\.h\\+\\+\\'" . c++-mode)
     ("\\.m\\'" . objc-mode)
     ("\\.java\\'" . java-mode)
     ("\\.mk\\'" . makefile-mode)
     ("\\(M\\|m\\|GNUm\\)akefile\\(\\.in\\)?\\'" . makefile-mode)
     ("\\.am\\'" . makefile-mode)	;For Automake.
     ;; Less common extensions come here
     ;; so more common ones above are found faster.
     ("\\.texinfo\\'" . texinfo-mode)
     ("\\.te?xi\\'" . texinfo-mode)
     ("\\.s\\'" . asm-mode)
     ("\\.S\\'" . asm-mode)
     ("\\.asm\\'" . asm-mode)
     ("ChangeLog\\'" . change-log-mode)
     ("change\\.log\\'" . change-log-mode)
     ("changelo\\'" . change-log-mode)
     ("ChangeLog\\.[0-9]+\\'" . change-log-mode)
     ;; for MSDOS and MS-Windows (which are case-insensitive)
     ("changelog\\'" . change-log-mode)
     ("changelog\\.[0-9]+\\'" . change-log-mode)
     ("\\$CHANGE_LOG\\$\\.TXT" . change-log-mode)
     ("\\.scm\\.[0-9]*\\'" . scheme-mode)
     ("\\.[ck]?sh\\'\\|\\.shar\\'\\|/\\.z?profile\\'" . sh-mode)
     ("\\(/\\|\\`\\)\\.\\(bash_profile\\|z?login\\|bash_login\\|z?logout\\)\\'" . sh-mode)
     ("\\(/\\|\\`\\)\\.\\(bash_logout\\|shrc\\|[kz]shrc\\|bashrc\\|t?cshrc\\|esrc\\)\\'" . sh-mode)
     ("\\(/\\|\\`\\)\\.\\([kz]shenv\\|xinitrc\\|startxrc\\|xsession\\)\\'" . sh-mode)
     ("\\.m?spec\\'" . sh-mode)
     ("\\.mm\\'" . nroff-mode)
     ("\\.me\\'" . nroff-mode)
     ("\\.ms\\'" . nroff-mode)
     ("\\.man\\'" . nroff-mode)
     ("\\.\\(u?lpc\\|pike\\|pmod\\)\\'" . pike-mode)
     ("\\.TeX\\'" . tex-mode)
     ("\\.sty\\'" . latex-mode)
     ("\\.cls\\'" . latex-mode)		;LaTeX 2e class
     ("\\.clo\\'" . latex-mode)		;LaTeX 2e class option
     ("\\.bbl\\'" . latex-mode)
     ("\\.bib\\'" . bibtex-mode)
     ("\\.sql\\'" . sql-mode)
     ("\\.m4\\'" . m4-mode)
     ("\\.mc\\'" . m4-mode)
     ("\\.mf\\'" . metafont-mode)
     ("\\.mp\\'" . metapost-mode)
     ("\\.vhdl?\\'" . vhdl-mode)
     ("\\.article\\'" . text-mode)
     ("\\.letter\\'" . text-mode)
     ("\\.tcl\\'" . tcl-mode)
     ("\\.exp\\'" . tcl-mode)
     ("\\.itcl\\'" . tcl-mode)
     ("\\.itk\\'" . tcl-mode)
     ("\\.icn\\'" . icon-mode)
     ("\\.sim\\'" . simula-mode)
     ("\\.mss\\'" . scribe-mode)
     ("\\.f90\\'" . f90-mode)
     ("\\.indent\\.pro\\'" . fundamental-mode) ; to avoid idlwave-mode
     ("\\.pro\\'" . idlwave-mode)
     ("\\.lsp\\'" . lisp-mode)
     ("\\.awk\\'" . awk-mode)
     ("\\.prolog\\'" . prolog-mode)
     ("\\.tar\\'" . tar-mode)
     ("\\.\\(arc\\|zip\\|lzh\\|zoo\\|jar\\)\\'" . archive-mode)
     ("\\.\\(ARC\\|ZIP\\|LZH\\|ZOO\\|JAR\\)\\'" . archive-mode)
     ;; Mailer puts message to be edited in
     ;; /tmp/Re.... or Message
     ("\\`/tmp/Re" . text-mode)
     ("/Message[0-9]*\\'" . text-mode)
     ("/drafts/[0-9]+\\'" . mh-letter-mode)
     ("\\.zone\\'" . zone-mode)
     ;; some news reader is reported to use this
     ("\\`/tmp/fol/" . text-mode)
     ("\\.y\\'" . c-mode)
     ("\\.lex\\'" . c-mode)
     ("\\.oak\\'" . scheme-mode)
     ("\\.sgml?\\'" . sgml-mode)
     ("\\.xml\\'" . sgml-mode)
     ("\\.dtd\\'" . sgml-mode)
     ("\\.ds\\(ss\\)?l\\'" . dsssl-mode)
     ("\\.idl\\'" . idl-mode)
     ;; .emacs following a directory delimiter
     ;; in Unix, MSDOG or VMS syntax.
     ("[]>:/\\]\\..*emacs\\'" . emacs-lisp-mode)
     ("\\`\\..*emacs\\'" . emacs-lisp-mode)
     ;; _emacs following a directory delimiter
     ;; in MsDos syntax
     ("[:/]_emacs\\'" . emacs-lisp-mode)
     ("/crontab\\.X*[0-9]+\\'" . shell-script-mode)
     ("\\.ml\\'" . lisp-mode)
     ("\\.\\(asn\\|mib\\|smi\\)\\'" . snmp-mode)
     ("\\.\\(as\\|mi\\|sm\\)2\\'" . snmpv2-mode)
     ("\\.\\(diffs?\\|patch\\|rej\\)\\'" . diff-mode)
     ("\\.\\(dif\\|pat\\)\\'" . diff-mode) ; for MSDOG
     ("\\.[eE]?[pP][sS]\\'" . ps-mode)
     ("configure\\.\\(ac\\|in\\)\\'" . autoconf-mode)
     ("BROWSE\\'" . ebrowse-tree-mode)
     ("\\.ebrowse\\'" . ebrowse-tree-mode)
     ("#\\*mail\\*" . mail-mode)
     ;; Get rid of any trailing .n.m and try again.
     ;; This is for files saved by cvs-merge that look like .#<file>.<rev>
     ;; or .#<file>.<rev>-<rev> or VC's <file>.~<rev>~.
     ;; Using mode nil rather than `ignore' would let the search continue
     ;; through this list (with the shortened name) rather than start over.
     ("\\.~?[0-9]+\\.[0-9][-.0-9]*~?\\'" ignore t)
     ;; The following should come after the ChangeLog pattern
     ;; for the sake of ChangeLog.1, etc.
     ;; and after the .scm.[0-9] and CVS' <file>.<rev> patterns too.
     ("\\.[1-9]\\'" . nroff-mode)
     ("\\.g\\'" . antlr-mode)))
  "Alist of filename patterns vs corresponding major mode functions.
Each element looks like (REGEXP . FUNCTION) or (REGEXP FUNCTION NON-NIL).
\(NON-NIL stands for anything that is not nil; the value does not matter.)
Visiting a file whose name matches REGEXP specifies FUNCTION as the
mode function to use.  FUNCTION will be called, unless it is nil.

If the element has the form (REGEXP FUNCTION NON-NIL), then after
calling FUNCTION (if it's not nil), we delete the suffix that matched
REGEXP and search the list again for another match.")


(defvar interpreter-mode-alist
  (mapc
   (lambda (l)
     (cons (purecopy (car l)) (cdr l)))
   '(("perl" . perl-mode)
     ("perl5" . perl-mode)
     ("miniperl" . perl-mode)
     ("wish" . tcl-mode)
     ("wishx" . tcl-mode)
     ("tcl" . tcl-mode)
     ("tclsh" . tcl-mode)
     ("awk" . awk-mode)
     ("mawk" . awk-mode)
     ("nawk" . awk-mode)
     ("gawk" . awk-mode)
     ("scm" . scheme-mode)
     ("ash" . sh-mode)
     ("bash" . sh-mode)
     ("bash2" . sh-mode)
     ("csh" . sh-mode)
     ("dtksh" . sh-mode)
     ("es" . sh-mode)
     ("itcsh" . sh-mode)
     ("jsh" . sh-mode)
     ("ksh" . sh-mode)
     ("oash" . sh-mode)
     ("pdksh" . sh-mode)
     ("rc" . sh-mode)
     ("rpm" . sh-mode)
     ("sh" . sh-mode)
     ("sh5" . sh-mode)
     ("tcsh" . sh-mode)
     ("wksh" . sh-mode)
     ("wsh" . sh-mode)
     ("zsh" . sh-mode)
     ("tail" . text-mode)
     ("more" . text-mode)
     ("less" . text-mode)
     ("pg" . text-mode)
     ("make" . makefile-mode)		; Debian uses this
     ("guile" . scheme-mode)
     ("clisp" . lisp-mode)))
  "Alist mapping interpreter names to major modes.
This alist applies to files whose first line starts with `#!'.
Each element looks like (INTERPRETER . MODE).
The car of each element is compared with
the name of the interpreter specified in the first line.
If it matches, mode MODE is selected.")

(defvar inhibit-first-line-modes-regexps '("\\.tar\\'" "\\.tgz\\'")
  "List of regexps; if one matches a file name, don't look for `-*-'.")

(defvar inhibit-first-line-modes-suffixes nil
  "List of regexps for what to ignore, for `inhibit-first-line-modes-regexps'.
When checking `inhibit-first-line-modes-regexps', we first discard
from the end of the file name anything that matches one of these regexps.")

(defvar auto-mode-interpreter-regexp
  "#![ \t]?\\([^ \t\n]*\
/bin/env[ \t]\\)?\\([^ \t\n]+\\)"
  "Regular expression matching interpreters, for file mode determination.
This regular expression is matched against the first line of a file
to determine the file's mode in `set-auto-mode' when Emacs can't deduce
a mode from the file's name.  If it matches, the file is assumed to
be interpreted by the interpreter matched by the second group of the
regular expression.  The mode is then determined as the mode associated
with that interpreter in `interpreter-mode-alist'.")

(defun set-auto-mode (&optional just-from-file-name)
  "Select major mode appropriate for current buffer.
This checks for a -*- mode tag in the buffer's text,
compares the filename against the entries in `auto-mode-alist',
or checks the interpreter that runs this file against
`interpreter-mode-alist'.

It does not check for the `mode:' local variable in the
Local Variables section of the file; for that, use `hack-local-variables'.

If `enable-local-variables' is nil, this function does not check for a
-*- mode tag.

If the optional argument JUST-FROM-FILE-NAME is non-nil,
then we do not set anything but the major mode,
and we don't even do that unless it would come from the file name."
  ;; Look for -*-MODENAME-*- or -*- ... mode: MODENAME; ... -*-
  (let (end done modes)
    (save-excursion
      (goto-char (point-min))
      (skip-chars-forward " \t\n")
      (and enable-local-variables
	   (setq end (set-auto-mode-1))
	   (if (save-excursion (search-forward ":" end t))
	       ;; Find all specifications for the `mode:' variable
	       ;; and execute them left to right.
	       (while (let ((case-fold-search t))
			(or (and (looking-at "mode:")
				 (goto-char (match-end 0)))
			    (re-search-forward "[ \t;]mode:" end t)))
		 (skip-chars-forward " \t")
		 (let ((beg (point)))
		   (if (search-forward ";" end t)
		       (forward-char -1)
		     (goto-char end))
		   (skip-chars-backward " \t")
		   (push (intern (concat (downcase (buffer-substring beg (point))) "-mode"))
			 modes)))
	     ;; Simple -*-MODE-*- case.
	     (push (intern (concat (downcase (buffer-substring (point) end))
				   "-mode"))
		   modes))))
    ;; If we found modes to use, invoke them now,
    ;; outside the save-excursion.
    (unless just-from-file-name
      (dolist (mode (nreverse modes))
	(if (not (functionp mode))
	    (message "Ignoring unknown mode `%s'" mode)
	  (setq done t)
	  (funcall mode))))
    ;; If we didn't find a mode from a -*- line, try using the file name.
    (if (and (not done) buffer-file-name)
	(let ((name buffer-file-name)
	      (keep-going t))
	  ;; Remove backup-suffixes from file name.
	  (setq name (file-name-sans-versions name))
	  (while keep-going
	    (setq keep-going nil)
	    (let ((alist auto-mode-alist)
		  (mode nil))
	      ;; Find first matching alist entry.
	      (let ((case-fold-search
		     (memq system-type '(vax-vms windows-nt))))
		(while (and (not mode) alist)
		  (if (string-match (car (car alist)) name)
		      (if (and (consp (cdr (car alist)))
			       (nth 2 (car alist)))
			  (setq mode (car (cdr (car alist)))
				name (substring name 0 (match-beginning 0))
				keep-going t)
			(setq mode (cdr (car alist))
			      keep-going nil)))
		  (setq alist (cdr alist))))
	      (if mode
		  ;; When JUST-FROM-FILE-NAME is set,
		  ;; we are working on behalf of set-visited-file-name.
		  ;; In that case, if the major mode specified is the
		  ;; same one we already have, don't actually reset it.
		  ;; We don't want to lose minor modes such as Font Lock.
		  (unless (and just-from-file-name (eq mode major-mode))
		    (funcall mode))
		;; If we can't deduce a mode from the file name,
		;; look for an interpreter specified in the first line.
		;; As a special case, allow for things like "#!/bin/env perl",
		;; which finds the interpreter anywhere in $PATH.
		(let ((interpreter
		       (save-excursion
			 (goto-char (point-min))
			 (if (looking-at auto-mode-interpreter-regexp)
			     (match-string 2)
			   "")))
		      elt)
		  ;; Map interpreter name to a mode.
		  (setq elt (assoc (file-name-nondirectory interpreter)
				   interpreter-mode-alist))
		  (unless just-from-file-name
		    (if elt
			(funcall (cdr elt))))))))))))


(defun set-auto-mode-1 ()
  "Find the -*- spec in the buffer.
Call with point at the place to start searching from.
If one is found, set point to the beginning
and return the position of the end.
Otherwise, return nil; point may be changed."
  (let (beg end)
    (and
     ;; Don't look for -*- if this file name matches any
     ;; of the regexps in inhibit-first-line-modes-regexps.
     (let ((temp inhibit-first-line-modes-regexps)
	   (name (if buffer-file-name
		     (file-name-sans-versions buffer-file-name)
		   (buffer-name))))
       (while (let ((sufs inhibit-first-line-modes-suffixes))
		(while (and sufs (not (string-match (car sufs) name)))
		  (setq sufs (cdr sufs)))
		sufs)
	 (setq name (substring name 0 (match-beginning 0))))
       (while (and temp
		   (not (string-match (car temp) name)))
	 (setq temp (cdr temp)))
       (not temp))

     (search-forward "-*-" (save-excursion
			     ;; If the file begins with "#!"
			     ;; (exec interpreter magic), look
			     ;; for mode frobs in the first two
			     ;; lines.  You cannot necessarily
			     ;; put them in the first line of
			     ;; such a file without screwing up
			     ;; the interpreter invocation.
			     (end-of-line (and (looking-at "^#!") 2))
			     (point)) t)
     (progn
       (skip-chars-forward " \t")
       (setq beg (point))
       (search-forward "-*-"
		       (save-excursion (end-of-line) (point))
		       t))
     (progn
       (forward-char -3)
       (skip-chars-backward " \t")
       (setq end (point))
       (goto-char beg)
       end))))

(defun hack-local-variables-prop-line ()
  "Set local variables specified in the -*- line.
Ignore any specification for `mode:' and `coding:';
`set-auto-mode' should already have handled `mode:',
`set-auto-coding' should already have handled `coding:'."
  (save-excursion
    (goto-char (point-min))
    (let ((result nil)
	  (end (set-auto-mode-1))
	  (enable-local-variables
	   (and local-enable-local-variables enable-local-variables)))
      ;; Parse the -*- line into the `result' alist.
      (cond ((not end)
	     nil)
	    ((looking-at "[ \t]*\\([^ \t\n\r:;]+\\)\\([ \t]*-\\*-\\)")
	     ;; Simple form: "-*- MODENAME -*-".  Already handled.
	     nil)
	    (t
	     ;; Hairy form: '-*-' [ <variable> ':' <value> ';' ]* '-*-'
	     ;; (last ";" is optional).
	     (while (< (point) end)
	       (or (looking-at "[ \t]*\\([^ \t\n:]+\\)[ \t]*:[ \t]*")
		   (error "Malformed -*- line"))
	       (goto-char (match-end 0))
	       ;; There used to be a downcase here,
	       ;; but the manual didn't say so,
	       ;; and people want to set var names that aren't all lc.
	       (let ((key (intern (buffer-substring
				   (match-beginning 1)
				   (match-end 1))))
		     (val (save-restriction
			    (narrow-to-region (point) end)
			    (read (current-buffer)))))
		 ;; It is traditional to ignore
		 ;; case when checking for `mode' in set-auto-mode,
		 ;; so we must do that here as well.
		 ;; That is inconsistent, but we're stuck with it.
		 ;; The same can be said for `coding' in set-auto-coding.
		 (or (equal (downcase (symbol-name key)) "mode")
		     (equal (downcase (symbol-name key)) "coding")
		     (setq result (cons (cons key val) result)))
		 (skip-chars-forward " \t;")))
	     (setq result (nreverse result))))

      (if (and result
	       (or (eq enable-local-variables t)
		   (and enable-local-variables
			(save-window-excursion
			  (condition-case nil
			      (switch-to-buffer (current-buffer))
			    (error
			     ;; If we fail to switch in the selected window,
			     ;; it is probably a minibuffer.
			     ;; So try another window.
			     (condition-case nil
				 (switch-to-buffer-other-window (current-buffer))
			       (error
				(switch-to-buffer-other-frame (current-buffer))))))
			  (y-or-n-p (format "Set local variables as specified in -*- line of %s? "
					    (file-name-nondirectory buffer-file-name)))))))
	  (let ((enable-local-eval enable-local-eval))
	    (while result
	      (hack-one-local-variable (car (car result)) (cdr (car result)))
	      (setq result (cdr result))))))))

(defvar hack-local-variables-hook nil
  "Normal hook run after processing a file's local variables specs.
Major modes can use this to examine user-specified local variables
in order to initialize other data structure based on them.")

(defun hack-local-variables (&optional mode-only)
  "Parse and put into effect this buffer's local variables spec.
If MODE-ONLY is non-nil, all we do is check whether the major mode
is specified, returning t if it is specified."
  (unless mode-only
    (hack-local-variables-prop-line))
  ;; Look for "Local variables:" line in last page.
  (let (mode-specified
	(enable-local-variables
	 (and local-enable-local-variables enable-local-variables)))
    (save-excursion
      (goto-char (point-max))
      (search-backward "\n\^L" (max (- (point-max) 3000) (point-min)) 'move)
      (if (let ((case-fold-search t))
	    (and (search-forward "Local Variables:" nil t)
		 (or (eq enable-local-variables t)
		     mode-only
		     (and enable-local-variables
			  (save-window-excursion
			    (switch-to-buffer (current-buffer))
			    (save-excursion
			      (beginning-of-line)
			      (set-window-start (selected-window) (point)))
			    (y-or-n-p (format "Set local variables as specified at end of %s? "
					      (if buffer-file-name
						  (file-name-nondirectory
						   buffer-file-name)
						(concat "buffer "
							(buffer-name))))))))))
	  (let ((continue t)
		prefix prefixlen suffix beg
		mode-specified
		(enable-local-eval enable-local-eval))
	    ;; The prefix is what comes before "local variables:" in its line.
	    ;; The suffix is what comes after "local variables:" in its line.
	    (skip-chars-forward " \t")
	    (or (eolp)
		(setq suffix (buffer-substring (point)
					       (progn (end-of-line) (point)))))
	    (goto-char (match-beginning 0))
	    (or (bolp)
		(setq prefix
		      (buffer-substring (point)
					(progn (beginning-of-line) (point)))))

	    (if prefix (setq prefixlen (length prefix)
			     prefix (regexp-quote prefix)))
	    (if suffix (setq suffix (concat (regexp-quote suffix) "$")))
	    (while continue
	      ;; Look at next local variable spec.
	      (if selective-display (re-search-forward "[\n\C-m]")
		(forward-line 1))
	      ;; Skip the prefix, if any.
	      (if prefix
		  (if (looking-at prefix)
		      (forward-char prefixlen)
		    (error "Local variables entry is missing the prefix")))
	      ;; Find the variable name; strip whitespace.
	      (skip-chars-forward " \t")
	      (setq beg (point))
	      (skip-chars-forward "^:\n")
	      (if (eolp) (error "Missing colon in local variables entry"))
	      (skip-chars-backward " \t")
	      (let* ((str (buffer-substring beg (point)))
		     (var (read str))
		    val)
		;; Setting variable named "end" means end of list.
		(if (string-equal (downcase str) "end")
		    (setq continue nil)
		  ;; Otherwise read the variable value.
		  (skip-chars-forward "^:")
		  (forward-char 1)
		  (setq val (read (current-buffer)))
		  (skip-chars-backward "\n")
		  (skip-chars-forward " \t")
		  (or (if suffix (looking-at suffix) (eolp))
		      (error "Local variables entry is terminated incorrectly"))
		  (if mode-only
		      (if (eq var 'mode)
			  (setq mode-specified t))
		    ;; Set the variable.  "Variables" mode and eval are funny.
		    (hack-one-local-variable var val))))))))
    (unless mode-only
      (run-hooks 'hack-local-variables-hook))
    mode-specified))

(defvar ignored-local-variables
  '(enable-local-eval)
  "Variables to be ignored in a file's local variable spec.")

;; Get confirmation before setting these variables as locals in a file.
(put 'debugger 'risky-local-variable t)
(put 'enable-local-eval 'risky-local-variable t)
(put 'ignored-local-variables 'risky-local-variable t)
(put 'eval 'risky-local-variable t)
(put 'file-name-handler-alist 'risky-local-variable t)
(put 'minor-mode-alist 'risky-local-variable t)
(put 'minor-mode-map-alist 'risky-local-variable t)
(put 'minor-mode-overriding-map-alist 'risky-local-variable t)
(put 'overriding-local-map 'risky-local-variable t)
(put 'overriding-terminal-local-map 'risky-local-variable t)
(put 'auto-mode-alist 'risky-local-variable t)
(put 'after-load-alist 'risky-local-variable t)
(put 'buffer-file-name 'risky-local-variable t)
(put 'buffer-undo-list 'risky-local-variable t)
(put 'buffer-auto-save-file-name 'risky-local-variable t)
(put 'buffer-file-truename 'risky-local-variable t)
(put 'default-text-properties 'risky-local-variable t)
(put 'exec-path 'risky-local-variable t)
(put 'load-path 'risky-local-variable t)
(put 'exec-directory 'risky-local-variable t)
(put 'process-environment 'risky-local-variable t)
(put 'dabbrev-case-fold-search 'risky-local-variable t)
(put 'dabbrev-case-replace 'risky-local-variable t)
;; Don't wait for outline.el to be loaded, for the sake of outline-minor-mode.
(put 'outline-level 'risky-local-variable t)
(put 'rmail-output-file-alist 'risky-local-variable t)
(put 'font-lock-defaults 'risky-local-variable t)
(put 'special-display-buffer-names 'risky-local-variable t)
(put 'frame-title-format 'risky-local-variable t)
(put 'global-mode-string 'risky-local-variable t)
(put 'header-line-format 'risky-local-variable t)
(put 'icon-title-format 'risky-local-variable t)
(put 'input-method-alist 'risky-local-variable t)
(put 'format-alist 'risky-local-variable t)
(put 'vc-mode 'risky-local-variable t)
(put 'imenu-generic-expression 'risky-local-variable t)
(put 'imenu-index-alist 'risky-local-variable t)
(put 'standard-input 'risky-local-variable t)
(put 'standard-output 'risky-local-variable t)
(put 'unread-command-events 'risky-local-variable t)
(put 'max-lisp-eval-depth 'risky-local-variable t)
(put 'max-specpdl-size 'risky-local-variable t)
(put 'mode-line-format 'risky-local-variable t)
(put 'mode-line-modified 'risky-local-variable t)
(put 'mode-line-mule-info 'risky-local-variable t)
(put 'mode-line-buffer-identification 'risky-local-variable t)
(put 'mode-line-modes 'risky-local-variable t)
(put 'mode-line-position 'risky-local-variable t)
(put 'display-time-string 'risky-local-variable t)

;; This one is safe because the user gets to check it before it is used.
(put 'compile-command 'safe-local-variable t)

(defun hack-one-local-variable-quotep (exp)
  (and (consp exp) (eq (car exp) 'quote) (consp (cdr exp))))

(defun hack-one-local-variable (var val)
  "\"Set\" one variable in a local variables spec.
A few patterns are specified so that any name which matches one
is considered risky."
  (cond ((eq var 'mode)
	 (funcall (intern (concat (downcase (symbol-name val))
				  "-mode"))))
	((eq var 'coding)
	 ;; We have already handled coding: tag in set-auto-coding.
	 nil)
	((memq var ignored-local-variables)
	 nil)
	;; "Setting" eval means either eval it or do nothing.
	;; Likewise for setting hook variables.
	((or (get var 'risky-local-variable)
	     (and
	      (string-match "-hooks?$\\|-functions?$\\|-forms?$\\|-program$\\|-command$\\|-predicate$\\|font-lock-keywords$\\|font-lock-keywords-[0-9]+$\\|font-lock-syntactic-keywords$\\|-frame-alist$\\|-mode-alist$\\|-map$\\|-map-alist$"
			    (symbol-name var))
	      (not (get var 'safe-local-variable))))
	 ;; Permit evalling a put of a harmless property.
	 ;; if the args do nothing tricky.
	 (if (or (and (eq var 'eval)
		      (consp val)
		      (eq (car val) 'put)
		      (hack-one-local-variable-quotep (nth 1 val))
		      (hack-one-local-variable-quotep (nth 2 val))
		      ;; Only allow safe values of lisp-indent-hook;
		      ;; not functions.
		      (or (numberp (nth 3 val))
			  (equal (nth 3 val) ''defun))
		      (memq (nth 1 (nth 2 val))
			    '(lisp-indent-hook)))
		 ;; Permit eval if not root and user says ok.
		 (and (not (zerop (user-uid)))
		      (or (eq enable-local-eval t)
			  (and enable-local-eval
			       (save-window-excursion
				 (switch-to-buffer (current-buffer))
				 (save-excursion
				   (beginning-of-line)
				   (set-window-start (selected-window) (point)))
				 (setq enable-local-eval
				       (y-or-n-p (format "Process `eval' or hook local variables in %s? "
							 (if buffer-file-name
							     (concat "file " (file-name-nondirectory buffer-file-name))
							   (concat "buffer " (buffer-name)))))))))))
	     (if (eq var 'eval)
		 (save-excursion (eval val))
	       (make-local-variable var)
	       (set var val))
	   (message "Ignoring `eval:' in the local variables list")))
	;; Ordinary variable, really set it.
	(t (make-local-variable var)
	   ;; Make sure the string has no text properties.
	   ;; Some text properties can get evaluated in various ways,
	   ;; so it is risky to put them on with a local variable list.
	   (if (stringp val)
	       (set-text-properties 0 (length val) nil val))
	   (set var val))))


(defcustom change-major-mode-with-file-name t
  "*Non-nil means \\[write-file] should set the major mode from the file name.
However, the mode will not be changed if
\(1) a local variables list or the `-*-' line specifies a major mode, or
\(2) the current major mode is a \"special\" mode,
\    not suitable for ordinary files, or
\(3) the new file name does not particularly specify any mode."
  :type 'boolean
  :group 'editing-basics)

(defun set-visited-file-name (filename &optional no-query along-with-file)
  "Change name of file visited in current buffer to FILENAME.
The next time the buffer is saved it will go in the newly specified file.
nil or empty string as argument means make buffer not be visiting any file.
Remember to delete the initial contents of the minibuffer
if you wish to pass an empty string as the argument.

The optional second argument NO-QUERY, if non-nil, inhibits asking for
confirmation in the case where another buffer is already visiting FILENAME.

The optional third argument ALONG-WITH-FILE, if non-nil, means that
the old visited file has been renamed to the new name FILENAME."
  (interactive "FSet visited file name: ")
  (if (buffer-base-buffer)
      (error "An indirect buffer cannot visit a file"))
  (let (truename)
    (if filename
	(setq filename
	      (if (string-equal filename "")
		  nil
		(expand-file-name filename))))
    (if filename
	(progn
	  (setq truename (file-truename filename))
	  (if find-file-visit-truename
	      (setq filename truename))))
    (let ((buffer (and filename (find-buffer-visiting filename))))
      (and buffer (not (eq buffer (current-buffer)))
	   (not no-query)
	   (not (y-or-n-p (message "A buffer is visiting %s; proceed? "
				   filename)))
	   (error "Aborted")))
    (or (equal filename buffer-file-name)
	(progn
	  (and filename (lock-buffer filename))
	  (unlock-buffer)))
    (setq buffer-file-name filename)
    (if filename			; make buffer name reflect filename.
	(let ((new-name (file-name-nondirectory buffer-file-name)))
	  (if (string= new-name "")
	      (error "Empty file name"))
	  (if (eq system-type 'vax-vms)
	      (setq new-name (downcase new-name)))
	  (setq default-directory (file-name-directory buffer-file-name))
	  (or (string= new-name (buffer-name))
	      (rename-buffer new-name t))))
    (setq buffer-backed-up nil)
    (or along-with-file
	(clear-visited-file-modtime))
    ;; Abbreviate the file names of the buffer.
    (if truename
	(progn
	  (setq buffer-file-truename (abbreviate-file-name truename))
	  (if find-file-visit-truename
	      (setq buffer-file-name buffer-file-truename))))
    (setq buffer-file-number
	  (if filename
	      (nthcdr 10 (file-attributes buffer-file-name))
	      nil)))
  ;; write-file-hooks is normally used for things like ftp-find-file
  ;; that visit things that are not local files as if they were files.
  ;; Changing to visit an ordinary local file instead should flush the hook.
  (kill-local-variable 'write-file-hooks)
  (kill-local-variable 'local-write-file-hooks)
  (kill-local-variable 'revert-buffer-function)
  (kill-local-variable 'backup-inhibited)
  ;; If buffer was read-only because of version control,
  ;; that reason is gone now, so make it writable.
  (if vc-mode
      (setq buffer-read-only nil))
  (kill-local-variable 'vc-mode)
  ;; Turn off backup files for certain file names.
  ;; Since this is a permanent local, the major mode won't eliminate it.
  (and buffer-file-name
       (not (funcall backup-enable-predicate buffer-file-name))
       (progn
	 (make-local-variable 'backup-inhibited)
	 (setq backup-inhibited t)))
  (let ((oauto buffer-auto-save-file-name))
    ;; If auto-save was not already on, turn it on if appropriate.
    (if (not buffer-auto-save-file-name)
	(and buffer-file-name auto-save-default
	     (auto-save-mode t))
      ;; If auto save is on, start using a new name.
      ;; We deliberately don't rename or delete the old auto save
      ;; for the old visited file name.  This is because perhaps
      ;; the user wants to save the new state and then compare with the
      ;; previous state from the auto save file.
      (setq buffer-auto-save-file-name
	    (make-auto-save-file-name)))
    ;; Rename the old auto save file if any.
    (and oauto buffer-auto-save-file-name
	 (file-exists-p oauto)
	 (rename-file oauto buffer-auto-save-file-name t)))
  (and buffer-file-name
       (not along-with-file)
       (set-buffer-modified-p t))
  ;; Update the major mode, if the file name determines it.
  (condition-case nil
      ;; Don't change the mode if it is special.
      (or (not change-major-mode-with-file-name)
	  (get major-mode 'mode-class)
	  ;; Don't change the mode if the local variable list specifies it.
	  (hack-local-variables t)
	  (set-auto-mode t))
    (error nil)))

(defun write-file (filename &optional confirm)
  "Write current buffer into file FILENAME.
This makes the buffer visit that file, and marks it as not modified.

If you specify just a directory name as FILENAME, that means to use
the default file name but in that directory.  You can also yank
the default file name into the minibuffer to edit it, using M-n.

If the buffer is not already visiting a file, the default file name
for the output file is the buffer name.

If optional second arg CONFIRM is non-nil, this function
asks for confirmation before overwriting an existing file.
Interactively, confirmation is required unless you supply a prefix argument."
;;  (interactive "FWrite file: ")
  (interactive
   (list (if buffer-file-name
	     (read-file-name "Write file: "
				 nil nil nil nil)
	   (read-file-name "Write file: " default-directory
			   (expand-file-name
			    (file-name-nondirectory (buffer-name))
			    default-directory)
			   nil nil))
	 (not current-prefix-arg)))
  (or (null filename) (string-equal filename "")
      (progn
	;; If arg is just a directory,
	;; use the default file name, but in that directory.
	(if (file-directory-p filename)
	    (setq filename (concat (file-name-as-directory filename)
				   (file-name-nondirectory
				    (or buffer-file-name (buffer-name))))))
	(and confirm
	     (file-exists-p filename)
	     (or (y-or-n-p (format "File `%s' exists; overwrite? " filename))
		 (error "Canceled")))
	(set-visited-file-name filename (not confirm))))
  (set-buffer-modified-p t)
  ;; Make buffer writable if file is writable.
  (and buffer-file-name
       (file-writable-p buffer-file-name)
       (setq buffer-read-only nil))
  (save-buffer))

(defun backup-buffer ()
  "Make a backup of the disk file visited by the current buffer, if appropriate.
This is normally done before saving the buffer the first time.
If the value is non-nil, it is the result of `file-modes' on the original
file; this means that the caller, after saving the buffer, should change
the modes of the new file to agree with the old modes.

A backup may be done by renaming or by copying; see documentation of
variable `make-backup-files'.  If it's done by renaming, then the file is
no longer accessible under its old name."
  (if (and make-backup-files (not backup-inhibited)
	   (not buffer-backed-up)
	   (file-exists-p buffer-file-name)
	   (memq (aref (elt (file-attributes buffer-file-name) 8) 0)
		 '(?- ?l)))
      (let ((real-file-name buffer-file-name)
	    backup-info backupname targets setmodes)
	;; If specified name is a symbolic link, chase it to the target.
	;; Thus we make the backups in the directory where the real file is.
	(setq real-file-name (file-chase-links real-file-name))
	(setq backup-info (find-backup-file-name real-file-name)
	      backupname (car backup-info)
	      targets (cdr backup-info))
	;; (if (file-directory-p buffer-file-name)
	;;     (error "Cannot save buffer in directory %s" buffer-file-name))
	(if backup-info
	    (condition-case ()
		(let ((delete-old-versions
		       ;; If have old versions to maybe delete,
		       ;; ask the user to confirm now, before doing anything.
		       ;; But don't actually delete til later.
		       (and targets
			    (or (eq delete-old-versions t) (eq delete-old-versions nil))
			    (or delete-old-versions
				(y-or-n-p (format "Delete excess backup versions of %s? "
						  real-file-name))))))
		  ;; Actually write the back up file.
		  (condition-case ()
		      (if (or file-precious-flag
    ;			      (file-symlink-p buffer-file-name)
			      backup-by-copying
			      (and backup-by-copying-when-linked
				   (> (file-nlinks real-file-name) 1))
			      (and (or backup-by-copying-when-mismatch
				       (integerp backup-by-copying-when-privileged-mismatch))
				   (let ((attr (file-attributes real-file-name)))
				     (and (or backup-by-copying-when-mismatch
					      (and (integerp (nth 2 attr))
						   (integerp backup-by-copying-when-privileged-mismatch)
						   (<= (nth 2 attr) backup-by-copying-when-privileged-mismatch)))
					  (or (nth 9 attr)
					      (not (file-ownership-preserved-p real-file-name)))))))
			  (condition-case ()
			      (copy-file real-file-name backupname t t)
			    (file-error
			     ;; If copying fails because file BACKUPNAME
			     ;; is not writable, delete that file and try again.
			     (if (and (file-exists-p backupname)
				      (not (file-writable-p backupname)))
				 (delete-file backupname))
			     (copy-file real-file-name backupname t t)))
			;; rename-file should delete old backup.
			(rename-file real-file-name backupname t)
			(setq setmodes (file-modes backupname)))
		    (file-error
		     ;; If trouble writing the backup, write it in ~.
		     (setq backupname (expand-file-name
				       (convert-standard-filename
					"~/%backup%~")))
		     (message "Cannot write backup file; backing up in %s"
			      (file-name-nondirectory backupname))
		     (sleep-for 1)
		     (condition-case ()
			 (copy-file real-file-name backupname t t)
		       (file-error
			;; If copying fails because file BACKUPNAME
			;; is not writable, delete that file and try again.
			(if (and (file-exists-p backupname)
				 (not (file-writable-p backupname)))
			    (delete-file backupname))
			(copy-file real-file-name backupname t t)))))
		  (setq buffer-backed-up t)
		  ;; Now delete the old versions, if desired.
		  (if delete-old-versions
		      (while targets
			(condition-case ()
			    (delete-file (car targets))
			  (file-error nil))
			(setq targets (cdr targets))))
		  setmodes)
	    (file-error nil))))))

(defun file-name-sans-versions (name &optional keep-backup-version)
  "Return file NAME sans backup versions or strings.
This is a separate procedure so your site-init or startup file can
redefine it.
If the optional argument KEEP-BACKUP-VERSION is non-nil,
we do not remove backup version numbers, only true file version numbers."
  (let ((handler (find-file-name-handler name 'file-name-sans-versions)))
    (if handler
	(funcall handler 'file-name-sans-versions name keep-backup-version)
      (substring name 0
		 (if (eq system-type 'vax-vms)
		     ;; VMS version number is (a) semicolon, optional
		     ;; sign, zero or more digits or (b) period, option
		     ;; sign, zero or more digits, provided this is the
		     ;; second period encountered outside of the
		     ;; device/directory part of the file name.
		     (or (string-match ";[-+]?[0-9]*\\'" name)
			 (if (string-match "\\.[^]>:]*\\(\\.[-+]?[0-9]*\\)\\'"
					   name)
			     (match-beginning 1))
			 (length name))
		   (if keep-backup-version
		       (length name)
		     (or (string-match "\\.~[0-9.]+~\\'" name)
			 (string-match "~\\'" name)
			 (length name))))))))

(defun file-ownership-preserved-p (file)
  "Return t if deleting FILE and rewriting it would preserve the owner."
  (let ((handler (find-file-name-handler file 'file-ownership-preserved-p)))
    (if handler
	(funcall handler 'file-ownership-preserved-p file)
      (let ((attributes (file-attributes file)))
	;; Return t if the file doesn't exist, since it's true that no
	;; information would be lost by an (attempted) delete and create.
	(or (null attributes)
	    (= (nth 2 attributes) (user-uid)))))))

(defun file-name-sans-extension (filename)
  "Return FILENAME sans final \"extension\".
The extension, in a file name, is the part that follows the last `.',
except that a leading `.', if any, doesn't count."
  (save-match-data
    (let ((file (file-name-sans-versions (file-name-nondirectory filename)))
	  directory)
      (if (and (string-match "\\.[^.]*\\'" file)
	       (not (eq 0 (match-beginning 0))))
	  (if (setq directory (file-name-directory filename))
	      (expand-file-name (substring file 0 (match-beginning 0))
				directory)
	    (substring file 0 (match-beginning 0)))
	filename))))

(defun file-name-extension (filename &optional period)
  "Return FILENAME's final \"extension\".
The extension, in a file name, is the part that follows the last `.',
except that a leading `.', if any, doesn't count.
Return nil for extensionless file names such as `foo'.
Return the empty string for file names such as `foo.'.

If PERIOD is non-nil, then the returned value includes the period
that delimits the extension, and if FILENAME has no extension,
the value is \"\"."
  (save-match-data
    (let ((file (file-name-sans-versions (file-name-nondirectory filename))))
      (if (and (string-match "\\.[^.]*\\'" file)
	       (not (eq 0 (match-beginning 0))))
          (substring file (+ (match-beginning 0) (if period 0 1)))
        (if period
            "")))))

(defcustom make-backup-file-name-function nil
  "A function to use instead of the default `make-backup-file-name'.
A value of nil gives the default `make-backup-file-name' behaviour.

This could be buffer-local to do something special for specific
files.  If you define it, you may need to change `backup-file-name-p'
and `file-name-sans-versions' too.

See also `backup-directory-alist'."
  :group 'backup
  :type '(choice (const :tag "Default" nil)
		 (function :tag "Your function")))

(defcustom backup-directory-alist nil
  "Alist of filename patterns and backup directory names.
Each element looks like (REGEXP . DIRECTORY).  Backups of files with
names matching REGEXP will be made in DIRECTORY.  DIRECTORY may be
relative or absolute.  If it is absolute, so that all matching files
are backed up into the same directory, the file names in this
directory will be the full name of the file backed up with all
directory separators changed to `!' to prevent clashes.  This will not
work correctly if your filesystem truncates the resulting name.

For the common case of all backups going into one directory, the alist
should contain a single element pairing \".\" with the appropriate
directory name.

If this variable is nil, or it fails to match a filename, the backup
is made in the original file's directory.

On MS-DOS filesystems without long names this variable is always
ignored."
  :group 'backup
  :type '(repeat (cons (regexp :tag "Regexp matching filename")
		       (directory :tag "Backup directory name"))))

(defun normal-backup-enable-predicate (name)
  "Default `backup-enable-predicate' function.
Checks for files in `temporary-file-directory' or
`small-temporary-file-directory'."
  (not (or (let ((comp (compare-strings temporary-file-directory 0 nil
					name 0 nil)))
	     ;; Directory is under temporary-file-directory.
	     (and (not (eq comp t))
		  (< comp (- (length temporary-file-directory)))))
	   (if small-temporary-file-directory
	       (let ((comp (compare-strings small-temporary-file-directory
					    0 nil
					    name 0 nil)))
		 ;; Directory is under small-temporary-file-directory.
		 (and (not (eq comp t))
		      (< comp (- (length small-temporary-file-directory)))))))))

(defun make-backup-file-name (file)
  "Create the non-numeric backup file name for FILE.
Normally this will just be the file's name with `~' appended.
Customization hooks are provided as follows.

If the variable `make-backup-file-name-function' is non-nil, its value
should be a function which will be called with FILE as its argument;
the resulting name is used.

Otherwise a match for FILE is sought in `backup-directory-alist'; see
the documentation of that variable.  If the directory for the backup
doesn't exist, it is created."
  (if make-backup-file-name-function
      (funcall make-backup-file-name-function file)
    (if (and (eq system-type 'ms-dos)
	     (not (msdos-long-file-names)))
	(let ((fn (file-name-nondirectory file)))
	  (concat (file-name-directory file)
		  (or (and (string-match "\\`[^.]+\\'" fn)
			   (concat (match-string 0 fn) ".~"))
		      (and (string-match "\\`[^.]+\\.\\(..?\\)?" fn)
			   (concat (match-string 0 fn) "~")))))
      (concat (make-backup-file-name-1 file) "~"))))

(defun make-backup-file-name-1 (file)
  "Subroutine of `make-backup-file-name' and `find-backup-file-name'."
  (let ((alist backup-directory-alist)
	elt backup-directory dir-sep-string)
    (while alist
      (setq elt (pop alist))
      (if (string-match (car elt) file)
	  (setq backup-directory (cdr elt)
		alist nil)))
    (if (null backup-directory)
	file
      (unless (file-exists-p backup-directory)
	(condition-case nil
	    (make-directory backup-directory 'parents)
	  (file-error file)))
      (if (file-name-absolute-p backup-directory)
	  (progn
	    (when (memq system-type '(windows-nt ms-dos))
	      ;; Normalize DOSish file names: convert all slashes to
	      ;; directory-sep-char, downcase the drive letter, if any,
	      ;; and replace the leading "x:" with "/drive_x".
	      (or (file-name-absolute-p file)
		  (setq file (expand-file-name file))) ; make defaults explicit
	      ;; Replace any invalid file-name characters (for the
	      ;; case of backing up remote files).
	      (setq file (expand-file-name (convert-standard-filename file)))
	      (setq dir-sep-string (char-to-string directory-sep-char))
	      (if (eq (aref file 1) ?:)
		  (setq file (concat dir-sep-string
				     "drive_"
				     (char-to-string (downcase (aref file 0)))
				     (if (eq (aref file 2) directory-sep-char)
					 ""
				       dir-sep-string)
				     (substring file 2)))))
	    ;; Make the name unique by substituting directory
	    ;; separators.  It may not really be worth bothering about
	    ;; doubling `!'s in the original name...
	    (expand-file-name
	     (subst-char-in-string
	      directory-sep-char ?!
	      (replace-regexp-in-string "!" "!!" file))
	     backup-directory))
	(expand-file-name (file-name-nondirectory file)
			  (file-name-as-directory
			   (expand-file-name backup-directory
					     (file-name-directory file))))))))

(defun backup-file-name-p (file)
  "Return non-nil if FILE is a backup file name (numeric or not).
This is a separate function so you can redefine it for customization.
You may need to redefine `file-name-sans-versions' as well."
    (string-match "~\\'" file))

(defvar backup-extract-version-start)

;; This is used in various files.
;; The usage of backup-extract-version-start is not very clean,
;; but I can't see a good alternative, so as of now I am leaving it alone.
(defun backup-extract-version (fn)
  "Given the name of a numeric backup file, FN, return the backup number.
Uses the free variable `backup-extract-version-start', whose value should be
the index in the name where the version number begins."
  (if (and (string-match "[0-9]+~$" fn backup-extract-version-start)
	   (= (match-beginning 0) backup-extract-version-start))
      (string-to-int (substring fn backup-extract-version-start -1))
      0))

;; I believe there is no need to alter this behavior for VMS;
;; since backup files are not made on VMS, it should not get called.
(defun find-backup-file-name (fn)
  "Find a file name for a backup file FN, and suggestions for deletions.
Value is a list whose car is the name for the backup file
and whose cdr is a list of old versions to consider deleting now.
If the value is nil, don't make a backup.
Uses `backup-directory-alist' in the same way as does
`make-backup-file-name'."
  (let ((handler (find-file-name-handler fn 'find-backup-file-name)))
    ;; Run a handler for this function so that ange-ftp can refuse to do it.
    (if handler
	(funcall handler 'find-backup-file-name fn)
      (if (or (eq version-control 'never)
	      ;; We don't support numbered backups on plain MS-DOS
	      ;; when long file names are unavailable.
	      (and (eq system-type 'ms-dos)
		   (not (msdos-long-file-names))))
	  (list (make-backup-file-name fn))
	(let* ((basic-name (make-backup-file-name-1 fn))
	       (base-versions (concat (file-name-nondirectory basic-name)
				      ".~"))
	       (backup-extract-version-start (length base-versions))
	       (high-water-mark 0)
	       (number-to-delete 0)
	       possibilities deserve-versions-p versions)
	  (condition-case ()
	      (setq possibilities (file-name-all-completions
				   base-versions
				   (file-name-directory basic-name))
		    versions (sort (mapcar #'backup-extract-version
					   possibilities)
				   #'<)
		    high-water-mark (apply 'max 0 versions)
		    deserve-versions-p (or version-control
					   (> high-water-mark 0))
		    number-to-delete (- (length versions)
					kept-old-versions
					kept-new-versions
					-1))
	    (file-error (setq possibilities nil)))
	  (if (not deserve-versions-p)
	      (list (make-backup-file-name fn))
	    (cons (format "%s.~%d~" basic-name (1+ high-water-mark))
		  (if (and (> number-to-delete 0)
			   ;; Delete nothing if there is overflow
			   ;; in the number of versions to keep.
			   (>= (+ kept-new-versions kept-old-versions -1) 0))
		      (mapcar (lambda (n)
				(format "%s.~%d~" basic-name n))
			      (let ((v (nthcdr kept-old-versions versions)))
				(rplacd (nthcdr (1- number-to-delete) v) ())
				v))))))))))

(defun file-nlinks (filename)
  "Return number of names file FILENAME has."
  (car (cdr (file-attributes filename))))

(defun file-relative-name (filename &optional directory)
  "Convert FILENAME to be relative to DIRECTORY (default: `default-directory').
This function returns a relative file name which is equivalent to FILENAME
when used with that default directory as the default.
If this is impossible (which can happen on MSDOS and Windows
when the file name and directory use different drive names)
then it returns FILENAME."
  (save-match-data
    (let ((fname (expand-file-name filename)))
      (setq directory (file-name-as-directory
		       (expand-file-name (or directory default-directory))))
      ;; On Microsoft OSes, if FILENAME and DIRECTORY have different
      ;; drive names, they can't be relative, so return the absolute name.
      (if (and (or (eq system-type 'ms-dos)
		   (eq system-type 'windows-nt))
	       (not (string-equal (substring fname  0 2)
				  (substring directory 0 2))))
	  filename
	(let ((ancestor ".")
	      (fname-dir (file-name-as-directory fname)))
	  (while (and (not (string-match (concat "^" (regexp-quote directory)) fname-dir))
		      (not (string-match (concat "^" (regexp-quote directory)) fname)))
	    (setq directory (file-name-directory (substring directory 0 -1))
		  ancestor (if (equal ancestor ".")
			       ".."
			     (concat "../" ancestor))))
	  ;; Now ancestor is empty, or .., or ../.., etc.
	  (if (string-match (concat "^" (regexp-quote directory)) fname)
	      ;; We matched within FNAME's directory part.
	      ;; Add the rest of FNAME onto ANCESTOR.
	      (let ((rest (substring fname (match-end 0))))
		(if (and (equal ancestor ".")
			 (not (equal rest "")))
		    ;; But don't bother with ANCESTOR if it would give us `./'.
		    rest
		  (concat (file-name-as-directory ancestor) rest)))
	    ;; We matched FNAME's directory equivalent.
	    ancestor))))))

(defun save-buffer (&optional args)
  "Save current buffer in visited file if modified.  Versions described below.
By default, makes the previous version into a backup file
 if previously requested or if this is the first save.
With 1 \\[universal-argument], marks this version
 to become a backup when the next save is done.
With 2 \\[universal-argument]'s,
 unconditionally makes the previous version into a backup file.
With 3 \\[universal-argument]'s, marks this version
 to become a backup when the next save is done,
 and unconditionally makes the previous version into a backup file.

With argument of 0, never make the previous version into a backup file.

If a file's name is FOO, the names of its numbered backup versions are
 FOO.~i~ for various integers i.  A non-numbered backup file is called FOO~.
Numeric backups (rather than FOO~) will be made if value of
 `version-control' is not the atom `never' and either there are already
 numeric versions of the file being backed up, or `version-control' is
 non-nil.
We don't want excessive versions piling up, so there are variables
 `kept-old-versions', which tells Emacs how many oldest versions to keep,
 and `kept-new-versions', which tells how many newest versions to keep.
 Defaults are 2 old versions and 2 new.
`dired-kept-versions' controls dired's clean-directory (.) command.
If `delete-old-versions' is nil, system will query user
 before trimming versions.  Otherwise it does it silently.

If `vc-make-backup-files' is nil, which is the default,
 no backup files are made for files managed by version control.
 (This is because the version control system itself records previous versions.)

See the subroutine `basic-save-buffer' for more information."
  (interactive "p")
  (let ((modp (buffer-modified-p))
	(large (> (buffer-size) 50000))
	(make-backup-files (or (and make-backup-files (not (eq args 0)))
			       (memq args '(16 64)))))
    (and modp (memq args '(16 64)) (setq buffer-backed-up nil))
    (if (and modp large (buffer-file-name))
	(message "Saving file %s..." (buffer-file-name)))
    (basic-save-buffer)
    (and modp (memq args '(4 64)) (setq buffer-backed-up nil))))

(defun delete-auto-save-file-if-necessary (&optional force)
  "Delete auto-save file for current buffer if `delete-auto-save-files' is t.
Normally delete only if the file was written by this Emacs since
the last real save, but optional arg FORCE non-nil means delete anyway."
  (and buffer-auto-save-file-name delete-auto-save-files
       (not (string= buffer-file-name buffer-auto-save-file-name))
       (or force (recent-auto-save-p))
       (progn
	 (condition-case ()
	     (delete-file buffer-auto-save-file-name)
	   (file-error nil))
	 (set-buffer-auto-saved))))

(defvar auto-save-hook nil
  "Normal hook run just before auto-saving.")

(defcustom after-save-hook nil
  "Normal hook that is run after a buffer is saved to its file."
  :options '(executable-make-buffer-file-executable-if-script-p)
  :type 'hook
  :group 'files)

(defvar save-buffer-coding-system nil
  "If non-nil, use this coding system for saving the buffer.
More precisely, use this coding system in place of the
value of `buffer-file-coding-system', when saving the buffer.
Calling `write-region' for any purpose other than saving the buffer
will still use `buffer-file-coding-system'; this variable has no effect
in such cases.")

(make-variable-buffer-local 'save-buffer-coding-system)
(put 'save-buffer-coding-system 'permanent-local t)

(defun basic-save-buffer ()
  "Save the current buffer in its visited file, if it has been modified.
The hooks `write-contents-hooks', `local-write-file-hooks' and
`write-file-hooks' get a chance to do the job of saving; if they do not,
then the buffer is saved in the visited file file in the usual way.
After saving the buffer, this function runs `after-save-hook'."
  (interactive)
  (save-current-buffer
    ;; In an indirect buffer, save its base buffer instead.
    (if (buffer-base-buffer)
	(set-buffer (buffer-base-buffer)))
    (if (buffer-modified-p)
	(let ((recent-save (recent-auto-save-p))
	      setmodes tempsetmodes)
	  ;; On VMS, rename file and buffer to get rid of version number.
	  (if (and (eq system-type 'vax-vms)
		   (not (string= buffer-file-name
				 (file-name-sans-versions buffer-file-name))))
	      (let (buffer-new-name)
		;; Strip VMS version number before save.
		(setq buffer-file-name
		      (file-name-sans-versions buffer-file-name))
		;; Construct a (unique) buffer name to correspond.
		(let ((buf (create-file-buffer (downcase buffer-file-name))))
		  (setq buffer-new-name (buffer-name buf))
		  (kill-buffer buf))
		(rename-buffer buffer-new-name)))
	  ;; If buffer has no file name, ask user for one.
	  (or buffer-file-name
	      (let ((filename
		     (expand-file-name
		      (read-file-name "File to save in: ") nil)))
		(and (file-exists-p filename)
		     (or (y-or-n-p (format "File `%s' exists; overwrite? "
					   filename))
			 (error "Canceled")))
		(set-visited-file-name filename)))
	  (or (verify-visited-file-modtime (current-buffer))
	      (not (file-exists-p buffer-file-name))
	      (yes-or-no-p
	       (format "%s has changed since visited or saved.  Save anyway? "
		       (file-name-nondirectory buffer-file-name)))
	      (error "Save not confirmed"))
	  (save-restriction
	    (widen)
	    (save-excursion
	      (and (> (point-max) 1)
		   (not find-file-literally)
		   (/= (char-after (1- (point-max))) ?\n)
		   (not (and (eq selective-display t)
			     (= (char-after (1- (point-max))) ?\r)))
		   (or (eq require-final-newline t)
		       (and require-final-newline
			    (y-or-n-p
			     (format "Buffer %s does not end in newline.  Add one? "
				     (buffer-name)))))
		   (save-excursion
		     (goto-char (point-max))
		     (insert ?\n))))
	    ;; Support VC version backups.
	    (vc-before-save)
	    (or (run-hook-with-args-until-success 'write-contents-hooks)
		(run-hook-with-args-until-success 'local-write-file-hooks)
		(run-hook-with-args-until-success 'write-file-hooks)
		;; If a hook returned t, file is already "written".
		;; Otherwise, write it the usual way now.
		(setq setmodes (basic-save-buffer-1)))
	    ;; Now we have saved the current buffer.  Let's make sure
	    ;; that buffer-file-coding-system is fixed to what
	    ;; actually used for saving by binding it locally.
	    (if save-buffer-coding-system
		(setq save-buffer-coding-system last-coding-system-used)
	      (setq buffer-file-coding-system last-coding-system-used))
	    (setq buffer-file-number
		  (nthcdr 10 (file-attributes buffer-file-name)))
	    (if setmodes
		(condition-case ()
		    (set-file-modes buffer-file-name setmodes)
		  (error nil))))
	  ;; If the auto-save file was recent before this command,
	  ;; delete it now.
	  (delete-auto-save-file-if-necessary recent-save)
	  ;; Support VC `implicit' locking.
	  (vc-after-save)
	  (run-hooks 'after-save-hook))
      (message "(No changes need to be saved)"))))

;; This does the "real job" of writing a buffer into its visited file
;; and making a backup file.  This is what is normally done
;; but inhibited if one of write-file-hooks returns non-nil.
;; It returns a value to store in setmodes.
(defun basic-save-buffer-1 ()
  (if save-buffer-coding-system
      (let ((coding-system-for-write save-buffer-coding-system))
	(basic-save-buffer-2))
    (basic-save-buffer-2)))

(defun basic-save-buffer-2 ()
  (let (tempsetmodes setmodes)
    (if (not (file-writable-p buffer-file-name))
	(let ((dir (file-name-directory buffer-file-name)))
	  (if (not (file-directory-p dir))
	      (if (file-exists-p dir)
		  (error "%s is not a directory" dir)
		(error "%s: no such directory" buffer-file-name))
	    (if (not (file-exists-p buffer-file-name))
		(error "Directory %s write-protected" dir)
	      (if (yes-or-no-p
		   (format "File %s is write-protected; try to save anyway? "
			   (file-name-nondirectory
			    buffer-file-name)))
		  (setq tempsetmodes t)
		(error "Attempt to save to a file which you aren't allowed to write"))))))
    (or buffer-backed-up
	(setq setmodes (backup-buffer)))
    (let ((dir (file-name-directory buffer-file-name)))
      (if (and file-precious-flag
	       (file-writable-p dir))
	  ;; If file is precious, write temp name, then rename it.
	  ;; This requires write access to the containing dir,
	  ;; which is why we don't try it if we don't have that access.
	  (let ((realname buffer-file-name)
		tempname temp nogood i succeed
		(old-modtime (visited-file-modtime)))
	    (setq i 0)
	    (setq nogood t)
	    ;; Find the temporary name to write under.
	    (while nogood
	      (setq tempname (format
			      (if (and (eq system-type 'ms-dos)
				       (not (msdos-long-file-names)))
				  "%s#%d.tm#" ; MSDOS limits files to 8+3
				(if (memq system-type '(vax-vms axp-vms))
				    "%s$tmp$%d"
				  "%s#tmp#%d"))
			      dir i))
	      (setq nogood (file-exists-p tempname))
	      (setq i (1+ i)))
	    (unwind-protect
		(progn (clear-visited-file-modtime)
		       (write-region (point-min) (point-max)
				     tempname nil realname
				     buffer-file-truename)
		       (setq succeed t))
	      ;; If writing the temp file fails,
	      ;; delete the temp file.
	      (or succeed
		  (progn
		    (condition-case nil
			(delete-file tempname)
		      (file-error nil))
		    (set-visited-file-modtime old-modtime))))
	    ;; Since we have created an entirely new file
	    ;; and renamed it, make sure it gets the
	    ;; right permission bits set.
	    (setq setmodes (file-modes buffer-file-name))
	    ;; We succeeded in writing the temp file,
	    ;; so rename it.
	    (rename-file tempname buffer-file-name t))
	;; If file not writable, see if we can make it writable
	;; temporarily while we write it.
	;; But no need to do so if we have just backed it up
	;; (setmodes is set) because that says we're superseding.
	(cond ((and tempsetmodes (not setmodes))
	       ;; Change the mode back, after writing.
	       (setq setmodes (file-modes buffer-file-name))
	       (set-file-modes buffer-file-name (logior setmodes 128))))
	(write-region (point-min) (point-max)
		      buffer-file-name nil t buffer-file-truename)))
    setmodes))

(defun save-some-buffers (&optional arg pred)
  "Save some modified file-visiting buffers.  Asks user about each one.
Optional argument (the prefix) non-nil means save all with no questions.
Optional second argument PRED determines which buffers are considered:
If PRED is nil, all the file-visiting buffers are considered.
If PRED is t, then certain non-file buffers will also be considered.
If PRED is a zero-argument function, it indicates for each buffer whether
to consider it or not when called with that buffer current."
  (interactive "P")
  (save-window-excursion
    (let* ((queried nil)
	   (files-done
	    (map-y-or-n-p
	     (function
	      (lambda (buffer)
		(and (buffer-modified-p buffer)
		     (not (buffer-base-buffer buffer))
		     (or
		      (buffer-file-name buffer)
		      (and pred
			   (progn
			     (set-buffer buffer)
			     (and buffer-offer-save (> (buffer-size) 0)))))
		     (or (not (functionp pred))
			 (with-current-buffer buffer (funcall pred)))
		     (if arg
			 t
		       (setq queried t)
		       (if (buffer-file-name buffer)
			   (format "Save file %s? "
				   (buffer-file-name buffer))
			 (format "Save buffer %s? "
				 (buffer-name buffer)))))))
	     (function
	      (lambda (buffer)
		(set-buffer buffer)
		(save-buffer)))
	     (buffer-list)
	     '("buffer" "buffers" "save")
	     (list (list ?\C-r (lambda (buf)
				 (view-buffer buf
					      (function
					       (lambda (ignore)
						 (exit-recursive-edit))))
				 (recursive-edit)
				 ;; Return nil to ask about BUF again.
				 nil)
			 "display the current buffer"))))
	   (abbrevs-done
	    (and save-abbrevs abbrevs-changed
		 (progn
		   (if (or arg
			   (eq save-abbrevs 'silently)
			   (y-or-n-p (format "Save abbrevs in %s? "
					     abbrev-file-name)))
		       (write-abbrev-file nil))
		   ;; Don't keep bothering user if he says no.
		   (setq abbrevs-changed nil)
		   t))))
      (or queried (> files-done 0) abbrevs-done
	  (message "(No files need saving)")))))

(defun not-modified (&optional arg)
  "Mark current buffer as unmodified, not needing to be saved.
With prefix arg, mark buffer as modified, so \\[save-buffer] will save.

It is not a good idea to use this function in Lisp programs, because it
prints a message in the minibuffer.  Instead, use `set-buffer-modified-p'."
  (interactive "P")
  (message (if arg "Modification-flag set"
	       "Modification-flag cleared"))
  (set-buffer-modified-p arg))

(defun toggle-read-only (&optional arg)
  "Change whether this buffer is visiting its file read-only.
With arg, set read-only iff arg is positive.
If visiting file read-only and `view-read-only' is non-nil, enter view mode."
  (interactive "P")
  (cond
   ((and arg (if (> (prefix-numeric-value arg) 0) buffer-read-only
	       (not buffer-read-only)))	; If buffer-read-only is set correctly,
    nil)				; do nothing.
   ;; Toggle.
   ((and buffer-read-only view-mode)
    (View-exit-and-edit)
    (make-local-variable 'view-read-only)
    (setq view-read-only t))		; Must leave view mode.
   ((and (not buffer-read-only) view-read-only
	 (not (eq (get major-mode 'mode-class) 'special)))
    (view-mode-enter))
   (t (setq buffer-read-only (not buffer-read-only))
      (force-mode-line-update))))

(defun insert-file (filename)
  "Insert contents of file FILENAME into buffer after point.
Set mark after the inserted text.

This function is meant for the user to run interactively.
Don't call it from programs!  Use `insert-file-contents' instead.
\(Its calling sequence is different; see its documentation)."
  (interactive "*fInsert file: ")
  (if (file-directory-p filename)
      (signal 'file-error (list "Opening input file" "file is a directory"
				filename)))
  (let ((tem (insert-file-contents filename)))
    (push-mark (+ (point) (car (cdr tem))))))

(defun append-to-file (start end filename)
  "Append the contents of the region to the end of file FILENAME.
When called from a function, expects three arguments,
START, END and FILENAME.  START and END are buffer positions
saying what text to write."
  (interactive "r\nFAppend to file: ")
  (write-region start end filename t))

(defun file-newest-backup (filename)
  "Return most recent backup file for FILENAME or nil if no backups exist."
  ;; `make-backup-file-name' will get us the right directory for
  ;; ordinary or numeric backups.  It might create a directory for
  ;; backups as a side-effect, according to `backup-directory-alist'.
  (let* ((filename (make-backup-file-name filename))
	 (file (file-name-nondirectory filename))
	 (dir  (file-name-directory    filename))
	 (comp (file-name-all-completions file dir))
         (newest nil)
         tem)
    (while comp
      (setq tem (pop comp))
      (cond ((and (backup-file-name-p tem)
                  (string= (file-name-sans-versions tem) file))
             (setq tem (concat dir tem))
             (if (or (null newest)
                     (file-newer-than-file-p tem newest))
                 (setq newest tem)))))
    newest))

(defun rename-uniquely ()
  "Rename current buffer to a similar name not already taken.
This function is useful for creating multiple shell process buffers
or multiple mail buffers, etc."
  (interactive)
  (save-match-data
    (let ((base-name (buffer-name)))
      (and (string-match "<[0-9]+>\\'" base-name)
	   (not (and buffer-file-name
		     (string= base-name
			      (file-name-nondirectory buffer-file-name))))
	   ;; If the existing buffer name has a <NNN>,
	   ;; which isn't part of the file name (if any),
	   ;; then get rid of that.
	   (setq base-name (substring base-name 0 (match-beginning 0))))
      (rename-buffer (generate-new-buffer-name base-name))
      (force-mode-line-update))))

(defun make-directory (dir &optional parents)
  "Create the directory DIR and any nonexistent parent dirs.
Interactively, the default choice of directory to create
is the current default directory for file names.
That is useful when you have visited a file in a nonexistent directory.

Noninteractively, the second (optional) argument PARENTS says whether
to create parent directories if they don't exist."
  (interactive
   (list (read-file-name "Make directory: " default-directory default-directory
			 nil nil)
	 t))
  (let ((handler (find-file-name-handler dir 'make-directory)))
    (if handler
	(funcall handler 'make-directory dir parents)
      (if (not parents)
	  (make-directory-internal dir)
	(let ((dir (directory-file-name (expand-file-name dir)))
	      create-list)
	  (while (not (file-exists-p dir))
	    (setq create-list (cons dir create-list)
		  dir (directory-file-name (file-name-directory dir))))
	  (while create-list
	    (make-directory-internal (car create-list))
	    (setq create-list (cdr create-list))))))))

(put 'revert-buffer-function 'permanent-local t)
(defvar revert-buffer-function nil
  "Function to use to revert this buffer, or nil to do the default.
The function receives two arguments IGNORE-AUTO and NOCONFIRM,
which are the arguments that `revert-buffer' received.")

(put 'revert-buffer-insert-file-contents-function 'permanent-local t)
(defvar revert-buffer-insert-file-contents-function nil
  "Function to use to insert contents when reverting this buffer.
Gets two args, first the nominal file name to use,
and second, t if reading the auto-save file.

The function you specify is responsible for updating (or preserving) point.")

(defvar before-revert-hook nil
  "Normal hook for `revert-buffer' to run before reverting.
If `revert-buffer-function' is used to override the normal revert
mechanism, this hook is not used.")

(defvar after-revert-hook nil
  "Normal hook for `revert-buffer' to run after reverting.
Note that the hook value that it runs is the value that was in effect
before reverting; that makes a difference if you have buffer-local
hook functions.

If `revert-buffer-function' is used to override the normal revert
mechanism, this hook is not used.")

(defvar revert-buffer-internal-hook)

(defun revert-buffer (&optional ignore-auto noconfirm preserve-modes)
  "Replace current buffer text with the text of the visited file on disk.
This undoes all changes since the file was visited or saved.
With a prefix argument, offer to revert from latest auto-save file, if
that is more recent than the visited file.

This command also works for special buffers that contain text which
doesn't come from a file, but reflects some other data base instead:
for example, Dired buffers and buffer-list buffers.  In these cases,
it reconstructs the buffer contents from the appropriate data base.

When called from Lisp, the first argument is IGNORE-AUTO; only offer
to revert from the auto-save file when this is nil.  Note that the
sense of this argument is the reverse of the prefix argument, for the
sake of backward compatibility.  IGNORE-AUTO is optional, defaulting
to nil.

Optional second argument NOCONFIRM means don't ask for confirmation at
all.  (The local variable `revert-without-query', if non-nil, prevents
confirmation.)

Optional third argument PRESERVE-MODES non-nil means don't alter
the files modes.  Normally we reinitialize them using `normal-mode'.

If the value of `revert-buffer-function' is non-nil, it is called to
do all the work for this command.  Otherwise, the hooks
`before-revert-hook' and `after-revert-hook' are run at the beginning
and the end, and if `revert-buffer-insert-file-contents-function' is
non-nil, it is called instead of rereading visited file contents."

  ;; I admit it's odd to reverse the sense of the prefix argument, but
  ;; there is a lot of code out there which assumes that the first
  ;; argument should be t to avoid consulting the auto-save file, and
  ;; there's no straightforward way to encourage authors to notice a
  ;; reversal of the argument sense.  So I'm just changing the user
  ;; interface, but leaving the programmatic interface the same.
  (interactive (list (not current-prefix-arg)))
  (if revert-buffer-function
      (funcall revert-buffer-function ignore-auto noconfirm)
    (let* ((auto-save-p (and (not ignore-auto)
			     (recent-auto-save-p)
			     buffer-auto-save-file-name
			     (file-readable-p buffer-auto-save-file-name)
			     (y-or-n-p
   "Buffer has been auto-saved recently.  Revert from auto-save file? ")))
	   (file-name (if auto-save-p
			  buffer-auto-save-file-name
			buffer-file-name)))
      (cond ((null file-name)
	     (error "Buffer does not seem to be associated with any file"))
	    ((or noconfirm
		 (and (not (buffer-modified-p))
		      (let ((tail revert-without-query)
			    (found nil))
			(while tail
			  (if (string-match (car tail) file-name)
			      (setq found t))
			  (setq tail (cdr tail)))
			found))
		 (yes-or-no-p (format "Revert buffer from file %s? "
				      file-name)))
	     (run-hooks 'before-revert-hook)
	     ;; If file was backed up but has changed since,
	     ;; we shd make another backup.
	     (and (not auto-save-p)
		  (not (verify-visited-file-modtime (current-buffer)))
		  (setq buffer-backed-up nil))
	     ;; Get rid of all undo records for this buffer.
	     (or (eq buffer-undo-list t)
		 (setq buffer-undo-list nil))
	     ;; Effectively copy the after-revert-hook status,
	     ;; since after-find-file will clobber it.
	     (let ((global-hook (default-value 'after-revert-hook))
		   (local-hook-p (local-variable-p 'after-revert-hook))
		   (local-hook (and (local-variable-p 'after-revert-hook)
				    after-revert-hook)))
	       (let (buffer-read-only
		     ;; Don't make undo records for the reversion.
		     (buffer-undo-list t))
		 (if revert-buffer-insert-file-contents-function
		     (funcall revert-buffer-insert-file-contents-function
			      file-name auto-save-p)
		   (if (not (file-exists-p file-name))
		       (error "File %s no longer exists!" file-name))
		   ;; Bind buffer-file-name to nil
		   ;; so that we don't try to lock the file.
		   (let ((buffer-file-name nil))
		     (or auto-save-p
			 (unlock-buffer)))
		   (widen)
		   (let ((coding-system-for-read
			  ;; Auto-saved file shoule be read without
			  ;; any code conversion.
			  (if auto-save-p 'emacs-mule-unix
			    coding-system-for-read)))
		     ;; Note that this preserves point in an intelligent way.
		     (insert-file-contents file-name (not auto-save-p)
					   nil nil t))))
	       ;; Recompute the truename in case changes in symlinks
	       ;; have changed the truename.
	       (setq buffer-file-truename
		     (abbreviate-file-name (file-truename buffer-file-name)))
	       (after-find-file nil nil t t preserve-modes)
	       ;; Run after-revert-hook as it was before we reverted.
	       (setq-default revert-buffer-internal-hook global-hook)
	       (if local-hook-p
		   (progn
		     (make-local-variable 'revert-buffer-internal-hook)
		     (setq revert-buffer-internal-hook local-hook))
		 (kill-local-variable 'revert-buffer-internal-hook))
	       (run-hooks 'revert-buffer-internal-hook))
	     t)))))

(defun recover-file (file)
  "Visit file FILE, but get contents from its last auto-save file."
  ;; Actually putting the file name in the minibuffer should be used
  ;; only rarely.
  ;; Not just because users often use the default.
  (interactive "FRecover file: ")
  (setq file (expand-file-name file))
  (if (auto-save-file-name-p (file-name-nondirectory file))
      (error "%s is an auto-save file" file))
  (let ((file-name (let ((buffer-file-name file))
		     (make-auto-save-file-name))))
    (cond ((if (file-exists-p file)
	       (not (file-newer-than-file-p file-name file))
	     (not (file-exists-p file-name)))
	   (error "Auto-save file %s not current" file-name))
	  ((save-window-excursion
	     (with-output-to-temp-buffer "*Directory*"
	       (buffer-disable-undo standard-output)
	       (save-excursion
		 (let ((switches dired-listing-switches))
		   (if (file-symlink-p file)
		       (setq switches (concat switches "L")))
		   (set-buffer standard-output)
		   ;; Use insert-directory-safely, not insert-directory,
		   ;; because these files might not exist.  In particular,
		   ;; FILE might not exist if the auto-save file was for
		   ;; a buffer that didn't visit a file, such as "*mail*".
		   ;; The code in v20.x called `ls' directly, so we need
		   ;; to emulate what `ls' did in that case.
		   (insert-directory-safely file switches)
		   (insert-directory-safely file-name switches))))
	     (yes-or-no-p (format "Recover auto save file %s? " file-name)))
	   (switch-to-buffer (find-file-noselect file t))
	   (let ((buffer-read-only nil)
		 ;; Keep the current buffer-file-coding-system.
		 (coding-system buffer-file-coding-system)
		 ;; Auto-saved file shoule be read without any code conversion.
		 (coding-system-for-read 'emacs-mule-unix))
	     (erase-buffer)
	     (insert-file-contents file-name nil)
	     (set-buffer-file-coding-system coding-system))
	   (after-find-file nil nil t))
	  (t (error "Recover-file cancelled")))))

(defun recover-session ()
  "Recover auto save files from a previous Emacs session.
This command first displays a Dired buffer showing you the
previous sessions that you could recover from.
To choose one, move point to the proper line and then type C-c C-c.
Then you'll be asked about a number of files to recover."
  (interactive)
  (if (null auto-save-list-file-prefix)
      (error "You set `auto-save-list-file-prefix' to disable making session files"))
  (let ((dir (file-name-directory auto-save-list-file-prefix)))
    (unless (file-directory-p dir)
      (make-directory dir t)))
  (let ((ls-lisp-support-shell-wildcards t))
    (dired (concat auto-save-list-file-prefix "*")
	   (concat dired-listing-switches "t")))
  (save-excursion
    (goto-char (point-min))
    (or (looking-at " Move to the session you want to recover,")
	(let ((inhibit-read-only t))
	  ;; Each line starts with a space
	  ;; so that Font Lock mode won't highlight the first character.
	  (insert " Move to the session you want to recover,\n"
		  " then type C-c C-c to select it.\n\n"
		  " You can also delete some of these files;\n"
		  " type d on a line to mark that file for deletion.\n\n"))))
  (use-local-map (nconc (make-sparse-keymap) (current-local-map)))
  (define-key (current-local-map) "\C-c\C-c" 'recover-session-finish))

(defun recover-session-finish ()
  "Choose one saved session to recover auto-save files from.
This command is used in the special Dired buffer created by
\\[recover-session]."
  (interactive)
  ;; Get the name of the session file to recover from.
  (let ((file (dired-get-filename))
	files
	(buffer (get-buffer-create " *recover*")))
    (dired-unmark 1)
    (dired-do-flagged-delete t)
    (unwind-protect
	(save-excursion
	  ;; Read in the auto-save-list file.
	  (set-buffer buffer)
	  (erase-buffer)
	  (insert-file-contents file)
	  ;; Loop thru the text of that file
	  ;; and get out the names of the files to recover.
	  (while (not (eobp))
	    (let (thisfile autofile)
	      (if (eolp)
		  ;; This is a pair of lines for a non-file-visiting buffer.
		  ;; Get the auto-save file name and manufacture
		  ;; a "visited file name" from that.
		  (progn
		    (forward-line 1)
		    (setq autofile
			  (buffer-substring-no-properties
			   (point)
			   (save-excursion
			     (end-of-line)
			     (point))))
		    (setq thisfile
			  (expand-file-name
			   (substring
			    (file-name-nondirectory autofile)
			    1 -1)
			   (file-name-directory autofile)))
		    (forward-line 1))
		;; This pair of lines is a file-visiting
		;; buffer.  Use the visited file name.
		(progn
		  (setq thisfile
			(buffer-substring-no-properties
			 (point) (progn (end-of-line) (point))))
		  (forward-line 1)
		  (setq autofile
			(buffer-substring-no-properties
			 (point) (progn (end-of-line) (point))))
		  (forward-line 1)))
	      ;; Ignore a file if its auto-save file does not exist now.
	      (if (file-exists-p autofile)
		  (setq files (cons thisfile files)))))
	  (setq files (nreverse files))
	  ;; The file contains a pair of line for each auto-saved buffer.
	  ;; The first line of the pair contains the visited file name
	  ;; or is empty if the buffer was not visiting a file.
	  ;; The second line is the auto-save file name.
	  (if files
	      (map-y-or-n-p  "Recover %s? "
			     (lambda (file)
			       (condition-case nil
				   (save-excursion (recover-file file))
				 (error
				  "Failed to recover `%s'" file)))
			     files
			     '("file" "files" "recover"))
	    (message "No files can be recovered from this session now")))
      (kill-buffer buffer))))

(defun kill-some-buffers (&optional list)
  "For each buffer in LIST, ask whether to kill it.
LIST defaults to all existing live buffers."
  (interactive)
  (if (null list)
      (setq list (buffer-list)))
  (while list
    (let* ((buffer (car list))
	   (name (buffer-name buffer)))
      (and (not (string-equal name ""))
	   (/= (aref name 0) ? )
	   (yes-or-no-p
	    (format "Buffer %s %s.  Kill? "
		    name
		    (if (buffer-modified-p buffer)
			"HAS BEEN EDITED" "is unmodified")))
	   (kill-buffer buffer)))
    (setq list (cdr list))))

(defun auto-save-mode (arg)
  "Toggle auto-saving of contents of current buffer.
With prefix argument ARG, turn auto-saving on if positive, else off."
  (interactive "P")
  (setq buffer-auto-save-file-name
        (and (if (null arg)
		 (or (not buffer-auto-save-file-name)
		     ;; If auto-save is off because buffer has shrunk,
		     ;; then toggling should turn it on.
		     (< buffer-saved-size 0))
	       (or (eq arg t) (listp arg) (and (integerp arg) (> arg 0))))
	     (if (and buffer-file-name auto-save-visited-file-name
		      (not buffer-read-only))
		 buffer-file-name
	       (make-auto-save-file-name))))
  ;; If -1 was stored here, to temporarily turn off saving,
  ;; turn it back on.
  (and (< buffer-saved-size 0)
       (setq buffer-saved-size 0))
  (if (interactive-p)
      (message "Auto-save %s (in this buffer)"
	       (if buffer-auto-save-file-name "on" "off")))
  buffer-auto-save-file-name)

(defun rename-auto-save-file ()
  "Adjust current buffer's auto save file name for current conditions.
Also rename any existing auto save file, if it was made in this session."
  (let ((osave buffer-auto-save-file-name))
    (setq buffer-auto-save-file-name
	  (make-auto-save-file-name))
    (if (and osave buffer-auto-save-file-name
	     (not (string= buffer-auto-save-file-name buffer-file-name))
	     (not (string= buffer-auto-save-file-name osave))
	     (file-exists-p osave)
	     (recent-auto-save-p))
	(rename-file osave buffer-auto-save-file-name t))))

(defun make-auto-save-file-name ()
  "Return file name to use for auto-saves of current buffer.
Does not consider `auto-save-visited-file-name' as that variable is checked
before calling this function.  You can redefine this for customization.
See also `auto-save-file-name-p'."
  (if buffer-file-name
      (let ((list auto-save-file-name-transforms)
	    (filename buffer-file-name)
	    result)
	;; Apply user-specified translations
	;; to the file name.
	(while (and list (not result))
	  (if (string-match (car (car list)) filename)
	      (setq result (replace-match (cadr (car list)) t nil
					  filename)))
	  (setq list (cdr list)))
	(if result (setq filename result))

	(setq result
	      (if (and (eq system-type 'ms-dos)
		       (not (msdos-long-file-names)))
		  ;; We truncate the file name to DOS 8+3 limits
		  ;; before doing anything else, because the regexp
		  ;; passed to string-match below cannot handle
		  ;; extensions longer than 3 characters, multiple
		  ;; dots, and other atrocities.
		  (let ((fn (dos-8+3-filename
			     (file-name-nondirectory buffer-file-name))))
		    (string-match
		     "\\`\\([^.]+\\)\\(\\.\\(..?\\)?.?\\|\\)\\'"
		     fn)
		    (concat (file-name-directory buffer-file-name)
			    "#" (match-string 1 fn)
			    "." (match-string 3 fn) "#"))
		(concat (file-name-directory filename)
			"#"
			(file-name-nondirectory filename)
			"#")))
	;; Make sure auto-save file names don't contain characters
	;; invalid for the underlying filesystem.
	(if (and (memq system-type '(ms-dos windows-nt))
		 ;; Don't modify remote (ange-ftp) filenames
		 (not (string-match "^/\\w+@[-A-Za-z0-9._]+:" result)))
	    (convert-standard-filename result)
	  result))

    ;; Deal with buffers that don't have any associated files.  (Mail
    ;; mode tends to create a good number of these.)

    (let ((buffer-name (buffer-name))
	  (limit 0))
      ;; Eliminate all slashes and backslashes by
      ;; replacing them with sequences that start with %.
      ;; Quote % also, to keep distinct names distinct.
      (while (string-match "[/\\%]" buffer-name limit)
	(let* ((character (aref buffer-name (match-beginning 0)))
	       (replacement
		(cond ((eq character ?%) "%%")
		      ((eq character ?/) "%+")
		      ((eq character ?\\) "%-"))))
	  (setq buffer-name (replace-match replacement t t buffer-name))
	  (setq limit (1+ (match-end 0)))))
      ;; Generate the file name.
      (make-temp-file
       (let ((fname
	      (expand-file-name
	       (format "#%s#" buffer-name)
	       ;; Try a few alternative directories, to get one we can
	       ;; write it.
	       (cond
		((file-writable-p default-directory) default-directory)
		((file-writable-p "/var/tmp/") "/var/tmp/")
		("~/")))))
	 (if (and (memq system-type '(ms-dos windows-nt))
		  ;; Don't modify remote (ange-ftp) filenames
		  (not (string-match "^/\\w+@[-A-Za-z0-9._]+:" fname)))
	     ;; The call to convert-standard-filename is in case
	     ;; buffer-name includes characters not allowed by the
	     ;; DOS/Windows filesystems.  make-temp-file writes to the
	     ;; file it creates, so we must fix the file name _before_
	     ;; make-temp-file is called.
	     (convert-standard-filename fname)
	   fname))))))

(defun auto-save-file-name-p (filename)
  "Return non-nil if FILENAME can be yielded by `make-auto-save-file-name'.
FILENAME should lack slashes.  You can redefine this for customization."
  (string-match "^#.*#$" filename))

(defun wildcard-to-regexp (wildcard)
  "Given a shell file name pattern WILDCARD, return an equivalent regexp.
The generated regexp will match a filename iff the filename
matches that wildcard according to shell rules.  Only wildcards known
by `sh' are supported."
  (let* ((i (string-match "[[.*+\\^$?]" wildcard))
	 ;; Copy the initial run of non-special characters.
	 (result (substring wildcard 0 i))
	 (len (length wildcard)))
    ;; If no special characters, we're almost done.
    (if i
	(while (< i len)
	  (let ((ch (aref wildcard i))
		j)
	    (setq
	     result
	     (concat result
		     (cond
		      ((and (eq ch ?\[)
			    (< (1+ i) len)
			    (eq (aref wildcard (1+ i)) ?\]))
		       "\\[")
		      ((eq ch ?\[)	; [...] maps to regexp char class
		       (progn
			 (setq i (1+ i))
			 (concat
			  (cond
			   ((eq (aref wildcard i) ?!) ; [!...] -> [^...]
			    (progn
			      (setq i (1+ i))
			      (if (eq (aref wildcard i) ?\])
				  (progn
				    (setq i (1+ i))
				    "[^]")
				"[^")))
			   ((eq (aref wildcard i) ?^)
			    ;; Found "[^".  Insert a `\0' character
			    ;; (which cannot happen in a filename)
			    ;; into the character class, so that `^'
			    ;; is not the first character after `[',
			    ;; and thus non-special in a regexp.
			    (progn
			      (setq i (1+ i))
			      "[\000^"))
			   ((eq (aref wildcard i) ?\])
			    ;; I don't think `]' can appear in a
			    ;; character class in a wildcard, but
			    ;; let's be general here.
			    (progn
			      (setq i (1+ i))
			      "[]"))
			   (t "["))
			  (prog1	; copy everything upto next `]'.
			      (substring wildcard
					 i
					 (setq j (string-match
						  "]" wildcard i)))
			    (setq i (if j (1- j) (1- len)))))))
		      ((eq ch ?.)  "\\.")
		      ((eq ch ?*)  "[^\000]*")
		      ((eq ch ?+)  "\\+")
		      ((eq ch ?^)  "\\^")
		      ((eq ch ?$)  "\\$")
		      ((eq ch ?\\) "\\\\") ; probably cannot happen...
		      ((eq ch ??)  "[^\000]")
		      (t (char-to-string ch)))))
	    (setq i (1+ i)))))
    ;; Shell wildcards should match the entire filename,
    ;; not its part.  Make the regexp say so.
    (concat "\\`" result "\\'")))

(defcustom list-directory-brief-switches
  (if (eq system-type 'vax-vms) "" "-CF")
  "*Switches for `list-directory' to pass to `ls' for brief listing."
  :type 'string
  :group 'dired)

(defcustom list-directory-verbose-switches
  (if (eq system-type 'vax-vms)
      "/PROTECTION/SIZE/DATE/OWNER/WIDTH=(OWNER:10)"
    "-l")
  "*Switches for `list-directory' to pass to `ls' for verbose listing."
  :type 'string
  :group 'dired)

(defun file-expand-wildcards (pattern &optional full)
  "Expand wildcard pattern PATTERN.
This returns a list of file names which match the pattern.

If PATTERN is written as an absolute relative file name,
the values are absolute also.

If PATTERN is written as a relative file name, it is interpreted
relative to the current default directory, `default-directory'.
The file names returned are normally also relative to the current
default directory.  However, if FULL is non-nil, they are absolute."
  (save-match-data
    (let* ((nondir (file-name-nondirectory pattern))
	   (dirpart (file-name-directory pattern))
	   ;; A list of all dirs that DIRPART specifies.
	   ;; This can be more than one dir
	   ;; if DIRPART contains wildcards.
	   (dirs (if (and dirpart (string-match "[[*?]" dirpart))
		     (mapcar 'file-name-as-directory
			     (file-expand-wildcards (directory-file-name dirpart)))
		   (list dirpart)))
	   contents)
      (while dirs
	(when (or (null (car dirs))	; Possible if DIRPART is not wild.
		  (file-directory-p (directory-file-name (car dirs))))
	  (let ((this-dir-contents
		 ;; Filter out "." and ".."
		 (delq nil
		       (mapcar #'(lambda (name)
				   (unless (string-match "\\`\\.\\.?\\'"
							 (file-name-nondirectory name))
				     name))
			       (directory-files (or (car dirs) ".") full
						(wildcard-to-regexp nondir))))))
	    (setq contents
		  (nconc
		   (if (and (car dirs) (not full))
		       (mapcar (function (lambda (name) (concat (car dirs) name)))
			       this-dir-contents)
		     this-dir-contents)
		   contents))))
	(setq dirs (cdr dirs)))
      contents)))

(defun list-directory (dirname &optional verbose)
  "Display a list of files in or matching DIRNAME, a la `ls'.
DIRNAME is globbed by the shell if necessary.
Prefix arg (second arg if noninteractive) means supply -l switch to `ls'.
Actions controlled by variables `list-directory-brief-switches'
and `list-directory-verbose-switches'."
  (interactive (let ((pfx current-prefix-arg))
		 (list (read-file-name (if pfx "List directory (verbose): "
					 "List directory (brief): ")
				       nil default-directory nil)
		       pfx)))
  (let ((switches (if verbose list-directory-verbose-switches
		    list-directory-brief-switches))
	buffer)
    (or dirname (setq dirname default-directory))
    (setq dirname (expand-file-name dirname))
    (with-output-to-temp-buffer "*Directory*"
      (setq buffer standard-output)
      (buffer-disable-undo standard-output)
      (princ "Directory ")
      (princ dirname)
      (terpri)
      (save-excursion
	(set-buffer "*Directory*")
	(let ((wildcard (not (file-directory-p dirname))))
	  (insert-directory dirname switches wildcard (not wildcard)))))
    ;; Finishing with-output-to-temp-buffer seems to clobber default-directory.
    (with-current-buffer buffer
      (setq default-directory
	    (if (file-directory-p dirname)
		(file-name-as-directory dirname)
	      (file-name-directory dirname))))))

(defun shell-quote-wildcard-pattern (pattern)
  "Quote characters special to the shell in PATTERN, leave wildcards alone.

PATTERN is assumed to represent a file-name wildcard suitable for the
underlying filesystem.  For Unix and GNU/Linux, the characters from the
set [ \\t\\n;<>&|()#$] are quoted with a backslash; for DOS/Windows, all
the parts of the pattern which don't include wildcard characters are
quoted with double quotes.
Existing quote characters in PATTERN are left alone, so you can pass
PATTERN that already quotes some of the special characters."
  (save-match-data
    (cond
     ((memq system-type '(ms-dos windows-nt))
      ;; DOS/Windows don't allow `"' in file names.  So if the
      ;; argument has quotes, we can safely assume it is already
      ;; quoted by the caller.
      (if (or (string-match "[\"]" pattern)
	      ;; We quote [&()#$'] in case their shell is a port of a
	      ;; Unixy shell.  We quote [,=+] because stock DOS and
	      ;; Windows shells require that in some cases, such as
	      ;; passing arguments to batch files that use positional
	      ;; arguments like %1.
	      (not (string-match "[ \t;&()#$',=+]" pattern)))
	  pattern
	(let ((result "\"")
	      (beg 0)
	      end)
	  (while (string-match "[*?]+" pattern beg)
	    (setq end (match-beginning 0)
		  result (concat result (substring pattern beg end)
				 "\""
				 (substring pattern end (match-end 0))
				 "\"")
		  beg (match-end 0)))
	  (concat result (substring pattern beg) "\""))))
     (t
      (let ((beg 0))
	(while (string-match "[ \t\n;<>&|()#$]" pattern beg)
	  (setq pattern
		(concat (substring pattern 0 (match-beginning 0))
			"\\"
			(substring pattern (match-beginning 0)))
		beg (1+ (match-end 0)))))
      pattern))))


(defvar insert-directory-program "ls"
  "Absolute or relative name of the `ls' program used by `insert-directory'.")

(defcustom directory-free-space-program "df"
  "*Program to get the amount of free space on a file system.
We assume the output has the format of `df'.
The value of this variable must be just a command name or file name;
if you want to specify options, use `directory-free-space-args'.

A value of nil disables this feature.

If the function `file-system-info' is defined, it is always used in
preference to the program given by this variable."
  :type '(choice (string :tag "Program") (const :tag "None" nil))
  :group 'dired)

(defcustom directory-free-space-args "-Pk"
  "*Options to use when running `directory-free-space-program'."
  :type 'string
  :group 'dired)

(defun get-free-disk-space (dir)
  "Return the mount of free space on directory DIR's file system.
The result is a string that gives the number of free 1KB blocks,
or nil if the system call or the program which retrieve the infornmation
fail.

This function calls `file-system-info' if it is available, or invokes the
program specified by `directory-free-space-program' if that is non-nil."
  ;; Try to find the number of free blocks.  Non-Posix systems don't
  ;; always have df, but might have an equivalent system call.
  (if (fboundp 'file-system-info)
      (let ((fsinfo (file-system-info dir)))
	(if fsinfo
	    (format "%.0f" (/ (nth 2 fsinfo) 1024))))
    (save-match-data
      (with-temp-buffer
	(when (and directory-free-space-program
		   (zerop (call-process directory-free-space-program
					nil t nil
					directory-free-space-args
					dir)))
	  ;; Usual format is a header line followed by a line of
	  ;; numbers.
	  (goto-char (point-min))
	  (forward-line 1)
	  (if (not (eobp))
	      (progn
		;; Move to the end of the "available blocks" number.
		(skip-chars-forward "^ \t")
		(forward-word 3)
		;; Copy it into AVAILABLE.
		(let ((end (point)))
		  (forward-word -1)
		  (buffer-substring (point) end)))))))))


;; insert-directory
;; - must insert _exactly_one_line_ describing FILE if WILDCARD and
;;   FULL-DIRECTORY-P is nil.
;;   The single line of output must display FILE's name as it was
;;   given, namely, an absolute path name.
;; - must insert exactly one line for each file if WILDCARD or
;;   FULL-DIRECTORY-P is t, plus one optional "total" line
;;   before the file lines, plus optional text after the file lines.
;;   Lines are delimited by "\n", so filenames containing "\n" are not
;;   allowed.
;;   File lines should display the basename.
;; - must be consistent with
;;   - functions dired-move-to-filename, (these two define what a file line is)
;;   		 dired-move-to-end-of-filename,
;;		 dired-between-files, (shortcut for (not (dired-move-to-filename)))
;;   		 dired-insert-headerline
;;   		 dired-after-subdir-garbage (defines what a "total" line is)
;;   - variable dired-subdir-regexp
(defun insert-directory (file switches &optional wildcard full-directory-p)
  "Insert directory listing for FILE, formatted according to SWITCHES.
Leaves point after the inserted text.
SWITCHES may be a string of options, or a list of strings.
Optional third arg WILDCARD means treat FILE as shell wildcard.
Optional fourth arg FULL-DIRECTORY-P means file is a directory and
switches do not contain `d', so that a full listing is expected.

This works by running a directory listing program
whose name is in the variable `insert-directory-program'.
If WILDCARD, it also runs the shell specified by `shell-file-name'."
  ;; We need the directory in order to find the right handler.
  (let ((handler (find-file-name-handler (expand-file-name file)
					 'insert-directory)))
    (if handler
	(funcall handler 'insert-directory file switches
		 wildcard full-directory-p)
      (if (eq system-type 'vax-vms)
	  (vms-read-directory file switches (current-buffer))
	(let (result available)

	  ;; Read the actual directory using `insert-directory-program'.
	  ;; RESULT gets the status code.
	  (let ((coding-system-for-read
		 (and enable-multibyte-characters
		      (or file-name-coding-system
			  default-file-name-coding-system)))
		;; This is to control encoding the arguments in call-process.
		(coding-system-for-write coding-system-for-read))
	    (setq result
		  (if wildcard
		      ;; Run ls in the directory part of the file pattern
		      ;; using the last component as argument.
		      (let ((default-directory
			      (if (file-name-absolute-p file)
				  (file-name-directory file)
				(file-name-directory (expand-file-name file))))
			    (pattern (file-name-nondirectory file)))
			(call-process
			 shell-file-name nil t nil
			 "-c"
			 (concat (if (memq system-type '(ms-dos windows-nt))
				     ""
				   "\\") ; Disregard Unix shell aliases!
				 insert-directory-program
				 " -d "
				 (if (stringp switches)
				     switches
				   (mapconcat 'identity switches " "))
				 " -- "
				 ;; Quote some characters that have
				 ;; special meanings in shells; but
				 ;; don't quote the wildcards--we want
				 ;; them to be special.  We also
				 ;; currently don't quote the quoting
				 ;; characters in case people want to
				 ;; use them explicitly to quote
				 ;; wildcard characters.
				 (shell-quote-wildcard-pattern pattern))))
		    ;; SunOS 4.1.3, SVr4 and others need the "." to list the
		    ;; directory if FILE is a symbolic link.
		    (apply 'call-process
			   insert-directory-program nil t nil
			   (append
			    (if (listp switches) switches
			      (unless (equal switches "")
				;; Split the switches at any spaces so we can
				;; pass separate options as separate args.
				(split-string switches)))
			    ;; Avoid lossage if FILE starts with `-'.
			    '("--")
			    (progn
			      (if (string-match "\\`~" file)
				  (setq file (expand-file-name file)))
			      (list
			       (if full-directory-p
				   (concat (file-name-as-directory file) ".")
				 file))))))))

	  ;; If `insert-directory-program' failed, signal an error.
	  (if (/= result 0)
	      ;; On non-Posix systems, we cannot open a directory, so
	      ;; don't even try, because that will always result in
	      ;; the ubiquitous "Access denied".  Instead, show the
	      ;; command line so the user can try to guess what went wrong.
	      (if (and (file-directory-p file)
		       (memq system-type '(ms-dos windows-nt)))
		  (error
		   "Reading directory: \"%s %s -- %s\" exited with status %s"
		   insert-directory-program
		   (if (listp switches) (concat switches) switches)
		   file result)
		;; Unix.  Access the file to get a suitable error.
		(access-file file "Reading directory")
		(error "Listing directory failed but `access-file' worked")))

	  ;; Try to insert the amount of free space.
	  (save-excursion
	    (goto-char (point-min))
	    ;; First find the line to put it on.
	    (when (re-search-forward "^total" nil t)
	      (let ((available (get-free-disk-space ".")))
		(when available
		  ;; Replace "total" with "used", to avoid confusion.
		  (replace-match "total used in directory")
		  (end-of-line)
		  (insert " available " available))))))))))

(defun insert-directory-safely (file switches
				     &optional wildcard full-directory-p)
  "Insert directory listing for FILE, formatted according to SWITCHES.

Like `insert-directory', but if FILE does not exist, it inserts a
message to that effect instead of signaling an error."
  (if (file-exists-p file)
      (insert-directory file switches wildcard full-directory-p)
    ;; Simulate the message printed by `ls'.
    (insert (format "%s: No such file or directory\n" file))))

(defvar kill-emacs-query-functions nil
  "Functions to call with no arguments to query about killing Emacs.
If any of these functions returns nil, killing Emacs is cancelled.
`save-buffers-kill-emacs' (\\[save-buffers-kill-emacs]) calls these functions,
but `kill-emacs', the low level primitive, does not.
See also `kill-emacs-hook'.")

(defcustom confirm-kill-emacs nil
  "How to ask for confirmation when leaving Emacs.
If nil, the default, don't ask at all.  If the value is non-nil, it should
be a predicate function such as `yes-or-no-p'."
  :type '(choice (const :tag "Ask with yes-or-no-p" yes-or-no-p)
		 (const :tag "Ask with y-or-n-p" y-or-n-p)
		 (const :tag "Don't confirm" nil))
  :group 'emacs
  :version "21.1")

(defun save-buffers-kill-emacs (&optional arg)
  "Offer to save each buffer, then kill this Emacs process.
With prefix arg, silently save all file-visiting buffers, then kill."
  (interactive "P")
  (save-some-buffers arg t)
  (and (or (not (memq t (mapcar (function
				  (lambda (buf) (and (buffer-file-name buf)
						     (buffer-modified-p buf))))
				(buffer-list))))
	   (yes-or-no-p "Modified buffers exist; exit anyway? "))
       (or (not (fboundp 'process-list))
	   ;; process-list is not defined on VMS.
	   (let ((processes (process-list))
		 active)
	     (while processes
	       (and (memq (process-status (car processes)) '(run stop open listen))
		    (process-query-on-exit-flag (car processes))
		    (setq active t))
	       (setq processes (cdr processes)))
	     (or (not active)
		 (list-processes t)
		 (yes-or-no-p "Active processes exist; kill them and exit anyway? "))))
       ;; Query the user for other things, perhaps.
       (run-hook-with-args-until-failure 'kill-emacs-query-functions)
       (or (null confirm-kill-emacs)
	   (funcall confirm-kill-emacs "Really exit Emacs? "))
       (kill-emacs)))

;; We use /: as a prefix to "quote" a file name
;; so that magic file name handlers will not apply to it.

(setq file-name-handler-alist
      (cons '("\\`/:" . file-name-non-special)
	    file-name-handler-alist))

;; We depend on being the last handler on the list,
;; so that anything else which does need handling
;; has been handled already.
;; So it is safe for us to inhibit *all* magic file name handlers.

(defun file-name-non-special (operation &rest arguments)
  (let ((file-name-handler-alist nil)
	(default-directory
	  (if (eq operation 'insert-directory)
	      (directory-file-name
	       (expand-file-name
		(unhandled-file-name-directory default-directory)))
	    default-directory))
	;; Get a list of the indices of the args which are file names.
	(file-arg-indices
	 (cdr (or (assq operation
			;; The first five are special because they
			;; return a file name.  We want to include the /:
			;; in the return value.
			;; So just avoid stripping it in the first place.
			'((expand-file-name . nil)
			  (file-name-directory . nil)
			  (file-name-as-directory . nil)
			  (directory-file-name . nil)
			  (file-name-sans-versions . nil)
			  ;; `identity' means just return the first arg
			  ;; as stripped of its quoting.
			  (substitute-in-file-name . identity)
			  (file-name-completion 0 1)
			  (file-name-all-completions 0 1)
			  (rename-file 0 1)
			  (copy-file 0 1)
			  (make-symbolic-link 0 1)
			  (add-name-to-file 0 1)))
		  ;; For all other operations, treat the first argument only
		  ;; as the file name.
		  '(nil 0))))
	;; Copy ARGUMENTS so we can replace elements in it.
	(arguments (copy-sequence arguments)))
    ;; Strip off the /: from the file names that have this handler.
    (save-match-data
      (while (consp file-arg-indices)
	(let ((pair (nthcdr (car file-arg-indices) arguments)))
	  (and (car pair)
	       (string-match "\\`/:" (car pair))
	       (setcar pair
		       (if (= (length (car pair)) 2)
			   "/"
			 (substring (car pair) 2)))))
	(setq file-arg-indices (cdr file-arg-indices))))
    (if (eq file-arg-indices 'identity)
	(car arguments)
      (let ((value (apply operation arguments)))
	(cond ((memq operation '(file-name-completion))
	       (and value (if (eq value t) t (concat "/:" value))))
	      ((memq operation '(file-name-all-completions))
	       (mapcar (lambda (name) (concat "/:" name)) value))
	      (t value))))))

(define-key ctl-x-map "\C-f" 'find-file)
(define-key ctl-x-map "\C-r" 'find-file-read-only)
(define-key ctl-x-map "\C-v" 'find-alternate-file)
(define-key ctl-x-map "\C-s" 'save-buffer)
(define-key ctl-x-map "s" 'save-some-buffers)
(define-key ctl-x-map "\C-w" 'write-file)
(define-key ctl-x-map "i" 'insert-file)
(define-key esc-map "~" 'not-modified)
(define-key ctl-x-map "\C-d" 'list-directory)
(define-key ctl-x-map "\C-c" 'save-buffers-kill-emacs)

(define-key ctl-x-4-map "f" 'find-file-other-window)
(define-key ctl-x-4-map "r" 'find-file-read-only-other-window)
(define-key ctl-x-4-map "\C-f" 'find-file-other-window)
(define-key ctl-x-4-map "b" 'switch-to-buffer-other-window)
(define-key ctl-x-4-map "\C-o" 'display-buffer)

(define-key ctl-x-5-map "b" 'switch-to-buffer-other-frame)
(define-key ctl-x-5-map "f" 'find-file-other-frame)
(define-key ctl-x-5-map "\C-f" 'find-file-other-frame)
(define-key ctl-x-5-map "r" 'find-file-read-only-other-frame)

;;; files.el ends here
