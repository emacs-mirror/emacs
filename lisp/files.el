;;; files.el --- file input and output commands for Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 1985-1987, 1992-2022 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
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

;; Defines most of Emacs's file- and directory-handling functions,
;; including basic file visiting, backup generation, link handling,
;; ITS-id version control, load- and write-hook handling, and the like.

;;; Code:

(eval-when-compile
  (require 'pcase)
  (require 'easy-mmode)) ; For `define-minor-mode'.

(defvar font-lock-keywords)

(defgroup backup nil
  "Backups of edited data files."
  :group 'files)

(defgroup find-file nil
  "Finding files."
  :group 'files)

(defcustom directory-abbrev-alist
  nil
  "Alist of abbreviations for file directories.
A list of elements of the form (FROM . TO), each meaning to replace
a match for FROM with TO when a directory name matches FROM.  This
replacement is done when setting up the default directory of a
newly visited file buffer.

FROM is a regexp that is matched against directory names anchored at
the first character, so it should start with a \"\\\\\\=`\", or, if
directory names cannot have embedded newlines, with a \"^\".

FROM and TO should be equivalent names, which refer to the
same directory.  TO should be an absolute directory name.
Do not use `~' in the TO strings.

Use this feature when you have directories that you normally refer to
via absolute symbolic links.  Make TO the name of the link, and FROM
a regexp matching the name it is linked to."
  :type '(repeat (cons :format "%v"
		       :value ("\\`" . "")
		       (regexp :tag "From")
		       (string :tag "To")))
  :group 'abbrev
  :group 'find-file)

(defcustom make-backup-files t
  "Non-nil means make a backup of a file the first time it is saved.
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
  "If non-nil, backups will be inhibited.
This variable is intended for use by making it local to a buffer,
but it is not an automatically buffer-local variable.")
(put 'backup-inhibited 'permanent-local t)

(defcustom backup-by-copying nil
 "Non-nil means always use copying to create backup files.
See documentation of variable `make-backup-files'."
  :type 'boolean
  :group 'backup)

(defcustom backup-by-copying-when-linked nil
 "Non-nil means use copying to create backups for files with multiple names.
This causes the alternate names to refer to the latest version as edited.
This variable is relevant only if `backup-by-copying' is nil."
  :type 'boolean
  :group 'backup)

(defcustom backup-by-copying-when-mismatch t
  "Non-nil means create backups by copying if this preserves owner or group.
Renaming may still be used (subject to control of other variables)
when it would not result in changing the owner or group of the file;
that is, for files that are owned by you and whose group matches
the default for a new file created there by you.
This variable is relevant only if `backup-by-copying' is nil."
  :version "24.1"
  :type 'boolean
  :group 'backup)
(put 'backup-by-copying-when-mismatch 'permanent-local t)

(defcustom backup-by-copying-when-privileged-mismatch 200
  "Non-nil means create backups by copying to preserve a privileged owner.
Renaming may still be used (subject to control of other variables)
when it would not result in changing the owner of the file or if the
user id and group id of the file are both greater than the value of
this variable.  This is useful when low-numbered uid's and gid's are
used for special system users (such as root) that must maintain
ownership of certain files.
This variable is relevant only if `backup-by-copying' and
`backup-by-copying-when-mismatch' are nil."
  :type '(choice (const nil) integer)
  :group 'backup)

(defvar backup-enable-predicate 'normal-backup-enable-predicate
  "Predicate that looks at a file name and decides whether to make backups.
Called with an absolute file name as argument, it returns t to enable backup.")

(defcustom buffer-offer-save nil
  "Non-nil in a buffer means always offer to save buffer on exiting Emacs.
Do so even if the buffer is not visiting a file.
Automatically local in all buffers.

Set to the symbol `always' to offer to save buffer whenever
`save-some-buffers' is called.

Note that this option has no effect on `kill-buffer';
if you want to control what happens when a buffer is killed,
use `kill-buffer-query-functions'."
  :type '(choice (const :tag "Never" nil)
                 (const :tag "On Emacs exit" t)
                 (const :tag "Whenever save-some-buffers is called" always))
  :group 'backup)
(make-variable-buffer-local 'buffer-offer-save)
(put 'buffer-offer-save 'permanent-local t)

(defcustom find-file-existing-other-name t
  "Non-nil means find a file under alternative names, in existing buffers.
This means if any existing buffer is visiting the file you want
under another name, you get the existing buffer instead of a new buffer."
  :type 'boolean
  :group 'find-file)

(defcustom find-file-visit-truename nil
  "Non-nil means visiting a file uses its truename as the visited-file name.
That is, the buffer visiting the file has the truename as the
value of `buffer-file-name'.  The truename of a file is found by
chasing all links both at the file level and at the levels of the
containing directories."
  :type 'boolean
  :group 'find-file)

(defcustom revert-without-query nil
  "Specify which files should be reverted without query.
The value is a list of regular expressions.
If the file name matches one of these regular expressions,
then `revert-buffer' reverts the file without querying
if the file has changed on disk and you have not edited the buffer."
  :type '(repeat regexp)
  :group 'find-file)

(defvar-local buffer-file-number nil
  "The device number and file number of the file visited in the current buffer.
The value is a list of the form (FILENUM DEVNUM).
This pair of numbers uniquely identifies the file.
If the buffer is visiting a new file, the value is nil.")
(put 'buffer-file-number 'permanent-local t)

(defvar buffer-file-numbers-unique (not (memq system-type '(windows-nt)))
  "Non-nil means that `buffer-file-number' uniquely identifies files.")

(defvar-local buffer-file-read-only nil
  "Non-nil if visited file was read-only when visited.")

(defcustom small-temporary-file-directory
  (if (eq system-type 'ms-dos) (getenv "TMPDIR"))
  "The directory for writing small temporary files.
If non-nil, this directory is used instead of `temporary-file-directory'
by programs that create small temporary files.  This is for systems that
have fast storage with limited space, such as a RAM disk."
  :group 'files
  :initialize 'custom-initialize-delay
  :type '(choice (const nil) directory))

;; The system null device. (Should reference NULL_DEVICE from C.)
(defvar null-device (purecopy "/dev/null") "The system null device.")

(declare-function msdos-long-file-names "msdos.c")
(declare-function w32-long-file-name "w32proc.c")
(declare-function dired-get-filename "dired" (&optional localp no-error-if-not-filep))
(declare-function dired-unmark "dired" (arg &optional interactive))
(declare-function dired-do-flagged-delete "dired" (&optional nomessage))
(declare-function dos-8+3-filename "dos-fns" (filename))
(declare-function dosified-file-name "dos-fns" (file-name))

(defvar file-name-invalid-regexp
  (cond ((and (eq system-type 'ms-dos) (not (msdos-long-file-names)))
	 (purecopy
	 (concat "^\\([^A-Z[-`a-z]\\|..+\\)?:\\|" ; colon except after drive
		 "[+, ;=|<>\"?*]\\|\\[\\|\\]\\|"  ; invalid characters
		 "[\000-\037]\\|"		  ; control characters
		 "\\(/\\.\\.?[^/]\\)\\|"	  ; leading dots
		 "\\(/[^/.]+\\.[^/.]*\\.\\)")))	  ; more than a single dot
	((memq system-type '(ms-dos windows-nt cygwin))
	 (purecopy
	 (concat "^\\([^A-Z[-`a-z]\\|..+\\)?:\\|" ; colon except after drive
		 "[|<>\"?*\000-\037]")))		  ; invalid characters
	(t (purecopy "[\000]")))
  "Regexp recognizing file names that aren't allowed by the filesystem.")

(defcustom file-precious-flag nil
  "Non-nil means protect against I/O errors while saving files.
Some modes set this non-nil in particular buffers.

This feature works by writing the new contents into a temporary file
and then renaming the temporary file to replace the original.
In this way, any I/O error in writing leaves the original untouched,
and there is never any instant where the file is nonexistent.

Note that this feature forces backups to be made by copying.
Yet, at the same time, saving a precious file
breaks any hard links between it and other files.

This feature is advisory: for example, if the directory in which the
file is being saved is not writable, Emacs may ignore a non-nil value
of `file-precious-flag' and write directly into the file.

See also: `break-hardlink-on-save' and `file-preserve-symlinks-on-save'."
  :type 'boolean
  :group 'backup)

(defcustom break-hardlink-on-save nil
  "Whether to allow breaking hardlinks when saving files.
If non-nil, then when saving a file that exists under several
names \(i.e., has multiple hardlinks), break the hardlink
associated with `buffer-file-name' and write to a new file, so
that the other instances of the file are not affected by the
save.

If `buffer-file-name' refers to a symlink, do not break the symlink.

Unlike `file-precious-flag', `break-hardlink-on-save' is not advisory.
For example, if the directory in which a file is being saved is not
itself writable, then error instead of saving in some
hardlink-nonbreaking way.

See also `backup-by-copying' and `backup-by-copying-when-linked'."
  :type 'boolean
  :group 'files
  :version "23.1")

(defcustom version-control nil
  "Control use of version numbers for backup files.
When t, make numeric backup versions unconditionally.
When nil, make them for files that have some already.
The value `never' means do not make them."
  :type '(choice (const :tag "Never" never)
		 (const :tag "If existing" nil)
		 (other :tag "Always" t))
  :group 'backup)

(defun version-control-safe-local-p (x)
  "Return whether X is safe as local value for `version-control'."
  (or (booleanp x) (equal x 'never)))

(put 'version-control 'safe-local-variable
     #'version-control-safe-local-p)

(defcustom dired-kept-versions 2
  "When cleaning directory, number of versions to keep."
  :type 'integer
  :group 'backup
  :group 'dired)

(defcustom delete-old-versions nil
  "If t, delete excess backup versions silently.
If nil, ask confirmation.  Any other value prevents any trimming."
  :type '(choice (const :tag "Delete" t)
		 (const :tag "Ask" nil)
		 (other :tag "Leave" other))
  :group 'backup)

(defcustom kept-old-versions 2
  "Number of oldest versions to keep when a new numbered backup is made."
  :type 'integer
  :group 'backup)
(put 'kept-old-versions 'safe-local-variable 'integerp)

(defcustom kept-new-versions 2
  "Number of newest versions to keep when a new numbered backup is made.
Includes the new backup.  Must be greater than 0."
  :type 'integer
  :group 'backup)
(put 'kept-new-versions 'safe-local-variable 'integerp)

(defcustom require-final-newline nil
  "Whether to add a newline automatically at the end of the file.

A value of t means do this only when the file is about to be saved.
A value of `visit' means do this right after the file is visited.
A value of `visit-save' means do it at both of those times.
Any other non-nil value means ask user whether to add a newline, when saving.
A value of nil means don't add newlines.

Certain major modes set this locally to the value obtained
from `mode-require-final-newline'.

This variable is heeded only when visiting files (or saving
buffers into files they visit).  Writing data to the file system
with `write-region' and the like is not influenced by this variable."
  :safe #'symbolp
  :type '(choice (const :tag "When visiting" visit)
		 (const :tag "When saving" t)
		 (const :tag "When visiting or saving" visit-save)
		 (const :tag "Don't add newlines" nil)
		 (other :tag "Ask each time" ask))
  :group 'editing-basics)

(defcustom mode-require-final-newline t
  "Whether to add a newline at end of file, in certain major modes.
Those modes set `require-final-newline' to this value when you enable them.
They do so because they are often used for files that are supposed
to end in newlines, and the question is how to arrange that.

A value of t means do this only when the file is about to be saved.
A value of `visit' means do this right after the file is visited.
A value of `visit-save' means do it at both of those times.
Any other non-nil value means ask user whether to add a newline, when saving.

A value of nil means do not add newlines.  That is a risky choice in this
variable since this value is used for modes for files that ought to have
final newlines.  So if you set this to nil, you must explicitly check and
add a final newline, whenever you save a file that really needs one."
  :type '(choice (const :tag "When visiting" visit)
		 (const :tag "When saving" t)
		 (const :tag "When visiting or saving" visit-save)
		 (const :tag "Don't add newlines" nil)
		 (other :tag "Ask each time" ask))
  :group 'editing-basics
  :version "22.1")

(defcustom auto-save-default t
  "Non-nil says by default do auto-saving of every file-visiting buffer."
  :type 'boolean
  :group 'auto-save)

(defcustom auto-save-file-name-transforms
  `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
     ;; Don't put "\\2" inside expand-file-name, since it will be
     ;; transformed to "/2" on DOS/Windows.
     ,(concat temporary-file-directory "\\2") t))
  "Transforms to apply to buffer file name before making auto-save file name.
Each transform is a list (REGEXP REPLACEMENT UNIQUIFY):
REGEXP is a regular expression to match against the file name.
If it matches, `replace-match' is used to replace the
matching part with REPLACEMENT.
If the optional element UNIQUIFY is non-nil, the auto-save file name is
constructed by taking the directory part of the replaced file-name,
concatenated with the buffer file name with all directory separators
changed to `!' to prevent clashes.  This will not work
correctly if your filesystem truncates the resulting name.
If UNIQUIFY is one of the members of `secure-hash-algorithms',
Emacs constructs the nondirectory part of the auto-save file name
by applying that `secure-hash' to the buffer file name.  This
avoids any risk of excessively long file names.

All the transforms in the list are tried, in the order they are listed.
When one transform applies, its result is final;
no further transforms are tried.

The default value is set up to put the auto-save file into the
temporary directory (see the variable `temporary-file-directory') for
editing a remote file.

On MS-DOS filesystems without long names this variable is always
ignored."
  :group 'auto-save
  :type '(repeat (list (regexp :tag "Regexp") (string :tag "Replacement")
					   (boolean :tag "Uniquify")))
  :initialize 'custom-initialize-delay
  :version "21.1")

(defvar auto-save--timer nil "Timer for `auto-save-visited-mode'.")

(defcustom auto-save-visited-interval 5
  "Interval in seconds for `auto-save-visited-mode'.
If `auto-save-visited-mode' is enabled, Emacs will save all
buffers visiting a file to the visited file after it has been
idle for `auto-save-visited-interval' seconds."
  :group 'auto-save
  :type 'number
  :version "26.1"
  :set (lambda (symbol value)
         (set-default symbol value)
         (when auto-save--timer
           (timer-set-idle-time auto-save--timer value :repeat))))

(define-minor-mode auto-save-visited-mode
  "Toggle automatic saving to file-visiting buffers on or off.

Unlike `auto-save-mode', this mode will auto-save buffer contents
to the visited files directly and will also run all save-related
hooks.  See Info node `Saving' for details of the save process.

You can also set the buffer-local value of the variable
`auto-save-visited-mode' to nil.  A buffer where the buffer-local
value of this variable is nil is ignored for the purpose of
`auto-save-visited-mode', even if `auto-save-visited-mode' is
enabled."
  :group 'auto-save
  :global t
  (when auto-save--timer (cancel-timer auto-save--timer))
  (setq auto-save--timer
        (when auto-save-visited-mode
          (run-with-idle-timer
           auto-save-visited-interval :repeat
           #'save-some-buffers :no-prompt
           (lambda ()
             (and buffer-file-name
                  auto-save-visited-mode
                  (not (and buffer-auto-save-file-name
                            auto-save-visited-file-name))))))))

;; The 'set' part is so we don't get a warning for using this variable
;; above, while still catching code that _sets_ the variable to get
;; the same effect as the new auto-save-visited-mode.
(make-obsolete-variable 'auto-save-visited-file-name 'auto-save-visited-mode
                        "Emacs 26.1" 'set)

(defcustom save-abbrevs t
  "Non-nil means save word abbrevs too when files are saved.
If `silently', don't ask the user before saving."
  :type '(choice (const t) (const nil) (const silently))
  :group 'abbrev)

(defcustom lock-file-name-transforms nil
  "Transforms to apply to buffer file name before making a lock file name.
This has the same syntax as `auto-save-file-name-transforms',
but applies to lock file names instead of auto-save file names.

By default, Emacs puts each lock file into the same directory as the
file it locks, prepending \".#\" to the base file name.

Note that changing this could break lock file functionality, e.g.:
if different users access the same file, using different lock file settings;
if accessing files on a shared file system from different hosts,
using a transform that puts the lock files on a local file system."
  :group 'files
  :type '(repeat (list (regexp :tag "Regexp")
                       (string :tag "Replacement")
		       (boolean :tag "Uniquify")))
  :version "28.1")

(defcustom remote-file-name-inhibit-locks nil
  "Whether to use file locks for remote files."
  :group 'files
  :version "28.1"
  :type 'boolean)

(define-minor-mode lock-file-mode
  "Toggle file locking in the current buffer (Lock File mode)."
  :version "28.1"
  (setq-local create-lockfiles (and lock-file-mode t)))

(defcustom find-file-run-dired t
  "Non-nil means allow `find-file' to visit directories.
To visit the directory, `find-file' runs `find-directory-functions'."
  :type 'boolean
  :group 'find-file)

(defcustom find-directory-functions '(cvs-dired-noselect dired-noselect)
  "List of functions to try in sequence to visit a directory.
Each function is called with the directory name as the sole argument
and should return either a buffer or nil."
  :type '(hook :options (cvs-dired-noselect dired-noselect))
  :group 'find-file)

;; FIXME: also add a hook for `(thing-at-point 'filename)'
(defcustom file-name-at-point-functions '(ffap-guess-file-name-at-point)
  "List of functions to try in sequence to get a file name at point.
Each function should return either nil or a file name found at the
location of point in the current buffer."
  :type '(hook :options (ffap-guess-file-name-at-point))
  :group 'find-file)

;;;It is not useful to make this a local variable.
;;;(put 'find-file-not-found-functions 'permanent-local t)
(define-obsolete-variable-alias 'find-file-not-found-hooks
    'find-file-not-found-functions "22.1")
(defvar find-file-not-found-functions nil
  "List of functions to be called for `find-file' on nonexistent file.
These functions are called as soon as the error is detected.
Variable `buffer-file-name' is already set up.
The functions are called in the order given until one of them returns non-nil.")

;;;It is not useful to make this a local variable.
;;;(put 'find-file-hook 'permanent-local t)
;; I found some external files still using the obsolete form in 2018.
(define-obsolete-variable-alias 'find-file-hooks 'find-file-hook "22.1")
(defcustom find-file-hook nil
  "List of functions to be called after a buffer is loaded from a file.
The buffer's local variables (if any) will have been processed before the
functions are called.  This includes directory-local variables, if any,
for the file's directory."
  :group 'find-file
  :type 'hook
  :options '(auto-insert)
  :version "22.1")

;; I found some external files still using the obsolete form in 2018.
(define-obsolete-variable-alias 'write-file-hooks 'write-file-functions "22.1")
(defvar write-file-functions nil
  "List of functions to be called before saving a buffer to a file.
Used only by `save-buffer'.
If one of them returns non-nil, the file is considered already written
and the rest are not called.
These hooks are considered to pertain to the visited file.
So any buffer-local binding of this variable is discarded if you change
the visited file name with \\[set-visited-file-name], but not when you
change the major mode.

This hook is not run if any of the functions in
`write-contents-functions' returns non-nil.  Both hooks pertain
to how to save a buffer to file, for instance, choosing a suitable
coding system and setting mode bits.  (See Info
node `(elisp)Saving Buffers'.)  To perform various checks or
updates before the buffer is saved, use `before-save-hook'.")
(put 'write-file-functions 'permanent-local t)

;; I found some files still using the obsolete form in 2018.
(defvar-local local-write-file-hooks nil)
(put 'local-write-file-hooks 'permanent-local t)
(make-obsolete-variable 'local-write-file-hooks 'write-file-functions "22.1")

;; I found some files still using the obsolete form in 2018.
(define-obsolete-variable-alias 'write-contents-hooks
    'write-contents-functions "22.1")
(defvar-local write-contents-functions nil
  "List of functions to be called before writing out a buffer to a file.

Used only by `save-buffer'.  If one of them returns non-nil, the
file is considered already written and the rest are not called
and neither are the functions in `write-file-functions'.  This
hook can thus be used to create save behavior for buffers that
are not visiting a file at all.

This variable is meant to be used for hooks that pertain to the
buffer's contents, not to the particular visited file; thus,
`set-visited-file-name' does not clear this variable; but changing the
major mode does clear it.

For hooks that _do_ pertain to the particular visited file, use
`write-file-functions'.  Both this variable and
`write-file-functions' relate to how a buffer is saved to file.
To perform various checks or updates before the buffer is saved,
use `before-save-hook'.")

(defcustom enable-local-variables t
  "Control use of local variables in files you visit.
The value can be t, nil, :safe, :all, or something else.

A value of t means file local variables specifications are obeyed
if all the specified variable values are safe; if any values are
not safe, Emacs queries you, once, whether to set them all.
\(When you say yes to certain values, they are remembered as safe.)

:safe means set the safe variables, and ignore the rest.
:all means set all variables, whether safe or not.
 (Don't set it permanently to :all.)
A value of nil means always ignore the file local variables.

Any other value means always query you once whether to set them all.
\(When you say yes to certain values, they are remembered as safe, but
this has no effect when `enable-local-variables' is \"something else\".)

This variable also controls use of major modes specified in
a -*- line.

The command \\[normal-mode], when used interactively,
always obeys file local variable specifications and the -*- line,
and ignores this variable.

Also see the `permanently-enabled-local-variables' variable."
  :risky t
  :type '(choice (const :tag "Query Unsafe" t)
		 (const :tag "Safe Only" :safe)
		 (const :tag "Do all" :all)
		 (const :tag "Ignore" nil)
		 (other :tag "Query" other))
  :group 'find-file)

(defvar enable-dir-local-variables t
  "Non-nil means enable use of directory-local variables.
Some modes may wish to set this to nil to prevent directory-local
settings being applied, but still respect file-local ones.")

;; This is an odd variable IMO.
;; You might wonder why it is needed, when we could just do:
;; (setq-local enable-local-variables nil)
;; These two are not precisely the same.
;; Setting this variable does not cause -*- mode settings to be
;; ignored, whereas setting enable-local-variables does.
;; Only three places in Emacs use this variable: tar and arc modes,
;; and rmail.  The first two don't need it.  They already use
;; inhibit-local-variables-regexps, which is probably enough, and
;; could also just set enable-local-variables locally to nil.
;; Them setting it has the side-effect that dir-locals cannot apply to
;; eg tar files (?).  FIXME Is this appropriate?
;; AFAICS, rmail is the only thing that needs this, and the only
;; reason it uses it is for BABYL files (which are obsolete).
;; These contain "-*- rmail -*-" in the first line, which rmail wants
;; to respect, so that find-file on a BABYL file will switch to
;; rmail-mode automatically (this is nice, but hardly essential,
;; since most people are used to explicitly running a command to
;; access their mail; M-x gnus etc).  Rmail files may happen to
;; contain Local Variables sections in messages, which Rmail wants to
;; ignore.  So AFAICS the only reason this variable exists is for a
;; minor convenience feature for handling of an obsolete Rmail file format.
(defvar local-enable-local-variables t
  "Like `enable-local-variables', except for major mode in a -*- line.
The meaningful values are nil and non-nil.  The default is non-nil.
It should be set in a buffer-local fashion.

Setting this to nil has the same effect as setting `enable-local-variables'
to nil, except that it does not ignore any mode: setting in a -*- line.
Unless this difference matters to you, you should set `enable-local-variables'
instead of this variable.")

(defcustom enable-local-eval 'maybe
  "Control processing of the \"variable\" `eval' in a file's local variables.
The value can be t, nil or something else.
A value of t means obey `eval' variables.
A value of nil means ignore them; anything else means query."
  :risky t
  :type '(choice (const :tag "Obey" t)
		 (const :tag "Ignore" nil)
		 (other :tag "Query" other))
  :group 'find-file)

(defcustom view-read-only nil
  "Non-nil means buffers visiting files read-only do so in view mode.
In fact, this means that all read-only buffers normally have
View mode enabled, including buffers that are read-only because
you visit a file you cannot alter, and buffers you make read-only
using \\[read-only-mode]."
  :type 'boolean
  :group 'view)

(defvar file-name-history nil
  "History list of file names entered in the minibuffer.

Maximum length of the history list is determined by the value
of `history-length', which see.")

(defvar save-silently nil
  "If non-nil, avoid messages when saving files.
Error-related messages will still be printed, but all other
messages will not.")


(put 'ange-ftp-completion-hook-function 'safe-magic t)
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

(declare-function dos-convert-standard-filename "dos-fns.el" (filename))
(declare-function w32-convert-standard-filename "w32-fns.el" (filename))

(defun convert-standard-filename (filename)
  "Convert a standard file's name to something suitable for the OS.
This means to guarantee valid names and perhaps to canonicalize
certain patterns.

FILENAME should be an absolute file name since the conversion rules
sometimes vary depending on the position in the file name.  E.g. c:/foo
is a valid DOS file name, but c:/bar/c:/foo is not.

This function's standard definition is trivial; it just returns
the argument.  However, on Windows and DOS, replace invalid
characters.  On DOS, make sure to obey the 8.3 limitations.
In the native Windows build, turn Cygwin names into native names.

See Info node `(elisp)Standard File Names' for more details."
  (cond
   ((eq system-type 'cygwin)
    (let ((name (copy-sequence filename))
	  (start 0))
      ;; Replace invalid filename characters with !
      (while (string-match "[?*:<>|\"\000-\037]" name start)
	(aset name (match-beginning 0) ?!)
	(setq start (match-end 0)))
      name))
   ((eq system-type 'windows-nt)
    (w32-convert-standard-filename filename))
   ((eq system-type 'ms-dos)
    (dos-convert-standard-filename filename))
   (t filename)))

(defun read-directory-name (prompt &optional dir default-dirname mustmatch initial)
  "Read directory name, prompting with PROMPT and completing in directory DIR.
Value is not expanded---you must call `expand-file-name' yourself.
Default name to DEFAULT-DIRNAME if user exits with the same
non-empty string that was inserted by this function.
 (If DEFAULT-DIRNAME is omitted, DIR combined with INITIAL is used,
  or just DIR if INITIAL is nil.)
If the user exits with an empty minibuffer, this function returns
an empty string.  (This can happen only if the user erased the
pre-inserted contents or if `insert-default-directory' is nil.)
Fourth arg MUSTMATCH non-nil means require existing directory's name.
 Non-nil and non-t means also require confirmation after completion.
Fifth arg INITIAL specifies text to start with.
DIR should be an absolute directory name.  It defaults to
the value of `default-directory'."
  (unless dir
    (setq dir default-directory))
  (read-file-name prompt dir (or default-dirname
				 (if initial (expand-file-name initial dir)
				   dir))
		  mustmatch initial
		  'file-directory-p))


(defun pwd (&optional insert)
  "Show the current default directory.
With prefix argument INSERT, insert the current default directory
at point instead."
  (interactive "P")
  (if insert
      (insert default-directory)
    (message "Directory %s" default-directory)))

(defvar cd-path nil
  "Value of the CDPATH environment variable, as a list.
Not actually set up until the first time you use it.")

(defun parse-colon-path (search-path)
  "Explode a search path into a list of directory names.
Directories are separated by `path-separator' (which is colon in
GNU and Unix systems).  Substitute environment variables into the
resulting list of directory names.  For an empty path element (i.e.,
a leading or trailing separator, or two adjacent separators), return
nil (meaning `default-directory') as the associated list element."
  (when (stringp search-path)
    (let ((spath (substitute-env-vars search-path)))
      (mapcar (lambda (f)
                (if (equal "" f) nil
                  (let ((dir (file-name-as-directory f)))
                    ;; Previous implementation used `substitute-in-file-name'
                    ;; which collapse multiple "/" in front.  Do the same for
                    ;; backward compatibility.
                    (if (string-match "\\`/+" dir)
                        (substring dir (1- (match-end 0))) dir))))
              (split-string spath path-separator)))))

(defun cd-absolute (dir)
  "Change current directory to given absolute file name DIR."
  ;; Put the name into directory syntax now,
  ;; because otherwise expand-file-name may give some bad results.
  (setq dir (file-name-as-directory dir))
  ;; We used to additionally call abbreviate-file-name here, for an
  ;; unknown reason.  Problem is that most buffers are setup
  ;; without going through cd-absolute and don't call
  ;; abbreviate-file-name on their default-directory, so the few that
  ;; do end up using a superficially different directory.
  (setq dir (expand-file-name dir))
  (if (not (file-directory-p dir))
      (error (if (file-exists-p dir)
	         "%s is not a directory"
               "%s: no such directory")
             dir)
    (unless (file-accessible-directory-p dir)
      (error "Cannot cd to %s:  Permission denied" dir))
    (setq default-directory dir)
    (setq list-buffers-directory dir)))

(defun cd (dir)
  "Make DIR become the current buffer's default directory.
If your environment includes a `CDPATH' variable, try each one of
that list of directories (separated by occurrences of
`path-separator') when resolving a relative directory name.
The path separator is colon in GNU and GNU-like systems."
  (interactive
   (list
    ;; FIXME: There's a subtle bug in the completion below.  Seems linked
    ;; to a fundamental difficulty of implementing `predicate' correctly.
    ;; The manifestation is that TAB may list non-directories in the case where
    ;; those files also correspond to valid directories (if your cd-path is (A/
    ;; B/) and you have A/a a file and B/a a directory, then both `a' and `a/'
    ;; will be listed as valid completions).
    ;; This is because `a' (listed because of A/a) is indeed a valid choice
    ;; (which will lead to the use of B/a).
    (minibuffer-with-setup-hook
        (lambda ()
          (setq-local minibuffer-completion-table
		      (apply-partially #'locate-file-completion-table
				       cd-path nil))
          (setq-local minibuffer-completion-predicate
		      (lambda (dir)
			(locate-file dir cd-path nil
				     (lambda (f) (and (file-directory-p f) 'dir-ok))))))
      (unless cd-path
        (setq cd-path (or (parse-colon-path (getenv "CDPATH"))
                          (list "./"))))
      (read-directory-name "Change default directory: "
                           default-directory default-directory
                           t))))
  (unless cd-path
    (setq cd-path (or (parse-colon-path (getenv "CDPATH"))
                      (list "./"))))
  (cd-absolute
   (or
    ;; locate-file doesn't support remote file names, so detect them
    ;; and support them here by hand.
    (and (file-remote-p (expand-file-name dir))
         (file-accessible-directory-p (expand-file-name dir))
         (expand-file-name dir))
    (locate-file dir cd-path nil
                 (lambda (f) (and (file-directory-p f) 'dir-ok)))
    (if (getenv "CDPATH")
        (error "No such directory found via CDPATH environment variable: %s" dir)
      (error "No such directory: %s" dir)))))

(defun directory-files-recursively (dir regexp
                                        &optional include-directories predicate
                                        follow-symlinks)
  "Return list of all files under directory DIR whose names match REGEXP.
This function works recursively.  Files are returned in \"depth
first\" order, and files from each directory are sorted in
alphabetical order.  Each file name appears in the returned list
in its absolute form.

By default, the returned list excludes directories, but if
optional argument INCLUDE-DIRECTORIES is non-nil, they are
included.

PREDICATE can be either nil (which means that all subdirectories
of DIR are descended into), t (which means that subdirectories that
can't be read are ignored), or a function (which is called with
the name of each subdirectory, and should return non-nil if the
subdirectory is to be descended into).

If FOLLOW-SYMLINKS is non-nil, symbolic links that point to
directories are followed.  Note that this can lead to infinite
recursion."
  (let* ((result nil)
	 (files nil)
         (dir (directory-file-name dir))
	 ;; When DIR is "/", remote file names like "/method:" could
	 ;; also be offered.  We shall suppress them.
	 (tramp-mode (and tramp-mode (file-remote-p (expand-file-name dir)))))
    (dolist (file (sort (file-name-all-completions "" dir)
			'string<))
      (unless (member file '("./" "../"))
	(if (directory-name-p file)
	    (let* ((leaf (substring file 0 (1- (length file))))
		   (full-file (concat dir "/" leaf)))
	      ;; Don't follow symlinks to other directories.
	      (when (and (or (not (file-symlink-p full-file))
                             (and (file-symlink-p full-file)
                                  follow-symlinks))
                         ;; Allow filtering subdirectories.
                         (or (eq predicate nil)
                             (eq predicate t)
                             (funcall predicate full-file)))
                (let ((sub-files
                       (if (eq predicate t)
                           (ignore-error file-error
                             (directory-files-recursively
			      full-file regexp include-directories
                              predicate follow-symlinks))
                         (directory-files-recursively
			  full-file regexp include-directories
                          predicate follow-symlinks))))
		  (setq result (nconc result sub-files))))
	      (when (and include-directories
			 (string-match regexp leaf))
		(setq result (nconc result (list full-file)))))
	  (when (string-match regexp file)
	    (push (concat dir "/" file) files)))))
    (nconc result (nreverse files))))

(defun directory-empty-p (dir)
  "Return t if DIR names an existing directory containing no other files.
Return nil if DIR does not name a directory, or if there was
trouble determining whether DIR is a directory or empty.

Symbolic links to directories count as directories.
See `file-symlink-p' to distinguish symlinks."
  (and (file-directory-p dir)
       (null (directory-files dir nil directory-files-no-dot-files-regexp t 1))))

(defvar module-file-suffix)

(defun load-file (file)
  "Load the Lisp file named FILE."
  ;; This is a case where .elc and .so/.dll make a lot of sense.
  (interactive (list (let ((completion-ignored-extensions
                            (remove module-file-suffix
                                    (remove ".elc"
                                            completion-ignored-extensions))))
		       (read-file-name "Load file: " nil nil 'lambda))))
  (load (expand-file-name file) nil nil t))

(defvar comp-eln-to-el-h)

(defun locate-file (filename path &optional suffixes predicate)
  "Search for FILENAME through PATH.
If found, return the absolute file name of FILENAME; otherwise
return nil.
PATH should be a list of directories to look in, like the lists in
`exec-path' or `load-path'.
If SUFFIXES is non-nil, it should be a list of suffixes to append to
file name when searching.  If SUFFIXES is nil, it is equivalent to (\"\").
Use (\"/\") to disable PATH search, but still try the suffixes in SUFFIXES.
If non-nil, PREDICATE is used instead of `file-readable-p'.

This function will normally skip directories, so if you want it to find
directories, make sure the PREDICATE function returns `dir-ok' for them.

PREDICATE can also be an integer to pass to the `access' system call,
in which case file name handlers are ignored.  This usage is deprecated.
For compatibility, PREDICATE can also be one of the symbols
`executable', `readable', `writable', or `exists', or a list of
one or more of those symbols."
  (if (and predicate (symbolp predicate) (not (functionp predicate)))
      (setq predicate (list predicate)))
  (when (and (consp predicate) (not (functionp predicate)))
    (setq predicate
	  (logior (if (memq 'executable predicate) 1 0)
		  (if (memq 'writable predicate) 2 0)
		  (if (memq 'readable predicate) 4 0))))
  (let ((file (locate-file-internal filename path suffixes predicate)))
    (if (and file (string-match "\\.eln\\'" file))
        (gethash (file-name-nondirectory file) comp-eln-to-el-h)
      file)))

(defun locate-file-completion-table (dirs suffixes string pred action)
  "Do completion for file names passed to `locate-file'."
  (cond
   ((file-name-absolute-p string)
    ;; FIXME: maybe we should use completion-file-name-table instead,
    ;; tho at least for `load', the arg is passed through
    ;; substitute-in-file-name for historical reasons.
    (read-file-name-internal string pred action))
   ((eq (car-safe action) 'boundaries)
    (let ((suffix (cdr action)))
      `(boundaries
        ,(length (file-name-directory string))
        ,@(let ((x (file-name-directory suffix)))
            (if x (1- (length x)) (length suffix))))))
   (t
    (let ((names '())
          ;; If we have files like "foo.el" and "foo.elc", we could load one of
          ;; them with "foo.el", "foo.elc", or "foo", where just "foo" is the
          ;; preferred way.  So if we list all 3, that gives a lot of redundant
          ;; entries for the poor soul looking just for "foo".  OTOH, sometimes
          ;; the user does want to pay attention to the extension.  We try to
          ;; diffuse this tension by stripping the suffix, except when the
          ;; result is a single element (i.e. usually we list only "foo" unless
          ;; it's the only remaining element in the list, in which case we do
          ;; list "foo", "foo.elc" and "foo.el").
          (fullnames '())
	  (suffix (concat (regexp-opt suffixes t) "\\'"))
	  (string-dir (file-name-directory string))
          (string-file (file-name-nondirectory string)))
      (dolist (dir dirs)
        (unless dir
          (setq dir default-directory))
        (if string-dir (setq dir (expand-file-name string-dir dir)))
        (when (file-directory-p dir)
          (dolist (file (file-name-all-completions
                         string-file dir))
            (if (not (string-match suffix file))
                (push file names)
              (push file fullnames)
              (push (substring file 0 (match-beginning 0)) names)))))
      ;; Switching from names to names+fullnames creates a non-monotonicity
      ;; which can cause problems with things like partial-completion.
      ;; To minimize the problem, filter out completion-regexp-list, so that
      ;; M-x load-library RET t/x.e TAB finds some files.  Also remove elements
      ;; from `names' that matched `string' only when they still had
      ;; their suffix.
      (setq names (all-completions string-file names))
      ;; Remove duplicates of the first element, so that we can easily check
      ;; if `names' really contains only a single element.
      (when (cdr names) (setcdr names (delete (car names) (cdr names))))
      (unless (cdr names)
        ;; There's no more than one matching non-suffixed element, so expand
        ;; the list by adding the suffixed elements as well.
        (setq names (nconc names fullnames)))
      (completion-table-with-context
       string-dir names string-file pred action)))))

(defvar locate-dominating-stop-dir-regexp
  (purecopy "\\`\\(?:[\\/][\\/][^\\/]+[\\/]\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'")
  "Regexp of directory names that stop the search in `locate-dominating-file'.
Any directory whose name matches this regexp will be treated like
a kind of root directory by `locate-dominating-file', which will stop its
search when it bumps into it.
The default regexp prevents fruitless and time-consuming attempts to find
special files in directories in which file names are interpreted as host names,
or mount points potentially requiring authentication as a different user.")

(defun locate-dominating-file (file name)
  "Starting at FILE, look up directory hierarchy for directory containing NAME.
FILE can be a file or a directory.  If it's a file, its directory will
serve as the starting point for searching the hierarchy of directories.
Stop at the first parent directory containing a file NAME,
and return the directory.  Return nil if not found.
Instead of a string, NAME can also be a predicate taking one argument
\(a directory) and returning a non-nil value if that directory is the one for
which we're looking.  The predicate will be called with every file/directory
the function needs to examine, starting with FILE."
  ;; Represent /home/luser/foo as ~/foo so that we don't try to look for
  ;; `name' in /home or in /.
  (setq file (abbreviate-file-name (expand-file-name file)))
  (let ((root nil)
        try)
    (while (not (or root
                    (null file)
                    (string-match locate-dominating-stop-dir-regexp file)))
      (setq try (if (stringp name)
                    (and (file-directory-p file)
                         (file-exists-p (expand-file-name name file)))
                  (funcall name file)))
      (cond (try (setq root file))
            ((equal file (setq file (file-name-directory
                                     (directory-file-name file))))
             (setq file nil))))
    (if root (file-name-as-directory root))))

(defcustom user-emacs-directory-warning t
  "Non-nil means warn if unable to access or create `user-emacs-directory'.
Set this to nil at your own risk, as it might lead to data loss
when Emacs tries to write something to a non-existent or
inaccessible location."
  :type 'boolean
  :group 'initialization
  :version "24.4")

(defun locate-user-emacs-file (new-name &optional old-name)
  "Return an absolute per-user Emacs-specific file name.
If NEW-NAME exists in `user-emacs-directory', return it.
Else if OLD-NAME is non-nil and ~/OLD-NAME exists, return ~/OLD-NAME.
Else return NEW-NAME in `user-emacs-directory', creating the
directory if it does not exist."
  (convert-standard-filename
   (let* ((home (concat "~" (or init-file-user "")))
	  (at-home (and old-name (expand-file-name old-name home)))
          (bestname (abbreviate-file-name
                     (expand-file-name new-name user-emacs-directory))))
     (if (and at-home (not (file-readable-p bestname))
              (file-readable-p at-home))
	 at-home
       ;; Make sure `user-emacs-directory' exists,
       ;; unless we're in batch mode or dumping Emacs.
       (or noninteractive
           dump-mode
	   (let (errtype)
	     (if (file-directory-p user-emacs-directory)
		 (or (file-accessible-directory-p user-emacs-directory)
		     (setq errtype "access"))
	       (with-file-modes ?\700
		 (condition-case nil
		     (make-directory user-emacs-directory t)
		   (error (setq errtype "create")))))
	     (when (and errtype
			user-emacs-directory-warning
			(not (get 'user-emacs-directory-warning 'this-session)))
	       ;; Warn only once per Emacs session.
	       (put 'user-emacs-directory-warning 'this-session t)
	       (display-warning 'initialization
				(format "\
Unable to %s `user-emacs-directory' (%s).
Any data that would normally be written there may be lost!
If you never want to see this message again,
customize the variable `user-emacs-directory-warning'."
					errtype user-emacs-directory)))))
       bestname))))

(defun exec-path ()
  "Return list of directories to search programs to run in remote subprocesses.
The remote host is identified by `default-directory'.  For remote
hosts that do not support subprocesses, this returns nil.
If `default-directory' is a local directory, this function returns
the value of the variable `exec-path'."
  (let ((handler (find-file-name-handler default-directory 'exec-path)))
    (if handler
	(funcall handler 'exec-path)
      exec-path)))

(defun executable-find (command &optional remote)
  "Search for COMMAND in `exec-path' and return the absolute file name.
Return nil if COMMAND is not found anywhere in `exec-path'.  If
REMOTE is non-nil, search on the remote host indicated by
`default-directory' instead."
  (if (and remote (file-remote-p default-directory))
      (let ((res (locate-file
	          command
	          (mapcar
	           (lambda (x) (concat (file-remote-p default-directory) x))
	           (exec-path))
	          exec-suffixes 'file-executable-p)))
        (when (stringp res) (file-local-name res)))
    ;; Use 1 rather than file-executable-p to better match the
    ;; behavior of call-process.
    (let ((default-directory (file-name-quote default-directory 'top)))
      (locate-file command exec-path exec-suffixes 1))))

(declare-function read-library-name "find-func" nil)

(defun load-library (library)
  "Load the Emacs Lisp library named LIBRARY.
LIBRARY should be a string.
This is an interface to the function `load'.  LIBRARY is searched
for in `load-path', both with and without `load-suffixes' (as
well as `load-file-rep-suffixes').

See Info node `(emacs)Lisp Libraries' for more details.
See `load-file' for a different interface to `load'."
  (interactive (list (read-library-name)))
  (load library))

(defun file-remote-p (file &optional identification connected)
  "Test whether FILE specifies a location on a remote system.
A file is considered remote if accessing it is likely to
be slower or less reliable than accessing local files.

`file-remote-p' never opens a new remote connection.  It can
reuse only a connection that is already open.

Return nil or a string identifying the remote connection
\(ideally a prefix of FILE).  Return nil if FILE is a relative
file name.

When IDENTIFICATION is nil, the returned string is a complete
remote identifier: with components method, user, and host.  The
components are those present in FILE, with defaults filled in for
any that are missing.

IDENTIFICATION can specify which part of the identification to
return.  IDENTIFICATION can be the symbol `method', `user',
`host', or `localname'.  Any other value is handled like nil and
means to return the complete identification.  The string returned
for IDENTIFICATION `localname' can differ depending on whether
there is an existing connection.

If CONNECTED is non-nil, return an identification only if FILE is
located on a remote system and a connection is established to
that remote system.

Tip: You can use this expansion of remote identifier components
     to derive a new remote file name from an existing one.  For
     example, if FILE is \"/sudo::/path/to/file\" then

       (concat (file-remote-p FILE) \"/bin/sh\")

     returns a remote file name for file \"/bin/sh\" that has the
     same remote identifier as FILE but expanded; a name such as
     \"/sudo:root@myhost:/bin/sh\"."
  (let ((handler (find-file-name-handler file 'file-remote-p)))
    (if handler
	(funcall handler 'file-remote-p file identification connected)
      nil)))

;; Probably this entire variable should be obsolete now, in favor of
;; something Tramp-related (?).  It is not used in many places.
;; It's not clear what the best file for this to be in is, but given
;; it uses custom-initialize-delay, it is easier if it is preloaded
;; rather than autoloaded.
(defcustom remote-shell-program
  ;; This used to try various hard-coded places for remsh, rsh, and
  ;; rcmd, trying to guess based on location whether "rsh" was
  ;; "restricted shell" or "remote shell", but I don't see the point
  ;; in this day and age.  Almost everyone will use ssh, and have
  ;; whatever command they want to use in PATH.
  (purecopy
   (let ((list '("ssh" "remsh" "rcmd" "rsh")))
     (while (and list
		 (not (executable-find (car list)))
		 (setq list (cdr list))))
     (or (car list) "ssh")))
  "Program to use to execute commands on a remote host (e.g. ssh or rsh)."
  :version "24.3"			; ssh rather than rsh, etc
  :initialize 'custom-initialize-delay
  :group 'environment
  :type 'file)

(defcustom remote-file-name-inhibit-cache 10
  "Whether to use the remote file-name cache for read access.
When nil, never expire cached values (caution)
When t, never use the cache (safe, but may be slow)
A number means use cached values for that amount of seconds since caching.

The attributes of remote files are cached for better performance.
If they are changed outside of Emacs's control, the cached values
become invalid, and must be reread.  If you are sure that nothing
other than Emacs changes the files, you can set this variable to nil.

If a remote file is checked regularly, it might be a good idea to
let-bind this variable to a value less than the interval between
consecutive checks.  For example:

  (defun display-time-file-nonempty-p (file)
    (let ((remote-file-name-inhibit-cache (- display-time-interval 5)))
      (and (file-exists-p file)
           (< 0 (file-attribute-size
                 (file-attributes (file-chase-links file)))))))"
  :group 'files
  :version "24.1"
  :type '(choice
	  (const   :tag "Do not inhibit file name cache" nil)
	  (const   :tag "Do not use file name cache" t)
	  (integer :tag "Do not use file name cache"
		   :format "Do not use file name cache older then %v seconds"
		   :value 10)))

(defun file-local-name (file)
  "Return the local name component of FILE.
This function removes from FILE the specification of the remote host
and the method of accessing the host, leaving only the part that
identifies FILE locally on the remote system.
The returned file name can be used directly as argument of
`process-file', `start-file-process', or `shell-command'."
  (or (file-remote-p file 'localname) file))

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

(defun files--name-absolute-system-p (file)
  "Return non-nil if FILE is an absolute name to the operating system.
This is like `file-name-absolute-p', except that it returns nil for
names beginning with `~'."
  (and (file-name-absolute-p file)
       (not (eq (aref file 0) ?~))))

(defun files--splice-dirname-file (dirname file)
  "Splice DIRNAME to FILE like the operating system would.
If FILE is relative, return DIRNAME concatenated to FILE.
Otherwise return FILE, quoted as needed if DIRNAME and FILE have
different file name handlers; although this quoting is dubious if
DIRNAME is magic, it is not clear what would be better.  This
function differs from `expand-file-name' in that DIRNAME must be
a directory name and leading `~' and `/:' are not special in
FILE."
  (let ((unquoted (if (files--name-absolute-system-p file)
		      file
		    (concat dirname file))))
    (if (eq (find-file-name-handler dirname 'file-symlink-p)
	    (find-file-name-handler unquoted 'file-symlink-p))
	unquoted
      (let (file-name-handler-alist) (file-name-quote unquoted)))))

(defun file-truename (filename &optional counter prev-dirs)
  "Return the truename of FILENAME.
If FILENAME is not absolute, first expands it against `default-directory'.
The truename of a file name is found by chasing symbolic links
both at the level of the file and at the level of the directories
containing it, until no links are left at any level.

\(fn FILENAME)"  ;; Don't document the optional arguments.
  ;; COUNTER and PREV-DIRS are used only in recursive calls.
  ;; COUNTER can be a cons cell whose car is the count of how many
  ;; more links to chase before getting an error.
  ;; PREV-DIRS can be a cons cell whose car is an alist
  ;; of truenames we've just recently computed.
  (cond ((or (string= filename "") (string= filename "~"))
	 (setq filename (expand-file-name filename))
	 (if (string= filename "")
	     (setq filename "/")))
	((and (string= (substring filename 0 1) "~")
	      (string-match "~[^/]*/?" filename))
	 (let ((first-part
		(substring filename 0 (match-end 0)))
	       (rest (substring filename (match-end 0))))
	   (setq filename (concat (expand-file-name first-part) rest)))))

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

    ;; andrewi@harlequin.co.uk - on Windows, there is an issue with
    ;; case differences being ignored by the OS, and short "8.3 DOS"
    ;; name aliases existing for all files.  (The short names are not
    ;; reported by directory-files, but can be used to refer to files.)
    ;; It seems appropriate for file-truename to resolve these issues in
    ;; the most natural way, which on Windows is to call the function
    ;; `w32-long-file-name' - this returns the exact name of a file as
    ;; it is stored on disk (expanding short name aliases with the full
    ;; name in the process).
    (if (eq system-type 'windows-nt)
	(unless (string-match "[[*?]" filename)
	  ;; If filename exists, use its long name.  If it doesn't
	  ;; exist, the recursion below on the directory of filename
	  ;; will drill down until we find a directory that exists,
	  ;; and use the long name of that, with the extra
	  ;; non-existent path components concatenated.
	  (let ((longname (w32-long-file-name filename)))
	    (if longname
		(setq filename longname)))))

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
		(and (file-name-case-insensitive-p dir)
		     (eq (compare-strings dir 0 nil dirfile 0 nil t) t))
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
		    (setq filename (files--splice-dirname-file dir target)
			  done nil)
		  ;; No, we are done!
		  (setq done t))))))))
    filename))

(defun file-chase-links (filename &optional limit)
  "Chase links in FILENAME until a name that is not a link.
Unlike `file-truename', this does not check whether a parent
directory name is a symbolic link.
If the optional argument LIMIT is a number,
it means chase no more than that many links and then stop."
  (let (tem (newname filename)
	    (count 0))
    (while (and (or (null limit) (< count limit))
		(setq tem (file-symlink-p newname)))
      (save-match-data
	(if (and (null limit) (= count 100))
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
	(setq newname (files--splice-dirname-file (file-name-directory newname)
						  tem))
	(setq count (1+ count))))
    newname))

;; A handy function to display file sizes in human-readable form.
;; See https://en.wikipedia.org/wiki/Kibibyte for the reference.
(defun file-size-human-readable (file-size &optional flavor space unit)
  "Produce a string showing FILE-SIZE in human-readable form.

Optional second argument FLAVOR controls the units and the display format:

 If FLAVOR is nil or omitted, each kilobyte is 1024 bytes and the produced
    suffixes are \"k\", \"M\", \"G\", \"T\", etc.
 If FLAVOR is `si', each kilobyte is 1000 bytes and the produced suffixes
    are \"k\", \"M\", \"G\", \"T\", etc.
 If FLAVOR is `iec', each kilobyte is 1024 bytes and the produced suffixes
    are \"KiB\", \"MiB\", \"GiB\", \"TiB\", etc.

Optional third argument SPACE is a string put between the number and unit.
It defaults to the empty string.  We recommend a single space or
non-breaking space, unless other constraints prohibit a space in that
position.

Optional fourth argument UNIT is the unit to use.  It defaults to \"B\"
when FLAVOR is `iec' and the empty string otherwise.  We recommend \"B\"
in all cases, since that is the standard symbol for byte."
  (let ((power (if (or (null flavor) (eq flavor 'iec))
		   1024.0
		 1000.0))
	(prefixes '("" "k" "M" "G" "T" "P" "E" "Z" "Y")))
    (while (and (>= file-size power) (cdr prefixes))
      (setq file-size (/ file-size power)
	    prefixes (cdr prefixes)))
    (let* ((prefix (car prefixes))
           (prefixed-unit (if (eq flavor 'iec)
                              (concat
                               (if (string= prefix "k") "K" prefix)
                               (if (string= prefix "") "" "i")
                               (or unit "B"))
                            (concat prefix unit))))
      (format (if (and (>= (mod file-size 1.0) 0.05)
                       (< (mod file-size 1.0) 0.95))
		  "%.1f%s%s"
	        "%.0f%s%s")
	      file-size
              (if (string= prefixed-unit "") "" (or space ""))
              prefixed-unit))))

(defun file-size-human-readable-iec (size)
  "Human-readable string for SIZE bytes, using IEC prefixes."
  (file-size-human-readable size 'iec " "))

(defcustom byte-count-to-string-function #'file-size-human-readable-iec
  "Function that turns a number of bytes into a human-readable string.
It is for use when displaying file sizes and disk space where other
constraints do not force a specific format."
  :type '(radio
          (function-item file-size-human-readable-iec)
          (function-item file-size-human-readable)
          (function :tag "Custom function" :value number-to-string))
  :group 'files
  :version "27.1")

(defcustom mounted-file-systems
  (if (memq system-type '(windows-nt cygwin))
      "^//[^/]+/"
    ;; regexp-opt.el is not dumped into emacs binary.
    ;;(concat
    ;; "^" (regexp-opt '("/afs/" "/media/" "/mnt" "/net/" "/tmp_mnt/"))))
    "^\\(?:/\\(?:afs/\\|m\\(?:edia/\\|nt\\)\\|\\(?:ne\\|tmp_mn\\)t/\\)\\)")
  "File systems that ought to be mounted."
  :group 'files
  :version "26.1"
  :require 'regexp-opt
  :type 'regexp)

(defun temporary-file-directory ()
  "The directory for writing temporary files.
In case of a remote `default-directory', this is a directory for
temporary files on that remote host.  If such a directory does
not exist, or `default-directory' ought to be located on a
mounted file system (see `mounted-file-systems'), the function
returns `default-directory'.
For a non-remote and non-mounted `default-directory', the value of
the variable `temporary-file-directory' is returned."
  (let ((handler (find-file-name-handler
                  default-directory 'temporary-file-directory)))
    (if handler
	(funcall handler 'temporary-file-directory)
      (if (string-match mounted-file-systems default-directory)
          default-directory
        temporary-file-directory))))

(defun make-temp-file (prefix &optional dir-flag suffix text)
  "Create a temporary file.
The returned file name (created by appending some random characters at the end
of PREFIX, and expanding against `temporary-file-directory' if necessary),
is guaranteed to point to a newly created file.
You can then use `write-region' to write new data into the file.

If DIR-FLAG is non-nil, create a new empty directory instead of a file.

If SUFFIX is non-nil, add that at the end of the file name.

If TEXT is a string, insert it into the new file; DIR-FLAG should be nil.
Otherwise the file will be empty."
  (let ((absolute-prefix
	 (if (or (zerop (length prefix)) (member prefix '("." "..")))
	     (concat (file-name-as-directory temporary-file-directory) prefix)
	   (expand-file-name prefix temporary-file-directory))))
    (if (find-file-name-handler absolute-prefix 'write-region)
        (files--make-magic-temp-file absolute-prefix dir-flag suffix text)
      (make-temp-file-internal absolute-prefix
			       (if dir-flag t) (or suffix "") text))))

(defun files--make-magic-temp-file (absolute-prefix
                                    &optional dir-flag suffix text)
  "Implement (make-temp-file ABSOLUTE-PREFIX DIR-FLAG SUFFIX TEXT).
This implementation works on magic file names."
  ;; Create temp files with strict access rights.  It's easy to
  ;; loosen them later, whereas it's impossible to close the
  ;; time-window of loose permissions otherwise.
  (with-file-modes ?\700
    (let ((contents (if (stringp text) text ""))
          file)
      (while (condition-case ()
		 (progn
		   (setq file (make-temp-name absolute-prefix))
		   (if suffix
		       (setq file (concat file suffix)))
		   (if dir-flag
		       (make-directory file)
		     (write-region contents nil file nil 'silent nil 'excl))
		   nil)
	       (file-already-exists t))
	;; the file was somehow created by someone else between
	;; `make-temp-name' and `write-region', let's try again.
	nil)
      file)))

(defun make-nearby-temp-file (prefix &optional dir-flag suffix)
  "Create a temporary file as close as possible to `default-directory'.
Return the absolute file name of the created file.
If PREFIX is a relative file name, and `default-directory' is a
remote file name or located on a mounted file systems, the
temporary file is created in the directory returned by the
function `temporary-file-directory'.  Otherwise, the function
`make-temp-file' is used.  PREFIX, DIR-FLAG and SUFFIX have the
same meaning as in `make-temp-file'."
  (let ((handler (find-file-name-handler
                  default-directory 'make-nearby-temp-file)))
    (if (and handler (not (file-name-absolute-p default-directory)))
	(funcall handler 'make-nearby-temp-file prefix dir-flag suffix)
      (let ((temporary-file-directory (temporary-file-directory)))
        (make-temp-file prefix dir-flag suffix)))))

(defun recode-file-name (file coding new-coding &optional ok-if-already-exists)
  "Change the encoding of FILE's name from CODING to NEW-CODING.
The value is a new name of FILE.
Signals a `file-already-exists' error if a file of the new name
already exists unless optional fourth argument OK-IF-ALREADY-EXISTS
is non-nil.  A number as fourth arg means request confirmation if
the new name already exists.  This is what happens in interactive
use with \\[execute-extended-command]."
  (interactive
   (let ((default-coding (or file-name-coding-system
			     default-file-name-coding-system))
	 (filename (read-file-name "Recode filename: " nil nil t))
	 from-coding to-coding)
     (if (and default-coding
	      ;; We provide the default coding only when it seems that
	      ;; the filename is correctly decoded by the default
	      ;; coding.
	      (let ((charsets (find-charset-string filename)))
		(and (not (memq 'eight-bit-control charsets))
		     (not (memq 'eight-bit-graphic charsets)))))
	 (setq from-coding (read-coding-system
			    (format-prompt "Recode filename %s from"
				           filename default-coding)
			    default-coding))
       (setq from-coding (read-coding-system
			  (format "Recode filename %s from: " filename))))

     ;; We provide the default coding only when a user is going to
     ;; change the encoding not from the default coding.
     (if (eq from-coding default-coding)
	 (setq to-coding (read-coding-system
			  (format "Recode filename %s from %s to: "
				  filename from-coding)))
       (setq to-coding (read-coding-system
			(format-prompt "Recode filename %s from %s to"
				       default-coding filename from-coding)
			default-coding)))
     (list filename from-coding to-coding)))

  (let* ((default-coding (or file-name-coding-system
			     default-file-name-coding-system))
	 ;; FILE should have been decoded by DEFAULT-CODING.
	 (encoded (encode-coding-string file default-coding))
	 (newname (decode-coding-string encoded coding))
	 (new-encoded (encode-coding-string newname new-coding))
	 ;; Suppress further encoding.
	 (file-name-coding-system nil)
	 (default-file-name-coding-system nil)
	 (locale-coding-system nil))
    (rename-file encoded new-encoded ok-if-already-exists)
    newname))

(defcustom confirm-nonexistent-file-or-buffer 'after-completion
  "Whether confirmation is requested before visiting a new file or buffer.
If nil, confirmation is not requested.
If the value is `after-completion', confirmation is requested
 only if the user called `minibuffer-complete' right before
 `minibuffer-complete-and-exit'.
Any other non-nil value means to request confirmation.

This affects commands like `switch-to-buffer' and `find-file'."
  :group 'find-file
  :version "23.1"
  :type '(choice (const :tag "After completion" after-completion)
		 (const :tag "Never" nil)
		 (other :tag "Always" t)))

(defun confirm-nonexistent-file-or-buffer ()
  "Whether to request confirmation before visiting a new file or buffer.
The variable `confirm-nonexistent-file-or-buffer' determines the
return value, which may be passed as the REQUIRE-MATCH arg to
`read-buffer' or `find-file-read-args'."
  (cond ((eq confirm-nonexistent-file-or-buffer 'after-completion)
	 'confirm-after-completion)
	(confirm-nonexistent-file-or-buffer
	 'confirm)
	(t nil)))

(defmacro minibuffer-with-setup-hook (fun &rest body)
  "Temporarily add FUN to `minibuffer-setup-hook' while executing BODY.

By default, FUN is prepended to `minibuffer-setup-hook'.  But if FUN is of
the form `(:append FUN1)', FUN1 will be appended to `minibuffer-setup-hook'
instead of prepending it.

BODY should use the minibuffer at most once.
Recursive uses of the minibuffer are unaffected (FUN is not
called additional times).

This macro actually adds an auxiliary function that calls FUN,
rather than FUN itself, to `minibuffer-setup-hook'."
  (declare (indent 1) (debug ([&or (":append" form) [&or symbolp form]] body)))
  (let ((hook (make-symbol "setup-hook"))
        (funsym (make-symbol "fun"))
        (append nil))
    (when (eq (car-safe fun) :append)
      (setq append '(t) fun (cadr fun)))
    `(let ((,funsym ,fun)
           ;; Use a symbol to make sure `add-hook' doesn't waste time
           ;; in `equal'ity testing (bug#46326).
           (,hook (make-symbol "minibuffer-setup")))
       (fset ,hook (lambda ()
                     ;; Clear out this hook so it does not interfere
                     ;; with any recursive minibuffer usage.
                     (remove-hook 'minibuffer-setup-hook ,hook)
                     (funcall ,funsym)))
       (unwind-protect
           (progn
             (add-hook 'minibuffer-setup-hook ,hook ,@append)
             ,@body)
         (remove-hook 'minibuffer-setup-hook ,hook)))))

(defun find-file-read-args (prompt mustmatch)
  (list (read-file-name prompt nil default-directory mustmatch)
	t))

(defun file-name-history--add (file)
  "Add FILE to `file-name-history'."
  (add-to-history 'file-name-history (abbreviate-file-name file)))

(defun find-file (filename &optional wildcards)
  "Edit file FILENAME.
Switch to a buffer visiting file FILENAME,
creating one if none already exists.
Interactively, the default if you just type RET is the current directory,
but the visited file name is available through the minibuffer history:
type \\[next-history-element] to pull it into the minibuffer.

The first time \\[next-history-element] is used after Emacs prompts for
the file name, the result is affected by `file-name-at-point-functions',
which by default try to guess the file name by looking at point in the
current buffer.  Customize the value of `file-name-at-point-functions'
or set it to nil, if you want only the visited file name and the
current directory to be available on first \\[next-history-element]
request.

You can visit files on remote machines by specifying something
like /ssh:SOME_REMOTE_MACHINE:FILE for the file name.  You can
also visit local files as a different user by specifying
/sudo::FILE for the file name.
See the Info node `(tramp)File name Syntax' in the Tramp Info
manual, for more about this.

Interactively, or if WILDCARDS is non-nil in a call from Lisp,
expand wildcards (if any) and visit multiple files.  You can
suppress wildcard expansion by setting `find-file-wildcards' to nil.

To visit a file without any kind of conversion and without
automatically choosing a major mode, use \\[find-file-literally]."
  (interactive
   (find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer)))
  (let ((value (find-file-noselect filename nil nil wildcards)))
    (if (listp value)
	(mapcar 'pop-to-buffer-same-window (nreverse value))
      (pop-to-buffer-same-window value))))

(defun find-file-other-window (filename &optional wildcards)
  "Edit file FILENAME, in another window.

Like \\[find-file] (which see), but creates a new window or reuses
an existing one.  See the function `display-buffer'.

Interactively, the default if you just type RET is the current directory,
but the visited file name is available through the minibuffer history:
type \\[next-history-element] to pull it into the minibuffer.

The first time \\[next-history-element] is used after Emacs prompts for
the file name, the result is affected by `file-name-at-point-functions',
which by default try to guess the file name by looking at point in the
current buffer.  Customize the value of `file-name-at-point-functions'
or set it to nil, if you want only the visited file name and the
current directory to be available on first \\[next-history-element]
request.

Interactively, or if WILDCARDS is non-nil in a call from Lisp,
expand wildcards (if any) and visit multiple files."
  (interactive
   (find-file-read-args "Find file in other window: "
                        (confirm-nonexistent-file-or-buffer)))
  (let ((value (find-file-noselect filename nil nil wildcards)))
    (if (listp value)
	(progn
	  (setq value (nreverse value))
	  (switch-to-buffer-other-window (car value))
	  (mapc 'switch-to-buffer (cdr value))
	  value)
      (switch-to-buffer-other-window value))))

(defun find-file-other-frame (filename &optional wildcards)
  "Edit file FILENAME, in another frame.

Like \\[find-file] (which see), but creates a new frame or reuses
an existing one.  See the function `display-buffer'.

Interactively, the default if you just type RET is the current directory,
but the visited file name is available through the minibuffer history:
type \\[next-history-element] to pull it into the minibuffer.

The first time \\[next-history-element] is used after Emacs prompts for
the file name, the result is affected by `file-name-at-point-functions',
which by default try to guess the file name by looking at point in the
current buffer.  Customize the value of `file-name-at-point-functions'
or set it to nil, if you want only the visited file name and the
current directory to be available on first \\[next-history-element]
request.

Interactively, or if WILDCARDS is non-nil in a call from Lisp,
expand wildcards (if any) and visit multiple files."
  (interactive
   (find-file-read-args "Find file in other frame: "
                        (confirm-nonexistent-file-or-buffer)))
  (let ((value (find-file-noselect filename nil nil wildcards)))
    (if (listp value)
	(progn
	  (setq value (nreverse value))
	  (switch-to-buffer-other-frame (car value))
	  (mapc 'switch-to-buffer (cdr value))
	  value)
      (switch-to-buffer-other-frame value))))

(defun find-file-existing (filename)
   "Edit the existing file FILENAME.
Like \\[find-file], but allow only a file that exists, and do not allow
file names with wildcards."
   (interactive (nbutlast (find-file-read-args "Find existing file: " t)))
   (if (and (not (called-interactively-p 'interactive))
	    (not (file-exists-p filename)))
       (error "%s does not exist" filename)
     (find-file filename)
     (current-buffer)))

(defun find-file--read-only (fun filename wildcards)
  (unless (or (and wildcards find-file-wildcards
		   (not (file-name-quoted-p filename))
		   (string-match "[[*?]" filename))
	      (file-exists-p filename))
    (error "%s does not exist" filename))
  (let ((value (funcall fun filename wildcards)))
    (mapc (lambda (b) (with-current-buffer b (read-only-mode 1)))
	  (if (listp value) value (list value)))
    value))

(defun find-file-read-only (filename &optional wildcards)
  "Edit file FILENAME but don't allow changes.
Like \\[find-file], but marks buffer as read-only.
Use \\[read-only-mode] to permit editing."
  (interactive
   (find-file-read-args "Find file read-only: "
                        (confirm-nonexistent-file-or-buffer)))
  (find-file--read-only #'find-file filename wildcards))

(defun find-file-read-only-other-window (filename &optional wildcards)
  "Edit file FILENAME in another window but don't allow changes.
Like \\[find-file-other-window], but marks buffer as read-only.
Use \\[read-only-mode] to permit editing."
  (interactive
   (find-file-read-args "Find file read-only other window: "
                        (confirm-nonexistent-file-or-buffer)))
  (find-file--read-only #'find-file-other-window filename wildcards))

(defun find-file-read-only-other-frame (filename &optional wildcards)
  "Edit file FILENAME in another frame but don't allow changes.
Like \\[find-file-other-frame], but marks buffer as read-only.
Use \\[read-only-mode] to permit editing."
  (interactive
   (find-file-read-args "Find file read-only other frame: "
                        (confirm-nonexistent-file-or-buffer)))
  (find-file--read-only #'find-file-other-frame filename wildcards))

(defun find-alternate-file-other-window (filename &optional wildcards)
  "Find file FILENAME as a replacement for the file in the next window.
This command does not select that window.

See \\[find-file] for the possible forms of the FILENAME argument.

Interactively, or if WILDCARDS is non-nil in a call from Lisp,
expand wildcards (if any) and replace the file with multiple files."
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
	      "Find alternate file: " file-dir nil
              (confirm-nonexistent-file-or-buffer) file-name)
	     t))))
  (if (one-window-p)
      (find-file-other-window filename wildcards)
    (save-selected-window
      (other-window 1)
      (find-alternate-file filename wildcards))))

;; Defined and used in buffer.c, but not as a DEFVAR_LISP.
(defvar kill-buffer-hook nil
  "Hook run when a buffer is killed.
The buffer being killed is current while the hook is running.
See `kill-buffer'.

This hook is not run for internal or temporary buffers created by
`get-buffer-create' or `generate-new-buffer' with argument
INHIBIT-BUFFER-HOOKS non-nil.

Note: Be careful with let-binding this hook considering it is
frequently used for cleanup.")

(defun find-alternate-file (filename &optional wildcards)
  "Find file FILENAME, select its buffer, kill previous buffer.
If the current buffer now contains an empty file that you just visited
\(presumably by mistake), use this command to visit the file you really want.

See \\[find-file] for the possible forms of the FILENAME argument.

Interactively, or if WILDCARDS is non-nil in a call from Lisp,
expand wildcards (if any) and replace the file with multiple files.

If the current buffer is an indirect buffer, or the base buffer
for one or more indirect buffers, the other buffer(s) are not
killed."
  (interactive
   (let ((file buffer-file-name)
	 (file-name nil)
	 (file-dir nil))
     (and file
	  (setq file-name (file-name-nondirectory file)
		file-dir (file-name-directory file)))
     (list (read-file-name
	    "Find alternate file: " file-dir nil
            (confirm-nonexistent-file-or-buffer) file-name)
	   t)))
  (unless (run-hook-with-args-until-failure 'kill-buffer-query-functions)
    (user-error "Aborted"))
  (and (buffer-modified-p) buffer-file-name
       (not (yes-or-no-p
             (format-message "Kill and replace buffer `%s' without saving it? "
                             (buffer-name))))
       (user-error "Aborted"))
  (let ((obuf (current-buffer))
	(ofile buffer-file-name)
	(onum buffer-file-number)
	(odir dired-directory)
	(otrue buffer-file-truename)
	(oname (buffer-name)))
    ;; Run `kill-buffer-hook' here.  It needs to happen before
    ;; variables like `buffer-file-name' etc are set to nil below,
    ;; because some of the hooks that could be invoked
    ;; (e.g., `save-place-to-alist') depend on those variables.
    ;;
    ;; Note that `kill-buffer-hook' is not what queries whether to
    ;; save a modified buffer visiting a file.  Rather, `kill-buffer'
    ;; asks that itself.  Thus, there's no need to temporarily do
    ;; `(set-buffer-modified-p nil)' before running this hook.
    (run-hooks 'kill-buffer-hook)
    ;; Okay, now we can end-of-life the old buffer.
    (if (get-buffer " **lose**")
	(kill-buffer " **lose**"))
    (rename-buffer " **lose**")
    (unwind-protect
	(progn
	  (unlock-buffer)
	  ;; This prevents us from finding the same buffer
	  ;; if we specified the same file again.
	  (setq buffer-file-name nil)
	  (setq buffer-file-number nil)
	  (setq buffer-file-truename nil)
	  ;; Likewise for dired buffers.
	  (setq dired-directory nil)
          ;; Don't use `find-file' because it may end up using another window
          ;; in some corner cases, e.g. when the selected window is
          ;; softly-dedicated.
	  (let ((newbuf (find-file-noselect filename nil nil wildcards)))
            (switch-to-buffer (if (consp newbuf) (car newbuf) newbuf))))
      (when (eq obuf (current-buffer))
	;; This executes if find-file gets an error
	;; and does not really find anything.
	;; We put things back as they were.
	;; If find-file actually finds something, we kill obuf below.
	(setq buffer-file-name ofile)
	(setq buffer-file-number onum)
	(setq buffer-file-truename otrue)
	(setq dired-directory odir)
	(lock-buffer)
        (if (get-buffer oname)
            (kill-buffer oname))
	(rename-buffer oname)))
    (unless (eq (current-buffer) obuf)
      (with-current-buffer obuf
	;; We already ran these; don't run them again.
	(let (kill-buffer-query-functions kill-buffer-hook)
	  (kill-buffer obuf))))))

;; FIXME we really need to fold the uniquify stuff in here by default,
;; not using advice, and add it to the doc string.
(defun create-file-buffer (filename)
  "Create a suitably named buffer for visiting FILENAME, and return it.
FILENAME (sans directory) is used unchanged if that name is free;
otherwise a string <2> or <3> or ... is appended to get an unused name.

Emacs treats buffers whose names begin with a space as internal buffers.
To avoid confusion when visiting a file whose name begins with a space,
this function prepends a \"|\" to the final result if necessary."
  (let ((lastname (file-name-nondirectory filename)))
    (if (string= lastname "")
	(setq lastname filename))
    (generate-new-buffer (if (string-prefix-p " " lastname)
			     (concat "|" lastname)
			   lastname))))

(defcustom automount-dir-prefix (purecopy "^/tmp_mnt/")
  "Regexp to match the automounter prefix in a directory name."
  :group 'files
  :type 'regexp)
(make-obsolete-variable 'automount-dir-prefix 'directory-abbrev-alist "24.3")

(defvar abbreviated-home-dir nil
  "Regexp matching the user's homedir at the beginning of file name.
The value includes abbreviation according to `directory-abbrev-alist'.")

(defun abbreviate-file-name (filename)
  "Return a version of FILENAME shortened using `directory-abbrev-alist'.
This also substitutes \"~\" for the user's home directory (unless the
home directory is a root directory) and removes automounter prefixes
\(see the variable `automount-dir-prefix').

When this function is first called, it caches the user's home
directory as a regexp in `abbreviated-home-dir', and reuses it
afterwards (so long as the home directory does not change;
if you want to permanently change your home directory after having
started Emacs, set `abbreviated-home-dir' to nil so it will be recalculated)."
  ;; Get rid of the prefixes added by the automounter.
  (save-match-data                      ;FIXME: Why?
    (if (and automount-dir-prefix
	     (string-match automount-dir-prefix filename)
	     (file-exists-p (file-name-directory
			     (substring filename (1- (match-end 0))))))
	(setq filename (substring filename (1- (match-end 0)))))
    ;; Avoid treating /home/foo as /home/Foo during `~' substitution.
    (let ((case-fold-search (file-name-case-insensitive-p filename)))
      ;; If any elt of directory-abbrev-alist matches this name,
      ;; abbreviate accordingly.
      (dolist (dir-abbrev directory-abbrev-alist)
	(if (string-match (car dir-abbrev) filename)
	    (setq filename
		  (concat (cdr dir-abbrev)
			  (substring filename (match-end 0))))))
      ;; Compute and save the abbreviated homedir name.
      ;; We defer computing this until the first time it's needed, to
      ;; give time for directory-abbrev-alist to be set properly.
      ;; We include a slash at the end, to avoid spurious matches
      ;; such as `/usr/foobar' when the home dir is `/usr/foo'.
      (unless abbreviated-home-dir
        (put 'abbreviated-home-dir 'home (expand-file-name "~"))
        (setq abbreviated-home-dir
              (let* ((abbreviated-home-dir "\\`\\'.") ;Impossible regexp.
                     (regexp
                      (concat "\\`"
                              (regexp-quote
                               (abbreviate-file-name
                                (get 'abbreviated-home-dir 'home)))
                              "\\(/\\|\\'\\)")))
                ;; Depending on whether default-directory does or
                ;; doesn't include non-ASCII characters, the value
                ;; of abbreviated-home-dir could be multibyte or
                ;; unibyte.  In the latter case, we need to decode
                ;; it.  Note that this function is called for the
                ;; first time (from startup.el) when
                ;; locale-coding-system is already set up.
                (if (multibyte-string-p regexp)
                    regexp
                  (decode-coding-string regexp
                                        (if (eq system-type 'windows-nt)
                                            'utf-8
                                          locale-coding-system))))))

      ;; If FILENAME starts with the abbreviated homedir,
      ;; and ~ hasn't changed since abbreviated-home-dir was set,
      ;; make it start with `~' instead.
      ;; If ~ has changed, we ignore abbreviated-home-dir rather than
      ;; invalidating it, on the assumption that a change in HOME
      ;; is likely temporary (eg for testing).
      ;; FIXME Is it even worth caching abbreviated-home-dir?
      ;; Ref: https://debbugs.gnu.org/19657#20
      (let (mb1)
        (if (and (string-match abbreviated-home-dir filename)
                 (setq mb1 (match-beginning 1))
	         ;; If the home dir is just /, don't change it.
	         (not (and (= (match-end 0) 1)
			   (= (aref filename 0) ?/)))
	         ;; MS-DOS root directories can come with a drive letter;
	         ;; Novell Netware allows drive letters beyond `Z:'.
	         (not (and (memq system-type '(ms-dos windows-nt cygwin))
			   (string-match "\\`[a-zA-`]:/\\'" filename)))
                 (equal (get 'abbreviated-home-dir 'home)
                        (expand-file-name "~")))
	    (setq filename
		  (concat "~"
			  (substring filename mb1))))
        filename))))

(defun find-buffer-visiting (filename &optional predicate)
  "Return the buffer visiting file FILENAME (a string).
This is like `get-file-buffer', except that it checks for any buffer
visiting the same file, possibly under a different name.
If PREDICATE is non-nil, only buffers satisfying it are eligible,
and others are ignored.
If there is no such live buffer, return nil."
  (let ((predicate (or predicate #'identity))
        (truename (abbreviate-file-name (file-truename filename))))
    (or (let ((buf (get-file-buffer filename)))
          (when (and buf (funcall predicate buf)) buf))
        (let ((list (buffer-list)) found)
          (while (and (not found) list)
            (with-current-buffer (car list)
              (if (and buffer-file-name
                       (string= buffer-file-truename truename)
                       (funcall predicate (current-buffer)))
                  (setq found (car list))))
            (setq list (cdr list)))
          found)
        (let* ((attributes (file-attributes truename))
               (number (nthcdr 10 attributes))
               (list (buffer-list)) found)
          (and buffer-file-numbers-unique
               (car-safe number)       ;Make sure the inode is not just nil.
               (while (and (not found) list)
                 (with-current-buffer (car list)
                   (if (and buffer-file-name
                            (equal buffer-file-number number)
                            ;; Verify this buffer's file number
                            ;; still belongs to its file.
                            (file-exists-p buffer-file-name)
                            (equal (file-attributes buffer-file-truename)
                                   attributes)
                            (funcall predicate (current-buffer)))
                       (setq found (car list))))
                 (setq list (cdr list))))
          found))))

(defcustom find-file-wildcards t
  "Non-nil means file-visiting commands should handle wildcards.
For example, if you specify `*.c', that would visit all the files
whose names match the pattern."
  :group 'files
  :version "20.4"
  :type 'boolean)

(defcustom find-file-suppress-same-file-warnings nil
  "Non-nil means suppress warning messages for symlinked files.
When nil, Emacs prints a warning when visiting a file that is already
visited, but with a different name.  Setting this option to t
suppresses this warning."
  :group 'files
  :version "21.1"
  :type 'boolean)

(defcustom large-file-warning-threshold 10000000
  "Maximum size of file above which a confirmation is requested.
When nil, never request confirmation."
  :group 'files
  :group 'find-file
  :version "22.1"
  :type '(choice integer (const :tag "Never request confirmation" nil)))

(defcustom out-of-memory-warning-percentage nil
  "Warn if file size exceeds this percentage of available free memory.
When nil, never issue warning.  Beware: This probably doesn't do what you
think it does, because \"free\" is pretty hard to define in practice."
  :group 'files
  :group 'find-file
  :version "25.1"
  :type '(choice integer (const :tag "Never issue warning" nil)))

(defcustom query-about-changed-file t
  "If non-nil, query the user when re-visiting a file that has changed.
This happens if the file is already visited in a buffer, the
file was changed externally, and the user re-visits the file.

If nil, don't prompt the user, but instead provide instructions for
reverting, after switching to the buffer with its contents before
the external changes."
  :group 'files
  :group 'find-file
  :version "28.1"
  :type 'boolean)

(declare-function x-popup-dialog "menu.c" (position contents &optional header))

(defun files--ask-user-about-large-file-help-text (op-type size)
  "Format the text that explains the options to open large files in Emacs.
OP-TYPE contains the kind of file operation that will be
performed.  SIZE is the size of the large file."
  (format
   "The file that you want to %s is large (%s), which exceeds the
 threshold above which Emacs asks for confirmation (%s).

 Large files may be slow to edit or navigate so Emacs asks you
 before you try to %s such files.

 You can press:
 'y' to %s the file.
 'n' to abort, and not %s the file.
 'l' (the letter ell) to %s the file literally, which means that
 Emacs will %s the file without doing any format or character code
 conversion and in Fundamental mode, without loading any potentially
 expensive features.

 You can customize the option `large-file-warning-threshold' to be the
 file size, in bytes, from which Emacs will ask for confirmation.  Set
 it to nil to never request confirmation."
   op-type
   size
   (funcall byte-count-to-string-function large-file-warning-threshold)
   op-type
   op-type
   op-type
   op-type
   op-type))

(defun files--ask-user-about-large-file (size op-type filename offer-raw)
  "Query the user about what to do with large files.
Files are \"large\" if file SIZE is larger than `large-file-warning-threshold'.

OP-TYPE specifies the file operation being performed on FILENAME.

If OFFER-RAW is true, give user the additional option to open the
file literally."
  (let ((prompt (format "File %s is large (%s), really %s?"
		        (file-name-nondirectory filename)
		        (funcall byte-count-to-string-function size) op-type)))
    (if (not offer-raw)
        (if (y-or-n-p prompt) nil 'abort)
      (let ((choice
             (car
              (read-multiple-choice
               prompt '((?y "yes")
                        (?n "no")
                        (?l "literally"))
               (files--ask-user-about-large-file-help-text
                op-type (funcall byte-count-to-string-function size))))))
        (cond ((eq choice ?y) nil)
              ((eq choice ?l) 'raw)
              (t 'abort))))))

(defun abort-if-file-too-large (size op-type filename &optional offer-raw)
  "If file SIZE larger than `large-file-warning-threshold', allow user to abort.
OP-TYPE specifies the file operation being performed (for message
to user).  If OFFER-RAW is true, give user the additional option
to open the file literally.  If the user chooses this option,
`abort-if-file-too-large' returns the symbol `raw'.  Otherwise,
it returns nil or exits non-locally."
  (let ((choice (and large-file-warning-threshold size
	             (> size large-file-warning-threshold)
                     ;; No point in warning if we can't read it.
                     (file-readable-p filename)
                     (files--ask-user-about-large-file
                      size op-type filename offer-raw))))
    (when (eq choice 'abort)
      (user-error "Aborted"))
    choice))

(defun warn-maybe-out-of-memory (size)
  "Warn if an attempt to open file of SIZE bytes may run out of memory."
  (when (and (numberp size) (not (zerop size))
	     (integerp out-of-memory-warning-percentage))
    (let ((meminfo (memory-info)))
      (when (consp meminfo)
	(let ((total-free-memory (float (+ (nth 1 meminfo) (nth 3 meminfo)))))
	  (when (> (/ size 1024)
		   (/ (* total-free-memory out-of-memory-warning-percentage)
		      100.0))
	    (warn
	     "You are trying to open a file whose size (%s)
exceeds the %S%% of currently available free memory (%s).
If that fails, try to open it with `find-file-literally'
\(but note that some characters might be displayed incorrectly)."
	     (funcall byte-count-to-string-function size)
	     out-of-memory-warning-percentage
	     (funcall byte-count-to-string-function
                      (* total-free-memory 1024)))))))))

(defun files--message (format &rest args)
  "Like `message', except sometimes don't show the message text.
If the variable `save-silently' is non-nil, the message will not
be visible in the echo area."
  (apply #'message format args)
  (when save-silently (message nil)))

(defun find-file-noselect (filename &optional nowarn rawfile wildcards)
  "Read file FILENAME into a buffer and return the buffer.
If a buffer exists visiting FILENAME, return that one, but
verify that the file has not changed since visited or saved.
The buffer is not selected, just returned to the caller.
Optional second arg NOWARN non-nil means suppress any warning messages.
Optional third arg RAWFILE non-nil means the file is read literally.
Optional fourth arg WILDCARDS non-nil means do wildcard processing
and visit all the matching files.  When wildcards are actually
used and expanded, return a list of buffers that are visiting
the various files."
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
	     (not (file-name-quoted-p filename))
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
	     (attributes (file-attributes truename))
	     (number (nthcdr 10 attributes))
	     ;; Find any buffer for a file that has same truename.
	     (other (and (not buf) (find-buffer-visiting filename))))
	;; Let user know if there is a buffer with the same truename.
	(if other
	    (progn
	      (or nowarn
		  find-file-suppress-same-file-warnings
		  (string-equal filename (buffer-file-name other))
		  (files--message "%s and %s are the same file"
                                  filename (buffer-file-name other)))
	      ;; Optionally also find that buffer.
	      (if (or find-file-existing-other-name find-file-visit-truename)
		  (setq buf other))))
	;; Check to see if the file looks uncommonly large.
	(when (not (or buf nowarn))
          (when (eq (abort-if-file-too-large
                     (file-attribute-size attributes) "open" filename
                     (not rawfile))
                    'raw)
            (setf rawfile t))
	  (warn-maybe-out-of-memory (file-attribute-size attributes)))
	(if buf
	    ;; We are using an existing buffer.
	    (let (nonexistent)
	      (or nowarn
		  (verify-visited-file-modtime buf)
		  (cond ((not (file-exists-p filename))
			 (setq nonexistent t)
			 (message "File %s no longer exists!" filename))
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
                        ((not query-about-changed-file)
                         (message
                          (substitute-command-keys
                           "File %s changed on disk.  \\[revert-buffer-quick] to load new contents%s")
                          (file-name-nondirectory filename)
                          (if (buffer-modified-p buf)
                              " and discard your edits"
                            "")))
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
		  (unless (or nonexistent
			      (eq read-only buffer-file-read-only)
			      (eq read-only buffer-read-only))
		    (when (or nowarn
			      (let* ((new-status
				      (if read-only "read-only" "writable"))
				     (question
				      (format "File %s is %s on disk.  Make buffer %s, too? "
					      buffer-file-name
					      new-status new-status)))
				(y-or-n-p question)))
		      (setq buffer-read-only read-only)))
		  (setq buffer-file-read-only read-only))

		(unless (or (eq (null rawfile) (null find-file-literally))
			    nonexistent
			    ;; It is confusing to ask whether to visit
			    ;; non-literally if they have the file in
			    ;; hexl-mode or image-mode.
			    (memq major-mode '(hexl-mode image-mode)))
		  (if (buffer-modified-p)
		      (if (let ((help-form
				 (format-message
				  (if rawfile "\
The file %s is already visited normally,
and you have edited the buffer.  Now you have asked to visit it literally,
meaning no coding system handling, format conversion, or local variables.
Emacs can visit a file in only one way at a time."
				    "\
The file %s is already visited literally,
meaning no coding system handling, format conversion, or local variables.
You have edited the buffer.  Now you have asked to visit the file normally,
but Emacs can visit a file in only one way at a time.")
				  (file-name-nondirectory filename))))
			    (y-or-n-p
			     (if rawfile "\
Do you want to save the file, and visit it literally instead? " "\
Do you want to save the file, and visit it normally instead? ")))
			  (progn
			    (save-buffer)
			    (find-file-noselect-1 buf filename nowarn
						  rawfile truename number))
			(if (y-or-n-p
			     (if rawfile "\
Do you want to discard your changes, and visit the file literally now? " "\
Do you want to discard your changes, and visit the file normally now? "))
			    (find-file-noselect-1 buf filename nowarn
						  rawfile truename number)
			  (error (if rawfile "File already visited non-literally"
				   "File already visited literally"))))
		    (if (let ((help-form
			       (format-message
				(if rawfile "\
The file %s is already visited normally.
You have asked to visit it literally,
meaning no coding system decoding, format conversion, or local variables.
But Emacs can visit a file in only one way at a time."
				  "\
The file %s is already visited literally,
meaning no coding system decoding, format conversion, or local variables.
You have asked to visit it normally,
but Emacs can visit a file in only one way at a time.")
				(file-name-nondirectory filename))))
			  (y-or-n-p
			   (if rawfile "\
Do you want to revisit the file literally now? " "\
Do you want to revisit the file normally now? ")))
			(find-file-noselect-1 buf filename nowarn
					      rawfile truename number)
		      (error (if rawfile "File already visited non-literally"
			       "File already visited literally"))))))
	      ;; Return the buffer we are using.
	      buf)
	  ;; Create a new buffer.
	  (setq buf (create-file-buffer filename))
	  ;; find-file-noselect-1 may use a different buffer.
	  (find-file-noselect-1 buf filename nowarn
				rawfile truename number))))))

(defun find-file-noselect-1 (buf filename nowarn rawfile truename number)
  (let (error)
    (with-current-buffer buf
      (kill-local-variable 'find-file-literally)
      ;; Needed in case we are re-visiting the file with a different
      ;; text representation.
      (kill-local-variable 'buffer-file-coding-system)
      (kill-local-variable 'cursor-type)
      (let ((inhibit-read-only t))
	(erase-buffer))
      (and (not rawfile)
	   (set-buffer-multibyte t))
      (if rawfile
	  (condition-case ()
	      (let ((inhibit-read-only t)
                    (enable-local-variables nil))
		(insert-file-contents-literally filename t))
	    (file-error
	     (when (and (file-exists-p filename)
			(not (file-readable-p filename)))
	       (kill-buffer buf)
	       (signal 'file-error (list "File is not readable"
					 filename)))
	     ;; Unconditionally set error
	     (setq error t)))
	(condition-case ()
	    (let ((inhibit-read-only t))
	      (insert-file-contents filename t))
	  (file-error
	   (when (and (file-exists-p filename)
		      (not (file-readable-p filename)))
	     (kill-buffer buf)
	     (signal 'file-error (list "File is not readable"
				       filename)))
	   ;; Run find-file-not-found-functions until one returns non-nil.
	   (or (run-hook-with-args-until-success 'find-file-not-found-functions)
	       ;; If they fail too, set error.
	       (setq error t)))))
      ;; Record the file's truename, and maybe use that as visited name.
      (setq buffer-file-truename
            (if (equal filename buffer-file-name)
                truename
	      (abbreviate-file-name (file-truename buffer-file-name))))
      (setq buffer-file-number number)
      (if find-file-visit-truename
	  (setq buffer-file-name (expand-file-name buffer-file-truename)))
      ;; Set buffer's default directory to that of the file.
      (setq default-directory (file-name-directory buffer-file-name))
      ;; Turn off backup files for certain file names.  Since
      ;; this is a permanent local, the major mode won't eliminate it.
      (and backup-enable-predicate
	   (not (funcall backup-enable-predicate buffer-file-name))
           (setq-local backup-inhibited t))
      (if rawfile
	  (let ((enable-local-variables nil))
	    (set-buffer-multibyte nil)
	    (setq buffer-file-coding-system 'no-conversion)
	    (set-buffer-major-mode buf)
	    (setq-local find-file-literally t))
	(after-find-file error (not nowarn)))
      (current-buffer))))

(defun insert-file-contents-literally (filename &optional visit beg end replace)
  "Like `insert-file-contents', but only read in the file literally.
See `insert-file-contents' for an explanation of the parameters.
A buffer may be modified in several ways after reading into the buffer,
due to Emacs features such as format decoding, character code
conversion, `find-file-hook', automatic uncompression, etc.

This function ensures that none of these modifications will take place.

Unlike `find-file-literally', this function does not make the
buffer unibyte, so if this function is used when handling
binary (non-character) data, it can be convenient to make the
buffer unibyte first.  This isn't, strictly speaking, necessary,
because multibyte buffers can also deal with raw bytes.  See info
node `(elisp)Character Codes' for details."
  (let ((format-alist nil)
	(after-insert-file-functions nil)
	(coding-system-for-read 'no-conversion)
	(coding-system-for-write 'no-conversion)
        (inhibit-file-name-handlers
         ;; FIXME: Yuck!!  We should turn insert-file-contents-literally
         ;; into a file operation instead!
         (append '(jka-compr-handler image-file-handler epa-file-handler)
	         (and (eq inhibit-file-name-operation 'insert-file-contents)
		      inhibit-file-name-handlers)))
        (inhibit-file-name-operation 'insert-file-contents))
    (insert-file-contents filename visit beg end replace)))

(defun insert-file-1 (filename insert-func)
  (if (file-directory-p filename)
      (signal 'file-error (list "Opening input file" "Is a directory"
                                filename)))
  ;; Check whether the file is uncommonly large
  (abort-if-file-too-large (file-attribute-size (file-attributes filename))
			   "insert" filename)
  (let* ((buffer (find-buffer-visiting (abbreviate-file-name (file-truename filename))
                                       #'buffer-modified-p))
         (tem (funcall insert-func filename)))
    (push-mark (+ (point) (car (cdr tem))))
    (when buffer
      (message "File %s already visited and modified in buffer %s"
               filename (buffer-name buffer)))))

(defun insert-file-literally (filename)
  "Insert contents of file FILENAME into buffer after point with no conversion.

This function is meant for the user to run interactively.
Don't call it from programs!  Use `insert-file-contents-literally' instead.
\(Its calling sequence is different; see its documentation)."
  (declare (interactive-only insert-file-contents-literally))
  (interactive "*fInsert file literally: ")
  (insert-file-1 filename #'insert-file-contents-literally))

(defvar find-file-literally nil
  "Non-nil if this buffer was made by `find-file-literally' or equivalent.
This has the `permanent-local' property, which takes effect if you
make the variable buffer-local.")
(put 'find-file-literally 'permanent-local t)

(defun find-file-literally (filename)
  "Visit file FILENAME with no conversion of any kind.
Format conversion and character code conversion are both disabled,
and multibyte characters are disabled in the resulting buffer.
The major mode used is Fundamental mode regardless of the file name,
and local variable specifications in the file are ignored.
Automatic uncompression and adding a newline at the end of the
file due to `require-final-newline' is also disabled.

If Emacs already has a buffer that is visiting the file,
this command asks you whether to visit it literally instead.

In non-interactive use, the value is the buffer where the file is
visited literally.  If the file was visited in a buffer before
this command was invoked, it will reuse the existing buffer,
regardless of whether it was created literally or not; however,
the contents of that buffer will be the literal text of the file
without any conversions.

In a Lisp program, if you want to be sure of accessing a file's
contents literally, you should create a temporary buffer and then read
the file contents into it using `insert-file-contents-literally'."
  (interactive
   (list (read-file-name
  	  "Find file literally: " nil default-directory
  	  (confirm-nonexistent-file-or-buffer))))
  (switch-to-buffer (find-file-noselect filename nil t)))

(defun after-find-file (&optional error warn noauto
				  _after-find-file-from-revert-buffer
				  nomodes)
  "Called after finding a file and by the default revert function.
Sets buffer mode, parses file-local and directory-local variables.
Optional args ERROR, WARN, and NOAUTO: ERROR non-nil means there was an
error in reading the file.  WARN non-nil means warn if there
exists an auto-save file more recent than the visited file.
NOAUTO means don't mess with auto-save mode.
Fourth arg AFTER-FIND-FILE-FROM-REVERT-BUFFER is ignored
\(see `revert-buffer-in-progress-p' for similar functionality).
Fifth arg NOMODES non-nil means don't alter the file's modes.
Finishes by calling the functions in `find-file-hook'
unless NOMODES is non-nil."
  (setq buffer-read-only (not (file-writable-p buffer-file-name)))
  (if noninteractive
      nil
    (let* (not-serious
	   (msg
	    (cond
	     ((and error (file-exists-p buffer-file-name))
	      (setq buffer-read-only t)
	      "File exists, but cannot be read")
	     ((and error (file-symlink-p buffer-file-name))
	      "Symbolic link that points to nonexistent file")
	     ((not buffer-read-only)
	      (if (and
                   ;; No need to warn if buffer is auto-saved
		   ;; under the name of the visited file.
		   (not (and buffer-file-name
			     auto-save-visited-file-name))
		   (file-newer-than-file-p (or buffer-auto-save-file-name
					       (make-auto-save-file-name))
					   buffer-file-name))
		  (format "%s has auto save data; consider M-x recover-this-file"
			  (file-name-nondirectory buffer-file-name))
		(setq not-serious t)
		(if error "(New file)" nil)))
	     ((not error)
	      (setq not-serious t)
	      "Note: file is write protected")
	     ((file-accessible-directory-p default-directory)
	      "File not found and directory write-protected")
	     (t
	      (setq buffer-read-only nil)
	      (unless (file-directory-p default-directory)
		"Use M-x make-directory RET RET to create the directory and its parents")))))
      (when (and warn msg)
	(message "%s" msg)
	(or not-serious (sit-for 1 t))))
    (when (and auto-save-default (not noauto))
      (auto-save-mode 1)))
  ;; Make people do a little extra work (C-x C-q)
  ;; before altering a backup file.
  ;; When a file is marked read-only,
  ;; make the buffer read-only even if root is looking at it.
  (unless buffer-read-only
    (when (or (backup-file-name-p buffer-file-name)
	      (let ((modes (file-modes (buffer-file-name))))
		(and modes (zerop (logand modes #o222)))))
      (setq buffer-read-only t)))
  (unless nomodes
    (when (and view-read-only view-mode)
      (view-mode -1))
    (normal-mode t)
    ;; If requested, add a newline at the end of the file.
    (and (memq require-final-newline '(visit visit-save))
	 (> (point-max) (point-min))
	 (/= (char-after (1- (point-max))) ?\n)
	 (not (and (eq selective-display t)
		   (= (char-after (1- (point-max))) ?\r)))
	 (not buffer-read-only)
	 (save-excursion
	   (goto-char (point-max))
	   (ignore-errors (insert "\n"))))
    (when (and buffer-read-only
	       view-read-only
	       (not (eq (get major-mode 'mode-class) 'special)))
      (view-mode-enter))
    (run-hooks 'find-file-hook)))

(define-obsolete-function-alias 'report-errors 'with-demoted-errors "25.1")

(defun normal-mode (&optional find-file)
  "Choose the major mode for this buffer automatically.
Also sets up any specified local variables of the file or its directory.
Uses the visited file name, the -*- line, and the local variables spec.

This function is called automatically from `find-file'.  In that case,
we may set up the file-specified mode and local variables,
depending on the value of `enable-local-variables'.
In addition, if `local-enable-local-variables' is nil, we do
not set local variables (though we do notice a mode specified with -*-.)

`enable-local-variables' is ignored if you run `normal-mode' interactively,
or from Lisp without specifying the optional argument FIND-FILE;
in that case, this function acts as if `enable-local-variables' were t."
  (interactive)
  (kill-all-local-variables)
  (unless delay-mode-hooks
    (run-hooks 'change-major-mode-after-body-hook
               'after-change-major-mode-hook))
  (let ((enable-local-variables (or (not find-file) enable-local-variables)))
    ;; FIXME this is less efficient than it could be, since both
    ;; s-a-m and h-l-v may parse the same regions, looking for "mode:".
    (with-demoted-errors "File mode specification error: %s"
      (set-auto-mode))
    ;; `delay-mode-hooks' being non-nil will have prevented the major
    ;; mode's call to `run-mode-hooks' from calling
    ;; `hack-local-variables'.  In that case, call it now.
    (when delay-mode-hooks
      (with-demoted-errors "File local-variables error: %s"
        (hack-local-variables 'no-mode))))
  ;; Turn font lock off and on, to make sure it takes account of
  ;; whatever file local variables are relevant to it.
  (when (and font-lock-mode
             ;; Font-lock-mode (now in font-core.el) can be ON when
             ;; font-lock.el still hasn't been loaded.
             (boundp 'font-lock-keywords)
             (eq (car font-lock-keywords) t))
    (setq font-lock-keywords (cadr font-lock-keywords))
    (font-lock-mode 1)))

(defcustom auto-mode-case-fold t
  "Non-nil means to try second pass through `auto-mode-alist'.
This means that if the first case-sensitive search through the alist fails
to find a matching major mode, a second case-insensitive search is made.
On systems with case-insensitive file names, this variable is ignored,
since only a single case-insensitive search through the alist is made."
  :group 'files
  :version "22.1"
  :type 'boolean)

(defvar auto-mode-alist
  ;; Note: The entries for the modes defined in cc-mode.el (c-mode,
  ;; c++-mode, java-mode and more) are added through autoload
  ;; directives in that file.  That way is discouraged since it
  ;; spreads out the definition of the initial value.
  (mapcar
   (lambda (elt)
     (cons (purecopy (car elt)) (cdr elt)))
   `(;; do this first, so that .html.pl is Polish html, not Perl
     ("\\.[sx]?html?\\(\\.[a-zA-Z_]+\\)?\\'" . mhtml-mode)
     ("\\.svgz?\\'" . image-mode)
     ("\\.svgz?\\'" . xml-mode)
     ("\\.x[bp]m\\'" . image-mode)
     ("\\.x[bp]m\\'" . c-mode)
     ("\\.p[bpgn]m\\'" . image-mode)
     ("\\.tiff?\\'" . image-mode)
     ("\\.gif\\'" . image-mode)
     ("\\.png\\'" . image-mode)
     ("\\.jpe?g\\'" . image-mode)
     ("\\.te?xt\\'" . text-mode)
     ("\\.[tT]e[xX]\\'" . tex-mode)
     ("\\.ins\\'" . tex-mode)		;Installation files for TeX packages.
     ("\\.ltx\\'" . latex-mode)
     ("\\.dtx\\'" . doctex-mode)
     ("\\.org\\'" . org-mode)
     ;; .dir-locals.el is not really Elisp.  Could use the
     ;; `dir-locals-file' constant if it weren't defined below.
     ("\\.dir-locals\\(?:-2\\)?\\.el\\'" . lisp-data-mode)
     ("eww-bookmarks\\'" . lisp-data-mode)
     ("tramp\\'" . lisp-data-mode)
     ("/archive-contents\\'" . lisp-data-mode)
     ("places\\'" . lisp-data-mode)
     ("\\.emacs-places\\'" . lisp-data-mode)
     ("\\.el\\'" . emacs-lisp-mode)
     ("Project\\.ede\\'" . emacs-lisp-mode)
     ("\\.\\(scm\\|stk\\|ss\\|sch\\)\\'" . scheme-mode)
     ("\\.l\\'" . lisp-mode)
     ("\\.li?sp\\'" . lisp-mode)
     ("\\.[fF]\\'" . fortran-mode)
     ("\\.for\\'" . fortran-mode)
     ("\\.p\\'" . pascal-mode)
     ("\\.pas\\'" . pascal-mode)
     ("\\.\\(dpr\\|DPR\\)\\'" . delphi-mode)
     ("\\.\\([pP]\\([Llm]\\|erl\\|od\\)\\|al\\)\\'" . perl-mode)
     ("Imakefile\\'" . makefile-imake-mode)
     ("Makeppfile\\(?:\\.mk\\)?\\'" . makefile-makepp-mode) ; Put this before .mk
     ("\\.makepp\\'" . makefile-makepp-mode)
     ,@(if (memq system-type '(berkeley-unix darwin))
	   '(("\\.mk\\'" . makefile-bsdmake-mode)
	     ("\\.make\\'" . makefile-bsdmake-mode)
	     ("GNUmakefile\\'" . makefile-gmake-mode)
	     ("[Mm]akefile\\'" . makefile-bsdmake-mode))
	 '(("\\.mk\\'" . makefile-gmake-mode)	; Might be any make, give GNU the host advantage
	   ("\\.make\\'" . makefile-gmake-mode)
	   ("[Mm]akefile\\'" . makefile-gmake-mode)))
     ("\\.am\\'" . makefile-automake-mode)
     ;; Less common extensions come here
     ;; so more common ones above are found faster.
     ("\\.texinfo\\'" . texinfo-mode)
     ("\\.te?xi\\'" . texinfo-mode)
     ("\\.[sS]\\'" . asm-mode)
     ("\\.asm\\'" . asm-mode)
     ("\\.css\\'" . css-mode)
     ("\\.mixal\\'" . mixal-mode)
     ("\\.gcov\\'" . compilation-mode)
     ;; Besides .gdbinit, gdb documents other names to be usable for init
     ;; files, cross-debuggers can use something like
     ;; .PROCESSORNAME-gdbinit so that the host and target gdbinit files
     ;; don't interfere with each other.
     ("/\\.[a-z0-9-]*gdbinit" . gdb-script-mode)
     ;; GDB 7.5 introduced OBJFILE-gdb.gdb script files; e.g. a file
     ;; named 'emacs-gdb.gdb', if it exists, will be automatically
     ;; loaded when GDB reads an objfile called 'emacs'.
     ("-gdb\\.gdb" . gdb-script-mode)
     ("[cC]hange\\.?[lL]og?\\'" . change-log-mode)
     ("[cC]hange[lL]og[-.][0-9]+\\'" . change-log-mode)
     ("\\$CHANGE_LOG\\$\\.TXT" . change-log-mode)
     ("\\.scm\\.[0-9]*\\'" . scheme-mode)
     ("\\.[ckz]?sh\\'\\|\\.shar\\'\\|/\\.z?profile\\'" . sh-mode)
     ("\\.bash\\'" . sh-mode)
     ("/PKGBUILD\\'" . sh-mode)
     ("\\(/\\|\\`\\)\\.\\(bash_\\(profile\\|history\\|log\\(in\\|out\\)\\)\\|z?log\\(in\\|out\\)\\)\\'" . sh-mode)
     ("\\(/\\|\\`\\)\\.\\(shrc\\|zshrc\\|m?kshrc\\|bashrc\\|t?cshrc\\|esrc\\)\\'" . sh-mode)
     ("\\(/\\|\\`\\)\\.\\([kz]shenv\\|xinitrc\\|startxrc\\|xsession\\)\\'" . sh-mode)
     ("\\.m?spec\\'" . sh-mode)
     ("\\.m[mes]\\'" . nroff-mode)
     ("\\.man\\'" . nroff-mode)
     ("\\.sty\\'" . latex-mode)
     ("\\.cl[so]\\'" . latex-mode)		;LaTeX 2e class option
     ("\\.bbl\\'" . latex-mode)
     ("\\.bib\\'" . bibtex-mode)
     ("\\.bst\\'" . bibtex-style-mode)
     ("\\.sql\\'" . sql-mode)
     ;; These .m4 files are Autoconf files.
     ("\\(acinclude\\|aclocal\\|acsite\\)\\.m4\\'" . autoconf-mode)
     ("\\.m[4c]\\'" . m4-mode)
     ("\\.mf\\'" . metafont-mode)
     ("\\.mp\\'" . metapost-mode)
     ("\\.vhdl?\\'" . vhdl-mode)
     ("\\.article\\'" . text-mode)
     ("\\.letter\\'" . text-mode)
     ("\\.i?tcl\\'" . tcl-mode)
     ("\\.exp\\'" . tcl-mode)
     ("\\.itk\\'" . tcl-mode)
     ("\\.icn\\'" . icon-mode)
     ("\\.sim\\'" . simula-mode)
     ("\\.mss\\'" . scribe-mode)
     ;; The Fortran standard does not say anything about file extensions.
     ;; .f90 was widely used for F90, now we seem to be trapped into
     ;; using a different extension for each language revision.
     ;; Anyway, the following extensions are supported by gfortran.
     ("\\.f9[05]\\'" . f90-mode)
     ("\\.f0[38]\\'" . f90-mode)
     ("\\.indent\\.pro\\'" . fundamental-mode) ; to avoid idlwave-mode
     ("\\.\\(pro\\|PRO\\)\\'" . idlwave-mode)
     ("\\.srt\\'" . srecode-template-mode)
     ("\\.prolog\\'" . prolog-mode)
     ("\\.tar\\'" . tar-mode)
     ;; The list of archive file extensions should be in sync with
     ;; `auto-coding-alist' with `no-conversion' coding system.
     ("\\.\\(\
arc\\|zip\\|lzh\\|lha\\|zoo\\|[jew]ar\\|xpi\\|rar\\|cbr\\|7z\\|squashfs\\|\
ARC\\|ZIP\\|LZH\\|LHA\\|ZOO\\|[JEW]AR\\|XPI\\|RAR\\|CBR\\|7Z\\|SQUASHFS\\)\\'" . archive-mode)
     ("\\.oxt\\'" . archive-mode) ;(Open|Libre)Office extensions.
     ("\\.\\(deb\\|[oi]pk\\)\\'" . archive-mode) ; Debian/Opkg packages.
     ;; Mailer puts message to be edited in
     ;; /tmp/Re.... or Message
     ("\\`/tmp/Re" . text-mode)
     ("/Message[0-9]*\\'" . text-mode)
     ;; some news reader is reported to use this
     ("\\`/tmp/fol/" . text-mode)
     ("\\.oak\\'" . scheme-mode)
     ("\\.sgml?\\'" . sgml-mode)
     ("\\.x[ms]l\\'" . xml-mode)
     ("\\.dbk\\'" . xml-mode)
     ("\\.dtd\\'" . sgml-mode)
     ("\\.ds\\(ss\\)?l\\'" . dsssl-mode)
     ("\\.js[mx]?\\'" . javascript-mode)
     ;; https://en.wikipedia.org/wiki/.har
     ("\\.har\\'" . javascript-mode)
     ("\\.json\\'" . javascript-mode)
     ("\\.[ds]?va?h?\\'" . verilog-mode)
     ("\\.by\\'" . bovine-grammar-mode)
     ("\\.wy\\'" . wisent-grammar-mode)
     ;; .emacs or .gnus or .viper following a directory delimiter in
     ;; Unix or MS-DOS syntax.
     ("[:/\\]\\..*\\(emacs\\|gnus\\|viper\\)\\'" . emacs-lisp-mode)
     ("\\`\\..*emacs\\'" . emacs-lisp-mode)
     ;; _emacs following a directory delimiter in MS-DOS syntax
     ("[:/]_emacs\\'" . emacs-lisp-mode)
     ("/crontab\\.X*[0-9]+\\'" . shell-script-mode)
     ("\\.ml\\'" . lisp-mode)
     ;; Linux-2.6.9 uses some different suffix for linker scripts:
     ;; "ld", "lds", "lds.S", "lds.in", "ld.script", and "ld.script.balo".
     ;; eCos uses "ld" and "ldi".  Netbsd uses "ldscript.*".
     ("\\.ld[si]?\\'" . ld-script-mode)
     ("ld\\.?script\\'" . ld-script-mode)
     ;; .xs is also used for ld scripts, but seems to be more commonly
     ;; associated with Perl .xs files (C with Perl bindings).  (Bug#7071)
     ("\\.xs\\'" . c-mode)
     ;; Explained in binutils ld/genscripts.sh.  Eg:
     ;; A .x script file is the default script.
     ;; A .xr script is for linking without relocation (-r flag).  Etc.
     ("\\.x[abdsru]?[cnw]?\\'" . ld-script-mode)
     ("\\.zone\\'" . dns-mode)
     ("\\.soa\\'" . dns-mode)
     ;; Common Lisp ASDF package system.
     ("\\.asd\\'" . lisp-mode)
     ("\\.\\(asn\\|mib\\|smi\\)\\'" . snmp-mode)
     ("\\.\\(as\\|mi\\|sm\\)2\\'" . snmpv2-mode)
     ("\\.\\(diffs?\\|patch\\|rej\\)\\'" . diff-mode)
     ("\\.\\(dif\\|pat\\)\\'" . diff-mode) ; for MS-DOS
     ("\\.[eE]?[pP][sS]\\'" . ps-mode)
     ("\\.\\(?:PDF\\|DVI\\|OD[FGPST]\\|DOCX\\|XLSX?\\|PPTX?\\|pdf\\|djvu\\|dvi\\|od[fgpst]\\|docx\\|xlsx?\\|pptx?\\)\\'" . doc-view-mode-maybe)
     ("configure\\.\\(ac\\|in\\)\\'" . autoconf-mode)
     ("\\.s\\(v\\|iv\\|ieve\\)\\'" . sieve-mode)
     ("BROWSE\\'" . ebrowse-tree-mode)
     ("\\.ebrowse\\'" . ebrowse-tree-mode)
     ("#\\*mail\\*" . mail-mode)
     ("\\.g\\'" . antlr-mode)
     ("\\.mod\\'" . m2-mode)
     ("\\.ses\\'" . ses-mode)
     ("\\.docbook\\'" . sgml-mode)
     ("\\.com\\'" . dcl-mode)
     ("/config\\.\\(?:bat\\|log\\)\\'" . fundamental-mode)
     ("/\\.\\(authinfo\\|netrc\\)\\'" . authinfo-mode)
     ;; Windows candidates may be opened case sensitively on Unix
     ("\\.\\(?:[iI][nN][iI]\\|[lL][sS][tT]\\|[rR][eE][gG]\\|[sS][yY][sS]\\)\\'" . conf-mode)
     ("\\.la\\'" . conf-unix-mode)
     ("\\.ppd\\'" . conf-ppd-mode)
     ("java.+\\.conf\\'" . conf-javaprop-mode)
     ("\\.properties\\(?:\\.[a-zA-Z0-9._-]+\\)?\\'" . conf-javaprop-mode)
     ("\\.toml\\'" . conf-toml-mode)
     ("\\.desktop\\'" . conf-desktop-mode)
     ("/\\.redshift\\.conf\\'" . conf-windows-mode)
     ("\\`/etc/\\(?:DIR_COLORS\\|ethers\\|.?fstab\\|.*hosts\\|lesskey\\|login\\.?de\\(?:fs\\|vperm\\)\\|magic\\|mtab\\|pam\\.d/.*\\|permissions\\(?:\\.d/.+\\)?\\|protocols\\|rpc\\|services\\)\\'" . conf-space-mode)
     ("\\`/etc/\\(?:acpid?/.+\\|aliases\\(?:\\.d/.+\\)?\\|default/.+\\|group-?\\|hosts\\..+\\|inittab\\|ksysguarddrc\\|opera6rc\\|passwd-?\\|shadow-?\\|sysconfig/.+\\)\\'" . conf-mode)
     ;; ChangeLog.old etc.  Other change-log-mode entries are above;
     ;; this has lower priority to avoid matching changelog.sgml etc.
     ("[cC]hange[lL]og[-.][-0-9a-z]+\\'" . change-log-mode)
     ;; either user's dot-files or under /etc or some such
     ("/\\.?\\(?:gitconfig\\|gnokiirc\\|hgrc\\|kde.*rc\\|mime\\.types\\|wgetrc\\)\\'" . conf-mode)
     ;; alas not all ~/.*rc files are like this
     ("/\\.\\(?:asound\\|enigma\\|fetchmail\\|gltron\\|gtk\\|hxplayer\\|mairix\\|mbsync\\|msmtp\\|net\\|neverball\\|nvidia-settings-\\|offlineimap\\|qt/.+\\|realplayer\\|reportbug\\|rtorrent\\.\\|screen\\|scummvm\\|sversion\\|sylpheed/.+\\|xmp\\)rc\\'" . conf-mode)
     ("/\\.\\(?:gdbtkinit\\|grip\\|mpdconf\\|notmuch-config\\|orbital/.+txt\\|rhosts\\|tuxracer/options\\)\\'" . conf-mode)
     ("/\\.?X\\(?:default\\|resource\\|re\\)s\\>" . conf-xdefaults-mode)
     ("/X11.+app-defaults/\\|\\.ad\\'" . conf-xdefaults-mode)
     ("/X11.+locale/.+/Compose\\'" . conf-colon-mode)
     ;; this contains everything twice, with space and with colon :-(
     ("/X11.+locale/compose\\.dir\\'" . conf-javaprop-mode)
     ;; Get rid of any trailing .n.m and try again.
     ;; This is for files saved by cvs-merge that look like .#<file>.<rev>
     ;; or .#<file>.<rev>-<rev> or VC's <file>.~<rev>~.
     ;; Using mode nil rather than `ignore' would let the search continue
     ;; through this list (with the shortened name) rather than start over.
     ("\\.~?[0-9]+\\.[0-9][-.0-9]*~?\\'" nil t)
     ("\\.\\(?:orig\\|in\\|[bB][aA][kK]\\)\\'" nil t)
     ;; This should come after "in" stripping (e.g. config.h.in).
     ;; *.cf, *.cfg, *.conf, *.config[.local|.de_DE.UTF8|...], */config
     ("[/.]c\\(?:on\\)?f\\(?:i?g\\)?\\(?:\\.[a-zA-Z0-9._-]+\\)?\\'" . conf-mode-maybe)
     ;; The following should come after the ChangeLog pattern
     ;; for the sake of ChangeLog.1, etc.
     ;; and after the .scm.[0-9] and CVS' <file>.<rev> patterns too.
     ("\\.[1-9]\\'" . nroff-mode)
     ;; Image file types probably supported by `image-convert'.
     ("\\.art\\'" . image-mode)
     ("\\.avs\\'" . image-mode)
     ("\\.bmp\\'" . image-mode)
     ("\\.cmyk\\'" . image-mode)
     ("\\.cmyka\\'" . image-mode)
     ("\\.crw\\'" . image-mode)
     ("\\.dcr\\'" . image-mode)
     ("\\.dcx\\'" . image-mode)
     ("\\.dng\\'" . image-mode)
     ("\\.dpx\\'" . image-mode)
     ("\\.fax\\'" . image-mode)
     ("\\.hrz\\'" . image-mode)
     ("\\.icb\\'" . image-mode)
     ("\\.icc\\'" . image-mode)
     ("\\.icm\\'" . image-mode)
     ("\\.ico\\'" . image-mode)
     ("\\.icon\\'" . image-mode)
     ("\\.jbg\\'" . image-mode)
     ("\\.jbig\\'" . image-mode)
     ("\\.jng\\'" . image-mode)
     ("\\.jnx\\'" . image-mode)
     ("\\.miff\\'" . image-mode)
     ("\\.mng\\'" . image-mode)
     ("\\.mvg\\'" . image-mode)
     ("\\.otb\\'" . image-mode)
     ("\\.p7\\'" . image-mode)
     ("\\.pcx\\'" . image-mode)
     ("\\.pdb\\'" . image-mode)
     ("\\.pfa\\'" . image-mode)
     ("\\.pfb\\'" . image-mode)
     ("\\.picon\\'" . image-mode)
     ("\\.pict\\'" . image-mode)
     ("\\.rgb\\'" . image-mode)
     ("\\.rgba\\'" . image-mode)
     ("\\.tga\\'" . image-mode)
     ("\\.wbmp\\'" . image-mode)
     ("\\.webp\\'" . image-mode)
     ("\\.wmf\\'" . image-mode)
     ("\\.wpg\\'" . image-mode)
     ("\\.xcf\\'" . image-mode)
     ("\\.xmp\\'" . image-mode)
     ("\\.xwd\\'" . image-mode)
     ("\\.yuv\\'" . image-mode)))
  "Alist of file name patterns vs corresponding major mode functions.
Each element looks like (REGEXP . FUNCTION) or (REGEXP FUNCTION NON-NIL).
\(NON-NIL stands for anything that is not nil; the value does not matter.)
Visiting a file whose name matches REGEXP specifies FUNCTION as the
mode function to use.  FUNCTION will be called, unless it is nil.

If the element has the form (REGEXP FUNCTION NON-NIL), then after
calling FUNCTION (if it's not nil), we delete the suffix that matched
REGEXP and search the list again for another match.

The extensions whose FUNCTION is `archive-mode' should also
appear in `auto-coding-alist' with `no-conversion' coding system.

See also `interpreter-mode-alist', which detects executable script modes
based on the interpreters they specify to run,
and `magic-mode-alist', which determines modes based on file contents.")
(put 'auto-mode-alist 'risky-local-variable t)

(defun conf-mode-maybe ()
  "Select Conf mode or XML mode according to start of file."
  (if (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  (looking-at "<\\?xml \\|<!-- \\|<!DOCTYPE ")))
      (xml-mode)
    (conf-mode)))

(defvar interpreter-mode-alist
  ;; Note: The entries for the modes defined in cc-mode.el (awk-mode
  ;; and pike-mode) are added through autoload directives in that
  ;; file.  That way is discouraged since it spreads out the
  ;; definition of the initial value.
  (mapcar
   (lambda (l)
     (cons (purecopy (car l)) (cdr l)))
   '(("\\(mini\\)?perl5?" . perl-mode)
     ("wishx?" . tcl-mode)
     ("tcl\\(sh\\)?" . tcl-mode)
     ("expect" . tcl-mode)
     ("octave" . octave-mode)
     ("scm" . scheme-mode)
     ("[acjkwz]sh" . sh-mode)
     ("r?bash2?" . sh-mode)
     ("dash" . sh-mode)
     ("mksh" . sh-mode)
     ("\\(dt\\|pd\\|w\\)ksh" . sh-mode)
     ("es" . sh-mode)
     ("i?tcsh" . sh-mode)
     ("oash" . sh-mode)
     ("rc" . sh-mode)
     ("rpm" . sh-mode)
     ("sh5?" . sh-mode)
     ("tail" . text-mode)
     ("more" . text-mode)
     ("less" . text-mode)
     ("pg" . text-mode)
     ("make" . makefile-gmake-mode)		; Debian uses this
     ("guile" . scheme-mode)
     ("clisp" . lisp-mode)
     ("emacs" . emacs-lisp-mode)))
  "Alist mapping interpreter names to major modes.
This is used for files whose first lines match `auto-mode-interpreter-regexp'.
Each element looks like (REGEXP . MODE).
If REGEXP matches the entire name (minus any directory part) of
the interpreter specified in the first line of a script, enable
major mode MODE.

See also `auto-mode-alist'.")

(define-obsolete-variable-alias 'inhibit-first-line-modes-regexps
  'inhibit-file-local-variables-regexps "24.1")

;; TODO really this should be a list of modes (eg tar-mode), not regexps,
;; because we are duplicating info from auto-mode-alist.
;; TODO many elements of this list are also in auto-coding-alist.
(defvar inhibit-local-variables-regexps
  (mapcar 'purecopy '("\\.tar\\'" "\\.t[bg]z\\'"
		      "\\.arc\\'" "\\.zip\\'" "\\.lzh\\'" "\\.lha\\'"
		      "\\.zoo\\'" "\\.[jew]ar\\'" "\\.xpi\\'" "\\.rar\\'"
		      "\\.7z\\'"
		      "\\.sx[dmicw]\\'" "\\.odt\\'"
		      "\\.diff\\'" "\\.patch\\'"
		      "\\.tiff?\\'" "\\.gif\\'" "\\.png\\'" "\\.jpe?g\\'"))
  "List of regexps matching file names in which to ignore local variables.
This includes `-*-' lines as well as trailing \"Local Variables\" sections.
Files matching this list are typically binary file formats.
They may happen to contain sequences that look like local variable
specifications, but are not really, or they may be containers for
member files with their own local variable sections, which are
not appropriate for the containing file.
The function `inhibit-local-variables-p' uses this.")

(define-obsolete-variable-alias 'inhibit-first-line-modes-suffixes
  'inhibit-local-variables-suffixes "24.1")

(defvar inhibit-local-variables-suffixes nil
  "List of regexps matching suffixes to remove from file names.
The function `inhibit-local-variables-p' uses this: when checking
a file name, it first discards from the end of the name anything that
matches one of these regexps.")

;; Can't think of any situation in which you'd want this to be nil...
(defvar inhibit-local-variables-ignore-case t
  "Non-nil means `inhibit-local-variables-p' ignores case.")

(defun inhibit-local-variables-p ()
  "Return non-nil if file local variables should be ignored.
This checks the file (or buffer) name against `inhibit-local-variables-regexps'
and `inhibit-local-variables-suffixes'.  If
`inhibit-local-variables-ignore-case' is non-nil, this ignores case."
  (let ((temp inhibit-local-variables-regexps)
	(name (if buffer-file-name
		  (file-name-sans-versions buffer-file-name)
		(buffer-name)))
	(case-fold-search inhibit-local-variables-ignore-case))
    (while (let ((sufs inhibit-local-variables-suffixes))
	     (while (and sufs (not (string-match (car sufs) name)))
	       (setq sufs (cdr sufs)))
	     sufs)
      (setq name (substring name 0 (match-beginning 0))))
    (while (and temp
		(not (string-match (car temp) name)))
      (setq temp (cdr temp)))
    temp))

(defvar auto-mode-interpreter-regexp
  (purecopy "#![ \t]?\\([^ \t\n]*\
/bin/env[ \t]\\)?\\([^ \t\n]+\\)")
  "Regexp matching interpreters, for file mode determination.
This regular expression is matched against the first line of a file
to determine the file's mode in `set-auto-mode'.  If it matches, the file
is assumed to be interpreted by the interpreter matched by the second group
of the regular expression.  The mode is then determined as the mode
associated with that interpreter in `interpreter-mode-alist'.")

(defvar magic-mode-alist nil
  "Alist of buffer beginnings vs. corresponding major mode functions.
Each element looks like (REGEXP . FUNCTION) or (MATCH-FUNCTION . FUNCTION).
After visiting a file, if REGEXP matches the text at the beginning of the
buffer (case-sensitively), or calling MATCH-FUNCTION returns non-nil,
`normal-mode' will call FUNCTION rather than allowing `auto-mode-alist' to
decide the buffer's major mode.

If FUNCTION is nil, then it is not called.  (That is a way of saying
\"allow `auto-mode-alist' to decide for these files.\")")
(put 'magic-mode-alist 'risky-local-variable t)

(defvar magic-fallback-mode-alist
  (purecopy
  `((image-type-auto-detected-p . image-mode)
    ("\\(PK00\\)?[P]K\003\004" . archive-mode) ; zip
    ;; The < comes before the groups (but the first) to reduce backtracking.
    ;; TODO: UTF-16 <?xml may be preceded by a BOM 0xff 0xfe or 0xfe 0xff.
    ;; We use [ \t\r\n] instead of `\\s ' to make regex overflow less likely.
    (,(let* ((incomment-re "\\(?:[^-]\\|-[^-]\\)")
	     (comment-re (concat "\\(?:!--" incomment-re "*-->[ \t\r\n]*<\\)")))
	(concat "\\(?:<\\?xml[ \t\r\n]+[^>]*>\\)?[ \t\r\n]*<"
		comment-re "*"
		"\\(?:!DOCTYPE[ \t\r\n]+[^>]*>[ \t\r\n]*<[ \t\r\n]*" comment-re "*\\)?"
		"[Hh][Tt][Mm][Ll]"))
     . mhtml-mode)
    ("<![Dd][Oo][Cc][Tt][Yy][Pp][Ee][ \t\r\n]+[Hh][Tt][Mm][Ll]" . mhtml-mode)
    ;; These two must come after html, because they are more general:
    ("<\\?xml " . xml-mode)
    (,(let* ((incomment-re "\\(?:[^-]\\|-[^-]\\)")
	     (comment-re (concat "\\(?:!--" incomment-re "*-->[ \t\r\n]*<\\)")))
	(concat "[ \t\r\n]*<" comment-re "*!DOCTYPE "))
     . sgml-mode)
    ("\320\317\021\340\241\261\032\341" . doc-view-mode-maybe) ; Word documents 1997-2004
    ("%!PS" . ps-mode)
    ("# xmcd " . conf-unix-mode)))
  "Like `magic-mode-alist' but has lower priority than `auto-mode-alist'.
Each element looks like (REGEXP . FUNCTION) or (MATCH-FUNCTION . FUNCTION).
After visiting a file, if REGEXP matches the text at the beginning of the
buffer (case-sensitively), or calling MATCH-FUNCTION returns non-nil,
`normal-mode' will call FUNCTION, provided that `magic-mode-alist' and
`auto-mode-alist' have not specified a mode for this file.

If FUNCTION is nil, then it is not called.")
(put 'magic-fallback-mode-alist 'risky-local-variable t)

(defvar magic-mode-regexp-match-limit 4000
  "Upper limit on `magic-mode-alist' regexp matches.
Also applies to `magic-fallback-mode-alist'.")

(defun set-auto-mode--apply-alist (alist keep-mode-if-same dir-local)
  "Helper function for `set-auto-mode'.
This function takes an alist of the same form as
`auto-mode-alist'.  It then tries to find the appropriate match
in the alist for the current buffer; setting the mode if
possible.
Return non-nil if the mode was set, nil otherwise.
DIR-LOCAL non-nil means this call is via directory-locals, and
extra checks should be done."
  (if buffer-file-name
      (let (mode
            (name buffer-file-name)
            (remote-id (file-remote-p buffer-file-name))
            (case-insensitive-p (file-name-case-insensitive-p
                                 buffer-file-name)))
        ;; Remove backup-suffixes from file name.
        (setq name (file-name-sans-versions name))
        ;; Remove remote file name identification.
        (when (and (stringp remote-id)
                   (string-match (regexp-quote remote-id) name))
          (setq name (substring name (match-end 0))))
        (while name
          ;; Find first matching alist entry.
          (setq mode
                (if case-insensitive-p
                    ;; Filesystem is case-insensitive.
                    (let ((case-fold-search t))
                      (assoc-default name alist 'string-match))
                  ;; Filesystem is case-sensitive.
                  (or
                   ;; First match case-sensitively.
                   (let ((case-fold-search nil))
                     (assoc-default name alist 'string-match))
                   ;; Fallback to case-insensitive match.
                   (and auto-mode-case-fold
                        (let ((case-fold-search t))
                          (assoc-default name alist 'string-match))))))
          (if (and mode
                   (consp mode)
                   (cadr mode))
              (setq mode (car mode)
                    name (substring name 0 (match-beginning 0)))
            (setq name nil)))
        (when (and dir-local mode
                   (not (set-auto-mode--dir-local-valid-p mode)))
          (message "Ignoring invalid mode `%s'" mode)
          (setq mode nil))
        (when mode
          (set-auto-mode-0 mode keep-mode-if-same)
          t))))

(defun set-auto-mode--dir-local-valid-p (mode)
  "Say whether MODE can be used in a .dir-local.el `auto-mode-alist'."
  (and (symbolp mode)
       (string-suffix-p "-mode" (symbol-name mode))
       (commandp mode)
       (not (provided-mode-derived-p mode 'special-mode))))

(defun set-auto-mode (&optional keep-mode-if-same)
  "Select major mode appropriate for current buffer.

To find the right major mode, this function checks for a -*- mode tag
checks for a `mode:' entry in the Local Variables section of the file,
checks if there an `auto-mode-alist' entry in `.dir-locals.el',
checks if it uses an interpreter listed in `interpreter-mode-alist',
matches the buffer beginning against `magic-mode-alist',
compares the file name against the entries in `auto-mode-alist',
then matches the buffer beginning against `magic-fallback-mode-alist'.

If `enable-local-variables' is nil, or if the file name matches
`inhibit-local-variables-regexps', this function does not check
for any mode: tag anywhere in the file.  If `local-enable-local-variables'
is nil, then the only mode: tag that can be relevant is a -*- one.

If the optional argument KEEP-MODE-IF-SAME is non-nil, then we
set the major mode only if that would change it.  In other words
we don't actually set it to the same mode the buffer already has."
  ;; Look for -*-MODENAME-*- or -*- ... mode: MODENAME; ... -*-
  (let ((try-locals (not (inhibit-local-variables-p)))
	end done mode modes)
    ;; Once we drop the deprecated feature where mode: is also allowed to
    ;; specify minor-modes (ie, there can be more than one "mode:"), we can
    ;; remove this section and just let (hack-local-variables t) handle it.
    ;; Find a -*- mode tag.
    (save-excursion
      (goto-char (point-min))
      (skip-chars-forward " \t\n")
      ;; Note by design local-enable-local-variables does not matter here.
      (and enable-local-variables
	   try-locals
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
    ;; If we found modes to use, invoke them now, outside the save-excursion.
    (if modes
	(catch 'nop
	  (dolist (mode (nreverse modes))
	    (if (not (functionp mode))
		(message "Ignoring unknown mode `%s'" mode)
	      (setq done t)
	      (or (set-auto-mode-0 mode keep-mode-if-same)
		  ;; continuing would call minor modes again, toggling them off
		  (throw 'nop nil))))))
    ;; Check for auto-mode-alist entry in dir-locals.
    (unless done
      (with-demoted-errors "Directory-local variables error: %s"
	;; Note this is a no-op if enable-local-variables is nil.
        (let* ((mode-alist (cdr (hack-dir-local--get-variables
                                 (lambda (key) (eq key 'auto-mode-alist))))))
          (setq done (set-auto-mode--apply-alist mode-alist
                                                 keep-mode-if-same t)))))
    (and (not done)
	 (setq mode (hack-local-variables t (not try-locals)))
	 (not (memq mode modes))	; already tried and failed
	 (if (not (functionp mode))
	     (message "Ignoring unknown mode `%s'" mode)
	   (setq done t)
	   (set-auto-mode-0 mode keep-mode-if-same)))
    ;; If we didn't, look for an interpreter specified in the first line.
    ;; As a special case, allow for things like "#!/bin/env perl", which
    ;; finds the interpreter anywhere in $PATH.
    (and (not done)
	 (setq mode (save-excursion
		      (goto-char (point-min))
		      (if (looking-at auto-mode-interpreter-regexp)
			  (match-string 2))))
	 ;; Map interpreter name to a mode, signaling we're done at the
	 ;; same time.
	 (setq done (assoc-default
		     (file-name-nondirectory mode)
		     (mapcar (lambda (e)
                               (cons
                                (format "\\`%s\\'" (car e))
                                (cdr e)))
			     interpreter-mode-alist)
		     #'string-match-p))
	 ;; If we found an interpreter mode to use, invoke it now.
	 (set-auto-mode-0 done keep-mode-if-same))
    ;; Next try matching the buffer beginning against magic-mode-alist.
    (unless done
      (if (setq done (save-excursion
		       (goto-char (point-min))
		       (save-restriction
			 (narrow-to-region (point-min)
					   (min (point-max)
						(+ (point-min) magic-mode-regexp-match-limit)))
                         (assoc-default
                          nil magic-mode-alist
                          (lambda (re _dummy)
                            (cond
                             ((functionp re)
                              (funcall re))
                             ((stringp re)
                              (let ((case-fold-search nil))
                                (looking-at re)))
                             (t
                              (error
                               "Problem in magic-mode-alist with element %s"
                               re))))))))
	  (set-auto-mode-0 done keep-mode-if-same)))
    ;; Next compare the filename against the entries in auto-mode-alist.
    (unless done
      (setq done (set-auto-mode--apply-alist auto-mode-alist
                                             keep-mode-if-same nil)))
    ;; Next try matching the buffer beginning against magic-fallback-mode-alist.
    (unless done
      (if (setq done (save-excursion
		       (goto-char (point-min))
		       (save-restriction
			 (narrow-to-region (point-min)
					   (min (point-max)
						(+ (point-min) magic-mode-regexp-match-limit)))
			 (assoc-default nil magic-fallback-mode-alist
                                        (lambda (re _dummy)
                                          (cond
                                           ((functionp re)
                                            (funcall re))
                                           ((stringp re)
                                            (let ((case-fold-search nil))
                                              (looking-at re)))
                                           (t
                                            (error
                                             "Problem with magic-fallback-mode-alist element: %s"
                                             re))))))))
	  (set-auto-mode-0 done keep-mode-if-same)))
    (unless done
      (set-buffer-major-mode (current-buffer)))))

;; When `keep-mode-if-same' is set, we are working on behalf of
;; set-visited-file-name.  In that case, if the major mode specified is the
;; same one we already have, don't actually reset it.  We don't want to lose
;; minor modes such as Font Lock.
(defun set-auto-mode-0 (mode &optional keep-mode-if-same)
  "Apply MODE and return it.
If optional arg KEEP-MODE-IF-SAME is non-nil, MODE is chased of
any aliases and compared to current major mode.  If they are the
same, do nothing and return nil."
  (unless (and keep-mode-if-same
	       (eq (indirect-function mode)
		   (indirect-function major-mode)))
    (when mode
      (funcall mode)
      mode)))

(defvar file-auto-mode-skip "^\\(#!\\|'\\\\\"\\)"
  "Regexp of lines to skip when looking for file-local settings.
If the first line matches this regular expression, then the -*-...-*- file-
local settings will be consulted on the second line instead of the first.")

(defun set-auto-mode-1 ()
  "Find the -*- spec in the buffer.
Call with point at the place to start searching from.
If one is found, set point to the beginning and return the position
of the end.  Otherwise, return nil; may change point.
The variable `inhibit-local-variables-regexps' can cause a -*- spec to
be ignored; but `enable-local-variables' and `local-enable-local-variables'
have no effect."
  (let (beg end)
    (and
     ;; Don't look for -*- if this file name matches any
     ;; of the regexps in inhibit-local-variables-regexps.
     (not (inhibit-local-variables-p))
     (search-forward "-*-" (line-end-position
                            ;; If the file begins with "#!"  (exec
                            ;; interpreter magic), look for mode frobs
                            ;; in the first two lines.  You cannot
                            ;; necessarily put them in the first line
                            ;; of such a file without screwing up the
                            ;; interpreter invocation.  The same holds
                            ;; for '\" in man pages (preprocessor
                            ;; magic for the `man' program).
                            (and (looking-at file-auto-mode-skip) 2)) t)
     (progn
       (skip-chars-forward " \t")
       (setq beg (point))
       (search-forward "-*-" (line-end-position) t))
     (progn
       (forward-char -3)
       (skip-chars-backward " \t")
       (setq end (point))
       (goto-char beg)
       end))))

;;; Handling file local variables

(defvar ignored-local-variables
  '(ignored-local-variables safe-local-variable-values
    file-local-variables-alist dir-local-variables-alist)
  "Variables to be ignored in a file's local variable spec.")
(put 'ignored-local-variables 'risky-local-variable t)

(defvar hack-local-variables-hook nil
  "Normal hook run after processing a file's local variables specs.
Major modes can use this to examine user-specified local variables
in order to initialize other data structure based on them.")

(defcustom safe-local-variable-values nil
  "List of variable-value pairs that are considered safe.
Each element is a cons cell (VAR . VAL), where VAR is a variable
symbol and VAL is a value that is considered safe.

Also see `ignored-local-variable-values'."
  :risky t
  :group 'find-file
  :type 'alist)

(defcustom ignored-local-variable-values nil
  "List of variable-value pairs that should always be ignored.
Each element is a cons cell (VAR . VAL), where VAR is a variable
symbol and VAL is its value; if VAR is set to VAL by a file-local
variables section, that setting should be ignored.

Also see `safe-local-variable-values'."
  :risky t
  :group 'find-file
  :type 'alist
  :version "28.1")

(defcustom safe-local-eval-forms
  ;; This should be here at least as long as Emacs supports write-file-hooks.
  '((add-hook 'write-file-hooks 'time-stamp)
    (add-hook 'write-file-functions 'time-stamp)
    (add-hook 'before-save-hook 'time-stamp nil t)
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))
  "Expressions that are considered safe in an `eval:' local variable.
Add expressions to this list if you want Emacs to evaluate them, when
they appear in an `eval' local variable specification, without first
asking you for confirmation."
  :risky t
  :group 'find-file
  :version "24.1"			; added write-file-hooks
  :type '(repeat sexp))

;; Risky local variables:
(mapc (lambda (var) (put var 'risky-local-variable t))
      '(after-load-alist
	buffer-auto-save-file-name
	buffer-file-name
	buffer-file-truename
	buffer-undo-list
	debugger
	default-text-properties
	eval
	exec-directory
	exec-path
	file-name-handler-alist
	frame-title-format
	global-mode-string
	header-line-format
	icon-title-format
	inhibit-quit
	load-path
	max-lisp-eval-depth
	max-specpdl-size
	minor-mode-map-alist
	minor-mode-overriding-map-alist
	mode-line-format
	mode-name
	overriding-local-map
	overriding-terminal-local-map
	process-environment
	standard-input
	standard-output
	unread-command-events))

;; Safe local variables:
;;
;; For variables defined by major modes, the safety declarations can go into
;; the major mode's file, since that will be loaded before file variables are
;; processed.
;;
;; For variables defined by minor modes, put the safety declarations in the
;; file defining the minor mode after the defcustom/defvar using an autoload
;; cookie, e.g.:
;;
;;   ;;;###autoload(put 'variable 'safe-local-variable 'stringp)
;;
;; Otherwise, when Emacs visits a file specifying that local variable, the
;; minor mode file may not be loaded yet.
;;
;; For variables defined in the C source code the declaration should go here:

(dolist (pair
	 '((buffer-read-only        . booleanp)	;; C source code
	   (default-directory       . stringp)	;; C source code
	   (fill-column             . integerp)	;; C source code
	   (indent-tabs-mode        . booleanp)	;; C source code
	   (left-margin             . integerp)	;; C source code
	   (inhibit-compacting-font-caches . booleanp) ;; C source code
	   (no-update-autoloads     . booleanp)
	   (lexical-binding	 . booleanp)	  ;; C source code
	   (tab-width               . integerp)	  ;; C source code
	   (truncate-lines          . booleanp)	  ;; C source code
	   (word-wrap               . booleanp)	  ;; C source code
	   (bidi-display-reordering . booleanp))) ;; C source code
  (put (car pair) 'safe-local-variable (cdr pair)))

(put 'bidi-paragraph-direction 'safe-local-variable
     (lambda (v) (memq v '(nil right-to-left left-to-right))))

(put 'c-set-style 'safe-local-eval-function t)

(defvar-local file-local-variables-alist nil
  "Alist of file-local variable settings in the current buffer.
Each element in this list has the form (VAR . VALUE), where VAR
is a file-local variable (a symbol) and VALUE is the value
specified.  The actual value in the buffer may differ from VALUE,
if it is changed by the major or minor modes, or by the user.")
(put 'file-local-variables-alist 'permanent-local t)

(defvar-local dir-local-variables-alist nil
  "Alist of directory-local variable settings in the current buffer.
Each element in this list has the form (VAR . VALUE), where VAR
is a directory-local variable (a symbol) and VALUE is the value
specified in .dir-locals.el.  The actual value in the buffer
may differ from VALUE, if it is changed by the major or minor modes,
or by the user.")

(defvar before-hack-local-variables-hook nil
  "Normal hook run before setting file-local variables.
It is called after checking for unsafe/risky variables and
setting `file-local-variables-alist', and before applying the
variables stored in `file-local-variables-alist'.  A hook
function is allowed to change the contents of this alist.

This hook is called only if there is at least one file-local
variable to set.")

(defvar permanently-enabled-local-variables '(lexical-binding)
  "A list of file-local variables that are always enabled.
This overrides any `enable-local-variables' setting.")

(defun hack-local-variables-confirm (all-vars unsafe-vars risky-vars dir-name)
  "Get confirmation before setting up local variable values.
ALL-VARS is the list of all variables to be set up.
UNSAFE-VARS is the list of those that aren't marked as safe or risky.
RISKY-VARS is the list of those that are marked as risky.
If these settings come from directory-local variables, then
DIR-NAME is the name of the associated directory.  Otherwise it is nil."
  (unless noninteractive
    (let ((name (cond (dir-name)
		      (buffer-file-name
		       (file-name-nondirectory buffer-file-name))
		      ((concat "buffer " (buffer-name)))))
	  (offer-save (and (eq enable-local-variables t)
			   unsafe-vars))
	  (buf (get-buffer-create "*Local Variables*")))
      ;; Set up the contents of the *Local Variables* buffer.
      (with-current-buffer buf
	(erase-buffer)
	(cond
	 (unsafe-vars
	  (insert "The local variables list in " name
		  "\ncontains values that may not be safe (*)"
		  (if risky-vars
		      ", and variables that are risky (**)."
		    ".")))
	 (risky-vars
	  (insert "The local variables list in " name
		  "\ncontains variables that are risky (**)."))
	 (t
	  (insert "A local variables list is specified in " name ".")))
	(insert "\n\nDo you want to apply it?  You can type
y  -- to apply the local variables list.
n  -- to ignore the local variables list.")
	(if offer-save
	    (insert "
!  -- to apply the local variables list, and permanently mark these
      values (*) as safe (in the future, they will be set automatically.)
i  -- to ignore the local variables list, and permanently mark these
      values (*) as ignored\n\n")
	  (insert "\n\n"))
	(dolist (elt all-vars)
	  (cond ((member elt unsafe-vars)
		 (insert "  * "))
		((member elt risky-vars)
		 (insert " ** "))
		(t
		 (insert "    ")))
	  (princ (car elt) buf)
	  (insert " : ")
	  ;; Make strings with embedded whitespace easier to read.
	  (let ((print-escape-newlines t))
	    (prin1 (cdr elt) buf))
	  (insert "\n"))
        (setq-local cursor-type nil)
	(set-buffer-modified-p nil)
	(goto-char (point-min)))

      ;; Display the buffer and read a choice.
      (save-window-excursion
	(pop-to-buffer buf '(display-buffer--maybe-at-bottom))
	(let* ((exit-chars '(?y ?n ?\s))
	       (prompt (format "Please type %s%s: "
			       (if offer-save "y, n, ! or i" "y or n")
			       (if (< (line-number-at-pos (point-max))
				      (window-body-height))
				   ""
				 ", or C-v/M-v to scroll")))
	       char)
	  (when offer-save
            (push ?i exit-chars)
            (push ?! exit-chars))
	  (setq char (read-char-choice prompt exit-chars))
	  (when (and offer-save
                     (or (= char ?!) (= char ?i))
                     unsafe-vars)
	    (customize-push-and-save
             (if (= char ?!)
                 'safe-local-variable-values
               'ignored-local-variable-values)
             unsafe-vars))
	  (prog1 (memq char '(?! ?\s ?y))
	    (quit-window t)))))))

(defconst hack-local-variable-regexp
  "[ \t]*\\([^][;\"'?()\\ \t\n]+\\)[ \t]*:[ \t]*")

(defun hack-local-variables-prop-line (&optional handle-mode)
  "Return local variables specified in the -*- line.
Usually returns an alist of elements (VAR . VAL), where VAR is a
variable and VAL is the specified value.  Ignores any
specification for `coding:', and sometimes for `mode' (which
should have already been handled by `set-auto-coding' and
`set-auto-mode', respectively).  Return nil if the -*- line is
malformed.

If HANDLE-MODE is nil, we return the alist of all the local
variables in the line except `coding' as described above.  If it
is neither nil nor t, we do the same, except that any settings of
`mode' and `coding' are ignored.  If HANDLE-MODE is t, we ignore
all settings in the line except for `mode', which \(if present) we
return as the symbol specifying the mode."
  (catch 'malformed-line
    (save-excursion
      (goto-char (point-min))
      (let ((end (set-auto-mode-1))
	    result)
	(cond ((not end)
	       nil)
	      ((looking-at "[ \t]*\\([^ \t\n\r:;]+\\)\\([ \t]*-\\*-\\)")
	       ;; Simple form: "-*- MODENAME -*-".
	       (if (eq handle-mode t)
		   (intern (concat (match-string 1) "-mode"))))
	      (t
	       ;; Hairy form: '-*-' [ <variable> ':' <value> ';' ]* '-*-'
	       ;; (last ";" is optional).
	       ;; If HANDLE-MODE is t, just check for `mode'.
	       ;; Otherwise, parse the -*- line into the RESULT alist.
	       (while (not (or (and (eq handle-mode t) result)
                               (>= (point) end)))
		 (unless (looking-at hack-local-variable-regexp)
		   (message "Malformed mode-line: %S"
                            (buffer-substring-no-properties (point) end))
		   (throw 'malformed-line nil))
		 (goto-char (match-end 0))
		 ;; There used to be a downcase here,
		 ;; but the manual didn't say so,
		 ;; and people want to set var names that aren't all lc.
		 (let* ((key (intern (match-string 1)))
			(val (save-restriction
			       (narrow-to-region (point) end)
                               ;; As a defensive measure, we do not allow
                               ;; circular data in the file-local data.
			       (let ((read-circle nil))
				 (read (current-buffer)))))
			;; It is traditional to ignore
			;; case when checking for `mode' in set-auto-mode,
			;; so we must do that here as well.
			;; That is inconsistent, but we're stuck with it.
			;; The same can be said for `coding' in set-auto-coding.
			(keyname (downcase (symbol-name key))))
                   (cond
                    ((eq handle-mode t)
                     (and (equal keyname "mode")
                          (setq result
                                (intern (concat (downcase (symbol-name val))
                                                "-mode")))))
                    ((equal keyname "coding"))
                    (t
                     (when (or (not handle-mode)
                               (not (equal keyname "mode")))
                       (condition-case nil
                           (push (cons (cond ((eq key 'eval) 'eval)
                                             ;; Downcase "Mode:".
                                             ((equal keyname "mode") 'mode)
                                             (t (indirect-variable key)))
                                       val)
                                 result)
                         (error nil)))))
		   (skip-chars-forward " \t;")))
	       result))))))

(defun hack-local-variables-filter (variables dir-name)
  "Filter local variable settings, querying the user if necessary.
VARIABLES is the alist of variable-value settings.  This alist is
 filtered based on the values of `ignored-local-variables',
 `enable-local-eval', `enable-local-variables', and (if necessary)
 user interaction.  The results are added to
 `file-local-variables-alist', without applying them.
If these settings come from directory-local variables, then
DIR-NAME is the name of the associated directory.  Otherwise it is nil."
  ;; Find those variables that we may want to save to
  ;; `safe-local-variable-values'.
  (let (all-vars risky-vars unsafe-vars ignored)
    (dolist (elt variables)
      (let ((var (car elt))
	    (val (cdr elt)))
	(cond ((memq var ignored-local-variables)
	       ;; Ignore any variable in `ignored-local-variables'.
	       nil)
              ((seq-some (lambda (elem)
                           (and (eq (car elem) var)
                                (eq (cdr elem) val)))
                         ignored-local-variable-values)
               nil)
	      ;; Obey `enable-local-eval'.
	      ((eq var 'eval)
	       (when enable-local-eval
		 (let ((safe (or (hack-one-local-variable-eval-safep val)
				 ;; In case previously marked safe (bug#5636).
				 (safe-local-variable-p var val))))
		   ;; If not safe and e-l-v = :safe, ignore totally.
		   (when (or safe (not (eq enable-local-variables :safe)))
		     (push elt all-vars)
		     (or (eq enable-local-eval t)
			 safe
			 (push elt unsafe-vars))))))
	      ;; Ignore duplicates (except `mode') in the present list.
	      ((and (assq var all-vars) (not (eq var 'mode))) nil)
	      ;; Accept known-safe variables.
	      ((or (memq var '(mode unibyte coding))
		   (safe-local-variable-p var val))
	       (push elt all-vars))
	      ;; The variable is either risky or unsafe:
	      ((not (eq enable-local-variables :safe))
	       (push elt all-vars)
	       (if (risky-local-variable-p var val)
		   (push elt risky-vars)
		 (push elt unsafe-vars))))))
    (and all-vars
	 ;; Query, unless all vars are safe or user wants no querying.
	 (or (and (eq enable-local-variables t)
		  (null unsafe-vars)
		  (null risky-vars))
	     (memq enable-local-variables '(:all :safe))
	     (hack-local-variables-confirm all-vars unsafe-vars
					   risky-vars dir-name))
	 (dolist (elt all-vars)
	   (unless (memq (car elt) '(eval mode))
	     (unless dir-name
	       (setq dir-local-variables-alist
		     (assq-delete-all (car elt) dir-local-variables-alist)))
	     (setq file-local-variables-alist
		   (assq-delete-all (car elt) file-local-variables-alist)))
	   (push elt file-local-variables-alist)))))

;; TODO?  Warn once per file rather than once per session?
(defvar hack-local-variables--warned-lexical nil)

(defun hack-local-variables (&optional handle-mode inhibit-locals)
  "Parse and put into effect this buffer's local variables spec.
For buffers visiting files, also puts into effect directory-local
variables.

Uses `hack-local-variables-apply' to apply the variables.

See `hack-local-variables--find-variables' for the meaning of
HANDLE-MODE.

If `enable-local-variables' or `local-enable-local-variables' is
nil, or INHIBIT-LOCALS is non-nil, this function disregards all
normal local variables.  If `inhibit-local-variables-regexps'
applies to the file in question, the file is not scanned for
local variables, but directory-local variables may still be
applied.

Variables present in `permanently-enabled-local-variables' will
still be evaluated, even if local variables are otherwise
inhibited."
  ;; We don't let inhibit-local-variables-p influence the value of
  ;; enable-local-variables, because then it would affect dir-local
  ;; variables.  We don't want to search eg tar files for file local
  ;; variable sections, but there is no reason dir-locals cannot apply
  ;; to them.  The real meaning of inhibit-local-variables-p is "do
  ;; not scan this file for local variables".
  (let ((enable-local-variables
	 (and (not inhibit-locals)
              local-enable-local-variables enable-local-variables)))
    (if (eq handle-mode t)
        ;; We're looking just for the major mode setting.
        (and enable-local-variables
             (not (inhibit-local-variables-p))
	     ;; If HANDLE-MODE is t, and the prop line specifies a
	     ;; mode, then we're done, and have no need to scan further.
             (or (hack-local-variables-prop-line t)
                 ;; Look for the mode elsewhere in the buffer.
                 (hack-local-variables--find-variables t)))
      ;; Normal handling of local variables.
      (setq file-local-variables-alist nil)
      (when (and (file-remote-p default-directory)
                 (fboundp 'hack-connection-local-variables)
                 (fboundp 'connection-local-criteria-for-default-directory))
        (with-demoted-errors "Connection-local variables error: %s"
	  ;; Note this is a no-op if enable-local-variables is nil.
	  (hack-connection-local-variables
           (connection-local-criteria-for-default-directory))))
      (with-demoted-errors "Directory-local variables error: %s"
	;; Note this is a no-op if enable-local-variables is nil.
	(hack-dir-local-variables))
      (let ((result (append (hack-local-variables--find-variables)
                            (hack-local-variables-prop-line))))
        (if (and enable-local-variables
                 (not (inhibit-local-variables-p)))
            (progn
	      ;; Set the variables.
	      (hack-local-variables-filter result nil)
	      (hack-local-variables-apply))
          ;; Handle `lexical-binding' and other special local
          ;; variables.
          (dolist (variable permanently-enabled-local-variables)
            (when-let ((elem (assq variable result)))
              (push elem file-local-variables-alist)))
          (hack-local-variables-apply))))))

(defun hack-local-variables--find-variables (&optional handle-mode)
  "Return all local variables in the current buffer.
If HANDLE-MODE is nil, we gather all the specified local
variables.  If HANDLE-MODE is neither nil nor t, we do the same,
except that any settings of `mode' are ignored.

If HANDLE-MODE is t, all we do is check whether a \"mode:\"
is specified, and return the corresponding mode symbol, or nil.
In this case, we try to ignore minor-modes, and return only a
major-mode."
  (let ((result nil))
    ;; Look for "Local variables:" line in last page.
    (save-excursion
      (goto-char (point-max))
      (search-backward "\n\^L" (max (- (point-max) 3000) (point-min))
		       'move)
      (when (let ((case-fold-search t))
	      (search-forward "Local Variables:" nil t))
        (skip-chars-forward " \t")
        ;; suffix is what comes after "local variables:" in its line.
        ;; prefix is what comes before "local variables:" in its line.
        (let ((suffix
	       (concat
	        (regexp-quote (buffer-substring (point)
					        (line-end-position)))
	        "$"))
	      (prefix
	       (concat "^" (regexp-quote
			    (buffer-substring (line-beginning-position)
					      (match-beginning 0))))))

	  (forward-line 1)
	  (let ((startpos (point))
	        endpos
	        (thisbuf (current-buffer)))
	    (save-excursion
	      (unless (let ((case-fold-search t))
		        (re-search-forward
		         (concat prefix "[ \t]*End:[ \t]*" suffix)
		         nil t))
	        ;; This used to be an error, but really all it means is
	        ;; that this may simply not be a local-variables section,
	        ;; so just ignore it.
	        (message "Local variables list is not properly terminated"))
	      (beginning-of-line)
	      (setq endpos (point)))

	    (with-temp-buffer
	      (insert-buffer-substring thisbuf startpos endpos)
	      (goto-char (point-min))
	      (subst-char-in-region (point) (point-max) ?\^m ?\n)
	      (while (not (eobp))
	        ;; Discard the prefix.
	        (if (looking-at prefix)
		    (delete-region (point) (match-end 0))
		  (error "Local variables entry is missing the prefix"))
	        (end-of-line)
	        ;; Discard the suffix.
	        (if (looking-back suffix (line-beginning-position))
		    (delete-region (match-beginning 0) (point))
		  (error "Local variables entry is missing the suffix"))
	        (forward-line 1))
	      (goto-char (point-min))

	      (while (not (or (eobp)
                              (and (eq handle-mode t) result)))
	        ;; Find the variable name;
	        (unless (looking-at hack-local-variable-regexp)
                  (error "Malformed local variable line: %S"
                         (buffer-substring-no-properties
                          (point) (line-end-position))))
                (goto-char (match-end 1))
	        (let* ((str (match-string 1))
		       (var (intern str))
		       val val2)
		  (and (equal (downcase (symbol-name var)) "mode")
		       (setq var 'mode))
		  ;; Read the variable value.
		  (skip-chars-forward "^:")
		  (forward-char 1)
                  ;; As a defensive measure, we do not allow
                  ;; circular data in the file-local data.
		  (let ((read-circle nil))
		    (setq val (read (current-buffer))))
		  (if (eq handle-mode t)
		      (and (eq var 'mode)
			   ;; Specifying minor-modes via mode: is
			   ;; deprecated, but try to reject them anyway.
			   (not (string-match
			         "-minor\\'"
			         (setq val2 (downcase (symbol-name val)))))
			   (setq result (intern (concat val2 "-mode"))))
		    (cond ((eq var 'coding))
			  ((eq var 'lexical-binding)
			   (unless hack-local-variables--warned-lexical
			     (setq hack-local-variables--warned-lexical t)
			     (display-warning
                              'files
                              (format-message
                               "%s: `lexical-binding' at end of file unreliable"
                               (file-name-nondirectory
                                ;; We are called from
                                ;; 'with-temp-buffer', so we need
                                ;; to use 'thisbuf's name in the
                                ;; warning message.
                                (or (buffer-file-name thisbuf) ""))))))
                          ((and (eq var 'mode) handle-mode))
			  (t
			   (ignore-errors
			     (push (cons (if (eq var 'eval)
					     'eval
					   (indirect-variable var))
				         val)
                                   result))))))
	        (forward-line 1)))))))
    result))

(defun hack-local-variables-apply ()
  "Apply the elements of `file-local-variables-alist'.
If there are any elements, runs `before-hack-local-variables-hook',
then calls `hack-one-local-variable' to apply the alist elements one by one.
Finishes by running `hack-local-variables-hook', regardless of whether
the alist is empty or not.

Note that this function ignores a `mode' entry if it specifies the same
major mode as the buffer already has."
  (when file-local-variables-alist
    ;; Any 'evals must run in the Right sequence.
    (setq file-local-variables-alist
	  (nreverse file-local-variables-alist))
    (run-hooks 'before-hack-local-variables-hook)
    (dolist (elt file-local-variables-alist)
      (hack-one-local-variable (car elt) (cdr elt))))
  (run-hooks 'hack-local-variables-hook))

(defun safe-local-variable-p (sym val)
  "Non-nil if SYM is safe as a file-local variable with value VAL.
It is safe if any of these conditions are met:

 * There is a matching entry (SYM . VAL) in the
   `safe-local-variable-values' user option.

 * The `safe-local-variable' property of SYM is a function that
   evaluates to a non-nil value with VAL as an argument."
  (or (member (cons sym val) safe-local-variable-values)
      (let ((safep (get sym 'safe-local-variable)))
        (and (functionp safep)
             ;; If the function signals an error, that means it
             ;; can't assure us that the value is safe.
             (with-demoted-errors (funcall safep val))))))

(defun risky-local-variable-p (sym &optional _ignored)
  "Non-nil if SYM could be dangerous as a file-local variable.
It is dangerous if either of these conditions are met:

 * Its `risky-local-variable' property is non-nil.

 * Its name ends with \"hook(s)\", \"function(s)\", \"form(s)\", \"map\",
   \"program\", \"command(s)\", \"predicate(s)\", \"frame-alist\",
   \"mode-alist\", \"font-lock-(syntactic-)keyword*\",
   \"map-alist\", or \"bindat-spec\"."
  ;; If this is an alias, check the base name.
  (condition-case nil
      (setq sym (indirect-variable sym))
    (error nil))
  (or (get sym 'risky-local-variable)
      (string-match "-hooks?$\\|-functions?$\\|-forms?$\\|-program$\\|\
-commands?$\\|-predicates?$\\|font-lock-keywords$\\|font-lock-keywords\
-[0-9]+$\\|font-lock-syntactic-keywords$\\|-frame-alist$\\|-mode-alist$\\|\
-map$\\|-map-alist$\\|-bindat-spec$" (symbol-name sym))))

(defun hack-one-local-variable-quotep (exp)
  (and (consp exp) (eq (car exp) 'quote) (consp (cdr exp))))

(defun hack-one-local-variable-constantp (exp)
  (or (and (not (symbolp exp)) (not (consp exp)))
      (memq exp '(t nil))
      (keywordp exp)
      (hack-one-local-variable-quotep exp)))

(defun hack-one-local-variable-eval-safep (exp)
  "Return non-nil if it is safe to eval EXP when it is found in a file."
  (or (not (consp exp))
      ;; Detect certain `put' expressions.
      (and (eq (car exp) 'put)
	   (hack-one-local-variable-quotep (nth 1 exp))
	   (hack-one-local-variable-quotep (nth 2 exp))
	   (let ((prop (nth 1 (nth 2 exp)))
		 (val (nth 3 exp)))
	     (cond ((memq prop '(lisp-indent-hook
				 lisp-indent-function
				 scheme-indent-function))
		    ;; Allow only safe values (not functions).
		    (or (numberp val)
			(and (hack-one-local-variable-quotep val)
			     (eq (nth 1 val) 'defun))))
		   ((eq prop 'edebug-form-spec)
		    ;; Allow only indirect form specs.
		    ;; During bootstrapping, edebug-basic-spec might not be
		    ;; defined yet.
                    (and (fboundp 'edebug-basic-spec)
			 (hack-one-local-variable-quotep val)
                         (edebug-basic-spec (nth 1 val)))))))
      ;; Allow expressions that the user requested.
      (member exp safe-local-eval-forms)
      ;; Certain functions can be allowed with safe arguments
      ;; or can specify verification functions to try.
      (and (symbolp (car exp))
	   ;; Allow (minor)-modes calls with no arguments.
	   ;; This obsoletes the use of "mode:" for such things.  (Bug#8613)
	   (or (and (member (cdr exp) '(nil (1) (0) (-1)))
		    (string-match "-mode\\'" (symbol-name (car exp))))
	       (let ((prop (get (car exp) 'safe-local-eval-function)))
		 (cond ((eq prop t)
			(let ((ok t))
			  (dolist (arg (cdr exp))
			    (unless (hack-one-local-variable-constantp arg)
			      (setq ok nil)))
			  ok))
		       ((functionp prop)
			(funcall prop exp))
		       ((listp prop)
			(let ((ok nil))
			  (dolist (function prop)
			    (if (funcall function exp)
				(setq ok t)))
			  ok))))))))

(defun hack-one-local-variable--obsolete (var)
  (let ((o (get var 'byte-obsolete-variable)))
    (when o
      (let ((instead (nth 0 o))
            (since (nth 2 o)))
        (message "%s is obsolete%s; %s"
                 var (if since (format " (since %s)" since))
                 (if (stringp instead)
                     (substitute-command-keys instead)
                   (format-message "use `%s' instead" instead)))))))

(defun hack-one-local-variable (var val)
  "Set local variable VAR with value VAL.
If VAR is `mode', call `VAL-mode' as a function unless it's
already the major mode."
  (pcase var
    ('mode
     (let ((mode (intern (concat (downcase (symbol-name val))
                                 "-mode"))))
       (unless (eq (indirect-function mode)
                   (indirect-function major-mode))
         (funcall mode))))
    ('eval
     (pcase val
       (`(add-hook ',hook . ,_) (hack-one-local-variable--obsolete hook)))
     (save-excursion (eval val t)))
    (_
     (hack-one-local-variable--obsolete var)
     ;; Make sure the string has no text properties.
     ;; Some text properties can get evaluated in various ways,
     ;; so it is risky to put them on with a local variable list.
     (if (stringp val)
         (set-text-properties 0 (length val) nil val))
     (set (make-local-variable var) val))))

;;; Handling directory-local variables, aka project settings.

(defvar dir-locals-class-alist '()
  "Alist mapping directory-local variable classes (symbols) to variable lists.")

(defvar dir-locals-directory-cache '()
  "List of cached directory roots for directory-local variable classes.
Each element in this list has the form (DIR CLASS MTIME).
DIR is the name of the directory.
CLASS is the name of a variable class (a symbol).
MTIME is the recorded modification time of the directory-local
variables file associated with this entry.  This time is a Lisp
timestamp (the same format as `current-time'), and is
used to test whether the cache entry is still valid.
Alternatively, MTIME can be nil, which means the entry is always
considered valid.")

(defsubst dir-locals-get-class-variables (class)
  "Return the variable list for CLASS."
  (cdr (assq class dir-locals-class-alist)))

(defun dir-locals-collect-mode-variables (mode-variables variables)
  "Collect directory-local variables from MODE-VARIABLES.
VARIABLES is the initial list of variables.
Returns the new list."
  (dolist (pair mode-variables variables)
    (let* ((variable (car pair))
	   (value (cdr pair))
	   (slot (assq variable variables)))
      ;; If variables are specified more than once, use only the last.  (Why?)
      ;; The pseudo-variables mode and eval are different (bug#3430).
      (if (and slot (not (memq variable '(mode eval))))
	  (setcdr slot value)
	;; Need a new cons in case we setcdr later.
	(push (cons variable value) variables)))))

(defun dir-locals-collect-variables (class-variables root variables
                                                     &optional predicate)
  "Collect entries from CLASS-VARIABLES into VARIABLES.
ROOT is the root directory of the project.
Return the new variables list.
If PREDICATE is given, it is used to test a symbol key in the alist
to see whether it should be considered."
  (let* ((file-name (or (buffer-file-name)
			;; Handle non-file buffers, too.
			(expand-file-name default-directory)))
	 (sub-file-name (if (and file-name
                                 (file-name-absolute-p file-name))
                            ;; FIXME: Why not use file-relative-name?
			    (substring file-name (length root)))))
    (condition-case err
        (dolist (entry class-variables variables)
          (let ((key (car entry)))
            (cond
             ((stringp key)
              ;; Don't include this in the previous condition, because we
              ;; want to filter all strings before the next condition.
              (when (and sub-file-name
                         (>= (length sub-file-name) (length key))
                         (string-prefix-p key sub-file-name))
                (setq variables (dir-locals-collect-variables
                                 (cdr entry) root variables predicate))))
             ((if predicate
                  (funcall predicate key)
                (or (not key)
                    (derived-mode-p key)))
              (let* ((alist (cdr entry))
                     (subdirs (assq 'subdirs alist)))
                (if (or (not subdirs)
                        (progn
                          (setq alist (remq subdirs alist))
                          (cdr-safe subdirs))
                        ;; TODO someone might want to extend this to allow
                        ;; integer values for subdir, where N means
                        ;; variables apply to this directory and N levels
                        ;; below it (0 == nil).
                        (equal root (expand-file-name default-directory)))
                    (setq variables (dir-locals-collect-mode-variables
                                     alist variables))))))))
      (error
       ;; The file's content might be invalid (e.g. have a merge conflict), but
       ;; that shouldn't prevent the user from opening the file.
       (message "%s error: %s" dir-locals-file (error-message-string err))
       nil))))

(defun dir-locals-set-directory-class (directory class &optional mtime)
  "Declare that the DIRECTORY root is an instance of CLASS.
DIRECTORY is the name of a directory, a string.
CLASS is the name of a project class, a symbol.
MTIME is either the modification time of the directory-local
variables file that defined this class, or nil.

When a file beneath DIRECTORY is visited, the mode-specific
variables from CLASS are applied to the buffer.  The variables
for a class are defined using `dir-locals-set-class-variables'."
  (setq directory (file-name-as-directory (expand-file-name directory)))
  (unless (assq class dir-locals-class-alist)
    (error "No such class `%s'" (symbol-name class)))
  (push (list directory class mtime) dir-locals-directory-cache))

(defun dir-locals-set-class-variables (class variables)
  "Map the type CLASS to a list of variable settings.
CLASS is the project class, a symbol.  VARIABLES is a list
that declares directory-local variables for the class.
An element in VARIABLES is either of the form:
    (MAJOR-MODE . ALIST)
or
    (DIRECTORY . LIST)

In the first form, MAJOR-MODE is a symbol, and ALIST is an alist
whose elements are of the form (VARIABLE . VALUE).

In the second form, DIRECTORY is a directory name (a string), and
LIST is a list of the form accepted by the function.

When a file is visited, the file's class is found.  A directory
may be assigned a class using `dir-locals-set-directory-class'.
Then variables are set in the file's buffer according to the
VARIABLES list of the class.  The list is processed in order.

* If the element is of the form (MAJOR-MODE . ALIST), and the
  buffer's major mode is derived from MAJOR-MODE (as determined
  by `derived-mode-p'), then all the variables in ALIST are
  applied.  A MAJOR-MODE of nil may be used to match any buffer.
  `make-local-variable' is called for each variable before it is
  set.

* If the element is of the form (DIRECTORY . LIST), and DIRECTORY
  is an initial substring of the file's directory, then LIST is
  applied by recursively following these rules."
  (setf (alist-get class dir-locals-class-alist) variables))

(defconst dir-locals-file ".dir-locals.el"
  "File that contains directory-local variables.
It has to be constant to enforce uniform values across different
environments and users.

A second dir-locals file can be used by a user to specify their
personal dir-local variables even if the current directory
already has a `dir-locals-file' that is shared with other
users (such as in a git repository).  The name of this second
file is derived by appending \"-2\" to the base name of
`dir-locals-file'.  With the default value of `dir-locals-file',
a \".dir-locals-2.el\" file in the same directory will override
the \".dir-locals.el\".

See Info node `(elisp)Directory Local Variables' for details.")

(defun dir-locals--all-files (directory)
  "Return a list of all readable dir-locals files in DIRECTORY.
The returned list is sorted by increasing priority.  That is,
values specified in the last file should take precedence over
those in the first."
  (when (file-readable-p directory)
    (let* ((file-1 (expand-file-name (if (eq system-type 'ms-dos)
                                        (dosified-file-name dir-locals-file)
                                      dir-locals-file)
                                    directory))
           (file-2 (when (string-match "\\.el\\'" file-1)
                     (replace-match "-2.el" t nil file-1)))
          (out nil))
      ;; The order here is important.
      (dolist (f (list file-2 file-1))
        (when (and f
                   (file-readable-p f)
                   ;; FIXME: Aren't file-regular-p and
                   ;; file-directory-p mutually exclusive?
                   (file-regular-p f)
                   (not (file-directory-p f)))
          (push f out)))
      out)))

(defun dir-locals-find-file (file)
  "Find the directory-local variables for FILE.
This searches upward in the directory tree from FILE.
It stops at the first directory that has been registered in
`dir-locals-directory-cache' or contains a `dir-locals-file'.
If it finds an entry in the cache, it checks that it is valid.
A cache entry with no modification time element (normally, one that
has been assigned directly using `dir-locals-set-directory-class', not
set from a file) is always valid.
A cache entry based on a `dir-locals-file' is valid if the modification
time stored in the cache matches the current file modification time.
If not, the cache entry is cleared so that the file will be re-read.

This function returns either:
  - nil (no directory local variables found),
  - the matching entry from `dir-locals-directory-cache' (a list),
  - or the full path to the directory (a string) containing at
    least one `dir-locals-file' in the case of no valid cache
    entry."
  (setq file (expand-file-name file))
  (let* ((locals-dir (locate-dominating-file (file-name-directory file)
                                             #'dir-locals--all-files))
         dir-elt)
    ;; `locate-dominating-file' may have abbreviated the name.
    (when locals-dir
      (setq locals-dir (expand-file-name locals-dir)))
    ;; Find the best cached value in `dir-locals-directory-cache'.
    (dolist (elt dir-locals-directory-cache)
      (when (and (string-prefix-p (car elt) file
                                  (memq system-type
                                        '(windows-nt cygwin ms-dos)))
                 (> (length (car elt)) (length (car dir-elt))))
        (setq dir-elt elt)))
    (if (and dir-elt
             (or (null locals-dir)
                 (<= (length locals-dir)
                     (length (car dir-elt)))))
        ;; Found a potential cache entry.  Check validity.
        ;; A cache entry with no MTIME is assumed to always be valid
        ;; (ie, set directly, not from a dir-locals file).
        ;; Note, we don't bother to check that there is a matching class
        ;; element in dir-locals-class-alist, since that's done by
        ;; dir-locals-set-directory-class.
        (if (or (null (nth 2 dir-elt))
                (let ((cached-files (dir-locals--all-files (car dir-elt))))
                  ;; The entry MTIME should match the most recent
                  ;; MTIME among matching files.
                  (and cached-files
		       (equal (nth 2 dir-elt)
			      (let ((latest 0))
				(dolist (f cached-files latest)
				  (let ((f-time
					 (file-attribute-modification-time
					  (file-attributes f))))
				    (if (time-less-p latest f-time)
					(setq latest f-time)))))))))
            ;; This cache entry is OK.
            dir-elt
          ;; This cache entry is invalid; clear it.
          (setq dir-locals-directory-cache
                (delq dir-elt dir-locals-directory-cache))
          ;; Return the first existing dir-locals file.  Might be the same
          ;; as dir-elt's, might not (eg latter might have been deleted).
          locals-dir)
      ;; No cache entry.
      locals-dir)))

(declare-function map-merge-with "map" (type function &rest maps))
(declare-function map-merge "map" (type &rest maps))

(defun dir-locals--get-sort-score (node)
  "Return a number used for sorting the definitions of dir locals.
NODE is assumed to be a cons cell where the car is either a
string or a symbol representing a mode name.

If it is a mode then the depth of the mode (ie, how many parents
that mode has) will be returned.

If it is a string then the length of the string plus 1000 will be
returned.

Otherwise it returns -1.

That way the value can be used to sort the list such that deeper
modes will be after the other modes.  This will be followed by
directory entries in order of length.  If the entries are all
applied in order then that means the more specific modes will
  override the values specified by the earlier modes and directory
variables will override modes."
  (let ((key (car node)))
    (cond ((null key) -1)
          ((symbolp key)
           (let ((mode key)
                 (depth 0))
             (while (setq mode (get mode 'derived-mode-parent))
               (setq depth (1+ depth)))
             depth))
          ((stringp key)
           (+ 1000 (length key)))
          (t -2))))

(defun dir-locals--sort-variables (variables)
  "Sort VARIABLES so that applying them in order has the right effect.
The variables are compared by `dir-locals--get-sort-score'.
Directory entries are then recursively sorted using the same
criteria."
  (setq variables (sort variables
                        (lambda (a b)
                          (< (dir-locals--get-sort-score a)
                             (dir-locals--get-sort-score b)))))
  (dolist (n variables)
    (when (stringp (car n))
      (setcdr n (dir-locals--sort-variables (cdr n)))))

  variables)

(defun dir-locals-read-from-dir (dir)
  "Load all variables files in DIR and register a new class and instance.
DIR is the absolute name of a directory, which must contain at
least one dir-local file (which is a file holding variables to
apply).
Return the new class name, which is a symbol named DIR."
  (let* ((class-name (intern dir))
         (files (dir-locals--all-files dir))
	 ;; If there was a problem, use the values we could get but
	 ;; don't let the cache prevent future reads.
	 (latest 0) (success 0)
         (variables))
    (with-demoted-errors "Error reading dir-locals: %S"
      (dolist (file files)
	(let ((file-time (file-attribute-modification-time
			  (file-attributes file))))
	  (if (time-less-p latest file-time)
	    (setq latest file-time)))
        (with-temp-buffer
          (insert-file-contents file)
          (let ((newvars
                 (condition-case-unless-debug nil
                     ;; As a defensive measure, we do not allow
                     ;; circular data in the file/dir-local data.
                     (let ((read-circle nil))
                       (read (current-buffer)))
                   (end-of-file nil))))
            (unless (listp newvars)
              (message "Invalid data in %s: %s" file newvars)
              (setq newvars nil))
            (setq variables
                  ;; Try and avoid loading `map' since that also loads cl-lib
                  ;; which then might hamper bytecomp warnings (bug#30635).
                  (if (not (and newvars variables))
                      (or newvars variables)
                    (require 'map)
                    ;; We want to make the variable setting from
                    ;; newvars (the second .dir-locals file) take
                    ;; presedence over the old variables, but we also
                    ;; want to preserve all `eval' elements as is from
                    ;; both lists.
                    (map-merge-with
                     'list
                     (lambda (a b)
                       (let ((ag
                              (seq-group-by
                               (lambda (e) (eq (car e) 'eval)) a))
                             (bg
                              (seq-group-by
                               (lambda (e) (eq (car e) 'eval)) b)))
                         (append (map-merge 'list
                                            (assoc-default nil ag)
                                            (assoc-default nil bg))
                                 (assoc-default t ag)
                                 (assoc-default t bg))))
                     variables
                     newvars))))))
      (setq success latest))
    (setq variables (dir-locals--sort-variables variables))
    (dir-locals-set-class-variables class-name variables)
    (dir-locals-set-directory-class dir class-name success)
    class-name))

(define-obsolete-function-alias 'dir-locals-read-from-file
  'dir-locals-read-from-dir "25.1")

(defcustom enable-remote-dir-locals nil
  "Non-nil means dir-local variables will be applied to remote files."
  :version "24.3"
  :type 'boolean
  :group 'find-file)

(defvar hack-dir-local-variables--warned-coding nil)

(defun hack-dir-local--get-variables (predicate)
  "Read per-directory local variables for the current buffer.
Return a cons of the form (DIR . ALIST), where DIR is the
directory name (maybe nil) and ALIST is an alist of all variables
that might apply.  These will be filtered according to the
buffer's directory, but not according to its mode.
PREDICATE is passed to `dir-locals-collect-variables'."
  (when (and enable-local-variables
	     enable-dir-local-variables
	     (or enable-remote-dir-locals
		 (not (file-remote-p (or (buffer-file-name)
					 default-directory)))))
    ;; Find the variables file.
    (let ((dir-or-cache (dir-locals-find-file
                         (or (buffer-file-name) default-directory)))
	  (class nil)
	  (dir-name nil))
      (cond
       ((stringp dir-or-cache)
	(setq dir-name dir-or-cache
	      class (dir-locals-read-from-dir dir-or-cache)))
       ((consp dir-or-cache)
	(setq dir-name (nth 0 dir-or-cache))
	(setq class (nth 1 dir-or-cache))))
      (when class
        (cons dir-name
              (dir-locals-collect-variables
               (dir-locals-get-class-variables class)
               dir-name nil predicate))))))

(defun hack-dir-local-variables ()
  "Read per-directory local variables for the current buffer.
Store the directory-local variables in `dir-local-variables-alist'
and `file-local-variables-alist', without applying them.

This does nothing if either `enable-local-variables' or
`enable-dir-local-variables' are nil."
  (let* ((items (hack-dir-local--get-variables nil))
         (dir-name (car items))
         (variables (cdr items)))
    (when variables
      (dolist (elt variables)
        (if (eq (car elt) 'coding)
            (unless hack-dir-local-variables--warned-coding
              (setq hack-dir-local-variables--warned-coding t)
              (display-warning 'files
                               "Coding cannot be specified by dir-locals"))
          (unless (memq (car elt) '(eval mode))
            (setq dir-local-variables-alist
                  (assq-delete-all (car elt) dir-local-variables-alist)))
          (push elt dir-local-variables-alist)))
      (hack-local-variables-filter variables dir-name))))

(defun hack-dir-local-variables-non-file-buffer ()
  "Apply directory-local variables to a non-file buffer.
For non-file buffers, such as Dired buffers, directory-local
variables are looked for in `default-directory' and its parent
directories."
  (hack-dir-local-variables)
  (hack-local-variables-apply))


(defcustom change-major-mode-with-file-name t
  "Non-nil means \\[write-file] should set the major mode from the file name.
However, the mode will not be changed if
\(1) a local variables list or the `-*-' line specifies a major mode, or
\(2) the current major mode is a \"special\" mode,
    not suitable for ordinary files, or
\(3) the new file name does not particularly specify any mode."
  :type 'boolean
  :group 'editing-basics)

(defvar after-set-visited-file-name-hook nil
  "Normal hook run just after setting visited file name of current buffer.")

(defun set-visited-file-name (filename &optional no-query along-with-file)
  "Change name of file visited in current buffer to FILENAME.
This also renames the buffer to correspond to the new file.
The next time the buffer is saved it will go in the newly specified file.
FILENAME nil or an empty string means mark buffer as not visiting any file.
Remember to delete the initial contents of the minibuffer
if you wish to pass an empty string as the argument.

The optional second argument NO-QUERY, if non-nil, inhibits asking for
confirmation in the case where another buffer is already visiting FILENAME.

The optional third argument ALONG-WITH-FILE, if non-nil, means that
the old visited file has been renamed to the new name FILENAME."
  (interactive "FSet visited file name: ")
  (if (buffer-base-buffer)
      (error "An indirect buffer cannot visit a file"))
  (let (truename old-try-locals)
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
    (if filename
	(let ((new-name (file-name-nondirectory filename)))
	  (if (string= new-name "")
	      (error "Empty file name"))))
    (let ((buffer (and filename (find-buffer-visiting filename))))
      (and buffer (not (eq buffer (current-buffer)))
	   (not no-query)
	   (not (y-or-n-p (format "A buffer is visiting %s; proceed? "
				  filename)))
	   (user-error "Aborted")))
    (or (equal filename buffer-file-name)
	(progn
	  (and filename (lock-buffer filename))
	  (unlock-buffer)))
    (setq old-try-locals (not (inhibit-local-variables-p))
	  buffer-file-name filename)
    (if filename			; make buffer name reflect filename.
	(let ((new-name (file-name-nondirectory buffer-file-name)))
	  (setq default-directory (file-name-directory buffer-file-name))
	  ;; If new-name == old-name, renaming would add a spurious <2>
	  ;; and it's considered as a feature in rename-buffer.
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
	      (setq buffer-file-name truename))))
    (setq buffer-file-number
	  (if filename
	      (nthcdr 10 (file-attributes buffer-file-name))
	    nil))
    ;; write-file-functions is normally used for things like ftp-find-file
    ;; that visit things that are not local files as if they were files.
    ;; Changing to visit an ordinary local file instead should flush the hook.
    (kill-local-variable 'write-file-functions)
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
	 backup-enable-predicate
	 (not (funcall backup-enable-predicate buffer-file-name))
         (setq-local backup-inhibited t))
    (let ((oauto buffer-auto-save-file-name))
      (cond ((null filename)
	     (setq buffer-auto-save-file-name nil))
	    ((not buffer-auto-save-file-name)
	     ;; If auto-save was not already on, turn it on if appropriate.
	     (and buffer-file-name auto-save-default (auto-save-mode t)))
	    (t
	     ;; If auto save is on, start using a new name. We
	     ;; deliberately don't rename or delete the old auto save
	     ;; for the old visited file name.  This is because
	     ;; perhaps the user wants to save the new state and then
	     ;; compare with the previous state from the auto save
	     ;; file.
	     (setq buffer-auto-save-file-name (make-auto-save-file-name))))
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
	    ;; The file name can influence whether the local variables apply.
	    (and old-try-locals
		 ;; h-l-v also checks it, but might as well be explicit.
		 (not (inhibit-local-variables-p))
		 (hack-local-variables t))
	    ;; TODO consider making normal-mode handle this case.
	    (let ((old major-mode))
	      (set-auto-mode t)
	      (or (eq old major-mode)
		  (hack-local-variables))))
      (error nil))
    (run-hooks 'after-set-visited-file-name-hook)))

(defun write-file (filename &optional confirm)
  "Write current buffer into file FILENAME.
This makes the buffer visit that file, and marks it as not modified.

Interactively, prompt for FILENAME.
If you specify just a directory name as FILENAME, that means to write
to a file in that directory.  In this case, the base name of the file
is the same as that of the file visited in the buffer, or the buffer
name sans leading directories, if any, if the buffer is not already
visiting a file.

You can also yank the file name into the minibuffer to edit it,
using \\<minibuffer-local-map>\\[next-history-element].

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
	;; If arg is a directory name,
	;; use the default file name, but in that directory.
	(if (directory-name-p filename)
	    (setq filename (concat filename
				   (file-name-nondirectory
				    (or buffer-file-name (buffer-name))))))
	(and confirm
	     (file-exists-p filename)
	     ;; NS does its own confirm dialog.
	     (not (and (eq (framep-on-display) 'ns)
		       (listp last-nonmenu-event)
		       use-dialog-box))
	     (or (y-or-n-p (format-message
                            "File `%s' exists; overwrite? " filename))
		 (user-error "Canceled")))
	(set-visited-file-name filename (not confirm))))
  (set-buffer-modified-p t)
  ;; Make buffer writable if file is writable.
  (and buffer-file-name
       (file-writable-p buffer-file-name)
       (setq buffer-read-only nil))
  (save-buffer)
  ;; It's likely that the VC status at the new location is different from
  ;; the one at the old location.
  (vc-refresh-state))

(defun file-extended-attributes (filename)
  "Return an alist of extended attributes of file FILENAME.

Extended attributes are platform-specific metadata about the file,
such as SELinux context, list of ACL entries, etc."
  `((acl . ,(file-acl filename))
    (selinux-context . ,(file-selinux-context filename))))

(defun set-file-extended-attributes (filename attributes)
  "Set extended attributes of file FILENAME to ATTRIBUTES.

ATTRIBUTES must be an alist of file attributes as returned by
`file-extended-attributes'.
Value is t if the function succeeds in setting the attributes."
  (let (result rv)
    (dolist (elt attributes)
      (let ((attr (car elt))
	    (val (cdr elt)))
	(cond ((eq attr 'acl)
               (setq rv (set-file-acl filename val)))
	      ((eq attr 'selinux-context)
               (setq rv (set-file-selinux-context filename val))))
        (setq result (or result rv))))

    result))

(defun backup-buffer ()
  "Make a backup of the disk file visited by the current buffer, if appropriate.
This is normally done before saving the buffer the first time.

A backup may be done by renaming or by copying; see documentation of
variable `make-backup-files'.  If it's done by renaming, then the file is
no longer accessible under its old name.

The value is non-nil after a backup was made by renaming.
It has the form (MODES EXTENDED-ATTRIBUTES BACKUPNAME).
MODES is the result of `file-modes' on the original
file; this means that the caller, after saving the buffer, should change
the modes of the new file to agree with the old modes.
EXTENDED-ATTRIBUTES is the result of `file-extended-attributes'
on the original file; this means that the caller, after saving
the buffer, should change the extended attributes of the new file
to agree with the old attributes.
BACKUPNAME is the backup file name, which is the old file renamed."
  (when (and make-backup-files (not backup-inhibited) (not buffer-backed-up))
    (let ((attributes (file-attributes buffer-file-name)))
      (when (and attributes (memq (aref (elt attributes 8) 0) '(?- ?l)))
	;; If specified name is a symbolic link, chase it to the target.
	;; This makes backups in the directory where the real file is.
	(let* ((real-file-name (file-chase-links buffer-file-name))
	       (backup-info (find-backup-file-name real-file-name)))
	  (when backup-info
	    (let* ((backupname (car backup-info))
		   (targets (cdr backup-info))
		   (old-versions
		    ;; If have old versions to maybe delete,
		    ;; ask the user to confirm now, before doing anything.
		    ;; But don't actually delete til later.
		    (and targets
			 (booleanp delete-old-versions)
			 (or delete-old-versions
			     (y-or-n-p
			      (format "Delete excess backup versions of %s? "
				      real-file-name)))
			 targets))
		   (modes (file-modes buffer-file-name))
		   (extended-attributes
		    (file-extended-attributes buffer-file-name))
		   (copy-when-priv-mismatch
		    backup-by-copying-when-privileged-mismatch)
		   (make-copy
		    (or file-precious-flag backup-by-copying
			;; Don't rename a suid or sgid file.
			(and modes (< 0 (logand modes #o6000)))
			(not (file-writable-p
			      (file-name-directory real-file-name)))
			(and backup-by-copying-when-linked
			     (< 1 (file-nlinks real-file-name)))
			(and (or backup-by-copying-when-mismatch
				 (and (integerp copy-when-priv-mismatch)
				      (let ((attr (file-attributes
						   real-file-name
						   'integer)))
                                        (or (<= (file-attribute-user-id attr)
                                                copy-when-priv-mismatch)
                                            (<= (file-attribute-group-id attr)
                                                copy-when-priv-mismatch)))))
			     (not (file-ownership-preserved-p real-file-name
							      t)))))
		   setmodes)
	      (condition-case ()
		  (progn
		    ;; Actually make the backup file.
		    (if make-copy
			(backup-buffer-copy real-file-name backupname
					    modes extended-attributes)
		      ;; rename-file should delete old backup.
		      (rename-file real-file-name backupname t)
		      (setq setmodes (list modes extended-attributes
					   backupname)))
		    (setq buffer-backed-up t)
		    ;; Now delete the old versions, if desired.
		    (dolist (old-version old-versions)
		      (delete-file old-version)))
		(file-error nil))
	      ;; If trouble writing the backup, write it in .emacs.d/%backup%.
	      (when (not buffer-backed-up)
		(setq backupname (locate-user-emacs-file "%backup%~"))
		(message "Cannot write backup file; backing up in %s"
			 backupname)
		(sleep-for 1)
		(backup-buffer-copy real-file-name backupname
				    modes extended-attributes)
		(setq buffer-backed-up t))
	      setmodes)))))))

(defun backup-buffer-copy (from-name to-name modes extended-attributes)
  ;; Create temp files with strict access rights.  It's easy to
  ;; loosen them later, whereas it's impossible to close the
  ;; time-window of loose permissions otherwise.
 (let (nofollow-flag)
  (with-file-modes ?\700
    (when (condition-case nil
	      ;; Try to overwrite old backup first.
	      (copy-file from-name to-name t t t)
	    (error t))
      (while (condition-case nil
		 (progn
		   (when (file-exists-p to-name)
		     (delete-file to-name))
		   (copy-file from-name to-name nil t t)
		   (setq nofollow-flag 'nofollow)
		   nil)
	       (file-already-exists t))
	;; The file was somehow created by someone else between
	;; `delete-file' and `copy-file', so let's try again.
	;; rms says "I think there is also a possible race
	;; condition for making backup files" (emacs-devel 20070821).
	nil)))
  ;; If set-file-extended-attributes fails, fall back on set-file-modes.
  (unless (and extended-attributes
	       (with-demoted-errors
		 (set-file-extended-attributes to-name extended-attributes)))
    (and modes
	 (set-file-modes to-name (logand modes #o1777) nofollow-flag)))))

(defvar file-name-version-regexp
  "\\(?:~\\|\\.~[-[:alnum:]:#@^._]+\\(?:~[[:digit:]]+\\)?~\\)"
  ;; The last ~[[:digit]]+ matches relative versions in git,
  ;; e.g. `foo.js.~HEAD~1~'.
  "Regular expression matching the backup/version part of a file name.
Used by `file-name-sans-versions'.")

(defun file-name-sans-versions (name &optional keep-backup-version)
  "Return file NAME sans backup versions or strings.
This is a separate procedure so your site-init or startup file can
redefine it.
If the optional argument KEEP-BACKUP-VERSION is non-nil,
we do not remove backup version numbers, only true file version numbers.
See also `file-name-version-regexp'."
  (let ((handler (find-file-name-handler name 'file-name-sans-versions)))
    (if handler
	(funcall handler 'file-name-sans-versions name keep-backup-version)
      (substring name 0
		 (unless keep-backup-version
                   (string-match (concat file-name-version-regexp "\\'")
                                 name))))))

(defun file-ownership-preserved-p (file &optional group)
  "Return t if deleting FILE and rewriting it would preserve the owner.
Return also t if FILE does not exist.  If GROUP is non-nil, check whether
the group would be preserved too."
  (let ((handler (find-file-name-handler file 'file-ownership-preserved-p)))
    (if handler
	(funcall handler 'file-ownership-preserved-p file group)
      (let ((attributes (file-attributes file 'integer)))
	;; Return t if the file doesn't exist, since it's true that no
	;; information would be lost by an (attempted) delete and create.
	(or (null attributes)
	    (and (or (= (file-attribute-user-id attributes) (user-uid))
		     ;; Files created on Windows by Administrator (RID=500)
		     ;; have the Administrators group (RID=544) recorded as
		     ;; their owner.  Rewriting them will still preserve the
		     ;; owner.
		     (and (eq system-type 'windows-nt)
			  (= (user-uid) 500)
			  (= (file-attribute-user-id attributes) 544)))
		 (or (not group)
		     ;; On BSD-derived systems files always inherit the parent
		     ;; directory's group, so skip the group-gid test.
		     (memq system-type '(berkeley-unix darwin gnu/kfreebsd))
		     (= (file-attribute-group-id attributes) (group-gid)))
		 (let* ((parent (or (file-name-directory file) "."))
			(parent-attributes (file-attributes parent 'integer)))
		   (and parent-attributes
			;; On some systems, a file created in a setuid directory
			;; inherits that directory's owner.
			(or
			 (= (file-attribute-user-id parent-attributes)
			    (user-uid))
			 (string-match
			  "^...[^sS]"
			  (file-attribute-modes parent-attributes)))
			;; On many systems, a file created in a setgid directory
			;; inherits that directory's group.  On some systems
			;; this happens even if the setgid bit is not set.
			(or (not group)
			    (= (file-attribute-group-id parent-attributes)
			       (file-attribute-group-id attributes)))))))))))

(defun file-name-sans-extension (filename)
  "Return FILENAME sans final \"extension\".
The extension, in a file name, is the part that begins with the last `.',
except that a leading `.' of the file name, if there is one, doesn't count."
  (save-match-data
    (let ((file (file-name-sans-versions (file-name-nondirectory filename)))
	  directory)
      (if (and (string-match "\\.[^.]*\\'" file)
	       (not (eq 0 (match-beginning 0))))
	  (if (setq directory (file-name-directory filename))
	      ;; Don't use expand-file-name here; if DIRECTORY is relative,
	      ;; we don't want to expand it.
	      (concat directory (substring file 0 (match-beginning 0)))
	    (substring file 0 (match-beginning 0)))
	filename))))

(defun file-name-extension (filename &optional period)
  "Return FILENAME's final \"extension\".
The extension, in a file name, is the part that begins with the last `.',
excluding version numbers and backup suffixes, except that a leading `.'
of the file name, if there is one, doesn't count.
Return nil for extensionless file names such as `foo'.
Return the empty string for file names such as `foo.'.

By default, the returned value excludes the period that starts the
extension, but if the optional argument PERIOD is non-nil, the period
is included in the value, and in that case, if FILENAME has no
extension, the value is \"\"."
  (save-match-data
    (let ((file (file-name-sans-versions (file-name-nondirectory filename))))
      (if (and (string-match "\\.[^.]*\\'" file)
	       (not (eq 0 (match-beginning 0))))
          (substring file (+ (match-beginning 0) (if period 0 1)))
        (if period
            "")))))

(defun file-name-with-extension (filename extension)
  "Set the EXTENSION of a FILENAME.
The extension (in a file name) is the part that begins with the last \".\".

Trims a leading dot from the EXTENSION so that either \"foo\" or
\".foo\" can be given.

Errors if the FILENAME or EXTENSION are empty, or if the given
FILENAME has the format of a directory.

See also `file-name-sans-extension'."
  (let ((extn (string-trim-left extension "[.]")))
    (cond ((string-empty-p filename)
           (error "Empty filename"))
          ((string-empty-p extn)
           (error "Malformed extension: %s" extension))
          ((directory-name-p filename)
           (error "Filename is a directory: %s" filename))
          (t
           (concat (file-name-sans-extension filename) "." extn)))))

(defun file-name-base (&optional filename)
  "Return the base name of the FILENAME: no directory, no extension."
  (declare (advertised-calling-convention (filename) "27.1"))
  (file-name-sans-extension
   (file-name-nondirectory (or filename (buffer-file-name)))))

(defcustom make-backup-file-name-function
  #'make-backup-file-name--default-function
  "A function that `make-backup-file-name' uses to create backup file names.
The function receives a single argument, the original file name.

If you change this, you may need to change `backup-file-name-p' and
`file-name-sans-versions' too.

You could make this buffer-local to do something special for specific files.

For historical reasons, a value of nil means to use the default function.
This should not be relied upon.

See also `backup-directory-alist'."
  :version "24.4"     ; nil -> make-backup-file-name--default-function
  :group 'backup
  :type '(choice (const :tag "Deprecated way to get the default function" nil)
		 (function :tag "Function")))

(defcustom backup-directory-alist nil
  "Alist of file name patterns and backup directory names.
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

If this variable is nil, or it fails to match a file name, the backup
is made in the original file's directory.

On MS-DOS filesystems without long names this variable is always
ignored."
  :group 'backup
  :type '(repeat (cons (regexp :tag "Regexp matching filename")
		       (directory :tag "Backup directory name"))))

(defun normal-backup-enable-predicate (name)
  "Default `backup-enable-predicate' function.
Checks for files in `temporary-file-directory',
`small-temporary-file-directory', and \"/tmp\"."
  (let ((temporary-file-directory temporary-file-directory)
	caseless)
    ;; On MS-Windows, file-truename will convert short 8+3 aliases to
    ;; their long file-name equivalents, so compare-strings does TRT.
    (if (memq system-type '(ms-dos windows-nt))
	(setq temporary-file-directory (file-truename temporary-file-directory)
	      name (file-truename name)
	      caseless t))
    (not (or (let ((comp (compare-strings temporary-file-directory 0 nil
					  name 0 nil caseless)))
	       ;; Directory is under temporary-file-directory.
	       (and (not (eq comp t))
		    (< comp (- (length temporary-file-directory)))))
	     (let ((comp (compare-strings "/tmp" 0 nil
					  name 0 nil)))
	       ;; Directory is under /tmp.
	       (and (not (eq comp t))
		    (< comp (- (length "/tmp")))))
	     (if small-temporary-file-directory
		 (let ((comp (compare-strings small-temporary-file-directory
					      0 nil
					      name 0 nil caseless)))
		   ;; Directory is under small-temporary-file-directory.
		   (and (not (eq comp t))
			(< comp (- (length small-temporary-file-directory))))))))))

(defun make-backup-file-name (file)
  "Create the non-numeric backup file name for FILE.
This calls the function that `make-backup-file-name-function' specifies,
with a single argument FILE."
  (funcall (or make-backup-file-name-function
               #'make-backup-file-name--default-function)
           file))

(defun make-backup-file-name--default-function (file)
  "Default function for `make-backup-file-name'.
Normally this just returns FILE's name with `~' appended.
It searches for a match for FILE in `backup-directory-alist'.
If the directory for the backup doesn't exist, it is created."
  (if (and (eq system-type 'ms-dos)
           (not (msdos-long-file-names)))
      (let ((fn (file-name-nondirectory file)))
        (concat (file-name-directory file)
                (or (and (string-match "\\`[^.]+\\'" fn)
                         (concat (match-string 0 fn) ".~"))
                    (and (string-match "\\`[^.]+\\.\\(..?\\)?" fn)
                         (concat (match-string 0 fn) "~")))))
    (concat (make-backup-file-name-1 file) "~")))

(defun make-backup-file-name-1 (file)
  "Subroutine of `make-backup-file-name--default-function'.
The function `find-backup-file-name' also uses this."
  (let ((alist backup-directory-alist)
	elt backup-directory abs-backup-directory)
    (while alist
      (setq elt (pop alist))
      (if (string-match (car elt) file)
	  (setq backup-directory (cdr elt)
		alist nil)))
    ;; If backup-directory is relative, it should be relative to the
    ;; file's directory.  By expanding explicitly here, we avoid
    ;; depending on default-directory.
    (if backup-directory
	(setq abs-backup-directory
	      (expand-file-name backup-directory
				(file-name-directory file))))
    (if (and abs-backup-directory (not (file-exists-p abs-backup-directory)))
	(condition-case nil
	    (make-directory abs-backup-directory 'parents)
	  (file-error (setq backup-directory nil
			    abs-backup-directory nil))))
    (if (null backup-directory)
	file
      (if (file-name-absolute-p backup-directory)
	  (progn
	    (when (memq system-type '(windows-nt ms-dos cygwin))
	      ;; Normalize DOSish file names: downcase the drive
	      ;; letter, if any, and replace the leading "x:" with
	      ;; "/drive_x".
	      (or (file-name-absolute-p file)
		  (setq file (expand-file-name file))) ; make defaults explicit
              (cond
               ((file-remote-p file)
                ;; Remove the leading slash, if any, to prevent
                ;; convert-standard-filename from converting that to a
                ;; backslash.
                (and (memq (aref file 0) '(?/ ?\\))
                     (setq file (substring file 1)))
	        ;; Replace any invalid file-name characters, then
	        ;; prepend the leading slash back.
                (setq file (concat "/" (convert-standard-filename file))))
               (t
	        ;; Replace any invalid file-name characters.
	        (setq file (expand-file-name (convert-standard-filename file)))
	        (if (eq (aref file 1) ?:)
		    (setq file (concat "/"
				       "drive_"
				       (char-to-string (downcase (aref file 0)))
				       (if (eq (aref file 2) ?/)
					   ""
				         "/")
				       (substring file 2)))))))
	    ;; Make the name unique by substituting directory
	    ;; separators.  It may not really be worth bothering about
	    ;; doubling `!'s in the original name...
	    (expand-file-name
	     (subst-char-in-string
	      ?/ ?!
	      (string-replace "!" "!!" file))
	     backup-directory))
	(expand-file-name (file-name-nondirectory file)
			  (file-name-as-directory abs-backup-directory))))))

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
  (if (and (string-match "[0-9]+~/?$" fn backup-extract-version-start)
	   (= (match-beginning 0) backup-extract-version-start))
      (string-to-number (substring fn backup-extract-version-start -1))
      0))

(defun find-backup-file-name (fn)
  "Find a file name for a backup file FN, and suggestions for deletions.
Value is a list whose car is the name for the backup file
and whose cdr is a list of old versions to consider deleting now.
If the value is nil, don't make a backup.
Uses `backup-directory-alist' in the same way as
`make-backup-file-name--default-function' does."
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
			   ;; Delete nothing if kept-new-versions and
			   ;; kept-old-versions combine to an outlandish value.
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
This function returns a relative file name that is equivalent to FILENAME
when used with that default directory as the default.
If FILENAME is a relative file name, it will be interpreted as existing in
`default-directory'.
If FILENAME and DIRECTORY lie on different machines or on different drives
on a DOS/Windows machine, it returns FILENAME in expanded form."
  (save-match-data
    (setq directory
	  (file-name-as-directory (expand-file-name (or directory
							default-directory))))
    (setq filename (expand-file-name filename))
    (let ((fremote (file-remote-p filename))
	  (dremote (file-remote-p directory))
	  (fold-case (or (file-name-case-insensitive-p filename)
			 read-file-name-completion-ignore-case)))
      (if ;; Conditions for separate trees
	  (or
	   ;; Test for different filesystems on DOS/Windows
	   (and
	    ;; Should `cygwin' really be included here?  --stef
	    (memq system-type '(ms-dos cygwin windows-nt))
	    (or
	     ;; Test for different drive letters
	     (not (eq t (compare-strings filename 0 2 directory 0 2 fold-case)))
	     ;; Test for UNCs on different servers
	     (not (eq t (compare-strings
			 (progn
			   (if (string-match "\\`//\\([^:/]+\\)/" filename)
			       (match-string 1 filename)
			     ;; Windows file names cannot have ? in
			     ;; them, so use that to detect when
			     ;; neither FILENAME nor DIRECTORY is a
			     ;; UNC.
			     "?"))
			 0 nil
			 (progn
			   (if (string-match "\\`//\\([^:/]+\\)/" directory)
			       (match-string 1 directory)
			     "?"))
			 0 nil t)))))
	   ;; Test for different remote file system identification
	   (not (equal fremote dremote)))
	  filename
        (let ((ancestor ".")
	      (filename-dir (file-name-as-directory filename)))
          (while (not
		  (or (string-prefix-p directory filename-dir fold-case)
		      (string-prefix-p directory filename fold-case)))
            (setq directory (file-name-directory (substring directory 0 -1))
		  ancestor (if (equal ancestor ".")
			       ".."
			     (concat "../" ancestor))))
          ;; Now ancestor is empty, or .., or ../.., etc.
          (if (string-prefix-p directory filename fold-case)
	      ;; We matched within FILENAME's directory part.
	      ;; Add the rest of FILENAME onto ANCESTOR.
	      (let ((rest (substring filename (length directory))))
		(if (and (equal ancestor ".") (not (equal rest "")))
		    ;; But don't bother with ANCESTOR if it would give us `./'.
		    rest
		  (concat (file-name-as-directory ancestor) rest)))
            ;; We matched FILENAME's directory equivalent.
            ancestor))))))

(defun save-buffer (&optional arg)
  "Save current buffer in visited file if modified.
Variations are described below.

By default, makes the previous version into a backup file
 if previously requested or if this is the first save.
Prefixed with one \\[universal-argument], marks this version
 to become a backup when the next save is done.
Prefixed with two \\[universal-argument]'s,
 makes the previous version into a backup file.
Prefixed with three \\[universal-argument]'s, marks this version
 to become a backup when the next save is done,
 and makes the previous version into a backup file.

With a numeric prefix argument of 0, never make the previous version
into a backup file.

Note that the various variables that control backups, such
as `version-control', `backup-enable-predicate', `vc-make-backup-files',
and `backup-inhibited', to name just the more popular ones, still
control whether a backup will actually be produced, even when you
invoke this command prefixed with two or three \\[universal-argument]'s.

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
	(make-backup-files (or (and make-backup-files (not (eq arg 0)))
			       (memq arg '(16 64)))))
    (and modp (memq arg '(16 64)) (setq buffer-backed-up nil))
    ;; We used to display the message below only for files > 50KB, but
    ;; then Rmail-mbox never displays it due to buffer swapping.  If
    ;; the test is ever re-introduced, be sure to handle saving of
    ;; Rmail files.
    (if (and modp
             (buffer-file-name)
             (not noninteractive)
             (not save-silently))
	(message "Saving file %s..." (buffer-file-name)))
    (basic-save-buffer (called-interactively-p 'any))
    (and modp (memq arg '(4 64)) (setq buffer-backed-up nil))))

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

(defcustom before-save-hook nil
  "Normal hook that is run before a buffer is saved to its file.
Used only by `save-buffer'."
  :options '(copyright-update time-stamp)
  :type 'hook
  :group 'files)

(defcustom after-save-hook nil
  "Normal hook that is run after a buffer is saved to its file.
Used only by `save-buffer'."
  :options '(executable-make-buffer-file-executable-if-script-p)
  :type 'hook
  :group 'files)

(defcustom copy-directory-create-symlink nil
  "This option influences the handling of symbolic links in `copy-directory'.
If non-nil, `copy-directory' will create a symbolic link if the
source directory is a symbolic link.  If nil, it'll follow the
symbolic link and copy the contents instead."
  :type 'boolean
  :version "28.1"
  :group 'files)

(defcustom file-preserve-symlinks-on-save nil
  "If non-nil, saving a buffer visited via a symlink won't overwrite the symlink.
This is only relevant if `file-precious-flag' is non-nil -- if
this is nil, Emacs will preserve the symlinks anyway."
  :type 'boolean
  :version "28.1"
  :group 'files)

(defvar-local save-buffer-coding-system nil
  "If non-nil, use this coding system for saving the buffer.
More precisely, use this coding system in place of the
value of `buffer-file-coding-system', when saving the buffer.
Calling `write-region' for any purpose other than saving the buffer
will still use `buffer-file-coding-system'; this variable has no effect
in such cases.")

(put 'save-buffer-coding-system 'permanent-local t)

(defun basic-save-buffer (&optional called-interactively)
  "Save the current buffer in its visited file, if it has been modified.

The hooks `write-contents-functions', `local-write-file-hooks'
and `write-file-functions' get a chance to do the job of saving;
if they do not, then the buffer is saved in the visited file in
the usual way.

Before and after saving the buffer, this function runs
`before-save-hook' and `after-save-hook', respectively."
  (interactive '(called-interactively))
  (save-current-buffer
    ;; In an indirect buffer, save its base buffer instead.
    (if (buffer-base-buffer)
	(set-buffer (buffer-base-buffer)))
    (if (or (buffer-modified-p)
	    ;; Handle the case when no modification has been made but
	    ;; the file disappeared since visited.
	    (and buffer-file-name
		 (not (file-exists-p buffer-file-name))))
	(let ((recent-save (recent-auto-save-p))
	      setmodes)
	  (or (null buffer-file-name)
              (verify-visited-file-modtime (current-buffer))
	      (not (file-exists-p buffer-file-name))
	      (yes-or-no-p
	       (format
		"%s has changed since visited or saved.  Save anyway? "
		(file-name-nondirectory buffer-file-name)))
	      (user-error "Save not confirmed"))
	  (save-restriction
	    (widen)
	    (save-excursion
	      (and (> (point-max) (point-min))
		   (not find-file-literally)
                   (null buffer-read-only)
		   (/= (char-after (1- (point-max))) ?\n)
		   (not (and (eq selective-display t)
			     (= (char-after (1- (point-max))) ?\r)))
		   (or (eq require-final-newline t)
		       (eq require-final-newline 'visit-save)
		       (and require-final-newline
			    (y-or-n-p
			     (format "Buffer %s does not end in newline.  Add one? "
				     (buffer-name)))))
		   (save-excursion
		     (goto-char (point-max))
		     (insert ?\n))))
	    ;; Don't let errors prevent saving the buffer.
	    (with-demoted-errors (run-hooks 'before-save-hook))
            ;; Give `write-contents-functions' a chance to
            ;; short-circuit the whole process.
	    (unless (run-hook-with-args-until-success 'write-contents-functions)
              ;; If buffer has no file name, ask user for one.
              (or buffer-file-name
                  (let ((filename
                         (expand-file-name
                          (read-file-name "File to save in: "
                                          nil (expand-file-name (buffer-name))))))
                    (if (file-exists-p filename)
                        (if (file-directory-p filename)
                            ;; Signal an error if the user specified the name of an
                            ;; existing directory.
                            (error "%s is a directory" filename)
                          (unless (y-or-n-p (format-message
                                             "File `%s' exists; overwrite? "
                                             filename))
                            (error "Canceled"))))
                    (set-visited-file-name filename)))
              ;; Support VC version backups.
	      (vc-before-save)
	      (or (run-hook-with-args-until-success 'local-write-file-hooks)
	          (run-hook-with-args-until-success 'write-file-functions)
	          ;; If a hook returned t, file is already "written".
	          ;; Otherwise, write it the usual way now.
	          (let ((dir (file-name-directory
			      (expand-file-name buffer-file-name))))
		    (unless (file-exists-p dir)
		      (if (y-or-n-p
		           (format-message
                            "Directory `%s' does not exist; create? " dir))
		          (make-directory dir t)
		        (error "Canceled")))
		    (setq setmodes (basic-save-buffer-1)))))
	    ;; Now we have saved the current buffer.  Let's make sure
	    ;; that buffer-file-coding-system is fixed to what
	    ;; actually used for saving by binding it locally.
            (when buffer-file-name
	      (if save-buffer-coding-system
		  (setq save-buffer-coding-system last-coding-system-used)
	        (setq buffer-file-coding-system last-coding-system-used))
	      (setq buffer-file-number
		    (nthcdr 10 (file-attributes buffer-file-name)))
	      (if setmodes
		  (condition-case ()
		      (progn
		        (unless
			    (with-demoted-errors
			        (set-file-modes buffer-file-name (car setmodes)))
			  (set-file-extended-attributes buffer-file-name
						        (nth 1 setmodes))))
		    (error nil)))
              ;; Support VC `implicit' locking.
	      (vc-after-save))
            ;; If the auto-save file was recent before this command,
	    ;; delete it now.
	    (delete-auto-save-file-if-necessary recent-save))
	  (run-hooks 'after-save-hook))
      (or noninteractive
          (not called-interactively)
          (files--message "(No changes need to be saved)")))))

;; This does the "real job" of writing a buffer into its visited file
;; and making a backup file.  This is what is normally done
;; but inhibited if one of write-file-functions returns non-nil.
;; It returns a value (MODES EXTENDED-ATTRIBUTES BACKUPNAME), like
;; backup-buffer.
(defun basic-save-buffer-1 ()
  (prog1
      (if save-buffer-coding-system
	  (let ((coding-system-for-write save-buffer-coding-system))
	    (basic-save-buffer-2))
	(basic-save-buffer-2))
    (if buffer-file-coding-system-explicit
	(setcar buffer-file-coding-system-explicit last-coding-system-used))))

;; This returns a value (MODES EXTENDED-ATTRIBUTES BACKUPNAME), like
;; backup-buffer.
(defun basic-save-buffer-2 ()
  (let (tempsetmodes setmodes)
    (if (not (file-writable-p buffer-file-name))
	(let ((dir (file-name-directory buffer-file-name)))
	  (if (not (file-directory-p dir))
	      (if (file-exists-p dir)
		  (error "%s is not a directory" dir)
                (error "%s: No such directory" dir))
	    (if (not (file-exists-p buffer-file-name))
		(error "Directory %s write-protected" dir)
	      (if (yes-or-no-p
		   (format
		    "File %s is write-protected; try to save anyway? "
		    (file-name-nondirectory
		     buffer-file-name)))
		  (setq tempsetmodes t)
		(error "Attempt to save to a file that you aren't allowed to write"))))))
    (or buffer-backed-up
	(setq setmodes (backup-buffer)))
    (let* ((dir (file-name-directory buffer-file-name))
           (dir-writable (file-writable-p dir)))
      (if (or (and file-precious-flag dir-writable)
              (and break-hardlink-on-save
                   (file-exists-p buffer-file-name)
                   (> (file-nlinks buffer-file-name) 1)
                   (or dir-writable
                       (error (concat "Directory %s write-protected; "
                                      "cannot break hardlink when saving")
                              dir))))
	  ;; Write temp name, then rename it.
	  ;; This requires write access to the containing dir,
	  ;; which is why we don't try it if we don't have that access.
	  (let ((realname buffer-file-name)
		tempname
		(old-modtime (visited-file-modtime)))
	    ;; Create temp files with strict access rights.  It's easy to
	    ;; loosen them later, whereas it's impossible to close the
	    ;; time-window of loose permissions otherwise.
	    (condition-case err
		(progn
		  (clear-visited-file-modtime)
		  ;; Call write-region in the appropriate way
		  ;; for saving the buffer.
		  (setq tempname
			(make-temp-file
			 (expand-file-name "tmp" dir)))
		  ;; Pass in nil&nil rather than point-min&max
		  ;; cause we're saving the whole buffer.
		  ;; write-region-annotate-functions may use it.
		  (write-region nil nil tempname nil realname
				buffer-file-truename)
		  (when save-silently (message nil)))
	      ;; If we failed, restore the buffer's modtime.
	      (error (set-visited-file-modtime old-modtime)
		     (signal (car err) (cdr err))))
	    ;; Since we have created an entirely new file,
	    ;; make sure it gets the right permission bits set.
	    (setq setmodes (or setmodes
 			       (list (or (file-modes buffer-file-name)
					 (logand ?\666 (default-file-modes)))
				     (file-extended-attributes buffer-file-name)
				     buffer-file-name)))
	    ;; We succeeded in writing the temp file,
	    ;; so rename it.
	    (rename-file tempname
                         (if (and file-preserve-symlinks-on-save
                                  (file-symlink-p buffer-file-name))
                             ;; Write to the file that the symlink
                             ;; points to.
                             (file-chase-links buffer-file-name)
                           buffer-file-name)
                         t))
	;; If file not writable, see if we can make it writable
	;; temporarily while we write it.
	;; But no need to do so if we have just backed it up
	;; (setmodes is set) because that says we're superseding.
	(cond ((and tempsetmodes (not setmodes))
	       ;; Change the mode back, after writing.
	       (setq setmodes (list (file-modes buffer-file-name)
				    (file-extended-attributes buffer-file-name)
				    buffer-file-name))
	       ;; If set-file-extended-attributes fails, fall back on
	       ;; set-file-modes.
	       (unless
		   (with-demoted-errors
		     (set-file-extended-attributes buffer-file-name
						   (nth 1 setmodes)))
		 (set-file-modes buffer-file-name
				 (logior (car setmodes) 128)))))
	(let (success)
	  (unwind-protect
	      (progn
                ;; Pass in nil&nil rather than point-min&max to indicate
                ;; we're saving the buffer rather than just a region.
                ;; write-region-annotate-functions may make use of it.
                (write-region nil nil
                              buffer-file-name nil t buffer-file-truename)
                (when save-silently (message nil))
		(setq success t))
	    ;; If we get an error writing the new file, and we made
	    ;; the backup by renaming, undo the backing-up.
	    (and setmodes (not success)
		 (progn
		   (rename-file (nth 2 setmodes) buffer-file-name t)
		   (setq buffer-backed-up nil)))))))
    setmodes))

(declare-function diff-no-select "diff"
		  (old new &optional switches no-async buf))

(defvar save-some-buffers--switch-window-callback nil)

(defvar save-some-buffers-action-alist
  `((?\C-r
     ,(lambda (buf)
        (if (not enable-recursive-minibuffers)
            (progn (display-buffer buf)
                   (setq other-window-scroll-buffer buf))
          (view-buffer buf (lambda (_) (exit-recursive-edit)))
          (recursive-edit))
        ;; Return nil to ask about BUF again.
        nil)
     ,(purecopy "view this buffer"))
    (?\C-f
     ,(lambda (buf)
        (funcall save-some-buffers--switch-window-callback buf)
        (setq quit-flag t))
     ,(purecopy "view this buffer and quit"))
    (?d ,(lambda (buf)
           (if (null (buffer-file-name buf))
               (message "Not applicable: no file")
             (require 'diff)            ;for diff-no-select.
             (let ((diffbuf (diff-no-select (buffer-file-name buf) buf
                                            nil 'noasync)))
               (if (not enable-recursive-minibuffers)
                   (progn (display-buffer diffbuf)
                          (setq other-window-scroll-buffer diffbuf))
                 (view-buffer diffbuf (lambda (_) (exit-recursive-edit)))
                 (recursive-edit))))
           ;; Return nil to ask about BUF again.
           nil)
	,(purecopy "view changes in this buffer")))
  "ACTION-ALIST argument used in call to `map-y-or-n-p'.")
(put 'save-some-buffers-action-alist 'risky-local-variable t)

(defvar-local buffer-save-without-query nil
  "Non-nil means `save-some-buffers' should save this buffer without asking.")

(defcustom save-some-buffers-default-predicate nil
  "Default predicate for `save-some-buffers'.

This allows you to stop `save-some-buffers' from asking
about certain files that you'd usually rather not save.

This function is called (with no parameters) from the buffer to
be saved.  When the function's symbol has the property
`save-some-buffers-function', the higher-order function is supposed
to return a predicate used to check buffers."
  :group 'auto-save
  ;; FIXME nil should not be a valid option, let alone the default,
  ;; eg so that add-function can be used.
  :type '(choice (const :tag "Default" nil)
                 (function :tag "Only in subdirs of current project"
                           save-some-buffers-root)
                 (function :tag "Custom function"))
  :version "26.1")

(defun save-some-buffers-root ()
  "A predicate to check whether the buffer is under the project root directory.
Can be used as a value of `save-some-buffers-default-predicate'
to save buffers only under the project root or in subdirectories
of the directory that was default during command invocation."
  (let ((root (or (and (featurep 'project) (project-current)
                       (fboundp 'project-root)
                       (project-root (project-current)))
                  default-directory)))
    (lambda () (file-in-directory-p default-directory root))))
(put 'save-some-buffers-root 'save-some-buffers-function t)

(defun save-some-buffers (&optional arg pred)
  "Save some modified file-visiting buffers.  Asks user about each one.
You can answer `y' or SPC to save, `n' or DEL not to save, `C-r'
to look at the buffer in question with `view-buffer' before
deciding, `d' to view the differences using
`diff-buffer-with-file', `!' to save the buffer and all remaining
buffers without any further querying, `.' to save only the
current buffer and skip the remaining ones and `q' or RET to exit
the function without saving any more buffers.  `C-h' displays a
help message describing these options.

This command first saves any buffers where `buffer-save-without-query' is
non-nil, without asking.

Optional argument ARG (interactively, prefix argument) non-nil means save
all with no questions.
Optional second argument PRED determines which buffers are considered:
If PRED is nil, all the file-visiting buffers are considered.
If PRED is t, then certain non-file buffers will also be considered.
If PRED is a function, it is called with no argument in each buffer and
should return non-nil if that buffer should be considered.
PRED defaults to the value of `save-some-buffers-default-predicate'.

See `save-some-buffers-action-alist' if you want to
change the additional actions you can take on files."
  (interactive "P")
  (unless pred
    (setq pred
          ;; Allow `pred' to be a function that returns a predicate
          ;; with lexical bindings in its original environment (bug#46374).
          (if (and (symbolp save-some-buffers-default-predicate)
                   (get save-some-buffers-default-predicate
                        'save-some-buffers-function))
              (funcall save-some-buffers-default-predicate)
            save-some-buffers-default-predicate)))
  (let* ((switched-buffer nil)
         (save-some-buffers--switch-window-callback
          (lambda (buffer)
            (setq switched-buffer buffer)))
         queried autosaved-buffers
	 files-done abbrevs-done)
    (unwind-protect
        (save-window-excursion
          (dolist (buffer (buffer-list))
	    ;; First save any buffers that we're supposed to save
	    ;; unconditionally.  That way the following code won't ask
	    ;; about them.
	    (with-current-buffer buffer
	      (when (and buffer-save-without-query (buffer-modified-p))
	        (push (buffer-name) autosaved-buffers)
	        (save-buffer))))
          ;; Ask about those buffers that merit it,
          ;; and record the number thus saved.
          (setq files-done
	        (map-y-or-n-p
                 (lambda (buffer)
	           ;; Note that killing some buffers may kill others via
	           ;; hooks (e.g. Rmail and its viewing buffer).
	           (and (buffer-live-p buffer)
		        (buffer-modified-p buffer)
                        (not (buffer-base-buffer buffer))
                        (or
                         (buffer-file-name buffer)
                         (with-current-buffer buffer
                           (or (eq buffer-offer-save 'always)
                               (and pred buffer-offer-save
                                    (> (buffer-size) 0)))))
                        (or (not (functionp pred))
                            (with-current-buffer buffer (funcall pred)))
                        (if arg
                            t
                          (setq queried t)
                          (if (buffer-file-name buffer)
                              (if (or
                                   (equal (buffer-name buffer)
                                          (file-name-nondirectory
                                           (buffer-file-name buffer)))
                                   (string-match
                                    (concat "\\<"
                                            (regexp-quote
                                             (file-name-nondirectory
                                              (buffer-file-name buffer)))
                                            "<[^>]*>\\'")
                                    (buffer-name buffer)))
                                  ;; The buffer name is similar to the
                                  ;; file name.
                                  (format "Save file %s? "
                                          (buffer-file-name buffer))
                                ;; The buffer and file names are
                                ;; dissimilar; display both.
                                (format "Save file %s (buffer %s)? "
                                        (buffer-file-name buffer)
                                        (buffer-name buffer)))
                            ;; No file name
                            (format "Save buffer %s? " (buffer-name buffer))))))
                 (lambda (buffer)
                   (with-current-buffer buffer
                     (save-buffer)))
                 (buffer-list)
	         '("buffer" "buffers" "save")
	         save-some-buffers-action-alist))
          ;; Maybe to save abbrevs, and record whether
          ;; we either saved them or asked to.
          (and save-abbrevs abbrevs-changed
	       (progn
	         (if (or arg
		         (eq save-abbrevs 'silently)
		         (y-or-n-p (format "Save abbrevs in %s? "
                                           abbrev-file-name)))
		     (write-abbrev-file nil))
	         ;; Don't keep bothering user if he says no.
	         (setq abbrevs-changed nil)
	         (setq abbrevs-done t)))
          (or queried (> files-done 0) abbrevs-done
	      (cond
	       ((null autosaved-buffers)
                (when (called-interactively-p 'any)
                  (files--message "(No files need saving)")))
	       ((= (length autosaved-buffers) 1)
	        (files--message "(Saved %s)" (car autosaved-buffers)))
	       (t
	        (files--message
                 "(Saved %d files: %s)" (length autosaved-buffers)
                 (mapconcat 'identity autosaved-buffers ", "))))))
      (when switched-buffer
        (pop-to-buffer-same-window switched-buffer)))))

(defun clear-visited-file-modtime ()
  "Clear out records of last mod time of visited file.
Next attempt to save will not complain of a discrepancy."
  (set-visited-file-modtime 0))

(defun not-modified (&optional arg)
  "Mark current buffer as unmodified, not needing to be saved.
With prefix ARG, mark buffer as modified, so \\[save-buffer] will save.

It is not a good idea to use this function in Lisp programs, because it
prints a message in the minibuffer.  Instead, use `set-buffer-modified-p'."
  (declare (interactive-only set-buffer-modified-p))
  (interactive "P")
  (files--message (if arg "Modification-flag set"
                    "Modification-flag cleared"))
  (set-buffer-modified-p arg))

(defun toggle-read-only (&optional arg interactive)
  "Change whether this buffer is read-only."
  (declare (obsolete read-only-mode "24.3"))
  (interactive (list current-prefix-arg t))
  (if interactive
      (call-interactively 'read-only-mode)
    (read-only-mode (or arg 'toggle))))

(defun insert-file (filename)
  "Insert contents of file FILENAME into buffer after point.
Set mark after the inserted text.

This function is meant for the user to run interactively.
Don't call it from programs!  Use `insert-file-contents' instead.
\(Its calling sequence is different; see its documentation)."
  (declare (interactive-only insert-file-contents))
  (interactive "*fInsert file: ")
  (insert-file-1 filename #'insert-file-contents))

(defun append-to-file (start end filename)
  "Append the contents of the region to the end of file FILENAME.
When called from a function, expects three arguments,
START, END and FILENAME.  START and END are normally buffer positions
specifying the part of the buffer to write.
If START is nil, that means to use the entire buffer contents.
If START is a string, then output that string to the file
instead of any buffer contents; END is ignored.

This does character code conversion and applies annotations
like `write-region' does."
  (interactive "r\nFAppend to file: ")
  (prog1 (write-region start end filename t)
    (when save-silently (message nil))))

(defun file-newest-backup (filename)
  "Return most recent backup file for FILENAME or nil if no backups exist."
  (car (file-backup-file-names filename)))

(defun file-backup-file-names (filename)
  "Return a list of backup files for FILENAME.
The list will be sorted by modification time so that the most
recent files are first."
  ;; `make-backup-file-name' will get us the right directory for
  ;; ordinary or numeric backups.  It might create a directory for
  ;; backups as a side-effect, according to `backup-directory-alist'.
  (let* ((filename (file-name-sans-versions
		    (make-backup-file-name (expand-file-name filename))))
         (dir (file-name-directory filename)))
    (sort
     (seq-filter
      (lambda (candidate)
        (and (backup-file-name-p candidate)
             (string= (file-name-sans-versions candidate) filename)))
      (mapcar
       (lambda (file)
         (concat dir file))
       (file-name-all-completions (file-name-nondirectory filename) dir)))
     #'file-newer-than-file-p)))

(defun rename-uniquely ()
  "Rename current buffer to a similar name not already taken.
This function is useful for creating multiple shell process buffers
or multiple mail buffers, etc.

Note that some commands, in particular those based on `compilation-mode'
\(`compile', `grep', etc.) will reuse the current buffer if it has the
appropriate mode even if it has been renamed.  So as well as renaming
the buffer, you also need to switch buffers before running another
instance of such commands."
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

(defun files--ensure-directory (dir)
  "Make directory DIR if it is not already a directory.  Return nil."
  (condition-case err
      (make-directory-internal dir)
    (error
     (unless (file-directory-p dir)
       (signal (car err) (cdr err))))))

(defun make-directory (dir &optional parents)
  "Create the directory DIR and optionally any nonexistent parent dirs.
If DIR already exists as a directory, signal an error, unless
PARENTS is non-nil.

Interactively, the default choice of directory to create is the
current buffer's default directory.  That is useful when you have
visited a file in a nonexistent directory.

Noninteractively, the second (optional) argument PARENTS, if
non-nil, says whether to create parent directories that don't
exist.  Interactively, this happens by default.

If creating the directory or directories fail, an error will be
raised."
  (interactive
   (list (read-file-name "Make directory: " default-directory default-directory
			 nil nil)
	 t))
  ;; If default-directory is a remote directory,
  ;; make sure we find its make-directory handler.
  (setq dir (expand-file-name dir))
  (let ((handler (find-file-name-handler dir 'make-directory)))
    (if handler
	(funcall handler 'make-directory dir parents)
      (if (not parents)
	  (make-directory-internal dir)
	(let ((dir (directory-file-name (expand-file-name dir)))
	      create-list parent)
	  (while (progn
		   (setq parent (directory-file-name
				 (file-name-directory dir)))
		   (condition-case ()
		       (files--ensure-directory dir)
		     (file-missing
		      ;; Do not loop if root does not exist (Bug#2309).
		      (not (string= dir parent)))))
	    (setq create-list (cons dir create-list)
		  dir parent))
	  (dolist (dir create-list)
            (files--ensure-directory dir)))))))

(defun make-empty-file (filename &optional parents)
  "Create an empty file FILENAME.
Optional arg PARENTS, if non-nil then creates parent dirs as needed.

If called interactively, then PARENTS is non-nil."
  (interactive
   (let ((filename (read-file-name "Create empty file: ")))
     (list filename t)))
  (when (and (file-exists-p filename) (null parents))
    (signal 'file-already-exists `("File exists" ,filename)))
  (let ((paren-dir (file-name-directory filename)))
    (when (and paren-dir (not (file-exists-p paren-dir)))
      (make-directory paren-dir parents)))
  (write-region "" nil filename nil 0))

(defconst directory-files-no-dot-files-regexp
  "[^.]\\|\\.\\.\\."
  "Regexp matching any file name except \".\" and \"..\".
More precisely, it matches parts of any nonempty string except those two.
It is useful as the regexp argument to `directory-files' and
`directory-files-and-attributes'.")

(defun files--force (no-such fn &rest args)
  "Use NO-SUCH to affect behavior of function FN applied to list ARGS.
This acts like (apply FN ARGS) except it returns NO-SUCH if it is
non-nil and if FN fails due to a missing file or directory."
  (condition-case err
      (apply fn args)
    (file-missing (or no-such (signal (car err) (cdr err))))))

(defun delete-directory (directory &optional recursive trash)
  "Delete the directory named DIRECTORY.  Does not follow symlinks.
If RECURSIVE is non-nil, delete files in DIRECTORY as well, with
no error if something else is simultaneously deleting them.
TRASH non-nil means to trash the directory instead, provided
`delete-by-moving-to-trash' is non-nil.

When called interactively, TRASH is nil if and only if a prefix
argument is given, and a further prompt asks the user for
RECURSIVE if DIRECTORY is nonempty."
  (interactive
   (let* ((trashing (and delete-by-moving-to-trash
			 (null current-prefix-arg)))
	  (dir (expand-file-name
		(read-directory-name
		 (if trashing
		     "Move directory to trash: "
		   "Delete directory: ")
		 default-directory default-directory nil nil))))
     (list dir
	   (if (directory-files	dir nil directory-files-no-dot-files-regexp)
	       (y-or-n-p
		(format-message "Directory `%s' is not empty, really %s? "
                                dir (if trashing "trash" "delete")))
	     nil)
	   (null current-prefix-arg))))
  ;; If default-directory is a remote directory, make sure we find its
  ;; delete-directory handler.
  (setq directory (directory-file-name (expand-file-name directory)))
  (let ((handler (find-file-name-handler directory 'delete-directory)))
    (cond
     (handler
      (funcall handler 'delete-directory directory recursive trash))
     ((and delete-by-moving-to-trash trash)
      ;; Move non-empty dir to trash only if recursive deletion was
      ;; requested.  This mimics the non-`delete-by-moving-to-trash'
      ;; case, where the operation fails in delete-directory-internal.
      ;; As `move-file-to-trash' trashes directories (empty or
      ;; otherwise) as a unit, we do not need to recurse here.
      (if (not (or recursive (directory-empty-p directory)))
	  (error "Directory is not empty, not moving to trash")
	(move-file-to-trash directory)))
     ;; Otherwise, call ourselves recursively if needed.
     (t
      (when (or (not recursive) (file-symlink-p directory)
		(let* ((files
			(files--force t #'directory-files directory 'full
				      directory-files-no-dot-files-regexp))
		       (directory-exists (listp files)))
		  (when directory-exists
		    (mapc (lambda (file)
			    ;; This test is equivalent to but more efficient
			    ;; than (and (file-directory-p fn)
			    ;;		 (not (file-symlink-p fn))).
			    (if (eq t (car (file-attributes file)))
				(delete-directory file recursive)
			      (files--force t #'delete-file file)))
			  files))
		  directory-exists))
	(files--force recursive #'delete-directory-internal directory))))))

(defun file-equal-p (file1 file2)
  "Return non-nil if files FILE1 and FILE2 name the same file.
If FILE1 or FILE2 does not exist, the return value is unspecified."
  (let ((handler (or (find-file-name-handler file1 'file-equal-p)
                     (find-file-name-handler file2 'file-equal-p))))
    (if handler
        (funcall handler 'file-equal-p file1 file2)
      (let (f1-attr f2-attr)
        (and (setq f1-attr (file-attributes (file-truename file1)))
	     (setq f2-attr (file-attributes (file-truename file2)))
	     (equal f1-attr f2-attr))))))

(defun file-in-directory-p (file dir)
  "Return non-nil if FILE is in DIR or a subdirectory of DIR.
A directory is considered to be \"in\" itself.
Return nil if DIR is not an existing directory."
  (let ((handler (or (find-file-name-handler file 'file-in-directory-p)
                     (find-file-name-handler dir  'file-in-directory-p))))
    (if handler
        (funcall handler 'file-in-directory-p file dir)
      (when (file-directory-p dir) ; DIR must exist.
	(setq file (file-truename file)
	      dir  (file-truename dir))
	(let ((ls1 (split-string file "/" t))
	      (ls2 (split-string dir  "/" t))
	      (root
               (cond
                ;; A UNC on Windows systems, or a "super-root" on Apollo.
                ((string-match "\\`//" file) "//")
                ((string-match "\\`/" file) "/")
                (t "")))
	      (mismatch nil))
	  (while (and ls1 ls2 (not mismatch))
	    (if (string-equal (car ls1) (car ls2))
		(setq root (concat root (car ls1) "/"))
	      (setq mismatch t))
	    (setq ls1 (cdr ls1)
		  ls2 (cdr ls2)))
	  (unless mismatch
	    (file-equal-p root dir)))))))

(defun copy-directory (directory newname &optional keep-time parents copy-contents)
  "Copy DIRECTORY to NEWNAME.  Both args must be strings.
This function always sets the file modes of the output files to match
the corresponding input file.

The third arg KEEP-TIME non-nil means give the output files the same
last-modified time as the old ones.  (This works on only some systems.)

A prefix arg makes KEEP-TIME non-nil.

Noninteractively, the PARENTS argument says whether to create
parent directories if they don't exist.  Interactively, this
happens by default.

If DIRECTORY is a symlink and `copy-directory-create-symlink' is
non-nil, create a symlink with the same target as DIRECTORY.

If NEWNAME is a directory name, copy DIRECTORY as a subdirectory
there.  However, if called from Lisp with a non-nil optional
argument COPY-CONTENTS, copy the contents of DIRECTORY directly
into NEWNAME instead."
  (interactive
   (let ((dir (read-directory-name
	       "Copy directory: " default-directory default-directory t nil)))
     (list dir
	   (read-directory-name
	    (format "Copy directory %s to: " dir)
	    default-directory default-directory nil nil)
	   current-prefix-arg t nil)))
  (when (file-in-directory-p newname directory)
    (error "Cannot copy `%s' into its subdirectory `%s'"
           directory newname))
  ;; If default-directory is a remote directory, make sure we find its
  ;; copy-directory handler.
  (let ((handler (or (find-file-name-handler directory 'copy-directory)
		     (find-file-name-handler newname 'copy-directory)))
	(follow parents))
    (if handler
	(funcall handler 'copy-directory directory
                 newname keep-time parents copy-contents)

      ;; Compute target name.
      (setq directory (directory-file-name (expand-file-name directory))
	    newname (expand-file-name newname))

      ;; If DIRECTORY is a symlink, create a symlink with the same target.
      (if (and (file-symlink-p directory)
               copy-directory-create-symlink)
          (let ((target (car (file-attributes directory))))
	    (if (directory-name-p newname)
		(make-symbolic-link target
				    (concat newname
					    (file-name-nondirectory directory))
				    t)
	      (make-symbolic-link target newname t)))
        ;; Else proceed to copy as a regular directory
        (cond ((not (directory-name-p newname))
	       ;; If NEWNAME is not a directory name, create it;
	       ;; that is where we will copy the files of DIRECTORY.
	       (make-directory newname parents))
	      ;; NEWNAME is a directory name.  If COPY-CONTENTS is non-nil,
	      ;; create NEWNAME if it is not already a directory;
	      ;; otherwise, create NEWNAME/[DIRECTORY-BASENAME].
	      ((if copy-contents
		   (or parents (not (file-directory-p newname)))
	         (setq newname (concat newname
				       (file-name-nondirectory directory))))
	       (make-directory (directory-file-name newname) parents))
	      (t (setq follow t)))

        ;; Copy recursively.
        (dolist (file
	         ;; We do not want to copy "." and "..".
	         (directory-files directory 'full
				  directory-files-no-dot-files-regexp))
	  (let ((target (concat (file-name-as-directory newname)
			        (file-name-nondirectory file)))
	        (filetype (car (file-attributes file))))
	    (cond
	     ((eq filetype t)           ; Directory but not a symlink.
	      (copy-directory file target keep-time parents t))
	     ((stringp filetype)        ; Symbolic link
	      (make-symbolic-link filetype target t))
	     ((copy-file file target t keep-time)))))

        ;; Set directory attributes.
        (let ((modes (file-modes directory))
	      (times (and keep-time (file-attribute-modification-time
				     (file-attributes directory))))
	      (follow-flag (unless follow 'nofollow)))
	  (if modes (set-file-modes newname modes follow-flag))
	  (if times (set-file-times newname times follow-flag)))))))


;; At time of writing, only info uses this.
(defun prune-directory-list (dirs &optional keep reject)
  "Return a copy of DIRS with all non-existent directories removed.
The optional argument KEEP is a list of directories to retain even if
they don't exist, and REJECT is a list of directories to remove from
DIRS, even if they exist; REJECT takes precedence over KEEP.

Note that membership in REJECT and KEEP is checked using simple string
comparison."
  (apply #'nconc
	 (mapcar (lambda (dir)
		   (and (not (member dir reject))
			(or (member dir keep) (file-directory-p dir))
			(list dir)))
		 dirs)))


(put 'revert-buffer-function 'permanent-local t)
(defvar revert-buffer-function #'revert-buffer--default
  "Function to use to revert this buffer.
The function receives two arguments IGNORE-AUTO and NOCONFIRM,
which are the arguments that `revert-buffer' received.
It also has access to the `preserve-modes' argument of `revert-buffer'
via the `revert-buffer-preserve-modes' dynamic variable.

For historical reasons, a value of nil means to use the default function.
This should not be relied upon.")

(put 'revert-buffer-insert-file-contents-function 'permanent-local t)
(defvar revert-buffer-insert-file-contents-function
  #'revert-buffer-insert-file-contents--default-function
  "Function to use to insert contents when reverting this buffer.
The function receives two arguments: the first the nominal file name to use;
the second is t if reading the auto-save file.

The function is responsible for updating (or preserving) point.

For historical reasons, a value of nil means to use the default function.
This should not be relied upon.")

(defun buffer-stale--default-function (&optional _noconfirm)
  "Default function to use for `buffer-stale-function'.
This function ignores its argument.
This returns non-nil if the current buffer is visiting a readable file
whose modification time does not match that of the buffer.

This function handles only buffers that are visiting files.
Non-file buffers need a custom function."
  (and buffer-file-name
       (file-readable-p buffer-file-name)
       (not (buffer-modified-p (current-buffer)))
       (not (verify-visited-file-modtime (current-buffer)))))

(defvar buffer-stale-function #'buffer-stale--default-function
  "Function to check whether a buffer needs reverting.
This should be a function with one optional argument NOCONFIRM.
Auto Revert Mode passes t for NOCONFIRM.  The function should return
non-nil if the buffer should be reverted.  A return value of
`fast' means that the need for reverting was not checked, but
that reverting the buffer is fast.  The buffer is current when
this function is called.

The idea behind the NOCONFIRM argument is that it should be
non-nil if the buffer is going to be reverted without asking the
user.  In such situations, one has to be careful with potentially
time consuming operations.

For historical reasons, a value of nil means to use the default function.
This should not be relied upon.

For more information on how this variable is used by Auto Revert mode,
see Info node `(emacs)Supporting additional buffers'.")

(defvar-local buffer-auto-revert-by-notification nil
  "Whether a buffer can rely on notification in Auto-Revert mode.
If non-nil, monitoring changes to the directory of the current
buffer is sufficient for knowing when that buffer needs to be
updated in Auto Revert Mode.  Such notification does not include
changes to files in that directory, only to the directory itself.

This variable applies only to buffers where `buffer-file-name' is
nil; other buffers are tracked by their files.")

(defvar before-revert-hook nil
  "Normal hook for `revert-buffer' to run before reverting.
The function `revert-buffer--default' runs this.
A customized `revert-buffer-function' need not run this hook.")

(defvar after-revert-hook nil
  "Normal hook for `revert-buffer' to run after reverting.
Note that the hook value that it runs is the value that was in effect
before reverting; that makes a difference if you have buffer-local
hook functions.

The function `revert-buffer--default' runs this.
A customized `revert-buffer-function' need not run this hook.")

(defvar revert-buffer-in-progress-p nil
  "Non-nil if a `revert-buffer' operation is in progress, nil otherwise.")

(defvar revert-buffer-internal-hook)

;; `revert-buffer-function' was defined long ago to be a function of only
;; 2 arguments, so we have to use a dynbind variable to pass the
;; `preserve-modes' argument of `revert-buffer'.
(defvar revert-buffer-preserve-modes)

(defun revert-buffer (&optional ignore-auto noconfirm preserve-modes)
  "Replace current buffer text with the text of the visited file on disk.
This undoes all changes since the file was visited or saved.
With a prefix argument, offer to revert from latest auto-save file, if
that is more recent than the visited file.

This command also implements an interface for special buffers
that contain text that doesn't come from a file, but reflects
some other data instead (e.g. Dired buffers, `buffer-list'
buffers).  This is done via the variable `revert-buffer-function'.
In these cases, it should reconstruct the buffer contents from the
appropriate data.

When called from Lisp, the first argument is IGNORE-AUTO; offer to
revert from the auto-save file only when this is nil.  Note that the
sense of this argument is the reverse of the prefix argument, for the
sake of backward compatibility.  IGNORE-AUTO is optional, defaulting
to nil.

Optional second argument NOCONFIRM means don't ask for confirmation
at all.  (The variable `revert-without-query' offers another way to
revert buffers without querying for confirmation.)

Optional third argument PRESERVE-MODES non-nil means don't alter
the files modes.  Normally we reinitialize them using `normal-mode'.

This function binds `revert-buffer-in-progress-p' non-nil while it operates.

This function calls the function that `revert-buffer-function' specifies
to do the work, with arguments IGNORE-AUTO and NOCONFIRM.
The default function runs the hooks `before-revert-hook' and
`after-revert-hook'

Reverting a buffer will try to preserve markers in the buffer,
but it cannot always preserve all of them.  For better results,
use `revert-buffer-with-fine-grain', which tries harder to
preserve markers and overlays, at the price of being slower."
  ;; I admit it's odd to reverse the sense of the prefix argument, but
  ;; there is a lot of code out there that assumes that the first
  ;; argument should be t to avoid consulting the auto-save file, and
  ;; there's no straightforward way to encourage authors to notice a
  ;; reversal of the argument sense.  So I'm just changing the user
  ;; interface, but leaving the programmatic interface the same.
  (interactive (list (not current-prefix-arg)))
  (let ((revert-buffer-in-progress-p t)
        (revert-buffer-preserve-modes preserve-modes))
    (funcall (or revert-buffer-function #'revert-buffer--default)
             ignore-auto noconfirm)))

(defun revert-buffer--default (ignore-auto noconfirm)
  "Default function for `revert-buffer'.
The arguments IGNORE-AUTO and NOCONFIRM are as described for `revert-buffer'.
Runs the hooks `before-revert-hook' and `after-revert-hook' at the
start and end.

Calls `revert-buffer-insert-file-contents-function' to reread the
contents of the visited file, with two arguments: the first is the file
name, the second is non-nil if reading an auto-save file.

This function handles only buffers that are visiting files.
Non-file buffers need a custom function."
  (with-current-buffer (or (buffer-base-buffer (current-buffer))
                           (current-buffer))
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
                      (catch 'found
                        (dolist (regexp revert-without-query)
                          (when (string-match regexp file-name)
                            (throw 'found t)))))
                 (yes-or-no-p
                  (format (if (buffer-modified-p)
                              "Discard edits and reread from %s? "
                            "Revert buffer from file %s? ")
                          file-name)))
             (run-hooks 'before-revert-hook)
             ;; If file was backed up but has changed since,
             ;; we should make another backup.
             (and (not auto-save-p)
                  (not (verify-visited-file-modtime (current-buffer)))
                  (setq buffer-backed-up nil))
             ;; Effectively copy the after-revert-hook status,
             ;; since after-find-file will clobber it.
             (let ((global-hook (default-value 'after-revert-hook))
                   (local-hook (when (local-variable-p 'after-revert-hook)
                                 after-revert-hook))
                   (inhibit-read-only t))
               ;; FIXME: Throw away undo-log when preserve-modes is nil?
               (funcall
                (or revert-buffer-insert-file-contents-function
                    #'revert-buffer-insert-file-contents--default-function)
                file-name auto-save-p)
               ;; Recompute the truename in case changes in symlinks
               ;; have changed the truename.
               (setq buffer-file-truename
                     (abbreviate-file-name (file-truename buffer-file-name)))
               (after-find-file nil nil t nil revert-buffer-preserve-modes)
               ;; Run after-revert-hook as it was before we reverted.
               (setq-default revert-buffer-internal-hook global-hook)
               (if local-hook
                   (setq-local revert-buffer-internal-hook
                        local-hook)
                 (kill-local-variable 'revert-buffer-internal-hook))
               (run-hooks 'revert-buffer-internal-hook))
             t)))))

(defun revert-buffer-insert-file-contents--default-function (file-name auto-save-p)
  "Default function for `revert-buffer-insert-file-contents-function'.
The function `revert-buffer--default' calls this.
FILE-NAME is the name of the file.  AUTO-SAVE-P is non-nil if this is
an auto-save file."
  (cond
   ((not (file-exists-p file-name))
    (error (if buffer-file-number
               "File %s no longer exists!"
             "Cannot revert nonexistent file %s")
           file-name))
   ((not (file-readable-p file-name))
    (error (if buffer-file-number
               "File %s no longer readable!"
             "Cannot revert unreadable file %s")
           file-name))
   (t
    (widen)
    (let ((coding-system-for-read
           ;; Auto-saved file should be read by Emacs's
           ;; internal coding.
           (if auto-save-p 'auto-save-coding
             (or coding-system-for-read
                 (and
                  buffer-file-coding-system-explicit
                  (car buffer-file-coding-system-explicit))))))
      (if (and (not enable-multibyte-characters)
               coding-system-for-read
               (not (memq (coding-system-base
                           coding-system-for-read)
                          '(no-conversion raw-text))))
          ;; As a coding system suitable for multibyte
          ;; buffer is specified, make the current
          ;; buffer multibyte.
          (set-buffer-multibyte t))

      ;; This force after-insert-file-set-coding
      ;; (called from insert-file-contents) to set
      ;; buffer-file-coding-system to a proper value.
      (kill-local-variable 'buffer-file-coding-system)

      ;; Note that this preserves point in an intelligent way.
      (if revert-buffer-preserve-modes
          (let ((buffer-file-format buffer-file-format))
            (insert-file-contents file-name (not auto-save-p)
                                  nil nil t))
        (insert-file-contents file-name (not auto-save-p)
                              nil nil t))))))

(defvar revert-buffer-with-fine-grain-max-seconds 2.0
  "Maximum time that `revert-buffer-with-fine-grain' should use.
The command tries to preserve markers, properties and overlays.
If the operation takes more than this time, a single
delete+insert is performed.  Actually, this value is passed as
the MAX-SECS argument to the function `replace-buffer-contents',
so it is not ensured that the whole execution won't take longer.
See `replace-buffer-contents' for more details.")

(defun revert-buffer-insert-file-contents-delicately (file-name _auto-save-p)
  "Optional function for `revert-buffer-insert-file-contents-function'.
The function `revert-buffer-with-fine-grain' uses this function by binding
`revert-buffer-insert-file-contents-function' to it.

As with `revert-buffer-insert-file-contents--default-function', FILE-NAME is
the name of the file and AUTO-SAVE-P is non-nil if this is an auto-save file.
Since calling `replace-buffer-contents' can take a long time, depending of
the number of changes made to the buffer, it uses the value of the variable
`revert-buffer-with-fine-grain-max-seconds' as a maximum time to try delicately
reverting the buffer.  If it fails, it does a delete+insert.  For more details,
see `replace-buffer-contents'."
  (cond
   ((not (file-exists-p file-name))
    (error (if buffer-file-number
               "File %s no longer exists"
             "Cannot revert nonexistent file %s")
           file-name))
   ((not (file-readable-p file-name))
    (error (if buffer-file-number
               "File %s no longer readable"
             "Cannot revert unreadable file %s")
           file-name))
   (t
    (let* ((buf (current-buffer)) ; current-buffer is the buffer to revert.
           (success
            (save-excursion
              (save-restriction
                (widen)
                (with-temp-buffer
                  (insert-file-contents file-name)
                  (let ((temp-buf (current-buffer)))
                    (set-buffer buf)
                    (let ((buffer-file-name nil))
                      (replace-buffer-contents
                       temp-buf
                       revert-buffer-with-fine-grain-max-seconds))))))))
      ;; See comments in revert-buffer-with-fine-grain for an explanation.
      (defun revert-buffer-with-fine-grain-success-p ()
        success))
    (set-buffer-modified-p nil)
    (set-visited-file-modtime))))

(defun revert-buffer-with-fine-grain (&optional ignore-auto noconfirm)
  "Revert buffer preserving markers, overlays, etc.
This command is an alternative to `revert-buffer' because it tries to be as
non-destructive as possible, preserving markers, properties and overlays.
Binds `revert-buffer-insert-file-contents-function' to the function
`revert-buffer-insert-file-contents-delicately'.

With a prefix argument, offer to revert from latest auto-save file.  For more
details on the arguments, see `revert-buffer'."
  ;; See revert-buffer for an explanation of this.
  (interactive (list (not current-prefix-arg)))
  ;; Simply bind revert-buffer-insert-file-contents-function to the specialized
  ;; function, and call revert-buffer.
  (let ((revert-buffer-insert-file-contents-function
         #'revert-buffer-insert-file-contents-delicately))
    (revert-buffer ignore-auto noconfirm t)
    ;; This closure is defined in revert-buffer-insert-file-contents-function.
    ;; It is needed because revert-buffer--default always returns t after
    ;; reverting, and it might be needed to report the success/failure of
    ;; reverting delicately.
    (when (fboundp 'revert-buffer-with-fine-grain-success-p)
      (prog1
          (revert-buffer-with-fine-grain-success-p)
        (fmakunbound 'revert-buffer-with-fine-grain-success-p)))))

(defcustom revert-buffer-quick-short-answers nil
  "How much confirmation to be done by the `revert-buffer-quick' command.
If non-nil, use `y-or-n-p' instead of `yes-or-no-p'."
  :version "28.1"
  :type 'boolean)

(defun revert-buffer-quick (&optional auto-save)
  "Like `revert-buffer', but asks for less confirmation.
If the current buffer is visiting a file, and the buffer is not
modified, no confirmation is required.

This command heeds the `revert-buffer-quick-short-answers' user option.

If AUTO-SAVE (the prefix argument), offer to revert from latest
auto-save file, if that is more recent than the visited file."
  (interactive "P")
  (cond
   ;; If we've visiting a file, and we have no changes, don't ask for
   ;; confirmation.
   ((and buffer-file-name
         (not (buffer-modified-p)))
    (revert-buffer (not auto-save) t)
    (message "Reverted buffer"))
   ;; Heed `revert-buffer-quick-short-answers'.
   (revert-buffer-quick-short-answers
    (let ((use-short-answers t))
      (revert-buffer (not auto-save))))
   ;; Call `revert-buffer' normally.
   (t
    (revert-buffer (not auto-save)))))


(defun recover-this-file ()
  "Recover the visited file--get contents from its last auto-save file."
  (interactive)
  (or buffer-file-name
      (user-error "This buffer is not visiting a file"))
  (recover-file buffer-file-name))

(defun recover-file (file)
  "Visit file FILE, but get contents from its last auto-save file."
  ;; Actually putting the file name in the minibuffer should be used
  ;; only rarely.
  ;; Not just because users often use the default.
  (interactive "FRecover file: ")
  (setq file (expand-file-name file))
  (if (auto-save-file-name-p (file-name-nondirectory file))
      (user-error "%s is an auto-save file" (abbreviate-file-name file)))
  (let ((file-name (let ((buffer-file-name file))
		     (make-auto-save-file-name))))
    (cond ((and (file-exists-p file)
                (not (file-exists-p file-name)))
           (error "Auto save file %s does not exist"
                  (abbreviate-file-name file-name)))
          ((if (file-exists-p file)
	       (not (file-newer-than-file-p file-name file))
	     (not (file-exists-p file-name)))
	   (user-error "Auto-save file %s not current"
                       (abbreviate-file-name file-name)))
	  ((with-temp-buffer-window
	    "*Directory*" nil
	    #'(lambda (window _value)
		(with-selected-window window
		  (unwind-protect
		      (yes-or-no-p (format "Recover auto save file %s? " file-name))
		    (when (window-live-p window)
		      (quit-restore-window window 'kill)))))
	    (with-current-buffer standard-output
	      (let ((switches dired-listing-switches))
		(if (file-symlink-p file)
		    (setq switches (concat switches " -L")))
		;; Use insert-directory-safely, not insert-directory,
		;; because these files might not exist.  In particular,
		;; FILE might not exist if the auto-save file was for
		;; a buffer that didn't visit a file, such as "*mail*".
		;; The code in v20.x called `ls' directly, so we need
		;; to emulate what `ls' did in that case.
		(insert-directory-safely file switches)
		(insert-directory-safely file-name switches))))
	   (switch-to-buffer (find-file-noselect file t))
	   (let ((inhibit-read-only t)
		 ;; Keep the current buffer-file-coding-system.
		 (coding-system buffer-file-coding-system)
		 ;; Auto-saved file should be read with special coding.
		 (coding-system-for-read 'auto-save-coding))
	     (erase-buffer)
	     (insert-file-contents file-name nil)
	     (set-buffer-file-coding-system coding-system)
             (set-buffer-auto-saved))
	   (after-find-file nil nil t))
	  (t (user-error "Recover-file canceled")))))

(defvar dired-mode-hook)

(defun recover-session ()
  "Recover auto save files from a previous Emacs session.
This command first displays a Dired buffer showing you the
previous sessions that you could recover from.
To choose one, move point to the proper line and then type C-c C-c.
Then you'll be asked about a number of files to recover."
  (interactive)
  (if (null auto-save-list-file-prefix)
      (error "You set `auto-save-list-file-prefix' to disable making session files"))
  (let ((dir (file-name-directory auto-save-list-file-prefix))
        (nd (file-name-nondirectory auto-save-list-file-prefix)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (unless (directory-files dir nil
                             (if (string= "" nd)
                                 directory-files-no-dot-files-regexp
                               (concat "\\`" (regexp-quote nd)))
			     t)
      (error "No previous sessions to recover")))
  (require 'dired)
  (let ((ls-lisp-support-shell-wildcards t)
        ;; Ensure that we don't omit the session files as the user may
        ;; have (as suggested by the manual) `dired-omit-mode' in the
        ;; hook.
        (dired-mode-hook (delete 'dired-omit-mode dired-mode-hook)))
    (dired (concat auto-save-list-file-prefix "*")
	   (concat dired-listing-switches " -t")))
  (use-local-map (nconc (make-sparse-keymap) (current-local-map)))
  (define-key (current-local-map) "\C-c\C-c" 'recover-session-finish)
  (save-excursion
    (goto-char (point-min))
    (or (looking-at " Move to the session you want to recover,")
	(let ((inhibit-read-only t))
	  ;; Each line starts with a space
	  ;; so that Font Lock mode won't highlight the first character.
	  (insert " To recover a session, move to it and type C-c C-c.\n"
		  (substitute-command-keys
		   " To delete a session file, type \
\\[dired-flag-file-deletion] on its line to flag
 the file for deletion, then \\[dired-do-flagged-delete] to \
delete flagged files.\n\n"))))))

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
	(with-current-buffer buffer
	  ;; Read in the auto-save-list file.
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
		    ;; If there is no auto-save file name, the
		    ;; auto-save-list file is probably corrupted.
		    (unless (eolp)
		      (setq autofile
			    (buffer-substring-no-properties
			     (point)
			     (line-end-position)))
		      (setq thisfile
			    (expand-file-name
			     (substring
			      (file-name-nondirectory autofile)
			      1 -1)
			     (file-name-directory autofile))))
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
	      (if (and autofile (file-exists-p autofile))
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


(defun kill-buffer-ask (buffer)
  "Kill BUFFER if confirmed."
  (when (yes-or-no-p (format "Buffer %s %s.  Kill? "
			     (buffer-name buffer)
			     (if (buffer-modified-p buffer)
				 "HAS BEEN EDITED" "is unmodified")))
    (kill-buffer buffer)))

(defun kill-some-buffers (&optional list)
  "Kill some buffers.  Asks the user whether to kill each one of them.
Non-interactively, if optional argument LIST is non-nil, it
specifies the list of buffers to kill, asking for approval for each one."
  (interactive)
  (if (null list)
      (setq list (buffer-list)))
  (while list
    (let* ((buffer (car list))
	   (name (buffer-name buffer)))
      (and name				; Can be nil for an indirect buffer
					; if we killed the base buffer.
	   (not (string-equal name ""))
	   (/= (aref name 0) ?\s)
	   (kill-buffer-ask buffer)))
    (setq list (cdr list))))

(defun kill-matching-buffers (regexp &optional internal-too no-ask)
  "Kill buffers whose name matches the specified REGEXP.
Ignores buffers whose name starts with a space, unless optional
prefix argument INTERNAL-TOO is non-nil.  Asks before killing
each buffer, unless NO-ASK is non-nil."
  (interactive "sKill buffers matching this regular expression: \nP")
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (and name (not (string-equal name ""))
                 (or internal-too (/= (aref name 0) ?\s))
                 (string-match regexp name))
        (funcall (if no-ask 'kill-buffer 'kill-buffer-ask) buffer)))))


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
before calling this function.
See also `auto-save-file-name-p'."
  (if buffer-file-name
      (let ((handler (find-file-name-handler
                      buffer-file-name 'make-auto-save-file-name)))
	(if handler
	    (funcall handler 'make-auto-save-file-name)
          (files--transform-file-name
           buffer-file-name auto-save-file-name-transforms
                                          "#" "#")))
    ;; Deal with buffers that don't have any associated files.  (Mail
    ;; mode tends to create a good number of these.)
    (let ((buffer-name (buffer-name))
	  (limit 0)
	  file-name)
      ;; Restrict the characters used in the file name to those that
      ;; are known to be safe on all filesystems, url-encoding the
      ;; rest.
      ;; We do this on all platforms, because even if we are not
      ;; running on DOS/Windows, the current directory may be on a
      ;; mounted VFAT filesystem, such as a USB memory stick.
      (while (string-match "[^A-Za-z0-9_.~#+-]" buffer-name limit)
	(let* ((character (aref buffer-name (match-beginning 0)))
	       (replacement
                ;; For multibyte characters, this will produce more than
                ;; 2 hex digits, so is not true URL encoding.
                (format "%%%02X" character)))
	  (setq buffer-name (replace-match replacement t t buffer-name))
	  (setq limit (1+ (match-end 0)))))
      ;; Generate the file name.
      (setq file-name
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
	       (if (and (memq system-type '(ms-dos windows-nt cygwin))
			;; Don't modify remote filenames
			(not (file-remote-p fname)))
		   ;; The call to convert-standard-filename is in case
		   ;; buffer-name includes characters not allowed by the
		   ;; DOS/Windows filesystems.  make-temp-file writes to the
		   ;; file it creates, so we must fix the file name _before_
		   ;; make-temp-file is called.
		   (convert-standard-filename fname)
		 fname))
	     nil "#"))
      ;; make-temp-file creates the file,
      ;; but we don't want it to exist until we do an auto-save.
      (condition-case ()
	  (delete-file file-name)
	(file-error nil))
      file-name)))

(defun files--transform-file-name (filename transforms prefix suffix)
  "Transform FILENAME according to TRANSFORMS.
See `auto-save-file-name-transforms' for the format of
TRANSFORMS.  PREFIX is prepended to the non-directory portion of
the resulting file name, and SUFFIX is appended."
  (save-match-data
    (let (result uniq)
      ;; Apply user-specified translations to the file name.
      (while (and transforms (not result))
        (if (string-match (car (car transforms)) filename)
	    (setq result (replace-match (cadr (car transforms)) t nil
				        filename)
		  uniq (car (cddr (car transforms)))))
        (setq transforms (cdr transforms)))
      (when result
        (setq filename
              (cond
               ((memq uniq (secure-hash-algorithms))
                (concat
                 (file-name-directory result)
                 (secure-hash uniq filename)))
               (uniq
                (concat
	         (file-name-directory result)
	         (subst-char-in-string
		  ?/ ?!
		  (string-replace
                   "!" "!!" filename))))
	       (t result))))
      (setq result
	    (if (and (eq system-type 'ms-dos)
		     (not (msdos-long-file-names)))
	        ;; We truncate the file name to DOS 8+3 limits before
	        ;; doing anything else, because the regexp passed to
	        ;; string-match below cannot handle extensions longer
	        ;; than 3 characters, multiple dots, and other
	        ;; atrocities.
	        (let ((fn (dos-8+3-filename
			   (file-name-nondirectory buffer-file-name))))
		  (string-match
		   "\\`\\([^.]+\\)\\(\\.\\(..?\\)?.?\\|\\)\\'"
		   fn)
		  (concat (file-name-directory buffer-file-name)
			  prefix (match-string 1 fn)
			  "." (match-string 3 fn) suffix))
	      (concat (file-name-directory filename)
		      prefix
		      (file-name-nondirectory filename)
		      suffix)))
      ;; Make sure auto-save file names don't contain characters
      ;; invalid for the underlying filesystem.
      (expand-file-name
       (if (and (memq system-type '(ms-dos windows-nt cygwin))
	        ;; Don't modify remote filenames
                (not (file-remote-p result)))
	   (convert-standard-filename result)
         result)))))

(defun make-lock-file-name (filename)
  "Make a lock file name for FILENAME.
By default, this just prepends \".#\" to the non-directory part
of FILENAME, but the transforms in `lock-file-name-transforms'
are done first."
  (let ((handler (find-file-name-handler filename 'make-lock-file-name)))
    (if handler
	(funcall handler 'make-lock-file-name filename)
      (files--transform-file-name filename lock-file-name-transforms ".#" ""))))

(defun auto-save-file-name-p (filename)
  "Return non-nil if FILENAME can be yielded by `make-auto-save-file-name'.
FILENAME should lack slashes.
See also `make-auto-save-file-name'."
  (string-match "\\`#.*#\\'" filename))

(defun wildcard-to-regexp (wildcard)
  "Given a shell file name pattern WILDCARD, return an equivalent regexp.
The generated regexp will match a file name only if the file name
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
					 (setq j (string-search
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
  (purecopy "-CF")
  "Switches for `list-directory' to pass to `ls' for brief listing."
  :type 'string
  :group 'dired)

(defcustom list-directory-verbose-switches
    (purecopy "-l")
  "Switches for `list-directory' to pass to `ls' for verbose listing."
  :type 'string
  :group 'dired)

(defun file-expand-wildcards (pattern &optional full)
  "Expand wildcard pattern PATTERN.
This returns a list of file names that match the pattern.
Files are sorted in `string<' order.

If PATTERN is written as an absolute file name,
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
	   (dirs (if (and dirpart
			  (string-match "[[*?]" (file-local-name dirpart)))
		     (mapcar 'file-name-as-directory
			     (file-expand-wildcards (directory-file-name dirpart)))
		   (list dirpart)))
	   contents)
      (dolist (dir dirs)
	(when (or (null dir)	; Possible if DIRPART is not wild.
		  (file-accessible-directory-p dir))
	  (let ((this-dir-contents
		 ;; Filter out "." and ".."
		 (delq nil
		       (mapcar #'(lambda (name)
				   (unless (string-match "\\`\\.\\.?\\'"
							 (file-name-nondirectory name))
				     name))
			       (directory-files (or dir ".") full
						(wildcard-to-regexp nondir))))))
	    (setq contents
		  (nconc
		   (if (and dir (not full))
		       (mapcar #'(lambda (name) (concat dir name))
			       this-dir-contents)
		     this-dir-contents)
		   contents)))))
      contents)))

;; Let Tramp know that `file-expand-wildcards' does not need an advice.
(provide 'files '(remote-wildcards))

(defun list-directory (dirname &optional verbose)
  "Display a list of files in or matching DIRNAME, a la `ls'.
DIRNAME is globbed by the shell if necessary.
Prefix arg (second arg if noninteractive) means supply -l switch to `ls'.
Actions controlled by variables `list-directory-brief-switches'
and `list-directory-verbose-switches'."
  (interactive (let ((pfx current-prefix-arg))
		 (list (read-directory-name (if pfx "List directory (verbose): "
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
      (with-current-buffer "*Directory*"
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
underlying filesystem.  For Unix and GNU/Linux, each character from the
set [ \\t\\n;<>&|()\\=`\\='\"#$] is quoted with a backslash; for DOS/Windows, all
the parts of the pattern that don't include wildcard characters are
quoted with double quotes.

This function leaves alone existing quote characters (\\ on Unix and \"
on Windows), so PATTERN can use them to quote wildcard characters that
need to be passed verbatim to shell commands."
  (save-match-data
    (cond
     ((memq system-type '(ms-dos windows-nt cygwin))
      ;; DOS/Windows don't allow `"' in file names.  So if the
      ;; argument has quotes, we can safely assume it is already
      ;; quoted by the caller.
      (if (or (string-search "\"" pattern)
	      ;; We quote [&()#$`'] in case their shell is a port of a
	      ;; Unixy shell.  We quote [,=+] because stock DOS and
	      ;; Windows shells require that in some cases, such as
	      ;; passing arguments to batch files that use positional
	      ;; arguments like %1.
	      (not (string-match "[ \t;&()#$`',=+]" pattern)))
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
	(while (string-match "[ \t\n;<>&|()`'\"#$]" pattern beg)
	  (setq pattern
		(concat (substring pattern 0 (match-beginning 0))
			"\\"
			(substring pattern (match-beginning 0)))
		beg (1+ (match-end 0)))))
      pattern))))


(defvar insert-directory-program (purecopy "ls")
  "Absolute or relative name of the `ls' program used by `insert-directory'.")

(defcustom directory-free-space-program (purecopy "df")
  "Program to get the amount of free space on a file system.
We assume the output has the format of `df'.
The value of this variable must be just a command name or file name;
if you want to specify options, use `directory-free-space-args'.

A value of nil disables this feature."
  :type '(choice (string :tag "Program") (const :tag "None" nil))
  :group 'dired)
(make-obsolete-variable 'directory-free-space-program
			"ignored, as Emacs uses `file-system-info' instead"
			"27.1")

(defcustom directory-free-space-args
  (purecopy (if (eq system-type 'darwin) "-k" "-Pk"))
  "Options to use when running `directory-free-space-program'."
  :type 'string
  :group 'dired)
(make-obsolete-variable 'directory-free-space-args
			"ignored, as Emacs uses `file-system-info' instead"
			"27.1")

(defun get-free-disk-space (dir)
  "String describing the amount of free space on DIR's file system.
If DIR's free space cannot be obtained, this function returns nil."
  (save-match-data
    (let ((avail (nth 2 (file-system-info dir))))
      (if avail
          (funcall byte-count-to-string-function avail)))))

;; The following expression replaces `dired-move-to-filename-regexp'.
(defvar directory-listing-before-filename-regexp
  (let* ((l "\\([A-Za-z]\\|[^\0-\177]\\)")
	 (l-or-quote "\\([A-Za-z']\\|[^\0-\177]\\)")
	 ;; In some locales, month abbreviations are as short as 2 letters,
	 ;; and they can be followed by ".".
	 ;; In Breton, a month name  can include a quote character.
	 (month (concat l-or-quote l-or-quote "+\\.?"))
	 (s " ")
	 (yyyy "[0-9][0-9][0-9][0-9]")
	 (dd "[ 0-3][0-9]")
	 (HH:MM "[ 0-2][0-9][:.][0-5][0-9]")
	 (seconds "[0-6][0-9]\\([.,][0-9]+\\)?")
	 (zone "[-+][0-2][0-9][0-5][0-9]")
	 (iso-mm-dd "[01][0-9]-[0-3][0-9]")
	 (iso-time (concat HH:MM "\\(:" seconds "\\( ?" zone "\\)?\\)?"))
	 (iso (concat "\\(\\(" yyyy "-\\)?" iso-mm-dd "[ T]" iso-time
		      "\\|" yyyy "-" iso-mm-dd "\\)"))
	 (western (concat "\\(" month s "+" dd "\\|" dd "\\.?" s month "\\)"
			  s "+"
			  "\\(" HH:MM "\\|" yyyy "\\)"))
	 (western-comma (concat month s "+" dd "," s "+" yyyy))
         ;; This represents the date in strftime(3) format "%e-%b-%Y"
         ;; (aka "%v"), as it is the default for many ls incarnations.
         (DD-MMM-YYYY (concat dd "-" month "-" yyyy s HH:MM))
	 ;; Japanese MS-Windows ls-lisp has one-digit months, and
	 ;; omits the Kanji characters after month and day-of-month.
	 ;; On Mac OS X 10.3, the date format in East Asian locales is
	 ;; day-of-month digits followed by month digits.
	 (mm "[ 0-1]?[0-9]")
	 (east-asian
	  (concat "\\(" mm l "?" s dd l "?" s "+"
		  "\\|" dd s mm s "+" "\\)"
		  "\\(" HH:MM "\\|" yyyy l "?" "\\)")))
	 ;; The "[0-9]" below requires the previous column to end in a digit.
	 ;; This avoids recognizing `1 may 1997' as a date in the line:
	 ;; -r--r--r--   1 may      1997        1168 Oct 19 16:49 README

	 ;; The "[BkKMGTPEZY]?" below supports "ls -alh" output.

	 ;; For non-iso date formats, we add the ".*" in order to find
	 ;; the last possible match.  This avoids recognizing
	 ;; `jservice 10 1024' as a date in the line:
	 ;; drwxr-xr-x  3 jservice  10  1024 Jul  2  1997 esg-host

         ;; vc dired listings provide the state or blanks between file
         ;; permissions and date.  The state is always surrounded by
         ;; parentheses:
         ;; -rw-r--r-- (modified) 2005-10-22 21:25 files.el
         ;; This is not supported yet.
    (purecopy (concat "\\([0-9][BkKMGTPEZY]? " iso
		      "\\|.*[0-9][BkKMGTPEZY]? "
	              "\\(" western "\\|" western-comma
                      "\\|" DD-MMM-YYYY "\\|" east-asian "\\)"
		      "\\) +")))
  "Regular expression to match up to the file name in a directory listing.
The default value is designed to recognize dates and times
regardless of the language.")

(defvar insert-directory-ls-version 'unknown)

(defun insert-directory-wildcard-in-dir-p (dir)
  "Return non-nil if DIR contents a shell wildcard in the directory part.
The return value is a cons (DIR . WILDCARDS); DIR is the
`default-directory' in the Dired buffer, and WILDCARDS are the wildcards.

Valid wildcards are `*', `?', `[abc]' and `[a-z]'."
  (let ((wildcards "[?*"))
    (when (and (or (not (featurep 'ls-lisp))
                   ls-lisp-support-shell-wildcards)
               (string-match (concat "[" wildcards "]") (file-name-directory dir))
               (not (file-exists-p dir))) ; Prefer an existing file to wildcards.
      (let ((regexp (format "\\`\\([^%s]*/\\)\\([^%s]*[%s].*\\)"
                            wildcards wildcards wildcards)))
        (string-match regexp dir)
        (cons (match-string 1 dir) (match-string 2 dir))))))

(defun insert-directory-clean (beg switches)
  (when (if (stringp switches)
	    (string-match "--dired\\>" switches)
	  (member "--dired" switches))
    ;; The following overshoots by one line for an empty
    ;; directory listed with "--dired", but without "-a"
    ;; switch, where the ls output contains a
    ;; "//DIRED-OPTIONS//" line, but no "//DIRED//" line.
    ;; We take care of that case later.
    (forward-line -2)
    (when (looking-at "//SUBDIRED//")
      (delete-region (point) (progn (forward-line 1) (point)))
      (forward-line -1))
    (if (looking-at "//DIRED//")
	(let ((end (line-end-position))
	      (linebeg (point))
	      error-lines)
	  ;; Find all the lines that are error messages,
	  ;; and record the bounds of each one.
	  (goto-char beg)
	  (while (< (point) linebeg)
	    (or (eql (following-char) ?\s)
		(push (list (point) (line-end-position)) error-lines))
	    (forward-line 1))
	  (setq error-lines (nreverse error-lines))
	  ;; Now read the numeric positions of file names.
	  (goto-char linebeg)
	  (forward-word-strictly 1)
	  (forward-char 3)
	  (while (< (point) end)
	    (let ((start (insert-directory-adj-pos
			  (+ beg (read (current-buffer)))
			  error-lines))
		  (end (insert-directory-adj-pos
			(+ beg (read (current-buffer)))
			error-lines)))
	      (if (memq (char-after end) '(?\n ?\s))
		  ;; End is followed by \n or by " -> ".
		  (put-text-property start end 'dired-filename t)
		;; It seems that we can't trust ls's output as to
		;; byte positions of filenames.
		(put-text-property beg (point) 'dired-filename nil)
		(end-of-line))))
	  (goto-char end)
	  (beginning-of-line)
	  (delete-region (point) (progn (forward-line 1) (point))))
      ;; Take care of the case where the ls output contains a
      ;; "//DIRED-OPTIONS//"-line, but no "//DIRED//"-line
      ;; and we went one line too far back (see above).
      (forward-line 1))
    (if (looking-at "//DIRED-OPTIONS//")
	(delete-region (point) (progn (forward-line 1) (point))))))

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
;; - may be passed "--dired" as the first argument in SWITCHES.
;;   File name handlers might have to remove this switch if their
;;   "ls" command does not support it.
(defun insert-directory (file switches &optional wildcard full-directory-p)
  "Insert directory listing for FILE, formatted according to SWITCHES.
Leaves point after the inserted text.
SWITCHES may be a string of options, or a list of strings
representing individual options.
Optional third arg WILDCARD means treat FILE as shell wildcard.
Optional fourth arg FULL-DIRECTORY-P means file is a directory and
switches do not contain `d', so that a full listing is expected.

This works by running a directory listing program
whose name is in the variable `insert-directory-program'.
If WILDCARD, it also runs the shell specified by `shell-file-name'.

When SWITCHES contains the long `--dired' option, this function
treats it specially, for the sake of dired.  However, the
normally equivalent short `-D' option is just passed on to
`insert-directory-program', as any other option."
  ;; We need the directory in order to find the right handler.
  (let ((handler (find-file-name-handler (expand-file-name file)
					 'insert-directory)))
    (if handler
	(funcall handler 'insert-directory file switches
		 wildcard full-directory-p)
	(let (result (beg (point)))

	  ;; Read the actual directory using `insert-directory-program'.
	  ;; RESULT gets the status code.
	  (let* (;; We at first read by no-conversion, then after
		 ;; putting text property `dired-filename, decode one
		 ;; bunch by one to preserve that property.
		 (coding-system-for-read 'no-conversion)
		 ;; This is to control encoding the arguments in call-process.
		 (coding-system-for-write
		  (and enable-multibyte-characters
		       (or file-name-coding-system
			   default-file-name-coding-system))))
	    (setq result
		  (if wildcard
		      ;; If the wildcard is just in the file part, then run ls in
                      ;; the directory part of the file pattern using the last
                      ;; component as argument.  Otherwise, run ls in the longest
                      ;; subdirectory of the directory part free of wildcards; use
                      ;; the remaining of the file pattern as argument.
		      (let* ((dir-wildcard (insert-directory-wildcard-in-dir-p file))
                             (default-directory
                               (cond (dir-wildcard (car dir-wildcard))
                                     (t
			              (if (file-name-absolute-p file)
				          (file-name-directory file)
				        (file-name-directory (expand-file-name file))))))
			     (pattern (if dir-wildcard (cdr dir-wildcard) (file-name-nondirectory file))))
			;; NB since switches is passed to the shell, be
			;; careful of malicious values, eg "-l;reboot".
			;; See eg dired-safe-switches-p.
			(call-process
			 shell-file-name nil t nil
			 shell-command-switch
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
 		    (unless full-directory-p
 		      (setq switches
 			    (cond
                             ((stringp switches) (concat switches " -d"))
                             ((member "-d" switches) switches)
                             (t (append switches '("-d"))))))
		    (if (string-match "\\`~" file)
			(setq file (expand-file-name file)))
		    (apply 'call-process
			   insert-directory-program nil t nil
			   (append
			    (if (listp switches) switches
			      (unless (equal switches "")
				;; Split the switches at any spaces so we can
				;; pass separate options as separate args.
				(split-string-and-unquote switches)))
			    ;; Avoid lossage if FILE starts with `-'.
			    '("--")
			    (list file))))))

	  ;; If we got "//DIRED//" in the output, it means we got a real
	  ;; directory listing, even if `ls' returned nonzero.
	  ;; So ignore any errors.
	  (when (if (stringp switches)
		    (string-match "--dired\\>" switches)
		  (member "--dired" switches))
	    (save-excursion
	      (forward-line -2)
	      (when (looking-at "//SUBDIRED//")
		(forward-line -1))
	      (if (looking-at "//DIRED//")
		  (setq result 0))))

	  (when (and (not (eq 0 result))
		     (eq insert-directory-ls-version 'unknown))
	    ;; The first time ls returns an error,
	    ;; find the version numbers of ls,
	    ;; and set insert-directory-ls-version
	    ;; to > if it is more than 5.2.1, < if it is less, nil if it
	    ;; is equal or if the info cannot be obtained.
	    ;; (That can mean it isn't GNU ls.)
	    (let ((version-out
		   (with-temp-buffer
		     (call-process "ls" nil t nil "--version")
		     (buffer-string))))
	      (if (string-match "ls (.*utils) \\([0-9.]*\\)$" version-out)
		  (let* ((version (match-string 1 version-out))
			 (split (split-string version "[.]"))
			 (numbers (mapcar 'string-to-number split))
			 (min '(5 2 1))
			 comparison)
		    (while (and (not comparison) (or numbers min))
		      (cond ((null min)
			     (setq comparison '>))
			    ((null numbers)
			     (setq comparison '<))
			    ((> (car numbers) (car min))
			     (setq comparison '>))
			    ((< (car numbers) (car min))
			     (setq comparison '<))
			    (t
			     (setq numbers (cdr numbers)
				   min (cdr min)))))
		    (setq insert-directory-ls-version (or comparison '=)))
		(setq insert-directory-ls-version nil))))

	  ;; For GNU ls versions 5.2.2 and up, ignore minor errors.
	  (when (and (eq 1 result) (eq insert-directory-ls-version '>))
	    (setq result 0))

	  ;; If `insert-directory-program' failed, signal an error.
	  (unless (eq 0 result)
	    ;; Delete the error message it may have output.
	    (delete-region beg (point))
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
          (insert-directory-clean beg switches)
	  ;; Now decode what read if necessary.
	  (let ((coding (or coding-system-for-read
			    file-name-coding-system
			    default-file-name-coding-system
			    'undecided))
		coding-no-eol
		val pos)
	    (when (and enable-multibyte-characters
		       (not (memq (coding-system-base coding)
				  '(raw-text no-conversion))))
	      ;; If no coding system is specified or detection is
	      ;; requested, detect the coding.
	      (if (eq (coding-system-base coding) 'undecided)
		  (setq coding (detect-coding-region beg (point) t)))
	      (if (not (eq (coding-system-base coding) 'undecided))
		  (save-restriction
		    (setq coding-no-eol
			  (coding-system-change-eol-conversion coding 'unix))
		    (narrow-to-region beg (point))
		    (goto-char (point-min))
		    (while (not (eobp))
		      (setq pos (point)
			    val (get-text-property (point) 'dired-filename))
		      (goto-char (next-single-property-change
				  (point) 'dired-filename nil (point-max)))
		      ;; Force no eol conversion on a file name, so
		      ;; that CR is preserved.
		      (decode-coding-region pos (point)
					    (if val coding-no-eol coding))
		      (if val
			  (put-text-property pos (point)
					     'dired-filename t)))))))

	  (if full-directory-p
	      ;; Try to insert the amount of free space.
	      (save-excursion
		(goto-char beg)
		;; First find the line to put it on.
		(when (re-search-forward "^ *\\(total\\)" nil t)
		  ;; Replace "total" with "total used in directory" to
		  ;; avoid confusion.
		  (replace-match "total used in directory" nil nil nil 1)
		  (let ((available (get-free-disk-space file)))
		    (when available
		      (end-of-line)
		      (insert " available " available))))))))))

(defun insert-directory-adj-pos (pos error-lines)
  "Convert `ls --dired' file name position value POS to a buffer position.
File name position values returned in ls --dired output
count only stdout; they don't count the error messages sent to stderr.
So this function converts to them to real buffer positions.
ERROR-LINES is a list of buffer positions of error message lines,
of the form (START END)."
  (while (and error-lines (< (caar error-lines) pos))
    (setq pos (+ pos (- (nth 1 (car error-lines)) (nth 0 (car error-lines)))))
    (pop error-lines))
  pos)

(defun insert-directory-safely (file switches
				     &optional wildcard full-directory-p)
  "Insert directory listing for FILE, formatted according to SWITCHES.

Like `insert-directory', but if FILE does not exist, it inserts a
message to that effect instead of signaling an error."
  (if (file-exists-p file)
      (insert-directory file switches wildcard full-directory-p)
    ;; Simulate the message printed by `ls'.
    (insert (format "%s: No such file or directory\n" file))))

(defcustom kill-emacs-query-functions nil
  "Functions to call with no arguments to query about killing Emacs.
If any of these functions returns nil, killing Emacs is canceled.
`save-buffers-kill-emacs' calls these functions, but `kill-emacs',
the low level primitive, does not.  See also `kill-emacs-hook'."
  :type 'hook
  :version "26.1"
  :group 'convenience)

(defcustom confirm-kill-emacs nil
  "How to ask for confirmation when leaving Emacs.
If nil, the default, don't ask at all.  If the value is non-nil, it should
be a predicate function; for example `yes-or-no-p'."
  :type '(choice (const :tag "Ask with yes-or-no-p" yes-or-no-p)
		 (const :tag "Ask with y-or-n-p" y-or-n-p)
		 (const :tag "Don't confirm" nil)
		 (function :tag "Predicate function"))
  :group 'convenience
  :version "21.1")

(defcustom confirm-kill-processes t
  "Non-nil if Emacs should confirm killing processes on exit.
If this variable is nil, the value of
`process-query-on-exit-flag' is ignored.  Otherwise, if there are
processes with a non-nil `process-query-on-exit-flag', Emacs will
prompt the user before killing them."
  :type 'boolean
  :group 'convenience
  :version "26.1")

(defun save-buffers-kill-emacs (&optional arg)
  "Offer to save each buffer, then kill this Emacs process.
With prefix ARG, silently save all file-visiting buffers without asking.
If there are active processes where `process-query-on-exit-flag'
returns non-nil and `confirm-kill-processes' is non-nil,
asks whether processes should be killed.
Runs the members of `kill-emacs-query-functions' in turn and stops
if any returns nil.  If `confirm-kill-emacs' is non-nil, calls it."
  (interactive "P")
  ;; Don't use save-some-buffers-default-predicate, because we want
  ;; to ask about all the buffers before killing Emacs.
  (save-some-buffers arg t)
  (let ((confirm confirm-kill-emacs))
    (and
     (or (not (memq t (mapcar (lambda (buf)
                                (and (buffer-file-name buf)
                                     (buffer-modified-p buf)))
                              (buffer-list))))
         (progn (setq confirm nil)
                (yes-or-no-p "Modified buffers exist; exit anyway? ")))
     (or (not (fboundp 'process-list))
         ;; process-list is not defined on MSDOS.
         (not confirm-kill-processes)
         (let ((processes (process-list))
               active)
           (while processes
             (and (memq (process-status (car processes)) '(run stop open listen))
                  (process-query-on-exit-flag (car processes))
                  (setq active t))
             (setq processes (cdr processes)))
           (or (not active)
               (with-current-buffer-window
                (get-buffer-create "*Process List*")
                `(display-buffer--maybe-at-bottom
                  (dedicated . t)
                  (window-height . fit-window-to-buffer)
                  (preserve-size . (nil . t))
                  (body-function
                   . ,#'(lambda (_window)
                          (list-processes t))))
                #'(lambda (window _value)
                    (with-selected-window window
                      (unwind-protect
                          (progn
                            (setq confirm nil)
                            (yes-or-no-p "Active processes exist; kill them and exit anyway? "))
                        (when (window-live-p window)
                          (quit-restore-window window 'kill)))))))))
     ;; Query the user for other things, perhaps.
     (run-hook-with-args-until-failure 'kill-emacs-query-functions)
     (or (null confirm)
         (funcall confirm "Really exit Emacs? "))
     (kill-emacs))))

(defun save-buffers-kill-terminal (&optional arg)
  "Offer to save each buffer, then kill the current connection.
If the current frame has no client, kill Emacs itself using
`save-buffers-kill-emacs'.

With prefix ARG, silently save all file-visiting buffers, then kill.

If emacsclient was started with a list of file names to edit, then
only these files will be asked to be saved."
  (interactive "P")
  (if (frame-parameter nil 'client)
      (server-save-buffers-kill-terminal arg)
    (save-buffers-kill-emacs arg)))

;; We use /: as a prefix to "quote" a file name
;; so that magic file name handlers will not apply to it.

(setq file-name-handler-alist
      (cons (cons (purecopy "\\`/:") 'file-name-non-special)
	    file-name-handler-alist))

;; We depend on being the last handler on the list,
;; so that anything else that does need handling
;; has been handled already.
;; So it is safe for us to inhibit *all* magic file name handlers for
;; operations, which return a file name.  See Bug#29579.

(defun file-name-non-special (operation &rest arguments)
  (let ((inhibit-file-name-handlers
         (cons 'file-name-non-special
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation)
        ;; Some operations respect file name handlers in
        ;; `default-directory'.  Because core function like
        ;; `call-process' don't care about file name handlers in
        ;; `default-directory', we here have to resolve the directory
        ;; into a local one.  For `process-file',
        ;; `start-file-process', and `shell-command', this fixes
        ;; Bug#25949.
        (default-directory
	  (if (memq operation
                    '(insert-directory process-file start-file-process
                                       make-process shell-command
                                       temporary-file-directory))
	      (directory-file-name
	       (expand-file-name
		(unhandled-file-name-directory default-directory)))
	    default-directory))
	;; Get a list of the indices of the args that are file names.
	(file-arg-indices
	 (cdr (or (assq operation
			'(;; The first seven are special because they
			  ;; return a file name.  We want to include
			  ;; the /: in the return value.  So just
			  ;; avoid stripping it in the first place.
                          (directory-file-name)
                          (expand-file-name)
                          (file-name-as-directory)
                          (file-name-directory)
                          (file-name-sans-versions)
                          (file-remote-p)
                          (find-backup-file-name)
	                  ;; `identity' means just return the first
			  ;; arg not stripped of its quoting.
			  (substitute-in-file-name identity)
			  ;; `add' means add "/:" to the result.
			  (file-truename add 0)
                          ;;`insert-file-contents' needs special handling.
			  (insert-file-contents insert-file-contents 0)
			  ;; `unquote-then-quote' means set buffer-file-name
			  ;; temporarily to unquoted filename.
			  (verify-visited-file-modtime unquote-then-quote)
                          ;; Unquote `buffer-file-name' temporarily.
                          (make-auto-save-file-name buffer-file-name)
                          (set-visited-file-modtime buffer-file-name)
                          ;; Use a temporary local copy.
			  (copy-file local-copy)
			  (rename-file local-copy)
                          (copy-directory local-copy)
			  ;; List the arguments that are filenames.
			  (file-name-completion 0 1)
			  (file-name-all-completions 0 1)
                          (file-equal-p 0 1)
                          (file-newer-than-file-p 0 1)
			  (write-region 2 5)
			  (file-in-directory-p 0 1)
			  (make-symbolic-link 0 1)
			  (add-name-to-file 0 1)
                          ;; These file-notify-* operations take a
                          ;; descriptor.
                          (file-notify-rm-watch)
                          (file-notify-valid-p)
                          ;; `make-process' uses keyword arguments and
                          ;; doesn't mangle its filenames in any way.
                          ;; It already strips /: from the binary
                          ;; filename, so we don't have to do this
                          ;; here.
                          (make-process)))
		  ;; For all other operations, treat the first
		  ;; argument only as the file name.
		  '(nil 0))))
	method
	;; Copy ARGUMENTS so we can replace elements in it.
	(arguments (copy-sequence arguments)))
    (if (symbolp (car file-arg-indices))
	(setq method (pop file-arg-indices)))
    ;; Strip off the /: from the file names that have it.
    (save-match-data                    ;FIXME: Why?
      (while (consp file-arg-indices)
	(let ((pair (nthcdr (car file-arg-indices) arguments)))
	  (when (car pair)
	    (setcar pair (file-name-unquote (car pair) t))))
	(setq file-arg-indices (cdr file-arg-indices))))
    ;; In general, we don't want any file name handler, see Bug#47625,
    ;; Bug#48349.  For some few cases, operations with two file name
    ;; arguments which might be bound to different file name handlers,
    ;; we still need this.
    (let ((tramp-mode (and tramp-mode (eq method 'local-copy))))
      (pcase method
        ('identity (car arguments))
        ('add (file-name-quote (apply operation arguments) t))
        ('buffer-file-name
         (let ((buffer-file-name (file-name-unquote buffer-file-name t)))
           (apply operation arguments)))
        ('insert-file-contents
         (let ((visit (nth 1 arguments)))
           (unwind-protect
               (apply operation arguments)
             (when (and visit buffer-file-name)
               (setq buffer-file-name (file-name-quote buffer-file-name t))))))
        ('unquote-then-quote
         ;; We can't use `cl-letf' with `(buffer-local-value)' here
         ;; because it wouldn't work during bootstrapping.
         (let ((buffer (current-buffer)))
           ;; `unquote-then-quote' is used only for the
           ;; `verify-visited-file-modtime' action, which takes a
           ;; buffer as only optional argument.
           (with-current-buffer (or (car arguments) buffer)
             (let ((buffer-file-name (file-name-unquote buffer-file-name t)))
               ;; Make sure to hide the temporary buffer change from
               ;; the underlying operation.
               (with-current-buffer buffer
                 (apply operation arguments))))))
        ('local-copy
         (let ((source (car arguments))
               (target (car (cdr arguments)))
               (prefix (expand-file-name
                        "file-name-non-special" temporary-file-directory))
               tmpfile)
           (cond
            ;; If source is remote, we must create a local copy.
            ((file-remote-p source)
             (setq tmpfile (make-temp-name prefix))
             (apply operation source tmpfile (cddr arguments))
             (setq source tmpfile))
            ;; If source is quoted, and the unquoted source looks
            ;; remote, we must create a local copy.
            ((file-name-quoted-p source t)
             (setq source (file-name-unquote source t))
             (when (file-remote-p source)
               (setq tmpfile (make-temp-name prefix))
               (let (file-name-handler-alist)
                 (apply operation source tmpfile (cddr arguments)))
               (setq source tmpfile))))
           ;; If target is quoted, and the unquoted target looks
           ;; remote, we must disable the file name handler.
           (when (file-name-quoted-p target t)
             (setq target (file-name-unquote target t))
             (when (file-remote-p target)
               (setq file-name-handler-alist nil)))
           ;; Do it.
           (setcar arguments source)
           (setcar (cdr arguments) target)
           (apply operation arguments)
           ;; Cleanup.
           (when (and tmpfile (file-exists-p tmpfile))
             (if (file-directory-p tmpfile)
                 (delete-directory tmpfile 'recursive) (delete-file tmpfile)))))
        (_
         (apply operation arguments))))))

(defsubst file-name-quoted-p (name &optional top)
  "Whether NAME is quoted with prefix \"/:\".
If NAME is a remote file name and TOP is nil, check the local part of NAME."
  (let ((file-name-handler-alist (unless top file-name-handler-alist)))
    (string-prefix-p "/:" (file-local-name name))))

(defsubst file-name-quote (name &optional top)
  "Add the quotation prefix \"/:\" to file NAME.
If NAME is a remote file name and TOP is nil, the local part of
NAME is quoted.  If NAME is already a quoted file name, NAME is
returned unchanged."
  (let ((file-name-handler-alist (unless top file-name-handler-alist)))
    (if (file-name-quoted-p name top)
        name
      (concat (file-remote-p name) "/:" (file-local-name name)))))

(defsubst file-name-unquote (name &optional top)
  "Remove quotation prefix \"/:\" from file NAME, if any.
If NAME is a remote file name and TOP is nil, the local part of
NAME is unquoted."
  (let* ((file-name-handler-alist (unless top file-name-handler-alist))
         (localname (file-local-name name)))
    (when (file-name-quoted-p localname top)
      (setq
       localname (if (= (length localname) 2) "/" (substring localname 2))))
    (concat (file-remote-p name) localname)))

;; Symbolic modes and read-file-modes.

(defun file-modes-char-to-who (char)
  "Convert CHAR to a numeric bit-mask for extracting mode bits.
CHAR is in [ugoa] and represents the category of users (Owner, Group,
Others, or All) for whom to produce the mask.
The bit-mask that is returned extracts from mode bits the access rights
for the specified category of users."
  (cond ((= char ?u) #o4700)
	((= char ?g) #o2070)
	((= char ?o) #o1007)
	((= char ?a) #o7777)
        (t (error "%c: Bad `who' character" char))))

(defun file-modes-char-to-right (char &optional from)
  "Convert CHAR to a numeric value of mode bits.
CHAR is in [rwxXstugo] and represents symbolic access permissions.
If CHAR is in [Xugo], the value is taken from FROM (or 0 if omitted)."
  (or from (setq from 0))
  (cond ((= char ?r) #o0444)
	((= char ?w) #o0222)
	((= char ?x) #o0111)
	((= char ?s) #o6000)
	((= char ?t) #o1000)
	;; Rights relative to the previous file modes.
	((= char ?X) (if (= (logand from #o111) 0) 0 #o0111))
	((= char ?u) (let ((uright (logand #o4700 from)))
		       ;; FIXME: These divisions/shifts seem to be right
                       ;; for the `7' part of the #o4700 mask, but not
                       ;; for the `4' part.  Same below for `g' and `o'.
		       (+ uright (/ uright #o10) (/ uright #o100))))
	((= char ?g) (let ((gright (logand #o2070 from)))
		       (+ gright (/ gright #o10) (* gright #o10))))
	((= char ?o) (let ((oright (logand #o1007 from)))
		       (+ oright (* oright #o10) (* oright #o100))))
        (t (error "%c: Bad right character" char))))

(defun file-modes-rights-to-number (rights who-mask &optional from)
  "Convert a symbolic mode string specification to an equivalent number.
RIGHTS is the symbolic mode spec, it should match \"([+=-][rwxXstugo]*)+\".
WHO-MASK is the bit-mask specifying the category of users to which to
apply the access permissions.  See `file-modes-char-to-who'.
FROM (or 0 if nil) gives the mode bits on which to base permissions if
RIGHTS request to add, remove, or set permissions based on existing ones,
as in \"og+rX-w\"."
  (let* ((num-rights (or from 0))
	 (list-rights (string-to-list rights))
	 (op (pop list-rights)))
    (while (memq op '(?+ ?- ?=))
      (let ((num-right 0)
	    char-right)
	(while (memq (setq char-right (pop list-rights))
		     '(?r ?w ?x ?X ?s ?t ?u ?g ?o))
	  (setq num-right
		(logior num-right
			(file-modes-char-to-right char-right num-rights))))
	(setq num-right (logand who-mask num-right)
	      num-rights
	      (cond ((= op ?+) (logior num-rights num-right))
		    ((= op ?-) (logand num-rights (lognot num-right)))
		    (t (logior (logand num-rights (lognot who-mask)) num-right)))
	      op char-right)))
    num-rights))

(defun file-modes-number-to-symbolic (mode &optional filetype)
  "Return a string describing a file's MODE.
For instance, if MODE is #o700, then it produces `-rwx------'.
FILETYPE if provided should be a character denoting the type of file,
such as `?d' for a directory, or `?l' for a symbolic link and will override
the leading `-' char."
  (string
   (or filetype
       (pcase (lsh mode -12)
         ;; POSIX specifies that the file type is included in st_mode
         ;; and provides names for the file types but values only for
         ;; the permissions (e.g., S_IWOTH=2).

         ;; (#o017 ??) ;; #define S_IFMT  00170000
         (#o014 ?s)    ;; #define S_IFSOCK 0140000
         (#o012 ?l)    ;; #define S_IFLNK  0120000
         ;; (8  ??)    ;; #define S_IFREG  0100000
         (#o006  ?b)   ;; #define S_IFBLK  0060000
         (#o004  ?d)   ;; #define S_IFDIR  0040000
         (#o002  ?c)   ;; #define S_IFCHR  0020000
         (#o001  ?p)   ;; #define S_IFIFO  0010000
         (_ ?-)))
   (if (zerop (logand   256 mode)) ?- ?r)
   (if (zerop (logand   128 mode)) ?- ?w)
   (if (zerop (logand    64 mode))
       (if (zerop (logand  2048 mode)) ?- ?S)
     (if (zerop (logand  2048 mode)) ?x ?s))
   (if (zerop (logand    32 mode)) ?- ?r)
   (if (zerop (logand    16 mode)) ?- ?w)
   (if (zerop (logand     8 mode))
       (if (zerop (logand  1024 mode)) ?- ?S)
     (if (zerop (logand  1024 mode)) ?x ?s))
   (if (zerop (logand     4 mode)) ?- ?r)
   (if (zerop (logand     2 mode)) ?- ?w)
   (if (zerop (logand 512 mode))
       (if (zerop (logand   1 mode)) ?- ?x)
     (if (zerop (logand   1 mode)) ?T ?t))))

(defun file-modes-symbolic-to-number (modes &optional from)
  "Convert symbolic file modes to numeric file modes.
MODES is the string to convert, it should match
\"[ugoa]*([+-=][rwxXstugo]*)+,...\".
See Info node `(coreutils)File permissions' for more information on this
notation.
FROM (or 0 if nil) gives the mode bits on which to base permissions if
MODES request to add, remove, or set permissions based on existing ones,
as in \"og+rX-w\"."
  (save-match-data
    (let ((case-fold-search nil)
	  (num-modes (or from 0)))
      (while (/= (string-to-char modes) 0)
	(if (string-match "^\\([ugoa]*\\)\\([+=-][rwxXstugo]*\\)+\\(,\\|\\)" modes)
	    (let ((num-who (apply 'logior 0
				  (mapcar 'file-modes-char-to-who
					  (match-string 1 modes)))))
	      (when (= num-who 0)
		(setq num-who (logior #o7000 (default-file-modes))))
	      (setq num-modes
		    (file-modes-rights-to-number (substring modes (match-end 1))
						 num-who num-modes)
		    modes (substring modes (match-end 3))))
	  (error "Parse error in modes near `%s'" (substring modes 0))))
      num-modes)))

(defun read-file-modes (&optional prompt orig-file)
  "Read file modes in octal or symbolic notation and return its numeric value.
PROMPT is used as the prompt, default to \"File modes (octal or symbolic): \".
ORIG-FILE is the name of a file on whose mode bits to base returned
permissions if what user types requests to add, remove, or set permissions
based on existing mode bits, as in \"og+rX-w\"."
  (let* ((modes (or (if orig-file (file-modes orig-file) 0)
		    (error "File not found")))
	 (modestr (and (stringp orig-file)
		       (file-attribute-modes (file-attributes orig-file))))
	 (default
	   (and (stringp modestr)
		(string-match "^.\\(...\\)\\(...\\)\\(...\\)$" modestr)
		(string-replace
		 "-" ""
		 (format "u=%s,g=%s,o=%s"
			 (match-string 1 modestr)
			 (match-string 2 modestr)
			 (match-string 3 modestr)))))
	 (value (read-string (or prompt "File modes (octal or symbolic): ")
			     nil nil default)))
    (save-match-data
      (if (string-match "^[0-7]+" value)
	  (string-to-number value 8)
	(file-modes-symbolic-to-number value modes)))))

(define-obsolete-variable-alias 'cache-long-line-scans
  'cache-long-scans "24.4")


;; Trashcan handling.
(defcustom trash-directory nil
  "Directory for `move-file-to-trash' to move files and directories to.
This directory is used only when the function `system-move-file-to-trash'
is not defined.
Relative paths are interpreted relative to `default-directory'.
If the value is nil, Emacs uses a freedesktop.org-style trashcan."
  :type  '(choice (const nil) directory)
  :group 'auto-save
  :version "23.2")

(defvar trash--hexify-table)

(declare-function system-move-file-to-trash "w32fns.c" (filename))

(defun move-file-to-trash (filename)
  "Move the file (or directory) named FILENAME to the trash.
When `delete-by-moving-to-trash' is non-nil, this function is
called by `delete-file' and `delete-directory' instead of
deleting files outright.

If the function `system-move-file-to-trash' is defined, call it
 with FILENAME as an argument.
Otherwise, if `trash-directory' is non-nil, move FILENAME to that
 directory.
Otherwise, trash FILENAME using the freedesktop.org conventions,
 like the GNOME, KDE and XFCE desktop environments.  Emacs moves
 files only to \"home trash\", ignoring per-volume trashcans."
  (interactive "fMove file to trash: ")
  ;; If `system-move-file-to-trash' is defined, use it.
  (cond ((fboundp 'system-move-file-to-trash)
	 (system-move-file-to-trash filename))
        (trash-directory
	 ;; If `trash-directory' is non-nil, move the file there.
	 (let* ((trash-dir   (expand-file-name trash-directory))
		(fn          (directory-file-name (expand-file-name filename)))
		(new-fn      (concat (file-name-as-directory trash-dir)
				     (file-name-nondirectory fn))))
	   ;; We can't trash a parent directory of trash-directory.
	   (if (string-prefix-p fn trash-dir)
	       (error "Trash directory `%s' is a subdirectory of `%s'"
		      trash-dir filename))
	   (unless (file-directory-p trash-dir)
	     (make-directory trash-dir t))
	   ;; Ensure that the trashed file-name is unique.
	   (if (file-exists-p new-fn)
	       (let ((version-control t)
		     (backup-directory-alist nil))
		 (setq new-fn (car (find-backup-file-name new-fn)))))
	   (let (delete-by-moving-to-trash)
	     (rename-file fn new-fn))))
	;; Otherwise, use the freedesktop.org method, as specified at
        ;; https://freedesktop.org/wiki/Specifications/trash-spec
	(t
	 (let* ((xdg-data-dir
		 (directory-file-name
		  (expand-file-name "Trash"
				    (or (getenv "XDG_DATA_HOME")
					"~/.local/share"))))
		(trash-files-dir (expand-file-name "files" xdg-data-dir))
		(trash-info-dir (expand-file-name "info" xdg-data-dir))
		(fn (directory-file-name (expand-file-name filename))))

	   ;; Check if we have permissions to delete.
	   (unless (file-writable-p (directory-file-name
				     (file-name-directory fn)))
	     (error "Cannot move %s to trash: Permission denied" filename))
	   ;; The trashed file cannot be the trash dir or its parent.
	   (if (string-prefix-p fn trash-files-dir)
	       (error "The trash directory %s is a subdirectory of %s"
		      trash-files-dir filename))
	   (if (string-prefix-p fn trash-info-dir)
	       (error "The trash directory %s is a subdirectory of %s"
		      trash-info-dir filename))

	   ;; Ensure that the trash directory exists; otherwise, create it.
	   (with-file-modes #o700
	     (unless (file-exists-p trash-files-dir)
	       (make-directory trash-files-dir t))
	     (unless (file-exists-p trash-info-dir)
	       (make-directory trash-info-dir t)))

	   ;; Try to move to trash with .trashinfo undo information
	   (save-excursion
	     (with-temp-buffer
	       (set-buffer-file-coding-system 'utf-8-unix)
	       (insert "[Trash Info]\nPath=")
	       ;; Perform url-encoding on FN.  For compatibility with
	       ;; other programs (e.g. XFCE Thunar), allow literal "/"
	       ;; for path separators.
	       (unless (boundp 'trash--hexify-table)
		 (setq trash--hexify-table (make-vector 256 nil))
		 (let ((unreserved-chars
			(list ?/ ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m
			      ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z ?A
			      ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O
			      ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z ?0 ?1 ?2
			      ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?- ?_ ?. ?! ?~ ?* ?'
			      ?\( ?\))))
		   (dotimes (byte 256)
		     (aset trash--hexify-table byte
			   (if (memq byte unreserved-chars)
			       (char-to-string byte)
			     (format "%%%02x" byte))))))
	       (mapc (lambda (byte)
		       (insert (aref trash--hexify-table byte)))
		     (if (multibyte-string-p fn)
			 (encode-coding-string fn 'utf-8)
		       fn))
	       (insert "\nDeletionDate="
		       (format-time-string "%Y-%m-%dT%T")
		       "\n")

	       ;; Make a .trashinfo file.  Use O_EXCL, as per trash-spec 1.0.
	       (let* ((files-base (file-name-nondirectory fn))
                      (is-directory (file-directory-p fn))
                      (overwrite nil)
                      info-fn)
                 ;; We're checking further down whether the info file
                 ;; exists, but the file name may exist in the trash
                 ;; directory even if there is no info file for it.
                 (when (file-exists-p
                        (file-name-concat trash-files-dir files-base))
                   (setq overwrite t
                         files-base (file-name-nondirectory
                                     (make-temp-file
                                      (file-name-concat
                                       trash-files-dir files-base)
                                      is-directory))))
		 (setq info-fn (file-name-concat
				trash-info-dir
                                (concat files-base ".trashinfo")))
                 ;; Re-check the existence (sort of).
		 (condition-case nil
		     (write-region nil nil info-fn nil 'quiet info-fn 'excl)
		   (file-already-exists
		    ;; Uniquify new-fn.  Some file managers do not
		    ;; like Emacs-style backup file names.  E.g.:
		    ;; https://bugs.kde.org/170956
		    (setq info-fn (make-temp-file
				   (file-name-concat trash-info-dir files-base)
				   nil ".trashinfo"))
		    (setq files-base (substring (file-name-nondirectory info-fn)
                                                0 (- (length ".trashinfo"))))
		    (write-region nil nil info-fn nil 'quiet info-fn)))
		 ;; Finally, try to move the file to the trashcan.
		 (let ((delete-by-moving-to-trash nil)
		       (new-fn (file-name-concat trash-files-dir files-base)))
		   (rename-file fn new-fn overwrite)))))))))


(defsubst file-attribute-type (attributes)
  "The type field in ATTRIBUTES returned by `file-attributes'.
The value is either t for directory, string (name linked to) for
symbolic link, or nil."
  (nth 0 attributes))

(defsubst file-attribute-link-number (attributes)
  "Return the number of links in ATTRIBUTES returned by `file-attributes'."
  (nth 1 attributes))

(defsubst file-attribute-user-id (attributes)
  "The UID field in ATTRIBUTES returned by `file-attributes'.
This is either a string or a number.  If a string value cannot be
looked up, a numeric value, either an integer or a float, is
returned."
  (nth 2 attributes))

(defsubst file-attribute-group-id (attributes)
  "The GID field in ATTRIBUTES returned by `file-attributes'.
This is either a string or a number.  If a string value cannot be
looked up, a numeric value, either an integer or a float, is
returned."
  (nth 3 attributes))

(defsubst file-attribute-access-time (attributes)
  "The last access time in ATTRIBUTES returned by `file-attributes'.
This a Lisp timestamp in the style of `current-time'."
  (nth 4 attributes))

(defsubst file-attribute-modification-time (attributes)
  "The modification time in ATTRIBUTES returned by `file-attributes'.
This is the time of the last change to the file's contents, and
is a Lisp timestamp in the style of `current-time'."
  (nth 5 attributes))

(defsubst file-attribute-status-change-time (attributes)
  "The status modification time in ATTRIBUTES returned by `file-attributes'.
This is the time of last change to the file's attributes: owner
and group, access mode bits, etc., and is a Lisp timestamp in the
style of `current-time'."
  (nth 6 attributes))

(defsubst file-attribute-size (attributes)
  "The integer size (in bytes) in ATTRIBUTES returned by `file-attributes'."
  (nth 7 attributes))

(defsubst file-attribute-modes (attributes)
  "The file modes in ATTRIBUTES returned by `file-attributes'.
This is a string of ten letters or dashes as in ls -l."
  (nth 8 attributes))

(defsubst file-attribute-inode-number (attributes)
  "The inode number in ATTRIBUTES returned by `file-attributes'.
It is a nonnegative integer."
  (nth 10 attributes))

(defsubst file-attribute-device-number (attributes)
  "The file system device number in ATTRIBUTES returned by `file-attributes'.
It is an integer."
  (nth 11 attributes))

(defun file-attribute-collect (attributes &rest attr-names)
  "Return a sublist of ATTRIBUTES returned by `file-attributes'.
ATTR-NAMES are symbols with the selected attribute names.

Valid attribute names are: type, link-number, user-id, group-id,
access-time, modification-time, status-change-time, size, modes,
inode-number and device-number."
  (let ((all '(type link-number user-id group-id access-time
               modification-time status-change-time
               size modes inode-number device-number))
        result)
    (while attr-names
      (let ((attr (pop attr-names)))
        (if (memq attr all)
            (push (funcall
                   (intern (format "file-attribute-%s" (symbol-name attr)))
                   attributes)
                  result)
          (error "Wrong attribute name '%S'" attr))))
    (nreverse result)))

(define-key ctl-x-map "\C-f" 'find-file)
(define-key ctl-x-map "\C-r" 'find-file-read-only)
(define-key ctl-x-map "\C-v" 'find-alternate-file)
(define-key ctl-x-map "\C-s" 'save-buffer)
(define-key ctl-x-map "s" 'save-some-buffers)
(define-key ctl-x-map "\C-w" 'write-file)
(define-key ctl-x-map "i" 'insert-file)
(define-key esc-map "~" 'not-modified)
(define-key ctl-x-map "\C-d" 'list-directory)
(define-key ctl-x-map "\C-c" 'save-buffers-kill-terminal)
(define-key ctl-x-map "\C-q" 'read-only-mode)

(define-key ctl-x-4-map "f" 'find-file-other-window)
(define-key ctl-x-4-map "r" 'find-file-read-only-other-window)
(define-key ctl-x-4-map "\C-f" 'find-file-other-window)
(define-key ctl-x-4-map "b" 'switch-to-buffer-other-window)
(define-key ctl-x-4-map "\C-o" 'display-buffer)

(define-key ctl-x-5-map "b" 'switch-to-buffer-other-frame)
(define-key ctl-x-5-map "f" 'find-file-other-frame)
(define-key ctl-x-5-map "\C-f" 'find-file-other-frame)
(define-key ctl-x-5-map "r" 'find-file-read-only-other-frame)
(define-key ctl-x-5-map "\C-o" 'display-buffer-other-frame)

;;; files.el ends here
