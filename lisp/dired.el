;;; dired.el --- directory-browsing commands -*- lexical-binding: t -*-

;; Copyright (C) 1985-1986, 1992-1997, 2000-2022 Free Software
;; Foundation, Inc.

;; Author: Sebastian Kremer <sk@thp.uni-koeln.de>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: files
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

;; This is a major mode for directory browsing and editing.
;; It is documented in the Emacs manual.

;; Rewritten in 1990/1991 to add tree features, file marking and
;; sorting by Sebastian Kremer <sk@thp.uni-koeln.de>.
;; Finished up by rms in 1992.

;;; Code:

(eval-when-compile (require 'subr-x))
;; When bootstrapping dired-loaddefs has not been generated.
(require 'dired-loaddefs nil t)

(declare-function dired-buffer-more-recently-used-p
		  "dired-x" (buffer1 buffer2))


;;; Customizable variables

(defgroup dired nil
  "Directory editing."
  :link '(custom-manual "(emacs)Dired")
  :group 'files)

(defgroup dired-mark nil
  "Handling marks in Dired."
  :prefix "dired-"
  :group 'dired)

;;;###autoload
(defcustom dired-listing-switches (purecopy "-al")
  "Switches passed to `ls' for Dired.  MUST contain the `l' option.
May contain all other options that don't contradict `-l';
may contain even `F', `b', `i' and `s'.  See also the variable
`dired-ls-F-marks-symlinks' concerning the `F' switch.

If you have files with names with embedded newline characters, adding
`b' to the switches will allow Dired to handle those files better.

Options that include embedded whitespace must be quoted
like this: \"--option=value with spaces\"; you can use
`combine-and-quote-strings' to produce the correct quoting of
each option.

On systems such as MS-DOS and MS-Windows, which use `ls' emulation in Lisp,
some of the `ls' switches are not supported; see the doc string of
`insert-directory' in `ls-lisp.el' for more details."
  :type 'string
  :group 'dired)

(defcustom dired-subdir-switches nil
  "If non-nil, switches passed to `ls' for inserting subdirectories.
If nil, `dired-listing-switches' is used."
  :group 'dired
  :type '(choice (const :tag "Use dired-listing-switches" nil)
                 (string :tag "Switches")))

(defcustom dired-maybe-use-globstar nil
  "If non-nil, enable globstar if the shell supports it.
Some shells enable this feature by default (e.g. zsh or fish).

See `dired-enable-globstar-in-shell' for a list of shells
that support globstar and disable it by default.

Note that the implementations of globstar have small differences
between shells.  You must check your shell documentation to see
what to expect."
  :type 'boolean
  :group 'dired
  :version "28.1")

(defconst dired-enable-globstar-in-shell
  '(("ksh" . "set -G")
    ("bash" . "shopt -s globstar"))
  "Alist of (SHELL . COMMAND), where COMMAND enables globstar in SHELL.
If `dired-maybe-use-globstar' is non-nil, then `dired-insert-directory'
checks this alist to enable globstar in the shell subprocess.")

(defcustom dired-chown-program
  (purecopy (cond ((executable-find "chown") "chown")
                  ((file-executable-p "/usr/sbin/chown") "/usr/sbin/chown")
                  ((file-executable-p "/etc/chown") "/etc/chown")
                  (t "chown")))
  "Name of chown command (usually `chown')."
  :group 'dired
  :type 'file)

(defcustom dired-use-ls-dired 'unspecified
  "Non-nil means Dired should pass the \"--dired\" option to \"ls\".
If nil, don't pass \"--dired\" to \"ls\".
The special value of `unspecified' means to check whether \"ls\"
supports the \"--dired\" option, and save the result in this
variable.  This is performed the first time `dired-insert-directory'
is invoked.

Note that if you set this option to nil, either through choice or
because your \"ls\" program does not support \"--dired\", Dired
will fail to parse some \"unusual\" file names, e.g. those with leading
spaces.  You might want to install ls from GNU Coreutils, which does
support this option.  Alternatively, you might want to use Emacs's
own emulation of \"ls\", by using:
  (setq ls-lisp-use-insert-directory-program nil)
  (require \\='ls-lisp)
This is used by default on MS Windows, which does not have an \"ls\" program.
Note that `ls-lisp' does not support as many options as GNU ls, though.
For more details, see Info node `(emacs)ls in Lisp'."
  :group 'dired
  :type '(choice (const :tag
                        "Use --dired only if `ls' supports it" unspecified)
                 (const :tag "Do not use --dired" nil)
                 (other :tag "Always use --dired" t)))

(defvar dired-chmod-program "chmod"
  "Name of chmod command (usually `chmod').")
(make-obsolete-variable 'dired-chmod-program nil "28.1")

(defcustom dired-touch-program "touch"
  "Name of touch command (usually `touch')."
  :group 'dired
  :type 'file)

(defcustom dired-ls-F-marks-symlinks nil
  "Informs Dired about how `ls -lF' marks symbolic links.
Set this to t if `ls' (or whatever program is specified by
`insert-directory-program') with `-lF' marks the symbolic link
itself with a trailing @ (usually the case under Ultrix and macOS).

Example: if `ln -s foo bar; ls -F bar' gives `bar -> foo', set it to
nil (the default), if it gives `bar@ -> foo', set it to t.

Dired checks if there is really a @ appended.  Thus, if you have a
marking `ls' program on one host and a non-marking on another host, and
don't care about symbolic links which really end in a @, you can
always set this variable to t."
  :type 'boolean
  :group 'dired-mark)

(defcustom dired-trivial-filenames (purecopy "\\`\\.\\.?\\'\\|\\`\\.?#")
  "Regexp of files to skip when finding first file of a directory.
A value of nil means move to the subdir line.
A value of t means move to first file."
  :type '(choice (const :tag "Move to subdir" nil)
		 (const :tag "Move to first" t)
		 regexp)
  :group 'dired)

(defcustom dired-keep-marker-rename t
  ;; Use t as default so that moved files "take their markers with them".
  "Controls marking of renamed files.
If t, files keep their previous marks when they are renamed.
If a character, renamed files (whether previously marked or not)
are afterward marked with that character.
This option affects only files renamed by `dired-do-rename' and
`dired-do-rename-regexp'.  See `wdired-keep-marker-rename'
if you want to do the same for files renamed in WDired mode."
  :type '(choice (const :tag "Keep" t)
		 (character :tag "Mark" :value ?R))
  :group 'dired-mark)

(defcustom dired-keep-marker-copy ?C
  "Controls marking of copied files.
If t, copied files are marked if and as the corresponding original files were.
If a character, copied files are unconditionally marked with that character."
  :type '(choice (const :tag "Keep" t)
		 (character :tag "Mark"))
  :group 'dired-mark)

(defcustom dired-keep-marker-hardlink ?H
  "Controls marking of newly made hard links.
If t, they are marked if and as the files linked to were marked.
If a character, new links are unconditionally marked with that character."
  :type '(choice (const :tag "Keep" t)
		 (character :tag "Mark"))
  :group 'dired-mark)

(defcustom dired-keep-marker-symlink ?Y
  "Controls marking of newly made symbolic links.
If t, they are marked if and as the files linked to were marked.
If a character, new links are unconditionally marked with that character."
  :type '(choice (const :tag "Keep" t)
		 (character :tag "Mark"))
  :group 'dired-mark)

(defcustom dired-dwim-target nil
  "If non-nil, Dired tries to guess a default target directory.
This means: if there is a Dired buffer displayed in some window,
use its current directory, instead of this Dired buffer's
current directory.

You can customize it to prefer either the next window with a Dired buffer,
or the most recently used window with a Dired buffer, or to use any other
function.  When the value is a function, it will be called with no
arguments and is expected to return a list of directories which will
be used as defaults (i.e. default target and \"future history\")
\(though, `dired-dwim-target-defaults' might modify it a bit).
The value t prefers the next windows on the same frame.

The target is used in the prompt for file copy, rename etc."
  :type '(choice
          (const :tag "No guess" nil)
          (function-item :tag "Prefer next windows on the same frame"
                         dired-dwim-target-next)
          (function-item :tag "Prefer next windows on visible frames"
                         dired-dwim-target-next-visible)
          (function-item :tag "Prefer most recently used windows"
                         dired-dwim-target-recent)
          (function :tag "Custom function")
          (other :tag "Try to guess" t))
  :group 'dired)

(defcustom dired-copy-preserve-time t
  "If non-nil, Dired preserves the last-modified time in a file copy.
\(This works on only some systems.)"
  :type 'boolean
  :group 'dired)

(defcustom dired-copy-dereference nil
  "If non-nil, Dired dereferences symlinks when copying them.
This is similar to the \"-L\" option for the \"cp\" shell command."
  :type 'boolean
  :group 'dired)

;; These variables were deleted and the replacements are in files.el.
;; We leave aliases behind for back-compatibility.
(define-obsolete-variable-alias 'dired-free-space-program
  'directory-free-space-program "27.1")
(define-obsolete-variable-alias 'dired-free-space-args
  'directory-free-space-args "27.1")

(defcustom dired-load-hook nil
  "Run after loading Dired.
You can customize key bindings or load extensions with this."
  :group 'dired
  :type 'hook)
(make-obsolete-variable 'dired-load-hook
                        "use `with-eval-after-load' instead." "28.1")

(defcustom dired-mode-hook nil
  "Run at the very end of `dired-mode'."
  :group 'dired
  :type 'hook)

(defcustom dired-before-readin-hook nil
  "This hook is run before a Dired buffer is read in (created or reverted)."
  :group 'dired
  :type 'hook)

(defcustom dired-after-readin-hook nil
  "Hook run after each time a file or directory is read by Dired.
After each listing of a file or directory, this hook is run
with the buffer narrowed to the listing."
  :group 'dired
  :type 'hook)
;; Note this can't simply be run inside function `dired-ls' as the hook
;; functions probably depend on the dired-subdir-alist to be OK.

(defcustom dired-initial-position-hook nil
  "This hook is used to position the point.
It is run by the function `dired-initial-position'."
  :group 'dired
  :type 'hook
  :version "24.4")

(defcustom dired-dnd-protocol-alist
  '(("^file:///" . dired-dnd-handle-local-file)
    ("^file://"  . dired-dnd-handle-file)
    ("^file:"    . dired-dnd-handle-local-file))
  "The functions to call when a drop in `dired-mode' is made.
See `dnd-protocol-alist' for more information.  When nil, behave
as in other buffers.  Changing this option is effective only for
new Dired buffers."
  :type '(choice (repeat (cons (regexp) (function)))
		 (const :tag "Behave as in other buffers" nil))
  :version "22.1"
  :group 'dired)

(defcustom dired-hide-details-hide-symlink-targets t
  "Non-nil means `dired-hide-details-mode' hides symbolic link targets."
  :type 'boolean
  :version "24.4"
  :group 'dired)

(defcustom dired-hide-details-hide-information-lines t
  "Non-nil means `dired-hide-details-mode' hides all but header and file lines."
  :type 'boolean
  :version "24.4"
  :group 'dired)

(defcustom dired-always-read-filesystem nil
  "Non-nil means revert buffers visiting files before searching them.
By default,  commands like `dired-mark-files-containing-regexp' will
search any buffers visiting the marked files without reverting them,
even if they were changed on disk.  When this option is non-nil, such
buffers are always reverted in a temporary buffer before searching
them: the search is performed on the temporary buffer, the original
buffer visiting the file is not modified."
  :type 'boolean
  :version "26.1"
  :group 'dired)

(defcustom dired-mark-region 'file
  "Defines what commands that mark files do with the active region.

When nil, marking commands don't operate on all files in the
active region.  They process their prefix arguments as usual.

When the value of this option is non-nil, then all Dired commands
that mark or unmark files will operate on all files in the region
if the region is active in Transient Mark mode.

When `file', the region marking is based on the file name.
This means don't mark the file if the end of the region is
before the file name displayed on the Dired line, so the file name
is visually outside the region.  This behavior is consistent with
marking files without the region using the key `m' that advances
point to the next line after marking the file.  Thus the number
of keys used to mark files is the same as the number of keys
used to select the region, e.g. `M-2 m' marks 2 files, and
`C-SPC M-2 n m' marks 2 files, and `M-2 S-down m' marks 2 files.

When `line', the region marking is based on Dired lines,
so include the file into marking if the end of the region
is anywhere on its Dired line, except the beginning of the line."
  :type '(choice
          (const :tag "Don't mark files in active region" nil)
          (const :tag "Exclude file name outside of region" file)
          (const :tag "Include the file at region end line" line))
  :group 'dired
  :version "28.1")

(defcustom dired-kill-when-opening-new-dired-buffer nil
  "If non-nil, kill the current buffer when selecting a new directory."
  :type 'boolean
  :version "28.1")


;;; Internal variables

(defvar dired-marker-char ?*		; the answer is 42
  ;; so that you can write things like
  ;; (let ((dired-marker-char ?X))
  ;;    ;; great code using X markers ...
  ;;    )
  ;; For example, commands operating on two sets of files, A and B.
  ;; Or marking files with digits 0-9.  This could implicate
  ;; concentric sets or an order for the marked files.
  ;; The code depends on dynamic scoping on the marker char.
  "In Dired, the current mark character.
This is what the do-commands look for, and what the mark-commands store.")

(defvar dired-del-marker ?D
  "Character used to flag files for deletion.")

(defvar dired-shrink-to-fit t
  ;; I see no reason ever to make this nil -- rms.
  ;;  (> baud-rate search-slow-speed)
  "Non-nil means Dired shrinks the display buffer to fit the marked files.")
(make-obsolete-variable 'dired-shrink-to-fit
			"use the Customization interface to add a new rule
to `display-buffer-alist' where condition regexp is \"^ \\*Marked Files\\*$\",
action argument symbol is `window-height' and its value is nil." "24.3")

(defvar dired-file-version-alist)

;;;###autoload
(defvar dired-directory nil
  "The directory name or wildcard spec that this Dired directory lists.
Local to each Dired buffer.  May be a list, in which case the car is the
directory name and the cdr is the list of files to mention.
The directory name must be absolute, but need not be fully expanded.")

;; Beware of "-l;reboot" etc.  See bug#3230.
(defun dired-safe-switches-p (switches)
  "Return non-nil if string SWITCHES does not look risky for Dired."
  (or (not switches)
      (and (stringp switches)
           (< (length switches) 100)    ; arbitrary
           (string-match-p "\\` *-[- [:alnum:]]+\\'" switches))))

(defvar dired-actual-switches nil
  "The value of `dired-listing-switches' used to make this buffer's text.")

(put 'dired-actual-switches 'safe-local-variable 'dired-safe-switches-p)

(defvar dired-re-inode-size "[0-9 \t]*[.,0-9]*[BkKMGTPEZY]?[ \t]*"
  "Regexp for optional initial inode and file size as made by `ls -i -s'.")

;; These regexps must be tested at beginning-of-line, but are also
;; used to search for next matches, so neither omitting "^" nor
;; replacing "^" by "\n" (to make it slightly faster) will work.

(defvar dired-re-mark "^[^ \n]")
;; "Regexp matching a marked line.
;; Important: the match ends just after the marker."
(defvar dired-re-maybe-mark "^. ")
;; The [^:] part after "d" and "l" is to avoid confusion with the
;; DOS/Windows-style drive letters in directory names, like in "d:/foo".
(defvar dired-re-dir (concat dired-re-maybe-mark dired-re-inode-size "d[^:]"))
(defvar dired-re-sym (concat dired-re-maybe-mark dired-re-inode-size "l[^:]"))
(defvar dired-re-special (concat dired-re-maybe-mark dired-re-inode-size
                                 "[bcsp][^:]"))
(defvar dired-re-exe;; match ls permission string of an executable file
  (mapconcat (lambda (x)
	       (concat dired-re-maybe-mark dired-re-inode-size x))
	     '("-[-r][-w][xs][-r][-w].[-r][-w]."
	       "-[-r][-w].[-r][-w][xs][-r][-w]."
	       "-[-r][-w].[-r][-w].[-r][-w][xst]")
	     "\\|"))
(defvar dired-re-perms "[-bcdlps][-r][-w].[-r][-w].[-r][-w].")
(defvar dired-re-dot "^.* \\.\\.?/?$")

;; The subdirectory names in the next two lists are expanded.
(defvar dired-subdir-alist nil
  "Alist of listed directories and their buffer positions.
Alist elements have the form (DIRNAME . STARTMARKER), where
DIRNAME is the absolute name of the directory and STARTMARKER is
a marker at the beginning of DIRNAME.

The order of elements is the reverse of the order in the buffer.
If no subdirectories are listed then the alist contains only one
element, for the listed directory.")

(defvar-local dired-switches-alist nil
  "Keeps track of which switches to use for inserted subdirectories.
This is an alist of the form (SUBDIR . SWITCHES).")

(defvaralias 'dired-move-to-filename-regexp
  'directory-listing-before-filename-regexp)

(defvar dired-subdir-regexp "^. \\(.+\\)\\(:\\)\n"
  "Regexp matching a maybe hidden subdirectory line in `ls -lR' output.
Subexpression 1 is the subdirectory proper, no trailing colon.
The match starts at the beginning of the line and ends after the end
of the line.
Subexpression 2 must end right before the \\n.")


;;; Faces

(defgroup dired-faces nil
  "Faces used by Dired."
  :group 'dired
  :group 'faces)

(defface dired-header
  '((t (:inherit font-lock-type-face)))
  "Face used for directory headers."
  :group 'dired-faces
  :version "22.1")
(defvar dired-header-face 'dired-header
  "Face name used for directory headers.")

(defface dired-mark
  '((t (:inherit font-lock-constant-face)))
  "Face used for Dired marks."
  :group 'dired-faces
  :version "22.1")
(defvar dired-mark-face 'dired-mark
  "Face name used for Dired marks.")

(defface dired-marked
  '((t (:inherit warning)))
  "Face used for marked files."
  :group 'dired-faces
  :version "22.1")
(defvar dired-marked-face 'dired-marked
  "Face name used for marked files.")

(defface dired-flagged
  '((t (:inherit error)))
  "Face used for files flagged for deletion."
  :group 'dired-faces
  :version "22.1")
(defvar dired-flagged-face 'dired-flagged
  "Face name used for files flagged for deletion.")

(defface dired-warning
  ;; Inherit from font-lock-warning-face since with min-colors 8
  ;; font-lock-comment-face is not colored any more.
  '((t (:inherit font-lock-warning-face)))
  "Face used to highlight a part of a buffer that needs user attention."
  :group 'dired-faces
  :version "22.1")
(defvar dired-warning-face 'dired-warning
  "Face name used for a part of a buffer that needs user attention.")

(defface dired-perm-write
  '((((type w32 pc)) :inherit default)  ;; These default to rw-rw-rw.
    ;; Inherit from font-lock-comment-delimiter-face since with min-colors 8
    ;; font-lock-comment-face is not colored any more.
    (t (:inherit font-lock-comment-delimiter-face)))
  "Face used to highlight permissions of group- and world-writable files."
  :group 'dired-faces
  :version "22.2")
(defvar dired-perm-write-face 'dired-perm-write
  "Face name used for permissions of group- and world-writable files.")

(defface dired-set-id
  '((((type w32 pc)) :inherit default)  ;; These default to rw-rw-rw.
    (t (:inherit font-lock-warning-face)))
  "Face used to highlight permissions of suid and guid files."
  :group 'dired-faces
  :version "27.1")

(defface dired-directory
  '((t (:inherit font-lock-function-name-face)))
  "Face used for subdirectories."
  :group 'dired-faces
  :version "22.1")
(defvar dired-directory-face 'dired-directory
  "Face name used for subdirectories.")

(defface dired-symlink
  '((t (:inherit font-lock-keyword-face)))
  "Face used for symbolic links."
  :group 'dired-faces
  :version "22.1")
(defvar dired-symlink-face 'dired-symlink
  "Face name used for symbolic links.")

(defface dired-broken-symlink
  '((((class color))
     :foreground "yellow1" :background "red1" :weight bold)
    (t :weight bold :slant italic :underline t))
  "Face used for broken symbolic links."
  :group 'dired-faces
  :version "28.1")

(defface dired-special
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for sockets, pipes, block devices and char devices."
  :group 'dired-faces
  :version "27.1")

(defface dired-ignored
  '((t (:inherit shadow)))
  "Face used for files suffixed with `completion-ignored-extensions'."
  :group 'dired-faces
  :version "22.1")
(defvar dired-ignored-face 'dired-ignored
  "Face name used for files suffixed with `completion-ignored-extensions'.")


;;; Font-lock

(defvar dired-font-lock-keywords
  (list
   ;;
   ;; Dired marks.
   (list dired-re-mark '(0 dired-mark-face))
   ;;
   ;; We make heavy use of MATCH-ANCHORED, since the regexps don't identify the
   ;; file name itself.  We search for Dired defined regexps, and then use the
   ;; Dired defined function `dired-move-to-filename' before searching for the
   ;; simple regexp ".+".  It is that regexp which matches the file name.
   ;;
   ;; Marked files.
   (list (concat "^[" (char-to-string dired-marker-char) "]")
         '(".+" (dired-move-to-filename) nil (0 dired-marked-face)))
   ;;
   ;; Flagged files.
   (list (concat "^[" (char-to-string dired-del-marker) "]")
         '(".+" (dired-move-to-filename) nil (0 dired-flagged-face)))
   ;; People who are paranoid about security would consider this more
   ;; important than other things such as whether it is a directory.
   ;; But we don't want to encourage paranoia, so our default
   ;; should be what's most useful for non-paranoids. -- rms.
;;;   ;;
;;;   ;; Files that are group or world writable.
;;;   (list (concat dired-re-maybe-mark dired-re-inode-size
;;;		 "\\([-d]\\(....w....\\|.......w.\\)\\)")
;;;	 '(1 dired-warning-face)
;;;	 '(".+" (dired-move-to-filename) nil (0 dired-warning-face)))
   ;; However, we don't need to highlight the file name, only the
   ;; permissions, to win generally.  -- fx.
   ;; Fixme: we could also put text properties on the permission
   ;; fields with keymaps to frob the permissions, somewhat a la XEmacs.
   (list (concat dired-re-maybe-mark dired-re-inode-size
		 "[-d]....\\(w\\)....")	; group writable
	 '(1 dired-perm-write-face))
   (list (concat dired-re-maybe-mark dired-re-inode-size
		 "[-d].......\\(w\\).")	; world writable
	 '(1 dired-perm-write-face))
   (list (concat dired-re-maybe-mark dired-re-inode-size
		 "[-d]..\\(s\\)......")	; suid
	 '(1 'dired-set-id))
   (list (concat dired-re-maybe-mark dired-re-inode-size
		 "[-d].....\\([sS]\\)...")	; guid
	 '(1 'dired-set-id))
   ;;
   ;; Subdirectories.
   (list dired-re-dir
	 '(".+" (dired-move-to-filename) nil (0 dired-directory-face)))
   ;;
   ;; Files suffixed with `completion-ignored-extensions'.
   '(eval .
     ;; It is quicker to first find just an extension, then go back to the
     ;; start of that file name.  So we do this complex MATCH-ANCHORED form.
          (list (concat
                 "\\(" (regexp-opt completion-ignored-extensions)
                 "\\|#\\|\\.#.+\\)$")
	   '(".+" (dired-move-to-filename) nil (0 dired-ignored-face))))
   ;;
   ;; Files suffixed with `completion-ignored-extensions'
   ;; plus a character put in by -F.
   '(eval .
     (list (concat "\\(" (regexp-opt completion-ignored-extensions)
		   "\\|#\\|\\.#.+\\)[*=|]$")
	   '(".+" (progn
		    (end-of-line)
		    ;; If the last character is not part of the filename,
		    ;; move back to the start of the filename
		    ;; so it can be fontified.
		    ;; Otherwise, leave point at the end of the line;
		    ;; that way, nothing is fontified.
		    (unless (get-text-property (1- (point)) 'mouse-face)
		      (dired-move-to-filename)))
	     nil (0 dired-ignored-face))))
   ;;
   ;; Broken Symbolic link.
   (list dired-re-sym
         (list (lambda (end)
                 (let* ((file (dired-file-name-at-point))
                        (truename (ignore-errors (file-truename file))))
                   ;; either not existent target or circular link
                   (and (not (and truename (file-exists-p truename)))
                        (search-forward-regexp "\\(.+\\) \\(->\\) ?\\(.+\\)" end t))))
               '(dired-move-to-filename)
               nil
               '(1 'dired-broken-symlink)
               '(2 dired-symlink-face)
               '(3 'dired-broken-symlink)))
   ;;
   ;; Symbolic link to a directory.
   (list dired-re-sym
         (list (lambda (end)
                 (when-let* ((file (dired-file-name-at-point))
                             (truename (ignore-errors (file-truename file))))
                   (and (file-directory-p truename)
		        (search-forward-regexp "\\(.+-> ?\\)\\(.+\\)" end t))))
               '(dired-move-to-filename)
               nil
               '(1 dired-symlink-face)
               '(2 dired-directory-face)))
   ;;
   ;; Symbolic link to a non-directory.
   (list dired-re-sym
         (list (lambda (end)
                 (when-let ((file (dired-file-name-at-point)))
                   (let ((truename (ignore-errors (file-truename file))))
                     (and (or (not truename)
		              (not (file-directory-p truename)))
		          (search-forward-regexp "\\(.+-> ?\\)\\(.+\\)"
                                                 end t)))))
               '(dired-move-to-filename)
               nil
               '(1 dired-symlink-face)
               '(2 'default)))
   ;;
   ;; Sockets, pipes, block devices, char devices.
   (list dired-re-special
	 '(".+" (dired-move-to-filename) nil (0 'dired-special)))
   ;;
   ;; Explicitly put the default face on file names ending in a colon to
   ;; avoid fontifying them as directory header.
   (list (concat dired-re-maybe-mark dired-re-inode-size dired-re-perms ".*:$")
	 '(".+" (dired-move-to-filename) nil (0 'default)))
   ;;
   ;; Directory headers.
   (list dired-subdir-regexp '(1 dired-header-face))
   )
  "Additional expressions to highlight in Dired mode.")

(defvar dnd-protocol-alist)


;;; Macros

;; Macros must be defined before they are used, for the byte compiler.

(defmacro dired-mark-if (predicate msg)
  "Mark files for PREDICATE, according to `dired-marker-char'.
PREDICATE is evaluated on each line, with point at beginning of line.
MSG is a noun phrase for the type of files being marked.
It should end with a noun that can be pluralized by adding `s'.

In Transient Mark mode, if the mark is active, operate on the contents
of the region if `dired-mark-region' is non-nil.  Otherwise, operate
on the whole buffer.

Return value is the number of files marked, or nil if none were marked."
  `(let ((inhibit-read-only t) count
         (use-region-p (dired-mark--region-use-p))
         (beg (dired-mark--region-beginning))
         (end (dired-mark--region-end)))
    (save-excursion
      (setq count 0)
      (when ,msg
	(message "%s %ss%s%s..."
		 (cond ((eq dired-marker-char ?\s) "Unmarking")
		       ((eq dired-del-marker dired-marker-char)
			"Flagging")
		       (t "Marking"))
		 ,msg
		 (if (eq dired-del-marker dired-marker-char)
		     " for deletion"
		   "")
                 (if use-region-p
                     " in region"
                   "")))
      (goto-char beg)
      (while (< (point) end)
        (when ,predicate
          (unless (= (following-char) dired-marker-char)
            (delete-char 1)
            (insert dired-marker-char)
            (setq count (1+ count))))
        (forward-line 1))
      (when ,msg (message "%s %s%s %s%s%s"
                        count
                        ,msg
                        (dired-plural-s count)
                        (if (eq dired-marker-char ?\s) "un" "")
                        (if (eq dired-marker-char dired-del-marker)
                            "flagged" "marked")
                        (if use-region-p
                            " in region"
                          ""))))
    (and (> count 0) count)))

(defmacro dired-map-over-marks (body arg &optional show-progress
				     distinguish-one-marked)
  "Eval BODY with point on each marked line.  Return a list of BODY's results.
If no marked file could be found, execute BODY on the current
line.  ARG, if non-nil, specifies the files to use instead of the
marked files.

If ARG is an integer, use the next ARG (or previous -ARG, if
ARG<0) files.  In that case, point is dragged along.  This is so
that commands on the next ARG (instead of the marked) files can
be chained easily.
For any other non-nil value of ARG, use the current file.

If optional third arg SHOW-PROGRESS evaluates to non-nil,
redisplay the dired buffer after each file is processed.

No guarantee is made about the position on the marked line.
BODY must ensure this itself if it depends on this.

Search starts at the beginning of the buffer, thus the car of the
list corresponds to the line nearest to the buffer's bottom.
This is also true for (positive and negative) integer values of
ARG.

BODY should not be too long as it is expanded four times.

If DISTINGUISH-ONE-MARKED is non-nil, then if we find just one
marked file, return (t FILENAME) instead of (FILENAME)."
  ;;
  ;;Warning: BODY must not add new lines before point - this may cause an
  ;;endless loop.
  ;;This warning should not apply any longer, sk  2-Sep-1991 14:10.
  `(prog1
       (let ((inhibit-read-only t) case-fold-search found results)
	 (if ,arg
	     (if (integerp ,arg)
		 (progn	;; no save-excursion, want to move point.
		   (dired-repeat-over-lines
		    ,arg
		    (lambda ()
		      (if ,show-progress (sit-for 0))
		      (setq results (cons ,body results))))
		   (if (< ,arg 0)
		       (nreverse results)
		     results))
	       ;; non-nil, non-integer ARG means use current file:
	       (list ,body))
	   (let ((regexp (dired-marker-regexp)) next-position)
	     (save-excursion
	       (goto-char (point-min))
	       ;; remember position of next marked file before BODY
	       ;; can insert lines before the just found file,
	       ;; confusing us by finding the same marked file again
	       ;; and again and...
	       (setq next-position (and (re-search-forward regexp nil t)
					(point-marker))
		     found (not (null next-position)))
	       (while next-position
		 (goto-char next-position)
		 (if ,show-progress (sit-for 0))
		 (setq results (cons ,body results))
		 ;; move after last match
		 (goto-char next-position)
		 (forward-line 1)
		 (set-marker next-position nil)
		 (setq next-position (and (re-search-forward regexp nil t)
					  (point-marker)))))
	     (if (and ,distinguish-one-marked (= (length results) 1))
		 (setq results (cons t results)))
	     (if found
		 results
	       (list ,body)))))
     ;; save-excursion loses, again
     (dired-move-to-filename)))

(defun dired-get-marked-files (&optional localp arg filter distinguish-one-marked error)
  "Return the marked files' names as list of strings.
The list is in the same order as the buffer, that is, the car is the
  first marked file.
Values returned are normally absolute file names.
Optional arg LOCALP as in `dired-get-filename'.
Optional second argument ARG, if non-nil, specifies files near
 point instead of marked files.  It usually comes from the prefix
 argument.
  If ARG is an integer, use the next ARG files.
  If ARG is any other non-nil value, return the current file name.
  If no files are marked, and ARG is nil, also return the current file name.
Optional third argument FILTER, if non-nil, is a function to select
  some of the files--those for which (funcall FILTER FILENAME) is non-nil.

If DISTINGUISH-ONE-MARKED is non-nil, then if we find just one marked file,
return (t FILENAME) instead of (FILENAME).
Don't use that together with FILTER.

If ERROR is non-nil, signal an error when the list of found files is empty.
ERROR can be a string with the error message."
  (let ((all-of-them
	 (save-excursion
	   (delq nil (dired-map-over-marks
		      (dired-get-filename localp 'no-error-if-not-filep)
		      arg nil distinguish-one-marked))))
	result)
    (when (equal all-of-them '(t))
      (setq all-of-them nil))
    (if (not filter)
	(setq result
              (if (and distinguish-one-marked (eq (car all-of-them) t))
	          all-of-them
	        (nreverse all-of-them)))
      (dolist (file all-of-them)
	(if (funcall filter file)
	    (push file result))))
    (when (and (null result) error)
      (user-error (if (stringp error) error "No files specified")))
    result))

(defun dired-mark--region-use-p ()
  "Whether Dired marking commands should act on region."
  (and dired-mark-region
       (region-active-p)
       (> (region-end) (region-beginning))))

(defun dired-mark--region-beginning ()
  "Return the value of the region beginning aligned to Dired file lines."
  (if (dired-mark--region-use-p)
      (save-excursion
        (goto-char (region-beginning))
        (line-beginning-position))
    (point-min)))

(defun dired-mark--region-end ()
  "Return the value of the region end aligned to Dired file lines."
  (if (dired-mark--region-use-p)
      (save-excursion
        (goto-char (region-end))
        (if (if (eq dired-mark-region 'line)
                (not (bolp))
              (get-text-property (1- (point)) 'dired-filename))
            (line-end-position)
          (line-beginning-position)))
    (point-max)))


;;; The dired command

(defun dired-read-dir-and-switches (str)
  ;; For use in interactive.
  (reverse (list
	    (if current-prefix-arg
		(read-string "Dired listing switches: "
			     dired-listing-switches))
	    ;; If a dialog is used, call `read-directory-name' so the
	    ;; dialog code knows we want directories.  Some dialogs
	    ;; can only select directories or files when popped up,
	    ;; not both.  If no dialog is used, call `read-file-name'
	    ;; because the user may want completion of file names for
	    ;; use in a wildcard pattern.
	    (if (next-read-file-uses-dialog-p)
		(read-directory-name (format "Dired %s(directory): " str)
				     nil default-directory nil)
	      (read-file-name (format "Dired %s(directory): " str)
			      nil default-directory nil)))))

;; We want to switch to a more sophisticated version of
;; dired-read-dir-and-switches like the following, if there is a way
;; to make it more intuitive.  See bug#1285.

;; (defun dired-read-dir-and-switches (str)
;;   ;; For use in interactive.
;;   (reverse
;;    (list
;;     (if current-prefix-arg
;;         (read-string "Dired listing switches: "
;;                      dired-listing-switches))
;;     ;; If a dialog is about to be used, call read-directory-name so
;;     ;; the dialog code knows we want directories.  Some dialogs can
;;     ;; only select directories or files when popped up, not both.
;;     (if (next-read-file-uses-dialog-p)
;;         (read-directory-name (format "Dired %s(directory): " str)
;;                              nil default-directory nil)
;;       (let ((cie ()))
;;         (dolist (ext completion-ignored-extensions)
;;           (if (eq ?/ (aref ext (1- (length ext)))) (push ext cie)))
;;         (setq cie (concat (regexp-opt cie "\\(?:") "\\'"))
;;         (let* ((default (and buffer-file-name
;;                              (abbreviate-file-name buffer-file-name)))
;;                (cie cie)
;;                (completion-table
;;                 ;; We need a mix of read-file-name and
;;                 ;; read-directory-name so that completion to directories
;;                 ;; is preferred, but if the user wants to enter a global
;;                 ;; pattern, he can still use completion on filenames to
;;                 ;; help him write the pattern.
;;                 ;; Essentially, we want to use
;;                 ;; (completion-table-with-predicate
;;                 ;;  'read-file-name-internal 'file-directory-p nil)
;;                 ;; but that doesn't work because read-file-name-internal
;;                 ;; does not obey its `predicate' argument.
;;                 (completion-table-in-turn
;;                  (lambda (str pred action)
;;                    (let ((read-file-name-predicate
;;                           (lambda (f)
;;                             (and (not (member f '("./" "../")))
;;                                  ;; Hack! Faster than file-directory-p!
;;                                  (eq (aref f (1- (length f))) ?/)
;;                                  (not (string-match cie f))))))
;;                      (complete-with-action
;;                       action 'read-file-name-internal str nil)))
;;                  'read-file-name-internal)))
;;           (minibuffer-with-setup-hook
;;               (lambda ()
;;                 (setq minibuffer-default default)
;;                 (setq minibuffer-completion-table completion-table))
;;             (read-file-name (format "Dired %s(directory): " str)
;;                             nil default-directory nil))))))))

(defun dired-file-name-at-point ()
  "Try to get a file name at point in the current dired buffer.
This hook is intended to be put in `file-name-at-point-functions'.
Note that it returns an abbreviated name that can't be used
as an argument to `dired-goto-file'."
  (let ((filename (dired-get-filename nil t)))
    (when filename
      (if (file-directory-p filename)
	  (file-name-as-directory (abbreviate-file-name filename))
	(abbreviate-file-name filename)))))

(defun dired-grep-read-files ()
  "Use file at point as the file for grep's default file-name pattern suggestion.
If a directory or nothing is found at point, return nil."
  (let ((file-name (dired-file-name-at-point)))
    (if (and file-name
	     (not (file-directory-p file-name)))
	file-name)))

;;;###autoload (define-key ctl-x-map "d" 'dired)
;;;###autoload
(defun dired (dirname &optional switches)
  "\"Edit\" directory DIRNAME--delete, rename, print, etc. some files in it.
Optional second argument SWITCHES specifies the `ls' options used.
\(Interactively, use a prefix argument to be able to specify SWITCHES.)

If DIRNAME is a string, Dired displays a list of files in DIRNAME (which
may also have shell wildcards appended to select certain files).

If DIRNAME is a cons, its first element is taken as the directory name
and the rest as an explicit list of files to make directory entries for.
In this case, SWITCHES are applied to each of the files separately, and
therefore switches that control the order of the files in the produced
listing have no effect.

\\<dired-mode-map>\
You can flag files for deletion with \\[dired-flag-file-deletion] and then
delete them by typing \\[dired-do-flagged-delete].
Type \\[describe-mode] after entering Dired for more info.

If DIRNAME is already in a Dired buffer, that buffer is used without refresh."
  ;; Cannot use (interactive "D") because of wildcards.
  (interactive (dired-read-dir-and-switches ""))
  (pop-to-buffer-same-window (dired-noselect dirname switches)))

;;;###autoload (define-key ctl-x-4-map "d" 'dired-other-window)
;;;###autoload
(defun dired-other-window (dirname &optional switches)
  "\"Edit\" directory DIRNAME.  Like `dired' but select in another window."
  (interactive (dired-read-dir-and-switches "in other window "))
  (switch-to-buffer-other-window (dired-noselect dirname switches)))

;;;###autoload (define-key ctl-x-5-map "d" 'dired-other-frame)
;;;###autoload
(defun dired-other-frame (dirname &optional switches)
  "\"Edit\" directory DIRNAME.  Like `dired' but make a new frame."
  (interactive (dired-read-dir-and-switches "in other frame "))
  (switch-to-buffer-other-frame (dired-noselect dirname switches)))

;;;###autoload (define-key tab-prefix-map "d" 'dired-other-tab)
;;;###autoload
(defun dired-other-tab (dirname &optional switches)
  "\"Edit\" directory DIRNAME.  Like `dired' but make a new tab."
  (interactive (dired-read-dir-and-switches "in other tab "))
  (switch-to-buffer-other-tab (dired-noselect dirname switches)))

;;;###autoload
(defun dired-noselect (dir-or-list &optional switches)
  "Like `dired' but return the Dired buffer as value, do not select it."
  (or dir-or-list (setq dir-or-list default-directory))
  ;; This loses the distinction between "/foo/*/" and "/foo/*" that
  ;; some shells make:
  (let (dirname initially-was-dirname)
    (if (consp dir-or-list)
	(setq dirname (car dir-or-list))
      (setq dirname dir-or-list))
    (setq initially-was-dirname
	  (string= (file-name-as-directory dirname) dirname))
    (setq dirname (abbreviate-file-name
		   (expand-file-name (directory-file-name dirname))))
    (if find-file-visit-truename
	(setq dirname (file-truename dirname)))
    ;; If the argument was syntactically  a directory name not a file name,
    ;; or if it happens to name a file that is a directory,
    ;; convert it syntactically to a directory name.
    ;; The reason for checking initially-was-dirname
    ;; and not just file-directory-p
    ;; is that file-directory-p is slow over ftp.
    (if (or initially-was-dirname (file-directory-p dirname))
	(setq dirname  (file-name-as-directory dirname)))
    (if (consp dir-or-list)
	(setq dir-or-list (cons dirname (cdr dir-or-list)))
      (setq dir-or-list dirname))
    (dired-internal-noselect dir-or-list switches)))

;; The following is an internal dired function.  It returns non-nil if
;; the directory visited by the current dired buffer has changed on
;; disk.  DIRNAME should be the directory name of that directory.
(defun dired-directory-changed-p (dirname)
  (not (let ((attributes (file-attributes dirname))
	     (modtime (visited-file-modtime)))
	 (or (eq modtime 0)
	     (not (eq (file-attribute-type attributes) t))
	     (equal (file-attribute-modification-time attributes) modtime)))))

(defvar auto-revert-remote-files)

(defun dired-buffer-stale-p (&optional noconfirm)
  "Return non-nil if current Dired buffer needs updating.
If NOCONFIRM is non-nil, then this function returns nil for a
remote directory, unless `auto-revert-remote-files' is non-nil.
This feature is used by Auto Revert mode."
  (let ((dirname
	 (if (consp dired-directory) (car dired-directory) dired-directory)))
    (and (stringp dirname)
	 (not (when noconfirm (and (not auto-revert-remote-files)
                                   (file-remote-p dirname))))
	 (file-readable-p dirname)
	 ;; Do not auto-revert when the dired buffer can be currently
	 ;; written by the user as in `wdired-mode'.
	 buffer-read-only
	 (dired-directory-changed-p dirname))))

(defcustom dired-auto-revert-buffer nil
  "Automatically revert Dired buffers on revisiting their directory.
This option controls whether to refresh the directory listing in a
Dired buffer when the directory that is already in some Dired buffer
is revisited by commands such as \\[dired] and \\[dired-find-file].
If the value is t, revisiting an existing Dired buffer always reverts it.
If the value is a function, it is called with the directory name as a
single argument, and the buffer is reverted if the function returns non-nil.
One such function is `dired-directory-changed-p', which returns non-nil
if the directory has been changed since it was last revisited.
Otherwise, Emacs prompts whether to revert the changed Dired buffer.
Note that this is not the same as `auto-revert-mode' that
periodically reverts at specified time intervals."
  :type '(choice
          (const :tag "Don't revert" nil)
          (const :tag "Always revert visited Dired buffer" t)
          (const :tag "Revert changed Dired buffer" dired-directory-changed-p)
          (function :tag "Predicate function"))
  :group 'dired
  :version "23.2")

(defun dired--need-align-p ()
  "Return non-nil if some file names are misaligned.
The return value is the target column for the file names."
  (save-excursion
    (goto-char (point-min))
    (dired-goto-next-file)
    ;; Use point difference instead of `current-column', because
    ;; the former works when `dired-hide-details-mode' is enabled.
    (let* ((first (- (point) (point-at-bol)))
           (target first))
      (while (and (not (eobp))
                  (progn
                    (forward-line)
                    (dired-move-to-filename)))
        (when-let* ((distance (- (point) (point-at-bol)))
                    (higher (> distance target)))
          (setq target distance)))
      (and (/= first target) target))))

(defun dired--align-all-files ()
  "Align all files adding spaces in front of the size column."
  (let ((target (dired--need-align-p))
        (regexp directory-listing-before-filename-regexp))
    (when target
      (save-excursion
        (goto-char (point-min))
        (dired-goto-next-file)
        (while (dired-move-to-filename)
          ;; Use point difference instead of `current-column', because
          ;; the former works when `dired-hide-details-mode' is enabled.
          (let ((distance (- target (- (point) (point-at-bol))))
                (inhibit-read-only t))
            (unless (zerop distance)
              (re-search-backward regexp nil t)
              (goto-char (match-beginning 0))
              (search-backward-regexp "[[:space:]]" nil t)
              (skip-chars-forward "[:space:]")
              (insert-char ?\s distance 'inherit))
            (forward-line)))))))

(defun dired-internal-noselect (dir-or-list &optional switches mode)
  ;; If DIR-OR-LIST is a string and there is an existing dired buffer
  ;; for it, just leave buffer as it is (don't even call dired-revert).
  ;; This saves time especially for deep trees or with ange-ftp.
  ;; The user can type `g' easily, and it is more consistent with find-file.
  ;; But if SWITCHES are given they are probably different from the
  ;; buffer's old value, so call dired-sort-other, which does
  ;; revert the buffer.
  ;; Revert the buffer if DIR-OR-LIST is a cons or `dired-directory'
  ;; is a cons and DIR-OR-LIST is a string.
  ;; A pity we can't possibly do "Directory has changed - refresh? "
  ;; like find-file does.
  ;; Optional argument MODE is passed to dired-find-buffer-nocreate,
  ;; see there.
  (let* ((old-buf (current-buffer))
	 (dirname (if (consp dir-or-list) (car dir-or-list) dir-or-list))
         ;; Look for an existing buffer.
         (buffer (dired-find-buffer-nocreate dirname mode))
	 ;; Note that buffer already is in dired-mode, if found.
	 (new-buffer-p (null buffer)))
    (or buffer
        (setq buffer (create-file-buffer (directory-file-name dirname))))
    (set-buffer buffer)
    (if (not new-buffer-p)		; existing buffer ...
	(cond (switches			; ... but new switches
	       ;; file list may have changed
	       (setq dired-directory dir-or-list)
	       ;; this calls dired-revert
	       (dired-sort-other switches))
	      ;; Always revert when `dir-or-list' is a cons.  Also revert
	      ;; if `dired-directory' is a cons but `dir-or-list' is not.
	      ((or (consp dir-or-list) (consp dired-directory))
	       (setq dired-directory dir-or-list)
	       (revert-buffer))
	      ;; Always revert regardless of whether it has changed or not.
	      ((eq dired-auto-revert-buffer t)
	       (revert-buffer))
	      ;; Revert when predicate function returns non-nil.
	      ((functionp dired-auto-revert-buffer)
	       (when (funcall dired-auto-revert-buffer dirname)
		 (revert-buffer)
		 (message "Changed directory automatically updated")))
	      ;; If directory has changed on disk, offer to revert.
	      ((when (dired-directory-changed-p dirname)
		 (message "%s"
			  (substitute-command-keys
			   "Directory has changed on disk; type \\[revert-buffer] to update Dired")))))
      ;; Else a new buffer
      (setq default-directory
            (or (car-safe (insert-directory-wildcard-in-dir-p dirname))
	        ;; We can do this unconditionally
	        ;; because dired-noselect ensures that the name
	        ;; is passed in directory name syntax
	        ;; if it was the name of a directory at all.
	        (file-name-directory dirname)))
      (or switches (setq switches dired-listing-switches))
      (if mode (funcall mode)
        (dired-mode dir-or-list switches))
      ;; default-directory and dired-actual-switches are set now
      ;; (buffer-local), so we can call dired-readin:
      (let ((failed t))
	(unwind-protect
	    (progn (dired-readin)
		   (setq failed nil))
	  ;; dired-readin can fail if parent directories are inaccessible.
	  ;; Don't leave an empty buffer around in that case.
	  (if failed (kill-buffer buffer))))
      (goto-char (point-min))
      (dired-initial-position dirname))
    (when (consp dired-directory)
      (dired--align-all-files))
    (set-buffer old-buf)
    buffer))

(defvar dired-buffers nil
  ;; Enlarged by dired-advertise
  ;; Queried by function dired-buffers-for-dir. When this detects a
  ;; killed buffer, it is removed from this list.
  "Alist of expanded directories and their associated Dired buffers.")

(defvar dired-find-subdir)

;; FIXME add a doc-string, and document dired-x extensions.
(defun dired-find-buffer-nocreate (dirname &optional mode)
  ;; This differs from dired-buffers-for-dir in that it does not consider
  ;; subdirs of default-directory and searches for the first match only.
  ;; Also, the major mode must be MODE.
  (if (and (featurep 'dired-x)
           dired-find-subdir
           ;; Don't try to find a wildcard as a subdirectory.
	   (string-equal dirname (file-name-directory dirname)))
      (let* ((cur-buf (current-buffer))
	     (buffers (nreverse
		       (dired-buffers-for-dir (expand-file-name dirname))))
	     (cur-buf-matches (and (memq cur-buf buffers)
				   ;; Wildcards must match, too:
				   (equal dired-directory dirname))))
	;; We don't want to switch to the same buffer---
	(setq buffers (delq cur-buf buffers))
	(or (car (sort buffers #'dired-buffer-more-recently-used-p))
	    ;; ---unless it's the only possibility:
	    (and cur-buf-matches cur-buf)))
    ;; No dired-x, or dired-find-subdir nil.
    (setq dirname (expand-file-name dirname))
    (let (found (blist dired-buffers))    ; was (buffer-list)
      (or mode (setq mode 'dired-mode))
      (while blist
        (if (null (buffer-name (cdr (car blist))))
            (setq blist (cdr blist))
          (with-current-buffer (cdr (car blist))
            (if (and (eq major-mode mode)
                     dired-directory  ;; nil during find-alternate-file
                     (equal dirname
                            (expand-file-name
                             (if (consp dired-directory)
                                 (car dired-directory)
                               dired-directory))))
                (setq found (cdr (car blist))
                      blist nil)
              (setq blist (cdr blist))))))
      found)))


;;; Read in a new dired buffer

(defun dired-readin ()
  "Read in a new Dired buffer.
Differs from `dired-insert-subdir' in that it accepts
wildcards, erases the buffer, and builds the subdir-alist anew
\(including making it buffer-local and clearing it first)."

  ;; default-directory and dired-actual-switches must be buffer-local
  ;; and initialized by now.
  (let ((dirname
	 (expand-file-name
	  (if (consp dired-directory)
	      (car dired-directory)
	    dired-directory))))
    (save-excursion
      ;; This hook which may want to modify dired-actual-switches
      ;; based on dired-directory, e.g. with ange-ftp to a SysV host
      ;; where ls won't understand -Al switches.
      (run-hooks 'dired-before-readin-hook)
      (if (consp buffer-undo-list)
	  (setq buffer-undo-list nil))
      (setq-local file-name-coding-system
                  (or coding-system-for-read file-name-coding-system))
      (widen)
      ;; We used to bind `inhibit-modification-hooks' to try and speed up
      ;; execution, in particular, to prevent the font-lock hook from running
      ;; until the directory is all read in.
      ;; It's not clear why font-lock would be a significant issue
      ;; here, but I used `combine-change-calls' which should provide the
      ;; same performance advantages without the problem of breaking
      ;; users of after/before-change-functions.
      (combine-change-calls (point-min) (point-max)
	(let ((inhibit-read-only t)
	      ;; Don't make undo entries for readin.
	      (buffer-undo-list t))
	  (erase-buffer)
	  (dired-readin-insert))
	(goto-char (point-min))
	;; Must first make alist buffer local and set it to nil because
	;; dired-build-subdir-alist will call dired-clear-alist first
	(setq-local dired-subdir-alist nil)
	(dired-build-subdir-alist))
      (let ((attributes (file-attributes dirname)))
	(if (eq (car attributes) t)
	    (set-visited-file-modtime (file-attribute-modification-time
                                       attributes))))
      (set-buffer-modified-p nil)
      ;; No need to narrow since the whole buffer contains just
      ;; dired-readin's output, nothing else.  The hook can
      ;; successfully use dired functions (e.g. dired-get-filename)
      ;; as the subdir-alist has been built in dired-readin.
      (run-hooks 'dired-after-readin-hook))))

;; Subroutines of dired-readin

(defun dired-readin-insert ()
  ;; Insert listing for the specified dir (and maybe file list)
  ;; already in dired-directory, assuming a clean buffer.
  (let (dir file-list)
    (if (consp dired-directory)
	(setq dir (car dired-directory)
	      file-list (cdr dired-directory))
      (setq dir dired-directory
	    file-list nil))
    (setq dir (expand-file-name dir))
    (if (and (equal "" (file-name-nondirectory dir))
	     (not file-list))
	;; If we are reading a whole single directory...
	(dired-insert-directory dir dired-actual-switches nil nil t)
      (if (and (not (insert-directory-wildcard-in-dir-p dir))
               (not (file-readable-p
		     (directory-file-name (file-name-directory dir)))))
	  (error "Directory %s inaccessible or nonexistent" dir))
      ;; Else treat it as a wildcard spec
      ;; unless we have an explicit list of files.
      (dired-insert-directory dir dired-actual-switches
			      file-list (not file-list) t))))

(defun dired-align-file (beg end)
  "Align the fields of a file to the ones of surrounding lines.
BEG..END is the line where the file info is located."
  ;; Some versions of ls try to adjust the size of each field so as to just
  ;; hold the largest element ("largest" in the current invocation, of
  ;; course).  So when a single line is output, the size of each field is
  ;; just big enough for that one output.  Thus when dired refreshes one
  ;; line, the alignment if this line w.r.t the rest is messed up because
  ;; the fields of that one line will generally be smaller.
  ;;
  ;; To work around this problem, we here add spaces to try and
  ;; re-align the fields as needed.  Since this is purely aesthetic,
  ;; it is of utmost importance that it doesn't mess up anything like
  ;; `dired-move-to-filename'.  To this end, we limit ourselves to
  ;; adding spaces only, and to only add them at places where there
  ;; was already at least one space.  This way, as long as
  ;; `directory-listing-before-filename-regexp' always matches spaces
  ;; with "*" or "+", we know we haven't made anything worse.  There
  ;; is one spot where the exact number of spaces is important, which
  ;; is just before the actual filename, so we refrain from adding
  ;; spaces there (and within the filename as well, of course).
  (save-excursion
    (let (file file-col other other-col)
      ;; Check that there is indeed a file, and that there is another adjacent
      ;; file with which to align, and that additional spaces are needed to
      ;; align the filenames.
      (when (and (setq file (progn (goto-char beg)
				   (dired-move-to-filename nil end)))
		 (setq file-col (current-column))
		 (setq other
		       (or (and (goto-char beg)
				(zerop (forward-line -1))
				(dired-move-to-filename))
			   (and (goto-char beg)
				(zerop (forward-line 1))
				(dired-move-to-filename))))
		 (setq other-col (current-column))
		 (/= file other)
		 ;; Make sure there is some work left to do.
		 (> other-col file-col))
	;; If we've only looked at the line above, check to see if the line
	;; below exists as well and if so, align with the shorter one.
	(when (and (< other file)
		   (goto-char beg)
		   (zerop (forward-line 1))
		   (dired-move-to-filename))
	  (let ((alt-col (current-column)))
	    (when (< alt-col other-col)
	      (setq other-col alt-col)
	      (setq other (point)))))
	;; Keep positions uptodate when we insert stuff.
	(if (> other file) (setq other (copy-marker other)))
	(setq file (copy-marker file))
	;; Main loop.
	(goto-char beg)
	(skip-chars-forward " ")	;Skip to the first field.
	(while (and (> other-col file-col)
		    ;; Don't touch anything just before (and after) the
		    ;; beginning of the filename.
		    (> file (point)))
	  ;; We're now just in front of a field, with a space behind us.
	  (let* ((curcol (current-column))
		 ;; Nums are right-aligned.
		 (num-align (looking-at-p "[0-9]"))
		 ;; Let's look at the other line, in the same column: we
		 ;; should be either near the end of the previous field, or
		 ;; in the space between that field and the next.
		 ;; [ Of course, it's also possible that we're already within
		 ;; the next field or even past it, but that's unlikely since
		 ;; other-col > file-col. ]
		 ;; Let's find the distance to the alignment-point (either
		 ;; the beginning or the end of the next field, depending on
		 ;; whether this field is left or right aligned).
		 (align-pt-offset
		  (save-excursion
		    (goto-char other)
		    (move-to-column curcol)
		    (when (looking-at
			   (concat
			    (if (eq (char-before) ?\s) " *" "[^ ]* *")
			    (if num-align "[0-9][^ ]*")))
		      (- (match-end 0) (match-beginning 0)))))
		 ;; Now, the number of spaces to insert is align-pt-offset
		 ;; minus the distance to the equivalent point on the
		 ;; current line.
		 (spaces
		  (if (not num-align)
		      align-pt-offset
		    (and align-pt-offset
			 (save-excursion
			   (skip-chars-forward "^ ")
			   (- align-pt-offset (- (current-column) curcol)))))))
	    (when (and spaces (> spaces 0))
	      (setq file-col (+ spaces file-col))
	      (if (> file-col other-col)
		  (setq spaces (- spaces (- file-col other-col))))
	      (insert-char ?\s spaces 'inherit)
	      ;; Let's just make really sure we did not mess up.
	      (unless (save-excursion
			(eq (dired-move-to-filename) (marker-position file)))
		;; Damn!  We messed up: let's revert the change.
		(delete-char (- spaces)))))
	  ;; Now skip to next field.
	  (skip-chars-forward "^ ") (skip-chars-forward " "))
	(set-marker file nil)))))


(defvar ls-lisp-use-insert-directory-program)

(defun dired-check-switches (switches short &optional long)
  "Return non-nil if the string SWITCHES matches LONG or SHORT format."
  (let (case-fold-search)
    (and (stringp switches)
	 (string-match-p (concat "\\(\\`\\| \\)-[[:alnum:]]*" short
				 (if long (concat "\\|--" long "\\>") ""))
			 switches))))

(defun dired-switches-escape-p (switches)
  "Return non-nil if the string SWITCHES contains -b or --escape."
  ;; Do not match things like "--block-size" that happen to contain "b".
  (dired-check-switches switches "b" "escape"))

(defun dired-switches-recursive-p (switches)
  "Return non-nil if the string SWITCHES contains -R or --recursive."
  (dired-check-switches switches "R" "recursive"))

(defun dired-insert-directory (dir switches &optional file-list wildcard hdr)
  "Insert a directory listing of DIR, Dired style.
Use SWITCHES to make the listings.
If FILE-LIST is non-nil, list only those files.
Otherwise, if WILDCARD is non-nil, expand wildcards;
 in that case, DIR should be a file name that uses wildcards.
In other cases, DIR should be a directory name or a directory filename.
If HDR is non-nil, insert a header line with the directory name."
  (let ((opoint (point))
	(process-environment (copy-sequence process-environment))
	end)
    (if (and
	 ;; Don't try to invoke `ls' if we are on DOS/Windows where
	 ;; ls-lisp emulation is used, except if they want to use `ls'
	 ;; as indicated by `ls-lisp-use-insert-directory-program'.
	 (not (and (featurep 'ls-lisp)
		   (null ls-lisp-use-insert-directory-program)))
         ;; FIXME: Big ugly hack for Eshell's eshell-ls-use-in-dired.
         (not (bound-and-true-p eshell-ls-use-in-dired))
	 (or (file-remote-p dir)
             (if (eq dired-use-ls-dired 'unspecified)
		 ;; Check whether "ls --dired" gives exit code 0, and
		 ;; save the answer in `dired-use-ls-dired'.
		 (or (setq dired-use-ls-dired
			   (eq 0 (call-process insert-directory-program
                                               nil nil nil "--dired")))
		     (progn
		       (message "ls does not support --dired; \
see `dired-use-ls-dired' for more details.")
		       nil))
	       dired-use-ls-dired)))
	(setq switches (concat "--dired " switches)))
    ;; Expand directory wildcards and fill file-list.
    (let ((dir-wildcard (insert-directory-wildcard-in-dir-p dir)))
      (cond (dir-wildcard
             (setq switches (concat "-d " switches))
             ;; We don't know whether the remote ls supports
             ;; "--dired", so we cannot add it to the `process-file'
             ;; call for wildcards.
             (when (file-remote-p dir)
               (setq switches (string-replace "--dired" "" switches)))
             (let* ((default-directory (car dir-wildcard))
                    (script (format "ls %s %s" switches (cdr dir-wildcard)))
                    (remotep (file-remote-p dir))
                    (sh (or (and remotep "/bin/sh")
                            (executable-find shell-file-name)
                            (executable-find "sh")))
                    (switch (if remotep "-c" shell-command-switch)))
               ;; Enable globstar
               (when-let ((globstar dired-maybe-use-globstar)
                          (enable-it
                           (assoc-default
                            (file-truename sh) dired-enable-globstar-in-shell
                            (lambda (reg shell) (string-match reg shell)))))
                 (setq script (format "%s; %s" enable-it script)))
               (unless
                   (zerop
                    (process-file sh nil (current-buffer) nil switch script))
                 (user-error
                  "%s: No files matching wildcard" (cdr dir-wildcard)))
               (insert-directory-clean (point) switches)))
            (t
             ;; We used to specify the C locale here, to force English
             ;; month names; but this should not be necessary any
             ;; more, with the new value of
             ;; `directory-listing-before-filename-regexp'.
             (if file-list
	         (dolist (f file-list)
	           (let ((beg (point)))
	             (insert-directory f switches nil nil)
	             ;; Re-align fields, if necessary.
	             (dired-align-file beg (point))))
               (insert-directory dir switches wildcard (not wildcard))))))
    ;; Quote certain characters, unless ls quoted them for us.
    (if (not (dired-switches-escape-p dired-actual-switches))
	(save-excursion
	  (setq end (point-marker))
	  (goto-char opoint)
	  (while (search-forward "\\" end t)
	    (replace-match (apply #'propertize
				  "\\\\"
				  (text-properties-at (match-beginning 0)))
			   nil t))
	  (goto-char opoint)
	  (while (search-forward "\^m" end t)
	    (replace-match (apply #'propertize
				  "\\015"
				  (text-properties-at (match-beginning 0)))
			   nil t))
	  (set-marker end nil))
      ;; Replace any newlines in DIR with literal "\n"s, for the sake
      ;; of the header line.  To disambiguate a literal "\n" in the
      ;; actual dirname, we also replace "\" with "\\".
      ;; Personally, I think this should always be done, irrespective
      ;; of the value of dired-actual-switches, because:
      ;;  i) Dired simply does not work with an unescaped newline in
      ;;  the directory name used in the header (bug=10469#28), and
      ;;  ii) "\" is always replaced with "\\" in the listing, so doing
      ;;  it in the header as well makes things consistent.
      ;; But at present it is only done if "-b" is in ls-switches,
      ;; because newlines in dirnames are uncommon, and people may
      ;; have gotten used to seeing unescaped "\" in the headers.
      ;; Note: adjust dired-build-subdir-alist if you change this.
      (setq dir (string-replace "\\" "\\\\" dir)
            dir (string-replace "\n" "\\n" dir)))
    ;; If we used --dired and it worked, the lines are already indented.
    ;; Otherwise, indent them.
    (unless (save-excursion
	      (goto-char opoint)
	      (looking-at-p "  "))
      (let ((indent-tabs-mode nil))
	(indent-rigidly opoint (point) 2)))
    ;; Insert text at the beginning to standardize things.
    (let ((content-point opoint))
      (save-excursion
	(goto-char opoint)
	(when (and (or hdr wildcard)
		   (not (and (looking-at "^  \\(.*\\):$")
			     (file-name-absolute-p (match-string 1)))))
	  ;; Note that dired-build-subdir-alist will replace the name
	  ;; by its expansion, so it does not matter whether what we insert
	  ;; here is fully expanded, but it should be absolute.
	  (insert "  " (or (car-safe (insert-directory-wildcard-in-dir-p dir))
                           (directory-file-name (file-name-directory dir))) ":\n")
	  (setq content-point (point)))
	(when wildcard
	  ;; Insert "wildcard" line where "total" line would be for a full dir.
	  (insert "  wildcard " (or (cdr-safe (insert-directory-wildcard-in-dir-p dir))
                                    (file-name-nondirectory dir))
                  "\n")))
      (dired-insert-set-properties content-point (point)))))

(defun dired-insert-set-properties (beg end)
  "Add various text properties to the lines in the region, from BEG to END."
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (ignore-errors
	(if (not (dired-move-to-filename))
	    (unless (or (looking-at-p "^$")
			(looking-at-p dired-subdir-regexp))
	      (put-text-property (line-beginning-position)
				 (1+ (line-end-position))
				 'invisible 'dired-hide-details-information))
	  (put-text-property (+ (line-beginning-position) 1) (1- (point))
			     'invisible 'dired-hide-details-detail)
	  (add-text-properties
	   (point)
	   (progn
	     (dired-move-to-end-of-filename)
	     (point))
	   '(mouse-face
	     highlight
	     dired-filename t
	     help-echo "mouse-2: visit this file in other window"))
	  (when (< (+ (point) 4) (line-end-position))
	    (put-text-property (+ (point) 4) (line-end-position)
			       'invisible 'dired-hide-details-link))))
      (forward-line 1))))


;;; Reverting a dired buffer

(defun dired-revert (&optional _arg _noconfirm)
  "Reread the Dired buffer.
Must also be called after `dired-actual-switches' have changed.
Should not fail even on completely garbaged buffers.
Preserves old cursor, marks/flags, hidden-p.

Dired sets `revert-buffer-function' to this function.  The args
ARG and NOCONFIRM, passed from `revert-buffer', are ignored."
  (widen)				; just in case user narrowed
  (let ((modflag (buffer-modified-p))
	(positions (dired-save-positions))
	(mark-alist nil)		; save marked files
	(hidden-subdirs (dired-remember-hidden))
	(old-subdir-alist (cdr (reverse dired-subdir-alist))) ; except pwd
	(case-fold-search nil)		; we check for upper case ls flags
	(inhibit-read-only t))
    (goto-char (point-min))
    (setq mark-alist;; only after dired-remember-hidden since this unhides:
	  (dired-remember-marks (point-min) (point-max)))
    ;; treat top level dir extra (it may contain wildcards)
    (if (not (consp dired-directory))
	(dired-uncache dired-directory)
      (dired-uncache (car dired-directory))
      (dolist (dir (cdr dired-directory))
	(if (file-name-absolute-p dir)
	    (dired-uncache dir))))
    ;; Run dired-after-readin-hook just once, below.
    (let ((dired-after-readin-hook nil))
      (dired-readin)
      (dired-insert-old-subdirs old-subdir-alist))
    (dired-mark-remembered mark-alist)	; mark files that were marked
    ;; ... run the hook for the whole buffer, and only after markers
    ;; have been reinserted (else omitting in dired-x would omit marked files)
    (run-hooks 'dired-after-readin-hook)	; no need to narrow
    (dired-restore-positions positions)
    (save-excursion			; hide subdirs that were hidden
      (dolist (dir hidden-subdirs)
	(if (dired-goto-subdir dir)
	    (dired-hide-subdir 1))))
    (unless modflag (restore-buffer-modified-p nil))
    (hack-dir-local-variables-non-file-buffer))
  ;; outside of the let scope
;;;  Might as well not override the user if the user changed this.
;;;  (setq buffer-read-only t)
  )

;; Subroutines of dired-revert
;; Some of these are also used when inserting subdirs.

(defun dired-save-positions ()
  "Return current positions in the buffer and all windows with this directory.
The positions have the form (BUFFER-POSITION WINDOW-POSITIONS).

BUFFER-POSITION is the point position in the current Dired buffer.
It has the form (BUFFER DIRED-FILENAME BUFFER-LINE-NUMBER).

WINDOW-POSITIONS are current positions in all windows displaying
this dired buffer.  The window positions have the form (WINDOW
DIRED-FILENAME WINDOW-LINE-NUMBER).

We store line numbers instead of point positions because the header
lines might change as well: when this happen the line number doesn't
change; the point does."
  (list
   (list (current-buffer) (dired-get-filename nil t) (line-number-at-pos))
   (mapcar (lambda (w)
	     (with-selected-window w
               (list w
		     (dired-get-filename nil t)
                     (line-number-at-pos (window-point w)))))
	   (get-buffer-window-list nil 0 t))
   ;; For each window that showed the current buffer before, scan its
   ;; list of previous buffers.  For each association thus found save
   ;; a triple <point, name, line> where 'point' is that window's
   ;; window-point marker stored in the window's list of previous
   ;; buffers, 'name' is the filename at the position of 'point' and
   ;; 'line' is the line number at the position of 'point'.
   (let ((buffer (current-buffer))
         prevs)
     (walk-windows
      (lambda (window)
        (let ((prev (assq buffer (window-prev-buffers window))))
          (when prev
            (with-current-buffer buffer
              (save-excursion
                (goto-char (nth 2 prev))
                (setq prevs
                      (cons
                       (list (nth 2 prev)
                             (dired-get-filename nil t)
                             (line-number-at-pos (point)))
                       prevs)))))))
      'nomini t)
     prevs)))

(defun dired-restore-positions (positions)
  "Restore POSITIONS saved with `dired-save-positions'."
  (let* ((buf-file-pos (nth 0 positions))
	 (buffer (nth 0 buf-file-pos))
         (prevs (nth 2 positions)))
    (unless (and (nth 1 buf-file-pos)
		 (dired-goto-file (nth 1 buf-file-pos)))
      (goto-char (point-min))
      (forward-line (1- (nth 2 buf-file-pos)))
      (dired-move-to-filename))
    (dolist (win-file-pos (nth 1 positions))
      ;; Ensure that window still displays the original buffer.
      (when (eq (window-buffer (nth 0 win-file-pos)) buffer)
	(with-selected-window (nth 0 win-file-pos)
	  (unless (and (nth 1 win-file-pos)
		       (dired-goto-file (nth 1 win-file-pos)))
            (goto-char (point-min))
	    (forward-line (1- (nth 2 win-file-pos)))
	    (dired-move-to-filename)))))
    (when prevs
      (with-current-buffer buffer
        (save-excursion
          (dolist (prev prevs)
            (let ((point (nth 0 prev)))
              ;; Sanity check of the point marker.
              (when (and (markerp point)
                         (eq (marker-buffer point) buffer))
                (unless (and (nth 1 prev)
                             (dired-goto-file (nth 1 prev)))
                  (goto-char (point-min))
	          (forward-line (1- (nth 2 prev))))
	        (dired-move-to-filename)
                (move-marker point (point) buffer)))))))))

(defun dired-remember-marks (beg end)
  "Return alist of files and their marks, from BEG to END."
  (if (dired--find-hidden-pos (point-min) (point-max))
      (dired--unhide (point-min) (point-max))) ;Must unhide to make this work.
  (let (fil chr alist)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward dired-re-mark end t)
	(if (setq fil (dired-get-filename nil t))
	    (setq chr (preceding-char)
		  alist (cons (cons fil chr) alist)))))
    alist))

(defun dired-mark-remembered (alist)
  "Mark all files remembered in ALIST.
Each element of ALIST looks like (FILE . MARKERCHAR)."
  (let (elt fil chr)
    (save-excursion
      (while alist
	(setq elt (car alist)
	      alist (cdr alist)
	      fil (car elt)
	      chr (cdr elt))
	(when (dired-goto-file fil)
	  (beginning-of-line)
	  (delete-char 1)
	  (insert chr))))))

(defun dired-remember-hidden ()
  "Return a list of names of subdirs currently hidden."
  (let (result)
    (pcase-dolist (`(,dir . ,pos) dired-subdir-alist)
      (goto-char pos)
      (end-of-line)
      (if (dired--hidden-p)
	  (push dir result)))
    result))

(defun dired-insert-old-subdirs (old-subdir-alist)
  "Try to insert all subdirs that were displayed before.
Do so according to the former subdir alist OLD-SUBDIR-ALIST."
  (or (dired-switches-recursive-p dired-actual-switches)
      (let (elt dir)
	(while old-subdir-alist
	  (setq elt (car old-subdir-alist)
		old-subdir-alist (cdr old-subdir-alist)
		dir (car elt))
	  (ignore-errors
	    (dired-uncache dir)
	    (dired-insert-subdir dir))))))

(defun dired-uncache (dir)
  "Remove directory DIR from any directory cache."
  (let ((handler (find-file-name-handler dir 'dired-uncache)))
    (if handler
	(funcall handler 'dired-uncache dir))))


;;; Dired mode key bindings and menus

(defvar dired-mode-map
  ;; This looks ugly when substitute-command-keys uses C-d instead d:
  ;;  (define-key dired-mode-map "\C-d" 'dired-flag-file-deletion)
  (let ((map (make-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map [mouse-2] 'dired-mouse-find-file-other-window)
    (define-key map [follow-link] 'mouse-face)
    ;; Commands to mark or flag certain categories of files
    (define-key map "#" 'dired-flag-auto-save-files)
    (define-key map "." 'dired-clean-directory)
    (define-key map "~" 'dired-flag-backup-files)
    ;; Upper case keys (except !) for operating on the marked files
    (define-key map "A" 'dired-do-find-regexp)
    (define-key map "C" 'dired-do-copy)
    (define-key map "B" 'dired-do-byte-compile)
    (define-key map "D" 'dired-do-delete)
    (define-key map "G" 'dired-do-chgrp)
    (define-key map "H" 'dired-do-hardlink)
    (define-key map "L" 'dired-do-load)
    (define-key map "M" 'dired-do-chmod)
    (define-key map "O" 'dired-do-chown)
    (define-key map "P" 'dired-do-print)
    (define-key map "Q" 'dired-do-find-regexp-and-replace)
    (define-key map "R" 'dired-do-rename)
    (define-key map "S" 'dired-do-symlink)
    (define-key map "T" 'dired-do-touch)
    (define-key map "X" 'dired-do-shell-command)
    (define-key map "Z" 'dired-do-compress)
    (define-key map "c" 'dired-do-compress-to)
    (define-key map "!" 'dired-do-shell-command)
    (define-key map "&" 'dired-do-async-shell-command)
    ;; Comparison commands
    (define-key map "=" 'dired-diff)
    ;; Tree Dired commands
    (define-key map "\M-\C-?" 'dired-unmark-all-files)
    (define-key map "\M-\C-d" 'dired-tree-down)
    (define-key map "\M-\C-u" 'dired-tree-up)
    (define-key map "\M-\C-n" 'dired-next-subdir)
    (define-key map "\M-\C-p" 'dired-prev-subdir)
    ;; move to marked files
    (define-key map "\M-{" 'dired-prev-marked-file)
    (define-key map "\M-}" 'dired-next-marked-file)
    ;; Make all regexp commands share a `%' prefix:
    ;; We used to get to the submap via a symbol dired-regexp-prefix,
    ;; but that seems to serve little purpose, and copy-keymap
    ;; does a better job without it.
    (define-key map "%" nil)
    (define-key map "%u" 'dired-upcase)
    (define-key map "%l" 'dired-downcase)
    (define-key map "%d" 'dired-flag-files-regexp)
    (define-key map "%g" 'dired-mark-files-containing-regexp)
    (define-key map "%m" 'dired-mark-files-regexp)
    (define-key map "%r" 'dired-do-rename-regexp)
    (define-key map "%C" 'dired-do-copy-regexp)
    (define-key map "%H" 'dired-do-hardlink-regexp)
    (define-key map "%R" 'dired-do-rename-regexp)
    (define-key map "%S" 'dired-do-symlink-regexp)
    (define-key map "%&" 'dired-flag-garbage-files)
    ;; Commands for marking and unmarking.
    (define-key map "*" nil)
    (define-key map "**" 'dired-mark-executables)
    (define-key map "*/" 'dired-mark-directories)
    (define-key map "*@" 'dired-mark-symlinks)
    (define-key map "*%" 'dired-mark-files-regexp)
    (define-key map "*N" 'dired-number-of-marked-files)
    (define-key map "*c" 'dired-change-marks)
    (define-key map "*s" 'dired-mark-subdir-files)
    (define-key map "*m" 'dired-mark)
    (define-key map "*u" 'dired-unmark)
    (define-key map "*?" 'dired-unmark-all-files)
    (define-key map "*!" 'dired-unmark-all-marks)
    (define-key map "U" 'dired-unmark-all-marks)
    (define-key map "*\177" 'dired-unmark-backward)
    (define-key map "*\C-n" 'dired-next-marked-file)
    (define-key map "*\C-p" 'dired-prev-marked-file)
    (define-key map "*t" 'dired-toggle-marks)
    ;; Lower keys for commands not operating on all the marked files
    (define-key map "a" 'dired-find-alternate-file)
    (define-key map "d" 'dired-flag-file-deletion)
    (define-key map "e" 'dired-find-file)
    (define-key map "f" 'dired-find-file)
    (define-key map "\C-m" 'dired-find-file)
    (put 'dired-find-file :advertised-binding "\C-m")
    (define-key map "g" 'revert-buffer)
    (define-key map "i" 'dired-maybe-insert-subdir)
    (define-key map "j" 'dired-goto-file)
    (define-key map "k" 'dired-do-kill-lines)
    (define-key map "l" 'dired-do-redisplay)
    (define-key map "m" 'dired-mark)
    (define-key map "n" 'dired-next-line)
    (define-key map "o" 'dired-find-file-other-window)
    (define-key map "\C-o" 'dired-display-file)
    (define-key map "p" 'dired-previous-line)
    (define-key map "s" 'dired-sort-toggle-or-edit)
    (define-key map "t" 'dired-toggle-marks)
    (define-key map "u" 'dired-unmark)
    (define-key map "v" 'dired-view-file)
    (define-key map "w" 'dired-copy-filename-as-kill)
    (define-key map "W" 'browse-url-of-dired-file)
    (define-key map "x" 'dired-do-flagged-delete)
    (define-key map "y" 'dired-show-file-type)
    (define-key map "+" 'dired-create-directory)
    ;; moving
    (define-key map "<" 'dired-prev-dirline)
    (define-key map ">" 'dired-next-dirline)
    (define-key map "^" 'dired-up-directory)
    (define-key map " " 'dired-next-line)
    (define-key map [?\S-\ ] 'dired-previous-line)
    (define-key map [remap next-line] 'dired-next-line)
    (define-key map [remap previous-line] 'dired-previous-line)
    ;; hiding
    (define-key map "$" 'dired-hide-subdir)
    (define-key map "\M-$" 'dired-hide-all)
    (define-key map "(" 'dired-hide-details-mode)
    ;; isearch
    (define-key map (kbd "M-s a C-s")   'dired-do-isearch)
    (define-key map (kbd "M-s a M-C-s") 'dired-do-isearch-regexp)
    (define-key map (kbd "M-s f C-s")   'dired-isearch-filenames)
    (define-key map (kbd "M-s f M-C-s") 'dired-isearch-filenames-regexp)
    ;; misc
    (define-key map [remap read-only-mode] 'dired-toggle-read-only)
    ;; `toggle-read-only' is an obsolete alias for `read-only-mode'
    (define-key map [remap toggle-read-only] 'dired-toggle-read-only)
    (define-key map "?" 'dired-summary)
    (define-key map "\177" 'dired-unmark-backward)
    (define-key map [remap undo] 'dired-undo)
    (define-key map [remap advertised-undo] 'dired-undo)
    (define-key map [remap vc-next-action] 'dired-vc-next-action)
    ;; thumbnail manipulation (image-dired)
    (define-key map "\C-td" 'image-dired-display-thumbs)
    (define-key map "\C-tt" 'image-dired-tag-files)
    (define-key map "\C-tr" 'image-dired-delete-tag)
    (define-key map "\C-tj" 'image-dired-jump-thumbnail-buffer)
    (define-key map "\C-ti" 'image-dired-dired-display-image)
    (define-key map "\C-tx" 'image-dired-dired-display-external)
    (define-key map "\C-ta" 'image-dired-display-thumbs-append)
    (define-key map "\C-t." 'image-dired-display-thumb)
    (define-key map "\C-tc" 'image-dired-dired-comment-files)
    (define-key map "\C-tf" 'image-dired-mark-tagged-files)
    (define-key map "\C-t\C-t" 'image-dired-dired-toggle-marked-thumbs)
    (define-key map "\C-te" 'image-dired-dired-edit-comment-and-tags)
    ;; encryption and decryption (epa-dired)
    (define-key map ":d" 'epa-dired-do-decrypt)
    (define-key map ":v" 'epa-dired-do-verify)
    (define-key map ":s" 'epa-dired-do-sign)
    (define-key map ":e" 'epa-dired-do-encrypt)

    ;; No need to do this, now that top-level items are fewer.
    ;;;;
    ;; Get rid of the Edit menu bar item to save space.
    ;;(define-key map [menu-bar edit] 'undefined)

    map)
  "Local keymap for Dired mode buffers.")

(easy-menu-define dired-mode-subdir-menu dired-mode-map
  "Subdir menu for Dired mode."
  '("Subdir"
    ["Insert This Subdir" dired-maybe-insert-subdir
     :help "Insert contents of subdirectory"
     :enable (let ((f (dired-get-filename nil t)))
               (and f (file-directory-p f)))]
    ["Next Dirline" dired-next-dirline
     :help "Move to previous directory-file line"]
    ["Prev Dirline" dired-prev-dirline
     :help "Move to next directory-file line"]
    ["Next Subdir" dired-next-subdir
     :help "Go to next subdirectory header line"]
    ["Prev Subdir" dired-prev-subdir
     :help "Go to previous subdirectory header line"]
    ["Up Directory" dired-up-directory
     :help "Edit the parent directory"]
    ["Tree Up" dired-tree-up
     :help "Go to first subdirectory header up the tree"]
    ["Tree Down" dired-tree-down
     :help "Go to first subdirectory header down the tree"]
    ["Hide/UnHide Subdir" dired-hide-subdir
     :help "Hide or unhide current directory listing"]
    ["Hide All" dired-hide-all
     :help "Hide all subdirectories, leave only header lines"]))

(easy-menu-define dired-mode-immediate-menu dired-mode-map
  "Immediate menu for Dired mode."
  '("Immediate"
    ["Edit File Names" wdired-change-to-wdired-mode
     :help "Put a Dired buffer in a mode in which filenames are editable"
     :keys "C-x C-q"
     :filter (lambda (x) (if (eq major-mode 'dired-mode) x))]
    ["Create Empty file..." dired-create-empty-file
     :help "Create an empty file"]
    ["Create Directory..." dired-create-directory
     :help "Create a directory"]
    ["Find This File" dired-find-file
     :help "Edit file at cursor"]
    ["Find in Other Window" dired-find-file-other-window
     :help "Edit file at cursor in other window"]
    ["Display in Other Window" dired-display-file
     :help "Display file at cursor in other window"]
    ["View This File" dired-view-file
     :help "Examine file at cursor in read-only mode"]
    ["Diff..." dired-diff
     :help "Compare file at cursor with another file"]
    ["Compare with Backup" dired-backup-diff
     :help "Diff file at cursor with its latest backup"]
    ["Compare Directories..." dired-compare-directories
     :help "Mark files with different attributes in two Dired buffers"]
    ["Isearch in File Names..." dired-isearch-filenames
     :help "Incrementally search for string in file names only."]
    ["Isearch Regexp in File Names..." dired-isearch-filenames-regexp
     :help "Incrementally search for regexp in file names only"]
    "---"
    ["#Marked Files" dired-number-of-marked-files
     :help "Display the number and size of the marked files"]
    ["Refresh" revert-buffer
     :help "Update contents of shown directories"]
    ["Hide Details" dired-hide-details-mode
     :help "Hide details in buffer"
     :style toggle
     :selected dired-hide-details-mode]
    ["Toggle Image Thumbnails in This Buffer" image-dired-dired-toggle-marked-thumbs
     :help "Add or remove image thumbnails in front of marked file names"]
    ["Display Image" image-dired-dired-display-image
     :help "Display sized image in a separate window"]
    ["Display Image Externally" image-dired-dired-display-external
     :help "Display image in external viewer"]))

(easy-menu-define dired-mode-regexp-menu dired-mode-map
  "Regexp menu for Dired mode."
  '("Regexp"
    ["Mark Containing..." dired-mark-files-containing-regexp
     :help "Mark files whose contents matches regexp"]
    ["Mark..." dired-mark-files-regexp
     :help "Mark files matching regexp for future operations"]
    ["Flag..." dired-flag-files-regexp
     :help "Flag files matching regexp for deletion"]
    ["Copy..." dired-do-copy-regexp
     :help "Copy marked files matching regexp"]
    ["Rename..." dired-do-rename-regexp
     :help "Rename marked files matching regexp"]
    ["Symlink..." dired-do-symlink-regexp
     :visible (fboundp 'make-symbolic-link)
     :help "Make symbolic links for files matching regexp"]
    ["Hardlink..." dired-do-hardlink-regexp
     :help "Make hard links for files matching regexp"]
    ["Upcase" dired-upcase
     :enable (or (not (fboundp 'msdos-long-file-names))
                 (msdos-long-file-names))
     :help "Rename marked files to upper-case name"]
    ["Downcase" dired-downcase
     ;; When running on plain MS-DOS, there's only one
     ;; letter-case for file names.
     :enable (or (not (fboundp 'msdos-long-file-names))
                 (msdos-long-file-names))
     :help "Rename marked files to lower-case name"]
    "---"
    ["Mark From Image Tag..." image-dired-mark-tagged-files
     :help "Mark files whose image tags matches regexp"]))

(easy-menu-define dired-mode-mark-menu dired-mode-map
  "Mark menu for Dired mode."
  '("Mark"
    ["Toggle Marks" dired-toggle-marks
     :help "Mark unmarked files, unmark marked ones"]
    ["Mark" dired-mark
     :help "Mark current line's file for future operations"]
    ["Unmark" dired-unmark
     :help "Unmark or unflag current line's file"]
    ["Flag" dired-flag-file-deletion
     :help "Flag current line's file for deletion"]
    ["Flag Auto-save Files" dired-flag-auto-save-files
     :help "Flag auto-save files for deletion"]
    ["Flag Backup Files" dired-flag-backup-files
     :help "Flag all backup files for deletion"]
    ["Flag Garbage Files" dired-flag-garbage-files
     :help "Flag unneeded files for deletion"]
    ["Mark Executables" dired-mark-executables
     :help "Mark all executable files"]
    ["Mark Old Backups" dired-clean-directory
     :help "Flag old numbered backups for deletion"]
    ["Mark Directories" dired-mark-directories
     :help "Mark all directories except `.' and `..'"]
    ["Mark Symlinks" dired-mark-symlinks
     :visible (fboundp 'make-symbolic-link)
     :help "Mark all symbolic links"]
    ["Unmark All" dired-unmark-all-marks]
    ["Change Marks..." dired-change-marks
     :help "Replace marker with another character"]
    ["Next Marked" dired-next-marked-file
     :help "Move to next marked file"]
    ["Previous Marked" dired-prev-marked-file
     :help "Move to previous marked file"]))

(easy-menu-define dired-mode-operate-menu dired-mode-map
  "Operate menu for Dired mode."
  '("Operate"
    ["Copy to..." dired-do-copy
     :help "Copy current file or all marked files"]
    ["Rename to..." dired-do-rename
     :help "Rename current file or move marked files"]
    ("Delete"
     ["Delete Flagged Files" dired-do-flagged-delete
      :help "Delete all files flagged for deletion (D)"]
     ["Delete Marked (Not Flagged) Files" dired-do-delete
      :help "Delete current file or all marked files (excluding flagged files)"])
    ["Shell Command..." dired-do-shell-command
     :help "Run a shell command on current or marked files"]
    ["Asynchronous Shell Command..." dired-do-async-shell-command
     :help "Run a shell command asynchronously on current or marked files"]
    ["Symlink to..." dired-do-symlink
     :visible (fboundp 'make-symbolic-link)
     :help "Make symbolic links for current or marked files"]
    ["Hardlink to..." dired-do-hardlink
     :help "Make hard links for current or marked files"]
    ["Print..." dired-do-print
     :help "Ask for print command and print marked files"]
    ["Compress" dired-do-compress
     :help "Compress/uncompress marked files"]
    ["Byte-compile" dired-do-byte-compile
     :help "Byte-compile marked Emacs Lisp files"]
    ["Load" dired-do-load
     :help "Load marked Emacs Lisp files"]
    ["Change Timestamp..." dired-do-touch
     :help "Change timestamp of marked files"]
    ["Change Mode..." dired-do-chmod
     :help "Change mode (attributes) of marked files"]
    ["Change Group..." dired-do-chgrp
     :visible (not (memq system-type '(ms-dos windows-nt)))
     :help "Change the group of marked files"]
    ["Change Owner..." dired-do-chown
     :visible (not (memq system-type '(ms-dos windows-nt)))
     :help "Change the owner of marked files"]
    ["Isearch Files..." dired-do-isearch
     :help "Incrementally search marked files for string"]
    ["Isearch Regexp Files..." dired-do-isearch-regexp
     :help "Incrementally search marked files for regexp"]
    ["Search Files..." dired-do-find-regexp
     :help "Search marked files for matches of regexp"]
    ["Query Replace in Files..." dired-do-find-regexp-and-replace
     :help "Replace regexp matches in marked files"]
    "---"
    ["Encrypt..." epa-dired-do-encrypt
     :help "Encrypt current or marked files"]
    ["Sign..." epa-dired-do-sign
     :help "Create digital signature of current or marked files"]
    ["Verify" epa-dired-do-verify
     :help "Verify digital signature of current or marked files"]
    ["Decrypt..." epa-dired-do-decrypt
     :help "Decrypt current or marked files"]
    "---"
    ["Display Image Thumbnails" image-dired-display-thumbs
     :help "Display image thumbnails for current or marked image files"]
    ["Add Image Comment..." image-dired-dired-comment-files
     :help "Add image comment to current or marked files"]
    ["Add Image Tags..." image-dired-tag-files
     :help "Add image tags to current or marked files"]
    ["Delete Image Tag..." image-dired-delete-tag
     :help "Delete image tag from current or marked files"]))

(defun dired-context-menu (menu click)
  "Populate MENU with Dired mode commands at CLICK."
  (when (mouse-posn-property (event-start click) 'dired-filename)
    (define-key menu [dired-separator] menu-bar-separator)
    (let ((easy-menu (make-sparse-keymap "Immediate")))
      (easy-menu-define nil easy-menu nil
        '("Immediate"
          ["Find This File" dired-mouse-find-file
           :help "Edit file at mouse click"]
          ["Find in Other Window" dired-mouse-find-file-other-window
           :help "Edit file at mouse click in other window"]))
      (dolist (item (reverse (lookup-key easy-menu [menu-bar immediate])))
        (when (consp item)
          (define-key menu (vector (car item)) (cdr item))))))
  menu)


;;; Dired mode

;; Dired mode is suitable only for specially formatted data.
(put 'dired-mode 'mode-class 'special)

(defvar grep-read-files-function)
;; Autoload cookie needed by desktop.el
;;;###autoload
(defun dired-mode (&optional dirname switches)
  "Mode for \"editing\" directory listings.
In Dired, you are \"editing\" a list of the files in a directory and
  (optionally) its subdirectories, in the format of `ls -lR'.
  Each directory is a page: use \\[backward-page] and \\[forward-page] to move pagewise.
\"Editing\" means that you can run shell commands on files, visit,
  compress, load or byte-compile them, change their file attributes
  and insert subdirectories into the same buffer.  You can \"mark\"
  files for later commands or \"flag\" them for deletion, either file
  by file or all files matching certain criteria.
You can move using the usual cursor motion commands.\\<dired-mode-map>
The buffer is read-only.  Digits are prefix arguments.
Type \\[dired-flag-file-deletion] to flag a file `D' for deletion.
Type \\[dired-mark] to Mark a file or subdirectory for later commands.
  Most commands operate on the marked files and use the current file
  if no files are marked.  Use a numeric prefix argument to operate on
  the next ARG (or previous -ARG if ARG<0) files, or just `1'
  to operate on the current file only.  Prefix arguments override marks.
  Mark-using commands display a list of failures afterwards.  Type \\[dired-summary]
  to see why something went wrong.
Type \\[dired-unmark] to Unmark a file or all files of an inserted subdirectory.
Type \\[dired-unmark-backward] to back up one line and unmark or unflag.
Type \\[dired-do-flagged-delete] to delete (eXpunge) the files flagged `D'.
Type \\[dired-find-file] to Find the current line's file
  (or dired it in another buffer, if it is a directory).
Type \\[dired-find-file-other-window] to find file or Dired directory in Other window.
Type \\[dired-maybe-insert-subdir] to Insert a subdirectory in this buffer.
Type \\[dired-do-rename] to Rename a file or move the marked files to another directory.
Type \\[dired-do-copy] to Copy files.
Type \\[dired-sort-toggle-or-edit] to toggle Sorting by name/date or change the `ls' switches.
Type \\[revert-buffer] to read all currently expanded directories aGain.
  This retains all marks and hides subdirs again that were hidden before.
Use `SPC' and `DEL' to move down and up by lines.

If Dired ever gets confused, you can either type \\[revert-buffer] \
to read the
directories again, type \\[dired-do-redisplay] \
to relist the file at point or the marked files or a
subdirectory, or type \\[dired-build-subdir-alist] to parse the buffer
again for the directory tree.

See the `dired' customization group for a list of user options.

This mode runs the following hooks:

  `dired-before-readin-hook'
  `dired-after-readin-hook'
  `dired-mode-hook'

Keybindings:
\\{dired-mode-map}"
  ;; Not to be called interactively (e.g. dired-directory will be set
  ;; to default-directory, which is wrong with wildcards).
  (kill-all-local-variables)
  (use-local-map dired-mode-map)
  (dired-advertise)			; default-directory is already set
  (setq major-mode 'dired-mode
	mode-name "Dired"
	;; case-fold-search nil
	buffer-read-only t
	mode-line-buffer-identification
	(propertized-buffer-identification "%17b"))
  (add-to-invisibility-spec '(dired . t))
  ;; Ignore dired-hide-details-* value of invisible text property by default.
  (when (eq buffer-invisibility-spec t)
    (setq buffer-invisibility-spec (list t)))
  (setq-local revert-buffer-function #'dired-revert)
  (setq-local buffer-stale-function #'dired-buffer-stale-p)
  (setq-local buffer-auto-revert-by-notification t)
  (setq-local page-delimiter "\n\n")
  (setq-local dired-directory (or dirname default-directory))
  ;; list-buffers uses this to display the dir being edited in this buffer.
  (setq list-buffers-directory
	(expand-file-name (if (listp dired-directory)
			      (car dired-directory)
			    dired-directory)))
  (setq-local dired-actual-switches (or switches dired-listing-switches))
  (setq-local font-lock-defaults
              '(dired-font-lock-keywords t nil nil beginning-of-line))
  (setq-local desktop-save-buffer 'dired-desktop-buffer-misc-data)
  (setq-local grep-read-files-function #'dired-grep-read-files)
  (setq dired-switches-alist nil)
  (hack-dir-local-variables-non-file-buffer) ; before sorting
  (dired-sort-other dired-actual-switches t)
  (when (featurep 'dnd)
    (setq-local dnd-protocol-alist
                (append dired-dnd-protocol-alist dnd-protocol-alist)))
  (add-hook 'file-name-at-point-functions #'dired-file-name-at-point nil t)
  (add-hook 'isearch-mode-hook #'dired-isearch-filenames-setup nil t)
  (add-hook 'context-menu-functions 'dired-context-menu 5 t)
  (run-mode-hooks 'dired-mode-hook))


;;; Idiosyncratic dired commands that don't deal with marks

(defun dired-summary ()
  "Summarize basic Dired commands and show recent Dired errors."
  (interactive)
  (dired-why)
  ;; FIXME this should check the key-bindings and use
  ;; substitute-command-keys if non-standard
  (message
   "d-elete, u-ndelete, x-punge, f-ind, o-ther window, R-ename, C-opy, h-elp"))

(defun dired-undo ()
  "Undo in a Dired buffer.
This doesn't recover lost files, it just undoes changes in the buffer itself.
You can use it to recover marks, killed lines or subdirs."
  (interactive)
  (let ((inhibit-read-only t))
    (undo))
  (dired-build-subdir-alist)
  (message "Change in Dired buffer undone.
Actual changes in files cannot be undone by Emacs."))

(defun dired-toggle-read-only ()
  "Edit Dired buffer with Wdired, or make it read-only.
If the current buffer can be edited with Wdired, (i.e. the major
mode is `dired-mode'), call `wdired-change-to-wdired-mode'.
Otherwise, toggle `read-only-mode'."
  (interactive)
  (when (and (not (file-writable-p default-directory))
             (not (y-or-n-p
                   "Directory isn't writable; edit anyway? ")))
    (user-error "Directory %s isn't writable" default-directory))
  (if (derived-mode-p 'dired-mode)
      (wdired-change-to-wdired-mode)
    (read-only-mode 'toggle)))

(defun dired-next-line (arg)
  "Move down lines then position at filename.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "^p")
  (let ((line-move-visual)
	(goal-column))
    (line-move arg t))
  ;; We never want to move point into an invisible line.
  (while (and (invisible-p (point))
	      (not (if (and arg (< arg 0)) (bobp) (eobp))))
    (forward-char (if (and arg (< arg 0)) -1 1)))
  (dired-move-to-filename))

(defun dired-previous-line (arg)
  "Move up lines then position at filename.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "^p")
  (dired-next-line (- (or arg 1))))

(defun dired-next-dirline (arg &optional opoint)
  "Goto ARGth next directory file line."
  (interactive "p")
  (or opoint (setq opoint (point)))
  (if (if (> arg 0)
	  (re-search-forward dired-re-dir nil t arg)
	(beginning-of-line)
	(re-search-backward dired-re-dir nil t (- arg)))
      (dired-move-to-filename)		; user may type `i' or `f'
    (goto-char opoint)
    (error "No more subdirectories")))

(defun dired-prev-dirline (arg)
  "Goto ARGth previous directory file line."
  (interactive "p")
  (dired-next-dirline (- arg)))

(defun dired-up-directory (&optional other-window)
  "Run Dired on parent directory of current directory.
Find the parent directory either in this buffer or another buffer.
Creates a buffer if necessary.
If OTHER-WINDOW (the optional prefix arg), display the parent
directory in another window."
  (interactive "P")
  (let* ((dir (dired-current-directory))
	 (up (file-name-directory (directory-file-name dir))))
    (or (dired-goto-file (directory-file-name dir))
	;; Only try dired-goto-subdir if buffer has more than one dir.
	(and (cdr dired-subdir-alist)
	     (dired-goto-subdir up))
	(progn
	  (if other-window
	      (dired-other-window up)
	    (dired--find-possibly-alternative-file up))
	  (dired-goto-file dir)))))

(defun dired-get-file-for-visit ()
  "Get the current line's file name, with an error if file does not exist."
  (interactive)
  ;; We pass t for second arg so that we don't get error for `.' and `..'.
  (let ((raw (dired-get-filename nil t))
	file-name)
    (if (null raw)
	(user-error "No file on this line"))
    (setq file-name (file-name-sans-versions raw t))
    (if (file-exists-p file-name)
	file-name
      (if (file-symlink-p file-name)
	  (error "File is a symlink to a nonexistent target")
	(error "File no longer exists; type `g' to update Dired buffer")))))

;; Force C-m keybinding rather than `f' or `e' in the mode doc:
(define-obsolete-function-alias 'dired-advertised-find-file
  #'dired-find-file "23.2")
(defun dired-find-file ()
  "In Dired, visit the file or directory named on this line."
  (interactive)
  (dired--find-possibly-alternative-file (dired-get-file-for-visit)))

(defun dired--find-possibly-alternative-file (file)
  "Find FILE, but respect `dired-kill-when-opening-new-dired-buffer'."
  (if (and dired-kill-when-opening-new-dired-buffer
           (file-directory-p file))
      (progn
        (set-buffer-modified-p nil)
        (dired--find-file #'find-alternate-file file))
    (dired--find-file #'find-file file)))

(defun dired--find-file (find-file-function file)
  "Call FIND-FILE-FUNCTION on FILE, but bind some relevant variables."
  ;; Bind `find-file-run-dired' so that the command works on directories
  ;; too, independent of the user's setting.
  (let ((find-file-run-dired t)
        ;; This binding prevents problems with preserving point in
        ;; windows displaying Dired buffers, because reverting a Dired
        ;; buffer empties it, which changes the places where the
        ;; markers used by switch-to-buffer-preserve-window-point
        ;; point.
        (switch-to-buffer-preserve-window-point
         (if dired-auto-revert-buffer
             nil
           switch-to-buffer-preserve-window-point)))
    (funcall find-file-function file)))

(defun dired-find-alternate-file ()
  "In Dired, visit file or directory on current line via `find-alternate-file'.
This kills the Dired buffer, then visits the current line's file or directory."
  (interactive)
  (set-buffer-modified-p nil)
  (find-alternate-file (dired-get-file-for-visit)))
;; Don't override the setting from .emacs.
;;;###autoload (put 'dired-find-alternate-file 'disabled t)

(defun dired-mouse-find-file (event &optional find-file-func find-dir-func)
  "In Dired, visit the file or directory name you click on.
The optional arguments FIND-FILE-FUNC and FIND-DIR-FUNC specify
functions to visit the file and directory, respectively.  If
omitted or nil, these arguments default to `find-file' and `dired',
respectively."
  (interactive "e")
  (or find-file-func (setq find-file-func 'find-file))
  (or find-dir-func (setq find-dir-func 'dired))
  (let (window pos file)
    (save-excursion
      (setq window (posn-window (event-end event))
	    pos (posn-point (event-end event)))
      (if (not (windowp window))
	  (error "No file chosen"))
      (set-buffer (window-buffer window))
      (goto-char pos)
      (setq file (dired-get-file-for-visit)))
    (if (file-directory-p file)
	(or (and (cdr dired-subdir-alist)
		 (dired-goto-subdir file))
	    (progn
	      (select-window window)
              (funcall find-dir-func file)))
      (select-window window)
      (dired--find-file find-file-func (file-name-sans-versions file t)))))

(defun dired-mouse-find-file-other-window (event)
  "In Dired, visit the file or directory name you click on in another window."
  (interactive "e")
  (dired-mouse-find-file event 'find-file-other-window 'dired-other-window))

(defun dired-mouse-find-file-other-frame (event)
  "In Dired, visit the file or directory name you click on in another frame."
  (interactive "e")
  (dired-mouse-find-file event 'find-file-other-frame 'dired-other-frame))

(defun dired-view-file ()
  "In Dired, examine a file in view mode, returning to Dired when done.
When file is a directory, show it in this buffer if it is inserted.
Otherwise, display it in another buffer."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
	(or (and (cdr dired-subdir-alist)
		 (dired-goto-subdir file))
	    (dired file))
      (view-file file))))

(defun dired-find-file-other-window ()
  "In Dired, visit this file or directory in another window."
  (interactive)
  (dired--find-file #'find-file-other-window (dired-get-file-for-visit)))

(defun dired-display-file ()
  "In Dired, display this file or directory in another window."
  (interactive)
  (display-buffer (find-file-noselect (dired-get-file-for-visit))
		  t))


;;; Functions for extracting and manipulating file names in Dired buffers

(defun dired-unhide-subdir ()
  (with-silent-modifications
    (dired--unhide (dired-subdir-min) (dired-subdir-max))))

(defun dired-subdir-hidden-p (dir)
  (save-excursion
    (dired-goto-subdir dir)
    (dired--hidden-p)))

(defun dired-subdir-min ()
  (save-excursion
    (if (not (dired-prev-subdir 0 t t))
	(error "Not in a subdir!")
      (point))))

(defun dired-get-filename (&optional localp no-error-if-not-filep)
  "In Dired, return name of file mentioned on this line.
Value returned normally includes the directory name.
Optional arg LOCALP with value `no-dir' means don't include directory
name in result.  A value of `verbatim' means to return the name exactly as
it occurs in the buffer, and a value of t means construct name relative to
`default-directory', which still may contain slashes if in a subdirectory.
Optional arg NO-ERROR-IF-NOT-FILEP means treat `.' and `..' as
regular filenames and return nil if no filename on this line.
Otherwise, an error occurs in these cases."
  (let ((hidden (and dired-subdir-alist
                     (dired-subdir-hidden-p
                      (dired-current-directory))))
	case-fold-search file p1 p2 already-absolute)
    (when hidden
      (dired-unhide-subdir))
    (save-excursion
      (if (setq p1 (dired-move-to-filename (not no-error-if-not-filep)))
	  (setq p2 (dired-move-to-end-of-filename no-error-if-not-filep))))
    (when hidden
      (dired-hide-subdir 1))
    ;; nil if no file on this line, but no-error-if-not-filep is t:
    (if (setq file (and p1 p2 (buffer-substring p1 p2)))
	(progn
	  ;; Get rid of the mouse-face property that file names have.
	  (set-text-properties 0 (length file) nil file)
	  ;; Unquote names quoted by ls or by dired-insert-directory.
	  ;; This code was written using `read' to unquote, because
          ;; it's faster than substituting \007 (4 chars) -> ^G (1
          ;; char) etc. in a lisp loop.  Unfortunately, this decision
          ;; has necessitated hacks such as dealing with filenames
          ;; with quotation marks in their names.
	  (while (string-match "\\(?:[^\\]\\|\\`\\)\\(\"\\)" file)
	    (setq file (replace-match "\\\"" nil t file 1)))
          ;; Unescape any spaces escaped by ls -b (bug#10469).
          ;; Other -b quotes, eg \t, \n, work transparently.
          (if (dired-switches-escape-p dired-actual-switches)
              (let ((start 0)
                    (rep "")
                    (shift -1))
                (if (eq localp 'verbatim)
                    (setq rep "\\\\"
                          shift +1))
                (while (string-match "\\(\\\\\\) " file start)
                  (setq file (replace-match rep nil t file 1)
                        start (+ shift (match-end 0))))))
	  (when (eq system-type 'windows-nt)
	    (save-match-data
	      (let ((start 0))
		(while (string-match "\\\\" file start)
		  (aset file (match-beginning 0) ?/)
		  (setq start (match-end 0))))))

          ;; Hence we don't need to worry about converting `\\' back to `\'.
          (setq file (read (concat "\"" file "\"")))))
    (and file (files--name-absolute-system-p file)
	 (setq already-absolute t))
    (cond
     ((null file)
      nil)
     ((eq localp 'verbatim)
      file)
     ((and (not no-error-if-not-filep)
	   (member file '("." "..")))
      (error "Cannot operate on `.' or `..'"))
     ((and (eq localp 'no-dir) already-absolute)
      (file-name-nondirectory file))
     (already-absolute
      (let ((handler (find-file-name-handler file nil)))
	;; check for safe-magic property so that we won't
	;; put /: for names that don't really need them.
	;; For instance, .gz files when auto-compression-mode is on.
	(if (and handler (not (get handler 'safe-magic)))
	    (concat "/:" file)
	  file)))
     ((eq localp 'no-dir)
      file)
     ((equal (dired-current-directory) "/")
      (setq file (concat (dired-current-directory localp) file))
      (let ((handler (find-file-name-handler file nil)))
	;; check for safe-magic property so that we won't
	;; put /: for names that don't really need them.
	;; For instance, .gz files when auto-compression-mode is on.
	(if (and handler (not (get handler 'safe-magic)))
	    (concat "/:" file)
	  file)))
     (t
      (concat (dired-current-directory localp) file)))))

(defun dired-string-replace-match (regexp string newtext
                                          &optional literal global)
  "Replace first match of REGEXP in STRING with NEWTEXT.
If it does not match, nil is returned instead of the new string.
Optional arg LITERAL means to take NEWTEXT literally.
Optional arg GLOBAL means to replace all matches."
  (if global
      (let ((start 0) ret)
	(while (string-match regexp string start)
	  (let ((from-end (- (length string) (match-end 0))))
	    (setq ret (setq string (replace-match newtext t literal string)))
	    (setq start (- (length string) from-end))))
	ret)
    (if (not (string-match regexp string 0))
	nil
      (replace-match newtext t literal string))))

(defun dired-make-absolute (file &optional dir)
  ;;"Convert FILE (a file name relative to DIR) to an absolute file name."
  ;; We can't always use expand-file-name as this would get rid of `.'
  ;; or expand in / instead default-directory if DIR=="".
  ;; This should be good enough for ange-ftp.
  ;; It should be reasonably fast, though, as it is called in
  ;; dired-get-filename.
  (concat (or dir default-directory) file))

(defun dired-make-relative (file &optional dir)
  "Convert FILE (an absolute file name) to a name relative to DIR.
If DIR is omitted or nil, it defaults to `default-directory'.
If FILE is not in the directory tree of DIR, return FILE
unchanged."
  (or dir (setq dir default-directory))
  ;; This case comes into play if default-directory is set to
  ;; use ~.
  (if (and (> (length dir) 0) (= (aref dir 0) ?~))
      (setq dir (expand-file-name dir)))
  (if (string-match (concat "^" (regexp-quote dir)) file)
      (substring file (match-end 0))
    file))


;;; Mode to hide details

(define-minor-mode dired-hide-details-mode
  "Toggle visibility of detailed information in current Dired buffer.
When this minor mode is enabled, details such as file ownership and
permissions are hidden from view.

See options: `dired-hide-details-hide-symlink-targets' and
`dired-hide-details-hide-information-lines'."
  :group 'dired
  (unless (derived-mode-p 'dired-mode)
    (error "Not a Dired buffer"))
  (dired-hide-details-update-invisibility-spec)
  (if dired-hide-details-mode
      (add-hook 'wdired-mode-hook
		'dired-hide-details-update-invisibility-spec
		nil
		t)
    (remove-hook 'wdired-mode-hook
		 'dired-hide-details-update-invisibility-spec
		 t)))

(defun dired-hide-details-update-invisibility-spec ()
  (funcall (if dired-hide-details-mode
	       'add-to-invisibility-spec
	     'remove-from-invisibility-spec)
	   'dired-hide-details-detail)
  (funcall (if (and dired-hide-details-mode
		    dired-hide-details-hide-information-lines)
	       'add-to-invisibility-spec
	     'remove-from-invisibility-spec)
	   'dired-hide-details-information)
  (funcall (if (and dired-hide-details-mode
		    dired-hide-details-hide-symlink-targets
		    (not (derived-mode-p 'wdired-mode)))
	       'add-to-invisibility-spec
	     'remove-from-invisibility-spec)
	   'dired-hide-details-link))


;;; Functions to hide/unhide text

(defun dired--find-hidden-pos (start end)
  (text-property-any start end 'invisible 'dired))

(defun dired--hidden-p (&optional pos)
  (eq (get-char-property (or pos (point)) 'invisible) 'dired))

(defun dired--hide (start end)
  ;; The old code used selective-display which only works at
  ;; a line-granularity, so it used start and end positions that where
  ;; approximate ("anywhere on the line is fine").
  (save-excursion
    (put-text-property (progn (goto-char start) (line-end-position))
                       (progn (goto-char end) (line-end-position))
                       'invisible 'dired)))

(defun dired--unhide (start end)
  ;; The old code used selective-display which only works at
  ;; a line-granularity, so it used start and end positions that where
  ;; approximate ("anywhere on the line is fine").
  ;; FIXME: This also removes other invisible properties!
  (save-excursion
    (remove-list-of-text-properties
     (progn (goto-char start) (line-end-position))
     (progn (goto-char end) (line-end-position))
     '(invisible))))

;;; Functions for finding the file name in a dired buffer line

(defvar dired-permission-flags-regexp
  "\\([^ ]\\)[-r][-w]\\([^ ]\\)[-r][-w]\\([^ ]\\)[-r][-w]\\([^ ]\\)"
  "Regular expression to match the permission flags in `ls -l'.")

;; Move to first char of filename on this line.
;; Returns position (point) or nil if no filename on this line."
(defun dired-move-to-filename (&optional raise-error eol)
  "Move to the beginning of the filename on the current line.
Return the position of the beginning of the filename, or nil if none found.

If RAISE-ERROR, signal an error if we can't find the filename on
the current line.

If EOL, it should be an position to use instead of
`line-end-position' as the end of the line."
  ;; This is the UNIX version.
  (or eol (setq eol (line-end-position)))
  (beginning-of-line)
  ;; First try assuming `ls --dired' was used.
  (let ((change (next-single-property-change (point) 'dired-filename nil eol)))
    (cond
     ((and change (< change eol))
      (goto-char change))
     ((re-search-forward directory-listing-before-filename-regexp eol t)
      (goto-char (match-end 0)))
     ((re-search-forward dired-permission-flags-regexp eol t)
      ;; Ha!  There *is* a file.  Our regexp-from-hell just failed to find it.
      (if raise-error
	  (error "Unrecognized line!  Check directory-listing-before-filename-regexp"))
      (beginning-of-line)
      nil)
     (raise-error
      (error "No file on this line")))))

(defun dired-move-to-end-of-filename (&optional no-error)
  ;; Assumes point is at beginning of filename,
  ;; thus the rwx bit re-search-backward below will succeed in *this*
  ;; line if at all.  So, it should be called only after
  ;; (dired-move-to-filename t).
  ;; On failure, signals an error (with non-nil NO-ERROR just returns nil).
  ;; This is the UNIX version.
  (if (get-text-property (point) 'dired-filename)
      (goto-char (next-single-property-change (point) 'dired-filename))
    (let ((opoint (point))
          (used-F (dired-check-switches dired-actual-switches "F" "classify"))
          (eol (line-end-position))
          (hidden (dired--hidden-p))
          file-type executable symlink)
      (if hidden
	  nil
	(save-excursion	;; Find out what kind of file this is:
	  ;; Restrict perm bits to be non-blank,
	  ;; otherwise this matches one char to early (looking backward):
	  ;; "l---------" (some systems make symlinks that way)
	  ;; "----------" (plain file with zero perms)
	  (if (re-search-backward
	       dired-permission-flags-regexp nil t)
	      (setq file-type (char-after (match-beginning 1))
		    symlink (eq file-type ?l)
		    ;; Only with -F we need to know whether it's an executable
		    executable (and
				used-F
				(string-match
				 "[xst]" ;; execute bit set anywhere?
				 (concat
				  (match-string 2)
				  (match-string 3)
				  (match-string 4)))))
	    (or no-error (error "No file on this line"))))
	;; Move point to end of name:
	(if symlink
	    (if (search-forward " -> " eol t)
		(progn
		  (forward-char -4)
		  (and used-F
		       dired-ls-F-marks-symlinks
		       (eq (preceding-char) ?@)	;; did ls really mark the link?
		       (forward-char -1))))
	  (goto-char eol) ;; else not a symbolic link
	  ;; ls -lF marks dirs, sockets, fifos and executables with exactly
	  ;; one trailing character. (Executable bits on symlinks ain't mean
	  ;; a thing, even to ls, but we know it's not a symlink.)
	  (and used-F
	       (or (memq file-type '(?d ?s ?p))
		   executable)
	       (forward-char -1))))
      (or no-error
	  (not (eq opoint (point)))
	  (error "%s" (if hidden
                          (substitute-command-keys
                           "File line is hidden, type \\[dired-hide-subdir] to unhide")
                        "No file on this line")))
      (if (eq opoint (point))
	  nil
	(point)))))


;;; Copy names of marked files into kill-ring

(defun dired-copy-filename-as-kill (&optional arg)
  "Copy names of marked (or next ARG) files into the kill ring.
The names are separated by a space.
With a zero prefix arg, use the absolute file name of each marked file.
With \\[universal-argument], use the file name relative to the Dired buffer's
`default-directory'.  (This still may contain slashes if in a subdirectory.)

If on a subdir headerline, use absolute subdirname instead;
prefix arg and marked files are ignored in this case.

You can then feed the file name(s) to other commands with \\[yank]."
  (interactive "P")
  (let ((string
         (or (dired-get-subdir)
             (mapconcat #'identity
                        (if arg
                            (cond ((zerop (prefix-numeric-value arg))
                                   (dired-get-marked-files))
                                  ((consp arg)
                                   (dired-get-marked-files t))
                                  (t
                                   (dired-get-marked-files
				    'no-dir (prefix-numeric-value arg))))
                          (dired-get-marked-files 'no-dir))
                        " "))))
    (unless (string= string "")
      (if (eq last-command 'kill-region)
          (kill-append string nil)
        (kill-new string))
      (message "%s" string))))


;;; Keeping Dired buffers in sync with the filesystem and with each other

(defun dired-buffers-for-dir (dir &optional file)
  "Return a list of buffers for DIR (top level or in-situ subdir).
If FILE is non-nil, include only those whose wildcard pattern (if any)
matches FILE.
The list is in reverse order of buffer creation, most recent last.
As a side effect, killed dired buffers for DIR are removed from
`dired-buffers'."
  (setq dir (file-name-as-directory dir))
  (let (result buf)
    (dolist (elt dired-buffers)
      (setq buf (cdr elt))
      (cond
       ((null (buffer-name buf))
	;; Buffer is killed - clean up:
	(setq dired-buffers (delq elt dired-buffers)))
       ((dired-in-this-tree-p dir (car elt))
	(with-current-buffer buf
          (and (assoc dir dired-subdir-alist)
	       (or (null file)
		   (if (stringp dired-directory)
		       (let ((wildcards (file-name-nondirectory
					 dired-directory)))
			 (or (zerop (length wildcards))
			     (string-match-p (dired-glob-regexp wildcards)
                                             file)))
		     (member (expand-file-name file dir)
			     (cdr dired-directory))))
               (setq result (cons buf result)))))))
    result))

(defun dired-buffers-for-dir-or-subdir (dir)
  "Return a list of buffers for DIR or a subdirectory thereof.
As a side effect, killed dired buffers for DIR are removed from
`dired-buffers'."
  (setq dir (file-name-as-directory dir))
  (let (result buf)
    (dolist (elt dired-buffers)
      (setq buf (cdr elt))
      (cond
       ((null (buffer-name buf))
	;; Buffer is killed - clean up:
	(setq dired-buffers (delq elt dired-buffers)))
       ((dired-in-this-tree-p (car elt) dir)
        (setq result (cons buf result)))))
    result))

(defun dired-glob-regexp (pattern)
  "Convert glob-pattern PATTERN to a regular expression."
  (let ((matched-in-pattern 0)  ;; How many chars of PATTERN we've handled.
	regexp)
    (while (string-match "[[?*]" pattern matched-in-pattern)
      (let ((op-end (match-end 0))
	    (next-op (aref pattern (match-beginning 0))))
	(setq regexp (concat regexp
			     (regexp-quote
			      (substring pattern matched-in-pattern
					 (match-beginning 0)))))
	(cond ((= next-op ??)
	       (setq regexp (concat regexp "."))
	       (setq matched-in-pattern op-end))
	      ((= next-op ?\[)
	       ;; Fails to handle ^ yet ????
	       (let* ((set-start (match-beginning 0))
		      (set-cont
		       (if (= (aref pattern (1+ set-start)) ?^)
			   (+ 3 set-start)
			 (+ 2 set-start)))
		      (set-end (string-search "]" pattern set-cont))
		      (set (substring pattern set-start (1+ set-end))))
		 (setq regexp (concat regexp set))
		 (setq matched-in-pattern (1+ set-end))))
	      ((= next-op ?*)
	       (setq regexp (concat regexp ".*"))
	       (setq matched-in-pattern op-end)))))
    (concat "\\`"
	    regexp
	    (regexp-quote
	     (substring pattern matched-in-pattern))
	    "\\'")))

(defun dired-advertise ()
  ;;"Advertise in variable `dired-buffers' that we dired `default-directory'."
  ;; With wildcards we actually advertise too much.
  (let ((expanded-default (expand-file-name default-directory)))
    (if (memq (current-buffer) (dired-buffers-for-dir expanded-default))
	t				; we have already advertised ourselves
      (setq dired-buffers
	    (cons (cons expanded-default (current-buffer))
		  dired-buffers)))))

(defun dired-unadvertise (dir)
  ;; Remove DIR from the buffer alist in variable dired-buffers.
  ;; This has the effect of removing any buffer whose main directory is DIR.
  ;; It does not affect buffers in which DIR is a subdir.
  ;; Removing is also done as a side-effect in dired-buffer-for-dir.
  (setq dired-buffers
	(delq (assoc (expand-file-name dir) dired-buffers) dired-buffers)))


;;; Utility functions

(defun dired-in-this-tree-p (file dir)
  ;;"Is FILE part of the directory tree starting at DIR?"
  (let (case-fold-search)
    (string-match-p (concat "^" (regexp-quote dir)) file)))

(define-obsolete-function-alias 'dired-in-this-tree
  'dired-in-this-tree-p "27.1")

(defun dired-normalize-subdir (dir)
  ;; Prepend default-directory to DIR if relative file name.
  ;; dired-get-filename must be able to make a valid file name from a
  ;; file and its directory DIR.
  (file-name-as-directory
   (if (file-name-absolute-p dir)
       dir
     (expand-file-name dir default-directory))))

(defun dired-get-subdir ()
  ;;"Return the subdir name on this line, or nil if not on a headerline."
  ;; Look up in the alist whether this is a headerline.
  (save-excursion
    (let ((cur-dir (dired-current-directory)))
      (beginning-of-line)		; alist stores b-o-l positions
      (and (zerop (- (point)
                     (cdr (assoc cur-dir
                                 dired-subdir-alist))))
           cur-dir))))

(define-obsolete-function-alias 'dired-get-subdir-min 'cdr "27.1")

(defun dired-get-subdir-max (elt)
  (save-excursion
    (goto-char (cdr elt))
    (dired-subdir-max)))

(defun dired-clear-alist ()
  (while dired-subdir-alist
    (set-marker (cdr (car dired-subdir-alist)) nil)
    (setq dired-subdir-alist (cdr dired-subdir-alist))))

(defun dired-subdir-index (dir)
  ;; Return an index into alist for use with nth
  ;; for the sake of subdir moving commands.
  (let (found (index 0) (alist dired-subdir-alist))
    (while alist
      (if (string= dir (car (car alist)))
	  (setq alist nil found t)
	(setq alist (cdr alist) index (1+ index))))
    (if found index nil)))

(defun dired-next-subdir (arg &optional no-error-if-not-found no-skip)
  "Go to next subdirectory, regardless of level."
  ;; Use 0 arg to go to this directory's header line.
  ;; NO-SKIP prevents moving to end of header line, returning whatever
  ;; position was found in dired-subdir-alist.
  (interactive "p")
  (let ((this-dir (dired-current-directory))
	pos index)
    ;; nth with negative arg does not return nil but the first element
    (setq index (- (dired-subdir-index this-dir) arg))
    (setq pos (if (>= index 0)
                  (cdr (nth index dired-subdir-alist))))
    (if pos
	(progn
	  (goto-char pos)
	  (or no-skip (end-of-line))
	  (point))
      (if no-error-if-not-found
	  nil				; return nil if not found
	(error "%s directory" (if (> arg 0) "Last" "First"))))))

(defun dired-build-subdir-alist (&optional switches)
  "Build `dired-subdir-alist' by parsing the buffer.
Returns the new value of the alist.
If optional arg SWITCHES is non-nil, use its value
instead of `dired-actual-switches'."
  (interactive)
  (dired-clear-alist)
  (save-excursion
    (let* ((count 0)
	   (inhibit-read-only t)
	   (buffer-undo-list t)
	   (switches (or switches dired-actual-switches))
	   new-dir-name
	   (R-ftp-base-dir-regex
	    ;; Used to expand subdirectory names correctly in recursive
	    ;; ange-ftp listings.
	    (and (dired-switches-recursive-p switches)
		 (string-match "\\`/.*:\\(/.*\\)" default-directory)
		 (concat "\\`" (match-string 1 default-directory)))))
      (goto-char (point-min))
      (setq dired-subdir-alist nil)
      (while (re-search-forward dired-subdir-regexp nil t)
	;; Avoid taking a file name ending in a colon
	;; as a subdir name.
	(unless (save-excursion
		  (goto-char (match-beginning 0))
		  (beginning-of-line)
		  (forward-char 2)
		  (looking-at-p dired-re-perms))
	  (save-excursion
	    (goto-char (match-beginning 1))
	    (setq new-dir-name
		  (buffer-substring-no-properties (point) (match-end 1))
		  new-dir-name
		  (save-match-data
		    (if (and R-ftp-base-dir-regex
			     (not (string= new-dir-name default-directory))
			     (string-match R-ftp-base-dir-regex new-dir-name))
			(concat default-directory
				(substring new-dir-name (match-end 0)))
		      (expand-file-name new-dir-name))))
	    (delete-region (point) (match-end 1))
	    (insert new-dir-name))
	  (setq count (1+ count))
	  ;; Undo any escaping of newlines and \ by dired-insert-directory.
	  ;; Convert "n" preceded by odd number of \ to newline, and \\ to \.
	  (when (and (dired-switches-escape-p switches)
		     (string-match-p "\\\\" new-dir-name))
	    (let (temp res)
	      (mapc (lambda (char)
		      (cond ((equal char ?\\)
			     (if temp
				 (setq res (concat res "\\")
				       temp nil)
			       (setq temp "\\")))
			    ((and temp (equal char ?n))
			     (setq res (concat res "\n")
				   temp nil))
			    (t
			     (setq res (concat res temp (char-to-string char))
				   temp nil))))
		    new-dir-name)
	      (setq new-dir-name res)))
	  (dired-alist-add-1 new-dir-name
                             ;; Place a sub directory boundary between lines.
                             (save-excursion
                               (goto-char (match-beginning 0))
                               (beginning-of-line)
                               (point-marker)))))
      (if (and (> count 1) (called-interactively-p 'interactive))
	  (message "Buffer includes %d directories" count)))
    ;; We don't need to sort it because it is in buffer order per
    ;; constructionem.  Return new alist:
    dired-subdir-alist))

(defun dired-alist-add-1 (dir new-marker)
  ;; Add new DIR at NEW-MARKER.  Don't sort.
  (setq dired-subdir-alist
	(cons (cons (dired-normalize-subdir dir) new-marker)
	      dired-subdir-alist)))

(defun dired-goto-next-nontrivial-file ()
  ;; Position point on first nontrivial file after point.
  (dired-goto-next-file);; so there is a file to compare with
  (if (stringp dired-trivial-filenames)
      (while (and (not (eobp))
		  (string-match-p dired-trivial-filenames
                                  (file-name-nondirectory
                                   (or (dired-get-filename nil t) ""))))
	(forward-line 1)
	(dired-move-to-filename))))

(defun dired-goto-next-file ()
  (let ((max (1- (dired-subdir-max))))
    (while (and (not (dired-move-to-filename)) (< (point) max))
      (forward-line 1))))

(defun dired-goto-file (file)
  "Go to line describing file FILE in this Dired buffer."
  ;; Return value of point on success, else nil.
  ;; FILE must be an absolute file name.
  ;; Loses if FILE contains control chars like "\007" for which ls
  ;; either inserts "?" or "\\007" into the buffer, so we won't find
  ;; it in the buffer.
  (interactive
   (prog1				; let push-mark display its message
       (list (expand-file-name
	      (read-file-name "Goto file: "
			      (dired-current-directory))))
     (push-mark)))
  (unless (file-name-absolute-p file)
    (error "File name `%s' is not absolute" file))
  (setq file (directory-file-name file)) ; does no harm if not a directory
  (let* ((case-fold-search nil)
	 (dir (file-name-directory file))
	 (found (or
		 ;; First, look for a listing under the absolute name.
		 (save-excursion
		   (goto-char (point-min))
		   (dired-goto-file-1 file file (point-max)))
                 ;; Next, look for it as a relative name with leading
                 ;; subdirectories.  (This happens in Dired buffers
                 ;; created by find-dired, for example.)
                 (save-excursion
                   (goto-char (point-min))
                   (dired-goto-file-1 (file-relative-name file
                                                          default-directory)
                                      file (point-max)))
		 ;; Otherwise, look for it as a relative name, a base
		 ;; name only.  The hair is to get the result of
		 ;; `dired-goto-subdir' without calling it if we don't
		 ;; have any subdirs.
		 (save-excursion
		   (when (if (string= dir (expand-file-name default-directory))
			     (goto-char (point-min))
			   (and (cdr dired-subdir-alist)
				(dired-goto-subdir dir)))
		     (dired-goto-file-1 (file-name-nondirectory file)
					file
					(dired-subdir-max)))))))
    ;; Return buffer position, if found.
    (if found
	(goto-char found))))

(defun dired-goto-file-1 (file full-name limit)
  "Advance to the Dired listing labeled by FILE; return its position.
Return nil if the listing is not found.  If FILE contains
characters that would not appear in a Dired buffer, search using
the quoted forms of those characters.

FULL-NAME specifies the actual file name the listing must have,
as returned by `dired-get-filename'.  LIMIT is the search limit."
  (let (str)
    (setq str (string-replace "\^m" "\\^m"  file))
    (setq str (string-replace "\\" "\\\\" str))
    (and (dired-switches-escape-p dired-actual-switches)
	 (string-match-p "[ \t\n]" str)
	 ;; FIXME: to fix this for embedded control characters etc, we
	 ;; should escape everything that `ls -b' does.
	 (setq str (string-replace " " "\\ "  str)
	       str (string-replace "\t" "\\t" str)
	       str (string-replace "\n" "\\n" str)))
    (let ((found nil)
	  ;; filenames are preceded by SPC, this makes the search faster
	  ;; (e.g. for the filename "-").
	  (search-string (concat " " str)))
      (while (and (not found)
		  (search-forward search-string limit 'move))
	;; Check that we are in the right place.  Match could have
	;; BASE just as initial substring or in permission bits etc.
	(if (equal full-name (dired-get-filename nil t))
	    (setq found (dired-move-to-filename))
	  (forward-line 1)))
      found)))

(defvar dired-find-subdir)

;; FIXME document whatever dired-x is doing.
(defun dired-initial-position (dirname)
  "Where point should go in a new listing of DIRNAME.
Point is assumed to be at the beginning of new subdir line.
It runs the hook `dired-initial-position-hook'."
  (end-of-line)
  (and (featurep 'dired-x) dired-find-subdir
       (dired-goto-subdir dirname))
  (if dired-trivial-filenames (dired-goto-next-nontrivial-file))
  (run-hooks 'dired-initial-position-hook))

;; These are hooks which make tree dired work.
;; They are in this file because other parts of dired need to call them.
;; But they don't call the rest of tree dired unless there are subdirs loaded.

;; This function is called for each retrieved filename.
;; It could stand to be faster, though it's mostly function call
;; overhead.  Avoiding the function call seems to save about 10% in
;; dired-get-filename.  Make it a defsubst?
(defun dired-current-directory (&optional localp)
  "Return the name of the subdirectory to which this line belongs.
This returns a string with trailing slash, like `default-directory'.
Optional argument means return a file name relative to `default-directory',
in which case the value could be an empty string if `default-directory'
is the directory where the file on this line resides."
  (let ((here (point))
	(alist (or dired-subdir-alist
		   ;; probably because called in a non-dired buffer
		   (error "No subdir-alist in %s" (current-buffer))))
	elt dir)
    (while alist
      (setq elt (car alist)
	    dir (car elt)
	    ;; use `<=' (not `<') as subdir line is part of subdir
	    alist (if (<= (cdr elt) here)
		      nil		; found
		    (cdr alist))))
    (if localp
	(dired-make-relative dir default-directory)
      dir)))

;; Subdirs start at the beginning of their header lines and end just
;; before the beginning of the next header line (or end of buffer).

(defun dired-subdir-max ()
  (save-excursion
    (if (or (null (cdr dired-subdir-alist)) (not (dired-next-subdir 1 t t)))
	(point-max)
      (point))))


;;; Deleting files

(defcustom dired-recursive-deletes 'top
  "Whether Dired deletes directories recursively.
If nil, Dired will not delete non-empty directories.
`always' means to delete non-empty directories recursively,
without asking.  This is dangerous!
`top' means to ask for each top-level directory specified by the
Dired deletion command, and delete its subdirectories without
asking.
Any other value means to ask for each directory."
  :type '(choice :tag "Delete non-empty directories"
		 (const :tag "Yes" always)
		 (const :tag "No--only delete empty directories" nil)
		 (const :tag "Confirm for each directory" t)
		 (const :tag "Confirm for each top directory only" top))
  :group 'dired)

(define-obsolete-variable-alias 'dired-re-no-dot
  'directory-files-no-dot-files-regexp "28.1")

;; Delete file, possibly delete a directory and all its files.
;; This function is useful outside of dired.  One could change its name
;; to e.g. recursive-delete-file and put it somewhere else.
(defun dired-delete-file (file &optional recursive trash) "\
Delete FILE or directory (possibly recursively if optional RECURSIVE is true.)
RECURSIVE determines what to do with a non-empty directory.  The effect of
its possible values is:

  nil           -- do not delete.
  `always'      -- delete recursively without asking.
  `top'         -- ask for each directory at top level.
  Anything else -- ask for each sub-directory.

TRASH non-nil means to trash the file instead of deleting, provided
`delete-by-moving-to-trash' (which see) is non-nil."
       ;; This test is equivalent to
       ;; (and (file-directory-p fn) (not (file-symlink-p fn)))
       ;; but more efficient
       (if (not (eq t (car (file-attributes file))))
           (delete-file file trash)
         (let* ((empty-dir-p (null (directory-files
                                    file t
                                    directory-files-no-dot-files-regexp))))
           (if (and recursive (not empty-dir-p))
               (unless (eq recursive 'always)
                 (let ((prompt
                        (format "Recursively %s %s? "
				(if (and trash delete-by-moving-to-trash)
				    "trash"
				  "delete")
				(dired-make-relative file))))
                   (pcase (read-answer
                           prompt
                           '(("yes"  ?y "delete recursively the current directory")
                             ("no"   ?n "skip to next")
                             ("all"  ?! "delete all remaining directories with no more questions")
                             ("quit" ?q "exit")))
                     ("all" (setq recursive 'always dired-recursive-deletes recursive))
                     ("yes" (if (eq recursive 'top) (setq recursive 'always)))
                     ("no" (setq recursive nil))
                     ("quit" (keyboard-quit))
                     (_ (keyboard-quit))))) ; catch all unknown answers
             (setq recursive nil)) ; Empty dir or recursive is nil.
           (delete-directory file recursive trash))))

(defun dired-do-flagged-delete (&optional nomessage)
  "In Dired, delete the files flagged for deletion.
If NOMESSAGE is non-nil, we don't display any message
if there are no flagged files.
`dired-recursive-deletes' controls whether deletion of
non-empty directories is allowed."
  (interactive)
  (let* ((dired-marker-char dired-del-marker)
	 (regexp (dired-marker-regexp))
	 case-fold-search markers)
    (if (save-excursion (goto-char (point-min))
			(re-search-forward regexp nil t))
	(dired-internal-do-deletions
         (nreverse
	  ;; this can't move point since ARG is nil
	  (dired-map-over-marks (cons (dired-get-filename)
                                      (let ((m (point-marker)))
                                        (push m markers)
                                        m))
			        nil))
	 nil t)
      (dolist (m markers) (set-marker m nil))
      (or nomessage
	  (message "(No deletions requested)")))))

(defun dired-do-delete (&optional arg)
  "Delete all marked (or next ARG) files.
`dired-recursive-deletes' controls whether deletion of
non-empty directories is allowed."
  ;; This is more consistent with the file marking feature than
  ;; dired-do-flagged-delete.
  (interactive "P")
  (let (markers)
    (dired-internal-do-deletions
     (nreverse
      ;; this may move point if ARG is an integer
      (dired-map-over-marks (cons (dired-get-filename)
                                  (let ((m (point-marker)))
                                    (push m markers)
                                    m))
                            arg))
     arg t)
    (dolist (m markers) (set-marker m nil))))

(defvar dired-deletion-confirmer 'yes-or-no-p) ; or y-or-n-p?

(defun dired-internal-do-deletions (l arg &optional trash)
  ;; L is an alist of files to delete, with their buffer positions.
  ;; ARG is the prefix arg.
  ;; Filenames are absolute.
  (let* ((files (mapcar #'car l))
	 (count (length l))
	 (succ 0)
	 ;; Bind `dired-recursive-deletes' so that we can change it
	 ;; locally according with the user answer within `dired-delete-file'.
	 (dired-recursive-deletes dired-recursive-deletes)
	 (trashing (and trash delete-by-moving-to-trash)))
    ;; canonicalize file list for pop up
    (setq files (mapcar #'dired-make-relative files))
    (if (dired-mark-pop-up
	 " *Deletions*" 'delete files dired-deletion-confirmer
	 (format "%s %s "
		 (if trashing "Trash" "Delete")
		 (dired-mark-prompt arg files)))
	(save-excursion
          (catch '--delete-cancel
	  (let ((progress-reporter
		 (make-progress-reporter
		  (if trashing "Trashing..." "Deleting...")
		  succ count))
		failures)
	    (while l
	      (goto-char (marker-position (cdr (car l))))
              (dired-move-to-filename)
	      (let ((inhibit-read-only t))
		(condition-case err
		    (let ((fn (car (car l))))
		      (dired-delete-file fn dired-recursive-deletes trash)
		      ;; if we get here, removing worked
		      (setq succ (1+ succ))
		      (progress-reporter-update progress-reporter succ)
		      (dired-fun-in-all-buffers
		       (file-name-directory fn) (file-name-nondirectory fn)
		       #'dired-delete-entry fn))
                  (quit (throw '--delete-cancel (message "OK, canceled")))
		  (error ;; catch errors from failed deletions
		   (dired-log "%s: %s\n" (car err) (error-message-string err))
		   (setq failures (cons (car (car l)) failures)))))
	      (setq l (cdr l)))
	    (if (not failures)
		(progress-reporter-done progress-reporter)
	      (dired-log-summary
	       (format (ngettext "%d of %d deletion failed"
			         "%d of %d deletions failed"
			         count)
		       (length failures) count)
	       failures)))))
      (message "(No deletions performed)")))
  (dired-move-to-filename))

(defun dired-fun-in-all-buffers (directory file fun &rest args)
  "In all buffers dired'ing DIRECTORY, run FUN with ARGS.
If the buffer has a wildcard pattern, check that it matches FILE.
\(FILE does not include a directory component.)
FILE may be nil, in which case ignore it.
Return list of buffers where FUN succeeded (i.e., returned non-nil)."
  (let (success-list)
    (dolist (buf (dired-buffers-for-dir (expand-file-name directory) file))
      (with-current-buffer buf
	(when (apply fun args)
	  (push (buffer-name buf) success-list))))
    ;; FIXME: AFAICT, this return value is not used by any of the callers!
    success-list))

;; Delete the entry for FILE from
(defun dired-remove-entry (file)
  "Remove entry FILE in the current dired buffer.
Note this doesn't delete FILE in the file system.
See `dired-delete-file' in case you wish that."
  (save-excursion
    (and (dired-goto-file file)
	 (let ((inhibit-read-only t))
	   (delete-region (progn (beginning-of-line) (point))
			  (line-beginning-position 2))))))

(defun dired-delete-entry (file)
  "Remove entry FILE in the current dired buffer.
Like `dired-remove-entry' followed by `dired-clean-up-after-deletion'.
Note this doesn't delete FILE in the file system.
See `dired-delete-file' in case you wish that."
  (dired-remove-entry file)
  (dired-clean-up-after-deletion file))

(defvar dired-clean-up-buffers-too)
(defvar dired-clean-confirm-killing-deleted-buffers)

(defun dired-clean-up-after-deletion (fn)
  "Clean up after a deleted file or directory FN.
Removes any expanded subdirectory of deleted directory.  If
`dired-x' is loaded and `dired-clean-up-buffers-too' is non-nil,
kill any buffers visiting those files, prompting for
confirmation.  To disable the confirmation, see
`dired-clean-confirm-killing-deleted-buffers'."
  (save-excursion (and (cdr dired-subdir-alist)
		       (dired-goto-subdir fn)
		       (dired-kill-subdir)))
  ;; Offer to kill buffer of deleted file FN.
  (when (and (featurep 'dired-x) dired-clean-up-buffers-too)
    (let ((buf (get-file-buffer fn)))
      (and buf
           (or (and dired-clean-confirm-killing-deleted-buffers
                    (funcall #'y-or-n-p
                             (format "Kill buffer of %s, too? "
                                     (file-name-nondirectory fn))))
               (not dired-clean-confirm-killing-deleted-buffers))
           (kill-buffer buf)))
    (let ((buf-list (dired-buffers-for-dir-or-subdir
                     (expand-file-name fn))))
      (and buf-list
           (or (and dired-clean-confirm-killing-deleted-buffers
                    (y-or-n-p
                     (format
                      (ngettext
                       "Kill Dired buffer of %s, too? "
                       "Kill Dired buffers of %s and its sub-directories, too? "
                       (length buf-list))
                      (file-name-nondirectory
                       ;; FN may end in a / if `dired-listing-switches'
                       ;; contains -p, so we need to strip that
                       ;; (bug#48301).
                       (directory-file-name fn)))))
               (not dired-clean-confirm-killing-deleted-buffers))
           (dolist (buf buf-list)
             (kill-buffer buf))))))


;;; Confirmation

(defun dired-marker-regexp ()
  (concat "^" (regexp-quote (char-to-string dired-marker-char))))

(defun dired-plural-s (count)
  (if (= 1 count) "" "s"))

(defun dired-mark-prompt (arg files)
  "Return a string suitable for use in a Dired prompt.
ARG is normally the prefix argument for the calling command.
FILES should be a list of file names.

The return value has a form like \"foo.txt\", \"[next 3 files]\",
or \"* [3 files]\"."
  ;; distinguish-one-marked can cause the first element to be just t.
  (if (eq (car files) t) (setq files (cdr files)))
  (let ((count (length files)))
    (if (= count 1)
	(car files)
      ;; more than 1 file:
      (if (integerp arg)
	  ;; abs(arg) = count
	  ;; Perhaps this is nicer, but it also takes more screen space:
	  ;;(format "[%s %d files]" (if (> arg 0) "next" "previous")
	  ;;                        count)
	  (format "[next %d files]" arg)
	(format "%c [%d files]" dired-marker-char count)))))

(defun dired-pop-to-buffer (buf)
  "Pop up buffer BUF in a way suitable for Dired."
  (declare (obsolete pop-to-buffer "24.3"))
  (let ((split-window-preferred-function
	 (lambda (window)
	   (or (and (let ((split-height-threshold 0))
		      (window-splittable-p (selected-window)))
		    ;; Try to split the selected window vertically if
		    ;; that's possible.  (Bug#1806)
		    (split-window-below))
	       ;; Otherwise, try to split WINDOW sensibly.
	       (split-window-sensibly window))))
	pop-up-frames)
    (pop-to-buffer (get-buffer-create buf)))
  ;; See Bug#12281.
  (set-window-start nil (point-min))
  ;; If dired-shrink-to-fit is t, make its window fit its contents.
  (when dired-shrink-to-fit
    ;; Try to not delete window when we want to display less than
    ;; `window-min-height' lines.
    (fit-window-to-buffer (get-buffer-window buf) nil 1 nil nil t)))

(defcustom dired-no-confirm nil
  "A list of symbols for commands Dired should not confirm, or t.
Command symbols are `byte-compile', `chgrp', `chmod', `chown', `compress',
`copy', `delete', `hardlink', `load', `move', `print', `shell', `symlink',
`touch' and `uncompress'.
If t, confirmation is never needed."
  :group 'dired
  :type '(choice (const :tag "Confirmation never needed" t)
		 (set (const byte-compile) (const chgrp)
		      (const chmod) (const chown) (const compress)
		      (const copy) (const delete) (const hardlink)
		      (const load) (const move) (const print)
		      (const shell) (const symlink) (const touch)
		      (const uncompress))))

(defun dired-mark-pop-up (buffer-or-name op-symbol files function &rest args)
  "Return FUNCTION's result on ARGS after showing which files are marked.
Displays the file names in a window showing a buffer named
BUFFER-OR-NAME; the default name being \" *Marked Files*\".  The
window is not shown if there is just one file, `dired-no-confirm'
is t, or OP-SYMBOL is a member of the list in `dired-no-confirm'.

By default, Dired shrinks the display buffer to fit the marked files.
To disable this, use the Customization interface to add a new rule
to `display-buffer-alist' where condition regexp is \"^ \\*Marked Files\\*$\",
action argument symbol is `window-height' and its value is nil.

FILES is the list of marked files.  It can also be (t FILENAME)
in the case of one marked file, to distinguish that from using
just the current file.

FUNCTION should not manipulate files, just read input (an
argument or confirmation)."
  (if (or (eq dired-no-confirm t)
	  (memq op-symbol dired-no-confirm)
	  ;; If FILES defaulted to the current line's file.
	  (= (length files) 1))
      (apply function args)
    (let ((buffer (get-buffer-create (or buffer-or-name " *Marked Files*")))
	  ;; Mark *Marked Files* window as softly-dedicated, to prevent
	  ;; other buffers e.g. *Completions* from reusing it (bug#17554).
	  (display-buffer-mark-dedicated 'soft))
      (with-current-buffer-window
       buffer
       `(display-buffer-below-selected
         (window-height . fit-window-to-buffer)
         (preserve-size . (nil . t))
         (body-function
          . ,#'(lambda (_window)
                 ;; Handle (t FILE) just like (FILE), here.  That value is
                 ;; used (only in some cases), to mean just one file that was
                 ;; marked, rather than the current line file.
                 (dired-format-columns-of-files
                  (if (eq (car files) t) (cdr files) files))
                 (remove-text-properties (point-min) (point-max)
                                         '(mouse-face nil help-echo nil))
                 (setq tab-line-exclude nil))))
       #'(lambda (window _value)
	   (with-selected-window window
	     (unwind-protect
		 (apply function args)
	       (when (window-live-p window)
		 (quit-restore-window window 'kill)))))))))

(defun dired-format-columns-of-files (files)
  (let ((beg (point)))
    (completion--insert-strings files)
    (put-text-property beg (point) 'mouse-face nil)))


;;; Commands to mark or flag file(s) at or near current line

(defun dired-repeat-over-lines (arg function)
  ;; This version skips non-file lines.
  (let ((pos (make-marker)))
    (beginning-of-line)
    (while (and (> arg 0) (not (eobp)))
      (setq arg (1- arg))
      (beginning-of-line)
      (while (and (not (eobp)) (dired-between-files)) (forward-line 1))
      (save-excursion
	(forward-line 1)
	(move-marker pos (1+ (point))))
      (save-excursion (funcall function))
      ;; Advance to the next line--actually, to the line that *was* next.
      ;; (If FUNCTION inserted some new lines in between, skip them.)
      (goto-char pos))
    (while (and (< arg 0) (not (bobp)))
      (setq arg (1+ arg))
      (forward-line -1)
      (while (and (not (bobp)) (dired-between-files)) (forward-line -1))
      (beginning-of-line)
      (save-excursion (funcall function)))
    (move-marker pos nil)
    (dired-move-to-filename)))

(defun dired-between-files ()
  ;; This used to be a regexp match of the `total ...' line output by
  ;; ls, which is slightly faster, but that is not very robust; notably,
  ;; it fails for non-english locales.
  (save-excursion (not (dired-move-to-filename))))

(defun dired-next-marked-file (arg &optional wrap opoint)
  "Move to the ARGth next marked file.
ARG is the numeric prefix argument and defaults to 1.
If WRAP is non-nil, which happens interactively, wrap around
to the beginning of the buffer and search from there, if no
marked file is found after this line.
Optional argument OPOINT specifies the buffer position to
return to if no ARGth marked file is found; it defaults to
the position where this command was invoked."
  (interactive "p\np")
  (or opoint (setq opoint (point)));; return to where interactively started
  (if (if (> arg 0)
	  (re-search-forward dired-re-mark nil t arg)
	(beginning-of-line)
	(re-search-backward dired-re-mark nil t (- arg)))
      (dired-move-to-filename)
    (if (null wrap)
	(progn
	  (goto-char opoint)
	  (error "No next marked file"))
      (message "(Wraparound for next marked file)")
      (goto-char (if (> arg 0) (point-min) (point-max)))
      (dired-next-marked-file arg nil opoint))))

(defun dired-prev-marked-file (arg &optional wrap)
  "Move to the ARGth previous marked file.
ARG is the numeric prefix argument and defaults to 1.
If WRAP is non-nil, which happens interactively, wrap around
to the end of the buffer and search backwards from there, if
no ARGth marked file is found before this line."
  (interactive "p\np")
  (dired-next-marked-file (- arg) wrap))

(defun dired-file-marker (file)
  ;; Return FILE's marker, or nil if unmarked.
  (save-excursion
    (and (dired-goto-file file)
	 (progn
	   (beginning-of-line)
	   (if (not (equal ?\s (following-char)))
	       (following-char))))))

(defun dired-mark-files-in-region (start end)
  (let ((inhibit-read-only t))
    (if (> start end)
        (error "Start > End"))
    (goto-char start)			; assumed at beginning of line
    (while (< (point) end)
      ;; Skip subdir line and following garbage like the `total' line:
      (while (and (< (point) end) (dired-between-files))
	(forward-line 1))
      (if (and (not (looking-at-p dired-re-dot))
	       (dired-get-filename nil t))
	  (progn
	    (delete-char 1)
	    (insert dired-marker-char)))
      (forward-line 1))))

(defun dired-mark (arg &optional interactive)
  "Mark the file at point in the Dired buffer.
If the region is active in Transient Mark mode, mark all files
in the region if `dired-mark-region' is non-nil.
Otherwise, with a prefix arg, mark files on the next ARG lines.

If on a subdir headerline, mark all its files except `.' and `..'.

Use \\[dired-unmark-all-files] to remove all marks
and \\[dired-unmark] on a subdir to remove the marks in
this subdir."
  (interactive (list current-prefix-arg t))
  (cond
   ;; Mark files in the active region.
   ((and interactive dired-mark-region
         (region-active-p)
         (> (region-end) (region-beginning)))
    (save-excursion
      (let ((beg (region-beginning))
	    (end (region-end)))
	(dired-mark-files-in-region
	 (progn (goto-char beg) (line-beginning-position))
	 (progn (goto-char end)
                (if (if (eq dired-mark-region 'line)
                        (not (bolp))
                      (get-text-property (1- (point)) 'dired-filename))
                    (line-end-position)
                  (line-beginning-position)))))))
   ;; Mark subdir files from the subdir headerline.
   ((dired-get-subdir)
    (save-excursion (dired-mark-subdir-files)))
   ;; Mark the current (or next ARG) files.
   (t
    (let ((inhibit-read-only t))
      (dired-repeat-over-lines
       (prefix-numeric-value arg)
       (lambda () (delete-char 1) (insert dired-marker-char)))))))

(defun dired-unmark (arg &optional interactive)
  "Unmark the file at point in the Dired buffer.
If the region is active, unmark all files in the region.
Otherwise, with a prefix arg, unmark files on the next ARG lines.

If looking at a subdir, unmark all its files except `.' and `..'.
If the region is active in Transient Mark mode, unmark all files
in the active region."
  (interactive (list current-prefix-arg t))
  (let ((dired-marker-char ?\s))
    (dired-mark arg interactive)))

(defun dired-flag-file-deletion (arg &optional interactive)
  "In Dired, flag the current line's file for deletion.
If the region is active, flag all files in the region.
Otherwise, with a prefix arg, flag files on the next ARG lines.

If on a subdir headerline, flag all its files except `.' and `..'.
If the region is active in Transient Mark mode, flag all files
in the active region."
  (interactive (list current-prefix-arg t))
  (let ((dired-marker-char dired-del-marker))
    (dired-mark arg interactive)))

(defun dired-unmark-backward (arg)
  "In Dired, move up lines and remove marks or deletion flags there.
Optional prefix ARG says how many lines to unmark/unflag; default
is one line.
If the region is active in Transient Mark mode, unmark all files
in the active region."
  (interactive "p")
  (dired-unmark (- arg) t))

(defun dired-toggle-marks ()
  "Toggle marks: marked files become unmarked, and vice versa.
Flagged files (indicated with flags such as `C' and `D', not
with `*') are not affected, and `.' and `..' are never toggled.
As always, hidden subdirs are not affected.

In Transient Mark mode, if the mark is active, operate on the contents
of the region if `dired-mark-region' is non-nil.  Otherwise, operate
on the whole buffer."
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t)
          (beg (dired-mark--region-beginning))
          (end (dired-mark--region-end)))
      (goto-char beg)
      (while (< (point) end)
        (or (dired-between-files)
            (looking-at-p dired-re-dot)
            ;; use subst instead of insdel because it does not move
            ;; the gap and thus should be faster and because
            ;; other characters are left alone automatically
            (apply #'subst-char-in-region
                   (point) (1+ (point))
                   (if (eq ?\s (following-char))
                       (list ?\s dired-marker-char)
                     (list dired-marker-char ?\s))))
        (forward-line 1)))))


;;; Commands to mark or flag files based on their characteristics or names

(defvar dired-regexp-history nil
  "History list of regular expressions used in Dired commands.")

(defun dired-read-regexp (prompt &optional default history)
  "Read a regexp using `read-regexp'."
  (declare (obsolete read-regexp "24.5"))
  (read-regexp prompt default (or history 'dired-regexp-history)))

(defun dired-mark-files-regexp (regexp &optional marker-char)
  "Mark all files matching REGEXP for use in later commands.
A prefix argument means to unmark them instead.
`.' and `..' are never marked.

If the region is active in Transient Mark mode, mark files
only in the active region if `dired-mark-region' is non-nil.

REGEXP is an Emacs regexp, not a shell wildcard.  Thus, use `\\.o$' for
object files--just `.o' will mark more than you might think."
  (interactive
   (list (read-regexp (concat (if current-prefix-arg "Unmark" "Mark")
                              " files (regexp): ")
                      ;; Add more suggestions into the default list
                      (cons nil (list (dired-get-filename t t)
                                      (and (dired-get-filename nil t)
                                           (concat (regexp-quote
                                                    (file-name-extension
                                                     (dired-get-filename nil t) t))
                                                   "\\'"))))
                      'dired-regexp-history)
	 (if current-prefix-arg ?\s)))
  (let ((dired-marker-char (or marker-char dired-marker-char)))
    (dired-mark-if
     (and (not (looking-at-p dired-re-dot))
	  (not (eolp))			; empty line
	  (let ((fn (dired-get-filename t t)))
	    (and fn (string-match-p regexp fn))))
     "matching file")))

(defun dired-number-of-marked-files ()
  "Display the number and total size of the marked files."
  (interactive)
  (let* ((files (dired-get-marked-files nil nil nil t))
         (nmarked
          (cond ((null (cdr files))
                 0)
                ((and (= (length files) 2)
                      (eq (car files) t))
                 1)
                (t
                 (length files))))
         (size (cl-loop for file in files
                        when (stringp file)
                        sum (file-attribute-size (file-attributes file)))))
    (if (zerop nmarked)
        (message "No marked files")
      (message "%d marked file%s (%s total size)"
               nmarked
               (if (= nmarked 1)
                   ""
                 "s")
               (funcall byte-count-to-string-function size)))))

(defun dired-mark-files-containing-regexp (regexp &optional marker-char)
  "Mark all files with contents containing REGEXP for use in later commands.
A prefix argument means to unmark them instead.
`.' and `..' are never marked.

If the region is active in Transient Mark mode, mark files
only in the active region if `dired-mark-region' is non-nil.

Note that if a file is visited in an Emacs buffer, and
`dired-always-read-filesystem' is nil, this command will
look in the buffer without revisiting the file, so the results might
be inconsistent with the file on disk if its contents has changed
since it was last visited."
  (interactive
   (list (read-regexp (concat (if current-prefix-arg "Unmark" "Mark")
                              " files containing (regexp): ")
                      nil 'dired-regexp-history)
	 (if current-prefix-arg ?\s)))
  (let ((dired-marker-char (or marker-char dired-marker-char)))
    (dired-mark-if
     (and (not (looking-at-p dired-re-dot))
	  (not (eolp))			; empty line
	  (let ((fn (dired-get-filename nil t)))
	    (when (and fn (file-readable-p fn)
		       (not (file-directory-p fn)))
	      (let ((prebuf (get-file-buffer fn)))
		(message "Checking %s" fn)
		;; For now we do it inside emacs
		;; Grep might be better if there are a lot of files
		(if (and prebuf (not dired-always-read-filesystem))
		    (with-current-buffer prebuf
		      (save-excursion
			(goto-char (point-min))
			(re-search-forward regexp nil t)))
		  (with-temp-buffer
		    (insert-file-contents fn)
		    (goto-char (point-min))
		    (re-search-forward regexp nil t)))))))
     "matching file")))

(defun dired-flag-files-regexp (regexp)
  "In Dired, flag all files containing the specified REGEXP for deletion.
The match is against the non-directory part of the filename.  Use `^'
  and `$' to anchor matches.  Exclude subdirs by hiding them.
`.' and `..' are never flagged."
  (interactive (list (read-regexp "Flag for deletion (regexp): "
                                  nil 'dired-regexp-history)))
  (dired-mark-files-regexp regexp dired-del-marker))

(defun dired-mark-symlinks (unflag-p)
  "Mark all symbolic links.
With prefix argument, unmark or unflag all those files.
If the region is active in Transient Mark mode, mark files
only in the active region if `dired-mark-region' is non-nil."
  (interactive "P")
  (let ((dired-marker-char (if unflag-p ?\s dired-marker-char)))
    (dired-mark-if (looking-at-p dired-re-sym) "symbolic link")))

(defun dired-mark-directories (unflag-p)
  "Mark all directory file lines except `.' and `..'.
With prefix argument, unmark or unflag all those files.
If the region is active in Transient Mark mode, mark files
only in the active region if `dired-mark-region' is non-nil."
  (interactive "P")
  (let ((dired-marker-char (if unflag-p ?\s dired-marker-char)))
    (dired-mark-if (and (looking-at-p dired-re-dir)
			(not (looking-at-p dired-re-dot)))
		   "directory file")))

(defun dired-mark-executables (unflag-p)
  "Mark all executable files.
With prefix argument, unmark or unflag all those files.
If the region is active in Transient Mark mode, mark files
only in the active region if `dired-mark-region' is non-nil."
  (interactive "P")
  (let ((dired-marker-char (if unflag-p ?\s dired-marker-char)))
    (dired-mark-if (looking-at-p dired-re-exe) "executable file")))

;; dired-x.el has a dired-mark-sexp interactive command: mark
;; files for which PREDICATE returns non-nil.

(defun dired-flag-auto-save-files (&optional unflag-p)
  "Flag for deletion files whose names suggest they are auto save files.
A prefix argument says to unmark or unflag those files instead.
If the region is active in Transient Mark mode, flag files
only in the active region if `dired-mark-region' is non-nil."
  (interactive "P")
  (let ((dired-marker-char (if unflag-p ?\s dired-del-marker)))
    (dired-mark-if
     ;; It is less than general to check for # here,
     ;; but it's the only way this runs fast enough.
     (and (save-excursion (end-of-line)
                          (or
                           (eq (preceding-char) ?#)
                           ;; Handle executables in case of -F option.
                           ;; We need not worry about the other kinds
                           ;; of markings that -F makes, since they won't
                           ;; appear on real auto-save files.
                           (if (eq (preceding-char) ?*)
                               (progn
                                 (forward-char -1)
                                 (eq (preceding-char) ?#)))))
	  (not (looking-at-p dired-re-dir))
	  (let ((fn (dired-get-filename t t)))
	    (if fn (auto-save-file-name-p
		    (file-name-nondirectory fn)))))
     "auto save file")))

(defcustom dired-garbage-files-regexp
  ;; `log' here is dubious, since it's typically used for useful log
  ;; files, not just TeX stuff.  -- fx
  (concat (regexp-opt
	   '(".log" ".toc" ".dvi" ".bak" ".orig" ".rej" ".aux"))
	  "\\'")
  "Regular expression to match \"garbage\" files for `dired-flag-garbage-files'."
  :type 'regexp
  :group 'dired)

(defun dired-flag-garbage-files ()
  "Flag for deletion all files that match `dired-garbage-files-regexp'."
  (interactive)
  (dired-flag-files-regexp dired-garbage-files-regexp))

(defun dired-flag-backup-files (&optional unflag-p)
  "Flag all backup files (names ending with `~') for deletion.
With prefix argument, unmark or unflag these files.
If the region is active in Transient Mark mode, flag files
only in the active region if `dired-mark-region' is non-nil."
  (interactive "P")
  (let ((dired-marker-char (if unflag-p ?\s dired-del-marker)))
    (dired-mark-if
     ;; Don't call backup-file-name-p unless the last character looks like
     ;; it might be the end of a backup file name.  This isn't very general,
     ;; but it's the only way this runs fast enough.
     (and (save-excursion (end-of-line)
			  ;; Handle executables in case of -F option.
			  ;; We need not worry about the other kinds
			  ;; of markings that -F makes, since they won't
			  ;; appear on real backup files.
			  (if (eq (preceding-char) ?*)
			      (forward-char -1))
			  (eq (preceding-char) ?~))
	  (not (looking-at-p dired-re-dir))
	  (let ((fn (dired-get-filename t t)))
	    (if fn (backup-file-name-p fn))))
     "backup file")))

(defun dired-change-marks (&optional old new)
  "Change all OLD marks to NEW marks.
OLD and NEW are both characters used to mark files."
  (declare (advertised-calling-convention (old new) "28.1"))
  (interactive
   (let* ((cursor-in-echo-area t)
	  (old (progn (message "Change (old mark): ") (read-char)))
	  (new (progn (message  "Change %c marks to (new mark): " old)
		      (read-char))))
     (list old new)))
  (dolist (c (list new old))
    (if (or (not (char-displayable-p c))
            (eq c ?\r))
        (user-error "Invalid mark character: `%c'" c)))
  (let ((string (format "\n%c" old))
        (inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward string nil t)
        (if (if (= old ?\s)
                (save-match-data
                  (dired-get-filename 'no-dir t))
              t)
            (subst-char-in-region (match-beginning 0)
                                  (match-end 0) old new))))))

(defun dired-unmark-all-marks ()
  "Remove all marks from all files in the Dired buffer."
  (interactive)
  (dired-unmark-all-files ?\r))

;; Bound in dired-unmark-all-files
(defvar dired-unmark-all-files-query)

(defun dired-unmark-all-files (mark &optional arg)
  "Remove a specific mark (or any mark) from every file.
After this command, type the mark character to remove,
or type RET to remove all marks.
With prefix arg, query for each marked file.
Type \\[help-command] at that time for help."
  (interactive "cRemove marks (RET means all): \nP")
  (save-excursion
    (let* ((count 0)
	   (inhibit-read-only t) case-fold-search
           dired-unmark-all-files-query
	   (string (format "\n%c" mark))
	   (help-form "\
Type SPC or `y' to unmark one file, DEL or `n' to skip to next,
`!' to unmark all remaining files with no more questions."))
      (goto-char (point-min))
      (while (if (eq mark ?\r)
		 (re-search-forward dired-re-mark nil t)
	       (search-forward string nil t))
	(if (or (not arg)
		(let ((file (dired-get-filename t t)))
		  (and file
		       (dired-query 'dired-unmark-all-files-query
				    "Unmark file `%s'? "
				    file))))
	    (progn (subst-char-in-region (1- (point)) (point)
					 (preceding-char) ?\s)
		   (setq count (1+ count)))))
      (message (if (= count 1) "1 mark removed"
		 "%d marks removed")
	       count))))


;;; Logging failures operating on files, and showing the results

(defvar dired-log-buffer "*Dired log*")

(defun dired-why ()
  "Pop up a buffer with error log output from Dired.
A group of errors from a single command ends with a formfeed.
Thus, use \\[backward-page] to find the beginning of a group of errors."
  (interactive)
  (if (get-buffer dired-log-buffer)
      (let ((owindow (selected-window))
	    (window (display-buffer (get-buffer dired-log-buffer))))
	(unwind-protect
	    (progn
	      (select-window window)
	      (goto-char (point-max))
	      (forward-line -1)
	      (backward-page 1)
	      (recenter 0))
	  (select-window owindow)))))

(defun dired-log (log &rest args)
  ;; Log a message or the contents of a buffer.
  ;; If LOG is a string and there are more args, it is formatted with
  ;; those ARGS.  Usually the LOG string ends with a \n.
  ;; End each bunch of errors with (dired-log t):
  ;; this inserts the current time and buffer at the start of the page,
  ;; and \f (formfeed) at the end.
  (let ((obuf (current-buffer)))
    (with-current-buffer (get-buffer-create dired-log-buffer)
      (goto-char (point-max))
      (let ((inhibit-read-only t))
	(cond ((stringp log)
	       (insert (if args
			   (apply #'format-message log args)
			 log)))
	      ((bufferp log)
	       (insert-buffer-substring log))
	      ((eq t log)
	       (backward-page 1)
	       (unless (bolp)
		 (insert "\n"))
	       (insert (current-time-string)
		       (format-message "\tBuffer `%s'\n" (buffer-name obuf)))
	       (goto-char (point-max))
	       (insert "\f\n")))))))

(defun dired-log-summary (string failures)
  "State a summary of a command's failures, in echo area and log buffer.
STRING is an overall summary of the failures.
FAILURES is a list of file names that we failed to operate on,
or nil if file names are not applicable."
  (if (= (length failures) 1)
      (message "%s"
	       (with-current-buffer dired-log-buffer
		 (goto-char (point-max))
		 (backward-page 1)
		 (if (eolp) (forward-line 1))
		 (buffer-substring (point) (point-max))))
    (message (if failures "%s--type ? for details (%s)"
	       "%s--type ? for details")
	     string failures))
  ;; Log a summary describing a bunch of errors.
  (dired-log (concat "\n" string "\n"))
  (dired-log t))


;;; Sorting

;; Most ls can only sort by name or by date (with -t), nothing else.
;; GNU ls sorts on size with -S, on extension with -X, and unsorted with -U.
;; So anything that does not contain these is sort "by name".

(defvar dired-ls-sorting-switches "SXU"
  "String of `ls' switches (single letters) except \"t\" that influence sorting.

This indicates to Dired which option switches to watch out for because they
will change the sorting order behavior of `ls'.

To change the default sorting order (e.g. add a `-v' option), see the
variable `dired-listing-switches'.  To temporarily override the listing
format, use `\\[universal-argument] \\[dired]'.")

(defvar dired-sort-by-date-regexp
  (concat "\\(\\`\\| \\)-[^- ]*t"
	  ;; `dired-ls-sorting-switches' after -t overrides -t.
	  "[^ " dired-ls-sorting-switches "]*"
	  "\\(\\(\\`\\| +\\)\\(--[^ ]+\\|-[^- t"
	  dired-ls-sorting-switches "]+\\)\\)* *$")
  "Regexp recognized by Dired to set `by date' mode.")

(defvar dired-sort-by-name-regexp
  (concat "\\`\\(\\(\\`\\| +\\)\\(--[^ ]+\\|"
	  "-[^- t" dired-ls-sorting-switches "]+\\)\\)* *$")
  "Regexp recognized by Dired to set `by name' mode.")

(defvar dired-sort-inhibit nil
  "Non-nil means the Dired sort command is disabled.
The idea is to set this buffer-locally in special Dired buffers.")

(defcustom dired-switches-in-mode-line nil
  "How to indicate `dired-actual-switches' in mode-line.
Possible values:
 * nil:      Indicate name-or-date sort order, if possible.
             Else show full switches.
 * `as-is':  Show full switches.
 * Integer:  Show only the first N chars of full switches.
 * Function: Pass `dired-actual-switches' as arg and show result."
  :group 'dired
  :version "28.1"
  :type '(choice
          (const    :tag "Indicate by name or date, else full"   nil)
          (const    :tag "Show full switches"                    as-is)
          (integer  :tag "Show first N chars of switches" :value 10)
          (function :tag "Format with function"           :value identity)))

(defun dired-sort-set-mode-line ()
  "Set mode-line according to option `dired-switches-in-mode-line'."
  (when (eq major-mode 'dired-mode)
    (setq mode-name
	  (let ((case-fold-search  nil))
            (if dired-switches-in-mode-line
                (concat
                 "Dired"
                 (cond ((integerp dired-switches-in-mode-line)
                        (let* ((l1 (length dired-actual-switches))
                               (xs (substring
                                    dired-actual-switches
                                    0 (min l1 dired-switches-in-mode-line)))
                               (l2 (length xs)))
                          (if (zerop l2)
                              xs
                            (concat " " xs (and (< l2  l1) "…")))))
                       ((functionp dired-switches-in-mode-line)
                        (format " %s" (funcall
                                       dired-switches-in-mode-line
                                       dired-actual-switches)))
                       (t (concat " " dired-actual-switches))))
              (cond ((string-match-p dired-sort-by-name-regexp
                                     dired-actual-switches)
                     "Dired by name")
                    ((string-match-p dired-sort-by-date-regexp
                                     dired-actual-switches)
                     "Dired by date")
                    (t (concat "Dired " dired-actual-switches))))))
    (force-mode-line-update)))

(define-obsolete-function-alias 'dired-sort-set-modeline
  #'dired-sort-set-mode-line "24.3")

(defun dired-sort-toggle-or-edit (&optional arg)
  "Toggle sorting by date, and refresh the Dired buffer.
With a prefix argument, edit the current listing switches instead."
  (interactive "P")
  (when dired-sort-inhibit
    (error "Cannot sort this Dired buffer"))
  (if arg
      (dired-sort-other
       (read-string "ls switches (must contain -l): " dired-actual-switches))
    (dired-sort-toggle)))

(defun dired-sort-toggle ()
  ;; Toggle between sort by date/name.  Reverts the buffer.
  (let ((sorting-by-date (string-match-p dired-sort-by-date-regexp
                                         dired-actual-switches))
	;; Regexp for finding (possibly embedded) -t switches.
	(switch-regexp "\\(\\`\\| \\)-\\([a-su-zA-Z]*\\)\\(t\\)\\([^ ]*\\)")
	case-fold-search)
    ;; Remove the -t switch.
    (while (string-match switch-regexp dired-actual-switches)
      (if (and (equal (match-string 2 dired-actual-switches) "")
	       (equal (match-string 4 dired-actual-switches) ""))
	  ;; Remove a stand-alone -t switch.
	  (setq dired-actual-switches
		(replace-match "" t t dired-actual-switches))
	;; Remove a switch of the form -XtY for some X and Y.
	(setq dired-actual-switches
	      (replace-match "" t t dired-actual-switches 3))))

    ;; Now, if we weren't sorting by date before, add the -t switch.
    ;; Some simple-minded ls implementations (eg ftp servers) only
    ;; allow a single option string, so try not to add " -t" if possible.
    (unless sorting-by-date
      (setq dired-actual-switches
            (concat dired-actual-switches
                    (if (string-match-p "\\`-[[:alnum:]]+\\'"
                                        dired-actual-switches)
                        "t"
                      " -t")))))
  (dired-sort-set-mode-line)
  (revert-buffer))

(defun dired-replace-in-string (regexp newtext string)
  ;; Replace REGEXP with NEWTEXT everywhere in STRING and return result.
  ;; NEWTEXT is taken literally---no \\DIGIT escapes will be recognized.
  (declare (obsolete replace-regexp-in-string "28.1"))
  (let ((result "") (start 0) mb me)
    (while (string-match regexp string start)
      (setq mb (match-beginning 0)
	    me (match-end 0)
	    result (concat result (substring string start mb) newtext)
	    start me))
    (concat result (substring string start))))

(defun dired-sort-other (switches &optional no-revert)
  "Specify new `ls' SWITCHES for current Dired buffer.
Values matching `dired-sort-by-date-regexp' or `dired-sort-by-name-regexp'
set the minor mode accordingly, others appear literally in the mode line.
With optional second arg NO-REVERT, don't refresh the listing afterwards."
  (dired-sort-R-check switches)
  (setq dired-actual-switches switches)
  (dired-sort-set-mode-line)
  (or no-revert (revert-buffer)))

(defvar-local dired-subdir-alist-pre-R nil
  "Value of `dired-subdir-alist' before -R switch added.")

(defun dired-sort-R-check (switches)
  "Additional processing of -R in ls option string SWITCHES.
Saves `dired-subdir-alist' when R is set and restores saved value
minus any directories explicitly deleted when R is cleared.
To be called first in body of `dired-sort-other', etc."
  (cond
   ((and (dired-switches-recursive-p switches)
	 (not (dired-switches-recursive-p dired-actual-switches)))
    ;; Adding -R to ls switches -- save `dired-subdir-alist':
    (setq dired-subdir-alist-pre-R dired-subdir-alist))
   ((and (dired-switches-recursive-p dired-actual-switches)
	 (not (dired-switches-recursive-p switches)))
    ;; Deleting -R from ls switches -- revert to pre-R subdirs
    ;; that are still present:
    (setq dired-subdir-alist
	  (if dired-subdir-alist-pre-R
	      (let (subdirs)
		(while dired-subdir-alist-pre-R
		  (if (assoc (caar dired-subdir-alist-pre-R)
			     dired-subdir-alist)
		      ;; subdir still present...
		      (setq subdirs
			    (cons (car dired-subdir-alist-pre-R)
				  subdirs)))
		  (setq dired-subdir-alist-pre-R
			(cdr dired-subdir-alist-pre-R)))
		(reverse subdirs))
	    ;; No pre-R subdir alist, so revert to main directory
	    ;; listing:
	    (list (car (reverse dired-subdir-alist))))))))


;;; Drag and drop support

(defcustom dired-recursive-copies 'top
  "Whether Dired copies directories recursively.
If nil, never copy recursively.
`always' means to copy recursively without asking.
`top' means to ask for each directory at top level.
Any other value means to ask for each directory."
  :type '(choice :tag "Copy directories"
		 (const :tag "No recursive copies" nil)
		 (const :tag "Ask for each directory" t)
		 (const :tag "Ask for each top directory only" top)
		 (const :tag "Copy directories without asking" always))
  :group 'dired)

(defun dired-dnd-popup-notice ()
  (message-box
   "Dired recursive copies are currently disabled.\nSee the variable `dired-recursive-copies'."))

(declare-function x-popup-menu "menu.c" (position menu))

(defun dired-dnd-do-ask-action (uri)
  ;; No need to get actions and descriptions from the source,
  ;; we only have three actions anyway.
  (let ((action (x-popup-menu
		 t
		 (list "What action?"
		       (cons ""
			     '(("Copy here" . copy)
			       ("Move here" . move)
			       ("Link here" . link)
			       "--"
			       ("Cancel" . nil)))))))
    (if action
	(dired-dnd-handle-local-file uri action)
      nil)))

(declare-function dired-relist-entry "dired-aux" (file))
(declare-function make-symbolic-link "fileio.c")

;; Only used when (featurep 'dnd).
(declare-function dnd-get-local-file-name "dnd" (uri &optional must-exist))
(declare-function dnd-get-local-file-uri "dnd" (uri))

(defvar dired-overwrite-confirmed)      ;Defined in dired-aux.

(defun dired-dnd-handle-local-file (uri action)
  "Copy, move or link a file to the Dired directory.
URI is the file to handle, ACTION is one of copy, move, link or ask.
Ask means pop up a menu for the user to select one of copy, move or link."
  (require 'dired-aux)
  (let* ((from (dnd-get-local-file-name uri t))
	 (to (when from
	       (concat (dired-current-directory)
		       (file-name-nondirectory from)))))
    (when from
      (cond ((eq action 'ask)
	     (dired-dnd-do-ask-action uri))
	    ;; If copying a directory and dired-recursive-copies is
	    ;; nil, dired-copy-file fails.  Pop up a notice.
	    ((and (memq action '(copy private))
		  (file-directory-p from)
		  (not dired-recursive-copies))
	     (dired-dnd-popup-notice))
	    ((memq action '(copy private move link))
	     (let ((overwrite (and (file-exists-p to)
				   (y-or-n-p
				    (format-message
				     "Overwrite existing file `%s'? " to))))
		   ;; Binding dired-overwrite-confirmed to nil makes
		   ;; dired-handle-overwrite a no-op.  We instead use
		   ;; y-or-n-p, which pops a graphical menu.
		   dired-overwrite-confirmed backup-file)
	       (when (and overwrite
			  ;; d-b-o is defined in dired-aux.
			  (boundp 'dired-backup-overwrite)
			  dired-backup-overwrite
			  (setq backup-file
				(car (find-backup-file-name to)))
			  (or (eq dired-backup-overwrite 'always)
			      (y-or-n-p
			       (format-message
				"Make backup for existing file `%s'? " to))))
		 (rename-file to backup-file 0)
		 (dired-relist-entry backup-file))
	       (cond ((memq action '(copy private))
		      (dired-copy-file from to overwrite))
		     ((eq action 'move)
		      (dired-rename-file from to overwrite))
		     ((eq action 'link)
		      (make-symbolic-link from to overwrite)))
	       (dired-relist-entry to)
	       action))))))

(defun dired-dnd-handle-file (uri action)
  "Copy, move or link a file to the Dired directory if it is a local file.
URI is the file to handle.  If the hostname in the URI isn't local, do nothing.
ACTION is one of copy, move, link or ask.
Ask means pop up a menu for the user to select one of copy, move or link."
  (let ((local-file (dnd-get-local-file-uri uri)))
    (if local-file (dired-dnd-handle-local-file local-file action)
      nil)))


;;; Desktop support

(eval-when-compile (require 'desktop))
(declare-function desktop-file-name "desktop" (filename dirname))

(defun dired-desktop-buffer-misc-data (dirname)
  "Auxiliary information to be saved in desktop file."
  (cons
   ;; Value of `dired-directory'.
   (if (consp dired-directory)
       ;; Directory name followed by list of files.
       (cons (desktop-file-name (car dired-directory) dirname)
             (cdr dired-directory))
     ;; Directory name, optionally with shell wildcard.
     (desktop-file-name dired-directory dirname))
   ;; Subdirectories in `dired-subdir-alist'.
   (cdr
    (nreverse
     (mapcar
      (lambda (f) (desktop-file-name (car f) dirname))
      dired-subdir-alist)))))

(defun dired-restore-desktop-buffer (_file-name
                                     _buffer-name
                                     misc-data)
  "Restore a Dired buffer specified in a desktop file."
  ;; First element of `misc-data' is the value of `dired-directory'.
  ;; This value is a directory name, optionally with shell wildcard or
  ;; a directory name followed by list of files.
  (let* ((dired-dir (car misc-data))
         (dir (if (consp dired-dir) (car dired-dir) dired-dir)))
    (if (file-directory-p (file-name-directory dir))
        (with-demoted-errors "Desktop: Problem restoring directory: %S"
          (dired dired-dir)
          ;; The following elements of `misc-data' are the keys
          ;; from `dired-subdir-alist'.
          (mapc #'dired-maybe-insert-subdir (cdr misc-data))
          (current-buffer))
      (message "Desktop: Directory %s no longer exists." dir)
      (when desktop-missing-file-warning (sit-for 1))
      nil)))

(add-to-list 'desktop-buffer-mode-handlers
	     '(dired-mode . dired-restore-desktop-buffer))


;;; Jump to Dired

(defvar archive-superior-buffer)
(defvar tar-superior-buffer)
(declare-function dired-omit-mode "dired-x" (&optional arg))

;;;###autoload
(defun dired-jump (&optional other-window file-name)
  "Jump to Dired buffer corresponding to current buffer.
If in a buffer visiting a file, Dired that file's directory and
move to that file's line in the directory listing.

If the current buffer isn't visiting a file, Dired `default-directory'.

If in Dired already, pop up a level and goto old directory's line.
In case the proper Dired file line cannot be found, refresh the dired
buffer and try again.

When OTHER-WINDOW is non-nil, jump to Dired buffer in other window.

When FILE-NAME is non-nil, jump to its line in Dired.
Interactively with prefix argument, read FILE-NAME."
  (interactive
   (list nil (and current-prefix-arg
                  (read-file-name "Jump to Dired file: "))))
  (cond
   ((and (bound-and-true-p archive-subfile-mode)
         (buffer-live-p archive-superior-buffer))
    (switch-to-buffer archive-superior-buffer))
   ((and (bound-and-true-p tar-subfile-mode)
         (buffer-live-p tar-superior-buffer))
    (switch-to-buffer tar-superior-buffer))
   (t
    ;; Expand file-name before `dired-goto-file' call:
    ;; `dired-goto-file' requires its argument to be an absolute
    ;; file name; the result of `read-file-name' could be
    ;; an abbreviated file name (Bug#24409).
    (let* ((file (or (and file-name (expand-file-name file-name))
                     buffer-file-name))
           (dir (if file (file-name-directory file) default-directory)))
      (if (and (eq major-mode 'dired-mode) (null file-name))
          (progn
            (setq dir (dired-current-directory))
            (dired-up-directory other-window)
            (unless (dired-goto-file dir)
              ;; refresh and try again
              (dired-insert-subdir (file-name-directory dir))
              (dired-goto-file dir)))
        (if other-window
            (dired-other-window dir)
          (dired dir))
        (if file
            (or (dired-goto-file file)
                ;; refresh and try again
                (progn
                  (dired-insert-subdir (file-name-directory file))
                  (dired-goto-file file))
                ;; Toggle omitting, if it is on, and try again.
                (when (bound-and-true-p dired-omit-mode)
                  (dired-omit-mode)
                  (dired-goto-file file)))))))))

;;;###autoload
(defun dired-jump-other-window (&optional file-name)
  "Like \\[dired-jump] (`dired-jump') but in other window."
  (interactive
   (list (and current-prefix-arg
	      (read-file-name "Jump to Dired file: "))))
  (dired-jump t file-name))

(provide 'dired)

(run-hooks 'dired-load-hook)		; for your customizations

;;; dired.el ends here
