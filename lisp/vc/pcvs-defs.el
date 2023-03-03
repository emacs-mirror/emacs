;;; pcvs-defs.el --- variable definitions for PCL-CVS  -*- lexical-binding: t; -*-

;; Copyright (C) 1991-2023 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: pcl-cvs
;; Package: pcvs

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


;;; Code:

(require 'pcvs-util)

;;;; -------------------------------------------------------
;;;;	    START OF THINGS TO CHECK WHEN INSTALLING

(defvar cvs-program "cvs"
  "Name or full path of the cvs executable.")

(defvar cvs-version
  ;; With the divergence of the CVSNT codebase and version numbers, this is
  ;; not really good any more.
  (ignore-errors
    (with-temp-buffer
      (call-process cvs-program nil t nil "-v")
      (goto-char (point-min))
      (when (re-search-forward "(CVS\\(NT\\)?) \\([0-9]+\\)\\.\\([0-9]+\\)"
                               nil t)
	(cons (string-to-number (match-string 1))
	      (string-to-number (match-string 2))))))
  "Version of `cvs' installed on your system.
It must be in the (MAJOR . MINOR) format.")

;; FIXME: this is only used by cvs-mode-diff-backup
(defvar cvs-diff-program (or (and (boundp 'diff-command) diff-command) "diff")
  "Name or full path of the best diff program you've got.
NOTE:  there are some nasty bugs in the context diff variants of some vendor
versions, such as the one in SunOS-4.")

;;;;	     END OF THINGS TO CHECK WHEN INSTALLING
;;;; --------------------------------------------------------

;;;;	User configuration variables:

(defgroup pcl-cvs nil
  "Special support for the CVS versioning system."
  :version "21.1"
  :group 'tools
  :prefix "cvs-")

;;
;;  cvsrc options
;;

(defcustom cvs-cvsrc-file (convert-standard-filename "~/.cvsrc")
  "Path to your cvsrc file."
  :type '(file))

(defvar cvs-shared-start 4
  "Index of the first shared flag.
If set to 4, for instance, a numeric argument smaller than 4 will
select a non-shared flag, while a numeric argument greater than 3
will select a shared-flag.")

(defvar cvs-shared-flags (make-list cvs-shared-start nil)
  "List of flags whose settings is shared among several commands.")

(defvar cvs-cvsroot nil
  "Specifies where the (current) cvs master repository is.
Overrides the environment variable $CVSROOT by sending \" -d dir\" to
all CVS commands.  This switch is useful if you have multiple CVS
repositories.  It can be set interactively with \\[cvs-change-cvsroot.]
There is no need to set this if $CVSROOT is set to a correct value.")

(defcustom cvs-auto-remove-handled nil
  "If up-to-date files should be acknowledged automatically.
If t, they will be removed from the *cvs* buffer after every command.
If `delayed', they will be removed from the *cvs* buffer before every command.
If `status', they will only be removed after a `cvs-mode-status' command.
Else, they will never be automatically removed from the *cvs* buffer."
  :type '(choice (const nil) (const status) (const delayed) (const t)))

(defcustom cvs-auto-remove-directories 'handled
  "If `all', directory entries will never be shown.
If `handled', only non-handled directories will be shown.
If `empty', only non-empty directories will be shown."
  :type '(choice (const :tag "No" nil) (const all) (const handled) (const empty)))

(defcustom cvs-auto-revert t
  "Non-nil if changed files should automatically be reverted."
  :type '(boolean))

(defcustom cvs-sort-ignore-file t
  "Non-nil if `cvs-mode-ignore' should sort the .cvsignore automatically."
  :type '(boolean))

(defcustom cvs-force-dir-tag t
  "If non-nil, tagging can only be applied to directories.
Tagging should generally be applied a directory at a time, but sometimes it is
useful to be able to tag a single file.  The normal way to do that is to use
`cvs-mode-force-command' so as to temporarily override the restrictions."
  :type '(boolean))

(defcustom cvs-default-ignore-marks nil
  "Non-nil if cvs mode commands should ignore any marked files.
Normally they run on the files that are marked (with `cvs-mode-mark'),
or the file under the cursor if no files are marked.  If this variable
is set to a non-nil value they will by default run on the file on the
current line.  See also `cvs-invert-ignore-marks'."
  :type '(boolean))

(defcustom cvs-invert-ignore-marks
  (let ((l ()))
    (unless (equal cvs-default-ignore-marks t)
      (push "diff" l))
    (when (and cvs-force-dir-tag (not cvs-default-ignore-marks))
      (push "tag" l))
    l)
  "List of cvs commands that invert the default ignore-mark behavior.
Commands in this set will use the opposite default from the one set
in `cvs-default-ignore-marks'."
  :type '(set (const "diff")
	      (const "tag")
	      (const "ignore")))

(defcustom cvs-confirm-removals t
  "Ask for confirmation before removing files.
Non-nil means that PCL-CVS will ask confirmation before removing files
except for files whose content can readily be recovered from the repository.
A value of `list' means that the list of files to be deleted will be
displayed when asking for confirmation."
  :type '(choice (const list)
		 (const t)
		 (const nil)))

(defcustom cvs-add-default-message nil
  "Default message to use when adding files.
If set to nil, `cvs-mode-add' will always prompt for a message."
  :type '(choice (const :tag "Prompt" nil)
		 (string)))

(defcustom cvs-find-file-and-jump nil
  "Jump to the modified area when finding a file.
If non-nil, `cvs-mode-find-file' will place the cursor at the beginning of
the modified area.  If the file is not locally modified, this will obviously
have no effect."
  :type '(boolean))

(defcustom cvs-buffer-name-alist
  '(("diff" "*cvs-diff*" diff-mode)
    ("status" "*cvs-info*" cvs-status-mode)
    ("tree" "*cvs-info*" cvs-status-mode)
    ("message" "*cvs-commit*" nil log-edit)
    ("log" "*cvs-info*" log-view-mode))
  "Buffer name and mode to be used for each command.
This is a list of elements of the form

	(CMD BUFNAME MODE &optional POSTPROC)

CMD is the name of the command.
BUFNAME is an expression that should evaluate to a string used as
  a buffer name.  It can use the variable CMD if it wants to.
MODE is the command to use to setup the buffer.
POSTPROC is a function that should be executed when the command terminates

The CMD used for `cvs-mode-commit' is \"message\".  For that special
  case, POSTPROC is called just after MODE with special arguments."
  :type '(repeat
	  (list (choice (const "diff")
			(const "status")
			(const "tree")
			(const "message")
			(const "log")
			(string))
		(choice (const "*vc-diff*")
			(const "*cvs-info*")
			(const "*cvs-commit*")
			(const (expand-file-name "*cvs-commit*"))
			(const (format "*cvs-%s*" cmd))
			(const (expand-file-name (format "*cvs-%s*" cmd)))
			(sexp :value "my-cvs-info-buffer")
			(const nil))
		(choice (function-item diff-mode)
			(function-item cvs-edit-mode)
			(function-item cvs-status-mode)
			function
			(const nil))
		(set :inline t
		     (choice (function-item cvs-status-cvstrees)
			     (function-item cvs-status-trees)
			     function)))))

(defvar cvs-buffer-name '(expand-file-name "*cvs*" dir) ;; "*cvs*"
  "Name of the cvs buffer.
This expression will be evaluated in an environment where DIR is set to
the directory name of the cvs buffer.")

(defvar cvs-temp-buffer-name
  ;; Was '(expand-file-name " *cvs-tmp*" dir), but that causes them to
  ;; become non-hidden if uniquification is done `forward'.
  " *cvs-tmp*"
  "Name of the cvs temporary buffer.
Output from cvs is placed here for asynchronous commands.")

(defcustom cvs-idiff-imerge-handlers
  (if (fboundp 'ediff)
      '(cvs-ediff-diff . cvs-ediff-merge)
    '(cvs-emerge-diff . cvs-emerge-merge))
  "Pair of functions to be used for resp. diff'ing and merg'ing interactively."
  :type '(choice (const :tag "Ediff" (cvs-ediff-diff . cvs-ediff-merge))
		 (const :tag "Emerge" (cvs-emerge-diff . cvs-emerge-merge))))

(defvar cvs-mode-hook nil
  "Run after `cvs-mode' was setup.")


;;;;
;;;; Internal variables for the *cvs* buffer.
;;;;

(defcustom cvs-reuse-cvs-buffer 'subdir
  "When to reuse an existing cvs buffer.
Alternatives are:
 `current': just reuse the current buffer if it is a cvs buffer
 `samedir': reuse any cvs buffer displaying the same directory
 `subdir':  or reuse any cvs buffer displaying any sub- or super- directory
 `always':  reuse any cvs buffer."
  :type '(choice (const always) (const subdir) (const samedir) (const current)))

(defvar cvs-temp-buffer nil
  "(Buffer local) The temporary buffer associated with this *cvs* buffer.")

(defvar cvs-lock-file nil
  "Full path to a lock file that CVS is waiting for (or was waiting for).
This variable is buffer local and only used in the *cvs* buffer.")

(defvar cvs-lock-file-regexp "^#cvs\\.\\([trw]fl\\.[-.a-z0-9]+\\|lock\\)\\'"
  "Regexp matching the possible names of locks in the CVS repository.")

(defconst cvs-cursor-column 22
  "Column to position cursor in, in `cvs-mode'.")

;;;;
;;;; Global internal variables
;;;;

(defconst cvs-vendor-branch "1.1.1"
  "The default branch used by CVS for vendor code.")

(defvar cvs-buffer nil
  "(Buffer local) The *cvs* buffer associated with this buffer.")
(put 'cvs-buffer 'permanent-local t)
;;(make-variable-buffer-local 'cvs-buffer)

(defvar cvs-minor-wrap-function nil
  "Function to call when switching to the *cvs* buffer.
Takes two arguments:
- a *cvs* buffer.
- a zero-arg function which is guaranteed not to switch buffer.
It is expected to call the function.")
;;(make-variable-buffer-local 'cvs-minor-wrap-function)

(defvar cvs-minor-current-files)
;;"Current files in a `cvs-minor-mode' buffer."
;; This should stay `void' because we want to be able to tell the difference
;; between an empty list and no list at all.

(defconst cvs-pcl-cvs-dirchange-re "^pcl-cvs: descending directory \\(.*\\)$")

;;;;
;;;; autoload the global menu
;;;;

;;;###autoload
(defvar cvs-global-menu
  (let ((m (make-sparse-keymap "PCL-CVS")))
    (define-key m [status]
      `(menu-item ,(purecopy "Directory Status") cvs-status
		  :help ,(purecopy "A more verbose status of a workarea")))
    (define-key m [checkout]
      `(menu-item ,(purecopy "Checkout Module") cvs-checkout
		  :help ,(purecopy "Check out a module from the repository")))
    (define-key m [update]
      `(menu-item ,(purecopy "Update Directory") cvs-update
		  :help ,(purecopy "Fetch updates from the repository")))
    (define-key m [examine]
      `(menu-item ,(purecopy "Examine Directory") cvs-examine
		  :help ,(purecopy "Examine the current state of a workarea")))
    (fset 'cvs-global-menu m))
  "Global menu used by PCL-CVS.")


;; cvs-1.10 and above can take file arguments in other directories
;; while others need to be executed once per directory
(defvar cvs-execute-single-dir
  (if (or (null cvs-version)
          (or (>= (cdr cvs-version) 10) (> (car cvs-version) 1)))
      ;; Supposedly some recent versions of CVS output some directory info
      ;; as they recurse down the tree, but it's not good enough in the case
      ;; where we run "cvs status foo bar/foo".
      '("status")
    t)
  "Whether cvs commands should be executed a directory at a time.
If a list, specifies for which commands the single-dir mode should be used.
If t, single-dir mode should be used for all operations.

CVS versions before 1.10 did not allow passing them arguments in different
directories, so PCL-CVS checks what version you're using to determine
whether to use the new feature or not.
Sadly, even with a new cvs executable, if you connect to an older cvs server
\(typically a cvs-1.9 on the server), the old restriction applies.  In such
a case the sanity check made by PCL-CVS fails and you will have to manually
set this variable to t (until the cvs server is upgraded).
When the above problem occurs, PCL-CVS should (hopefully) catch cvs' error
message and replace it with a message telling you to change this variable.")

;;
(provide 'pcvs-defs)

;;; pcvs-defs.el ends here
