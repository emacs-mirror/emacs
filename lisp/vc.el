;;; vc.el --- drive a version-control system from within Emacs

;; Copyright (C) 1992,93,94,95,96,97,98,2000,2001  Free Software Foundation, Inc.

;; Author:     FSF (see below for full credits)
;; Maintainer: Andre Spiegel <spiegel@gnu.org>
;; Keywords: tools

;; $Id: vc.el,v 1.311.4.4 2001/11/15 10:56:37 spiegel Exp $

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

;;; Credits:

;; VC was initially designed and implemented by Eric S. Raymond
;; <esr@snark.thyrsus.com>.  Over the years, many people have
;; contributed substantial amounts of work to VC.  These include:
;;   Per Cederqvist <ceder@lysator.liu.se>
;;   Paul Eggert <eggert@twinsun.com>
;;   Sebastian Kremer <sk@thp.uni-koeln.de>
;;   Martin Lorentzson <martinl@gnu.org>
;;   Dave Love <fx@gnu.org>
;;   Stefan Monnier <monnier@cs.yale.edu>
;;   Andre Spiegel <spiegel@gnu.org>
;;   Richard Stallman <rms@gnu.org>
;;   ttn@netcom.com

;;; Commentary:

;; This mode is fully documented in the Emacs user's manual.
;;
;; Supported version-control systems presently include SCCS, RCS, and CVS.
;;
;; Some features will not work with old RCS versions.  Where
;; appropriate, VC finds out which version you have, and allows or
;; disallows those features (stealing locks, for example, works only
;; from 5.6.2 onwards).
;; Even initial checkins will fail if your RCS version is so old that ci
;; doesn't understand -t-; this has been known to happen to people running
;; NExTSTEP 3.0.
;;
;; You can support the RCS -x option by customizing vc-rcs-master-templates.
;;
;; Proper function of the SCCS diff commands requires the shellscript vcdiff
;; to be installed somewhere on Emacs's path for executables.
;;
;; If your site uses the ChangeLog convention supported by Emacs, the
;; function vc-comment-to-change-log should prove a useful checkin hook.
;;
;; The vc code maintains some internal state in order to reduce expensive
;; version-control operations to a minimum.  Some names are only computed
;; once.  If you perform version control operations with RCS/SCCS/CVS while
;; vc's back is turned, or move/rename master files while vc is running,
;; vc may get seriously confused.  Don't do these things!
;;
;; Developer's notes on some concurrency issues are included at the end of
;; the file.
;;
;; ADDING SUPPORT FOR OTHER BACKENDS
;;
;; VC can use arbitrary version control systems as a backend.  To add
;; support for a new backend named SYS, write a library vc-sys.el that
;; contains functions of the form `vc-sys-...' (note that SYS is in lower
;; case for the function and library names).  VC will use that library if
;; you put the symbol SYS somewhere into the list of
;; `vc-handled-backends'.  Then, for example, if `vc-sys-registered'
;; returns non-nil for a file, all SYS-specific versions of VC commands
;; will be available for that file.
;;
;; VC keeps some per-file information in the form of properties (see
;; vc-file-set/getprop in vc-hooks.el).  The backend-specific functions
;; do not generally need to be aware of these properties.  For example,
;; `vc-sys-workfile-version' should compute the workfile version and
;; return it; it should not look it up in the property, and it needn't
;; store it there either.  However, if a backend-specific function does
;; store a value in a property, that value takes precedence over any
;; value that the generic code might want to set (check for uses of 
;; the macro `with-vc-properties' in vc.el).
;;
;; In the list of functions below, each identifier needs to be prepended
;; with `vc-sys-'.  Some of the functions are mandatory (marked with a
;; `*'), others are optional (`-').
;;
;; STATE-QUERYING FUNCTIONS
;;
;; * registered (file)
;;
;;   Return non-nil if FILE is registered in this backend.
;;
;; * state (file) 
;;
;;   Return the current version control state of FILE.  For a list of
;;   possible values, see `vc-state'.  This function should do a full and
;;   reliable state computation; it is usually called immediately after
;;   C-x v v.  If you want to use a faster heuristic when visiting a
;;   file, put that into `state-heuristic' below.
;;
;; - state-heuristic (file)
;;
;;   If provided, this function is used to estimate the version control
;;   state of FILE at visiting time.  It should be considerably faster
;;   than the implementation of `state'.  For a list of possible values,
;;   see the doc string of `vc-state'.
;;
;; - dir-state (dir)
;;
;;   If provided, this function is used to find the version control state
;;   of all files in DIR in a fast way.  The function should not return
;;   anything, but rather store the files' states into the corresponding
;;   `vc-state' properties.
;;
;; * workfile-version (file)
;;
;;   Return the current workfile version of FILE.
;;
;; - latest-on-branch-p (file)
;;
;;   Return non-nil if the current workfile version of FILE is the latest
;;   on its branch.  The default implementation always returns t, which
;;   means that working with non-current versions is not supported by
;;   default.
;;
;; * checkout-model (file)
;;
;;   Indicate whether FILE needs to be "checked out" before it can be
;;   edited.  See `vc-checkout-model' for a list of possible values.
;;
;; - workfile-unchanged-p (file)
;;
;;   Return non-nil if FILE is unchanged from its current workfile
;;   version.  This function should do a brief comparison of FILE's
;;   contents with those of the master version.  If the backend does not
;;   have such a brief-comparison feature, the default implementation of
;;   this function can be used, which delegates to a full
;;   vc-BACKEND-diff.
;;
;; - mode-line-string (file)
;;
;;   If provided, this function should return the VC-specific mode line
;;   string for FILE.  The default implementation deals well with all
;;   states that `vc-state' can return.
;;
;; - dired-state-info (file)
;;
;;   Translate the `vc-state' property of FILE into a string that can be
;;   used in a vc-dired buffer.  The default implementation deals well
;;   with all states that `vc-state' can return.
;;
;; STATE-CHANGING FUNCTIONS
;;
;; * register (file &optional rev comment)
;;
;;   Register FILE in this backend.  Optionally, an initial revision REV
;;   and an initial description of the file, COMMENT, may be specified.
;;
;; - responsible-p (file)
;;
;;   Return non-nil if this backend considers itself "responsible" for
;;   FILE, which can also be a directory.  This function is used to find
;;   out what backend to use for registration of new files and for things
;;   like change log generation.  The default implementation always
;;   returns nil.
;;
;; - could-register (file)
;;
;;   Return non-nil if FILE could be registered under this backend.  The
;;   default implementation always returns t.
;;
;; - receive-file (file rev)
;;
;;   Let this backend "receive" a file that is already registered under
;;   another backend.  The default implementation simply calls `register'
;;   for FILE, but it can be overridden to do something more specific,
;;   e.g. keep revision numbers consistent or choose editing modes for
;;   FILE that resemble those of the other backend.
;;
;; - unregister (file)
;;
;;   Unregister FILE from this backend.  This is only needed if this
;;   backend may be used as a "more local" backend for temporary editing.
;;
;; * checkin (file rev comment)
;;
;;   Commit changes in FILE to this backend.  If REV is non-nil, that
;;   should become the new revision number.  COMMENT is used as a
;;   check-in comment.
;;
;; * checkout (file &optional editable rev destfile)
;;
;;   Check out revision REV of FILE into the working area.  If EDITABLE
;;   is non-nil, FILE should be writable by the user and if locking is
;;   used for FILE, a lock should also be set.  If REV is non-nil, that
;;   is the revision to check out (default is current workfile version);
;;   if REV is the empty string, that means to check out the head of the
;;   trunk.  If optional arg DESTFILE is given, it is an alternate
;;   filename to write the contents to.
;;
;; * revert (file &optional contents-done)
;;
;;   Revert FILE back to the current workfile version.  If optional
;;   arg CONTENTS-DONE is non-nil, then the contents of FILE have
;;   already been reverted from a version backup, and this function
;;   only needs to update the status of FILE within the backend.
;;
;; - cancel-version (file editable)
;;
;;   Cancel the current workfile version of FILE, i.e. remove it from the
;;   master.  EDITABLE non-nil means that FILE should be writable
;;   afterwards, and if locking is used for FILE, then a lock should also
;;   be set.  If this function is not provided, trying to cancel a
;;   version is caught as an error.
;;
;; - merge (file rev1 rev2)
;;
;;   Merge the changes between REV1 and REV2 into the current working file.
;;
;; - merge-news (file)
;;
;;   Merge recent changes from the current branch into FILE.
;;
;; - steal-lock (file &optional version)
;;
;;   Steal any lock on the current workfile version of FILE, or on
;;   VERSION if that is provided.  This function is only needed if
;;   locking is used for files under this backend, and if files can
;;   indeed be locked by other users.
;;
;; HISTORY FUNCTIONS
;;
;; * print-log (file)
;;
;;   Insert the revision log of FILE into the *vc* buffer.
;;
;; - show-log-entry (version)
;;
;;   If provided, search the log entry for VERSION in the current buffer,
;;   and make sure it is displayed in the buffer's window.  The default
;;   implementation of this function works for RCS-style logs.
;;
;; - wash-log (file)
;;
;;   Remove all non-comment information from the output of print-log.  The
;;   default implementation of this function works for RCS-style logs.
;;
;; - logentry-check ()
;;
;;   If defined, this function is run to find out whether the user
;;   entered a valid log entry for check-in.  The log entry is in the
;;   current buffer, and if it is not a valid one, the function should
;;   throw an error.
;;
;; - comment-history (file)
;;
;;   Return a string containing all log entries that were made for FILE.
;;   This is used for transferring a file from one backend to another,
;;   retaining comment information.  The default implementation of this
;;   function does this by calling print-log and then wash-log, and
;;   returning the resulting buffer contents as a string.
;;
;; - update-changelog (files)
;;
;;   Using recent log entries, create ChangeLog entries for FILES, or for
;;   all files at or below the default-directory if FILES is nil.  The
;;   default implementation runs rcs2log, which handles RCS- and
;;   CVS-style logs.
;;
;; * diff (file &optional rev1 rev2)
;;
;;   Insert the diff for FILE into the *vc-diff* buffer.  If REV1 and REV2
;;   are non-nil, report differences from REV1 to REV2.  If REV1 is nil,
;;   use the current workfile version (as found in the repository) as the
;;   older version; if REV2 is nil, use the current workfile contents as
;;   the newer version.  This function should return a status of either 0
;;   (no differences found), or 1 (either non-empty diff or the diff is
;;   run asynchronously).
;;
;; - diff-tree (dir &optional rev1 rev2) 
;;
;;   Insert the diff for all files at and below DIR into the *vc-diff*
;;   buffer.  The meaning of REV1 and REV2 is the same as for 
;;   vc-BACKEND-diff.  The default implementation does an explicit tree
;;   walk, calling vc-BACKEND-diff for each individual file.
;;
;; - annotate-command (file buf rev)
;;
;;   If this function is provided, it should produce an annotated version
;;   of FILE in BUF, relative to version REV.  This is currently only
;;   implemented for CVS, using the `cvs annotate' command.
;;
;; - annotate-difference (point)
;;
;;   Only required if `annotate-command' is defined for the backend.
;;   Return the difference between the age of the line at point and the
;;   current time.  Return NIL if there is no more comparison to be made
;;   in the buffer.  Return value as defined for `current-time'.  You can
;;   safely assume that point is placed at the beginning of each line,
;;   starting at `point-min'.  The buffer that point is placed in is the
;;   Annotate output, as defined by the relevant backend.
;;
;; SNAPSHOT SYSTEM
;;
;; - create-snapshot (dir name branchp)
;;
;;   Take a snapshot of the current state of files under DIR and name it
;;   NAME.  This should make sure that files are up-to-date before
;;   proceeding with the action.  DIR can also be a file and if BRANCHP
;;   is specified, NAME should be created as a branch and DIR should be
;;   checked out under this new branch.  The default implementation does
;;   not support branches but does a sanity check, a tree traversal and
;;   for each file calls `assign-name'.
;;
;; - assign-name (file name)
;;
;;   Give name NAME to the current version of FILE, assuming it is
;;   up-to-date.  Only used by the default version of `create-snapshot'.
;;
;; - retrieve-snapshot (dir name update)
;;
;;   Retrieve a named snapshot of all registered files at or below DIR.
;;   If UPDATE is non-nil, then update buffers of any files in the
;;   snapshot that are currently visited.  The default implementation
;;   does a sanity check whether there aren't any uncommitted changes at
;;   or below DIR, and then performs a tree walk, using the `checkout'
;;   function to retrieve the corresponding versions.
;;
;; MISCELLANEOUS
;;
;; - make-version-backups-p (file)
;;
;;   Return non-nil if unmodified repository versions of FILE should be
;;   backed up locally.  If this is done, VC can perform `diff' and
;;   `revert' operations itself, without calling the backend system.  The
;;   default implementation always returns nil.
;;
;; - check-headers ()
;;
;;   Return non-nil if the current buffer contains any version headers.
;;
;; - clear-headers ()
;;
;;   In the current buffer, reset all version headers to their unexpanded
;;   form.  This function should be provided if the state-querying code
;;   for this backend uses the version headers to determine the state of
;;   a file.  This function will then be called whenever VC changes the
;;   version control state in such a way that the headers would give
;;   wrong information.
;;
;; - rename-file (old new)
;;
;;   Rename file OLD to NEW, both in the working area and in the
;;   repository.  If this function is not provided, the command
;;   `vc-rename-file' will signal an error.

;;; Code:

(require 'vc-hooks)
(require 'ring)
(eval-when-compile
  (require 'cl)
  (require 'compile)
  (require 'dired)      ; for dired-map-over-marks macro
  (require 'dired-aux))	; for dired-kill-{line,tree}

(if (not (assoc 'vc-parent-buffer minor-mode-alist))
    (setq minor-mode-alist
	  (cons '(vc-parent-buffer vc-parent-buffer-name)
		minor-mode-alist)))

;; General customization

(defgroup vc nil
  "Version-control system in Emacs."
  :group 'tools)

(defcustom vc-suppress-confirm nil
  "*If non-nil, treat user as expert; suppress yes-no prompts on some things."
  :type 'boolean
  :group 'vc)

(defcustom vc-delete-logbuf-window t
  "*If non-nil, delete the *VC-log* buffer and window after each logical action.
If nil, bury that buffer instead.
This is most useful if you have multiple windows on a frame and would like to
preserve the setting."
  :type 'boolean
  :group 'vc)

(defcustom vc-initial-comment nil
  "*If non-nil, prompt for initial comment when a file is registered."
  :type 'boolean
  :group 'vc)

(defcustom vc-default-init-version "1.1"
  "*A string used as the default version number when a new file is registered.
This can be overridden by giving a prefix argument to \\[vc-register]."
  :type 'string
  :group 'vc
  :version "20.3")

(defcustom vc-command-messages nil
  "*If non-nil, display run messages from back-end commands."
  :type 'boolean
  :group 'vc)

(defcustom vc-checkin-switches nil
  "*A string or list of strings specifying extra switches for checkin.
These are passed to the checkin program by \\[vc-checkin]."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string))
  :group 'vc)

(defcustom vc-checkout-switches nil
  "*A string or list of strings specifying extra switches for checkout.
These are passed to the checkout program by \\[vc-checkout]."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string))
  :group 'vc)

(defcustom vc-register-switches nil
  "*A string or list of strings; extra switches for registering a file.
These are passed to the checkin program by \\[vc-register]."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string))
  :group 'vc)

(defcustom vc-dired-listing-switches "-al"
  "*Switches passed to `ls' for vc-dired.  MUST contain the `l' option."
  :type 'string
  :group 'vc
  :version "21.1")

(defcustom vc-dired-recurse t
  "*If non-nil, show directory trees recursively in VC Dired."
  :type 'boolean
  :group 'vc
  :version "20.3")

(defcustom vc-dired-terse-display t
  "*If non-nil, show only locked files in VC Dired."
  :type 'boolean
  :group 'vc
  :version "20.3")

(defcustom vc-directory-exclusion-list '("SCCS" "RCS" "CVS")
  "*List of directory names to be ignored when walking directory trees."
  :type '(repeat string)
  :group 'vc)

(defconst vc-maximum-comment-ring-size 32
  "Maximum number of saved comments in the comment ring.")

;; This is duplicated in diff.el.
(defvar diff-switches "-c"
  "*A string or list of strings specifying switches to be passed to diff.")

(defcustom vc-diff-switches nil
  "*A string or list of strings specifying switches for diff under VC.
There is also an option vc-BACKEND-diff-switches for each BACKEND that
VC can handle."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string))
  :group 'vc
  :version "21.1")

;;;###autoload
(defcustom vc-checkout-hook nil
  "*Normal hook (list of functions) run after checking out a file.
See `run-hooks'."
  :type 'hook
  :group 'vc
  :version "21.1")

;;;###autoload
(defcustom vc-checkin-hook nil
  "*Normal hook (list of functions) run after a checkin is done.
See `run-hooks'."
  :type 'hook
  :options '(vc-comment-to-change-log)
  :group 'vc)

;;;###autoload
(defcustom vc-before-checkin-hook nil
  "*Normal hook (list of functions) run before a file is checked in.
See `run-hooks'."
  :type 'hook
  :group 'vc)

(defcustom vc-logentry-check-hook nil
  "*Normal hook run by `vc-backend-logentry-check'.
Use this to impose your own rules on the entry in addition to any the
version control backend imposes itself."
  :type 'hook
  :group 'vc)

;; Annotate customization
(defcustom vc-annotate-color-map
  '(( 26.3672 . "#FF0000")
    ( 52.7344 . "#FF3800")
    ( 79.1016 . "#FF7000")
    (105.4688 . "#FFA800")
    (131.8359 . "#FFE000")
    (158.2031 . "#E7FF00")
    (184.5703 . "#AFFF00")
    (210.9375 . "#77FF00")
    (237.3047 . "#3FFF00")
    (263.6719 . "#07FF00")
    (290.0391 . "#00FF31")
    (316.4063 . "#00FF69")
    (342.7734 . "#00FFA1")
    (369.1406 . "#00FFD9")
    (395.5078 . "#00EEFF")
    (421.8750 . "#00B6FF")
    (448.2422 . "#007EFF"))
  "*Association list of age versus color, for \\[vc-annotate].
Ages are given in units of 2**16 seconds.
Default is eighteen steps using a twenty day increment."
  :type 'alist
  :group 'vc)

(defcustom vc-annotate-very-old-color "#0046FF"
  "*Color for lines older than current color range in \\[vc-annotate]]."
  :type 'string
  :group 'vc)

(defcustom vc-annotate-background "black"
  "*Background color for \\[vc-annotate].
Default color is used if nil."
  :type 'string
  :group 'vc)

(defcustom vc-annotate-menu-elements '(2 0.5 0.1 0.01)
  "*Menu elements for the mode-specific menu of VC-Annotate mode.
List of factors, used to expand/compress the time scale.  See `vc-annotate'."
  :type '(repeat number)
  :group 'vc)

;; vc-annotate functionality (CVS only).
(defvar vc-annotate-mode nil
  "Variable indicating if VC-Annotate mode is active.")

(defvar vc-annotate-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m [menu-bar] (make-sparse-keymap "VC-Annotate"))
    m)
  "Local keymap used for VC-Annotate mode.")

(defvar vc-annotate-mode-menu nil
  "Local keymap used for VC-Annotate mode's menu bar menu.")

;; Header-insertion hair

(defcustom vc-static-header-alist
  '(("\\.c$" .
     "\n#ifndef lint\nstatic char vcid[] = \"\%s\";\n#endif /* lint */\n"))
  "*Associate static header string templates with file types.
A \%s in the template is replaced with the first string associated with
the file's version control type in `vc-header-alist'."
  :type '(repeat (cons :format "%v"
		       (regexp :tag "File Type")
		       (string :tag "Header String")))
  :group 'vc)

(defcustom vc-comment-alist
  '((nroff-mode ".\\\"" ""))
  "*Special comment delimiters for generating VC headers.
Add an entry in this list if you need to override the normal `comment-start'
and `comment-end' variables.  This will only be necessary if the mode language
is sensitive to blank lines."
  :type '(repeat (list :format "%v"
		       (symbol :tag "Mode")
		       (string :tag "Comment Start")
		       (string :tag "Comment End")))
  :group 'vc)

(defcustom vc-checkout-carefully (= (user-uid) 0)
  "*This variable is obsolete.
The corresponding checks are always done now.
From the old doc string:

Non-nil means be extra-careful in checkout.
Verify that the file really is not locked
and that its contents match what the master file says."
  :type 'boolean
  :group 'vc)


;; The main keymap

;; Initialization code, to be done just once at load-time
(defvar vc-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-n" 'vc-next-comment)
    (define-key map "\M-p" 'vc-previous-comment)
    (define-key map "\M-r" 'vc-comment-search-reverse)
    (define-key map "\M-s" 'vc-comment-search-forward)
    (define-key map "\C-c\C-c" 'vc-finish-logentry)
    map))
;; Compatibility with old name.  Should we bother ?
(defvar vc-log-entry-mode vc-log-mode-map)


;; Variables the user doesn't need to know about.
(defvar vc-log-operation nil)
(defvar vc-log-after-operation-hook nil)
(defvar vc-annotate-buffers nil
  "Alist of current \"Annotate\" buffers and their corresponding backends.
The keys are \(BUFFER . BACKEND\).  See also `vc-annotate-get-backend'.")
;; In a log entry buffer, this is a local variable
;; that points to the buffer for which it was made
;; (either a file, or a VC dired buffer).
(defvar vc-parent-buffer nil)
(put 'vc-parent-buffer 'permanent-local t)
(defvar vc-parent-buffer-name nil)
(put 'vc-parent-buffer-name 'permanent-local t)

(defvar vc-log-file)
(defvar vc-log-version)

(defvar vc-dired-mode nil)
(make-variable-buffer-local 'vc-dired-mode)

(defvar vc-comment-ring (make-ring vc-maximum-comment-ring-size))
(defvar vc-comment-ring-index nil)
(defvar vc-last-comment-match "")

;; functions that operate on RCS revision numbers.  This code should
;; also be moved into the backends.  It stays for now, however, since
;; it is used in code below.
(defun vc-trunk-p (rev)
  "Return t if REV is a revision on the trunk."
  (not (eq nil (string-match "\\`[0-9]+\\.[0-9]+\\'" rev))))

(defun vc-branch-p (rev)
  "Return t if REV is a branch revision."
  (not (eq nil (string-match "\\`[0-9]+\\(\\.[0-9]+\\.[0-9]+\\)*\\'" rev))))

(defun vc-branch-part (rev)
  "Return the branch part of a revision number REV."
  (substring rev 0 (string-match "\\.[0-9]+\\'" rev)))

(defun vc-minor-part (rev)
  "Return the minor version number of a revision number REV."
  (string-match "[0-9]+\\'" rev)
  (substring rev (match-beginning 0) (match-end 0)))

(defun vc-previous-version (rev)
  "Guess the version number immediately preceding REV."
  (let ((branch (vc-branch-part rev))
        (minor-num (string-to-number (vc-minor-part rev))))
    (if (> minor-num 1)
        ;; version does probably not start a branch or release
        (concat branch "." (number-to-string (1- minor-num)))
      (if (vc-trunk-p rev)
          ;; we are at the beginning of the trunk --
          ;; don't know anything to return here
          ""
        ;; we are at the beginning of a branch --
        ;; return version of starting point
        (vc-branch-part branch)))))

;; File property caching

(defun vc-clear-context ()
  "Clear all cached file properties and the comment ring."
  (interactive)
  (fillarray vc-file-prop-obarray 0)
  ;; Note: there is potential for minor lossage here if there is an open
  ;; log buffer with a nonzero local value of vc-comment-ring-index.
  (setq vc-comment-ring (make-ring vc-maximum-comment-ring-size)))

(defmacro with-vc-properties (file form settings)
  "Execute FORM, then maybe set per-file properties for FILE.
SETTINGS is an association list of property/value pairs.  After
executing FORM, set those properties from SETTINGS that have not yet
been updated to their corresponding values."
  `(let ((vc-touched-properties (list t))
	 (filename ,file))
     ,form
     (mapcar (lambda (setting)
	       (let ((property (car setting)))
		 (unless (memq property vc-touched-properties)
		   (put (intern filename vc-file-prop-obarray)
			property (cdr setting)))))
	     ,settings)))

;; Random helper functions

(defsubst vc-editable-p (file)
  "Return non-nil if FILE can be edited."
  (or (eq (vc-checkout-model file) 'implicit)
      (memq (vc-state file) '(edited needs-merge))))

;; Two macros for elisp programming
;;;###autoload
(defmacro with-vc-file (file comment &rest body)
  "Check out a writable copy of FILE if necessary, then execute BODY.
Check in FILE with COMMENT (a string) after BODY has been executed.
FILE is passed through `expand-file-name'; BODY executed within
`save-excursion'.  If FILE is not under version control, or locked by
somebody else, signal error."
  `(let ((file (expand-file-name ,file)))
     (or (vc-backend file)
	 (error (format "File not under version control: `%s'" file)))
     (unless (vc-editable-p file)
       (let ((state (vc-state file)))
	 (if (stringp state) (error (format "`%s' is locking `%s'" state file))
	   (vc-checkout file t))))
     (save-excursion
       ,@body)
     (vc-checkin file nil ,comment)))
(put 'with-vc-file 'indent-function 1)

;;;###autoload
(defmacro edit-vc-file (file comment &rest body)
  "Edit FILE under version control, executing body.
Checkin with COMMENT after executing BODY.
This macro uses `with-vc-file', passing args to it.
However, before executing BODY, find FILE, and after BODY, save buffer."
  `(with-vc-file
    ,file ,comment
    (set-buffer (find-file-noselect ,file))
    ,@body
    (save-buffer)))
(put 'edit-vc-file 'indent-function 1)

(defun vc-ensure-vc-buffer ()
  "Make sure that the current buffer visits a version-controlled file."
  (if vc-dired-mode
      (set-buffer (find-file-noselect (dired-get-filename)))
    (while vc-parent-buffer
      (pop-to-buffer vc-parent-buffer))
    (if (not (buffer-file-name))
	(error "Buffer %s is not associated with a file" (buffer-name))
      (if (not (vc-backend (buffer-file-name)))
	  (error "File %s is not under version control" (buffer-file-name))))))

(defvar vc-binary-assoc nil)
(defvar vc-binary-suffixes
  (if (memq system-type '(ms-dos windows-nt))
      '(".exe" ".com" ".bat" ".cmd" ".btm" "")
    '("")))

(defun vc-process-filter (p s)
  "An alternative output filter for async process P.
The only difference with the default filter is to insert S after markers."
  (with-current-buffer (process-buffer p)
    (save-excursion
      (let ((inhibit-read-only t))
	(goto-char (process-mark p))
	(insert s)
	(set-marker (process-mark p) (point))))))

(defun vc-setup-buffer (&optional buf)
  "Prepare BUF for executing a VC command and make it current.
BUF defaults to \"*vc*\", can be a string and will be created if necessary."
  (unless buf (setq buf "*vc*"))
  (let ((camefrom (current-buffer))
	(olddir default-directory))
    (set-buffer (get-buffer-create buf))
    (kill-all-local-variables)
    (set (make-local-variable 'vc-parent-buffer) camefrom)
    (set (make-local-variable 'vc-parent-buffer-name)
	 (concat " from " (buffer-name camefrom)))
    (setq default-directory olddir)
    (let ((inhibit-read-only t))
      (erase-buffer))))

(defun vc-exec-after (code)
  "Eval CODE when the current buffer's process is done.
If the current buffer has no process, just evaluate CODE.
Else, add CODE to the process' sentinel."
  (let ((proc (get-buffer-process (current-buffer))))
    (cond
     ;; If there's no background process, just execute the code.
     ((null proc) (eval code))
     ;; If the background process has exited, reap it and try again
     ((eq (process-status proc) 'exit)
      (delete-process proc)
      (vc-exec-after code))
     ;; If a process is running, add CODE to the sentinel
     ((eq (process-status proc) 'run)
      (let ((sentinel (process-sentinel proc)))
	(set-process-sentinel proc
	  `(lambda (p s)
	     (with-current-buffer ',(current-buffer)
	       (goto-char (process-mark p))
	       ,@(append (cdr (cdr (cdr ;strip off `with-current-buffer buf
                                        ;             (goto-char...)'
			   (car (cdr (cdr ;strip off `lambda (p s)'
			    sentinel))))))
			 (list `(vc-exec-after ',code))))))))
     (t (error "Unexpected process state"))))
  nil)

(defvar vc-post-command-functions nil
  "Hook run at the end of `vc-do-command'.
Each function is called inside the buffer in which the command was run
and is passed 3 arguments: the COMMAND, the FILE and the FLAGS.")

;;;###autoload
(defun vc-do-command (buffer okstatus command file &rest flags)
  "Execute a VC command, notifying user and checking for errors.
Output from COMMAND goes to BUFFER, or *vc* if BUFFER is nil or the
current buffer if BUFFER is t.  If the destination buffer is not
already current, set it up properly and erase it.  The command is
considered successful if its exit status does not exceed OKSTATUS (if
OKSTATUS is nil, that means to ignore errors, if it is 'async, that
means not to wait for termination of the subprocess).  FILE is the
name of the working file (may also be nil, to execute commands that
don't expect a file name).  If an optional list of FLAGS is present,
that is inserted into the command line before the filename."
  (and file (setq file (expand-file-name file)))
  (if vc-command-messages
      (message "Running %s on %s..." command file))
  (save-current-buffer
    (unless (or (eq buffer t)
                (and (stringp buffer)
                     (string= (buffer-name) buffer))
                (eq buffer (current-buffer)))
      (vc-setup-buffer buffer))
    (let ((squeezed nil)
	  (inhibit-read-only t)
	  (status 0))
      (setq squeezed (delq nil (copy-sequence flags)))
      (when file
	;; FIXME: file-relative-name can return a bogus result because
	;; it doesn't look at the actual file-system to see if symlinks
	;; come into play.
	(setq squeezed (append squeezed (list (file-relative-name file)))))
      (let ((exec-path (append vc-path exec-path))
	    ;; Add vc-path to PATH for the execution of this command.
	    (process-environment
	     (cons (concat "PATH=" (getenv "PATH")
			   path-separator
			   (mapconcat 'identity vc-path path-separator))
		   process-environment))
	    (w32-quote-process-args t))
	(if (eq okstatus 'async)
	    (let ((proc (apply 'start-process command (current-buffer) command
			       squeezed)))
              (unless (active-minibuffer-window)
                (message "Running %s in the background..." command))
	      ;;(set-process-sentinel proc (lambda (p msg) (delete-process p)))
	      (set-process-filter proc 'vc-process-filter)
	      (vc-exec-after
	       `(unless (active-minibuffer-window)
                  (message "Running %s in the background... done" ',command))))
	  (setq status (apply 'call-process command nil t nil squeezed))
	  (when (or (not (integerp status)) (and okstatus (< okstatus status)))
	    (pop-to-buffer (current-buffer))
	    (goto-char (point-min))
	    (shrink-window-if-larger-than-buffer)
	    (error "Running %s...FAILED (%s)" command
		   (if (integerp status) (format "status %d" status) status))))
	(if vc-command-messages
	    (message "Running %s...OK" command)))
      (vc-exec-after
       `(run-hook-with-args 'vc-post-command-functions ',command ',file ',flags))
      status)))

(defun vc-position-context (posn)
  "Save a bit of the text around POSN in the current buffer.
Used to help us find the corresponding position again later
if markers are destroyed or corrupted."
  ;; A lot of this was shamelessly lifted from Sebastian Kremer's
  ;; rcs.el mode.
  (list posn
	(buffer-size)
	(buffer-substring posn
			  (min (point-max) (+ posn 100)))))

(defun vc-find-position-by-context (context)
  "Return the position of CONTEXT in the current buffer.
If CONTEXT cannot be found, return nil."
  (let ((context-string (nth 2 context)))
    (if (equal "" context-string)
	(point-max)
      (save-excursion
	(let ((diff (- (nth 1 context) (buffer-size))))
	  (if (< diff 0) (setq diff (- diff)))
	  (goto-char (nth 0 context))
	  (if (or (search-forward context-string nil t)
		  ;; Can't use search-backward since the match may continue
		  ;; after point.
		  (progn (goto-char (- (point) diff (length context-string)))
			 ;; goto-char doesn't signal an error at
			 ;; beginning of buffer like backward-char would
			 (search-forward context-string nil t)))
	      ;; to beginning of OSTRING
	      (- (point) (length context-string))))))))

(defun vc-context-matches-p (posn context)
  "Return t if POSN matches CONTEXT, nil otherwise."
  (let* ((context-string (nth 2 context))
	 (len (length context-string))
	 (end (+ posn len)))
    (if (> end (1+ (buffer-size)))
	nil
      (string= context-string (buffer-substring posn end)))))

(defun vc-buffer-context ()
  "Return a list (POINT-CONTEXT MARK-CONTEXT REPARSE).
Used by `vc-restore-buffer-context' to later restore the context."
  (let ((point-context (vc-position-context (point)))
	;; Use mark-marker to avoid confusion in transient-mark-mode.
	(mark-context  (if (eq (marker-buffer (mark-marker)) (current-buffer))
			   (vc-position-context (mark-marker))))
	;; Make the right thing happen in transient-mark-mode.
	(mark-active nil)
	;; We may want to reparse the compilation buffer after revert
	(reparse (and (boundp 'compilation-error-list) ;compile loaded
		      (let ((curbuf (current-buffer)))
			;; Construct a list; each elt is nil or a buffer
			;; iff that buffer is a compilation output buffer
			;; that contains markers into the current buffer.
			(save-excursion
			  (mapcar (lambda (buffer)
				    (set-buffer buffer)
				    (let ((errors (or
						   compilation-old-error-list
						   compilation-error-list))
					  (buffer-error-marked-p nil))
				      (while (and (consp errors)
						  (not buffer-error-marked-p))
					(and (markerp (cdr (car errors)))
					     (eq buffer
						 (marker-buffer
						  (cdr (car errors))))
					     (setq buffer-error-marked-p t))
					(setq errors (cdr errors)))
				      (if buffer-error-marked-p buffer)))
				  (buffer-list)))))))
    (list point-context mark-context reparse)))

(defun vc-restore-buffer-context (context)
  "Restore point/mark, and reparse any affected compilation buffers.
CONTEXT is that which `vc-buffer-context' returns."
  (let ((point-context (nth 0 context))
	(mark-context (nth 1 context))
	(reparse (nth 2 context)))
    ;; Reparse affected compilation buffers.
    (while reparse
      (if (car reparse)
	  (with-current-buffer (car reparse)
	    (let ((compilation-last-buffer (current-buffer)) ;select buffer
		  ;; Record the position in the compilation buffer of
		  ;; the last error next-error went to.
		  (error-pos (marker-position
			      (car (car-safe compilation-error-list)))))
	      ;; Reparse the error messages as far as they were parsed before.
	      (compile-reinitialize-errors '(4) compilation-parsing-end)
	      ;; Move the pointer up to find the error we were at before
	      ;; reparsing.  Now next-error should properly go to the next one.
	      (while (and compilation-error-list
			  (/= error-pos (car (car compilation-error-list))))
		(setq compilation-error-list (cdr compilation-error-list))))))
      (setq reparse (cdr reparse)))

    ;; if necessary, restore point and mark
    (if (not (vc-context-matches-p (point) point-context))
	(let ((new-point (vc-find-position-by-context point-context)))
	  (if new-point (goto-char new-point))))
    (and mark-active
         mark-context
         (not (vc-context-matches-p (mark) mark-context))
         (let ((new-mark (vc-find-position-by-context mark-context)))
           (if new-mark (set-mark new-mark))))))

(defun vc-revert-buffer1 (&optional arg no-confirm)
  "Revert buffer, keeping point and mark where user expects them.
Try to be clever in the face of changes due to expanded version control
key words.  This is important for typeahead to work as expected.
ARG and NO-CONFIRM are passed on to `revert-buffer'."
  (interactive "P")
  (widen)
  (let ((context (vc-buffer-context)))
    ;; Use save-excursion here, because it may be able to restore point
    ;; and mark properly even in cases where vc-restore-buffer-context
    ;; would fail.  However, save-excursion might also get it wrong --
    ;; in this case, vc-restore-buffer-context gives it a second try.
    (save-excursion
      ;; t means don't call normal-mode;
      ;; that's to preserve various minor modes.
      (revert-buffer arg no-confirm t))
    (vc-restore-buffer-context context)))


(defun vc-buffer-sync (&optional not-urgent)
  "Make sure the current buffer and its working file are in sync.
NOT-URGENT means it is ok to continue if the user says not to save."
  (if (buffer-modified-p)
      (if (or vc-suppress-confirm
	      (y-or-n-p (format "Buffer %s modified; save it? " (buffer-name))))
	  (save-buffer)
	(unless not-urgent
	  (error "Aborted")))))

(defun vc-workfile-unchanged-p (file)
  "Return non-nil if FILE has not changed since the last checkout."
  (let ((checkout-time (vc-file-getprop file 'vc-checkout-time))
        (lastmod (nth 5 (file-attributes file))))
    (if checkout-time
        (equal checkout-time lastmod)
      (let ((unchanged (vc-call workfile-unchanged-p file)))
        (vc-file-setprop file 'vc-checkout-time (if unchanged lastmod 0))
        unchanged))))

(defun vc-default-workfile-unchanged-p (backend file)
  "Check if FILE is unchanged by diffing against the master version.
Return non-nil if FILE is unchanged."
  (zerop (vc-call diff file (vc-workfile-version file))))

(defun vc-default-latest-on-branch-p (backend file)
  "Return non-nil if FILE is the latest on its branch.
This default implementation always returns non-nil, which means that
editing non-current versions is not supported by default."
  t)

(defun vc-recompute-state (file)
  "Force a recomputation of the version control state of FILE.
The state is computed using the exact, and possibly expensive
function `vc-BACKEND-state', not the heuristic."
  (vc-file-setprop file 'vc-state (vc-call state file)))

(defun vc-next-action-on-file (file verbose &optional comment)
  "Do The Right Thing for a given FILE under version control.
If COMMENT is specified, it will be used as an admin or checkin comment.
If VERBOSE is non-nil, query the user rather than using default parameters."
  (let ((visited (get-file-buffer file))
	state version)
    (when visited
      ;; Check relation of buffer and file, and make sure
      ;; user knows what he's doing.  First, finding the file
      ;; will check whether the file on disk is newer.
      (if vc-dired-mode
	  (find-file-other-window file)
	(set-buffer (find-file-noselect file)))
      (if (not (verify-visited-file-modtime (current-buffer)))
	  (if (yes-or-no-p "Replace file on disk with buffer contents? ")
	      (write-file (buffer-file-name))
	    (error "Aborted"))
	;; Now, check if we have unsaved changes.
	(vc-buffer-sync t)
	(if (buffer-modified-p)
	    (or (y-or-n-p "Operate on disk file, keeping modified buffer? ")
		(error "Aborted")))))

    ;; Do the right thing
    (if (not (vc-registered file))
	(vc-register verbose comment)
      (vc-recompute-state file)
      (if visited (vc-mode-line file))
      (setq state (vc-state file))
      (cond
       ;; up-to-date
       ((or (eq state 'up-to-date)
	    (and verbose (eq state 'needs-patch)))
	(cond
	 (verbose
	  ;; go to a different version
	  (setq version
		(read-string "Branch, version, or backend to move to: "))
	  (let ((vsym (intern-soft (upcase version))))
	    (if (member vsym vc-handled-backends)
		(vc-transfer-file file vsym)
	      (vc-checkout file (eq (vc-checkout-model file) 'implicit)
			   version))))
	 ((not (eq (vc-checkout-model file) 'implicit))
	  ;; check the file out
	  (vc-checkout file t))
	 (t
	  ;; do nothing
	  (message "%s is up-to-date" file))))

       ;; Abnormal: edited but read-only
       ((and visited (eq state 'edited)
	     buffer-read-only (not (file-writable-p file)))
	;; Make the file+buffer read-write.  If the user really wanted to
	;; commit, he'll get a chance to do that next time around, anyway.
	(message "File is edited but read-only; making it writable")
	(set-file-modes buffer-file-name
			(logior (file-modes buffer-file-name) 128))
	(toggle-read-only -1))

       ;; edited
       ((eq state 'edited)
	(cond
	 ;; For files with locking, if the file does not contain
	 ;; any changes, just let go of the lock, i.e. revert.
	 ((and (not (eq (vc-checkout-model file) 'implicit))
	       (vc-workfile-unchanged-p file)
	       ;; If buffer is modified, that means the user just
	       ;; said no to saving it; in that case, don't revert,
	       ;; because the user might intend to save after
	       ;; finishing the log entry.
	       (not (and visited (buffer-modified-p))))
	  ;; DO NOT revert the file without asking the user!
	  (if (not visited) (find-file-other-window file))
	  (if (yes-or-no-p "Revert to master version? ")
	      (vc-revert-buffer)))
	 (t ;; normal action
	  (if (not verbose)
	      (vc-checkin file nil comment)
	    (setq version (read-string "New version or backend: "))
	    (let ((vsym (intern (upcase version))))
	      (if (member vsym vc-handled-backends)
		  (vc-transfer-file file vsym)
		(vc-checkin file version comment)))))))

       ;; locked by somebody else
       ((stringp state)
	(if comment
	    (error "Sorry, you can't steal the lock on %s this way"
		   (file-name-nondirectory file)))
	(vc-steal-lock file
                       (if verbose (read-string "Version to steal: ")
                         (vc-workfile-version file))
		       state))

       ;; needs-patch
       ((eq state 'needs-patch)
	(if (yes-or-no-p (format
			  "%s is not up-to-date.  Get latest version? "
			  (file-name-nondirectory file)))
	    (vc-checkout file (eq (vc-checkout-model file) 'implicit) "")
	  (if (and (not (eq (vc-checkout-model file) 'implicit))
		   (yes-or-no-p "Lock this version? "))
	      (vc-checkout file t)
	    (error "Aborted"))))

       ;; needs-merge
       ((eq state 'needs-merge)
	(if (yes-or-no-p (format
			  "%s is not up-to-date.  Merge in changes now? "
			  (file-name-nondirectory file)))
	    (vc-maybe-resolve-conflicts file (vc-call merge-news file))
	  (error "Aborted")))

       ;; unlocked-changes
       ((eq state 'unlocked-changes)
	(if (not visited) (find-file-other-window file))
	(if (save-window-excursion
	      (vc-version-diff file (vc-workfile-version file) nil)
	      (goto-char (point-min))
	      (let ((inhibit-read-only t))
		(insert-string
		 (format "Changes to %s since last lock:\n\n" file)))
	      (not (beep))
	      (yes-or-no-p (concat "File has unlocked changes.  "
				   "Claim lock retaining changes? ")))
	    (progn (vc-call steal-lock file)
		   ;; Must clear any headers here because they wouldn't
		   ;; show that the file is locked now.
		   (vc-clear-headers file)
		   (vc-mode-line file))
	  (if (not (yes-or-no-p
		    "Revert to checked-in version, instead? "))
	      (error "Checkout aborted")
	    (vc-revert-buffer1 t t)
	    (vc-checkout file t))))))))

(defvar vc-dired-window-configuration)

(defun vc-next-action-dired (file rev comment)
  "Call `vc-next-action-on-file' on all the marked files.
Ignores FILE and REV, but passes on COMMENT."
  (let ((dired-buffer (current-buffer))
	(dired-dir default-directory))
    (dired-map-over-marks
     (let ((file (dired-get-filename)))
       (message "Processing %s..." file)
       (vc-next-action-on-file file nil comment)
       (set-buffer dired-buffer)
       (set-window-configuration vc-dired-window-configuration)
       (message "Processing %s...done" file))
    nil t))
  (dired-move-to-filename))

;; Here's the major entry point.

;;;###autoload
(defun vc-next-action (verbose)
  "Do the next logical version control operation on the current file.

If you call this from within a VC dired buffer with no files marked,
it will operate on the file in the current line.

If you call this from within a VC dired buffer, and one or more
files are marked, it will accept a log message and then operate on
each one.  The log message will be used as a comment for any register
or checkin operations, but ignored when doing checkouts.  Attempted
lock steals will raise an error.

A prefix argument lets you specify the version number to use.

For RCS and SCCS files:
   If the file is not already registered, this registers it for version
control.
   If the file is registered and not locked by anyone, this checks out
a writable and locked file ready for editing.
   If the file is checked out and locked by the calling user, this
first checks to see if the file has changed since checkout.  If not,
it performs a revert.
   If the file has been changed, this pops up a buffer for entry
of a log message; when the message has been entered, it checks in the
resulting changes along with the log message as change commentary.  If
the variable `vc-keep-workfiles' is non-nil (which is its default), a
read-only copy of the changed file is left in place afterwards.
   If the file is registered and locked by someone else, you are given
the option to steal the lock.

For CVS files:
   If the file is not already registered, this registers it for version
control.  This does a \"cvs add\", but no \"cvs commit\".
   If the file is added but not committed, it is committed.
   If your working file is changed, but the repository file is
unchanged, this pops up a buffer for entry of a log message; when the
message has been entered, it checks in the resulting changes along
with the logmessage as change commentary.  A writable file is retained.
   If the repository file is changed, you are asked if you want to
merge in the changes into your working copy."

  (interactive "P")
  (catch 'nogo
    (if vc-dired-mode
	(let ((files (dired-get-marked-files)))
          (set (make-local-variable 'vc-dired-window-configuration)
               (current-window-configuration))
	  (if (string= ""
		 (mapconcat
	             (lambda (f)
		       (if (not (vc-up-to-date-p f)) "@" ""))
		     files ""))
		(vc-next-action-dired nil nil "dummy")
	      (vc-start-entry nil nil nil nil
			      "Enter a change comment for the marked files."
			      'vc-next-action-dired))
	    (throw 'nogo nil)))
    (while vc-parent-buffer
      (pop-to-buffer vc-parent-buffer))
    (if buffer-file-name
        (vc-next-action-on-file buffer-file-name verbose)
      (error "Buffer %s is not associated with a file" (buffer-name)))))

;; These functions help the vc-next-action entry point

;;;###autoload
(defun vc-register (&optional set-version comment)
  "Register the current file into a version control system.
With prefix argument SET-VERSION, allow user to specify initial version
level.  If COMMENT is present, use that as an initial comment.

The version control system to use is found by cycling through the list
`vc-handled-backends'.  The first backend in that list which declares
itself responsible for the file (usually because other files in that
directory are already registered under that backend) will be used to
register the file.  If no backend declares itself responsible, the
first backend that could register the file is used."
  (interactive "P")
  (unless buffer-file-name (error "No visited file"))
  (when (vc-backend buffer-file-name)
    (if (vc-registered buffer-file-name)
	(error "This file is already registered")
      (unless (y-or-n-p "Previous master file has vanished.  Make a new one? ")
	(error "Aborted"))))
  ;; Watch out for new buffers of size 0: the corresponding file
  ;; does not exist yet, even though buffer-modified-p is nil.
  (if (and (not (buffer-modified-p))
	   (zerop (buffer-size))
	   (not (file-exists-p buffer-file-name)))
      (set-buffer-modified-p t))
  (vc-buffer-sync)

  (vc-start-entry buffer-file-name
                  (if set-version
                      (read-string (format "Initial version level for %s: "
					   (buffer-name)))
                    ;; TODO: Use backend-specific init version.
                    vc-default-init-version)
                  (or comment (not vc-initial-comment))
		  nil
                  "Enter initial comment."
		  (lambda (file rev comment)
		    (message "Registering %s... " file)
		    (let ((backend (vc-responsible-backend file t)))
		      (vc-file-clearprops file)
		      (vc-call-backend backend 'register file rev comment)
		      (vc-file-setprop file 'vc-backend backend)
		      (unless vc-make-backup-files
			(make-local-variable 'backup-inhibited)
			(setq backup-inhibited t)))
		    (message "Registering %s... done" file))))


(defun vc-responsible-backend (file &optional register)
  "Return the name of a backend system that is responsible for FILE.
The optional argument REGISTER means that a backend suitable for
registration should be found.

If REGISTER is nil, then if FILE is already registered, return the
backend of FILE.  If FILE is not registered, or a directory, then the
first backend in `vc-handled-backends' that declares itself
responsible for FILE is returned.  If no backend declares itself
responsible, return the first backend.

If REGISTER is non-nil, return the first responsible backend under
which FILE is not yet registered.  If there is no such backend, return
the first backend under which FILE is not yet registered, but could
be registered."
  (if (not vc-handled-backends)
      (error "No handled backends"))
  (or (and (not (file-directory-p file)) (not register) (vc-backend file))
      (catch 'found
	;; First try: find a responsible backend.  If this is for registration,
	;; it must be a backend under which FILE is not yet registered.
	(dolist (backend vc-handled-backends)
	  (and (or (not register)
		   (not (vc-call-backend backend 'registered file)))
	       (vc-call-backend backend 'responsible-p file)
	       (throw 'found backend)))
	;; no responsible backend
	(if (not register)
	    ;; if this is not for registration, the first backend must do
	    (car vc-handled-backends)
	  ;; for registration, we need to find a new backend that
	  ;; could register FILE
	  (dolist (backend vc-handled-backends)
	    (and (not (vc-call-backend backend 'registered file))
		 (vc-call-backend backend 'could-register file)
		 (throw 'found backend)))
	  (error "No backend that could register")))))

(defun vc-default-responsible-p (backend file)
  "Indicate whether BACKEND is reponsible for FILE.
The default is to return nil always."
  nil)

(defun vc-default-could-register (backend file)
  "Return non-nil if BACKEND could be used to register FILE.
The default implementation returns t for all files."
  t)

(defun vc-resynch-window (file &optional keep noquery)
  "If FILE is in the current buffer, either revert or unvisit it.
The choice between revert (to see expanded keywords) and unvisit depends on
`vc-keep-workfiles'.  NOQUERY if non-nil inhibits confirmation for
reverting.  NOQUERY should be t *only* if it is known the only
difference between the buffer and the file is due to version control
rather than user editing!"
  (and (string= buffer-file-name file)
       (if keep
	   (progn
	     (vc-revert-buffer1 t noquery)
             ;; TODO: Adjusting view mode might no longer be necessary
             ;; after RMS change to files.el of 1999-08-08.  Investigate
             ;; this when we install the new VC.
             (and view-read-only
                  (if (file-writable-p file)
                      (and view-mode
                           (let ((view-old-buffer-read-only nil))
                             (view-mode-exit)))
                    (and (not view-mode)
                         (not (eq (get major-mode 'mode-class) 'special))
                         (view-mode-enter))))
	     (vc-mode-line buffer-file-name))
	 (kill-buffer (current-buffer)))))

(defun vc-resynch-buffer (file &optional keep noquery)
  "If FILE is currently visited, resynch its buffer."
  (if (string= buffer-file-name file)
      (vc-resynch-window file keep noquery)
    (let ((buffer (get-file-buffer file)))
      (if buffer
	  (with-current-buffer buffer
	    (vc-resynch-window file keep noquery)))))
  (vc-dired-resynch-file file))

(defun vc-start-entry (file rev comment initial-contents msg action &optional after-hook)
  "Accept a comment for an operation on FILE revision REV.
If COMMENT is nil, pop up a VC-log buffer, emit MSG, and set the
action on close to ACTION.  If COMMENT is a string and
INITIAL-CONTENTS is non-nil, then COMMENT is used as the initial
contents of the log entry buffer.  If COMMENT is a string and
INITIAL-CONTENTS is nil, do action immediately as if the user had
entered COMMENT.  If COMMENT is t, also do action immediately with an
empty comment.  Remember the file's buffer in `vc-parent-buffer'
\(current one if no file).  AFTER-HOOK specifies the local value
for vc-log-operation-hook."
  (let ((parent (or (and file (get-file-buffer file)) (current-buffer))))
    (if vc-before-checkin-hook
        (if file
            (with-current-buffer parent
              (run-hooks 'vc-before-checkin-hook))
          (run-hooks 'vc-before-checkin-hook)))
    (if (and comment (not initial-contents))
	(set-buffer (get-buffer-create "*VC-log*"))
      (pop-to-buffer (get-buffer-create "*VC-log*")))
    (set (make-local-variable 'vc-parent-buffer) parent)
    (set (make-local-variable 'vc-parent-buffer-name)
	 (concat " from " (buffer-name vc-parent-buffer)))
    (if file (vc-mode-line file))
    (vc-log-edit file)
    (make-local-variable 'vc-log-after-operation-hook)
    (if after-hook
	(setq vc-log-after-operation-hook after-hook))
    (setq vc-log-operation action)
    (setq vc-log-version rev)
    (when comment
      (erase-buffer)
      (when (stringp comment) (insert comment)))
    (if (or (not comment) initial-contents)
	(message "%s  Type C-c C-c when done" msg)
      (vc-finish-logentry (eq comment t)))))

(defun vc-checkout (file &optional writable rev)
  "Retrieve a copy of the revision REV of FILE.
If WRITABLE is non-nil, make sure the retrieved file is writable.
REV defaults to the latest revision.

After check-out, runs the normal hook `vc-checkout-hook'."
  (and writable
       (not rev)
       (vc-call make-version-backups-p file)
       (vc-up-to-date-p file)
       (vc-make-version-backup file))
  (with-vc-properties
   file
   (condition-case err
       (vc-call checkout file writable rev)
     (file-error
      ;; Maybe the backend is not installed ;-(
      (when writable
	(let ((buf (get-file-buffer file)))
	  (when buf (with-current-buffer buf (toggle-read-only -1)))))
      (signal (car err) (cdr err))))
   `((vc-state . ,(if (or (eq (vc-checkout-model file) 'implicit)
			  (not writable))
		      (if (vc-call latest-on-branch-p file)
			  'up-to-date
			'needs-patch)
		    'edited))
     (vc-checkout-time . ,(nth 5 (file-attributes file)))))
  (vc-resynch-buffer file t t)
  (run-hooks 'vc-checkout-hook))

(defun vc-steal-lock (file rev owner)
  "Steal the lock on FILE."
  (let (file-description)
    (if rev
	(setq file-description (format "%s:%s" file rev))
      (setq file-description file))
    (if (not (yes-or-no-p (format "Steal the lock on %s from %s? "
				  file-description owner)))
	(error "Steal canceled"))
    (compose-mail owner (format "Stolen lock on %s" file-description)
		  nil nil nil nil
		  (list (list 'vc-finish-steal file rev)))
    (setq default-directory (expand-file-name "~/"))
    (goto-char (point-max))
    (insert
     (format "I stole the lock on %s, " file-description)
     (current-time-string)
     ".\n")
    (message "Please explain why you stole the lock.  Type C-c C-c when done.")))

(defun vc-finish-steal (file version)
  ;; This is called when the notification has been sent.
  (message "Stealing lock on %s..." file)
  (with-vc-properties
   file
   (vc-call steal-lock file version)
   `((vc-state . edited)))
  (vc-resynch-buffer file t t)
  (message "Stealing lock on %s...done" file))

(defun vc-checkin (file &optional rev comment initial-contents)
  "Check in FILE.
The optional argument REV may be a string specifying the new version
level (if nil increment the current level).  COMMENT is a comment
string; if omitted, a buffer is popped up to accept a comment.  If
INITIAL-CONTENTS is non-nil, then COMMENT is used as the initial contents
of the log entry buffer.

If `vc-keep-workfiles' is nil, FILE is deleted afterwards, provided
that the version control system supports this mode of operation.

Runs the normal hook `vc-checkin-hook'."
  (vc-start-entry
   file rev comment initial-contents
   "Enter a change comment."
   (lambda (file rev comment)
     (message "Checking in %s..." file)
     ;; "This log message intentionally left almost blank".
     ;; RCS 5.7 gripes about white-space-only comments too.
     (or (and comment (string-match "[^\t\n ]" comment))
	 (setq comment "*** empty log message ***"))
     (with-vc-properties
      file
      ;; Change buffers to get local value of vc-checkin-switches.
      (with-current-buffer (or (get-file-buffer file) (current-buffer))
	(progn
	  (vc-call checkin file rev comment)
	  (vc-delete-automatic-version-backups file)))
      `((vc-state . up-to-date)
	(vc-checkout-time . ,(nth 5 (file-attributes file)))
	(vc-workfile-version . nil)))
     (message "Checking in %s...done" file))
   'vc-checkin-hook))

(defun vc-comment-to-change-log (&optional whoami file-name)
  "Enter last VC comment into the change log for the current file.
WHOAMI (interactive prefix) non-nil means prompt for user name
and site.  FILE-NAME is the name of the change log; if nil, use
`change-log-default-name'.

This may be useful as a `vc-checkin-hook' to update change logs
automatically."
  (interactive (if current-prefix-arg
		   (list current-prefix-arg
			 (prompt-for-change-log-name))))
  ;; Make sure the defvar for add-log-current-defun-function has been executed
  ;; before binding it.
  (require 'add-log)
  (let (;; Extract the comment first so we get any error before doing anything.
	(comment (ring-ref vc-comment-ring 0))
	;; Don't let add-change-log-entry insert a defun name.
	(add-log-current-defun-function 'ignore)
	end)
    ;; Call add-log to do half the work.
    (add-change-log-entry whoami file-name t t)
    ;; Insert the VC comment, leaving point before it.
    (setq end (save-excursion (insert comment) (point-marker)))
    (if (looking-at "\\s *\\s(")
	;; It starts with an open-paren, as in "(foo): Frobbed."
	;; So remove the ": " add-log inserted.
	(delete-char -2))
    ;; Canonicalize the white space between the file name and comment.
    (just-one-space)
    ;; Indent rest of the text the same way add-log indented the first line.
    (let ((indentation (current-indentation)))
      (save-excursion
	(while (< (point) end)
	  (forward-line 1)
	  (indent-to indentation))
	(setq end (point))))
    ;; Fill the inserted text, preserving open-parens at bol.
    (let ((paragraph-separate (concat paragraph-separate "\\|\\s *\\s("))
	  (paragraph-start (concat paragraph-start "\\|\\s *\\s(")))
      (beginning-of-line)
      (fill-region (point) end))
    ;; Canonicalize the white space at the end of the entry so it is
    ;; separated from the next entry by a single blank line.
    (skip-syntax-forward " " end)
    (delete-char (- (skip-syntax-backward " ")))
    (or (eobp) (looking-at "\n\n")
	(insert "\n"))))

(defun vc-finish-logentry (&optional nocomment)
  "Complete the operation implied by the current log entry.
Use the contents of the current buffer as a check-in or registration
comment.  If the optional arg NOCOMMENT is non-nil, then don't check
the buffer contents as a comment, and don't add it to
`vc-comment-ring'."
  (interactive)
  ;; Check and record the comment, if any.
  (unless nocomment
    ;; Comment too long?
    (vc-call-backend (or (and vc-log-file (vc-backend vc-log-file))
			 (vc-responsible-backend default-directory))
		     'logentry-check)
    (run-hooks 'vc-logentry-check-hook)
    ;; Record the comment in the comment ring
    (let ((comment (buffer-string)))
      (unless (and (ring-p vc-comment-ring)
		   (not (ring-empty-p vc-comment-ring))
		   (equal comment (ring-ref vc-comment-ring 0)))
	(ring-insert vc-comment-ring comment))))
  ;; Sync parent buffer in case the user modified it while editing the comment.
  ;; But not if it is a vc-dired buffer.
  (with-current-buffer vc-parent-buffer
    (or vc-dired-mode (vc-buffer-sync)))
  (if (not vc-log-operation) (error "No log operation is pending"))
  ;; save the parameters held in buffer-local variables
  (let ((log-operation vc-log-operation)
	(log-file vc-log-file)
	(log-version vc-log-version)
	(log-entry (buffer-string))
	(after-hook vc-log-after-operation-hook)
	(tmp-vc-parent-buffer vc-parent-buffer))
    (pop-to-buffer vc-parent-buffer)
    ;; OK, do it to it
    (save-excursion
      (funcall log-operation
	       log-file
	       log-version
	       log-entry))
    ;; Remove checkin window (after the checkin so that if that fails
    ;; we don't zap the *VC-log* buffer and the typing therein).
    (let ((logbuf (get-buffer "*VC-log*")))
      (cond ((and logbuf vc-delete-logbuf-window)
	     (delete-windows-on logbuf (selected-frame))
	     ;; Kill buffer and delete any other dedicated windows/frames.
	     (kill-buffer logbuf))
	    (logbuf (pop-to-buffer "*VC-log*")
		    (bury-buffer)
		    (pop-to-buffer tmp-vc-parent-buffer))))
    ;; Now make sure we see the expanded headers
    (if log-file
	(vc-resynch-buffer log-file vc-keep-workfiles t))
    (if vc-dired-mode
      (dired-move-to-filename))
    (run-hooks after-hook 'vc-finish-logentry-hook)))

;; Code for access to the comment ring

(defun vc-new-comment-index (stride len)
  "Return the comment index STRIDE elements from the current one.
LEN is the length of `vc-comment-ring'."
  (mod (cond
	(vc-comment-ring-index (+ vc-comment-ring-index stride))
	;; Initialize the index on the first use of this command
	;; so that the first M-p gets index 0, and the first M-n gets
	;; index -1.
	((> stride 0) (1- stride))
	(t stride))
       len))

(defun vc-previous-comment (arg)
  "Cycle backwards through comment history.
With a numeric prefix ARG, go back ARG comments."
  (interactive "*p")
  (let ((len (ring-length vc-comment-ring)))
    (if (<= len 0)
	(progn (message "Empty comment ring") (ding))
      (erase-buffer)
      (setq vc-comment-ring-index (vc-new-comment-index arg len))
      (message "Comment %d" (1+ vc-comment-ring-index))
      (insert (ring-ref vc-comment-ring vc-comment-ring-index)))))

(defun vc-next-comment (arg)
  "Cycle forwards through comment history.
With a numeric prefix ARG, go forward ARG comments."
  (interactive "*p")
  (vc-previous-comment (- arg)))

(defun vc-comment-search-reverse (str &optional stride)
  "Search backwards through comment history for substring match of STR.
If the optional argument STRIDE is present, that is a step-width to use
when going through the comment ring."
  ;; Why substring rather than regexp ?   -sm
  (interactive
   (list (read-string "Comment substring: " nil nil vc-last-comment-match)))
  (unless stride (setq stride 1))
  (if (string= str "")
      (setq str vc-last-comment-match)
    (setq vc-last-comment-match str))
  (let* ((str (regexp-quote str))
	 (len (ring-length vc-comment-ring))
	 (n (vc-new-comment-index stride len)))
    (while (progn (when (or (>= n len) (< n 0)) (error "Not found"))
		  (not (string-match str (ring-ref vc-comment-ring n))))
      (setq n (+ n stride)))
    (setq vc-comment-ring-index n)
    (vc-previous-comment 0)))

(defun vc-comment-search-forward (str)
  "Search forwards through comment history for a substring match of STR."
  (interactive
   (list (read-string "Comment substring: " nil nil vc-last-comment-match)))
  (vc-comment-search-reverse str -1))

;; Additional entry points for examining version histories

;;;###autoload
(defun vc-diff (historic &optional not-urgent)
  "Display diffs between file versions.
Normally this compares the current file and buffer with the most
recent checked in version of that file.  This uses no arguments.  With
a prefix argument HISTORIC, it reads the file name to use and two
version designators specifying which versions to compare.  The
optional argument NOT-URGENT non-nil means it is ok to say no to
saving the buffer."
  (interactive (list current-prefix-arg t))
  (if historic
      (call-interactively 'vc-version-diff)
    (vc-ensure-vc-buffer)
    (let ((file buffer-file-name))
      (vc-buffer-sync not-urgent)
      (if (vc-workfile-unchanged-p buffer-file-name)
	  (message "No changes to %s since latest version" file)
	(vc-version-diff file nil nil)))))

(defun vc-version-diff (file rel1 rel2)
  "List the differences between FILE's versions REL1 and REL2.
If REL1 is empty or nil it means to use the current workfile version;
REL2 empty or nil means the current file contents.  FILE may also be
a directory, in that case, generate diffs between the correponding
versions of all registered files in or below it."
  (interactive
   (let ((file (expand-file-name
                (read-file-name (if buffer-file-name
                                    "File or dir to diff: (default visited file) "
                                  "File or dir to diff: ")
                                default-directory buffer-file-name t)))
         (rel1-default nil) (rel2-default nil))
     ;; compute default versions based on the file state
     (cond
      ;; if it's a directory, don't supply any version default
      ((file-directory-p file)
       nil)
      ;; if the file is not up-to-date, use current version as older version
      ((not (vc-up-to-date-p file))
       (setq rel1-default (vc-workfile-version file)))
      ;; if the file is not locked, use last and previous version as default
      (t
       (setq rel1-default (vc-previous-version (vc-workfile-version file)))
       (if (string= rel1-default "") (setq rel1-default nil))
       (setq rel2-default (vc-workfile-version file))))
     ;; construct argument list
     (list file
           (read-string (if rel1-default
			    (concat "Older version: (default "
				    rel1-default ") ")
			  "Older version: ")
			nil nil rel1-default)
           (read-string (if rel2-default
			    (concat "Newer version: (default "
				    rel2-default ") ")
			  "Newer version (default: current source): ")
			nil nil rel2-default))))
  (if (file-directory-p file)
      ;; recursive directory diff
      (progn
        (vc-setup-buffer "*vc-diff*")
	(if (string-equal rel1 "") (setq rel1 nil))
	(if (string-equal rel2 "") (setq rel2 nil))
        (let ((inhibit-read-only t))
          (insert "Diffs between "
                  (or rel1 "last version checked in")
                  " and "
                  (or rel2 "current workfile(s)")
                  ":\n\n"))
        (let ((dir (file-name-as-directory file)))
          (vc-call-backend (vc-responsible-backend dir)
                           'diff-tree dir rel1 rel2))
	(vc-exec-after `(let ((inhibit-read-only t))
			  (insert "\nEnd of diffs.\n"))))
    ;; single file diff
    (vc-diff-internal file rel1 rel2))
  (set-buffer "*vc-diff*")
  (if (and (zerop (buffer-size))
	   (not (get-buffer-process (current-buffer))))
      (progn
	(if rel1
	    (if rel2
		(message "No changes to %s between %s and %s" file rel1 rel2)
	      (message "No changes to %s since %s" file rel1))
	  (message "No changes to %s since latest version" file))
	nil)
    (pop-to-buffer (current-buffer))
    ;; Gnus-5.8.5 sets up an autoload for diff-mode, even if it's
    ;; not available.  Work around that.
    (if (require 'diff-mode nil t) (diff-mode))
    (vc-exec-after '(let ((inhibit-read-only t))
		      (if (eq (buffer-size) 0)
			  (insert "No differences found.\n"))
		      (goto-char (point-min))
		      (shrink-window-if-larger-than-buffer)))
    t))

(defun vc-diff-internal (file rel1 rel2)
  "Run diff to compare FILE's revisions REL1 and REL2.
Output goes to the current buffer, which is assumed properly set up.
The exit status of the diff command is returned.

This function takes care to set up a proper coding system for diff output.
If both revisions are available as local files, then it also does not
actually call the backend, but performs a local diff."
  (if (or (not rel1) (string-equal rel1 ""))
      (setq rel1 (vc-workfile-version file)))
  (if (string-equal rel2 "")
      (setq rel2 nil))
  (let ((file-rel1 (vc-version-backup-file file rel1))
        (file-rel2 (if (not rel2)
                       file
                     (vc-version-backup-file file rel2)))
        (coding-system-for-read (vc-coding-system-for-diff file)))
    (if (and file-rel1 file-rel2)
        (apply 'vc-do-command "*vc-diff*" 1 "diff" nil
               (append (if (listp diff-switches)
                           diff-switches
                         (list diff-switches))
                       (if (listp vc-diff-switches)
                           vc-diff-switches
                         (list vc-diff-switches))
                       (list (file-relative-name file-rel1)
                             (file-relative-name file-rel2))))
      (vc-call diff file rel1 rel2))))

(defmacro vc-diff-switches-list (backend)
  "Return the list of switches to use for executing diff under BACKEND."
  `(append 
    (if (listp diff-switches) diff-switches (list diff-switches))
    (if (listp vc-diff-switches) vc-diff-switches (list vc-diff-switches))
    (let ((backend-switches 
           (eval (intern (concat "vc-" (symbol-name ',backend) 
                                 "-diff-switches")))))
      (if (listp backend-switches) backend-switches (list backend-switches)))))

(defun vc-default-diff-tree (backend dir rel1 rel2)
  "List differences for all registered files at and below DIR.
The meaning of REL1 and REL2 is the same as for `vc-version-diff'."
  ;; This implementation does an explicit tree walk, and calls 
  ;; vc-BACKEND-diff directly for each file.  An optimization
  ;; would be to use `vc-diff-internal', so that diffs can be local,
  ;; and to call it only for files that are actually changed.
  ;; However, this is expensive for some backends, and so it is left
  ;; to backend-specific implementations.
  (setq default-directory dir)
  (vc-file-tree-walk
   default-directory
   (lambda (f)
     (vc-exec-after
      `(let ((coding-system-for-read (vc-coding-system-for-diff ',f))) 
         (message "Looking at %s" ',f)
         (vc-call-backend ',(vc-backend f) 
                          'diff ',f ',rel1 ',rel2))))))

(defun vc-coding-system-for-diff (file)
  "Return the coding system for reading diff output for FILE."
  (or coding-system-for-read
      ;; if we already have this file open, 
      ;; use the buffer's coding system
      (let ((buf (find-buffer-visiting file)))
        (if buf (with-current-buffer buf
                  buffer-file-coding-system)))
      ;; otherwise, try to find one based on the file name
      (car (find-operation-coding-system 'insert-file-contents
                                         file))
      ;; and a final fallback
      'undecided))

;;;###autoload
(defun vc-version-other-window (rev)
  "Visit version REV of the current file in another window.
If the current file is named `F', the version is named `F.~REV~'.
If `F.~REV~' already exists, use it instead of checking it out again."
  (interactive "sVersion to visit (default is workfile version): ")
  (vc-ensure-vc-buffer)
  (let* ((file buffer-file-name)
	 (version (if (string-equal rev "")
		      (vc-workfile-version file)
		    rev))
	 (automatic-backup (vc-version-backup-file-name file version))
         (manual-backup (vc-version-backup-file-name file version 'manual)))
    (unless (file-exists-p manual-backup)
      (if (file-exists-p automatic-backup)
          (rename-file automatic-backup manual-backup nil)
        (vc-call checkout file nil version manual-backup)))
    (find-file-other-window manual-backup)))

;; Header-insertion code

;;;###autoload
(defun vc-insert-headers ()
  "Insert headers into a file for use with a version control system.
Headers desired are inserted at point, and are pulled from
the variable `vc-BACKEND-header'."
  (interactive)
  (vc-ensure-vc-buffer)
  (save-excursion
    (save-restriction
      (widen)
      (if (or (not (vc-check-headers))
	      (y-or-n-p "Version headers already exist.  Insert another set? "))
	  (progn
	    (let* ((delims (cdr (assq major-mode vc-comment-alist)))
		   (comment-start-vc (or (car delims) comment-start "#"))
		   (comment-end-vc (or (car (cdr delims)) comment-end ""))
		   (hdsym (vc-make-backend-sym (vc-backend (buffer-file-name))
					       'header))
		   (hdstrings (and (boundp hdsym) (symbol-value hdsym))))
	      (mapcar (lambda (s)
			(insert comment-start-vc "\t" s "\t"
				comment-end-vc "\n"))
		      hdstrings)
	      (if vc-static-header-alist
		  (mapcar (lambda (f)
			    (if (string-match (car f) buffer-file-name)
				(insert (format (cdr f) (car hdstrings)))))
			  vc-static-header-alist))
	      )
	    )))))

(defun vc-clear-headers (&optional file)
  "Clear all version headers in the current buffer (or FILE).
The headers are reset to their non-expanded form."
  (let* ((filename (or file buffer-file-name))
	 (visited (find-buffer-visiting filename))
	 (backend (vc-backend filename)))
    (when (vc-find-backend-function backend 'clear-headers)
	(if visited
	    (let ((context (vc-buffer-context)))
	      ;; save-excursion may be able to relocate point and mark
	      ;; properly.  If it fails, vc-restore-buffer-context
	      ;; will give it a second try.
	      (save-excursion
		(vc-call-backend backend 'clear-headers))
	      (vc-restore-buffer-context context))
	  (set-buffer (find-file-noselect filename))
	  (vc-call-backend backend 'clear-headers)
	  (kill-buffer filename)))))

;;;###autoload
(defun vc-merge ()
  "Merge changes between two versions into the current buffer's file.
This asks for two versions to merge from in the minibuffer.  If the
first version is a branch number, then merge all changes from that
branch.  If the first version is empty, merge news, i.e. recent changes
from the current branch.

See Info node `Merging'."
  (interactive)
  (vc-ensure-vc-buffer)
  (vc-buffer-sync)
  (let* ((file buffer-file-name)
	 (backend (vc-backend file))
	 (state (vc-state file))
	 first-version second-version status)
    (cond
     ((stringp state)
      (error "File is locked by %s" state))
     ((not (vc-editable-p file))
      (if (y-or-n-p
	   "File must be checked out for merging.  Check out now? ")
	  (vc-checkout file t)
	(error "Merge aborted"))))
    (setq first-version
	  (read-string (concat "Branch or version to merge from "
			       "(default: news on current branch): ")))
    (if (string= first-version "")
	(if (not (vc-find-backend-function backend 'merge-news))
	    (error "Sorry, merging news is not implemented for %s" backend)
	  (setq status (vc-call merge-news file)))
      (if (not (vc-find-backend-function backend 'merge))
	  (error "Sorry, merging is not implemented for %s" backend)
	(if (not (vc-branch-p first-version))
	    (setq second-version
		  (read-string "Second version: "
			       (concat (vc-branch-part first-version) ".")))
	  ;; We want to merge an entire branch.  Set versions
	  ;; accordingly, so that vc-BACKEND-merge understands us.
	  (setq second-version first-version)
	  ;; first-version must be the starting point of the branch
	  (setq first-version (vc-branch-part first-version)))
	(setq status (vc-call merge file first-version second-version))))
    (vc-maybe-resolve-conflicts file status "WORKFILE" "MERGE SOURCE")))

(defun vc-maybe-resolve-conflicts (file status &optional name-A name-B)
  (vc-resynch-buffer file t (not (buffer-modified-p)))
  (if (zerop status) (message "Merge successful")
    (if (fboundp 'smerge-mode) (smerge-mode 1))
    (if (y-or-n-p "Conflicts detected.  Resolve them now? ")
	(if (fboundp 'smerge-ediff)
	    (smerge-ediff)
	  (vc-resolve-conflicts name-A name-B))
      (message "File contains conflict markers"))))

(defvar vc-ediff-windows)
(defvar vc-ediff-result)
(eval-when-compile
  (defvar ediff-buffer-A)
  (defvar ediff-buffer-B)
  (defvar ediff-buffer-C)
  (require 'ediff-util))
;;;###autoload
(defun vc-resolve-conflicts (&optional name-A name-B)
  "Invoke ediff to resolve conflicts in the current buffer.
The conflicts must be marked with rcsmerge conflict markers."
  (interactive)
  (vc-ensure-vc-buffer)
  (let* ((found nil)
         (file-name (file-name-nondirectory buffer-file-name))
	 (your-buffer   (generate-new-buffer
                         (concat "*" file-name
				 " " (or name-A "WORKFILE") "*")))
	 (other-buffer  (generate-new-buffer
                         (concat "*" file-name
				 " " (or name-B "CHECKED-IN") "*")))
         (result-buffer (current-buffer)))
    (save-excursion
      (set-buffer your-buffer)
      (erase-buffer)
      (insert-buffer result-buffer)
      (goto-char (point-min))
      (while (re-search-forward (concat "^<<<<<<< "
					(regexp-quote file-name) "\n") nil t)
        (setq found t)
	(replace-match "")
	(if (not (re-search-forward "^=======\n" nil t))
	    (error "Malformed conflict marker"))
	(replace-match "")
	(let ((start (point)))
	  (if (not (re-search-forward "^>>>>>>> [0-9.]+\n" nil t))
	      (error "Malformed conflict marker"))
	  (delete-region start (point))))
      (if (not found)
          (progn
            (kill-buffer your-buffer)
            (kill-buffer other-buffer)
            (error "No conflict markers found")))
      (set-buffer other-buffer)
      (erase-buffer)
      (insert-buffer result-buffer)
      (goto-char (point-min))
      (while (re-search-forward (concat "^<<<<<<< "
					(regexp-quote file-name) "\n") nil t)
	(let ((start (match-beginning 0)))
	(if (not (re-search-forward "^=======\n" nil t))
	    (error "Malformed conflict marker"))
	(delete-region start (point))
	(if (not (re-search-forward "^>>>>>>> [0-9.]+\n" nil t))
	    (error "Malformed conflict marker"))
	(replace-match "")))
      (let ((config (current-window-configuration))
            (ediff-default-variant 'default-B))

        ;; Fire up ediff.

        (set-buffer (ediff-merge-buffers your-buffer other-buffer))

        ;; Ediff is now set up, and we are in the control buffer.
        ;; Do a few further adjustments and take precautions for exit.

        (make-local-variable 'vc-ediff-windows)
        (setq vc-ediff-windows config)
        (make-local-variable 'vc-ediff-result)
        (setq vc-ediff-result result-buffer)
        (make-local-variable 'ediff-quit-hook)
        (setq ediff-quit-hook
              (lambda ()
		(let ((buffer-A ediff-buffer-A)
		      (buffer-B ediff-buffer-B)
		      (buffer-C ediff-buffer-C)
		      (result vc-ediff-result)
		      (windows vc-ediff-windows))
		  (ediff-cleanup-mess)
		  (set-buffer result)
		  (erase-buffer)
		  (insert-buffer buffer-C)
		  (kill-buffer buffer-A)
		  (kill-buffer buffer-B)
		  (kill-buffer buffer-C)
		  (set-window-configuration windows)
		  (message "Conflict resolution finished; you may save the buffer"))))
        (message "Please resolve conflicts now; exit ediff when done")
        nil))))

;; The VC directory major mode.  Coopt Dired for this.
;; All VC commands get mapped into logical equivalents.

(defvar vc-dired-switches)
(defvar vc-dired-terse-mode)

(defvar vc-dired-mode-map
  (let ((map (make-sparse-keymap))
	(vmap (make-sparse-keymap)))
    (define-key map "\C-xv" vmap)
    (define-key map "v" vmap)
    (set-keymap-parent vmap vc-prefix-map)
    (define-key vmap "t" 'vc-dired-toggle-terse-mode)
    map))

(define-derived-mode vc-dired-mode dired-mode "Dired under VC"
  "The major mode used in VC directory buffers.

It works like Dired, but lists only files under version control, with
the current VC state of each file being indicated in the place of the
file's link count, owner, group and size.  Subdirectories are also
listed, and you may insert them into the buffer as desired, like in
Dired.

All Dired commands operate normally, with the exception of `v', which
is redefined as the version control prefix, so that you can type
`vl', `v=' etc. to invoke `vc-print-log', `vc-diff', and the like on
the file named in the current Dired buffer line.  `vv' invokes
`vc-next-action' on this file, or on all files currently marked.
There is a special command, `*l', to mark all files currently locked."
  ;; define-derived-mode does it for us in Emacs-21, but not in Emacs-20.
  ;; We do it here because dired might not be loaded yet
  ;; when vc-dired-mode-map is initialized.
  (set-keymap-parent vc-dired-mode-map dired-mode-map)
  (make-local-hook 'dired-after-readin-hook)
  (add-hook 'dired-after-readin-hook 'vc-dired-hook nil t)
  ;; The following is slightly modified from dired.el,
  ;; because file lines look a bit different in vc-dired-mode.
  (set (make-local-variable 'dired-move-to-filename-regexp)
       (let*
          ((l "\\([A-Za-z]\\|[^\0-\177]\\)")
           ;; In some locales, month abbreviations are as short as 2 letters,
           ;; and they can be padded on the right with spaces.
           (month (concat l l "+ *"))
           ;; Recognize any non-ASCII character.
           ;; The purpose is to match a Kanji character.
           (k "[^\0-\177]")
           ;; (k "[^\x00-\x7f\x80-\xff]")
           (s " ")
           (yyyy "[0-9][0-9][0-9][0-9]")
           (mm "[ 0-1][0-9]")
           (dd "[ 0-3][0-9]")
           (HH:MM "[ 0-2][0-9]:[0-5][0-9]")
           (western (concat "\\(" month s dd "\\|" dd s month "\\)"
                            s "\\(" HH:MM "\\|" s yyyy"\\|" yyyy s "\\)"))
           (japanese (concat mm k s dd k s "\\(" s HH:MM "\\|" yyyy k "\\)")))
	 ;; the .* below ensures that we find the last match on a line
         (concat ".*" s "\\(" western "\\|" japanese "\\)" s)))
  (and (boundp 'vc-dired-switches)
       vc-dired-switches
       (set (make-local-variable 'dired-actual-switches)
            vc-dired-switches))
  (set (make-local-variable 'vc-dired-terse-mode) vc-dired-terse-display)
  (setq vc-dired-mode t))

(defun vc-dired-toggle-terse-mode ()
  "Toggle terse display in VC Dired."
  (interactive)
  (if (not vc-dired-mode)
      nil
    (setq vc-dired-terse-mode (not vc-dired-terse-mode))
    (if vc-dired-terse-mode
        (vc-dired-hook)
      (revert-buffer))))

(defun vc-dired-mark-locked ()
  "Mark all files currently locked."
  (interactive)
  (dired-mark-if (let ((f (dired-get-filename nil t)))
		   (and f
			(not (file-directory-p f))
			(not (vc-up-to-date-p f))))
		 "locked file"))

(define-key vc-dired-mode-map "*l" 'vc-dired-mark-locked)

(defun vc-default-dired-state-info (backend file)
  (let ((state (vc-state file)))
    (cond
     ((stringp state) (concat "(" state ")"))
     ((eq state 'edited) (concat "(" (vc-user-login-name) ")"))
     ((eq state 'needs-merge) "(merge)")
     ((eq state 'needs-patch) "(patch)")
     ((eq state 'unlocked-changes) "(stale)"))))

(defun vc-dired-reformat-line (x)
  "Reformat a directory-listing line.
Replace various columns with version control information.
This code, like dired, assumes UNIX -l format."
  (beginning-of-line)
  (let ((pos (point)) limit perm date-and-file)
    (end-of-line)
    (setq limit (point))
    (goto-char pos)
    (when
        (or
         (re-search-forward  ;; owner and group
          "^\\(..[drwxlts-]+ \\) *[0-9]+ [^ ]+ +[^ ]+ +[0-9]+\\( .*\\)"
          limit t)
         (re-search-forward  ;; only owner displayed
          "^\\(..[drwxlts-]+ \\) *[0-9]+ [^ ]+ +[0-9]+\\( .*\\)"
	  limit t)
         (re-search-forward  ;; OS/2 -l format, no links, owner, group
          "^\\(..[drwxlts-]+ \\) *[0-9]+\\( .*\\)"
          limit t))
      (setq perm          (match-string 1)
	    date-and-file (match-string 2))
      (setq x (substring (concat x "          ") 0 10))
      (replace-match (concat perm x date-and-file)))))

(defun vc-dired-hook ()
  "Reformat the listing according to version control.
Called by dired after any portion of a vc-dired buffer has been read in."
  (message "Getting version information... ")
  (let (subdir filename (buffer-read-only nil) cvs-dir)
    (goto-char (point-min))
    (while (not (eobp))
      (cond
       ;; subdir header line
       ((setq subdir (dired-get-subdir))
	;; if the backend supports it, get the state
	;; of all files in this directory at once
	(let ((backend (vc-responsible-backend subdir)))
	  (if (vc-find-backend-function backend 'dir-state)
	      (vc-call-backend backend 'dir-state subdir)))
        (forward-line 1)
        ;; erase (but don't remove) the "total" line
	(delete-region (point) (line-end-position))
	(beginning-of-line)
	(forward-line 1))
       ;; file line
       ((setq filename (dired-get-filename nil t))
        (cond
         ;; subdir
         ((file-directory-p filename)
          (cond
           ((member (file-name-nondirectory filename)
                    vc-directory-exclusion-list)
            (let ((pos (point)))
              (dired-kill-tree filename)
              (goto-char pos)
              (dired-kill-line)))
           (vc-dired-terse-mode
            ;; Don't show directories in terse mode.  Don't use
            ;; dired-kill-line to remove it, because in recursive listings,
            ;; that would remove the directory contents as well.
            (delete-region (line-beginning-position)
                           (progn (forward-line 1) (point))))
           ((string-match "\\`\\.\\.?\\'" (file-name-nondirectory filename))
            (dired-kill-line))
           (t
            (vc-dired-reformat-line nil)
            (forward-line 1))))
         ;; ordinary file
         ((and (vc-backend filename)
	       (not (and vc-dired-terse-mode
			 (vc-up-to-date-p filename))))
          (vc-dired-reformat-line (vc-call dired-state-info filename))
          (forward-line 1))
         (t
          (dired-kill-line))))
       ;; any other line
       (t (forward-line 1))))
    (vc-dired-purge))
  (message "Getting version information... done")
  (save-restriction
    (widen)
    (cond ((eq (count-lines (point-min) (point-max)) 1)
           (goto-char (point-min))
           (message "No files locked under %s" default-directory)))))

(defun vc-dired-purge ()
  "Remove empty subdirs."
  (let (subdir)
    (goto-char (point-min))
    (while (setq subdir (dired-get-subdir))
      (forward-line 2)
      (if (dired-get-filename nil t)
          (if (not (dired-next-subdir 1 t))
              (goto-char (point-max)))
        (forward-line -2)
        (if (not (string= (dired-current-directory) default-directory))
            (dired-do-kill-lines t "")
          ;; We cannot remove the top level directory.
          ;; Just make it look a little nicer.
          (forward-line 1)
          (kill-line)
          (if (not (dired-next-subdir 1 t))
              (goto-char (point-max))))))
    (goto-char (point-min))))

(defun vc-dired-buffers-for-dir (dir)
  "Return a list of all vc-dired buffers that currently display DIR."
  (let (result)
    ;; Check whether dired is loaded.
    (when (fboundp 'dired-buffers-for-dir)
      (mapcar (lambda (buffer)
		(with-current-buffer buffer
		  (if vc-dired-mode
		      (setq result (append result (list buffer))))))
	      (dired-buffers-for-dir dir)))
    result))

(defun vc-dired-resynch-file (file)
  "Update the entries for FILE in any VC Dired buffers that list it."
  (let ((buffers (vc-dired-buffers-for-dir (file-name-directory file))))
    (when buffers
      (mapcar (lambda (buffer)
		(with-current-buffer buffer
		  (if (dired-goto-file file)
		      ;; bind vc-dired-terse-mode to nil so that
		      ;; files won't vanish when they are checked in
		      (let ((vc-dired-terse-mode nil))
			(dired-do-redisplay 1)))))
	      buffers))))

;;;###autoload
(defun vc-directory (dir read-switches)
  "Create a buffer in VC Dired Mode for directory DIR.

See Info node `VC Dired Mode'.

With prefix arg READ-SWITCHES, specify a value to override
`dired-listing-switches' when generating the listing."
  (interactive "DDired under VC (directory): \nP")
  (let ((vc-dired-switches (concat vc-dired-listing-switches
                                   (if vc-dired-recurse "R" ""))))
    (if read-switches
        (setq vc-dired-switches
              (read-string "Dired listing switches: "
                           vc-dired-switches)))
    (require 'dired)
    (require 'dired-aux)
    (switch-to-buffer
     (dired-internal-noselect (expand-file-name (file-name-as-directory dir))
                              vc-dired-switches
                              'vc-dired-mode))))


;; Named-configuration entry points

(defun vc-snapshot-precondition (dir)
  "Scan the tree below DIR, looking for files not up-to-date.
If any file is not up-to-date, return the name of the first such file.
\(This means, neither snapshot creation nor retrieval is allowed.\)
If one or more of the files are currently visited, return `visited'.
Otherwise, return nil."
  (let ((status nil))
    (catch 'vc-locked-example
      (vc-file-tree-walk
       dir
       (lambda (f)
	 (if (not (vc-up-to-date-p f)) (throw 'vc-locked-example f)
	   (if (get-file-buffer f) (setq status 'visited)))))
      status)))

;;;###autoload
(defun vc-create-snapshot (dir name branchp)
  "Descending recursively from DIR, make a snapshot called NAME.
For each registered file, the version level of its latest version
becomes part of the named configuration.  If the prefix argument
BRANCHP is given, the snapshot is made as a new branch and the files
are checked out in that new branch."
  (interactive
   (list (read-file-name "Directory: " default-directory default-directory t)
         (read-string "New snapshot name: ")
	 current-prefix-arg))
  (message "Making %s... " (if branchp "branch" "snapshot"))
  (if (file-directory-p dir) (setq dir (file-name-as-directory dir)))
  (vc-call-backend (vc-responsible-backend dir)
		   'create-snapshot dir name branchp)
  (message "Making %s... done" (if branchp "branch" "snapshot")))

(defun vc-default-create-snapshot (backend dir name branchp)
  (when branchp
    (error "VC backend %s does not support module branches" backend))
  (let ((result (vc-snapshot-precondition dir)))
    (if (stringp result)
	(error "File %s is not up-to-date" result)
      (vc-file-tree-walk
       dir
       (lambda (f)
	 (vc-call assign-name f name))))))

;;;###autoload
(defun vc-retrieve-snapshot (dir name)
  "Descending recursively from DIR, retrieve the snapshot called NAME.
If NAME is empty, it refers to the latest versions.
If locking is used for the files in DIR, then there must not be any
locked files at or below DIR (but if NAME is empty, locked files are
allowed and simply skipped)."
  (interactive
   (list (read-file-name "Directory: " default-directory default-directory t)
         (read-string "Snapshot name to retrieve (default latest versions): ")))
  (let ((update (yes-or-no-p "Update any affected buffers? "))
	(msg (if (or (not name) (string= name ""))
		 (format "Updating %s... " (abbreviate-file-name dir))
	       (format "Retrieving snapshot into %s... "
		       (abbreviate-file-name dir)))))
    (message msg)
    (vc-call-backend (vc-responsible-backend dir)
		     'retrieve-snapshot dir name update)
    (message (concat msg "done"))))

(defun vc-default-retrieve-snapshot (backend dir name update)
  (if (string= name "")
      (progn
        (vc-file-tree-walk
         dir
         (lambda (f) (and
                      (vc-up-to-date-p f)
                      (vc-error-occurred
                       (vc-call checkout f nil "")
                       (if update (vc-resynch-buffer f t t)))))))
    (let ((result (vc-snapshot-precondition dir)))
      (if (stringp result)
          (error "File %s is locked" result)
        (setq update (and (eq result 'visited) update))
        (vc-file-tree-walk
         dir
         (lambda (f) (vc-error-occurred
		      (vc-call checkout f nil name)
		      (if update (vc-resynch-buffer f t t)))))))))

;; Miscellaneous other entry points

;;;###autoload
(defun vc-print-log ()
  "List the change log of the current buffer in a window."
  (interactive)
  (vc-ensure-vc-buffer)
  (let ((file buffer-file-name))
    (vc-call print-log file)
    (set-buffer "*vc*")
    (pop-to-buffer (current-buffer))
    (if (fboundp 'log-view-mode) (log-view-mode))
    (vc-exec-after
     `(progn
	(goto-char (point-max)) (forward-line -1)
	(while (looking-at "=*\n")
	  (delete-char (- (match-end 0) (match-beginning 0)))
	  (forward-line -1))
	(goto-char (point-min))
	(if (looking-at "[\b\t\n\v\f\r ]+")
	    (delete-char (- (match-end 0) (match-beginning 0))))
	(shrink-window-if-larger-than-buffer)
	;; move point to the log entry for the current version
	(if (fboundp 'log-view-goto-rev)
	    (log-view-goto-rev ',(vc-workfile-version file))
	  (if (vc-find-backend-function ',(vc-backend file) 'show-log-entry)
	      (vc-call-backend ',(vc-backend file)
			       'show-log-entry
			       ',(vc-workfile-version file))))))))

(defun vc-default-comment-history (backend file)
  "Return a string with all log entries stored in BACKEND for FILE."
  (if (vc-find-backend-function backend 'print-log)
      (with-temp-buffer
	(vc-call print-log file)
	(vc-call wash-log file)
	(buffer-string))))

(defun vc-default-wash-log (backend file)
  "Remove all non-comment information from log output.
This default implementation works for RCS logs; backends should override
it if their logs are not in RCS format."
  (let ((separator (concat "^-+\nrevision [0-9.]+\ndate: .*\n"
			   "\\(branches: .*;\n\\)?"
			   "\\(\\*\\*\\* empty log message \\*\\*\\*\n\\)?")))
    (goto-char (point-max)) (forward-line -1)
    (while (looking-at "=*\n")
      (delete-char (- (match-end 0) (match-beginning 0)))
      (forward-line -1))
    (goto-char (point-min))
    (if (looking-at "[\b\t\n\v\f\r ]+")
	(delete-char (- (match-end 0) (match-beginning 0))))
    (goto-char (point-min))
    (re-search-forward separator nil t)
    (delete-region (point-min) (point))
    (while (re-search-forward separator nil t)
      (delete-region (match-beginning 0) (match-end 0)))))

;;;###autoload
(defun vc-revert-buffer ()
  "Revert the current buffer's file to the version it was based on.
This asks for confirmation if the buffer contents are not identical
to that version.  This function does not automatically pick up newer
changes found in the master file; use \\[universal-argument] \\[vc-next-action] to do so."
  (interactive)
  (vc-ensure-vc-buffer)
  ;; Make sure buffer is saved.  If the user says `no', abort since
  ;; we cannot show the changes and ask for confirmation to discard them.
  (vc-buffer-sync nil)
  (let ((file buffer-file-name)
	;; This operation should always ask for confirmation.
	(vc-suppress-confirm nil)
	(obuf (current-buffer))
	status)
    (if (vc-up-to-date-p file)
        (unless (yes-or-no-p "File seems up-to-date.  Revert anyway? ")
          (error "Revert canceled")))
    (unless (vc-workfile-unchanged-p file)
      ;; vc-diff selects the new window, which is not what we want:
      ;; if the new window is on another frame, that'd require the user
      ;; moving her mouse to answer the yes-or-no-p question.
      (let ((win (save-selected-window
		   (setq status (vc-diff nil t)) (selected-window))))
	(vc-exec-after `(message nil))
	(when status
	  (unwind-protect
	      (unless (yes-or-no-p "Discard changes? ")
		(error "Revert canceled"))
	    (select-window win)
	    (if (one-window-p t)
		(if (window-dedicated-p (selected-window))
		    (make-frame-invisible))
	      (delete-window))))))
    (set-buffer obuf)
    ;; Do the reverting
    (message "Reverting %s..." file)
    (vc-revert-file file)
    (message "Reverting %s...done" file)))

(defun vc-version-backup-file (file &optional rev)
  "Return name of backup file for revision REV of FILE.
If version backups should be used for FILE, and there exists
such a backup for REV or the current workfile version of file,
return its name; otherwise return nil."
  (when (vc-call make-version-backups-p file)
    (let ((backup-file (vc-version-backup-file-name file rev)))
      (if (file-exists-p backup-file)
          backup-file
        ;; there is no automatic backup, but maybe the user made one manually
        (setq backup-file (vc-version-backup-file-name file rev 'manual))
        (if (file-exists-p backup-file)
            backup-file)))))

(defun vc-revert-file (file)
  "Revert FILE back to the version it was based on."
  (with-vc-properties
   file
   (let ((backup-file (vc-version-backup-file file)))
     (when backup-file
       (copy-file backup-file file 'ok-if-already-exists 'keep-date)
       (vc-delete-automatic-version-backups file))
     (vc-call revert file backup-file))
   `((vc-state . up-to-date)
     (vc-checkout-time . ,(nth 5 (file-attributes file)))))
  (vc-resynch-buffer file t t))

;;;###autoload
(defun vc-cancel-version (norevert)
  "Get rid of most recently checked in version of this file.
A prefix argument NOREVERT means do not revert the buffer afterwards."
  (interactive "P")
  (vc-ensure-vc-buffer)
  (let* ((file (buffer-file-name))
	 (backend (vc-backend file))
         (target (vc-workfile-version file))
         (config (current-window-configuration)) done)
    (cond
     ((not (vc-find-backend-function backend 'cancel-version))
      (error "Sorry, canceling versions is not supported under %s" backend))
     ((not (vc-call latest-on-branch-p file))
      (error "This is not the latest version; VC cannot cancel it"))
     ((not (vc-up-to-date-p file))
      (error (substitute-command-keys "File is not up to date; use \\[vc-revert-buffer] to discard changes"))))
    (if (null (yes-or-no-p (format "Remove version %s from master? " target)))
	(error "Aborted")
      (setq norevert (or norevert (not
          (yes-or-no-p "Revert buffer to most recent remaining version? "))))

      (message "Removing last change from %s..." file)
      (with-vc-properties
       file
       (vc-call cancel-version file norevert)
       `((vc-state . ,(if norevert 'edited 'up-to-date))
	 (vc-checkout-time . ,(if norevert
				0
			      (nth 5 (file-attributes file))))
	 (vc-workfile-version . nil)))
      (message "Removing last change from %s...done" file)

      (cond
       (norevert ;; clear version headers and mark the buffer modified
	(set-visited-file-name file)
	(when (not vc-make-backup-files)
	  ;; inhibit backup for this buffer
	  (make-local-variable 'backup-inhibited)
	  (setq backup-inhibited t))
	(setq buffer-read-only nil)
	(vc-clear-headers)
	(vc-mode-line file)
	(vc-dired-resynch-file file))
       (t ;; revert buffer to file on disk
	(vc-resynch-buffer file t t)))
      (message "Version %s has been removed from the master" target))))

;;;###autoload
(defun vc-switch-backend (file backend)
  "Make BACKEND the current version control system for FILE.
FILE must already be registered in BACKEND.  The change is not
permanent, only for the current session.  This function only changes
VC's perspective on FILE, it does not register or unregister it.
By default, this command cycles through the registered backends.
To get a prompt, use a prefix argument."
  (interactive
   (list
    buffer-file-name
    (let ((backend (vc-backend buffer-file-name))
	  (backends nil))
      ;; Find the registered backends.
      (dolist (backend vc-handled-backends)
	(when (vc-call-backend backend 'registered buffer-file-name)
	  (push backend backends)))
      ;; Find the next backend.
      (let ((def (car (delq backend (append (memq backend backends) backends))))
	    (others (delete backend backends)))
	(cond
	 ((null others) (error "No other backend to switch to"))
	 (current-prefix-arg
	  (intern
	   (upcase
	    (completing-read
	     (format "Switch to backend [%s]: " def)
	     (mapcar (lambda (b) (list (downcase (symbol-name b)))) backends)
	     nil t nil nil (downcase (symbol-name def))))))
       (t def))))))
  (unless (eq backend (vc-backend file))
    (vc-file-clearprops file)
    (vc-file-setprop file 'vc-backend backend)
    ;; Force recomputation of the state
    (unless (vc-call-backend backend 'registered file)
      (vc-file-clearprops file)
      (error "%s is not registered in %s" file backend))
    (vc-mode-line file)))

;;;###autoload
(defun vc-transfer-file (file new-backend)
  "Transfer FILE to another version control system NEW-BACKEND.
If NEW-BACKEND has a higher precedence than FILE's current backend
\(i.e.  it comes earlier in `vc-handled-backends'), then register FILE in
NEW-BACKEND, using the version number from the current backend as the
base level.  If NEW-BACKEND has a lower precedence than the current
backend, then commit all changes that were made under the current
backend to NEW-BACKEND, and unregister FILE from the current backend.
\(If FILE is not yet registered under NEW-BACKEND, register it.)"
  (let* ((old-backend (vc-backend file))
	 (edited (memq (vc-state file) '(edited needs-merge)))
	 (registered (vc-call-backend new-backend 'registered file))
	 (move
	  (and registered    ; Never move if not registered in new-backend yet.
	       ;; move if new-backend comes later in vc-handled-backends
	       (or (memq new-backend (memq old-backend vc-handled-backends))
		   (y-or-n-p "Final transfer? "))))
	 (comment nil))
    (if (eq old-backend new-backend)
	(error "%s is the current backend of %s" new-backend file))
    (if registered
	(set-file-modes file (logior (file-modes file) 128))
      ;; `registered' might have switched under us.
      (vc-switch-backend file old-backend)
      (let* ((rev (vc-workfile-version file))
	     (modified-file (and edited (make-temp-name file)))
	     (unmodified-file (and modified-file (vc-version-backup-file file))))
	;; Go back to the base unmodified file.
	(unwind-protect
	    (progn
	      (when modified-file
		(copy-file file modified-file)
		;; If we have a local copy of the unmodified file, handle that
		;; here and not in vc-revert-file because we don't want to
		;; delete that copy -- it is still useful for OLD-BACKEND.
		(if unmodified-file
		    (copy-file unmodified-file file 'ok-if-already-exists)
		  (if (y-or-n-p "Get base version from master? ")
		      (vc-revert-file file))))
	      (vc-call-backend new-backend 'receive-file file rev))
	  (when modified-file
	    (vc-switch-backend file new-backend)
	    (unless (eq (vc-checkout-model file) 'implicit)
	      (vc-checkout file t nil))
	    (rename-file modified-file file 'ok-if-already-exists)
	    (vc-file-setprop file 'vc-checkout-time nil)))))
    (when move
      (vc-switch-backend file old-backend)
      (setq comment (vc-call comment-history file))
      (vc-call unregister file))
    (vc-switch-backend file new-backend)
    (when (or move edited)
      (vc-file-setprop file 'vc-state 'edited)
      (vc-mode-line file)
      (vc-checkin file nil comment (stringp comment)))))

(defun vc-default-unregister (backend file)
  "Default implementation of `vc-unregister', signals an error."
  (error "Unregistering files is not supported for %s" backend))

(defun vc-default-receive-file (backend file rev)
  "Let BACKEND receive FILE from another version control system."
  (vc-call-backend backend 'register file rev ""))

(defun vc-rename-master (oldmaster newfile templates)
  "Rename OLDMASTER to be the master file for NEWFILE based on TEMPLATES."
  (let* ((dir (file-name-directory (expand-file-name oldmaster)))
	 (newdir (or (file-name-directory newfile) ""))
	 (newbase (file-name-nondirectory newfile))
	 (masters
	  ;; List of potential master files for `newfile'
	  (mapcar
	   (lambda (s) (vc-possible-master s newdir newbase))
	   templates)))
    (if (or (file-symlink-p oldmaster)
	    (file-symlink-p (file-name-directory oldmaster)))
	(error "This is unsafe in the presence of symbolic links"))
    (rename-file
     oldmaster
     (catch 'found
       ;; If possible, keep the master file in the same directory.
       (mapcar (lambda (f)
		 (if (and f (string= (file-name-directory (expand-file-name f))
				     dir))
		     (throw 'found f)))
	       masters)
       ;; If not, just use the first possible place.
       (mapcar (lambda (f)
		 (and f
		      (or (not (setq dir (file-name-directory f)))
			  (file-directory-p dir))
		      (throw 'found f)))
	       masters)
       (error "New file lacks a version control directory")))))

;;;###autoload
(defun vc-rename-file (old new)
  "Rename file OLD to NEW, and rename its master file likewise."
  (interactive "fVC rename file: \nFRename to: ")
  (let ((oldbuf (get-file-buffer old))
	(backend (vc-backend old)))
    (unless (or (null backend) (vc-find-backend-function backend 'rename-file))
      (error "Renaming files under %s is not supported in VC" backend))
    (if (and oldbuf (buffer-modified-p oldbuf))
	(error "Please save files before moving them"))
    (if (get-file-buffer new)
	(error "Already editing new file name"))
    (if (file-exists-p new)
	(error "New file already exists"))
    (when backend
      (if (and backend (not (vc-up-to-date-p old)))
	  (error "Please check in files before moving them"))
      (vc-call-backend backend 'rename-file old new))
    ;; Move the actual file (unless the backend did it already)
    (if (or (not backend) (file-exists-p old))
	(rename-file old new))
    ;; ?? Renaming a file might change its contents due to keyword expansion.
    ;; We should really check out a new copy if the old copy was precisely equal
    ;; to some checked in version.  However, testing for this is tricky....
    (if oldbuf
	(with-current-buffer oldbuf
	  (let ((buffer-read-only buffer-read-only))
	    (set-visited-file-name new))
	  (vc-backend new)
	  (vc-mode-line new)
	  (set-buffer-modified-p nil)))))

;; Only defined in very recent Emacsen
(defvar small-temporary-file-directory nil)

;;;###autoload
(defun vc-update-change-log (&rest args)
  "Find change log file and add entries from recent version control logs.
Normally, find log entries for all registered files in the default
directory.

With prefix arg of \\[universal-argument], only find log entries for the current buffer's file.

With any numeric prefix arg, find log entries for all currently visited
files that are under version control.  This puts all the entries in the
log for the default directory, which may not be appropriate.

From a program, any ARGS are assumed to be filenames for which
log entries should be gathered."
  (interactive
   (cond ((consp current-prefix-arg)	;C-u
	  (list buffer-file-name))
	 (current-prefix-arg		;Numeric argument.
	  (let ((files nil)
		(buffers (buffer-list))
		file)
	    (while buffers
	      (setq file (buffer-file-name (car buffers)))
	      (and file (vc-backend file)
		   (setq files (cons file files)))
	      (setq buffers (cdr buffers)))
	    files))
	 (t
          ;; Don't supply any filenames to backend; this means
          ;; it should find all relevant files relative to
          ;; the default-directory.
	  nil)))
  (vc-call-backend (vc-responsible-backend default-directory)
                   'update-changelog args))

(defun vc-default-update-changelog (backend files)
  "Default implementation of update-changelog.
Uses `rcs2log' which only works for RCS and CVS."
  ;; FIXME: We (c|sh)ould add support for cvs2cl
  (let ((odefault default-directory)
	(changelog (find-change-log))
	;; Presumably not portable to non-Unixy systems, along with rcs2log:
	(tempfile (funcall
		   (if (fboundp 'make-temp-file) 'make-temp-file 'make-temp-name)
		   (expand-file-name "vc"
				     (or small-temporary-file-directory
					 temporary-file-directory))))
	(full-name (or add-log-full-name
		       (user-full-name)
		       (user-login-name)
		       (format "uid%d" (number-to-string (user-uid)))))
	(mailing-address (or add-log-mailing-address
			     user-mail-address)))
    (find-file-other-window changelog)
    (barf-if-buffer-read-only)
    (vc-buffer-sync)
    (undo-boundary)
    (goto-char (point-min))
    (push-mark)
    (message "Computing change log entries...")
    (message "Computing change log entries... %s"
	     (unwind-protect
		 (progn
		   (setq default-directory odefault)
		   (if (eq 0 (apply 'call-process
                                    (expand-file-name "rcs2log"
                                                      exec-directory)
                                    nil (list t tempfile) nil
                                    "-c" changelog
                                    "-u" (concat (vc-user-login-name)
                                                 "\t" full-name
                                                 "\t" mailing-address)
                                    (mapcar
                                     (lambda (f)
                                       (file-relative-name
                                        (if (file-name-absolute-p f)
                                            f
                                          (concat odefault f))))
                                     files)))
                       "done"
		     (pop-to-buffer
		      (set-buffer (get-buffer-create "*vc*")))
		     (erase-buffer)
		     (insert-file tempfile)
		     "failed"))
	       (setq default-directory (file-name-directory changelog))
	       (delete-file tempfile)))))

;; Annotate functionality

;; Declare globally instead of additional parameter to
;; temp-buffer-show-function (not possible to pass more than one
;; parameter).
(defvar vc-annotate-ratio nil "Global variable.")
(defvar vc-annotate-backend nil "Global variable.")

(defun vc-annotate-get-backend (buffer)
  "Return the backend matching \"Annotate\" buffer BUFFER.
Return NIL if no match made.  Associations are made based on
`vc-annotate-buffers'."
  (cdr (assoc buffer vc-annotate-buffers)))

(define-derived-mode vc-annotate-mode fundamental-mode "Annotate"
  "Major mode for output buffers of the `vc-annotate' command.

You can use the mode-specific menu to alter the time-span of the used
colors.  See variable `vc-annotate-menu-elements' for customizing the
menu items."
  (vc-annotate-add-menu))

(defun vc-annotate-display-default (&optional event)
  "Use the default color spectrum for VC Annotate mode."
  (interactive "e")
  (message "Redisplaying annotation...")
  (vc-annotate-display (current-buffer)
		       nil
		       (vc-annotate-get-backend (current-buffer)))
  (message "Redisplaying annotation...done"))

(defun vc-annotate-add-menu ()
  "Add the menu 'Annotate' to the menu bar in VC-Annotate mode."
  (setq vc-annotate-mode-menu (make-sparse-keymap "Annotate"))
  (define-key vc-annotate-mode-map [menu-bar vc-annotate-mode]
    (cons "VC-Annotate" vc-annotate-mode-menu))
  (define-key vc-annotate-mode-menu [default]
    '("Default" . vc-annotate-display-default))
  (let ((menu-elements vc-annotate-menu-elements))
    (while menu-elements
      (let* ((element (car menu-elements))
	     (days (round (* element
			     (vc-annotate-car-last-cons vc-annotate-color-map)
			     0.7585))))
	(setq menu-elements (cdr menu-elements))
	(define-key vc-annotate-mode-menu
	  (vector days)
	  (cons (format "Span %d days"
			days)
		`(lambda ()
		   ,(format "Use colors spanning %d days" days)
		   (interactive)
		   (message "Redisplaying annotation...")
		   (vc-annotate-display
		    (get-buffer (buffer-name))
		    (vc-annotate-time-span vc-annotate-color-map ,element)
		    (vc-annotate-get-backend (current-buffer)))
		   (message "Redisplaying annotation...done"))))))))


;;;; (defun vc-BACKEND-annotate-command (file buffer) ...)
;;;;  Execute "annotate" on FILE by using `call-process' and insert
;;;;  the contents in BUFFER.

;;;###autoload
(defun vc-annotate (prefix)
  "Display the edit history of the current file using colours.

This command creates a buffer that shows, for each line of the current
file, when it was last edited and by whom.  Additionally, colours are
used to show the age of each line--blue means oldest, red means
youngest, and intermediate colours indicate intermediate ages.  By
default, the time scale stretches back one year into the past;
everything that is older than that is shown in blue.

With a prefix argument, this command asks two questions in the
minibuffer.  First, you may enter a version number; then the buffer
displays and annotates that version instead of the current version
\(type RET in the minibuffer to leave that default unchanged).  Then,
you are prompted for a stretch factor for the time scale.  This makes
the color range cover a time span longer or shorter than the default
of one year.  For example, a factor of 0.1 means that the range from
red to blue stands for the past 36 days only, and everything that is
older than that is shown in blue.

Customization variables:

`vc-annotate-menu-elements' customizes the menu elements of the
mode-specific menu. `vc-annotate-color-map' and
`vc-annotate-very-old-color' defines the mapping of time to
colors. `vc-annotate-background' specifies the background color."
  (interactive "P")
  (vc-ensure-vc-buffer)
  (let* ((temp-buffer-name (concat "*Annotate " (buffer-name) "*"))
         (temp-buffer-show-function 'vc-annotate-display)
         (rev (vc-workfile-version (buffer-file-name)))
         (vc-annotate-version 
          (if prefix (read-string 
                      (format "Annotate from version: (default %s) " rev) 
                      nil nil rev)
            rev))
         (vc-annotate-ratio 
          (if prefix (string-to-number
                      (read-string "Annotate ratio: (default 1.0) " 
                                   nil nil "1.0"))
            1.0))
         (vc-annotate-backend (vc-backend (buffer-file-name))))
    (message "Annotating...")
    (if (not (vc-find-backend-function vc-annotate-backend 'annotate-command))
	(error "Sorry, annotating is not implemented for %s"
	       vc-annotate-backend))
    (with-output-to-temp-buffer temp-buffer-name
      (vc-call-backend vc-annotate-backend 'annotate-command
		       (file-name-nondirectory (buffer-file-name))
		       (get-buffer temp-buffer-name)
                       vc-annotate-version))
    ;; Don't use the temp-buffer-name until the buffer is created
    ;; (only after `with-output-to-temp-buffer'.)
    (setq vc-annotate-buffers
	  (append vc-annotate-buffers
		  (list (cons (get-buffer temp-buffer-name) vc-annotate-backend))))
  (message "Annotating... done")))


(defun vc-annotate-car-last-cons (a-list)
  "Return car of last cons in association list A-LIST."
  (if (not (eq nil (cdr a-list)))
      (vc-annotate-car-last-cons (cdr a-list))
    (car (car a-list))))

(defun vc-annotate-time-span (a-list span &optional quantize)
  "Apply factor SPAN to the time-span of association list A-LIST.
Return the new alist.
Optionally quantize to the factor of QUANTIZE."
  ;; Apply span to each car of every cons
  (if (not (eq nil a-list))
      (append (list (cons (* (car (car a-list)) span)
			  (cdr (car a-list))))
	      (vc-annotate-time-span (nthcdr (or quantize ; optional
						 1) ; Default to cdr
					     a-list) span quantize))))

(defun vc-annotate-compcar (threshold a-list)
  "Test successive cons cells of A-LIST against THRESHOLD.
Return the first cons cell with a car that is not less than THRESHOLD,
nil if no such cell exists."
 (let ((i 1)
       (tmp-cons (car a-list)))
   (while (and tmp-cons (< (car tmp-cons) threshold))
     (setq tmp-cons (car (nthcdr i a-list)))
     (setq i (+ i 1)))
   tmp-cons))				; Return the appropriate value


(defun vc-annotate-display (buffer &optional color-map backend)
  "Do the VC-Annotate display in BUFFER using COLOR-MAP.
The original annotating file is supposed to be handled by BACKEND.
If BACKEND is NIL, variable VC-ANNOTATE-BACKEND is used instead.
This function is destructive on VC-ANNOTATE-BACKEND when BACKEND is non-nil."

  ;; Handle the case of the global variable vc-annotate-ratio being
  ;; set. This variable is used to pass information from function
  ;; vc-annotate since it is not possible to use another parameter
  ;; (see temp-buffer-show-function).
  (if (and (not color-map) vc-annotate-ratio)
      ;; This will only be true if called from vc-annotate with ratio
      ;; being non-nil.
      (setq color-map (vc-annotate-time-span vc-annotate-color-map
					     vc-annotate-ratio)))
  (set-buffer buffer)
  (display-buffer buffer)
  (if (not vc-annotate-mode)		; Turn on vc-annotate-mode if not done
      (vc-annotate-mode))
  (goto-char (point-min))		; Position at the top of the buffer.
  ;; Delete old overlays
  (mapcar
   (lambda (overlay)
     (if (overlay-get overlay 'vc-annotation)
	 (delete-overlay overlay)))
   (overlays-in (point-min) (point-max)))
  (goto-char (point-min))		; Position at the top of the buffer.

  (if backend (setq vc-annotate-backend backend)) ; Destructive on `vc-annotate-backend'

  (let ((difference (vc-call-backend vc-annotate-backend 'annotate-difference (point))))
    (while difference
      (let*
	  ((color (or (vc-annotate-compcar
		       difference (or color-map vc-annotate-color-map))
		      (cons nil vc-annotate-very-old-color)))
	   ;; substring from index 1 to remove any leading `#' in the name
	   (face-name (concat "vc-annotate-face-" (substring (cdr color) 1)))
	   ;; Make the face if not done.
	   (face (or (intern-soft face-name)
		     (let ((tmp-face (make-face (intern face-name))))
		       (set-face-foreground tmp-face (cdr color))
		       (if vc-annotate-background
			   (set-face-background tmp-face vc-annotate-background))
		       tmp-face)))	; Return the face
	   (point (point))
	   overlay)
	(forward-line 1)
	(setq overlay (make-overlay point (point)))
	(overlay-put overlay 'face face)
	(overlay-put overlay 'vc-annotation t))
      (setq difference (vc-call-backend vc-annotate-backend 'annotate-difference (point))))))


;; Collect back-end-dependent stuff here

(defalias 'vc-default-logentry-check 'ignore)

(defun vc-check-headers ()
  "Check if the current file has any headers in it."
  (interactive)
  (vc-call-backend (vc-backend buffer-file-name) 'check-headers))

(defun vc-default-check-headers (backend)
  "Default implementation of check-headers; always returns nil."
  nil)

;; Back-end-dependent stuff ends here.

;; Set up key bindings for use while editing log messages

(define-derived-mode vc-log-mode text-mode "VC-Log"
  "Major mode for editing VC log entries.
These bindings are added to the global keymap when you enter this mode:
\\[vc-next-action]		perform next logical version-control operation on current file
\\[vc-register]		register current file
\\[vc-toggle-read-only]		like next-action, but won't register files
\\[vc-insert-headers]		insert version-control headers in current file
\\[vc-print-log]		display change history of current file
\\[vc-revert-buffer]		revert buffer to latest version
\\[vc-cancel-version]		undo latest checkin
\\[vc-diff]		show diffs between file versions
\\[vc-version-other-window]		visit old version in another window
\\[vc-directory]		show all files locked by any user in or below .
\\[vc-annotate]		colorful display of the cvs annotate command
\\[vc-update-change-log]		add change log entry from recent checkins

While you are entering a change log message for a version, the following
additional bindings will be in effect.

\\[vc-finish-logentry]	proceed with check in, ending log message entry

Whenever you do a checkin, your log comment is added to a ring of
saved comments.  These can be recalled as follows:

\\[vc-next-comment]	replace region with next message in comment ring
\\[vc-previous-comment]	replace region with previous message in comment ring
\\[vc-comment-search-reverse]	search backward for regexp in the comment ring
\\[vc-comment-search-forward]	search backward for regexp in the comment ring

Entry to the change-log submode calls the value of `text-mode-hook', then
the value of `vc-log-mode-hook'.

Global user options:
	`vc-initial-comment'	If non-nil, require user to enter a change
				comment upon first checkin of the file.

	`vc-keep-workfiles'	Non-nil value prevents workfiles from being
				deleted when changes are checked in

        `vc-suppress-confirm'	Suppresses some confirmation prompts.

	vc-BACKEND-header	Which keywords to insert when adding headers
				with \\[vc-insert-headers].  Defaults to
				'(\"\%\W\%\") under SCCS, '(\"\$Id\$\") under
				RCS and CVS.

	`vc-static-header-alist' By default, version headers inserted in C files
				get stuffed in a static string area so that
				ident(RCS/CVS) or what(SCCS) can see them in
				the compiled object code.  You can override
				this by setting this variable to nil, or change
				the header template by changing it.

	`vc-command-messages'	if non-nil, display run messages from the
				actual version-control utilities (this is
				intended primarily for people hacking vc
				itself)."
  (make-local-variable 'vc-comment-ring-index))

(defun vc-log-edit (file)
  "Set up `log-edit' for use with VC on FILE.
If `log-edit' is not available, resort to `vc-log-mode'."
  (setq default-directory
	(if file (file-name-directory file)
	  (with-current-buffer vc-parent-buffer default-directory)))
  (if (fboundp 'log-edit)
      (log-edit 'vc-finish-logentry nil
		(if file `(lambda () ',(list (file-name-nondirectory file)))
		  ;; If FILE is nil, we were called from vc-dired.
		  (lambda ()
		    (with-current-buffer vc-parent-buffer
		      (dired-get-marked-files t)))))
    (vc-log-mode))
  (set (make-local-variable 'vc-log-file) file)
  (make-local-variable 'vc-log-version)
  (set-buffer-modified-p nil)
  (setq buffer-file-name nil))

;; These things should probably be generally available

(defun vc-file-tree-walk (dirname func &rest args)
  "Walk recursively through DIRNAME.
Invoke FUNC f ARGS on each VC-managed file f underneath it."
  (vc-file-tree-walk-internal (expand-file-name dirname) func args)
  (message "Traversing directory %s...done" dirname))

(defun vc-file-tree-walk-internal (file func args)
  (if (not (file-directory-p file))
      (if (vc-backend file) (apply func file args))
    (message "Traversing directory %s..." (abbreviate-file-name file))
    (let ((dir (file-name-as-directory file)))
      (mapcar
       (lambda (f) (or
		    (string-equal f ".")
		    (string-equal f "..")
		    (member f vc-directory-exclusion-list)
		    (let ((dirf (expand-file-name f dir)))
		      (or
		       (file-symlink-p dirf);; Avoid possible loops
		       (vc-file-tree-walk-internal dirf func args)))))
       (directory-files dir)))))

(provide 'vc)

;; DEVELOPER'S NOTES ON CONCURRENCY PROBLEMS IN THIS CODE
;;
;; These may be useful to anyone who has to debug or extend the package.
;; (Note that this information corresponds to versions 5.x. Some of it
;; might have been invalidated by the additions to support branching
;; and RCS keyword lookup. AS, 1995/03/24)
;;
;; A fundamental problem in VC is that there are time windows between
;; vc-next-action's computations of the file's version-control state and
;; the actions that change it.  This is a window open to lossage in a
;; multi-user environment; someone else could nip in and change the state
;; of the master during it.
;;
;; The performance problem is that rlog/prs calls are very expensive; we want
;; to avoid them as much as possible.
;;
;; ANALYSIS:
;;
;; The performance problem, it turns out, simplifies in practice to the
;; problem of making vc-state fast.  The two other functions that call
;; prs/rlog will not be so commonly used that the slowdown is a problem; one
;; makes snapshots, the other deletes the calling user's last change in the
;; master.
;;
;; The race condition implies that we have to either (a) lock the master
;; during the entire execution of vc-next-action, or (b) detect and
;; recover from errors resulting from dispatch on an out-of-date state.
;;
;; Alternative (a) appears to be infeasible.  The problem is that we can't
;; guarantee that the lock will ever be removed.  Suppose a user starts a
;; checkin, the change message buffer pops up, and the user, having wandered
;; off to do something else, simply forgets about it?
;;
;; Alternative (b), on the other hand, works well with a cheap way to speed up
;; vc-state.  Usually, if a file is registered, we can read its locked/
;; unlocked state and its current owner from its permissions.
;;
;; This shortcut will fail if someone has manually changed the workfile's
;; permissions; also if developers are munging the workfile in several
;; directories, with symlinks to a master (in this latter case, the
;; permissions shortcut will fail to detect a lock asserted from another
;; directory).
;;
;; Note that these cases correspond exactly to the errors which could happen
;; because of a competing checkin/checkout race in between two instances of
;; vc-next-action.
;;
;; For VC's purposes, a workfile/master pair may have the following states:
;;
;; A. Unregistered.  There is a workfile, there is no master.
;;
;; B. Registered and not locked by anyone.
;;
;; C. Locked by calling user and unchanged.
;;
;; D. Locked by the calling user and changed.
;;
;; E. Locked by someone other than the calling user.
;;
;; This makes for 25 states and 20 error conditions.  Here's the matrix:
;;
;; VC's idea of state
;;  |
;;  V  Actual state   RCS action              SCCS action          Effect
;;    A  B  C  D  E
;;  A .  1  2  3  4   ci -u -t-          admin -fb -i<file>      initial admin
;;  B 5  .  6  7  8   co -l              get -e                  checkout
;;  C 9  10 .  11 12  co -u              unget; get              revert
;;  D 13 14 15 .  16  ci -u -m<comment>  delta -y<comment>; get  checkin
;;  E 17 18 19 20 .   rcs -u -M -l       unget -n ; get -g       steal lock
;;
;; All commands take the master file name as a last argument (not shown).
;;
;; In the discussion below, a "self-race" is a pathological situation in
;; which VC operations are being attempted simultaneously by two or more
;; Emacsen running under the same username.
;;
;; The vc-next-action code has the following windows:
;;
;; Window P:
;;    Between the check for existence of a master file and the call to
;; admin/checkin in vc-buffer-admin (apparent state A).  This window may
;; never close if the initial-comment feature is on.
;;
;; Window Q:
;;    Between the call to vc-workfile-unchanged-p in and the immediately
;; following revert (apparent state C).
;;
;; Window R:
;;    Between the call to vc-workfile-unchanged-p in and the following
;; checkin (apparent state D).  This window may never close.
;;
;; Window S:
;;    Between the unlock and the immediately following checkout during a
;; revert operation (apparent state C).  Included in window Q.
;;
;; Window T:
;;    Between vc-state and the following checkout (apparent state B).
;;
;; Window U:
;;    Between vc-state and the following revert (apparent state C).
;; Includes windows Q and S.
;;
;; Window V:
;;    Between vc-state and the following checkin (apparent state
;; D).  This window may never be closed if the user fails to complete the
;; checkin message.  Includes window R.
;;
;; Window W:
;;    Between vc-state and the following steal-lock (apparent
;; state E).  This window may never close if the user fails to complete
;; the steal-lock message.  Includes window X.
;;
;; Window X:
;;    Between the unlock and the immediately following re-lock during a
;; steal-lock operation (apparent state E).  This window may never close
;; if the user fails to complete the steal-lock message.
;;
;; Errors:
;;
;; Apparent state A ---
;;
;; 1. File looked unregistered but is actually registered and not locked.
;;
;;    Potential cause: someone else's admin during window P, with
;; caller's admin happening before their checkout.
;;
;;    RCS: Prior to version 5.6.4, ci fails with message
;;         "no lock set by <user>".  From 5.6.4 onwards, VC uses the new
;;         ci -i option and the message is "<file>,v: already exists".
;;    SCCS: admin will fail with error (ad19).
;;
;;    We can let these errors be passed up to the user.
;;
;; 2. File looked unregistered but is actually locked by caller, unchanged.
;;
;;    Potential cause: self-race during window P.
;;
;;    RCS: Prior to version 5.6.4, reverts the file to the last saved
;;         version and unlocks it.  From 5.6.4 onwards, VC uses the new
;;         ci -i option, failing with message "<file>,v: already exists".
;;    SCCS: will fail with error (ad19).
;;
;;    Either of these consequences is acceptable.
;;
;; 3. File looked unregistered but is actually locked by caller, changed.
;;
;;    Potential cause: self-race during window P.
;;
;;    RCS: Prior to version 5.6.4, VC registers the caller's workfile as
;;         a delta with a null change comment (the -t- switch will be
;;         ignored). From 5.6.4 onwards, VC uses the new ci -i option,
;;         failing with message "<file>,v: already exists".
;;    SCCS: will fail with error (ad19).
;;
;; 4. File looked unregistered but is locked by someone else.
;;;
;;    Potential cause: someone else's admin during window P, with
;; caller's admin happening *after* their checkout.
;;
;;    RCS: Prior to version 5.6.4, ci fails with a
;;         "no lock set by <user>" message.  From 5.6.4 onwards,
;;         VC uses the new ci -i option, failing with message
;;         "<file>,v: already exists".
;;    SCCS: will fail with error (ad19).
;;
;;    We can let these errors be passed up to the user.
;;
;; Apparent state B ---
;;
;; 5. File looked registered and not locked, but is actually unregistered.
;;
;;    Potential cause: master file got nuked during window P.
;;
;;    RCS: will fail with "RCS/<file>: No such file or directory"
;;    SCCS: will fail with error ut4.
;;
;;    We can let these errors be passed up to the user.
;;
;; 6. File looked registered and not locked, but is actually locked by the
;; calling user and unchanged.
;;
;;    Potential cause: self-race during window T.
;;
;;    RCS: in the same directory as the previous workfile, co -l will fail
;; with "co error: writable foo exists; checkout aborted".  In any other
;; directory, checkout will succeed.
;;    SCCS: will fail with ge17.
;;
;;    Either of these consequences is acceptable.
;;
;; 7. File looked registered and not locked, but is actually locked by the
;; calling user and changed.
;;
;;    As case 6.
;;
;; 8. File looked registered and not locked, but is actually locked by another
;; user.
;;
;;    Potential cause: someone else checks it out during window T.
;;
;;    RCS: co error: revision 1.3 already locked by <user>
;;    SCCS: fails with ge4 (in directory) or ut7 (outside it).
;;
;;    We can let these errors be passed up to the user.
;;
;; Apparent state C ---
;;
;; 9. File looks locked by calling user and unchanged, but is unregistered.
;;
;;    As case 5.
;;
;; 10. File looks locked by calling user and unchanged, but is actually not
;; locked.
;;
;;    Potential cause: a self-race in window U, or by the revert's
;; landing during window X of some other user's steal-lock or window S
;; of another user's revert.
;;
;;    RCS: succeeds, refreshing the file from the identical version in
;; the master.
;;    SCCS: fails with error ut4 (p file nonexistent).
;;
;;    Either of these consequences is acceptable.
;;
;; 11. File is locked by calling user.  It looks unchanged, but is actually
;; changed.
;;
;;    Potential cause: the file would have to be touched by a self-race
;; during window Q.
;;
;;    The revert will succeed, removing whatever changes came with
;; the touch.  It is theoretically possible that work could be lost.
;;
;; 12. File looks like it's locked by the calling user and unchanged, but
;; it's actually locked by someone else.
;;
;;    Potential cause: a steal-lock in window V.
;;
;;    RCS: co error: revision <rev> locked by <user>; use co -r or rcs -u
;;    SCCS: fails with error un2
;;
;;    We can pass these errors up to the user.
;;
;; Apparent state D ---
;;
;; 13. File looks like it's locked by the calling user and changed, but it's
;; actually unregistered.
;;
;;    Potential cause: master file got nuked during window P.
;;
;;    RCS: Prior to version 5.6.4, checks in the user's version as an
;;         initial delta.  From 5.6.4 onwards, VC uses the new ci -j
;;         option, failing with message "no such file or directory".
;;    SCCS: will fail with error ut4.
;;
;;    This case is kind of nasty.  Under RCS prior to version 5.6.4,
;; VC may fail to detect the loss of previous version information.
;;
;; 14. File looks like it's locked by the calling user and changed, but it's
;; actually unlocked.
;;
;;    Potential cause: self-race in window V, or the checkin happening
;; during the window X of someone else's steal-lock or window S of
;; someone else's revert.
;;
;;    RCS: ci will fail with "no lock set by <user>".
;;    SCCS: delta will fail with error ut4.
;;
;; 15. File looks like it's locked by the calling user and changed, but it's
;; actually locked by the calling user and unchanged.
;;
;;    Potential cause: another self-race --- a whole checkin/checkout
;; sequence by the calling user would have to land in window R.
;;
;;    SCCS: checks in a redundant delta and leaves the file unlocked as usual.
;;    RCS: reverts to the file state as of the second user's checkin, leaving
;; the file unlocked.
;;
;;    It is theoretically possible that work could be lost under RCS.
;;
;; 16. File looks like it's locked by the calling user and changed, but it's
;; actually locked by a different user.
;;
;;    RCS: ci error: no lock set by <user>
;;    SCCS: unget will fail with error un2
;;
;;    We can pass these errors up to the user.
;;
;; Apparent state E ---
;;
;; 17. File looks like it's locked by some other user, but it's actually
;; unregistered.
;;
;;    As case 13.
;;
;; 18. File looks like it's locked by some other user, but it's actually
;; unlocked.
;;
;;    Potential cause: someone released a lock during window W.
;;
;;    RCS: The calling user will get the lock on the file.
;;    SCCS: unget -n will fail with cm4.
;;
;;    Either of these consequences will be OK.
;;
;; 19. File looks like it's locked by some other user, but it's actually
;; locked by the calling user and unchanged.
;;
;;    Potential cause: the other user relinquishing a lock followed by
;; a self-race, both in window W.
;;
;;     Under both RCS and SCCS, both unlock and lock will succeed, making
;; the sequence a no-op.
;;
;; 20. File looks like it's locked by some other user, but it's actually
;; locked by the calling user and changed.
;;
;;     As case 19.
;;
;; PROBLEM CASES:
;;
;;    In order of decreasing severity:
;;
;;    Cases 11 and 15 are the only ones that potentially lose work.
;; They would require a self-race for this to happen.
;;
;;    Case 13 in RCS loses information about previous deltas, retaining
;; only the information in the current workfile.  This can only happen
;; if the master file gets nuked in window P.
;;
;;    Case 3 in RCS and case 15 under SCCS insert a redundant delta with
;; no change comment in the master.  This would require a self-race in
;; window P or R respectively.
;;
;;    Cases 2, 10, 19 and 20 do extra work, but make no changes.
;;
;;    Unfortunately, it appears to me that no recovery is possible in these
;; cases.  They don't yield error messages, so there's no way to tell that
;; a race condition has occurred.
;;
;;    All other cases don't change either the workfile or the master, and
;; trigger command errors which the user will see.
;;
;;    Thus, there is no explicit recovery code.

;;; vc.el ends here
