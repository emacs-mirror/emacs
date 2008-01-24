;;; vc.el --- drive a version-control system from within Emacs

;; Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998, 2000,
;;   2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author:     FSF (see below for full credits)
;; Maintainer: Andre Spiegel <spiegel@gnu.org>
;; Keywords: tools

;; $Id$

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

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
;;   J.D. Smith <jdsmith@alum.mit.edu>
;;   Andre Spiegel <spiegel@gnu.org>
;;   Richard Stallman <rms@gnu.org>
;;   Thien-Thi Nguyen <ttn@gnu.org>

;;; Commentary:

;; This mode is fully documented in the Emacs user's manual.
;;
;; Supported version-control systems presently include CVS, RCS, GNU
;; Arch, Subversion, Bzr, Git, Mercurial, Meta-CVS, Monotone and SCCS
;; (or its free replacement, CSSC).
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
;; function log-edit-comment-to-change-log could prove a useful checkin hook,
;; although you might prefer to use C-c C-a (i.e. log-edit-insert-changelog)
;; from the commit buffer instead or to set `log-edit-setup-invert'.
;;
;; The vc code maintains some internal state in order to reduce expensive
;; version-control operations to a minimum.  Some names are only computed
;; once.  If you perform version control operations with the backend while
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
;;   Return non-nil if FILE is registered in this backend.  Both this
;;   function as well as `state' should be careful to fail gracefully
;;   in the event that the backend executable is absent.  It is
;;   preferable that this function's body is autoloaded, that way only
;;   calling vc-registered does not cause the backend to be loaded
;;   (all the vc-FOO-registered functions are called to try to find
;;   the controlling backend for FILE.
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
;;   vc-BACKEND-diff.  (Note that vc-BACKEND-diff must not run
;;   asynchronously in this case, see variable `vc-disable-async-diff'.)
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
;;   The implementation should pass the value of vc-register-switches
;;   to the backend command.
;;
;; - init-version (file)
;;
;;   The initial version to use when registering FILE if one is not
;;   specified by the user.  If not provided, the variable
;;   vc-default-init-version is used instead.
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
;;   check-in comment.  The implementation should pass the value of
;;   vc-checkin-switches to the backend command.
;;
;; * find-version (file rev buffer)
;;
;;   Fetch revision REV of file FILE and put it into BUFFER.
;;   If REV is the empty string, fetch the head of the trunk.
;;   The implementation should pass the value of vc-checkout-switches
;;   to the backend command.
;;
;; * checkout (file &optional editable rev)
;;
;;   Check out revision REV of FILE into the working area.  If EDITABLE
;;   is non-nil, FILE should be writable by the user and if locking is
;;   used for FILE, a lock should also be set.  If REV is non-nil, that
;;   is the revision to check out (default is current workfile version).
;;   If REV is t, that means to check out the head of the current branch;
;;   if it is the empty string, check out the head of the trunk.
;;   The implementation should pass the value of vc-checkout-switches
;;   to the backend command.
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
;; * print-log (file &optional buffer)
;;
;;   Insert the revision log of FILE into BUFFER, or the *vc* buffer
;;   if BUFFER is nil.
;;
;; - log-view-mode ()
;;
;;   Mode to use for the output of print-log.  This defaults to
;;   `log-view-mode' and is expected to be changed (if at all) to a derived
;;   mode of `log-view-mode'.
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
;; * diff (file &optional rev1 rev2 buffer)
;;
;;   Insert the diff for FILE into BUFFER, or the *vc-diff* buffer if
;;   BUFFER is nil.  If REV1 and REV2 are non-nil, report differences
;;   from REV1 to REV2.  If REV1 is nil, use the current workfile
;;   version (as found in the repository) as the older version; if
;;   REV2 is nil, use the current workfile contents as the newer
;;   version.  This function should pass the value of (vc-switches
;;   BACKEND 'diff) to the backend command.  It should return a status
;;   of either 0 (no differences found), or 1 (either non-empty diff
;;   or the diff is run asynchronously).
;;
;; - revision-completion-table (file)
;;
;;   Return a completion table for existing revisions of FILE.
;;   The default is to not use any completion table.
;;
;; - diff-tree (dir &optional rev1 rev2)
;;
;;   Insert the diff for all files at and below DIR into the *vc-diff*
;;   buffer.  The meaning of REV1 and REV2 is the same as for
;;   vc-BACKEND-diff.  The default implementation does an explicit tree
;;   walk, calling vc-BACKEND-diff for each individual file.
;;
;; - annotate-command (file buf &optional rev)
;;
;;   If this function is provided, it should produce an annotated display
;;   of FILE in BUF, relative to version REV.  Annotation means each line
;;   of FILE displayed is prefixed with version information associated with
;;   its addition (deleted lines leave no history) and that the text of the
;;   file is fontified according to age.
;;
;; - annotate-time ()
;;
;;   Only required if `annotate-command' is defined for the backend.
;;   Return the time of the next line of annotation at or after point,
;;   as a floating point fractional number of days.  The helper
;;   function `vc-annotate-convert-time' may be useful for converting
;;   multi-part times as returned by `current-time' and `encode-time'
;;   to this format.  Return nil if no more lines of annotation appear
;;   in the buffer.  You can safely assume that point is placed at the
;;   beginning of each line, starting at `point-min'.  The buffer that
;;   point is placed in is the Annotate output, as defined by the
;;   relevant backend.  This function also affects how much of the line
;;   is fontified; where it leaves point is where fontification begins.
;;
;; - annotate-current-time ()
;;
;;   Only required if `annotate-command' is defined for the backend,
;;   AND you'd like the current time considered to be anything besides
;;   (vs-annotate-convert-time (current-time)) -- i.e. the current
;;   time with hours, minutes, and seconds included.  Probably safe to
;;   ignore.  Return the current-time, in units of fractional days.
;;
;; - annotate-extract-revision-at-line ()
;;
;;   Only required if `annotate-command' is defined for the backend.
;;   Invoked from a buffer in vc-annotate-mode, return the revision
;;   corresponding to the current line, or nil if there is no revision
;;   corresponding to the current line.
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
;; - repository-hostname (dirname)
;;
;;   Return the hostname that the backend will have to contact
;;   in order to operate on a file in DIRNAME.  If the return value
;;   is nil, it means that the repository is local.
;;   This function is used in `vc-stay-local-p' which backends can use
;;   for their convenience.
;;
;; - previous-version (file rev)
;;
;;   Return the version number that precedes REV for FILE, or nil if no such
;;   version exists.
;;
;; - next-version (file rev)
;;
;;   Return the version number that follows REV for FILE, or nil if no such
;;   version exists.
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
;; - delete-file (file)
;;
;;   Delete FILE and mark it as deleted in the repository.  If this
;;   function is not provided, the command `vc-delete-file' will
;;   signal an error.
;;
;; - rename-file (old new)
;;
;;   Rename file OLD to NEW, both in the working area and in the
;;   repository.  If this function is not provided, the renaming
;;   will be done by (vc-delete-file old) and (vc-register new).
;;
;; - find-file-hook ()
;;
;;   Operation called in current buffer when opening a file.  This can
;;   be used by the backend to setup some local variables it might need.
;
;; - find-file-not-found-hook ()
;;
;;   Operation called in current buffer when opening a non-existing file.
;;   By default, this asks the user if she wants to check out the file.
;;
;; - extra-menu ()
;;
;;   Return a menu keymap, the items in the keymap will appear at the
;;   end of the Version Control menu.  The goal is to allow backends
;;   to specify extra menu items that appear in the VC menu.  This way
;;   you can provide menu entries for functionality that is specific
;;   to your backend and which does not map to any of the VC generic
;;   concepts.

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
  "If non-nil, treat user as expert; suppress yes-no prompts on some things."
  :type 'boolean
  :group 'vc)

(defcustom vc-delete-logbuf-window t
  "If non-nil, delete the *VC-log* buffer and window after each logical action.
If nil, bury that buffer instead.
This is most useful if you have multiple windows on a frame and would like to
preserve the setting."
  :type 'boolean
  :group 'vc)

(defcustom vc-initial-comment nil
  "If non-nil, prompt for initial comment when a file is registered."
  :type 'boolean
  :group 'vc)

(defcustom vc-default-init-version "1.1"
  "A string used as the default version number when a new file is registered.
This can be overridden by giving a prefix argument to \\[vc-register].  This
can also be overridden by a particular VC backend."
  :type 'string
  :group 'vc
  :version "20.3")

(defcustom vc-command-messages nil
  "If non-nil, display run messages from back-end commands."
  :type 'boolean
  :group 'vc)

(defcustom vc-checkin-switches nil
  "A string or list of strings specifying extra switches for checkin.
These are passed to the checkin program by \\[vc-checkin]."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string))
  :group 'vc)

(defcustom vc-checkout-switches nil
  "A string or list of strings specifying extra switches for checkout.
These are passed to the checkout program by \\[vc-checkout]."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string))
  :group 'vc)

(defcustom vc-register-switches nil
  "A string or list of strings; extra switches for registering a file.
These are passed to the checkin program by \\[vc-register]."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string))
  :group 'vc)

(defcustom vc-dired-listing-switches "-al"
  "Switches passed to `ls' for vc-dired.  MUST contain the `l' option."
  :type 'string
  :group 'vc
  :version "21.1")

(defcustom vc-dired-recurse t
  "If non-nil, show directory trees recursively in VC Dired."
  :type 'boolean
  :group 'vc
  :version "20.3")

(defcustom vc-dired-terse-display t
  "If non-nil, show only locked files in VC Dired."
  :type 'boolean
  :group 'vc
  :version "20.3")

(defcustom vc-directory-exclusion-list '("SCCS" "RCS" "CVS" "MCVS" ".svn" 
					 ".git" ".hg" "{arch}")
  "List of directory names to be ignored when walking directory trees."
  :type '(repeat string)
  :group 'vc)

(defcustom vc-diff-switches nil
  "A string or list of strings specifying switches for diff under VC.
When running diff under a given BACKEND, VC concatenates the values of
`diff-switches', `vc-diff-switches', and `vc-BACKEND-diff-switches' to
get the switches for that command.  Thus, `vc-diff-switches' should
contain switches that are specific to version control, but not
specific to any particular backend."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string))
  :group 'vc
  :version "21.1")

(defcustom vc-diff-knows-L nil
  "*Indicates whether diff understands the -L option.
The value is either `yes', `no', or nil.  If it is nil, VC tries
to use -L and sets this variable to remember whether it worked."
  :type '(choice (const :tag "Work out" nil) (const yes) (const no))
  :group 'vc)

(defcustom vc-allow-async-revert nil
  "Specifies whether the diff during \\[vc-revert-buffer] may be asynchronous.
Enabling this option means that you can confirm a revert operation even
if the local changes in the file have not been found and displayed yet."
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t))
  :group 'vc
  :version "22.1")

;;;###autoload
(defcustom vc-checkout-hook nil
  "Normal hook (list of functions) run after checking out a file.
See `run-hooks'."
  :type 'hook
  :group 'vc
  :version "21.1")

(defcustom vc-annotate-display-mode 'fullscale
  "Which mode to color the output of \\[vc-annotate] with by default."
  :type '(choice (const :tag "By Color Map Range" nil)
		 (const :tag "Scale to Oldest" scale)
		 (const :tag "Scale Oldest->Newest" fullscale)
		 (number :tag "Specify Fractional Number of Days"
			 :value "20.5"))
  :group 'vc)

;;;###autoload
(defcustom vc-checkin-hook nil
  "Normal hook (list of functions) run after a checkin is done.
See also `log-edit-done-hook'."
  :type 'hook
  :options '(log-edit-comment-to-change-log)
  :group 'vc)

;;;###autoload
(defcustom vc-before-checkin-hook nil
  "Normal hook (list of functions) run before a file is checked in.
See `run-hooks'."
  :type 'hook
  :group 'vc)

(defcustom vc-logentry-check-hook nil
  "Normal hook run by `vc-backend-logentry-check'.
Use this to impose your own rules on the entry in addition to any the
version control backend imposes itself."
  :type 'hook
  :group 'vc)

;; Annotate customization
(defcustom vc-annotate-color-map
  (if (and (tty-display-color-p) (<= (display-color-cells) 8))
      ;; A custom sorted TTY colormap
      (let* ((colors
	      (sort
	       (delq nil
		     (mapcar (lambda (x)
			       (if (not (or
					 (string-equal (car x) "white")
					 (string-equal (car x) "black") ))
				   (car x)))
			     (tty-color-alist)))
	       (lambda (a b)
		 (cond
		  ((or (string-equal a "red") (string-equal b "blue")) t)
		  ((or (string-equal b "red") (string-equal a "blue")) nil)
		  ((string-equal a "yellow") t)
		  ((string-equal b "yellow") nil)
		  ((string-equal a "cyan") t)
		  ((string-equal b "cyan") nil)
		  ((string-equal a "green") t)
		  ((string-equal b "green") nil)
		  ((string-equal a "magenta") t)
		  ((string-equal b "magenta") nil)
		  (t (string< a b))))))
	     (date 20.)
	     (delta (/ (- 360. date) (1- (length colors)))))
	(mapcar (lambda (x)
		  (prog1
		      (cons date x)
		    (setq date (+ date delta)))) colors))
    ;; Normal colormap: hue stepped from 0-240deg, value=1., saturation=0.75
    '(( 20. . "#FF3F3F")
      ( 40. . "#FF6C3F")
      ( 60. . "#FF993F")
      ( 80. . "#FFC63F")
      (100. . "#FFF33F")
      (120. . "#DDFF3F")
      (140. . "#B0FF3F")
      (160. . "#83FF3F")
      (180. . "#56FF3F")
      (200. . "#3FFF56")
      (220. . "#3FFF83")
      (240. . "#3FFFB0")
      (260. . "#3FFFDD")
      (280. . "#3FF3FF")
      (300. . "#3FC6FF")
      (320. . "#3F99FF")
      (340. . "#3F6CFF")
      (360. . "#3F3FFF")))
  "Association list of age versus color, for \\[vc-annotate].
Ages are given in units of fractional days.  Default is eighteen
steps using a twenty day increment, from red to blue.  For TTY
displays with 8 or fewer colors, the default is red to blue with
all other colors between (excluding black and white)."
  :type 'alist
  :group 'vc)

(defcustom vc-annotate-very-old-color "#3F3FFF"
  "Color for lines older than the current color range in \\[vc-annotate]]."
  :type 'string
  :group 'vc)

(defcustom vc-annotate-background "black"
  "Background color for \\[vc-annotate].
Default color is used if nil."
  :type 'string
  :group 'vc)

(defcustom vc-annotate-menu-elements '(2 0.5 0.1 0.01)
  "Menu elements for the mode-specific menu of VC-Annotate mode.
List of factors, used to expand/compress the time scale.  See `vc-annotate'."
  :type '(repeat number)
  :group 'vc)

(defvar vc-annotate-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "A" 'vc-annotate-revision-previous-to-line)
    (define-key m "D" 'vc-annotate-show-diff-revision-at-line)
    (define-key m "J" 'vc-annotate-revision-at-line)
    (define-key m "L" 'vc-annotate-show-log-revision-at-line)
    (define-key m "N" 'vc-annotate-next-version)
    (define-key m "P" 'vc-annotate-prev-version)
    (define-key m "W" 'vc-annotate-workfile-version)
    m)
  "Local keymap used for VC-Annotate mode.")

;; Header-insertion hair

(defcustom vc-static-header-alist
  '(("\\.c\\'" .
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
  "*Non-nil means be extra-careful in checkout.
Verify that the file really is not locked
and that its contents match what the master file says."
  :type 'boolean
  :group 'vc)
(make-obsolete-variable 'vc-checkout-carefully
                        "the corresponding checks are always done now."
                        "21.1")


;; Variables the user doesn't need to know about.
(defvar vc-log-operation nil)
(defvar vc-log-after-operation-hook nil)

;; In a log entry buffer, this is a local variable
;; that points to the buffer for which it was made
;; (either a file, or a VC dired buffer).
(defvar vc-parent-buffer nil)
(put 'vc-parent-buffer 'permanent-local t)
(defvar vc-parent-buffer-name nil)
(put 'vc-parent-buffer-name 'permanent-local t)

(defvar vc-disable-async-diff nil
  "VC sets this to t locally to disable some async diff operations.
Backends that offer asynchronous diffs should respect this variable
in their implementation of vc-BACKEND-diff.")

(defvar vc-log-file)
(defvar vc-log-version)

(defvar vc-dired-mode nil)
(make-variable-buffer-local 'vc-dired-mode)

;; functions that operate on RCS revision numbers.  This code should
;; also be moved into the backends.  It stays for now, however, since
;; it is used in code below.
;;;###autoload
(defun vc-trunk-p (rev)
  "Return t if REV is a revision on the trunk."
  (not (eq nil (string-match "\\`[0-9]+\\.[0-9]+\\'" rev))))

(defun vc-branch-p (rev)
  "Return t if REV is a branch revision."
  (not (eq nil (string-match "\\`[0-9]+\\(\\.[0-9]+\\.[0-9]+\\)*\\'" rev))))

;;;###autoload
(defun vc-branch-part (rev)
  "Return the branch part of a revision number REV."
  (let ((index (string-match "\\.[0-9]+\\'" rev)))
    (if index
        (substring rev 0 index))))

(defun vc-minor-part (rev)
  "Return the minor version number of a revision number REV."
  (string-match "[0-9]+\\'" rev)
  (substring rev (match-beginning 0) (match-end 0)))

(defun vc-default-previous-version (backend file rev)
  "Return the version number immediately preceding REV for FILE,
or nil if there is no previous version.  This default
implementation works for MAJOR.MINOR-style version numbers as
used by RCS and CVS."
  (let ((branch (vc-branch-part rev))
        (minor-num (string-to-number (vc-minor-part rev))))
    (when branch
      (if (> minor-num 1)
          ;; version does probably not start a branch or release
          (concat branch "." (number-to-string (1- minor-num)))
        (if (vc-trunk-p rev)
            ;; we are at the beginning of the trunk --
            ;; don't know anything to return here
            nil
          ;; we are at the beginning of a branch --
          ;; return version of starting point
          (vc-branch-part branch))))))

(defun vc-default-next-version (backend file rev)
  "Return the version number immediately following REV for FILE,
or nil if there is no next version.  This default implementation
works for MAJOR.MINOR-style version numbers as used by RCS
and CVS."
  (when (not (string= rev (vc-workfile-version file)))
    (let ((branch (vc-branch-part rev))
	  (minor-num (string-to-number (vc-minor-part rev))))
      (concat branch "." (number-to-string (1+ minor-num))))))

;; File property caching

(defun vc-clear-context ()
  "Clear all cached file properties."
  (interactive)
  (fillarray vc-file-prop-obarray 0))

(defmacro with-vc-properties (file form settings)
  "Execute FORM, then maybe set per-file properties for FILE.
SETTINGS is an association list of property/value pairs.  After
executing FORM, set those properties from SETTINGS that have not yet
been updated to their corresponding values."
  (declare (debug t))
  `(let ((vc-touched-properties (list t)))
     ,form
     (mapcar (lambda (setting)
	       (let ((property (car setting)))
		 (unless (memq property vc-touched-properties)
		   (put (intern ,file vc-file-prop-obarray)
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
  (declare (debug t) (indent 2))
  (let ((filevar (make-symbol "file")))
    `(let ((,filevar (expand-file-name ,file)))
       (or (vc-backend ,filevar)
           (error "File not under version control: `%s'" file))
       (unless (vc-editable-p ,filevar)
         (let ((state (vc-state ,filevar)))
           (if (stringp state)
               (error "`%s' is locking `%s'" state ,filevar)
             (vc-checkout ,filevar t))))
       (save-excursion
         ,@body)
       (vc-checkin ,filevar nil ,comment))))

;;;###autoload
(defmacro edit-vc-file (file comment &rest body)
  "Edit FILE under version control, executing body.
Checkin with COMMENT after executing BODY.
This macro uses `with-vc-file', passing args to it.
However, before executing BODY, find FILE, and after BODY, save buffer."
  (declare (debug t) (indent 2))
  (let ((filevar (make-symbol "file")))
    `(let ((,filevar (expand-file-name ,file)))
       (with-vc-file
        ,filevar ,comment
        (set-buffer (find-file-noselect ,filevar))
        ,@body
        (save-buffer)))))

(defun vc-ensure-vc-buffer ()
  "Make sure that the current buffer visits a version-controlled file."
  (if vc-dired-mode
      (set-buffer (find-file-noselect (dired-get-filename)))
    (while vc-parent-buffer
      (set-buffer vc-parent-buffer))
    (if (not buffer-file-name)
	(error "Buffer %s is not associated with a file" (buffer-name))
      (if (not (vc-backend buffer-file-name))
	  (error "File %s is not under version control" buffer-file-name)))))

(defun vc-process-filter (p s)
  "An alternative output filter for async process P.
One difference with the default filter is that this inserts S after markers.
Another is that undo information is not kept."
  (with-current-buffer (process-buffer p)
    (save-excursion
      (let ((buffer-undo-list t)
            (inhibit-read-only t))
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
    (let ((buffer-undo-list t)
          (inhibit-read-only t))
      (erase-buffer))))

(defun vc-exec-after (code)
  "Eval CODE when the current buffer's process is done.
If the current buffer has no process, just evaluate CODE.
Else, add CODE to the process' sentinel."
  (let ((proc (get-buffer-process (current-buffer))))
    (cond
     ;; If there's no background process, just execute the code.
     ;; We used to explicitly call delete-process on exited processes,
     ;; but this led to timing problems causing process output to be
     ;; lost.  Terminated processes get deleted automatically
     ;; anyway. -- cyd
     ((or (null proc) (eq (process-status proc) 'exit))
      (eval code))
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

(defvar w32-quote-process-args)
;;;###autoload
(defun vc-do-command (buffer okstatus command file &rest flags)
  "Execute a VC command, notifying user and checking for errors.
Output from COMMAND goes to BUFFER, or *vc* if BUFFER is nil or the
current buffer if BUFFER is t.  If the destination buffer is not
already current, set it up properly and erase it.  The command is
considered successful if its exit status does not exceed OKSTATUS (if
OKSTATUS is nil, that means to ignore error status, if it is `async', that
means not to wait for termination of the subprocess; if it is t it means to
ignore all execution errors).  FILE is the
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
    (let ((squeezed (remq nil flags))
	  (inhibit-read-only t)
	  (status 0))
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
	(if (and (eq okstatus 'async) (file-remote-p default-directory))
	    ;; start-process does not support remote execution
	    (setq okstatus nil))
	(if (eq okstatus 'async)
	    (let ((proc
		   (let ((process-connection-type nil))
		     (apply 'start-process command (current-buffer) command
			    squeezed))))
              (unless (active-minibuffer-window)
                (message "Running %s in the background..." command))
	      ;;(set-process-sentinel proc (lambda (p msg) (delete-process p)))
	      (set-process-filter proc 'vc-process-filter)
	      (vc-exec-after
	       `(unless (active-minibuffer-window)
                  (message "Running %s in the background... done" ',command))))
	  (let ((buffer-undo-list t))
            (setq status (apply 'process-file command nil t nil squeezed)))
	  (when (and (not (eq t okstatus))
                     (or (not (integerp status))
                         (and okstatus (< okstatus status))))
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
	;; The new compilation code does not use compilation-error-list any
	;; more, so the code below is now ineffective and might as well
	;; be disabled.  -- Stef
	;; ;; We may want to reparse the compilation buffer after revert
	;; (reparse (and (boundp 'compilation-error-list) ;compile loaded
	;; 	      ;; Construct a list; each elt is nil or a buffer
	;; 	      ;; if that buffer is a compilation output buffer
	;; 	      ;; that contains markers into the current buffer.
	;; 	      (save-current-buffer
	;; 		(mapcar (lambda (buffer)
	;; 			  (set-buffer buffer)
	;; 			  (let ((errors (or
	;; 					 compilation-old-error-list
	;; 					 compilation-error-list))
	;; 				(buffer-error-marked-p nil))
	;; 			    (while (and (consp errors)
	;; 					(not buffer-error-marked-p))
	;; 			      (and (markerp (cdr (car errors)))
	;; 				   (eq buffer
	;; 				       (marker-buffer
	;; 					(cdr (car errors))))
	;; 				   (setq buffer-error-marked-p t))
	;; 			      (setq errors (cdr errors)))
	;; 			    (if buffer-error-marked-p buffer)))
	;; 			(buffer-list)))))
	(reparse nil))
    (list point-context mark-context reparse)))

(defun vc-restore-buffer-context (context)
  "Restore point/mark, and reparse any affected compilation buffers.
CONTEXT is that which `vc-buffer-context' returns."
  (let ((point-context (nth 0 context))
	(mark-context (nth 1 context))
	(reparse (nth 2 context)))
    ;; The new compilation code does not use compilation-error-list any
    ;; more, so the code below is now ineffective and might as well
    ;; be disabled.  -- Stef
    ;; ;; Reparse affected compilation buffers.
    ;; (while reparse
    ;;   (if (car reparse)
    ;; 	  (with-current-buffer (car reparse)
    ;; 	    (let ((compilation-last-buffer (current-buffer)) ;select buffer
    ;; 		  ;; Record the position in the compilation buffer of
    ;; 		  ;; the last error next-error went to.
    ;; 		  (error-pos (marker-position
    ;; 			      (car (car-safe compilation-error-list)))))
    ;; 	      ;; Reparse the error messages as far as they were parsed before.
    ;; 	      (compile-reinitialize-errors '(4) compilation-parsing-end)
    ;; 	      ;; Move the pointer up to find the error we were at before
    ;; 	      ;; reparsing.  Now next-error should properly go to the next one.
    ;; 	      (while (and compilation-error-list
    ;; 			  (/= error-pos (car (car compilation-error-list))))
    ;; 		(setq compilation-error-list (cdr compilation-error-list))))))
    ;;   (setq reparse (cdr reparse)))

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

(defun vc-default-latest-on-branch-p (backend file)
  "Return non-nil if FILE is the latest on its branch.
This default implementation always returns non-nil, which means that
editing non-current versions is not supported by default."
  t)

(defun vc-next-action-on-file (file verbose &optional comment)
  "Do The Right Thing for a given FILE under version control.
If COMMENT is specified, it will be used as an admin or checkin comment.
If VERBOSE is non-nil, query the user rather than using default parameters."
  (let ((visited (get-file-buffer file))
	state version)
    (when visited
      (if vc-dired-mode
          (switch-to-buffer-other-window visited)
        (set-buffer visited))
      ;; Check relation of buffer and file, and make sure
      ;; user knows what he's doing.  First, finding the file
      ;; will check whether the file on disk is newer.
      ;; Ignore buffer-read-only during this test, and
      ;; preserve find-file-literally.
      (let ((buffer-read-only (not (file-writable-p file))))
        (find-file-noselect file nil find-file-literally))
      (if (not (verify-visited-file-modtime (current-buffer)))
	  (if (yes-or-no-p "Replace file on disk with buffer contents? ")
	      (write-file buffer-file-name)
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
	    (vc-checkout file (eq (vc-checkout-model file) 'implicit) t)
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
		(insert
		 (format "Changes to %s since last lock:\n\n" file)))
	      (not (beep))
	      (yes-or-no-p (concat "File has unlocked changes.  "
				   "Claim lock retaining changes? ")))
	    (progn (vc-call steal-lock file)
                   (clear-visited-file-modtime)
		   ;; Must clear any headers here because they wouldn't
		   ;; show that the file is locked now.
		   (vc-clear-headers file)
		   (write-file buffer-file-name)
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
  (let ((dired-buffer (current-buffer)))
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

(defun vc-default-init-version (backend) vc-default-init-version)

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
		    (vc-call-backend (vc-responsible-backend buffer-file-name)
				     'init-version))
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
    (message "Stealing lock on %s..." file)
    (with-vc-properties
     file
     (vc-call steal-lock file rev)
     `((vc-state . edited)))
    (vc-resynch-buffer file t t)
    (message "Stealing lock on %s...done" file)
    ;; Write mail after actually stealing, because if the stealing
    ;; goes wrong, we don't want to send any mail.
    (compose-mail owner (format "Stolen lock on %s" file-description))
    (setq default-directory (expand-file-name "~/"))
    (goto-char (point-max))
    (insert
     (format "I stole the lock on %s, " file-description)
     (current-time-string)
     ".\n")
    (message "Please explain why you stole the lock.  Type C-c C-c when done.")))

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

(defun vc-finish-logentry (&optional nocomment)
  "Complete the operation implied by the current log entry.
Use the contents of the current buffer as a check-in or registration
comment.  If the optional arg NOCOMMENT is non-nil, then don't check
the buffer contents as a comment."
  (interactive)
  ;; Check and record the comment, if any.
  (unless nocomment
    ;; Comment too long?
    (vc-call-backend (or (and vc-log-file (vc-backend vc-log-file))
			 (vc-responsible-backend default-directory))
		     'logentry-check)
    (run-hooks 'vc-logentry-check-hook))
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

(defun vc-default-revision-completion-table (backend file) nil)

(defun vc-version-diff (file rev1 rev2)
  "List the differences between FILE's versions REV1 and REV2.
If REV1 is empty or nil it means to use the current workfile version;
REV2 empty or nil means the current file contents.  FILE may also be
a directory, in that case, generate diffs between the corresponding
versions of all registered files in or below it."
  (interactive
   (let* ((file (expand-file-name
                 (read-file-name (if buffer-file-name
                                     "File or dir to diff (default visited file): "
                                   "File or dir to diff: ")
                                 default-directory buffer-file-name t)))
          (rev1-default nil) (rev2-default nil)
          (completion-table (vc-call revision-completion-table file)))
     ;; compute default versions based on the file state
     (cond
      ;; if it's a directory, don't supply any version default
      ((file-directory-p file)
       nil)
      ;; if the file is not up-to-date, use current version as older version
      ((not (vc-up-to-date-p file))
       (setq rev1-default (vc-workfile-version file)))
      ;; if the file is not locked, use last and previous version as default
      (t
       (setq rev1-default (vc-call previous-version file
                                   (vc-workfile-version file)))
       (if (string= rev1-default "") (setq rev1-default nil))
       (setq rev2-default (vc-workfile-version file))))
     ;; construct argument list
     (let* ((rev1-prompt (if rev1-default
			     (concat "Older version (default "
				     rev1-default "): ")
			   "Older version: "))
	    (rev2-prompt (concat "Newer version (default "
				 (or rev2-default "current source") "): "))
	    (rev1 (if completion-table
		      (completing-read rev1-prompt completion-table
                                       nil nil nil nil rev1-default)
		    (read-string rev1-prompt nil nil rev1-default)))
	    (rev2 (if completion-table
		      (completing-read rev2-prompt completion-table
                                       nil nil nil nil rev2-default)
		    (read-string rev2-prompt nil nil rev2-default))))
       (list file rev1 rev2))))
  (if (file-directory-p file)
      ;; recursive directory diff
      (progn
        (vc-setup-buffer "*vc-diff*")
	(if (string-equal rev1 "") (setq rev1 nil))
	(if (string-equal rev2 "") (setq rev2 nil))
        (let ((inhibit-read-only t))
          (insert "Diffs between "
                  (or rev1 "last version checked in")
                  " and "
                  (or rev2 "current workfile(s)")
                  ":\n\n"))
        (let ((dir (file-name-as-directory file)))
          (vc-call-backend (vc-responsible-backend dir)
                           'diff-tree dir rev1 rev2))
	(vc-exec-after `(let ((inhibit-read-only t))
			  (insert "\nEnd of diffs.\n"))))
    ;; Single file diff.  It is important that the vc-controlled buffer
    ;; is still current at this time, because any local settings in that
    ;; buffer should affect the diff command.
    (vc-diff-internal file rev1 rev2))
  (set-buffer "*vc-diff*")
  (if (and (zerop (buffer-size))
	   (not (get-buffer-process (current-buffer))))
      (progn
	(if rev1
	    (if rev2
		(message "No changes to %s between %s and %s" file rev1 rev2)
	      (message "No changes to %s since %s" file rev1))
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
                      (let ((win (get-buffer-window (current-buffer) t)))
                        (if win (shrink-window-if-larger-than-buffer win)))))
    t))

(defun vc-diff-label (file file-rev rev)
  (concat (file-relative-name file)
	  (format-time-string "\t%d %b %Y %T %z\t"
			      (nth 5 (file-attributes file-rev)))
	  rev))

(defun vc-diff-internal (file rev1 rev2)
  "Run diff to compare FILE's revisions REV1 and REV2.
Diff output goes to the *vc-diff* buffer.  The exit status of the diff
command is returned.

This function takes care to set up a proper coding system for diff output.
If both revisions are available as local files, then it also does not
actually call the backend, but performs a local diff."
  (if (or (not rev1) (string-equal rev1 ""))
      (setq rev1 (vc-workfile-version file)))
  (if (string-equal rev2 "")
      (setq rev2 nil))
  (let ((file-rev1 (vc-version-backup-file file rev1))
        (file-rev2 (if (not rev2)
                       file
                     (vc-version-backup-file file rev2)))
        (coding-system-for-read (vc-coding-system-for-diff file)))
    (if (and file-rev1 file-rev2)
        (let ((status
               (if (eq vc-diff-knows-L 'no)
                   (apply 'vc-do-command "*vc-diff*" 1 "diff" nil
                          (append (vc-switches nil 'diff)
                                  (list (file-relative-name file-rev1)
                                        (file-relative-name file-rev2))))
                 (apply 'vc-do-command "*vc-diff*" 2 "diff" nil
                        (append (vc-switches nil 'diff)
                                ;; Provide explicit labels like RCS or
                                ;; CVS would do so diff-mode refers to
                                ;; `file' rather than to `file-rev1'
                                ;; when trying to find/apply/undo
                                ;; hunks.
                                (list "-L" (vc-diff-label file file-rev1 rev1)
                                      "-L" (vc-diff-label file file-rev2 rev2)
                                      (file-relative-name file-rev1)
                                      (file-relative-name file-rev2)))))))
          (if (eq status 2)
              (if (not vc-diff-knows-L)
                  (setq vc-diff-knows-L 'no
                        status (apply 'vc-do-command "*vc-diff*" 1 "diff" nil
                                      (append 
                                       (vc-switches nil 'diff)
                                       (list (file-relative-name file-rev1)
                                             (file-relative-name file-rev2)))))
                (error "diff failed"))
            (if (not vc-diff-knows-L) (setq vc-diff-knows-L 'yes)))
          status)
      (vc-call diff file rev1 rev2))))

(defun vc-switches (backend op)
  (let ((switches
	 (or (if backend
		 (let ((sym (vc-make-backend-sym
			     backend (intern (concat (symbol-name op)
						     "-switches")))))
		   (if (boundp sym) (symbol-value sym))))
	     (let ((sym (intern (format "vc-%s-switches" (symbol-name op)))))
	       (if (boundp sym) (symbol-value sym)))
	     (cond
	      ((eq op 'diff) diff-switches)))))
    (if (stringp switches) (list switches)
      ;; If not a list, return nil.
      ;; This is so we can set vc-diff-switches to t to override
      ;; any switches in diff-switches.
      (if (listp switches) switches))))

;; Old def for compatibility with Emacs-21.[123].
(defmacro vc-diff-switches-list (backend) `(vc-switches ',backend 'diff))
(make-obsolete 'vc-diff-switches-list 'vc-switches "22.1")

(defun vc-default-diff-tree (backend dir rev1 rev2)
  "List differences for all registered files at and below DIR.
The meaning of REV1 and REV2 is the same as for `vc-version-diff'."
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
                          'diff ',f ',rev1 ',rev2))))))

(defun vc-coding-system-for-diff (file)
  "Return the coding system for reading diff output for FILE."
  (or coding-system-for-read
      ;; if we already have this file open,
      ;; use the buffer's coding system
      (let ((buf (find-buffer-visiting file)))
        (if buf (with-current-buffer buf
                  buffer-file-coding-system)))
      ;; otherwise, try to find one based on the file name
      (car (find-operation-coding-system 'insert-file-contents file))
      ;; and a final fallback
      'undecided))

;;;###autoload
(defun vc-version-other-window (rev)
  "Visit version REV of the current file in another window.
If the current file is named `F', the version is named `F.~REV~'.
If `F.~REV~' already exists, use it instead of checking it out again."
  (interactive
   (save-current-buffer
     (vc-ensure-vc-buffer)
     (let ((completion-table
            (vc-call revision-completion-table buffer-file-name))
           (prompt "Version to visit (default is workfile version): "))
       (list
        (if completion-table
            (completing-read prompt completion-table)
          (read-string prompt))))))
  (vc-ensure-vc-buffer)
  (let* ((file buffer-file-name)
	 (version (if (string-equal rev "")
		      (vc-workfile-version file)
		    rev)))
    (switch-to-buffer-other-window (vc-find-version file version))))

(defun vc-find-version (file version)
  "Read VERSION of FILE into a buffer and return the buffer."
  (let ((automatic-backup (vc-version-backup-file-name file version))
	(filebuf (or (get-file-buffer file) (current-buffer)))
        (filename (vc-version-backup-file-name file version 'manual)))
    (unless (file-exists-p filename)
      (if (file-exists-p automatic-backup)
          (rename-file automatic-backup filename nil)
	(message "Checking out %s..." filename)
	(with-current-buffer filebuf
	  (let ((failed t))
	    (unwind-protect
		(let ((coding-system-for-read 'no-conversion)
		      (coding-system-for-write 'no-conversion))
		  (with-temp-file filename
		    (let ((outbuf (current-buffer)))
		      ;; Change buffer to get local value of
		      ;; vc-checkout-switches.
		      (with-current-buffer filebuf
			(vc-call find-version file version outbuf))))
		  (setq failed nil))
	      (if (and failed (file-exists-p filename))
		  (delete-file filename))))
	  (vc-mode-line file))
	(message "Checking out %s...done" filename)))
    (find-file-noselect filename)))

(defun vc-default-find-version (backend file rev buffer)
  "Provide the new `find-version' op based on the old `checkout' op.
This is only for compatibility with old backends.  They should be updated
to provide the `find-version' operation instead."
  (let ((tmpfile (make-temp-file (expand-file-name file))))
    (unwind-protect
	(progn
	  (vc-call-backend backend 'checkout file nil rev tmpfile)
	  (with-current-buffer buffer
	    (insert-file-contents-literally tmpfile)))
      (delete-file tmpfile))))

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
          (let* ((delims (cdr (assq major-mode vc-comment-alist)))
                 (comment-start-vc (or (car delims) comment-start "#"))
                 (comment-end-vc (or (car (cdr delims)) comment-end ""))
                 (hdsym (vc-make-backend-sym (vc-backend buffer-file-name)
                                             'header))
                 (hdstrings (and (boundp hdsym) (symbol-value hdsym))))
            (dolist (s hdstrings)
              (insert comment-start-vc "\t" s "\t"
                      comment-end-vc "\n"))
            (if vc-static-header-alist
                (dolist (f vc-static-header-alist)
                  (if (string-match (car f) buffer-file-name)
                      (insert (format (cdr f) (car hdstrings)))))))))))

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
			       "(default news on current branch): ")))
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
    (smerge-mode 1)
    (message "File contains conflicts.")))

;;;###autoload
(defalias 'vc-resolve-conflicts 'smerge-ediff)

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
  (add-hook 'dired-after-readin-hook 'vc-dired-hook nil t)
  ;; The following is slightly modified from files.el,
  ;; because file lines look a bit different in vc-dired-mode
  ;; (the column before the date does not end in a digit).
  ;; albinus: It should be done in the original declaration.  Problem
  ;; is the optional empty state-info; otherwise ")" would be good
  ;; enough as delimeter.
  (set (make-local-variable 'directory-listing-before-filename-regexp)
  (let* ((l "\\([A-Za-z]\\|[^\0-\177]\\)")
         ;; In some locales, month abbreviations are as short as 2 letters,
         ;; and they can be followed by ".".
         (month (concat l l "+\\.?"))
         (s " ")
         (yyyy "[0-9][0-9][0-9][0-9]")
         (dd "[ 0-3][0-9]")
         (HH:MM "[ 0-2][0-9]:[0-5][0-9]")
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
         ;; Japanese MS-Windows ls-lisp has one-digit months, and
         ;; omits the Kanji characters after month and day-of-month.
         (mm "[ 0-1]?[0-9]")
         (japanese
          (concat mm l "?" s dd l "?" s "+"
                  "\\(" HH:MM "\\|" yyyy l "?" "\\)")))
    ;; the .* below ensures that we find the last match on a line
    (concat ".*" s
            "\\(" western "\\|" western-comma "\\|" japanese "\\|" iso "\\)"
            s "+")))
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
     ((eq state 'edited) (concat "(" (vc-user-login-name file) ")"))
     ((eq state 'needs-merge) "(merge)")
     ((eq state 'needs-patch) "(patch)")
     ((eq state 'unlocked-changes) "(stale)"))))

(defun vc-dired-reformat-line (vc-info)
  "Reformat a directory-listing line.
Replace various columns with version control information, VC-INFO.
This code, like dired, assumes UNIX -l format."
  (beginning-of-line)
  (when (re-search-forward
         ;; Match link count, owner, group, size.  Group may be missing,
         ;; and only the size is present in OS/2 -l format.
         "^..[drwxlts-]+ \\( *[0-9]+\\( [^ ]+ +\\([^ ]+ +\\)?[0-9]+\\)?\\) "
         (line-end-position) t)
      (replace-match (substring (concat vc-info "          ") 0 10)
                     t t nil 1)))

(defun vc-dired-hook ()
  "Reformat the listing according to version control.
Called by dired after any portion of a vc-dired buffer has been read in."
  (message "Getting version information... ")
  (let (subdir filename (buffer-read-only nil))
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
  (goto-char (point-min))
  (while (dired-get-subdir)
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
	(or (eobp) (kill-line))
	(if (not (dired-next-subdir 1 t))
	    (goto-char (point-max))))))
  (goto-char (point-min)))

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
    (if (eq (string-match tramp-file-name-regexp dir) 0)
        (error "Sorry, vc-directory does not work over Tramp"))
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
    (message "%s" msg)
    (vc-call-backend (vc-responsible-backend dir)
		     'retrieve-snapshot dir name update)
    (message "%s" (concat msg "done"))))

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
(defun vc-print-log (&optional focus-rev)
  "List the change log of the current buffer in a window.
If FOCUS-REV is non-nil, leave the point at that revision."
  (interactive)
  (vc-ensure-vc-buffer)
  (let ((file buffer-file-name))
    (or focus-rev (setq focus-rev (vc-workfile-version file)))
    ;; Don't switch to the output buffer before running the command,
    ;; so that any buffer-local settings in the vc-controlled
    ;; buffer can be accessed by the command.
    (condition-case err
        (progn
          (vc-call print-log file "*vc-change-log*")
          (set-buffer "*vc-change-log*"))
      (wrong-number-of-arguments
       ;; If this error came from the above call to print-log, try again
       ;; without the optional buffer argument (for backward compatibility).
       ;; Otherwise, resignal.
       (if (or (not (eq (cadr err)
                        (indirect-function
                         (vc-find-backend-function (vc-backend file)
                                                   'print-log))))
               (not (eq (caddr err) 2)))
           (signal (car err) (cdr err))
         ;; for backward compatibility
         (vc-call print-log file)
         (set-buffer "*vc*"))))
    (pop-to-buffer (current-buffer))
    (vc-exec-after
     `(let ((inhibit-read-only t))
    	(vc-call-backend ',(vc-backend file) 'log-view-mode)
	(goto-char (point-max)) (forward-line -1)
	(while (looking-at "=*\n")
	  (delete-char (- (match-end 0) (match-beginning 0)))
	  (forward-line -1))
	(goto-char (point-min))
	(if (looking-at "[\b\t\n\v\f\r ]+")
	    (delete-char (- (match-end 0) (match-beginning 0))))
	;; (shrink-window-if-larger-than-buffer)
	;; move point to the log entry for the current version
	(vc-call-backend ',(vc-backend file)
			 'show-log-entry
			 ',focus-rev)
        (set-buffer-modified-p nil)))))

(defun vc-default-log-view-mode (backend) (log-view-mode))
(defun vc-default-show-log-entry (backend rev)
  (with-no-warnings
   (log-view-goto-rev rev)))

(defun vc-default-comment-history (backend file)
  "Return a string with all log entries stored in BACKEND for FILE."
  (if (vc-find-backend-function backend 'print-log)
      (with-current-buffer "*vc*"
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
      (message "Finding changes...")
      ;; vc-diff selects the new window, which is not what we want:
      ;; if the new window is on another frame, that'd require the user
      ;; moving her mouse to answer the yes-or-no-p question.
      (let* ((vc-disable-async-diff (not vc-allow-async-revert))
             (win (save-selected-window
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

;;;###autoload
(defun vc-update ()
  "Update the current buffer's file to the latest version on its branch.
If the file contains no changes, and is not locked, then this simply replaces
the working file with the latest version on its branch.  If the file contains
changes, and the backend supports merging news, then any recent changes from
the current branch are merged into the working file."
  (interactive)
  (vc-ensure-vc-buffer)
  (vc-buffer-sync nil)
  (let ((file buffer-file-name))
    (if (vc-up-to-date-p file)
        (vc-checkout file nil "")
      (if (eq (vc-checkout-model file) 'locking)
          (if (eq (vc-state file) 'edited)
              (error
               (substitute-command-keys
           "File is locked--type \\[vc-revert-buffer] to discard changes"))
            (error
             (substitute-command-keys
           "Unexpected file state (%s)--type \\[vc-next-action] to correct")
                   (vc-state file)))
        (if (not (vc-find-backend-function (vc-backend file) 'merge-news))
            (error "Sorry, merging news is not implemented for %s"
                   (vc-backend file))
          (vc-call merge-news file)
          (vc-resynch-window file t t))))))

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

(defun vc-default-revert (backend file contents-done)
  (unless contents-done
    (let ((rev (vc-workfile-version file))
          (file-buffer (or (get-file-buffer file) (current-buffer))))
      (message "Checking out %s..." file)
      (let ((failed t)
            (backup-name (car (find-backup-file-name file))))
        (when backup-name
          (copy-file file backup-name 'ok-if-already-exists 'keep-date)
          (unless (file-writable-p file)
            (set-file-modes file (logior (file-modes file) 128))))
        (unwind-protect
            (let ((coding-system-for-read 'no-conversion)
                  (coding-system-for-write 'no-conversion))
              (with-temp-file file
                (let ((outbuf (current-buffer)))
                  ;; Change buffer to get local value of vc-checkout-switches.
                  (with-current-buffer file-buffer
                    (let ((default-directory (file-name-directory file)))
                      (vc-call find-version file rev outbuf)))))
              (setq failed nil))
          (when backup-name
            (if failed
                (rename-file backup-name file 'ok-if-already-exists)
              (and (not vc-make-backup-files) (delete-file backup-name))))))
      (message "Checking out %s...done" file))))

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
  (let* ((file buffer-file-name)
	 (backend (vc-backend file))
         (target (vc-workfile-version file)))
    (cond
     ((not (vc-find-backend-function backend 'cancel-version))
      (error "Sorry, canceling versions is not supported under %s" backend))
     ((not (vc-call latest-on-branch-p file))
      (error "This is not the latest version; VC cannot cancel it"))
     ((not (vc-up-to-date-p file))
      (error "%s" (substitute-command-keys "File is not up to date; use \\[vc-revert-buffer] to discard changes"))))
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
    (or buffer-file-name
        (error "There is no version-controlled file in this buffer"))
    (let ((backend (vc-backend buffer-file-name))
	  (backends nil))
      (unwind-protect
	  (progn
	    (unless backend
	      (error "File %s is not under version control" buffer-file-name))
	    ;; Find the registered backends.
	    (dolist (backend vc-handled-backends)
	      (when (vc-call-backend backend 'registered buffer-file-name)
		(push backend backends)))
	    ;; Find the next backend.
	    (let ((def (car (delq backend
				  (append (memq backend backends) backends))))
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
	       (t def))))
	;; Calling the `registered' method can mess up the file
	;; properties, so we want to revert them to what they were.
	(if (and backend (delete backend backends))
	    (vc-call-backend backend 'registered buffer-file-name))))))
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
	     (modified-file (and edited (make-temp-file file)))
	     (unmodified-file (and modified-file (vc-version-backup-file file))))
	;; Go back to the base unmodified file.
	(unwind-protect
	    (progn
	      (when modified-file
		(copy-file file modified-file 'ok-if-already-exists)
		;; If we have a local copy of the unmodified file, handle that
		;; here and not in vc-revert-file because we don't want to
		;; delete that copy -- it is still useful for OLD-BACKEND.
		(if unmodified-file
		    (copy-file unmodified-file file
			       'ok-if-already-exists 'keep-date)
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
       (dolist (f masters)
	 (if (and f (string= (file-name-directory (expand-file-name f)) dir))
	     (throw 'found f)))
       ;; If not, just use the first possible place.
       (dolist (f masters)
	 (and f (or (not (setq dir (file-name-directory f)))
		    (file-directory-p dir))
	      (throw 'found f)))
       (error "New file lacks a version control directory")))))

(defun vc-delete-file (file)
  "Delete file and mark it as such in the version control system."
  (interactive "fVC delete file: ")
  (let ((buf (get-file-buffer file))
        (backend (vc-backend file)))
    (unless backend
      (error "File %s is not under version control"
             (file-name-nondirectory file)))
    (unless (vc-find-backend-function backend 'delete-file)
      (error "Deleting files under %s is not supported in VC" backend))
    (if (and buf (buffer-modified-p buf))
	(error "Please save files before deleting them"))
    (unless (y-or-n-p (format "Really want to delete %s? "
			      (file-name-nondirectory file)))
      (error "Abort!"))
    (unless (or (file-directory-p file) (null make-backup-files))
      (with-current-buffer (or buf (find-file-noselect file))
	(let ((backup-inhibited nil))
	  (backup-buffer))))
    (vc-call delete-file file)
    ;; If the backend hasn't deleted the file itself, let's do it for him.
    (if (file-exists-p file) (delete-file file))))

(defun vc-default-rename-file (backend old new)
  (condition-case nil
      (add-name-to-file old new)
    (error (rename-file old new)))
  (vc-delete-file old)
  (with-current-buffer (find-file-noselect new)
    (vc-register)))

;;;###autoload
(defun vc-rename-file (old new)
  "Rename file OLD to NEW, and rename its master file likewise."
  (interactive "fVC rename file: \nFRename to: ")
  (let ((oldbuf (get-file-buffer old)))
    (if (and oldbuf (buffer-modified-p oldbuf))
	(error "Please save files before moving them"))
    (if (get-file-buffer new)
	(error "Already editing new file name"))
    (if (file-exists-p new)
	(error "New file already exists"))
    (let ((state (vc-state old)))
      (unless (memq state '(up-to-date edited))
	(error "Please %s files before moving them"
	       (if (stringp state) "check in" "update"))))
    (vc-call rename-file old new)
    (vc-file-clearprops old)
    ;; Move the actual file (unless the backend did it already)
    (if (file-exists-p old) (rename-file old new))
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
  (dolist (file (or args (list default-directory)))
    (if (eq (string-match tramp-file-name-regexp file) 0)
        (error "Sorry, vc-update-change-log does not work over Tramp")))
  (vc-call-backend (vc-responsible-backend default-directory)
                   'update-changelog args))

(defun vc-default-update-changelog (backend files)
  "Default implementation of update-changelog.
Uses `rcs2log' which only works for RCS and CVS."
  ;; FIXME: We (c|sh)ould add support for cvs2cl
  (let ((odefault default-directory)
	(changelog (find-change-log))
	;; Presumably not portable to non-Unixy systems, along with rcs2log:
	(tempfile (make-temp-file
		   (expand-file-name "vc"
				     (or small-temporary-file-directory
					 temporary-file-directory))))
        (login-name (or user-login-name
                        (format "uid%d" (number-to-string (user-uid)))))
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
                                    "-u" (concat login-name
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
		     (pop-to-buffer (get-buffer-create "*vc*"))
		     (erase-buffer)
		     (insert-file-contents tempfile)
		     "failed"))
	       (setq default-directory (file-name-directory changelog))
	       (delete-file tempfile)))))

;; Annotate functionality

;; Declare globally instead of additional parameter to
;; temp-buffer-show-function (not possible to pass more than one
;; parameter).  The use of annotate-ratio is deprecated in favor of
;; annotate-mode, which replaces it with the more sensible "span-to
;; days", along with autoscaling support.
(defvar vc-annotate-ratio nil "Global variable.")

;; internal buffer-local variables
(defvar vc-annotate-backend nil)
(defvar vc-annotate-parent-file nil)
(defvar vc-annotate-parent-rev nil)
(defvar vc-annotate-parent-display-mode nil)

(defconst vc-annotate-font-lock-keywords
  ;; The fontification is done by vc-annotate-lines instead of font-lock.
  '((vc-annotate-lines)))

(define-derived-mode vc-annotate-mode fundamental-mode "Annotate"
  "Major mode for output buffers of the `vc-annotate' command.

You can use the mode-specific menu to alter the time-span of the used
colors.  See variable `vc-annotate-menu-elements' for customizing the
menu items."
  (set (make-local-variable 'truncate-lines) t)
  (set (make-local-variable 'font-lock-defaults)
       '(vc-annotate-font-lock-keywords t))
  (view-mode 1))

(defun vc-annotate-display-default (ratio)
  "Display the output of \\[vc-annotate] using the default color range.
The color range is given by `vc-annotate-color-map', scaled by RATIO.
The current time is used as the offset."
  (interactive (progn (kill-local-variable 'vc-annotate-color-map) '(1.0)))
  (message "Redisplaying annotation...")
  (vc-annotate-display ratio)
  (message "Redisplaying annotation...done"))

(defun vc-annotate-oldest-in-map (color-map)
  "Return the oldest time in the COLOR-MAP."
  ;; Since entries should be sorted, we can just use the last one.
  (caar (last color-map)))

(defun vc-annotate-display-autoscale (&optional full)
  "Highlight the output of \\[vc-annotate] using an autoscaled color map.
Autoscaling means that the map is scaled from the current time to the
oldest annotation in the buffer, or, with prefix argument FULL, to
cover the range from the oldest annotation to the newest."
  (interactive "P")
  (let ((newest 0.0)
	(oldest 999999.)		;Any CVS users at the founding of Rome?
	(current (vc-annotate-convert-time (current-time)))
	date)
    (message "Redisplaying annotation...")
    ;; Run through this file and find the oldest and newest dates annotated.
    (save-excursion
      (goto-char (point-min))
      (while (setq date (prog1 (vc-call-backend vc-annotate-backend
                                                'annotate-time)
                          (forward-line 1)))
	(if (> date newest)
	    (setq newest date))
	(if (< date oldest)
	    (setq oldest date))))
    (vc-annotate-display
     (/ (- (if full newest current) oldest)
        (vc-annotate-oldest-in-map vc-annotate-color-map))
     (if full newest))
    (message "Redisplaying annotation...done \(%s\)"
	     (if full
		 (format "Spanned from %.1f to %.1f days old"
			 (- current oldest)
			 (- current newest))
	       (format "Spanned to %.1f days old" (- current oldest))))))

;; Menu -- Using easymenu.el
(easy-menu-define vc-annotate-mode-menu vc-annotate-mode-map
  "VC Annotate Display Menu"
  `("VC-Annotate"
    ["By Color Map Range" (unless (null vc-annotate-display-mode)
                 (setq vc-annotate-display-mode nil)
                 (vc-annotate-display-select))
     :style toggle :selected (null vc-annotate-display-mode)]
    ,@(let ((oldest-in-map (vc-annotate-oldest-in-map vc-annotate-color-map)))
        (mapcar (lambda (element)
                  (let ((days (* element oldest-in-map)))
                    `[,(format "Span %.1f days" days)
                      (vc-annotate-display-select nil ,days)
                      :style toggle :selected
                      (eql vc-annotate-display-mode ,days) ]))
                vc-annotate-menu-elements))
    ["Span ..."
     (vc-annotate-display-select
      nil (float (string-to-number (read-string "Span how many days? "))))]
    "--"
    ["Span to Oldest"
     (unless (eq vc-annotate-display-mode 'scale)
       (vc-annotate-display-select nil 'scale))
     :style toggle :selected
     (eq vc-annotate-display-mode 'scale)]
    ["Span Oldest->Newest"
     (unless (eq vc-annotate-display-mode 'fullscale)
       (vc-annotate-display-select nil 'fullscale))
     :style toggle :selected
     (eq vc-annotate-display-mode 'fullscale)]
    "--"
    ["Annotate previous revision" vc-annotate-prev-version]
    ["Annotate next revision" vc-annotate-next-version]
    ["Annotate revision at line" vc-annotate-revision-at-line]
    ["Annotate revision previous to line" vc-annotate-revision-previous-to-line]
    ["Annotate latest revision" vc-annotate-workfile-version]
    ["Show log of revision at line" vc-annotate-show-log-revision-at-line]
    ["Show diff of revision at line" vc-annotate-show-diff-revision-at-line]))

(defun vc-annotate-display-select (&optional buffer mode)
  "Highlight the output of \\[vc-annotate].
By default, the current buffer is highlighted, unless overridden by
BUFFER.  `vc-annotate-display-mode' specifies the highlighting mode to
use; you may override this using the second optional arg MODE."
  (interactive)
  (if mode (setq vc-annotate-display-mode mode))
  (pop-to-buffer (or buffer (current-buffer)))
  (cond ((null vc-annotate-display-mode)
         ;; The ratio is global, thus relative to the global color-map.
         (kill-local-variable 'vc-annotate-color-map)
	 (vc-annotate-display-default (or vc-annotate-ratio 1.0)))
        ;; One of the auto-scaling modes
	((eq vc-annotate-display-mode 'scale)
	 (vc-annotate-display-autoscale))
	((eq vc-annotate-display-mode 'fullscale)
	 (vc-annotate-display-autoscale t))
	((numberp vc-annotate-display-mode) ; A fixed number of days lookback
	 (vc-annotate-display-default
	  (/ vc-annotate-display-mode
             (vc-annotate-oldest-in-map vc-annotate-color-map))))
	(t (error "No such display mode: %s"
		  vc-annotate-display-mode))))

;;;###autoload
(defun vc-annotate (file rev &optional display-mode buf)
  "Display the edit history of the current file using colors.

This command creates a buffer that shows, for each line of the current
file, when it was last edited and by whom.  Additionally, colors are
used to show the age of each line--blue means oldest, red means
youngest, and intermediate colors indicate intermediate ages.  By
default, the time scale stretches back one year into the past;
everything that is older than that is shown in blue.

With a prefix argument, this command asks two questions in the
minibuffer.  First, you may enter a version number; then the buffer
displays and annotates that version instead of the current version
\(type RET in the minibuffer to leave that default unchanged).  Then,
you are prompted for the time span in days which the color range
should cover.  For example, a time span of 20 days means that changes
over the past 20 days are shown in red to blue, according to their
age, and everything that is older than that is shown in blue.

Customization variables:

`vc-annotate-menu-elements' customizes the menu elements of the
mode-specific menu. `vc-annotate-color-map' and
`vc-annotate-very-old-color' defines the mapping of time to
colors. `vc-annotate-background' specifies the background color."
  (interactive
   (save-current-buffer
     (vc-ensure-vc-buffer)
     (list buffer-file-name
	   (let ((def (vc-workfile-version buffer-file-name)))
	     (if (null current-prefix-arg) def
	       (read-string
		(format "Annotate from version (default %s): " def)
		nil nil def)))
	   (if (null current-prefix-arg)
	       vc-annotate-display-mode
	     (float (string-to-number
		     (read-string "Annotate span days (default 20): "
				  nil nil "20")))))))
  (vc-ensure-vc-buffer)
  (setq vc-annotate-display-mode display-mode) ;Not sure why.  --Stef
  (let* ((temp-buffer-name (format "*Annotate %s (rev %s)*" (buffer-name) rev))
         (temp-buffer-show-function 'vc-annotate-display-select)
         ;; If BUF is specified, we presume the caller maintains current line,
         ;; so we don't need to do it here.  This implementation may give
         ;; strange results occasionally in the case of REV != WORKFILE-REV.
         (current-line (unless buf (line-number-at-pos))))
    (message "Annotating...")
    ;; If BUF is specified it tells in which buffer we should put the
    ;; annotations.  This is used when switching annotations to another
    ;; revision, so we should update the buffer's name.
    (if buf (with-current-buffer buf
	      (rename-buffer temp-buffer-name t)
	      ;; In case it had to be uniquified.
	      (setq temp-buffer-name (buffer-name))))
    (with-output-to-temp-buffer temp-buffer-name
      (vc-call annotate-command file (get-buffer temp-buffer-name) rev)
      ;; we must setup the mode first, and then set our local
      ;; variables before the show-function is called at the exit of
      ;; with-output-to-temp-buffer
      (with-current-buffer temp-buffer-name
        (if (not (equal major-mode 'vc-annotate-mode))
            (vc-annotate-mode))
        (set (make-local-variable 'vc-annotate-backend) (vc-backend file))
        (set (make-local-variable 'vc-annotate-parent-file) file)
        (set (make-local-variable 'vc-annotate-parent-rev) rev)
        (set (make-local-variable 'vc-annotate-parent-display-mode)
             display-mode)))
    (when current-line
      (goto-line current-line temp-buffer-name))
    (message "Annotating... done")))

(defun vc-annotate-prev-version (prefix)
  "Visit the annotation of the version previous to this one.

With a numeric prefix argument, annotate the version that many
versions previous."
  (interactive "p")
  (vc-annotate-warp-version (- 0 prefix)))

(defun vc-annotate-next-version (prefix)
  "Visit the annotation of the version after this one.

With a numeric prefix argument, annotate the version that many
versions after."
  (interactive "p")
  (vc-annotate-warp-version prefix))

(defun vc-annotate-workfile-version ()
  "Visit the annotation of the workfile version of this file."
  (interactive)
  (if (not (equal major-mode 'vc-annotate-mode))
      (message "Cannot be invoked outside of a vc annotate buffer")
    (let ((warp-rev (vc-workfile-version vc-annotate-parent-file)))
      (if (equal warp-rev vc-annotate-parent-rev)
	  (message "Already at version %s" warp-rev)
	(vc-annotate-warp-version warp-rev)))))

(defun vc-annotate-extract-revision-at-line ()
  "Extract the revision number of the current line."
  ;; This function must be invoked from a buffer in vc-annotate-mode
  (vc-call-backend vc-annotate-backend 'annotate-extract-revision-at-line))

(defun vc-annotate-revision-at-line ()
  "Visit the annotation of the version identified in the current line."
  (interactive)
  (if (not (equal major-mode 'vc-annotate-mode))
      (message "Cannot be invoked outside of a vc annotate buffer")
    (let ((rev-at-line (vc-annotate-extract-revision-at-line)))
      (if (not rev-at-line)
	  (message "Cannot extract revision number from the current line")
	(if (equal rev-at-line vc-annotate-parent-rev)
	    (message "Already at version %s" rev-at-line)
	  (vc-annotate-warp-version rev-at-line))))))

(defun vc-annotate-revision-previous-to-line ()
  "Visit the annotation of the version before the version at line."
  (interactive)
  (if (not (equal major-mode 'vc-annotate-mode))
      (message "Cannot be invoked outside of a vc annotate buffer")
    (let ((rev-at-line (vc-annotate-extract-revision-at-line))
	  (prev-rev nil))
      (if (not rev-at-line)
	  (message "Cannot extract revision number from the current line")
	(setq prev-rev
	      (vc-call previous-version vc-annotate-parent-file rev-at-line))
	(vc-annotate-warp-version prev-rev)))))

(defun vc-annotate-show-log-revision-at-line ()
  "Visit the log of the version at line."
  (interactive)
  (if (not (equal major-mode 'vc-annotate-mode))
      (message "Cannot be invoked outside of a vc annotate buffer")
    (let ((rev-at-line (vc-annotate-extract-revision-at-line)))
      (if (not rev-at-line)
	  (message "Cannot extract revision number from the current line")
	(vc-print-log rev-at-line)))))

(defun vc-annotate-show-diff-revision-at-line ()
  "Visit the diff of the version at line from its previous version."
  (interactive)
  (if (not (equal major-mode 'vc-annotate-mode))
      (message "Cannot be invoked outside of a vc annotate buffer")
    (let ((rev-at-line (vc-annotate-extract-revision-at-line))
	  (prev-rev nil))
      (if (not rev-at-line)
	  (message "Cannot extract revision number from the current line")
	(setq prev-rev
	      (vc-call previous-version vc-annotate-parent-file rev-at-line))
	(if (not prev-rev)
	    (message "Cannot diff from any version prior to %s" rev-at-line)
	  (save-window-excursion
	    (vc-version-diff vc-annotate-parent-file prev-rev rev-at-line))
	  (switch-to-buffer "*vc-diff*"))))))

(defun vc-annotate-warp-version (revspec)
  "Annotate the version described by REVSPEC.

If REVSPEC is a positive integer, warp that many versions
forward, if possible, otherwise echo a warning message.  If
REVSPEC is a negative integer, warp that many versions backward,
if possible, otherwise echo a warning message.  If REVSPEC is a
string, then it describes a revision number, so warp to that
revision."
  (if (not (equal major-mode 'vc-annotate-mode))
      (message "Cannot be invoked outside of a vc annotate buffer")
    (let* ((buf (current-buffer))
	   (oldline (line-number-at-pos))
	   (revspeccopy revspec)
	   (newrev nil))
      (cond
       ((and (integerp revspec) (> revspec 0))
	(setq newrev vc-annotate-parent-rev)
	(while (and (> revspec 0) newrev)
	       (setq newrev (vc-call next-version
				     vc-annotate-parent-file newrev))
	       (setq revspec (1- revspec)))
	(if (not newrev)
	    (message "Cannot increment %d versions from version %s"
		     revspeccopy vc-annotate-parent-rev)))
       ((and (integerp revspec) (< revspec 0))
	(setq newrev vc-annotate-parent-rev)
	(while (and (< revspec 0) newrev)
	       (setq newrev (vc-call previous-version
				     vc-annotate-parent-file newrev))
	       (setq revspec (1+ revspec)))
	(if (not newrev)
	    (message "Cannot decrement %d versions from version %s"
		     (- 0 revspeccopy) vc-annotate-parent-rev)))
       ((stringp revspec) (setq newrev revspec))
       (t (error "Invalid argument to vc-annotate-warp-version")))
      (when newrev
	(vc-annotate vc-annotate-parent-file newrev
                     vc-annotate-parent-display-mode
                     buf)
	(goto-line (min oldline (progn (goto-char (point-max))
				       (forward-line -1)
				       (line-number-at-pos))) buf)))))

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

(defun vc-annotate-convert-time (time)
  "Convert a time value to a floating-point number of days.
The argument TIME is a list as returned by `current-time' or
`encode-time', only the first two elements of that list are considered."
  (/ (+ (* (float (car time)) (lsh 1 16)) (cadr time)) 24 3600))

(defun vc-annotate-difference (&optional offset)
  "Return the time span in days to the next annotation.
This calls the backend function annotate-time, and returns the
difference in days between the time returned and the current time,
or OFFSET if present."
   (let ((next-time (vc-call-backend vc-annotate-backend 'annotate-time)))
     (if next-time
	 (- (or offset
		(vc-call-backend vc-annotate-backend 'annotate-current-time))
	    next-time))))

(defun vc-default-annotate-current-time (backend)
  "Return the current time, encoded as fractional days."
  (vc-annotate-convert-time (current-time)))

(defvar vc-annotate-offset nil)

(defun vc-annotate-display (ratio &optional offset)
  "Highlight `vc-annotate' output in the current buffer.
RATIO, is the expansion that should be applied to `vc-annotate-color-map'.
The annotations are relative to the current time, unless overridden by OFFSET."
  (if (/= ratio 1.0)
      (set (make-local-variable 'vc-annotate-color-map)
           (mapcar (lambda (elem) (cons (* (car elem) ratio) (cdr elem)))
                   vc-annotate-color-map)))
  (set (make-local-variable 'vc-annotate-offset) offset)
  (font-lock-mode 1))

(defun vc-annotate-lines (limit)
  (let (difference)
    (while (and (< (point) limit)
		(setq difference (vc-annotate-difference vc-annotate-offset)))
      (let* ((color (or (vc-annotate-compcar difference vc-annotate-color-map)
			(cons nil vc-annotate-very-old-color)))
	     ;; substring from index 1 to remove any leading `#' in the name
	     (face-name (concat "vc-annotate-face-"
				(if (string-equal
				     (substring (cdr color) 0 1) "#")
				    (substring (cdr color) 1)
				  (cdr color))))
	     ;; Make the face if not done.
	     (face (or (intern-soft face-name)
		       (let ((tmp-face (make-face (intern face-name))))
			 (set-face-foreground tmp-face (cdr color))
			 (if vc-annotate-background
			     (set-face-background tmp-face
						  vc-annotate-background))
			 tmp-face)))	; Return the face
	     (point (point)))
	(forward-line 1)
	(put-text-property point (point) 'face face)))
    ;; Pretend to font-lock there were no matches.
    nil))

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

(defun vc-log-edit (file)
  "Set up `log-edit' for use with VC on FILE."
  (setq default-directory
	(if file (file-name-directory file)
	  (with-current-buffer vc-parent-buffer default-directory)))
  (log-edit 'vc-finish-logentry nil
	    (if file `(lambda () ',(list (file-name-nondirectory file)))
	      ;; If FILE is nil, we were called from vc-dired.
	      (lambda ()
		(with-current-buffer vc-parent-buffer
		  (dired-get-marked-files t)))))
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

;; arch-tag: ca82c1de-3091-4e26-af92-460abc6213a6
;;; vc.el ends here
