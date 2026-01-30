;;; vc.el --- drive a version-control system from within Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 1992-1998, 2000-2026 Free Software Foundation, Inc.

;; Author: FSF (see below for full credits)
;; Maintainer: emacs-devel@gnu.org
;; Keywords: vc tools

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

;;; Credits:

;; VC was initially designed and implemented by Eric S. Raymond
;; <esr@thyrsus.com> in 1992.  Over the years, many other people have
;; contributed substantial amounts of work to VC.  These include:
;;
;;   Per Cederqvist <ceder@lysator.liu.se>
;;   Paul Eggert <eggert@twinsun.com>
;;   Dmitry Gutov <dmitry@gutov.dev>
;;   Sebastian Kremer <sk@thp.uni-koeln.de>
;;   Juri Linkov <juri@linkov.net>
;;   Martin Lorentzson <martinl@gnu.org>
;;   Dave Love <fx@gnu.org>
;;   Stefan Monnier <monnier@cs.yale.edu>
;;   Thien-Thi Nguyen <ttn@gnu.org>
;;   Dan Nicolaescu <dann@ics.uci.edu>
;;   J.D. Smith <jdsmith@alum.mit.edu>
;;   Andre Spiegel <spiegel@gnu.org>
;;   Richard Stallman <rms@gnu.org>
;;   Sean Whitton <spwhitton@spwhitton.name>
;;
;; In July 2007 ESR returned and redesigned the mode to cope better
;; with modern version-control systems that do commits by fileset
;; rather than per individual file.
;;
;; If you maintain a client of the mode or customize it in your .emacs,
;; note that some backend functions which formerly took single file arguments
;; now take a list of files.  These include: register, checkin, print-log,
;; and diff.

;;; Commentary:

;; This mode is fully documented in the Emacs user's manual.
;;
;; Supported version-control systems presently include CVS, RCS, SRC,
;; GNU Subversion, Bzr, Git, Mercurial, Monotone and SCCS (or its free
;; replacement, CSSC).
;;
;; If your site uses the ChangeLog convention supported by Emacs, the
;; function `log-edit-comment-to-change-log' could prove a useful checkin hook,
;; although you might prefer to use C-c C-a (i.e. `log-edit-insert-changelog')
;; from the commit buffer instead or to set `log-edit-setup-invert'.
;;
;; When using SCCS, RCS, CVS: be careful not to do repo surgery, or
;; operations like registrations and deletions and renames, outside VC
;; while VC is running.  The support for these systems was designed
;; when disks were much slower, and the code maintains a lot of
;; internal state in order to reduce expensive operations to a
;; minimum.  Thus, if you mess with the repo while VC's back is turned,
;; VC may get seriously confused.
;;
;; When using Subversion or a later system, anything you do outside VC
;; *through the VCS tools* should safely interlock with VC
;; operations.  Under these VC does little state caching, because local
;; operations are assumed to be fast.
;;
;; The 'assumed to be fast' category includes SRC, even though it's
;; a wrapper around RCS.
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
;; `vc-sys-working-revision' should compute the working revision and
;; return it; it should not look it up in the property, and it needn't
;; store it there either.  However, if a backend-specific function does
;; store a value in a property, that value takes precedence over any
;; value that the generic code might want to set (check for uses of
;; the macro `with-vc-properties' in vc.el).
;;
;; In the list of functions below, each identifier needs to be prepended
;; with `vc-sys-'.  Some of the functions are mandatory (marked with a
;; `*'), others are optional (`-').

;; BACKEND PROPERTIES
;;
;; * revision-granularity
;;
;;   Takes no arguments.  Returns either 'file or 'repository.  Backends
;;   that return 'file have per-file revision numbering; backends
;;   that return 'repository have per-repository revision numbering,
;;   so a revision level implicitly identifies a changeset
;;
;; - update-on-retrieve-tag
;;
;;   Takes no arguments.  Backends that return non-nil can update
;;   buffers on `vc-retrieve-tag' based on user input.  In this case
;;   user will be prompted to update buffers on `vc-retrieve-tag'.
;;
;; - async-checkins
;;
;;   Takes no arguments.  Backends that return non-nil can (and do)
;;   perform async checkins when `vc-async-checkin' is non-nil.
;;
;; - working-revision-symbol
;;
;;   Symbolic name for the/a working revision, a constant string.  If
;;   defined, backend API functions that take revision numbers, revision
;;   hashes or branch names can also take this string in place of those.
;;   Emacs passes this name without first having to look up the working
;;   revision, which is a small performance improvement.
;;   In addition, using a name instead of a number or hash makes it
;;   easier to edit backend commands with `vc-edit-next-command'.

;; STATE-QUERYING FUNCTIONS
;;
;; * registered (file)
;;
;;   Return non-nil if FILE is registered in this backend.  Both this
;;   function as well as `state' should be careful to fail gracefully
;;   in the event that the backend executable is absent.  It is
;;   preferable that this function's *body* is autoloaded, that way only
;;   calling vc-registered does not cause the backend to be loaded
;;   (all the vc-FOO-registered functions are called to try to find
;;   the controlling backend for FILE).
;;
;; * state (file)
;;
;;   Return the current version control state of FILE.  For a list of
;;   possible values, see `vc-state'.  This function should do a full and
;;   reliable state computation; it is usually called immediately after
;;   C-x v v.
;;
;; - dir-status-files (dir files update-function)
;;
;;   Produce RESULT: a list of lists of the form (FILE VC-STATE EXTRA)
;;   for FILES in DIR.  If FILES is nil, report on all files in DIR.
;;   (It is OK, though possibly inefficient, to ignore the FILES argument
;;   and always report on all files in DIR.)
;;
;;   If FILES is non-nil, this function should report on all requested
;;   files, including up-to-date or ignored files.
;;
;;   EXTRA can be used for backend specific information about FILE.
;;
;;   If a command needs to be run to compute this list, it should be
;;   run asynchronously using (current-buffer) as the buffer for the
;;   command.
;;
;;   When RESULT is computed, it should be passed back by doing:
;;   (funcall UPDATE-FUNCTION RESULT nil).  If the backend uses a
;;   process filter, hence it produces partial results, they can be
;;   passed back by doing: (funcall UPDATE-FUNCTION RESULT t) and then
;;   do a (funcall UPDATE-FUNCTION RESULT nil) when all the results
;;   have been computed.
;;
;;   To provide more backend specific functionality for `vc-dir'
;;   the following functions might be needed: `dir-extra-headers',
;;   `dir-printer', and `extra-dir-menu'.
;;
;;   NOTE: project.el includes a similar method `project-list-files'
;;   that has a slightly different return value and performance
;;   trade-offs.  If you want to use it in your code and it suits your
;;   needs better than `dir-status-files', consider contacting the
;;   development list about changes or having it promoted to the core
;;   VC.  See also `vc-dir-status-files'.
;;
;; - dir-extra-headers (dir)
;;
;;   Return a string that will be added to the *vc-dir* buffer header.
;;
;; - dir-printer (fileinfo)
;;
;;   Pretty print the `vc-dir-fileinfo' FILEINFO.
;;   If a backend needs to show more information than the default FILE
;;   and STATE in the vc-dir listing, it can store that extra
;;   information in `vc-dir-fileinfo->extra'.  This function can be
;;   used to display that extra information in the *vc-dir* buffer.
;;
;; - status-fileinfo-extra (file)
;;
;;   Compute `vc-dir-fileinfo->extra' for FILE.
;;
;; * working-revision (file)
;;
;;   Return the working revision of FILE.  This is the revision fetched
;;   by the last checkout or update, not necessarily the same thing as the
;;   head or tip revision.  Should return "0" for a file added but not yet
;;   committed.
;;
;; * checkout-model (files)
;;
;;   Indicate whether FILES need to be "checked out" before they can be
;;   edited.  See `vc-checkout-model' for a list of possible values.
;;
;; - mode-line-string (file)
;;
;;   If provided, this function should return the VC-specific mode
;;   line string for FILE.  The returned string should have a
;;   `help-echo' property which is the text to be displayed as a
;;   tooltip when the mouse hovers over the VC entry on the mode-line.
;;   The default implementation deals well with all states that
;;   `vc-state' can return.
;;
;; - known-other-working-trees ()
;;
;;   Return a list of all other working trees known to use the same
;;   backing repository as this working tree.  The members of the list
;;   are the abbreviated (with `abbreviate-file-name') absolute file
;;   names of the root directories of the other working trees.
;;   For some VCS, the known working trees will not be all the other
;;   working trees, because other working trees can share the same
;;   backing repository in a way that's transparent to the original
;;   working tree (Mercurial is like this).
;;
;; STATE-CHANGING FUNCTIONS
;;
;; * create-repo ()
;;
;;   Create an empty repository in the current directory and initialize
;;   it so VC mode can add files to it.  For file-oriented systems, this
;;   need do no more than create a subdirectory with the right name.
;;
;; * register (files &optional comment)
;;
;;   Register FILES in this backend.  Optionally, an initial
;;   description of the file, COMMENT, may be specified, but it is not
;;   guaranteed that the backend will do anything with this.  The
;;   implementation should pass the value of vc-register-switches to
;;   the backend command.  (Note: in older versions of VC, this
;;   command had an optional revision first argument that was
;;   not used; in still older ones it took a single file argument and
;;   not a list.)
;;
;; - responsible-p (file)
;;
;;   Return the directory if this backend considers itself "responsible" for
;;   FILE, which can also be a directory.  This function is used to find
;;   out what backend to use for registration of new files and for things
;;   like change log generation.  The default implementation always
;;   returns nil.
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
;; * checkin (files comment &optional rev)
;;
;;   Commit changes in FILES to this backend.  COMMENT is used as a
;;   check-in comment.  The implementation should pass the value of
;;   vc-checkin-switches to the backend command.  The optional REV
;;   revision argument is only supported with some older VCSes, like
;;   RCS and CVS, and is otherwise silently ignored.
;;
;; - checkin-patch (patch-string comment)
;;
;;   Commit a single patch PATCH-STRING to this backend, bypassing any
;;   changes to the fileset.  COMMENT is used as a check-in comment.
;;   If PATCH-STRING contains authorship and date information in a
;;   format commonly used with the backend, it should be used as the
;;   commit authorship identity and date; in particular, this should
;;   always occur if PATCH-STRING was generated by the backend's
;;   prepare-patch function (see below).  Similarly, if COMMENT is nil
;;   and PATCH-STRING contains a log message, that log message should be
;;   used as the check-in comment.
;;
;; * find-revision (file rev buffer)
;;
;;   Fetch revision REV of file FILE and put it into BUFFER.
;;   If REV is the empty string, fetch the head of the trunk.
;;   The implementation should pass the value of vc-checkout-switches
;;   to the backend command.
;;
;; * checkout (file &optional rev)
;;
;;   Check out revision REV of FILE into the working area.  FILE
;;   should be writable by the user and if locking is used for FILE, a
;;   lock should also be set.  If REV is non-nil, that is the revision
;;   to check out (default is the working revision).  If REV is t,
;;   that means to check out the head of the current branch; if it is
;;   the empty string, check out the head of the trunk.  The
;;   implementation should pass the value of vc-checkout-switches to
;;   the backend command.  The 'editable' argument of older VC versions
;;   is gone; all files are checked out editable.
;;
;; * revert (file &optional contents-done)
;;
;;   Revert FILE back to the working revision.  If optional
;;   arg CONTENTS-DONE is non-nil, then the contents of FILE have
;;   already been reverted from a version backup, and this function
;;   only needs to update the status of FILE within the backend.
;;   If FILE is in the `added' state it should be returned to the
;;   `unregistered' state.
;;
;; - revert-files (files)
;;
;;   As revert, except that the first argument is a list of files, all
;;   of which require reversion, and reversion from version backups is
;;   not done.  Backends can implement this for faster mass reverts.
;;
;; - merge-file (file &optional rev1 rev2)
;;
;;   Merge the changes between REV1 and REV2 into the current working
;;   file (for non-distributed VCS).  It is expected that with an
;;   empty first revision this will behave like the merge-news method.
;;
;; - merge-branch ()
;;
;;   Merge another branch into the current one, prompting for a
;;   location to merge from.
;;
;; - merge-news (file)
;;
;;   Merge recent changes from the current branch into FILE.
;;   (for non-distributed VCS).
;;
;; - pull (prompt)
;;
;;   Pull "upstream" changes into the current branch (for distributed
;;   VCS).  If PROMPT is non-nil, or if necessary, prompt for a
;;   location to pull from.
;;
;; - steal-lock (file &optional revision)
;;
;;   Steal any lock on the working revision of FILE, or on REVISION if
;;   that is provided.  This function is only needed if locking is
;;   used for files under this backend, and if files can indeed be
;;   locked by other users.
;;
;; - get-change-comment (files rev)
;;
;;   Return the change comments associated with the files at the given
;;   revision.  The FILES argument it for forward-compatibility;
;;   existing implementations care only about REV.
;;
;; - modify-change-comment (files rev comment)
;;
;;   Modify the change comments associated with the files at the
;;   given revision.  This is optional, many backends do not support it.
;;
;; - mark-resolved (files)
;;
;;   Mark conflicts as resolved.  Some VC systems need to run a
;;   command to mark conflicts as resolved.
;;
;; - find-admin-dir (file)
;;
;;   Return the administrative directory of FILE.
;;
;; - add-working-tree (directory)
;;
;;   Create a new working tree at DIRECTORY that uses the same backing
;;   repository as this working tree.
;;   What gets checked out in DIRECTORY is left to the backend because
;;   while some VCS can check out the same branch in multiple working
;;   trees (e.g. Mercurial), others allow each branch to be checked out
;;   in only one working tree (e.g. Git).
;;   If a new branch should be created then the backend should handle
;;   prompting for this, including prompting for a branch or tag from
;;   which to start/fork the new branch, like `vc-create-branch'.
;;
;; - delete-working-tree (directory)
;;
;;   Remove the working tree, assumed to be one that uses the same
;;   backing repository as this working tree, at DIRECTORY.
;;   Callers must ensure that DIRECTORY is not the current working tree.
;;   This removal should be unconditional with respect to the state of
;;   the working tree: the caller is responsible for checking for
;;   uncommitted work in DIRECTORY.
;;
;; - move-working-tree (from to)
;;
;;   Relocate the working tree, assumed to be one that uses the same
;;   backing repository as this working tree, at FROM to TO.
;;   Callers must ensure that FROM is not the current working tree.
;;
;; - delete-revision (rev)
;;
;;   Remove REV from the revision history of the current branch.
;;   For a distributed VCS, this means a rebase operation to rewrite the
;;   history of the current branch so that it no longer contains REV (or
;;   its changes).  For a centralized VCS this may mean something
;;   different; for example CVS has true undos (not yet implemented in
;;   Emacs).  A distributed VCS that implements this must also implement
;;   revision-published-p.
;;
;; - delete-revisions-from-end (rev)
;;
;;   Delete revisions from the revision history, from the end of the
;;   branch up to but not including REV, including removing the changes
;;   made by those revisions to the working tree.  If there are
;;   uncommitted changes the implementation should discard them.
;;
;; - uncommit-revisions-from-end (rev)
;;
;;   Delete revisions from the revision history, from the end of the
;;   branch up to but not including REV, but without removing the
;;   changes made by those revisions from the working tree.
;;   I.e., the working tree contents should not change.

;; HISTORY FUNCTIONS
;;
;; * print-log (files buffer &optional shortlog start-revision limit)
;;
;;   Insert the revision log for FILES into BUFFER.
;;   If SHORTLOG is non-nil insert a short version of the log.
;;   If LIMIT is non-nil insert only insert LIMIT log entries.
;;   When LIMIT is a string it means stop right before that revision
;;   (i.e., revision LIMIT itself should not be included in the log).
;;   If the backend does not support limiting the number of entries to
;;   show it should return `limit-unsupported'.
;;   If START-REVISION is given, then show the log starting from that
;;   revision ("starting" in the sense of it being the _newest_
;;   revision shown, rather than the working revision, which is normally
;;   the case).  Not all backends support this.
;;
;; - log-outgoing (buffer upstream-location) (DEPRECATED)
;;
;;   Insert in BUFFER the revision log for the changes that will be
;;   sent when performing a push operation to UPSTREAM-LOCATION.
;;   Deprecated: implement incoming-revision and mergebase instead.
;;
;; - log-incoming (buffer upstream-location) (DEPRECATED)
;;
;;   Insert in BUFFER the revision log for the changes that will be
;;   received when performing a pull operation from UPSTREAM-LOCATION.
;;   Deprecated: implement incoming-revision and mergebase instead.
;;
;; * incoming-revision (&optional upstream-location refresh)
;;
;;   Return revision at the head of the branch at UPSTREAM-LOCATION.
;;   UPSTREAM-LOCATION defaults to where `vc-update' would pull from.
;;   If there is no such branch there, return nil.  (Should signal an
;;   error, not return nil, in the case that fetching data fails.)
;;   For a distributed VCS, should also fetch that revision into local
;;   storage for operating on by subsequent calls into the backend.
;;   The backend may rely on cached information from a previous fetch
;;   from UPSTREAM-LOCATION unless REFRESH is non-nil, which means that
;;   the most up-to-date information possible is required.
;;
;; - log-search (buffer pattern)
;;
;;   Search for PATTERN in the revision log and output results into BUFFER.
;;
;; - log-view-mode ()
;;
;;   Mode to use for the output of print-log.  This defaults to
;;   `log-view-mode' and is expected to be changed (if at all) to a derived
;;   mode of `log-view-mode'.
;;
;; - show-log-entry (revision)
;;
;;   If provided, search the log entry for REVISION in the current buffer,
;;   and make sure it is displayed in the buffer's window.  The default
;;   implementation of this function works for RCS-style logs.
;;
;; - comment-history (file)
;;
;;   Return a string containing all log entries that were made for FILE.
;;   This is used for transferring a file from one backend to another,
;;   retaining comment information.
;;
;; - update-changelog (files)
;;
;;   Using recent log entries, create ChangeLog entries for FILES, or for
;;   all files at or below the default-directory if FILES is nil.  The
;;   default implementation runs rcs2log, which handles RCS- and
;;   CVS-style logs.
;;
;; * diff (files &optional rev1 rev2 buffer async)
;;
;;   Insert the diff for FILE into BUFFER, or the *vc-diff* buffer if
;;   BUFFER is nil.  If ASYNC is non-nil, run asynchronously.  If REV1
;;   and REV2 are non-nil, report differences from REV1 to REV2.  If
;;   REV1 is nil, use the working revision (as found in the
;;   repository) as the older revision if REV2 is nil as well;
;;   otherwise, diff against an empty tree.  If REV2 is nil, use the
;;   current working-copy contents as the newer revision.  This
;;   function should pass the value of (vc-switches BACKEND 'diff) to
;;   the backend command.  It should return a status of either 0 (no
;;   differences found), or 1 (either non-empty diff or the diff is
;;   run asynchronously).
;;
;; - revision-completion-table (files)
;;
;;   Return a completion table for existing revisions of FILES.
;;   The default is to not use any completion table.
;;
;; - annotate-command (file buf &optional rev)
;;
;;   If this function is provided, it should produce an annotated display
;;   of FILE in BUF, relative to revision REV.  Annotation means each line
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
;;   (vc-annotate-convert-time (current-time)) -- i.e. the current
;;   time with hours, minutes, and seconds included.  Probably safe to
;;   ignore.  Return the current time, in units of fractional days.
;;
;; - annotate-extract-revision-at-line ()
;;
;;   Only required if `annotate-command' is defined for the backend.
;;   Invoked from a buffer in vc-annotate-mode, return the revision
;;   corresponding to the current line, or nil if there is no revision
;;   corresponding to the current line.
;;   If the backend supports annotating through copies and renames,
;;   and displays a file name and a revision, then return a cons
;;   (REVISION . FILENAME).
;;
;; - region-history (file buffer lfrom lto)
;;
;;   Insert into BUFFER the history (log comments and diffs) of the content of
;;   FILE between lines LFROM and LTO.  This is typically done asynchronously.
;;
;; - region-history-mode ()
;;
;;   Major mode to use for the output of `region-history'.
;;
;; - mergebase (rev1 &optional rev2)
;;
;;   Return the common ancestor between REV1 and REV2 revisions.
;;
;; - last-change (file line)
;;
;;   Return the most recent revision of FILE that made a change
;;   on LINE.
;;
;; - revision-published-p (rev)
;;
;;   For a distributed VCS, return whether REV is part of the public
;;   history of this branch, or only local history.  I.e., whether REV
;;   has been pushed.  Implementations should not consider whether REV
;;   is part of the public history of any other branches.
;;   It is an error if REV is not present on the current branch.
;;   Centralized VCS *must not* implement this, and there is no default
;;   implementation.

;; TAG/BRANCH SYSTEM
;;
;; - create-tag (dir name branchp)
;;
;;   Attach the tag NAME to the state of the working copy.  This
;;   should make sure that files are up-to-date before proceeding with
;;   the action.  DIR can also be a file and if BRANCHP is non-nil,
;;   NAME should be created as a branch and DIR should be checked out
;;   under this new branch.  Where it makes sense with the underlying
;;   VCS, should prompt for a branch or tag from which to start/fork the
;;   new branch, with completion candidates including all the known
;;   branches and tags of the repository.  The default implementation
;;   does not support branches but does a sanity check, a tree traversal
;;   and assigns the tag to each file.
;;
;; - retrieve-tag (dir name update)
;;
;;   Retrieve the version tagged by NAME of all registered files at or below DIR.
;;   If NAME is a branch name, switch to that branch.
;;   If UPDATE is non-nil, then update buffers of any files in the
;;   tag/branch that are currently visited.  The default implementation
;;   does a sanity check whether there aren't any uncommitted changes at
;;   or below DIR, and then performs a tree walk, using the `checkout'
;;   function to retrieve the corresponding revisions.
;;
;; - working-branch ()
;;
;;   Return the name of the current branch, if there is one, else nil.
;;
;; - trunk-or-topic-p ()
;;
;;   For the current branch, or the closest equivalent for a VCS without
;;   named branches, return `trunk' if it is definitely a longer-lived
;;   trunk branch, `topic' if it is definitely a shorter-lived topic
;;   branch, or nil if no general determination can be made.
;;
;;   What counts as a longer-lived or shorter-lived branch for VC is
;;   explained in Info node `(emacs)Outstanding Changes' and in the
;;   docstrings for the `vc-trunk-branch-regexps' and
;;   `vc-topic-branch-regexps' user options.
;;
;; - topic-outgoing-base ()
;;
;;   Return an outgoing base for the current branch (or the closest
;;   equivalent for a VCS without named branches) considered as a topic
;;   branch.  That is, on the assumption that the current branch is a
;;   shorter-lived branch which will later be merged into a longer-lived
;;   branch, return, if possible, the upstream location to which those
;;   changes will be merged.  See Info node `(emacs) Outstanding
;;   Changes'.  The return value should be suitable for passing to the
;;   incoming-revision backend function as its UPSTREAM-LOCATION
;;   argument.  For example, for Git the value will typically be of the
;;   form 'origin/foo' whereas Mercurial uses the unmodified name of the
;;   longer-lived branch.

;; MISCELLANEOUS
;;
;; - make-version-backups-p (file)
;;
;;   Return non-nil if unmodified repository revisions of FILE should be
;;   backed up locally.  If this is done, VC can perform `diff' and
;;   `revert' operations itself, without calling the backend system.  The
;;   default implementation always returns nil.
;;
;; - root (file)
;;
;;   Return the root of the VC controlled hierarchy for file.
;;
;; - ignore (file &optional directory remove)
;;
;;   Ignore FILE under DIRECTORY (default is 'default-directory').
;;   FILE is a file wildcard relative to DIRECTORY.
;;   When called interactively and with a prefix argument, remove FILE
;;   from ignored files.
;;   When called from Lisp code, if DIRECTORY is non-nil, the
;;   repository to use will be deduced by DIRECTORY.
;;   The default behavior is to add or remove a line from the file
;;   returned by the `find-ignore-file' function.
;;
;; - ignore-completion-table (directory)
;;
;;   Return the list of patterns for files ignored by the current
;;   version control system, e.g., the entries in `.gitignore' and
;;   `.bzrignore'.  The default behavior is to read the contents of
;;   the file returned by the `find-ignore-file' function.
;;
;;   NOTE: The return value should be a list of strings, not a general
;;   completion table value, despite what the name implies.
;;
;; - find-ignore-file (file)
;;
;;   Return the ignore file that controls FILE, e.g. `.gitignore' or
;;   `.bzrignore'.
;;
;; - previous-revision (file rev)
;;
;;   Return the revision number/hash that precedes REV for FILE, or nil
;;   if no such revision exists.  If the working-revision-symbol
;;   function is defined for this backend and that symbol, or a symbolic
;;   name involving that symbol, is passed to this function as REV, this
;;   function may return a symbolic name.
;;   The implementation should respect the value of vc-use-short-revision.
;;
;;   Possible future extension: make REV an optional argument, and if
;;   nil, default it to FILE's working revision.
;;
;; - file-name-changes (rev)
;;
;;   Return the list of pairs with changes in file names in REV.  When
;;   a file was added, it should be a cons with nil car.  When
;;   deleted, a cons with nil cdr.  When copied or renamed, a cons
;;   with the source name as car and destination name as cdr.
;;
;; - next-revision (file rev)
;;
;;   Return the revision number that follows REV for FILE, or nil if no such
;;   revision exists.
;;   The implementation should respect the value of vc-use-short-revision.
;;
;; - log-edit-mode ()
;;
;;   Turn on the mode used for editing the check in log.  This
;;   defaults to `log-edit-mode'.  If changed, it should use a mode
;;   derived from `log-edit-mode'.
;;
;; - check-headers ()
;;
;;   Return non-nil if the current buffer contains any version headers.
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
;;
;; - extra-menu ()
;;
;;   Return a menu keymap, the items in the keymap will appear at the
;;   end of the Version Control menu.  The goal is to allow backends
;;   to specify extra menu items that appear in the VC menu.  This way
;;   you can provide menu entries for functionality that is specific
;;   to your backend and which does not map to any of the VC generic
;;   concepts.
;;
;; - extra-dir-menu ()
;;
;;   Return a menu keymap, the items in the keymap will appear at the
;;   end of the VC Status menu.  The goal is to allow backends to
;;   specify extra menu items that appear in the VC Status menu.  This
;;   makes it possible to provide menu entries for functionality that
;;   is specific to a backend and which does not map to any of the VC
;;   generic concepts.
;;
;; - conflicted-files (dir)
;;
;;   Return the list of files where conflict resolution is needed in
;;   the project that contains DIR.
;;   FIXME: what should it do with non-text conflicts?
;;
;; - repository-url (file-or-dir &optional remote-name)
;;
;;   Returns the URL of the repository of the current checkout
;;   containing FILE-OR-DIR.  The optional REMOTE-NAME specifies the
;;   remote (in Git parlance) whose URL is to be returned.  It has
;;   only a meaning for distributed VCS and is ignored otherwise.
;;
;; - prepare-patch (rev)
;;
;;   Prepare a patch and return a property list with the keys `:subject'
;;   with the summary line (first line) of the patch message as a
;;   string; `:buffer' with a buffer object that contains the entire
;;   patch message; `:body-start' and `:body-end' demarcating the part
;;   of that buffer which should be inserted inline into a mail message
;;   body; and `:patch-start' and `:patch-end' demarcating the part of
;;   the buffer that is purely the patch, excluding any log message.
;;   If any of these *-start and *-end properties are omitted, they
;;   default to (point-min) and (point-max), respectively.
;;   If supported by the backend, the patch should contain authorship
;;   identity and date information, and REV's log message.
;;
;; - clone (remote directory rev)
;;
;;   Attempt to clone a REMOTE repository, into a local DIRECTORY.
;;   Returns a string with the directory with the contents of the
;;   repository if successful, otherwise nil.  With a non-nil value
;;   for REV the backend will attempt to check out a specific
;;   revision, if possible without first checking out the default
;;   branch.
;;
;; - cherry-pick-comment (files rev reverse)
;;
;;   Return a suitable log message for cherry-picking REV onto another
;;   branch.  Typically this will be REV's original log message with
;;   something appended (e.g. Git's "(cherry picked from commit ...)").
;;   If REVERSE, return a suitable log message for a commit that undoes
;;   the effects of REV.  FILES is for forward-compatibility; existing
;;   implementations ignore it.

;;; Changes from the pre-25.1 API:
;;
;; - INCOMPATIBLE CHANGE: The 'editable' optional argument of
;;   vc-checkout is gone.  The upper level assumes that all files are
;;   checked out editable.  This moves closer to emulating modern
;;   non-locking behavior even on very old VCSes.
;;
;; - INCOMPATIBLE CHANGE: The vc-register function and its backend
;;   implementations no longer take a first optional revision
;;   argument, since on no system since RCS has setting the initial
;;   revision been even possible, let alone sane.
;;
;; - INCOMPATIBLE CHANGE: In older versions of the API, vc-diff did
;;   not take an async-mode flag as a fourth optional argument.  (This
;;   change eliminated a particularly ugly global.)
;;
;; - INCOMPATIBLE CHANGE: The backend operation for non-distributed
;;   VCSes formerly called "merge" is now "merge-file" (to contrast
;;   with merge-branch), and does its own prompting for revisions.
;;   (This fixes a layer violation that produced bad behavior under
;;   SVN.)
;;
;; - INCOMPATIBLE CHANGE: The old fourth 'default-state' argument of
;;   dir-status-files is gone; none of the back ends actually used it.
;;
;; - dir-status is no longer a public method; it has been replaced by
;;   dir-status-files.
;;
;; - state-heuristic is no longer a public method (the CVS backend
;;   retains it as a private one).
;;
;; - the vc-mistrust-permissions configuration variable is gone; the
;;   code no longer relies on permissions except in one corner case where
;;   CVS leaves no alternative (which was not gated by this variable).  The
;;   only affected back ends were SCCS and RCS.
;;
;; - vc-stay-local-p and repository-hostname are no longer part
;;   of the public API.  The vc-cvs-stay-local configuration variable
;;   remains and only affects the CVS back end.
;;
;; - The init-revision function and the default-initial-revision
;;   variable are gone.  These haven't made sense on anything shipped
;;   since RCS, and using them was a dumb stunt even on RCS.
;;
;; - workfile-unchanged-p is no longer a public back-end method.  It
;;   was redundant with vc-state and usually implemented with a trivial
;;   call to it.  A few older back ends retain versions for internal use in
;;   their vc-state functions.
;;
;; - could-register is no longer a public method.  Only vc-cvs ever used it
;;
;;   The vc-keep-workfiles configuration variable is gone.  Used only by
;;   the RCS and SCCS backends, it was an invitation to shoot self in foot
;;   when set to the (non-default) value nil.  The original justification
;;   for it (saving disk space) is long obsolete.
;;
;; - The rollback method (implemented by RCS and SCCS only) is gone.  See
;;   the to-do note on uncommit.
;;
;; - latest-on-branch-p is no longer a public method.  It was to be used
;;   for implementing rollback.  RCS keeps its implementation (the only one)
;;   for internal use.


;;; Todo:

;;;; New Primitives:
;;
;; - uncommit: undo last checkin, leave changes in place in the workfile,
;;   stash the commit comment for reuse.
;;
;; - deal with push operations.
;;
;;;; Primitives that need changing:
;;
;; - vc-update/vc-merge should deal with VC systems that don't do
;;   update/merge on a file basis, but on a whole repository basis.
;;   vc-update and vc-merge assume the arguments are always files,
;;   they don't deal with directories.  Make sure the *vc-dir* buffer
;;   is updated after these operations.
;;   At least bzr, git and hg should benefit from this.
;;
;;;; Improved branch and tag handling:
;;
;; - Make sure the *vc-dir* buffer is updated after merge-branch operations.
;;
;; - add a generic mechanism for remembering the current branch names,
;;   display the branch name in the mode-line.  Replace
;;   vc-cvs-sticky-tag with that.
;;
;; - Add the ability to list tags and branches.
;;
;;;; Other
;;
;; - asynchronous checkin and commit, so you can keep working in other
;;   buffers while the repo operation happens.
;;
;; - Direct support for stash/shelve.
;;
;; - when a file is in `conflict' state, turn on smerge-mode.
;;
;; - figure out what to do with conflicts that are not caused by the
;;   file contents, but by metadata or other causes.  Example: File A
;;   gets renamed to B in one branch and to C in another and you merge
;;   the two branches.  Or you locally add file FOO and then pull a
;;   change that also adds a new file FOO, ...
;;
;; - make it easier to write logs.  Maybe C-x 4 a should add to the log
;;   buffer, if one is present, instead of adding to the ChangeLog.
;;
;; - When vc-next-action calls vc-checkin it could pre-fill the
;;   *vc-log* buffer with some obvious items: the list of files that
;;   were added, the list of files that were removed.  If the diff is
;;   available, maybe it could even call something like
;;   `diff-add-change-log-entries-other-window' to create a detailed
;;   skeleton for the log...
;;
;; - most vc-dir backends need more work.  They might need to
;;   provide custom headers, use the `extra' field and deal with all
;;   possible VC states.
;;
;; - add a function that calls vc-dir to `find-directory-functions'.
;;
;; - vc-diff, vc-annotate, etc. need to deal better with unregistered
;;   files.  Now that unregistered and ignored files are shown in
;;   vc-dir, it is possible that these commands are called
;;   for unregistered/ignored files.
;;
;; - vc-next-action needs work in order to work with multiple
;;   backends: `vc-state' returns the state for the default backend,
;;   not for the backend in the current *vc-dir* buffer.
;;
;; - vc-dir-kill-dir-status-process should not be specific to dir-status,
;;   it should work for other async commands done through vc-do-command
;;   as well,
;;
;; - vc-dir toolbar needs more icons.
;;
;; - The backends should avoid using `vc-file-setprop' and `vc-file-getprop'.
;;
;;; Code:

(require 'vc-hooks)
(require 'vc-dispatcher)
(require 'cl-lib)

(declare-function diff-setup-whitespace "diff-mode" ())
(declare-function diff-setup-buffer-type "diff-mode" ())

(eval-when-compile
  (require 'dired))

(declare-function dired-get-filename "dired" (&optional localp noerror))
(declare-function dired-move-to-filename "dired" (&optional err eol))
(declare-function dired-marker-regexp "dired" ())

(unless (assoc 'vc-parent-buffer minor-mode-alist)
  (setq minor-mode-alist
	(cons '(vc-parent-buffer vc-parent-buffer-name)
	      minor-mode-alist)))

;; General customization

(defgroup vc nil
  "Emacs interface to version control systems."
  :group 'tools)

(defcustom vc-checkin-switches nil
  "A string or list of strings specifying extra switches for checkin.
These are passed to the checkin program by \\[vc-checkin]."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string)))

(defcustom vc-checkout-switches nil
  "A string or list of strings specifying extra switches for checkout.
These are passed to the checkout program by \\[vc-checkout]."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string)))

(defcustom vc-register-switches nil
  "A string or list of strings; extra switches for registering a file.
These are passed to the checkin program by \\[vc-register]."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string)))

(defcustom vc-diff-switches nil
  "A string or list of strings specifying switches for diff under VC.
When running diff under a given BACKEND, VC uses the first
non-nil value of `vc-BACKEND-diff-switches', `vc-diff-switches',
and `diff-switches', in that order.  Since nil means to check the
next variable in the sequence, either of the first two may use
the value t to mean no switches at all.  `vc-diff-switches'
should contain switches that are specific to version control, but
not specific to any particular backend."
  :type '(choice (const :tag "Unspecified" nil)
		 (const :tag "None" t)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List" :value ("") string))
  :version "21.1")

(defcustom vc-annotate-switches nil
  "A string or list of strings specifying switches for annotate under VC.
When running annotate under a given BACKEND, VC uses the first
non-nil value of `vc-BACKEND-annotate-switches' and
`vc-annotate-switches', in that order.  Since nil means to check
the next variable in the sequence, setting the first to the value
t means no switches at all.  `vc-annotate-switches' should
contain switches that are specific to version control, but not
specific to any particular backend.

As very few switches (if any) are used across different VC tools,
please consider using the specific `vc-BACKEND-annotate-switches'
for the backend you use."
  :type '(choice (const :tag "Unspecified" nil)
		 (const :tag "None" t)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List" :value ("") string))
  :version "25.1")

(defcustom vc-log-show-limit 2000
  "Limit the number of items shown by the VC log commands.
Zero means unlimited.
Not all VC backends are able to support this feature."
  :type 'natnum)

(defcustom vc-allow-async-revert nil
  "Specifies whether the diff during \\[vc-revert] may be asynchronous.
Enabling this option means that you can confirm a revert operation even
if the local changes in the file have not been found and displayed yet."
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t))
  :version "22.1")

(defcustom vc-allow-async-diff nil
  "Non-nil to allow asynchronous diff process.
Enabling this means the buffer will be displayed before the diff is
generated, and so might only say \"No changes ...\"."
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t))
  :version "31.1")

;;;###autoload
(defcustom vc-checkout-hook nil
  "Normal hook (list of functions) run after checking out a file.
See `run-hooks'."
  :type 'hook
  :version "21.1")

;;;###autoload
(defcustom vc-checkin-hook nil
  "Normal hook (list of functions) run after commit or file checkin.
See also `log-edit-done-hook'."
  :type 'hook
  :options '(log-edit-comment-to-change-log))

;;;###autoload
(defcustom vc-before-checkin-hook nil
  "Normal hook (list of functions) run before a commit or a file checkin.
See `run-hooks'."
  :type 'hook)

(defcustom vc-retrieve-tag-hook nil
  "Normal hook (list of functions) run after retrieving a tag."
  :type 'hook
  :version "27.1")

(defcustom vc-revert-show-diff t
  "If non-nil, `vc-revert' shows a `vc-diff' buffer before querying."
  :type '(choice (const :tag "Show and bury afterwards" t)
                 (const :tag "Show and kill afterwards" kill)
                 (const :tag "Don't show" nil))
  :version "24.1")

;; Header-insertion hair

(defcustom vc-static-header-alist
  '(("\\.c\\'" .
     "\n#ifndef lint\nstatic char vcid[] = \"%s\";\n#endif /* lint */\n"))
  "Associate static header string templates with file types.
A %s in the template is replaced with the first string associated with
the file's version control type in `vc-BACKEND-header'."
  :type '(repeat (cons :format "%v"
		       (regexp :tag "File Type")
		       (string :tag "Header String"))))

(defcustom vc-comment-alist
  '((nroff-mode ".\\\"" ""))
  "Special comment delimiters for generating VC headers.
Add an entry in this list if you need to override the normal `comment-start'
and `comment-end' variables.  This will only be necessary if the mode language
is sensitive to blank lines."
  :type '(repeat (list :format "%v"
		       (symbol :tag "Mode")
		       (string :tag "Comment Start")
		       (string :tag "Comment End"))))

(defcustom vc-find-revision-no-save nil
  "If non-nil, `vc-find-revision' doesn't write the created buffer to file."
  :type 'boolean
  :version "27.1")

;; The default is nil because only a VC user who also possesses a lot of
;; knowledge specific to the VCS in use can know when it is okay to
;; rewrite history, and we can't convey to a user who is relatively
;; naïve regarding the VCS in use the potential risks in only the space
;; of a minibuffer yes/no prompt.
;;
;; See `vc-git--assert-allowed-rewrite' for an example of how to use
;; this variable in VCS backend code.
(defcustom vc-allow-rewriting-published-history nil
  "When non-nil, permit VCS operations that may rewrite published history.

Many VCS commands can change your copy of published change history
without warning.  If this occurs, you won't be able to pull and push in
the ordinary way until you take special action.  For example, for Git,
see \"Recovering from Upstream Rebase\" in the Man page git-rebase(1).

Normally, Emacs refuses to run VCS commands that it thinks will rewrite
published history.  If you customize this variable to `ask', Emacs will
instead prompt you to confirm that you really want to perform the
rewrite.  Any other non-nil value means to proceed with no prompting.

We recommend customizing this variable to `ask' or leaving it nil,
because if published history is rewritten unexpectedly it can be fairly
time-consuming to recover.  Only customize this variable to a non-nil
value other than `ask' if you have a strong grasp of the VCS in use."
  :type '(choice (const :tag "Don't allow" nil)
                 (const :tag "Prompt to allow" ask)
                 (const :tag "Allow without prompting" t))
  :version "31.1")

(define-obsolete-variable-alias
  'vc-cloneable-backends-custom-type
  'vc-clonable-backends-custom-type "31.1")
(defconst vc-clonable-backends-custom-type
  `(choice :convert-widget
           ,(lambda (widget)
              (let (opts)
                (dolist (be vc-handled-backends)
                  (when (or (vc-find-backend-function be 'clone)
                            (alist-get 'clone (get be 'vc-functions)))
                    (push (widget-convert (list 'const be)) opts)))
                (widget-put widget :args opts))
              widget))
  "The type of VC backends that support cloning VCS repositories.")

(defcustom vc-clone-heuristic-alist
  `((,(rx bos "http" (? "s") "://"
          (or (: (? "www.") "github.com"
                 "/" (+ (or alnum "-" "." "_"))
                 "/" (+ (or alnum "-" "." "_")))
              (: "codeberg.org"
                 "/" (+ (or alnum "-" "." "_"))
                 "/" (+ (or alnum "-" "." "_")))
              (: (? "www.") "gitlab" (+ "." (+ alnum))
                 "/" (+ (or alnum "-" "." "_"))
                 "/" (+ (or alnum "-" "." "_")))
              (: "git.sr.ht"
                 "/~" (+ (or alnum "-" "." "_"))
                 "/" (+ (or alnum "-" "." "_")))
              (: "git." (or "savannah" "sv") "." (? "non") "gnu.org/"
                 (or "r" "git") "/"
                 (+ (or alnum "-" "." "_")) (? "/")))
          (or (? "/") ".git") eos)
     . Git)
    (,(rx bos "http" (? "s") "://"
          (or (: "hg.sr.ht"
                 "/~" (+ (or alnum "-" "." "_"))
                 "/" (+ (or alnum "-" "." "_")))
              (: "hg." (or "savannah" "sv") "." (? "non") "gnu.org/hgweb/"
                 (+ (or alnum "-" "." "_")) (? "/")))
          eos)
     . Hg)
    (,(rx bos "http" (? "s") "://"
          (or (: "bzr." (or "savannah" "sv") "." (? "non") "gnu.org/r/"
                 (+ (or alnum "-" "." "_")) (? "/")))
          eos)
     . Bzr))
  "Alist mapping repository URLs to VC backends.
`vc-clone' consults this alist to determine the VC
backend from the repository URL when you call it without
specifying a backend.  Each element of the alist has the form
\(URL-REGEXP . BACKEND).  `vc-clone' will use BACKEND of
the first association for which the URL of the repository matches
the URL-REGEXP of the association."
  :type `(alist :key-type (regexp :tag "Regular expression matching URLs")
                :value-type ,vc-clonable-backends-custom-type)
  :version "31.1")

(defcustom vc-async-checkin nil
  "If non-nil, checkin operations should be done asynchronously.

This is useful to set as a directory local variable in repositories
where the VCS in use performs checkin operations slowly.
For example, Git is slow when committing changes to very large files,
and Mercurial can be slow when there is a very large number of files.

While an asynchronous checkin operation is in progress, Emacs installs a
`before-save-hook' to switch back to a synchronous checkin if you ask to
save buffers under the current VC tree.  This is to avoid nondeterminism
regarding exactly what changes get checked in.

Not supported by all backends."
  :type 'boolean
  :safe #'booleanp
  :version "31.1")

(defmacro vc--with-backend-in-rootdir (desc &rest body)
  (declare (indent 1) (debug (sexp body)))
  ;; Intentionally capture `backend' and `rootdir':
  ;; no need to keep repeating them.
  `(let ((backend (vc-deduce-backend))
         (default-directory default-directory)
	 rootdir)
     (if backend
	 (setq rootdir (vc-call-backend backend 'root default-directory))
       (setq rootdir
             (read-directory-name ,(format "Directory for %s: " desc)))
       (setq backend (vc-responsible-backend rootdir))
       (unless backend
         (error "Directory is not version controlled")))
     (setq default-directory rootdir)
     ,@body))


;; File property caching

(defun vc-clear-context ()
  "Clear all cached file properties."
  (interactive)
  (obarray-clear vc-file-prop-obarray))

(defmacro with-vc-properties (files form settings)
  "Execute FORM, then maybe set per-file properties for FILES.
If any of FILES is actually a directory, then do the same for all
buffers for files in that directory.
SETTINGS is an association list of property/value pairs.  After
executing FORM, set those properties from SETTINGS that have not yet
been updated to their corresponding values.
Return the result of evaluating FORM."
  (declare (debug t))
  (cl-with-gensyms (vc-touched-properties flist)
    `(let ((,vc-touched-properties (list t))
	   (,flist nil))
       (prog2 (dolist (file ,files)
                (if (file-directory-p file)
	            (dolist (buffer (buffer-list))
	              (let ((fname (buffer-file-name buffer)))
	                (when (and fname (string-prefix-p file fname))
		          (push fname ,flist))))
	          (push file ,flist)))
           ,form
         (dolist (file ,flist)
           (dolist (setting ,settings)
             (let ((property (car setting)))
               (unless (memq property ,vc-touched-properties)
                 (put (intern file vc-file-prop-obarray)
                      property (cdr setting))))))))))

;;; Code for deducing what fileset and backend to assume

(defun vc-backend-for-registration (file)
  "Return a backend that can be used for registering FILE.

If no backend declares itself responsible for FILE, then FILE
must not be in a version controlled directory, so try to create a
repository, prompting for the directory and the VC backend to
use."
  (catch 'found
    ;; First try: find a responsible backend, it must be a backend
    ;; under which FILE is not yet registered and with the most
    ;; specific path to FILE.
    (let ((max 0)
          bk)
      (dolist (backend vc-handled-backends)
        (when (not (vc-call-backend backend 'registered file))
          (let* ((dir-name (vc-call-backend backend 'responsible-p file))
                 (len (and dir-name
                           (length (file-name-split
                                    (expand-file-name dir-name))))))
            (when (and len (> len max))
              (setq max len bk backend)))))
      (when bk
        (throw 'found bk)))
    ;; no responsible backend
    (let* ((possible-backends
	    (let (pos)
	      (dolist (crt vc-handled-backends)
		(when (vc-find-backend-function crt 'create-repo)
		  (push crt pos)))
	      pos))
	   (bk
	    (intern
	     ;; Read the VC backend from the user, only
	     ;; complete with the backends that have the
	     ;; 'create-repo method.
	     (completing-read
	      (format "%s is not in a version controlled directory.\nUse VC backend: " file)
	      (mapcar #'symbol-name possible-backends) nil t)))
	   (repo-dir
	    (let ((def-dir (file-name-directory file)))
	      ;; read the directory where to create the
	      ;; repository, make sure it's a parent of
	      ;; file.
	      (read-file-name
	       (format "create %s repository in: " bk)
	       default-directory def-dir t nil
	       (lambda (arg)
		 (message "arg %s" arg)
		 (and (file-directory-p arg)
		      (string-prefix-p (expand-file-name arg) def-dir)))))))
      (let ((default-directory repo-dir))
	(vc-call-backend bk 'create-repo))
      (throw 'found bk))))

(defun vc-guess-url-backend (url)
  "Guess the VC backend for URL.
This function will internally query `vc-clone-heuristic-alist'
and return nil if it cannot reasonably guess."
  (and url (alist-get url vc-clone-heuristic-alist
                      nil nil #'string-match-p)))

;;;###autoload
(defun vc-responsible-backend (file &optional no-error)
  "Return the name of a backend system that is responsible for FILE.

If FILE is already registered, return the
backend of FILE.  If FILE is not registered, then the
first backend in `vc-handled-backends' that declares itself
responsible for FILE is returned.

Note that if FILE is a symbolic link, it will not be resolved --
the responsible backend system for the symbolic link itself will
be reported.

If NO-ERROR is nil, signal an error that no VC backend is
responsible for the given file."
  (or (and (not (file-directory-p file)) (vc-backend file))
      ;; FIXME it would be more efficient to walk up the directory tree,
      ;; stopping the first time a backend is responsible.
      ;;
      ;; First try: find a responsible backend.  If this is for registration,
      ;; it must be a backend under which FILE is not yet registered.
      (let* ((file (expand-file-name file))
             (dirs (delq nil
                         (mapcar
                          (lambda (backend)
                            (when-let* ((dir (vc-call-backend
                                              backend 'responsible-p file)))
                              ;; We run DIR through `expand-file-name'
                              ;; so that abbreviated directories, such
                              ;; as "~/", wouldn't look "less specific"
                              ;; due to their artificially shorter length.
                              (cons backend (expand-file-name dir))))
                          vc-handled-backends))))
        ;; Just a single response (or none); use it.
        (if (< (length dirs) 2)
            (caar dirs)
          ;; Several roots; we seem to have one vc inside another's
          ;; directory.  Choose the most specific.
          (caar (sort dirs (lambda (d1 d2)
                             (< (length (cdr d2)) (length (cdr d1))))))))
      (unless no-error
        (error "No VC backend is responsible for %s" file))))

(defun vc-expand-dirs (file-or-dir-list backend)
  "Expand directories in a file list specification.
Within directories, only files already under version control are noticed."
  (let ((flattened '()))
    (dolist (node file-or-dir-list)
      (when (file-directory-p node)
	(vc-file-tree-walk
	 node (lambda (f) (when (eq (vc-backend f) backend) (push f flattened)))))
      (unless (file-directory-p node) (push node flattened)))
    (nreverse flattened)))

(defvar vc-dir-backend)
(defvar log-view-vc-backend)
(defvar log-view-vc-fileset)
(defvar log-edit-vc-backend)
(defvar diff-vc-backend)
(defvar diff-vc-revisions)

(defcustom vc-deduce-backend-nonvc-modes
  ;; Maybe we could even use comint-mode rather than shell-mode?
  '(dired-mode shell-mode eshell-mode compilation-mode)
  "List of modes not supported by VC where backend should be deduced.
In these modes the backend is deduced based on `default-directory'.
If the value is t, the backend is deduced in all modes."
  :type '(choice (const :tag "None" nil)
                 (repeat symbol)
                 (const :tag "All" t))
  :version "30.1")

(defvar-local vc-buffer-overriding-fileset nil
  "Specialized, static value for `vc-deduce-fileset' for this buffer.
If non-nil, this should be a list of length 2 or 5.
See `vc-deduce-fileset' regarding these possible forms.
If this list is of length 2, it will be used only when the
STATE-MODEL-ONLY-FILES argument to `vc-deduce-fileset' is nil.")

(defvar-local vc-buffer-revision nil
  "VCS revision to which this buffer's contents corresponds.
Lisp code which sets this should also set `vc-buffer-overriding-fileset'
such that the buffer's local variables also specify a VC backend,
rendering the value of this variable unambiguous.
Should never be a symbolic name but always a revision number/hash.")

(defun vc-deduce-backend ()
  (cond ((car vc-buffer-overriding-fileset))
        ((derived-mode-p 'vc-dir-mode)   vc-dir-backend)
        ((derived-mode-p 'log-view-mode) log-view-vc-backend)
        ((derived-mode-p 'log-edit-mode) log-edit-vc-backend)
        ((derived-mode-p 'diff-mode)     diff-vc-backend)
        ((or (eq vc-deduce-backend-nonvc-modes t)
             (derived-mode-p vc-deduce-backend-nonvc-modes))
         (ignore-errors (vc-responsible-backend default-directory)))
        (vc-mode (vc-backend buffer-file-name))))

(declare-function vc-dir-current-file "vc-dir" ())
(declare-function vc-dir-deduce-fileset "vc-dir" (&optional state-model-only-files))
(declare-function dired-vc-deduce-fileset "dired-aux" (&optional state-model-only-files not-state-changing))

(defun vc-deduce-fileset (&optional not-state-changing
				    allow-unregistered
				    state-model-only-files)
  "Deduce a set of files and a backend to which to apply an operation.
Return a list of the form:

  (BACKEND FILESET FILESET-ONLY-FILES STATE CHECKOUT-MODEL)

where the last 3 members are optional, and must be present only if
STATE-MODEL-ONLY-FILES is non-nil.

NOT-STATE-CHANGING, if non-nil, means that the operation
requesting the fileset doesn't intend to change the VC state,
such as when printing the log or showing the diffs.

If the current buffer is in `vc-dir' or Dired mode, FILESET is the
list of marked files, or the file under point if no files are
marked.
Otherwise, if the current buffer is visiting a version-controlled
file or is an indirect buffer whose base buffer visits a
version-controlled file, FILESET is a single-file list containing
that file's name.
Otherwise, if ALLOW-UNREGISTERED is non-nil and the visited file
is unregistered, FILESET is a single-file list containing the
name of the visited file.
Otherwise, throw an error.

STATE-MODEL-ONLY-FILES, if non-nil, means that the caller needs
the FILESET-ONLY-FILES, STATE, and CHECKOUT-MODEL info, where
FILESET-ONLY-FILES means only files in similar VC states,
possible values of STATE are explained in `vc-state', and MODEL in
`vc-checkout-model'.  Otherwise, these 3 members may be omitted from
the returned list.

BEWARE: this function may change the current buffer."
  (when (buffer-base-buffer)
    (set-buffer (buffer-base-buffer)))
  (let (backend)
    (cond
     ((and vc-buffer-overriding-fileset
           (not (or (length= vc-buffer-overriding-fileset 2)
                    (length= vc-buffer-overriding-fileset 5))))
      (error "Invalid value for `vc-buffer-overriding-fileset' %S"
             vc-buffer-overriding-fileset))
     ((and (or (not state-model-only-files)
               (length= vc-buffer-overriding-fileset 5))
           vc-buffer-overriding-fileset))
     ((derived-mode-p 'vc-dir-mode)
      (vc-dir-deduce-fileset state-model-only-files))
     ((derived-mode-p 'dired-mode)
      (dired-vc-deduce-fileset state-model-only-files not-state-changing))
     ((and (derived-mode-p 'diff-mode) (not buffer-file-name))
      (diff-vc-deduce-fileset))
     ((setq backend (vc-backend buffer-file-name))
      (if state-model-only-files
	(list backend (list buffer-file-name)
	      (list buffer-file-name)
	      (vc-state buffer-file-name)
	      (vc-checkout-model backend buffer-file-name))
	(list backend (list buffer-file-name))))
     ((derived-mode-p 'log-view-mode)
      ;; 'log-view-mode' stashes the backend and the fileset in the
      ;; two special variables, so we use them to avoid any possible
      ;; mistakes from a decision made here ad-hoc.
      (list log-view-vc-backend log-view-vc-fileset))
     ((and (buffer-live-p vc-parent-buffer)
           ;; FIXME: Why this test?  --Stef
           (or (buffer-file-name vc-parent-buffer)
				(with-current-buffer vc-parent-buffer
				  (or (derived-mode-p 'vc-dir-mode)
				      (derived-mode-p 'dired-mode)
				      (derived-mode-p 'diff-mode)))))
      (progn                  ;FIXME: Why not `with-current-buffer'? --Stef.
	(set-buffer vc-parent-buffer)
        (vc-deduce-fileset not-state-changing allow-unregistered state-model-only-files)))
     ((and (not buffer-file-name)
	   (setq backend (vc-responsible-backend default-directory)))
      (list backend (list default-directory)))
     ((and allow-unregistered (not (vc-registered buffer-file-name)))
      (if state-model-only-files
	  (list (vc-backend-for-registration (buffer-file-name))
		(list buffer-file-name)
		(list buffer-file-name)
		(when state-model-only-files 'unregistered)
		nil)
	(list (vc-backend-for-registration (buffer-file-name))
	      (list buffer-file-name))))
     (t (error "File is not under version control")))))

;; This function should possibly honor `vc-buffer-overriding-fileset'
;; when the fileset consists of a single file, but only if that file is
;; part of the current working revision, i.e., actually on disk now.
(defun vc-ensure-vc-buffer ()
  "Make sure that the current buffer visits a version-controlled file."
  (cond
   ((derived-mode-p 'vc-dir-mode)
    (set-buffer (find-file-noselect (vc-dir-current-file))))
   ((derived-mode-p 'dired-mode)
    (set-buffer (find-file-noselect (dired-get-filename))))
   (t
    (while (and vc-parent-buffer
                (buffer-live-p vc-parent-buffer)
		;; Avoid infinite looping when vc-parent-buffer and
		;; current buffer are the same buffer.
 		(not (eq vc-parent-buffer (current-buffer))))
      (set-buffer vc-parent-buffer))))
  (if (not buffer-file-name)
      (error "Buffer %s is not associated with a file" (buffer-name))
    (unless (vc-backend buffer-file-name)
      (error "File %s is not under version control" buffer-file-name))))

(defun vc-only-files-state-and-model (files backend)
  "Compute last three `vc-deduce-fileset' return value elements for FILES.
FILES should be a pair, or list of pairs, of files and their VC states.
BACKEND is the VC backend responsible for FILES."
  (let* ((files (if (proper-list-p files) files (list files)))
         files* states-alist states state)
    ;; Check that all files are in a consistent state, since we use that
    ;; state to decide which operation to perform.
    (pcase-dolist (`(,file . ,state) files)
      (push file files*)
      (push file (alist-get state states-alist nil nil #'eq)))
    (setq states (mapcar #'car states-alist))
    (cond ((length= states 1)
           (setq state (car states)))
          ((cl-subsetp states '(added missing removed edited))
           (setq state 'edited))

          ;; Special, but common case: checking in both changes and new
          ;; files at once.  The actual registration is delayed until
          ;; `vc-checkin' so that if the user changes their mind while
          ;; entering the log message, we leave things as we found them.
          ;;
          ;; An alternative would be to delay it until the backend
          ;; `vc-*-checkin'.  The advantages would be that those
          ;; functions could complete the whole operation in fewer total
          ;; shell commands, and if the checkin itself fails they could
          ;; ensure the file is left unregistered then too (e.g. for Git
          ;; we may be able to use 'git add -N', though that would still
          ;; require a subsequent 'git reset').
          ;; The disadvantage would be a more complex VC API because we
          ;; would have to distinguish between backends which can and
          ;; can't handle registration and checkin together.
          ((and (cl-subsetp states
                            '(added missing removed edited unregistered))
                (y-or-n-p "\
Some files are unregistered; register them before checking in?"))
           (setq state 'edited))

          (t
           (let* ((pred (lambda (elt)
                          (memq (car elt) '(added missing removed edited))))
                  (compat-alist (cl-remove-if-not pred states-alist))
                  (other-alist (cl-remove-if pred states-alist))
                  (first (car (or compat-alist other-alist)))
                  (second (if compat-alist
                              (car other-alist)
                            (cadr other-alist))))
             (error "\
To apply VC operations to multiple files, the files must be in similar VC states.
%s in state %s clashes with %s in state %s"
                    (cadr first) (car first) (cadr second) (car second)))))
    (list files* state
          (and state (not (eq state 'unregistered))
               (vc-checkout-model backend files*)))))

;;; Support for the C-x v v command.
;; This is where all the single-file-oriented code from before the fileset
;; rewrite lives.

(defsubst vc-editable-p (file)
  "Return non-nil if FILE can be edited."
  (let ((backend (vc-backend file)))
    (and backend
         (or (eq (vc-checkout-model backend (list file)) 'implicit)
             (memq (vc-state file) '(edited needs-merge conflict))))))

(defun vc-read-backend (prompt &optional backends default)
  (let ((backends (or backends vc-handled-backends))
        (completion-ignore-case t))
    (intern
     (completing-read prompt (mapcar #'symbol-name backends)
                      nil 'require-match nil nil default))))

;; Here's the major entry point.

;;;###autoload
(defun vc-next-action (verbose)
  "Do the next logical version control operation on the current fileset.
This requires that all files in the current VC fileset be in the
sufficiently similar states.  If they are not, signal an error.
Also signal an error if files in the fileset are ignored by the VCS.

For modern merging-based version control systems:
  If every file in the fileset is not registered for version
   control, register the fileset (but don't commit).  If VERBOSE is
   non-nil (interactively, the prefix argument), ask for the VC
   backend with which to register the fileset.
  If every work file in the VC fileset is either added or modified,
   pop up a *vc-log* buffer to commit the fileset changes.
  (If some are added or modified and some are unregistered, offer to
   register the unregistered ones, first.)
  For a centralized version control system, if any work file in
   the VC fileset is out of date, offer to update the fileset.

For old-style locking-based version control systems, like RCS:
  If every file is not registered, register the file(s); with a prefix
   argument, allow to specify the VC backend for registration.
  If every file is registered and unlocked, check out (lock)
   the file(s) for editing.
  If every file is locked by you and has changes, pop up a
   *vc-log* buffer to check in the changes.  Leave a
   read-only copy of each changed file after checking in.
  If every file is locked by you and unchanged, unlock them.
  If every file is locked by someone else, offer to steal the lock.
  If files are unlocked, but have changes, offer to either claim the
   lock or revert to the last checked-in version.

If this command is invoked from a patch buffer under `diff-mode', it
will apply the diffs from the patch and pop up a *vc-log* buffer to
check-in the resulting changes.

When using this command to register a new file (or files), it
will automatically deduce which VC repository to register it
with, using the most specific one.

If VERBOSE is non-nil (interactively, the prefix argument),
you can specify another VC backend for the file(s),
or (for centralized VCS only) the revision ID or branch ID
from which to check out the file(s)."
  (interactive "P")
  (let* ((vc-fileset (vc-deduce-fileset nil t 'state-model-only-files))
         (backend (car vc-fileset))
	 (files (nth 1 vc-fileset))
         (fileset-only-files (nth 2 vc-fileset))
         ;; FIXME: We used to call `vc-recompute-state' here.
         (state (nth 3 vc-fileset))
         ;; The backend should check that the checkout-model is consistent
         ;; among all the `files'.
	 (model (nth 4 vc-fileset)))

    ;; If a buffer has unsaved changes, a checkout would discard those
    ;; changes, so treat the buffer as having unlocked changes.
    (when (and (not (eq model 'implicit)) (eq state 'up-to-date))
      (dolist (file files)
        (let ((buffer (get-file-buffer file)))
          (and buffer
               (buffer-modified-p buffer)
               (setq state 'unlocked-changes)))))

    ;; Do the right thing.
    (cond
     ((eq state 'ignored)
      (error "Fileset files are ignored by the version-control system"))
     ;; Fileset comes from a diff-mode buffer, see
     ;; 'diff-vc-deduce-fileset', and the buffer is the patch to apply.
     ((eq model 'patch)
      (vc-checkin files backend nil nil nil (buffer-string)))
     ((or (null state) (eq state 'unregistered))
      (cond (verbose
             (let ((backend (vc-read-backend "Backend to register to: ")))
               (vc-register (cons backend (cdr vc-fileset)))))
            (t
             (vc-register vc-fileset))))
     ((eq state 'missing)
      (vc-delete-file files))
     ;; Files are up-to-date, or need a merge and user specified a revision
     ((or (eq state 'up-to-date) (and verbose (eq state 'needs-update)))
      (cond
       (verbose
	;; Go to a different revision.
	(let* ((revision
                ;; FIXME: Provide completion.
                (read-string "Branch, revision, or backend to move to: "))
               (revision-downcase (downcase revision)))
	  (if (member
	       revision-downcase
	       (mapcar (lambda (arg) (downcase (symbol-name arg)))
                       vc-handled-backends))
	      (let ((vsym (intern-soft revision-downcase)))
		(dolist (file files) (vc-transfer-file file vsym)))
	    (dolist (file files)
              (vc-checkout file revision)))))
       ((not (eq model 'implicit))
	;; check the files out
	(dolist (file files) (vc-checkout file)))
       (t
        ;; do nothing
        (message "Fileset is up-to-date"))))
     ;; Files have local changes
     ((memq state '(added missing removed edited))
      (let ((ready-for-commit files))
	;; CVS, SVN and bzr don't care about read-only (bug#9781).
	;; RCS does, SCCS might (someone should check...).
	(when (memq backend '(RCS SCCS))
	  ;; If files are edited but read-only, give user a chance to correct.
	  (dolist (file files)
	    ;; If committing a mix of removed and edited files, the
	    ;; fileset has state = 'edited.  Rather than checking the
	    ;; state of each individual file in the fileset, it seems
	    ;; simplest to just check if the file exists.  Bug#9781.
	    (when (and (file-exists-p file) (not (file-writable-p file)))
	      ;; Make the file-buffer read-write.
	      (unless (y-or-n-p (format "%s is edited but read-only; make it writable and continue? " file))
		(error "Aborted"))
	      ;; Maybe we somehow lost permissions on the directory.
	      (condition-case nil
		  (set-file-modes file (logior (file-modes file) 128))
		(error (error "Unable to make file writable")))
	      (let ((visited (get-file-buffer file)))
		(when visited
		  (with-current-buffer visited
		    (read-only-mode -1)))))))
	;; Allow user to revert files with no changes
        ;; FIXME: This will never do anything because STATE will never
        ;; be `up-to-date' in this branch of the cond.
        ;; How did the code end up like this?  --spwhitton
	(save-excursion
          (let (to-revert)
            (dolist (file files)
              (let ((visited (get-file-buffer file)))
                ;; For files with locking, if the file does not contain
                ;; any changes, just let go of the lock, i.e. revert.
                (when (and (not (eq model 'implicit))
			   (eq state 'up-to-date)
			   ;; If buffer is modified, that means the user just
			   ;; said no to saving it; in that case, don't revert,
			   ;; because the user might intend to save after
			   ;; finishing the log entry and committing.
			   (not (and visited (buffer-modified-p visited))))
                  (push file to-revert))))
            (vc-revert-files backend to-revert)
            (setq ready-for-commit
                  (cl-nset-difference ready-for-commit to-revert))))
	;; Remaining files need to be committed
	(if (not ready-for-commit)
	    (message "No files remain to be committed")
          ;; In the case there actually are any unregistered files then
          ;; `vc-deduce-backend', via `vc-only-files-state-and-model',
          ;; has already prompted the user to approve registering them.
          ;;
          ;; FIXME: We should be able to use `vc-backend' instead of
          ;; `vc-registered' here given that `vc-deduce-backend' just
          ;; determined a state for all of the files.  However, there
          ;; are case(s) where the cached information is out-of-date.
          ;; For example, if we used C-x v v on a directory in *vc-dir*
          ;; and thereby newly registered files within that directory,
          ;; only that directory's name will have been passed to
          ;; `vc-register', and so `vc-backend' will still consider them
          ;; unregistered, even though `vc-dir-deduce-fileset' will
          ;; return `added' for their states.
	  (let ((register (cl-remove-if #'vc-registered fileset-only-files)))
            (if (not verbose)
	        (vc-checkin ready-for-commit backend nil nil nil nil register)
	      (let* ((revision (read-string "New revision or backend: "))
                     (revision-downcase (downcase revision)))
	        (if (member
		     revision-downcase
		     (mapcar (lambda (arg) (downcase (symbol-name arg)))
			     vc-handled-backends))
		    (let ((vsym (intern revision-downcase)))
		      (dolist (file files) (vc-transfer-file file vsym)))
		  (vc-checkin ready-for-commit backend
                              nil nil revision nil register))))))))
     ;; locked by somebody else (locking VCSes only)
     ((stringp state)
      ;; In the old days, we computed the revision once and used it on
      ;; the single file.  Then, for the 2007-2008 fileset rewrite, we
      ;; computed the revision once (incorrectly, using a free var) and
      ;; used it on all files.  To fix the free var bug, we can either
      ;; use `(car files)' or do what we do here: distribute the
      ;; revision computation among `files'.  Although this may be
      ;; tedious for those backends where a "revision" is a trans-file
      ;; concept, it is nonetheless correct for both those and (more
      ;; importantly) for those where "revision" is a per-file concept.
      ;; If the intersection of the former group and "locking VCSes" is
      ;; non-empty [I vaguely doubt it --ttn], we can reinstate the
      ;; pre-computation approach of yore.
      (dolist (file files)
        (vc-steal-lock
         file (if verbose
                  (read-string (format "%s revision to steal: " file))
                (vc-working-revision file))
         state)))
     ;; conflict
     ((eq state 'conflict)
      ;; FIXME: Is it really the UI we want to provide?
      ;; In my experience, the conflicted files should be marked as resolved
      ;; one-by-one when saving the file after resolving the conflicts.
      ;; I.e. stating explicitly that the conflicts are resolved is done
      ;; very rarely.
      (vc-mark-resolved backend files))
     ;; needs-update
     ((eq state 'needs-update)
      (dolist (file files)
	(if (yes-or-no-p (format
			  "%s is not up-to-date.  Get latest revision? "
			  (file-name-nondirectory file)))
	    (vc-checkout file t)
	  (when (and (not (eq model 'implicit))
		     (yes-or-no-p "Lock this revision? "))
	    (vc-checkout file)))))
     ;; needs-merge
     ((eq state 'needs-merge)
      (dolist (file files)
	(when (yes-or-no-p (format
			  "%s is not up-to-date.  Merge in changes now? "
			  (file-name-nondirectory file)))
	  (vc-maybe-resolve-conflicts
           file (vc-call-backend backend 'merge-news file)))))

     ;; unlocked-changes
     ((eq state 'unlocked-changes)
      (dolist (file files)
	(when (not (equal buffer-file-name file))
	  (find-file-other-window file))
	(if (save-window-excursion
	      (vc-diff-internal nil
				(cons (car vc-fileset)
                                      (cons (cadr vc-fileset) (list file)))
				(vc-working-revision file) nil)
	      (goto-char (point-min))
	      (let ((inhibit-read-only t))
		(insert
		 (format "Changes to %s since last lock:\n\n" file)))
	      (beep)
	      (yes-or-no-p (concat "File has unlocked changes.  "
				   "Claim lock retaining changes? ")))
	    (progn (vc-call-backend backend 'steal-lock file)
		   (clear-visited-file-modtime)
		   (write-file buffer-file-name)
		   (vc-mode-line file backend))
	  (if (not (yes-or-no-p
		    "Revert to checked-in revision, instead? "))
	      (error "Checkout aborted")
	    (vc-revert-buffer-internal t t)
	    (vc-checkout file)))))
     ;; Unknown fileset state
     (t
      (error "Fileset is in an unknown state %s" state)))))

(defun vc-create-repo (backend)
  "Create an empty repository in the current directory."
  (interactive (list (vc-read-backend "Create repository for: ")))
  (vc-call-backend backend 'create-repo))

;;;###autoload
(defun vc-register (&optional vc-fileset comment)
  "Register into a version control system.
If VC-FILESET is given, register the files in that fileset.
Otherwise register the current file.
If COMMENT is present, use that as an initial comment.

The version control system to use is found by cycling through the list
`vc-handled-backends'.  The first backend in that list which declares
itself responsible for the file (usually because other files in that
directory are already registered under that backend) will be used to
register the file.  If no backend declares itself responsible, the
first backend that could register the file is used."
  (interactive)
  (let* ((fileset-arg (or vc-fileset (vc-deduce-fileset nil t)))
         (backend (car fileset-arg))
	 (files (nth 1 fileset-arg)))
    ;; We used to operate on `only-files', but VC wants to provide the
    ;; possibility to register directories rather than files only, since
    ;; many VCS allow that as well.
    (dolist (fname files)
      (when (vc-call-backend backend 'registered fname)
	(error "This file is already registered: %s" fname))
      ;; Watch out for new buffers of size 0: the corresponding file
      ;; does not exist yet, even though buffer-modified-p is nil.
      (when-let* ((bname (get-file-buffer fname)))
	(with-current-buffer bname
	  (when (and (not (buffer-modified-p))
		     (zerop (buffer-size))
		     (not (file-exists-p buffer-file-name)))
	    (set-buffer-modified-p t))
	  (vc-buffer-sync))))
    (message "Registering %s... " files)
    (mapc #'vc-file-clearprops files)
    (vc-call-backend backend 'register files comment)
    (dolist (fname files)
      (vc-file-setprop fname 'vc-backend backend)
      (when-let* ((bname (get-file-buffer fname)))
        (with-current-buffer bname
          (unless vc-make-backup-files
            (setq-local backup-inhibited t))
          (when vc-auto-revert-mode
            (auto-revert-mode 1))))
      (vc-resynch-buffer fname t t))
    (message "Registering %s... done" files)))

(defun vc-register-with (backend)
  "Register the current file with a specified back end."
  (interactive "SBackend: ")
  (when (not (member backend vc-handled-backends))
    (error "Unknown back end"))
  (let ((vc-handled-backends (list backend)))
    (call-interactively 'vc-register)))

;;;###autoload
(defun vc-ignore (file &optional directory remove)
  "Ignore FILE under the VCS of DIRECTORY.

Normally, FILE is a wildcard specification that matches the files
to be ignored.  When REMOVE is non-nil, remove FILE from the list
of ignored files.

DIRECTORY defaults to `default-directory' and is used to
determine the responsible VC backend.

When called interactively, prompt for a FILE to ignore, unless a
prefix argument is given, in which case prompt for a file FILE to
remove from the list of ignored files."
  (interactive
   (let* ((rel-dir (vc--ignore-base-dir))
          (file (read-file-name "File to ignore: ")))
     (when (and (file-name-absolute-p file)
                (file-in-directory-p file rel-dir))
       (setq file (file-relative-name file rel-dir)))
     (list file
           rel-dir
           current-prefix-arg)))
  (let* ((directory (or directory default-directory))
	 (backend (or (vc-responsible-backend default-directory)
                      (error "Unknown backend"))))
    (vc-call-backend backend 'ignore file directory remove)))

(defun vc--ignore-base-dir ()
  (let ((backend (vc-responsible-backend default-directory)))
    (condition-case nil
        (file-name-directory
         (vc-call-backend backend 'find-ignore-file
                          default-directory))
      (vc-not-supported
       default-directory))))

(defun vc-default-ignore (backend file &optional directory remove)
  "Ignore FILE under DIRECTORY (default is `default-directory').
FILE is a wildcard specification relative to DIRECTORY.

When called from Lisp code, if DIRECTORY is non-nil, the
repository to use will be deduced by DIRECTORY.

If REMOVE is non-nil, remove FILE from ignored files instead.

Argument BACKEND is the backend to use."
  (let ((ignore
         (vc-call-backend backend
                          'find-ignore-file
                          (or directory default-directory))))
    (if remove
        (vc--remove-regexp (concat "^" (regexp-quote file) "\\(\n\\|$\\)") ignore)
      (vc--add-line file ignore))))

(defun vc-default-ignore-completion-table (backend file)
  "Return the list of ignored files under BACKEND."
  (cl-delete-if
   (lambda (str)
     ;; Commented or empty lines.
     (string-match-p "\\`\\(?:#\\|[ \t\r\n]*\\'\\)" str))
   (let ((file (vc-call-backend backend 'find-ignore-file file)))
     (and (file-exists-p file)
          (vc--read-lines file)))))

(defun vc--read-lines (file)
  "Return a list of lines of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

;; Subroutine for `vc-default-ignore'.
(defun vc--add-line (string file)
  "Add STRING as a line to FILE."
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (unless (re-search-forward (concat "^" (regexp-quote string) "$") nil t)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert string "\n")
      (save-buffer))))

(defun vc--remove-regexp (regexp file)
  "Remove all matching for REGEXP in FILE."
  (if (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (replace-match ""))
        (save-buffer))))

(defun vc-checkout (file &optional rev)
  "Retrieve a copy of the revision REV of FILE.
REV defaults to the latest revision.

After check-out, runs the normal hook `vc-checkout-hook'."
  (and (not rev)
       (vc-call make-version-backups-p file)
       (vc-up-to-date-p file)
       (vc-make-version-backup file))
  (let ((backend (or (bound-and-true-p vc-dir-backend) (vc-backend file))))
    (with-vc-properties (list file)
      (condition-case err
          (vc-call-backend backend 'checkout file rev)
        (file-error
         ;; Maybe the backend is not installed ;-(
         (when t
           (let ((buf (get-file-buffer file)))
             (when buf (with-current-buffer buf (read-only-mode -1)))))
         (signal (car err) (cdr err))))
      `((vc-state . ,(if (or (eq (vc-checkout-model backend (list file)) 'implicit)
                             nil)
			 'up-to-date
                       'edited))
        (vc-checkout-time . ,(file-attribute-modification-time
			      (file-attributes file))))))
  (vc-resynch-buffer file t t)
  (run-hooks 'vc-checkout-hook))

(defun vc-mark-resolved (backend files)
  (prog1 (with-vc-properties
	  files
	  (vc-call-backend backend 'mark-resolved files)
	  ;; FIXME: Is this TRTD?  Might not be.
	  `((vc-state . edited)))
    ;; Recompute mode lines.
    (dolist (file files)
      (vc-mode-line file backend))
    (message
     (substitute-command-keys
      "Conflicts have been resolved in %s.  \
Type \\[vc-next-action] to check in changes.")
     (if (> (length files) 1)
	 (format "%d files" (length files))
       "this file"))))

(declare-function mail-text "sendmail" ())
(declare-function message-goto-body "message" (&optional interactive))
(defun vc-steal-lock (file rev owner)
  "Steal the lock on FILE."
  (let (file-description)
    (if rev
	(setq file-description (format "%s:%s" file rev))
      (setq file-description file))
    (when (not (yes-or-no-p (format "Steal the lock on %s from %s? "
				    file-description owner)))
      (error "Steal canceled"))
    (message "Stealing lock on %s..." file)
    (with-vc-properties
     (list file)
     (vc-call steal-lock file rev)
     `((vc-state . edited)))
    (vc-resynch-buffer file t t)
    (message "Stealing lock on %s...done" file)
    ;; Write mail after actually stealing, because if the stealing
    ;; goes wrong, we don't want to send any mail.
    (compose-mail owner (format "Stolen lock on %s" file-description))
    (setq default-directory (expand-file-name "~/"))
    (cond
     ((eq mail-user-agent 'sendmail-user-agent)
      (mail-text))
     ((message-goto-body)))
    (insert
     (format "I stole the lock on %s, " file-description)
     (current-time-string)
     ".\n")
    (message
     (substitute-command-keys
      "Please explain why you stole the lock.  Type \\`C-c C-c' when done"))))

(defalias 'vc-default-async-checkins #'ignore)

(defun vc-checkin
    (files backend &optional comment initial-contents rev patch-string register)
  "Check in FILES.

There are three calling conventions for the COMMENT and INITIAL-CONTENTS
optional arguments:
- COMMENT a string, INITIAL-CONTENTS nil means use that comment string
  without prompting the user to edit it.
- COMMENT a string, INITIAL-CONTENTS non-nil means use that comment
  string as the initial contents of the log entry buffer but stop for
  editing.
- COMMENT t means check in immediately with an empty comment, and ignore
  INITIAL-CONTENTS.

The optional argument REV may be a string specifying the new revision
level (only supported for some older VCSes, like RCS and CVS).
The optional argument PATCH-STRING is a string to check in as a patch.
If the optional argument REGISTER is non-nil, it should be a list of
files to register before checking in; if any of these are already
registered the checkin will abort.

Runs the normal hooks `vc-before-checkin-hook' and `vc-checkin-hook'."
  (run-hooks 'vc-before-checkin-hook)
  (let ((do-async (and vc-async-checkin
                       (vc-call-backend backend 'async-checkins))))
   (vc-start-logentry
    files comment initial-contents
    "Enter a change comment."
    "*vc-log*"
    (lambda ()
      (vc-call-backend backend 'log-edit-mode))
    (lambda (files comment)
      ;; Check the user isn't likely to be surprised by what is included
      ;; in the checkin.  Once a log operation is started, the fileset
      ;; or patch string is locked in.  In particular, it's probably too
      ;; late to offer to change it now -- checks in hooks and/or the
      ;; backend's Log Edit derived mode have all already okayed the
      ;; checkin.  Restarting with the new fileset or patch is easy.
      (let* ((start-again
              (substitute-command-keys "\\[vc-next-action] to check in again"))
             (instructions
              (substitute-command-keys
               (string-join
                (list "type \\<log-edit-mode-map>\\[log-edit-kill-buffer] to cancel"
                      start-again
                      "\\[log-edit-previous-comment] to recall your message")
                ", "))))
        (cond (patch-string
               (unless (or (not (derived-mode-p 'diff-mode))
                           (equal patch-string (buffer-string))
                           (yes-or-no-p
                            (format-message "Patch in buffer \"%s\" \
has changed; continue with old patch?" (current-buffer))))
                 (user-error "%s %s"
                             "To check in the new patch" instructions)))
              ((vc-dispatcher-browsing)
               (unless (or (and (length= files 1)
                                ;; If no files in the dispatcher were
                                ;; marked and it was just that point
                                ;; moved to a different line, we don't
                                ;; want to bother the user.  This isn't
                                ;; foolproof because we don't know
                                ;; whether FILES was selected by means
                                ;; of marking a single file or the
                                ;; implicit selection of the file at
                                ;; point in the absence of any marks.
                                (not (vc-dispatcher--explicit-marks-p)))
                           (equal files (cadr (vc-deduce-fileset)))
                           (yes-or-no-p
                            (format-message "Selected file(s) in buffer \"%s\" \
have changed; continue with old fileset?" (current-buffer))))
                 (user-error "%s %s"
                             "To use the new fileset" instructions)))))

      ;; "This log message intentionally left almost blank".
      ;; RCS 5.7 gripes about whitespace-only comments too.
      (unless (and comment (string-match "[^\t\n ]" comment))
        (setq comment "*** empty log message ***"))
      (unless patch-string
        ;; Must not pass non-nil NOT-ESSENTIAL because we will shortly
        ;; call (in `vc-finish-logentry') `vc-resynch-buffer' with its
        ;; NOQUERY parameter non-nil.
        (vc-buffer-sync-fileset (list backend files)))
      (when register (vc-register (list backend register)))
      (cl-flet ((do-it ()
                  ;; We used to change buffers to get local value of
                  ;; `vc-checkin-switches', but the (singular) local
                  ;; buffer is not well defined for filesets.
                  (prog1 (if patch-string
                             (vc-call-backend backend 'checkin-patch
                                              patch-string comment)
                           (vc-call-backend backend 'checkin
                                            files comment rev))
                    (mapc #'vc-delete-automatic-version-backups files)))
                (done-msg ()
                  (message "Checking in %s...done" (vc-delistify files))))
        (if do-async
            ;; Rely on `vc-set-async-update' to update properties.
            (let ((ret (do-it)))
              (when (eq (car-safe ret) 'async)
                (vc-exec-after #'done-msg nil (cadr ret)))
              ret)
          (prog2 (message "Checking in %s..." (vc-delistify files))
              (with-vc-properties files (do-it)
                                  `((vc-state . up-to-date)
                                    (vc-checkout-time
                                     . ,(file-attribute-modification-time
			                 (file-attributes file)))
                                    (vc-working-revision . nil)))
            (done-msg)))))
    'vc-checkin-hook
    backend
    patch-string)))

(declare-function diff-buffer-file-names "diff-mode")
(declare-function diff-reverse-direction "diff-mode")

(defun vc--pick-or-revert
    (rev reverse interactive delete comment initial-contents backend)
  "Copy a single revision REV to branch checked out in this working tree.

REVERSE non-nil means to undo the effects of REV, instead.
This is affected by whether the VCS is centralized or distributed and
the INTERACTIVE and DELETE arguments, as follows:
- For a centralized VCS for which Emacs knows how to do true undos, then
  unless DELETE is the special value `never', do a true undo of REV.
  This function supports creating new commits undoing the effects of REV
  for even a centralized VCS with true undos by passing `never' as
  DELETE (as `vc-revert-revision' does).
  For centralized VCS, INTERACTIVE is ignored.
- For a distributed VCS, when INTERACTIVE is non-nil, DELETE is nil, and
  REV has not yet been pushed, offer to delete REV entirely instead of
  creating a new commit undoing its EFFECTS.
  If INTERACTIVE is `no-confirm', don't prompt to confirm the deletion.
- For a distributed VCS, when DELETE is non-nil (but not `never'), only
  consider deleting REV, never create a new commit, but subject to
  `vc-allow-rewriting-published-history'.
  In this case INTERACTIVE is ignored.
(This complex calling convention makes for simple usage of this
workhorse function from the frontend VC commands that provide access to
all this functionality.)

COMMENT is a comment string; if omitted, a buffer is popped up to accept
a comment.  If INITIAL-CONTENTS is non-nil, then COMMENT is used as the
initial contents of the log entry buffer.  If COMMENT is t then use
BACKEND's default cherry-pick comment for REV without prompting.
BACKEND is the VC backend to use.

Return `deleted' if we actually undid/deleted a commit.
Any other return value means we called `vc-start-logentry'."
  (cond*
   ((bind* (backend (or backend
                        (vc-responsible-backend default-directory)))))
   ((and reverse (not (eq delete 'never))
         (null (vc-find-backend-function backend
                                         'revision-published-p))
         (vc-find-backend-function backend 'delete-revision))
    ;; Centralized VCS implementing `delete-revision'.
    (vc-call-backend backend 'delete-revision rev)
    'deleted)
   ((and reverse interactive (not delete)
         ;; Distributed VCS for which we can do deletions.
         (vc-find-backend-function backend 'revision-published-p)
         (vc-find-backend-function backend 'delete-revision)
         ;; REV is safe to delete.
         (not (vc-call-backend backend 'revision-published-p rev)))
    ;; Require confirmation, because the commit is unpublished, and so
    ;; this might be the only copy of the work in REV.  Don't fall back
    ;; to making a new commit undoing REV's changes because we don't
    ;; know the user wants that just because they said "no" to our
    ;; question here, and we want to avoid two y/n prompts in a row,
    ;; which is probably a less good UI than this.
    (cond ((or (eq interactive 'no-confirm)
               (yes-or-no-p
                (format "Permanently delete %s from the revision history?"
                        rev)))
           (vc-call-backend backend 'delete-revision rev)
           'deleted)
          ((derived-mode-p 'log-view-mode)
           (user-error (substitute-command-keys "\
Use \\[log-view-revert-revisions] to create new commits \
undoing changes made by revision(s)")))
          (t
           (user-error (substitute-command-keys "\
Use \\[vc-revert-revision] to create a new commit undoing %s's changes")
                       rev))))
   ((and reverse delete (not (eq delete 'never))
         ;; Distributed VCS for which we can do deletions.
         (vc-find-backend-function backend 'revision-published-p)
         (vc-find-backend-function backend 'delete-revision))
    ;; Even though the user has explicitly requested deletion with a
    ;; prefix argument / invoking `vc-delete-revision' / invoking
    ;; `log-view-delete-revisions', by default we still confirm such a
    ;; destructive operation.
    ;; However, we want to avoid prompting twice in the case that the
    ;; user has set `vc-allow-rewriting-published-history' to `ask', and
    ;; we should avoid prompting at all in the case that
    ;; `vc-allow-rewriting-published-history' is another non-nil value.
    ;; These requirements lead to the nested `cond*' form here.
    (cond*
     ((and vc-allow-rewriting-published-history
           (not (eq vc-allow-rewriting-published-history 'ask)))
      (vc-call-backend backend 'delete-revision rev)
      'deleted)
     ((bind* (published (vc-call-backend backend 'revision-published-p rev))))
     ((and published
           (eq vc-allow-rewriting-published-history 'ask)
           (yes-or-no-p
            (format "Revision %s appears published; allow rewriting history?"
                    rev)))
      (vc-call-backend backend 'delete-revision rev)
      'deleted)
     (published
      (user-error "Will not rewrite likely-public history"))
     ((yes-or-no-p
       (format "Permanently delete %s from the revision history?"
               rev))
      (vc-call-backend backend 'delete-revision rev)
      'deleted)
     (t
      (user-error "Aborted"))))
   ;; If we get this far we give up on `delete-revision', i.e. we fall
   ;; back to creating a commit undoing the effects of REV.
   ;;
   ;; `vc-*-prepare-patch' will always give us a patch with file names
   ;; relative to the VC root, so switch to there now.  In particular
   ;; this is needed for `diff-buffer-file-names' to work properly.
   ((bind* (default-directory (vc-call-backend backend 'root
                                               default-directory))
           (patch (vc-call-backend backend 'prepare-patch rev))
           files whole-patch-string diff-patch-string))
   (t
    (with-current-buffer (plist-get patch :buffer)
      (diff-mode)
      (with-restriction
          (or (plist-get patch :patch-start) (point-min))
          (or (plist-get patch :patch-end) (point-max))
        (when reverse
          (diff-reverse-direction (point-min) (point-max)))
        (setq files (diff-buffer-file-names nil t)
              diff-patch-string (buffer-string)))
      ;; In the case of reverting we mustn't copy the original
      ;; authorship information.  The author of the revert is the
      ;; current user, and its timestamp is now.
      (setq whole-patch-string
            (if reverse diff-patch-string (buffer-string))))
    (unless (stringp comment)
      (cl-psetq comment (vc-call-backend backend 'cherry-pick-comment
                                         files rev reverse)
                initial-contents (not (eq comment t))))
    (vc-start-logentry files comment initial-contents
                       (format "Edit log message for %s revision."
                               (if reverse
                                   "new"
                                 ;; ^ "reverted revision" would mean
                                 ;;   REV, not the revision we are about
                                 ;;   to create.  We could use
                                 ;;   "reverting revision" but it reads
                                 ;;   oddly.
                                 "copied"))
                       "*vc-cherry-pick*"
                       (lambda ()
                         (vc-call-backend backend 'log-edit-mode))
                       (lambda (_files comment)
                         (vc-call-backend backend 'checkin-patch
                                          whole-patch-string comment))
                       nil
                       backend
                       diff-patch-string))))

;; No bindings in `vc-prefix-map' for the following three items because
;; we expect users will usually use `log-view-cherry-pick' and
;; `log-view-revert-or-delete-revisions', which do have bindings.

;;;###autoload
(defun vc-cherry-pick (rev &optional comment initial-contents backend)
  "Copy the changes from a single revision REV to the current branch.
When called interactively, prompts for REV.
Typically REV is a revision from another branch, where that branch is
one that will not be merged into the branch checked out in this working
tree.

Normally a log message for the new commit is generated by the backend
and includes a reference to REV so that the copy can be traced.
When called interactively with a prefix argument, use REV's log message
unmodified, and also skip editing it.

When called from Lisp, there are three calling conventions for the
COMMENT and INITIAL-CONTENTS optional arguments:
- COMMENT a string, INITIAL-CONTENTS nil means use that comment string
  without prompting the user to edit it.
- COMMENT a string, INITIAL-CONTENTS non-nil means use that comment
  string as the initial contents of the log entry buffer but stop for
  editing.
- COMMENT t means use BACKEND's default cherry-pick comment for REV
  without prompting for editing, and ignore INITIAL-CONTENTS.

Optional argument BACKEND is the VC backend to use."
  (interactive (let ((rev (vc-read-revision "Revision to copy: "))
                     (backend (vc-responsible-backend default-directory)))
                 (list rev
                       (and current-prefix-arg
                            (vc-call-backend backend 'get-change-comment
                                             nil rev))
                       nil
                       backend)))
  (vc--pick-or-revert rev nil nil nil comment initial-contents backend))

;;;###autoload
(defun vc-revert-or-delete-revision
    (rev &optional interactive delete comment initial-contents backend)
  "Undo the effects of revision REV.
When called interactively, prompts for REV.

When called interactively (or with optional argument INTERACTIVE
non-nil), then if the underlying VCS is distributed and REV has not been
pushed, offer to entirely delete REV.
This is instead of creating a new commit undoing the effects of REV.

With a prefix argument (or with optional argument DELETE non-nil),
only consider deleting REV, never create a new commit.
In this case INTERACTIVE is ignored.
This works only if REV has not been pushed, unless you have customized
`vc-allow-rewriting-published-history' to a non-nil value.

When called from Lisp, there are three calling conventions for the
COMMENT and INITIAL-CONTENTS optional arguments:
- COMMENT a string, INITIAL-CONTENTS nil means use that comment string
  without prompting the user to edit it.
- COMMENT a string, INITIAL-CONTENTS non-nil means use that comment
  string as the initial contents of the log entry buffer but stop for
  editing.
- COMMENT t means use BACKEND's default revert comment for REV without
  prompting for editing, and ignore INITIAL-CONTENTS.

Optional argument BACKEND is the VC backend to use.

See also `vc-revert-revision'."
  (interactive (list (vc-read-revision (if current-prefix-arg
                                           "Revision to delete: "
                                         "Revision to revert: "))
                     t current-prefix-arg))
  (vc--pick-or-revert rev t interactive delete
                      comment initial-contents backend))

;;;###autoload
(defun vc-revert-revision
  (rev &optional comment initial-contents backend)
  "Make a commit undoing the effects of revision REV.
When called interactively, prompts for REV.

This is like `vc-revert-or-delete-revision' except that it only ever
makes a new commit undoing the effects of REV, instead of considering
VCS-specific alternative mechanisms to undo the effects of REV.

When called from Lisp, there are three calling conventions for the
COMMENT and INITIAL-CONTENTS optional arguments:
- COMMENT a string, INITIAL-CONTENTS nil means use that comment string
  without prompting the user to edit it.
- COMMENT a string, INITIAL-CONTENTS non-nil means use that comment
  string as the initial contents of the log entry buffer but stop for
  editing.
- COMMENT t means use BACKEND's default revert comment for REV without
  prompting for editing, and ignore INITIAL-CONTENTS.

Optional argument BACKEND is the VC backend to use."
  (interactive (list (vc-read-revision "Revision to revert: ")))
  (vc--pick-or-revert rev t nil 'never comment initial-contents backend))

;;;###autoload
(defun vc-delete-revision (rev &optional backend)
  "Delete revision REV from the revision history.
This works only if REV has not been pushed, unless you have customized
`vc-allow-rewriting-published-history' to a non-nil value.

This is the same as `vc-revert-or-delete-revision' invoked interactively
with a prefix argument."
  (interactive (list (vc-read-revision "Revision to delete: ")))
  (vc--pick-or-revert rev t nil t nil nil backend))

(defun vc--remove-revisions-from-end (rev delete prompt backend)
  "Delete revisions newer than REV.
DELETE non-nil means to remove the changes from the working tree.
DELETE `discard' means to silently discard uncommitted changes.
PROMPT non-nil means to always get confirmation.  (This is passed by
`log-view-uncommit-revisions-from-end' and `log-view-delete-revisions'
because they have single-letter bindings and don't otherwise prompt, so
might be easy to use accidentally.)
BACKEND is the VC backend."
  (let ((backend (or backend (vc-responsible-backend default-directory))))
    (unless (eq (vc-call-backend backend 'revision-granularity)
                'repository)
      (error "Requires VCS with whole-repository revision granularity"))
    (unless (vc-find-backend-function backend 'revision-published-p)
      (signal 'vc-not-supported (list 'revision-published-p backend)))
    ;; Rewinding the end of the branch to REV does not in itself mean
    ;; rewriting public history because a subsequent pull will generally
    ;; undo the rewinding.  Rewinding and then making new commits before
    ;; syncing with the upstream will necessitate merging, but that's
    ;; just part of the normal workflow with a distributed VCS.
    ;; Therefore we don't prompt about deleting published revisions (and
    ;; so we ignore `vc-allow-rewriting-published-history').
    ;; We do care about deleting *unpublished* revisions, however,
    ;; because that could potentially mean losing work permanently.
    (when (if (vc-call-backend backend 'revision-published-p
                               (vc-call-backend backend
                                                'working-revision-symbol))
              (and prompt
                   (not (y-or-n-p
                         (format "Uncommit revisions newer than %s?"
                                 rev))))
            ;; FIXME: Actually potentially not all revisions newer than
            ;; REV would be permanently deleted -- only those which are
            ;; unpushed.  So this prompt is a little misleading.
            (not (yes-or-no-p
                  (format "Permanently delete revisions newer than %s?"
                          rev))))
      (user-error "Aborted"))
    (if delete
        ;; FIXME: As discussed in bug#79408, instead of just failing if
        ;; the user declines reverting the changes, we would leave
        ;; behind some sort of conflict for the user to resolve, like we
        ;; do when there is a merge conflict.
        (let ((root (vc-root-dir)))
          (when (vc-dir-status-files root nil backend)
            (if (eq delete 'discard)
                (vc-revert-file root)
              (let ((vc-buffer-overriding-fileset `(,backend (,root))))
                (vc-revert))))
          (vc-call-backend backend 'delete-revisions-from-end rev))
      (vc-call-backend backend 'uncommit-revisions-from-end rev))))

;;;###autoload
(defun vc-uncommit-revisions-from-end (rev &optional backend)
  "Delete revisions newer than REV without touching the working tree.
REV must be on the current branch.  The newer revisions are deleted from
the revision history but the changes made by those revisions to files in
the working tree are not undone.
When called interactively, prompts for REV.
BACKEND is the VC backend.

To delete revisions from the revision history and also undo the changes
in the working tree, see `vc-delete-revisions-from-end'."
  (interactive (list
                (vc-read-revision "Uncommit revisions newer than revision: ")))
  (vc--remove-revisions-from-end rev nil nil backend))

;;;###autoload
(defun vc-delete-revisions-from-end (rev &optional discard backend)
  "Delete revisions newer than REV.
REV must be on the current branch.  The newer revisions are deleted from
the revision history and the changes made by those revisions to files in
the working tree are undone.
When called interactively, prompts for REV.
If the are uncommitted changes, prompts to discard them.
With a prefix argument (when called from Lisp, with optional argument
DISCARD non-nil), discard any uncommitted changes without prompting.
BACKEND is the VC backend.

To delete revisions from the revision history without undoing the
changes in the working tree, see `vc-uncommit-revisions-from-end'."
  (interactive (list
                (vc-read-revision "Delete revisions newer than revision: ")
                current-prefix-arg))
  (vc--remove-revisions-from-end rev (if discard 'discard t) nil backend))

(declare-function diff-bounds-of-hunk "diff-mode")

(defun vc-default-checkin-patch (_backend patch-string comment)
  (pcase-let* ((`(,backend ,files)
                (with-temp-buffer
                  (diff-mode)
                  (insert patch-string)
                  (goto-char (point-min))
                  (when (and (re-search-forward
                              "^\\(?:Date\\|From\\|Author\\):[\t\s]*[^\t\n\s]"
                              (car (diff-bounds-of-hunk))
                              t)
                             (not (yes-or-no-p "Patch appears to contain \
authorship information but this will be ignored when checking in; \
proceed anyway?")))
                    (user-error "Aborted"))
                  (diff-vc-deduce-fileset)))
               (tmpdir (make-temp-file "vc-checkin-patch" t)))
    (dolist (f files)
      (make-directory (file-name-directory (expand-file-name f tmpdir)) t)
      (copy-file (expand-file-name f)
                 (expand-file-name f tmpdir)))
    (unwind-protect
        (progn
          (vc-revert-files backend
                           (mapcar (lambda (f)
                                     (with-current-buffer (find-file-noselect f)
                                       buffer-file-name))
                                   files))
          (with-temp-buffer
            ;; Trying to support CVS too.  Assuming that vc-diff
            ;; there will usually have diff root in default-directory.
            (when (vc-find-backend-function backend 'root)
              (setq-local default-directory
                          (vc-call-backend backend 'root (car files))))
            (unless (eq 0
                        (call-process-region patch-string
                                             nil
                                             "patch"
                                             nil
                                             t
                                             nil
                                             "-p1"
                                             "-r" null-device
                                             "--posix"
                                             "--remove-empty-files"
                                             "-i" "-"))
              (user-error "Patch failed: %s" (buffer-string))))
          (vc-call-backend backend 'checkin files comment))
      (dolist (f files)
        (copy-file (expand-file-name f tmpdir)
                   (expand-file-name f)
                   t)
        (with-current-buffer (get-file-buffer f)
          (revert-buffer t t t)))
      (delete-directory tmpdir t))))

;;; Additional entry points for examining version histories

;; (defun vc-default-diff-tree (backend dir rev1 rev2)
;;   "List differences for all registered files at and below DIR.
;; The meaning of REV1 and REV2 is the same as for `vc-revision-diff'."
;;   ;; This implementation does an explicit tree walk, and calls
;;   ;; vc-BACKEND-diff directly for each file.  An optimization
;;   ;; would be to use `vc-diff-internal', so that diffs can be local,
;;   ;; and to call it only for files that are actually changed.
;;   ;; However, this is expensive for some backends, and so it is left
;;   ;; to backend-specific implementations.
;;   (setq default-directory dir)
;;   (vc-file-tree-walk
;;    default-directory
;;    (lambda (f)
;;      (vc-run-delayed
;;       (let ((coding-system-for-read (vc-coding-system-for-diff f)))
;;          (message "Looking at %s" f)
;;          (vc-call-backend (vc-backend f)
;;                           'diff (list f) rev1 rev2))))))

(defvar vc-coding-system-inherit-eol t
  "When non-nil, inherit the EOL format for reading Diff output from the file.

Used in `vc-coding-system-for-diff' to determine the EOL format to use
for reading Diff output for a file.  If non-nil, the EOL format is
inherited from the file itself.
Set this variable to nil if your Diff tool might use a different
EOL.  Then Emacs will auto-detect the EOL format in Diff output, which
gives better results.") ;; Cf. bug#4451.

(defun vc-coding-system-for-diff (file)
  "Return the coding system for reading diff output for FILE."
  (or coding-system-for-read
      ;; if we already have this file open,
      ;; use the buffer's coding system
      (let ((buf (find-buffer-visiting file)))
        (when buf (with-current-buffer buf
		    (if vc-coding-system-inherit-eol
			buffer-file-coding-system
		      ;; Don't inherit the EOL part of the coding-system,
		      ;; because some Diff tools may choose to use
		      ;; a different one.  bug#4451.
		      (coding-system-base buffer-file-coding-system)))))
      ;; otherwise, try to find one based on the file name
      (car (find-operation-coding-system 'insert-file-contents file))
      ;; and a final fallback
      'undecided))

(defun vc-switches (backend op)
  "Return a list of vc-BACKEND switches for operation OP.
BACKEND is a symbol such as `CVS', which will be downcased.
OP is a symbol such as `diff'.

In decreasing order of preference, return the value of:
vc-BACKEND-OP-switches (e.g. `vc-cvs-diff-switches');
vc-OP-switches (e.g. `vc-diff-switches'); or, in the case of
diff only, `diff-switches'.

If the chosen value is not a string or a list, return nil.
This is so that you may set, e.g. `vc-svn-diff-switches' to t in order
to override the value of `vc-diff-switches' and `diff-switches'."
  (let ((switches
	 (or (when backend
	       (let ((sym (vc-make-backend-sym
			   backend (intern (concat (symbol-name op)
						   "-switches")))))
		   (when (boundp sym) (symbol-value sym))))
	     (let ((sym (intern (format "vc-%s-switches" (symbol-name op)))))
	       (when (boundp sym) (symbol-value sym)))
	     (cond
	      ((eq op 'diff) diff-switches)))))
    (if (stringp switches) (list switches)
      ;; If not a list, return nil.
      ;; This is so we can set vc-diff-switches to t to override
      ;; any switches in diff-switches.
      (when (listp switches) switches))))

(defun vc-shrink-buffer-window (&optional buffer)
  "Call `shrink-window-if-larger-than-buffer' only when BUFFER is visible.
BUFFER defaults to the current buffer."
  (let ((window (get-buffer-window buffer t)))
    (when window
      (shrink-window-if-larger-than-buffer window))))

(defvar vc-diff-finish-functions '(vc-shrink-buffer-window)
  "Functions run at the end of the diff command.
Each function runs in the diff output buffer without args.")

(defun vc-diff-restore-buffer (original new)
  "Restore point in buffer NEW to where it was in ORIGINAL.

This function works by updating buffer ORIGINAL with the contents
of NEW (without destroying existing markers), swapping their text
objects, and finally killing buffer ORIGINAL."
  (with-current-buffer original
    (let ((inhibit-read-only t))
      (replace-region-contents (point-min) (point-max) new)))
  (with-current-buffer new
    (buffer-swap-text original))
  (kill-buffer original))

(defun vc-diff-finish (buffer messages &optional oldbuf)
  ;; The empty sync output case has already been handled, so the only
  ;; possibility of an empty output is for an async process.
  (when (buffer-live-p buffer)
    (let ((emptyp (zerop (buffer-size buffer))))
      (with-current-buffer buffer
	(and messages emptyp
	     (let ((inhibit-read-only t))
	       (insert (cdr messages) ".\n")
	       (message "%s" (cdr messages))))
	(diff-setup-whitespace)
	(diff-setup-buffer-type)
        ;; `oldbuf' is the buffer that used to show this diff.  Make
        ;; sure that we restore point in it if it's given.
	(if oldbuf
            (vc-diff-restore-buffer oldbuf buffer)
          (goto-char (point-min)))
	(run-hooks 'vc-diff-finish-functions))
      (when (and messages (not emptyp))
	(message "%sdone" (car messages))))))

(defvar vc-diff-added-files nil
  "If non-nil, diff added files by comparing them to /dev/null.")

(defvar vc-patch-string nil)

(defun vc-diff-patch-string (patch-string)
  "Report diffs to be committed from the patch.
Like `vc-diff-internal' but uses PATCH-STRING to display
in the output buffer."
  (let ((buffer "*vc-diff*"))
    (vc-setup-buffer buffer)
    (let ((buffer-undo-list t)
          (inhibit-read-only t))
      (insert patch-string))
    (setq buffer-read-only t)
    (diff-mode)
    (setq-local diff-vc-backend (vc-responsible-backend default-directory))
    (setq-local revert-buffer-function
                (lambda (_ _) (vc-diff-patch-string patch-string)))
    (setq-local vc-patch-string patch-string)
    (pop-to-buffer (current-buffer))
    (vc-run-delayed (vc-diff-finish (current-buffer) nil))))

(defun vc-diff-internal (async vc-fileset rev1 rev2 &optional verbose buffer)
  "Report diffs between revisions REV1 and REV2 of a fileset in VC-FILESET.
ASYNC non-nil means run the backend's commands asynchronously if possible.
VC-FILESET should have the format described in `vc-deduce-fileset'.
Output goes to the buffer BUFFER, which defaults to *vc-diff*.
BUFFER, if non-nil, should be a buffer or a buffer name.
Return t if the buffer had changes, nil otherwise."
  (unless buffer
    (setq buffer (get-buffer-create "*vc-diff*")))
  (let* ((files (cadr vc-fileset))
	 (messages (cons (format "Finding changes in %s..."
                                 (vc-delistify files))
                         (format "No changes between %s and %s"
                                 (or rev1 "working revision")
                                 (or rev2 "workfile"))))
	 ;; Set coding system based on the first file.  It's a kluge,
	 ;; but the only way to set it for each file included would
	 ;; be to call the back end separately for each file.
	 (coding-system-for-read
          ;; Force the EOL conversion to be -unix, in case the files
          ;; to be compared have DOS EOLs.  In that case, EOL
          ;; conversion will produce a patch file that will either
          ;; fail to apply, or will change the EOL format of some of
          ;; the lines in the patched file.
          (coding-system-change-eol-conversion
	   (if files (vc-coding-system-for-diff (car files)) 'undecided)
           'unix))
         (orig-diff-buffer-clone
          (if revert-buffer-in-progress
              (clone-buffer
               (generate-new-buffer-name " *vc-diff-clone*") nil))))
    ;; On MS-Windows and MS-DOS, Diff is likely to produce DOS-style
    ;; EOLs, which will look ugly if (car files) happens to have Unix
    ;; EOLs.  But for Git, we must force Unix EOLs in the diffs, since
    ;; Git always produces Unix EOLs in the parts that didn't come
    ;; from the file, and wants to see any CR characters when applying
    ;; patches.
    (if (and (memq system-type '(windows-nt ms-dos))
             (not (eq (car vc-fileset) 'Git)))
	(setq coding-system-for-read
	      (coding-system-change-eol-conversion coding-system-for-read
						   'dos)))
    (vc-setup-buffer buffer)
    (message "%s" (car messages))
    ;; Many backends don't handle well the case of a file that has been
    ;; added but not yet committed to the repo (notably CVS and Subversion).
    ;; Do that work here so the backends don't have to futz with it.  --ESR
    ;;
    ;; Actually most backends (including CVS) have options to control the
    ;; behavior since which one is better depends on the user and on the
    ;; situation).  Worse yet: this code does not handle the case where
    ;; `file' is a directory which contains added files.
    ;; I made it conditional on vc-diff-added-files but it should probably
    ;; just be removed (or copied/moved to specific backends).  --Stef.
    (when vc-diff-added-files
      (let ((filtered '())
	    process-file-side-effects)
        (dolist (file files)
          (if (or (file-directory-p file)
                  (not (string= (vc-working-revision file) "0")))
              (push file filtered)
            ;; This file is added but not yet committed;
            ;; there is no repository version to diff against.
            (if (or rev1 rev2)
                (error "No revisions of %s exist" file)
              ;; We regard this as "changed".
              ;; Diff it against /dev/null.
              (apply #'vc-do-command buffer
                     (if async 'async 1) "diff" file
                     (append (vc-switches nil 'diff) `(,(null-device)))))))
        (setq files (nreverse filtered))))
    (with-current-buffer buffer
      ;; Make the *vc-diff* buffer read only, the diff-mode key
      ;; bindings are nicer for read only buffers. pcl-cvs does the
      ;; same thing.
      (setq buffer-read-only t)
      ;; Set the major mode and some local variables before calling into
      ;; the backend.  This means that the backend can itself set local
      ;; variables and enable minor modes in BUFFER if it wants to.
      ;; Call into the backend with the old current buffer, though, so
      ;; that its operation can be influenced by local variables in that
      ;; buffer (some discussion in bug#80005).
      (diff-mode)
      (setq-local diff-vc-backend (car vc-fileset))
      (setq-local diff-vc-revisions (list rev1 rev2))
      (setq-local revert-buffer-function
                  (lambda (_ignore-auto _noconfirm)
                    (vc-diff-internal async vc-fileset rev1 rev2 verbose))))
    (vc-call-backend (car vc-fileset) 'diff files rev1 rev2 buffer async)
    (set-buffer buffer)
    (if (and (zerop (buffer-size))
             (not (get-buffer-process (current-buffer))))
        ;; Treat this case specially so as not to pop the buffer.
        (progn
          (message "%s" (cdr messages))
          nil)
      ;; Display the buffer, but at the end because it can change point.
      (pop-to-buffer (current-buffer))
      ;; The diff process may finish early, so call `vc-diff-finish'
      ;; after `pop-to-buffer'; the former assumes the diff buffer is
      ;; shown in some window.
      (let ((buf (current-buffer)))
        (vc-run-delayed (vc-diff-finish buf (when verbose messages)
                                        orig-diff-buffer-clone)))
      ;; In the async case, we return t even if there are no differences
      ;; because we don't know that yet.
      t)))

(defvar vc-revision-history nil
  "History for `vc-read-revision'.")

(defun vc-read-revision (prompt &optional files backend default initial-input multiple)
  "Query the user for a revision using PROMPT.
All subsequent arguments are optional.  FILES may specify a file
set to restrict the revisions to.  BACKEND is a VC backend as
listed in `vc-handled-backends'.  DEFAULT and INITIAL-INPUT are
handled as defined by `completing-read'.  If MULTIPLE is non-nil,
the user may be prompted for multiple revisions.  If possible
this means that `completing-read-multiple' will be used."
  (cond
   ((null files)
    (let ((vc-fileset (vc-deduce-fileset t))) ;FIXME: why t?  --Stef
      (setq files (cadr vc-fileset))
      (setq backend (car vc-fileset))))
   ((null backend) (setq backend (vc-backend (car files)))))
  ;; Override any `vc-filter-command-function' value, as user probably
  ;; doesn't want to edit the command to get the completions.
  (let* ((vc-filter-command-function #'list)
         (completion-table
          (vc-call-backend backend 'revision-completion-table files)))
    (if completion-table
        (funcall
         (if multiple #'completing-read-multiple #'completing-read)
         prompt completion-table nil nil initial-input 'vc-revision-history default)
      (let ((answer (read-string prompt initial-input nil default)))
        (if multiple
            (split-string answer "[ \t]*,[ \t]*")
          answer)))))

(defun vc-read-multiple-revisions (prompt &optional files backend default initial-input)
  "Query the user for multiple revisions.
This is equivalent to invoking `vc-read-revision' with t for
MULTIPLE.  The arguments PROMPT, FILES, BACKEND, DEFAULT and
INITIAL-INPUT are passed on to `vc-read-revision' directly."
  (vc-read-revision prompt files backend default initial-input t))

(defun vc-diff-build-argument-list-internal (&optional fileset)
  "Build argument list for calling internal diff functions."
  (let* ((vc-fileset (or fileset (vc-deduce-fileset t))) ;FIXME: why t?  --Stef
         (files (cadr vc-fileset))
         (backend (car vc-fileset))
         (first (car files))
         (rev1-default nil)
         ) ;; (rev2-default nil)
    (cond
     ;; someday we may be able to do revision completion on non-singleton
     ;; filesets, but not yet.
     ((/= (length files) 1)
      nil)
     ;; if the file is not locked, use previous revision and current source as defaults
     (t
      (push (ignore-errors         ;If `previous-revision' doesn't work.
              (vc-call-backend backend 'previous-revision first
                               (vc-symbolic-working-revision first backend)))
            rev1-default)
      (when (member (car rev1-default) '("" nil)) (setq rev1-default nil))))
    ;; construct argument list
    (let* ((rev1-prompt (format-prompt "Older revision" (car rev1-default)))
           (rev2-prompt (format-prompt "Newer revision"
                                       ;; (or rev2-default
                                       "current source"))
           (rev1 (vc-read-revision rev1-prompt files backend rev1-default))
           (rev2 (vc-read-revision rev2-prompt files backend nil))) ;; rev2-default
      (when (string= rev1 "") (setq rev1 nil))
      (when (string= rev2 "") (setq rev2 nil))
      (list files rev1 rev2))))

;;;###autoload
(defun vc-version-diff (_files rev1 rev2)
  "Report diffs between revisions REV1 and REV2 in the repository history.
This compares two revisions of the current fileset.
If REV1 is nil, it defaults to the previous revision, i.e. revision
before the last commit.
If REV2 is nil, it defaults to the work tree, i.e. the current
state of each file in the fileset."
  (interactive (vc-diff-build-argument-list-internal))
  ;; All that was just so we could do argument completion!
  (when (and (not rev1) rev2)
    (error "Not a valid revision range"))
  ;; Yes, it's painful to call (vc-deduce-fileset) again.  Alas, the
  ;; placement rules for (interactive) don't actually leave us a choice.
  (vc-diff-internal vc-allow-async-diff (vc-deduce-fileset t) rev1 rev2
		    (called-interactively-p 'interactive)))

;;;###autoload
(defun vc-root-version-diff (_files rev1 rev2)
  "Report diffs between REV1 and REV2 revisions of the whole tree."
  (interactive
   (vc-diff-build-argument-list-internal
    (let ((backend (or (vc-deduce-backend) (vc-responsible-backend default-directory))))
      (list backend (list (vc-call-backend backend 'root default-directory))))))
  ;; This is a mix of `vc-root-diff' and `vc-version-diff'
  (when (and (not rev1) rev2)
    (error "Not a valid revision range"))
  (vc--with-backend-in-rootdir "VC root-diff"
    (vc-diff-internal vc-allow-async-diff (list backend (list rootdir)) rev1 rev2
                      (called-interactively-p 'interactive))))

;;;###autoload
(defun vc-diff (&optional historic not-essential fileset)
  "Display diffs between file revisions.
Normally this compares the currently selected fileset with their
working revisions.  With a prefix argument HISTORIC, it reads two revision
designators specifying which revisions to compare.

Optional argument NOT-ESSENTIAL non-nil means it is okay to say no to
saving the buffer.
Optional argument FILESET, if non-nil, overrides the fileset."
  (interactive (list current-prefix-arg t))
  (if historic
      (call-interactively 'vc-version-diff)
    (let ((fileset (or fileset (vc-deduce-fileset t))))
      (vc-buffer-sync-fileset fileset not-essential)
      (vc-diff-internal vc-allow-async-diff fileset nil nil
			(called-interactively-p 'interactive)))))

(defun vc-buffer-sync-fileset (fileset &optional not-essential missing-in-dirs)
  "Call `vc-buffer-sync' for most buffers visiting files in FILESET.
NOT-ESSENTIAL means it is okay to continue if the user says not to save.

For files named explicitly in FILESET, this function always syncs their
buffers.  By contrast, for directories named in FILESET, its behavior
depends on MISSING-IN-DIRS.  For each directory named in FILESET, it
considers buffers visiting any file contained within that directory or
its subdirectories.  If MISSING-IN-DIRS is nil, it syncs only those
buffers whose files exist on disk.  Otherwise it syncs all of them."
  ;; This treatment of directories named in FILESET is wanted for, at
  ;; least, users with `vc-find-revision-no-save' set to non-nil: not
  ;; treating directories this way would imply calling `vc-buffer-sync'
  ;; on all buffers generated by \\`C-x v ~' during \\`C-x v D'.
  (let ((non-essential not-essential)
        dirs buffers)
    (dolist (name (cadr fileset))
      (if (file-directory-p name)
          (push (file-name-as-directory name) dirs)
        (when-let* ((buf (find-buffer-visiting name)))
          (push buf buffers))))
    (when dirs
      (setq buffers
            (cl-nunion buffers
                       (match-buffers
                        (lambda (buf)
                          (and-let*
                              ((file (buffer-local-value 'buffer-file-name buf))
                               ((cl-some (if not-essential
                                             (lambda (dir)
                                               ;; For speed (bug#79137).
                                               (string-prefix-p dir file))
                                           (lambda (dir)
                                             (file-in-directory-p file dir)))
                                         dirs))
                               ((or missing-in-dirs
                                    (file-exists-p file))))))))))
    (dolist (buf buffers)
      (with-current-buffer buf
        (vc-buffer-sync not-essential)))))

;;;###autoload
(defun vc-diff-mergebase (_files rev1 rev2)
  "Report diffs between the merge base of REV1 and REV2 revisions.
The merge base is a common ancestor between REV1 and REV2 revisions."
  (interactive
   (vc-diff-build-argument-list-internal
    (or (ignore-errors (vc-deduce-fileset t))
        (let ((backend (or (vc-deduce-backend) (vc-responsible-backend default-directory))))
          (list backend (list (vc-call-backend backend 'root default-directory)))))))
  (when (and (not rev1) rev2)
    (error "Not a valid revision range"))
  (vc--with-backend-in-rootdir "VC root-diff"
    (let ((rev1 (vc-call-backend backend 'mergebase rev1 rev2)))
      (vc-diff-internal
       vc-allow-async-diff (list backend (list rootdir)) rev1 rev2
       (called-interactively-p 'interactive)))))

;;;###autoload
(defun vc-root-diff-incoming (&optional upstream-location)
  "Report diff of all changes that would be pulled from UPSTREAM-LOCATION.
When unspecified UPSTREAM-LOCATION is the place \\[vc-update] would pull
from.  When called interactively with a prefix argument, prompt for
UPSTREAM-LOCATION.  In some version control systems UPSTREAM-LOCATION
can be a remote branch name.

See `vc-use-incoming-outgoing-prefixes' regarding giving this command a
global binding."
  (interactive (list (vc--maybe-read-upstream-location)))
  (vc--with-backend-in-rootdir "VC root-diff"
    (vc-diff-incoming upstream-location `(,backend (,rootdir)))))

;;;###autoload
(defun vc-diff-incoming (&optional upstream-location fileset)
  "Report changes to VC fileset that would be pulled from UPSTREAM-LOCATION.
When unspecified UPSTREAM-LOCATION is the place \\[vc-update] would pull
from.  When called interactively with a prefix argument, prompt for
UPSTREAM-LOCATION.  In some version control systems UPSTREAM-LOCATION
can be a remote branch name.
When called from Lisp optional argument FILESET overrides the VC
fileset.

See `vc-use-incoming-outgoing-prefixes' regarding giving this command a
global binding."
  (interactive (list (vc--maybe-read-upstream-location) nil))
  (let* ((fileset (or fileset (vc-deduce-fileset t)))
         (backend (car fileset))
         (incoming (vc--incoming-revision backend
                                          upstream-location 'refresh)))
    (vc-diff-internal vc-allow-async-diff fileset
                      (vc-call-backend backend 'mergebase incoming)
                      incoming
                      (called-interactively-p 'interactive))))

;;;###autoload
(defun vc-root-diff-outgoing (&optional upstream-location)
  "Report diff of all changes that would be pushed to UPSTREAM-LOCATION.
When unspecified UPSTREAM-LOCATION is the place \\[vc-push] would push
to.  When called interactively with a prefix argument, prompt for
UPSTREAM-LOCATION.  In some version control systems UPSTREAM-LOCATION
can be a remote branch name.

This command is like `vc-root-diff-outgoing-base' except that it does
not include uncommitted changes.

See `vc-use-incoming-outgoing-prefixes' regarding giving this command a
global binding."
  (interactive (list (vc--maybe-read-upstream-location)))
  (vc--with-backend-in-rootdir "VC root-diff"
    (vc-diff-outgoing upstream-location `(,backend (,rootdir)))))

;;;###autoload
(defun vc-diff-outgoing (&optional upstream-location fileset)
  "Report changes to VC fileset that would be pushed to UPSTREAM-LOCATION.
When unspecified UPSTREAM-LOCATION is the place \\[vc-push] would push
to.  When called interactively with a prefix argument, prompt for
UPSTREAM-LOCATION.  In some version control systems UPSTREAM-LOCATION
can be a remote branch name.
When called from Lisp optional argument FILESET overrides the VC
fileset.

This command is like `vc-diff-outgoing-base' except that it does not
include uncommitted changes.

See `vc-use-incoming-outgoing-prefixes' regarding giving this command a
global binding."
  ;; For this command, for distributed VCS, we want to ignore
  ;; uncommitted changes because those are not outgoing, and the point
  ;; for those VCS is to make a comparison between locally committed
  ;; changes and remote committed changes.
  ;; (Hence why we don't call `vc-buffer-sync-fileset'.)
  (interactive (list (vc--maybe-read-upstream-location)))
  (let* ((fileset (or fileset (vc-deduce-fileset t)))
         (backend (car fileset))
         (incoming (vc--incoming-revision backend upstream-location)))
    (vc-diff-internal vc-allow-async-diff fileset
                      (vc-call-backend backend 'mergebase incoming)
                      ;; FIXME: In order to exclude uncommitted
                      ;; changes we need to pass the most recent
                      ;; revision as REV2.  Calling `working-revision'
                      ;; like this works for all the backends we have
                      ;; in core that implement `mergebase' and so can
                      ;; be used with this command (Git and Hg).
                      ;; However, it is not clearly permitted by the
                      ;; current semantics of `working-revision' to
                      ;; call it on a directory.
                      ;;
                      ;; A possible alternative would be something
                      ;; like this which effectively falls back to
                      ;; including uncommitted changes in the case of
                      ;; an older VCS or where the backend rejects our
                      ;; attempt to call `working-revision' on a
                      ;; directory:
                      ;; (and (eq (vc-call-backend backend
                      ;;                           'revision-granularity)
                      ;;          'repository)
                      ;;      (ignore-errors
                      ;;        (vc-symbolic-working-revision (caadr fileset)
                      ;;                                      backend)))
                      (vc-symbolic-working-revision (caadr fileset) backend)
                      (called-interactively-p 'interactive))))

;; This is used in .dir-locals.el in the Emacs source tree.
;;;###autoload (put 'vc-trunk-branch-regexps 'safe-local-variable
;;;###autoload      #'vc--safe-branch-regexps-p)
(defcustom vc-trunk-branch-regexps '("trunk" "master" "main" "default")
  "Regular expressions matching the names of longer-lived VCS branches.
There value can be of one of the following forms:
- A list of regular expressions.  A trunk branch is one whose name
  matches any of the regular expressions.  If an element of the list
  contains no characters that are special in regular expressions, then
  the regexp is implicitly anchored at both ends, i.e., it is the full
  name of a branch.
- A list whose first element is `not' and whose remaining elements are
  regular expressions.  This is the same as the previous case except
  that a trunk branch is one whose name does *not* match any of the
  regular expressions.
- The symbol t.  A trunk branch is any branch that
  `vc-topic-branch-regexps' does not positively identify as a topic
  branch.
- An empty list (or, the symbol nil).  The branch name does not indicate
  whether a branch is a trunk.  Emacs will ask the backend whether it
  thinks the current branch is a trunk.

In VC, trunk branches are those where you've finished sharing the work
on the branch with your collaborators just as soon as you've checked it
in, and in the case of a decentralized VCS, pushed it.  In addition,
typically you never delete trunk branches.

The specific VCS workflow you are using may only acknowledge a single
trunk, and give other names to kinds of branches which VC would consider
to be just further trunks.

If trunk branches in your project can be identified by name, include
regexps matching their names in the value of this variable.  This is
more reliable than letting Emacs ask the backend.

See also `vc-topic-branch-regexps'."
  :type '(choice (repeat :tag "Regexps" string)
                 (cons :tag "Negated regexps"
                       (const not) (repeat :tag "Regexps" string))
                 (const :tag "Inverse of `vc-branch-trunk-regexps'" t))
  :safe #'vc--safe-branch-regexps-p
  :version "31.1")

;; This is used in .dir-locals.el in the Emacs source tree.
;;;###autoload (put 'vc-topic-branch-regexps 'safe-local-variable
;;;###autoload      #'vc--safe-branch-regexps-p)
(defcustom vc-topic-branch-regexps nil
  "Regular expressions matching the names of shorter-lived VCS branches.
There value can be of one of the following forms:
- A list of regular expressions.  A topic branch is one whose name
  matches any of the regular expressions.  If an element of the list
  contains no characters that are special in regular expressions, then
  the regexp is implicitly anchored at both ends, i.e., it is the full
  name of a branch.
- A list whose first element is `not' and whose remaining elements are
  regular expressions.  This is the same as the previous case except
  that a topic branch is one whose name does *not* match any of the
  regular expressions.
- The symbol t.  A topic branch is any branch that
  `vc-trunk-branch-regexps' does not positively identify as a trunk
  branch.
- An empty list (or, the symbol nil).  The branch name does not indicate
  whether a branch is a topic branch.  Emacs will ask the backend
  whether it thinks the current branch is a topic branch.

In VC, topic branches are those where checking in work, and pushing it
in the case of a decentralized VCS, is not enough to complete the
process of sharing the changes with your collaborators.  In addition,
it's required that you merge the topic branch into another branch.
After this is done, typically you delete the topic branch.

Topic branches are sometimes called \"feature branches\", though it is
also common for that term to be reserved for only a certain kind of
topic branch.

If topic branches in your project can be identified by name, include
regexps matching their names in the value of this variable.  This is
more reliable than letting Emacs ask the backend.

See also `vc-trunk-branch-regexps'."
  :type '(choice (repeat :tag "Regexps" string)
                 (cons :tag "Negated regexps"
                       (const not) (repeat :tag "Regexps" string))
                 (const :tag "Inverse of `vc-trunk-branch-regexps'" t))
  :safe #'vc--safe-branch-regexps-p
  :version "31.1")

(defun vc--match-branch-name-regexps (branch)
  "Match against `vc-trunk-branch-regexps' and `vc-topic-branch-regexps'.
See the docstrings for those two variables for how this matching works.

If BRANCH matches both sets of regexps we signal an error; this is to
allow for future extension.
If BRANCH matches neither set of regexps return nil to mean that the
defcustoms don't decide the matter of which kind of branch this is."
  (when (and (eq vc-trunk-branch-regexps t)
             (eq vc-topic-branch-regexps t))
    (user-error "\
`vc-trunk-branch-regexps' and `vc-topic-branch-regexps' cannot both be `t'"))
  (cl-labels ((join-regexps (regexps)
                (mapconcat (lambda (elt)
                             (format (if (equal (regexp-quote elt) elt)
                                         "\\`%s\\'"
                                       "\\(?:%s\\)")
                                     elt))
                           regexps "\\|"))
              (compile-regexps (regexps)
                (if regexps
                    (let* ((negated (eq (car regexps) 'not))
                           (joined (join-regexps (if negated
                                                     (cdr regexps)
                                                   regexps))))
                      (if negated
                          (lambda (s) (not (string-match-p joined s)))
                        (lambda (s) (string-match-p joined s))))
                  #'ignore))
              (match-trunk (if (eq vc-trunk-branch-regexps t)
                               (lambda (s) (not (match-topic s)))
                             (compile-regexps vc-trunk-branch-regexps)))
              (match-topic (if (eq vc-topic-branch-regexps t)
                               (lambda (s) (not (match-trunk s)))
                             (compile-regexps vc-topic-branch-regexps))))
    (let ((trunk (match-trunk branch))
          (topic (match-topic branch)))
      (cond ((and trunk topic)
             (error "Branch name `%s' matches both \
`vc-trunk-branch-regexps' and `vc-topic-branch-regexps'"
                    branch))
            (trunk 'trunk)
            (topic 'topic)))))

(defun vc--outgoing-base (backend)
  "Return an outgoing base for the current branch under VC backend BACKEND.
The outgoing base is the upstream location for which outstanding changes
on this branch are destined once they are no longer outstanding.

There are two stages to determining the outgoing base.
First we decide whether we think this is a shorter-lived or a
longer-lived (\"trunk\") branch (see `vc-trunk-branch-regexps' and
`vc-topic-branch-regexps' regarding this distinction), as follows:
1. Ask the backend for the name of the current branch.
   If it returns non-nil, compare that name against
   `vc-trunk-branch-regexps' and `vc-topic-branch-regexps'.
2. If that doesn't settle it, either because the backend returns nil for
   the name of the current branch, or because comparing the name against
   the two regexp defcustoms yields no decisive answer, call BACKEND's
   `trunk-or-topic-p' VC API function.
3. If that doesn't settle it either, assume this is a shorter-lived
   branch.  This is based on how it's commands primarily intended for
   working with shorter-lived branches that call this function.
Second, if we have determined that this is a trunk, return nil, meaning
that the outgoing base is the place to which `vc-push' would push.
Otherwise, we have determined that this is a shorter-lived branch, and
we return the value of calling BACKEND's `topic-outgoing-base' VC API
function."
  ;; For further discussion see bug#80006.
  (let* ((branch (vc-call-backend backend 'working-branch))
         (type (or (and branch (vc--match-branch-name-regexps branch))
                   (vc-call-backend backend 'trunk-or-topic-p)
                   'topic)))
    (and (eq type 'topic)
         (vc-call-backend backend 'topic-outgoing-base))))

(defun vc--outgoing-base-mergebase (backend &optional upstream-location refresh)
  "Return, under VC backend BACKEND, the merge base with UPSTREAM-LOCATION.
Normally UPSTREAM-LOCATION, if non-nil, is a string.
If UPSTREAM-LOCATION is nil, it means to call `vc--outgoing-base' and
use its return value as UPSTREAM-LOCATION.  If `vc--outgoing-base'
returns nil, that means to use the place to which `vc-push' would push.
If UPSTREAM-LOCATION is the special value t, it means to use the place
to which `vc-push' would push as UPSTREAM-LOCATION, unconditionally.
(This is passed when the user invokes an outgoing base command with a
 \\`C-u C-u' prefix argument; see `vc--maybe-read-outgoing-base'.)
REFRESH is passed on to `vc--incoming-revision'."
  (vc-call-backend backend 'mergebase
                   (vc--incoming-revision backend
                                          (pcase upstream-location
                                            ('t nil)
                                            ('nil (vc--outgoing-base backend))
                                            (_ upstream-location))
                                          refresh)))

;;;###autoload
(defun vc-root-diff-outgoing-base (&optional upstream-location)
  "Report diff of all changes since the merge base with UPSTREAM-LOCATION.
The merge base with UPSTREAM-LOCATION means the common ancestor of the
working revision and UPSTREAM-LOCATION.
Uncommitted changes are included in the diff.

When unspecified, UPSTREAM-LOCATION is the outgoing base.
For a trunk branch this is always the place \\[vc-push] would push to.
For a topic branch, query the backend for an appropriate outgoing base.
See `vc-trunk-branch-regexps' and `vc-topic-branch-regexps' regarding
the difference between trunk and topic branches.

When called interactively with a prefix argument, prompt for
UPSTREAM-LOCATION.  In some version control systems, UPSTREAM-LOCATION
can be a remote branch name.

When called interactively with a \\[universal-argument] \\[universal-argument] \
prefix argument, always
use the place to which \\[vc-push] would push to as the outgoing base,
i.e., treat this branch as a trunk branch even if Emacs thinks it is a
topic branch.  (With a double prefix argument, this command is like
`vc-diff-outgoing' except that it includes uncommitted changes.)"
  (interactive (list (vc--maybe-read-outgoing-base)))
  (vc--with-backend-in-rootdir "VC root-diff"
    (vc-diff-outgoing-base upstream-location `(,backend (,rootdir)))))

;;;###autoload
(defun vc-diff-outgoing-base (&optional upstream-location fileset)
  "Report changes to VC fileset since the merge base with UPSTREAM-LOCATION.

The merge base with UPSTREAM-LOCATION means the common ancestor of the
working revision and UPSTREAM-LOCATION.
Uncommitted changes are included in the diff.

When unspecified, UPSTREAM-LOCATION is the outgoing base.
For a trunk branch this is always the place \\[vc-push] would push to.
For a topic branch, query the backend for an appropriate outgoing base.
See `vc-trunk-branch-regexps' and `vc-topic-branch-regexps' regarding
the difference between trunk and topic branches.

When called interactively with a prefix argument, prompt for
UPSTREAM-LOCATION.  In some version control systems, UPSTREAM-LOCATION
can be a remote branch name.

When called interactively with a \\[universal-argument] \\[universal-argument] \
prefix argument, always
use the place to which \\[vc-push] would push to as the outgoing base,
i.e., treat this branch as a trunk branch even if Emacs thinks it is a
topic branch.  (With a double prefix argument, this command is like
`vc-diff-outgoing' except that it includes uncommitted changes.)

When called from Lisp, optional argument FILESET overrides the fileset."
  (interactive (let ((fileset (vc-deduce-fileset t)))
                 (list (vc--maybe-read-outgoing-base (car fileset))
                       fileset)))
  (let ((fileset (or fileset (vc-deduce-fileset t))))
    (vc-diff-internal vc-allow-async-diff fileset
                      (vc--outgoing-base-mergebase (car fileset)
                                                   upstream-location)
                      nil
                      (called-interactively-p 'interactive))))

;;;###autoload
(defun vc-log-outgoing-base (&optional upstream-location fileset)
  "Show log for the VC fileset since the merge base with UPSTREAM-LOCATION.
The merge base with UPSTREAM-LOCATION means the common ancestor of the
working revision and UPSTREAM-LOCATION.

When unspecified, UPSTREAM-LOCATION is the outgoing base.
For a trunk branch this is always the place \\[vc-push] would push to.
For a topic branch, query the backend for an appropriate outgoing base.
See `vc-trunk-branch-regexps' and `vc-topic-branch-regexps' regarding
the difference between trunk and topic branches.

When called interactively with a prefix argument, prompt for
UPSTREAM-LOCATION.  In some version control systems, UPSTREAM-LOCATION
can be a remote branch name.

When called interactively with a \\[universal-argument] \\[universal-argument] \
prefix argument, always
use the place to which \\[vc-push] would push to as the outgoing base,
i.e., treat this branch as a trunk branch even if Emacs thinks it is a
topic branch.

When called from Lisp, optional argument FILESET overrides the fileset."
  (interactive (let ((fileset (vc-deduce-fileset t)))
                 (list (vc--maybe-read-outgoing-base (car fileset))
                       fileset)))
  (let* ((fileset (or fileset (vc-deduce-fileset t)))
         (backend (car fileset)))
    (vc-print-log-internal backend (cadr fileset) nil nil
                           (vc--outgoing-base-mergebase backend
                                                        upstream-location))))

;;;###autoload
(defun vc-root-log-outgoing-base (&optional upstream-location)
  "Show log of revisions since the merge base with UPSTREAM-LOCATION.
The merge base with UPSTREAM-LOCATION means the common ancestor of the
working revision and UPSTREAM-LOCATION.

When unspecified, UPSTREAM-LOCATION is the outgoing base.
For a trunk branch this is always the place \\[vc-push] would push to.
For a topic branch, query the backend for an appropriate outgoing base.
See `vc-trunk-branch-regexps' and `vc-topic-branch-regexps' regarding
the difference between trunk and topic branches.

When called interactively with a prefix argument, prompt for
UPSTREAM-LOCATION.  In some version control systems, UPSTREAM-LOCATION
can be a remote branch name.

When called interactively with a \\[universal-argument] \\[universal-argument] \
prefix argument, always
use the place to which \\[vc-push] would push to as the outgoing base,
i.e., treat this branch as a trunk branch even if Emacs thinks it is a
topic branch."
  (interactive (list (vc--maybe-read-outgoing-base)))
  (vc--with-backend-in-rootdir "VC revision log"
    (vc-log-outgoing-base upstream-location `(,backend (,rootdir)))))

(declare-function ediff-load-version-control "ediff" (&optional silent))
(declare-function ediff-vc-internal "ediff-vers"
                  (rev1 rev2 &optional startup-hooks))

;;;###autoload
(defun vc-version-ediff (files rev1 rev2)
  "Show differences between REV1 and REV2 of FILES using ediff.
This compares two revisions of the files in FILES.  Currently,
only a single file's revisions can be compared, i.e. FILES can
specify only one file name.
If REV1 is nil, it defaults to the current revision, i.e. revision
of the last commit.
If REV2 is nil, it defaults to the work tree, i.e. the current
state of each file in FILES."
  (interactive (vc-diff-build-argument-list-internal))
  ;; All that was just so we could do argument completion!
  (when (and (not rev1) rev2)
    (error "Not a valid revision range"))

  (message "%s" (format "Finding changes in %s..." (vc-delistify files)))

  ;; Functions ediff-(vc|rcs)-internal use "" instead of nil.
  (when (null rev1) (setq rev1 ""))
  (when (null rev2) (setq rev2 ""))

  (cond
   ;; FIXME We only support running ediff on one file for now.
   ;; We could spin off an ediff session per file in the file set.
   ((= (length files) 1)
    (require 'ediff)
    (ediff-load-version-control)  ; loads ediff-vers
    (find-file (car files))             ;FIXME: find-file from Elisp is bad.
    (ediff-vc-internal rev1 rev2 nil))
   (t
    (error "More than one file is not supported"))))

;;;###autoload
(defun vc-ediff (historic &optional not-essential)
  "Display diffs between file revisions using ediff.
Normally this compares the currently selected fileset with their
working revisions.  With a prefix argument HISTORIC, it reads two revision
designators specifying which revisions to compare.

Optional argument NOT-ESSENTIAL non-nil means it is okay to say no to
saving the buffer."
  (interactive (list current-prefix-arg t))
  (if historic
      (call-interactively 'vc-version-ediff)
    (let ((fileset (vc-deduce-fileset)))
      (vc-buffer-sync-fileset fileset not-essential)
      (vc-version-ediff (cadr fileset) nil nil))))

;;;###autoload
(defun vc-root-diff (historic &optional not-essential)
  "Display diffs between VC-controlled whole tree revisions.
Normally, this compares the tree corresponding to the current
fileset with the working revision.
With a prefix argument HISTORIC, prompt for two revision
designators specifying which revisions to compare.

Optional argument NOT-ESSENTIAL non-nil means it is okay to say no to
saving the buffer."
  (interactive (list current-prefix-arg t))
  (if historic
      ;; We want the diff for the VC root dir.
      (call-interactively 'vc-root-version-diff)
    (vc--with-backend-in-rootdir "VC root-diff"
      ;; VC diff for the root directory produces output that is
      ;; relative to it.  Bind default-directory to the root directory
      ;; here, this way the *vc-diff* buffer is setup correctly, so
      ;; relative file names work.
      (let ((fileset `(,backend (,rootdir))))
        (vc-buffer-sync-fileset fileset not-essential)
        (vc-diff-internal vc-allow-async-diff fileset nil nil
                          (called-interactively-p 'interactive))))))

;;;###autoload
(defun vc-root-dir (&optional backend)
  "Return the root directory for the current VC tree.
Return nil if the root directory cannot be identified.
BACKEND is the VC backend."
  (and-let* ((backend (or backend (vc-deduce-backend))))
    (condition-case err
        (vc-call-backend backend 'root default-directory)
      (vc-not-supported
       (unless (eq (cadr err) 'root)
         (signal (car err) (cdr err)))
       nil))))

;;;###autoload
(defun vc-revision-other-window (rev)
  "Visit revision REV of the current file in another window.
If the current file is named `F', the revision is named `F.~REV~'.
If `F.~REV~' already exists, use it instead of checking it out again.

If this command needs to split the current window, it by default obeys
the user options `split-height-threshold' and `split-width-threshold',
when it decides whether to split the window horizontally or vertically."
  (interactive
   (with-current-buffer (or (buffer-base-buffer) (current-buffer))
     (vc-ensure-vc-buffer)
     (list
      (vc-read-revision (format-prompt "Revision to visit" "working revision")
                        (list buffer-file-name)))))
  (set-buffer (or (buffer-base-buffer) (current-buffer)))
  (vc-ensure-vc-buffer)
  (let* ((file buffer-file-name)
	 (revision (if (string-empty-p rev)
		       (vc-symbolic-working-revision file)
		     rev)))
    (switch-to-buffer-other-window (vc-find-revision file revision))))

(defun vc-find-revision (file revision &optional backend)
  "Read REVISION of FILE into a buffer and return the buffer.
Use BACKEND as the VC backend if specified."
  (if vc-find-revision-no-save
      (vc-find-revision-no-save file revision backend)
    (vc-find-revision-save file revision backend)))

(defun vc-find-revision-save (file revision &optional backend)
  "Read REVISION of FILE into a buffer and return the buffer.
Saves the buffer to the file."
  (let ((automatic-backup (vc-version-backup-file-name file revision))
	(filebuf (or (get-file-buffer file) (current-buffer)))
        (filename (vc-version-backup-file-name file revision 'manual))
        (backend (or backend (vc-backend file))))
    (unless (file-exists-p filename)
      (if (file-exists-p automatic-backup)
          (rename-file automatic-backup filename nil)
	(message "Checking out %s..." filename)
	(with-current-buffer filebuf
	  (let ((failed t))
	    (unwind-protect
		(let ((coding-system-for-read 'no-conversion))
		  (with-temp-file filename
		    (let ((outbuf (current-buffer)))
                      ;; We will read the backend's output with no
                      ;; conversions, so we should also save the
                      ;; temporary file with no encoding conversions.
                      (setq buffer-file-coding-system 'no-conversion)
		      ;; Change buffer to get local value of
		      ;; vc-checkout-switches.
		      (with-current-buffer filebuf
			(vc-call-backend backend 'find-revision
                                         file revision outbuf))))
		  (setq failed nil))
	      (when (and failed (file-exists-p filename))
		(delete-file filename))))
	  (vc-mode-line file))
	(message "Checking out %s...done" filename)))
    (let ((result-buf (find-file-noselect filename))
          (file (expand-file-name file))) ; ensure it's absolute
      (with-current-buffer result-buf
        (setq-local vc-parent-buffer filebuf
                    vc-buffer-overriding-fileset `(,backend (,file))
                    vc-buffer-revision revision))
      result-buf)))

(defun vc-find-revision-no-save (file revision &optional backend buffer)
  "Read REVISION of FILE into BUFFER and return the buffer.
If BUFFER omitted or nil, this function creates a new buffer and sets
`buffer-file-name' to the name constructed from the file name and the
revision number.
Unlike `vc-find-revision-save', doesn't save the buffer to the file."
  (let* ((buffer (and (buffer-live-p buffer) buffer))
         (filebuf (or buffer (get-file-buffer file) (current-buffer)))
         (filename (and (not buffer)
                        (vc-version-backup-file-name file revision 'manual)))
         (backend (or backend (vc-backend file))))
    (unless (and (not buffer)
                 (or (get-file-buffer filename)
                     (file-exists-p filename)))
      (with-current-buffer filebuf
	(let ((failed t))
	  (unwind-protect
	      (with-current-buffer (or buffer (create-file-buffer filename))
                (unless buffer (setq buffer-file-name filename))
		(let ((outbuf (current-buffer)))
		  (with-current-buffer filebuf
		    (vc-call-backend backend 'find-revision file revision outbuf)))
                (decode-coding-inserted-region (point-min) (point-max) file)
                (after-insert-file-set-coding (- (point-max) (point-min)))
                (goto-char (point-min))
                (if buffer
                    ;; For non-interactive, skip any questions
                    (let ((enable-local-variables
                           (if (memq enable-local-variables '(:safe :all nil))
                               enable-local-variables
                             ;; Ignore other values that query,
                             ;; use `:safe' to find `mode:'.
                             :safe))
                          (buffer-file-name file))
                      ;; Don't run hooks that might assume buffer-file-name
                      ;; really associates buffer with a file (bug#39190).
                      (ignore-errors (delay-mode-hooks (set-auto-mode))))
                  ;; Use non-nil 'find-file' arg of 'normal-mode'
                  ;; to not ignore 'enable-local-variables' when nil.
                  (normal-mode (not enable-local-variables)))
	        (set-buffer-modified-p nil)
                (read-only-mode 1)
                (setq failed nil))
	    (when (and failed (unless buffer (get-file-buffer filename)))
	      (with-current-buffer (get-file-buffer filename)
		(set-buffer-modified-p nil))
	      (kill-buffer (get-file-buffer filename)))))))
    (let ((result-buf (or buffer
                          (get-file-buffer filename)
                          (find-file-noselect filename)))
          (file (expand-file-name file))) ; ensure it's absolute
      (with-current-buffer result-buf
        (setq-local vc-parent-buffer filebuf
                    vc-buffer-overriding-fileset `(,backend (,file))
                    vc-buffer-revision revision))
      result-buf)))

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
      (when (or (not (vc-check-headers))
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
	  (when vc-static-header-alist
	    (dolist (f vc-static-header-alist)
	      (when (string-match (car f) buffer-file-name)
		(insert (format (cdr f) (car hdstrings)))))))))))

(defun vc-modify-change-comment (files rev oldcomment)
  "Edit the comment associated with the given files and revision."
  ;; Less of a kluge than it looks like; log-view mode only passes
  ;; this function a singleton list.  Arguments left in this form in
  ;; case the more general operation ever becomes meaningful.
  (let ((backend (vc-responsible-backend (car files))))
    (vc-start-logentry
     files oldcomment t
     "Enter a replacement change comment."
     "*vc-log*"
     (lambda () (vc-call-backend backend 'log-edit-mode))
     (lambda (files comment)
       (vc-call-backend backend
                        'modify-change-comment files rev comment)
       ;; We are now back in `vc-parent-buffer'.
       ;; If this is Log View, then revision IDs might now be
       ;; out-of-date, which could be hazardous if the user immediately
       ;; tries to use `log-view-modify-change-comment' a second time.
       ;; E.g. with Git, `vc-git-modify-change-comment' could create an
       ;; "amend!" commit referring to a commit which no longer exists
       ;; on the branch, such that it wouldn't be autosquashed.
       ;; So refresh the view.
       (when (derived-mode-p 'log-view-mode)
         (revert-buffer)))
     nil backend nil
     (lambda ()
       ;; Here we want the root diff for REV, even if we were called
       ;; from a buffer generated by C-x v l, because the change comment
       ;; we will edit applies to the whole revision.
       (let* ((rootdir
               (vc-call-backend backend 'root default-directory))
              (prevrev
               (vc-call-backend backend
                                'previous-revision rootdir rev)))
         (vc-diff-internal nil (list backend (list rootdir))
                           prevrev rev))))))

;;;###autoload
(defun vc-merge ()
  "Perform a version control merge operation.
You must be visiting a version controlled file, or in a `vc-dir' buffer.
On a distributed version control system, this runs a \"merge\"
operation to incorporate changes from another branch onto the
current branch, prompting for an argument list.

On a non-distributed version control system, this merges changes
between two revisions into the current fileset.  This asks for
two revisions to merge from in the minibuffer.  If the first
revision is a branch number, then merge all changes from that
branch.  If the first revision is empty, merge the most recent
changes from the current branch."
  (interactive)
  (let* ((vc-fileset (vc-deduce-fileset t))
	 (backend (car vc-fileset))
	 (files (cadr vc-fileset)))
    (cond
     ;; If a branch-merge operation is defined, use it.
     ((vc-find-backend-function backend 'merge-branch)
      (vc-call-backend backend 'merge-branch))
     ;; Otherwise, do a per-file merge.
     ((vc-find-backend-function backend 'merge-file)
      (vc-buffer-sync)
      (dolist (file files)
	(let* ((state (vc-state file))
	       status)
	  (cond
	   ((stringp state)	;; Locking VCses only
	    (error "File %s is locked by %s" file state))
	   ((not (vc-editable-p file))
	    (vc-checkout file t)))
	  (setq status (vc-call-backend backend 'merge-file file))
	  (vc-maybe-resolve-conflicts file status "WORKFILE" "MERGE SOURCE"))))
     (t
      (error "Sorry, merging is not implemented for %s" backend)))))

(defun vc-maybe-resolve-conflicts (file status &optional _name-A _name-B)
  (vc-resynch-buffer file t (not (buffer-modified-p)))
  (if (zerop status) (message "Merge successful")
    (smerge-mode 1)
    (message "File contains conflicts.")))

;;;###autoload
(defun vc-message-unresolved-conflicts (filename)
  "Display a message indicating unresolved conflicts in FILENAME."
  ;; This enables all VC backends to give a standard, recognizable
  ;; conflict message that indicates which file is conflicted.
  (message "There are unresolved conflicts in %s" filename))

;;;###autoload
(defalias 'vc-resolve-conflicts 'smerge-ediff)

;; TODO: This is OK but maybe we could integrate it better.
;; E.g. it could be run semi-automatically (via a prompt?) when saving a file
;; that was conflicted (i.e. upon mark-resolved).
;; FIXME: should we add an "other-window" version?  Or maybe we should
;; hook it inside find-file so it automatically works for
;; find-file-other-window as well.  E.g. find-file could use a new
;; `default-next-file' variable for its default file (M-n), and
;; we could then set it upon mark-resolve, so C-x C-s C-x C-f M-n would
;; automatically offer the next conflicted file.
;;;###autoload
(defun vc-find-conflicted-file ()
  "Visit the next conflicted file in the current project."
  (interactive)
  (let* ((backend (or (if buffer-file-name (vc-backend buffer-file-name))
                      (vc-responsible-backend default-directory)
                      (error "No VC backend")))
         (root (vc-root-dir))
         (files (vc-call-backend backend
                                 'conflicted-files (or root default-directory))))
    ;; Don't try and visit the current file.
    (if (equal (car files) buffer-file-name) (pop files))
    (if (null files)
        (message "No more conflicted files")
      (find-file (pop files))
      (message "%s more conflicted files after this one"
               (if files (length files) "No")))))

;; Named-configuration entry points

(defun vc-tag-precondition (dir)
  "Scan the tree below DIR, looking for files not up-to-date.
If any file is not up-to-date, return the name of the first such file.
\(This means, neither tag creation nor retrieval is allowed.)
If one or more of the files are currently visited, return `visited'.
Otherwise, return nil."
  (let ((status nil))
    (catch 'vc-locked-example
      (vc-file-tree-walk
       dir
       (lambda (f)
	 (if (not (vc-up-to-date-p f)) (throw 'vc-locked-example f)
	   (when (get-file-buffer f) (setq status 'visited)))))
      status)))

;;;###autoload
(defun vc-create-tag (dir name branchp)
  "Descending recursively from DIR, make a tag called NAME.
For each registered file, the working revision becomes part of
the configuration identified by the tag.
If BRANCHP is non-nil (interactively, the prefix argument), the
tag NAME is a new branch, and the files are checked out and
updated to reflect their revisions on that branch.
In interactive use, DIR is `default-directory' for repository-granular
VCSes (all the modern decentralized VCSes belong to this group),
otherwise the command will prompt for DIR."
  (interactive
   (let ((granularity
	  (vc-call-backend (vc-responsible-backend default-directory)
			   'revision-granularity)))
     (list
      (if (eq granularity 'repository)
	  ;; For VC's that do not work at file level, it's pointless
	  ;; to ask for a directory, branches are created at repository level.
	  default-directory
	(read-directory-name "Directory: " default-directory default-directory t))
      (read-string (if current-prefix-arg "New branch name: " "New tag name: ")
                   nil 'vc-revision-history)
      current-prefix-arg)))
  (message "Making %s... " (if branchp "branch" "tag"))
  (when (file-directory-p dir) (setq dir (file-name-as-directory dir)))
  (vc-call-backend (vc-responsible-backend dir)
		   'create-tag dir name branchp)
  (vc-resynch-buffer dir t t t)
  (message "Making %s... done" (if branchp "branch" "tag")))

;;;###autoload
(defun vc-create-branch (dir name)
  "Make a branch called NAME in directory DIR.
After making the new branch, check out the branch, i.e. update the
files in the tree to their revisions on the branch.

Interactively, prompt for the NAME of the branch.

With VCSes that maintain version information per file, this command also
prompts for the directory DIR whose files, recursively, will be tagged
with the NAME of new branch.  For VCSes that maintain version
information for the entire repository (all the modern decentralized
VCSes belong to this group), DIR is always the `default-directory'.

Finally, this command might prompt for the branch or tag from which to
start (\"fork\") the new branch, with completion candidates including
all the known branches and tags in the repository.

This command invokes `vc-create-tag' with the non-nil BRANCHP argument."
  (interactive
   (let ((granularity
          (vc-call-backend (vc-responsible-backend default-directory)
                           'revision-granularity)))
     (list
      (if (eq granularity 'repository)
          default-directory
        (read-directory-name "Directory: " default-directory default-directory t))
      (read-string "New branch name: " nil 'vc-revision-history))))
  (vc-create-tag dir name t))

;;;###autoload
(defun vc-retrieve-tag (dir name &optional branchp)
  "For each file in or below DIR, retrieve their version identified by tag NAME.
NAME can name a branch, in which case this command will switch to the
named branch in the directory DIR.
Interactively, prompt for DIR only for VCS that works at file level;
otherwise use the root directory of the current buffer's VC tree.
If NAME is empty, it refers to the latest revisions of the current branch.
If locking is used for the files in DIR, then there must not be any
locked files at or below DIR (but if NAME is empty, locked files are
allowed and simply skipped).
If BRANCHP is non-nil (interactively, the prefix argument), switch to the
branch and check out and update the files to their version on that branch.
In this case NAME may not be empty.
This function runs the hook `vc-retrieve-tag-hook' when finished."
  (interactive
   (let* ((granularity
           (vc-call-backend (vc-responsible-backend default-directory)
                            'revision-granularity))
          (dir
           (if (eq granularity 'repository)
               ;; For VC's that do not work at file level, it's pointless
               ;; to ask for a directory, branches are created at repository level.
               ;; XXX: Either we call expand-file-name here, or use
               ;; file-in-directory-p inside vc-resynch-buffers-in-directory.
               (expand-file-name (vc-root-dir))
             (read-directory-name "Directory: " default-directory nil t))))
     (list dir
           (vc-read-revision (if current-prefix-arg
                                 "Switch to branch: "
                               (format-prompt "Tag name to retrieve"
                                              "latest revisions"))
                             (list dir)
                             (vc-responsible-backend dir))
           current-prefix-arg)))
  (unless (or (not branchp) (and name (not (string-empty-p name))))
    (user-error "Branch name required"))
  (let* ((backend (vc-responsible-backend dir))
         (update (when (vc-call-backend backend 'update-on-retrieve-tag)
                   (yes-or-no-p "Update any affected buffers? ")))
	 (msg (if (or (not name) (string= name ""))
		  (format "Updating %s... " (abbreviate-file-name dir))
	        (format "Retrieving %s %s into %s... "
                        (if branchp "branch" "tag")
		        name (abbreviate-file-name dir)))))
    (message "%s" msg)
    (vc-call-backend backend 'retrieve-tag dir name update)
    (vc-resynch-buffer dir t t t)
    (run-hooks 'vc-retrieve-tag-hook)
    (message "%s" (concat msg "done"))))

;;;###autoload
(defun vc-switch-branch (dir name)
  "Switch to the branch NAME in the directory DIR.
Interactively, prompt for DIR only for VCS that works at file level;
otherwise use the root directory of the current buffer's VC tree.
Interactively, prompt for the NAME of the branch.
After switching to the branch, check out and update the files to their
version on that branch.
Uses `vc-retrieve-tag' with the non-nil arg `branchp'."
  (interactive
   (let* ((granularity
           (vc-call-backend (vc-responsible-backend default-directory)
                            'revision-granularity))
          (dir
           (if (eq granularity 'repository)
               (expand-file-name (vc-root-dir))
             (read-directory-name "Directory: " default-directory nil t))))
     (list dir
           (vc-read-revision "Switch to branch: "
                             (list dir)
                             (vc-responsible-backend dir)))))
  (vc-retrieve-tag dir name t))

;; Miscellaneous other entry points

;; FIXME: this should be a defcustom
;; FIXME: maybe add another choice:
;; `root-directory' (or somesuch), which would mean show a short log
;; for the root directory.
(defvar vc-log-short-style '(directory)
  "Whether or not to show a short log.
If it contains `directory', show a short log if the fileset
contains a directory.
If it contains `file', show short logs for files.
Not all VC backends support short logs!")

(defvar log-view-vc-fileset)
(defvar log-view-message-re)
;; XXX: File might have been renamed multiple times, so to support
;; multiple jumps back, this probably should be a stack of entries.
(defvar log-view-vc-prev-revision nil)
(defvar log-view-vc-prev-fileset nil)

(defun vc-print-log-setup-buttons (working-revision is-start-revision limit pl-return)
  "Insert at the end of the current buffer buttons to show more log entries.
In the new log, leave point at WORKING-REVISION (if non-nil).
LIMIT is the current maximum number of entries shown, or the
revision (string) before which to stop.  Does nothing if
IS-START-REVISION is non-nil and LIMIT is 1, or if LIMIT is nil,
or if PL-RETURN is `limit-unsupported'."
  ;; LIMIT=1 is set by vc-annotate-show-log-revision-at-line
  ;; or by vc-print-root-log with current-prefix-arg=1.
  ;; In either case only one revision is wanted, no buttons.
  (when (and limit (not (eq 'limit-unsupported pl-return))
             (not (and is-start-revision
                       (eql limit 1))))
    (let ((entries 0))
      (goto-char (point-min))
      (while (re-search-forward log-view-message-re nil t)
        (incf entries))
      (if (or (stringp limit)
              (< entries limit))
          ;; The log has been printed in full.  Perhaps it started
          ;; with a copy or rename?
          ;; FIXME: We'd probably still want this button even when
          ;; vc-log-show-limit is customized to 0 (should be rare).
          (let* ((last-revision (log-view-current-tag (point-max)))
                 ;; XXX: Could skip this when vc-git-print-log-follow = t.
                 (name-changes
                  (condition-case nil
                      (vc-call-backend log-view-vc-backend
                                       'file-name-changes last-revision)
                    (vc-not-supported nil)))
                 (matching-changes
                  (cl-delete-if-not (lambda (f) (member f log-view-vc-fileset))
                                    name-changes :key #'cdr))
                 (old-names (delq nil (mapcar #'car matching-changes))))
            (when old-names
              (goto-char (point-max))
              (unless (looking-back "\n\n" (- (point) 2))
                (insert "\n"))
              (vc-print-log-renamed-add-button old-names log-view-vc-backend
                                               log-view-vc-fileset
                                               working-revision
                                               last-revision
                                               limit)))
        ;; Perhaps there are more entries in the log.
        (goto-char (point-max))
        (insert "\n")
        (insert-text-button
         "Show 2X entries"
         'action (lambda (&rest _ignore)
                   (vc-print-log-internal
                    log-view-vc-backend log-view-vc-fileset
                    working-revision nil (* 2 limit)))
         'help-echo
         "Show the log again, and double the number of log entries shown")
        (insert "    ")
        (insert-text-button
         "Show unlimited entries"
         'action (lambda (&rest _ignore)
                   (vc-print-log-internal
                    log-view-vc-backend log-view-vc-fileset
                    working-revision nil nil))
         'help-echo "Show the log again, including all entries")
        (insert "\n")))))

(defun vc-print-log-renamed-add-button ( renamed-files backend
                                         current-fileset
                                         current-revision
                                         revision limit)
  "Print the button for jump to the log for a different fileset.
RENAMED-FILES is the fileset to use.  BACKEND is the VC backend.
REVISION is the revision from which to start the new log.
CURRENT-FILESET, if non-nil, is the fileset to use in the \"back\"
button for.  Same for CURRENT-REVISION.  LIMIT means the usual."
  (let ((relatives (mapcar #'file-relative-name renamed-files))
        (from-to (if current-fileset "from" "to"))
        (before-after (if current-fileset "before" "after")))
    (insert
     (format
      "Renamed %s %s"
      from-to
      (mapconcat (lambda (s)
                   (propertize s 'font-lock-face
                               'log-view-file))
                 relatives
                 ", "))
     " ")
    (insert-text-button
     "View log"
     'action (lambda (&rest _ignore)
               ;; To set up parent buffer in the new viewer.
               (with-current-buffer vc-parent-buffer
                 (let ((log-view-vc-prev-fileset current-fileset)
                       (log-view-vc-prev-revision current-revision))
                   (vc-print-log-internal backend renamed-files
                                          revision t limit))))
     ;; XXX: Showing the full history for OLD-NAMES (with
     ;; IS-START-REVISION=nil) can be better sometimes
     ;; (e.g. when some edits still occurred after a rename
     ;; -- multiple branches scenario), but it also can hurt
     ;; in others because of Git's automatic history
     ;; simplification: as a result, the logs for some
     ;; use-package's files before merge could not be found.
     'help-echo
     (format
      "Show the log for the file name(s) %s the rename"
      before-after))))

(defun vc-print-log-internal (backend files working-revision
                                      &optional is-start-revision limit type)
  "For specified BACKEND and FILES, show the VC log.
Leave point at WORKING-REVISION, if it is non-nil.
If IS-START-REVISION is non-nil, start the log from WORKING-REVISION
\(not all backends support this); i.e., show only WORKING-REVISION and
earlier revisions.  Show up to LIMIT entries (nil means unlimited).
LIMIT can also be a string, which means the revision before which to stop."
  ;; Don't switch to the output buffer before running the command,
  ;; so that any buffer-local settings in the vc-controlled
  ;; buffer can be accessed by the command.
  (let* ((dir-present (cl-some #'file-directory-p files))
         (shortlog (not (null (memq (if dir-present 'directory 'file)
                                    vc-log-short-style))))
	 (buffer-name "*vc-change-log*")
         (type (or type (if shortlog 'short 'long))))
      (vc-log-internal-common
       backend buffer-name files type
       (lambda (bk buf _type-arg files-arg)
         (vc-call-backend bk 'print-log files-arg buf shortlog
                          (when is-start-revision working-revision) limit)
         (when log-view-vc-prev-fileset
           (with-current-buffer buf
             (let ((inhibit-read-only t)
                   (pmark (process-mark (get-buffer-process buf))))
               (goto-char (point-min))
               (vc-print-log-renamed-add-button log-view-vc-prev-fileset
                                                backend
                                                nil
                                                nil
                                                log-view-vc-prev-revision
                                                limit)
               (insert "\n\n")
               (when (< pmark (point))
                 (set-marker pmark (point)))))))
       (lambda (_bk _files-arg ret)
         (save-excursion
           (vc-print-log-setup-buttons working-revision
                                       is-start-revision limit ret)))
       ;; When it's nil, point really shouldn't move (bug#15322).
       (when working-revision
         (lambda (bk)
           (vc-call-backend bk 'show-log-entry working-revision)))
       (lambda (_ignore-auto _noconfirm)
	 (vc-print-log-internal backend files working-revision
                                is-start-revision limit type)))))

(defvar vc-log-view-type nil
  "Set this to record the type of VC log shown in the current buffer.
Supported values are:

  `short'        -- short log form, one line for each commit
  `long'         -- long log form, including full log message and author
  `with-diff'    -- log including diffs
  `log-outgoing' -- log of changes to be pushed to upstream
  `log-incoming' -- log of changes to be brought by pulling from upstream
  `log-search'   -- log entries matching a pattern; shown in long format
  `mergebase'    -- log created by `vc-log-mergebase'.")
(put 'vc-log-view-type 'permanent-local t)
(defvar vc-sentinel-movepoint)

(defvar vc-log-finish-functions '(vc-shrink-buffer-window)
  "Functions run at the end of the log command.
Each function runs in the log output buffer without args.")

(defun vc-log-internal-common (backend
			       buffer-name
			       files
			       type
			       backend-func
			       setup-buttons-func
			       goto-location-func
			       rev-buff-func)
  (let (retval (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (setq-local vc-log-view-type type))
    (setq retval (funcall backend-func backend buffer-name type files))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
	;; log-view-mode used to be called with inhibit-read-only bound
	;; to t, so let's keep doing it, just in case.
	(vc-call-backend backend
                         (if (and (eq type 'with-diff)
                                  (vc-find-backend-function
                                   backend 'region-history-mode))
                             'region-history-mode
                           'log-view-mode))
        (setq-local log-view-vc-backend backend)
        (setq-local log-view-vc-fileset files)
        (setq-local revert-buffer-function rev-buff-func)))
    ;; Display after setting up major-mode, so display-buffer-alist can know
    ;; the major-mode.
    (pop-to-buffer buffer)
    (vc-run-delayed
     (let ((inhibit-read-only t))
       (funcall setup-buttons-func backend files retval)
       (when goto-location-func
         (funcall goto-location-func backend)
         (setq vc-sentinel-movepoint (point)))
       (set-buffer-modified-p nil)
       (run-hooks 'vc-log-finish-functions)))))

(defun vc-incoming-outgoing-internal (backend upstream-location buffer-name type)
  (vc-log-internal-common
   backend buffer-name (list (vc-root-dir)) type
   (lambda (bk buf type-arg _files)
     (vc-call-backend bk type-arg buf upstream-location))
   (lambda (_bk _files-arg _ret) nil)
   nil ;; Don't move point.
   (lambda (_ignore-auto _noconfirm)
     (vc-incoming-outgoing-internal backend upstream-location buffer-name type))))

(defun vc--read-limit ()
  "Read a LIMIT argument for a VC log command."
  (string-to-number
   (read-from-minibuffer "Limit display (0 for unlimited): "
                         (format "%s" vc-log-show-limit))))

;;;###autoload
(defun vc-print-log (&optional working-revision limit)
  "Show in another window the VC change history of the current fileset.
If WORKING-REVISION is non-nil, it should be a revision ID; position
point in the change history buffer at that revision.
If LIMIT is non-nil, it should be a number specifying the maximum
number of revisions to show; the default for interactive calls is
`vc-log-show-limit'.

When called interactively with a prefix argument, prompt for
WORKING-REVISION and LIMIT.

This shows a short log (one line for each commit) if the current
fileset includes directories and the VC backend supports that;
otherwise it shows the detailed log of each commit, which includes
the full log message and the author.  Additional control of the
shown log style is available via `vc-log-short-style'."
  (interactive
   (cond
    (current-prefix-arg
     (let ((rev (read-from-minibuffer (format-prompt "Leave point at revision"
                                                     "last revision")))
	   (lim (vc--read-limit)))
       (list (and (not (string-empty-p rev)) rev) (and (plusp lim) lim))))
    (t
     (list nil (and (plusp vc-log-show-limit) vc-log-show-limit)))))
  (let ((fileset (vc-deduce-fileset t))
	(working-revision (or working-revision vc-buffer-revision)))
    (vc-print-log-internal (car fileset) (cadr fileset)
                           working-revision nil limit)))

;;;###autoload
(defun vc-print-change-log ()
  "Show in another window the VC change history of the current fileset.
With a \\[universal-argument] prefix argument, prompt for a branch \
or revision to log
instead of the working revision, and a number specifying the maximum
number of revisions to show; the default is `vc-log-show-limit'.
You can also use a numeric prefix argument to specify this.

This is like `vc-print-log' but with an alternative prefix argument that
some users might prefer for interactive usage."
  (declare (interactive-only vc-print-log))
  (interactive)
  (if current-prefix-arg
      (let ((branch
             (vc--read-branch-to-log t))
            (vc-log-show-limit
             (if (equal current-prefix-arg '(4))
                 (vc--read-limit)
               (prefix-numeric-value current-prefix-arg))))
        (vc-print-fileset-branch-log branch))
    (vc-print-log)))

;;;###autoload
(defun vc-print-root-log (&optional limit revision)
  "Show in another window VC change history of the current VC controlled tree.
If LIMIT is non-nil, it should be a number specifying the maximum
number of revisions to show; the default is `vc-log-show-limit'.
When called interactively with a prefix argument, prompt for LIMIT, but
if the prefix argument is a number, use it as LIMIT.
A special case is when the prefix argument is 1: in this case
the command prompts for the id of a REVISION, and shows that revision
with its diffs (if the underlying VCS backend supports that)."
  (interactive
   (cond
    ((eq current-prefix-arg 1)
     (let* ((default (thing-at-point 'word t))
	    (revision (read-string (format-prompt "Revision to show" default)
		                   nil nil default)))
       (list 1 revision)))
    ((numberp current-prefix-arg)
     (list current-prefix-arg))
    (current-prefix-arg
     (let ((lim (vc--read-limit)))
       (list (and (plusp lim) lim))))
    (t
     (list (and (plusp vc-log-show-limit) vc-log-show-limit)))))
  (vc--with-backend-in-rootdir "VC revision log"
    (let* ((with-diff (and (eq limit 1) revision))
           (vc-log-short-style (and (not with-diff) vc-log-short-style)))
      (vc-print-log-internal backend (list rootdir) revision revision limit
                             (and with-diff 'with-diff))
      ;; We're looking at the root, so displaying " from <some-file>" in
      ;; the mode line isn't helpful.
      (setq vc-parent-buffer-name nil))))

;;;###autoload
(defun vc-print-root-change-log ()
  "Show in another window the VC change history of the whole tree.
With a \\[universal-argument] prefix argument, prompt for a branch \
or revision to log
instead of the working revision, and a number specifying the maximum
number of revisions to show; the default is `vc-log-show-limit'.
You can also use a numeric prefix argument to specify this.

This is like `vc-root-print-log' but with an alternative prefix argument
that some users might prefer for interactive usage."
  (declare (interactive-only vc-print-root-log))
  (interactive)
  (if current-prefix-arg
      (let ((branch
             (vc--read-branch-to-log))
            (vc-log-show-limit
             (if (equal current-prefix-arg '(4))
                 (vc--read-limit)
               (prefix-numeric-value current-prefix-arg))))
        (vc-print-root-branch-log branch))
    (vc-print-root-log)))

(defun vc--read-branch-to-log (&optional fileset)
  "Read the name of a branch to log.
FILESET, if non-nil, means to pass the current VC fileset to
`vc-read-revision'."
  (let* ((fileset (and fileset (vc-deduce-fileset t)))
         (branch (vc-read-revision "Branch to log: "
                                   (cadr fileset) (car fileset))))
    (when (string-empty-p branch)
      (user-error "No branch specified"))
    branch))

;;;###autoload
(defun vc-print-fileset-branch-log (branch)
  "Show log of VC changes on BRANCH, limited to the current fileset.
When called interactively, prompts for BRANCH.
In addition to logging branches, for VCS for which it makes sense you
can specify a revision ID instead of a branch name to produce a log
starting at that revision.  Tags and remote references also work."
  ;; Currently the prefix argument is conserved.  Possibly it could be
  ;; used to prompt for a LIMIT argument like \\`C-x v l' has.  Though
  ;; now we have "Show 2X entries" and "Show unlimited entries" that
  ;; might be a waste of the prefix argument to this command.  --spwhitton
  (interactive (list (vc--read-branch-to-log t)))
  (let ((fileset (vc-deduce-fileset t)))
    (vc-print-log-internal (car fileset) (cadr fileset) branch t
                           (and (plusp vc-log-show-limit)
                                vc-log-show-limit))))

;;;###autoload
(defun vc-print-root-branch-log (branch)
  "Show root log of VC changes on BRANCH in another window.
When called interactively, prompts for BRANCH.
In addition to logging branches, for VCS for which it makes sense you
can specify a revision ID instead of a branch name to produce a log
starting at that revision.  Tags and remote references also work."
  ;; Prefix argument conserved; see previous command.  --spwhitton
  (interactive (list (vc--read-branch-to-log)))
  (vc--with-backend-in-rootdir "VC branch log"
    (vc-print-log-internal backend (list rootdir) branch t
                           (and (plusp vc-log-show-limit)
                                vc-log-show-limit))))
;; We plan to reuse the name `vc-print-branch-log' for the
;; fileset-specific command in Emacs 32.1.  --spwhitton
(define-obsolete-function-alias
  'vc-print-branch-log
  #'vc-print-root-branch-log
  "31.1")

;; FIXME: Consider renaming to `vc-upstream-location-history'.
(defvar vc-remote-location-history nil
  "History of upstream locations for VC incoming and outgoing commands.")

(defun vc--maybe-read-upstream-location ()
  "Read upstream location if there is a prefix argument, else return nil."
  (and current-prefix-arg
       (let ((res (read-string "Upstream location/branch (empty for default): "
                               nil 'vc-remote-location-history)))
         (and (not (string-empty-p res)) res))))

(defun vc--maybe-read-outgoing-base (&optional backend)
  "Return upstream location for interactive uses of outgoing base commands.
If there is no prefix argument, return nil.
If the current prefix argument is \\`C-u C-u', return t.
Otherwise prompt for an upstream location.
BACKEND is the VC backend."
  (cond
   ((equal current-prefix-arg '(16)) t)
   (current-prefix-arg
    (let* ((outgoing-base (vc-call-backend (or backend
                                               (vc-deduce-backend))
                                           'topic-outgoing-base))
           ;; If OUTGOING-BASE is non-nil then 'C-u C-x v T ... RET' is
           ;; how the user can force Emacs to treat the current branch
           ;; as a topic while having Emacs automatically determine the
           ;; outgoing base with which to do so (otherwise, forcing
           ;; Emacs to treat the current branch as a topic if it thinks
           ;; it's a trunk requires specifying an outgoing base which
           ;; will have that effect).
           ;;
           ;; In this case that OUTGOING-BASE is non-nil, it isn't
           ;; possible to specify an empty string as the outgoing base,
           ;; which normally means that Emacs should treat the current
           ;; branch as a trunk.  That's okay because you can use a
           ;; double prefix argument to achieve that.
           (res (read-string (if outgoing-base
                                 (format-prompt "Upstream location/branch"
                                                outgoing-base)
                               "Upstream location/branch (empty to treat as trunk): ")
                             nil 'vc-remote-location-history outgoing-base)))
      (and (not (string-empty-p res)) res)))))

(defun vc--incoming-revision (backend &optional upstream-location refresh)
  ;; Some backends don't support REFRESH and so always behave as though
  ;; REFRESH is non-nil.  This is not just for a lack of implementation
  ;; in Emacs; for example, Mercurial repositories don't store any
  ;; representation of the incoming revision between running commands.
  ;;
  ;; Fetching the incoming revision is often slow, and in many cases the
  ;; last known incoming revision will serve perfectly well.  For
  ;; example, when finding revisions that are outgoing, the last known
  ;; incoming revision is fine except for the rare case in which someone
  ;; else cherry-picks the very same commits that you have outstanding,
  ;; and pushes them.  Given this, we implement our own caching.
  ;;
  ;; Do store `nil', before signaling an error, if there is no incoming
  ;; revision, because that's also something that can be slow to
  ;; determine and so should be remembered.
  (or (if-let* ((_ (not refresh))
                (record (assoc upstream-location
                               (vc--repo-getprop backend
                                                 'vc-incoming-revision))))
          (cdr record)
        (let ((res (vc-call-backend backend 'incoming-revision
                                    upstream-location refresh)))
          (if-let* ((alist (vc--repo-getprop backend
                                             'vc-incoming-revision)))
              (setf (alist-get upstream-location alist
                               nil nil #'equal)
                    res)
            (vc--repo-setprop backend
                              'vc-incoming-revision
                              `((,upstream-location . ,res))))
          res))
      (user-error "No incoming revision -- local-only branch?")))

;;;###autoload
(defun vc-root-log-incoming (&optional upstream-location)
  "Show log of changes that will be received with pull from UPSTREAM-LOCATION.
When unspecified UPSTREAM-LOCATION is the place \\[vc-update] would pull
from.  When called interactively with a prefix argument, prompt for
UPSTREAM-LOCATION.  In some version control systems UPSTREAM-LOCATION
can be a remote branch name."
  (interactive (list (vc--maybe-read-upstream-location)))
  (vc--with-backend-in-rootdir "VC root-log"
    (vc-incoming-outgoing-internal backend upstream-location
                                   "*vc-incoming*" 'log-incoming)))
;; We plan to reuse the name `vc-log-incoming' for the fileset-specific
;; command in Emacs 32.1.  --spwhitton
(define-obsolete-function-alias 'vc-log-incoming #'vc-root-log-incoming
  "31.1")

(defun vc-default-log-incoming (backend buffer upstream-location)
  (let ((incoming (vc--incoming-revision backend upstream-location
                                         'refresh))
        (default-directory (vc-root-dir backend)))
    (vc-call-backend backend 'print-log (list default-directory)
                     buffer t incoming
                     (vc-call-backend backend 'mergebase incoming))))

;;;###autoload
(defun vc-root-log-outgoing (&optional upstream-location)
  "Show log of changes that will be sent with a push to UPSTREAM-LOCATION.
When unspecified UPSTREAM-LOCATION is the place \\[vc-push] would push
to.  When called interactively with a prefix argument, prompt for
UPSTREAM-LOCATION.  In some version control systems UPSTREAM-LOCATION
can be a remote branch name."
  (interactive (list (vc--maybe-read-upstream-location)))
  (vc--with-backend-in-rootdir "VC root-log"
    (vc-incoming-outgoing-internal backend upstream-location
                                   "*vc-outgoing*" 'log-outgoing)))
;; We plan to reuse the name `vc-log-outgoing' for the fileset-specific
;; command in Emacs 32.1.  --spwhitton
(define-obsolete-function-alias 'vc-log-outgoing #'vc-root-log-outgoing
  "31.1")

(defun vc-default-log-outgoing (backend buffer upstream-location)
  (let ((incoming (vc--incoming-revision backend upstream-location))
        (default-directory (vc-root-dir backend)))
    (vc-call-backend backend 'print-log (list default-directory)
                     buffer t ""
                     (vc-call-backend backend 'mergebase incoming))))

(defun vc--count-outgoing (backend)
  "Return number of changes that will be sent with a `vc-push'."
  (with-temp-buffer
    (let ((display-buffer-overriding-action
           '(display-buffer-no-window (allow-no-window . t))))
      (vc-incoming-outgoing-internal backend nil
                                     (current-buffer) 'log-outgoing))
    (let ((proc (get-buffer-process (current-buffer))))
      (while (accept-process-output proc)))
    (how-many log-view-message-re)))

;;;###autoload
(defun vc-log-search (pattern)
  "Search the VC log of changes for PATTERN and show log of matching changes.

PATTERN is usually interpreted as a regular expression.  However, its
exact semantics is up to the backend's log search command; some can
only match fixed strings.

This command displays in long format all the changes whose log messages
match PATTERN.

With a prefix argument, the command asks for a shell command to run that
will output log entries, and displays those log entries instead."
  (interactive (list (unless current-prefix-arg
                       (read-regexp "Search log with pattern: "))))
  (let ((backend (vc-deduce-backend)))
    (unless backend
      (error "Buffer is not version controlled"))
    (vc-incoming-outgoing-internal backend pattern
                                   "*vc-search-log*" 'log-search)))

;;;###autoload
(defun vc-log-mergebase (_files rev1 rev2)
  "Show a log of changes between the merge base of revisions REV1 and REV2.
The merge base is a common ancestor of revisions REV1 and REV2."
  (interactive
   (vc-diff-build-argument-list-internal
    (or (ignore-errors (vc-deduce-fileset t))
        (let ((backend (or (vc-deduce-backend) (vc-responsible-backend default-directory))))
          (list backend (list (vc-call-backend backend 'root default-directory)))))))
  (vc--with-backend-in-rootdir "VC root-log"
    (setq rev1 (vc-call-backend backend 'mergebase rev1 rev2))
    (vc-print-log-internal backend (list rootdir) (or rev2 "") t rev1)))

;;;###autoload
(defun vc-region-history (from to)
  "Show the history of the region between FROM and TO.

If called interactively, show the history between point and
mark."
  (interactive "r")
  (let* ((lfrom (line-number-at-pos from t))
         (lto   (line-number-at-pos (1- to) t))
         (file buffer-file-name)
         (backend (vc-backend file))
         (buf (get-buffer-create "*VC-history*")))
    (unless backend
      (error "Buffer is not version controlled"))
    (with-current-buffer buf
      (setq-local vc-log-view-type 'long))
    (vc-call region-history file buf lfrom lto)
    (with-current-buffer buf
      (vc-call-backend backend 'region-history-mode)
      (setq-local log-view-vc-backend backend)
      (setq-local log-view-vc-fileset (list file))
      (setq-local revert-buffer-function
                  (lambda (_ignore-auto _noconfirm)
                    (with-current-buffer buf
                      (let ((inhibit-read-only t)) (erase-buffer)))
                    (vc-call region-history file buf lfrom lto))))
    (display-buffer buf)))

;;;###autoload
(defun vc-revert ()
  "Revert working copies of the selected fileset to their repository contents.
This asks for confirmation if the buffer contents are not identical
to the working revision (except for keyword expansion)."
  (interactive)
  (let* ((vc-fileset (vc-deduce-fileset))
         (backend (car vc-fileset))
	 (files (cadr vc-fileset))
	 (queried nil)
	 diff-buffer)
    ;; If any of the files is visited by the current buffer, make sure
    ;; buffer is saved.  If the user says `no', abort since we cannot
    ;; show the changes and ask for confirmation to discard them.
    (when-let* ((n (buffer-file-name))
                ((or (not files) (member n files))))
      (vc-buffer-sync nil))
    (save-some-buffers nil (lambda ()
                             (and-let* ((n (buffer-file-name)))
                               (member n files))))
    (let (needs-save)
      (dolist (file files)
        (let ((buf (get-file-buffer file)))
	  (when (and buf (buffer-modified-p buf))
            (push buf needs-save)))
        (when (vc-up-to-date-p file)
	  (if (yes-or-no-p (format "%s seems up-to-date.  Revert anyway? "
                                   file))
	      (setq queried t)
	    (error "Revert canceled"))))
      (when needs-save
        (error "Cannot revert with these buffers unsaved: %s"
               (string-join (mapcar #'buffer-name needs-save) ", "))))
    (unwind-protect
	(when (if vc-revert-show-diff
		  (progn
		    (setq diff-buffer (generate-new-buffer "*vc-diff*"))
		    (vc-diff-internal vc-allow-async-revert vc-fileset
				      nil nil nil diff-buffer))
		;; Avoid querying the user again.
		(null queried))
	  (unless (yes-or-no-p
		   (format "Discard changes in %s? "
			   (let ((str (vc-delistify files))
				 (nfiles (length files)))
			     (if (length< str 50)
				 str
                               (format (ngettext "%d file" "%d files"
                                                 nfiles)
                                       nfiles)))))
	    (error "Revert cancelled")))
      (when diff-buffer
	(quit-windows-on diff-buffer (eq vc-revert-show-diff 'kill))))
    (vc-revert-files backend files)))

;;;###autoload
(defalias 'vc-restore #'vc-revert)

;;;###autoload
(defun vc-pull (&optional arg)
  "Update the current fileset or branch.
You must be visiting a version controlled file, or in a `vc-dir' buffer.
On a distributed version control system, this runs a \"pull\"
operation to update the current branch, prompting for an argument
list if required.  Optional prefix ARG forces a prompt for the VCS
command to run.

On a non-distributed version control system, update the current
fileset to the tip revisions.  For each unchanged and unlocked
file, this simply replaces the work file with the latest revision
on its branch.  If the file contains changes, any changes in the
tip revision are merged into the working file."
  (interactive "P")
  (let* ((vc-fileset (vc-deduce-fileset t))
	 (backend (car vc-fileset))
	 (files (cadr vc-fileset)))
    (cond
     ;; If a pull operation is defined, use it.
     ((vc-find-backend-function backend 'pull)
      (vc-call-backend backend 'pull arg)
      ;; FIXME: Ideally we would only clear out the stored value for the
      ;; REMOTE-LOCATION from which we are pulling.
      (vc-run-delayed
        (vc--repo-setprop backend 'vc-incoming-revision nil)))
     ;; If VCS has `merge-news' functionality (CVS and SVN), use it.
     ((vc-find-backend-function backend 'merge-news)
      (save-some-buffers                ; save buffers visiting files
       nil (lambda ()
	     (and (buffer-modified-p)
		  (let ((file (buffer-file-name)))
		    (and file (member file files))))))
      (dolist (file files)
	(if (vc-up-to-date-p file)
	    (vc-checkout file t)
	  (vc-maybe-resolve-conflicts
	   file (vc-call-backend backend 'merge-news file)))))
     ;; For a locking VCS, check out each file.
     ((eq (vc-checkout-model backend files) 'locking)
      (dolist (file files)
	(if (vc-up-to-date-p file)
	    (vc-checkout file t))))
     (t
      (error "VC update is unsupported for `%s'" backend)))))

;;;###autoload
(defalias 'vc-update 'vc-pull)

;;;###autoload
(defun vc-push (&optional arg)
  "Push the current branch.
You must be visiting a version controlled file, or in a `vc-dir' buffer.
On a distributed version control system, this runs a \"push\"
operation on the current branch, prompting for the precise command
if required.  Optional prefix ARG non-nil forces a prompt for the
VCS command to run.

On a non-distributed version control system, this signals an error.
It also signals an error in a Bazaar bound branch."
  (interactive "P")
  (let* ((vc-fileset (vc-deduce-fileset t))
	 (backend (car vc-fileset)))
;;;	 (files (cadr vc-fileset)))
    (if (vc-find-backend-function backend 'push)
        (progn (vc-call-backend backend 'push arg)
               ;; FIXME: Ideally we would only clear out the
               ;; REMOTE-LOCATION to which we are pushing.
               (vc-run-delayed
                 (vc--repo-setprop backend 'vc-incoming-revision nil)))
      (user-error "VC push is unsupported for `%s'" backend))))

;;;###autoload
(defun vc-pull-and-push (&optional arg)
  "First pull, and then push the current branch.
The push will only be performed if the pull operation was successful.

You must be visiting a version controlled file, or in a `vc-dir' buffer.

On a distributed version control system, this runs a \"pull\"
operation on the current branch, prompting for the precise
command if required.  Optional prefix ARG non-nil forces a prompt
for the VCS command to run.  If this is successful, a \"push\"
operation will then be done.  This is supported only in backends
where the pull operation returns a process.

On a non-distributed version control system, this signals an error.
It also signals an error in a Bazaar bound branch."
  (interactive "P")
  (let* ((vc-fileset (vc-deduce-fileset t))
	 (backend (car vc-fileset)))
    (if (vc-find-backend-function backend 'pull)
        (when-let* ((proc (vc-call-backend backend 'pull arg))
                    (buf (and (processp proc) (process-buffer proc))))
          (with-current-buffer buf
            (vc-run-delayed-success 0
              (let ((vc--inhibit-async-window t))
                (vc-push arg)))))
      (user-error "VC pull is unsupported for `%s'" backend))))

(defun vc-version-backup-file (file &optional rev)
  "Return name of backup file for revision REV of FILE.
If version backups should be used for FILE, and there exists
such a backup for REV or the working revision of file, return
its name; otherwise return nil."
  (when (vc-call make-version-backups-p file)
    (let ((backup-file (vc-version-backup-file-name file rev)))
      (if (file-exists-p backup-file)
          backup-file
        ;; there is no automatic backup, but maybe the user made one manually
        (setq backup-file (vc-version-backup-file-name file rev 'manual))
        (when (file-exists-p backup-file)
	  backup-file)))))

(defun vc-revert-file (file)
  "Revert FILE back to the repository working revision it was based on.
If FILE is a directory, revert all files inside that directory."
  (with-vc-properties
   (list file)
   (let* ((dir (file-directory-p file))
          (backup-file (and (not dir) (vc-version-backup-file file))))
     (when backup-file
       (copy-file backup-file file 'ok-if-already-exists)
       (vc-delete-automatic-version-backups file))
     (vc-call-backend (if dir
                          (vc-responsible-backend file)
                        (vc-backend file))
                      'revert file backup-file))
   `((vc-state . up-to-date)
     (vc-checkout-time . ,(file-attribute-modification-time
			   (file-attributes file)))))
  (vc-resynch-buffer file t t))

(defun vc-revert-files (backend files)
  "Revert each of FILES to the repository working version it was based on.
For entries in FILES that are directories, revert all files inside them."
  (when files
    (message "Reverting %s..." (vc-delistify files))
    (if (not (vc-find-backend-function backend 'revert-files))
        (mapc #'vc-revert-file files)
      (with-vc-properties files
                          (vc-call-backend backend 'revert-files files)
                          '((vc-state . up-to-date)))
      (dolist (file files)
        (vc-file-setprop file 'vc-checkout-time
                         (file-attribute-modification-time
                          (file-attributes file)))
        (vc-resynch-buffer file t t)))
    (message "Reverting %s...done" (vc-delistify files))))

;;;###autoload
(defun vc-change-backend (file backend)
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
    (let ((crt-bk (vc-backend buffer-file-name))
	  (backends nil))
      (unless crt-bk
        (error "File %s is not under version control" buffer-file-name))
      ;; Find the registered backends.
      (dolist (crt vc-handled-backends)
	(when (and (vc-call-backend crt 'registered buffer-file-name)
		   (not (eq crt-bk crt)))
	  (push crt backends)))
      ;; Find the next backend.
      (let ((def (car backends))
	    (others backends))
	(cond
	 ((null others) (error "No other backend to switch to"))
	 (current-prefix-arg
          (vc-read-backend "Switch to backend: " backends (symbol-name def)))
	 (t def))))))
  (unless (eq backend (vc-backend file))
    (vc-file-clearprops file)
    (vc-file-setprop file 'vc-backend backend)
    ;; Force recomputation of the state
    (unless (vc-call-backend backend 'registered file)
      (vc-file-clearprops file)
      (error "%s is not registered in %s" file backend))
    (vc-mode-line file)))

(define-obsolete-function-alias 'vc-switch-backend #'vc-change-backend
  "30.1")

;;;###autoload
(defun vc-transfer-file (file new-backend)
  "Transfer FILE to another version control system NEW-BACKEND.
If NEW-BACKEND has a higher precedence than FILE's current backend
\(i.e.  it comes earlier in `vc-handled-backends'), then register FILE in
NEW-BACKEND, using the revision number from the current backend as the
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
    (when (eq old-backend new-backend)
      (error "%s is the current backend of %s" new-backend file))
    (if registered
	(set-file-modes file (logior (file-modes file) 128))
      ;; `registered' might have switched under us.
      (vc-change-backend file old-backend)
      (let* ((rev (vc-working-revision file))
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
		  (when (y-or-n-p "Get base revision from repository? ")
		    (vc-revert-file file))))
	      (vc-call-backend new-backend 'receive-file file rev))
	  (when modified-file
            (vc-change-backend file new-backend)
	    (unless (eq (vc-checkout-model new-backend (list file)) 'implicit)
	      (vc-checkout file))
	    (rename-file modified-file file 'ok-if-already-exists)
	    (vc-file-setprop file 'vc-checkout-time nil)))))
    (when move
      (vc-change-backend file old-backend)
      (setq comment (vc-call-backend old-backend 'comment-history file))
      (vc-call-backend old-backend 'unregister file))
    (vc-change-backend file new-backend)
    (when (or move edited)
      (vc-file-setprop file 'vc-state 'edited)
      (vc-mode-line file new-backend)
      (vc-checkin file new-backend comment (stringp comment)))))

;;;###autoload
(defun vc-delete-file (file-or-files)
  "Delete file and mark it as such in the version control system.
If called interactively, read FILE-OR-FILES, defaulting to the current
buffer's file name if it's under version control.
When called from Lisp, FILE-OR-FILES can be a file name or a list of
file names."
  (interactive (list (read-file-name "VC delete file: " nil
                                     (when (vc-backend buffer-file-name)
                                       buffer-file-name)
                                     t)))
  (setq file-or-files (mapcar #'expand-file-name (ensure-list file-or-files)))
  (dolist (file file-or-files)
    (let ((buf (get-file-buffer file))
          (backend (vc-backend file)))
      (unless (or (not backend)
                  (vc-find-backend-function backend 'delete-file))
        (error "Deleting files under %s is not supported in VC" backend))
      (when (and buf (buffer-modified-p buf))
        (error "Please save or undo your changes before deleting %s" file))
      (let ((state (vc-state file)))
        (when (eq state 'edited)
          (error "Please commit or undo your changes before deleting %s" file))
        (when (eq state 'conflict)
          (error "Please resolve the conflicts before deleting %s" file)))))
  (unless (y-or-n-p (if (cdr file-or-files)
                        (format "Really want to delete these %d files? "
                                (length file-or-files))
                      (format "Really want to delete %s? "
			      (file-name-nondirectory (car file-or-files)))))
    (error "Abort!"))
  (dolist (file file-or-files)
    (let ((buf (get-file-buffer file))
          (backend (vc-backend file)))
      (unless (or (file-directory-p file) (null make-backup-files)
                  (not (file-exists-p file)))
        (with-current-buffer (or buf (find-file-noselect file))
          (let ((backup-inhibited nil))
	    (backup-buffer))))
      (when backend
        ;; Bind `default-directory' so that the command that the backend
        ;; runs to remove the file is invoked in the correct context.
        (let ((default-directory (file-name-directory file)))
          (vc-call-backend backend 'delete-file file)))
      ;; For the case of unregistered files, or if the backend didn't
      ;; actually delete the file.
      (when (file-exists-p file) (delete-file file))
      ;; Forget what VC knew about the file.
      (vc-file-clearprops file)
      ;; Make sure the buffer is deleted and the *vc-dir* buffers are
      ;; updated after this.
      (vc-resynch-buffer file nil t))))

;;;###autoload
(defun vc-rename-file (old new)
  "Rename file OLD to NEW in both working tree and repository.
When called interactively, read OLD and NEW, defaulting OLD to the
current buffer's file name if it's under version control."
  ;; FIXME: Support renaming whole directories.
  ;; The use of `vc-call' will need to change to something like
  ;;
  ;;     (vc-call-backend (if dir
  ;;                          (vc-responsible-backend file)
  ;;                        (vc-backend file))
  ;;                      'rename-file old new)
  ;;
  ;; as was done in `vc-revert-file'; see bug#43464.  --spwhitton
  (interactive (list (read-file-name "VC rename file: " nil
                                     (and (vc-backend buffer-file-name)
                                          buffer-file-name)
                                     t)
                     (read-file-name "Rename to: ")))
  ;; in CL I would have said (setq new (merge-pathnames new old))
  (let ((old-base (file-name-nondirectory old)))
    (when (and (not (string-empty-p old-base))
               (string-empty-p (file-name-nondirectory new)))
      (setq new (concat new old-base))))
  (cl-callf expand-file-name old)
  (cl-callf expand-file-name new)
  (let ((oldbuf (get-file-buffer old))
        (default-directory (file-name-directory old)))
    (when (and oldbuf (buffer-modified-p oldbuf))
      (error "Please save files before moving them"))
    (when (get-file-buffer new)
      (error "Already editing new file name"))
    (when (file-exists-p new)
      (error "New file already exists"))
    (let ((state (vc-state old)))
      (unless (memq state '(up-to-date edited added))
	(error "Please %s files before moving them"
	       (if (stringp state) "check in" "update"))))
    (vc-call rename-file old new)
    (vc-file-clearprops old)
    (vc-file-clearprops new)
    ;; Move the actual file (unless the backend did it already)
    (when (file-exists-p old) (rename-file old new))
    ;; ?? Renaming a file might change its contents due to keyword expansion.
    ;; We should really check out a new copy if the old copy was precisely equal
    ;; to some checked-in revision.  However, testing for this is tricky....
    (when oldbuf
      (with-current-buffer oldbuf
	(let ((buffer-read-only buffer-read-only))
	  (set-visited-file-name new))
	(vc-mode-line new (vc-backend new))
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
	  (let ((files nil))
            (dolist (buffer (buffer-list))
	      (let ((file (buffer-file-name buffer)))
                (and file (vc-backend file)
                     (setq files (cons file files)))))
	    files))
	 (t
          ;; Don't supply any filenames to backend; this means
          ;; it should find all relevant files relative to
          ;; the default-directory.
	  nil)))
  (vc-call-backend (vc-responsible-backend default-directory)
                   'update-changelog args))

(defvar vc-filter-command-function)

;;;###autoload
(defun vc-edit-next-command ()
  "Request editing the next VC shell command before execution.
This is a prefix command.  It affects only a VC command executed
immediately after this one."
  (interactive)
  (letrec ((minibuffer-depth (minibuffer-depth))
           (command this-command)
           (keys (key-description (this-command-keys)))
           (old vc-filter-command-function)
           (echofun (lambda () keys))
           (postfun
            (lambda ()
              (unless (or (eq this-command command)
                          (> (minibuffer-depth) minibuffer-depth))
                (remove-hook 'post-command-hook postfun)
                (remove-hook 'prefix-command-echo-keystrokes-functions
                             echofun)
                (setq vc-filter-command-function old)))))
    (add-hook 'post-command-hook postfun)
    (add-hook 'prefix-command-echo-keystrokes-functions echofun)
    (setq vc-filter-command-function
          (lambda (&rest args)
            (apply #'vc-user-edit-command (apply old args))))))

;; This is used in .dir-locals.el in the Emacs source tree.
;;;###autoload (put 'vc-prepare-patches-separately 'safe-local-variable 'booleanp)
(defcustom vc-prepare-patches-separately t
  "Whether `vc-prepare-patch' should generate a separate message for each patch.
If nil, `vc-prepare-patch' creates a single email message by attaching
all the patches to the body of that message.  If non-nil, each patch
will be sent out in a separate message, and the messages will be
prepared sequentially."
  :type 'boolean
  :safe #'booleanp
  :version "29.1")

;; This is used in .dir-locals.el in the Emacs source tree.
;;;###autoload (put 'vc-default-patch-addressee 'safe-local-variable 'stringp)
(defcustom vc-default-patch-addressee nil
  "Default addressee for `vc-prepare-patch'.
If nil, no default will be used.  This option may be set locally."
  :type '(choice (const :tag "No default" nil)
                 (string :tag "Addressee"))
  :safe #'stringp
  :version "29.1")

(declare-function message--name-table "message" (orig-string))
(declare-function mml-attach-buffer "mml"
                  (buffer &optional type description disposition filename))
(declare-function log-view-get-marked "log-view" ())

(defun vc-default-prepare-patch (_backend rev)
  (let ((backend (vc-backend buffer-file-name)))
    (with-current-buffer (generate-new-buffer " *vc-default-prepare-patch*")
      (vc-diff-internal
       nil (list backend) rev
       (vc-call-backend backend 'previous-revision
                        buffer-file-name rev)
       nil t)
      (list :subject (concat "Patch for "
                             (file-name-nondirectory
                              (directory-file-name
                               (vc-root-dir))))
            :buffer (current-buffer)))))

(defun vc-prepare-patch-prompt-revisions ()
  "Prompt the user for a list of revisions.
Prepare a default value, depending on the current context.  With
a numerical prefix argument, use the last N revisions as the
default value.  If the current buffer is a log-view buffer, use
the marked commits.  Otherwise fall back to the working revision
of the current file."
  (vc-read-multiple-revisions
   "Revisions: " nil nil nil
   (or (and-let* ((arg current-prefix-arg)
                  (fs (vc-deduce-fileset t)))
         (cl-loop with file = (caadr fs)
                  repeat (prefix-numeric-value arg)
                  for rev = (vc-working-revision file)
                  then (vc-call-backend
                        (car fs) 'previous-revision
                        file rev)
                  when rev collect it into revs
                  finally return (mapconcat #'identity revs ",")))
       (and-let* ((revs (log-view-get-marked)))
         (mapconcat #'identity revs ","))
       (and-let* ((file (buffer-file-name)))
         (vc-working-revision file)))))

(defun vc--subject-to-file-name (subject)
  "Generate a file name for a patch with subject line SUBJECT."
  (let* ((stripped
          (replace-regexp-in-string "\\`\\[.*PATCH.*\\]\\s-*" ""
                                    subject))
         (truncated (if (length> stripped 50)
                        (substring stripped 0 50)
                      stripped)))
    (concat
     (string-trim (replace-regexp-in-string "\\W" "-" truncated)
                  "-+" "-+")
     ".patch")))

;;;###autoload
(defun vc-prepare-patch (addressee subject revisions)
  "Compose an Email sending patches for REVISIONS to ADDRESSEE.
If `vc-prepare-patches-separately' is nil, use SUBJECT as the
default subject for the message, or prompt a subject when invoked
interactively.  Otherwise compose a separate message for each
revision, with SUBJECT derived from each revision subject.
When invoked with a numerical prefix argument, use the last N
revisions.
When invoked interactively in a Log View buffer with
marked revisions, use those."
  (interactive
   (let* ((revs (vc-prepare-patch-prompt-revisions))
          (subject
           (and (length= revs 1)
                (plist-get
                 (vc-call-backend
                  (vc-responsible-backend default-directory)
                  'prepare-patch (car revs))
                 :subject)))
          to)
     (require 'message)
     (while (null (setq to (completing-read-multiple
                            (format-prompt
                             "Addressee"
                             vc-default-patch-addressee)
                            (message--name-table "")
                            nil nil nil nil
                            vc-default-patch-addressee)))
       (message "At least one addressee required.")
       (sit-for blink-matching-delay))
     (list (string-join to ", ")
           (and (not vc-prepare-patches-separately)
                (read-string "Subject: " (or subject "[PATCH] ") nil nil t))
           revs)))
  (save-current-buffer
    (let ((patches (mapcar (lambda (rev)
                             (vc-call-backend
                              (vc-responsible-backend default-directory)
                              'prepare-patch rev))
                           revisions)))
      (if vc-prepare-patches-separately
          (cl-loop with l = (length patches)
                   for patch in (reverse patches) do
                   (compose-mail addressee
                                 (plist-get patch :subject)
                                 nil nil nil nil
                                 `((kill-buffer ,(plist-get patch :buffer))))
                   (rfc822-goto-eoh) (forward-line)
                   (save-excursion      ;don't jump to the end
                     (insert-buffer-substring
                      (plist-get patch :buffer)
                      (plist-get patch :body-start)
                      (plist-get patch :body-end)))
                   finally (message (ngettext "Prepared %d patch..."
                                              "Prepared %d patches..."
                                              l)
                                    l))
        (compose-mail addressee subject nil nil nil nil
                      (mapcar
                       (lambda (p)
                         (list #'kill-buffer (plist-get p :buffer)))
                       patches))
        (rfc822-goto-eoh)
        (forward-line)
        (save-excursion
          (let ((i 0))
            (dolist (patch patches)
              (let* ((patch-subject (plist-get patch :subject))
                     (filename
                      (vc--subject-to-file-name patch-subject)))
                (mml-attach-buffer
                 (buffer-name (plist-get patch :buffer))
                 "text/x-patch"
                 patch-subject
                 "attachment"
                 (format "%04d-%s" (incf i) filename))))))
        (open-line 2)))))

(defun vc-default-responsible-p (_backend _file)
  "Indicate whether BACKEND is responsible for FILE.
The default is to return nil always."
  nil)

(defun vc-default-find-revision (backend file rev buffer)
  "Provide the new `find-revision' op based on the old `checkout' op.
This is only for compatibility with old backends.  They should be updated
to provide the `find-revision' operation instead."
  (let ((tmpfile (make-temp-file (expand-file-name file))))
    (unwind-protect
	(progn
	  (vc-call-backend backend 'checkout file nil rev tmpfile)
	  (with-current-buffer buffer
	    (insert-file-contents-literally tmpfile)))
      (delete-file tmpfile))))

(defun vc-default-rename-file (_backend old new)
  (condition-case nil
      (add-name-to-file old new)
    (error (rename-file old new)))
  (vc-delete-file old)
  (with-current-buffer (find-file-noselect new)
    (vc-register)))

(defalias 'vc-default-check-headers 'ignore)

(declare-function log-edit-mode "log-edit" ())

(defun vc-default-log-edit-mode (_backend) (log-edit-mode))

(defun vc-default-log-view-mode (_backend) (log-view-mode))

(defun vc-default-show-log-entry (_backend rev)
  (with-no-warnings
   (log-view-goto-rev rev)))

(defun vc-default-comment-history (backend file)
  "Return a string with all log entries stored in BACKEND for FILE."
  (when (vc-find-backend-function backend 'print-log)
    (with-current-buffer "*vc*"
      (vc-call-backend backend 'print-log (list file))
      (buffer-string))))

(defun vc-default-receive-file (backend file rev)
  "Let BACKEND receive FILE from another version control system."
  (vc-call-backend backend 'register (list file) rev ""))

(defun vc-default-update-on-retrieve-tag (_backend)
  "Prompt for update buffers on `vc-retrieve-tag'."
  t)

(defun vc-default-retrieve-tag (backend dir name update)
  (if (string-empty-p name)
      (vc-file-tree-walk dir
                         (lambda (f)
                           (and (vc-up-to-date-p f)
	                        (vc-error-occurred
	                         (vc-call-backend backend 'checkout f nil "")
	                         (when update
                                   (vc-resynch-buffer f t t))))))
    (let ((result (vc-tag-precondition dir)))
      (if (stringp result)
          (error "File %s is locked" result)
        (setq update (and (eq result 'visited) update))
        (vc-file-tree-walk dir
                           (lambda (f)
                             (vc-error-occurred
	                      (vc-call-backend backend 'checkout f nil name)
	                      (when update
                                (vc-resynch-buffer f t t)))))))))

(defun vc-default-revert (backend file contents-done)
  (unless contents-done
    (let ((rev (vc-symbolic-working-revision file))
          (file-buffer (or (get-file-buffer file) (current-buffer))))
      (message "Checking out %s..." file)
      (let ((failed t)
            (backup-name (when (file-exists-p file)
                           (car (find-backup-file-name file)))))
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
                      (vc-call-backend backend 'find-revision
                                       file rev outbuf)))))
              (setq failed nil))
          (when backup-name
            (if failed
                (rename-file backup-name file 'ok-if-already-exists)
              (and (not vc-make-backup-files) (delete-file backup-name))))))
      (message "Checking out %s...done" file))))

(defalias 'vc-default-revision-completion-table 'ignore)
(defalias 'vc-default-mark-resolved 'ignore)

(defun vc-default-dir-status-files (_backend _dir files update-function)
  (funcall update-function
           (mapcar (lambda (file) (list file 'up-to-date)) files)))

(defun vc-check-headers ()
  "Check if the current file has any headers in it."
  (interactive)
  (vc-call-backend (vc-backend buffer-file-name) 'check-headers))

(defvar vc--remotes-history)

(defun vc-clone (remote &optional backend directory rev open-dir)
  "Clone repository REMOTE using version-control BACKEND, into DIRECTORY.
If successful, return the string with the directory of the checkout;
otherwise return nil.
REMOTE should be a string, the URL of the remote repository or the name
of a directory (if the repository is local).

When called interactively, prompt for REMOTE, BACKEND and DIRECTORY,
except attempt to determine BACKEND automatically based on REMOTE.

If DIRECTORY is nil or omitted, it defaults to `default-directory'.
If BACKEND is nil or omitted, the function iterates through every known
backend in `vc-handled-backends' until one succeeds to clone REMOTE.
If REV is non-nil, it indicates a specific revision to check out after
cloning; the syntax of REV depends on what BACKEND accepts.
If OPEN-DIR is non-nil, as it is interactively, also switches to a
buffer visiting DIRECTORY."
  (interactive
   (let* ((url (read-string "Remote: " nil 'vc--remotes-history))
          (backend (or (vc-guess-url-backend url)
                       (intern (completing-read
                                "Backend: " vc-handled-backends nil t)))))
     (list url backend
           (read-directory-name
            "Clone into new or empty directory: " nil nil
            (lambda (dir) (or (not (file-exists-p dir))
                              (directory-empty-p dir))))
           nil t)))
  (let* ((directory (expand-file-name (or directory default-directory)))
         (backend (or backend (vc-guess-url-backend remote)))
         (directory (if backend
                        (progn
                          (unless (memq backend vc-handled-backends)
                            (error "Unknown VC backend %s" backend))
                          (vc-call-backend backend 'clone remote directory rev))
                      (catch 'ok
                        (dolist (backend vc-handled-backends)
                          (ignore-error vc-not-supported
                            (when-let* ((res (vc-call-backend
                                              backend 'clone
                                              remote directory rev)))
                              (throw 'ok res))))))))
    (when (file-directory-p directory)
      (when open-dir
        (find-file directory))
      directory)))

(declare-function log-view-current-tag "log-view" (&optional pos))
(defun vc-default-last-change (_backend file line)
  "Default `last-change' implementation.
It returns the last revision that changed LINE number in FILE."
  (unless (file-exists-p file)
    (signal 'file-error '("File doesn't exist")))
  (with-temp-buffer
    (vc-call-backend (vc-backend file) 'annotate-command
                     file (current-buffer))
    (goto-char (point-min))
    (forward-line (1- line))
    (let ((rev (vc-call annotate-extract-revision-at-line file)))
      (if (consp rev) (car rev) rev))))

(defun vc-dir-status-files (directory &optional files backend)
  "Return VC status information about files in DIRECTORY.
Return a list of the form (FILE VC-STATE EXTRA) for each file.
VC-STATE is the current VC state of the file, and EXTRA is optional,
backend-specific information.
Normally files in the `up-to-date' and `ignored' states are not
included.
If the optional argument FILES is non-nil, report on only items in
FILES, and don't exclude any for being `up-to-date' or `ignored'.
BACKEND is the VC backend; if nil or omitted, it defaults to the result
of calling `vc-responsible-backend' with DIRECTORY as its first and only
argument.

This function provides Lisp programs with synchronous access to the same
information that Emacs requests from VC backends to populate VC-Dir
buffers.  It is usually considerably faster than walking the tree
yourself with a function like `vc-file-tree-walk'."
  ;; The `dir-status-files' API was designed for asynchronous use to
  ;; populate *vc-dir* buffers; see `vc-dir-refresh'.
  ;; This function provides Lisp programs with access to the same
  ;; information without touching the user's *vc-dir* buffers and
  ;; without having to add a new VC backend function.
  ;; This function is in this file despite its `vc-dir-' prefix to avoid
  ;; having to load `vc-dir' just to get access to this simple wrapper.
  (let ((morep t) results)
    (with-temp-buffer
      (setq default-directory directory)
      (vc-call-backend (or backend (vc-responsible-backend directory))
                       'dir-status-files directory files
                       (lambda (entries &optional more-to-come)
                         (let (entry)
                           (while (setq entry (pop entries))
                             (unless (and (not files)
                                          ;; In this case we shouldn't
                                          ;; actually get any
                                          ;; `up-to-date' or `ignored'
                                          ;; entries back, but just in
                                          ;; case, filter them.
                                          (memq (cadr entry)
                                                '(up-to-date ignored)))
                               (push entry results))))
                         (setq morep more-to-come)))
      (while morep (accept-process-output)))
    (nreverse results)))

;;;###autoload
(defun vc-add-working-tree (backend directory)
  "Create working tree DIRECTORY with same backing repository as this tree.
Must be called from within an existing VC working tree.
When called interactively, prompts for DIRECTORY.
When called from Lisp, BACKEND is the VC backend."
  (interactive
   (list
    (vc-responsible-backend default-directory)
    (read-directory-name "Location for new working tree: "
                         (file-name-parent-directory
                          (or (vc-root-dir)
                              (error "File is not under version control"))))))
  (vc-call-backend backend 'add-working-tree directory)

  ;; `vc-switch-working-tree' relies on project.el registration so try
  ;; to ensure that both the old and new working trees are registered.
  ;; `project-current' should not return nil in either case, but don't
  ;; signal an error if it does.
  (when-let* ((p (project-current)))
    (project-remember-project p nil t))
  (when-let* ((p (project-current nil directory)))
    (project-remember-project p))

  (dired directory))

(defvar project-prompter)

(defun vc--prompt-other-working-tree (backend prompt &optional allow-empty)
  "Invoke `project-prompter' to choose another working tree.
BACKEND is the VC backend.
PROMPT is the prompt string for `project-prompter'.
If ALLOW-EMPTY is non-nil, empty input means the current working tree.
In typical usage ALLOW-EMPTY non-nil means that it makes sense to apply
the caller's operation to the current working tree."
  ;; If there are no other working trees and ALLOW-EMPTY is non-nil, we
  ;; still invoke the `project-prompter' and require the user to type
  ;; \\`RET', even though it's redundant.  Doing it this way means that
  ;; invoking the command on the current working tree works the same
  ;; whether or not there exist any other working trees.  In particular,
  ;; the number of keys you have to type is always the same.  It's more
  ;; ergonomic not to require the user to think about whether there are
  ;; other working trees when what they care about is doing something
  ;; with the current working tree: they can just type \\`RET' without
  ;; stopping to look at the echo area.
  (let ((trees (vc-call-backend backend 'known-other-working-trees))
        res)
    (unless (or trees allow-empty)
      (user-error
       (substitute-command-keys
        "No other working trees.  Use \\[vc-add-working-tree] to add one")))
    (require 'project)
    (dolist (tree trees)
      (when-let* ((p (project-current nil tree)))
        (project-remember-project p nil t)))
    (setq res
          (funcall project-prompter
                   (if allow-empty
                       (format "%s (empty for this working tree)"
                               prompt)
                     prompt)
                   (if trees
                       (lambda (k &optional _v)
                         (member (or (car-safe k) k) trees))
                     #'ignore)
                   t allow-empty))
    (if (string-empty-p res) (vc-root-dir) res)))

(defvar project-current-directory-override)

;;;###autoload
(defun vc-switch-working-tree (directory)
  "Switch to the version of this file in working tree under DIRECTORY.
Must be called from within an existing VC working tree.
When called interactively, prompts for DIRECTORY.
This command switches to the file which has the same file
name relative to DIRECTORY that this buffer's file has relative
to the root of this working tree."
  (interactive
   (list
    (vc--prompt-other-working-tree (vc-responsible-backend default-directory)
                                   "Other working tree to visit")))
  (let ((project-current-directory-override directory))
    (project-find-matching-buffer)))

;;;###autoload
(defun vc-working-tree-switch-project (dir)
  "Like \\[project-switch-project] but limited to projects with the same backing repository.
Must be called from within an existing VC working tree.
Prompts for the directory file name of the other working tree."
  ;; There is no point in calling this from Lisp as opposed to calling
  ;; `project-switch-project' directly because it is a trivial wrapper.
  (declare (interactive-only project-switch-project))
  (interactive
   (list
    (vc--prompt-other-working-tree (vc-responsible-backend default-directory)
                                   "Other working tree to switch to")))
  (project-switch-project dir))

;;;###autoload
(defun vc-delete-working-tree (backend directory)
  "Delete working tree DIRECTORY with same backing repository as this tree.
Must be called from within an existing VC working tree.
When called interactively, prompts for DIRECTORY.
BACKEND is the VC backend."
  (interactive
   (let ((backend (vc-responsible-backend default-directory)))
     (list backend
           (vc--prompt-other-working-tree backend "Delete working tree"
                                          'allow-empty))))
  (let* ((delete-this (file-in-directory-p default-directory directory))
         (directory (expand-file-name directory))
         (default-directory
          (if delete-this
              (or (car (vc-call-backend backend
                                        'known-other-working-trees))
                  (user-error "No other working trees"))
            default-directory))
         (status (vc-dir-status-files directory nil backend)))

    ;; We could consider not prompting here, thus always failing when
    ;; there is uncommitted work, and requiring the user to review and
    ;; revert the uncommitted changes before invoking this command again.
    ;; But other working trees are often created as throwaways to quickly
    ;; test some changes, so it is more useful to offer to recursively
    ;; delete them on the user's behalf.
    (when (and status
               (not (yes-or-no-p (format "\
%s contains uncommitted work.  Continue to recursively delete it?" directory))))
      (user-error "Aborted due to uncommitted work in %s" directory))
    ;; Extra prompt to avoid a surprise after accidentally typing 'RET'.
    (when (and (not status) delete-this
               (not (yes-or-no-p (format "Really delete working tree %s?"
                                         directory))))
      (user-error "Aborted"))

    (project-forget-project directory)
    (vc-call-backend backend 'delete-working-tree directory)
    (when delete-this
      (bury-buffer)
      (while (string-prefix-p directory default-directory)
        (bury-buffer)))))

(autoload 'dired-rename-subdir "dired-aux")
;;;###autoload
(defun vc-move-working-tree (backend from to)
  "Relocate a working tree from FROM to TO, two directory file names.
Must be called from within an existing VC working tree.
When called interactively, prompts for the directory file names of each
of the other working trees FROM and TO.
BACKEND is the VC backend."
  (interactive
   (let ((backend (vc-responsible-backend default-directory)))
     (list backend
           (vc--prompt-other-working-tree backend "Relocate working tree"
                                          'allow-empty)
           (read-directory-name "New location for working tree: "
                                (file-name-parent-directory (vc-root-dir))))))
  (let* ((move-this (file-in-directory-p default-directory from))
         (default-directory
          (if move-this
              (or (car (vc-call-backend backend
                                        'known-other-working-trees))
                  (user-error "No other working trees"))
            default-directory)))
    (let ((inhibit-message t))
      (project-forget-project from))
    (vc-call-backend backend 'move-working-tree from to))

  ;; Update visited file names for buffers visiting files under FROM.
  (let ((from (expand-file-name from)))
    (dired-rename-subdir from (expand-file-name to))
    (dolist (buf vc-dir-buffers)
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (string-prefix-p from default-directory)
            (setq default-directory
                  (expand-file-name (file-relative-name default-directory from)
                                    to))
            ;; If the *vc-dir* buffer has a uniquify'd name then we need
            ;; to obtain an new uniquify'd name for this buffer under
            ;; the new working tree, replacing the one for the old
            ;; working tree.  See also `vc-dir-prepare-status-buffer'.
            (when-let* ((base-name (uniquify-buffer-base-name))
                        (item (cl-find (current-buffer) uniquify-managed
                                       :key #'uniquify-item-buffer)))
              (let (name)
                ;; FIXME: There should be a way to get this information
                ;; without creating and killing a buffer.
                (unwind-protect
                    (setq name (buffer-name
                                (create-file-buffer
                                 (expand-file-name base-name
                                                   default-directory))))
                  (kill-buffer name))
                (uniquify-rename-buffer item name))))))))

  (when-let* ((p (project-current nil to)))
    (project-remember-project p)))

(declare-function diff-apply-buffer "diff-mode")

;;;###autoload
(defun vc-apply-to-other-working-tree (directory &optional move)
  "Apply VC fileset's local changes to working tree under DIRECTORY.
Must be called from within an existing VC working tree.
When called interactively, prompts for DIRECTORY.
With a prefix argument (when called from Lisp, with optional argument
MOVE non-nil), don't just copy the changes, but move them, from the
current working tree to DIRECTORY.

When called from a `diff-mode' buffer, move or copy the changes
specified by the contents of that buffer, only.

If any changes to be moved or copied can't be applied to DIRECTORY, it
is an error, and no changes are applied.
If any changes to be moved can't be reverse-applied to this working
tree, it is an error, and no changes are moved."
  ;; The double prefix arg that `vc-apply-root-to-other-working-tree'
  ;; has is omitted here, for now, because it is probably less useful.
  (interactive
   (list
    (vc--prompt-other-working-tree
     (vc-responsible-backend default-directory)
     (format "%s changes to working tree"
             (if current-prefix-arg "Move" "Apply")))
    current-prefix-arg))
  (let* ((relative-dir (file-relative-name default-directory
                                           (vc-root-dir)))
         (mirror-dir (expand-file-name relative-dir directory)))
    (unless (file-directory-p mirror-dir)
      (user-error "`%s' not found in `%s'" relative-dir directory))
    (vc--apply-to-other-working-tree directory mirror-dir
                                     (vc-deduce-fileset)
                                     (and (derived-mode-p 'diff-mode)
                                          (buffer-string))
                                     move)))

;;;###autoload
(defun vc-apply-root-to-other-working-tree (directory &optional move preview)
  "Apply all local changes in this working tree to the tree under DIRECTORY.
Must be called from within an existing VC working tree.
When called interactively, prompts for DIRECTORY.
With a prefix argument (when called from Lisp, with optional argument
MOVE non-nil), don't just copy the changes, but move them, from the
current working tree to DIRECTORY.

With a double prefix argument (\\[universal-argument] \\[universal-argument]; \
when called from Lisp, with
optional argument PREVIEW non-nil), don't actually apply changes to
DIRECTORY, but instead show all those changes in a `diff-mode' buffer
with `default-directory' set to DIRECTORY.
You can then selectively apply changes with `diff-mode' commands like
`diff-apply-hunk' and `diff-apply-buffer'.

If any changes to be moved or copied can't be applied to DIRECTORY, it
is an error, and (except with \\[universal-argument] \\[universal-argument]) \
no changes are applied.
If any changes to be moved can't be reverse-applied to this working
tree, it is an error, and no changes are moved."
  (interactive
   (list
    (vc--prompt-other-working-tree
     (vc-responsible-backend default-directory)
     (format "%s changes to working tree"
             (if (equal current-prefix-arg '(4)) "Move" "Apply")))
    (equal current-prefix-arg '(4))
    (equal current-prefix-arg '(16))))
  (cond ((and move preview)
         (error "Invalid arguments to vc-apply-root-to-other-working-tree"))
        (preview
         ;; In this mode, no need to abort if some hunks aren't
         ;; applicable.
         (vc-root-diff nil t)
         (setq default-directory directory)
         (message
          (substitute-command-keys
           "Use \\[diff-hunk-kill] to kill hunks not to be copied \
then \\[diff-apply-buffer] to copy changes,
or use \\[diff-apply-hunk] to copy individual hunks.  \
Type \\[describe-mode] for more commands")))
        (t
         (let ((default-directory (vc-root-dir)))
           (vc--apply-to-other-working-tree directory directory
                                            `(,(vc-deduce-backend)
                                              (,default-directory))
                                            nil move)))))

(defcustom vc-no-confirm-moving-changes nil
  "Whether VC commands prompt before moving changes between working trees.

Normally the commands \\[vc-apply-to-other-working-tree] \
and \\[vc-apply-root-to-other-working-tree] prompt for confirmation
when asked to move changes between working trees (i.e., when invoked
with a prefix argument).  This is because it can be surprising to have
work disappear from your current working tree.  You can customize this
option to non-nil to skip the prompting."
  :type '(choice (const :tag "Prompt before moving changes" nil)
                 (const :tag "Move changes without prompting" t))
  :group 'vc
  :version "31.1")

(defun vc--fileset-by-state (fileset)
  "Return alist of VC states of all files in FILESET.
The keys into the alist are VC states, and the values are file names.
For directories in FILESET, the alist includes values for all
non-ignored, non-up-to-date files within those directories."
  (let ((backend (car fileset))
        (remaining (cadr fileset))
        ret-val)
    (while remaining
      (cond* ((bind* (next (pop remaining))))
             ((atom next)
              (push next (alist-get (vc-state next backend) ret-val)))
             ((bind* (file (car next))))
             ((file-directory-p file)
              (setq remaining
                    (nconc (vc-dir-status-files file nil backend)
                           remaining)))
             (t
              (push file (alist-get (cadr next) ret-val)))))
    ret-val))

(declare-function diff-kill-creations-deletions "diff-mode")
(declare-function diff-filename-drop-dir "diff-mode")
(declare-function diff-hunk-file-names "diff-mode")
(declare-function diff-file-next "diff-mode")
(defvar diff-hunk-header-re)
(declare-function vc-dir-resynch-file "vc-dir")

(defun vc--apply-to-other-working-tree
    (directory mirror-dir fileset patch-string move)
  "Workhorse routine for copying/moving changes to other working trees.
DIRECTORY is the root of the target working tree
(used only for messages).
MIRROR-DIR is the target directory for application.
FILESET is the VC fileset from which to copy changes.
PATCH-STRING non-nil overrides calling `vc-diff-internal' on FILESET to
determine the changes to copy or move.
MOVE non-nil means to move instead of copy."
  (unless (or (not move)
              vc-no-confirm-moving-changes
              (y-or-n-p
               (format "Really %s uncommitted work out of this working tree?"
                       (propertize "move" 'face 'bold))))
    (user-error "Aborted"))
  (vc-buffer-sync-fileset fileset nil)
  (let* ((fileset (cl-list* (car fileset)
                            (mapcar #'file-relative-name (cadr fileset))
                            (cddr fileset)))
         (backend (car fileset))
         (by-state (vc--fileset-by-state fileset))
         (copies (append (alist-get 'added by-state)
                         (alist-get 'unregistered by-state)))
         (deletions (append (alist-get 'removed by-state)
                            (alist-get 'missing by-state)))
         (whole-files (append copies deletions))
         (orig-dd default-directory)
         non-empty-patch-p)
    (with-temp-buffer
      (cond* (patch-string
              (diff-mode)
              (let ((inhibit-read-only t)) ; `diff-default-read-only'.
                (insert patch-string)))
             ;; Some backends don't tolerate unregistered files
             ;; appearing in the fileset for a diff operation.
             ((bind* (diff-fileset
                      `(,backend ,(cl-set-difference
                                   (cadr fileset)
                                   (alist-get 'unregistered by-state))))))
             ;; An empty files list makes `vc-diff-internal' diff the
             ;; whole of `default-directory'.
             ((cadr diff-fileset)
              (cl-letf ((display-buffer-overriding-action
                         '(display-buffer-no-window (allow-no-window . t)))
                        ;; Try to disable, e.g., Git's rename detection.
                        ((symbol-value (vc-make-backend-sym backend
                                                            'diff-switches))
                         t))
                (vc-diff-internal nil diff-fileset nil nil nil
                                  (current-buffer))))
             (t (require 'diff-mode)))
      ;; We'll handle any `added', `removed', `missing' and
      ;; `unregistered' files in FILESET by copying or moving whole
      ;; files, so remove any of them that show up in the diff
      ;; (only `added' and `removed' should actually show up).
      (diff-kill-creations-deletions t)
      (goto-char (point-min))
      (if (not (setq non-empty-patch-p
                     (re-search-forward diff-hunk-header-re nil t)))
          ;; No hunks, so just sync WHOLE-FILES and skip over testing
          ;; reverse-application to the source working tree.
          (let ((default-directory mirror-dir))
            (vc-buffer-sync-fileset `(,backend ,whole-files) nil))
        ;; We cannot deal with renames, copies, and combinations of
        ;; renames and copies with ordinary changes detected by the VCS.
        ;; If we called `vc-diff-internal' just above then there shouldn't
        ;; be any, but check to make sure.  And if PATCH-STRING is non-nil
        ;; then we definitely need to check there aren't any.
        ;;
        ;; In order to be able to support these kinds of things, then
        ;; rather than do it entirely ad hoc here, we probably want new
        ;; VC states representing renames and copies.
        ;; There is an old FIXME about this in `vc-state'.  --spwhitton
        (cl-loop initially
                 (goto-char (point-min))
                 (ignore-errors (diff-file-next))
                 for (name1 name2) = (diff-hunk-file-names)
                 for name1* = (or (diff-filename-drop-dir name1) name1)
                 and name2* = (or (diff-filename-drop-dir name2) name2)
                 unless (equal name1* name2*)
                 do (funcall (if patch-string #'user-error #'error)
                             (format "Cannot %s renames and/or copies"
                                     (if move "move" "apply")))
                 until (eq (prog1 (point)
                             (ignore-errors (diff-file-next)))
                           (point)))
        (let* ((default-directory mirror-dir)
               (sync-fileset (diff-vc-deduce-fileset)))
          (rplacd (last (cadr sync-fileset)) whole-files)
          (vc-buffer-sync-fileset sync-fileset nil))
        (when-let* (move
                    (failed (diff-apply-buffer nil nil 'reverse 'test)))
          ;; If PATCH-STRING is non-nil and this fails, the user called us
          ;; from a `diff-mode' buffer that doesn't reverse-apply; that's
          ;; a `user-error'.
          ;; If PATCH-STRING is nil and this fails, `vc-diff-internal'
          ;; generated a nonsense diff -- not the user's fault.
          (funcall
           (if patch-string #'user-error #'error)
           (ngettext "%d hunk does not reverse-apply to this working tree"
                     "%d hunks do not reverse-apply to this working tree"
                     failed)
           failed)))
      (let ((default-directory mirror-dir)
            (mirror-states (make-hash-table :test #'equal)))
        (pcase-dolist (`(,file ,state . ,_)
                       (vc-dir-status-files mirror-dir nil backend))
          (puthash file state mirror-states))
        (dolist (copy copies)
          (when (file-exists-p copy)
            (user-error "`%s' already exists in `%s'"
                        copy mirror-dir)))
        (dolist (deletion deletions)
          (when (memq (gethash deletion mirror-states)
                      '(edited needs-merge unlocked-changes added
                               conflict unregistered))
            (user-error "`%s' in `%s' has incompatible state `%s'"
                        deletion mirror-dir
                        (gethash deletion mirror-states))))
        (when-let* (non-empty-patch-p
                    (failed (diff-apply-buffer)))
          (user-error (ngettext "%d hunk does not apply to `%s'"
                                "%d hunks do not apply to `%s'"
                                failed)
                      failed directory))
        ;; For both `added' & `unregistered' files we leave them
        ;; unregistered in the target working tree, and for `removed' &
        ;; `missing' files we leave them missing.  This means that if
        ;; the user wants to throw away their copied changes it's less
        ;; effort to do so.  If the user does want to check in the
        ;; copied changes then VC-Dir will implicitly handle registering
        ;; the additions and deletions as part of `vc-checkin'.
        (dolist (copy copies)
          (copy-file (expand-file-name copy orig-dd) copy))
        (mapc #'delete-file deletions)
        (when vc-dir-buffers
          (mapc #'vc-dir-resynch-file whole-files)))
      (when move
        (diff-apply-buffer nil nil 'reverse)
        (mapc (lambda (f) (vc-call-backend backend 'unregister f))
              (alist-get 'added by-state))
        (mapc #'delete-file copies)
        (when vc-dir-buffers
          (mapc #'vc-dir-resynch-file copies))
        (vc-revert-files backend deletions))
      (message "Changes %s to `%s'"
               (if move "moved" "applied") directory))))

;;;###autoload
(defun vc-kill-other-working-tree-buffers (backend)
  "Kill buffers visiting versions of this file in other working trees.
BACKEND is the VC backend.

This command kills the buffers that \\[vc-switch-working-tree] switches to,
except that this command works only in file-visiting buffers."
  (interactive (list (vc-responsible-backend default-directory)))
  (when (cdr uniquify-managed)
    (cl-loop with trees = (vc-call-backend backend
                                           'known-other-working-trees)
             for item in uniquify-managed
             for buf = (uniquify-item-buffer item)
             when (and (not (eq buf (current-buffer)))
                       (cl-find (uniquify-item-dirname item) trees
                                :test #'file-in-directory-p))
             do (kill-buffer buf))))

(defun vc-default-cherry-pick-comment (files rev reverse)
  (if reverse (format "Summary: Reverse-apply changes from revision %s\n\n"
                      rev)
    (and-let* ((fn (vc-find-backend-function
                    (vc-responsible-backend default-directory)
                    'get-change-comment)))
      (format "Summary: %s\n" (string-trim (funcall fn files rev))))))

(defalias 'vc-default-working-branch #'ignore)
(defalias 'vc-default-trunk-or-topic-p #'ignore)



;; These things should probably be generally available
(defun vc-file-tree-walk (dirname func &rest args)
  "Walk recursively through DIRNAME.
Invoke FUNC f ARGS on each VC-managed file f underneath it."
  (vc-file-tree-walk-internal (expand-file-name dirname) func args)
  (message "Traversing directory %s...done" dirname))

(defun vc-file-tree-walk-internal (file func args)
  (if (not (file-directory-p file))
      (when (vc-backend file) (apply func file args))
    (message "Traversing directory %s..." (abbreviate-file-name file))
    (let ((dir (file-name-as-directory file)))
      (mapcar
       (lambda (f) (or
               (string-equal f ".")
               (string-equal f "..")
               (member f vc-directory-exclusion-list)
               (let ((dirf (expand-file-name f dir)))
                 (or
                  (file-symlink-p dirf) ;; Avoid possible loops.
                  (vc-file-tree-walk-internal dirf func args)))))
       (directory-files dir)))))

(provide 'vc)

;;; vc.el ends here
