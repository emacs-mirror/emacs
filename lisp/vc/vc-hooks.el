;;; vc-hooks.el --- resident support for version-control

;; Copyright (C) 1992-1996, 1998-2018 Free Software Foundation, Inc.

;; Author:     FSF (see vc.el for full credits)
;; Maintainer: emacs-devel@gnu.org
;; Package: vc

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

;; This is the always-loaded portion of VC.  It takes care of
;; VC-related activities that are done when you visit a file, so that
;; vc.el itself is loaded only when you use a VC command.  See the
;; commentary of vc.el.

;;; Code:

(eval-when-compile (require 'cl-lib))

;; Faces

(defgroup vc-faces nil
  "Faces used in the mode line by the VC state indicator."
  :group 'vc
  :group 'mode-line
  :version "25.1")

(defface vc-state-base
  '((default))
  "Base face for VC state indicator."
  :group 'vc-faces
  :group 'mode-line
  :version "25.1")

(defface vc-up-to-date-state
  '((default :inherit vc-state-base))
  "Face for VC modeline state when the file is up to date."
  :version "25.1"
  :group 'vc-faces)

(defface vc-needs-update-state
  '((default :inherit vc-state-base))
  "Face for VC modeline state when the file needs update."
  :version "25.1"
  :group 'vc-faces)

(defface vc-locked-state
  '((default :inherit vc-state-base))
  "Face for VC modeline state when the file locked."
  :version "25.1"
  :group 'vc-faces)

(defface vc-locally-added-state
  '((default :inherit vc-state-base))
  "Face for VC modeline state when the file is locally added."
  :version "25.1"
  :group 'vc-faces)

(defface vc-conflict-state
  '((default :inherit vc-state-base))
  "Face for VC modeline state when the file contains merge conflicts."
  :version "25.1"
  :group 'vc-faces)

(defface vc-removed-state
  '((default :inherit vc-state-base))
  "Face for VC modeline state when the file was removed from the VC system."
  :version "25.1"
  :group 'vc-faces)

(defface vc-missing-state
  '((default :inherit vc-state-base))
  "Face for VC modeline state when the file is missing from the file system."
  :version "25.1"
  :group 'vc-faces)

(defface vc-edited-state
  '((default :inherit vc-state-base))
  "Face for VC modeline state when the file is edited."
  :version "25.1"
  :group 'vc-faces)

;; Customization Variables (the rest is in vc.el)

(defcustom vc-ignore-dir-regexp
  ;; Stop SMB, automounter, AFS, and DFS host lookups.
  locate-dominating-stop-dir-regexp
  "Regexp matching directory names that are not under VC's control.
The default regexp prevents fruitless and time-consuming attempts
to determine the VC status in directories in which filenames are
interpreted as hostnames."
  :type 'regexp
  :group 'vc)

(defcustom vc-handled-backends '(RCS CVS SVN SCCS SRC Bzr Git Hg Mtn)
  ;; RCS, CVS, SVN, SCCS, and SRC come first because they are per-dir
  ;; rather than per-tree.  RCS comes first because of the multibackend
  ;; support intended to use RCS for local commits (with a remote CVS server).
  "List of version control backends for which VC will be used.
Entries in this list will be tried in order to determine whether a
file is under that sort of version control.
Removing an entry from the list prevents VC from being activated
when visiting a file managed by that backend.
An empty list disables VC altogether."
  :type '(repeat symbol)
  :version "25.1"
  :group 'vc)

;; Note: we don't actually have a darcs back end yet.
;; Also, Arch is unsupported, and the Meta-CVS back end has been removed.
;; The Arch back end will be retrieved and fixed if it is ever required.
(defcustom vc-directory-exclusion-list (purecopy '("SCCS" "RCS" "CVS" "MCVS"
					 ".src" ".svn" ".git" ".hg" ".bzr"
					 "_MTN" "_darcs" "{arch}"))
  "List of directory names to be ignored when walking directory trees."
  :type '(repeat string)
  :group 'vc)

(defcustom vc-make-backup-files nil
  "If non-nil, backups of registered files are made as with other files.
If nil (the default), files covered by version control don't get backups."
  :type 'boolean
  :group 'vc
  :group 'backup)

(defcustom vc-follow-symlinks 'ask
  "What to do if visiting a symbolic link to a file under version control.
Editing such a file through the link bypasses the version control system,
which is dangerous and probably not what you want.

If this variable is t, VC follows the link and visits the real file,
telling you about it in the echo area.  If it is `ask', VC asks for
confirmation whether it should follow the link.  If nil, the link is
visited and a warning displayed."
  :type '(choice (const :tag "Ask for confirmation" ask)
		 (const :tag "Visit link and warn" nil)
		 (const :tag "Follow link" t))
  :group 'vc)

(defcustom vc-display-status t
  "If non-nil, display revision number and lock status in mode line.
Otherwise, not displayed."
  :type 'boolean
  :group 'vc)


(defcustom vc-consult-headers t
  "If non-nil, identify work files by searching for version headers."
  :type 'boolean
  :group 'vc)

;;; This is handled specially now.
;; Tell Emacs about this new kind of minor mode
;; (add-to-list 'minor-mode-alist '(vc-mode vc-mode))

;;;###autoload
(put 'vc-mode 'risky-local-variable t)
(make-variable-buffer-local 'vc-mode)
(put 'vc-mode 'permanent-local t)

;;; We signal this error when we try to do something a VC backend
;;; doesn't support.  Two arguments: the method that's not supported
;;; and the backend
(define-error 'vc-not-supported "VC method not implemented for backend")

(defun vc-mode (&optional _arg)
  ;; Dummy function for C-h m
  "Version Control minor mode.
This minor mode is automatically activated whenever you visit a file under
control of one of the revision control systems in `vc-handled-backends'.
VC commands are globally reachable under the prefix `\\[vc-prefix-map]':
\\{vc-prefix-map}")

(defmacro vc-error-occurred (&rest body)
  `(condition-case nil (progn ,@body nil) (error t)))

;; We need a notion of per-file properties because the version
;; control state of a file is expensive to derive --- we compute
;; them when the file is initially found, keep them up to date
;; during any subsequent VC operations, and forget them when
;; the buffer is killed.

(defvar vc-file-prop-obarray (make-vector 17 0)
  "Obarray for per-file properties.")

(defvar vc-touched-properties nil)

(defun vc-file-setprop (file property value)
  "Set per-file VC PROPERTY for FILE to VALUE."
  (if (and vc-touched-properties
	   (not (memq property vc-touched-properties)))
      (setq vc-touched-properties (append (list property)
					  vc-touched-properties)))
  (put (intern (expand-file-name file) vc-file-prop-obarray) property value))

(defun vc-file-getprop (file property)
  "Get per-file VC PROPERTY for FILE."
  (get (intern (expand-file-name file) vc-file-prop-obarray) property))

(defun vc-file-clearprops (file)
  "Clear all VC properties of FILE."
  (if (boundp 'vc-parent-buffer)
      (kill-local-variable 'vc-parent-buffer))
  (setplist (intern (expand-file-name file) vc-file-prop-obarray) nil))


;; We keep properties on each symbol naming a backend as follows:
;;  * `vc-functions': an alist mapping vc-FUNCTION to vc-BACKEND-FUNCTION.

(defun vc-make-backend-sym (backend sym)
  "Return BACKEND-specific version of VC symbol SYM."
  (intern (concat "vc-" (downcase (symbol-name backend))
		  "-" (symbol-name sym))))

(defun vc-find-backend-function (backend fun)
  "Return BACKEND-specific implementation of FUN.
If there is no such implementation, return the default implementation;
if that doesn't exist either, return nil."
  (let ((f (vc-make-backend-sym backend fun)))
    (if (fboundp f) f
      ;; Load vc-BACKEND.el if needed.
      (require (intern (concat "vc-" (downcase (symbol-name backend)))))
      (if (fboundp f) f
	(let ((def (vc-make-backend-sym 'default fun)))
	  (if (fboundp def) (cons def backend) nil))))))

(defun vc-call-backend (backend function-name &rest args)
  "Call for BACKEND the implementation of FUNCTION-NAME with the given ARGS.
Calls

    (apply \\='vc-BACKEND-FUN ARGS)

if vc-BACKEND-FUN exists (after trying to find it in vc-BACKEND.el)
and else calls

    (apply \\='vc-default-FUN BACKEND ARGS)

It is usually called via the `vc-call' macro."
  (let ((f (assoc function-name (get backend 'vc-functions))))
    (if f (setq f (cdr f))
      (setq f (vc-find-backend-function backend function-name))
      (push (cons function-name f) (get backend 'vc-functions)))
    (cond
      ((null f)
       (signal 'vc-not-supported (list function-name backend)))
      ((consp f)	(apply (car f) (cdr f) args))
      (t		(apply f args)))))

(defmacro vc-call (fun file &rest args)
  "A convenience macro for calling VC backend functions.
Functions called by this macro must accept FILE as the first argument.
ARGS specifies any additional arguments.  FUN should be unquoted.
BEWARE!! FILE is evaluated twice!!"
  `(vc-call-backend (vc-backend ,file) ',fun ,file ,@args))

(defsubst vc-parse-buffer (pattern i)
  "Find PATTERN in the current buffer and return its Ith submatch."
  (goto-char (point-min))
  (if (re-search-forward pattern nil t)
      (match-string i)))

(defun vc-insert-file (file &optional limit blocksize)
  "Insert the contents of FILE into the current buffer.

Optional argument LIMIT is a regexp.  If present, the file is inserted
in chunks of size BLOCKSIZE (default 8 kByte), until the first
occurrence of LIMIT is found.  Anything from the start of that occurrence
to the end of the buffer is then deleted.  The function returns
non-nil if FILE exists and its contents were successfully inserted."
  (erase-buffer)
  (when (file-exists-p file)
    (if (not limit)
        (insert-file-contents file)
      (unless blocksize (setq blocksize 8192))
      (let ((filepos 0))
        (while
	    (and (< 0 (cadr (insert-file-contents
			     file nil filepos (cl-incf filepos blocksize))))
		 (progn (beginning-of-line)
                        (let ((pos (re-search-forward limit nil 'move)))
                          (when pos (delete-region (match-beginning 0)
						   (point-max)))
                          (not pos)))))))
    (set-buffer-modified-p nil)
    t))

(defun vc-find-root (file witness)
  "Find the root of a checked out project.
The function walks up the directory tree from FILE looking for WITNESS.
If WITNESS if not found, return nil, otherwise return the root."
  (let ((locate-dominating-stop-dir-regexp
         (or vc-ignore-dir-regexp locate-dominating-stop-dir-regexp)))
    (locate-dominating-file file witness)))

;; Access functions to file properties
;; (Properties should be _set_ using vc-file-setprop, but
;; _retrieved_ only through these functions, which decide
;; if the property is already known or not.  A property should
;; only be retrieved by vc-file-getprop if there is no
;; access function.)

;; properties indicating the backend being used for FILE

(defun vc-registered (file)
  "Return non-nil if FILE is registered in a version control system.

This function performs the check each time it is called.  To rely
on the result of a previous call, use `vc-backend' instead.  If the
file was previously registered under a certain backend, then that
backend is tried first."
  (let (handler)
    (cond
     ((and (file-name-directory file)
           (string-match vc-ignore-dir-regexp (file-name-directory file)))
      nil)
     ((and (boundp 'file-name-handler-alist)
          (setq handler (find-file-name-handler file 'vc-registered)))
      ;; handler should set vc-backend and return t if registered
      (funcall handler 'vc-registered file))
     (t
      ;; There is no file name handler.
      ;; Try vc-BACKEND-registered for each handled BACKEND.
      (catch 'found
	(let ((backend (vc-file-getprop file 'vc-backend)))
	  (mapc
	   (lambda (b)
	     (and (vc-call-backend b 'registered file)
		  (vc-file-setprop file 'vc-backend b)
		  (throw 'found t)))
	   (if (or (not backend) (eq backend 'none))
	       vc-handled-backends
	     (cons backend vc-handled-backends))))
        ;; File is not registered.
        (vc-file-setprop file 'vc-backend 'none)
        nil)))))

(defun vc-backend (file-or-list)
  "Return the version control type of FILE-OR-LIST, nil if it's not registered.
If the argument is a list, the files must all have the same back end."
  ;; `file' can be nil in several places (typically due to the use of
  ;; code like (vc-backend buffer-file-name)).
  (cond ((stringp file-or-list)
	 (let ((property (vc-file-getprop file-or-list 'vc-backend)))
	   ;; Note that internally, Emacs remembers unregistered
	   ;; files by setting the property to `none'.
	   (cond ((eq property 'none) nil)
		 (property)
		 ;; vc-registered sets the vc-backend property
		 (t (if (vc-registered file-or-list)
			(vc-file-getprop file-or-list 'vc-backend)
		      nil)))))
	((and file-or-list (listp file-or-list))
	 (vc-backend (car file-or-list)))
	(t
	 nil)))


(defun vc-backend-subdirectory-name (file)
  "Return where the repository for the current directory is kept."
  (symbol-name (vc-backend file)))

(defun vc-checkout-model (backend files)
  "Indicate how FILES are checked out.

If FILES are not registered, this function always returns nil.
For registered files, the possible values are:

  `implicit'   FILES are always writable, and checked out `implicitly'
              when the user saves the first changes to the file.

  `locking'   FILES are read-only if up-to-date; user must type
              \\[vc-next-action] before editing.  Strict locking
              is assumed.

  `announce'  FILES are read-only if up-to-date; user must type
              \\[vc-next-action] before editing.  But other users
              may be editing at the same time."
  (vc-call-backend backend 'checkout-model files))

(defun vc-user-login-name (file)
  "Return the name under which the user accesses the given FILE."
  (or (and (file-remote-p file)
           ;; tramp case: execute "whoami" via tramp
           (let ((default-directory (file-name-directory file))
		 process-file-side-effects)
             (with-temp-buffer
               (if (not (zerop (process-file "whoami" nil t)))
                   ;; fall through if "whoami" didn't work
                   nil
                 ;; remove trailing newline
                 (delete-region (1- (point-max)) (point-max))
                 (buffer-string)))))
      ;; normal case
      (user-login-name)
      ;; if user-login-name is nil, return the UID as a string
      (number-to-string (user-uid))))

(defun vc-state (file &optional backend)
  "Return the version control state of FILE.

A return of nil from this function means we have no information on the
status of this file.  Otherwise, the value returned is one of:

  `up-to-date'       The working file is unmodified with respect to the
                     latest version on the current branch, and not locked.

  `edited'           The working file has been edited by the user.  If
                     locking is used for the file, this state means that
                     the current version is locked by the calling user.
                     This status should *not* be reported for files
                     which have a changed mtime but the same content
                     as the repo copy.

  USER               The current version of the working file is locked by
                     some other USER (a string).

  `needs-update'     The file has not been edited by the user, but there is
                     a more recent version on the current branch stored
                     in the repository.

  `needs-merge'      The file has been edited by the user, and there is also
                     a more recent version on the current branch stored in
                     the repository.  This state can only occur if locking
                     is not used for the file.

  `unlocked-changes' The working version of the file is not locked,
                     but the working file has been changed with respect
                     to that version.  This state can only occur for files
                     with locking; it represents an erroneous condition that
                     should be resolved by the user (vc-next-action will
                     prompt the user to do it).

  `added'            Scheduled to go into the repository on the next commit.
                     Often represented by vc-working-revision = \"0\" in VCSes
                     with monotonic IDs like Subversion and Mercurial.

  `removed'          Scheduled to be deleted from the repository on next commit.

  `conflict'         The file contains conflicts as the result of a merge.
                     For now the conflicts are text conflicts.  In the
                     future this might be extended to deal with metadata
                     conflicts too.

  `missing'          The file is not present in the file system, but the VC
                     system still tracks it.

  `ignored'          The file showed up in a dir-status listing with a flag
                     indicating the version-control system is ignoring it,
                     Note: This property is not set reliably (some VCSes
                     don't have useful directory-status commands) so assume
                     that any file with vc-state nil might be ignorable
                     without VC knowing it.

  `unregistered'     The file is not under version control."

  ;; Note: we usually return nil here for unregistered files anyway
  ;; when called with only one argument.  This doesn't seem to cause
  ;; any problems.  But if we wanted to change that, we should
  ;; probably opt for redefining the `registered' command to return
  ;; non-nil even for unregistered files (maybe also rename it), and
  ;; then make sure that all `state' implementations handle
  ;; unregistered file appropriately.

  ;; FIXME: New (sub)states needed (?):
  ;; - `copied' and `moved' (might be handled by `removed' and `added')
  (or (vc-file-getprop file 'vc-state)
      (when (> (length file) 0)         ;Why??  --Stef
        (setq backend (or backend (vc-backend file)))
        (when backend
          (vc-state-refresh file backend)))))

(defun vc-state-refresh (file backend)
  "Quickly recompute the `state' of FILE."
  (vc-file-setprop
   file 'vc-state
   (vc-call-backend backend 'state file)))

(defsubst vc-up-to-date-p (file)
  "Convenience function that checks whether `vc-state' of FILE is `up-to-date'."
  (eq (vc-state file) 'up-to-date))

(defun vc-working-revision (file &optional backend)
  "Return the repository version from which FILE was checked out.
If FILE is not registered, this function always returns nil."
  (or (vc-file-getprop file 'vc-working-revision)
      (progn
        (setq backend (or backend (vc-backend file)))
        (when backend
          (vc-file-setprop file 'vc-working-revision
                           (vc-call-backend
                            backend 'working-revision file))))))

;; Backward compatibility.
(define-obsolete-function-alias
  'vc-workfile-version 'vc-working-revision "23.1")
(defun vc-default-working-revision (backend file)
  (message
   "`working-revision' not found: using the old `workfile-version' instead")
  (vc-call-backend backend 'workfile-version file))

(defun vc-default-registered (backend file)
  "Check if FILE is registered in BACKEND using vc-BACKEND-master-templates."
  (let ((sym (vc-make-backend-sym backend 'master-templates)))
    (unless (get backend 'vc-templates-grabbed)
      (put backend 'vc-templates-grabbed t))
    (let ((result (vc-check-master-templates file (symbol-value sym))))
      (if (stringp result)
	  (vc-file-setprop file 'vc-master-name result)
	nil))))				; Not registered

;;;###autoload
(defun vc-possible-master (s dirname basename)
  (cond
   ((stringp s) (format s dirname basename))
   ((functionp s)
    ;; The template is a function to invoke.  If the
    ;; function returns non-nil, that means it has found a
    ;; master.  For backward compatibility, we also handle
    ;; the case that the function throws a 'found atom
    ;; and a pair (cons MASTER-FILE BACKEND).
    (let ((result (catch 'found (funcall s dirname basename))))
      (if (consp result) (car result) result)))))

(defun vc-check-master-templates (file templates)
  "Return non-nil if there is a master corresponding to FILE.

TEMPLATES is a list of strings or functions.  If an element is a
string, it must be a control string as required by `format', with two
string placeholders, such as \"%sRCS/%s,v\".  The directory part of
FILE is substituted for the first placeholder, the basename of FILE
for the second.  If a file with the resulting name exists, it is taken
as the master of FILE, and returned.

If an element of TEMPLATES is a function, it is called with the
directory part and the basename of FILE as arguments.  It should
return non-nil if it finds a master; that value is then returned by
this function."
  (let ((dirname (or (file-name-directory file) ""))
        (basename (file-name-nondirectory file)))
    (catch 'found
      (mapcar
       (lambda (s)
	 (let ((trial (vc-possible-master s dirname basename)))
	   (when (and trial (file-exists-p trial)
		      ;; Make sure the file we found with name
		      ;; TRIAL is not the source file itself.
		      ;; That can happen with RCS-style names if
		      ;; the file name is truncated (e.g. to 14
		      ;; chars).  See if either directory or
		      ;; attributes differ.
		      (or (not (string= dirname
					(file-name-directory trial)))
			  (not (equal (file-attributes file)
				      (file-attributes trial)))))
	       (throw 'found trial))))
       templates))))


;; toggle-read-only is obsolete since 24.3, but since vc-t-r-o was made
;; obsolete earlier, it is ok for the latter to be an alias to the former,
;; since the latter will be removed first.  We can't just make it
;; an alias for read-only-mode, since that is not 100% the same.
(defalias 'vc-toggle-read-only 'toggle-read-only)
(make-obsolete 'vc-toggle-read-only
               "use `read-only-mode' instead (or `toggle-read-only' in older versions of Emacs)."
               "24.1")

(defun vc-default-make-version-backups-p (_backend _file)
  "Return non-nil if unmodified versions should be backed up locally.
The default is to switch off this feature."
  nil)

(defun vc-version-backup-file-name (file &optional rev manual regexp)
  "Return a backup file name for REV or the current version of FILE.
If MANUAL is non-nil it means that a name for backups created by
the user should be returned; if REGEXP is non-nil that means to return
a regexp for matching all such backup files, regardless of the version."
  (if regexp
      (concat (regexp-quote (file-name-nondirectory file))
              "\\.~.+" (unless manual "\\.") "~")
    (expand-file-name (concat (file-name-nondirectory file)
                              ".~" (subst-char-in-string
                                    ?/ ?_ (or rev (vc-working-revision file)))
                              (unless manual ".") "~")
                      (file-name-directory file))))

(defun vc-delete-automatic-version-backups (file)
  "Delete all existing automatic version backups for FILE."
  (condition-case nil
      (mapc
       'delete-file
       (directory-files (or (file-name-directory file) default-directory) t
			(vc-version-backup-file-name file nil nil t)))
    ;; Don't fail when the directory doesn't exist.
    (file-error nil)))

(defun vc-make-version-backup (file)
  "Make a backup copy of FILE, which is assumed in sync with the repository.
Before doing that, check if there are any old backups and get rid of them."
  (unless (and (fboundp 'msdos-long-file-names)
               (not (with-no-warnings (msdos-long-file-names))))
    (vc-delete-automatic-version-backups file)
    (condition-case nil
        (copy-file file (vc-version-backup-file-name file)
                   nil 'keep-date)
      ;; It's ok if it doesn't work (e.g. directory not writable),
      ;; since this is just for efficiency.
      (file-error
       (message
        (concat "Warning: Cannot make version backup; "
                "diff/revert therefore not local"))))))

(defun vc-before-save ()
  "Function to be called by `basic-save-buffer' (in files.el)."
  ;; If the file on disk is still in sync with the repository,
  ;; and version backups should be made, copy the file to
  ;; another name.  This enables local diffs and local reverting.
  (let ((file buffer-file-name)
        backend)
    (ignore-errors               ;Be careful not to prevent saving the file.
      (unless (file-exists-p file)
        (vc-file-clearprops file))
      (and (setq backend (vc-backend file))
           (vc-up-to-date-p file)
           (eq (vc-checkout-model backend (list file)) 'implicit)
           (vc-call-backend backend 'make-version-backups-p file)
           (vc-make-version-backup file)))))

(declare-function vc-dir-resynch-file "vc-dir" (&optional fname))

(defvar vc-dir-buffers nil "List of vc-dir buffers.")

(defun vc-after-save ()
  "Function to be called by `basic-save-buffer' (in files.el)."
  ;; If the file in the current buffer is under version control,
  ;; up-to-date, and locking is not used for the file, set
  ;; the state to 'edited and redisplay the mode line.
  (let* ((file buffer-file-name)
         (backend (vc-backend file)))
    (cond
     ((null backend))
     ((eq (vc-checkout-model backend (list file)) 'implicit)
      ;; If the file was saved in the same second in which it was
      ;; checked out, clear the checkout-time to avoid confusion.
      (if (equal (vc-file-getprop file 'vc-checkout-time)
		 (nth 5 (file-attributes file)))
	  (vc-file-setprop file 'vc-checkout-time nil))
      (if (vc-state-refresh file backend)
	  (vc-mode-line file backend)))
     ;; If we saved an unlocked file on a locking based VCS, that
     ;; file is not longer up-to-date.
     ((eq (vc-file-getprop file 'vc-state) 'up-to-date)
      (vc-file-setprop file 'vc-state nil)))
    ;; Resynch *vc-dir* buffers, if any are present.
    (when vc-dir-buffers
      (vc-dir-resynch-file file))))

(defvar vc-menu-entry
  `(menu-item ,(purecopy "Version Control") vc-menu-map
    :filter vc-menu-map-filter))

(when (boundp 'menu-bar-tools-menu)
  ;; We do not need to worry here about the placement of this entry
  ;; because menu-bar.el has already created the proper spot for us
  ;; and this will simply use it.
  (define-key menu-bar-tools-menu [vc] vc-menu-entry))

(defconst vc-mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] vc-menu-entry)
    map))

(defun vc-mode-line (file &optional backend)
  "Set `vc-mode' to display type of version control for FILE.
The value is set in the current buffer, which should be the buffer
visiting FILE.
If BACKEND is passed use it as the VC backend when computing the result."
  (interactive (list buffer-file-name))
  (setq backend (or backend (vc-backend file)))
  (if (not backend)
      (setq vc-mode nil)
    (let* ((ml-string (vc-call-backend backend 'mode-line-string file))
	   (ml-echo (get-text-property 0 'help-echo ml-string)))
      (setq vc-mode
	    (concat
	     " "
	     (if (null vc-display-status)
		 (symbol-name backend)
	       (propertize
		ml-string
		'mouse-face 'mode-line-highlight
		'help-echo
		(concat (or ml-echo
			    (format "File under the %s version control system"
				    backend))
			"\nmouse-1: Version Control menu")
		'local-map vc-mode-line-map)))))
    ;; If the user is root, and the file is not owner-writable,
    ;; then pretend that we can't write it
    ;; even though we can (because root can write anything).
    ;; This way, even root cannot modify a file that isn't locked.
    (and (equal file buffer-file-name)
	 (not buffer-read-only)
	 (zerop (user-real-uid))
	 (zerop (logand (file-modes buffer-file-name) 128))
	 (setq buffer-read-only t)))
  (force-mode-line-update)
  backend)

(defun vc-default-mode-line-string (backend file)
  "Return a string for `vc-mode-line' to put in the mode line for FILE.
Format:

  \"BACKEND-REV\"        if the file is up-to-date
  \"BACKEND:REV\"        if the file is edited (or locked by the calling user)
  \"BACKEND:LOCKER:REV\" if the file is locked by somebody else
  \"BACKEND@REV\"        if the file was locally added
  \"BACKEND!REV\"        if the file contains conflicts or was removed
  \"BACKEND?REV\"        if the file is under VC, but is missing

This function assumes that the file is registered."
  (let* ((backend-name (symbol-name backend))
	 (state   (vc-state file backend))
	 (state-echo nil)
	 (face nil)
	 (rev     (vc-working-revision file backend)))
    (propertize
     (cond ((or (eq state 'up-to-date)
		(eq state 'needs-update))
	    (setq state-echo "Up to date file")
	    (setq face 'vc-up-to-date-state)
	    (concat backend-name "-" rev))
	   ((stringp state)
	    (setq state-echo (concat "File locked by" state))
	    (setq face 'vc-locked-state)
	    (concat backend-name ":" state ":" rev))
           ((eq state 'added)
            (setq state-echo "Locally added file")
	    (setq face 'vc-locally-added-state)
            (concat backend-name "@" rev))
           ((eq state 'conflict)
            (setq state-echo "File contains conflicts after the last merge")
	    (setq face 'vc-conflict-state)
            (concat backend-name "!" rev))
           ((eq state 'removed)
            (setq state-echo "File removed from the VC system")
	    (setq face 'vc-removed-state)
            (concat backend-name "!" rev))
           ((eq state 'missing)
            (setq state-echo "File tracked by the VC system, but missing from the file system")
	    (setq face 'vc-missing-state)
            (concat backend-name "?" rev))
	   (t
	    ;; Not just for the 'edited state, but also a fallback
	    ;; for all other states.  Think about different symbols
	    ;; for 'needs-update and 'needs-merge.
	    (setq state-echo "Locally modified file")
	    (setq face 'vc-edited-state)
	    (concat backend-name ":" rev)))
     'face face
     'help-echo (concat state-echo " under the " backend-name
			" version control system"))))

(defun vc-follow-link ()
  "If current buffer visits a symbolic link, visit the real file.
If the real file is already visited in another buffer, make that buffer
current, and kill the buffer that visits the link."
  (let* ((true-buffer (find-buffer-visiting buffer-file-truename))
	 (this-buffer (current-buffer)))
    (if (eq true-buffer this-buffer)
	(let ((truename buffer-file-truename))
	  (kill-buffer this-buffer)
	  ;; In principle, we could do something like set-visited-file-name.
	  ;; However, it can't be exactly the same as set-visited-file-name.
	  ;; I'm not going to work out the details right now. -- rms.
	  (set-buffer (find-file-noselect truename)))
      (set-buffer true-buffer)
      (kill-buffer this-buffer))))

(defun vc-default-find-file-hook (_backend)
  nil)

(defun vc-refresh-state ()
  "Refresh the VC state of the current buffer's file.

This command is more thorough than `vc-state-refresh', in that it
also supports switching a back-end or removing the file from VC.
In the latter case, VC mode is deactivated for this buffer."
  (interactive)
  ;; Recompute whether file is version controlled,
  ;; if user has killed the buffer and revisited.
  (when vc-mode
    (setq vc-mode nil))
  (when buffer-file-name
    (vc-file-clearprops buffer-file-name)
    ;; FIXME: Why use a hook?  Why pass it buffer-file-name?
    (add-hook 'vc-mode-line-hook 'vc-mode-line nil t)
    (let (backend)
      (cond
        ((setq backend (with-demoted-errors (vc-backend buffer-file-name)))
         ;; Let the backend setup any buffer-local things he needs.
         (vc-call-backend backend 'find-file-hook)
	;; Compute the state and put it in the mode line.
	(vc-mode-line buffer-file-name backend)
	(unless vc-make-backup-files
	  ;; Use this variable, not make-backup-files,
	  ;; because this is for things that depend on the file name.
          (set (make-local-variable 'backup-inhibited) t)))
       ((let* ((truename (and buffer-file-truename
			      (expand-file-name buffer-file-truename)))
	       (link-type (and truename
			       (not (equal buffer-file-name truename))
			       (vc-backend truename))))
	  (cond ((not link-type) nil)	;Nothing to do.
		((eq vc-follow-symlinks nil)
		 (message
		  "Warning: symbolic link to %s-controlled source file" link-type))
		((or (not (eq vc-follow-symlinks 'ask))
		     ;; Assume we cannot ask, default to yes.
		     noninteractive
		     ;; Copied from server-start.  Seems like there should
		     ;; be a better way to ask "can we get user input?"...
		     (and (daemonp)
			  (null (cdr (frame-list)))
			  (eq (selected-frame) terminal-frame))
		     ;; If we already visited this file by following
		     ;; the link, don't ask again if we try to visit
		     ;; it again.  GUD does that, and repeated questions
		     ;; are painful.
		     (get-file-buffer
		      (abbreviate-file-name
		       (file-chase-links buffer-file-name))))

		 (vc-follow-link)
		 (message "Followed link to %s" buffer-file-name)
		 (vc-refresh-state))
		(t
		 (if (yes-or-no-p (format
				   "Symbolic link to %s-controlled source file; follow link? " link-type))
		     (progn (vc-follow-link)
			    (message "Followed link to %s" buffer-file-name)
			    (vc-refresh-state))
		   (message
		    "Warning: editing through the link bypasses version control")
		   )))))))))

(add-hook 'find-file-hook #'vc-refresh-state)
(define-obsolete-function-alias 'vc-find-file-hook 'vc-refresh-state "25.1")

(defun vc-kill-buffer-hook ()
  "Discard VC info about a file when we kill its buffer."
  (when buffer-file-name (vc-file-clearprops buffer-file-name)))

(add-hook 'kill-buffer-hook 'vc-kill-buffer-hook)

;; Now arrange for (autoloaded) bindings of the main package.
;; Bindings for this have to go in the global map, as we'll often
;; want to call them from random buffers.

;; Autoloading works fine, but it prevents shortcuts from appearing
;; in the menu because they don't exist yet when the menu is built.
;; (autoload 'vc-prefix-map "vc" nil nil 'keymap)
(defvar vc-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" 'vc-update-change-log)
    (define-key map "b" 'vc-switch-backend)
    (define-key map "d" 'vc-dir)
    (define-key map "g" 'vc-annotate)
    (define-key map "G" 'vc-ignore)
    (define-key map "h" 'vc-region-history)
    (define-key map "i" 'vc-register)
    (define-key map "l" 'vc-print-log)
    (define-key map "L" 'vc-print-root-log)
    (define-key map "I" 'vc-log-incoming)
    (define-key map "O" 'vc-log-outgoing)
    (define-key map "m" 'vc-merge)
    (define-key map "r" 'vc-retrieve-tag)
    (define-key map "s" 'vc-create-tag)
    (define-key map "u" 'vc-revert)
    (define-key map "v" 'vc-next-action)
    (define-key map "+" 'vc-update)
    ;; I'd prefer some kind of symmetry with vc-update:
    (define-key map "P" 'vc-push)
    (define-key map "=" 'vc-diff)
    (define-key map "D" 'vc-root-diff)
    (define-key map "~" 'vc-revision-other-window)
    (define-key map "x" 'vc-delete-file)
    map))
(fset 'vc-prefix-map vc-prefix-map)
(define-key ctl-x-map "v" 'vc-prefix-map)

(defvar vc-menu-map
  (let ((map (make-sparse-keymap "Version Control")))
    ;;(define-key map [show-files]
    ;;  '("Show Files under VC" . (vc-directory t)))
    (bindings--define-key map [vc-retrieve-tag]
      '(menu-item "Retrieve Tag" vc-retrieve-tag
		  :help "Retrieve tagged version or branch"))
    (bindings--define-key map [vc-create-tag]
      '(menu-item "Create Tag" vc-create-tag
		  :help "Create version tag"))
    (bindings--define-key map [separator1] menu-bar-separator)
    (bindings--define-key map [vc-annotate]
      '(menu-item "Annotate" vc-annotate
		  :help "Display the edit history of the current file using colors"))
    (bindings--define-key map [vc-rename-file]
      '(menu-item "Rename File" vc-rename-file
		  :help "Rename file"))
    (bindings--define-key map [vc-revision-other-window]
      '(menu-item "Show Other Version" vc-revision-other-window
		  :help "Visit another version of the current file in another window"))
    (bindings--define-key map [vc-diff]
      '(menu-item "Compare with Base Version" vc-diff
		  :help "Compare file set with the base version"))
    (bindings--define-key map [vc-root-diff]
      '(menu-item "Compare Tree with Base Version" vc-root-diff
		  :help "Compare current tree with the base version"))
    (bindings--define-key map [vc-update-change-log]
      '(menu-item "Update ChangeLog" vc-update-change-log
		  :help "Find change log file and add entries from recent version control logs"))
    (bindings--define-key map [vc-log-out]
      '(menu-item "Show Outgoing Log" vc-log-outgoing
		  :help "Show a log of changes that will be sent with a push operation"))
    (bindings--define-key map [vc-log-in]
      '(menu-item "Show Incoming Log" vc-log-incoming
		  :help "Show a log of changes that will be received with a pull operation"))
    (bindings--define-key map [vc-print-log]
      '(menu-item "Show History" vc-print-log
		  :help "List the change log of the current file set in a window"))
    (bindings--define-key map [vc-print-root-log]
      '(menu-item "Show Top of the Tree History " vc-print-root-log
		  :help "List the change log for the current tree in a window"))
    (bindings--define-key map [separator2] menu-bar-separator)
    (bindings--define-key map [vc-insert-header]
      '(menu-item "Insert Header" vc-insert-headers
		  :help "Insert headers into a file for use with a version control system.
"))
    (bindings--define-key map [vc-revert]
      '(menu-item "Revert to Base Version" vc-revert
		  :help "Revert working copies of the selected file set to their repository contents"))
    ;; TODO Only :enable if (vc-find-backend-function backend 'push)
    (bindings--define-key map [vc-push]
      '(menu-item "Push Changes" vc-push
		  :help "Push the current branch's changes"))
    (bindings--define-key map [vc-update]
      '(menu-item "Update to Latest Version" vc-update
		  :help "Update the current fileset's files to their tip revisions"))
    (bindings--define-key map [vc-next-action]
      '(menu-item "Check In/Out" vc-next-action
		  :help "Do the next logical version control operation on the current fileset"))
    (bindings--define-key map [vc-register]
      '(menu-item "Register" vc-register
		  :help "Register file set into a version control system"))
    (bindings--define-key map [vc-ignore]
      '(menu-item "Ignore File..." vc-ignore
		  :help "Ignore a file under current version control system"))
    (bindings--define-key map [vc-dir]
      '(menu-item "VC Dir"  vc-dir
		  :help "Show the VC status of files in a directory"))
    map))

(defalias 'vc-menu-map vc-menu-map)

(declare-function vc-responsible-backend "vc" (file))

(defun vc-menu-map-filter (orig-binding)
  (if (and (symbolp orig-binding) (fboundp orig-binding))
      (setq orig-binding (indirect-function orig-binding)))
  (let ((ext-binding
         (when vc-mode
	   (vc-call-backend
	    (if buffer-file-name
		(vc-backend buffer-file-name)
	      (vc-responsible-backend default-directory))
	    'extra-menu))))
    ;; Give the VC backend a chance to add menu entries
    ;; specific for that backend.
    (if (null ext-binding)
        orig-binding
      (append orig-binding
	      '((ext-menu-separator "--"))
              ext-binding))))

(defun vc-default-extra-menu (_backend)
  nil)

(provide 'vc-hooks)

;;; vc-hooks.el ends here
