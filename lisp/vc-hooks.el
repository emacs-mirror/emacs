;;; vc-hooks.el --- resident support for version-control

;; Copyright (C) 1992, 1993, 1994, 1995, 1996, 1998, 1999, 2000, 2001,
;;   2002, 2003, 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author:     FSF (see vc.el for full credits)
;; Maintainer: Andre Spiegel <spiegel@gnu.org>

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

;;; Commentary:

;; This is the always-loaded portion of VC.  It takes care of
;; VC-related activities that are done when you visit a file, so that
;; vc.el itself is loaded only when you use a VC command.  See the
;; commentary of vc.el.

;;; Code:

(eval-when-compile
  (require 'cl))

;; Customization Variables (the rest is in vc.el)

(defvar vc-ignore-vc-files nil)
(make-obsolete-variable 'vc-ignore-vc-files
                        "set `vc-handled-backends' to nil to disable VC.")

(defvar vc-master-templates ())
(make-obsolete-variable 'vc-master-templates
 "to define master templates for a given BACKEND, use
vc-BACKEND-master-templates.  To enable or disable VC for a given
BACKEND, use `vc-handled-backends'.")

(defvar vc-header-alist ())
(make-obsolete-variable 'vc-header-alist 'vc-BACKEND-header)

(defcustom vc-ignore-dir-regexp
  ;; Stop SMB, automounter, AFS, and DFS host lookups.
  "\\`\\(?:[\\/][\\/]\\|/\\(?:net\\|afs\\|\\.\\\.\\.\\)/\\)\\'"
  "Regexp matching directory names that are not under VC's control.
The default regexp prevents fruitless and time-consuming attempts
to determine the VC status in directories in which filenames are
interpreted as hostnames."
  :type 'regexp
  :group 'vc)

(defcustom vc-handled-backends '(RCS CVS SVN SCCS Bzr Git Hg Mtn Arch MCVS)
  ;; RCS, CVS, SVN and SCCS come first because they are per-dir
  ;; rather than per-tree.  RCS comes first because of the multibackend
  ;; support intended to use RCS for local commits (with a remote CVS server).
  "List of version control backends for which VC will be used.
Entries in this list will be tried in order to determine whether a
file is under that sort of version control.
Removing an entry from the list prevents VC from being activated
when visiting a file managed by that backend.
An empty list disables VC altogether."
  :type '(repeat symbol)
  :version "22.2"
  :group 'vc)

(defcustom vc-path
  (if (file-directory-p "/usr/sccs")
      '("/usr/sccs")
    nil)
  "*List of extra directories to search for version control commands."
  :type '(repeat directory)
  :group 'vc)

(defcustom vc-make-backup-files nil
  "*If non-nil, backups of registered files are made as with other files.
If nil (the default), files covered by version control don't get backups."
  :type 'boolean
  :group 'vc
  :group 'backup)

(defcustom vc-follow-symlinks 'ask
  "*What to do if visiting a symbolic link to a file under version control.
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
  "*If non-nil, display revision number and lock status in modeline.
Otherwise, not displayed."
  :type 'boolean
  :group 'vc)


(defcustom vc-consult-headers t
  "*If non-nil, identify work files by searching for version headers."
  :type 'boolean
  :group 'vc)

(defcustom vc-keep-workfiles t
  "*If non-nil, don't delete working files after registering changes.
If the back-end is CVS, workfiles are always kept, regardless of the
value of this flag."
  :type 'boolean
  :group 'vc)

(defcustom vc-mistrust-permissions nil
  "*If non-nil, don't assume permissions/ownership track version-control status.
If nil, do rely on the permissions.
See also variable `vc-consult-headers'."
  :type 'boolean
  :group 'vc)

(defun vc-mistrust-permissions (file)
  "Internal access function to variable `vc-mistrust-permissions' for FILE."
  (or (eq vc-mistrust-permissions 't)
      (and vc-mistrust-permissions
	   (funcall vc-mistrust-permissions
		    (vc-backend-subdirectory-name file)))))

(defcustom vc-stay-local t
  "*Non-nil means use local operations when possible for remote repositories.
This avoids slow queries over the network and instead uses heuristics
and past information to determine the current status of a file.

The value can also be a regular expression or list of regular
expressions to match against the host name of a repository; then VC
only stays local for hosts that match it.  Alternatively, the value
can be a list of regular expressions where the first element is the
symbol `except'; then VC always stays local except for hosts matched
by these regular expressions."
  :type '(choice (const :tag "Always stay local" t)
	  (const :tag "Don't stay local" nil)
	  (list :format "\nExamine hostname and %v" :tag "Examine hostname ..."
		(set :format "%v" :inline t (const :format "%t" :tag "don't" except))
		(regexp :format " stay local,\n%t: %v" :tag "if it matches")
		(repeat :format "%v%i\n" :inline t (regexp :tag "or"))))
  :version "22.1"
  :group 'vc)

(defun vc-stay-local-p (file)
  "Return non-nil if VC should stay local when handling FILE.
This uses the `repository-hostname' backend operation."
  (let* ((backend (vc-backend file))
	 (sym (vc-make-backend-sym backend 'stay-local))
	 (stay-local (if (boundp sym) (symbol-value sym) t)))
    (if (eq stay-local t) (setq stay-local vc-stay-local))
    (if (symbolp stay-local) stay-local
      (let ((dirname (if (file-directory-p file)
			 (directory-file-name file)
		       (file-name-directory file))))
	(eq 'yes
	    (or (vc-file-getprop dirname 'vc-stay-local-p)
		(vc-file-setprop
		 dirname 'vc-stay-local-p
		 (let ((hostname (vc-call-backend
				  backend 'repository-hostname dirname)))
		   (if (not hostname)
		       'no
		     (let ((default t))
		       (if (eq (car-safe stay-local) 'except)
			   (setq default nil stay-local (cdr stay-local)))
		       (when (consp stay-local)
			 (setq stay-local
			       (mapconcat 'identity stay-local "\\|")))
		       (if (if (string-match stay-local hostname)
			       default (not default))
			   'yes 'no)))))))))))

;;; This is handled specially now.
;; Tell Emacs about this new kind of minor mode
;; (add-to-list 'minor-mode-alist '(vc-mode vc-mode))

(make-variable-buffer-local 'vc-mode)
(put 'vc-mode 'permanent-local t)

(defun vc-mode (&optional arg)
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
  (put (intern file vc-file-prop-obarray) property value))

(defun vc-file-getprop (file property)
  "Get per-file VC PROPERTY for FILE."
  (get (intern file vc-file-prop-obarray) property))

(defun vc-file-clearprops (file)
  "Clear all VC properties of FILE."
  (setplist (intern file vc-file-prop-obarray) nil))


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

    (apply 'vc-BACKEND-FUN ARGS)

if vc-BACKEND-FUN exists (after trying to find it in vc-BACKEND.el)
and else calls

    (apply 'vc-default-FUN BACKEND ARGS)

It is usually called via the `vc-call' macro."
  (let ((f (assoc function-name (get backend 'vc-functions))))
    (if f (setq f (cdr f))
      (setq f (vc-find-backend-function backend function-name))
      (push (cons function-name f) (get backend 'vc-functions)))
    (cond
     ((null f)
      (error "Sorry, %s is not implemented for %s" function-name backend))
     ((consp f)	(apply (car f) (cdr f) args))
     (t		(apply f args)))))

(defmacro vc-call (fun file &rest args)
  ;; BEWARE!! `file' is evaluated twice!!
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
      (if (not blocksize) (setq blocksize 8192))
      (let ((filepos 0))
        (while
	    (and (< 0 (cadr (insert-file-contents
			     file nil filepos (incf filepos blocksize))))
		 (progn (beginning-of-line)
                        (let ((pos (re-search-forward limit nil 'move)))
                          (if pos (delete-region (match-beginning 0)
                                                 (point-max)))
                          (not pos)))))))
    (set-buffer-modified-p nil)
    t))

(defun vc-find-root (file witness)
  "Find the root of a checked out project.
The function walks up the directory tree from FILE looking for WITNESS.
If WITNESS if not found, return nil, otherwise return the root."
  ;; Represent /home/luser/foo as ~/foo so that we don't try to look for
  ;; witnesses in /home or in /.
  (while (and (not (file-directory-p file)) (file-exists-p file))
    (setq file (file-name-directory (directory-file-name file))))
  (setq file (abbreviate-file-name file))
  (let ((root nil)
        (user (nth 2 (file-attributes file))))
    (while (not (or root
                    (null file)
                    ;; As a heuristic, we stop looking up the hierarchy of
                    ;; directories as soon as we find a directory belonging
                    ;; to another user.  This should save us from looking in
                    ;; things like /net and /afs.  This assumes that all the
                    ;; files inside a project belong to the same user.
                    (not (equal user (nth 2 (file-attributes file))))
                    (string-match vc-ignore-dir-regexp file)))
      (if (file-exists-p (expand-file-name witness file))
          (setq root file)
        (if (equal file
                   (setq file (file-name-directory (directory-file-name file))))
            (setq file nil))))
    root))

;; Access functions to file properties
;; (Properties should be _set_ using vc-file-setprop, but
;; _retrieved_ only through these functions, which decide
;; if the property is already known or not. A property should
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
     ((string-match vc-ignore-dir-regexp (file-name-directory file)) nil)
     ((and (boundp 'file-name-handler-alist)
          (setq handler (find-file-name-handler file 'vc-registered)))
      ;; handler should set vc-backend and return t if registered
      (funcall handler 'vc-registered file))
     (t
      ;; There is no file name handler.
      ;; Try vc-BACKEND-registered for each handled BACKEND.
      (catch 'found
	(let ((backend (vc-file-getprop file 'vc-backend)))
	  (mapcar
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

(defun vc-backend (file)
  "Return the version control type of FILE, nil if it is not registered."
  ;; `file' can be nil in several places (typically due to the use of
  ;; code like (vc-backend buffer-file-name)).
  (when (stringp file)
    (let ((property (vc-file-getprop file 'vc-backend)))
      ;; Note that internally, Emacs remembers unregistered
      ;; files by setting the property to `none'.
      (cond ((eq property 'none) nil)
	    (property)
	    ;; vc-registered sets the vc-backend property
	    (t (if (vc-registered file)
		   (vc-file-getprop file 'vc-backend)
		 nil))))))

(defun vc-backend-subdirectory-name (file)
  "Return where the master and lock FILEs for the current directory are kept."
  (symbol-name (vc-backend file)))

(defun vc-name (file)
  "Return the master name of FILE.
If the file is not registered, or the master name is not known, return nil."
  ;; TODO: This should ultimately become obsolete, at least up here
  ;; in vc-hooks.
  (or (vc-file-getprop file 'vc-name)
      ;; force computation of the property by calling
      ;; vc-BACKEND-registered explicitly
      (if (and (vc-backend file)
	       (vc-call-backend (vc-backend file) 'registered file))
	  (vc-file-getprop file 'vc-name))))

(defun vc-checkout-model (file)
  "Indicate how FILE is checked out.

If FILE is not registered, this function always returns nil.
For registered files, the possible values are:

  'implicit   FILE is always writeable, and checked out `implicitly'
              when the user saves the first changes to the file.

  'locking    FILE is read-only if up-to-date; user must type
              \\[vc-next-action] before editing.  Strict locking
              is assumed.

  'announce   FILE is read-only if up-to-date; user must type
              \\[vc-next-action] before editing.  But other users
              may be editing at the same time."
  (or (vc-file-getprop file 'vc-checkout-model)
      (if (vc-backend file)
          (vc-file-setprop file 'vc-checkout-model
                           (vc-call checkout-model file)))))

(defun vc-user-login-name (file)
  "Return the name under which the user accesses the given FILE."
  (or (and (eq (string-match tramp-file-name-regexp file) 0)
           ;; tramp case: execute "whoami" via tramp
           (let ((default-directory (file-name-directory file)))
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

(defun vc-state (file)
  "Return the version control state of FILE.

If FILE is not registered, this function always returns nil.
For registered files, the value returned is one of:

  'up-to-date        The working file is unmodified with respect to the
                     latest version on the current branch, and not locked.

  'edited            The working file has been edited by the user.  If
                     locking is used for the file, this state means that
                     the current version is locked by the calling user.

  USER               The current version of the working file is locked by
                     some other USER (a string).

  'needs-patch       The file has not been edited by the user, but there is
                     a more recent version on the current branch stored
                     in the master file.

  'needs-merge       The file has been edited by the user, and there is also
                     a more recent version on the current branch stored in
                     the master file.  This state can only occur if locking
                     is not used for the file.

  'unlocked-changes  The current version of the working file is not locked,
                     but the working file has been changed with respect
                     to that version.  This state can only occur for files
                     with locking; it represents an erroneous condition that
                     should be resolved by the user (vc-next-action will
                     prompt the user to do it)."
  ;; FIXME: New (sub)states needed (?):
  ;; - `added' (i.e. `edited' but with no base version yet,
  ;;            typically represented by vc-workfile-version = "0")
  ;; - `conflict' (i.e. `edited' with conflict markers)
  ;; - `removed'
  ;; - `copied' and `moved' (might be handled by `removed' and `added')
  (or (vc-file-getprop file 'vc-state)
      (if (vc-backend file)
          (vc-file-setprop file 'vc-state
                           (vc-call state-heuristic file)))))

(defun vc-recompute-state (file)
  "Recompute the version control state of FILE, and return it.
This calls the possibly expensive function vc-BACKEND-state,
rather than the heuristic."
  (vc-file-setprop file 'vc-state (vc-call state file)))

(defsubst vc-up-to-date-p (file)
  "Convenience function that checks whether `vc-state' of FILE is `up-to-date'."
  (eq (vc-state file) 'up-to-date))

(defun vc-default-state-heuristic (backend file)
  "Default implementation of vc-state-heuristic.
It simply calls the real state computation function `vc-BACKEND-state'
and does not employ any heuristic at all."
   (vc-call-backend backend 'state file))

(defun vc-workfile-unchanged-p (file)
  "Return non-nil if FILE has not changed since the last checkout."
  (let ((checkout-time (vc-file-getprop file 'vc-checkout-time))
        (lastmod (nth 5 (file-attributes file))))
    (if (and checkout-time
             ;; Tramp and Ange-FTP return this when they don't know the time.
             (not (equal lastmod '(0 0))))
        (equal checkout-time lastmod)
      (let ((unchanged (vc-call workfile-unchanged-p file)))
        (vc-file-setprop file 'vc-checkout-time (if unchanged lastmod 0))
        unchanged))))

(defun vc-default-workfile-unchanged-p (backend file)
  "Check if FILE is unchanged by diffing against the master version.
Return non-nil if FILE is unchanged."
  (zerop (condition-case err
             ;; If the implementation supports it, let the output
             ;; go to *vc*, not *vc-diff*, since this is an internal call.
             (vc-call diff file nil nil "*vc*")
           (wrong-number-of-arguments
            ;; If this error came from the above call to vc-BACKEND-diff,
            ;; try again without the optional buffer argument (for
            ;; backward compatibility).  Otherwise, resignal.
            (if (or (not (eq (cadr err)
                             (indirect-function
                              (vc-find-backend-function (vc-backend file)
                                                        'diff))))
                    (not (eq (caddr err) 4)))
                (signal (car err) (cdr err))
              (vc-call diff file))))))

(defun vc-workfile-version (file)
  "Return the version level of the current workfile FILE.
If FILE is not registered, this function always returns nil."
  (or (vc-file-getprop file 'vc-workfile-version)
      (if (vc-backend file)
          (vc-file-setprop file 'vc-workfile-version
                           (vc-call workfile-version file)))))

(defun vc-default-registered (backend file)
  "Check if FILE is registered in BACKEND using vc-BACKEND-master-templates."
  (let ((sym (vc-make-backend-sym backend 'master-templates)))
    (unless (get backend 'vc-templates-grabbed)
      (put backend 'vc-templates-grabbed t)
      (set sym (append (delq nil
			     (mapcar
			      (lambda (template)
				(and (consp template)
				     (eq (cdr template) backend)
				     (car template)))
                              (with-no-warnings
                               vc-master-templates)))
		       (symbol-value sym))))
    (let ((result (vc-check-master-templates file (symbol-value sym))))
      (if (stringp result)
	  (vc-file-setprop file 'vc-name result)
	nil))))				; Not registered

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
	   (if (and trial (file-exists-p trial)
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

(defun vc-toggle-read-only (&optional verbose)
  "Change read-only status of current buffer, perhaps via version control.

If the buffer is visiting a file registered with version control,
then check the file in or out.  Otherwise, just change the read-only flag
of the buffer.
With prefix argument, ask for version number to check in or check out.
Check-out of a specified version number does not lock the file;
to do that, use this command a second time with no argument.

If you bind this function to \\[toggle-read-only], then Emacs checks files
in or out whenever you toggle the read-only flag."
  (interactive "P")
  (if (or (and (boundp 'vc-dired-mode) vc-dired-mode)
	  ;; use boundp because vc.el might not be loaded
	  (vc-backend buffer-file-name))
      (vc-next-action verbose)
    (toggle-read-only)))

(defun vc-default-make-version-backups-p (backend file)
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
                                    ?/ ?_ (or rev (vc-workfile-version file)))
                              (unless manual ".") "~")
                      (file-name-directory file))))

(defun vc-delete-automatic-version-backups (file)
  "Delete all existing automatic version backups for FILE."
  (condition-case nil
      (mapcar
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
  (let ((file buffer-file-name))
    (and (vc-backend file)
	 (vc-up-to-date-p file)
	 (eq (vc-checkout-model file) 'implicit)
	 (vc-call make-version-backups-p file)
         (vc-make-version-backup file))))

(defun vc-after-save ()
  "Function to be called by `basic-save-buffer' (in files.el)."
  ;; If the file in the current buffer is under version control,
  ;; up-to-date, and locking is not used for the file, set
  ;; the state to 'edited and redisplay the mode line.
  (let ((file buffer-file-name))
    (and (vc-backend file)
	 (or (and (equal (vc-file-getprop file 'vc-checkout-time)
			 (nth 5 (file-attributes file)))
		  ;; File has been saved in the same second in which
		  ;; it was checked out.  Clear the checkout-time
		  ;; to avoid confusion.
		  (vc-file-setprop file 'vc-checkout-time nil))
	     t)
         (vc-up-to-date-p file)
         (eq (vc-checkout-model file) 'implicit)
         (vc-file-setprop file 'vc-state 'edited)
	 (vc-mode-line file)
	 (if (featurep 'vc)
	     ;; If VC is not loaded, then there can't be
	     ;; any VC Dired buffer to synchronize.
	     (vc-dired-resynch-file file)))))

(defvar vc-menu-entry
  '(menu-item "Version Control" vc-menu-map
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

(defun vc-mode-line (file)
  "Set `vc-mode' to display type of version control for FILE.
The value is set in the current buffer, which should be the buffer
visiting FILE."
  (interactive (list buffer-file-name))
  (let ((backend (vc-backend file)))
    (if (not backend)
	(setq vc-mode nil)
      (setq vc-mode (concat " " (if vc-display-status
				    (vc-call mode-line-string file)
				  (symbol-name backend))))
      ;; If the file is locked by some other user, make
      ;; the buffer read-only.  Like this, even root
      ;; cannot modify a file that someone else has locked.
      (and (equal file buffer-file-name)
	   (stringp (vc-state file))
	   (setq buffer-read-only t))
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
    backend))

(defun vc-default-mode-line-string (backend file)
  "Return string for placement in modeline by `vc-mode-line' for FILE.
Format:

  \"BACKEND-REV\"        if the file is up-to-date
  \"BACKEND:REV\"        if the file is edited (or locked by the calling user)
  \"BACKEND:LOCKER:REV\" if the file is locked by somebody else

This function assumes that the file is registered."
  (setq backend (symbol-name backend))
  (let ((state   (vc-state file))
	(rev     (vc-workfile-version file)))
    (cond ((or (eq state 'up-to-date)
	       (eq state 'needs-patch))
	   (concat backend "-" rev))
          ((stringp state)
	   (concat backend ":" state ":" rev))
          (t
           ;; Not just for the 'edited state, but also a fallback
           ;; for all other states.  Think about different symbols
           ;; for 'needs-patch and 'needs-merge.
           (concat backend ":" rev)))))

(defun vc-follow-link ()
  "If current buffer visits a symbolic link, visit the real file.
If the real file is already visited in another buffer, make that buffer
current, and kill the buffer that visits the link."
  (let* ((truename (abbreviate-file-name (file-chase-links buffer-file-name)))
         (true-buffer (find-buffer-visiting truename))
	 (this-buffer (current-buffer)))
    (if (eq true-buffer this-buffer)
	(progn
	  (kill-buffer this-buffer)
	  ;; In principle, we could do something like set-visited-file-name.
	  ;; However, it can't be exactly the same as set-visited-file-name.
	  ;; I'm not going to work out the details right now. -- rms.
	  (set-buffer (find-file-noselect truename)))
      (set-buffer true-buffer)
      (kill-buffer this-buffer))))

(defun vc-default-find-file-hook (backend)
  nil)

(defun vc-find-file-hook ()
  "Function for `find-file-hook' activating VC mode if appropriate."
  ;; Recompute whether file is version controlled,
  ;; if user has killed the buffer and revisited.
  (if vc-mode
      (setq vc-mode nil))
  (when buffer-file-name
    (vc-file-clearprops buffer-file-name)
    (cond
     ((ignore-errors (vc-backend buffer-file-name))
      ;; Compute the state and put it in the modeline.
      (vc-mode-line buffer-file-name)
      (unless vc-make-backup-files
	;; Use this variable, not make-backup-files,
	;; because this is for things that depend on the file name.
	(set (make-local-variable 'backup-inhibited) t))
      ;; Let the backend setup any buffer-local things he needs.
      (vc-call-backend (vc-backend buffer-file-name) 'find-file-hook))
     ((let ((link-type (and (file-symlink-p buffer-file-name)
			    (vc-backend (file-chase-links buffer-file-name)))))
	(cond ((not link-type) nil)	;Nothing to do.
	      ((eq vc-follow-symlinks nil)
	       (message
        "Warning: symbolic link to %s-controlled source file" link-type))
	      ((or (not (eq vc-follow-symlinks 'ask))
		   ;; If we already visited this file by following
		   ;; the link, don't ask again if we try to visit
		   ;; it again.  GUD does that, and repeated questions
		   ;; are painful.
		   (get-file-buffer
		    (abbreviate-file-name
		     (file-chase-links buffer-file-name))))

	       (vc-follow-link)
	       (message "Followed link to %s" buffer-file-name)
	       (vc-find-file-hook))
	      (t
	       (if (yes-or-no-p (format
        "Symbolic link to %s-controlled source file; follow link? " link-type))
		   (progn (vc-follow-link)
			  (message "Followed link to %s" buffer-file-name)
			  (vc-find-file-hook))
		 (message
        "Warning: editing through the link bypasses version control")
		 ))))))))

(add-hook 'find-file-hook 'vc-find-file-hook)

;; more hooks, this time for file-not-found
(defun vc-file-not-found-hook ()
  "When file is not found, try to check it out from version control.
Returns t if checkout was successful, nil otherwise.
Used in `find-file-not-found-functions'."
  ;; When a file does not exist, ignore cached info about it
  ;; from a previous visit.
  (vc-file-clearprops buffer-file-name)
  (let ((backend (vc-backend buffer-file-name)))
    (if backend (vc-call-backend backend 'find-file-not-found-hook))))

(defun vc-default-find-file-not-found-hook (backend)
  ;; This used to do what vc-rcs-find-file-not-found-hook does, but it only
  ;; really makes sense for RCS.  For other backends, better not do anything.
  nil)

(add-hook 'find-file-not-found-functions 'vc-file-not-found-hook)

(defun vc-kill-buffer-hook ()
  "Discard VC info about a file when we kill its buffer."
  (if buffer-file-name
      (vc-file-clearprops buffer-file-name)))

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
    (define-key map "c" 'vc-cancel-version)
    (define-key map "d" 'vc-directory)
    (define-key map "g" 'vc-annotate)
    (define-key map "h" 'vc-insert-headers)
    (define-key map "i" 'vc-register)
    (define-key map "l" 'vc-print-log)
    (define-key map "m" 'vc-merge)
    (define-key map "r" 'vc-retrieve-snapshot)
    (define-key map "s" 'vc-create-snapshot)
    (define-key map "u" 'vc-revert-buffer)
    (define-key map "v" 'vc-next-action)
    (define-key map "=" 'vc-diff)
    (define-key map "~" 'vc-version-other-window)
    map))
(fset 'vc-prefix-map vc-prefix-map)
(define-key global-map "\C-xv" 'vc-prefix-map)

(defvar vc-menu-map
  (let ((map (make-sparse-keymap "Version Control")))
    ;;(define-key map [show-files]
    ;;  '("Show Files under VC" . (vc-directory t)))
    (define-key map [vc-retrieve-snapshot]
      '("Retrieve Snapshot" . vc-retrieve-snapshot))
    (define-key map [vc-create-snapshot]
      '("Create Snapshot" . vc-create-snapshot))
    (define-key map [vc-directory] '("VC Directory Listing" . vc-directory))
    (define-key map [separator1] '("----"))
    (define-key map [vc-annotate] '("Annotate" . vc-annotate))
    (define-key map [vc-rename-file] '("Rename File" . vc-rename-file))
    (define-key map [vc-version-other-window]
      '("Show Other Version" . vc-version-other-window))
    (define-key map [vc-diff] '("Compare with Base Version" . vc-diff))
    (define-key map [vc-update-change-log]
      '("Update ChangeLog" . vc-update-change-log))
    (define-key map [vc-print-log] '("Show History" . vc-print-log))
    (define-key map [separator2] '("----"))
    (define-key map [vc-insert-header]
      '("Insert Header" . vc-insert-headers))
    (define-key map [undo] '("Undo Last Check-In" . vc-cancel-version))
    (define-key map [vc-revert-buffer]
      '("Revert to Base Version" . vc-revert-buffer))
    (define-key map [vc-update]
      '("Update to Latest Version" . vc-update))
    (define-key map [vc-next-action] '("Check In/Out" . vc-next-action))
    (define-key map [vc-register] '("Register" . vc-register))
    map))

(defalias 'vc-menu-map vc-menu-map)

(defun vc-menu-map-filter (orig-binding)
  (if (and (symbolp orig-binding) (fboundp orig-binding))
      (setq orig-binding (indirect-function orig-binding)))
  (let ((ext-binding
         (if vc-mode (vc-call-backend (vc-backend buffer-file-name)
                                      'extra-menu))))
    ;; Give the VC backend a chance to add menu entries
    ;; specific for that backend.
    (if (null ext-binding)
        orig-binding
      (append orig-binding
	      '((ext-menu-separator "---"))
              ext-binding))))

(defun vc-default-extra-menu (backend)
  nil)

;; These are not correct and it's not currently clear how doing it
;; better (with more complicated expressions) might slow things down
;; on older systems.

;;(put 'vc-rename-file 'menu-enable 'vc-mode)
;;(put 'vc-annotate 'menu-enable '(eq (vc-buffer-backend) 'CVS))
;;(put 'vc-version-other-window 'menu-enable 'vc-mode)
;;(put 'vc-diff 'menu-enable 'vc-mode)
;;(put 'vc-update-change-log 'menu-enable
;;     '(member (vc-buffer-backend) '(RCS CVS)))
;;(put 'vc-print-log 'menu-enable 'vc-mode)
;;(put 'vc-cancel-version 'menu-enable 'vc-mode)
;;(put 'vc-revert-buffer 'menu-enable 'vc-mode)
;;(put 'vc-insert-headers 'menu-enable 'vc-mode)
;;(put 'vc-next-action 'menu-enable 'vc-mode)
;;(put 'vc-register 'menu-enable '(and buffer-file-name (not vc-mode)))

(provide 'vc-hooks)

;; arch-tag: 2e5a6fa7-1d30-48e2-8bd0-e3d335f04f32
;;; vc-hooks.el ends here
