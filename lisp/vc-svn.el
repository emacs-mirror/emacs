;;; vc-svn.el --- non-resident support for Subversion version-control

;; Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author:      FSF (see vc.el for full credits)
;; Maintainer:  Stefan Monnier <monnier@gnu.org>

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

;; Sync'd with Subversion's vc-svn.el as of revision 5801. but this version
;; has been extensively modified since to handle filesets.

;;; Code:

(eval-when-compile
  (require 'vc))

;;;
;;; Customization options
;;;

(defcustom vc-svn-global-switches nil
  "*Global switches to pass to any SVN command."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string))
  :version "22.1"
  :group 'vc)

(defcustom vc-svn-register-switches nil
  "*Extra switches for registering a file into SVN.
A string or list of strings passed to the checkin program by
\\[vc-register]."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string))
  :version "22.1"
  :group 'vc)

(defcustom vc-svn-diff-switches
  t			   ;`svn' doesn't support common args like -c or -b.
  "String or list of strings specifying extra switches for svn diff under VC.
If nil, use the value of `vc-diff-switches'.
If you want to force an empty list of arguments, use t."
  :type '(choice (const :tag "Unspecified" nil)
		 (const :tag "None" t)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string))
  :version "22.1"
  :group 'vc)

(defcustom vc-svn-header (or (cdr (assoc 'SVN vc-header-alist)) '("\$Id\$"))
  "*Header keywords to be inserted by `vc-insert-headers'."
  :version "22.1"
  :type '(repeat string)
  :group 'vc)

;; We want to autoload it for use by the autoloaded version of
;; vc-svn-registered, but we want the value to be compiled at startup, not
;; at dump time.
;; ;;;###autoload
(defconst vc-svn-admin-directory
  (cond ((and (memq system-type '(cygwin windows-nt ms-dos))
	      (getenv "SVN_ASP_DOT_NET_HACK"))
	 "_svn")
	(t ".svn"))
  "The name of the \".svn\" subdirectory or its equivalent.")

;;; Properties of the backend

(defun vc-svn-revision-granularity ()
     'repository)
;;;
;;; State-querying functions
;;;

;;; vc-svn-admin-directory is generally not defined when the
;;; autoloaded function is called.

;;;###autoload (defun vc-svn-registered (f)
;;;###autoload   (let ((admin-dir (cond ((and (eq system-type 'windows-nt)
;;;###autoload                                (getenv "SVN_ASP_DOT_NET_HACK"))
;;;###autoload                           "_svn")
;;;###autoload                          (t ".svn"))))
;;;###autoload     (when (file-readable-p (expand-file-name
;;;###autoload                             (concat admin-dir "/entries")
;;;###autoload                             (file-name-directory f)))
;;;###autoload       (load "vc-svn")
;;;###autoload       (vc-svn-registered f))))

;;;###autoload
(add-to-list 'completion-ignored-extensions ".svn/")

(defun vc-svn-registered (file)
  "Check if FILE is SVN registered."
  (when (file-readable-p (expand-file-name (concat vc-svn-admin-directory
						   "/entries")
					   (file-name-directory file)))
    (with-temp-buffer
      (cd (file-name-directory file))
      (let ((status
             (condition-case nil
                 ;; Ignore all errors.
                 (vc-svn-command t t file "status" "-v")
               ;; Some problem happened.  E.g. We can't find an `svn'
               ;; executable.  We used to only catch `file-error' but when
               ;; the process is run on a remote host via Tramp, the error
               ;; is only reported via the exit status which is turned into
               ;; an `error' by vc-do-command.
               (error nil))))
        (when (eq 0 status)
	  (let ((parsed (vc-svn-parse-status file)))
	    (and parsed (not (memq parsed '(ignored unregistered))))))))))

(defun vc-svn-state (file &optional localp)
  "SVN-specific version of `vc-state'."
  (setq localp (or localp (vc-stay-local-p file)))
  (with-temp-buffer
    (cd (file-name-directory file))
    (vc-svn-command t 0 file "status" (if localp "-v" "-u"))
    (vc-svn-parse-status file)))

(defun vc-svn-state-heuristic (file)
  "SVN-specific state heuristic."
  (vc-svn-state file 'local))

(defun vc-svn-dir-state (dir &optional localp)
  "Find the SVN state of all files in DIR and its subdirectories."
  (setq localp (or localp (vc-stay-local-p dir)))
  (let ((default-directory dir))
    ;; Don't specify DIR in this command, the default-directory is
    ;; enough.  Otherwise it might fail with remote repositories.
    (with-temp-buffer
      (buffer-disable-undo)		;; Because these buffers can get huge
      (vc-svn-command t 0 nil "status" (if localp "-v" "-u"))
      (vc-svn-parse-status))))

(defun vc-svn-after-dir-status (callback buffer)
  (let ((state-map '((?A . added)
                    (?C . edited)
                    (?D . removed)
                    (?I . ignored)
                    (?M . edited)
                    (?R . removed)
                    (?? . unregistered)
                    ;; This is what vc-svn-parse-status does.
                    (?~ . edited)))
       result)
    (goto-char (point-min))
    (while (re-search-forward "^\\(.\\)..... \\(.*\\)$" nil t)
      (let ((state (cdr (assq (aref (match-string 1) 0) state-map)))
           (filename (match-string 2)))
       (when state
         (setq result (cons (cons filename state) result)))))
    (funcall callback result buffer)))

(defun vc-svn-dir-status (dir callback buffer)
  "Run 'svn status' for DIR and update BUFFER via CALLBACK.
CALLBACK is called as (CALLBACK RESULT BUFFER), where
RESULT is a list of conses (FILE . STATE) for directory DIR."
  (with-current-buffer (get-buffer-create
                       (generate-new-buffer-name " *vc svn status*"))
    (vc-svn-command (current-buffer) 'async nil "status")
    (vc-exec-after
     `(vc-svn-after-dir-status (quote ,callback) ,buffer))))

(defun vc-svn-working-revision (file)
  "SVN-specific version of `vc-working-revision'."
  ;; There is no need to consult RCS headers under SVN, because we
  ;; get the workfile version for free when we recognize that a file
  ;; is registered in SVN.
  (vc-svn-registered file)
  (vc-file-getprop file 'vc-working-revision))

(defun vc-svn-checkout-model (file)
  "SVN-specific version of `vc-checkout-model'."
  ;; It looks like Subversion has no equivalent of CVSREAD.
  'implicit)

;; vc-svn-mode-line-string doesn't exist because the default implementation
;; works just fine.

(defun vc-svn-dired-state-info (file)
  "SVN-specific version of `vc-dired-state-info'."
  (let ((svn-state (vc-state file)))
    (cond ((eq svn-state 'edited)
	   (if (equal (vc-working-revision file) "0")
	       "(added)" "(modified)"))
	  (t
	   ;; fall back to the default VC representation
	   (vc-default-dired-state-info 'SVN file)))))


(defun vc-svn-previous-revision (file rev)
  (let ((newrev (1- (string-to-number rev))))
    (when (< 0 newrev)
      (number-to-string newrev))))

(defun vc-svn-next-revision (file rev)
  (let ((newrev (1+ (string-to-number rev))))
    ;; The "working revision" is an uneasy conceptual fit under Subversion;
    ;; we use it as the upper bound until a better idea comes along.  If the
    ;; workfile version W coincides with the tree's latest revision R, then
    ;; this check prevents a "no such revision: R+1" error.  Otherwise, it
    ;; inhibits showing of W+1 through R, which could be considered anywhere
    ;; from gracious to impolite.
    (unless (< (string-to-number (vc-file-getprop file 'vc-working-revision))
               newrev)
      (number-to-string newrev))))


;;;
;;; State-changing functions
;;;

(defun vc-svn-create-repo ()
  "Create a new SVN repository."
  (vc-do-command nil 0 "svnadmin" '("create" "SVN"))
  (vc-do-command nil 0 "svn" '(".") 
		 "checkout" (concat "file://" default-directory "SVN")))

(defun vc-svn-register (files &optional rev comment)
  "Register FILES into the SVN version-control system.
The COMMENT argument is ignored  This does an add but not a commit.

`vc-register-switches' and `vc-svn-register-switches' are passed to
the SVN command (in that order)."
  (apply 'vc-svn-command nil 0 files "add" (vc-switches 'SVN 'register)))

(defun vc-svn-responsible-p (file)
  "Return non-nil if SVN thinks it is responsible for FILE."
  (file-directory-p (expand-file-name vc-svn-admin-directory
				      (if (file-directory-p file)
					  file
					(file-name-directory file)))))

(defalias 'vc-svn-could-register 'vc-svn-responsible-p
  "Return non-nil if FILE could be registered in SVN.
This is only possible if SVN is responsible for FILE's directory.")

(defun vc-svn-checkin (files rev comment)
  "SVN-specific version of `vc-backend-checkin'."
  (if rev (error "Committing to a specific revision is unsupported in SVN."))
  (let ((status (apply
                 'vc-svn-command nil 1 files "ci"
                 (nconc (list "-m" comment) (vc-switches 'SVN 'checkin)))))
    (set-buffer "*vc*")
    (goto-char (point-min))
    (unless (equal status 0)
      ;; Check checkin problem.
      (cond
       ((search-forward "Transaction is out of date" nil t)
        (mapc (lambda (file) (vc-file-setprop file 'vc-state 'needs-merge))
	      files)
        (error (substitute-command-keys
                (concat "Up-to-date check failed: "
                        "type \\[vc-next-action] to merge in changes"))))
       (t
        (pop-to-buffer (current-buffer))
        (goto-char (point-min))
        (shrink-window-if-larger-than-buffer)
        (error "Check-in failed"))))
    ;; Update file properties
    ;; (vc-file-setprop
    ;;  file 'vc-working-revision
    ;;  (vc-parse-buffer "^\\(new\\|initial\\) revision: \\([0-9.]+\\)" 2))
    ))

(defun vc-svn-find-revision (file rev buffer)
  "SVN-specific retrieval of a specified version into a buffer."
  (apply 'vc-svn-command
	 buffer 0 file
	 "cat"
	 (and rev (not (string= rev ""))
	      (concat "-r" rev))
	 (vc-switches 'SVN 'checkout)))

(defun vc-svn-checkout (file &optional editable rev)
  (message "Checking out %s..." file)
  (with-current-buffer (or (get-file-buffer file) (current-buffer))
    (vc-call update file editable rev (vc-switches 'SVN 'checkout)))
  (vc-mode-line file)
  (message "Checking out %s...done" file))

(defun vc-svn-update (file editable rev switches)
  (if (and (file-exists-p file) (not rev))
      ;; If no revision was specified, there's nothing to do.
      nil
    ;; Check out a particular version (or recreate the file).
    (vc-file-setprop file 'vc-working-revision nil)
    (apply 'vc-svn-command nil 0 file
	   "update"
	   ;; default for verbose checkout: clear the sticky tag so
	   ;; that the actual update will get the head of the trunk
	   (cond
	    ((null rev) "-rBASE")
	    ((or (eq rev t) (equal rev "")) nil)
	    (t (concat "-r" rev)))
	   switches)))

(defun vc-svn-delete-file (file)
  (vc-svn-command nil 0 file "remove"))

(defun vc-svn-rename-file (old new)
  (vc-svn-command nil 0 new "move" (file-relative-name old)))

(defun vc-svn-revert (file &optional contents-done)
  "Revert FILE to the version it was based on."
  (unless contents-done
    (vc-svn-command nil 0 file "revert")))

(defun vc-svn-merge (file first-version &optional second-version)
  "Merge changes into current working copy of FILE.
The changes are between FIRST-VERSION and SECOND-VERSION."
  (vc-svn-command nil 0 file
                 "merge"
		 "-r" (if second-version
			(concat first-version ":" second-version)
		      first-version))
  (vc-file-setprop file 'vc-state 'edited)
  (with-current-buffer (get-buffer "*vc*")
    (goto-char (point-min))
    (if (looking-at "C  ")
        1				; signal conflict
      0)))				; signal success

(defun vc-svn-merge-news (file)
  "Merge in any new changes made to FILE."
  (message "Merging changes into %s..." file)
  ;; (vc-file-setprop file 'vc-working-revision nil)
  (vc-file-setprop file 'vc-checkout-time 0)
  (vc-svn-command nil 0 file "update")
  ;; Analyze the merge result reported by SVN, and set
  ;; file properties accordingly.
  (with-current-buffer (get-buffer "*vc*")
    (goto-char (point-min))
    ;; get new working revision
    (if (re-search-forward
	 "^\\(Updated to\\|At\\) revision \\([0-9]+\\)" nil t)
	(vc-file-setprop file 'vc-working-revision (match-string 2))
      (vc-file-setprop file 'vc-working-revision nil))
    ;; get file status
    (goto-char (point-min))
    (prog1
        (if (looking-at "At revision")
            0 ;; there were no news; indicate success
          (if (re-search-forward
               ;; Newer SVN clients have 3 columns of chars (one for the
               ;; file's contents, then second for its properties, and the
               ;; third for lock-grabbing info), before the 2 spaces.
               ;; We also used to match the filename in column 0 without any
               ;; meta-info before it, but I believe this can never happen.
               (concat "^\\(\\([ACGDU]\\)\\(.[B ]\\)?  \\)"
                       (regexp-quote (file-name-nondirectory file)))
               nil t)
              (cond
               ;; Merge successful, we are in sync with repository now
               ((string= (match-string 2) "U")
                (vc-file-setprop file 'vc-state 'up-to-date)
                (vc-file-setprop file 'vc-checkout-time
                                 (nth 5 (file-attributes file)))
                0);; indicate success to the caller
               ;; Merge successful, but our own changes are still in the file
               ((string= (match-string 2) "G")
                (vc-file-setprop file 'vc-state 'edited)
                0);; indicate success to the caller
               ;; Conflicts detected!
               (t
                (vc-file-setprop file 'vc-state 'edited)
                1);; signal the error to the caller
               )
            (pop-to-buffer "*vc*")
            (error "Couldn't analyze svn update result")))
      (message "Merging changes into %s...done" file))))

(defun vc-svn-modify-change-comment (files rev comment)
  "Modify the change comments for a specified REV.
You must have ssh access to the repository host, and the directory Emacs
uses locally for temp files must also be writeable by you on that host."
  (vc-do-command nil 0 "svn" nil "info")
  (set-buffer "*vc*")
  (goto-char (point-min))
  (unless (re-search-forward "Repository Root: svn\\+ssh://\\([^/]+\\)\\(/.*\\)" nil t)
    (error "Repository information is unavailable."))
  (let* ((tempfile (make-temp-file user-mail-address)) 
	(host (match-string 1))
	(directory (match-string 2))
	(remotefile (concat host ":" tempfile)))
    (with-temp-buffer
      (insert comment)
      (write-region (point-min) (point-max) tempfile))
    (unless (vc-do-command nil 0 "scp" nil "-q" tempfile remotefile)
      (error "Copy of comment to %s failed" remotefile))
    (unless (vc-do-command nil 0 "ssh" nil 
			   "-q" host 
			   (format "svnadmin setlog --bypass-hooks %s -r %s %s; rm %s" 
				   directory rev tempfile tempfile))
      (error "Log edit failed"))
  ))

;;;
;;; History functions
;;;

(defun vc-svn-print-log (files &optional buffer)
  "Get change log(s) associated with FILES."
  (save-current-buffer
    (vc-setup-buffer buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (if files
	  (dolist (file files)
		  (insert "Working file: " file "\n")
		  (vc-svn-command
		   buffer
		   'async
		   ;; (if (and (= (length files) 1) (vc-stay-local-p file)) 'async 0)
		   (list file)
		   "log"
		   ;; By default Subversion only shows the log up to the
		   ;; working revision, whereas we also want the log of the
		   ;; subsequent commits.  At least that's what the
		   ;; vc-cvs.el code does.
		   "-rHEAD:0"))
	;; Dump log for the entire directory.
	(vc-svn-command buffer 0 nil "log" "-rHEAD:0")))))

(defun vc-svn-wash-log ()
  "Remove all non-comment information from log output."
  ;; FIXME: not implemented for SVN
  nil)

(defun vc-svn-diff (files &optional oldvers newvers buffer)
  "Get a difference report using SVN between two revisions of fileset FILES."
  (and oldvers
       (catch 'no
	 (dolist (f files)
	   (or (equal oldvers (vc-working-revision f))
	       (throw 'no nil)))
	 t)
       ;; Use nil rather than the current revision because svn handles
       ;; it better (i.e. locally).  Note that if _any_ of the files
       ;; has a different revision, we fetch the lot, which is
       ;; obviously sub-optimal.
       (setq oldvers nil))
  (let* ((switches
	    (if vc-svn-diff-switches
		(vc-switches 'SVN 'diff)
	      (list "-x" (mapconcat 'identity (vc-switches nil 'diff) " "))))
	   (async (and (not vc-disable-async-diff)
                       (vc-stay-local-p files)
		       (or oldvers newvers)))) ; Svn diffs those locally.
      (apply 'vc-svn-command buffer
	     (if async 'async 0)
	     files "diff"
	     (append
	      switches
	      (when oldvers
		(list "-r" (if newvers (concat oldvers ":" newvers)
			     oldvers)))))
      (if async 1		      ; async diff => pessimistic assumption
	;; For some reason `svn diff' does not return a useful
	;; status w.r.t whether the diff was empty or not.
	(buffer-size (get-buffer buffer)))))

;;;
;;; Snapshot system
;;;

(defun vc-svn-create-snapshot (dir name branchp)
  "Assign to DIR's current revision a given NAME.
If BRANCHP is non-nil, the name is created as a branch (and the current
workspace is immediately moved to that new branch).
NAME is assumed to be a URL."
  (vc-svn-command nil 0 dir "copy" name)
  (when branchp (vc-svn-retrieve-snapshot dir name nil)))

(defun vc-svn-retrieve-snapshot (dir name update)
  "Retrieve a snapshot at and below DIR.
NAME is the name of the snapshot; if it is empty, do a `svn update'.
If UPDATE is non-nil, then update (resynch) any affected buffers.
NAME is assumed to be a URL."
  (vc-svn-command nil 0 dir "switch" name)
  ;; FIXME: parse the output and obey `update'.
  )

;;;
;;; Miscellaneous
;;;

;; Subversion makes backups for us, so don't bother.
;; (defalias 'vc-svn-make-version-backups-p 'vc-stay-local-p
;;   "Return non-nil if version backups should be made for FILE.")

(defun vc-svn-check-headers ()
  "Check if the current file has any headers in it."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "\\$[A-Za-z\300-\326\330-\366\370-\377]+\
\\(: [\t -#%-\176\240-\377]*\\)?\\$" nil t)))


;;;
;;; Internal functions
;;;

(defcustom vc-svn-program "svn"
  "Name of the svn executable."
  :type 'string
  :group 'vc)

(defun vc-svn-root (dir)
  (vc-find-root dir vc-svn-admin-directory t))

(defun vc-svn-command (buffer okstatus file-or-list &rest flags)
  "A wrapper around `vc-do-command' for use in vc-svn.el.
The difference to vc-do-command is that this function always invokes `svn',
and that it passes `vc-svn-global-switches' to it before FLAGS."
  (apply 'vc-do-command buffer okstatus vc-svn-program file-or-list
         (if (stringp vc-svn-global-switches)
             (cons vc-svn-global-switches flags)
           (append vc-svn-global-switches
                   flags))))

(defun vc-svn-repository-hostname (dirname)
  (with-temp-buffer
    (let ((coding-system-for-read
	   (or file-name-coding-system
	       default-file-name-coding-system)))
      (vc-insert-file (expand-file-name (concat vc-svn-admin-directory
						"/entries")
					dirname)))
    (goto-char (point-min))
    (when (re-search-forward
	   ;; Old `svn' used name="svn:this_dir", newer use just name="".
	   (concat "name=\"\\(?:svn:this_dir\\)?\"[\n\t ]*"
		   "\\(?:[-a-z]+=\"[^\"]*\"[\n\t ]*\\)*?"
		   "url=\"\\(?1:[^\"]+\\)\""
                   ;; Yet newer ones don't use XML any more.
                   "\\|^\ndir\n[0-9]+\n\\(?1:.*\\)") nil t)
      ;; This is not a hostname but a URL.  This may actually be considered
      ;; as a feature since it allows vc-svn-stay-local to specify different
      ;; behavior for different modules on the same server.
      (match-string 1))))

(defun vc-svn-resolve-when-done ()
  "Call \"svn resolved\" if the conflict markers have been removed."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^<<<<<<< " nil t)
      (vc-svn-command nil 0 buffer-file-name "resolved")
      ;; Remove the hook so that it is not called multiple times.
      (remove-hook 'after-save-hook 'vc-svn-resolve-when-done t))))

;; Inspired by vc-arch-find-file-hook.
(defun vc-svn-find-file-hook ()
  (when (eq ?C (vc-file-getprop buffer-file-name 'vc-svn-status))
    ;; If the file is marked as "conflicted", then we should try and call
    ;; "svn resolved" when applicable.
    (if (save-excursion
          (goto-char (point-min))
          (re-search-forward "^<<<<<<< " nil t))
        ;; There are conflict markers.
        (progn
          (smerge-start-session)
          (add-hook 'after-save-hook 'vc-svn-resolve-when-done nil t))
      ;; There are no conflict markers.  This is problematic: maybe it means
      ;; the conflict has been resolved and we should immediately call "svn
      ;; resolved", or it means that the file's type does not allow Svn to
      ;; use conflict markers in which case we don't really know what to do.
      ;; So let's just punt for now.
      nil)
    (message "There are unresolved conflicts in this file")))

(defun vc-svn-parse-status (&optional filename)
  "Parse output of \"svn status\" command in the current buffer.
Set file properties accordingly.  Unless FILENAME is non-nil, parse only
information about FILENAME and return its status."
  (let (file status)
    (goto-char (point-min))
    (while (re-search-forward
            ;; Ignore the files with status X.
	    "^\\(\\?\\|[ ACDGIMR!~][ MC][ L][ +][ S]..\\([ *]\\) +\\([-0-9]+\\) +\\([0-9?]+\\) +\\([^ ]+\\)\\) +" nil t)
      ;; If the username contains spaces, the output format is ambiguous,
      ;; so don't trust the output's filename unless we have to.
      (setq file (or filename
                     (expand-file-name
                      (buffer-substring (point) (line-end-position)))))
      (setq status (char-after (line-beginning-position)))
      (if (eq status ??)
	  (vc-file-setprop file 'vc-state 'unregistered)
	;; `vc-BACKEND-registered' must not set vc-backend,
	;; which is instead set in vc-registered.
	(unless filename (vc-file-setprop file 'vc-backend 'SVN))
	;; Use the last-modified revision, so that searching in vc-print-log
	;; output works.
	(vc-file-setprop file 'vc-working-revision (match-string 3))
        ;; Remember Svn's own status.
        (vc-file-setprop file 'vc-svn-status status)
	(vc-file-setprop
	 file 'vc-state
	 (cond
	  ((eq status ?\ )
	   (if (eq (char-after (match-beginning 1)) ?*)
	       'needs-patch
             (vc-file-setprop file 'vc-checkout-time
                              (nth 5 (file-attributes file)))
	     'up-to-date))
	  ((eq status ?A)
	   ;; If the file was actually copied, (match-string 2) is "-".
	   (vc-file-setprop file 'vc-working-revision "0")
	   (vc-file-setprop file 'vc-checkout-time 0)
	   'added)
	  ((memq status '(?M ?C))
	   (if (eq (char-after (match-beginning 1)) ?*)
	       'needs-merge
	     'edited))
	  ((eq status ?I)
	   (vc-file-setprop file 'vc-state 'ignored))
	  ((eq status ?R)
	   (vc-file-setprop file 'vc-state 'removed))
	  (t 'edited)))))
    (if filename (vc-file-getprop filename 'vc-state))))

(defun vc-svn-dir-state-heuristic (dir)
  "Find the SVN state of all files in DIR, using only local information."
  (vc-svn-dir-state dir 'local))

(defun vc-svn-valid-symbolic-tag-name-p (tag)
  "Return non-nil if TAG is a valid symbolic tag name."
  ;; According to the SVN manual, a valid symbolic tag must start with
  ;; an uppercase or lowercase letter and can contain uppercase and
  ;; lowercase letters, digits, `-', and `_'.
  (and (string-match "^[a-zA-Z]" tag)
       (not (string-match "[^a-z0-9A-Z-_]" tag))))

(defun vc-svn-valid-revision-number-p (tag)
  "Return non-nil if TAG is a valid revision number."
  (and (string-match "^[0-9]" tag)
       (not (string-match "[^0-9]" tag))))

;; Support for `svn annotate'

(defun vc-svn-annotate-command (file buf &optional rev)
  (vc-svn-command buf 0 file "annotate" (if rev (concat "-r" rev))))

(defun vc-svn-annotate-time-of-rev (rev)
  ;; Arbitrarily assume 10 commmits per day.
  (/ (string-to-number rev) 10.0))

(defun vc-svn-annotate-current-time ()
  (vc-svn-annotate-time-of-rev vc-annotate-parent-rev))

(defconst vc-svn-annotate-re "[ \t]*\\([0-9]+\\)[ \t]+[^\t ]+ ")

(defun vc-svn-annotate-time ()
  (when (looking-at vc-svn-annotate-re)
    (goto-char (match-end 0))
    (vc-svn-annotate-time-of-rev (match-string 1))))

(defun vc-svn-annotate-extract-revision-at-line ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at vc-svn-annotate-re) (match-string 1))))

(provide 'vc-svn)

;; arch-tag: 02f10c68-2b4d-453a-90fc-1eee6cfb268d
;;; vc-svn.el ends here
