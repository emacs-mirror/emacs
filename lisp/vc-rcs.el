;;; vc-rcs.el --- support for RCS version-control

;; Copyright (C) 1992,93,94,95,96,97,98,99,2000,2001  Free Software Foundation, Inc.

;; Author:     FSF (see vc.el for full credits)
;; Maintainer: Andre Spiegel <spiegel@gnu.org>

;; $Id: vc-rcs.el,v 1.23 2002/02/25 22:04:29 spiegel Exp $

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

;; See vc.el

;;; Code:

;;;
;;; Customization options
;;;

(eval-when-compile
  (require 'cl)
  (require 'vc))

(defcustom vc-rcs-release nil
  "*The release number of your RCS installation, as a string.
If nil, VC itself computes this value when it is first needed."
  :type '(choice (const :tag "Auto" nil)
		 (string :tag "Specified")
		 (const :tag "Unknown" unknown))
  :group 'vc)

(defcustom vc-rcs-register-switches nil
  "*Extra switches for registering a file in RCS.
A string or list of strings.  These are passed to the checkin program
by \\[vc-rcs-register]."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string))
  :version "21.1"
  :group 'vc)

(defcustom vc-rcs-diff-switches nil
  "*A string or list of strings specifying extra switches for rcsdiff under VC."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string))
  :version "21.1"
  :group 'vc)

(defcustom vc-rcs-header (or (cdr (assoc 'RCS vc-header-alist)) '("\$Id\$"))
  "*Header keywords to be inserted by `vc-insert-headers'."
  :type '(repeat string)
  :version "21.1"
  :group 'vc)

(defcustom vc-rcsdiff-knows-brief nil
  "*Indicates whether rcsdiff understands the --brief option.
The value is either `yes', `no', or nil.  If it is nil, VC tries
to use --brief and sets this variable to remember whether it worked."
  :type '(choice (const :tag "Work out" nil) (const yes) (const no))
  :group 'vc)

;;;###autoload
(defcustom vc-rcs-master-templates
  '("%sRCS/%s,v" "%s%s,v" "%sRCS/%s")
  "*Where to look for RCS master files.
For a description of possible values, see `vc-check-master-templates'."
  :type '(choice (const :tag "Use standard RCS file names"
			'("%sRCS/%s,v" "%s%s,v" "%sRCS/%s"))
		 (repeat :tag "User-specified"
			 (choice string
				 function)))
  :version "21.1"
  :group 'vc)


;;;
;;; State-querying functions
;;;

;;;###autoload
(progn (defun vc-rcs-registered (f) (vc-default-registered 'RCS f)))

(defun vc-rcs-state (file)
  "Implementation of `vc-state' for RCS."
  (or (boundp 'vc-rcs-headers-result)
      (and vc-consult-headers
           (vc-rcs-consult-headers file)))
  (let ((state
         ;; vc-workfile-version might not be known; in that case the
         ;; property is nil.  vc-rcs-fetch-master-state knows how to
         ;; handle that.
         (vc-rcs-fetch-master-state file
                                    (vc-file-getprop file
                                                     'vc-workfile-version))))
    (if (not (eq state 'up-to-date))
        state
      (require 'vc)
      (if (vc-workfile-unchanged-p file)
          'up-to-date
        (if (eq (vc-checkout-model file) 'locking)
            'unlocked-changes
          'edited)))))

(defun vc-rcs-state-heuristic (file)
  "State heuristic for RCS."
  (let (vc-rcs-headers-result)
    (if (and vc-consult-headers
             (setq vc-rcs-headers-result
                   (vc-rcs-consult-headers file))
             (eq vc-rcs-headers-result 'rev-and-lock))
        (let ((state (vc-file-getprop file 'vc-state)))
          ;; If the headers say that the file is not locked, the
          ;; permissions can tell us whether locking is used for
          ;; the file or not.
          (if (and (eq state 'up-to-date)
                   (not (vc-mistrust-permissions file)))
              (cond
               ((string-match ".rw..-..-." (nth 8 (file-attributes file)))
                (vc-file-setprop file 'vc-checkout-model 'implicit)
		(setq state 
		      (if (vc-rcs-workfile-is-newer file) 
			  'edited 
			'up-to-date)))
               ((string-match ".r-..-..-." (nth 8 (file-attributes file)))
                (vc-file-setprop file 'vc-checkout-model 'locking))))
          state)
      (if (not (vc-mistrust-permissions file))
          (let* ((attributes  (file-attributes file))
                 (owner-uid   (nth 2 attributes))
                 (permissions (nth 8 attributes)))
            (cond ((string-match ".r-..-..-." permissions)
                   (vc-file-setprop file 'vc-checkout-model 'locking)
                   'up-to-date)
                  ((string-match ".rw..-..-." permissions)
		   (if (eq (vc-checkout-model file) 'locking)
		       (if (file-ownership-preserved-p file)
			   'edited
			 (vc-user-login-name owner-uid))
		     (if (vc-rcs-workfile-is-newer file) 
			 'edited
		       'up-to-date)))
                  (t
                   ;; Strange permissions.  Fall through to
                   ;; expensive state computation.
                   (vc-rcs-state file))))
        (vc-rcs-state file)))))

(defun vc-rcs-workfile-version (file)
  "RCS-specific version of `vc-workfile-version'."
  (or (and vc-consult-headers
           (vc-rcs-consult-headers file)
           (vc-file-getprop file 'vc-workfile-version))
      (progn
        (vc-rcs-fetch-master-state file)
        (vc-file-getprop file 'vc-workfile-version))))

(defun vc-rcs-latest-on-branch-p (file &optional version)
  "Return non-nil if workfile version of FILE is the latest on its branch.
When VERSION is given, perform check for that version."
  (unless version (setq version (vc-workfile-version file)))
  (with-temp-buffer
    (string= version
	     (if (vc-trunk-p version)
		 (progn
		   ;; Compare VERSION to the head version number.
		   (vc-insert-file (vc-name file) "^[0-9]")
		   (vc-parse-buffer "^head[ \t\n]+\\([^;]+\\);" 1))
	       ;; If we are not on the trunk, we need to examine the
	       ;; whole current branch.
	       (vc-insert-file (vc-name file) "^desc")
	       (vc-rcs-find-most-recent-rev (vc-branch-part version))))))

(defun vc-rcs-checkout-model (file)
  "RCS-specific version of `vc-checkout-model'."
  (vc-rcs-consult-headers file)
  (or (vc-file-getprop file 'vc-checkout-model)
      (progn (vc-rcs-fetch-master-state file)
	     (vc-file-getprop file 'vc-checkout-model))))

(defun vc-rcs-workfile-unchanged-p (file)
  "RCS-specific implementation of vc-workfile-unchanged-p."
  ;; Try to use rcsdiff --brief.  If rcsdiff does not understand that,
  ;; do a double take and remember the fact for the future
  (let* ((version (concat "-r" (vc-workfile-version file)))
         (status (if (eq vc-rcsdiff-knows-brief 'no)
                     (vc-do-command nil 1 "rcsdiff" file version)
                   (vc-do-command nil 2 "rcsdiff" file "--brief" version))))
    (if (eq status 2)
        (if (not vc-rcsdiff-knows-brief)
            (setq vc-rcsdiff-knows-brief 'no
                  status (vc-do-command nil 1 "rcsdiff" file version))
          (error "rcsdiff failed"))
      (if (not vc-rcsdiff-knows-brief) (setq vc-rcsdiff-knows-brief 'yes)))
    ;; The workfile is unchanged if rcsdiff found no differences.
    (zerop status)))


;;;
;;; State-changing functions
;;;

(defun vc-rcs-register (file &optional rev comment)
  "Register FILE into the RCS version-control system.
REV is the optional revision number for the file.  COMMENT can be used
to provide an initial description of FILE.

`vc-register-switches' and `vc-rcs-register-switches' are passed to
the RCS command (in that order).

Automatically retrieve a read-only version of the file with keywords
expanded if `vc-keep-workfiles' is non-nil, otherwise, delete the workfile."
    (let ((subdir (expand-file-name "RCS" (file-name-directory file)))
	  (switches (append
		     (if (stringp vc-register-switches)
			 (list vc-register-switches)
		       vc-register-switches)
		     (if (stringp vc-rcs-register-switches)
		     (list vc-rcs-register-switches)
		     vc-rcs-register-switches))))
      
      (and (not (file-exists-p subdir))
	   (not (directory-files (file-name-directory file)
				 nil ".*,v$" t))
	   (yes-or-no-p "Create RCS subdirectory? ")
	   (make-directory subdir))
      (apply 'vc-do-command nil 0 "ci" file
	     ;; if available, use the secure registering option
	     (and (vc-rcs-release-p "5.6.4") "-i")
	     (concat (if vc-keep-workfiles "-u" "-r") rev)
	     (and comment (concat "-t-" comment))
	     switches)
      ;; parse output to find master file name and workfile version
      (with-current-buffer "*vc*"
        (goto-char (point-min))
        (let ((name (if (looking-at (concat "^\\(.*\\)  <--  "
                                            (file-name-nondirectory file)))
                        (match-string 1))))
          (if (not name)
              ;; if we couldn't find the master name,
              ;; run vc-rcs-registered to get it
              ;; (will be stored into the vc-name property)
              (vc-rcs-registered file)
            (vc-file-setprop file 'vc-name
                             (if (file-name-absolute-p name)
                                 name
                               (expand-file-name
                                name
                                (file-name-directory file))))))
        (vc-file-setprop file 'vc-workfile-version
                         (if (re-search-forward
                              "^initial revision: \\([0-9.]+\\).*\n"
                              nil t)
                             (match-string 1))))))

(defun vc-rcs-responsible-p (file)
  "Return non-nil if RCS thinks it would be responsible for registering FILE."
  ;; TODO: check for all the patterns in vc-rcs-master-templates
  (file-directory-p (expand-file-name "RCS" (file-name-directory file))))

(defun vc-rcs-receive-file (file rev)
  "Implementation of receive-file for RCS."
  (let ((checkout-model (vc-checkout-model file)))
    (vc-rcs-register file rev "")
    (when (eq checkout-model 'implicit)
      (vc-rcs-set-non-strict-locking file))
    (vc-rcs-set-default-branch file (concat rev ".1"))))

(defun vc-rcs-unregister (file)
  "Unregister FILE from RCS.
If this leaves the RCS subdirectory empty, ask the user
whether to remove it."
  (let* ((master (vc-name file))
	 (dir (file-name-directory master))
	 (backup-info (find-backup-file-name master)))
    (if (not backup-info)
	(delete-file master)
      (rename-file master (car backup-info) 'ok-if-already-exists)
      (dolist (f (cdr backup-info)) (ignore-errors (delete-file f))))
    (and (string= (file-name-nondirectory (directory-file-name dir)) "RCS")
	 ;; check whether RCS dir is empty, i.e. it does not
	 ;; contain any files except "." and ".."
	 (not (directory-files dir nil 
			       "^\\([^.]\\|\\.[^.]\\|\\.\\.[^.]\\).*"))
	 (yes-or-no-p (format "Directory %s is empty; remove it? " dir))
	 (delete-directory dir))))

(defun vc-rcs-checkin (file rev comment)
  "RCS-specific version of `vc-backend-checkin'."
  (let ((switches (if (stringp vc-checkin-switches)
		      (list vc-checkin-switches)
		    vc-checkin-switches)))
    (let ((old-version (vc-workfile-version file)) new-version
	  (default-branch (vc-file-getprop file 'vc-rcs-default-branch)))
      ;; Force branch creation if an appropriate 
      ;; default branch has been set.
      (and (not rev)
	   default-branch
	   (string-match (concat "^" (regexp-quote old-version) "\\.")
			 default-branch)
	   (setq rev default-branch)
	   (setq switches (cons "-f" switches)))
      (apply 'vc-do-command nil 0 "ci" (vc-name file)
	     ;; if available, use the secure check-in option
	     (and (vc-rcs-release-p "5.6.4") "-j")
	     (concat (if vc-keep-workfiles "-u" "-r") rev)
	     (concat "-m" comment)
	     switches)
      (vc-file-setprop file 'vc-workfile-version nil)

      ;; determine the new workfile version
      (set-buffer "*vc*")
      (goto-char (point-min))
      (when (or (re-search-forward
		 "new revision: \\([0-9.]+\\);" nil t)
		(re-search-forward
		 "reverting to previous revision \\([0-9.]+\\)" nil t))
	(setq new-version (match-string 1))
	(vc-file-setprop file 'vc-workfile-version new-version))

      ;; if we got to a different branch, adjust the default
      ;; branch accordingly
      (cond
       ((and old-version new-version
	     (not (string= (vc-branch-part old-version)
			   (vc-branch-part new-version))))
	(vc-rcs-set-default-branch file 
				   (if (vc-trunk-p new-version) nil
				     (vc-branch-part new-version)))
	;; If this is an old RCS release, we might have
	;; to remove a remaining lock.
	(if (not (vc-rcs-release-p "5.6.2"))
	    ;; exit status of 1 is also accepted.
	    ;; It means that the lock was removed before.
	    (vc-do-command nil 1 "rcs" (vc-name file)
			   (concat "-u" old-version))))))))

(defun vc-rcs-checkout (file &optional editable rev workfile)
  "Retrieve a copy of a saved version of FILE into a workfile."
  (let ((filename (or workfile file))
	(file-buffer (get-file-buffer file))
	switches)
    (message "Checking out %s..." filename)
    (save-excursion
      ;; Change buffers to get local value of vc-checkout-switches.
      (if file-buffer (set-buffer file-buffer))
      (setq switches (if (stringp vc-checkout-switches)
			 (list vc-checkout-switches)
		       vc-checkout-switches))
      ;; Save this buffer's default-directory
      ;; and use save-excursion to make sure it is restored
      ;; in the same buffer it was saved in.
      (let ((default-directory default-directory))
	(save-excursion
	  ;; Adjust the default-directory so that the check-out creates
	  ;; the file in the right place.
	  (setq default-directory (file-name-directory filename))
	  (if workfile  ;; RCS
	      ;; RCS can't check out into arbitrary file names directly.
	      ;; Use `co -p' and make stdout point to the correct file.
	      (let ((vc-modes (logior (file-modes (vc-name file))
				      (if editable 128 0)))
		    (failed t))
		(unwind-protect
		    (progn
                      (let ((coding-system-for-read 'no-conversion)
                            (coding-system-for-write 'no-conversion))
                        (with-temp-file filename
                          (apply 'vc-do-command
                                 (current-buffer) 0 "co" (vc-name file)
                                 "-q" ;; suppress diagnostic output
                                 (if editable "-l")
                                 (concat "-p" rev)
                                 switches)))
                      (set-file-modes filename
				      (logior (file-modes (vc-name file))
					      (if editable 128 0)))
		      (setq failed nil))
		  (and failed (file-exists-p filename)
		       (delete-file filename))))
	    (let (new-version)
	      ;; if we should go to the head of the trunk,
	      ;; clear the default branch first
	      (and rev (string= rev "")
		   (vc-rcs-set-default-branch file nil))
	      ;; now do the checkout
	      (apply 'vc-do-command
		     nil 0 "co" (vc-name file)
		     ;; If locking is not strict, force to overwrite
		     ;; the writable workfile.
		     (if (eq (vc-checkout-model file) 'implicit) "-f")
		     (if editable "-l")
		     (if rev (concat "-r" rev)
		       ;; if no explicit revision was specified,
		       ;; check out that of the working file
		       (let ((workrev (vc-workfile-version file)))
			 (if workrev (concat "-r" workrev)
			   nil)))
		     switches)
	      ;; determine the new workfile version
	      (with-current-buffer "*vc*"
		(setq new-version
		      (vc-parse-buffer "^revision \\([0-9.]+\\).*\n" 1)))
	      (vc-file-setprop file 'vc-workfile-version new-version)
	      ;; if necessary, adjust the default branch
	      (and rev (not (string= rev ""))
		   (vc-rcs-set-default-branch 
		    file
		    (if (vc-rcs-latest-on-branch-p file new-version)
			(if (vc-trunk-p new-version) nil
			  (vc-branch-part new-version))
		      new-version))))))
	(message "Checking out %s...done" filename)))))

(defun vc-rcs-revert (file &optional contents-done)
  "Revert FILE to the version it was based on."
  (vc-do-command nil 0 "co" (vc-name file) "-f"
		 (concat "-u" (vc-workfile-version file))))

(defun vc-rcs-cancel-version (file editable)
  "Undo the most recent checkin of FILE.
EDITABLE non-nil means previous version should be locked."
  (let* ((target (vc-workfile-version file))
	 (previous (if (vc-trunk-p target) "" (vc-branch-part target)))
	 (config (current-window-configuration))
	 (done nil))
    (vc-do-command nil 0 "rcs" (vc-name file) (concat "-o" target))
    ;; Check out the most recent remaining version.  If it fails, because
    ;; the whole branch got deleted, do a double-take and check out the
    ;; version where the branch started.
    (while (not done)
      (condition-case err
	  (progn
	    (vc-do-command nil 0 "co" (vc-name file) "-f"
			   (concat (if editable "-l" "-u") previous))
	    (setq done t))
	(error (set-buffer "*vc*")
	       (goto-char (point-min))
	       (if (search-forward "no side branches present for" nil t)
		   (progn (setq previous (vc-branch-part previous))
			  (vc-rcs-set-default-branch file previous)
			  ;; vc-do-command popped up a window with
			  ;; the error message.  Get rid of it, by
			  ;; restoring the old window configuration.
			  (set-window-configuration config))
		 ;; No, it was some other error: re-signal it.
		 (signal (car err) (cdr err))))))))

(defun vc-rcs-merge (file first-version &optional second-version)
  "Merge changes into current working copy of FILE.
The changes are between FIRST-VERSION and SECOND-VERSION."
  (vc-do-command nil 1 "rcsmerge" (vc-name file)
		 "-kk"			; ignore keyword conflicts
		 (concat "-r" first-version)
		 (if second-version (concat "-r" second-version))))

(defun vc-rcs-steal-lock (file &optional rev)
  "Steal the lock on the current workfile for FILE and revision REV.
Needs RCS 5.6.2 or later for -M."
  (vc-do-command nil 0 "rcs" (vc-name file) "-M" (concat "-u" rev))
  ;; Do a real checkout after stealing the lock, so that we see 
  ;; expanded headers.
  (vc-do-command nil 0 "co" (vc-name file) "-f" (concat "-l" rev)))



;;;
;;; History functions
;;;

(defun vc-rcs-print-log (file)
  "Get change log associated with FILE."
  (vc-do-command nil 0 "rlog" (vc-name file)))

(defun vc-rcs-show-log-entry (version)
  (when (re-search-forward
	 ;; also match some context, for safety
	 (concat "----\nrevision " version
		 "\\(\tlocked by:.*\n\\|\n\\)date: ") nil t)
    ;; set the display window so that
    ;; the whole log entry is displayed
    (let (start end lines)
      (beginning-of-line) (forward-line -1) (setq start (point))
      (if (not (re-search-forward "^----*\nrevision" nil t))
	  (setq end (point-max))
	(beginning-of-line) (forward-line -1) (setq end (point)))
      (setq lines (count-lines start end))
      (cond
       ;; if the global information and this log entry fit
       ;; into the window, display from the beginning
       ((< (count-lines (point-min) end) (window-height))
	(goto-char (point-min))
	(recenter 0)
	(goto-char start))
       ;; if the whole entry fits into the window,
       ;; display it centered
       ((< (1+ lines) (window-height))
	(goto-char start)
	(recenter (1- (- (/ (window-height) 2) (/ lines 2)))))
       ;; otherwise (the entry is too large for the window),
       ;; display from the start
       (t
	(goto-char start)
	(recenter 0))))))

(defun vc-rcs-diff (file &optional oldvers newvers)
  "Get a difference report using RCS between two versions of FILE."
  (if (not oldvers) (setq oldvers (vc-workfile-version file)))
  (apply 'vc-do-command "*vc-diff*" 1 "rcsdiff" file
         (append (list "-q"
                       (concat "-r" oldvers)
                       (and newvers (concat "-r" newvers)))
                 (vc-diff-switches-list 'RCS))))


;;;
;;; Snapshot system
;;;

(defun vc-rcs-assign-name (file name)
  "Assign to FILE's latest version a given NAME."
  (vc-do-command nil 0 "rcs" (vc-name file) (concat "-n" name ":")))


;;;
;;; Miscellaneous
;;;

(defun vc-rcs-check-headers ()
  "Check if the current file has any headers in it."
  (save-excursion
    (goto-char (point-min))
         (re-search-forward "\\$[A-Za-z\300-\326\330-\366\370-\377]+\
\\(: [\t -#%-\176\240-\377]*\\)?\\$" nil t)))

(defun vc-rcs-clear-headers ()
  "Implementation of vc-clear-headers for RCS."
  (let ((case-fold-search nil))
    (goto-char (point-min))
    (while (re-search-forward
            (concat "\\$\\(Author\\|Date\\|Header\\|Id\\|Locker\\|Name\\|"
                    "RCSfile\\|Revision\\|Source\\|State\\): [^$\n]+\\$")
            nil t)
      (replace-match "$\\1$"))))

(defun vc-rcs-rename-file (old new)
  ;; Just move the master file (using vc-rcs-master-templates).
  (vc-rename-master (vc-name old) new vc-rcs-master-templates))


;;;
;;; Internal functions
;;;

(defun vc-rcs-workfile-is-newer (file)
  "Return non-nil if FILE is newer than its RCS master.
This likely means that FILE has been changed with respect
to its master version."
  (let ((file-time (nth 5 (file-attributes file)))
	(master-time (nth 5 (file-attributes (vc-name file)))))
    (or (> (nth 0 file-time) (nth 0 master-time))
	(and (= (nth 0 file-time) (nth 0 master-time))
	     (> (nth 1 file-time) (nth 1 master-time))))))

(defun vc-rcs-find-most-recent-rev (branch)
  "Find most recent revision on BRANCH."
  (goto-char (point-min))
  (let ((latest-rev -1) value)
    (while (re-search-forward (concat "^\\(" (regexp-quote branch)
				      "\\.\\([0-9]+\\)\\)\ndate[ \t]+[0-9.]+;")
			      nil t)
      (let ((rev (string-to-number (match-string 2))))
	(when (< latest-rev rev)
	  (setq latest-rev rev)
	  (setq value (match-string 1)))))
    (or value
	(vc-branch-part branch))))

(defun vc-rcs-fetch-master-state (file &optional workfile-version)
  "Compute the master file's idea of the state of FILE.
If a WORKFILE-VERSION is given, compute the state of that version,
otherwise determine the workfile version based on the master file.
This function sets the properties `vc-workfile-version' and
`vc-checkout-model' to their correct values, based on the master
file."
  (with-temp-buffer
    (vc-insert-file (vc-name file) "^[0-9]")
    (let ((workfile-is-latest nil)
	  (default-branch (vc-parse-buffer "^branch[ \t\n]+\\([^;]*\\);" 1)))
      (vc-file-setprop file 'vc-rcs-default-branch default-branch)
      (unless workfile-version
	;; Workfile version not known yet.  Determine that first.  It
	;; is either the head of the trunk, the head of the default
	;; branch, or the "default branch" itself, if that is a full
	;; revision number.
	(cond
	 ;; no default branch
	 ((or (not default-branch) (string= "" default-branch))
	  (setq workfile-version
		(vc-parse-buffer "^head[ \t\n]+\\([^;]+\\);" 1))
	  (setq workfile-is-latest t))
	 ;; default branch is actually a revision
	 ((string-match "^[0-9]+\\.[0-9]+\\(\\.[0-9]+\\.[0-9]+\\)*$"
			default-branch)
	  (setq workfile-version default-branch))
	 ;; else, search for the head of the default branch
	 (t (vc-insert-file (vc-name file) "^desc")
	    (setq workfile-version
		  (vc-rcs-find-most-recent-rev default-branch))
	    (setq workfile-is-latest t)))
	(vc-file-setprop file 'vc-workfile-version workfile-version))
      ;; Check strict locking
      (goto-char (point-min))
      (vc-file-setprop file 'vc-checkout-model
		       (if (re-search-forward ";[ \t\n]*strict;" nil t)
			   'locking 'implicit))
      ;; Compute state of workfile version
      (goto-char (point-min))
      (let ((locking-user
	     (vc-parse-buffer (concat "^locks[ \t\n]+[^;]*[ \t\n]+\\([^:]+\\):"
				      (regexp-quote workfile-version)
				      "[^0-9.]")
			      1)))
	(cond
	 ;; not locked
	 ((not locking-user)
          (if (or workfile-is-latest
                  (vc-rcs-latest-on-branch-p file workfile-version))
              ;; workfile version is latest on branch
              'up-to-date
            ;; workfile version is not latest on branch
            'needs-patch))
	 ;; locked by the calling user
	 ((and (stringp locking-user)
	       (string= locking-user (vc-user-login-name)))
	  (if (or (eq (vc-checkout-model file) 'locking)
		  workfile-is-latest
		  (vc-rcs-latest-on-branch-p file workfile-version))
	      'edited
	    ;; Locking is not used for the file, but the owner does
	    ;; have a lock, and there is a higher version on the current
	    ;; branch.  Not sure if this can occur, and if it is right
	    ;; to use `needs-merge' in this case.
	    'needs-merge))
	 ;; locked by somebody else
	 ((stringp locking-user)
	  locking-user)
	 (t
	  (error "Error getting state of RCS file")))))))

(defun vc-rcs-consult-headers (file)
  "Search for RCS headers in FILE, and set properties accordingly.

Returns: nil            if no headers were found
         'rev           if a workfile revision was found
         'rev-and-lock  if revision and lock info was found"
  (cond
   ((not (get-file-buffer file)) nil)
   ((let (status version locking-user)
     (save-excursion
      (set-buffer (get-file-buffer file))
      (goto-char (point-min))
      (cond
       ;; search for $Id or $Header
       ;; -------------------------
       ;; The `\ 's below avoid an RCS 5.7 bug when checking in this file.
       ((or (and (search-forward "$Id\ : " nil t)
		 (looking-at "[^ ]+ \\([0-9.]+\\) "))
	    (and (progn (goto-char (point-min))
			(search-forward "$Header\ : " nil t))
		 (looking-at "[^ ]+ \\([0-9.]+\\) ")))
	(goto-char (match-end 0))
	;; if found, store the revision number ...
	(setq version (match-string-no-properties 1))
	;; ... and check for the locking state
	(cond
	 ((looking-at
	   (concat "[0-9]+[/-][01][0-9][/-][0-3][0-9] "             ; date
	    "[0-2][0-9]:[0-5][0-9]+:[0-6][0-9]+\\([+-][0-9:]+\\)? " ; time
	           "[^ ]+ [^ ]+ "))                       ; author & state
	  (goto-char (match-end 0)) ; [0-6] in regexp handles leap seconds
	  (cond
	   ;; unlocked revision
	   ((looking-at "\\$")
	    (setq locking-user 'none)
	    (setq status 'rev-and-lock))
	   ;; revision is locked by some user
	   ((looking-at "\\([^ ]+\\) \\$")
	    (setq locking-user (match-string-no-properties 1))
	    (setq status 'rev-and-lock))
	   ;; everything else: false
	   (nil)))
	 ;; unexpected information in
	 ;; keyword string --> quit
	 (nil)))
       ;; search for $Revision
       ;; --------------------
       ((re-search-forward (concat "\\$"
				   "Revision: \\([0-9.]+\\) \\$")
			   nil t)
	;; if found, store the revision number ...
	(setq version (match-string-no-properties 1))
	;; and see if there's any lock information
	(goto-char (point-min))
	(if (re-search-forward (concat "\\$" "Locker:") nil t)
	    (cond ((looking-at " \\([^ ]+\\) \\$")
		   (setq locking-user (match-string-no-properties 1))
		   (setq status 'rev-and-lock))
		  ((looking-at " *\\$")
		   (setq locking-user 'none)
		   (setq status 'rev-and-lock))
		  (t
		   (setq locking-user 'none)
		   (setq status 'rev-and-lock)))
	  (setq status 'rev)))
       ;; else: nothing found
       ;; -------------------
       (t nil)))
     (if status (vc-file-setprop file 'vc-workfile-version version))
     (and (eq status 'rev-and-lock)
	  (vc-file-setprop file 'vc-state
			   (cond
			    ((eq locking-user 'none) 'up-to-date)
			    ((string= locking-user (vc-user-login-name)) 'edited)
			    (t locking-user)))
	  ;; If the file has headers, we don't want to query the
	  ;; master file, because that would eliminate all the
	  ;; performance gain the headers brought us.  We therefore
	  ;; use a heuristic now to find out whether locking is used
	  ;; for this file.  If we trust the file permissions, and the
	  ;; file is not locked, then if the file is read-only we
          ;; assume that locking is used for the file, otherwise
          ;; locking is not used.
	  (not (vc-mistrust-permissions file))
	  (vc-up-to-date-p file)
	  (if (string-match ".r-..-..-." (nth 8 (file-attributes file)))
	      (vc-file-setprop file 'vc-checkout-model 'locking)
	    (vc-file-setprop file 'vc-checkout-model 'implicit)))
     status))))

(defun vc-release-greater-or-equal (r1 r2)
  "Compare release numbers, represented as strings.
Release components are assumed cardinal numbers, not decimal fractions
\(5.10 is a higher release than 5.9\).  Omitted fields are considered
lower \(5.6.7 is earlier than 5.6.7.1\).  Comparison runs till the end
of the string is found, or a non-numeric component shows up \(5.6.7 is
earlier than \"5.6.7 beta\", which is probably not what you want in
some cases\).  This code is suitable for existing RCS release numbers.
CVS releases are handled reasonably, too \(1.3 < 1.4* < 1.5\)."
  (let (v1 v2 i1 i2)
    (catch 'done
      (or (and (string-match "^\\.?\\([0-9]+\\)" r1)
	       (setq i1 (match-end 0))
	       (setq v1 (string-to-number (match-string 1 r1)))
	       (or (and (string-match "^\\.?\\([0-9]+\\)" r2)
			(setq i2 (match-end 0))
			(setq v2 (string-to-number (match-string 1 r2)))
			(if (> v1 v2) (throw 'done t)
			  (if (< v1 v2) (throw 'done nil)
			    (throw 'done
				   (vc-release-greater-or-equal
				    (substring r1 i1)
				    (substring r2 i2)))))))
		   (throw 'done t)))
	  (or (and (string-match "^\\.?\\([0-9]+\\)" r2)
		   (throw 'done nil))
	      (throw 'done t)))))

(defun vc-rcs-release-p (release)
  "Return t if we have RELEASE or better."
  (let ((installation (vc-rcs-system-release)))
    (if (and installation
	     (not (eq installation 'unknown)))
	(vc-release-greater-or-equal installation release))))


(defun vc-rcs-system-release ()
  "Return the RCS release installed on this system, as a string.
Return symbol UNKNOWN if the release cannot be deducted.  The user can
override this using variable `vc-rcs-release'.

If the user has not set variable `vc-rcs-release' and it is nil,
variable `vc-rcs-release' is set to the returned value."
  (or vc-rcs-release
      (setq vc-rcs-release
	    (or (and (zerop (vc-do-command nil nil "rcs" nil "-V"))
		     (with-current-buffer (get-buffer "*vc*")
		       (vc-parse-buffer "^RCS version \\([0-9.]+ *.*\\)" 1)))
		'unknown))))

(defun vc-rcs-set-non-strict-locking (file)
  (vc-do-command nil 0 "rcs" file "-U")
  (vc-file-setprop file 'vc-checkout-model 'implicit)
  (set-file-modes file (logior (file-modes file) 128)))

(defun vc-rcs-set-default-branch (file branch)
  (vc-do-command nil 0 "rcs" (vc-name file) (concat "-b" branch))
  (vc-file-setprop file 'vc-rcs-default-branch branch))

(provide 'vc-rcs)

;;; vc-rcs.el ends here
