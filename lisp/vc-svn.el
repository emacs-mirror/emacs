;;; vc-svn.el --- non-resident support for Subversion version-control

;; Copyright (C) 2003, 2004, 2005 Free Software Foundation, Inc.

;; Author:      FSF (see vc.el for full credits)
;; Maintainer:  Stefan Monnier <monnier@gnu.org>

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This is preliminary support for Subversion (http://subversion.tigris.org/).
;; It started as `sed s/cvs/svn/ vc.cvs.el' (from version 1.56)
;; and hasn't been completely fixed since.

;; Sync'd with Subversion's vc-svn.el as of revision 5801.

;;; Bugs:

;; - VC-dired is (really) slow.

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

(defconst vc-svn-use-edit nil
  ;; Subversion does not provide this feature (yet).
  "*Non-nil means to use `svn edit' to \"check out\" a file.
This is only meaningful if you don't use the implicit checkout model
\(i.e. if you have $SVNREAD set)."
  ;; :type 'boolean
  ;; :version "22.1"
  ;; :group 'vc
  )

;;;
;;; State-querying functions
;;;

;;;###autoload (defun vc-svn-registered (f)
;;;###autoload   (when (file-readable-p (expand-file-name
;;;###autoload 			  ".svn/entries" (file-name-directory f)))
;;;###autoload       (load "vc-svn")
;;;###autoload       (vc-svn-registered f)))

;;;###autoload
(add-to-list 'completion-ignored-extensions ".svn/")

(defun vc-svn-registered (file)
  "Check if FILE is SVN registered."
  (when (file-readable-p (expand-file-name ".svn/entries"
					   (file-name-directory file)))
    (with-temp-buffer
      (cd (file-name-directory file))
      (condition-case nil
	  (vc-svn-command t 0 file "status" "-v")
	;; We can't find an `svn' executable.  We could also deregister SVN.
	(file-error nil))
      (vc-svn-parse-status t)
      (eq 'SVN (vc-file-getprop file 'vc-backend)))))

(defun vc-svn-state (file &optional localp)
  "SVN-specific version of `vc-state'."
  (setq localp (or localp (vc-stay-local-p file)))
  (with-temp-buffer
    (cd (file-name-directory file))
    (vc-svn-command t 0 file "status" (if localp "-v" "-u"))
    (vc-svn-parse-status localp)
    (vc-file-getprop file 'vc-state)))

(defun vc-svn-state-heuristic (file)
  "SVN-specific state heuristic."
  (vc-svn-state file 'local))

(defun vc-svn-dir-state (dir &optional localp)
  "Find the SVN state of all files in DIR."
  (setq localp (or localp (vc-stay-local-p dir)))
  (let ((default-directory dir))
    ;; Don't specify DIR in this command, the default-directory is
    ;; enough.  Otherwise it might fail with remote repositories.
    (with-temp-buffer
      (vc-svn-command t 0 nil "status" (if localp "-v" "-u"))
      (vc-svn-parse-status localp))))

(defun vc-svn-workfile-version (file)
  "SVN-specific version of `vc-workfile-version'."
  ;; There is no need to consult RCS headers under SVN, because we
  ;; get the workfile version for free when we recognize that a file
  ;; is registered in SVN.
  (vc-svn-registered file)
  (vc-file-getprop file 'vc-workfile-version))

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
	   (if (equal (vc-workfile-version file) "0")
	       "(added)" "(modified)"))
	  ((eq svn-state 'needs-patch) "(patch)")
	  ((eq svn-state 'needs-merge) "(merge)"))))


;;;
;;; State-changing functions
;;;

(defun vc-svn-register (file &optional rev comment)
  "Register FILE into the SVN version-control system.
COMMENT can be used to provide an initial description of FILE.

`vc-register-switches' and `vc-svn-register-switches' are passed to
the SVN command (in that order)."
  (apply 'vc-svn-command nil 0 file "add" (vc-switches 'SVN 'register)))

(defun vc-svn-responsible-p (file)
  "Return non-nil if SVN thinks it is responsible for FILE."
  (file-directory-p (expand-file-name ".svn"
				      (if (file-directory-p file)
					  file
					(file-name-directory file)))))

(defalias 'vc-svn-could-register 'vc-svn-responsible-p
  "Return non-nil if FILE could be registered in SVN.
This is only possible if SVN is responsible for FILE's directory.")

(defun vc-svn-checkin (file rev comment)
  "SVN-specific version of `vc-backend-checkin'."
  (let ((status (apply
                 'vc-svn-command nil 1 file "ci"
                 (nconc (list "-m" comment) (vc-switches 'SVN 'checkin)))))
    (set-buffer "*vc*")
    (goto-char (point-min))
    (unless (equal status 0)
      ;; Check checkin problem.
      (cond
       ((search-forward "Transaction is out of date" nil t)
        (vc-file-setprop file 'vc-state 'needs-merge)
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
    ;;  file 'vc-workfile-version
    ;;  (vc-parse-buffer "^\\(new\\|initial\\) revision: \\([0-9.]+\\)" 2))
    ))

(defun vc-svn-find-version (file rev buffer)
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
      ;; If no revision was specified, just make the file writable
      ;; if necessary (using `svn-edit' if requested).
      (and editable (not (eq (vc-svn-checkout-model file) 'implicit))
	   (if vc-svn-use-edit
	       (vc-svn-command nil 0 file "edit")
	     (set-file-modes file (logior (file-modes file) 128))
	     (if (equal file buffer-file-name) (toggle-read-only -1))))
    ;; Check out a particular version (or recreate the file).
    (vc-file-setprop file 'vc-workfile-version nil)
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
    (vc-svn-command nil 0 file "revert"))
  (unless (eq (vc-checkout-model file) 'implicit)
    (if vc-svn-use-edit
        (vc-svn-command nil 0 file "unedit")
      ;; Make the file read-only by switching off all w-bits
      (set-file-modes file (logand (file-modes file) 3950)))))

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
  ;; (vc-file-setprop file 'vc-workfile-version nil)
  (vc-file-setprop file 'vc-checkout-time 0)
  (vc-svn-command nil 0 file "update")
  ;; Analyze the merge result reported by SVN, and set
  ;; file properties accordingly.
  (with-current-buffer (get-buffer "*vc*")
    (goto-char (point-min))
    ;; get new workfile version
    (if (re-search-forward
	 "^\\(Updated to\\|At\\) revision \\([0-9]+\\)" nil t)
	(vc-file-setprop file 'vc-workfile-version (match-string 2))
      (vc-file-setprop file 'vc-workfile-version nil))
    ;; get file status
    (goto-char (point-min))
    (prog1
        (if (looking-at "At revision")
            0 ;; there were no news; indicate success
          (if (re-search-forward
               (concat "^\\([CGDU]  \\)?"
                       (regexp-quote (file-name-nondirectory file)))
               nil t)
              (cond
               ;; Merge successful, we are in sync with repository now
               ((string= (match-string 1) "U  ")
                (vc-file-setprop file 'vc-state 'up-to-date)
                (vc-file-setprop file 'vc-checkout-time
                                 (nth 5 (file-attributes file)))
                0);; indicate success to the caller
               ;; Merge successful, but our own changes are still in the file
               ((string= (match-string 1) "G  ")
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


;;;
;;; History functions
;;;

(defun vc-svn-print-log (file &optional buffer)
  "Get change log associated with FILE."
  (save-current-buffer
    (vc-setup-buffer buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      ;; Add a line to tell log-view-mode what file this is.
      (insert "Working file: " (file-relative-name file) "\n"))
    (vc-svn-command
     buffer
     (if (and (vc-stay-local-p file) (fboundp 'start-process)) 'async 0)
     file "log")))

(defun vc-svn-diff (file &optional oldvers newvers buffer)
  "Get a difference report using SVN between two versions of FILE."
  (unless buffer (setq buffer "*vc-diff*"))
  (if (and oldvers (equal oldvers (vc-workfile-version file)))
      ;; Use nil rather than the current revision because svn handles it
      ;; better (i.e. locally).
      (setq oldvers nil))
  (if (string= (vc-workfile-version file) "0")
      ;; This file is added but not yet committed; there is no master file.
      (if (or oldvers newvers)
	  (error "No revisions of %s exist" file)
	;; We regard this as "changed".
	;; Diff it against /dev/null.
	;; Note: this is NOT a "svn diff".
	(apply 'vc-do-command buffer
	       1 "diff" file
	       (append (vc-switches nil 'diff) '("/dev/null")))
	;; Even if it's empty, it's locally modified.
	1)
    (let* ((switches
	    (if vc-svn-diff-switches
		(vc-switches 'SVN 'diff)
	      (list "-x" (mapconcat 'identity (vc-switches nil 'diff) " "))))
	   (async (and (not vc-disable-async-diff)
                       (vc-stay-local-p file)
		       (or oldvers newvers) ; Svn diffs those locally.
		       (fboundp 'start-process))))
      (apply 'vc-svn-command buffer
	     (if async 'async 0)
	     file "diff"
	     (append
	      switches
	      (when oldvers
		(list "-r" (if newvers (concat oldvers ":" newvers)
			     oldvers)))))
      (if async 1		      ; async diff => pessimistic assumption
	;; For some reason `svn diff' does not return a useful
	;; status w.r.t whether the diff was empty or not.
	(buffer-size (get-buffer buffer))))))

(defun vc-svn-diff-tree (dir &optional rev1 rev2)
  "Diff all files at and below DIR."
  (vc-svn-diff (file-name-as-directory dir) rev1 rev2))

;;;
;;; Snapshot system
;;;

(defun vc-svn-create-snapshot (dir name branchp)
  "Assign to DIR's current version a given NAME.
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

(defun vc-svn-command (buffer okstatus file &rest flags)
  "A wrapper around `vc-do-command' for use in vc-svn.el.
The difference to vc-do-command is that this function always invokes `svn',
and that it passes `vc-svn-global-switches' to it before FLAGS."
  (apply 'vc-do-command buffer okstatus "svn" file
         (if (stringp vc-svn-global-switches)
             (cons vc-svn-global-switches flags)
           (append vc-svn-global-switches
                   flags))))

(defun vc-svn-repository-hostname (dirname)
  (with-temp-buffer
    (let ((coding-system-for-read
	   (or file-name-coding-system
	       default-file-name-coding-system)))
      (vc-insert-file (expand-file-name ".svn/entries" dirname)))
    (goto-char (point-min))
    (when (re-search-forward
	   ;; Old `svn' used name="svn:dir", newer use just name="".
	   (concat "name=\"\\(?:svn:this_dir\\)?\"[\n\t ]*"
		   "\\(?:[-a-z]+=\"[^\"]*\"[\n\t ]*\\)*?"
		   "url=\"\\([^\"]+\\)\"") nil t)
      ;; This is not a hostname but a URL.  This may actually be considered
      ;; as a feature since it allows vc-svn-stay-local to specify different
      ;; behavior for different modules on the same server.
      (match-string 1))))

(defun vc-svn-parse-status (localp)
  "Parse output of \"svn status\" command in the current buffer.
Set file properties accordingly.  Unless FULL is t, parse only
essential information."
  (let (file status)
    (goto-char (point-min))
    (while (re-search-forward
	    "^[ ADMCI?!~][ MC][ L][ +][ S]..\\([ *]\\) +\\([-0-9]+\\) +\\([0-9?]+\\) +\\([^ ]+\\) +" nil t)
      (setq file (expand-file-name
		  (buffer-substring (point) (line-end-position))))
      (setq status (char-after (line-beginning-position)))
      (unless (eq status ??)
	(vc-file-setprop file 'vc-backend 'SVN)
	;; Use the last-modified revision, so that searching in vc-print-log
	;; output works.
	(vc-file-setprop file 'vc-workfile-version (match-string 3))
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
	   (vc-file-setprop file 'vc-workfile-version "0")
	   (vc-file-setprop file 'vc-checkout-time 0)
	   'edited)
	  ((memq status '(?M ?C))
	   (if (eq (char-after (match-beginning 1)) ?*)
	       'needs-merge
	     'edited))
	  (t 'edited)))))))

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

(defun vc-svn-valid-version-number-p (tag)
  "Return non-nil if TAG is a valid version number."
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
