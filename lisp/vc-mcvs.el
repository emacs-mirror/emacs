;;; vc-mcvs.el --- VC backend for the Meta-CVS version-control system

;; Copyright (C) 1995,98,99,2000,01,02,2003  Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The home page of the Meta-CVS version control system is at
;; 
;;      http://users.footprints.net/~kaz/mcvs.html
;; 
;; This is derived from vc-cvs.el as follows:
;; - cp vc-cvs.el vc-mcvs.el
;; - Replace CVS/ with MCVS/CVS/
;; - Replace 'CVS with 'MCVS
;; - Replace -cvs- with -mcvs-
;; - Replace most of the rest of CVS to Meta-CVS
;;
;; Then of course started the hacking.  Only a small part of the code
;; has been touched and not much more than that was tested, so if
;; you bump into a bug, don't be surprised: just report it to me.
;;
;; What has been partly tested:
;; - C-x v v to start editing a file that was checked out with CVSREAD on.
;; - C-x v v to commit a file
;; - C-x v =
;; - C-x v l
;; - C-x v i
;; - C-x v g

;;; Bugs:

;; - VC-dired doesn't work.

;;; Code:

(eval-when-compile (require 'vc))
(require 'vc-cvs)

;;;
;;; Customization options
;;;

(defcustom vc-mcvs-global-switches nil
  "*Global switches to pass to any Meta-CVS command."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string))
  :version "21.4"
  :group 'vc)

(defcustom vc-mcvs-register-switches nil
  "*Extra switches for registering a file into Meta-CVS.
A string or list of strings passed to the checkin program by
\\[vc-register]."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string))
  :version "21.4"
  :group 'vc)

(defcustom vc-mcvs-diff-switches nil
  "*A string or list of strings specifying extra switches for cvs diff under VC."
    :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string))
  :version "21.4"
  :group 'vc)

(defcustom vc-mcvs-header (or (cdr (assoc 'MCVS vc-header-alist))
			      vc-cvs-header)
  "*Header keywords to be inserted by `vc-insert-headers'."
  :version "21.4"
  :type '(repeat string)
  :group 'vc)

(defcustom vc-mcvs-use-edit vc-cvs-use-edit
  "*Non-nil means to use `cvs edit' to \"check out\" a file.
This is only meaningful if you don't use the implicit checkout model
\(i.e. if you have $CVSREAD set)."
  :type 'boolean
  :version "21.4"
  :group 'vc)

(defcustom vc-mcvs-stay-local vc-cvs-stay-local
  "*Non-nil means use local operations when possible for remote repositories.
This avoids slow queries over the network and instead uses heuristics
and past information to determine the current status of a file.
The value can also be a regular expression to match against the host name
of a repository; then VC only stays local for hosts that match it."
  :type '(choice (const :tag "Always stay local" t)
		 (string :tag "Host regexp")
		 (const :tag "Don't stay local" nil))
  :version "21.4"
  :group 'vc)

;;;
;;; State-querying functions
;;;

;;;###autoload (defun vc-mcvs-registered (file)
;;;###autoload   (let ((dir file))
;;;###autoload     (while (and (stringp dir)
;;;###autoload 		(not (equal dir (setq dir (file-name-directory dir)))))
;;;###autoload       (setq dir (if (file-directory-p
;;;###autoload 		     (expand-file-name "MCVS/CVS" dir))
;;;###autoload 		    t (directory-file-name dir))))
;;;###autoload     (if (eq dir t)
;;;###autoload 	(progn
;;;###autoload 	  (load "vc-mcvs")
;;;###autoload 	  (vc-mcvs-registered file)))))

(defun vc-mcvs-root (file)
  "Return the root directory of a Meta-CVS project, if any."
  (or (vc-file-getprop file 'mcvs-root)
      (vc-file-setprop
       file 'mcvs-root
       (let ((root nil))
	 (while (not (or root
			 (equal file (setq file (file-name-directory file)))))
	   (if (file-directory-p (expand-file-name "MCVS/CVS" file))
	       (setq root file)
	     (setq file (directory-file-name file))))
	 root))))

(defun vc-mcvs-read (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (read (current-buffer))))

(defun vc-mcvs-map-file (dir file)
  (let ((map (vc-mcvs-read (expand-file-name "MCVS/MAP" dir)))
	inode)
    (dolist (x map inode)
      (if (equal (nth 2 x) file) (setq inode (nth 1 x))))))

(defun vc-mcvs-registered (file)
  (let (root inode cvsfile)
    (when (and (setq root (vc-mcvs-root file))
	       (setq inode (vc-mcvs-map-file
			    root (file-relative-name file root))))
      (vc-file-setprop file 'mcvs-inode inode)
      ;; Avoid calling `mcvs diff' in vc-workfile-unchanged-p.
      (vc-file-setprop file 'vc-checkout-time
		       (if (vc-cvs-registered
			    (setq cvsfile (expand-file-name inode root)))
			   (vc-file-getprop cvsfile 'vc-checkout-time)
			 ;; The file might not be registered yet because
			 ;; of lazy-adding.
			 0))
      t)))

(defmacro vc-mcvs-cvs (op file &rest args)
  (declare (debug t))
  `(,(intern (concat "vc-cvs-" (symbol-name op)))
    (expand-file-name (vc-file-getprop ,file 'mcvs-inode)
		      (vc-file-getprop ,file 'mcvs-root))
    ,@args))

(defun vc-mcvs-state (file)
  ;; This would assume the Meta-CVS sandbox is synchronized.
  ;; (vc-mcvs-cvs state file))
  "Meta-CVS-specific version of `vc-state'."
  (if (vc-mcvs-stay-local-p file)
      (let ((state (vc-file-getprop file 'vc-state)))
        ;; If we should stay local, use the heuristic but only if
        ;; we don't have a more precise state already available.
	(if (memq state '(up-to-date edited))
	    (vc-mcvs-state-heuristic file)
	  state))
    (with-temp-buffer
      (cd (file-name-directory file))
      (vc-mcvs-command t 0 file "status")
      (vc-cvs-parse-status t))))


(defalias 'vc-mcvs-state-heuristic 'vc-cvs-state-heuristic)

(defun vc-mcvs-dir-state (dir)
  "Find the Meta-CVS state of all files in DIR."
  ;; if DIR is not under Meta-CVS control, don't do anything.
  (when (file-readable-p (expand-file-name "MCVS/CVS/Entries" dir))
    (if (vc-mcvs-stay-local-p dir)
	(vc-mcvs-dir-state-heuristic dir)
      (let ((default-directory dir))
	;; Don't specify DIR in this command, the default-directory is
	;; enough.  Otherwise it might fail with remote repositories.
	(with-temp-buffer
	  (vc-mcvs-command t 0 nil "status" "-l")
	  (goto-char (point-min))
	  (while (re-search-forward "^=+\n\\([^=\n].*\n\\|\n\\)+" nil t)
	    (narrow-to-region (match-beginning 0) (match-end 0))
	    (vc-cvs-parse-status)
	    (goto-char (point-max))
	    (widen)))))))

(defun vc-mcvs-workfile-version (file) (vc-mcvs-cvs workfile-version file))

(defalias 'vc-mcvs-checkout-model 'vc-cvs-checkout-model)

(defun vc-mcvs-mode-line-string (file) (vc-mcvs-cvs mode-line-string file))

;;;
;;; State-changing functions
;;;

(defun vc-mcvs-register (file &optional rev comment)
  "Register FILE into the Meta-CVS version-control system.
COMMENT can be used to provide an initial description of FILE.

`vc-register-switches' and `vc-mcvs-register-switches' are passed to
the Meta-CVS command (in that order)."
  (let* ((filename (file-name-nondirectory file))
	 (extpos (string-match "\\." filename))
	 (ext (if extpos (substring filename (1+ extpos))))
	 (root (vc-mcvs-root file))
	 (types-file (expand-file-name "MCVS/TYPES" root))
	 (map-file (expand-file-name "MCVS/MAP" root))
	 (types (vc-mcvs-read types-file)))
    ;; Make sure meta files like MCVS/MAP are not read-only (happens with
    ;; CVSREAD) since Meta-CVS doesn't pay attention to it at all and goes
    ;; belly-up.
    (unless (file-writable-p map-file)
      (vc-checkout map-file t))
    (unless (or (file-writable-p types-file) (not (file-exists-p types-file)))
      (vc-checkout types-file t))
    ;; Make sure the `mcvs add' will not fire up the CVSEDITOR
    ;; to add a rule for the given file's extension.
    (when (and ext (not (assoc ext types)))
      (let ((type (completing-read "Type to use [default]: "
				   '("default" "name-only" "keep-old"
				     "binary" "value-only")
				   nil t nil nil "default")))
	(push (list ext (make-symbol (upcase (concat ":" type)))) types)
	(setq types (sort types (lambda (x y) (string< (car x) (car y)))))
	(with-current-buffer (find-file-noselect types-file)
	  (erase-buffer)
	  (pp types (current-buffer))
	  (save-buffer)
	  (unless (get-buffer-window (current-buffer) t)
	    (kill-buffer (current-buffer)))))))
  ;; Now do the ADD.
  (let ((switches (append
		   (if (stringp vc-register-switches)
		       (list vc-register-switches)
		     vc-register-switches)
		   (if (stringp vc-mcvs-register-switches)
		       (list vc-mcvs-register-switches)
		     vc-mcvs-register-switches))))
    (prog1 (apply 'vc-mcvs-command nil 0 file
		  "add"
		  (and comment (string-match "[^\t\n ]" comment)
		       (concat "-m" comment))
		  switches)
      ;; I'm not sure exactly why, but if we don't setup the inode and root
      ;; prop of the file, things break later on in vc-mode-line that
      ;; ends up calling vc-mcvs-workfile-version.
      ;; We also need to set vc-checkout-time so that vc-workfile-unchanged-p
      ;; doesn't try to call `mcvs diff' on the file.
      (vc-mcvs-registered file))))

(defalias 'vc-mcvs-responsible-p 'vc-mcvs-root
  "Return non-nil if CVS thinks it is responsible for FILE.")

(defalias 'vc-cvs-could-register 'vc-cvs-responsible-p
  "Return non-nil if FILE could be registered in Meta-CVS.
This is only possible if Meta-CVS is responsible for FILE's directory.")

(defun vc-mcvs-checkin (file rev comment)
  "Meta-CVS-specific version of `vc-backend-checkin'."
  (let ((switches (if (stringp vc-checkin-switches)
		      (list vc-checkin-switches)
		    vc-checkin-switches))
	status)
    (if (or (not rev) (vc-mcvs-valid-version-number-p rev))
        (setq status (apply 'vc-mcvs-command nil 1 file
                            "ci" (if rev (concat "-r" rev))
                            "-m" comment
                            switches))
      (if (not (vc-mcvs-valid-symbolic-tag-name-p rev))
          (error "%s is not a valid symbolic tag name" rev)
        ;; If the input revison is a valid symbolic tag name, we create it
        ;; as a branch, commit and switch to it.
        (apply 'vc-mcvs-command nil 0 file "tag" "-b" (list rev))
        (apply 'vc-mcvs-command nil 0 file "update" "-r" (list rev))
        (setq status (apply 'vc-mcvs-command nil 1 file
                            "ci"
                            "-m" comment
                            switches))
        (vc-file-setprop file 'vc-mcvs-sticky-tag rev)))
    (set-buffer "*vc*")
    (goto-char (point-min))
    (when (not (zerop status))
      ;; Check checkin problem.
      (cond
       ((re-search-forward "Up-to-date check failed" nil t)
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
    (vc-file-setprop
     file 'vc-workfile-version
     (vc-parse-buffer "^\\(new\\|initial\\) revision: \\([0-9.]+\\)" 2))
    ;; Forget the checkout model of the file, because we might have
    ;; guessed wrong when we found the file.  After commit, we can
    ;; tell it from the permissions of the file (see
    ;; vc-mcvs-checkout-model).
    (vc-file-setprop file 'vc-checkout-model nil)

    ;; if this was an explicit check-in (does not include creation of
    ;; a branch), remove the sticky tag.
    (if (and rev (not (vc-mcvs-valid-symbolic-tag-name-p rev)))
	(vc-mcvs-command nil 0 file "update" "-A"))))

(defun vc-mcvs-find-version (file rev buffer)
  (apply 'vc-mcvs-command
	 buffer 0 file
	 "-Q"				; suppress diagnostic output
	 "update"
	 (and rev (not (string= rev ""))
	      (concat "-r" rev))
	 "-p"
	 (if (stringp vc-checkout-switches)
	     (list vc-checkout-switches)
	   vc-checkout-switches)))

(defun vc-mcvs-checkout (file &optional editable rev)
  (message "Checking out %s..." file)
  (with-current-buffer (or (get-file-buffer file) (current-buffer))
    (let ((switches (if (stringp vc-checkout-switches)
			(list vc-checkout-switches)
		      vc-checkout-switches)))
      (vc-call update file editable rev switches)))
  (vc-mode-line file)
  (message "Checking out %s...done" file))

(defun vc-mcvs-update (file editable rev switches)
  (if (and (file-exists-p file) (not rev))
      ;; If no revision was specified, just make the file writable
      ;; if necessary (using `cvs-edit' if requested).
      (and editable (not (eq (vc-mcvs-checkout-model file) 'implicit))
	   (if vc-mcvs-use-edit
	       (vc-mcvs-command nil 0 file "edit")
	     (set-file-modes file (logior (file-modes file) 128))
	     (if (equal file buffer-file-name) (toggle-read-only -1))))
    ;; Check out a particular version (or recreate the file).
    (vc-file-setprop file 'vc-workfile-version nil)
    (apply 'vc-mcvs-command nil 0 file
	   (if editable "-w")
	   "update"
	   ;; default for verbose checkout: clear the sticky tag so
	   ;; that the actual update will get the head of the trunk
	   (if (or (not rev) (string= rev ""))
	       "-A"
	     (concat "-r" rev))
	   switches)))

(defun vc-mcvs-revert (file &optional contents-done)
  "Revert FILE to the version it was based on."
  (vc-default-revert file contents-done)
  (unless (eq (vc-checkout-model file) 'implicit)
    (if vc-mcvs-use-edit
        (vc-mcvs-command nil 0 file "unedit")
      ;; Make the file read-only by switching off all w-bits
      (set-file-modes file (logand (file-modes file) 3950)))))

(defun vc-mcvs-merge (file first-version &optional second-version)
  "Merge changes into current working copy of FILE.
The changes are between FIRST-VERSION and SECOND-VERSION."
  (vc-mcvs-command nil 0 file
		   "update" "-kk"
		   (concat "-j" first-version)
		   (concat "-j" second-version))
  (vc-file-setprop file 'vc-state 'edited)
  (with-current-buffer (get-buffer "*vc*")
    (goto-char (point-min))
    (if (re-search-forward "conflicts during merge" nil t)
        1				; signal error
      0)))				; signal success

(defun vc-mcvs-merge-news (file)
  "Merge in any new changes made to FILE."
  (message "Merging changes into %s..." file)
  ;; (vc-file-setprop file 'vc-workfile-version nil)
  (vc-file-setprop file 'vc-checkout-time 0)
  (vc-mcvs-command nil 0 file "update")
  ;; Analyze the merge result reported by Meta-CVS, and set
  ;; file properties accordingly.
  (with-current-buffer (get-buffer "*vc*")
    (goto-char (point-min))
    ;; get new workfile version
    (if (re-search-forward
	 "^Merging differences between [0-9.]* and \\([0-9.]*\\) into" nil t)
	(vc-file-setprop file 'vc-workfile-version (match-string 1))
      (vc-file-setprop file 'vc-workfile-version nil))
    ;; get file status
    (prog1
        (if (eq (buffer-size) 0)
            0 ;; there were no news; indicate success
          (if (re-search-forward
               (concat "^\\([CMUP] \\)?"
                       ".*"
                       "\\( already contains the differences between \\)?")
               nil t)
              (cond
               ;; Merge successful, we are in sync with repository now
               ((or (match-string 2)
                    (string= (match-string 1) "U ")
                    (string= (match-string 1) "P "))
                (vc-file-setprop file 'vc-state 'up-to-date)
                (vc-file-setprop file 'vc-checkout-time
                                 (nth 5 (file-attributes file)))
                0);; indicate success to the caller
               ;; Merge successful, but our own changes are still in the file
               ((string= (match-string 1) "M ")
                (vc-file-setprop file 'vc-state 'edited)
                0);; indicate success to the caller
               ;; Conflicts detected!
               (t
                (vc-file-setprop file 'vc-state 'edited)
                1);; signal the error to the caller
               )
            (pop-to-buffer "*vc*")
            (error "Couldn't analyze mcvs update result")))
      (message "Merging changes into %s...done" file))))

;;;
;;; History functions
;;;

(defun vc-mcvs-print-log (file)
  "Get change log associated with FILE."
  (vc-mcvs-command
   nil
   (if (and (vc-mcvs-stay-local-p file) (fboundp 'start-process)) 'async 0)
   file "log"))

(defun vc-mcvs-diff (file &optional oldvers newvers)
  "Get a difference report using Meta-CVS between two versions of FILE."
  (let (status (diff-switches-list (vc-diff-switches-list 'MCVS)))
    (if (string= (vc-workfile-version file) "0")
	;; This file is added but not yet committed; there is no master file.
	(if (or oldvers newvers)
	    (error "No revisions of %s exist" file)
	  ;; We regard this as "changed".
	  ;; Diff it against /dev/null.
          ;; Note: this is NOT a "mcvs diff".
          (apply 'vc-do-command "*vc-diff*"
                 1 "diff" file
                 (append diff-switches-list '("/dev/null"))))
      (setq status
            (apply 'vc-mcvs-command "*vc-diff*"
                   (if (and (vc-mcvs-stay-local-p file)
			    (fboundp 'start-process))
		       'async
		     1)
                   file "diff"
                   (and oldvers (concat "-r" oldvers))
                   (and newvers (concat "-r" newvers))
                   diff-switches-list))
      (if (vc-mcvs-stay-local-p file)
          1 ;; async diff, pessimistic assumption
        status))))

(defun vc-mcvs-diff-tree (dir &optional rev1 rev2)
  "Diff all files at and below DIR."
  (with-current-buffer "*vc-diff*"
    (setq default-directory dir)
    (if (vc-mcvs-stay-local-p dir)
        ;; local diff: do it filewise, and only for files that are modified
        (vc-file-tree-walk
         dir
         (lambda (f)
           (vc-exec-after
            `(let ((coding-system-for-read (vc-coding-system-for-diff ',f)))
               ;; possible optimization: fetch the state of all files
               ;; in the tree via vc-mcvs-dir-state-heuristic
               (unless (vc-up-to-date-p ',f)
                 (message "Looking at %s" ',f)
                 (vc-diff-internal ',f ',rev1 ',rev2))))))
      ;; cvs diff: use a single call for the entire tree
      (let ((coding-system-for-read
             (or coding-system-for-read 'undecided)))
        (apply 'vc-mcvs-command "*vc-diff*" 1 nil "diff"
               (and rev1 (concat "-r" rev1))
               (and rev2 (concat "-r" rev2))
               (vc-diff-switches-list 'MCVS))))))

(defun vc-mcvs-annotate-command (file buffer &optional version)
  "Execute \"mcvs annotate\" on FILE, inserting the contents in BUFFER.
Optional arg VERSION is a version to annotate from."
  (vc-mcvs-command
   buffer
   (if (and (vc-mcvs-stay-local-p file) (fboundp 'start-process)) 'async 0)
   file "annotate" (if version (concat "-r" version))))

(defalias 'vc-mcvs-annotate-current-time 'vc-cvs-annotate-current-time)
(defalias 'vc-mcvs-annotate-time 'vc-cvs-annotate-time)

;;;
;;; Snapshot system
;;;

(defun vc-mcvs-create-snapshot (dir name branchp)
  "Assign to DIR's current version a given NAME.
If BRANCHP is non-nil, the name is created as a branch (and the current
workspace is immediately moved to that new branch)."
  (vc-mcvs-command nil 0 dir "tag" "-c" (if branchp "-b") name)
  (when branchp (vc-mcvs-command nil 0 dir "update" "-r" name)))

(defun vc-mcvs-retrieve-snapshot (dir name update)
  "Retrieve a snapshot at and below DIR.
NAME is the name of the snapshot; if it is empty, do a `cvs update'.
If UPDATE is non-nil, then update (resynch) any affected buffers."
  (with-current-buffer (get-buffer-create "*vc*")
    (let ((default-directory dir)
	  (sticky-tag))
      (erase-buffer)
      (if (or (not name) (string= name ""))
	  (vc-mcvs-command t 0 nil "update")
	(vc-mcvs-command t 0 nil "update" "-r" name)
	(setq sticky-tag name))
      (when update
	(goto-char (point-min))
	(while (not (eobp))
	  (if (looking-at "\\([CMUP]\\) \\(.*\\)")
	      (let* ((file (expand-file-name (match-string 2) dir))
		     (state (match-string 1))
		     (buffer (find-buffer-visiting file)))
		(when buffer
		  (cond
		   ((or (string= state "U")
			(string= state "P"))
		    (vc-file-setprop file 'vc-state 'up-to-date)
		    (vc-file-setprop file 'vc-workfile-version nil)
		    (vc-file-setprop file 'vc-checkout-time
				     (nth 5 (file-attributes file))))
		   ((or (string= state "M")
			(string= state "C"))
		    (vc-file-setprop file 'vc-state 'edited)
		    (vc-file-setprop file 'vc-workfile-version nil)
		    (vc-file-setprop file 'vc-checkout-time 0)))
		  (vc-file-setprop file 'vc-mcvs-sticky-tag sticky-tag)
		  (vc-resynch-buffer file t t))))
	  (forward-line 1))))))


;;;
;;; Miscellaneous
;;;

(defalias 'vc-mcvs-make-version-backups-p 'vc-mcvs-stay-local-p
  "Return non-nil if version backups should be made for FILE.")
(defalias 'vc-mcvs-check-headers 'vc-cvs-check-headers)


;;;
;;; Internal functions
;;;

(defun vc-mcvs-command (buffer okstatus file &rest flags)
  "A wrapper around `vc-do-command' for use in vc-mcvs.el.
The difference to vc-do-command is that this function always invokes `mcvs',
and that it passes `vc-mcvs-global-switches' to it before FLAGS."
  (let ((args (append '("--error-continue")
		      (if (stringp vc-mcvs-global-switches)
			  (cons vc-mcvs-global-switches flags)
			(append vc-mcvs-global-switches
				flags)))))
    (if (member (car flags) '("diff" "log"))
	;; We need to filter the output.
	(vc-do-command buffer okstatus "sh" nil "-c"
		       (concat "mcvs "
			       (mapconcat
				'shell-quote-argument
				(append (remq nil args)
					(if file (list (file-relative-name file))))
				" ")
			       " | mcvs filt"))
      (apply 'vc-do-command buffer okstatus "mcvs" file args))))

(defun vc-mcvs-stay-local-p (file) (vc-mcvs-cvs stay-local-p file))

(defun vc-mcvs-dir-state-heuristic (dir)
  "Find the Meta-CVS state of all files in DIR, using only local information."
  (with-temp-buffer
    (vc-cvs-get-entries dir)
    (goto-char (point-min))
    (while (not (eobp))
      ;; Meta-MCVS-removed files are not taken under VC control.
      (when (looking-at "/\\([^/]*\\)/[^/-]")
	(let ((file (expand-file-name (match-string 1) dir)))
	  (unless (vc-file-getprop file 'vc-state)
	    (vc-cvs-parse-entry file t))))
      (forward-line 1))))

(defalias 'vc-mcvs-valid-symbolic-tag-name-p 'vc-cvs-valid-symbolic-tag-name-p)
(defalias 'vc-mcvs-valid-version-number-p 'vc-cvs-valid-version-number-p)

(provide 'vc-mcvs)
;;; vc-mcvs.el ends here
