;;; vc-sccs.el --- support for SCCS version-control  -*- lexical-binding:t -*-

;; Copyright (C) 1992-2018 Free Software Foundation, Inc.

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

;;; Code:

(eval-when-compile
  (require 'vc))

;;;
;;; Customization options
;;;

;; ;; Maybe a better solution is to not use "get" but "sccs get".
;; ;; Note for GNU CSSC, you can parse sccs -V to get the libexec path.
;;  (defcustom vc-sccs-path
;;    (prune-directory-list '("/usr/ccs/bin" "/usr/sccs" "/usr/lib/sccs"
;; 			   "/usr/libexec/sccs"))
;;    "List of extra directories to search for SCCS commands."
;;    :type '(repeat directory)
;;    :group 'vc)

(defgroup vc-sccs nil
  "VC SCCS backend."
  :version "24.1"
  :group 'vc)

(defcustom vc-sccs-register-switches nil
  "Switches for registering a file in SCCS.
A string or list of strings passed to the checkin program by
\\[vc-register].  If nil, use the value of `vc-register-switches'.
If t, use no switches."
  :type '(choice (const :tag "Unspecified" nil)
		 (const :tag "None" t)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List" :value ("") string))
  :version "21.1"
  :group 'vc-sccs)

(defcustom vc-sccs-diff-switches nil
  "String or list of strings specifying switches for SCCS diff under VC.
If nil, use the value of `vc-diff-switches'.  If t, use no switches."
  :type '(choice (const :tag "Unspecified" nil)
		 (const :tag "None" t)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List" :value ("") string))
  :version "21.1"
  :group 'vc-sccs)

(defcustom vc-sccs-header '("%W%")
  "Header keywords to be inserted by `vc-insert-headers'."
  :type '(repeat string)
  :version "24.1"     ; no longer consult the obsolete vc-header-alist
  :group 'vc-sccs)

;; This needs to be autoloaded because vc-sccs-registered uses it (via
;; vc-default-registered), and vc-hooks needs to be able to check
;; for a registered backend without loading every backend.
;;;###autoload
(defcustom vc-sccs-master-templates
  (purecopy '("%sSCCS/s.%s" "%ss.%s" vc-sccs-search-project-dir))
  "Where to look for SCCS master files.
For a description of possible values, see `vc-check-master-templates'."
  :type '(choice (const :tag "Use standard SCCS file names"
			("%sSCCS/s.%s" "%ss.%s" vc-sccs-search-project-dir))
		 (repeat :tag "User-specified"
			 (choice string
				 function)))
  :version "21.1"
  :group 'vc-sccs)


;;;
;;; Internal variables
;;;

(defconst vc-sccs-name-assoc-file "VC-names")


;;; Properties of the backend

(defun vc-sccs-revision-granularity () 'file)
(defun vc-sccs-checkout-model (_files) 'locking)

;;;
;;; State-querying functions
;;;

;; The autoload cookie below places vc-sccs-registered directly into
;; loaddefs.el, so that vc-sccs.el does not need to be loaded for
;; every file that is visited.
;;;###autoload
(progn
(defun vc-sccs-registered (f) (vc-default-registered 'SCCS f)))

(defun vc-sccs-state (file)
  "SCCS-specific function to compute the version control state."
  (if (not (vc-sccs-registered file))
      'unregistered
    (with-temp-buffer
      (if (vc-insert-file (vc-sccs-lock-file file))
	  (let* ((locks (vc-sccs-parse-locks))
		 (working-revision (vc-working-revision file))
		 (locking-user (cdr (assoc working-revision locks))))
	    (if (not locking-user)
		(if (vc-sccs-workfile-unchanged-p file)
		    'up-to-date
		  'unlocked-changes)
	      (if (string= locking-user (vc-user-login-name file))
		  'edited
		locking-user)))
	'up-to-date))))

(autoload 'vc-expand-dirs "vc")

(defun vc-sccs-dir-status-files (dir files update-function)
  (if (not files) (setq files (vc-expand-dirs (list dir) 'RCS)))
  (let ((result nil))
    (dolist (file files)
      (let ((state (vc-state file))
	    (frel (file-relative-name file)))
	(when (and (eq (vc-backend file) 'SCCS)
		   (not (eq state 'up-to-date)))
	  (push (list frel state) result))))
    (funcall update-function result)))

(autoload 'vc-master-name "vc-filewise")

(defun vc-sccs-working-revision (file)
  "SCCS-specific version of `vc-working-revision'."
  (when (and (file-regular-p file) (vc-master-name file))
    (with-temp-buffer
      ;; The working revision is always the latest revision number.
      ;; To find this number, search the entire delta table,
      ;; rather than just the first entry, because the
      ;; first entry might be a deleted ("R") revision.
      (vc-insert-file (vc-master-name file) "^\001e\n\001[^s]")
      (vc-parse-buffer "^\001d D \\([^ ]+\\)" 1))))

;; Cf vc-sccs-find-revision.
(defun vc-sccs-write-revision (file outfile &optional rev)
  "Write the SCCS version of input file FILE to output file OUTFILE.
Optional string REV is a revision."
  (with-temp-buffer
    (apply 'vc-sccs-do-command t 0 "get" (vc-master-name file)
	   (append '("-s" "-p" "-k") ; -k: no keyword expansion
		   (if rev (list (concat "-r" rev)))))
    (write-region nil nil outfile nil 'silent)))

(defun vc-sccs-workfile-unchanged-p (file)
  "Has FILE remained unchanged since last checkout?"
  (let ((tempfile (make-temp-file "vc-sccs")))
    (unwind-protect
	(progn
	  (vc-sccs-write-revision file tempfile (vc-working-revision file))
	  (zerop (vc-do-command "*vc*" 1 "cmp" file tempfile)))
      (delete-file tempfile))))


;;;
;;; State-changing functions
;;;

(defun vc-sccs-do-command (buffer okstatus command file-or-list &rest flags)
  ;; (let ((load-path (append vc-sccs-path load-path)))
  ;;   (apply 'vc-do-command buffer okstatus command file-or-list flags))
  (apply 'vc-do-command (or buffer "*vc*") okstatus "sccs" file-or-list command flags))

(defun vc-sccs-create-repo ()
  "Create a new SCCS repository."
  ;; SCCS is totally file-oriented, so all we have to do is make the directory
  (make-directory "SCCS"))

(autoload 'vc-switches "vc")

(defun vc-sccs-register (files &optional comment)
  "Register FILES into the SCCS version-control system.
Automatically retrieve a read-only version of the files with keywords expanded.
COMMENT can be used to provide an initial description of FILES.
Passes either `vc-sccs-register-switches' or `vc-register-switches'
to the SCCS command."
  (dolist (file files)
    (let* ((dirname (or (file-name-directory file) ""))
	   (basename (file-name-nondirectory file))
	   (project-file (vc-sccs-search-project-dir dirname basename)))
      (let ((vc-master-name
	     (or project-file
		 (format (car vc-sccs-master-templates) dirname basename))))
	(apply 'vc-sccs-do-command nil 0 "admin" vc-master-name
	       "-fb"
	       (concat "-i" (file-relative-name file))
	       (and comment (concat "-y" comment))
	       (vc-switches 'SCCS 'register)))
      (delete-file file)
      (vc-sccs-do-command nil 0 "get" (vc-master-name file)))))

(defun vc-sccs-responsible-p (file)
  "Return non-nil if SCCS thinks it would be responsible for registering FILE."
  ;; TODO: check for all the patterns in vc-sccs-master-templates
  (or (file-directory-p (expand-file-name "SCCS" (file-name-directory file)))
      (stringp (vc-sccs-search-project-dir (or (file-name-directory file) "")
					   (file-name-nondirectory file)))))

(defun vc-sccs-checkin (files comment &optional rev)
  "SCCS-specific version of `vc-backend-checkin'."
  (dolist (file (vc-expand-dirs files 'SCCS))
    (apply 'vc-sccs-do-command nil 0 "delta" (vc-master-name file)
           (if rev (concat "-r" rev))
	   (concat "-y" comment)
	   (vc-switches 'SCCS 'checkin))
	(vc-sccs-do-command nil 0 "get" (vc-master-name file))))

(defun vc-sccs-find-revision (file rev buffer)
  (apply 'vc-sccs-do-command
	 buffer 0 "get" (vc-master-name file)
	 "-s" ;; suppress diagnostic output
	 "-p"
	 (and rev
	      (concat "-r"
		      (vc-sccs-lookup-triple file rev)))
	 (vc-switches 'SCCS 'checkout)))

(defun vc-sccs-checkout (file &optional rev)
  "Retrieve a copy of a saved revision of SCCS controlled FILE.
If FILE is a directory, all version-controlled files beneath are checked out.
EDITABLE non-nil means that the file should be writable and
locked.  REV is the revision to check out."
  (if (file-directory-p file)
      (mapc 'vc-sccs-checkout (vc-expand-dirs (list file) 'SCCS))
    (let ((file-buffer (get-file-buffer file))
	  switches)
      (message "Checking out %s..." file)
      (save-excursion
	;; Change buffers to get local value of vc-checkout-switches.
	(if file-buffer (set-buffer file-buffer))
	(setq switches (vc-switches 'SCCS 'checkout))
	;; Save this buffer's default-directory
	;; and use save-excursion to make sure it is restored
	;; in the same buffer it was saved in.
	(let ((default-directory default-directory))
	  (save-excursion
	    ;; Adjust the default-directory so that the check-out creates
	    ;; the file in the right place.
	    (setq default-directory (file-name-directory file))

	    (and rev (or (string= rev "")
			 (not (stringp rev)))
		 (setq rev nil))
	    (apply 'vc-sccs-do-command nil 0 "get" (vc-master-name file)
		   "-e"
		   (and rev (concat "-r" (vc-sccs-lookup-triple file rev)))
		   switches))))
      (message "Checking out %s...done" file))))

(defun vc-sccs-revert (file &optional _contents-done)
  "Revert FILE to the version it was based on. If FILE is a directory,
revert all subfiles."
  (if (file-directory-p file)
      (mapc 'vc-sccs-revert (vc-expand-dirs (list file) 'SCCS))
    (vc-sccs-do-command nil 0 "unget" (vc-master-name file))
    (vc-sccs-do-command nil 0 "get" (vc-master-name file))
    ;; Checking out explicit revisions is not supported under SCCS, yet.
    ;; We always "revert" to the latest revision; therefore
    ;; vc-working-revision is cleared here so that it gets recomputed.
    (vc-file-setprop file 'vc-working-revision nil)))

(defun vc-sccs-steal-lock (file &optional rev)
  "Steal the lock on the current workfile for FILE and revision REV."
  (if (file-directory-p file)
      (mapc 'vc-sccs-steal-lock (vc-expand-dirs (list file) 'SCCS))
    (vc-sccs-do-command nil 0 "unget"
			(vc-master-name file) "-n" (if rev (concat "-r" rev)))
    (vc-sccs-do-command nil 0 "get"
			(vc-master-name file) "-g" (if rev (concat "-r" rev)))))

(defun vc-sccs-modify-change-comment (files rev comment)
  "Modify (actually, append to) the change comments for FILES on a specified REV."
  (dolist (file (vc-expand-dirs files 'SCCS))
    (vc-sccs-do-command nil 0 "cdc" (vc-master-name file)
                        (concat "-y" comment) (concat "-r" rev))))


;;;
;;; History functions
;;;

(defun vc-sccs-print-log (files buffer &optional _shortlog _start-revision-ignored limit)
  "Print commit log associated with FILES into specified BUFFER.
Remaining arguments are ignored."
  (setq files (vc-expand-dirs files 'SCCS))
  (vc-sccs-do-command buffer 0 "prs" (mapcar 'vc-master-name files))
  (when limit 'limit-unsupported))

(autoload 'vc-setup-buffer "vc-dispatcher")
(autoload 'vc-delistify "vc-dispatcher")

(defvar w32-quote-process-args)

;; FIXME use sccsdiff if present?
(defun vc-sccs-diff (files &optional oldvers newvers buffer _async)
  "Get a difference report using SCCS between two filesets."
  (setq files (vc-expand-dirs files 'SCCS))
  (setq oldvers (vc-sccs-lookup-triple (car files) oldvers))
  (setq newvers (vc-sccs-lookup-triple (car files) newvers))
  (or buffer (setq buffer "*vc-diff*"))
  ;; We have to reimplement pieces of vc-do-command, because
  ;; we want to run multiple external commands, and only do the setup
  ;; and exit pieces once.
  (save-current-buffer
    (unless (or (eq buffer t)
		(and (stringp buffer) (string= (buffer-name) buffer))
		(eq buffer (current-buffer)))
      (vc-setup-buffer buffer))
    (let* ((fake-flags (append (vc-switches 'SCCS 'diff)
			       (if oldvers (list (concat " -r" oldvers)))
			       (if newvers (list (concat " -r" newvers)))))
	   (fake-command
	    (format "diff%s %s"
		    (if fake-flags
			(concat " " (mapconcat 'identity fake-flags " "))
		      "")
		    (vc-delistify files)))
	   (status 0)
	   (oldproc (get-buffer-process (current-buffer))))
    (when vc-command-messages
      (message "Running %s in foreground..." fake-command))
    (if oldproc (delete-process oldproc))
    (dolist (file files)
      (let ((oldfile (make-temp-file "vc-sccs"))
	    newfile)
	(unwind-protect
	    (progn
	      (vc-sccs-write-revision file oldfile oldvers)
	      (if newvers
		  (vc-sccs-write-revision file (setq newfile
						     (make-temp-file "vc-sccs"))
					  newvers))
	      (let* ((inhibit-read-only t)
		     (buffer-undo-list t)
		     (process-environment
		      (cons "LC_MESSAGES=C" process-environment))
		     (w32-quote-process-args t)
		     (this-status
		      (apply 'process-file "diff" nil t nil
			     (append (vc-switches 'SCCS 'diff)
				     (list oldfile
					   (or newfile
					       (file-relative-name file)))))))
		(or (integerp this-status) (setq status 'error))
		(and (integerp status)
		     (> this-status status)
		     (setq status this-status))))
	  (delete-file oldfile)
	  (if newfile (delete-file newfile)))))
    (when (or (not (integerp status)) (> status 1))
      (unless (eq ?\s (aref (buffer-name (current-buffer)) 0))
	(pop-to-buffer (current-buffer))
	(goto-char (point-min))
	(shrink-window-if-larger-than-buffer))
      (error "Running %s...FAILED (%s)" fake-command
	     (if (integerp status) (format "status %d" status) status)))
    (when vc-command-messages
      (message "Running %s...OK = %d" fake-command status))
    ;; Should we pretend we ran sccsdiff instead?
    ;; This might not actually be a valid diff command.
    (run-hook-with-args 'vc-post-command-functions "diff" files fake-flags)
    status)))


;;;
;;; Tag system.  SCCS doesn't have tags, so we simulate them by maintaining
;;; our own set of name-to-revision mappings.
;;;

(autoload 'vc-tag-precondition "vc")
(declare-function vc-file-tree-walk "vc" (dirname func &rest args))

(defun vc-sccs-create-tag (dir name branchp)
  (when branchp
    (error "SCCS backend does not support module branches"))
  (let ((result (vc-tag-precondition dir)))
    (if (stringp result)
	(error "File %s is not up-to-date" result)
      (vc-file-tree-walk
       dir
       (lambda (f)
	 (vc-sccs-add-triple name f (vc-working-revision f)))))))


;;;
;;; Miscellaneous
;;;

(defun vc-sccs-previous-revision (file rev)
  (vc-call-backend 'RCS 'previous-revision file rev))

(defun vc-sccs-next-revision (file rev)
  (vc-call-backend 'RCS 'next-revision file rev))

(defun vc-sccs-check-headers ()
  "Check if the current file has any headers in it."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward  "%[A-Z]%" nil t)))

(autoload 'vc-rename-master "vc-filewise")

(defun vc-sccs-rename-file (old new)
  ;; Move the master file (using vc-rcs-master-templates).
  (vc-rename-master (vc-master-name old) new vc-sccs-master-templates)
  ;; Update the tag file.
  (with-current-buffer
      (find-file-noselect
       (expand-file-name vc-sccs-name-assoc-file
			 (file-name-directory (vc-master-name old))))
    (goto-char (point-min))
    ;; (replace-regexp (concat ":" (regexp-quote old) "$") (concat ":" new))
    (while (re-search-forward (concat ":" (regexp-quote old) "$") nil t)
      (replace-match (concat ":" new) nil nil))
    (basic-save-buffer)
    (kill-buffer (current-buffer))))

(defun vc-sccs-find-file-hook ()
  ;; If the file is locked by some other user, make
  ;; the buffer read-only.  Like this, even root
  ;; cannot modify a file that someone else has locked.
  (and (stringp (vc-state buffer-file-name 'SCCS))
       (setq buffer-read-only t)))


;;;
;;; Internal functions
;;;

;; This function is wrapped with `progn' so that the autoload cookie
;; copies the whole function itself into loaddefs.el rather than just placing
;; a (autoload 'vc-sccs-search-project-dir "vc-sccs") which would not
;; help us avoid loading vc-sccs.
;;;###autoload
(progn (defun vc-sccs-search-project-dir (_dirname basename)
  "Return the name of a master file in the SCCS project directory.
Does not check whether the file exists but returns nil if it does not
find any project directory."
  (let ((project-dir (getenv "PROJECTDIR")) dirs dir)
    (when project-dir
      (if (file-name-absolute-p project-dir)
	  (setq dirs '("SCCS" ""))
	(setq dirs '("src/SCCS" "src" "source/SCCS" "source"))
	(setq project-dir (expand-file-name (concat "~" project-dir))))
      (while (and (not dir) dirs)
	(setq dir (expand-file-name (car dirs) project-dir))
	(unless (file-directory-p dir)
	  (setq dir nil)
	  (setq dirs (cdr dirs))))
      (and dir (expand-file-name (concat "s." basename) dir))))))

(defun vc-sccs-lock-file (file)
  "Generate lock file name corresponding to FILE."
  (let ((master (vc-master-name file)))
    (and
     master
     (string-match "\\(.*/\\)\\(s\\.\\)\\(.*\\)" master)
     (replace-match "p." t t master 2))))

(defun vc-sccs-parse-locks ()
  "Parse SCCS locks in current buffer.
The result is a list of the form ((REVISION . USER) (REVISION . USER) ...)."
  (let (master-locks)
    (goto-char (point-min))
    (while (re-search-forward "^\\([0-9.]+\\) [0-9.]+ \\([^ ]+\\) .*\n?"
			      nil t)
      (setq master-locks
	    (cons (cons (match-string 1) (match-string 2)) master-locks)))
    ;; FIXME: is it really necessary to reverse ?
    (nreverse master-locks)))

(defun vc-sccs-add-triple (name file rev)
  (with-current-buffer
      (find-file-noselect
       (expand-file-name vc-sccs-name-assoc-file
			 (file-name-directory (vc-master-name file))))
    (goto-char (point-max))
    (insert name "\t:\t" file "\t" rev "\n")
    (basic-save-buffer)
    (kill-buffer (current-buffer))))

(defun vc-sccs-lookup-triple (file name)
  "Return the numeric revision corresponding to a named tag of FILE.
If NAME is nil or a revision number string it's just passed through."
  (if (or (null name)
	  (let ((firstchar (aref name 0)))
	    (and (>= firstchar ?0) (<= firstchar ?9))))
      name
    (with-temp-buffer
      (vc-insert-file
       (expand-file-name vc-sccs-name-assoc-file
			 (file-name-directory (vc-master-name file))))
      (vc-parse-buffer (concat name "\t:\t" file "\t\\(.+\\)") 1))))

(provide 'vc-sccs)

;;; vc-sccs.el ends here
