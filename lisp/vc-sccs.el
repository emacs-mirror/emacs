;;; vc-sccs.el --- support for SCCS version-control

;; Copyright (C) 1992,93,94,95,96,97,98,99,2000,2001 Free Software Foundation, Inc.

;; Author:     FSF (see vc.el for full credits)
;; Maintainer: Andre Spiegel <spiegel@gnu.org>

;; $Id: vc-sccs.el,v 1.14 2002/02/25 22:05:16 spiegel Exp $

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

;;; Code:

(eval-when-compile 
  (require 'vc))

;;; 
;;; Customization options
;;;

(defcustom vc-sccs-register-switches nil
  "*Extra switches for registering a file in SCCS.
A string or list of strings passed to the checkin program by
\\[vc-sccs-register]."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string))
  :version "21.1"
  :group 'vc)

(defcustom vc-sccs-diff-switches nil
  "*A string or list of strings specifying extra switches for `vcdiff',
the diff utility used for SCCS under VC."
    :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string))
  :version "21.1"
  :group 'vc)

(defcustom vc-sccs-header (or (cdr (assoc 'SCCS vc-header-alist)) '("%W%"))
  "*Header keywords to be inserted by `vc-insert-headers'."
  :type '(repeat string)
  :group 'vc)

;;;###autoload
(defcustom vc-sccs-master-templates
  '("%sSCCS/s.%s" "%ss.%s" vc-sccs-search-project-dir)
  "*Where to look for SCCS master files.
For a description of possible values, see `vc-check-master-templates'."
  :type '(choice (const :tag "Use standard SCCS file names"
			("%sSCCS/s.%s" "%ss.%s" vc-sccs-search-project-dir))
		 (repeat :tag "User-specified"
			 (choice string
				 function)))
  :version "21.1"
  :group 'vc)


;;;
;;; Internal variables
;;;

(defconst vc-sccs-name-assoc-file "VC-names")


;;;
;;; State-querying functions
;;;

;;;###autoload
(progn (defun vc-sccs-registered (f) (vc-default-registered 'SCCS f)))

(defun vc-sccs-state (file)
  "SCCS-specific function to compute the version control state."
  (with-temp-buffer
    (if (vc-insert-file (vc-sccs-lock-file file))
        (let* ((locks (vc-sccs-parse-locks))
               (workfile-version (vc-workfile-version file))
               (locking-user (cdr (assoc workfile-version locks))))
          (if (not locking-user)
              (if (vc-workfile-unchanged-p file)
                  'up-to-date
                'unlocked-changes)
            (if (string= locking-user (vc-user-login-name))
                'edited
              locking-user)))
      'up-to-date)))

(defun vc-sccs-state-heuristic (file)
  "SCCS-specific state heuristic."
  (if (not (vc-mistrust-permissions file))
      ;;   This implementation assumes that any file which is under version
      ;; control and has -rw-r--r-- is locked by its owner.  This is true
      ;; for both RCS and SCCS, which keep unlocked files at -r--r--r--.
      ;; We have to be careful not to exclude files with execute bits on;
      ;; scripts can be under version control too.  Also, we must ignore the
      ;; group-read and other-read bits, since paranoid users turn them off.
      (let* ((attributes (file-attributes file))
             (owner-uid  (nth 2 attributes))
             (permissions (nth 8 attributes)))
	(if (string-match ".r-..-..-." permissions)
            'up-to-date
          (if (string-match ".rw..-..-." permissions)
              (if (file-ownership-preserved-p file)
                  'edited
                (vc-user-login-name owner-uid))
          ;; Strange permissions.
          ;; Fall through to real state computation.
          (vc-sccs-state file)))
    (vc-sccs-state file))))

(defun vc-sccs-workfile-version (file)
  "SCCS-specific version of `vc-workfile-version'."
  (with-temp-buffer
    (vc-insert-file (vc-name file) "^\001e")
    (vc-parse-buffer "^\001d D \\([^ ]+\\)" 1)))

(defun vc-sccs-checkout-model (file)
  "SCCS-specific version of `vc-checkout-model'."
  'locking)

(defun vc-sccs-workfile-unchanged-p (file)
  "SCCS-specific implementation of vc-workfile-unchanged-p."
  (zerop (apply 'vc-do-command nil 1 "vcdiff" (vc-name file)
                (list "--brief" "-q"
                      (concat "-r" (vc-workfile-version file))))))


;;;
;;; State-changing functions
;;;

(defun vc-sccs-register (file &optional rev comment)
  "Register FILE into the SCCS version-control system.
REV is the optional revision number for the file.  COMMENT can be used
to provide an initial description of FILE.

`vc-register-switches' and `vc-sccs-register-switches' are passed to
the SCCS command (in that order).

Automatically retrieve a read-only version of the file with keywords
expanded if `vc-keep-workfiles' is non-nil, otherwise, delete the workfile."
    (let* ((switches (append
		     (if (stringp vc-register-switches)
			 (list vc-register-switches)
		       vc-register-switches)
		     (if (stringp vc-sccs-register-switches)
			 (list vc-sccs-register-switches)
		       vc-sccs-register-switches)))
	   (dirname (or (file-name-directory file) ""))
	   (basename (file-name-nondirectory file))
	   (project-file (vc-sccs-search-project-dir dirname basename)))
      (let ((vc-name
	     (or project-file
		 (format (car vc-sccs-master-templates) dirname basename)))|)
	(apply 'vc-do-command nil 0 "admin" vc-name
	       (and rev (concat "-r" rev))
	       "-fb"
	       (concat "-i" (file-relative-name file))
	       (and comment (concat "-y" comment))
	       switches))
      (delete-file file)
      (if vc-keep-workfiles
	  (vc-do-command nil 0 "get" (vc-name file)))))

(defun vc-sccs-responsible-p (file)
  "Return non-nil if SCCS thinks it would be responsible for registering FILE."
  ;; TODO: check for all the patterns in vc-sccs-master-templates
  (or (file-directory-p (expand-file-name "SCCS" (file-name-directory file)))
      (stringp (vc-sccs-search-project-dir (or (file-name-directory file) "")
					   (file-name-nondirectory file)))))

(defun vc-sccs-checkin (file rev comment)
  "SCCS-specific version of `vc-backend-checkin'."
  (let ((switches (if (stringp vc-checkin-switches)
		      (list vc-checkin-switches)
		    vc-checkin-switches)))
    (apply 'vc-do-command nil 0 "delta" (vc-name file)
	   (if rev (concat "-r" rev))
	   (concat "-y" comment)
	   switches)
    (if vc-keep-workfiles
	(vc-do-command nil 0 "get" (vc-name file)))))

(defun vc-sccs-checkout (file &optional editable rev workfile)
  "Retrieve a copy of a saved version of SCCS controlled FILE into a WORKFILE.
EDITABLE non-nil means that the file should be writable and
locked.  REV is the revision to check out into WORKFILE."
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

	  (and rev (string= rev "") (setq rev nil))
	  (if workfile
	      ;; Some SCCS implementations allow checking out directly to a
	      ;; file using the -G option, but then some don't so use the
	      ;; least common denominator approach and use the -p option
	      ;; ala RCS.
	      (let ((vc-modes (logior (file-modes (vc-name file))
				      (if editable 128 0)))
		    (failed t))
		(unwind-protect
		    (progn
                      (let ((coding-system-for-read 'no-conversion)
                            (coding-system-for-write 'no-conversion))
                        (with-temp-file filename
                          (apply 'vc-do-command
                                 (current-buffer) 0 "get" (vc-name file)
                                 "-s" ;; suppress diagnostic output
                                 (if editable "-e")
                                 "-p"
                                 (and rev
                                      (concat "-r"
                                              (vc-sccs-lookup-triple file rev)))
                                 switches)))
                      (set-file-modes filename
                                      (logior (file-modes (vc-name file))
                                              (if editable 128 0)))
		      (setq failed nil))
		  (and failed (file-exists-p filename)
		       (delete-file filename))))
	    (apply 'vc-do-command nil 0 "get" (vc-name file)
		   (if editable "-e")
		   (and rev (concat "-r" (vc-sccs-lookup-triple file rev)))
		   switches)))))
    (message "Checking out %s...done" filename)))

(defun vc-sccs-revert (file &optional contents-done)
  "Revert FILE to the version it was based on."
  (vc-do-command nil 0 "unget" (vc-name file))
  (vc-do-command nil 0 "get" (vc-name file))
  ;; Checking out explicit versions is not supported under SCCS, yet.
  ;; We always "revert" to the latest version; therefore
  ;; vc-workfile-version is cleared here so that it gets recomputed.
  (vc-file-setprop file 'vc-workfile-version nil))

(defun vc-sccs-cancel-version (file editable)
  "Undo the most recent checkin of FILE.
EDITABLE non-nil means previous version should be locked."
  (vc-do-command nil 0 "rmdel"
		 (vc-name file)
		 (concat "-r" (vc-workfile-version file)))
  (vc-do-command nil 0 "get"
		 (vc-name file)
		 (if editable "-e")))

(defun vc-sccs-steal-lock (file &optional rev)
  "Steal the lock on the current workfile for FILE and revision REV."
  (vc-do-command nil 0 "unget" (vc-name file) "-n" (if rev (concat "-r" rev)))
  (vc-do-command nil 0 "get" (vc-name file) "-g" (if rev (concat "-r" rev))))


;;;
;;; History functions
;;;

(defun vc-sccs-print-log (file)
  "Get change log associated with FILE."
  (vc-do-command nil 0 "prs" (vc-name file)))

(defun vc-sccs-logentry-check ()
  "Check that the log entry in the current buffer is acceptable for SCCS."
  (when (>= (buffer-size) 512)
    (goto-char 512)
    (error "Log must be less than 512 characters; point is now at pos 512")))

(defun vc-sccs-diff (file &optional oldvers newvers)
  "Get a difference report using SCCS between two versions of FILE."
  (setq oldvers (vc-sccs-lookup-triple file oldvers))
  (setq newvers (vc-sccs-lookup-triple file newvers))
  (apply 'vc-do-command "*vc-diff*" 1 "vcdiff" (vc-name file) 
         (append (list "-q"
                       (and oldvers (concat "-r" oldvers))
                       (and newvers (concat "-r" newvers)))
                 (vc-diff-switches-list 'SCCS))))


;;;
;;; Snapshot system
;;;

(defun vc-sccs-assign-name (file name)
  "Assign to FILE's latest version a given NAME."
  (vc-sccs-add-triple name file (vc-workfile-version file)))


;;;
;;; Miscellaneous
;;;

(defun vc-sccs-check-headers ()
  "Check if the current file has any headers in it."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward  "%[A-Z]%" nil t)))

(defun vc-sccs-rename-file (old new)
  ;; Move the master file (using vc-rcs-master-templates).
  (vc-rename-master (vc-name old) new vc-sccs-master-templates)
  ;; Update the snapshot file.
  (with-current-buffer
      (find-file-noselect
       (expand-file-name vc-sccs-name-assoc-file
			 (file-name-directory (vc-name old))))
    (goto-char (point-min))
    ;; (replace-regexp (concat ":" (regexp-quote old) "$") (concat ":" new))
    (while (re-search-forward (concat ":" (regexp-quote old) "$") nil t)
      (replace-match (concat ":" new) nil nil))
    (basic-save-buffer)
    (kill-buffer (current-buffer))))


;;;
;;; Internal functions
;;;

;; This function is wrapped with `progn' so that the autoload cookie
;; copies the whole function itself into loaddefs.el rather than just placing
;; a (autoload 'vc-sccs-search-project-dir "vc-sccs") which would not
;; help us avoid loading vc-sccs.
;;;###autoload
(progn (defun vc-sccs-search-project-dir (dirname basename)
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
  (let ((master (vc-name file)))
    (and
     master
     (string-match "\\(.*/\\)\\(s\\.\\)\\(.*\\)" master)
     (replace-match "p." t t master 2))))

(defun vc-sccs-parse-locks ()
  "Parse SCCS locks in current buffer.
The result is a list of the form ((VERSION . USER) (VERSION . USER) ...)."
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
			 (file-name-directory (vc-name file))))
    (goto-char (point-max))
    (insert name "\t:\t" file "\t" rev "\n")
    (basic-save-buffer)
    (kill-buffer (current-buffer))))

(defun vc-sccs-lookup-triple (file name)
  "Return the numeric version corresponding to a named snapshot of FILE.
If NAME is nil or a version number string it's just passed through."
  (if (or (null name)
	  (let ((firstchar (aref name 0)))
	    (and (>= firstchar ?0) (<= firstchar ?9))))
      name
    (with-temp-buffer
      (vc-insert-file
       (expand-file-name vc-sccs-name-assoc-file
			 (file-name-directory (vc-name file))))
      (vc-parse-buffer (concat name "\t:\t" file "\t\\(.+\\)") 1))))

(provide 'vc-sccs)

;;; vc-sccs.el ends here
