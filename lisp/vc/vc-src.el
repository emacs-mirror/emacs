;;; vc-src.el --- support for SRC version-control  -*- lexical-binding:t -*-

;; Copyright (C) 1992-2020 Free Software Foundation, Inc.

;; Author: FSF (see vc.el for full credits)
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

;; See vc.el.  SRC requires an underlying RCS version of 4.0 or greater.

;; FUNCTION NAME                               STATUS
;; BACKEND PROPERTIES
;; * revision-granularity                      OK
;; STATE-QUERYING FUNCTIONS
;; * registered (file)                         OK
;; * state (file)                              OK
;; - dir-status-files (dir files uf)           OK
;; - dir-extra-headers (dir)                   NOT NEEDED
;; - dir-printer (fileinfo)                    ??
;; * working-revision (file)                   OK
;; * checkout-model (files)                    OK
;; - mode-line-string (file)                   NOT NEEDED
;; STATE-CHANGING FUNCTIONS
;; * register (files &optional rev comment)    OK
;; * create-repo ()                            OK
;; * responsible-p (file)                      OK
;; - receive-file (file rev)                   NOT NEEDED
;; - unregister (file)                         NOT NEEDED
;; * checkin (files comment)                   OK
;; * find-revision (file rev buffer)           OK
;; * checkout (file &optional rev)             OK
;; * revert (file &optional contents-done)     OK
;; - merge (file rev1 rev2)                    NOT NEEDED
;; - merge-news (file)                         NOT NEEDED
;; - steal-lock (file &optional revision)      NOT NEEDED
;; HISTORY FUNCTIONS
;; * print-log (files buffer &optional shortlog start-revision limit) OK
;; - log-view-mode ()                          ??
;; - show-log-entry (revision)                 NOT NEEDED
;; - comment-history (file)                    NOT NEEDED
;; - update-changelog (files)                  NOT NEEDED
;; * diff (files &optional rev1 rev2 buffer)   OK
;; - revision-completion-table (files)         ??
;; - annotate-command (file buf &optional rev) ??
;; - annotate-time ()                          ??
;; - annotate-current-time ()                  NOT NEEDED
;; - annotate-extract-revision-at-line ()      ??
;; TAG SYSTEM
;; - create-tag (dir name branchp)             ??
;; - retrieve-tag (dir name update)            ??
;; MISCELLANEOUS
;; - make-version-backups-p (file)             ??
;; - previous-revision (file rev)              ??
;; - next-revision (file rev)                  ??
;; - check-headers ()                          ??
;; - delete-file (file)                        ??
;; * rename-file (old new)                     OK
;; - find-file-hook ()                         NOT NEEDED


;;; Code:

;;;
;;; Customization options
;;;

(eval-when-compile
  (require 'cl-lib)
  (require 'vc))

(declare-function vc-setup-buffer "vc-dispatcher" (buf))

(defgroup vc-src nil
  "VC SRC backend."
  :version "25.1"
  :group 'vc)

(defcustom vc-src-release nil
  "The release number of your SRC installation, as a string.
If nil, VC itself computes this value when it is first needed."
  :type '(choice (const :tag "Auto" nil)
		 (string :tag "Specified")
		 (const :tag "Unknown" unknown))
  :group 'vc-src)

(defcustom vc-src-program "src"
  "Name of the SRC executable (excluding any arguments)."
  :type 'string
  :group 'vc-src)

(defcustom vc-src-diff-switches nil
  "String or list of strings specifying switches for SRC diff under VC.
If nil, use the value of `vc-diff-switches'.  If t, use no switches."
  :type '(choice (const :tag "Unspecified" nil)
                 (const :tag "None" t)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List" :value ("") string))
  :group 'vc-src)

;; This needs to be autoloaded because vc-src-registered uses it (via
;; vc-default-registered), and vc-hooks needs to be able to check
;; for a registered backend without loading every backend.
;;;###autoload
(defcustom vc-src-master-templates
  (purecopy '("%s.src/%s,v"))
  "Where to look for SRC master files.
For a description of possible values, see `vc-check-master-templates'."
  :type '(choice (const :tag "Use standard SRC file names"
			'("%s.src/%s,v"))
		 (repeat :tag "User-specified"
			 (choice string
				 function)))
  :group 'vc-src)


;;; Properties of the backend

(defun vc-src-revision-granularity () 'file)
(defun vc-src-checkout-model (_files) 'implicit)

;;;
;;; State-querying functions
;;;

;; The autoload cookie below places vc-src-registered directly into
;; loaddefs.el, so that vc-src.el does not need to be loaded for
;; every file that is visited.
;;;###autoload
(progn
(defun vc-src-registered (f) (vc-default-registered 'src f)))

(defun vc-src-state (file)
  "SRC-specific version of `vc-state'."
  (let*
      ((status nil)
       (default-directory (file-name-directory file))
       (out
	(with-output-to-string
	  (with-current-buffer
	      standard-output
	    (setq status
		  ;; Ignore all errors.
		  (condition-case nil
		      (process-file
		       vc-src-program nil t nil
		       "status" "-a" (file-relative-name file))
		    (error nil)))))))
    (when (eq 0 status)
      (when (null (string-match "does not exist or is unreadable" out))
	(let ((state (aref out 0)))
	  (cond
	   ;; FIXME: What to do about A and L codes?
	   ((eq state ?.) 'up-to-date)
	   ((eq state ?A) 'added)
	   ((eq state ?M) 'edited)
	   ((eq state ?I) 'ignored)
	   ((eq state ?R) 'removed)
	   ((eq state ?!) 'missing)
	   ((eq state ??) 'unregistered)
	   (t 'up-to-date)))))))

(autoload 'vc-expand-dirs "vc")

(defun vc-src-dir-status-files (dir files update-function)
  ;; FIXME: Use one src status -a call for this
  (if (not files) (setq files (vc-expand-dirs (list dir) 'SRC)))
  (let ((result nil))
    (dolist (file files)
      (let ((state (vc-state file))
	    (frel (file-relative-name file)))
	(when (and (eq (vc-backend file) 'SRC)
		   (not (eq state 'up-to-date)))
	  (push (list frel state) result))))
    (funcall update-function result)))

(defun vc-src-command (buffer file-or-list &rest flags)
  "A wrapper around `vc-do-command' for use in vc-src.el.
This function differs from vc-do-command in that it invokes `vc-src-program'."
  (let (file-list)
    (cond ((stringp file-or-list)
	   (setq file-list (list "--" file-or-list)))
	  (file-or-list
	   (setq file-list (cons "--" file-or-list))))
    (apply 'vc-do-command (or buffer "*vc*") 0 vc-src-program file-list flags)))

(defun vc-src-working-revision (file)
  "SRC-specific version of `vc-working-revision'."
  (let ((result (ignore-errors
		  (with-output-to-string
		    (vc-src-command standard-output file "list" "-f{1}" "@")))))
    (if (zerop (length result)) "0" result)))

;;;
;;; State-changing functions
;;;

(defun vc-src-create-repo ()
  "Create a new SRC repository."
  ;; SRC is totally file-oriented, so all we have to do is make the directory.
  (make-directory ".src"))

(autoload 'vc-switches "vc")

(defun vc-src-register (files &optional _comment)
  "Register FILES under src. COMMENT is ignored."
  (vc-src-command nil files "add"))

(defun vc-src-responsible-p (file)
  "Return non-nil if SRC thinks it would be responsible for registering FILE."
  (file-directory-p (expand-file-name ".src"
                                      (if (file-directory-p file)
                                          file
                                        (file-name-directory file)))))

(defun vc-src-checkin (files comment &optional _rev)
  "SRC-specific version of `vc-backend-checkin'.
REV is ignored."
  (vc-src-command nil files "commit" "-m" comment))

(defun vc-src-find-revision (file rev buffer)
  (let ((coding-system-for-read 'binary)
        (coding-system-for-write 'binary))
    (if rev
        (vc-src-command buffer file "cat" rev)
      (vc-src-command buffer file "cat"))))

(defun vc-src-checkout (file &optional rev)
  "Retrieve a revision of FILE.
REV is the revision to check out into WORKFILE."
  (if rev
      (vc-src-command nil file "co" rev)
    (vc-src-command nil file "co")))

(defun vc-src-revert (file &optional _contents-done)
  "Revert FILE to the version it was based on.  If FILE is a directory,
revert all registered files beneath it."
  (if (file-directory-p file)
      (mapc 'vc-src-revert (vc-expand-dirs (list file) 'SRC))
    (vc-src-command nil file "co")))

(defun vc-src-modify-change-comment (files rev comment)
  "Modify the change comments change on FILES on a specified REV.  If FILE is a
directory the operation is applied to all registered files beneath it."
  (dolist (file (vc-expand-dirs files 'SRC))
    (vc-src-command nil file "amend" "-m" comment rev)))

;; History functions

(defcustom vc-src-log-switches nil
  "String or list of strings specifying switches for src log under VC."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Argument String")
                 (repeat :tag "Argument List" :value ("") string))
  :group 'vc-src)

(defun vc-src-print-log (files buffer &optional shortlog _start-revision limit)
  "Print commit log associated with FILES into specified BUFFER.
If SHORTLOG is non-nil, use the list method.
If START-REVISION is non-nil, it is the newest revision to show.
If LIMIT is non-nil, show no more than this many entries."
  ;; FIXME: Implement the range restrictions.
  ;; `vc-do-command' creates the buffer, but we need it before running
  ;; the command.
  (vc-setup-buffer buffer)
  ;; If the buffer exists from a previous invocation it might be
  ;; read-only.
  (let ((inhibit-read-only t))
    (with-current-buffer
	buffer
      (apply 'vc-src-command buffer files (if shortlog "list" "log")
	     (nconc
	      ;;(when start-revision (list (format "%s-1" start-revision)))
	      (when limit (list "-l" (format "%s" limit)))
	      vc-src-log-switches)))))

(defun vc-src-diff (files &optional oldvers newvers buffer _async)
  "Get a difference report using src between two revisions of FILES."
  (let* ((firstfile (car files))
         (working (and firstfile (vc-working-revision firstfile))))
    (when (and (equal oldvers working) (not newvers))
      (setq oldvers nil))
    (when (and (not oldvers) newvers)
      (setq oldvers working))
    (apply #'vc-src-command (or buffer "*vc-diff*") files "diff"
	   (when oldvers
	     (if newvers
		 (list (concat oldvers "-" newvers))
	       (list oldvers))))))

;; Miscellaneous

(defun vc-src-rename-file (old new)
  "Rename file from OLD to NEW using `src mv'."
  (vc-src-command nil 0 new "mv" old))

(provide 'vc-src)

;;; vc-src.el ends here
