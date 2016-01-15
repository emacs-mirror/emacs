;;; version.el --- record version number of Emacs

;; Copyright (C) 1985, 1992, 1994-1995, 1999-2016 Free Software
;; Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: internal
;; Package: emacs

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defconst emacs-major-version
  (progn (string-match "^[0-9]+" emacs-version)
         (string-to-number (match-string 0 emacs-version)))
  "Major version number of this version of Emacs.
This variable first existed in version 19.23.")

(defconst emacs-minor-version
  (progn (string-match "^[0-9]+\\.\\([0-9]+\\)" emacs-version)
         (string-to-number (match-string 1 emacs-version)))
  "Minor version number of this version of Emacs.
This variable first existed in version 19.23.")

(defconst emacs-build-time (current-time)
  "Time at which Emacs was dumped out.")

;; I think this should be obsoleted/removed.  It's just one more meaningless
;; difference between different builds.  It's usually not even an fqdn.
(defconst emacs-build-system (system-name)
  "Name of the system on which Emacs was built.")

(defvar motif-version-string)
(defvar gtk-version-string)
(defvar ns-version-string)
(defvar cairo-version-string)

(defun emacs-version (&optional here)
  "Return string describing the version of Emacs that is running.
If optional argument HERE is non-nil, insert string at point.
Don't use this function in programs to choose actions according
to the system configuration; look at `system-configuration' instead."
  (interactive "P")
  (let ((version-string
         (format (if (not (called-interactively-p 'interactive))
		     "GNU Emacs %s (%s%s%s%s)\n of %s"
		   "GNU Emacs %s (%s%s%s%s) of %s")
                 emacs-version
		 system-configuration
		 (cond ((featurep 'motif)
			(concat ", " (substring motif-version-string 4)))
		       ((featurep 'gtk)
			(concat ", GTK+ Version " gtk-version-string))
		       ((featurep 'x-toolkit) ", X toolkit")
		       ((featurep 'ns)
			(format ", NS %s" ns-version-string))
		       (t ""))
		 (if (featurep 'cairo)
		     (format ", cairo version %s" cairo-version-string)
		   "")
		 (if (and (boundp 'x-toolkit-scroll-bars)
			  (memq x-toolkit-scroll-bars '(xaw xaw3d)))
		     (format ", %s scroll bars"
			     (capitalize (symbol-name x-toolkit-scroll-bars)))
		   "")
		 (format-time-string "%Y-%m-%d" emacs-build-time))))
    (if here
        (insert version-string)
      (if (called-interactively-p 'interactive)
          (message "%s" version-string)
        version-string))))

;; We hope that this alias is easier for people to find.
(defalias 'version 'emacs-version)

;; Set during dumping, this is a defvar so that it can be setq'd.
(defvar emacs-repository-version nil
  "String giving the repository revision from which this Emacs was built.
Value is nil if Emacs was not built from a repository checkout,
or if we could not determine the revision.")

(define-obsolete-variable-alias 'emacs-bzr-version
                                'emacs-repository-version "24.4")

(define-obsolete-function-alias 'emacs-bzr-get-version
                                'emacs-repository-get-version "24.4")

(defun emacs-repository-version-git (dir)
  "Ask git itself for the version information for directory DIR."
  (message "Waiting for git...")
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory dir)))
      (and (eq 0
	       (with-demoted-errors "Error running git rev-parse: %S"
		 (call-process "git" nil '(t nil) nil "rev-parse" "HEAD")))
	   (progn (goto-char (point-min))
		  (looking-at "[0-9a-fA-F]\\{40\\}"))
	   (match-string 0)))))

(defun emacs-repository--version-git-1 (file dir)
  "Internal subroutine of `emacs-repository-get-version'."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (cond ((looking-at "[0-9a-fA-F]\\{40\\}")
             (match-string 0))
            ((looking-at "ref: \\(.*\\)")
             (emacs-repository--version-git-1
              (expand-file-name (match-string 1) dir)
              dir))))))

(defun emacs-repository-get-version (&optional dir external)
  "Try to return as a string the repository revision of the Emacs sources.
The format of the returned string is dependent on the VCS in use.
Value is nil if the sources do not seem to be under version
control, or if we could not determine the revision.  Note that
this reports on the current state of the sources, which may not
correspond to the running Emacs.

Optional argument DIR is a directory to use instead of `source-directory'.
Optional argument EXTERNAL non-nil means to just ask the VCS itself,
if the sources appear to be under version control.  Otherwise only ask
the VCS if we cannot find any information ourselves."
  (or dir (setq dir source-directory))
  (let* ((base-dir (expand-file-name ".git" dir))
         (in-main-worktree (file-directory-p base-dir))
         (in-linked-worktree nil)
         sub-dir)
    ;; If the sources are in a linked worktree, .git is a file that points to
    ;; the location of the main worktree and the repo's administrative files.
    (when (and (not in-main-worktree)
               (file-regular-p base-dir)
               (file-readable-p base-dir))
      (with-temp-buffer
        (insert-file-contents base-dir)
        (when (looking-at "gitdir: \\(.*\.git\\)\\(.*\\)$")
          (setq base-dir (match-string 1)
                sub-dir (concat base-dir (match-string 2))
                in-linked-worktree t))))
    ;; We've found a worktree, either main or linked.
    (when (or in-main-worktree in-linked-worktree)
      (if external
          (emacs-repository-version-git dir)
        (or (if in-linked-worktree
                (emacs-repository--version-git-1
                 (expand-file-name "HEAD" sub-dir) base-dir)
              (let ((files '("HEAD" "refs/heads/master"))
                    file rev)
                (while (and (not rev)
                            (setq file (car files)))
                  (setq file (expand-file-name file base-dir)
                        files (cdr files)
                        rev (emacs-repository--version-git-1 file base-dir)))
                rev))
            ;; AFAICS this doesn't work during dumping (bug#20799).
            (emacs-repository-version-git dir))))))

;; We put version info into the executable in the form that `ident' uses.
(purecopy (concat "\n$Id: " (subst-char-in-string ?\n ?\s (emacs-version))
		  " $\n"))

;;; version.el ends here
