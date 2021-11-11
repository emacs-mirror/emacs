;;; org-attach-git.el --- Automatic git commit extension to org-attach -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2021 Free Software Foundation, Inc.

;; Original Author: John Wiegley <johnw@newartisans.com>
;; Restructurer: Gustav Wikström <gustav@whil.se>
;; Keywords: org data git

;; This file is part of GNU Emacs.
;;
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

;; An extension to org-attach.  If `org-attach-id-dir' is initialized
;; as a Git repository, then `org-attach-git' will automatically commit
;; changes when it sees them.  Requires git-annex.

;;; Code:

(require 'org-attach)
(require 'vc-git)

(defcustom org-attach-git-annex-cutoff (* 32 1024)
  "If non-nil, files larger than this will be annexed instead of stored."
  :group 'org-attach
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (const :tag "None" nil)
	  (integer :tag "Bytes")))

(defcustom org-attach-git-annex-auto-get 'ask
  "Confirmation preference for automatically getting annex files.
If \\='ask, prompt using `y-or-n-p'.  If t, always get.  If nil, never get."
  :group 'org-attach
  :package-version '(Org . "9.0")
  :version "26.1"
  :type '(choice
	  (const :tag "confirm with `y-or-n-p'" ask)
	  (const :tag "always get from annex if necessary" t)
	  (const :tag "never get from annex" nil)))

(defcustom org-attach-git-dir 'default
  "Attachment directory with the Git repository to use.
The default value is to use `org-attach-id-dir'.  When set to
`individual-repository', then the directory attached to the
current node, if correctly initialized as a Git repository, will
be used instead."
  :group 'org-attach
  :package-version '(Org . "9.5")
  :type '(choice
          (const :tag "Default" default)
          (const :tag "Individual repository" individual-repository)))

(defun org-attach-git-use-annex ()
  "Return non-nil if git annex can be used."
  (let ((git-dir (vc-git-root
                  (cond ((eq org-attach-git-dir 'default)
                         (expand-file-name org-attach-id-dir))
                        ((eq org-attach-git-dir 'individual-repository)
                         (org-attach-dir))))))
    (and org-attach-git-annex-cutoff
         (or (file-exists-p (expand-file-name "annex" git-dir))
             (file-exists-p (expand-file-name ".git/annex" git-dir))))))

(defun org-attach-git-annex-get-maybe (path)
  "Call git annex get PATH (via shell) if using git annex.
Signals an error if the file content is not available and it was not retrieved."
  (let* ((default-directory
           (cond ((eq org-attach-git-dir 'default)
                  (expand-file-name org-attach-id-dir))
                 ((eq org-attach-git-dir 'individual-repository)
                  (org-attach-dir))))
	 (path-relative (file-relative-name path)))
    (when (and (org-attach-git-use-annex)
	       (not
		(string-equal
		 "found"
		 (shell-command-to-string
		  (format "git annex find --format=found --in=here %s"
			  (shell-quote-argument path-relative))))))
      (let ((should-get
	     (if (eq org-attach-git-annex-auto-get 'ask)
		 (y-or-n-p (format "Run git annex get %s? " path-relative))
	       org-attach-git-annex-auto-get)))
	(unless should-get
	  (error "File %s stored in git annex but unavailable" path))
	(message "Running git annex get \"%s\"." path-relative)
	(call-process "git" nil nil nil "annex" "get" path-relative)))))

(defun org-attach-git-commit (&optional _)
  "Commit changes to git if `org-attach-id-dir' is properly initialized.
This checks for the existence of a \".git\" directory in that directory.

Takes an unused optional argument for the sake of being compatible
with hook `org-attach-after-change-hook'."
  (let* ((dir (cond ((eq org-attach-git-dir 'default)
                     (expand-file-name org-attach-id-dir))
                    ((eq org-attach-git-dir 'individual-repository)
                     (org-attach-dir))))
	 (git-dir (vc-git-root dir))
	 (use-annex (org-attach-git-use-annex))
	 (changes 0))
    (when (and git-dir (executable-find "git"))
      (with-temp-buffer
	(cd dir)
        (dolist (new-or-modified
                 (split-string
                  (shell-command-to-string
                   "git ls-files -zmo --exclude-standard") "\0" t))
          (if (and use-annex
                   (>= (file-attribute-size (file-attributes new-or-modified))
                       org-attach-git-annex-cutoff))
              (call-process "git" nil nil nil "annex" "add" new-or-modified)
            (call-process "git" nil nil nil "add" new-or-modified))
	  (cl-incf changes))
	(dolist (deleted
		 (split-string
		  (shell-command-to-string "git ls-files -z --deleted") "\0" t))
	  (call-process "git" nil nil nil "rm" deleted)
	  (cl-incf changes))
	(when (> changes 0)
	  (shell-command "git commit -m 'Synchronized attachments'"))))))

(add-hook 'org-attach-after-change-hook 'org-attach-git-commit)
(add-hook 'org-attach-open-hook 'org-attach-git-annex-get-maybe)

(provide 'org-attach-git)

;;; org-attach-git.el ends here
