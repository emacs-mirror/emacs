;;; version.el --- record version number of Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 1985, 1992, 1994-1995, 1999-2026 Free Software
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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:



;; If either of the files examined by the following two functions does
;; not exist, Emacs was configured `--disable-build-details'.

(defun android-read-build-system ()
  "Obtain the host name of the system on which Emacs was built.
Use the data stored in the special file `/assets/build_info'.
Value is the string ``Unknown'' upon failure, else the hostname
of the build system."
  (when (file-exists-p "/assets/build_info")
    (with-temp-buffer
      (insert-file-contents "/assets/build_info")
      (let ((string (buffer-substring 1 (line-end-position))))
        (and (not (equal string "Unknown")) string)))))

(defun android-read-build-time ()
  "Obtain the time at which Emacs was built.
Use the data stored in the special file `/assets/build_info'.
Value is nil upon failure, else the time in the same format as
returned by `current-time'."
  (when (file-exists-p "/assets/build_info")
    (with-temp-buffer
      (insert-file-contents "/assets/build_info")
      (end-of-line)
      (let ((number (read (current-buffer))))
        (time-convert number 'list)))))



(defconst emacs-major-version
  (progn (string-match "^[0-9]+" emacs-version)
         (string-to-number (match-string 0 emacs-version)))
  "Major version number of this version of Emacs.")

(defconst emacs-minor-version
  (progn (string-match "^[0-9]+\\.\\([0-9]+\\)" emacs-version)
         (string-to-number (match-string 1 emacs-version)))
  "Minor version number of this version of Emacs.")

;; N.B. (featurep 'android) is tested for in addition to
;; `system-type', because that can also be Android on a TTY-only
;; Android build that doesn't employ the window system packaging
;; support.  (bug#65319)
(defconst emacs-build-system (or (and (featurep 'android)
                                      (eq system-type 'android)
                                      (android-read-build-system))
                                 (system-name))
  "Name of the system on which Emacs was built, or nil if not available.")

(defconst emacs-build-time (if emacs-build-system
                               (or (and (featurep 'android)
                                        (eq system-type 'android)
                                        (android-read-build-time))
                                   (current-time)))
  "Time at which Emacs was dumped out, or nil if not available.")

(defconst emacs-build-number 1          ; loadup.el may increment this
  "The build number of this version of Emacs.
This is an integer that increments each time Emacs is built in a given
directory (without cleaning).  This is likely to only be relevant when
developing Emacs.")

(defvar motif-version-string)
(defvar gtk-version-string)
(defvar ns-version-string)
(defvar cairo-version-string)

(declare-function haiku-get-version-string "haikufns.c")

(defun emacs-version (&optional here)
  "Display the version of Emacs that is running in this session.
With a prefix argument, insert the Emacs version string at point
instead of displaying it.
If called from Lisp, by default return the version string; but
if the optional argument HERE is non-nil, insert the string at
point instead.

Don't use this function in programs to choose actions according
to the system configuration; look at `system-configuration' instead."
  (interactive "P")
  (let ((version-string
         (format "GNU Emacs %s (build %s, %s%s%s%s)%s"
                 emacs-version
                 emacs-build-number
		 system-configuration
		 (cond ((featurep 'motif)
			(concat ", " (substring motif-version-string 4)))
		       ((featurep 'gtk)
			(concat ", GTK+ Version " gtk-version-string))
		       ((featurep 'x-toolkit) ", X toolkit")
		       ((featurep 'ns)
			(format ", NS %s" ns-version-string))
                       ((featurep 'haiku)
                        (format ", Haiku %s" (haiku-get-version-string)))
		       (t ""))
		 (if (featurep 'cairo)
		     (format ", cairo version %s" cairo-version-string)
		   "")
		 (if (and (boundp 'x-toolkit-scroll-bars)
			  (memq x-toolkit-scroll-bars '(xaw xaw3d)))
		     (format ", %s scroll bars"
			     (capitalize (symbol-name x-toolkit-scroll-bars)))
		   "")
		 (if emacs-build-time
		     (format-time-string (concat
					  (if (called-interactively-p
					       'interactive)
					      "" "\n")
					  " of %Y-%m-%d")
					 emacs-build-time)
		   ""))))
    (if here
        (insert version-string)
      (if (called-interactively-p 'interactive)
          (message "%s" version-string)
        version-string))))

;; We hope that this alias is easier for people to find.
(defalias 'version 'emacs-version)

(define-obsolete-variable-alias 'emacs-bzr-version
                                'emacs-repository-version "24.4")

;; Set during dumping, this is a defvar so that it can be setq'd.
(defvar emacs-repository-version nil
  "String giving the repository revision from which this Emacs was built.
Value is nil if Emacs was not built from a repository checkout,
or if we could not determine the revision.")

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
		  (looking-at "[[:xdigit:]]\\{40\\}"))
	   (match-string 0)))))

(defun emacs-repository-version-android ()
  "Return the Emacs repository revision Emacs was built from.
Value is nil if Emacs was not built from a repository checkout.
Use information from the `/assets/version' special file."
  (with-temp-buffer
    (insert-file-contents "/assets/version")
    (let ((string (buffer-substring 1 (line-end-position))))
      (and (not (equal string "Unknown")) string))))

(defun emacs-repository-get-version (&optional dir _external)
  "Try to return as a string the repository revision of the Emacs sources.
The format of the returned string is dependent on the VCS in use.

If Emacs is built for Android, use the version information
embedded in the Emacs installation package.

Value is nil if the sources do not seem to be under version
control, or if we could not determine the revision.  Note that
this reports on the current state of the sources, which may not
correspond to the running Emacs.

Optional argument DIR is a directory to use instead of `source-directory'.
Optional argument EXTERNAL is ignored."
  (cond ((and (featurep 'android)
              (eq system-type 'android))
         (emacs-repository-version-android))
        (t (emacs-repository-version-git
            (or dir source-directory)))))

(defvar emacs-repository-branch nil
  "String giving the repository branch from which this Emacs was built.
Value is nil if Emacs was not built from a repository checkout,
or if we could not determine the branch.")

(defun emacs-repository-branch-android ()
  "Return the Emacs repository branch Emacs was built from.
Value is nil if Emacs was not built from a repository checkout.
Use information from the `/assets/version' special file."
  (with-temp-buffer
    (insert-file-contents "/assets/version")
    (end-of-line)
    (forward-char)
    (let ((string (buffer-substring (point) (line-end-position))))
      (and (not (equal string "Unknown")) string))))

(defun emacs-repository-branch-git (dir)
  "Ask git itself for the branch information for directory DIR."
  (message "Waiting for git...")
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory dir)))
      (and (zerop
	    (with-demoted-errors "Error running git rev-parse --abbrev-ref: %S"
	      (call-process "git" nil '(t nil) nil
                            "rev-parse" "--abbrev-ref" "HEAD")))
           (goto-char (point-min))
           (buffer-substring (point) (line-end-position))))))

(defun emacs-repository-get-branch (&optional dir)
  "Try to return as a string the repository branch of the Emacs sources.
The format of the returned string is dependent on the VCS in use.

If Emacs is built for Android, use the version information
embedded in the Emacs installation package.

Value is nil if the sources do not seem to be under version
control, or if we could not determine the branch.  Note that
this reports on the current state of the sources, which may not
correspond to the running Emacs.

Optional argument DIR is a directory to use instead of `source-directory'."
  (cond ((and (featurep 'android)
              (eq system-type 'android))
         (emacs-repository-branch-android))
        (t (emacs-repository-branch-git
            (or dir source-directory)))))

;;; version.el ends here
