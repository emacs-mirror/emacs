;;; paths.el --- define pathnames for use by various Emacs commands

;; Copyright (C) 1986, 1988, 1994, 1999-2012 Free Software Foundation, Inc.

;; Maintainer: FSF
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

;; These are default settings for names of certain files and directories
;; that Emacs needs to refer to from time to time.

;; If these settings are not right, override them with `setq'
;; in site-init.el.  Do not change this file.

;;; Code:

;; This is a defcustom largely so that we can get the benefit
;; of custom-initialize-delay.  Perhaps it would work to make it a
;; defvar and explicitly give it a standard-value property, and
;; call custom-initialize-delay on it.
(defcustom Info-default-directory-list
  (let* ((config-dir
	  (file-name-as-directory
	   ;; Self-contained NS build with info/ in the app-bundle.
	   (or (and (featurep 'ns)
		    (let ((dir (expand-file-name "../info" data-directory)))
		      (if (file-directory-p dir) dir)))
	       configure-info-directory)))
	 (prefixes
	  ;; Directory trees in which to look for info subdirectories
	  (prune-directory-list '("/usr/local/" "/usr/" "/opt/" "/")))
	 (suffixes
	  ;; Subdirectories in each directory tree that may contain info
	  ;; directories.  Most of these are rather outdated.
	  ;; It ought to be fine to stop checking the "emacs" ones now,
	  ;; since this is Emacs and we have not installed info files
	  ;; into such directories for a looong time...
	  '("share/" "" "gnu/" "gnu/lib/" "gnu/lib/emacs/"
	    "emacs/" "lib/" "lib/emacs/"))
	 (standard-info-dirs
	  (apply #'nconc
		 (mapcar (lambda (pfx)
			   (let ((dirs
				  (mapcar (lambda (sfx)
					    (concat pfx sfx "info/"))
					  suffixes)))
			     (prune-directory-list dirs)))
			 prefixes)))
	 ;; If $(prefix)/share/info is not one of the standard info
	 ;; directories, they are probably installing an experimental
	 ;; version of Emacs, so make sure that experimental version's Info
	 ;; files override the ones in standard directories.
	 (dirs
	  (if (member config-dir standard-info-dirs)
	      ;; FIXME?  What is the point of adding it again at the end
	      ;; when it is already present earlier in the list?
	      (nconc standard-info-dirs (list config-dir))
	    (cons config-dir standard-info-dirs))))
    (if (not (eq system-type 'windows-nt))
	dirs
      ;; Include the info directory near where Emacs executable was installed.
      (let* ((instdir (file-name-directory invocation-directory))
	     (dir1 (expand-file-name "../info/" instdir))
	     (dir2 (expand-file-name "../../../info/" instdir)))
	(cond ((file-exists-p dir1) (append dirs (list dir1)))
	      ((file-exists-p dir2) (append dirs (list dir2)))
	      (t dirs)))))

  "Default list of directories to search for Info documentation files.
They are searched in the order they are given in the list.
Therefore, the directory of Info files that come with Emacs
normally should come last (so that local files override standard ones),
unless Emacs is installed into a non-standard directory.  In the latter
case, the directory of Info files that come with Emacs should be
first in this list.

Once Info is started, the list of directories to search
comes from the variable `Info-directory-list'.
This variable `Info-default-directory-list' is used as the default
for initializing `Info-directory-list' when Info is started, unless
the environment variable INFOPATH is set.

Although this is a customizable variable, that is mainly for technical
reasons.  Normally, you should either set INFOPATH or customize
`Info-additional-directory-list', rather than changing this variable."
  :initialize 'custom-initialize-delay
  :type '(repeat directory)
  :group 'info)


;;; paths.el ends here
