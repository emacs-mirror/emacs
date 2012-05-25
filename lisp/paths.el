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

;; Docstrings in this file should, where reasonable, follow the
;; conventions described in make-docfile, so that they get put in the
;; DOC file rather than in memory.

(defvar Info-default-directory-list
  (let* ((config-dir
	  (file-name-as-directory configure-info-directory))
	 (config
	  (list config-dir))
	 (unpruned-prefixes
	  ;; Directory trees that may not exist at installation time, and
	  ;; so shouldn't be pruned based on existence.
	  '("/usr/local/"))
	 (prefixes
	  ;; Directory trees in which to look for info subdirectories
	  (prune-directory-list '("/usr/local/" "/usr/" "/opt/" "/")
				unpruned-prefixes))
	 (suffixes
	  ;; Subdirectories in each directory tree that may contain info
	  ;; directories.
	  '("share/" "" "gnu/" "gnu/lib/" "gnu/lib/emacs/"
	    "emacs/" "lib/" "lib/emacs/"))
	 (standard-info-dirs
	  (apply #'nconc
		 (mapcar (lambda (pfx)
			   (let ((dirs
				  (mapcar (lambda (sfx)
					    (concat pfx sfx "info/"))
					  suffixes)))
			     (if (member pfx unpruned-prefixes)
				 dirs
			       (prune-directory-list dirs config))))
			 prefixes))))
    ;; If $(prefix)/share/info is not one of the standard info
    ;; directories, they are probably installing an experimental
    ;; version of Emacs, so make sure that experimental version's Info
    ;; files override the ones in standard directories.
    (if (member config-dir standard-info-dirs)
	(nconc standard-info-dirs config)
      (cons config-dir standard-info-dirs)))
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
the environment variable INFOPATH is set.")


;;; paths.el ends here
