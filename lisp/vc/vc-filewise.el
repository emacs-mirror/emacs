;;; vc-filewise.el --- common functions for file-oriented back ends.

;; Copyright (C) 1992-1996, 1998-2020 Free Software Foundation, Inc.

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

;; Common functions for file-oriented back ends - SCCS, RCS, SRC, CVS
;;
;; The main purpose of this file is so none of this code has to live
;; in the always-resident vc-hooks.  A secondary purpose is to remove
;; code specific to this class of back ends from vc.el.

;;; Code:

(eval-when-compile (require 'vc))

(defun vc-master-name (file)
  "Return the master name of FILE.
If the file is not registered, or the master name is not known, return nil."
  (or (vc-file-getprop file 'vc-name)
      ;; force computation of the property by calling
      ;; vc-BACKEND-registered explicitly
      (let ((backend (vc-backend file)))
	(if (and backend
		 (vc-filewise-registered backend file))
	    (vc-file-getprop file 'vc-name)))))

(defun vc-rename-master (oldmaster newfile templates)
  "Rename OLDMASTER to be the master file for NEWFILE based on TEMPLATES."
  (let* ((dir (file-name-directory (expand-file-name oldmaster)))
	 (newdir (or (file-name-directory newfile) ""))
	 (newbase (file-name-nondirectory newfile))
	 (masters
	  ;; List of potential master files for `newfile'
	  (mapcar
	   (lambda (s) (vc-possible-master s newdir newbase))
	   templates)))
    (when (or (file-symlink-p oldmaster)
	      (file-symlink-p (file-name-directory oldmaster)))
      (error "This is unsafe in the presence of symbolic links"))
    (rename-file
     oldmaster
     (catch 'found
       ;; If possible, keep the master file in the same directory.
       (dolist (f masters)
	 (when (and f (string= (file-name-directory (expand-file-name f)) dir))
	   (throw 'found f)))
       ;; If not, just use the first possible place.
       (dolist (f masters)
	 (and f (or (not (setq dir (file-name-directory f)))
		    (file-directory-p dir))
	      (throw 'found f)))
       (error "New file lacks a version control directory")))))

(defun vc-filewise-registered (backend file)
  "Check if FILE is registered in BACKEND using vc-BACKEND-master-templates."
  (let ((sym (vc-make-backend-sym backend 'master-templates)))
    (unless (get backend 'vc-templates-grabbed)
      (put backend 'vc-templates-grabbed t))
    (let ((result (vc-check-master-templates file (symbol-value sym))))
      (if (stringp result)
	  (vc-file-setprop file 'vc-name result)
	nil))))				; Not registered

(provide 'vc-filewise)
