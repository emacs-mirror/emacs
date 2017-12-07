;;; ede/detect.el --- EDE project detection and file associations

;; Copyright (C) 2014-2017 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <eric@siege-engine.com>

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
;;
;; Project detection for EDE;
;;
;; Detection comes in multiple forms:
;;
;; `ede-detect-scan-directory-for-project' -
;;        Scan for a project via the file system.
;; `ede-detect-directory-for-project' -
;;        Check our file cache for a project.  If that fails, use
;;        the scan fcn above.

;;; Code:

(require 'ede/auto) ;; Autoload settings.

(when (or (<= emacs-major-version 23)
	  ;; predicate as name added in Emacs 24.2
	  (and (= emacs-major-version 24)
	       (< emacs-minor-version 2)))
  (message "Loading CEDET fallback autoload library.")
  (require 'cedet/dominate
	   (expand-file-name "../../../etc/fallback-libraries/dominate.el"
			     (file-name-directory load-file-name))))


;;; BASIC PROJECT SCAN
;;
(defun ede--detect-stop-scan-p (dir)
  "Return non-nil if we need to stop scanning upward in DIR."
  ;;(let ((stop
  (file-exists-p (expand-file-name ".ede_stop_scan" dir)))
;;)
;;(when stop
;;(message "Stop Scan at %s" dir))
;;stop))

(defvar ede--detect-found-project nil
  "When searching for a project, temporarily save that file.")

(defun ede--detect-ldf-predicate (dir)
  "Non-nil if DIR contain any known EDE project types."
  (if (ede--detect-stop-scan-p dir)
      (throw 'stopscan nil)
    (let ((types ede-project-class-files))
      ;; Loop over all types, loading in the first type that we find.
      (while (and types (not ede--detect-found-project))
	(if (ede-auto-detect-in-dir (car types) dir)
	    (progn
	      ;; We found one!
	      (setq ede--detect-found-project (car types)))
	  (setq types (cdr types)))
	)
      ede--detect-found-project)))

(defun ede--detect-scan-directory-for-project (directory)
  "Detect an EDE project for the current DIRECTORY by scanning.
This function ALWAYS scans files and directories and DOES NOT
use any file caches.
Return a cons cell:
  ( ROOTDIR . PROJECT-AUTOLOAD)"
  (let* ((ede--detect-found-project nil)
	 (root
	  (catch 'stopscan
	    (locate-dominating-file directory
				    'ede--detect-ldf-predicate))))
    (when root
      (cons root ede--detect-found-project))))

;;; Root Only project detect
;;
;; For projects that only have a detectable ROOT file, but may in fact
;; contain a generic file such as a Makefile, we need to do a second scan
;; to make sure we don't miss-match.
(defun ede--detect-ldf-rootonly-predicate (dir)
  "Non-nil if DIR contain any known EDE project types."
  (if (ede--detect-stop-scan-p dir)
      (throw 'stopscan nil)
    (let ((types ede-project-class-files))
      ;; Loop over all types, loading in the first type that we find.
      (while (and types (not ede--detect-found-project))
	(if (and
	     (oref (car types) root-only)
	     (ede-auto-detect-in-dir (car types) dir))
	    (progn
	      ;; We found one!
	      (setq ede--detect-found-project (car types)))
	  (setq types (cdr types)))
	)
      ede--detect-found-project)))

(defun ede--detect-scan-directory-for-rootonly-project (directory)
  "Detect an EDE project for the current DIRECTORY by scanning.
This function ALWAYS scans files and directories and DOES NOT
use any file caches.
Return a cons cell:
  ( ROOTDIR . PROJECT-AUTOLOAD)"
  (let* ((ede--detect-found-project nil)
	 (root
	  (catch 'stopscan
	    (locate-dominating-file directory
				    'ede--detect-ldf-rootonly-predicate))))
    (when root
      (cons root ede--detect-found-project))))


;;; NESTED PROJECT SCAN
;;
;; For projects that can have their dominating file exist in all their
;; sub-directories as well.

(defvar ede--detect-nomatch-auto nil
  "An ede autoload that needs to be un-matched.")

(defun ede--detect-ldf-root-predicate (dir)
  "Non-nil if DIR no longer match `ede--detect-nomatch-auto'."
  (or (ede--detect-stop-scan-p dir)
      ;; To know if DIR is at the top, we need to look just above
      ;; to see if there is a match.
      (let ((updir (file-name-directory (directory-file-name dir))))
	(if (equal updir dir)
	    ;; If it didn't change, then obviously this must be the top.
	    t
	  ;; If it is different, check updir for the file.
	  (not (ede-auto-detect-in-dir ede--detect-nomatch-auto updir))))))

(defun ede--detect-scan-directory-for-project-root (directory auto)
  "If DIRECTORY has already been detected with AUTO, find the root.
Some projects have their dominating file in all their directories, such
as Project.ede.  In that case we will detect quickly, but then need
to scan upward to find the topmost occurrence of that file."
  (let* ((ede--detect-nomatch-auto auto)
	 (root (locate-dominating-file directory
				       'ede--detect-ldf-root-predicate)))
    root))

;;; TOP LEVEL SCAN
;;
;; This function for combining the above scans.
(defun ede-detect-directory-for-project (directory)
  "Detect an EDE project for the current DIRECTORY.
Scan the filesystem for a project.
Return a cons cell:
  ( ROOTDIR . PROJECT-AUTOLOAD)"
  (let* ((scan (ede--detect-scan-directory-for-project directory))
	 (root (car scan))
	 (auto (cdr scan)))
    (when scan
      ;; If what we found is already a root-only project, return it.
      (if (oref auto root-only)
	  scan

	;; If what we found is a generic project, check to make sure we aren't
	;; in some other kind of root project.
	(if (oref auto generic-p)
	    (let ((moreroot (ede--detect-scan-directory-for-rootonly-project root)))
	      ;; If we found a rootier project, return that.
	      (if moreroot
		  moreroot

		;; If we didn't find a root from the generic project, then
		;; we need to rescan upward.
		(cons (ede--detect-scan-directory-for-project-root root auto) auto)))

	  ;; Non-generic non-root projects also need to rescan upward.
	  (cons (ede--detect-scan-directory-for-project-root root auto) auto)))

	  )))

;;; TEST
;;
;; A quick interactive testing fcn.
(defun ede-detect-qtest ()
  "Run a quick test for autodetecting on BUFFER."
  (interactive)
  (let ((start (current-time))
	(ans (ede-detect-directory-for-project default-directory)))
    (if ans
	(message "Project found in %d sec @ %s of type %s"
		 (float-time (time-subtract nil start))
		 (car ans)
		 (eieio-object-name-string (cdr ans)))
      (message "No Project found.") )))


(provide 'ede/detect)

;;; ede/detect.el ends here
