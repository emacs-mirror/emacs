;;; cus-dep.el --- Find customization dependencies.
;;
;; Copyright (C) 1997 Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Keywords: internal

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

;;; Code:

(require 'cl)
(require 'widget)
(require 'cus-face)

(defun custom-make-dependencies ()
  "Batch function to extract custom dependencies from .el files.
Usage: emacs -batch -l ./cus-dep.el -f custom-make-dependencies"
  (let ((enable-local-eval nil)
	all-subdirs pending
	(start-directory default-directory))
    (get-buffer-create " cus-dep temp")
    (set-buffer " cus-dep temp")
    (setq pending '("."))
    (while pending
      (let ((this (car pending))
	    this-subdirs
	    default-directory)
	(setq all-subdirs (cons this all-subdirs))
	(setq pending (cdr pending))
	(setq default-directory
	      (expand-file-name this start-directory))
	(message "Finding subdirs of %s" this)
	(erase-buffer)
	(condition-case nil
	    (progn
	      (insert-file-contents "subdirs.el")
	      (goto-char (point-min))
	      (search-forward "'(")
	      (forward-char -1)
	      (setq this-subdirs (read (current-buffer)))
	      (setq pending (nconc pending
				   (mapcar 
				    (function (lambda (dir)
						(file-relative-name
						 (file-name-as-directory
						  (expand-file-name dir this))
						 start-directory)))
				    this-subdirs))))
	  (error nil))))

    (while all-subdirs
      (message "Directory %s" (car all-subdirs))
      (let ((files (directory-files (car all-subdirs) nil "\\`[^=].*\\.el\\'"))
	    (default-directory default-directory)
	    file)
	(cd (car all-subdirs))
	(while files
	  (setq file (car files)
		files (cdr files))
	  (message "Checking %s..." file)
	  (erase-buffer)
	  (insert-file-contents file)
	  (goto-char (point-min))
	  (string-match "\\`\\(.*\\)\\.el\\'" file)
	  (let ((name (file-name-nondirectory (match-string 1 file))))
	    (condition-case nil
		(while (re-search-forward "^(defcustom\\|^(defface\\|^(defgroup"
					  nil t)
		  (beginning-of-line)
		  (let ((expr (read (current-buffer))))
		    (eval expr)
		    (put (nth 1 expr) 'custom-where name)))
	      (error nil))))
	(setq all-subdirs (cdr all-subdirs)))))
  (message "Generating cus-load.el...")
  (find-file "cus-load.el")
  (erase-buffer)
  (insert "\
;;; cus-load.el --- automatically extracted custom dependencies
;;
;;; Code:

")
  (mapatoms (lambda (symbol)
	      (let ((members (get symbol 'custom-group))
		    item where found)
		(when members
		  (while members
		    (setq item (car (car members))
			  members (cdr members)
			  where (get item 'custom-where))
		    (unless (or (null where)
				(member where found))
		      (if found
			  (insert " ")
			(insert "(put '" (symbol-name symbol) 
				" 'custom-loads '("))
		      (prin1 where (current-buffer))
		      (push where found)))
		  (when found
		    (insert "))\n"))))))
  (insert "\

\(provide 'cus-load)

;;; cus-load.el ends here\n")
  (let ((kept-new-versions 10000000))
    (save-buffer))
  (message "Generating cus-load.el...done"))

;;; cus-dep.el ends here
