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

(eval-when-compile (require 'cl))
(require 'widget)
(require 'cus-face)
(require 'autoload)

(defun custom-make-dependencies ()
  "Batch function to extract custom dependencies from .el files.
Usage: emacs -batch -l ./cus-dep.el -f custom-make-dependencies DIRS"
  (let ((enable-local-eval nil)
	(all-subdirs command-line-args-left)
	(start-directory default-directory))
    (get-buffer-create " cus-dep temp")
    (set-buffer " cus-dep temp")
    (while all-subdirs
      (message "Directory %s" (car all-subdirs))
      (let ((files (directory-files (car all-subdirs) nil "\\`[^=].*\\.el\\'"))
	    (default-directory default-directory)
	    file
	    is-autoloaded)
	(cd (car all-subdirs))
	(while files
	  (setq file (car files)
		files (cdr files))
	  (when (file-exists-p file)
	    (erase-buffer)
	    (insert-file-contents file)
	    (goto-char (point-min))
	    (string-match "\\`\\(.*\\)\\.el\\'" file)
	    (let ((name (file-name-nondirectory (match-string 1 file))))
	      (condition-case nil
		  (while (re-search-forward "^(defcustom\\|^(defface\\|^(defgroup"
					    nil t)
		    (setq is-autoloaded nil)
		    (beginning-of-line)
		    (save-excursion
		      (forward-line -1)
		      (if (looking-at generate-autoload-cookie)
			  (setq is-autoloaded t)))
		    (let ((expr (read (current-buffer))))
		      (condition-case nil
			  (progn
			    (eval expr)
			    (put (nth 1 expr) 'custom-autoloaded is-autoloaded)
			    (put (nth 1 expr) 'custom-where name))
			(error nil))))
		(error nil)))))
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
;;; These are for handling :version.  We need to have a minimum of
;;; information so `custom-changed-variables' could do its job.  
;;; For both groups and variables we have to set `custom-version'.
;;; For variables we also set the `standard-value' and for groups
;;; `group-documentation' (which is shown in the customize buffer), so
;;; we don't have to load the file containing the group.

;;; `custom-versions-load-alist' is an alist that has as car a version
;;; number and as elts the files that have variables that contain that
;;; version. These files should be loaded before showing the
;;; customization buffer that `customize-changed-options' generates.


;;; This macro is used so we don't modify the information about
;;; variables and groups if it's already set. (We don't know when
;;; cus-load.el is going to be loaded and at that time some of the
;;; files might be loaded and some others might not).
(defmacro custom-put-if-not (symbol propname value)
  `(unless (get ,symbol ,propname)
     (put ,symbol ,propname ,value)))

")
  (let ((version-alist nil))
    (mapatoms (lambda (symbol)
		(let ((version (get symbol 'custom-version))
		      where)
		  (when version 
		    (setq where (get symbol 'custom-where))
		    (when (and where 
			       ;; Don't bother to do anything if it's
			       ;; autoloaded because we will have all
			       ;; this info when emacs is running
			       ;; anyway.
			       (not (get symbol 'custom-autoloaded)))
		      (insert "(custom-put-if-not '" (symbol-name symbol) 
			      " 'custom-version ")
		      (prin1 version (current-buffer))
		      (insert ")\n")
		      (insert "(custom-put-if-not '" (symbol-name symbol))
		      (if (get symbol 'standard-value)
			  ;; This means it's a variable
			  (progn
			    (insert " 'standard-value t)\n")
			    (if (assoc version version-alist)
				(unless 
				    (member where 
					    (cdr (assoc version version-alist)))
				  (push where (cdr (assoc version version-alist))))
			      (push (cons version (list where)) version-alist)))
			;; This is a group
			(insert " 'group-documentation ")
			(prin1 (get symbol 'group-documentation) (current-buffer))
			(insert ")\n")))))))

    (insert "\n(defvar custom-versions-load-alist "
	    (if version-alist "'" ""))
    (prin1 version-alist (current-buffer))
    (insert "\n \"For internal use by custom.\")\n"))
    
  (insert "\

\(provide 'cus-load)

;;; cus-load.el ends here\n")
  (let ((kept-new-versions 10000000))
    (save-buffer))
  (message "Generating cus-load.el...done")
  (kill-emacs))

;;; cus-dep.el ends here
