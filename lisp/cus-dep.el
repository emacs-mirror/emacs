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
(load-file "widget.el")
(load-file "custom.el")
(load-file "cus-face.el")

(defun custom-make-dependencies ()
  "Batch function to extract custom dependencies from .el files.
Usage: emacs -batch -l ./cus-dep.el -f custom-make-dependencies"
  (let ((enable-local-eval nil)
	(files (directory-files "" nil "\\`[^=].*\\.el\\'" t))
	file)
    (while files
      (setq file (car files)
	    files (cdr files))
      (message "Checking %s..." file)
      (set-buffer (find-file-noselect file))
      (goto-char (point-min))
      (string-match "\\`\\(.*\\)\\.el\\'" file)
      (condition-case nil
	  (let ((name (file-name-nondirectory (match-string 1 file))))
	    (while t
	      (let ((expr (read (current-buffer))))
		(when (and (listp expr)
			   (memq (car expr) '(defcustom defface defgroup)))
		  (eval expr)
		  (put (nth 1 expr) 'custom-where name)))))
	(error nil))
      (kill-buffer (current-buffer))))
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
  (save-buffer)
  (message "Generating cus-load.el...done"))

;;; cus-dep.el ends here
