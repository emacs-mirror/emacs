;;; upd-copyr.el --- update the copyright notice in a GNU Emacs Lisp file

;;; Copyright (C) 1991, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.

;; Author: Roland McGrath <roland@gnu.ai.mit.edu>
;; Keywords: maint

;;; This file is part of GNU Emacs.

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to roland@ai.mit.edu) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.

;;; Code:

(defconst current-year (substring (current-time-string) -4)
  "String representing the current year.")

(defvar current-gpl-version "2"
  "String representing the current version of the GPL.")

;;;###autoload
(defvar replace-copying-with nil
  "*If non-nil, replace copying notices with this file.")

(defvar inhibit-update-copyright nil
  "If nil, ask the user whether or not to update the copyright notice.
If the user has said no, we set this to t locally.")

;;;###autoload
(defun update-copyright (&optional replace ask-upd ask-year)
  "Update the copyright notice at the beginning of the buffer
to indicate the current year.  If optional arg REPLACE is given
\(interactively, with prefix arg\) replace the years in the notice
rather than adding the current year after them.
If `replace-copying-with' is set, the copying permissions following the
copyright are replaced as well.

If optional third argument ASK is non-nil, the user is prompted for whether
or not to update the copyright.  If optional fourth argument ASK-YEAR is
non-nil, the user is prompted for whether or not to replace the year rather
than adding to it."
  (interactive "*P")
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      ;; Handle abbreviated year lists like "1800, 01, 02, 03"
      ;; or "1900, '01, '02, '03".
      (if (re-search-forward (concat "\\(" (substring current-year 0 2)
				     "\\)?"
				     "\\([0-9][0-9]\\(,\\s \\)+\\)*'?"
				     (substring current-year 2))
			     nil t)
	  (or ask-upd
	      (message "Copyright notice already includes %s." current-year))
	(goto-char (point-min))
	(if (and (not inhibit-update-copyright)
		 (or (not ask-upd)
		     ;; If implicit, narrow it down to things that
		     ;; look like GPL notices.
		     (prog1
			 (search-forward "is free software" nil t)
		       (goto-char (point-min))))
		 (re-search-forward
		  "[Cc]opyright[^0-9]*\\(\\(\\([-, \t]*\\([0-9]+\\)\\)\\)+\\)"
		  nil t)
		 (or (not ask-upd)
		     (save-window-excursion
		       (pop-to-buffer (current-buffer))
		       (save-excursion
			 ;; Show the user the copyright.
			 (goto-char (point-min))
			 (sit-for 0)
			 (or (y-or-n-p "Update copyright? ")
			     (progn
			       (set (make-local-variable
				     'inhibit-update-copyright) t)
			       nil))))))
	    (progn
	      (setq replace
		    (or replace
			(and ask-year
			     (save-window-excursion
			       (pop-to-buffer (current-buffer))
			       (save-excursion
				 ;; Show the user the copyright.
				 (goto-char (point-min))
				 (sit-for 0)
				 (y-or-n-p "Replace copyright year? "))))))
	      (if replace
		  (delete-region (match-beginning 1) (match-end 1))
		(insert ", "))
	      (insert current-year)
	      (message "Copyright updated to %s%s."
		       (if replace "" "include ") current-year)
	  (if replace-copying-with
	      (let ((case-fold-search t)
		    beg)
		(goto-char (point-min))
		;; Find the beginning of the copyright.
		(if (search-forward "copyright" nil t)
		    (progn
		      ;; Look for a blank line or a line
		      ;; containing only comment chars.
		      (if (re-search-forward "^\\(\\s \\s<\\|\\s>\\)*$" nil t)
			  (forward-line 1)
			(with-output-to-temp-buffer "*Help*"
			  (princ (substitute-command-keys "\
I don't know where the copying notice begins.
Put point there and hit \\[exit-recursive-edit]."))
			  (save-excursion
			    (set-buffer standard-output)
			    (help-mode))
			  (recursive-edit)))
		      (setq beg (point))
		      (or (search-forward "02139, USA." nil t)
			  (with-output-to-temp-buffer "*Help*"
			    (princ (substitute-command-keys "\
I don't know where the copying notice ends.
Put point there and hit \\[exit-recursive-edit]."))
			    (save-excursion
			      (set-buffer standard-output)
			      (help-mode))
			    (recursive-edit)))
		      (delete-region beg (point))))
		(insert-file replace-copying-with))
	    (if (re-search-forward
		 "; either version \\(.+\\), or (at your option)"
		 nil t)
		(progn
		  (goto-char (match-beginning 1))
		  (delete-region (point) (match-end 1))
		  (insert current-gpl-version))))))))))

;;;###autoload
(defun ask-to-update-copyright ()
  "If the current buffer contains a copyright notice that is out of date,
ask the user if it should be updated with `update-copyright' (which see).
Put this on write-file-hooks."
  (update-copyright nil t t)
  ;; Be sure return nil; if a write-file-hook return non-nil,
  ;; the file is presumed to be already written.
  nil)

(provide 'upd-copyr)

;;; upd-copyr.el ends here
