;;; Copyright (C) 1991 Free Software Foundation, Inc.
;;; Written by Roland McGrath
;;;
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

(defconst current-year (substring (current-time-string) -4)
  "String representing the current year.")

(defvar current-gpl-version "2"
  "String representing the current version of the GPL.")

;;;###autoload
(defvar replace-copying-with nil
  "*If non-nil, replace copying notices with this file.")

;;;###autoload
(defun update-copyright (&optional replace)
  "Update the copyright notice at the beginning of the buffer
to indicate the current year.  If optional arg REPLACE is given
\(interactively, with prefix arg\) replace the years in the notice
rather than adding the current year after them.
If `replace-copying-with' is set, the copying permissions following the
copyright are replaced as well."
  (interactive "*P")
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (if (search-forward current-year nil t)
	  (message "Copyright notice already includes %s." current-year)
	(goto-char (point-min))
	(or (re-search-forward
	     "[Cc]opyright[^0-9]*\\(\\([-, \t]*\\([0-9]+\\)\\)\\)+"
	      nil t)
	    (error "This buffer contains no copyright notice!"))
	(if replace
	    (delete-region (match-beginning 1) (match-end 1))
	  (insert ", "))
	(insert current-year)
	(message "Copyright updated to %s%s."
		 (if replace "" "include ") current-year))
      (if replace-copying-with
	  (let ((case-fold-search t)
		beg)
	    (goto-char (point-min))
	    ;; Find the beginning of the copyright.
	    (or (search-forward "copyright" nil t)
		(error "Copyright notice not found!"))
	    ;; Look for a blank line or a line containing only comment chars.
	    (if (re-search-forward "^\\(\\s \\s<\\|\\s>\\)*$" nil t)
		(forward-line 1)
	      (with-output-to-temp-buffer "*Help*"
		(princ (substitute-command-keys "\
I don't know where the copying notice begins.
Put point there and hit \\[exit-recursive-edit]."))
		(recursive-edit)))
	    (setq beg (point))
	    (or (search-forward "02139, USA." nil t)
		(with-output-to-temp-buffer "*Help*"
		  (princ (substitute-command-keys "\
I don't know where the copying notice ends.
Put point there and hit \\[exit-recursive-edit]."))
		  (recursive-edit)))
	    (delete-region beg (point))
	    (insert-file replace-copying-with))
	(if (re-search-forward "; either version \\(.+\\), or (at your option)"
			       nil t)
	    (progn
	      (goto-char (match-beginning 1))
	      (delete-region (point) (match-end 1))
	      (insert current-gpl-version)))))))

(provide 'upd-copyr)
