;;; rect.el --- rectangle functions for GNU Emacs

;; Copyright (C) 1985, 1999, 2000, 2001 Free Software Foundation, Inc.

;; Maintainer: Didier Verna <didier@xemacs.org>
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

;;; Commentary:

;; This package provides the operations on rectangles that are documented
;; in the Emacs manual.

;; ### NOTE: this file has been almost completely rewritten by Didier Verna
;; <didier@xemacs.org> in July 1999. The purpose of this rewrite is to be less
;; intrusive and fill lines with whitespaces only when needed. A few functions
;; are untouched though, as noted above their definition.


;;; Code:

;;;###autoload
(defun move-to-column-force (column &optional flag)
  "Obsolete.  Use `move-to-column'.
If COLUMN is within a multi-column character, replace it by spaces and tab.
As for `move-to-column', passing anything but nil or t in FLAG will move to
the desired column only if the line is long enough."
  (move-to-column column (or flag t)))
(make-obsolete 'move-to-column-force "move-to-column" "21.2")

;; not used any more --dv
;; extract-rectangle-line stores lines into this list
;; to accumulate them for extract-rectangle and delete-extract-rectangle.
(defvar operate-on-rectangle-lines)

;; ### NOTE: this function is untouched, but not used anymore apart from
;; `delete-whitespace-rectangle'. `apply-on-rectangle' is used instead. --dv
(defun operate-on-rectangle (function start end coerce-tabs)
  "Call FUNCTION for each line of rectangle with corners at START, END.
If COERCE-TABS is non-nil, convert multi-column characters
that span the starting or ending columns on any line
to multiple spaces before calling FUNCTION.
FUNCTION is called with three arguments:
 position of start of segment of this line within the rectangle,
 number of columns that belong to rectangle but are before that position,
 number of columns that belong to rectangle but are after point.
Point is at the end of the segment of this line within the rectangle."
  (let (startcol startlinepos endcol endlinepos)
    (save-excursion
     (goto-char start)
     (setq startcol (current-column))
     (beginning-of-line)
     (setq startlinepos (point)))
    (save-excursion
     (goto-char end)
     (setq endcol (current-column))
     (forward-line 1)
     (setq endlinepos (point-marker)))
    (if (< endcol startcol)
	(setq startcol (prog1 endcol (setq endcol startcol))))
    (save-excursion
     (goto-char startlinepos)
     (while (< (point) endlinepos)
       (let (startpos begextra endextra)
	 (if coerce-tabs
	     (move-to-column startcol t)
	   (move-to-column startcol))
	 (setq begextra (- (current-column) startcol))
	 (setq startpos (point))
	 (if coerce-tabs
	     (move-to-column endcol t)
	   (move-to-column endcol))
	 ;; If we overshot, move back one character
	 ;; so that endextra will be positive.
	 (if (and (not coerce-tabs) (> (current-column) endcol))
	     (backward-char 1))
	 (setq endextra (- endcol (current-column)))
	 (if (< begextra 0)
	     (setq endextra (+ endextra begextra)
		   begextra 0))
	 (funcall function startpos begextra endextra))
       (forward-line 1)))
    (- endcol startcol)))

;; The replacement for `operate-on-rectangle' -- dv
(defun apply-on-rectangle (function start end &rest args)
  "Call FUNCTION for each line of rectangle with corners at START, END.
FUNCTION is called with two arguments: the start and end columns of the
rectangle, plus ARGS extra arguments.  Point is at the beginning of line when
the function is called."
  (let (startcol startpt endcol endpt)
    (save-excursion
      (goto-char start)
      (setq startcol (current-column))
      (beginning-of-line)
      (setq startpt (point))
      (goto-char end)
      (setq endcol (current-column))
      (forward-line 1)
      (setq endpt (point-marker))
      ;; ensure the start column is the left one.
      (if (< endcol startcol)
	  (let ((col startcol))
	    (setq startcol endcol endcol col)))
      ;; start looping over lines
      (goto-char startpt)
      (while (< (point) endpt)
	(apply function startcol endcol args)
	(forward-line 1)))
    ))

(defun delete-rectangle-line (startcol endcol fill)
  (when (= (move-to-column startcol (or fill 'coerce)) startcol)
    (delete-region (point)
		   (progn (move-to-column endcol 'coerce)
			  (point)))))

(defun delete-extract-rectangle-line (startcol endcol lines fill)
  (let ((pt (point-at-eol)))
    (if (< (move-to-column startcol (or fill 'coerce)) startcol)
	(setcdr lines (cons (spaces-string (- endcol startcol))
			    (cdr lines)))
      ;; else
      (setq pt (point))
      (move-to-column endcol t)
      (setcdr lines (cons (buffer-substring pt (point)) (cdr lines)))
      (delete-region pt (point)))
    ))

;; ### NOTE: this is actually the only function that needs to do complicated
;; stuff like what's happening in `operate-on-rectangle', because the buffer
;; might be read-only. --dv
(defun extract-rectangle-line (startcol endcol lines)
  (let (start end begextra endextra line)
    (move-to-column startcol)
    (setq start (point)
	  begextra (- (current-column) startcol))
    (move-to-column endcol)
    (setq end (point)
	  endextra (- endcol (current-column)))
    (setq line (buffer-substring start (point)))
    (if (< begextra 0)
	(setq endextra (+ endextra begextra)
	      begextra 0))
    (if (< endextra 0)
	(setq endextra 0))
    (goto-char start)
    (while (search-forward "\t" end t)
      (let ((width (- (current-column)
		      (save-excursion (forward-char -1)
				      (current-column)))))
	(setq line (concat (substring line 0 (- (point) end 1))
			   (spaces-string width)
			   (substring line (+ (length line)
					      (- (point) end)))))))
    (if (or (> begextra 0) (> endextra 0))
	(setq line (concat (spaces-string begextra)
			   line
			   (spaces-string endextra))))
    (setcdr lines (cons line (cdr lines)))))

(defconst spaces-strings
  '["" " " "  " "   " "    " "     " "      " "       " "        "])

;; this one is untouched --dv
(defun spaces-string (n)
  (if (<= n 8) (aref spaces-strings n)
    (let ((val ""))
      (while (> n 8)
	(setq val (concat "        " val)
	      n (- n 8)))
      (concat val (aref spaces-strings n)))))
    
;;;###autoload
(defun delete-rectangle (start end &optional fill)
  "Delete (don't save) text in the region-rectangle.
The same range of columns is deleted in each line starting with the
line where the region begins and ending with the line where the region
ends.

When called from a program the rectangle's corners are START and END.
With a prefix (or a FILL) argument, also fill lines where nothing has
to be deleted."
  (interactive "*r\nP")
  (apply-on-rectangle 'delete-rectangle-line start end fill))

;;;###autoload
(defun delete-extract-rectangle (start end &optional fill)
  "Delete the contents of the rectangle with corners at START and END.
Return it as a list of strings, one for each line of the rectangle.

When called from a program the rectangle's corners are START and END.
With an optional FILL argument, also fill lines where nothing has to be
deleted."
  (let ((lines (list nil)))
    (apply-on-rectangle 'delete-extract-rectangle-line start end lines fill)
    (nreverse (cdr lines))))

;;;###autoload
(defun extract-rectangle (start end)
  "Return the contents of the rectangle with corners at START and END.
Return it as a list of strings, one for each line of the rectangle."
  (let ((lines (list nil)))
    (apply-on-rectangle 'extract-rectangle-line start end lines)
    (nreverse (cdr lines))))

(defvar killed-rectangle nil
  "Rectangle for `yank-rectangle' to insert.")

;;;###autoload
(defun kill-rectangle (start end &optional fill)
  "Delete the region-rectangle and save it as the last killed one.

When called from a program the rectangle's corners are START and END.
You might prefer to use `delete-extract-rectangle' from a program.

With a prefix (or a FILL) argument, also fill lines where nothing has to be
deleted."
  (interactive "*r\nP")
  (when buffer-read-only
    (setq killed-rectangle (extract-rectangle start end))
    (barf-if-buffer-read-only))
  (setq killed-rectangle (delete-extract-rectangle start end fill)))

;; this one is untouched --dv
;;;###autoload
(defun yank-rectangle ()
  "Yank the last killed rectangle with upper left corner at point."
  (interactive "*")
  (insert-rectangle killed-rectangle))

;; this one is untoutched --dv
;;;###autoload
(defun insert-rectangle (rectangle)
  "Insert text of RECTANGLE with upper left corner at point.
RECTANGLE's first line is inserted at point, its second
line is inserted at a point vertically under point, etc.
RECTANGLE should be a list of strings.
After this command, the mark is at the upper left corner
and point is at the lower right corner."
  (let ((lines rectangle)
	(insertcolumn (current-column))
	(first t))
    (push-mark)
    (while lines
      (or first
	  (progn
	   (forward-line 1)
	   (or (bolp) (insert ?\n))
	   (move-to-column insertcolumn t)))
      (setq first nil)
      (insert-for-yank (car lines))
      (setq lines (cdr lines)))))

;;;###autoload
(defun open-rectangle (start end &optional fill)
  "Blank out the region-rectangle, shifting text right.

The text previously in the region is not overwritten by the blanks,
but instead winds up to the right of the rectangle.

When called from a program the rectangle's corners are START and END.
With a prefix (or a FILL) argument, fill with blanks even if there is no text
on the right side of the rectangle."
  (interactive "*r\nP")
  (apply-on-rectangle 'open-rectangle-line start end fill)
  (goto-char start))

(defun open-rectangle-line (startcol endcol fill)
  (when (= (move-to-column startcol (or fill 'coerce)) startcol)
    (unless (and (not fill)
		 (= (point) (point-at-eol)))
      (indent-to endcol))))

(defun delete-whitespace-rectangle-line (startcol endcol fill)
  (when (= (move-to-column startcol (or fill 'coerce)) startcol)
    (unless (= (point) (point-at-eol))
      (delete-region (point) (progn (skip-syntax-forward " ") (point))))))

;;;###autoload
(defalias 'close-rectangle 'delete-whitespace-rectangle) ;; Old name

;;;###autoload
(defun delete-whitespace-rectangle (start end &optional fill)
  "Delete all whitespace following a specified column in each line.
The left edge of the rectangle specifies the position in each line
at which whitespace deletion should begin.  On each line in the
rectangle, all continuous whitespace starting at that column is deleted.

When called from a program the rectangle's corners are START and END.
With a prefix (or a FILL) argument, also fill too short lines."
  (interactive "*r\nP")
  (apply-on-rectangle 'delete-whitespace-rectangle-line start end fill))

;; not used any more --dv
;; string-rectangle uses this variable to pass the string
;; to string-rectangle-line.
(defvar string-rectangle-string)
(defvar string-rectangle-history nil)
(defun string-rectangle-line (startcol endcol string delete)
  (move-to-column startcol t)
  (if delete
      (delete-rectangle-line startcol endcol nil))
  (insert string))

;;;###autoload
(defun string-rectangle (start end string)
  "Replace rectangle contents with STRING on each line.
The length of STRING need not be the same as the rectangle width.

Called from a program, takes three args; START, END and STRING."
  (interactive
   (progn (barf-if-buffer-read-only)
	  (list
	   (region-beginning)
	   (region-end)
	   (read-string (format "String rectangle (default `%s'): "
				(or (car string-rectangle-history) ""))
			nil 'string-rectangle-history
			(car string-rectangle-history)))))
  (apply-on-rectangle 'string-rectangle-line start end string t))

;;;###autoload
(defalias 'replace-rectangle 'string-rectangle)

;;;###autoload
(defun string-insert-rectangle (start end string)
  "Insert STRING on each line of region-rectangle, shifting text right.

When called from a program, the rectangle's corners are START and END.
The left edge of the rectangle specifies the column for insertion.
This command does not delete or overwrite any existing text."
  (interactive
   (progn (barf-if-buffer-read-only)
	  (list
	   (region-beginning)
	   (region-end)
	   (read-string (format "String insert rectangle (default `%s'): "
				(or (car string-rectangle-history) ""))
			nil 'string-rectangle-history
			(car string-rectangle-history)))))
  (apply-on-rectangle 'string-rectangle-line start end string nil))

;;;###autoload
(defun clear-rectangle (start end &optional fill)
  "Blank out the region-rectangle.
The text previously in the region is overwritten with blanks.

When called from a program the rectangle's corners are START and END.
With a prefix (or a FILL) argument, also fill with blanks the parts of the
rectangle which were empty."
  (interactive "*r\nP")
  (apply-on-rectangle 'clear-rectangle-line start end fill))

(defun clear-rectangle-line (startcol endcol fill)
  (let ((pt (point-at-eol)))
    (when (= (move-to-column startcol (or fill 'coerce)) startcol)
      (if (and (not fill)
	       (<= (save-excursion (goto-char pt) (current-column)) endcol))
	  (delete-region (point) pt)
	;; else
	(setq pt (point))
	(move-to-column endcol t)
	(setq endcol (current-column))
	(delete-region pt (point))
	(indent-to endcol)))))

(provide 'rect)

;;; rect.el ends here
