;;; array.el --- array editing commands for Gnu Emacs

;; Copyright (C) 1987 Free Software Foundation, Inc.

;; Author David M. Brown
;; Maintainer: FSF
;; Keywords: extensions

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Commands for editing a buffer interpreted as a rectangular array
;; or matrix of whitespace-separated strings.  You specify the array
;; dimensions and some other parameters at startup time.

;;  Written by dmb%morgoth@harvard.harvard.edu (address is old)
;;   (David M. Brown at Goldberg-Zoino & Associates, Inc.)
;;  Thanks to cph@kleph.ai.mit.edu for assistance

;; To do:
;;   Smooth initialization process by grokking local variables list
;;     at end of buffer or parsing buffer using whitespace as delimiters.
;;   Make 'array-copy-column-right faster.


;;; Code:

;;; Internal information functions.
  
(defun array-cursor-in-array-range ()
  "Returns t if the cursor is in a valid array cell.
Its ok to be on a row number line."
  (let ((columns-last-line (% max-column columns-per-line)))
    ;; Requires buffer-line and buffer-column to be current.
    (not (or
	  ;; The cursor is too far to the right.
	  (>= buffer-column line-length)
	  ;; The cursor is below the last row.
	  (>= buffer-line (* lines-per-row max-row))
	  ;; The cursor is on the last line of the row, the line is smaller
	  ;;  than the others, and the cursor is after the last array column
	  ;;  on the line.
	  (and (zerop (% (1+ buffer-line) lines-per-row))
	       (not (zerop columns-last-line))
	       (>= buffer-column (* columns-last-line field-width)))))))

(defun array-current-row ()
  "Return the array row of the field in which the cursor is located."
  ;; Requires buffer-line and buffer-column to be current.
  (and (array-cursor-in-array-range)
       (1+ (floor buffer-line lines-per-row))))

(defun array-current-column ()
  "Return the array column of the field in which the cursor is located."
  ;; Requires buffer-line and buffer-column to be current.
  (and (array-cursor-in-array-range)
       ;; It's not okay to be on a row number line.
       (not (and rows-numbered
		 (zerop (% buffer-line lines-per-row))))
       (+
	;; Array columns due to line differences.
	(* columns-per-line
	   (if rows-numbered
	       (1- (% buffer-line lines-per-row))
	     (% buffer-line lines-per-row)))
	;; Array columns on the current line.
	(1+ (floor buffer-column field-width)))))

(defun array-update-array-position (&optional a-row a-column)
  "Set `array-row' and `array-column' to their current values or
to the optional arguments A-ROW and A-COLUMN."
  ;; Requires that buffer-line and buffer-column be current.
  (setq array-row (or a-row (array-current-row))
	array-column (or a-column (array-current-column))))

(defun array-update-buffer-position ()
  "Set buffer-line and buffer-column to their current values."
  (setq buffer-line (current-line)
	buffer-column (current-column)))



;;; Information commands.

(defun array-what-position ()
  "Display the row and column in which the cursor is positioned."
  (interactive)
  (let ((buffer-line (current-line))
	(buffer-column (current-column)))
    (message (format "Array row: %s  Array column: %s" 
		     (prin1-to-string (array-current-row))
		     (prin1-to-string (array-current-column))))))

(defun array-display-local-variables ()
  "Display the current state of the local variables in the minibuffer."
  (interactive)
  (let ((buf (buffer-name (current-buffer))))
    (with-output-to-temp-buffer "*Local Variables*"
      (buffer-disable-undo standard-output)
      (terpri)
      (princ (format " Buffer:             %s\n\n" buf))
      (princ (format "  max-row:           %s\n"
		     (prin1-to-string max-row)))
      (princ (format "  max-column:        %s\n"
		     (prin1-to-string max-column)))
      (princ (format "  columns-per-line:  %s\n"
		     (prin1-to-string columns-per-line)))
      (princ (format "  field-width:       %s\n"
		     (prin1-to-string field-width)))
      (princ (format "  rows-numbered:     %s\n"
		     (prin1-to-string rows-numbered)))
      (princ (format "  lines-per-row:     %s\n"
		     (prin1-to-string lines-per-row)))
      (princ (format "  line-length:       %s\n"
		     (prin1-to-string line-length))))))



;;; Internal movement functions.

(defun array-beginning-of-field (&optional go-there)
   "Return the column of the beginning of the current field.
Optional argument GO-THERE, if non-nil, means go there too."
   ;; Requires that buffer-column be current.
   (let ((goal-column (- buffer-column (% buffer-column field-width))))
     (if go-there
	 (move-to-column-untabify goal-column)
       goal-column)))

(defun array-end-of-field (&optional go-there)
  "Return the column of the end of the current array field.
If optional argument GO-THERE is non-nil, go there too."
  ;; Requires that buffer-column be current.
  (let ((goal-column (+ (- buffer-column (% buffer-column field-width))
			field-width)))
    (if go-there
	(move-to-column-untabify goal-column)
      goal-column)))

(defun array-move-to-cell (a-row a-column)
  "Move to array row A-ROW and array column A-COLUMN.  
Leave point at the beginning of the field and return the new buffer column."
  (let ((goal-line (+ (* lines-per-row (1- a-row))
		      (if rows-numbered 1 0)
		      (floor (1- a-column) columns-per-line)))
	(goal-column (* field-width (% (1- a-column) columns-per-line))))
    (goto-char (point-min))
    (forward-line goal-line)
    (move-to-column-untabify goal-column)))

(defun array-move-to-row (a-row)
  "Move to array row A-ROW preserving the current array column.
Leave point at the beginning of the field and return the new array row."
  ;; Requires that buffer-line and buffer-column be current.
  (let ((goal-line (+ (* lines-per-row (1- a-row))
		      (% buffer-line lines-per-row)))
	(goal-column (- buffer-column (% buffer-column field-width))))
    (forward-line (- goal-line buffer-line))
    (move-to-column-untabify goal-column)
    a-row))

(defun array-move-to-column (a-column)
  "Move to array column A-COLUMN preserving the current array row.
Leave point at the beginning of the field and return the new array column."
  ;; Requires that buffer-line and buffer-column be current.
  (let ((goal-line (+ (- buffer-line (% buffer-line lines-per-row))
		      (if rows-numbered 1 0)
		      (floor (1- a-column) columns-per-line)))
	(goal-column (* field-width (% (1- a-column) columns-per-line))))
    (forward-line (- goal-line buffer-line))
    (move-to-column-untabify goal-column)
    a-column))

(defun array-move-one-row (sign)
  "Move one array row in direction SIGN (1 or -1).
Leave point at the beginning of the field and return the new array row.
If requested to move beyond the array bounds, signal an error."
  ;; Requires that buffer-line and buffer-column be current.
  (let ((goal-column (array-beginning-of-field))
	(array-row (or (array-current-row)
		       (error "Cursor is not in a valid array cell."))))
    (cond ((and (= array-row max-row) (= sign 1))
	   (error "End of array."))
	  ((and (= array-row 1) (= sign -1))
	   (error "Beginning of array."))
	  (t
	   (progn
	     (forward-line (* sign lines-per-row))
	     (move-to-column-untabify goal-column)
	     (+ array-row sign))))))

(defun array-move-one-column (sign)
  "Move one array column in direction SIGN (1 or -1).
Leave point at the beginning of the field and return the new array column.
If requested to move beyond the array bounds, signal an error."
  ;; Requires that buffer-line and buffer-column be current.
  (let ((array-column (or (array-current-column)
		      (error "Cursor is not in a valid array cell."))))
    (cond ((and (= array-column max-column) (= sign 1))
	   (error "End of array."))
	  ((and (= array-column 1) (= sign -1))
	   (error "Beginning of array."))
	  (t
	   (cond 
	    ;; Going backward from first column on the line.
	    ((and (= sign -1) (= 1 (% array-column columns-per-line)))
	     (forward-line -1)
	     (move-to-column-untabify
	      (* field-width (1- columns-per-line))))
	    ;; Going forward from last column on the line.
	    ((and (= sign 1) (zerop (% array-column columns-per-line)))
	     (forward-line 1))
	    ;; Somewhere in the middle of the line.
	    (t
	     (move-to-column-untabify (+ (array-beginning-of-field)
					 (* field-width sign)))))
	   (+ array-column sign)))))

(defun array-normalize-cursor ()
  "Move the cursor to the first non-whitespace character in the field and,
if necessary, scroll horizontally to keep the cursor in view."
  ;; Assumes point is at the beginning of the field.
  (let ((buffer-column (current-column)))
    (skip-chars-forward " \t"
			(1- (save-excursion (array-end-of-field t) (point))))
    (array-maybe-scroll-horizontally)))

(defun array-maybe-scroll-horizontally ()
  "If necessary, scroll horizontally to keep the cursor in view."
  ;; This is only called from array-normalize-cursor so
  ;;  buffer-column will always be current.
  (let ((w-hscroll (window-hscroll))
	(w-width (window-width)))
    (cond
     ((and (>= buffer-column w-hscroll)
	   (<= buffer-column (+ w-hscroll w-width)))
      ;; It's already visible.  Do nothing.
      nil)
     ((> buffer-column (+ w-hscroll w-width))
      ;; It's to the right.  Scroll left.
      (scroll-left (- (- buffer-column w-hscroll)
		      (/ w-width 2))))
     (t
      ;; It's to the left.  Scroll right.
      (scroll-right (+ (- w-hscroll buffer-column)
		       (/ w-width 2)))))))



;;; Movement commands.

(defun array-next-row (&optional arg)
  "Move down one array row, staying in the current array column.
If optional ARG is given, move down ARG array rows."
  (interactive "p")
  (let ((buffer-line (current-line))
	(buffer-column (current-column)))
    (if (= (abs arg) 1)
	(array-move-one-row arg)
      (array-move-to-row
       (limit-index (+ (or (array-current-row)
			   (error "Cursor is not in an array cell."))
		       arg)
		    max-row))))
  (array-normalize-cursor))

(defun array-previous-row (&optional arg)
  "Move up one array row, staying in the current array column.
If optional ARG is given, move up ARG array rows."
  (interactive "p")
  (array-next-row (- arg)))

(defun array-forward-column (&optional arg)
  "Move forward one field, staying in the current array row.
If optional ARG is given, move forward ARG array columns.
If necessary, keep the cursor in the window by scrolling right or left."
  (interactive "p")
  (let ((buffer-line (current-line))
	(buffer-column (current-column)))
    (if (= (abs arg) 1)
	(array-move-one-column arg)
      (array-move-to-column
       (limit-index (+ (or (array-current-column)
			   (error "Cursor is not in an array cell."))
		       arg)
		    max-column))))
  (array-normalize-cursor))

(defun array-backward-column (&optional arg)
  "Move backward one field, staying in the current array row.
If optional ARG is given, move backward ARG array columns.
If necessary, keep the cursor in the window by scrolling right or left."
  (interactive "p")
  (array-forward-column (- arg)))

(defun array-goto-cell (a-row a-column)
  "Go to array row A-ROW and array column A-COLUMN."
  (interactive "nArray row: \nnArray column: ")
  (array-move-to-cell
   (limit-index a-row max-row)
   (limit-index a-column max-column))
  (array-normalize-cursor))



;;; Internal copying functions.

(defun array-field-string ()
  "Return the field string at the current cursor location."
  ;; Requires that buffer-column be current.
  (buffer-substring
   (save-excursion (array-beginning-of-field t) (point))
   (save-excursion (array-end-of-field t) (point))))

(defun array-copy-once-vertically (sign)
  "Copy the current field into one array row in direction SIGN (1 or -1).
Leave point at the beginning of the field and return the new array row.
If requested to move beyond the array bounds, signal an error."
  ;; Requires that buffer-line, buffer-column, and copy-string be current.
  (let ((a-row (array-move-one-row sign)))
    (let ((inhibit-quit t))
      (delete-region (point) (save-excursion (array-end-of-field t) (point)))
      (insert copy-string))
    (move-to-column buffer-column)
    a-row))

(defun array-copy-once-horizontally (sign)
  "Copy the current field into one array column in direction SIGN (1 or -1).
Leave point at the beginning of the field and return the new array column.
If requested to move beyond the array bounds, signal an error."
  ;; Requires that buffer-line, buffer-column, and copy-string be current.
  (let ((a-column (array-move-one-column sign)))
    (array-update-buffer-position)
    (let ((inhibit-quit t))
      (delete-region (point) (save-excursion (array-end-of-field t) (point)))
      (insert copy-string))
    (move-to-column buffer-column)
    a-column))

(defun array-copy-to-row (a-row)
  "Copy the current field vertically into every cell up to and including A-ROW.
Leave point at the beginning of the field."
  ;; Requires that buffer-line, buffer-column, array-row, and
  ;;  copy-string be current.
  (let* ((num (- a-row array-row))
	 (count (abs num))
	 (sign (if (zerop count) () (/ num count))))
    (while (> count 0)
      (array-move-one-row sign)
      (array-update-buffer-position)
      (let ((inhibit-quit t))
	(delete-region (point) (save-excursion (array-end-of-field t) (point)))
	(insert copy-string))
      (move-to-column buffer-column)
      (setq count (1- count)))))
	
(defun array-copy-to-column (a-column)
  "Copy the current field horizontally into every cell up to and including
A-COLUMN.  Leave point at the beginning of the field."
  ;; Requires that buffer-line, buffer-column, array-column, and
  ;;  copy-string be current.
  (let* ((num (- a-column array-column))
	 (count (abs num))
	 (sign (if (zerop count) () (/ num count))))
    (while (> count 0)
      (array-move-one-column sign)
      (array-update-buffer-position)
      (let ((inhibit-quit t))
	(delete-region (point) (save-excursion (array-end-of-field t) (point)))
	(insert copy-string))
      (move-to-column buffer-column)
      (setq count (1- count)))))

(defun array-copy-to-cell (a-row a-column)
  "Copy the current field into the cell at A-ROW, A-COLUMN.
Leave point at the beginning of the field."
  ;; Requires that copy-string be current.
  (array-move-to-cell a-row a-column)
  (array-update-buffer-position)
  (delete-region (point) (save-excursion (array-end-of-field t) (point)))
  (insert copy-string)
  (move-to-column buffer-column))



;;; Commands for copying.

(defun array-copy-down (&optional arg)
  "Copy the current field one array row down.
If optional ARG is given, copy down through ARG array rows."
  (interactive "p")
  (let* ((buffer-line (current-line))
	 (buffer-column (current-column))
	 (array-row (or (array-current-row)
			   (error "Cursor is not in a valid array cell.")))
	 (copy-string (array-field-string)))
    (if (= (abs arg) 1)
	(array-copy-once-vertically arg)
      (array-copy-to-row
       (limit-index (+ array-row arg) max-row))))
  (array-normalize-cursor))

(defun array-copy-up (&optional arg)
  "Copy the current field one array row up.
If optional ARG is given, copy up through ARG array rows."
  (interactive "p")
  (array-copy-down (- arg)))

(defun array-copy-forward (&optional arg)
  "Copy the current field one array column to the right.
If optional ARG is given, copy through ARG array columns to the right."
  (interactive "p")
  (let* ((buffer-line (current-line))
	 (buffer-column (current-column))
	 (array-column (or (array-current-column)
			   (error "Cursor is not in a valid array cell.")))
	 (copy-string (array-field-string)))
    (if (= (abs arg) 1)
	(array-copy-once-horizontally arg)
      (array-copy-to-column
       (limit-index (+ array-column arg) max-column))))
  (array-normalize-cursor))

(defun array-copy-backward (&optional arg)
  "Copy the current field one array column to the left.
If optional ARG is given, copy through ARG array columns to the left."
  (interactive "p")
  (array-copy-forward (- arg)))

(defun array-copy-column-forward (&optional arg)
  "Copy the entire current column in to the column to the right.
If optional ARG is given, copy through ARG array columns to the right."
  (interactive "p")
  (array-update-buffer-position)
  (array-update-array-position)
  (if (not array-column)
      (error "Cursor is not in a valid array cell."))
  (message "Working...")
  (let ((this-row 0))
    (while (< this-row max-row)
      (setq this-row (1+ this-row))
      (array-move-to-cell this-row array-column)
      (array-update-buffer-position)
      (let ((copy-string (array-field-string)))
	(if (= (abs arg) 1)
	    (array-copy-once-horizontally arg)
	  (array-copy-to-column
	   (limit-index (+ array-column arg) max-column))))))
  (message "Working...done")
  (array-move-to-row array-row)
  (array-normalize-cursor))

(defun array-copy-column-backward (&optional arg)
  "Copy the entire current column one column to the left.
If optional ARG is given, copy through ARG columns to the left."
  (interactive "p")
  (array-copy-column-forward (- arg)))

(defun array-copy-row-down (&optional arg)
  "Copy the entire current row one row down.
If optional ARG is given, copy through ARG rows down."
  (interactive "p")
  (array-update-buffer-position)
  (array-update-array-position)
  (if (not array-row)
      (error "Cursor is not in a valid array cell."))
  (cond
   ((and (= array-row 1) (= arg -1))
    (error "Beginning of array."))
   ((and (= array-row max-row) (= arg 1))
    (error "End of array."))
   (t
    (let* ((copy-string
	    (buffer-substring
	     (save-excursion (array-move-to-cell array-row 1)
			     (point))
	     (save-excursion (array-move-to-cell array-row max-column)
			     (forward-line 1)
			     (point))))
	   (this-row array-row)
	   (goal-row (limit-index (+ this-row arg) max-row))
	   (num (- goal-row this-row))
	   (count (abs num))
	   (sign (if (not (zerop count)) (/ num count))))
      (while (> count 0)
	(setq this-row (+ this-row sign))
	(array-move-to-cell this-row 1)
	(let ((inhibit-quit t))
	  (delete-region (point)
			 (save-excursion
			   (array-move-to-cell this-row max-column)
			   (forward-line 1)
			   (point)))
	  (insert copy-string))
	(setq count (1- count)))
      (array-move-to-cell goal-row (or array-column 1)))))
  (array-normalize-cursor))

(defun array-copy-row-up (&optional arg)
  "Copy the entire current array row into the row above.
If optional ARG is given, copy through ARG rows up."
  (interactive "p")
  (array-copy-row-down (- arg)))

(defun array-fill-rectangle ()
  "Copy the field at mark into every cell between mark and point."
  (interactive)
  ;; Bind arguments.
  (array-update-buffer-position)
  (let ((p-row (or (array-current-row)
		   (error "Cursor is not in a valid array cell.")))
	(p-column (or (array-current-column)
		      (error "Cursor is not in a valid array cell.")))
	(m-row
	 (save-excursion
	   (exchange-point-and-mark)
	   (array-update-buffer-position)
	   (or (array-current-row)
	       (error "Mark is not in a valid array cell."))))
	(m-column 
	 (save-excursion
	   (exchange-point-and-mark)
	   (array-update-buffer-position)
	   (or (array-current-column)
	       (error "Mark is not in a valid array cell.")))))
    (message "Working...")
    (let ((top-row (min m-row p-row))
	  (bottom-row (max m-row p-row))
	  (left-column (min m-column p-column))
	  (right-column (max m-column p-column)))
      ;; Do the first row.
      (let ((copy-string
	     (save-excursion
	       (array-move-to-cell m-row m-column)
	       (array-update-buffer-position)
	       (array-field-string))))
	(array-copy-to-cell top-row left-column)
	(array-update-array-position top-row left-column)
	(array-update-buffer-position)
	(array-copy-to-column right-column))
      ;; Do the rest of the rows.
      (array-move-to-cell top-row left-column)
      (let ((copy-string
	     (buffer-substring
	      (point)
	      (save-excursion
		(array-move-to-cell top-row right-column)
		(setq buffer-column (current-column))
		(array-end-of-field t)
		(point))))
	    (this-row top-row))
	(while (/= this-row bottom-row)
	  (setq this-row (1+ this-row))
	  (array-move-to-cell this-row left-column)
	  (let ((inhibit-quit t))
	    (delete-region
	     (point)
	     (save-excursion
	       (array-move-to-cell this-row right-column)
	       (setq buffer-column (current-column))
	       (array-end-of-field t)
	       (point)))
	    (insert copy-string)))))
    (message "Working...done")
    (array-goto-cell p-row p-column)))



;;; Reconfiguration of the array.

(defun array-make-template ()
  "Create the template of an array."
  (interactive)
  ;; If there is a conflict between field-width and init-string, resolve it.
  (let ((check t)
	(len))
    (while check
      (setq init-field (read-input "Initial field value: "))
      (setq len (length init-field))
      (if (/= len field-width)
	  (if (y-or-n-p (format "Change field width to %d? " len))
	      (progn (setq field-width len)
		     (setq check nil)))
	(setq check nil))))
  (goto-char (point-min))
  (message "Working...")
  (let ((this-row 1))
    ;; Loop through the rows.
    (while (<= this-row max-row)
      (if rows-numbered
	  (insert (format "%d:\n" this-row)))
      (let ((this-column 1))
	;; Loop through the columns.
	(while (<= this-column max-column)
	  (insert init-field)
	  (if (and (zerop (% this-column columns-per-line))
		   (/= this-column max-column))
	      (newline))
	  (setq this-column (1+ this-column))))
      (setq this-row (1+ this-row))
      (newline)))
  (message "Working...done")
  (array-goto-cell 1 1))

(defun array-reconfigure-rows (new-columns-per-line new-rows-numbered)
  "Reconfigure the state of `rows-numbered' and `columns-per-line'.
NEW-COLUMNS-PER-LINE is the desired value of `columns-per-line' and
NEW-ROWS-NUMBERED (a character, either ?y or ?n) is the desired value
of rows-numbered."
  (interactive "nColumns per line: \ncRows numbered? (y or n) ")
  ;; Check on new-columns-per-line
  (let ((check t))
    (while check
      (if (and (>= new-columns-per-line 1)
	       (<= new-columns-per-line max-column))
	  (setq check nil)
	(setq new-columns-per-line
	      (string-to-int 
	       (read-input 
		(format "Columns per line (1 - %d): " max-column)))))))
  ;; Check on new-rows-numbered.  It has to be done this way
  ;;  because interactive does not have y-or-n-p.
  (cond
   ((eq new-rows-numbered ?y)
    (setq new-rows-numbered t))
   ((eq new-rows-numbered ?n)
    (setq new-rows-numbered nil))
   (t
    (setq new-rows-numbered (y-or-n-p "Rows numbered? "))))
  (message "Working...")
  (array-update-buffer-position)
  (let* ((main-buffer (buffer-name (current-buffer)))
	 (temp-buffer (make-temp-name "Array"))
	 (temp-max-row max-row)
	 (temp-max-column max-column)
	 (old-rows-numbered rows-numbered)
	 (old-columns-per-line columns-per-line)
	 (old-lines-per-row lines-per-row)
	 (old-field-width field-width)
	 (old-line-length line-length)
	 (this-row 1))
    (array-update-array-position)
    ;; Do the cutting in a temporary buffer.
    (copy-to-buffer temp-buffer (point-min) (point-max))
    (set-buffer temp-buffer)
    (goto-char (point-min))
    (while (<= this-row temp-max-row)
      ;; Deal with row number.
      (cond
       ((or (and old-rows-numbered new-rows-numbered)
	    (and (not old-rows-numbered) (not new-rows-numbered)))
	;; Nothing is changed.
	())
       ((and old-rows-numbered (not new-rows-numbered))
	;; Delete the row number.
	(kill-line 1))
       (t
	;; Add the row number.
	(insert-string (format "%d:\n" this-row))))
      ;; Deal with the array columns in this row.
      (cond
       ((= old-columns-per-line new-columns-per-line)
	;; Nothing is changed.  Go to the next row.
	(forward-line (- old-lines-per-row (if old-rows-numbered 1 0))))
       (t
	;; First expand the row.  Then cut it up into new pieces.
	(let ((newlines-to-be-removed
	       (floor (1- temp-max-column) old-columns-per-line))
	      (newlines-removed 0)
	      (newlines-to-be-added
	       (floor (1- temp-max-column) new-columns-per-line))
	      (newlines-added 0))
	  (while (< newlines-removed newlines-to-be-removed)
	    (move-to-column-untabify
	     (* (1+ newlines-removed) old-line-length))
	    (kill-line 1)
	    (setq newlines-removed (1+ newlines-removed)))
	  (beginning-of-line)
	  (while (< newlines-added newlines-to-be-added)
	    (move-to-column-untabify (* old-field-width new-columns-per-line))
	    (newline)
	    (setq newlines-added (1+ newlines-added)))
	  (forward-line 1))))
      (setq this-row (1+ this-row)))
    (let ((inhibit-quit t))
      (set-buffer main-buffer)
      (erase-buffer)
      (insert-buffer temp-buffer) 
      ;; Update local variables.
      (setq columns-per-line new-columns-per-line)
      (setq rows-numbered new-rows-numbered)
      (setq line-length (* old-field-width new-columns-per-line))
      (setq lines-per-row 
	    (+ (floor (1- temp-max-column) new-columns-per-line)
	       (if new-rows-numbered 2 1)))
      (array-goto-cell (or array-row 1) (or array-column 1)))
    (kill-buffer temp-buffer))
  (message "Working...done"))

(defun array-expand-rows ()
  "Expand the rows so each fits on one line and remove row numbers."
  (interactive)
  (array-reconfigure-rows max-column ?n))



;;; Utilities.

(defun limit-index (index limit)
  (cond ((< index 1) 1)
	((> index limit) limit)
	(t index)))

(defun xor (pred1 pred2)
  "Returns the logical exclusive or of predicates PRED1 and PRED2."
  (and (or pred1 pred2)
       (not (and pred1 pred2))))

(defun current-line ()
  "Return the current buffer line at point.  The first line is 0." 
  (save-excursion
    (beginning-of-line)
    (count-lines (point-min) (point))))

(defun move-to-column-untabify (column)
  "Move to COLUMN on the current line, untabifying if necessary.
Return COLUMN."
  (or (and (= column (move-to-column column))
	   column)
      ;; There is a tab in the way.
      (if respect-tabs
	  (error "There is a TAB character in the way.")
	(progn
	  (untabify-backward)
	  (move-to-column column)))))

(defun untabify-backward ()
  "Untabify the preceding tab."
  (save-excursion
    (let ((start (point)))
      (backward-char 1)
      (untabify (point) start))))



;;; Array mode.

(defvar array-mode-map nil 
  "Keymap used in array mode.")

(if array-mode-map
    ()
  (setq array-mode-map (make-keymap))
  ;; Bind keys.
  (define-key array-mode-map "\M-ad"   'array-display-local-variables)
  (define-key array-mode-map "\M-am"   'array-make-template)
  (define-key array-mode-map "\M-ae"   'array-expand-rows)
  (define-key array-mode-map "\M-ar"   'array-reconfigure-rows)
  (define-key array-mode-map "\M-a="   'array-what-position)
  (define-key array-mode-map "\M-ag"   'array-goto-cell)
  (define-key array-mode-map "\M-af"   'array-fill-rectangle)
  (define-key array-mode-map "\C-n"    'array-next-row)
  (define-key array-mode-map "\C-p"    'array-previous-row)
  (define-key array-mode-map "\C-f"    'array-forward-column)
  (define-key array-mode-map "\C-b"    'array-backward-column)
  (define-key array-mode-map "\M-n"    'array-copy-down)
  (define-key array-mode-map "\M-p"    'array-copy-up)
  (define-key array-mode-map "\M-f"    'array-copy-forward)
  (define-key array-mode-map "\M-b"    'array-copy-backward)
  (define-key array-mode-map "\M-\C-n" 'array-copy-row-down)
  (define-key array-mode-map "\M-\C-p" 'array-copy-row-up)
  (define-key array-mode-map "\M-\C-f" 'array-copy-column-forward)
  (define-key array-mode-map "\M-\C-b" 'array-copy-column-backward))

(put 'array-mode 'mode-class 'special)

(defun array-mode ()
  "Major mode for editing arrays.

  Array mode is a specialized mode for editing arrays.  An array is
considered to be a two-dimensional set of strings.  The strings are
NOT recognized as integers or real numbers.

  The array MUST reside at the top of the buffer.  

  TABs are not respected, and may be converted into spaces at any time.
Setting the variable 'respect-tabs to non-nil will prevent TAB conversion,
but will cause many functions to give errors if they encounter one.

  Upon entering array mode, you will be prompted for the values of
several variables.  Others will be calculated based on the values you
supply.  These variables are all local the the buffer.  Other buffer
in array mode may have different values assigned to the variables.
The variables are:

Variables you assign:
     max-row:          The number of rows in the array.
     max-column:       The number of columns in the array. 
     columns-per-line: The number of columns in the array per line of buffer.
     field-width:      The width of each field, in characters.
     rows-numbered:    A logical variable describing whether to ignore
                       row numbers in the buffer.

Variables which are calculated:
     line-length:      The number of characters in a buffer line.
     lines-per-row:    The number of buffer lines used to display each row.

  The following commands are available (an asterisk indicates it may
take a numeric prefix argument):

    *  	\\<array-mode-map>\\[array-forward-column]	  Move forward one column.
    *  	\\[array-backward-column]	  Move backward one column.
    *  	\\[array-next-row]	  Move down one row.
    *  	\\[array-previous-row]	  Move up one row.

    *   \\[array-copy-forward]	  Copy the current field into the column to the right.
    *   \\[array-copy-backward]	  Copy the current field into the column to the left.
    *   \\[array-copy-down]	  Copy the current field into the row below.
    *   \\[array-copy-up]	  Copy the current field into the row above.

    *   \\[array-copy-column-forward]   Copy the current column into the column to the right.
    *   \\[array-copy-column-backward]   Copy the current column into the column to the left.
    *   \\[array-copy-row-down]   Copy the current row into the row below.
    *   \\[array-copy-row-up]   Copy the current row into the row above.

        \\[array-fill-rectangle]   Copy the field at mark into every cell with row and column
                  between that of point and mark.

	\\[array-what-position]	  Display the current array row and column.
	\\[array-goto-cell]	  Go to a particular array cell.

	\\[array-make-template]	  Make a template for a new array.
	\\[array-reconfigure-rows]	  Reconfigure the array.
        \\[array-expand-rows]   Expand the array (remove row numbers and
                  newlines inside rows)

        \\[array-display-local-variables]   Display the current values of local variables.

Entering array mode calls the function `array-mode-hook'."

  (interactive)
  ;; Number of rows in the array.
  (make-local-variable 'max-row)    
  ;; Number of columns in the array.
  (make-local-variable 'max-column)    
  ;; Number of array columns per line.
  (make-local-variable 'columns-per-line) 
  ;; Width of a field in the array.
  (make-local-variable 'field-width)   
  ;; Are rows numbered in the buffer?
  (make-local-variable 'rows-numbered) 
  ;; Length of a line in the array.
  (make-local-variable 'line-length)   
  ;; Number of lines per array row.
  (make-local-variable 'lines-per-row) 
  ;; Current line number of point in the buffer.
  (make-local-variable 'buffer-line)
  ;; Current column number of point in the buffer.
  (make-local-variable 'buffer-column)
  ;; Current array row location of point.
  (make-local-variable 'array-row)
  ;; Current array column location of point.
  (make-local-variable 'array-column)
  ;; Current field string being copied.
  (make-local-variable 'copy-string)
  ;; Should TAB conversion be prevented?
  (make-local-variable 'respect-tabs)
  (setq respect-tabs nil)
  (array-init-local-variables)
  (setq major-mode 'array-mode)
  (setq mode-name "Array")
  (force-mode-line-update)
  (make-variable-buffer-local 'truncate-lines)
  (setq truncate-lines t)
  (setq overwrite-mode t)
  (use-local-map array-mode-map)
  (run-hooks 'array-mode-hook))



;;; Initialization functions.  These are not interactive.

(defun array-init-local-variables ()
  "Initialize the variables associated with the
array in this buffer."
  (array-init-max-row)
  (array-init-max-column)
  (array-init-columns-per-line)
  (array-init-field-width)
  (array-init-rows-numbered)
  (array-init-line-length)
  (array-init-lines-per-row)
  (message ""))

(defun array-init-max-row (&optional arg)
  "Initialize the value of max-row."
  (setq max-row
	(or arg (string-to-int (read-input "Number of array rows: ")))))

(defun array-init-max-column (&optional arg)
  "Initialize the value of max-column."
  (setq max-column
	(or arg (string-to-int (read-input "Number of array columns: ")))))

(defun array-init-columns-per-line (&optional arg)
  "Initialize the value of columns-per-line."
  (setq columns-per-line 
	(or arg (string-to-int (read-input "Array columns per line: ")))))

(defun array-init-field-width (&optional arg)
  "Initialize the value of field-width."
  (setq field-width 
	(or arg (string-to-int (read-input "Field width: ")))))

(defun array-init-rows-numbered (&optional arg)
  "Initialize the value of rows-numbered."
  (setq rows-numbered 
	(or arg (y-or-n-p "Rows numbered? "))))

(defun array-init-line-length (&optional arg)
  "Initialize the value of line-length."
  (setq line-length
	(or arg
	  (* field-width columns-per-line))))

(defun array-init-lines-per-row (&optional arg)
  "Initialize the value of lines-per-row."
  (setq lines-per-row
	(or arg
	  (+ (floor (1- max-column) columns-per-line)
	     (if rows-numbered 2 1)))))

;;; array.el ends here
