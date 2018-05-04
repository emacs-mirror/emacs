;;; tabulated-list.el --- generic major mode for tabulated lists -*- lexical-binding: t -*-

;; Copyright (C) 2011-2018 Free Software Foundation, Inc.

;; Author: Chong Yidong <cyd@stupidchicken.com>
;; Keywords: extensions, lisp
;; Version: 1.0

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

;; This file defines Tabulated List mode, a generic major mode for
;; displaying lists of tabulated data, intended for other major modes
;; to inherit from.  It provides several utility routines, e.g. for
;; pretty-printing lines of tabulated data to fit into the appropriate
;; columns.

;; For usage information, see the documentation of `tabulated-list-mode'.

;; This package originated from Tom Tromey's Package Menu mode,
;; extended and generalized to be used by other modes.

;;; Code:

;; The reason `tabulated-list-format' and other variables are
;; permanent-local is to make it convenient to switch to a different
;; major mode, switch back, and have the original Tabulated List data
;; still valid.  See, for example, ebuff-menu.el.

(defvar-local tabulated-list-format nil
  "The format of the current Tabulated List mode buffer.
This should be a vector of elements (NAME WIDTH SORT . PROPS),
where:
 - NAME is a string describing the column.
   This is the label for the column in the header line.
   Different columns must have non-`equal' names.
 - WIDTH is the width to reserve for the column.
   For the final element, its numerical value is ignored.
 - SORT specifies how to sort entries by this column.
   If nil, this column cannot be used for sorting.
   If t, sort by comparing the string value printed in the column.
   Otherwise, it should be a predicate function suitable for
   `sort', accepting arguments with the same form as the elements
   of `tabulated-list-entries'.
 - PROPS is a plist of additional column properties.
   Currently supported properties are:
   - `:right-align': If non-nil, the column should be right-aligned.
   - `:pad-right': Number of additional padding spaces to the
     right of the column (defaults to 1 if omitted).")
(put 'tabulated-list-format 'permanent-local t)

(defvar-local tabulated-list-use-header-line t
  "Whether the Tabulated List buffer should use a header line.")

(defvar-local tabulated-list-entries nil
  "Entries displayed in the current Tabulated List buffer.
This should be either a function, or a list.
If a list, each element has the form (ID [DESC1 ... DESCN]),
where:
 - ID is nil, or a Lisp object uniquely identifying this entry,
   which is used to keep the cursor on the \"same\" entry when
   rearranging the list.  Comparison is done with `equal'.

 - Each DESC is a column descriptor, one for each column
   specified in `tabulated-list-format'.  A descriptor is either
   a string, which is printed as-is, or a list (LABEL . PROPS),
   which means to use `insert-text-button' to insert a text
   button with label LABEL and button properties PROPS.
   The string, or button label, must not contain any newline.

If `tabulated-list-entries' is a function, it is called with no
arguments and must return a list of the above form.")
(put 'tabulated-list-entries 'permanent-local t)

(defvar-local tabulated-list-padding 0
  "Number of characters preceding each Tabulated List mode entry.
By default, lines are padded with spaces, but you can use the
function `tabulated-list-put-tag' to change this.")
(put 'tabulated-list-padding 'permanent-local t)

(defvar tabulated-list-revert-hook nil
  "Hook run before reverting a Tabulated List buffer.
This is commonly used to recompute `tabulated-list-entries'.")

(defvar-local tabulated-list-printer 'tabulated-list-print-entry
  "Function for inserting a Tabulated List entry at point.
It is called with two arguments, ID and COLS.  ID is a Lisp
object identifying the entry, and COLS is a vector of column
descriptors, as documented in `tabulated-list-entries'.")

(defvar tabulated-list--near-rows)

(defvar-local tabulated-list-sort-key nil
  "Sort key for the current Tabulated List mode buffer.
If nil, no additional sorting is performed.
Otherwise, this should be a cons cell (NAME . FLIP).
NAME is a string matching one of the column names in
`tabulated-list-format' (the corresponding SORT entry in
`tabulated-list-format' then specifies how to sort).  FLIP, if
non-nil, means to invert the resulting sort.")
(put 'tabulated-list-sort-key 'permanent-local t)

(defsubst tabulated-list-get-id (&optional pos)
  "Return the entry ID of the Tabulated List entry at POS.
The value is an ID object from `tabulated-list-entries', or nil.
POS, if omitted or nil, defaults to point."
  (get-text-property (or pos (point)) 'tabulated-list-id))

(defsubst tabulated-list-get-entry (&optional pos)
  "Return the Tabulated List entry at POS.
The value is a vector of column descriptors, or nil if there is
no entry at POS.  POS, if omitted or nil, defaults to point."
  (get-text-property (or pos (point)) 'tabulated-list-entry))

(defun tabulated-list-put-tag (tag &optional advance)
  "Put TAG in the padding area of the current line.
TAG should be a string, with length <= `tabulated-list-padding'.
If ADVANCE is non-nil, move forward by one line afterwards."
  (unless (stringp tag)
    (error "Invalid argument to `tabulated-list-put-tag'"))
  (unless (> tabulated-list-padding 0)
    (error "Unable to tag the current line"))
  (save-excursion
    (beginning-of-line)
    (when (tabulated-list-get-entry)
      (let ((beg (point))
	    (inhibit-read-only t))
	(forward-char tabulated-list-padding)
	(insert-and-inherit
	 (let ((width (string-width tag)))
	   (if (<= width tabulated-list-padding)
	       (concat tag
		       (make-string (- tabulated-list-padding width) ?\s))
	     (truncate-string-to-width tag tabulated-list-padding))))
	(delete-region beg (+ beg tabulated-list-padding)))))
  (if advance
      (forward-line)))

(defvar tabulated-list-mode-map
  (let ((map (copy-keymap special-mode-map)))
    (set-keymap-parent map button-buffer-map)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "S" 'tabulated-list-sort)
    (define-key map [follow-link] 'mouse-face)
    (define-key map [mouse-2] 'mouse-select-window)
    map)
  "Local keymap for `tabulated-list-mode' buffers.")

(defvar tabulated-list-sort-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] 'tabulated-list-col-sort)
    (define-key map [header-line mouse-2] 'tabulated-list-col-sort)
    (define-key map [mouse-1] 'tabulated-list-col-sort)
    (define-key map [mouse-2] 'tabulated-list-col-sort)
    (define-key map "\C-m" 'tabulated-list-sort)
    (define-key map [follow-link] 'mouse-face)
    map)
  "Local keymap for `tabulated-list-mode' sort buttons.")

(defvar tabulated-list-glyphless-char-display
  (let ((table (make-char-table 'glyphless-char-display nil)))
    (set-char-table-parent table glyphless-char-display)
    ;; Some text terminals can't display the Unicode arrows; be safe.
    (aset table 9650 (cons nil "^"))
    (aset table 9660 (cons nil "v"))
    table)
  "The `glyphless-char-display' table in Tabulated List buffers.")

(defvar tabulated-list--header-string nil
  "Holds the header if `tabulated-list-use-header-line' is nil.
Populated by `tabulated-list-init-header'.")
(defvar tabulated-list--header-overlay nil)

(defun tabulated-list-line-number-width ()
  "Return the width taken by display-line-numbers in the current buffer."
  ;; line-number-display-width returns the value for the selected
  ;; window, which might not be the window in which the current buffer
  ;; is displayed.
  (if (not display-line-numbers)
           0
    (let ((cbuf-window (get-buffer-window (current-buffer) t)))
      (if (window-live-p cbuf-window)
          (with-selected-window cbuf-window
            (line-number-display-width 'columns))
        4))))

(defun tabulated-list-init-header ()
  "Set up header line for the Tabulated List buffer."
  ;; FIXME: Should share code with tabulated-list-print-col!
  (let ((x (max tabulated-list-padding 0))
	(button-props `(help-echo "Click to sort by column"
			mouse-face header-line-highlight
			keymap ,tabulated-list-sort-button-map))
	(cols nil))
    (if display-line-numbers
        (setq x (+ x (tabulated-list-line-number-width))))
    (push (propertize " " 'display `(space :align-to ,x)) cols)
    (dotimes (n (length tabulated-list-format))
      (let* ((col (aref tabulated-list-format n))
	     (label (nth 0 col))
	     (width (nth 1 col))
	     (props (nthcdr 3 col))
	     (pad-right (or (plist-get props :pad-right) 1))
             (right-align (plist-get props :right-align))
             (next-x (+ x pad-right width)))
	(push
	 (cond
	  ;; An unsortable column
	  ((not (nth 2 col))
	   (propertize label 'tabulated-list-column-name label))
	  ;; The selected sort column
	  ((equal (car col) (car tabulated-list-sort-key))
	   (apply 'propertize
		  (concat label
			  (cond
			   ((> (+ 2 (length label)) width) "")
			   ((cdr tabulated-list-sort-key) " ▲")
			   (t " ▼")))
		  'face 'bold
		  'tabulated-list-column-name label
		  button-props))
	  ;; Unselected sortable column.
	  (t (apply 'propertize label
		    'tabulated-list-column-name label
		    button-props)))
	 cols)
        (when right-align
          (let ((shift (- width (string-width (car cols)))))
            (when (> shift 0)
              (setq cols
                    (cons (car cols)
                          (cons (propertize (make-string shift ?\s)
                                            'display
                                            `(space :align-to ,(+ x shift)))
                                (cdr cols))))
              (setq x (+ x shift)))))
	(if (>= pad-right 0)
	    (push (propertize " "
			      'display `(space :align-to ,next-x)
			      'face 'fixed-pitch)
		  cols))
        (setq x next-x)))
    (setq cols (apply 'concat (nreverse cols)))
    (if tabulated-list-use-header-line
	(setq header-line-format cols)
      (setq header-line-format nil)
      (setq-local tabulated-list--header-string cols))))

(defun tabulated-list-print-fake-header ()
  "Insert a fake Tabulated List \"header line\" at the start of the buffer.
Do nothing if `tabulated-list--header-string' is nil."
  (when tabulated-list--header-string
    (goto-char (point-min))
    (let ((inhibit-read-only t))
      (insert tabulated-list--header-string "\n")
      (if tabulated-list--header-overlay
          (move-overlay tabulated-list--header-overlay (point-min) (point))
        (setq-local tabulated-list--header-overlay
                    (make-overlay (point-min) (point))))
      (overlay-put tabulated-list--header-overlay 'face 'underline))))

(defsubst tabulated-list-header-overlay-p (&optional pos)
  "Return non-nil if there is a fake header.
Optional arg POS is a buffer position where to look for a fake header;
defaults to `point-min'."
  (overlays-at (or pos (point-min))))

(defun tabulated-list-revert (&rest ignored)
  "The `revert-buffer-function' for `tabulated-list-mode'.
It runs `tabulated-list-revert-hook', then calls `tabulated-list-print'."
  (interactive)
  (unless (derived-mode-p 'tabulated-list-mode)
    (error "The current buffer is not in Tabulated List mode"))
  (run-hooks 'tabulated-list-revert-hook)
  (tabulated-list-print t))

(defun tabulated-list--column-number (name)
  (let ((len (length tabulated-list-format))
	(n 0)
	found)
    (while (and (< n len) (null found))
      (if (equal (car (aref tabulated-list-format n)) name)
	  (setq found n))
      (setq n (1+ n)))
    (or found
	(error "No column named %s" name))))

(defun tabulated-list--get-sorter ()
  "Return a sorting predicate for the current tabulated-list.
Return nil if `tabulated-list-sort-key' specifies an unsortable
column.  Negate the predicate that would be returned if
`tabulated-list-sort-key' has a non-nil cdr."
  (when (and tabulated-list-sort-key
             (car tabulated-list-sort-key))
    (let* ((sort-column (car tabulated-list-sort-key))
           (n (tabulated-list--column-number sort-column))
           (sorter (nth 2 (aref tabulated-list-format n))))
      (when (eq sorter t); Default sorter checks column N:
        (setq sorter (lambda (A B)
                       (let ((a (aref (cadr A) n))
                             (b (aref (cadr B) n)))
                         (string< (if (stringp a) a (car a))
                                  (if (stringp b) b (car b)))))))
      ;; Reversed order.
      (if (cdr tabulated-list-sort-key)
          (lambda (a b) (not (funcall sorter a b)))
        sorter))))

(defsubst tabulated-list--col-local-max-widths (col)
   "Return maximum entry widths at column COL around current row.
Check the current row, the previous one and the next row."
  (apply #'max (mapcar (lambda (x)
                         (let ((nt (elt x col)))
                           (string-width (if (stringp nt) nt (car nt)))))
                       tabulated-list--near-rows)))

(defun tabulated-list-print (&optional remember-pos update)
  "Populate the current Tabulated List mode buffer.
This sorts the `tabulated-list-entries' list if sorting is
specified by `tabulated-list-sort-key'.  It then erases the
buffer and inserts the entries with `tabulated-list-printer'.

Optional argument REMEMBER-POS, if non-nil, means to move point
to the entry with the same ID element as the current line and
recenter window line accordingly.

Non-nil UPDATE argument means to use an alternative printing
method which is faster if most entries haven't changed since the
last print.  The only difference in outcome is that tags will not
be removed from entries that haven't changed (see
`tabulated-list-put-tag').  Don't use this immediately after
changing `tabulated-list-sort-key'."
  (let ((inhibit-read-only t)
	(entries (if (functionp tabulated-list-entries)
		     (funcall tabulated-list-entries)
		   tabulated-list-entries))
        (sorter (tabulated-list--get-sorter))
	entry-id saved-pt saved-col window-line)
    (and remember-pos
	 (setq entry-id (tabulated-list-get-id))
	 (setq saved-col (current-column))
         (when (eq (window-buffer) (current-buffer))
           (setq window-line
                 (count-screen-lines (window-start) (point)))))
    ;; Sort the entries, if necessary.
    (when sorter
      (setq entries (sort entries sorter)))
    (unless (functionp tabulated-list-entries)
      (setq tabulated-list-entries entries))
    ;; Without a sorter, we have no way to just update.
    (when (and update (not sorter))
      (setq update nil))
    (if update (goto-char (point-min))
      ;; Redo the buffer, unless we're just updating.
      (erase-buffer)
      (unless tabulated-list-use-header-line
        (tabulated-list-print-fake-header)))
    ;; Finally, print the resulting list.
    (while entries
      (let* ((elt (car entries))
             (tabulated-list--near-rows
              (list
               (or (tabulated-list-get-entry (point-at-bol 0)) (cadr elt))
               (cadr elt)
               (or (cadr (cadr entries)) (cadr elt))))
             (id (car elt)))
        (and entry-id
             (equal entry-id id)
             (setq entry-id nil
                   saved-pt (point)))
        ;; If the buffer is empty, simply print each elt.
        (if (or (not update) (eobp))
            (apply tabulated-list-printer elt)
          (while (let ((local-id (tabulated-list-get-id)))
                   ;; If we find id, then nothing to update.
                   (cond ((equal id local-id)
                          (forward-line 1)
                          nil)
                         ;; If this entry sorts after id (or it's the
                         ;; end), then just insert id and move on.
                         ((or (not local-id)
                              (funcall sorter elt
                                       ;; FIXME: Might be faster if
                                       ;; don't construct this list.
                                       (list local-id (tabulated-list-get-entry))))
                          (apply tabulated-list-printer elt)
                          nil)
                         ;; We find an entry that sorts before id,
                         ;; it needs to be deleted.
                         (t t)))
            (let ((old (point)))
              (forward-line 1)
              (delete-region old (point))))))
      (setq entries (cdr entries)))
    (set-buffer-modified-p nil)
    ;; If REMEMBER-POS was specified, move to the "old" location.
    (if saved-pt
	(progn (goto-char saved-pt)
	       (move-to-column saved-col)
	       (when window-line
                 (recenter window-line)))
      (goto-char (point-min)))))

(defun tabulated-list-print-entry (id cols)
  "Insert a Tabulated List entry at point.
This is the default `tabulated-list-printer' function.  ID is a
Lisp object identifying the entry to print, and COLS is a vector
of column descriptors."
  (let ((beg   (point))
	(x     (max tabulated-list-padding 0))
	(ncols (length tabulated-list-format))
	(inhibit-read-only t))
    (if (> tabulated-list-padding 0)
	(insert (make-string x ?\s)))
    (let ((tabulated-list--near-rows ; Bind it if not bound yet (Bug#25506).
           (or (bound-and-true-p tabulated-list--near-rows)
               (list (or (tabulated-list-get-entry (point-at-bol 0))
                         cols)
                     cols))))
      (dotimes (n ncols)
        (setq x (tabulated-list-print-col n (aref cols n) x))))
    (insert ?\n)
    ;; Ever so slightly faster than calling `put-text-property' twice.
    (add-text-properties
     beg (point)
     `(tabulated-list-id ,id tabulated-list-entry ,cols))))

(defun tabulated-list-print-col (n col-desc x)
  "Insert a specified Tabulated List entry at point.
N is the column number, COL-DESC is a column descriptor (see
`tabulated-list-entries'), and X is the column number at point.
Return the column number after insertion."
  (let* ((format    (aref tabulated-list-format n))
	 (name      (nth 0 format))
	 (width     (nth 1 format))
	 (props     (nthcdr 3 format))
	 (pad-right (or (plist-get props :pad-right) 1))
         (right-align (plist-get props :right-align))
	 (label     (if (stringp col-desc) col-desc (car col-desc)))
         (label-width (string-width label))
	 (help-echo (concat (car format) ": " label))
	 (opoint (point))
	 (not-last-col (< (1+ n) (length tabulated-list-format)))
	  available-space)
    (when not-last-col
      (let* ((next-col-format (aref tabulated-list-format (1+ n)))
             (next-col-right-align (plist-get (nthcdr 3 next-col-format)
                                              :right-align))
             (next-col-width (nth 1 next-col-format)))
        (setq available-space
              (if (and (not right-align)
                       next-col-right-align)
                  (-
                   (+ width next-col-width)
                   (min next-col-width
                        (tabulated-list--col-local-max-widths (1+ n))))
                width))))
    ;; Truncate labels if necessary (except last column).
    ;; Don't truncate to `width' if the next column is align-right
    ;; and has some space left, truncate to `available-space' instead.
    (when (and not-last-col
               (> label-width available-space)
               (setq label (truncate-string-to-width
                            label available-space nil nil t)
                     label-width available-space)))
    (setq label (bidi-string-mark-left-to-right label))
    (when (and right-align (> width label-width))
      (let ((shift (- width label-width)))
        (insert (propertize (make-string shift ?\s)
                            'display `(space :align-to ,(+ x shift))))
        (setq width (- width shift))
        (setq x (+ x shift))))
    (if (stringp col-desc)
	(insert (if (get-text-property 0 'help-echo label)
		    label
		  (propertize label 'help-echo help-echo)))
      (apply 'insert-text-button label (cdr col-desc)))
    (let ((next-x (+ x pad-right width)))
      ;; No need to append any spaces if this is the last column.
      (when not-last-col
        (when (> pad-right 0) (insert (make-string pad-right ?\s)))
        (insert (propertize
                 (make-string (- width (min width label-width)) ?\s)
                 'display `(space :align-to ,next-x))))
      (put-text-property opoint (point) 'tabulated-list-column-name name)
      next-x)))

(defun tabulated-list-delete-entry ()
  "Delete the Tabulated List entry at point.
Return a list (ID COLS), where ID is the ID of the deleted entry
and COLS is a vector of its column descriptors.  Move point to
the beginning of the deleted entry.  Return nil if there is no
entry at point.

This function only changes the buffer contents; it does not alter
`tabulated-list-entries'."
  ;; Assume that each entry occupies one line.
  (let* ((id (tabulated-list-get-id))
	 (cols (tabulated-list-get-entry))
	 (inhibit-read-only t))
    (when cols
      (delete-region (line-beginning-position) (1+ (line-end-position)))
      (list id cols))))

(defun tabulated-list-set-col (col desc &optional change-entry-data)
  "Change the Tabulated List entry at point, setting COL to DESC.
COL is the column number to change, or the name of the column to change.
DESC is the new column descriptor, which is inserted via
`tabulated-list-print-col'.

If CHANGE-ENTRY-DATA is non-nil, modify the underlying entry data
by setting the appropriate slot of the vector originally used to
print this entry.  If `tabulated-list-entries' has a list value,
this is the vector stored within it."
  (let* ((opoint (point))
	 (eol    (line-end-position))
	 (pos    (line-beginning-position))
	 (id     (tabulated-list-get-id pos))
	 (entry  (tabulated-list-get-entry pos))
	 (prop 'tabulated-list-column-name)
	 (inhibit-read-only t)
	 name)
    (cond ((numberp col)
	   (setq name (car (aref tabulated-list-format col))))
	  ((stringp col)
	   (setq name col
		 col (tabulated-list--column-number col)))
	  (t
	   (error "Invalid column %s" col)))
    (unless entry
      (error "No Tabulated List entry at position %s" opoint))
    (unless (equal (get-text-property pos prop) name)
      (while (and (setq pos
			(next-single-property-change pos prop nil eol))
		  (< pos eol)
		  (not (equal (get-text-property pos prop) name)))))
    (when (< pos eol)
      (delete-region pos (next-single-property-change pos prop nil eol))
      (goto-char pos)
      (let ((tabulated-list--near-rows
             (list
              (tabulated-list-get-entry (point-at-bol 0))
              entry
              (or (tabulated-list-get-entry (point-at-bol 2)) entry))))
        (tabulated-list-print-col col desc (current-column)))
      (if change-entry-data
	  (aset entry col desc))
      (put-text-property pos (point) 'tabulated-list-id id)
      (put-text-property pos (point) 'tabulated-list-entry entry)
      (goto-char opoint))))

(defun tabulated-list-col-sort (&optional e)
  "Sort Tabulated List entries by the column of the mouse click E."
  (interactive "e")
  (let* ((pos (event-start e))
	 (obj (posn-object pos)))
    (with-current-buffer (window-buffer (posn-window pos))
      (tabulated-list--sort-by-column-name
       (get-text-property (if obj (cdr obj) (posn-point pos))
			  'tabulated-list-column-name
			  (car obj))))))

(defun tabulated-list-sort (&optional n)
  "Sort Tabulated List entries by the column at point.
With a numeric prefix argument N, sort the Nth column."
  (interactive "P")
  (let ((name (if n
		  (car (aref tabulated-list-format n))
		(get-text-property (point)
				   'tabulated-list-column-name))))
    (if (nth 2 (assoc name (append tabulated-list-format nil)))
        (tabulated-list--sort-by-column-name name)
      (user-error "Cannot sort by %s" name))))

(defun tabulated-list--sort-by-column-name (name)
  (when (and name (derived-mode-p 'tabulated-list-mode))
    ;; Flip the sort order on a second click.
    (if (equal name (car tabulated-list-sort-key))
	(setcdr tabulated-list-sort-key
		(not (cdr tabulated-list-sort-key)))
      (setq tabulated-list-sort-key (cons name nil)))
    (tabulated-list-init-header)
    (tabulated-list-print t)))

(defvar tabulated-list--current-lnum-width nil)
(defun tabulated-list-watch-line-number-width (_window)
  (if display-line-numbers
      (let ((lnum-width (tabulated-list-line-number-width)))
        (when (not (= tabulated-list--current-lnum-width lnum-width))
          (setq-local tabulated-list--current-lnum-width lnum-width)
          (tabulated-list-init-header)))))

(defun tabulated-list-window-scroll-function (window _start)
  (if display-line-numbers
      (let ((lnum-width
             (with-selected-window window
               (line-number-display-width 'columns))))
        (when (not (= tabulated-list--current-lnum-width lnum-width))
          (setq-local tabulated-list--current-lnum-width lnum-width)
          (tabulated-list-init-header)))))

;;; The mode definition:

(define-derived-mode tabulated-list-mode special-mode "Tabulated"
  "Generic major mode for browsing a list of items.
This mode is usually not used directly; instead, other major
modes are derived from it, using `define-derived-mode'.

In this major mode, the buffer is divided into multiple columns,
which are labeled using the header line.  Each non-empty line
belongs to one \"entry\", and the entries can be sorted according
to their column values.

An inheriting mode should usually do the following in their body:

 - Set `tabulated-list-format', specifying the column format.
 - Set `tabulated-list-revert-hook', if the buffer contents need
   to be specially recomputed prior to `revert-buffer'.
 - Maybe set a `tabulated-list-entries' function (see below).
 - Maybe set `tabulated-list-printer' (see below).
 - Maybe set `tabulated-list-padding'.
 - Call `tabulated-list-init-header' to initialize `header-line-format'
   according to `tabulated-list-format'.

An inheriting mode is usually accompanied by a \"list-FOO\"
command (e.g. `list-packages', `list-processes').  This command
creates or switches to a buffer and enables the major mode in
that buffer.  If `tabulated-list-entries' is not a function, the
command should initialize it to a list of entries for displaying.
Finally, it should call `tabulated-list-print'.

`tabulated-list-print' calls the printer function specified by
`tabulated-list-printer', once for each entry.  The default
printer is `tabulated-list-print-entry', but a mode that keeps
data in an ewoc may instead specify a printer function (e.g., one
that calls `ewoc-enter-last'), with `tabulated-list-print-entry'
as the ewoc pretty-printer."
  (setq-local truncate-lines t)
  (setq-local buffer-undo-list t)
  (setq-local revert-buffer-function #'tabulated-list-revert)
  (setq-local glyphless-char-display tabulated-list-glyphless-char-display)
  ;; Avoid messing up the entries' display just because the first
  ;; column of the first entry happens to begin with a R2L letter.
  (setq bidi-paragraph-direction 'left-to-right)
  ;; This is for if/when they turn on display-line-numbers
  (add-hook 'display-line-numbers-mode-hook #'tabulated-list-revert nil t)
  ;; This is for if/when they customize the line-number face or when
  ;; the line-number width needs to change due to scrolling.
  (setq-local tabulated-list--current-lnum-width 0)
  (add-hook 'pre-redisplay-functions
            #'tabulated-list-watch-line-number-width nil t)
  (add-hook 'window-scroll-functions
            #'tabulated-list-window-scroll-function nil t))

(put 'tabulated-list-mode 'mode-class 'special)

(provide 'tabulated-list)

;;; tabulated-list.el ends here
