;;; abbrev.el --- abbrev mode commands for Emacs

;; Copyright (C) 1985, 1986, 1987, 1992 Free Software Foundation, Inc.

;; Keywords: abbrev

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

;; This facility is documented in the Emacs Manual.

;;; Code:

(defconst only-global-abbrevs nil "\
*t means user plans to use global abbrevs only.
Makes the commands to define mode-specific abbrevs define global ones instead.")

(defun abbrev-mode (arg)
  "Toggle abbrev mode.
With argument ARG, turn abbrev mode on iff ARG is positive.
In abbrev mode, inserting an abbreviation causes it to expand
and be replaced by its expansion."
  (interactive "P")
  (setq abbrev-mode
	(if (null arg) (not abbrev-mode)
	  (> (prefix-numeric-value arg) 0)))
  (force-mode-line-update))

(defvar edit-abbrevs-map nil
  "Keymap used in edit-abbrevs.")
(if edit-abbrevs-map
    nil
  (setq edit-abbrevs-map (make-sparse-keymap))
  (define-key edit-abbrevs-map "\C-x\C-s" 'edit-abbrevs-redefine)
  (define-key edit-abbrevs-map "\C-c\C-c" 'edit-abbrevs-redefine))

(defun kill-all-abbrevs ()
  "Undefine all defined abbrevs."
  (interactive)
  (let ((tables abbrev-table-name-list))
    (while tables
      (clear-abbrev-table (symbol-value (car tables)))
      (setq tables (cdr tables)))))

(defun insert-abbrevs ()
  "Insert after point a description of all defined abbrevs.
Mark is set after the inserted text."
  (interactive)
  (push-mark
   (save-excursion
    (let ((tables abbrev-table-name-list))
      (while tables
	(insert-abbrev-table-description (car tables) t)
	(setq tables (cdr tables))))
    (point))))

(defun list-abbrevs ()
  "Display a list of all defined abbrevs."
  (interactive)
  (display-buffer (prepare-abbrev-list-buffer)))

(defun prepare-abbrev-list-buffer ()
  (save-excursion
    (set-buffer (get-buffer-create "*Abbrevs*"))
    (erase-buffer)
    (let ((tables abbrev-table-name-list))
      (while tables
	(insert-abbrev-table-description (car tables) t)
	(setq tables (cdr tables))))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (edit-abbrevs-mode))
  (get-buffer-create "*Abbrevs*"))

(defun edit-abbrevs-mode ()
  "Major mode for editing the list of abbrev definitions.
\\{edit-abbrevs-map}"
  (interactive)
  (setq major-mode 'edit-abbrevs-mode)
  (setq mode-name "Edit-Abbrevs")
  (use-local-map edit-abbrevs-map))

(defun edit-abbrevs ()
  "Alter abbrev definitions by editing a list of them.
Selects a buffer containing a list of abbrev definitions.
You can edit them and type \\<edit-abbrevs-map>\\[edit-abbrevs-redefine] to redefine abbrevs
according to your editing.
Buffer contains a header line for each abbrev table,
 which is the abbrev table name in parentheses.
This is followed by one line per abbrev in that table:
NAME   USECOUNT   EXPANSION   HOOK
where NAME and EXPANSION are strings with quotes,
USECOUNT is an integer, and HOOK is any valid function
or may be omitted (it is usually omitted)."
  (interactive)
  (switch-to-buffer (prepare-abbrev-list-buffer)))

(defun edit-abbrevs-redefine ()
  "Redefine abbrevs according to current buffer contents."
  (interactive)
  (define-abbrevs t)
  (set-buffer-modified-p nil))

(defun define-abbrevs (&optional arg)
  "Define abbrevs according to current visible buffer contents.
See documentation of `edit-abbrevs' for info on the format of the
text you must have in the buffer.
With argument, eliminate all abbrev definitions except
the ones defined from the buffer now."
  (interactive "P")
  (if arg (kill-all-abbrevs))
  (save-excursion
   (goto-char (point-min))
   (while (and (not (eobp)) (re-search-forward "^(" nil t))
     (let* ((buf (current-buffer))
	    (table (read buf))
	    abbrevs name hook exp count)
       (forward-line 1)
       (while (progn (forward-line 1)
		     (not (eolp)))
	 (setq name (read buf) count (read buf) exp (read buf))
	 (skip-chars-backward " \t\n\f")
	 (setq hook (if (not (eolp)) (read buf)))
	 (skip-chars-backward " \t\n\f")
	 (setq abbrevs (cons (list name exp hook count) abbrevs)))
       (define-abbrev-table table abbrevs)))))

(defun read-abbrev-file (&optional file quietly)
  "Read abbrev definitions from file written with `write-abbrev-file'.
Optional argument FILE is the name of the file to read;
it defaults to the value of `abbrev-file-name'.
Optional second argument QUIETLY non-nil means don't print anything."
  (interactive "fRead abbrev file: ")
  (load (if (and file (> (length file) 0)) file abbrev-file-name)
	nil quietly)
  (setq save-abbrevs t abbrevs-changed nil))

(defun quietly-read-abbrev-file (&optional file)
  "Read abbrev definitions from file written with write-abbrev-file.
Optional argument FILE is the name of the file to read;
it defaults to the value of `abbrev-file-name'.
Does not print anything."
  ;(interactive "fRead abbrev file: ")
  (read-abbrev-file file t))

(defun write-abbrev-file (file)
  "Write all abbrev definitions to a file of Lisp code.
The file written can be loaded in another session to define the same abbrevs.
The argument FILE is the file name to write."
  (interactive
   (list
    (read-file-name "Write abbrev file: "
		    (file-name-directory (expand-file-name abbrev-file-name))
		    abbrev-file-name)))
  (or (and file (> (length file) 0))
      (setq file abbrev-file-name))
  (save-excursion
   (set-buffer (get-buffer-create " write-abbrev-file"))
   (erase-buffer)
   (let ((tables abbrev-table-name-list))
     (while tables
       (insert-abbrev-table-description (car tables) nil)
       (setq tables (cdr tables))))
   (write-region 1 (point-max) file)
   (erase-buffer)))

(defun add-mode-abbrev (arg)
  "Define mode-specific abbrev for last word(s) before point.
Argument is how many words before point form the expansion;
or zero means the region is the expansion.
A negative argument means to undefine the specified abbrev.
Reads the abbreviation in the minibuffer.

Don't use this function in a Lisp program; use `define-abbrev' instead."
  (interactive "p")
  (add-abbrev
   (if only-global-abbrevs
       global-abbrev-table 
     (or local-abbrev-table
	 (error "No per-mode abbrev table")))
   "Mode" arg))

(defun add-global-abbrev (arg)
  "Define global (all modes) abbrev for last word(s) before point.
The prefix argument specifies the number of words before point that form the
expansion; or zero means the region is the expansion.
A negative argument means to undefine the specified abbrev.
This command uses the minibuffer to read the abbreviation.

Don't use this function in a Lisp program; use `define-abbrev' instead."
  (interactive "p")
  (add-abbrev global-abbrev-table "Global" arg))

(defun add-abbrev (table type arg)
  (let ((exp (and (>= arg 0)
		  (buffer-substring
		   (point)
		   (if (= arg 0) (mark)
		     (save-excursion (forward-word (- arg)) (point))))))
	name)
    (setq name
	  (read-string (format (if exp "%s abbrev for \"%s\": "
				 "Undefine %s abbrev: ")
			       type exp)))
    (set-text-properties 0 (length name) nil name)
    (if (or (null exp)
	    (not (abbrev-expansion name table))
	    (y-or-n-p (format "%s expands to \"%s\"; redefine? "
			      name (abbrev-expansion name table))))
	(define-abbrev table (downcase name) exp))))
	
(defun inverse-add-mode-abbrev (arg)
  "Define last word before point as a mode-specific abbrev.
With prefix argument N, defines the Nth word before point.
This command uses the minibuffer to read the expansion.
Expands the abbreviation after defining it."
  (interactive "p")
  (inverse-add-abbrev
   (if only-global-abbrevs
       global-abbrev-table 
     (or local-abbrev-table
	 (error "No per-mode abbrev table")))
   "Mode" arg))

(defun inverse-add-global-abbrev (arg)
  "Define last word before point as a global (mode-independent) abbrev.
With prefix argument N, defines the Nth word before point.
This command uses the minibuffer to read the expansion.
Expands the abbreviation after defining it."
  (interactive "p")
  (inverse-add-abbrev global-abbrev-table "Global" arg))

(defun inverse-add-abbrev (table type arg)
  (let (name nameloc exp)
    (save-excursion
     (forward-word (- arg))
     (setq name (buffer-substring (point) (progn (forward-word 1)
					       (setq nameloc (point))))))
    (set-text-properties 0 (length name) nil name)
    (setq exp (read-string (format "%s expansion for \"%s\": "
				   type name)))
    (if (or (not (abbrev-expansion name table))
	    (y-or-n-p (format "%s expands to \"%s\"; redefine? "
			      name (abbrev-expansion name table))))
	(progn
	 (define-abbrev table (downcase name) exp)
	 (save-excursion
	  (goto-char nameloc)
	  (expand-abbrev))))))

(defun abbrev-prefix-mark (&optional arg)
  "Mark current point as the beginning of an abbrev.
Abbrev to be expanded starts here rather than at beginning of word.
This way, you can expand an abbrev with a prefix: insert the prefix,
use this command, then insert the abbrev."
  (interactive "P")
  (or arg (expand-abbrev))
  (setq abbrev-start-location (point-marker)
	abbrev-start-location-buffer (current-buffer))
  (insert "-"))

(defun expand-region-abbrevs (start end &optional noquery)
  "For abbrev occurrence in the region, offer to expand it.
The user is asked to type y or n for each occurrence.
A prefix argument means don't query; expand all abbrevs.
If called from a Lisp program, arguments are START END &optional NOQUERY."
  (interactive "r\nP")
  (save-excursion
    (goto-char start)
    (let ((lim (- (point-max) end))
	  pnt string)
      (while (and (not (eobp))
		  (progn (forward-word 1)
			 (<= (setq pnt (point)) (- (point-max) lim))))
	(if (abbrev-expansion
	     (setq string
		   (buffer-substring
		    (save-excursion (forward-word -1) (point))
		    pnt)))
	    (if (or noquery (y-or-n-p (format "Expand `%s'? " string)))
		(expand-abbrev)))))))

;;; abbrev.el ends here
