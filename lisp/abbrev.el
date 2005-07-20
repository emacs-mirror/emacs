;;; abbrev.el --- abbrev mode commands for Emacs

;; Copyright (C) 1985, 1986, 1987, 1992 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: abbrev convenience

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This facility is documented in the Emacs Manual.

;;; Code:

(defcustom only-global-abbrevs nil
  "Non-nil means user plans to use global abbrevs only.
This makes the commands that normally define mode-specific abbrevs
define global abbrevs instead."
  :type 'boolean
  :group 'abbrev-mode
  :group 'convenience)

(defun abbrev-mode (&optional arg)
  "Toggle Abbrev mode in the current buffer.
With argument ARG, turn abbrev mode on iff ARG is positive.
In Abbrev mode, inserting an abbreviation causes it to expand
and be replaced by its expansion."
  (interactive "P")
  (setq abbrev-mode
	(if (null arg) (not abbrev-mode)
	  (> (prefix-numeric-value arg) 0)))
  (force-mode-line-update))

(defcustom abbrev-mode nil
  "Enable or disable Abbrev mode.
Non-nil means automatically expand abbrevs as they are inserted.

Setting this variable with `setq' changes it for the current buffer.
Changing it with \\[customize] sets the default value.
Interactively, use the command `abbrev-mode'
to enable or disable Abbrev mode in the current buffer."
  :type 'boolean
  :group 'abbrev-mode)


(defvar edit-abbrevs-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-x\C-s" 'edit-abbrevs-redefine)
    (define-key map "\C-c\C-c" 'edit-abbrevs-redefine)
    map)
  "Keymap used in `edit-abbrevs'.")

(defun kill-all-abbrevs ()
  "Undefine all defined abbrevs."
  (interactive)
  (let ((tables abbrev-table-name-list))
    (while tables
      (clear-abbrev-table (symbol-value (car tables)))
      (setq tables (cdr tables)))))

(defun copy-abbrev-table (table)
  "Make a new abbrev-table with the same abbrevs as TABLE."
  (let ((new-table (make-abbrev-table)))
    (mapatoms
     (lambda (symbol)
       (define-abbrev new-table
	 (symbol-name symbol)
	 (symbol-value symbol)
	 (symbol-function symbol)))
     table)
    new-table))

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

(defun list-abbrevs (&optional local)
  "Display a list of defined abbrevs.
If LOCAL is non-nil, interactively when invoked with a
prefix arg, display only local, i.e. mode-specific, abbrevs.
Otherwise display all abbrevs."
  (interactive "P")
  (display-buffer (prepare-abbrev-list-buffer local)))

(defun abbrev-table-name (table)
  "Value is the name of abbrev table TABLE."
  (let ((tables abbrev-table-name-list)
	found)
    (while (and (not found) tables)
      (when (eq (symbol-value (car tables)) table)
	(setq found (car tables)))
      (setq tables (cdr tables)))
    found))

(defun prepare-abbrev-list-buffer (&optional local)
  (save-excursion
    (let ((table local-abbrev-table))
      (set-buffer (get-buffer-create "*Abbrevs*"))
      (erase-buffer)
      (if local
	  (insert-abbrev-table-description (abbrev-table-name table) t)
	(dolist (table abbrev-table-name-list)
	  (insert-abbrev-table-description table t)))
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (edit-abbrevs-mode)
      (current-buffer))))

(defun edit-abbrevs-mode ()
  "Major mode for editing the list of abbrev definitions.
\\{edit-abbrevs-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'edit-abbrevs-mode)
  (setq mode-name "Edit-Abbrevs")
  (use-local-map edit-abbrevs-map)
  (run-mode-hooks 'edit-abbrevs-mode-hook))

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
	     abbrevs name hook exp count sys)
	(forward-line 1)
	(while (progn (forward-line 1)
		      (not (eolp)))
	  (setq name (read buf) count (read buf))
	  (if (equal count '(sys))
	      (setq sys t count (read buf)))
	  (setq exp (read buf))
	  (skip-chars-backward " \t\n\f")
	  (setq hook (if (not (eolp)) (read buf)))
	  (skip-chars-backward " \t\n\f")
	  (setq abbrevs (cons (list name exp hook count sys) abbrevs)))
	(define-abbrev-table table abbrevs)))))

(defun read-abbrev-file (&optional file quietly)
  "Read abbrev definitions from file written with `write-abbrev-file'.
Optional argument FILE is the name of the file to read;
it defaults to the value of `abbrev-file-name'.
Optional second argument QUIETLY non-nil means don't display a message."
  (interactive "fRead abbrev file: ")
  (load (if (and file (> (length file) 0)) file abbrev-file-name)
	nil quietly)
  (setq abbrevs-changed nil))

(defun quietly-read-abbrev-file (&optional file)
  "Read abbrev definitions from file written with `write-abbrev-file'.
Optional argument FILE is the name of the file to read;
it defaults to the value of `abbrev-file-name'.
Does not display any message."
					;(interactive "fRead abbrev file: ")
  (read-abbrev-file file t))

(defun write-abbrev-file (&optional file)
  "Write all user-level abbrev definitions to a file of Lisp code.
This does not include system abbrevs; it includes only the abbrev tables
listed in listed in `abbrev-table-name-list'.
The file written can be loaded in another session to define the same abbrevs.
The argument FILE is the file name to write.  If omitted or nil, the file
specified in `abbrev-file-name' is used."
  (interactive
   (list
    (read-file-name "Write abbrev file: "
		    (file-name-directory (expand-file-name abbrev-file-name))
		    abbrev-file-name)))
  (or (and file (> (length file) 0))
      (setq file abbrev-file-name))
  (let ((coding-system-for-write 'emacs-mule))
    (with-temp-file file
      (insert ";;-*-coding: emacs-mule;-*-\n")
      (dolist (table
               ;; We sort the table in order to ease the automatic
               ;; merging of different versions of the user's abbrevs
               ;; file.  This is useful, for example, for when the
               ;; user keeps their home directory in a revision
               ;; control system, and is therefore keeping multiple
               ;; slightly-differing copies loosely synchronized.
               (sort (copy-sequence abbrev-table-name-list)
                     (lambda (s1 s2)
                       (string< (symbol-name s1)
                                (symbol-name s2)))))
	(insert-abbrev-table-description table nil)))))

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
		  (buffer-substring-no-properties
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

(defun inverse-add-mode-abbrev (n)
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
   "Mode" n))

(defun inverse-add-global-abbrev (n)
  "Define last word before point as a global (mode-independent) abbrev.
With prefix argument N, defines the Nth word before point.
This command uses the minibuffer to read the expansion.
Expands the abbreviation after defining it."
  (interactive "p")
  (inverse-add-abbrev global-abbrev-table "Global" n))

(defun inverse-add-abbrev (table type arg)
  (let (name exp start end)
    (save-excursion
      (forward-word (1+ (- arg)))
      (setq end (point))
      (backward-word 1)
      (setq start (point)
	    name (buffer-substring-no-properties start end)))

    (setq exp (read-string (format "%s expansion for \"%s\": " type name)
			   nil nil nil t))
    (when (or (not (abbrev-expansion name table))
	      (y-or-n-p (format "%s expands to \"%s\"; redefine? "
				name (abbrev-expansion name table))))
      (define-abbrev table (downcase name) exp)
      (save-excursion
	(goto-char end)
	(expand-abbrev)))))

(defun abbrev-prefix-mark (&optional arg)
  "Mark current point as the beginning of an abbrev.
Abbrev to be expanded starts here rather than at beginning of word.
This way, you can expand an abbrev with a prefix: insert the prefix,
use this command, then insert the abbrev.  This command inserts a
temporary hyphen after the prefix \(until the intended abbrev
expansion occurs).
If the prefix is itself an abbrev, this command expands it, unless
ARG is non-nil.  Interactively, ARG is the prefix argument."
  (interactive "P")
  (or arg (expand-abbrev))
  (setq abbrev-start-location (point-marker)
	abbrev-start-location-buffer (current-buffer))
  (insert "-"))

(defun expand-region-abbrevs (start end &optional noquery)
  "For abbrev occurrence in the region, offer to expand it.
The user is asked to type `y' or `n' for each occurrence.
A prefix argument means don't query; expand all abbrevs."
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
		   (buffer-substring-no-properties
		    (save-excursion (forward-word -1) (point))
		    pnt)))
	    (if (or noquery (y-or-n-p (format "Expand `%s'? " string)))
		(expand-abbrev)))))))

;;; arch-tag: dbd6f3ae-dfe3-40ba-b00f-f9e3ff960df5
;;; abbrev.el ends here
