;;; derived.el -- allow inheritance of major modes.
;;; (formerly mode-clone.el)

;; Copyright (C) 1993, 1994 Free Software Foundation, Inc.

;; Author: David Megginson (dmeggins@aix1.uottawa.ca)
;; Maintainer: FSF

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

;; GNU Emacs is already, in a sense, object oriented -- each object
;; (buffer) belongs to a class (major mode), and that class defines
;; the relationship between messages (input events) and methods
;; (commands) by means of a keymap.
;;
;; The only thing missing is a good scheme of inheritance.  It is
;; possible to simulate a single level of inheritance with generous
;; use of hooks and a bit of work -- sgml-mode, for example, also runs
;; the hooks for text-mode, and keymaps can inherit from other keymaps
;; -- but generally, each major mode ends up reinventing the wheel.
;; Ideally, someone should redesign all of Emacs's major modes to
;; follow a more conventional object-oriented system: when defining a
;; new major mode, the user should need only to name the existing mode
;; it is most similar to, then list the (few) differences.
;;
;; In the mean time, this package offers most of the advantages of
;; full inheritance with the existing major modes.  The macro
;; `define-derived-mode' allows the user to make a variant of an existing
;; major mode, with its own keymap.  The new mode will inherit the key
;; bindings of its parent, and will, in fact, run its parent first
;; every time it is called.  For example, the commands
;;
;;  (define-derived-mode hypertext-mode text-mode "Hypertext"
;;    "Major mode for hypertext.\n\n\\{hypertext-mode-map}"
;;    (setq case-fold-search nil))
;;
;;  (define-key hypertext-mode-map [down-mouse-3] 'do-hyper-link)
;;
;; will create a function `hypertext-mode' with its own (sparse)
;; keymap `hypertext-mode-map.'  The command M-x hypertext-mode will
;; perform the following actions:
;;
;; - run the command (text-mode) to get its default setup
;; - replace the current keymap with 'hypertext-mode-map,' which will
;;   inherit from 'text-mode-map'.
;; - replace the current syntax table with
;;   'hypertext-mode-syntax-table', which will borrow its defaults
;;   from the current text-mode-syntax-table.
;; - replace the current abbrev table with
;;   'hypertext-mode-abbrev-table', which will borrow its defaults
;;   from the current text-mode-abbrev table
;; - change the mode line to read "Hypertext"
;; - assign the value 'hypertext-mode' to the 'major-mode' variable
;; - run the body of commands provided in the macro -- in this case,
;;   set the local variable `case-fold-search' to nil.
;; - **run the command (hypertext-mode-setup), which is empty by
;;   default, but may be redefined by the user to contain special
;;   commands (ie. setting local variables like 'outline-regexp')
;;   **NOTE: do not use this option -- it will soon be obsolete.
;; - run anything assigned to 'hypertext-mode-hooks' (obsolete, but
;;   supported for the sake of compatibility).
;;
;; The advantages of this system are threefold.  First, text mode is
;; untouched -- if you had added the new keystroke to `text-mode-map,'
;; possibly using hooks, you would have added it to all text buffers
;; -- here, it appears only in hypertext buffers, where it makes
;; sense.  Second, it is possible to build even further, and make
;; a derived mode from a derived mode.  The commands
;;
;;   (define-derived-mode html-mode hypertext-mode "HTML")
;;   [various key definitions]
;; 
;; will add a new major mode for HTML with very little fuss.
;;
;; Note also the function `derived-mode-class,' which returns the non-derived
;; major mode which a derived mode is based on (ie. NOT necessarily the
;; immediate parent).
;;
;; (derived-mode-class 'text-mode) ==> text-mode
;; (derived-mode-class 'hypertext-mode) ==> text-mode
;; (derived-mode-class 'html-mode) ==> text-mode

;;; Code:

;; PUBLIC: define a new major mode which inherits from an existing one.

;;;###autoload
(defmacro define-derived-mode (child parent name &optional docstring &rest body)
  "Create a new mode as a variant of an existing mode.

The arguments to this command are as follow:

CHILD:     the name of the command for the derived mode.
PARENT:    the name of the command for the parent mode (ie. text-mode).
NAME:      a string which will appear in the status line (ie. \"Hypertext\")
DOCSTRING: an optional documentation string--if you do not supply one,
           the function will attempt to invent something useful.
BODY:      forms to execute just before running the
           hooks for the new mode.

Here is how you could define LaTeX-Thesis mode as a variant of LaTeX mode:

  (define-derived-mode LaTeX-thesis-mode LaTeX-mode \"LaTeX-Thesis\")

You could then make new key bindings for `LaTeX-thesis-mode-map'
without changing regular LaTeX mode.  In this example, BODY is empty,
and DOCSTRING is generated by default.

On a more complicated level, the following command uses sgml-mode as
the parent, and then sets the variable `case-fold-search' to nil:

  (define-derived-mode article-mode sgml-mode \"Article\"
    \"Major mode for editing technical articles.\"
    (setq case-fold-search nil))

Note that if the documentation string had been left out, it would have
been generated automatically, with a reference to the keymap."

					; Some trickiness, since what
					; appears to be the docstring
					; may really be the first
					; element of the body.
  (if (and docstring (not (stringp docstring)))
      (progn (setq body (cons docstring body))
	     (setq docstring nil)))
  (setq docstring (or docstring (derived-mode-make-docstring parent child)))

  (` (progn 
       (derived-mode-init-mode-variables (quote (, child)))
       (defun (, child) ()
	 (, docstring)
	 (interactive)
					; Run the parent.
	 ((, parent))
					; Identify special modes.
	 (if (get (quote (, parent)) 'special)
	     (put (quote (, child)) 'special t))
					; Identify the child mode.
	 (setq major-mode (quote (, child)))
	 (setq mode-name (, name))
					; Set up maps and tables.
	 (derived-mode-set-keymap (quote (, child)))
	 (derived-mode-set-syntax-table (quote (, child)))
	 (derived-mode-set-abbrev-table (quote (, child)))
					; Splice in the body (if any).
	 (,@ body)
;;;					; Run the setup function, if
;;;					; any -- this will soon be
;;;					; obsolete.
;;;	 (derived-mode-run-setup-function (quote (, child)))
					; Run the hooks, if any.
	 (derived-mode-run-hooks (quote (, child)))))))


;; PUBLIC: find the ultimate class of a derived mode.

(defun derived-mode-class (mode)
  "Find the class of a major mode.
A mode's class is the first ancestor which is NOT a derived mode.
Use the `derived-mode-parent' property of the symbol to trace backwards."
  (while (get mode 'derived-mode-parent)
    (setq mode (get mode 'derived-mode-parent)))
  mode)


;; Inline functions to construct various names from a mode name.

(defsubst derived-mode-setup-function-name (mode)
  "Construct a setup-function name based on a mode name."
  (intern (concat (symbol-name mode) "-setup")))

(defsubst derived-mode-hooks-name (mode)
  "Construct a hooks name based on a mode name."
  (intern (concat (symbol-name mode) "-hooks")))

(defsubst derived-mode-map-name (mode)
  "Construct a map name based on a mode name."
  (intern (concat (symbol-name mode) "-map")))

(defsubst derived-mode-syntax-table-name (mode)
  "Construct a syntax-table name based on a mode name."
  (intern (concat (symbol-name mode) "-syntax-table")))

(defsubst derived-mode-abbrev-table-name (mode)
  "Construct an abbrev-table name based on a mode name."
  (intern (concat (symbol-name mode) "-abbrev-table")))


;; Utility functions for defining a derived mode.

;;;###autoload
(defun derived-mode-init-mode-variables (mode)
  "Initialise variables for a new mode. 
Right now, if they don't already exist, set up a blank keymap, an
empty syntax table, and an empty abbrev table -- these will be merged
the first time the mode is used."

  (if (boundp (derived-mode-map-name mode))
      t
    (eval (` (defvar (, (derived-mode-map-name mode)) 
	       (make-sparse-keymap)
	       (, (format "Keymap for %s." mode)))))
    (put (derived-mode-map-name mode) 'derived-mode-unmerged t))

  (if (boundp (derived-mode-syntax-table-name mode))
      t
    (eval (` (defvar (, (derived-mode-syntax-table-name mode))
	       (make-vector 256 nil)
	       (, (format "Syntax table for %s." mode)))))
    (put (derived-mode-syntax-table-name mode) 'derived-mode-unmerged t))

  (if (boundp (derived-mode-abbrev-table-name mode))
      t
    (eval (` (defvar (, (derived-mode-abbrev-table-name mode))
	       (progn (define-abbrev-table (derived-mode-abbrev-table-name mode) nil)
		      (make-abbrev-table))
	       (, (format "Abbrev table for %s." mode)))))))

(defun derived-mode-make-docstring (parent child)
  "Construct a docstring for a new mode if none is provided."

  (format "This major mode is a variant of `%s', created by `define-derived-mode'.
It inherits all of the parent's attributes, but has its own keymap,
abbrev table and syntax table:

  `%s-map' and `%s-syntax-table'

which more-or-less shadow

  `%s-map' and `%s-syntax-table'

\\{%s-map}" parent child child parent parent child))


;; Utility functions for running a derived mode.

(defun derived-mode-set-keymap (mode)
  "Set the keymap of the new mode, maybe merging with the parent."
  (let* ((map-name (derived-mode-map-name mode))
	 (new-map (eval map-name))
	 (old-map (current-local-map)))
    (if (get map-name 'derived-mode-unmerged)
	(derived-mode-merge-keymaps old-map new-map))
    (put map-name 'derived-mode-unmerged nil)
    (use-local-map new-map)))

(defun derived-mode-set-syntax-table (mode) 
  "Set the syntax table of the new mode, maybe merging with the parent."
  (let* ((table-name (derived-mode-syntax-table-name mode))
	 (old-table (syntax-table))
	 (new-table (eval table-name)))
    (if (get table-name 'derived-mode-unmerged)
	(derived-mode-merge-syntax-tables old-table new-table))
    (put table-name 'derived-mode-unmerged nil)
    (set-syntax-table new-table)))

(defun derived-mode-set-abbrev-table (mode)
  "Set the abbrev table if it exists.  
Always merge its parent into it, since the merge is non-destructive."
  (let* ((table-name (derived-mode-abbrev-table-name mode))
	 (old-table local-abbrev-table)
	 (new-table (eval table-name)))
    (derived-mode-merge-abbrev-tables old-table new-table)
    (setq local-abbrev-table new-table)))

;;;(defun derived-mode-run-setup-function (mode)
;;;  "Run the setup function if it exists."

;;;  (let ((fname (derived-mode-setup-function-name mode)))
;;;    (if (fboundp fname)
;;;	(funcall fname))))

(defun derived-mode-run-hooks (mode)
  "Run the hooks if they exist."

  (let ((hooks-name (derived-mode-hooks-name mode)))
    (if (boundp hooks-name)
	(run-hooks hooks-name))))

;; Functions to merge maps and tables.

(defun derived-mode-merge-keymaps (old new)
  "Merge an old keymap into a new one.
The old keymap is set to be the last cdr of the new one, so that there will
be automatic inheritance."
  (let ((tail new))
    ;; Scan the NEW map for prefix keys.
    (while (consp tail)
      (and (consp (car tail))
	   (let* ((key (vector (car (car tail))))
		  (subnew (lookup-key new key))
		  (subold (lookup-key old key)))
	     ;; If KEY is a prefix key in both OLD and NEW, merge them.
	     (and (keymapp subnew) (keymapp subold)
		  (derived-mode-merge-keymaps subold subnew))))
      (and (vectorp (car tail))
	   ;; Search a vector of ASCII char bindings for prefix keys.
	   (let ((i (1- (length (car tail)))))
	     (while (>= i 0)
	       (let* ((key (vector i))
		      (subnew (lookup-key new key))
		      (subold (lookup-key old key)))
		 ;; If KEY is a prefix key in both OLD and NEW, merge them.
		 (and (keymapp subnew) (keymapp subold)
		      (derived-mode-merge-keymaps subold subnew)))
	       (setq i (1- i)))))
      (setq tail (cdr tail))))
  (setcdr (nthcdr (1- (length new)) new) old))

(defun derived-mode-merge-syntax-tables (old new)
  "Merge an old syntax table into a new one.
Where the new table already has an entry, nothing is copied from the old one."
  (let ((idx 0)
	(end (min (length new) (length old))))
    (while (< idx end)
      (if (not (aref new idx))
	  (aset new idx (aref old idx)))
      (setq idx (1+ idx)))))

;; Merge an old abbrev table into a new one.
;; This function requires internal knowledge of how abbrev tables work,
;; presuming that they are obarrays with the abbrev as the symbol, the expansion
;; as the value of the symbol, and the hook as the function definition.
(defun derived-mode-merge-abbrev-tables (old new)
  (if old
      (mapatoms 
       (function 
	(lambda (symbol)
	  (or (intern-soft (symbol-name symbol) new)
	      (define-abbrev new (symbol-name symbol)
		(symbol-value symbol) (symbol-function symbol)))))
       old)))
    
(provide 'derived)

;;; derived.el ends here
