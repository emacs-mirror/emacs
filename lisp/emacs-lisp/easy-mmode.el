;;; easy-mmode.el --- easy definition for major and minor modes.

;; Copyright (C) 1997  Free Software Foundation, Inc.

;; Author:  Georges Brun-Cottan <Georges.Brun-Cottan@inria.fr>
;; Maintainer:  Stefan Monnier <monnier@gnu.org>

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

;; Minor modes are useful and common.  This package makes defining a
;; minor mode easy, by focusing on the writing of the minor mode
;; functionalities themselves.  Moreover, this package enforces a
;; conventional naming of user interface primitives, making things
;; natural for the minor-mode end-users.

;; For each mode, easy-mmode defines the following:
;; <mode>      : The minor mode predicate. A buffer-local variable.
;; <mode>-map  : The keymap possibly associated to <mode>.
;; <mode>-hook,<mode>-on-hook,<mode>-off-hook and <mode>-mode:
;;       see `easy-mmode-define-minor-mode' documentation
;;
;; eval
;;  (pp (macroexpand '(easy-mmode-define-minor-mode <your-mode> <doc>)))
;; to check the result before using it.

;; The order in which minor modes are installed is important.  Keymap
;; lookup proceeds down minor-mode-map-alist, and the order there
;; tends to be the reverse of the order in which the modes were
;; installed.  Perhaps there should be a feature to let you specify
;; orderings.

;; Additionally to `define-minor-mode', the package provides convenient
;; ways to define keymaps, and other helper functions for major and minor modes.

;;; Code:

(defmacro easy-mmode-define-toggle (mode &optional doc &rest body)
  "Define a one arg toggle mode MODE function and associated hooks.
MODE is the so defined function that toggles the mode.
optional DOC is its associated documentation.
BODY is executed after the toggling and before running the hooks.

Hooks are checked for run, each time MODE-mode is called.
They run under the followings conditions:
MODE-hook: if the mode is toggled.
MODE-on-hook: if the mode is on.
MODE-off-hook: if the mode is off.

When the mode is effectively toggled, two hooks may run.
If so MODE-hook is guaranteed to be the first."
  (let* ((mode-name (symbol-name mode))
	 (hook (intern (concat mode-name "-hook")))
	 (hook-on (intern (concat mode-name "-on-hook")))
	 (hook-off (intern (concat mode-name "-off-hook")))
	 (toggle-doc (or doc
			 (format "With no argument, toggle %s.
With universal prefix ARG turn mode on.
With zero or negative ARG turn mode off.
\\{%s}" mode-name (concat mode-name "-map")))))
    `(progn
       (defcustom ,hook  nil
	 ,(format "Hook called when `%s' is toggled" mode-name)
	 :type 'hook)

       (defcustom ,hook-on  nil
	 ,(format "Hook called when `%s' is turned on" mode-name)
	 :type 'hook)

       (defcustom ,hook-off nil
	 ,(format "Hook called when `%s' is turned off" mode-name)
	 :type 'hook)

       (defun ,mode (&optional arg)
	 ,toggle-doc
	 (interactive "P")
	 (let ((old-mode ,mode))
	   (setq ,mode
		 (if arg
		     (> (prefix-numeric-value arg) 0)
		   (not ,mode)))
	   ,@body
	   (unless (equal old-mode ,mode) (run-hooks ',hook))
	   (run-hooks (if ,mode ',hook-on ',hook-off)))))))

;;;###autoload
(defalias 'easy-mmode-define-minor-mode 'define-minor-mode)
;;;###autoload
(defmacro define-minor-mode (mode doc &optional init-value lighter keymap &rest body)
  "Define a new minor mode MODE.
This function defines the associated control variable, keymap,
toggle command, and hooks (see `easy-mmode-define-toggle').

DOC is the documentation for the mode toggle command.
Optional INIT-VALUE is the initial value of the mode's variable.
Optional LIGHTER is displayed in the mode-bar when the mode is on.
Optional KEYMAP is the default (defvar) keymap bound to the mode keymap.
If it is a list, it is passed to `easy-mmode-define-keymap'
in order to build a valid keymap.
BODY contains code that will be executed each time the mode is (dis)activated.
It will be executed after any toggling but before running the hooks."
  (let* ((mode-name (symbol-name mode))
	 (mode-doc (format "Non-nil if mode is enabled.
Use the function `%s' to change this variable." mode-name))
	 (keymap-sym (intern (concat mode-name "-map")))
	 (keymap-doc (format "Keymap for `%s'." mode-name)))
    `(progn
       ;; Define the variable to enable or disable the mode.
       (defvar ,mode ,init-value ,mode-doc)
       (make-variable-buffer-local ',mode)

       ;; Define the minor-mode keymap.
       ,(when keymap
	  `(defvar ,keymap-sym
	     (cond ((and ,keymap (keymapp ,keymap))
		    ,keymap)
		   ((listp ,keymap)
		    (easy-mmode-define-keymap ,keymap))
		   (t (error "Invalid keymap %S" ,keymap)))
	     ,keymap-doc))

       ;; Define the toggle and the hooks.
       (easy-mmode-define-toggle ,mode ,doc ,@body)

       ;; Update the mode line.
       (or (assq ',mode minor-mode-alist)
	   (setq minor-mode-alist
		 (cons (list ',mode nil) minor-mode-alist)))
       (setcar (cdr (assq ',mode minor-mode-alist)) ,lighter)

       ;; Update the minor mode map.
       (or (assq ',mode minor-mode-map-alist)
	   (setq minor-mode-map-alist
		 (cons (cons ',mode nil) minor-mode-map-alist)))
       (setcdr (assq ',mode minor-mode-map-alist)
	       ,keymap-sym)) ))


;;;
;;; easy-mmode-defmap
;;;

(if (fboundp 'set-keymap-parents)
    (defalias 'easy-mmode-set-keymap-parents 'set-keymap-parents)
  (defun easy-mmode-set-keymap-parents (m parents)
    (set-keymap-parent
     m
     (cond
      ((not (consp parents)) parents)
      ((not (cdr parents)) (car parents))
      (t (let ((m (copy-keymap (pop parents))))
	   (easy-mmode-set-keymap-parents m parents)
	   m))))))

(defun easy-mmode-define-keymap (bs &optional name m args)
  "Return a keymap built from bindings BS.
BS must be a list of (KEY . BINDING) where
KEY and BINDINGS are suited as for define-key.
optional NAME is passed to `make-sparse-keymap'.
optional map M can be used to modify an existing map.
ARGS is a list of additional arguments."
  (let (inherit dense suppress)
    (while args
      (let ((key (pop args))
	    (val (pop args)))
	(cond
	 ((eq key :dense) (setq dense val))
	 ((eq key :inherit) (setq inherit val))
	 ((eq key :group) )
	 ;;((eq key :suppress) (setq suppress val))
	 (t (message "Unknown argument %s in defmap" key)))))
    (unless (keymapp m)
      (setq bs (append m bs))
      (setq m (if dense (make-keymap name) (make-sparse-keymap name))))
    (dolist (b bs)
      (let ((keys (car b))
	    (binding (cdr b)))
	(dolist (key (if (consp keys) keys (list keys)))
	  (cond
	   ((symbolp key)
	    (substitute-key-definition key binding m global-map))
	   ((null binding)
	    (unless (keymapp (lookup-key m key)) (define-key m key binding)))
	   ((let ((o (lookup-key m key)))
	      (or (null o) (numberp o) (eq o 'undefined)))
	    (define-key m key binding))))))
    (cond
     ((keymapp inherit) (set-keymap-parent m inherit))
     ((consp inherit) (easy-mmode-set-keymap-parents m inherit)))
    m))

;;;###autoload
(defmacro easy-mmode-defmap (m bs doc &rest args)
  `(defconst ,m
     (easy-mmode-define-keymap ,bs nil (if (boundp ',m) ,m) ,(cons 'list args))
     ,doc))


;;;
;;; easy-mmode-defsyntax
;;;

(defun easy-mmode-define-syntax (css args)
  (let ((st (make-syntax-table (cadr (memq :copy args)))))
    (dolist (cs css)
      (let ((char (car cs))
	    (syntax (cdr cs)))
	(if (sequencep char)
	    (mapcar* (lambda (c) (modify-syntax-entry c syntax st)) char)
	  (modify-syntax-entry char syntax st))))
    st))

;;;###autoload
(defmacro easy-mmode-defsyntax (st css doc &rest args)
  `(defconst ,st (custom-create-syntax ,css ,(cons 'list args)) doc))



;;; A "macro-only" reimplementation of define-derived-mode.

(defmacro easy-mmode-define-derived-mode (child parent name &optional docstring &rest body)
  "Create a new mode as a variant of an existing mode.

The arguments to this command are as follow:

CHILD:     the name of the command for the derived mode.
PARENT:    the name of the command for the parent mode (e.g. `text-mode').
NAME:      a string which will appear in the status line (e.g. \"Hypertext\")
DOCSTRING: an optional documentation string--if you do not supply one,
           the function will attempt to invent something useful.
BODY:      forms to execute just before running the
           hooks for the new mode.

Here is how you could define LaTeX-Thesis mode as a variant of LaTeX mode:

  (define-derived-mode LaTeX-thesis-mode LaTeX-mode \"LaTeX-Thesis\")

You could then make new key bindings for `LaTeX-thesis-mode-map'
without changing regular LaTeX mode.  In this example, BODY is empty,
and DOCSTRING is generated by default.

On a more complicated level, the following command uses `sgml-mode' as
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
  (let* ((child-name (symbol-name child))
	 (map (intern (concat child-name "-map")))
	 (syntax (intern (concat child-name "-syntax-table")))
	 (abbrev (intern (concat child-name "-abbrev-table")))
	 (hook (intern (concat child-name "-hook"))))
	 
  `(progn
     (defvar ,map (make-sparse-keymap))
     (defvar ,syntax (make-char-table 'syntax-table nil))
     (defvar ,abbrev (progn (define-abbrev-table ',abbrev nil) ,abbrev))
     
     (defun ,child ()
       ,(or docstring
	    (format "Major mode derived from `%s' by `define-derived-mode'.
Inherits all of the parent's attributes, but has its own keymap,
abbrev table and syntax table:

  `%s', `%s' and `%s'

which more-or-less shadow %s's corresponding tables.
It also runs its own `%s' after its parent's.

\\{%s}" parent map syntax abbrev parent hook map))
       (interactive)
					; Run the parent.
       (,parent)
					; Identify special modes.
       (put ',child 'special (get ',parent 'special))
					; Identify the child mode.
       (setq major-mode ',child)
       (setq mode-name ,name)
					; Set up maps and tables.
       (unless (keymap-parent ,map)
	 (set-keymap-parent ,map (current-local-map)))
       (let ((parent (char-table-parent ,syntax)))
	 (unless (and parent (not (eq parent (standard-syntax-table))))
	   (set-char-table-parent ,syntax (syntax-table))))
       (when local-abbrev-table
	 (mapatoms
	  (lambda (symbol)
	    (or (intern-soft (symbol-name symbol) ,abbrev)
		(define-abbrev ,abbrev (symbol-name symbol)
		  (symbol-value symbol) (symbol-function symbol))))
	  local-abbrev-table))
       
       (use-local-map ,map)
       (set-syntax-table ,syntax)
       (setq local-abbrev-table ,abbrev)
					; Splice in the body (if any).
       ,@body
					; Run the hooks, if any.
       (run-hooks ',hook)))))


(provide 'easy-mmode)

;;; easy-mmode.el ends here
