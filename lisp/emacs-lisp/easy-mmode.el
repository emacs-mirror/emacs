;;; easy-mmode.el --- easy definition of minor modes.

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

;;; Code:

(defun easy-mmode-define-keymap (keymap-alist &optional menu-name)
  "Return a keymap built from KEYMAP-ALIST.
KEYMAP-ALIST must be a list of (KEYBINDING . BINDING) where
KEYBINDING and BINDINGS are suited as for define-key.
optional MENU-NAME is passed to `make-sparse-keymap'."
  (let ((keymap (make-sparse-keymap menu-name)))
    (mapcar
     (function (lambda (bind)
		 (define-key keymap
		   (car bind) (cdr bind))))
     keymap-alist)
    keymap))

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

(provide 'easy-mmode)

;;; easy-mmode.el ends here
