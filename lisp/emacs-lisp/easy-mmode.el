;;; easy-mmode.el --- easy definition of minor modes.

;; Copyright (C) 1997  Free Software Foundation, Inc.

;; Author:  Georges Brun-Cottan <Georges.Brun-Cottan@inria.fr>

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

(defmacro easy-mmode-define-toggle (mode &optional doc)
  "Define a one arg toggle mode MODE function and associated hooks.
MODE-mode is the so defined function that toggle the mode.
optional DOC is its associated documentation.

Hooks are checked for run, each time MODE-mode is called.
They run under the followings conditions:
MODE-hook: if the mode is toggled.
MODE-on-hook: if the mode is on.
MODE-off-hook: if the mode is off.

When the mode is effectively toggled, two hooks may run.
If so MODE-hook is guaranteed to be the first.

\(defmacro easy-mmode-define-toggle (MODE &optional DOC)"
  (let* ((mode-name
	  (if (string-match "-mode\\'" (symbol-name mode))
	      (symbol-name mode)
	    (concat (symbol-name mode) "-mode")))
	 (hook (intern (concat mode-name "-hook")))
	 (hook-on (intern (concat mode-name "-on-hook")))
	 (hook-off (intern (concat mode-name "-off-hook")))
	 (toggle (intern mode-name))
	 (mode toggle)
	 (toggle-doc (or doc
			 (format "With no argument, toggle %s mode.
With arg turn mode on.
With zero or negative arg turn mode off"
				 mode-name))))
    `(progn
       (defvar ,hook  nil	     
	 ,(format "Hook called when %s mode is toggled" mode-name))

       (defvar ,hook-on  nil	     
	 ,(format "Hook called when %s mode is turned on" mode-name))

       (defvar ,hook-off nil
	 ,(format "Hook called when %s mode is turned off" mode-name))

       (defun ,toggle (&optional arg)
	 ,toggle-doc
	 (interactive "P")
	 (let ((old-mode ,mode))
	   (setq ,mode
		 (if arg
		     (or (listp arg);; C-u alone
			 (> (prefix-numeric-value arg) 0))
		   (not ,mode)))
	   (and ,hook
		(not (equal old-mode ,mode))
	        (run-hooks ',hook))
	   (and ,hook-on
		,mode
		(run-hooks ',hook-on))
	   (and ,hook-off
		(not ,mode)
		(run-hooks ',hook-off)))))))

;;;###autoload
(defmacro easy-mmode-define-minor-mode (mode doc &optional init-value lighter keymap)
  "Define a new minor mode MODE.
This function defines the associated control variable, keymap,
toggle command, and hooks (see `easy-mmode-define-toggle').

DOC is the documentation for the mode toggle command.
Optional LIGHTER is displayed in the mode-bar when the mode is on.
Optional KEYMAP is the default (defvar) keymap bound to the mode keymap.
If it is a list, it is passed to `easy-mmode-define-keymap'
in order to build a valid keymap.
 
\(defmacro easy-mmode-define-minor-mode
  (MODE DOC &optional INIT-VALUE LIGHTER KEYMAP)...\)" 
  (let* ((mode-name (symbol-name mode))
	 (mode-doc (format "Non-nil if %s mode is enabled." mode-name))
	 (keymap-name (concat mode-name "-map"))
	 (keymap-doc (format "Keymap for %s mode." mode-name)))
    `(progn
       ;; define the switch
       (defvar ,mode ,init-value ,mode-doc)
       (make-variable-buffer-local ',mode)

       ;; define the minor-mode keymap
       (defvar ,(intern keymap-name)
	 (cond ((and ,keymap (keymapp ,keymap))
		,keymap)
	       ((listp ,keymap)
		(easy-mmode-define-keymap ,keymap))
	       (t (error "Invalid keymap %S" ,keymap)))
	 ,keymap-doc)

       ;; define the toggle and the hooks
       ,(macroexpand `(easy-mmode-define-toggle ,mode ,doc)) ; toggle and hooks

       ;; update the mode-bar
       (or (assq ',mode minor-mode-alist)
	   (setq minor-mode-alist
		 (cons (list ',mode ,lighter) minor-mode-alist)))

       ;; update the minor-mode-map
       (or (assq ',mode minor-mode-map-alist)
	   (setq minor-mode-map-alist 
		 (cons (cons ',mode ,(intern keymap-name)) minor-mode-map-alist)))) ))

(provide 'easy-mmode)

;;; easy-mmode.el ends here
