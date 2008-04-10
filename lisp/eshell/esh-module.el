;;; esh-module.el --- Eshell modules

;; Copyright (C) 1999, 2000, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>
;; Keywords: processes

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(provide 'esh-module)

(eval-when-compile
  (require 'cl)
  (require 'esh-util))

(require 'esh-util)

(defgroup eshell-module nil
  "The `eshell-module' group is for Eshell extension modules, which
provide optional behavior which the user can enable or disable by
customizing the variable `eshell-modules-list'."
  :tag "Extension modules"
  :group 'eshell)

(eval-and-compile
(defun eshell-load-defgroups (&optional directory)
  "Load `defgroup' statements from Eshell's module files."
  (let ((vc-handled-backends nil)) ; avoid VC fucking things up
    (with-current-buffer
	(find-file-noselect (expand-file-name "esh-groups.el" directory))
      (erase-buffer)
      (insert ";;; do not modify this file; it is auto-generated -*- no-byte-compile: t -*-\n\n")
      (let ((files (directory-files (or directory
					(car command-line-args-left))
				    nil "\\`em-.*\\.el\\'")))
	(while files
	  (message "Loading defgroup from `%s'" (car files))
	  (let (defgroup)
	    (catch 'handled
	      (with-current-buffer (find-file-noselect (car files))
		(goto-char (point-min))
		(while t
		  (forward-sexp)
		  (if (eobp) (throw 'handled t))
		  (backward-sexp)
		  (let ((begin (point))
			(defg (looking-at "(defgroup")))
		    (forward-sexp)
		    (if defg
			(setq defgroup (buffer-substring begin (point))))))))
	    (if defgroup
		(insert defgroup "\n\n")))
	  (setq files (cdr files))))
      ;; Don't make backups, to avoid prompting the user if there are
      ;; excess backup versions.
      (save-buffer 0)))))

;; load the defgroup's for the standard extension modules, so that
;; documentation can be provided when the user customize's
;; `eshell-modules-list'.
(eval-when-compile
  (when (and (boundp 'byte-compile-current-file)
	     byte-compile-current-file
	     (or
	      (equal (file-name-nondirectory byte-compile-current-file)
		     "esh-module.el")
	      ;; When eshell file names are expanded from a wildcard
	      ;; or by reading the Eshell directory, e.g. when they
	      ;; say "make recompile" in the lisp directory, Emacs on
	      ;; MS-DOS sees a truncated name "esh-modu.el" instead of
	      ;; "esh-module.el".
	      (and (fboundp 'msdos-long-file-names)
		   (null (msdos-long-file-names))
		   (equal (file-name-nondirectory byte-compile-current-file)
			  "esh-modu.el"))))
    (let* ((directory (file-name-directory byte-compile-current-file))
	   (elc-file (expand-file-name "esh-groups.elc" directory)))
      (eshell-load-defgroups directory)
      (if (file-exists-p elc-file) (delete-file elc-file)))))

(load "esh-groups" t t)

;;; User Variables:

(defcustom eshell-module-unload-hook
  '(eshell-unload-extension-modules)
  "*A hook run when `eshell-module' is unloaded."
  :type 'hook
  :group 'eshell-module)

(defcustom eshell-modules-list
  '(eshell-alias
    eshell-banner
    eshell-basic
    eshell-cmpl
    eshell-dirs
    eshell-glob
    eshell-hist
    eshell-ls
    eshell-pred
    eshell-prompt
    eshell-script
    eshell-term
    eshell-unix)
  "*A list of optional add-on modules to be loaded by Eshell.
Changes will only take effect in future Eshell buffers."
  :type (append
	 (list 'set ':tag "Supported modules")
	 (mapcar
	  (function
	   (lambda (modname)
	     (let ((modsym (intern modname)))
	       (list 'const
		     ':tag (format "%s -- %s" modname
				   (get modsym 'custom-tag))
		     ':link (caar (get modsym 'custom-links))
		     ':doc (concat "\n" (get modsym 'group-documentation)
				   "\n ")
		     modsym))))
	  (sort (mapcar 'symbol-name
			(eshell-subgroups 'eshell-module))
		'string-lessp))
	 '((repeat :inline t :tag "Other modules" symbol)))
  :group 'eshell-module)

;;; Code:

(defsubst eshell-using-module (module)
  "Return non-nil if a certain Eshell MODULE is in use.
The MODULE should be a symbol corresponding to that module's
customization group.  Example: `eshell-cmpl' for that module."
  (memq module eshell-modules-list))

(defun eshell-unload-extension-modules ()
  "Unload any memory resident extension modules."
  (eshell-for module (eshell-subgroups 'eshell-module)
    (if (featurep module)
	(ignore-errors
	  (message "Unloading %s..." (symbol-name module))
	  (unload-feature module)
	  (message "Unloading %s...done" (symbol-name module))))))

;; arch-tag: 97a3fa16-9d08-40e6-bc2c-36bd70986507
;;; esh-module.el ends here
