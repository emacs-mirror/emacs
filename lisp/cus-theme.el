;;; cus-theme.el -- custom theme creation user interface
;;
;; Copyright (C) 2001, 2005 Free Software Foundation, Inc.
;;
;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: FSF
;; Keywords: help, faces

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

;;; Code:

(require 'widget)
(require 'cus-edit)

(eval-when-compile
  (require 'wid-edit))

(define-derived-mode custom-new-theme-mode nil "New-Theme"
  "Major mode for the buffer created by `customize-create-theme'.
Do not call this mode function yourself.  It is only meant for internal
use by `customize-create-theme'."
  (set-keymap-parent custom-new-theme-mode-map widget-keymap))
(put 'custom-new-theme-mode 'mode-class 'special)

(defvar custom-theme-name)
(defvar custom-theme-variables)
(defvar custom-theme-faces)
(defvar custom-theme-description)

;;;###autoload
(defun customize-create-theme ()
  "Create a custom theme."
  (interactive)
  (if (get-buffer "*New Custom Theme*")
      (kill-buffer "*New Custom Theme*"))
  (switch-to-buffer "*New Custom Theme*")
  (let ((inhibit-read-only t))
    (erase-buffer))
  (custom-new-theme-mode)
  (make-local-variable 'custom-theme-name)
  (make-local-variable 'custom-theme-variables)
  (make-local-variable 'custom-theme-faces)
  (make-local-variable 'custom-theme-description)
  (widget-insert "This buffer helps you write a custom theme elisp file.
This will help you share your customizations with other people.

Just insert the names of all variables and faces you want the theme
to include.  Then clicking mouse-2 or pressing RET on the [Done] button
will write a theme file that sets all these variables and faces to their
current global values.  It will write that file into the directory given
by the variable `custom-theme-directory', usually \"~/.emacs.d/\".

To undo all your edits to the buffer, use the [Reset] button.\n\n")
  (widget-insert "Theme name: ")
  (setq custom-theme-name
	(widget-create 'editable-field
		       :size 10
		       user-login-name))
  (widget-insert "\n\nDocumentation:\n")
  (setq custom-theme-description
	(widget-create 'text
		       :value (format-time-string "Created %Y-%m-%d.")))
  (widget-insert "\nVariables:\n\n")
  (setq custom-theme-variables
     	(widget-create 'editable-list
     		       :entry-format "%i %d %v"
		       'variable))
  (widget-insert "\nFaces:\n\n")
  (setq custom-theme-faces
     	(widget-create 'editable-list
     		       :entry-format "%i %d %v"
		       'face))
  (widget-insert "\n")
  (widget-create 'push-button
     		 :notify (function custom-theme-write)
     		 "Done")
  (widget-insert " ")
  (widget-create 'push-button
     		 :notify (lambda (&rest ignore)
     			   (customize-create-theme))
     		 "Reset")
  (widget-insert " ")
  (widget-create 'push-button
     		 :notify (lambda (&rest ignore)
     			   (bury-buffer))
     		 "Bury Buffer")
  (widget-insert "\n")
  (widget-setup))

(defun custom-theme-write (&rest ignore)
  (let ((name (widget-value custom-theme-name))
	(doc (widget-value custom-theme-description))
	(variables (widget-value custom-theme-variables))
	(faces (widget-value custom-theme-faces)))
    (switch-to-buffer (concat name "-theme.el"))
    (emacs-lisp-mode)
    (unless (file-exists-p custom-theme-directory)
      (make-directory (file-name-as-directory custom-theme-directory) t))
    (setq default-directory custom-theme-directory)
    (setq buffer-file-name (expand-file-name (concat name "-theme.el")))
    (let ((inhibit-read-only t))
      (erase-buffer))
    (insert "(deftheme " name)
    (when doc
      (newline)
      (insert "  \"" doc "\""))
    (insert  ")\n")
    (custom-theme-write-variables name variables)
    (custom-theme-write-faces name faces)
    (insert "\n(provide-theme '" name ")\n")
    (save-buffer)))

(defun custom-theme-write-variables (theme vars)
  "Write a `custom-theme-set-variables' command for THEME.
It includes all variables in list VARS."
  ;; Most code is stolen from `custom-save-variables'.
  (when vars
    (let ((standard-output (current-buffer)))
      (princ "\n(custom-theme-set-variables\n")
      (princ " '")
      (princ theme)
      (princ "\n")
      (mapc (lambda (symbol)
	      (when (boundp symbol)
		(unless (bolp)
		  (princ "\n"))
		(princ " '(")
		(prin1 symbol)
		(princ " ")
		(prin1 (custom-quote (symbol-value symbol)))
		(princ ")")))
	      vars)
      (if (bolp)
	  (princ " "))
      (princ ")")
      (unless (looking-at "\n")
	(princ "\n")))))

(defun custom-theme-write-faces (theme faces)
  "Write a `custom-theme-set-faces' command for THEME.
It includes all faces in list FACES."
  (when faces
    (let ((standard-output (current-buffer)))
      (princ "\n(custom-theme-set-faces\n")
      (princ " '")
      (princ theme)
      (princ "\n")
      (mapc (lambda (symbol)
	      (when (facep symbol)
		(unless (bolp)
		  (princ "\n"))
		(princ " '(")
		(prin1 symbol)
		(princ " ")
		(prin1 (or (get symbol 'customized-face)
			   (get symbol 'face-defface-spec)))
		(princ ")")))
	      faces)
      (if (bolp)
	  (princ " "))
      (princ ")")
      (unless (looking-at "\n")
	(princ "\n")))))

;;; arch-tag: cd6919bc-63af-410e-bae2-b6702e762344
;;; cus-theme.el ends here
