;;; cus-theme.el -- custom theme creation user interface
;;
;; Copyright (C) 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007, 2008 Free Software Foundation, Inc.
;;
;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: FSF
;; Keywords: help, faces

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

(require 'widget)
(require 'cus-edit)

(eval-when-compile
  (require 'wid-edit))

(defvar custom-new-theme-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map widget-keymap)
    (suppress-keymap map)
    (define-key map "n" 'widget-forward)
    (define-key map "p" 'widget-backward)
    map)
  "Keymap for `custom-new-theme-mode'.")

(define-derived-mode custom-new-theme-mode nil "New-Theme"
  "Major mode for the buffer created by `customize-create-theme'.
Do not call this mode function yourself.  It is only meant for internal
use by `customize-create-theme'."
  (use-local-map custom-new-theme-mode-map)
  (define-key custom-new-theme-mode-map [mouse-1] 'widget-move-and-invoke)
  (set (make-local-variable 'widget-documentation-face) 'custom-documentation)
  (set (make-local-variable 'widget-button-face) custom-button)
  (set (make-local-variable 'widget-button-pressed-face) custom-button-pressed)
  (set (make-local-variable 'widget-mouse-face) custom-button-mouse)
  (when custom-raised-buttons
    (set (make-local-variable 'widget-push-button-prefix) "")
    (set (make-local-variable 'widget-push-button-suffix) "")
    (set (make-local-variable 'widget-link-prefix) "")
    (set (make-local-variable 'widget-link-suffix) "")))
(put 'custom-new-theme-mode 'mode-class 'special)

(defvar custom-theme-name nil)
(defvar custom-theme-variables nil)
(defvar custom-theme-faces nil)
(defvar custom-theme-description)
(defvar custom-theme-insert-variable-marker)
(defvar custom-theme-insert-face-marker)

;;;###autoload
(defun customize-create-theme ()
  "Create a custom theme."
  (interactive)
  (switch-to-buffer (generate-new-buffer "*New Custom Theme*"))
  (let ((inhibit-read-only t))
    (erase-buffer))
  (custom-new-theme-mode)
  (make-local-variable 'custom-theme-name)
  (make-local-variable 'custom-theme-variables)
  (make-local-variable 'custom-theme-faces)
  (make-local-variable 'custom-theme-description)
  (make-local-variable 'custom-theme-insert-variable-marker)
  (make-local-variable 'custom-theme-insert-face-marker)
  (widget-insert "This buffer helps you write a custom theme elisp file.
This will help you share your customizations with other people.

Insert the names of all variables and faces you want the theme to include.
Invoke \"Save Theme\" to save the theme.  The theme file will be saved to
the directory " custom-theme-directory "\n\n")
  (widget-create 'push-button
		 :tag "Visit Theme"
		 :help-echo "Insert the settings of a pre-defined theme."
		 :action (lambda (widget &optional event)
			   (call-interactively 'custom-theme-visit-theme)))
  (widget-insert "  ")
  (widget-create 'push-button
		 :tag "Merge Theme"
		 :help-echo "Merge in the settings of a pre-defined theme."
		 :action (lambda (widget &optional event)
			   (call-interactively 'custom-theme-merge-theme)))
  (widget-insert "  ")
  (widget-create 'push-button
     		 :notify (lambda (&rest ignore)
			   (when (y-or-n-p "Discard current changes? ")
			     (kill-buffer (current-buffer))
			     (customize-create-theme)))
     		 "Reset Buffer")
  (widget-insert "  ")
  (widget-create 'push-button
     		 :notify (function custom-theme-write)
     		 "Save Theme")
  (widget-insert "\n")

  (widget-insert "\n\nTheme name: ")
  (setq custom-theme-name
	(widget-create 'editable-field
		       :size 10
		       user-login-name))
  (widget-insert "\n\nDocumentation:\n")
  (setq custom-theme-description
	(widget-create 'text
		       :value (format-time-string "Created %Y-%m-%d.")))
  (widget-insert "\n")
  (widget-create 'push-button
		 :tag "Insert Variable"
		 :help-echo "Add another variable to this theme."
		 :action (lambda (widget &optional event)
			   (call-interactively 'custom-theme-add-variable)))
  (widget-insert "\n")
  (setq custom-theme-insert-variable-marker (point-marker))
  (widget-insert "\n")
  (widget-create 'push-button
		 :tag "Insert Face"
		 :help-echo "Add another face to this theme."
		 :action (lambda (widget &optional event)
			   (call-interactively 'custom-theme-add-face)))
  (widget-insert "\n")
  (setq custom-theme-insert-face-marker (point-marker))
  (widget-insert "\n")
  (widget-create 'push-button
     		 :notify (lambda (&rest ignore)
			   (when (y-or-n-p "Discard current changes? ")
			     (kill-buffer (current-buffer))
			     (customize-create-theme)))
     		 "Reset Buffer")
  (widget-insert "  ")
  (widget-create 'push-button
     		 :notify (function custom-theme-write)
     		 "Save Theme")
  (widget-insert "\n")
  (widget-setup)
  (goto-char (point-min))
  (message ""))

;;; Theme variables

(defun custom-theme-add-variable (symbol)
  (interactive "vVariable name: ")
  (cond ((assq symbol custom-theme-variables)
	 (message "%s is already in the theme" (symbol-name symbol)))
	((not (boundp symbol))
	 (message "%s is not defined as a variable" (symbol-name symbol)))
	((eq symbol 'custom-enabled-themes)
	 (message "Custom theme cannot contain `custom-enabled-themes'"))
	(t
	 (save-excursion
	   (goto-char custom-theme-insert-variable-marker)
	   (widget-insert "\n")
	   (let ((widget (widget-create 'custom-variable
					:tag (custom-unlispify-tag-name symbol)
					:custom-level 0
					:action 'custom-theme-variable-action
					:custom-state 'unknown
					:value symbol)))
	     (push (cons symbol widget) custom-theme-variables)
	     (custom-magic-reset widget))
	   (widget-setup)))))

(defvar custom-theme-variable-menu
  `(("Reset to Current" custom-redraw
     (lambda (widget)
       (and (boundp (widget-value widget))
	    (memq (widget-get widget :custom-state)
		  '(themed modified changed)))))
    ("Reset to Theme Value" custom-variable-reset-theme
     (lambda (widget)
       (let ((theme  (intern (widget-value custom-theme-name)))
	     (symbol (widget-value widget))
	     found)
	 (and (custom-theme-p theme)
	      (dolist (setting (get theme 'theme-settings) found)
	 	(if (and (eq (cadr setting) symbol)
	 		 (eq (car  setting) 'theme-value))
	 	    (setq found t)))))))
    ("---" ignore ignore)
    ("Delete" custom-theme-delete-variable nil))
  "Alist of actions for the `custom-variable' widget in Custom Theme Mode.
See the documentation for `custom-variable'.")

(defun custom-theme-variable-action (widget &optional event)
  "Show the Custom Theme Mode menu for a `custom-variable' widget.
Optional EVENT is the location for the menu."
  (let ((custom-variable-menu custom-theme-variable-menu))
    (custom-variable-action widget event)))

(defun custom-variable-reset-theme (widget)
  "Reset WIDGET to its value for the currently edited theme."
  (let ((theme  (intern (widget-value custom-theme-name)))
	(symbol (widget-value widget))
	found)
    (dolist (setting (get theme 'theme-settings))
      (if (and (eq (cadr setting) symbol)
	       (eq (car  setting) 'theme-value))
	  (setq found setting)))
    (widget-value-set (car (widget-get widget :children))
		      (nth 3 found)))
  (widget-put widget :custom-state 'themed)
  (custom-redraw-magic widget)
  (widget-setup))

(defun custom-theme-delete-variable (widget)
  (setq custom-theme-variables
	(assq-delete-all (widget-value widget) custom-theme-variables))
  (widget-delete widget))

;;; Theme faces

(defun custom-theme-add-face (symbol)
  (interactive (list (read-face-name "Face name" nil nil)))
  (cond ((assq symbol custom-theme-faces)
	 (message "%s is already in the theme" (symbol-name symbol)))
	((not (facep symbol))
	 (message "%s is not defined as a face" (symbol-name symbol)))
	(t
	 (save-excursion
	   (goto-char custom-theme-insert-face-marker)
	   (widget-insert "\n")
	   (let ((widget (widget-create 'custom-face
					:tag (custom-unlispify-tag-name symbol)
					:custom-level 0
					:action 'custom-theme-face-action
					:custom-state 'unknown
					:value symbol)))
	     (push (cons symbol widget) custom-theme-faces)
	     (custom-magic-reset widget)
	     (widget-setup))))))

(defvar custom-theme-face-menu
  `(("Reset to Theme Value" custom-face-reset-theme
     (lambda (widget)
       (let ((theme  (intern (widget-value custom-theme-name)))
	     (symbol (widget-value widget))
	     found)
	 (and (custom-theme-p theme)
	      (dolist (setting (get theme 'theme-settings) found)
	 	(if (and (eq (cadr setting) symbol)
	 		 (eq (car  setting) 'theme-face))
	 	    (setq found t)))))))
    ("---" ignore ignore)
    ("Delete" custom-theme-delete-face nil))
  "Alist of actions for the `custom-variable' widget in Custom Theme Mode.
See the documentation for `custom-variable'.")

(defun custom-theme-face-action (widget &optional event)
  "Show the Custom Theme Mode menu for a `custom-face' widget.
Optional EVENT is the location for the menu."
  (let ((custom-face-menu custom-theme-face-menu))
    (custom-face-action widget event)))

(defun custom-face-reset-theme (widget)
  "Reset WIDGET to its value for the currently edited theme."
  (let ((theme  (intern (widget-value custom-theme-name)))
	(symbol (widget-value widget))
	found)
    (dolist (setting (get theme 'theme-settings))
      (if (and (eq (cadr setting) symbol)
	       (eq (car  setting) 'theme-face))
	  (setq found setting)))
    (widget-value-set (car (widget-get widget :children))
		      (nth 3 found)))
  (widget-put widget :custom-state 'themed)
  (custom-redraw-magic widget)
  (widget-setup))

(defun custom-theme-delete-face (widget)
  (setq custom-theme-faces
	(assq-delete-all (widget-value widget) custom-theme-faces))
  (widget-delete widget))

;;; Reading and writing

(defun custom-theme-visit-theme ()
  (interactive)
  (when (or (null custom-theme-variables)
	    (if (y-or-n-p "Discard current changes? ")
		(progn (customize-create-theme) t)))
    (let ((theme (call-interactively 'custom-theme-merge-theme)))
      (unless (eq theme 'user)
	(widget-value-set custom-theme-name (symbol-name theme)))
      (widget-value-set custom-theme-description
			(or (get theme 'theme-documentation)
			    (format-time-string "Created %Y-%m-%d.")))
      (widget-setup))))

(defun custom-theme-merge-theme (theme)
  (interactive "SCustom theme name: ")
  (unless (eq theme 'user)
    (load-theme theme))
  (let ((settings (get theme 'theme-settings)))
    (dolist (setting settings)
      (if (eq (car setting) 'theme-value)
	  (custom-theme-add-variable (cadr setting))
	(custom-theme-add-face (cadr setting)))))
  (disable-theme theme)
  theme)

(defun custom-theme-write (&rest ignore)
  (let* ((name (widget-value custom-theme-name))
	 (filename (expand-file-name (concat name "-theme.el")
				     custom-theme-directory))
	 (doc (widget-value custom-theme-description))
	 (vars custom-theme-variables)
	 (faces custom-theme-faces))
    (cond ((or (string-equal name "")
	      (string-equal name "user")
	      (string-equal name "changed"))
	   (error "Custom themes cannot be named `%s'" name))
	  ((string-match " " name)
	   (error "Custom theme names should not contain spaces"))
	  ((if (file-exists-p filename)
	       (not (y-or-n-p
		     (format "File %s exists.  Overwrite? " filename))))
	   (error "Aborted")))
    (with-temp-buffer
      (emacs-lisp-mode)
      (unless (file-exists-p custom-theme-directory)
	(make-directory (file-name-as-directory custom-theme-directory) t))
      (setq buffer-file-name filename)
      (erase-buffer)
      (insert "(deftheme " name)
      (if doc (insert "\n  \"" doc "\""))
      (insert  ")\n")
      (custom-theme-write-variables name vars)
      (custom-theme-write-faces name faces)
      (insert "\n(provide-theme '" name ")\n")
      (save-buffer))
    (dolist (var vars)
      (widget-put (cdr var) :custom-state 'saved)
      (custom-redraw-magic (cdr var)))
    (dolist (face faces)
      (widget-put (cdr face) :custom-state 'saved)
      (custom-redraw-magic (cdr face)))))

(defun custom-theme-write-variables (theme vars)
  "Write a `custom-theme-set-variables' command for THEME.
It includes all variables in list VARS."
  (when vars
    (let ((standard-output (current-buffer)))
      (princ "\n(custom-theme-set-variables\n")
      (princ " '")
      (princ theme)
      (princ "\n")
      (mapc (lambda (spec)
	      (let* ((symbol (car spec))
		     (child (car-safe (widget-get (cdr spec) :children)))
		     (value (if child
				(widget-value child)
			      ;; For hidden widgets, use the standard value
			      (get symbol 'standard-value))))
		(when (boundp symbol)
		  (unless (bolp)
		    (princ "\n"))
		  (princ " '(")
		  (prin1 symbol)
		  (princ " ")
		  (prin1 (custom-quote value))
		  (princ ")"))))
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
      (mapc (lambda (spec)
	      (let* ((symbol (car spec))
		     (child (car-safe (widget-get (cdr spec) :children)))
		     (value (if child (widget-value child))))
		(when (and (facep symbol) child)
		  (unless (bolp)
		    (princ "\n"))
		  (princ " '(")
		  (prin1 symbol)
		  (princ " ")
		  (prin1 value)
		  (princ ")"))))
	    faces)
      (if (bolp)
	  (princ " "))
      (princ ")")
      (unless (looking-at "\n")
	(princ "\n")))))

;; arch-tag: cd6919bc-63af-410e-bae2-b6702e762344
;;; cus-theme.el ends here
