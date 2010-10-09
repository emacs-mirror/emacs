;;; cus-theme.el -- custom theme creation user interface
;;
;; Copyright (C) 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007, 2008, 2009, 2010 Free Software Foundation, Inc.
;;
;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: FSF
;; Keywords: help, faces
;; Package: emacs

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

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
  (set (make-local-variable 'revert-buffer-function) 'custom-theme-revert)
  (when custom-raised-buttons
    (set (make-local-variable 'widget-push-button-prefix) "")
    (set (make-local-variable 'widget-push-button-suffix) "")
    (set (make-local-variable 'widget-link-prefix) "")
    (set (make-local-variable 'widget-link-suffix) "")))
(put 'custom-new-theme-mode 'mode-class 'special)

(defvar custom-theme-name nil)
(defvar custom-theme-variables nil)
(defvar custom-theme-faces nil)
(defvar custom-theme-description nil)
(defvar custom-theme-insert-variable-marker nil)
(defvar custom-theme-insert-face-marker nil)

(defvar custom-theme--listed-faces '(default fixed-pitch
  variable-pitch escape-glyph minibuffer-prompt highlight region
  shadow secondary-selection trailing-whitespace
  font-lock-builtin-face font-lock-comment-delimiter-face
  font-lock-comment-face font-lock-constant-face
  font-lock-doc-face font-lock-function-name-face
  font-lock-keyword-face font-lock-negation-char-face
  font-lock-preprocessor-face font-lock-regexp-grouping-backslash
  font-lock-regexp-grouping-construct font-lock-string-face
  font-lock-type-face font-lock-variable-name-face
  font-lock-warning-face button link link-visited fringe
  header-line tooltip mode-line mode-line-buffer-id
  mode-line-emphasis mode-line-highlight mode-line-inactive
  isearch isearch-fail lazy-highlight match next-error
  query-replace)
  "Faces listed by default in the *Custom Theme* buffer.")

;;;###autoload
(defun customize-create-theme (&optional buffer)
  "Create a custom theme.
BUFFER, if non-nil, should be a buffer to use."
  (interactive)
  (switch-to-buffer (or buffer (generate-new-buffer "*Custom Theme*")))
  ;; Save current faces
  (let ((inhibit-read-only t))
    (erase-buffer))
  (custom-new-theme-mode)
  (make-local-variable 'custom-theme-name)
  (set (make-local-variable 'custom-theme-faces) nil)
  (set (make-local-variable 'custom-theme-variables) nil)
  (set (make-local-variable 'custom-theme-description) "")
  (make-local-variable 'custom-theme-insert-face-marker)
  (make-local-variable 'custom-theme-insert-variable-marker)
  (make-local-variable 'custom-theme--listed-faces)

  (widget-create 'push-button
		 :tag " Visit Theme "
		 :help-echo "Insert the settings of a pre-defined theme."
		 :action (lambda (widget &optional event)
			   (call-interactively 'custom-theme-visit-theme)))
  (widget-insert "  ")
  (widget-create 'push-button
		 :tag " Merge Theme "
		 :help-echo "Merge in the settings of a pre-defined theme."
		 :action (lambda (widget &optional event)
			   (call-interactively 'custom-theme-merge-theme)))
  (widget-insert "  ")
  (widget-create 'push-button :notify 'revert-buffer " Revert ")

  (widget-insert "\n\nTheme name : ")
  (setq custom-theme-name
	(widget-create 'editable-field))
  (widget-insert "Description: ")
  (setq custom-theme-description
	(widget-create 'text
		       :value (format-time-string "Created %Y-%m-%d.")))
  (widget-insert "             ")
  (widget-create 'push-button
     		 :notify (function custom-theme-write)
     		 " Save Theme ")
  ;; Face widgets
  (widget-insert "\n\n  Theme faces:\n")
  (let (widget)
    (dolist (face custom-theme--listed-faces)
      (widget-insert "  ")
      (setq widget (widget-create 'custom-face
				  :documentation-shown t
				  :tag (custom-unlispify-tag-name face)
				  :value face
				  :display-style 'concise
				  :custom-state 'hidden
				  :sample-indent 34))
      (custom-magic-reset widget)
      (push (cons face widget) custom-theme-faces)))
  (insert " ")
  (setq custom-theme-insert-face-marker (point-marker))
  (insert " ")
  (widget-create 'push-button
		 :tag "Insert Additional Face"
		 :help-echo "Add another face to this theme."
		 :follow-link 'mouse-face
		 :button-face 'custom-link
		 :mouse-face 'highlight
		 :pressed-face 'highlight
		 :action (lambda (widget &optional event)
			   (call-interactively 'custom-theme-add-face)))
  (widget-insert "\n\n  Theme variables:\n ")
  (setq custom-theme-insert-variable-marker (point-marker))
  (widget-insert ?\s)
  (widget-create 'push-button
		 :tag "Insert Variable"
		 :help-echo "Add another variable to this theme."
		 :follow-link 'mouse-face
		 :button-face 'custom-link
		 :mouse-face 'highlight
		 :pressed-face 'highlight
		 :action (lambda (widget &optional event)
			   (call-interactively 'custom-theme-add-variable)))
  (widget-insert ?\n)
  (widget-setup)
  (goto-char (point-min))
  (message ""))

(defun custom-theme-revert (ignore-auto noconfirm)
  (when (or noconfirm (y-or-n-p "Discard current changes? "))
    (erase-buffer)
    (customize-create-theme (current-buffer))))

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
	   (widget-insert " ")
	   (let ((widget (widget-create 'custom-variable
					:tag (custom-unlispify-tag-name symbol)
					:custom-level 0
					:action 'custom-theme-variable-action
					:custom-state 'unknown
					:value symbol)))
	     (push (cons symbol widget) custom-theme-variables)
	     (custom-magic-reset widget))
	   (widget-insert " ")
	   (move-marker custom-theme-insert-variable-marker (point))
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
	   (widget-insert " ")
	   (let ((widget (widget-create 'custom-face
					:tag (custom-unlispify-tag-name symbol)
					:custom-level 0
					:action 'custom-theme-face-action
					:custom-state 'unknown
					:display-style 'concise
					:sample-indent 34
					:value symbol)))
	     (push (cons symbol widget) custom-theme-faces)
	     (custom-magic-reset widget)
	     (widget-insert " ")
	     (move-marker custom-theme-insert-face-marker (point))
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
  (when (or (and (null custom-theme-variables)
		 (null custom-theme-faces))
	    (and (y-or-n-p "Discard current changes? ")
		 (progn (revert-buffer) t)))
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
	 (doc (widget-value custom-theme-description))
	 (vars  custom-theme-variables)
	 (faces custom-theme-faces)
	 filename)
    (when (string-equal name "")
      (setq name (read-from-minibuffer "Theme name: " (user-login-name)))
      (widget-value-set custom-theme-name name))
    (cond ((or (string-equal name "")
	       (string-equal name "user")
	       (string-equal name "changed"))
	   (error "Custom themes cannot be named `%s'" name))
	  ((string-match " " name)
	   (error "Custom theme names should not contain spaces")))

    (setq filename (expand-file-name (concat name "-theme.el")
				     custom-theme-directory))
    (and (file-exists-p filename)
	 (not (y-or-n-p (format "File %s exists.  Overwrite? " filename)))
	 (error "Aborted"))

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
      (when (widget-get (cdr var) :children)
	(widget-put (cdr var) :custom-state 'saved)
	(custom-redraw-magic (cdr var))))
    (dolist (face custom-theme-faces)
      (when (widget-get (cdr face) :children)
	(widget-put (cdr face) :custom-state 'saved)
	(custom-redraw-magic (cdr face))))))

(defun custom-theme-write-variables (theme vars)
  "Write a `custom-theme-set-variables' command for THEME.
It includes all variables in list VARS."
  (when vars
    (let ((standard-output (current-buffer)))
      (princ "\n(custom-theme-set-variables\n")
      (princ " '")
      (princ theme)
      (princ "\n")
      (dolist (spec vars)
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
      (dolist (spec faces)
	(let* ((symbol (car spec))
	       (widget (cdr spec))
	       (child  (car-safe (widget-get widget :children)))
	       (state  (if child
			   (widget-get widget :custom-state)
			 (custom-face-state symbol)))
	       (value
		(cond ((eq state 'standard)
		       nil) ; do nothing
		      (child
		       (custom-face-widget-to-spec widget))
		      (t
		       ;; Widget is closed (hidden), but the face has
		       ;; a non-standard value.  Try to extract that
		       ;; value and save it.
		       (custom-face-get-current-spec symbol)))))
	  (when (and (facep symbol) value)
	    (if (bolp)
		(princ " '(")
	      (princ "\n '("))
	    (prin1 symbol)
	    (princ " ")
	    (prin1 value)
	    (princ ")"))))
      (if (bolp)
	  (princ " "))
      (princ ")")
      (unless (looking-at "\n")
	(princ "\n")))))

;; arch-tag: cd6919bc-63af-410e-bae2-b6702e762344
;;; cus-theme.el ends here
