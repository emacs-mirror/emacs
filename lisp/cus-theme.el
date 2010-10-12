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
    (define-key map "\C-x\C-s" 'custom-theme-write)
    (define-key map "n" 'widget-forward)
    (define-key map "p" 'widget-backward)
    map)
  "Keymap for `custom-new-theme-mode'.")

(define-derived-mode custom-new-theme-mode nil "Cus-Theme"
  "Major mode for editing Custom themes.
Do not call this mode function yourself.  It is meant for internal use."
  (use-local-map custom-new-theme-mode-map)
  (custom--initialize-widget-variables)
  (set (make-local-variable 'revert-buffer-function) 'custom-theme-revert))
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

(defvar custom-theme--save-name)

;;;###autoload
(defun customize-create-theme (&optional theme buffer)
  "Create or edit a custom theme.
THEME, if non-nil, should be an existing theme to edit.
BUFFER, if non-nil, should be a buffer to use."
  (interactive)
  (switch-to-buffer (get-buffer-create (or buffer "*Custom Theme*")))
  ;; Save current faces
  (let ((inhibit-read-only t))
    (erase-buffer))
  (custom-new-theme-mode)
  (make-local-variable 'custom-theme-name)
  (set (make-local-variable 'custom-theme--save-name) theme)
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
	(widget-create 'editable-field
		       :value (if theme (symbol-name theme) "")))
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
  (if theme
      (custom-theme-merge-theme theme))
  (widget-setup)
  (goto-char (point-min))
  (message ""))

(defun custom-theme-revert (ignore-auto noconfirm)
  (when (or noconfirm (y-or-n-p "Discard current changes? "))
    (customize-create-theme custom-theme--save-name (current-buffer))))

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
  (when (and (y-or-n-p "Discard current changes? ")
	     (progn (revert-buffer) t))
    (let ((theme (call-interactively 'custom-theme-merge-theme)))
      (unless (eq theme 'user)
	(widget-value-set custom-theme-name (symbol-name theme)))
      (widget-value-set custom-theme-description
			(or (get theme 'theme-documentation)
			    (format-time-string "Created %Y-%m-%d.")))
      (widget-setup))))

(defun custom-theme-merge-theme (theme)
  (interactive
   (list
    (intern (completing-read "Merge custom theme: "
			     (mapcar 'symbol-name
				     (custom-available-themes))))))
  (unless (custom-theme-name-valid-p theme)
    (error "Invalid theme name `%s'" theme))
  (load-theme theme)
  (let ((settings (get theme 'theme-settings)))
    (dolist (setting settings)
      (if (eq (car setting) 'theme-value)
	  (custom-theme-add-variable (cadr setting))
	(custom-theme-add-face (cadr setting)))))
  (disable-theme theme)
  theme)

(defun custom-theme-write (&rest ignore)
  (interactive)
  (let* ((name (widget-value custom-theme-name))
	 (doc (widget-value custom-theme-description))
	 (vars  custom-theme-variables)
	 (faces custom-theme-faces)
	 filename)
    (when (string-equal name "")
      (setq name (read-from-minibuffer "Theme name: " (user-login-name)))
      (widget-value-set custom-theme-name name))
    (unless (custom-theme-name-valid-p (intern name))
      (error "Custom themes cannot be named `%s'" name))

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
	(custom-redraw-magic (cdr face))))
    (message "Theme written to %s" filename)))

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


;;; Describing Custom themes.

;;;###autoload
(defun describe-theme (theme)
  "Display a description of the Custom theme THEME (a symbol)."
  (interactive
   (list
    (intern (completing-read "Describe custom theme: "
			     (mapcar 'symbol-name
				     (custom-available-themes))))))
  (unless (custom-theme-name-valid-p theme)
    (error "Invalid theme name `%s'" theme))
  (help-setup-xref (list 'describe-theme theme)
		   (called-interactively-p 'interactive))
  (with-help-window (help-buffer)
    (with-current-buffer standard-output
      (describe-theme-1 theme))))

(defun describe-theme-1 (theme)
  (prin1 theme)
  (princ " is a custom theme")
  (let ((fn (locate-file (concat (symbol-name theme) "-theme.el")
			 (cons custom-theme-directory load-path)
			 '("" "c"))))
    (when fn
      (princ " in `")
      (help-insert-xref-button (file-name-nondirectory fn)
			       'help-theme-def fn)
      (princ "'"))
    (princ ".\n"))
  (if (not (memq theme custom-known-themes))
      (princ "It is not loaded.")
    (if (custom-theme-enabled-p theme)
	(princ "It is loaded and enabled.\n")
      (princ "It is loaded but disabled.\n"))
    (princ "\nDocumentation:\n")
    (princ (or (get theme 'theme-documentation)
	       "No documentation available.")))
  (princ "\n\nYou can ")
  (help-insert-xref-button "customize" 'help-theme-edit theme)
  (princ " this theme."))


;;; Theme chooser

(defvar custom--listed-themes)

(defcustom custom-theme-allow-multiple-selections nil
  "Whether to allow multi-selections in the *Custom Themes* buffer."
  :type 'boolean
  :group 'custom-buffer)

(defvar custom-theme-choose-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map widget-keymap)
    (suppress-keymap map)
    (define-key map "\C-x\C-s" 'custom-theme-save)
    (define-key map "n" 'widget-forward)
    (define-key map "p" 'widget-backward)
    (define-key map "?" 'custom-describe-theme)
    map)
  "Keymap for `custom-theme-choose-mode'.")

(define-derived-mode custom-theme-choose-mode nil "Cus-Theme"
  "Major mode for selecting Custom themes.
Do not call this mode function yourself.  It is meant for internal use."
  (use-local-map custom-theme-choose-mode-map)
  (custom--initialize-widget-variables)
  (set (make-local-variable 'revert-buffer-function)
       (lambda (ignore-auto noconfirm)
	 (when (or noconfirm (y-or-n-p "Discard current choices? "))
	   (customize-themes (current-buffer))))))
(put 'custom-theme-choose-mode 'mode-class 'special)

;;;###autoload
(defun customize-themes (&optional buffer)
  "Display a selectable list of Custom themes.
When called from Lisp, BUFFER should be the buffer to use; if
omitted, a buffer named *Custom Themes* is used."
  (interactive)
  (pop-to-buffer (get-buffer-create (or buffer "*Custom Themes*")))
  (let ((inhibit-read-only t))
    (erase-buffer))
  (custom-theme-choose-mode)
  (set (make-local-variable 'custom--listed-themes) nil)
  (make-local-variable 'custom-theme-allow-multiple-selections)
  (and (null custom-theme-allow-multiple-selections)
       (> (length custom-enabled-themes) 1)
       (setq custom-theme-allow-multiple-selections t))

  (widget-insert
   (substitute-command-keys
    "Type RET or click to enable/disable listed custom themes.
Type \\[custom-describe-theme] to describe the theme at point.
Theme files are named *-theme.el in `"))
  (when (stringp custom-theme-directory)
    (widget-create 'link :value custom-theme-directory
		   :button-face 'custom-link
		   :mouse-face 'highlight
		   :pressed-face 'highlight
		   :help-echo "Describe `custom-theme-directory'."
		   :keymap custom-mode-link-map
		   :follow-link 'mouse-face
		   :action (lambda (widget &rest ignore)
			     (describe-variable 'custom-theme-directory)))
    (widget-insert "' or `"))
  (widget-create 'link :value "load-path"
		 :button-face 'custom-link
		 :mouse-face 'highlight
		 :pressed-face 'highlight
		 :help-echo "Describe `load-path'."
		 :keymap custom-mode-link-map
		 :follow-link 'mouse-face
		 :action (lambda (widget &rest ignore)
			   (describe-variable 'load-path)))
  (widget-insert "'.\n\n")
  (widget-create 'push-button
		 :tag " Save Theme Settings "
		 :help-echo "Save the selected themes for future sessions."
		 :action 'custom-theme-save)
  (widget-insert ?\n)
  (widget-create 'checkbox
		 :value custom-theme-allow-multiple-selections
		 :action 'custom-theme-selections-toggle)
  (widget-insert (propertize " Allow more than one theme at a time"
			     'face '(variable-pitch (:height 0.9))))

  (widget-insert "\n\nAvailable Custom Themes:\n")
  (let (widget)
    (dolist (theme (custom-available-themes))
      (setq widget (widget-create 'checkbox
				  :value (custom-theme-enabled-p theme)
				  :theme-name theme
				  :action 'custom-theme-checkbox-toggle))
      (push (cons theme widget) custom--listed-themes)
      (widget-create-child-and-convert widget 'push-button
				       :button-face-get 'ignore
				       :mouse-face-get 'ignore
				       :value (format " %s" theme)
				       :action 'widget-parent-action)
      (widget-insert ?\n)))
  (goto-char (point-min))
  (widget-setup))

(defun custom-theme-checkbox-toggle (widget &optional event)
  (let ((this-theme (widget-get widget :theme-name)))
    (if (widget-value widget)
	;; Disable the theme.
	(disable-theme this-theme)
      ;; Enable the theme.
      (unless custom-theme-allow-multiple-selections
	;; If only one theme is allowed, disable all other themes and
	;; uncheck their boxes.
	(dolist (theme custom-enabled-themes)
	  (and (not (eq theme this-theme))
	       (assq theme custom--listed-themes)
	       (disable-theme theme)))
	(dolist (theme custom--listed-themes)
	  (unless (eq (car theme) this-theme)
	    (widget-value-set (cdr theme) nil)
	    (widget-apply (cdr theme) :notify (cdr theme) event))))
      (load-theme this-theme)))
  ;; Mark `custom-enabled-themes' as "set for current session".
  (put 'custom-enabled-themes 'customized-value
       (list (custom-quote custom-enabled-themes)))
  ;; Check/uncheck the widget.
  (widget-toggle-action widget event))

(defun custom-describe-theme ()
  "Describe the Custom theme on the current line."
  (interactive)
  (let ((widget (widget-at (line-beginning-position))))
    (and widget
	 (describe-theme (widget-get widget :theme-name)))))

(defun custom-theme-save (&rest ignore)
  (interactive)
  (customize-save-variable 'custom-enabled-themes custom-enabled-themes)
  (message "Custom themes saved for future sessions."))

(defun custom-theme-selections-toggle (widget &optional event)
  (when (widget-value widget)
    ;; Deactivate multiple-selections.
    (if (> (length (delq nil (mapcar (lambda (x) (widget-value (cdr x)))
				     custom--listed-themes)))
	   1)
	(error "More than one theme is currently selected")))
  (widget-toggle-action widget event)
  (setq custom-theme-allow-multiple-selections (widget-value widget)))

;; arch-tag: cd6919bc-63af-410e-bae2-b6702e762344
;;; cus-theme.el ends here
