;;; wid-edit.el --- Functions for creating and using widgets -*-byte-compile-dynamic: t;-*-
;;
;; Copyright (C) 1996, 1997, 1999, 2000, 2001, 2002 Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Maintainer: FSF
;; Keywords: extensions

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

;;; Wishlist items (from widget.texi):

;; * The `menu-choice' tag should be prettier, something like the
;;   abbreviated menus in Open Look.

;; * Finish `:tab-order'.

;; * Make indentation work with glyphs and proportional fonts.

;; * Add commands to show overview of object and class hierarchies to
;;   the browser.

;; * Find a way to disable mouse highlight for inactive widgets.

;; * Find a way to make glyphs look inactive.

;; * Add `key-binding' widget.

;; * Add `widget' widget for editing widget specifications.

;; * Find clean way to implement variable length list.  See
;;   `TeX-printer-list' for an explanation.

;; * `C-h' in `widget-prompt-value' should give type specific help.

;; * A mailto widget. [This should work OK as a url-link if with
;;   browse-url-browser-function' set up appropriately.]

;;; Commentary:
;;
;; See `widget.el'.

;;; Code:

;;; Compatibility.

(defun widget-event-point (event)
  "Character position of the end of event if that exists, or nil."
  (posn-point (event-end event)))

(autoload 'pp-to-string "pp")
(autoload 'Info-goto-node "info")

(defun widget-button-release-event-p (event)
  "Non-nil if EVENT is a mouse-button-release event object."
  (and (eventp event)
       (memq (event-basic-type event) '(mouse-1 mouse-2 mouse-3))
       (or (memq 'click (event-modifiers event))
	   (memq  'drag (event-modifiers event)))))

;;; Customization.

(defgroup widgets nil
  "Customization support for the Widget Library."
  :link '(custom-manual "(widget)Top")
  :link '(emacs-library-link :tag "Lisp File" "widget.el")
  :prefix "widget-"
  :group 'extensions
  :group 'hypermedia)

(defgroup widget-documentation nil
  "Options controling the display of documentation strings."
  :group 'widgets)

(defgroup widget-faces nil
  "Faces used by the widget library."
  :group 'widgets
  :group 'faces)

(defvar widget-documentation-face 'widget-documentation-face
  "Face used for documentation strings in widgets.
This exists as a variable so it can be set locally in certain buffers.")

(defface widget-documentation-face '((((class color)
				       (background dark))
				      (:foreground "lime green"))
				     (((class color)
				       (background light))
				      (:foreground "dark green"))
				     (t nil))
  "Face used for documentation text."
  :group 'widget-documentation
  :group 'widget-faces)

(defvar widget-button-face 'widget-button-face
  "Face used for buttons in widgets.
This exists as a variable so it can be set locally in certain buffers.")

(defface widget-button-face '((t (:weight bold)))
  "Face used for widget buttons."
  :group 'widget-faces)

(defcustom widget-mouse-face 'highlight
  "Face used for widget buttons when the mouse is above them."
  :type 'face
  :group 'widget-faces)

;; TTY gets special definitions here and in the next defface, because
;; the gray colors defined for other displays cause black text on a black
;; background, at least on light-background TTYs.
(defface widget-field-face '((((type tty))
			      :background "yellow3"
			      :foreground "black")
			     (((class grayscale color)
			       (background light))
			      :background "gray85")
			     (((class grayscale color)
			       (background dark))
			      :background "dim gray")
			     (t
			      :slant italic))
  "Face used for editable fields."
  :group 'widget-faces)

(defface widget-single-line-field-face '((((type tty))
					  :background "green3"
					  :foreground "black")
					 (((class grayscale color)
					   (background light))
					  :background "gray85")
					 (((class grayscale color)
					   (background dark))
					  :background "dim gray")
					 (t
					  :slant italic))
  "Face used for editable fields spanning only a single line."
  :group 'widget-faces)

;;; This causes display-table to be loaded, and not usefully.
;;;(defvar widget-single-line-display-table
;;;  (let ((table (make-display-table)))
;;;    (aset table 9  "^I")
;;;    (aset table 10 "^J")
;;;    table)
;;;  "Display table used for single-line editable fields.")

;;;(when (fboundp 'set-face-display-table)
;;;  (set-face-display-table 'widget-single-line-field-face
;;;			  widget-single-line-display-table))

;;; Utility functions.
;;
;; These are not really widget specific.

(defun widget-princ-to-string (object)
  "Return string representation of OBJECT, any Lisp object.
No quoting characters are used; no delimiters are printed around
the contents of strings."
  (with-output-to-string
      (princ object)))

(defun widget-clear-undo ()
  "Clear all undo information."
  (buffer-disable-undo (current-buffer))
  (buffer-enable-undo))

(defcustom widget-menu-max-size 40
  "Largest number of items allowed in a popup-menu.
Larger menus are read through the minibuffer."
  :group 'widgets
  :type 'integer)

(defcustom widget-menu-max-shortcuts 40
  "Largest number of items for which it works to choose one with a character.
For a larger number of items, the minibuffer is used."
  :group 'widgets
  :type 'integer)

(defcustom widget-menu-minibuffer-flag nil
  "*Control how to ask for a choice from the keyboard.
Non-nil means use the minibuffer;
nil means read a single character."
  :group 'widgets
  :type 'boolean)

(defun widget-choose (title items &optional event)
  "Choose an item from a list.

First argument TITLE is the name of the list.
Second argument ITEMS is an list whose members are either
 (NAME . VALUE), to indicate selectable items, or just strings to
 indicate unselectable items.
Optional third argument EVENT is an input event.

The user is asked to choose between each NAME from the items alist,
and the VALUE of the chosen element will be returned.  If EVENT is a
mouse event, and the number of elements in items is less than
`widget-menu-max-size', a popup menu will be used, otherwise the
minibuffer."
  (cond ((and (< (length items) widget-menu-max-size)
	      event (display-popup-menus-p))
	 ;; Mouse click.
	 (x-popup-menu event
		       (list title (cons "" items))))
	((or widget-menu-minibuffer-flag
	     (> (length items) widget-menu-max-shortcuts))
	 ;; Read the choice of name from the minibuffer.
	 (setq items (widget-remove-if 'stringp items))
	 (let ((val (completing-read (concat title ": ") items nil t)))
	   (if (stringp val)
	       (let ((try (try-completion val items)))
		 (when (stringp try)
		   (setq val try))
		 (cdr (assoc val items))))))
	(t
	 ;; Construct a menu of the choices
	 ;; and then use it for prompting for a single character.
	 (let* ((overriding-terminal-local-map (make-sparse-keymap))
		(next-digit ?0)
		map choice some-choice-enabled value)
	   ;; Define SPC as a prefix char to get to this menu.
	   (define-key overriding-terminal-local-map " "
	     (setq map (make-sparse-keymap title)))
	   (save-excursion
	     (set-buffer (get-buffer-create " widget-choose"))
	     (erase-buffer)
	     (insert "Available choices:\n\n")
	     (while items
	       (setq choice (car items) items (cdr items))
	       (if (consp choice)
		   (let* ((name (car choice))
			 (function (cdr choice)))
		     (insert (format "%c = %s\n" next-digit name))
		     (define-key map (vector next-digit) function)
		     (setq some-choice-enabled t)))
	       ;; Allocate digits to disabled alternatives
	       ;; so that the digit of a given alternative never varies.
	       (setq next-digit (1+ next-digit)))
	     (insert "\nC-g = Quit"))
	   (or some-choice-enabled
	       (error "None of the choices is currently meaningful"))
	   (define-key map [?\C-g] 'keyboard-quit)
	   (define-key map [t] 'keyboard-quit)
	   (define-key map [?\M-\C-v] 'scroll-other-window)
	   (define-key map [?\M--] 'negative-argument)
	   (setcdr map (nreverse (cdr map)))
	   ;; Read a char with the menu, and return the result
	   ;; that corresponds to it.
	   (save-window-excursion
	     (let ((buf (get-buffer " widget-choose")))
	       (fit-window-to-buffer (display-buffer buf))
	       (let ((cursor-in-echo-area t)
		     keys
		     (char 0)
		     (arg 1))
		 (while (not (or (and (>= char ?0) (< char next-digit))
				 (eq value 'keyboard-quit)))
		   ;; Unread a SPC to lead to our new menu.
		   (setq unread-command-events (cons ?\ unread-command-events))
		   (setq keys (read-key-sequence title))
		   (setq value
			 (lookup-key overriding-terminal-local-map keys t)
			 char (string-to-char (substring keys 1)))
		   (cond ((eq value 'scroll-other-window)
			  (let ((minibuffer-scroll-window
				 (get-buffer-window buf)))
			    (if (> 0 arg)
				(scroll-other-window-down
				 (window-height minibuffer-scroll-window))
			      (scroll-other-window))
			    (setq arg 1)))
			 ((eq value 'negative-argument)
			  (setq arg -1))
			 (t
			  (setq arg 1)))))))
	   (when (eq value 'keyboard-quit)
	     (error "Canceled"))
	   value))))

(defun widget-remove-if (predictate list)
  (let (result (tail list))
    (while tail
      (or (funcall predictate (car tail))
	  (setq result (cons (car tail) result)))
      (setq tail (cdr tail)))
    (nreverse result)))

;;; Widget text specifications.
;; 
;; These functions are for specifying text properties.

(defvar widget-field-add-space t
  "Non-nil means add extra space at the end of editable text fields.
If you don't add the space, it will become impossible to edit a zero
size field.")

(defvar widget-field-use-before-change t
  "Non-nil means use `before-change-functions' to track editable fields.
This enables the use of undo, but doesn't work on Emacs 19.34 and earlier.
Using before hooks also means that the :notify function can't know the
new value.")

(defun widget-specify-field (widget from to)
  "Specify editable button for WIDGET between FROM and TO."
  ;; Terminating space is not part of the field, but necessary in
  ;; order for local-map to work.  Remove next sexp if local-map works
  ;; at the end of the overlay.
  (save-excursion
    (goto-char to)
    (cond ((null (widget-get widget :size))
	   (forward-char 1))
	  (widget-field-add-space
	   (insert-and-inherit " ")))
    (setq to (point)))
  (let ((keymap (widget-get widget :keymap))
	(face (or (widget-get widget :value-face) 'widget-field-face))
	(help-echo (widget-get widget :help-echo))
	(rear-sticky
	 (or (not widget-field-add-space) (widget-get widget :size))))
    (if (functionp help-echo)
      (setq help-echo 'widget-mouse-help))    
    (when (= (char-before to) ?\n)
      ;; When the last character in the field is a newline, we want to
      ;; give it a `field' char-property of `boundary', which helps the
      ;; C-n/C-p act more naturally when entering/leaving the field.  We
     ;; do this by making a small secondary overlay to contain just that
      ;; one character.
      (let ((overlay (make-overlay (1- to) to nil t nil)))
	(overlay-put overlay 'field 'boundary)
	;; Use `local-map' here, not `keymap', so that normal editing
	;; works in the field when, say, Custom uses `suppress-keymap'.
	(overlay-put overlay 'local-map keymap)
	(overlay-put overlay 'face face)
	(overlay-put overlay 'help-echo help-echo))
      (setq to (1- to))
      (setq rear-sticky t))
    (let ((overlay (make-overlay from to nil nil rear-sticky)))
      (widget-put widget :field-overlay overlay)
      ;;(overlay-put overlay 'detachable nil)
      (overlay-put overlay 'field widget)
      (overlay-put overlay 'local-map keymap)
      (overlay-put overlay 'face face)
      (overlay-put overlay 'help-echo help-echo)))
  (widget-specify-secret widget))

(defun widget-specify-secret (field)
  "Replace text in FIELD with value of `:secret', if non-nil."
  (let ((secret (widget-get field :secret))
	(size (widget-get field :size)))
    (when secret
      (let ((begin (widget-field-start field))
	    (end (widget-field-end field)))
	(when size
	  (while (and (> end begin)
		      (eq (char-after (1- end)) ?\ ))
	    (setq end (1- end))))
	(while (< begin end)
	  (let ((old (char-after begin)))
	    (unless (eq old secret)
	      (subst-char-in-region begin (1+ begin) old secret)
	      (put-text-property begin (1+ begin) 'secret old))
	    (setq begin (1+ begin))))))))

(defun widget-specify-button (widget from to)
  "Specify button for WIDGET between FROM and TO."
  (let ((overlay (make-overlay from to nil t nil))
	(help-echo (widget-get widget :help-echo)))
    (widget-put widget :button-overlay overlay)
    (if (functionp help-echo)
      (setq help-echo 'widget-mouse-help))
    (overlay-put overlay 'button widget)
    (overlay-put overlay 'keymap (widget-get widget :keymap))
    ;; We want to avoid the face with image buttons.
    (unless (widget-get widget :suppress-face)
      (overlay-put overlay 'face (widget-apply widget :button-face-get))
      (overlay-put overlay 'mouse-face widget-mouse-face))
    (overlay-put overlay 'help-echo help-echo)))

(defun widget-mouse-help (window overlay point)
  "Help-echo callback for widgets whose :help-echo is a function."
  (with-current-buffer (overlay-buffer overlay)
    (let* ((widget (widget-at (overlay-start overlay)))
	   (help-echo (if widget (widget-get widget :help-echo))))
      (if (functionp help-echo)
	  (funcall help-echo widget)
	help-echo))))

(defun widget-specify-sample (widget from to)
  "Specify sample for WIDGET between FROM and TO."
  (let ((overlay (make-overlay from to nil t nil)))
    (overlay-put overlay 'face (widget-apply widget :sample-face-get))
    (widget-put widget :sample-overlay overlay)))

(defun widget-specify-doc (widget from to)
  "Specify documentation for WIDGET between FROM and TO."
  (let ((overlay (make-overlay from to nil t nil)))
    (overlay-put overlay 'widget-doc widget)
    (overlay-put overlay 'face widget-documentation-face)
    (widget-put widget :doc-overlay overlay)))

(defmacro widget-specify-insert (&rest form)
  "Execute FORM without inheriting any text properties."
  `(save-restriction
    (let ((inhibit-read-only t)
	  (inhibit-modification-hooks t)
	  result)
      (insert "<>")
      (narrow-to-region (- (point) 2) (point))
      (goto-char (1+ (point-min)))
      (setq result (progn ,@form))
      (delete-region (point-min) (1+ (point-min)))
      (delete-region (1- (point-max)) (point-max))
      (goto-char (point-max))
      result)))

(defface widget-inactive-face '((((class grayscale color)
				  (background dark))
				 (:foreground "light gray"))
				(((class grayscale color)
				  (background light))
				 (:foreground "dim gray"))
				(t
				 (:slant italic)))
  "Face used for inactive widgets."
  :group 'widget-faces)

(defun widget-specify-inactive (widget from to)
  "Make WIDGET inactive for user modifications."
  (unless (widget-get widget :inactive)
    (let ((overlay (make-overlay from to nil t nil)))
      (overlay-put overlay 'face 'widget-inactive-face)
      ;; This is disabled, as it makes the mouse cursor change shape.
      ;; (overlay-put overlay 'mouse-face 'widget-inactive-face)
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'priority 100)
      (overlay-put overlay 'modification-hooks '(widget-overlay-inactive))
      (widget-put widget :inactive overlay))))

(defun widget-overlay-inactive (&rest junk)
  "Ignoring the arguments, signal an error."
  (unless inhibit-read-only
    (error "The widget here is not active")))


(defun widget-specify-active (widget)
  "Make WIDGET active for user modifications."
  (let ((inactive (widget-get widget :inactive)))
    (when inactive
      (delete-overlay inactive)
      (widget-put widget :inactive nil))))

;;; Widget Properties.

(defsubst widget-type (widget)
  "Return the type of WIDGET, a symbol."
  (car widget))

;;;###autoload
(defun widgetp (widget)
  "Return non-nil iff WIDGET is a widget."
  (if (symbolp widget)
      (get widget 'widget-type)
    (and (consp widget)
	 (symbolp (car widget))
	 (get (car widget) 'widget-type))))

(defun widget-get-indirect (widget property)
  "In WIDGET, get the value of PROPERTY.
If the value is a symbol, return its binding.
Otherwise, just return the value."
  (let ((value (widget-get widget property)))
    (if (symbolp value)
	(symbol-value value)
      value)))

(defun widget-member (widget property)
  "Non-nil iff there is a definition in WIDGET for PROPERTY."
  (cond ((plist-member (cdr widget) property)
	 t)
	((car widget)
	 (widget-member (get (car widget) 'widget-type) property))
	(t nil)))

(defun widget-value (widget)
  "Extract the current value of WIDGET."
  (widget-apply widget
		:value-to-external (widget-apply widget :value-get)))

(defun widget-value-set (widget value)
  "Set the current value of WIDGET to VALUE."
  (widget-apply widget
		:value-set (widget-apply widget
					 :value-to-internal value)))

(defun widget-default-get (widget)
  "Extract the default value of WIDGET."
  (or (widget-get widget :value)
      (widget-apply widget :default-get)))

(defun widget-match-inline (widget vals)
  "In WIDGET, match the start of VALS."
  (cond ((widget-get widget :inline)
	 (widget-apply widget :match-inline vals))
	((and (listp vals)
	      (widget-apply widget :match (car vals)))
	 (cons (list (car vals)) (cdr vals)))
	(t nil)))

(defun widget-apply-action (widget &optional event)
  "Apply :action in WIDGET in response to EVENT."
  (if (widget-apply widget :active)
      (widget-apply widget :action event)
    (error "Attempt to perform action on inactive widget")))

;;; Helper functions.
;;
;; These are widget specific.

;;;###autoload
(defun widget-prompt-value (widget prompt &optional value unbound)
  "Prompt for a value matching WIDGET, using PROMPT.
The current value is assumed to be VALUE, unless UNBOUND is non-nil."
  (unless (listp widget)
    (setq widget (list widget)))
  (setq prompt (format "[%s] %s" (widget-type widget) prompt))
  (setq widget (widget-convert widget))
  (let ((answer (widget-apply widget :prompt-value prompt value unbound)))
    (unless (widget-apply widget :match answer)
      (error "Value does not match %S type" (car widget)))
    answer))

(defun widget-get-sibling (widget)
  "Get the item WIDGET is assumed to toggle.
This is only meaningful for radio buttons or checkboxes in a list."
  (let* ((children (widget-get (widget-get widget :parent) :children))
	 child)
    (catch 'child
      (while children
	(setq child (car children)
	      children (cdr children))
	(when (eq (widget-get child :button) widget)
	  (throw 'child child)))
      nil)))

(defun widget-map-buttons (function &optional buffer maparg)
  "Map FUNCTION over the buttons in BUFFER.
FUNCTION is called with the arguments WIDGET and MAPARG.

If FUNCTION returns non-nil, the walk is cancelled.

The arguments MAPARG, and BUFFER default to nil and (current-buffer),
respectively."
  (let ((cur (point-min))
	(widget nil)
	(parent nil)
	(overlays (if buffer
		      (save-excursion (set-buffer buffer) (overlay-lists))
		    (overlay-lists))))
    (setq overlays (append (car overlays) (cdr overlays)))
    (while (setq cur (pop overlays))
      (setq widget (overlay-get cur 'button))
      (if (and widget (funcall function widget maparg))
	  (setq overlays nil)))))

;;; Images.

(defcustom widget-image-directory (file-name-as-directory
				   (expand-file-name "custom" data-directory))
  "Where widget button images are located.
If this variable is nil, widget will try to locate the directory
automatically."
  :group 'widgets
  :type 'directory)

(defcustom widget-image-enable t
  "If non nil, use image buttons in widgets when available."
  :version "21.1"
  :group 'widgets
  :type 'boolean)

(defcustom widget-image-conversion
  '((xpm ".xpm") (gif ".gif") (png ".png") (jpeg ".jpg" ".jpeg")
    (xbm ".xbm"))
  "Conversion alist from image formats to file name suffixes."
  :group 'widgets
  :type '(repeat (cons :format "%v"
		       (symbol :tag "Image Format" unknown)
		       (repeat :tag "Suffixes"
			       (string :format "%v")))))

(defun widget-image-find (image)
  "Create a graphical button from IMAGE.
IMAGE should either already be an image, or be a file name sans
extension (xpm, xbm, gif, jpg, or png) located in
`widget-image-directory' or otherwise where `find-image' will find it."
  (cond ((not (and image widget-image-enable (display-graphic-p)))
	 ;; We don't want or can't use images.
	 nil)
	((and (consp image)
	      (eq 'image (car image)))
	 ;; Already an image spec.  Use it.
	 image)
	((stringp image)
	 ;; A string.  Look it up in relevant directories.
	 (let* ((load-path (cons widget-image-directory load-path))
		specs)
	   (dolist (elt widget-image-conversion)
	     (dolist (ext (cdr elt))
	       (push (list :type (car elt) :file (concat image ext)) specs)))
	   (setq specs (nreverse specs))
	   (find-image specs)))
	(t
	 ;; Oh well.
	 nil)))

(defvar widget-button-pressed-face 'widget-button-pressed-face
  "Face used for pressed buttons in widgets.
This exists as a variable so it can be set locally in certain
buffers.")

(defun widget-image-insert (widget tag image &optional down inactive)
  "In WIDGET, insert the text TAG or, if supported, IMAGE.
IMAGE should either be an image or an image file name sans extension
\(xpm, xbm, gif, jpg, or png) located in `widget-image-directory'.

Optional arguments DOWN and INACTIVE are used instead of IMAGE when the
button is pressed or inactive, respectively.  These are currently ignored."
  (if (and (display-graphic-p)
	   (setq image (widget-image-find image)))
      (progn (widget-put widget :suppress-face t)
	     (insert-image image
			   (propertize
			    tag 'mouse-face widget-button-pressed-face)))
    (insert tag)))

;;; Buttons.

(defgroup widget-button nil
  "The look of various kinds of buttons."
  :group 'widgets)

(defcustom widget-button-prefix ""
  "String used as prefix for buttons."
  :type 'string
  :group 'widget-button)

(defcustom widget-button-suffix ""
  "String used as suffix for buttons."
  :type 'string
  :group 'widget-button)

;;; Creating Widgets.

;;;###autoload
(defun widget-create (type &rest args)
  "Create widget of TYPE.
The optional ARGS are additional keyword arguments."
  (let ((widget (apply 'widget-convert type args)))
    (widget-apply widget :create)
    widget))

(defun widget-create-child-and-convert (parent type &rest args)
  "As part of the widget PARENT, create a child widget TYPE.
The child is converted, using the keyword arguments ARGS."
  (let ((widget (apply 'widget-convert type args)))
    (widget-put widget :parent parent)
    (unless (widget-get widget :indent)
      (widget-put widget :indent (+ (or (widget-get parent :indent) 0)
				    (or (widget-get widget :extra-offset) 0)
				    (widget-get parent :offset))))
    (widget-apply widget :create)
    widget))

(defun widget-create-child (parent type)
  "Create widget of TYPE."
  (let ((widget (copy-sequence type)))
    (widget-put widget :parent parent)
    (unless (widget-get widget :indent)
      (widget-put widget :indent (+ (or (widget-get parent :indent) 0)
				    (or (widget-get widget :extra-offset) 0)
				    (widget-get parent :offset))))
    (widget-apply widget :create)
    widget))

(defun widget-create-child-value (parent type value)
  "Create widget of TYPE with value VALUE."
  (let ((widget (copy-sequence type)))
    (widget-put widget :value (widget-apply widget :value-to-internal value))
    (widget-put widget :parent parent)
    (unless (widget-get widget :indent)
      (widget-put widget :indent (+ (or (widget-get parent :indent) 0)
				    (or (widget-get widget :extra-offset) 0)
				    (widget-get parent :offset))))
    (widget-apply widget :create)
    widget))

;;;###autoload
(defun widget-delete (widget)
  "Delete WIDGET."
  (widget-apply widget :delete))

(defun widget-convert (type &rest args)
  "Convert TYPE to a widget without inserting it in the buffer.
The optional ARGS are additional keyword arguments."
  ;; Don't touch the type.
  (let* ((widget (if (symbolp type)
		     (list type)
		   (copy-sequence type)))
	 (current widget)
	 (keys args))
    ;; First set the :args keyword.
    (while (cdr current)		;Look in the type.
      (if (keywordp (car (cdr current)))
	  (setq current (cdr (cdr current)))
	(setcdr current (list :args (cdr current)))
	(setq current nil)))
    (while args				;Look in the args.
      (if (keywordp (nth 0 args))
	  (setq args (nthcdr 2 args))
	(widget-put widget :args args)
	(setq args nil)))
    ;; Then Convert the widget.
    (setq type widget)
    (while type
      (let ((convert-widget (plist-get (cdr type) :convert-widget)))
	(if convert-widget
	    (setq widget (funcall convert-widget widget))))
      (setq type (get (car type) 'widget-type)))
    ;; Finally set the keyword args.
    (while keys
      (let ((next (nth 0 keys)))
	(if (keywordp next)
	    (progn
	      (widget-put widget next (nth 1 keys))
	      (setq keys (nthcdr 2 keys)))
	  (setq keys nil))))
    ;; Convert the :value to internal format.
    (if (widget-member widget :value)
	(widget-put widget
		    :value (widget-apply widget
					 :value-to-internal
					 (widget-get widget :value))))
    ;; Return the newly create widget.
    widget))

;;;###autoload
(defun widget-insert (&rest args)
  "Call `insert' with ARGS even if surrounding text is read only."
  (let ((inhibit-read-only t)
	(inhibit-modification-hooks t))
    (apply 'insert args)))

(defun widget-convert-text (type from to
				 &optional button-from button-to
				 &rest args)
  "Return a widget of type TYPE with endpoint FROM TO.
Optional ARGS are extra keyword arguments for TYPE.
and TO will be used as the widgets end points. If optional arguments
BUTTON-FROM and BUTTON-TO are given, these will be used as the widgets
button end points.
Optional ARGS are extra keyword arguments for TYPE."
  (let ((widget (apply 'widget-convert type :delete 'widget-leave-text args))
	(from (copy-marker from))
	(to (copy-marker to)))
    (set-marker-insertion-type from t)
    (set-marker-insertion-type to nil)
    (widget-put widget :from from)
    (widget-put widget :to to)
    (when button-from
      (widget-specify-button widget button-from button-to))
    widget))

(defun widget-convert-button (type from to &rest args)
  "Return a widget of type TYPE with endpoint FROM TO.
Optional ARGS are extra keyword arguments for TYPE.
No text will be inserted to the buffer, instead the text between FROM
and TO will be used as the widgets end points, as well as the widgets
button end points."
  (apply 'widget-convert-text type from to from to args))

(defun widget-leave-text (widget)
  "Remove markers and overlays from WIDGET and its children."
  (let ((button (widget-get widget :button-overlay))
	(sample (widget-get widget :sample-overlay))
	(doc (widget-get widget :doc-overlay))
	(field (widget-get widget :field-overlay)))
    (set-marker (widget-get widget :from) nil)
    (set-marker (widget-get widget :to) nil)
    (when button
      (delete-overlay button))
    (when sample
      (delete-overlay sample))
    (when doc
      (delete-overlay doc))
    (when field
      (delete-overlay field))
    (mapc 'widget-leave-text (widget-get widget :children))))

;;; Keymap and Commands.

;;;###autoload
(defvar widget-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'widget-forward)
    (define-key map [(shift tab)] 'widget-backward)
    (define-key map [backtab] 'widget-backward)
    (define-key map [down-mouse-2] 'widget-button-click)
    (define-key map "\C-m" 'widget-button-press)
    map)
  "Keymap containing useful binding for buffers containing widgets.
Recommended as a parent keymap for modes using widgets.")

(defvar widget-global-map global-map
  "Keymap used for events a widget does not handle itself.")
(make-variable-buffer-local 'widget-global-map)

(defvar widget-field-keymap
  (let ((map (copy-keymap widget-keymap)))
    (define-key map "\C-k" 'widget-kill-line)
    (define-key map "\M-\t" 'widget-complete)
    (define-key map "\C-m" 'widget-field-activate)
    ;; Since the widget code uses a `field' property to identify fields,
    ;; ordinary beginning-of-line does the right thing.
    ;;  (define-key map "\C-a" 'widget-beginning-of-line)
    (define-key map "\C-e" 'widget-end-of-line)
    map)
  "Keymap used inside an editable field.")

(defvar widget-text-keymap
  (let ((map (copy-keymap widget-keymap)))
    ;; Since the widget code uses a `field' property to identify fields,
    ;; ordinary beginning-of-line does the right thing.
    ;;  (define-key map "\C-a" 'widget-beginning-of-line)
    (define-key map "\C-e" 'widget-end-of-line)
    map)
  "Keymap used inside a text field.")

(defun widget-field-activate (pos &optional event)
  "Invoke the editable field at point."
  (interactive "@d")
  (let ((field (widget-field-at pos)))
    (if field
	(widget-apply-action field event)
      (call-interactively
       (lookup-key widget-global-map (this-command-keys))))))

(defface widget-button-pressed-face
  '((((class color))
     (:foreground "red"))
    (t
     (:weight bold :underline t)))
  "Face used for pressed buttons."
  :group 'widget-faces)

(defun widget-button-click (event)
  "Invoke the button that the mouse is pointing at."
  (interactive "e")
  (if (widget-event-point event)
      (let* ((pos (widget-event-point event))
	     (start (event-start event))
	     (button (get-char-property 
		      pos 'button (and (windowp (posn-window start))
				       (window-buffer (posn-window start))))))
	(if button
	    ;; Mouse click on a widget button.  Do the following
	    ;; in a save-excursion so that the click on the button
	    ;; doesn't change point.
	    (save-selected-window
	      (select-window (posn-window (event-start event)))
	      (save-excursion
		(goto-char (posn-point (event-start event)))
		(let* ((overlay (widget-get button :button-overlay))
		       (face (overlay-get overlay 'face))
		       (mouse-face (overlay-get overlay 'mouse-face)))
		  (unwind-protect
		      ;; Read events, including mouse-movement events
		      ;; until we receive a release event.  Highlight/
		      ;; unhighlight the button the mouse was initially
		      ;; on when we move over it.
		      (let ((track-mouse t))
			(save-excursion
			  (when face	; avoid changing around image
			    (overlay-put overlay
					 'face widget-button-pressed-face)
			    (overlay-put overlay
					 'mouse-face widget-button-pressed-face))
			  (unless (widget-apply button :mouse-down-action event)
			    (while (not (widget-button-release-event-p event))
			      (setq event (read-event)
				    pos (widget-event-point event))
			      (if (and pos
				       (eq (get-char-property pos 'button)
					   button))
				  (when face
				    (overlay-put overlay
						 'face
						 widget-button-pressed-face)
				    (overlay-put overlay
						 'mouse-face
						 widget-button-pressed-face))
				(overlay-put overlay 'face face)
				(overlay-put overlay 'mouse-face mouse-face))))

			  ;; When mouse is released over the button, run
			  ;; its action function.
			  (when (and pos
				     (eq (get-char-property pos 'button) button))
			    (widget-apply-action button event))))
		    (overlay-put overlay 'face face)
		    (overlay-put overlay 'mouse-face mouse-face))))

	      (unless (pos-visible-in-window-p (widget-event-point event))
		(mouse-set-point event)
		(beginning-of-line)
		(recenter))
	      )

	    (let ((up t) command)
	      ;; Mouse click not on a widget button.  Find the global
	      ;; command to run, and check whether it is bound to an
	      ;; up event.
	      (mouse-set-point event)
	      (if (memq (event-basic-type event) '(mouse-1 down-mouse-1))
		  (cond ((setq command	;down event
			       (lookup-key widget-global-map [down-mouse-1]))
			 (setq up nil))
			((setq command	;up event
			       (lookup-key widget-global-map [mouse-1]))))
		(cond ((setq command	;down event
			     (lookup-key widget-global-map [down-mouse-2]))
		       (setq up nil))
		      ((setq command	;up event
			     (lookup-key widget-global-map [mouse-2])))))
	      (when up
		;; Don't execute up events twice.
		(while (not (widget-button-release-event-p event))
		  (setq event (read-event))))
	      (when command
		(call-interactively command)))))
    (message "You clicked somewhere weird.")))

(defun widget-button-press (pos &optional event)
  "Invoke button at POS."
  (interactive "@d")
  (let ((button (get-char-property pos 'button)))
    (if button
	(widget-apply-action button event)
      (let ((command (lookup-key widget-global-map (this-command-keys))))
	(when (commandp command)
	  (call-interactively command))))))

(defun widget-tabable-at (&optional pos)
  "Return the tabable widget at POS, or nil.
POS defaults to the value of (point)."
  (let ((widget (widget-at pos)))
    (if widget
	(let ((order (widget-get widget :tab-order)))
	  (if order
	      (if (>= order 0)
		  widget)
	    widget)))))

(defvar widget-use-overlay-change t
  "If non-nil, use overlay change functions to tab around in the buffer.
This is much faster, but doesn't work reliably on Emacs 19.34.")

(defun widget-move (arg)
  "Move point to the ARG next field or button.
ARG may be negative to move backward."
  (or (bobp) (> arg 0) (backward-char))
  (let ((pos (point))
	(number arg)
	(old (widget-tabable-at))
	new)
    ;; Forward.
    (while (> arg 0)
      (cond ((eobp)
	     (goto-char (point-min)))
	    (widget-use-overlay-change
	     (goto-char (next-overlay-change (point))))
	    (t
	     (forward-char 1)))
      (and (eq pos (point))
	   (eq arg number)
	   (error "No buttons or fields found"))
      (let ((new (widget-tabable-at)))
	(when new
	  (unless (eq new old)
	    (setq arg (1- arg))
	    (setq old new)))))
    ;; Backward.
    (while (< arg 0)
      (cond ((bobp)
	     (goto-char (point-max)))
	    (widget-use-overlay-change
	     (goto-char (previous-overlay-change (point))))
	    (t
	     (backward-char 1)))
      (and (eq pos (point))
	   (eq arg number)
	   (error "No buttons or fields found"))
      (let ((new (widget-tabable-at)))
	(when new
	  (unless (eq new old)
	    (setq arg (1+ arg))))))
    (let ((new (widget-tabable-at)))
      (while (eq (widget-tabable-at) new)
	(backward-char)))
    (forward-char))
  (widget-echo-help (point))
  (run-hooks 'widget-move-hook))

(defun widget-forward (arg)
  "Move point to the next field or button.
With optional ARG, move across that many fields."
  (interactive "p")
  (run-hooks 'widget-forward-hook)
  (widget-move arg))

(defun widget-backward (arg)
  "Move point to the previous field or button.
With optional ARG, move across that many fields."
  (interactive "p")
  (run-hooks 'widget-backward-hook)
  (widget-move (- arg)))

;; Since the widget code uses a `field' property to identify fields,
;; ordinary beginning-of-line does the right thing.
(defalias 'widget-beginning-of-line 'beginning-of-line)

(defun widget-end-of-line ()
  "Go to end of field or end of line, whichever is first.
Trailing spaces at the end of padded fields are not considered part of
the field."
  (interactive)
  ;; Ordinary end-of-line does the right thing, because we're inside
  ;; text with a `field' property.
  (end-of-line)
  (unless (eolp)
    ;; ... except that we want to ignore trailing spaces in fields that
    ;; aren't terminated by a newline, because they are used as padding,
    ;; and ignored when extracting the entered value of the field.
    (skip-chars-backward " " (field-beginning (1- (point))))))

(defun widget-kill-line ()
  "Kill to end of field or end of line, whichever is first."
  (interactive)
  (let* ((field (widget-field-find (point)))
	 (end (and field (widget-field-end field))))
    (if (and field (> (line-beginning-position 2) end))
	(kill-region (point) end)
      (call-interactively 'kill-line))))

(defcustom widget-complete-field (lookup-key global-map "\M-\t")
  "Default function to call for completion inside fields."
  :options '(ispell-complete-word complete-tag lisp-complete-symbol)
  :type 'function
  :group 'widgets)

(defun widget-complete ()
  "Complete content of editable field from point.
When not inside a field, move to the previous button or field."
  (interactive)
  (let ((field (widget-field-find (point))))
    (if field
	(widget-apply field :complete)
      (error "Not in an editable field"))))

;;; Setting up the buffer.

(defvar widget-field-new nil)
;; List of all newly created editable fields in the buffer.
(make-variable-buffer-local 'widget-field-new)

(defvar widget-field-list nil)
;; List of all editable fields in the buffer.
(make-variable-buffer-local 'widget-field-list)

(defun widget-at (&optional pos)
  "The button or field at POS (default, point)."
  (or (get-char-property (or pos (point)) 'button)
      (widget-field-at pos)))

;;;###autoload
(defun widget-setup ()
  "Setup current buffer so editing string widgets works."
  (let ((inhibit-read-only t)
	(inhibit-modification-hooks t)
	field)
    (while widget-field-new
      (setq field (car widget-field-new)
	    widget-field-new (cdr widget-field-new)
	    widget-field-list (cons field widget-field-list))
      (let ((from (car (widget-get field :field-overlay)))
	    (to (cdr (widget-get field :field-overlay))))
	(widget-specify-field field
			      (marker-position from) (marker-position to))
	(set-marker from nil)
	(set-marker to nil))))
  (widget-clear-undo)
  (widget-add-change))

(defvar widget-field-last nil)
;; Last field containing point.
(make-variable-buffer-local 'widget-field-last)

(defvar widget-field-was nil)
;; The widget data before the change.
(make-variable-buffer-local 'widget-field-was)

(defun widget-field-at (pos)
  "Return the widget field at POS, or nil if none."
  (let ((field (get-char-property (or pos (point)) 'field)))
    (if (eq field 'boundary)
	nil
      field)))

(defun widget-field-buffer (widget)
  "Return the start of WIDGET's editing field."
  (let ((overlay (widget-get widget :field-overlay)))
    (cond ((overlayp overlay)
	   (overlay-buffer overlay))
	  ((consp overlay)
	   (marker-buffer (car overlay))))))

(defun widget-field-start (widget)
  "Return the start of WIDGET's editing field."
  (let ((overlay (widget-get widget :field-overlay)))
    (if (overlayp overlay)
	(overlay-start overlay)
      (car overlay))))

(defun widget-field-end (widget)
  "Return the end of WIDGET's editing field."
  (let ((overlay (widget-get widget :field-overlay)))
    ;; Don't subtract one if local-map works at the end of the overlay,
    ;; or if a special `boundary' field has been added after the widget
    ;; field.
    (if (overlayp overlay)
	(if (and (not (eq (get-char-property (overlay-end overlay)
					     'field
					     (widget-field-buffer widget))
			  'boundary))
		 (or widget-field-add-space
		     (null (widget-get widget :size))))
	    (1- (overlay-end overlay))
	  (overlay-end overlay))
      (cdr overlay))))

(defun widget-field-find (pos)
  "Return the field at POS.
Unlike (get-char-property POS 'field) this, works with empty fields too."
  (let ((fields widget-field-list)
	field found)
    (while fields
      (setq field (car fields)
	    fields (cdr fields))
      (when (and (<= (widget-field-start field) pos)
		 (<= pos (widget-field-end field)))
	(when found
	  (error "Overlapping fields"))
	(setq found field)))
    found))

(defun widget-before-change (from to)
  ;; This is how, for example, a variable changes its state to `modified'.
  ;; when it is being edited.
  (unless inhibit-read-only
    (let ((from-field (widget-field-find from))
	  (to-field (widget-field-find to)))
      (cond ((not (eq from-field to-field))
	     (add-hook 'post-command-hook 'widget-add-change nil t)
	     (signal 'text-read-only
		     '("Change should be restricted to a single field")))
	    ((null from-field)
	     (add-hook 'post-command-hook 'widget-add-change nil t)
	     (signal 'text-read-only
		     '("Attempt to change text outside editable field")))
	    (widget-field-use-before-change
	     (widget-apply from-field :notify from-field))))))

(defun widget-add-change ()
  (remove-hook 'post-command-hook 'widget-add-change t)
  (add-hook 'before-change-functions 'widget-before-change nil t)
  (add-hook 'after-change-functions 'widget-after-change nil t))

(defun widget-after-change (from to old)
  "Adjust field size and text properties."
  (let ((field (widget-field-find from))
	(other (widget-field-find to)))
    (when field
      (unless (eq field other)
	(error "Change in different fields"))
      (let ((size (widget-get field :size)))
	(when size
	  (let ((begin (widget-field-start field))
		(end (widget-field-end field)))
	    (cond ((< (- end begin) size)
		   ;; Field too small.
		   (save-excursion
		     (goto-char end)
		     (insert-char ?\  (- (+ begin size) end))))
		  ((> (- end begin) size)
		   ;; Field too large and
		   (if (or (< (point) (+ begin size))
			   (> (point) end))
		       ;; Point is outside extra space.
		       (setq begin (+ begin size))
		     ;; Point is within the extra space.
		     (setq begin (point)))
		   (save-excursion
		     (goto-char end)
		     (while (and (eq (preceding-char) ?\ )
				 (> (point) begin))
		       (delete-backward-char 1)))))))
	(widget-specify-secret field))
      (widget-apply field :notify field))))

;;; Widget Functions
;;
;; These functions are used in the definition of multiple widgets.

(defun widget-parent-action (widget &optional event)
  "Tell :parent of WIDGET to handle the :action.
Optional EVENT is the event that triggered the action."
  (widget-apply (widget-get widget :parent) :action event))

(defun widget-children-value-delete (widget)
  "Delete all :children and :buttons in WIDGET."
  (mapc 'widget-delete (widget-get widget :children))
  (widget-put widget :children nil)
  (mapc 'widget-delete (widget-get widget :buttons))
  (widget-put widget :buttons nil))

(defun widget-children-validate (widget)
  "All the :children must be valid."
  (let ((children (widget-get widget :children))
	child found)
    (while (and children (not found))
      (setq child (car children)
	    children (cdr children)
	    found (widget-apply child :validate)))
    found))

;; Made defsubst to speed up face editor creation.
(defsubst widget-types-convert-widget (widget)
  "Convert :args as widget types in WIDGET."
  (widget-put widget :args (mapcar 'widget-convert (widget-get widget :args)))
  widget)

(defun widget-value-convert-widget (widget)
  "Initialize :value from :args in WIDGET."
  (let ((args (widget-get widget :args)))
    (when args
      (widget-put widget :value (car args))
      ;; Don't convert :value here, as this is done in `widget-convert'.
      ;; (widget-put widget :value (widget-apply widget
      ;;  				      :value-to-internal (car args)))
      (widget-put widget :args nil)))
  widget)

(defun widget-value-value-get (widget)
  "Return the :value property of WIDGET."
  (widget-get widget :value))

;;; The `default' Widget.

(define-widget 'default nil
  "Basic widget other widgets are derived from."
  :value-to-internal (lambda (widget value) value)
  :value-to-external (lambda (widget value) value)
  :button-prefix 'widget-button-prefix
  :button-suffix 'widget-button-suffix
  :complete 'widget-default-complete
  :create 'widget-default-create
  :indent nil
  :offset 0
  :format-handler 'widget-default-format-handler
  :button-face-get 'widget-default-button-face-get 
  :sample-face-get 'widget-default-sample-face-get 
  :delete 'widget-default-delete
  :value-set 'widget-default-value-set
  :value-inline 'widget-default-value-inline
  :default-get 'widget-default-default-get
  :menu-tag-get 'widget-default-menu-tag-get
  :validate #'ignore
  :active 'widget-default-active
  :activate 'widget-specify-active
  :deactivate 'widget-default-deactivate
  :mouse-down-action #'ignore
  :action 'widget-default-action
  :notify 'widget-default-notify
  :prompt-value 'widget-default-prompt-value)

(defun widget-default-complete (widget)
  "Call the value of the :complete-function property of WIDGET.
If that does not exists, call the value of `widget-complete-field'."
  (call-interactively (or (widget-get widget :complete-function)
			  widget-complete-field)))

(defun widget-default-create (widget)
  "Create WIDGET at point in the current buffer."
  (widget-specify-insert
   (let ((from (point))
	 button-begin button-end
	 sample-begin sample-end
	 doc-begin doc-end
	 value-pos)
     (insert (widget-get widget :format))
     (goto-char from)
     ;; Parse escapes in format.
     (while (re-search-forward "%\\(.\\)" nil t)
       (let ((escape (char-after (match-beginning 1))))
	 (delete-backward-char 2)
	 (cond ((eq escape ?%)
		(insert ?%))
	       ((eq escape ?\[)
		(setq button-begin (point))
		(insert (widget-get-indirect widget :button-prefix)))
	       ((eq escape ?\])
		(insert (widget-get-indirect widget :button-suffix))
		(setq button-end (point)))
	       ((eq escape ?\{)
		(setq sample-begin (point)))
	       ((eq escape ?\})
		(setq sample-end (point)))
	       ((eq escape ?n)
		(when (widget-get widget :indent)
		  (insert ?\n)
		  (insert-char ?  (widget-get widget :indent))))
	       ((eq escape ?t)
		(let ((image (widget-get widget :tag-glyph))
		      (tag (widget-get widget :tag)))
		  (cond (image
			 (widget-image-insert widget (or tag "image") image))
			(tag
			 (insert tag))
			(t
			 (princ (widget-get widget :value)
				(current-buffer))))))
	       ((eq escape ?d)
		(let ((doc (widget-get widget :doc)))
		  (when doc
		    (setq doc-begin (point))
		    (insert doc)
		    (while (eq (preceding-char) ?\n)
		      (delete-backward-char 1))
		    (insert ?\n)
		    (setq doc-end (point)))))
	       ((eq escape ?v)
		(if (and button-begin (not button-end))
		    (widget-apply widget :value-create)
		  (setq value-pos (point))))
	       (t
		(widget-apply widget :format-handler escape)))))
     ;; Specify button, sample, and doc, and insert value.
     (and button-begin button-end
	  (widget-specify-button widget button-begin button-end))
     (and sample-begin sample-end
	  (widget-specify-sample widget sample-begin sample-end))
     (and doc-begin doc-end
	  (widget-specify-doc widget doc-begin doc-end))
     (when value-pos
       (goto-char value-pos)
       (widget-apply widget :value-create)))
   (let ((from (point-min-marker))
	 (to (point-max-marker)))
     (set-marker-insertion-type from t)
     (set-marker-insertion-type to nil)
     (widget-put widget :from from)
     (widget-put widget :to to)))
  (widget-clear-undo))

(defun widget-default-format-handler (widget escape)
  ;; We recognize the %h escape by default.
  (let* ((buttons (widget-get widget :buttons)))
    (cond ((eq escape ?h)
	   (let* ((doc-property (widget-get widget :documentation-property))
		  (doc-try (cond ((widget-get widget :doc))
				 ((functionp doc-property)
				  (funcall doc-property
					   (widget-get widget :value)))
				 ((symbolp doc-property)
				  (documentation-property
				   (widget-get widget :value)
				   doc-property))))
		  (doc-text (and (stringp doc-try)
				 (> (length doc-try) 1)
				 doc-try))
		  (doc-indent (widget-get widget :documentation-indent)))
	     (when doc-text
	       (and (eq (preceding-char) ?\n)
		    (widget-get widget :indent)
		    (insert-char ?  (widget-get widget :indent)))
	       ;; The `*' in the beginning is redundant.
	       (when (eq (aref doc-text  0) ?*)
		 (setq doc-text (substring doc-text 1)))
	       ;; Get rid of trailing newlines.
	       (when (string-match "\n+\\'" doc-text)
		 (setq doc-text (substring doc-text 0 (match-beginning 0))))
	       (push (widget-create-child-and-convert
		      widget 'documentation-string
		      :indent (cond ((numberp doc-indent )
				     doc-indent)
				    ((null doc-indent)
				     nil)
				    (t 0))
		      doc-text)
		     buttons))))
	  (t
	   (error "Unknown escape `%c'" escape)))
    (widget-put widget :buttons buttons)))

(defun widget-default-button-face-get (widget)
  ;; Use :button-face or widget-button-face
  (or (widget-get widget :button-face)
      (let ((parent (widget-get widget :parent)))
	(if parent
	    (widget-apply parent :button-face-get)
	  widget-button-face))))

(defun widget-default-sample-face-get (widget)
  ;; Use :sample-face.
  (widget-get widget :sample-face))

(defun widget-default-delete (widget)
  "Remove widget from the buffer."
  (let ((from (widget-get widget :from))
	(to (widget-get widget :to))
	(inactive-overlay (widget-get widget :inactive))
	(button-overlay (widget-get widget :button-overlay))
	(sample-overlay (widget-get widget :sample-overlay))
	(doc-overlay (widget-get widget :doc-overlay))
	(inhibit-modification-hooks t)
	(inhibit-read-only t))
    (widget-apply widget :value-delete)
    (when inactive-overlay
      (delete-overlay inactive-overlay))
    (when button-overlay
      (delete-overlay button-overlay))
    (when sample-overlay
      (delete-overlay sample-overlay))
    (when doc-overlay
      (delete-overlay doc-overlay))
    (when (< from to)
      ;; Kludge: this doesn't need to be true for empty formats.
      (delete-region from to))
    (set-marker from nil)
    (set-marker to nil))
  (widget-clear-undo))

(defun widget-default-value-set (widget value)
  "Recreate widget with new value."
  (let* ((old-pos (point))
	 (from (copy-marker (widget-get widget :from)))
	 (to (copy-marker (widget-get widget :to)))
	 (offset (if (and (<= from old-pos) (<= old-pos to))
		     (if (>= old-pos (1- to))
			 (- old-pos to 1)
		       (- old-pos from)))))
    ;;??? Bug: this ought to insert the new value before deleting the old one,
    ;; so that markers on either side of the value automatically
    ;; stay on the same side.  -- rms.
    (save-excursion
      (goto-char (widget-get widget :from))
      (widget-apply widget :delete)
      (widget-put widget :value value)
      (widget-apply widget :create))
    (if offset
	(if (< offset 0)
	    (goto-char (+ (widget-get widget :to) offset 1))
	  (goto-char (min (+ from offset) (1- (widget-get widget :to))))))))

(defun widget-default-value-inline (widget)
  "Wrap value in a list unless it is inline."
  (if (widget-get widget :inline)
      (widget-value widget)
    (list (widget-value widget))))

(defun widget-default-default-get (widget)
  "Get `:value'."
  (widget-get widget :value))

(defun widget-default-menu-tag-get (widget)
  "Use tag or value for menus."
  (or (widget-get widget :menu-tag)
      (widget-get widget :tag)
      (widget-princ-to-string (widget-get widget :value))))

(defun widget-default-active (widget)
  "Return t iff this widget active (user modifiable)."
  (or (widget-get widget :always-active)
      (and (not (widget-get widget :inactive))
	   (let ((parent (widget-get widget :parent)))
	     (or (null parent) 
		 (widget-apply parent :active))))))

(defun widget-default-deactivate (widget)
  "Make WIDGET inactive for user modifications."
  (widget-specify-inactive widget
			   (widget-get widget :from)
			   (widget-get widget :to)))

(defun widget-default-action (widget &optional event)
  "Notify the parent when a widget changes."
  (let ((parent (widget-get widget :parent)))
    (when parent
      (widget-apply parent :notify widget event))))

(defun widget-default-notify (widget child &optional event)
  "Pass notification to parent."
  (widget-default-action widget event))

(defun widget-default-prompt-value (widget prompt value unbound)
  "Read an arbitrary value.  Stolen from `set-variable'."
;; (let ((initial (if unbound
;; nil
;; It would be nice if we could do a `(cons val 1)' here.
;; (prin1-to-string (custom-quote value))))))
  (eval-minibuffer prompt))

;;; The `item' Widget.

(define-widget 'item 'default
  "Constant items for inclusion in other widgets."
  :convert-widget 'widget-value-convert-widget
  :value-create 'widget-item-value-create
  :value-delete 'ignore
  :value-get 'widget-value-value-get
  :match 'widget-item-match
  :match-inline 'widget-item-match-inline
  :action 'widget-item-action
  :format "%t\n")

(defun widget-item-value-create (widget)
  "Insert the printed representation of the value."
  (princ (widget-get widget :value) (current-buffer)))

(defun widget-item-match (widget value)
  ;; Match if the value is the same.
  (equal (widget-get widget :value) value))

(defun widget-item-match-inline (widget values)
  ;; Match if the value is the same.
  (let ((value (widget-get widget :value)))
    (and (listp value)
	 (<= (length value) (length values))
	 (let ((head (widget-sublist values 0 (length value))))
	   (and (equal head value)
		(cons head (widget-sublist values (length value))))))))

(defun widget-sublist (list start &optional end)
  "Return the sublist of LIST from START to END.
If END is omitted, it defaults to the length of LIST."
  (if (> start 0) (setq list (nthcdr start list)))
  (if end
      (unless (<= end start)
	(setq list (copy-sequence list))
	(setcdr (nthcdr (- end start 1) list) nil)
	list)
    (copy-sequence list)))

(defun widget-item-action (widget &optional event)
  ;; Just notify itself.
  (widget-apply widget :notify widget event))

;;; The `push-button' Widget.

;; (defcustom widget-push-button-gui t
;;   "If non nil, use GUI push buttons when available."
;;   :group 'widgets
;;   :type 'boolean)

;; Cache already created GUI objects.
;; (defvar widget-push-button-cache nil)

(defcustom widget-push-button-prefix "["
  "String used as prefix for buttons."
  :type 'string
  :group 'widget-button)

(defcustom widget-push-button-suffix "]"
  "String used as suffix for buttons."
  :type 'string
  :group 'widget-button)

(define-widget 'push-button 'item
  "A pushable button."
  :button-prefix ""
  :button-suffix ""
  :value-create 'widget-push-button-value-create
  :format "%[%v%]")

(defun widget-push-button-value-create (widget)
  "Insert text representing the `on' and `off' states."
  (let* ((tag (or (widget-get widget :tag)
		  (widget-get widget :value)))
	 (tag-glyph (widget-get widget :tag-glyph))
	 (text (concat widget-push-button-prefix
		       tag widget-push-button-suffix)))
    (if tag-glyph
	(widget-image-insert widget text tag-glyph)
      (insert text))))

;; (defun widget-gui-action (widget)
;;   "Apply :action for WIDGET."
;;   (widget-apply-action widget (this-command-keys)))

;;; The `link' Widget.

(defcustom widget-link-prefix "["
  "String used as prefix for links."
  :type 'string
  :group 'widget-button)

(defcustom widget-link-suffix "]"
  "String used as suffix for links."
  :type 'string
  :group 'widget-button)

(define-widget 'link 'item
  "An embedded link."
  :button-prefix 'widget-link-prefix
  :button-suffix 'widget-link-suffix
  :help-echo "Follow the link."
  :format "%[%t%]")

;;; The `info-link' Widget.

(define-widget 'info-link 'link
  "A link to an info file."
  :action 'widget-info-link-action)

(defun widget-info-link-action (widget &optional event)
  "Open the info node specified by WIDGET."
  (Info-goto-node (widget-value widget)))

;;; The `url-link' Widget.

(define-widget 'url-link 'link
  "A link to an www page."
  :action 'widget-url-link-action)

(defun widget-url-link-action (widget &optional event)
  "Open the url specified by WIDGET."
  (browse-url (widget-value widget)))

;;; The `function-link' Widget.

(define-widget 'function-link 'link
  "A link to an Emacs function."
  :action 'widget-function-link-action)

(defun widget-function-link-action (widget &optional event)
  "Show the function specified by WIDGET."
  (describe-function (widget-value widget)))

;;; The `variable-link' Widget.

(define-widget 'variable-link 'link
  "A link to an Emacs variable."
  :action 'widget-variable-link-action)

(defun widget-variable-link-action (widget &optional event)
  "Show the variable specified by WIDGET."
  (describe-variable (widget-value widget)))

;;; The `file-link' Widget.

(define-widget 'file-link 'link
  "A link to a file."
  :action 'widget-file-link-action)

(defun widget-file-link-action (widget &optional event)
  "Find the file specified by WIDGET."
  (find-file (widget-value widget)))

;;; The `emacs-library-link' Widget.

(define-widget 'emacs-library-link 'link
  "A link to an Emacs Lisp library file."
  :action 'widget-emacs-library-link-action)

(defun widget-emacs-library-link-action (widget &optional event)
  "Find the Emacs Library file specified by WIDGET."
  (find-file (locate-library (widget-value widget))))

;;; The `emacs-commentary-link' Widget.
    
(define-widget 'emacs-commentary-link 'link
  "A link to Commentary in an Emacs Lisp library file."
  :action 'widget-emacs-commentary-link-action)
    
(defun widget-emacs-commentary-link-action (widget &optional event)
  "Find the Commentary section of the Emacs file specified by WIDGET."
  (finder-commentary (widget-value widget)))

;;; The `editable-field' Widget.

(define-widget 'editable-field 'default
  "An editable text field."
  :convert-widget 'widget-value-convert-widget
  :keymap widget-field-keymap
  :format "%v"
  :help-echo "M-TAB: complete field; RET: enter value"
  :value ""
  :prompt-internal 'widget-field-prompt-internal
  :prompt-history 'widget-field-history
  :prompt-value 'widget-field-prompt-value
  :action 'widget-field-action
  :validate 'widget-field-validate
  :valid-regexp ""
  :error "Field's value doesn't match allowed forms"
  :value-create 'widget-field-value-create
  :value-delete 'widget-field-value-delete
  :value-get 'widget-field-value-get
  :match 'widget-field-match)

(defvar widget-field-history nil
  "History of field minibuffer edits.")

(defun widget-field-prompt-internal (widget prompt initial history)
  "Read string for WIDGET promptinhg with PROMPT.
INITIAL is the initial input and HISTORY is a symbol containing
the earlier input."
  (read-string prompt initial history))

(defun widget-field-prompt-value (widget prompt value unbound)
  "Prompt for a string."
  (widget-apply widget
		:value-to-external
		(widget-apply widget
			      :prompt-internal prompt
			      (unless unbound
				(cons (widget-apply widget
						    :value-to-internal value)
				      0))
			      (widget-get widget :prompt-history))))

(defvar widget-edit-functions nil)

(defun widget-field-action (widget &optional event)
  "Move to next field."
  (widget-forward 1)
  (run-hook-with-args 'widget-edit-functions widget))

(defun widget-field-validate (widget)
  "Valid if the content matches `:valid-regexp'."
  (unless (string-match (widget-get widget :valid-regexp)
			(widget-apply widget :value-get))
    widget))

(defun widget-field-value-create (widget)
  "Create an editable text field."
  (let ((size (widget-get widget :size))
	(value (widget-get widget :value))
	(from (point))
	;; This is changed to a real overlay in `widget-setup'.  We
	;; need the end points to behave differently until
	;; `widget-setup' is called.
	(overlay (cons (make-marker) (make-marker))))
    (widget-put widget :field-overlay overlay)
    (insert value)
    (and size
	 (< (length value) size)
	 (insert-char ?\  (- size (length value))))
    (unless (memq widget widget-field-list)
      (setq widget-field-new (cons widget widget-field-new)))
    (move-marker (cdr overlay) (point))
    (set-marker-insertion-type (cdr overlay) nil)
    (when (null size)
      (insert ?\n))
    (move-marker (car overlay) from)
    (set-marker-insertion-type (car overlay) t)))

(defun widget-field-value-delete (widget)
  "Remove the widget from the list of active editing fields."
  (setq widget-field-list (delq widget widget-field-list))
  (setq widget-field-new (delq widget widget-field-new))
  ;; These are nil if the :format string doesn't contain `%v'.
  (let ((overlay (widget-get widget :field-overlay)))
    (when (overlayp overlay)
      (delete-overlay overlay))))

(defun widget-field-value-get (widget)
  "Return current text in editing field."
  (let ((from (widget-field-start widget))
	(to (widget-field-end widget))
	(buffer (widget-field-buffer widget))
	(size (widget-get widget :size))
	(secret (widget-get widget :secret))
	(old (current-buffer)))
    (if (and from to)
	(progn
	  (set-buffer buffer)
	  (while (and size
		      (not (zerop size))
		      (> to from)
		      (eq (char-after (1- to)) ?\ ))
	    (setq to (1- to)))
	  (let ((result (buffer-substring-no-properties from to)))
	    (when secret
	      (let ((index 0))
		(while (< (+ from index) to)
		  (aset result index
			(get-char-property (+ from index) 'secret))
		  (setq index (1+ index)))))
	    (set-buffer old)
	    result))
      (widget-get widget :value))))

(defun widget-field-match (widget value)
  ;; Match any string.
  (stringp value))

;;; The `text' Widget.

(define-widget 'text 'editable-field
  "A multiline text area."
  :keymap widget-text-keymap)

;;; The `menu-choice' Widget.

(define-widget 'menu-choice 'default
  "A menu of options."
  :convert-widget  'widget-types-convert-widget
  :format "%[%t%]: %v"
  :case-fold t
  :tag "choice"
  :void '(item :format "invalid (%t)\n")
  :value-create 'widget-choice-value-create
  :value-delete 'widget-children-value-delete
  :value-get 'widget-choice-value-get
  :value-inline 'widget-choice-value-inline
  :default-get 'widget-choice-default-get
  :mouse-down-action 'widget-choice-mouse-down-action
  :action 'widget-choice-action
  :error "Make a choice"
  :validate 'widget-choice-validate
  :match 'widget-choice-match
  :match-inline 'widget-choice-match-inline)

(defun widget-choice-value-create (widget)
  "Insert the first choice that matches the value."
  (let ((value (widget-get widget :value))
	(args (widget-get widget :args))
	(explicit (widget-get widget :explicit-choice))
	current)
    (if (and explicit (equal value (widget-get widget :explicit-choice-value)))
	(progn
	  ;; If the user specified the choice for this value,
	  ;; respect that choice as long as the value is the same.
	  (widget-put widget :children (list (widget-create-child-value
					      widget explicit value)))
	  (widget-put widget :choice explicit))
      (while args
	(setq current (car args)
	      args (cdr args))
	(when (widget-apply current :match value)
	  (widget-put widget :children (list (widget-create-child-value
					      widget current value)))
	  (widget-put widget :choice current)
	  (setq args nil
		current nil)))
      (when current
	(let ((void (widget-get widget :void)))
	  (widget-put widget :children (list (widget-create-child-and-convert
					      widget void :value value)))
	  (widget-put widget :choice void))))))

(defun widget-choice-value-get (widget)
  ;; Get value of the child widget.
  (widget-value (car (widget-get widget :children))))

(defun widget-choice-value-inline (widget)
  ;; Get value of the child widget.
  (widget-apply (car (widget-get widget :children)) :value-inline))

(defun widget-choice-default-get (widget)
  ;; Get default for the first choice.
  (widget-default-get (car (widget-get widget :args))))

(defcustom widget-choice-toggle nil
  "If non-nil, a binary choice will just toggle between the values.
Otherwise, the user will explicitly have to choose between the values
when he invoked the menu."
  :type 'boolean
  :group 'widgets)

(defun widget-choice-mouse-down-action (widget &optional event)
  ;; Return non-nil if we need a menu.
  (let ((args (widget-get widget :args))
	(old (widget-get widget :choice)))
    (cond ((not (display-popup-menus-p))
	   ;; No place to pop up a menu.
	   nil)
	  ((< (length args) 2)
	   ;; Empty or singleton list, just return the value.
	   nil)
	  ((> (length args) widget-menu-max-size)
	   ;; Too long, prompt.
	   nil)
	  ((> (length args) 2)
	   ;; Reasonable sized list, use menu.
	   t)
	  ((and widget-choice-toggle (memq old args))
	   ;; We toggle.
	   nil)
	  (t
	   ;; Ask which of the two.
	   t))))

(defun widget-choice-action (widget &optional event)
  ;; Make a choice.
  (let ((args (widget-get widget :args))
	(old (widget-get widget :choice))
	(tag (widget-apply widget :menu-tag-get))
	(completion-ignore-case (widget-get widget :case-fold))
	this-explicit
	current choices)
    ;; Remember old value.
    (if (and old (not (widget-apply widget :validate)))
	(let* ((external (widget-value widget))
	       (internal (widget-apply old :value-to-internal external)))
	  (widget-put old :value internal)))
    ;; Find new choice.
    (setq current
	  (cond ((= (length args) 0)
		 nil)
		((= (length args) 1)
		 (nth 0 args))
		((and widget-choice-toggle
		      (= (length args) 2)
		      (memq old args))
		 (if (eq old (nth 0 args))
		     (nth 1 args)
		   (nth 0 args)))
		(t
		 (while args
		   (setq current (car args)
			 args (cdr args))
		   (setq choices
			 (cons (cons (widget-apply current :menu-tag-get)
				     current)
			       choices)))
		 (setq this-explicit t)
		 (widget-choose tag (reverse choices) event))))
    (when current
      ;; If this was an explicit user choice,
      ;; record the choice, and the record the value it was made for.
      ;; widget-choice-value-create will respect this choice,
      ;; as long as the value is the same.
      (when this-explicit
	(widget-put widget :explicit-choice current)
	(widget-put widget :explicit-choice-value (widget-get widget :value)))
      (widget-value-set
       widget (widget-apply current
			    :value-to-external (widget-default-get current)))
      (widget-setup)
      (widget-apply widget :notify widget event)))
  (run-hook-with-args 'widget-edit-functions widget))

(defun widget-choice-validate (widget)
  ;; Valid if we have made a valid choice.
  (if (eq (widget-get widget :void) (widget-get widget :choice))
      widget
    (widget-apply (car (widget-get widget :children)) :validate)))

(defun widget-choice-match (widget value)
  ;; Matches if one of the choices matches.
  (let ((args (widget-get widget :args))
	current found)
    (while (and args (not found))
      (setq current (car args)
	    args (cdr args)
	    found (widget-apply current :match value)))
    found))

(defun widget-choice-match-inline (widget values)
  ;; Matches if one of the choices matches.
  (let ((args (widget-get widget :args))
	current found)
    (while (and args (null found))
      (setq current (car args)
	    args (cdr args)
	    found (widget-match-inline current values)))
    found))

;;; The `toggle' Widget.

(define-widget 'toggle 'item
  "Toggle between two states."
  :format "%[%v%]\n"
  :value-create 'widget-toggle-value-create
  :action 'widget-toggle-action
  :match (lambda (widget value) t)
  :on "on"
  :off "off")

(defun widget-toggle-value-create (widget)
  "Insert text representing the `on' and `off' states."
  (if (widget-value widget)
      (let ((image (widget-get widget :on-glyph)))
	(and (display-graphic-p)
	     (listp image)
	     (not (eq (car image) 'image))
	     (widget-put widget :on-glyph (setq image (eval image))))
	(widget-image-insert widget
			     (widget-get widget :on)
			     image))
    (let ((image (widget-get widget :off-glyph)))
      (and (display-graphic-p)
	   (listp image)
	   (not (eq (car image) 'image))
	   (widget-put widget :off-glyph (setq image (eval image))))
      (widget-image-insert widget (widget-get widget :off) image))))

(defun widget-toggle-action (widget &optional event)
  ;; Toggle value.
  (widget-value-set widget (not (widget-value widget)))
  (widget-apply widget :notify widget event)
  (run-hook-with-args 'widget-edit-functions widget))

;;; The `checkbox' Widget.

(define-widget 'checkbox 'toggle
  "A checkbox toggle."
  :button-suffix ""
  :button-prefix ""
  :format "%[%v%]"
  :on "[X]"
  ;; We could probably do the same job as the images using single
  ;; space characters in a boxed face with a stretch specification to
  ;; make them square.
  :on-glyph '(create-image "\000\066\076\034\076\066\000"
			   'xbm t :width 7 :height 7
			   :background "grey75"	; like default mode line
			   :foreground "black"
			   :relief -3
			   :ascent 'center)
  :off "[ ]"
  :off-glyph '(create-image (make-string 7 0)
			    'xbm t :width 7 :height 7
			    :background "grey75"
			    :foreground "black"
			    :relief 3
			    :ascent 'center)
  :help-echo "Toggle this item."
  :action 'widget-checkbox-action)

(defun widget-checkbox-action (widget &optional event)
  "Toggle checkbox, notify parent, and set active state of sibling."
  (widget-toggle-action widget event)
  (let ((sibling (widget-get-sibling widget)))
    (when sibling
      (if (widget-value widget)
	  (widget-apply sibling :activate)
	(widget-apply sibling :deactivate)))))

;;; The `checklist' Widget.

(define-widget 'checklist 'default
  "A multiple choice widget."
  :convert-widget 'widget-types-convert-widget
  :format "%v"
  :offset 4
  :entry-format "%b %v"
  :greedy nil
  :value-create 'widget-checklist-value-create
  :value-delete 'widget-children-value-delete
  :value-get 'widget-checklist-value-get
  :validate 'widget-checklist-validate
  :match 'widget-checklist-match
  :match-inline 'widget-checklist-match-inline)

(defun widget-checklist-value-create (widget)
  ;; Insert all values
  (let ((alist (widget-checklist-match-find widget (widget-get widget :value)))
	(args (widget-get widget :args)))
    (while args
      (widget-checklist-add-item widget (car args) (assq (car args) alist))
      (setq args (cdr args)))
    (widget-put widget :children (nreverse (widget-get widget :children)))))

(defun widget-checklist-add-item (widget type chosen)
  "Create checklist item in WIDGET of type TYPE.
If the item is checked, CHOSEN is a cons whose cdr is the value."
  (and (eq (preceding-char) ?\n)
       (widget-get widget :indent)
       (insert-char ?  (widget-get widget :indent)))
  (widget-specify-insert
   (let* ((children (widget-get widget :children))
	  (buttons (widget-get widget :buttons))
	  (button-args (or (widget-get type :sibling-args)
			   (widget-get widget :button-args)))
	  (from (point))
	  child button)
     (insert (widget-get widget :entry-format))
     (goto-char from)
     ;; Parse % escapes in format.
     (while (re-search-forward "%\\([bv%]\\)" nil t)
       (let ((escape (char-after (match-beginning 1))))
	 (delete-backward-char 2)
	 (cond ((eq escape ?%)
		(insert ?%))
	       ((eq escape ?b)
		(setq button (apply 'widget-create-child-and-convert
				    widget 'checkbox
				    :value (not (null chosen))
				    button-args)))
	       ((eq escape ?v)
		(setq child
		      (cond ((not chosen)
			     (let ((child (widget-create-child widget type)))
			       (widget-apply child :deactivate)
			       child))
			    ((widget-get type :inline)
			     (widget-create-child-value
			      widget type (cdr chosen)))
			    (t
			     (widget-create-child-value
			      widget type (car (cdr chosen)))))))
	       (t
		(error "Unknown escape `%c'" escape)))))
     ;; Update properties.
     (and button child (widget-put child :button button))
     (and button (widget-put widget :buttons (cons button buttons)))
     (and child (widget-put widget :children (cons child children))))))

(defun widget-checklist-match (widget values)
  ;; All values must match a type in the checklist.
  (and (listp values)
       (null (cdr (widget-checklist-match-inline widget values)))))

(defun widget-checklist-match-inline (widget values)
  ;; Find the values which match a type in the checklist.
  (let ((greedy (widget-get widget :greedy))
	(args (copy-sequence (widget-get widget :args)))
	found rest)
    (while values
      (let ((answer (widget-checklist-match-up args values)))
	(cond (answer
	       (let ((vals (widget-match-inline answer values)))
		 (setq found (append found (car vals))
		       values (cdr vals)
		       args (delq answer args))))
	      (greedy
	       (setq rest (append rest (list (car values)))
		     values (cdr values)))
	      (t
	       (setq rest (append rest values)
		     values nil)))))
    (cons found rest)))

(defun widget-checklist-match-find (widget vals)
  "Find the vals which match a type in the checklist.
Return an alist of (TYPE MATCH)."
  (let ((greedy (widget-get widget :greedy))
	(args (copy-sequence (widget-get widget :args)))
	found)
    (while vals
      (let ((answer (widget-checklist-match-up args vals)))
	(cond (answer
	       (let ((match (widget-match-inline answer vals)))
		 (setq found (cons (cons answer (car match)) found)
		       vals (cdr match)
		       args (delq answer args))))
	      (greedy
	       (setq vals (cdr vals)))
	      (t
	       (setq vals nil)))))
    found))

(defun widget-checklist-match-up (args vals)
  "Return the first type from ARGS that matches VALS."
  (let (current found)
    (while (and args (null found))
      (setq current (car args)
	    args (cdr args)
	    found (widget-match-inline current vals)))
    (if found
	current)))

(defun widget-checklist-value-get (widget)
  ;; The values of all selected items.
  (let ((children (widget-get widget :children))
	child result)
    (while children
      (setq child (car children)
	    children (cdr children))
      (if (widget-value (widget-get child :button))
	  (setq result (append result (widget-apply child :value-inline)))))
    result))

(defun widget-checklist-validate (widget)
  ;; Ticked chilren must be valid.
  (let ((children (widget-get widget :children))
	child button found)
    (while (and children (not found))
      (setq child (car children)
	    children (cdr children)
	    button (widget-get child :button)
	    found (and (widget-value button)
		       (widget-apply child :validate))))
    found))

;;; The `option' Widget

(define-widget 'option 'checklist
  "An widget with an optional item."
  :inline t)

;;; The `choice-item' Widget.

(define-widget 'choice-item 'item
  "Button items that delegate action events to their parents."
  :action 'widget-parent-action
  :format "%[%t%] \n")

;;; The `radio-button' Widget.

(define-widget 'radio-button 'toggle
  "A radio button for use in the `radio' widget."
  :notify 'widget-radio-button-notify
  :format "%[%v%]"
  :button-suffix ""
  :button-prefix ""
  :on "(*)"
  :on-glyph "radio1"
  :off "( )"
  :off-glyph "radio0")

(defun widget-radio-button-notify (widget child &optional event)
  ;; Tell daddy.
  (widget-apply (widget-get widget :parent) :action widget event))

;;; The `radio-button-choice' Widget.

(define-widget 'radio-button-choice 'default
  "Select one of multiple options."
  :convert-widget 'widget-types-convert-widget
  :offset 4
  :format "%v"
  :entry-format "%b %v"
  :value-create 'widget-radio-value-create
  :value-delete 'widget-children-value-delete
  :value-get 'widget-radio-value-get
  :value-inline 'widget-radio-value-inline
  :value-set 'widget-radio-value-set
  :error "You must push one of the buttons"
  :validate 'widget-radio-validate
  :match 'widget-choice-match
  :match-inline 'widget-choice-match-inline
  :action 'widget-radio-action)

(defun widget-radio-value-create (widget)
  ;; Insert all values
  (let ((args (widget-get widget :args))
	arg)
    (while args
      (setq arg (car args)
	    args (cdr args))
      (widget-radio-add-item widget arg))))

(defun widget-radio-add-item (widget type)
  "Add to radio widget WIDGET a new radio button item of type TYPE."
  ;; (setq type (widget-convert type))
  (and (eq (preceding-char) ?\n)
       (widget-get widget :indent)
       (insert-char ?  (widget-get widget :indent)))
  (widget-specify-insert
   (let* ((value (widget-get widget :value))
	  (children (widget-get widget :children))
	  (buttons (widget-get widget :buttons))
	  (button-args (or (widget-get type :sibling-args)
			   (widget-get widget :button-args)))
	  (from (point))
	  (chosen (and (null (widget-get widget :choice))
		       (widget-apply type :match value)))
	  child button)
     (insert (widget-get widget :entry-format))
     (goto-char from)
     ;; Parse % escapes in format.
     (while (re-search-forward "%\\([bv%]\\)" nil t)
       (let ((escape (char-after (match-beginning 1))))
	 (delete-backward-char 2)
	 (cond ((eq escape ?%)
		(insert ?%))
	       ((eq escape ?b)
		(setq button (apply 'widget-create-child-and-convert
				    widget 'radio-button
				    :value (not (null chosen))
				    button-args)))
	       ((eq escape ?v)
		(setq child (if chosen
				(widget-create-child-value
				 widget type value)
			      (widget-create-child widget type)))
		(unless chosen
		  (widget-apply child :deactivate)))
	       (t
		(error "Unknown escape `%c'" escape)))))
     ;; Update properties.
     (when chosen
       (widget-put widget :choice type))
     (when button
       (widget-put child :button button)
       (widget-put widget :buttons (nconc buttons (list button))))
     (when child
       (widget-put widget :children (nconc children (list child))))
     child)))

(defun widget-radio-value-get (widget)
  ;; Get value of the child widget.
  (let ((chosen (widget-radio-chosen widget)))
    (and chosen (widget-value chosen))))

(defun widget-radio-chosen (widget)
  "Return the widget representing the chosen radio button."
  (let ((children (widget-get widget :children))
	current found)
    (while children
      (setq current (car children)
	    children (cdr children))
      (when (widget-apply (widget-get current :button) :value-get)
	(setq found current
	      children nil)))
    found))

(defun widget-radio-value-inline (widget)
  ;; Get value of the child widget.
  (let ((children (widget-get widget :children))
	current found)
    (while children
      (setq current (car children)
	    children (cdr children))
      (when (widget-apply (widget-get current :button) :value-get)
	(setq found (widget-apply current :value-inline)
	      children nil)))
    found))

(defun widget-radio-value-set (widget value)
  ;; We can't just delete and recreate a radio widget, since children
  ;; can be added after the original creation and won't be recreated
  ;; by `:create'.
  (let ((children (widget-get widget :children))
	current found)
    (while children
      (setq current (car children)
	    children (cdr children))
      (let* ((button (widget-get current :button))
	     (match (and (not found)
			 (widget-apply current :match value))))
	(widget-value-set button match)
	(if match
	    (progn
	      (widget-value-set current value)
	      (widget-apply current :activate))
	  (widget-apply current :deactivate))
	(setq found (or found match))))))

(defun widget-radio-validate (widget)
  ;; Valid if we have made a valid choice.
  (let ((children (widget-get widget :children))
	current found button)
    (while (and children (not found))
      (setq current (car children)
	    children (cdr children)
	    button (widget-get current :button)
	    found (widget-apply button :value-get)))
    (if found
	(widget-apply current :validate)
      widget)))

(defun widget-radio-action (widget child event)
  ;; Check if a radio button was pressed.
  (let ((children (widget-get widget :children))
	(buttons (widget-get widget :buttons))
	current)
    (when (memq child buttons)
      (while children
	(setq current (car children)
	      children (cdr children))
	(let* ((button (widget-get current :button)))
	  (cond ((eq child button)
		 (widget-value-set button t)
		 (widget-apply current :activate))
		((widget-value button)
		 (widget-value-set button nil)
		 (widget-apply current :deactivate)))))))
  ;; Pass notification to parent.
  (widget-apply widget :notify child event))

;;; The `insert-button' Widget.

(define-widget 'insert-button 'push-button
  "An insert button for the `editable-list' widget."
  :tag "INS"
  :help-echo "Insert a new item into the list at this position."
  :action 'widget-insert-button-action)

(defun widget-insert-button-action (widget &optional event)
  ;; Ask the parent to insert a new item.
  (widget-apply (widget-get widget :parent)
		:insert-before (widget-get widget :widget)))

;;; The `delete-button' Widget.

(define-widget 'delete-button 'push-button
  "A delete button for the `editable-list' widget."
  :tag "DEL"
  :help-echo "Delete this item from the list."
  :action 'widget-delete-button-action)

(defun widget-delete-button-action (widget &optional event)
  ;; Ask the parent to insert a new item.
  (widget-apply (widget-get widget :parent)
		:delete-at (widget-get widget :widget)))

;;; The `editable-list' Widget.

;; (defcustom widget-editable-list-gui nil
;;   "If non nil, use GUI push-buttons in editable list when available."
;;   :type 'boolean
;;   :group 'widgets)

(define-widget 'editable-list 'default
  "A variable list of widgets of the same type."
  :convert-widget 'widget-types-convert-widget
  :offset 12
  :format "%v%i\n"
  :format-handler 'widget-editable-list-format-handler
  :entry-format "%i %d %v"
  :value-create 'widget-editable-list-value-create
  :value-delete 'widget-children-value-delete
  :value-get 'widget-editable-list-value-get
  :validate 'widget-children-validate
  :match 'widget-editable-list-match
  :match-inline 'widget-editable-list-match-inline
  :insert-before 'widget-editable-list-insert-before
  :delete-at 'widget-editable-list-delete-at)

(defun widget-editable-list-format-handler (widget escape)
  ;; We recognize the insert button.
;;;   (let ((widget-push-button-gui widget-editable-list-gui))
    (cond ((eq escape ?i)
	   (and (widget-get widget :indent)
		(insert-char ?\  (widget-get widget :indent)))
	   (apply 'widget-create-child-and-convert
		  widget 'insert-button
		  (widget-get widget :append-button-args)))
	  (t
	   (widget-default-format-handler widget escape)))
;;;     )
  )

(defun widget-editable-list-value-create (widget)
  ;; Insert all values
  (let* ((value (widget-get widget :value))
	 (type (nth 0 (widget-get widget :args)))
	 children)
    (widget-put widget :value-pos (copy-marker (point)))
    (set-marker-insertion-type (widget-get widget :value-pos) t)
    (while value
      (let ((answer (widget-match-inline type value)))
	(if answer
	    (setq children (cons (widget-editable-list-entry-create
				  widget
				  (if (widget-get type :inline)
				      (car answer)
				    (car (car answer)))
				  t)
				 children)
		  value (cdr answer))
	  (setq value nil))))
    (widget-put widget :children (nreverse children))))

(defun widget-editable-list-value-get (widget)
  ;; Get value of the child widget.
  (apply 'append (mapcar (lambda (child) (widget-apply child :value-inline))
			 (widget-get widget :children))))

(defun widget-editable-list-match (widget value)
  ;; Value must be a list and all the members must match the type.
  (and (listp value)
       (null (cdr (widget-editable-list-match-inline widget value)))))

(defun widget-editable-list-match-inline (widget value)
  (let ((type (nth 0 (widget-get widget :args)))
	(ok t)
	found)
    (while (and value ok)
      (let ((answer (widget-match-inline type value)))
	(if answer
	    (setq found (append found (car answer))
		  value (cdr answer))
	  (setq ok nil))))
    (cons found value)))

(defun widget-editable-list-insert-before (widget before)
  ;; Insert a new child in the list of children.
  (save-excursion
    (let ((children (widget-get widget :children))
	  (inhibit-read-only t)
	  before-change-functions
	  after-change-functions)
      (cond (before
	     (goto-char (widget-get before :entry-from)))
	    (t
	     (goto-char (widget-get widget :value-pos))))
      (let ((child (widget-editable-list-entry-create
		    widget nil nil)))
	(when (< (widget-get child :entry-from) (widget-get widget :from))
	  (set-marker (widget-get widget :from)
		      (widget-get child :entry-from)))
	(if (eq (car children) before)
	    (widget-put widget :children (cons child children))
	  (while (not (eq (car (cdr children)) before))
	    (setq children (cdr children)))
	  (setcdr children (cons child (cdr children)))))))
  (widget-setup)
  (widget-apply widget :notify widget))

(defun widget-editable-list-delete-at (widget child)
  ;; Delete child from list of children.
  (save-excursion
    (let ((buttons (copy-sequence (widget-get widget :buttons)))
	  button
	  (inhibit-read-only t)
	  before-change-functions
	  after-change-functions)
      (while buttons
	(setq button (car buttons)
	      buttons (cdr buttons))
	(when (eq (widget-get button :widget) child)
	  (widget-put widget
		      :buttons (delq button (widget-get widget :buttons)))
	  (widget-delete button))))
    (let ((entry-from (widget-get child :entry-from))
	  (entry-to (widget-get child :entry-to))
	  (inhibit-read-only t)
	  before-change-functions
	  after-change-functions)
      (widget-delete child)
      (delete-region entry-from entry-to)
      (set-marker entry-from nil)
      (set-marker entry-to nil))
    (widget-put widget :children (delq child (widget-get widget :children))))
  (widget-setup)
  (widget-apply widget :notify widget))

(defun widget-editable-list-entry-create (widget value conv)
  ;; Create a new entry to the list.
  (let ((type (nth 0 (widget-get widget :args)))
;;; 	(widget-push-button-gui widget-editable-list-gui)
	child delete insert)
    (widget-specify-insert
     (save-excursion
       (and (widget-get widget :indent)
	    (insert-char ?\  (widget-get widget :indent)))
       (insert (widget-get widget :entry-format)))
     ;; Parse % escapes in format.
     (while (re-search-forward "%\\(.\\)" nil t)
       (let ((escape (char-after (match-beginning 1))))
	 (delete-backward-char 2)
	 (cond ((eq escape ?%)
		(insert ?%))
	       ((eq escape ?i)
		(setq insert (apply 'widget-create-child-and-convert
				    widget 'insert-button
				    (widget-get widget :insert-button-args))))
	       ((eq escape ?d)
		(setq delete (apply 'widget-create-child-and-convert
				    widget 'delete-button
				    (widget-get widget :delete-button-args))))
	       ((eq escape ?v)
		(if conv
		    (setq child (widget-create-child-value
				 widget type value))
		  (setq child (widget-create-child-value
			       widget type
			       (widget-apply type :value-to-external
					     (widget-default-get type))))))
	       (t
		(error "Unknown escape `%c'" escape)))))
     (widget-put widget
		 :buttons (cons delete
				(cons insert
				      (widget-get widget :buttons))))
     (let ((entry-from (point-min-marker))
	   (entry-to (point-max-marker)))
       (set-marker-insertion-type entry-from t)
       (set-marker-insertion-type entry-to nil)
       (widget-put child :entry-from entry-from)
       (widget-put child :entry-to entry-to)))
    (widget-put insert :widget child)
    (widget-put delete :widget child)
    child))

;;; The `group' Widget.

(define-widget 'group 'default
  "A widget which groups other widgets inside."
  :convert-widget 'widget-types-convert-widget
  :format "%v"
  :value-create 'widget-group-value-create
  :value-delete 'widget-children-value-delete
  :value-get 'widget-editable-list-value-get
  :default-get 'widget-group-default-get
  :validate 'widget-children-validate
  :match 'widget-group-match
  :match-inline 'widget-group-match-inline)

(defun widget-group-value-create (widget)
  ;; Create each component.
  (let ((args (widget-get widget :args))
	(value (widget-get widget :value))
	arg answer children)
    (while args
      (setq arg (car args)
	    args (cdr args)
	    answer (widget-match-inline arg value)
	    value (cdr answer))
      (and (eq (preceding-char) ?\n)
	   (widget-get widget :indent)
	   (insert-char ?\  (widget-get widget :indent)))
      (push (cond ((null answer)
		   (widget-create-child widget arg))
		  ((widget-get arg :inline)
		   (widget-create-child-value widget arg (car answer)))
		  (t
		   (widget-create-child-value widget arg (car (car answer)))))
	    children))
    (widget-put widget :children (nreverse children))))

(defun widget-group-default-get (widget)
  ;; Get the default of the components.
  (mapcar 'widget-default-get (widget-get widget :args)))

(defun widget-group-match (widget values)
  ;; Match if the components match.
  (and (listp values)
       (let ((match (widget-group-match-inline widget values)))
	 (and match (null (cdr match))))))

(defun widget-group-match-inline (widget vals)
  ;; Match if the components match.
  (let ((args (widget-get widget :args))
	argument answer found)
    (while args
      (setq argument (car args)
	    args (cdr args)
	    answer (widget-match-inline argument vals))
      (if answer
	  (setq vals (cdr answer)
		found (append found (car answer)))
	(setq vals nil
	      args nil)))
    (if answer
	(cons found vals))))

;;; The `visibility' Widget.

(define-widget 'visibility 'item
  "An indicator and manipulator for hidden items."
  :format "%[%v%]"
  :button-prefix ""
  :button-suffix ""
  :on "Hide"
  :off "Show"
  :value-create 'widget-visibility-value-create
  :action 'widget-toggle-action
  :match (lambda (widget value) t))

(defun widget-visibility-value-create (widget)
  ;; Insert text representing the `on' and `off' states.
  (let ((on (widget-get widget :on))
	(off (widget-get widget :off)))
    (if on
	(setq on (concat widget-push-button-prefix
			 on
			 widget-push-button-suffix))
      (setq on ""))
    (if off
	(setq off (concat widget-push-button-prefix
			  off
			  widget-push-button-suffix))
      (setq off ""))
    (if (widget-value widget)
	(widget-image-insert widget on "down" "down-pushed")
      (widget-image-insert widget off "right" "right-pushed"))))

;;; The `documentation-link' Widget.
;;
;; This is a helper widget for `documentation-string'.

(define-widget 'documentation-link 'link
  "Link type used in documentation strings."
  :tab-order -1
  :help-echo "Describe this symbol"
  :action 'widget-documentation-link-action)

(defun widget-documentation-link-action (widget &optional event)
  "Display documentation for WIDGET's value.  Ignore optional argument EVENT."
  (let* ((string (widget-get widget :value))
	 (symbol (intern string)))
    (if (and (fboundp symbol) (boundp symbol))
	;; If there are two doc strings, give the user a way to pick one.
	(apropos (concat "\\`" (regexp-quote string) "\\'"))
      (if (fboundp symbol)
	  (describe-function symbol)
	(describe-variable symbol)))))

(defcustom widget-documentation-links t
  "Add hyperlinks to documentation strings when non-nil."
  :type 'boolean
  :group 'widget-documentation)

(defcustom widget-documentation-link-regexp "`\\([^\n`' ]+\\)'"
  "Regexp for matching potential links in documentation strings.
The first group should be the link itself."
  :type 'regexp
  :group 'widget-documentation)

(defcustom widget-documentation-link-p 'intern-soft
  "Predicate used to test if a string is useful as a link.
The value should be a function.  The function will be called one
argument, a string, and should return non-nil if there should be a
link for that string."
  :type 'function
  :options '(widget-documentation-link-p)
  :group 'widget-documentation)

(defcustom widget-documentation-link-type 'documentation-link
  "Widget type used for links in documentation strings."
  :type 'symbol
  :group 'widget-documentation)

(defun widget-documentation-link-add (widget from to)
  (widget-specify-doc widget from to)
  (when widget-documentation-links
    (let ((regexp widget-documentation-link-regexp)
	  (buttons (widget-get widget :buttons))
	  (widget-mouse-face (default-value 'widget-mouse-face))
	  (widget-button-face widget-documentation-face)
	  (widget-button-pressed-face widget-documentation-face))
      (save-excursion
	(goto-char from)
	(while (re-search-forward regexp to t)
	  (let ((name (match-string 1))
		(begin (match-beginning 1))
		(end (match-end 1)))
	    (when (funcall widget-documentation-link-p name)
	      (push (widget-convert-button widget-documentation-link-type
					   begin end :value name)
		    buttons)))))
      (widget-put widget :buttons buttons)))
  (let ((indent (widget-get widget :indent)))
    (when (and indent (not (zerop indent)))
      (save-excursion
	(save-restriction
	  (narrow-to-region from to)
	  (goto-char (point-min))
	  (while (search-forward "\n" nil t)
	    (insert-char ?\  indent)))))))

;;; The `documentation-string' Widget.

(define-widget 'documentation-string 'item
  "A documentation string."
  :format "%v"
  :action 'widget-documentation-string-action
  :value-delete 'widget-children-value-delete
  :value-create 'widget-documentation-string-value-create)

(defun widget-documentation-string-value-create (widget)
  ;; Insert documentation string.
  (let ((doc (widget-value widget))
	(indent (widget-get widget :indent))
	(shown (widget-get (widget-get widget :parent) :documentation-shown))
	(start (point)))
    (if (string-match "\n" doc)
	(let ((before (substring doc 0 (match-beginning 0)))
	      (after (substring doc (match-beginning 0)))
	      button)
	  (insert before ?\ )
	  (widget-documentation-link-add widget start (point))
	  (setq button
		(widget-create-child-and-convert
		 widget 'visibility
		 :help-echo "Show or hide rest of the documentation."
		 :off "More"
		 :always-active t
		 :action 'widget-parent-action
		 shown))
	  (when shown
	    (setq start (point))
	    (when (and indent (not (zerop indent)))
	      (insert-char ?\  indent))
	    (insert after)
	    (widget-documentation-link-add widget start (point)))
	  (widget-put widget :buttons (list button)))
      (insert doc)
      (widget-documentation-link-add widget start (point))))
  (insert ?\n))

(defun widget-documentation-string-action (widget &rest ignore)
  ;; Toggle documentation.
  (let ((parent (widget-get widget :parent)))
    (widget-put parent :documentation-shown
		(not (widget-get parent :documentation-shown))))
  ;; Redraw.
  (widget-value-set widget (widget-value widget)))

;;; The Sexp Widgets.

(define-widget 'const 'item
  "An immutable sexp."
  :prompt-value 'widget-const-prompt-value
  :format "%t\n%d")

(defun widget-const-prompt-value (widget prompt value unbound)
  ;; Return the value of the const.
  (widget-value widget))

(define-widget 'function-item 'const
  "An immutable function name."
  :format "%v\n%h"
  :documentation-property (lambda (symbol)
			    (condition-case nil
				(documentation symbol t)
			      (error nil))))

(define-widget 'variable-item 'const
  "An immutable variable name."
  :format "%v\n%h"
  :documentation-property 'variable-documentation)

(define-widget 'other 'sexp
  "Matches any value, but doesn't let the user edit the value.
This is useful as last item in a `choice' widget.
You should use this widget type with a default value,
as in (other DEFAULT) or (other :tag \"NAME\" DEFAULT).
If the user selects this alternative, that specifies DEFAULT
as the value."
  :tag "Other"
  :format "%t%n"
  :value 'other)

(defvar widget-string-prompt-value-history nil
  "History of input to `widget-string-prompt-value'.")

(define-widget 'string 'editable-field
  "A string"
  :tag "String"
  :format "%{%t%}: %v"
  :complete-function 'ispell-complete-word
  :prompt-history 'widget-string-prompt-value-history)

(define-widget 'regexp 'string
  "A regular expression."
  :match 'widget-regexp-match
  :validate 'widget-regexp-validate
  ;; Doesn't work well with terminating newline.
  ;; :value-face 'widget-single-line-field-face
  :tag "Regexp")

(defun widget-regexp-match (widget value)
  ;; Match valid regexps.
  (and (stringp value)
       (condition-case nil
	   (prog1 t
	     (string-match value ""))
	 (error nil))))

(defun widget-regexp-validate (widget)
  "Check that the value of WIDGET is a valid regexp."
  (condition-case data
      (prog1 nil
	(string-match (widget-value widget) ""))
    (error (widget-put widget :error (error-message-string data))
	   widget)))

(define-widget 'file 'string
  "A file widget.
It will read a file name from the minibuffer when invoked."
  :complete-function 'widget-file-complete
  :prompt-value 'widget-file-prompt-value
  :format "%{%t%}: %v"
  ;; Doesn't work well with terminating newline.
  ;; :value-face 'widget-single-line-field-face
  :tag "File")

(defun widget-file-complete ()
  "Perform completion on file name preceding point."
  (interactive)
  (let* ((end (point))
	 (beg (save-excursion
		(skip-chars-backward "^ ")
		(point)))
	 (pattern (buffer-substring beg end))
	 (name-part (file-name-nondirectory pattern))
	 (directory (file-name-directory pattern))
	 (completion (file-name-completion name-part directory)))
    (cond ((eq completion t))
	  ((null completion)
	   (message "Can't find completion for \"%s\"" pattern)
	   (ding))
	  ((not (string= name-part completion))
	   (delete-region beg end)
	   (insert (expand-file-name completion directory)))
	  (t
	   (message "Making completion list...")
	   (with-output-to-temp-buffer "*Completions*"
	     (display-completion-list
	      (sort (file-name-all-completions name-part directory)
		    'string<)))
	   (message "Making completion list...%s" "done")))))

(defun widget-file-prompt-value (widget prompt value unbound)
  ;; Read file from minibuffer.
  (abbreviate-file-name
   (if unbound
       (read-file-name prompt)
     (let ((prompt2 (format "%s (default %s) " prompt value))
	   (dir (file-name-directory value))
	   (file (file-name-nondirectory value))
	   (must-match (widget-get widget :must-match)))
       (read-file-name prompt2 dir nil must-match file)))))

;;;(defun widget-file-action (widget &optional event)
;;;  ;; Read a file name from the minibuffer.
;;;  (let* ((value (widget-value widget))
;;;	 (dir (file-name-directory value))
;;;	 (file (file-name-nondirectory value))
;;;	 (menu-tag (widget-apply widget :menu-tag-get))
;;;	 (must-match (widget-get widget :must-match))
;;;	 (answer (read-file-name (concat menu-tag ": (default `" value "') ")
;;;				 dir nil must-match file)))
;;;    (widget-value-set widget (abbreviate-file-name answer))
;;;    (widget-setup)
;;;    (widget-apply widget :notify widget event)))

;; Fixme: use file-name-as-directory.
(define-widget 'directory 'file
  "A directory widget.
It will read a directory name from the minibuffer when invoked."
  :tag "Directory")

(defvar widget-symbol-prompt-value-history nil
  "History of input to `widget-symbol-prompt-value'.")

(define-widget 'symbol 'editable-field
  "A Lisp symbol."
  :value nil
  :tag "Symbol"
  :format "%{%t%}: %v"
  :match (lambda (widget value) (symbolp value))
  :complete-function 'lisp-complete-symbol
  :prompt-internal 'widget-symbol-prompt-internal
  :prompt-match 'symbolp
  :prompt-history 'widget-symbol-prompt-value-history
  :value-to-internal (lambda (widget value)
		       (if (symbolp value)
			   (symbol-name value)
			 value))
  :value-to-external (lambda (widget value)
		       (if (stringp value)
			   (intern value)
			 value)))

(defun widget-symbol-prompt-internal (widget prompt initial history)
  ;; Read file from minibuffer.
  (let ((answer (completing-read prompt obarray
				 (widget-get widget :prompt-match)
				 nil initial history)))
    (if (and (stringp answer)
	     (not (zerop (length answer))))
	answer
      (error "No value"))))

(defvar widget-function-prompt-value-history nil
  "History of input to `widget-function-prompt-value'.")

(define-widget 'function 'sexp
  "A Lisp function."
  :complete-function (lambda ()
		       (interactive)
		       (lisp-complete-symbol 'fboundp))
  :prompt-value 'widget-field-prompt-value
  :prompt-internal 'widget-symbol-prompt-internal
  :prompt-match 'fboundp
  :prompt-history 'widget-function-prompt-value-history
  :action 'widget-field-action
  :match-alternatives '(functionp)
  :validate (lambda (widget)
	      (unless (functionp (widget-value widget))
		(widget-put widget :error (format "Invalid function: %S"
						  (widget-value widget)))
		widget))
  :value 'ignore
  :tag "Function")

(defvar widget-variable-prompt-value-history nil
  "History of input to `widget-variable-prompt-value'.")

(define-widget 'variable 'symbol
  "A Lisp variable."
  :prompt-match 'boundp
  :prompt-history 'widget-variable-prompt-value-history
  :complete-function (lambda ()
		       (interactive)
		       (lisp-complete-symbol 'boundp))
  :tag "Variable")

(defvar widget-coding-system-prompt-value-history nil
  "History of input to `widget-coding-system-prompt-value'.")
  
(define-widget 'coding-system 'symbol
  "A MULE coding-system."
  :format "%{%t%}: %v"
  :tag "Coding system"
  :base-only nil
  :prompt-history 'widget-coding-system-prompt-value-history
  :prompt-value 'widget-coding-system-prompt-value
  :action 'widget-coding-system-action
  :complete-function (lambda ()
		       (interactive)
		       (lisp-complete-symbol 'coding-system-p))
  :validate (lambda (widget)
	      (unless (coding-system-p (widget-value widget))
		(widget-put widget :error (format "Invalid coding system: %S"
						  (widget-value widget)))
		widget))
  :value 'undecided
  :prompt-match 'coding-system-p)

(defun widget-coding-system-prompt-value (widget prompt value unbound)
  "Read coding-system from minibuffer."
  (if (widget-get widget :base-only)
      (intern
       (completing-read (format "%s (default %s) " prompt value)
			(mapcar #'list (coding-system-list t)) nil nil nil
			coding-system-history))
      (read-coding-system (format "%s (default %s) " prompt value) value)))

(defun widget-coding-system-action (widget &optional event)
  (let ((answer
	 (widget-coding-system-prompt-value
	  widget
	  (widget-apply widget :menu-tag-get)
	  (widget-value widget)
	  t)))
    (widget-value-set widget answer)
    (widget-apply widget :notify widget event)
    (widget-setup)))

(define-widget 'sexp 'editable-field
  "An arbitrary Lisp expression."
  :tag "Lisp expression"
  :format "%{%t%}: %v"
  :value nil
  :validate 'widget-sexp-validate
  :match (lambda (widget value) t)
  :value-to-internal 'widget-sexp-value-to-internal
  :value-to-external (lambda (widget value) (read value))
  :prompt-history 'widget-sexp-prompt-value-history
  :prompt-value 'widget-sexp-prompt-value)

(defun widget-sexp-value-to-internal (widget value)
  ;; Use pp for printer representation.
  (let ((pp (if (symbolp value)
		(prin1-to-string value)
	      (pp-to-string value))))
    (while (string-match "\n\\'" pp)
      (setq pp (substring pp 0 -1)))
    (if (or (string-match "\n\\'" pp)
	    (> (length pp) 40))
	(concat "\n" pp)
      pp)))

(defun widget-sexp-validate (widget)
  ;; Valid if we can read the string and there is no junk left after it.
  (with-temp-buffer
    (insert (widget-apply widget :value-get))
    (goto-char (point-min))
    (let (err)
      (condition-case data
	  (progn
	    ;; Avoid a confusing end-of-file error.
	    (skip-syntax-forward "\\s-")
	    (if (eobp)
		(setq err "Empty sexp -- use `nil'?")
	      (unless (widget-apply widget :match (read (current-buffer)))
		(setq err (widget-get widget :type-error))))
	    (if (and (not (eobp))
		     (not err))
		(setq err (format "Junk at end of expression: %s"
				  (buffer-substring (point)
						    (point-max))))))
	(end-of-file			; Avoid confusing error message.
	 (setq err "Unbalanced sexp"))
	(error (setq err (error-message-string data))))
      (if (not err)
	  nil
	(widget-put widget :error err)
	widget))))

(defvar widget-sexp-prompt-value-history nil
  "History of input to `widget-sexp-prompt-value'.")

(defun widget-sexp-prompt-value (widget prompt value unbound)
  ;; Read an arbitrary sexp.
  (let ((found (read-string prompt
			    (if unbound nil (cons (prin1-to-string value) 0))
			    (widget-get widget :prompt-history))))
    (let ((answer (read-from-string found)))
      (unless (= (cdr answer) (length found))
	(error "Junk at end of expression: %s"
	       (substring found (cdr answer))))
      (car answer))))

(define-widget 'restricted-sexp 'sexp
  "A Lisp expression restricted to values that match.
To use this type, you must define :match or :match-alternatives."
  :type-error "The specified value is not valid"
  :match 'widget-restricted-sexp-match
  :value-to-internal (lambda (widget value)
		       (if (widget-apply widget :match value)
			   (prin1-to-string value)
			 value)))

(defun widget-restricted-sexp-match (widget value)
  (let ((alternatives (widget-get widget :match-alternatives))
	matched)
    (while (and alternatives (not matched))
      (if (cond ((functionp (car alternatives))
		 (funcall (car alternatives) value))
		((and (consp (car alternatives))
		      (eq (car (car alternatives)) 'quote))
		 (eq value (nth 1 (car alternatives)))))
	  (setq matched t))
      (setq alternatives (cdr alternatives)))
    matched))

(define-widget 'integer 'restricted-sexp
  "An integer."
  :tag "Integer"
  :value 0
  :type-error "This field should contain an integer"
  :match-alternatives '(integerp))

(define-widget 'number 'restricted-sexp
  "A floating point number."
  :tag "Number"
  :value 0.0
  :type-error "This field should contain a number"
  :match-alternatives '(numberp))

(define-widget 'character 'editable-field
  "A character."
  :tag "Character"
  :value 0
  :size 1
  :format "%{%t%}: %v\n"
  :valid-regexp "\\`.\\'"
  :error "This field should contain a single character"
  :value-to-internal (lambda (widget value)
		       (if (stringp value)
			   value
			 (char-to-string value)))
  :value-to-external (lambda (widget value)
		       (if (stringp value)
			   (aref value 0)
			 value))
  :match (lambda (widget value)
	   (char-valid-p value)))

(define-widget 'list 'group
  "A Lisp list."
  :tag "List"
  :format "%{%t%}:\n%v")

(define-widget 'vector 'group
  "A Lisp vector."
  :tag "Vector"
  :format "%{%t%}:\n%v"
  :match 'widget-vector-match
  :value-to-internal (lambda (widget value) (append value nil))
  :value-to-external (lambda (widget value) (apply 'vector value)))

(defun widget-vector-match (widget value)
  (and (vectorp value)
       (widget-group-match widget
			   (widget-apply widget :value-to-internal value))))

(define-widget 'cons 'group
  "A cons-cell."
  :tag "Cons-cell"
  :format "%{%t%}:\n%v"
  :match 'widget-cons-match
  :value-to-internal (lambda (widget value)
		       (list (car value) (cdr value)))
  :value-to-external (lambda (widget value)
		       (cons (nth 0 value) (nth 1 value))))

(defun widget-cons-match (widget value)
  (and (consp value)
       (widget-group-match widget
			   (widget-apply widget :value-to-internal value))))

;;; The `plist' Widget.
;;
;; Property lists.

(define-widget 'plist 'list
  "A property list."
  :key-type '(symbol :tag "Key")
  :value-type '(sexp :tag "Value")
  :convert-widget 'widget-plist-convert-widget
  :tag "Plist")

(defvar widget-plist-value-type)	;Dynamic variable

(defun widget-plist-convert-widget (widget)
  ;; Handle `:options'.
  (let* ((options (widget-get widget :options))
	 (widget-plist-value-type (widget-get widget :value-type))
	 (other `(editable-list :inline t
				(group :inline t
				       ,(widget-get widget :key-type)
				       ,widget-plist-value-type)))
	 (args (if options
		   (list `(checklist :inline t
				     :greedy t
				     ,@(mapcar 'widget-plist-convert-option
					       options))
			 other)
		 (list other))))
    (widget-put widget :args args)
    widget))

(defun widget-plist-convert-option (option)
  ;; Convert a single plist option.
  (let (key-type value-type)
    (if (listp option)
	(let ((key (nth 0 option)))
	  (setq value-type (nth 1 option))
	  (if (listp key)
	      (setq key-type key)
	    (setq key-type `(const ,key))))
      (setq key-type `(const ,option)
	    value-type widget-plist-value-type))
    `(group :format "Key: %v" :inline t ,key-type ,value-type)))


;;; The `alist' Widget.
;;
;; Association lists.

(define-widget 'alist 'list
  "An association list."
  :key-type '(sexp :tag "Key")
  :value-type '(sexp :tag "Value")
  :convert-widget 'widget-alist-convert-widget
  :tag "Alist")

(defvar widget-alist-value-type)	;Dynamic variable

(defun widget-alist-convert-widget (widget)
  ;; Handle `:options'.
  (let* ((options (widget-get widget :options))
	 (widget-alist-value-type (widget-get widget :value-type))
	 (other `(editable-list :inline t
				(cons :format "%v"
				      ,(widget-get widget :key-type)
				      ,widget-alist-value-type)))
	 (args (if options
		   (list `(checklist :inline t
				     :greedy t
				     ,@(mapcar 'widget-alist-convert-option
					       options))
			 other)
		 (list other))))
    (widget-put widget :args args)
    widget))

(defun widget-alist-convert-option (option)
  ;; Convert a single alist option.
  (let (key-type value-type)
    (if (listp option)
	(let ((key (nth 0 option)))
	  (setq value-type (nth 1 option))
	  (if (listp key)
	      (setq key-type key)
	    (setq key-type `(const ,key))))
      (setq key-type `(const ,option)
	    value-type widget-alist-value-type))
    `(cons :format "Key: %v" ,key-type ,value-type)))

(define-widget 'choice 'menu-choice
  "A union of several sexp types."
  :tag "Choice"
  :format "%{%t%}: %[Value Menu%] %v"
  :button-prefix 'widget-push-button-prefix
  :button-suffix 'widget-push-button-suffix
  :prompt-value 'widget-choice-prompt-value)

(defun widget-choice-prompt-value (widget prompt value unbound)
  "Make a choice."
  (let ((args (widget-get widget :args))
	(completion-ignore-case (widget-get widget :case-fold))
	current choices old)
    ;; Find the first arg that matches VALUE.
    (let ((look args))
      (while look
	(if (widget-apply (car look) :match value)
	    (setq old (car look)
		  look nil)
	  (setq look (cdr look)))))
    ;; Find new choice.
    (setq current
	  (cond ((= (length args) 0)
		 nil)
		((= (length args) 1)
		 (nth 0 args))
		((and (= (length args) 2)
		      (memq old args))
		 (if (eq old (nth 0 args))
		     (nth 1 args)
		   (nth 0 args)))
		(t
		 (while args
		   (setq current (car args)
			 args (cdr args))
		   (setq choices
			 (cons (cons (widget-apply current :menu-tag-get)
				     current)
			       choices)))
		 (let ((val (completing-read prompt choices nil t)))
		   (if (stringp val)
		       (let ((try (try-completion val choices)))
			 (when (stringp try)
			   (setq val try))
			 (cdr (assoc val choices)))
		     nil)))))
    (if current
	(widget-prompt-value current prompt nil t)
      value)))

(define-widget 'radio 'radio-button-choice
  "A union of several sexp types."
  :tag "Choice"
  :format "%{%t%}:\n%v"
  :prompt-value 'widget-choice-prompt-value)

(define-widget 'repeat 'editable-list
  "A variable length homogeneous list."
  :tag "Repeat"
  :format "%{%t%}:\n%v%i\n")

(define-widget 'set 'checklist
  "A list of members from a fixed set."
  :tag "Set"
  :format "%{%t%}:\n%v")

(define-widget 'boolean 'toggle
  "To be nil or non-nil, that is the question."
  :tag "Boolean"
  :prompt-value 'widget-boolean-prompt-value
  :button-prefix 'widget-push-button-prefix
  :button-suffix 'widget-push-button-suffix
  :format "%{%t%}: %[Toggle%]  %v\n"
  :on "on (non-nil)"
  :off "off (nil)")

(defun widget-boolean-prompt-value (widget prompt value unbound)
  ;; Toggle a boolean.
  (y-or-n-p prompt))

;;; The `color' Widget.

;; Fixme: match 
(define-widget 'color 'editable-field
  "Choose a color name (with sample)."
  :format "%t: %v (%{sample%})\n"
  :size 10
  :tag "Color"
  :value "black"
  :complete 'widget-color-complete
  :sample-face-get 'widget-color-sample-face-get
  :notify 'widget-color-notify
  :action 'widget-color-action)

(defun widget-color-complete (widget)
  "Complete the color in WIDGET."
  (require 'facemenu)			; for facemenu-color-alist
  (let* ((prefix (buffer-substring-no-properties (widget-field-start widget)
						 (point)))
	 (list (or facemenu-color-alist
		   (mapcar 'list (defined-colors))))
	 (completion (try-completion prefix list)))
    (cond ((eq completion t)
	   (message "Exact match."))
	  ((null completion)
	   (error "Can't find completion for \"%s\"" prefix))
	  ((not (string-equal prefix completion))
	   (insert-and-inherit (substring completion (length prefix))))
	  (t
	   (message "Making completion list...")
	   (with-output-to-temp-buffer "*Completions*"
	     (display-completion-list (all-completions prefix list nil)))
	   (message "Making completion list...done")))))

(defun widget-color-sample-face-get (widget)
  (let* ((value (condition-case nil
		    (widget-value widget)
		  (error (widget-get widget :value)))))
    (if (color-defined-p value)
	(list (cons 'foreground-color value))
      'default)))

(defun widget-color-action (widget &optional event)
  "Prompt for a color."
  (let* ((tag (widget-apply widget :menu-tag-get))
	 (prompt (concat tag ": "))
	 (value (widget-value widget))
	 (start (widget-field-start widget))
	 (pos (cond ((< (point) start)
		     0)
		    ((> (point) (+ start (length value)))
		     (length value))
		    (t
		     (- (point) start))))
	 (answer (facemenu-read-color prompt)))
    (unless (zerop (length answer))
      (widget-value-set widget answer)
      (widget-setup)
      (widget-apply widget :notify widget event))))

(defun widget-color-notify (widget child &optional event)
  "Update the sample, and notofy the parent."
  (overlay-put (widget-get widget :sample-overlay)
	       'face (widget-apply widget :sample-face-get))
  (widget-default-notify widget child event))

;;; The Help Echo

(defun widget-echo-help (pos)
  "Display help-echo text for widget at POS."
  (let* ((widget (widget-at pos))
	 (help-echo (and widget (widget-get widget :help-echo))))
    (if (functionp help-echo)
	(setq help-echo (funcall help-echo widget)))
    (if (stringp help-echo)
	(message "%s" help-echo))))

;;; The End:

(provide 'wid-edit)

;;; wid-edit.el ends here
