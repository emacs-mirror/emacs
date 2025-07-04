;; wid-edit.el --- Functions for creating and using widgets -*- lexical-binding:t -*-
;;
;; Copyright (C) 1996-1997, 1999-2025 Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: extensions
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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

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
(require 'cl-lib)

;; The `string' widget completion uses this.
(declare-function ispell-get-word "ispell"
                  (following &optional extra-otherchars))

;;; Compatibility.

(defsubst widget-event-point (event)
  "Character position of the end of event if that exists, or nil.
EVENT can either be a mouse event or a touch screen event."
  (posn-point (event-end event)))

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
  :group 'extensions)

(defgroup widget-documentation nil
  "Options controlling the display of documentation strings."
  :group 'widgets)

(defgroup widget-faces nil
  "Faces used by the widget library."
  :group 'widgets
  :group 'faces)

(defvar widget-documentation-face 'widget-documentation
  "Face used for documentation strings in widgets.
This exists as a variable so it can be set locally in certain buffers.")

(defface widget-documentation '((((class color)
				  (background dark))
				 (:foreground "lime green"))
				(((class color)
				  (background light))
				 (:foreground "dark green"))
				(t nil))
  "Face used for documentation text."
  :group 'widget-documentation
  :group 'widget-faces)

(defvar widget-button-face 'widget-button
  "Face used for buttons in widgets.
This exists as a variable so it can be set locally in certain buffers.")

(defface widget-button '((t (:weight bold)))
  "Face used for widget buttons."
  :group 'widget-faces)

(defcustom widget-mouse-face 'highlight
  "Face used for widget buttons when the mouse is above them."
  :type 'face
  :group 'widget-faces)

;; TTY gets special definitions here and in the next defface, because
;; the gray colors defined for other displays cause black text on a black
;; background, at least on light-background TTYs.
(defface widget-field '((((type tty))
			 :background "yellow3"
			 :foreground "black"
			 :extend t)
			(((class grayscale color)
			  (background light))
			 :background "gray85"
                         ;; We use negative thickness of the horizontal box border line to
                         ;; avoid making lines taller when fields become visible.
                         :box (:line-width (1 . -1) :color "gray80")
			 :extend t)
			(((class grayscale color)
			  (background dark))
			 :background "dim gray"
                         :box (:line-width (1 . -1) :color "gray46")
			 :extend t)
                        ;; Monochrome displays.
                        (((background light))
                         :background "white"
                         :box (:line-width (1 . -1) :color "black")
			 :extend t)
                        (((background dark))
                         :background "black"
                         :box (:line-width (1 . -1) :color "white")
			 :extend t)
			(t
			 :slant italic
			 :extend t))
  "Face used for editable fields."
  :group 'widget-faces
  :version "30.1")

(defface widget-single-line-field '((((type tty))
				     :background "green3"
				     :foreground "black")
				    (((class grayscale color)
				      (background light))
				     :background "gray85")
				    (((class grayscale color)
				      (background dark))
				     :background "dim gray")
                                    ;; Monochrome displays.
                                    (((background light))
                                     :stipple "gray3"
			             :extend t)
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
  "Largest number of items allowed in a popup menu.
Larger menus are read through the minibuffer."
  :group 'widgets
  :type 'integer)

(defcustom widget-menu-max-shortcuts 40
  "Largest number of items for which it works to choose one with a character.
For a larger number of items, the minibuffer is used."
  :group 'widgets
  :type 'integer)

(defcustom widget-menu-minibuffer-flag nil
  "Non-nil means use the minibuffer; to ask for a choice from the keyboard.
If nil, read a single character."
  :group 'widgets
  :type 'boolean)

(defun widget--simplify-menu (extended)
  "Convert the EXTENDED menu into a menu composed of simple menu items.

Each item in the simplified menu is of the form (ITEM-STRING . REAL-BINDING),
where both elements are taken from the EXTENDED MENU.  ITEM-STRING is the
correspondent ITEM-NAME in the menu-item entry:
 (menu-item ITEM-NAME REAL-BINDING . ITEM-PROPERTY-LIST), and REAL-BINDING is
the symbol in the key vector, as in `define-key'.
 (See `(elisp)Defining Menus' for more information.)

Only visible, enabled and meaningful menu items make their way into
the returned simplified menu.  That is:
For the menu item to be visible, it has to either lack a :visible form in its
item-property-list, or the :visible form has to evaluate to a non-nil value.
For the menu item to be enabled, it has to either lack a :enabled form in its
item-property-list, or the :enable form has to evaluate to a non-nil value.
Additionally, if the menu item is a radio button, then its selected form has
to evaluate to nil for the menu item to be meaningful."
  (let (simplified)
    (map-keymap (lambda (ev def)
                  (when (and (eq (nth 0 def) 'menu-item)
                             (nth 2 def)) ; Only menu-items with a real binding.
                    ;; Loop through the item-property-list, looking for
                    ;; :visible, :enable (or :active) and :button properties.
                    (let ((plist (nthcdr 3 def))
                          (enable t) ; Enabled by default.
                          (visible t) ; Visible by default.
                          selected keyword value)
                      (while (and plist (cdr plist)
                                  (keywordp (setq keyword (car plist))))
                        (setq value (cadr plist))
                        (cond ((memq keyword '(:visible :included))
                               (setq visible value))
                              ((memq keyword '(:enable :active))
                               (setq enable value))
                              ((and (eq keyword :button)
                                    (eq (car value) :radio))
                               (setq selected (cdr value))))
                        (setq plist (cddr plist)))
                      (when (and (eval visible t)
                                 (eval enable t)
                                 (or (not selected)
                                     (not (eval selected t))))
                        (push (cons (nth 1 def) ev) simplified)))))
                extended)
    (reverse simplified)))

(defun widget-choose (title items &optional event)
  "Choose an item from a list.

First argument TITLE is the name of the list.
Second argument ITEMS should be a menu, either with simple item definitions,
or with extended item definitions.
When ITEMS has simple item definitions, it is a list whose members are either
 (NAME . VALUE), to indicate selectable items, or just strings to
 indicate unselectable items.

When ITEMS is a menu that uses an extended format, then ITEMS should be a
keymap, and each binding should look like this:
 (menu-item ITEM-NAME REAL-BINDING . ITEM-PROPERTY-LIST)
or like this: (menu-item ITEM-NAME) to indicate a non-selectable item.
REAL-BINDING should be a symbol, and should not be a keymap, because submenus
are not supported.

Optional third argument EVENT is an input event.

If EVENT is a mouse event, and the number of elements in items is less than
`widget-menu-max-size', a popup menu will be used, otherwise the
minibuffer.

The user is asked to choose between each NAME from ITEMS.
If ITEMS has simple item definitions, then this function returns the VALUE of
the chosen element.  If ITEMS is a keymap, then the return value is the symbol
in the key vector, as in the argument of `define-key'."
  ;; Apply substitution to choice menu title and item text, whether it
  ;; occurs in a widget buffer or in a popup menu.
  (let ((items (mapc (lambda (x)
                       (if (proper-list-p x)
                           (dotimes (i (1- (length x)))
                             (when (stringp (nth i x))
                               (setcar (nthcdr i x)
                                       (substitute-command-keys
                                        (car (nthcdr i x))))))
                         ;; ITEMS has simple item definitions.
                         (when (and (consp x) (stringp (car x)))
                           (setcar x (substitute-command-keys (car x))))))
		     items))
        (title (substitute-command-keys title)))
    (cond ((and (< (length items) widget-menu-max-size)
	        event (display-popup-menus-p))
	   ;; Mouse click.
           (if (keymapp items)
               ;; Modify the keymap prompt, and then restore the old one, if any.
               (let ((prompt (keymap-prompt items)))
                 (unwind-protect
                     (progn
                       (setq items (delete prompt items))
                       (push title (cdr items))
                       ;; Return just the first element of the list of events.
                       (car (x-popup-menu event items)))
                   (setq items (delete title items))
                   (when prompt
                     (push prompt (cdr items)))))
	     (x-popup-menu event (list title (cons "" items)))))
	  ((or widget-menu-minibuffer-flag
	       (> (length items) widget-menu-max-shortcuts))
           (when (keymapp items)
             (setq items (widget--simplify-menu items)))
	   ;; Read the choice of name from the minibuffer.
	   (setq items (cl-remove-if #'stringp items))
	   (let ((val (completing-read (concat title ": ") items nil t)))
	     (if (stringp val)
	         (let ((try (try-completion val items)))
		   (when (stringp try)
		     (setq val try))
		   (cdr (assoc val items))))))
	  (t
           (when (keymapp items)
             (setq items (widget--simplify-menu items)))
	   ;; Construct a menu of the choices
	   ;; and then use it for prompting for a single character.
	   (let ((next-digit ?0)
		 alist some-choice-enabled value)
	     (with-current-buffer (get-buffer-create " widget-choose")
	       (erase-buffer)
	       (insert "Available choices:\n\n")
	       (dolist (choice items)
	         (when (consp choice)
                   (insert (format "%c = %s\n" next-digit (car choice)))
                   (push (cons next-digit (cdr choice)) alist)
                   (setq some-choice-enabled t))
	         ;; Allocate digits to disabled alternatives
	         ;; so that the digit of a given alternative never varies.
	         (setq next-digit (1+ next-digit)))
	       (insert "\nC-g = Quit")
	       (goto-char (point-min))
	       (forward-line))
	     (or some-choice-enabled
	         (error "None of the choices is currently meaningful"))
	     (save-window-excursion
               ;; Select window to be able to scroll it from minibuffer
               (with-selected-window
                   (display-buffer (get-buffer " widget-choose")
                                   '(display-buffer-in-direction
                                     (direction . bottom)
                                     (window-height . fit-window-to-buffer)))
                 (setq value (read-char-choice
                              (format "%s: " title)
                              (mapcar #'car alist)))))
	     (cdr (assoc value alist)))))))

;;; Widget text specifications.
;;
;; These functions are for specifying text properties.

;; We can set it to nil now that get_local_map uses get_pos_property.
(defconst widget-field-add-space nil
  "Non-nil means add extra space at the end of editable text fields.
If you don't add the space, it will become impossible to edit a zero
size field.")

(defvar widget-field-use-before-change t
  "Non-nil means use `before-change-functions' to track editable fields.
This enables the use of undo.  Using before hooks also means that
the :notify function can't know the new value.")

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
	(face (or (widget-get widget :value-face) 'widget-field))
	(help-echo (widget-get widget :help-echo))
	(follow-link (widget-get widget :follow-link))
	(rear-sticky
	 (or (not widget-field-add-space) (widget-get widget :size))))
    (if (functionp help-echo)
      (setq help-echo 'widget-mouse-help))
    (when (and (or (> to (1+ from)) (null (widget-get widget :size)))
               (= (char-before to) ?\n))
      ;; When the last character in the field is a newline, we want to
      ;; give it a `field' char-property of `boundary', which helps the
      ;; C-n/C-p act more naturally when entering/leaving the field.  We
      ;; do this by making a small secondary overlay to contain just that
      ;; one character.  BUT we only do this if there is more than one
      ;; character (so we don't do this for the character widget),
      ;; or if the size of the editable field isn't specified.
      (let ((overlay (make-overlay (1- to) to nil t nil)))
        ;; Save it so that we can easily delete it in
        ;; `widget-field-value-delete'.  (Bug#75646)
        (widget-put widget :field-end-overlay overlay)
	(overlay-put overlay 'field 'boundary)
        ;; We need the real field for tabbing.
	(overlay-put overlay 'real-field widget)
	;; Use `local-map' here, not `keymap', so that normal editing
	;; works in the field when, say, Custom uses `suppress-keymap'.
	(overlay-put overlay 'local-map keymap)
	(overlay-put overlay 'face face)
	(overlay-put overlay 'follow-link follow-link)
        (overlay-put overlay 'help-echo help-echo)
        ;; Since the `widget-field' face has a :box attribute, we need to add
        ;; some character with no face after the newline character, to avoid
        ;; clashing with text that comes after the field and has a face with
        ;; a :box attribute too.  (Bug#51550)
        (overlay-put overlay 'after-string #(" " 0 1 (invisible t))))
      (setq to (1- to))
      (setq rear-sticky t))
    (let ((overlay (make-overlay from to nil nil rear-sticky)))
      (widget-put widget :field-overlay overlay)
      ;;(overlay-put overlay 'detachable nil)
      (overlay-put overlay 'field widget)
      (overlay-put overlay 'local-map keymap)
      (overlay-put overlay 'face face)
      (overlay-put overlay 'follow-link follow-link)
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
		      (eq (char-after (1- end)) ?\s))
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
	(follow-link (widget-get widget :follow-link))
	(help-echo (widget-get widget :help-echo))
	(face (unless (widget-get widget :suppress-face)
		(widget-apply widget :button-face-get))))
    (widget-put widget :button-overlay overlay)
    (when (functionp help-echo)
      (setq help-echo 'widget-mouse-help))
    (overlay-put overlay 'button widget)
    (overlay-put overlay 'keymap (widget-get widget :keymap))
    (overlay-put overlay 'evaporate t)
    ;; We want to avoid the face with image buttons.
    (when face
      (overlay-put overlay 'face face)
      (overlay-put overlay 'mouse-face
		   ;; Make new list structure for the mouse-face value
		   ;; so that different widgets will have
		   ;; different `mouse-face' property values
		   ;; and will highlight separately.
		   (let ((mouse-face-value
			  (widget-apply widget :mouse-face-get)))
		     ;; If it's a list, copy it.
		     (if (listp mouse-face-value)
			 (copy-sequence mouse-face-value)
		       ;; If it's a symbol, put it in a list.
		       (list mouse-face-value)))))
    (overlay-put overlay 'pointer 'hand)
    (overlay-put overlay 'follow-link follow-link)
    (overlay-put overlay 'help-echo help-echo)))

(defun widget-mouse-help (_window overlay _point)
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
    (overlay-put overlay 'evaporate t)
    (widget-put widget :sample-overlay overlay)))

(defun widget-specify-doc (widget from to)
  "Specify documentation for WIDGET between FROM and TO."
  (let ((overlay (make-overlay from to nil t nil)))
    (overlay-put overlay 'widget-doc widget)
    (overlay-put overlay 'face widget-documentation-face)
    (overlay-put overlay 'evaporate t)
    (widget-put widget :doc-overlay overlay)))

(defun widget--should-indent-p (&optional check-after)
  "Non-nil if we should indent at the current position.
With CHECK-AFTER non-nil, considers also the content after point, if needed."
  (save-restriction
    (widen)
    (and (eq (preceding-char) ?\n)
         (or (not check-after)
             ;; If there is a space character, then we probably already
             ;; indented it.
             (not (eq (following-char) ?\s))))))

(defmacro widget--allow-insertion (&rest forms)
  "Run FORMS such that they can insert widgets in the current buffer."
  (declare (debug t))
  `(let ((inhibit-read-only t)
	 (inhibit-modification-hooks t)) ;; FIXME: Why?  This is risky!
     ,@forms))

(defmacro widget-specify-insert (&rest forms)
  "Execute FORMS without inheriting any text properties."
  (declare (debug t))
  `(save-restriction
     (widget--allow-insertion
      (narrow-to-region (point) (point))
      (prog1 (progn ,@forms)
	(goto-char (point-max))))))

(defface widget-inactive
  '((t :inherit shadow))
  "Face used for inactive widgets."
  :group 'widget-faces)

(defun widget-specify-inactive (widget from to)
  "Make WIDGET inactive for user modifications.

If WIDGET is already inactive, moves the :inactive overlay to the positions
indicated by FROM and TO, either numbers or markers.

If WIDGET is not inactive, creates an overlay that spans from FROM to TO,
and saves that overlay under the :inactive property for WIDGET."
  (if (widget-get widget :inactive)
      (move-overlay (widget-get widget :inactive) from to)
    (let ((overlay (make-overlay from to nil t nil)))
      (overlay-put overlay 'face 'widget-inactive)
      ;; This is disabled, as it makes the mouse cursor change shape.
      ;; (overlay-put overlay 'mouse-face 'widget-inactive)
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'priority 100)
      (overlay-put overlay 'modification-hooks '(widget-overlay-inactive))
      (widget-put widget :inactive overlay))))

(defun widget-overlay-inactive (&rest _junk)
  "Ignoring the arguments, signal an error."
  (unless inhibit-read-only
    (error "The widget here is not active")))


(defun widget-specify-active (widget)
  "Make WIDGET active for user modifications."
  (let ((inactive (widget-get widget :inactive)))
    (when inactive
      (delete-overlay inactive)
      (widget-put widget :inactive nil))))

(defface widget-unselected
  '((t :inherit widget-inactive))
  "Face used for unselected widgets."
  :group 'widget-faces
  :version "30.1")

(defun widget-specify-unselected (widget from to)
  "Fontify WIDGET as unselected."
  (let ((overlay (make-overlay from to nil t nil)))
    (overlay-put overlay 'face 'widget-unselected)
    (overlay-put overlay 'evaporate t)
    ;; The overlay priority here should be lower than the priority in
    ;; `widget-specify-active' (bug#69942).
    (overlay-put overlay 'priority 90)
    (widget-put widget :unselected overlay)))

(defun widget-specify-selected (widget)
  "Remove fontification of WIDGET as unselected."
  (let ((unselected (widget-get widget :unselected)))
    (when unselected
      (delete-overlay unselected)
      (widget-put widget :unselected nil))))

;;; Widget Properties.

(defsubst widget-type (widget)
  "Return the type of WIDGET.  The type is a symbol."
  (car widget))

;;;###autoload
(defun widgetp (widget)
  "Return non-nil if WIDGET is a widget."
  (if (symbolp widget)
      (get widget 'widget-type)
    (and (consp widget)
	 (symbolp (car widget))
	 (get (car widget) 'widget-type))))

;;;###autoload
(defun widget-put (widget property value)
  "In WIDGET, set PROPERTY to VALUE.
The value can later be retrieved with `widget-get'."
  (setcdr widget (plist-put (cdr widget) property value))
  value)

;;;###autoload
(defun widget-get (widget property)
  "In WIDGET, get the value of PROPERTY.
The value could either be specified when the widget was created, or
later with `widget-put'."
  (let (value)
    (while (and widget
                (let ((found (plist-member (cdr widget) property)))
                  (cond (found
                         (setq value (cadr found))
                         nil)
                        (t
                         (setq widget (get (widget-type widget) 'widget-type))
                         t)))))
    value))

(defun widget-get-indirect (widget property)
  "In WIDGET, get the value of PROPERTY.
If the value is a symbol, return its binding.
Otherwise, just return the value."
  (let ((value (widget-get widget property)))
    (if (symbolp value)
	(symbol-value value)
      value)))

(defun widget-member (widget property)
  "Non-nil if there is a definition in WIDGET for PROPERTY."
  (cond ((plist-member (cdr widget) property)
	 t)
	((car widget)
	 (widget-member (get (car widget) 'widget-type) property))
	(t nil)))

;;;###autoload
(defun widget-apply (widget property &rest args)
  "Apply the value of WIDGET's PROPERTY to the widget itself.
Return the result of applying the value of PROPERTY to WIDGET.
ARGS are passed as extra arguments to the function."
  (apply (widget-get widget property) widget args))

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
  "Extract the default external value of WIDGET."
  (widget-apply widget :value-to-external
		(or (widget-get widget :value)
		    (progn
		      (when (widget-get widget :args)
			(setq widget (widget-copy widget))
			(let (args)
			  (dolist (arg (widget-get widget :args))
			    (setq args (append args
					       (if (widget-get arg :inline)
						   (widget-get arg :args)
						 (list arg)))))
			  (widget-put widget :args args)))
		      (widget-apply widget :default-get)))))

(defun widget-inline-p (widget &optional bubblep)
  "Non-nil if the widget WIDGET is inline.

With BUBBLEP non-nil, check also if WIDGET has a member that bubbles its inline
property (if any), up to WIDGET, so that WIDGET can act as an inline widget."
  (or (widget-get widget :inline)
      (and bubblep
           (widget-get widget :inline-bubbles-p)
           (widget-apply widget :inline-bubbles-p))))

(defun widget-match-inline (widget vals)
  "In WIDGET, match the start of VALS.

For an inline widget or for a widget that acts like one (see `widget-inline-p'),
try to match elements in VALS as far as possible.  Otherwise, match the first
element of the list VALS.

Return a list whose car contains all members of VALS that matched WIDGET."
  (cond ((widget-inline-p widget t)
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
  (setq widget (ensure-list widget))
  (setq prompt (format "[%s] %s" (widget-type widget) prompt))
  (setq widget (widget-convert widget))
  (let ((answer (widget-apply widget :prompt-value prompt value unbound)))
    (unless (widget-apply widget :match answer)
      (error "Value does not match %S type" (car widget)))
    answer))

(defun widget-get-sibling (widget)
  "Get the item WIDGET is assumed to toggle.
This is only meaningful for radio buttons or checkboxes in a list."
  (let* ((children (widget-get (widget-get widget :parent) :children)))
    (catch 'child
      (dolist (child children)
	(when (eq (widget-get child :button) widget)
	  (throw 'child child)))
      nil)))

(defun widget-map-buttons (function &optional buffer maparg)
  "Map FUNCTION over the buttons in BUFFER.
FUNCTION is called with the arguments WIDGET and MAPARG.

If FUNCTION returns non-nil, the walk is canceled.

The arguments MAPARG, and BUFFER default to nil and (current-buffer),
respectively."
  (let ((cur (point-min))
	(widget nil)
	(overlays (if buffer
		      (with-current-buffer buffer (overlay-lists))
		    (overlay-lists))))
    (setq overlays (append (car overlays) (cdr overlays)))
    (while (setq cur (pop overlays))
      (setq widget (overlay-get cur 'button))
      (if (and widget (funcall function widget maparg))
	  (setq overlays nil)))))

(defun widget-describe (&optional widget-or-pos)
  "Describe the widget at point.
Displays a buffer with information about the widget (e.g., its actions) as well
as a link to browse all the properties of the widget.

This command resolves the indirection of widgets running the action of its
parents, so the real action executed can be known.

When called from Lisp, pass WIDGET-OR-POS as the widget to describe,
or a buffer position where a widget is present.  If WIDGET-OR-POS is nil,
the widget at point is the widget to describe."
  (interactive "d")
  (require 'wid-browse) ; The widget-browse widget.
  (let ((widget (if (widgetp widget-or-pos)
                    widget-or-pos
                  (widget-at widget-or-pos)))
        props)
    (when widget
      (help-setup-xref (list #'widget-describe widget)
                       (called-interactively-p 'interactive))
      (setq props (list (cons 'action (widget--resolve-parent-action widget))
                        (cons 'mouse-down-action
                              (widget-get widget :mouse-down-action))))
      (with-help-window (help-buffer)
        (with-current-buffer (help-buffer)
          (widget-insert "This widget's type is ")
          (widget-create 'widget-browse :format "%[%v%]\n%d"
                         :doc (get (car widget) 'widget-documentation)
                         :help-echo "Browse this widget's properties"
                         widget)
          (dolist (action '(action mouse-down-action))
            (let ((name (symbol-name action))
                  (val (alist-get action props)))
              (when (functionp val)
                (widget-insert "\n\n" (propertize (capitalize name) 'face 'bold)
                               "'\nThe " name " of this widget is")
                (if (symbolp val)
                    (progn (widget-insert " ")
                           (widget-create 'function-link :value val
                                          :button-prefix "" :button-suffix ""
                                          :help-echo "Describe this function"))
                  (widget-insert "\n")
                  (princ val)))))))
      (widget-setup)
      t)))

(defun widget--resolve-parent-action (widget)
  "Resolve the real action of WIDGET up its inheritance chain.
Follow the WIDGET's parents, until its :action is no longer
`widget-parent-action', and return its value."
  (let ((action (widget-get widget :action))
        (parent (widget-get widget :parent)))
    (while (eq action 'widget-parent-action)
      (setq parent (widget-get parent :parent)
            action (widget-get parent :action)))
    action))

;;; Images.

(defcustom widget-image-directory (file-name-as-directory
				   (expand-file-name "images/custom" data-directory))
  "Where widget button images are located.
If this variable is nil, widget will try to locate the directory
automatically."
  :group 'widgets
  :type 'directory)

(defcustom widget-image-enable t
  "If non-nil, use image buttons in widgets when available."
  :version "21.1"
  :group 'widgets
  :type 'boolean)

(defcustom widget-image-conversion
  '((svg ".svg") (xpm ".xpm") (gif ".gif") (png ".png") (jpeg ".jpg" ".jpeg")
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
	       (push (list :type (car elt) :file (concat image ext))
		     specs)))
 	   (find-image (nreverse specs))))
	(t
	 ;; Oh well.
	 nil)))

(defvar widget-button-pressed-face 'widget-button-pressed
  "Face used for pressed buttons in widgets.
This exists as a variable so it can be set locally in certain
buffers.")

(defun widget-image-insert (widget tag image &optional _down _inactive)
  "In WIDGET, insert the text TAG or, if supported, IMAGE.
IMAGE should either be an image or an image file name sans extension
\(xpm, xbm, gif, jpg, or png) located in `widget-image-directory'.

Optional arguments DOWN and INACTIVE are used instead of IMAGE when the
button is pressed or inactive, respectively.  These are currently ignored."
  (if (and (featurep 'image)
	   (setq image (widget-image-find image)))
      (progn (widget-put widget :suppress-face t)
	     (insert-image image tag))
    (insert tag)))

(defun widget-move-and-invoke (event)
  "Move to where you click, and if it is an active field, invoke it."
  (interactive "e")
  (mouse-set-point event)
  (let ((pos (widget-event-point event)))
    (if (and pos (get-char-property pos 'button))
	(widget-button-click event))))

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
  (let ((widget (apply #'widget-convert type args)))
    (widget-apply widget :create)
    widget))

(defun widget-create-child-and-convert (parent type &rest args)
  "As part of the widget PARENT, create a child widget TYPE.
The child is converted, using the keyword arguments ARGS."
  (let ((widget (apply #'widget-convert type args)))
    (widget-put widget :parent parent)
    (unless (widget-get widget :indent)
      (widget-put widget :indent (+ (or (widget-get parent :indent) 0)
				    (or (widget-get widget :extra-offset) 0)
				    (widget-get parent :offset))))
    (widget-apply widget :create)
    widget))

(defun widget-create-child (parent type)
  "Create widget of TYPE."
  (let ((widget (widget-copy type)))
    (widget-put widget :parent parent)
    (unless (widget-get widget :indent)
      (widget-put widget :indent (+ (or (widget-get parent :indent) 0)
				    (or (widget-get widget :extra-offset) 0)
				    (widget-get parent :offset))))
    (widget-apply widget :create)
    widget))

(defun widget-create-child-value (parent type value)
  "Create widget of TYPE with value VALUE."
  (let ((widget (widget-copy type)))
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

(defun widget-copy (widget)
  "Make a deep copy of WIDGET."
  (widget-apply (copy-sequence widget) :copy))

;;;###autoload
(defun widget-convert (type &rest args)
  "Convert TYPE to a widget without inserting it in the buffer.
The optional ARGS are additional keyword arguments."
  ;; Don't touch the type.
  (let* ((widget (if (symbolp type)
		     (list type)
		   (copy-sequence type)))
	 (current widget)
	 done
	 (keys args))
    ;; First set the :args keyword.
    (while (cdr current)		;Look in the type.
      (setq current
	    (if (and (keywordp (cadr current))
	             ;; If the last element is a keyword,
	             ;; it is still the :args element,
	             ;; even though it is a keyword.
	             (cddr current))
	        (if (eq (cadr current) :args)
	            ;; If :args is explicitly specified, obey it.
	            nil
	          ;; Some other irrelevant keyword.
	          (cdr (cdr current)))
	      (setcdr current (list :args (cdr current)))
	      nil)))
    (while (and args (not done))	;Look in ARGS.
      (cond ((eq (car args) :args)
	     ;; Handle explicit specification of :args.
	     (setq args (cadr args)
		   done t))
	    ((keywordp (car args))
	     (setq args (cddr args)))
	    (t (setq done t))))
    (when done
      (widget-put widget :args args))
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
	(setq keys (when (keywordp next)
	             (widget-put widget next (nth 1 keys))
	             (nthcdr 2 keys)))))
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
  (widget--allow-insertion
    (apply #'insert args)))

(defun widget-convert-text (type from to
				 &optional button-from button-to
				 &rest args)
  "Return a widget of type TYPE with endpoint FROM TO.
No text will be inserted to the buffer, instead the text between FROM
and TO will be used as the widgets end points.  If optional arguments
BUTTON-FROM and BUTTON-TO are given, these will be used as the widgets
button end points.
Optional ARGS are extra keyword arguments for TYPE."
  (let ((widget (apply #'widget-convert type :delete 'widget-leave-text args))
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
  (apply #'widget-convert-text type from to from to args))

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
    (mapc #'widget-leave-text (widget-get widget :children))))

(defun widget-text (widget)
  "Get the text representation of the widget."
  (when-let* ((from (widget-get widget :from))
              (to (widget-get widget :to)))
    (when (eq (marker-buffer from) (marker-buffer to)) ; is this check necessary?
      (buffer-substring-no-properties from to))))

;;; Keymap and Commands.

;;;###autoload
(defvar widget-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'widget-forward)
    (define-key map "\e\t" 'widget-backward)
    (define-key map [(shift tab)] 'widget-backward)
    (put 'widget-backward :advertised-binding [(shift tab)])
    (define-key map [backtab] 'widget-backward)
    (define-key map [down-mouse-2] 'widget-button-click)
    (define-key map [down-mouse-1] 'widget-button-click)
    (define-key map [touchscreen-begin] 'widget-button-click)
    ;; The following definition needs to avoid using escape sequences that
    ;; might get converted to ^M when building loaddefs.el
    (define-key map [(control ?m)] 'widget-button-press)
    map)
  "Keymap containing useful binding for buffers containing widgets.
Recommended as a parent keymap for modes using widgets.
Note that such modes will need to require wid-edit.")

(defvar-local widget-global-map global-map
  "Keymap used for events a widget does not handle itself.")

(defvar widget-field-keymap
  (let ((map (copy-keymap widget-keymap)))
    (define-key map "\C-k" #'widget-kill-line)
    (define-key map "\M-\t" #'widget-complete)
    (define-key map "\C-m" #'widget-field-activate)
    ;; Since the widget code uses a `field' property to identify fields,
    ;; ordinary beginning-of-line does the right thing.
    ;;  (define-key map "\C-a" #'widget-beginning-of-line)
    (define-key map "\C-e" #'widget-end-of-line)
    map)
  "Keymap used inside an editable field.")

(defvar widget-text-keymap
  (let ((map (copy-keymap widget-keymap)))
    ;; Since the widget code uses a `field' property to identify fields,
    ;; ordinary beginning-of-line does the right thing.
    ;;  (define-key map "\C-a" #'widget-beginning-of-line)
    (define-key map "\C-e" #'widget-end-of-line)
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

(defface widget-button-pressed
  '((((min-colors 88) (class color))
     (:foreground "red1"))
    (((class color))
     (:foreground "red"))
    (t
     (:weight bold :underline t)))
  "Face used for pressed buttons."
  :group 'widget-faces)

(defvar widget-button-click-moves-point nil
  "If non-nil, `widget-button-click' moves point to a button after invoking it.
If nil, point returns to its original position after invoking a button.")

(defun widget-button--check-and-call-button (event button)
  "Call BUTTON if BUTTON is a widget and EVENT is correct for it.
EVENT can either be a mouse event or a touchscreen-begin event.
If nothing was called, return non-nil."
  (let* ((oevent event)
         (mouse-1 (memq (event-basic-type event) '(mouse-1 down-mouse-1)))
         (pos (widget-event-point event))
         newpoint)
    (setq newpoint
          (catch 'button-press-cancelled
            ;; Mouse click on a widget button.  Do the following
            ;; in a save-excursion so that the click on the button
            ;; doesn't change point.
            (save-selected-window
              (select-window (posn-window (event-start event)))
              (save-excursion
	        (goto-char (posn-point (event-start event)))
	        (let* ((overlay (widget-get button :button-overlay))
	               (pressed-face (or (widget-get button :pressed-face)
				         widget-button-pressed-face))
	               (face (overlay-get overlay 'face))
	               (mouse-face (overlay-get overlay 'mouse-face)))
	          (unwind-protect
	              ;; Read events, including mouse-movement events,
	              ;; waiting for a release event.  If we began with
	              ;; a mouse-1 event and receive a movement event,
	              ;; that means the user wants to perform
	              ;; drag-selection, so cancel the button press and
	              ;; do the default mouse-1 action.  For mouse-2,
	              ;; just highlight/ unhighlight the button the
	              ;; mouse was initially on when we move over it.
                      ;;
                      ;; If this function was called in response to a
                      ;; touchscreen event, then wait for a
                      ;; corresponding touchscreen-end event instead.
	              (save-excursion
		        (when face ; avoid changing around image
		          (overlay-put overlay 'face pressed-face)
		          (overlay-put overlay 'mouse-face pressed-face))
                        (if (eq (car event) 'touchscreen-begin)
                            ;; This a touchscreen event and must be
                            ;; handled specially through
                            ;; `touch-screen-track-tap'.
                            (progn
                              (unless (touch-screen-track-tap event nil nil t)
                                ;; Report the current position of point
                                ;; to the catch block.
                                (throw 'button-press-cancelled (point))))
                          (unless (widget-apply button :mouse-down-action event)
                            (let ((track-mouse t))
                              (while (not (widget-button-release-event-p event))
                                (setq event (read--potential-mouse-event))
                                (when (and mouse-1 (mouse-movement-p event))
                                  (push event unread-command-events)
                                  (setq event oevent)
                                  (throw 'button-press-cancelled nil))
                                (unless (or (integerp event)
                                            (memq (car event)
                                                  '(switch-frame select-window))
                                            (eq (car event)
                                                'scroll-bar-movement))
                                  (setq pos (widget-event-point event))
                                  (if (and pos
                                           (eq (get-char-property pos 'button)
                                               button))
                                      (when face
                                        (overlay-put overlay
                                                     'face pressed-face)
                                        (overlay-put overlay
                                                     'mouse-face pressed-face))
                                    (overlay-put overlay
                                                 'face face)
                                    (overlay-put overlay
                                                 'mouse-face mouse-face)))))))

		        ;; When mouse is released over the button, run
		        ;; its action function.
		        (when (and pos (eq (get-char-property pos 'button)
                                           button))
		          (goto-char pos)
		          (widget-apply-action button event)
		          (if widget-button-click-moves-point
		              (setq newpoint (point)))))
	            (overlay-put overlay 'face face)
	            (overlay-put overlay 'mouse-face mouse-face))))
              (when newpoint
                (goto-char newpoint)))
            nil))
    ;; Return to the position of point as it existed during the
    ;; button-tracking loop if the event being tracked is a touch screen
    ;; event, to prevent hscroll from being disturbed by movement of
    ;; point to any previous location outside the visible confines of
    ;; the window.
    (when newpoint (goto-char newpoint))))

(defun widget-button-click (event)
  "Invoke the button that the mouse is pointing at."
  (interactive "e")
  (if (widget-event-point event)
      (let* ((mouse-1 (memq (event-basic-type event) '(mouse-1 down-mouse-1)))
	     (pos (widget-event-point event))
	     (start (event-start event))
             (button (get-char-property
		      pos 'button (and (windowp (posn-window start))
				       (window-buffer (posn-window start))))))

	(when (or (null button)
                  (widget-button--check-and-call-button event button))
	  (let ((up (not (eq (car event) 'touchscreen-begin)))
                command)
	    ;; Mouse click not on a widget button.  Find the global
	    ;; command to run, and check whether it is bound to an
	    ;; up event.
            (cond
             ((eq (car event) 'touchscreen-begin)
              (setq command 'touch-screen-handle-touch))
             (mouse-1 (cond ((setq command	;down event
                                   (lookup-key widget-global-map [down-mouse-1]))
                             (setq up nil))
                            ((setq command	;up event
                                   (lookup-key widget-global-map [mouse-1])))))
             (t (cond ((setq command	;down event
                             (lookup-key widget-global-map [down-mouse-2]))
                       (setq up nil))
                      ((setq command	;up event
                             (lookup-key widget-global-map [mouse-2]))))))
	    (when up
	      ;; Don't execute up events twice.
	      (while (not (and (widget-button-release-event-p event)))
		(setq event (read--potential-mouse-event))))
	    (when command
	      (call-interactively command)))))
    (message "You clicked somewhere weird.")))

;; Make sure `touch-screen-handle-touch' abstains from emulating
;; down-mouse-1 events for `widget-button-click'.

(put 'widget-button-click 'ignored-mouse-command t)

(defun widget-button-press (pos &optional event)
  "Invoke button at POS."
  (interactive "@d")
  (let ((button (get-char-property pos 'button)))
    (if button
	(widget-apply-action button event)
      (let ((command (lookup-key widget-global-map (this-command-keys))))
	(when (commandp command)
	  (call-interactively command))))))

(defcustom widget-skip-inactive nil
  "If non-nil, skip inactive widgets when tabbing through buffer."
  :version "30.1"
  :group 'widgets
  :type 'boolean)

(defun widget-tabable-at (&optional pos)
  "Return the tabable widget at POS, or nil.
POS defaults to the value of (point).  If user option
`widget-skip-inactive' is non-nil, inactive widgets are not tabable."
  (let ((widget (widget-at pos)))
    (if (and widget (if widget-skip-inactive
                        (widget-apply widget :active)
                      t))
	(let ((order (widget-get widget :tab-order)))
	  (if order
	      (if (>= order 0)
		  widget)
	    widget)))))

(defvar widget-use-overlay-change t
  "If non-nil, use overlay change functions to tab around in the buffer.
This is much faster.")

(defun widget-move (arg &optional suppress-echo)
  "Move point to the ARG next field or button.
ARG may be negative to move backward.
When the second optional argument is non-nil,
nothing is shown in the echo area."
  (let* ((wrapped 0)
	 (number arg)
         (fwd (> arg 0))                ; widget-forward is caller.
         (bwd (< arg 0))                ; widget-backward is caller.
	 (old (widget-tabable-at))
         (tabable (if old 1 0))
         pos)
    (catch 'one
      (while (> (abs arg) 0)
        (cond ((or (and fwd (eobp)) (and bwd (bobp)))
	       (goto-char (cond (fwd (point-min))
                                (bwd (point-max))))
	       (setq wrapped (1+ wrapped)))
	      (widget-use-overlay-change
	       (goto-char (cond (fwd (next-overlay-change (point)))
                                (bwd (previous-overlay-change (point))))))
	      (t
	       (cond (fwd (forward-char 1))
                     (bwd (backward-char 1)))))
        (and (= wrapped 2)
	     (eq arg number)
             (if (= tabable 1)
                 (progn
                   (goto-char pos)
                   (throw 'one (message "Only one tabable widget")))
	       (error "No buttons or fields found")))
        (let ((new (widget-tabable-at)))
	  (when new
	    (if (eq new old)
                (setq pos (point))
              (incf tabable)
	      (setq arg (cond (fwd (1- arg))
                              (bwd (1+ arg))))
	      (setq old new))))))
    (let ((new (widget-tabable-at)))
      (while (and (eq (widget-tabable-at) new) (not (bobp)))
	(backward-char)))
    ;; If the widget is at BOB, point is already at the widget's
    ;; starting position; otherwise, advance point to put it at the
    ;; start of the widget (cf. bug#69943 and bug#72995).
    (unless (and (widget-tabable-at) (bobp)) (forward-char)))
  (unless suppress-echo
    (widget-echo-help (point)))
  (run-hooks 'widget-move-hook))

(defun widget-forward (arg &optional suppress-echo)
  "Move point to the next field or button.
With optional ARG, move across that many fields.
When the second optional argument is non-nil,
nothing is shown in the echo area."
  (interactive "p")
  (run-hooks 'widget-forward-hook)
  (widget-move arg suppress-echo))

(defun widget-backward (arg &optional suppress-echo)
  "Move point to the previous field or button.
With optional ARG, move across that many fields.
When the second optional argument is non-nil,
nothing is shown in the echo area."
  (interactive "p")
  (run-hooks 'widget-backward-hook)
  (widget-move (- arg) suppress-echo))

;; Since the widget code uses a `field' property to identify fields,
;; ordinary beginning-of-line does the right thing.
(defalias 'widget-beginning-of-line #'beginning-of-line)

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

(defun widget-narrow-to-field ()
  "Narrow to field."
  (interactive)
  (let ((field (widget-field-find (point))))
    (if field
	(narrow-to-region (line-beginning-position) (line-end-position)))))

;; This used to say:
;; "When not inside a field, move to the previous button or field."
;; but AFAICS, it has always just thrown an error.
(defun widget-complete ()
  "Complete content of editable field from point.
When not inside a field, signal an error."
  (interactive)
  (let ((data (widget-completions-at-point)))
    (cond
     ((functionp data) (funcall data))
     ((consp data)
      (let ((completion-extra-properties (nth 3 data)))
        (completion-in-region (nth 0 data) (nth 1 data) (nth 2 data)
                              (plist-get completion-extra-properties
                                         :predicate))))
     (t
      (error "No completions available for this field")))))
;; We may want to use widget completion in buffers where the major mode
;; hasn't added widget-completions-at-point to completion-at-point-functions,
;; so it's not really obsolete (yet).
;; (make-obsolete 'widget-complete 'completion-at-point "24.1")

(defun widget-completions-at-point ()
  (let ((field (widget-field-find (point))))
    (if field
        (widget-apply field :completions-function)
      (error "Not in an editable field"))))

;;; Setting up the buffer.

(defvar-local widget-field-new nil
  "List of all newly created editable fields in the buffer.")

(defvar-local widget-field-list nil
  "List of all editable fields in the buffer.")

(defun widget-at (&optional pos)
  "The button or field at POS (default, point)."
  (let ((widget (or (get-char-property (or pos (point)) 'button)
                    (widget-field-at pos))))
    (and (widgetp widget) widget)))

;;;###autoload
(defun widget-setup ()
  "Setup current buffer so editing string widgets works."
  (widget--allow-insertion
   (let (field)
     (while widget-field-new
       (setq field (car widget-field-new)
	     widget-field-new (cdr widget-field-new)
	     widget-field-list (cons field widget-field-list))
       (let ((from (car (widget-get field :field-overlay)))
	     (to (cdr (widget-get field :field-overlay))))
	 (widget-specify-field field
			       (marker-position from) (marker-position to))
	 (set-marker from nil)
	 (set-marker to nil)))))
  (widget-clear-undo)
  (widget-add-change))

(defvar-local widget-field-last nil
  "Last field containing point.")

(defvar-local widget-field-was nil
  "The widget data before the change.")

(defun widget-field-at (pos)
  "Return the widget field at POS, or nil if none."
  (let ((field (get-char-property (or pos (point)) 'field)))
    (if (eq field 'boundary)
	(get-char-property (or pos (point)) 'real-field)
      field)))

(defun widget-field-buffer (widget)
  "Return the buffer of WIDGET's editing field."
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
        ;; Don't proceed if overlay has been removed from buffer.
        (when (overlay-buffer overlay)
          (if (and (not (eq (with-current-buffer
                                (widget-field-buffer widget)
                              (save-restriction
                                ;; `widget-narrow-to-field' can be
                                ;; active when this function is called
                                ;; from a change-functions hook. So
                                ;; temporarily remove field narrowing
                                ;; before to call `get-char-property'.
                                (widen)
                                (get-char-property (overlay-end overlay)
                                                   'field)))
                            'boundary))
                   (or widget-field-add-space
                       (null (widget-get widget :size))))
              (1- (overlay-end overlay))
            (overlay-end overlay)))
      (cdr overlay))))

(defun widget-field-text-end (widget)
  (let ((to   (widget-field-end widget))
	(size (widget-get widget :size)))
    (if (or (null size) (zerop size))
        to
      (let ((from (widget-field-start widget)))
        (if (and from to)
            (with-current-buffer (widget-field-buffer widget)
              (while (and (> to from)
                          (eq (char-after (1- to)) ?\s))
                (setq to (1- to)))
              to))))))

(defun widget-field-find (pos)
  "Return the field at POS.
Unlike (get-char-property POS \\='field), this works with empty fields too."
  (let (found)
    (dolist (field widget-field-list)
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
	     (add-hook 'post-command-hook #'widget-add-change nil t)
	     (signal 'text-read-only
		     '("Change should be restricted to a single field")))
	    ((null from-field)
	     (add-hook 'post-command-hook #'widget-add-change nil t)
	     (signal 'text-read-only
		     '("Attempt to change text outside editable field")))
	    (widget-field-use-before-change
	     (widget-apply from-field :notify
                           from-field (list 'before-change from to)))))))

(defun widget-add-change ()
  (remove-hook 'post-command-hook #'widget-add-change t)
  (add-hook 'before-change-functions #'widget-before-change nil t)
  (add-hook 'after-change-functions #'widget-after-change nil t))

(defun widget-after-change (from to _old)
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
		     (insert-char ?\s (- (+ begin size) end))))
		  ((> (- end begin) size)
		   ;; Field too large and
		   (setq begin (if (or (< (point) (+ begin size))
			               (> (point) end))
			           ;; Point is outside extra space.
			           (+ begin size)
			         ;; Point is within the extra space.
			         (point)))
		   (save-excursion
		     (goto-char end)
		     (while (and (eq (preceding-char) ?\s)
				 (> (point) begin))
		       (delete-char -1)))))))
	(widget-specify-secret field))
      (widget-apply field :notify field (list 'after-change from to)))))

;;; Widget Functions
;;
;; These functions are used in the definition of multiple widgets.

(defun widget-parent-action (widget &optional event)
  "Tell :parent of WIDGET to handle the :action.
Optional EVENT is the event that triggered the action."
  (widget-apply (widget-get widget :parent) :action event))

(defun widget-children-value-delete (widget)
  "Delete all :children and :buttons in WIDGET."
  (mapc #'widget-delete (widget-get widget :children))
  (widget-put widget :children nil)
  (mapc #'widget-delete (widget-get widget :buttons))
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

(defun widget-child-value-get (widget)
  "Get the value of the first member of :children in WIDGET."
  (widget-value (car (widget-get widget :children))))

(defun widget-child-value-inline (widget)
  "Get the inline value of the first member of :children in WIDGET."
  (widget-apply (car (widget-get widget :children)) :value-inline))

(defun widget-child-validate (widget)
  "The result of validating the first member of :children in WIDGET."
  (widget-apply (car (widget-get widget :children)) :validate))

(defun widget-type-value-create (widget)
  "Convert and instantiate the value of the :type attribute of WIDGET.
Store the newly created widget in the :children attribute.

The value of the :type attribute should be an unconverted widget type."
  (let ((value (widget-get widget :value))
	(type (widget-get widget :type)))
    (widget-put widget :children
                (list (widget-create-child-value widget
                                                 (widget-convert type)
                                                 value)))))

(defun widget-type-default-get (widget)
  "Get default value from the :type attribute of WIDGET.

The value of the :type attribute should be an unconverted widget type."
  (widget-default-get (widget-convert (widget-get widget :type))))

(defun widget-type-match (widget value)
  "Non-nil if the :type value of WIDGET matches VALUE.

The value of the :type attribute should be an unconverted widget type."
  (widget-apply (widget-convert (widget-get widget :type)) :match value))

(defun widget-types-copy (widget)
  "Copy :args as widget types in WIDGET."
  (widget-put widget :args (mapcar #'widget-copy (widget-get widget :args)))
  widget)

;; Made defsubst to speed up face editor creation.
(defsubst widget-types-convert-widget (widget)
  "Convert :args as widget types in WIDGET."
  (widget-put widget :args (mapcar #'widget-convert (widget-get widget :args)))
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
  :value-to-internal (lambda (_widget value) value)
  :value-to-external (lambda (_widget value) value)
  :button-prefix 'widget-button-prefix
  :button-suffix 'widget-button-suffix
  :completions-function #'widget-default-completions
  :create 'widget-default-create
  :indent nil
  :offset 0
  :format-handler 'widget-default-format-handler
  :button-face-get 'widget-default-button-face-get
  :mouse-face-get 'widget-default-mouse-face-get
  :sample-face-get 'widget-default-sample-face-get
  :delete 'widget-default-delete
  :copy 'identity
  :value-set 'widget-default-value-set
  :value-inline 'widget-default-value-inline
  :value-delete 'ignore
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

(defvar widget--completing-widget)

(defun widget-default-completions (widget)
  "Return completion data, like `completion-at-point-functions' would."
  (let ((completions (widget-get widget :completions)))
    (cond
     (completions
      (list (widget-field-start widget)
            (max (point) (widget-field-text-end widget))
            completions))
     ((widget-get widget :complete)
      (lambda () (widget-apply widget :complete)))
     ((widget-get widget :complete-function)
      (lambda ()
        (let ((widget--completing-widget widget))
          (call-interactively
           (widget-get widget :complete-function))))))))

(defun widget--prepare-markers-for-inside-insertion (widget)
  "Prepare the WIDGET's parent for insertions inside it, if necessary.

Usually, the :from marker has type t, while the :to marker has type nil.
When recreating a child or a button inside a composite widget right at these
markers, they have to be changed to nil and t respectively,
so that the WIDGET's parent (if any), properly contains all of its
recreated children and buttons.

Prepares also the markers of the WIDGET's grandparent, if necessary.

Returns a list of the markers that had its type changed, for later resetting."
  (let* ((parent (widget-get widget :parent))
         (parent-from-marker (and parent (widget-get parent :from)))
         (parent-to-marker (and parent (widget-get parent :to)))
         (lst nil)
         (pos (point)))
    (when (and parent-from-marker
               (eq pos (marker-position parent-from-marker))
               (marker-insertion-type parent-from-marker))
      (set-marker-insertion-type parent-from-marker nil)
      (push (cons parent-from-marker t) lst))
    (when (and parent-to-marker
               (eq pos (marker-position parent-to-marker))
               (not (marker-insertion-type parent-to-marker)))
      (set-marker-insertion-type parent-to-marker t)
      (push (cons parent-to-marker nil) lst))
    (when lst
      (nconc lst (widget--prepare-markers-for-inside-insertion parent)))))

(defun widget--revert-markers-for-outside-insertion (markers)
  "Revert MARKERS for insertions that do not belong to a widget.

MARKERS is a list of the form (MARKER . NEW-TYPE), as returned by
`widget--prepare-markers-for-inside-insertion' and this function sets MARKER
to NEW-TYPE.

Coupled with `widget--prepare-parent-for-inside-insertion', this has the effect
of setting markers back to the type needed for insertions that do not belong
to a given widget."
  (dolist (marker markers)
    (set-marker-insertion-type (car marker) (cdr marker))))

(defun widget-default-create (widget)
  "Create WIDGET at point in the current buffer."
  (widget-specify-insert
   (let ((str (widget-get widget :format))
         (onext 0) (next 0)
	 button-begin button-end
	 sample-begin sample-end
	 doc-begin doc-end
         value-pos
         (markers (widget--prepare-markers-for-inside-insertion widget)))
     ;; Parse escapes in format.
     (while (string-match "%\\(.\\)" str next)
       (setq next (match-end 1))
       ;; If we skipped some literal text, insert it.
       (when (/= (- next onext) 2)
         (insert (substring str onext (- next 2))))
       (let ((escape (string-to-char (match-string 1 str))))
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
		  (insert-char ?\s (widget-get widget :indent))))
	       ((eq escape ?t)
		(let ((image (widget-get widget :tag-glyph))
		      (tag (substitute-command-keys
			    (widget-get widget :tag))))
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
		    (insert (substitute-command-keys doc))
		    (while (eq (preceding-char) ?\n)
		      (delete-char -1))
		    (insert ?\n)
		    (setq doc-end (point)))))
	       ((eq escape ?h)
		(widget-add-documentation-string-button widget))
	       ((eq escape ?v)
		(if (and button-begin (not button-end))
		    (widget-apply widget :value-create)
		  (setq value-pos (point))))
	       (t
		(widget-apply widget :format-handler escape))))
       (setq onext next))
     ;; Insert remaining literal text, if any.
     (when (> (length str) next)
       (insert (substring str next)))
     ;; Specify button, sample, and doc, and insert value.
     (and button-begin button-end
	  (widget-specify-button widget button-begin button-end))
     (and sample-begin sample-end
	  (widget-specify-sample widget sample-begin sample-end))
     (and doc-begin doc-end
	  (widget-specify-doc widget doc-begin doc-end))
     (when value-pos
       (goto-char value-pos)
       (widget-apply widget :value-create))
     (widget--revert-markers-for-outside-insertion markers))
   (let ((from (point-min-marker))
	 (to (point-max-marker)))
     (set-marker-insertion-type from t)
     (set-marker-insertion-type to nil)
     (widget-put widget :from from)
     (widget-put widget :to to)))
  (widget-clear-undo))

(defun widget-default-format-handler (_widget escape)
  (error "Unknown escape `%c'" escape))

(defun widget-default-button-face-get (widget)
  ;; Use :button-face or widget-button-face
  (or (widget-get widget :button-face)
      (let ((parent (widget-get widget :parent)))
	(if parent
	    (widget-apply parent :button-face-get)
	  widget-button-face))))

(defun widget-default-mouse-face-get (widget)
  ;; Use :mouse-face or widget-mouse-face
  (or (widget-get widget :mouse-face)
      (let ((parent (widget-get widget :parent)))
	(if parent
	    (widget-apply parent :mouse-face-get)
	  widget-mouse-face))))

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
	(doc-overlay (widget-get widget :doc-overlay)))
    (widget--allow-insertion
     (widget-apply widget :value-delete)
     (widget-children-value-delete widget)
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
     (set-marker to nil)))
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
	(goto-char (if (< offset 0)
	               (+ (widget-get widget :to) offset 1)
	             (min (+ from offset) (1- (widget-get widget :to))))))))

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
  "Return t if this widget is active (user modifiable)."
  (or (widget-get widget :always-active)
      (and (not (widget-get widget :inactive))
	   (let ((parent (widget-get widget :parent)))
	     (or (null parent)
		 (widget-apply parent :active)))
           t)))

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

(defun widget-default-notify (widget _child &optional event)
  "Pass notification to parent."
  (widget-default-action widget event))

(defun widget-default-prompt-value (_widget prompt _value _unbound)
  "Read an arbitrary value."
  (eval-minibuffer prompt))

(defun widget-docstring (widget)
  "Return the documentation string specified by WIDGET, or nil if none.
If WIDGET has a `:doc' property, that specifies the documentation string.
Otherwise, try the `:documentation-property' property.  If this
is a function, call it with the widget's value as an argument; if
it is a symbol, use this symbol together with the widget's value
as the argument to `documentation-property'."
  (let ((doc (or (widget-get widget :doc)
		 (let ((doc-prop (widget-get widget :documentation-property))
		       (value (widget-get widget :value)))
		   (cond ((functionp doc-prop)
			  (funcall doc-prop value))
			 ((symbolp doc-prop)
			  (documentation-property value doc-prop t)))))))
    (when (and (stringp doc) (> (length doc) 0))
      ;; Remove any redundant `*' in the beginning.
      (when (eq (aref doc 0) ?*)
	(setq doc (substring doc 1)))
      ;; Remove trailing newlines.
      (when (string-match "\n+\\'" doc)
	(setq doc (substring doc 0 (match-beginning 0))))
      doc)))

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

(defun widget-item-match-inline (widget vals)
  ;; Match if the value is the same.
  (let ((value (widget-get widget :value)))
    (and (listp value)
	 (<= (length value) (length vals))
         (let ((head (seq-subseq vals 0 (length value))))
	   (and (equal head value)
                (cons head (seq-subseq vals (length value))))))))

(defun widget-item-action (widget &optional event)
  ;; Just notify itself.
  (widget-apply widget :notify widget event))

;;; The `push-button' Widget.

;; (defcustom widget-push-button-gui t
;;   "If non-nil, use GUI push buttons when available."
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
  (let* ((tag (or (substitute-command-keys (widget-get widget :tag))
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

(defvar widget-link-keymap
  (let ((map (copy-keymap widget-keymap)))
    ;; Only bind mouse-2, since mouse-1 will be translated accordingly to
    ;; the customization of `mouse-1-click-follows-link'.
    (define-key map [down-mouse-1] (lookup-key widget-global-map [down-mouse-1]))
    (define-key map [down-mouse-2] #'widget-button-click)
    (define-key map [mouse-2] #'widget-button-click)
    map)
  "Keymap used inside a link widget.")

(define-widget 'link 'item
  "An embedded link."
  :button-prefix 'widget-link-prefix
  :button-suffix 'widget-link-suffix
  :follow-link 'mouse-face
  :keymap widget-link-keymap
  :help-echo "Follow the link."
  :format "%[%t%]")

;;; The `info-link' Widget.

(define-widget 'info-link 'link
  "A link to an info file."
  :action 'widget-info-link-action)

(defun widget-info-link-action (widget &optional _event)
  "Open the info node specified by WIDGET."
  (info (widget-value widget)))

;;; The `url-link' Widget.

(define-widget 'url-link 'link
  "A link to a web page."
  :action 'widget-url-link-action)

(defun widget-url-link-action (widget &optional _event)
  "Open the URL specified by WIDGET."
  (browse-url (widget-value widget)))

;;; The `function-link' Widget.

(define-widget 'function-link 'link
  "A link to an Emacs function."
  :action 'widget-function-link-action)

(defun widget-function-link-action (widget &optional _event)
  "Show the function specified by WIDGET."
  (describe-function (widget-value widget)))

;;; The `variable-link' Widget.

(define-widget 'variable-link 'link
  "A link to an Emacs variable."
  :action 'widget-variable-link-action)

(defun widget-variable-link-action (widget &optional _event)
  "Show the variable specified by WIDGET."
  (describe-variable (widget-value widget)))

;;; The `face-link' Widget.

(define-widget 'face-link 'link
  "A link to an Emacs face."
  :action 'widget-face-link-action)

(defun widget-face-link-action (widget &optional _event)
  "Show the variable specified by WIDGET."
  (describe-face (widget-value widget)))

;;; The `file-link' Widget.

(define-widget 'file-link 'link
  "A link to a file."
  :action 'widget-file-link-action)

(defun widget-file-link-action (widget &optional _event)
  "Find the file specified by WIDGET."
  (find-file (widget-value widget)))

;;; The `emacs-library-link' Widget.

(define-widget 'emacs-library-link 'link
  "A link to an Emacs Lisp library file."
  :action 'widget-emacs-library-link-action)

(defun widget-emacs-library-link-action (widget &optional _event)
  "Find the Emacs library file specified by WIDGET."
  (find-file (locate-library (widget-value widget))))

;;; The `emacs-commentary-link' Widget.

(define-widget 'emacs-commentary-link 'link
  "A link to Commentary in an Emacs Lisp library file."
  :action 'widget-emacs-commentary-link-action)

(defun widget-emacs-commentary-link-action (widget &optional _event)
  "Find the Commentary section of the Emacs file specified by WIDGET."
  (finder-commentary (widget-value widget)))

;;; The `editable-field' Widget.

(define-widget 'editable-field 'default
  "An editable text field.
Note: In an `editable-field' widget, the `%v' escape must be preceded
by some other text in the `:format' string (if specified)."
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
  :value-set 'widget-field-value-set
  :value-delete 'widget-field-value-delete
  :value-get 'widget-field-value-get
  :match 'widget-field-match)

(defvar widget-field-history nil
  "History of field minibuffer edits.")

(defun widget-field-prompt-internal (_widget prompt initial history)
  "Read string for WIDGET prompting with PROMPT.
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

(defun widget-field-action (widget &optional _event)
  "Move to next field."
  (widget-forward 1)
  (run-hook-with-args 'widget-edit-functions widget))

(defun widget-field-validate (widget)
  "Valid if the content matches `:valid-regexp'."
  (unless (string-match (widget-get widget :valid-regexp)
			(widget-apply widget :value-get))
    widget))

(defun widget-field-value-set (widget value)
  "Set an editable text field WIDGET to VALUE."
  (let ((from (widget-field-start widget))
	(to (widget-field-text-end widget))
	(buffer (widget-field-buffer widget)))
    (when (and from to (buffer-live-p buffer))
      (with-current-buffer buffer
	(goto-char from)
	(delete-char (- to from))
	(insert value)))))

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
    (when value
      (insert value))
    (and size
	 (< (length value) size)
	 (insert-char ?\s (- size (length value))))
    (unless (memq widget widget-field-list)
      (setq widget-field-new (cons widget widget-field-new)))
    (move-marker (cdr overlay) (point))
    (set-marker-insertion-type (cdr overlay) nil)
    (when (null size)
      (insert ?\n))
    (move-marker (car overlay) from)
    (set-marker-insertion-type (car overlay) t)))

(defun widget-field-value-delete (widget)
  "Remove the field WIDGET from the list of active editing fields.

Delete its overlays as well."
  (setq widget-field-list (delq widget widget-field-list))
  (setq widget-field-new (delq widget widget-field-new))
  ;; These are nil if the :format string doesn't contain `%v'.
  (let ((overlay (widget-get widget :field-overlay)))
    (when (overlayp overlay)
      (delete-overlay overlay)))
  (let ((overlay (widget-get widget :field-end-overlay)))
    (when (overlayp overlay)
      (delete-overlay overlay))))

(defun widget-field-value-get (widget &optional no-truncate)
  "Return current text in editing field.
Normally, trailing spaces within the editing field are truncated.
But if NO-TRUNCATE is non-nil, include them."
  (let ((from (widget-field-start widget))
	(to   (if no-truncate
		  (widget-field-end widget)
		(widget-field-text-end widget)))
	(buffer (widget-field-buffer widget))
	(secret (widget-get widget :secret))
	(old (current-buffer)))
    (if (and from to)
	(progn
	  (set-buffer buffer)
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

(defun widget-field-match (_widget value)
  ;; Match any string.
  (stringp value))

;;; The `text' Widget.

(define-widget 'text 'editable-field
  "A multiline text area."
  :format "%{%t%}: %v"
  :keymap widget-text-keymap)

;;; The `menu-choice' Widget.

(define-widget 'menu-choice 'default
  "A menu of options."
  :convert-widget  'widget-types-convert-widget
  :copy 'widget-types-copy
  :format "%[%t%]: %v"
  :case-fold t
  :tag "choice"
  :void '(item :format "invalid (%t)\n")
  :value-create 'widget-choice-value-create
  :value-get 'widget-child-value-get
  :value-inline 'widget-child-value-inline
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
        current val inline-p fun)
    (if explicit
	(progn
	  ;; If the user specified the choice for this value,
	  ;; respect that choice.
	  (widget-put widget :children (list (widget-create-child-value
					      widget explicit value)))
	  (widget-put widget :choice explicit)
	  (widget-put widget :explicit-choice nil))
      (setq inline-p (widget-inline-p widget t))
      (while args
	(setq current (car args)
	      args (cdr args))
        (if inline-p
            (if (widget-get current :inline)
                (setq val value
                      fun :match-inline)
              (setq val (if (consp value)
                            (car value)
                          value)
                    fun :match))
          (setq val value
                fun :match))
        (when (widget-apply current fun val)
          (widget-put widget :children (list (widget-create-child-value
                                              widget current val)))
          (widget-put widget :choice current)
          (setq args nil
                current nil)))
      (when current
	(let ((void (widget-get widget :void)))
	  (widget-put widget :children (list (widget-create-child-and-convert
					      widget void :value value)))
	  (widget-put widget :choice void))))))

(defun widget-choice-default-get (widget)
  ;; Get default for the first choice.
  (widget-default-get (car (widget-get widget :args))))

(defcustom widget-choice-toggle nil
  "If non-nil, a binary choice will just toggle between the values.
Otherwise, the user will explicitly have to choose between the values
when he invoked the menu."
  :type 'boolean
  :group 'widgets)

(defun widget-choice-mouse-down-action (widget &optional _event)
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
		 (nth (if (eq old (nth 0 args)) 1 0)
		      args))
		(t
		 (dolist (current args)
		   (setq choices
			 (cons (cons (widget-apply current :menu-tag-get)
				     current)
			       choices)))
		 (setq this-explicit t)
		 (widget-choose tag (reverse choices) event))))
    (when current
      ;; If this was an explicit user choice, record the choice,
      ;; so that widget-choice-value-create will respect it.
      (when this-explicit
	(widget-put widget :explicit-choice current))
      (widget-value-set widget (widget-default-get current))
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

(defun widget-choice-match-inline (widget vals)
  ;; Matches if one of the choices matches.
  (let ((args (widget-get widget :args))
	current found)
    (while (and args (null found))
      (setq current (car args)
	    args (cdr args)
	    found (widget-match-inline current vals)))
    found))

;;; The `toggle' Widget.

(define-widget 'toggle 'item
  "Toggle between two states."
  :format "%[%v%]\n"
  :value-create 'widget-toggle-value-create
  :action 'widget-toggle-action
  :match (lambda (_widget _value) t)
  :on "on"
  :off "off")

(defun widget-toggle-value-create (widget)
  "Insert text representing the `on' and `off' states."
  (let* ((val (widget-value widget))
	 (text (substitute-command-keys
		(widget-get widget (if val :on :off))))
	 (img (widget-image-find
	       (widget-get widget (if val :on-glyph :off-glyph)))))
    (widget-image-insert widget (or text "")
			 (if img
			     (append img '(:ascent center))))))

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
  :on-glyph "checked"
  :off "[ ]"
  :off-glyph "unchecked"
  :help-echo "Toggle this item."
  :action 'widget-checkbox-action)

(defun widget-checkbox-action (widget &optional event)
  "Toggle checkbox, notify parent, and set active state of sibling."
  (widget-toggle-action widget event)
  (let* ((sibling (widget-get-sibling widget))
         (from (widget-get sibling :from))
	 (to (widget-get sibling :to)))
    (when sibling
      (if (widget-value widget)
          (progn
            (widget-apply sibling :activate)
            (widget-specify-selected sibling))
        :deactivate
        (widget-specify-unselected sibling from to))
      (widget-clear-undo))))

;;; The `checklist' Widget.

(define-widget 'checklist 'default
  "A multiple choice widget."
  :convert-widget 'widget-types-convert-widget
  :copy 'widget-types-copy
  :format "%v"
  :offset 4
  :entry-format "%b %v"
  :greedy nil
  :value-create 'widget-checklist-value-create
  :value-get 'widget-checklist-value-get
  :validate 'widget-checklist-validate
  :match 'widget-checklist-match
  :match-inline 'widget-checklist-match-inline)

(defun widget-checklist-value-create (widget)
  ;; Insert all values
  (let ((alist (widget-checklist-match-find widget))
	(args  (widget-get widget :args)))
    (dolist (item args)
      (widget-checklist-add-item widget item (assq item alist)))
    (widget-put widget :children (nreverse (widget-get widget :children)))))

(defun widget-checklist-add-item (widget type chosen)
  "Create checklist item in WIDGET of type TYPE.
If the item is checked, CHOSEN is a cons whose cdr is the value."
  (and (widget--should-indent-p)
       (widget-get widget :indent)
       (insert-char ?\s (widget-get widget :indent)))
  (widget-specify-insert
   (let* ((children (widget-get widget :children))
	  (buttons (widget-get widget :buttons))
	  (button-args (or (widget-get type :sibling-args)
			   (widget-get widget :button-args)))
          (str (widget-get widget :entry-format))
          (onext 0) (next 0)
	  child button)
     ;; Parse % escapes in format.
     (while (string-match "%\\([bv%]\\)" str next)
       (setq next (match-end 1))
       (when (/= (- next onext) 2)
         (insert (substring str onext (- next 2))))
       (let ((escape (string-to-char (match-string 1 str))))
	 (cond ((eq escape ?%)
		(insert ?%))
	       ((eq escape ?b)
		(setq button (apply #'widget-create-child-and-convert
				    widget 'checkbox
				    :value (not (null chosen))
				    button-args)))
	       ((eq escape ?v)
		(setq child
		      (cond ((not chosen)
			     (let* ((child (widget-create-child widget type))
                                    (from (widget-get child :from))
			            (to (widget-get child :to)))
                               (widget-specify-unselected child from to)
			       child))
                            ((widget-inline-p type t)
			     (widget-create-child-value
			      widget type (cdr chosen)))
			    (t
                             (widget-specify-selected child)
                             (widget-create-child-value
                              widget type (car (cdr chosen)))))))
	       (t
		(error "Unknown escape `%c'" escape))))
       (setq onext next))
     (when (> (length str) next)
       (insert (substring str next)))
     ;; Update properties.
     (and button child (widget-put child :button button))
     (and button (widget-put widget :buttons (cons button buttons)))
     (and child (widget-put widget :children (cons child children))))))

(defun widget-checklist-match (widget vals)
  ;; All values must match a type in the checklist.
  (and (listp vals)
       (null (cdr (widget-checklist-match-inline widget vals)))))

(defun widget-checklist-match-inline (widget vals)
  ;; Find the values which match a type in the checklist.
  (let ((greedy (widget-get widget :greedy))
	(args (copy-sequence (widget-get widget :args)))
	found rest)
    (while vals
      (let ((answer (widget-checklist-match-up args vals)))
	(cond (answer
	       (let ((vals2 (widget-match-inline answer vals)))
		 (setq found (append found (car vals2))
		       vals (cdr vals2)
		       args (delq answer args))))
	      (greedy
	       (setq rest (append rest (list (car vals)))
		     vals (cdr vals)))
	      (t
	       (setq rest (append rest vals)
		     vals nil)))))
    (cons found rest)))

(defun widget-checklist-match-find (widget &optional vals)
  "Find the vals which match a type in the checklist.
Return an alist of (TYPE MATCH)."
  (or vals (setq vals (widget-get widget :value)))
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
  (let (result)
    (dolist (child (widget-get widget :children))
      (if (widget-value (widget-get child :button))
	  (setq result (append result (widget-apply child :value-inline)))))
    result))

(defun widget-checklist-validate (widget)
  ;; Ticked children must be valid.
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
  :on-glyph "radio-checked"
  :off "( )"
  :off-glyph "radio")

(defun widget-radio-button-notify (widget _child &optional event)
  ;; Tell daddy.
  (widget-apply (widget-get widget :parent) :action widget event))

;;; The `radio-button-choice' Widget.

(define-widget 'radio-button-choice 'default
  "Select one of multiple options."
  :convert-widget 'widget-types-convert-widget
  :copy 'widget-types-copy
  :offset 4
  :format "%v"
  :entry-format "%b %v"
  :value-create 'widget-radio-value-create
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
  (dolist (arg (widget-get widget :args))
    (widget-radio-add-item widget arg)))

(defun widget-radio-add-item (widget type)
  "Add to radio widget WIDGET a new radio button item of type TYPE."
  ;; (setq type (widget-convert type))
  (and (widget--should-indent-p)
       (widget-get widget :indent)
       (insert-char ?\s (widget-get widget :indent)))
  (widget-specify-insert
   (let* ((value (widget-get widget :value))
	  (children (widget-get widget :children))
	  (buttons (widget-get widget :buttons))
	  (button-args (or (widget-get type :sibling-args)
			   (widget-get widget :button-args)))
          (str (widget-get widget :entry-format))
          (onext 0) (next 0)
	  (chosen (and (null (widget-get widget :choice))
		       (widget-apply type :match value)))
	  child button)
     ;; Parse % escapes in format.
     (while (string-match "%\\([bv%]\\)" str next)
       (setq next (match-end 1))
       (when (/= (- next onext) 2)
         (insert (substring str onext (- next 2))))
       (let ((escape (string-to-char (match-string 1 str))))
	 (cond ((eq escape ?%)
		(insert ?%))
	       ((eq escape ?b)
		(setq button (apply #'widget-create-child-and-convert
				    widget 'radio-button
				    :value (not (null chosen))
				    button-args)))
	       ((eq escape ?v)
		(setq child (if chosen
				(widget-create-child-value
				 widget type value)
			      (widget-create-child widget type)))
                (if chosen
                    (widget-specify-selected child)
                  (let ((from (widget-get child :from))
			(to (widget-get child :to)))
                    (widget-specify-unselected child from to))))
	       (t
		(error "Unknown escape `%c'" escape))))
       (setq onext next))
     (when (> (length str) next)
       (insert (substring str next)))
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
  (let (found)
    (dolist (current (widget-get widget :children))
      (let* ((button (widget-get current :button))
	     (match (and (not found)
			 (widget-apply current :match value)))
             (from (widget-get current :from))
	     (to (widget-get current :to)))
	(widget-value-set button match)
	(if match
            (progn
              (widget-value-set current value)
              (widget-apply current :activate)
              (widget-specify-selected current))
          (widget-specify-unselected current from to))
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
  (let ((buttons (widget-get widget :buttons)))
    (when (memq child buttons)
      (dolist (current (widget-get widget :children))
	(let* ((button (widget-get current :button))
               (from (widget-get current :from))
	       (to (widget-get current :to)))
	  (cond ((eq child button)
		 (widget-value-set button t)
		 (widget-apply current :activate)
                 (widget-specify-selected current))
		((widget-value button)
		 (widget-value-set button nil)
                 (widget-specify-unselected current from to)))))))
  ;; Pass notification to parent.
  (widget-apply widget :notify child event))

;;; The `insert-button' Widget.

(define-widget 'insert-button 'push-button
  "An insert button for the `editable-list' widget."
  :tag "INS"
  :help-echo (lambda (widget)
               (if (widget-get (widget-get widget :parent) :last-deleted)
                   "Insert back the last deleted item from this list, at this position."
                 "Insert a new item into the list at this position."))
  :action 'widget-insert-button-action)

(defun widget-insert-button-action (widget &optional _event)
  ;; Ask the parent to insert a new item.
  (widget-apply (widget-get widget :parent)
		:insert-before (widget-get widget :widget)))

;;; The `delete-button' Widget.

(define-widget 'delete-button 'push-button
  "A delete button for the `editable-list' widget."
  :tag "DEL"
  :help-echo "Delete this item from the list, saving it for later reinsertion."
  :action 'widget-delete-button-action)

(defun widget-delete-button-action (widget &optional _event)
  ;; Ask the parent to insert a new item.
  (widget-apply (widget-get widget :parent)
		:delete-at (widget-get widget :widget)))

;;; The `editable-list' Widget.

;; (defcustom widget-editable-list-gui nil
;;   "If non-nil, use GUI push-buttons in editable list when available."
;;   :type 'boolean
;;   :group 'widgets)

(define-widget 'editable-list 'default
  "A variable list of widgets of the same type."
  :convert-widget 'widget-types-convert-widget
  :copy 'widget-types-copy
  :offset 12
  :format "%v%i\n"
  :format-handler 'widget-editable-list-format-handler
  :entry-format "%i %d %v"
  :value-create 'widget-editable-list-value-create
  :value-get 'widget-editable-list-value-get
  :validate 'widget-children-validate
  :match 'widget-editable-list-match
  :match-inline 'widget-editable-list-match-inline
  :insert-before 'widget-editable-list-insert-before
  :delete-at 'widget-editable-list-delete-at)

(defun widget-editable-list-format-handler (widget escape)
  ;; We recognize the insert button.
    ;; (let ((widget-push-button-gui widget-editable-list-gui))
    (cond ((eq escape ?i)
	   (and (widget--should-indent-p)
                (widget-get widget :indent)
		(insert-char ?\s (widget-get widget :indent)))
	   (apply #'widget-create-child-and-convert
		  widget 'insert-button
		  (widget-get widget :append-button-args)))
	  (t
	   (widget-default-format-handler widget escape)))
    ;; )
  )

(defun widget-editable-list-value-create (widget)
  ;; Insert all values
  (let* ((value (widget-get widget :value))
	 (type (nth 0 (widget-get widget :args)))
	 children)
    (widget-put widget :value-pos (point-marker))
    (set-marker-insertion-type (widget-get widget :value-pos) t)
    (while value
      (let ((answer (widget-match-inline type value)))
	(if answer
	    (setq children (cons (widget-editable-list-entry-create
				  widget
                                  (car (if (widget-inline-p type t)
				           answer
				         (car answer)))
				  t)
				 children)
		  value (cdr answer))
	  (setq value nil))))
    (widget-put widget :children (nreverse children))))

(defun widget-editable-list-value-get (widget)
  ;; Get value of the child widget.
  (apply #'append (mapcar (lambda (child) (widget-apply child :value-inline))
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
  "Insert a new widget as a child of WIDGET.

If there is a recently deleted child, the new widget is that deleted child.
Otherwise, the new widget is the default child of WIDGET.

The new widget gets inserted at the position of the BEFORE child."
  (save-excursion
    (let ((children (widget-get widget :children))
          (last-deleted (when-let* ((lst (widget-get widget :last-deleted)))
                          (prog1
                              (pop lst)
                            (widget-put widget :last-deleted lst)))))
      (widget--allow-insertion
       (cond (before
	      (goto-char (widget-get before :entry-from)))
	     (t
	      (goto-char (widget-get widget :value-pos))))
       (let ((child (widget-editable-list-entry-create
                     widget (and last-deleted
                                 (widget-apply last-deleted
                                               :value-to-external
                                               (widget-get last-deleted :value)))
                     last-deleted)))
	 (when (< (widget-get child :entry-from) (widget-get widget :from))
	   (set-marker (widget-get widget :from)
		       (widget-get child :entry-from)))
	 (if (eq (car children) before)
	     (widget-put widget :children (cons child children))
	   (while (not (eq (car (cdr children)) before))
	     (setq children (cdr children)))
	   (setcdr children (cons child (cdr children))))))))
  (widget-setup)
  (widget-apply widget :notify widget))

(defun widget-editable-list-delete-at (widget child)
  "Delete the widget CHILD from the known children of widget WIDGET.

Save CHILD into the :last-deleted list, so it can be inserted later."
  ;; Save the current value of CHILD, to use if the user later inserts the
  ;; widget.
  (widget-put child :value (widget-apply child :value-get))
  (let ((lst (widget-get widget :last-deleted)))
    (push child lst)
    (widget-put widget :last-deleted lst))
  ;; Delete child from list of children.
  (save-excursion
    (widget--allow-insertion
     (dolist (button (copy-sequence (widget-get widget :buttons)))
       (when (eq (widget-get button :widget) child)
	 (widget-put widget
		     :buttons (delq button (widget-get widget :buttons)))
	 (widget-delete button))))
    (let ((entry-from (widget-get child :entry-from))
	  (entry-to (widget-get child :entry-to)))
      (widget--allow-insertion
       (widget-delete child)
       (delete-region entry-from entry-to)
       (set-marker entry-from nil)
       (set-marker entry-to nil)))
    (widget-put widget :children (delq child (widget-get widget :children))))
  (widget-setup)
  (widget-apply widget :notify widget))

(defun widget-editable-list-entry-create (widget value conv)
  ;; Create a new entry to the list.
  (let ((type (nth 0 (widget-get widget :args)))
	;; (widget-push-button-gui widget-editable-list-gui)
        (str (widget-get widget :entry-format))
        (onext 0) (next 0)
	child delete insert)
    (widget-specify-insert
     (and (widget--should-indent-p)
          (widget-get widget :indent)
          (insert-char ?\s (widget-get widget :indent)))
     ;; Parse % escapes in format.
     (while (string-match "%\\(.\\)" str next)
       (setq next (match-end 1))
       (when (/= (- next onext) 2)
         (insert (substring str onext (- next 2))))
       (let ((escape (string-to-char (match-string 1 str))))
	 (cond ((eq escape ?%)
		(insert ?%))
	       ((eq escape ?i)
		(setq insert (apply #'widget-create-child-and-convert
				    widget 'insert-button
				    (widget-get widget :insert-button-args))))
	       ((eq escape ?d)
		(setq delete (apply #'widget-create-child-and-convert
				    widget 'delete-button
				    (widget-get widget :delete-button-args))))
	       ((eq escape ?v)
		(setq child (widget-create-child-value
		             widget type
		             (if conv value (widget-default-get type)))))
	       (t
		(error "Unknown escape `%c'" escape))))
       (setq onext next))
     (when (> (length str) next)
       (insert (substring str next)))
     (let ((buttons (widget-get widget :buttons)))
       (if insert (push insert buttons))
       (if delete (push delete buttons))
       (widget-put widget :buttons buttons))
     ;; After creating the entry, we must check if we should indent the
     ;; following entry.  This is necessary, for example, to keep the correct
     ;; indentation of editable lists inside group widgets.
     (and (widget--should-indent-p t)
          (widget-get widget :indent)
          (insert-char ?\s (widget-get widget :indent)))
     (let ((entry-from (point-min-marker))
	   (entry-to (point-max-marker)))
       (set-marker-insertion-type entry-from t)
       (set-marker-insertion-type entry-to nil)
       (widget-put child :entry-from entry-from)
       (widget-put child :entry-to entry-to)))
    (if insert (widget-put insert :widget child))
    (if delete (widget-put delete :widget child))
    child))

;;; The `group' Widget.

(define-widget 'group 'default
  "A widget which groups other widgets inside."
  :convert-widget 'widget-types-convert-widget
  :copy 'widget-types-copy
  :format (concat (propertize ":" 'display "")
                  "\n%v")
  :value-create 'widget-group-value-create
  :value-get 'widget-editable-list-value-get
  :default-get 'widget-group-default-get
  :validate 'widget-children-validate
  :match 'widget-group-match
  :match-inline 'widget-group-match-inline)

(defun widget-group-value-create (widget)
  ;; Create each component.
  (let ((value (widget-get widget :value))
	answer children)
    (dolist (arg (widget-get widget :args))
      (setq answer (widget-match-inline arg value)
	    value (cdr answer))
      (and (widget--should-indent-p)
	   (widget-get widget :indent)
	   (insert-char ?\s (widget-get widget :indent)))
      (push (cond ((null answer)
		   (widget-create-child widget arg))
                  ((widget-inline-p arg t)
		   (widget-create-child-value widget arg (car answer)))
		  (t
		   (widget-create-child-value widget arg (car (car answer)))))
	    children))
    (widget-put widget :children (nreverse children))))

(defun widget-group-default-get (widget)
  ;; Get the default of the components.
  (mapcar #'widget-default-get (widget-get widget :args)))

(defun widget-group-match (widget vals)
  ;; Match if the components match.
  (and (listp vals)
       (let ((match (widget-group-match-inline widget vals)))
	 (and match (null (cdr match))))))

(defun widget-group-match-inline (widget vals)
  ;; Match if the components match.
  (let ((args (widget-get widget :args))
	argument answer found)
    (while args
      (setq argument (car args)
	    args     (cdr args))
      (if (setq answer (widget-match-inline argument vals))
	  (setq found (append found (car answer))
		vals (cdr answer))
	(setq vals nil
	      args nil)))
    (if answer
	(cons found vals))))

;;; The `visibility' Widget.

(define-widget 'visibility 'item
  "An indicator and manipulator for hidden items.

The following properties have special meanings for this widget:
:on-glyph  Image filename or spec to display when the item is visible.
:on        Text shown if the \"on\" image is nil or cannot be displayed.
:off-glyph Image filename or spec to display when the item is hidden.
:off       Text shown if the \"off\" image is nil cannot be displayed."
  :format "%[%v%]"
  :button-prefix ""
  :button-suffix ""
  :on-glyph "down"
  :on "Hide"
  :off-glyph "right"
  :off "Show"
  :value-create 'widget-toggle-value-create
  :action 'widget-toggle-action
  :match (lambda (_widget _value) t))

;;; The `documentation-link' Widget.
;;
;; This is a helper widget for `documentation-string'.

(define-widget 'documentation-link 'link
  "Link type used in documentation strings."
  :tab-order -1
  :help-echo "Describe this symbol"
  :action 'widget-documentation-link-action)

(defun widget-documentation-link-action (widget &optional _event)
  "Display documentation for WIDGET's value.  Ignore optional argument EVENT."
  (let* ((string (widget-get widget :value))
	 (symbol (intern string)))
    (cond
     ((and (fboundp symbol) (boundp symbol))
      ;; If there are two doc strings, give the user a way to pick one.
      (apropos (concat "\\`" (regexp-quote string) "\\'")))
     ((fboundp symbol)
      (describe-function symbol))
     ((facep symbol)
      (describe-face symbol))
     ((featurep symbol)
      (describe-package symbol))
     ((or (boundp symbol) (get symbol 'variable-documentation))
      (describe-variable symbol))
     (t
      (message "No documentation available for %s" symbol)))))

(defcustom widget-documentation-links t
  "Add hyperlinks to documentation strings when non-nil."
  :type 'boolean
  :group 'widget-documentation)

(defcustom widget-documentation-link-regexp "['`‘]\\([^\n `'‘’]+\\)['’]"
  "Regexp for matching potential links in documentation strings.
The first group should be the link itself."
  :type 'regexp
  :group 'widget-documentation)

(defcustom widget-documentation-link-p 'intern-soft
  "Predicate used to test if a string is useful as a link.
The value should be a function.  The function will be called with one
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
      (widget-put widget :buttons buttons))))

;;; The `documentation-string' Widget.

(define-widget 'documentation-string 'item
  "A documentation string."
  :format "%v"
  :action 'widget-documentation-string-action
  :value-create 'widget-documentation-string-value-create
  :visibility-widget 'visibility)

(defun widget-documentation-string-value-create (widget)
  ;; Insert documentation string.
  (let ((doc (substitute-command-keys (widget-value widget)))
	(indent (widget-get widget :indent))
	(shown (widget-get (widget-get widget :parent) :documentation-shown))
	(start (point)))
    (if (string-match "\n" doc)
	(let ((before (substring doc 0 (match-beginning 0)))
	      (after (substring doc (match-end 0)))
	      button end)
	  (widget-documentation-string-indent-to indent)
	  (insert before ?\s)
	  (widget-documentation-link-add widget start (point))
	  (setq button
		(widget-create-child-and-convert
		 widget (widget-get widget :visibility-widget)
		 :help-echo "Show or hide rest of the documentation."
		 :on "Hide"
		 :off "More"
		 :always-active t
		 :action 'widget-parent-action
		 shown))
	  (when shown
	    (insert ?\n)
	    (setq start (point))
	    (when (and indent (not (zerop indent)))
	      (insert-char ?\s indent))
	    (insert after)
	    (setq end (point))
	    (widget-documentation-link-add widget start end)
	    ;; Indent the subsequent lines.
	    (when (and indent (> indent 0))
	      (save-excursion
		(save-restriction
		  (narrow-to-region start end)
		  (goto-char (point-min))
		  (while (search-forward "\n" nil t)
		    (widget-documentation-string-indent-to indent))))))
	  (widget-put widget :buttons (list button)))
      (widget-documentation-string-indent-to indent)
      (insert doc)
      (widget-documentation-link-add widget start (point))))
  (insert ?\n))

(defun widget-documentation-string-indent-to (col)
  (when (and (numberp col)
	     (> col 0))
    (let ((opoint (point)))
      (indent-to col)
      (put-text-property opoint (point)
      			 'display `(space :align-to ,col)))))

(defun widget-documentation-string-action (widget &rest _ignore)
  ;; Toggle documentation.
  (let ((parent (widget-get widget :parent)))
    (widget-put parent :documentation-shown
		(not (widget-get parent :documentation-shown))))
  ;; Redraw.
  (widget-value-set widget (widget-value widget)))

(defun widget-add-documentation-string-button (widget &rest args)
  "Insert a new `documentation-string' widget based on WIDGET.
The new widget becomes a child of WIDGET, and is also added to
its `:buttons' list.  The documentation string is found from
WIDGET using the function `widget-docstring'.
Optional ARGS specifies additional keyword arguments for the
`documentation-string' widget."
  (let ((doc (widget-docstring widget))
	(indent (widget-get widget :indent))
	(doc-indent (widget-get widget :documentation-indent)))
    (when doc
      (and (eq (preceding-char) ?\n)
	   indent
	   (insert-char ?\s indent))
      (unless (or (numberp doc-indent) (null doc-indent))
	(setq doc-indent 0))
      (widget-put widget :buttons
		  (cons (apply #'widget-create-child-and-convert
			       widget 'documentation-string
			       :indent doc-indent
			       (nconc args (list doc)))
			(widget-get widget :buttons))))))

;;; The Sexp Widgets.

(define-widget 'const 'item
  "An immutable sexp."
  :prompt-value 'widget-const-prompt-value
  :format "%t\n%d")

(defun widget-const-prompt-value (widget _prompt _value _unbound)
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
  :format "%t\n"
  :value 'other)

(defvar widget-string-prompt-value-history nil
  "History of input to `widget-string-prompt-value'.")

(define-widget 'string 'editable-field
  "A string."
  :tag "String"
  :format "%{%t%}: %v"
  :complete (lambda (widget)
              (require 'ispell)
              (let ((start (save-excursion (nth 1 (ispell-get-word nil)))))
                (if (< start (widget-field-start widget))
                    (message "No word to complete inside field")
                  (ispell-complete-word))))
  :prompt-history 'widget-string-prompt-value-history)

(define-widget 'regexp 'string
  "A regular expression."
  :match 'widget-regexp-match
  :validate 'widget-regexp-validate
  ;; Doesn't work well with terminating newline.
  ;; :value-face 'widget-single-line-field
  :tag "Regexp")

(defun widget-regexp-match (_widget value)
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
It reads a file name from an editable text field."
  :completions (completion-table-case-fold
                #'completion-file-name-table
                (not read-file-name-completion-ignore-case))
  :match (lambda (widget value)
           (and (stringp value)
                (or (not (widget-get widget :must-match))
                    (file-exists-p value))))
  :validate (lambda (widget)
              (let ((value (widget-value widget)))
                (unless (widget-apply widget :match value)
                  (widget-put widget
                              :error (format "File %s does not exist" value))
                  widget)))
  :prompt-value 'widget-file-prompt-value
  :format "%{%t%}: %v"
  ;; Doesn't work well with terminating newline.
  ;; :value-face 'widget-single-line-field
  :tag "File")

(defun widget-file-prompt-value (widget prompt value unbound)
  ;; Read file from minibuffer.
  (abbreviate-file-name
   (if unbound
       (read-file-name prompt)
     (let ((dir (file-name-directory value))
	   (file (file-name-nondirectory value))
	   (must-match (widget-get widget :must-match)))
       (read-file-name (format-prompt prompt value) dir nil must-match file)))))

;;(defun widget-file-action (widget &optional event)
;;  ;; Read a file name from the minibuffer.
;;  (let* ((value (widget-value widget))
;;	 (dir (file-name-directory value))
;;	 (file (file-name-nondirectory value))
;;	 (menu-tag (widget-apply widget :menu-tag-get))
;;	 (must-match (widget-get widget :must-match))
;;	 (answer (read-file-name (format-prompt menu-tag value)
;;				 dir nil must-match file)))
;;    (widget-value-set widget (abbreviate-file-name answer))
;;    (widget-setup)
;;    (widget-apply widget :notify widget event)))

;; Fixme: use file-name-as-directory.
(define-widget 'directory 'file
  "A directory widget.
It reads a directory name from an editable text field."
  :completions (apply-partially #'completion-table-with-predicate
                                (completion-table-case-fold
                                 #'completion-file-name-table
                                 (not read-file-name-completion-ignore-case))
                                #'directory-name-p 'strict)
  :tag "Directory")

(defvar widget-symbol-prompt-value-history nil
  "History of input to `widget-symbol-prompt-value'.")

(define-widget 'symbol 'editable-field
  "A Lisp symbol."
  :value nil
  :tag "Symbol"
  :format "%{%t%}: %v"
  :match (lambda (_widget value) (symbolp value))
  :completions obarray
  :prompt-internal 'widget-symbol-prompt-internal
  :prompt-match 'symbolp
  :prompt-history 'widget-symbol-prompt-value-history
  :value-to-internal (lambda (_widget value)
		       (if (symbolp value)
			   (symbol-name value)
			 value))
  :value-to-external (lambda (_widget value)
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

(define-widget 'function 'restricted-sexp
  "A Lisp function."
  :completions (apply-partially #'completion-table-with-predicate
                                obarray #'fboundp 'strict)
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
  :completions (apply-partially #'completion-table-with-predicate
                                obarray #'boundp 'strict)
  :tag "Variable")

(define-widget 'coding-system 'symbol
  "A MULE coding-system."
  :format "%{%t%}: %v"
  :tag "Coding system"
  :base-only nil
  :prompt-history 'coding-system-value-history
  :prompt-value 'widget-coding-system-prompt-value
  :action 'widget-coding-system-action
  :completions (apply-partially #'completion-table-with-predicate
                                obarray #'coding-system-p 'strict)
  :validate (lambda (widget)
	      (unless (coding-system-p (widget-value widget))
		(widget-put widget :error (format "Invalid coding system: %S"
						  (widget-value widget)))
		widget))
  :value 'undecided
  :prompt-match 'coding-system-p)

(defun widget-coding-system-prompt-value (widget prompt value _unbound)
  "Read coding-system from minibuffer."
  (if (widget-get widget :base-only)
      (intern
       (completing-read (format-prompt prompt value)
			(mapcar #'list (coding-system-list t)) nil nil nil
			coding-system-history))
      (read-coding-system (format-prompt prompt value) value)))

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

;;; I'm not sure about what this is good for?  KFS.
(defvar widget-key-sequence-prompt-value-history nil
  "History of input to `widget-key-sequence-prompt-value'.")

(defvar widget-key-sequence-default-value [ignore]
  "Default value for an empty key sequence.")

(defvar-keymap widget-key-sequence-map
  :parent widget-field-keymap
  "C-q" #'widget-key-sequence-read-event)

(define-widget 'key-sequence 'restricted-sexp
  "A key sequence.  This is obsolete; use the `key' type instead."
  :prompt-value 'widget-field-prompt-value
  :prompt-internal 'widget-symbol-prompt-internal
; :prompt-match 'fboundp   ;; What was this good for?  KFS
  :prompt-history 'widget-key-sequence-prompt-value-history
  :action 'widget-field-action
  :match-alternatives '(stringp vectorp)
  :format "%{%t%}: %v"
  :validate 'widget-key-sequence-validate
  :value-to-internal 'widget-key-sequence-value-to-internal
  :value-to-external 'widget-key-sequence-value-to-external
  :value widget-key-sequence-default-value
  :keymap widget-key-sequence-map
  :help-echo "C-q: insert KEY, EVENT, or CODE; RET: enter value"
  :tag "Key sequence")

;; FIXME: Consider combining this with help--read-key-sequence which
;; can also read double and triple mouse events.
(defun widget-key-sequence-read-event (ev)
  (interactive (list
		(let ((inhibit-quit t) quit-flag)
		  (read-key "Insert KEY, EVENT, or CODE: " t))))
  (let ((ev2 (and (memq 'down (event-modifiers ev))
		  (read-key nil t)))
	(tr (and (keymapp local-function-key-map)
		 (lookup-key local-function-key-map (vector ev)))))
    (when (and (integerp ev)
	       (or (and (<= ?0 ev) (< ev (+ ?0 (min 10 read-quoted-char-radix))))
		   (and (<= ?a (downcase ev))
			(< (downcase ev) (+ ?a -10 (min 36 read-quoted-char-radix))))))
      (setq unread-command-events (cons ev unread-command-events)
	    ev (read-quoted-char (format "Enter code (radix %d)" read-quoted-char-radix))
	    tr nil)
      (if (and (integerp ev) (not (characterp ev)))
	  (insert (char-to-string ev))))  ;; throw invalid char error
    (setq ev (key-description (list ev)))
    (when (arrayp tr)
      (setq tr (key-description (list (aref tr 0))))
      (if (y-or-n-p (format "Key %s is translated to %s -- use %s? " ev tr tr))
	  (setq ev tr ev2 nil)))
    (insert (if (= (char-before) ?\s)  "" " ") ev " ")
    (if ev2
	(insert (key-description (list ev2)) " "))))

(defun widget-key-sequence-validate (widget)
  (unless (or (stringp (widget-value widget))
	      (vectorp (widget-value widget)))
    (widget-put widget :error (format "Invalid key sequence: %S"
				      (widget-value widget)))
    widget))

(defun widget-key-sequence-value-to-internal (widget value)
  (if (widget-apply widget :match value)
      (if (equal value widget-key-sequence-default-value)
	  ""
	(key-description value))
    value))

(defun widget-key-sequence-value-to-external (_widget value)
  (if (stringp value)
      (if (string-match "\\`[[:space:]]*\\'" value)
	  widget-key-sequence-default-value
	(key-parse value))
    value))


(defvar widget-key-prompt-value-history nil
  "History of input to `widget-key-prompt-value'.")

(define-widget 'key 'editable-field
  "A key sequence."
  :prompt-value 'widget-field-prompt-value
  :match #'widget-key-valid-p
  :format "%{%t%}: %v"
  :validate 'widget-key-validate
  :keymap widget-key-sequence-map
  :help-echo "C-q: insert KEY, EVENT, or CODE; RET: enter value"
  :tag "Key")

(defun widget-key-valid-p (_widget value)
  "Non-nil if VALUE is a valid value for the key widget WIDGET."
  (key-valid-p value))

(defun widget-key-validate (widget)
  (unless (and (stringp (widget-value widget))
               (key-valid-p (widget-value widget)))
    (widget-put widget :error (format "Invalid key: %S"
                                      (widget-value widget)))
    widget))


(define-widget 'sexp 'editable-field
  "An arbitrary Lisp expression."
  :tag "Lisp expression"
  :format "%{%t%}: %v"
  :value nil
  :validate 'widget-sexp-validate
  :match (lambda (_widget _value) t)
  :value-to-internal 'widget-sexp-value-to-internal
  :value-to-external (lambda (_widget value) (read value))
  :prompt-history 'widget-sexp-prompt-value-history
  :prompt-value 'widget-sexp-prompt-value)

(defun widget-sexp-value-to-internal (_widget value)
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
      (condition-case data ;Note: We get a spurious byte-compile warning here.
	  (progn
	    ;; Avoid a confusing end-of-file error.
	    (skip-syntax-forward "-")
	    (if (eobp)
		(setq err "Empty sexp -- use nil?")
	      (unless (widget-apply widget :match (read (current-buffer)))
		(setq err (widget-get widget :type-error))))
	    ;; Allow whitespace after expression.
	    (skip-syntax-forward "-")
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
                           (widget-sexp-value-to-internal widget value)
                         value))
  :value-to-external (lambda (widget value)
                       ;; We expect VALUE to be a string, so we can convert it
                       ;; into the external format just by `read'ing it.
                       ;; But for a restricted-sexp widget with a bad default
                       ;; value, we might end up calling read with a nil
                       ;; argument, resulting in an undesired prompt to the
                       ;; user.  A bad default value is not always a big
                       ;; problem, but might end up in a messed up buffer,
                       ;; so display a warning here.  (Bug#25152)
                       (unless (stringp value)
                         (display-warning
                          'widget-bad-default-value
                          (format-message
                           "\nA widget of type %S has a bad default value.
value: %S
match function: %S
match-alternatives: %S"
                           (widget-type widget)
                           value
                           (widget-get widget :match)
                           (widget-get widget :match-alternatives))
                          :warning)
                         ;; Make sure we will `read' a string.
                         (setq value (prin1-to-string value)))
                       (if (string-empty-p value)
                           value
                       (read value))))

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

(define-widget 'natnum 'restricted-sexp
  "A nonnegative integer."
  :tag "Integer (positive or zero)"
  :value 0
  :type-error "This field should contain a nonnegative integer"
  :match-alternatives '(natnump))

(define-widget 'number 'restricted-sexp
  "A number (floating point or integer)."
  :tag "Number"
  :value 0.0
  :type-error "This field should contain a number (floating point or integer)"
  :match-alternatives '(numberp))

(define-widget 'float 'restricted-sexp
  "A floating point number."
  :tag "Floating point number"
  :value 0.0
  :type-error "This field should contain a floating point number"
  :match-alternatives '(floatp))

(define-widget 'character 'editable-field
  "A character."
  :tag "Character"
  :value 0
  :size 1
  :format "%{%t%}: %v\n"
  :valid-regexp "\\`\\(.\\|\n\\)\\'"
  :error "This field should contain a single character"
  :value-get (lambda (w) (widget-field-value-get w t))
  :value-to-internal (lambda (_widget value)
		       (if (stringp value)
			   value
                         (let ((disp
                                (widget-character--change-character-display
                                 value)))
                           (if disp
                               (propertize (char-to-string value) 'display disp)
                             (char-to-string value)))))
  :value-to-external (lambda (_widget value)
		       (if (stringp value)
			   (aref value 0)
			 value))
  :match (lambda (_widget value)
	   (characterp value))
  :notify #'widget-character-notify)

;; Only some escape sequences, not all of them.  (Bug#15925)
(defvar widget-character--escape-sequences-alist
  '((?\t . ?t)
    (?\n . ?n)
    (?\s . ?s))
  "Alist that associates escape sequences to a character.
Each element has the form (ESCAPE-SEQUENCE . CHARACTER).

The character widget uses this alist to display the
non-printable character represented by ESCAPE-SEQUENCE as \\CHARACTER,
since that makes it easier to see what's in the widget.")

(defun widget-character--change-character-display (c)
  "Return a string to represent the character C, or nil.

The character widget represents some characters (e.g., the newline character
or the tab character) specially, to make it easier for the user to see what's
in it.  For those characters, return a string to display that character in a
more user-friendly way.

For the caller, nil should mean that it is good enough to use the return value
of `char-to-string' for the representation of C."
  (let ((char (alist-get c widget-character--escape-sequences-alist)))
    (and char (propertize (format "\\%c" char) 'face 'escape-glyph))))

(defun widget-character-notify (widget child &optional event)
  "Notify function for the character widget.

This function allows the widget character to better display some characters,
like the newline character or the tab character."
  (when (eq (car-safe event) 'after-change)
    (let* ((start (nth 1 event))
           (end (nth 2 event))
           str)
      (if (eql start end)
          (when (char-equal (widget-value widget) ?\s)
            ;; The character widget is not really empty:
            ;; its value is a single space character.
            ;; We need to propertize it again, if it became empty for a while.
            (let ((ov (widget-get widget :field-overlay)))
              (put-text-property
               (overlay-start ov) (overlay-end ov)
               'display (widget-character--change-character-display ?\s))))
        (setq str (buffer-substring-no-properties start end))
        ;; This assumes the user enters one character at a time,
        ;; and does nothing crazy, like yanking a long string.
        (let ((disp (widget-character--change-character-display (aref str 0))))
          (when disp
            (put-text-property start end 'display disp))))))
  (widget-default-notify widget child event))

(define-widget 'list 'group
  "A Lisp list."
  :tag "List"
  :default-get #'widget-list-default-get
  :format "%{%t%}:\n%v")

(defun widget-list-default-get (widget)
  "Return the default external value for a list WIDGET.

The default value is the one stored in the :value property, even if it is nil,
or a list with the default value of each component of the list WIDGET."
  (widget-apply widget :value-to-external
                (if (widget-member widget :value)
                    (widget-get widget :value)
                  (widget-group-default-get widget))))

(define-widget 'vector 'group
  "A Lisp vector."
  :tag "Vector"
  :format "%{%t%}:\n%v"
  :match 'widget-vector-match
  :value-to-internal (lambda (_widget value) (append value nil))
  :value-to-external (lambda (_widget value) (apply #'vector value)))

(defun widget-vector-match (widget value)
  (and (vectorp value)
       (widget-group-match widget
			   (widget-apply widget :value-to-internal value))))

(define-widget 'cons 'group
  "A cons-cell."
  :tag "Cons-cell"
  :format "%{%t%}:\n%v"
  :match 'widget-cons-match
  :value-to-internal (lambda (_widget value)
		       (list (car value) (cdr value)))
  :value-to-external (lambda (_widget value)
		       (apply #'cons value)))

(defun widget-cons-match (widget value)
  (and (consp value)
       (widget-group-match widget
			   (widget-apply widget :value-to-internal value))))

(defun widget-single-or-list-to-internal (widget val)
  (if (listp val) val
    (cons val (make-list (1- (length (widget-get widget :args))) nil))))

(define-widget 'single-or-list 'group
  "Either a single value (`nlistp') or a list of values (`listp').

If the initial value is `nlistp', the first child widget gets
that value and the other children get nil.

If the first child's value is `nlistp' and the other children are
nil, then `widget-value' just returns the first child's value."
  ;; The internal value is always a list; only :value-to-internal and
  ;; :match ever get called with the external value, which might be
  ;; `nlistp'.
  :value-to-external (lambda (_ val)
                       (if (and (nlistp (car val))
                                (cl-every #'null (cdr val)))
                           (car val) val))
  :value-to-internal #'widget-single-or-list-to-internal
  :match (lambda (widget val)
           (widget-group-match widget (widget-single-or-list-to-internal widget val))))


;;; The `lazy' Widget.
;;
;; Recursive datatypes.

(define-widget 'lazy 'default
  "Base widget for recursive data structures.

The `lazy' widget will, when instantiated, contain a single inferior
widget, of the widget type specified by the :type parameter.  The
value of the `lazy' widget is the same as the value of the inferior
widget.  When deriving a new widget from the `lazy' widget, the :type
parameter is allowed to refer to the widget currently being defined,
thus allowing recursive data structures to be described.

The :type parameter takes the same arguments as the defcustom
parameter with the same name.

Most composite widgets, i.e. widgets containing other widgets, do
not allow recursion.  That is, when you define a new widget type, none
of the inferior widgets may be of the same type you are currently
defining.

In Lisp, however, it is custom to define data structures in terms of
themselves.  A list, for example, is defined as either nil, or a cons
cell whose cdr itself is a list.  The obvious way to translate this
into a widget type would be

  (define-widget \\='my-list \\='choice
    \"A list of sexps.\"
    :tag \"Sexp list\"
    :args \\='((const nil) (cons :value (nil) sexp my-list)))

Here we attempt to define my-list as a choice of either the constant
nil, or a cons-cell containing a sexp and my-lisp.  This will not work
because the `choice' widget does not allow recursion.

Using the `lazy' widget you can overcome this problem, as in this
example:

  (define-widget \\='sexp-list \\='lazy
    \"A list of sexps.\"
    :tag \"Sexp list\"
    :type \\='(choice (const nil) (cons :value (nil) sexp sexp-list)))"
  :format "%{%t%}: %v"
  ;; We don't convert :type because we want to allow recursive
  ;; data structures.  This is slow, so we should not create speed
  ;; critical widgets by deriving from this.
  :convert-widget 'widget-value-convert-widget
  :value-create 'widget-type-value-create
  :value-get 'widget-child-value-get
  :value-inline 'widget-child-value-inline
  :default-get 'widget-type-default-get
  :match 'widget-type-match
  :validate 'widget-child-validate)


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
				     ,@(mapcar #'widget-plist-convert-option
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
	  (setq key-type (if (listp key) key `(const ,key))))
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
  :default-get #'widget-alist-default-get
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
				     ,@(mapcar #'widget-alist-convert-option
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
	  (setq key-type (if (listp key) key `(const ,key))))
      (setq key-type `(const ,option)
	    value-type widget-alist-value-type))
    `(cons :format "Key: %v" ,key-type ,value-type)))

(defun widget-alist-default-get (widget)
  "Return the default value for WIDGET, an alist widget.

The default value may be one of:
- The one stored in the :value property, even if it is nil.
- If WIDGET has options available, an alist consisting of the
default values for each option.
- nil, otherwise."
  (widget-apply widget :value-to-external
                (cond ((widget-member widget :value)
                       (widget-get widget :value))
                      ((widget-get widget :options)
                       (mapcar #'widget-default-get
                               ;; Last one is the editable-list part, and
                               ;; we don't want those showing up as
                               ;; part of the default value.  (Bug#63290)
                               (butlast (widget-get widget :args))))
                      (t nil))))

(define-widget 'choice 'menu-choice
  "A union of several sexp types.

If one of the choices of a choice widget has an :inline t property,
then the choice widget can act as an inline widget on its own if the
current choice is inline."
  :tag "Choice"
  :format "%{%t%}: %[Value Menu%] %v"
  :button-prefix 'widget-push-button-prefix
  :button-suffix 'widget-push-button-suffix
  :prompt-value 'widget-choice-prompt-value
  :inline-bubbles-p #'widget-choice-inline-bubbles-p)

(defun widget-choice-prompt-value (widget prompt value _unbound)
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
                ((and widget-choice-toggle
                      (= (length args) 2)
		      (memq old args))
		 (nth (if (eq old (nth 0 args)) 1 0)
		      args))
		(t
		 (dolist (current args)
		   (push (cons (widget-apply current :menu-tag-get)
			       current)
			 choices))
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

(defun widget-choice-inline-bubbles-p (widget)
  "Non-nil if the choice WIDGET has at least one choice that is inline.
This is used when matching values, because a choice widget needs to
match a value inline rather than just match it if at least one of its choices
is inline."
  (let ((args (widget-get widget :args))
        cur found)
    (while (and args (not found))
      (setq cur (car args)
            args (cdr args)
            found (widget-get cur :inline)))
    found))


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
  :match (lambda (_widget value) (booleanp value))
  :on "on (non-nil)"
  :off "off (nil)")

(defun widget-boolean-prompt-value (_widget prompt _value _unbound)
  ;; Toggle a boolean.
  ;; Say what "y" means.  A la
  ;; "Set customized value for bar to true: (y or n)"
  (y-or-n-p (concat (replace-regexp-in-string ": ?\\'" "" prompt)
                    " true: ")))

;;; The `color' Widget.

(define-widget 'color 'editable-field
  "Choose a color name (with sample)."
  :format "%{%t%}: %v (%{sample%})\n"
  :value-create 'widget-color-value-create
  :size (1+ (apply #'max 13 ; Longest RGB hex string.
                   (mapcar #'length (defined-colors))))
  :tag "Color"
  :value "black"
  :completions (defined-colors)
  :sample-face-get 'widget-color-sample-face-get
  :notify 'widget-color-notify
  :match #'widget-color-match
  :validate #'widget-color-validate
  :action 'widget-color-action)

(defun widget-color-value-create (widget)
  (widget-field-value-create widget)
  (widget-insert " ")
  (widget-create-child-and-convert
   widget 'push-button
   :tag " Choose " :action 'widget-color--choose-action)
  (widget-insert " "))

(declare-function list-colors-display "facemenu")

(defun widget-color--choose-action (widget &optional _event)
  (require 'facemenu)
  (list-colors-display
   nil nil
   (let ((cbuf (current-buffer))
         (wp (widget-get widget :parent)))
     (lambda (color)
       (when (buffer-live-p cbuf)
	 (widget-value-set wp color)
	 (let* ((buf (get-buffer "*Colors*"))
	        (win (get-buffer-window buf 0)))
	   (if win
	       (quit-window nil win)
	     (bury-buffer buf)))
	 (pop-to-buffer cbuf))))))

(defun widget-color-sample-face-get (widget)
  (let* ((value (condition-case nil
		    (widget-value widget)
		  (error (widget-get widget :value)))))
    (if (color-defined-p value)
	(list (cons 'foreground-color value))
      'default)))

(declare-function facemenu-read-color "facemenu")

(defun widget-color-action (widget &optional event)
  "Prompt for a color."
  (require 'facemenu)
  (let* ((tag (widget-apply widget :menu-tag-get))
	 (prompt (concat tag ": "))
	 (answer (facemenu-read-color prompt)))
    (unless (zerop (length answer))
      (widget-value-set widget answer)
      (widget-setup)
      (widget-apply widget :notify widget event))))

(defun widget-color-notify (widget child &optional event)
  "Update the sample, and notify the parent."
  (overlay-put (widget-get widget :sample-overlay)
	       'face (widget-apply widget :sample-face-get))
  (widget-default-notify widget child event))

(defun widget-color-match (_widget value)
  "Non-nil if VALUE is a defined color or a RGB hex string."
  (and (stringp value)
       (or (color-defined-p value)
           (string-match-p "^#\\(?:[[:xdigit:]]\\{3\\}\\)\\{1,4\\}$" value))))

(defun widget-color-validate (widget)
  "Check that WIDGET's value is a valid color."
  (let ((value (widget-value widget)))
    (unless (widget-color-match widget value)
      (widget-put widget :error (format "Invalid color: %S" value))
      widget)))

;;; The Help Echo

(defun widget-echo-help (pos)
  "Display help-echo text for widget at POS."
  (let* ((widget (widget-at pos))
	 (help-echo (and widget (widget-get widget :help-echo))))
    (if (functionp help-echo)
	(setq help-echo (funcall help-echo widget)))
    (if help-echo (message "%s" (eval help-echo t)))))

(define-obsolete-function-alias 'widget-sublist #'seq-subseq "28.1")
(define-obsolete-function-alias 'widget-visibility-value-create
  #'widget-toggle-value-create "29.1")

;;; Buffer predicates.
(define-widget 'buffer-predicate 'lazy
  "A buffer predicate."
  :tag "Buffer predicate"
  :type '(choice (const :tag "All buffers" t)
                 (const :tag "No buffers" nil)
                 ;; FIXME: This should be expanded somehow.
                 sexp))

(provide 'wid-edit)

;;; wid-edit.el ends here
