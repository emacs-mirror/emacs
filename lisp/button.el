;;; button.el --- Clickable buttons
;;
;; Copyright (C) 2001 Free Software Foundation, Inc.
;;
;; Author: Miles Bader <miles@gnu.org>
;; Keywords: extensions
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This package defines functions for inserting and manipulating
;; clickable buttons in Emacs buffers, such as might be used for help
;; hyperlinks, etc.
;;
;; In some ways it duplicates functionality also offered by the
;; `widget' package, but the button package has the advantage that it
;; is (1) much faster, (2) much smaller, and (3) much, much, simpler
;; (the code, that is, not the interface).
;;
;; Buttons can either use overlays, in which case the button is
;; represented by the overlay itself, or text-properties, in which case
;; the button is represented by a marker or buffer-position pointing
;; somewhere in the button.  In the latter case, no markers into the
;; buffer are retained, which is important for speed if there are are
;; extremely large numbers of buttons.
;;
;; Using `define-button-type' to define default properties for buttons
;; is not necessary, but it is is encouraged, since doing so makes the
;; resulting code clearer and more efficient.
;;

;;; Code:


;; Globals

(defface button '((t :underline t))
  "Default face used for buttons.")

;;;###autoload
(defvar button-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'push-button)
    (define-key map [mouse-2] 'push-button)
    map)
  "Keymap used by buttons.")

;;;###autoload
(defvar button-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\t] 'forward-button)
    (define-key map [backtab] 'backward-button)
    map)
  "Keymap useful for buffers containing buttons.
Mode-specific keymaps may want to use this as their parent keymap.")

;; Default properties for buttons
(put 'default-button 'face 'button)
(put 'default-button 'mouse-face 'highlight)
(put 'default-button 'keymap button-map)
(put 'default-button 'type 'button)
(put 'default-button 'action 'button-nop)
(put 'default-button 'help-echo "mouse-2, RET: Push this button")
;; Make overlay buttons go away if their underlying text is deleted.
(put 'default-button 'evaporate t)
;; Prevent insertions adjacent to the text-property buttons from
;; inheriting its properties.
(put 'default-button 'rear-nonsticky t)
;; Text property buttons don't have a `button' property of their own, so
;; they inherit this.
(put 'default-button 'button t)

;; This is the default button action.
(defun button-nop (button)
  "Do nothing to BUTTON."
  nil)


;; Button types (which can be used to hold default properties for buttons)

;;;###autoload
(defun define-button-type (name &rest properties)
  "Define a `button type' called NAME.
The remaining arguments form a sequence of PROPERTY VALUE pairs,
specifying properties to use as defaults for buttons with this type
\(a button's type may be set by giving it a `type' property when
creating the button)."
  ;; We use a different symbol than NAME (with `-button' appended, and
  ;; uninterned) to store the properties.  This is to avoid name
  ;; clashes, since many very general properties may be include in
  ;; PROPERTIES.
  (let ((catsym (make-symbol (concat (symbol-name name) "-button"))))
    ;; Provide a link so that it's easy to find the real symbol.
    (put name 'button-category-symbol catsym)
    ;; Initialize NAME's properties using the global defaults.
    (let ((default-props (symbol-plist 'default-button)))
      (while default-props
	(put catsym (pop default-props) (pop default-props))))
    ;; Add NAME as the `type' property, which will then be returned as
    ;; the type property of individual buttons.
    (put catsym 'type name)
    ;; Add the properties in PROPERTIES to the real symbol.
    (while properties
      (put catsym (pop properties) (pop properties)))
    name))

;; [this is an internal function]
(defsubst button-category-symbol (type)
  "Return the symbol used by button-type TYPE to store properties.
Buttons inherit them by setting their `category' property to that symbol."
  (or (get type 'button-category-symbol)
      (error "Unknown button type `%s'" type)))

(defun button-type-put (type prop val)
  "Set the button-type TYPE's PROP property to VAL."
  (put (button-category-symbol type) prop val))

(defun button-type-get (type prop)
  "Get the property of button-type TYPE named PROP."
  (get (button-category-symbol type) prop))


;; Button properties and other attributes

(defun button-start (button)
  "Return the position at which BUTTON starts."
  (if (overlayp button)
      (overlay-start button)
    ;; Must be a text-property button.
    (or (previous-single-property-change (1+ button) 'button)
	(point-min))))

(defun button-end (button)
  "Return the position at which BUTTON ends."
  (if (overlayp button)
      (overlay-end button)
    ;; Must be a text-property button.
    (or (next-single-property-change button 'button)
	(point-max))))

(defun button-get (button prop)
  "Get the property of button BUTTON named PROP."
  (if (overlayp button)
      (overlay-get button prop)
    ;; Must be a text-property button.
    (get-text-property button prop)))

(defun button-put (button prop val)
  "Set BUTTON's PROP property to VAL."
  ;; Treat some properties specially.
  (cond ((eq prop 'type)
	 ;; We translate a `type' property a `category' property, since
	 ;; that's what's actually used by overlays/text-properties for
	 ;; inheriting properties.
	 (setq prop 'category)
	 (setq val (button-category-symbol val)))
	((eq prop 'category)
	 ;; Disallow updating the `category' property directly.
	 (error "Button `category' property may not be set directly")))
  ;; Add the property.
  (if (overlayp button)
      (overlay-put button prop val)
    ;; Must be a text-property button.
    (put-text-property
     (or (previous-single-property-change (1+ button) 'button)
	 (point-min))
     (or (next-single-property-change button 'button)
	 (point-max))
     prop val)))

(defsubst button-activate (button)
  "Call BUTTON's action property."
  (funcall (button-get button 'action) button))

(defun button-label (button)
  "Return BUTTON's text label."
  (buffer-substring-no-properties (button-start button) (button-end button)))


;; Creating overlay buttons

;;;###autoload
(defun make-button (beg end &rest properties)
  "Make a button from BEG to END in the current buffer.
The remaining arguments form a sequence of PROPERTY VALUE pairs,
specifying properties to add to the button.  In particular, the `type'
property may be used to specify a button-type from which to inherit
other properties; see `define-button-type'.

Also see `make-text-button', `insert-button'."
  (let ((overlay (make-overlay beg end nil t nil)))
    (while properties
      (button-put overlay (pop properties) (pop properties)))
    ;; Put a pointer to the button in the overlay, so it's easy to get
    ;; when we don't actually have a reference to the overlay.
    (overlay-put overlay 'button overlay)
    ;; If the user didn't specify a type, use the default.
    (unless (overlay-get overlay 'category)
      (overlay-put overlay 'category 'default-button))
    ;; OVERLAY is the button, so return it
    overlay))

;;;###autoload
(defun insert-button (label &rest properties)
  "Insert a button with the label LABEL.
The remaining arguments form a sequence of PROPERTY VALUE pairs,
specifying properties to add to the button.  In particular, the `type'
property may be used to specify a button-type from which to inherit
other properties; see `define-button-type'.

Also see `insert-text-button', `make-button'."
  (apply #'make-button
	 (prog1 (point) (insert label))
	 (point)
	 properties))


;; Creating text-property buttons

;;;###autoload
(defun make-text-button (beg end &rest properties)
  "Make a button from BEG to END in the current buffer.
The remaining arguments form a sequence of PROPERTY VALUE pairs,
specifying properties to add to the button.  In particular, the `type'
property may be used to specify a button-type from which to inherit
other properties; see `define-button-type'.

This function is like `make-button', except that the button is actually
part of the text instead of being a property of the buffer.  Creating
large numbers of buttons can also be somewhat faster using
`make-text-button'.

Also see `insert-text-button'."
  (let (prop val)
    (while properties
      (setq prop (pop properties))
      (setq val (pop properties))
      ;; Note that all the following code is basically equivalent to
      ;; `button-put', but we can do it much more efficiently since we
      ;; already have BEG and END.
      (cond ((eq prop 'type)
	     ;; We translate a `type' property into a `category'
	     ;; property, since that's what's actually used by
	     ;; text-properties for inheritance.
	     (setq prop 'category)
	     (setq val (button-category-symbol val)))
	    ((eq prop 'category)
	     ;; Disallow setting the `category' property directly.
	     (error "Button `category' property may not be set directly")))
      ;; Add the property.
      (put-text-property beg end prop val)))
  ;; Return something that can be used to get at the button.
  beg)

;;;###autoload
(defun insert-text-button (label &rest properties)
  "Insert a button with the label LABEL.
The remaining arguments form a sequence of PROPERTY VALUE pairs,
specifying properties to add to the button.  In particular, the `type'
property may be used to specify a button-type from which to inherit
other properties; see `define-button-type'.

This function is like `insert-button', except that the button is
actually part of the text instead of being a property of the buffer.
Creating large numbers of buttons can also be somewhat faster using
`insert-text-button'.

Also see `make-text-button'."
  (apply #'make-text-button
	 (prog1 (point) (insert label))
	 (point)
	 properties))


;; Finding buttons in a buffer

(defun button-at (pos)
  "Return the button at position POS in the current buffer, or nil."
  (let ((button (get-char-property pos 'button)))
    (if (or (overlayp button) (null button))
	button
      ;; Must be a text-property button; return a marker pointing to it.
      (copy-marker pos t))))

(defun next-button (pos &optional n wrap count-current)
  "Return the Nth button after position POS in the current buffer.
If N is negative, return the Nth button before POS.
If no Nth button is found, return nil.
If WRAP is non-nil, the search wraps around at the end of the buffer.
If COUNT-CURRENT is non-nil, count any button at POS in the search,
 instead of starting at the next button."
  (when (null n)
    (setq n 1))
  (if (< n 0)
      ;; reverse direction
      (previous-button pos (- n) wrap)
    (unless count-current
      ;; Search for the next button boundary.
      (setq pos (next-single-char-property-change pos 'button)))
    (let ((button (button-at pos)))
      (cond ((and button (>= n 2))
	     ;; Found a button, but we want a different one; recurse.
	     (next-button (button-start button) (1- n) wrap))
	    (button
	     ;; This is the button we want.
	     button)
	    ((= pos (point-max))
	     ;; Failed to find a button going forwards, either wrap or
	     ;; return failure.
	     (and wrap (next-button (point-min) n nil t)))
	    (t
	     ;; We must have originally been on a button, and are now in
	     ;; the inter-button space.  Recurse to find a button.
	     (next-button pos n wrap))))))

(defun previous-button (pos &optional n wrap count-current)
  "Return the Nth button before position POS in the current buffer.
If N is negative, return the Nth button after POS.
If no Nth button is found, return nil.
If WRAP is non-nil, the search wraps around at the beginning of the buffer.
If COUNT-CURRENT is non-nil, count any button at POS in the search,
 instead of starting at the next button."
  (when (null n)
    (setq n 1))
  (if (< n 0)
      ;; reverse direction
      (next-button pos (- n) wrap)
    (unless count-current
      (setq pos (previous-single-char-property-change pos 'button)))
    (let ((button (and (> pos (point-min)) (button-at (1- pos)))))
      (cond ((and button (>= n 2))
	     ;; Found a button, but we want a different one; recurse.
	     (previous-button (button-start button) (1- n) wrap))
	    (button
	     ;; This is the button we want.
	     button)
	    ((= pos (point-min))
	     ;; Failed to find a button going backwards, either wrap
	     ;; or return failure.
	     (and wrap (previous-button (point-max) n nil t)))
	    (t
	     ;; We must have originally been on a button, and are now in
	     ;; the inter-button space.  Recurse to find a button.
	     (previous-button pos (max n 1) wrap))))))


;; User commands

(defun push-button (&optional pos)
  "Perform the action specified by a button at location POS.
POS may be either a buffer position or a mouse-event.
POS defaults to point, except when `push-button' is invoked
interactively as the result of a mouse-event, in which case, the
mouse event is used.
If there's no button at POS, do nothing and return nil, otherwise
return t."
  (interactive
   (list (if (integerp last-command-event) (point) last-command-event)))
  (if (and (not (integerp pos)) (eventp pos))
      ;; POS is a mouse event; switch to the proper window/buffer
      (let ((posn (event-start pos)))
	(with-current-buffer (window-buffer (posn-window posn))
	  (push-button (posn-point posn))))
    ;; POS is just normal position
    (let ((button (button-at (or pos (point)))))
      (if (not button)
	  nil
	(button-activate button)
	t))))

(defun forward-button (n &optional wrap display-message)
  "Move to the Nth next button, or Nth previous button if N is negative.
If WRAP is non-nil, moving past either end of the buffer continues from the
other end.
If DISPLAY-MESSAGE is non-nil, the button's help-echo string is displayed.
Returns the button found."
  (interactive "p\nd\nd")
  (let ((button (next-button (point) n wrap)))
    (if (null button)
	(error (if wrap "No buttons!" "No more buttons"))
      (goto-char (button-start button))
      (let ((msg (and display-message (button-get button 'help-echo))))
	(when msg
	  (message "%s" msg)))
      button)))

(defun backward-button (n &optional wrap display-message)
  "Move to the Nth previous button, or Nth next button if N is negative.
If WRAP is non-nil, moving past either end of the buffer continues from the
other end.
If DISPLAY-MESSAGE is non-nil, the button's help-echo string is displayed.
Returns the button found."
  (interactive "p\nd\nd")
  (forward-button (- n) wrap display-message))


(provide 'button)

;;; button.el ends here
