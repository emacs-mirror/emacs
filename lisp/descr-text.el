;;; descr-text.el --- describe text mode

;; Copyright (c) 1994, 1995, 1996, 2001, 2002 Free Software Foundation, Inc.

;; Author: Boris Goldowsky <boris@gnu.org>
;; Keywords: faces

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

;;; Describe-Text Mode.

;;; Code:

(defun describe-text-done ()
  "Delete the current window or bury the current buffer."
  (interactive)
  (if (> (count-windows) 1)
      (delete-window)
    (bury-buffer)))

(defvar describe-text-mode-map 
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    map)
  "Keymap for `describe-text-mode'.")
  
(defcustom describe-text-mode-hook nil
  "List of hook functions ran by `describe-text-mode'."
  :type 'hook)

(defun describe-text-mode ()
  "Major mode for buffers created by `describe-text-at'.

\\{describe-text-mode-map}
Entry to this mode calls the value of `describe-text-mode-hook'
if that value is non-nil."
  (kill-all-local-variables)
  (setq major-mode 'describe-text-mode
	mode-name "Describe-Text")
  (use-local-map describe-text-mode-map)
  (widget-setup)
  (run-hooks 'describe-text-mode-hook))

;;; Describe-Text Utilities.

(defun describe-text-widget (widget)
  "Insert text to describe WIDGET in the current buffer."
  (widget-create 'link
		 :notify `(lambda (&rest ignore)
			    (widget-browse ',widget))
		 (format "%S" (if (symbolp widget) 
				  widget
				(car widget))))
  (widget-insert " ")
  (widget-create 'info-link :tag "widget" "(widget)Top"))

(defun describe-text-sexp (sexp)
  "Insert a short description of SEXP in the current buffer."
  (let ((pp (condition-case signal
		(pp-to-string sexp)
	      (error (prin1-to-string signal)))))
    (when (string-match "\n\\'" pp)
      (setq pp (substring pp 0 (1- (length pp)))))
    (if (cond ((string-match "\n" pp)
	       nil)
	      ((> (length pp) (- (window-width) (current-column)))
	       nil)
	      (t t))
	(widget-insert pp)
      (widget-create 'push-button
		     :tag "show"
		     :action (lambda (widget &optional event)
			       (with-output-to-temp-buffer
				   "*Pp Eval Output*"
				 (princ (widget-get widget :value))))
		     pp))))

(defun describe-text-properties (properties)
  "Insert a description of PROPERTIES in the current buffer.
PROPERTIES should be a list of overlay or text properties.
The `category' property is made into a widget button that call 
`describe-text-category' when pushed."
  ;; Sort the properties by the size of their value.
  (dolist (elt (sort (let ((ret nil)
			   (key nil)
			   (val nil)
			   (len nil))
		       (while properties
			 (setq key (pop properties)
			       val (pop properties)
			       len 0)
			 (unless (or (eq key 'category)
				     (widgetp val))
			   (setq val (pp-to-string val)
				 len (length val)))
			 (push (list key val len) ret))
		       ret)
		     (lambda (a b)
		       (< (nth 2 a)
			  (nth 2 b)))))
    (let ((key (nth 0 elt))
	  (value (nth 1 elt)))
      (widget-insert (propertize (format "  %-20s" key)
				 'font-lock-face 'italic))
      (cond ((eq key 'category)
	     (widget-create 'link 
			    :notify `(lambda (&rest ignore)
				       (describe-text-category ',value))
			    (format "%S" value)))
	    ((widgetp value)
	     (describe-text-widget value))
	    (t
	     (widget-insert value))))
    (widget-insert "\n")))

;;; Describe-Text Commands.

(defun describe-text-category (category)
  "Describe a text property category."
  (interactive "S")
  (when (get-buffer "*Text Category*")
    (kill-buffer "*Text Category*"))
  (save-excursion
    (with-output-to-temp-buffer "*Text Category*"
      (set-buffer "*Text Category*")
      (widget-insert "Category " (format "%S" category) ":\n\n")
      (describe-text-properties (symbol-plist category))
      (describe-text-mode)
      (goto-char (point-min)))))

;;;###autoload
(defun describe-text-at (pos)
  "Describe widgets, buttons, overlays and text properties at POS."
  (interactive "d")
  (when (eq (current-buffer) (get-buffer "*Text Description*"))
    (error "Can't do self inspection"))
  (let* ((properties (text-properties-at pos))
	 (overlays (overlays-at pos))
	 overlay
	 (wid-field (get-char-property pos 'field))
	 (wid-button (get-char-property pos 'button))
	 (wid-doc (get-char-property pos 'widget-doc))
	 ;; If button.el is not loaded, we have no buttons in the text.
	 (button (and (fboundp 'button-at) (button-at pos)))
	 (button-type (and button (button-type button)))
	 (button-label (and button (button-label button)))
	 (widget (or wid-field wid-button wid-doc)))
    (if (not (or properties overlays))
	(message "This is plain text.")
      (when (get-buffer "*Text Description*")
	(kill-buffer "*Text Description*"))
      (save-excursion
	(with-output-to-temp-buffer "*Text Description*"
	  (set-buffer "*Text Description*")
	  (widget-insert "Text content at position " (format "%d" pos) ":\n\n")
	  ;; Widgets
	  (when (widgetp widget)
	    (widget-insert (cond (wid-field "This is an editable text area")
				 (wid-button "This is an active area")
				 (wid-doc "This is documentation text")))
	    (widget-insert " of a ")
	    (describe-text-widget widget)
	    (widget-insert ".\n\n"))
	  ;; Buttons
	  (when (and button (not (widgetp wid-button)))
	    (widget-insert "Here is a " (format "%S" button-type) 
			   " button labeled `" button-label "'.\n\n"))
	  ;; Overlays
	  (when overlays
	    (if (eq (length overlays) 1)
		(widget-insert "There is an overlay here:\n")
	      (widget-insert "There are " (format "%d" (length overlays))
			     " overlays here:\n"))
	    (dolist (overlay overlays)
	      (widget-insert " From " (format "%d" (overlay-start overlay)) 
			     " to " (format "%d" (overlay-end overlay)) "\n")
	      (describe-text-properties (overlay-properties overlay)))
	    (widget-insert "\n"))
	  ;; Text properties
	  (when properties
	    (widget-insert "There are text properties here:\n")
	    (describe-text-properties properties))
	  (describe-text-mode)
	  (goto-char (point-min)))))))

(provide 'descr-text)

;;; descr-text.el ends here
