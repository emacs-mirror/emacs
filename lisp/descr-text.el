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
  :type 'hook
  :group 'facemenu)

(defun describe-text-mode ()
  "Major mode for buffers created by `describe-char'.

\\{describe-text-mode-map}
Entry to this mode calls the value of `describe-text-mode-hook'
if that value is non-nil."
  (kill-all-local-variables)
  (setq major-mode 'describe-text-mode
	mode-name "Describe-Text")
  (use-local-map describe-text-mode-map)
  (widget-setup)
  (add-hook 'change-major-mode-hook 'font-lock-defontify nil t)
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

(defun describe-property-list (properties)
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
      (widget-insert (propertize (format "  %-20s " key)
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
  (save-excursion
    (with-output-to-temp-buffer "*Help*"
      (set-buffer standard-output)
      (widget-insert "Category " (format "%S" category) ":\n\n")
      (describe-property-list (symbol-plist category))
      (describe-text-mode)
      (goto-char (point-min)))))

;;;###autoload
(defun describe-text-properties (pos &optional output-buffer)
  "Describe widgets, buttons, overlays and text properties at POS.
Interactively, describe them for the character after point.
If optional second argument OUTPUT-BUFFER is non-nil,
insert the output into that buffer, and don't initialize or clear it
otherwise."
  (interactive "d")
  (if (>= pos (point-max))
      (error "No character follows specified position"))
  (if output-buffer
      (describe-text-properties-1 pos output-buffer)
    (if (not (or (text-properties-at pos) (overlays-at pos)))
	(message "This is plain text.")
      (let ((buffer (current-buffer)))
	(when (eq buffer (get-buffer "*Help*"))
	  (error "Can't do self inspection"))
	(save-excursion
	  (with-output-to-temp-buffer "*Help*"
	    (set-buffer standard-output)
	    (setq output-buffer (current-buffer))
	    (widget-insert "Text content at position " (format "%d" pos) ":\n\n")
	    (with-current-buffer buffer
	      (describe-text-properties-1 pos output-buffer))
	    (describe-text-mode)
	    (goto-char (point-min))))))))

(defun describe-text-properties-1 (pos output-buffer)
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
    (with-current-buffer output-buffer
      ;; Widgets
      (when (widgetp widget)
	(newline)
	(widget-insert (cond (wid-field "This is an editable text area")
			     (wid-button "This is an active area")
			     (wid-doc "This is documentation text")))
	(widget-insert " of a ")
	(describe-text-widget widget)
	(widget-insert ".\n\n"))
      ;; Buttons
      (when (and button (not (widgetp wid-button)))
	(newline)
	(widget-insert "Here is a " (format "%S" button-type)
		       " button labeled `" button-label "'.\n\n"))
      ;; Overlays
      (when overlays
	(newline)
	(if (eq (length overlays) 1)
	    (widget-insert "There is an overlay here:\n")
	  (widget-insert "There are " (format "%d" (length overlays))
			 " overlays here:\n"))
	(dolist (overlay overlays)
	  (widget-insert " From " (format "%d" (overlay-start overlay))
			 " to " (format "%d" (overlay-end overlay)) "\n")
	  (describe-property-list (overlay-properties overlay)))
	(widget-insert "\n"))
      ;; Text properties
      (when properties
	(newline)
	(widget-insert "There are text properties here:\n")
	(describe-property-list properties)))))

;;;###autoload
(defun describe-char (pos)
  "Describe the character after POS (interactively, the character after point).
The information includes character code, charset and code points in it,
syntax, category, how the character is encoded in a file,
character composition information (if relevant),
as well as widgets, buttons, overlays, and text properties."
  (interactive "d")
  (if (>= pos (point-max))
      (error "No character follows specified position"))
  (let* ((char (char-after pos))
	 (charset (char-charset char))
	 (buffer (current-buffer))
	 (composition (find-composition pos nil nil t))
	 (composed (if composition (buffer-substring (car composition)
						     (nth 1 composition))))
	 (multibyte-p enable-multibyte-characters)
	 item-list max-width)
    (if (eq charset 'unknown)
	(setq item-list
	      `(("character"
		 ,(format "%s (0%o, %d, 0x%x) -- invalid character code"
			  (if (< char 256)
			      (single-key-description char)
			    (char-to-string char))
			  char char char))))
      (setq item-list
	    `(("character"
	       ,(format "%s (0%o, %d, 0x%x)" (if (< char 256)
						 (single-key-description char)
					       (char-to-string char))
			char char char))
	      ("charset"
	       ,(symbol-name charset)
	       ,(format "(%s)" (charset-description charset)))
	      ("code point"
	       ,(let ((split (split-char char)))
		  (if (= (charset-dimension charset) 1)
		      (format "%d" (nth 1 split))
		    (format "%d %d" (nth 1 split) (nth 2 split)))))
	      ("syntax"
 	       ,(let ((syntax (syntax-after pos)))
		  (with-temp-buffer
		    (internal-describe-syntax-value syntax)
		    (buffer-string))))
	      ("category"
	       ,@(let ((category-set (char-category-set char)))
		   (if (not category-set)
		       '("-- none --")
		     (mapcar #'(lambda (x) (format "%c:%s  "
						   x (category-docstring x)))
			     (category-set-mnemonics category-set)))))
	      ,@(let ((props (aref char-code-property-table char))
		      ps)
		  (when props
		    (while props
		      (push (format "%s:" (pop props)) ps)
		      (push (format "%s;" (pop props)) ps))
		    (list (cons "Properties" (nreverse ps)))))
	      ("buffer code"
	       ,(encoded-string-description
		 (string-as-unibyte (char-to-string char)) nil))
	      ("file code"
	       ,@(let* ((coding buffer-file-coding-system)
			(encoded (encode-coding-char char coding)))
		   (if encoded
		       (list (encoded-string-description encoded coding)
			     (format "(encoded by coding system %S)" coding))
		     (list "not encodable by coding system"
			   (symbol-name coding)))))
	      ,@(if (or (memq 'mule-utf-8
			  (find-coding-systems-region pos (1+ pos)))
			(get-char-property pos 'untranslated-utf-8))
		    (let ((uc (or (get-char-property pos 'untranslated-utf-8)
				  (encode-char char 'ucs))))
		      (if uc
			  (list (list "Unicode"
				      (format "%04X" uc))))))
	      ,(if (display-graphic-p (selected-frame))
		   (list "font" (or (internal-char-font pos)
				    "-- none --"))
		 (list "terminal code"
		       (let* ((coding (terminal-coding-system))
			      (encoded (encode-coding-char char coding)))
			 (if encoded
			     (encoded-string-description encoded coding)
			   "not encodable")))))))
    (setq max-width (apply #'max (mapcar #'(lambda (x) (length (car x)))
					 item-list)))
    (when (eq (current-buffer) (get-buffer "*Help*"))
      (error "Can't do self inspection"))
    (with-output-to-temp-buffer "*Help*"
      (with-current-buffer standard-output
	(set-buffer-multibyte multibyte-p)
	(let ((formatter (format "%%%ds:" max-width)))
	  (dolist (elt item-list)
	    (insert (format formatter (car elt)))
	    (dolist (clm (cdr elt))
	      (when (>= (+ (current-column)
			   (or (string-match "\n" clm)
			       (string-width clm)) 1)
			(frame-width))
		(insert "\n")
		(indent-to (1+ max-width)))
	      (insert " " clm))
	    (insert "\n")))
	(when composition
	  (insert "\nComposed with the "
		  (cond
		   ((eq pos (car composition)) "following ")
		   ((eq (1+ pos) (cadr composition)) "preceding ")
		   (t ""))
		  "character(s) `"
		  (cond
		   ((eq pos (car composition)) (substring composed 1))
		   ((eq (1+ pos) (cadr composition)) (substring composed 0 -1))
		   (t (concat (substring composed 0 (- pos (car composition)))
			      "' and `"
			      (substring composed (- (1+ pos) (car composition))))))

		  "' to form `" composed "'")
	  (if (nth 3 composition)
	      (insert ".\n")
	    (insert "\nby the rule ("
		    (mapconcat (lambda (x)
				 (format (if (consp x) "%S" "?%c") x))
			       (nth 2 composition)
			       " ")
		    ").\n"
		    "See the variable `reference-point-alist' for "
		    "the meaning of the rule.\n")))

	(let ((output (current-buffer)))
	  (with-current-buffer buffer
	    (describe-text-properties pos output))
	  (describe-text-mode))))))

(provide 'descr-text)

;;; descr-text.el ends here
