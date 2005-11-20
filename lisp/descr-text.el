;;; descr-text.el --- describe text mode

;; Copyright (C) 1994, 1995, 1996, 2001, 2002, 2003, 2004,
;;   2005 Free Software Foundation, Inc.

;; Author: Boris Goldowsky <boris@gnu.org>
;; Keywords: faces, i18n, Unicode, multilingual

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

;;; Commentary:

;;; Describe-Text Mode.

;;; Code:

(eval-when-compile (require 'button) (require 'quail))

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
  (run-mode-hooks 'describe-text-mode-hook))

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
The `category', `face' and `font-lock-face' properties are made
into widget buttons that call `describe-text-category' or
`describe-face' when pushed."
  ;; Sort the properties by the size of their value.
  (dolist (elt (sort (let (ret)
		       (while properties
			 (push (list (pop properties) (pop properties)) ret))
		       ret)
		     (lambda (a b) (string< (prin1-to-string (nth 0 a) t)
					    (prin1-to-string (nth 0 b) t)))))
    (let ((key (nth 0 elt))
	  (value (nth 1 elt)))
      (widget-insert (propertize (format "  %-20s " key)
				 'font-lock-face 'italic))
      (cond ((eq key 'category)
	     (widget-create 'link
			    :notify `(lambda (&rest ignore)
				       (describe-text-category ',value))
			    (format "%S" value)))
            ((memq key '(face font-lock-face mouse-face))
	     (widget-create 'link
			    :notify `(lambda (&rest ignore)
				       (describe-face ',value))
			    (format "%S" value)))
            ((widgetp value)
	     (describe-text-widget value))
	    (t
	     (describe-text-sexp value))))
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
      (let ((buffer (current-buffer))
	    (target-buffer "*Help*"))
	(when (eq buffer (get-buffer target-buffer))
	  (setq target-buffer "*Help-2*"))
	(save-excursion
	  (with-output-to-temp-buffer target-buffer
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

(defcustom describe-char-unicodedata-file nil
  "Location of Unicode data file.
This is the UnicodeData.txt file from the Unicode consortium, used for
diagnostics.  If it is non-nil `describe-char' will print data
looked up from it.  This facility is mostly of use to people doing
multilingual development.

This is a fairly large file, not typically present on GNU systems.  At
the time of writing it is at
<URL:http://www.unicode.org/Public/UNIDATA/UnicodeData.txt>."
  :group 'mule
  :version "22.1"
  :type '(choice (const :tag "None" nil)
		 file))

;; We could convert the unidata file into a Lispy form once-for-all
;; and distribute it for loading on demand.  It might be made more
;; space-efficient by splitting strings word-wise and replacing them
;; with lists of symbols interned in a private obarray, e.g.
;; "LATIN SMALL LETTER A" => '(LATIN SMALL LETTER A).

;; Fixme: Check whether this needs updating for Unicode 4.
(defun describe-char-unicode-data (char)
  "Return a list of Unicode data for unicode CHAR.
Each element is a list of a property description and the property value.
The list is null if CHAR isn't found in `describe-char-unicodedata-file'."
  (when describe-char-unicodedata-file
    (unless (file-exists-p describe-char-unicodedata-file)
      (error "`unicodedata-file' %s not found" describe-char-unicodedata-file))
    (with-current-buffer
	;; Find file in fundamental mode to avoid, e.g. flyspell turned
	;; on for .txt.  Don't use RAWFILE arg in case of DOS line endings.
	(let ((auto-mode-alist))
	  (find-file-noselect describe-char-unicodedata-file))
      (goto-char (point-min))
      (let ((hex (format "%04X" char))
	    found first last)
	(if (re-search-forward (concat "^" hex) nil t)
	    (setq found t)
	  ;; It's not listed explicitly.  Look for ranges, e.g. CJK
	  ;; ideographs, and check whether it's in one of them.
	  (while (and (re-search-forward "^\\([^;]+\\);[^;]+First>;" nil t)
		      (>= char (setq first
				     (string-to-number (match-string 1) 16)))
		      (progn
			(forward-line 1)
			(looking-at "^\\([^;]+\\);[^;]+Last>;")
			(> char
			   (setq last
				 (string-to-number (match-string 1) 16))))))
	  (if (and (>= char first)
		   (<= char last))
	      (setq found t)))
	(if found
	    (let ((fields (mapcar (lambda (elt)
				    (if (> (length elt) 0)
					elt))
				  (cdr (split-string
					(buffer-substring
					 (line-beginning-position)
					 (line-end-position))
					";")))))
	      ;; The length depends on whether the last field was empty.
	      (unless (or (= 13 (length fields))
			  (= 14 (length fields)))
		(error "Invalid contents in %s" describe-char-unicodedata-file))
	      ;; The field names and values lists are slightly
	      ;; modified from Mule-UCS unidata.el.
	      (list
	       (list "Name" (let ((name (nth 0 fields)))
			      ;; Check for <..., First>, <..., Last>
			      (if (string-match "\\`\\(<[^,]+\\)," name)
				  (concat (match-string 1 name) ">")
				name)))
	       (list "Category"
		     (cdr (assoc
			   (nth 1 fields)
			   '(("Lu" . "uppercase letter")
			     ("Ll" . "lowercase letter")
			     ("Lt" . "titlecase letter")
			     ("Mn" . "non-spacing mark")
			     ("Mc" . "spacing-combining mark")
			     ("Me" . "enclosing mark")
			     ("Nd" . "decimal digit")
			     ("Nl" . "letter number")
			     ("No" . "other number")
			     ("Zs" . "space separator")
			     ("Zl" . "line separator")
			     ("Zp" . "paragraph separator")
			     ("Cc" . "other control")
			     ("Cf" . "other format")
			     ("Cs" . "surrogate")
			     ("Co" . "private use")
			     ("Cn" . "not assigned")
			     ("Lm" . "modifier letter")
			     ("Lo" . "other letter")
			     ("Pc" . "connector punctuation")
			     ("Pd" . "dash punctuation")
			     ("Ps" . "open punctuation")
			     ("Pe" . "close punctuation")
			     ("Pi" . "initial-quotation punctuation")
			     ("Pf" . "final-quotation punctuation")
			     ("Po" . "other punctuation")
			     ("Sm" . "math symbol")
			     ("Sc" . "currency symbol")
			     ("Sk" . "modifier symbol")
			     ("So" . "other symbol")))))
	       (list "Combining class"
		     (cdr (assoc
			   (string-to-number (nth 2 fields))
			   '((0 . "Spacing")
			     (1 . "Overlays and interior")
			     (7 . "Nuktas")
			     (8 . "Hiragana/Katakana voicing marks")
			     (9 . "Viramas")
			     (10 . "Start of fixed position classes")
			     (199 . "End of fixed position classes")
			     (200 . "Below left attached")
			     (202 . "Below attached")
			     (204 . "Below right attached")
			     (208 . "Left attached (reordrant around \
single base character)")
			     (210 . "Right attached")
			     (212 . "Above left attached")
			     (214 . "Above attached")
			     (216 . "Above right attached")
			     (218 . "Below left")
			     (220 . "Below")
			     (222 . "Below right")
			     (224 . "Left (reordrant around single base \
character)")
			     (226 . "Right")
			     (228 . "Above left")
			     (230 . "Above")
			     (232 . "Above right")
			     (233 . "Double below")
			     (234 . "Double above")
			     (240 . "Below (iota subscript)")))))
	       (list "Bidi category"
		     (cdr (assoc
			   (nth 3 fields)
			   '(("L" . "Left-to-Right")
			     ("LRE" . "Left-to-Right Embedding")
			     ("LRO" . "Left-to-Right Override")
			     ("R" . "Right-to-Left")
			     ("AL" . "Right-to-Left Arabic")
			     ("RLE" . "Right-to-Left Embedding")
			     ("RLO" . "Right-to-Left Override")
			     ("PDF" . "Pop Directional Format")
			     ("EN" . "European Number")
			     ("ES" . "European Number Separator")
			     ("ET" . "European Number Terminator")
			     ("AN" . "Arabic Number")
			     ("CS" . "Common Number Separator")
			     ("NSM" . "Non-Spacing Mark")
			     ("BN" . "Boundary Neutral")
			     ("B" . "Paragraph Separator")
			     ("S" . "Segment Separator")
			     ("WS" . "Whitespace")
			     ("ON" . "Other Neutrals")))))
	       (list
		"Decomposition"
		(if (nth 4 fields)
		    (let* ((parts (split-string (nth 4 fields)))
			   (info (car parts)))
		      (if (string-match "\\`<\\(.+\\)>\\'" info)
			  (setq info (match-string 1 info))
			(setq info nil))
		      (if info (setq parts (cdr parts)))
		      ;; Maybe printing ? for unrepresentable unicodes
		      ;; here and below should be changed?
		      (setq parts (mapconcat
				   (lambda (arg)
				     (string (or (decode-char
						  'ucs
						  (string-to-number arg 16))
						 ??)))
				   parts " "))
		      (concat info parts))))
	       (list "Decimal digit value"
		     (nth 5 fields))
	       (list "Digit value"
		     (nth 6 fields))
	       (list "Numeric value"
		     (nth 7 fields))
	       (list "Mirrored"
		     (if (equal "Y" (nth 8 fields))
			 "yes"))
	       (list "Old name" (nth 9 fields))
	       (list "ISO 10646 comment" (nth 10 fields))
	       (list "Uppercase" (and (nth 11 fields)
				      (string (or (decode-char
						   'ucs
						   (string-to-number
						    (nth 11 fields) 16))
						  ??))))
	       (list "Lowercase" (and (nth 12 fields)
				      (string (or (decode-char
						   'ucs
						   (string-to-number
						    (nth 12 fields) 16))
						  ??))))
	       (list "Titlecase" (and (nth 13 fields)
				      (string (or (decode-char
						   'ucs
						   (string-to-number
						    (nth 13 fields) 16))
						  ??)))))))))))

;; Return information about how CHAR is displayed at the buffer
;; position POS.  If the selected frame is on a graphic display,
;; return a cons (FONTNAME . GLYPH-CODE).  Otherwise, return a string
;; describing the terminal codes for the character.
(defun describe-char-display (pos char)
  (if (display-graphic-p (selected-frame))
      (internal-char-font pos char)
    (let* ((coding (terminal-coding-system))
	   (encoded (encode-coding-char char coding)))
      (if encoded
	  (encoded-string-description encoded coding)))))


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
	 (composition (find-composition pos nil nil t))
	 (component-chars nil)
	 (display-table (or (window-display-table)
			    buffer-display-table
			    standard-display-table))
	 (disp-vector (and display-table (aref display-table char)))
	 (multibyte-p enable-multibyte-characters)
	 (overlays (mapcar #'(lambda (o) (overlay-properties o))
			   (overlays-at pos)))
	 item-list max-width unicode)

    (if (or (< char 256)
	    (memq 'mule-utf-8 (find-coding-systems-region pos (1+ pos)))
	    (get-char-property pos 'untranslated-utf-8))
	(setq unicode (or (get-char-property pos 'untranslated-utf-8)
			  (encode-char char 'ucs))))
    (setq item-list
	  `(("character"
	    ,(format "%s (%d, #o%o, #x%x%s)"
		     (apply 'propertize (if (not multibyte-p)
					    (single-key-description char)
					  (if (< char 128)
					      (single-key-description char)
					    (string-to-multibyte
					     (char-to-string char))))
			    (text-properties-at pos))
		     char char char
		     (if unicode
			 (format ", U+%04X" unicode)
		       "")))
	    ("charset"
	     ,`(widget-create 'link
			      :notify (lambda (&rest ignore)
					(describe-character-set ',charset))
			      ,(symbol-name charset))
	     ,(format "(%s)" (charset-description charset)))
	    ("code point"
	     ,(let ((split (split-char char)))
		`(widget-create
		  'link
		  :notify (lambda (&rest ignore)
			    (list-charset-chars ',charset)
			    (with-selected-window
				(get-buffer-window "*Character List*" 0)
			      (goto-char (point-min))
                              (forward-line 2) ;Skip the header.
                              (let ((case-fold-search nil))
                                (search-forward ,(char-to-string char)
                                                nil t))))
		  ,(if (= (charset-dimension charset) 1)
		       (format "%d" (nth 1 split))
		     (format "%d %d" (nth 1 split) (nth 2 split))))))
	    ("syntax"
	     ,(let ((syntax (syntax-after pos)))
		(with-temp-buffer
		  (internal-describe-syntax-value syntax)
		  (buffer-string))))
	    ("category"
	     ,@(let ((category-set (char-category-set char)))
		 (if (not category-set)
		     '("-- none --")
		   (mapcar #'(lambda (x) (format "%c:%s"
						 x (category-docstring x)))
			   (category-set-mnemonics category-set)))))
	    ,@(let ((props (aref char-code-property-table char))
		    ps)
		(when props
		  (while props
		    (push (format "%s:" (pop props)) ps)
		    (push (format "%s;" (pop props)) ps))
		  (list (cons "Properties" (nreverse ps)))))
	    ("to input"
	     ,@(let ((key-list (and (eq input-method-function
					'quail-input-method)
				    (quail-find-key char))))
		 (if (consp key-list)
		     (list "type"
			   (mapconcat #'(lambda (x) (concat "\"" x "\""))
				      key-list " or ")
			   "with"
			   `(widget-create
			     'link
			     :notify (lambda (&rest ignore)
				       (describe-input-method
					',current-input-method))
			     ,(format "%s" current-input-method))))))
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
	    ("display"
	     ,(cond
	       (disp-vector
		(setq disp-vector (copy-sequence disp-vector))
		(dotimes (i (length disp-vector))
		  (setq char (aref disp-vector i))
		  (aset disp-vector i
			(cons char (describe-char-display
				    pos (logand char #x7ffff)))))
		(format "by display table entry [%s] (see below)"
			(mapconcat
			 #'(lambda (x)
			     (format "?%c" (logand (car x) #x7ffff)))
			 disp-vector " ")))
	       (composition
		(let ((from (car composition))
		      (to (nth 1 composition))
		      (next (1+ pos))
		      (components (nth 2 composition))
		      ch)
		  (setcar composition
			  (and (< from pos) (buffer-substring from pos)))
		  (setcar (cdr composition)
			  (and (< next to) (buffer-substring next to)))
		  (dotimes (i (length components))
		    (if (integerp (setq ch (aref components i)))
			(push (cons ch (describe-char-display pos ch))
			      component-chars)))
		  (setq component-chars (nreverse component-chars))
		  (format "composed to form \"%s\" (see below)"
			  (buffer-substring from to))))
	       (t
		(let ((display (describe-char-display pos char)))
		  (if (display-graphic-p (selected-frame))
		      (if display
			  (concat
			   "by this font (glyph code)\n"
			   (format "     %s (#x%02X)"
				   (car display) (cdr display)))
			"no font available")
		    (if display
			(format "terminal code %s" display)
		      "not encodable for terminal"))))))
	    ,@(let ((face
		     (if (not (or disp-vector composition))
			 (cond
			  ((and show-trailing-whitespace
				(save-excursion (goto-char pos)
						(looking-at "[ \t]+$")))
			   'trailing-whitespace)
			  ((and nobreak-char-display unicode (eq unicode '#xa0))
			   'nobreak-space)
			  ((and nobreak-char-display unicode (eq unicode '#xad))
			   'escape-glyph)
			  ((and (< char 32) (not (memq char '(9 10))))
			   'escape-glyph)))))
		(if face (list (list "hardcoded face"
				     `(widget-create
				       'link
				       :notify (lambda (&rest ignore)
						 (describe-face ',face))
				       ,(format "%s" face))))))
	    ,@(let ((unicodedata (and unicode
				      (describe-char-unicode-data unicode))))
		(if unicodedata
		    (cons (list "Unicode data" " ") unicodedata)))))
    (setq max-width (apply #'max (mapcar #'(lambda (x)
					     (if (cadr x) (length (car x)) 0))
					 item-list)))
    (with-output-to-temp-buffer "*Help*"
      (with-current-buffer standard-output
	(set-buffer-multibyte multibyte-p)
	(let ((formatter (format "%%%ds:" max-width)))
	  (dolist (elt item-list)
	    (when (cadr elt)
	      (insert (format formatter (car elt)))
	      (dolist (clm (cdr elt))
		(if (eq (car-safe clm) 'widget-create)
		    (progn (insert " ") (eval clm))
		  (when (>= (+ (current-column)
			       (or (string-match "\n" clm)
				   (string-width clm))
			       1)
			    (window-width))
		    (insert "\n")
		    (indent-to (1+ max-width)))
		  (insert " " clm)))
	      (insert "\n"))))

	(save-excursion
	  (goto-char (point-min))
	  (re-search-forward "character:[ \t\n]+")
	  (setq pos (point)))
	(if overlays
	    (mapc #'(lambda (props)
		      (let ((o (make-overlay pos (1+ pos))))
			(while props
			  (overlay-put o (car props) (nth 1 props))
			  (setq props (cddr props)))))
		  overlays))

	(when disp-vector
	  (insert
	   "\nThe display table entry is displayed by ")
	  (if (display-graphic-p (selected-frame))
	      (progn
		(insert "these fonts (glyph codes):\n")
		(dotimes (i (length disp-vector))
		  (insert (logand (car (aref disp-vector i)) #x7ffff) ?:
			  (propertize " " 'display '(space :align-to 5))
			  (if (cdr (aref disp-vector i))
			      (format "%s (#x%02X)" (cadr (aref disp-vector i))
				      (cddr (aref disp-vector i)))
			    "-- no font --")
			  "\n")
		  (when (> (car (aref disp-vector i)) #x7ffff)
		    (let* ((face-id (lsh (car (aref disp-vector i)) -19))
			   (face (car (delq nil (mapcar (lambda (face)
							  (and (eq (face-id face)
								   face-id) face))
							(face-list))))))
		      (when face
			(insert (propertize " " 'display '(space :align-to 5))
				"face: ")
			(widget-create 'link
				       :notify `(lambda (&rest ignore)
						  (describe-face ',face))
				       (format "%S" face))
			(insert "\n"))))))
	    (insert "these terminal codes:\n")
	    (dotimes (i (length disp-vector))
	      (insert (car (aref disp-vector i))
		      (propertize " " 'display '(space :align-to 5))
		      (or (cdr (aref disp-vector i)) "-- not encodable --")
		      "\n"))))

	(when composition
	  (insert "\nComposed")
	  (if (car composition)
	      (if (cadr composition)
		  (insert " with the surrounding characters \""
			  (car composition) "\" and \""
			  (cadr composition) "\"")
		(insert " with the preceding character(s) \""
			(car composition) "\""))
	    (if (cadr composition)
		(insert " with the following character(s) \""
			(cadr composition) "\"")))
	  (insert " by the rule:\n\t("
		  (mapconcat (lambda (x)
			       (format (if (consp x) "%S" "?%c") x))
			     (nth 2 composition)
			     " ")
		  ")")
	  (insert  "\nThe component character(s) are displayed by ")
	  (if (display-graphic-p (selected-frame))
	      (progn
		(insert "these fonts (glyph codes):")
		(dolist (elt component-chars)
		  (insert "\n " (car elt) ?:
			  (propertize " " 'display '(space :align-to 5))
			  (if (cdr elt)
			      (format "%s (#x%02X)" (cadr elt) (cddr elt))
			    "-- no font --"))))
	    (insert "these terminal codes:")
	    (dolist (elt component-chars)
	      (insert "\n  " (car elt) ":"
		      (propertize " " 'display '(space :align-to 5))
		      (or (cdr elt) "-- not encodable --"))))
	  (insert "\nSee the variable `reference-point-alist' for "
		  "the meaning of the rule.\n"))

	(describe-text-properties pos (current-buffer))
	(describe-text-mode)))))

(defalias 'describe-char-after 'describe-char)
(make-obsolete 'describe-char-after 'describe-char "22.1")

(provide 'descr-text)

;; arch-tag: fc55a498-f3e9-4312-b5bd-98cc02480af1
;;; descr-text.el ends here
