;;; enriched.el -- read and save files in text/enriched format
;; Copyright (c) 1994 Free Software Foundation

;; Author: Boris Goldowsky <boris@cs.rochester.edu>
;; Keywords: wp, faces

;; This file is part of GNU Emacs.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;
;; This file implements reading, editing, and saving files with
;; text-properties such as faces, levels of indentation, and true line breaks
;; distinguished from newlines just used to fit text into the window.
;;
;; The file format used is the MIME text/enriched format, which is a
;; standard format defined in internet RFC 1563.  All standard annotations are
;; supported except for <smaller> and <bigger>, which are currently not
;; possible to display.
;; 
;; A separate file, enriched.doc, contains further documentation and other
;; important information about this code.  It also serves as an example file
;; in text/enriched format.  It should be in the etc directory of your emacs
;; distribution.

(provide 'enriched)
(if window-system (require 'facemenu))

;;;
;;; Variables controlling the display
;;;

(defvar enriched-verbose t
  "*If non-nil, give status messages when reading and writing files.")

(defvar enriched-default-right-margin 10
  "*Default amount of space to leave on the right edge of the screen.
This can be increased inside text by changing the 'right-margin text property.
Measured in character widths.  If the screen is narrower than this, it is
assumed to be 0.")

(defvar enriched-fill-after-visiting t
  "If t, fills paragraphs when reading in enriched documents.
If nil, only fills when you explicitly request it.  If the value is 'ask, then
it will query you whether to fill.
Filling is never done if the current text-width is the same as the value
stored in the file.")

;;;
;;; Set up faces & display table
;;;

;; A slight cheat - all emacs's faces are fixed-width.  
;; The idea is just to pick one that looks different from the default.
(if (internal-find-face 'fixed)
    nil
  (make-face 'fixed)
  (if window-system
      (set-face-font 'fixed
		     (car (or (x-list-fonts "*fixed-medium*" 
					    'default (selected-frame))
			      (x-list-fonts "*fixed*" 
					    'default (selected-frame)))))))
			      
(if (internal-find-face 'excerpt)
    nil
  (make-face 'excerpt)
  (if window-system
      (make-face-italic 'excerpt)))

(defconst enriched-display-table (or (copy-sequence standard-display-table)
				     (make-display-table)))
(aset enriched-display-table ?\f (make-vector (1- (frame-width)) ?-))

(defconst enriched-par-props '(left-margin right-margin justification)
  "Text-properties that usually apply to whole paragraphs.
These are set front-sticky everywhere except at hard newlines.")

;;;
;;; Variables controlling the file format
;;;   (bidirectional)

(defconst enriched-initial-annotation
  (lambda ()
    (format "Content-Type: text/enriched\nText-Width: %d\n\n"
	    (enriched-text-width)))
  "What to insert at the start of a text/enriched file.
If this is a string, it is inserted.  If it is a list, it should be a lambda
expression, which is evaluated to get the string to insert.")

(defconst enriched-annotation-format "<%s%s>"
  "General format of enriched-text annotations.")

(defconst enriched-annotation-regexp "<\\(/\\)?\\([-A-za-z0-9]+\\)>"
  "Regular expression matching enriched-text annotations.")

(defconst enriched-translations
  '((face          (bold-italic "bold" "italic")
		   (bold        "bold")
		   (italic      "italic")
		   (underline   "underline")
		   (fixed       "fixed")
		   (excerpt     "excerpt")
		   (default     )
		   (nil         enriched-encode-other-face))
    (left-margin   (4           "indent"))
    (right-margin  (4           "indentright"))
    (justification (none        "nofill")
		   (right       "flushright")
		   (left        "flushleft")
		   (full        "flushboth")
		   (center      "center")) 
    (PARAMETER     (t           "param")) ; Argument of preceding annotation
    ;; The following are not part of the standard:
    (FUNCTION      (enriched-decode-foreground "x-color")
		   (enriched-decode-background "x-bg-color"))
    (read-only     (t           "x-read-only"))
    (unknown       (nil         format-annotate-value))
;   (font-size     (2           "bigger")       ; unimplemented
;		   (-2          "smaller"))
)
  "List of definitions of text/enriched annotations.
See `format-annotate-region' and `format-deannotate-region' for the definition
of this structure.")

(defconst enriched-ignore
  '(front-sticky rear-nonsticky hard)
  "Properties that are OK to ignore when saving text/enriched files.
Any property that is neither on this list nor dealt with by
`enriched-translations' will generate a warning.")

;;; Internal variables

(defvar enriched-mode nil
  "True if `enriched-mode' is in use.")
(make-variable-buffer-local 'enriched-mode)

(if (not (assq 'enriched-mode minor-mode-alist))
    (setq minor-mode-alist
	  (cons '(enriched-mode " Enriched")
		minor-mode-alist)))

(defvar enriched-mode-hooks nil
  "Functions to run when entering `enriched-mode'.
If you set variables in this hook, you should arrange for them to be restored
to their old values if enriched-mode is left.  One way to do this is to add
them and their old values to `enriched-old-bindings'.")

(defvar enriched-old-bindings nil
  "Store old variable values that we change when entering mode.
The value is a list of \(VAR VALUE VAR VALUE...).")
(make-variable-buffer-local 'enriched-old-bindings)

(defvar enriched-text-width nil)
(make-variable-buffer-local 'enriched-text-width)

;;;
;;; Define the mode
;;;

;;;###autoload
(defun enriched-mode (&optional arg)
  "Minor mode for editing text/enriched files.
These are files with embedded formatting information in the MIME standard
text/enriched format.
Turning the mode on runs `enriched-mode-hooks'.

More information about enriched-mode is available in the file 
etc/enriched.doc  in the Emacs distribution directory.

Commands:

\\<enriched-mode-map>\\{enriched-mode-map}"
  (interactive "P")
  (let ((mod (buffer-modified-p)))
    (cond ((or (<= (prefix-numeric-value arg) 0)
	       (and enriched-mode (null arg)))
	   ;; Turn mode off
	   (setq enriched-mode nil)
	   (setq buffer-file-format (delq 'text/enriched buffer-file-format))
	   ;; restore old variable values
	   (while enriched-old-bindings
	     (funcall 'set (car enriched-old-bindings)
		      (car (cdr enriched-old-bindings)))
	     (setq enriched-old-bindings (cdr (cdr enriched-old-bindings)))))

	  (enriched-mode nil)		; Mode already on; do nothing.

	  (t (setq enriched-mode t)	; Turn mode on
	     (if (not (memq 'text/enriched buffer-file-format))
		 (setq buffer-file-format 
		       (cons 'text/enriched buffer-file-format)))
	     ;; Save old variable values before we change them.
	     ;; These will be restored if we exit enriched-mode.
	     (setq enriched-old-bindings
		   (list 'buffer-display-table buffer-display-table
			 'indent-line-function indent-line-function
			 'use-hard-newlines    use-hard-newlines
			 'default-text-properties default-text-properties))
	     (make-local-variable 'indent-line-function)
	     (make-local-variable 'use-hard-newlines)
	     (make-local-variable 'default-text-properties)
	     (setq indent-line-function 'indent-to-left-margin
		   buffer-display-table  enriched-display-table
		   use-hard-newlines     t)
	     (let ((sticky (plist-get default-text-properties 'front-sticky))
		   (p enriched-par-props))
	       (while p
		 (if (not (memq (car p) sticky))
		     (setq sticky (cons (car p) sticky)))
		 (setq p (cdr p)))
	       (if sticky
		   (setq default-text-properties
			 (plist-put default-text-properties
				    'front-sticky sticky))))
	     (run-hooks 'enriched-mode-hooks)))
    (set-buffer-modified-p mod)
    (force-mode-line-update)))

;;;
;;; Keybindings
;;;

(defvar enriched-mode-map nil
  "Keymap for `enriched-mode'.")

(if (null enriched-mode-map)
    (fset 'enriched-mode-map (setq enriched-mode-map (make-sparse-keymap))))

(if (not (assq 'enriched-mode minor-mode-map-alist))
    (setq minor-mode-map-alist
	  (cons (cons 'enriched-mode enriched-mode-map)
		minor-mode-map-alist)))

(define-key enriched-mode-map "\C-a" 'beginning-of-line-text)
(define-key enriched-mode-map "\C-m" 'reindent-then-newline-and-indent)
(define-key enriched-mode-map "\C-j" 'reindent-then-newline-and-indent)
(define-key enriched-mode-map "\M-j" 'facemenu-justification-menu)
(define-key enriched-mode-map "\M-S" 'set-justification-center)
(define-key enriched-mode-map "\C-x\t" 'increase-left-margin)
(define-key enriched-mode-map "\C-c\C-l" 'set-left-margin)
(define-key enriched-mode-map "\C-c\C-r" 'set-right-margin)

;;;
;;; Some functions dealing with text-properties, especially indentation
;;;

(defun enriched-map-property-regions (prop func &optional from to)
  "Apply a function to regions of the buffer based on a text property.
For each contiguous region of the buffer for which the value of PROPERTY is
eq, the FUNCTION will be called.  Optional arguments FROM and TO specify the
region over which to scan.

The specified function receives three arguments: the VALUE of the property in
the region, and the START and END of each region."
  (save-excursion
    (save-restriction
      (if to (narrow-to-region (point-min) to))
      (goto-char (or from (point-min)))
      (let ((begin (point))
	    end
	    (marker (make-marker))
	    (val (get-text-property (point) prop)))
	(while (setq end (text-property-not-all begin (point-max) prop val))
	  (move-marker marker end)
	  (funcall func val begin (marker-position marker))
	  (setq begin (marker-position marker)
		val (get-text-property marker prop)))
	(if (< begin (point-max))
	    (funcall func val begin (point-max)))))))

(put 'enriched-map-property-regions 'lisp-indent-hook 1)

(defun enriched-insert-indentation (&optional from to)
  "Indent and justify each line in the region."
  (save-excursion
    (save-restriction
      (if to (narrow-to-region (point-min) to))
      (goto-char (or from (point-min)))
      (if (not (bolp)) (forward-line 1))
      (while (not (eobp))
	(if (eolp)
	    nil ; skip blank lines
	  (indent-to (current-left-margin))
	  (justify-current-line t nil t))
	(forward-line 1)))))

(defun enriched-text-width ()
  "The width of unindented text in this window, in characters.
This is the width of the window minus `enriched-default-right-margin'."
  (or enriched-text-width
      (let ((ww (window-width)))
	(setq enriched-text-width
	      (if (> ww enriched-default-right-margin)
		  (- ww enriched-default-right-margin)
		ww)))))

;;;
;;; Encoding Files
;;;

;;;###autoload
(defun enriched-encode (from to)
  (if enriched-verbose (message "Enriched: encoding document..."))
  (save-restriction
    (narrow-to-region from to)
    (delete-to-left-margin)
    (unjustify-region)
    (goto-char from)
    (format-replace-strings '(("<" . "<<")))
    (format-insert-annotations 
     (format-annotate-region from (point-max) enriched-translations
			     'enriched-make-annotation enriched-ignore))
    (goto-char from)
    (insert (if (stringp enriched-initial-annotation)
		enriched-initial-annotation
	      (funcall enriched-initial-annotation)))
    (enriched-map-property-regions 'hard
      (lambda (v b e)
	(if (and v (= ?\n (char-after b)))
	    (progn (goto-char b) (insert "\n"))))
      (point) nil)
    (if enriched-verbose (message nil))
    ;; Return new end.
    (point-max)))

(defun enriched-make-annotation (name positive)
  "Format an annotation called NAME.
If POSITIVE is non-nil, this is the opening annotation, if nil, this is the
matching close."
  (cond ((stringp name)
	 (format enriched-annotation-format (if positive "" "/") name))
	;; Otherwise it is an annotation with parameters, represented as a list
	(positive
	 (let ((item (car name))
	       (params (cdr name)))
	   (concat (format enriched-annotation-format "" item)
		   (mapconcat (lambda (i) (concat "<param>" i "</param>"))
			      params ""))))
	(t (format enriched-annotation-format "/" (car name)))))

(defun enriched-encode-other-face (old new)
  "Generate annotations for random face change.
One annotation each for foreground color, background color, italic, etc."
  (cons (and old (enriched-face-ans old))
	(and new (enriched-face-ans new))))
	    
(defun enriched-face-ans (face)
  "Return annotations specifying FACE."
  (cond ((string-match "^fg:" (symbol-name face))
	 (list (list "x-color" (substring (symbol-name face) 3))))
	((string-match "^bg:" (symbol-name face))
	 (list (list "x-bg-color" (substring (symbol-name face) 3))))
	((let* ((fg (face-foreground face))
		(bg (face-background face))
		(props (face-font face t))
		(ans (cdr (format-annotate-single-property-change
			   'face nil props enriched-translations))))
	   (if fg (setq ans (cons (list "x-color" fg) ans)))
	   (if bg (setq ans (cons (list "x-bg-color" bg) ans)))
	   ans))))

;;;
;;; Decoding files
;;;

;;;###autoload
(defun enriched-decode (from to)
  (if enriched-verbose (message "Enriched: decoding document..."))
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char from)
      (let ((file-width (enriched-get-file-width))
	    (use-hard-newlines t))
	(enriched-remove-header)

	;; Deal with newlines
	(goto-char from)
	(while (search-forward-regexp "\n\n+" nil t)
	  (if (current-justification)
	      (delete-char -1))
	  (put-text-property (match-beginning 0) (point) 'hard t)
	  (put-text-property (match-beginning 0) (point) 'front-sticky nil))

	;; Translate annotations
	(format-deannotate-region from (point-max) enriched-translations
				  'enriched-next-annotation)

	;; Fill paragraphs
	(if (or (and file-width		; possible reasons not to fill:
		     (= file-width (enriched-text-width))) ; correct wd.
		(null enriched-fill-after-visiting) ; never fill
		(and (eq 'ask enriched-fill-after-visiting) ; asked & declined
		     (not (y-or-n-p "Re-fill for current display width? "))))
	    ;; Minimally, we have to insert indentation and justification.
	    (enriched-insert-indentation)
	  (if enriched-verbose (message "Filling paragraphs..."))
	  (fill-region (point-min) (point-max))))
      (if enriched-verbose (message nil))
      (point-max))))

(defun enriched-next-annotation ()
  "Find and return next text/enriched annotation.
Any \"<<\" strings encountered are coverted to \"<\".
Return value is \(begin end name positive-p), or nil if none was found."
  (while (and (search-forward "<" nil 1)
	      (progn (goto-char (match-beginning 0))
		     (not (looking-at enriched-annotation-regexp))))
    (forward-char 1)
    (if (= ?< (char-after (point)))
	(delete-char 1)
      ;; A single < that does not start an annotation is an error,
      ;; which we note and then ignore.
      (message (format "Warning: malformed annotation in file at %s" 
		       (1- (point))))))
  (if (not (eobp))
      (let* ((beg (match-beginning 0))
	     (end (match-end 0))
	     (name (downcase (buffer-substring 
			      (match-beginning 2) (match-end 2))))
	     (pos (not (match-beginning 1))))
	(list beg end name pos))))

(defun enriched-get-file-width ()
  "Look for file width information on this line."
  (save-excursion
    (if (search-forward "Text-Width: " (+ (point) 1000) t)
	(read (current-buffer)))))

(defun enriched-remove-header ()
  "Remove file-format header at point."
  (while (looking-at "^[-A-Za-z]+: .*\n")
    (delete-region (point) (match-end 0)))
  (if (looking-at "^\n")
      (delete-char 1)))

(defun enriched-decode-foreground (from to color)
  (let ((face (intern (concat "fg:" color))))
    (cond ((internal-find-face face))
	  ((and window-system (facemenu-get-face face)))
	  (window-system
	   (message "Warning: color \"%s\" is not defined." color))
	  ((make-face face)
	   (message "Warning: Color \"%s\" can't be displayed." color)))
    (list from to 'face face)))

(defun enriched-decode-background (from to color)
  (let ((face (intern (concat "bg:" color))))
    (cond ((internal-find-face face))
	  ((and window-system (facemenu-get-face face)))
	  (window-system
	   (message "Warning: color \"%s\" is not defined." color))
	  ((make-face face)
	   (message "Warning: Color \"%s\" can't be displayed." color)))
    (list from to 'face face)))

;;; enriched.el ends here
