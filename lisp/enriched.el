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

(defvar enriched-auto-save-interval 1000
  "*`Auto-save-interval' to use for `enriched-mode'.
Auto-saving enriched files is slow, so you may wish to have them happen less
often.  You can set this to nil to only do auto-saves when you are not
actively working.")

;;Unimplemented:
;(defvar enriched-aggressive-auto-fill t
;  "*If t, try to keep things properly filled and justified always.
;Set this to nil if you have a slow terminal or prefer to justify on request.
;The difference between aggressive and non-aggressive is subtle right now, but
;may become stronger in the future.")

;; Unimplemented:
; (defvar enriched-keep-ignored-items nil
;   "*If t, keep track of codes that are not understood.
; Otherwise they are deleted on reading the file, and not written out.")

;;Unimplemented:
;(defvar enriched-electric-indentation t
;  "*If t, newlines and following indentation stick together.
;Deleting a newline or any part of the indenation will delete the whole
;stretch.")

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

;;; The following two faces should not appear on menu.
(if (boundp 'facemenu-unlisted-faces)
    (setq facemenu-unlisted-faces 
	  (append '(enriched-code-face enriched-indentation-face)
		  facemenu-unlisted-faces)))

(if (internal-find-face 'enriched-code-face)
    nil
  (make-face 'enriched-code-face)
  (if window-system
      (set-face-background 'enriched-code-face 
			   (if (x-display-color-p)
			       "LightSteelBlue"
			     "gray35"))))

(if (internal-find-face 'enriched-indentation-face)
    nil
  (make-face 'enriched-indentation-face)
  (if window-system
      (set-face-background 'enriched-indentation-face
			   (if (x-display-color-p)
			       "DarkSlateBlue" 
			     "gray25"))))

(defvar enriched-display-table (make-display-table))
(aset enriched-display-table ?\f (make-vector (1- (frame-width)) ?-))

; (defvar enriched-show-codes nil "See the function of the same name")

(defvar enriched-par-props '(left-margin right-margin justification 
					 front-sticky)
  "Text-properties that usually apply to whole paragraphs.
These are set front-sticky everywhere except at hard newlines.")

;;;
;;; Variables controlling the file format
;;;   (bidirectional)

(defvar enriched-initial-annotation
  (lambda ()
    (format "<param>-*-enriched-*-width:%d
</param>" (enriched-text-width)))
  "What to insert at the start of a text/enriched file.
If this is a string, it is inserted.  If it is a list, it should be a lambda
expression, which is evaluated to get the string to insert.")

(defvar enriched-annotation-format "<%s%s>"
  "General format of enriched-text annotations.")

(defvar enriched-annotation-regexp "<\\(/\\)?\\([-A-za-z0-9]+\\)>"
  "Regular expression matching enriched-text annotations.")

(defvar enriched-downcase-annotations t
  "Set to t if case of annotations is irrelevant.
In this case all annotations listed in enriched-annotation-list should be
lowercase, and annotations read from files will be downcased before being
compared to that list.")

(defvar enriched-list-valued-properties '(face unknown)
  "List of properties whose values can be lists.")

(defvar enriched-annotation-alist
  '((face          (bold-italic "bold" "italic")
		   (bold        "bold")
		   (italic      "italic")
		   (underline   "underline")
		   (fixed       "fixed")
		   (excerpt     "excerpt")
		   (default     )
		   (nil         enriched-encode-other-face))
    (hard          (nil         enriched-encode-hard-newline))
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
    (unknown       (nil         enriched-encode-unknown)) ;anything else found
;   (font-size     (2           "bigger")       ; unimplemented
;		   (-2          "smaller"))
)
  "List of definitions of text/enriched annotations.
Each element is a list whose car is a PROPERTY, and the following
elements are VALUES of that property followed by zero or more ANNOTATIONS.
Whenever the property takes on that value, each of the annotations
will be inserted into the file.  Only the name of the annotation
should be specified, it will be formatted by `enriched-make-annotation'.
At the point that the property stops having that value, the matching
negated annotation will be inserted (it may actually be closed earlier and
reopened, if necessary, to keep proper nesting).

Conversely, when annotations are read, they are searched for in this list, and
the relevant text property is added to the buffer.  The first match found whose
conditions are satisfied is used.  If enriched-downcase-annotations is true,
then annotations in this list should be listed in lowercase, and annotations
read from the file will be downcased.

If the VALUE is numeric, then it is assumed that there is a single annotation
and each occurrence of it increments the value of the property by that number.
Thus, given the entry \(left-margin \(4 \"indent\")), `enriched-encode-region'
will insert two <indent> annotations if the left margin changes from 4 to 12.

If the VALUE is nil, then instead of annotations, a function should be
specified.  This function is used as a default: it is called for all
transitions not explicitly listed in the table.  The function is called with
two arguments, the OLD and NEW values of the property.  It should return a
list of annotations like `enriched-loc-annotations' does, or may directly
modify the buffer.  Note that this only works for encoding; there must be some
other way of decoding the annotations thus produced.

[For future expansion:] If the VALUE is a list, then the property's value will
be appended to the surrounding value of the property.

For decoding, there are some special symbols that can be used in the
\"property\" slot.  Annotations listed under the pseudo-property PARAMETER are
considered to be arguments of the immediately surrounding annotation; the text
between the opening and closing parameter annotations is deleted from the
buffer but saved as a string.  The surrounding annotation should be listed
under the pseudo-property FUNCTION.  Instead of inserting a text-property for
this annotation, enriched-decode-buffer will call the function listed in the
VALUE slot, with the first two arguments being the start and end locations and
the rest of the arguments being any PARAMETERs found in that region.")

;;; This is not needed for text/enriched format, since all annotations are in
;;; a standard form:
;(defvar enriched-special-annotations-alist nil
;  "List of annotations not formatted in the usual way.
;Each element has the form (ANNOTATION BEGIN END), where
;ANNOTATION is the annotation's name, which is a symbol (normal
;annotations are named with strings, special ones with symbols),
;BEGIN is the literal string to insert as the opening annotation, and
;END is the literal string to insert as the close.
;This is used only for encoding.  Typically, each will have an entry in
;enriched-decode-special-alist to deal with its decoding.")

;;; Encoding variables

(defvar enriched-encode-interesting-regexp "<"
  "Regexp matching the start of something that may require encoding.
All text-property changes are also considered \"interesting\".")

(defvar enriched-encode-special-alist
  '(("<" . (lambda () (insert-and-inherit "<"))))
  "List of special operations for writing enriched files.
Each element has the form \(STRING . FUNCTION).
Whenever one of the strings \(including its properties, if any)
is found, the corresponding function is called.
Match data is available to the function.  
See `enriched-decode-special-alist' for instructions on decoding special
items.")

(defvar enriched-ignored-ok
  '(front-sticky rear-nonsticky)
  "Properties that are not written into enriched files.
Generally this list should only contain properties that just for enriched's
internal purposes; other properties that cannot be recorded will generate
a warning message to the user since information will be lost.")

;;; Decoding variables

(defvar enriched-decode-interesting-regexp "[<\n]"
  "Regexp matching the start of something that may require decoding.")

(defvar enriched-decode-special-alist
  '(("<<" . (lambda () (delete-char 1) (forward-char 1)))
    ("\n\n" . enriched-decode-hard-newline))
  "List of special operations for reading enriched files.
Each element has the form \(STRING . FUNCTION).
Whenever one of the strings is found, the corresponding function is called,
with point at the beginning of the match and the match data is available to
the function.  Should leave point where next search should start.")

;;; Internal variables

(defvar enriched-mode nil
  "True if `enriched-mode' \(which see) is enabled.")
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

(defvar enriched-translated nil
  "True if buffer has already been decoded.")
(make-variable-buffer-local 'enriched-translated)

(defvar enriched-text-width nil)
(make-variable-buffer-local 'enriched-text-width)

(defvar enriched-ignored-list nil)

(defvar enriched-open-ans nil)

;;;
;;; Functions defining the format of annotations
;;;

(defun enriched-make-annotation (name positive)
  "Format an annotation called NAME.
If POSITIVE is non-nil, this is the opening annotation, if nil, this is the
matching close."
;; Could be used for annotations not following standard form:
;  (if (symbolp name)
;      (if positive
;	  (elt (assq name enriched-special-annotations-alist) 1)
;	(elt (assq name enriched-special-annotations-alist) 2))  )
  (if (stringp name)
      (format enriched-annotation-format (if positive "" "/") name)
    ;; has parameters.
    (if positive
	(let ((item (car name))
	      (params (cdr name)))
	  (concat (format enriched-annotation-format "" item)
		  (mapconcat (lambda (i) (concat "<param>" i "</param>"))
			     params "")))
      (format enriched-annotation-format "/" (car name)))))

(defun enriched-annotation-name (a)
  "Find the name of an ANNOTATION."
  (save-match-data
    (if (string-match enriched-annotation-regexp a)
	(substring a (match-beginning 2) (match-end 2)))))

(defun enriched-annotation-positive-p (a)
  "Returns t if ANNOTATION is positive (open),
or nil if it is a closing (negative) annotation."
  (save-match-data
    (and (string-match enriched-annotation-regexp a)
	 (not (match-beginning 1)))))

(defun enriched-encode-unknown (old new)
  "Deals with re-inserting unknown annotations."
  (cons (if old (list old))
	(if new (list new))))

(defun enriched-encode-hard-newline (old new)
  "Deal with encoding `hard-newline' property change."
  ;; This makes a sequence of N hard newlines into N+1 duplicates of the first
  ;; one- so all property changes are put off until after all the newlines.
  (if (and new (current-justification))  ; no special processing inside NoFill
      (let* ((length (skip-chars-forward "\n"))
	     (s (make-string length ?\n)))
	(backward-delete-char (1- length))
	(add-text-properties 0 length (text-properties-at (1- (point))) s)
	(insert s)
	(backward-char (+ length 1)))))

(defun enriched-decode-hard-newline () 
  "Deal with newlines while decoding file."
  (let ((nofill (equal "nofill" ; find out if we're in NoFill region
		       (enriched-which-assoc 
			'("nofill" "flushleft" "flushright" "center" 
			  "flushboth")
			enriched-open-ans)))
	(n (skip-chars-forward "\n")))
    (delete-char (- n))
    (newline (if nofill n (1- n)))))

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
		(ans (cdr (enriched-annotate-change 'face nil props))))
	   (if fg (enriched-push (list "x-color" fg) ans))
	   (if bg (enriched-push (list "x-bg-color" bg) ans))
	   ans))))

(defun enriched-decode-foreground (from to color)
  (let ((face (intern (concat "fg:" color))))
    (cond ((internal-find-face face))
	  ((and window-system (facemenu-get-face face)))
	  (window-system
	   (enriched-warn "Color \"%s\" not defined:
    Try M-x set-face-foreground RET %s RET some-other-color" color face))
	  ((make-face face)
	   (enriched-warn "Color \"%s\" can't be displayed." color)))
    (list from to 'face face)))

(defun enriched-decode-background (from to color)
  (let ((face (intern (concat "bg:" color))))
    (cond ((internal-find-face face))
	  ((and window-system (facemenu-get-face face)))
	  (window-system
	   (enriched-warn "Color \"%s\" not defined:
    Try M-x set-face-background RET %s RET some-other-color" color face))
	  ((make-face face)
	   (enriched-warn "Color \"%s\" can't be displayed." color)))
    (list from to 'face face)))

;;;
;;; NOTE: Everything below this point is intended to be independent of the file
;;; format, which is defined by the variables and functions above.
;;;

;;;
;;; Define the mode
;;;

;;;###autoload
(defun enriched-mode (&optional arg notrans)
  "Minor mode for editing text/enriched files.
These are files with embedded formatting information in the MIME standard
text/enriched format.

Turning the mode on or off interactively will query whether the buffer
should be translated into or out of text/enriched format immediately.
Noninteractively translation is done without query unless the optional
second argument NO-TRANS is non-nil.  
Turning mode on runs `enriched-mode-hooks'.

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
	   (if (if (interactive-p)
		   (y-or-n-p "Translate buffer into text/enriched format?")
		 (not notrans))
	       (progn (enriched-encode-region)
		      (mapcar (lambda (x)
				(remove-text-properties 
				 (point-min) (point-max)
				 (list (if (consp x) (car x) x) nil)))
			      (append enriched-ignored-ok
				      enriched-annotation-alist))
		      (setq enriched-translated nil)))
	   ;; restore old variable values
	   (while enriched-old-bindings
	     (funcall 'set (car enriched-old-bindings)
		      (car (cdr enriched-old-bindings)))
	     (setq enriched-old-bindings (cdr (cdr enriched-old-bindings))))
	   (remove-hook 'write-region-annotate-functions
			'enriched-annotate-function t)
	   (remove-hook 'after-change-functions 'enriched-nogrow-hook t))
	  (enriched-mode nil)		; Mode already on; do nothing.
	  (t				; Turn mode on
	   ;; save old variable values before we change them.
	   (setq enriched-mode t
		 enriched-old-bindings 
		 (list 'auto-save-interval   auto-save-interval
		       'buffer-display-table buffer-display-table
		       'indent-line-function indent-line-function
		       'use-hard-newlines    use-hard-newlines))
	   (make-local-variable 'auto-save-interval)
	   (make-local-variable 'indent-line-function)
	   (make-local-variable 'use-hard-newlines)
	   (setq auto-save-interval enriched-auto-save-interval
		 indent-line-function 'indent-to-left-margin
		 buffer-display-table enriched-display-table
		 use-hard-newlines t) ; Weird in Center&FlushRight
	   ;; Add hooks
	   (add-hook 'write-region-annotate-functions 
		     'enriched-annotate-function)
;	   (add-hook 'after-change-functions 'enriched-nogrow-hook)

	   (put-text-property (point-min) (point-max)
			      'front-sticky enriched-par-props)

	   (if (and (not enriched-translated)
		    (if (interactive-p) 
			(y-or-n-p "Does buffer need to be translated now? ")
		      (not notrans)))
	       (progn (enriched-decode-region)
		      (setq enriched-translated t)))
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

(define-key enriched-mode-map "\C-a" 'move-to-left-margin)
(define-key enriched-mode-map "\C-j" 'newline)
(define-key enriched-mode-map "\M-j" 'enriched-justification-menu-map)
(define-key enriched-mode-map "\M-S" 'set-justification-center)
(define-key enriched-mode-map "\C-x\t" 'increment-left-margin)
(define-key enriched-mode-map "\C-c\C-l" 'set-left-margin)
(define-key enriched-mode-map "\C-c\C-r" 'set-right-margin)
;;(define-key enriched-mode-map "\C-c\C-s" 'enriched-show-codes)

;;;
;;; General list/stack manipulation
;;;

(defmacro enriched-push (item stack)
  "Push ITEM onto STACK.
STACK should be a symbol whose value is a list."
  (` (setq (, stack) (cons (, item) (, stack)))))

(defmacro enriched-pop (stack)
  "Remove and return first item on STACK."
  (` (let ((pop-item (car (, stack))))
       (setq (, stack) (cdr (, stack)))
       pop-item)))

(defun enriched-delq1 (cons list)
  "Remove the given CONS from LIST by side effect.
Since CONS could be the first element of LIST, write
`(setq foo (enriched-delq1 element foo))' to be sure of changing the value
of `foo'."
  (if (eq cons list)
      (cdr list)
    (let ((p list))
      (while (not (eq (cdr p) cons))
	(if (null p) (error "enriched-delq1: Attempt to delete a non-element"))
	(setq p (cdr p)))
      ;; Now (cdr p) is the cons to delete
      (setcdr p (cdr cons))
      list)))
    
(defun enriched-make-list-uniq (list)
  "Destructively remove duplicates from LIST.
Compares using `eq'."
  (let ((l list))
    (while l
      (setq l (setcdr l (delq (car l) (cdr l)))))
    list))

(defun enriched-make-relatively-unique (a b)
  "Delete common elements of lists A and B, return as pair.
Compares using `equal'."
  (let* ((acopy (copy-sequence a))
	 (bcopy (copy-sequence b))
	 (tail acopy))
    (while tail
      (let ((dup (member (car tail) bcopy))
	    (next (cdr tail)))
	(if dup (setq acopy (enriched-delq1 tail acopy)
		      bcopy (enriched-delq1 dup  bcopy)))
	(setq tail next)))
    (cons acopy bcopy)))

(defun enriched-common-tail (a b)
  "Given two lists that have a common tail, return it.
Compares with `equal', and returns the part of A that is equal to the
equivalent part of B.  If even the last items of the two are not equal,
returns nil."
  (let ((la (length a))
	(lb (length b)))
    ;; Make sure they are the same length
    (while (> la lb)
      (setq a (cdr a)
	    la (1- la)))
    (while (> lb la)
      (setq b (cdr b)
	    lb (1- lb))))
  (while (not (equal a b))
    (setq a (cdr a)
	  b (cdr b)))
  a)

(defun enriched-which-assoc (items list)
  "Return which one of ITEMS occurs first as a car of an element of LIST."
  (let (res)
    (while list
      (if (setq res (member (car (car list)) items))
	  (setq res (car res)
		list nil)
	(setq list (cdr list))))
    res))
	
(defun enriched-reorder (items order)
  "Arrange ITEMS to following partial ORDER.
Elements of ITEMS equal to elements of ORDER will be rearranged to follow the
ORDER.  Unmatched items will go last."
  (if order
      (let ((item (member (car order) items)))
	(if item
	    (cons (car item) 
		  (enriched-reorder (enriched-delq1 item items)
			      (cdr order)))
	  (enriched-reorder items (cdr order))))
    items))

;;;
;;; Utility functions
;;;

(defun enriched-get-face-attribute (attr face &optional frame)
  "Get an attribute of a face or list of faces.
ATTRIBUTE should be one of the functions `face-font' `face-foreground',
`face-background', or `face-underline-p'.  FACE can be a face or a list of
faces.  If optional argument FRAME is given, report on the face in that frame.
If FRAME is t, report on the defaults for the face in new frames.  If FRAME is
omitted or nil, use the selected frame."
  (cond ((null face) nil)
	((or (symbolp face) (internal-facep face)) (funcall attr face frame))
	((funcall attr (car face) frame))
	((enriched-get-face-attribute attr (cdr face) frame))))

(defun enriched-overlays-overlapping (begin end &optional test)
  "Return a list of the overlays which overlap the specified region.
If optional arg TEST is given, it is called with each overlay as its
argument, and only those for which it is true are returned."
  (overlay-recenter begin)
  (let ((res nil)
	(overlays (cdr (overlay-lists)))) ; includes all ending after BEGIN
    (while overlays
      (if (and (< (overlay-start (car overlays)) end)
	       (or (not test)
		   (funcall test (car overlays))))
	  (enriched-push (car overlays) res))
      (setq overlays (cdr overlays)))
    res))

;(defun enriched-show-codes (&rest which)
;  "Enable or disable highlighting of special regions.
;With argument null or `none', turns off highlighting.
;If argument is `newline', turns on display of hard newlines.
;If argument is `indent', highlights the automatic indentation at the beginning
;of each line.
;If argument is `margin', highlights all regions with non-standard margins."
;  (interactive
;   (list (intern (completing-read "Show which codes: "
;				  '(("none") ("newline") ("indent") ("margin"))
;				  nil t))))
;  (if (null which)
;      (setq enriched-show-codes nil)
;    (setq enriched-show-codes which))
;  ;; First delete current overlays
;  (let* ((ol (overlay-lists))
;	 (overlays (append (car ol) (cdr ol))))
;    (while overlays
;      (if (eq (overlay-get (car overlays) 'face) 'enriched-code-face)
;	  (delete-overlay (car overlays)))
;      (setq overlays (cdr overlays))))
;  ;; Now add new ones for each thing displayed.
;  (if (null which)
;      (message "Code display off."))
;  (while which
;    (cond ((eq (car which) 'margin)
;	   (enriched-show-margin-codes))
;	  ((eq (car which) 'indent)
;	   (enriched-map-property-regions 'enriched-indentation
;	     (lambda (v b e)
;	       (if v (enriched-show-region-as-code b e 'indent)))))
;	  ((eq (car which) 'newline)
;	   (save-excursion
;	     (goto-char (point-min))
;	     (while (enriched-search-forward-with-props
;		     enriched-hard-newline nil t)
;	       (enriched-show-region-as-code (match-beginning 0) (match-end 0)
;				       'newline)))))
;    (setq which (cdr which))))
  
;(defun enriched-show-margin-codes (&optional from to)
;  "Highlight regions with nonstandard left-margins.
;See `enriched-show-codes'."
;  (enriched-map-property-regions 'left-margin
;    (lambda (v b e)
;      (if (and v (> v 0))
;	  (enriched-show-region-as-code b e 'margin)))
;    from to)
;  (enriched-map-property-regions 'right-margin
;    (lambda (v b e)
;      (if (and v (> v 0))
;	  (enriched-show-region-as-code b e 'margin)))
;    from to))
	
;(defun enriched-show-region-as-code (from to type)
;  "Display region between FROM and TO as a code if TYPE is displayed.
;Displays it only if TYPE is an element of `enriched-show-codes' or is t."
;  (if (or (eq t type) (memq type enriched-show-codes))
;      (let* ((old (enriched-overlays-overlapping 
;		   from to (lambda (o)
;			     (eq 'enriched-code-face 
;				 (overlay-get o 'face)))))
;	     (new (if old (move-overlay (car old) from to)
;		    (make-overlay from to))))
;	(overlay-put new 'face 'enriched-code-face)
;	(overlay-put new 'front-nogrow t)
;	(if (eq type 'margin)
;	    (overlay-put new 'rear-grow t))
;	(while (setq old (cdr old))
;	  (delete-overlay (car old))))))

;(defun enriched-nogrow-hook (beg end old-length)
;  "Implement front-nogrow and rear-grow for overlays.
;Normally overlays have opposite inheritance properties than
;text-properties: they will expand to include text inserted at their
;beginning, but not text inserted at their end.  However, 
;if this function is an element of `after-change-functions', then
;overlays with a non-nil value of the `front-nogrow' property will not
;expand to include text that is inserted just in front of them, and
;overlays with a non-nil value of the `rear-grow' property will 
;expand to include text that is inserted just after them."
;  (if (not (zerop old-length))
;      nil ;; not an insertion
;    (let ((overlays (overlays-at end)) o)
;      (while overlays
;	(setq o (car overlays)
;	      overlays (cdr overlays))
;	(if (and (overlay-get o 'front-nogrow)
;		 (= beg (overlay-start o)))
;	    (move-overlay o end (overlay-end o)))))
;    (let ((overlays (overlays-at (1- beg))) o)
;      (while overlays
;	(setq o (car overlays)
;	      overlays (cdr overlays))
;	(if (and (overlay-get o 'rear-grow)
;		 (= beg (overlay-end o)))
;	    (move-overlay o (overlay-start o) end))))))

(defun enriched-warn (&rest args)
  "Display a warning message.
Arguments are given to `format' and the result is displayed in a buffer."
  (save-excursion
    (let ((buf (current-buffer))
	  (line (1+ (count-lines 1 (point))))
	  (mark (point-marker)))
      (pop-to-buffer (get-buffer-create "*Enriched Warnings*"))
      (goto-char (point-max))
      (insert 
;       (format "%s:%d: " (if (boundp 'enriched-file) enriched-file
;			   (buffer-file-name buf))
;	       line)
       (apply (function format) args)
       "\n")
      (pop-to-buffer buf))))

(defun enriched-looking-at-with-props (string)
  "True if text at point is equal to STRING, including text props.
This is a literal, not a regexp match.
The buffer text must include all text properties that STRING has, in
the same places, but it is allowed to have others that STRING lacks."
  (let ((buffer-string (buffer-substring (point) (+ (point) (length string)))))
    (and (string-equal string buffer-string)
	 (enriched-text-properties-include string buffer-string))))

(defun enriched-search-forward-with-props
  (string &optional bound noerror count)
  "Search forward for STRING, including its text properties.
Set point to end of occurrence found, and return point.
The match found must include all text properties that STRING has, in
the same places, but it is allowed to have others that STRING lacks.
An optional second argument bounds the search; it is a buffer position.
The match found must not extend after that position.  nil is equivalent
  to (point-max).
Optional third argument, if t, means if fail just return nil (no error).
  If not nil and not t, move to limit of search and return nil.
Optional fourth argument is repeat count--search for successive occurrences.
See also the functions `match-beginning', `match-end' and `replace-match'."
  (interactive "sSearch for: ")
  (or bound (setq bound (point-max)))
  (or count (setq count 1))
  (let ((start (point))
	(res t))
    (while (and res (> count 0))
      (while (and (setq res (search-forward string bound t))
		  (not (enriched-text-properties-include
			string (buffer-substring (match-beginning 0)
						 (match-end 0))))))
      (setq count (1- count)))
    (cond (res)
	  ((eq noerror t) (goto-char start) nil)
	  (noerror (goto-char bound) nil)
	  (t (goto-char start)
	     (error "Search failed: %s" string)))))

(defun enriched-search-backward-with-props 
  (string &optional bound noerror count)
  "Search backward for STRING, including its text properties.
Set point to the beginning of occurrence found, and return point.
The match found must include all text properties that STRING has, in
the same places, but it is allowed to have others that STRING lacks.
An optional second argument bounds the search; it is a buffer position.
The match found must not start before that position.  nil is equivalent
  to (point-min).
Optional third argument, if t, means if fail just return nil (no error).
  If not nil and not t, move to limit of search and return nil.
Optional fourth argument is repeat count--search for successive occurrences.
See also the functions `match-beginning', `match-end' and `replace-match'."
  (interactive "sSearch for: ")
  (or bound (setq bound (point-min)))
  (or count (setq count 1))
  (let ((start (point))
	(res t))
    (while (and res (> count 0))
      (while (and (setq res (search-backward string bound t))
		  (not (enriched-text-properties-include
			string (buffer-substring (match-beginning 0)
						 (match-end 0))))))
      (setq count (1- count)))
    (cond (res)
	  ((eq noerror t) (goto-char start) nil)
	  (noerror (goto-char bound) nil)
	  (t (goto-char start)
	     (error "Search failed: %s" string)))))

(defun enriched-text-properties-include (a b)
  "True if all of A's text-properties are also properties of B.
They must match in property name, value, and position.  B must be at least as
long as A, but comparison is done only up to the length of A."
  (let ((loc (length a)))
    (catch 'fail 
      (while (>= loc 0)
	(let ((plist (text-properties-at loc a)))
	  (while plist
	    (if (not (equal (car (cdr plist))
			    (get-text-property loc (car plist) b)))
		(throw 'fail nil))
	    (setq plist (cdr (cdr plist)))))
	(setq loc (1- loc)))
      t)))

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

(defun enriched-insert-annotations (list &optional offset)
  "Apply list of annotations to buffer as write-region would.
Inserts each element of LIST of buffer annotations at its appropriate place.
Use second arg OFFSET if the annotations' locations are not
relative to the beginning of the buffer: annotations will be inserted
at their location-OFFSET+1 \(ie, the offset is the character number of
the first character in the buffer)."
  (if (not offset) 
      (setq offset 0)
    (setq offset (1- offset)))
  (let ((l (reverse list)))
    (while l
      (goto-char (- (car (car l)) offset))
      (insert (cdr (car l)))
      (setq l (cdr l)))))

;;;
;;; Indentation, Filling, Justification
;;;

(defun enriched-text-width ()
  "The width of unindented text in this window, in characters.
This is the width of the window minus `enriched-default-right-margin'."
  (or enriched-text-width
      (let ((ww (window-width)))
	(setq enriched-text-width
	      (if (> ww enriched-default-right-margin)
		  (- ww enriched-default-right-margin)
		ww)))))

(defun enriched-tag-indentation (from to)
  "Define region to be indentation."
  (add-text-properties from to '(enriched-indentation t
				 rear-nonsticky (enriched-indentation))))

(defun enriched-insert-indentation (&optional from to)
  "Indent and justify each line in the region."
  (save-excursion
    (save-restriction
      (if to (narrow-to-region (point-min) to))
      (goto-char (or from (point-min)))
      (if (not (bolp)) (forward-line 1))
      (while (not (eobp))
	(indent-to (current-left-margin))
	(justify-current-line t nil t)
	(forward-line 1)))))

(defun enriched-delete-indentation (&optional from to)
  "Remove indentation and justification from region.
Does not alter the left-margin and right-margin text properties, so the
indentation can be reconstructed.  Tries only to remove whitespace that was
added automatically, not spaces and tabs inserted by user."
  (save-excursion
    (save-restriction
      (if to (narrow-to-region (point-min) to))
      (if from
	  (progn (goto-char from)
		 (if (not (bolp)) (forward-line 1))
		 (setq from (point)))
	(setq from (point-min)))
      (delete-to-left-margin from (point-max))
      (enriched-map-property-regions 'justification
	(lambda (v b e)
	  (if (eq v 'full)
	      (canonically-space-region b e)))
	from nil))))

;;;
;;; Writing Files
;;;

(defsubst enriched-open-annotation (name)
  (insert-and-inherit (enriched-make-annotation name t)))

(defsubst enriched-close-annotation (name)
  (insert-and-inherit (enriched-make-annotation name nil)))

(defun enriched-annotate-function (start end)
  "For use on write-region-annotations-functions.
Makes a new buffer containing the region in text/enriched format."
  (if enriched-mode
      (let (;(enriched-file (file-name-nondirectory buffer-file-name))
	    (copy-buf (generate-new-buffer "*Enriched Temp*")))
	(copy-to-buffer copy-buf start end)
	(set-buffer copy-buf)
	(enriched-insert-annotations write-region-annotations-so-far start)
	(setq write-region-annotations-so-far nil)
	(enriched-encode-region)))
  nil)

(defun enriched-encode-region (&optional from to)
  "Transform buffer into text/enriched format."
  (if enriched-verbose (message "Enriched: encoding document..."))
  (setq enriched-ignored-list enriched-ignored-ok)
  (save-excursion
    (save-restriction
      (if to (narrow-to-region (point-min) to))
      (enriched-delete-indentation from to)
      (let ((enriched-open-ans nil)
	    (inhibit-read-only t))
	(goto-char (or from (point-min)))
	(insert (if (stringp enriched-initial-annotation)
		    enriched-initial-annotation
		  (funcall enriched-initial-annotation)))
	(while 
	    (let* ((ans (enriched-loc-annotations (point)))
		   (neg-ans (enriched-reorder (car ans) enriched-open-ans))
		   (pos-ans (cdr ans)))
	      ;; First do the negative (closing) annotations
	      (while neg-ans
		(if (not (member (car neg-ans) enriched-open-ans))
		    (enriched-warn "BUG DETECTED: Closing %s with open list=%s"
				   (enriched-pop neg-ans) enriched-open-ans)
		  (while (not (equal (car neg-ans) (car enriched-open-ans)))
		    ;; To close anno. N, need to first close ans 1 to N-1,
		    ;; remembering to re-open them later.
		    (enriched-push (car enriched-open-ans) pos-ans)
		    (enriched-close-annotation (enriched-pop enriched-open-ans)))
		  ;; Now we can safely close this anno & remove from open list 
		  (enriched-close-annotation (enriched-pop neg-ans))
		  (enriched-pop enriched-open-ans)))
	      ;; Now deal with positive (opening) annotations
	      (while pos-ans
		(enriched-push (car pos-ans) enriched-open-ans)
		(enriched-open-annotation (enriched-pop pos-ans)))
	      (enriched-move-to-next-property-change)))

	;; Close up shop...
	(goto-char (point-max))
	(while enriched-open-ans
	  (enriched-close-annotation (enriched-pop enriched-open-ans)))
	(if (not (= ?\n (char-after (1- (point)))))
	    (insert ?\n)))
    (if (and enriched-verbose (> (length enriched-ignored-list)
				 (length enriched-ignored-ok)))
	(let ((not-ok nil))
	  (while (not (eq enriched-ignored-list enriched-ignored-ok))
	    (setq not-ok (cons (car enriched-ignored-list) not-ok)
		  enriched-ignored-list (cdr enriched-ignored-list)))
	  (enriched-warn "Not recorded: %s" not-ok)
	  (sit-for 1))))))

(defun enriched-move-to-next-property-change ()
  "Advance point to next prop change, dealing with special items on the way.
Returns the location, or nil."
  (let ((prop-change (next-property-change (point))))
    (while (and (< (point) (or prop-change (point-max)))
		(search-forward enriched-encode-interesting-regexp
				prop-change 1))
      (goto-char (match-beginning 0))
      (let ((specials enriched-encode-special-alist))
	(while specials
	  (if (enriched-looking-at-with-props (car (car specials)))
	      (progn (goto-char (match-end 0))
		     (funcall (cdr (car specials)))
		     (setq specials nil))
	    (enriched-pop specials)))))
    prop-change))

(defun enriched-loc-annotations (loc)
  "Return annotation(s) needed at LOCATION.
This includes any properties that change between LOC-1 and LOC.
If LOC is at the beginning of the buffer, will generate annotations for any
non-nil properties there, plus the enriched-version annotation.
   Annotations are returned as a list.  The car of the list is the list of
names of the annotations to close, and the cdr is the list of the names of the
annotations to open."
  (let* ((prev-loc (1- loc))
	 (begin (< prev-loc (point-min)))
	 (before-plist (if begin nil (text-properties-at prev-loc)))
	 (after-plist (text-properties-at loc))
	 negatives positives prop props)
    ;; make list of all property names involved
    (while before-plist
      (enriched-push (car before-plist) props)
      (setq before-plist (cdr (cdr before-plist))))
    (while after-plist
      (enriched-push (car after-plist) props)
      (setq after-plist (cdr (cdr after-plist))))
    (setq props (enriched-make-list-uniq props))

    (while props
      (setq prop (enriched-pop props))
      (if (memq prop enriched-ignored-list)
	  nil  ; If its been ignored before, ignore it now.
	(let ((before (if begin nil (get-text-property prev-loc prop)))
	      (after (get-text-property loc prop)))
	  (if (equal before after)
	      nil ; no change; ignore
	    (let ((result (enriched-annotate-change prop before after)))
	      (setq negatives (nconc negatives (car result))
		    positives (nconc positives (cdr result))))))))
    (cons negatives positives)))

(defun enriched-annotate-change (prop old new)
  "Return annotations for PROPERTY changing from OLD to NEW.
These are searched for in `enriched-annotation-list'.
If NEW does not appear in the list, but there is a default function, then that
function is called.
Annotations are returned as a list, as in `enriched-loc-annotations'."
  ;; If property is numeric, nil means 0
  (if (or (consp old) (consp new))
      (let* ((old (if (listp old) old (list old)))
	     (new (if (listp new) new (list new)))
	     (tail (enriched-common-tail old new))
	     close open)
	(while old
	  (setq close 
		(append (car (enriched-annotate-change prop (car old) nil))
			close)
		old (cdr old)))
	(while new
	  (setq open 
		(append (cdr (enriched-annotate-change prop nil (car new)))
			open)
		new (cdr new)))
	(enriched-make-relatively-unique close open))
    (cond ((and (numberp old) (null new))
	   (setq new 0))
	  ((and (numberp new) (null old))
	   (setq old 0)))
    (let ((prop-alist (cdr (assoc prop enriched-annotation-alist)))
	  default)
      (cond ((null prop-alist)		; not found
	     (if (not (memq prop enriched-ignored-list))
		 (enriched-push prop enriched-ignored-list))
	     nil)

	    ;; Numerical values: use the difference
	    ((and (numberp old) (numberp new))
	     (let* ((entry (progn
			     (while (and (car (car prop-alist))
					 (not (numberp (car (car prop-alist)))))
			       (enriched-pop prop-alist))
			     (car prop-alist)))
		    (increment (car (car prop-alist)))
		    (n (ceiling (/ (float (- new old)) (float increment))))
		    (anno (car (cdr (car prop-alist)))))
	       (if (> n 0)
		   (cons nil (make-list n anno))
		 (cons (make-list (- n) anno) nil))))

	    ;; Standard annotation
	    (t (let ((close (and old (cdr (assoc old prop-alist))))
		     (open  (and new (cdr (assoc new prop-alist)))))
		 (if (or close open)
		     (enriched-make-relatively-unique close open)
		   (let ((default (assoc nil prop-alist)))
		     (if default
			 (funcall (car (cdr default)) old new))))))))))

;;;
;;; Reading files
;;;

(defun enriched-decode-region (&optional from to)
  "Decode text/enriched buffer into text with properties.
This is the primary entry point for decoding."
  (if enriched-verbose (message "Enriched: decoding document..."))
  (save-excursion
    (save-restriction
      (if to (narrow-to-region (point-min) to))
      (goto-char (or from (point-min)))
      (let ((file-width (enriched-get-file-width))
	    (inhibit-read-only t)
	    enriched-open-ans todo loc unknown-ans)

	(while (enriched-move-to-next-annotation)
	  (let* ((loc (match-beginning 0))
		 (anno (buffer-substring (match-beginning 0) (match-end 0)))
		 (name (enriched-annotation-name anno))
		 (positive (enriched-annotation-positive-p anno)))

	    (if enriched-downcase-annotations
		(setq name (downcase name)))

	    (delete-region (match-beginning 0) (match-end 0))
	    (if positive
		(enriched-push (list name loc) enriched-open-ans)
	      ;; negative...
	      (let* ((top (car enriched-open-ans))
		     (top-name (car top))
		     (start (car (cdr top)))
		     (params (cdr (cdr top)))
		     (aalist enriched-annotation-alist)
		     (matched nil))
		(if (not (equal name top-name))
		    (error (format "Improper nesting in file: %s != %s"
				   name top)))
		(while aalist
		  (let ((prop (car (car aalist)))
			(alist (cdr (car aalist))))
		    (while alist
		      (let ((value (car (car alist)))
			    (ans (cdr (car alist))))
			(if (member name ans)
			    ;; Check if multiple annotations are satisfied
			    (if (member 'nil (mapcar 
					      (lambda (r)
						(assoc r enriched-open-ans))
					      ans))
				nil	; multiple ans not satisfied
			      ;; Yes, we got it:
			      (setq alist nil aalist nil matched t
				    enriched-open-ans (cdr enriched-open-ans))
			      (cond 
			       ((eq prop 'PARAMETER)
				;; This is a parameter of the top open ann.
				(let ((nxt (enriched-pop enriched-open-ans)))
				  (if nxt
				      (enriched-push
				       (append 
					nxt 
					(list (buffer-substring start loc)))
				       enriched-open-ans))
				  (delete-region start loc)))
			       ((eq prop 'FUNCTION)
				(let ((rtn (apply value start loc params)))
				  (if rtn (enriched-push rtn todo))))
			       (t 
				;; Normal property/value pair
				(enriched-push (list start loc prop value)
					       todo))))))
			(enriched-pop alist)))
		  (enriched-pop aalist))
		(if matched
		    nil
		  ;; Didn't find it
		  (enriched-pop enriched-open-ans)
		  (enriched-push (list start loc 'unknown name) todo)
		  (enriched-push name unknown-ans))))))

	;; Now actually add the properties

	(while todo
	  (let* ((item (enriched-pop todo))
		 (from (elt item 0))
		 (to   (elt item 1))
		 (prop (elt item 2))
		 (val  (elt item 3)))
	
;	    (if (and (eq prop 'IGNORE)	; 'IGNORE' pseudo-property was special
;		     (eq val t))
;		(delete-region from to))
	    (put-text-property 
	       from to prop
	       (cond ((numberp val)
		      (+ val (or (get-text-property from prop) 0)))
		     ((memq prop enriched-list-valued-properties)
		      (let ((prev (get-text-property from prop)))
			(cons val (if (listp prev) prev (list prev)))))
		     (t val)))))
    
	(if (or (and file-width		; possible reasons not to fill:
		     (= file-width (enriched-text-width)))  ; correct wd.
		(null enriched-fill-after-visiting)         ; never fill
		(and (eq 'ask enriched-fill-after-visiting) ; asked & declined
		     (not (y-or-n-p "Reformat for current display width? "))))
	    ;; Minimally, we have to insert indentation and justification.
	    (enriched-insert-indentation)
	  (sit-for 1)
	  (if enriched-verbose (message "Filling paragraphs..."))
	  (fill-region (point-min) (point-max))
	  (if enriched-verbose (message nil)))
    
	(if enriched-verbose 
	    (progn
	      (message nil)
	      (if unknown-ans
		  (enriched-warn "Unknown annotations: %s" unknown-ans))))))))

(defun enriched-get-file-width ()
  "Look for file width information on this line."
  (save-excursion
    (if (search-forward "width:" (save-excursion (end-of-line) (point)) t)
	(read (current-buffer)))))

(defun enriched-move-to-next-annotation ()
  "Advances point to next annotation, dealing with special items on the way.
Returns t if one was found, otherwise nil."
  (while (and (re-search-forward enriched-decode-interesting-regexp nil t)
	      (goto-char (match-beginning 0))
	      (not (looking-at enriched-annotation-regexp)))
      (let ((regexps enriched-decode-special-alist))
	(while (and regexps
		    (not (looking-at (car (car regexps)))))
	  (enriched-pop regexps))
	(if regexps
	    (funcall (cdr (car regexps)))
	  (forward-char 1)))) ; nothing found
  (not (eobp)))

;;; enriched.el ends here
