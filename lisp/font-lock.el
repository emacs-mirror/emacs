;; Electric Font Lock Mode
;; Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.

;; Author: jwz, then rms and sm (simon.marshall@mail.esrin.esa.it)
;; Maintainer: FSF
;; Keywords: languages, faces

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:

;; Font Lock mode is a minor mode that causes your comments to be displayed in
;; one face, strings in another, reserved words in another, and so on.
;;
;; Comments will be displayed in `font-lock-comment-face'.
;; Strings will be displayed in `font-lock-string-face'.
;; Regexps are used to display selected patterns in other faces.
;;
;; To make the text you type be fontified, use M-x font-lock-mode.
;; When this minor mode is on, the faces of the current line are
;; updated with every insertion or deletion.
;;
;; To turn Font Lock mode on automatically, add this to your .emacs file:
;;
;;  (add-hook 'emacs-lisp-mode-hook 'turn-on-font-lock)
;;
;; On a Sparc2, `font-lock-fontify-buffer' takes about 10 seconds for a 120k
;; file of C code using the default configuration, and about 25 seconds using
;; the more extensive configuration, though times also depend on file contents.
;; You can speed this up substantially by removing some of the patterns that
;; are highlighted by default.  Fontifying Lisp code is significantly faster,
;; because Lisp has a more regular syntax than C, so the expressions don't have
;; to be as hairy.
;;
;; If you add patterns for a new mode, say foo.el's `foo-mode', say in which
;; you don't want syntactic fontification to occur, you can make Font Lock mode
;; use your regexps when turning on Font Lock by adding to `foo-mode-hook':
;;
;;  (add-hook 'foo-mode-hook
;;   '(lambda () (make-local-variable 'font-lock-defaults)
;;               (setq font-lock-defaults '(foo-font-lock-keywords t))))
;;
;; Nasty regexps of the form "bar\\(\\|lo\\)\\|f\\(oo\\|u\\(\\|bar\\)\\)\\|lo"
;; are made thusly: (make-regexp '("foo" "fu" "fubar" "bar" "barlo" "lo")) for
;; efficiency.  See /pub/gnu/emacs/elisp-archive/functions/make-regexp.el.Z on
;; archive.cis.ohio-state.edu for this and other functions.

;;; Code:

(defvar font-lock-comment-face 'font-lock-comment-face
  "Face to use for comments.")

(defvar font-lock-string-face 'font-lock-string-face
  "Face to use for strings.")

(defvar font-lock-function-name-face 'font-lock-function-name-face
  "Face to use for function names.")

(defvar font-lock-variable-name-face 'font-lock-variable-name-face
  "Face to use for variable names.")

(defvar font-lock-keyword-face 'font-lock-keyword-face
  "Face to use for keywords.")

(defvar font-lock-type-face 'font-lock-type-face
  "Face to use for data types.")

(defvar font-lock-reference-face 'font-lock-reference-face
  "Face to use for references.")

(defvar font-lock-no-comments nil
  "Non-nil means Font Lock should not fontify comments or strings.")

(make-variable-buffer-local 'font-lock-keywords)
(defvar font-lock-keywords nil
  "*The keywords to highlight.
Elements should be of the form:

 REGEXP
 (REGEXP . MATCH)
 (REGEXP . FACENAME)
 (REGEXP . HIGHLIGHT)
 (REGEXP HIGHLIGHT ...)

where HIGHLIGHT should be of the form (MATCH FACENAME OVERRIDE LAXMATCH).
REGEXP is the regexp to search for, MATCH is the subexpression of REGEXP to be
highlighted, FACENAME is an expression whose value is the face name to use.
FACENAME's default attributes may be defined in `font-lock-face-attributes'.

OVERRIDE and LAXMATCH are flags.  If OVERRIDE is t, existing fontification may
be overriden.  If `keep', only parts not already fontified are highlighted.
If LAXMATCH is non-nil, no error is signalled if there is no MATCH in REGEXP.

These regular expressions should not match text which spans lines.  While
\\[font-lock-fontify-buffer] handles multi-line patterns correctly, updating
when you edit the buffer does not, since it considers text one line at a time.

Be careful composing regexps for this list;
the wrong pattern can dramatically slow things down!")

(defvar font-lock-defaults nil
  "If set by a major mode, should be the defaults for Font Lock mode.
The value should look like the `cdr' of an item in `font-lock-defaults-alist'.")

(defvar font-lock-defaults-alist
  '((bibtex-mode .		(tex-font-lock-keywords))
    (c++-c-mode .		(c-font-lock-keywords nil nil ((?\_ . "w"))))
    (c++-mode .			(c++-font-lock-keywords nil nil ((?\_ . "w"))))
    (c-mode .			(c-font-lock-keywords nil nil ((?\_ . "w"))))
    (emacs-lisp-mode .		(lisp-font-lock-keywords))
    (latex-mode .		(tex-font-lock-keywords))
    (lisp-mode .		(lisp-font-lock-keywords))
    (plain-tex-mode .		(tex-font-lock-keywords))
    (scheme-mode .		(lisp-font-lock-keywords))
    (slitex-mode .		(tex-font-lock-keywords))
    (tex-mode .			(tex-font-lock-keywords)))
  "*Alist of default major mode and Font Lock defaults.
Each item should be a list of the form:
 (MAJOR-MODE . (FONT-LOCK-KEYWORDS KEYWORDS-ONLY CASE-FOLD FONT-LOCK-SYNTAX))
where both MAJOR-MODE and FONT-LOCK-KEYWORDS are symbols.  If KEYWORDS-ONLY is
non-nil, syntactic fontification (strings and comments) is not performed.
If CASE-FOLD is non-nil, the case of the keywords is ignored when fontifying.
FONT-LOCK-SYNTAX should be a list of cons pairs of the form (CHAR . STRING), it
is used to set the local Font Lock syntax table for keyword fontification.")

(defvar font-lock-maximum-size (* 100 1024)
  "*If non-nil, the maximum size for buffers.
Only buffers less than are fontified when Font Lock mode is turned on.
If nil, means size is irrelevant.")

(defvar font-lock-keywords-case-fold-search nil
  "*Non-nil means the patterns in `font-lock-keywords' are case-insensitive.")

(defvar font-lock-syntax-table nil
  "*Non-nil means use this syntax table for fontifying.
If this is nil, the major mode's syntax table is used.")

(defvar font-lock-verbose t
  "*Non-nil means `font-lock-fontify-buffer' should print status messages.")

;;;###autoload
(defvar font-lock-mode-hook nil
  "Function or functions to run on entry to Font Lock mode.")

;; Colour etc. support.

(defvar font-lock-display-type nil
  "A symbol indicating the display Emacs is running under.
The symbol should be one of `color', `grayscale' or `mono'.
If Emacs guesses this display attribute wrongly, either set this variable in
your `~/.emacs' or set the resource `Emacs.displayType' in your `~/.Xdefaults'.
See also `font-lock-background-mode' and `font-lock-face-attributes'.")

(defvar font-lock-background-mode nil
  "A symbol indicating the Emacs background brightness.
The symbol should be one of `light' or `dark'.
If Emacs guesses this frame attribute wrongly, either set this variable in
your `~/.emacs' or set the resource `Emacs.backgroundMode' in your
`~/.Xdefaults'.
See also `font-lock-display-type' and `font-lock-face-attributes'.")

(defvar font-lock-face-attributes nil
  "A list of default attributes to use for face attributes.
Each element of the list should be of the form

 (FACE FOREGROUND BACKGROUND BOLD-P ITALIC-P UNDERLINE-P)

where FACE should be one of the face symbols `font-lock-comment-face',
`font-lock-string-face', `font-lock-keyword-face', `font-lock-type-face',
`font-lock-function-name-face', `font-lock-variable-name-face', and
`font-lock-reference-face'.  A form for each of these face symbols should be
provided in the list, but other face symbols and attributes may be given and
used in highlighting.  See `font-lock-keywords'.

Subsequent element items should be the attributes for the corresponding
Font Lock mode faces.  Attributes FOREGROUND and BACKGROUND should be strings
\(default if nil), while BOLD-P, ITALIC-P, and UNDERLINE-P should specify the
corresponding face attributes (yes if non-nil).

Emacs uses default attributes based on display type and background brightness.
See variables `font-lock-display-type' and `font-lock-background-mode'.

Resources can be used to over-ride these face attributes.  For example, the
resource `Emacs.font-lock-comment-face.attributeUnderline' can be used to
specify the UNDERLINE-P attribute for face `font-lock-comment-face'.")

(defun font-lock-make-faces ()
  "Make faces from `font-lock-face-attributes'.
A default list is used if this is nil.
See `font-lock-make-face' and `list-faces-display'."
  ;; We don't need to `setq' any of these variables, but the user can see what
  ;; is being used if we do.
  (if (null font-lock-display-type)
      (setq font-lock-display-type
	    (let ((display-resource (x-get-resource ".displayType"
						    "DisplayType")))
	      (cond (display-resource (intern (downcase display-resource)))
		    ((x-display-color-p) 'color)
		    ((x-display-grayscale-p) 'grayscale)
		    (t 'mono)))))
  (if (null font-lock-background-mode)
      (setq font-lock-background-mode
	    (let ((bg-resource (x-get-resource ".backgroundMode"
					       "BackgroundMode"))
		  (params (frame-parameters)))
	      (cond (bg-resource (intern (downcase bg-resource)))
		    ((or (string-equal "white"
			  (downcase (cdr (assq 'foreground-color params))))
			 (string-equal "black"
			  (downcase (cdr (assq 'background-color params)))))
		     'dark)
		    (t 'light)))))
  (if (null font-lock-face-attributes)
      (setq font-lock-face-attributes
	    (let ((light-bg (eq font-lock-background-mode 'light)))
	      (cond ((memq font-lock-display-type '(mono monochrome))
		     ;; Emacs 19.25's font-lock defaults:
		     ;;'((font-lock-comment-face nil nil nil t nil)
		     ;;  (font-lock-string-face nil nil nil nil t)
		     ;;  (font-lock-keyword-face nil nil t nil nil)
		     ;;  (font-lock-function-name-face nil nil t t nil)
		     ;;  (font-lock-type-face nil nil nil t nil))
		     (list '(font-lock-comment-face nil nil t t nil)
			   '(font-lock-string-face nil nil nil t nil)
			   '(font-lock-keyword-face nil nil t nil nil)
			   (list
			    'font-lock-function-name-face
			    (cdr (assq 'background-color (frame-parameters)))
			    (cdr (assq 'foreground-color (frame-parameters)))
			    t nil nil)
			   '(font-lock-variable-name-face nil nil t t nil)
			   '(font-lock-type-face nil nil t nil t)
			   '(font-lock-reference-face nil nil t nil t)))
		    ((memq font-lock-display-type '(grayscale greyscale
						    grayshade greyshade))
		     (list
		      (list 'font-lock-comment-face
			    (if light-bg "DimGray" "Gray80") nil t t nil)
		      (list 'font-lock-string-face
			    (if light-bg "Gray50" "LightGray") nil nil t nil)
		      (list 'font-lock-keyword-face
			    (if light-bg "DimGray" "Gray90") nil t nil nil)
		      (list 'font-lock-function-name-face
			    (cdr (assq 'background-color (frame-parameters)))
			    (cdr (assq 'foreground-color (frame-parameters)))
			    t nil nil)
		      (list 'font-lock-variable-name-face
			    (if light-bg "DimGray" "Gray90") nil t t nil)
		      (list 'font-lock-type-face
			    (if light-bg "DimGray" "Gray80") nil t nil t)
		      (list 'font-lock-reference-face
			    (if light-bg "Gray50" "LightGray") nil t nil t)))
		    (light-bg		; light colour background
		     '((font-lock-comment-face "Firebrick")
		       (font-lock-string-face "RosyBrown")
		       (font-lock-keyword-face "Purple")
		       (font-lock-function-name-face "Blue")
		       (font-lock-variable-name-face "DarkGoldenrod")
		       (font-lock-type-face "DarkOliveGreen")
		       (font-lock-reference-face "CadetBlue")))
		    (t			; dark colour background
		     '((font-lock-comment-face "OrangeRed")
		       (font-lock-string-face "LightSalmon")
		       (font-lock-keyword-face "LightSteelBlue")
		       (font-lock-function-name-face "LightSkyBlue")
		       (font-lock-variable-name-face "LightGoldenrod")
		       (font-lock-type-face "PaleGreen")
		       (font-lock-reference-face "Aquamarine")))))))
  (mapcar 'font-lock-make-face font-lock-face-attributes))

(defun font-lock-make-face (face-attributes)
  "Make a face from FACE-ATTRIBUTES.
FACE-ATTRIBUTES should be like an element `font-lock-face-attributes', so that
the face name is the first item in the list.  A variable with the same name as
the face is also set; its value is the face name."
  (let* ((face (nth 0 face-attributes))
	 (face-name (symbol-name face))
	 (set-p (function (lambda (face-name resource)
		 (x-get-resource (concat face-name ".attribute" resource)
				 (concat "Face.Attribute" resource)))))
	 (on-p (function (lambda (face-name resource)
		(let ((set (funcall set-p face-name resource)))
		  (and set (member (downcase set) '("on" "true"))))))))
    (make-face face)
    ;; Set attributes not set from X resources (and therefore `make-face').
    (or (funcall set-p face-name "Foreground")
	(condition-case nil
	    (set-face-foreground face (nth 1 face-attributes))
	  (error nil)))
    (or (funcall set-p face-name "Background")
	(condition-case nil
	    (set-face-background face (nth 2 face-attributes))
	  (error nil)))
    (if (funcall set-p face-name "Bold")
	(and (funcall on-p face-name "Bold") (make-face-bold face nil t))
      (and (nth 3 face-attributes) (make-face-bold face nil t)))
    (if (funcall set-p face-name "Italic")
	(and (funcall on-p face-name "Italic") (make-face-italic face nil t))
      (and (nth 4 face-attributes) (make-face-italic face nil t)))
    (or (funcall set-p face-name "Underline")
	(set-face-underline-p face (nth 5 face-attributes)))
    (set face face)))

;; Fontification.

;; These variables record, for each buffer, the parse state at a particular
;; position, always the start of a line.  Used to make font-lock-fontify-region
;; faster.
(defvar font-lock-cache-position nil)
(defvar font-lock-cache-state nil)
(make-variable-buffer-local 'font-lock-cache-position)
(make-variable-buffer-local 'font-lock-cache-state)

(defun font-lock-fontify-region (start end &optional loudly)
  "Put proper face on each string and comment between START and END."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char start)
      (beginning-of-line)
      (if loudly (message "Fontifying %s... (syntactically...)" (buffer-name)))
      (let ((buffer-read-only nil)
	    (modified (buffer-modified-p))
	    (cstart (if comment-start-skip
			(concat "\\s\"\\|" comment-start-skip)
		      "\\s\""))
	    (cend (if comment-end
		      (concat "\\s>\\|"
			      (regexp-quote
			       ;; Discard leading spaces from comment-end.
			       ;; In C mode, it is " */"
			       ;; and we don't want to fail to notice a */
			       ;; just because there's no space there.
			       (save-match-data
				 (if (string-match "^ +" comment-end)
				     (substring comment-end (match-end 0))
				   comment-end))))
		    "\\s>"))
	    (startline (point))
	    state prev prevstate)
	;; Find the state at the line-beginning before START.
	(if (eq startline font-lock-cache-position)
	    (setq state font-lock-cache-state)
	  ;; Find outermost containing sexp.
	  (beginning-of-defun)
	  ;; Find the state at STARTLINE.
	  (while (< (point) startline)
	    (setq state (parse-partial-sexp (point) startline 0)))
	  (setq font-lock-cache-state state
		font-lock-cache-position (point)))
	;; Now find the state precisely at START.
	(setq state (parse-partial-sexp (point) start nil nil state))
	;; If the region starts inside a string, show the extent of it.
	(if (nth 3 state)
	    (let ((beg (point)))
	      (while (and (re-search-forward "\\s\"" end 'move)
			  (nth 3 (parse-partial-sexp beg (point) nil nil
						     state))))
	      (put-text-property beg (point) 'face font-lock-string-face)
	      (setq state (parse-partial-sexp beg (point) nil nil state))))
	;; Likewise for a comment.
	(if (or (nth 4 state) (nth 7 state))
	    (let ((beg (point)))
	      (while (and (re-search-forward cend end 'move)
			  (nth 3 (parse-partial-sexp beg (point) nil nil
						     state))))
	      (put-text-property beg (point) 'face font-lock-comment-face)
	      (setq state (parse-partial-sexp beg (point) nil nil state))))
	;; Find each interesting place between here and END.
	(while (and (< (point) end)
		    (setq prev (point) prevstate state)
		    (re-search-forward cstart end t)
		    (progn
		      ;; Clear out the fonts of what we skip over.
		      (remove-text-properties prev (point) '(face nil))
		      ;; Verify the state at that place
		      ;; so we don't get fooled by \" or \;.
		      (setq state (parse-partial-sexp prev (point) nil nil
						      state))))
	  (let ((here (point)))
	    (if (or (nth 4 state) (nth 7 state))
		;; We found a real comment start.
		(let ((beg (match-beginning 0)))
		  (goto-char beg)
		  (save-restriction
		    (narrow-to-region (point-min) end)
		    (condition-case nil
			(progn
			  (forward-comment 1)
			  ;; forward-comment skips all whitespace,
			  ;; so go back to the real end of the comment.
			  (skip-chars-backward " \t"))
		      (error (goto-char end))))
		  (put-text-property beg (point) 'face font-lock-comment-face)
		  (setq state (parse-partial-sexp here (point) nil nil state)))
	      (if (nth 3 state)
		  (let ((beg (match-beginning 0)))
		    (while (and (re-search-forward "\\s\"" end 'move)
				(nth 3 (parse-partial-sexp here (point) nil nil
							   state))))
		    (put-text-property beg (point) 'face font-lock-string-face)
		    (setq state (parse-partial-sexp here (point) nil nil
						    state))))))
	  ;; Make sure PREV is non-nil after the loop
	  ;; only if it was set on the very last iteration.
	  (setq prev nil))
	(and prev
	     (remove-text-properties prev end '(face nil)))
	(and (buffer-modified-p)
	     (not modified)
	     (set-buffer-modified-p nil))))))

;; This code used to be used to show a string on reaching the end of it.
;; It is probably not needed due to later changes to handle strings
;; starting before the region in question.
;;	    (if (and (null (nth 3 state))
;;		     (eq (char-syntax (preceding-char)) ?\")
;;		     (save-excursion
;;		       (nth 3 (parse-partial-sexp prev (1- (point))
;;						  nil nil prevstate))))
;;		;; We found the end of a string.
;;		(save-excursion
;;		  (setq foo2 (point))
;;		  (let ((ept (point)))
;;		    (forward-sexp -1)
;;		    ;; Highlight the string when we see the end.
;;		    ;; Doing it at the start leads to trouble:
;;		    ;; either it fails to handle multiline strings
;;		    ;; or it can run away when an unmatched " is inserted.
;;		    (put-text-property (point) ept 'face
;;				       (if (= (car state) 1)
;;					   font-lock-doc-string-face
;;					 font-lock-string-face)))))

(defun font-lock-unfontify-region (beg end)
  (let ((modified (buffer-modified-p))
	(buffer-read-only nil))
    (remove-text-properties beg end '(face nil))
    (set-buffer-modified-p modified)))

;; Called when any modification is made to buffer text.
(defun font-lock-after-change-function (beg end old-len)
  (save-excursion
    (save-match-data
      ;; Discard the cache info if text before it has changed.
      (and font-lock-cache-position
	   (> font-lock-cache-position beg)
	   (setq font-lock-cache-position nil))
      ;; Rescan between start of line from `beg' and start of line after `end'.
      (goto-char beg)
      (beginning-of-line)
      (setq beg (point))
      (goto-char end)
      (forward-line 1)
      (setq end (point))
      ;; First scan for strings and comments.
      ;; Must scan from line start in case of
      ;; inserting space into `intfoo () {}', and after widened.
      (if font-lock-no-comments
	  (remove-text-properties beg end '(face nil))
	(font-lock-fontify-region beg end))
      (font-lock-hack-keywords beg end))))

;      ;; Now scan for keywords, but not if we are inside a comment now.
;      (or (and (not font-lock-no-comments)
;	       (let ((state (parse-partial-sexp beg end nil nil 
;						font-lock-cache-state)))
;		 (or (nth 4 state) (nth 7 state))))
;	  (font-lock-hack-keywords beg end))

;;; Fontifying arbitrary patterns

(defun font-lock-hack-keywords (start end &optional loudly)
  "Fontify according to `font-lock-keywords' between START and END."
  (let ((case-fold-search font-lock-keywords-case-fold-search)
	(keywords font-lock-keywords)
	(count 0)
	(buffer-read-only nil)
	(modified (buffer-modified-p))
	(old-syntax (syntax-table))
	(bufname (buffer-name)))
    (unwind-protect
	(let (keyword regexp match highlights hs h s e)
	  (if loudly (message "Fontifying %s... (regexps...)" bufname))
	  (if font-lock-syntax-table (set-syntax-table font-lock-syntax-table))
	  (while keywords
	    (setq keyword (car keywords) keywords (cdr keywords)
		  regexp (if (stringp keyword) keyword (car keyword))
		  highlights (cond ((stringp keyword)
				    '((0 font-lock-keyword-face)))
				   ((numberp (cdr keyword))
				    (list (list (cdr keyword)
						'font-lock-keyword-face)))
				   ((symbolp (cdr keyword))
				    (list (list 0 (cdr keyword))))
				   ((nlistp (nth 1 keyword))
				    (list (cdr keyword)))
				   (t
				    (cdr keyword))))
	    (goto-char start)
	    (while (re-search-forward regexp end t)
	      (setq hs highlights)
	      (while hs
		(setq h (car hs) match (nth 0 h)
		      s (match-beginning match) e (match-end match)
		      hs (cdr hs))
		(cond ((not s)
		       ;; No match but we might not signal an error
		       (or (nth 3 h)
			   (error "No subexpression %d in expression %d"
				  match (1+ count))))
		      ((and (not (nth 2 h))
			    (text-property-not-all s e 'face nil))
		       ;; Can't override and already fontified
		       nil)
		      ((not (eq (nth 2 h) 'keep))
		       ;; Can override but need not keep existing fontification
		       (put-text-property s e 'face (eval (nth 1 h))))
		      (t
		       ;; Can override but must keep existing fontification
		       ;; (Does anyone use this?  sm.)
		       (let ((p (text-property-any s e 'face nil)) n
			     (face (eval (nth 1 h))))
			 (while p
			   (setq n (next-single-property-change p 'face nil e))
			   (put-text-property p n 'face face)
			   (setq p (text-property-any n e 'face nil))))))))
;; the above form was:
;		    (save-excursion
;		      (goto-char s)
;		      (while (< (point) e)
;			(let ((next (next-single-property-change (point) 'face
;								 nil e)))
;			  (if (or (null next) (> next e))
;			      (setq next e))
;			  (if (not (get-text-property (point) 'face))
;			      (put-text-property (point) next 'face face))
;			  (goto-char next))))

	    (if loudly (message "Fontifying %s... (regexps...%s)" bufname
				(make-string (setq count (1+ count)) ?.)))))
      (set-syntax-table old-syntax))
    (and (buffer-modified-p)
	 (not modified)
	 (set-buffer-modified-p nil))))

;; The user level functions

(defvar font-lock-mode nil) ; for modeline

(defvar font-lock-fontified nil) ; whether we have hacked this buffer
(put 'font-lock-fontified 'permanent-local t)

;;;###autoload
(defun font-lock-mode (&optional arg)
  "Toggle Font Lock mode.
With arg, turn Font Lock mode on if and only if arg is positive.

When Font Lock mode is enabled, text is fontified as you type it:

 - Comments are displayed in `font-lock-comment-face';
 - Strings are displayed in `font-lock-string-face';
 - Certain other expressions are displayed in other faces according to the
   value of the variable `font-lock-keywords'.

You can enable Font Lock mode in any major mode automatically by turning on in
the major mode's hook.  For example, put in your ~/.emacs:

 (add-hook 'c-mode-hook 'turn-on-font-lock)

Or for any visited file with the following in your ~/.emacs:

 (add-hook 'find-file-hooks 'turn-on-font-lock)

The default Font Lock mode faces and their attributes are defined in the
variable `font-lock-face-attributes', and Font Lock mode default settings in
the variable `font-lock-defaults-alist'.

When you turn Font Lock mode on/off the buffer is fontified/defontified, though
fontification occurs only if the buffer is less than `font-lock-maximum-size'.
To fontify a buffer without turning on Font Lock mode, and regardless of buffer
size, you can use \\[font-lock-fontify-buffer]."
  (interactive "P")
  (let ((on-p (if arg (> (prefix-numeric-value arg) 0) (not font-lock-mode))))
    (if (equal (buffer-name) " *Compiler Input*") ; hack for bytecomp...
	(setq on-p nil))
    (if (not on-p)
	(remove-hook 'after-change-functions 'font-lock-after-change-function)
      (make-local-variable 'after-change-functions)
      (add-hook 'after-change-functions 'font-lock-after-change-function))
    (set (make-local-variable 'font-lock-mode) on-p)
    (cond (on-p
	   (font-lock-set-defaults)
	   (make-local-variable 'before-revert-hook)
	   (make-local-variable 'after-revert-hook)
	   ;; If buffer is reverted, must clean up the state.
	   (add-hook 'before-revert-hook 'font-lock-revert-setup)
	   (add-hook 'after-revert-hook 'font-lock-revert-cleanup)
	   (run-hooks 'font-lock-mode-hook)
	   (cond (font-lock-fontified
		  nil)
		 ((or (null font-lock-maximum-size)
		      (> font-lock-maximum-size (buffer-size)))
		  (font-lock-fontify-buffer))
		 (font-lock-verbose
		  (message "Fontifying %s... buffer too big." (buffer-name)))))
	  (font-lock-fontified
	   (setq font-lock-fontified nil)
	   (remove-hook 'before-revert-hook 'font-lock-revert-setup)
	   (remove-hook 'after-revert-hook 'font-lock-revert-cleanup)
	   (font-lock-unfontify-region (point-min) (point-max))))
    (force-mode-line-update)))

;;;###autoload
(defun turn-on-font-lock ()
  "Unconditionally turn on Font Lock mode."
  (font-lock-mode 1))

;; If the buffer is about to be reverted, it won't be fontified.
(defun font-lock-revert-setup ()
  (setq font-lock-fontified nil))

;; If the buffer has just been reverted, we might not even be in font-lock
;; mode anymore, and if we are, the buffer may or may not have already been
;; refontified.  Refontify here if it looks like we need to.
(defun font-lock-revert-cleanup ()
  (and font-lock-mode
       (not font-lock-fontified)
       (font-lock-mode 1)))

;;;###autoload
(defun font-lock-fontify-buffer ()
  "Fontify the current buffer the way `font-lock-mode' would."
  (interactive)
  (let ((was-on font-lock-mode)
	(verbose (or font-lock-verbose (interactive-p)))
	(modified (buffer-modified-p)))
    (set (make-local-variable 'font-lock-fontified) nil)
    (if verbose (message "Fontifying %s..." (buffer-name)))
    ;; Turn it on to run hooks and get the right `font-lock-keywords' etc.
    (or was-on (font-lock-set-defaults))
    (condition-case nil
	(save-excursion
	  (font-lock-unfontify-region (point-min) (point-max))
	  (if (not font-lock-no-comments)
	      (font-lock-fontify-region (point-min) (point-max) verbose))
	  (font-lock-hack-keywords (point-min) (point-max) verbose)
	  (setq font-lock-fontified t))
      ;; We don't restore the old fontification, so it's best to unfontify.
      (quit (font-lock-unfontify-region (point-min) (point-max))))
    (if verbose (message "Fontifying %s... %s." (buffer-name)
			 (if font-lock-fontified "done" "aborted")))
    (and (buffer-modified-p)
	 (not modified)
	 (set-buffer-modified-p nil))))


;;; Various information shared by several modes.
;;; Information specific to a single mode should go in its load library.

(defconst lisp-font-lock-keywords-1
  (list
   ;; highlight defining forms.  This doesn't work too nicely for
   ;; (defun (setf foo) ...) but it does work for (defvar foo) which
   ;; is more important.
   (list (concat "^(\\(def\\(const\\|ine-key\\(\\|-after\\)\\|var\\)\\)\\>"
		 "\\s *\\([^ \t\n\)]+\\)?")
	 '(1 font-lock-keyword-face) '(4 font-lock-variable-name-face nil t))
   (list (concat "^(\\(def\\(a\\(dvice\\|lias\\)\\|macro\\|subst\\|un\\)\\)\\>"
		 "\\s *\\([^ \t\n\)]+\\)?")
	 '(1 font-lock-keyword-face) '(4 font-lock-function-name-face nil t))
   ;;
   ;; this is highlights things like (def* (setf foo) (bar baz)), but may
   ;; be slower (I haven't really thought about it)
;   ("^(def[-a-z]+\\s +\\(\\s(\\S)*\\s)\\|\\S(\\S *\\)"
;    1 font-lock-function-name-face)
   )
 "For consideration as a value of `lisp-font-lock-keywords'.
This does fairly subdued highlighting.")

(defconst lisp-font-lock-keywords-2
  (append
   lisp-font-lock-keywords-1
   (list
    ;;
    ;; Control structures.
    ;; ELisp:
;    ("cond" "if" "while" "let\\*?" "prog[nv12*]?" "catch" "throw"
;     "save-restriction" "save-excursion"
;     "save-window-excursion" "save-match-data" "unwind-protect"
;     "condition-case" "track-mouse")
    (cons
     (concat "(\\("
      "c\\(atch\\|ond\\(\\|ition-case\\)\\)\\|if\\|let\\*?\\|prog[nv12*]?\\|"
      "save-\\(excursion\\|match-data\\|restriction\\|window-excursion\\)\\|"
      "t\\(hrow\\|rack-mouse\\)\\|unwind-protect\\|while"
      "\\)[ \t\n]") 1)
    ;; CLisp:
;    ("when" "unless" "do" "flet" "labels" "return" "return-from")
    '("(\\(do\\|flet\\|labels\\|return\\(\\|-from\\)\\|unless\\|when\\)[ \t\n]"
      . 1)
    ;;
    ;; Fontify CLisp keywords.
    '("\\s :\\([-a-zA-Z0-9]+\\)\\>" . 1)
    ;;
    ;; Function names in emacs-lisp docstrings (in the syntax that
    ;; substitute-command-keys understands.)
    '("\\\\\\\\\\[\\([^]\\\n]+\\)]" 1 font-lock-reference-face t)
    ;;
    ;; Words inside `' which tend to be function names
    (let ((word-char "[-+a-zA-Z0-9_:*]"))
      (list (concat "`\\(" word-char word-char "+\\)'")
	    1 'font-lock-reference-face t))
    ;;
    ;; & keywords as types
    '("\\&\\(optional\\|rest\\)\\>" . font-lock-type-face)
    ))
 "For consideration as a value of `lisp-font-lock-keywords'.
This does a lot more highlighting.")

;; default to the gaudier variety?
;(defvar lisp-font-lock-keywords lisp-font-lock-keywords-2
;  "Additional expressions to highlight in Lisp modes.")
(defvar lisp-font-lock-keywords lisp-font-lock-keywords-1
  "Additional expressions to highlight in Lisp modes.")


(defconst c-font-lock-keywords-1 nil
  "For consideration as a value of `c-font-lock-keywords'.
This does fairly subdued highlighting.")

(defconst c-font-lock-keywords-2 nil
  "For consideration as a value of `c-font-lock-keywords'.
This does a lot more highlighting.")

(defconst c++-font-lock-keywords-1 nil
  "For consideration as a value of `c++-font-lock-keywords'.
This does fairly subdued highlighting.")

(defconst c++-font-lock-keywords-2 nil
  "For consideration as a value of `c++-font-lock-keywords'.
This does a lot more highlighting.")

(let ((c-keywords
;      ("break" "continue" "do" "else" "for" "if" "return" "switch" "while")
       "break\\|continue\\|do\\|else\\|for\\|if\\|return\\|switch\\|while")
      (c-type-types
;      ("auto" "extern" "register" "static" "typedef" "struct" "union" "enum"
;	"signed" "unsigned" "short" "long" "int" "char" "float" "double"
;	"void" "volatile" "const")
       (concat "auto\\|c\\(har\\|onst\\)\\|double\\|e\\(num\\|xtern\\)\\|"
	       "float\\|int\\|long\\|register\\|"
	       "s\\(hort\\|igned\\|t\\(atic\\|ruct\\)\\)\\|typedef\\|"
	       "un\\(ion\\|signed\\)\\|vo\\(id\\|latile\\)"))	; 6 ()s deep.
      (c++-keywords
;      ("break" "continue" "do" "else" "for" "if" "return" "switch" "while"
;	"asm" "catch" "delete" "new" "operator" "sizeof" "this" "throw" "try"
;       "protected" "private" "public")
       (concat "asm\\|break\\|c\\(atch\\|ontinue\\)\\|d\\(elete\\|o\\)\\|"
	       "else\\|for\\|if\\|new\\|operator\\|"
	       "p\\(r\\(ivate\\|otected\\)\\|ublic\\)\\|return\\|"
	       "s\\(izeof\\|witch\\)\\|t\\(h\\(is\\|row\\)\\|ry\\)\\|while"))
      (c++-type-types
;      ("auto" "extern" "register" "static" "typedef" "struct" "union" "enum"
;	"signed" "unsigned" "short" "long" "int" "char" "float" "double"
;	"void" "volatile" "const" "class" "inline" "friend" "bool"
;	"virtual" "complex" "template")
       (concat "auto\\|bool\\|c\\(har\\|lass\\|o\\(mplex\\|nst\\)\\)\\|"
	       "double\\|e\\(num\\|xtern\\)\\|f\\(loat\\|riend\\)\\|"
	       "in\\(line\\|t\\)\\|long\\|register\\|"
	       "s\\(hort\\|igned\\|t\\(atic\\|ruct\\)\\)\\|"
	       "t\\(emplate\\|ypedef\\)\\|un\\(ion\\|signed\\)\\|"
	       "v\\(irtual\\|o\\(id\\|latile\\)\\)"))		; 11 ()s deep.
      (ctoken "[a-zA-Z0-9_:~]+"))
 (setq c-font-lock-keywords-1
  (list
   ;;
   ;; Fontify filenames in #include <...> preprocessor directives.
   '("^#[ \t]*include[ \t]+\\(<[^>\"\n]+>\\)" 1 font-lock-string-face)
   ;;
   ;; Fontify function macro names.
   '("^#[ \t]*define[ \t]+\\(\\(\\sw+\\)(\\)" 2 font-lock-function-name-face)
   ;;
   ;; Fontify otherwise as symbol names, and the preprocessor directive names.
   '("^\\(#[ \t]*[a-z]+\\)\\>[ \t]*\\(\\sw+\\)?"
     (1 font-lock-reference-face) (2 font-lock-variable-name-face nil t))
   ;;
   ;; Fontify function name definitions (without type on line).
   (list (concat "^\\(" ctoken "\\)[ \t]*(") 1 'font-lock-function-name-face)
   ))

 (setq c-font-lock-keywords-2
  (append c-font-lock-keywords-1
   (list
    ;;
    ;; Fontify all storage classes and type specifiers (before declarations).
    (cons (concat "\\<\\(" c-type-types "\\)\\>") 'font-lock-type-face)
    ;;
    ;; Fontify variable/structure name declarations and definitions, or
    ;; function name declarations (plus definitions with type on same line).
    (list (concat "\\<\\(" c-type-types "\\)[ \t*]+"
		  "\\(" ctoken "[ \t*]+\\)*"
		  "\\(" ctoken "\\)[ \t]*\\((\\)?")
	  9
	  '(if (match-beginning 10)
	       font-lock-function-name-face
	     font-lock-variable-name-face))
    ;;
    ;; Fontify function/variable name declarations at the start of the line.
    ;; (Not everyone follows the GNU convention of function name at the start.)
    (list (concat "^" ctoken "[ \t*]+"
		  "\\(" ctoken "[ \t*]+\\)*"
		  "\\(" ctoken "\\)[ \t]*\\((\\)?")
	  2
	  '(if (match-beginning 3)
	       font-lock-function-name-face
	     font-lock-variable-name-face))
    ;;
    ;; Fontify variable names declared with structures, or typedef names.
    '("}[ \t*]*\\(\\sw+\\)[ \t]*[;,[]" 1 font-lock-variable-name-face)
    ;;
    ;; Fontify all builtin keywords (except case, default and goto; see below).
    (concat "\\<\\(" c-keywords "\\)\\>")
    ;;
    ;; Fontify case/goto keywords and targets, and goto tags (incl "default:").
    '("\\<\\(case\\|goto\\)\\>[ \t]*\\([^ \t\n:;]+\\)?"
      (1 font-lock-keyword-face) (2 font-lock-reference-face nil t))
    '("^[ \t]*\\(\\sw+\\)[ \t]*:" 1 font-lock-reference-face)
    )))

 (setq c++-font-lock-keywords-1 c-font-lock-keywords-1)
 (setq c++-font-lock-keywords-2
  (append c++-font-lock-keywords-1
   (list
    ;; We don't just add to the C keywords for subtle differences and speed.
    ;; See the above comments for `c-font-lock-keywords-2'.
    (cons (concat "\\<\\(" c++-type-types "\\)\\>") 'font-lock-type-face)
    (list (concat "\\<\\(" c++-type-types "\\)[ \t*&]+"
		  "\\(" ctoken "[ \t*&]+\\)*"
		  "\\(" ctoken "\\)[ \t]*\\((\\)?")
	  14
	  '(if (match-beginning 15)
	       font-lock-function-name-face
	     font-lock-variable-name-face))
    (list (concat "^" ctoken "[ \t*]+"
		  "\\(" ctoken "[ \t*]+\\)*"
		  "\\(" ctoken "\\)[ \t]*\\((\\)?")
	  2
	  '(if (match-beginning 3)
	       font-lock-function-name-face
	     font-lock-variable-name-face))
    '("}[ \t*]*\\(\\sw+\\)[ \t]*[;,[]" 1 font-lock-variable-name-face)
    (concat "\\<\\(" c++-keywords "\\)\\>")
    '("\\<\\(case\\|goto\\)\\>[ \t]*\\([^ \t\n:;]+\\)?"
      (1 font-lock-keyword-face) (2 font-lock-reference-face nil t))
    '("^[ \t]*\\(\\sw+\\)[ \t]*:[^:]" 1 font-lock-reference-face))))
 )

; default to the gaudier variety?
(defvar c-font-lock-keywords c-font-lock-keywords-1
  "Additional expressions to highlight in C mode.")

(defvar c++-font-lock-keywords c++-font-lock-keywords-1
  "Additional expressions to highlight in C++ mode.")

(defvar tex-font-lock-keywords
  (list
   '("\\(\\\\\\([a-zA-Z@]+\\|.\\)\\)" 1 font-lock-keyword-face t)
   '("{\\\\em\\([^}]+\\)}" 1 font-lock-comment-face t)
   '("{\\\\bf\\([^}]+\\)}" 1 font-lock-keyword-face t)
   '("^[ \t\n]*\\\\def[\\\\@]\\(\\w+\\)" 1 font-lock-function-name-face t)
   '("\\\\\\(begin\\|end\\){\\([a-zA-Z0-9\\*]+\\)}"
     2 font-lock-function-name-face t)
   '("[^\\\\]\\$\\([^$]*\\)\\$" 1 font-lock-string-face t)
;   '("\\$\\([^$]*\\)\\$" 1 font-lock-string-face t)
   )
  "Additional expressions to highlight in TeX mode.")

;; There is no html-mode.el shipped with Emacs; `font-lock-defaults' entry
; would be: (html-font-lock-keywords nil t)
;(defconst html-font-lock-keywords
; '(("<!--[^>]*>" 0 font-lock-comment-face t)		; Comment.
;   ("</?\\sw+" . font-lock-type-face)			; Normal tag start.
;   (">" . font-lock-type-face)				; Normal tag end.
;   ("<\\(/?\\(a\\|form\\|img\\|input\\)\\)\\>"		; Special tag name.
;    1 font-lock-function-name-face t)
;   ("\\<\\(\\sw+\\)[>=]" 1 font-lock-keyword-face))	; Tag attribute.
; "Additional expressions to highlight in HTML mode.")

(defun font-lock-set-defaults ()
  "Set fontification defaults appropriately for this mode.
Sets `font-lock-keywords', `font-lock-no-comments', `font-lock-syntax-table'
and `font-lock-keywords-case-fold-search' using `font-lock-defaults-alist'."
  (or font-lock-keywords		; if not already set.
      (let ((defaults (or font-lock-defaults
			  (cdr (assq major-mode font-lock-defaults-alist)))))
	;; Keywords?
	(setq font-lock-keywords (eval (nth 0 defaults)))
	;; Syntactic?
	(if (nth 1 defaults)
	    (set (make-local-variable 'font-lock-no-comments) t))
	;; Case fold?
	(if (nth 2 defaults)
	    (set (make-local-variable 'font-lock-keywords-case-fold-search) t))
	;; Syntax table?
	(if (nth 3 defaults)
	    (let ((slist (nth 3 defaults)))
	      (make-local-variable 'font-lock-syntax-table)
	      (setq font-lock-syntax-table (copy-syntax-table (syntax-table)))
	      (while slist
		(modify-syntax-entry (car (car slist)) (cdr (car slist))
				     font-lock-syntax-table)
		(setq slist (cdr slist))))))))

;; Install ourselves:

(if purify-flag
    (add-hook 'after-init-hook 'font-lock-make-faces)
  (font-lock-make-faces))

(or (assq 'font-lock-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(font-lock-mode " Font") minor-mode-alist)))

;; Provide ourselves:

(provide 'font-lock)

;;; font-lock.el ends here
