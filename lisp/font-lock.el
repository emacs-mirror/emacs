;;; font-lock.el --- Electric font lock mode

;; Copyright (C) 1992, 93, 94, 95, 96, 1997 Free Software Foundation, Inc.

;; Author: jwz, then rms, then sm <simon@gnu.ai.mit.edu>
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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Font Lock mode is a minor mode that causes your comments to be displayed in
;; one face, strings in another, reserved words in another, and so on.
;;
;; Comments will be displayed in `font-lock-comment-face'.
;; Strings will be displayed in `font-lock-string-face'.
;; Regexps are used to display selected patterns in other faces.
;;
;; To make the text you type be fontified, use M-x font-lock-mode RET.
;; When this minor mode is on, the faces of the current line are updated with
;; every insertion or deletion.
;;
;; To turn Font Lock mode on automatically, add this to your ~/.emacs file:
;;
;;  (add-hook 'emacs-lisp-mode-hook 'turn-on-font-lock)
;;
;; Or if you want to turn Font Lock mode on in many modes:
;;
;;  (global-font-lock-mode t)
;;
;; Fontification for a particular mode may be available in a number of levels
;; of decoration.  The higher the level, the more decoration, but the more time
;; it takes to fontify.  See the variable `font-lock-maximum-decoration', and
;; also the variable `font-lock-maximum-size'.  Support modes for Font Lock
;; mode can be used to speed up Font Lock mode.  See `font-lock-support-mode'.

;;; How Font Lock mode fontifies:

;; When Font Lock mode is turned on in a buffer, it (a) fontifies the entire
;; buffer and (b) installs one of its fontification functions on one of the
;; hook variables that are run by Emacs after every buffer change (i.e., an
;; insertion or deletion).  Fontification means the replacement of `face' text
;; properties in a given region; Emacs displays text with these `face' text
;; properties appropriately.
;;
;; Fontification normally involves syntactic (i.e., strings and comments) and
;; regexp (i.e., keywords and everything else) passes.  The syntactic pass
;; involves a syntax table and a syntax parsing function to determine the
;; context of different parts of a region of text.  It is necessary because
;; generally strings and/or comments can span lines, and so the context of a
;; given region is not necessarily apparent from the content of that region.
;; Because the regexp pass only works within a given region, it is not
;; generally appropriate for syntactic fontification.  The regexp pass involves
;; searching for given regexps (or calling given functions) within the given
;; region.  For each match of the regexp (or non-nil value of the called
;; function), `face' text properties are added appropriately.

;;; How Font Lock mode supports modes or is supported by modes:

;; Modes that support Font Lock mode do so by defining one or more variables
;; whose values specify the fontification.  Font Lock mode knows of these
;; variable names from (a) the buffer local variable `font-lock-defaults', if
;; non-nil, or (b) the global variable `font-lock-defaults-alist', if the major
;; mode has an entry.  (Font Lock mode is set up via (a) where a mode's
;; patterns are distributed with the mode's package library, and (b) where a
;; mode's patterns are distributed with font-lock.el itself.  An example of (a)
;; is Pascal mode, an example of (b) is Lisp mode.  Normally, the mechanism is
;; (a); (b) is used where it is not clear which package library should contain
;; the pattern definitions.)  Font Lock mode chooses which variable to use for
;; fontification based on `font-lock-maximum-decoration'.
;;
;; Font Lock mode fontification behaviour can be modified in a number of ways.
;; See the below comments and the comments distributed throughout this file.

;;; Constructing patterns:

;; See the documentation for the variable `font-lock-keywords'.
;;
;; Nasty regexps of the form "bar\\(\\|lo\\)\\|f\\(oo\\|u\\(\\|bar\\)\\)\\|lo"
;; are made thusly: (make-regexp '("foo" "fu" "fubar" "bar" "barlo" "lo")) for
;; efficiency.  See /pub/gnu/emacs/elisp-archive/functions/make-regexp.el.Z on
;; archive.cis.ohio-state.edu for this and other functions not just by sm.

;;; Adding patterns for modes that already support Font Lock:

;; Though Font Lock highlighting patterns already exist for many modes, it's
;; likely there's something that you want fontified that currently isn't, even
;; at the maximum fontification level.  You can add highlighting patterns via
;; `font-lock-add-keywords'.  For example, say in some C
;; header file you #define the token `and' to expand to `&&', etc., to make
;; your C code almost readable.  In your ~/.emacs there could be:
;;
;;  (font-lock-add-keywords 'c-mode '("\\<\\(and\\|or\\|not\\)\\>"))
;;
;; Some modes provide specific ways to modify patterns based on the values of
;; other variables.  For example, additional C types can be specified via the
;; variable `c-font-lock-extra-types'.

;;; Adding patterns for modes that do not support Font Lock:

;; Not all modes support Font Lock mode.  If you (as a user of the mode) add
;; patterns for a new mode, you must define in your ~/.emacs a variable or
;; variables that specify regexp fontification.  Then, you should indicate to
;; Font Lock mode, via the mode hook setting `font-lock-defaults', exactly what
;; support is required.  For example, say Foo mode should have the following
;; regexps fontified case-sensitively, and comments and strings should not be
;; fontified automagically.  In your ~/.emacs there could be:
;;
;;  (defvar foo-font-lock-keywords
;;    '(("\\<\\(one\\|two\\|three\\)\\>" . font-lock-keyword-face)
;;      ("\\<\\(four\\|five\\|six\\)\\>" . font-lock-type-face))
;;    "Default expressions to highlight in Foo mode.")
;;
;;  (add-hook 'foo-mode-hook
;;   (function (lambda ()
;;               (make-local-variable 'font-lock-defaults)
;;               (setq font-lock-defaults '(foo-font-lock-keywords t)))))

;;; Adding Font Lock support for modes:

;; Of course, it would be better that the mode already supports Font Lock mode.
;; The package author would do something similar to above.  The mode must
;; define at the top-level a variable or variables that specify regexp
;; fontification.  Then, the mode command should indicate to Font Lock mode,
;; via `font-lock-defaults', exactly what support is required.  For example,
;; say Bar mode should have the following regexps fontified case-insensitively,
;; and comments and strings should be fontified automagically.  In bar.el there
;; could be:
;;
;;  (defvar bar-font-lock-keywords
;;    '(("\\<\\(uno\\|due\\|tre\\)\\>" . font-lock-keyword-face)
;;      ("\\<\\(quattro\\|cinque\\|sei\\)\\>" . font-lock-type-face))
;;    "Default expressions to highlight in Bar mode.")
;;
;; and within `bar-mode' there could be:
;;
;;  (make-local-variable 'font-lock-defaults)
;;  (setq font-lock-defaults '(bar-font-lock-keywords nil t))

;; What is fontification for?  You might say, "It's to make my code look nice."
;; I think it should be for adding information in the form of cues.  These cues
;; should provide you with enough information to both (a) distinguish between
;; different items, and (b) identify the item meanings, without having to read
;; the items and think about it.  Therefore, fontification allows you to think
;; less about, say, the structure of code, and more about, say, why the code
;; doesn't work.  Or maybe it allows you to think less and drift off to sleep.
;;
;; So, here are my opinions/advice/guidelines:
;; 
;; - Highlight conceptual objects, such as function and variable names, and
;;   different objects types differently, i.e., (a) and (b) above, highlight
;;   function names differently to variable names.
;; - Keep the faces distinct from each other as far as possible.
;;   i.e., (a) above.
;; - Use the same face for the same conceptual object, across all modes.
;;   i.e., (b) above, all modes that have items that can be thought of as, say,
;;   keywords, should be highlighted with the same face, etc.
;; - Make the face attributes fit the concept as far as possible.
;;   i.e., function names might be a bold colour such as blue, comments might
;;   be a bright colour such as red, character strings might be brown, because,
;;   err, strings are brown (that was not the reason, please believe me).
;; - Don't use a non-nil OVERRIDE unless you have a good reason.
;;   Only use OVERRIDE for special things that are easy to define, such as the
;;   way `...' quotes are treated in strings and comments in Emacs Lisp mode.
;;   Don't use it to, say, highlight keywords in commented out code or strings.
;; - Err, that's it.

;;; Code:

;; User variables.

(defvar font-lock-verbose (* 0 1024)
  "*If non-nil, means show status messages for buffer fontification.
If a number, only buffers greater than this size have fontification messages.")

;;;###autoload
(defvar font-lock-maximum-decoration nil
  "*Maximum decoration level for fontification.
If nil, use the default decoration (typically the minimum available).
If t, use the maximum decoration available.
If a number, use that level of decoration (or if not available the maximum).
If a list, each element should be a cons pair of the form (MAJOR-MODE . LEVEL),
where MAJOR-MODE is a symbol or t (meaning the default).  For example:
 ((c-mode . t) (c++-mode . 2) (t . 1))
means use the maximum decoration available for buffers in C mode, level 2
decoration for buffers in C++ mode, and level 1 decoration otherwise.")

;;;###autoload
(defvar font-lock-maximum-size (* 250 1024)
  "*Maximum size of a buffer for buffer fontification.
Only buffers less than this can be fontified when Font Lock mode is turned on.
If nil, means size is irrelevant.
If a list, each element should be a cons pair of the form (MAJOR-MODE . SIZE),
where MAJOR-MODE is a symbol or t (meaning the default).  For example:
 ((c-mode . 256000) (c++-mode . 256000) (rmail-mode . 1048576))
means that the maximum size is 250K for buffers in C or C++ modes, one megabyte
for buffers in Rmail mode, and size is irrelevant otherwise.")

;; Fontification variables:

;; Originally these variable values were face names such as `bold' etc.
;; Now we create our own faces, but we keep these variables for compatibility
;; and they give users another mechanism for changing face appearance.
;; We now allow a FACENAME in `font-lock-keywords' to be any expression that
;; returns a face.  So the easiest thing is to continue using these variables,
;; rather than sometimes evaling FACENAME and sometimes not.  sm.
(defvar font-lock-comment-face		'font-lock-comment-face
  "Face to use for comments.")

(defvar font-lock-string-face		'font-lock-string-face
  "Face to use for strings.")

(defvar font-lock-keyword-face		'font-lock-keyword-face
  "Face to use for keywords.")

(defvar font-lock-builtin-face		'font-lock-builtin-face
  "Face to use for builtins.")

(defvar font-lock-function-name-face	'font-lock-function-name-face
  "Face to use for function names.")

(defvar font-lock-variable-name-face	'font-lock-variable-name-face
  "Face to use for variable names.")

(defvar font-lock-type-face		'font-lock-type-face
  "Face to use for type names.")

(defvar font-lock-reference-face	'font-lock-reference-face
  "Face to use for reference names.")

(defvar font-lock-warning-face		'font-lock-warning-face
  "Face to use for things that should stand out.")

(defvar font-lock-keywords nil
  "*A list of the keywords to highlight.
Each element should be of the form:

 MATCHER
 (MATCHER . MATCH)
 (MATCHER . FACENAME)
 (MATCHER . HIGHLIGHT)
 (MATCHER HIGHLIGHT ...)
 (eval . FORM)

where HIGHLIGHT should be either MATCH-HIGHLIGHT or MATCH-ANCHORED.

FORM is an expression, whose value should be a keyword element, evaluated when
the keyword is (first) used in a buffer.  This feature can be used to provide a
keyword that can only be generated when Font Lock mode is actually turned on.

For highlighting single items, typically only MATCH-HIGHLIGHT is required.
However, if an item or (typically) items are to be highlighted following the
instance of another item (the anchor) then MATCH-ANCHORED may be required.

MATCH-HIGHLIGHT should be of the form:

 (MATCH FACENAME OVERRIDE LAXMATCH)

Where MATCHER can be either the regexp to search for, or the function name to
call to make the search (called with one argument, the limit of the search).
MATCH is the subexpression of MATCHER to be highlighted.  FACENAME is an
expression whose value is the face name to use.  FACENAME's default attributes
may be defined in `font-lock-face-attributes'.

OVERRIDE and LAXMATCH are flags.  If OVERRIDE is t, existing fontification may
be overwritten.  If `keep', only parts not already fontified are highlighted.
If `prepend' or `append', existing fontification is merged with the new, in
which the new or existing fontification, respectively, takes precedence.
If LAXMATCH is non-nil, no error is signaled if there is no MATCH in MATCHER.

For example, an element of the form highlights (if not already highlighted):

 \"\\\\\\=<foo\\\\\\=>\"		Discrete occurrences of \"foo\" in the value of the
			variable `font-lock-keyword-face'.
 (\"fu\\\\(bar\\\\)\" . 1)	Substring \"bar\" within all occurrences of \"fubar\" in
			the value of `font-lock-keyword-face'.
 (\"fubar\" . fubar-face)	Occurrences of \"fubar\" in the value of `fubar-face'.
 (\"foo\\\\|bar\" 0 foo-bar-face t)
			Occurrences of either \"foo\" or \"bar\" in the value
			of `foo-bar-face', even if already highlighted.

MATCH-ANCHORED should be of the form:

 (MATCHER PRE-MATCH-FORM POST-MATCH-FORM MATCH-HIGHLIGHT ...)

Where MATCHER is as for MATCH-HIGHLIGHT with one exception; see below.
PRE-MATCH-FORM and POST-MATCH-FORM are evaluated before the first, and after
the last, instance MATCH-ANCHORED's MATCHER is used.  Therefore they can be
used to initialise before, and cleanup after, MATCHER is used.  Typically,
PRE-MATCH-FORM is used to move to some position relative to the original
MATCHER, before starting with MATCH-ANCHORED's MATCHER.  POST-MATCH-FORM might
be used to move, before resuming with MATCH-ANCHORED's parent's MATCHER.

For example, an element of the form highlights (if not already highlighted):

 (\"\\\\\\=<anchor\\\\\\=>\" (0 anchor-face) (\"\\\\\\=<item\\\\\\=>\" nil nil (0 item-face)))

 Discrete occurrences of \"anchor\" in the value of `anchor-face', and subsequent
 discrete occurrences of \"item\" (on the same line) in the value of `item-face'.
 (Here PRE-MATCH-FORM and POST-MATCH-FORM are nil.  Therefore \"item\" is
 initially searched for starting from the end of the match of \"anchor\", and
 searching for subsequent instance of \"anchor\" resumes from where searching
 for \"item\" concluded.)

The above-mentioned exception is as follows.  The limit of the MATCHER search
defaults to the end of the line after PRE-MATCH-FORM is evaluated.
However, if PRE-MATCH-FORM returns a position greater than the position after
PRE-MATCH-FORM is evaluated, that position is used as the limit of the search.
It is generally a bad idea to return a position greater than the end of the
line, i.e., cause the MATCHER search to span lines.

Note that the MATCH-ANCHORED feature is experimental; in the future, we may
replace it with other ways of providing this functionality.

These regular expressions should not match text which spans lines.  While
\\[font-lock-fontify-buffer] handles multi-line patterns correctly, updating
when you edit the buffer does not, since it considers text one line at a time.

Be very careful composing regexps for this list;
the wrong pattern can dramatically slow things down!")
(make-variable-buffer-local 'font-lock-keywords)

;; This variable is used by mode packages that support Font Lock mode by
;; defining their own keywords to use for `font-lock-keywords'.  (The mode
;; command should make it buffer-local and set it to provide the set up.)
(defvar font-lock-defaults nil
  "If set by a major mode, should be the defaults for Font Lock mode.
The value should be like the `cdr' of an item in `font-lock-defaults-alist'.")

;; This variable is used where font-lock.el itself supplies the keywords.
(defvar font-lock-defaults-alist
  (let (;; We use `beginning-of-defun', rather than nil, for SYNTAX-BEGIN.
	;; Thus the calculation of the cache is usually faster but not
	;; infallible, so we risk mis-fontification.  sm.
	(c-mode-defaults
	 '((c-font-lock-keywords c-font-lock-keywords-1
	    c-font-lock-keywords-2 c-font-lock-keywords-3)
	   nil nil ((?_ . "w")) beginning-of-defun
	   (font-lock-comment-start-regexp . "/[*/]")
	   (font-lock-mark-block-function . mark-defun)))
	(c++-mode-defaults
	 '((c++-font-lock-keywords c++-font-lock-keywords-1 
	    c++-font-lock-keywords-2 c++-font-lock-keywords-3)
	   nil nil ((?_ . "w") (?~ . "w")) beginning-of-defun
	   (font-lock-comment-start-regexp . "/[*/]")
	   (font-lock-mark-block-function . mark-defun)))
	(objc-mode-defaults
	 '((objc-font-lock-keywords objc-font-lock-keywords-1
	    objc-font-lock-keywords-2 objc-font-lock-keywords-3)
	   nil nil ((?_ . "w") (?$ . "w")) nil
	   (font-lock-comment-start-regexp . "/[*/]")
	   (font-lock-mark-block-function . mark-defun)))
	(java-mode-defaults
	 '((java-font-lock-keywords java-font-lock-keywords-1
	    java-font-lock-keywords-2 java-font-lock-keywords-3)
	   nil nil ((?_ . "w") (?$ . "w") (?. . "w")) nil
	   (font-lock-comment-start-regexp . "/[*/]")
	   (font-lock-mark-block-function . mark-defun)))
	(lisp-mode-defaults
	 '((lisp-font-lock-keywords
	    lisp-font-lock-keywords-1 lisp-font-lock-keywords-2)
	   nil nil (("+-*/.<>=!?$%_&~^:" . "w")) beginning-of-defun
	   (font-lock-comment-start-regexp . ";")
	   (font-lock-mark-block-function . mark-defun)))
	(scheme-mode-defaults
	 '(scheme-font-lock-keywords
	   nil t (("+-*/.<>=!?$%_&~^:" . "w")) beginning-of-defun
	   (font-lock-comment-start-regexp . ";")
	   (font-lock-mark-block-function . mark-defun)))
	;; For TeX modes we could use `backward-paragraph' for the same reason.
	;; But we don't, because paragraph breaks are arguably likely enough to
	;; occur within a genuine syntactic block to make it too risky.
	;; However, we do specify a MARK-BLOCK function as that cannot result
	;; in a mis-fontification even if it might not fontify enough.  --sm.
	(tex-mode-defaults
	 '(tex-font-lock-keywords nil nil ((?$ . "\"")) nil
	   (font-lock-comment-start-regexp . "%")
	   (font-lock-mark-block-function . mark-paragraph)))
	)
    (list
     (cons 'c-mode			c-mode-defaults)
     (cons 'c++-mode			c++-mode-defaults)
     (cons 'objc-mode			objc-mode-defaults)
     (cons 'java-mode			java-mode-defaults)
     (cons 'emacs-lisp-mode		lisp-mode-defaults)
     (cons 'inferior-scheme-mode	scheme-mode-defaults)
     (cons 'latex-mode			tex-mode-defaults)
     (cons 'lisp-mode			lisp-mode-defaults)
     (cons 'lisp-interaction-mode	lisp-mode-defaults)
     (cons 'plain-tex-mode		tex-mode-defaults)
     (cons 'scheme-mode			scheme-mode-defaults)
     (cons 'scheme-interaction-mode	scheme-mode-defaults)
     (cons 'slitex-mode			tex-mode-defaults)
     (cons 'tex-mode			tex-mode-defaults)))
  "Alist of default major mode and Font Lock defaults.
Each item should be a list of the form:

 (MAJOR-MODE . (KEYWORDS KEYWORDS-ONLY CASE-FOLD SYNTAX-ALIST SYNTAX-BEGIN
                ...))

where MAJOR-MODE is a symbol.  KEYWORDS may be a symbol (a variable or function
whose value is the keywords to use for fontification) or a list of symbols.
If KEYWORDS-ONLY is non-nil, syntactic fontification (strings and comments) is
not performed.  If CASE-FOLD is non-nil, the case of the keywords is ignored
when fontifying.  If SYNTAX-ALIST is non-nil, it should be a list of cons pairs
of the form (CHAR-OR-STRING . STRING) used to set the local Font Lock syntax
table, for keyword and syntactic fontification (see `modify-syntax-entry').

If SYNTAX-BEGIN is non-nil, it should be a function with no args used to move
backwards outside any enclosing syntactic block, for syntactic fontification.
Typical values are `beginning-of-line' (i.e., the start of the line is known to
be outside a syntactic block), or `beginning-of-defun' for programming modes or
`backward-paragraph' for textual modes (i.e., the mode-dependent function is
known to move outside a syntactic block).  If nil, the beginning of the buffer
is used as a position outside of a syntactic block, in the worst case.

These item elements are used by Font Lock mode to set the variables
`font-lock-keywords', `font-lock-keywords-only',
`font-lock-keywords-case-fold-search', `font-lock-syntax-table' and
`font-lock-beginning-of-syntax-function', respectively.

Further item elements are alists of the form (VARIABLE . VALUE) and are in no
particular order.  Each VARIABLE is made buffer-local before set to VALUE.

Currently, appropriate variables include `font-lock-mark-block-function'.
If this is non-nil, it should be a function with no args used to mark any
enclosing block of text, for fontification via \\[font-lock-fontify-block].
Typical values are `mark-defun' for programming modes or `mark-paragraph' for
textual modes (i.e., the mode-dependent function is known to put point and mark
around a text block relevant to that mode).

Other variables include those for buffer-specialised fontification functions,
`font-lock-fontify-buffer-function', `font-lock-unfontify-buffer-function',
`font-lock-fontify-region-function', `font-lock-unfontify-region-function',
`font-lock-comment-start-regexp', `font-lock-inhibit-thing-lock' and
`font-lock-maximum-size'.")

(defvar font-lock-keywords-alist nil
  "*Alist of `font-lock-keywords' local to a `major-mode'.
This is normally set via `font-lock-add-keywords'.")

(defvar font-lock-keywords-only nil
  "*Non-nil means Font Lock should not fontify comments or strings.
This is normally set via `font-lock-defaults'.")

(defvar font-lock-keywords-case-fold-search nil
  "*Non-nil means the patterns in `font-lock-keywords' are case-insensitive.
This is normally set via `font-lock-defaults'.")

(defvar font-lock-syntax-table nil
  "Non-nil means use this syntax table for fontifying.
If this is nil, the major mode's syntax table is used.
This is normally set via `font-lock-defaults'.")

;; If this is nil, we only use the beginning of the buffer if we can't use
;; `font-lock-cache-position' and `font-lock-cache-state'.
(defvar font-lock-beginning-of-syntax-function nil
  "*Non-nil means use this function to move back outside of a syntactic block.
When called with no args it should leave point at the beginning of any
enclosing syntactic block.
If this is nil, the beginning of the buffer is used (in the worst case).
This is normally set via `font-lock-defaults'.")

(defvar font-lock-mark-block-function nil
  "*Non-nil means use this function to mark a block of text.
When called with no args it should leave point at the beginning of any
enclosing textual block and mark at the end.
This is normally set via `font-lock-defaults'.")

(defvar font-lock-comment-start-regexp nil
  "*Regexp to match the start of a comment.
This need not discriminate between genuine comments and quoted comment
characters or comment characters within strings.
If nil, `comment-start-skip' is used instead; see that variable for more info.
This is normally set via `font-lock-defaults'.")

(defvar font-lock-fontify-buffer-function 'font-lock-default-fontify-buffer
  "Function to use for fontifying the buffer.
This is normally set via `font-lock-defaults'.")

(defvar font-lock-unfontify-buffer-function 'font-lock-default-unfontify-buffer
  "Function to use for unfontifying the buffer.
This is used when turning off Font Lock mode.
This is normally set via `font-lock-defaults'.")

(defvar font-lock-fontify-region-function 'font-lock-default-fontify-region
  "Function to use for fontifying a region.
It should take two args, the beginning and end of the region, and an optional
third arg VERBOSE.  If non-nil, the function should print status messages.
This is normally set via `font-lock-defaults'.")

(defvar font-lock-unfontify-region-function 'font-lock-default-unfontify-region
  "Function to use for unfontifying a region.
It should take two args, the beginning and end of the region.
This is normally set via `font-lock-defaults'.")

(defvar font-lock-inhibit-thing-lock nil
  "List of Font Lock mode related modes that should not be turned on.
Currently, valid mode names as `fast-lock-mode' and `lazy-lock-mode'.
This is normally set via `font-lock-defaults'.")

(defvar font-lock-mode nil)		; Whether we are turned on/modeline.
(defvar font-lock-fontified nil)	; Whether we have fontified the buffer.

;;;###autoload
(defvar font-lock-mode-hook nil
  "Function or functions to run on entry to Font Lock mode.")

;; Font Lock mode.

(eval-when-compile
  ;;
  ;; We don't do this at the top-level as we only use non-autoloaded macros.
  (require 'cl)
  ;;
  ;; Borrowed from lazy-lock.el.
  ;; We use this to preserve or protect things when modifying text properties.
  (defmacro save-buffer-state (varlist &rest body)
    "Bind variables according to VARLIST and eval BODY restoring buffer state."
    (` (let* ((,@ (append varlist
		   '((modified (buffer-modified-p))
		     (inhibit-read-only t) (buffer-undo-list t)
		     before-change-functions after-change-functions
		     deactivate-mark buffer-file-name buffer-file-truename))))
	 (,@ body)
	 (when (and (not modified) (buffer-modified-p))
	   (set-buffer-modified-p nil)))))
  (put 'save-buffer-state 'lisp-indent-function 1))

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

Alternatively, you can use Global Font Lock mode to automagically turn on Font
Lock mode in buffers whose major mode supports it and whose major mode is one
of `font-lock-global-modes'.  For example, put in your ~/.emacs:

 (global-font-lock-mode t)

There are a number of support modes that may be used to speed up Font Lock mode
in various ways, specified via the variable `font-lock-support-mode'.  Where
major modes support different levels of fontification, you can use the variable
`font-lock-maximum-decoration' to specify which level you generally prefer.
When you turn Font Lock mode on/off the buffer is fontified/defontified, though
fontification occurs only if the buffer is less than `font-lock-maximum-size'.

For example, to specify that Font Lock mode use use Lazy Lock mode as a support
mode and use maximum levels of fontification, put in your ~/.emacs:

 (setq font-lock-support-mode 'lazy-lock-mode)
 (setq font-lock-maximum-decoration t)

To add your own highlighting for some major mode, and modify the highlighting
selected automatically via the variable `font-lock-maximum-decoration', you can
use `font-lock-add-keywords'.

To fontify a buffer, without turning on Font Lock mode and regardless of buffer
size, you can use \\[font-lock-fontify-buffer].

To fontify a block (the function or paragraph containing point, or a number of
lines around point), perhaps because modification on the current line caused
syntactic change on other lines, you can use \\[font-lock-fontify-block].

The default Font Lock mode faces and their attributes are defined in the
variable `font-lock-face-attributes', and Font Lock mode default settings in
the variable `font-lock-defaults-alist'.  You can set your own default settings
for some mode, by setting a buffer local value for `font-lock-defaults', via
its mode hook."
  (interactive "P")
  ;; Don't turn on Font Lock mode if we don't have a display (we're running a
  ;; batch job) or if the buffer is invisible (the name starts with a space).
  (let ((on-p (and (not noninteractive)
		   (not (eq (aref (buffer-name) 0) ?\ ))
		   (if arg
		       (> (prefix-numeric-value arg) 0)
		     (not font-lock-mode)))))
    (set (make-local-variable 'font-lock-mode) on-p)
    ;; Turn on Font Lock mode.
    (when on-p
      (make-local-hook 'after-change-functions)
      (add-hook 'after-change-functions 'font-lock-after-change-function nil t)
      (font-lock-set-defaults)
      (font-lock-turn-on-thing-lock)
      (run-hooks 'font-lock-mode-hook)
      ;; Fontify the buffer if we have to.
      (let ((max-size (font-lock-value-in-major-mode font-lock-maximum-size)))
	(cond (font-lock-fontified
	       nil)
	      ((or (null max-size) (> max-size (buffer-size)))
	       (font-lock-fontify-buffer))
	      (font-lock-verbose
	       (message "Fontifying %s...buffer too big" (buffer-name))))))
    ;; Turn off Font Lock mode.
    (unless on-p
      (remove-hook 'after-change-functions 'font-lock-after-change-function t)
      (font-lock-unfontify-buffer)
      (font-lock-turn-off-thing-lock)
      (font-lock-unset-defaults))
    (force-mode-line-update)))

;;;###autoload
(defun turn-on-font-lock ()
  "Turn on Font Lock mode conditionally.
Turn on only if the terminal can display it."
  (when (and (not font-lock-mode) window-system)
    (font-lock-mode)))

;;;###autoload
(defun font-lock-add-keywords (major-mode keywords &optional append)
  "Add highlighting KEYWORDS for MAJOR-MODE.
MAJOR-MODE should be a symbol, the major mode command name, such as `c-mode'
or nil.  If nil, highlighting keywords are added for the current buffer.
KEYWORDS should be a list; see the variable `font-lock-keywords'.
By default they are added at the beginning of the current highlighting list.
If optional argument APPEND is `set', they are used to replace the current
highlighting list.  If APPEND has any other value, e.g., t, they are added at
the end of the current highlighting list.

For example:

 (font-lock-add-keywords 'c-mode
  '((\"\\\\\\=<\\\\(FIXME\\\\):\" 1 font-lock-warning-face prepend)
    (\"\\\\\\=<\\\\(and\\\\|or\\\\|not\\\\)\\\\\\=>\" . font-lock-keyword-face)))

adds two fontification patterns for C mode, to fontify `FIXME:' words, even in
comments, and to fontify `and', `or' and `not' words as keywords."
  (cond (major-mode
	 ;; If MAJOR-MODE is non-nil, add the KEYWORDS and APPEND spec to
	 ;; `font-lock-keywords-alist' so `font-lock-set-defaults' uses them.
	 (let ((spec (cons keywords append)) cell)
	   (if (setq cell (assq major-mode font-lock-keywords-alist))
	       (setcdr cell (append (cdr cell) (list spec)))
	     (push (list major-mode spec) font-lock-keywords-alist))))
	(font-lock-mode
	 ;; Otherwise if Font Lock mode is on, set or add the keywords now.
	 (if (eq append 'set)
	     (setq font-lock-keywords keywords)
	   (let ((old (if (eq (car-safe font-lock-keywords) t)
			  (cdr font-lock-keywords)
			font-lock-keywords)))
	     (setq font-lock-keywords (if append
					  (append old keywords)
					(append keywords old))))))))

;; Global Font Lock mode.
;;
;; A few people have hassled in the past for a way to make it easier to turn on
;; Font Lock mode, without the user needing to know for which modes s/he has to
;; turn it on, perhaps the same way hilit19.el/hl319.el does.  I've always
;; balked at that way, as I see it as just re-moulding the same problem in
;; another form.  That is; some person would still have to keep track of which
;; modes (which may not even be distributed with Emacs) support Font Lock mode.
;; The list would always be out of date.  And that person might have to be me.

;; Implementation.
;;
;; In a previous discussion the following hack came to mind.  It is a gross
;; hack, but it generally works.  We use the convention that major modes start
;; by calling the function `kill-all-local-variables', which in turn runs
;; functions on the hook variable `change-major-mode-hook'.  We attach our
;; function `font-lock-change-major-mode' to that hook.  Of course, when this
;; hook is run, the major mode is in the process of being changed and we do not
;; know what the final major mode will be.  So, `font-lock-change-major-mode'
;; only (a) notes the name of the current buffer, and (b) adds our function
;; `turn-on-font-lock-if-enabled' to the hook variables `find-file-hooks' and
;; `post-command-hook' (for buffers that are not visiting files).  By the time
;; the functions on the first of these hooks to be run are run, the new major
;; mode is assumed to be in place.  This way we get a Font Lock function run
;; when a major mode is turned on, without knowing major modes or their hooks.
;;
;; Naturally this requires that (a) major modes run `kill-all-local-variables',
;; as they are supposed to do, and (b) the major mode is in place after the
;; file is visited or the command that ran `kill-all-local-variables' has
;; finished, whichever the sooner.  Arguably, any major mode that does not
;; follow the convension (a) is broken, and I can't think of any reason why (b)
;; would not be met (except `gnudoit' on non-files).  However, it is not clean.
;;
;; Probably the cleanest solution is to have each major mode function run some
;; hook, e.g., `major-mode-hook', but maybe implementing that change is
;; impractical.  I am personally against making `setq' a macro or be advised,
;; or have a special function such as `set-major-mode', but maybe someone can
;; come up with another solution?

;; User interface.
;;
;; Although Global Font Lock mode is a pseudo-mode, I think that the user
;; interface should conform to the usual Emacs convention for modes, i.e., a
;; command to toggle the feature (`global-font-lock-mode') with a variable for
;; finer control of the mode's behaviour (`font-lock-global-modes').
;;
;; The feature should not be enabled by loading font-lock.el, since other
;; mechanisms for turning on Font Lock mode, such as M-x font-lock-mode RET or
;; (add-hook 'c-mode-hook 'turn-on-font-lock), would cause Font Lock mode to be
;; turned on everywhere.  That would not be intuitive or informative because
;; loading a file tells you nothing about the feature or how to control it.  It
;; would also be contrary to the Principle of Least Surprise.  sm.

(defvar font-lock-buffers nil)		; For remembering buffers.
(defvar global-font-lock-mode nil)

;;;###autoload
(defvar font-lock-global-modes t
  "*Modes for which Font Lock mode is automagically turned on.
Global Font Lock mode is controlled by the `global-font-lock-mode' command.
If nil, means no modes have Font Lock mode automatically turned on.
If t, all modes that support Font Lock mode have it automatically turned on.
If a list, it should be a list of `major-mode' symbol names for which Font Lock
mode should be automatically turned on.  The sense of the list is negated if it
begins with `not'.  For example:
 (c-mode c++-mode)
means that Font Lock mode is turned on for buffers in C and C++ modes only.")

;;;###autoload
(defun global-font-lock-mode (&optional arg message)
  "Toggle Global Font Lock mode.
With prefix ARG, turn Global Font Lock mode on if and only if ARG is positive.
Displays a message saying whether the mode is on or off if MESSAGE is non-nil.
Returns the new status of Global Font Lock mode (non-nil means on).

When Global Font Lock mode is enabled, Font Lock mode is automagically
turned on in a buffer if its major mode is one of `font-lock-global-modes'."
  (interactive "P\np")
  (let ((off-p (if arg
		   (<= (prefix-numeric-value arg) 0)
		 global-font-lock-mode)))
    (if off-p
	(remove-hook 'find-file-hooks 'turn-on-font-lock-if-enabled)
      (add-hook 'find-file-hooks 'turn-on-font-lock-if-enabled)
      (add-hook 'post-command-hook 'turn-on-font-lock-if-enabled)
      (setq font-lock-buffers (buffer-list)))
    (when message
      (message "Global Font Lock mode is now %s." (if off-p "OFF" "ON")))
    (setq global-font-lock-mode (not off-p))))

(defun font-lock-change-major-mode ()
  ;; Turn off Font Lock mode if it's on.
  (when font-lock-mode
    (font-lock-mode))
  ;; Gross hack warning: Delicate readers should avert eyes now.
  ;; Something is running `kill-all-local-variables', which generally means the
  ;; major mode is being changed.  Run `turn-on-font-lock-if-enabled' after the
  ;; file is visited or the current command has finished.
  (when global-font-lock-mode
    (add-hook 'post-command-hook 'turn-on-font-lock-if-enabled)
    (add-to-list 'font-lock-buffers (current-buffer))))

(defun turn-on-font-lock-if-enabled ()
  ;; Gross hack warning: Delicate readers should avert eyes now.
  ;; Turn on Font Lock mode if it's supported by the major mode and enabled by
  ;; the user.
  (remove-hook 'post-command-hook 'turn-on-font-lock-if-enabled)
  (while font-lock-buffers
    (when (buffer-live-p (car font-lock-buffers))
      (save-excursion
	(set-buffer (car font-lock-buffers))
	(when (and (or font-lock-defaults
		       (assq major-mode font-lock-defaults-alist))
		   (or (eq font-lock-global-modes t)
		       (if (eq (car-safe font-lock-global-modes) 'not)
			   (not (memq major-mode (cdr font-lock-global-modes)))
			 (memq major-mode font-lock-global-modes))))
	  (let (inhibit-quit)
	    (turn-on-font-lock)))))
    (setq font-lock-buffers (cdr font-lock-buffers))))

(add-hook 'change-major-mode-hook 'font-lock-change-major-mode)

;; End of Global Font Lock mode.

;; Font Lock Support mode.
;;
;; This is the code used to interface font-lock.el with any of its add-on
;; packages, and provide the user interface.  Packages that have their own
;; local buffer fontification functions (see below) may have to call
;; `font-lock-after-fontify-buffer' and/or `font-lock-after-unfontify-buffer'
;; themselves.

;;;###autoload
(defvar font-lock-support-mode nil
  "*Support mode for Font Lock mode.
Support modes speed up Font Lock mode by being choosy about when fontification
occurs.  Known support modes are Fast Lock mode (symbol `fast-lock-mode') and
Lazy Lock mode (symbol `lazy-lock-mode').  See those modes for more info.
If nil, means support for Font Lock mode is never performed.
If a symbol, use that support mode.
If a list, each element should be of the form (MAJOR-MODE . SUPPORT-MODE),
where MAJOR-MODE is a symbol or t (meaning the default).  For example:
 ((c-mode . fast-lock-mode) (c++-mode . fast-lock-mode) (t . lazy-lock-mode))
means that Fast Lock mode is used to support Font Lock mode for buffers in C or
C++ modes, and Lazy Lock mode is used to support Font Lock mode otherwise.

The value of this variable is used when Font Lock mode is turned on.")

(defvar fast-lock-mode nil)
(defvar lazy-lock-mode nil)

(defun font-lock-turn-on-thing-lock ()
  (let ((thing-mode (font-lock-value-in-major-mode font-lock-support-mode)))
    (cond ((eq thing-mode 'fast-lock-mode)
	   (fast-lock-mode t))
	  ((eq thing-mode 'lazy-lock-mode)
	   (lazy-lock-mode t)))))

(defun font-lock-turn-off-thing-lock ()
  (cond (fast-lock-mode
	 (fast-lock-mode nil))
	(lazy-lock-mode
	 (lazy-lock-mode nil))))

(defun font-lock-after-fontify-buffer ()
  (cond (fast-lock-mode
	 (fast-lock-after-fontify-buffer))
	(lazy-lock-mode
	 (lazy-lock-after-fontify-buffer))))

(defun font-lock-after-unfontify-buffer ()
  (cond (fast-lock-mode
	 (fast-lock-after-unfontify-buffer))
	(lazy-lock-mode
	 (lazy-lock-after-unfontify-buffer))))

;; End of Font Lock Support mode.

;; Fontification functions.

;; Rather than the function, e.g., `font-lock-fontify-region' containing the
;; code to fontify a region, the function runs the function whose name is the
;; value of the variable, e.g., `font-lock-fontify-region-function'.  Normally,
;; the value of this variable is, e.g., `font-lock-default-fontify-region'
;; which does contain the code to fontify a region.  However, the value of the
;; variable could be anything and thus, e.g., `font-lock-fontify-region' could
;; do anything.  The indirection of the fontification functions gives major
;; modes the capability of modifying the way font-lock.el fontifies.  Major
;; modes can modify the values of, e.g., `font-lock-fontify-region-function',
;; via the variable `font-lock-defaults'.
;;
;; For example, Rmail mode sets the variable `font-lock-defaults' so that
;; font-lock.el uses its own function for buffer fontification.  This function
;; makes fontification be on a message-by-message basis and so visiting an
;; RMAIL file is much faster.  A clever implementation of the function might
;; fontify the headers differently than the message body.  (It should, and
;; correspondingly for Mail mode, but I can't be bothered to do the work.  Can
;; you?)  This hints at a more interesting use...
;;
;; Languages that contain text normally contained in different major modes
;; could define their own fontification functions that treat text differently
;; depending on its context.  For example, Perl mode could arrange that here
;; docs are fontified differently than Perl code.  Or Yacc mode could fontify
;; rules one way and C code another.  Neat!
;;
;; A further reason to use the fontification indirection feature is when the
;; default syntactual fontification, or the default fontification in general,
;; is not flexible enough for a particular major mode.  For example, perhaps
;; comments are just too hairy for `font-lock-fontify-syntactically-region' and
;; `font-lock-comment-start-regexp' to cope with.  You need to write your own
;; version of that function, e.g., `hairy-fontify-syntactically-region', and
;; make your own version of `hairy-fontify-region' call it before calling
;; `font-lock-fontify-keywords-region' for the normal regexp fontification
;; pass.  And Hairy mode would set `font-lock-defaults' so that font-lock.el
;; would call your region fontification function instead of its own.  For
;; example, TeX modes could fontify {\foo ...} and \bar{...} etc. multi-line
;; directives correctly and cleanly.  (It is the same problem as fontifying
;; multi-line strings and comments; regexps are not appropriate for the job.)

;;;###autoload
(defun font-lock-fontify-buffer ()
  "Fontify the current buffer the way `font-lock-mode' would."
  (interactive)
  (let ((font-lock-verbose (or font-lock-verbose (interactive-p))))
    (funcall font-lock-fontify-buffer-function)))

(defun font-lock-unfontify-buffer ()
  (funcall font-lock-unfontify-buffer-function))

(defun font-lock-fontify-region (beg end &optional loudly)
  (funcall font-lock-fontify-region-function beg end loudly))

(defun font-lock-unfontify-region (beg end)
  (funcall font-lock-unfontify-region-function beg end))

(defun font-lock-default-fontify-buffer ()
  (let ((verbose (if (numberp font-lock-verbose)
		     (> (buffer-size) font-lock-verbose)
		   font-lock-verbose)))
    (when verbose
      (message "Fontifying %s..." (buffer-name)))
    ;; Make sure we have the right `font-lock-keywords' etc.
    (unless font-lock-mode
      (font-lock-set-defaults))
    ;; Make sure we fontify etc. in the whole buffer.
    (save-restriction
      (widen)
      (condition-case nil
	  (save-excursion
	    (save-match-data
	      (font-lock-fontify-region (point-min) (point-max) verbose)
	      (font-lock-after-fontify-buffer)
	      (setq font-lock-fontified t)))
	;; We don't restore the old fontification, so it's best to unfontify.
	(quit (font-lock-unfontify-buffer))))
    ;; Make sure we undo `font-lock-keywords' etc.
    (unless font-lock-mode
      (font-lock-unset-defaults))
    (if verbose (message "Fontifying %s...%s" (buffer-name)
			 (if font-lock-fontified "done" "quit")))))

(defun font-lock-default-unfontify-buffer ()
  ;; Make sure we unfontify etc. in the whole buffer.
  (save-restriction
    (widen)
    (font-lock-unfontify-region (point-min) (point-max))
    (font-lock-after-unfontify-buffer)
    (setq font-lock-fontified nil)))

(defun font-lock-default-fontify-region (beg end loudly)
  (save-buffer-state ((old-syntax-table (syntax-table)))
    (unwind-protect
	(save-restriction
	  (widen)
	  ;; Use the fontification syntax table, if any.
	  (when font-lock-syntax-table
	    (set-syntax-table font-lock-syntax-table))
	  ;; Now do the fontification.
	  (font-lock-unfontify-region beg end)
	  (unless font-lock-keywords-only
	    (font-lock-fontify-syntactically-region beg end loudly))
	  (font-lock-fontify-keywords-region beg end loudly))
      ;; Clean up.
      (set-syntax-table old-syntax-table))))

;; The following must be rethought, since keywords can override fontification.
;      ;; Now scan for keywords, but not if we are inside a comment now.
;      (or (and (not font-lock-keywords-only)
;	       (let ((state (parse-partial-sexp beg end nil nil 
;						font-lock-cache-state)))
;		 (or (nth 4 state) (nth 7 state))))
;	  (font-lock-fontify-keywords-region beg end))

(defun font-lock-default-unfontify-region (beg end)
  (save-buffer-state nil
    (remove-text-properties beg end '(face nil))))

;; Called when any modification is made to buffer text.
(defun font-lock-after-change-function (beg end old-len)
  (save-excursion
    (save-match-data
      ;; Rescan between start of lines enclosing the region.
      (font-lock-fontify-region
       (progn (goto-char beg) (beginning-of-line) (point))
       (progn (goto-char end) (forward-line 1) (point))))))

(defun font-lock-fontify-block (&optional arg)
  "Fontify some lines the way `font-lock-fontify-buffer' would.
The lines could be a function or paragraph, or a specified number of lines.
If ARG is given, fontify that many lines before and after point, or 16 lines if
no ARG is given and `font-lock-mark-block-function' is nil.
If `font-lock-mark-block-function' non-nil and no ARG is given, it is used to
delimit the region to fontify."
  (interactive "P")
  (let (font-lock-beginning-of-syntax-function deactivate-mark)
    ;; Make sure we have the right `font-lock-keywords' etc.
    (if (not font-lock-mode) (font-lock-set-defaults))
    (save-excursion
      (save-match-data
	(condition-case error-data
	    (if (or arg (not font-lock-mark-block-function))
		(let ((lines (if arg (prefix-numeric-value arg) 16)))
		  (font-lock-fontify-region
		   (save-excursion (forward-line (- lines)) (point))
		   (save-excursion (forward-line lines) (point))))
	      (funcall font-lock-mark-block-function)
	      (font-lock-fontify-region (point) (mark)))
	  ((error quit) (message "Fontifying block...%s" error-data)))))))

(define-key facemenu-keymap "\M-g" 'font-lock-fontify-block)

;; Syntactic fontification functions.

;; These record the parse state at a particular position, always the start of a
;; line.  Used to make `font-lock-fontify-syntactically-region' faster.
;; Previously, `font-lock-cache-position' was just a buffer position.  However,
;; under certain situations, this occasionally resulted in mis-fontification.
;; I think the "situations" were deletion with Lazy Lock mode's deferral.  sm.
(defvar font-lock-cache-state nil)
(defvar font-lock-cache-position nil)

(defun font-lock-fontify-syntactically-region (start end &optional loudly)
  "Put proper face on each string and comment between START and END.
START should be at the beginning of a line."
  (let ((synstart (cond (font-lock-comment-start-regexp
			 (concat "\\s\"\\|" font-lock-comment-start-regexp))
			(comment-start-skip
			 (concat "\\s\"\\|" comment-start-skip))
			(t
			 "\\s\"")))
	(cache (marker-position font-lock-cache-position))
	state prev here beg)
    (if loudly (message "Fontifying %s... (syntactically...)" (buffer-name)))
    (goto-char start)
    ;;
    ;; Find the state at the `beginning-of-line' before `start'.
    (if (eq start cache)
	;; Use the cache for the state of `start'.
	(setq state font-lock-cache-state)
      ;; Find the state of `start'.
      (if (null font-lock-beginning-of-syntax-function)
	  ;; Use the state at the previous cache position, if any, or
	  ;; otherwise calculate from `point-min'.
	  (if (or (null cache) (< start cache))
	      (setq state (parse-partial-sexp (point-min) start))
	    (setq state (parse-partial-sexp cache start nil nil
					    font-lock-cache-state)))
	;; Call the function to move outside any syntactic block.
	(funcall font-lock-beginning-of-syntax-function)
	(setq state (parse-partial-sexp (point) start)))
      ;; Cache the state and position of `start'.
      (setq font-lock-cache-state state)
      (set-marker font-lock-cache-position start))
    ;;
    ;; If the region starts inside a string, show the extent of it.
    (when (nth 3 state)
      (setq here (point))
      (while (and (re-search-forward "\\s\"" end 'move)
		  ;; Verify the state so we don't get fooled by quoting.
		  (nth 3 (parse-partial-sexp here (point) nil nil state))))
      (put-text-property here (point) 'face font-lock-string-face)
      (setq state (parse-partial-sexp here (point) nil nil state)))
    ;;
    ;; Likewise for a comment.
    (when (or (nth 4 state) (nth 7 state))
      (let ((comstart (cond (font-lock-comment-start-regexp
			     font-lock-comment-start-regexp)
			    (comment-start-skip
			     (concat "\\s<\\|" comment-start-skip))
			    (t
			     "\\s<")))
	    (count 1))
	(setq here (point))
	(condition-case nil
	    (save-restriction
	      (narrow-to-region (point-min) end)
	      ;; Go back to the real start of the comment.
	      (re-search-backward comstart)
	      (forward-comment 1)
	      ;; If there is more than one comment type, then the previous
	      ;; comment start might not be the real comment start.
	      ;; For example, in C++ code, `here' might be on a line following
	      ;; a // comment that is actually within a /* */ comment.
	      (while (<= (point) here)
		(goto-char here)
		(re-search-backward comstart nil nil (incf count))
		(forward-comment 1))
	      ;; Go back to the real end of the comment.
	      (skip-chars-backward " \t"))
	  (error (goto-char end)))
	(put-text-property here (point) 'face font-lock-comment-face)
	(setq state (parse-partial-sexp here (point) nil nil state))))
    ;;
    ;; Find each interesting place between here and `end'.
    (while (and (< (point) end)
		(setq prev (point))
		(re-search-forward synstart end t)
		(setq state (parse-partial-sexp prev (point) nil nil state)))
      (cond ((nth 3 state)
	     ;;
	     ;; Found a real string start.
	     (setq here (point) beg (match-beginning 0))
	     (condition-case nil
		 (save-restriction
		   (narrow-to-region (point-min) end)
		   (goto-char (scan-sexps beg 1)))
	       (error (goto-char end)))
	     (put-text-property beg (point) 'face font-lock-string-face)
	     (setq state (parse-partial-sexp here (point) nil nil state)))
	    ((or (nth 4 state) (nth 7 state))
	     ;;
	     ;; Found a real comment start.
	     (setq here (point) beg (or (match-end 1) (match-beginning 0)))
	     (goto-char beg)
	     (condition-case nil
		 (save-restriction
		   (narrow-to-region (point-min) end)
		   (forward-comment 1)
		   (skip-chars-backward " \t"))
	       (error (goto-char end)))
	     (put-text-property beg (point) 'face font-lock-comment-face)
	     (setq state (parse-partial-sexp here (point) nil nil state)))))))

;;; Additional text property functions.

;; The following text property functions should be builtins.  This means they
;; should be written in C and put with all the other text property functions.
;; In the meantime, those that are used by font-lock.el are defined in Lisp
;; below and given a `font-lock-' prefix.  Those that are not used are defined
;; in Lisp below and commented out.  sm.

(defun font-lock-prepend-text-property (start end prop value &optional object)
  "Prepend to one property of the text from START to END.
Arguments PROP and VALUE specify the property and value to prepend to the value
already in place.  The resulting property values are always lists.
Optional argument OBJECT is the string or buffer containing the text."
  (let ((val (if (listp value) value (list value))) next prev)
    (while (/= start end)
      (setq next (next-single-property-change start prop object end)
	    prev (get-text-property start prop object))
      (put-text-property start next prop
			 (append val (if (listp prev) prev (list prev)))
			 object)
      (setq start next))))

(defun font-lock-append-text-property (start end prop value &optional object)
  "Append to one property of the text from START to END.
Arguments PROP and VALUE specify the property and value to append to the value
already in place.  The resulting property values are always lists.
Optional argument OBJECT is the string or buffer containing the text."
  (let ((val (if (listp value) value (list value))) next prev)
    (while (/= start end)
      (setq next (next-single-property-change start prop object end)
	    prev (get-text-property start prop object))
      (put-text-property start next prop
			 (append (if (listp prev) prev (list prev)) val)
			 object)
      (setq start next))))

(defun font-lock-fillin-text-property (start end prop value &optional object)
  "Fill in one property of the text from START to END.
Arguments PROP and VALUE specify the property and value to put where none are
already in place.  Therefore existing property values are not overwritten.
Optional argument OBJECT is the string or buffer containing the text."
  (let ((start (text-property-any start end prop nil object)) next)
    (while start
      (setq next (next-single-property-change start prop object end))
      (put-text-property start next prop value object)
      (setq start (text-property-any next end prop nil object)))))

;; For completeness: this is to `remove-text-properties' as `put-text-property'
;; is to `add-text-properties', etc.
;(defun remove-text-property (start end property &optional object)
;  "Remove a property from text from START to END.
;Argument PROPERTY is the property to remove.
;Optional argument OBJECT is the string or buffer containing the text.
;Return t if the property was actually removed, nil otherwise."
;  (remove-text-properties start end (list property) object))

;; For consistency: maybe this should be called `remove-single-property' like
;; `next-single-property-change' (not `next-single-text-property-change'), etc.
;(defun remove-single-text-property (start end prop value &optional object)
;  "Remove a specific property value from text from START to END.
;Arguments PROP and VALUE specify the property and value to remove.  The
;resulting property values are not equal to VALUE nor lists containing VALUE.
;Optional argument OBJECT is the string or buffer containing the text."
;  (let ((start (text-property-not-all start end prop nil object)) next prev)
;    (while start
;      (setq next (next-single-property-change start prop object end)
;	    prev (get-text-property start prop object))
;      (cond ((and (symbolp prev) (eq value prev))
;	     (remove-text-property start next prop object))
;	    ((and (listp prev) (memq value prev))
;	     (let ((new (delq value prev)))
;	       (cond ((null new)
;		      (remove-text-property start next prop object))
;		     ((= (length new) 1)
;		      (put-text-property start next prop (car new) object))
;		     (t
;		      (put-text-property start next prop new object))))))
;      (setq start (text-property-not-all next end prop nil object)))))

;;; Regexp fontification functions.

(defsubst font-lock-apply-highlight (highlight)
  "Apply HIGHLIGHT following a match.
HIGHLIGHT should be of the form MATCH-HIGHLIGHT, see `font-lock-keywords'."
  (let* ((match (nth 0 highlight))
	 (start (match-beginning match)) (end (match-end match))
	 (override (nth 2 highlight)))
    (cond ((not start)
	   ;; No match but we might not signal an error.
	   (or (nth 3 highlight)
	       (error "No match %d in highlight %S" match highlight)))
	  ((not override)
	   ;; Cannot override existing fontification.
	   (or (text-property-not-all start end 'face nil)
	       (put-text-property start end 'face (eval (nth 1 highlight)))))
	  ((eq override t)
	   ;; Override existing fontification.
	   (put-text-property start end 'face (eval (nth 1 highlight))))
	  ((eq override 'prepend)
	   ;; Prepend to existing fontification.
	   (font-lock-prepend-text-property start end 'face (eval (nth 1 highlight))))
	  ((eq override 'append)
	   ;; Append to existing fontification.
	   (font-lock-append-text-property start end 'face (eval (nth 1 highlight))))
	  ((eq override 'keep)
	   ;; Keep existing fontification.
	   (font-lock-fillin-text-property start end 'face (eval (nth 1 highlight)))))))

(defsubst font-lock-fontify-anchored-keywords (keywords limit)
  "Fontify according to KEYWORDS until LIMIT.
KEYWORDS should be of the form MATCH-ANCHORED, see `font-lock-keywords',
LIMIT can be modified by the value of its PRE-MATCH-FORM."
  (let ((matcher (nth 0 keywords)) (lowdarks (nthcdr 3 keywords)) highlights
	;; Evaluate PRE-MATCH-FORM.
	(pre-match-value (eval (nth 1 keywords))))
    ;; Set LIMIT to value of PRE-MATCH-FORM or the end of line.
    (if (and (numberp pre-match-value) (> pre-match-value (point)))
	(setq limit pre-match-value)
      (save-excursion (end-of-line) (setq limit (point))))
    (save-match-data
      ;; Find an occurrence of `matcher' before `limit'.
      (while (if (stringp matcher)
		 (re-search-forward matcher limit t)
	       (funcall matcher limit))
	;; Apply each highlight to this instance of `matcher'.
	(setq highlights lowdarks)
	(while highlights
	  (font-lock-apply-highlight (car highlights))
	  (setq highlights (cdr highlights)))))
    ;; Evaluate POST-MATCH-FORM.
    (eval (nth 2 keywords))))

(defun font-lock-fontify-keywords-region (start end &optional loudly)
  "Fontify according to `font-lock-keywords' between START and END.
START should be at the beginning of a line."
  (let ((case-fold-search font-lock-keywords-case-fold-search)
	(keywords (cdr (if (eq (car-safe font-lock-keywords) t)
			   font-lock-keywords
			 (font-lock-compile-keywords))))
	(bufname (buffer-name)) (count 0)
	keyword matcher highlights)
    ;;
    ;; Fontify each item in `font-lock-keywords' from `start' to `end'.
    (while keywords
      (if loudly (message "Fontifying %s... (regexps..%s)" bufname
			  (make-string (incf count) ?.)))
      ;;
      ;; Find an occurrence of `matcher' from `start' to `end'.
      (setq keyword (car keywords) matcher (car keyword))
      (goto-char start)
      (while (if (stringp matcher)
		 (re-search-forward matcher end t)
	       (funcall matcher end))
	;; Apply each highlight to this instance of `matcher', which may be
	;; specific highlights or more keywords anchored to `matcher'.
	(setq highlights (cdr keyword))
	(while highlights
	  (if (numberp (car (car highlights)))
	      (font-lock-apply-highlight (car highlights))
	    (font-lock-fontify-anchored-keywords (car highlights) end))
	  (setq highlights (cdr highlights))))
      (setq keywords (cdr keywords)))))

;; Various functions.

(defun font-lock-compile-keywords (&optional keywords)
  ;; Compile `font-lock-keywords' into the form (t KEYWORD ...) where KEYWORD
  ;; is the (MATCHER HIGHLIGHT ...) shown in the variable's doc string.
  (let ((keywords (or keywords font-lock-keywords)))
    (setq font-lock-keywords 
	  (if (eq (car-safe keywords) t)
	      keywords
	    (cons t (mapcar 'font-lock-compile-keyword keywords))))))

(defun font-lock-compile-keyword (keyword)
  (cond ((nlistp keyword)			; MATCHER
	 (list keyword '(0 font-lock-keyword-face)))
	((eq (car keyword) 'eval)		; (eval . FORM)
	 (font-lock-compile-keyword (eval (cdr keyword))))
	((eq (car-safe (cdr keyword)) 'quote)	; (MATCHER . 'FORM)
	 ;; If FORM is a FACENAME then quote it.  Otherwise ignore the quote.
	 (if (symbolp (nth 2 keyword))
	     (list (car keyword) (list 0 (cdr keyword)))
	   (font-lock-compile-keyword (cons (car keyword) (nth 2 keyword)))))
	((numberp (cdr keyword))		; (MATCHER . MATCH)
	 (list (car keyword) (list (cdr keyword) 'font-lock-keyword-face)))
	((symbolp (cdr keyword))		; (MATCHER . FACENAME)
	 (list (car keyword) (list 0 (cdr keyword))))
	((nlistp (nth 1 keyword))		; (MATCHER . HIGHLIGHT)
	 (list (car keyword) (cdr keyword)))
	(t					; (MATCHER HIGHLIGHT ...)
	 keyword)))

(defun font-lock-value-in-major-mode (alist)
  ;; Return value in ALIST for `major-mode', or ALIST if it is not an alist.
  ;; Structure is ((MAJOR-MODE . VALUE) ...) where MAJOR-MODE may be t.
  (if (consp alist)
      (cdr (or (assq major-mode alist) (assq t alist)))
    alist))

(defun font-lock-choose-keywords (keywords level)
  ;; Return LEVELth element of KEYWORDS.  A LEVEL of nil is equal to a
  ;; LEVEL of 0, a LEVEL of t is equal to (1- (length KEYWORDS)).
  (cond ((symbolp keywords)
	 keywords)
	((numberp level)
	 (or (nth level keywords) (car (reverse keywords))))
	((eq level t)
	 (car (reverse keywords)))
	(t
	 (car keywords))))

(defvar font-lock-set-defaults nil)	; Whether we have set up defaults.

(defun font-lock-set-defaults ()
  "Set fontification defaults appropriately for this mode.
Sets various variables using `font-lock-defaults' (or, if nil, using
`font-lock-defaults-alist') and `font-lock-maximum-decoration'."
  ;; Set face defaults.
  (font-lock-make-faces)
  ;; Set fontification defaults.
  (make-local-variable 'font-lock-fontified)
  ;; Set iff not previously set.
  (unless font-lock-set-defaults
    (set (make-local-variable 'font-lock-set-defaults)		t)
    (set (make-local-variable 'font-lock-cache-state)		nil)
    (set (make-local-variable 'font-lock-cache-position)	(make-marker))
    (let* ((defaults (or font-lock-defaults
			 (cdr (assq major-mode font-lock-defaults-alist))))
	   (keywords
	    (font-lock-choose-keywords (nth 0 defaults)
	     (font-lock-value-in-major-mode font-lock-maximum-decoration)))
	   (local (cdr (assq major-mode font-lock-keywords-alist))))
      ;; Regexp fontification?
      (setq font-lock-keywords (if (fboundp keywords)
				   (funcall keywords)
				 (eval keywords)))
      ;; Local fontification?
      (while local
	(font-lock-add-keywords nil (car (car local)) (cdr (car local)))
	(setq local (cdr local)))
      ;; Syntactic fontification?
      (when (nth 1 defaults)
	(set (make-local-variable 'font-lock-keywords-only) t))
      ;; Case fold during regexp fontification?
      (when (nth 2 defaults)
	(set (make-local-variable 'font-lock-keywords-case-fold-search) t))
      ;; Syntax table for regexp and syntactic fontification?
      (when (nth 3 defaults)
	(let ((slist (nth 3 defaults)))
	  (set (make-local-variable 'font-lock-syntax-table)
	       (copy-syntax-table (syntax-table)))
	  (while slist
	    ;; The character to modify may be a single CHAR or a STRING.
	    (let ((chars (if (numberp (car (car slist)))
			     (list (car (car slist)))
			   (mapcar 'identity (car (car slist)))))
		  (syntax (cdr (car slist))))
	      (while chars
		(modify-syntax-entry (car chars) syntax
				     font-lock-syntax-table)
		(setq chars (cdr chars)))
	      (setq slist (cdr slist))))))
      ;; Syntax function for syntactic fontification?
      (when (nth 4 defaults)
	(set (make-local-variable 'font-lock-beginning-of-syntax-function)
	     (nth 4 defaults)))
      ;; Variable alist?
      (let ((alist (nthcdr 5 defaults)))
	(while alist
	  (set (make-local-variable (car (car alist))) (cdr (car alist)))
	  (setq alist (cdr alist)))))))

(defun font-lock-unset-defaults ()
  "Unset fontification defaults.  See `font-lock-set-defaults'."
  (setq font-lock-set-defaults			nil
	font-lock-keywords			nil
	font-lock-keywords-only			nil
	font-lock-keywords-case-fold-search	nil
	font-lock-syntax-table			nil
	font-lock-beginning-of-syntax-function	nil)
  (let* ((defaults (or font-lock-defaults
		       (cdr (assq major-mode font-lock-defaults-alist))))
	 (alist (nthcdr 5 defaults)))
    (while alist
      (set (car (car alist)) (default-value (car (car alist))))
      (setq alist (cdr alist)))))

;; Colour etc. support.

;; This section of code is crying out for revision.  Come on down, custom.el?

;; To begin with, `display-type' and `background-mode' are `frame-parameters'
;; so we don't have to calculate them here anymore.  But all the face stuff
;; should be frame-local (and thus display-local) anyway.  Because we're not
;; sure what support Emacs is going to have for general frame-local face
;; attributes, we leave this section of code as it is.  For now.  sm.

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

where FACE could be one of the face symbols `font-lock-comment-face',
`font-lock-string-face', `font-lock-keyword-face', `font-lock-builtin-face',
`font-lock-type-face', `font-lock-function-name-face',
`font-lock-variable-name-face', `font-lock-reference-face' and
`font-lock-warning-face', or any other face symbols and attributes may be
specified here and used in `font-lock-keywords'.

Subsequent element items should be the attributes for the corresponding
Font Lock mode faces.  Attributes FOREGROUND and BACKGROUND should be strings
\(default if nil), while BOLD-P, ITALIC-P, and UNDERLINE-P should specify the
corresponding face attributes (yes if non-nil).  For example:

 (setq font-lock-face-attributes '((font-lock-warning-face \"HotPink\" nil t t)
				   (font-lock-comment-face \"Red\")))

in your ~/.emacs makes a garish bold-italic warning face and red comment face.

Emacs uses default attributes based on display type and background brightness.
See variables `font-lock-display-type' and `font-lock-background-mode'.

Resources can be used to over-ride these face attributes.  For example, the
resource `Emacs.font-lock-comment-face.attributeUnderline' can be used to
specify the UNDERLINE-P attribute for face `font-lock-comment-face'.")

(defun font-lock-make-faces (&optional override)
  "Make faces from `font-lock-face-attributes'.
A default list is used if this is nil.
If optional OVERRIDE is non-nil, faces that already exist are reset.
See `font-lock-make-face' and `list-faces-display'."
  ;; We don't need to `setq' any of these variables, but the user can see what
  ;; is being used if we do.
  (unless font-lock-display-type
    (setq font-lock-display-type
     (let ((display-resource (x-get-resource ".displayType" "DisplayType")))
       (cond (display-resource (intern (downcase display-resource)))
	     ((x-display-color-p) 'color)
	     ((x-display-grayscale-p) 'grayscale)
	     (t 'mono)))))
  (unless font-lock-background-mode
    (setq font-lock-background-mode
     (let ((bg-resource (x-get-resource ".backgroundMode" "BackgroundMode"))
	   (params (frame-parameters)))
       (cond (bg-resource (intern (downcase bg-resource)))
	     ((eq system-type 'ms-dos)
	      (if (string-match "light" (cdr (assq 'background-color params)))
		  'light
		'dark))
	     ((< (apply '+ (x-color-values
			    (cdr (assq 'background-color params))))
		 (* (apply '+ (x-color-values "white")) .6))
	      'dark)
	     (t 'light)))))
  (let ((face-attributes
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
			'(font-lock-builtin-face nil nil t nil nil)
			(list
			 'font-lock-function-name-face
			 (cdr (assq 'background-color (frame-parameters)))
			 (cdr (assq 'foreground-color (frame-parameters)))
			 t nil nil)
			'(font-lock-variable-name-face nil nil t t nil)
			'(font-lock-type-face nil nil t nil t)
			'(font-lock-reference-face nil nil t nil t)
			(list
			 'font-lock-warning-face
			 (cdr (assq 'background-color (frame-parameters)))
			 (cdr (assq 'foreground-color (frame-parameters)))
			 t nil nil)))
		 ((memq font-lock-display-type '(grayscale greyscale
						 grayshade greyshade))
		  (list
		   (list 'font-lock-comment-face
			 (if light-bg "DimGray" "LightGray") nil t t nil)
		   (list 'font-lock-string-face
			 (if light-bg "DimGray" "LightGray") nil nil t nil)
		   (list 'font-lock-keyword-face
			 nil (if light-bg "LightGray" "DimGray") t nil nil)
		   (list 'font-lock-builtin-face
			 nil (if light-bg "LightGray" "DimGray") t nil nil)
		   (list 'font-lock-function-name-face
			 (cdr (assq 'background-color (frame-parameters)))
			 (cdr (assq 'foreground-color (frame-parameters)))
			 t nil nil)
		   (list 'font-lock-variable-name-face
			 nil (if light-bg "Gray90" "DimGray") t t nil)
		   (list 'font-lock-type-face
			 nil (if light-bg "Gray80" "DimGray") t nil nil)
		   (list 'font-lock-reference-face
			 nil (if light-bg "LightGray" "Gray50") t nil t)
		   (list 'font-lock-warning-face
			 (cdr (assq 'background-color (frame-parameters)))
			 (cdr (assq 'foreground-color (frame-parameters)))
			 t nil nil)))
		 (light-bg		; light colour background
		  '((font-lock-comment-face "Firebrick")
		    (font-lock-string-face "RosyBrown")
		    (font-lock-keyword-face "Purple")
		    (font-lock-builtin-face "Orchid")
		    (font-lock-function-name-face "Blue")
		    (font-lock-variable-name-face "DarkGoldenrod")
		    (font-lock-type-face "DarkOliveGreen")
		    (font-lock-reference-face "CadetBlue")
		    (font-lock-warning-face "Red" nil t nil nil)))
		 (t			; dark colour background
		  '((font-lock-comment-face "OrangeRed")
		    (font-lock-string-face "LightSalmon")
		    (font-lock-keyword-face "Cyan")
		    (font-lock-builtin-face "LightSteelBlue")
		    (font-lock-function-name-face "LightSkyBlue")
		    (font-lock-variable-name-face "LightGoldenrod")
		    (font-lock-type-face "PaleGreen")
		    (font-lock-reference-face "Aquamarine")
		    (font-lock-warning-face "Pink" nil t nil nil)))))))
    (while face-attributes
      (unless (assq (car (car face-attributes)) font-lock-face-attributes)
	(push (car face-attributes) font-lock-face-attributes))
      (setq face-attributes (cdr face-attributes))))
  ;; Now make the faces if we have to.
  (mapcar (function
	   (lambda (face-attributes)
	     (let ((face (nth 0 face-attributes)))
	       (cond (override
		      ;; We can stomp all over it anyway.  Get outta my face!
		      (font-lock-make-face face-attributes))
		     ((and (boundp face) (facep (symbol-value face)))
		      ;; The variable exists and is already bound to a face.
		      nil)
		     ((facep face)
		      ;; We already have a face so we bind the variable to it.
		      (set face face))
		     (t
		      ;; No variable or no face.
		      (font-lock-make-face face-attributes))))))
	  font-lock-face-attributes))

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
    (add-to-list 'facemenu-unlisted-faces face)
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

;;; Menu support.

;; This section of code is commented out because Emacs does not have real menu
;; buttons.  (We can mimic them by putting "( ) " or "(X) " at the beginning of
;; the menu entry text, but with Xt it looks both ugly and embarrassingly
;; amateur.)  If/When Emacs gets real menus buttons, put in menu-bar.el after
;; the entry for "Text Properties" something like:
;;
;; (define-key menu-bar-edit-menu [font-lock]
;;   '("Syntax Highlighting" . font-lock-menu))
;;
;; and remove a single ";" from the beginning of each line in the rest of this
;; section.  Probably the mechanism for telling the menu code what are menu
;; buttons and when they are on or off needs tweaking.  I have assumed that the
;; mechanism is via `menu-toggle' and `menu-selected' symbol properties.  sm.

;;;;###autoload
;(progn
;  ;; Make the Font Lock menu.
;  (defvar font-lock-menu (make-sparse-keymap "Syntax Highlighting"))
;  ;; Add the menu items in reverse order.
;  (define-key font-lock-menu [fontify-less]
;    '("Less In Current Buffer" . font-lock-fontify-less))
;  (define-key font-lock-menu [fontify-more]
;    '("More In Current Buffer" . font-lock-fontify-more))
;  (define-key font-lock-menu [font-lock-sep]
;    '("--"))
;  (define-key font-lock-menu [font-lock-mode]
;    '("In Current Buffer" . font-lock-mode))
;  (define-key font-lock-menu [global-font-lock-mode]
;    '("In All Buffers" . global-font-lock-mode)))
;
;;;;###autoload
;(progn
;  ;; We put the appropriate `menu-enable' etc. symbol property values on when
;  ;; font-lock.el is loaded, so we don't need to autoload the three variables.
;  (put 'global-font-lock-mode 'menu-toggle t)
;  (put 'font-lock-mode 'menu-toggle t)
;  (put 'font-lock-fontify-more 'menu-enable '(identity))
;  (put 'font-lock-fontify-less 'menu-enable '(identity)))
;
;;; Put the appropriate symbol property values on now.  See above.
;(put 'global-font-lock-mode 'menu-selected 'global-font-lock-mode))
;(put 'font-lock-mode 'menu-selected 'font-lock-mode)
;(put 'font-lock-fontify-more 'menu-enable '(nth 2 font-lock-fontify-level))
;(put 'font-lock-fontify-less 'menu-enable '(nth 1 font-lock-fontify-level))
;
;(defvar font-lock-fontify-level nil)	; For less/more fontification.
;
;(defun font-lock-fontify-level (level)
;  (let ((font-lock-maximum-decoration level))
;    (when font-lock-mode
;      (font-lock-mode))
;    (font-lock-mode)
;    (when font-lock-verbose
;      (message "Fontifying %s... level %d" (buffer-name) level))))
;
;(defun font-lock-fontify-less ()
;  "Fontify the current buffer with less decoration.
;See `font-lock-maximum-decoration'."
;  (interactive)
;  ;; Check in case we get called interactively.
;  (if (nth 1 font-lock-fontify-level)
;      (font-lock-fontify-level (1- (car font-lock-fontify-level)))
;    (error "No less decoration")))
;
;(defun font-lock-fontify-more ()
;  "Fontify the current buffer with more decoration.
;See `font-lock-maximum-decoration'."
;  (interactive)
;  ;; Check in case we get called interactively.
;  (if (nth 2 font-lock-fontify-level)
;      (font-lock-fontify-level (1+ (car font-lock-fontify-level)))
;    (error "No more decoration")))
;
;;; This should be called by `font-lock-set-defaults'.
;(defun font-lock-set-menu ()
;  ;; Activate less/more fontification entries if there are multiple levels for
;  ;; the current buffer.  Sets `font-lock-fontify-level' to be of the form
;  ;; (CURRENT-LEVEL IS-LOWER-LEVEL-P IS-HIGHER-LEVEL-P) for menu activation.
;  (let ((keywords (or (nth 0 font-lock-defaults)
;		      (nth 1 (assq major-mode font-lock-defaults-alist))))
;	(level (font-lock-value-in-major-mode font-lock-maximum-decoration)))
;    (make-local-variable 'font-lock-fontify-level)
;    (if (or (symbolp keywords) (= (length keywords) 1))
;	(font-lock-unset-menu)
;      (cond ((eq level t)
;	     (setq level (1- (length keywords))))
;	    ((or (null level) (zerop level))
;	     ;; The default level is usually, but not necessarily, level 1.
;	     (setq level (- (length keywords)
;			    (length (member (eval (car keywords))
;					    (mapcar 'eval (cdr keywords))))))))
;      (setq font-lock-fontify-level (list level (> level 1)
;					  (< level (1- (length keywords))))))))
;
;;; This should be called by `font-lock-unset-defaults'.
;(defun font-lock-unset-menu ()
;  ;; Deactivate less/more fontification entries.
;  (setq font-lock-fontify-level nil))

;;; Various regexp information shared by several modes.
;;; Information specific to a single mode should go in its load library.

;; Font Lock support for C, C++, Objective-C and Java modes will one day be in
;; some cc-font.el (and required by cc-mode.el).  However, the below function
;; should stay in font-lock.el, since it is used by other libraries.  sm.

(defun font-lock-match-c-style-declaration-item-and-skip-to-next (limit)
  "Match, and move over, any declaration/definition item after point.
Matches after point, but ignores leading whitespace and `*' characters.
Does not move further than LIMIT.

The expected syntax of a declaration/definition item is `word', possibly ending
with optional whitespace and a `('.  Everything following the item (but
belonging to it) is expected to by skip-able by `scan-sexps', and items are
expected to be separated with a `,' and to be terminated with a `;'.

Thus the regexp matches after point:	word (
					^^^^ ^
Where the match subexpressions are:	  1  2

The item is delimited by (match-beginning 1) and (match-end 1).
If (match-beginning 2) is non-nil, the item is followed by a `('.

This function could be MATCHER in a MATCH-ANCHORED `font-lock-keywords' item."
  (when (looking-at "[ \t*]*\\(\\sw+\\)[ \t]*\\((\\)?")
    (save-match-data
      (condition-case nil
	  (save-restriction
	    ;; Restrict to the end of line, currently guaranteed to be LIMIT.
	    (narrow-to-region (point-min) limit)
	    (goto-char (match-end 1))
	    ;; Move over any item value, etc., to the next item.
	    (while (not (looking-at "[ \t]*\\(\\(,\\)\\|;\\|$\\)"))
	      (goto-char (or (scan-sexps (point) 1) (point-max))))
	    (goto-char (match-end 2)))
	(error t)))))


(defconst lisp-font-lock-keywords-1
  (eval-when-compile
    (list
     ;; Anything not a variable or type declaration is fontified as a function.
     ;; It would be cleaner to allow preceding whitespace, but it would also be
     ;; about five times slower.
     (list (concat "^(\\(def\\("
		   ;; Variable declarations.
		   "\\(const\\|custom\\|face\\|var\\)\\|"
		   ;; Structure declarations.
		   "\\(class\\|group\\|struct\\|type\\)\\|"
		   ;; Everything else is a function declaration.
		   "\\sw+"
		   "\\)\\)\\>"
		   ;; Any whitespace and declared object.
		   "[ \t'\(]*"
		   "\\(\\sw+\\)?")
	   '(1 font-lock-keyword-face)
	   '(5 (cond ((match-beginning 3) font-lock-variable-name-face)
		     ((match-beginning 4) font-lock-type-face)
		     (t font-lock-function-name-face))
	       nil t))
     ))
  "Subdued level highlighting for Lisp modes.")

(defconst lisp-font-lock-keywords-2
  (append lisp-font-lock-keywords-1
   (eval-when-compile
     (list
      ;;
      ;; Control structures.  Common ELisp and CLisp forms combined.
;      (make-regexp
;       '("cond" "if" "while" "let\\*?" "prog[nv12*]?" "inline" "catch" "throw"
;	 "save-restriction" "save-excursion" "save-window-excursion"
;	 "save-selected-window" "save-match-data" "save-current-buffer"
;	 "unwind-protect" "condition-case" "track-mouse" "dont-compile"
;	 "eval-after-load" "eval-and-compile" "eval-when-compile"
;	 "when" "unless" "do" "flet" "labels" "return" "return-from"
;	 "with-output-to-temp-buffer" "with-timeout" "with-current-buffer"
;	 "with-temp-buffer" "with-temp-file"))
      (cons (concat "(\\("
		    "c\\(atch\\|ond\\(\\|ition-case\\)\\)\\|"
		    "do\\(\\|nt-compile\\)\\|"
		    "eval-\\(a\\(fter-load\\|nd-compile\\)\\|"
		    "when-compile\\)\\|flet\\|i\\(f\\|nline\\)\\|"
		    "l\\(abels\\|et\\*?\\)\\|prog[nv12*]?\\|"
		    "return\\(\\|-from\\)\\|"
		    "save-\\(current-buffer\\|excursion\\|match-data\\|"
		    "restriction\\|selected-window\\|window-excursion\\)\\|"
		    "t\\(hrow\\|rack-mouse\\)\\|un\\(less\\|wind-protect\\)\\|"
		    "w\\(h\\(en\\|ile\\)\\|ith-\\(current-buffer\\|"
		    "output-to-temp-buffer\\|"
		    "t\\(emp-\\(buffer\\|file\\)\\|imeout\\)\\)\\)"
		    "\\)\\>")
	    1)
      ;;
      ;; Feature symbols as references.
      '("(\\(featurep\\|provide\\|require\\)\\>[ \t']*\\(\\sw+\\)?"
	(1 font-lock-keyword-face) (2 font-lock-reference-face nil t))
      ;;
      ;; Words inside \\[] tend to be for `substitute-command-keys'.
      '("\\\\\\\\\\[\\(\\sw+\\)]" 1 font-lock-reference-face prepend)
      ;;
      ;; Words inside `' tend to be symbol names.
      '("`\\(\\sw\\sw+\\)'" 1 font-lock-reference-face prepend)
      ;;
      ;; CLisp `:' keywords as builtins.
      '("\\<:\\sw\\sw+\\>" 0 font-lock-builtin-face)
      ;;
      ;; ELisp and CLisp `&' keywords as types.
      '("\\<\\&\\sw+\\>" . font-lock-type-face)
      )))
  "Gaudy level highlighting for Lisp modes.")


(defvar lisp-font-lock-keywords lisp-font-lock-keywords-1
  "Default expressions to highlight in Lisp modes.")


(defvar scheme-font-lock-keywords
  (eval-when-compile
    (list
     ;;
     ;; Declarations.  Hannes Haug <hannes.haug@student.uni-tuebingen.de> says
     ;; this works for SOS, STklos, SCOOPS, Meroon and Tiny CLOS.
     (list (concat "(\\(define\\("
		   ;; Function names.
		   "\\(\\|-\\(generic\\(\\|-procedure\\)\\|method\\)\\)\\|"
		   ;; Macro names, as variable names.  A bit dubious, this.
		   "\\(-syntax\\)\\|"
		   ;; Class names.
		   "-class"
		   "\\)\\)\\>"
		   ;; Any whitespace and declared object.
		   "[ \t]*(?"
		   "\\(\\sw+\\)?")
	   '(1 font-lock-keyword-face)
	   '(7 (cond ((match-beginning 3) font-lock-function-name-face)
		     ((match-beginning 6) font-lock-variable-name-face)
		     (t font-lock-type-face))
	       nil t))
     ;;
     ;; Control structures.
;(make-regexp '("begin" "call-with-current-continuation" "call/cc"
;	       "call-with-input-file" "call-with-output-file" "case" "cond"
;	       "do" "else" "for-each" "if" "lambda"
;	       "let\\*?" "let-syntax" "letrec" "letrec-syntax"
;	       ;; Hannes Haug <hannes.haug@student.uni-tuebingen.de> wants:
;	       "and" "or" "delay"
;	       ;; Stefan Monnier <stefan.monnier@epfl.ch> says don't bother:
;	       ;;"quasiquote" "quote" "unquote" "unquote-splicing"
;	       "map" "syntax" "syntax-rules"))
     (cons
      (concat "(\\("
	      "and\\|begin\\|c\\(a\\(ll\\(-with-\\(current-continuation\\|"
	      "input-file\\|output-file\\)\\|/cc\\)\\|se\\)\\|ond\\)\\|"
	      "d\\(elay\\|o\\)\\|else\\|for-each\\|if\\|"
	      "l\\(ambda\\|et\\(-syntax\\|\\*?\\|rec\\(\\|-syntax\\)\\)\\)\\|"
	      "map\\|or\\|syntax\\(\\|-rules\\)"
	      "\\)\\>") 1)
     ;;
     ;; David Fox <fox@graphics.cs.nyu.edu> for SOS/STklos class specifiers.
     '("\\<<\\sw+>\\>" . font-lock-type-face)
     ;;
     ;; Scheme `:' keywords as references.
     '("\\<:\\sw+\\>" . font-lock-reference-face)
     ))
  "Default expressions to highlight in Scheme modes.")


(defvar tex-font-lock-keywords
;  ;; Regexps updated with help from Ulrik Dickow <dickow@nbi.dk>.
;  '(("\\\\\\(begin\\|end\\|newcommand\\){\\([a-zA-Z0-9\\*]+\\)}"
;     2 font-lock-function-name-face)
;    ("\\\\\\(cite\\|label\\|pageref\\|ref\\){\\([^} \t\n]+\\)}"
;     2 font-lock-reference-face)
;    ;; It seems a bit dubious to use `bold' and `italic' faces since we might
;    ;; not be able to display those fonts.
;    ("{\\\\bf\\([^}]+\\)}" 1 'bold keep)
;    ("{\\\\\\(em\\|it\\|sl\\)\\([^}]+\\)}" 2 'italic keep)
;    ("\\\\\\([a-zA-Z@]+\\|.\\)" . font-lock-keyword-face)
;    ("^[ \t\n]*\\\\def[\\\\@]\\(\\w+\\)" 1 font-lock-function-name-face keep))
  ;; Rewritten and extended for LaTeX2e by Ulrik Dickow <dickow@nbi.dk>.
  '(("\\\\\\(begin\\|end\\|newcommand\\){\\([a-zA-Z0-9\\*]+\\)}"
     2 font-lock-function-name-face)
    ("\\\\\\(cite\\|label\\|pageref\\|ref\\){\\([^} \t\n]+\\)}"
     2 font-lock-reference-face)
    ("^[ \t]*\\\\def\\\\\\(\\(\\w\\|@\\)+\\)" 1 font-lock-function-name-face)
    "\\\\\\([a-zA-Z@]+\\|.\\)"
    ;; It seems a bit dubious to use `bold' and `italic' faces since we might
    ;; not be able to display those fonts.
    ;; LaTeX2e: \emph{This is emphasized}.
    ("\\\\emph{\\([^}]+\\)}" 1 'italic keep)
    ;; LaTeX2e: \textbf{This is bold}, \textit{...}, \textsl{...}
    ("\\\\text\\(\\(bf\\)\\|it\\|sl\\){\\([^}]+\\)}"
     3 (if (match-beginning 2) 'bold 'italic) keep)
    ;; Old-style bf/em/it/sl. Stop at `\\' and un-escaped `&', for good tables.
    ("\\\\\\(\\(bf\\)\\|em\\|it\\|sl\\)\\>\\(\\([^}&\\]\\|\\\\[^\\]\\)+\\)"
     3 (if (match-beginning 2) 'bold 'italic) keep))
  "Default expressions to highlight in TeX modes.")

;;; User choices.

;; These provide a means to fontify types not defined by the language.  Those
;; types might be the user's own or they might be generally accepted and used.
;; Generally excepted types are used to provide default variable values.

(defvar c-font-lock-extra-types '("FILE" "\\sw+_t")
  "*List of extra types to fontify in C mode.
Each list item should be a regexp without word-delimiters or parentheses.
For example, a value of (\"FILE\" \"\\\\sw+_t\") means the word FILE and words
ending in _t are treated as type names.")

(defvar c++-font-lock-extra-types nil
  "*List of extra types to fontify in C++ mode.
Each list item should be a regexp without word-delimiters or parentheses.
For example, a value of (\"String\") means the word String is treated as a type
name.")

(defvar objc-font-lock-extra-types '("Class" "BOOL" "IMP" "SEL")
  "*List of extra types to fontify in Objective-C mode.
Each list item should be a regexp without word-delimiters or parentheses.
For example, a value of (\"Class\" \"BOOL\" \"IMP\" \"SEL\") means the words
Class, BOOL, IMP and SEL are treated as type names.")

(defvar java-font-lock-extra-types '("[A-Z\300-\326\330-\337]\\sw+")
  "*List of extra types to fontify in Java mode.
Each list item should be a regexp without word-delimiters or parentheses.
For example, a value of (\"[A-Z\300-\326\330-\337]\\\\sw+\") means capitalised
words (and words conforming to the Java id spec) are treated as type names.")

;;; C.

;; [Murmur murmur murmur] Maestro, drum-roll please...  [Murmur murmur murmur.]
;; Ahem.  [Murmur murmur murmur] Lay-dees an Gennel-men.  [Murmur murmur shhh!]
;; I am most proud and humbly honoured today [murmur murmur cough] to present
;; to you good people, the winner of the Second Millennium Award for The Most
;; Hairy Language Syntax.  [Ahhh!]  All rise please.  [Shuffle shuffle
;; shuffle.]  And a round of applause please.  For...  The C Language!  [Roar.]
;;
;; Thank you...  You are too kind...  It is with a feeling of great privilege
;; and indeed emotion [sob] that I accept this award.  It has been a long hard
;; road.  But we know our destiny.  And our future.  For we must not rest.
;; There are more tokens to overload, more shoehorn, more methodologies.  But
;; more is a plus!  [Ha ha ha.]  And more means plus!  [Ho ho ho.]  The future
;; is C++!  [Ohhh!]  The Third Millennium Award...  Will be ours!  [Roar.]

(defconst c-font-lock-keywords-1 nil
  "Subdued level highlighting for C mode.")

(defconst c-font-lock-keywords-2 nil
  "Medium level highlighting for C mode.
See also `c-font-lock-extra-types'.")

(defconst c-font-lock-keywords-3 nil
  "Gaudy level highlighting for C mode.
See also `c-font-lock-extra-types'.")

(let ((c-keywords
;      ("break" "continue" "do" "else" "for" "if" "return" "switch" "while")
       "break\\|continue\\|do\\|else\\|for\\|if\\|return\\|switch\\|while")
      (c-type-types
;      ("auto" "extern" "register" "static" "typedef" "struct" "union" "enum"
;	"signed" "unsigned" "short" "long" "int" "char" "float" "double"
;	"void" "volatile" "const")
       `(mapconcat 'identity
	 (cons 
	  (,@ (concat "auto\\|c\\(har\\|onst\\)\\|double\\|"	; 6 ()s deep.
		      "e\\(num\\|xtern\\)\\|float\\|int\\|long\\|register\\|"
		      "s\\(hort\\|igned\\|t\\(atic\\|ruct\\)\\)\\|typedef\\|"
		      "un\\(ion\\|signed\\)\\|vo\\(id\\|latile\\)"))
	  c-font-lock-extra-types)
	 "\\|"))
      )
 (setq c-font-lock-keywords-1
  (list
   ;;
   ;; These are all anchored at the beginning of line for speed.
   ;; Note that `c++-font-lock-keywords-1' depends on `c-font-lock-keywords-1'.
   ;;
   ;; Fontify function name definitions (GNU style; without type on line).
   '("^\\(\\sw+\\)[ \t]*(" 1 font-lock-function-name-face)
   ;;
   ;; Fontify error directives.
   '("^#[ \t]*error[ \t]+\\(.+\\)" 1 font-lock-warning-face prepend)
   ;;
   ;; Fontify filenames in #include <...> preprocessor directives as strings.
   '("^#[ \t]*\\(import\\|include\\)[ \t]+\\(<[^>\"\n]*>?\\)"
     2 font-lock-string-face)
   ;;
   ;; Fontify function macro names.
   '("^#[ \t]*define[ \t]+\\(\\sw+\\)(" 1 font-lock-function-name-face)
   ;;
   ;; Fontify symbol names in #elif or #if ... defined preprocessor directives.
   '("^#[ \t]*\\(elif\\|if\\)\\>"
     ("\\<\\(defined\\)\\>[ \t]*(?\\(\\sw+\\)?" nil nil
      (1 font-lock-reference-face) (2 font-lock-variable-name-face nil t)))
   ;;
   ;; Fontify otherwise as symbol names, and the preprocessor directive names.
   '("^#[ \t]*\\(\\sw+\\)\\>[ \t]*\\(\\sw+\\)?"
     (1 font-lock-reference-face) (2 font-lock-variable-name-face nil t))
   ))

 (setq c-font-lock-keywords-2
  (append c-font-lock-keywords-1
   (list
    ;;
    ;; Simple regexps for speed.
    ;;
    ;; Fontify all type specifiers.
    `(eval .
      (cons (concat "\\<\\(" (,@ c-type-types) "\\)\\>") 'font-lock-type-face))
    ;;
    ;; Fontify all builtin keywords (except case, default and goto; see below).
    (concat "\\<\\(" c-keywords "\\)\\>")
    ;;
    ;; Fontify case/goto keywords and targets, and case default/goto tags.
    '("\\<\\(case\\|goto\\)\\>[ \t]*\\(-?\\sw+\\)?"
      (1 font-lock-keyword-face) (2 font-lock-reference-face nil t))
    ;; Anders Lindgren <andersl@csd.uu.se> points out that it is quicker to use
    ;; MATCH-ANCHORED to effectively anchor the regexp on the left.
    '(":" ("^[ \t]*\\(\\sw+\\)[ \t]*:"
	   (beginning-of-line) (end-of-line)
	   (1 font-lock-reference-face)))
    )))

 (setq c-font-lock-keywords-3
  (append c-font-lock-keywords-2
   ;;
   ;; More complicated regexps for more complete highlighting for types.
   ;; We still have to fontify type specifiers individually, as C is so hairy.
   (list
    ;;
    ;; Fontify all storage classes and type specifiers, plus their items.
    `(eval .
      (list (concat "\\<\\(" (,@ c-type-types) "\\)\\>"
		    "\\([ \t*&]+\\sw+\\>\\)*")
	    ;; Fontify each declaration item.
	    '(font-lock-match-c-style-declaration-item-and-skip-to-next
	      ;; Start with point after all type specifiers.
	      (goto-char (or (match-beginning 8) (match-end 1)))
	      ;; Finish with point after first type specifier.
	      (goto-char (match-end 1))
	      ;; Fontify as a variable or function name.
	      (1 (if (match-beginning 2)
		     font-lock-function-name-face
		   font-lock-variable-name-face)))))
    ;;
    ;; Fontify structures, or typedef names, plus their items.
    '("\\(}\\)[ \t*]*\\sw"
      (font-lock-match-c-style-declaration-item-and-skip-to-next
       (goto-char (match-end 1)) nil
       (1 (if (match-beginning 2)
	      font-lock-function-name-face
	    font-lock-variable-name-face))))
    ;;
    ;; Fontify anything at beginning of line as a declaration or definition.
    '("^\\(\\sw+\\)\\>\\([ \t*]+\\sw+\\>\\)*"
      (1 font-lock-type-face)
      (font-lock-match-c-style-declaration-item-and-skip-to-next
       (goto-char (or (match-beginning 2) (match-end 1))) nil
       (1 (if (match-beginning 2)
	      font-lock-function-name-face
	    font-lock-variable-name-face))))
    )))
 )

(defvar c-font-lock-keywords c-font-lock-keywords-1
  "Default expressions to highlight in C mode.
See also `c-font-lock-extra-types'.")

;;; C++.

(defconst c++-font-lock-keywords-1 nil
  "Subdued level highlighting for C++ mode.")

(defconst c++-font-lock-keywords-2 nil
  "Medium level highlighting for C++ mode.
See also `c++-font-lock-extra-types'.")

(defconst c++-font-lock-keywords-3 nil
  "Gaudy level highlighting for C++ mode.
See also `c++-font-lock-extra-types'.")

(defun font-lock-match-c++-style-declaration-item-and-skip-to-next (limit)
  ;; Regexp matches after point:		word<word>::word (
  ;;						^^^^ ^^^^   ^^^^ ^
  ;; Where the match subexpressions are:	  1    3      5  6
  ;;
  ;; Item is delimited by (match-beginning 1) and (match-end 1).
  ;; If (match-beginning 3) is non-nil, that part of the item incloses a `<>'.
  ;; If (match-beginning 5) is non-nil, that part of the item follows a `::'.
  ;; If (match-beginning 6) is non-nil, the item is followed by a `('.
  (when (looking-at (eval-when-compile
		      (concat "[ \t*&]*\\(\\sw+\\)"
			      "\\(<\\(\\sw+\\)[ \t*&]*>\\)?"
			      "\\(::\\*?\\(\\sw+\\)\\)?"
			      "[ \t]*\\((\\)?")))
    (save-match-data
      (condition-case nil
	  (save-restriction
	    ;; Restrict to the end of line, currently guaranteed to be LIMIT.
	    (narrow-to-region (point-min) limit)
	    (goto-char (match-end 1))
	    ;; Move over any item value, etc., to the next item.
	    (while (not (looking-at "[ \t]*\\(\\(,\\)\\|;\\|$\\)"))
	      (goto-char (or (scan-sexps (point) 1) (point-max))))
	    (goto-char (match-end 2)))
	(error t)))))

(let* ((c++-keywords
;      ("break" "continue" "do" "else" "for" "if" "return" "switch" "while"
;       "asm" "catch" "delete" "new" "operator" "sizeof" "this" "throw" "try"
;       ;; Eric Hopper <hopper@omnifarious.mn.org> says these are new.
;       "static_cast" "dynamic_cast" "const_cast" "reinterpret_cast")
	(concat "asm\\|break\\|c\\(atch\\|on\\(st_cast\\|tinue\\)\\)\\|"
		"d\\(elete\\|o\\|ynamic_cast\\)\\|else\\|for\\|if\\|new\\|"
		"operator\\|re\\(interpret_cast\\|turn\\)\\|"
		"s\\(izeof\\|tatic_cast\\|"
		"witch\\)\\|t\\(h\\(is\\|row\\)\\|ry\\)\\|while"))
       (c++-operators
	(mapconcat 'identity
	 (mapcar 'regexp-quote
		 ;; Taken from Stroustrup, minus keywords otherwise fontified.
		 (sort '("+" "-" "*" "/" "%" "^" "&" "|" "~" "!" "=" "<" ">"
			 "+=" "-=" "*=" "/=" "%=" "^=" "&=" "|=" "<<" ">>"
			 ">>=" "<<=" "==" "!=" "<=" ">=" "&&" "||" "++" "--"
			 "->*" "," "->" "[]" "()")
		       #'(lambda (a b) (> (length a) (length b)))))
	 "\\|"))
       (c++-type-types
;      ("auto" "extern" "register" "static" "typedef" "struct" "union" "enum"
;       "signed" "unsigned" "short" "long" "int" "char" "float" "double"
;       "void" "volatile" "const" "inline" "friend" "bool"
;       "virtual" "complex" "template"
;       ;; Eric Hopper <hopper@omnifarious.mn.org> says these are new.
;       "namespace" "using")
	`(mapconcat 'identity
	  (cons 
	   (,@ (concat "auto\\|bool\\|c\\(har\\|o\\(mplex\\|nst\\)\\)\\|"
		       "double\\|e\\(num\\|xtern\\)\\|f\\(loat\\|riend\\)\\|"
		       "in\\(line\\|t\\)\\|long\\|namespace\\|register\\|"
		       "s\\(hort\\|igned\\|t\\(atic\\|ruct\\)\\)\\|"
		       "t\\(emplate\\|ypedef\\)\\|"
		       "u\\(n\\(ion\\|signed\\)\\|sing\\)\\|"
		       "v\\(irtual\\|o\\(id\\|latile\\)\\)"))	; 12 ()s deep.
	   c++-font-lock-extra-types)
	  "\\|"))
       (c++-type-suffix "\\(<\\(\\sw+\\)[ \t*&]*>\\)?\\(::\\*?\\(\\sw+\\)\\)?")
       (c++-type-spec (concat "\\(\\sw+\\)\\>" c++-type-suffix))
      )
 (setq c++-font-lock-keywords-1
  (append
   ;;
   ;; The list `c-font-lock-keywords-1' less that for function names.
   (cdr c-font-lock-keywords-1)
   (list
    ;;
    ;; Class names etc.
    (list (concat "\\<\\(class\\|public\\|private\\|protected\\)\\>[ \t]*"
		  "\\(" c++-type-spec "\\)?")
	  '(1 font-lock-type-face)
	  '(3 (if (match-beginning 6)
		  font-lock-type-face
		font-lock-function-name-face) nil t)
	  '(5 font-lock-function-name-face nil t)
	  '(7 font-lock-function-name-face nil t))
    ;;
    ;; Fontify function name definitions, possibly incorporating class names.
    (list (concat "^" c++-type-spec "[ \t]*(")
	  '(1 (if (or (match-beginning 2) (match-beginning 4))
		  font-lock-type-face
		font-lock-function-name-face))
	  '(3 font-lock-function-name-face nil t)
	  '(5 font-lock-function-name-face nil t))
    )))

 (setq c++-font-lock-keywords-2
  (append c++-font-lock-keywords-1
   (list
    ;;
    ;; The list `c-font-lock-keywords-2' for C++ plus operator overloading.
    `(eval .
      (cons (concat "\\<\\(" (,@ c++-type-types) "\\)\\>")
	    'font-lock-type-face))
    ;;
    ;; Fontify operator overloading.
    (list (concat "\\<\\(operator\\)\\>[ \t]*\\(" c++-operators "\\)?")
	  '(1 font-lock-keyword-face)
	  '(2 font-lock-builtin-face nil t))
    ;;
    ;; Fontify case/goto keywords and targets, and case default/goto tags.
    '("\\<\\(case\\|goto\\)\\>[ \t]*\\(-?\\sw+\\)?"
      (1 font-lock-keyword-face) (2 font-lock-reference-face nil t))
    '(":" ("^[ \t]*\\(\\sw+\\)[ \t]*:\\($\\|[^:]\\)"
	   (beginning-of-line) (end-of-line)
	   (1 font-lock-reference-face)))
    ;;
    ;; Fontify other builtin keywords.
    (cons (concat "\\<\\(" c++-keywords "\\)\\>") 'font-lock-keyword-face)
    ;;
    ;; Eric Hopper <hopper@omnifarious.mn.org> says `true' and `false' are new.
    '("\\<\\(false\\|true\\)\\>" . font-lock-reference-face)
    )))

 (setq c++-font-lock-keywords-3
  (append c++-font-lock-keywords-2
   ;;
   ;; More complicated regexps for more complete highlighting for types.
   (list
    ;;
    ;; Fontify all storage classes and type specifiers, plus their items.
    `(eval .
      (list (concat "\\<\\(" (,@ c++-type-types) "\\)\\>" (,@ c++-type-suffix)
		    "\\([ \t*&]+" (,@ c++-type-spec) "\\)*")
	    ;; Fontify each declaration item.
	    '(font-lock-match-c++-style-declaration-item-and-skip-to-next
	      ;; Start with point after all type specifiers.
	      (goto-char (or (match-beginning 18) (match-end 1)))
	      ;; Finish with point after first type specifier.
	      (goto-char (match-end 1))
	      ;; Fontify as a variable or function name.
	      (1 (cond ((or (match-beginning 2) (match-beginning 4))
			font-lock-type-face)
		       ((match-beginning 6) font-lock-function-name-face)
		       (t font-lock-variable-name-face)))
	      (3 font-lock-function-name-face nil t)
	      (5 (if (match-beginning 6)
		     font-lock-function-name-face
		   font-lock-variable-name-face) nil t))))
    ;;
    ;; Fontify structures, or typedef names, plus their items.
    '("\\(}\\)[ \t*]*\\sw"
      (font-lock-match-c++-style-declaration-item-and-skip-to-next
       (goto-char (match-end 1)) nil
       (1 (if (match-beginning 6)
	      font-lock-function-name-face
	    font-lock-variable-name-face))))
    ;;
    ;; Fontify anything at beginning of line as a declaration or definition.
    (list (concat "^\\(" c++-type-spec "[ \t*&]*\\)+")
	  '(font-lock-match-c++-style-declaration-item-and-skip-to-next
	    (goto-char (match-beginning 1))
	    (goto-char (match-end 1))
	    (1 (cond ((or (match-beginning 2) (match-beginning 4))
		      font-lock-type-face)
		     ((match-beginning 6) font-lock-function-name-face)
		     (t font-lock-variable-name-face)))
	    (3 font-lock-function-name-face nil t)
	    (5 (if (match-beginning 6)
		   font-lock-function-name-face
		 font-lock-variable-name-face) nil t)))
    )))
 )

(defvar c++-font-lock-keywords c++-font-lock-keywords-1
  "Default expressions to highlight in C++ mode.
See also `c++-font-lock-extra-types'.")

;;; Objective-C.

(defconst objc-font-lock-keywords-1 nil
  "Subdued level highlighting for Objective-C mode.")

(defconst objc-font-lock-keywords-2 nil
  "Medium level highlighting for Objective-C mode.
See also `objc-font-lock-extra-types'.")

(defconst objc-font-lock-keywords-3 nil
  "Gaudy level highlighting for Objective-C mode.
See also `objc-font-lock-extra-types'.")

;; Regexps written with help from Stephen Peters <speters@us.oracle.com> and
;; Jacques Duthen Prestataire <duthen@cegelec-red.fr>.
(let ((objc-keywords
;       (make-regexp
;	'("break" "continue" "do" "else" "for" "if" "return" "switch" "while"
;	  "sizeof" "self" "super"))
       (concat "break\\|continue\\|do\\|else\\|for\\|if\\|return\\|"
	       "s\\(elf\\|izeof\\|uper\\|witch\\)\\|while"))
      (objc-type-types
       `(mapconcat 'identity
	 (cons
;	 '("auto" "extern" "register" "static" "typedef" "struct" "union"
;	   "enum" "signed" "unsigned" "short" "long" "int" "char"
;	   "float" "double" "void" "volatile" "const"
;	   "id" "oneway" "in" "out" "inout" "bycopy" "byref")
	  (,@ (concat "auto\\|by\\(copy\\|ref\\)\\|c\\(har\\|onst\\)\\|"
		      "double\\|e\\(num\\|xtern\\)\\|float\\|"
		      "i\\([dn]\\|n\\(out\\|t\\)\\)\\|long\\|"
		      "o\\(neway\\|ut\\)\\|register\\|s\\(hort\\|igned\\|"
		      "t\\(atic\\|ruct\\)\\)\\|typedef\\|"
		      "un\\(ion\\|signed\\)\\|vo\\(id\\|latile\\)"))
	  objc-font-lock-extra-types)
	 "\\|"))
      )
 (setq objc-font-lock-keywords-1
  (append
   ;;
   ;; The list `c-font-lock-keywords-1' less that for function names.
   (cdr c-font-lock-keywords-1)
   (list
    ;;
    ;; Fontify compiler directives.
    '("@\\(\\sw+\\)\\>"
      (1 font-lock-keyword-face)
      ("\\=[ \t:<(,]*\\(\\sw+\\)" nil nil
       (1 font-lock-function-name-face)))
    ;;
    ;; Fontify method names and arguments.  Oh Lordy!
    ;; First, on the same line as the function declaration.
    '("^[+-][ \t]*\\(PRIVATE\\)?[ \t]*\\((\\([^)\n]+\\))\\)?[ \t]*\\(\\sw+\\)"
      (1 font-lock-type-face nil t)
      (3 font-lock-type-face nil t)
      (4 font-lock-function-name-face)
      ("\\=[ \t]*\\(\\sw+\\)?:[ \t]*\\((\\([^)\n]+\\))\\)?[ \t]*\\(\\sw+\\)"
       nil nil
       (1 font-lock-function-name-face nil t)
       (3 font-lock-type-face nil t)
       (4 font-lock-variable-name-face)))
    ;; Second, on lines following the function declaration.
    '(":" ("^[ \t]*\\(\\sw+\\)?:[ \t]*\\((\\([^)\n]+\\))\\)?[ \t]*\\(\\sw+\\)"
	   (beginning-of-line) (end-of-line)
	   (1 font-lock-function-name-face nil t)
	   (3 font-lock-type-face nil t)
	   (4 font-lock-variable-name-face)))
    )))

 (setq objc-font-lock-keywords-2
  (append objc-font-lock-keywords-1
   (list
    ;;
    ;; Simple regexps for speed.
    ;;
    ;; Fontify all type specifiers.
    `(eval .
      (cons (concat "\\<\\(" (,@ objc-type-types) "\\)\\>")
	    'font-lock-type-face))
    ;;
    ;; Fontify all builtin keywords (except case, default and goto; see below).
    (concat "\\<\\(" objc-keywords "\\)\\>")
    ;;
    ;; Fontify case/goto keywords and targets, and case default/goto tags.
    '("\\<\\(case\\|goto\\)\\>[ \t]*\\(-?\\sw+\\)?"
      (1 font-lock-keyword-face) (2 font-lock-reference-face nil t))
    ;; Fontify tags iff sole statement on line, otherwise we detect selectors.
    '(":" ("^[ \t]*\\(\\sw+\\)[ \t]*:[ \t]*$"
	   (beginning-of-line) (end-of-line)
	   (1 font-lock-reference-face)))
    ;;
    ;; Fontify null object pointers.
    '("\\<\\(Nil\\|nil\\)\\>" 1 font-lock-reference-face)
    )))

 (setq objc-font-lock-keywords-3
  (append objc-font-lock-keywords-2
   ;;
   ;; More complicated regexps for more complete highlighting for types.
   ;; We still have to fontify type specifiers individually, as C is so hairy.
   (list
    ;;
    ;; Fontify all storage classes and type specifiers, plus their items.
    `(eval .
      (list (concat "\\<\\(" (,@ objc-type-types) "\\)\\>"
		    "\\([ \t*&]+\\sw+\\>\\)*")
	    ;; Fontify each declaration item.
	    '(font-lock-match-c-style-declaration-item-and-skip-to-next
	      ;; Start with point after all type specifiers.
	      (goto-char (or (match-beginning 2) (match-end 1)))
	      ;; Finish with point after first type specifier.
	      (goto-char (match-end 1))
	      ;; Fontify as a variable or function name.
	      (1 (if (match-beginning 2)
		     font-lock-function-name-face
		   font-lock-variable-name-face)))))
    ;;
    ;; Fontify structures, or typedef names, plus their items.
    '("\\(}\\)[ \t*]*\\sw"
      (font-lock-match-c-style-declaration-item-and-skip-to-next
       (goto-char (match-end 1)) nil
       (1 (if (match-beginning 2)
	      font-lock-function-name-face
	    font-lock-variable-name-face))))
    ;;
    ;; Fontify anything at beginning of line as a declaration or definition.
    '("^\\(\\sw+\\)\\>\\([ \t*]+\\sw+\\>\\)*"
      (1 font-lock-type-face)
      (font-lock-match-c-style-declaration-item-and-skip-to-next
       (goto-char (or (match-beginning 2) (match-end 1))) nil
       (1 (if (match-beginning 2)
	      font-lock-function-name-face
	    font-lock-variable-name-face))))
    )))
 )

(defvar objc-font-lock-keywords objc-font-lock-keywords-1
  "Default expressions to highlight in Objective-C mode.
See also `objc-font-lock-extra-types'.")

;;; Java.

(defconst java-font-lock-keywords-1 nil
  "Subdued level highlighting for Java mode.")

(defconst java-font-lock-keywords-2 nil
  "Medium level highlighting for Java mode.
See also `java-font-lock-extra-types'.")

(defconst java-font-lock-keywords-3 nil
  "Gaudy level highlighting for Java mode.
See also `java-font-lock-extra-types'.")

;; Regexps written with help from Fred White <fwhite@bbn.com> and
;; Anders Lindgren <andersl@csd.uu.se>.
(let ((java-keywords
       (concat "\\<\\("
;	       (make-regexp
;		'("catch" "do" "else" "super" "this" "finally" "for" "if"
;;		  ;; Anders Lindgren <andersl@csd.uu.se> says these have gone.
;;		  "cast" "byvalue" "future" "generic" "operator" "var"
;;		  "inner" "outer" "rest"
;		  "interface" "return" "switch" "throw" "try" "while")
	       "catch\\|do\\|else\\|f\\(inally\\|or\\)\\|"
	       "i\\(f\\|nterface\\)\\|return\\|s\\(uper\\|witch\\)\\|"
	       "t\\(h\\(is\\|row\\)\\|ry\\)\\|while"
	       "\\)\\>"))
      ;;
      ;; These are immediately followed by an object name.
      (java-minor-types
       (mapconcat 'identity
	'("boolean" "char" "byte" "short" "int" "long" "float" "double" "void")
	"\\|"))
      ;;
      ;; These are eventually followed by an object name.
      (java-major-types
;       (make-regexp
;	'("abstract" "const" "final" "synchronized" "transient" "static"
;;	  ;; Anders Lindgren <andersl@csd.uu.se> says this has gone.
;;	  "threadsafe"
;	  "volatile" "public" "private" "protected" "native")
       (concat "abstract\\|const\\|final\\|native\\|"
	       "p\\(r\\(ivate\\|otected\\)\\|ublic\\)\\|"
	       "s\\(tatic\\|ynchronized\\)\\|transient\\|volatile"))
      ;;
      ;; Random types immediately followed by an object name.
      (java-other-types
       '(mapconcat 'identity (cons "\\sw+\\.\\sw+" java-font-lock-extra-types)
		   "\\|"))
      )
 (setq java-font-lock-keywords-1
  (list
   ;;
   ;; Fontify class names.
   '("\\<\\(class\\)\\>[ \t]*\\(\\sw+\\)?"
     (1 font-lock-type-face) (2 font-lock-function-name-face nil t))
   ;;
   ;; Fontify package names in import directives.
   '("\\<\\(import\\|package\\)\\>[ \t]*\\(\\sw+\\)?"
     (1 font-lock-keyword-face) (2 font-lock-reference-face nil t))
   ))

 (setq java-font-lock-keywords-2
  (append java-font-lock-keywords-1
   (list
    ;;
    ;; Fontify all builtin type specifiers.
    (cons (concat "\\<\\(" java-minor-types "\\|" java-major-types "\\)\\>")
	  'font-lock-type-face)
    ;;
    ;; Fontify all builtin keywords (except below).
    (concat "\\<\\(" java-keywords "\\)\\>")
    ;;
    ;; Fontify keywords and targets, and case default/goto tags.
    (list "\\<\\(break\\|case\\|continue\\|goto\\)\\>[ \t]*\\(-?\\sw+\\)?"
	  '(1 font-lock-keyword-face) '(2 font-lock-reference-face nil t))
    '(":" ("^[ \t]*\\(\\sw+\\)[ \t]*:"
	   (beginning-of-line) (end-of-line)
	   (1 font-lock-reference-face)))
    ;;
    ;; Fontify keywords and types; the first can be followed by a type list.
    (list (concat "\\<\\("
		  "implements\\|throws\\|"
		  "\\(extends\\|instanceof\\|new\\)"
		  "\\)\\>[ \t]*\\(\\sw+\\)?")
	  '(1 font-lock-keyword-face) '(3 font-lock-type-face nil t)
	  '("\\=[ \t]*,[ \t]*\\(\\sw+\\)"
	    (if (match-beginning 2) (goto-char (match-end 2))) nil
	    (1 font-lock-type-face)))
    ;;
    ;; Fontify all constants.
    '("\\<\\(false\\|null\\|true\\)\\>" . font-lock-reference-face)
    ;;
    ;; Javadoc tags within comments.
    '("@\\(author\\|exception\\|return\\|see\\|version\\)\\>"
      (1 font-lock-reference-face prepend))
    '("@\\(param\\)\\>[ \t]*\\(\\sw+\\)?"
      (1 font-lock-reference-face prepend)
      (2 font-lock-variable-name-face prepend t))
    )))

 (setq java-font-lock-keywords-3
  (append java-font-lock-keywords-2
   ;;
   ;; More complicated regexps for more complete highlighting for types.
   ;; We still have to fontify type specifiers individually, as Java is hairy.
   (list
    ;;
    ;; Fontify random types in casts.
    `(eval .
      (list (concat "(\\(" (,@ java-other-types) "\\))"
		    "[ \t]*\\(\\sw\\|[\"\(]\\)")
	    ;; Fontify the type name.
	    '(1 font-lock-type-face)))
    ;;
    ;; Fontify random types immediately followed by an item or items.
    `(eval .
      (list (concat "\\<\\(" (,@ java-other-types) "\\)\\>"
		    "\\([ \t]*\\[[ \t]*\\]\\)*"
		    "[ \t]*\\sw")
	    ;; Fontify the type name.
	    '(1 font-lock-type-face)))
    `(eval .
      (list (concat "\\<\\(" (,@ java-other-types) "\\)\\>"
		    "\\([ \t]*\\[[ \t]*\\]\\)*"
		    "\\([ \t]*\\sw\\)")
	    ;; Fontify each declaration item.
	    '(font-lock-match-c-style-declaration-item-and-skip-to-next
	      ;; Start and finish with point after the type specifier.
	      (goto-char (match-beginning 3)) (goto-char (match-beginning 3))
	      ;; Fontify as a variable or function name.
	      (1 (if (match-beginning 2)
		     font-lock-function-name-face
		   font-lock-variable-name-face)))))
    ;;
    ;; Fontify those that are immediately followed by an item or items.
    (list (concat "\\<\\(" java-minor-types "\\)\\>"
		  "\\([ \t]*\\[[ \t]*\\]\\)*")
	  ;; Fontify each declaration item.
	  '(font-lock-match-c-style-declaration-item-and-skip-to-next
	    ;; Start and finish with point after the type specifier.
	    nil (goto-char (match-end 0))
	    ;; Fontify as a variable or function name.
	    (1 (if (match-beginning 2)
		   font-lock-function-name-face
		 font-lock-variable-name-face))))
    ;;
    ;; Fontify those that are eventually followed by an item or items.
    (list (concat "\\<\\(" java-major-types "\\)\\>"
		  "\\([ \t]+\\sw+\\>"
		  "\\([ \t]*\\[[ \t]*\\]\\)*"
		  "\\)*")
	  ;; Fontify each declaration item.
	  '(font-lock-match-c-style-declaration-item-and-skip-to-next
	    ;; Start with point after all type specifiers.
	    (goto-char (or (match-beginning 2) (match-end 1)))
	    ;; Finish with point after first type specifier.
	    (goto-char (match-end 1))
	    ;; Fontify as a variable or function name.
	    (1 (if (match-beginning 2)
		   font-lock-function-name-face
		 font-lock-variable-name-face))))
    )))
 )

(defvar java-font-lock-keywords java-font-lock-keywords-1
  "Default expressions to highlight in Java mode.
See also `java-font-lock-extra-types'.")

;; Install ourselves:

(unless (assq 'font-lock-mode minor-mode-alist)
  (push '(font-lock-mode " Font") minor-mode-alist))

;; Provide ourselves:

(provide 'font-lock)

;;; font-lock.el ends here
