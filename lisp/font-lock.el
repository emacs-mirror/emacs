;;; font-lock.el --- Electric font lock mode

;; Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
;;   2000, 2001, 2002, 2003, 2004 2005 Free Software Foundation, Inc.

;; Author: jwz, then rms, then sm
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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

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
;; regexp (i.e., keywords and everything else) passes.  There are actually
;; three passes; (a) the syntactic keyword pass, (b) the syntactic pass and (c)
;; the keyword pass.  Confused?
;;
;; The syntactic keyword pass places `syntax-table' text properties in the
;; buffer according to the variable `font-lock-syntactic-keywords'.  It is
;; necessary because Emacs' syntax table is not powerful enough to describe all
;; the different syntactic constructs required by the sort of people who decide
;; that a single quote can be syntactic or not depending on the time of day.
;; (What sort of person could decide to overload the meaning of a quote?)
;; Obviously the syntactic keyword pass must occur before the syntactic pass.
;;
;; The syntactic pass places `face' text properties in the buffer according to
;; syntactic context, i.e., according to the buffer's syntax table and buffer
;; text's `syntax-table' text properties.  It involves using a syntax parsing
;; function to determine the context of different parts of a region of text.  A
;; syntax parsing function is necessary because generally strings and/or
;; comments can span lines, and so the context of a given region is not
;; necessarily apparent from the content of that region.  Because the keyword
;; pass only works within a given region, it is not generally appropriate for
;; syntactic fontification.  This is the first fontification pass that makes
;; changes visible to the user; it fontifies strings and comments.
;;
;; The keyword pass places `face' text properties in the buffer according to
;; the variable `font-lock-keywords'.  It involves searching for given regexps
;; (or calling given search functions) within the given region.  This is the
;; second fontification pass that makes changes visible to the user; it
;; fontifies language reserved words, etc.
;;
;; Oh, and the answer is, "Yes, obviously just about everything should be done
;; in a single syntactic pass, but the only syntactic parser available
;; understands only strings and comments."  Perhaps one day someone will write
;; some syntactic parsers for common languages and a son-of-font-lock.el could
;; use them rather then relying so heavily on the keyword (regexp) pass.

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
;; Efficient regexps for use as MATCHERs for `font-lock-keywords' and
;; `font-lock-syntactic-keywords' can be generated via the function
;; `regexp-opt'.

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
;;   (lambda ()
;;     (make-local-variable 'font-lock-defaults)
;;     (setq font-lock-defaults '(foo-font-lock-keywords t))))

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

(require 'syntax)

;; Define core `font-lock' group.
(defgroup font-lock '((jit-lock custom-group))
  "Font Lock mode text highlighting package."
  :link '(custom-manual "(emacs)Font Lock")
  :link '(custom-manual "(elisp)Font Lock Mode")
  :group 'faces)

(defgroup font-lock-highlighting-faces nil
  "Faces for highlighting text."
  :prefix "font-lock-"
  :group 'font-lock)

(defgroup font-lock-extra-types nil
  "Extra mode-specific type names for highlighting declarations."
  :group 'font-lock)

;; User variables.

(defcustom font-lock-maximum-size 256000
  "*Maximum size of a buffer for buffer fontification.
Only buffers less than this can be fontified when Font Lock mode is turned on.
If nil, means size is irrelevant.
If a list, each element should be a cons pair of the form (MAJOR-MODE . SIZE),
where MAJOR-MODE is a symbol or t (meaning the default).  For example:
 ((c-mode . 256000) (c++-mode . 256000) (rmail-mode . 1048576))
means that the maximum size is 250K for buffers in C or C++ modes, one megabyte
for buffers in Rmail mode, and size is irrelevant otherwise."
  :type '(choice (const :tag "none" nil)
		 (integer :tag "size")
		 (repeat :menu-tag "mode specific" :tag "mode specific"
			 :value ((t . nil))
			 (cons :tag "Instance"
			       (radio :tag "Mode"
				      (const :tag "all" t)
				      (symbol :tag "name"))
			       (radio :tag "Size"
				      (const :tag "none" nil)
				      (integer :tag "size")))))
  :group 'font-lock)

(defcustom font-lock-maximum-decoration t
  "*Maximum decoration level for fontification.
If nil, use the default decoration (typically the minimum available).
If t, use the maximum decoration available.
If a number, use that level of decoration (or if not available the maximum).
If a list, each element should be a cons pair of the form (MAJOR-MODE . LEVEL),
where MAJOR-MODE is a symbol or t (meaning the default).  For example:
 ((c-mode . t) (c++-mode . 2) (t . 1))
means use the maximum decoration available for buffers in C mode, level 2
decoration for buffers in C++ mode, and level 1 decoration otherwise."
  :type '(choice (const :tag "default" nil)
		 (const :tag "maximum" t)
		 (integer :tag "level" 1)
		 (repeat :menu-tag "mode specific" :tag "mode specific"
			 :value ((t . t))
			 (cons :tag "Instance"
			       (radio :tag "Mode"
				      (const :tag "all" t)
				      (symbol :tag "name"))
			       (radio :tag "Decoration"
				      (const :tag "default" nil)
				      (const :tag "maximum" t)
				      (integer :tag "level" 1)))))
  :group 'font-lock)

(defcustom font-lock-verbose 0
  "*If non-nil, means show status messages for buffer fontification.
If a number, only buffers greater than this size have fontification messages."
  :type '(choice (const :tag "never" nil)
		 (other :tag "always" t)
		 (integer :tag "size"))
  :group 'font-lock)

(defcustom font-lock-lines-before 0
  "*Number of lines before the changed text to include in refontification."
  :type 'integer
  :group 'font-lock
  :version "22.1")


;; Originally these variable values were face names such as `bold' etc.
;; Now we create our own faces, but we keep these variables for compatibility
;; and they give users another mechanism for changing face appearance.
;; We now allow a FACENAME in `font-lock-keywords' to be any expression that
;; returns a face.  So the easiest thing is to continue using these variables,
;; rather than sometimes evaling FACENAME and sometimes not.  sm.
(defvar font-lock-comment-face		'font-lock-comment-face
  "Face name to use for comments.")

(defvar font-lock-comment-delimiter-face 'font-lock-comment-delimiter-face
  "Face name to use for comment delimiters.")

(defvar font-lock-string-face		'font-lock-string-face
  "Face name to use for strings.")

(defvar font-lock-doc-face		'font-lock-doc-face
  "Face name to use for documentation.")

(defvar font-lock-keyword-face		'font-lock-keyword-face
  "Face name to use for keywords.")

(defvar font-lock-builtin-face		'font-lock-builtin-face
  "Face name to use for builtins.")

(defvar font-lock-function-name-face	'font-lock-function-name-face
  "Face name to use for function names.")

(defvar font-lock-variable-name-face	'font-lock-variable-name-face
  "Face name to use for variable names.")

(defvar font-lock-type-face		'font-lock-type-face
  "Face name to use for type and class names.")

(defvar font-lock-constant-face		'font-lock-constant-face
  "Face name to use for constant and label names.")

(defvar font-lock-warning-face		'font-lock-warning-face
  "Face name to use for things that should stand out.")

(defvar font-lock-negation-char-face	'font-lock-negation-char-face
  "Face name to use for easy to overlook negation.
This can be an \"!\" or the \"n\" in \"ifndef\".")

(defvar font-lock-preprocessor-face	'font-lock-preprocessor-face
  "Face name to use for preprocessor directives.")

(defvar font-lock-reference-face	'font-lock-constant-face)
(make-obsolete-variable 'font-lock-reference-face 'font-lock-constant-face)

;; Fontification variables:

(defvar font-lock-keywords nil
  "A list of the keywords to highlight.
There are two kinds of values: user-level, and compiled.

A user-level keywords list is what a major mode or the user would
set up.  Normally the list would come from `font-lock-defaults'.
through selection of a fontification level and evaluation of any
contained expressions.  You can also alter it by calling
`font-lock-add-keywords' or `font-lock-remove-keywords' with MODE = nil.

Each element in a user-level keywords list should have one of these forms:

 MATCHER
 (MATCHER . MATCH)
 (MATCHER . FACENAME)
 (MATCHER . HIGHLIGHT)
 (MATCHER HIGHLIGHT ...)
 (eval . FORM)

where MATCHER can be either the regexp to search for, or the function name to
call to make the search (called with one argument, the limit of the search;
it should return non-nil, move point, and set `match-data' appropriately iff
it succeeds; like `re-search-forward' would).
MATCHER regexps can be generated via the function `regexp-opt'.

FORM is an expression, whose value should be a keyword element, evaluated when
the keyword is (first) used in a buffer.  This feature can be used to provide a
keyword that can only be generated when Font Lock mode is actually turned on.

HIGHLIGHT should be either MATCH-HIGHLIGHT or MATCH-ANCHORED.

For highlighting single items, for example each instance of the word \"foo\",
typically only MATCH-HIGHLIGHT is required.
However, if an item or (typically) items are to be highlighted following the
instance of another item (the anchor), for example each instance of the
word \"bar\" following the word \"anchor\" then MATCH-ANCHORED may be required.

MATCH-HIGHLIGHT should be of the form:

 (MATCH FACENAME [OVERRIDE [LAXMATCH]])

MATCH is the subexpression of MATCHER to be highlighted.  FACENAME is an
expression whose value is the face name to use.  Face default attributes
can be modified via \\[customize].  Instead of a face, FACENAME can
evaluate to a property list of the form (face FACE PROP1 VAL1 PROP2 VAL2 ...)
in which case all the listed text-properties will be set rather than
just FACE.  In such a case, you will most likely want to put those
properties in `font-lock-extra-managed-props' or to override
`font-lock-unfontify-region-function'.

OVERRIDE and LAXMATCH are flags.  If OVERRIDE is t, existing fontification can
be overwritten.  If `keep', only parts not already fontified are highlighted.
If `prepend' or `append', existing fontification is merged with the new, in
which the new or existing fontification, respectively, takes precedence.
If LAXMATCH is non-nil, no error is signaled if there is no MATCH in MATCHER.

For example, an element of the form highlights (if not already highlighted):

 \"\\\\\\=<foo\\\\\\=>\"		discrete occurrences of \"foo\" in the value of the
			variable `font-lock-keyword-face'.
 (\"fu\\\\(bar\\\\)\" . 1)	substring \"bar\" within all occurrences of \"fubar\" in
			the value of `font-lock-keyword-face'.
 (\"fubar\" . fubar-face)	Occurrences of \"fubar\" in the value of `fubar-face'.
 (\"foo\\\\|bar\" 0 foo-bar-face t)
			occurrences of either \"foo\" or \"bar\" in the value
			of `foo-bar-face', even if already highlighted.
 (fubar-match 1 fubar-face)
			the first subexpression within all occurrences of
			whatever the function `fubar-match' finds and matches
			in the value of `fubar-face'.

MATCH-ANCHORED should be of the form:

 (MATCHER PRE-MATCH-FORM POST-MATCH-FORM MATCH-HIGHLIGHT ...)

where MATCHER is a regexp to search for or the function name to call to make
the search, as for MATCH-HIGHLIGHT above, but with one exception; see below.
PRE-MATCH-FORM and POST-MATCH-FORM are evaluated before the first, and after
the last, instance MATCH-ANCHORED's MATCHER is used.  Therefore they can be
used to initialize before, and cleanup after, MATCHER is used.  Typically,
PRE-MATCH-FORM is used to move to some position relative to the original
MATCHER, before starting with MATCH-ANCHORED's MATCHER.  POST-MATCH-FORM might
be used to move back, before resuming with MATCH-ANCHORED's parent's MATCHER.

For example, an element of the form highlights (if not already highlighted):

 (\"\\\\\\=<anchor\\\\\\=>\" (0 anchor-face) (\"\\\\\\=<item\\\\\\=>\" nil nil (0 item-face)))

 discrete occurrences of \"anchor\" in the value of `anchor-face', and subsequent
 discrete occurrences of \"item\" (on the same line) in the value of `item-face'.
 (Here PRE-MATCH-FORM and POST-MATCH-FORM are nil.  Therefore \"item\" is
 initially searched for starting from the end of the match of \"anchor\", and
 searching for subsequent instances of \"anchor\" resumes from where searching
 for \"item\" concluded.)

The above-mentioned exception is as follows.  The limit of the MATCHER search
defaults to the end of the line after PRE-MATCH-FORM is evaluated.
However, if PRE-MATCH-FORM returns a position greater than the position after
PRE-MATCH-FORM is evaluated, that position is used as the limit of the search.
It is generally a bad idea to return a position greater than the end of the
line, i.e., cause the MATCHER search to span lines.

These regular expressions can match text which spans lines, although
it is better to avoid it if possible since updating them while editing
text is slower, and it is not guaranteed to be always correct when using
support modes like jit-lock or lazy-lock.

This variable is set by major modes via the variable `font-lock-defaults'.
Be careful when composing regexps for this list; a poorly written pattern can
dramatically slow things down!

A compiled keywords list starts with t.  It is produced internal
by `font-lock-compile-keywords' from a user-level keywords list.
Its second element is the user-level keywords list that was
compiled.  The remaining elements have the same form as
user-level keywords, but normally their values have been
optimized.")

(defvar font-lock-keywords-alist nil
  "Alist of additional `font-lock-keywords' elements for major modes.

Each element has the form (MODE KEYWORDS . APPEND).
`font-lock-set-defaults' adds the elements in the list KEYWORDS to
`font-lock-keywords' when Font Lock is turned on in major mode MODE.

If APPEND is nil, KEYWORDS are added at the beginning of
`font-lock-keywords'.  If it is `set', they are used to replace the
value of `font-lock-keywords'.  If APPEND is any other non-nil value,
they are added at the end.

This is normally set via `font-lock-add-keywords' and
`font-lock-remove-keywords'.")

(defvar font-lock-removed-keywords-alist nil
  "Alist of `font-lock-keywords' elements to be removed for major modes.

Each element has the form (MODE . KEYWORDS).  `font-lock-set-defaults'
removes the elements in the list KEYWORDS from `font-lock-keywords'
when Font Lock is turned on in major mode MODE.

This is normally set via `font-lock-add-keywords' and
`font-lock-remove-keywords'.")

(defvar font-lock-keywords-only nil
  "*Non-nil means Font Lock should not fontify comments or strings.
This is normally set via `font-lock-defaults'.")

(defvar font-lock-keywords-case-fold-search nil
  "*Non-nil means the patterns in `font-lock-keywords' are case-insensitive.
This is normally set via `font-lock-defaults'.")
(make-variable-buffer-local 'font-lock-keywords-case-fold-search)

(defvar font-lock-syntactically-fontified 0
  "Point up to which `font-lock-syntactic-keywords' has been applied.
If nil, this is ignored, in which case the syntactic fontification may
sometimes be slightly incorrect.")
(make-variable-buffer-local 'font-lock-syntactically-fontified)

(defvar font-lock-syntactic-face-function
  (lambda (state)
    (if (nth 3 state) font-lock-string-face font-lock-comment-face))
  "Function to determine which face to use when fontifying syntactically.
The function is called with a single parameter (the state as returned by
`parse-partial-sexp' at the beginning of the region to highlight) and
should return a face.  This is normally set via `font-lock-defaults'.")

(defvar font-lock-syntactic-keywords nil
  "A list of the syntactic keywords to highlight.
Can be the list or the name of a function or variable whose value is the list.
See `font-lock-keywords' for a description of the form of this list;
the differences are listed below.  MATCH-HIGHLIGHT should be of the form:

 (MATCH SYNTAX OVERRIDE LAXMATCH)

where SYNTAX can be a string (as taken by `modify-syntax-entry'), a syntax
table, a cons cell (as returned by `string-to-syntax') or an expression whose
value is such a form.  OVERRIDE cannot be `prepend' or `append'.

For example, an element of the form highlights syntactically:

 (\"\\\\$\\\\(#\\\\)\" 1 \".\")

 a hash character when following a dollar character, with a SYNTAX of
 \".\" (meaning punctuation syntax).  Assuming that the buffer syntax table does
 specify hash characters to have comment start syntax, the element will only
 highlight hash characters that do not follow dollar characters as comments
 syntactically.

 (\"\\\\('\\\\).\\\\('\\\\)\"
  (1 \"\\\"\")
  (2 \"\\\"\"))

 both single quotes which surround a single character, with a SYNTAX of
 \"\\\"\" (meaning string quote syntax).  Assuming that the buffer syntax table
 does not specify single quotes to have quote syntax, the element will only
 highlight single quotes of the form 'c' as strings syntactically.
 Other forms, such as foo'bar or 'fubar', will not be highlighted as strings.

This is normally set via `font-lock-defaults'.")

(defvar font-lock-syntax-table nil
  "Non-nil means use this syntax table for fontifying.
If this is nil, the major mode's syntax table is used.
This is normally set via `font-lock-defaults'.")

(defvar font-lock-beginning-of-syntax-function nil
  "*Non-nil means use this function to move back outside all constructs.
When called with no args it should move point backward to a place which
is not in a string or comment and not within any bracket-pairs (or else,
a place such that any bracket-pairs outside it can be ignored for Emacs
syntax analysis and fontification).

If this is nil, Font Lock uses `syntax-begin-function' to move back
outside of any comment, string, or sexp.  This variable is semi-obsolete;
we recommend setting `syntax-begin-function' instead.

This is normally set via `font-lock-defaults'.")

(defvar font-lock-mark-block-function nil
  "*Non-nil means use this function to mark a block of text.
When called with no args it should leave point at the beginning of any
enclosing textual block and mark at the end.
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
third arg VERBOSE.  If VERBOSE is non-nil, the function should print status
messages.  This is normally set via `font-lock-defaults'.")

(defvar font-lock-unfontify-region-function 'font-lock-default-unfontify-region
  "Function to use for unfontifying a region.
It should take two args, the beginning and end of the region.
This is normally set via `font-lock-defaults'.")

(defvar font-lock-inhibit-thing-lock nil
  "List of Font Lock mode related modes that should not be turned on.
Currently, valid mode names are `fast-lock-mode', `jit-lock-mode' and
`lazy-lock-mode'.  This is normally set via `font-lock-defaults'.")

(defvar font-lock-multiline nil
  "Whether font-lock should cater to multiline keywords.
If nil, don't try to handle multiline patterns.
If t, always handle multiline patterns.
If `undecided', don't try to handle multiline patterns until you see one.
Major/minor modes can set this variable if they know which option applies.")

(defvar font-lock-fontified nil)	; Whether we have fontified the buffer.

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
    (declare (indent 1) (debug let))
    (let ((modified (make-symbol "modified")))
      `(let* ,(append varlist
		      `((,modified (buffer-modified-p))
			(buffer-undo-list t)
			(inhibit-read-only t)
			(inhibit-point-motion-hooks t)
			(inhibit-modification-hooks t)
			deactivate-mark
			buffer-file-name
			buffer-file-truename))
	 (progn
	   ,@body)
	 (unless ,modified
	   (restore-buffer-modified-p nil)))))
  ;;
  ;; Shut up the byte compiler.
  (defvar font-lock-face-attributes))	; Obsolete but respected if set.

;;;###autoload
(defun font-lock-mode-internal (arg)
  ;; Turn on Font Lock mode.
  (when arg
    (add-hook 'after-change-functions 'font-lock-after-change-function t t)
    (font-lock-set-defaults)
    (font-lock-turn-on-thing-lock)
    ;; Fontify the buffer if we have to.
    (let ((max-size (font-lock-value-in-major-mode font-lock-maximum-size)))
      (cond (font-lock-fontified
	     nil)
	    ((or (null max-size) (> max-size (buffer-size)))
	     (font-lock-fontify-buffer))
	    (font-lock-verbose
	     (message "Fontifying %s...buffer size greater than font-lock-maximum-size"
		      (buffer-name))))))
  ;; Turn off Font Lock mode.
  (unless font-lock-mode
    (remove-hook 'after-change-functions 'font-lock-after-change-function t)
    (font-lock-unfontify-buffer)
    (font-lock-turn-off-thing-lock)))

;;;###autoload
(defun font-lock-add-keywords (mode keywords &optional append)
  "Add highlighting KEYWORDS for MODE.

MODE should be a symbol, the major mode command name, such as `c-mode'
or nil.  If nil, highlighting keywords are added for the current buffer.
KEYWORDS should be a list; see the variable `font-lock-keywords'.
By default they are added at the beginning of the current highlighting list.
If optional argument APPEND is `set', they are used to replace the current
highlighting list.  If APPEND is any other non-nil value, they are added at the
end of the current highlighting list.

For example:

 (font-lock-add-keywords 'c-mode
  '((\"\\\\\\=<\\\\(FIXME\\\\):\" 1 font-lock-warning-face prepend)
    (\"\\\\\\=<\\\\(and\\\\|or\\\\|not\\\\)\\\\\\=>\" . font-lock-keyword-face)))

adds two fontification patterns for C mode, to fontify `FIXME:' words, even in
comments, and to fontify `and', `or' and `not' words as keywords.

The above procedure will only add the keywords for C mode, not
for modes derived from C mode.  To add them for derived modes too,
pass nil for MODE and add the call to c-mode-hook.

For example:

 (add-hook 'c-mode-hook
  (lambda ()
   (font-lock-add-keywords nil
    '((\"\\\\\\=<\\\\(FIXME\\\\):\" 1 font-lock-warning-face prepend)
      (\"\\\\\\=<\\\\(and\\\\|or\\\\|not\\\\)\\\\\\=>\" .
       font-lock-keyword-face)))))

The above procedure may fail to add keywords to derived modes if
some involved major mode does not follow the standard conventions.
File a bug report if this happens, so the major mode can be corrected.

Note that some modes have specialized support for additional patterns, e.g.,
see the variables `c-font-lock-extra-types', `c++-font-lock-extra-types',
`objc-font-lock-extra-types' and `java-font-lock-extra-types'."
  (cond (mode
	 ;; If MODE is non-nil, add the KEYWORDS and APPEND spec to
	 ;; `font-lock-keywords-alist' so `font-lock-set-defaults' uses them.
	 (let ((spec (cons keywords append)) cell)
	   (if (setq cell (assq mode font-lock-keywords-alist))
	       (if (eq append 'set)
		   (setcdr cell (list spec))
		 (setcdr cell (append (cdr cell) (list spec))))
	     (push (list mode spec) font-lock-keywords-alist)))
	 ;; Make sure that `font-lock-removed-keywords-alist' does not
	 ;; contain the new keywords.
	 (font-lock-update-removed-keyword-alist mode keywords append))
	(t
	 ;; Otherwise set or add the keywords now.
	 ;; This is a no-op if it has been done already in this buffer
	 ;; for the correct major mode.
	 (font-lock-set-defaults)
	 (let ((was-compiled (eq (car font-lock-keywords) t)))
	   ;; Bring back the user-level (uncompiled) keywords.
	   (if was-compiled
	       (setq font-lock-keywords (cadr font-lock-keywords)))
	   ;; Now modify or replace them.
	   (if (eq append 'set)
	       (setq font-lock-keywords keywords)
	     (font-lock-remove-keywords nil keywords) ;to avoid duplicates
	     (let ((old (if (eq (car-safe font-lock-keywords) t)
			    (cdr font-lock-keywords)
			  font-lock-keywords)))
	       (setq font-lock-keywords (if append
					    (append old keywords)
					  (append keywords old)))))
	   ;; If the keywords were compiled before, compile them again.
	   (if was-compiled
	       (set (make-local-variable 'font-lock-keywords)
		    (font-lock-compile-keywords font-lock-keywords t)))))))

(defun font-lock-update-removed-keyword-alist (mode keywords append)
  "Update `font-lock-removed-keywords-alist' when adding new KEYWORDS to MODE."
  ;; When font-lock is enabled first all keywords in the list
  ;; `font-lock-keywords-alist' are added, then all keywords in the
  ;; list `font-lock-removed-keywords-alist' are removed.  If a
  ;; keyword was once added, removed, and then added again it must be
  ;; removed from the removed-keywords list.  Otherwise the second add
  ;; will not take effect.
  (let ((cell (assq mode font-lock-removed-keywords-alist)))
    (if cell
	(if (eq append 'set)
	    ;; A new set of keywords is defined.  Forget all about
	    ;; our old keywords that should be removed.
	    (setq font-lock-removed-keywords-alist
		  (delq cell font-lock-removed-keywords-alist))
	  ;; Delete all previously removed keywords.
	  (dolist (kword keywords)
	    (setcdr cell (delete kword (cdr cell))))
	  ;; Delete the mode cell if empty.
	  (if (null (cdr cell))
	      (setq font-lock-removed-keywords-alist
		    (delq cell font-lock-removed-keywords-alist)))))))

;; Written by Anders Lindgren <andersl@andersl.com>.
;;
;; Case study:
;; (I)  The keywords are removed from a major mode.
;;      In this case the keyword could be local (i.e. added earlier by
;;      `font-lock-add-keywords'), global, or both.
;;
;;      (a) In the local case we remove the keywords from the variable
;;          `font-lock-keywords-alist'.
;;
;;      (b) The actual global keywords are not known at this time.
;;          All keywords are added to `font-lock-removed-keywords-alist',
;;          when font-lock is enabled those keywords are removed.
;;
;;      Note that added keywords are taken out of the list of removed
;;      keywords.  This ensure correct operation when the same keyword
;;      is added and removed several times.
;;
;; (II) The keywords are removed from the current buffer.
;;;###autoload
(defun font-lock-remove-keywords (mode keywords)
  "Remove highlighting KEYWORDS for MODE.

MODE should be a symbol, the major mode command name, such as `c-mode'
or nil.  If nil, highlighting keywords are removed for the current buffer.

To make the removal apply to modes derived from MODE as well,
pass nil for MODE and add the call to MODE-hook.  This may fail
for some derived modes if some involved major mode does not
follow the standard conventions.  File a bug report if this
happens, so the major mode can be corrected."
  (cond (mode
	 ;; Remove one keyword at the time.
	 (dolist (keyword keywords)
	   (let ((top-cell (assq mode font-lock-keywords-alist)))
	     ;; If MODE is non-nil, remove the KEYWORD from
	     ;; `font-lock-keywords-alist'.
	     (when top-cell
	       (dolist (keyword-list-append-pair (cdr top-cell))
		 ;; `keywords-list-append-pair' is a cons with a list of
		 ;; keywords in the car top-cell and the original append
		 ;; argument in the cdr top-cell.
		 (setcar keyword-list-append-pair
			 (delete keyword (car keyword-list-append-pair))))
	       ;; Remove keyword list/append pair when the keyword list
	       ;; is empty and append doesn't specify `set'.  (If it
	       ;; should be deleted then previously deleted keywords
	       ;; would appear again.)
	       (let ((cell top-cell))
		 (while (cdr cell)
		   (if (and (null (car (car (cdr cell))))
			    (not (eq (cdr (car (cdr cell))) 'set)))
		       (setcdr cell (cdr (cdr cell)))
		     (setq cell (cdr cell)))))
	       ;; Final cleanup, remove major mode cell if last keyword
	       ;; was deleted.
	       (if (null (cdr top-cell))
		   (setq font-lock-keywords-alist
			 (delq top-cell font-lock-keywords-alist))))
	     ;; Remember the keyword in case it is not local.
	     (let ((cell (assq mode font-lock-removed-keywords-alist)))
	       (if cell
		   (unless (member keyword (cdr cell))
		     (nconc cell (list keyword)))
		 (push (cons mode (list keyword))
		       font-lock-removed-keywords-alist))))))
	(t
	 ;; Otherwise remove it immediately.
	 (font-lock-set-defaults)
	 (let ((was-compiled (eq (car font-lock-keywords) t)))
	   ;; Bring back the user-level (uncompiled) keywords.
	   (if was-compiled
	       (setq font-lock-keywords (cadr font-lock-keywords)))

	   ;; Edit them.
	   (setq font-lock-keywords (copy-sequence font-lock-keywords))
	   (dolist (keyword keywords)
	     (setq font-lock-keywords
		   (delete keyword font-lock-keywords)))

	   ;; If the keywords were compiled before, compile them again.
	   (if was-compiled
	       (set (make-local-variable 'font-lock-keywords)
		    (font-lock-compile-keywords font-lock-keywords t)))))))

;;; Font Lock Support mode.

;; This is the code used to interface font-lock.el with any of its add-on
;; packages, and provide the user interface.  Packages that have their own
;; local buffer fontification functions (see below) may have to call
;; `font-lock-after-fontify-buffer' and/or `font-lock-after-unfontify-buffer'
;; themselves.

(defcustom font-lock-support-mode 'jit-lock-mode
  "*Support mode for Font Lock mode.
Support modes speed up Font Lock mode by being choosy about when fontification
occurs.  Known support modes are Fast Lock mode (symbol `fast-lock-mode'),
Lazy Lock mode (symbol `lazy-lock-mode'), and Just-in-time Lock mode (symbol
`jit-lock-mode'.  See those modes for more info.
If nil, means support for Font Lock mode is never performed.
If a symbol, use that support mode.
If a list, each element should be of the form (MAJOR-MODE . SUPPORT-MODE),
where MAJOR-MODE is a symbol or t (meaning the default).  For example:
 ((c-mode . fast-lock-mode) (c++-mode . fast-lock-mode) (t . lazy-lock-mode))
means that Fast Lock mode is used to support Font Lock mode for buffers in C or
C++ modes, and Lazy Lock mode is used to support Font Lock mode otherwise.

The value of this variable is used when Font Lock mode is turned on."
  :type '(choice (const :tag "none" nil)
		 (const :tag "fast lock" fast-lock-mode)
		 (const :tag "lazy lock" lazy-lock-mode)
		 (const :tag "jit lock" jit-lock-mode)
		 (repeat :menu-tag "mode specific" :tag "mode specific"
			 :value ((t . jit-lock-mode))
			 (cons :tag "Instance"
			       (radio :tag "Mode"
				      (const :tag "all" t)
				      (symbol :tag "name"))
			       (radio :tag "Support"
				      (const :tag "none" nil)
				      (const :tag "fast lock" fast-lock-mode)
				      (const :tag "lazy lock" lazy-lock-mode)
				      (const :tag "JIT lock" jit-lock-mode)))
			 ))
  :version "21.1"
  :group 'font-lock)

(defvar fast-lock-mode)
(defvar lazy-lock-mode)
(defvar jit-lock-mode)

(defun font-lock-turn-on-thing-lock ()
  (let ((thing-mode (font-lock-value-in-major-mode font-lock-support-mode)))
    (cond ((eq thing-mode 'fast-lock-mode)
	   (fast-lock-mode t))
	  ((eq thing-mode 'lazy-lock-mode)
	   (lazy-lock-mode t))
	  ((eq thing-mode 'jit-lock-mode)
	   ;; Prepare for jit-lock
	   (remove-hook 'after-change-functions
			'font-lock-after-change-function t)
	   (set (make-local-variable 'font-lock-fontify-buffer-function)
		'jit-lock-refontify)
	   ;; Don't fontify eagerly (and don't abort if the buffer is large).
	   (set (make-local-variable 'font-lock-fontified) t)
	   ;; Use jit-lock.
	   (jit-lock-register 'font-lock-fontify-region
			      (not font-lock-keywords-only))))))

(defun font-lock-turn-off-thing-lock ()
  (cond ((and (boundp 'fast-lock-mode) fast-lock-mode)
	 (fast-lock-mode -1))
	((and (boundp 'jit-lock-mode) jit-lock-mode)
	 (jit-lock-unregister 'font-lock-fontify-region)
	 ;; Reset local vars to the non-jit-lock case.
	 (kill-local-variable 'font-lock-fontify-buffer-function))
	((and (boundp 'lazy-lock-mode) lazy-lock-mode)
	 (lazy-lock-mode -1))))

(defun font-lock-after-fontify-buffer ()
  (cond ((and (boundp 'fast-lock-mode) fast-lock-mode)
	 (fast-lock-after-fontify-buffer))
	;; Useless now that jit-lock intercepts font-lock-fontify-buffer.  -sm
	;; (jit-lock-mode
	;;  (jit-lock-after-fontify-buffer))
	((and (boundp 'lazy-lock-mode) lazy-lock-mode)
	 (lazy-lock-after-fontify-buffer))))

(defun font-lock-after-unfontify-buffer ()
  (cond ((and (boundp 'fast-lock-mode) fast-lock-mode)
	 (fast-lock-after-unfontify-buffer))
	;; Useless as well.  It's only called when:
	;; - turning off font-lock: it does not matter if we leave spurious
	;;   `fontified' text props around since jit-lock-mode is also off.
	;; - font-lock-default-fontify-buffer fails: this is not run
	;;   any more anyway.   -sm
	;;
	;; (jit-lock-mode
	;;  (jit-lock-after-unfontify-buffer))
	((and (boundp 'lazy-lock-mode) lazy-lock-mode)
	 (lazy-lock-after-unfontify-buffer))))

;;; End of Font Lock Support mode.

;;; Fontification functions.

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
;; comments are just too hairy for `font-lock-fontify-syntactically-region' to
;; cope with.  You need to write your own version of that function, e.g.,
;; `hairy-fontify-syntactically-region', and make your own version of
;; `hairy-fontify-region' call that function before calling
;; `font-lock-fontify-keywords-region' for the normal regexp fontification
;; pass.  And Hairy mode would set `font-lock-defaults' so that font-lock.el
;; would call your region fontification function instead of its own.  For
;; example, TeX modes could fontify {\foo ...} and \bar{...}  etc. multi-line
;; directives correctly and cleanly.  (It is the same problem as fontifying
;; multi-line strings and comments; regexps are not appropriate for the job.)

;;;###autoload
(defun font-lock-fontify-buffer ()
  "Fontify the current buffer the way the function `font-lock-mode' would."
  (interactive)
  (let ((font-lock-verbose (or font-lock-verbose (interactive-p))))
    (funcall font-lock-fontify-buffer-function)))

(defun font-lock-unfontify-buffer ()
  (funcall font-lock-unfontify-buffer-function))

(defun font-lock-fontify-region (beg end &optional loudly)
  (funcall font-lock-fontify-region-function beg end loudly))

(defun font-lock-unfontify-region (beg end)
  (save-buffer-state nil
    (funcall font-lock-unfontify-region-function beg end)))

(defun font-lock-default-fontify-buffer ()
  (let ((verbose (if (numberp font-lock-verbose)
		     (> (buffer-size) font-lock-verbose)
		   font-lock-verbose)))
    (with-temp-message
	(when verbose
	  (format "Fontifying %s..." (buffer-name)))
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
	  (quit (font-lock-unfontify-buffer)))))))

(defun font-lock-default-unfontify-buffer ()
  ;; Make sure we unfontify etc. in the whole buffer.
  (save-restriction
    (widen)
    (font-lock-unfontify-region (point-min) (point-max))
    (font-lock-after-unfontify-buffer)
    (setq font-lock-fontified nil)))

(defvar font-lock-dont-widen nil
  "If non-nil, font-lock will work on the non-widened buffer.
Useful for things like RMAIL and Info where the whole buffer is not
a very meaningful entity to highlight.")

(defun font-lock-default-fontify-region (beg end loudly)
  (save-buffer-state
      ((parse-sexp-lookup-properties
        (or parse-sexp-lookup-properties font-lock-syntactic-keywords))
       (old-syntax-table (syntax-table)))
    (unwind-protect
	(save-restriction
	  (unless font-lock-dont-widen (widen))
	  ;; Use the fontification syntax table, if any.
	  (when font-lock-syntax-table
	    (set-syntax-table font-lock-syntax-table))
          (goto-char beg)
          (setq beg (line-beginning-position (- 1 font-lock-lines-before)))
	  ;; check to see if we should expand the beg/end area for
	  ;; proper multiline matches
	  (when (and font-lock-multiline
		     (> beg (point-min))
		     (get-text-property (1- beg) 'font-lock-multiline))
	    ;; We are just after or in a multiline match.
	    (setq beg (or (previous-single-property-change
			   beg 'font-lock-multiline)
			  (point-min)))
	    (goto-char beg)
	    (setq beg (line-beginning-position)))
	  (when font-lock-multiline
	    (setq end (or (text-property-any end (point-max)
					     'font-lock-multiline nil)
			  (point-max))))
	  (goto-char end)
	  (setq end (line-beginning-position 2))
	  ;; Now do the fontification.
	  (font-lock-unfontify-region beg end)
	  (when font-lock-syntactic-keywords
	    (font-lock-fontify-syntactic-keywords-region beg end))
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

(defvar font-lock-extra-managed-props nil
  "Additional text properties managed by font-lock.
This is used by `font-lock-default-unfontify-region' to decide
what properties to clear before refontifying a region.")

(defun font-lock-default-unfontify-region (beg end)
  (remove-list-of-text-properties
   beg end (append
	    font-lock-extra-managed-props
	    (if font-lock-syntactic-keywords
		'(syntax-table face font-lock-multiline)
	      '(face font-lock-multiline)))))

;; Called when any modification is made to buffer text.
(defun font-lock-after-change-function (beg end old-len)
  (let ((inhibit-point-motion-hooks t)
	(inhibit-quit t))
    (save-excursion
      (save-match-data
	;; Rescan between start of lines enclosing the region.
	(font-lock-fontify-region
	 (progn (goto-char beg) (forward-line 0) (point))
	 (progn (goto-char end) (forward-line 1) (point)))))))

(defun font-lock-fontify-block (&optional arg)
  "Fontify some lines the way `font-lock-fontify-buffer' would.
The lines could be a function or paragraph, or a specified number of lines.
If ARG is given, fontify that many lines before and after point, or 16 lines if
no ARG is given and `font-lock-mark-block-function' is nil.
If `font-lock-mark-block-function' non-nil and no ARG is given, it is used to
delimit the region to fontify."
  (interactive "P")
  (let ((inhibit-point-motion-hooks t) font-lock-beginning-of-syntax-function
	deactivate-mark)
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

(if (boundp 'facemenu-keymap)
    (define-key facemenu-keymap "\M-o" 'font-lock-fontify-block))

;;; End of Fontification functions.

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

;;; End of Additional text property functions.

;;; Syntactic regexp fontification functions.

;; These syntactic keyword pass functions are identical to those keyword pass
;; functions below, with the following exceptions; (a) they operate on
;; `font-lock-syntactic-keywords' of course, (b) they are all `defun' as speed
;; is less of an issue, (c) eval of property value does not occur JIT as speed
;; is less of an issue, (d) OVERRIDE cannot be `prepend' or `append' as it
;; makes no sense for `syntax-table' property values, (e) they do not do it
;; LOUDLY as it is not likely to be intensive.

(defun font-lock-apply-syntactic-highlight (highlight)
  "Apply HIGHLIGHT following a match.
HIGHLIGHT should be of the form MATCH-HIGHLIGHT,
see `font-lock-syntactic-keywords'."
  (let* ((match (nth 0 highlight))
	 (start (match-beginning match)) (end (match-end match))
	 (value (nth 1 highlight))
	 (override (nth 2 highlight)))
    (if (not start)
	;; No match but we might not signal an error.
	(or (nth 3 highlight)
	    (error "No match %d in highlight %S" match highlight))
      (when (and (consp value) (not (numberp (car value))))
	(setq value (eval value)))
      (when (stringp value) (setq value (string-to-syntax value)))
      ;; Flush the syntax-cache.  I believe this is not necessary for
      ;; font-lock's use of syntax-ppss, but I'm not 100% sure and it can
      ;; still be necessary for other users of syntax-ppss anyway.
      (syntax-ppss-after-change-function start)
      (cond
       ((not override)
	;; Cannot override existing fontification.
	(or (text-property-not-all start end 'syntax-table nil)
	    (put-text-property start end 'syntax-table value)))
       ((eq override t)
	;; Override existing fontification.
	(put-text-property start end 'syntax-table value))
       ((eq override 'keep)
	;; Keep existing fontification.
	(font-lock-fillin-text-property start end 'syntax-table value))))))

(defun font-lock-fontify-syntactic-anchored-keywords (keywords limit)
  "Fontify according to KEYWORDS until LIMIT.
KEYWORDS should be of the form MATCH-ANCHORED, see `font-lock-keywords',
LIMIT can be modified by the value of its PRE-MATCH-FORM."
  (let ((matcher (nth 0 keywords)) (lowdarks (nthcdr 3 keywords)) highlights
	;; Evaluate PRE-MATCH-FORM.
	(pre-match-value (eval (nth 1 keywords))))
    ;; Set LIMIT to value of PRE-MATCH-FORM or the end of line.
    (if (and (numberp pre-match-value) (> pre-match-value (point)))
	(setq limit pre-match-value)
      (setq limit (line-end-position)))
    (save-match-data
      ;; Find an occurrence of `matcher' before `limit'.
      (while (if (stringp matcher)
		 (re-search-forward matcher limit t)
	       (funcall matcher limit))
	;; Apply each highlight to this instance of `matcher'.
	(setq highlights lowdarks)
	(while highlights
	  (font-lock-apply-syntactic-highlight (car highlights))
	  (setq highlights (cdr highlights)))))
    ;; Evaluate POST-MATCH-FORM.
    (eval (nth 2 keywords))))

(defun font-lock-fontify-syntactic-keywords-region (start end)
  "Fontify according to `font-lock-syntactic-keywords' between START and END.
START should be at the beginning of a line."
  ;; Ensure the beginning of the file is properly syntactic-fontified.
  (when (and font-lock-syntactically-fontified
	     (< font-lock-syntactically-fontified start))
    (setq start (max font-lock-syntactically-fontified (point-min)))
    (setq font-lock-syntactically-fontified end))
  ;; If `font-lock-syntactic-keywords' is a symbol, get the real keywords.
  (when (symbolp font-lock-syntactic-keywords)
    (setq font-lock-syntactic-keywords (font-lock-eval-keywords
					font-lock-syntactic-keywords)))
  ;; If `font-lock-syntactic-keywords' is not compiled, compile it.
  (unless (eq (car font-lock-syntactic-keywords) t)
    (setq font-lock-syntactic-keywords (font-lock-compile-keywords
					font-lock-syntactic-keywords)))
  ;; Get down to business.
  (let ((case-fold-search font-lock-keywords-case-fold-search)
	(keywords (cddr font-lock-syntactic-keywords))
	keyword matcher highlights)
    (while keywords
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
	      (font-lock-apply-syntactic-highlight (car highlights))
	    (font-lock-fontify-syntactic-anchored-keywords (car highlights)
							   end))
	  (setq highlights (cdr highlights))))
      (setq keywords (cdr keywords)))))

;;; End of Syntactic regexp fontification functions.

;;; Syntactic fontification functions.

(defvar font-lock-comment-start-skip nil
  "If non-nil, Font Lock mode uses this instead of `comment-start-skip'.")

(defvar font-lock-comment-end-skip nil
  "If non-nil, Font Lock mode uses this instead of `comment-end'.")

(defun font-lock-fontify-syntactically-region (start end &optional loudly ppss)
  "Put proper face on each string and comment between START and END.
START should be at the beginning of a line."
  (let ((comment-end-regexp
	 (or font-lock-comment-end-skip
	     (regexp-quote
	      (replace-regexp-in-string "^ *" "" comment-end))))
        state face beg)
    (if loudly (message "Fontifying %s... (syntactically...)" (buffer-name)))
    (goto-char start)
    ;;
    ;; Find the `start' state.
    (setq state (or ppss (syntax-ppss start)))
    ;;
    ;; Find each interesting place between here and `end'.
    (while
	(progn
	  (when (or (nth 3 state) (nth 4 state))
	    (setq face (funcall font-lock-syntactic-face-function state))
	    (setq beg (max (nth 8 state) start))
	    (setq state (parse-partial-sexp (point) end nil nil state
					    'syntax-table))
	    (when face (put-text-property beg (point) 'face face))
	    (when (and (eq face 'font-lock-comment-face)
                       (or font-lock-comment-start-skip
			   comment-start-skip))
	      ;; Find the comment delimiters
	      ;; and use font-lock-comment-delimiter-face for them.
	      (save-excursion
		(goto-char beg)
		(if (looking-at (or font-lock-comment-start-skip
				    comment-start-skip))
		    (put-text-property beg (match-end 0) 'face
				       font-lock-comment-delimiter-face)))
	      (if (looking-back comment-end-regexp (point-at-bol) t)
		  (put-text-property (match-beginning 0) (point) 'face
				     font-lock-comment-delimiter-face))))
	  (< (point) end))
      (setq state (parse-partial-sexp (point) end nil nil state
				      'syntax-table)))))

;;; End of Syntactic fontification functions.

;;; Keyword regexp fontification functions.

(defsubst font-lock-apply-highlight (highlight)
  "Apply HIGHLIGHT following a match.
HIGHLIGHT should be of the form MATCH-HIGHLIGHT, see `font-lock-keywords'."
  (let* ((match (nth 0 highlight))
	 (start (match-beginning match)) (end (match-end match))
	 (override (nth 2 highlight)))
    (if (not start)
	;; No match but we might not signal an error.
	(or (nth 3 highlight)
	    (error "No match %d in highlight %S" match highlight))
      (let ((val (eval (nth 1 highlight))))
	(when (eq (car-safe val) 'face)
	  (add-text-properties start end (cddr val))
	  (setq val (cadr val)))
	(cond
	 ((not (or val (eq override t)))
	  ;; If `val' is nil, don't do anything.  It is important to do it
	  ;; explicitly, because when adding nil via things like
	  ;; font-lock-append-text-property, the property is actually
	  ;; changed from <face> to (<face>) which is undesirable.  --Stef
	  nil)
	 ((not override)
	  ;; Cannot override existing fontification.
	  (or (text-property-not-all start end 'face nil)
	      (put-text-property start end 'face val)))
	 ((eq override t)
	  ;; Override existing fontification.
	  (put-text-property start end 'face val))
	 ((eq override 'prepend)
	  ;; Prepend to existing fontification.
	  (font-lock-prepend-text-property start end 'face val))
	 ((eq override 'append)
	  ;; Append to existing fontification.
	  (font-lock-append-text-property start end 'face val))
	 ((eq override 'keep)
	  ;; Keep existing fontification.
	  (font-lock-fillin-text-property start end 'face val)))))))

(defsubst font-lock-fontify-anchored-keywords (keywords limit)
  "Fontify according to KEYWORDS until LIMIT.
KEYWORDS should be of the form MATCH-ANCHORED, see `font-lock-keywords',
LIMIT can be modified by the value of its PRE-MATCH-FORM."
  (let ((matcher (nth 0 keywords)) (lowdarks (nthcdr 3 keywords)) highlights
	(lead-start (match-beginning 0))
	;; Evaluate PRE-MATCH-FORM.
	(pre-match-value (eval (nth 1 keywords))))
    ;; Set LIMIT to value of PRE-MATCH-FORM or the end of line.
    (if (not (and (numberp pre-match-value) (> pre-match-value (point))))
	(setq limit (line-end-position))
      (setq limit pre-match-value)
      (when (and font-lock-multiline (>= limit (line-beginning-position 2)))
	;; this is a multiline anchored match
	;; (setq font-lock-multiline t)
	(put-text-property (if (= limit (line-beginning-position 2))
			       (1- limit)
			     (min lead-start (point)))
			   limit
			   'font-lock-multiline t)))
    (save-match-data
      ;; Find an occurrence of `matcher' before `limit'.
      (while (and (< (point) limit)
		  (if (stringp matcher)
		      (re-search-forward matcher limit t)
		    (funcall matcher limit)))
	;; Apply each highlight to this instance of `matcher'.
	(setq highlights lowdarks)
	(while highlights
	  (font-lock-apply-highlight (car highlights))
	  (setq highlights (cdr highlights)))))
    ;; Evaluate POST-MATCH-FORM.
    (eval (nth 2 keywords))))

(defun font-lock-fontify-keywords-region (start end &optional loudly)
  "Fontify according to `font-lock-keywords' between START and END.
START should be at the beginning of a line.
LOUDLY, if non-nil, allows progress-meter bar."
  (unless (eq (car font-lock-keywords) t)
    (setq font-lock-keywords
	  (font-lock-compile-keywords font-lock-keywords t)))
  (let ((case-fold-search font-lock-keywords-case-fold-search)
	(keywords (cddr font-lock-keywords))
	(bufname (buffer-name)) (count 0)
        (pos (make-marker))
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
      (while (and (< (point) end)
		  (if (stringp matcher)
		      (re-search-forward matcher end t)
		    (funcall matcher end))
                  ;; Beware empty string matches since they will
                  ;; loop indefinitely.
                  (or (> (point) (match-beginning 0))
                      (progn (forward-char 1) t)))
	(when (and font-lock-multiline
		   (>= (point)
		       (save-excursion (goto-char (match-beginning 0))
				       (forward-line 1) (point))))
	  ;; this is a multiline regexp match
	  ;; (setq font-lock-multiline t)
	  (put-text-property (if (= (point)
				    (save-excursion
				      (goto-char (match-beginning 0))
				      (forward-line 1) (point)))
				 (1- (point))
			       (match-beginning 0))
			     (point)
			     'font-lock-multiline t))
	;; Apply each highlight to this instance of `matcher', which may be
	;; specific highlights or more keywords anchored to `matcher'.
	(setq highlights (cdr keyword))
	(while highlights
	  (if (numberp (car (car highlights)))
	      (font-lock-apply-highlight (car highlights))
	    (set-marker pos (point))
            (font-lock-fontify-anchored-keywords (car highlights) end)
            ;; Ensure forward progress.  `pos' is a marker because anchored
            ;; keyword may add/delete text (this happens e.g. in grep.el).
            (if (< (point) pos) (goto-char pos)))
	  (setq highlights (cdr highlights))))
      (setq keywords (cdr keywords)))
    (set-marker pos nil)))

;;; End of Keyword regexp fontification functions.

;; Various functions.

(defun font-lock-compile-keywords (keywords &optional regexp)
  "Compile KEYWORDS into the form (t KEYWORDS COMPILED...)
Here each COMPILED is of the form (MATCHER HIGHLIGHT ...) as shown in the
`font-lock-keywords' doc string.
If REGEXP is non-nil, it means these keywords are used for
`font-lock-keywords' rather than for `font-lock-syntactic-keywords'."
  (if (eq (car-safe keywords) t)
      keywords
    (setq keywords
	  (cons t (cons keywords
			(mapcar 'font-lock-compile-keyword keywords))))
    (if (and regexp
	     (eq (or syntax-begin-function
		     font-lock-beginning-of-syntax-function)
		 'beginning-of-defun)
	     (not beginning-of-defun-function))
	;; Try to detect when a string or comment contains something that
	;; looks like a defun and would thus confuse font-lock.
	(nconc keywords
	       `((,(if defun-prompt-regexp
		       (concat "^\\(?:" defun-prompt-regexp "\\)?\\s(")
		     "^\\s(")
		  (0
		   (if (memq (get-text-property (match-beginning 0) 'face)
			     '(font-lock-string-face font-lock-doc-face
			       font-lock-comment-face))
		       font-lock-warning-face)
		   prepend)))))
    keywords))

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

(defun font-lock-eval-keywords (keywords)
  "Evalulate KEYWORDS if a function (funcall) or variable (eval) name."
  (if (listp keywords)
      keywords
    (font-lock-eval-keywords (if (fboundp keywords)
				 (funcall keywords)
			       (eval keywords)))))

(defun font-lock-value-in-major-mode (alist)
  "Return value in ALIST for `major-mode', or ALIST if it is not an alist.
Structure is ((MAJOR-MODE . VALUE) ...) where MAJOR-MODE may be t."
  (if (consp alist)
      (cdr (or (assq major-mode alist) (assq t alist)))
    alist))

(defun font-lock-choose-keywords (keywords level)
  "Return LEVELth element of KEYWORDS.
A LEVEL of nil is equal to a LEVEL of 0, a LEVEL of t is equal to
\(1- (length KEYWORDS))."
  (cond ((not (and (listp keywords) (symbolp (car keywords))))
	 keywords)
	((numberp level)
	 (or (nth level keywords) (car (reverse keywords))))
	((eq level t)
	 (car (reverse keywords)))
	(t
	 (car keywords))))

(defvar font-lock-set-defaults nil)	; Whether we have set up defaults.

(defvar font-lock-mode-major-mode)
(defun font-lock-set-defaults ()
  "Set fontification defaults appropriately for this mode.
Sets various variables using `font-lock-defaults' (or, if nil, using
`font-lock-defaults-alist') and `font-lock-maximum-decoration'."
  ;; Set fontification defaults iff not previously set for correct major mode.
  (unless (and font-lock-set-defaults
	       (eq font-lock-mode-major-mode major-mode))
    (setq font-lock-mode-major-mode major-mode)
    (set (make-local-variable 'font-lock-set-defaults) t)
    (make-local-variable 'font-lock-fontified)
    (make-local-variable 'font-lock-multiline)
    (let* ((defaults (or font-lock-defaults
			 (cdr (assq major-mode
				    (with-no-warnings
				     font-lock-defaults-alist)))))
	   (keywords
	    (font-lock-choose-keywords (nth 0 defaults)
				       (font-lock-value-in-major-mode font-lock-maximum-decoration)))
	   (local (cdr (assq major-mode font-lock-keywords-alist)))
	   (removed-keywords
	    (cdr-safe (assq major-mode font-lock-removed-keywords-alist))))
      (set (make-local-variable 'font-lock-defaults) defaults)
      ;; Syntactic fontification?
      (when (nth 1 defaults)
	(set (make-local-variable 'font-lock-keywords-only) t))
      ;; Case fold during regexp fontification?
      (when (nth 2 defaults)
	(set (make-local-variable 'font-lock-keywords-case-fold-search) t))
      ;; Syntax table for regexp and syntactic fontification?
      (when (nth 3 defaults)
	(set (make-local-variable 'font-lock-syntax-table)
	     (copy-syntax-table (syntax-table)))
	(dolist (selem (nth 3 defaults))
	  ;; The character to modify may be a single CHAR or a STRING.
	  (let ((syntax (cdr selem)))
	    (dolist (char (if (numberp (car selem))
			      (list (car selem))
			    (mapcar 'identity (car selem))))
	      (modify-syntax-entry char syntax font-lock-syntax-table)))))
      ;; Syntax function for syntactic fontification?
      (when (nth 4 defaults)
	(set (make-local-variable 'font-lock-beginning-of-syntax-function)
	     (nth 4 defaults)))
      ;; Variable alist?
      (dolist (x (nthcdr 5 defaults))
	(set (make-local-variable (car x)) (cdr x)))
      ;; Set up `font-lock-keywords' last because its value might depend
      ;; on other settings (e.g. font-lock-compile-keywords uses
      ;; font-lock-beginning-of-syntax-function).
      (set (make-local-variable 'font-lock-keywords)
	   (font-lock-eval-keywords keywords))
      ;; Local fontification?
      (while local
	(font-lock-add-keywords nil (car (car local)) (cdr (car local)))
	(setq local (cdr local)))
      (when removed-keywords
	(font-lock-remove-keywords nil removed-keywords))
      ;; Now compile the keywords.
      (unless (eq (car font-lock-keywords) t)
	(set (make-local-variable 'font-lock-keywords)
	     (font-lock-compile-keywords font-lock-keywords t))))))

;;; Colour etc. support.

;; Originally face attributes were specified via `font-lock-face-attributes'.
;; Users then changed the default face attributes by setting that variable.
;; However, we try and be back-compatible and respect its value if set except
;; for faces where M-x customize has been used to save changes for the face.
(when (boundp 'font-lock-face-attributes)
  (let ((face-attributes font-lock-face-attributes))
    (while face-attributes
      (let* ((face-attribute (pop face-attributes))
	     (face (car face-attribute)))
	;; Rustle up a `defface' SPEC from a `font-lock-face-attributes' entry.
	(unless (get face 'saved-face)
	  (let ((foreground (nth 1 face-attribute))
		(background (nth 2 face-attribute))
		(bold-p (nth 3 face-attribute))
		(italic-p (nth 4 face-attribute))
		(underline-p (nth 5 face-attribute))
		face-spec)
	    (when foreground
	      (setq face-spec (cons ':foreground (cons foreground face-spec))))
	    (when background
	      (setq face-spec (cons ':background (cons background face-spec))))
	    (when bold-p
	      (setq face-spec (append '(:weight bold) face-spec)))
	    (when italic-p
	      (setq face-spec (append '(:slant italic) face-spec)))
	    (when underline-p
	      (setq face-spec (append '(:underline t) face-spec)))
	    (custom-declare-face face (list (list t face-spec)) nil)))))))

;; But now we do it the custom way.  Note that `defface' will not overwrite any
;; faces declared above via `custom-declare-face'.
(defface font-lock-comment-face
  '((((class grayscale) (background light))
     (:foreground "DimGray" :weight bold :slant italic))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :weight bold :slant italic))
    (((class color) (min-colors 88) (background light))
     (:foreground "Firebrick"))
    (((class color) (min-colors 88) (background dark))
     (:foreground "chocolate1"))
    (((class color) (min-colors 16) (background light))
     (:foreground "red"))
    (((class color) (min-colors 16) (background dark))
     (:foreground "red1"))
    (((class color) (min-colors 8) (background light))
     )
    (((class color) (min-colors 8) (background dark))
     )
    (t (:weight bold :slant italic)))
  "Font Lock mode face used to highlight comments."
  :group 'font-lock-highlighting-faces)

(defface font-lock-comment-delimiter-face
  '((default :inherit font-lock-comment-face)
    (((class grayscale)))
    (((class color) (min-colors 16)))
    (((class color) (min-colors 8) (background light))
     :foreground "red")
    (((class color) (min-colors 8) (background dark))
     :foreground "red1"))
  "Font Lock mode face used to highlight comment delimiters."
  :group 'font-lock-highlighting-faces)

(defface font-lock-string-face
  '((((class grayscale) (background light)) (:foreground "DimGray" :slant italic))
    (((class grayscale) (background dark)) (:foreground "LightGray" :slant italic))
    (((class color) (min-colors 88) (background light)) (:foreground "RosyBrown"))
    (((class color) (min-colors 88) (background dark)) (:foreground "LightSalmon"))
    (((class color) (min-colors 16) (background light)) (:foreground "RosyBrown"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon"))
    (((class color) (min-colors 8)) (:foreground "green"))
    (t (:slant italic)))
  "Font Lock mode face used to highlight strings."
  :group 'font-lock-highlighting-faces)

(defface font-lock-doc-face
  '((t :inherit font-lock-string-face))
  "Font Lock mode face used to highlight documentation."
  :group 'font-lock-highlighting-faces)

(defface font-lock-keyword-face
  '((((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "Purple"))
    (((class color) (min-colors 88) (background dark)) (:foreground "Cyan1"))
    (((class color) (min-colors 16) (background light)) (:foreground "Purple"))
    (((class color) (min-colors 16) (background dark)) (:foreground "Cyan"))
    (((class color) (min-colors 8)) (:foreground "cyan" :weight bold))
    (t (:weight bold)))
  "Font Lock mode face used to highlight keywords."
  :group 'font-lock-highlighting-faces)

(defface font-lock-builtin-face
  '((((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "Orchid"))
    (((class color) (min-colors 88) (background dark)) (:foreground "LightSteelBlue"))
    (((class color) (min-colors 16) (background light)) (:foreground "Orchid"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightSteelBlue"))
    (((class color) (min-colors 8)) (:foreground "blue" :weight bold))
    (t (:weight bold)))
  "Font Lock mode face used to highlight builtins."
  :group 'font-lock-highlighting-faces)

(defface font-lock-function-name-face
  '((((class color) (min-colors 88) (background light)) (:foreground "Blue1"))
    (((class color) (min-colors 88) (background dark)) (:foreground "LightSkyBlue"))
    (((class color) (min-colors 16) (background light)) (:foreground "Blue"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue"))
    (((class color) (min-colors 8)) (:foreground "blue" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Font Lock mode face used to highlight function names."
  :group 'font-lock-highlighting-faces)

(defface font-lock-variable-name-face
  '((((class grayscale) (background light))
     (:foreground "Gray90" :weight bold :slant italic))
    (((class grayscale) (background dark))
     (:foreground "DimGray" :weight bold :slant italic))
    (((class color) (min-colors 88) (background light)) (:foreground "DarkGoldenrod"))
    (((class color) (min-colors 88) (background dark)) (:foreground "LightGoldenrod"))
    (((class color) (min-colors 16) (background light)) (:foreground "DarkGoldenrod"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightGoldenrod"))
    (((class color) (min-colors 8)) (:foreground "yellow" :weight light))
    (t (:weight bold :slant italic)))
  "Font Lock mode face used to highlight variable names."
  :group 'font-lock-highlighting-faces)

(defface font-lock-type-face
  '((((class grayscale) (background light)) (:foreground "Gray90" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "ForestGreen"))
    (((class color) (min-colors 88) (background dark)) (:foreground "PaleGreen"))
    (((class color) (min-colors 16) (background light)) (:foreground "ForestGreen"))
    (((class color) (min-colors 16) (background dark)) (:foreground "PaleGreen"))
    (((class color) (min-colors 8)) (:foreground "green"))
    (t (:weight bold :underline t)))
  "Font Lock mode face used to highlight type and classes."
  :group 'font-lock-highlighting-faces)

(defface font-lock-constant-face
  '((((class grayscale) (background light))
     (:foreground "LightGray" :weight bold :underline t))
    (((class grayscale) (background dark))
     (:foreground "Gray50" :weight bold :underline t))
    (((class color) (min-colors 88) (background light)) (:foreground "CadetBlue"))
    (((class color) (min-colors 88) (background dark)) (:foreground "Aquamarine"))
    (((class color) (min-colors 16) (background light)) (:foreground "CadetBlue"))
    (((class color) (min-colors 16) (background dark)) (:foreground "Aquamarine"))
    (((class color) (min-colors 8)) (:foreground "magenta"))
    (t (:weight bold :underline t)))
  "Font Lock mode face used to highlight constants and labels."
  :group 'font-lock-highlighting-faces)

(defface font-lock-warning-face
  '((((class color) (min-colors 88) (background light)) (:foreground "Red1" :weight bold))
    (((class color) (min-colors 88) (background dark)) (:foreground "Pink" :weight bold))
    (((class color) (min-colors 16) (background light)) (:foreground "Red" :weight bold))
    (((class color) (min-colors 16) (background dark)) (:foreground "Pink" :weight bold))
    (((class color) (min-colors 8)) (:foreground "red"))
    (t (:inverse-video t :weight bold)))
  "Font Lock mode face used to highlight warnings."
  :group 'font-lock-highlighting-faces)

(defface font-lock-negation-char-face
  '((t nil))
  "Font Lock mode face used to highlight easy to overlook negation."
  :group 'font-lock-highlighting-faces)

(defface font-lock-preprocessor-face
  '((t :inherit font-lock-builtin-face))
  "Font Lock mode face used to highlight preprocessor directives."
  :group 'font-lock-highlighting-faces)

(defface font-lock-regexp-grouping-backslash
  '((t :inherit bold))
  "Font Lock mode face for backslashes in Lisp regexp grouping constructs."
  :group 'font-lock-highlighting-faces)

(defface font-lock-regexp-grouping-construct
  '((t :inherit bold))
  "Font Lock mode face used to highlight grouping constructs in Lisp regexps."
  :group 'font-lock-highlighting-faces)

;;; End of Colour etc. support.

;;; Menu support.

;; This section of code is commented out because Emacs does not have real menu
;; buttons.  (We can mimic them by putting "( ) " or "(X) " at the beginning of
;; the menu entry text, but with Xt it looks both ugly and embarrassingly
;; amateur.)  If/When Emacs gets real menus buttons, put in menu-bar.el after
;; the entry for "Text Properties" something like:
;;
;; (define-key menu-bar-edit-menu [font-lock]
;;   (cons "Syntax Highlighting" font-lock-menu))
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
; ;; Put the appropriate symbol property values on now.  See above.
;(put 'global-font-lock-mode 'menu-selected 'global-font-lock-mode)
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
; ;; This should be called by `font-lock-set-defaults'.
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
; ;; This should be called by `font-lock-unset-defaults'.
;(defun font-lock-unset-menu ()
;  ;; Deactivate less/more fontification entries.
;  (setq font-lock-fontify-level nil))

;;; End of Menu support.

;;; Various regexp information shared by several modes.
; ;; Information specific to a single mode should go in its load library.

;; Font Lock support for C, C++, Objective-C and Java modes is now in
;; cc-fonts.el (and required by cc-mode.el).  However, the below function
;; should stay in font-lock.el, since it is used by other libraries.  sm.

(defun font-lock-match-c-style-declaration-item-and-skip-to-next (limit)
  "Match, and move over, any declaration/definition item after point.
Matches after point, but ignores leading whitespace and `*' characters.
Does not move further than LIMIT.

The expected syntax of a declaration/definition item is `word' (preceded by
optional whitespace and `*' characters and proceeded by optional whitespace)
optionally followed by a `('.  Everything following the item (but belonging to
it) is expected to be skip-able by `scan-sexps', and items are expected to be
separated with a `,' and to be terminated with a `;'.

Thus the regexp matches after point:	word (
					^^^^ ^
Where the match subexpressions are:	  1  2

The item is delimited by (match-beginning 1) and (match-end 1).
If (match-beginning 2) is non-nil, the item is followed by a `('.

This function could be MATCHER in a MATCH-ANCHORED `font-lock-keywords' item."
  (when (looking-at "[ \n\t*]*\\(\\sw+\\)[ \t\n]*\\(((?\\)?")
    (when (and (match-end 2) (> (- (match-end 2) (match-beginning 2)) 1))
      ;; If `word' is followed by a double open-paren, it's probably
      ;; a macro used for "int myfun P_ ((int arg1))".  Let's go back one
      ;; word to try and match `myfun' rather than `P_'.
      (let ((pos (point)))
	(skip-chars-backward " \t\n")
	(skip-syntax-backward "w")
	(unless (looking-at "\\(\\sw+\\)[ \t\n]*\\sw+[ \t\n]*\\(((?\\)?")
	  ;; Looks like it was something else, so go back to where we
	  ;; were and reset the match data by rematching.
	  (goto-char pos)
	  (looking-at "[ \n\t*]*\\(\\sw+\\)[ \t\n]*\\(((?\\)?"))))
    (save-match-data
      (condition-case nil
	  (save-restriction
	    ;; Restrict to the LIMIT.
	    (narrow-to-region (point-min) limit)
	    (goto-char (match-end 1))
	    ;; Move over any item value, etc., to the next item.
	    (while (not (looking-at "[ \t\n]*\\(\\(,\\)\\|;\\|\\'\\)"))
	      (goto-char (or (scan-sexps (point) 1) (point-max))))
	    (goto-char (match-end 2)))
	(error t)))))

;; Lisp.

(defconst lisp-font-lock-keywords-1
  (eval-when-compile
    `(;; Definitions.
      (,(concat "(\\(def\\("
		;; Function declarations.
		"\\(advice\\|varalias\\|alias\\|generic\\|macro\\*?\\|method\\|"
		"setf\\|subst\\*?\\|un\\*?\\|"
		"ine-\\(condition\\|\\(?:derived\\|minor\\|generic\\)-mode\\|"
		"method-combination\\|setf-expander\\|skeleton\\|widget\\|"
		"function\\|\\(compiler\\|modify\\|symbol\\)-macro\\)\\)\\|"
		;; Variable declarations.
		"\\(const\\(ant\\)?\\|custom\\|face\\|parameter\\|var\\)\\|"
		;; Structure declarations.
		"\\(class\\|group\\|theme\\|package\\|struct\\|type\\)"
		"\\)\\)\\>"
		;; Any whitespace and defined object.
		"[ \t'\(]*"
		"\\(setf[ \t]+\\sw+)\\|\\sw+\\)?")
       (1 font-lock-keyword-face)
       (9 (cond ((match-beginning 3) font-lock-function-name-face)
		((match-beginning 6) font-lock-variable-name-face)
		(t font-lock-type-face))
	  nil t))
      ;; Emacs Lisp autoload cookies.
      ("^;;;###\\(autoload\\)" 1 font-lock-warning-face prepend)
      ;; Regexp negated char group.
      ("\\[\\(\\^\\)" 1 font-lock-negation-char-face prepend)))
  "Subdued level highlighting for Lisp modes.")

(defconst lisp-font-lock-keywords-2
  (append lisp-font-lock-keywords-1
   (eval-when-compile
     `(;; Control structures.  Emacs Lisp forms.
       (,(concat
	  "(" (regexp-opt
	       '("cond" "if" "while" "while-no-input" "let" "let*"
		 "prog" "progn" "progv" "prog1" "prog2" "prog*"
		 "inline" "lambda" "save-restriction" "save-excursion"
		 "save-window-excursion" "save-selected-window"
		 "save-match-data" "save-current-buffer" "unwind-protect"
		 "condition-case" "track-mouse"
		 "eval-after-load" "eval-and-compile" "eval-when-compile"
		 "eval-when"
		 "with-category-table"
		 "with-current-buffer" "with-electric-help"
		 "with-local-quit" "with-no-warnings"
		 "with-output-to-string" "with-output-to-temp-buffer"
		 "with-selected-window" "with-syntax-table"
		 "with-temp-buffer" "with-temp-file" "with-temp-message"
		 "with-timeout" "with-timeout-handler") t)
	  "\\>")
	  .  1)
       ;; Control structures.  Common Lisp forms.
       (,(concat
	  "(" (regexp-opt
	       '("when" "unless" "case" "ecase" "typecase" "etypecase"
		 "ccase" "ctypecase" "handler-case" "handler-bind"
		 "restart-bind" "restart-case" "in-package"
		 "break" "ignore-errors"
		 "loop" "do" "do*" "dotimes" "dolist" "the" "locally"
		 "proclaim" "declaim" "declare" "symbol-macrolet"
		 "lexical-let" "lexical-let*" "flet" "labels" "compiler-let"
		 "destructuring-bind" "macrolet" "tagbody" "block" "go"
		 "multiple-value-bind" "multiple-value-prog1"
		 "return" "return-from"
		 "with-accessors" "with-compilation-unit"
		 "with-condition-restarts" "with-hash-table-iterator"
		 "with-input-from-string" "with-open-file"
		 "with-open-stream" "with-output-to-string"
		 "with-package-iterator" "with-simple-restart"
		 "with-slots" "with-standard-io-syntax") t)
	  "\\>")
	  . 1)
       ;; Exit/Feature symbols as constants.
       (,(concat "(\\(catch\\|throw\\|featurep\\|provide\\|require\\)\\>"
		 "[ \t']*\\(\\sw+\\)?")
	(1 font-lock-keyword-face)
	(2 font-lock-constant-face nil t))
       ;; Erroneous structures.
       ("(\\(abort\\|assert\\|warn\\|check-type\\|cerror\\|error\\|signal\\)\\>" 1 font-lock-warning-face)
       ;; Words inside \\[] tend to be for `substitute-command-keys'.
       ("\\\\\\\\\\[\\(\\sw+\\)]" 1 font-lock-constant-face prepend)
       ;; Words inside `' tend to be symbol names.
       ("`\\(\\sw\\sw+\\)'" 1 font-lock-constant-face prepend)
       ;; Constant values.
       ("\\<:\\sw+\\>" 0 font-lock-builtin-face)
       ;; ELisp and CLisp `&' keywords as types.
       ("\\&\\sw+\\>" . font-lock-type-face)
       ;; ELisp regexp grouping constructs
       ((lambda (bound)
          (catch 'found
            ;; The following loop is needed to continue searching after matches
            ;; that do not occur in strings.  The associated regexp matches one
            ;; of `\\\\' `\\(' `\\(?:' `\\|' `\\)'.  `\\\\' has been included to
            ;; avoid highlighting, for example, `\\(' in `\\\\('.
            (while (re-search-forward "\\(\\\\\\\\\\)\\(?:\\(\\\\\\\\\\)\\|\\((\\(?:\\?:\\)?\\|[|)]\\)\\)" bound t)
              (unless (match-beginning 2)
                (let ((face (get-text-property (1- (point)) 'face)))
                  (when (or (and (listp face)
                                 (memq 'font-lock-string-face face))
                            (eq 'font-lock-string-face face))
                    (throw 'found t)))))))
        (1 'font-lock-regexp-grouping-backslash prepend)
        (3 'font-lock-regexp-grouping-construct prepend))
;;;  This is too general -- rms.
;;;  A user complained that he has functions whose names start with `do'
;;;  and that they get the wrong color.
;;;      ;; CL `with-' and `do-' constructs
;;;      ("(\\(\\(do-\\|with-\\)\\(\\s_\\|\\w\\)*\\)" 1 font-lock-keyword-face)
      )))
  "Gaudy level highlighting for Lisp modes.")

(defvar lisp-font-lock-keywords lisp-font-lock-keywords-1
  "Default expressions to highlight in Lisp modes.")

(provide 'font-lock)

;; arch-tag: 682327e4-64d8-4057-b20b-1fbb9f1fc54c
;;; font-lock.el ends here
