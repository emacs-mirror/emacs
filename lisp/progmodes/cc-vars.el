;;; cc-vars.el --- user customization variables for CC Mode

;; Copyright (C) 1985,1987,1992-2001 Free Software Foundation, Inc.

;; Authors:    2000- Martin Stjernholm
;;	       1998-1999 Barry A. Warsaw and Martin Stjernholm
;;             1992-1997 Barry A. Warsaw
;;             1987 Dave Detlefs and Stewart Clamen
;;             1985 Richard M. Stallman
;; Maintainer: bug-cc-mode@gnu.org
;; Created:    22-Apr-1997 (split from cc-mode.el)
;; Version:    See cc-mode.el
;; Keywords:   c languages oop

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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-dest-file)
		  (stringp byte-compile-dest-file))
	     (cons (file-name-directory byte-compile-dest-file) load-path)
	   load-path)))
    (require 'cc-bytecomp)))

(cc-require 'cc-defs)

;; Silence the compiler.
(cc-bytecomp-defun get-char-table)	; XEmacs 20+
(cc-bytecomp-defun char-table-range)	; Emacs 19+
(cc-bytecomp-defun char-table-p)	; Emacs 19+, XEmacs 20+

;; Pull in custom if it exists and is recent enough (the one in Emacs
;; 19.34 isn't).
(eval
 (cc-eval-when-compile
   (condition-case nil
       (progn
	 (require 'custom)
	 (or (fboundp 'defcustom) (error ""))
	 (require 'wid-edit)
	 '(progn			; Compile in the require's.
	    (require 'custom)
	    (require 'wid-edit)))
     (error
      (message "Warning: Compiling without Customize support \
since a (good enough) custom library wasn't found")
      (cc-bytecomp-defmacro define-widget (name class doc &rest args))
      (cc-bytecomp-defmacro defcustom (symbol value doc &rest args)
	`(defvar ,symbol ,value ,doc))
      nil))))


;;; Helpers

;; This widget will show up in newer versions of the Custom library
(or (get 'other 'widget-type)
    (define-widget 'other 'sexp
      "Matches everything, but doesn't let the user edit the value.
Useful as last item in a `choice' widget."
      :tag "Other"
      :format "%t%n"
      :value 'other))

(define-widget 'c-const-symbol 'item
  "An uneditable lisp symbol."
  :value nil
  :tag "Symbol"
  :format "%t: %v\n%d"
  :match (lambda (widget value) (symbolp value))
  :value-to-internal
  (lambda (widget value)
    (let ((s (if (symbolp value)
		 (symbol-name value)
	       value))
	  (l (widget-get widget :size)))
      (if l
	  (setq s (concat s (make-string (- l (length s)) ?\ ))))
      s))
  :value-to-external
  (lambda (widget value)
    (if (stringp value)
	(intern (progn
		  (string-match "\\`[^ ]*" value)
		  (match-string 0 value)))
      value)))

(defvar c-style-variables
  '(c-basic-offset c-comment-only-line-offset c-block-comment-prefix
    c-comment-prefix-regexp c-cleanup-list c-hanging-braces-alist
    c-hanging-colons-alist c-hanging-semi&comma-criteria c-backslash-column
    c-special-indent-hook c-label-minimum-indentation c-offsets-alist)
  "List of the style variables.")

(defmacro defcustom-c-stylevar (name val doc &rest args)
  "Defines a style variable."
  `(progn
     (put ',name 'c-stylevar-fallback ,val)
     (defcustom ,name 'set-from-style
       ,(concat doc "

This is a style variable.  Apart from the valid values described
above, it can be set to the symbol `set-from-style'.  In that case, it
takes its value from the style system (see `c-default-style' and
`c-styles-alist') when a CC Mode buffer is initialized.  Otherwise,
the value set here overrides the style system (there is a variable
`c-old-style-variable-behavior' that changes this, though).")
       ,@(plist-put
	  args ':type
	  `'(radio
	     (const :tag "Use style settings"
		    set-from-style)
	     ,(let ((type (eval (plist-get args ':type))))
		(unless (consp type)
		  (setq type (list type)))
		(unless (c-safe (plist-get (cdr type) ':value))
		  (setcdr type (append `(:value ,val)
				       (cdr type))))
		(unless (c-safe (plist-get (cdr type) ':tag))
		  (setcdr type (append '(:tag "Override style settings")
				       (cdr type))))
		type))))))

(defun c-valid-offset (offset)
  "Return non-nil iff OFFSET is a valid offset for a syntactic symbol.
See `c-offsets-alist'."
  (or (eq offset '+)
      (eq offset '-)
      (eq offset '++)
      (eq offset '--)
      (eq offset '*)
      (eq offset '/)
      (integerp offset)
      (vectorp offset)
      (functionp offset)
      (and (symbolp offset)
	   (or (boundp offset)
	       (fboundp offset)))
      (progn
	(while (and (consp offset)
		    (c-valid-offset (car offset)))
	  (setq offset (cdr offset)))
	(null offset))))



;;; User variables

(defcustom c-strict-syntax-p nil
  "*If non-nil, all syntactic symbols must be found in `c-offsets-alist'.
If the syntactic symbol for a particular line does not match a symbol
in the offsets alist, or if no non-nil offset value can be determined
for a symbol, an error is generated, otherwise no error is reported
and the syntactic symbol is ignored.

This variable is considered obsolete; it doesn't work well with lineup
functions that return nil to support the feature of using lists on
syntactic symbols in `c-offsets-alist'.  Please keep it set to nil."
  :type 'boolean
  :group 'c)

(defcustom c-echo-syntactic-information-p nil
  "*If non-nil, syntactic info is echoed when the line is indented."
  :type 'boolean
  :group 'c)

(defcustom-c-stylevar c-basic-offset 4
  "*Amount of basic offset used by + and - symbols in `c-offsets-alist'.
Also used as the indentation step when `c-syntactic-indentation' is
nil."
  :type 'integer
  :group 'c)

(defcustom c-tab-always-indent t
  "*Controls the operation of the TAB key.
If t, hitting TAB always just indents the current line.  If nil,
hitting TAB indents the current line if point is at the left margin or
in the line's indentation, otherwise it insert a `real' tab character
\(see note\).  If the symbol `other', then tab is inserted only within
literals -- defined as comments and strings -- and inside preprocessor
directives, but the line is always reindented.

Note: The value of `indent-tabs-mode' will determine whether a real
tab character will be inserted, or the equivalent number of spaces.
When inserting a tab, actually the function stored in the variable
`c-insert-tab-function' is called.

Note: indentation of lines containing only comments is also controlled
by the `c-comment-only-line-offset' variable."
  :type '(radio
	  :extra-offset 8
	  :format "%{C Tab Always Indent%}:\n   The TAB key:\n%v"
	  (const :tag "always indents, never inserts TAB" t)
	  (const :tag "indents in left margin, otherwise inserts TAB" nil)
	  (other :tag "inserts TAB in literals, otherwise indent" other))
  :group 'c)

(defcustom c-insert-tab-function 'insert-tab
  "*Function used when inserting a tab for \\[c-indent-command].
Only used when `c-tab-always-indent' indicates a `real' tab character
should be inserted.  Value must be a function taking no arguments."
  :type 'function
  :group 'c)

(defcustom c-syntactic-indentation t
  "*Whether the indentation should be controlled by the syntactic context.

If t, the indentation functions indents according to the syntactic
context, using the style settings specified by `c-offsets-alist'.

If nil, every line is just indented to the same level as the previous
one, and the \\[c-indent-command] command adjusts the indentation in steps
specified by `c-basic-offset'.  The indentation style have no effect
in this mode, nor any of the indentation associated variables,
e.g. `c-special-indent-hook'."
  :type 'boolean
  :group 'c)

(defcustom-c-stylevar c-comment-only-line-offset 0
  "*Extra offset for line which contains only the start of a comment.
Can contain an integer or a cons cell of the form:

 (NON-ANCHORED-OFFSET . ANCHORED-OFFSET)

Where NON-ANCHORED-OFFSET is the amount of offset given to
non-column-zero anchored comment-only lines, and ANCHORED-OFFSET is
the amount of offset to give column-zero anchored comment-only lines.
Just an integer as value is equivalent to (<val> . -1000).

Note that this variable only has effect when the `c-lineup-comment'
lineup function is used on the `comment-intro' syntactic symbol (the
default)."
  :type '(choice (integer :tag "Non-anchored offset" 0)
		 (cons :tag "Non-anchored & anchored offset"
		       :value (0 . 0)
		       :extra-offset 8
		       (integer :tag "Non-anchored offset")
		       (integer :tag "Anchored offset")))
  :group 'c)

(defcustom c-indent-comments-syntactically-p nil
  "*Specifies how \\[indent-for-comment] should handle comment-only lines.
When this variable is non-nil, comment-only lines are indented
according to syntactic analysis via `c-offsets-alist'.  Otherwise, the
comment is indented as if it was preceded by code.  Note that this
variable does not affect how the normal line indentation treats
comment-only lines."
  :type 'boolean
  :group 'c)

(make-obsolete-variable 'c-comment-continuation-stars
			'c-block-comment-prefix)

;; Although c-comment-continuation-stars is obsolete, we look at it in
;; some places in CC Mode anyway, so make the compiler ignore it
;; during our compilation.
(cc-bytecomp-obsolete-var c-comment-continuation-stars)
(cc-bytecomp-defvar c-comment-continuation-stars)

(defcustom-c-stylevar c-block-comment-prefix
  (if (boundp 'c-comment-continuation-stars)
      c-comment-continuation-stars
    "* ")
  "*Specifies the line prefix of continued C-style block comments.
You should set this variable to the literal string that gets inserted
at the front of continued block style comment lines.  This should
either be the empty string, or some characters without preceding
spaces.  To adjust the alignment under the comment starter, put an
appropriate value on the `c' syntactic symbol (see the
`c-offsets-alist' variable).

It's only used when a one-line block comment is broken into two or
more lines for the first time; otherwise the appropriate prefix is
adapted from the comment.  This variable is not used for C++ line
style comments."
  :type 'string
  :group 'c)

(defcustom-c-stylevar c-comment-prefix-regexp
  '((pike-mode . "//+!?\\|\\**")
    (other . "//+\\|\\**"))
  "*Regexp to match the line prefix inside comments.
This regexp is used to recognize the fill prefix inside comments for
correct paragraph filling and other things.

If this variable is a string, it will be used in all CC Mode major
modes.  It can also be an association list, to associate specific
regexps to specific major modes.  The symbol for the major mode is
looked up in the association list, and its value is used as the line
prefix regexp.  If it's not found, then the symbol `other' is looked
up and its value is used instead.

The regexp should match the prefix used in both C++ style line
comments and C style block comments, but it does not need to match a
block comment starter.  In other words, it should at least match
\"//\" for line comments and the string in `c-block-comment-prefix',
which is sometimes inserted by CC Mode inside block comments.  It
should not match any surrounding whitespace.

Note that CC Mode modifies other variables from this one at mode
initialization, so you will need to do \\[c-mode] (or whatever mode
you're currently using) if you change it in a CC Mode buffer."
  :type '(radio
	  (regexp :tag "Regexp for all modes")
	  (list
	   :tag "Mode-specific regexps"
	   (set
	    :inline t :format "%v"
	    (cons :format "%v"
		  (const :format "C     " c-mode) (regexp :format "%v"))
	    (cons :format "%v"
		  (const :format "C++   " c++-mode) (regexp :format "%v"))
	    (cons :format "%v"
		  (const :format "ObjC  " objc-mode) (regexp :format "%v"))
	    (cons :format "%v"
		  (const :format "Java  " java-mode) (regexp :format "%v"))
	    (cons :format "%v"
		  (const :format "IDL   " idl-mode) (regexp :format "%v"))
	    (cons :format "%v"
		  (const :format "Pike  " pike-mode) (regexp :format "%v")))
	   (cons :format "    %v"
		 (const :format "Other " other) (regexp :format "%v"))))
  :group 'c)

(defcustom c-ignore-auto-fill '(string cpp code)
  "*List of contexts in which automatic filling never occurs.
If Auto Fill mode is active, it will be temporarily disabled if point
is in any context on this list.  It's e.g. useful to enable Auto Fill
in comments only, but not in strings or normal code.  The valid
contexts are:

 string  -- inside a string or character literal
 c       -- inside a C style block comment
 c++     -- inside a C++ style line comment
 cpp     -- inside a preprocessor directive
 code    -- anywhere else, i.e. in normal code"
  :type '(set
	  :extra-offset 8
	  (const :tag "String literals" string)
	  (const :tag "C style block comments" c)
	  (const :tag "C++ style line comments" c++)
	  (const :tag "Preprocessor directives" cpp)
	  (const :tag "Normal code" code))
  :group 'c)

(defcustom-c-stylevar c-cleanup-list '(scope-operator)
  "*List of various C/C++/ObjC constructs to \"clean up\".
The following clean ups only take place when the auto-newline feature
is turned on, as evidenced by the `/a' or `/ah' appearing next to the
mode name:

 brace-else-brace    -- Clean up \"} else {\" constructs by placing
                        entire construct on a single line.  This clean
                        up only takes place when there is nothing but
                        white space between the braces and the `else'.
                        Clean up occurs when the open brace after the
                        `else' is typed.
 brace-elseif-brace  -- Similar to brace-else-brace, but clean up
                        \"} else if (...) {\" constructs.  Clean up
                        occurs after the open parenthesis and the open
                        brace.
 brace-catch-brace   -- Similar to brace-elseif-brace, but clean up
                        \"} catch (...) {\" constructs.
 empty-defun-braces  -- Clean up empty defun braces by placing the
                        braces on the same line.  Clean up occurs when
			the defun closing brace is typed.
 defun-close-semi    -- Clean up the terminating semi-colon on defuns
			by placing the semi-colon on the same line as
			the closing brace.  Clean up occurs when the
			semi-colon is typed.
 list-close-comma    -- Clean up commas following braces in array
                        and aggregate initializers.  Clean up occurs
			when the comma is typed.
 scope-operator      -- Clean up double colons which may designate
			a C++ scope operator split across multiple
			lines.  Note that certain C++ constructs can
			generate ambiguous situations.  This clean up
			only takes place when there is nothing but
			whitespace between colons.  Clean up occurs
			when the second colon is typed.

The following clean ups always take place when they are on this list,
regardless of the auto-newline feature, since they typically don't
involve auto-newline inserted newlines:

 space-before-funcall -- Insert exactly one space before the opening
                        parenthesis of a function call.  Clean up
                        occurs when the opening parenthesis is typed.
 compact-empty-funcall -- Clean up any space before the function call
			opening parenthesis if and only if the
                        argument list is empty.  This is typically
                        useful together with `space-before-funcall' to
                        get the style \"foo (bar)\" and \"foo()\".
                        Clean up occurs when the closing parenthesis
                        is typed."
  :type '(set
	  :extra-offset 8
	  (const :tag "Put \"} else {\" on one line"
		 brace-else-brace)
	  (const :tag "Put \"} else if (...) {\" on one line"
		 brace-elseif-brace)
	  (const :tag "Put \"} catch (...) {\" on one line"
		 brace-catch-brace)
	  (const :tag "Put empty defun braces on one line"
		 empty-defun-braces)
	  (const :tag "Put \"};\" ending defuns on one line"
		 defun-close-semi)
	  (const :tag "Put \"},\" in aggregates on one line"
		 list-close-comma)
	  (const :tag "Put C++ style \"::\" on one line"
		 scope-operator)
	  (const :tag "Put a space before funcall parens, e.g. \"foo (bar)\""
		 space-before-funcall)
	  (const :tag "Remove space before empty funcalls, e.g. \"foo()\""
		 compact-empty-funcall))
  :group 'c)

(defcustom-c-stylevar c-hanging-braces-alist '((brace-list-open)
					       (brace-entry-open)
					       (substatement-open after)
					       (block-close . c-snug-do-while)
					       (extern-lang-open after)
					       (inexpr-class-open after)
					       (inexpr-class-close before))
  "*Controls the insertion of newlines before and after braces
when the auto-newline feature is active.  This variable contains an
association list with elements of the following form:
\(SYNTACTIC-SYMBOL . ACTION).

When a brace (either opening or closing) is inserted, the syntactic
context it defines is looked up in this list, and if found, the
associated ACTION is used to determine where newlines are inserted.
If the context is not found, the default is to insert a newline both
before and after the brace.

SYNTACTIC-SYMBOL can be any of: defun-open, defun-close, class-open,
class-close, inline-open, inline-close, block-open, block-close,
substatement-open, statement-case-open, extern-lang-open,
extern-lang-close, brace-list-open, brace-list-close,
brace-list-intro, brace-entry-open, namespace-open, namespace-close,
inexpr-class-open, or inexpr-class-close.  See `c-offsets-alist' for
details, except for inexpr-class-open and inexpr-class-close, which
doesn't have any corresponding symbols there.  Those two symbols are
used for the opening and closing braces, respectively, of anonymous
inner classes in Java.

ACTION can be either a function symbol or a list containing any
combination of the symbols `before' or `after'.  If the list is empty,
no newlines are inserted either before or after the brace.

When ACTION is a function symbol, the function is called with a two
arguments: the syntactic symbol for the brace and the buffer position
at which the brace was inserted.  The function must return a list as
described in the preceding paragraph.  Note that during the call to
the function, the variable `c-syntactic-context' is set to the entire
syntactic context for the brace line."
  :type
  `(set ,@(mapcar
	   (lambda (elt)
	     `(cons :format "%v"
		    (c-const-symbol :format "%v: "
				    :size 20
				    :value ,elt)
		    (choice :format "%[Choice%] %v"
		     :value (before after)
			    (set :menu-tag "Before/after"
				 :format "Newline %v brace\n"
				 (const :format "%v, " before)
				 (const :format "%v" after))
			    (function :menu-tag "Function"
				      :format "Run function: %v"
				      :value c-))))
	   '(defun-open defun-close
	      class-open class-close
	      inline-open inline-close
	      block-open block-close
	      substatement-open statement-case-open
	      extern-lang-open extern-lang-close
	      brace-list-open brace-list-close
	      brace-list-intro brace-entry-open
	      namespace-open namespace-close
	      inexpr-class-open inexpr-class-close)))
    :group 'c)

(defcustom-c-stylevar c-hanging-colons-alist nil
  "*Controls the insertion of newlines before and after certain colons.
This variable contains an association list with elements of the
following form: (SYNTACTIC-SYMBOL . ACTION).

SYNTACTIC-SYMBOL can be any of: case-label, label, access-label,
member-init-intro, or inher-intro.

See the variable `c-hanging-braces-alist' for the semantics of this
variable.  Note however that making ACTION a function symbol is
currently not supported for this variable."
  :type
  `(set ,@(mapcar
	   (lambda (elt)
	     `(cons :format "%v"
		    (c-const-symbol :format "%v: "
				    :size 20
				    :value ,elt)
		    (set :format "Newline %v brace\n"
			 (const :format "%v, " before)
			 (const :format "%v" after))))
	   '(case-label label access-label member-init-intro inher-intro)))
  :group 'c)

(defcustom-c-stylevar c-hanging-semi&comma-criteria
  '(c-semi&comma-inside-parenlist)
  "*List of functions that decide whether to insert a newline or not.
The functions in this list are called, in order, whenever the
auto-newline minor mode is activated (as evidenced by a `/a' or `/ah'
string in the mode line), and a semicolon or comma is typed (see
`c-electric-semi&comma').  Each function in this list is called with
no arguments, and should return one of the following values:

  nil             -- no determination made, continue checking
  'stop           -- do not insert a newline, and stop checking
  (anything else) -- insert a newline, and stop checking

If every function in the list is called with no determination made,
then no newline is inserted."
  :type '(repeat function)
  :group 'c)

(defcustom-c-stylevar c-backslash-column 48
  "*Column to insert backslashes when macroizing a region."
  :type 'integer
  :group 'c)

(defcustom c-special-indent-hook nil
  "*Hook for user defined special indentation adjustments.
This hook gets called after a line is indented by the mode."
  :type 'hook
  :group 'c)

(defcustom c-backspace-function 'backward-delete-char-untabify
  "*Function called by `c-electric-backspace' when deleting backwards."
  :type 'function
  :group 'c)

(defcustom c-delete-function 'delete-char
  "*Function called by `c-electric-delete' when deleting forwards."
  :type 'function
  :group 'c)

(defcustom c-electric-pound-behavior nil
  "*List of behaviors for electric pound insertion.
Only currently supported behavior is `alignleft'."
  :type '(set :extra-offset 8 (const alignleft))
  :group 'c)

(defcustom-c-stylevar c-label-minimum-indentation 1
  "*Minimum indentation for lines inside of top-level constructs.
This variable typically only affects code using the `gnu' style, which
mandates a minimum of one space in front of every line inside
top-level constructs.  Specifically, the function
`c-gnu-impose-minimum' on your `c-special-indent-hook' is what
enforces this."
  :type 'integer
  :group 'c)

(defcustom c-progress-interval 5
  "*Interval used to update progress status during long re-indentation.
If a number, percentage complete gets updated after each interval of
that many seconds.  To inhibit all messages during indentation, set
this variable to nil."
  :type 'integer
  :group 'c)

(defcustom c-default-style '((java-mode . "java") (other . "gnu"))
  "*Style which gets installed by default when a file is visited.

The value of this variable can be any style defined in
`c-style-alist', including styles you add.  The value can also be an
association list of major mode symbols to style names.

When the value is a string, all CC Mode major modes will install this
style by default.

When the value is an alist, the major mode symbol is looked up in it
and the associated style is installed.  If the major mode is not
listed in the alist, then the symbol `other' is looked up in it, and
if found, the style in that entry is used.  If `other' is not found in
the alist, then \"gnu\" style is used.

The default style gets installed before your mode hooks run, so you
can always override the use of `c-default-style' by making calls to
`c-set-style' in the appropriate mode hook."
  :type '(radio
	  (string :tag "Style in all modes")
	  (set :tag "Mode-specific styles"
	    (cons :format "%v"
		  (const :format "C     " c-mode) (string :format "%v"))
	    (cons :format "%v"
		  (const :format "C++   " c++-mode) (string :format "%v"))
	    (cons :format "%v"
		  (const :format "ObjC  " objc-mode) (string :format "%v"))
	    (cons :format "%v"
		  (const :format "Java  " java-mode) (string :format "%v"))
	    (cons :format "%v"
		  (const :format "IDL   " idl-mode) (string :format "%v"))
	    (cons :format "%v"
		  (const :format "Pike  " pike-mode) (string :format "%v"))
	    (cons :format "%v"
		  (const :format "Other " other) (string :format "%v"))))
  :group 'c)

(put 'c-offsets-alist 'c-stylevar-fallback
     '((string                . c-lineup-dont-change)
       ;; Relpos: Beg of previous line.
       (c                     . c-lineup-C-comments)
       ;; Relpos: Beg of the comment.
       (defun-open            . 0)
       ;; Relpos: Boi at the func decl start when inside classes, bol
       ;; at the func decl start when at top level.
       (defun-close           . 0)
       ;; Relpos: Boi at the func decl start.
       (defun-block-intro     . +)
       ;; Relpos: Boi at the block open.
       (class-open            . 0)
       ;; Relpos: Boi at the class decl start.
       (class-close           . 0)
       ;; Relpos: Boi at the class decl start.
       (inline-open           . +)
       ;; Relpos: None for functions (inclass got the relpos then),
       ;; boi at the lambda start for lambdas.
       (inline-close          . 0)
       ;; Relpos: For functions: Boi at the func decl start.  For
       ;; lambdas: At the block open if it's at boi, at the boi of the
       ;; lambda start otherwise.
       (func-decl-cont        . +)
       ;; Relpos: Boi at the func decl start.
       (knr-argdecl-intro     . +)
       ;; Relpos: Boi at the current line.
       (knr-argdecl           . 0)
       ;; Relpos: Boi at the argdecl intro line.
       (topmost-intro         . 0)
       ;; Relpos: Bol at the last line of previous construct.
       (topmost-intro-cont    . 0)
       ;; Relpos: Boi at the topmost intro line.
       (member-init-intro     . +)
       ;; Relpos: Boi at the func decl arglist open.
       (member-init-cont      . c-lineup-multi-inher)
       ;; Relpos: Beg of the first member init.
       (inher-intro           . +)
       ;; Relpos: Java: Boi at the class decl start.  Otherwise: Boi
       ;; of current line (a bug?), unless it begins with an inher
       ;; start colon, in which case boi of previous line is used.
       (inher-cont            . c-lineup-multi-inher)
       ;; Relpos: Java: At the implements/extends keyword start.
       ;; Otherwise: At the inher start colon, or boi at the class
       ;; decl start if the first inherit clause hangs and it's not a
       ;; func-local inherit clause (when does that occur?).
       (block-open            . 0)
       ;; Relpos: Inexpr statement: Boi at the preceding paren.
       ;; Otherwise: None.
       (block-close           . 0)
       ;; Relpos: At the open brace if it's at boi.  Otherwise boi at
       ;; the start of the statement the open brace hangs on, or boi
       ;; at the preceding paren for inexpr statements.
       (brace-list-open       . 0)
       ;; Relpos: Boi at the brace list decl start, but a starting
       ;; "typedef" token is ignored.
       (brace-list-close      . 0)
       ;; Relpos: Boi at the brace list open.
       (brace-list-intro      . +)
       ;; Relpos: Boi at the brace list open.
       (brace-list-entry      . 0)
       ;; Relpos: At the first non-ws char after the open paren if the
       ;; first token is on the same line, otherwise boi at that
       ;; token.
       (brace-entry-open      . 0)
       ;; Relpos: Same as brace-list-entry.
       (statement             . 0)
       ;; Relpos: After a ';' in the condition clause of a for
       ;; statement: At the first token after the starting paren.
       ;; Otherwise: Boi at the start of the closest non-hanging
       ;; previous statement, but after any switch label.
       (statement-cont        . +)
       ;; Relpos: After the first token in the condition clause of a
       ;; for statement: At the first token after the starting paren.
       ;; On the first line in a continued expression that starts with
       ;; a stream op and there's no stream op on the previous line:
       ;; Boi of previous line.  Otherwise: Boi at the beginning of
       ;; the statement, but after any type of label.
       (statement-block-intro . +)
       ;; Relpos: At the block start if it's at boi, otherwise boi at
       ;; the start of the statement the open brace hangs on, or boi
       ;; at the preceding paren for inexpr statements.
       (statement-case-intro  . +)
       ;; Relpos: At the label keyword (always at boi).
       (statement-case-open   . 0)
       ;; Relpos: At the label keyword (always at boi).
       (substatement          . +)
       ;; Relpos: Boi at the containing statement or else clause.
       (substatement-open     . +)
       ;; Relpos: Boi at the containing statement or else clause.
       (case-label            . 0)
       ;; Relpos: At the switch block start if it's at boi, otherwise
       ;; boi at the start of the switch condition clause.
       (access-label          . -)
       ;; Relpos: Eol (a bug?).
       (label                 . 2)
       ;; Relpos: At the start of the containing block if it's at boi,
       ;; otherwise boi at the start of the sexp before the block.
       (do-while-closure      . 0)
       ;; Relpos: Boi at the corresponding while keyword.
       (else-clause           . 0)
       ;; Relpos: Boi at the corresponding if keyword.
       (catch-clause          . 0)
       ;; Relpos: Boi at the previous try or catch keyword in the try
       ;; statement.
       (comment-intro         . c-lineup-comment)
       ;; Relpos: None.
       (arglist-intro         . +)
       ;; Relpos: Boi at the open paren, or at the first non-ws after
       ;; the open paren of the surrounding sexp, whichever is later.
       (arglist-cont          . 0)
       ;; Relpos: At the first token after the open paren.
       (arglist-cont-nonempty . c-lineup-arglist)
       ;; Relpos: Boi at the open paren, or at the first non-ws after
       ;; the open paren of the surrounding sexp, whichever is later.
       (arglist-close         . +)
       ;; Relpos: Boi at the open paren, or at the first non-ws after
       ;; the open paren of the surrounding sexp, whichever is later.
       (stream-op             . c-lineup-streamop)
       ;; Relpos: Boi at the first stream op in the statement.
       (inclass               . +)
       ;; Relpos: At the class open brace if it's at boi, otherwise
       ;; boi at the class decl start.
       (cpp-macro             . [0])
       ;; Relpos: None.
       (cpp-macro-cont        . c-lineup-dont-change)
       ;; Relpos: At the macro start (always at boi).
       (friend                . 0)
       ;; Relpos: None.
       (objc-method-intro     . [0])
       ;; Relpos: Boi.
       (objc-method-args-cont . c-lineup-ObjC-method-args)
       ;; Relpos: At the method start (always at boi).
       (objc-method-call-cont . c-lineup-ObjC-method-call)
       ;; Relpos: At the open bracket.
       (extern-lang-open      . 0)
       ;; Relpos: Boi at the extern keyword.
       (extern-lang-close     . 0)
       ;; Relpos: Boi at the corresponding extern keyword.
       (inextern-lang         . +)
       ;; Relpos: At the extern block open brace if it's at boi,
       ;; otherwise boi at the extern keyword.
       (namespace-open        . 0)
       ;; Relpos: Boi at the namespace keyword.
       (namespace-close       . 0)
       ;; Relpos: Boi at the corresponding namespace keyword.
       (innamespace           . +)
       ;; Relpos: At the namespace block open brace if it's at boi,
       ;; otherwise boi at the namespace keyword.
       (template-args-cont    . (c-lineup-template-args +))
       ;; Relpos: Boi at the decl start.
       (inlambda              . c-lineup-inexpr-block)
       ;; Relpos: None.
       (lambda-intro-cont     . +)
       ;; Relpos: Boi at the lambda start.
       (inexpr-statement      . 0)
       ;; Relpos: None.
       (inexpr-class          . +)
       ;; Relpos: None.
       ))
(defcustom c-offsets-alist nil
  "Association list of syntactic element symbols and indentation offsets.
As described below, each cons cell in this list has the form:

    (SYNTACTIC-SYMBOL . OFFSET)

When a line is indented, CC Mode first determines the syntactic
context of it by generating a list of symbols called syntactic
elements.  This list can contain more than one syntactic element and
the global variable `c-syntactic-context' contains the context list
for the line being indented.  Each element in this list is actually a
cons cell of the syntactic symbol and a buffer position.  This buffer
position is called the relative indent point for the line.  Some
syntactic symbols may not have a relative indent point associated with
them.

After the syntactic context list for a line is generated, CC Mode
calculates the absolute indentation for the line by looking at each
syntactic element in the list.  It compares the syntactic element
against the SYNTACTIC-SYMBOL's in `c-offsets-alist'.  When it finds a
match, it adds the OFFSET to the column of the relative indent point.
The sum of this calculation for each element in the syntactic list is
the absolute offset for line being indented.

If the syntactic element does not match any in the `c-offsets-alist',
the element is ignored.

If OFFSET is nil, the syntactic element is ignored in the offset
calculation.

If OFFSET is an integer, it's added to the relative indent.

If OFFSET is one of the symbols `+', `-', `++', `--', `*', or `/', a
positive or negative multiple of `c-basic-offset' is added; 1, -1, 2,
-2, 0.5, and -0.5, respectively.

If OFFSET is a vector, it's first element, which must be an integer,
is used as an absolute indentation column.  This overrides all
relative offsets.  If there are several syntactic elements which
evaluates to absolute indentation columns, the first one takes
precedence.  You can see in which order CC Mode combines the syntactic
elements in a certain context by using \\[c-show-syntactic-information] on the line.

If OFFSET is a function, it's called with a single argument
containing the cons of the syntactic element symbol and the relative
indent point.  The return value from the function is then
reinterpreted as an OFFSET value.

If OFFSET is a list, it's recursively evaluated using the semantics
described above.  The first element of the list to return a non-nil
value succeeds.  If none of the elements returns a non-nil value, the
syntactic element is ignored.

`c-offsets-alist' is a style variable.  This means that the offsets on
this variable are normally taken from the style system in CC Mode
\(see `c-default-style' and `c-styles-alist').  However, any offsets
put explicitly on this list will override the style system when a CC
Mode buffer is initialized \(there is a variable
`c-old-style-variable-behavior' that changes this, though).

Here is the current list of valid syntactic element symbols:

 string                 -- Inside multi-line string.
 c                      -- Inside a multi-line C style block comment.
 defun-open             -- Brace that opens a function definition.
 defun-close            -- Brace that closes a function definition.
 defun-block-intro      -- The first line in a top-level defun.
 class-open             -- Brace that opens a class definition.
 class-close            -- Brace that closes a class definition.
 inline-open            -- Brace that opens an in-class inline method.
 inline-close           -- Brace that closes an in-class inline method.
 func-decl-cont         -- The region between a function definition's
                           argument list and the function opening brace
                           (excluding K&R argument declarations).  In C, you
                           cannot put anything but whitespace and comments
                           between them; in C++ and Java, throws declarations
                           and other things can appear in this context.
 knr-argdecl-intro      -- First line of a K&R C argument declaration.
 knr-argdecl            -- Subsequent lines in a K&R C argument declaration.
 topmost-intro          -- The first line in a topmost construct definition.
 topmost-intro-cont     -- Topmost definition continuation lines.
 member-init-intro      -- First line in a member initialization list.
 member-init-cont       -- Subsequent member initialization list lines.
 inher-intro            -- First line of a multiple inheritance list.
 inher-cont             -- Subsequent multiple inheritance lines.
 block-open             -- Statement block open brace.
 block-close            -- Statement block close brace.
 brace-list-open        -- Open brace of an enum or static array list.
 brace-list-close       -- Close brace of an enum or static array list.
 brace-list-intro       -- First line in an enum or static array list.
 brace-list-entry       -- Subsequent lines in an enum or static array list.
 brace-entry-open       -- Subsequent lines in an enum or static array
                           list that start with an open brace.
 statement              -- A C (or like) statement.
 statement-cont         -- A continuation of a C (or like) statement.
 statement-block-intro  -- The first line in a new statement block.
 statement-case-intro   -- The first line in a case \"block\".
 statement-case-open    -- The first line in a case block starting with brace.
 substatement           -- The first line after an if/while/for/do/else.
 substatement-open      -- The brace that opens a substatement block.
 case-label             -- A `case' or `default' label.
 access-label           -- C++ private/protected/public access label.
 label                  -- Any ordinary label.
 do-while-closure       -- The `while' that ends a do/while construct.
 else-clause            -- The `else' of an if/else construct.
 catch-clause           -- The `catch' or `finally' of a try/catch construct.
 comment-intro          -- A line containing only a comment introduction.
 arglist-intro          -- The first line in an argument list.
 arglist-cont           -- Subsequent argument list lines when no
                           arguments follow on the same line as the
                           arglist opening paren.
 arglist-cont-nonempty  -- Subsequent argument list lines when at
                           least one argument follows on the same
                           line as the arglist opening paren.
 arglist-close          -- The solo close paren of an argument list.
 stream-op              -- Lines continuing a stream operator construct.
 inclass                -- The construct is nested inside a class definition.
                           Used together with e.g. `topmost-intro'.
 cpp-macro              -- The start of a C preprocessor macro definition.
 cpp-macro-cont         -- Subsequent lines in a multi-line C preprocessor
                           macro definition.
 friend                 -- A C++ friend declaration.
 objc-method-intro      -- The first line of an Objective-C method definition.
 objc-method-args-cont  -- Lines continuing an Objective-C method definition.
 objc-method-call-cont  -- Lines continuing an Objective-C method call.
 extern-lang-open       -- Brace that opens an external language block.
 extern-lang-close      -- Brace that closes an external language block.
 inextern-lang          -- Analogous to the `inclass' syntactic symbol,
                           but used inside extern constructs.
 namespace-open         -- Brace that opens a C++ namespace block.
 namespace-close        -- Brace that closes a C++ namespace block.
 innamespace            -- Analogous to the `inextern-lang' syntactic
                           symbol, but used inside C++ namespace constructs.
 template-args-cont     -- C++ template argument list continuations.
 inlambda               -- In the header or body of a lambda function.
 lambda-intro-cont      -- Continuation of the header of a lambda function.
 inexpr-statement       -- The statement is inside an expression.
 inexpr-class           -- The class is inside an expression.  Used e.g. for
                           Java anonymous classes."
  :type
  `(set :format "%{%t%}:
 Override style setting
 |  Syntax                     Offset
%v"
	,@(mapcar
	   (lambda (elt)
	     `(cons :format "%v"
		    :value ,elt
		    (c-const-symbol :format "%v: "
				    :size 25)
		    (sexp :format "%v"
			  :validate
			  (lambda (widget)
			    (unless (c-valid-offset (widget-value widget))
			      (widget-put widget :error "Invalid offset")
			      widget)))))
	   (get 'c-offsets-alist 'c-stylevar-fallback)))
  :group 'c)

(defcustom c-style-variables-are-local-p t
  "*Whether style variables should be buffer local by default.
If non-nil, then all indentation style related variables will be made
buffer local by default.  If nil, they will remain global.  Variables
are made buffer local when this file is loaded, and once buffer
localized, they cannot be made global again.

The list of variables to buffer localize are:
    c-offsets-alist
    c-basic-offset
    c-comment-only-line-offset
    c-block-comment-prefix
    c-comment-prefix-regexp
    c-cleanup-list
    c-hanging-braces-alist
    c-hanging-colons-alist
    c-hanging-semi&comma-criteria
    c-backslash-column
    c-label-minimum-indentation
    c-special-indent-hook
    c-indentation-style"
  :type 'boolean
  :group 'c)

(defcustom c-mode-hook nil
  "*Hook called by `c-mode'."
  :type 'hook
  :group 'c)

(defcustom c++-mode-hook nil
  "*Hook called by `c++-mode'."
  :type 'hook
  :group 'c)

(defcustom objc-mode-hook nil
  "*Hook called by `objc-mode'."
  :type 'hook
  :group 'c)

(defcustom java-mode-hook nil
  "*Hook called by `java-mode'."
  :type 'hook
  :group 'c)

(defcustom idl-mode-hook nil
  "*Hook called by `idl-mode'."
  :type 'hook
  :group 'c)

(defcustom pike-mode-hook nil
  "*Hook called by `pike-mode'."
  :type 'hook
  :group 'c)

(defcustom c-mode-common-hook nil
  "*Hook called by all CC Mode modes for common initializations."
  :type '(hook :format "%{CC Mode Common Hook%}:\n%v")
  :group 'c)

(defcustom c-initialization-hook nil
  "*Hook called when the CC Mode package gets initialized.
This hook is only run once per Emacs session and can be used as a
`load-hook' or in place of using `eval-after-load'."
  :type 'hook
  :group 'c)

(defcustom c-enable-xemacs-performance-kludge-p nil
  "*Enables a XEmacs only hack that may improve speed for some coding styles.
For styles that hang top-level opening braces (as is common with JDK
Java coding styles) this can improve performance between 3 and 60
times for core indentation functions (e.g. `c-parse-state').  For
styles that conform to the Emacs recommendation of putting these
braces in column zero, this can degrade performance about as much.
This variable only has effect in XEmacs.")

(defcustom c-old-style-variable-behavior nil
  "*Enables the old style variable behavior when non-nil.

Normally the values of the style variables will override the style
settings specified by the variables `c-default-style' and
`c-styles-alist'.  However, in CC Mode 5.25 and earlier, it was the
other way around, meaning that changes made to the style variables
from e.g. Customize would not take effect unless special precautions
were taken.  That was confusing, especially for novice users.

It's believed that despite this change, the new behavior will still
produce the same results for most old CC Mode configurations, since
all style variables are per default set in a special non-override
state.  Set this variable only if your configuration has stopped
working due to this change.")



;; Non-customizable variables, still part of the interface to CC Mode
(defvar c-file-style nil
  "Variable interface for setting style via File Local Variables.
In a file's Local Variable section, you can set this variable to a
string suitable for `c-set-style'.  When the file is visited, CC Mode
will set the style of the file to this value automatically.

Note that file style settings are applied before file offset settings
as designated in the variable `c-file-offsets'.")
(make-variable-buffer-local 'c-file-style)

(defvar c-file-offsets nil
  "Variable interface for setting offsets via File Local Variables.
In a file's Local Variable section, you can set this variable to an
association list similar to the values allowed in `c-offsets-alist'.
When the file is visited, CC Mode will institute these offset settings
automatically.

Note that file offset settings are applied after file style settings
as designated in the variable `c-file-style'.")
(make-variable-buffer-local 'c-file-offsets)

(defvar c-syntactic-context nil
  "Variable containing syntactic analysis list during indentation.
This is always bound dynamically.  It should never be set statically
(e.g. with `setq').")

(defvar c-indentation-style nil
  "Name of the currently installed style.
Don't change this directly; call `c-set-style' instead.")

(defvar c-current-comment-prefix nil
  "The current comment prefix regexp.
Set from `c-comment-prefix-regexp' at mode initialization.")
(make-variable-buffer-local 'c-current-comment-prefix)


;; Figure out what features this Emacs has
;;;###autoload
(defconst c-emacs-features
  (let ((infodock-p (boundp 'infodock-version))
	(comments
	 ;; XEmacs 19 and beyond use 8-bit modify-syntax-entry flags.
	 ;; Emacs 19 uses a 1-bit flag.  We will have to set up our
	 ;; syntax tables differently to handle this.
	 (let ((table (copy-syntax-table))
	       entry)
	   (modify-syntax-entry ?a ". 12345678" table)
	   (cond
	    ;; XEmacs 19, and beyond Emacs 19.34
	    ((arrayp table)
	     (setq entry (aref table ?a))
	     ;; In Emacs, table entries are cons cells
	     (if (consp entry) (setq entry (car entry))))
	    ;; XEmacs 20
	    ((fboundp 'get-char-table) (setq entry (get-char-table ?a table)))
	    ;; before and including Emacs 19.34
	    ((and (fboundp 'char-table-p)
		  (char-table-p table))
	     (setq entry (car (char-table-range table [?a]))))
	    ;; incompatible
	    (t (error "CC Mode is incompatible with this version of Emacs")))
	   (if (= (logand (lsh entry -16) 255) 255)
	       '8-bit
	     '1-bit))))
    (if infodock-p
	(list comments 'infodock)
      (list comments)))
  "A list of features extant in the Emacs you are using.
There are many flavors of Emacs out there, each with different
features supporting those needed by CC Mode.  Here's the current
supported list, along with the values for this variable:

 XEmacs 19, 20, 21:          (8-bit)
 Emacs 19, 20:               (1-bit)

Infodock (based on XEmacs) has an additional symbol on this list:
`infodock'.")


(cc-provide 'cc-vars)

;;; cc-vars.el ends here
