;;; cc-vars.el --- user customization variables for CC Mode

;; Copyright (C) 1985,87,92,93,94,95,96,97 Free Software Foundation, Inc.

;; Authors:    1992-1997 Barry A. Warsaw
;;             1987 Dave Detlefs and Stewart Clamen
;;             1985 Richard M. Stallman
;; Maintainer: cc-mode-help@python.org
;; Created:    22-Apr-1997 (split from cc-mode.el)
;; Version:    5.15
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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'custom)


(defcustom c-strict-syntax-p nil
  "*If non-nil, all syntactic symbols must be found in `c-offsets-alist'.
If the syntactic symbol for a particular line does not match a symbol
in the offsets alist, an error is generated, otherwise no error is
reported and the syntactic symbol is ignored."
  :type 'boolean
  :group 'c)

(defcustom c-echo-syntactic-information-p nil
  "*If non-nil, syntactic info is echoed when the line is indented."
  :type 'boolean
  :group 'c)

(defcustom c-basic-offset 4
  "*Amount of basic offset used by + and - symbols in `c-offsets-alist'."
  :type 'integer
  :group 'c)

(defcustom c-tab-always-indent t
  "*Controls the operation of the TAB key.
If t, hitting TAB always just indents the current line.  If nil,
hitting TAB indents the current line if point is at the left margin or
in the line's indentation, otherwise it insert a `real' tab character
\(see note\).  If other than nil or t, then tab is inserted only
within literals -- defined as comments and strings -- and inside
preprocessor directives, but line is always reindented.

Note: The value of `indent-tabs-mode' will determine whether a real
tab character will be inserted, or the equivalent number of space.
When inserting a tab, actually the function stored in the variable
`c-insert-tab-function' is called.

Note: indentation of lines containing only comments is also controlled
by the `c-comment-only-line-offset' variable."
  :type '(radio
	  :extra-offset 8
	  :format "%{C Tab Always Indent%}:\n   The TAB key:\n%v"
	  (const :tag "always indents, never inserts TAB" t)
	  (const :tag "indents in left margin, otherwise inserts TAB" nil)
	  (const :tag "inserts TAB in literals, otherwise indent" other))
  :group 'c)

(defcustom c-insert-tab-function 'insert-tab
  "*Function used when inserting a tab for \\[TAB].
Only used when `c-tab-always-indent' indicates a `real' tab character
should be inserted.  Value must be a function taking no arguments."
  :type 'function
  :group 'c)

(defcustom c-comment-only-line-offset 0
  "*Extra offset for line which contains only the start of a comment.
Can contain an integer or a cons cell of the form:

 (NON-ANCHORED-OFFSET . ANCHORED-OFFSET)

Where NON-ANCHORED-OFFSET is the amount of offset given to
non-column-zero anchored comment-only lines, and ANCHORED-OFFSET is
the amount of offset to give column-zero anchored comment-only lines.
Just an integer as value is equivalent to (<val> . -1000)."
  :type '(choice (integer :tag "Non-anchored offset")
		 (cons :tag "Non-anchored & anchored offset"
		       :value (0 . 0)
		       :extra-offset 8
		       (integer :tag "Non-anchored offset")
		       (integer :tag "Anchored offset")))
  :group 'c)

(defcustom c-indent-comments-syntactically-p nil
  "*Specifies how comment-only lines should be indented.
When this variable is non-nil, comment-only lines are indented
according to syntactic analysis via `c-offsets-alist', even when
\\[indent-for-comment] is used."
  :type 'boolean
  :group 'c)

(defcustom c-comment-continuation-stars "* "
  "*Specifies the leader of continued block comments.
You should set this variable to the literal string that gets inserted
at the front of continued block style comment lines.  This should
either be the empty string, or some number of stars followed by a
single space.  Note that for line style comments, this variable is not
used."
  :type '(choice (const :tag "Use old semantics" nil)
		 string)
  :group 'c)

(defcustom c-cleanup-list '(scope-operator)
  "*List of various C/C++/ObjC constructs to \"clean up\".
These clean ups only take place when the auto-newline feature is
turned on, as evidenced by the `/a' or `/ah' appearing next to the
mode name.  Valid symbols are:

 brace-else-brace    -- cleans up `} else {' constructs by placing entire
                        construct on a single line.  This clean up
                        only takes place when there is nothing but
                        white space between the braces and the `else'.
                        Clean up occurs when the open-brace after the
                        `else' is typed.
 brace-elseif-brace  -- similar to brace-else-brace, but cleans up
                        `} else if {' constructs.
 empty-defun-braces  -- cleans up empty defun braces by placing the
                        braces on the same line.  Clean up occurs when
			the defun closing brace is typed.
 defun-close-semi    -- cleans up the terminating semi-colon on defuns
			by placing the semi-colon on the same line as
			the closing brace.  Clean up occurs when the
			semi-colon is typed.
 list-close-comma    -- cleans up commas following braces in array
                        and aggregate initializers.  Clean up occurs
			when the comma is typed.
 scope-operator      -- cleans up double colons which may designate
			a C++ scope operator split across multiple
			lines. Note that certain C++ constructs can
			generate ambiguous situations.  This clean up
			only takes place when there is nothing but
			whitespace between colons. Clean up occurs
			when the second colon is typed."
  :type '(set
	  :extra-offset 8
	  (const :tag "Put `} else {' on one line" brace-else-brace)
	  (const :tag "Put `} else if {' on one line" brace-elseif-brace)
	  (const :tag "Put empty defun braces on one line" empty-defun-braces)
	  (const :tag "Put `},' in aggregates on one line" list-close-comma)
	  (const :tag "Put C++ style `::' on one line" scope-operator))
  :group 'c)

(defcustom c-hanging-braces-alist '((brace-list-open)
				    (substatement-open after)
				    (block-close . c-snug-do-while)
				    (extern-lang-open after)
				    )
  "*Controls the insertion of newlines before and after braces.
This variable contains an association list with elements of the
following form: (SYNTACTIC-SYMBOL . ACTION).

When a brace (either opening or closing) is inserted, the syntactic
context it defines is looked up in this list, and if found, the
associated ACTION is used to determine where newlines are inserted.
If the context is not found, the default is to insert a newline both
before and after the brace.

SYNTACTIC-SYMBOL can be any of: defun-open, defun-close, class-open,
class-close, inline-open, inline-close, block-open, block-close,
substatement-open, statement-case-open, extern-lang-open,
extern-lang-close, brace-list-open, brace-list-close,
brace-list-intro, or brace-list-entry. See `c-offsets-alist' for
details.

ACTION can be either a function symbol or a list containing any
combination of the symbols `before' or `after'.  If the list is empty,
no newlines are inserted either before or after the brace.

When ACTION is a function symbol, the function is called with a two
arguments: the syntactic symbol for the brace and the buffer position
at which the brace was inserted.  The function must return a list as
described in the preceding paragraph.  Note that during the call to
the function, the variable `c-syntactic-context' is set to the entire
syntactic context for the brace line."
  :type '(repeat
	  (cons :format "%v"
		(choice :tag "Syntax"
			(const defun-open) (const defun-close)
			(const class-open) (const class-close)
			(const inline-open) (const inline-close)
			(const block-open) (const block-close)
			(const substatement-open) (const statement-case-open)
			(const extern-lang-open) (const extern-lang-close)
			(const brace-list-open) (const brace-list-close)
			(const brace-list-intro) (const brace-list-entry))
		(choice :tag "Action"
			(set :format "Insert a newline %v"
			     :extra-offset 38
			     (const :tag "before brace" before)
			     (const :tag "after brace" after))
			(function :format "Run function %v" :value c-)
			)))
  :group 'c)

(defcustom c-hanging-colons-alist nil
  "*Controls the insertion of newlines before and after certain colons.
This variable contains an association list with elements of the
following form: (SYNTACTIC-SYMBOL . ACTION).

SYNTACTIC-SYMBOL can be any of: case-label, label, access-label,
member-init-intro, or inher-intro.

See the variable `c-hanging-braces-alist' for the semantics of this
variable.  Note however that making ACTION a function symbol is
currently not supported for this variable."
  :type '(repeat
	  (cons :format "%v"
		(choice :tag "Syntax"
			(const case-label) (const label) (const access-label)
			(const member-init-intro) (const inher-intro))
		(set :tag "Action"
		     :format "%t: %v"
		     :extra-offset 8
		     (const before) (const after))))
  :group 'c)

(defcustom c-hanging-semi&comma-criteria '(c-semi&comma-inside-parenlist)
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

(defcustom c-hanging-comment-ender-p t
  "*Controls what \\[fill-paragraph] does to C block comment enders.
When set to nil, C block comment enders are left on their own line.
When set to t, block comment enders will be placed at the end of the
previous line (i.e. they `hang' on that line)."
  :type 'boolean
  :group 'c)

(defcustom c-hanging-comment-starter-p t
  "*Controls what \\[fill-paragraph] does to C block comment starters.
When set to nil, C block comment starters are left on their own line.
When set to t, text that follows a block comment starter will be
placed on the same line as the block comment starter (i.e. the text
`hangs' on that line)."
  :type 'boolean
  :group 'c)

(defcustom c-backslash-column 48
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

(defcustom c-label-minimum-indentation 1
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
that many seconds.  Set to nil to inhibit updating.  This is only
useful for Emacs 19."
  :type 'integer
  :group 'c)

(defcustom c-site-default-style "gnu"
  "Default style for your site.
To change the default style at your site, you can set this variable to
any style defined in `c-style-alist'.  However, if CC Mode is usually
loaded into your Emacs at compile time, you will need to set this
variable in the `site-init.el' file before CC Mode is loaded, then
re-dump Emacs."
  :type 'string
  :group 'c)

(defcustom c-style-variables-are-local-p nil
  "*Whether style variables should be buffer local by default.
If non-nil, then all indentation style related variables will be made
buffer local by default.  If nil, they will remain global.  Variables
are made buffer local when this file is loaded, and once buffer
localized, they cannot be made global again.

The list of variables to buffer localize are:
    c-offsets-alist
    c-basic-offset
    c-file-style
    c-file-offsets
    c-comment-only-line-offset
    c-cleanup-list
    c-hanging-braces-alist
    c-hanging-colons-alist
    c-hanging-comment-starter-p
    c-hanging-comment-ender-p
    c-backslash-column
    c-label-minimum-indentation
    c-special-indent-hook
    c-indentation-style"
  :type 'boolean
  :group 'c)

(defcustom c-mode-hook nil
  "*Hook called by `c-mode'."
  :type '(hook :format "%{C Mode Hook%}:\n%v")
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



;; Non-customizable variables, still part of the interface to CC Mode
(defvar c-file-style nil
  "Variable interface for setting style via File Local Variables.
In a file's Local Variable section, you can set this variable to a
string suitable for `c-set-style'.  When the file is visited, CC Mode
will set the style of the file to this value automatically.

Note that file style settings are applied before file offset settings
as designated in the variable `c-file-offsets'.")

(defvar c-file-offsets nil
  "Variable interface for setting offsets via File Local Variables.
In a file's Local Variable section, you can set this variable to an
association list similar to the values allowed in `c-offsets-alist'.
When the file is visited, CC Mode will institute these offset settings
automatically.

Note that file offset settings are applied after file style settings
as designated in the variable `c-file-style'.")

(defvar c-syntactic-context nil
  "Variable containing syntactic analysis list during indentation.")

(defvar c-indentation-style c-site-default-style
  "Name of style installed in the current buffer.")

(defvar c-buffer-is-cc-mode nil
  "Non-nil for all buffers with a `major-mode' derived from CC Mode.
Otherwise, this variable is nil.  I.e. this variable is non-nil for
`c-mode', `c++-mode', `objc-mode', `java-mode', `idl-mode', and any
other non-CC Mode mode that calls `c-initialize-cc-mode'
\(e.g. `awk-mode').")
(make-variable-buffer-local 'c-buffer-is-cc-mode)


(provide 'cc-vars)
;;; cc-vars.el ends here
