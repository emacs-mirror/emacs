;;; vhdl-mode.el --- major mode for editing VHDL code

;; Copyright (C) 1992, 93, 94, 95, 96, 1997 Free Software Foundation, Inc.

;; Authors:       Reto Zimmermann             <mailto:Reto.Zimmermann@iaeth.ch>
;;                                          <http://www.iis.ee.ethz.ch/~zimmi/>
;;                Rodney J. Whitby               <mailto:rwhitby@geocities.com>
;;                          <http://www.geocities.com/SiliconValley/Park/8287/>
;; Maintainer:    vhdl-mode@geocities.com
;; Maintainers' Version:       3.19
;; Keywords:      languages vhdl

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

;; ############################################################################
;;; Commentary:
;; ############################################################################

;; This package provides an Emacs major mode for editing VHDL code.
;; It includes the following features:

;;   - Highlighting of VHDL syntax
;;   - Indentation based on versatile syntax analysis
;;   - Template insertion (electrification) for most VHDL constructs
;;   - Insertion of customizable VHDL file headers
;;   - Word completion (dynamic abbreviations)
;;   - Menu containing all VHDL Mode commands
;;   - Index menu (jump index to main units and blocks in a file)
;;   - Source file menu (menu of all source files in current directory)
;;   - Source file compilation (syntax analysis)
;;   - Postscript printing with fontification
;;   - Lower and upper case keywords
;;   - Hiding blocks of code
;;   - Alignment functions
;;   - Easy customization
;;   - Works under Emacs and XEmacs

;; ############################################################################
;; Usage
;; ############################################################################

;; see below (comment in vhdl-mode function) or type `C-c C-h' in Emacs.

;; ############################################################################
;; Emacs Versions
;; ############################################################################

;; - Emacs 20
;; - XEmacs 19.15
;; - This version does not support Emacs 19 (use VHDL Mode 3.10 instead)


;; ############################################################################
;; Acknowledgements
;; ############################################################################

;; Electrification ideas by Bob Pack <rlpst@cislabs.pitt.edu>
;; and Steve Grout

;; Fontification approach suggested by Ken Wood <ken@eda.com.au>
;; Source file menu suggested by Michael Laajanen <mila@enea.se>
;; Ideas about alignment from John Wiegley <johnw@borland.com>

;; Many thanks to all the users who sent me bug reports and enhancement
;; requests.
;; Special thanks go to Dan Nicolaescu <done@ece.arizona.edu> for reviewing
;; the code and for his valuable hints.

;;; Code:

;; ############################################################################
;; User definable variables
;; ############################################################################

;; ############################################################################
;; Variables for customization

(defgroup vhdl nil
  "Customizations for VHDL Mode."
  :prefix "vhdl-"
  :group 'languages
  :version "20.3")


(defgroup vhdl-mode nil
  "Customizations for modes."
  :group 'vhdl)

(defcustom vhdl-electric-mode t
  "*If non-nil, electrification (automatic template generation) is enabled.
If nil, template generators can still be invoked through key bindings
and menu. Can be toggled by `\\[vhdl-electric-mode]'."
  :type 'boolean
  :group 'vhdl-mode)

(defcustom vhdl-stutter-mode t
  "*If non-nil, stuttering is enabled.
Can be toggled by `\\[vhdl-stutter-mode]'."
  :type 'boolean
  :group 'vhdl-mode)

(defcustom vhdl-indent-tabs-mode t
  "*Indentation can insert tabs if this is non-nil.
Overrides local variable `indent-tabs-mode'."
  :type 'boolean
  :group 'vhdl-mode)


(defgroup vhdl-compile nil
  "Customizations for compilation."
  :group 'vhdl)

(defcustom vhdl-compiler 'v-system
  "*VHDL compiler to be used for syntax analysis.
  cadence       Cadence Design Systems (`cv -file')
  ikos          Ikos Voyager (`analyze')
  quickhdl      QuickHDL, Mentor Graphics (`qvhcom')
  synopsys      Synopsys, VHDL Analyzer (`vhdlan')
  vantage       Vantage Analysis Systems (`analyze -libfile vsslib.ini -src')
  viewlogic     Viewlogic (`analyze -libfile vsslib.ini -src')
  v-system      V-System, Model Technology (`vcom')
For incorporation of additional compilers, please send me their command syntax
and some example error messages."
  :type '(choice
	  (const cadence)
	  (const ikos)
	  (const quickhdl)
	  (const synopsys)
	  (const vantage)
	  (const viewlogic)
	  (const v-system)
	  )
  :group 'vhdl-compile)

(defcustom vhdl-compiler-options ""
  "*Options to be added to the compile command."
  :type 'string
  :group 'vhdl-compile)


(defgroup vhdl-style nil
  "Customizations for code styles."
  :group 'vhdl)

(defcustom vhdl-basic-offset 4
  "*Amount of basic offset used for indentation.
This value is used by + and - symbols in `vhdl-offsets-alist'."
  :type 'integer
  :group 'vhdl-style)


(defgroup vhdl-word-case nil
  "Customizations for case of VHDL words."
  :group 'vhdl-style)

(defcustom vhdl-upper-case-keywords nil
  "*If non-nil, keywords are converted to upper case
when typed or by the fix case functions."
  :type 'boolean
  :group 'vhdl-word-case)

(defcustom vhdl-upper-case-types nil
  "*If non-nil, standardized types are converted to upper case
by the fix case functions."
  :type 'boolean
  :group 'vhdl-word-case)

(defcustom vhdl-upper-case-attributes nil
  "*If non-nil, standardized attributes are converted to upper case
by the fix case functions."
  :type 'boolean
  :group 'vhdl-word-case)

(defcustom vhdl-upper-case-enum-values nil
  "*If non-nil, standardized enumeration values are converted to upper case
by the fix case functions."
  :type 'boolean
  :group 'vhdl-word-case)


(defgroup vhdl-electric nil
  "Customizations for comments."
  :group 'vhdl)

(defcustom vhdl-auto-align nil
  "*If non-nil, some templates are automatically aligned after generation."
  :type 'boolean
  :group 'vhdl-electric)

(defcustom vhdl-additional-empty-lines t
  "*If non-nil, additional empty lines are inserted in some templates.
This improves readability of code."
  :type 'boolean
  :group 'vhdl-electric)

(defcustom vhdl-argument-list-indent t
  "*If non-nil, argument lists are indented relative to the opening paren.
Normal indentation is applied otherwise."
  :type 'boolean
  :group 'vhdl-electric)

(defcustom vhdl-conditions-in-parenthesis nil
  "*If non-nil, parenthesis are placed around condition expressions."
  :type 'boolean
  :group 'vhdl-electric)

(defcustom vhdl-date-format 'scientific
  "*Specifies date format to be used in header.
Date formats are:
  american (09/17/1997)
  european (17.09.1997)
  scientific (1997/09/17)"
  :type '(choice (const american)
                 (const european)
                 (const scientific))
  :group 'vhdl-electric)

(defcustom vhdl-header-file nil
  "*Pathname/filename of the file to be inserted as header.
If the header contains RCS keywords, they may be written as <RCS>Keyword<RCS>
if the header needs to be version controlled.

The following keywords for template generation are supported:
  <filename>   : replaced by the name of the buffer
  <author>     : replaced by the user name and email address
  <date>       : replaced by the current date
  <... string> : replaced by a prompted string (... is the prompt word)
  <cursor>     : final cursor position

Example:
  -----------------------------------------
  -- Title       : <title string>
  -- File        : <filename>
  -- Author      : <author>
  -- Created     : <date>
  -- Description : <cursor>
  -----------------------------------------"
  :type '(choice (const nil) string)
  :group 'vhdl-electric)

(defcustom vhdl-modify-date-prefix-string "-- Last modified : "
  "*Prefix string of modification date in VHDL file header.
If actualization of the modification date is called (menu, `\\[vhdl-modify]'),
this string is searched and the rest of the line replaced by the current date."
  :type 'string
  :group 'vhdl-electric)

(defcustom vhdl-zero-string "'0'"
  "*String to use for a logic zero."
  :type 'string
  :group 'vhdl-electric)

(defcustom vhdl-one-string "'1'"
  "*String to use for a logic one."
  :type 'string
  :group 'vhdl-electric)


(defgroup vhdl-comment nil
  "Customizations for comments."
  :group 'vhdl-electric)

(defcustom vhdl-self-insert-comments t
  "*If non-nil, variables templates automatically insert help comments."
  :type 'boolean
  :group 'vhdl-comment)

(defcustom vhdl-prompt-for-comments t
  "*If non-nil, various templates prompt for user definable comments."
  :type 'boolean
  :group 'vhdl-comment)

(defcustom vhdl-comment-column 40
  "*Column to indent right-margin comments to.
Overrides local variable `comment-column'."
  :type 'integer
  :group 'vhdl-comment)

(defcustom vhdl-end-comment-column 79
  "*End of comment column."
  :type 'integer
  :group 'vhdl-comment)

(defvar end-comment-column 79
  "*End of comment column.")


(defgroup vhdl-highlight nil
  "Customizations for highlighting."
  :group 'vhdl)

(defcustom vhdl-highlight-names t
  "*If non-nil, unit names, subprogram names, and labels are highlighted."
  :type 'boolean
  :group 'vhdl-highlight)

(defcustom vhdl-highlight-keywords t
  "*If non-nil, VHDL keywords and other predefined words are highlighted.
That is, keywords, predefined types, predefined attributes, and predefined
enumeration values are highlighted."
  :type 'boolean
  :group 'vhdl-highlight)

(defcustom vhdl-highlight-signals nil
  "*If non-nil, signals of different classes are highlighted using colors.
Signal classes are: clock, reset, status/control, data, and test."
  :type 'boolean
  :group 'vhdl-highlight)

(defcustom vhdl-highlight-case-sensitive nil
  "*If non-nil, case is considered for highlighting.
Possible trade-off:
  non-nil  also upper-case VHDL words are highlighted, but case of signal names
           is not considered (may lead to highlighting of unwanted words),
  nil      only lower-case VHDL words are highlighted, but case of signal names
           is considered.
Overrides local variable `font-lock-keywords-case-fold-search'."
  :type 'boolean
  :group 'vhdl-highlight)

(defcustom vhdl-customize-colors nil
  "*If non-nil, colors are customized to go with the additional signal colors.
NOTE: this alters the behavior of Emacs for *all* modes,
so it is not enabled by default."
  :type 'boolean
  :group 'vhdl-highlight)

(defcustom vhdl-customize-faces t
  "*If non-nil, faces are customized to work better with VHDL Mode.
This variable comes only into effect if no colors are used
for highlighting or printing (i.e. variable `ps-print-color-p' is nil).

NOTE: this alters the behavior of Emacs for *all* modes,
so it is not enabled by default."
  :type 'boolean
  :group 'vhdl-highlight)


(defgroup vhdl-signal-syntax nil
  "Customizations of signal syntax for highlighting."
  :group 'vhdl-highlight)

(defcustom vhdl-signal-syntax-doc-string "
Must be of the form \"\\ \<\\\(...\\\)\\\>\", where ... specifies the actual syntax.
   (delete this space ^ , it's only a workaround to get this doc string.)
The basic regexp elements are:
  [A-Z]    any upper case letter
  [A-Za-z] any letter
  [0-9]    any digit
  \\w       any letter or digit (corresponds to [A-Za-z0-9])
  [XY]     letter \"X\" or \"Y\"
  [^XY]    neither letter \"X\" nor \"Y\"
  x        letter \"x\"
  *        postfix operator for matching previous regexp element any times
  +        postfix operator for matching previous regexp element at least once
  ?        postfix operator for matching previous regexp element at most once"
  "Common document string used for the custom variables below. Must be
defined as custom variable due to a bug in XEmacs.")

(defcustom vhdl-clock-signal-syntax "\\<\\([A-Z]\\w*xC\\w*\\)\\>"
  (concat
   "*Regular expression (regexp) for syntax of clock signals."
   vhdl-signal-syntax-doc-string)
  :type 'regexp
  :group 'vhdl-signal-syntax)

(defcustom vhdl-reset-signal-syntax "\\<\\([A-Z]\\w*xR\\w*\\)\\>"
  (concat
   "*Regular expression (regexp) for syntax of (asynchronous) reset signals."
   vhdl-signal-syntax-doc-string)
  :type 'regexp
  :group 'vhdl-signal-syntax)

(defcustom vhdl-control-signal-syntax "\\<\\([A-Z]\\w*x[IS]\\w*\\)\\>"
  (concat
   "*Regular expression (regexp) for syntax of status/control signals."
   vhdl-signal-syntax-doc-string)
  :type 'regexp
  :group 'vhdl-signal-syntax)

(defcustom vhdl-data-signal-syntax "\\<\\([A-Z]\\w*xD\\w*\\)\\>"
  (concat
   "*Regular expression (regexp) for syntax of data signals."
   vhdl-signal-syntax-doc-string)
  :type 'regexp
  :group 'vhdl-signal-syntax)

(defcustom vhdl-test-signal-syntax "\\<\\([A-Z]\\w*xT\\w*\\)\\>"
  (concat
   "*Regular expression (regexp) for syntax of test signals."
   vhdl-signal-syntax-doc-string)
  :type 'regexp
  :group 'vhdl-signal-syntax)


(defgroup vhdl-menu nil
  "Customizations for menues."
  :group 'vhdl)

(defcustom vhdl-source-file-menu t
  "*If non-nil, a menu of all source files in the current directory is created."
  :type 'boolean
  :group 'vhdl-menu)

(defcustom vhdl-index-menu t
  "*If non-nil, an index menu for the current source file is created."
  :type 'boolean
  :group 'vhdl-menu)

(defcustom vhdl-hideshow-menu (not (string-match "XEmacs" emacs-version))
  "*If non-nil, hideshow menu and functionality is added.
Hideshow allows hiding code of VHDL processes and blocks.
(Does not work under XEmacs.)"
  :type 'boolean
  :group 'vhdl-menu)


(defgroup vhdl-print nil
  "Customizations for printing."
  :group 'vhdl)

(defcustom vhdl-print-two-column t
  "*If non-nil, code is printed in two columns and landscape format."
  :type 'boolean
  :group 'vhdl-print)


(defgroup vhdl-misc nil
  "Miscellaneous customizations."
  :group 'vhdl)

(defcustom vhdl-intelligent-tab t
  "*If non-nil, `TAB' does indentation, word completion, and tab insertion.
That is, if preceeding character is part of a word then complete word,
else if not at beginning of line then insert tab,
else if last command was a `TAB' or `RET' then dedent one step,
else indent current line (i.e. `TAB' is bound to `vhdl-tab').
If nil, TAB always indents current line (i.e. `TAB' is bound to
`vhdl-indent-line')."
  :type 'boolean
  :group 'vhdl-misc)

(defcustom vhdl-template-key-binding-prefix "\C-t"
  "*`C-c' plus this key gives the key binding prefix for all VHDL templates.
Default key binding prefix for templates is `C-c C-t' (example: architecture
`C-c C-t a'). If you have no own `C-c LETTER' bindings, you can shorten the
template key binding prefix to `C-c' (example: architecture `C-c a') by
assigning the empty character (\"\") to this variable. The syntax to enter
control keys is \"\\C-t\"."
  :type 'sexp
  :group 'vhdl-misc)

(defcustom vhdl-word-completion-in-minibuffer t
  "*If non-nil, word completion works in minibuffer (for template prompts)."
  :type 'boolean
  :group 'vhdl-misc)

(defcustom vhdl-underscore-is-part-of-word nil
  "*If non-nil, the underscore character `_' is considered as part of word.
An identifier containing underscores is then treated as a single word in
select and move operations. All parts of an identifier separated by underscore
are treated as single words otherwise."
  :type 'boolean
  :group 'vhdl-misc)

;; ############################################################################
;; Other variables

(defvar vhdl-inhibit-startup-warnings-p nil
  "*If non-nil, inhibits start up compatibility warnings.")

(defvar vhdl-strict-syntax-p nil
  "*If non-nil, all syntactic symbols must be found in `vhdl-offsets-alist'.
If the syntactic symbol for a particular line does not match a symbol
in the offsets alist, an error is generated, otherwise no error is
reported and the syntactic symbol is ignored.")

(defvar vhdl-echo-syntactic-information-p nil
  "*If non-nil, syntactic info is echoed when the line is indented.")

(defconst vhdl-offsets-alist-default
  '((string                . -1000)
    (block-open            . 0)
    (block-close           . 0)
    (statement             . 0)
    (statement-cont        . vhdl-lineup-statement-cont)
    (statement-block-intro . +)
    (statement-case-intro  . +)
    (case-alternative      . +)
    (comment               . vhdl-lineup-comment)
    (arglist-intro         . +)
    (arglist-cont          . 0)
    (arglist-cont-nonempty . vhdl-lineup-arglist)
    (arglist-close         . vhdl-lineup-arglist)
    (entity                . 0)
    (configuration         . 0)
    (package               . 0)
    (architecture          . 0)
    (package-body          . 0)
    )
  "Default settings for offsets of syntactic elements.
Do not change this constant!  See the variable `vhdl-offsets-alist' for
more information.")

(defvar vhdl-offsets-alist (copy-alist vhdl-offsets-alist-default)
  "*Association list of syntactic element symbols and indentation offsets.
As described below, each cons cell in this list has the form:

    (SYNTACTIC-SYMBOL . OFFSET)

When a line is indented, vhdl-mode first determines the syntactic
context of the line by generating a list of symbols called syntactic
elements.  This list can contain more than one syntactic element and
the global variable `vhdl-syntactic-context' contains the context list
for the line being indented.  Each element in this list is actually a
cons cell of the syntactic symbol and a buffer position.  This buffer
position is call the relative indent point for the line.  Some
syntactic symbols may not have a relative indent point associated with
them.

After the syntactic context list for a line is generated, vhdl-mode
calculates the absolute indentation for the line by looking at each
syntactic element in the list.  First, it compares the syntactic
element against the SYNTACTIC-SYMBOL's in `vhdl-offsets-alist'.  When it
finds a match, it adds the OFFSET to the column of the relative indent
point.  The sum of this calculation for each element in the syntactic
list is the absolute offset for line being indented.

If the syntactic element does not match any in the `vhdl-offsets-alist',
an error is generated if `vhdl-strict-syntax-p' is non-nil, otherwise
the element is ignored.

Actually, OFFSET can be an integer, a function, a variable, or one of
the following symbols: `+', `-', `++', or `--'.  These latter
designate positive or negative multiples of `vhdl-basic-offset',
respectively: *1, *-1, *2, and *-2. If OFFSET is a function, it is
called with a single argument containing the cons of the syntactic
element symbol and the relative indent point.  The function should
return an integer offset.

Here is the current list of valid syntactic element symbols:

 string                 -- inside multi-line string
 block-open             -- statement block open
 block-close            -- statement block close
 statement              -- a VHDL statement
 statement-cont         -- a continuation of a VHDL statement
 statement-block-intro  -- the first line in a new statement block
 statement-case-intro   -- the first line in a case alternative block
 case-alternative       -- a case statement alternative clause
 comment                -- a line containing only a comment
 arglist-intro          -- the first line in an argument list
 arglist-cont           -- subsequent argument list lines when no
                           arguments follow on the same line as the
                           the arglist opening paren
 arglist-cont-nonempty  -- subsequent argument list lines when at
                           least one argument follows on the same
                           line as the arglist opening paren
 arglist-close          -- the solo close paren of an argument list
 entity                 -- inside an entity declaration
 configuration          -- inside a configuration declaration
 package                -- inside a package declaration
 architecture           -- inside an architecture body
 package-body           -- inside a package body
")

(defvar vhdl-comment-only-line-offset 0
  "*Extra offset for line which contains only the start of a comment.
Can contain an integer or a cons cell of the form:

 (NON-ANCHORED-OFFSET . ANCHORED-OFFSET)

Where NON-ANCHORED-OFFSET is the amount of offset given to
non-column-zero anchored comment-only lines, and ANCHORED-OFFSET is
the amount of offset to give column-zero anchored comment-only lines.
Just an integer as value is equivalent to (<val> . 0)")

(defvar vhdl-special-indent-hook nil
  "*Hook for user defined special indentation adjustments.
This hook gets called after a line is indented by the mode.")

(defvar vhdl-style-alist
  '(("IEEE"
     (vhdl-basic-offset . 4)
     (vhdl-offsets-alist . ())
     )
    )
  "Styles of Indentation.
Elements of this alist are of the form:

  (STYLE-STRING (VARIABLE . VALUE) [(VARIABLE . VALUE) ...])

where STYLE-STRING is a short descriptive string used to select a
style, VARIABLE is any vhdl-mode variable, and VALUE is the intended
value for that variable when using the selected style.

There is one special case when VARIABLE is `vhdl-offsets-alist'.  In this
case, the VALUE is a list containing elements of the form:

  (SYNTACTIC-SYMBOL . VALUE)

as described in `vhdl-offsets-alist'.  These are passed directly to
`vhdl-set-offset' so there is no need to set every syntactic symbol in
your style, only those that are different from the default.")

;; dynamically append the default value of most variables
(or (assoc "Default" vhdl-style-alist)
    (let* ((varlist '(vhdl-inhibit-startup-warnings-p
		      vhdl-strict-syntax-p
		      vhdl-echo-syntactic-information-p
		      vhdl-basic-offset
		      vhdl-offsets-alist
		      vhdl-comment-only-line-offset))
	   (default (cons "Default"
			  (mapcar
			   (function
			    (lambda (var)
			      (cons var (symbol-value var))
			      ))
			   varlist))))
      (setq vhdl-style-alist (cons default vhdl-style-alist))))

(defvar vhdl-mode-hook nil
  "*Hook called by `vhdl-mode'.")


;; ############################################################################
;; Emacs variant handling
;; ############################################################################

;; active regions

(defun vhdl-keep-region-active ()
  ;; do whatever is necessary to keep the region active in XEmacs
  ;; (formerly Lucid). ignore byte-compiler warnings you might see
  (and (boundp 'zmacs-region-stays)
       (setq zmacs-region-stays t)))

(defconst vhdl-emacs-features
  (let ((major (and (boundp 'emacs-major-version)
		    emacs-major-version))
	(minor (and (boundp 'emacs-minor-version)
		    emacs-minor-version))
	flavor)
    ;; figure out version numbers if not already discovered
    (and (or (not major) (not minor))
	 (string-match "\\([0-9]+\\).\\([0-9]+\\)" emacs-version)
	 (setq major (string-to-int (substring emacs-version
					       (match-beginning 1)
					       (match-end 1)))
	       minor (string-to-int (substring emacs-version
					       (match-beginning 2)
					       (match-end 2)))))
    (if (not (and major minor))
	(error "Cannot figure out the major and minor version numbers."))
    ;; calculate the major version
    (cond
     ((= major 18) (setq major 'v18))	;Emacs 18
     ((= major 4)  (setq major 'v18))	;Epoch 4
     ((= major 19) (setq major 'v19	;Emacs 19
			 flavor (cond
				 ((string-match "Win-Emacs" emacs-version)
				  'Win-Emacs)
				 ((or (string-match "Lucid" emacs-version)
				      (string-match "XEmacs" emacs-version))
				  'XEmacs)
				 (t
				  t))))
     ((= major 20) (setq major 'v20	;Emacs 20
			 flavor (cond
				 ((string-match "Win-Emacs" emacs-version)
				  'Win-Emacs)
				 ((or (string-match "Lucid" emacs-version)
				      (string-match "XEmacs" emacs-version))
				  'XEmacs)
				 (t
				  t))))
     ;; I don't know
     (t (error "Cannot recognize major version number: %s" major)))
    ;; lets do some minimal sanity checking.
    (if (and (or
	      ;; Emacs 18 is brain dead
	      (eq major 'v18)
	      ;; Lemacs before 19.6 had bugs
	      (and (eq major 'v19) (eq flavor 'XEmacs) (< minor 6))
	      ;; Emacs 19 before 19.21 had bugs
	      (and (eq major 'v19) (eq flavor t) (< minor 21)))
	     (not vhdl-inhibit-startup-warnings-p))
	(with-output-to-temp-buffer "*vhdl-mode warnings*"
	  (print (format
"The version of Emacs that you are running, %s,
has known bugs in its syntax.c parsing routines which will affect the
performance of vhdl-mode. You should strongly consider upgrading to the
latest available version.  vhdl-mode may continue to work, after a
fashion, but strange indentation errors could be encountered."
		     emacs-version))))
    (list major flavor))
  "A list of features extant in the Emacs you are using.
There are many flavors of Emacs out there, each with different
features supporting those needed by vhdl-mode.  Here's the current
supported list, along with the values for this variable:

 Emacs 18/Epoch 4:           (v18)
 XEmacs (formerly Lucid) 19: (v19 XEmacs)
 Win-Emacs 1.35:             (V19 Win-Emacs)
 Emacs 19:               (v19 t)
 Emacs 20:               (v20 t).")


;; ############################################################################
;; Bindings
;; ############################################################################

;; ############################################################################
;; Key bindings

(defvar vhdl-template-map ()
  "Keymap for VHDL templates.")

(if vhdl-template-map ()
  (setq vhdl-template-map (make-sparse-keymap))
  ;; key bindings for VHDL templates
  (define-key vhdl-template-map "\M-A" 'vhdl-alias)
  (define-key vhdl-template-map "a"    'vhdl-architecture)
  (define-key vhdl-template-map "A"    'vhdl-array)
  (define-key vhdl-template-map "\M-a" 'vhdl-assert)
  (define-key vhdl-template-map "b"    'vhdl-block)
  (define-key vhdl-template-map "c"    'vhdl-case)
  (define-key vhdl-template-map "\M-c" 'vhdl-component)
  (define-key vhdl-template-map "I"    'vhdl-component-instance)
  (define-key vhdl-template-map "\M-s" 'vhdl-concurrent-signal-assignment)
  (define-key vhdl-template-map "\M-Cb"'vhdl-block-configuration)
  (define-key vhdl-template-map "\M-Cc"'vhdl-component-configuration)
  (define-key vhdl-template-map "\M-Cd"'vhdl-configuration-decl)
  (define-key vhdl-template-map "\M-Cs"'vhdl-configuration-spec)
  (define-key vhdl-template-map "C"    'vhdl-constant)
  (define-key vhdl-template-map "d"    'vhdl-disconnect)
  (define-key vhdl-template-map "\M-e" 'vhdl-else)
  (define-key vhdl-template-map "E"    'vhdl-elsif)
  (define-key vhdl-template-map "e"    'vhdl-entity)
  (define-key vhdl-template-map "x"    'vhdl-exit)
  (define-key vhdl-template-map "f"    'vhdl-for)
  (define-key vhdl-template-map "F"    'vhdl-function)
  (define-key vhdl-template-map "g"    'vhdl-generate)
  (define-key vhdl-template-map "G"    'vhdl-generic)
  (define-key vhdl-template-map "h"    'vhdl-header)
  (define-key vhdl-template-map "i"    'vhdl-if)
  (define-key vhdl-template-map "L"    'vhdl-library)
  (define-key vhdl-template-map "l"    'vhdl-loop)
  (define-key vhdl-template-map "m"    'vhdl-modify)
  (define-key vhdl-template-map "M"    'vhdl-map)
  (define-key vhdl-template-map "n"    'vhdl-next)
  (define-key vhdl-template-map "k"    'vhdl-package)
  (define-key vhdl-template-map "("    'vhdl-paired-parens)
  (define-key vhdl-template-map "\M-p" 'vhdl-port)
  (define-key vhdl-template-map "p"    'vhdl-procedure)
  (define-key vhdl-template-map "P"    'vhdl-process)
  (define-key vhdl-template-map "R"    'vhdl-record)
  (define-key vhdl-template-map "r"    'vhdl-return-value)
  (define-key vhdl-template-map "\M-S" 'vhdl-selected-signal-assignment)
  (define-key vhdl-template-map "s"    'vhdl-signal)
  (define-key vhdl-template-map "S"    'vhdl-subtype)
  (define-key vhdl-template-map "t"    'vhdl-type)
  (define-key vhdl-template-map "u"    'vhdl-use)
  (define-key vhdl-template-map "v"    'vhdl-variable)
  (define-key vhdl-template-map "W"    'vhdl-wait)
  (define-key vhdl-template-map "w"    'vhdl-while-loop)
  (define-key vhdl-template-map "\M-w" 'vhdl-with)
  (define-key vhdl-template-map "\M-W" 'vhdl-clocked-wait)
  (define-key vhdl-template-map "Kb"   'vhdl-package-numeric-bit)
  (define-key vhdl-template-map "Kn"   'vhdl-package-numeric-std)
  (define-key vhdl-template-map "Ks"   'vhdl-package-std-logic-1164)
  (define-key vhdl-template-map "Kt"   'vhdl-package-textio)
  )

(defvar vhdl-mode-map ()
  "Keymap for VHDL Mode.")

(if vhdl-mode-map ()
  (setq vhdl-mode-map (make-sparse-keymap))
  ;; key bindings for templates
  (define-key vhdl-mode-map
    (concat "\C-c" vhdl-template-key-binding-prefix) vhdl-template-map)
  ;; standard key bindings
  (define-key vhdl-mode-map "\M-a"     'vhdl-beginning-of-statement)
  (define-key vhdl-mode-map "\M-e"     'vhdl-end-of-statement)
  (define-key vhdl-mode-map "\M-\C-f"  'vhdl-forward-sexp)
  (define-key vhdl-mode-map "\M-\C-b"  'vhdl-backward-sexp)
  (define-key vhdl-mode-map "\M-\C-u"  'vhdl-backward-up-list)
  ;(define-key vhdl-mode-map "\M-\C-d"	'vhdl-down-list)
  (define-key vhdl-mode-map "\M-\C-a"  'vhdl-beginning-of-defun)
  (define-key vhdl-mode-map "\M-\C-e"  'vhdl-end-of-defun)
  (define-key vhdl-mode-map "\M-\C-h"  'vhdl-mark-defun)
  (define-key vhdl-mode-map "\M-\C-q"  'vhdl-indent-sexp)
  (define-key vhdl-mode-map "\177"     'backward-delete-char-untabify)
  (define-key vhdl-mode-map "\r"       'vhdl-return)
  (if vhdl-intelligent-tab
      (define-key vhdl-mode-map "\t"       'vhdl-tab)
    (define-key vhdl-mode-map "\t"       'vhdl-indent-line))
  (define-key vhdl-mode-map " "        'vhdl-outer-space)
  ;; new key bindings for VHDL Mode, with no counterpart to BOCM
  (define-key vhdl-mode-map "\C-c\C-e" 'vhdl-electric-mode)
  (define-key vhdl-mode-map "\C-c\C-s" 'vhdl-stutter-mode)
  (define-key vhdl-mode-map "\C-c\C-u" 'vhdl-fix-case-buffer)
  (define-key vhdl-mode-map "\C-c\C-f" 'font-lock-fontify-buffer)
  (define-key vhdl-mode-map "\C-c\C-x" 'vhdl-show-syntactic-information)
  (define-key vhdl-mode-map "\C-c\C-r" 'vhdl-regress-line)
  (define-key vhdl-mode-map "\C-c\C-i" 'vhdl-indent-line)
  (define-key vhdl-mode-map "\C-c\C-a" 'vhdl-align-noindent-region)
  (define-key vhdl-mode-map "\C-c\M-\C-a" 'vhdl-align-comment-region)
  (define-key vhdl-mode-map "\C-c\C-c" 'vhdl-comment-uncomment-region)
  (define-key vhdl-mode-map "\C-c-"    'vhdl-inline-comment)
  (define-key vhdl-mode-map "\C-c\M--" 'vhdl-display-comment-line)
  (define-key vhdl-mode-map "\C-c\C-o" 'vhdl-open-line)
  (define-key vhdl-mode-map "\C-c\C-g" 'goto-line)
  (define-key vhdl-mode-map "\C-c\C-d" 'vhdl-kill-line)
  (define-key vhdl-mode-map "\C-c\C-h" 'vhdl-help)
  (define-key vhdl-mode-map "\C-c\C-v" 'vhdl-version)
  (define-key vhdl-mode-map "\C-c\C-b" 'vhdl-submit-bug-report)
  (define-key vhdl-mode-map "\C-c\C-k" 'vhdl-compile)
  (define-key vhdl-mode-map "\C-c\M-\C-k" 'vhdl-make)
  (define-key vhdl-mode-map "\M-\t"    'tab-to-tab-stop)
  ;; key bindings for stuttering
  (define-key vhdl-mode-map "-"        'vhdl-stutter-mode-dash)
  (define-key vhdl-mode-map "'"        'vhdl-stutter-mode-quote)
  (define-key vhdl-mode-map ";"        'vhdl-stutter-mode-semicolon)
  (define-key vhdl-mode-map "["        'vhdl-stutter-mode-open-bracket)
  (define-key vhdl-mode-map "]"        'vhdl-stutter-mode-close-bracket)
  (define-key vhdl-mode-map "."        'vhdl-stutter-mode-period)
  (define-key vhdl-mode-map ","        'vhdl-stutter-mode-comma)
  (let ((c 97))
    (while (< c 123) ; for little a-z
      (define-key vhdl-mode-map (char-to-string c) 'vhdl-stutter-mode-caps)
      (setq c (1+ c))
      ))
  )

;; define special minibuffer keymap for enabling word completion in minibuffer
;; (useful in template generator prompts)
(defvar vhdl-minibuffer-local-map (copy-keymap minibuffer-local-map)
  "Keymap for minibuffer used in VHDL Mode.")

(define-key vhdl-minibuffer-local-map "\t" 'vhdl-minibuffer-tab)

(defvar vhdl-mode-syntax-table nil
  "Syntax table used in vhdl-mode buffers.")

(if vhdl-mode-syntax-table ()
  (setq vhdl-mode-syntax-table (make-syntax-table))
  ;; DO NOT TRY TO SET _ (UNDERSCORE) TO WORD CLASS!
  ;; why not? (is left to the user here)
  (if vhdl-underscore-is-part-of-word
      (modify-syntax-entry ?_ "w"     vhdl-mode-syntax-table))
  (modify-syntax-entry ?\" "\""    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\$ "."     vhdl-mode-syntax-table)
  (modify-syntax-entry ?\% "."     vhdl-mode-syntax-table)
  (modify-syntax-entry ?\& "."     vhdl-mode-syntax-table)
  (modify-syntax-entry ?\' "."     vhdl-mode-syntax-table)
  (modify-syntax-entry ?\( "()"    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\) ")("    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\* "."     vhdl-mode-syntax-table)
  (modify-syntax-entry ?\+ "."     vhdl-mode-syntax-table)
  (modify-syntax-entry ?\. "."     vhdl-mode-syntax-table)
  (modify-syntax-entry ?\/ "."     vhdl-mode-syntax-table)
  (modify-syntax-entry ?\: "."     vhdl-mode-syntax-table)
  (modify-syntax-entry ?\; "."     vhdl-mode-syntax-table)
  (modify-syntax-entry ?\< "."     vhdl-mode-syntax-table)
  (modify-syntax-entry ?\= "."     vhdl-mode-syntax-table)
  (modify-syntax-entry ?\> "."     vhdl-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]"    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\"    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\] ")["    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}"    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\| "."     vhdl-mode-syntax-table)
  (modify-syntax-entry ?\} "){"    vhdl-mode-syntax-table)
  ;; add comment syntax
  (modify-syntax-entry ?\- ". 12"  vhdl-mode-syntax-table)
  (modify-syntax-entry ?\n ">"     vhdl-mode-syntax-table)
  (modify-syntax-entry ?\^M ">"    vhdl-mode-syntax-table))

(defvar vhdl-syntactic-context nil
  "Buffer local variable containing syntactic analysis list.")
(make-variable-buffer-local 'vhdl-syntactic-context)

;; ############################################################################
;; Abbrev hook bindings

(defvar vhdl-mode-abbrev-table nil
  "Abbrev table in use in vhdl-mode buffers.")

(define-abbrev-table 'vhdl-mode-abbrev-table
  '(
    ("--" "" vhdl-display-comment-hook 0)
    ("abs" "" vhdl-default-hook 0)
    ("access" "" vhdl-default-hook 0)
    ("after" "" vhdl-default-hook 0)
    ("alias" "" vhdl-alias-hook 0)
    ("all" "" vhdl-default-hook 0)
    ("and" "" vhdl-default-hook 0)
    ("arch" "" vhdl-architecture-hook 0)
    ("architecture" "" vhdl-architecture-hook 0)
    ("array" "" vhdl-array-hook 0)
    ("assert" "" vhdl-assert-hook 0)
    ("attr" "" vhdl-attribute-hook 0)
    ("attribute" "" vhdl-attribute-hook 0)
    ("begin" "" vhdl-default-indent-hook 0)
    ("block" "" vhdl-block-hook 0)
    ("body" "" vhdl-default-hook 0)
    ("buffer" "" vhdl-default-hook 0)
    ("bus" "" vhdl-default-hook 0)
    ("case" "" vhdl-case-hook 0)
    ("comp" "" vhdl-component-hook 0)
    ("component" "" vhdl-component-hook 0)
    ("conc" "" vhdl-concurrent-signal-assignment-hook 0)
    ("concurrent" "" vhdl-concurrent-signal-assignment-hook 0)
    ("conf" "" vhdl-configuration-hook 0)
    ("configuration" "" vhdl-configuration-hook 0)
    ("cons" "" vhdl-constant-hook 0)
    ("constant" "" vhdl-constant-hook 0)
    ("disconnect" "" vhdl-disconnect-hook 0)
    ("downto" "" vhdl-default-hook 0)
    ("else" "" vhdl-else-hook 0)
    ("elseif" "" vhdl-elsif-hook 0)
    ("elsif" "" vhdl-elsif-hook 0)
    ("end" "" vhdl-default-indent-hook 0)
    ("entity" "" vhdl-entity-hook 0)
    ("exit" "" vhdl-exit-hook 0)
    ("file" "" vhdl-default-hook 0)
    ("for" "" vhdl-for-hook 0)
    ("func" "" vhdl-function-hook 0)
    ("function" "" vhdl-function-hook 0)
    ("gen" "" vhdl-generate-hook 0)
    ("generate" "" vhdl-generate-hook 0)
    ("generic" "" vhdl-generic-hook 0)
    ("group" "" vhdl-default-hook 0)
    ("guarded" "" vhdl-default-hook 0)
    ("header" "" vhdl-header-hook 0)
    ("if" "" vhdl-if-hook 0)
    ("impure" "" vhdl-default-hook 0)
    ("in" "" vhdl-default-hook 0)
    ("inertial" "" vhdl-default-hook 0)
    ("inout" "" vhdl-default-hook 0)
    ("inst" "" vhdl-component-instance-hook 0)
    ("instance" "" vhdl-component-instance-hook 0)
    ("is" "" vhdl-default-hook 0)
    ("label" "" vhdl-default-hook 0)
    ("library" "" vhdl-library-hook 0)
    ("linkage" "" vhdl-default-hook 0)
    ("literal" "" vhdl-default-hook 0)
    ("loop" "" vhdl-loop-hook 0)
    ("map" "" vhdl-map-hook 0)
    ("mod" "" vhdl-default-hook 0)
    ("modify" "" vhdl-modify-hook 0)
    ("nand" "" vhdl-default-hook 0)
    ("new" "" vhdl-default-hook 0)
    ("next" "" vhdl-next-hook 0)
    ("nor" "" vhdl-default-hook 0)
    ("not" "" vhdl-default-hook 0)
    ("null" "" vhdl-default-hook 0)
    ("of" "" vhdl-default-hook 0)
    ("on" "" vhdl-default-hook 0)
    ("open" "" vhdl-default-hook 0)
    ("or" "" vhdl-default-hook 0)
    ("others" "" vhdl-default-hook 0)
    ("out" "" vhdl-default-hook 0)
    ("pack" "" vhdl-package-hook 0)
    ("package" "" vhdl-package-hook 0)
    ("port" "" vhdl-port-hook 0)
    ("postponed" "" vhdl-default-hook 0)
    ("procedure" "" vhdl-procedure-hook 0)
    ("process" "" vhdl-process-hook 0)
    ("pure" "" vhdl-default-hook 0)
    ("range" "" vhdl-default-hook 0)
    ("record" "" vhdl-record-hook 0)
    ("register" "" vhdl-default-hook 0)
    ("reject" "" vhdl-default-hook 0)
    ("rem" "" vhdl-default-hook 0)
    ("report" "" vhdl-default-hook 0)
    ("ret" "" vhdl-return-hook 0)
    ("return" "" vhdl-return-hook 0)
    ("rol" "" vhdl-default-hook 0)
    ("ror" "" vhdl-default-hook 0)
    ("select" "" vhdl-selected-signal-assignment-hook 0)
    ("severity" "" vhdl-default-hook 0)
    ("shared" "" vhdl-default-hook 0)
    ("sig" "" vhdl-signal-hook 0)
    ("signal" "" vhdl-signal-hook 0)
    ("sla" "" vhdl-default-hook 0)
    ("sll" "" vhdl-default-hook 0)
    ("sra" "" vhdl-default-hook 0)
    ("srl" "" vhdl-default-hook 0)
    ("sub" "" vhdl-subtype-hook 0)
    ("subtype" "" vhdl-subtype-hook 0)
    ("then" "" vhdl-default-hook 0)
    ("to" "" vhdl-default-hook 0)
    ("transport" "" vhdl-default-hook 0)
    ("type" "" vhdl-type-hook 0)
    ("unaffected" "" vhdl-default-hook 0)
    ("units" "" vhdl-default-hook 0)
    ("until" "" vhdl-default-hook 0)
    ("use" "" vhdl-use-hook 0)
    ("var" "" vhdl-variable-hook 0)
    ("variable" "" vhdl-variable-hook 0)
    ("wait" "" vhdl-wait-hook 0)
    ("warning" "" vhdl-default-hook 0)
    ("when" "" vhdl-when-hook 0)
    ("while" "" vhdl-while-loop-hook 0)
    ("with" "" vhdl-selected-signal-assignment-hook 0)
    ("xnor" "" vhdl-default-hook 0)
    ("xor" "" vhdl-default-hook 0)
    ))


;; ############################################################################
;; Menues
;; ############################################################################

;; ############################################################################
;; VHDL menu (using `easy-menu.el')

;; `customize-menu-create' is included in `cus-edit.el' version 1.9954,
;; which is not yet distributed with XEmacs 19.15
(defun vhdl-customize-menu-create (symbol &optional name)
  "Return a customize menu for customization group SYMBOL.
If optional NAME is given, use that as the name of the menu.
Otherwise the menu will be named `Customize'.
The format is suitable for use with `easy-menu-define'."
  (unless name
    (setq name "Customize"))
  (if (memq 'XEmacs vhdl-emacs-features)
      ;; We can delay it under XEmacs.
      `(,name
        :filter (lambda (&rest junk)
                  (cdr (custom-menu-create ',symbol))))
    ;; But we must create it now under Emacs.
    (cons name (cdr (custom-menu-create symbol)))))

(defvar vhdl-mode-menu
  (append
  '("VHDL"
    ("Mode"
     ["Electric" vhdl-electric-mode :style toggle :selected vhdl-electric-mode]
     ["Stutter" vhdl-stutter-mode :style toggle :selected vhdl-stutter-mode]
     )
    "--"
    ("Compile"
     ["Compile Buffer" vhdl-compile t]
     ["Stop Compilation" kill-compilation t]
     "--"
     ["Make" vhdl-make t]
     ["Generate Makefile" vhdl-generate-makefile t]
     "--"
     ["Next Error" next-error t]
     ["Previous Error" previous-error t]
     ["First Error" first-error t]
     )
    "--"
    ("Template"
     ("VHDL Construct 1"
      ["Alias" vhdl-alias t]
      ["Architecture" vhdl-architecture t]
      ["Array" vhdl-array t]
      ["Assert" vhdl-assert t]
      ["Attribute" vhdl-attribute t]
      ["Block" vhdl-block t]
      ["Case" vhdl-case t]
      ["Component" vhdl-component t]
      ["Concurrent (Signal Asst)" vhdl-concurrent-signal-assignment t]
      ["Configuration (Block)" vhdl-block-configuration t]
      ["Configuration (Comp)" vhdl-component-configuration t]
      ["Configuration (Decl)" vhdl-configuration-decl t]
      ["Configuration (Spec)" vhdl-configuration-spec t]
      ["Constant" vhdl-constant t]
      ["Disconnect" vhdl-disconnect t]
      ["Else" vhdl-else t]
      ["Elsif" vhdl-elsif t]
      ["Entity" vhdl-entity t]
      ["Exit" vhdl-exit t]
      ["For (Loop)" vhdl-for t]
      ["Function" vhdl-function t]
      ["(For/If) Generate" vhdl-generate t]
      ["Generic" vhdl-generic t]
      )
     ("VHDL Construct 2"
      ["If" vhdl-if t]
      ["Instance" vhdl-component-instance t]
      ["Library" vhdl-library t]
      ["Loop" vhdl-loop t]
      ["Map" vhdl-map t]
      ["Next" vhdl-next t]
      ["Package" vhdl-package t]
      ["Port" vhdl-port t]
      ["Procedure" vhdl-procedure t]
      ["Process" vhdl-process t]
      ["Record" vhdl-record t]
      ["Return" vhdl-return-value t]
      ["Select" vhdl-selected-signal-assignment t]
      ["Signal" vhdl-signal t]
      ["Subtype" vhdl-subtype t]
      ["Type" vhdl-type t]
      ["Use" vhdl-use t]
      ["Variable" vhdl-variable t]
      ["Wait" vhdl-wait t]
      ["(Clocked Wait)" vhdl-clocked-wait t]
      ["When" vhdl-when t]
      ["While (Loop)" vhdl-while-loop t]
      ["With" vhdl-with t]
      )
     ("Standard Package"
      ["numeric_bit" vhdl-package-numeric-bit t]
      ["numeric_std" vhdl-package-numeric-std t]
      ["std_logic_1164" vhdl-package-std-logic-1164 t]
      ["textio" vhdl-package-textio t]
      )
     ["Header" vhdl-header t]
     ["Modify (Date)" vhdl-modify t]
     )
    ("Comment"
     ["(Un)Comment Out Region" vhdl-comment-uncomment-region (mark)]
     ["Insert Inline Comment" vhdl-inline-comment t]
     ["Insert Horizontal Line" vhdl-display-comment-line t]
     ["Insert Display Comment" vhdl-display-comment t]
     ["Fill Comment" fill-paragraph t]
     ["Fill Comment Region" fill-region (mark)]
     )
    ("Indent"
     ["Line" vhdl-indent-line t]
     ["Region" indent-region (mark)]
     ["Buffer" vhdl-indent-buffer t]
     )
    ("Align"
     ["Region" vhdl-align-noindent-region (mark)]
     ["Comment Region" vhdl-align-comment-region (mark)]
     )
    ("Line"
     ["Open" vhdl-open-line t]
     ["Delete" vhdl-kill-line t]
     ["Join" delete-indentation t]
     ["Goto" goto-line t]
     )
    ("Move"
     ["Forward Statement" vhdl-end-of-statement t]
     ["Backward Statement" vhdl-beginning-of-statement t]
     ["Forward Expression" vhdl-forward-sexp t]
     ["Backward Expression" vhdl-backward-sexp t]
     ["Forward Function" vhdl-end-of-defun t]
     ["Backward Function" vhdl-beginning-of-defun t]
     )
     "--"
    ("Fix Case"
     ["Buffer" vhdl-fix-case-buffer t]
     ["Region" vhdl-fix-case-region (mark)]
     )
    ["Fontify Buffer" font-lock-fontify-buffer t]
    ["Syntactic Info" vhdl-show-syntactic-information t]
     "--"
    ["Help" vhdl-help t]
    ["Version" vhdl-version t]
    ["Bug Report" vhdl-submit-bug-report t]
     "--"
    )
  (list (vhdl-customize-menu-create 'vhdl))
))

(require 'easymenu)

;; ############################################################################
;; Index menu (using `imenu.el')

(defvar vhdl-imenu-generic-expression
  '(
    ("Entity"
     "^\\s-*\\(entity\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\)"
     2)
    ("Architecture"
     "^\\s-*\\(architecture\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\s-+of\\s-+\\(\\w\\|\\s_\\)+\\)"
     2)
    ("Configuration"
     "^\\s-*\\(configuration\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\s-+of\\s-+\\(\\w\\|\\s_\\)+\\)"
     2)
    ("Package Body"
     "^\\s-*\\(package body\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\)"
     2)
    ("Package"
     "^\\s-*\\(package\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\)"
     2)
    ("Type"
     "^\\s-*\\(sub\\)?type\\s-+\\(\\(\\w\\|\\s_\\)+\\)"
     2)
    ("Component"
     "^\\s-*\\(component\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\)"
     2)
    ("Function / Procedure"
     "^\\s-*\\(procedure\\|function\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\)"
     2)
    ("Process / Block"
     "^\\s-*\\(\\(\\w\\|\\s_\\)+\\)\\s-*:\\(\\s-\\|\n\\)*\\(process\\|block\\)"
     1)
    ("Instance"
     "^\\s-*\\(\\(\\w\\|\\s_\\)+\\s-*:\\(\\s-\\|\n\\)*\\(\\w\\|\\s_\\)+\\)\\(\\s-\\|\n\\)+\\(generic\\|port\\)\\s-+map\\>"
     1)
    )
  "Imenu generic expression for VHDL Mode.  See `imenu-generic-expression'.")

(defun vhdl-add-index-menu ()
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression (nreverse vhdl-imenu-generic-expression))
  (imenu-add-to-menubar "Index"))

;; ############################################################################
;; Source file menu (using `easy-menu.el')

(defvar vhdl-extlist '("[A-Za-z0-9_.]*.vhdl?$"))
(defvar vhdl-filelist-menu nil)

(defun vhdl-add-source-files-menu ()
  "Scan directory of current source file for all VHDL source files, and
generate menu."
  (interactive)
  (message "Scanning directory for source files ...")
  (let (filelist menulist tmpextlist found
		 (newmap (current-local-map)))
    (cd (file-name-directory (buffer-file-name)))
    ;; find files
    (setq menulist '())
    (setq tmpextlist vhdl-extlist)
    (while tmpextlist
      (setq filelist (nreverse (directory-files
				(file-name-directory (buffer-file-name))
				nil (car tmpextlist) nil)))
      ;; Create list for menu
      (setq found nil)
      (while filelist
	(setq found t)
	(setq menulist (cons (vector (car filelist)
				     (list 'find-file (car filelist)) t)
			     menulist))
	(setq filelist (cdr filelist)))
      (setq menulist (vhdl-menu-split menulist 25))
      (if found
	  (setq menulist (cons "--" menulist)))
      (setq tmpextlist (cdr tmpextlist)))
    (setq menulist (cons ["*Rescan*" vhdl-add-source-files-menu t] menulist))
    (setq menulist (cons "Sources" menulist))
    ;; Create menu
    (easy-menu-add menulist)
    (easy-menu-define vhdl-filelist-menu newmap
		      "VHDL source files menu" menulist)
;    (use-local-map (append (current-local-map) newmap))
;    (use-local-map newmap)
    )
  (message ""))

(defun vhdl-menu-split (list n)
  "Split menu into several submenues, if number of elements > n."
  (if (> (length list) n)
      (let ((remain list)
	    (result '())
	    (sublist '())
	    (menuno 1)
	    (i 0))
	(while remain
	  (setq sublist (cons (car remain) sublist))
	  (setq remain (cdr remain))
	  (setq i (+ i 1))
	  (if (= i n)
	      (progn
		(setq result (cons (cons (format "Sources %s" menuno)
					 (nreverse sublist)) result))
		(setq i 0)
		(setq menuno (+ menuno 1))
		(setq sublist '()))))
	(and sublist
	     (setq result (cons (cons (format "Sources %s" menuno)
				      (nreverse sublist)) result)))
	(nreverse result))
    list))


;; ############################################################################
;; VHDL Mode definition
;; ############################################################################
;;;###autoload
(defun vhdl-mode ()
  "Major mode for editing VHDL code.

Usage:
------

- TEMPLATE INSERTION (electrification) (`\\[vhdl-outer-space]'): After typing
  a VHDL keyword and entering `\\[vhdl-outer-space]', you are prompted for
  arguments while a template is generated for that VHDL construct. Typing
  `\\[vhdl-return]' (or `\\[keyboard-quit]' in yes-no queries) at the first
  prompt aborts the current template generation. Typing `\\[just-one-space]'
  after a keyword inserts a space without calling the template generator.
  Automatic calling of the template generators (i.e. electrification) can be
  disabled (enabled) by setting the variable `vhdl-electric-mode' to nil
  (non-nil) or by typing `\\[vhdl-electric-mode]' (toggles electrification
  mode).
  Template generators can be called using the VHDL menu, the key bindings, or
  by typing the keyword (first word of menu entry not in parenthesis) and
  `\\[vhdl-outer-space]'. The following abbreviations can also be used:
  arch, attr, conc, conf, comp, cons, func, inst, pack, ret, sig, sub, var.

- HEADER INSERTION (`\\[vhdl-header]'): A customized header can be inserted
  including the actual file name, user name, and current date as well as
  prompted title strings. A custom header can be defined in a separate file
  (see custom variable `vhdl-header-file').

- STUTTERING (double strike): Double striking of some keys inserts cumbersome
  VHDL syntax elements. Stuttering can be disabled by variable
  `vhdl-stutter-mode' and be toggled by typing `\\[vhdl-stutter-mode]'.
      ''   -->  \"             [   -->  (        --    -->  comment
      ;;   -->  \" : \"         [[  -->  [        --CR  -->  comment-out code
      ;;;  -->  \" := \"        ]   -->  )        ---   -->  horizontal line
      ..   -->  \" => \"        ]]  -->  ]        ----  -->  display comment
      ,,   -->  \" <= \"        aa  -->  A    -   zz  -->  Z

- WORD COMPLETION (`\\[vhdl-tab]'): Typing `\\[vhdl-tab]' after a (not
  completed) word looks for a word in the buffer that starts alike and
  inserts it. Re-typing `\\[vhdl-tab]' toggles through alternative word
  completions. This also works in the minibuffer (i.e. in template generator
  prompts).

  Typing `\\[vhdl-tab]' after a non-word character indents the line if at the
  beginning of a line (i.e. no preceding non-blank characters), and inserts a
  tabulator stop otherwise. `\\[tab-to-tab-stop]' always inserts a tabulator
  stop.

- COMMENTS (`--', `---', `----', `--CR'):
      `--'       puts a single comment.
      `---'      draws a horizontal line for separating code segments.
      `----'     inserts a display comment, i.e. two horizontal lines with a
                 comment in between.
      `--CR'     comments out code on that line. Re-hitting CR comments out
                 following lines.
      `\\[vhdl-comment-uncomment-region]'  comments out a region if not
                 commented out, uncomments out a region if already
                 commented out.

  You are prompted for comments after object definitions (i.e. signals,
  variables, constants, ports) and after subprogram and process specifications
  if variable `vhdl-prompt-for-comments' is non-nil. Comments are
  automatically inserted as additional labels (e.g. after begin statements)
  and help comments if `vhdl-self-insert-comments' is non-nil.
  Inline comments (i.e. comments after a piece of code on the same line) are
  indented at least to `vhdl-comment-column'. Comments go at maximum to
  `vhdl-end-comment-column'. `\\[vhdl-return]' after a space in a comment will
  open a new comment line. Typing beyond `vhdl-end-comment-column' in a
  comment automatically opens a new comment line. `\\[fill-paragraph]'
  re-fills multi-line comments.

- INDENTATION: `\\[vhdl-tab]' indents a line if at the beginning of the line.
  The amount of indentation is specified by variable `vhdl-basic-offset'.
  `\\[vhdl-indent-line]' always indents the current line (is bound to `TAB'
  if variable `vhdl-intelligent-tab' is nil). Indentation can be done for
  an entire region (`\\[indent-region]') or buffer (menu). Argument and
  port lists are indented normally (nil) or relative to the opening
  parenthesis (non-nil) according to variable `vhdl-argument-list-indent'.
  If variable `vhdl-indent-tabs-mode' is nil, spaces are used instead of tabs.
  `\\[tabify]' and `\\[untabify]' allow to convert spaces to tabs and vice
  versa.

- ALIGNMENT: `\\[vhdl-align-noindent-region]' aligns port maps, signal and
  variable assignments, inline comments, some keywords, etc., on consecutive
  lines relative to each other within a defined region.
  `\\[vhdl-align-comment-region]' only aligns inline comments (i.e. comments
  that are at the end of a line of code). Some templates are automatically
  aligned after generation if custom variable `vhdl-auto-align' is non-nil.

- KEY BINDINGS: Key bindings (`C-c ...') exist for most commands (see in menu).

- VHDL MENU: All commands can be called from the VHDL menu.

- INDEX MENU: For each VHDL source file, an index of the contained entities,
  architectures, packages, procedures, processes, etc., is created as a menu.
  Selecting a meny entry causes the cursor to jump to the corresponding
  position in the file. Controlled by variable `vhdl-index-menu'.

- SOURCE FILE MENU: A menu containing all VHDL source files in the directory
  of the current file is generated. Selecting a menu entry loads the file.
  Controlled by variable `vhdl-source-file-menu'.

- SOURCE FILE COMPILATION: The syntax of the current buffer can be analyzed
  by calling a VHDL compiler (menu, `\\[vhdl-compile]'). The compiler to be
  used is defined by variable `vhdl-compiler'. Currently supported are
  `cadence', `ikos', `quickhdl', `synopsys', `vantage', `viewlogic', and
  `v-system'. Not all compilers are tested. Please contact me for
  incorporating additional VHDL compilers. An entire hierarchy of source
  files can be compiled by the `make' command (menu, `\\[vhdl-make]').
  This only works if an appropriate `Makefile' exists. Compiler options can
  be defined by variable `vhdl-compiler-options'.

- KEYWORD CASE: Lower and upper case for keywords, predefined types, predefined
  attributes, and predefined enumeration values is supported. If the variable
  `vhdl-upper-case-keywords' is set to non-nil, keywords can be typed in
  lower case and are converted into upper case automatically (not for types,
  attributes, and enumeration values). The case of keywords, types,
  attributes, and enumeration values can be fixed for an entire region (menu)
  or buffer (`\\[vhdl-fix-case-buffer]') according to the variables
  `vhdl-upper-case-{keywords,types,attributes,enum-values}'.

- HIGHLIGHTING (fontification): Keywords, predefined types, predefined
  attributes, and predefined enumeration values (controlled by variable
  `vhdl-highlight-keywords'), as well as comments, strings, and template
  prompts are highlighted using different colors. Unit and subprogram names
  as well as labels are highlighted if variable `vhdl-highlight-names' is
  non-nil. The default colors from `font-lock.el' are used if variable
  `vhdl-customize-colors' is nil. Otherwise, an optimized set of colors
  is taken, which uses bright colors for signals and muted colors for
  everything else. Variable `vhdl-customize-faces' does the same on
  monochrome monitors.

  Signal highlighting allows distinction between clock, reset,
  status/control, data, and test signals according to some signal
  naming convention. Their syntax is defined by variables
  `vhdl-{clock,reset,control,data,test}-signal-syntax'. Signal coloring
  is controlled by the variable `vhdl-highlight-signals'. The default
  signal naming convention is as follows:

  Signal attributes:
      C  clock                   S  control and status
      R  asynchronous reset      D  data and address
      I  synchronous reset       T  test

  Syntax:
      signal name  ::=  \"[A-Z][a-zA-Z0-9]*x[CRISDT][a-zA-Z0-9]*\"
      signal identifier -^^^^^^^^^^^^^^^^^
      delimiter --------------------------^
      above signal attributes -------------^^^^^^^^
      additional attributes -----------------------^^^^^^^^^^^^

  (`x' is used as delimiter because `_' is reserved by the VITAL standard.)
  Examples: ClkxCfast, ResetxRB, ClearxI, SelectDataxS, DataxD, ScanEnablexT.

  If all VHDL words are written in lower case (i.e. variables
  `vhdl-upper-case-{keywords,types,attributes,enum-values}' are set to nil),
  make highlighting case sensitive by setting variable
  `vhdl-highlight-case-sensitive' to non-nil. This way, only names fulfilling
  the above signal syntax including case are highlighted.

- HIDE/SHOW: The code of entire VHDL processes or blocks can be hidden using
  the `Hide/Show' menu or by pressing `S-mouse-2' within the code
  (not in XEmacs).

- PRINTING: Postscript printing with different fonts (`ps-print-color-p' is
  nil, default faces from `font-lock.el' used if `vhdl-customize-faces' is
  nil) or colors (`ps-print-color-p' is non-nil) is possible using the
  standard Emacs postscript printing commands. Variable `vhdl-print-two-column'
  defines appropriate default settings for nice landscape two-column printing.
  The paper format can be set by variable `ps-paper-type'.

- CUSTOMIZATION: All variables can easily be customized using the `Customize'
  menu entry. For some variables, customization only takes effect after
  re-starting Emacs. Customization can also be done globally (i.e. site-wide,
  read INSTALL file). Variables of VHDL Mode must NOT be set using the
  `vhdl-mode-hook' in the .emacs file anymore (delete them if they still are).


Maintenance:
------------

To submit a bug report, enter `\\[vhdl-submit-bug-report]' within VHDL Mode.
Add a description of the problem and include a reproducible test case.

Questions and enhancement requests can be sent to <vhdl-mode@geocities.com>.

The `vhdl-mode-announce' mailing list informs about new VHDL Mode releases.
The `vhdl-mode-victims' mailing list informs about new VHDL Mode beta releases.
You are kindly invited to participate in beta testing. Subscribe to above
mailing lists by sending an email to <vhdl-mode@geocities.com>.

The archive with the latest version is located at
<http://www.geocities.com/SiliconValley/Peaks/8287>.


Bugs and Limitations:
---------------------

- Index menu does not work under XEmacs (limitation of XEmacs ?!).

- Re-indenting large regions or expressions can be slow.

- Hideshow does not work under XEmacs.

- Parsing compilation error messages for Ikos and Vantage VHDL compilers
  does not work under XEmacs.


Key bindings:
-------------

\\{vhdl-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table vhdl-mode-syntax-table)
  (setq major-mode 'vhdl-mode)
  (setq mode-name "VHDL")
  (setq local-abbrev-table vhdl-mode-abbrev-table)
  (use-local-map vhdl-mode-map)
  ;; set local variable values
  (set (make-local-variable 'paragraph-start) "\\s-*\\(---\\|[a-zA-Z]\\|$\\)")
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  (set (make-local-variable 'require-final-newline) t)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'indent-line-function) 'vhdl-indent-line)
  (set (make-local-variable 'comment-start) "--")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-column) vhdl-comment-column)
  (set (make-local-variable 'end-comment-column) vhdl-end-comment-column)
  (set (make-local-variable 'comment-start-skip) "--+\\s-*")
  (set (make-local-variable 'dabbrev-case-fold-search) nil)
  (set (make-local-variable 'indent-tabs-mode) vhdl-indent-tabs-mode)

  ;; setup the comment indent variable in a Emacs version portable way
  ;; ignore any byte compiler warnings you might get here
  (if (boundp 'comment-indent-function)
      (progn (make-local-variable 'comment-indent-function)
	     (setq comment-indent-function 'vhdl-comment-indent)))

  ;; initialize font locking
  (require 'font-lock)
  (vhdl-font-lock-init)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults (list 'vhdl-font-lock-keywords nil
				 (not vhdl-highlight-case-sensitive)
				 '((?\_ . "w"))))
  (turn-on-font-lock)

  ;; variables for source file compilation
  (make-local-variable 'compile-command)
  (set (make-local-variable 'compilation-error-regexp-alist)
       vhdl-compilation-error-regexp-alist)

  ;; add menus
  (if vhdl-index-menu
      (if (or (not (consp font-lock-maximum-size))
	      (> font-lock-maximum-size (buffer-size)))
	  (vhdl-add-index-menu)
	(message "Scanning buffer for index...buffer too big")))
  (if vhdl-source-file-menu (vhdl-add-source-files-menu))
  (easy-menu-add vhdl-mode-menu)
  (easy-menu-define vhdl-mode-easy-menu vhdl-mode-map
                    "Menu keymap for VHDL Mode." vhdl-mode-menu)
  (run-hooks 'menu-bar-update-hook)

  ;; initialize hideshow and add menu
  (if vhdl-hideshow-menu (hs-minor-mode))

  ;; initialize postscript printing
  (vhdl-ps-init)

  (setq mode-name (if vhdl-electric-mode "Electric VHDL" "VHDL"))
  (message "Type C-c C-h for VHDL Mode documentation.")

  (run-hooks 'vhdl-mode-hook)
  )


;; ############################################################################
;; Keywords and predefined words in VHDL'93
;; ############################################################################

;; `regexp-opt' was not used at this place because it is not yet implemented
;; in XEmacs and because it resulted in SLOWER regexps!!

(defconst vhdl-93-keywords-regexp
  (eval-when-compile
    (concat
     "\\<\\("
     (mapconcat
      'identity
      '(
	"abs" "access" "after" "alias" "all" "and" "architecture" "array"
	"assert" "attribute"
	"begin" "block" "body" "buffer" "bus"
	"case" "component" "configuration" "constant"
	"disconnect" "downto"
	"else" "elsif" "end" "entity" "exit"
	"file" "for" "function"
	"generate" "generic" "group" "guarded"
	"if" "impure" "in" "inertial" "inout" "is"
	"label" "library" "linkage" "literal" "loop"
	"map" "mod"
	"nand" "new" "next" "nor" "not" "null"
	"of" "on" "open" "or" "others" "out"
	"package" "port" "postponed" "procedure" "process" "pure"
	"range" "record" "register" "reject" "rem" "report" "return"
	"rol" "ror"
	"select" "severity" "shared" "signal" "sla" "sll" "sra" "srl" "subtype"
	"then" "to" "transport" "type"
	"unaffected" "units" "until" "use"
	"variable"
	"wait" "warning" "when" "while" "with"
	"xnor" "xor"
	)
      "\\|")
     "\\)\\>"))
  "Regexp for VHDL'93 keywords.")

(defconst vhdl-93-types-regexp
  (eval-when-compile
    (concat
     "\\<\\("
     (mapconcat
      'identity
      '(
	"boolean" "bit" "bit_vector" "character" "severity_level" "integer"
	"real" "time" "natural" "positive" "string" "text" "line"
	"unsigned" "signed"
	"std_logic" "std_logic_vector"
	"std_ulogic" "std_ulogic_vector"
	)
      "\\|")
     "\\)\\>"))
  "Regexp for VHDL'93 standardized types.")

(defconst vhdl-93-attributes-regexp
  (eval-when-compile
    (concat
     "\\<\\("
     (mapconcat
      'identity
      '(
	"base" "left" "right" "high" "low" "pos" "val" "succ"
	"pred" "leftof" "rightof" "range" "reverse_range"
	"length" "delayed" "stable" "quiet" "transaction"
	"event" "active" "last_event" "last_active" "last_value"
	"driving" "driving_value" "ascending" "value" "image"
	"simple_name" "instance_name" "path_name"
	"foreign"
	)
      "\\|")
     "\\)\\>"))
  "Regexp for VHDL'93 standardized attributes.")

(defconst vhdl-93-enum-values-regexp
  (eval-when-compile
    (concat
     "\\<\\("
     (mapconcat
      'identity
      '(
	"true" "false"
	"note" "warning" "error" "failure"
	"fs" "ps" "ns" "us" "ms" "sec" "min" "hr"
	)
      "\\|")
     "\\)\\>"))
  "Regexp for VHDL'93 standardized enumeration values.")


;; ############################################################################
;; Syntax analysis and indentation
;; ############################################################################

;; ############################################################################
;; Syntax analysis

;; constant regular expressions for looking at various constructs

(defconst vhdl-symbol-key "\\(\\w\\|\\s_\\)+"
  "Regexp describing a VHDL symbol.
We cannot use just `word' syntax class since `_' cannot be in word
class.  Putting underscore in word class breaks forward word movement
behavior that users are familiar with.")

(defconst vhdl-case-header-key "case[( \t\n][^;=>]+[) \t\n]is"
  "Regexp describing a case statement header key.")

(defconst vhdl-label-key
  (concat "\\(" vhdl-symbol-key "\\s-*:\\)[^=]")
  "Regexp describing a VHDL label.")

;; Macro definitions:

(defmacro vhdl-point (position)
  ;; Returns the value of point at certain commonly referenced POSITIONs.
  ;; POSITION can be one of the following symbols:
  ;;
  ;; bol  -- beginning of line
  ;; eol  -- end of line
  ;; bod  -- beginning of defun
  ;; boi  -- back to indentation
  ;; eoi  -- last whitespace on line
  ;; ionl -- indentation of next line
  ;; iopl -- indentation of previous line
  ;; bonl -- beginning of next line
  ;; bopl -- beginning of previous line
  ;;
  ;; This function does not modify point or mark.
  (or (and (eq 'quote (car-safe position))
	   (null (cdr (cdr position))))
      (error "bad buffer position requested: %s" position))
  (setq position (nth 1 position))
  (` (let ((here (point)))
       (,@ (cond
	    ((eq position 'bol)  '((beginning-of-line)))
	    ((eq position 'eol)  '((end-of-line)))
	    ((eq position 'bod)  '((save-match-data
				     (vhdl-beginning-of-defun))))
	    ((eq position 'boi)  '((back-to-indentation)))
	    ((eq position 'eoi)  '((end-of-line)(skip-chars-backward " \t")))
	    ((eq position 'bonl) '((forward-line 1)))
	    ((eq position 'bopl) '((forward-line -1)))
	    ((eq position 'iopl)
	     '((forward-line -1)
	       (back-to-indentation)))
	    ((eq position 'ionl)
	     '((forward-line 1)
	       (back-to-indentation)))
	    (t (error "unknown buffer position requested: %s" position))
	    ))
       (prog1
	   (point)
	 (goto-char here))
       ;; workaround for an Emacs18 bug -- blech! Well, at least it
       ;; doesn't hurt for v19
       (,@ nil)
       )))

(defmacro vhdl-safe (&rest body)
  ;; safely execute BODY, return nil if an error occurred
  (` (condition-case nil
	 (progn (,@ body))
       (error nil))))

(defmacro vhdl-add-syntax (symbol &optional relpos)
  ;; a simple macro to append the syntax in symbol to the syntax list.
  ;; try to increase performance by using this macro
  (` (setq vhdl-syntactic-context
	   (cons (cons (, symbol) (, relpos)) vhdl-syntactic-context))))

(defmacro vhdl-has-syntax (symbol)
  ;; a simple macro to return check the syntax list.
  ;; try to increase performance by using this macro
  (` (assoc (, symbol) vhdl-syntactic-context)))

;; Syntactic element offset manipulation:

(defun vhdl-read-offset (langelem)
  ;; read new offset value for LANGELEM from minibuffer. return a
  ;; legal value only
  (let ((oldoff (format "%s" (cdr-safe (assq langelem vhdl-offsets-alist))))
	(errmsg "Offset must be int, func, var, or one of +, -, ++, --: ")
	(prompt "Offset: ")
	offset input interned)
    (while (not offset)
      (setq input (read-string prompt oldoff)
	    offset (cond ((string-equal "+" input) '+)
			 ((string-equal "-" input) '-)
			 ((string-equal "++" input) '++)
			 ((string-equal "--" input) '--)
			 ((string-match "^-?[0-9]+$" input)
			  (string-to-int input))
			 ((fboundp (setq interned (intern input)))
			  interned)
			 ((boundp interned) interned)
			 ;; error, but don't signal one, keep trying
			 ;; to read an input value
			 (t (ding)
			    (setq prompt errmsg)
			    nil))))
    offset))

(defun vhdl-set-offset (symbol offset &optional add-p)
  "Change the value of a syntactic element symbol in `vhdl-offsets-alist'.
SYMBOL is the syntactic element symbol to change and OFFSET is the new
offset for that syntactic element.  Optional ADD says to add SYMBOL to
`vhdl-offsets-alist' if it doesn't already appear there."
  (interactive
   (let* ((langelem
	   (intern (completing-read
		    (concat "Syntactic symbol to change"
			    (if current-prefix-arg " or add" "")
			    ": ")
		    (mapcar
		     (function
		      (lambda (langelem)
			(cons (format "%s" (car langelem)) nil)))
		     vhdl-offsets-alist)
		    nil (not current-prefix-arg)
		    ;; initial contents tries to be the last element
		    ;; on the syntactic analysis list for the current
		    ;; line
		    (let* ((syntax (vhdl-get-syntactic-context))
			   (len (length syntax))
			   (ic (format "%s" (car (nth (1- len) syntax)))))
		      (if (memq 'v19 vhdl-emacs-features)
			  (cons ic 0)
			ic))
		    )))
	  (offset (vhdl-read-offset langelem)))
     (list langelem offset current-prefix-arg)))
  ;; sanity check offset
  (or (eq offset '+)
      (eq offset '-)
      (eq offset '++)
      (eq offset '--)
      (integerp offset)
      (fboundp offset)
      (boundp offset)
      (error "Offset must be int, func, var, or one of +, -, ++, --: %s"
	     offset))
  (let ((entry (assq symbol vhdl-offsets-alist)))
    (if entry
	(setcdr entry offset)
      (if add-p
	  (setq vhdl-offsets-alist (cons (cons symbol offset) vhdl-offsets-alist))
	(error "%s is not a valid syntactic symbol." symbol))))
  (vhdl-keep-region-active))

(defun vhdl-set-style (style &optional local)
  "Set vhdl-mode variables to use one of several different indentation styles.
STYLE is a string representing the desired style and optional LOCAL is
a flag which, if non-nil, means to make the style variables being
changed buffer local, instead of the default, which is to set the
global variables.  Interactively, the flag comes from the prefix
argument.  The styles are chosen from the `vhdl-style-alist' variable."
  (interactive (list (completing-read "Use which VHDL indentation style? "
                                      vhdl-style-alist nil t)
		     current-prefix-arg))
  (let ((vars (cdr (assoc style vhdl-style-alist))))
    (or vars
	(error "Invalid VHDL indentation style `%s'" style))
    ;; set all the variables
    (mapcar
     (function
      (lambda (varentry)
	(let ((var (car varentry))
	      (val (cdr varentry)))
	  (and local
	       (make-local-variable var))
	  ;; special case for vhdl-offsets-alist
	  (if (not (eq var 'vhdl-offsets-alist))
	      (set var val)
	    ;; reset vhdl-offsets-alist to the default value first
	    (setq vhdl-offsets-alist (copy-alist vhdl-offsets-alist-default))
	    ;; now set the langelems that are different
	    (mapcar
	     (function
	      (lambda (langentry)
		(let ((langelem (car langentry))
		      (offset (cdr langentry)))
		  (vhdl-set-offset langelem offset)
		  )))
	     val))
	  )))
     vars))
  (vhdl-keep-region-active))

(defun vhdl-get-offset (langelem)
  ;; Get offset from LANGELEM which is a cons cell of the form:
  ;; (SYMBOL . RELPOS).  The symbol is matched against
  ;; vhdl-offsets-alist and the offset found there is either returned,
  ;; or added to the indentation at RELPOS.  If RELPOS is nil, then
  ;; the offset is simply returned.
  (let* ((symbol (car langelem))
	 (relpos (cdr langelem))
	 (match  (assq symbol vhdl-offsets-alist))
	 (offset (cdr-safe match)))
    ;; offset can be a number, a function, a variable, or one of the
    ;; symbols + or -
    (cond
     ((not match)
      (if vhdl-strict-syntax-p
	  (error "don't know how to indent a %s" symbol)
	(setq offset 0
	      relpos 0)))
     ((eq offset '+)  (setq offset vhdl-basic-offset))
     ((eq offset '-)  (setq offset (- vhdl-basic-offset)))
     ((eq offset '++) (setq offset (* 2 vhdl-basic-offset)))
     ((eq offset '--) (setq offset (* 2 (- vhdl-basic-offset))))
     ((and (not (numberp offset))
	   (fboundp offset))
      (setq offset (funcall offset langelem)))
     ((not (numberp offset))
      (setq offset (eval offset)))
     )
    (+ (if (and relpos
		(< relpos (vhdl-point 'bol)))
	   (save-excursion
	     (goto-char relpos)
	     (current-column))
	 0)
       offset)))

;; Syntactic support functions:

;; Returns `comment' if in a comment, `string' if in a string literal,
;; or nil if not in a literal at all.  Optional LIM is used as the
;; backward limit of the search.  If omitted, or nil, (point-min) is
;; used.

(defun vhdl-in-literal (&optional lim)
  ;; Determine if point is in a VHDL literal.
  (save-excursion
    (let* ((lim (or lim (point-min)))
	   (state (parse-partial-sexp lim (point))))
      (cond
       ((nth 3 state) 'string)
       ((nth 4 state) 'comment)
       (t nil)))
    ))

;; This is the best we can do in Win-Emacs.
(defun vhdl-win-il (&optional lim)
  ;; Determine if point is in a VHDL literal
  (save-excursion
    (let* ((here (point))
	   (state nil)
	   (match nil)
	   (lim  (or lim (vhdl-point 'bod))))
      (goto-char lim )
      (while (< (point) here)
	(setq match
	      (and (re-search-forward "--\\|[\"']"
				      here 'move)
		   (buffer-substring (match-beginning 0) (match-end 0))))
	(setq state
	      (cond
	       ;; no match
	       ((null match) nil)
	       ;; looking at the opening of a VHDL style comment
	       ((string= "--" match)
		(if (<= here (progn (end-of-line) (point))) 'comment))
	       ;; looking at the opening of a double quote string
	       ((string= "\"" match)
		(if (not (save-restriction
			   ;; this seems to be necessary since the
			   ;; re-search-forward will not work without it
			   (narrow-to-region (point) here)
			   (re-search-forward
			    ;; this regexp matches a double quote
			    ;; which is preceded by an even number
			    ;; of backslashes, including zero
			    "\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\"" here 'move)))
		    'string))
	       ;; looking at the opening of a single quote string
	       ((string= "'" match)
		(if (not (save-restriction
			   ;; see comments from above
			   (narrow-to-region (point) here)
			   (re-search-forward
			    ;; this matches a single quote which is
			    ;; preceded by zero or two backslashes.
			    "\\([^\\]\\|^\\)\\(\\\\\\\\\\)?'"
			    here 'move)))
		    'string))
	       (t nil)))
	) ; end-while
      state)))

(and (memq 'Win-Emacs vhdl-emacs-features)
     (fset 'vhdl-in-literal 'vhdl-win-il))

;; Skipping of "syntactic whitespace".  Syntactic whitespace is
;; defined as lexical whitespace or comments.  Search no farther back
;; or forward than optional LIM.  If LIM is omitted, (point-min) is
;; used for backward skipping, (point-max) is used for forward
;; skipping.

(defun vhdl-forward-syntactic-ws (&optional lim)
  ;; Forward skip of syntactic whitespace.
  (save-restriction
    (let* ((lim (or lim (point-max)))
	   (here lim)
	   (hugenum (point-max)))
      (narrow-to-region lim (point))
      (while (/= here (point))
	(setq here (point))
	(forward-comment hugenum))
      )))

;; This is the best we can do in Win-Emacs.
(defun vhdl-win-fsws (&optional lim)
  ;; Forward skip syntactic whitespace for Win-Emacs.
  (let ((lim (or lim (point-max)))
	stop)
    (while (not stop)
      (skip-chars-forward " \t\n\r\f" lim)
      (cond
       ;; vhdl comment
       ((looking-at "--") (end-of-line))
       ;; none of the above
       (t (setq stop t))
       ))))

(and (memq 'Win-Emacs vhdl-emacs-features)
     (fset 'vhdl-forward-syntactic-ws 'vhdl-win-fsws))

(defun vhdl-backward-syntactic-ws (&optional lim)
  ;; Backward skip over syntactic whitespace.
  (save-restriction
    (let* ((lim (or lim (point-min)))
	   (here lim)
	   (hugenum (- (point-max))))
      (if (< lim (point))
	  (progn
	    (narrow-to-region lim (point))
	    (while (/= here (point))
	      (setq here (point))
	      (forward-comment hugenum)
	      )))
      )))

;; This is the best we can do in Win-Emacs.
(defun vhdl-win-bsws (&optional lim)
  ;; Backward skip syntactic whitespace for Win-Emacs.
  (let ((lim (or lim (vhdl-point 'bod)))
	stop)
    (while (not stop)
      (skip-chars-backward " \t\n\r\f" lim)
      (cond
       ;; vhdl comment
       ((eq (vhdl-in-literal lim) 'comment)
	(skip-chars-backward "^-" lim)
	(skip-chars-backward "-" lim)
	(while (not (or (and (= (following-char) ?-)
			     (= (char-after (1+ (point))) ?-))
			(<= (point) lim)))
	  (skip-chars-backward "^-" lim)
	  (skip-chars-backward "-" lim)))
       ;; none of the above
       (t (setq stop t))
       ))))

(and (memq 'Win-Emacs vhdl-emacs-features)
    (fset 'vhdl-backward-syntactic-ws 'vhdl-win-bsws))

;; Functions to help finding the correct indentation column:

(defun vhdl-first-word (point)
  "If the keyword at POINT is at boi, then return (current-column) at
that point, else nil."
  (save-excursion
    (and (goto-char point)
	 (eq (point) (vhdl-point 'boi))
	 (current-column))))

(defun vhdl-last-word (point)
  "If the keyword at POINT is at eoi, then return (current-column) at
that point, else nil."
  (save-excursion
    (and (goto-char point)
	 (save-excursion (or (eq (progn (forward-sexp) (point))
				 (vhdl-point 'eoi))
			     (looking-at "\\s-*\\(--\\)?")))
	 (current-column))))

;; Core syntactic evaluation functions:

(defconst vhdl-libunit-re
  "\\b\\(architecture\\|configuration\\|entity\\|package\\)\\b[^_]")

(defun vhdl-libunit-p ()
  (and
   (save-excursion
     (forward-sexp)
     (skip-chars-forward " \t\n")
     (not (looking-at "is\\b[^_]")))
   (save-excursion
     (backward-sexp)
     (and (not (looking-at "use\\b[^_]"))
	  (progn
	    (forward-sexp)
	    (vhdl-forward-syntactic-ws)
	    (/= (following-char) ?:))))
   ))

(defconst vhdl-defun-re
  "\\b\\(architecture\\|block\\|configuration\\|entity\\|package\\|process\\|procedure\\|function\\)\\b[^_]")

(defun vhdl-defun-p ()
  (save-excursion
    (if (looking-at "block\\|process")
	;; "block", "process":
	(save-excursion
	  (backward-sexp)
	  (not (looking-at "end\\s-+\\w")))
      ;; "architecture", "configuration", "entity",
      ;; "package", "procedure", "function":
      t)))

(defun vhdl-corresponding-defun ()
  "If the word at the current position corresponds to a \"defun\"
keyword, then return a string that can be used to find the
corresponding \"begin\" keyword, else return nil."
  (save-excursion
    (and (looking-at vhdl-defun-re)
	 (vhdl-defun-p)
	 (if (looking-at "block\\|process")
	     ;; "block", "process":
	     (buffer-substring (match-beginning 0) (match-end 0))
	   ;; "architecture", "configuration", "entity", "package",
	   ;; "procedure", "function":
	   "is"))))

(defconst vhdl-begin-fwd-re
  "\\b\\(is\\|begin\\|block\\|component\\|generate\\|then\\|else\\|loop\\|process\\|units\\|record\\|for\\)\\b\\([^_]\\|\\'\\)"
  "A regular expression for searching forward that matches all known
\"begin\" keywords.")

(defconst vhdl-begin-bwd-re
  "\\b\\(is\\|begin\\|block\\|component\\|generate\\|then\\|else\\|loop\\|process\\|units\\|record\\|for\\)\\b[^_]"
  "A regular expression for searching backward that matches all known
\"begin\" keywords.")

(defun vhdl-begin-p (&optional lim)
  "Return t if we are looking at a real \"begin\" keyword.
Assumes that the caller will make sure that we are looking at
vhdl-begin-fwd-re, and are not inside a literal, and that we are not in
the middle of an identifier that just happens to contain a \"begin\"
keyword."
  (cond
   ;; "[architecture|case|configuration|entity|package|
   ;;   procedure|function] ... is":
   ((and (looking-at "i")
	 (save-excursion
	   ;; Skip backward over first sexp (needed to skip over a
	   ;; procedure interface list, and is harmless in other
	   ;; situations).  Note that we need "return" in the
	   ;; following search list so that we don't run into
	   ;; semicolons in the function interface list.
	   (backward-sexp)
	   (let (foundp)
	     (while (and (not foundp)
			 (re-search-backward
			  ";\\|\\b\\(architecture\\|case\\|configuration\\|entity\\|package\\|procedure\\|return\\|is\\|begin\\|process\\|block\\)\\b[^_]"
			  lim 'move))
	       (if (or (= (preceding-char) ?_)
		       (vhdl-in-literal lim))
		   (backward-char)
		 (setq foundp t))))
	   (and (/= (following-char) ?\;)
		(not (looking-at "is\\|begin\\|process\\|block")))))
    t)
   ;; "begin", "then":
   ((looking-at "be\\|t")
    t)
   ;; "else":
   ((and (looking-at "e")
	 ;; make sure that the "else" isn't inside a
	 ;; conditional signal assignment.
	 (save-excursion
	   (re-search-backward ";\\|\\bwhen\\b[^_]" lim 'move)
	   (or (eq (following-char) ?\;)
	       (eq (point) lim))))
    t)
   ;; "block", "generate", "loop", "process",
   ;; "units", "record":
   ((and (looking-at "bl\\|[glpur]")
	 (save-excursion
	   (backward-sexp)
	   (not (looking-at "end\\s-+\\w"))))
    t)
   ;; "component":
   ((and (looking-at "c")
	 (save-excursion
	   (backward-sexp)
	   (not (looking-at "end\\s-+\\w")))
	 ;; look out for the dreaded entity class in an attribute
	 (save-excursion
	   (vhdl-backward-syntactic-ws lim)
	   (/= (preceding-char) ?:)))
    t)
   ;; "for" (inside configuration declaration):
   ((and (looking-at "f")
	 (save-excursion
	   (backward-sexp)
	   (not (looking-at "end\\s-+\\w")))
	 (vhdl-has-syntax 'configuration))
    t)
   ))

(defun vhdl-corresponding-mid (&optional lim)
  (cond
   ((looking-at "is\\|block\\|process")
    "begin")
   ((looking-at "then")
    "<else>")
   (t
    "end")))

(defun vhdl-corresponding-end (&optional lim)
  "If the word at the current position corresponds to a \"begin\"
keyword, then return a vector containing enough information to find
the corresponding \"end\" keyword, else return nil.  The keyword to
search forward for is aref 0.  The column in which the keyword must
appear is aref 1 or nil if any column is suitable.
Assumes that the caller will make sure that we are not in the middle
of an identifier that just happens to contain a \"begin\" keyword."
  (save-excursion
    (and (looking-at vhdl-begin-fwd-re)
	 (/= (preceding-char) ?_)
	 (not (vhdl-in-literal lim))
	 (vhdl-begin-p lim)
	 (cond
	  ;; "is", "generate", "loop":
	  ((looking-at "[igl]")
	   (vector "end"
		   (and (vhdl-last-word (point))
			(or (vhdl-first-word (point))
			    (save-excursion
			      (vhdl-beginning-of-statement-1 lim)
			      (vhdl-backward-skip-label lim)
			      (vhdl-first-word (point)))))))
	  ;; "begin", "else", "for":
	  ((looking-at "be\\|[ef]")
	   (vector "end"
		   (and (vhdl-last-word (point))
			(or (vhdl-first-word (point))
			    (save-excursion
			      (vhdl-beginning-of-statement-1 lim)
			      (vhdl-backward-skip-label lim)
			      (vhdl-first-word (point)))))))
	  ;; "component", "units", "record":
	  ((looking-at "[cur]")
	   ;; The first end found will close the block
	   (vector "end" nil))
	  ;; "block", "process":
	  ((looking-at "bl\\|p")
	   (vector "end"
		   (or (vhdl-first-word (point))
		       (save-excursion
			 (vhdl-beginning-of-statement-1 lim)
			 (vhdl-backward-skip-label lim)
			 (vhdl-first-word (point))))))
	  ;; "then":
	  ((looking-at "t")
	   (vector "elsif\\|else\\|end\\s-+if"
		   (and (vhdl-last-word (point))
			(or (vhdl-first-word (point))
			    (save-excursion
			      (vhdl-beginning-of-statement-1 lim)
			      (vhdl-backward-skip-label lim)
			      (vhdl-first-word (point)))))))
	  ))))

(defconst vhdl-end-fwd-re "\\b\\(end\\|else\\|elsif\\)\\b\\([^_]\\|\\'\\)")

(defconst vhdl-end-bwd-re "\\b\\(end\\|else\\|elsif\\)\\b[^_]")

(defun vhdl-end-p (&optional lim)
  "Return t if we are looking at a real \"end\" keyword.
Assumes that the caller will make sure that we are looking at
vhdl-end-fwd-re, and are not inside a literal, and that we are not in
the middle of an identifier that just happens to contain an \"end\"
keyword."
  (or (not (looking-at "else"))
      ;; make sure that the "else" isn't inside a conditional signal
      ;; assignment.
      (save-excursion
	(re-search-backward ";\\|\\bwhen\\b[^_]" lim 'move)
	(or (eq (following-char) ?\;)
	    (eq (point) lim)))))

(defun vhdl-corresponding-begin (&optional lim)
  "If the word at the current position corresponds to an \"end\"
keyword, then return a vector containing enough information to find
the corresponding \"begin\" keyword, else return nil.  The keyword to
search backward for is aref 0.  The column in which the keyword must
appear is aref 1 or nil if any column is suitable.  The supplementary
keyword to search forward for is aref 2 or nil if this is not
required.  If aref 3 is t, then the \"begin\" keyword may be found in
the middle of a statement.
Assumes that the caller will make sure that we are not in the middle
of an identifier that just happens to contain an \"end\" keyword."
  (save-excursion
    (let (pos)
      (if (and (looking-at vhdl-end-fwd-re)
	       (not (vhdl-in-literal lim))
	       (vhdl-end-p lim))
	  (if (looking-at "el")
	      ;; "else", "elsif":
	      (vector "if\\|elsif" (vhdl-first-word (point)) "then" nil)
	    ;; "end ...":
	    (setq pos (point))
	    (forward-sexp)
	    (skip-chars-forward " \t\n")
	    (cond
	     ;; "end if":
	     ((looking-at "if\\b[^_]")
	      (vector "else\\|elsif\\|if"
		      (vhdl-first-word pos)
		      "else\\|then" nil))
	     ;; "end component":
	     ((looking-at "component\\b[^_]")
	      (vector (buffer-substring (match-beginning 1)
					(match-end 1))
		      (vhdl-first-word pos)
		      nil nil))
	     ;; "end units", "end record":
	     ((looking-at "\\(units\\|record\\)\\b[^_]")
	      (vector (buffer-substring (match-beginning 1)
					(match-end 1))
		      (vhdl-first-word pos)
		      nil t))
	     ;; "end block", "end process":
	     ((looking-at "\\(block\\|process\\)\\b[^_]")
	      (vector "begin" (vhdl-first-word pos) nil nil))
	     ;; "end case":
	     ((looking-at "case\\b[^_]")
	      (vector "case" (vhdl-first-word pos) "is" nil))
	     ;; "end generate":
	     ((looking-at "generate\\b[^_]")
	      (vector "generate\\|for\\|if"
		      (vhdl-first-word pos)
		      "generate" nil))
	     ;; "end loop":
	     ((looking-at "loop\\b[^_]")
	      (vector "loop\\|while\\|for"
		      (vhdl-first-word pos)
		      "loop" nil))
	     ;; "end for" (inside configuration declaration):
	     ((looking-at "for\\b[^_]")
	      (vector "for" (vhdl-first-word pos) nil nil))
	     ;; "end [id]":
	     (t
	      (vector "begin\\|architecture\\|configuration\\|entity\\|package\\|procedure\\|function"
		      (vhdl-first-word pos)
		      ;; return an alist of (statement . keyword) mappings
		      '(
			;; "begin ... end [id]":
			("begin"          . nil)
			;; "architecture ... is ... begin ... end [id]":
			("architecture"   . "is")
			;; "configuration ... is ... end [id]":
			("configuration"  . "is")
			;; "entity ... is ... end [id]":
			("entity"         . "is")
			;; "package ... is ... end [id]":
			("package"        . "is")
			;; "procedure ... is ... begin ... end [id]":
			("procedure"      . "is")
			;; "function ... is ... begin ... end [id]":
			("function"       . "is")
			)
		      nil))
	     ))) ; "end ..."
      )))

(defconst vhdl-leader-re
  "\\b\\(block\\|component\\|process\\|for\\)\\b[^_]")

(defun vhdl-end-of-leader ()
  (save-excursion
    (cond ((looking-at "block\\|process")
	   (if (save-excursion
		 (forward-sexp)
		 (skip-chars-forward " \t\n")
		 (= (following-char) ?\())
	       (forward-sexp 2)
	     (forward-sexp))
	   (point))
	  ((looking-at "component")
	   (forward-sexp 2)
	   (point))
	  ((looking-at "for")
	   (forward-sexp 2)
	   (skip-chars-forward " \t\n")
	   (while (looking-at "[,:(]")
	     (forward-sexp)
	     (skip-chars-forward " \t\n"))
	   (point))
	  (t nil)
	  )))

(defconst vhdl-trailer-re
  "\\b\\(is\\|then\\|generate\\|loop\\)\\b[^_]")

(defconst vhdl-statement-fwd-re
  "\\b\\(if\\|for\\|while\\)\\b\\([^_]\\|\\'\\)"
  "A regular expression for searching forward that matches all known
\"statement\" keywords.")

(defconst vhdl-statement-bwd-re
  "\\b\\(if\\|for\\|while\\)\\b[^_]"
  "A regular expression for searching backward that matches all known
\"statement\" keywords.")

(defun vhdl-statement-p (&optional lim)
  "Return t if we are looking at a real \"statement\" keyword.
Assumes that the caller will make sure that we are looking at
vhdl-statement-fwd-re, and are not inside a literal, and that we are not in
the middle of an identifier that just happens to contain a \"statement\"
keyword."
  (cond
   ;; "for" ... "generate":
   ((and (looking-at "f")
	 ;; Make sure it's the start of a parameter specification.
	 (save-excursion
	   (forward-sexp 2)
	   (skip-chars-forward " \t\n")
	   (looking-at "in\\b[^_]"))
	 ;; Make sure it's not an "end for".
	 (save-excursion
	   (backward-sexp)
	   (not (looking-at "end\\s-+\\w"))))
    t)
   ;; "if" ... "then", "if" ... "generate", "if" ... "loop":
   ((and (looking-at "i")
	 ;; Make sure it's not an "end if".
	 (save-excursion
	   (backward-sexp)
	   (not (looking-at "end\\s-+\\w"))))
    t)
   ;; "while" ... "loop":
   ((looking-at "w")
    t)
   ))

(defconst vhdl-case-alternative-re "when[( \t\n][^;=>]+=>"
  "Regexp describing a case statement alternative key.")

(defun vhdl-case-alternative-p (&optional lim)
  "Return t if we are looking at a real case alternative.
Assumes that the caller will make sure that we are looking at
vhdl-case-alternative-re, and are not inside a literal, and that
we are not in the middle of an identifier that just happens to
contain a \"when\" keyword."
  (save-excursion
    (let (foundp)
      (while (and (not foundp)
		  (re-search-backward ";\\|<=" lim 'move))
	(if (or (= (preceding-char) ?_)
		(vhdl-in-literal lim))
	    (backward-char)
	  (setq foundp t)))
      (or (eq (following-char) ?\;)
	  (eq (point) lim)))
    ))

;; Core syntactic movement functions:

(defconst vhdl-b-t-b-re
  (concat vhdl-begin-bwd-re "\\|" vhdl-end-bwd-re))

(defun vhdl-backward-to-block (&optional lim)
  "Move backward to the previous \"begin\" or \"end\" keyword."
  (let (foundp)
    (while (and (not foundp)
		(re-search-backward vhdl-b-t-b-re lim 'move))
      (if (or (= (preceding-char) ?_)
	      (vhdl-in-literal lim))
	  (backward-char)
	(cond
	 ;; "begin" keyword:
	 ((and (looking-at vhdl-begin-fwd-re)
	       (/= (preceding-char) ?_)
	       (vhdl-begin-p lim))
	  (setq foundp 'begin))
	 ;; "end" keyword:
	 ((and (looking-at vhdl-end-fwd-re)
	       (/= (preceding-char) ?_)
	       (vhdl-end-p lim))
	  (setq foundp 'end))
	 ))
      )
    foundp
    ))

(defun vhdl-forward-sexp (&optional count lim)
  "Move forward across one balanced expression (sexp).
With COUNT, do it that many times."
  (interactive "p")
  (let ((count (or count 1))
	(case-fold-search t)
	end-vec target)
    (save-excursion
      (while (> count 0)
	;; skip whitespace
	(skip-chars-forward " \t\n")
	;; Check for an unbalanced "end" keyword
	(if (and (looking-at vhdl-end-fwd-re)
		 (/= (preceding-char) ?_)
		 (not (vhdl-in-literal lim))
		 (vhdl-end-p lim)
		 (not (looking-at "else")))
	    (error
	     "Containing expression ends prematurely in vhdl-forward-sexp"))
	;; If the current keyword is a "begin" keyword, then find the
	;; corresponding "end" keyword.
	(if (setq end-vec (vhdl-corresponding-end lim))
	    (let (
		  ;; end-re is the statement keyword to search for
		  (end-re
		   (concat "\\b\\(" (aref end-vec 0) "\\)\\b\\([^_]\\|\\'\\)"))
		  ;; column is either the statement keyword target column
		  ;; or nil
		  (column (aref end-vec 1))
		  (eol (vhdl-point 'eol))
		  foundp literal placeholder)
	      ;; Look for the statement keyword.
	      (while (and (not foundp)
			  (re-search-forward end-re nil t)
			  (setq placeholder (match-end 1))
			  (goto-char (match-beginning 0)))
		;; If we are in a literal, or not in the right target
		;; column and not on the same line as the begin, then
		;; try again.
		(if (or (and column
			     (/= (current-indentation) column)
			     (> (point) eol))
			(= (preceding-char) ?_)
			(setq literal (vhdl-in-literal lim)))
		    (if (eq literal 'comment)
			(end-of-line)
		      (forward-char))
		  ;; An "else" keyword corresponds to both the opening brace
		  ;; of the following sexp and the closing brace of the
		  ;; previous sexp.
		  (if (not (looking-at "else"))
		      (goto-char placeholder))
		  (setq foundp t))
		)
	      (if (not foundp)
		  (error "Unbalanced keywords in vhdl-forward-sexp"))
	      )
	  ;; If the current keyword is not a "begin" keyword, then just
	  ;; perform the normal forward-sexp.
	  (forward-sexp)
	  )
	(setq count (1- count))
	)
      (setq target (point)))
    (goto-char target)
    nil))

(defun vhdl-backward-sexp (&optional count lim)
  "Move backward across one balanced expression (sexp).
With COUNT, do it that many times.  LIM bounds any required backward
searches."
  (interactive "p")
  (let ((count (or count 1))
	(case-fold-search t)
	begin-vec target)
    (save-excursion
      (while (> count 0)
	;; Perform the normal backward-sexp, unless we are looking at
	;; "else" - an "else" keyword corresponds to both the opening brace
	;; of the following sexp and the closing brace of the previous sexp.
	(if (and (looking-at "else\\b\\([^_]\\|\\'\\)")
		 (/= (preceding-char) ?_)
		 (not (vhdl-in-literal lim)))
	    nil
	  (backward-sexp)
	  (if (and (looking-at vhdl-begin-fwd-re)
		   (/= (preceding-char) ?_)
		   (not (vhdl-in-literal lim))
		   (vhdl-begin-p lim))
	      (error "Containing expression ends prematurely in vhdl-backward-sexp")))
	;; If the current keyword is an "end" keyword, then find the
	;; corresponding "begin" keyword.
	(if (and (setq begin-vec (vhdl-corresponding-begin lim))
		 (/= (preceding-char) ?_))
	    (let (
		  ;; begin-re is the statement keyword to search for
		  (begin-re
		   (concat "\\b\\(" (aref begin-vec 0) "\\)\\b[^_]"))
		  ;; column is either the statement keyword target column
		  ;; or nil
		  (column (aref begin-vec 1))
		  ;; internal-p controls where the statement keyword can
		  ;; be found.
		  (internal-p (aref begin-vec 3))
		  (last-backward (point)) last-forward
		  foundp literal keyword)
	      ;; Look for the statement keyword.
	      (while (and (not foundp)
			  (re-search-backward begin-re lim t)
			  (setq keyword
				(buffer-substring (match-beginning 1)
						  (match-end 1))))
		;; If we are in a literal or in the wrong column,
		;; then try again.
		(if (or (and column
			     (and (/= (current-indentation) column)
				  ;; possibly accept current-column as
				  ;; well as current-indentation.
				  (or (not internal-p)
				      (/= (current-column) column))))
			(= (preceding-char) ?_)
			(vhdl-in-literal lim))
		    (backward-char)
		  ;; If there is a supplementary keyword, then
		  ;; search forward for it.
		  (if (and (setq begin-re (aref begin-vec 2))
			   (or (not (listp begin-re))
			       ;; If begin-re is an alist, then find the
			       ;; element corresponding to the actual
			       ;; keyword that we found.
			       (progn
				 (setq begin-re
				       (assoc keyword begin-re))
				 (and begin-re
				      (setq begin-re (cdr begin-re))))))
		      (and
		       (setq begin-re
			     (concat "\\b\\(" begin-re "\\)\\b[^_]"))
		       (save-excursion
			 (setq last-forward (point))
			 ;; Look for the supplementary keyword
			 ;; (bounded by the backward search start
			 ;; point).
			 (while (and (not foundp)
				     (re-search-forward begin-re
							last-backward t)
				     (goto-char (match-beginning 1)))
			   ;; If we are in a literal, then try again.
			   (if (or (= (preceding-char) ?_)
				   (setq literal
					 (vhdl-in-literal last-forward)))
			       (if (eq literal 'comment)
				   (goto-char
				    (min (vhdl-point 'eol) last-backward))
				 (forward-char))
			     ;; We have found the supplementary keyword.
			     ;; Save the position of the keyword in foundp.
			     (setq foundp (point)))
			   )
			 foundp)
		       ;; If the supplementary keyword was found, then
		       ;; move point to the supplementary keyword.
		       (goto-char foundp))
		    ;; If there was no supplementary keyword, then
		    ;; point is already at the statement keyword.
		    (setq foundp t)))
		) ; end of the search for the statement keyword
	      (if (not foundp)
		  (error "Unbalanced keywords in vhdl-backward-sexp"))
	      ))
	(setq count (1- count))
	)
      (setq target (point)))
    (goto-char target)
    nil))

(defun vhdl-backward-up-list (&optional count limit)
  "Move backward out of one level of blocks.
With argument, do this that many times."
  (interactive "p")
  (let ((count (or count 1))
	target)
    (save-excursion
      (while (> count 0)
	(if (looking-at vhdl-defun-re)
	    (error "Unbalanced blocks"))
	(vhdl-backward-to-block limit)
	(setq count (1- count)))
      (setq target (point)))
    (goto-char target)))

(defun vhdl-end-of-defun (&optional count)
  "Move forward to the end of a VHDL defun."
  (interactive)
  (let ((case-fold-search t))
    (vhdl-beginning-of-defun)
    (if (not (looking-at "block\\|process"))
	(re-search-forward "\\bis\\b"))
    (vhdl-forward-sexp)))

(defun vhdl-mark-defun ()
  "Put mark at end of this \"defun\", point at beginning."
  (interactive)
  (let ((case-fold-search t))
    (push-mark)
    (vhdl-beginning-of-defun)
    (push-mark)
    (if (not (looking-at "block\\|process"))
	(re-search-forward "\\bis\\b"))
    (vhdl-forward-sexp)
    (exchange-point-and-mark)))

(defun vhdl-beginning-of-libunit ()
  "Move backward to the beginning of a VHDL library unit.
Returns the location of the corresponding begin keyword, unless search
stops due to beginning or end of buffer."
  ;; Note that if point is between the "libunit" keyword and the
  ;; corresponding "begin" keyword, then that libunit will not be
  ;; recognised, and the search will continue backwards.  If point is
  ;; at the "begin" keyword, then the defun will be recognised.  The
  ;; returned point is at the first character of the "libunit" keyword.
  (let ((last-forward (point))
	(last-backward
	 ;; Just in case we are actually sitting on the "begin"
	 ;; keyword, allow for the keyword and an extra character,
	 ;; as this will be used when looking forward for the
	 ;; "begin" keyword.
	 (save-excursion (forward-word 1) (1+ (point))))
	foundp literal placeholder)
    ;; Find the "libunit" keyword.
    (while (and (not foundp)
		(re-search-backward vhdl-libunit-re nil 'move))
      ;; If we are in a literal, or not at a real libunit, then try again.
      (if (or (= (preceding-char) ?_)
	      (vhdl-in-literal (point-min))
	      (not (vhdl-libunit-p)))
	  (backward-char)
	;; Find the corresponding "begin" keyword.
	(setq last-forward (point))
	(while (and (not foundp)
		    (re-search-forward "\\bis\\b[^_]" last-backward t)
		    (setq placeholder (match-beginning 0)))
	  (if (or (= (preceding-char) ?_)
		  (setq literal (vhdl-in-literal last-forward)))
	      ;; It wasn't a real keyword, so keep searching.
	      (if (eq literal 'comment)
		  (goto-char
		   (min (vhdl-point 'eol) last-backward))
		(forward-char))
	    ;; We have found the begin keyword, loop will exit.
	    (setq foundp placeholder)))
	;; Go back to the libunit keyword
	(goto-char last-forward)))
    foundp))

(defun vhdl-beginning-of-defun (&optional count)
  "Move backward to the beginning of a VHDL defun.
With argument, do it that many times.
Returns the location of the corresponding begin keyword, unless search
stops due to beginning or end of buffer."
  ;; Note that if point is between the "defun" keyword and the
  ;; corresponding "begin" keyword, then that defun will not be
  ;; recognised, and the search will continue backwards.  If point is
  ;; at the "begin" keyword, then the defun will be recognised.  The
  ;; returned point is at the first character of the "defun" keyword.
  (interactive "p")
  (let ((count (or count 1))
	(case-fold-search t)
	(last-forward (point))
	foundp)
    (while (> count 0)
      (setq foundp nil)
      (goto-char last-forward)
      (let ((last-backward
	     ;; Just in case we are actually sitting on the "begin"
	     ;; keyword, allow for the keyword and an extra character,
	     ;; as this will be used when looking forward for the
	     ;; "begin" keyword.
	     (save-excursion (forward-word 1) (1+ (point))))
	    begin-string literal)
	(while (and (not foundp)
		    (re-search-backward vhdl-defun-re nil 'move))
	  ;; If we are in a literal, then try again.
	  (if (or (= (preceding-char) ?_)
		  (vhdl-in-literal (point-min)))
	      (backward-char)
	    (if (setq begin-string (vhdl-corresponding-defun))
		;; This is a real defun keyword.
		;; Find the corresponding "begin" keyword.
		;; Look for the begin keyword.
		(progn
		  ;; Save the search start point.
		  (setq last-forward (point))
		  (while (and (not foundp)
			      (search-forward begin-string last-backward t))
		    (if (or (= (preceding-char) ?_)
			    (save-match-data
			      (setq literal (vhdl-in-literal last-forward))))
			;; It wasn't a real keyword, so keep searching.
			(if (eq literal 'comment)
			    (goto-char
			     (min (vhdl-point 'eol) last-backward))
			  (forward-char))
		      ;; We have found the begin keyword, loop will exit.
		      (setq foundp (match-beginning 0)))
		    )
		  ;; Go back to the defun keyword
		  (goto-char last-forward)) ; end search for begin keyword
	      ))
	  ) ; end of the search for the defun keyword
	)
      (setq count (1- count))
      )
    (vhdl-keep-region-active)
    foundp))

(defun vhdl-beginning-of-statement (&optional count lim)
  "Go to the beginning of the innermost VHDL statement.
With prefix arg, go back N - 1 statements.  If already at the
beginning of a statement then go to the beginning of the preceding
one.  If within a string or comment, or next to a comment (only
whitespace between), move by sentences instead of statements.

When called from a program, this function takes 2 optional args: the
prefix arg, and a buffer position limit which is the farthest back to
search."
  (interactive "p")
  (let ((count (or count 1))
	(case-fold-search t)
	(lim (or lim (point-min)))
	(here (point))
	state)
    (save-excursion
      (goto-char lim)
      (setq state (parse-partial-sexp (point) here nil nil)))
    (if (and (interactive-p)
	     (or (nth 3 state)
		 (nth 4 state)
		 (looking-at (concat "[ \t]*" comment-start-skip))))
	(forward-sentence (- count))
      (while (> count 0)
	(vhdl-beginning-of-statement-1 lim)
	(setq count (1- count))))
    ;; its possible we've been left up-buf of lim
    (goto-char (max (point) lim))
    )
  (vhdl-keep-region-active))

(defconst vhdl-e-o-s-re
  (concat ";\\|" vhdl-begin-fwd-re "\\|" vhdl-statement-fwd-re))

(defun vhdl-end-of-statement ()
  "Very simple implementation."
  (interactive)
  (re-search-forward vhdl-e-o-s-re))

(defconst vhdl-b-o-s-re
  (concat ";\\|\(\\|\)\\|\\bwhen\\b[^_]\\|"
	  vhdl-begin-bwd-re "\\|" vhdl-statement-bwd-re))

(defun vhdl-beginning-of-statement-1 (&optional lim)
  ;; move to the start of the current statement, or the previous
  ;; statement if already at the beginning of one.
  (let ((lim (or lim (point-min)))
	(here (point))
	(pos (point))
	donep)
    ;; go backwards one balanced expression, but be careful of
    ;; unbalanced paren being reached
    (if (not (vhdl-safe (progn (backward-sexp) t)))
	(progn
	  (backward-up-list 1)
	  (forward-char)
	  (vhdl-forward-syntactic-ws here)
	  (setq donep t)))
    (while (and (not donep)
		(not (bobp))
		;; look backwards for a statement boundary
		(re-search-backward vhdl-b-o-s-re lim 'move))
      (if (or (= (preceding-char) ?_)
	      (vhdl-in-literal lim))
	  (backward-char)
	(cond
	 ;; If we are looking at an open paren, then stop after it
	 ((eq (following-char) ?\()
	  (forward-char)
	  (vhdl-forward-syntactic-ws here)
	  (setq donep t))
	 ;; If we are looking at a close paren, then skip it
	 ((eq (following-char) ?\))
	  (forward-char)
	  (setq pos (point))
	  (backward-sexp)
	  (if (< (point) lim)
	      (progn (goto-char pos)
		     (vhdl-forward-syntactic-ws here)
		     (setq donep t))))
	 ;; If we are looking at a semicolon, then stop
	 ((eq (following-char) ?\;)
	  (progn
	    (forward-char)
	    (vhdl-forward-syntactic-ws here)
	    (setq donep t)))
	 ;; If we are looking at a "begin", then stop
	 ((and (looking-at vhdl-begin-fwd-re)
	       (/= (preceding-char) ?_)
	       (vhdl-begin-p nil))
	  ;; If it's a leader "begin", then find the
	  ;; right place
	  (if (looking-at vhdl-leader-re)
	      (save-excursion
		;; set a default stop point at the begin
		(setq pos (point))
		;; is the start point inside the leader area ?
		(goto-char (vhdl-end-of-leader))
		(vhdl-forward-syntactic-ws here)
		(if (< (point) here)
		    ;; start point was not inside leader area
		    ;; set stop point at word after leader
		    (setq pos (point))))
	    (forward-word 1)
	    (vhdl-forward-syntactic-ws here)
	    (setq pos (point)))
	  (goto-char pos)
	  (setq donep t))
	 ;; If we are looking at a "statement", then stop
	 ((and (looking-at vhdl-statement-fwd-re)
	       (/= (preceding-char) ?_)
	       (vhdl-statement-p nil))
	  (setq donep t))
	 ;; If we are looking at a case alternative key, then stop
 	 ((and (looking-at vhdl-case-alternative-re)
 	       (vhdl-case-alternative-p lim))
	  (save-excursion
	    ;; set a default stop point at the when
	    (setq pos (point))
	    ;; is the start point inside the case alternative key ?
	    (looking-at vhdl-case-alternative-re)
	    (goto-char (match-end 0))
	    (vhdl-forward-syntactic-ws here)
	    (if (< (point) here)
		;; start point was not inside the case alternative key
		;; set stop point at word after case alternative keyleader
		(setq pos (point))))
	  (goto-char pos)
	  (setq donep t))
	 ;; Bogus find, continue
	 (t
	  (backward-char)))))
    ))

;; Defuns for calculating the current syntactic state:

(defun vhdl-get-library-unit (bod placeholder)
  ;; If there is an enclosing library unit at bod, with it's \"begin\"
  ;; keyword at placeholder, then return the library unit type.
  (let ((here (vhdl-point 'bol)))
    (if (save-excursion
	  (goto-char placeholder)
	  (vhdl-safe (vhdl-forward-sexp 1 bod))
	  (<= here (point)))
	(save-excursion
	  (goto-char bod)
	  (cond
	   ((looking-at "e") 'entity)
	   ((looking-at "a") 'architecture)
	   ((looking-at "c") 'configuration)
	   ((looking-at "p")
	    (save-excursion
	      (goto-char bod)
	      (forward-sexp)
	      (vhdl-forward-syntactic-ws here)
	      (if (looking-at "body\\b[^_]")
		  'package-body 'package))))))
    ))

(defun vhdl-get-block-state (&optional lim)
  ;; Finds and records all the closest opens.
  ;; lim is the furthest back we need to search (it should be the
  ;; previous libunit keyword).
  (let ((here (point))
	(lim (or lim (point-min)))
	keyword sexp-start sexp-mid sexp-end
	preceding-sexp containing-sexp
	containing-begin containing-mid containing-paren)
    (save-excursion
      ;; Find the containing-paren, and use that as the limit
      (if (setq containing-paren
		(save-restriction
		  (narrow-to-region lim (point))
		  (vhdl-safe (scan-lists (point) -1 1))))
	  (setq lim containing-paren))
      ;; Look backwards for "begin" and "end" keywords.
      (while (and (> (point) lim)
		  (not containing-sexp))
	(setq keyword (vhdl-backward-to-block lim))
	(cond
	 ((eq keyword 'begin)
	  ;; Found a "begin" keyword
	  (setq sexp-start (point))
	  (setq sexp-mid (vhdl-corresponding-mid lim))
	  (setq sexp-end (vhdl-safe
			  (save-excursion
			    (vhdl-forward-sexp 1 lim) (point))))
	  (if (and sexp-end (<= sexp-end here))
	      ;; we want to record this sexp, but we only want to
	      ;; record the last-most of any of them before here
	      (or preceding-sexp
		  (setq preceding-sexp sexp-start))
	    ;; we're contained in this sexp so put sexp-start on
	    ;; front of list
	    (setq containing-sexp sexp-start)
	    (setq containing-mid sexp-mid)
	    (setq containing-begin t)))
	 ((eq keyword 'end)
	  ;; Found an "end" keyword
	  (forward-sexp)
	  (setq sexp-end (point))
	  (setq sexp-mid nil)
	  (setq sexp-start
		(or (vhdl-safe (vhdl-backward-sexp 1 lim) (point))
		    (progn (backward-sexp) (point))))
	  ;; we want to record this sexp, but we only want to
	  ;; record the last-most of any of them before here
	  (or preceding-sexp
	      (setq preceding-sexp sexp-start)))
	 )))
    ;; Check if the containing-paren should be the containing-sexp
    (if (and containing-paren
	     (or (null containing-sexp)
		 (< containing-sexp containing-paren)))
	(setq containing-sexp containing-paren
	      preceding-sexp nil
	      containing-begin nil
	      containing-mid nil))
    (vector containing-sexp preceding-sexp containing-begin containing-mid)
    ))


(defconst vhdl-s-c-a-re
  (concat vhdl-case-alternative-re "\\|" vhdl-case-header-key))

(defun vhdl-skip-case-alternative (&optional lim)
  ;; skip forward over case/when bodies, with optional maximal
  ;; limit. if no next case alternative is found, nil is returned and point
  ;; is not moved
  (let ((lim (or lim (point-max)))
	(here (point))
	donep foundp)
    (while (and (< (point) lim)
		(not donep))
      (if (and (re-search-forward vhdl-s-c-a-re lim 'move)
	       (save-match-data
		 (not (vhdl-in-literal)))
	       (/= (match-beginning 0) here))
	  (progn
	    (goto-char (match-beginning 0))
	    (cond
	     ((and (looking-at "case")
		   (re-search-forward "\\bis[^_]" lim t))
	      (backward-sexp)
	      (vhdl-forward-sexp))
	     (t
	      (setq donep t
		    foundp t))))))
    (if (not foundp)
	(goto-char here))
    foundp))

(defun vhdl-backward-skip-label (&optional lim)
  ;; skip backward over a label, with optional maximal
  ;; limit. if label is not found, nil is returned and point
  ;; is not moved
  (let ((lim (or lim (point-min)))
	placeholder)
    (if (save-excursion
	  (vhdl-backward-syntactic-ws lim)
	  (and (eq (preceding-char) ?:)
	       (progn
		 (backward-sexp)
		 (setq placeholder (point))
		 (looking-at vhdl-label-key))))
	(goto-char placeholder))
    ))

(defun vhdl-forward-skip-label (&optional lim)
  ;; skip forward over a label, with optional maximal
  ;; limit. if label is not found, nil is returned and point
  ;; is not moved
  (let ((lim (or lim (point-max))))
    (if (looking-at vhdl-label-key)
	(progn
	  (goto-char (match-end 0))
	  (vhdl-forward-syntactic-ws lim)))
    ))

(defun vhdl-get-syntactic-context ()
  ;; guess the syntactic description of the current line of VHDL code.
  (save-excursion
    (save-restriction
      (beginning-of-line)
      (let* ((indent-point (point))
	     (case-fold-search t)
	     vec literal containing-sexp preceding-sexp
	     containing-begin containing-mid containing-leader
	     char-before-ip char-after-ip begin-after-ip end-after-ip
	     placeholder lim library-unit
	    )

	;; Reset the syntactic context
	(setq vhdl-syntactic-context nil)

	(save-excursion
	  ;; Move to the start of the previous library unit, and
	  ;; record the position of the "begin" keyword.
	  (setq placeholder (vhdl-beginning-of-libunit))
	  ;; The position of the "libunit" keyword gives us a gross
	  ;; limit point.
	  (setq lim (point))
	  )

	;; If there is a previous library unit, and we are enclosed by
	;; it, then set the syntax accordingly.
	(and placeholder
	     (setq library-unit (vhdl-get-library-unit lim placeholder))
	     (vhdl-add-syntax library-unit lim))

	;; Find the surrounding state.
	(if (setq vec (vhdl-get-block-state lim))
	    (progn
	      (setq containing-sexp (aref vec 0))
	      (setq preceding-sexp (aref vec 1))
	      (setq containing-begin (aref vec 2))
	      (setq containing-mid (aref vec 3))
	      ))

	;; set the limit on the farthest back we need to search
	(setq lim (if containing-sexp
		      (save-excursion
			(goto-char containing-sexp)
			;; set containing-leader if required
			(if (looking-at vhdl-leader-re)
			    (setq containing-leader (vhdl-end-of-leader)))
			(vhdl-point 'bol))
		    (point-min)))

	;; cache char before and after indent point, and move point to
	;; the most likely position to perform the majority of tests
	(goto-char indent-point)
	(skip-chars-forward " \t")
	(setq literal (vhdl-in-literal lim))
	(setq char-after-ip (following-char))
	(setq begin-after-ip (and
			      (not literal)
			      (looking-at vhdl-begin-fwd-re)
			      (vhdl-begin-p)))
	(setq end-after-ip (and
			    (not literal)
			    (looking-at vhdl-end-fwd-re)
			    (vhdl-end-p)))
	(vhdl-backward-syntactic-ws lim)
	(setq char-before-ip (preceding-char))
	(goto-char indent-point)
	(skip-chars-forward " \t")

	;; now figure out syntactic qualities of the current line
	(cond
	 ;; CASE 1: in a string or comment.
	 ((memq literal '(string comment))
	  (vhdl-add-syntax literal (vhdl-point 'bopl)))
	 ;; CASE 2: Line is at top level.
	 ((null containing-sexp)
	  ;; Find the point to which indentation will be relative
	  (save-excursion
	    (if (null preceding-sexp)
		;; CASE 2X.1
		;; no preceding-sexp -> use the preceding statement
		(vhdl-beginning-of-statement-1 lim)
	      ;; CASE 2X.2
	      ;; if there is a preceding-sexp then indent relative to it
	      (goto-char preceding-sexp)
	      ;; if not at boi, then the block-opening keyword is
	      ;; probably following a label, so we need a different
	      ;; relpos
	      (if (/= (point) (vhdl-point 'boi))
		  ;; CASE 2X.3
		  (vhdl-beginning-of-statement-1 lim)))
	    ;; v-b-o-s could have left us at point-min
	    (and (bobp)
		 ;; CASE 2X.4
		 (vhdl-forward-syntactic-ws indent-point))
	    (setq placeholder (point)))
	  (cond
	   ;; CASE 2A : we are looking at a block-open
	   (begin-after-ip
	    (vhdl-add-syntax 'block-open placeholder))
	   ;; CASE 2B: we are looking at a block-close
	   (end-after-ip
	    (vhdl-add-syntax 'block-close placeholder))
	   ;; CASE 2C: we are looking at a top-level statement
	   ((progn
	      (vhdl-backward-syntactic-ws lim)
	      (or (bobp)
		  (= (preceding-char) ?\;)))
	    (vhdl-add-syntax 'statement placeholder))
	   ;; CASE 2D: we are looking at a top-level statement-cont
	   (t
	    (vhdl-beginning-of-statement-1 lim)
	    ;; v-b-o-s could have left us at point-min
	    (and (bobp)
		 ;; CASE 2D.1
		 (vhdl-forward-syntactic-ws indent-point))
	    (vhdl-add-syntax 'statement-cont (point)))
	   )) ; end CASE 2
	 ;; CASE 3: line is inside parentheses.  Most likely we are
	 ;; either in a subprogram argument (interface) list, or a
	 ;; continued expression containing parentheses.
	 ((null containing-begin)
	  (vhdl-backward-syntactic-ws containing-sexp)
	  (cond
	   ;; CASE 3A: we are looking at the arglist closing paren
	   ((eq char-after-ip ?\))
	    (goto-char containing-sexp)
	    (vhdl-add-syntax 'arglist-close (vhdl-point 'boi)))
	   ;; CASE 3B: we are looking at the first argument in an empty
	   ;; argument list.
	   ((eq char-before-ip ?\()
	    (goto-char containing-sexp)
	    (vhdl-add-syntax 'arglist-intro (vhdl-point 'boi)))
	   ;; CASE 3C: we are looking at an arglist continuation line,
	   ;; but the preceding argument is on the same line as the
	   ;; opening paren.  This case includes multi-line
	   ;; expression paren groupings.
	   ((and (save-excursion
		   (goto-char (1+ containing-sexp))
		   (skip-chars-forward " \t")
		   (not (eolp))
		   (not (looking-at "--")))
		 (save-excursion
		   (vhdl-beginning-of-statement-1 containing-sexp)
		   (skip-chars-backward " \t(")
		   (<= (point) containing-sexp)))
	    (goto-char containing-sexp)
	    (vhdl-add-syntax 'arglist-cont-nonempty (vhdl-point 'boi)))
	   ;; CASE 3D: we are looking at just a normal arglist
	   ;; continuation line
	   (t (vhdl-beginning-of-statement-1 containing-sexp)
	      (vhdl-forward-syntactic-ws indent-point)
	      (vhdl-add-syntax 'arglist-cont (vhdl-point 'boi)))
	   ))
	 ;; CASE 4: A block mid open
	 ((and begin-after-ip
	       (looking-at containing-mid))
	  (goto-char containing-sexp)
	  ;; If the \"begin\" keyword is a trailer, then find v-b-o-s
	  (if (looking-at vhdl-trailer-re)
	      ;; CASE 4.1
	      (progn (forward-sexp) (vhdl-beginning-of-statement-1 nil)))
	  (vhdl-backward-skip-label (vhdl-point 'boi))
	  (vhdl-add-syntax 'block-open (point)))
	 ;; CASE 5: block close brace
	 (end-after-ip
	  (goto-char containing-sexp)
	  ;; If the \"begin\" keyword is a trailer, then find v-b-o-s
	  (if (looking-at vhdl-trailer-re)
	      ;; CASE 5.1
	      (progn (forward-sexp) (vhdl-beginning-of-statement-1 nil)))
	  (vhdl-backward-skip-label (vhdl-point 'boi))
	  (vhdl-add-syntax 'block-close (point)))
	 ;; CASE 6: A continued statement
	 ((and (/= char-before-ip ?\;)
	       ;; check it's not a trailer begin keyword, or a begin
	       ;; keyword immediately following a label.
	       (not (and begin-after-ip
			 (or (looking-at vhdl-trailer-re)
			     (save-excursion
			       (vhdl-backward-skip-label containing-sexp)))))
	       ;; check it's not a statement keyword
	       (not (and (looking-at vhdl-statement-fwd-re)
			 (vhdl-statement-p)))
	       ;; see if the b-o-s is before the indent point
	       (> indent-point
		  (save-excursion
		    (vhdl-beginning-of-statement-1 containing-sexp)
		    ;; If we ended up after a leader, then this will
		    ;; move us forward to the start of the first
		    ;; statement.  Note that a containing sexp here is
		    ;; always a keyword, not a paren, so this will
		    ;; have no effect if we hit the containing-sexp.
		    (vhdl-forward-syntactic-ws indent-point)
		    (setq placeholder (point))))
	       ;; check it's not a block-intro
	       (/= placeholder containing-sexp)
	       ;; check it's not a case block-intro
	       (save-excursion
		 (goto-char placeholder)
		 (or (not (looking-at vhdl-case-alternative-re))
		     (> (match-end 0) indent-point))))
	  ;; Make placeholder skip a label, but only if it puts us
	  ;; before the indent point at the start of a line.
	  (let ((new placeholder))
	    (if (and (> indent-point
			(save-excursion
			  (goto-char placeholder)
			  (vhdl-forward-skip-label indent-point)
			  (setq new (point))))
		     (save-excursion
		       (goto-char new)
		       (eq new (progn (back-to-indentation) (point)))))
		(setq placeholder new)))
	  (vhdl-add-syntax 'statement-cont placeholder)
	  (if begin-after-ip
	      (vhdl-add-syntax 'block-open)))
	 ;; Statement. But what kind?
	 ;; CASE 7: A case alternative key
	 ((and (looking-at vhdl-case-alternative-re)
	       (vhdl-case-alternative-p containing-sexp))
	  ;; for a case alternative key, we set relpos to the first
	  ;; non-whitespace char on the line containing the "case"
	  ;; keyword.
	  (goto-char containing-sexp)
	  ;; If the \"begin\" keyword is a trailer, then find v-b-o-s
	  (if (looking-at vhdl-trailer-re)
	      (progn (forward-sexp) (vhdl-beginning-of-statement-1 nil)))
	  (vhdl-add-syntax 'case-alternative (vhdl-point 'boi)))
	 ;; CASE 8: statement catchall
	 (t
	  ;; we know its a statement, but we need to find out if it is
	  ;; the first statement in a block
	  (if containing-leader
	      (goto-char containing-leader)
	    (goto-char containing-sexp)
	    ;; Note that a containing sexp here is always a keyword,
	    ;; not a paren, so skip over the keyword.
	    (forward-sexp))
	  ;; move to the start of the first statement
	  (vhdl-forward-syntactic-ws indent-point)
	  (setq placeholder (point))
	  ;; we want to ignore case alternatives keys when skipping forward
	  (let (incase-p)
	    (while (looking-at vhdl-case-alternative-re)
	      (setq incase-p (point))
	      ;; we also want to skip over the body of the
	      ;; case/when statement if that doesn't put us at
	      ;; after the indent-point
	      (while (vhdl-skip-case-alternative indent-point))
	      ;; set up the match end
	      (looking-at vhdl-case-alternative-re)
	      (goto-char (match-end 0))
	      ;; move to the start of the first case alternative statement
	      (vhdl-forward-syntactic-ws indent-point)
	      (setq placeholder (point)))
	    (cond
	     ;; CASE 8A: we saw a case/when statement so we must be
	     ;; in a switch statement.  find out if we are at the
	     ;; statement just after a case alternative key
	     ((and incase-p
		   (= (point) indent-point))
	      ;; relpos is the "when" keyword
	      (vhdl-add-syntax 'statement-case-intro incase-p))
	     ;; CASE 8B: any old statement
	     ((< (point) indent-point)
	      ;; relpos is the first statement of the block
	      (vhdl-add-syntax 'statement placeholder)
	      (if begin-after-ip
		  (vhdl-add-syntax 'block-open)))
	     ;; CASE 8C: first statement in a block
	     (t
	      (goto-char containing-sexp)
	      ;; If the \"begin\" keyword is a trailer, then find v-b-o-s
	      (if (looking-at vhdl-trailer-re)
		  (progn (forward-sexp) (vhdl-beginning-of-statement-1 nil)))
	      (vhdl-backward-skip-label (vhdl-point 'boi))
	      (vhdl-add-syntax 'statement-block-intro (point))
	      (if begin-after-ip
		  (vhdl-add-syntax 'block-open)))
	     )))
	 )

	;; now we need to look at any modifiers
	(goto-char indent-point)
	(skip-chars-forward " \t")
	(if (looking-at "--")
	    (vhdl-add-syntax 'comment))
	;; return the syntax
	vhdl-syntactic-context))))

;; Standard indentation line-ups:

(defun vhdl-lineup-arglist (langelem)
  ;; lineup the current arglist line with the arglist appearing just
  ;; after the containing paren which starts the arglist.
  (save-excursion
    (let* ((containing-sexp
	    (save-excursion
	      ;; arglist-cont-nonempty gives relpos ==
	      ;; to boi of containing-sexp paren. This
	      ;; is good when offset is +, but bad
	      ;; when it is vhdl-lineup-arglist, so we
	      ;; have to special case a kludge here.
	      (if (memq (car langelem) '(arglist-intro arglist-cont-nonempty))
		  (progn
		    (beginning-of-line)
		    (backward-up-list 1)
		    (skip-chars-forward " \t" (vhdl-point 'eol)))
		(goto-char (cdr langelem)))
	      (point)))
	   (cs-curcol (save-excursion
			(goto-char (cdr langelem))
			(current-column))))
      (if (save-excursion
	    (beginning-of-line)
	    (looking-at "[ \t]*)"))
	  (progn (goto-char (match-end 0))
		 (backward-sexp)
		 (forward-char)
		 (vhdl-forward-syntactic-ws)
		 (- (current-column) cs-curcol))
	(goto-char containing-sexp)
	(or (eolp)
	    (let ((eol (vhdl-point 'eol))
		  (here (progn
			  (forward-char)
			  (skip-chars-forward " \t")
			  (point))))
	      (vhdl-forward-syntactic-ws)
	      (if (< (point) eol)
		  (goto-char here))))
	(- (current-column) cs-curcol)
	))))

(defun vhdl-lineup-arglist-intro (langelem)
  ;; lineup an arglist-intro line to just after the open paren
  (save-excursion
    (let ((cs-curcol (save-excursion
		       (goto-char (cdr langelem))
		       (current-column)))
	  (ce-curcol (save-excursion
		       (beginning-of-line)
		       (backward-up-list 1)
		       (skip-chars-forward " \t" (vhdl-point 'eol))
		       (current-column))))
      (- ce-curcol cs-curcol -1))))

(defun vhdl-lineup-comment (langelem)
  ;; support old behavior for comment indentation. we look at
  ;; vhdl-comment-only-line-offset to decide how to indent comment
  ;; only-lines
  (save-excursion
    (back-to-indentation)
    ;; at or to the right of comment-column
    (if (>= (current-column) comment-column)
	(vhdl-comment-indent)
      ;; otherwise, indent as specified by vhdl-comment-only-line-offset
      (if (not (bolp))
	  (or (car-safe vhdl-comment-only-line-offset)
	      vhdl-comment-only-line-offset)
	(or (cdr-safe vhdl-comment-only-line-offset)
	    (car-safe vhdl-comment-only-line-offset)
	    -1000			;jam it against the left side
	    )))))

(defun vhdl-lineup-statement-cont (langelem)
  ;; line up statement-cont after the assignment operator
  (save-excursion
    (let* ((relpos (cdr langelem))
	   (assignp (save-excursion
		     (goto-char (vhdl-point 'boi))
		     (and (re-search-forward "\\(<\\|:\\)="
					     (vhdl-point 'eol) t)
			  (- (point) (vhdl-point 'boi)))))
	   (curcol (progn
		     (goto-char relpos)
		     (current-column)))
	   foundp)
      (while (and (not foundp)
		  (< (point) (vhdl-point 'eol)))
	(re-search-forward "\\(<\\|:\\)=\\|(" (vhdl-point 'eol) 'move)
	(if (vhdl-in-literal (cdr langelem))
	    (forward-char)
	  (if (= (preceding-char) ?\()
	      ;; skip over any parenthesized expressions
	      (goto-char (min (vhdl-point 'eol)
			      (scan-lists (point) 1 1)))
	    ;; found an assignment operator (not at eol)
	    (setq foundp (not (looking-at "\\s-*$"))))))
      (if (not foundp)
	  ;; there's no assignment operator on the line
	  vhdl-basic-offset
	;; calculate indentation column after assign and ws, unless
	;; our line contains an assignment operator
	(if (not assignp)
	    (progn
	      (forward-char)
	      (skip-chars-forward " \t")
	      (setq assignp 0)))
	(- (current-column) assignp curcol))
      )))

;; ############################################################################
;; Indentation commands

(defun vhdl-tab (&optional pre-arg)
  "If preceeding character is part of a word then dabbrev-expand,
else if right of non whitespace on line then tab-to-tab-stop,
else if last command was a tab or return then dedent one step,
else indent `correctly'."
  (interactive "*P")
  (cond ((= (char-syntax (preceding-char)) ?w)
	 (let ((case-fold-search nil)) (dabbrev-expand pre-arg)))
	((> (current-column) (current-indentation))
	 (tab-to-tab-stop))
	((and (or (eq last-command 'vhdl-tab)
		  (eq last-command 'vhdl-return))
	      (/= 0 (current-indentation)))
	 (backward-delete-char-untabify vhdl-basic-offset nil))
	((vhdl-indent-line))
        )
  (setq this-command 'vhdl-tab)
  )

(defun vhdl-untab ()
  "Delete backwards to previous tab stop."
  (interactive)
  (backward-delete-char-untabify vhdl-basic-offset nil)
  )

(defun vhdl-return ()
  "newline-and-indent or indent-new-comment-line if in comment and preceding
character is a space."
  (interactive)
  (if (and (= (preceding-char) ? ) (vhdl-in-comment-p))
      (indent-new-comment-line)
    (newline-and-indent)
    )
  )

(defun vhdl-indent-line ()
  "Indent the current line as VHDL code. Returns the amount of
indentation change."
  (interactive)
  (let* ((syntax (vhdl-get-syntactic-context))
	 (pos (- (point-max) (point)))
	 (indent (apply '+ (mapcar 'vhdl-get-offset syntax)))
	 (shift-amt  (- (current-indentation) indent)))
    (and vhdl-echo-syntactic-information-p
	 (message "syntax: %s, indent= %d" syntax indent))
    (if (zerop shift-amt)
	nil
      (delete-region (vhdl-point 'bol) (vhdl-point 'boi))
      (beginning-of-line)
      (indent-to indent))
    (if (< (point) (vhdl-point 'boi))
	(back-to-indentation)
      ;; If initial point was within line's indentation, position after
      ;; the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos)))
      )
    (run-hooks 'vhdl-special-indent-hook)
    shift-amt))

(defun vhdl-indent-buffer ()
  "Indent whole buffer as VHDL code."
  (interactive)
  (indent-region (point-min) (point-max) nil)
  )

(defun vhdl-indent-sexp (&optional endpos)
  "Indent each line of the list starting just after point.
If optional arg ENDPOS is given, indent each line, stopping when
ENDPOS is encountered."
  (interactive)
  (save-excursion
    (let ((beg (point))
	  (end (progn
		 (vhdl-forward-sexp nil endpos)
		 (point))))
      (indent-region beg end nil))))

;; ############################################################################
;; Miscellaneous commands

(defun vhdl-show-syntactic-information ()
  "Show syntactic information for current line."
  (interactive)
  (message "syntactic analysis: %s" (vhdl-get-syntactic-context))
  (vhdl-keep-region-active))

;; Verification and regression functions:

(defun vhdl-regress-line (&optional arg)
  "Check syntactic information for current line."
  (interactive "P")
  (let ((expected (save-excursion
		    (end-of-line)
		    (if (search-backward " -- ((" (vhdl-point 'bol) t)
			(progn
			  (forward-char 4)
			  (read (current-buffer))))))
	(actual (vhdl-get-syntactic-context))
	(expurgated))
    ;; remove the library unit symbols
    (mapcar
     (function
      (lambda (elt)
	(if (memq (car elt) '(entity configuration package
				     package-body architecture))
	    nil
	  (setq expurgated (append expurgated (list elt))))))
     actual)
    (if (and (not arg) expected (listp expected))
	(if (not (equal expected expurgated))
	    (error "Should be: %s, is: %s" expected expurgated))
      (save-excursion
	(beginning-of-line)
	(if (not (looking-at "^\\s-*\\(--.*\\)?$"))
	    (progn
	      (end-of-line)
	      (if (search-backward " -- ((" (vhdl-point 'bol) t)
		  (kill-line))
	      (insert " -- ")
	      (insert (format "%s" expurgated)))))))
  (vhdl-keep-region-active))


;; ############################################################################
;; Alignment
;; ############################################################################

(defvar vhdl-align-alist
  '(
    ;; after some keywords
    (vhdl-mode "\\<\\(alias\\|constant\\|signal\\|subtype\\|type\\|variable\\)[ \t]"
	       "\\<\\(alias\\|constant\\|signal\\|subtype\\|type\\|variable\\)\\([ \t]+\\)" 2)
    ;; before ':'
    (vhdl-mode ":[^=]" "[^ \t]\\([ \t]*\\):[^=]")
    ;; after ':'
    (vhdl-mode ":[^=]" ":\\([ \t]*\\)[^=]" 1)
    ;; after direction specifications
    (vhdl-mode ":[ \t]*\\(in\\|out\\|inout\\|buffer\\)\\>"
	       ":[ \t]*\\(in\\|out\\|inout\\|buffer\\)\\([ \t]+\\)" 2)
    ;; before "<=", "=>", and ":="
    (vhdl-mode "<=" "[^ \t]\\([ \t]*\\)<=" 1)
    (vhdl-mode "=>" "[^ \t]\\([ \t]*\\)=>" 1)
    (vhdl-mode ":=" "[^ \t]\\([ \t]*\\):=" 1)
    ;; after "<=", "=>", and ":="
    (vhdl-mode "<=" "<=\\([ \t]*\\)" 1)
    (vhdl-mode "=>" "=>\\([ \t]*\\)" 1)
    (vhdl-mode ":=" ":=\\([ \t]*\\)" 1)
    ;; before some keywords
    (vhdl-mode "[ \t]after\\>" "[^ \t]\\([ \t]+\\)after\\>" 1)
    (vhdl-mode "[ \t]\\(fs\\|ps\\|ns\\|us\\|ms\\|sec\\|min\\|hr\\)\\>"
	       "[^ \t]\\([ \t]+\\)\\(fs\\|ps\\|ns\\|us\\|ms\\|sec\\|min\\|hr\\)\\>" 1)
    (vhdl-mode "[ \t]when\\>" "[^ \t]\\([ \t]+\\)when\\>" 1)
    (vhdl-mode "[ \t]else\\>" "[^ \t]\\([ \t]+\\)else\\>" 1)
    (vhdl-mode "[ \t]is\\>" "[^ \t]\\([ \t]+\\)is\\>" 1)
    (vhdl-mode "[ \t]of\\>" "[^ \t]\\([ \t]+\\)of\\>" 1)
    (vhdl-mode "[ \t]use\\>" "[^ \t]\\([ \t]+\\)use\\>" 1)
    ;; before comments (two steps required for correct insertion of two spaces)
    (vhdl-mode "--" "[^ \t]\\([ \t]*\\)--" 1)
    (vhdl-mode "--" "[^ \t][ \t]\\([ \t]*\\)--" 1)
    )
  "The format of this alist is
  (MODES [or MODE] REGEXP ALIGN-PATTERN SUBEXP).
It is searched in order.  If REGEXP is found anywhere in the first
line of a region to be aligned, ALIGN-PATTERN will be used for that
region.  ALIGN-PATTERN must include the whitespace to be expanded or
contracted. It may also provide regexps for the text surrounding the
whitespace. SUBEXP specifies which sub-expression of
ALIGN-PATTERN matches the white space to be expanded/contracted.")

(defvar vhdl-align-try-all-clauses t
  "If REGEXP is not found on the first line of the region that clause
is ignored. If this variable is non-nil, then the clause is tried anyway.")

(defun vhdl-align (begin end spacing &optional alignment-list quick)
  "Attempt to align a range of lines based on the content of the
lines.  The definition of 'alignment-list' determines the matching
order and the manner in which the lines are aligned. If ALIGNMENT-LIST
is not specified 'vhdl-align-alist' is used. If QUICK is non-nil, no
indentation is done before aligning."
  (interactive "r\np")
  (if (not alignment-list)
      (setq alignment-list vhdl-align-alist))
  (if (not spacing)
      (setq spacing 1))
  (save-excursion
    (let (bol indent)
      (goto-char end)
      (setq end (point-marker))
      (goto-char begin)
      (setq bol
            (setq begin (progn (beginning-of-line) (point))))
      (untabify bol end)
      (if quick
          nil
        (indent-region bol end nil))))
  (let ((copy (copy-alist alignment-list)))
    (while copy
      (save-excursion
        (goto-char begin)
        (let (element
              (eol (save-excursion (progn (end-of-line) (point)))))
          (setq element (nth 0 copy))
          (if (and (or (and (listp (car element))
                            (memq major-mode (car element)))
                       (eq major-mode (car element)))
                   (or vhdl-align-try-all-clauses
                       (re-search-forward (car (cdr element)) eol t)))
              (progn
                (vhdl-align-region begin end (car (cdr (cdr element)))
                              (car (cdr (cdr (cdr element)))) spacing)))
          (setq copy (cdr copy)))))))

(defun vhdl-align-region (begin end match &optional substr spacing)
  "Align a range of lines from BEGIN to END.  The regular expression
MATCH must match exactly one fields: the whitespace to be
contracted/expanded.  The alignment column will equal the
rightmost column of the widest whitespace block. SPACING is
the amount of extra spaces to add to the calculated maximum required.
SPACING defaults to 1 so that at least one space is inserted after
the token in MATCH."
  (if (not spacing)
      (setq spacing 1))
  (if (not substr)
      (setq substr 1))
  (save-excursion
    (let (distance (max 0) (lines 0) bol eol width)
      ;; Determine the greatest whitespace distance to the alignment
      ;; character
      (goto-char begin)
      (setq eol (progn (end-of-line) (point))
            bol (setq begin (progn (beginning-of-line) (point))))
      (while (< bol end)
        (save-excursion
          (if (re-search-forward match eol t)
              (progn
                (setq distance (- (match-beginning substr) bol))
                (if (> distance max)
                    (setq max distance)))))
        (forward-line)
        (setq bol (point)
              eol (save-excursion
                    (end-of-line)
                    (point)))
        (setq lines (1+ lines)))
      ;; Now insert enough maxs to push each assignment operator to
      ;; the same column.  We need to use 'lines' as a counter, since
      ;; the location of the mark may change
      (goto-char (setq bol begin))
      (setq eol (save-excursion
                  (end-of-line)
                  (point)))
      (while (> lines 0)
        (if (re-search-forward match eol t)
            (progn
              (setq width (- (match-end substr) (match-beginning substr)))
              (setq distance (- (match-beginning substr) bol))
              (goto-char (match-beginning substr))
              (delete-char width)
              (insert-char ?  (+ (- max distance) spacing))))
        (beginning-of-line)
        (forward-line)
        (setq bol (point)
              eol (save-excursion
                    (end-of-line)
                    (point)))
        (setq lines (1- lines))
        ))))

(defun vhdl-align-comment-region (begin end spacing)
  "Aligns inline comments within a region relative to first comment."
  (interactive "r\nP")
  (vhdl-align begin end (or spacing 2)
	      `((vhdl-mode "--" "[^ \t]\\([ \t]*\\)--" 1)) t))

(defun vhdl-align-noindent-region (begin end spacing)
  "Align without indentation."
  (interactive "r\nP")
  (vhdl-align begin end spacing nil t)
  )


;; ############################################################################
;; VHDL electrification
;; ############################################################################

;; ############################################################################
;;  Stuttering

(defun vhdl-stutter-mode-caps (count)
  "Double first letters of a word replaced by a single capital of the letter."
  (interactive "p")
  (if vhdl-stutter-mode
      (if (and
	   (= (preceding-char) last-input-char) ; doubled
	   (or (= (point) 2)		; beginning of buffer
	       (/= (char-syntax (char-after (- (point) 2))) ?w) ;not mid-word
	       (<               (char-after (- (point) 2))  ?A))) ;alfa-numeric
	  (progn (delete-char -1) (insert-char (- last-input-char 32) count))
	(self-insert-command count))
    (self-insert-command count)
    ))

(defun vhdl-stutter-mode-close-bracket (count) " ']' --> ')', ')]' --> ']'"
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1))
      (progn
	(if (= (preceding-char) 41)	; close-paren
	    (progn (delete-char -1) (insert-char 93 1)) ; close-bracket
	  (insert-char 41 1)		; close-paren
	  )
	(blink-matching-open))
    (self-insert-command count)
    ))

(defun vhdl-stutter-mode-semicolon (count) " ';;' --> ' : ', ': ;' --> ' := '"
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1))
      (progn
	(cond ((= (preceding-char) last-input-char)
	       (progn (delete-char -1)
		      (if (not (eq (preceding-char) ? )) (insert " "))
		      (insert ": ")))
	      ((and
		(eq last-command 'vhdl-stutter-mode-colon) (= (preceding-char) ? ))
	       (progn (delete-char -1) (insert "= ")))
	      (t
	       (insert-char 59 1))	; semi-colon
	      )
	(setq this-command 'vhdl-stutter-mode-colon))
    (self-insert-command count)
    ))

(defun vhdl-stutter-mode-open-bracket (count) " '[' --> '(', '([' --> '['"
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1))
      (if (= (preceding-char) 40)	; open-paren
	  (progn (delete-char -1) (insert-char 91 1)) ; open-bracket
	(insert-char 40 1))		; open-paren
    (self-insert-command count)
    ))

(defun vhdl-stutter-mode-quote (count) " '' --> \""
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1))
      (if (= (preceding-char) last-input-char)
	  (progn (delete-backward-char 1) (insert-char 34 1)) ; double-quote
	(insert-char 39 1))		; single-quote
    (self-insert-command count)
    ))

(defun vhdl-stutter-mode-comma (count) " ',,' --> ' <= '"
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1))
      (cond ((= (preceding-char) last-input-char)
	     (progn (delete-char -1)
		    (if (not (eq (preceding-char) ? )) (insert " "))
		    (insert "<= ")))
	    (t
	     (insert-char 44 1)))	; comma
    (self-insert-command count)
    ))

(defun vhdl-stutter-mode-period (count) " '..' --> ' => '"
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1))
      (cond ((= (preceding-char) last-input-char)
	     (progn (delete-char -1)
		    (if (not (eq (preceding-char) ? )) (insert " "))
		    (insert "=> ")))
	    (t
	     (insert-char 46 1)))	; period
    (self-insert-command count)
    ))

(defun vhdl-paired-parens ()
  "Insert a pair of round parentheses, placing point between them."
  (interactive)
  (insert "()")
  (backward-char)
  )

(defun vhdl-stutter-mode-dash (count)
  "-- starts a comment, --- draws a horizontal line,
---- starts a display comment"
  (interactive "p")
  (if vhdl-stutter-mode
      (cond ((and abbrev-start-location (= abbrev-start-location (point)))
	     (setq abbrev-start-location nil)
	     (goto-char last-abbrev-location)
	     (beginning-of-line nil)
	     (vhdl-display-comment))
	    ((/= (preceding-char) ?-)	; standard dash (minus)
	     (self-insert-command count))
	    (t
	     (self-insert-command count)
	     (message "Enter - for horiz. line, CR for commenting-out code, else 1st char of comment")
	     (let ((next-input (read-char)))
	       (if (= next-input ?-)	; triple dash
		   (progn
		     (vhdl-display-comment-line)
		     (message
		      "Enter - for display comment, else continue with coding")
		     (let ((next-input (read-char)))
		       (if (= next-input ?-) ; four dashes
			   (vhdl-display-comment t)
			 (setq unread-command-events ;pushback the char
			       (list
				(vhdl-character-to-event-hack next-input)))
			 )))
		 (setq unread-command-events ;pushback the char
		       (list (vhdl-character-to-event-hack next-input)))
		 (vhdl-inline-comment)
		 ))))
    (self-insert-command count)
    ))

;; ############################################################################
;;  VHDL templates

(defun vhdl-alias ()
  "Insert alias declaration."
  (interactive)
  (vhdl-insert-keyword "ALIAS ")
  (if (equal (vhdl-field "name") "")
      nil
    (insert " : ")
    (vhdl-field "type")
    (vhdl-insert-keyword " IS ")
    (vhdl-field "name" ";")
    (vhdl-declaration-comment)
    ))

(defun vhdl-architecture ()
  "Insert architecture template."
  (interactive)
  (let ((margin (current-column))
	(vhdl-architecture-name)
	(position)
	(entity-exists)
	(string)
	(case-fold-search t))
    (vhdl-insert-keyword "ARCHITECTURE ")
    (if (equal (setq vhdl-architecture-name (vhdl-field "name")) "")
	nil
      (vhdl-insert-keyword " OF ")
      (setq position (point))
      (setq entity-exists
	    (re-search-backward "entity \\(\\(\\w\\|\\s_\\)+\\) is" nil t))
      (setq string (match-string 1))
      (goto-char position)
      (if (and entity-exists (not (equal string "")))
  	  (insert string)
        (vhdl-field "entity name"))
      (vhdl-insert-keyword " IS")
      (vhdl-begin-end (cons vhdl-architecture-name margin))
      (vhdl-block-comment)
      )))


(defun vhdl-array ()
  "Insert array type definition."
  (interactive)
  (vhdl-insert-keyword "ARRAY (")
  (if (equal (vhdl-field "range") "")
      (delete-char -1)
    (vhdl-insert-keyword ") OF ")
    (vhdl-field "type")
    (vhdl-insert-keyword ";")
    ))

(defun vhdl-assert ()
  "Inserts a assertion statement."
  (interactive)
  (vhdl-insert-keyword "ASSERT ")
  (if vhdl-conditions-in-parenthesis (insert "("))
  (if (equal (vhdl-field "condition (negated)") "")
      (progn (undo) (insert " "))
    (if vhdl-conditions-in-parenthesis (insert ")"))
    (vhdl-insert-keyword " REPORT \"")
    (vhdl-field "string-expression" "\" ")
    (vhdl-insert-keyword "SEVERITY ")
    (if (equal (vhdl-field "[note | warning | error | failure]") "")
	(delete-char -10))
    (insert ";")
    ))

(defun vhdl-attribute ()
  "Inserts an attribute declaration or specification."
  (interactive)
  (vhdl-insert-keyword "ATTRIBUTE ")
  (if (y-or-n-p "declaration (or specification)? ")
      (progn
	(vhdl-field "name" " : ")
        (vhdl-field "type" ";")
	(vhdl-declaration-comment))
    (vhdl-field "name")
    (vhdl-insert-keyword " OF ")
    (vhdl-field "entity name" " : ")
    (vhdl-field "entity class")
    (vhdl-insert-keyword " IS ")
    (vhdl-field "expression" ";")
    ))

(defun vhdl-block ()
  "Insert a block template."
  (interactive)
  (let ((position (point)))
    (vhdl-insert-keyword " : BLOCK ")
    (goto-char position))
  (let* ((margin (current-column))
	 (name (vhdl-field "label")))
    (if (equal name "")
	(progn (undo) (insert " "))
      (end-of-line)
      (insert "(")
      (if (equal (vhdl-field "[guard expression]") "")
	  (delete-char -2)
	(insert ")"))
      (vhdl-begin-end (cons (concat (vhdl-case-keyword "BLOCK ") name) margin))
      (vhdl-block-comment)
      )))

(defun vhdl-block-configuration ()
  "Insert a block configuration statement."
  (interactive)
  (let ((margin (current-column)))
    (vhdl-insert-keyword "FOR ")
    (if (equal (setq name (vhdl-field "block specification")) "")
	nil
      (vhdl-insert-keyword "\n\n")
      (indent-to margin)
      (vhdl-insert-keyword "END FOR;")
      (end-of-line 0)
      (indent-to (+ margin vhdl-basic-offset))
      )))

(defun vhdl-case ()
  "Inserts a case statement."
  (interactive)
  (let ((margin (current-column))
        (name))
    (vhdl-insert-keyword "CASE ")
    (if (equal (setq name (vhdl-field "expression")) "")
	nil
      (vhdl-insert-keyword " IS\n\n")
      (indent-to margin)
      (vhdl-insert-keyword "END CASE;")
;      (if vhdl-self-insert-comments (insert "  -- " name))
      (forward-line -1)
      (indent-to (+ margin vhdl-basic-offset))
      (vhdl-insert-keyword "WHEN  => ")
      (backward-char 4)
      )))

(defun vhdl-component ()
  "Inserts a component declaration."
  (interactive)
  (let ((margin (current-column)))
    (vhdl-insert-keyword "COMPONENT ")
    (if (equal (vhdl-field "name") "")
	nil
      (insert "\n\n")
      (indent-to margin)
      (vhdl-insert-keyword "END COMPONENT;")
      (end-of-line -0)
      (indent-to (+ margin vhdl-basic-offset))
      (vhdl-insert-keyword "GENERIC (")
      (vhdl-get-generic t t)
      (insert "\n")
      (indent-to (+ margin vhdl-basic-offset))
      (vhdl-insert-keyword "PORT (")
      (vhdl-get-port t t)
      (forward-line 1))
    ))

(defun vhdl-component-configuration ()
  "Inserts a component configuration (uses `vhdl-configuration-spec' since
these are almost equivalent)."
  (interactive)
  (let ((margin (current-column)))
    (vhdl-configuration-spec)
    (insert "\n")
    (indent-to margin)
    (vhdl-insert-keyword "END FOR;")
    ))

(defun vhdl-component-instance ()
  "Inserts a component instantiation statement."
  (interactive)
  (let ((margin (current-column)))
    (if (equal (vhdl-field "instance label") "")
	nil
      (insert " : ")
      (vhdl-field "component name" "\n")
      (indent-to (+ margin vhdl-basic-offset))
      (let ((position (point)))
	(vhdl-insert-keyword "GENERIC MAP (")
	(if (equal (vhdl-field "[association list]") "")
	    (progn (goto-char position)
		   (kill-line))
	  (insert ")\n")
	  (indent-to (+ margin vhdl-basic-offset))))
      (vhdl-insert-keyword "PORT MAP (")
      (vhdl-field "association list" ");")
    )))

(defun vhdl-concurrent-signal-assignment ()
  "Inserts a concurrent signal assignment."
  (interactive)
  (if (equal (vhdl-field "target signal") "")
      nil
    (insert " <= ")
;    (if (not (equal (vhdl-field "[GUARDED] [TRANSPORT]") ""))
;	(insert " "))
    (let ((margin (current-column))
	  (start (point)))
      (vhdl-field "waveform")
      (vhdl-insert-keyword " WHEN ")
      (if vhdl-conditions-in-parenthesis (insert "("))
      (while (not (equal (vhdl-field "[condition]") ""))
	(if vhdl-conditions-in-parenthesis (insert ")"))
	(vhdl-insert-keyword " ELSE")
	(insert "\n")
	(indent-to margin)
	(vhdl-field "waveform")
	(vhdl-insert-keyword " WHEN ")
	(if vhdl-conditions-in-parenthesis (insert "(")))
      (delete-char -6)
      (if vhdl-conditions-in-parenthesis (delete-char -1))
      (insert ";")
      (if vhdl-auto-align (vhdl-align start (point) 1))
      )))

(defun vhdl-configuration ()
  "Inserts a configuration specification if within an architecture,
a block or component configuration if within a configuration declaration,
a configuration declaration if not within a design unit."
  (interactive)
  (cond ((equal (car (car (cdr (vhdl-get-syntactic-context)))) 'architecture)
	 (vhdl-configuration-spec))
	((equal (car (car (cdr (vhdl-get-syntactic-context)))) 'configuration)
	 (if (y-or-n-p "block configuration (or component configuration)? ")
	     (vhdl-block-configuration)
	   (vhdl-component-configuration)))
	(t (vhdl-configuration-decl)))
  )

(defun vhdl-configuration-spec ()
  "Inserts a configuration specification."
  (interactive)
  (let ((margin (current-column)))
    (vhdl-insert-keyword "FOR ")
    (if (equal (vhdl-field "(component names | ALL)" " : ") "")
	(progn (undo) (insert " "))
      (vhdl-field "component type" "\n")
      (indent-to (+ margin vhdl-basic-offset))
      (vhdl-insert-keyword "USE ENTITY ")
      (vhdl-field "library name" ".")
      (vhdl-field "entity name" "(")
      (if (equal (vhdl-field "[architecture name]") "")
	  (delete-char -1)
	(insert ")"))
      (insert "\n")
      (indent-to (+ margin vhdl-basic-offset))
      (vhdl-insert-keyword "GENERIC MAP (")
      (if (equal (vhdl-field "[association list]") "")
	  (progn (kill-line -0)
		 (indent-to (+ margin vhdl-basic-offset)))
	(insert ")\n")
	(indent-to (+ margin vhdl-basic-offset)))
      (vhdl-insert-keyword "PORT MAP (")
      (if (equal (vhdl-field "[association list]") "")
	  (progn (kill-line -0)
		 (delete-char -1))
	(insert ")"))
      (insert ";")
      )))

(defun vhdl-configuration-decl ()
  "Inserts a configuration declaration."
  (interactive)
  (let ((margin (current-column))
	(position)
	(entity-exists)
	(string)
	(name))
    (vhdl-insert-keyword "CONFIGURATION ")
    (if (equal (setq name (vhdl-field "name")) "")
	nil
      (vhdl-insert-keyword " OF ")
      (setq position (point))
      (setq entity-exists
	    (re-search-backward "entity \\(\\(\\w\\|\\s_\\)*\\) is" nil t))
      (setq string (match-string 1))
      (goto-char position)
      (if (and entity-exists (not (equal string "")))
  	  (insert string)
        (vhdl-field "entity name"))
      (vhdl-insert-keyword " IS\n\n")
      (indent-to margin)
      (vhdl-insert-keyword "END ")
      (insert name ";")
      (end-of-line 0)
      (indent-to (+ margin vhdl-basic-offset))
      )))

(defun vhdl-constant ()
  "Inserts a constant declaration."
  (interactive)
  (vhdl-insert-keyword "CONSTANT ")
  (let ((in-arglist (string-match "arglist"
		     (format "%s" (car (car (vhdl-get-syntactic-context)))))))
    (if (not in-arglist)
	(let ((opoint (point)))
	  (beginning-of-line)
	  (setq in-arglist (looking-at ".*("))
	  (goto-char opoint)))
    (if (equal  (vhdl-field "name") "")
	nil
      (insert " : ")
      (if in-arglist (vhdl-insert-keyword "IN "))
      (vhdl-field "type")
      (if in-arglist
	  (insert ";")
	(let ((position (point)))
	  (insert " := ")
	  (if (equal (vhdl-field "[initialization]" ";") "")
	      (progn (goto-char position) (kill-line) (insert ";")))
	  (vhdl-declaration-comment))
    ))))

(defun vhdl-default ()
  "Insert nothing."
  (interactive)
  (insert " ")
  (unexpand-abbrev)
  (backward-word 1)
  (vhdl-case-word 1)
  (forward-char 1)
  )

(defun vhdl-default-indent ()
  "Insert nothing and indent."
  (interactive)
  (insert " ")
  (unexpand-abbrev)
  (backward-word 1)
  (vhdl-case-word 1)
  (forward-char 1)
  (vhdl-indent-line)
  )

(defun vhdl-disconnect ()
  "Insert a disconnect statement."
  (interactive)
  (vhdl-insert-keyword "DISCONNECT ")
  (if (equal (vhdl-field "guarded signal specification") "")
      nil
    (vhdl-insert-keyword " AFTER ")
    (vhdl-field "time expression" ";")
  ))

(defun vhdl-else ()
  "Insert an else statement."
  (interactive)
  (let ((margin))
    (vhdl-insert-keyword "ELSE")
    (if (not (equal 'block-close (car (car (vhdl-get-syntactic-context)))))
	(insert " ")
      (vhdl-indent-line)
      (setq margin (current-indentation))
      (insert "\n")
      (indent-to (+ margin vhdl-basic-offset))
    )))

(defun vhdl-elsif ()
  "Insert an elsif statement."
  (interactive)
  (let ((margin))
    (vhdl-insert-keyword "ELSIF ")
    (if vhdl-conditions-in-parenthesis (insert "("))
    (if (equal (vhdl-field "condition") "")
	(progn (undo) (insert " "))
      (if vhdl-conditions-in-parenthesis (insert ")"))
      (vhdl-indent-line)
      (setq margin (current-indentation))
      (vhdl-insert-keyword " THEN\n")
      (indent-to (+ margin vhdl-basic-offset))
      )))

(defun vhdl-entity ()
  "Insert an entity template."
  (interactive)
  (let ((margin (current-column))
	(vhdl-entity-name))
    (vhdl-insert-keyword "ENTITY ")
    (if (equal (setq vhdl-entity-name (vhdl-field "entity name")) "")
	nil
      (vhdl-insert-keyword " IS\n\n")
      (indent-to margin)
      (vhdl-insert-keyword "END ")
      (insert vhdl-entity-name ";")
      (end-of-line -0)
      (indent-to (+ margin vhdl-basic-offset))
      (vhdl-entity-body)
      )))

(defun vhdl-entity-body ()
  "Insert an entity body."
  (interactive)
  (let ((margin (current-column)))
    (if vhdl-additional-empty-lines (insert "\n"))
    (indent-to margin)
    (vhdl-insert-keyword "GENERIC (")
    (if (vhdl-get-generic t)
	(if vhdl-additional-empty-lines (insert "\n")))
    (insert "\n")
    (indent-to margin)
    (vhdl-insert-keyword "PORT (")
    (if (vhdl-get-port t)
	(if vhdl-additional-empty-lines (insert "\n")))
    (end-of-line 2)
    ))

(defun vhdl-exit ()
  "Insert an exit statement."
  (interactive)
  (vhdl-insert-keyword "EXIT ")
  (if (string-equal (vhdl-field "[loop label]") "")
      (delete-char -1))
  (let ((opoint (point)))
    (vhdl-insert-keyword " WHEN ")
    (if vhdl-conditions-in-parenthesis (insert "("))
    (if (equal (vhdl-field "[condition]") "")
      (progn (goto-char opoint)
	     (kill-line))
      (if vhdl-conditions-in-parenthesis (insert ")"))))
  (insert ";")
  )

(defun vhdl-for ()
  "Inserts a block or component configuration if within a configuration
declaration, a for loop otherwise."
  (interactive)
  (if (equal (car (car (cdr (vhdl-get-syntactic-context)))) 'configuration)
      (if (y-or-n-p "block configuration (or component configuration)? ")
	  (vhdl-block-configuration)
	(vhdl-component-configuration))
    (vhdl-for-loop)))

(defun vhdl-for-loop ()
  "Insert a for loop template."
  (interactive)
  (let ((position (point)))
    (vhdl-insert-keyword " : FOR ")
    (goto-char position))
  (let* ((margin (current-column))
	 (name (vhdl-field "[label]"))
	 (named (not (string-equal name "")))
	 (index))
    (if (not named) (delete-char 3))
    (end-of-line)
    (if (equal (setq index (vhdl-field "loop variable")) "")
	nil
      (vhdl-insert-keyword " IN ")
      (vhdl-field "range")
      (vhdl-insert-keyword " LOOP\n\n")
      (indent-to margin)
      (vhdl-insert-keyword "END LOOP")
      (if named (insert " " name ";")
	(insert ";")
	(if vhdl-self-insert-comments (insert "  -- " index)))
      (forward-line -1)
      (indent-to (+ margin vhdl-basic-offset))
      )))

(defun vhdl-function ()
  "Insert function specification or body template."
  (interactive)
  (let ((margin (current-column))
	(name))
    (vhdl-insert-keyword "FUNCTION ")
    (if (equal (setq name (vhdl-field "name")) "")
	nil
      (vhdl-get-arg-list)
      (vhdl-insert-keyword " RETURN ")
      (vhdl-field "type" " ")
      (if (y-or-n-p "insert body? ")
	  (progn (vhdl-insert-keyword "IS")
	         (vhdl-begin-end (cons name margin))
		 (vhdl-block-comment))
        (delete-char -1)
        (insert ";\n")
        (indent-to margin)))
    ))

(defun vhdl-generate ()
  "Insert a generate template."
  (interactive)
  (let ((position (point)))
    (vhdl-insert-keyword " GENERATE")
    (goto-char position))
  (let ((margin (current-column))
	(label (vhdl-field "label"))
	(string))
    (if (equal label "")
	(progn (undo) (insert " "))
      (insert " : ")
      (setq string (vhdl-field "(FOR | IF)"))
      (insert " ")
      (if (equal (upcase string) "IF")
	  (progn
	    (if vhdl-conditions-in-parenthesis (insert "("))
	    (vhdl-field "condition")
	    (if vhdl-conditions-in-parenthesis (insert ")")))
        (vhdl-field "loop variable")
	(vhdl-insert-keyword " IN ")
	(vhdl-field "range"))
      (end-of-line)
      (insert "\n\n")
      (indent-to margin)
      (vhdl-insert-keyword "END GENERATE ")
      (insert label ";")
      (end-of-line 0)
      (indent-to (+ margin vhdl-basic-offset))
      )))

(defun vhdl-generic ()
  "Insert generic declaration, or generic map in instantiation statements."
  (interactive)
  (vhdl-insert-keyword "GENERIC (")
  (cond ((equal (car (car (cdr (vhdl-get-syntactic-context)))) 'entity)
	 (vhdl-get-generic nil))
	((or (equal 'statement-cont (car (car (vhdl-get-syntactic-context))))
	  (save-excursion
	   (and (backward-word 2) (skip-chars-backward " ")
		(eq (preceding-char) ?:))))
	 (delete-char -1) (vhdl-map))
	(t (vhdl-get-generic nil t))))

(defun vhdl-header ()
  "Insert a VHDL file header."
  (interactive)
  (let (eot)
    (save-excursion
      (save-restriction
 	(widen)
	(goto-char (point-min))
	(if vhdl-header-file
	    (setq eot (car (cdr (insert-file-contents vhdl-header-file))))
	  ; insert default header
	  (insert "\
-------------------------------------------------------------------------------
-- Title         : <title string>
-- Project       : <project string>
-------------------------------------------------------------------------------
-- File          : <filename>
-- Author        : <author>
-- Created       : <date>
-- Last modified : <date>
-------------------------------------------------------------------------------
-- Description :
-- <cursor>
-------------------------------------------------------------------------------
-- Modification history :
-- <date> : created
-------------------------------------------------------------------------------

")
	  (setq eot (point)))
 	(narrow-to-region (point-min) eot)
	(goto-char (point-min))
	(while (search-forward "<filename>" nil t)
	  (replace-match (buffer-name) t t))
	(goto-char (point-min))
	(while (search-forward "<author>" nil t)
	  (replace-match "" t t)
	  (insert (user-full-name) "  <" user-mail-address ">"))
	(goto-char (point-min))
 	;; Replace <RCS> with $, so that RCS for the source is
 	;; not over-enthusiastic with replacements
 	(while (search-forward "<RCS>" nil t)
 	  (replace-match "$" nil t))
	(goto-char (point-min))
	(while (search-forward "<date>" nil t)
	  (replace-match "" t t)
	  (vhdl-insert-date))
	(goto-char (point-min))
	(let (string)
	  (while (re-search-forward "<\\(\\w*\\) string>" nil t)
	    (setq string (read-string (concat (match-string 1) ": ")))
	    (replace-match string t t)))))
    (goto-char (point-min))
    (if (search-forward "<cursor>" nil t)
	(replace-match "" t t))))

(defun vhdl-if ()
  "Insert an if statement template."
  (interactive)
  (let ((margin (current-column)))
    (vhdl-insert-keyword "IF ")
    (if vhdl-conditions-in-parenthesis (insert "("))
    (if (equal (vhdl-field "condition") "")
	(progn (undo) (insert " "))
      (if vhdl-conditions-in-parenthesis (insert ")"))
      (vhdl-insert-keyword " THEN\n\n")
      (indent-to margin)
      (vhdl-insert-keyword "END IF;")
      (forward-line -1)
      (indent-to (+ margin vhdl-basic-offset))
      )))

(defun vhdl-library ()
  "Insert a library specification."
  (interactive)
  (let ((margin (current-column))
	(lib-name))
    (vhdl-insert-keyword "LIBRARY ")
    (if (equal  (setq lib-name (vhdl-field "library name")) "")
	nil
      (insert ";\n")
      (indent-to margin)
      (vhdl-insert-keyword "USE ")
      (insert lib-name)
      (vhdl-insert-keyword "..ALL;")
      (backward-char 5)
      (if (equal (vhdl-field "package name") "")
	  (progn (vhdl-kill-entire-line)
	         (end-of-line -0))
        (end-of-line)
	))))

(defun vhdl-loop ()
  "Insert a loop template."
  (interactive)
  (let ((position (point)))
    (vhdl-insert-keyword " : LOOP")
    (goto-char position))
  (let* ((margin (current-column))
	 (name (vhdl-field "[label]"))
	 (named (not (string-equal name ""))))
    (if (not named) (delete-char 3))
    (end-of-line)
    (insert "\n\n")
    (indent-to margin)
    (vhdl-insert-keyword "END LOOP")
    (insert (if named (concat " " name ";") ?\;))
    (forward-line -1)
    (indent-to (+ margin vhdl-basic-offset))
    ))

(defun vhdl-map ()
  "Insert a map specification."
  (interactive)
  (vhdl-insert-keyword "MAP (")
  (if (equal (vhdl-field "[association list]") "")
      (progn (undo) (insert " "))
    (insert ")")
    ))

(defun vhdl-modify ()
  "Actualize modification date."
  (interactive)
  (goto-char (point-min))
  (if (search-forward vhdl-modify-date-prefix-string nil t)
      (progn (kill-line)
	     (vhdl-insert-date))
    (message (concat "Modification date prefix string \""
		     vhdl-modify-date-prefix-string
		     "\" not found!"))
    (beep)))

(defun vhdl-next ()
  "Inserts a next statement."
  (interactive)
  (vhdl-insert-keyword "NEXT ")
  (if (string-equal (vhdl-field "[loop label]") "")
      (delete-char -1))
  (let ((opoint (point)))
    (vhdl-insert-keyword " WHEN ")
    (if vhdl-conditions-in-parenthesis (insert "("))
    (if (equal (vhdl-field "[condition]") "")
	(progn (goto-char opoint)
	       (kill-line))
      (if vhdl-conditions-in-parenthesis (insert ")"))))
  (insert ";")
  )

(defun vhdl-package ()
  "Insert a package specification or body."
  (interactive)
  (let ((margin (current-column))
	(name))
    (vhdl-insert-keyword "PACKAGE ")
    (if (y-or-n-p "body? ")
	(vhdl-insert-keyword "BODY "))
    (setq name (vhdl-field "name" " is\n\n"))
    (indent-to margin)
    (vhdl-insert-keyword "END ")
    (insert name ";")
    (forward-line -1)
    (indent-to (+ margin vhdl-basic-offset))
    ))

(defun vhdl-port ()
  "Insert a port declaration, or port map in instantiation statements."
  (interactive)
  (vhdl-insert-keyword "PORT (")
  (cond ((equal (car (car (cdr (vhdl-get-syntactic-context)))) 'entity)
	 (vhdl-get-port nil))
	((or (equal 'statement-cont (car (car (vhdl-get-syntactic-context))))
	  (save-excursion
	   (and (backward-word 2) (skip-chars-backward " ")
		(eq (preceding-char) ?:))))
	 (delete-char -1) (vhdl-map))
	(t (vhdl-get-port nil t))))

(defun vhdl-procedure ()
  "Insert a procedure specification or body template."
  (interactive)
  (let ((margin (current-column))
	(name))
    (vhdl-insert-keyword "PROCEDURE ")
    (if (equal (setq name (vhdl-field "name")) "")
	nil
      (vhdl-get-arg-list)
      (insert " ")
      (if (y-or-n-p "insert body? ")
	  (progn (vhdl-insert-keyword "IS")
	         (vhdl-begin-end (cons name margin))
		 (vhdl-block-comment))
        (delete-char -1)
        (insert ";\n")
        (indent-to margin)
	))))

(defun vhdl-process ()
  "Insert a process template."
  (interactive)
  (let ((clocked))
    (let ((position (point)))
      (vhdl-insert-keyword "PROCESS")
      (setq clocked (y-or-n-p "clocked process? "))
      (goto-char position)
      (insert " : ")
      (goto-char position))
    (let* ((margin (current-column))
           (finalline)
	   (name (vhdl-field "[label]"))
	   (named (not (string-equal name "")))
	   (clock) (reset)
	   (case-fold-search t))
      (if (not named) (delete-char 3))
      (end-of-line)
      (insert " (")
      (if (not clocked)
	  (if (equal (vhdl-field "[sensitivity list]" ")") "")
	      (delete-char -3))
        (setq clock (vhdl-field "clock name" ", "))
        (setq reset (vhdl-field "reset name" ")")))
      (vhdl-begin-end (cons (concat (vhdl-case-keyword "PROCESS")
				    (if named (concat " " name))) margin))
      (if clocked (vhdl-clock-async-reset clock reset))
      (if vhdl-prompt-for-comments
	  (progn
	    (setq finalline (vhdl-current-line))
	    (if (and (re-search-backward "\\<begin\\>" nil t)
		     (re-search-backward "\\<process\\>" nil t))
		(progn
		  (end-of-line -0)
		  (insert "\n")
		  (indent-to margin)
		  (insert "-- purpose: ")
		  (if (equal (vhdl-field "description") "")
		      (vhdl-kill-entire-line)
		    (newline)
		    (indent-to margin)
		    (insert "-- type:    ")
		    (insert (if clocked "memorizing" "memoryless") "\n")
		    (indent-to margin)
		    (insert "-- inputs:  ")
		    (if clocked
			(insert clock ", " reset ", "))
		    (if (and (equal (vhdl-field "signal names") "")
			     clocked)
			(delete-char -2))
		    (insert "\n")
		    (indent-to margin)
		    (insert "-- outputs: ")
		    (vhdl-field "signal names")
		    (setq finalline (+ finalline 4)))))
	    (goto-line finalline)
	    (end-of-line)
	    )))))

(defun vhdl-record ()
  "Insert a record type declaration."
  (interactive)
  (let ((margin (current-column))
	(start (point))
	(first t))
    (vhdl-insert-keyword "RECORD\n")
    (indent-to (+ margin vhdl-basic-offset))
    (if (equal (vhdl-field "identifiers") "")
	(progn (kill-line -0)
	       (delete-char -1)
	       (insert " "))
      (while (or first (not (equal (vhdl-field "[identifiers]") "")))
        (insert " : ")
        (vhdl-field "type" ";")
        (vhdl-declaration-comment)
        (newline)
	(indent-to (+ margin vhdl-basic-offset))
        (setq first nil))
      (kill-line -0)
      (indent-to margin)
      (vhdl-insert-keyword "END RECORD;")
      (if vhdl-auto-align (vhdl-align start (point) 1))
      )))

(defun vhdl-return-value ()
  "Insert a return statement."
  (interactive)
  (vhdl-insert-keyword "RETURN ")
  (if (equal (vhdl-field "[expression]") "")
      (delete-char -1))
  (insert ";")
  )

(defun vhdl-selected-signal-assignment ()
  "Insert a selected signal assignment."
  (interactive)
  (let ((margin (current-column))
	(start (point)))
    (let ((position (point)))
      (vhdl-insert-keyword " SELECT")
      (goto-char position))
    (vhdl-insert-keyword "WITH ")
    (if (equal (vhdl-field "selector expression") "")
	(progn (undo) (insert " "))
      (end-of-line)
      (insert "\n")
      (indent-to (+ margin vhdl-basic-offset))
      (vhdl-field "target signal" " <= ")
;      (vhdl-field "[GUARDED] [TRANSPORT]")
      (insert "\n")
      (indent-to (+ margin vhdl-basic-offset))
      (while (not (equal (vhdl-field "[waveform]") ""))
	(vhdl-insert-keyword " WHEN ")
	(vhdl-field "choices" ",")
	(newline)
	(indent-to (+ margin vhdl-basic-offset)))
      (if (not (equal (vhdl-field "[alternative waveform]") ""))
	  (vhdl-insert-keyword " WHEN OTHERS")
	(fixup-whitespace)
	(delete-char -2))
      (insert ";")
      (if vhdl-auto-align (vhdl-align start (point) 1))
      )))

(defun vhdl-signal ()
  "Insert a signal declaration."
  (interactive)
  (vhdl-insert-keyword "SIGNAL ")
  (let ((in-arglist (string-match "arglist"
		     (format "%s" (car (car (vhdl-get-syntactic-context)))))))
    (if (not in-arglist)
	(let ((opoint (point)))
	  (beginning-of-line)
	  (setq in-arglist (looking-at ".*("))
	  (goto-char opoint)))
    (if (equal (vhdl-field "names") "")
	nil
      (insert " : ")
      (if in-arglist
	  (progn (vhdl-field "direction")
		 (insert " ")))
      (vhdl-field "type")
      (if in-arglist
	  (insert ";")
	(let ((position (point)))
	  (insert " := ")
	  (if (equal (vhdl-field "[initialization]" ";") "")
	      (progn (goto-char position) (kill-line) (insert ";")))
	  (vhdl-declaration-comment))
	))))

(defun vhdl-subtype ()
  "Insert a subtype declaration."
  (interactive)
  (vhdl-insert-keyword "SUBTYPE ")
  (if (equal (vhdl-field "name") "")
      nil
    (vhdl-insert-keyword " IS ")
    (vhdl-field "type" " ")
    (if (equal (vhdl-field "[RANGE value range | ( index range )]") "")
	(delete-char -1))
    (insert ";")
    (vhdl-declaration-comment)
    ))

(defun vhdl-type ()
  "Insert a type declaration."
  (interactive)
  (vhdl-insert-keyword "TYPE ")
  (if (equal (vhdl-field "name") "")
      nil
    (vhdl-insert-keyword " IS ")
    (let ((definition (upcase (vhdl-field "(scalar type | ARRAY | RECORD | ACCESS | FILE)"))))
      (cond ((equal definition "ARRAY")
	     (kill-word -1) (vhdl-array))
            ((equal definition "RECORD")
	     (kill-word -1) (vhdl-record))
            ((equal definition "ACCESS")
	     (insert " ") (vhdl-field "type" ";"))
            ((equal definition "FILE")
	     (vhdl-insert-keyword " OF ") (vhdl-field "type" ";"))
	    (t (insert ";")))
      (vhdl-declaration-comment)
      )))

(defun vhdl-use ()
  "Insert a use clause."
  (interactive)
  (vhdl-insert-keyword "USE ..ALL;")
  (backward-char 6)
  (if (equal (vhdl-field "library name") "")
      (progn (undo) (insert " "))
    (forward-char 1)
    (vhdl-field "package name")
    (end-of-line)
    ))

(defun vhdl-variable ()
  "Insert a variable declaration."
  (interactive)
  (vhdl-insert-keyword "VARIABLE ")
  (let ((in-arglist (string-match "arglist"
		     (format "%s" (car (car (vhdl-get-syntactic-context)))))))
    (if (not in-arglist)
	(let ((opoint (point)))
	  (beginning-of-line)
	  (setq in-arglist (looking-at ".*("))
	  (goto-char opoint)))
    (if (equal (vhdl-field "names") "")
	nil
      (insert " : ")
      (if in-arglist
	  (progn (vhdl-field "direction")
		 (insert " ")))
      (vhdl-field "type")
      (if in-arglist
	  (insert ";")
	(let ((position (point)))
	  (insert " := ")
	  (if (equal (vhdl-field "[initialization]" ";") "")
	      (progn (goto-char position) (kill-line) (insert ";")))
	  (vhdl-declaration-comment))
    ))))

(defun vhdl-wait ()
  "Insert a wait statement."
  (interactive)
  (vhdl-insert-keyword "WAIT ")
  (if (equal (vhdl-field
	      "[ON sensitivity list] [UNTIL condition] [FOR time expression]")
	     "")
      (delete-char -1))
  (insert ";")
  )

(defun vhdl-when ()
  "Indent correctly if within a case statement."
  (interactive)
  (let ((position (point))
	(margin))
    (if (and (= (current-column) (current-indentation))
	     (re-search-forward "\\<end\\>" nil t)
	     (looking-at "\\s-*\\<case\\>"))
	(progn
	  (setq margin (current-indentation))
	  (goto-char position)
	  (delete-horizontal-space)
	  (indent-to (+ margin vhdl-basic-offset)))
      (goto-char position)
      )
    (vhdl-insert-keyword "WHEN ")
    ))

(defun vhdl-while-loop ()
  "Insert a while loop template."
  (interactive)
  (let ((position (point)))
    (vhdl-insert-keyword " : WHILE ")
    (goto-char position))
  (let* ((margin (current-column))
	 (name (vhdl-field "[label]"))
	 (named (not (string-equal name ""))))
    (if (not named) (delete-char 3))
    (end-of-line)
    (if vhdl-conditions-in-parenthesis (insert "("))
    (if (equal (vhdl-field "condition") "")
	(progn (undo 2))
      (if vhdl-conditions-in-parenthesis (insert ")"))
      (vhdl-insert-keyword " LOOP\n\n")
      (indent-to margin)
      (vhdl-insert-keyword "END LOOP")
      (insert (if named (concat " " name ";") ?\;))
      (forward-line -1)
      (indent-to (+ margin vhdl-basic-offset))
      )))

(defun vhdl-with ()
  "Insert a with statement (i.e. selected signal assignment)."
  (interactive)
  (vhdl-selected-signal-assignment)
  )

;; ############################################################################
;; Custom functions

(defun vhdl-clocked-wait ()
  "Insert a wait statement for rising clock edge."
  (interactive)
  (vhdl-insert-keyword "WAIT UNTIL ")
  (let* ((clock  (vhdl-field "clock name")))
    (insert "'event")
    (vhdl-insert-keyword " AND ")
    (insert clock)
    (insert " = " vhdl-one-string ";")
  ))

(defun vhdl-clock-async-reset (clock reset)
  "Insert a template reacting on asynchronous reset and rising clock edge
for inside a memorizing processes."
  (interactive)
  (let* ( (margin (current-column))
	  (opoint))
    (if vhdl-self-insert-comments
       (insert "-- activities triggered by asynchronous reset (active low)\n"))
    (indent-to margin)
    (vhdl-insert-keyword "IF ")
    (insert reset " = " vhdl-zero-string)
    (vhdl-insert-keyword " THEN\n")
    (indent-to (+ margin vhdl-basic-offset))
    (setq opoint (point))
    (newline)
    (indent-to margin)
    (if vhdl-self-insert-comments
	(insert "-- activities triggered by rising edge of clock\n"))
    (indent-to margin)
    (vhdl-insert-keyword "ELSIF ")
    (insert clock "'event")
    (vhdl-insert-keyword " AND ")
    (insert clock " = " vhdl-one-string)
    (vhdl-insert-keyword " THEN\n")
    (indent-to (+ margin vhdl-basic-offset))
    (newline)
    (indent-to margin)
    (vhdl-insert-keyword "END IF;")
;    (if vhdl-self-insert-comments (insert "  -- " clock))
    (goto-char opoint)
    ))

(defun vhdl-standard-package (library package)
  "Insert specification of a standard package."
  (interactive)
  (let ((margin (current-column)))
    (vhdl-insert-keyword "LIBRARY ")
    (insert library ";\n")
    (indent-to margin)
    (vhdl-insert-keyword "USE ")
    (insert library "." package)
    (vhdl-insert-keyword ".ALL;")
    ))

(defun vhdl-package-numeric-bit ()
  "Insert specification of 'numeric_bit' package."
  (interactive)
  (vhdl-standard-package "ieee" "numeric_bit"))

(defun vhdl-package-numeric-std ()
  "Insert specification of 'numeric_std' package."
  (interactive)
  (vhdl-standard-package "ieee" "numeric_std"))

(defun vhdl-package-std-logic-1164 ()
  "Insert specification of 'std_logic_1164' package."
  (interactive)
  (vhdl-standard-package "ieee" "std_logic_1164"))

(defun vhdl-package-textio ()
  "Insert specification of 'textio' package."
  (interactive)
  (vhdl-standard-package "std" "textio"))

;; ############################################################################
;; Comment functions

(defun vhdl-comment-indent ()
  (let* ((opoint (point))
	(col (progn
	       (forward-line -1)
	       (if (re-search-forward "--" opoint t)
		   (- (current-column) 2) ;Existing comment at bol stays there.
		 (goto-char opoint)
		 (skip-chars-backward " \t")
		 (max comment-column  ;else indent to comment column
		      (1+ (current-column))) ;except leave at least one space.
		 ))))
    (goto-char opoint)
    col
    ))

(defun vhdl-inline-comment ()
  "Start a comment at the end of the line.
  if on line with code, indent at least comment-column.
  if starting after end-comment-column, start a new line."
  (interactive)
  (if (> (current-column) end-comment-column) (newline-and-indent))
  (if (or (looking-at "\\s-*$")	;end of line
	  (and (not unread-command-events) ; called with key binding or menu
	       (not (end-of-line))))
      (let ((margin))
        (while (= (preceding-char) ?-) (delete-char -1))
	(setq margin (current-column))
        (delete-horizontal-space)
        (if (bolp)
            (progn (indent-to margin) (insert "--"))
	  (insert "  ")
          (indent-to comment-column)
          (insert "--"))
	(if (not unread-command-events) (insert " ")))
     ; else code following current point implies commenting out code
    (let (next-input code)
      (while (= (preceding-char) ?-) (delete-char -2))
      (while (= (setq next-input (read-char)) 13) ; CR
	(insert "--"); or have a space after it?
	(forward-char -2)
	(forward-line 1)
	(message "Enter CR if commenting out a line of code.")
	(setq code t)
	)
      (if (not code) (progn
;	  (indent-to comment-column)
	  (insert "--") ;hardwire to 1 space or use vhdl-basic-offset?
	  ))
      (setq unread-command-events
	    (list (vhdl-character-to-event-hack next-input))) ;pushback the char
      )))

(defun vhdl-display-comment (&optional line-exists)
  "Add 2 comment lines at the current indent, making a display comment."
  (interactive)
  (if (not line-exists)
      (vhdl-display-comment-line))
  (let* ((col (current-column))
	 (len (- end-comment-column col)))
    (insert "\n")
    (insert-char ?  col)
    (insert-char ?- len)
    (insert "\n")
    (insert-char ?  col)
    (end-of-line -1)
    )
  (insert "-- ")
  )

(defun vhdl-display-comment-line ()
  "Displays one line of dashes."
  (interactive)
  (while (= (preceding-char) ?-) (delete-char -2))
  (let* ((col (current-column))
	 (len (- end-comment-column col)))
    (insert-char ?- len)
    (insert-char ?\n 1)
    (insert-char ?  col)
    ))

(defun vhdl-declaration-comment ()
  (if vhdl-prompt-for-comments
      (let ((position (point)))
	(insert "  ")
	(indent-to comment-column)
	(insert "-- ")
	(if (equal (vhdl-field "comment") "")
	    (progn (goto-char position) (kill-line))
	  ))))

(defun vhdl-block-comment ()
  (if vhdl-prompt-for-comments
      (let ((finalline (vhdl-current-line))
	    (case-fold-search t))
	(beginning-of-line -0)
	(if (re-search-backward "\\<\\(architecture\\|block\\|function\\|procedure\\|process\\)\\>" nil t)
	    (let ((margin))
	      (back-to-indentation)
	      (setq margin (current-column))
	      (end-of-line -0)
	      (insert "\n")
	      (indent-to margin)
	      (insert "-- purpose: ")
	      (if (equal (vhdl-field "description") "")
		  (vhdl-kill-entire-line)
		(setq finalline (+ finalline 1)))))
	(goto-line finalline)
	(end-of-line)
	)))

(defun vhdl-comment-uncomment-region (beg end &optional arg)
  "Comment out region if not commented out, uncomment out region if already
commented out."
  (interactive "r\nP")
  (goto-char beg)
  (if (looking-at comment-start)
      (comment-region beg end -1)
    (comment-region beg end)
      ))

;; ############################################################################
;; Help functions

(defun vhdl-outer-space (count)
  "Expand abbreviations and self-insert space(s), do indent-new-comment-line
if in comment and past end-comment-column."
  (interactive "p")
  (if (or (and (>= (preceding-char) ?a) (<= (preceding-char) ?z))
	  (and (>= (preceding-char) ?A) (<= (preceding-char) ?Z)))
      (progn
	(modify-syntax-entry ?_ "w" vhdl-mode-syntax-table)
	(expand-abbrev)
	(if (not vhdl-underscore-is-part-of-word)
	    (modify-syntax-entry ?_ "_" vhdl-mode-syntax-table))
	))
  (if (not (vhdl-in-comment-p))
      (self-insert-command count)
    (if (< (current-column) end-comment-column)
	(self-insert-command count)
      (while (> (current-column) end-comment-column) (forward-word -1))
      (while (> (preceding-char) ? ) (forward-word -1))
      (delete-horizontal-space)
      (indent-new-comment-line)
      (end-of-line nil)
      (insert-char ?  count)
      )))

(defun vhdl-field (prompt &optional following-string)
  "Prompt for string and insert it in buffer with optional following-string."
  (let ((opoint (point)))
    (insert "<" prompt ">")
    (let ((string (read-from-minibuffer (concat prompt ": ") ""
					vhdl-minibuffer-local-map)))
      (delete-region opoint (point))
      (insert string (or following-string ""))
      (if vhdl-upper-case-keywords
	  (vhdl-fix-case-region-1
	   opoint (point) t vhdl-93-keywords-regexp))
      string
      )))

(defun vhdl-in-comment-p ()
  "Check if point is to right of beginning comment delimiter."
  (interactive)
  (let ((opoint (point)))
    (save-excursion ; finds an unquoted comment
      (beginning-of-line)
      (re-search-forward "^\\([^\"]*\"[^\"]*\"\\)*[^\"]*--" opoint t)
      )))

(defun vhdl-in-string-p ()
  "Check if point is in a string."
  (interactive)
  (let ((opoint (point)))
    (save-excursion ; preceeded by odd number of string delimiters?
      (beginning-of-line)
      (equal
       opoint
       (re-search-forward "^\\([^\"]*\"[^\"]*\"\\)*[^\"]*\"[^\"]*" opoint t))
      )))

(defun vhdl-begin-end (list)
  "Insert a begin ... end pair with optional name after the end.
Point is left between them."
  (let ((return)
	(name (car list))
	(margin (cdr list)))
    (if vhdl-additional-empty-lines
	(progn
	  (insert "\n")
	  (indent-to (+ margin vhdl-basic-offset))))
    (insert "\n")
    (indent-to margin)
    (vhdl-insert-keyword "BEGIN")
    (if vhdl-self-insert-comments
	(insert (and name (concat "  -- " name))))
    (insert "\n")
    (indent-to (+ margin vhdl-basic-offset))
    (setq return (point))
    (newline)
    (indent-to margin)
    (vhdl-insert-keyword "END")
    (insert (and name (concat " " name)) ";")
    (goto-char return)
    ))

(defun vhdl-get-arg-list ()
  "Read from user a procedure or function argument list."
  (insert " (")
  (let ((margin (current-column)))
    (if (not vhdl-argument-list-indent)
	(let ((opoint (point)))
	  (back-to-indentation)
	  (setq margin (+ (current-column) vhdl-basic-offset))
	  (goto-char opoint)
	  (newline)
	  (indent-to margin)))
    (let (not-empty interface)
      (setq interface (vhdl-field "[CONSTANT] [SIGNAL] [VARIABLE]"))
      (if (not (equal interface ""))
	  (insert " "))
      (while (not (string-equal (vhdl-field "[names]") ""))
	(setq not-empty t)
	(insert " : ")
	(if (not (equal (vhdl-field "[direction]") ""))
	    (insert " "))
	(vhdl-field "type" ";\n")
	(indent-to margin)
	(setq interface (vhdl-field "[CONSTANT] [SIGNAL] [VARIABLE]"))
	(if (not (equal interface ""))
	    (insert " ")))
      (if not-empty
	  (progn (kill-line -0)
		 (delete-char -2)
		 (if (not vhdl-argument-list-indent)
		     (progn (insert "\n") (indent-to margin)))
		 (insert ")"))
	(if vhdl-argument-list-indent
	    (backward-delete-char 2)
	  (kill-line -0)
	  (backward-delete-char 3)))
;	(while (string-match "[,;]$" args)
;	  (newline)
;	  (indent-to margin) (setq args (vhdl-field "next argument")))
;	(insert 41) ;close-paren
	)))

(defun vhdl-get-port (optional &optional no-comment)
  "Read from user a port spec argument list."
  (let ((margin (current-column))
	(start (point)))
    (if (not vhdl-argument-list-indent)
	(let ((opoint (point)))
	  (back-to-indentation)
	  (setq margin (+ (current-column) vhdl-basic-offset))
	  (goto-char opoint)
	  (newline)
	  (indent-to margin)))
    (let ((vhdl-ports (vhdl-field "[names]")))
      (if (string-equal vhdl-ports "")
	  (if optional
	      (progn (vhdl-kill-entire-line) (forward-line -1)
		     (if (not vhdl-argument-list-indent)
			 (progn (vhdl-kill-entire-line) (forward-line -1))))
	    (progn (undo) (insert " "))
	    nil )
	(insert " : ")
	(progn
	  (let ((semicolon-pos))
	    (while (not (string-equal "" vhdl-ports))
	      (vhdl-field "direction")
	      (insert " ")
	      (vhdl-field "type")
	      (setq semicolon-pos (point))
	      (insert ";")
	      (if (not no-comment)
		  (vhdl-declaration-comment))
	      (newline)
	      (indent-to margin)
	      (setq vhdl-ports (vhdl-field "[names]" " : ")))
	    (goto-char semicolon-pos)
	    (if (not vhdl-argument-list-indent)
		(progn (delete-char 1) (end-of-line) (insert "\n")
		       (indent-to margin) (insert ";") (backward-char 1)))
	    (insert ")")
	    (forward-char 1)
	    (if (= (following-char) ? )
		(delete-char 1))
	    (forward-line 1)
	    (vhdl-kill-entire-line)
	    (end-of-line -0)
	    (if vhdl-auto-align (vhdl-align start (point) 1))
	    t))))))

(defun vhdl-get-generic (optional &optional no-value )
  "Read from user a generic spec argument list."
  (let ((margin (current-column))
	(start (point)))
    (if (not vhdl-argument-list-indent)
	(let ((opoint (point)))
	  (back-to-indentation)
	  (setq margin (+ (current-column) vhdl-basic-offset))
	  (goto-char opoint)
	  (newline)
	  (indent-to margin)))
    (let ((vhdl-generic))
      (if no-value
	  (setq vhdl-generic (vhdl-field "[names]"))
	(setq vhdl-generic (vhdl-field "[name]")))
      (if (string-equal vhdl-generic "")
	  (if optional
	      (progn (vhdl-kill-entire-line) (end-of-line -0)
		     (if (not vhdl-argument-list-indent)
			 (progn (vhdl-kill-entire-line) (end-of-line -0))))
	    (progn (undo) (insert " "))
	    nil )
	(insert " : ")
	(progn
	  (let ((semicolon-pos))
	    (while (not(string-equal "" vhdl-generic))
	      (vhdl-field "type")
	      (if no-value
		  (progn (setq semicolon-pos (point))
			 (insert ";"))
		(insert " := ")
		(if (equal (vhdl-field "[value]") "")
		    (delete-char -4))
		(setq semicolon-pos (point))
		(insert ";")
		(vhdl-declaration-comment))
	      (newline)
	      (indent-to margin)
	      (if no-value
		  (setq vhdl-generic (vhdl-field "[names]" " : "))
		(setq vhdl-generic (vhdl-field "[name]" " : "))))
	    (goto-char semicolon-pos)
	    (if (not vhdl-argument-list-indent)
		(progn (delete-char 1) (end-of-line) (insert "\n")
		       (indent-to margin) (insert ";") (backward-char 1)))
	    (insert ")")
	    (forward-char 1)
	    (if (= (following-char) ? )
		(delete-char 1))
	    (forward-line 1)
	    (vhdl-kill-entire-line)
	    (end-of-line -0)
	    (if vhdl-auto-align (vhdl-align start (point) 1))
	    t))))))

(defun vhdl-insert-date ()
  "Insert date in appropriate format."
  (interactive)
    (insert
     (cond
      ((eq vhdl-date-format 'american) (format-time-string "%m/%d/%Y" nil))
      ((eq vhdl-date-format 'european) (format-time-string "%d.%m.%Y" nil))
      ((eq vhdl-date-format 'scientific) (format-time-string "%Y/%m/%d" nil))
      )))

(defun vhdl-insert-keyword (keyword)
  (insert (if vhdl-upper-case-keywords (upcase keyword) (downcase keyword)))
  )

(defun vhdl-case-keyword (keyword)
  (if vhdl-upper-case-keywords (upcase keyword) (downcase keyword))
  )

(defun vhdl-case-word (num)
  (if vhdl-upper-case-keywords (upcase-word num) (downcase-word num))
  )

(defun vhdl-fix-case-region-1 (beg end upper-case word-regexp &optional count)
  "Convert all words matching word-regexp in region to lower or upper case,
depending on parameter upper-case."
  (let ((case-fold-search t)
	(case-replace nil)
	(busy-counter 0))
    (modify-syntax-entry ?_ "w" vhdl-mode-syntax-table)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward word-regexp end t)
	(or (vhdl-in-comment-p)
	    (vhdl-in-string-p)
	    (if upper-case
		(upcase-word -1)
	      (downcase-word -1)))
	(if (and count
		 (/= busy-counter (setq busy-counter
		     (+ (* count 25) (/ (* 25 (- (point) beg)) (- end beg))))))
	    (message (format "Fixing case ... (%2d%s)" busy-counter "%%"))))
      (goto-char end))
    (if (not vhdl-underscore-is-part-of-word)
	(modify-syntax-entry ?_ "_" vhdl-mode-syntax-table))
    (message "")
    ))

(defun vhdl-fix-case-region (beg end &optional arg)
  "Convert all VHDL words in region to lower or upper case, depending on
variables vhdl-upper-case-{keywords,types,attributes,enum-values}."
  (interactive "r\nP")
  (vhdl-fix-case-region-1
   beg end vhdl-upper-case-keywords vhdl-93-keywords-regexp 0)
  (vhdl-fix-case-region-1
   beg end vhdl-upper-case-types vhdl-93-types-regexp 1)
  (vhdl-fix-case-region-1
   beg end vhdl-upper-case-attributes vhdl-93-attributes-regexp 2)
  (vhdl-fix-case-region-1
   beg end vhdl-upper-case-enum-values vhdl-93-enum-values-regexp 3)
  )

(defun vhdl-fix-case-buffer ()
  "Convert all VHDL words in buffer to lower or upper case, depending on
variables vhdl-upper-case-{keywords,types,attributes,enum-values}."
  (interactive)
  (vhdl-fix-case-region (point-min) (point-max))
  )

(defun vhdl-minibuffer-tab (&optional prefix-arg)
  "If preceeding character is part of a word then dabbrev-expand,
else if right of non whitespace on line then tab-to-tab-stop,
else indent line in proper way for current major mode
(used for word completion in VHDL minibuffer)."
  (interactive "P")
  (cond ((= (char-syntax (preceding-char)) ?w)
	 (let ((case-fold-search nil)) (dabbrev-expand prefix-arg)))
	((> (current-column) (current-indentation))
	 (tab-to-tab-stop))
	(t
	 (if (eq indent-line-function 'indent-to-left-margin)
	     (insert-tab prefix-arg)
	   (if prefix-arg
	       (funcall indent-line-function prefix-arg)
	     (funcall indent-line-function))))))

(defun vhdl-help ()
  "Display help information in '*Help*' buffer ."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ mode-name)
    (princ " mode:\n")
    (princ (documentation major-mode))
    (save-excursion
      (set-buffer standard-output)
      (help-mode))
    (print-help-return-message)))

(defun vhdl-current-line ()
  "Return the line number of the line containing point."
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point)))))
  )

(defun vhdl-kill-entire-line ()
  "Delete entire line."
  (interactive)
  (end-of-line)
  (kill-line -0)
  (delete-char 1)
  )

(defun vhdl-open-line ()
  "Open a new line and indent."
  (interactive)
  (end-of-line)
  (newline-and-indent)
  )

(defun vhdl-kill-line ()
  "Kill current line."
  (interactive)
  (vhdl-kill-entire-line)
  )

(defun vhdl-character-to-event-hack (char)
  (if (memq 'XEmacs vhdl-emacs-features)
      (character-to-event char)
    char))

;; ############################################################################
;; Abbrev hooks

(defun vhdl-electric-mode ()
  "Toggle VHDL Electric mode."
  (interactive)
  (setq vhdl-electric-mode (not vhdl-electric-mode))
  (setq mode-name (if vhdl-electric-mode "Electric VHDL" "VHDL"))
  (force-mode-line-update)
  )

(defun vhdl-stutter-mode ()
  "Toggle VHDL Stuttering mode."
  (interactive)
  (setq vhdl-stutter-mode (not vhdl-stutter-mode))
  )

(defun vhdl-hooked-abbrev (fun)
  "Do function, if syntax says abbrev is a keyword, invoked by hooked abbrev,
but not if inside a comment or quote)"
  (if (or (vhdl-in-comment-p)
	  (vhdl-in-string-p)
	  (save-excursion (forward-word -1)
			  (and (looking-at "\\<end\\>")
			       (not (looking-at "\\<end;")))))
      (progn
	(insert " ")
	(unexpand-abbrev)
	(delete-char -1))
    (if (not vhdl-electric-mode)
	(progn
	  (insert " ")
	  (unexpand-abbrev)
	  (backward-word 1)
	  (vhdl-case-word 1)
	  (delete-char 1)
	)
      (let ((invoke-char last-command-char) (abbrev-mode -1))
	(funcall fun)
	(if (= invoke-char ?-) (setq abbrev-start-location (point)))
	;; delete CR which is still in event queue
	(if (memq 'XEmacs vhdl-emacs-features)
	    (enqueue-eval-event 'delete-char -1)
	  (setq unread-command-events		; push back a delete char
		(list (vhdl-character-to-event-hack ?\177))))
	))))

(defun vhdl-alias-hook () "hooked version of vhdl-alias."
  (vhdl-hooked-abbrev 'vhdl-alias))
(defun vhdl-architecture-hook () "hooked version of vhdl-architecture."
  (vhdl-hooked-abbrev 'vhdl-architecture))
(defun vhdl-array-hook () "hooked version of vhdl-array."
  (vhdl-hooked-abbrev 'vhdl-array))
(defun vhdl-assert-hook () "hooked version of vhdl-assert."
  (vhdl-hooked-abbrev 'vhdl-assert))
(defun vhdl-attribute-hook () "hooked version of vhdl-attribute."
  (vhdl-hooked-abbrev 'vhdl-attribute))
(defun vhdl-block-hook () "hooked version of vhdl-block."
  (vhdl-hooked-abbrev 'vhdl-block))
(defun vhdl-case-hook () "hooked version of vhdl-case."
  (vhdl-hooked-abbrev 'vhdl-case))
(defun vhdl-component-hook () "hooked version of vhdl-component."
  (vhdl-hooked-abbrev 'vhdl-component))
(defun vhdl-component-instance-hook ()
  "hooked version of vhdl-component-instance."
  (vhdl-hooked-abbrev 'vhdl-component-instance))
(defun vhdl-concurrent-signal-assignment-hook ()
  "hooked version of vhdl-concurrent-signal-assignment."
  (vhdl-hooked-abbrev 'vhdl-concurrent-signal-assignment))
(defun vhdl-configuration-hook ()
  "hooked version of vhdl-configuration."
  (vhdl-hooked-abbrev 'vhdl-configuration))
(defun vhdl-constant-hook () "hooked version of vhdl-constant."
  (vhdl-hooked-abbrev 'vhdl-constant))
(defun vhdl-disconnect-hook () "hooked version of vhdl-disconnect."
  (vhdl-hooked-abbrev 'vhdl-disconnect))
(defun vhdl-display-comment-hook () "hooked version of vhdl-display-comment."
  (vhdl-hooked-abbrev 'vhdl-display-comment))
(defun vhdl-else-hook () "hooked version of vhdl-else."
  (vhdl-hooked-abbrev 'vhdl-else))
(defun vhdl-elsif-hook () "hooked version of vhdl-elsif."
  (vhdl-hooked-abbrev 'vhdl-elsif))
(defun vhdl-entity-hook () "hooked version of vhdl-entity."
  (vhdl-hooked-abbrev 'vhdl-entity))
(defun vhdl-exit-hook () "hooked version of vhdl-exit."
  (vhdl-hooked-abbrev 'vhdl-exit))
(defun vhdl-for-hook () "hooked version of vhdl-for."
  (vhdl-hooked-abbrev 'vhdl-for))
(defun vhdl-function-hook () "hooked version of vhdl-function."
  (vhdl-hooked-abbrev 'vhdl-function))
(defun vhdl-generate-hook () "hooked version of vhdl-generate."
  (vhdl-hooked-abbrev 'vhdl-generate))
(defun vhdl-generic-hook () "hooked version of vhdl-generic."
  (vhdl-hooked-abbrev 'vhdl-generic))
(defun vhdl-library-hook () "hooked version of vhdl-library."
  (vhdl-hooked-abbrev 'vhdl-library))
(defun vhdl-header-hook () "hooked version of vhdl-header."
  (vhdl-hooked-abbrev 'vhdl-header))
(defun vhdl-if-hook () "hooked version of vhdl-if."
  (vhdl-hooked-abbrev 'vhdl-if))
(defun vhdl-loop-hook () "hooked version of vhdl-loop."
  (vhdl-hooked-abbrev 'vhdl-loop))
(defun vhdl-map-hook () "hooked version of vhdl-map."
  (vhdl-hooked-abbrev 'vhdl-map))
(defun vhdl-modify-hook () "hooked version of vhdl-modify."
  (vhdl-hooked-abbrev 'vhdl-modify))
(defun vhdl-next-hook () "hooked version of vhdl-next."
  (vhdl-hooked-abbrev 'vhdl-next))
(defun vhdl-package-hook () "hooked version of vhdl-package."
  (vhdl-hooked-abbrev 'vhdl-package))
(defun vhdl-port-hook () "hooked version of vhdl-port."
  (vhdl-hooked-abbrev 'vhdl-port))
(defun vhdl-procedure-hook () "hooked version of vhdl-procedure."
  (vhdl-hooked-abbrev 'vhdl-procedure))
(defun vhdl-process-hook () "hooked version of vhdl-process."
  (vhdl-hooked-abbrev 'vhdl-process))
(defun vhdl-record-hook () "hooked version of vhdl-record."
  (vhdl-hooked-abbrev 'vhdl-record))
(defun vhdl-return-hook () "hooked version of vhdl-return-value."
  (vhdl-hooked-abbrev 'vhdl-return-value))
(defun vhdl-selected-signal-assignment-hook ()
  "hooked version of vhdl-selected-signal-assignment."
  (vhdl-hooked-abbrev 'vhdl-selected-signal-assignment))
(defun vhdl-signal-hook () "hooked version of vhdl-signal."
  (vhdl-hooked-abbrev 'vhdl-signal))
(defun vhdl-subtype-hook () "hooked version of vhdl-subtype."
  (vhdl-hooked-abbrev 'vhdl-subtype))
(defun vhdl-type-hook () "hooked version of vhdl-type."
  (vhdl-hooked-abbrev 'vhdl-type))
(defun vhdl-use-hook () "hooked version of vhdl-use."
  (vhdl-hooked-abbrev 'vhdl-use))
(defun vhdl-variable-hook () "hooked version of vhdl-variable."
  (vhdl-hooked-abbrev 'vhdl-variable))
(defun vhdl-wait-hook () "hooked version of vhdl-wait."
  (vhdl-hooked-abbrev 'vhdl-wait))
(defun vhdl-when-hook () "hooked version of vhdl-when."
  (vhdl-hooked-abbrev 'vhdl-when))
(defun vhdl-while-loop-hook () "hooked version of vhdl-while-loop."
  (vhdl-hooked-abbrev 'vhdl-while-loop))
(defun vhdl-and-hook () "hooked version of vhdl-and."
  (vhdl-hooked-abbrev 'vhdl-and))
(defun vhdl-or-hook () "hooked version of vhdl-or."
  (vhdl-hooked-abbrev 'vhdl-or))
(defun vhdl-nand-hook () "hooked version of vhdl-nand."
  (vhdl-hooked-abbrev 'vhdl-nand))
(defun vhdl-nor-hook () "hooked version of vhdl-nor."
  (vhdl-hooked-abbrev 'vhdl-nor))
(defun vhdl-xor-hook () "hooked version of vhdl-xor."
  (vhdl-hooked-abbrev 'vhdl-xor))
(defun vhdl-xnor-hook () "hooked version of vhdl-xnor."
  (vhdl-hooked-abbrev 'vhdl-xnor))
(defun vhdl-not-hook () "hooked version of vhdl-not."
  (vhdl-hooked-abbrev 'vhdl-not))

(defun vhdl-default-hook () "hooked version of vhdl-default."
  (vhdl-hooked-abbrev 'vhdl-default))
(defun vhdl-default-indent-hook () "hooked version of vhdl-default-indent."
  (vhdl-hooked-abbrev 'vhdl-default-indent))


;; ############################################################################
;; Font locking
;; ############################################################################
;; (using `font-lock.el')

;; ############################################################################
;; Syntax definitions

(defvar vhdl-font-lock-keywords nil
  "Regular expressions to highlight in VHDL Mode.")

(defconst vhdl-font-lock-keywords-0
 (list
  ;; highlight template prompts
  '("\\(^\\|[ (.\t]\\)\\(<[^ =].*[^ =]>\\)\\([ .]\\|$\\)"
    2 vhdl-font-lock-prompt-face)

  ;; highlight character literals
  '("'\\(.\\)'" 1 'font-lock-string-face)
  )
  "For consideration as a value of `vhdl-font-lock-keywords'.
This does highlighting of template prompts and character literals.")

(defconst vhdl-font-lock-keywords-1
  (list
   ;; highlight names of units, subprograms, and components when declared
   (list
    (concat
     "^\\s-*\\("
     "architecture\\|configuration\\|entity\\|package\\(\\s-+body\\|\\)\\|"
     "function\\|procedure\\|component"
     "\\)\\s-+\\(\\w+\\)")
    3 'font-lock-function-name-face)

   ;; highlight labels of common constructs
   (list
    (concat
     "^\\s-*\\(\\w+\\)\\s-*:\\(\\s-\\|\n\\)*\\("
     "assert\\|block\\|case\\|exit\\|for\\|if\\|loop\\|"
     "next\\|null\\|process\\| with\\|while\\|"
     "\\w+\\(\\s-\\|\n\\)+\\(generic\\|port\\)\\s-+map"
     "\\)\\>")
    1 'font-lock-function-name-face)

   ;; highlight entity names of architectures and configurations
   (list
    "^\\s-*\\(architecture\\|configuration\\)\\s-+\\w+\\s-+of\\s-+\\(\\w+\\)"
    2 'font-lock-function-name-face)

   ;; highlight names and labels at end of constructs
   (list
    (concat
     "^\\s-*end\\s-+\\("
     "\\(block\\|case\\|component\\|for\\|generate\\|if\\|loop\\|"
     "process\\|record\\|units\\)\\>\\|"
     "\\)\\s-*\\(\\w*\\)")
     3 'font-lock-function-name-face)
   )
"For consideration as a value of `vhdl-font-lock-keywords'.
This does highlighting of names and labels.")

(defconst vhdl-font-lock-keywords-2
 (list
  ;; highlight keywords, and types, standardized attributes, enumeration values
  (list (concat "'" vhdl-93-attributes-regexp)
	                           1 'vhdl-font-lock-attribute-face)
  (list vhdl-93-types-regexp       1 'font-lock-type-face)
  (list vhdl-93-enum-values-regexp 1 'vhdl-font-lock-value-face)
  (list vhdl-93-keywords-regexp    1 'font-lock-keyword-face)
  )
  "For consideration as a value of `vhdl-font-lock-keywords'.
This does highlighting of comments, keywords, and standard types.")

(defconst vhdl-font-lock-keywords-3
 (list
  ;; highlight clock signals.
  (cons vhdl-clock-signal-syntax   'vhdl-font-lock-clock-signal-face)
  (cons vhdl-reset-signal-syntax   'vhdl-font-lock-reset-signal-face)
  (cons vhdl-control-signal-syntax 'vhdl-font-lock-control-signal-face)
  (cons vhdl-data-signal-syntax    'vhdl-font-lock-data-signal-face)
  (cons vhdl-test-signal-syntax    'vhdl-font-lock-test-signal-face)
  )
  "For consideration as a value of `vhdl-font-lock-keywords'.
This does highlighting of signal names with specific syntax.")

;; ############################################################################
;; Font and color definitions

(defvar vhdl-font-lock-prompt-face	   'vhdl-font-lock-prompt-face
  "Face name to use for prompts.")

(defvar vhdl-font-lock-attribute-face	   'vhdl-font-lock-attribute-face
  "Face name to use for attributes.")

(defvar vhdl-font-lock-value-face	   'vhdl-font-lock-value-face
  "Face name to use for enumeration values.")

(defvar vhdl-font-lock-clock-signal-face   'vhdl-font-lock-clock-signal-face
  "Face name to use for clock signals.")

(defvar vhdl-font-lock-reset-signal-face   'vhdl-font-lock-reset-signal-face
  "Face name to use for reset signals.")

(defvar vhdl-font-lock-control-signal-face 'vhdl-font-lock-control-signal-face
  "Face name to use for control signals.")

(defvar vhdl-font-lock-data-signal-face	   'vhdl-font-lock-data-signal-face
  "Face name to use for data signals.")

(defvar vhdl-font-lock-test-signal-face	   'vhdl-font-lock-test-signal-face
  "Face name to use for test signals.")

(defface vhdl-font-lock-prompt-face
  '((((class color) (background light)) (:foreground "Red"))
    (((class color) (background dark)) (:foreground "Red"))
    (t (:inverse-video t)))
  "Font Lock mode face used to highlight prompts."
  :group 'font-lock-highlighting-faces)

(defface vhdl-font-lock-attribute-face
  '((((class color) (background light)) (:foreground "CadetBlue"))
    (((class color) (background dark)) (:foreground "CadetBlue"))
    (t (:italic t :bold t)))
  "Font Lock mode face used to highlight attributes."
  :group 'font-lock-highlighting-faces)

(defface vhdl-font-lock-value-face
  '((((class color) (background light)) (:foreground "DarkGoldenrod"))
    (((class color) (background dark)) (:foreground "DarkGoldenrod"))
    (t (:italic t :bold t)))
  "Font Lock mode face used to highlight enumeration values."
  :group 'font-lock-highlighting-faces)

(defface vhdl-font-lock-clock-signal-face
  '((((class color) (background light)) (:foreground "LimeGreen"))
    (((class color) (background dark)) (:foreground "LimeGreen"))
    (t ()))
  "Font Lock mode face used to highlight clock signals."
  :group 'font-lock-highlighting-faces)

(defface vhdl-font-lock-reset-signal-face
  '((((class color) (background light)) (:foreground "Red"))
    (((class color) (background dark)) (:foreground "Red"))
    (t ()))
  "Font Lock mode face used to highlight reset signals."
  :group 'font-lock-highlighting-faces)

(defface vhdl-font-lock-control-signal-face
  '((((class color) (background light)) (:foreground "Blue"))
    (((class color) (background dark)) (:foreground "Blue"))
    (t ()))
  "Font Lock mode face used to highlight control signals."
  :group 'font-lock-highlighting-faces)

(defface vhdl-font-lock-data-signal-face
  '((((class color) (background light)) (:foreground "Black"))
    (((class color) (background dark)) (:foreground "Black"))
    (t ()))
  "Font Lock mode face used to highlight data signals."
  :group 'font-lock-highlighting-faces)

(defface vhdl-font-lock-test-signal-face
  '((((class color) (background light)) (:foreground "Gold"))
    (((class color) (background dark)) (:foreground "Gold"))
    (t ()))
  "Font Lock mode face used to highlight test signals."
  :group 'font-lock-highlighting-faces)

;; Custom color definitions for existing faces
(defun vhdl-set-face-foreground ()
  (set-face-foreground 'font-lock-comment-face "IndianRed")
  (set-face-foreground 'font-lock-function-name-face "MediumOrchid")
  (set-face-foreground 'font-lock-keyword-face "SlateBlue")
  (set-face-foreground 'font-lock-string-face "RosyBrown")
  (set-face-foreground 'font-lock-type-face "ForestGreen")
  )

(defun vhdl-set-face-grayscale ()
  (interactive)
  (set-face-bold-p 'font-lock-comment-face nil)
  (set-face-inverse-video-p 'font-lock-comment-face nil)
  (set-face-italic-p 'font-lock-comment-face t)
  (set-face-underline-p 'font-lock-comment-face nil)

  (set-face-bold-p 'font-lock-function-name-face nil)
  (set-face-inverse-video-p 'font-lock-function-name-face nil)
  (set-face-italic-p 'font-lock-function-name-face t)
  (set-face-underline-p 'font-lock-function-name-face nil)

  (set-face-bold-p 'font-lock-keyword-face t)
  (set-face-inverse-video-p 'font-lock-keyword-face nil)
  (set-face-italic-p 'font-lock-keyword-face nil)
  (set-face-underline-p 'font-lock-keyword-face nil)

  (set-face-bold-p 'font-lock-string-face nil)
  (set-face-inverse-video-p 'font-lock-string-face nil)
  (set-face-italic-p 'font-lock-string-face nil)
  (set-face-underline-p 'font-lock-string-face t)

  (set-face-bold-p 'font-lock-type-face t)
  (set-face-inverse-video-p 'font-lock-type-face nil)
  (set-face-italic-p 'font-lock-type-face t)
  (set-face-underline-p 'font-lock-type-face nil)
  )

;; ############################################################################
;; Font lock initialization

(defun vhdl-font-lock-init ()
  "Initializes fontification."
  (setq vhdl-font-lock-keywords
	(append vhdl-font-lock-keywords-0
		(if vhdl-highlight-names vhdl-font-lock-keywords-1)
	        (if vhdl-highlight-keywords vhdl-font-lock-keywords-2)
		(if (and vhdl-highlight-signals (x-display-color-p))
		    vhdl-font-lock-keywords-3)))
  (if (x-display-color-p)
      (if vhdl-customize-colors (vhdl-set-face-foreground))
    (if vhdl-customize-faces (vhdl-set-face-grayscale))
  ))

;; ############################################################################
;; Fontification for postscript printing

(defun vhdl-ps-init ()
  "Initializes face and page settings for postscript printing."
  (require 'ps-print)
  (unless (or (not vhdl-customize-faces)
	      ps-print-color-p)
    (set (make-local-variable 'ps-bold-faces)
	 '(font-lock-keyword-face
	   font-lock-type-face
	   vhdl-font-lock-attribute-face
	   vhdl-font-lock-value-face))
    (set (make-local-variable 'ps-italic-faces)
	 '(font-lock-comment-face
	   font-lock-function-name-face
	   font-lock-type-face
	   vhdl-font-lock-prompt-face
	   vhdl-font-lock-attribute-face
	   vhdl-font-lock-value-face))
    (set (make-local-variable 'ps-underlined-faces)
	 '(font-lock-string-face))
    )
  ;; define page settings, so that a line containing 79 characters (default)
  ;; fits into one column
  (if vhdl-print-two-column
      (progn
	(set (make-local-variable 'ps-landscape-mode) t)
	(set (make-local-variable 'ps-number-of-columns) 2)
	(set (make-local-variable 'ps-font-size) 7.0)
	(set (make-local-variable 'ps-header-title-font-size) 10.0)
	(set (make-local-variable 'ps-header-font-size) 9.0)
	(set (make-local-variable 'ps-header-offset) 12.0)
	(if (eq ps-paper-type 'letter)
	    (progn
	      (set (make-local-variable 'ps-inter-column) 40.0)
	      (set (make-local-variable 'ps-left-margin) 40.0)
	      (set (make-local-variable 'ps-right-margin) 40.0)
	)))))


;; ############################################################################
;; Hideshow
;; ############################################################################
;; (using `hideshow.el')

(defun vhdl-forward-sexp-function (&optional count)
  "Find begin and end of VHDL process or block (for hideshow)."
  (interactive "p")
  (let (name
	(case-fold-search t))
    (end-of-line)
    (if (< count 0)
	(re-search-backward "\\s-*\\(\\w\\|\\s_\\)+\\s-*:\\s-*\\(process\\|block\\)\\>" nil t)
      (re-search-forward "\\s-*\\<end\\s-+\\(process\\|block\\)\\>" nil t)
  )))

;; Not needed `hs-special-modes-alist' is autoloaded.
;(require 'hideshow)

(unless (assq 'vhdl-mode hs-special-modes-alist)
    (setq hs-special-modes-alist
	  (cons
	   '(vhdl-mode
	     "\\s-*\\(\\w\\|\\s_\\)+\\s-*:\\s-*\\(process\\|PROCESS\\|block\\|BLOCK\\)\\>"
	     "\\s-*\\<\\(end\\|END\\)\\s-+\\(process\\|PROCESS\\|block\\|BLOCK\\)\\>"
	     "-- "
	     vhdl-forward-sexp-function)
	   hs-special-modes-alist)))


;; ############################################################################
;; Compilation
;; ############################################################################
;; (using `compile.el')

(defvar vhdl-compile-commands
  '(
    (cadence "cv -file" nil)
    (ikos "analyze" nil)
    (quickhdl "qvhcom" nil)
    (synopsys "vhdlan" nil)
    (vantage "analyze -libfile vsslib.ini -src" nil)
    (viewlogic "analyze -libfile vsslib.ini -src" nil)
    (v-system "vcom" "vmake > Makefile")
    )
  "Commands to be called in the shell for compilation (syntax analysis) of a
single buffer and `Makefile' generation for different tools. First item is tool
identifier, second item is shell command for compilation, and third item is
shell command for `Makefile' generation. A tool is specified by assigning a
tool identifier to variable `vhdl-compiler'.")

(defvar vhdl-compilation-error-regexp-alist
  (list
    ;; Cadence Design Systems: cv -file test.vhd
    ;; duluth: *E,430 (test.vhd,13): identifier (POSITIV) is not declared
    '("duluth: \\*E,[0-9]+ (\\(.+\\),\\([0-9]+\\)):" 1 2)

    ;; Ikos Voyager: analyze test.vhd
    ;; E L4/C5:        this library unit is inaccessible
    ; Xemacs does not support error messages without included file name
    (if (not (memq 'XEmacs vhdl-emacs-features))
	'("E L\\([0-9]+\\)/C[0-9]+:" nil 1)
      '("E L\\([0-9]+\\)/C[0-9]+:" 2 1)
      )

    ;; QuickHDL, Mentor Graphics: qvhcom test.vhd
    ;; ERROR: test.vhd(24): near "dnd": expecting: END
    '("ERROR: \\(.+\\)(\\([0-9]+\\)):" 1 2)

    ;; Synopsys, VHDL Analyzer: vhdlan test.vhd
    ;; **Error: vhdlan,703 test.vhd(22): OTHERS is not legal in this context.
    '("\\*\\*Error: vhdlan,[0-9]+ \\(.+\\)(\\([0-9]+\\)):" 1 2)

    ;; Vantage Analysis Systems: analyze -libfile vsslib.ini -src test.vhd
    ;; **Error: LINE 499 *** No aggregate value is valid in this context.
    ; Xemacs does not support error messages without included file name
    (if (not (memq 'XEmacs vhdl-emacs-features))
	'("\\*\\*Error: LINE \\([0-9]+\\) \\*\\*\\*" nil 1)
      '("\\*\\*Error: LINE \\([0-9]+\\) \\*\\*\\*" 2 1)
      )

    ;; Viewlogic: analyze -libfile vsslib.ini -src test.vhd
    ;; **Error: LINE 499 *** No aggregate value is valid in this context.
    ;; same regexp as for Vantage

    ;; V-System, Model Technology: vcom test.vhd
    ;; ERROR: test.vhd(14): Unknown identifier: positiv
    ;; same regexp as for QuickHDL

    ) "Alist that specifies how to match errors in VHDL compiler output.")

(defvar compilation-file-regexp-alist
  '(
    ;; Ikos Voyager: analyze -libfile vsslib.ini -src test.vhd
    ;; analyze sdrctl.vhd
    ("^analyze +\\(.+ +\\)*\\(.+\\)$" 2)

    ;; Vantage Analysis Systems: analyze -libfile vsslib.ini -src test.vhd
    ;;     Compiling "pcu.vhd" line 1...
    (" *Compiling \"\\(.+\\)\" " 1)

    ;; Viewlogic: analyze -libfile vsslib.ini -src test.vhd
    ;;     Compiling "pcu.vhd" line 1...
    ;; same regexp as for Vantage

    ) "Alist specifying how to match lines that indicate a new current file.
Used for compilers with no file name in the error messages.")

(defun vhdl-compile ()
  "Compile current buffer using the VHDL compiler specified in
`vhdl-compiler'."
  (interactive)
  (let ((command-list vhdl-compile-commands)
	command)
    (while command-list
      (if (eq vhdl-compiler (car (car command-list)))
	  (setq command (car (cdr (car command-list)))))
      (setq command-list (cdr command-list)))
    (if command
	(compile (concat command " " vhdl-compiler-options
			 (if (not (string-equal vhdl-compiler-options "")) " ")
			 (file-name-nondirectory (buffer-file-name)))))))

(defun vhdl-make ()
  "Call make command for compilation of all updated source files
(requires `Makefile')."
  (interactive)
  (compile "make"))

(defun vhdl-generate-makefile ()
  "Generate new `Makefile'."
  (interactive)
  (let ((command-list vhdl-compile-commands)
	command)
    (while command-list
      (if (eq vhdl-compiler (car (car command-list)))
	  (setq command (car (cdr (cdr (car command-list))))))
      (setq command-list (cdr command-list)))
    (if command
	(compile command )
      (message (format "Not implemented for `%s'!" vhdl-compiler))
      (beep))))


;; ############################################################################
;; Bug reports
;; ############################################################################
;; (using `reporter.el')

(defconst vhdl-version "3.19"
  "VHDL Mode version number.")

(defconst vhdl-mode-help-address "vhdl-mode@geocities.com"
  "Address for VHDL Mode bug reports.")

(defun vhdl-version ()
  "Echo the current version of VHDL Mode in the minibuffer."
  (interactive)
  (message "Using VHDL Mode version %s" vhdl-version)
  (vhdl-keep-region-active))

;; get reporter-submit-bug-report when byte-compiling
(and (fboundp 'eval-when-compile)
     (eval-when-compile
      (require 'reporter)))

(defun vhdl-submit-bug-report ()
  "Submit via mail a bug report on VHDL Mode."
  (interactive)
  ;; load in reporter
  (and
   (y-or-n-p "Do you want to submit a report on VHDL Mode? ")
   (require 'reporter)
   (reporter-submit-bug-report
    vhdl-mode-help-address
    (concat "VHDL Mode " vhdl-version)
    (list
     ;; report all important variables
     'vhdl-basic-offset
     'vhdl-offsets-alist
     'vhdl-comment-only-line-offset
     'tab-width
     'vhdl-electric-mode
     'vhdl-stutter-mode
     'vhdl-indent-tabs-mode
     'vhdl-compiler
     'vhdl-compiler-options
     'vhdl-upper-case-keywords
     'vhdl-upper-case-types
     'vhdl-upper-case-attributes
     'vhdl-upper-case-enum-values
     'vhdl-auto-align
     'vhdl-additional-empty-lines
     'vhdl-argument-list-indent
     'vhdl-conditions-in-parenthesis
     'vhdl-date-format
     'vhdl-header-file
     'vhdl-modify-date-prefix-string
     'vhdl-zero-string
     'vhdl-one-string
     'vhdl-self-insert-comments
     'vhdl-prompt-for-comments
     'vhdl-comment-column
     'vhdl-end-comment-column
     'vhdl-highlight-names
     'vhdl-highlight-keywords
     'vhdl-highlight-signals
     'vhdl-highlight-case-sensitive
     'vhdl-customize-colors
     'vhdl-customize-faces
     'vhdl-clock-signal-syntax
     'vhdl-reset-signal-syntax
     'vhdl-control-signal-syntax
     'vhdl-data-signal-syntax
     'vhdl-test-signal-syntax
     'vhdl-source-file-menu
     'vhdl-index-menu
     'vhdl-hideshow-menu
     'vhdl-print-two-column
     'vhdl-intelligent-tab
     'vhdl-template-key-binding-prefix
     'vhdl-word-completion-in-minibuffer
     'vhdl-underscore-is-part-of-word
     'vhdl-mode-hook
     )
    (function
     (lambda ()
       (insert
	(if vhdl-special-indent-hook
	    (concat "\n@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n"
		    "vhdl-special-indent-hook is set to '"
		    (format "%s" vhdl-special-indent-hook)
		    ".\nPerhaps this is your problem?\n"
		    "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n\n")
	  "\n")
	(format "vhdl-emacs-features: %s\n" vhdl-emacs-features)
	)))
    nil
    "Dear VHDL Mode maintainers,"
    )))


;; ############################################################################

(provide 'vhdl-mode)

;;; vhdl-mode.el ends here
